! =============================================================================
! Module: mod_kmeans
!
! Description:
!   K-Means and DBSCAN clustering algorithms for 2D density surfaces.
!
!   Contains:
!     1. Raw algorithms (k_means_clustr, dbscan_cluster) from Sparks/Ester
!     2. Grid adapters (kmeans_grid_cluster, dbscan_grid_cluster) that
!        match the interface of watershed_cluster: take a 2D surface
!        and produce a 2D label array + cluster count.
!
!   Grid Adapter Mechanism:
!     - Extract cells where surface(i,j) > threshold as observations
!     - Each observation has 2 variables: cell_x(i), cell_y(j)
!     - Run the clustering algorithm on these observations
!     - Map cluster labels back to the 2D grid
!     - Cells below threshold get LABEL_NOISE (-1)
!
! Dependencies:
!   None (self-contained)
! =============================================================================

module mod_kmeans
    use mod_watershed, only: find_local_maxima, smooth_box_filter
    implicit none

    ! Label for unassigned / below-threshold cells (must match mod_watershed)
    integer, parameter :: KMEANS_LABEL_NOISE = -1

    public :: kmeans_grid_cluster
    public :: dbscan_grid_cluster
    public :: local_box_filter

contains

    ! =================================================================
    ! PUBLIC: kmeans_grid_cluster
    !
    ! Grid adapter for K-means.  Takes a 2D surface and produces
    ! a 2D integer label array + cluster count.
    !
    ! Arguments:
    !   surface(nx, ny)  : input density surface
    !   nx, ny           : grid dimensions
    !   labels(nx, ny)   : output cluster labels (1..n_clusters or NOISE)
    !   n_clusters       : output number of clusters found
    !   threshold        : optional, cells below this are noise
    !   n_requested      : optional, number of clusters to request
    !                      (default: 5)
    !   auto_k           : optional, local maxima based auto k detection
    !   auto_k_radius    : optional, smoothing radius for k detection
    ! =================================================================
    subroutine kmeans_grid_cluster(surface, nx, ny, labels, n_clusters, &
                                   threshold, n_requested, auto_k, &
                                   auto_k_radius)
        implicit none
        integer, intent(in)  :: nx, ny
        real(8), intent(in)  :: surface(nx, ny)
        integer, intent(out) :: labels(nx, ny)
        integer, intent(out) :: n_clusters
        real(8), intent(in), optional :: threshold
        integer, intent(in), optional :: n_requested
        logical, intent(in), optional :: auto_k
        integer, intent(in), optional :: auto_k_radius

        real(8) :: thresh
        integer :: n_req
        integer :: i, j, n_obs, idx, iter
        integer :: n_iter_clusters

        ! Observation data
        real(8), allocatable :: obs_x(:,:)    ! (n_obs, 2) — positions
        real(8), allocatable :: centers(:,:)  ! (n_req, 2) — cluster centers
        real(8), allocatable :: dev(:)        ! (n_req) — deviations
        real(8), allocatable :: work(:)       ! (n_obs) — workspace
        integer, allocatable :: assignments(:) ! (n_obs) — cluster IDs
        integer, allocatable :: counts(:)     ! (n_req) — obs per cluster
        integer, allocatable :: obs_gi(:), obs_gj(:) ! grid indices of obs

        ! For initial center selection
        real(8), allocatable :: densities(:)
        integer, allocatable :: sorted_idx(:)
        integer, allocatable :: maxima_labels(:,:)
        real(8), allocatable :: smooth_surface(:,:)
        real(8), allocatable :: smooth_temp(:,:)
        integer :: auto_k_count, k_radius
        
        k_radius = 4
        if (present(auto_k_radius)) k_radius = auto_k_radius

        thresh = 0.05d0
        if (present(threshold)) thresh = threshold
        n_req = 5
        if (present(n_requested)) n_req = n_requested

        if (present(auto_k)) then
            if (auto_k) then
                allocate(maxima_labels(nx, ny))
                allocate(smooth_surface(nx, ny))
                allocate(smooth_temp(nx, ny))
                
                smooth_surface = surface
                
                ! More aggressive standard iterative smoothing to avoid spurious seeds for K-Means
                do iter = 1, 4
                    call smooth_box_filter(smooth_surface, nx, ny, k_radius, smooth_temp)
                    smooth_surface = smooth_temp
                end do

                call find_local_maxima(smooth_surface, nx, ny, thresh, maxima_labels, auto_k_count)
                if (auto_k_count > 0) then
                    ! Cap K to 20 to prevent O(N*K) explosion which freezes the UI
                    if (auto_k_count > 20) then
                        n_req = 20
                        print *, "Auto K-Means detected K=", auto_k_count, " (capped to 20 to prevent UI freeze)"
                    else
                        n_req = auto_k_count
                        print *, "Auto K-Means detected K=", auto_k_count
                    end if
                end if
                deallocate(maxima_labels, smooth_surface, smooth_temp)
            end if
        end if

        ! -----------------------------------------------------------
        ! 1. Count valid cells (above threshold)
        ! -----------------------------------------------------------
        n_obs = 0
        do j = 1, ny
            do i = 1, nx
                if (surface(i, j) > thresh) then
                    n_obs = n_obs + 1
                end if
            end do
        end do

        ! If too few observations, mark all as noise
        if (n_obs < n_req .or. n_obs == 0) then
            labels = KMEANS_LABEL_NOISE
            n_clusters = 0
            return
        end if

        ! Limit clusters to available observations
        n_iter_clusters = min(n_req, n_obs)

        ! -----------------------------------------------------------
        ! 2. Extract observations (cell positions)
        ! -----------------------------------------------------------
        allocate(obs_x(n_obs, 2))
        allocate(obs_gi(n_obs))
        allocate(obs_gj(n_obs))
        allocate(densities(n_obs))

        idx = 0
        do j = 1, ny
            do i = 1, nx
                if (surface(i, j) > thresh) then
                    idx = idx + 1
                    obs_x(idx, 1) = dble(i)
                    obs_x(idx, 2) = dble(j)
                    obs_gi(idx) = i
                    obs_gj(idx) = j
                    densities(idx) = surface(i, j)
                end if
            end do
        end do

        ! -----------------------------------------------------------
        ! 3. Choose initial cluster centers
        !    Strategy: pick the n_iter_clusters cells with highest density
        ! -----------------------------------------------------------
        allocate(centers(n_iter_clusters, 2))
        allocate(sorted_idx(n_obs))

        ! Simple selection: find top-N by density
        call select_top_n_indices(densities, n_obs, n_iter_clusters, &
                                  sorted_idx)

        do i = 1, n_iter_clusters
            idx = sorted_idx(i)
            centers(i, 1) = obs_x(idx, 1)
            centers(i, 2) = obs_x(idx, 2)
        end do

        ! -----------------------------------------------------------
        ! 4. Run K-means
        ! -----------------------------------------------------------
        allocate(assignments(n_obs))
        allocate(dev(n_iter_clusters))
        allocate(work(n_obs))
        allocate(counts(n_iter_clusters))

        call k_means_clustr(obs_x, centers, dev, assignments, work, &
                            counts, n_obs, 2, n_iter_clusters, 1, &
                            n_iter_clusters)

        ! -----------------------------------------------------------
        ! 5. Map labels back to grid
        ! -----------------------------------------------------------
        labels = KMEANS_LABEL_NOISE

        do idx = 1, n_obs
            labels(obs_gi(idx), obs_gj(idx)) = assignments(idx)
        end do

        ! Count actual non-empty clusters
        n_clusters = 0
        do i = 1, n_iter_clusters
            if (counts(i) > 0) then
                n_clusters = n_clusters + 1
            end if
        end do

        ! If some clusters are empty, re-label to be contiguous 1..n_clusters
        if (n_clusters < n_iter_clusters) then
            call relabel_contiguous(labels, nx, ny, n_iter_clusters, &
                                    n_clusters)
        end if

        ! Cleanup
        deallocate(obs_x, obs_gi, obs_gj, densities)
        deallocate(centers, sorted_idx)
        deallocate(assignments, dev, work, counts)

    end subroutine kmeans_grid_cluster

    ! =================================================================
    ! PUBLIC: dbscan_grid_cluster
    !
    ! Grid adapter for DBSCAN.  Takes a 2D surface and produces
    ! a 2D integer label array + cluster count.
    !
    ! Arguments:
    !   surface(nx, ny)  : input density surface
    !   nx, ny           : grid dimensions
    !   labels(nx, ny)   : output cluster labels (1..n_clusters or NOISE)
    !   n_clusters       : output number of clusters found
    !   threshold        : optional, cells below this are noise
    !   eps              : optional, DBSCAN neighbourhood radius
    !                      (default: 3.0 grid cells)
    !   minpts           : optional, minimum points per cluster
    !                      (default: 3)
    ! =================================================================
    subroutine dbscan_grid_cluster(surface, nx, ny, labels, n_clusters, &
                                    threshold, eps, minpts)
        implicit none
        integer, intent(in)  :: nx, ny
        real(8), intent(in)  :: surface(nx, ny)
        integer, intent(out) :: labels(nx, ny)
        integer, intent(out) :: n_clusters
        real(8), intent(in), optional :: threshold
        real(8), intent(in), optional :: eps
        integer, intent(in), optional :: minpts

        real(8) :: thresh, eps_val
        integer :: minpts_val
        integer :: i, j, n_obs, idx

        ! Observation data
        real(8), allocatable :: obs_x(:,:)     ! (n_obs, 2)
        integer, allocatable :: obs_gi(:), obs_gj(:)
        integer, allocatable :: classi(:)      ! cluster assignments
        integer, allocatable :: p_in_classi(:) ! points per cluster

        thresh = 0.05d0
        if (present(threshold)) thresh = threshold
        eps_val = 3.0d0
        if (present(eps)) eps_val = eps
        minpts_val = 3
        if (present(minpts)) minpts_val = minpts

        ! -----------------------------------------------------------
        ! 1. Count valid cells
        ! -----------------------------------------------------------
        n_obs = 0
        do j = 1, ny
            do i = 1, nx
                if (surface(i, j) > thresh) then
                    n_obs = n_obs + 1
                end if
            end do
        end do

        if (n_obs == 0) then
            labels = KMEANS_LABEL_NOISE
            n_clusters = 0
            return
        end if

        ! -----------------------------------------------------------
        ! 2. Extract observations
        ! -----------------------------------------------------------
        allocate(obs_x(n_obs, 2))
        allocate(obs_gi(n_obs))
        allocate(obs_gj(n_obs))

        idx = 0
        do j = 1, ny
            do i = 1, nx
                if (surface(i, j) > thresh) then
                    idx = idx + 1
                    obs_x(idx, 1) = dble(i)
                    obs_x(idx, 2) = dble(j)
                    obs_gi(idx) = i
                    obs_gj(idx) = j
                end if
            end do
        end do

        ! -----------------------------------------------------------
        ! 3. Run DBSCAN
        ! -----------------------------------------------------------
        allocate(classi(n_obs))
        allocate(p_in_classi(n_obs))

        call dbscan_cluster(obs_x, n_obs, 2, minpts_val, eps_val, &
                            classi, p_in_classi, n_clusters)

        ! -----------------------------------------------------------
        ! 4. Map labels back to grid
        !    DBSCAN returns: 0 = noise, 1..n = clusters
        !    We map: 0 → KMEANS_LABEL_NOISE, 1..n → 1..n
        ! -----------------------------------------------------------
        labels = KMEANS_LABEL_NOISE

        do idx = 1, n_obs
            if (classi(idx) > 0) then
                labels(obs_gi(idx), obs_gj(idx)) = classi(idx)
            else
                labels(obs_gi(idx), obs_gj(idx)) = KMEANS_LABEL_NOISE
            end if
        end do

        ! Cleanup
        deallocate(obs_x, obs_gi, obs_gj)
        deallocate(classi, p_in_classi)

    end subroutine dbscan_grid_cluster

    ! =================================================================
    ! PRIVATE: select_top_n_indices
    !
    ! Find the indices of the N largest values in an array.
    ! Simple O(N*n) selection (fine for grid-sized data).
    ! =================================================================
    subroutine select_top_n_indices(vals, n_total, n_top, result_idx)
        implicit none
        integer, intent(in)  :: n_total, n_top
        real(8), intent(in)  :: vals(n_total)
        integer, intent(out) :: result_idx(n_total)

        logical :: used(n_total)
        integer :: i, k, best_idx
        real(8) :: best_val

        used = .false.

        do k = 1, n_top
            best_val = -1.0d30
            best_idx = 1
            do i = 1, n_total
                if (.not. used(i) .and. vals(i) > best_val) then
                    best_val = vals(i)
                    best_idx = i
                end if
            end do
            result_idx(k) = best_idx
            used(best_idx) = .true.
        end do

    end subroutine select_top_n_indices

    ! =================================================================
    ! PRIVATE: relabel_contiguous
    !
    ! Re-labels a 2D label array so cluster IDs are contiguous 1..N,
    ! removing gaps from empty clusters.
    ! =================================================================
    subroutine relabel_contiguous(labels, nx, ny, max_label, n_actual)
        implicit none
        integer, intent(inout) :: labels(nx, ny)
        integer, intent(in)    :: nx, ny, max_label
        integer, intent(out)   :: n_actual

        integer :: old_to_new(max_label)
        integer :: i, j, k, new_id
        logical :: has_cell(max_label)

        ! Find which labels actually appear
        has_cell = .false.
        do j = 1, ny
            do i = 1, nx
                k = labels(i, j)
                if (k >= 1 .and. k <= max_label) then
                    has_cell(k) = .true.
                end if
            end do
        end do

        ! Build mapping
        new_id = 0
        do k = 1, max_label
            if (has_cell(k)) then
                new_id = new_id + 1
                old_to_new(k) = new_id
            else
                old_to_new(k) = 0
            end if
        end do
        n_actual = new_id

        ! Apply mapping
        do j = 1, ny
            do i = 1, nx
                k = labels(i, j)
                if (k >= 1 .and. k <= max_label) then
                    labels(i, j) = old_to_new(k)
                end if
            end do
        end do

    end subroutine relabel_contiguous

    ! =================================================================
    ! RAW ALGORITHM: k_means_clustr
    !
    ! K-means clustering (Euclidean Cluster Analysis).
    !
    ! Original FORTRAN77 by David Sparks (AS 58).
    ! FORTRAN90 version by John Burkardt.
    !
    ! Parameters:
    !   X(I,J)   : observed data (I observations, J variables)
    !   D(K,J)   : cluster centers (input: initial, output: final)
    !   DEV(K)   : sums of squared deviations per cluster
    !   B(I)     : cluster assignment for each observation
    !   F(I)     : workspace
    !   E(K)     : number of observations per cluster
    !   I        : number of observations
    !   J        : number of variables
    !   N        : number of clusters
    !   NZ       : minimum observations per cluster
    !   K        : maximum number of clusters
    ! =================================================================
    subroutine k_means_clustr(x, d, dev, b, f, e, i, j, n, nz, k)

        implicit none

        integer, intent(in) :: i
        integer, intent(in) :: k

        integer :: b(i)
        real(8), parameter :: big = 1.0D+10
        real(8) :: d(k,j)
        real(8) :: da, db, dc, de
        real(8) :: dev(k)
        integer :: e(k)
        real(8) :: f(i)
        real(8) :: fl, fm, fq
        integer :: ic, id, ie, ig, ii, ij, ik, il
        integer :: in, ip, ir, is, it, iu, iw
        integer, intent(in) :: j
        integer, intent(in) :: n
        integer, intent(in) :: nz
        real(8) :: x(i,j)

        e(1:n) = 0
        !
        !  For each observation, calculate the distance from each cluster
        !  center, and assign to the nearest.
        !
        do ic = 1, i

            f(ic) = 0.0D+00
            da = big

            do id = 1, n

                db = 0.0D+00
                do ie = 1, j
                    dc = x(ic,ie) - d(id,ie)
                    db = db + dc * dc
                end do

                if ( db < da ) then
                    da = db
                    b(ic) = id
                end if

            end do

            ig = b(ic)
            e(ig) = e(ig) + 1

        end do
        !
        !  Calculate the mean and sum of squares for each cluster.
        !
        dev(1:n) = 0.0D+00
        d(1:n,1:j) = 0.0D+00

        do ic = 1, i
            ig = b(ic)
            d(ig,1:j) = d(ig,1:j) + x(ic,1:j)
        end do

        do ij = 1, j
            do ii = 1, n
                d(ii,ij) = d(ii,ij) / real( e(ii), kind = 8 )
            end do
        end do

        do ij = 1, j
            do ik = 1, i
                il = b(ik)
                da = x(ik,ij) - d(il,ij)
                db = da * da
                f(ik) = f(ik) + db
                dev(il) = dev(il) + db
            end do
        end do

        do ik = 1, i
            il = b(ik)
            fl = e(il)
            if ( 1 < e(il) ) then
                f(ik) = f(ik) * fl / ( fl - 1.0D+00 )
            end if
        end do
        !
        !  Examine each observation in turn to see if it should be
        !  reassigned to a different cluster.
        !
        do

            iw = 0

            do ik = 1, i

                il = b(ik)
                ir = il
                !
                !  If the number of cluster points is less than or equal to the
                !  specified minimum, NZ, then bypass this iteration.
                !
                if ( nz < e(il) ) then

                    fl = e(il)
                    dc = f(ik)

                    do in = 1, n

                        if ( in /= il ) then

                            fm = e(in)
                            fm = fm / ( fm + 1.0D+00 )

                            de = 0.0D+00
                            do ip = 1, j
                                da = x(ik,ip) - d(in,ip)
                                de = de + da * da * fm
                            end do

                            if ( de < dc ) then
                                dc = de
                                ir = in
                            end if

                        end if

                    end do
                    !
                    !  Reassignment is made here if necessary.
                    !
                    if ( ir /= il ) then

                        fq = e(ir)
                        dev(il) = dev(il) - f(ik)
                        dev(ir) = dev(ir) + dc
                        e(ir) = e(ir) + 1
                        e(il) = e(il) - 1

                        do is = 1, j
                            d(il,is) = ( d(il,is) * fl - x(ik,is) ) &
                                       / ( fl - 1.0D+00 )
                            d(ir,is) = ( d(ir,is) * fq + x(ik,is) ) &
                                       / ( fq + 1.0D+00 )
                        end do

                        b(ik) = ir

                        do it = 1, i

                            ij = b(it)

                            if ( ij == il .or. ij == ir ) then
                                f(it) = 0.0D+00
                                do iu = 1, j
                                    da = x(it,iu) - d(ij,iu)
                                    f(it) = f(it) + da * da
                                end do
                                fl = e(ij)
                                f(it) = f(it) * fl / ( fl - 1.0D+00 )
                            end if

                        end do

                        iw = iw + 1

                    end if

                end if

            end do
            !
            !  If any reassignments were made on this pass, then do another pass.
            !
            if ( iw == 0 ) then
                exit
            end if

        end do

        return
    end subroutine k_means_clustr

    ! =================================================================
    ! RAW ALGORITHM: dbscan_cluster
    !
    ! DBSCAN density-based clustering.
    ! Based on Ester et al. (1996).
    !
    ! Parameters:
    !   x(obs, var)       : input observations
    !   obs               : number of observations
    !   var               : number of variables
    !   minpts            : minimum neighbourhood size for a cluster
    !   eps               : neighbourhood radius
    !   classi(obs)       : output cluster labels (0=noise, 1..n=clusters)
    !   p_in_classi(obs)  : points per cluster (noise count at index obs)
    !   cluster_count     : output number of clusters
    ! =================================================================
    subroutine dbscan_cluster(x, obs, var, minpts, eps, classi, &
                              p_in_classi, cluster_count)

        implicit none

        integer, intent(in) :: obs
        integer, intent(in) :: var

        real(8), intent(in)    :: eps
        integer, intent(in)    :: minpts

        real(8), dimension(:,:), intent(in) :: x(obs, var)
        integer, dimension(:), intent(out) :: classi(obs)
        integer, dimension(:), intent(out) :: p_in_classi(obs)
        real(8), dimension(:,:), allocatable :: distm

        integer, intent(out) :: cluster_count
        integer, dimension(:), allocatable :: seeds, seeds_of_seed
        integer :: seed_count, seed_of_seed_count
        integer :: i, j, k, l, m

        cluster_count = 1
        allocate(distm(obs,obs), seeds(obs), seeds_of_seed(obs))
        call distance_matrix(x, obs, var, x, obs, distm, 2)

        where (distm <= eps)
            distm = 1
        elsewhere
            distm = 0
        end where

        classi(:) = -1
        p_in_classi(:) = 0

        do i = 1, obs
            if (classi(i) == -1) then
                seed_count = 0
                seeds(:) = 0
                do j = 1, obs
                    if (distm(i,j) == 1) then
                        seed_count = seed_count + 1
                        seeds(seed_count) = j
                    end if
                end do
                if (seed_count < minpts) then
                    classi(i) = 0  ! Noise
                else
                    classi(seeds(1:seed_count)) = cluster_count

                    where (seeds == i) seeds = 0
                    do k = 1, seed_count
                        if (seeds(k) /= 0) then
                            seed_of_seed_count = 0
                            seeds_of_seed(:) = 0
                            do l = 1, obs
                                if (distm(seeds(k),l) == 1) then
                                    seed_of_seed_count = &
                                        seed_of_seed_count + 1
                                    seeds_of_seed(seed_of_seed_count) = l
                                end if
                            end do

                            if (seed_of_seed_count >= minpts) then
                                do m = 1, seed_of_seed_count
                                    if ((classi(seeds_of_seed(m)) == -1) &
                                        .or. &
                                        (classi(seeds_of_seed(m)) == 0)) &
                                    then
                                        if (classi(seeds_of_seed(m)) &
                                            == -1) then
                                            seed_count = seed_count + 1
                                            seeds(seed_count) = &
                                                seeds_of_seed(m)
                                        end if
                                        classi(seeds_of_seed(m)) = &
                                            cluster_count
                                    end if
                                end do
                            end if
                            seeds(k) = 0
                        end if
                    end do
                    cluster_count = cluster_count + 1
                end if
            end if
        end do
        cluster_count = cluster_count - 1

        do i = 1, obs
            if (classi(i) == 0) then
                p_in_classi(obs) = p_in_classi(obs) + 1
            else
                p_in_classi(classi(i)) = p_in_classi(classi(i)) + 1
            end if
        end do

        deallocate(distm, seeds, seeds_of_seed)

        return
    end subroutine dbscan_cluster

    ! =================================================================
    ! HELPER: distance_matrix
    !
    ! Compute pairwise Euclidean distance matrix between two sets
    ! of observations.
    !
    ! Parameters:
    !   x1(n1, var)  : first set of observations
    !   n1           : number of observations in x1
    !   var          : number of variables
    !   x2(n2, var)  : second set of observations
    !   n2           : number of observations in x2
    !   distm(n1,n2) : output distance matrix
    !   metric       : distance metric (2 = Euclidean)
    ! =================================================================
    subroutine distance_matrix(x1, n1, var, x2, n2, distm, metric)
        implicit none
        integer, intent(in) :: n1, var, n2, metric
        real(8), intent(in) :: x1(n1, var), x2(n2, var)
        real(8), intent(out) :: distm(n1, n2)

        integer :: i, j, k
        real(8) :: diff, sum_sq

        do j = 1, n2
            do i = 1, n1
                sum_sq = 0.0d0
                do k = 1, var
                    diff = x1(i, k) - x2(j, k)
                    sum_sq = sum_sq + diff * diff
                end do
                if (metric == 2) then
                    distm(i, j) = sqrt(sum_sq)
                else
                    distm(i, j) = sum_sq
                end if
            end do
        end do

    end subroutine distance_matrix

    ! =================================================================
    ! PUBLIC: local_box_filter
    ! A simple local box blur to heavily smooth a surface before detecting K
    ! =================================================================
    subroutine local_box_filter(input, nx, ny, radius, output)
        implicit none
        integer, intent(in) :: nx, ny, radius
        real(8), intent(in) :: input(nx, ny)
        real(8), intent(out) :: output(nx, ny)
        integer :: i, j, di, dj, ni, nj
        real(8) :: sum, count
        
        do j = 1, ny
            do i = 1, nx
                sum = 0.0d0
                count = 0.0d0
                do dj = -radius, radius
                    do di = -radius, radius
                        ni = i + di
                        nj = j + dj
                        if (ni >= 1 .and. ni <= nx .and. nj >= 1 .and. nj <= ny) then
                            sum = sum + input(ni, nj)
                            count = count + 1.0d0
                        end if
                    end do
                end do
                output(i, j) = sum / count
            end do
        end do
    end subroutine local_box_filter

end module mod_kmeans
