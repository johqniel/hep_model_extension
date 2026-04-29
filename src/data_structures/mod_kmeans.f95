 module mod_kmeans

    use mod_watershed, only: find_local_maxima, smooth_box_filter
 
     use mod_config, only: world_config
    use mod_calculations, only: calculate_grid_pos
    implicit none

    integer, parameter :: KMEANS_LABEL_NOISE = -1

    contains


        ! =========================================================================
        ! f_1: dbscan_agents
        ! Returns a padded 3D matrix (2, max_m, n_clusters) containing (x,y).
        ! Empty slots are padded with -1000.0d0
        ! =========================================================================
        subroutine dbscan_agents(pos_list, eps, minpts, cluster_matrix, n_clusters)
            real(8), intent(in) :: pos_list(:,:) ! 2 x N
            real(8), intent(in) :: eps
            integer, intent(in) :: minpts
            real(8), allocatable, intent(out) :: cluster_matrix(:,:,:) ! (2, max_m, n_clusters)
            integer, intent(out) :: n_clusters
            
            integer :: n_agents, i, c
            integer, allocatable :: labels(:), cluster_counts(:)
            integer :: max_m
            
            n_agents = size(pos_list, 2)
            if (n_agents == 0) then
                n_clusters = 0
                allocate(cluster_matrix(2, 0, 0))
                return
            end if

            allocate(labels(n_agents))
            
            ! Call actual point-based DBSCAN logic
            call run_point_dbscan(pos_list, eps, minpts, labels, n_clusters)
            
            if (n_clusters <= 0) then
                allocate(cluster_matrix(2, 0, 0))
                deallocate(labels)
                return
            end if

            ! Determine max_m (largest cluster size)
            allocate(cluster_counts(n_clusters))
            cluster_counts = 0
            do i = 1, n_agents
                if (labels(i) > 0) then
                    cluster_counts(labels(i)) = cluster_counts(labels(i)) + 1
                end if
            end do
            max_m = maxval(cluster_counts)
            
            ! Allocate output matrix and pad with dummy coordinate -1000.0
            allocate(cluster_matrix(2, max_m, n_clusters))
            cluster_matrix = -1000.0d0 
            
            cluster_counts = 0 ! Reset to use as insertion index
            do i = 1, n_agents
                c = labels(i)
                if (c > 0) then
                    cluster_counts(c) = cluster_counts(c) + 1
                    cluster_matrix(1, cluster_counts(c), c) = pos_list(1, i)
                    cluster_matrix(2, cluster_counts(c), c) = pos_list(2, i)
                end if
            end do
            
            deallocate(labels, cluster_counts)
        end subroutine dbscan_agents


        ! =========================================================================
        ! f_2: kmeans_agents
        ! Returns a padded 3D matrix (2, max_m, k) containing (x,y).
        ! =========================================================================
        subroutine kmeans_agents(pos_list, k, cluster_matrix)
            real(8), intent(in) :: pos_list(:,:) ! 2 x N
            integer, intent(in) :: k
            real(8), allocatable, intent(out) :: cluster_matrix(:,:,:) ! (2, max_m, k)
            
            integer :: n_agents, i, c, max_m, n_clusters
            integer, allocatable :: labels(:), cluster_counts(:)
            
            n_agents = size(pos_list, 2)
            if (n_agents == 0 .or. k <= 0) then
                allocate(cluster_matrix(2, 0, 0))
                return
            end if

            allocate(labels(n_agents))
            
            ! Call actual point-based K-Means algorithm
            call run_point_kmeans(pos_list, k, labels, n_clusters)
            
            if (n_clusters <= 0) then
                allocate(cluster_matrix(2, 0, 0))
                deallocate(labels)
                return
            end if

            ! Determine max_m (largest cluster size)
            allocate(cluster_counts(n_clusters))
            cluster_counts = 0
            do i = 1, n_agents
                if (labels(i) > 0) then
                    cluster_counts(labels(i)) = cluster_counts(labels(i)) + 1
                end if
            end do
            max_m = maxval(cluster_counts)
            
            ! Allocate and populate
            allocate(cluster_matrix(2, max_m, n_clusters))
            cluster_matrix = -1000.0d0
            
            cluster_counts = 0 
            do i = 1, n_agents
                c = labels(i)
                if (c > 0) then
                    cluster_counts(c) = cluster_counts(c) + 1
                    cluster_matrix(1, cluster_counts(c), c) = pos_list(1, i)
                    cluster_matrix(2, cluster_counts(c), c) = pos_list(2, i)
                end if
            end do
            
            deallocate(labels, cluster_counts)
        end subroutine kmeans_agents


        ! =========================================================================
        ! f_3: auto_k_means_agents
        ! Counts peaks on the provided (pre-smoothed) density surface to find K,
        ! then calls f_2.
        ! The surface is expected to be human_density_smoothed, already smoothed
        ! by update_density_and_hep_grid using human_density_smoothing_radius.
        ! =========================================================================
        subroutine auto_k_means_agents(pos_list, surface, nx, ny, cluster_matrix, estimated_k)
            use mod_watershed, only: find_local_maxima
            real(8), intent(in) :: pos_list(:,:)
            integer, intent(in) :: nx, ny
            real(8), intent(in) :: surface(nx, ny)
            real(8), allocatable, intent(out) :: cluster_matrix(:,:,:)
            integer, intent(out) :: estimated_k
            
            integer, allocatable :: maxima_labels(:,:)

            allocate(maxima_labels(nx, ny))

            ! Use the pre-smoothed surface directly for peak detection
            call find_local_maxima(surface, nx, ny, 0.05d0, maxima_labels, estimated_k)
            
            ! Cap K to 20 to prevent performance issues
            if (estimated_k > 20) estimated_k = 20
            if (estimated_k <= 0) estimated_k = 1
            
            call kmeans_agents(pos_list, estimated_k, cluster_matrix)

            deallocate(maxima_labels)
        end subroutine auto_k_means_agents


        ! =========================================================================
        ! f_4: grid_cluster_vote
        ! Takes the agent cluster matrix and assigns grid cells to clusters 
        ! based on a majority vote of the agents residing in that cell.
        ! Fits into `cell_cluster_map(nx, ny)`.
        ! =========================================================================
        subroutine grid_cluster_vote(cluster_matrix, nx, ny, config, cell_cluster_map)
            real(8), intent(in) :: cluster_matrix(:,:,:) ! (2, max_m, n_clusters)
            integer, intent(in) :: nx, ny
            type(world_config), intent(in) :: config
            integer, allocatable, intent(out) :: cell_cluster_map(:,:)
            
            integer :: n_clusters, max_m, c, m, gx, gy, best_c, max_votes
            integer, allocatable :: cell_votes(:,:,:) ! (nx, ny, n_clusters)
            real(8) :: px, py
            
            n_clusters = size(cluster_matrix, 3)
            if (n_clusters == 0) then
                allocate(cell_cluster_map(nx, ny))
                cell_cluster_map = -1 ! CLUSTER_NOISE
                return
            end if

            max_m      = size(cluster_matrix, 2)
            
            allocate(cell_cluster_map(nx, ny))
            cell_cluster_map = -2 ! CLUSTER_UNASSIGNED
            
            allocate(cell_votes(nx, ny, n_clusters))
            cell_votes = 0
            
            ! 1. Map every clustered agent to a grid cell and cast a vote
            do c = 1, n_clusters
                do m = 1, max_m
                    px = cluster_matrix(1, m, c)
                    py = cluster_matrix(2, m, c)
                    
                    ! Ignore dummy padded values
                    if (px > -999.0d0 .and. py > -999.0d0) then
                        call calculate_grid_pos(px, py, gx, gy, config)
                        if (gx >= 1 .and. gx <= nx .and. gy >= 1 .and. gy <= ny) then
                            cell_votes(gx, gy, c) = cell_votes(gx, gy, c) + 1
                        end if
                    end if
                end do
            end do
            
            ! 2. Resolve votes for each grid cell
            do gy = 1, ny
                do gx = 1, nx
                    max_votes = 0
                    best_c    = -1  ! CLUSTER_NOISE
                    
                    do c = 1, n_clusters
                        if (cell_votes(gx, gy, c) > max_votes) then
                            max_votes = cell_votes(gx, gy, c)
                            best_c = c
                        end if
                    end do
                    
                    if (max_votes > 0) then
                        cell_cluster_map(gx, gy) = best_c
                    else
                        cell_cluster_map(gx, gy) = -1 ! CLUSTER_NOISE
                    end if
                end do
            end do
            ! 3. Post-process: fill holes using convex hull around each cluster
            call apply_convex_hull_fill(cell_cluster_map, nx, ny, n_clusters)
            
            deallocate(cell_votes)
        end subroutine grid_cluster_vote


        ! =========================================================================
        ! Internal Helpers: Wrappers for Point-based Clustering
        ! =========================================================================

        subroutine run_point_kmeans(pos_list, k, labels, n_clusters)
            real(8), intent(in) :: pos_list(:,:) ! 2 x N
            integer, intent(in) :: k
            integer, intent(out) :: labels(:)
            integer, intent(out) :: n_clusters
            
            integer :: n_agents, i
            real(8), allocatable :: obs(:,:), centers(:,:), dev(:), work(:)
            integer, allocatable :: counts(:)

            n_agents = size(pos_list, 2)
            if (n_agents == 0) then
                n_clusters = 0
                return
            end if

            allocate(obs(n_agents, 2))
            do i = 1, n_agents
                obs(i, 1) = pos_list(1, i)
                obs(i, 2) = pos_list(2, i)
            end do

            allocate(centers(k, 2))
            allocate(dev(k))
            allocate(work(n_agents))
            allocate(counts(k))

            ! Initialise centers using Farthest First Traversal
            ! Note: we don't have densities for agents, so we use a dummy constant density
            block
                real(8), allocatable :: dummy_densities(:)
                allocate(dummy_densities(n_agents))
                dummy_densities = 1.0d0
                call kmeans_farthest_first_init(dummy_densities, obs, n_agents, k, centers)
                deallocate(dummy_densities)
            end block

            call k_means_clustr(obs, centers, dev, labels, work, counts, n_agents, 2, k, 1, k)

            n_clusters = 0
            do i = 1, k
                if (counts(i) > 0) n_clusters = n_clusters + 1
            end do

            if (n_clusters < k) then
                call relabel_contiguous_1d(labels, n_agents, k, n_clusters)
            end if

            deallocate(obs, centers, dev, work, counts)
        end subroutine run_point_kmeans

        subroutine run_point_dbscan(pos_list, eps, minpts, labels, n_clusters)
            real(8), intent(in) :: pos_list(:,:) ! 2 x N
            real(8), intent(in) :: eps
            integer, intent(in) :: minpts
            integer, intent(out) :: labels(:)
            integer, intent(out) :: n_clusters
            
            integer :: n_agents, i
            real(8), allocatable :: obs(:,:)
            integer, allocatable :: p_in_classi(:)

            n_agents = size(pos_list, 2)
            if (n_agents == 0) then
                n_clusters = 0
                return
            end if

            allocate(obs(n_agents, 2))
            do i = 1, n_agents
                obs(i, 1) = pos_list(1, i)
                obs(i, 2) = pos_list(2, i)
            end do

            allocate(p_in_classi(n_agents))

            call dbscan_cluster(obs, n_agents, 2, minpts, eps, labels, p_in_classi, n_clusters)

            ! Map DBSCAN 0 (noise) to -1
            do i = 1, n_agents
                if (labels(i) == 0) labels(i) = -1
            end do

            deallocate(obs, p_in_classi)
        end subroutine run_point_dbscan

        ! =========================================================================
        ! Raw Algorithms
        ! =========================================================================

        subroutine k_means_clustr(x, d, dev, b, f, e, i, j, n, nz, k)
            implicit none
            integer, intent(in) :: i, j, n, nz, k
            real(8), intent(in) :: x(i,j)
            real(8), intent(inout) :: d(k,j)
            real(8), intent(out) :: dev(k)
            integer, intent(out) :: b(i)
            real(8), intent(out) :: f(i)
            integer, intent(out) :: e(k)
            real(8), parameter :: big = 1.0D+10
            real(8) :: da, db, dc, de, fl, fm, fq
            integer :: ic, id, ie, ig, ii, ij, ik, il, in, ip, ir, is, it, iu, iw
            e(1:n) = 0
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
            dev(1:n) = 0.0D+00
            d(1:n,1:j) = 0.0D+00
            do ic = 1, i
                ig = b(ic)
                d(ig,1:j) = d(ig,1:j) + x(ic,1:j)
            end do
            do ij = 1, j
                do ii = 1, n
                    if (e(ii) > 0) d(ii,ij) = d(ii,ij) / real( e(ii), kind = 8 )
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
            do
                iw = 0
                do ik = 1, i
                    il = b(ik)
                    ir = il
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
                        if ( ir /= il ) then
                            fq = e(ir)
                            dev(il) = dev(il) - f(ik)
                            dev(ir) = dev(ir) + dc
                            e(ir) = e(ir) + 1
                            e(il) = e(il) - 1
                            do is = 1, j
                                d(il,is) = ( d(il,is) * fl - x(ik,is) ) / ( fl - 1.0D+00 )
                                d(ir,is) = ( d(ir,is) * fq + x(ik,is) ) / ( fq + 1.0D+00 )
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
                                    if (fl > 1.0d0) f(it) = f(it) * fl / ( fl - 1.0D+00 )
                                end if
                            end do
                            iw = iw + 1
                        end if
                    end if
                end do
                if ( iw == 0 ) exit
            end do
        end subroutine k_means_clustr

        subroutine dbscan_cluster(x, obs, var, minpts, eps, classi, p_in_classi, cluster_count)
            implicit none
            integer, intent(in) :: obs, var, minpts
            real(8), intent(in) :: eps
            real(8), intent(in) :: x(obs, var)
            integer, intent(out) :: classi(obs), p_in_classi(obs), cluster_count
            real(8), allocatable :: distm(:,:)
            integer, allocatable :: seeds(:), seeds_of_seed(:)
            integer :: seed_count, seed_of_seed_count, i, j, k, l, m
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
                        classi(i) = 0
                    else
                        classi(seeds(1:seed_count)) = cluster_count
                        where (seeds == i) seeds = 0
                        do k = 1, seed_count
                            if (seeds(k) /= 0) then
                                seed_of_seed_count = 0
                                seeds_of_seed(:) = 0
                                do l = 1, obs
                                    if (distm(seeds(k),l) == 1) then
                                        seed_of_seed_count = seed_of_seed_count + 1
                                        seeds_of_seed(seed_of_seed_count) = l
                                    end if
                                end do
                                if (seed_of_seed_count >= minpts) then
                                    do m = 1, seed_of_seed_count
                                        if ((classi(seeds_of_seed(m)) == -1) .or. (classi(seeds_of_seed(m)) == 0)) then
                                            if (classi(seeds_of_seed(m)) == -1) then
                                                seed_count = seed_count + 1
                                                seeds(seed_count) = seeds_of_seed(m)
                                            end if
                                            classi(seeds_of_seed(m)) = cluster_count
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
                else if (classi(i) > 0) then
                    p_in_classi(classi(i)) = p_in_classi(classi(i)) + 1
                end if
            end do
            deallocate(distm, seeds, seeds_of_seed)
        end subroutine dbscan_cluster

        subroutine kmeans_farthest_first_init(densities, obs_x, n_obs, k, centers)
            implicit none
            integer, intent(in)  :: n_obs, k
            real(8), intent(in)  :: densities(n_obs)
            real(8), intent(in)  :: obs_x(n_obs, 2)
            real(8), intent(out) :: centers(k, 2)
            integer :: c, i, j, initial_peak, best_c
            real(8) :: best_val, max_min_dist, min_dist, dist_sq, dx, dy
            best_val = -1.0d30
            initial_peak = 1
            do i = 1, n_obs
                if (densities(i) > best_val) then
                    best_val = densities(i)
                    initial_peak = i
                end if
            end do
            centers(1, 1) = obs_x(initial_peak, 1)
            centers(1, 2) = obs_x(initial_peak, 2)
            do c = 2, k
                max_min_dist = -1.0d0
                best_c = 1
                do i = 1, n_obs
                    min_dist = 1.0d30
                    do j = 1, c - 1
                        dx = obs_x(i, 1) - centers(j, 1)
                        dy = obs_x(i, 2) - centers(j, 2)
                        dist_sq = (dx * dx) + (dy * dy)
                        if (dist_sq < min_dist) min_dist = dist_sq
                    end do
                    if (min_dist > max_min_dist) then
                        max_min_dist = min_dist
                        best_c = i
                    end if
                end do
                centers(c, 1) = obs_x(best_c, 1)
                centers(c, 2) = obs_x(best_c, 2)
            end do
        end subroutine kmeans_farthest_first_init

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

        subroutine relabel_contiguous_1d(labels, n_obs, max_label, n_actual)
            implicit none
            integer, intent(in) :: n_obs, max_label
            integer, intent(inout) :: labels(n_obs)
            integer, intent(out) :: n_actual
            integer :: old_to_new(max_label)
            integer :: i, k, new_id
            logical :: has_point(max_label)
            has_point = .false.
            do i = 1, n_obs
                k = labels(i)
                if (k >= 1 .and. k <= max_label) has_point(k) = .true.
            end do
            new_id = 0
            do k = 1, max_label
                if (has_point(k)) then
                    new_id = new_id + 1
                    old_to_new(k) = new_id
                else
                    old_to_new(k) = 0
                end if
            end do
            n_actual = new_id
            do i = 1, n_obs
                k = labels(i)
                if (k >= 1 .and. k <= max_label) labels(i) = old_to_new(k)
            end do
        end subroutine relabel_contiguous_1d

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
            integer :: n_req, i, j, n_obs, idx, iter, n_iter_clusters
            real(8), allocatable :: obs_x(:,:), centers(:,:), dev(:), work(:), densities(:)
            integer, allocatable :: assignments(:), counts(:), obs_gi(:), obs_gj(:)
            integer, allocatable :: maxima_labels(:,:)
            real(8), allocatable :: smooth_surface(:,:), smooth_temp(:,:)
            integer :: auto_k_count, k_radius
            k_radius = 4
            if (present(auto_k_radius)) k_radius = auto_k_radius
            thresh = 0.05d0
            if (present(threshold)) thresh = threshold
            n_req = 5
            if (present(n_requested)) n_req = n_requested
            if (present(auto_k)) then
                if (auto_k) then
                    allocate(maxima_labels(nx, ny), smooth_surface(nx, ny), smooth_temp(nx, ny))
                    smooth_surface = surface
                    do iter = 1, 4
                        call smooth_box_filter(smooth_surface, nx, ny, k_radius, smooth_temp)
                        smooth_surface = smooth_temp
                    end do
                    call find_local_maxima(smooth_surface, nx, ny, thresh, maxima_labels, auto_k_count)
                    if (auto_k_count > 0) then
                        n_req = min(auto_k_count, 20)
                    end if
                    deallocate(maxima_labels, smooth_surface, smooth_temp)
                end if
            end if
            n_obs = 0
            do j = 1, ny
                do i = 1, nx
                    if (surface(i, j) > thresh) n_obs = n_obs + 1
                end do
            end do
            if (n_obs < n_req .or. n_obs == 0) then
                labels = KMEANS_LABEL_NOISE
                n_clusters = 0
                return
            end if
            n_iter_clusters = min(n_req, n_obs)
            allocate(obs_x(n_obs, 2), obs_gi(n_obs), obs_gj(n_obs), densities(n_obs))
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
            allocate(centers(n_iter_clusters, 2))
            call kmeans_farthest_first_init(densities, obs_x, n_obs, n_iter_clusters, centers)
            allocate(assignments(n_obs), dev(n_iter_clusters), work(n_obs), counts(n_iter_clusters))
            call k_means_clustr(obs_x, centers, dev, assignments, work, counts, &
                                n_obs, 2, n_iter_clusters, 1, n_iter_clusters)
            labels = KMEANS_LABEL_NOISE
            do idx = 1, n_obs
                labels(obs_gi(idx), obs_gj(idx)) = assignments(idx)
            end do
            n_clusters = 0
            do i = 1, n_iter_clusters
                if (counts(i) > 0) n_clusters = n_clusters + 1
            end do
            if (n_clusters < n_iter_clusters) then
                call relabel_contiguous(labels, nx, ny, n_iter_clusters, n_clusters)
            end if
            deallocate(obs_x, obs_gi, obs_gj, densities, centers, assignments, dev, work, counts)
        end subroutine kmeans_grid_cluster

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
            integer :: minpts_val, i, j, n_obs, idx
            real(8), allocatable :: obs_x(:,:)
            integer, allocatable :: obs_gi(:), obs_gj(:), classi(:), p_in_classi(:)
            thresh = 0.05d0
            if (present(threshold)) thresh = threshold
            eps_val = 3.0d0
            if (present(eps)) eps_val = eps
            minpts_val = 3
            if (present(minpts)) minpts_val = minpts
            n_obs = 0
            do j = 1, ny
                do i = 1, nx
                    if (surface(i, j) > thresh) n_obs = n_obs + 1
                end do
            end do
            if (n_obs == 0) then
                labels = KMEANS_LABEL_NOISE
                n_clusters = 0
                return
            end if
            allocate(obs_x(n_obs, 2), obs_gi(n_obs), obs_gj(n_obs))
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
            allocate(classi(n_obs), p_in_classi(n_obs))
            call dbscan_cluster(obs_x, n_obs, 2, minpts_val, eps_val, classi, p_in_classi, n_clusters)
            labels = KMEANS_LABEL_NOISE
            do idx = 1, n_obs
                if (classi(idx) > 0) then
                    labels(obs_gi(idx), obs_gj(idx)) = classi(idx)
                else
                    labels(obs_gi(idx), obs_gj(idx)) = KMEANS_LABEL_NOISE
                end if
            end do
            deallocate(obs_x, obs_gi, obs_gj, classi, p_in_classi)
        end subroutine dbscan_grid_cluster

        subroutine relabel_contiguous(labels, nx, ny, max_label, n_actual)
            implicit none
            integer, intent(inout) :: labels(nx, ny)
            integer, intent(in)    :: nx, ny, max_label
            integer, intent(out)   :: n_actual
            integer :: old_to_new(max_label)
            integer :: i, j, k, new_id
            logical :: has_cell(max_label)
            has_cell = .false.
            do j = 1, ny
                do i = 1, nx
                    k = labels(i, j)
                    if (k >= 1 .and. k <= max_label) has_cell(k) = .true.
                end do
            end do
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
            do j = 1, ny
                do i = 1, nx
                    k = labels(i, j)
                    if (k >= 1 .and. k <= max_label) labels(i, j) = old_to_new(k)
                end do
            end do
        end subroutine relabel_contiguous

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


        ! =========================================================================
        ! Convex Hull & Point-in-Polygon Fill Helpers
        ! =========================================================================
        real(8) function cross_product(ox, oy, ax, ay, bx, by)
            implicit none
            real(8), intent(in) :: ox, oy, ax, ay, bx, by
            cross_product = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)
        end function cross_product

        subroutine sort_points(pts_x, pts_y, n)
            implicit none
            integer, intent(in) :: n
            real(8), intent(inout) :: pts_x(n), pts_y(n)
            integer :: i, j
            real(8) :: key_x, key_y
            
            do i = 2, n
                key_x = pts_x(i)
                key_y = pts_y(i)
                j = i - 1
                do while (j > 0)
                    if (pts_x(j) > key_x .or. (pts_x(j) == key_x .and. pts_y(j) > key_y)) then
                        pts_x(j+1) = pts_x(j)
                        pts_y(j+1) = pts_y(j)
                        j = j - 1
                    else
                        exit
                    end if
                end do
                pts_x(j+1) = key_x
                pts_y(j+1) = key_y
            end do
        end subroutine sort_points

        subroutine get_convex_hull(pts_x, pts_y, n, hull_x, hull_y, h)
            implicit none
            integer, intent(in) :: n
            real(8), intent(inout) :: pts_x(n), pts_y(n)
            real(8), intent(out) :: hull_x(n+1), hull_y(n+1)
            integer, intent(out) :: h
            integer :: i, k, t
            
            if (n <= 2) then
                do i = 1, n
                    hull_x(i) = pts_x(i)
                    hull_y(i) = pts_y(i)
                end do
                h = n
                return
            end if
            
            call sort_points(pts_x, pts_y, n)
            
            k = 0
            do i = 1, n
                do while (k >= 2)
                    if (cross_product(hull_x(k-1), hull_y(k-1), hull_x(k), hull_y(k), pts_x(i), pts_y(i)) <= 0.0d0) then
                        k = k - 1
                    else
                        exit
                    end if
                end do
                k = k + 1
                hull_x(k) = pts_x(i)
                hull_y(k) = pts_y(i)
            end do
            
            t = k + 1
            i = n - 1
            do while (i >= 1)
                do while (k >= t)
                    if (cross_product(hull_x(k-1), hull_y(k-1), hull_x(k), hull_y(k), pts_x(i), pts_y(i)) <= 0.0d0) then
                        k = k - 1
                    else
                        exit
                    end if
                end do
                k = k + 1
                hull_x(k) = pts_x(i)
                hull_y(k) = pts_y(i)
                i = i - 1
            end do
            h = k - 1
        end subroutine get_convex_hull
        
        logical function point_in_polygon(x, y, px, py, n)
            implicit none
            real(8), intent(in) :: x, y
            integer, intent(in) :: n
            real(8), intent(in) :: px(n), py(n)
            
            integer :: i, j
            logical :: c
            
            c = .false.
            j = n
            do i = 1, n
                if ( ((py(i) > y) .neqv. (py(j) > y)) .and. &
                     (x < (px(j) - px(i)) * (y - py(i)) / (py(j) - py(i)) + px(i)) ) then
                    c = .not. c
                end if
                j = i
            end do
            point_in_polygon = c
        end function point_in_polygon

        subroutine apply_convex_hull_fill(map, nx, ny, n_clusters)
            implicit none
            integer, intent(in) :: nx, ny, n_clusters
            integer, intent(inout) :: map(nx, ny)
            
            integer :: c, i, j, pt_count, h, min_x, max_x, min_y, max_y
            real(8), allocatable :: pts_x(:), pts_y(:), hull_x(:), hull_y(:)
            
            do c = 1, n_clusters
                ! Count points
                pt_count = 0
                do j = 1, ny
                    do i = 1, nx
                        if (map(i, j) == c) pt_count = pt_count + 1
                    end do
                end do
                
                if (pt_count > 2) then
                    allocate(pts_x(pt_count), pts_y(pt_count))
                    allocate(hull_x(pt_count+1), hull_y(pt_count+1))
                    
                    pt_count = 0
                    do j = 1, ny
                        do i = 1, nx
                            if (map(i, j) == c) then
                                pt_count = pt_count + 1
                                pts_x(pt_count) = real(i, 8)
                                pts_y(pt_count) = real(j, 8)
                            end if
                        end do
                    end do
                    
                    call get_convex_hull(pts_x, pts_y, pt_count, hull_x, hull_y, h)
                    
                    ! Find bounding box of hull
                    min_x = nx; max_x = 1; min_y = ny; max_y = 1
                    do i = 1, h
                        if (int(hull_x(i)) < min_x) min_x = int(hull_x(i))
                        if (int(hull_x(i)) > max_x) max_x = int(hull_x(i))
                        if (int(hull_y(i)) < min_y) min_y = int(hull_y(i))
                        if (int(hull_y(i)) > max_y) max_y = int(hull_y(i))
                    end do
                    
                    ! Expand bounding box slightly just in case
                    min_x = max(1, min_x - 1)
                    max_x = min(nx, max_x + 1)
                    min_y = max(1, min_y - 1)
                    max_y = min(ny, max_y + 1)
                    
                    ! Fill polygon
                    do j = min_y, max_y
                        do i = min_x, max_x
                            ! Option A: Only fill NOISE cells (-1) or UNASSIGNED (-2)
                            if (map(i, j) < 0) then
                                if (point_in_polygon(real(i, 8), real(j, 8), hull_x, hull_y, h)) then
                                    map(i, j) = c
                                end if
                            end if
                        end do
                    end do
                    
                    deallocate(pts_x, pts_y, hull_x, hull_y)
                end if
            end do
        end subroutine apply_convex_hull_fill


end module mod_kmeans

