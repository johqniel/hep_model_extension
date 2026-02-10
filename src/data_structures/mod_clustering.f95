! =============================================================================
! Module: mod_clustering
!
! Description:
!   Cell-based spatial clustering for agent populations.
!
!   Design:
!     1. The watershed algorithm (mod_watershed) runs on the HEP surface
!        in pure Fortran — no Python computation needed.
!     2. Each grid cell is assigned a cluster ID via cell_cluster_map(gx,gy).
!     3. All agents in a cell inherit that cell's cluster ID.
!        Dead agents vanish naturally — no stale references.
!     4. Cluster IDs are persistent across re-clustering: new clusters
!        are matched to previous ones by cell overlap.
!     5. Migration is detected when an agent moves from a cell in
!        cluster A to a cell in cluster B  (A ≠ B, both ≥ 0).
!
! Dependencies:
!   mod_watershed (for the actual clustering algorithm)
! =============================================================================

module mod_clustering

    use mod_watershed

    implicit none

    ! -----------------------------------------------------------------
    ! Label constants
    ! -----------------------------------------------------------------
    integer, parameter :: CLUSTER_NOISE      = -1  ! below threshold / water
    integer, parameter :: CLUSTER_UNASSIGNED = -2  ! not yet clustered

    ! -----------------------------------------------------------------
    ! Type: cluster_t
    !
    ! A single cluster — holds which cells belong to it.
    ! -----------------------------------------------------------------
    type :: cluster_t
        integer :: id            = -1        ! Persistent cluster ID
        integer :: n_cells       = 0         ! Number of grid cells
        integer :: n_agents      = 0         ! Total agents (computed on demand)

        integer, allocatable :: cell_gx(:)   ! Grid x-indices of member cells
        integer, allocatable :: cell_gy(:)   ! Grid y-indices of member cells

        real(8) :: centroid_x = 0.0d0        ! Weighted centroid (lon)
        real(8) :: centroid_y = 0.0d0        ! Weighted centroid (lat)

    contains
        procedure :: cleanup_cluster => cluster_cleanup

    end type cluster_t

    ! -----------------------------------------------------------------
    ! Type: cluster_store_t
    !
    ! Container: clusters + cell→cluster map + migration tracking.
    ! -----------------------------------------------------------------
    type :: cluster_store_t
        integer :: nx = 0
        integer :: ny = 0

        ! Cell → cluster map  (nx, ny)
        integer, allocatable :: cell_cluster_map(:,:)

        ! Clusters (1 : n_clusters)
        integer :: n_clusters = 0
        type(cluster_t), allocatable :: clusters(:)

        ! Persistent ID tracking
        integer :: next_cluster_id = 0

        ! Update tracking
        integer :: last_update_tick = 0
        integer :: update_interval  = 100

        ! Migration counters
        integer :: migration_events       = 0  ! per re-clustering cycle
        integer :: migration_events_total = 0  ! lifetime total

    contains
        procedure :: init                 => store_init
        procedure :: cleanup              => store_cleanup

        ! Main driver: runs watershed and builds cluster structures
        procedure :: run_watershed        => store_run_watershed

        ! Internal helpers
        procedure :: set_cell_labels      => store_set_cell_labels
        procedure :: count_agents         => store_count_agents
        procedure :: get_cluster_of_cell  => store_get_cluster_of_cell

        ! Migration
        procedure :: check_migration      => store_check_migration
        procedure :: reset_migration      => store_reset_migration

        ! Output
        procedure :: print_summary        => store_print_summary

    end type cluster_store_t

contains

    ! =================================================================
    ! cluster_t: cleanup
    ! =================================================================
    subroutine cluster_cleanup(self)
        implicit none
        class(cluster_t), intent(inout) :: self

        if (allocated(self%cell_gx)) deallocate(self%cell_gx)
        if (allocated(self%cell_gy)) deallocate(self%cell_gy)
        self%id       = -1
        self%n_cells  = 0
        self%n_agents = 0

    end subroutine cluster_cleanup

    ! =================================================================
    ! store_init: allocate map for grid size
    ! =================================================================
    subroutine store_init(self, nx, ny, update_interval)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        integer, intent(in) :: nx, ny
        integer, intent(in), optional :: update_interval

        call self%cleanup()

        self%nx = nx
        self%ny = ny

        allocate(self%cell_cluster_map(nx, ny))
        self%cell_cluster_map = CLUSTER_UNASSIGNED

        if (present(update_interval)) then
            self%update_interval = update_interval
        end if

    end subroutine store_init

    ! =================================================================
    ! store_cleanup
    ! =================================================================
    subroutine store_cleanup(self)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        integer :: k

        if (allocated(self%clusters)) then
            do k = 1, size(self%clusters)
                call self%clusters(k)%cleanup_cluster()
            end do
            deallocate(self%clusters)
        end if

        if (allocated(self%cell_cluster_map)) deallocate(self%cell_cluster_map)

        self%n_clusters       = 0
        self%nx               = 0
        self%ny               = 0
        self%next_cluster_id  = 0
        self%last_update_tick = 0
        self%migration_events = 0

    end subroutine store_cleanup

    ! =================================================================
    ! store_run_watershed
    !
    ! Main entry point: takes a 2D HEP surface, runs the watershed
    ! algorithm (from mod_watershed), and updates cluster structures
    ! with persistent ID matching.
    !
    ! Arguments:
    !   hep_surface(nx, ny) : the HEP values for one population
    !   cell_x(nx)          : lon coordinates of cell centres
    !   cell_y(ny)          : lat coordinates of cell centres
    !   tick                : current simulation tick
    !   smooth_radius       : optional, box-filter half-width (default 2)
    !   threshold           : optional, ignore cells below this (default 0.05)
    ! =================================================================
    subroutine store_run_watershed(self, hep_surface, cell_x, cell_y, tick, &
                                    smooth_radius, threshold)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        real(8), intent(in) :: hep_surface(self%nx, self%ny)
        real(8), intent(in) :: cell_x(self%nx)
        real(8), intent(in) :: cell_y(self%ny)
        integer, intent(in) :: tick
        integer, intent(in), optional :: smooth_radius
        real(8), intent(in), optional :: threshold

        integer, allocatable :: raw_labels(:,:)
        integer :: n_raw_clusters

        allocate(raw_labels(self%nx, self%ny))

        ! Run the watershed algorithm
        call watershed_cluster(hep_surface, self%nx, self%ny, &
                               raw_labels, n_raw_clusters, &
                               smooth_radius, threshold)

        ! Build cluster structures with persistent ID matching
        call self%set_cell_labels(raw_labels, n_raw_clusters, &
                                  cell_x, cell_y, tick)

        deallocate(raw_labels)

    end subroutine store_run_watershed

    ! =================================================================
    ! store_set_cell_labels
    !
    ! Receives raw watershed labels (1-based) and builds cluster
    ! structures with persistent ID matching by cell overlap.
    ! =================================================================
    subroutine store_set_cell_labels(self, raw_labels, n_raw, &
                                     cell_x, cell_y, tick)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        integer, intent(in) :: raw_labels(self%nx, self%ny)
        integer, intent(in) :: n_raw
        real(8), intent(in) :: cell_x(self%nx)
        real(8), intent(in) :: cell_y(self%ny)
        integer, intent(in) :: tick

        integer :: i, j, k, idx
        integer, allocatable :: raw_sizes(:)
        integer, allocatable :: raw_fill(:)
        real(8), allocatable :: raw_cx(:), raw_cy(:)
        type(cluster_t), allocatable :: new_clusters(:)

        ! For ID matching
        integer, allocatable :: old_map(:,:)
        integer, allocatable :: overlap(:,:)   ! (n_raw, old_n_clusters)
        integer, allocatable :: raw_to_id(:)   ! raw label → persistent ID
        integer :: best_old, best_count
        logical, allocatable :: old_used(:)

        ! -----------------------------------------------------------
        ! 1. Save old map for overlap matching
        ! -----------------------------------------------------------
        if (allocated(self%cell_cluster_map) .and. self%n_clusters > 0) then
            allocate(old_map(self%nx, self%ny))
            old_map = self%cell_cluster_map
        end if

        ! -----------------------------------------------------------
        ! 2. Count cells per raw cluster and accumulate centroids
        ! -----------------------------------------------------------
        allocate(raw_sizes(n_raw))
        allocate(raw_cx(n_raw))
        allocate(raw_cy(n_raw))
        raw_sizes = 0
        raw_cx    = 0.0d0
        raw_cy    = 0.0d0

        do j = 1, self%ny
            do i = 1, self%nx
                k = raw_labels(i, j)
                if (k >= 1 .and. k <= n_raw) then
                    raw_sizes(k) = raw_sizes(k) + 1
                    raw_cx(k) = raw_cx(k) + cell_x(i)
                    raw_cy(k) = raw_cy(k) + cell_y(j)
                end if
            end do
        end do

        ! -----------------------------------------------------------
        ! 3. Match to old clusters by cell overlap
        ! -----------------------------------------------------------
        allocate(raw_to_id(n_raw))
        raw_to_id = -1

        if (allocated(old_map) .and. self%n_clusters > 0) then
            allocate(overlap(n_raw, self%n_clusters))
            allocate(old_used(self%n_clusters))
            overlap  = 0
            old_used = .false.

            ! Build overlap matrix
            do j = 1, self%ny
                do i = 1, self%nx
                    k = raw_labels(i, j)
                    if (k >= 1 .and. old_map(i, j) >= 0) then
                        ! Find which old cluster index has this persistent ID
                        do idx = 1, self%n_clusters
                            if (self%clusters(idx)%id == old_map(i, j)) then
                                overlap(k, idx) = overlap(k, idx) + 1
                                exit
                            end if
                        end do
                    end if
                end do
            end do

            ! Greedy matching
            do k = 1, n_raw
                best_old   = -1
                best_count = 0
                do idx = 1, self%n_clusters
                    if (.not. old_used(idx) .and. &
                        overlap(k, idx) > best_count) then
                        best_count = overlap(k, idx)
                        best_old   = idx
                    end if
                end do
                if (best_old > 0 .and. best_count > 0) then
                    raw_to_id(k) = self%clusters(best_old)%id
                    old_used(best_old) = .true.
                end if
            end do

            deallocate(overlap, old_used)
        end if

        ! Assign fresh IDs to unmatched clusters
        do k = 1, n_raw
            if (raw_to_id(k) < 0) then
                raw_to_id(k) = self%next_cluster_id
                self%next_cluster_id = self%next_cluster_id + 1
            end if
        end do

        ! -----------------------------------------------------------
        ! 4. Build new cluster array
        ! -----------------------------------------------------------
        if (allocated(self%clusters)) then
            do k = 1, size(self%clusters)
                call self%clusters(k)%cleanup_cluster()
            end do
            deallocate(self%clusters)
        end if

        self%n_clusters = n_raw

        if (n_raw > 0) then
            allocate(self%clusters(n_raw))
            allocate(raw_fill(n_raw))
            raw_fill = 0

            do k = 1, n_raw
                self%clusters(k)%id     = raw_to_id(k)
                self%clusters(k)%n_cells = raw_sizes(k)
                allocate(self%clusters(k)%cell_gx(raw_sizes(k)))
                allocate(self%clusters(k)%cell_gy(raw_sizes(k)))

                if (raw_sizes(k) > 0) then
                    self%clusters(k)%centroid_x = raw_cx(k) / dble(raw_sizes(k))
                    self%clusters(k)%centroid_y = raw_cy(k) / dble(raw_sizes(k))
                end if
            end do

            ! Fill cell arrays
            do j = 1, self%ny
                do i = 1, self%nx
                    k = raw_labels(i, j)
                    if (k >= 1 .and. k <= n_raw) then
                        raw_fill(k) = raw_fill(k) + 1
                        self%clusters(k)%cell_gx(raw_fill(k)) = i
                        self%clusters(k)%cell_gy(raw_fill(k)) = j
                    end if
                end do
            end do

            deallocate(raw_fill)
        end if

        ! -----------------------------------------------------------
        ! 5. Update cell_cluster_map with persistent IDs
        ! -----------------------------------------------------------
        do j = 1, self%ny
            do i = 1, self%nx
                k = raw_labels(i, j)
                if (k >= 1 .and. k <= n_raw) then
                    self%cell_cluster_map(i, j) = raw_to_id(k)
                else
                    self%cell_cluster_map(i, j) = CLUSTER_NOISE
                end if
            end do
        end do

        self%last_update_tick = tick
        self%migration_events = 0

        ! Cleanup
        deallocate(raw_sizes, raw_cx, raw_cy, raw_to_id)
        if (allocated(old_map)) deallocate(old_map)

    end subroutine store_set_cell_labels

    ! =================================================================
    ! store_count_agents: sum cell agent counts per cluster
    ! =================================================================
    subroutine store_count_agents(self, cell_agent_counts)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        integer, intent(in) :: cell_agent_counts(self%nx, self%ny)

        integer :: k, c, gx, gy

        do k = 1, self%n_clusters
            self%clusters(k)%n_agents = 0
            do c = 1, self%clusters(k)%n_cells
                gx = self%clusters(k)%cell_gx(c)
                gy = self%clusters(k)%cell_gy(c)
                self%clusters(k)%n_agents = self%clusters(k)%n_agents + &
                                            cell_agent_counts(gx, gy)
            end do
        end do

    end subroutine store_count_agents

    ! =================================================================
    ! store_get_cluster_of_cell: O(1) lookup
    ! =================================================================
    integer function store_get_cluster_of_cell(self, gx, gy) result(cid)
        implicit none
        class(cluster_store_t), intent(in) :: self
        integer, intent(in) :: gx, gy

        if (.not. allocated(self%cell_cluster_map)) then
            cid = CLUSTER_UNASSIGNED
            return
        end if

        if (gx < 1 .or. gx > self%nx .or. gy < 1 .or. gy > self%ny) then
            cid = CLUSTER_UNASSIGNED
            return
        end if

        cid = self%cell_cluster_map(gx, gy)

    end function store_get_cluster_of_cell

    ! =================================================================
    ! store_check_migration: detect inter-cluster movement
    ! =================================================================
    subroutine store_check_migration(self, old_gx, old_gy, new_gx, new_gy, &
                                      migrated, from_cluster, to_cluster)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        integer, intent(in)  :: old_gx, old_gy, new_gx, new_gy
        logical, intent(out) :: migrated
        integer, intent(out) :: from_cluster, to_cluster

        migrated = .false.
        from_cluster = self%get_cluster_of_cell(old_gx, old_gy)
        to_cluster   = self%get_cluster_of_cell(new_gx, new_gy)

        if (from_cluster >= 0 .and. to_cluster >= 0 .and. &
            from_cluster /= to_cluster) then
            migrated = .true.
            self%migration_events       = self%migration_events + 1
            self%migration_events_total = self%migration_events_total + 1
        end if

    end subroutine store_check_migration

    ! =================================================================
    ! store_reset_migration
    ! =================================================================
    subroutine store_reset_migration(self)
        implicit none
        class(cluster_store_t), intent(inout) :: self
        self%migration_events = 0
    end subroutine store_reset_migration

    ! =================================================================
    ! store_print_summary
    ! =================================================================
    subroutine store_print_summary(self)
        implicit none
        class(cluster_store_t), intent(in) :: self
        integer :: k, noise_cells, i, j

        noise_cells = 0
        do j = 1, self%ny
            do i = 1, self%nx
                if (self%cell_cluster_map(i, j) == CLUSTER_NOISE) then
                    noise_cells = noise_cells + 1
                end if
            end do
        end do

        print *, "=== Watershed Cluster Summary (tick ", self%last_update_tick, ") ==="
        print *, "  Clusters:          ", self%n_clusters
        print *, "  Noise cells:       ", noise_cells
        print *, "  Next fresh ID:     ", self%next_cluster_id
        print *, "  Migrations (cycle):", self%migration_events
        print *, "  Migrations (total):", self%migration_events_total

        if (allocated(self%clusters)) then
            do k = 1, self%n_clusters
                print '(A,I4,A,I5,A,I6,A,F10.4,A,F10.4,A)', &
                    "  ID=", self%clusters(k)%id, &
                    "  cells=", self%clusters(k)%n_cells, &
                    "  agents=", self%clusters(k)%n_agents, &
                    "  centroid=(", self%clusters(k)%centroid_x, &
                    ", ", self%clusters(k)%centroid_y, ")"
            end do
        end if

        print *, "================================================"

    end subroutine store_print_summary

end module mod_clustering
