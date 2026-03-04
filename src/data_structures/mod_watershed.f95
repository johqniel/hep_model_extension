! =============================================================================
! Module: mod_watershed
!
! Description:
!   Pure-Fortran watershed clustering on a 2D surface (typically human
!   density values, but works on any non-negative 2D field).
!
!   Algorithm:
!     1. (Optional) Smooth the input surface with a box filter to reduce
!        over-segmentation from noise.
!     2. Find local maxima — cells higher than all 8 neighbours. Each
!        maximum becomes a cluster seed with a unique label.
!     3. Gradient ascent: for every unlabelled cell above a threshold,
!        follow the steepest-uphill neighbour until a labelled cell is
!        reached.  All cells on the path receive that label.
!     4. Cells below the threshold remain unlabelled (noise / water).
!
!   The number of clusters is determined dynamically by the number of
!   local maxima in the (smoothed) surface.
!
!   This module is standalone — it operates on plain 2D arrays.
!   It does NOT depend on mod_grid_id or mod_agent_world.
!
! Integration:
!   use mod_watershed
!   call watershed_cluster(surface, nx, ny, labels, n_clusters, &
!                          smooth_radius, threshold)
! =============================================================================

module mod_watershed

    implicit none

    ! Default parameters
    integer,  parameter :: DEFAULT_SMOOTH_RADIUS = 2     ! box-filter half-width
    real(8),  parameter :: DEFAULT_THRESHOLD     = 0.05d0 ! ignore cells below this

    ! Label for unassigned / below-threshold cells
    integer, parameter :: LABEL_NOISE = -1

contains

    ! =================================================================
    ! PUBLIC: watershed_cluster
    !
    ! Main entry point.  Takes a 2D real surface and produces a 2D
    ! integer label array + cluster count.
    !
    ! Arguments:
    !   surface(nx, ny)  : input values (e.g. human density)
    !   nx, ny           : grid dimensions
    !   labels(nx, ny)   : output cluster labels (-1 = noise, 1.. = cluster)
    !   n_clusters       : output number of clusters found
    !   smooth_radius    : optional, box-filter half-width (0 = no smoothing)
    !   threshold        : optional, cells below this are noise
    ! =================================================================
    subroutine watershed_cluster(surface, nx, ny, labels, n_clusters, &
                                  smooth_radius, threshold)
        implicit none
        integer, intent(in)  :: nx, ny
        real(8), intent(in)  :: surface(nx, ny)
        integer, intent(out) :: labels(nx, ny)
        integer, intent(out) :: n_clusters
        integer, intent(in), optional :: smooth_radius
        real(8), intent(in), optional :: threshold

        real(8), allocatable :: smoothed(:,:)
        integer :: sr
        real(8) :: thr

        ! Resolve optionals
        if (present(smooth_radius)) then
            sr = smooth_radius
        else
            sr = DEFAULT_SMOOTH_RADIUS
        end if

        if (present(threshold)) then
            thr = threshold
        else
            thr = DEFAULT_THRESHOLD
        end if

        ! -----------------------------------------------------------
        ! Step 1: Smooth the surface
        ! -----------------------------------------------------------
        allocate(smoothed(nx, ny))
        if (sr > 0) then
            call smooth_box_filter(surface, nx, ny, sr, smoothed)
        else
            smoothed = surface
        end if

        ! -----------------------------------------------------------
        ! Step 2: Find local maxima → seed labels
        ! -----------------------------------------------------------
        call find_local_maxima(smoothed, nx, ny, thr, labels, n_clusters)

        ! -----------------------------------------------------------
        ! Step 3: Gradient ascent — label remaining cells
        ! -----------------------------------------------------------
        call gradient_ascent_label(smoothed, nx, ny, thr, labels)

        deallocate(smoothed)

    end subroutine watershed_cluster

    ! =================================================================
    ! PRIVATE: smooth_box_filter
    !
    ! Simple box (mean) filter with half-width `radius`.
    ! Each cell becomes the average of the (2*radius+1)^2 neighbourhood.
    ! Handles boundaries by clamping indices to the valid domain.
    !
    ! Why Box Filtering?
    !   The raw HEP surface may have small local fluctuations (noise).
    !   Without smoothing, every single local noise spike would generate 
    !   its own tiny, irrelevant cluster (over-segmentation). 
    !   Smoothing blends these spikes into broader hills, ensuring that
    !   clusters represent macroscopic population groupings.
    ! =================================================================
    subroutine smooth_box_filter(input, nx, ny, radius, output)
        implicit none
        integer, intent(in)  :: nx, ny, radius
        real(8), intent(in)  :: input(nx, ny)
        real(8), intent(out) :: output(nx, ny)

        integer :: i, j, di, dj, ni, nj, count
        real(8) :: total

        do j = 1, ny
            do i = 1, nx
                total = 0.0d0
                count = 0

                do dj = -radius, radius
                    nj = j + dj
                    if (nj < 1 .or. nj > ny) cycle

                    do di = -radius, radius
                        ni = i + di
                        if (ni < 1 .or. ni > nx) cycle

                        total = total + input(ni, nj)
                        count = count + 1
                    end do
                end do

                if (count > 0) then
                    output(i, j) = total / dble(count)
                else
                    output(i, j) = input(i, j)
                end if
            end do
        end do

    end subroutine smooth_box_filter

    ! =================================================================
    ! PRIVATE: find_local_maxima
    !
    ! Scans the grid for cells that are strictly higher than all
    ! 8 of their immediate neighbours AND are strictly above the threshold.
    ! 
    ! Mechanism:
    !   - Outer loops iterate through every cell (i, j).
    !   - If grid(i,j) <= threshold, it's considered noise/water, ignore.
    !   - Inner loops check relative height against the 8 neighbours.
    !   - To prevent flat plateaus from forming multiple seeds, the condition
    !     is strictly greater (`>=` causes rejection).
    !
    ! The identified local maxima serve as the "seeds" or "peaks" for 
    ! the clusters. Each peak is given a unique integer label (1..N).
    ! All other non-peak cells are initialized to LABEL_NOISE (-1).
    ! =================================================================
    subroutine find_local_maxima(grid, nx, ny, threshold, labels, n_maxima)
        implicit none
        integer, intent(in)  :: nx, ny
        real(8), intent(in)  :: grid(nx, ny)
        real(8), intent(in)  :: threshold
        integer, intent(out) :: labels(nx, ny)
        integer, intent(out) :: n_maxima

        integer :: i, j, di, dj, ni, nj
        logical :: is_max

        labels   = LABEL_NOISE
        n_maxima = 0

        do j = 1, ny
            do i = 1, nx
                ! Skip cells below threshold
                if (grid(i, j) <= threshold) cycle

                ! Check if this cell is a local maximum
                is_max = .true.

                do dj = -1, 1
                    do di = -1, 1
                        if (di == 0 .and. dj == 0) cycle

                        ni = i + di
                        nj = j + dj

                        ! Boundary: treat out-of-bounds as lower
                        if (ni < 1 .or. ni > nx .or. nj < 1 .or. nj > ny) cycle

                        if (grid(ni, nj) >= grid(i, j)) then
                            is_max = .false.
                            exit
                        end if
                    end do
                    if (.not. is_max) exit
                end do

                if (is_max) then
                    n_maxima = n_maxima + 1
                    labels(i, j) = n_maxima
                end if
            end do
        end do

    end subroutine find_local_maxima

    ! =================================================================
    ! PRIVATE: gradient_ascent_label
    !
    ! Labels the topography by having every unlabelled cell "climb" 
    ! the surface until it reaches a peak (a seed from `find_local_maxima`).
    !
    ! Logic:
    !   1. Iterate through the grid. Skip cells that are already labelled
    !      (like the seed peaks) or cells that are below the threshold.
    !   2. From an unlabelled cell, record its position in a `path` buffer.
    !   3. Look at its 8 neighbours. Find the one with the highest value
    !      (the steepest uphill direction).
    !   4. Move to that highest neighbour. Keep adding the journey to `path`.
    !   5. Repeat until you step onto a cell that already has a cluster label.
    !   6. Once a label is found, pour that label back down the recorded `path`, 
    !      assigning it to every cell traversed during the climb.
    !
    ! This algorithm ensures that every cell is assigned to the peak
    ! it would flow up to, effectively segmenting the landscape into 
    ! distinct watershed basins/clusters.
    ! =================================================================
    subroutine gradient_ascent_label(grid, nx, ny, threshold, labels)
        implicit none
        integer, intent(in)    :: nx, ny
        real(8), intent(in)    :: grid(nx, ny)
        real(8), intent(in)    :: threshold
        integer, intent(inout) :: labels(nx, ny)

        integer :: i, j, ci, cj, bi, bj, di, dj, ni, nj
        integer :: path_len, k
        real(8) :: best_val
        integer :: found_label

        ! Path buffer (worst case: visit every cell)
        integer, allocatable :: path_i(:), path_j(:)
        allocate(path_i(nx * ny))
        allocate(path_j(nx * ny))

        do j = 1, ny
            do i = 1, nx
                ! Skip already labelled or below threshold
                if (labels(i, j) /= LABEL_NOISE) cycle
                if (grid(i, j) <= threshold) cycle

                ! Start climbing from (i, j)
                path_len = 0
                ci = i
                cj = j

                do
                    ! Record current position in path
                    path_len = path_len + 1
                    path_i(path_len) = ci
                    path_j(path_len) = cj

                    ! If we reached a labelled cell, we're done
                    if (labels(ci, cj) > 0) then
                        found_label = labels(ci, cj)
                        ! Label all cells in path
                        do k = 1, path_len
                            labels(path_i(k), path_j(k)) = found_label
                        end do
                        exit
                    end if

                    ! Find steepest uphill neighbour
                    best_val = grid(ci, cj)
                    bi = ci
                    bj = cj

                    do dj = -1, 1
                        do di = -1, 1
                            if (di == 0 .and. dj == 0) cycle

                            ni = ci + di
                            nj = cj + dj

                            if (ni < 1 .or. ni > nx .or. nj < 1 .or. nj > ny) cycle

                            if (grid(ni, nj) > best_val) then
                                best_val = grid(ni, nj)
                                bi = ni
                                bj = nj
                            end if
                        end do
                    end do

                    ! If no uphill neighbour found (plateau / isolated peak)
                    ! This shouldn't normally happen since maxima are labelled,
                    ! but handle it: label this path with a new label
                    if (bi == ci .and. bj == cj) then
                        ! This cell is a plateau peak not caught by find_local_maxima
                        ! (can happen with equal-valued neighbours)
                        ! Check if any neighbour has a label we can adopt
                        found_label = LABEL_NOISE
                        do dj = -1, 1
                            do di = -1, 1
                                if (di == 0 .and. dj == 0) cycle
                                ni = ci + di
                                nj = cj + dj
                                if (ni < 1 .or. ni > nx .or. nj < 1 .or. nj > ny) cycle
                                if (labels(ni, nj) > 0) then
                                    found_label = labels(ni, nj)
                                    exit
                                end if
                            end do
                            if (found_label > 0) exit
                        end do

                        ! Label the path
                        do k = 1, path_len
                            if (found_label > 0) then
                                labels(path_i(k), path_j(k)) = found_label
                            else
                                ! Leave as noise — truly isolated below-peak cell
                                labels(path_i(k), path_j(k)) = LABEL_NOISE
                            end if
                        end do
                        exit
                    end if

                    ! Move uphill
                    ci = bi
                    cj = bj

                end do  ! climbing loop

            end do  ! i
        end do  ! j

        deallocate(path_i, path_j)

    end subroutine gradient_ascent_label

    ! =================================================================
    ! PUBLIC: watershed_get_cluster_cells
    !
    ! Utility: given a label array and a cluster ID, returns the
    ! cell coordinates belonging to that cluster.
    !
    ! Arguments:
    !   labels(nx, ny) : label array from watershed_cluster
    !   nx, ny         : grid dimensions
    !   cluster_id     : which cluster to query
    !   cell_gx(:)     : output x-indices (pre-allocated, size >= n_cells)
    !   cell_gy(:)     : output y-indices
    !   n_cells        : output count
    ! =================================================================
    subroutine watershed_get_cluster_cells(labels, nx, ny, cluster_id, &
                                            cell_gx, cell_gy, n_cells)
        implicit none
        integer, intent(in)  :: nx, ny, cluster_id
        integer, intent(in)  :: labels(nx, ny)
        integer, intent(out) :: cell_gx(*), cell_gy(*)
        integer, intent(out) :: n_cells

        integer :: i, j

        n_cells = 0
        do j = 1, ny
            do i = 1, nx
                if (labels(i, j) == cluster_id) then
                    n_cells = n_cells + 1
                    cell_gx(n_cells) = i
                    cell_gy(n_cells) = j
                end if
            end do
        end do

    end subroutine watershed_get_cluster_cells

end module mod_watershed
