module mod_debug_grid

use mod_grid

contains

subroutine check_area_of_grid(grid, area_matrix)
    implicit none
    type(spatial_grid), intent(in) :: grid
    real(8), allocatable, dimension(:,:), intent(in) :: area_matrix

    integer :: nx, ny, i, j, mismatch_counter

    nx = grid%nx
    ny = grid%ny
    mismatch_counter = 0

    ! First check: dimension match
    if (.not. allocated(grid%cell)) then
        print *, "Grid cell array not allocated."
        return
    end if

    if (size(area_matrix, 1) /= nx .or. size(area_matrix, 2) /= ny) then
        print* , "Dimensions of area_matrix dont match dimensions of grid"
        return
    end if

    ! Second check: element-wise comparison
    do i = 1, nx
        do j = 1, ny
            if (grid%cell(i,j)%area /= area_matrix(i,j)) then
                mismatch_counter = mismatch_counter + 1
                return
            end if
        end do
    end do

    if (mismatch_counter > 0) then
        print* , "Found: ", mismatch_counter, " many mismatches between area matrix and area of grid."
    endif

end subroutine check_area_of_grid

end module mod_debug_grid