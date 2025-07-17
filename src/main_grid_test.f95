program main_grid_test

use mod_grid
use mod_setup_hep
    ! We use dlon_hep, dlat_hep (dimensions of hep grid)
    !        are_for_dens, (dimension dlon_hep x dlat_hep, contains area of gridcells)
    !                       area is computed in allocate_memory and open files
    !                       Ideally this is eventually computed in mod_grid

implicit none 

    type(grid_cell), allocatable :: grid(:,:) 
    integer :: nx, ny

    ! Define grid size 

    nx = dlon_hep
    ny = dlat_hep

    allocate(grid(nx,ny))

    ! initilize each cell 

end program main_grid_test