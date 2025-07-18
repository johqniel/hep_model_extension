program main_grid_test

use mod_grid
use mod_setup_hep
    ! We use dlon_hep, dlat_hep (dimensions of hep grid)
    !        are_for_dens, (dimension dlon_hep x dlat_hep, contains area of gridcells)
    !                       area is computed in allocate_memory and open files
    !                       Ideally this is eventually computed in mod_grid

implicit none 

    type(spatial_grid) :: grid
    integer :: nx, ny
    type(Node), target :: agent_one
    type(Node), pointer :: agent_head 

    ! Define grid size 

    agent_head => agent_one

    nx = dlon_hep
    ny = dlat_hep

    call grid%allocate_grid()
    print *, "grid allocated"

    call grid%initialize_grid(agent_head)

    ! initilize each cell 

end program main_grid_test   