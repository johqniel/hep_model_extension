module mod_grid

use mod_agent_class

use mod_grid_utilites

use mod_setup_hep
    ! Uses:             lon_hep
    !                   lat_hep 
    !                   R (Earth Radius)


implicit none

type :: grid_cell

    ! Technical variables
    integer :: i ! position in grid
    integer :: j

    ! Constant properties of cell
    real(8) :: lon_in
    real(8) :: lat_in
    real(8) :: area

    ! Variable properties of cell
    integer :: number_of_agents
    real(8) :: human_density
    real(8) :: human_density_adj

    type(pointer_node), pointer :: agents => null()

    contains

        procedure clear_cell
        procedure initilize_cell

end type grid_cell

    



type :: Grid    
        type(grid_cell), dimension(:,:), allocatable, target :: cell
        type(Node), pointer :: agent_list_head
        integer :: nx, ny



        contains

            procedure allocate_grid
            procedure initilize_grid
            procedure clear_grid      

end type Grid


! Procedures of grid_cell type

subroutine clear_cell(self)
    self%number_of_agents = 0
    self%human_density = 0
    self%human_density_adj = 0
    self%agents = null()
end subroutine clear_cell

subroutine initilize_cell(self)
    self%area = area_of_gridcell(self%i,self%j,lon_hep, lat_hep, R)
    self_%lon_in
end subroutine initilize_cell



! Procedures of Grid type

subroutine allocate_grid(self)
    allocate(self%cell(self%nx,self%ny))
end subroutine allocate_grid

subroutine initilize_grid(self,agent_list_head)
    type(Node), pointer, intent(in):: agent_list_head

    type(Node), pointer :: current_agent
    type(grid_cell), pointer :: current_cell
    integer :: i,J

    i = 0
    j = 0


    self%agent_list_head = agent_list_head
    current_agent => agent_list_head

    if (.not. associated(self%cell)) then
        print* , "Grid is not allocated, cant be initilized"
        return
    end if

    do i = 1, nx
        do j = 1, ny
            current_cell => self%cell(i,j)
            current_cell%i = i
            current_cell%j = j

            current_cell%initilize_cell()

        end do
    end do

    if (.not. associated(current_agent)) then
        print* , "Head of agents in initilize_grid() not associated." 
    end if






    do while (associated(current_agent))
        
    end do

end subroutine intilize_grid

subroutine clear_grid(self)
    integer :: i,j
    type(grid_cell), pointer :: cell
    i = 1 
    j = 1
    do i = 1, nx
        do j = 1, ny
            self.cell(i,j).clear_cell()
        end do
    end do
end subroutine clear_grid

subroutine agent_changes_gridcell()

end subroutine agent_changes_gridcell

subroutine place_agent_in_grid()

end subroutine place_agent_in_grid

subroutine remove_agent_from_grid()

end subroutine remove_agent_from_grid()

end module mod_grid

