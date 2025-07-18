module mod_grid

use mod_agent_class

use mod_grid_utilities

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

    !contains
    !
    !    procedure, pass(self) :: clear_cell => clear_cell
    !    procedure, pass(self) :: initialize_cell =>  initialize_cell

end type grid_cell

    



type :: spatial_grid    
        type(grid_cell), dimension(:,:), allocatable :: cell
        type(Node), pointer :: agent_list_head
        integer :: nx, ny



        contains
            
            ! procedures to manage individual cell
            procedure initialize_cell
            procedure clear_cell
            ! procedures to manage the grid
            procedure allocate_grid
            procedure initialize_grid
            procedure clear_grid      

end type spatial_grid

contains

! Procedures of grid_cell type

subroutine clear_cell(self,i,j)
    class(spatial_grid), intent(inout) :: self
    integer, intent(in) :: i,j
    self%cell(i,j)%number_of_agents = 0
    self%cell(i,j)%human_density = 0
    self%cell(i,j)%human_density_adj = 0
    self%cell(i,j)%agents => null()
end subroutine clear_cell

subroutine initialize_cell(self,i,j)
    class(spatial_grid), intent(inout) :: self
    integer, intent(in) :: i,j
    self%cell(i,j)%area = area_of_gridcell(i,j,lon_hep, lat_hep, R)
    !self%lon_in
end subroutine initialize_cell



! Procedures of Grid type

subroutine allocate_grid(self)
    class(spatial_grid), intent(inout) :: self

    allocate(self%cell(self%nx,self%ny))
end subroutine allocate_grid

subroutine initialize_grid(self,agent_list_head)
    class(spatial_grid), intent(inout) :: self

    type(Node), pointer, intent(in):: agent_list_head

    type(Node), pointer :: current_agent
    integer :: i,j

    i = 0
    j = 0

    if(.not. associated(agent_list_head)) then 
        print* , "Agent List Head is not associated, cant initilize grid"
        return
    endif

    self%agent_list_head => agent_list_head
    current_agent => agent_list_head

    if (.not. allocated(self%cell)) then
        print* , "Grid is not allocated, cant be initilized"
        return
    end if

    do i = 1, self%nx
        do j = 1, self%ny
            print* , "Initilize grid cell: ", i, ",", j , " of ", self%nx, ",", self%ny
            self%cell(i,j)%i = i
            self%cell(i,j)%j = j

            call self%initialize_cell(i,j)


        end do
    end do

    if (.not. associated(current_agent)) then
        print* , "Head of agents in initilize_grid() not associated." 
    end if

    do while (associated(current_agent))
        current_agent => null()
    end do






    do while (associated(current_agent))
        
    end do

end subroutine initialize_grid

subroutine clear_grid(self)
    class(spatial_grid), intent(inout) :: self

    integer :: i,j
    type(grid_cell), pointer :: cell
    i = 1 
    j = 1
    do i = 1, self%nx
        do j = 1, self%ny
            call self%clear_cell(i,j)
        end do
    end do
end subroutine clear_grid

subroutine agent_changes_gridcell()

end subroutine agent_changes_gridcell

subroutine place_agent_in_grid()

end subroutine place_agent_in_grid

subroutine remove_agent_from_grid()

end subroutine remove_agent_from_grid

end module mod_grid

