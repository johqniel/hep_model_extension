module mod_grid

use mod_agent_class
    ! Of which this script uses: 
    !
    ! Structures (with corresponding functions)
    !   - The Node class for agents
    !   - The pointer_node class for lightweight lists of agents
    !
    ! Vars: 
    !       
    !
    ! Functions: 
    !              ! compute_position_in_grid(agent,gx,gy)

use mod_grid_utilities

use mod_calculations

use mod_agent_matrix_merge
    ! Uses: 
    !
    !       - agent_die_from_matrix_calc function to mark agents as dead that are outside of grid. 

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
            ! procedures to manage agents in grid
            procedure place_agent_in_grid 
            procedure place_agent_in_cell
            procedure remove_agent_from_grid
            procedure remove_agent_from_cell
            procedure move_agent_to_cell
            ! procedures to manage the grid
            procedure allocate_grid
            procedure initialize_grid
            procedure clear_grid   
            ! procedures that return information about the grid
            procedure is_in_grid


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

logical function is_in_grid(self,gx,gy) 
    class(spatial_grid), intent(in) :: self
    integer, intent(in) :: gx,gy

    is_in_grid = .true.

    if (gx < 1 .or. gx > self%nx .or. gy < 1 .or. gy > self%ny) then
        is_in_grid = .false.
    endif

end function is_in_grid

subroutine allocate_grid(self)
    class(spatial_grid), intent(inout) :: self

    allocate(self%cell(self%nx,self%ny))
end subroutine allocate_grid

subroutine initialize_grid(self,agent_list_head)
    class(spatial_grid), intent(inout) :: self

    type(Node), pointer, intent(in):: agent_list_head

    type(Node), pointer :: current_agent
    integer :: i,j
    integer :: counter

    counter = 0
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
        print*, "Placing agent for the first time."
        call self%place_agent_in_grid(current_agent)
        current_agent => current_agent%next
        counter = counter + 1
    end do



    print*, "Placed: ", counter, " many agents in the grid, counted: " 

end subroutine initialize_grid

subroutine place_agents_in_grid(self,agent_list_head)
    class(spatial_grid), intent(inout) :: self

    type(Node), pointer, intent(in):: agent_list_head
end subroutine place_agents_in_grid

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

    subroutine move_agent_to_cell(self,agent,gx_old,gy_old,gx_new,gy_new)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent
        integer :: gx_new, gy_new, gx_old, gy_old

        call self%place_agent_in_cell(agent,gx_new,gy_new)
        call self%remove_agent_from_cell(agent, gx_old, gy_old)

    end subroutine move_agent_to_cell

    subroutine update_agents_position_in_grid(self,agent,x_old,y_old,x_new,y_new)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent
        real :: x_new, y_new, x_old, y_old

        integer :: gx_new, gy_new, gx_old, gy_old

        call calculate_grid_pos(x_old,y_old,gx_old,gy_old)
        call calculate_grid_pos(x_new,y_new,gx_new,gy_new)


        if (.not. (gx_old == gx_new .and. gy_old == gy_new)) then
            call self%move_agent_to_cell(agent,gx_old,gy_old,gx_new,gy_new)
        endif

    end subroutine update_agents_position_in_grid

    subroutine place_agent_in_grid(self,agent)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent

        
        integer :: gx,gy

        call compute_position_in_grid(agent,gx,gy) 
        ! If agents position is within grid this function writes
        ! the grid koordinates into gx and gy

        if (gx == -1 .or. gy == -1) then
            print*, "gx == -1 or gy == -1 => kill agent"
            call agent_die_from_matrix_calc(agent%position_human, agent%position_population)
            return
        endif

        if (.not. self%is_in_grid(gx,gy)) then
            print*, "Agent to be placed is not in grid => kill agent"
            call agent_die_from_matrix_calc(agent%position_human, agent%position_population)
            return
        endif

        call self%place_agent_in_cell(agent,gx,gy)

        


    end subroutine place_agent_in_grid


    subroutine place_agent_in_cell(self,agent,gx,gy)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent
        integer :: gx, gy

        type(pointer_node), pointer :: local_head

        if (.not. self%is_in_grid(gx,gy)) then
            print*, "Trying to place agent that is not in grid."
            return
        endif
        
        local_head => self%cell(gx,gy)%agents

        if (.not. associated(local_head)) then
            allocate(local_head)
            local_head%node => agent
            local_head%prev => null()
            local_head%next => null()
            self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents + 1
            return
        endif


        call append_ptr_node(agent, local_head) ! appends agent to the local list of agents. 
        self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents + 1

    end subroutine place_agent_in_cell



    subroutine remove_agent_from_cell(self,agent,gx,gy)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent
        integer :: gx, gy

        

        type(pointer_node), pointer :: local_head
        local_head => self%cell(gx,gy)%agents


        if (.not. associated(local_head)) then
            print*, "Error: Trying to remove agent from cell that has no agents in it."
            return
        endif

        if (.not. associated(agent)) then
            print* , "Error: Trying to remove an agent that doesnt exist."
            return
        endif

        do while (associated(local_head))
            if (associated(local_head%node,agent)) then
                call remove_ptr_node(local_head)
                self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents - 1
                return
            endif
        end do
        

        print*, "Error: Agent was not found in the cell where it should be removed from."


    end subroutine remove_agent_from_cell

    subroutine remove_agent_from_grid(self,agent)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent

        integer :: gx,gy

        call compute_position_in_grid(agent,gx,gy) 
        ! If agents position is within grid this function writes
        ! the grid koordinates into gx and gy


        call self%remove_agent_from_cell(agent,gx,gy)
    end subroutine remove_agent_from_grid


    ! ############################################################
    ! ################# grid utilities ###########################
    ! ############################################################

    integer function count_agents_in_grid(grid_ptr) result(counter)
        type(spatial_grid), pointer, intent(in) :: grid_ptr

        type(pointer_node), pointer :: current_agent_ptr

        integer :: i,j
        

        counter = 0

        do i = 1, grid_ptr%nx
            do j = 1, grid_ptr%ny
                current_agent_ptr => grid_ptr%cell(i,j)%agents
                do while (associated(current_agent_ptr))
                    counter = counter + 1 
                    current_agent_ptr => current_agent_ptr%next
                end do
            end do
        end do

    end function count_agents_in_grid

end module mod_grid

