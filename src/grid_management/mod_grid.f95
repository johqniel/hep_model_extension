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


use mod_calculations

use mod_globals
    ! Uses:             lon_hep
    !                   lat_ep 
    !                   R (Earth Radius)h


implicit none

type :: grid_cell

    ! Technical variables
    integer :: i = 0! position in grid
    integer :: j = 0

    ! Constant properties of cell
    real(8) :: lon_in = 0
    real(8) :: lat_in = 0
    real(8) :: area = 0

    ! Variable properties of cell
    integer :: number_of_agents = 0
    real(8) :: human_density = 0
    real(8) :: human_density_smoothed = 0

    type(pointer_node), pointer :: agents => null()

    !contains
    !
    !    procedure, pass(self) :: clear_cell => clear_cell
    !    procedure, pass(self) :: initialize_cell =>  initialize_cell

end type grid_cell

    



type, extends(dummy_grid) :: spatial_grid    
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
            procedure :: remove_agent_from_grid => Grid_remove_agent_from_grid
            procedure remove_agent_from_cell
            procedure move_agent_to_cell
            ! procedures to manage the grid
            procedure allocate_grid
            procedure initialize_grid
            procedure clear_grid   
            procedure clean_grid_from_dead_agents ! eventually we want thas this is done as agents die. 
            ! procedures that return information about the grid
            procedure is_in_grid
            procedure agents_in_grid
            procedure dead_agents_in_grid
            procedure is_agent_in_grid
            procedure count_agents_in_cell
            ! procedures that update the information in the cells
            procedure update_density_pure
            !procedure update_density_smoothed


end type spatial_grid

contains

! utilities

include "grid_utilities.inc"

! Procedures of spatial_grid type
subroutine update_density_pure(self)
    class(spatial_grid), intent(inout) :: self


    integer :: i,j, nx, ny

    real :: density

    nx = self%nx
    ny = self%ny

    do i = 1, nx
        do j = 1, ny
            density = self%cell(i,j)%number_of_agents * 100.0 / self%cell(i,j)%area

            self%cell(i,j)%human_density = density

        enddo
    enddo

    


end subroutine update_density_pure

subroutine clear_cell(self,i,j)
    class(spatial_grid), intent(inout) :: self
    integer, intent(in) :: i,j
    self%cell(i,j)%number_of_agents = 0
    self%cell(i,j)%human_density = 0
    self%cell(i,j)%agents => null()
end subroutine clear_cell

subroutine initialize_cell(self,i,j)
    class(spatial_grid), intent(inout) :: self
    integer, intent(in) :: i,j
    self%cell(i,j)%area = area_of_gridcell(i,j,lon_hep, lat_hep, R)
    !self%lon_in
end subroutine initialize_cell


integer function count_agents_in_cell(self,i,j) result(counter)
    class(spatial_grid), intent(in) :: self
    integer, intent(in) :: i,j

    type(pointer_node), pointer :: current_agent

    counter = 0

    if (.not. self%is_in_grid(i,j)) then
        print*, "Error: trying to count agents in cell outside of grid."
        return
    endif

    current_agent => self%cell(i,j)%agents 

    do while (associated(current_agent))
        counter = counter + 1
        current_agent => current_agent%next
    enddo    

end function count_agents_in_cell


logical function is_agent_in_grid(self,agent)
    class(spatial_grid), intent(in) :: self
    type(Node), pointer, intent(in) :: agent

    integer :: gx, gy

    call compute_position_in_grid(agent,gx,gy)

    is_agent_in_grid = .false.

    if (self%is_in_grid(gx,gy)) then

        is_agent_in_grid = search_pointer_node(self%cell(gx,gy)%agents,agent)

    else
        print*, " Error Agents position is not in the grid. (search function)."
    endif

end function is_agent_in_grid

subroutine clean_grid_from_dead_agents(self)
    class(spatial_grid), intent(inout) :: self

    integer nx,ny,i,j, counter
    type(pointer_node), pointer :: current_agent
    type(pointer_node), pointer :: next_agent

    counter = 0

    do i = 1, nx
        do j = 1, ny
            current_agent => self%cell(i,j)%agents
            do while (associated(current_agent))
                print*, "Enter do while."
                next_agent => current_agent%next

                print*, "Before if."
                ! check to avoid segfault
                if (.not. associated(current_agent%node)) then
                    current_agent => null()
                    print*, "In grid cell ", i, ",",j, " we have corrupted pointer node."
                    cycle
                endif
                print*, "After if."
                ! else:

                if (current_agent%node%is_dead) then
                    call self%remove_agent_from_cell(current_agent%node,i,j)
                    print*, "removed agent in clean grid." 
                    counter = counter + 1
                endif
                
                print*, "before next."
                current_agent => next_agent
                print*, "after next"

            enddo
        enddo
    enddo

    print*, " Cleaning done."

    if (counter > 0) then
        print*, "Cleaned grid from: ", counter, " many agents that are dead."
    endif

end subroutine clean_grid_from_dead_agents  

integer function agents_in_grid(self) result(counter)
    class(spatial_grid), intent(inout) :: self

    integer :: nx, ny, i, j
    type(pointer_node), pointer :: local_head

    nx = self%nx
    ny = self%ny

    counter = 0

    do i = 1, nx
        do j = 1, ny
            local_head => self%cell(i,j)%agents
            do while (associated(local_head))
                counter = counter + 1
                local_head => local_head%next
            enddo

        enddo
    enddo
    
end function agents_in_grid

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
            !print* , "Initilize grid cell: ", i, ",", j , " of ", self%nx, ",", self%ny
            self%cell(i,j)%i = i
            self%cell(i,j)%j = j

            call self%initialize_cell(i,j)


        end do
    end do

    if (.not. associated(current_agent)) then
        print* , "Head of agents in initilize_grid() not associated." 
    end if

    do while (associated(current_agent))
        call self%place_agent_in_grid(current_agent)
        current_agent => current_agent%next
        counter = counter + 1
    end do



    print*, "Placed: ", counter, " many agents in the grid, counted: ", self%agents_in_grid()

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

    subroutine move_agent_to_cell(self,agent,gx_old,gy_old,gx_new,gy_new)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(in) :: agent
        integer :: gx_new, gy_new, gx_old, gy_old

        if (.not. associated(agent)) then
            print*, "Error: Agent to be moved is not associated. "
        endif

        call self%place_agent_in_cell(agent,gx_new,gy_new)
        call self%remove_agent_from_cell(agent, gx_old, gy_old)

    end subroutine move_agent_to_cell

    subroutine update_agents_position_in_grid(self,agent,x_old,y_old,x_new,y_new)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent
        real(8) :: x_new, y_new, x_old, y_old

        integer :: gx_new, gy_new, gx_old, gy_old

        call calculate_grid_pos(x_old,y_old,gx_old,gy_old)
        call calculate_grid_pos(x_new,y_new,gx_new,gy_new)


        if (.not. (gx_old == gx_new .and. gy_old == gy_new)) then
            call self%move_agent_to_cell(agent,gx_old,gy_old,gx_new,gy_new)
        endif

    end subroutine update_agents_position_in_grid

    subroutine place_agent_in_grid(self,agent)
        class(spatial_grid), intent(inout), target :: self
        type(Node), pointer, intent(in) :: agent

        
        integer :: gx,gy

        call compute_position_in_grid(agent,gx,gy) 
        ! If agents position is within grid this function writes
        ! the grid koordinates into gx and gy

        if (gx == -1 .or. gy == -1) then
            print*, "gx == -1 or gy == -1 => kill agent"
            !call mark_agent_dead_remove_from_grid(agent%position_human, agent%position_population)
            return
        endif

        if (.not. self%is_in_grid(gx,gy)) then
            print*, "Agent to be placed is not in grid => kill agent"
            !call mark_agent_dead_remove_from_grid(agent%position_human, agent%position_population)
            return
        endif

        if (associated(agent%grid)) then
            print*, "Error: Agent to be placed in grid is already in a grid. (placing anyway)"
        endif

        agent%grid => self ! set grid pointer in agent

        call self%place_agent_in_cell(agent,gx,gy)
        

        


    end subroutine place_agent_in_grid


    subroutine place_agent_in_cell(self,agent,gx,gy)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(in) :: agent
        integer :: gx, gy

        !real :: pos_x, pos_y
        type(pointer_node), pointer :: new_node

        if (.not. self%is_in_grid(gx,gy)) then
            print*, "Trying to place agent that is not in grid."
            return
        endif
        

        call append_pointer_node(self%cell(gx,gy)%agents,agent)
        self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents + 1


        !pos_x = self%cell(gx,gy)%agents%node%pos_x
        !pos_y = self%cell(gx,gy)%agents%node%pos_y

        !print*, "placed agent with position: "
        !print*, "x: ", x, " y: ", y



    end subroutine place_agent_in_cell



    subroutine remove_agent_from_cell(self,agent,gx,gy)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(in) :: agent
        integer :: gx, gy, counter
        type(pointer_node), pointer :: temp_ptr_node
        
        if (self%cell(gx,gy)%number_of_agents == 0) then
            print*, "Error: Trying to remove agent from empty cell."
            if (associated(self%cell(gx,gy)%agents)) then
                print*, "Empty cell has associated agents list."
                !deallocate(self%cell(gx,gy)%agents)
                print*, "Nullifying dangling pointer, but this should not happen."

                self%cell(gx,gy)%agents => null()
                return
            else
                return
            endif
            
            
        endif
        !print*, "Exit if."

        if (.not. associated(agent)) then
            print*, "Error: Trying to remove agent from cell that is not associated."
            return
        endif   

        !print*, "second if."

        if (self%cell(gx,gy)%number_of_agents > 0) then
            if (.not. associated(self%cell(gx,gy)%agents)) then
                print*, "Error: number of agents larger then zero but no agents in cell. "
            endif
        endif

        !print*, "third if."

        ! Special case: head of agents in cell is to be removed. 
        if (associated(self%cell(gx,gy)%agents%node,agent)) then

            temp_ptr_node => self%cell(gx,gy)%agents%next
            deallocate(self%cell(gx,gy)%agents)
            self%cell(gx,gy)%agents => temp_ptr_node
            self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents - 1



            return
        endif

        !print*, "fourth if."

        ! If this was the case something went wrong somewhere
        if (self%cell(gx,gy)%agents%node%id == agent%id) then
            print*, "Error: Agents not the same but their id is the same."
        endif

        
        call remove_pointer_node(self%cell(gx,gy)%agents,agent)

        self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents - 1




        



    end subroutine remove_agent_from_cell

    subroutine Grid_remove_agent_from_grid(self,agent)
        class(spatial_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent

        integer :: gx,gy

        call compute_position_in_grid(agent,gx,gy) 
        ! If agents position is within grid this function writes
        ! the grid koordinates into gx and gy


        call self%remove_agent_from_cell(agent,gx,gy)

        agent%grid => null() ! remove grid pointer from agent
    end subroutine Grid_remove_agent_from_grid


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


    subroutine dead_agents_in_grid(self)
        class(spatial_grid), intent(in) :: self

        integer :: dead_counter
        integer :: unassociated_counter
        integer :: marked_dead_counter

        type(pointer_node), pointer :: current_agent_ptr

        integer :: i,j
        
        


        dead_counter = 0
        unassociated_counter = 0
        marked_dead_counter = 0

        do i = 1, self%nx
            do j = 1, self%ny
                current_agent_ptr => self%cell(i,j)%agents
                do while (associated(current_agent_ptr))
                    !print*, "enter do while"
                    if (.not. associated(current_agent_ptr%node)) then
                        unassociated_counter = unassociated_counter + 1
                        current_agent_ptr = current_agent_ptr%next
                        cycle
                    endif
                    !print *, "after first if"
                    if (current_agent_ptr%node%is_dead) then
                        dead_counter = dead_counter + 1
                        current_agent_ptr = current_agent_ptr%next
                        cycle

                    endif
                    !print*, "after second if"

                    if (is_dead(current_agent_ptr%node%position_human,current_agent_ptr%node%position_population)) then
                        marked_dead_counter = marked_dead_counter + 1
                    endif
                    current_agent_ptr => current_agent_ptr%next

                end do
            end do
        end do

        
        if ( unassociated_counter > 0 ) then
            print*, "There are: ", unassociated_counter, " unassociated agents in the grid."
        endif
        if ( dead_counter > 0 ) then
            print*, "There are: ", dead_counter, " unassociated agents in the grid."
        endif
        if ( marked_dead_counter > 0 ) then
            print*, "There are: ", marked_dead_counter, " unassociated agents in the grid."
        endif

    end subroutine dead_agents_in_grid


end module mod_grid

