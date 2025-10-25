module mod_agent_tracking
! This is the module that contains all function that glue the two data structures to gether
! on the one side we have the matrix way of organising things, that is really good a computing stuff
! very fast.

! On the other hand we have the agent class, that safes agents in a double linked list. 

! In here we basically declare all functions that are used in the mod_matrix_calculations, 
! that manipulate the agents list
! this way we can keep the two data structures as separate as possible
! but still have the possibility to manipulate the agents in the list

    use mod_agent_class
    ! Uses: population_agents_matrix

    use mod_globals
    !Uses: hum_t, hum_id, npops

    use mod_grid
    use mod_calculations

    implicit none

    integer :: born_counter_matrix = 0
    integer :: death_counter_matrix = 0


    

contains


subroutine update_grid_for_moved_agents_new(agents_matrix, num_alive_per_pop)
    implicit none
    type(Node), allocatable, target, intent(inout) :: agents_matrix(:,:)
    integer, intent(in) :: num_alive_per_pop(:)

    ! --- Local Variables ---
    integer :: j, i
    integer :: num_populations, max_positions
    type(Node), pointer :: agent
    integer :: gx_new, gy_new
    type(spatial_grid), pointer :: grid

    ! Get array dimensions
    max_positions = size(agents_matrix, 1)
    num_populations = size(agents_matrix, 2)

    ! Safety check
    if (size(num_alive_per_pop) /= num_populations) then
        print *, "ERROR (update_grid_for_moved_agents): num_alive_per_pop array has wrong size!"
        return
    end if

    do j = 1, num_populations
        do i = 1, num_alive_per_pop(j)
            

            agent => agents_matrix(i, j)

            if (agent%is_dead) then
                print*, "Error: agent should not be dead here."
                cycle
            endif

            ! Check if this agent needs to be moved on the grid
            if (agent%recently_moved) then
                ! 1. Calculate new grid position
                call calculate_grid_pos(agent%pos_x, agent%pos_y, gx_new, gy_new)

                select type(g => agent%grid)

                type is (spatial_grid)

                    grid => g

                class default
                    print*, "Error: current_agent%grid is not spatial grid.", agent%id
                end select


                ! 2. Call the grid's move function 
                call grid%move_agent_to_cell(agent, agent%gx, agent%gy, gx_new, gy_new)

                ! 3. CRITICAL: Update the agent's own record of its grid position
                agent%gx = gx_new
                agent%gy = gy_new
                
                ! 4. CRITICAL: Reset the flag so we don't move it again
                agent%recently_moved = .false.

            end if
            
        end do ! end loop over agents
    end do ! end loop over populations

end subroutine update_grid_for_moved_agents_new

subroutine compact_agents_new(agents_matrix, num_alive_per_pop)
    ! We assume 'type(Node)' is imported from a module
    ! use mod_agent_type, only: Node

    implicit none

    ! --- Arguments ---
    ! Assumes agents_matrix(position_in_population, population)
    type(Node), allocatable, target, intent(inout) :: agents_matrix(:,:)
    integer, intent(inout) :: num_alive_per_pop(:)

    ! --- Local Variables ---
    integer :: max_positions, num_populations
    integer :: j, read_idx, write_idx, k
    integer :: new_alive_count

    ! Get array dimensions
    max_positions = size(agents_matrix, 1)
    num_populations = size(agents_matrix, 2)

    ! Safety check
    if (size(num_alive_per_pop) /= num_populations) then
        print *, "ERROR (compact_agents_fast): num_alive_per_pop array has wrong size!"
        return
    end if

    ! Loop over each population (the 2nd dimension)
    ! This is the outer loop, which is fine.
    do j = 1, num_populations
        
        new_alive_count = 0
        write_idx = 1  ! This is the "slow" pointer for a living agent

        ! Loop over all "potentially alive" agents in this population (the 1st dimension)
        ! THIS LOOP IS NOW FAST because it iterates down a column.
        do read_idx = 1, num_alive_per_pop(j)
            
            if (.not. agents_matrix(read_idx, j)%is_dead) then
                ! This agent is ALIVE.
                new_alive_count = new_alive_count + 1

                ! Move the agent to the write_idx slot if it's not already there.
                if (read_idx /= write_idx) then
                    agents_matrix(write_idx, j) = agents_matrix(read_idx, j)
                end if

                ! --- CRITICAL ---
                ! Update the agent's internal indices to its new position.
                ! (Assumes 'my_i' is position, 'my_j' is population)
                agents_matrix(write_idx, j)%position_human = write_idx
                agents_matrix(write_idx, j)%position_population = j

                ! Advance the "write" position
                write_idx = write_idx + 1
            end if
        end do

        ! --- Mark the remaining "ghost" slots as dead ---
        do k = write_idx, num_alive_per_pop(j)
            agents_matrix(k, j)%is_dead = .true.
        end do

        ! --- Update the alive count for this population ---
        num_alive_per_pop(j) = new_alive_count

    end do

end subroutine compact_agents_new

    !=======================================================================
    ! SUBROUTINE: write_new_positions_to_matrix
    ! writes the positions and velocities of the agents in the public matrix x,y,ux,uy
    !
    !
    ! Notes:
    !   This function doesnt really fit into this file because it doesnt really
    !   help keeping track of alive and dead agents. 
    !
    !   This functions only purpose is to tell the old programm written by constantin
    !   That updates the hep, the positions of the agents that are now calculated by the 
    !   code written by DN. 
    !=======================================================================
    subroutine write_new_positions_to_matrix(x_mat,y_mat,ux_mat,uy_mat, agents_matrix, number_agents_in_pop)
        implicit none
        type(pointer_node), intent(in) :: agents_matrix(:,:)
        real(8), intent(out) :: x_mat(:,:), y_mat(:,:), ux_mat(:,:), uy_mat(:,:)
        integer, intent(in) :: number_agents_in_pop(:)
        
        integer :: i

        integer :: population


        do population = 1, size(number_agents_in_pop)




            x_mat(:,population) = -1000
            y_mat(:,population)  = -1000
            ux_mat(:,population)  = 0.0
            uy_mat(:,population)  = 0.0

            ! Write the new positions and velocities to the matrix
            do i = 1, number_agents_in_pop(population)




                ! Security checks: 
                if(.not. associated(agents_matrix(i,population)%node)) then
                    print*, "Problem in agents_matrix."
                    cycle 
                endif

                if (agents_matrix(i,population)%node%is_dead) then
                    print*, " Problem in agents_matrix" 
                    cycle
                endif

                ! End Security checks.

                x_mat(i,population) = agents_matrix(i,population)%node%pos_x
                y_mat(i,population) = agents_matrix(i,population)%node%pos_y
                ux_mat(i,population) = agents_matrix(i,population)%node%ux
                uy_mat(i,population) = agents_matrix(i,population)%node%uy
            end do

         

        enddo


    end subroutine write_new_positions_to_matrix

    !=======================================================================
    ! SUBROUTINE: write_matrix_info_to_agents
    ! writes the positions and velocities from public matrix x,y,ux,uy to agents in pop_agents_matrix
    !
    !
    ! Notes:
    !   This function doesnt really fit into this file because it doesnt really
    !   help keeping track of alive and dead agents. 
    !
    ! This function should eventually moved into setup and eventually eventually removed al together
    !=======================================================================
    subroutine write_matrix_info_to_agents(x_mat,y_mat,ux_mat,uy_mat,agents_matrix,number_agents_in_pop)
        implicit none
        type(pointer_node), intent(in) :: agents_matrix(:,:)
        real(8), intent(out) :: x_mat(:,:), y_mat(:,:), ux_mat(:,:), uy_mat(:,:)
        integer, intent(in) :: number_agents_in_pop(:)
        
        integer :: i

        integer :: population


        do population = 1, size(number_agents_in_pop)




            do i = 1, number_agents_in_pop(population)




                ! Security checks: 
                if(.not. associated(agents_matrix(i,population)%node)) then
                    print*, "Problem in agents_matrix. write_matrix_info_to_agents"
                    cycle 
                endif

                if (agents_matrix(i,population)%node%is_dead) then
                    print*, " Problem in agents_matrix. write_matrix_info_to_agents" 
                    cycle
                endif

                ! End Security checks.

                agents_matrix(i,population)%node%pos_x = x_mat(i,population)
                agents_matrix(i,population)%node%pos_y = y_mat(i,population)
                agents_matrix(i,population)%node%ux = ux_mat(i,population)
                agents_matrix(i,population)%node%uy = uy_mat(i,population)
                
            end do

         

        enddo
    end subroutine write_matrix_info_to_agents

    subroutine write_matrix_info_to_agents_new(x_mat,y_mat,ux_mat,uy_mat,agents_matrix,number_agents_in_pop)
        implicit none
        type(Node), intent(inout) :: agents_matrix(:,:)
        real(8), intent(out) :: x_mat(:,:), y_mat(:,:), ux_mat(:,:), uy_mat(:,:)
        integer, intent(in) :: number_agents_in_pop(:)
        
        integer :: i

        integer :: population


        do population = 1, size(number_agents_in_pop)




            do i = 1, number_agents_in_pop(population)




                ! Security checks:
                if (agents_matrix(i,population)%is_dead) then
                    print*, " Problem in agents_matrix. write_matrix_info_to_agents_new" 
                    cycle
                endif

                ! End Security checks.

                agents_matrix(i,population)%pos_x = x_mat(i,population)
                agents_matrix(i,population)%pos_y = y_mat(i,population)
                agents_matrix(i,population)%ux = ux_mat(i,population)
                agents_matrix(i,population)%uy = uy_mat(i,population)
                
            end do

         

        enddo
    end subroutine write_matrix_info_to_agents_new
 
!##########################################################################################################################
! Functions that keep track of the alive and dead agents and sorts them into an array such that we can quickly access them
!##########################################################################################################################

    

!################ Killing the agents #######################################

    subroutine kill_agents_outside_of_grid(agents_head)
        type(Node), pointer, intent(inout) :: agents_head

        type(Node), pointer :: current_agent
        type(Node), pointer :: next_agent
        integer :: gx,gy

        current_agent => agents_head

        do while (associated(current_agent))
        next_agent => current_agent%next
            call calculate_grid_pos(current_agent%pos_x, current_agent%pos_y,gx,gy)

            if (gx == -1 .or. gy == -1) then
                call current_agent%agent_die()
            endif

            current_agent => next_agent

        enddo

    end subroutine kill_agents_outside_of_grid

    subroutine kill_agents_outside_of_grid_new(agents_matrix, hum_alive_in_pop)
        implicit none
        type(Node), intent(inout) :: agents_matrix(:,:)
        integer, intent(in) :: hum_alive_in_pop(:)


        integer :: gx,gy, i, j

        do j = 1, size(agents_matrix,2)
            do i = 1, hum_alive_in_pop(j)



                if (agents_matrix(i,j)%is_dead) then 
                    print*, " Error: agent already dead in kill_agents_outside_of_grid_new."
                    cycle
                endif

                call calculate_grid_pos(agents_matrix(i,j)%pos_x, agents_matrix(i,j)%pos_y,gx,gy)

                if (gx == -1 .or. gy == -1) then
                    call agents_matrix(i,j)%agent_die_new()
                endif

            enddo
        enddo

    end subroutine kill_agents_outside_of_grid_new


!########## Create Agents ######################################################

    !=======================================================================
    ! SUBROUTINE: agent_born_place_in_grid
    ! Handles the birth of a new agent in the population matrix.
    !
    ! Arguments:
    !   population   [INTEGER, IN]         - Population index where the agent will be added
    !   hum_t        [INTEGER(npops)]     - Array holding number of humans in each population
    !
    ! Notes:
    !   - Ensures the new agent does not overwrite an existing live agent.
    !   - Increments the born counter.
    !   - Randomly selects two distinct parents and computes inherited traits.
    !   - Sets position, velocity, ID, and agent references.
    !=======================================================================
    subroutine agent_born_place_in_grid(population,grid, parent_one, parent_two)  
        implicit none 
        type(Node), pointer :: parent_one, parent_two ! parent_one = mother
        type(spatial_grid), pointer :: grid
        integer, intent(in) :: population
        
        integer :: population_size
        type(Node), pointer :: new_agent
        real(8) :: pos_x, pos_y

        integer :: gx,gy

        !print*, " We are in function."

        population_size = hum_t(population) ! get the size of the population
      
        if (population_size + 1 > hum_max_A) then
            print*, "Error: Population size exceeded maximum in population ", population
            return
        end if


        call agent_born(parent_one, parent_two)
        
        
        new_agent => tail_agents

      

        pos_x = new_agent%pos_x     
        pos_y = new_agent%pos_y




        ! PLacemend of the agent in the grid
        new_agent%grid => grid ! linked agent to the grid
        call calculate_grid_pos(pos_x, pos_y, gx, gy)
        call grid%place_agent_in_cell(new_agent,gx,gy)

        agents_born_counter = agents_born_counter + 1

    end subroutine agent_born_place_in_grid




!########## Functions that are used in the functions above #####################








end module mod_agent_tracking
