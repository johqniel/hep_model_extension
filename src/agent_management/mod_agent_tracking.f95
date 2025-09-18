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
    !Uses: hum_t, hum_id, is_dead, npops

    use mod_grid
    use mod_calculations

    implicit none

    integer :: born_counter_matrix = 0
    integer :: death_counter_matrix = 0


    

contains

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
    subroutine write_new_positions_to_matrix(x_mat,y_mat,ux_mat,uy_mat, agents_matrix, death_mat, number_agents_in_pop)
        implicit none
        type(pointer_node), intent(in) :: agents_matrix(:,:)
        logical, intent(in) :: death_mat(:,:)
        real(8), intent(out) :: x_mat(:,:), y_mat(:,:), ux_mat(:,:), uy_mat(:,:)
        integer, intent(in) :: number_agents_in_pop(:)
        
        integer :: i, j

        integer :: counter
        integer :: population


        do population = 1, size(number_agents_in_pop)



    
            counter = 0

            x_mat(:,population) = -1000
            y_mat(:,population)  = -1000
            ux_mat(:,population)  = 0.0
            uy_mat(:,population)  = 0.0

            ! Write the new positions and velocities to the matrix
            do i = 1, number_agents_in_pop(population)




                if (death_mat(i,population)) then
                    ! Security checks: 
                    if(.not. associated(agents_matrix(i,population)%node)) then
                        ! okay: 
                        cycle 
                    endif

                    ! else: 
                    if (agents_matrix(i,population)%node%is_dead) then
                        ! okay 
                        cycle
                    endif

                    counter = counter + 1
                    !print*, "This should not happen. Are you writing new positions to matrix, "
                    !print*, "before killing the agents marked as dead and reordering agents-matrix ?"

                    cycle
                end if

                if (.not. associated(agents_matrix(i,population)%node)) then
                    print*, "Error: Alive agent in matrix is not associated. "
                    cycle
                endif

                x_mat(i,population) = agents_matrix(i,population)%node%pos_x
                y_mat(i,population) = agents_matrix(i,population)%node%pos_y
                ux_mat(i,population) = agents_matrix(i,population)%node%ux
                uy_mat(i,population) = agents_matrix(i,population)%node%uy
            end do

            if (counter > 0 ) then
                !print*, "In Population ", population, ":"
                !print*, "There are: ", counter, " many agents that are marked dead but are alive. - write to matrixes function"
            ENDIF

        enddo


    end subroutine write_new_positions_to_matrix

 
!##########################################################################################################################
! Functions that keep track of the alive and dead agents and sorts them into an array such that we can quickly access them
!##########################################################################################################################

    

!################ Killing the agents #######################################

    subroutine kill_agents_outside_of_grid(agents_head,grid)
        type(spatial_grid), intent(in), pointer :: grid
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
        integer:: hum
        real(8) :: pos_x, pos_y, ux_i, uy_i

        integer :: gx,gy

        !print*, " We are in function."

        population_size = hum_t(population) ! get the size of the population
      
        if (population_size + 1 > hum_max_A) then
            !print *, "Error: arrays for matrix calc are not big enough! (agent_born_place_in_grid)"
            !print*, "hum_max_A: ", hum_max_A, " population_size + 1: ", population_size + 1
            return
        end if

        ! For debugging purposes: w
        born_counter_matrix = born_counter_matrix + 1      

        !print*, "agent born."
        call agent_born(parent_one, parent_two)
        
        
        new_agent => tail_agents

      
        !print*, "agent born succesfull."

        pos_x = new_agent%pos_x     
        pos_y = new_agent%pos_y


        hum_t(population) = population_size + 1 ! update the number of humans in the population
        hum = hum_t(population) ! get the new position of the agent in the matri

        !print*, "got agents position."


        if (population > size(is_dead,2)) then
            print*, "population is: ", population, "but there should only be: ", size(is_dead,2), " = ", npops
        endif
        if (hum > size(is_dead,1)) then
            print*, "is_dead array is to small. Its size: ", size(is_dead,1), " hum_max_A: ", hum_max_A 
            print*, "hum_t: ", hum, " count agents: ", count_agents()
            print*, "count dead agents: ", count_dead_agents()
            print*, "count agents in grid: ", count_agents_in_grid(grid)
        endif

        if (is_dead(hum, population) .eqv. .false.) then
            print *, "Error: trying overwrite a alive agent in matrix! (agent_born_place_in_grid)"
            !print *, "hum_id: ", hum_id(hum + 1, population), "population: ", population
            return
        end if
      
        !print*, "Before data Management."

        ! Data Management Variables:
        hum_id(hum,population) =  get_agent_id() ! get a new id for the agent                                                             
        is_dead(hum,population) = .false.                                                                           
        population_agents_matrix(hum,population)%node => new_agent

        new_agent%position_human = hum
        new_agent%position_population = population

        !print*, "before placing agent"

        ! PLacemend of the agent in the grid
        new_agent%grid => grid ! linked agent to the grid
        call calculate_grid_pos(pos_x, pos_y, gx, gy)
        call grid%place_agent_in_cell(new_agent,gx,gy)

        agents_born_counter = agents_born_counter + 1

    end subroutine agent_born_place_in_grid




!########## Functions that are used in the functions above #####################








end module mod_agent_tracking
