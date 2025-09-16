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
                print*, "In Population ", population, ":"
                print*, "There are: ", counter, " many agents that are marked dead but are alive. - write to matrixes function"
            ENDIF

        enddo


    end subroutine write_new_positions_to_matrix

 
!##########################################################################################################################
! Functions that keep track of the alive and dead agents and sorts them into an array such that we can quickly access them
!##########################################################################################################################

! How to use: 
! 
!   In the simulation within one time tick t you mark agents as dead that you want to die in that period using the function: 
!           - mark_agent_dead_remove_from_grid
!             As the name suggests this function marks a agent as dead and removes it from the grid
!               
!             The function mark_agent_dead_no_grid is only for debugging purposes DN 18.08.
!
!    At the end of the time tick you call the two functions: 
!           - kill_agents_in_population_marked_as_dead
!           - move alive agents to the beginning of matrix
!           As the name suggests these two functions: kill agents marked as dead and reorder the matrix that 
!           stores pointers to the alive agents such that the alive agents are in the beginning of the matrix 
!           rows. 
!                    
!   The killing of agents is this complicated because if we did it on the go and not killed all agents collectivly 
!   At the end of a time tick the matrix in which we store the pointers would be corrupted and point to dead agents
!
!   creating new agents can be done on the go using:
!       - agent_born_place_in_grid
!
!
!
!
    

!################ Killing the agents #######################################

    !=======================================================================
    ! SUBROUTINE: mark_agent_dead_remove_from_grid
    ! Marks an agent as dead in the matrix if not already dead.
    !
    ! Arguments:
    !   hum         [INTEGER, IN] - Index of the human within the population
    !   population  [INTEGER, IN] - Index of the population the agent belongs to
    !
    ! Notes:
    !   Increments the death counter. Prevents double-death errors.
    !=======================================================================
    subroutine mark_agent_dead_(hum, population)
        implicit none 
        type(Node), pointer :: agent_ptr
        integer, intent(in) :: hum, population

        integer :: gx, gy

        if(size(population_agents_matrix,1) < hum ) then
            print*, "position of human to mark dead to large."
            return
        endif

        if(size(population_agents_matrix,2) < npops) then
            print*, "population of human to mark dead doesnt exist, i.e. pop> npops"
            return
        endif

        !print*, "Enter mark agent."

        if (.not. associated(population_agents_matrix(hum,population)%node)) then
            print *, "Error: trying to die an agent that is not associated in matrix! (mark_agent_dead)"
            return
        endif

        !print*, "after if."

        agent_ptr => population_agents_matrix(hum, population)%node

        !print*, "agent selected successfully."


        if (.not. associated(agent_ptr)) then
            print *, "Error: trying to die an agent that is not associated in matrix! (mark_agent_dead)"
            return
        end if

        if (is_dead(hum, population) .eqv. .true.) then
            print *, "Error: trying to die an already dead agent in matrix! (mark_agent_dead)"
            return
        else
            is_dead(hum, population) = .true. ! mark the agent as dead in the matrix
            death_counter_matrix = death_counter_matrix + 1
        endif

    end subroutine mark_agent_dead_

    subroutine mark_agent_dead(hum, population)
        integer, intent(in) :: hum, population 

        call population_agents_matrix(hum,population)%node%agent_die()




    end subroutine mark_agent_dead

    !=======================================================================
    ! SUBROUTINE: kill_agents_in_population_marked_as_dead
    ! Walks throught the population and kills all agents that are marked as dead.
    !
    ! Notes: 
    !    - The following variables have to be updated: 
    !        
    !
    !
    !=======================================================================
    subroutine kill_agents_in_population_marked_as_dead(jp)
        integer, intent(in) :: jp

        ! Uses as inputs: 
        !   - hum_t:                    (number of humans in each population)
        !   - population_agents_matrix: (the matrix that holds the agents)
        !   - is_dead:                  (the matrix that tells us if an agent is 
        !                                dead/to be killed or not) 

        type(Node), pointer :: current_agent
        integer :: i
        integer :: counter

        counter = 0


        !print*, "Kill agents marked as dead."

        do i = 1, hum_t(jp)
            current_agent => population_agents_matrix(i,jp)%node

            if (is_dead(i,jp)) then
                if (.not. associated(current_agent)) then
                    print *, "Error: trying to kill an agent that is not associated in matrix! (kill_agents_marked_as_dead)"
                    cycle
                end if

                ! else: 

                if (current_agent%is_dead) then
                    print *, "Error: trying to kill an already dead agent in matrix! (kill_agents_marked_as_dead)"
                    cycle
                end if

                ! else: 

                call current_agent%agent_die() ! Call the agent's die method
                counter = counter + 1
                marked_agents_killed = marked_agents_killed + 1

            end if
        end do

        !print*, "Killed: ", counter, " many agents."
                           
    end subroutine kill_agents_in_population_marked_as_dead



    !=======================================================================
    ! SUBROUTINE: move_alive_agents_to_beginning_of_matrix
    ! Updates the population_agents_matrix such that the alive agents of each
    ! population are in the Beginning of the matrix. This function replaces 
    ! the old move active agents to beginning_of_matrix function. 
    !
    ! Notes: 
    !    - The following variables have to be updated: 
    !        - hum_t (number of humans in each population)
    !        - population_agents_matrix (the matrix that holds the agents) (as pointer_nodes)
    !        - the position of the agent in the matrix (its population and its position in the population)
    !
    !
    !=======================================================================
        subroutine move_alive_agents_to_beginning_of_matrix(jp)
                    integer, intent(in) :: jp
                    ! Uses as inputs: 
                    !   - population_agents_matrix: (the matrix that holds the agents) (as pointer_nodes)
                    !   - is_dead:                  (the matrix that tells us if an agent is dead or not)

                    ! Uses the following variables: 
                    !   - current_position, j:     integers to keep track of positions in matrix
                    !   - new_order:               integer array in which we store new order of agents

                    ! It changes the following variables that live outside this subroutine: 
                    !   - hum_t
                    !   - population_agents_matrix
                    
                    integer, allocatable :: new_order(:)
                    integer :: j, current_position, potential_size_of_population
                    integer :: number_of_humans_in_pop

                    potential_size_of_population = size(population_agents_matrix,1)

                    allocate(new_order(potential_size_of_population))

                    new_order = -1


                    current_position = 0

                    do j = 1, potential_size_of_population
                        ! Check if the agent is associated
                        if (.not. associated(population_agents_matrix(j,jp)%node)) then
                            cycle
                        endif

                        if (population_agents_matrix(j,jp)%node%is_dead) then
                            cycle
                        endif
                        
                        if (is_dead(j,jp) .eqv. .false.) then

                            ! We store in the i'th entry of the new_order array the 
                            ! position of the i'th alive agent in the population
                            current_position = current_position + 1
                            new_order(current_position) = j
                            
                        endif
                    enddo

                    ! current_position now equals number of humans in population 
                    number_of_humans_in_pop = current_position
                    hum_t(jp) = number_of_humans_in_pop


                    ! Reset the is_dead matrix for this population
                    is_dead(:,jp) = .true. 

                    do j = 1, number_of_humans_in_pop

                        ! Check if we reached the end of the new order array. 
                        ! Then j -1 = number of alive agents in population
                        if (new_order(j) == -1) then
                            print*, "Error: This should not happen. Check move_alive_agents_to_beginning_of_matrix function" 
                            return
                        endif

                        ! else: 

                        ! Check if the agent is associated
                        if (.not. associated(population_agents_matrix(j,jp)%node)) then
                            print *, "Error: Agent at position (", j, ",", jp, ") is not associated."
                            print*, "In move_alive_agents_to_beginning_of_matrix"
                            cycle
                        end if

                        ! Move the agent in the matrix
                        population_agents_matrix(j,jp) = population_agents_matrix(new_order(j),jp)

                        ! Update the position of the agent in the matrix
                        population_agents_matrix(j,jp)%node%position_human = j

                        ! Update the is_dead matrix
                        is_dead(j,jp) = .false.
                    enddo


        end subroutine move_alive_agents_to_beginning_of_matrix


    subroutine mark_agents_outside_grid_to_be_killed(agents_head,grid)
        type(spatial_grid), intent(in), pointer :: grid
        type(Node), pointer, intent(inout) :: agents_head

        type(Node), pointer :: current_agent
        integer :: gx,gy

        current_agent => agents_head

        do while (associated(current_agent))
            call calculate_grid_pos(current_agent%pos_x, current_agent%pos_y,gx,gy)

            if (gx == -1 .or. gy == -1) then
                call mark_agent_dead_no_grid(current_agent%position_human,current_agent%position_population)
            endif

            current_agent => current_agent%next

        enddo

    end subroutine mark_agents_outside_grid_to_be_killed


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



!########## Debugging stuff DN 18.08.25 ########################################

    subroutine mark_agent_dead_no_grid(hum,population)
        implicit none 
        type(Node), pointer :: agent_ptr
        integer, intent(in) :: hum, population

        integer :: gx, gy

        agent_ptr => population_agents_matrix(hum, population)%node



        if (.not. associated(agent_ptr)) then
            print *, "Error: trying to die an agent that is not associated in matrix! (mark_agent_dead_no_grid)"
            return
        end if

        if (is_dead(hum, population) .eqv. .true.) then
            print *, "Error: trying to die an already dead agent in matrix! (mark_agent_dead_no_grid)"
            return
        else
            is_dead(hum, population) = .true. ! mark the agent as dead in the matrix
            death_counter_matrix = death_counter_matrix + 1
        endif

    end subroutine mark_agent_dead_no_grid





!########## Functions that are used in the functions above #####################


    !=======================================================================
    ! SUBROUTINE: select_random_agents_distinct_from_population
    ! Selects two distinct and valid parent agents from a population.
    !
    ! Arguments:
    !   hum_id_mirror_array [TYPE(pointer_node), IN] - 2D array of agent pointers by (index, population)
    !   population          [INTEGER, IN]            - Index of the population
    !   size_of_population  [INTEGER, IN]            - Number of agents in the population
    !   parent_one          [TYPE(Node), OUT]        - Selected parent agent one
    !   parent_two          [TYPE(Node), OUT]        - Selected parent agent two
    !
    ! Notes:
    !   - Randomly selects two different indices.
    !   - Ensures selected agents are associated (valid).
    !   - Performs bounds checking.
    ! TODO: 
    !   - Sometimes this function fails to select two agents that are associated. 
    !   - It is really not at all clear why in my opinion but I have an idea: 
    !   - Maybe the hum_t variables is not updated correctly somwhere, which could lead this 
    !   - Function so select agents that well do not exist. 
    !=======================================================================

    subroutine select_random_agents_distinct_from_population(hum_id_mirror_matrix, population, & 
                                                             size_of_population, parent_one, parent_two) 
        integer, intent(in) :: population
        integer, intent(in) :: size_of_population
        type(Node), pointer, intent(out) :: parent_one, parent_two
        type(pointer_node), allocatable, intent(in) :: hum_id_mirror_matrix(:,:)  

        type(Node), pointer :: agentOne, agentTwo
        integer :: idx1, idx2
        real :: r
        type(Node), pointer :: temp_agent
        parent_one => null()
        parent_two => null()
        agentOne => null()
        agentTwo => null()


        ! Check if there are enough agents to select two
        if (size_of_population < 2) then
            print *, "Not enough agents to select from. (select_distinct function)"  
            return
        end if


        ! We need to loop until we find two distinct agents that are assiociated because: 
        !       - During the simulation agents at different logical logical sections of one time step,
        !       - The Matrix that holds pointers to the agents grouped by population is not updated 
        !         every time an agent is killed or dies but only once per time step. 
        !        
        !       - Obviously this is a design detail that can be improved in the future but maybe doesnt have to 
        !      
        !       DN 05.07.25

        do while (.not. associated(agentOne) .or. .not.associated(agentTwo))
            ! Select two random agents
            call random_seed()
            call random_number(r)
            idx1 = int(r * size_of_population) + 1
            call random_seed()
            call random_number(r)
            idx2 = int(r * size_of_population) + 1

        
            ! Ensure the two indices are different
            do while (idx1 == idx2)
                call random_seed()
                call random_number(r)
                idx2 = int(r * size_of_population) + 1

            end do

            ! Ensure the indices are within bounds
            if (idx1 > size_of_population .or. idx2 > size_of_population) then
                print *, "Random index out of bounds. (select_distinct function agent_matrix_merge)"
                agentOne => null()
                agentTwo => null()   
                !return
            else
                agentOne => hum_id_mirror_matrix(idx1,population)%node
                agentTwo => hum_id_mirror_matrix(idx2,population)%node
            end if
        end do

        if (.not. associated(agentOne)) then
            print *, "Error: randomly selected agentOne from matrix is not associated! " // &
                    "(select_distinct function) agent_matrix_merge)"
            print *, "idx2: ", idx1 , "max_agents: ", max_agents
            print *, "number_of_agents: ", number_of_agents
            return
        end if 
        if (.not. associated(agentTwo)) then
            print *, "Error: randomly selected agentTwo from matrix is not associated! " // &
                    "(select_distinct function) agent_matrix_merge)"
            print *, "idx2: ", idx2 , "max_agents: ", max_agents
            print *, "number_of_agents: ", number_of_agents
            return
        end if 
        !print *, "Parents get assigned in matrix merge module" ! Debugging 12.06.25
        parent_one => agentOne
        parent_two => agentTwo
        !print *, "We get out of random selection in matrix merge module" ! Debugging 12.06.25

    end subroutine select_random_agents_distinct_from_population











end module mod_agent_tracking
