
!===============================================================================
!
! This File contains functions that are used to debug the simulation.
! It should be included in the main file to be able to run it during simulations 
! So far it contains two types of functions:
!
!   - First it contains functions that simply print information to the console
!
!         Example: print_dimensions_of_arrays()
!                  (prints dimensions of some dynamic arrays to console)
!     
!   - Then it contains functions that check the datastructures wheter they 
!     are consistent with each other. They only print to the console if that is
!     is not the case
!
!         Example: compare_counters_of_agents()
!                  (Checks if matrix and list contain the same amount of agents)
!                  (Prints to console if that is not the case)
!
!===============================================================================



! #############################################################################
! Informative Functions // Function that just print information
! #############################################################################

        ! Function that prints information about the agents in the matrix
        subroutine print_information_about_agents()
            ! This subroutine prints out the information about the agents
            ! It is used for debugging purposes to check if the agents are correctly initialized


 

                print *, "Information about agents at time step: ", t
                print *, "Number of agents: ", number_of_agents , " / ", max_agents 
                print *, "Number of dead agents: ", number_of_dead_agents, " / ", max_dead_agents


        end subroutine print_information_about_agents





        ! Function that prints death and birth counters caused by matrix calculations
        subroutine print_born_death_counter_matrix()
      
            ! This subroutine prints the number of agents that were born during the simulation
            ! It is used for debugging purposes to check if the agents are correctly initialized
          
            print *, "From Matrix-Calc, agents born: ", born_counter_matrix , ", agents died: ", death_counter_matrix

          
        end subroutine print_born_death_counter_matrix



        subroutine count_agents_list()
            ! This subroutine counts the number of agents in the agent list
            ! It is used for debugging purposes to check if the agents are correctly initialized
            integer :: counter
            type(Node), pointer :: current_agent

            counter = 0
            current_agent => head_agents
            do while (associated(current_agent))
                counter = counter + 1
                current_agent => current_agent%next
            enddo

            print *, "Number of agents in agent list: ", counter
        end subroutine count_agents_list

        subroutine count_dead_agents_list()
            ! This subroutine counts the number of dead agents in the dead agent list
            ! It is used for debugging purposes to check if the agents are correctly initialized
            integer :: counter
            type(Node), pointer :: current_agent

            counter = 0
            current_agent => head_dead_agents
            do while (associated(current_agent))
                counter = counter + 1
                current_agent => current_agent%next
            enddo

            print *, "Number of dead agents in dead agent list: ", counter
        end subroutine count_dead_agents_list



! #############################################################################
! Functions that check the consistency of program and print if error are found
! #############################################################################



        subroutine compare_counters_of_agents()
    
            ! This subroutine compares the counters of the agents with the matrix
            ! It checks if the number of agents in the matrix is equal to the number of agents in the agent array
            integer :: i, population
            integer :: total_agents_in_matrix, total_agents_in_array
            integer :: length_agents_list

 
                total_agents_in_matrix = 0
                total_agents_in_array = 0

                do population = 1, npops
                    do i = 1, num_humans_in_pop(population)
                        if (.not. associated(population_agents_matrix(i,population)%node)) then
                            !print*, "Problem in population_matrix."
                            cycle
                        endif
                        if (population_agents_matrix(i,population)%node%is_dead) then
                            cycle
                        endif
                        
                        total_agents_in_matrix = total_agents_in_matrix + 1

                    enddo
                enddo
                ! Count the number of agents in the agent list manually
                length_agents_list = count_agents()
                if (total_agents_in_matrix /= number_of_agents) then
                    print *, "Mismatch in number of agents: ", &
                        "Matrix has ", total_agents_in_matrix, " agents, ", &
                        "but agent array has ", number_of_agents, " ==  ", length_agents_list ,"agents."
                else 
                    !print *, "Number of agents in matrix and agent array is consistent: ", &
                    !   total_agents_in_matrix, " agents."
                endif
        end subroutine compare_counters_of_agents



        subroutine check_agents_array()
            integer :: i
            integer :: counter
            counter = 0

            do i = 1, number_of_agents
                if (.not. associated(agents_array(i)%node)) then
                    counter = counter + 1
                endif
            enddo

            if (counter > 0) then
                print *, "Error: There are ", counter, &
                    " agents in the agents array that are not associated with a node."
            else
                !print *, "All agents in the agents array are associated with a node."
            endif

        end subroutine check_agents_array

        subroutine check_population_agents_matrix()
            integer :: i, population
            integer :: counter

            counter = 0

            do population = 1, npops
                do i = 1, num_humans_in_pop(population)
                    if (.not. associated(population_agents_matrix(i,population)%node)) then
                        counter = counter + 1
                    endif
                enddo
            enddo
            if (counter > 0) then
                print *, "Error: There are ", counter, &
                    " agents in the population_agents_matrix that are not associated with a node."
            else
                !print *, "All agents in the population_agents_matrix are associated with a node."
            endif 
        end subroutine check_population_agents_matrix

        subroutine check_alive_agents_list_for_dead_agents()
            integer :: counter
            type(Node), pointer :: current_agent
            counter = 0

            current_agent => head_agents
            if (current_agent%is_dead) then
                counter = counter + 1
            endif
            do while (associated(current_agent%next))
                if (current_agent%is_dead) then
                    counter = counter + 1
                endif
                current_agent => current_agent%next
            enddo

            if (counter > 0) then
                print *, "Error: There are ", counter, &
                    " dead agents in alive agents list."
            else
                !print *, "Alive agents list contains no dead agents."
            endif
        end subroutine check_alive_agents_list_for_dead_agents
        
        subroutine check_position_in_matrix_consistency()
            integer :: i, population
            integer :: counter_p, counter_i

            counter_p = 0
            counter_i = 0
            
            print*, "Searching ", npops, "many populations with: ", num_humans_in_pop, "many agents."

            do population = 1, npops
                do i = 1, num_humans_in_pop(population)
                    if (.not. i == population_agents_matrix(i, population)%node%position_human) then
                        counter_i = counter_i + 1
                        !print *, "Error: Agent at position (", i, ",", population, ") has inconsistent position in matrix."
                        !print *, "Expected position: ", i, " but found: ", &
                        !                                            population_agents_matrix(i, population)%node%position_human
                    endif
                    if (.not. population == population_agents_matrix(i, population)%node%position_population) then
                        counter_p = counter_p + 1
                        !print *, "Error: Agent at position (", i, ",", &
                        !                                                    population, ") has inconsistent population in matrix."
                        !print *, "Expected population: ", population, " but found: ", &
                        !                                            population_agents_matrix(i, population)%node%position_population
                    endif
                enddo
            enddo
            if (counter_p > 0) then
                print *, "Total number of inconsistencies in population_agents_matrix (population) found: ", counter_p
            else
                !print *, "All agents have consistent positions in the matrix."
            endif
            if (counter_i > 0) then
                print *, "Total number of inconsistencies in population_agents_matrix (index) found: ", counter_i
            else
                !print *, "All agents have consistent positions in the matrix."
            endif
        end subroutine check_position_in_matrix_consistency


        subroutine check_dead_agents_list_for_alive_agents()
            integer :: counter
            type(Node), pointer :: current_agent
            counter = 0

            current_agent => head_dead_agents
            
            if (.not. associated(current_agent)) then
                !print*, "No agent died yet."
                return
            endif

            if (current_agent%is_dead .eqv. .false.) then
                counter = counter + 1
            endif
            do while (associated(current_agent%next))
                if (current_agent%is_dead .eqv. .false.) then
                    counter = counter + 1
                endif
                current_agent => current_agent%next
            enddo

            if (counter > 0) then
                print *, "Error: There are ", counter, &
                    " alive agents in alive agents list."
            else
                !print *, "Dead agents list contains no alive agents."
            endif
        end subroutine check_dead_agents_list_for_alive_agents



        subroutine check_positions_matrix(agent_head)
            type(Node), pointer, intent(in) :: agent_head


            type(Node), pointer :: current_agent

            integer :: counter, i, jp

            counter = 0

            current_agent => agent_head

            do while (associated(current_agent))

                i = current_agent%position_human
                jp = current_agent%position_population

                if (reals_equal(current_agent%pos_x, x(i,jp)) .or. reals_equal(current_agent%pos_y,y(i,jp))) then
                    counter = counter + 1
                endif

                current_agent => current_agent%next

            enddo

            if (counter > 0) then
            
                print*, "There are: ", counter, " many agent where positions dont match with x,y matrix."

            endif

        end subroutine check_positions_matrix

        subroutine check_hum_t_coherent(agent_head, number_agents_in_population)
            implicit none
            type(Node), pointer, intent(in) :: agent_head
            integer, intent(in) :: number_agents_in_population(:)

            type(Node), pointer :: current_agent
            integer :: counter(size(number_agents_in_population))
            integer :: i = 1

            counter = 0
            current_agent => agent_head

            !print*, " We enter hum_t_coherent "

            do while (associated(current_agent))
                counter(current_agent%position_population) = counter(current_agent%position_population) + 1
                current_agent => current_agent%next

            enddo

            do i = 1, size(number_agents_in_population)
                if (counter(i) /= number_agents_in_population(i)) then
                    print*, "Mismatch in population: ", i, " expected: ", number_agents_in_population(i), &
                            " but found: ", counter(i)
                endif
            enddo


        end subroutine check_hum_t_coherent

        subroutine check_num_hum_in_pop_coherent(agent_head, number_agents_in_population)
            implicit none
            type(Node), pointer, intent(in) :: agent_head
            integer, intent(in) :: number_agents_in_population(:)

            type(Node), pointer :: current_agent
            integer :: counter(size(number_agents_in_population))
            integer :: i = 1

            counter = 0
            current_agent => agent_head

            !print*, " We enter num_hum_in_pop_coherent "

            do while (associated(current_agent))
                counter(current_agent%position_population) = counter(current_agent%position_population) + 1
                current_agent => current_agent%next

            enddo

            do i = 1, size(number_agents_in_population)
                if (counter(i) /= number_agents_in_population(i)) then
                    print*, "Mismatch in population: ", i, " expected: ", number_agents_in_population(i), &
                            " but found: ", counter(i)
                endif
            enddo


        end subroutine check_num_hum_in_pop_coherent

