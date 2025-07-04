module mod_debug_agents
    use mod_agent_class
    
    use mod_matrix_calculations           ! matrix calculations, e.g. for the birth and death of humans

    use mod_agent_matrix_merge
    

    contains

!===============================================================================
!
! This module contains functions that are used to debug the simulation. 
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
            integer :: t
            integer :: i, population

 

                print *, "Information about agents at time step: ", t
                print *, "Number of agents: ", number_of_agents , " / ", max_agents 
                print *, "Number of dead agents: ", number_of_dead_agents, " / ", max_dead_agents


        end subroutine print_information_about_agents


        ! Function that prints the dimensions of the arrays used in the program
        subroutine print_dimensions_of_arrays()
                integer :: t
                ! This subroutine prints out the dimensions of the arrays used in the program
                ! It is used for debugging purposes to check if the arrays are correctly initialized
                    print *, "Dimensions of arrays:"
                    print *, "is_dead: ", size(is_dead,1 ), size(is_dead,2)
                    print *, "x: ", size(x, 1), size(x, 2)
                    print *, "population_agents_matrix: ", size(population_agents_matrix, 1), size(population_agents_matrix, 2)
                
        end subroutine print_dimensions_of_arrays


        ! Function that prints death and birth counters caused by matrix calculations
        subroutine print_born_death_counter_matrix()
      
            ! This subroutine prints the number of agents that were born during the simulation
            ! It is used for debugging purposes to check if the agents are correctly initialized
          
            print *, "From Matrix-Calc, agents born: ", born_counter_matrix , ", agents died: ", death_counter_matrix

          
        end subroutine print_born_death_counter_matrix

        subroutine count_agents_matrix()
            ! This subroutine counts the number of agents in the matrix
            ! It is used for debugging purposes to check if the agents are correctly initialized
            integer :: i, population
            integer :: counter

            counter = 0
            do population = 1, npops
                do i = 1, hum_t(population)
                    if (is_dead(i, population) .eqv. .false.) then
                        counter = counter + 1
                    endif
                enddo
            enddo

            print *, "Number of agents in matrix: ", counter
        end subroutine count_agents_matrix

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

        subroutine compare_matrix_and_agent_matrix()
            ! This subroutine compares the matrixes that are used for the calculations
            ! with the the matrix of pointer to the agents that is used to connect the
            ! information stored in the matrix with the agent objects.

            ! It basically checks if all the agents are associated
            ! If there is a mismatch it will print out the mismatch. 
            integer :: i, j, population
            integer :: mismatch_counter

            mismatch_counter = 0
            do population = 1, npops
                do i = 1, hum_t(population)
                    if (x(i, population) /= -1.0E3) then
                        if (.not. associated(population_agents_matrix(i, population)%node)) then
                            if (mismatch_counter == 0) then
                                print *, "Mismatch in population", population, "for agent", i, &
                                    "in matrix x: ", x(i, population), "but no agent found."
                                print *, "Agents id is: ", hum_id(i,population)
                            endif
                            
                            mismatch_counter = mismatch_counter + 1
                        endif
                    endif
                enddo
            enddo
            if (mismatch_counter > 0) then
                print *, "Total number of mismatches found: ", mismatch_counter
            else
                !print *, "All agents are correctly associated with the matrix."
            endif
        end subroutine compare_matrix_and_agent_matrix


        subroutine compare_counters_of_agents()
    
            ! This subroutine compares the counters of the agents with the matrix
            ! It checks if the number of agents in the matrix is equal to the number of agents in the agent array
            integer :: i, population
            integer :: total_agents_in_matrix, total_agents_in_array
            integer :: length_agents_list

 
                total_agents_in_matrix = 0
                total_agents_in_array = 0

                do population = 1, npops
                    do i = 1, hum_t(population)
                        if (is_dead(i, population) .eqv. .false.) then
                            total_agents_in_matrix = total_agents_in_matrix + 1
                        endif
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

        subroutine check_is_dead_matrix()
            ! This subroutine checks the is_dead matrix for consistency
            ! It checks if the is_dead array is consistent with the hum_t matrix
            integer :: i, population
            integer :: counter

            counter = 0

            do population = 1, npops
                do i = 1, hum_t(population)
                    if (is_dead(i, population) .eqv. .false.) then
                        if (x(i,population) == -1.0E3) then
                            counter = counter + 1
                        endif
                    endif
                enddo
            enddo

            if (counter > 0) then
                print *, "Error: is_dead array has ", counter, &
                    " agents marked as alive but their x value is -1.0E3."
            else
                !print *, "is_dead array is consistent with the x array."
            endif
        end subroutine check_is_dead_matrix

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
                do i = 1, hum_t(population)
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
            integer :: counter

            counter = 0
            

            do population = 1, npops
                do i = 1, hum_t(population)
                    if (.not. i == population_agents_matrix(i, population)%node%position_human) then
                        counter = counter + 1
                        print *, "Error: Agent at position (", i, ",", population, ") has inconsistent position in matrix."
                        print *, "Expected position: ", i, " but found: ", &
                                                                    population_agents_matrix(i, population)%node%position_human
                    endif
                    if (.not. population == population_agents_matrix(i, population)%node%position_population) then
                        counter = counter + 1
                        print *, "Error: Agent at position (", i, ",", &
                                                                            population, ") has inconsistent population in matrix."
                        print *, "Expected population: ", population, " but found: ", &
                                                                    population_agents_matrix(i, population)%node%position_population
                    endif
                enddo
            enddo
            if (counter > 0) then
                print *, "Total number of inconsistencies in population_agents_matrix found: ", counter
            else
                !print *, "All agents have consistent positions in the matrix."
            endif
        end subroutine check_position_in_matrix_consistency


        subroutine check_dead_agents_list_for_alive_agents()
            integer :: counter
            type(Node), pointer :: current_agent
            counter = 0

            current_agent => head_dead_agents
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


        

end module mod_debug_agents