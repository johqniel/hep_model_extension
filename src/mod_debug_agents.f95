module mod_debug_agents
    use mod_agent_class
    
    use mod_matrix_calculations           ! matrix calculations, e.g. for the birth and death of humans
    

    contains

        subroutine print_information_about_agents(t)
            ! This subroutine prints out the information about the agents
            ! It is used for debugging purposes to check if the agents are correctly initialized
            integer :: t
            integer :: i, population

            if (mod(t,1000) == 0) then 

                print *, "Information about agents at time step: ", t
                print *, "Number of agents: ", number_of_agents , " / ", max_agents 
                print *, "Number of dead agents: ", number_of_dead_agents, " / ", max_dead_agents


            endif
        end subroutine print_information_about_agents

    subroutine print_dimensions_of_arrays(t)
            integer :: t
            ! This subroutine prints out the dimensions of the arrays used in the program
            ! It is used for debugging purposes to check if the arrays are correctly initialized
            if (mod(t,1000) == 0) then 
                print *, "Dimensions of arrays:"
                print *, "is_dead: ", size(is_dead,1 ), size(is_dead,2)
                print *, "x: ", size(x, 1), size(x, 2)
                print *, "population_agents_array: ", size(population_agents_array, 1), size(population_agents_array, 2)
            endif
        end subroutine print_dimensions_of_arrays

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
                        if (.not. associated(population_agents_array(i, population)%node)) then
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

        subroutine compare_counters_of_agents(t)
        integer :: t
            ! This subroutine compares the counters of the agents with the matrix
            ! It checks if the number of agents in the matrix is equal to the number of agents in the agent array
            integer :: i, population
            integer :: total_agents_in_matrix, total_agents_in_array
            integer :: length_agents_list

            if (mod(t,1000) == 0) then 
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
            endif
        end subroutine compare_counters_of_agents


        subroutine check_is_dead_array()
            ! This subroutine checks the is_dead array for consistency
            ! It checks if the is_dead array is consistent with the hum_t array
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
        end subroutine check_is_dead_array

end module mod_debug_agents