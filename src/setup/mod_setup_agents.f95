module mod_setup_agents
    use mod_agent_class


    use mod_globals
    ! in common_variables.inc
    ! Uses:     - x,y,ux,uy

    ! in parameters.inc
    ! Uses:     - npops

    contains


        !=======================================================================
        ! SUBROUTINE: setup_agents_from_matrix
        ! Initializes agents in memory from matrix data (positions, IDs, etc.).
        !
        ! Arguments:
        !   None (uses global/shared variables)
        !
        ! Notes:
        !   - Iterates over all populations and their agents.
        !   - For each valid agent (i.e., with position > -1.0E3), creates a node.
        !   - Populates `population_agents_matrix` with agent nodes.
        !   - Tracks and compares total agents expected vs processed.
        !=======================================================================
        subroutine setup_agents_from_matrix()
            implicit none
            type(Node), pointer :: current_agent
            integer :: population, human, agent_count, total_agents
            


            if (.not. allocated(population_agents_matrix)) then
                print *, "Error: Population_agents_matrix NOT ALLOCATED"
            end if
            



            ! determine the number of agents
            total_agents = 0
            do population = 1, npops
                total_agents = total_agents + hum_t(population)
            end do

            print*, "Size of population_agents_matrix:", size(population_agents_matrix,1), size(population_agents_matrix,2)
            print*, "Size of x:", size(x,1), size(x,2)
            print*, "Total agents expected from hum_t:", total_agents
            print*, "npops: ", npops, " =  size of hum_t: ", size(hum_t)

            ! Process each population
            agent_count = 0
            do population = 1, npops
            print *, "Population:", population  
            print*, "hum_t(population): ", hum_t(population)
                ! Process each agent in this population
                do human = 1, hum_t(population)
                    ! Only process valid agents (those with valid positions)
                    if (x(human, population) > -1.0E3 .and. y(human, population) > -1.0E3) then
                        ! Create a new agent node
                        !print *, "Creating agent for human:", human, "in population:", population
                        !population_agents_matrix(human,population)%node => spawn_agent(population)
                        !print*, "spawned agent."
                        ! This should also work: 
                        current_agent => spawn_agent(population)

                        if (population_agents_matrix(human,population)%node%position_human /= human) then
                            print*, "Error in index of humans for pop matrix."
                        endif

                        !population_agents_matrix(human,population)%node => tail_agents

                        !print*, "A"
                        population_agents_matrix(human,population)%node%position_human = human
                        population_agents_matrix(human,population)%node%position_population = population
                        !print*, "B: ", human, " ", population
                        

                        if (population_agents_matrix(human,population)%node%position_human /= human) then
                            print*, "Error in index of humans for pop matrix."
                        endif

                        current_agent => population_agents_matrix(human,population)%node
                        
                        !print*, "Spawned agent ID:", population_agents_matrix(population,human)%node%id, &
                        !         "at position (", population_agents_matrix(population,human)%node%pos_x, ",", &
                        !         population_agents_matrix(population,human)%node%pos_y, ")"


                        !print*, "Setting Values."
                        
                        call set_agents_values_from_matrix(current_agent)
                        !print*, "Values set."

                        
                    end if
                end do
            end do
            
            ! For debugging
            print *, "Total agents processed:", agent_count
            print *, "Expected total agents:", total_agents


            
        end subroutine setup_agents_from_matrix
        !=======================================================================
        ! SUBROUTINE: initilize_agent_array_mirror_of_hum_id
        ! Allocates the 2D array used to store agent pointers by (human, population).
        !
        ! Arguments:
        !   hum_max_A [INTEGER, IN] - Max number of agents per population
        !   npops     [INTEGER, IN] - Number of populations
        !
        ! Notes:
        !   - Initializes `population_agents_matrix` and `population_agents_matrix0`.
        !   - These arrays serve as mirrors of the matrix representation for agent linking.
        !=======================================================================

        subroutine set_agents_values_from_matrix(current_agent)
            type(Node), pointer, intent(inout) :: current_agent

            integer :: population, human

            !print*, "1"

            population = current_agent%position_population
            human = current_agent%position_human
            !print *, "Setting agent values for human:", human, "in population:", population

            !print*, "2:",human, "  ", population

            current_agent%pos_x = x(human, population)
            current_agent%pos_y = y(human, population)
            current_agent%ux = ux(human, population)
            current_agent%uy = uy(human, population)

            !print*, "3"
        end subroutine set_agents_values_from_matrix

        subroutine allocate_population_agents_matrix(n_humans, n_populations)
            implicit none
            integer, intent(in) :: n_humans, n_populations

            integer :: i,j


            ! I wanted to seperate the two different data structures as strict 
            ! as possible but i think regarding the position int the array wise 
            ! i have to go into the old code 

            ! each time when the position of the agents in the array is changed
            ! the new position has to be passed to the agent object. t
            allocate(population_agents_matrix(n_humans, n_populations))
            allocate(population_agents_matrix0(n_humans, n_populations))

            do j = 1, n_populations
                do i = 1, n_humans
                    population_agents_matrix(i,j)%node => null()
                    population_agents_matrix0(i,j)%node => null()
                                        population_agents_matrix(i,j)%next => null()
                    population_agents_matrix0(i,j)%next => null()
                                        population_agents_matrix(i,j)%prev => null()
                    population_agents_matrix0(i,j)%prev => null()
                end do
            end do
            
        end subroutine allocate_population_agents_matrix


        !=======================================================================
        ! FUNCTION: spawn_agent
        ! Creates and returns a new agent object with random age, no relatives, random gender.
        !
        ! Arguments:
        !   j_pop   [INTEGER, IN] - Population index

        !
        ! Returns:
        !   agent_spawned [TYPE(Node), POINTER] - Pointer to the new agent node
        !
        ! Notes:
        !   - Pulls position from matrix (x, y).
        !   - Generates a new unique agent ID.
        !   - Appends the new agent to the global agent list.
        !   - Sets metadata: position, ID, family links, gender, age.
        !=======================================================================
        function spawn_agent(population) result(agent_spawned)
            integer , intent(in) :: population ! the number of the population and the number of the agent

            type(Node), pointer :: agent_spawned

            
            integer :: agent_id 
            real :: r


            agent_id = get_agent_id()

            call append_agent(agent_id,population)

            tail_agents%father => null()
            tail_agents%mother => null()
            tail_agents%children => null()
            tail_agents%siblings => null()

        
            
            call random_number(r)
            if (r < 0.5) then
            tail_agents%gender = 'M'
            else
            tail_agents%gender = 'F'
            end if

            call random_number(r)
            tail_agents%age = r * 3500 ! Random age between 0-70
            
            agent_spawned => tail_agents
            ! management of the agent array
            ! is already done in append function

            !call add_agent_to_array(tail_agents)


        end function spawn_agent

end module mod_setup_agents
