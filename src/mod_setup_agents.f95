module mod_setup_agents
    use mod_agent_class
    use mod_matrix_calculations

    contains

        subroutine setup_agents_from_matrix()
            type(Node), pointer :: agent_head, current_agent, new_agent, last_agent
            integer :: population, human, agent_count, total_agents

            if (.not. allocated(agents_array)) then
                call initilize_agents_array()
                print *, "AGENTS ARRAY NOT ALLOCATED - allocating now"
            end if
            if (.not. allocated(dead_agents_array)) then
                call initilize_dead_agents_array()
                print *, "DEAD AGENTS ARRAY NOT ALLOCATED - allocating now"

            end if  



            ! determine the number of agents
            total_agents = 0
            do population = 1, npops
                total_agents = total_agents + hum_t(population)
            end do

            ! Process each population
            agent_count = 0
            do population = 1, npops
                ! Process each agent in this population
                do human = 1, hum_t(population)
                    ! Only process valid agents (those with valid positions)
                    if (x(human, population) > -1.0E3 .and. y(human, population) > -1.0E3) then
                        ! Create a new agent node
                        call spawn_agent_from_matrix(population, human)
                    end if
                end do
            end do
            
            ! For debugging
            print *, "Total agents processed:", agent_count
            print *, "Expected total agents:", total_agents


            
        end subroutine setup_agents_from_matrix


        subroutine spawn_agent_from_matrix(j_pop, i_hum)
            integer :: j_pop, i_hum ! the number of the population and the number of the agent
            
            real :: pos_x, pos_y
            integer :: agent_id 
            real :: r

            pos_x = x(i_hum, j_pop)
            pos_y = y(i_hum, j_pop)
            agent_id = get_agent_id()

            call append(agent_id)

            tail_agents%pos_x = pos_x
            tail_agents%pos_y = pos_y
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

            tail_agents%age = 20 + mod(agent_id, 40)  ! Random age between 20-80
            

            ! management of the agent array
            ! is already done in append function

            !call add_agent_to_array(tail_agents)


        end subroutine spawn_agent_from_matrix

end module mod_setup_agents
