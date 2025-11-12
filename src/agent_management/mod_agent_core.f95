module mod_agent_core

    use mod_agent_hashmap

    implicit none

    contains

    subroutine allocate_agents(agents,num_humans_per_pop,n_pops, max_hum_per_pop)

        implicit none

        integer, intent(in) :: n_pops
        integer, intent(in) :: max_hum_per_pop
        integer, allocatable, target, intent(inout) :: num_humans_per_pop(:) 
        type(Agent), allocatable, target, intent(inout) :: agents(:,:)

        if (allocated(agents)) then
            print*, "Warning: agents already allocated."
            deallocate(agents)

        endif

        allocate(agents(max_hum_per_pop,n_pops))

        allocate(num_humans_per_pop(n_pops))
        num_humans_per_pop = 0

    end subroutine allocate_agents

        function spawn_agent_hash(population) result(agent_spawned)
            integer , intent(in) :: population ! the number of the population and the number of the agent

            type(Agent) :: agent_spawned

            
            integer :: agent_id 
            real :: r


            agent_id = get_agent_id()

            
            call random_number(r)
            if (r < 0.5) then
            tail_agents%gender = 'M'
            else
            tail_agents%gender = 'F'
            end if

            call random_number(r)
            tail_agents%age = int(r * 3500) ! Random age between 0-70
            
            

        end function spawn_agent_hash

    subroutine set_agents_values_from_matrix_hash(agent_,population, human)
            type(Agent), intent(inout) :: agent_
            integer, intent(in) :: population, human




            agent_%pos_x = x(human, population)
            agent_%pos_y = y(human, population)
            agent_%ux = ux(human, population)
            agent_%uy = uy(human, population)

            !print*, "3"
    end subroutine set_agents_values_from_matrix_hash


    subroutine setup_agents_from_matrix_hash(agents,index_map, num_agents_per_pop)
            implicit none
            type(Agent), allocatable, target, intent(inout) :: agents(:)
            type(t_int_map), intent(inout) :: index_map
            type(integer), intent(inout) :: num_agents_per_pop

            integer :: population, human, agent_count, total_agents
            type(Agent) :: temp_agent
            

            if (.not. allocated(agents)) then

                print*, "Error: in setup agents not allocated."
                return
            endif




        

            ! determine the number of agents
            total_agents = 0
            do population = 1, npops
                total_agents = total_agents + hum_t(population)
            end do

            print*, "Size of agents_matrix:", size(agents,1), size(agents,2)
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

                        temp_agent = spawn_agent_hash(population)

                        call set_agents_values_from_matrix_hash(temp_agent, population, human)

                        call add_agent_to_array_hash(agents(:,population),index_map,temp_agent,num_agents_per_pop(population))
                        
                    end if
                end do
            end do
            
            ! For debugging
            print *, "Total agents processed:", agent_count
            print *, "Expected total agents:", total_agents


            
    end subroutine setup_agents_from_matrix_hash


end module mod_agent_core