module mod_agent_core

 
    use mod_agent_hashmap

    use mod_grid_id

    use mod_globals

    

    implicit none
    integer :: number_of_agents_all_time = 0

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
            
            agent_spawned%population = population


            agent_id = get_agent_id()

            agent_spawned%id = agent_id


            
            call random_number(r)
            if (r < 0.5) then
            agent_spawned%gender = 'M'
            else
            agent_spawned%gender = 'F'
            end if

            call random_number(r)
            agent_spawned%age = int(r * 3500) ! Random age between 0-70
            
            

        end function spawn_agent_hash

    function generate_agent_born(mother, father, population) result(agent_spawned)
        type(Agent), pointer, intent(in) :: father
        type(Agent), pointer, intent(in) :: mother
        integer, intent(in) :: population



        type(Agent) :: agent_spawned

        real(8) :: pos_x, pos_y, ux, uy
        real :: r


        ! Set gender, id and age
        agent_spawned = spawn_agent_hash(population)

        ! reset age: 

        agent_spawned%age = 0

        ! Set position and velo = mothers pos, velo
        agent_spawned%pos_x = mother%pos_x
        agent_spawned%pos_y = mother%pos_y
        agent_spawned%ux = mother%ux
        agent_spawned%uy = mother%uy

        ! Set population
        agent_spawned%population = population 




      ! #########    add new agent to child list of father and mother ########

            ! TODO

      ! ################## Gene propagation ####################################

            ! TODO

            ! call get_genes(father)
            ! call get_genes(mother)

            ! call gene_model()
            
            !self.genes = new_genes()
      
    end function generate_agent_born

    subroutine agent_born(agents, grid, index_map, num_humans_per_pop, population, parent_one_id, parent_two_id)  
        implicit none 
        type(Agent), allocatable, target, intent(inout) :: agents(:,:)
        type(t_int_map), intent(inout) :: index_map
        type(spatial_grid), pointer :: grid

        integer, intent(in) :: parent_one_id, parent_two_id ! parent_one = mother
        integer, intent(in) :: population
        integer, intent(inout) :: num_humans_per_pop(:)
        
        integer :: population_size
        type(Agent) :: new_agent
        type(Agent), pointer :: father
        type(Agent), pointer :: mother
        type(Agent), pointer :: child
        real(8) :: pos_x, pos_y

        integer :: gx,gy

        !print*, " We are in function."

        population_size = num_humans_per_pop(population) ! get the size of the population
      
        if (population_size + 1 > hum_max_A) then
            print*, "Error: Population size exceeded maximum in population ", population
            return
        end if

    
        father => get_agent(parent_one_id, index_map, agents)
        mother => get_agent(parent_two_id, index_map, agents)

        
        new_agent = generate_agent_born(mother, father, population)

        call add_agent_to_array_hash(agents,index_map,new_agent,num_humans_per_pop,population,child) ! optional argument to get pointer to new agent

      

        pos_x = new_agent%pos_x     
        pos_y = new_agent%pos_y




        ! PLacemend of the agent in the grid
        !child%grid => grid ! linked agent to the grid this is done in place agent in subroutine
        call grid%place_agent_in_grid(child)

        agents_born_counter = agents_born_counter + 1

    end subroutine agent_born

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
            type(Agent), allocatable, target, intent(inout) :: agents(:,:)
            type(t_int_map), intent(inout) :: index_map
            integer, intent(inout) :: num_agents_per_pop(:)

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

                        call add_agent_to_array_hash(agents,index_map,temp_agent,num_agents_per_pop,population)
                        
                    end if
                end do
            end do
            
            ! For debugging
            print *, "Total agents processed:", agent_count
            print *, "Expected total agents:", total_agents


            
    end subroutine setup_agents_from_matrix_hash


    function get_agent_id() result(id)
      integer :: id
      id = number_of_agents_all_time + 1
      number_of_agents_all_time = number_of_agents_all_time + 1
    end function get_agent_id

    !=======================================================================
    ! SUBROUTINE: write_new_positions_to_matrix
    ! writes the positions and velocities of the agents in the public matrix x,y,ux,uy
    !
    !
    ! Notes:
    !
    !   This functions only purpose is to tell the old programm written by constantin
    !   That updates the hep, the positions of the agents that are now calculated by the 
    !   code written by DN. 
    !=======================================================================
    subroutine write_new_positions_to_matrix(x_mat,y_mat,ux_mat,uy_mat, agents_matrix, number_agents_in_pop)
        implicit none
        type(Agent), intent(in), allocatable :: agents_matrix(:,:)
        real(8), intent(out) :: x_mat(:,:), y_mat(:,:), ux_mat(:,:), uy_mat(:,:)
        integer, intent(in) :: number_agents_in_pop(:)
        
        integer :: i

        integer :: population


        ! Security checks: 
        if(.not. allocated(agents_matrix) ) then
            print*, "Error: agents matrix not associated."
            return 
        endif

        do population = 1, size(number_agents_in_pop)




            x_mat(:,population) = -1000
            y_mat(:,population)  = -1000
            ux_mat(:,population)  = 0.0
            uy_mat(:,population)  = 0.0

            ! Write the new positions and velocities to the matrix
            do i = 1, number_agents_in_pop(population)



                if (agents_matrix(i,population)%is_dead) then
                    print*, "Warning: Matrix that holds agents <-> num_agents array, connetion corrupted." 
                    cycle
                endif


                x_mat(i,population) = agents_matrix(i,population)%pos_x
                y_mat(i,population) = agents_matrix(i,population)%pos_y
                ux_mat(i,population) = agents_matrix(i,population)%ux
                uy_mat(i,population) = agents_matrix(i,population)%uy
            end do

         

        enddo


    end subroutine write_new_positions_to_matrix



end module mod_agent_core