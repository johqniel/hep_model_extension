module mod_agent_core

 
    use mod_agent_world

    use mod_grid_id

    use mod_hashmap

    use mod_globals

    

    implicit none
    integer :: number_of_agents_all_time = 0

    contains





    subroutine set_agents_values_from_matrix_hash(agent_,population, human)
            type(Agent), intent(inout) :: agent_
            integer, intent(in) :: population, human




            agent_%pos_x = x(human, population)
            agent_%pos_y = y(human, population)
            agent_%ux = ux(human, population)
            agent_%uy = uy(human, population)

            !print*, "3"
    end subroutine set_agents_values_from_matrix_hash


    subroutine setup_agents_from_matrix_hash(world)
            implicit none
            type(world_container), target, intent(inout) :: world

            type(t_int_map), pointer :: index_map
            integer, pointer :: num_agents_per_pop(:)
            integer :: npops


            integer :: population, human, agent_count, total_agents
            type(Agent) :: temp_agent
            

            if (.not. allocated(world%agents)) then

                print*, "Error: in setup agents not allocated."
                return
            endif

            index_map => world%index_map
            num_agents_per_pop => world%num_humans
            npops = world%config%npops




        

            ! determine the number of agents
            total_agents = 0
            do population = 1, npops
                total_agents = total_agents + hum_t(population)
            end do

            print*, "Size of agents_matrix:", size(world%agents,1), size(world%agents,2)
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

                        temp_agent = world%spawn_agent_hash(population)

                        call set_agents_values_from_matrix_hash(temp_agent, population, human)

                        call add_agent_to_array_hash(world%agents,index_map,temp_agent,num_agents_per_pop,population)
                        
                    end if
                end do
            end do
            
            ! For debugging
            print *, "Total agents processed:", agent_count
            print *, "Expected total agents:", total_agents


            
    end subroutine setup_agents_from_matrix_hash




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