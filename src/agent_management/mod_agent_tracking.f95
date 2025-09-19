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
    !Uses: hum_t, hum_id, npops

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
    subroutine write_new_positions_to_matrix(x_mat,y_mat,ux_mat,uy_mat, agents_matrix, number_agents_in_pop)
        implicit none
        type(pointer_node), intent(in) :: agents_matrix(:,:)
        real(8), intent(out) :: x_mat(:,:), y_mat(:,:), ux_mat(:,:), uy_mat(:,:)
        integer, intent(in) :: number_agents_in_pop(:)
        
        integer :: i

        integer :: population


        do population = 1, size(number_agents_in_pop)




            x_mat(:,population) = -1000
            y_mat(:,population)  = -1000
            ux_mat(:,population)  = 0.0
            uy_mat(:,population)  = 0.0

            ! Write the new positions and velocities to the matrix
            do i = 1, number_agents_in_pop(population)




                ! Security checks: 
                if(.not. associated(agents_matrix(i,population)%node)) then
                    print*, "Problem in agents_matrix."
                    cycle 
                endif

                if (agents_matrix(i,population)%node%is_dead) then
                    print*, " Problem in agents_matrix" 
                    cycle
                endif

                ! End Security checks.

                x_mat(i,population) = agents_matrix(i,population)%node%pos_x
                y_mat(i,population) = agents_matrix(i,population)%node%pos_y
                ux_mat(i,population) = agents_matrix(i,population)%node%ux
                uy_mat(i,population) = agents_matrix(i,population)%node%uy
            end do

         

        enddo


    end subroutine write_new_positions_to_matrix

 
!##########################################################################################################################
! Functions that keep track of the alive and dead agents and sorts them into an array such that we can quickly access them
!##########################################################################################################################

    

!################ Killing the agents #######################################

    subroutine kill_agents_outside_of_grid(agents_head)
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
        real(8) :: pos_x, pos_y

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



      
        !print*, "Before data Management."

        ! Data Management Variables:
        hum_id(hum,population) =  get_agent_id() ! get a new id for the agent                                                             
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
