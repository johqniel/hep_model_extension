module mod_agent_matrix_merge
! This is the module that contains all function that glue the two data structures to gether
! on the one side we have the matrix way of organising things, that is really good a computing stuff
! very fast.

! On the other hand we have the agent class, that safes agents in a double linked list. 

! In here we basically declare all functions that are used in the mod_matrix_calculations, 
! that manipulate the agents list
! this way we can keep the two data structures as separate as possible
! but still have the possibility to manipulate the agents in the list

    use mod_agent_class
    use mod_setup_hep
    implicit none

    integer :: born_counter_matrix = 0
    integer :: death_counter_matrix = 0


    

contains
    
    !=======================================================================
    ! SUBROUTINE: agent_die_from_matrix_calc
    ! Marks an agent as dead in the matrix if not already dead.
    !
    ! Arguments:
    !   hum         [INTEGER, IN] - Index of the human within the population
    !   population  [INTEGER, IN] - Index of the population the agent belongs to
    !
    ! Notes:
    !   Increments the death counter. Prevents double-death errors.
    !=======================================================================
    subroutine agent_die_from_matrix_calc(hum, population)
        implicit none 
        type(Node), pointer :: agent_ptr
        integer, intent(in) :: hum, population

        if (is_dead(hum, population) .eqv. .true.) then
            print *, "Error: trying to die an already dead agent in matrix! (agent_die_from_matrix_calc)"
            return
        else
            is_dead(hum, population) = .true. ! mark the agent as dead in the matrix
            death_counter_matrix = death_counter_matrix + 1
        endif

    end subroutine agent_die_from_matrix_calc

    !=======================================================================
    ! SUBROUTINE: agent_spawn_from_matrix_calc
    ! Placeholder for spawning a new agent in the matrix-based population.
    !
    ! Arguments:
    !   population        [INTEGER, IN] - Index of the population
    !   population_size   [INTEGER, IN] - Current size of the population
    !
    ! Notes:
    !   Not yet implemented.
    !=======================================================================
    subroutine agent_spawn_from_matrix_calc(population, population_size)
      implicit none 
      integer, intent(in) :: population
      integer, intent(in) :: population_size
      
        ! Still needs to be implemented

    end subroutine agent_spawn_from_matrix_calc


    !=======================================================================
    ! SUBROUTINE: agent_born_from_matrix_calc
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
    subroutine agent_born_from_matrix_calc(population, hum_t)  
      implicit none 
      integer, dimension(npops) :: hum_t                     ! number of humans in each population

      integer, intent(in) :: population
      integer :: population_size
      type(Node), pointer :: new_agent
      integer :: pos_in_population
      integer:: pos_father, pos_mother, hum
      real :: pos_x, pos_y, ux_i, uy_i

      type(Node), pointer :: parent_one, parent_two

      population_size = hum_t(population) ! get the size of the population
      
      if (population_size + 1 > hum_max_A) then
          print *, "Error: arrays for matrix calc are not big enough! (agent_born_from_matrix_calc)"
          return
      end if

      ! For debugging purposes: 
      born_counter_matrix = born_counter_matrix + 1      

      ! gender has to be checked for the random selected agents as parents!!!!
      call select_random_agents_distinct_from_population(population_agents_matrix, &
                                                         population, population_size, parent_one, parent_two)

      !print *, "Parents selected in agend born from matrix calc function" ! Debugging DN 13.06.25
      call agent_born(parent_one, parent_two)
      !print *, "Agent born in agent born from matrix calc function" ! Debugging DN 13.06.25
      new_agent => tail_agents


      
      pos_x = new_agent%pos_x     
      pos_y = new_agent%pos_y

      hum_t(population) = population_size + 1 ! update the number of humans in the population
      hum = hum_t(population) ! get the new position of the agent in the matrix

      if (is_dead(hum + 1, population) .eqv. .false.) then
          print *, "Error: trying overwrite a alive agent in matrix! (agent_born_from_matrix_calc)"
          !print *, "hum_id: ", hum_id(hum + 1, population), "population: ", population
          return
      end if
      
     
       if (associated(parent_one) .and. associated(parent_two)) then
            pos_father = parent_one%position_human
            pos_mother = parent_two%position_human
            ux_i = (ux(pos_father,population) + ux(pos_mother,population))/2      
            uy_i = (uy(pos_father,population) + uy(pos_mother,population))/2 
        else
            print *, "Error: parents are not associated in agent_born_from_matrix_calc"
            ux_i = 0.0
            uy_i = 0.0
        end if

        !print *, "Parents selected in agend born from matrix calc function" ! Debugging DN 13.06.25
       
       ! Model Variables:                                                  
            x(hum,population) = pos_x                                                                         
            y(hum,population) = pos_y                                                                             
            ux(hum,population) = ux_i                                                                        
            uy(hum,population) = uy_i 

            ! Data Management Variables:
            hum_id(hum,jp) =  get_agent_id() ! get a new id for the agent                                                             
            is_dead(hum,jp) = .false.                                                                           
            population_agents_matrix(hum,jp)%node => new_agent

    end subroutine agent_born_from_matrix_calc


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


    !=======================================================================
    ! SUBROUTINE: make_pop_array_empty
    ! Sets all agent pointers in a population array to null.
    !
    ! Arguments:
    !   pop_array [TYPE(pointer_node), INOUT] - 2D allocatable array of agent pointers (agents x populations)
    !
    ! Notes:
    !   - Clears all agents across all populations.
    !   - Loops through entire matrix and nullifies each pointer.
    !   - This is used to rearange the matrix when agents die or are removed
    !=======================================================================
    subroutine make_pop_array_empty(pop_array)
        implicit none
        type(pointer_node), allocatable , intent(inout):: pop_array(:,:)
        integer :: number_of_pops, hum_per_pop, i, j

        number_of_pops = size(pop_array, 2)
        hum_per_pop = size(pop_array, 1)

        do j = 1, number_of_pops
            do i = 1, hum_per_pop
                ! This can be made a lot faster since most of the time the pointer is already null
                pop_array(i,j)%node => null()
            end do
        end do

    end subroutine make_pop_array_empty



    !=======================================================================
    ! SUBROUTINE: update_agent_list_from_matrix
    ! Updates the variables of each agent in the linked list based on the matrix data. 
    !
    ! Notes: 
    !    - The following variables have to be updated: 
    !        - all the variables that are computed in the matrix calculations
    !        - the position of the agent in the matrix (its population and its position in the population)
    !
    !    - We have to figure out a better way to do this. 
    !      DN 05.07.25
    !
    !=======================================================================

    
    subroutine update_agent_list_from_matrix(hum_t)
        integer, dimension(npops) :: hum_t   

        integer :: jp, i 
        type(Node), pointer :: current_agent

        do jp = 1, npops
            do i = 1, hum_t(jp)

                current_agent => population_agents_matrix(i,jp)%node

                if (.not. associated(current_agent)) then
                    print *, "Error: Agent at position (", i, ",", jp, ") is not associated."
                else
                    ! Update the agent's position in the linked list
                    current_agent%position_population = jp
                    current_agent%position_human = i
                    current_agent%pos_x = x(i,jp)
                    current_agent%pos_y = y(i,jp)
                    
                end if

            enddo
        enddo

    end subroutine update_agent_list_from_matrix

end module mod_agent_matrix_merge
