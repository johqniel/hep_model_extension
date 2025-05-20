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
    implicit none

contains


    subroutine select_random_agents_distinct_from_population(hum_id_mirror_array, population, & 
                                                             size_of_population, parent_one, parent_two) 
        integer, intent(in) :: population
        integer, intent(in) :: size_of_population
        type(Node), pointer, intent(out) :: parent_one, parent_two
        type(pointer_node), allocatable, intent(in) :: hum_id_mirror_array(:,:)  

        type(Node), pointer :: agentOne, agentTwo
        integer :: idx1, idx2
        real :: r
        type(Node), pointer :: temp_agent
        parent_one => null()
        parent_two => null()


        ! Check if there are enough agents to select two
        if (size_of_population < 2) then
            print *, "Not enough agents to select from. (select_distinct function)"
            agentOne => null()
            agentTwo => null()   
            return
        end if

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
            return
        end if


        agentOne => hum_id_mirror_array(idx1,population)%node
        if (.not. associated(agentOne)) then
            print *, "Error: randomly selected agentOne from array is not associated! " // & 
                     "(select_distinct function agent_matrix_merge)"
            print *, "idx1: ", idx1 , "max_agents: ", max_agents
            print *, "number_of_agents: ", number_of_agents
            return
        end if 
        
        agentTwo => hum_id_mirror_array(idx2,population)%node
        if (.not. associated(agentTwo)) then
            print *, "Error: randomly selected agentTwo from array is not associated! " // &
                     "(select_distinct function) agent_matrix_merge)"
            print *, "idx2: ", idx2 , "max_agents: ", max_agents
            print *, "number_of_agents: ", number_of_agents
            return
        end if 

        parent_one => agentOne
        parent_two => agentTwo

    end subroutine select_random_agents_distinct_from_population

end module mod_agent_matrix_merge
