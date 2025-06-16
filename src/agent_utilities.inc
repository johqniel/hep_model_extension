
    !===========================================================================
    ! FUNCTION: count_agents()
    ! Counts alive agents
    !
    ! ReturnS. 
    !       - count [integer], number of agents in list of agents
    !===========================================================================
    function count_agents() result(count)
        integer :: count
        type(Node), pointer :: current

        count = 0
        current => head_agents
        do while (associated(current))
          count = count + 1
          current => current%next
        end do
    end function count_agents


    !=======================================================================
    ! FUNCTION: move_agent
    ! Moves an agent to a new position.
    !
    ! Arguments:
    !   agent_ptr [TYPE(Node), POINTER] - Pointer to the agent to be moved
    !   new_x [REAL] - New x position of the agent
    !   new_y [REAL] - New y position of the agent
    !
    !=======================================================================
    subroutine move_agent(agent_ptr, new_x, new_y)
      type(Node), pointer :: agent_ptr
      real, intent(in) :: new_x, new_y

      if (.not. associated(agent_ptr)) return

      agent_ptr%pos_x = new_x
      agent_ptr%pos_y = new_y
    end subroutine move_agent

    !=======================================================================
    ! FUNCTION: agent_age
    ! Increments the age of an agent by 1.
    !
    ! Arguments:
    !   agent_ptr [TYPE(Node), POINTER] - Pointer to the agent whose age should be incremented
    !========================================================================
    subroutine agent_age(agent_ptr)
      type(Node), pointer :: agent_ptr

      if (.not. associated(agent_ptr)) return

      agent_ptr%age = agent_ptr%age + 1
    end subroutine agent_age


    !==============================================================================
    ! FUNCTION: get_agent_id()
    ! Returns a new agent_id when a new agent is born/spawned, etc. 
    !
    ! Returns: 
    ! id [integer] - A number that has not been assigned yet to any other agent
    !
    ! TODO: 
    !       - Ideally this id would be consistent with the id's assigned to agents 
    !       - in the hum_id matrix. Maybe this is already the case but it has to be checked
    !===============================================================================
    function get_agent_id() result(id)
      integer :: id
      id = number_of_agents_all_time + 1
      number_of_agents_all_time = number_of_agents_all_time + 1
    end function get_agent_id

    !===================================================================
    ! FUNCTION select_random_agents_distinct(agentOne, agentTwo)
    ! Randomly selects two alive agents
    ! 
    ! Arguments: 
    !     - agentOne [Type(Node)] refference to a variable of type Node
    !     - agentTwo [Type(Node)] refference to a variable of type Node
    !
    ! Returns: 
    !     - The function doesnt return the two selected agents. 
    !     - instead it implicitly returns them by overwriting 
    !       the two arguments it was given
    ! TODO: 
    !     - This is a little bit bad style or at least inconsistent 
    !       With the style in other parts of the program 
    !====================================================================
    subroutine select_random_agents(agent1, agent2)
      type(Node), pointer :: agent1, agent2
      integer :: idx1, idx2
      real r
      type(Node), pointer :: temp_agent
      integer :: count

      count = number_of_agents ! number of agents in the array


        ! Check if there are enough agents to select two
      if (count < 2) then
        print *, "Not enough agents to select from. (select function)"
        agent1 => null()
        agent2 => null()   
        return
      end if

      ! Select two random agents
      call random_seed()
      call random_number(r)     !    r in [0,1]   
      idx1 = int(r * count) + 1 ! because array in fortran are indexed [1,2,3,...n] we add 1


      !0.75 -> 0,    6.3 -> 6     we want even distribution on [1,2,3,... #numberOfagents]
                                 ! r is evenly distributed on 

      call random_seed()
      call random_number(r)
      idx2 = int(r * count) + 1

    

        ! Ensure the indices are within bounds
      if (idx1 > count .or. idx2 > count) then
        print *, "Random index out of bounds."
        agent1 => null()
        agent2 => null()   
        return
      end if


      agent1 => agents_array(idx1)%node
        if (.not. associated(agent1)) then
          print *, "Error: randomly selected agentOne from array is not associated!"
          print *, "idx1: ", idx1 , "max_agents: ", max_agents
          print *, "number_of_agents: ", number_of_agents
          return
        end if 
        
        agent2 => agents_array(idx2)%node

        if (.not. associated(agent2)) then
          print *, "Error: randomly selected agentTwo from array is not associated!"
          print *, "idx2: ", idx2 !, "max_agents: ", max_agents
          print *, "number_of_agents: ", number_of_agents
          return
        end if 

    end subroutine select_random_agents

    !===================================================================
    ! FUNCTION select_random_agents_distinct(agentOne, agentTwo)
    ! Randomly selects two alive agents that are not the same
    ! 
    ! Notes: The same as above but it makes sure that the two selected
    !        agents are not the same
    !====================================================================
    subroutine select_random_agents_distinct(agentOne, agentTwo)
      type(Node), pointer :: agentOne, agentTwo
      integer :: idx1, idx2
      real :: r
      type(Node), pointer :: temp_agent
      integer :: count

      count = number_of_agents ! number of agents in the array


        ! Check if there are enough agents to select two
      if (count < 2) then
        print *, "Not enough agents to select from. (select_distinct function)"
        agentOne => null()
        agentTwo => null()   
        return
      end if

      ! Select two random agents
      call random_seed()
      call random_number(r)
      idx1 = int(r * count) + 1
      call random_seed()
      call random_number(r)
      idx2 = int(r * count) + 1

    
        ! Ensure the two indices are different
      do while (idx1 == idx2)
        call random_seed()
        call random_number(r)
        idx2 = int(r * count) + 1

      end do

        ! Ensure the indices are within bounds
      if (idx1 > count .or. idx2 > count) then
        print *, "Random index out of bounds. (select_distinct function)"
        agentOne => null()
        agentTwo => null()   
        return
      end if


      agentOne => agents_array(idx1)%node
      if (.not. associated(agentOne)) then
        print *, "Error: randomly selected agentOne from array is not associated! (select_distinct function)"
        print *, "idx1: ", idx1 , "max_agents: ", max_agents
        print *, "number_of_agents: ", number_of_agents
        return
      end if 
      
      agentTwo => agents_array(idx2)%node
      if (.not. associated(agentTwo)) then
        print *, "Error: randomly selected agentTwo from array is not associated! (select_distinct function)"
        print *, "idx2: ", idx2 , "max_agents: ", max_agents
        print *, "number_of_agents: ", number_of_agents
        return
      end if 



    end subroutine select_random_agents_distinct
