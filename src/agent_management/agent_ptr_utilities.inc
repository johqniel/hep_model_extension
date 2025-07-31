  logical function is_agent_in_ptr_list(agent_to_search,ptr_list_head)
    type(Node), pointer, intent(in) :: agent_to_search
    type(pointer_node), pointer, intent(in) :: ptr_list_head

    type(pointer_node), pointer :: current
    integer :: counter 

    counter = 0
    is_agent_in_ptr_list = .false.
    current => ptr_list_head

    if (.not. associated(agent_to_search)) then
        print*, "Error: Searching for agent that is not associated." 
        
    endif
    do while (associated(current))
        if (.not. associated(current%node)) then
            print*, "Pointer Node associated but %node is not associated. ?"
        endif
        if (associated(current%node,agent_to_search)) then
            is_agent_in_ptr_list = .true.
            counter = counter + 1
        endif 
        if (associated(current%next)) then
            current => current%next
        else 
            current => null()
            print*, "Error: pointer_node is associated but %node is not. ?"
        endif
    end do


    if (counter > 1) then
        print*, "Error: an agent occours more than once in the list of agents that are in a gridcell. "
    endif

end function is_agent_in_ptr_list