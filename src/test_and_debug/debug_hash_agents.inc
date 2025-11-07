subroutine check_agents_hashmap_consistency(agents_array, hash_map, num_agents_alive)
    implicit none
    type(Agent), pointer, intent(in) :: agents_array(:)
    type(t_int_map), pointer, intent(in) :: hash_map
    integer, intent(in) :: num_agents_alive

    integer :: i
    integer :: index
    integer :: counter = 0
    type(Agent), pointer :: agent_ptr

    do i = 1, num_agents_alive

        agent_ptr => agents_array(i)

        index = get(hash_map, agent_ptr%id)

        if (.not. index == agent_ptr%id) then
            counter = counter + 1
        endif

    end do

    if (counter > 0 ) then
        print*, "Error: There are ", counter, " many agents that are not correctly mapped in the hashmap."
    endif

end subroutine check_agents_hashmap_consistency