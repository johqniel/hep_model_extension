subroutine check_agents_hashmap_consistency(agents_array, hash_map, num_agents_alive)
    implicit none
    type(Agent), pointer, intent(in) :: agents_array(:,:)
    type(t_int_map), pointer, intent(in) :: hash_map
    integer, intent(in) :: num_agents_alive(:)

    integer :: i, j
    integer :: index
    integer :: counter = 0
    integer :: counter_index_out_of_bounds = 0
    type(Agent), pointer :: agent_ptr


    do i = 1, size(num_agents_alive)
        if (num_agents_alive(i) > size(agents_array(:,i))) then
            print*, "Error: Num agents alive larger than array."
        endif
    enddo

    do j = 1, size(num_agents_alive)
        do i = 1, num_agents_alive(j)

            agent_ptr => agents_array(i,j)

            index = get(hash_map, agent_ptr%id)

            if (index > num_agents_alive(i) .or. index < 1) then
                print*, "Error: Index from hashmap is out of bound."
                print*, "Index: ", index, " num_agents_alive: ", num_agents_alive(i)
                counter_index_out_of_bounds = counter_index_out_of_bounds + 1
                cycle
            endif

            if (.not. agents_array(index,j)%id == agent_ptr%id) then
                print*, "ID by index: ", agents_array(index,j)%id, " ID: ", agent_ptr%id
                counter = counter + 1
            endif

        end do
    end do

    if (counter > 0 ) then
        print*, "Error: There are ", counter, " many agents that are not correctly mapped in the hashmap."
    endif

    if (counter_index_out_of_bounds > 0) then
        print*, "Error: There are ", counter_index_out_of_bounds, " many indexes in hashmap out of bound."
    endif

end subroutine check_agents_hashmap_consistency

subroutine check_agents_array_dead_alive_consistency(agents_arr,num_agents, num_dead_agents)
    implicit none
    type(Agent), intent(in), allocatable :: agents_arr(:,:)
    integer, intent(in) :: num_agents(:)
    integer, intent(in) :: num_dead_agents(:)

    integer :: i ,j
    integer :: count_dead_agents(size(num_dead_agents)), expect_dead_agents(size(num_dead_agents))
    count_dead_agents = 0

    do j = 1, size(num_dead_agents)
        do i = 1, size(agents_arr,1)

            if (agents_arr(i,j)%is_dead) then
                count_dead_agents(j) = count_dead_agents(j) + 1

            endif

        enddo
    enddo


    expect_dead_agents = size(agents_arr) - num_agents + num_dead_agents

    do i = 1, size(num_dead_agents)
        if (count_dead_agents(i) /= expect_dead_agents(i)) then
            print*, "Error: There are ", count_dead_agents, " Agents marked dead in array, but ther should be: ", expect_dead_agents
        endif
    enddo
end subroutine check_agents_array_dead_alive_consistency