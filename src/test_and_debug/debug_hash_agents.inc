subroutine check_agents_hashmap_consistency(agents_array, hash_map, num_agents_alive)
    implicit none
    type(Agent), pointer, intent(in) :: agents_array(:,:)
    type(t_int_map), pointer, intent(in) :: hash_map
    integer, intent(in) :: num_agents_alive(:)

    integer :: i, j
    integer :: index, population
    integer :: counter = 0
    integer :: counter_index_out_of_bounds = 0
    integer :: counter_population_wrong = 0
    type(Agent), pointer :: agent_ptr


    do i = 1, size(num_agents_alive)
        if (num_agents_alive(i) > size(agents_array(:,i))) then
            print*, "Error: Num agents alive larger than array."
        endif
    enddo

    do j = 1, size(num_agents_alive)
        do i = 1, num_agents_alive(j)

            agent_ptr => agents_array(i,j)
            population = agent_ptr%population

            index = get(hash_map, agent_ptr%id)

            if (index > num_agents_alive(j) .or. index < 1) then
                print*, "Error: Index from hashmap is out of bound."
                print*, "Index: ", index, " num_agents_alive: ", num_agents_alive(i)
                counter_index_out_of_bounds = counter_index_out_of_bounds + 1
                cycle
            endif

            if (.not. population == j) then
                print*, "Error: population not valid for agent: %population == ", population
                counter_population_wrong = counter_population_wrong + 1
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

    if (counter_population_wrong > 0) then
        print*, "Error: There are ", counter_population_wrong, " agents with wrong %population."
    endif

end subroutine check_agents_hashmap_consistency


