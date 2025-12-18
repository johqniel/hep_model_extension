module mod_test_utilities
    use mod_agent_world
    use mod_grid_id
    use mod_calculations
    use mod_hashmap
    implicit none

    private

    public :: verify_agent_array_integrity
    public :: verify_grid_integrity
    public :: verify_index_map_integrity
    public :: RED, GREEN, RESET

    character(len=*), parameter :: RED = char(27) // '[31m'
    character(len=*), parameter :: GREEN = char(27) // '[32m'
    character(len=*), parameter :: RESET = char(27) // '[0m'

contains

    subroutine verify_agent_array_integrity(world)
        class(world_container), intent(in) :: world
        integer :: pop, i, num_alive, array_size
        logical :: found_hole

        do pop = 1, world%config%npops
            num_alive = world%num_humans(pop)
            array_size = size(world%agents, 1)
            found_hole = .false.

            ! Check alive agents section
            do i = 1, num_alive
                if (world%agents(i, pop)%is_dead) then
                    print *, RED, "Error: Found dead agent in alive section at index ", &
                             i, " for population ", pop, RESET
                    stop "Test Failed: Hole in agent array (dead agent in alive section)"
                end if
                if (world%agents(i, pop)%id == -1) then
                    print *, RED, "Error: Found invalid agent ID (-1) in alive section at index ", &
                             i, " for population ", pop, RESET
                    stop "Test Failed: Hole in agent array (invalid ID in alive section)"
                end if

                ! Check if agent is in grid
                if (.not. world%grid%is_agent_in_cell(world%agents(i, pop)%id, &
                                                      world%agents(i, pop)%gx, &
                                                      world%agents(i, pop)%gy)) then
                    print *, RED, "Error: Agent ", world%agents(i, pop)%id, " (Pop ", pop, ", Index ", i, &
                             ") thinks it is in (", world%agents(i, pop)%gx, ",", world%agents(i, pop)%gy, &
                             ") but grid does not have it there.", RESET
                    stop "Test Failed: Agent not found in grid cell it claims to be in"
                end if
            end do

            ! Check dead/unused section
            do i = num_alive + 1, array_size
                if (.not. world%agents(i, pop)%is_dead .and. world%agents(i, pop)%id /= -1) then
                    print *, RED, "Error: Found alive agent in dead section at index ", &
                             i, " for population ", pop, RESET
                    stop "Test Failed: Alive agent found in dead section of array"
                end if
            end do
            
            print *, GREEN, "Agent array integrity verified for population ", pop, RESET
        end do

    end subroutine verify_agent_array_integrity

    subroutine verify_grid_integrity(world)
        class(world_container), target, intent(inout) :: world
        integer :: gx, gy, i, agent_id, count, expected_gx, expected_gy
        type(Agent), pointer :: agent_ptr
        type(Grid), pointer :: grid

        integer :: num_agents_error_counter
        integer :: tail_consistency_error_counter
        integer :: agent_position_error_counter
        integer :: agent_associated_error_counter
        integer :: sum_agents_in_gridcells
        integer :: sum_counts_in_gridcells
        integer :: num_agents_in_array

        grid => world%grid

        sum_agents_in_gridcells = 0
        sum_counts_in_gridcells = 0
        num_agents_error_counter = 0
        tail_consistency_error_counter = 0
        agent_position_error_counter = 0
        agent_associated_error_counter = 0

        num_agents_in_array = sum(world%num_humans(:))

        do gx = 1, grid%nx
            do gy = 1, grid%ny

                ! keep track of total number of agents expected in gridcells.
                sum_agents_in_gridcells = sum_agents_in_gridcells + grid%cell(gx, gy)%number_of_agents

                ! 1. Check number of agents consistency
                count = 0
                do i = 1, size(grid%cell(gx, gy)%agents_ids)
                    if (grid%cell(gx, gy)%agents_ids(i) /= -1) then
                        count = count + 1
                    end if
                end do


                ! keep track of total number of agents found in gridcells.
                sum_counts_in_gridcells = sum_counts_in_gridcells + count

                if (count /= grid%cell(gx, gy)%number_of_agents) then
                        num_agents_error_counter = num_agents_error_counter + 1
                end if

                ! 2. Check tail consistency
                do i = grid%cell(gx, gy)%number_of_agents + 1, size(grid%cell(gx, gy)%agents_ids)
                    if (grid%cell(gx, gy)%agents_ids(i) /= -1) then
                        tail_consistency_error_counter = tail_consistency_error_counter + 1

                        !print *, RED, "Error: Grid cell (", gx, ",", gy, ") has valid ID in tail at index ", &
                        !         i, RESET
                        !stop "Test Failed: Grid cell array tail contains valid ID"
                    end if
                end do

                ! 3. Verify agent positions
                do i = 1, grid%cell(gx, gy)%number_of_agents
                    agent_id = grid%cell(gx, gy)%agents_ids(i)
                    
                    ! Get agent pointer
                    agent_ptr => get_agent(agent_id, world%index_map, world%agents)
                    
                    if (.not. associated(agent_ptr)) then
                        agent_associated_error_counter = agent_associated_error_counter + 1
                        !print *, RED, "Error: Agent ID ", agent_id, " in grid cell (", gx, ",", gy, &
                        !         ") not found in world agents.", RESET
                        !stop "Test Failed: Agent in grid not found in world"
                        cycle
                    end if

                    ! Calculate expected grid position
                    call calculate_grid_pos(agent_ptr%pos_x, agent_ptr%pos_y, expected_gx, expected_gy, world%config)

                    if (expected_gx /= gx .or. expected_gy /= gy) then
                        agent_position_error_counter = agent_position_error_counter + 1
                        !print *, RED, "Error: Agent ", agent_id, " in wrong grid cell.", RESET
                        !print *, RED, "  Current Cell: (", gx, ",", gy, ")", RESET
                        !print *, RED, "  Calculated Cell: (", expected_gx, ",", expected_gy, ")", RESET
                        !print *, RED, "  Position: (", agent_ptr%pos_x, ",", agent_ptr%pos_y, ")", RESET
                        !stop "Test Failed: Agent in wrong grid cell"
                    end if
                end do

            end do
        end do


        if (tail_consistency_error_counter > 0) then
            print *, RED, "Error: Grid cell tail consistency mismatch.", RESET
            print *, RED, "  Number of errors: ", tail_consistency_error_counter, RESET
            !stop "Test Failed: Grid cell tail consistency mismatch"
        end if

        if (num_agents_error_counter > 0) then
            print *, RED, "Error: Grid cell agent count mismatch.", RESET
            print *, RED, "  Number of cells with errors: ", num_agents_error_counter, RESET
            print*, RED, " Total count: ", sum_counts_in_gridcells, RESET
            print*, RED, " Expected count: ", sum_agents_in_gridcells, RESET
            !stop "Test Failed: Grid cell agent count mismatch"
        end if

        if ((sum_agents_in_gridcells /= num_agents_in_array) .or. &
            (sum_counts_in_gridcells /= num_agents_in_array)) then

            print *, RED, "Error: sum of agents in gridcells /= num agents in array.", RESET
            print *, RED, " Num Agents in array: ", num_agents_in_array, RESET
            print *, RED, " Sum Agents in gridcells: ", sum_agents_in_gridcells, RESET
            print *, RED, " Sum Counts in gridcells: ", sum_counts_in_gridcells, RESET
            !stop "Test Failed: sum of agents in gridcells /= num agents in array"
        end if

        if (agent_position_error_counter > 0) then
            print *, RED, "Error: Agent position mismatch.", RESET
            print *, RED, "  Number of errors: ", agent_position_error_counter, RESET
            !stop "Test Failed: Agent position mismatch"
        end if

        if (agent_associated_error_counter > 0) then
            print *, RED, "Error: Agent associated mismatch.", RESET
            print *, RED, "  Number of errors: ", agent_associated_error_counter, RESET
            !stop "Test Failed: Agent associated mismatch"
        end if

        if (agent_position_error_counter > 0 .or. &
            agent_associated_error_counter > 0 .or. &
            num_agents_error_counter > 0 .or. &
            tail_consistency_error_counter > 0) then
            print *, RED, "Error: Grid integrity verification failed.", RESET
            stop "Test Failed: Grid integrity verification failed"
        end if

        print *, GREEN, "Grid integrity verified.", RESET

    end subroutine verify_grid_integrity

    subroutine verify_index_map_integrity(world)
        class(world_container), intent(in), target :: world
        integer :: i, pop
        integer :: error_count
        type(Agent), pointer :: agent_ptr
        integer :: retrieved_index, retrieved_pop

        error_count = 0

        do pop = 1, world%config%npops
            do i = 1, world%num_humans(pop)
                agent_ptr => world%agents(i, pop)

                if (agent_ptr%is_dead) cycle

                call get_index_and_pop(world%index_map, agent_ptr%id, retrieved_index, retrieved_pop)

                if (retrieved_index == -1 .or. retrieved_pop == -1) then
                    error_count = error_count + 1
                    print *, RED, "Error: Agent ID ", agent_ptr%id, " (Pop ", pop, ", Index ", i, &
                             ") not found in index_map.", RESET
                else if (retrieved_index /= i .or. retrieved_pop /= pop) then
                    error_count = error_count + 1
                    print *, RED, "Error: Agent ID ", agent_ptr%id, " map mismatch.", RESET
                    print *, RED, "  Expected: (Pop ", pop, ", Index ", i, ")", RESET
                    print *, RED, "  Found:    (Pop ", retrieved_pop, ", Index ", retrieved_index, ")", RESET
                end if
            end do
        end do

        if (error_count > 0) then
            print *, RED, "Error: Index map integrity check failed.", RESET
            print *, RED, "  Total errors: ", error_count, RESET
            stop "Test Failed: Index map integrity check failed"
        else
            print *, GREEN, "Index map integrity verified.", RESET
        end if

    end subroutine verify_index_map_integrity

end module mod_test_utilities
