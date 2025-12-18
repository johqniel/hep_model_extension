program test_compact_agents
    use mod_agent_world
    use mod_config
    use mod_test_utilities
    use mod_calculations
    use mod_grid_id
    implicit none

    type(world_container) :: world
    type(Agent) :: new_agent
    integer :: i, num_agents, num_killed, kill_count
    real :: r
    integer :: rand_idx
    integer :: initial_count, final_count, expected_count
    integer :: gx, gy

    print *, "========================================"
    print *, "Starting Test: Compact Agents"
    print *, "========================================"

    ! 1. Initialize World manually for 10x10 grid
    call world%init_world()
    
    ! Manually allocate agent arrays and counters
    allocate(world%num_humans(3))
    allocate(world%num_humans_marked_dead(3))
    world%num_humans = 0
    world%num_humans_marked_dead = 0
    
    allocate(world%agents(2000, 3)) ! Max 2000 agents, 3 pops
    
    ! Manually allocate and setup grid
    ! world%grid is not a pointer, so we don't allocate it.
    world%grid%nx = 10
    world%grid%ny = 10
    if (allocated(world%grid%cell)) deallocate(world%grid%cell)
    allocate(world%grid%cell(10, 10))
    
    do gx = 1, 10
        do gy = 1, 10
            world%grid%cell(gx, gy)%number_of_agents = 0
            if (allocated(world%grid%cell(gx, gy)%agents_ids)) deallocate(world%grid%cell(gx, gy)%agents_ids)
            allocate(world%grid%cell(gx, gy)%agents_ids(initial_array_size_for_agents_ids_in_gridcell))
            world%grid%cell(gx, gy)%agents_ids = -1
        end do
    end do
    
    ! Setup config for 10x10 grid
    world%config%lon_0 = 0.0d0
    world%config%lat_0 = 0.0d0
    world%config%delta_lon = 1.0d0
    world%config%delta_lat = 1.0d0
    world%config%dlon_hep = 10
    world%config%dlat_hep = 10
    world%config%npops = 3
    
    ! Initialize num_humans_marked_dead
    world%num_humans_marked_dead = 0

    print *, "Grid Size: ", world%grid%nx, " x ", world%grid%ny

    ! 2. Spawn 1000 Agents
    num_agents = 1000
    print *, "Spawning ", num_agents, " agents..."
    
    call random_seed() 

    do i = 1, num_agents
        new_agent = world%spawn_agent_hash(1)
        
        ! Random position in 10x10 grid (0..10)
        call random_number(r)
        new_agent%pos_x = r * 10.0d0
        call random_number(r)
        new_agent%pos_y = r * 10.0d0
        
        ! Ensure strictly within bounds (avoid 10.0 which might map to 11)
        if (new_agent%pos_x >= 10.0d0) new_agent%pos_x = 9.99d0
        if (new_agent%pos_y >= 10.0d0) new_agent%pos_y = 9.99d0
        
        call add_agent_to_array_hash(world, new_agent, 1)
    end do
    print *, "Agents spawned."

    ! Verify initial state
    call verify_agent_array_integrity(world)
    call verify_index_map_integrity(world)

    call verify_grid_integrity(world)
    
    initial_count = world%num_humans(1)
    if (initial_count /= num_agents) then
        print *, RED, "Error: Initial count mismatch. Expected ", num_agents, " got ", initial_count, RESET
        stop 1
    end if

    ! 3. Kill 100-500 random agents
    call random_number(r)
    num_killed = 100 + int(r * 401) ! 100 to 500
    print *, "Killing ", num_killed, " random agents..."

    kill_count = 0
    do while (kill_count < num_killed)
        call random_number(r)
        rand_idx = 1 + int(r * world%num_humans(1))
        
        ! Ensure index is valid and agent is alive
        if (rand_idx >= 1 .and. rand_idx <= world%num_humans(1)) then
            if (.not. world%agents(rand_idx, 1)%is_dead) then
                call world%agents(rand_idx, 1)%agent_dies()
                kill_count = kill_count + 1
            end if
        end if
    end do
    print *, "Killed ", kill_count, " agents."

    ! 4. Compact Agents
    print *, "Compacting agents..."
    call compact_agents(world)
    print *, "Compaction complete."

    ! 5. Verify Final State
    print *, "Verifying final state..."

    call verify_agent_array_integrity(world)


    call verify_index_map_integrity(world)

    call verify_grid_integrity(world)

    final_count = world%num_humans(1)
    expected_count = num_agents - num_killed
    
    print *, "Initial: ", num_agents
    print *, "Killed:  ", num_killed
    print *, "Final:   ", final_count
    print *, "Expected:", expected_count

    if (final_count /= expected_count) then
        print *, RED, "Error: Final count mismatch.", RESET
        stop "Test Failed: Count mismatch"
    end if

    print *, "========================================"
    print *, GREEN, "SUCCESS: Compact Agents Test Passed.", RESET
    print *, "========================================"

end program test_compact_agents
