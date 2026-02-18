program test_agent_movement
    use mod_agent_world
    use mod_grid_id
    use mod_config
    use mod_constants
    use mod_test_utilities
    implicit none

    type(world_container), target :: world
    integer :: i, iter
    integer :: num_agents = 100
    integer :: num_iterations = 50
    integer :: gx_new, gy_new, gx_old, gy_old
    real(8) :: r_x, r_y
    type(Agent), pointer :: agent_ptr
    logical :: check_passed
    integer :: count_in_cell
    integer :: k

    type(Agent) :: new_agent

    print *, "========================================"
    print *, "Starting Test: Agent Movement"
    print *, "========================================"

    ! 1. Initialize World
    call world%init_world()
    call world%setup_world()
    
    ! Manually set grid size if not set by setup_world (depending on config)
    ! Assuming setup_world does it based on default config or we might need to load one.
    ! Let's ensure we have a grid.
    if (world%grid%nx == 0) then
        world%grid%nx = 100
        world%grid%ny = 100
        call world%grid%allocate_grid(1, 1)
    endif

    print *, "Grid Size: ", world%grid%nx, "x", world%grid%ny

    ! 2. Spawn Agents
    print *, "Spawning ", num_agents, " agents..."
    do i = 1, num_agents
        ! Spawn in population 1
        new_agent = world%spawn_agent_hash(1)
        
        ! Place randomly
        call random_number(r_x)
        call random_number(r_y)
        gx_new = int(r_x * world%grid%nx) + 1
        gy_new = int(r_y * world%grid%ny) + 1
        
        ! Ensure bounds
        if (gx_new > world%grid%nx) gx_new = world%grid%nx
        if (gy_new > world%grid%ny) gy_new = world%grid%ny
        if (gx_new < 1) gx_new = 1
        if (gy_new < 1) gy_new = 1
        
        new_agent%gx = gx_new
        new_agent%gy = gy_new
        ! Calculate pos_x and pos_y to be in the center of the cell
        new_agent%pos_x = world%config%lon_0 + (real(gx_new, 8) - 0.5d0) * world%config%delta_lon
        new_agent%pos_y = world%config%lat_0 + (real(gy_new, 8) - 0.5d0) * world%config%delta_lat

        if (associated(new_agent%grid)) then
            print*, "Warning: Spawned agent is already in a grid."
        endif
        ! Add to array
        call add_agent_to_array_hash(world, new_agent, 1)
        

    end do
    print *, "Agents spawned."

    ! 3. Movement Loop
    print *, "Starting movement loop (", num_iterations, " iterations)..."
    
    do iter = 1, num_iterations
        do i = 1, world%num_humans(1)
            agent_ptr => world%agents(i, 1)
            
            if (agent_ptr%is_dead) cycle
            
            gx_old = agent_ptr%gx
            gy_old = agent_ptr%gy
            
            ! Pick random new cell
            call random_number(r_x)
            call random_number(r_y)
            gx_new = int(r_x * world%grid%nx) + 1
            gy_new = int(r_y * world%grid%ny) + 1
             
            ! Ensure bounds
            if (gx_new > world%grid%nx) gx_new = world%grid%nx
            if (gy_new > world%grid%ny) gy_new = world%grid%ny
            if (gx_new < 1) gx_new = 1
            if (gy_new < 1) gy_new = 1
            
            ! Move
            call world%grid%move_agent_to_cell(agent_ptr%id, gx_old, gy_old, gx_new, gy_new)
            
            ! Update agent state
            agent_ptr%gx = gx_new
            agent_ptr%gy = gy_new
            
            ! Update pos_x/pos_y to match cell (Mocking coordinates)
            ! Assuming simple mapping for verification: pos_x = gx, pos_y = gy
            agent_ptr%pos_x = world%config%lon_0 + (real(gx_new, 8) - 0.5d0) * world%config%delta_lon
            agent_ptr%pos_y = world%config%lat_0 + (real(gy_new, 8) - 0.5d0) * world%config%delta_lat
            
        end do
    end do
    print *, "Movement loop complete."

    ! 4. Verification
    print *, "Verifying consistency..."
    
    call verify_agent_array_integrity(world)
    call verify_index_map_integrity(world)

    call verify_grid_integrity(world)

    print *, GREEN, "SUCCESS: All checks passed.", RESET
    
    print *, "========================================"

end program test_agent_movement
