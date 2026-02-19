program test_agent_update_pos
    use mod_agent_world
    use mod_grid_id
    use mod_config
    use mod_constants
    use mod_calculations
    use mod_test_utilities
    implicit none

    type(world_container), target :: world
    integer :: i, iter
    integer :: num_agents = 100
    integer :: num_iterations = 50
    real(8) :: r_x, r_y
    real(8) :: new_pos_x, new_pos_y
    real(8) :: lon_min, lon_max, lat_min, lat_max
    type(Agent), pointer :: agent_ptr
    type(Agent) :: new_agent
    logical :: check_passed
    integer :: count_in_cell
    integer :: k, gx, gy
    integer :: gx_calc, gy_calc

    print *, "========================================"
    print *, "Starting Test: Agent Update Pos"
    print *, "========================================"

    ! 1. Initialize World
    call world%init_world()
    call world%setup_world()
    
    ! Manually set grid size if not set by setup_world
    if (world%grid%nx == 0) then
        world%grid%nx = 100
        world%grid%ny = 100
        call world%grid%allocate_grid(1, 1)
    endif

    print *, "Grid Size: ", world%grid%nx, "x", world%grid%ny

    ! Define Domain Bounds
    lon_min = world%config%lon_0
    lon_max = world%config%lon_0 + real(world%grid%nx, 8) * world%config%delta_lon
    lat_min = world%config%lat_0
    lat_max = world%config%lat_0 + real(world%grid%ny, 8) * world%config%delta_lat
    
    print *, "Domain: Lon [", lon_min, ",", lon_max, "] Lat [", lat_min, ",", lat_max, "]"

    ! 2. Spawn Agents
    print *, "Spawning ", num_agents, " agents..."
    do i = 1, num_agents
        ! Spawn in population 1
        new_agent = world%spawn_agent_hash(1)
        
        ! Place randomly in domain
        call random_number(r_x)
        call random_number(r_y)
        
        new_agent%pos_x = lon_min + r_x * (lon_max - lon_min)
        new_agent%pos_y = lat_min + r_y * (lat_max - lat_min)
        
        ! Calculate initial grid position
        call calculate_grid_pos(new_agent%pos_x, new_agent%pos_y, gx, gy, world%config)
        new_agent%gx = gx
        new_agent%gy = gy

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
            
            ! Generate random new position in domain
            call random_number(r_x)
            call random_number(r_y)
            
            new_pos_x = lon_min + r_x * (lon_max - lon_min)
            new_pos_y = lat_min + r_y * (lat_max - lat_min)
            
            ! Ensure slightly inside bounds to avoid edge cases with float precision if needed
            ! But calculate_grid_pos should handle it.
            
            ! Call update_pos
            call agent_ptr%update_pos(new_pos_x, new_pos_y)
            
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

end program test_agent_update_pos
