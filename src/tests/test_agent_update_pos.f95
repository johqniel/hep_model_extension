program test_agent_update_pos
    use mod_agent_world
    use mod_grid_id
    use mod_config
    use mod_constants
    use mod_calculations
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
    check_passed = .true.

    ! Check 1: Agent positions
    do i = 1, world%num_humans(1)
        agent_ptr => world%agents(i, 1)
        if (agent_ptr%is_dead) cycle
        
        ! Calculate expected grid position
        call calculate_grid_pos(agent_ptr%pos_x, agent_ptr%pos_y, gx_calc, gy_calc, world%config)
        
        ! Check if agent's gx, gy match calculated
        if (agent_ptr%gx /= gx_calc .or. agent_ptr%gy /= gy_calc) then
            print *, "FAIL: Agent ", agent_ptr%id, " grid indices mismatch. Agent: (", &
                     agent_ptr%gx, ",", agent_ptr%gy, ") Calc: (", gx_calc, ",", gy_calc, ")"
            check_passed = .false.
        endif

        ! Check if grid thinks agent is in (gx, gy)
        if (.not. world%grid%is_agent_in_cell(agent_ptr%id, agent_ptr%gx, agent_ptr%gy)) then
            print *, "FAIL: Agent ", agent_ptr%id, " thinks it is in (", &
                     agent_ptr%gx, ",", agent_ptr%gy, ") but grid does not have it there."
            check_passed = .false.
        endif
    end do

    ! Check 2: Grid consistency
    do i = 1, world%grid%nx
        do k = 1, world%grid%ny
            ! Count actual agents in this cell
            count_in_cell = world%grid%count_agents_in_cell(i, k)
            
            if (count_in_cell /= world%grid%cell(i,k)%number_of_agents) then
                print *, "FAIL: Cell (", i, ",", k, ") count mismatch. Variable: ", &
                         world%grid%cell(i,k)%number_of_agents, " Actual counted: ", count_in_cell
                check_passed = .false.
            endif
            
            ! Check uninitialized slots
            if (allocated(world%grid%cell(i,k)%agents_ids)) then
                if (size(world%grid%cell(i,k)%agents_ids) > count_in_cell) then
                    if (any(world%grid%cell(i,k)%agents_ids(count_in_cell+1:) /= -1)) then
                         print *, "FAIL: Cell (", i, ",", k, ") has garbage in unused slots."
                         check_passed = .false.
                    endif
                endif
            endif
        end do
    end do

    if (check_passed) then
        print *, "SUCCESS: All checks passed."
    else
        print *, "FAILURE: Some checks failed."
    endif
    
    print *, "========================================"

end program test_agent_update_pos
