program test_grid_funcs
    use mod_grid_id
    use mod_globals
    use mod_agent_world
    use mod_config
    implicit none

    type(world_container), target :: my_world
    integer :: i, j, k, id
    real(8) :: p, q
    real(8), allocatable :: test_hep(:,:)
    real(8) :: test_N_max, test_eta, test_epsilon
    type(Agent) :: new_agent
    type(Agent), pointer :: agent_ptr

    print *, "Starting test_grid_funcs..."

    ! 1. Initialize Globals
    allocate(lon_hep(10))
    allocate(lat_hep(10))
    do i = 1, 10
        lon_hep(i) = real(i, 8)
        lat_hep(i) = real(i, 8)
    end do

    ! 2. Initialize World
    ! We can use setup_world but it reads config. 
    ! Let's manually setup what we can, but we need to respect private access.
    ! setup_world is public. init_world is public.
    ! We can try to mock read_world_config or just set config manually and call setup_world?
    ! setup_world calls setup_world_config (private) then allocate_agents (private).
    ! If we set config manually, we might be able to use setup_world.
    
    my_world%config%npops = 1
    my_world%config%initial_max_pop_size = 100
    my_world%config%initial_hashmap_size = 100
    
    ! Initialize Grid dimensions BEFORE setup_world calls allocate_grid
    my_world%grid%nx = 10
    my_world%grid%ny = 10
    
    call my_world%setup_world()
    
    ! Grid is now allocated by setup_world -> setup_grid -> allocate_grid
    
    ! Initialize cells
    do i = 1, my_world%grid%nx
        do j = 1, my_world%grid%ny
            call my_world%grid%initialize_cell(i,j)
            my_world%grid%cell(i,j)%area = 100.0d0
        end do
    end do
    
    ! 3. Place Agents
    ! add_agent_to_array_hash is a module subroutine, not type bound.
    ! We need to call it as a normal subroutine.
    
    ! Agent 1
    new_agent%id = 1
    new_agent%population = 1
    new_agent%pos_x = 5.0d0
    new_agent%pos_y = 5.0d0
    new_agent%ux = 10.0d0
    new_agent%uy = 0.0d0
    new_agent%is_dead = .false.
    
    call add_agent_to_array_hash(my_world%agents, my_world%index_map, new_agent, my_world%num_humans, 1, agent_ptr)
    ! Link agent to grid
    agent_ptr%grid => my_world%grid
    call my_world%grid%place_agent_in_cell(1, 5, 5)
    
    ! Agent 2
    new_agent%id = 2
    new_agent%population = 1
    new_agent%pos_x = 5.0d0
    new_agent%pos_y = 5.0d0
    new_agent%ux = 0.0d0
    new_agent%uy = 10.0d0
    new_agent%is_dead = .false.
    
    call add_agent_to_array_hash(my_world%agents, my_world%index_map, new_agent, my_world%num_humans, 1, agent_ptr)
    agent_ptr%grid => my_world%grid
    call my_world%grid%place_agent_in_cell(2, 5, 5)

    ! 4. Test pop_dens_flow_func
    print *, "Testing pop_dens_flow_func (World)..."
    call my_world%pop_dens_flow_func(1, 0)

    ! Check Cell (5,5)
    print *, "Cell (5,5):"
    print *, "Density: ", my_world%grid%cell(5,5)%human_density, " Expected: 2.0"
    print *, "Flow X: ", my_world%grid%cell(5,5)%flow_x, " Expected: 10.0"
    print *, "Flow Y: ", my_world%grid%cell(5,5)%flow_y, " Expected: 10.0"

    if (abs(my_world%grid%cell(5,5)%human_density - 2.0d0) < 1e-6) then
        print *, "Density Check PASS"
    else
        print *, "Density Check FAIL"
    end if

    if (abs(my_world%grid%cell(5,5)%flow_x - 10.0d0) < 1e-6) then
        print *, "Flow X Check PASS"
    else
        print *, "Flow X Check FAIL"
    end if

    ! 5. Test smooth2d (still in Grid)
    print *, "Testing smooth2d..."
    p = 0.5d0
    q = 0.0d0
    call my_world%grid%smooth2d(p, q)
    
    print *, "Smoothed Density at (5,5): ", my_world%grid%cell(5,5)%human_density_smoothed
    print *, "Smoothed Density at (4,5): ", my_world%grid%cell(4,5)%human_density_smoothed
    
    if (my_world%grid%cell(4,5)%human_density_smoothed > 0.0d0) then
        print *, "Smoothing Check PASS"
    else
        print *, "Smoothing Check FAIL"
    end if

    ! 6. Test pop_pressure_func (still in Grid)
    print *, "Testing pop_pressure_func..."
    allocate(test_hep(10,10))
    test_hep = 1.0d0
    test_N_max = 10.0d0
    test_eta = 2.0d0
    test_epsilon = 1.0d0
    
    call my_world%grid%pop_pressure_func(test_hep, test_N_max, test_eta, test_epsilon)
    
    print *, "Pop Pressure at (5,5): ", my_world%grid%cell(5,5)%pop_pressure
    
    if (my_world%grid%cell(5,5)%pop_pressure > 0.0d0) then
        print *, "Pressure Check PASS"
    else
        print *, "Pressure Check FAIL"
    end if

    print *, "Test Complete."

end program test_grid_funcs
