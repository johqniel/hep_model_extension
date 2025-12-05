program test_update_hep
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

    print *, "Starting test_update_hep..."

    ! 1. Initialize Globals
    allocate(lon_hep(10))
    allocate(lat_hep(10))
    do i = 1, 10
        lon_hep(i) = real(i, 8)
        lat_hep(i) = real(i, 8)
    end do
    
    ! Initialize hep and hep_av
    allocate(hep(10,10,1,1))
    allocate(hep_av(10,10,1))
    hep = 1.0d0
    hep_av = 0.0d0
    t_hep = 1
    
    ! Initialize delta_lat for pop_dens_adj calculation
    ! delta_lat = lat_hep(2) - lat_hep(1) = 1.0
    ! deg_km = C_earth/360.0 (defined in constants.inc)
    
    ! 2. Initialize World
    my_world%config%npops = 1
    my_world%config%initial_max_pop_size = 100
    my_world%config%initial_hashmap_size = 100
    
    ! Setup config parameters for update_hep_density
    allocate(my_world%config%sigma_u(1))
    my_world%config%sigma_u(1) = 10.0d0 ! Some value
    
    allocate(my_world%config%rho_max(1))
    my_world%config%rho_max(1) = 10.0d0
    
    allocate(my_world%config%eta(1))
    my_world%config%eta(1) = 2.0d0
    
    allocate(my_world%config%epsilon(1))
    my_world%config%epsilon(1) = 1.0d0
    
    my_world%config%with_pop_pressure = .true.
    
    my_world%config%with_pop_pressure = .true.
    
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
    
    ! Initialize HEP in Grid
    my_world%grid%hep = 1.0d0
    my_world%grid%hep_av = 0.0d0
    t_hep = 1
    
    ! 3. Place Agents
    ! Agent 1
    new_agent%id = 1
    new_agent%population = 1
    new_agent%pos_x = 5.0d0
    new_agent%pos_y = 5.0d0
    new_agent%ux = 10.0d0
    new_agent%uy = 0.0d0
    new_agent%is_dead = .false.
    
    call add_agent_to_array_hash(my_world%agents, my_world%index_map, new_agent, my_world%num_humans, 1, agent_ptr)
    agent_ptr%grid => my_world%grid
    call my_world%grid%place_agent_in_cell(1, 5, 5)
    
    ! 4. Test update_hep_density
    print *, "Testing update_hep_density..."
    call my_world%update_hep_density(1)

    ! Check Cell (5,5)
    print *, "Cell (5,5):"
    print *, "Density: ", my_world%grid%cell(5,5)%human_density, " Expected: 2.0"
    print *, "Smoothed Density: ", my_world%grid%cell(5,5)%human_density_smoothed
    print *, "Pop Pressure: ", my_world%grid%cell(5,5)%pop_pressure
    print *, "HEP AV: ", my_world%grid%hep_av(5,5,1)
    
    if (my_world%grid%cell(5,5)%pop_pressure > 0.0d0) then
        print *, "Pressure Check PASS"
    else
        print *, "Pressure Check FAIL"
    end if
    
    if (my_world%grid%hep_av(5,5,1) > 0.0d0) then
        print *, "HEP AV Check PASS"
    else
        print *, "HEP AV Check FAIL"
    end if

    ! 5. Verify Config Pointer
    print *, "Testing Grid Config Pointer..."
    if (associated(my_world%grid%config)) then
        print *, "Grid Config Pointer Associated: YES"
        if (my_world%grid%config%npops == 1) then
            print *, "Grid Config npops Check: PASS"
        else
            print *, "Grid Config npops Check: FAIL (Expected 1, got ", my_world%grid%config%npops, ")"
        end if
    else
        print *, "Grid Config Pointer Associated: NO"
    end if

    print *, "Test Complete."

end program test_update_hep
