program test_config_read
    use mod_agent_world
    use mod_config
    use mod_basic_config
    use mod_test_utilities
    implicit none

    type(world_container) :: world
    integer :: i

    print *, "========================================"
    print *, "Starting Test: Configuration Reading"
    print *, "========================================"

    ! Initialize and setup world
    call world%init_world()
    call world%setup_world()

    print *, "World Setup Complete."
    print *, "----------------------------------------"
    print *, "Configuration Variables:"
    print *, "----------------------------------------"
    
    print *, "npops: ", world%config%npops
    print *, "Tn: ", world%config%Tn
    print *, "dt: ", world%config%dt
    print *, "delta_t_hep: ", world%config%delta_t_hep
    print *, "delta_lat: ", world%config%delta_lat
    print *, "delta_lon: ", world%config%delta_lon
    print *, "lat_0: ", world%config%lat_0
    print *, "lon_0: ", world%config%lon_0
    
    print *, "HEP Dimensions (from Config):"
    print *, "dlon_hep: ", world%config%dlon_hep
    print *, "dlat_hep: ", world%config%dlat_hep
    
    print *, "HEP Paths:"
    if (allocated(world%config%hep_paths)) then
        do i = 1, size(world%config%hep_paths)
            print *, "  Path ", i, ": ", trim(world%config%hep_paths(i))
        end do
    else
        print *, RED, "  HEP Paths NOT allocated!", RESET
    end if

    print *, "----------------------------------------"
    print *, "HEP Data Verification:"
    print *, "----------------------------------------"

    if (allocated(world%grid%hep)) then
        print *, "Grid HEP Array Allocated: YES"
        print *, "Dimensions: ", size(world%grid%hep, 1), "x", size(world%grid%hep, 2), &
                 "x", size(world%grid%hep, 3), "x", size(world%grid%hep, 4)
        
        ! Check a few values (e.g., middle of gradient)
        ! Gradient is 1 at West (index 1) to 0 at East (index 100)
        ! Value at index 1 should be ~1.0
        ! Value at index 100 should be ~0.0
        ! Value at index 50 should be ~0.5
        
        print *, "Sample Values (Time 1, Pop 1):"
        print *, "  HEP(1, 50): ", world%grid%hep(1, 50, 1, 1), " (Expected ~1.0)"
        print *, "  HEP(50, 50): ", world%grid%hep(50, 50, 1, 1), " (Expected ~0.5)"
        print *, "  HEP(100, 50): ", world%grid%hep(100, 50, 1, 1), " (Expected ~0.0)"
        
    else
        print *, RED, "Grid HEP Array Allocated: NO", RESET
    end if

    print *, "========================================"
    print *, GREEN, "Test Complete", RESET
    print *, "========================================"

end program test_config_read
