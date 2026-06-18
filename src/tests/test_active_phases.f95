program test_active_phases
    use mod_agent_world
    use mod_config
    use mod_read_inputs, only: set_config_path
    use mod_test_utilities
    implicit none

    type(world_container) :: world
    integer :: jp

    print *, "========================================"
    print *, "Starting Test: Staggered Active Phases"
    print *, "========================================"

    ! Set the config path to our new test config file
    call set_config_path("input/config/test_active_phases.nml")

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
    print *, "use_active_time_phases: ", world%config%use_active_time_phases

    print *, "----------------------------------------"
    print *, "Population Active Boundaries:"
    print *, "----------------------------------------"
    do jp = 1, world%config%npops
        print *, "Pop ", jp, ":"
        print *, "  tyr_start:   ", world%config%tyr_start(jp)
        print *, "  tyr_end:     ", world%config%tyr_end(jp)
        print *, "  tstep_start: ", world%config%tstep_start(jp)
        print *, "  tstep_end:   ", world%config%tstep_end(jp)
    end do

    print *, "----------------------------------------"
    print *, "Assertions & Verification:"
    print *, "----------------------------------------"

    ! Assertions for Population 1 (NEA)
    if (world%config%tstep_start(1) /= 1) then
        print *, RED, "Error: Pop 1 start step should be 1", RESET
        stop 1
    end if
    if (world%config%tstep_end(1) /= 30001) then
        print *, RED, "Error: Pop 1 end step should be 30001", RESET
        stop 1
    end if

    ! Assertions for Population 2 (AMH)
    if (world%config%tstep_start(2) /= 30001) then
        print *, RED, "Error: Pop 2 start step should be 30001", RESET
        stop 1
    end if
    if (world%config%tstep_end(2) /= 60001) then
        print *, RED, "Error: Pop 2 end step should be 60001", RESET
        stop 1
    end if

    ! Assertions for Population 3 (MIX)
    if (world%config%tstep_start(3) /= 60001) then
        print *, RED, "Error: Pop 3 start step should be 60001", RESET
        stop 1
    end if
    if (world%config%tstep_end(3) /= 100001) then
        print *, RED, "Error: Pop 3 end step should be 100001", RESET
        stop 1
    end if

    print *, GREEN, "SUCCESS: All active phase assertions passed!", RESET
    print *, "========================================"

end program test_active_phases
