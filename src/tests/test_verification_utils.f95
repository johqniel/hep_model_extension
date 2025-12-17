program test_verification_utils
    use mod_agent_world
    use mod_basic_config
    use mod_test_utilities
    use mod_calculations
    implicit none

    type(world_container) :: world
    type(Agent) :: new_agent
    integer :: i

    print *, "========================================"
    print *, "Starting Test: Verification Utilities"
    print *, "========================================"

    ! 1. Initialize World
    call world%init_world()
    call world%setup_world()

    ! 2. Spawn Agents
    print *, "Spawning 10 agents..."
    do i = 1, 10
        new_agent = world%spawn_agent_hash(1)
        new_agent%pos_x = 0.5d0 ! Valid position in 1x1 grid (default config)
        new_agent%pos_y = 0.5d0
        call add_agent_to_array_hash(world, new_agent, 1)
    end do
    print *, "Agents spawned."

    ! 3. Verify Valid State
    print *, "Verifying valid state..."
    call verify_agent_array_integrity(world)
    call verify_index_map_integrity(world)

    call verify_grid_integrity(world)
    print *, GREEN, "Valid state verification passed.", RESET

    ! 4. Corrupt State (Optional - for manual testing, uncomment to see failure)
    ! print *, "Corrupting state..."
    ! world%agents(1, 1)%id = -1 ! Create a hole
    ! call verify_agent_array_integrity(world) ! Should fail

    print *, "========================================"
    print *, GREEN, "SUCCESS: Verification utilities working.", RESET
    print *, "========================================"

end program test_verification_utils
