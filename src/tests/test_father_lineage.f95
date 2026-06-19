program test_father_lineage
    use mod_agent_world
    use mod_test_utilities
    use mod_calculations
    implicit none

    type(world_container), target :: world
    type(Agent) :: mother_temp, father_temp
    type(Agent), pointer :: mother_ptr => null()
    type(Agent), pointer :: father_ptr => null()
    type(Agent), pointer :: child_ptr => null()
    type(Agent) :: child_val
    integer :: father_id, father_pop

    print *, "========================================"
    print *, "Starting Test: Father Lineage Preservation"
    print *, "========================================"

    ! 1. Initialize World
    call world%init_world()
    call world%setup_world()

    ! 2. Spawn Mother (Population 1, Female)
    mother_temp = world%spawn_agent_hash(1)
    mother_temp%pos_x = -5.0d0
    mother_temp%pos_y = 40.0d0
    mother_temp%gender = 'F'
    call add_agent_to_array_hash(world, mother_temp, 1, mother_ptr)

    ! 3. Spawn Father (Population 2, Male)
    father_temp = world%spawn_agent_hash(2)
    father_temp%pos_x = -5.0d0
    father_temp%pos_y = 40.0d0
    father_temp%gender = 'M'
    call add_agent_to_array_hash(world, father_temp, 2, father_ptr)

    if (.not. associated(mother_ptr) .or. .not. associated(father_ptr)) then
        print *, "Error: Spawning failed"
        call exit(1)
    endif

    father_id = father_ptr%id
    father_pop = father_ptr%population

    ! 4. Set mother to pregnant with the father
    mother_ptr%is_pregnant = 1
    mother_ptr%father_of_unborn_child = father_id
    mother_ptr%father_pop_of_unborn_child = father_pop

    ! 5. Kill and compact the father
    call father_ptr%agent_dies(reason=1)
    call compact_agents(world)

    ! Verify father is indeed dead/unassociated
    father_ptr => get_agent(father_id, world)
    if (associated(father_ptr)) then
        print *, "Error: Father should be dead and unassociated"
        call exit(1)
    endif

    ! 6. Realize birth with null father pointer (father_ptr is null)
    child_val = generate_agent_born(world, mother_ptr, father_ptr)

    ! Verify parent lineage and population inheritance
    print *, "Child Father ID: ", child_val%father, " (Expected: ", father_id, ")"
    print *, "Child Mother ID: ", child_val%mother, " (Expected: ", mother_ptr%id, ")"
    print *, "Child Population: ", child_val%population, " (Expected: 3 due to cross-population)"

    if (child_val%father /= father_id) then
        print *, "FAIL: Child father ID is incorrect"
        call exit(1)
    endif

    if (child_val%population /= 3) then
        print *, "FAIL: Child population is incorrect"
        call exit(1)
    endif

    print *, "========================================"
    print *, GREEN, "SUCCESS: Father Lineage Preservation Test Passed.", RESET
    print *, "========================================"

end program test_father_lineage
