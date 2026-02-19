program main_fortran

    use mod_config
    use mod_agent_world
    use mod_birth_death_agb
    use mod_birth_technical
    use mod_initial_agents
    use mod_export_agents_hash
    use mod_reviewed_modules
    use mod_yaping_development
    use mod_analyze
    use mod_test_utilities

    implicit none

    ! Command line arguments
    integer :: output_interval = 1000
    integer :: argc, i, t_max
    character(len=100) :: arg, value_str
    
    ! World container
    type(world_container), target :: world
    
    ! Simulation variables
    integer :: t, jp, k, t_hep
    type(Agent), pointer :: current_agent

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read Command-Line Arguments 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    argc = command_argument_count()
    i = 1
    do while (i <= argc)
        call get_command_argument(i, arg)

        if (trim(arg) == '--output_interval') then
            i = i + 1
            call get_command_argument(i, value_str)
            read(value_str, *) output_interval
        end if
        
        i = i + 1
    end do

    print *, "--- Simulation Parameters ---"
    print *, "Output Interval: ", output_interval
    print *, "---------------------------"
    call flush(6)

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup World
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    call set_config_path("input/config/main_fortran_config.nml")

    print*, "init World..."
    call world%init_world()
    
    print*, "setup World..."
    call world%setup_world()
    
    print*, "generate initial agents..."
    call generate_initial_agents(world)

    ! Initial compaction and verification
    call compact_agents(world)
    call verify_agent_array_integrity(world)
    call verify_grid_integrity(world)

    print *, "Agents in array: ", count_agents_in_array(world)
    print *, "Agents in grid: ", count_agents_in_grid(world)
    call flush(6)

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main Calculation Loop
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print *, "Begin main calculation..."
    call flush(6)
    
    t_max = world%config%Tn

    timesteps: do t = 1, t_max

        ! Update t_hep
        t_hep = int(t / world%config%delta_t_hep) + 1
        world%grid%t_hep = t_hep

        ! ---------------------------------------------------------
        ! 1. Reviewed Agent Motion (Agent-Centric)
        ! ---------------------------------------------------------
        call apply_agent_module(reviewed_agent_motion, t)

        ! ---------------------------------------------------------
        ! 2. Yaping Move (Agent-Centric)
        ! ---------------------------------------------------------
        call apply_agent_module(yaping_move, t)

        ! ---------------------------------------------------------
        ! 3. Yaping Birth Grid (Grid-Centric)
        ! ---------------------------------------------------------
        call yaping_birth_grid(world, t)

        ! ---------------------------------------------------------
        ! 4. Yaping Death AGB (Agent-Centric)
        ! ---------------------------------------------------------
        call apply_agent_module(yaping_death_agb, t)

        ! ---------------------------------------------------------
        ! 5. Yaping Death Grid (Grid-Centric)
        ! ---------------------------------------------------------
        call yaping_death_grid(world, t)


        ! ---------------------------------------------------------
        ! Housekeeping
        ! ---------------------------------------------------------
        call compact_agents(world)
        
        do jp = 1, world%config%npops
             call world%update_hep_density(jp)
        end do

        ! ---------------------------------------------------------
        ! Output / Status
        ! ---------------------------------------------------------

        if (mod(t, 1000) == 0) then
            print *, "--- Tick:", t, " / ", t_max, " ---"
            print *, "Agents (Array): ", count_agents_in_array(world)
            print *, "Agents (Grid):  ", count_agents_in_grid(world)
            print *, "Deaths (Natural/Starv/OOB/Conflict/Random):", &
                world%counter%death_natural, "/", &
                world%counter%death_starvation, "/", &
                world%counter%death_out_of_bounds, "/", &
                world%counter%death_conflict, "/", &
                world%counter%death_random
            call flush(6)
        end if
        
        if (mod(t, output_interval) == 0) then
            ! Optional: write output logic if needed, mimicking main.f95 structure but simplified
            ! For now just print that we passed the interval
            ! print *, "Output interval reached."
        end if

    end do timesteps

    print *, "Simulation finished."
    print *, "Final GXGY counter: ", world%counter%gxgy_out_counter
    call flush(6)
    
    call world%cleanup_world()

contains

    subroutine apply_agent_module(func, t)
        implicit none
        interface
            subroutine func(agent_ptr)
                import :: Agent
                type(Agent), pointer, intent(inout) :: agent_ptr
            end subroutine func
        end interface
        integer, intent(in) :: t

        integer :: jp, k
        type(Agent), pointer :: current_agent

        do jp = 1, world%config%npops
            ! Check start time for population
            if (t < world%config%tstep_start(jp)) cycle

            do k = 1, world%num_humans(jp)
                current_agent => world%agents(k, jp)

                if (current_agent%is_dead) cycle

                call func(current_agent)
            end do
        end do

    end subroutine apply_agent_module

end program main_fortran
