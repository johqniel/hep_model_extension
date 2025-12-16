program main_program

    use mod_config
    use mod_agent_world
    use mod_modules_hash
    use mod_setup
    use mod_export_agents_hash

    use mod_analyze

    implicit none



    ! Command line arguments
    integer :: output_interval_visualization_data = 1000
    real :: test_arg = 0.
    integer :: argc, i
    character(len=100) :: arg, value_str
    character(len=100) :: temp_string

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
            read(value_str, *) output_interval_visualization_data
        else if (trim(arg) == '--test_arg') then
            i = i + 1
            call get_command_argument(i, value_str)
            read(value_str, *) test_arg
        end if

        i = i + 1
    end do

    print *, "--- Simulation Parameters ---"
    print *, "Output Interval: ", output_interval_visualization_data
    print *, "test_arg:  ", test_arg
    print *, "---------------------------"

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup World
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print*, "init World..."

    call world%init_world()
    print*, "setup World..."
    call world%setup_world()
    print*, "generate initial agents..."
    call generate_initial_agents_old(world)

    ! Initial output
    call write_agents_to_csv_hash("agents_output_0.csv", 0, world)

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main Calculation Loop
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print*, "Agents in array: ", count_agents_in_array(world)
    print*, "Agents in grid: ", count_agents_in_grid(world)

    print *, "Begin main calculation..."

    timesteps: do t = 1, world%config%Tn

        ! Update t_hep
        t_hep = int(t / world%config%delta_t_hep) + 1

        ! ---------------------------------------------------------
        ! Agent Modules
        ! ---------------------------------------------------------

        call apply_module_to_agents(agent_move, t)
        call compact_agents(world%agents, world%index_map, world%num_humans)

        ! ---------------------------------------------------------
        ! Grid Management
        ! ---------------------------------------------------------

        ! ---------------------------------------------------------
        ! Update HEP
        ! ---------------------------------------------------------

        do jp = 1, world%config%npops
             call world%update_hep_density(jp)
        end do

        ! ---------------------------------------------------------
        ! Output
        ! ---------------------------------------------------------

        if (mod(t, output_interval_visualization_data) == 0) then
            write(temp_string, '(I0)') t
            call write_agents_to_csv_hash("data/agents_plotting_data_" // trim(temp_string) // ".csv", t, world)
            print *, "Time: ", t, " - Output written."
        end if



        if (mod(t, 100) == 0) then
            print *, "Update pos calls: ", world%counter%update_pos_calls
            !print*, "Move calls: ", world%counter%move_calls
            print*, "Agents in array: ", count_agents_in_array(world)
            print*, "Agents in grid: ", count_agents_in_grid(world)
        end if

    end do timesteps

    print *, "Simulation finished."

    print *, "GXGY counter: ", world%counter%gxgy_out_counter

contains

    subroutine apply_module_to_agents(func, t)
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
    end subroutine apply_module_to_agents



end program main_program

