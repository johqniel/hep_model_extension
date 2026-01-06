module mod_python_interface

    use mod_config
    use mod_agent_world
    use mod_modules_hash
    use mod_setup
    use mod_test_utilities
    use mod_export_agents_hash
    use mod_extract_plottable_data

    implicit none

    private

    public :: init_simulation, step_simulation, get_simulation_hep
    public :: get_simulation_agents, get_agent_count, get_grid_dims
    public :: get_simulation_config, set_simulation_config_path, set_custom_hep_paths
    public :: set_spawn_configuration, regenerate_agents

    ! Global world container for the interface
    type(world_container), target, save :: world

    contains

    ! =================================================================================
    ! Initialize the simulation
    ! =================================================================================
    subroutine init_simulation()
        implicit none

        print *, "--- Python Interface: Initializing Simulation ---"

        ! 1. Initialize World
        print*, "init World..."
        call world%init_world()
        
        ! 2. Setup World (Config, Grid, etc.)
        print*, "setup World..."
        call world%setup_world()
        
        ! 3. Generate Initial Agents
        print*, "generate initial agents..."
        call generate_initial_agents_old(world)

        ! 4. Initial Compaction
        call compact_agents(world)

        ! 5. Verify Integrity
        call verify_agent_array_integrity(world)
        call verify_grid_integrity(world)

        print *, "--- Initialization Complete ---"
        print*, "Agents in array: ", count_agents_in_array(world)
        print*, "Agents in grid: ", count_agents_in_grid(world)

    end subroutine init_simulation

    ! =================================================================================
    ! Step the simulation by one time step
    ! =================================================================================
    subroutine step_simulation(t)
        implicit none
        integer, intent(in) :: t
        integer :: jp

        ! Update t_hep (assuming delta_t_hep is available in config)
        ! Note: t_hep is used in agent_above_water check inside agent_move
        ! We need to make sure the grid knows about t_hep if it's stored there, 
        ! or if it's passed. In main.f95 it was a local variable passed to nothing directly
        ! but used in the loop.
        ! Wait, agent_move uses `grid%t_hep`. We need to update that.
        
        world%grid%t_hep = int(t / world%config%delta_t_hep) + 1

        ! 1. Agent Modules (Move)
        call apply_module_to_agents(agent_move, t)
        
        ! 2. Compact Agents (Handle deaths, etc.)
        call compact_agents(world)

        ! 3. Update HEP Density (if needed for pop pressure or stats)
        do jp = 1, world%config%npops
             call world%update_hep_density(jp)
        end do

        ! Optional: Periodic verification or output could go here, 
        ! but for a raw interface, we keep it minimal.

    end subroutine step_simulation

    ! =================================================================================
    ! Helper: Get Grid Dimensions
    ! =================================================================================
    subroutine get_grid_dims(dlon, dlat, npops)
        implicit none
        integer, intent(out) :: dlon, dlat, npops
        
        if (allocated(world%grid%hep)) then
            dlon = size(world%grid%hep, 1)
            dlat = size(world%grid%hep, 2)
            npops = size(world%grid%hep, 3)
        else
            dlon = 0; dlat = 0; npops = 0
        endif
    end subroutine get_grid_dims

    ! =================================================================================
    ! Helper: Get Simulation Config
    ! =================================================================================
    subroutine get_simulation_config(lon_0, lat_0, delta_lon, delta_lat, dlon_hep, dlat_hep)
        implicit none
        real(8), intent(out) :: lon_0, lat_0, delta_lon, delta_lat
        integer, intent(out) :: dlon_hep, dlat_hep
        
        lon_0 = world%config%lon_0
        lat_0 = world%config%lat_0
        delta_lon = world%config%delta_lon
        delta_lat = world%config%delta_lat
        dlon_hep = world%config%dlon_hep
        dlat_hep = world%config%dlat_hep
    end subroutine get_simulation_config

    ! =================================================================================
    ! Helper: Set Simulation Config Path
    ! =================================================================================
    subroutine set_simulation_config_path(path)
        use mod_read_inputs, only: set_config_path
        implicit none
        character(len=*), intent(in) :: path
        call set_config_path(path)
    end subroutine set_simulation_config_path

    ! =================================================================================
    ! Helper: Set Custom HEP Paths
    ! =================================================================================
    subroutine set_custom_hep_paths(paths, count)
        use mod_read_inputs, only: set_hep_paths
        implicit none
        integer, intent(in) :: count
        character(len=256), dimension(count), intent(in) :: paths
        call set_hep_paths(paths)
    end subroutine set_custom_hep_paths

    ! =================================================================================
    ! Helper: Set Spawn Configuration
    ! =================================================================================
    subroutine set_spawn_configuration(ns, x_ini, y_ini, spread, counts, npops)
        implicit none
        integer, intent(in) :: ns, npops
        real(8), dimension(ns, npops), intent(in) :: x_ini, y_ini, spread
        integer, dimension(ns, npops), intent(in) :: counts
        
        integer :: jp, n
        
        ! Update config
        world%config%ns = ns
        ! npops should match, but we trust the input for now or check it
        if (npops /= world%config%npops) then
            print *, "Warning: npops mismatch in set_spawn_configuration"
        endif
        
        ! Re-allocate arrays if necessary (or just deallocate and allocate)
        if (allocated(world%config%x_ini_c)) deallocate(world%config%x_ini_c)
        if (allocated(world%config%y_ini_c)) deallocate(world%config%y_ini_c)
        if (allocated(world%config%ini_spread)) deallocate(world%config%ini_spread)
        if (allocated(world%config%hum_0)) deallocate(world%config%hum_0)
        
        allocate(world%config%x_ini_c(ns, npops))
        allocate(world%config%y_ini_c(ns, npops))
        allocate(world%config%ini_spread(ns, npops))
        allocate(world%config%hum_0(ns, npops))
        
        ! Copy data
        world%config%x_ini_c = x_ini
        world%config%y_ini_c = y_ini
        world%config%ini_spread = spread
        world%config%hum_0 = counts
        
        print *, "Spawn configuration updated via Python interface."
        print *, "NS:", ns, " NPOPS:", npops
        
    end subroutine set_spawn_configuration

    ! =================================================================================
    ! Helper: Regenerate Agents
    ! =================================================================================
    subroutine regenerate_agents()
        implicit none
        
        print *, "--- Python Interface: Regenerating Agents ---"
        
        ! 1. Reset Agents (Clear grid, map, counters)
        call world%reset_agents()
        
        ! 2. Generate Initial Agents (using current config)
        call generate_initial_agents_old(world)
        
        ! 3. Initial Compaction
        call compact_agents(world)
        
        ! 4. Verify Integrity
        call verify_agent_array_integrity(world)
        call verify_grid_integrity(world)
        
        print *, "--- Regeneration Complete ---"
        print*, "Agents in array: ", count_agents_in_array(world)
        
    end subroutine regenerate_agents

    ! =================================================================================
    ! Wrapper: Get HEP data
    ! =================================================================================
    subroutine get_simulation_hep(t_hep, hep_array, dlon, dlat, npops)
        implicit none
        integer, intent(in) :: t_hep, dlon, dlat, npops
        real, intent(out) :: hep_array(dlon, dlat, npops)
        real, allocatable :: temp_hep(:,:,:)
        
        ! We use the allocatable version from mod_extract_plottable_data and copy
        call get_hep_at_time(world, t_hep, temp_hep)
        
        if (allocated(temp_hep)) then
             hep_array = temp_hep
        endif
    end subroutine get_simulation_hep

    ! =================================================================================
    ! Helper: Get Agent Count
    ! =================================================================================
    subroutine get_agent_count(count)
        implicit none
        integer, intent(out) :: count
        count = count_agents_in_array(world)
    end subroutine get_agent_count

    ! =================================================================================
    ! Wrapper: Get Agents data
    ! =================================================================================
    subroutine get_simulation_agents(count, x, y, pop)
        implicit none
        integer, intent(in) :: count
        real(8), intent(out) :: x(count)
        real(8), intent(out) :: y(count)
        integer, intent(out) :: pop(count)
        
        real(8), allocatable :: temp_x(:), temp_y(:)
        integer, allocatable :: temp_pop(:)
        integer :: actual_count
        
        call get_alive_agents_data(world, actual_count, temp_x, temp_y, temp_pop)
        
        if (allocated(temp_x)) then
            ! Copy up to min(count, actual_count)
            x(1:min(count, actual_count)) = temp_x(1:min(count, actual_count))
            y(1:min(count, actual_count)) = temp_y(1:min(count, actual_count))
            pop(1:min(count, actual_count)) = temp_pop(1:min(count, actual_count))
        endif
    end subroutine get_simulation_agents

    ! =================================================================================
    ! Helper: Apply module to agents (Copied/Adapted from main.f95)
    ! =================================================================================
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

    ! =================================================================================
    ! Helper: Count agents in array
    ! =================================================================================
    integer function count_agents_in_array(world)
        class(world_container), intent(in) :: world
        integer :: pop
        
        count_agents_in_array = 0
        do pop = 1, world%config%npops
            count_agents_in_array = count_agents_in_array + world%num_humans(pop)
        end do
    end function count_agents_in_array

    ! =================================================================================
    ! Helper: Count agents in grid
    ! =================================================================================
    integer function count_agents_in_grid(world)
        class(world_container), intent(in) :: world
        integer :: i, j, k
        
        count_agents_in_grid = 0
        ! Iterate over grid cells (assuming grid dimensions are accessible)
        ! This is expensive, maybe just skip it or use a simplified check if needed.
        ! For now, let's just return -1 or implement a proper count if grid dims are known.
        ! Actually, let's just use the array count for now to verify compilation, 
        ! or implement the loop if we have access to grid dims.
        ! world%config%dlon_hep and dlat_hep should be available.
        
        if (allocated(world%grid%cell)) then
            do i = 1, world%config%dlon_hep
                do j = 1, world%config%dlat_hep
                    count_agents_in_grid = count_agents_in_grid + world%grid%cell(i, j)%number_of_agents
                end do
            end do
        endif
    end function count_agents_in_grid

end module mod_python_interface
