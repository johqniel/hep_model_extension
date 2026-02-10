module mod_python_interface

    use mod_config
    use mod_agent_world
    use mod_birth_death_agb
    use mod_birth_death_strict
    use mod_birth_death_probabilistic
    use mod_move
    use mod_birth_technical
    use mod_setup
    use mod_test_utilities
    use mod_export_agents_hash
    use mod_extract_plottable_data
    use mod_watershed
    use mod_clustering

    implicit none

    private

    public :: init_simulation, step_simulation, get_simulation_hep
    public :: get_simulation_agents, get_agent_count, get_grid_dims
    public :: get_simulation_config, set_simulation_config_path, set_custom_hep_paths
    public :: set_spawn_configuration, regenerate_agents
    public :: set_active_modules, get_debug_stats, cleanup_simulation, cleanup_sim_step_1, cleanup_sim_step_2, cleanup_sim_step_3
    public :: init_sim_step_1, init_sim_step_2, init_sim_step_3, init_sim_step_4
    public :: init_sim_step_2_part_1, init_sim_step_2_part_2, init_sim_step_2_part_3
    public :: init_sim_step_2_part_2_arrays_only, init_sim_step_2_part_2_chunk, get_grid_nx
    public :: init_cluster_store, run_watershed_clustering
    public :: get_cluster_count, get_cluster_info, get_cell_cluster_map
    public :: check_agent_migration


    ! Module Constants
    integer, parameter :: MODULE_NATURAL_DEATHS = 1
    integer, parameter :: MODULE_BIRTHS = 2
    integer, parameter :: MODULE_MOVE = 3
    integer, parameter :: MODULE_UPDATE_AGE = 4
    integer, parameter :: MODULE_FIND_MATE = 5
    integer, parameter :: MODULE_DISTRIBUTE_RESOURCES = 6
    integer, parameter :: MODULE_RESOURCE_MORTALITY = 7
    integer, parameter :: MODULE_LANGEVIN_MOVE = 8
    integer, parameter :: MODULE_BIRTH_DEATH = 9
    integer, parameter :: MODULE_VERHULST_PRESSURE = 10
    integer, parameter :: MODULE_CLUSTERING = 11

    ! Active Modules Configuration
    integer, allocatable, save :: active_module_ids(:)
    integer, save :: num_active_modules = 0

    ! Global world container for the interface
    type(world_container), target, save :: world
    type(cluster_store_t), target, save :: cluster_store

    contains

    ! =================================================================================
    ! Initialize the simulation
    ! =================================================================================
    subroutine init_simulation(skip_generation)
        implicit none
        logical, optional, intent(in) :: skip_generation
        logical :: skip

        print *, "--- Python Interface: Initializing Simulation ---"

        ! 1. Initialize World
        print*, "init World..."
        call world%init_world()
        
        ! 2. Setup World (Config, Grid, etc.)
        print*, "setup World..."
        call world%setup_world()
        
        ! 3. Generate Initial Agents
        skip = .false.
        if (present(skip_generation)) skip = skip_generation
        
        if (.not. skip) then
            print*, "generate initial agents..."
            call generate_initial_agents_old(world)

            ! 4. Initial Compaction
            call compact_agents(world)
        else
            print*, "Skipping initial agent generation (custom spawn expected)."
        end if

        ! 5. Verify Integrity
        call verify_agent_array_integrity(world)
        call verify_grid_integrity(world)

        print *, "--- Initialization Complete ---"
        print*, "Agents in array: ", count_agents_in_array(world)
        print*, "Agents in grid: ", count_agents_in_grid(world)

    end subroutine init_simulation

    subroutine init_sim_step_1()
        implicit none
        print *, "--- Step 1: Init World ---"
        call flush(6)
        call world%init_world()
    end subroutine init_sim_step_1

    subroutine init_sim_step_2()
        implicit none
        print *, "--- Step 2: Setup World ---"
        call flush(6)
        call world%setup_world()
    end subroutine init_sim_step_2

    subroutine init_sim_step_2_part_1()
        implicit none
        print *, "--- Step 2.1: Setup World Config ---"
        call flush(6)
        call world%setup_world_p1_config()
    end subroutine init_sim_step_2_part_1

    subroutine init_sim_step_2_part_2()
        implicit none
        print *, "--- Step 2.2: Setup World Alloc ---"
        call flush(6)
        call world%setup_world_p2_alloc()
    end subroutine init_sim_step_2_part_2

    subroutine init_sim_step_2_part_2_arrays_only()
        implicit none
        print *, "--- Step 2.2: Setup World Alloc (Arrays Only) ---"
        call flush(6)
        call world%setup_world_p2_alloc_arrays_only()
    end subroutine init_sim_step_2_part_2_arrays_only

    subroutine init_sim_step_2_part_2_chunk(start_x, end_x)
        implicit none
        integer, intent(in) :: start_x, end_x
        ! print *, "--- Step 2.2: Setup World Chunk ", start_x, " to ", end_x, " ---"
        call world%setup_world_init_grid_range(start_x, end_x)
    end subroutine init_sim_step_2_part_2_chunk

    function get_grid_nx() result(nx)
        implicit none
        integer :: nx
        nx = world%setup_world_get_grid_nx()
    end function get_grid_nx

    subroutine init_sim_step_2_part_3()
        implicit none
        print *, "--- Step 2.3: Setup World Data ---"
        call flush(6)
        call world%setup_world_p3_data()
    end subroutine init_sim_step_2_part_3


    subroutine init_sim_step_3(skip_gen)
        implicit none
        logical, intent(in) :: skip_gen
        
        if (.not. skip_gen) then
             print *, "--- Step 3: Generate Agents ---"
             call flush(6)
             call generate_initial_agents_old(world)
             call compact_agents(world)
        else
             print *, "--- Step 3: Skipping Generation ---"
             call flush(6)
        endif
    end subroutine init_sim_step_3

    subroutine init_sim_step_4()
        implicit none
        print *, "--- Step 4: Verify ---"
        call flush(6)
        call verify_agent_array_integrity(world)
        call verify_grid_integrity(world)
        print *, "--- Initialization Complete ---"
        print*, "Agents in array: ", count_agents_in_array(world)
        call flush(6)
    end subroutine init_sim_step_4

    ! =================================================================================
    ! Step the simulation by one time step
    ! =================================================================================
    subroutine step_simulation(t)
        implicit none
        integer, intent(in) :: t
        integer :: jp, ipop

        ! Update t_hep (assuming delta_t_hep is available in config)
        ! Note: t_hep is used in agent_above_water check inside agent_move
        ! We need to make sure the grid knows about t_hep if it's stored there, 
        ! or if it's passed. In main.f95 it was a local variable passed to nothing directly
        ! but used in the loop.
        ! Wait, agent_move uses `grid%t_hep`. We need to update that.
        
        world%grid%t_hep = int(t / world%config%delta_t_hep) + 1

        ! 1. Load Agent Modules (Configurable)
        if (num_active_modules > 0) then
            do jp = 1, num_active_modules
                select case (active_module_ids(jp))
                    case (MODULE_NATURAL_DEATHS)
                        call apply_module_to_agents(realise_natural_deaths, t)
                    case (MODULE_BIRTHS)
                        call apply_module_to_agents(realise_births, t)
                    case (MODULE_MOVE)
                        call apply_module_to_agents(agent_move, t)
                    case (MODULE_UPDATE_AGE)
                        call apply_module_to_agents(update_age_pregnancy, t)
                    case (MODULE_FIND_MATE)
                        call apply_module_to_agents(find_mate, t)
                    case (MODULE_DISTRIBUTE_RESOURCES)
                        call distribute_ressources(world)
                    case (MODULE_RESOURCE_MORTALITY)
                        call apply_module_to_agents(resource_mortality, t)
                    case (MODULE_LANGEVIN_MOVE)
                        call apply_module_to_agents(agent_move_langevin, t)
                    case (MODULE_BIRTH_DEATH)
                        do ipop = 1, world%config%npops
                            call apply_birth_death_all_cells(world, ipop)
                        end do
                    case (MODULE_VERHULST_PRESSURE)
                        do ipop = 1, world%config%npops
                            call apply_verhulst_pressure_all_cells( &
                                world, ipop)
                        end do
                    case (MODULE_CLUSTERING)
                        if (mod(t, cluster_store%update_interval) &
                            == 0) then
                            call run_watershed_clustering( &
                                1, t, 2, 0.05d0)
                        end if
                end select
            end do
        else
            ! Default Order
            !call apply_module_to_agents(realise_natural_deaths, t)
            !call apply_module_to_agents(realise_births, t)
            !call apply_module_to_agents(agent_move, t)
            !call apply_module_to_agents(update_age_pregnancy, t)
        end if

        ! 1.5 Set booleans for modules in world 
        if (num_active_modules > 0) then
            do jp = 1, num_active_modules
                select case (active_module_ids(jp))
                    case (MODULE_NATURAL_DEATHS)

                    case (MODULE_BIRTHS)

                    case (MODULE_MOVE)

                    case (MODULE_UPDATE_AGE)

                    case (MODULE_FIND_MATE)
                        
                    case (MODULE_DISTRIBUTE_RESOURCES)
                        world%ressources_module_active = .true.
                end select
            end do
        else
        endif
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
    ! Helper: Set Active Modules
    ! =================================================================================
    subroutine set_active_modules(modules, count)
        implicit none
        integer, intent(in) :: count
        integer, dimension(count), intent(in) :: modules
        
        if (allocated(active_module_ids)) deallocate(active_module_ids)
        allocate(active_module_ids(count))
        
        active_module_ids = modules
        num_active_modules = count
        
        print *, "Active modules updated. Count:", count
    end subroutine set_active_modules

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
    subroutine get_simulation_agents(count, x, y, pop, age, gender_int, resources, children, is_pregnant_out, avg_resources_out)
        implicit none
        integer, intent(in) :: count
        real(8), intent(out), dimension(count) :: x
        real(8), intent(out), dimension(count) :: y
        integer, intent(out), dimension(count) :: pop
        integer, intent(out), dimension(count) :: age
        integer, intent(out), dimension(count) :: gender_int
        integer, intent(out), dimension(count) :: resources
        integer, intent(out), dimension(count) :: children
        integer, intent(out), dimension(count) :: is_pregnant_out
        real(8), intent(out), dimension(count) :: avg_resources_out
        
        real(8), allocatable :: temp_x(:), temp_y(:), temp_avg_resources(:)
        integer, allocatable :: temp_pop(:), temp_age(:), temp_resources(:), temp_children(:), temp_is_pregnant(:)
        character(len=1), allocatable :: temp_gender(:)
        
        integer :: actual_count, i, limit
        
        call get_alive_agents_data(world, actual_count, temp_x, temp_y, temp_pop, &
                                   temp_age, temp_gender, temp_resources, temp_children, &
                                   temp_is_pregnant, temp_avg_resources)
        
        if (allocated(temp_x)) then
            limit = min(count, actual_count)
            do i = 1, limit
                x(i) = temp_x(i)
                y(i) = temp_y(i)
                pop(i) = temp_pop(i)
                age(i) = temp_age(i)
                if (temp_gender(i) == 'M') then
                    gender_int(i) = 1
                else
                    gender_int(i) = 0
                end if
                resources(i) = temp_resources(i)
                children(i) = temp_children(i)
                is_pregnant_out(i) = temp_is_pregnant(i)
                avg_resources_out(i) = temp_avg_resources(i)
            end do
        end if
        
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

        ! Bemerkung: 
        !                It would be cleaner/ more robust to compact agents after each module, 
        !                but compacting is costly and we probably save time by compacting less often.
        !                Eventually there should be like a rule to compact agents after: 
        !                    - num dead agents > num alive agents
        !                    - num dead_agents + num alive agents > 0.9 * array size 
        !                    - something like that ...

        !if (world%number_of_agents_marked_for_death > 0) then
        !    print*, "Warning: compact_agents called before applying module to agents."
        !    call compact_agents(world)
        !end if

        do jp = 1, world%config%npops
            ! Check start time for population
            if (t < world%config%tstep_start(jp)) cycle

            do k = 1, world%num_humans(jp)
                current_agent => world%agents(k, jp)

                if (current_agent%is_dead) cycle

                call func(current_agent)
            end do
        end do

        !if (world%number_of_agents_marked_for_death > 0) then
        !    call compact_agents(world)
        !end if

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

    ! =================================================================================
    ! Wrapper: Get Debug Stats
    ! =================================================================================
    subroutine get_debug_stats(natural, starvation, oob, conflict, random, gxgy_out, update_pos, move_calls)
        implicit none
        integer, intent(out) :: natural
        integer, intent(out) :: starvation
        integer, intent(out) :: oob
        integer, intent(out) :: conflict
        integer, intent(out) :: random
        integer, intent(out) :: gxgy_out
        integer, intent(out) :: update_pos
        integer, intent(out) :: move_calls
        
        if (.not. allocated(world%agents)) then
             natural = -1
             starvation = -1
             oob = -1
             conflict = -1
             random = -1
             gxgy_out = -1
             update_pos = -1
             move_calls = -1
             return
        endif

        natural = world%counter%death_natural
        starvation = world%counter%death_starvation
        oob = world%counter%death_out_of_bounds
        conflict = world%counter%death_conflict
        random = world%counter%death_random
        gxgy_out = world%counter%gxgy_out_counter
        update_pos = world%counter%update_pos_calls
        move_calls = world%counter%move_calls

    end subroutine get_debug_stats

    ! =================================================================================
    ! Cleanup Simulation
    ! =================================================================================
    subroutine cleanup_simulation()
        implicit none
        print *, "--- Python Interface: Cleaning up Simulation ---"
        call flush(6)
        call world%cleanup_world()
        call cluster_store%cleanup()
        
        num_active_modules = 0
        if (allocated(active_module_ids)) deallocate(active_module_ids)
        
        print *, "--- Cleanup Complete ---"
        call flush(6)
    end subroutine cleanup_simulation

    subroutine cleanup_sim_step_1()
        implicit none
        print *, "--- Cleanup Step 1: Grid ---"
        call flush(6)
        call world%cleanup_grid_subset()
    end subroutine cleanup_sim_step_1

    subroutine cleanup_sim_step_2()
        implicit none
        print *, "--- Cleanup Step 2: Agents ---"
        call flush(6)
        call world%cleanup_agents_subset()
    end subroutine cleanup_sim_step_2

    subroutine cleanup_sim_step_3()
        implicit none
        print *, "--- Cleanup Step 3: Finalize ---"
        call flush(6)
        call world%cleanup_final_subset()
        num_active_modules = 0
        if (allocated(active_module_ids)) deallocate(active_module_ids)
    end subroutine cleanup_sim_step_3


    ! =================================================================================
    ! Clustering: Init cluster store
    ! =================================================================================
    subroutine init_cluster_store(update_interval)
        implicit none
        integer, intent(in) :: update_interval

        call cluster_store%init(world%grid%nx, world%grid%ny, &
                                update_interval)
        print *, "Cluster store initialized:", &
                 world%grid%nx, "x", world%grid%ny
    end subroutine init_cluster_store

    ! =================================================================================
    ! Clustering: Run watershed clustering
    ! =================================================================================
    subroutine run_watershed_clustering(pop, tick, &
                                         smooth_radius, threshold)
        implicit none
        integer, intent(in) :: pop, tick, smooth_radius
        real(8), intent(in) :: threshold

        integer :: i, j, nx, ny
        real(8), allocatable :: hep_surface(:,:)
        integer, allocatable :: agent_counts(:,:)

        nx = world%grid%nx
        ny = world%grid%ny

        ! Initialise store if needed
        if (cluster_store%nx == 0) then
            call cluster_store%init(nx, ny)
        end if

        ! Extract HEP surface for this population
        allocate(hep_surface(nx, ny))
        do j = 1, ny
            do i = 1, nx
                hep_surface(i, j) = world%grid%hep(i, j, &
                                     pop, world%grid%t_hep)
            end do
        end do

        ! Run watershed + build clusters with persistent IDs
        call cluster_store%run_watershed(hep_surface, &
                                          world%grid%lon_hep, &
                                          world%grid%lat_hep, &
                                          tick, smooth_radius, &
                                          threshold)

        ! Count agents per cluster
        allocate(agent_counts(nx, ny))
        do j = 1, ny
            do i = 1, nx
                agent_counts(i, j) = &
                    world%grid%cell(i, j)%number_of_agents
            end do
        end do
        call cluster_store%count_agents(agent_counts)

        deallocate(hep_surface, agent_counts)

        call cluster_store%print_summary()

    end subroutine run_watershed_clustering

    ! =================================================================================
    ! Clustering: Get cluster count and migration stats
    ! =================================================================================
    subroutine get_cluster_count(info)
        implicit none
        integer, intent(out) :: info(3)
        info(1) = cluster_store%n_clusters
        info(2) = cluster_store%migration_events
        info(3) = cluster_store%migration_events_total
    end subroutine get_cluster_count

    ! =================================================================================
    ! Clustering: Get info for cluster k (1-indexed)
    ! =================================================================================
    subroutine get_cluster_info(k, iinfo, rinfo)
        implicit none
        integer, intent(in)  :: k
        integer, intent(out) :: iinfo(3)
        real(8), intent(out) :: rinfo(2)

        if (k >= 1 .and. k <= cluster_store%n_clusters) then
            iinfo(1) = cluster_store%clusters(k)%id
            iinfo(2) = cluster_store%clusters(k)%n_cells
            iinfo(3) = cluster_store%clusters(k)%n_agents
            rinfo(1) = cluster_store%clusters(k)%centroid_x
            rinfo(2) = cluster_store%clusters(k)%centroid_y
        else
            iinfo = 0
            rinfo = 0.0d0
        end if
    end subroutine get_cluster_info

    ! =================================================================================
    ! Clustering: Get full cell_cluster_map (2D)
    ! =================================================================================
    subroutine get_cell_cluster_map(map_out, nx, ny)
        implicit none
        integer, intent(in)  :: nx, ny
        integer, intent(out) :: map_out(nx, ny)

        if (allocated(cluster_store%cell_cluster_map)) then
            map_out = cluster_store%cell_cluster_map
        else
            map_out = -2
        end if
    end subroutine get_cell_cluster_map

    ! =================================================================================
    ! Clustering: Check agent migration between clusters
    ! =================================================================================
    subroutine check_agent_migration(old_gx, old_gy, &
                                      new_gx, new_gy, migrated)
        implicit none
        integer, intent(in)  :: old_gx, old_gy, new_gx, new_gy
        logical, intent(out) :: migrated
        integer :: from_c, to_c

        call cluster_store%check_migration( &
            old_gx, old_gy, new_gx, new_gy, &
            migrated, from_c, to_c)
    end subroutine check_agent_migration

end module mod_python_interface

