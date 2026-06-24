module mod_python_interface

    use mod_config
    use mod_agent_world
    use mod_reviewed_modules
    use mod_initial_agents
    use mod_birth_death_new, only: update_cluster_macroscopic_fertility_scale, &
                                   new_birth, new_death, &
                                   new_birth_shared_mc, new_death_shared_mc, &
                                   update_cluster_macroscopic_fertility_scale_shared_mc
    use mod_technical_modules
    use mod_test_utilities
    use mod_export_agents_hash
    use mod_extract_plottable_data
    use mod_watershed
    use mod_clustering
    use mod_creativity
    use mod_creativity_simple, only: precompute_cell_creativity_stats_simple => precompute_cell_creativity_stats, &
                                     update_creativity_simple
    use mod_creativity_fast, only: precompute_cell_creativity_stats_fast => precompute_cell_high_creativity, &
                                   update_creativity_fast

    implicit none

    private

    public :: init_simulation, step_simulation, get_simulation_hep
    public :: get_simulation_agents, get_agent_count, get_grid_dims
    public :: get_simulation_config, get_t_hep, set_simulation_config_path, set_custom_hep_paths, set_use_active_time_phases
    public :: set_spawn_configuration, regenerate_agents
    public :: set_active_modules, get_debug_stats, get_dynamic_state_stats
    public :: cleanup_simulation, cleanup_sim_step_1, cleanup_sim_step_2, cleanup_sim_step_3
    public :: init_sim_step_1, init_sim_step_2, init_sim_step_3, init_sim_step_4
    public :: init_sim_step_2_part_1, init_sim_step_2_part_2, init_sim_step_2_part_3
    public :: init_sim_step_2_part_2_arrays_only, init_sim_step_2_part_2_chunk, get_grid_nx
    public :: init_cluster_store, run_clustering
    public :: set_clustering_algorithm, get_clustering_algorithm, set_kmeans_clusters
    public :: set_dbscan_eps, set_dbscan_minpts
    public :: get_grid_density, apply_find_local_maxima
    public :: get_cluster_count, get_cluster_info
    public :: get_cell_cluster_map
    public :: set_age_distribution_interface, init_sim_step_apply_age_dist
    public :: set_forget_dead_agents, get_dead_agents_count, get_dead_simulation_agents, clear_dead_agents
    public :: set_performance_timing_enabled, set_verbose_step_debug
    public :: get_performance_stats, get_active_modules_count, get_active_modules_performance_stats
    public :: set_allow_across_populations


    ! Module Constants
    integer, parameter :: MODULE_REVIEWED_DEATH = 12
    integer, parameter :: MODULE_REVIEWED_BIRTH = 13
    integer, parameter :: MODULE_MOVE_CHILDREN_TO_MOTHERS = 14
    integer, parameter :: MODULE_REVIEWED_AGENT_MOTION = 21
    integer, parameter :: MODULE_CLUSTER_DEATH          = 22  ! Cluster Death (No Interaction)
    integer, parameter :: MODULE_CLUSTER_BIRTH          = 23  ! Cluster Birth (No Interaction)
    integer, parameter :: MODULE_CREATIVITY             = 24  ! Individual evolution (throttled by c3_individual_update_interval)
    integer, parameter :: MODULE_CREATIVITY_CLUSTER     = 25  ! Cluster accumulation (cheap, runs every tick)
    integer, parameter :: MODULE_CREATIVITY_SIMPLE      = 26  ! O(N) cell-aggregated individual evolution
    integer, parameter :: MODULE_CREATIVITY_FAST        = 27  ! O(N) top individuals creativity module
    integer, parameter :: MODULE_CLUSTER_DEATH_SHARED_MC = 28  ! Cluster Death (Shared MC)
    integer, parameter :: MODULE_CLUSTER_BIRTH_SHARED_MC = 29  ! Cluster Birth (Shared MC)

    ! Active Modules Configuration
    integer, allocatable, save :: active_module_ids(:)
    integer, save :: num_active_modules = 0

    ! Global world container for the interface
    type(world_container), target, save :: world

    ! Performance Timing Rolling Window (Arithmetic average of last 20 ticks)
    integer, parameter :: PERF_WINDOW = 20
    real(8), save :: perf_history(9, PERF_WINDOW) = 0.0d0
    real(8), allocatable, save :: perf_history_modules(:, :)
    integer, save :: perf_history_idx = 1

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
        call set_use_active_time_phases(.false.)
        
        ! 3. Generate Initial Agents
        skip = .false.
        if (present(skip_generation)) skip = skip_generation
        
        if (.not. skip) then
            print*, "generate initial agents..."
            call generate_initial_agents(world)

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
             call generate_initial_agents(world)
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
        call set_use_active_time_phases(.false.)
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
        integer :: jp
        logical :: use_cluster_dyn_state
        logical :: use_cluster_shared_mc_dyn_state
        integer :: dbg_jp, dbg_c, dbg_n_fast, dbg_n_acc, dbg_n_acc_global
        integer :: ii, dbg_gx, dbg_gy, dbg_c_idx, count_unclustered, count_clustered, count_invalid_c

        ! Timing variables
        integer(kind=8) :: t_start, t_end, t_rate
        integer(kind=8) :: t0, t1
        real(8) :: dt_p, dt_a, dt_c, dt_cl, dt_t, dt_module
        real(8) :: dt_g_density, dt_g_flows, dt_g_smoothing, dt_g_hep

        t_rate = 1
        t0 = 0
        t1 = 0
        t_start = 0
        t_end = 0
        dt_p = 0.0d0
        dt_a = 0.0d0
        dt_c = 0.0d0
        dt_cl = 0.0d0
        dt_t = 0.0d0
        dt_module = 0.0d0
        dt_g_density = 0.0d0
        dt_g_flows = 0.0d0
        dt_g_smoothing = 0.0d0
        dt_g_hep = 0.0d0

        if (world%performance_timing_enabled) then
            call system_clock(t_start, t_rate)
            t0 = t_start
        end if

        world%grid%t_hep = int(t / world%config%delta_t_hep) + 1

        ! 0. Cycle accumulators (reset current, shift history)
        ! This ensures history(1) is fresh for the current tick.
        if (world%performance_timing_enabled) call system_clock(t0)
        call world%cycle_accumulators()
        call apply_module_to_clusters(cycle_cluster_accumulators, world)
        world%current_tick = t

       ! 0.1. Load Permanent Modules (Always active, not configurable)
        if (world%performance_timing_enabled) call system_clock(t0)
        call apply_module_to_agents(update_agent_age, t)
        if (world%performance_timing_enabled) then
            call system_clock(t1)
            dt_p = dt_p + dble(t1 - t0) / dble(t_rate)
        end if

        ! 0.5. Update Dynamic State Vars (K_fertility, etc)
        if (world%performance_timing_enabled) call system_clock(t0)
        use_cluster_dyn_state = .false.
        use_cluster_shared_mc_dyn_state = .false.
        if (num_active_modules > 0) then
            do jp = 1, num_active_modules
                if (active_module_ids(jp) == MODULE_CLUSTER_DEATH       .or. &
                    active_module_ids(jp) == MODULE_CLUSTER_BIRTH        .or. &
                    active_module_ids(jp) == MODULE_CLUSTER_DEATH_SHARED_MC .or. &
                    active_module_ids(jp) == MODULE_CLUSTER_BIRTH_SHARED_MC) then
                    use_cluster_dyn_state = .true.
                end if
                if (active_module_ids(jp) == MODULE_CLUSTER_DEATH_SHARED_MC .or. &
                    active_module_ids(jp) == MODULE_CLUSTER_BIRTH_SHARED_MC) then
                    use_cluster_shared_mc_dyn_state = .true.
                end if
            end do
        end if
        
        ! DEBUG: Compare counting methods before controller update
        do dbg_jp = 1, world%config%npops
            dbg_n_fast = world%num_humans(dbg_jp) - world%num_humans_marked_dead(dbg_jp)
            dbg_n_acc_global = world%accumulators_history(1)%n_alive_acc(dbg_jp)
            if (dbg_n_fast < 0.9 * dbg_n_acc_global .or. dbg_n_fast > 1.1 * dbg_n_acc_global) then
                print*, "Warning: Counting agents fast differs a lot from global accumulators."

                print*, "[DBG] GLOBAL pop", dbg_jp, &
                        " count_fast=", dbg_n_fast, &
                        " n_alive_acc(1)=", dbg_n_acc_global
            end if
            if (allocated(world%cluster_store%clusters)) then
                dbg_n_acc = 0
                do dbg_c = 1, world%cluster_store%n_clusters
                    dbg_n_acc = dbg_n_acc + world%cluster_store%clusters(dbg_c)%accumulators_history(1)%n_alive_acc(dbg_jp)
                end do
                
                if (dbg_n_acc < 0.9 * dbg_n_fast .or. dbg_n_acc > 1.1 * dbg_n_fast) then
                    print*, "Warning: Sum of agents in all clusters differs a lot from global count (fast)."
                    print*, "[DBG] Cluster Sum pop", dbg_jp, &
                            " sum_cluster_n_alive_acc=", dbg_n_acc, &
                            " global_count_fast=", dbg_n_fast
                    
                    count_unclustered = 0
                    count_clustered = 0
                    count_invalid_c = 0
                    do ii = 1, world%num_humans(dbg_jp)
                        if (.not. world%agents(ii, dbg_jp)%is_dead) then
                            dbg_gx = world%agents(ii, dbg_jp)%gx
                            dbg_gy = world%agents(ii, dbg_jp)%gy
                            ! Bounds check: agents with default gx=-1 are not on the grid
                            if (dbg_gx < 1 .or. dbg_gx > world%grid%nx .or. &
                                dbg_gy < 1 .or. dbg_gy > world%grid%ny) then
                                count_invalid_c = count_invalid_c + 1
                                cycle
                            end if
                            if (allocated(world%cluster_store%cell_cluster_idx)) then
                                dbg_c_idx = world%cluster_store%cell_cluster_idx(dbg_gx, dbg_gy)
                            else
                                dbg_c_idx = -1
                            end if
                            
                            if (dbg_c_idx == -1 .or. dbg_c_idx == 0) then
                                count_unclustered = count_unclustered + 1
                            else if (dbg_c_idx > 0 .and. dbg_c_idx <= world%cluster_store%n_clusters) then
                                count_clustered = count_clustered + 1
                            else
                                count_invalid_c = count_invalid_c + 1
                            end if
                        end if
                    end do
                    print*, "[DBG] Detailed breakdown for population", dbg_jp, ":"
                    print*, "  - Total Alive Agents (calculated):", dbg_n_fast
                    print*, "  - Count inside valid cluster cells:", count_clustered
                    print*, "  - Count inside unclustered cells (c_idx=-1):", count_unclustered
                    print*, "  - Count with invalid/unallocated indices:", count_invalid_c
                    print*, "  - Number of active clusters:", world%cluster_store%n_clusters
                end if
            end if
        end do

        ! Always update global fallback (used by noise agents / global mode)
        call update_dynamic_state_variables(world)
        
        if (use_cluster_dyn_state) then
            call update_cluster_macroscopic_fertility_scale(world)
        end if
        if (use_cluster_shared_mc_dyn_state) then
            call update_cluster_macroscopic_fertility_scale_shared_mc(world)
        end if
        if (world%performance_timing_enabled) then
            call system_clock(t1)
            dt_g_density = dt_g_density + dble(t1 - t0) / dble(t_rate)
        end if



      
        ! 1. Load Agent Modules (Configurable)
        dt_a = 0.0d0
        if (num_active_modules > 0) then
            do jp = 1, num_active_modules
                if (world%performance_timing_enabled) call system_clock(t0)
                select case (active_module_ids(jp))
                    case (MODULE_REVIEWED_DEATH)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_REVIEWED_DEATH"
                            call flush(6)
                        end if
                        call apply_module_to_agents(reviewed_death, t)
                    case (MODULE_REVIEWED_BIRTH)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_REVIEWED_BIRTH"
                            call flush(6)
                        end if
                        call apply_module_to_agents(reviewed_birth, t)
                    case (MODULE_MOVE_CHILDREN_TO_MOTHERS)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_MOVE_CHILDREN_TO_MOTHERS"
                            call flush(6)
                        end if
                        call apply_module_to_agents(move_children_to_mothers, t)
                    case (MODULE_REVIEWED_AGENT_MOTION)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_REVIEWED_AGENT_MOTION"
                            call flush(6)
                        end if
                        call apply_module_to_agents( &
                            reviewed_agent_motion, t)
                    case (MODULE_CLUSTER_DEATH)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CLUSTER_DEATH"
                            call flush(6)
                        end if
                        call apply_module_to_agents(new_death, t)
                    case (MODULE_CLUSTER_BIRTH)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CLUSTER_BIRTH"
                            call flush(6)
                        end if
                        call apply_module_to_agents(new_birth, t)
                    case (MODULE_CREATIVITY)
                        ! Throttle via c3_individual_update_interval -- expensive neighbor scan
                        if (mod(t, world%config%c3_individual_update_interval) == 0) then
                            if (world%verbose_step_debug) then
                                print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CREATIVITY"
                                call flush(6)
                            end if
                            call apply_module_to_agents(update_creativity, t)
                        end if
                    case (MODULE_CREATIVITY_SIMPLE)
                        ! O(N) cell-aggregated creativity: pre-sweep + agent loop
                        if (mod(t, world%config%c3_individual_update_interval) == 0) then
                            if (world%verbose_step_debug) then
                                print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CREATIVITY_SIMPLE"
                                call flush(6)
                            end if
                            call precompute_cell_creativity_stats_simple(world)
                            call apply_module_to_agents(update_creativity_simple, t)
                        end if
                    case (MODULE_CREATIVITY_FAST)
                        ! O(N) top individuals creativity: pre-sweep + agent loop
                        if (mod(t, world%config%c3_individual_update_interval) == 0) then
                            if (world%verbose_step_debug) then
                                print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CREATIVITY_FAST"
                                call flush(6)
                            end if
                            call precompute_cell_creativity_stats_fast(world)
                            call apply_module_to_agents(update_creativity_fast, t)
                        end if
                    case (MODULE_CREATIVITY_CLUSTER)
                        ! Cheap per-tick accumulation of creativity into cluster sums
                        call apply_module_to_agents(accumulate_cluster_creativity, t)
                    case (MODULE_CLUSTER_DEATH_SHARED_MC)
                        ! Cluster Death with a shared MC constraint across all populations
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CLUSTER_DEATH_SHARED_MC"
                            call flush(6)
                        end if
                        call apply_module_to_agents(new_death_shared_mc, t)
                    case (MODULE_CLUSTER_BIRTH_SHARED_MC)
                        ! Cluster Birth with a shared MC constraint across all populations
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " module=", jp, " MODULE_CLUSTER_BIRTH_SHARED_MC"
                            call flush(6)
                        end if
                        call apply_module_to_agents(new_birth_shared_mc, t)
                end select
                if (world%verbose_step_debug) then
                    print*, "[DBG STEP] tick=", t, " module=", jp, " id=", active_module_ids(jp), " DONE"
                    call flush(6)
                end if
                if (world%performance_timing_enabled) then
                    call system_clock(t1)
                    dt_module = dble(t1 - t0) / dble(t_rate)
                    dt_a = dt_a + dt_module
                    if (allocated(perf_history_modules)) then
                        perf_history_modules(jp, perf_history_idx) = dt_module
                    end if
                end if
            end do
        else
            ! Default Order
            !call apply_module_to_agents(realise_natural_deaths, t)
            !call apply_module_to_agents(agent_move, t)
        end if


        ! 2. Compact Agents (Handle deaths, etc.)
        if (world%verbose_step_debug) then
            print*, "[DBG STEP] tick=", t, " compact_agents START"
            call flush(6)
        end if
        if (world%performance_timing_enabled) call system_clock(t0)
        call compact_agents(world, t)
        if (world%performance_timing_enabled) then
            call system_clock(t1)
            dt_c = dble(t1 - t0) / dble(t_rate)
        end if
        if (world%verbose_step_debug) then
            print*, "[DBG STEP] tick=", t, " compact_agents DONE"
            call flush(6)
        end if

        ! 3. realise new births 
        if (world%verbose_step_debug) then
            print*, "[DBG STEP] tick=", t, " realise_births START"
            call flush(6)
        end if
        if (world%performance_timing_enabled) call system_clock(t0)
        call apply_module_to_agents(realise_births, t)
        if (world%performance_timing_enabled) then
            call system_clock(t1)
            dt_p = dt_p + dble(t1 - t0) / dble(t_rate)
        end if
        if (world%verbose_step_debug) then
            print*, "[DBG STEP] tick=", t, " realise_births DONE"
            call flush(6)
        end if


        ! 4. Update Base HEP Density Computations (Pure Density, Flow, basic hep_av)
        if (world%config%efficient_density_updates .and. &
            (mod(t, world%cluster_store%update_interval) /= 0) .and. &
            allocated(world%cluster_store%clusters) .and. &
            world%cluster_store%n_clusters > 0) then
            call update_density_and_hep_grid_efficient(world, t, dt_g_density, dt_g_flows, dt_g_smoothing, dt_g_hep)
        else
            call update_density_and_hep_grid(world, t, dt_g_density, dt_g_flows, dt_g_smoothing, dt_g_hep)
        end if
        

        ! 4.5 density update done marker
        if (world%verbose_step_debug) then
            print*, "[DBG STEP] tick=", t, " density_update DONE"
            call flush(6)
        end if

        ! 5. Update Clustering (permanent, runs every cluster_update_interval ticks)
        if (world%performance_timing_enabled) call system_clock(t0)
        if (mod(t, world%cluster_store%update_interval) == 0) then
            if (world%verbose_step_debug) then
                print*, "[DBG STEP] tick=", t, " run_clustering START"
                call flush(6)
            end if
            call run_clustering(t)
            if (world%verbose_step_debug) then
                print*, "[DBG STEP] tick=", t, " run_clustering DONE"
                call flush(6)
            end if
            ! 5.1 Run cluster modules (compute per-cluster HEP sum and NC)
            if (world%verbose_step_debug) then
                print*, "[DBG STEP] tick=", t, " compute_cluster_hep_nc START"
                call flush(6)
            end if
            call apply_module_to_clusters(compute_cluster_hep_nc, world)
            if (world%verbose_step_debug) then
                print*, "[DBG STEP] tick=", t, " compute_cluster_hep_nc DONE"
                call flush(6)
            end if
        end if

        ! 5.2 Compute dynamic creativity-adapted carrying capacity
        !     Runs on its own independent c3_cluster_update_interval (config parameter).
        if (mod(t, world%config%c3_cluster_update_interval) == 0) then
            if (num_active_modules > 0) then
                do jp = 1, num_active_modules
                    if (active_module_ids(jp) == MODULE_CREATIVITY_CLUSTER) then
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " compute_available_hep START"
                            call flush(6)
                        end if
                        call apply_module_to_clusters(compute_available_hep, world)
                        if (world%verbose_step_debug) then
                            print*, "[DBG STEP] tick=", t, " compute_available_hep DONE"
                            call flush(6)
                        end if
                        exit
                    end if
                end do
            end if
        end if
        if (world%performance_timing_enabled) then
            call system_clock(t1)
            dt_cl = dble(t1 - t0) / dble(t_rate)
        end if

        if (world%performance_timing_enabled) then
            call system_clock(t_end)
            dt_t = dble(t_end - t_start) / dble(t_rate)

            ! Save in sliding window circular buffer
            perf_history(1, perf_history_idx) = dt_p
            perf_history(2, perf_history_idx) = dt_a
            perf_history(3, perf_history_idx) = dt_c
            perf_history(4, perf_history_idx) = dt_g_density
            perf_history(5, perf_history_idx) = dt_g_flows
            perf_history(6, perf_history_idx) = dt_g_smoothing
            perf_history(7, perf_history_idx) = dt_g_hep
            perf_history(8, perf_history_idx) = dt_cl
            perf_history(9, perf_history_idx) = dt_t
            
            perf_history_idx = perf_history_idx + 1
            if (perf_history_idx > PERF_WINDOW) perf_history_idx = 1
            
            world%perf_timed_ticks = world%perf_timed_ticks + 1
        end if

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

    subroutine get_t_hep(t_hep_out, nt_out)
        implicit none
        integer, intent(out) :: t_hep_out, nt_out
        t_hep_out = world%grid%t_hep
        nt_out = world%grid%nt
    end subroutine get_t_hep

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
    ! Helper: Set Use Active Time Phases
    ! =================================================================================
    subroutine set_use_active_time_phases(enabled)
        implicit none
        logical, intent(in) :: enabled
        integer :: jp
        
        world%config%use_active_time_phases = enabled
        
        if (.not. enabled) then
            world%config%tstep_start = 1
            world%config%tstep_end = world%config%Tn
        else
            do jp = 1, world%config%npops
                world%config%tstep_start(jp) = nint((world%config%tyr_start(jp) - &
                    minval(world%config%tyr_start)) / world%config%dt) + 1
                world%config%tstep_end(jp) = nint((world%config%tyr_end(jp) - &
                    minval(world%config%tyr_start)) / world%config%dt) + 1
            end do
        end if
    end subroutine set_use_active_time_phases

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
        
        if (allocated(perf_history_modules)) deallocate(perf_history_modules)
        if (count > 0) then
            allocate(perf_history_modules(count, PERF_WINDOW))
            perf_history_modules = 0.0d0
        end if
        
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
        call generate_initial_agents(world)
        
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
    subroutine get_simulation_agents(count, x, y, pop, age, gender_int, resources, children, is_pregnant_out, avg_resources_out, &
                                     ux, uy, is_dead_out, cluster_rank_out, creativity_out)
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
        
        real(8), intent(out), dimension(count) :: ux
        real(8), intent(out), dimension(count) :: uy
        integer, intent(out), dimension(count) :: is_dead_out
        integer, intent(out), dimension(count) :: cluster_rank_out
        real(8), intent(out), dimension(count) :: creativity_out
        
        real(8), allocatable :: temp_x(:), temp_y(:), temp_avg_resources(:)
        real(8), allocatable :: temp_ux(:), temp_uy(:)
        real(8), allocatable :: temp_creativity(:)
        integer, allocatable :: temp_pop(:), temp_age(:), temp_resources(:), temp_children(:), temp_is_pregnant(:)
        integer, allocatable :: temp_is_dead(:), temp_cluster_rank(:)
        character(len=1), allocatable :: temp_gender(:)
        
        integer :: actual_count, i, limit
        
        call get_alive_agents_data(world, actual_count, temp_x, temp_y, temp_pop, &
                                   temp_age, temp_gender, temp_resources, temp_children, &
                                   temp_is_pregnant, temp_avg_resources, &
                                   temp_ux, temp_uy, temp_is_dead, temp_cluster_rank, &
                                   temp_creativity)
        
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
                
                ux(i) = temp_ux(i)
                uy(i) = temp_uy(i)
                is_dead_out(i) = temp_is_dead(i)
                cluster_rank_out(i) = temp_cluster_rank(i)
                creativity_out(i) = temp_creativity(i)
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

        ! DN: Dezember 2025:
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
            ! Check start/end time for population
            if (t < world%config%tstep_start(jp) .or. t > world%config%tstep_end(jp)) cycle

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
    ! Helper: Apply yaping agent module (with tick argument)
    ! =================================================================================


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
    ! Get Dynamic State & Accumulators
    ! =================================================================================
    subroutine get_dynamic_state_stats(c_id, jp_in, k_fertility, phi_death, phi_birth, &
                                       n_alive_acc, avg_creativity) bind(c, name="get_dynamic_state_stats")
        use iso_c_binding, only: c_double, c_int
        implicit none
        integer(c_int), intent(in) :: c_id
        integer(c_int), intent(in) :: jp_in
        real(c_double), intent(out) :: k_fertility, phi_death, phi_birth, avg_creativity
        integer(c_int), intent(out) :: n_alive_acc

        integer :: jp, start_pop, end_pop, k_idx
        integer :: n_clusters, i, j, temp
        integer, allocatable :: active_ids(:)
        type(t_tick_accumulators), pointer :: acc
        type(t_dynamic_state), pointer :: dyn

        k_fertility = 0.0d0
        phi_death   = 0.0d0
        phi_birth   = 0.0d0
        n_alive_acc = 0
        avg_creativity = 0.0d0

        ! Resolve cluster (c_id is passed as the cluster rank)
        k_idx = 0
        if (c_id > 0 .and. allocated(world%cluster_store%clusters)) then
            n_clusters = world%cluster_store%n_clusters
            if (c_id <= n_clusters) then
                allocate(active_ids(n_clusters))
                do i = 1, n_clusters
                    active_ids(i) = world%cluster_store%clusters(i)%id
                end do
                ! Bubble sort
                do i = 1, n_clusters-1
                    do j = 1, n_clusters-i
                        if (active_ids(j) > active_ids(j+1)) then
                            temp = active_ids(j)
                            active_ids(j) = active_ids(j+1)
                            active_ids(j+1) = temp
                        end if
                    end do
                end do
                
                ! The c_id-th smallest ID is active_ids(c_id)
                ! Find which cluster has this ID
                do jp = 1, n_clusters
                    if (world%cluster_store%clusters(jp)%id == active_ids(c_id)) then
                        k_idx = jp
                        exit
                    end if
                end do
            end if
        end if

        if (k_idx > 0) then
            acc => world%cluster_store%clusters(k_idx)%accumulators_history(1)
            dyn => world%cluster_store%clusters(k_idx)%dynamic_state_vars
        else
            acc => world%accumulators_history(1)
            dyn => world%dynamic_state_vars
        end if

        ! Resolve population
        if (jp_in > 0 .and. jp_in <= world%config%npops) then
            start_pop = jp_in
            end_pop   = jp_in
        else
            start_pop = 1
            end_pop   = world%config%npops
        end if

        do jp = start_pop, end_pop
            ! Average K_fertility, sum others
            k_fertility = k_fertility + dyn%K_fertility(jp)
            phi_death   = phi_death + acc%phi_death_acc(jp)
            phi_birth   = phi_birth + acc%phi_birth_acc(jp)
            n_alive_acc = n_alive_acc + acc%n_alive_acc(jp)
            
            ! Accumulate creativity
            if (k_idx > 0) then
                if (allocated(world%cluster_store%clusters(k_idx)%pop_creativity_sum)) then
                    avg_creativity = avg_creativity + world%cluster_store%clusters(k_idx)%pop_creativity_sum(jp)
                end if
            else
                if (allocated(world%cluster_store%clusters)) then
                    do i = 1, world%cluster_store%n_clusters
                        if (allocated(world%cluster_store%clusters(i)%pop_creativity_sum)) then
                            avg_creativity = avg_creativity + world%cluster_store%clusters(i)%pop_creativity_sum(jp)
                        end if
                    end do
                end if
            end if
        end do

        if (start_pop /= end_pop) then
            k_fertility = k_fertility / dble(end_pop - start_pop + 1)
        end if
        
        if (n_alive_acc > 0) then
            avg_creativity = avg_creativity / dble(n_alive_acc)
        else
            avg_creativity = 0.5d0 ! baseline_creativity
        end if

    end subroutine get_dynamic_state_stats

    ! =================================================================================
    ! Cleanup Simulation
    ! =================================================================================
    subroutine cleanup_simulation()
        implicit none
        print *, "--- Python Interface: Cleaning up Simulation ---"
        call flush(6)
        call world%cleanup_world()
        call world%cluster_store%cleanup()
        
        num_active_modules = 0
        if (allocated(active_module_ids)) deallocate(active_module_ids)
        if (allocated(perf_history_modules)) deallocate(perf_history_modules)
        
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
        if (allocated(perf_history_modules)) deallocate(perf_history_modules)
    end subroutine cleanup_sim_step_3


    ! =================================================================================
    ! Clustering: Init cluster store (reads calibration from config)
    ! =================================================================================
    subroutine init_cluster_store()
        implicit none

        call world%cluster_store%init( &
            world%grid%nx, world%grid%ny, &
            world%config%cluster_update_interval, &
            world%config%human_density_smoothing_radius, &
            world%config%watershed_threshold, &
            world%config%clustering_algorithm, &
            world%config%kmeans_n_clusters, &
            world%config%dbscan_eps, &
            world%config%dbscan_minpts)
        print *, "Cluster store initialized:", &
                 world%cluster_store%nx, "x", world%cluster_store%ny, &
                 " algorithm=", world%cluster_store%algorithm, &
                 " threshold=", world%cluster_store%threshold, &
                 " kmeans_n=", world%cluster_store%kmeans_n_clusters, &
                 " dbscan_eps=", world%cluster_store%dbscan_eps, &
                 " dbscan_min=", world%cluster_store%dbscan_minpts
    end subroutine init_cluster_store

    ! =================================================================================
    ! Clustering: Run clustering (on smoothed density surface)
    !   Uses human_density_smoothed (computed every tick by update_density_and_hep_grid).
    !   Cells with smoothed density <= 0 are forced to NOISE.
    ! =================================================================================
    subroutine run_clustering(tick)
        implicit none
        integer, intent(in) :: tick

        integer :: i, j, nx, ny, k, c, gx, gy
        real(8), allocatable :: density_surface(:,:)
        integer, allocatable :: agent_counts(:,:)
        real(8), allocatable :: pos_list(:,:)
        integer :: total_alive, p, ii, idx

        nx = world%grid%nx
        ny = world%grid%ny

        ! Initialise store if needed
        if (world%cluster_store%nx == 0) then
            call init_cluster_store()
        end if

        ! Extract smoothed density surface and agent counts
        allocate(density_surface(nx, ny))
        allocate(agent_counts(nx, ny))
        do j = 1, ny
            do i = 1, nx
                density_surface(i, j) = &
                    world%grid%cell(i, j)%human_density_smoothed
                agent_counts(i, j) = &
                    world%grid%cell(i, j)%number_of_agents
            end do
        end do

        ! Extract (x,y) positions of all alive agents into a 2 x N matrix
        total_alive = 0
        do p = 1, size(world%agents, 2)
            do ii = 1, world%num_humans(p)
                if (.not. world%agents(ii, p)%is_dead) then
                    total_alive = total_alive + 1
                end if
            end do
        end do

        allocate(pos_list(2, total_alive))
        idx = 1
        do p = 1, size(world%agents, 2)
            do ii = 1, world%num_humans(p)
                if (.not. world%agents(ii, p)%is_dead) then
                    pos_list(1, idx) = world%agents(ii, p)%pos_x
                    pos_list(2, idx) = world%agents(ii, p)%pos_y
                    idx = idx + 1
                end if
            end do
        end do

        ! DEBUG: Print density surface statistics before clustering
        print *, "[DBG CLUSTERING] density_surface max=", maxval(density_surface), &
                 " min_nonzero=", minval(density_surface, mask=density_surface > 0.0d0)
        print *, "[DBG CLUSTERING] cells with density > 0:", count(density_surface > 0.0d0), &
                 " cells > threshold(", world%cluster_store%threshold, "):", &
                 count(density_surface > world%cluster_store%threshold)
        print *, "[DBG CLUSTERING] total agents on grid:", sum(agent_counts), &
                 " total_alive:", total_alive

        ! Run clustering algorithm + build clusters with persistent IDs
        ! Cells with smoothed density <= 0 will be forced to NOISE.
        call world%cluster_store%run_clustering(density_surface, &
                                           world%grid%lon_hep, &
                                           world%grid%lat_hep, &
                                           tick, &
                                           world%cluster_store%threshold, &
                                           pos_list)
        deallocate(pos_list)

        ! Count agents per cluster
        call world%cluster_store%count_agents(agent_counts)

        deallocate(density_surface, agent_counts)

        call world%cluster_store%print_summary()

        ! Mark currently clustered cells as was_clustered = .true.
        world%grid%was_clustered = .false.
        if (allocated(world%cluster_store%clusters)) then
            do k = 1, world%cluster_store%n_clusters
                do c = 1, world%cluster_store%clusters(k)%n_cells
                    gx = world%cluster_store%clusters(k)%cell_gx(c)
                    gy = world%cluster_store%clusters(k)%cell_gy(c)
                    world%grid%was_clustered(gx, gy) = .true.
                end do
            end do
        end if

        if (world%config%efficient_density_updates) then
            do j = 1, ny
                do i = 1, nx
                    if (.not. world%grid%was_clustered(i, j)) then
                        world%grid%cell(i, j)%human_density = 0.0d0
                        world%grid%cell(i, j)%human_density_smoothed = 0.0d0
                        world%grid%cell(i, j)%flow_x = 0.0d0
                        world%grid%cell(i, j)%flow_y = 0.0d0
                        if (allocated(world%grid%cell(i, j)%flow_x_pop)) then
                            world%grid%cell(i, j)%flow_x_pop = 0.0d0
                            world%grid%cell(i, j)%flow_y_pop = 0.0d0
                        end if
                    end if
                end do
            end do
        end if

    end subroutine run_clustering

    ! =================================================================================
    ! Clustering: Get cluster count and migration stats
    ! =================================================================================
    subroutine get_cluster_count(info)
        implicit none
        integer, intent(out) :: info(3)
        info(1) = world%cluster_store%n_clusters
        info(2) = 0
        info(3) = 0
    end subroutine get_cluster_count

    ! =================================================================================
    ! Clustering: Get info for cluster k (1-indexed)
    ! =================================================================================
    subroutine get_cluster_info(k, jp_in, iinfo, rinfo)
        implicit none
        integer, intent(in)  :: k
        integer, intent(in), optional :: jp_in
        integer, intent(out) :: iinfo(3)
        real(8), intent(out) :: rinfo(6)
        
        integer :: pop_idx

        if (k >= 1 .and. k <= world%cluster_store%n_clusters) then
            iinfo(1) = world%cluster_store%clusters(k)%id
            iinfo(2) = world%cluster_store%clusters(k)%n_cells
            
            pop_idx = 0
            if (present(jp_in)) pop_idx = jp_in
            
            if (pop_idx > 0 .and. pop_idx <= world%config%npops) then
                iinfo(3) = world%cluster_store%clusters(k)%accumulators_history(1)%n_alive_acc(pop_idx)
                rinfo(1) = world%cluster_store%clusters(k)%centroid_x
                rinfo(2) = world%cluster_store%clusters(k)%centroid_y
                rinfo(3) = world%cluster_store%clusters(k)%pop_hep_sum(pop_idx)
                rinfo(4) = world%cluster_store%clusters(k)%MC_cl(pop_idx)
                if (allocated(world%cluster_store%clusters(k)%MC_cl_AV)) then
                    if (world%cluster_store%clusters(k)%MC_cl_AV(pop_idx) >= 0.0d0) then
                        rinfo(5) = world%cluster_store%clusters(k)%MC_cl_AV(pop_idx)
                    else
                        rinfo(5) = world%cluster_store%clusters(k)%MC_cl(pop_idx)
                    end if
                else
                    rinfo(5) = world%cluster_store%clusters(k)%MC_cl(pop_idx)
                end if
                rinfo(6) = world%cluster_store%clusters(k)%MC_cl_shared  ! Shared MC (NaN/-1 if not in shared MC mode)
            else
                iinfo(3) = sum(world%cluster_store%clusters(k)%accumulators_history(1)%n_alive_acc)
                rinfo(1) = world%cluster_store%clusters(k)%centroid_x
                rinfo(2) = world%cluster_store%clusters(k)%centroid_y
                rinfo(3) = world%cluster_store%clusters(k)%hep_sum
                rinfo(4) = world%cluster_store%clusters(k)%MC_cl_total
                if (allocated(world%cluster_store%clusters(k)%MC_cl_AV)) then
                    rinfo(5) = 0.0d0
                    do pop_idx = 1, world%config%npops
                        if (world%cluster_store%clusters(k)%MC_cl_AV(pop_idx) >= 0.0d0) then
                            rinfo(5) = rinfo(5) + world%cluster_store%clusters(k)%MC_cl_AV(pop_idx)
                        else
                            rinfo(5) = rinfo(5) + world%cluster_store%clusters(k)%MC_cl(pop_idx)
                        end if
                    end do
                else
                    rinfo(5) = world%cluster_store%clusters(k)%MC_cl_total
                end if
                rinfo(6) = world%cluster_store%clusters(k)%MC_cl_shared  ! Shared MC (NaN/-1 if not in shared MC mode)
            end if
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

        if (allocated(world%cluster_store%cell_cluster_map)) then
            map_out = world%cluster_store%cell_cluster_map
        else
            map_out = -2
        end if
    end subroutine get_cell_cluster_map

    ! =================================================================================
    ! Clustering: Set the active clustering algorithm
    ! =================================================================================
    subroutine set_clustering_algorithm(alg_id)
        implicit none
        integer, intent(in) :: alg_id
        world%config%clustering_algorithm = alg_id
        world%cluster_store%algorithm = alg_id
        print *, "Clustering algorithm set to:", alg_id
    end subroutine set_clustering_algorithm

    ! =================================================================================
    ! Clustering: Set K-Means N Clusters 
    ! =================================================================================
    subroutine set_kmeans_clusters(k) bind(c, name="set_kmeans_clusters")
        use iso_c_binding, only: c_int
        implicit none
        integer(c_int), intent(in), value :: k

        world%config%kmeans_n_clusters = k
        if (allocated(world%cluster_store%cell_cluster_map)) then
            world%cluster_store%kmeans_n_clusters = k
        end if
    end subroutine set_kmeans_clusters

    ! =================================================================================
    ! Clustering: Set K-Means Auto Smoothing Radius
    ! =================================================================================


    ! =================================================================================
    ! Clustering: Set DBSCAN Epsilon (neighbourhood radius)
    ! =================================================================================
    subroutine set_dbscan_eps(eps) bind(c, name="set_dbscan_eps")
        use iso_c_binding, only: c_double
        implicit none
        real(c_double), intent(in), value :: eps

        world%config%dbscan_eps = eps
        if (allocated(world%cluster_store%cell_cluster_map)) then
            world%cluster_store%dbscan_eps = eps
        end if
    end subroutine set_dbscan_eps

    ! =================================================================================
    ! Clustering: Set DBSCAN MinPts (minimum points per cluster)
    ! =================================================================================
    subroutine set_dbscan_minpts(minpts) bind(c, name="set_dbscan_minpts")
        use iso_c_binding, only: c_int
        implicit none
        integer(c_int), intent(in), value :: minpts

        world%config%dbscan_minpts = minpts
        if (allocated(world%cluster_store%cell_cluster_map)) then
            world%cluster_store%dbscan_minpts = minpts
        end if
    end subroutine set_dbscan_minpts

    ! =================================================================================
    ! Clustering: Get the active clustering algorithm
    ! =================================================================================
    subroutine get_clustering_algorithm(alg_id)
        implicit none
        integer, intent(out) :: alg_id
        alg_id = world%cluster_store%algorithm
    end subroutine get_clustering_algorithm

    ! =================================================================================
    ! Age Distribution Interface
    ! =================================================================================
    subroutine set_age_distribution_interface(dist, n)
        implicit none
        integer, intent(in) :: n
        real(8), dimension(n), intent(in) :: dist
        
        if (allocated(world%config%age_distribution)) deallocate(world%config%age_distribution)
        allocate(world%config%age_distribution(n))
        
        world%config%age_distribution = dist
        world%config%age_distribution_set = .true.
        
        print *, "Age distribution set. Size: ", n
    end subroutine set_age_distribution_interface
    
    subroutine init_sim_step_apply_age_dist()
        implicit none
        
        print *, "--- Step Age Dist: Apply ---"
        call apply_age_distribution(world)
        print *, "--- Age Distribution Applied ---"
    end subroutine init_sim_step_apply_age_dist

    ! =================================================================================
    ! Expose Smoothing Algorithms and Grid Access for Python Testing
    ! =================================================================================
    subroutine get_grid_density(nx, ny, density_array)
        implicit none
        integer, intent(in) :: nx, ny
        real(8), intent(out) :: density_array(nx, ny)
        integer :: i, j
        !f2py depend(nx, ny) density_array
        
        do i = 1, nx
            do j = 1, ny
                density_array(i, j) = world%grid%cell(i, j)%human_density
            end do
        end do
    end subroutine get_grid_density

    subroutine apply_find_local_maxima(input_surface, nx, ny, threshold, labels, n_maxima)
        use mod_watershed, only: find_local_maxima
        implicit none
        integer, intent(in) :: nx, ny
        real(8), intent(in) :: threshold
        real(8), intent(in) :: input_surface(nx, ny)
        integer, intent(out) :: labels(nx, ny)
        integer, intent(out) :: n_maxima
        !f2py depend(nx, ny) input_surface, labels
        call find_local_maxima(input_surface, nx, ny, threshold, labels, n_maxima)
    end subroutine apply_find_local_maxima


    ! =================================================================================
    ! Dead Agent Export: Set the forget_dead_agents flag
    !   When forget is .false., compact_agents will park dead agents.
    ! =================================================================================
    subroutine set_forget_dead_agents(forget)
        implicit none
        logical, intent(in) :: forget
        world%forget_dead_agents = forget
        if (forget) then
            print *, "Dead agent archiving: DISABLED (forget_dead_agents = .true.)"
        else
            print *, "Dead agent archiving: ENABLED (forget_dead_agents = .false.)"
        endif
    end subroutine set_forget_dead_agents

    ! =================================================================================
    ! Dead Agent Export: Get the count of currently parked dead agents
    ! =================================================================================
    subroutine get_dead_agents_count(count)
        implicit none
        integer, intent(out) :: count
        count = world%num_dead_agents_export
    end subroutine get_dead_agents_count

    ! =================================================================================
    ! Dead Agent Export: Get dead agent data arrays for Python
    !   Returns the same fields as get_simulation_agents for consistency.
    ! =================================================================================
    subroutine get_dead_simulation_agents(count, id_out, x, y, pop, age, gender_int, death_tick, birth_tick, father_id, mother_id)
        implicit none
        integer, intent(in) :: count
        integer, intent(out), dimension(count) :: id_out
        real(8), intent(out), dimension(count) :: x
        real(8), intent(out), dimension(count) :: y
        integer, intent(out), dimension(count) :: pop
        integer, intent(out), dimension(count) :: age
        integer, intent(out), dimension(count) :: gender_int
        integer, intent(out), dimension(count) :: death_tick
        integer, intent(out), dimension(count) :: birth_tick
        integer, intent(out), dimension(count) :: father_id
        integer, intent(out), dimension(count) :: mother_id

        integer, allocatable :: temp_id(:)
        real(8), allocatable :: temp_x(:), temp_y(:)
        integer, allocatable :: temp_pop(:), temp_age(:), temp_death_tick(:)
        integer, allocatable :: temp_birth_tick(:), temp_father_id(:), temp_mother_id(:)
        character(len=1), allocatable :: temp_gender(:)

        integer :: actual_count, i, limit

        call get_dead_agents_data(world, actual_count, temp_id, temp_x, temp_y, temp_pop, &
                                 temp_age, temp_gender, temp_death_tick, temp_birth_tick, temp_father_id, temp_mother_id)

        if (allocated(temp_x)) then
            limit = min(count, actual_count)
            do i = 1, limit
                id_out(i) = temp_id(i)
                x(i) = temp_x(i)
                y(i) = temp_y(i)
                pop(i) = temp_pop(i)
                age(i) = temp_age(i)
                if (temp_gender(i) == 'M') then
                    gender_int(i) = 1
                else
                    gender_int(i) = 0
                end if
                death_tick(i) = temp_death_tick(i)
                birth_tick(i) = temp_birth_tick(i)
                father_id(i) = temp_father_id(i)
                mother_id(i) = temp_mother_id(i)
            end do
        end if

    end subroutine get_dead_simulation_agents

    ! =================================================================================
    ! Dead Agent Export: Clear the parked dead agents buffer
    !   Should be called by Python after extracting the data.
    ! =================================================================================
    subroutine clear_dead_agents()
        implicit none
        world%num_dead_agents_export = 0
    end subroutine clear_dead_agents

    ! =================================================================================
    ! Performance Timing: Set whether timing is enabled
    ! =================================================================================
    subroutine set_performance_timing_enabled(enabled)
        implicit none
        logical, intent(in) :: enabled
        world%performance_timing_enabled = enabled
        world%perf_accumulated_time = 0.0d0
        world%perf_timed_ticks = 0
        perf_history = 0.0d0
        if (allocated(perf_history_modules)) perf_history_modules = 0.0d0
        perf_history_idx = 1
    end subroutine set_performance_timing_enabled

    ! =================================================================================
    ! Verbose step debug: toggle per-tick [DBG STEP] prints (off by default)
    ! =================================================================================
    subroutine set_verbose_step_debug(enabled)
        implicit none
        logical, intent(in) :: enabled
        world%verbose_step_debug = enabled
    end subroutine set_verbose_step_debug

    ! =================================================================================
    ! Performance Timing: Get current accumulated average timing values
    ! =================================================================================
    subroutine get_performance_stats(count, p_avg, a_avg, c_avg, &
        g_density_avg, g_flows_avg, g_smoothing_avg, g_hep_avg, cl_avg, t_avg)
        implicit none
        integer, intent(out) :: count
        real(8), intent(out) :: p_avg, a_avg, c_avg, &
            g_density_avg, g_flows_avg, g_smoothing_avg, g_hep_avg, cl_avg, t_avg
        
        integer :: n_samples, i
        real(8) :: sums(9)
        
        count = world%perf_timed_ticks
        n_samples = min(count, PERF_WINDOW)
        
        if (n_samples > 0) then
            sums = 0.0d0
            do i = 1, n_samples
                sums(1) = sums(1) + perf_history(1, i)
                sums(2) = sums(2) + perf_history(2, i)
                sums(3) = sums(3) + perf_history(3, i)
                sums(4) = sums(4) + perf_history(4, i)
                sums(5) = sums(5) + perf_history(5, i)
                sums(6) = sums(6) + perf_history(6, i)
                sums(7) = sums(7) + perf_history(7, i)
                sums(8) = sums(8) + perf_history(8, i)
                sums(9) = sums(9) + perf_history(9, i)
            end do
            
            p_avg = sums(1) / dble(n_samples)
            a_avg = sums(2) / dble(n_samples)
            c_avg = sums(3) / dble(n_samples)
            g_density_avg = sums(4) / dble(n_samples)
            g_flows_avg = sums(5) / dble(n_samples)
            g_smoothing_avg = sums(6) / dble(n_samples)
            g_hep_avg = sums(7) / dble(n_samples)
            cl_avg = sums(8) / dble(n_samples)
            t_avg = sums(9) / dble(n_samples)
        else
            p_avg = 0.0d0
            a_avg = 0.0d0
            c_avg = 0.0d0
            g_density_avg = 0.0d0
            g_flows_avg = 0.0d0
            g_smoothing_avg = 0.0d0
            g_hep_avg = 0.0d0
            cl_avg = 0.0d0
            t_avg = 0.0d0
        end if
    end subroutine get_performance_stats

    ! =================================================================================
    ! Get current active modules count
    ! =================================================================================
    subroutine get_active_modules_count(count)
        implicit none
        integer, intent(out) :: count
        count = num_active_modules
    end subroutine get_active_modules_count

    ! =================================================================================
    ! Get active module performance statistics
    ! =================================================================================
    subroutine get_active_modules_performance_stats(n_modules, module_ids, module_avgs)
        implicit none
        integer, intent(in) :: n_modules
        integer, intent(out), dimension(n_modules) :: module_ids
        real(8), intent(out), dimension(n_modules) :: module_avgs

        integer :: n_samples, i, jp
        real(8) :: sum_val

        n_samples = min(world%perf_timed_ticks, PERF_WINDOW)

        do jp = 1, min(n_modules, num_active_modules)
            module_ids(jp) = active_module_ids(jp)
            if (n_samples > 0 .and. allocated(perf_history_modules)) then
                sum_val = 0.0d0
                do i = 1, n_samples
                    sum_val = sum_val + perf_history_modules(jp, i)
                end do
                module_avgs(jp) = sum_val / dble(n_samples)
            else
                module_avgs(jp) = 0.0d0
            end if
        end do

        ! If requested more than active, pad with 0
        if (n_modules > num_active_modules) then
            do jp = num_active_modules + 1, n_modules
                module_ids(jp) = 0
                module_avgs(jp) = 0.0d0
            end do
        end if
    end subroutine get_active_modules_performance_stats

    ! =================================================================================
    ! Runtime toggle for allow_across_populations (temporal interbreeding)
    ! =================================================================================
    subroutine set_allow_across_populations(val)
        implicit none
        logical, intent(in) :: val
        world%config%allow_across_populations = val
        if (val) then
            print *, "[Interbreeding] allow_across_populations = .TRUE."
        else
            print *, "[Interbreeding] allow_across_populations = .FALSE."
        end if
    end subroutine set_allow_across_populations

end module mod_python_interface

