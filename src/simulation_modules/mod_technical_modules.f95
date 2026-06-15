module mod_technical_modules
    use mod_agent_world
    use mod_constants
    use mod_config
    use mod_hashmap
    use mod_rnorm
    use mod_grid_id
    use mod_calculations
    use mod_watershed, only: smooth_box_filter
    
    implicit none

contains

    subroutine update_agent_age(agent_ptr)
        type(Agent), pointer, intent(inout) :: agent_ptr
        integer :: jp, c_idx
        type(t_tick_accumulators), pointer :: accumulators
        
        ! Update age in ticks and years
        agent_ptr%age_ticks = agent_ptr%age_ticks + 1
        agent_ptr%age_years = ticks_in_years(agent_ptr%age_ticks, agent_ptr%world%config%dt)

        ! Update ticks since last birth and is_pregnant for females

        if (agent_ptr%gender == 'F') then
            agent_ptr%ticks_since_last_birth = agent_ptr%ticks_since_last_birth + 1
        endif

        if (agent_ptr%is_pregnant > 0 ) then
            print*, "Warning: pregnancy should not exceed one tick."
            agent_ptr%is_pregnant = agent_ptr%is_pregnant + 1
        end if

        ! Accumulate population count for the current tick (used by fertility scale logic)

        jp = agent_ptr%population
        if (allocated(agent_ptr%world%cluster_store%cell_cluster_idx) .and. &
            agent_ptr%gx >= 1 .and. agent_ptr%gx <= agent_ptr%world%grid%nx .and. &
            agent_ptr%gy >= 1 .and. agent_ptr%gy <= agent_ptr%world%grid%ny) then
            c_idx = agent_ptr%world%cluster_store%cell_cluster_idx(agent_ptr%gx, agent_ptr%gy)
        else
            c_idx = 0
        end if
    
        accumulators => agent_ptr%world%accumulators_history(1)

        accumulators%n_alive_acc(jp) = accumulators%n_alive_acc(jp) + 1

        if (c_idx > 0 .and. c_idx <= agent_ptr%world%cluster_store%n_clusters) then
            accumulators => agent_ptr%world%cluster_store%clusters(c_idx)%accumulators_history(1)
            accumulators%n_alive_acc(jp) = accumulators%n_alive_acc(jp) + 1
        endif
        
    end subroutine update_agent_age

    ! =========================================================================
    ! SUBROUTINE: update_density_and_hep_grid (GRID-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE PER TICK on the entire grid automatically (permanent module).
    ! Updates the base density computations across all cells.
    ! Calculates basic agents counts per cell, smoothed density using
    ! smooth_box_filter (human_density_smoothing_radius), calculates flow fields,
    ! and sets basic hep_av.
    !
    ! =========================================================================
    subroutine update_density_and_hep_grid(w, t, t_density_out, t_flows_out, t_smoothing_out, t_hep_out)
        use mod_constants, only: deg_km
        use mod_read_inputs, only: load_hep_chunk_from_file
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t
        real(8), intent(out), optional :: t_density_out, t_flows_out, t_smoothing_out, t_hep_out
        
        integer :: jp
        type(Grid), pointer :: grid
        integer :: i, j, k, id, nx, ny, local_idx, gx, gy
        real(8) :: flow_x_sum, flow_y_sum
        type(Agent), pointer :: agent_ptr
        real(8), allocatable :: raw_density(:,:), smoothed(:,:)
        real(8), allocatable :: flow_x_accum(:,:), flow_y_accum(:,:)
        integer :: iter

        ! Timing variables
        integer(kind=8) :: tc0, tc1, tc_rate
        real(8) :: dt_dens, dt_flow, dt_smooth, dt_h

        dt_dens = 0.0d0
        dt_flow = 0.0d0
        dt_smooth = 0.0d0
        dt_h = 0.0d0
        tc_rate = 1

        grid => w%grid
        nx = grid%nx
        ny = grid%ny

        ! Reset flows in all cells
        do j = 1, ny
            do i = 1, nx
                grid%cell(i,j)%flow_x = 0.0d0
                grid%cell(i,j)%flow_y = 0.0d0
            end do
        end do

        ! We process each population
        do jp = 1, w%config%npops
        
            ! 1. update the Pure Density
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            call grid%update_density_pure()
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_dens = dt_dens + dble(tc1 - tc0) / dble(tc_rate)
            end if
            
            ! 2. Calculate agent flows
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            allocate(flow_x_accum(nx, ny), flow_y_accum(nx, ny))
            flow_x_accum = 0.0d0
            flow_y_accum = 0.0d0
            
            do k = 1, w%num_humans(jp)
                if (w%agents(k, jp)%is_dead) cycle
                gx = w%agents(k, jp)%gx
                gy = w%agents(k, jp)%gy
                if (gx >= 1 .and. gx <= nx .and. gy >= 1 .and. gy <= ny) then
                    flow_x_accum(gx, gy) = flow_x_accum(gx, gy) + w%agents(k, jp)%ux
                    flow_y_accum(gx, gy) = flow_y_accum(gx, gy) + w%agents(k, jp)%uy
                end if
            end do

            do j = 1, ny
                do i = 1, nx
                    if (grid%cell(i,j)%area > 0.0d0) then
                        grid%cell(i,j)%flow_x_pop(jp) = flow_x_accum(i,j) * 100.0d0 / grid%cell(i,j)%area
                        grid%cell(i,j)%flow_y_pop(jp) = flow_y_accum(i,j) * 100.0d0 / grid%cell(i,j)%area
                        
                        grid%cell(i,j)%flow_x = grid%cell(i,j)%flow_x + grid%cell(i,j)%flow_x_pop(jp)
                        grid%cell(i,j)%flow_y = grid%cell(i,j)%flow_y + grid%cell(i,j)%flow_y_pop(jp)
                    else
                        grid%cell(i,j)%flow_x_pop(jp) = 0.0d0
                        grid%cell(i,j)%flow_y_pop(jp) = 0.0d0
                    end if
                end do
            end do
            deallocate(flow_x_accum, flow_y_accum)
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_flow = dt_flow + dble(tc1 - tc0) / dble(tc_rate)
            endif
                
            ! 3. update smoothed density (human_density_smoothed)
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            allocate(raw_density(nx, ny), smoothed(nx, ny))
            do j = 1, ny
                do i = 1, nx
                    raw_density(i, j) = grid%cell(i, j)%human_density
                end do
            end do

            if (w%config%human_density_smoothing_radius > 0) then
                smoothed = raw_density
                do iter = 1, w%config%human_density_smoothing_iterations
                    call smooth_box_filter(smoothed, nx, ny, &
                        w%config%human_density_smoothing_radius, raw_density)
                    smoothed = raw_density
                end do
            else
                smoothed = raw_density
            end if

            ! update smoothed density in cells 
            do j = 1, ny
                do i = 1, nx
                    grid%cell(i, j)%human_density_smoothed = smoothed(i, j)
                end do
            end do

            deallocate(raw_density, smoothed)
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_smooth = dt_smooth + dble(tc1 - tc0) / dble(tc_rate)
            end if
                
            ! 4. Base HEP transfer (hep_av = hep) with dynamic chunked loading
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            if (grid%t_hep < grid%chunk_start_t .or. grid%t_hep > grid%chunk_end_t) then
                 call load_hep_chunk_from_file(grid, grid%t_hep)
            end if
            local_idx = grid%t_hep - grid%chunk_start_t + 1
            grid%hep_av(:,:,jp) = grid%hep(:,:,jp, local_idx)

            ! 4b. Population pressure scaling (legacy Weibull equation)
            if (w%config%with_pop_pressure) then
                call apply_pop_pressure_to_hep_av(grid, jp, &
                    w%config%rho_max(jp), w%config%eta(jp), w%config%epsilon(jp))
            end if
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_h = dt_h + dble(tc1 - tc0) / dble(tc_rate)
            end if
                
        end do

        if (present(t_density_out)) t_density_out = dt_dens
        if (present(t_flows_out)) t_flows_out = dt_flow
        if (present(t_smoothing_out)) t_smoothing_out = dt_smooth
        if (present(t_hep_out)) t_hep_out = dt_h
                
    end subroutine update_density_and_hep_grid


    subroutine update_density_and_hep_grid_efficient(w, t, t_density_out, t_flows_out, t_smoothing_out, t_hep_out)
        use mod_constants, only: deg_km
        use mod_read_inputs, only: load_hep_chunk_from_file
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t
        real(8), intent(out), optional :: t_density_out, t_flows_out, t_smoothing_out, t_hep_out
        
        integer :: jp, c, k, id, nx, ny, local_idx
        type(Grid), pointer :: grid
        real(8) :: flow_x_sum, flow_y_sum
        type(Agent), pointer :: agent_ptr
        integer :: i, j, iter
        integer :: gx, gy, ni, nj, di, dj, count
        real(8) :: total
        logical :: reclustering_tick
        real(8), allocatable :: raw_density(:,:), smoothed(:,:), flow_x_accum(:,:), flow_y_accum(:,:)

        ! Timing variables
        integer(kind=8) :: tc0, tc1, tc_rate
        real(8) :: dt_dens, dt_flow, dt_smooth, dt_h

        dt_dens = 0.0d0
        dt_flow = 0.0d0
        dt_smooth = 0.0d0
        dt_h = 0.0d0
        tc_rate = 1
        
        grid => w%grid
        nx = grid%nx
        ny = grid%ny
        
        reclustering_tick = (mod(t, w%cluster_store%update_interval) == 0)
        
        ! 1. On re-clustering ticks, clean up cells that transitioned OUT of clusters
        if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
        if (reclustering_tick) then
            do j = 1, ny
                do i = 1, nx
                    if (grid%was_clustered(i, j)) then
                        ! If this cell is no longer part of any cluster, reset its state
                        if (w%cluster_store%get_cluster_of_cell(i, j) < 0) then
                            grid%cell(i, j)%human_density = 0.0d0
                            grid%cell(i, j)%human_density_smoothed = 0.0d0
                            grid%cell(i, j)%flow_x = 0.0d0
                            grid%cell(i, j)%flow_y = 0.0d0
                            grid%was_clustered(i, j) = .false.
                        end if
                    end if
                end do
            end do
            
            ! Mark currently clustered cells as was_clustered = .true.
            if (allocated(w%cluster_store%clusters)) then
                do k = 1, w%cluster_store%n_clusters
                    do c = 1, w%cluster_store%clusters(k)%n_cells
                        gx = w%cluster_store%clusters(k)%cell_gx(c)
                        gy = w%cluster_store%clusters(k)%cell_gy(c)
                        grid%was_clustered(gx, gy) = .true.
                    end do
                end do
            end if
        end if
        if (w%performance_timing_enabled) then
            call system_clock(tc1)
            dt_dens = dt_dens + dble(tc1 - tc0) / dble(tc_rate)
        end if
        
        ! 2. Process density and flows strictly for active cluster cells
        if (allocated(w%cluster_store%clusters) .and. w%cluster_store%n_clusters > 0) then
            
            ! 2.1 First pass: Compute raw density in clustered cells
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            do k = 1, w%cluster_store%n_clusters
                do c = 1, w%cluster_store%clusters(k)%n_cells
                    gx = w%cluster_store%clusters(k)%cell_gx(c)
                    gy = w%cluster_store%clusters(k)%cell_gy(c)
                    
                    if (grid%cell(gx, gy)%area > 0.0d0) then
                        grid%cell(gx, gy)%human_density = &
                            dble(grid%cell(gx, gy)%number_of_agents) * 100.0d0 / grid%cell(gx, gy)%area
                    else
                        grid%cell(gx, gy)%human_density = 0.0d0
                    end if
                end do
            end do
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_dens = dt_dens + dble(tc1 - tc0) / dble(tc_rate)
            end if
            
            ! 2.2 Compute flow in clustered cells
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            do k = 1, w%cluster_store%n_clusters
                do c = 1, w%cluster_store%clusters(k)%n_cells
                    gx = w%cluster_store%clusters(k)%cell_gx(c)
                    gy = w%cluster_store%clusters(k)%cell_gy(c)
                    grid%cell(gx, gy)%flow_x = 0.0d0
                    grid%cell(gx, gy)%flow_y = 0.0d0
                    grid%cell(gx, gy)%flow_x_pop = 0.0d0
                    grid%cell(gx, gy)%flow_y_pop = 0.0d0
                end do
            end do

            do jp = 1, w%config%npops
                allocate(flow_x_accum(nx, ny), flow_y_accum(nx, ny))
                flow_x_accum = 0.0d0
                flow_y_accum = 0.0d0
                
                do i = 1, w%num_humans(jp)
                    if (w%agents(i, jp)%is_dead) cycle
                    gx = w%agents(i, jp)%gx
                    gy = w%agents(i, jp)%gy
                    if (gx >= 1 .and. gx <= nx .and. gy >= 1 .and. gy <= ny) then
                        if (w%cluster_store%get_cluster_of_cell(gx, gy) >= 0) then
                            flow_x_accum(gx, gy) = flow_x_accum(gx, gy) + w%agents(i, jp)%ux
                            flow_y_accum(gx, gy) = flow_y_accum(gx, gy) + w%agents(i, jp)%uy
                        end if
                    end if
                end do

                do k = 1, w%cluster_store%n_clusters
                    do c = 1, w%cluster_store%clusters(k)%n_cells
                        gx = w%cluster_store%clusters(k)%cell_gx(c)
                        gy = w%cluster_store%clusters(k)%cell_gy(c)
                        
                        if (grid%cell(gx, gy)%area > 0.0d0) then
                            grid%cell(gx, gy)%flow_x_pop(jp) = &
                                flow_x_accum(gx, gy) * 100.0d0 / grid%cell(gx, gy)%area
                            grid%cell(gx, gy)%flow_y_pop(jp) = &
                                flow_y_accum(gx, gy) * 100.0d0 / grid%cell(gx, gy)%area
                            
                            grid%cell(gx, gy)%flow_x = grid%cell(gx, gy)%flow_x + grid%cell(gx, gy)%flow_x_pop(jp)
                            grid%cell(gx, gy)%flow_y = grid%cell(gx, gy)%flow_y + grid%cell(gx, gy)%flow_y_pop(jp)
                        else
                            grid%cell(gx, gy)%flow_x_pop(jp) = 0.0d0
                            grid%cell(gx, gy)%flow_y_pop(jp) = 0.0d0
                        end if
                    end do
                end do
                deallocate(flow_x_accum, flow_y_accum)
            end do
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_flow = dt_flow + dble(tc1 - tc0) / dble(tc_rate)
            end if
            
            ! 2.3 Second pass: Apply box-smoothing strictly to clustered cells
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            if (w%config%human_density_smoothing_radius > 0) then
                allocate(raw_density(nx, ny), smoothed(nx, ny))
                raw_density = 0.0d0
                
                ! Load raw densities of clustered cells
                do k = 1, w%cluster_store%n_clusters
                    do c = 1, w%cluster_store%clusters(k)%n_cells
                        gx = w%cluster_store%clusters(k)%cell_gx(c)
                        gy = w%cluster_store%clusters(k)%cell_gy(c)
                        raw_density(gx, gy) = grid%cell(gx, gy)%human_density
                    end do
                end do
                
                smoothed = raw_density
                
                do iter = 1, w%config%human_density_smoothing_iterations
                    do k = 1, w%cluster_store%n_clusters
                        do c = 1, w%cluster_store%clusters(k)%n_cells
                            gx = w%cluster_store%clusters(k)%cell_gx(c)
                            gy = w%cluster_store%clusters(k)%cell_gy(c)
                            
                            total = 0.0d0
                            count = 0
                            
                            do dj = -w%config%human_density_smoothing_radius, &
                                    w%config%human_density_smoothing_radius
                                nj = gy + dj
                                if (nj < 1 .or. nj > ny) cycle
                                
                                do di = -w%config%human_density_smoothing_radius, &
                                        w%config%human_density_smoothing_radius
                                    ni = gx + di
                                    if (ni < 1 .or. ni > nx) cycle
                                    
                                    total = total + smoothed(ni, nj)
                                    count = count + 1
                                end do
                            end do
                            
                            if (count > 0) then
                                raw_density(gx, gy) = total / dble(count)
                            else
                                raw_density(gx, gy) = smoothed(gx, gy)
                            end if
                        end do
                    end do
                    smoothed = raw_density
                end do
                
                ! Save back smoothed density
                do k = 1, w%cluster_store%n_clusters
                    do c = 1, w%cluster_store%clusters(k)%n_cells
                        gx = w%cluster_store%clusters(k)%cell_gx(c)
                        gy = w%cluster_store%clusters(k)%cell_gy(c)
                        grid%cell(gx, gy)%human_density_smoothed = smoothed(gx, gy)
                    end do
                end do
                
                deallocate(raw_density, smoothed)
            else
                ! No smoothing
                do k = 1, w%cluster_store%n_clusters
                    do c = 1, w%cluster_store%clusters(k)%n_cells
                        gx = w%cluster_store%clusters(k)%cell_gx(c)
                        gy = w%cluster_store%clusters(k)%cell_gy(c)
                        grid%cell(gx, gy)%human_density_smoothed = grid%cell(gx, gy)%human_density
                    end do
                end do
            end if
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_smooth = dt_smooth + dble(tc1 - tc0) / dble(tc_rate)
            end if
            
            ! 2.4 Third pass: Copy HEP data strictly for clustered cells
            if (w%performance_timing_enabled) call system_clock(tc0, tc_rate)
            if (grid%t_hep < grid%chunk_start_t .or. grid%t_hep > grid%chunk_end_t) then
                 call load_hep_chunk_from_file(grid, grid%t_hep)
            end if
            local_idx = grid%t_hep - grid%chunk_start_t + 1
            
            do jp = 1, w%config%npops
                do k = 1, w%cluster_store%n_clusters
                    do c = 1, w%cluster_store%clusters(k)%n_cells
                        gx = w%cluster_store%clusters(k)%cell_gx(c)
                        gy = w%cluster_store%clusters(k)%cell_gy(c)
                        grid%hep_av(gx, gy, jp) = grid%hep(gx, gy, jp, local_idx)
                    end do
                end do

                ! 2.5 Population pressure scaling (legacy Weibull equation, clustered cells only)
                if (w%config%with_pop_pressure) then
                    do k = 1, w%cluster_store%n_clusters
                        do c = 1, w%cluster_store%clusters(k)%n_cells
                            gx = w%cluster_store%clusters(k)%cell_gx(c)
                            gy = w%cluster_store%clusters(k)%cell_gy(c)
                            call apply_pop_pressure_to_cell(grid, gx, gy, jp, &
                                w%config%rho_max(jp), w%config%eta(jp), w%config%epsilon(jp))
                        end do
                    end do
                end if
            end do
            if (w%performance_timing_enabled) then
                call system_clock(tc1)
                dt_h = dt_h + dble(tc1 - tc0) / dble(tc_rate)
            end if
            
        end if
        
        if (present(t_density_out)) t_density_out = dt_dens
        if (present(t_flows_out)) t_flows_out = dt_flow
        if (present(t_smoothing_out)) t_smoothing_out = dt_smooth
        if (present(t_hep_out)) t_hep_out = dt_h
        
    end subroutine update_density_and_hep_grid_efficient


    subroutine update_dynamic_state_variables(w)
        ! we want to move the functions called here in a seperate file once we have more
        ! dynamic state vars to update. For now ill leave it here
        ! DN 28.04.26 
        use mod_reviewed_modules, only: update_macroscopic_fertility_scale
        implicit none
        class(world_container), target, intent(inout) :: w


        call update_macroscopic_fertility_scale(w)


    end subroutine update_dynamic_state_variables

    ! =================================================================
    ! SUBROUTINE: realise_births
    !
    ! NEW REALISE BIRTH FUNCTION THAT MODELS PREGNANCIES IMPLICITLY WITH TIMESINCELASTBIRTH.
    !
    ! Handles the birth of new agents from pregnant females.
    ! Logic:
    !   - Checks pregnancy > 0.
    !   - If yes: spawns new agent (age=0) at mother's location.
    ! =================================================================
    subroutine realise_births(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        type(Agent) :: new_agent
        type(Agent), pointer :: father_ptr
        type(Agent), pointer :: mother_fresh
        type(world_container), pointer :: world_ptr

        integer :: parent_one_id, parent_two_id, population

        if (current_agent%is_pregnant == 0) then
            ! agent is not pregnant
            return
        endif

        parent_one_id = current_agent%id
        parent_two_id = current_agent%father_of_unborn_child
        population = current_agent%population

        ! Save world pointer BEFORE the birth call.
        ! world itself is never moved (save variable in the interface) —
        ! only world%agents is reallocated inside add_agent_to_array_hash.
        ! After the resize current_agent is a dangling pointer, but world_ptr is not.
        world_ptr => current_agent%world

        ! birth occurs
        father_ptr => get_agent(parent_two_id, world_ptr)

        if (.not. associated(father_ptr)) then
            ! Father died during pregnancy.
            father_ptr => current_agent
        endif

        new_agent = generate_agent_born(world_ptr, current_agent, father_ptr)

        ! This call may trigger resize_agent_array_hash, which deallocates the
        ! old world%agents block. After it returns, current_agent is DANGLING.
        call add_agent_to_array_hash(world_ptr, new_agent, new_agent%population)

        ! FIX: do NOT use current_agent after this point — it may be dangling.
        ! Re-fetch the mother from the (now valid, possibly new) world%agents array.
        mother_fresh => get_agent(parent_one_id, world_ptr)

        if (associated(mother_fresh)) then
            mother_fresh%is_pregnant = 0
        else
            print*, "Warning: realise_births could not re-fetch mother id=", parent_one_id, " after birth"
        end if

    end subroutine realise_births

    ! =========================================================================
    ! SUBROUTINE: apply_module_to_clusters
    !
    ! Iterates over all clusters in the cluster_store and calls
    ! a user-provided subroutine on each cluster.
    !
    ! Pattern mirrors apply_module_to_agents but operates on cluster_t.
    ! =========================================================================
    subroutine apply_module_to_clusters(func, w)
        use mod_clustering, only: cluster_t
        implicit none

        interface
            subroutine func(cluster, w)
                use mod_clustering, only: cluster_t
                use mod_agent_world, only: world_container
                type(cluster_t), intent(inout) :: cluster
                class(world_container), target, intent(inout) :: w
            end subroutine func
        end interface

        class(world_container), target, intent(inout) :: w
        integer :: k

        if (.not. allocated(w%cluster_store%clusters)) return
        if (w%cluster_store%n_clusters <= 0) return

        do k = 1, w%cluster_store%n_clusters
            call func(w%cluster_store%clusters(k), w)
        end do

    end subroutine apply_module_to_clusters

    ! =========================================================================
    ! SUBROUTINE: cycle_cluster_accumulators
    !
    ! Cycles the tick accumulators for a specific cluster.
    ! Called via apply_module_to_clusters in python_interface.
    ! =========================================================================
    subroutine cycle_cluster_accumulators(cluster, w)
        use mod_clustering, only: cluster_t
        implicit none
        type(cluster_t), intent(inout) :: cluster
        class(world_container), target, intent(inout) :: w

        integer :: i

        do i = w%history_length, 2, -1
            cluster%accumulators_history(i) = cluster%accumulators_history(i-1)
        end do
        
        ! Reset current tick's accumulators
        cluster%accumulators_history(1) = t_tick_accumulators()

        ! Reset the creativity sum accumulator for the new tick.
        ! update_creativity will re-accumulate each agent's creativity into this.
        if (allocated(cluster%pop_creativity_sum)) then
            cluster%pop_creativity_sum = 0.0d0
        end if

    end subroutine cycle_cluster_accumulators

    ! =========================================================================
    ! SUBROUTINE: compute_cluster_hep_nc
    !
    ! First cluster module: computes the sum of hep_av across all grid cells
    ! belonging to a cluster (summed over all populations), then derives NC:
    !
    !   hep_sum = SUM( hep_av(gx, gy, :) ) for all cells in cluster
    !   NC      = hep_sum * NC_per_hep
    !
    ! Also computes per-population values: pop_hep_sum and pop_NC
    !
    ! Stores the results in the cluster_t fields:
    !   cluster%hep_sum
    !   cluster%NC
    !   cluster%NC_per_hep
    !   cluster%pop_hep_sum(:)
    !   cluster%pop_NC(:)
    ! =========================================================================
    subroutine compute_cluster_hep_nc(cluster, w)
        use mod_clustering, only: cluster_t
        implicit none
        type(cluster_t), intent(inout) :: cluster
        class(world_container), target, intent(inout) :: w

        integer :: c, gx, gy, jp
        real(8) :: cell_area, cell_nc_per_hep, cell_mc

        ! Allocate population arrays if needed
        if (.not. allocated(cluster%pop_hep_sum)) then
            allocate(cluster%pop_hep_sum(w%config%npops))
            allocate(cluster%MC_cl(w%config%npops))
        end if

        cluster%pop_hep_sum = 0.0d0
        cluster%MC_cl = 0.0d0
        cluster%hep_sum = 0.0d0
        cluster%MC_cl_total = 0.0d0

        do c = 1, cluster%n_cells
            gx = cluster%cell_gx(c)
            gy = cluster%cell_gy(c)
            cell_area = w%grid%cell(gx, gy)%area

            ! Sum hep_av and compute MC_cl for each population
            do jp = 1, w%config%npops
                cluster%pop_hep_sum(jp) = cluster%pop_hep_sum(jp) + max(0.0d0, w%grid%hep_av(gx, gy, jp))
                cluster%hep_sum = cluster%hep_sum + max(0.0d0, w%grid%hep_av(gx, gy, jp))
                
                ! config%NC is capacity density in People / 100 km2.
                ! Compute effective nc_per_hep for this specific cell's area:
                cell_nc_per_hep = w%config%NC * (cell_area / 100.0d0)
                
                ! Compute the cell's carrying capacity contribution:
                cell_mc = max(0.0d0, w%grid%hep_av(gx, gy, jp)) * cell_nc_per_hep
                
                cluster%MC_cl(jp) = cluster%MC_cl(jp) + cell_mc
                cluster%MC_cl_total = cluster%MC_cl_total + cell_mc
            end do
        end do

        ! Store the reference config NC parameter in the cluster
        cluster%NC = w%config%NC

    end subroutine compute_cluster_hep_nc

    ! =========================================================================
    ! SUBROUTINE: apply_pop_pressure_to_hep_av  (full grid, single population)
    !
    ! Applies the Weibull population pressure scaling to every cell in the grid
    ! for a given population jp.  Matches the legacy equation (Konstantin Klein /
    ! Yaping Shao, Oktober-3 archive):
    !
    !   rho_c      = N_max * hep_av(i,j,jp)          [People / 100 km^2]
    !   delta_rho  = human_density(i,j) / rho_c
    !   max_pp     = (eta/eps) * (1-1/eta)^(1-1/eta) * exp(-(1-1/eta))
    !   pop_press  = (eta/eps) * (delta_rho/eps)^(eta-1) * exp(-(delta_rho/eps)^eta)
    !              / max_pp
    !   hep_av(i,j,jp) = pop_press * hep_av(i,j,jp)
    !
    ! Water cells (hep_av <= 0) are left unchanged (pop_press = 1 there).
    ! =========================================================================
    subroutine apply_pop_pressure_to_hep_av(g, jp, N_max, eta, eps)
        implicit none
        type(Grid), intent(inout) :: g
        integer,    intent(in)    :: jp
        real(8),    intent(in)    :: N_max, eta, eps

        integer :: i, j
        real(8) :: rho, hep_val, rho_c, delta_rho, max_pp, pp

        ! Normalisation constant: peak of the Weibull PDF (at delta_rho=eps*(1-1/eta)^(1/eta))
        max_pp = (eta / eps) * (1.0d0 - 1.0d0/eta)**(1.0d0 - 1.0d0/eta) &
                             * exp(-(1.0d0 - 1.0d0/eta))

        do j = 1, g%ny
            do i = 1, g%nx
                hep_val = g%hep_av(i, j, jp)
                if (hep_val <= 0.0d0) cycle          ! water: leave as -1

                rho     = g%cell(i, j)%human_density   ! [People / 100 km^2]
                rho_c   = N_max * hep_val

                if (rho_c <= 0.0d0) then
                    pp = 1.0d0
                else
                    delta_rho = rho / rho_c
                    pp = (eta / eps) * (delta_rho / eps)**(eta - 1.0d0) &
                                     * exp(-(delta_rho / eps)**eta) / max_pp
                end if

                g%hep_av(i, j, jp) = pp * hep_val
                g%cell(i, j)%pop_pressure = pp
            end do
        end do
    end subroutine apply_pop_pressure_to_hep_av

    ! =========================================================================
    ! SUBROUTINE: apply_pop_pressure_to_cell  (single cell, single population)
    !
    ! Same Weibull scaling as apply_pop_pressure_to_hep_av but for one cell.
    ! Used by the efficient (cluster-only) update path.
    ! =========================================================================
    subroutine apply_pop_pressure_to_cell(g, gx, gy, jp, N_max, eta, eps)
        implicit none
        type(Grid), intent(inout) :: g
        integer,    intent(in)    :: gx, gy, jp
        real(8),    intent(in)    :: N_max, eta, eps

        real(8) :: rho, hep_val, rho_c, delta_rho, max_pp, pp

        hep_val = g%hep_av(gx, gy, jp)
        if (hep_val <= 0.0d0) return              ! water: leave as -1

        max_pp = (eta / eps) * (1.0d0 - 1.0d0/eta)**(1.0d0 - 1.0d0/eta) &
                             * exp(-(1.0d0 - 1.0d0/eta))

        rho   = g%cell(gx, gy)%human_density  ! [People / 100 km^2]
        rho_c = N_max * hep_val

        if (rho_c <= 0.0d0) then
            pp = 1.0d0
        else
            delta_rho = rho / rho_c
            pp = (eta / eps) * (delta_rho / eps)**(eta - 1.0d0) &
                             * exp(-(delta_rho / eps)**eta) / max_pp
        end if

        g%hep_av(gx, gy, jp) = pp * hep_val
        g%cell(gx, gy)%pop_pressure = pp
    end subroutine apply_pop_pressure_to_cell

end module mod_technical_modules
