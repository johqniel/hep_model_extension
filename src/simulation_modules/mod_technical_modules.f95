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
        if (allocated(agent_ptr%world%cluster_store%cell_cluster_idx)) then
            c_idx = agent_ptr%world%cluster_store%cell_cluster_idx(agent_ptr%gx, agent_ptr%gy)
        else
            c_idx = 0
        end if
    
        accumulators => agent_ptr%world%accumulators_history(1)

        accumulators%n_alive_acc(jp) = accumulators%n_alive_acc(jp) + 1

        if (c_idx > 0) then
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
    subroutine update_density_and_hep_grid(w, t)
        use mod_constants, only: deg_km
        use mod_read_inputs, only: load_hep_chunk_from_file
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t
        
        integer :: jp
        type(Grid), pointer :: grid
        integer :: i, j, k, id, nx, ny, local_idx
        real(8) :: flow_x_sum, flow_y_sum
        type(Agent), pointer :: agent_ptr
        real(8), allocatable :: raw_density(:,:), smoothed(:,:)
        integer :: iter

        grid => w%grid
        nx = grid%nx
        ny = grid%ny

        ! We process each population
        do jp = 1, w%config%npops
        
            ! 1. update the Pure Density
            call grid%update_density_pure()
            
        ! 2. Calculate agent flows
        do i = 1, nx
            do j = 1, ny
                flow_x_sum = 0.0d0
                flow_y_sum = 0.0d0
                    
                if (grid%cell(i,j)%number_of_agents > 0) then
                    do k = 1, grid%cell(i,j)%number_of_agents
                        id = grid%cell(i,j)%agents_ids(k)
                        if (id > 0) then
                            agent_ptr => get_agent(id, w)
                            if (associated(agent_ptr)) then
                                if (agent_ptr%population == jp) then
                                    flow_x_sum = flow_x_sum + agent_ptr%ux
                                    flow_y_sum = flow_y_sum + agent_ptr%uy
                                end if
                            end if
                        end if
                    end do
                end if

                ! Normalize flow by area
                if (grid%cell(i,j)%area > 0.0d0) then
                        grid%cell(i,j)%flow_x = flow_x_sum * 100.0d0 / grid%cell(i,j)%area
                        grid%cell(i,j)%flow_y = flow_y_sum * 100.0d0 / grid%cell(i,j)%area
                else
                        grid%cell(i,j)%flow_x = 0.0d0
                        grid%cell(i,j)%flow_y = 0.0d0
                end if
            end do
        end do
            
        ! 3. update smoothed density (human_density_smoothed)
        !    Uses smooth_box_filter from mod_watershed with human_density_smoothing_radius.
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
            
        ! 4. Base HEP transfer (hep_av = hep) with dynamic chunked loading
        if (grid%t_hep < grid%chunk_start_t .or. grid%t_hep > grid%chunk_end_t) then
             call load_hep_chunk_from_file(grid, grid%t_hep)
        end if
        local_idx = grid%t_hep - grid%chunk_start_t + 1
        grid%hep_av(:,:,jp) = grid%hep(:,:,jp, local_idx)
            
        end do
            
    end subroutine update_density_and_hep_grid


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

        integer :: parent_one_id, parent_two_id, population

        if (current_agent%is_pregnant == 0) then
            ! agent is not pregnant
            return
        endif

        ! Else: 

        parent_one_id = current_agent%id
        parent_two_id = current_agent%father_of_unborn_child

        population = current_agent%population

        ! birth occurs
        father_ptr => get_agent(parent_two_id, current_agent%world)
        
        if (.not. associated(father_ptr)) then
            ! Father died during pregnancy.
            father_ptr => current_agent
        endif

        new_agent = generate_agent_born(current_agent%world, current_agent, father_ptr)
        call add_agent_to_array_hash(current_agent%world, new_agent, new_agent%population)

        ! Reset pregnancy state
        current_agent%is_pregnant = 0

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
        real(8) :: total_hep

        ! Allocate population arrays if needed
        if (.not. allocated(cluster%pop_hep_sum)) then
            allocate(cluster%pop_hep_sum(w%config%npops))
            allocate(cluster%pop_NC(w%config%npops))
        end if

        cluster%pop_hep_sum = 0.0d0
        total_hep = 0.0d0

        do c = 1, cluster%n_cells
            gx = cluster%cell_gx(c)
            gy = cluster%cell_gy(c)

            ! Sum hep_av across all populations for this cell (ignoring water coded as -1.0)
            do jp = 1, w%config%npops
                cluster%pop_hep_sum(jp) = cluster%pop_hep_sum(jp) + max(0.0d0, w%grid%hep_av(gx, gy, jp))
                total_hep = total_hep + max(0.0d0, w%grid%hep_av(gx, gy, jp))
            end do
        end do

        cluster%hep_sum    = total_hep
        cluster%NC_per_hep = w%config%NC_per_hep
        cluster%NC         = total_hep * w%config%NC_per_hep

        ! Per-population NC
        do jp = 1, w%config%npops
            cluster%pop_NC(jp) = cluster%pop_hep_sum(jp) * w%config%NC_per_hep
        end do

    end subroutine compute_cluster_hep_nc

end module mod_technical_modules
