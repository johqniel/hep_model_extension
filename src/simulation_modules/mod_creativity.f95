! =============================================================================
! Module: mod_creativity (C3)
!
! PURPOSE:
!   Models the evolution of agent creativity over time, driven by:
!     1. Forced creativity   — environmental stress (low HEP) triggers innovation
!     2. Curiosity creativity — surplus environment (high HEP) enables innovation
!     3. Interactive creativity — agents learn from nearby agents with higher creativity
!
! ORIGINAL AUTHOR: Y. Shao, yshao@uni-koeln.de, 13 Jan 2026
! REFACTORED:      May 2026 — rewritten to use Agent type and grid-based neighbor lookup
!
! USAGE:
!   Registered as MODULE_CREATIVITY (ID 24) in python_interface.f95.
!   Called via: call apply_module_to_agents(update_creativity, t)
!
! =============================================================================

module mod_creativity

    use mod_config
    use mod_constants
    use mod_agent_world
    use mod_grid_id
    use mod_hashmap

    implicit none

    ! =========================================================================
    ! All creativity model parameters are now read from world_config (config%).
    ! See mod_config.f95 fields: c3_Pmax1, c3_Alpha1, c3_Phi_l1, c3_tau1,
    !   c3_Pmax2, c3_k2, c3_Phi_l2, c3_tau2, c3_l, c3_R, c3_search_r_cap,
    !   c3_min_creativity, c3_max_creativity.
    ! Set them in basic_config.nml / config_sandesh.nml under the C3 section.
    ! =========================================================================

contains

    ! =========================================================================
    ! SUBROUTINE: update_creativity  (AGENT-CENTRIC, MODULE_CREATIVITY)
    !
    ! Called at creativity_update_interval ticks per living agent.
    ! Updates the agent's creativity value based on:
    !   - local HEP (forced + curiosity terms)
    !   - nearby agents with higher creativity (interaction term)
    ! Parameters are read from world_config (c3_* fields).
    ! NOTE: Does NOT update cluster sums — use MODULE_CREATIVITY_CLUSTER for that.
    ! =========================================================================
    subroutine update_creativity(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        type(world_config), pointer :: config
        type(Grid), pointer :: grid
        integer :: gx, gy, jp
        real(8) :: phi_av, dt
        real(8) :: forced_term, curiosity_term, interaction_term
        real(8) :: rand_val
        real(8) :: ysP1, ysP2, gamma1, gamma2

        ! Neighbor search variables
        integer :: di, dj, ni, nj, search_r
        integer :: a_slot, agent_id_neighbor
        real(8) :: dx_km, dy_km, dist_km
        real(8) :: neighbor_creativity
        type(Agent), pointer :: neighbor_agent

        ! --- Safety checks ---
        if (.not. associated(current_agent%world)) return
        config => current_agent%world%config
        grid   => current_agent%world%grid

        gx = current_agent%gx
        gy = current_agent%gy
        jp = current_agent%population
        dt = config%dt

        if (gx < 1 .or. gx > grid%nx .or. gy < 1 .or. gy > grid%ny) return
        if (jp < 1 .or. jp > config%npops) return

        ! --- Get HEP at agent's location ---
        phi_av = grid%hep_av(gx, gy, jp)

        ! --- Forced creativity probability (Weibull-like) ---
        ysP1 = config%c3_Pmax1 * (phi_av / config%c3_Phi_l1)**(config%c3_Alpha1 - 1.0d0) &
             * exp(-(phi_av / config%c3_Phi_l1)**config%c3_Alpha1)

        ! --- Curiosity creativity probability (logistic) ---
        ysP2 = config%c3_Pmax2 / (1.0d0 + exp(-config%c3_k2 * (phi_av - config%c3_Phi_l2)))

        ! --- Stochastic activation ---
        call random_number(rand_val)
        gamma1 = merge(1.0d0, 0.0d0, rand_val <= ysP1)

        call random_number(rand_val)
        gamma2 = merge(1.0d0, 0.0d0, rand_val <= ysP2)

        forced_term    = gamma1 * current_agent%creativity / config%c3_tau1
        curiosity_term = gamma2 * current_agent%creativity / config%c3_tau2

        ! --- Interactive creativity (grid-based neighbor search) ---
        interaction_term = 0.0d0

        if (config%delta_lon > 0.0d0) then
            search_r = ceiling(3.0d0 * config%c3_R / (111.3d0 * config%delta_lon))
            search_r = min(search_r, config%c3_search_r_cap)
        else
            search_r = config%c3_search_r_cap
        end if

        do di = -search_r, search_r
            do dj = -search_r, search_r
                ni = gx + di
                nj = gy + dj
                if (ni < 1 .or. ni > grid%nx .or. nj < 1 .or. nj > grid%ny) cycle

                do a_slot = 1, grid%cell(ni, nj)%number_of_agents
                    agent_id_neighbor = grid%cell(ni, nj)%agents_ids(a_slot)
                    if (agent_id_neighbor < 0) cycle
                    if (agent_id_neighbor == current_agent%id) cycle

                    call get_agent_by_id(current_agent%world, agent_id_neighbor, neighbor_agent)
                    if (.not. associated(neighbor_agent)) cycle
                    if (neighbor_agent%is_dead) cycle

                    neighbor_creativity = neighbor_agent%creativity
                    if (neighbor_creativity <= current_agent%creativity) cycle

                    dx_km = 111.3d0 * (current_agent%pos_x - neighbor_agent%pos_x) &
                          * cos(current_agent%pos_y * 3.14159265358979d0 / 180.0d0)
                    dy_km = 111.3d0 * (current_agent%pos_y - neighbor_agent%pos_y)
                    dist_km = sqrt(dx_km**2 + dy_km**2)

                    if (dist_km <= 3.0d0 * config%c3_R .and. dist_km > 0.0d0) then
                        interaction_term = interaction_term + &
                            config%c3_l * current_agent%creativity * &
                            (neighbor_creativity - current_agent%creativity) * &
                            exp(-dist_km / config%c3_R)
                    end if
                end do
            end do
        end do

        ! --- Update and clamp ---
        current_agent%creativity = current_agent%creativity + &
            (forced_term + curiosity_term + interaction_term) * dt
        current_agent%creativity = max(config%c3_min_creativity, &
                                   min(config%c3_max_creativity, current_agent%creativity))

    end subroutine update_creativity

    ! =========================================================================
    ! SUBROUTINE: accumulate_cluster_creativity  (AGENT-CENTRIC, MODULE_CREATIVITY_CLUSTER)
    !
    ! Called every tick per living agent — O(1) per agent.
    ! Looks up the agent's cluster and adds its current creativity to
    ! cluster%pop_creativity_sum(jp). compute_available_hep then reads this
    ! sum instead of scanning all agents, making the cluster module O(npops).
    ! =========================================================================
    subroutine accumulate_cluster_creativity(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        integer :: c_idx, jp
        type(world_config), pointer :: config

        if (.not. associated(current_agent%world)) return
        config => current_agent%world%config

        jp = current_agent%population
        if (jp < 1 .or. jp > config%npops) return

        if (.not. allocated(current_agent%world%cluster_store%cell_cluster_idx)) return

        c_idx = current_agent%world%cluster_store%cell_cluster_idx( &
                    current_agent%gx, current_agent%gy)
        if (c_idx < 1) return

        ! Lazy-allocate (reset is handled each tick by cycle_cluster_accumulators)
        if (.not. allocated(current_agent%world%cluster_store%clusters(c_idx)%pop_creativity_sum)) then
            allocate(current_agent%world%cluster_store%clusters(c_idx)%pop_creativity_sum(config%npops))
            current_agent%world%cluster_store%clusters(c_idx)%pop_creativity_sum = 0.0d0
        end if

        current_agent%world%cluster_store%clusters(c_idx)%pop_creativity_sum(jp) = &
            current_agent%world%cluster_store%clusters(c_idx)%pop_creativity_sum(jp) &
            + current_agent%creativity

    end subroutine accumulate_cluster_creativity


    ! =========================================================================
    ! Helper: Get agent pointer by ID from world hashmap
    ! =========================================================================
    subroutine get_agent_by_id(w, agent_id, agent_ptr)
        implicit none
        type(world_container), intent(in), target :: w
        integer, intent(in) :: agent_id
        type(Agent), pointer, intent(out) :: agent_ptr

        integer :: k, pop_idx

        agent_ptr => null()

        if (.not. contains_key(w%index_map, agent_id)) return

        call get_index_and_pop(w%index_map, agent_id, k, pop_idx)

        if (k < 1 .or. pop_idx < 1) return
        if (pop_idx > w%config%npops) return
        if (k > w%num_humans(pop_idx)) return

        agent_ptr => w%agents(k, pop_idx)

    end subroutine get_agent_by_id

    ! =========================================================================
    ! SUBROUTINE: compute_available_hep (CLUSTER-CENTRIC)
    !
    ! Called once per creativity_update_interval ticks per cluster.
    ! Uses pre-accumulated pop_creativity_sum (filled by update_creativity each tick)
    ! and n_alive_acc from the previous tick's accumulators to compute the
    ! average creativity per population.  O(npops) -- no agent scanning.
    ! =========================================================================
    subroutine compute_available_hep(cluster, w)
        use mod_clustering, only: cluster_t
        implicit none
        type(cluster_t), intent(inout) :: cluster
        class(world_container), target, intent(inout) :: w

        integer :: jp
        real(8) :: avg_creativity, n_agents

        if (.not. allocated(cluster%MC_cl_AV)) then
            allocate(cluster%MC_cl_AV(w%config%npops))
        end if
        cluster%MC_cl_AV = -1.0d0

        ! If the accumulator has never been filled (module just activated), skip.
        if (.not. allocated(cluster%pop_creativity_sum)) return

        do jp = 1, w%config%npops
            ! Use the count from the *previous* tick (history slot 2), which is
            ! the fully completed count — same approach as the fertility controller.
            n_agents = dble(cluster%accumulators_history(2)%n_alive_acc(jp))

            if (n_agents > 0.0d0) then
                ! pop_creativity_sum was filled this tick by update_creativity
                avg_creativity = cluster%pop_creativity_sum(jp) / n_agents
            else
                ! No agents in this cluster
                avg_creativity = -1
            end if

            cluster%MC_cl_AV(jp) = cluster%MC_cl(jp) * (avg_creativity)
            if (cluster%MC_cl_AV(jp) < 0.0d0) cluster%MC_cl_AV(jp) = 0.0d0
        end do

    end subroutine compute_available_hep

end module mod_creativity
