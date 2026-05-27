! =============================================================================
! Module: mod_creativity_fast (C3 — Cell Pre-Aggregation)
!
! PURPOSE:
!   O(N) replacement for the O(N²) interactive creativity update in
!   mod_creativity.  Forced and curiosity terms are IDENTICAL to
!   update_creativity.  Only the interaction term is approximated:
!     – A one-pass pre-sweep computes per-cell creativity statistics.
!     – Each agent then sums over ~9 cell aggregates instead of scanning
!       every individual neighbor agent.
!
! USAGE:
!   Registered as MODULE_CREATIVITY_FAST (ID 26) in python_interface.f95.
!   Call precompute_cell_creativity_stats(world) once per update tick,
!   then call apply_module_to_agents(update_creativity_fast, t).
!
! SHARED CONFIG:
!   Uses all c3_* parameters from world_config — no additional config needed.
!   See mod_config.f95 for the full list.
!
! CLUSTER INTEGRATION:
!   Works with MODULE_CREATIVITY_CLUSTER (25) unchanged — both modules
!   write to agent%creativity, which accumulate_cluster_creativity reads.
!
! =============================================================================

module mod_creativity_fast

    use mod_config
    use mod_constants
    use mod_agent_world
    use mod_grid_id

    implicit none

    ! Cell-level creativity aggregates (allocated on first call)
    real(8), allocatable, save :: cell_creativity_sum(:,:)
    real(8), allocatable, save :: cell_max_creativity(:,:)
    integer, allocatable, save :: cell_creativity_count(:,:)

contains

    ! =========================================================================
    ! SUBROUTINE: precompute_cell_creativity_stats
    !
    ! Called ONCE per creativity update tick, BEFORE apply_module_to_agents.
    ! Sweeps all alive agents in O(N) and fills per-cell aggregate arrays.
    ! =========================================================================
    subroutine precompute_cell_creativity_stats(w)
        implicit none
        type(world_container), intent(in), target :: w

        integer :: jp, k, gx, gy, nx, ny

        nx = w%grid%nx
        ny = w%grid%ny

        ! Allocate on first call (persists across ticks via 'save')
        if (.not. allocated(cell_creativity_sum)) then
            allocate(cell_creativity_sum(nx, ny))
            allocate(cell_max_creativity(nx, ny))
            allocate(cell_creativity_count(nx, ny))
        end if

        ! Reset
        cell_creativity_sum   = 0.0d0
        cell_max_creativity   = -1.0d0
        cell_creativity_count = 0

        ! Single pass over all alive agents
        do jp = 1, w%config%npops
            do k = 1, w%num_humans(jp)
                if (w%agents(k, jp)%is_dead) cycle

                gx = w%agents(k, jp)%gx
                gy = w%agents(k, jp)%gy

                if (gx < 1 .or. gx > nx .or. gy < 1 .or. gy > ny) cycle

                cell_creativity_sum(gx, gy)   = cell_creativity_sum(gx, gy) + w%agents(k, jp)%creativity
                cell_creativity_count(gx, gy) = cell_creativity_count(gx, gy) + 1
                if (w%agents(k, jp)%creativity > cell_max_creativity(gx, gy)) then
                    cell_max_creativity(gx, gy) = w%agents(k, jp)%creativity
                end if
            end do
        end do

    end subroutine precompute_cell_creativity_stats


    ! =========================================================================
    ! SUBROUTINE: update_creativity_fast  (AGENT-CENTRIC, MODULE_CREATIVITY_FAST)
    !
    ! Called at c3_individual_update_interval ticks per living agent.
    ! Forced & curiosity terms: identical to update_creativity.
    ! Interaction term: uses precomputed cell aggregates — O(cells_in_range) per agent.
    !
    ! IMPORTANT: precompute_cell_creativity_stats MUST be called before this.
    ! =========================================================================
    subroutine update_creativity_fast(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        type(world_config), pointer :: config
        type(Grid), pointer :: grid
        integer :: gx, gy, jp
        real(8) :: phi_av, dt
        real(8) :: forced_term, curiosity_term, interaction_term
        real(8) :: rand_val
        real(8) :: ysP1, ysP2, gamma1, gamma2

        ! Cell-level interaction variables
        integer :: di, dj, ni, nj, search_r
        real(8) :: dx_km, dy_km, dist_km
        real(8) :: delta_sum

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
        ! IDENTICAL to update_creativity
        ysP1 = config%c3_Pmax1 * (phi_av / config%c3_Phi_l1)**(config%c3_Alpha1 - 1.0d0) &
             * exp(-(phi_av / config%c3_Phi_l1)**config%c3_Alpha1)

        ! --- Curiosity creativity probability (logistic) ---
        ! IDENTICAL to update_creativity
        ysP2 = config%c3_Pmax2 / (1.0d0 + exp(-config%c3_k2 * (phi_av - config%c3_Phi_l2)))

        ! --- Stochastic activation ---
        call random_number(rand_val)
        gamma1 = merge(1.0d0, 0.0d0, rand_val <= ysP1)

        call random_number(rand_val)
        gamma2 = merge(1.0d0, 0.0d0, rand_val <= ysP2)

        forced_term    = gamma1 * current_agent%creativity / config%c3_tau1
        curiosity_term = gamma2 * current_agent%creativity / config%c3_tau2

        ! --- Interactive creativity (cell pre-aggregation) ---
        interaction_term = 0.0d0

        if (.not. allocated(cell_creativity_sum)) goto 100  ! Skip if pre-sweep not called

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

                ! Skip cells with no agents or no higher creativity
                if (cell_creativity_count(ni, nj) == 0) cycle
                if (cell_max_creativity(ni, nj) <= current_agent%creativity) cycle

                ! Distance from agent to cell center
                dx_km = 111.3d0 * (current_agent%pos_x - grid%cell(ni, nj)%lon_in) &
                      * cos(current_agent%pos_y * 3.14159265358979d0 / 180.0d0)
                dy_km = 111.3d0 * (current_agent%pos_y - grid%cell(ni, nj)%lat_in)
                dist_km = sqrt(dx_km**2 + dy_km**2)

                if (dist_km <= 3.0d0 * config%c3_R) then
                    ! Approximate: sum_c - count_c * C_i  ≈  sum_{j: C_j > C_i} (C_j - C_i)
                    ! This slightly overestimates because it includes agents with C_j <= C_i
                    ! in the count, but their negative contributions are clamped to zero.
                    delta_sum = cell_creativity_sum(ni, nj) &
                              - dble(cell_creativity_count(ni, nj)) * current_agent%creativity
                    delta_sum = max(0.0d0, delta_sum)

                    if (dist_km > 0.0d0) then
                        interaction_term = interaction_term + &
                            config%c3_l * current_agent%creativity * &
                            delta_sum * exp(-dist_km / config%c3_R)
                    else
                        ! Agent is in same cell — use dist_km = 0 → exp(0) = 1
                        interaction_term = interaction_term + &
                            config%c3_l * current_agent%creativity * delta_sum
                    end if
                end if
            end do
        end do

100     continue

        ! --- Update and clamp ---
        current_agent%creativity = current_agent%creativity + &
            (forced_term + curiosity_term + interaction_term) * dt
        current_agent%creativity = max(config%c3_min_creativity, &
                                   min(config%c3_max_creativity, current_agent%creativity))

    end subroutine update_creativity_fast

end module mod_creativity_fast
