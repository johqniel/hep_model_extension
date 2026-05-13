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
    ! Creativity Model Parameters (from Y. Shao's original C3.f95)
    ! =========================================================================

    ! Forced creativity: dh_1 = gamma_1 * f_1(h)
    real(8), parameter :: ysPmax1  = 0.1d0     ! max probability of forced creativity      [0]
    real(8), parameter :: ysAlpha1 = 2.0d0     ! shape parameter (Weibull)                 [0]
    real(8), parameter :: ysPhi_l1 = 0.6d0     ! scale parameter                           [PDU]
    real(8), parameter :: ystau1   = 10.0d0    ! forced creativity time scale               [yr]

    ! Curiosity creativity: dh_2 = gamma_2 * f_2(h)
    real(8), parameter :: ysPmax2  = 0.1d0     ! max probability of curiosity creativity   [0]
    real(8), parameter :: ysk2     = 3.0d0     ! shape parameter (logistic)                [0]
    real(8), parameter :: ysPhi_l2 = 3.5d0     ! scale parameter                           [PDU]
    real(8), parameter :: ystau2   = 50.0d0    ! curiosity creativity time scale            [yr]

    ! Interactive creativity
    real(8), parameter :: ysl      = 0.00005d0 ! learning capability                       [PDU yr]^-1
    real(8), parameter :: ysR      = 300.0d0   ! interaction scale distance                 [km]

    ! Bounds
    real(8), parameter :: min_creativity = 0.1d0
    real(8), parameter :: max_creativity = 10.0d0

contains

    ! =========================================================================
    ! SUBROUTINE: update_creativity  (AGENT-CENTRIC)
    !
    ! Called once per tick per living agent.
    ! Updates the agent's creativity value based on:
    !   - local HEP (forced + curiosity terms)
    !   - nearby agents with higher creativity (interaction term)
    ! =========================================================================
    subroutine update_creativity(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        type(world_config), pointer :: config
        type(Grid), pointer :: grid
        integer :: gx, gy, jp
        real(8) :: phi_av, dt
        real(8) :: ysP1, ysP2, gamma1, gamma2
        real(8) :: forced_term, curiosity_term, interaction_term
        real(8) :: rand_val

        ! Neighbor search variables
        integer :: di, dj, ni, nj, search_r
        integer :: a_slot, agent_id_neighbor, neighbor_k, neighbor_pop
        real(8) :: dx_km, dy_km, dist_km
        real(8) :: neighbor_creativity
        type(Agent), pointer :: neighbor_agent

        ! --- Safety checks ---
        if (.not. associated(current_agent%world)) return
        config => current_agent%world%config
        grid => current_agent%world%grid

        gx = current_agent%gx
        gy = current_agent%gy
        jp = current_agent%population
        dt = config%dt

        if (gx < 1 .or. gx > grid%nx .or. gy < 1 .or. gy > grid%ny) return
        if (jp < 1 .or. jp > config%npops) return

        ! --- Get HEP at agent's location ---
        phi_av = grid%hep_av(gx, gy, jp)

        ! --- Forced creativity probability (Weibull-like) ---
        ysP1 = ysPmax1 * (phi_av / ysPhi_l1)**(ysAlpha1 - 1.0d0) &
             * exp(-(phi_av / ysPhi_l1)**ysAlpha1)

        ! --- Curiosity creativity probability (logistic) ---
        ysP2 = ysPmax2 / (1.0d0 + exp(-ysk2 * (phi_av - ysPhi_l2)))

        ! --- Stochastic activation ---
        call random_number(rand_val)
        if (rand_val <= ysP1) then
            gamma1 = 1.0d0
        else
            gamma1 = 0.0d0
        end if

        call random_number(rand_val)
        if (rand_val <= ysP2) then
            gamma2 = 1.0d0
        else
            gamma2 = 0.0d0
        end if

        forced_term    = gamma1 * current_agent%creativity / ystau1
        curiosity_term = gamma2 * current_agent%creativity / ystau2

        ! --- Interactive creativity (grid-based neighbor search) ---
        interaction_term = 0.0d0

        ! Search radius in grid cells: ysR km / (111.3 km/deg * delta_lon deg)
        ! Using 3*ysR as the cutoff distance (as in original code)
        if (config%delta_lon > 0.0d0) then
            search_r = ceiling(3.0d0 * ysR / (111.3d0 * config%delta_lon))
            search_r = min(search_r, 5) ! cap to avoid excessive search
        else
            search_r = 3
        end if

        do di = -search_r, search_r
            do dj = -search_r, search_r
                ni = gx + di
                nj = gy + dj
                if (ni < 1 .or. ni > grid%nx .or. nj < 1 .or. nj > grid%ny) cycle

                ! Loop over agents in this cell
                do a_slot = 1, grid%cell(ni, nj)%number_of_agents
                    agent_id_neighbor = grid%cell(ni, nj)%agents_ids(a_slot)
                    if (agent_id_neighbor < 0) cycle
                    if (agent_id_neighbor == current_agent%id) cycle

                    ! Look up neighbor agent via hashmap
                    call get_agent_by_id(current_agent%world, agent_id_neighbor, neighbor_agent)
                    if (.not. associated(neighbor_agent)) cycle
                    if (neighbor_agent%is_dead) cycle

                    neighbor_creativity = neighbor_agent%creativity

                    ! Only learn from agents with higher creativity
                    if (neighbor_creativity <= current_agent%creativity) cycle

                    ! Calculate distance in km
                    dx_km = 111.3d0 * (current_agent%pos_x - neighbor_agent%pos_x) &
                          * cos(current_agent%pos_y * 3.14159265358979d0 / 180.0d0)
                    dy_km = 111.3d0 * (current_agent%pos_y - neighbor_agent%pos_y)
                    dist_km = sqrt(dx_km**2 + dy_km**2)

                    ! Within interaction range (3 * ysR)
                    if (dist_km <= 3.0d0 * ysR .and. dist_km > 0.0d0) then
                        interaction_term = interaction_term + &
                            ysl * current_agent%creativity * &
                            (neighbor_creativity - current_agent%creativity) * &
                            exp(-dist_km / ysR)
                    end if
                end do
            end do
        end do

        ! --- Update creativity ---
        current_agent%creativity = current_agent%creativity + &
            (forced_term + curiosity_term + interaction_term) * dt

        ! --- Clamp to bounds ---
        current_agent%creativity = max(min_creativity, min(max_creativity, current_agent%creativity))

    end subroutine update_creativity

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

end module mod_creativity
