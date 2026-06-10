! =============================================================================
! Module: mod_creativity_fast (C3 — Cell Top-Individuals Tracking)
!
! PURPOSE:
!   O(N) replacement for the interactive creativity update in mod_creativity.
!   - Agents sort themselves into the high creativity individuals array
!     (with creativity and id) inside each gridcell.
!   - After all agents have done that, for each gridcell we find the ids and
!     creativity values of the max_high_creativity_individuals most creative.
!   - Then, when updating creativity, we loop only over the top individuals
!     in the neighbor cells to compute the interaction term.
!
! USAGE:
!   Registered as MODULE_CREATIVITY_FAST (ID 27) in python_interface.f95.
!   Call precompute_cell_high_creativity(world) once per update tick,
!   then call apply_module_to_agents(update_creativity_fast, t).
!
! =============================================================================

module mod_creativity_fast

    use mod_config
    use mod_constants
    use mod_agent_world
    use mod_grid_id
    use mod_hashmap

    implicit none

contains

    ! =========================================================================
    ! SUBROUTINE: precompute_cell_high_creativity
    !
    ! Resets the high-creativity arrays and counters, sweeps all alive agents
    ! to place them in their respective grid cell buffers, and sorts/truncates
    ! to keep only the top max_high_creativity_fast individuals.
    ! =========================================================================
    subroutine precompute_cell_high_creativity(w)
        implicit none
        type(world_container), intent(inout), target :: w

        integer :: jp, k, gx, gy, nx, ny, limit, count
        real(8) :: c_val, c_x, c_y
        integer :: c_id

        if (.not. allocated(w%grid%cell)) return

        nx = w%grid%nx
        ny = w%grid%ny
        limit = 2 * w%config%max_high_creativity_fast

        ! 1. Reset all cells
        do gx = 1, nx
            do gy = 1, ny
                w%grid%cell(gx, gy)%number_of_high_creativity_individuals = 0
            end do
        end do

        ! 2. Sweep over all alive agents and insert them into cell buffers
        do jp = 1, w%config%npops
            do k = 1, w%num_humans(jp)
                if (w%agents(k, jp)%is_dead) cycle

                gx = w%agents(k, jp)%gx
                gy = w%agents(k, jp)%gy

                if (gx < 1 .or. gx > nx .or. gy < 1 .or. gy > ny) cycle

                c_val = w%agents(k, jp)%creativity
                c_id = w%agents(k, jp)%id
                c_x = w%agents(k, jp)%pos_x
                c_y = w%agents(k, jp)%pos_y

                count = w%grid%cell(gx, gy)%number_of_high_creativity_individuals
                w%grid%cell(gx, gy)%high_creativity_values(count + 1) = c_val
                w%grid%cell(gx, gy)%high_creativity_ids(count + 1) = c_id
                w%grid%cell(gx, gy)%high_creativity_pos_x(count + 1) = c_x
                w%grid%cell(gx, gy)%high_creativity_pos_y(count + 1) = c_y
                w%grid%cell(gx, gy)%number_of_high_creativity_individuals = count + 1

                ! Sort and truncate when buffer is full
                if (w%grid%cell(gx, gy)%number_of_high_creativity_individuals == limit) then
                    call sort_and_truncate_cell(w%grid%cell(gx, gy), w%config%max_high_creativity_fast)
                end if
            end do
        end do

        ! 3. Final sweep to sort and truncate any remaining cell contents
        do gx = 1, nx
            do gy = 1, ny
                if (w%grid%cell(gx, gy)%number_of_high_creativity_individuals > 0) then
                    call sort_and_truncate_cell(w%grid%cell(gx, gy), w%config%max_high_creativity_fast)
                end if
            end do
        end do

    end subroutine precompute_cell_high_creativity


    ! =========================================================================
    ! SUBROUTINE: sort_and_truncate_cell
    !
    ! Sorts the cell's high-creativity arrays in descending order using Bubble Sort
    ! and keeps at most max_high individuals.
    ! =========================================================================
    subroutine sort_and_truncate_cell(cell, max_high)
        implicit none
        type(grid_cell), intent(inout) :: cell
        integer, intent(in) :: max_high
        integer :: n, i, j, temp_id
        real(8) :: temp_val

        n = cell%number_of_high_creativity_individuals
        if (n <= 1) return

        ! Descending Bubble Sort
        do i = 1, n - 1
            do j = 1, n - i
                if (cell%high_creativity_values(j) < cell%high_creativity_values(j+1)) then
                    ! Swap values
                    temp_val = cell%high_creativity_values(j)
                    cell%high_creativity_values(j) = cell%high_creativity_values(j+1)
                    cell%high_creativity_values(j+1) = temp_val

                    ! Swap IDs
                    temp_id = cell%high_creativity_ids(j)
                    cell%high_creativity_ids(j) = cell%high_creativity_ids(j+1)
                    cell%high_creativity_ids(j+1) = temp_id

                    ! Swap pos_x
                    temp_val = cell%high_creativity_pos_x(j)
                    cell%high_creativity_pos_x(j) = cell%high_creativity_pos_x(j+1)
                    cell%high_creativity_pos_x(j+1) = temp_val

                    ! Swap pos_y
                    temp_val = cell%high_creativity_pos_y(j)
                    cell%high_creativity_pos_y(j) = cell%high_creativity_pos_y(j+1)
                    cell%high_creativity_pos_y(j+1) = temp_val
                end if
            end do
        end do

        ! Truncate to max_high
        if (n > max_high) then
            cell%number_of_high_creativity_individuals = max_high
        end if
    end subroutine sort_and_truncate_cell


    ! =========================================================================
    ! SUBROUTINE: update_creativity_fast
    !
    ! Agent-centric update. Performs learning by looping only over the top
    ! creative individuals in neighbor grid cells.
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

        ! Neighbor search variables
        integer :: di, dj, ni, nj, search_r, k, num_high
        integer :: other_id
        real(8) :: other_creativity, other_pos_x, other_pos_y
        real(8) :: dx_km, dy_km, dist_km

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

        ! --- Interactive creativity (top individuals search) ---
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

                num_high = grid%cell(ni, nj)%number_of_high_creativity_individuals
                do k = 1, num_high
                    other_creativity = grid%cell(ni, nj)%high_creativity_values(k)
                    other_id = grid%cell(ni, nj)%high_creativity_ids(k)

                    ! Don't learn from ourselves
                    if (other_id == current_agent%id) cycle

                    ! Only learn from individuals with higher creativity
                    if (other_creativity <= current_agent%creativity) exit ! sorted descending: subsequent elements are smaller

                    other_pos_x = grid%cell(ni, nj)%high_creativity_pos_x(k)
                    other_pos_y = grid%cell(ni, nj)%high_creativity_pos_y(k)

                    dx_km = 111.3d0 * (current_agent%pos_x - other_pos_x) &
                          * cos(current_agent%pos_y * 3.14159265358979d0 / 180.0d0)
                    dy_km = 111.3d0 * (current_agent%pos_y - other_pos_y)
                    dist_km = sqrt(dx_km**2 + dy_km**2)

                    if (dist_km <= 3.0d0 * config%c3_R .and. dist_km > 0.0d0) then
                        interaction_term = interaction_term + &
                            config%c3_l * current_agent%creativity * &
                            (other_creativity - current_agent%creativity) * &
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

    end subroutine update_creativity_fast

end module mod_creativity_fast
