! =============================================================================
! Module: mod_birth_death_target
!
! Description:
!   Provides a function that computes the desired (target) number of agents
!   in a single grid cell for the next time period.
!
!   The implementation uses the VERHULST population growth model
!   (Pierre-François Verhulst, 1838)
!
!       dN/dt = r · N · (1 - N/K)
!
!   where N is the population size, r the intrinsic growth rate, and
!   K the carrying capacity of the environment.
!
!   We discretise this with a forward-Euler step of Δt = 1 tick:
!
!       N(t+1) = N(t) + r · N(t) · (1 - N(t) / K)
!
!   The carrying capacity K is derived from the cell's HEP (habitat
!   suitability) value:  K = hep_value × carrying_capacity_scale.
!
! Reference:
!   Verhulst, P.-F. (1838). "Notice sur la loi que la population suit
!   dans son accroissement". Correspondance Mathématique et Physique,
!   10, 113–121.
!
! Integration:
!   use mod_birth_death_target
!   n_target = calc_target_agents_in_cell(n_current, hep_value, cc_scale)
!
! To swap in your own growth model, simply replace the body of
! calc_target_agents_in_cell while keeping the same interface.
! =============================================================================

module mod_birth_death_target

    implicit none

    ! ---------------------------------------------------------------
    ! Tunable default parameters (can be overridden via arguments)
    ! ---------------------------------------------------------------

    ! Intrinsic growth rate per tick (dimensionless, >0)
    real(8), parameter :: DEFAULT_GROWTH_RATE = 0.05d0

    ! Default scale: carrying_capacity = hep_value * cc_scale
    real(8), parameter :: DEFAULT_CC_SCALE = 10.0d0

contains

    ! =================================================================
    ! FUNCTION: calc_target_agents_in_cell
    !
    ! Computes the desired number of agents in a cell for the next
    ! tick, given the current count and the cell's HEP suitability.
    !
    ! Model: Verhulst logistic growth (discrete forward-Euler)
    !
    !   Continuous:  dN/dt = r · N · (1 - N/K)
    !   Discrete:    N(t+1) = N(t) + r · N(t) · (1 - N(t)/K)
    !
    ! where
    !   N = n_current                             (population size)
    !   r = growth_rate                           (intrinsic rate)
    !   K = hep_value × carrying_capacity_scale   (carrying capacity)
    !
    ! Arguments:
    !   n_current               : current alive agents in this cell (>=0)
    !   hep_value               : HEP suitability for the cell/pop
    !   carrying_capacity_scale : multiplier converting HEP -> max agents
    !                             (optional, defaults to DEFAULT_CC_SCALE)
    !   growth_rate             : intrinsic growth rate r
    !                             (optional, defaults to DEFAULT_GROWTH_RATE)
    !
    ! Returns:
    !   Integer >= 0 representing desired agent count.
    ! =================================================================

    integer function calc_target_agents_in_cell(n_current, hep_value, &
                                                 carrying_capacity_scale, &
                                                 growth_rate) result(n_target)
        implicit none

        integer, intent(in) :: n_current
        real(8), intent(in) :: hep_value
        real(8), intent(in), optional :: carrying_capacity_scale
        real(8), intent(in), optional :: growth_rate

        real(8) :: cc_scale, r, K, n_real, delta

        ! --- Resolve optional arguments ---
        if (present(carrying_capacity_scale)) then
            cc_scale = carrying_capacity_scale
        else
            cc_scale = DEFAULT_CC_SCALE
        end if

        if (present(growth_rate)) then
            r = growth_rate
        else
            r = DEFAULT_GROWTH_RATE
        end if

        ! --- Carrying capacity ---
        K = hep_value * cc_scale

        ! --- Edge cases ---
        ! If HEP is zero or negative (water / uninhabitable), target is 0
        if (K <= 0.0d0) then
            n_target = 0
            return
        end if

        ! If no agents currently, allow spontaneous colonisation:
        !   a small fraction of K can appear (minimum 1 if K >= 1)
        if (n_current <= 0) then
            n_target = max(1, int(K * 0.01d0))
            return
        end if

        ! --- Verhulst logistic growth (forward-Euler step) ---
        n_real = dble(n_current)
        delta = r * n_real * (1.0d0 - n_real / K)
        n_target = nint(n_real + delta)

        ! Clamp to [0, ceiling(K)]
        if (n_target < 0) n_target = 0
        if (n_target > ceiling(K)) n_target = ceiling(K)

    end function calc_target_agents_in_cell

end module mod_birth_death_target
