! =============================================================================
! Module: mod_verhulst_pressure
!
! Description:
!   "Verhulst as a Pressure" — density-dependent agent mortality
!   combined with Gompertz age-mortality, plus density-limited births.
!
!   This is a SEPARATE module from mod_birth_death.  While the original
!   mod_birth_death directly kills/spawns agents to match a target count,
!   this module uses the Verhulst logistic feedback as a PROBABILITY
!   modifier, producing demographically realistic outcomes:
!
!   DEATH:
!     For each agent in a cell:
!       1. Density multiplier:   M = N / K   (N = current, K = carrying cap)
!       2. Gompertz base risk:   P_base = α + β × exp(γ × age)
!       3. Combined risk:        P_total = P_base × M
!       4. If rand() < P_total → agent dies
!
!     When N > K, M > 1, so everyone has elevated risk.
!     Older agents die preferentially (higher P_base).
!     When N ≪ K, M ≈ 0, almost nobody dies from overcrowding.
!
!   BIRTH:
!     Birth probability:   P_birth = r × max(0, 1 - N/K)
!     When N ≈ K → almost no births.
!     When N ≥ K → no births at all.
!
!   RESULT:
!     ● Population follows the logistic S-curve
!     ● Age pyramid is realistic (age-biased mortality)
!     ● Self-correcting: overcrowding kills, undercrowding enables births
!     ● Integrates with watershed clusters: high-density cluster cells
!       experience higher death pressure
!
! Integration:
!   use mod_verhulst_pressure
!
!   do jp = 1, world%config%npops
!       call apply_verhulst_pressure_all_cells(world, jp)
!   end do
!   call compact_agents(world)
!
! Dependencies:
!   mod_agent_world  (Agent, world_container, get_agent, get_ith_agent_from_cell,
!                     spawn_agent_hash, add_agent_to_array_hash, agent_dies)
!   mod_grid_id      (Grid, grid_cell)
!   mod_config       (world_config)
! =============================================================================

module mod_birth_death_probabilistic

    use mod_agent_world
    use mod_grid_id
    use mod_config

    implicit none

    ! =================================================================
    ! Gompertz age-mortality parameters
    !
    ! P_base(age) = ALPHA + BETA * exp(GAMMA * age_years)
    !
    ! ALPHA : baseline mortality per tick (accident / disease, age-independent)
    ! BETA  : Gompertz scale factor (initial mortality coefficient)
    ! GAMMA : Gompertz shape factor (rate of exponential increase with age)
    !
    ! With default values and dt_ticks_per_year = 365:
    !   age  0 yrs → P_base ≈ 0.0001 (low)
    !   age 30 yrs → P_base ≈ 0.0003
    !   age 60 yrs → P_base ≈ 0.004
    !   age 80 yrs → P_base ≈ 0.04
    ! =================================================================
    real(8), parameter :: DEFAULT_ALPHA = 1.0d-4    ! baseline mortality / tick
    real(8), parameter :: DEFAULT_BETA  = 1.0d-5    ! Gompertz scale
    real(8), parameter :: DEFAULT_GAMMA = 6.0d-2    ! Gompertz shape (per year)

    ! Carrying capacity from HEP
    real(8), parameter :: DEFAULT_CC_SCALE = 10.0d0  ! K = HEP × cc_scale

    ! Birth rate
    real(8), parameter :: DEFAULT_BIRTH_RATE = 0.02d0  ! max birth prob / tick

    ! Ticks per year (for converting age in ticks to years)
    real(8), parameter :: DEFAULT_TICKS_PER_YEAR = 365.0d0

    ! Death reason code (for agent_dies)
    integer, parameter :: DEATH_REASON_VERHULST = 6

contains

    ! =================================================================
    ! PUBLIC: apply_verhulst_pressure_all_cells
    !
    ! Loops over the entire grid and applies density-dependent
    ! mortality + births for one population.
    !
    ! Arguments:
    !   world       : world_container (inout)
    !   jp          : population index
    !   cc_scale    : optional, HEP → K multiplier (default 10)
    !   birth_rate  : optional, max birth probability (default 0.02)
    !   alpha       : optional, baseline mortality (default 1e-4)
    !   beta        : optional, Gompertz scale (default 1e-5)
    !   gamma       : optional, Gompertz shape (default 0.06)
    !   ticks_per_year : optional, age conversion (default 365)
    ! =================================================================
    subroutine apply_verhulst_pressure_all_cells(world, jp, &
                                                  cc_scale, birth_rate, &
                                                  alpha, beta, gamma_param, &
                                                  ticks_per_year)
        implicit none
        class(world_container), target, intent(inout) :: world
        integer, intent(in) :: jp
        real(8), intent(in), optional :: cc_scale, birth_rate
        real(8), intent(in), optional :: alpha, beta, gamma_param
        real(8), intent(in), optional :: ticks_per_year

        integer :: i, j
        type(Grid), pointer :: grid

        grid => world%grid

        do i = 1, grid%nx
            do j = 1, grid%ny
                if (grid%cell(i, j)%is_water == 1) cycle

                call apply_verhulst_pressure_to_cell(world, i, j, jp, &
                    cc_scale, birth_rate, alpha, beta, gamma_param, &
                    ticks_per_year)
            end do
        end do

    end subroutine apply_verhulst_pressure_all_cells

    ! =================================================================
    ! PRIVATE: apply_verhulst_pressure_to_cell
    !
    ! For a single cell (gx, gy) and population jp:
    !   Phase 1: Density-dependent mortality (Gompertz × density)
    !   Phase 2: Density-limited births
    ! =================================================================
    subroutine apply_verhulst_pressure_to_cell(world, gx, gy, jp, &
                                                cc_scale, birth_rate, &
                                                alpha, beta, gamma_param, &
                                                ticks_per_year)
        implicit none
        class(world_container), target, intent(inout) :: world
        integer, intent(in) :: gx, gy, jp
        real(8), intent(in), optional :: cc_scale, birth_rate
        real(8), intent(in), optional :: alpha, beta, gamma_param
        real(8), intent(in), optional :: ticks_per_year

        ! Local parameters
        real(8) :: cc_s, r_birth, a_val, b_val, g_val, tpy
        real(8) :: hep_value, K, M, age_years
        real(8) :: p_base, p_total, p_birth
        integer :: n_current, k_idx, agent_id, n_births, s
        type(Grid), pointer :: grid
        type(Agent), pointer :: agent_ptr
        type(Agent) :: new_agent
        real :: rand_val

        grid => world%grid

        ! -----------------------------------------------------------
        ! Resolve optional parameters
        ! -----------------------------------------------------------
        cc_s    = world%config%prob_birth_cc_scale;      if (present(cc_scale))       cc_s    = cc_scale
        r_birth = world%config%prob_birth_rate;          if (present(birth_rate))     r_birth = birth_rate
        a_val   = world%config%prob_death_alpha;         if (present(alpha))          a_val   = alpha
        b_val   = world%config%prob_death_beta;          if (present(beta))           b_val   = beta
        g_val   = world%config%prob_death_gamma;         if (present(gamma_param))    g_val   = gamma_param
        tpy     = world%config%ticks_per_year;           if (present(ticks_per_year)) tpy     = ticks_per_year

        ! -----------------------------------------------------------
        ! Get HEP and carrying capacity
        ! -----------------------------------------------------------
        if (allocated(grid%hep_av)) then
            hep_value = grid%hep_av(gx, gy, jp)
        else if (allocated(grid%hep)) then
            hep_value = grid%hep(gx, gy, jp, grid%t_hep)
        else
            return
        end if

        if (hep_value <= 0.0d0) return

        K = hep_value * cc_s
        if (K < 1.0d0) K = 1.0d0  ! minimum carrying capacity

        n_current = grid%cell(gx, gy)%number_of_agents
        if (n_current <= 0 .and. r_birth <= 0.0d0) return

        ! -----------------------------------------------------------
        ! Phase 1: DENSITY-DEPENDENT MORTALITY
        !
        ! Density multiplier: M = N / K
        !   M < 1  →  under capacity, low pressure
        !   M = 1  →  at capacity
        !   M > 1  →  over capacity, high pressure
        !
        ! For each agent:
        !   P_base  = α + β × exp(γ × age_years)
        !   P_total = P_base × M
        !   Kill if rand() < P_total
        ! -----------------------------------------------------------
        M = dble(n_current) / K

        ! Iterate backwards to avoid index issues when agents die
        do k_idx = n_current, 1, -1
            if (k_idx > grid%cell(gx, gy)%number_of_agents) cycle

            agent_id = grid%cell(gx, gy)%agents_ids(k_idx)
            if (agent_id <= 0) cycle

            agent_ptr => get_agent(agent_id, world)
            if (.not. associated(agent_ptr)) cycle
            if (agent_ptr%is_dead) cycle
            if (agent_ptr%population /= jp) cycle

            ! Gompertz age-mortality
            age_years = dble(agent_ptr%age) / tpy
            p_base = a_val + b_val * exp(g_val * age_years)

            ! Combined probability (clamped to [0, 1])
            p_total = p_base * M
            if (p_total > 1.0d0) p_total = 1.0d0
            if (p_total < 0.0d0) p_total = 0.0d0

            call random_number(rand_val)
            if (dble(rand_val) < p_total) then
                call agent_ptr%agent_dies(reason=DEATH_REASON_VERHULST)
            end if
        end do

        ! -----------------------------------------------------------
        ! Phase 2: DENSITY-LIMITED BIRTHS
        !
        ! P_birth = r × max(0, 1 - N/K)
        !   When N ≥ K → P_birth = 0 (no births)
        !   When N ≪ K → P_birth ≈ r (maximum birth rate)
        !
        ! Expected births = P_birth × (# fertile agents)
        ! For simplicity: n_births = nint(P_birth × n_current)
        ! -----------------------------------------------------------

        ! Refresh n_current after deaths
        n_current = grid%cell(gx, gy)%number_of_agents

        if (n_current > 0 .and. r_birth > 0.0d0) then
            p_birth = r_birth * max(0.0d0, 1.0d0 - dble(n_current) / K)

            if (p_birth > 0.0d0) then
                ! Expected births this tick
                n_births = nint(p_birth * dble(n_current))

                ! Stochastic correction for fractional births
                if (n_births == 0) then
                    call random_number(rand_val)
                    if (dble(rand_val) < p_birth * dble(n_current)) then
                        n_births = 1
                    end if
                end if

                ! Spawn new agents at cell centre
                if (n_births > 0) then
                    call spawn_new_agents(world, grid, gx, gy, jp, n_births)
                end if
            end if
        end if

    end subroutine apply_verhulst_pressure_to_cell

    ! =================================================================
    ! PRIVATE: spawn_new_agents
    !
    ! Spawns n_to_spawn agents at cell centre for population jp.
    ! Same pattern as mod_birth_death.
    ! =================================================================
    subroutine spawn_new_agents(world, grd, gx, gy, jp, n_to_spawn)
        implicit none
        type(world_container), target, intent(inout) :: world
        type(Grid), pointer, intent(in) :: grd
        integer, intent(in) :: gx, gy, jp, n_to_spawn

        integer :: s
        type(Agent) :: new_agent
        real(8) :: spawn_x, spawn_y

        if (.not. allocated(grd%lon_hep) .or. &
            .not. allocated(grd%lat_hep)) return

        spawn_x = grd%lon_hep(gx)
        spawn_y = grd%lat_hep(gy)

        do s = 1, n_to_spawn
            new_agent = world%spawn_agent_hash(jp)
            new_agent%pos_x = spawn_x
            new_agent%pos_y = spawn_y
            new_agent%age = 0
            call add_agent_to_array_hash(world, new_agent, jp)
        end do

    end subroutine spawn_new_agents

    ! =================================================================
    ! PUBLIC UTILITY: gompertz_mortality_rate
    !
    ! Returns the Gompertz base mortality for a given age.
    ! Useful for debugging / plotting the mortality curve.
    ! =================================================================
    real(8) function gompertz_mortality_rate(age_ticks, alpha, beta, &
                                             gamma_param, ticks_per_year) &
                                             result(p)
        implicit none
        integer, intent(in) :: age_ticks
        real(8), intent(in), optional :: alpha, beta, gamma_param, ticks_per_year

        real(8) :: a_val, b_val, g_val, tpy, age_years

        a_val = 1.0d-4;          if (present(alpha))          a_val = alpha
        b_val = 1.0d-5;          if (present(beta))           b_val = beta
        g_val = 6.0d-2;          if (present(gamma_param))    g_val = gamma_param
        tpy   = 365.0;           if (present(ticks_per_year)) tpy   = ticks_per_year

        age_years = dble(age_ticks) / tpy
        p = a_val + b_val * exp(g_val * age_years)

    end function gompertz_mortality_rate

end module mod_birth_death_probabilistic
