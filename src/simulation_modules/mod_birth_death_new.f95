! =============================================================================
! Module: mod_birth_death_new
!
! Description:
!   Custom birth, death, and preparation module for workshop implementation.
!   Contains 3 independently selectable subroutines:
!
!   1. new_death(agent_ptr)       - Runs on each alive agent (death logic)
!   2. new_birth(agent_ptr)       - Runs on each alive agent (birth logic)
!   3. new_preparation(world)     - Runs on the grid (preparation/resource logic)
!
!   Each subroutine has access to 10 config parameters:
!   - Birth:       agent_ptr%world%config%b1 .. b10
!   - Death:       agent_ptr%world%config%d1 .. d10
!   - Preparation: w%config%p1 .. p10
!
!   These parameters are read from basic_config.nml and editable in the UI.
! =============================================================================

module mod_birth_death_new

    use mod_constants
    use mod_config
    use mod_hashmap
    use mod_rnorm
    use mod_grid_id
    use mod_agent_world
    use mod_calculations

    implicit none


    contains


    ! DN 23.04. I have edited this file such that it compiles in the new version of the program. I have not changed any logic. 
    ! 
    !           There are a few issues. 
    ! 
    !                 1. inconsistencies regarding ticks and years and how to compute years from ticks
    !                 2. inneffizient access of agents 
    !             
    !
    !           I have changed the following: 
    !
    !               A. moved all the accumulators and dynamic state variables to mod_counter
    !                  Access them via the agent: agent_ptr%world%dynamic_state%K_fertility
    !               B. removed current time function
    !                  Acces time via: agent%world%current_tick


    ! =================================================================
    ! SUBROUTINE: new_death
    !
    ! Called for EACH ALIVE AGENT via apply_module_to_agents.
    !
    ! Available agent fields (via agent_ptr%...):
    !   - pos_x, pos_y         : position (real(8))
    !   - vel_x, vel_y         : velocity (real(8))
    !   - age                  : age in ticks/weeks (integer)
    !   - gender               : 'M' or 'F' (character)
    !   - is_pregnant           : 0 = no, >0 = weeks pregnant (integer)
    !   - is_dead              : .true. / .false. (logical)
    !   - resources            : current resource count (integer)
    !   - avg_resources        : running average resources (real(8))
    !   - population           : population index (integer)
    !   - children             : number of children (integer)
    !
    ! To access config:
    !   agent_ptr%world%config%d1  (through d10)
    !
    ! To kill an agent:
    !   call agent_ptr%agent_dies(reason=N)
    !   where reason:
    !     1 = natural death
    !     2 = resource starvation
    !     3 = drowned (out of bounds)
    !     4 = verhulst pressure  
    !     5 = birth-death controller
    !     6+ = custom (your own reasons)
    !
    ! Config parameters: agent_ptr%world%config%d1 .. d10
    ! =================================================================

!************************************************************************************
!**********************   DEATH BLOCK   *********************************************
!**********************   DEATH BLOCK   *********************************************
!************************************************************************************
subroutine new_death(current_agent)
    !------------------------------------------------------------------------------------
    ! Sandesh, 31 Mar 2026
    ! input: current_agent
    !
    ! Purpose: obtain for given age a death rate from function natural_death_rate
    !          and generate a random number to check whether death acutally happens
    !------------------------------------------------------------------------------------
    !
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent
    type(world_config), pointer :: config
    type(t_tick_accumulators), pointer :: accumulators

    real(8) :: death_prob, age_in_yr
    character(len=2) :: opt
    real :: r                                                ! random number
    real :: mu

    integer :: c_idx, jp

    ! Associate config pointer
    config => current_agent%world%config
    jp = current_agent%population

    ! Resolve cluster index
    if (allocated(current_agent%world%cluster_store%cell_cluster_idx)) then
        c_idx = current_agent%world%cluster_store%cell_cluster_idx(current_agent%gx, current_agent%gy)
    else
        c_idx = 0
    end if

    if (c_idx > 0) then
        accumulators => current_agent%world%cluster_store%clusters(c_idx)%accumulators_history(1)
    else
        accumulators => current_agent%world%accumulators_history(1)
    end if

    ! Calculate age in years - ensure floating point arithmetic
    age_in_yr = current_agent%age_years      

    opt = 'GM'

    mu = real(natural_death_rate(age_in_yr, opt), 8)   ! avoids mutiple calls
    
    ! Note: accumulators are now arrays indexed by population
    accumulators%phi_death_acc(jp) = accumulators%phi_death_acc(jp) - mu

    !print *, "Age in years is:", age_in_yr
    
    death_prob = 1.0d0 - exp(-mu * config%dt)
    !death_prob = mu * config%dt
    if (death_prob < 0.0d0) death_prob = 0.0d0
    if (death_prob > 1.0d0) death_prob = 1.0d0
        
    call random_number(r)

    if ( r < death_prob ) then
        call current_agent%agent_dies(reason=1)
    end if

end subroutine new_death

!*************************************************************************
! Sandesh, 31 Mar 2026, function for natural death rate
!*************************************************************************
real function natural_death_rate(age_real, opt) result(drate)
    !-------------------------------------------------------------------------
    ! Y Shao, 19 Feb 2026
    ! input:  age_real: agent age in                       [yr]
    !         opt     : 'GM', 'GK' or 'YS', defaul is 'YS'
    ! output: drate   : natural death rate in              [p_yr]^-1
    !
    ! Function: calculate the relative mortality rate mu
    ! The mortality equation is dN = N mu dt or mu dt = dN/N;
    ! Note: [mu] = [p-yr]^-1; Here, p-yr stands for person-year;
    ! Note: [mu d age] = [mu dt] = [0] is probability;
    !
    ! Gompertz-Makeham GM-Modell: mu = a_gm + b_gm exp(c_gm x)
    !                             x  = age in yr
    !
    ! S(x) = exp( - int_0^x mu(t) dt ) is closely related to
    ! age profile. Preliminary fitting to hunter-gather age profile
    ! data gives (see HESCOR Research Note of Yaping Shao, Feb 2026
    ! a_gm = 0.0122 [p-yr]^-1, b_gm = 0.0002 [p-yr]^-1 and c_gm = 0.1209 [p-yr]
    !
    ! Gurven-Kaplan GK-Model: a piecewise linear model
    ! mu = a_gk (age - 14) + b_gk        for age < 14
    !      b_gk                          for 14 <= age < 50
    !      c_gk (age - 50) + b_gk        for age > 50
    ! Thomas Elble (2022) MSc gives
    ! a_gk = -0.0027 [p-yr]^-2, b_gk = 0.0046 [p-yr]^-1, c_gk = 0.0033 [p-yr]^-2
    !
    ! Yaping Shao YS-Model: mu = a_ys/x + b_ys exp(c_ys x)
    ! a_ys = 0.01 [0], b_ys = 0.0006 [p-yr]^-1, c_ys = 0.1 [p-yr]
    !
    ! Note: YS-Model retains the basics of GM-Model, but with increased mortality
    !       rate at young age, is preferred, but note the math probleme at x = 0.
    !-----------------------------------------------------------------------------
    !
    implicit none
    real(8), intent(in) :: age_real       ! age in yr
    character(len=2), intent(in) :: opt

    real(8), parameter :: a_gm = 0.0122d0,  b_gm = 0.0002d0, c_gm = 0.1209d0
    real(8), parameter :: a_gk = -0.0027d0, b_gk = 0.0046d0, c_gk = 0.0033d0
    real(8), parameter :: yr_l = 14.0d0, yr_h = 50.0d0
    real(8), parameter :: a_ys = 0.01d0,    b_ys = 0.0006d0, c_ys = 0.1d0

    ! Local variable for age to avoid modifying intent(in)
    real(8) :: age_local

    ! Make a local copy that can be modified
    age_local = age_real

    ! Check for valid option
    if (opt /= 'GM' .and. opt /= 'GK' .and. opt /= 'Sa' .and. opt /= 'Sb' .and. opt /= 'YS') then
        print *, "Warning: natural_death_rate: invalid opt '", opt, "', using YS model"
        ! Fall through to YS model
    endif

    ! Calculate mortality rate based on selected model
    if (opt == 'GM') then
        if (age_local < 85.0d0) then
            drate = a_gm + b_gm * exp(c_gm * age_local)
        else
            drate = 1.0d0
        end if

    else if (opt == 'GK') then
        if (age_local < yr_l) then
            drate = a_gk * (age_local - yr_l) + b_gk
        else if (age_local <= yr_h) then
            drate = b_gk
        else if (age_local < 85.0d0) then
            drate = c_gk * (age_local - yr_h) + b_gk
        else
            drate = 1.0d0
        end if

    else if (opt == 'Sa') then
        !if (age_local <5.0d0) then
        !    drate = -0.1d0
        !else
            drate = a_gm + b_gm * exp(c_gm * age_local)
        !end if

    else if (opt == 'Sb') then
        if (age_local < 5.0d0) then
            ! Phase 1: GM-Shao Net Growth (Births > Deaths)
            drate = -0.1d0
        elseif (age_local < 15.0d0) then
            ! Phase 2: Gurven-Kaplan Childhood (Linear)
            drate = a_gk * (age_local - yr_l) + b_gk
        elseif (age_local < 85.0d0) then
            ! Phase 3: Gompertz-Makeham (exponential)
            drate = a_gm + b_gm * exp(c_gm * age_local)
        else
            drate = 1.0d0
        end if

    else  ! Default to YS model (including when opt is invalid)
        ! Handle age 0 for YS model (a_ys/age would be division by zero)
        if (age_local <= 0.0d0) then
            print *, "Warning: age = ", age_local, " <= 0, using age = 0.1 yr for YS model"
            age_local = 0.1d0
        else if (age_local < 85.0d0) then
            drate = a_ys / age_local + b_ys * exp(c_ys * age_local)
        else
            drate = 1.0d0
        end if

    end if
end function natural_death_rate

    !subroutine calc_de


! =================================================================
! SUBROUTINE: new_birth
!
! Called for EACH ALIVE AGENT via apply_module_to_agents.
!
! Config parameters: agent_ptr%world%config%b1 .. b10
! =================================================================

    subroutine new_birth(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent
        type(world_config), pointer :: config
        type(t_tick_accumulators), pointer :: accumulators
        type(t_dynamic_state), pointer :: dynamic_state
        type(Agent), pointer :: father_agent
        type(Agent) :: new_agent

        integer :: c_idx, jp
        integer :: tsb_in_ticks
        real(8) :: birth_prob, age_in_yr, tsb_in_yr, rho, lambda
        real :: r                                                ! random number

        birth_prob = 0.0d0

        jp = current_agent%population

        ! Associate config pointer
        config => current_agent%world%config

        ! Resolve cluster index
        if (allocated(current_agent%world%cluster_store%cell_cluster_idx)) then
            c_idx = current_agent%world%cluster_store%cell_cluster_idx(current_agent%gx, current_agent%gy)
        else
            c_idx = 0
        end if

        if (c_idx > 0) then
            accumulators => current_agent%world%cluster_store%clusters(c_idx)%accumulators_history(1)
            dynamic_state => current_agent%world%cluster_store%clusters(c_idx)%dynamic_state_vars
        else
            accumulators => current_agent%world%accumulators_history(1)
            dynamic_state => current_agent%world%dynamic_state_vars
        end if

        ! Calculate age in years - ensure floating point arithmetic
        age_in_yr = current_agent%age_years

        tsb_in_ticks = current_agent%ticks_since_last_birth

        if (tsb_in_ticks < 0) then
            ! tsb = -1, means never given birth 
            tsb_in_ticks = 200
        endif

        tsb_in_yr = ticks_in_years(tsb_in_ticks, config%dt)

        !!! check  with Daniel (it is called in interface - update_age)
        !!! call update_age(current_agent)
        !!! pregnancy counter for the below female agent advanced in update_age function

        !! DN 23.04. age_ticks, age_years, pregnancy are updated in update_age subroutine
        !!           update_age subroutine is always on. Does not have to be configured in python interface.

        if (current_agent%gender == 'F' .and. tsb_in_yr > 2.0d0) then
            if (age_in_yr >= 18.0d0 .and. age_in_yr <= 46.0d0) then
                ! Get density directly from agent's current cell
                rho = current_agent%grid%cell(current_agent%gx, current_agent%gy)%human_density
                if (rho >= 0.10d0) then
                    lambda = real(fertility_rate(age_in_yr), 8)
        
                    ! Find a male in the current cell as father
                    father_agent => get_male_from_cell(current_agent%world, current_agent%gx, current_agent%gy)

                    ! Only consider birth if father is found
                    if (associated(father_agent)) then
                        ! accumulate for Eq.26 (unscaled, same gates as actual birth)
                        accumulators%phi_birth_acc(jp) = accumulators%phi_birth_acc(jp) + &
                            (lambda * frate_ftsb(tsb_in_yr) * frate_fenc(rho))
                            
                        birth_prob = fertility_rate(age_in_yr) * frate_ftsb(tsb_in_yr) &
                                   * frate_fenc(rho) * dynamic_state%K_fertility(jp) * config%dt
                        if (birth_prob > 1.0d0) then
                            print*, "Warning: birth prob > 1, cluster birth."
                            birth_prob = 1.0d0
                        end if
                        if (birth_prob < 0.0d0) then
                            print*, "Warning: birth prob < 0, cluster birth ."
                            birth_prob = 0.0d0
                        end if
            
                        call random_number(r)
                        if (r < birth_prob) then
                            current_agent%is_pregnant = 1       ! start pregnancy counter (will be incremented by update_age_pregnancy)
                            current_agent%ticks_since_last_birth = 0      ! reset time since birth counter
                            current_agent%father_of_unborn_child = father_agent%id
                            ! Births are realised in realise_birth function.

                        end if
                    end if
                end if
            end if
        end if

    end subroutine new_birth

    


real function fertility_rate(age_real) result(frate)
    implicit none
    real(8), intent(in) :: age_real       ! age in yr

    real(8) :: x, y

    real(8), parameter :: a_ys = 2.10,    b_ys = 0.4    ! a_ys changed from 2.10 provided by Yaping
    !real(8), parameter :: a_ys = 7.0,    b_ys = 4.0
    real(8), parameter :: yr_l = 18.0d0, yr_h = 46.0d0, tau = 28.0    ! tau = yr_h - yr_l

    ! Local variable for age to avoid modifying intent(in)
    real(8) :: age_local

    ! Make a local copy that can be modified
    age_local = age_real

    x = (age_local - yr_l)/tau
    y = age_local/tau
    frate = a_ys * x * exp( -b_ys * x - y )     !!!!!!!!!!!!!!!!!!!!!!!!note single tau used here
end function fertility_rate


!*************************************************************************
! function for correction of age-specific fertility rate
!*************************************************************************
real function frate_ftsb(tsb) result(ftsb)
    !-------------------------------------------------------------------------
    ! Y Shao, 20 Feb 2026
    ! input:  tsb, time since birth             [yr]
    ! output: ftsb, correction factor
    !
    ! Purpose: calculate correction function for asfr, see HESCOR Notes 2026:
    !          Research Notes on Birth Death Modelling)
    ! Fertility depends on time since last birth, a correction on asbr needs
    ! applied
    ! frate = frate * ftsb
    ! ftsb = 0              for tsb < tsb_min
    !      = 1 - exp( -(tsb - tsb_min)/tau_tsb ) for tsb >= tsb_min
    !
    ! Recommended parameters: tsb_min = 2 [yr]; tau_tsb = 1 [yr]
    !----------------------------------------------------------------------------
    implicit none
        real(8), intent(in) :: tsb          ! time since birth in [yr]
        real(8), parameter  :: tsb_min = 2.0d0, tau_tsb = 1.0d0

        if (tsb < tsb_min) then
            ftsb = 0.0d0
        else
            ftsb = 1.0d0 - exp(-(tsb - tsb_min)/tau_tsb)
        endif
end function frate_ftsb


!*************************************************************************
! function for encounter-correction of age-specific fertility rate
!*************************************************************************
real function frate_fenc(rho) result(f_enc)
    !-------------------------------------------------------------------------
    ! Y Shao, 20 Feb 2026
    ! input:  rho, population density           [pdu]
    !         embedded in rho is how many males are near a female, assuming gender balance
    ! output: fenc, correction factor for encounter
    !
    ! Purpose: asfr model assumes mating is not limited. In case of low
    ! population density, mating becomes limited. To account for this,
    ! we use "saturation" based on number of humans in female vicinity
    !
    ! f_enc (rho) = 0                                     for  rho < rho_min
    !             = 1 - exp( -(rho - rho_min)/tau_rho )   for  rho >= rho_min
    !
    ! For testing YS guesses:
    ! rho_min = 0.1 PDU (2 humans/2000 km^2 i.e. 1 males/2000 km^2)
    ! tau_rho = 0.8 PDU (8 humans/2000 km^2 i.e. 4 males/2000 km^2)
    !-------------------------------------------------------------------------
    implicit none
    real(8), intent(in) :: rho                  ! population density in PDU
    real(8), parameter  :: rho_min = 0.10d0, tau_rho = 0.80d0
    real(8) :: x

    x = (rho - rho_min)
    if ( x <= 0. ) then
        f_enc = 0.0d0
    else
      f_enc = 1. - exp( - x/tau_rho)
    endif
end function frate_fenc


integer function count_alive_now_fast(w) result(n_alive)
    implicit none
    class(world_container), intent(in) :: w
    integer :: jp, n_pop

    n_alive = 0
    do jp = 1, w%config%npops
        n_pop = w%num_humans(jp) - w%num_humans_marked_dead(jp)
        if (n_pop > 0) n_alive = n_alive + n_pop
    end do
end function count_alive_now_fast


! =================================================================
! SUBROUTINE: update_cluster_macroscopic_fertility_scale
!
! Eq.26 controller:
!   phi_target = r * (1 - N/Nc)
!   K_fertility <- clamp(phi_target / phi_sim, Kmin, Kmax)
!
! This routine evaluates K_fertility for each cluster independently.
! =================================================================

subroutine update_cluster_macroscopic_fertility_scale(w)
    implicit none
    class(world_container), target, intent(inout) :: w

    real(8) :: r, Nc, Kmin, Kmax
    real(8) :: phi_target, n_total
    real(8) :: K_raw
    real(8), parameter :: eps = 1.0d-12
    integer :: jp, c_idx

    type(t_tick_accumulators), pointer :: accumulators
    type(t_dynamic_state), pointer :: dynamic_state

    ! Base config parameters
    r    = w%config%r
    Kmin = w%config%Kmin
    Kmax = w%config%Kmax


    if (Kmax <= 0.0d0) then
        print*, "Warning: Kmax <= 0, setting Kmax = 1"
        Kmax = 1.0d0
    end if
    if (Kmin < 0.0d0) then
        print*, "Warning: Kmin < 0, setting Kmin = 0"
        Kmin = 0.0d0
    end if
    if (Kmin > Kmax) then
        print*, "Warning: Kmin > Kmax, setting Kmin = Kmax"
        Kmin = Kmax
    end if


    ! -------------------------------------------------------------------
    ! Evaluate PER-CLUSTER K_fertility
    ! -------------------------------------------------------------------
    if (allocated(w%cluster_store%clusters)) then
        do c_idx = 1, w%cluster_store%n_clusters
            accumulators => w%cluster_store%clusters(c_idx)%accumulators_history(2)
            dynamic_state => w%cluster_store%clusters(c_idx)%dynamic_state_vars

            do jp = 1, w%config%npops
                ! instead of global NC we use the local NC of that cluster
                Nc = w%cluster_store%clusters(c_idx)%pop_NC(jp)

                if (Nc <= 0.0d0) then
                    ! DN 13.05. Changed this from 1.0 to Kmin, because if NC = 0 -> humans cant surbvive???
                    dynamic_state%K_fertility(jp) = Kmin
                    cycle
                end if

                n_total = real(accumulators%n_alive_acc(jp), 8)
                if (n_total <= 0.0d0) then
                    dynamic_state%K_fertility(jp) = Kmin
                    cycle
                end if

                phi_target = r * (1.0d0 - n_total / Nc)

                if (phi_target <= 0.0d0) then
                    K_raw = Kmin
                else
                    if (accumulators%phi_birth_acc(jp) <= eps) then
                        if (n_total > Nc) then
                            K_raw = Kmin
                        else
                            K_raw = dynamic_state%K_fertility(jp)
                        end if
                    else
                        ! Deaths are negative in phi_death_acc
                        K_raw = (phi_target * n_total - accumulators%phi_death_acc(jp)) / (accumulators%phi_birth_acc(jp))
                        if (K_raw < Kmin) K_raw = Kmin
                        if (K_raw > Kmax) K_raw = Kmax
                    end if
                end if
                dynamic_state%K_fertility(jp) = K_raw
            end do
        end do
    end if

end subroutine update_cluster_macroscopic_fertility_scale




end module mod_birth_death_new
