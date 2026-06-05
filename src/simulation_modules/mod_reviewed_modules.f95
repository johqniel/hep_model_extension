! =============================================================================
! Module: mod_reviewed_modules
!
! PURPOSE:
!   Reviewed simulation modules.
!   Contains:
!
!     1. reviewed_agent_motion(agent_ptr, t) - Agent-centric movement module
!
! CONFIG PARAMETERS:
!   Access via agent_ptr%world%config%r1 .. r10 (all real(8), default 0.0)
!
! =============================================================================

module mod_reviewed_modules

    use mod_config
    use mod_constants
    use mod_rnorm
    use mod_agent_world
    use mod_grid_id
    use mod_calculations
    use mod_hashmap

    implicit none

    ! =========================================================================
    ! MODULE-LEVEL VARIABLES
    ! =========================================================================
    ! These persist across ticks. Use "save" to keep values between calls.
    ! Example:
    !   integer, save :: my_counter = 0
    !   real(8), save :: accumulated_value = 0.0d0
    ! =========================================================================

contains


    ! =========================================================================
    ! SUBROUTINE: reviewed_agent_motion  (AGENT-CENTRIC)
    ! =========================================================================

    subroutine reviewed_agent_motion(current_agent)
        !-------------------------------------------------------
        ! Y Shao, 18Feb2026
        ! Update individual agent position
        !-------------------------------------------------------
            implicit none
            type(Agent), pointer, intent(inout) :: current_agent
            type(world_config), pointer :: config
            type(Grid), pointer :: grid

            real(8) :: old_x, old_y, new_x, new_y
            real(8) :: ux_old, uy_old, ux_new, uy_new
            real(8) :: grad_x, grad_y, heploc_max
            real(8) :: heploc(0:8)
            integer :: gxx(0:8), gyy(0:8)
            integer :: gx, gy, il, iloc, jp
            real(8) :: ax, ay

            real(8) :: lon_min, lon_max, lat_min, lat_max                       ! YS, 17Feb2026 define boundaries
            integer :: gx_w, gy_w                                               ! YS, 17022026, local, test if surface is water

            ! Coefficients (read from config)
            real(8) :: cb1, cb2, cb3

            ! 1. Pointer setup and state check
            if (current_agent%is_dead) return

            config => current_agent%world%config
            grid   => current_agent%grid
            jp     =  current_agent%population

            cb1 = config%cb1
            cb2 = config%cb2
            cb3 = config%cb3

            old_x  = current_agent%pos_x
            old_y  = current_agent%pos_y
            ux_old = current_agent%ux
            uy_old = current_agent%uy

            ! 2. Identify current grid cell
            call calculate_grid_pos(old_x, old_y, gx, gy, config)

            ! 3. Gradient Calculation (Moore Neighborhood)
            call calculate_gradient(gx, gy, jp, old_y, current_agent, grad_x, grad_y)

            ! 4. Generate Random Diffusion (Normal distribution)
            ax = rnorm() * config%sqdt
            ay = rnorm() * config%sqdt

            ! 5. Update Velocity (Langevin Equation)
            ! Formula: u_new = u_old + attraction - drag + random_diffusion
            ux_new = ux_old + (cb1 * grad_x) - (ux_old * cb2) + (cb3 * ax)
            uy_new = uy_old + (cb1 * grad_y) - (uy_old * cb2) + (cb3 * ay)

            ! 6. Calculate New Position
            new_x = old_x + ux_new / (deg_km * cos(old_y * deg_rad)) * config%dt
            new_y = old_y + uy_new / deg_km * config%dt

            ! 7. Boundary Reflection
            lon_min = grid%lon_hep(1) - 0.5*config%delta_lon
            lon_max = grid%lon_hep(config%dlon_hep) + 0.5*config%delta_lon
            lat_min = grid%lat_hep(1) - 0.5*config%delta_lat
            lat_max = grid%lat_hep(config%dlat_hep) + 0.5*config%delta_lat

            if (new_x < lon_min) then
            new_x  = 2.*lon_min - new_x
            ux_new = 2.*(new_x - old_x)/config%dt - ux_old
            elseif (new_x > lon_max) then
            new_x  = 2.*lon_max - new_x
            ux_new = 2.*(new_x - old_x)/config%dt - ux_old
            endif

            if (new_y < lat_min) then
            new_y = 2.*lat_min - new_y
            uy_new = 2.*(new_y - old_y)/config%dt - uy_old
            elseif (new_y > lat_max) then
            new_y = 2.*lat_max - new_y
            uy_new = 2.*(new_y - old_y)/config%dt - uy_old
            endif

            ! 8. Water Surface Check, stay there, but change velocity and hope for new direction
            call calculate_grid_pos(new_x, new_y, gx, gy, config)
            if (grid%cell(gx, gy)%is_water == 1) then                 ! grid%cell(gx,gy)%is_water == 1 means water surface
            new_x = old_x
            new_y = old_y
            ux_new = cb3*ax
            uy_new = cb3*ay
            call calculate_grid_pos(new_x, new_y, gx, gy, config)
            endif

            ! 9. Final checks, but should not happen
            if ( (gx < 1 .or. gx > config%dlon_hep) .or. &
                (gy < 1 .or. gy > config%dlat_hep) .or. &
                (grid%cell(gx, gy)%is_water == 1 )     ) then
            call current_agent%agent_dies(reason=3)
            return
            endif

            ! 10. Update Position and Velocity for Valid Move
            call current_agent%update_pos(new_x, new_y)                ! YS, 17Feb2026, Why not include velocity in the update? Improve
            current_agent%ux = ux_new
            current_agent%uy = uy_new

    end subroutine reviewed_agent_motion

    subroutine calculate_gradient(gx, gy, jp, ylat, current_agent, grad_x, grad_y)
        !****************************************************************************
        !----------------------------------------------------------------------------
        ! YS, 17Feb2026
        ! Calculates gradient toward maximum HEP value in Moore neighborhood
        !----------------------------------------------------------------------------
            implicit none
        ! input variables
            integer, intent(in) :: gx, gy, jp
            real*8, intent(in) :: ylat

        ! Output variables
            real*8, intent(out) :: grad_x, grad_y

            type(Agent), pointer, intent(in) :: current_agent  ! Pass agent instead of grid/config

            ! Local variables
            type(Grid), pointer :: grid
            type(world_config), pointer :: config
            real*8 :: heploc(0:8), heploc_max
            integer :: gxx(0:8), gyy(0:8)
            integer :: il, iloc
            logical :: valid_neighbor(0:8)
            real*8 :: inv_lat_step, inv_lon_step, diff_hep
            integer :: dx, dy

            ! Get local grid and local config from the agent
            grid => current_agent%grid
            config => current_agent%world%config

            ! Initialize
            grad_x = 0.d0
            grad_y = 0.d0
            heploc_max = -9999.d0
            iloc = 0

            ! Check if point is within grid
            if (gx < 1 .or. gx > config%dlon_hep .or. &
                gy < 1 .or. gy > config%dlat_hep) return

            ! Precompute uniform scale factors to replace division with multiplication
            inv_lat_step = 1.0d0 / (config%delta_lat * deg_km)
            inv_lon_step = 1.0d0 / (config%delta_lon * deg_km)

            if (gx > 1 .and. gx < config%dlon_hep .and. gy > 1 .and. gy < config%dlat_hep) then
                ! Fast path: all neighbors are strictly valid! Bypass all 8 boundary IF checks.
                heploc(0) = grid%hep_av(gx, gy, jp)       ! Center point
                heploc(1) = grid%hep_av(gx-1, gy-1, jp)   ! Top-left neighbor
                heploc(2) = grid%hep_av(gx, gy-1, jp)     ! Top neighbor
                heploc(3) = grid%hep_av(gx+1, gy-1, jp)   ! Top-right neighbor
                heploc(4) = grid%hep_av(gx+1, gy, jp)     ! Right neighbor
                heploc(5) = grid%hep_av(gx+1, gy+1, jp)   ! Bottom-right neighbor
                heploc(6) = grid%hep_av(gx, gy+1, jp)     ! Bottom neighbor
                heploc(7) = grid%hep_av(gx-1, gy+1, jp)   ! Bottom-left neighbor
                heploc(8) = grid%hep_av(gx-1, gy, jp)     ! Left neighbor

                gxx(0) = gx;     gyy(0) = gy              ! Center
                gxx(1) = gx-1;   gyy(1) = gy-1            ! Top-left
                gxx(2) = gx;     gyy(2) = gy-1            ! Top
                gxx(3) = gx+1;   gyy(3) = gy-1            ! Top-right
                gxx(4) = gx+1;   gyy(4) = gy              ! Right
                gxx(5) = gx+1;   gyy(5) = gy+1            ! Bottom-right
                gxx(6) = gx;     gyy(6) = gy+1            ! Bottom
                gxx(7) = gx-1;   gyy(7) = gy+1            ! Bottom-left
                gxx(8) = gx-1;   gyy(8) = gy              ! Left

                ! Unrolled maximum finding
                heploc_max = heploc(0)
                iloc = 0
                if (heploc(1) > heploc_max) then
                    heploc_max = heploc(1)
                    iloc = 1
                endif
                if (heploc(2) > heploc_max) then
                    heploc_max = heploc(2)
                    iloc = 2
                endif
                if (heploc(3) > heploc_max) then
                    heploc_max = heploc(3)
                    iloc = 3
                endif
                if (heploc(4) > heploc_max) then
                    heploc_max = heploc(4)
                    iloc = 4
                endif
                if (heploc(5) > heploc_max) then
                    heploc_max = heploc(5)
                    iloc = 5
                endif
                if (heploc(6) > heploc_max) then
                    heploc_max = heploc(6)
                    iloc = 6
                endif
                if (heploc(7) > heploc_max) then
                    heploc_max = heploc(7)
                    iloc = 7
                endif
                if (heploc(8) > heploc_max) then
                    heploc_max = heploc(8)
                    iloc = 8
                endif

            else
                ! Slow path: boundary cells
                valid_neighbor = .false.

                ! Center point
                heploc(0) = grid%hep_av(gx, gy, jp)
                gxx(0) = gx
                gyy(0) = gy
                valid_neighbor(0) = .true.

                ! Neighbors (only if within bounds)
                ! Top-left
                if (gx-1 >= 1 .and. gy-1 >= 1) then
                    heploc(1) = grid%hep_av(gx-1, gy-1, jp)
                    gxx(1) = gx-1; gyy(1) = gy-1
                    valid_neighbor(1) = .true.
                endif

                ! Top
                if (gy-1 >= 1) then
                    heploc(2) = grid%hep_av(gx, gy-1, jp)
                    gxx(2) = gx; gyy(2) = gy-1
                    valid_neighbor(2) = .true.
                endif

                ! Top-right
                if (gx+1 <= config%dlon_hep .and. gy-1 >= 1) then
                    heploc(3) = grid%hep_av(gx+1, gy-1, jp)
                    gxx(3) = gx+1; gyy(3) = gy-1
                    valid_neighbor(3) = .true.
                endif

                ! Right
                if (gx+1 <= config%dlon_hep) then
                    heploc(4) = grid%hep_av(gx+1, gy, jp)
                    gxx(4) = gx+1; gyy(4) = gy
                    valid_neighbor(4) = .true.
                endif

                ! Bottom-right
                if (gx+1 <= config%dlon_hep .and. gy+1 <= config%dlat_hep) then
                    heploc(5) = grid%hep_av(gx+1, gy+1, jp)
                    gxx(5) = gx+1; gyy(5) = gy+1
                    valid_neighbor(5) = .true.
                endif

                ! Bottom
                if (gy+1 <= config%dlat_hep) then
                    heploc(6) = grid%hep_av(gx, gy+1, jp)
                    gxx(6) = gx; gyy(6) = gy+1
                    valid_neighbor(6) = .true.
                endif

                ! Bottom-left
                if (gx-1 >= 1 .and. gy+1 <= config%dlat_hep) then
                    heploc(7) = grid%hep_av(gx-1, gy+1, jp)
                    gxx(7) = gx-1; gyy(7) = gy+1
                    valid_neighbor(7) = .true.
                endif

                ! Left
                if (gx-1 >= 1) then
                    heploc(8) = grid%hep_av(gx-1, gy, jp)
                    gxx(8) = gx-1; gyy(8) = gy
                    valid_neighbor(8) = .true.
                endif

                ! Find maximum among valid neighbors
                heploc_max = heploc(0)
                iloc = 0
                do il = 1, 8
                    if (valid_neighbor(il) .and. heploc(il) > heploc_max) then
                        heploc_max = heploc(il)
                        iloc = il
                    end if
                end do
            endif

            ! Calculate gradient toward maximum using linear step values
            if (iloc /= 0) then
                dy = gyy(iloc) - gyy(0)
                dx = gxx(iloc) - gxx(0)
                
                diff_hep = heploc_max - heploc(0)
                
                if (dy /= 0) then
                    grad_y = diff_hep * (real(dy, 8) * inv_lat_step)
                endif
                if (dx /= 0) then
                    grad_x = diff_hep * (real(dx, 8) * inv_lon_step / cos(ylat*deg_rad))
                endif
            endif

    end subroutine calculate_gradient

    !**************************************************************
    logical function agent_above_water(gx, gy, jp, t_hep, grid_ptr)
        !**************************************************************
        !----------------------------------------------------------------------
        ! Daniel N. (improvement YShao, 18Feb2026)
        ! logical function: agent_above_water
        ! Returns .true. if an agent from population jp that is in grid (gx, gy)
        ! that is a water surface
        !-----------------------------------------------------------------------
        implicit none
        integer, intent(in) :: jp, gx, gy, t_hep
        type(Grid), pointer :: grid_ptr

        agent_above_water = .false.

        if ( .not. allocated(grid_ptr%hep) ) then
            print *, "t_hep is not associated."
            return
        endif

        if ( gx    < lbound(grid_ptr%hep,1) .or. gx    > ubound(grid_ptr%hep,1) .or. &
            gy    < lbound(grid_ptr%hep,2) .or. gy    > ubound(grid_ptr%hep,2) .or. &
            jp    < lbound(grid_ptr%hep,3) .or. jp    > ubound(grid_ptr%hep,3) .or. &
            t_hep < lbound(grid_ptr%hep,4) .or. t_hep > ubound(grid_ptr%hep,4) ) then
            print *, "Warning: index out of bounds gx:    ", gx,    lbound(grid_ptr%hep,1), ubound(grid_ptr%hep,1)
            print *, "Warning: index out of bounds gy:    ", gy,    lbound(grid_ptr%hep,2), ubound(grid_ptr%hep,2)
            print *, "Warning: index out of bounds jp:    ", jp,    lbound(grid_ptr%hep,3), ubound(grid_ptr%hep,3)
            print *, "Warning: index out of bounds t_hep: ", t_hep, lbound(grid_ptr%hep,4), ubound(grid_ptr%hep,4)
            return
        endif

        if ( grid_ptr%hep(gx, gy, jp, t_hep) == -1 )    then
            agent_above_water = .true.
        endif

    end function agent_above_water




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
subroutine reviewed_death(current_agent)
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

    integer :: jp
    
    ! Associate config pointer
    config => current_agent%world%config
    jp = current_agent%population

    !accumulators for birth-death controller
    ! first slot = present
    accumulators => current_agent%world%accumulators_history(1)

    ! Calculate age in years - ensure floating point arithmetic
    age_in_yr = current_agent%age_years      
    
    opt = 'GM'

    mu = real(natural_death_rate(age_in_yr, opt), 8)   ! avoids mutiple calls
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

end subroutine reviewed_death

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


!************************************************************************************
!**********************   Birth BLOCK   *********************************************
!**********************   Birth BLOCK   *********************************************
!************************************************************************************

! =================================================================
! SUBROUTINE: new_birth
!
! Called for EACH ALIVE AGENT via apply_module_to_agents.
!
! Config parameters: agent_ptr%world%config%r, NC, Kmin, Kmax, b5-b10
! =================================================================

    subroutine reviewed_birth(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent
        type(world_config), pointer :: config
        type(t_tick_accumulators), pointer :: accumulators
        type(t_dynamic_state), pointer :: dynamic_state
        type(Agent), pointer :: father_agent
        type(Agent) :: new_agent

        integer :: jp
        integer :: tsb_in_ticks
        real(8) :: birth_prob, age_in_yr, tsb_in_yr, rho, lambda
        real :: r                                                ! random number

        birth_prob = 0.0d0

        jp = current_agent%population

        !jp = current_agent%population

        ! Associate config pointer
        config => current_agent%world%config

        ! Associate accumulators & dynamic state
        ! first slot = present
        accumulators => current_agent%world%accumulators_history(1)
        dynamic_state => current_agent%world%dynamic_state_vars

        ! Calculate age in years - ensure floating point arithmetic
        age_in_yr = current_agent%age_years

        tsb_in_ticks = current_agent%ticks_since_last_birth

        if (tsb_in_ticks < 0) then
            ! tsb = -1, means never given birth 
            print*, "Warning: tsb in ticks shouldn t be negative."
            tsb_in_ticks = 200
        endif

        tsb_in_yr = ticks_in_years(tsb_in_ticks, config%dt)   


        !! DN 23.04. age_ticks, age_years, pregnancy are updated in update_age subroutine
        !!           update_age subroutine is always on. Does not have to be configured in python interface.

        if (current_agent%gender == 'F' .and. tsb_in_yr > 2.0d0) then
            if (age_in_yr >= 18.0d0 .and. age_in_yr <= 46.0d0) then
                ! Get density directly from agent's current cell
                rho = current_agent%grid%cell(current_agent%gx, current_agent%gy)%human_density
                if (rho >= 0.10d0) then
                    lambda = real(fertility_rate(age_in_yr), 8)   
        
                    ! Find a male in the current cell as father
                    if (config%allow_across_populations) then
                        father_agent => get_male_from_cell(current_agent%world, current_agent%gx, current_agent%gy)
                    else
                        father_agent => get_male_from_cell(current_agent%world, &
                            current_agent%gx, current_agent%gy, only_population=jp)
                    end if


                        
                    ! Only consider birth if father is found
                    if (associated(father_agent)) then


                        ! accumulate for Eq.26 (unscaled, same gates as actual birth)
                        accumulators%phi_birth_acc(jp) = accumulators%phi_birth_acc(jp) + &
                            (lambda * frate_ftsb(tsb_in_yr) * frate_fenc(rho))
                        
                        birth_prob = fertility_rate(age_in_yr) * frate_ftsb(tsb_in_yr) &
                                   * frate_fenc(rho) * dynamic_state%K_fertility(jp) * config%dt  
                        if (birth_prob > 1.0d0) then
                            print*, "Warning: birth prob > 1."
                            birth_prob = 1.0d0
                        end if
                        if (birth_prob < 0.0d0) then
                            print*, "Warning: birth prob < 0."
                            birth_prob = 0.0d0
                        end if

                        call random_number(r)
                        if (r < birth_prob) then
                            current_agent%is_pregnant = 1       ! start pregnancy counter (will be incremented by update_agent_age)
                            current_agent%ticks_since_last_birth = 0      ! reset time since birth counter
                            current_agent%father_of_unborn_child = father_agent%id

                            ! Births are realised in realise_birth function.

                        end if
                    end if
                end if
            end if
        end if

    end subroutine reviewed_birth

    


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


integer function count_alive_now_fast(w, jp) result(n_alive)
    implicit none
    class(world_container), intent(in) :: w
    integer, intent(in) :: jp
    integer :: n_pop

    n_alive = 0
    n_pop = w%num_humans(jp) - w%num_humans_marked_dead(jp)
    if (n_pop > 0) n_alive = n_pop
end function count_alive_now_fast


! =================================================================
! SUBROUTINE: update_macroscopic_fertility_scale
!
! Sandesh, 31 Mar 2026
!
! Eq.26 controller:
!   phi_target = r * (1 - N/NC)
!   K_fertility <- clamp(phi_target / phi_sim, Kmin, Kmax)
!
! Config mapping:
!   r    = r
!   NC   = NC
!   Kmin = Kmin
!   Kmax = Kmax
! =================================================================

subroutine update_macroscopic_fertility_scale(w)
    implicit none
    class(world_container), target, intent(inout) :: w

    real(8) :: r, Nc, Kmin, Kmax
    real(8) :: phi_target, n_total
    real(8) :: K_raw
    real(8), parameter :: eps = 1.0d-12
    integer :: jp

    type(t_tick_accumulators), pointer :: accumulators
    type(t_dynamic_state), pointer :: dynamic_state

    ! Associate accumulators and dynamic state
    ! Use history(2) = previous tick's completed data, since history(1) was
    ! just zeroed by cycle_accumulators before this function is called.
    accumulators => w%accumulators_history(2)
    dynamic_state => w%dynamic_state_vars

    ! parameters below are fed from the config values in the interface app
    ! r = 0.02, NC = 1500.0, Kmin = 0.0, Kmax = 1.0
    r    = w%config%r
    Nc   = w%config%NC
    Kmin = w%config%Kmin
    Kmax = w%config%Kmax

    !print*, "Enter update macroscopic fertility scale, r = ", r, " NC = ", Nc, " Kmin = ", Kmin, " Kmax = ", Kmax

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

    ! Constraint disabled unless Nc > 0
    if (Nc <= 0.0d0) then
        print*, "Warning: NC <= 0, disabling fertility constraint."
    end if


    do jp = 1, w%config%npops
        if (Nc <= 0.0d0) then
            ! DN 13.05. Changed this from 1.0 to Kmin, because if NC = 0 -> humans cant surbvive???

            dynamic_state%K_fertility(jp) = Kmin
            cycle
        end if

        n_total = real(count_alive_now_fast(w, jp), 8)

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
                K_raw = (phi_target * n_total - accumulators%phi_death_acc(jp)) / (accumulators%phi_birth_acc(jp))
                if (K_raw < Kmin) K_raw = Kmin
                if (K_raw > Kmax) K_raw = Kmax
            end if
        end if

        dynamic_state%K_fertility(jp) = K_raw
    end do

end subroutine update_macroscopic_fertility_scale





    ! move children age <= 5 yr with their mothers
    subroutine move_children_to_mothers(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent
        type(Agent), pointer :: mother_ptr
        integer, parameter :: CHILD_AGE_LIMIT = 500  ! ticks

        if (current_agent%is_dead) return

        ! Check if this is a child (age < 500 ticks) and has a mother
        if (current_agent%age_ticks < CHILD_AGE_LIMIT .and. current_agent%mother > 0) then
            ! Try to get mother
            mother_ptr => get_agent(current_agent%mother, current_agent%world)

            if (associated(mother_ptr) .and. .not. mother_ptr%is_dead) then
                ! Move child to mother's position
                call current_agent%update_pos(mother_ptr%pos_x, mother_ptr%pos_y)
            end if
        end if

    end subroutine move_children_to_mothers


end module mod_reviewed_modules
