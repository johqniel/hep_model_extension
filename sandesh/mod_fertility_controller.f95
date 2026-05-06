! =============================================================================
! Module: mod_fertility_controller
!
! Description:
!   Encapsulates Eq.26 controller state and logic for macroscopic fertility scaling.
!   All global fertility-related variables are contained here.
!
! Key variables:
!   - K_fertility      : Current fertility scaling factor
!   - phi_death_acc    : Accumulated death rate for current tick
!   - phi_birth_acc    : Accumulated birth rate for current tick
!   - n_alive_acc      : Accumulated alive count for current tick
!
! =============================================================================

module mod_fertility_controller
    use mod_config
    use mod_agent_world
    
    implicit none
    
    ! All Eq.26 controller state bundled together
    type :: fertility_controller_t
        ! Current controller output
        real(8) :: K_fertility = 1.0d0
        
        ! Accumulators (collected during death/birth loops this tick)
        real(8) :: phi_death_acc = 0.0d0
        real(8) :: phi_birth_acc = 0.0d0
        integer :: n_alive_acc = 0
    end type fertility_controller_t
    
    ! Global instance (this module owns it)
    type(fertility_controller_t), save :: controller
    
contains

    subroutine controller_reset_accumulators()
        ! Called at START of each tick to reset accumulators
        implicit none
        controller%phi_death_acc = 0.0d0
        controller%phi_birth_acc = 0.0d0
        controller%n_alive_acc = 0
    end subroutine controller_reset_accumulators
    
    
    subroutine controller_finalize_tick(w)
        ! Called at END of tick to:
        ! 1. Update K_fertility from Eq.26
        ! 2. Reset accumulators for next tick
        implicit none
        class(world_container), intent(inout) :: w
        
        call update_K_from_config(w)
        
        ! Add diagnostics in controller_finalize_tick:
        !print *, "Tick:", current_tick
        !print *, "  N_total:", n_total, " Nc:", Nc, " ratio:", n_total/Nc
        !print *, "  phi_birth_acc:", controller%phi_birth_acc
        !print *, "  phi_death_acc:", controller%phi_death_acc
        !print *, "  K_fertility:", controller%K_fertility

        ! Reset accumulators for next tick
        controller%phi_death_acc = 0.0d0
        controller%phi_birth_acc = 0.0d0
        controller%n_alive_acc = 0
    end subroutine controller_finalize_tick
    
    
    subroutine update_K_from_config(w)
        ! Eq.26 controller logic
        ! phi_target = r * (1 - N/Nc)
        ! K_fertility <- clamp(phi_target / phi_sim, Kmin, Kmax)
        !
        ! Config parameters:
        !   b1 = r      (target growth rate)
        !   b2 = Nc     (carrying capacity)
        !   b3 = Kmin   (minimum fertility scale)
        !   b4 = Kmax   (maximum fertility scale)
        
        implicit none
        class(world_container), intent(in) :: w
        
        real(8) :: r, Nc, Kmin, Kmax
        real(8) :: phi_target, n_total
        real(8) :: K_raw
        real(8), parameter :: eps = 1.0d-12

        ! NEW: Smooth the transition (exponential filter)
        real(8), parameter :: alpha = 0.90d0  ! Smoothing factor (0.8-0.95)
                                      ! Higher = more smoothing
        
        r    = w%config%b1
        Nc   = w%config%b2
        Kmin = w%config%b3
        Kmax = w%config%b4
        
        ! Validate bounds
        !if (Kmax <= 0.0d0) Kmax = 1.0d0
        !if (Kmin < 0.0d0)  Kmin = 0.0d0
        !if (Kmin > Kmax)   Kmin = Kmax
        
        ! Constraint disabled unless Nc > 0
        if (Nc <= 0.0d0) then
            controller%K_fertility = 1.0d0
            return
        end if
        
        ! True alive count at this point in tick
        n_total = real(count_alive_now_fast(w), 8)
        
        !if (n_total <= 0.0d0) then
        !    controller%K_fertility = Kmin
        !    return
        !end if
        
        phi_target = r * (1.0d0 - n_total / Nc)
        
        !if (phi_target <= 0.0d0) then
        !    K_raw = Kmin
        !else
            if (controller%phi_birth_acc <= eps) then
                ! Avoid unstable jump when births are tiny
                if (n_total > Nc) then
                    K_raw = Kmin
                else
                    K_raw = controller%K_fertility
                end if
            else
                ! deaths are stored negative in phi_death_acc
                K_raw = (phi_target * n_total - controller%phi_death_acc) / (controller%phi_birth_acc)
                if (K_raw < Kmin) K_raw = Kmin
                if (K_raw > Kmax) K_raw = Kmax
            end if
        !end if
        
        controller%K_fertility = K_raw
        !controller%K_fertility = alpha * controller%K_fertility + (1.0d0 - alpha) * K_raw

        !print *, "Equilibrium check:"
        !print *, "  phi_target:", phi_target
        !print *, "  phi_birth_acc:", controller%phi_birth_acc
        !print *, "  phi_death_acc:", controller%phi_death_acc
        !print *, "  K_fertility:", controller%K_fertility
        
    end subroutine update_K_from_config
    
    
    integer function count_alive_now_fast(w) result(n_alive)
        ! Count total alive agents (used by controller)
        implicit none
        class(world_container), intent(in) :: w
        integer :: jp, n_pop
        
        n_alive = 0
        do jp = 1, w%config%npops
            n_pop = w%num_humans(jp) - w%num_humans_marked_dead(jp)
            if (n_pop > 0) n_alive = n_alive + n_pop
        end do
    end function count_alive_now_fast

end module mod_fertility_controller
