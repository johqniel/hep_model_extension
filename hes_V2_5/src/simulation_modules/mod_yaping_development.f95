! =============================================================================
! Module: mod_yaping_development
!
! PURPOSE:
!   Development modules for Prof. Yaping Shao
!   Contains four independently selectable subroutines:
!
!     1. yaping_move(agent_ptr, t)     - Agent-centric movement module
!     2. yaping_birth_grid(w, t)       - Grid-centric birth module
!     3. yaping_death_agb(agent_ptr, t)- Agent-centric death module (AGB)
!     4. yaping_death_grid(w, t)       - Grid-centric death module
!     5. yaping_population_pressure_grid(w, t) - Adjusts HEP due to overpopulation
!
! HOW TO USE:
!   1. Edit the subroutine(s) you want to modify below.
!   2. Declare any local variables you need right after "implicit none".
!   3. Write your logic where it says "YOUR CODE HERE".
!   4. Rebuild: run  ./build.sh  from the project root.
!   5. Restart the Python UI (application.py).
!   6. In the Spawn Editor, add the module(s) from the "Yaping Development"
!      group to the active module list.
!
! NOTES:
!   - You do NOT need to modify any config files.
!   - Use "print *, ..." followed by "call flush(6)" for debug output.
!
! =============================================================================

module mod_yaping_development

    use mod_config
    use mod_agent_world
    use mod_grid_id

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
    ! SUBROUTINE: yaping_move  (AGENT-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE FOR EVERY LIVING AGENT, every tick.
    !
    ! Available agent fields: agent_ptr%pos_x, pos_y, vel_x, vel_y,
    !   age, gender, is_pregnant, is_dead, resources, avg_resources,
    !   population, children, id
    !
    ! To move the agent:
    !   call agent_ptr%update_pos(new_x, new_y)
    !
    ! =========================================================================

    subroutine yaping_move(agent_ptr)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================
        ! Example: move agent slightly eastward each tick
        !   call agent_ptr%update_pos( &
        !       agent_ptr%pos_x + 0.001d0, &
        !       agent_ptr%pos_y)
        ! =====================================================================


    end subroutine yaping_move


    ! =========================================================================
    ! SUBROUTINE: yaping_birth_grid  (GRID-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE PER TICK on the entire grid.
    ! Use for birth logic that depends on spatial/grid-level information.
    !
    ! Grid access:
    !   w%config%dlon_hep / dlat_hep       : grid dimensions
    !   w%grid%cell(gx, gy)                : a single grid cell
    !   w%grid%cell(gx, gy)%number_of_agents : agents in cell
    !   w%grid%cell(gx, gy)%agents_ids(k)  : k-th agent ID in cell
    !
    ! To get agent from ID:
    !   agent_ptr => get_agent(agent_id, w)
    !
    ! To spawn a new agent:
    !   new_agent = w%spawn_agent_hash(jp)
    !   new_agent%pos_x = ...
    !   new_agent%pos_y = ...
    !   call add_agent_to_array_hash(w, new_agent, jp)
    !
    ! =========================================================================

    subroutine yaping_birth_grid(w, t)
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================


    end subroutine yaping_birth_grid


    ! =========================================================================
    ! SUBROUTINE: yaping_death_agb  (AGENT-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE FOR EVERY LIVING AGENT, every tick.
    ! Use for age/gender-based death logic.
    !
    ! To kill the agent:
    !   call agent_ptr%agent_dies(reason=N)
    !   reason codes: 1=natural, 2=starvation, 3=drowned,
    !                 4=verhulst, 5=birth-death, 6+=custom
    !
    ! =========================================================================

    subroutine yaping_death_agb(agent_ptr)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================
        ! Example: kill agents older than 4000 weeks
        !   if (agent_ptr%age_ticks > 4000) then
        !       call agent_ptr%agent_dies(reason=6)
        !   end if
        ! =====================================================================


    end subroutine yaping_death_agb


    ! =========================================================================
    ! SUBROUTINE: yaping_death_grid  (GRID-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE PER TICK on the entire grid.
    ! Use for death logic that depends on spatial/grid-level information
    ! (e.g., overcrowding in cells, resource depletion).
    !
    ! Grid access: same as yaping_birth_grid above.
    !
    ! =========================================================================

    subroutine yaping_death_grid(w, t)
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================


    end subroutine yaping_death_grid


    ! =========================================================================
    ! SUBROUTINE: pop_pressure_func
    ! =========================================================================
    ! Contains the actual Verhulst mathematical calculation.
    ! Extracted from mod_grid_id.f95 
    ! =========================================================================
    subroutine pop_pressure_func(grid_in, hep, N_max, eta, epsilon)
        implicit none
        type(Grid), intent(inout) :: grid_in
        real(8), dimension(:,:), intent(in) :: hep
        real(8), intent(in) :: N_max, eta, epsilon
        
        integer :: i, j
        real(8) :: rho, rho_c, delta_rho, max_pp
        
        max_pp = (eta/epsilon) * (1.0d0 - 1.0d0/eta)**(1.0d0 - 1.0d0/eta) * exp(-(1.0d0 - 1.0d0/eta))
        
        do i = 1, grid_in%nx
            do j = 1, grid_in%ny
                rho = grid_in%cell(i,j)%human_density
                rho_c = N_max * hep(i,j)
                
                if (rho_c > 0.0d0) then
                    delta_rho = rho / rho_c
                    grid_in%cell(i,j)%pop_pressure = (eta/epsilon) * (delta_rho/epsilon)**(eta-1.0d0) * &
                                                  exp(-(delta_rho/epsilon)**eta) / max_pp
                else
                    grid_in%cell(i,j)%pop_pressure = 0.0d0 ! Or handle as needed
                end if
                
                if (hep(i,j) <= 0.0d0) then
                    grid_in%cell(i,j)%pop_pressure = 1.0d0
                end if
            end do
        end do
        
    end subroutine pop_pressure_func

    ! =========================================================================
    ! SUBROUTINE: yaping_population_pressure_grid  (GRID-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE PER TICK on the entire grid.
    ! @TODO (Yaping): Please review if this population pressure logic is still 
    ! required or if it should be toggled off/deleted.
    ! 
    ! This logic uses mathematical parameters (rho_max, eta, epsilon) from the
    ! config to calculate how much the environmental HEP should be degraded
    ! based on the local high density of agents. It modifies grid%hep_av.
    !
    ! It is left over from the old code base. I think it might be usefull, 
    ! but it might also be redundant because we want to kind of implement a 
    ! much more sophisticated model with carrying capacity etc. am i right?
    !
    ! =========================================================================

    subroutine yaping_population_pressure_grid(w, t)
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t

        integer :: jp, i, j
        type(Grid), pointer :: grid

        grid => w%grid


        ! Check toggle in config
        if (w%config%with_pop_pressure) then
            do jp = 1, w%config%npops
                ! Use the already-smoothed density from update_density_and_hep_grid
                ! (no need for extra smooth2d call — smoothing happens every tick)
                
                ! Calculate population pressure fraction in each cell
                call pop_pressure_func(grid, grid%hep(:,:,jp, grid%t_hep), &
                                       w%config%rho_max(jp), &
                                       w%config%eta(jp), &
                                       w%config%epsilon(jp))
                                            
                ! Apply pressure to hep_av
                do i = 1, grid%nx
                    do j = 1, grid%ny
                        grid%hep_av(i,j,jp) = grid%cell(i,j)%pop_pressure * grid%hep(i,j,jp, grid%t_hep)
                    end do
                end do
            end do
        end if

    end subroutine yaping_population_pressure_grid

end module mod_yaping_development
