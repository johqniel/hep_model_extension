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

    subroutine yaping_move(agent_ptr, t)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr
        integer, intent(in) :: t

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

    subroutine yaping_death_agb(agent_ptr, t)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr
        integer, intent(in) :: t

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================
        ! Example: kill agents older than 4000 weeks
        !   if (agent_ptr%age > 4000) then
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

end module mod_yaping_development
