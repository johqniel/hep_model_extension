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
!
!   The current tick is stored in module variable 'current_tick'.
!   Call set_module_tick(t) from the dispatch before using any function.
! =============================================================================

module mod_birth_death_new

    use mod_config
    use mod_agent_world
    use mod_grid_id

    implicit none

    ! Module-level tick counter (set by dispatch before each call)
    integer, save :: current_tick = 0

    ! Flag to ensure birth spawn only happens once at t=100
    logical, save :: birth_test_done = .false.

contains

    ! =================================================================
    ! SUBROUTINE: set_module_tick
    ! Sets the current tick so agent-level functions can access it.
    ! Called from the dispatch in python_interface before each module.
    ! =================================================================
    subroutine set_module_tick(t)
        implicit none
        integer, intent(in) :: t
        current_tick = t
    end subroutine set_module_tick


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

    subroutine new_death(agent_ptr)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr

        ! ----- TEST: Kill ALL agents at t=200 -----
        if (current_tick == 200) then
            call agent_ptr%agent_dies(reason=5)
        end if

    end subroutine new_death


    ! =================================================================
    ! SUBROUTINE: new_birth
    !
    ! Called for EACH ALIVE AGENT via apply_module_to_agents.
    !
    ! To spawn a new agent (same pattern as spawn_deficit_agents):
    !   new_agent = agent_ptr%world%spawn_agent_hash(jp)
    !   new_agent%pos_x = ...
    !   new_agent%pos_y = ...
    !   new_agent%age = 0
    !   call add_agent_to_array_hash(agent_ptr%world, new_agent, jp)
    !
    ! Config parameters: agent_ptr%world%config%b1 .. b10
    ! =================================================================

    subroutine new_birth(agent_ptr)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr

        integer :: i, jp
        type(Agent) :: new_agent

        ! ----- TEST: Spawn 1000 agents at t=100 (once) -----
        if (current_tick == 100 .and. &
            .not. birth_test_done) then
            birth_test_done = .true.
            jp = agent_ptr%population

            print *, "NEW_BIRTH TEST: Spawning 1000 &
                &agents at t=100"

            do i = 1, 1000
                new_agent = &
                    agent_ptr%world%spawn_agent_hash(jp)
                new_agent%pos_x = agent_ptr%pos_x
                new_agent%pos_y = agent_ptr%pos_y
                new_agent%age = 0
                new_agent%population = jp
                call add_agent_to_array_hash( &
                    agent_ptr%world, new_agent, jp)
            end do
        end if

    end subroutine new_birth


    ! =================================================================
    ! SUBROUTINE: new_preparation
    !
    ! Called ONCE PER TICK on the entire grid.
    !
    ! Access to the grid structure:
    !   w%grid                              : the Grid object
    !   w%grid%cell(gx, gy)                 : a single cell
    !   w%grid%cell(gx, gy)%number_of_agents: agent count
    !   w%grid%cell(gx, gy)%agents_ids(:)   : hash IDs
    !
    ! To get agent from cell ID:
    !   agent_id = w%grid%cell(gx,gy)%agents_ids(k)
    !   agent_ptr => get_agent(agent_id, w)
    !
    ! To move an agent (updates grid cell associations):
    !   call agent_ptr%update_pos(new_x, new_y)
    !
    ! Config parameters: w%config%p1 .. w%config%p10
    ! =================================================================

    subroutine new_preparation(w)
        implicit none
        class(world_container), target, intent(inout) :: w

        integer :: gx, gy, nx, ny, k, n_in_cell
        integer :: agent_id
        type(Agent), pointer :: agent_ptr
        real(8) :: new_x, new_y

        ! ----- TEST: Move all agents 2.0 deg right at t=50 -----
        ! Access agents through grid cells
        if (current_tick == 50) then
            nx = w%config%dlon_hep
            ny = w%config%dlat_hep

            print *, "NEW_PREPARATION TEST: Moving all &
                &agents 2.0 deg right at t=50"

            do gy = 1, ny
                do gx = 1, nx
                    n_in_cell = &
                        w%grid%cell(gx, gy)%number_of_agents

                    do k = 1, n_in_cell
                        agent_id = &
                            w%grid%cell(gx, gy)%agents_ids(k)
                        if (agent_id <= 0) cycle

                        agent_ptr => get_agent(agent_id, w)
                        if (.not. associated(agent_ptr)) cycle
                        if (agent_ptr%is_dead) cycle

                        ! Move ~200 km right (2.0 degrees lon)
                        ! Use update_pos to properly update
                        ! grid cell associations
                        new_x = agent_ptr%pos_x + 2.0d0
                        new_y = agent_ptr%pos_y
                        call agent_ptr%update_pos( &
                            new_x, new_y)
                    end do
                end do
            end do
        end if

    end subroutine new_preparation

end module mod_birth_death_new
