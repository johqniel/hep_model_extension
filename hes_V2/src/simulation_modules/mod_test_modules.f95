! =============================================================================
! Module: mod_test_modules
!
! PURPOSE:
!   Template module for people who want to experiment with the simulation
!   without needing deep knowledge of the full codebase.
!
!   This file contains TWO independent modules that you can toggle on/off
!   in the Spawn Editor UI like any other module:
!
!     1. "Test Module (Agents)"  — runs YOUR code once per tick on every living agent
!     2. "Test Module (Grid)"    — runs YOUR code once per tick on the grid
!
! HOW TO USE:
!   1. Open this file and find the subroutine you want to modify:
!        - test_module_agents  (for per-agent logic)
!        - test_module_grid    (for grid/spatial logic)
!   2. Declare any variables you need at the TOP of the subroutine,
!      right after "implicit none". You do NOT need to touch any
!      config files or other source files.
!   3. Write your logic where it says "YOUR CODE HERE".
!   4. Rebuild: run  ./build.sh  from the project root.
!   5. Restart the Python UI (application.py).
!   6. In the Spawn Editor, add "Test Module (Agents)" or
!      "Test Module (Grid)" to your active module list.
!
! IMPORTANT NOTES:
!   - You do NOT need to add config variables. Define everything you need
!     as local variables or module-level variables (with "save") below.
!   - Use "print *, ..." to print debug output to the terminal.
!     Always call "call flush(6)" right after to make sure it shows up.
!   - To see which tick you are on, use the module variable "current_tick".
!   - If the simulation crashes after your changes, check the terminal
!     output for error messages and see the Debugging chapter in the
!     Interface Documentation.
!
! =============================================================================

module mod_test_modules

    use mod_config
    use mod_agent_world
    use mod_grid_id

    implicit none

    ! =========================================================================
    ! MODULE-LEVEL VARIABLES
    ! =========================================================================
    ! These variables persist across ticks. Use them for counters, flags, etc.
    ! The "save" attribute keeps the value between calls.
    !
    ! Example:
    !   integer, save :: my_counter = 0
    !   logical, save :: my_flag = .false.
    !   real(8), save :: my_accumulator = 0.0d0
    ! =========================================================================

    integer, save :: current_tick = 0

contains

    ! =========================================================================
    ! set_test_module_tick
    ! =========================================================================
    ! Called automatically before your modules run. Do NOT modify this.
    ! =========================================================================
    subroutine set_test_module_tick(t)
        implicit none
        integer, intent(in) :: t
        current_tick = t
    end subroutine set_test_module_tick


    ! =========================================================================
    ! SUBROUTINE: test_module_agents
    ! =========================================================================
    !
    ! This function is called ONCE FOR EVERY LIVING AGENT, every tick.
    ! Think of it as: "for each person in the simulation, do ..."
    !
    ! ---- WHAT YOU CAN READ FROM THE AGENT (agent_ptr%...) ----
    !
    !   agent_ptr%pos_x        : longitude position (real(8))
    !   agent_ptr%pos_y        : latitude  position (real(8))
    !   agent_ptr%vel_x        : velocity x (real(8))
    !   agent_ptr%vel_y        : velocity y (real(8))
    !   agent_ptr%age          : age in weeks (integer)
    !   agent_ptr%gender       : 'M' or 'F' (character)
    !   agent_ptr%is_pregnant   : 0 = no, >0 = weeks pregnant (integer)
    !   agent_ptr%is_dead      : .true. / .false. (logical)
    !   agent_ptr%resources    : current resources (integer)
    !   agent_ptr%avg_resources : average resources (real(8))
    !   agent_ptr%population   : which population (1, 2, ...) (integer)
    !   agent_ptr%children     : number of children (integer)
    !   agent_ptr%id           : unique agent ID (integer)
    !
    ! ---- WHAT YOU CAN DO ----
    !
    !   Move the agent:
    !     call agent_ptr%update_pos(new_x, new_y)
    !
    !   Kill the agent:
    !     call agent_ptr%agent_dies(reason=6)
    !     (reason 6+ = custom; see mod_birth_death_new.f95 for codes 1-5)
    !
    !   Change agent properties:
    !     agent_ptr%resources = agent_ptr%resources + 10
    !     agent_ptr%vel_x = 0.0d0
    !
    !   Print debug info:
    !     print *, "Agent", agent_ptr%id, "pos:", agent_ptr%pos_x
    !     call flush(6)
    !
    !   Use the current tick:
    !     if (current_tick == 100) then ... end if
    !
    ! =========================================================================

    subroutine test_module_agents(agent_ptr)
        implicit none
        type(Agent), pointer, intent(inout) :: agent_ptr

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================
        ! Examples:
        !   real(8) :: distance
        !   integer :: random_number
        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================
        ! This runs for EVERY living agent, every tick.
        !
        ! Example 1: Print a message at tick 50
        !   if (current_tick == 50) then
        !       print *, "Hello from agent", agent_ptr%id
        !       call flush(6)
        !   end if
        !
        ! Example 2: Kill agents older than 3000 weeks
        !   if (agent_ptr%age > 3000) then
        !       call agent_ptr%agent_dies(reason=6)
        !   end if
        !
        ! Example 3: Move agents slightly to the right each tick
        !   call agent_ptr%update_pos( &
        !       agent_ptr%pos_x + 0.001d0, &
        !       agent_ptr%pos_y)
        ! =====================================================================


    end subroutine test_module_agents


    ! =========================================================================
    ! SUBROUTINE: test_module_grid
    ! =========================================================================
    !
    ! This function is called ONCE PER TICK on the entire simulation.
    ! Use it when you want to work with the spatial grid, iterate over
    ! cells, or do things that affect all agents at once.
    !
    ! ---- WHAT YOU CAN ACCESS (via w%...) ----
    !
    !   Grid cells:
    !     w%config%dlon_hep                    : number of grid columns (integer)
    !     w%config%dlat_hep                    : number of grid rows    (integer)
    !     w%grid%cell(gx, gy)                  : a single grid cell
    !     w%grid%cell(gx, gy)%number_of_agents : how many agents in this cell
    !     w%grid%cell(gx, gy)%agents_ids(k)    : the k-th agent's ID in this cell
    !
    !   Get an agent from its ID:
    !     agent_ptr => get_agent(agent_id, w)
    !
    !   Population info:
    !     w%config%npops                       : number of populations
    !     w%num_humans(pop)                     : alive agent count for pop
    !
    !   Agents directly (without grid):
    !     w%agents(k, pop)                     : the k-th agent of population pop
    !
    ! ---- WHAT YOU CAN DO ----
    !
    !   Loop over all grid cells:
    !     do gy = 1, ny
    !         do gx = 1, nx
    !             n = w%grid%cell(gx, gy)%number_of_agents
    !             ...
    !         end do
    !     end do
    !
    !   Move an agent (updates grid cell associations automatically):
    !     call agent_ptr%update_pos(new_x, new_y)
    !
    !   Kill an agent:
    !     call agent_ptr%agent_dies(reason=6)
    !
    !   Print debug info:
    !     print *, "Cell (", gx, ",", gy, ") has", n, "agents"
    !     call flush(6)
    !
    ! =========================================================================

    subroutine test_module_grid(w)
        implicit none
        class(world_container), target, intent(inout) :: w

        ! =====================================================================
        ! DECLARE YOUR VARIABLES HERE
        ! =====================================================================

        ! =====================================================================


        ! =====================================================================
        ! YOUR CODE HERE
        ! =====================================================================
     
        ! =====================================================================


    end subroutine test_module_grid

end module mod_test_modules
