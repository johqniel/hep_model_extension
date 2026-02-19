! =============================================================================
! Module: mod_birth_death
!
! Description:
!   Birth-death controller that adjusts agent counts per grid cell towards
!   a target computed by calc_target_agents_in_cell (from mod_birth_death_target).
!
!   For each cell:
!     - If current agents > target  →  randomly kill (current - target) agents
!     - If current agents < target  →  spawn (target - current) new agents
!
! Integration (add to main.f95 timestep loop):
!
!   use mod_birth_death
!
!   ! Inside the timestep loop, after movement:
!   do jp = 1, world%config%npops
!       call apply_birth_death_all_cells(world, jp)
!   end do
!   call compact_agents(world)
!
! Dependencies:
!   mod_birth_death_target  (calc_target_agents_in_cell)
!   mod_agent_world         (Agent, world_container, agent_dies,
!                            spawn_agent_hash, add_agent_to_array_hash,
!                            get_agent, get_ith_agent_from_cell)
!   mod_grid_id             (Grid, grid_cell, calculate_grid_pos)
!   mod_config              (world_config)
! =============================================================================

module mod_birth_death_strict

    use mod_birth_death_target
    use mod_agent_world
    use mod_grid_id
    use mod_config

    implicit none

contains

    ! =================================================================
    ! SUBROUTINE: apply_birth_death_to_cell
    !
    ! For a single grid cell (gx, gy) and population jp:
    !   1. Compute target agent count via calc_target_agents_in_cell
    !   2. Kill surplus agents or spawn missing agents
    !
    ! Arguments:
    !   world                   : the world_container (inout)
    !   gx, gy                 : grid cell coordinates
    !   jp                     : population index
    !   carrying_capacity_scale : HEP → carrying-capacity multiplier
    !                             (optional, passed through to target func)
    !   growth_rate             : intrinsic growth rate
    !                             (optional, passed through to target func)
    ! =================================================================

    subroutine apply_birth_death_to_cell(world, gx, gy, jp,  &
                                          carrying_capacity_scale, &
                                          growth_rate)
        implicit none
        class(world_container), target, intent(inout) :: world
        integer, intent(in) :: gx, gy, jp
        real(8), intent(in), optional :: carrying_capacity_scale
        real(8), intent(in), optional :: growth_rate

        ! Local variables
        integer :: n_current, n_target, diff
        integer :: i, k, kill_count
        real(8) :: hep_value
        real(8) :: cc_scale_local, r_local
        type(Grid), pointer :: grid
        type(Agent), pointer :: agent_ptr
        type(Agent) :: new_agent
        real :: rand_val

        ! Pointers
        grid => world%grid

        ! ---------------------------------------------------------
        ! 0. Resolve optional parameters
        ! ---------------------------------------------------------
        if (present(carrying_capacity_scale)) then
            cc_scale_local = carrying_capacity_scale
        else
            cc_scale_local = world%config%strict_cc_scale
        end if

        if (present(growth_rate)) then
            r_local = growth_rate
        else
            r_local = world%config%strict_growth_rate
        end if

        ! ---------------------------------------------------------
        ! 1. Gather cell info
        ! ---------------------------------------------------------
        n_current = grid%cell(gx, gy)%number_of_agents

        ! Get HEP value for this cell/population
        ! Use the time-averaged HEP (hep_av) for stability
        if (allocated(grid%hep_av)) then
            hep_value = grid%hep_av(gx, gy, jp)
        else if (allocated(grid%hep)) then
            hep_value = grid%hep(gx, gy, jp, grid%t_hep)
        else
            ! No HEP data available — skip this cell
            return
        end if

        ! Skip water cells (HEP == -1 signals water)
        if (hep_value <= 0.0d0) return

        ! ---------------------------------------------------------
        ! 2. Compute target
        ! ---------------------------------------------------------
        n_target = calc_target_agents_in_cell(n_current, hep_value, &
                                               cc_scale_local, r_local)

        diff = n_current - n_target

        ! ---------------------------------------------------------
        ! 3a. Too many agents → kill surplus
        ! ---------------------------------------------------------
        if (diff > 0) then
            call kill_surplus_agents(world, grid, gx, gy, jp, diff)
        end if

        ! ---------------------------------------------------------
        ! 3b. Too few agents → spawn deficit
        ! ---------------------------------------------------------
        if (diff < 0) then
            call spawn_deficit_agents(world, grid, gx, gy, jp, -diff)
        end if

    end subroutine apply_birth_death_to_cell


    ! =================================================================
    ! SUBROUTINE: apply_birth_death_all_cells
    !
    ! Driver that loops over the entire grid and calls
    ! apply_birth_death_to_cell for each land cell.
    !
    ! Arguments:
    !   world                   : the world_container (inout)
    !   jp                     : population index
    !   carrying_capacity_scale : (optional) passed through
    !   growth_rate             : (optional) passed through
    ! =================================================================

    subroutine apply_birth_death_all_cells(world, jp, &
                                            carrying_capacity_scale, &
                                            growth_rate)
        implicit none
        class(world_container), target, intent(inout) :: world
        integer, intent(in) :: jp
        real(8), intent(in), optional :: carrying_capacity_scale
        real(8), intent(in), optional :: growth_rate

        integer :: i, j
        type(Grid), pointer :: grid

        grid => world%grid

        do i = 1, grid%nx
            do j = 1, grid%ny
                ! Skip water
                if (grid%cell(i, j)%is_water == 1) cycle

                call apply_birth_death_to_cell(world, i, j, jp, &
                                                carrying_capacity_scale, &
                                                growth_rate)
            end do
        end do

    end subroutine apply_birth_death_all_cells


    ! =================================================================
    ! PRIVATE HELPER: kill_surplus_agents
    !
    ! Randomly selects and kills `n_to_kill` agents from cell (gx, gy).
    ! Only kills agents belonging to population jp.
    !
    ! Strategy: iterate through agents in the cell, and for each one
    ! that belongs to the target population, kill it with a probability
    ! that ensures roughly n_to_kill deaths.
    ! =================================================================

    subroutine kill_surplus_agents(world, grd, gx, gy, jp, n_to_kill)
        implicit none
        type(world_container), target, intent(inout) :: world
        type(Grid), pointer, intent(in) :: grd
        integer, intent(in) :: gx, gy, jp, n_to_kill

        integer :: k, killed, num_in_cell, agent_id
        real :: rand_val
        real :: kill_prob
        type(Agent), pointer :: agent_ptr

        killed = 0
        num_in_cell = grd%cell(gx, gy)%number_of_agents

        if (num_in_cell <= 0) return

        ! Probability per agent to achieve roughly n_to_kill deaths
        kill_prob = real(n_to_kill) / real(num_in_cell)
        if (kill_prob > 1.0) kill_prob = 1.0

        ! Walk through agents in this cell
        ! NOTE: we iterate from the end to avoid index shifting issues
        !       when agents are marked dead
        do k = num_in_cell, 1, -1
            if (killed >= n_to_kill) exit

            if (k > grd%cell(gx, gy)%number_of_agents) cycle

            agent_id = grd%cell(gx, gy)%agents_ids(k)
            if (agent_id <= 0) cycle

            agent_ptr => get_agent(agent_id, world)
            if (.not. associated(agent_ptr)) cycle
            if (agent_ptr%is_dead) cycle
            if (agent_ptr%population /= jp) cycle

            ! Stochastic kill decision
            call random_number(rand_val)
            if (rand_val < kill_prob) then
                call agent_ptr%agent_dies(reason=5)  ! reason 5 = random/birth-death
                killed = killed + 1
            end if
        end do

    end subroutine kill_surplus_agents


    ! =================================================================
    ! PRIVATE HELPER: spawn_deficit_agents
    !
    ! Spawns `n_to_spawn` new agents in cell (gx, gy) for population jp.
    ! New agents are placed at the centre of the grid cell.
    !
    ! Uses existing spawn_agent_hash + add_agent_to_array_hash, the
    ! same pattern as realise_births in mod_modules_hash.
    ! =================================================================

    subroutine spawn_deficit_agents(world, grd, gx, gy, jp, n_to_spawn)
        implicit none
        type(world_container), target, intent(inout) :: world
        type(Grid), pointer, intent(in) :: grd
        integer, intent(in) :: gx, gy, jp, n_to_spawn

        integer :: s
        type(Agent) :: new_agent
        real(8) :: spawn_x, spawn_y

        ! Place new agents at the cell centre
        if (allocated(grd%lon_hep) .and. allocated(grd%lat_hep)) then
            spawn_x = grd%lon_hep(gx)
            spawn_y = grd%lat_hep(gy)
        else
            ! Fallback: cannot determine position, skip spawning
            return
        end if

        do s = 1, n_to_spawn

            ! Create a new agent using the existing factory
            new_agent = world%spawn_agent_hash(jp)

            ! Set position to cell centre
            new_agent%pos_x = spawn_x
            new_agent%pos_y = spawn_y

            ! Age = 0 for newly spawned agents
            new_agent%age = 0

            ! Add to world data structures (array, hashmap, grid)
            call add_agent_to_array_hash(world, new_agent, jp)

        end do

    end subroutine spawn_deficit_agents

end module mod_birth_death_strict
