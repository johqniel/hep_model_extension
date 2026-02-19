module mod_birth_death_agb

    use mod_constants
    use mod_config
    use mod_hashmap
    use mod_rnorm
    use mod_grid_id
    use mod_agent_world
    use mod_calculations

    implicit none

    contains

! =================================================================
! SUBROUTINE: realise_natural_deaths
!
! Implements natural mortality based on age.
! Logic:
!   - Calculates a probability based on age using calc natural death function.
!   - If random < prob, the agent dies (Reason: 1).
! =================================================================
subroutine realise_natural_deaths(current_agent)
    type(Agent), pointer, intent(inout) :: current_agent
    real :: r ! random number 

    call random_number(r)

    if ( r < calc_natural_death_prob(current_agent%age, current_agent%world%config)) then
        call agent_dies(current_agent, reason=1)
    end if

end subroutine realise_natural_deaths

! helper function of module above (realise_natural_deaths)
real function calc_natural_death_prob(age, config) result(prob)
    implicit none
    integer, intent(in) :: age ! in ticks
    type(world_config), intent(in) :: config

    integer :: x
    integer :: age_in_years

    age_in_years = age / 52

    if (age_in_years > config%agb_age_min .and. age_in_years < config%agb_age_max) then
        x = (2 * config%agb_age_min - age)
    endif

    if ( age_in_years > (config%agb_age_max - 1) ) then
        x = 0
    endif

    prob = config%agb_f0 * (1 / (log(real(config%agb_M) * real(x + 1))+1))

    ! natural death prob per tick starts at x% per tick for newborns and then 
    ! goes down and eventually goes back up 

end function calc_natural_death_prob


! =================================================================
! SUBROUTINE: find_mate
!
! Enables reproduction by allowing agents to find partners.
! Logic:
!   - Active for Females (checks if male partners exist in cell).
!   - Checks conditions: Age, Resources.
!   - If successful, agent becomes pregnant.
!
!
! ARCHIEVED FOR NOW MIGHT BE REACTIVATED IN THE FUTURE, DEPENDS ON MODELING CHOICES
! DN: 11.02.26
! =================================================================
subroutine find_mate(current_agent)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent

    logical :: ressources_module_active

    type(world_container), pointer :: world_ptr
    integer :: potential_partner_index_in_cell
    integer :: num_agents_in_cell
    real :: r
    type(Agent), pointer :: potential_partner
    type(Grid), pointer :: grid_ptr

    world_ptr => current_agent%world
    grid_ptr => current_agent%grid

    ressources_module_active = current_agent%world%ressources_module_active

    if (current_agent%gender == 'M') then
        ! Female search model
        return
    end if

    if (current_agent%is_pregnant > 0) then
        ! agent can not be twice pregnant
        return
    endif

    if (current_agent%age < world_ptr%config%age_when_fertile_f) then
        ! agent is to young to get pregnant
        return
    endif


    num_agents_in_cell = grid_ptr%cell(current_agent%gx, current_agent%gy)%number_of_agents

    if (num_agents_in_cell < 2) then
        ! no potential partners
        return
    endif

    call random_number(r)

    potential_partner_index_in_cell = int(r * num_agents_in_cell) + 1

    potential_partner => get_ith_agent_from_cell(world_ptr, potential_partner_index_in_cell, current_agent%gx, current_agent%gy)

    if (.not. associated(potential_partner)) then
        ! Should not happen if grid/map consistent, but safety first
        return
    endif

    ! misses with prob: 1 / num_agents_in_cell
    if (potential_partner%id == current_agent%id) then
        ! same agent
        return
    endif

    ! misses with prob: 1 / 2
    if (potential_partner%gender == 'F') then
        ! potential partner is female
        return
    endif

    ! misses with prob that depends on age distribution
    if (potential_partner%age < world_ptr%config%age_when_fertile_m) then
        ! potential partner is to young to get pregnant
        return
    endif


    ! check if ressources module is active
    if (ressources_module_active) then
        ! check if potential partner has enough resources
        if (current_agent%resources < world_ptr%config%min_resources_for_mating) then
            return
        end if

    end if


    ! probability of vertilisation per tick should be adjusted to the three miss probabilities above
    call random_number(r)
    if (r > world_ptr%config%probability_vertilisation_per_tick) then
        ! agent does not get pregnant
        return
    endif

    ! agent gets pregnant
    current_agent%is_pregnant = 1

    current_agent%father_of_unborn_child = potential_partner%id

end subroutine find_mate



! =================================================================
! SUBROUTINE: distribute_ressources
!
! THIS TRIES TO CONSTRAIN BIRTH IN A "EMERGENT FROM MICRO MODEL WAY" RATHER THAN A MACRO PROBABILITY, BY LINKING IT TO RESOURCES.
! ARCHIVED FOR NOW MIGHT BE REACTIVATED IN THE FUTURE, DEPENDS ON MODELING CHOICES
! DN: 11.02.26
!
! Allocates resources from grid cells to agents.
! Logic:
!   - Resources produced proportional to HEP.
!   - Agents compete for resources in rounds.
!   - Updates agent's avg_resources history.
!
!
!
!
! =================================================================
subroutine distribute_ressources(world)
    implicit none
    class(world_container), target, intent(inout) :: world
    
    integer :: i, j, k, jp
    integer :: res_amount
    type(Agent), pointer :: current_agent
    integer :: num_agents_in_cell
    integer :: agent_id
    integer :: picked
    integer :: total_demand
    real(8) :: sum_res
    real(8) :: max_hep
    real(8) :: max_possible_res
    integer :: min_res_floor
    real(8) :: res_range
    integer, allocatable :: min_agent_res(:,:)
    integer :: min_res_val
    integer :: bg_color
    
    allocate(min_agent_res(world%grid%nx, world%grid%ny))
    min_agent_res = 999
    
    ! 0. Reset Resources & Find Max Capacity
    max_possible_res = 1.0
    
    ! 0. Reset Resources
    ! Reset Agents
    do jp = 1, world%config%npops
        do k = 1, world%num_humans(jp)
            current_agent => world%agents(k, jp)
            if (current_agent%is_dead) cycle
            current_agent%resources = 0
        end do
    end do
    
    ! Reset Cells and 1. Add Resources to Cells
    do i = 1, world%grid%nx
        do j = 1, world%grid%ny
            world%grid%cell(i,j)%resources = 0
            
            ! Add resources proportional to HEP
            ! Assuming HEP is available in grid%hep_av (average HEP) or grid%hep (time dependent)
            ! Let's use hep_av for simplicity or check if t_hep is available.
            ! grid%hep is (nx, ny, npops, nt).
            ! But resources are per cell, not per pop?
            ! The user said "proportional to the hep of that cell".
            ! HEP is usually per population.
            ! Let's take the max HEP across populations in that cell? Or sum?
            ! Or just use the first population?
            ! Let's use the max HEP across populations to represent the environment's capacity.
            
            ! Find max HEP to determine potential capacity
            max_hep = -1.0
            do jp = 1, world%config%npops
                if (world%grid%hep_av(i,j,jp) > max_hep) then
                    max_hep = world%grid%hep_av(i,j,jp)
                endif
            end do
            
            ! Only distribute resources on land (is_water == 0)
            if (world%grid%cell(i,j)%is_water == 0) then
                if (max_hep > 0.0) then
                    ! Scale factor from config
                    world%grid%cell(i,j)%resources = int(max_hep * real(world%config%ressources_per_hep))
                else
                    ! HEP is 0 or negative (but land), start with 0
                    world%grid%cell(i,j)%resources = 0
                endif
                
                ! Apply Minimum Resource Floor
                if (world%grid%cell(i,j)%resources < world%config%min_resources_per_gridcell) then
                    world%grid%cell(i,j)%resources = world%config%min_resources_per_gridcell
                endif

                ! Track max for visualization
                if (real(world%grid%cell(i,j)%resources) > max_possible_res) then
                    max_possible_res = real(world%grid%cell(i,j)%resources)
                endif
            endif
        end do
    end do
    
    ! 2. Round 1: Pick up to 5
    do jp = 1, world%config%npops
        do k = 1, world%num_humans(jp)
            current_agent => world%agents(k, jp)
            if (current_agent%is_dead) cycle
            
            if (current_agent%gx > 0 .and. current_agent%gy > 0) then
                res_amount = world%grid%cell(current_agent%gx, current_agent%gy)%resources
                picked = min(5, res_amount)
                if (picked > 0) then
                    current_agent%resources = current_agent%resources + picked
                    world%grid%cell(current_agent%gx, current_agent%gy)%resources = res_amount - picked
                endif
            endif
        end do
    end do
    
    ! 3. Round 2: Pick up to 2
    do jp = 1, world%config%npops
        do k = 1, world%num_humans(jp)
            current_agent => world%agents(k, jp)
            if (current_agent%is_dead) cycle
            
            if (current_agent%gx > 0 .and. current_agent%gy > 0) then
                res_amount = world%grid%cell(current_agent%gx, current_agent%gy)%resources
                picked = min(2, res_amount)
                if (picked > 0) then
                    current_agent%resources = current_agent%resources + picked
                    world%grid%cell(current_agent%gx, current_agent%gy)%resources = res_amount - picked
                endif
            endif
        end do
    end do
    
    ! 4. Update Average
    do jp = 1, world%config%npops
        do k = 1, world%num_humans(jp)
            current_agent => world%agents(k, jp)
            if (current_agent%is_dead) cycle
            
            ! Update history
            current_agent%resource_history(current_agent%history_idx) = current_agent%resources
            current_agent%history_idx = mod(current_agent%history_idx, 12) + 1
            
            ! Compute average
            sum_res = 0.0
            do i = 1, 12
                sum_res = sum_res + real(current_agent%resource_history(i))
            end do
            current_agent%avg_resources = sum_res / 12.0
            
            ! Track min satisfaction for visualization
            if (current_agent%gx > 0 .and. current_agent%gy > 0) then
                if (current_agent%resources < min_agent_res(current_agent%gx, current_agent%gy)) then
                    min_agent_res(current_agent%gx, current_agent%gy) = current_agent%resources
                endif
            endif
        end do
    end do
    
    ! 5. Debug: Visual Grid Print (Double Line)
    print *, "=== Resource Grid (End of Tick) ==="
    print *, "Format: [Capacity Color] Top: Res | Bottom: Agents (Red=<5, Yellow=<7, Black=OK)"
    
    do j = world%grid%ny, 1, -1
        ! Line 1: Resources (Gradient Background)
        do i = 1, world%grid%nx
            if (world%grid%cell(i,j)%is_water == 1) then
                write(*, '(A)', advance='no') ACHAR(27)//"[44;37m ~~~ "//ACHAR(27)//"[0m"
            else
                res_amount = world%grid%cell(i,j)%resources
                min_res_floor = world%config%min_resources_per_gridcell
                
                if (max_possible_res > min_res_floor) then
                    res_range = max_possible_res - real(min_res_floor)
                else
                    res_range = 1.0 ! Avoid div by zero or negative logic
                endif
                
                if (res_amount <= min_res_floor) then
                    bg_color = 250
                else
                        if (res_amount < min_res_floor + res_range * 0.25) then
                            bg_color = 150
                        elseif (res_amount < min_res_floor + res_range * 0.50) then
                            bg_color = 113
                        elseif (res_amount < min_res_floor + res_range * 0.75) then
                            bg_color = 40
                        else
                            bg_color = 22
                        endif
                endif
                
                ! Calculate Gradient 0 (Grey) -> Max (Dark Green)
                ! Mapping to ANSI 256 colors: 250 (Grey) -> 118 (bt.Green) -> 46 (Green) -> 22 (Dark Green)
                ! Simplified:
                ! 0: 250 (Grey)
                ! >0 - 25%: 150 (Pale Green)
                ! 25-50%: 113 (Medium)
                ! 50-75%: 40 (Green)
                ! >75%: 22 (Dark Green)
                
                ! Print Res amount (Black text usually readable on these greens, maybe white on 22)
                if (bg_color == 22) then
                        write(*, '(A,I3,A,A,I3,A)', advance='no') &
                            ACHAR(27)//"[48;5;", bg_color, "m", &
                            ACHAR(27)//"[37m", res_amount, " "//ACHAR(27)//"[0m"
                else
                        write(*, '(A,I3,A,A,I3,A)', advance='no') &
                            ACHAR(27)//"[48;5;", bg_color, "m", &
                            ACHAR(27)//"[30m", res_amount, " "//ACHAR(27)//"[0m"
                endif
            endif
        end do
        print * ! End of Line 1
        
        ! Line 2: Agents (Condition Colored Text)
        do i = 1, world%grid%nx
                if (world%grid%cell(i,j)%is_water == 1) then
                write(*, '(A)', advance='no') ACHAR(27)//"[44;37m ~~~ "//ACHAR(27)//"[0m"
            else
                ! Re-calculate background for consistency
                res_amount = world%grid%cell(i,j)%resources
                
                if (res_amount <= min_res_floor) then
                    bg_color = 250
                else
                    if (res_amount < min_res_floor + res_range * 0.25) then
                        bg_color = 150
                    elseif (res_amount < min_res_floor + res_range * 0.50) then
                        bg_color = 113
                    elseif (res_amount < min_res_floor + res_range * 0.75) then
                        bg_color = 40
                    else
                        bg_color = 22
                    endif
                endif
                
                ! Agent text color
                min_res_val = min_agent_res(i,j)
                if (world%grid%cell(i,j)%number_of_agents == 0) then
                        ! No agents, black/standard text
                        if (bg_color == 22) then
                        write(*, '(A,I3,A,A,A)', advance='no') &
                            ACHAR(27)//"[48;5;", bg_color, "m", &
                            ACHAR(27)//"[37m", "  -  "//ACHAR(27)//"[0m"
                        else
                        write(*, '(A,I3,A,A,A)', advance='no') &
                            ACHAR(27)//"[48;5;", bg_color, "m", &
                            ACHAR(27)//"[30m", "  -  "//ACHAR(27)//"[0m"
                        endif
                else
                    ! Check conditions
                    if (min_res_val < 5) then
                        ! Red Text (31)
                        write(*, '(A,I3,A,A,I4,A)', advance='no') &
                            ACHAR(27)//"[48;5;", bg_color, "m", &
                            ACHAR(27)//"[31;1m", world%grid%cell(i,j)%number_of_agents, " "//ACHAR(27)//"[0m"
                    elseif (min_res_val < 7) then
                        ! Yellow Text (33)
                        write(*, '(A,I3,A,A,I4,A)', advance='no') &
                            ACHAR(27)//"[48;5;", bg_color, "m", &
                            ACHAR(27)//"[33;1m", world%grid%cell(i,j)%number_of_agents, " "//ACHAR(27)//"[0m"
                    else
                            ! Demand Met - Black (30) (or White on dark)
                        if (bg_color == 22) then
                            write(*, '(A,I3,A,A,I4,A)', advance='no') &
                                ACHAR(27)//"[48;5;", bg_color, "m", &
                                ACHAR(27)//"[37m", world%grid%cell(i,j)%number_of_agents, " "//ACHAR(27)//"[0m"
                        else
                            write(*, '(A,I3,A,A,I4,A)', advance='no') &
                                ACHAR(27)//"[48;5;", bg_color, "m", &
                                ACHAR(27)//"[30m", world%grid%cell(i,j)%number_of_agents, " "//ACHAR(27)//"[0m"
                        endif
                    endif
                endif
                endif
        end do
        print * ! End of Line 2
    end do
    
    deallocate(min_agent_res)

end subroutine distribute_ressources


! =================================================================
! SUBROUTINE: resource_mortality
!
! Kills agents that fall below a resource threshold.
! Logic:
!   - Checks avg_resources.
!   - If avg_resources < threshold, agent dies (Reason: 2).
! =================================================================
subroutine resource_mortality(current_agent)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent
    
    if (current_agent%is_dead) return
    
    ! Check if average resources are below threshold
    if (current_agent%avg_resources < current_agent%world%config%min_avg_resources_for_survival) then
        print*, "dying agents avg resources: ", current_agent%avg_resources
        call agent_dies(current_agent, reason=2)
    endif
    
end subroutine resource_mortality

end module mod_birth_death_agb
