module mod_birth_technical

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
! SUBROUTINE: realise_births
!
! Handles the birth of new agents from pregnant females.
! Logic:
!   - Checks pregnancy length > minimum.
!   - Checks random probability.
!   - If successful, spawns new agent (age=0) at mother's location.
! =================================================================
subroutine realise_births(current_agent)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent

    type(Agent) :: new_agent
    type(Agent), pointer :: father_ptr

    real :: r ! random number

    type(Grid), pointer :: grid_p
    type(Agent) :: child
    type(Agent), pointer :: father, mother
    integer :: parent_one_id, parent_two_id, population

    if (current_agent%is_pregnant == 0) then
        ! agent is not pregnant
        return
    endif

    ! ELSE: 

    if (current_agent%is_pregnant < current_agent%world%config%pregnancy_minimum_length) then
        ! pregnancy is not done yet
        return
    endif


    call random_number(r)

    if (r > current_agent%world%config%birth_prob_after_min_length) then
        ! the pregnancy is not done yet
        return
    endif


    parent_one_id = current_agent%id
    parent_two_id = current_agent%father_of_unborn_child

    population = current_agent%population


    ! birth occurs

    father_ptr => get_agent(parent_two_id, current_agent%world)
    
    if (.not. associated(father_ptr)) then
        ! Father died during pregnancy.
        father_ptr => current_agent
    endif

    new_agent = generate_agent_born(current_agent%world, current_agent, father_ptr)

    call add_agent_to_array_hash(current_agent%world, new_agent, new_agent%population)


end subroutine realise_births

! =================================================================
! SUBROUTINE: update_age_pregnancy
!
! Increments agent age and pregnancy duration.
! Logic:
!   - age = age + 1
!   - if is_pregnant > 0, is_pregnant = is_pregnant + 1
! =================================================================
subroutine update_age_pregnancy(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        current_agent%age = current_agent%age + 1

        if (current_agent%is_pregnant > 0) then
            current_agent%is_pregnant = current_agent%is_pregnant + 1
        end if

end subroutine update_age_pregnancy

end module mod_birth_technical
