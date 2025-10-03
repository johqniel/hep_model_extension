module mod_age_pregnancy

    use mod_agent_class
    use mod_grid
    use mod_agent_tracking


    implicit none

    contains

    subroutine update_age_pregnancy(current_agent)
        implicit none
        type(Node), pointer, intent(inout) :: current_agent

        current_agent%age = current_agent%age + 1

        if (current_agent%is_pregnant > 0) then
            current_agent%is_pregnant = current_agent%is_pregnant + 1
        end if

    end subroutine update_age_pregnancy


subroutine realise_births(current_agent)
    !class(spatial_grid), pointer, intent(inout) :: grid
    type(Node), pointer, intent(inout) :: current_agent
    !type(spatial_grid), pointer :: grid_p
    real :: r ! random number


    if (current_agent%is_pregnant < pregnancy_minimum_length) then
        ! pregnancy is not done yet
        return
    endif


    call random_number(r)

    if (r > birth_prob_after_min_length) then
        ! the pregnancy is not done yet
        return
    endif


    ! birth occurs
    select type(grid_p => current_agent%grid)

    type is (spatial_grid)

        if (.not. associated(grid_p)) then
            print*, "agent has unassociated %grid in realise birth."
        endif

        call agent_born_place_in_grid(current_agent%position_population,grid_p,current_agent, &
                                      current_agent%father_of_unborn_child)
        current_agent%is_pregnant = 0
        current_agent%father_of_unborn_child => null()
    class default
        print*, "current_agent%grid is not spatial grid."
        return
    end select

    realised_birth_counter = realised_birth_counter + 1


                

end subroutine realise_births

subroutine realise_natural_deaths(current_agent)
    type(Node), pointer, intent(inout) :: current_agent
    real :: r ! random number 

    call random_number(r)

    if ( r < calc_natural_death_prob(current_agent%age)) then
        call current_agent%agent_die()
    end if

end subroutine realise_natural_deaths

real function calc_natural_death_prob(age) result(prob)
    implicit none
    integer, intent(in) :: age ! in ticks

    integer :: x
    integer :: age_in_years

    age_in_years = age / 52



    if (age_in_years > 40 .and. age_in_years < 80) then
        x = (2 * 40 - age)
    endif

    if ( age_in_years > 79 ) then
        x = 0
    endif

    

    prob = 0.0025 * (1 / (log(200 * real(x + 1))+1))
    ! natural death prob per tick starts at 2% per tick for newborns and then 
    

end function calc_natural_death_prob





end module mod_age_pregnancy