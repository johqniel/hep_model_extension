module mod_age_pregnancy

    use mod_agent_class
    use mod_grid
    use mod_agent_tracking


    implicit none

    contains

    subroutine update_age_pregnancy(t)
        integer, intent(in) :: t

        type(Node), pointer :: current_agent

        current_agent => head_agents

        do while (associated(current_agent))
        
            current_agent%age = current_agent%age + 1

            if (current_agent%is_pregnant > 0) then
                current_agent%is_pregnant = current_agent%is_pregnant + 1
            end if

            current_agent => current_agent%next

        enddo



    end subroutine update_age_pregnancy


subroutine realise_births(t)
    !class(spatial_grid), pointer, intent(inout) :: grid
    integer, intent(in) :: t

    type(Node), pointer :: current_agent
    real :: r ! random number

    current_agent => head_agents

    do while (associated(current_agent))

        if (current_agent%is_pregnant > 37) then
            call random_number(r)
            if (r < 0.6) then
                ! birth occurs
                call agent_born_place_in_grid(current_agent%position_population,current_agent%grid,current_agent, &
                                              current_agent%father_of_unborn_child)
                current_agent%is_pregnant = 0
                current_agent%father_of_unborn_child => null()

                
            endif
        endif



        current_agent => current_agent%next
    enddo



end subroutine realise_births




end module mod_age_pregnancy