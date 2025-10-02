module mod_birth_death_example

    use mod_agent_class

    use mod_agent_tracking

    use mod_grid

    use mod_calculations
    ! Uses: 
    !
    !       subroutine calculate_grid_pos


    ! Constants
    !use mod_globals

    integer :: max_agents_per_cell = 25


contains

subroutine death_example(grid)
    implicit none
    type(spatial_grid), pointer, intent(inout) :: grid

    integer :: nx, ny
    integer :: i, j

    integer :: agents_exces_count
    nx = grid%nx
    ny = grid%ny


    do i = 1, nx
        do j = 1, ny

            agents_exces_count = 0

            if (grid%cell(i,j)%number_of_agents < max_agents_per_cell) then
                ! nothing to do
                cycle
            endif
            
            agents_exces_count = grid%cell(i,j)%number_of_agents - max_agents_per_cell

            !print*, "Going to kill: ", agents_exces_count, " agents in cell with: ", &
            !            grid%cell(i,j)%number_of_agents, "agents."

            if (agents_exces_count == 0) then
                ! nothing to do
                cycle
            endif

           

            ! Semi-Randomly kill an agent in the cell
            call kill_n_agents_in_cell(grid,i,j,max(agents_exces_count,10))

        

            
        end do
    end do


end subroutine death_example

subroutine find_mate(female)
    implicit none
    type(Node), pointer, intent(inout) :: female


    type(Node), pointer :: selected_male
    type(pointer_node), pointer :: current_agent_ptr
    integer :: i, j
    real :: r


    if (.not. associated(female)) then
        print*, "Error: Agent not associated in find mate."
        return
    endif

    if (female%gender /= 'F') then
        ! "We use a female choice mating model. "
        return
    endif

    if (female%is_pregnant > 0 ) then
        ! Female already pregnant
        return
    endif

    if(female%age < age_when_vertile_f) then  
        !to young to be pregnant
        return
                    
    endif

    if (female%age > age_until_vertile_f) then
        ! female to old to be pregnant
        return
    endif

    if (.not. associated(female%grid)) then
        print*, "Error: Agent with  %grid not associated, in function find mate."
        return
    endif

    call calculate_grid_pos(female%pos_x, female%pos_y, i, j)
    current_agent_ptr => null()
    selected_male => null()


    select type(grid_p => female%grid)


    type is (spatial_grid)
        current_agent_ptr => grid_p%cell(i,j)%agents
    class default
        print*, "current_agent%grid is not spatial grid."
    end select


    do while (associated(current_agent_ptr))

        ! A: Check if potential partner associated
        if (.not. associated(current_agent_ptr%node)) then
            print*, "Problem in pointer list."
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! B: Check if potential partner male
        if (current_agent_ptr%node%gender /= 'M') then
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! C: Check if potential partner old enough
        if (current_agent_ptr%node%age < age_when_vertile_m) then
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! D: Check if potential partner to old
        if (current_agent_ptr%node%age > age_until_vertile_m) then
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! E: Choose Partner
        selected_male => current_agent_ptr%node
        current_agent_ptr => null()
        
        

    
    enddo

    if (.not. associated(selected_male)) then
        ! No mate found in proximity of female
        return
    endif


    found_mates_counter = found_mates_counter + 1    

    ! D: Run probability if mating successfull
    call random_number(r)

    if (r > probability_vertilisation_per_tick ) then
        ! mating not successful
        return
    endif

   ! print*, "5"


    pregnancy_counter = pregnancy_counter + 1
    
    female%is_pregnant = 1
    !print*, "at origin"
    if (.not. associated(selected_male%children)) then

    endif

    female%father_of_unborn_child => selected_male
    pregnancy_counter = pregnancy_counter + 1



end subroutine find_mate


function select_random_agent_in_cell(grid,gx,gy) result(selected)
    implicit none

    type(spatial_grid), pointer, intent(in) :: grid
    integer :: gx, gy

    type(pointer_node), pointer :: current
    type(Node), pointer :: selected

    current => grid%cell(gx,gy)%agents
    selected => null()

    do while (associated(current))

        if (associated(current%node)) then
            selected => current%node
            current => null()
        else
            current => current%next
        endif
    end do

    if (.not. associated(selected)) then
        !print *, "Error: wasnt able to select agent in cell."
    endif

    

  
end function select_random_agent_in_cell


! This function should only be used if you only want to kill one agent in a cell!
subroutine kill_random_agent_in_cell(grid,gx,gy)
    implicit none

    type(spatial_grid), pointer, intent(inout) :: grid
    integer :: gx, gy

    type(Node), pointer :: selected_agent

    if(gx > grid%nx .or. gx < 1) then
        print*, "gx out of bound in kill_random_agent_in_cell()"
        return
    endif

    if(gy > grid%ny .or. gy < 1) then
        print*, "gy out of bound in kill_random_agent_in_cell(), gy: ",gy, " ny: ", grid%ny
        return
    endif

    if (grid%cell(gx,gy)%number_of_agents == 0) then
        print *, "Error: No agents in cell to kill."
        return
    end if

    ! Select a random agent in the cell
    selected_agent => select_random_agent_in_cell(grid,gx,gy)

    if (.not. associated(selected_agent)) then
        !print *, "Error: No agent selected for killing."
        return
    end if

    ! kill the agent
    call selected_agent%agent_die()




end subroutine kill_random_agent_in_cell

subroutine kill_n_agents_in_cell(grid, gx, gy, n)
    implicit none
    type(spatial_grid), pointer, intent(inout) :: grid
    integer, intent(in) :: gx, gy, n

    integer :: counter,m 
    type(pointer_node), pointer :: current
    type(Node), pointer :: temp_agent_ptr


    m = grid%cell(gx,gy)%number_of_agents
    current => grid%cell(gx,gy)%agents

    if (n < 1) then
        return
    endif

    counter = 0

    if(m-n < 0) then
        call kill_all_agents_in_cell(grid,gx,gy)
        return
    endif

    do while (associated(current))



        temp_agent_ptr => current%node
        current => current%next

        if (associated(temp_agent_ptr)) then
            call temp_agent_ptr%agent_die()
            counter = counter + 1
        end if

        if (counter == n) then 
            current => null()
        endif

    enddo


    if (counter < n) then
        print*, "Error: Not enough agents killed in cell ", gx, gy, " only killed ", counter, " out of ", n
    endif 

end subroutine kill_n_agents_in_cell

subroutine kill_all_agents_in_cell(grid,gx,gy)
    implicit none
    type(spatial_grid), pointer, intent(inout) :: grid
    integer, intent(in) :: gx, gy

    type(pointer_node), pointer :: current
    type(Node), pointer :: temp_agent_ptr

    if(gx > grid%nx .or. gx < 1) then
        print*, "gx out of bound in kill_all_agents_in_cell()"
        return
    endif

    if(gy > grid%ny .or. gy < 1) then
        print*, "gy out of bound in kill_all_agents_in_cell(), gy: ",gy, " ny: ", grid%ny
        return
    endif

    if (grid%cell(gx,gy)%number_of_agents == 0) then
        print *, "Error: No agents in cell to kill."
        return
    end if

    current => grid%cell(gx,gy)%agents

    do while (associated(current))
        temp_agent_ptr => current%node
        current => current%next
        if (associated(temp_agent_ptr)) then
            call temp_agent_ptr%agent_die()
        end if
    end do

end subroutine kill_all_agents_in_cell




end module mod_birth_death_example