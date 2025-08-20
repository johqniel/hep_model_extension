module mod_birth_death_example

    use mod_agent_class

    use mod_agent_tracking

    use mod_grid

    use mod_calculations
    ! Uses: 
    !
    !       subroutine calculate_grid_pos


    ! Constants

    integer :: max_agents_per_cell = 25


contains

subroutine death_example(grid)
    implicit none
    type(spatial_grid), pointer, intent(inout) :: grid

    integer :: nx, ny
    integer :: i, j, k

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

            do k = 1, max(agents_exces_count,10)

                ! Randomly kill an agent in the cell
                call kill_random_agent_in_cell(grid, i,j)

            end do

            
        end do
    end do


end subroutine death_example

subroutine birth_example(grid)
    implicit none
    type(spatial_grid), pointer, intent(inout) :: grid
    integer :: nx, ny
    integer :: i, j, k

    type(Node), pointer :: selected_female
    type(Node), pointer :: selected_male

    integer :: agent_born_count 
    integer :: count_f_in_cell
    integer :: count_m_in_cell

    real(8) :: x_new, y_new, ux_new, uy_new

    integer :: counter 

    counter = 0
    nx = grid%nx
    ny = grid%ny

    selected_female => null()
    selected_male => null()

    do i = 1, nx
        do j = 1, ny

            !print*, "in do."

            if (grid%cell(i,j)%number_of_agents < 3) then
                counter = counter + 1
                cycle
            endif

            !print*, "passed first if."

            if (grid%cell(i,j)%number_of_agents > 2) then

                !print*, "passed second if."
                count_f_in_cell = count_females_in_cell(grid, i, j)
                count_m_in_cell = count_males_in_cell(grid,i,j)

                if (count_m_in_cell == 0 .or. count_f_in_cell == 0) then
                    ! No birth possible
                    counter = counter + 1
                    cycle
                end if  

                !print*, "passed third if."

                call select_random_female_in_cell(grid,i,j, selected_female)
                call select_random_male_in_cell(grid,i,j, selected_male)


                !print*, "parents selected."
    
                if (.not. associated(selected_female)) then
                        print*, "selected female is not associated."
                        cycle
                endif

                if (.not. associated(selected_male)) then
                        print*, "selected male is not associated."
                        cycle
                endif              
                
                !print*, "parents associated"


                call agent_born_place_in_grid(selected_female%position_population,grid,selected_female, selected_male)

                !print*, "Agent born in cell (", i, ",", j


                    
            end if

        end do
    end do

    if (counter == nx * ny) then
        print*, "No birth possible in all cells."
    endif

end subroutine birth_example

integer function count_females_in_cell(grid,gx,gy)
    implicit none
    type(spatial_grid), pointer, intent(in) :: grid
    integer, intent(in) :: gx,gy
    integer :: count
    type(pointer_node), pointer :: current

    count = 0

    current => grid%cell(gx,gy)%agents

    do while (associated(current))
        !print *, "in while."

        if (current%node%gender == "F") then
            count = count + 1
        end if
        current => current%next
    end do


    count_females_in_cell = count
    !print*, "Counted: ", count, " many females in cell ",gx,gy
end function count_females_in_cell

integer function count_males_in_cell(grid,gx,gy)
    implicit none
    type(spatial_grid), pointer, intent(in) :: grid
    integer, intent(in) :: gx,gy

    integer :: count
    type(pointer_node), pointer :: current

    count = 0
    current => grid%cell(gx,gy)%agents

    do while (associated(current))
        if (current%node%gender == "M") then
            count = count + 1
        end if
        current => current%next
    end do


    count_males_in_cell = count
end function count_males_in_cell

subroutine select_random_male_in_cell(grid,gx,gy,male) 
        implicit none

    type(spatial_grid), pointer, intent(in) :: grid
    type(Node), pointer, intent(out) :: male
    integer, intent(in) :: gx, gy

    type(pointer_node), pointer :: current

    current => grid%cell(gx,gy)%agents
    male => null()

    do while (associated(current))

        if (current%node%gender == "M") then
            male => current%node
            current => null()
        else

            current => current%next
        endif
    end do

    if (.not. associated(male)) then
        print *, "Error: wasnt able to select male in cell."
    endif

    

  
end subroutine select_random_male_in_cell

subroutine select_random_female_in_cell(grid,gx,gy,female)
    implicit none

    type(spatial_grid), pointer, intent(in) :: grid
    integer, intent(in) :: gx, gy
    type(Node), pointer, intent(out) :: female

    type(pointer_node), pointer :: current


    current => grid%cell(gx,gy)%agents
    female => null()

    do while (associated(current))

        if (current%node%gender == "F") then
            female => current%node
            current => null()
        else

            current => current%next
        endif
    end do

    if (.not. associated(female)) then
        print *, "Error: wasnt able to select male in cell."
    endif

    

  
end subroutine select_random_female_in_cell


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
    call mark_agent_dead_remove_from_grid(selected_agent%position_human, selected_agent%position_population,grid)




end subroutine kill_random_agent_in_cell




end module mod_birth_death_example