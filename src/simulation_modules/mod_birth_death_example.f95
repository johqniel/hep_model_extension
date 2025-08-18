module mod_birth_death_example

    use mod_agent_class

    use mod_agent_tracking

    use mod_grid

    use mod_calculations
    ! Uses: 
    !
    !       subroutine calculate_grid_pos


    ! Constants

    integer :: max_agents_per_cell = 100


contains

subroutine death_example(grid)
    type(spatial_grid), pointer, intent(inout) :: grid

    integer :: nx, ny
    integer :: i, j, k

    integer :: agents_exces_count

    do i = 1, nx
        do j = 1, ny
            if (grid%cell(i,j)%number_of_agents > max_agents_per_cell) then
                agents_exces_count = grid%cell(i,j)%number_of_agents - max_agents_per_cell
                do k = 1, agents_exces_count
                    ! Randomly kill an agent in the cell
                    !print*, "Number of agents in cell: ", grid%cell(i,j)%number_of_agents, " Killing one agent."
                    call kill_random_agent_in_cell(grid, i,j)
                    !!!print*, "Killed an agent in cell (", i, ",", j, ")"
                end do
            end if
        end do
    end do


end subroutine death_example

subroutine birth_example(grid)
    type(spatial_grid), pointer, intent(inout) :: grid
    integer :: nx, ny
    integer :: i, j, k

    type(pointer_node), pointer :: selected_female
    type(Node), pointer :: selected_male

    integer :: agent_born_count 
    integer :: count_f_in_cell
    integer :: count_m_in_cell

    real(8) :: x_new, y_new, ux_new, uy_new

    do i = 1, nx
        do j = 1, ny
            if (grid%cell(i,j)%number_of_agents > 2) then
                count_f_in_cell = count_females_in_cell(grid, i, j)
                count_m_in_cell = count_males_in_cell(grid,i,j)

                if (count_m_in_cell == 0 .or. count_f_in_cell == 0) then
                    ! No birth possible
                    cycle
                end if  

                do while (associated(selected_female)) 
                    if (selected_female%node%gender == "M") then
                        selected_female => selected_female%next
                    else
                        selected_male = select_random_male_in_cell(grid,i,j)

                        new_x = selected_female%node%pos_x
                        new_y = selected_female%node%pos_y
                        ux_new = selected_female%node%ux
                        uy_new = selected_female%node%uy

                        call agent_born_place_in_grid(selected_female%node%position_population,hum_t,grid)

                        !print*, "Agent born in cell (", i, ",", j


                    endif
                    
                enddo



                    
            end if
        end do
    end do

end subroutine birth_example

integer function count_females_in_cell(grid,gx,gy)
    type(spatial_grid), pointer, intent(in) :: grid
    integer, intent(in) :: gx,gy
    integer :: count
    type(pointer_node), pointer :: current

    count = 0

    current => grid%cell(gx,gy)%agents

    do while (associated(current))

        if (current%node%gender == "F") then
            count = count + 1
        end if
        current => current%next
    end do


    count_females_in_cell = count
end function count_females_in_cell

integer function count_males_in_cell(grid,gx,gy)
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

function select_random_male_in_cell(grid,gx,gy) result(selected)
    type(spatial_grid), pointer, intent(in) :: grid
    integer :: gx, gy

    type(pointer_node), pointer :: current
    type(Node), pointer :: selected

    current => grid%cell(gx,gy)%agents
    selected => null()

    do while (associated(current))

        if (current%node%gender == "M") then
            selected => current%node
            current => null()
        else

            current => current%next
        endif
    end do

    if (.not. associated(selected)) then
        print *, "Error: wasnt able to select male in cell."
    endif

    

  
end function select_random_male_in_cell


function select_random_agent_in_cell(grid,gx,gy) result(selected)
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
    type(spatial_grid), pointer, intent(inout) :: grid
    integer :: gx, gy

    type(Node), pointer :: selected_agent



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