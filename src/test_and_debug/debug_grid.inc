


! #####################################################################################################
! Functions that print information about the grid
! #####################################################################################################

        subroutine compare_num_of_agents_in_grid(grid)
            type(spatial_grid), pointer, intent(in) :: grid

            integer :: num_of_agents

            num_of_agents = count_agents_in_grid(grid)

            call grid%dead_agents_in_grid()

            print*, "Agents in grid: ", num_of_agents

        end subroutine compare_num_of_agents_in_grid

! #####################################################################################################
! Functions that check the grid for consitency
! #####################################################################################################

subroutine check_grid_data(grid)
    type(spatial_grid), intent(in) :: grid

    integer :: nx, ny, i, j, counter_less, counter_more
    integer :: n_count, n_safe

    nx = grid%nx
    ny = grid%ny
    counter_less = 0
    counter_more = 0

    do i = 1, nx
        do j = 1, ny
            n_count = grid%count_agents_in_cell(i,j)
            n_safe = grid%cell(i,j)%number_of_agents
            if (n_count < n_safe ) then
                counter_less = counter_less + 1
                
            end if
            if (n_count > n_safe ) then
                counter_more = counter_more + 1
                
            end if
        end do
    end do

    if (counter_less > 0 ) then
        print*, "Error: There are ", counter_less, " cells that have less agents then they should."
    endif
    if (counter_more > 0) then
        print*, "Error: There are ", counter_more, " cells that have more agents then they should."
    endif
end subroutine check_grid_data

subroutine check_area_of_grid(grid, area_matrix)
    implicit none
    type(spatial_grid), intent(in) :: grid
    real(8), allocatable, dimension(:,:), intent(in) :: area_matrix

    integer :: nx, ny, i, j, mismatch_counter

    nx = grid%nx
    ny = grid%ny
    mismatch_counter = 0

    ! First check: dimension match
    if (.not. allocated(grid%cell)) then
        print *, "Grid cell array not allocated."
        return
    end if

    if (size(area_matrix, 1) /= nx .or. size(area_matrix, 2) /= ny) then
        print* , "Dimensions of area_matrix dont match dimensions of grid"
        return
    end if

    ! Second check: element-wise comparison
    do i = 1, nx
        do j = 1, ny
            if (reals_equal(grid%cell(i,j)%area,area_matrix(i,j))) then
                mismatch_counter = mismatch_counter + 1
                return
            end if
        end do
    end do

    if (mismatch_counter > 0) then
        print* , "Found: ", mismatch_counter, " many mismatches between area matrix and area of grid."
    endif

end subroutine check_area_of_grid

subroutine check_density_of_grid(grid,density_matrix,x_mat,y_mat)
    implicit none
    type(spatial_grid), intent(in) :: grid
    real(8), dimension(:,:), intent(in) :: density_matrix
    real(8) , dimension(:,:), intent(in) :: x_mat, y_mat

    integer :: counter_empty, counter_filled, counter, nx, ny, i, j
    integer, allocatable :: agent_counter_matrix(:,:)

    integer :: gx,gy

    allocate(agent_counter_matrix(grid%nx, grid%ny)) 

    agent_counter_matrix = 0

    
    do i = 1, size(x,1)
        do j = 1, size(x,2)
            call calculate_grid_pos(x_mat(i,j), y_mat(i,j), gx, gy)
            if (gx > 0 .and. gy > 0 .and. gx <= grid%nx .and. gy <= grid%ny) then
                agent_counter_matrix(gx,gy) = agent_counter_matrix(gx,gy) + 1
            end if
        end do
    end do

    nx = grid%nx
    ny = grid%ny

    counter_empty = 0
    counter_filled = 0

    do i = 1 , nx
        do j = 1, ny
            if (density_matrix(i,j) == 0) then 
                if (abs(grid%cell(i,j)%human_density) > 0.01) then 
                    if (grid%cell(i,j)%number_of_agents > 0) then
                        counter_empty = counter_empty + 1
                    endif

                endif
            else 
                if (abs( (grid%cell(i,j)%human_density-density_matrix(i,j)) / density_matrix(i,j) ) > 0.01) then
                    counter_filled = counter_filled + 1
                endif
            endif

            if (grid%cell(i,j)%number_of_agents /= agent_counter_matrix(i,j)) then
                counter = counter + 1
            endif

        enddo
    enddo

    if (counter > 0) then
        print*, "There are ", counter, " many cells that have a different number of agents than the agent counter matrix."
    else
        return
    endif

    if (counter_filled > 0 ) then 
        print*, "There are ", counter_filled , " many supiscious cells regarding the density of humans."
        print*, " (more/ less  humans than supposed to) -> check grid data type."
    
    endif

    if (counter_empty > 0 ) then 
        print*, "There are ", counter_empty , " many supiscious cells regarding the density of humans. "
        print*, "(humans in empty cells ) -> check grid data type."
    
    ENDIF


end subroutine check_density_of_grid


subroutine check_grid_for_dead_agents(grid)
    type(spatial_grid), intent(in) :: grid

    call grid%dead_agents_in_grid()

    
end subroutine check_grid_for_dead_agents

subroutine check_if_all_alive_agents_in_correct_cell(grid)
    class(spatiaL_grid), intent(in) :: grid

    type(Node), pointer :: current_agent
    integer :: gx, gy, counter

    current_agent => head_agents
    counter = 0

    do while ( associated(current_agent)) 
        call compute_position_in_grid(current_agent,gx,gy)

        if ( .not. grid%is_agent_in_grid(current_agent)) then
            counter = counter + 1

        endif

        current_agent => current_agent%next

    enddo

    if (counter > 0 ) then
        print*, "Error: There are: ", counter, " many agents that are not in the grid cell where they should be."
    endif
end subroutine check_if_all_alive_agents_in_correct_cell


subroutine check_consistency_grid_agents(grid)
    implicit none 
    type(spatial_grid), intent(in) :: grid

    integer :: nx,ny,i,j,mismatch_counter
    type(pointer_node), pointer :: current_agent_ptr
    type(Node), pointer :: current_agent

    integer :: gx, gy

    nx = grid%nx
    ny = grid%ny

    mismatch_counter = 0

    ! ###########################################################################################################
    ! ############ Go through grid and check if all agents in gridcell have correct position ####################
    ! ###########################################################################################################



    do i = 1, nx 

        do j = 1, ny
            current_agent_ptr => grid%cell(i,j)%agents


            do while(associated(current_agent_ptr))

                if (.not. associated(current_agent_ptr%node)) then
                    print*, "Error: ptr node associated but %node not associated. in check_consistency_grid_agents."
                    
                endif


                current_agent => current_agent_ptr%node

                
                if (.not. associated(current_agent)) then
                    print*, "Error: Agent that is in cell is not associated."
                endif


                call calculate_grid_pos(current_agent%pos_x, current_agent%pos_y, gx ,gy)

                if ((gx /= i) .or. (gy /= j)) then
                    mismatch_counter = mismatch_counter + 1
                endif

                current_agent_ptr => current_agent_ptr%next
            enddo 

        enddo

    enddo


    if (mismatch_counter > 0) then
        print*, " Error: There are:", mismatch_counter, " many agents that are placed in the wrong grid cell."
    endif


    ! #############################################################################
    ! ############ Go through agents check if agent is in grid cell ###############
    ! #############################################################################


    mismatch_counter = 0

    current_agent => head_agents

    do while (associated(current_agent))

        call calculate_grid_pos(current_agent%pos_x, current_agent%pos_y,gx,gy)

        if (gx < 1 .or. gx > nx .or. gy < 1 .or. gy > ny) then 
            current_agent => current_agent%next
            cycle
            ! We have to check here if the agent is in the grid. if not the 
            !          grid%cell(gx,gy)%agents 
            ! in the following lines will cause a seg fault that was hard to find RIP
        endif 


        if (.not. is_agent_in_ptr_list(current_agent, grid%cell(gx,gy)%agents)) then
            mismatch_counter = mismatch_counter + 1
        endif

        current_agent => current_agent%next

    end do

    if (mismatch_counter > 0) then
        print*, "Error: There are: ", mismatch_counter, " many agents that are not in the grid cell where they should be"
    endif

end subroutine check_consistency_grid_agents

subroutine check_number_of_agents_in_grid_using_matrix(grid,x_mat,y_mat,hum_t)
    implicit none
    type(spatial_grid), pointer, intent(in) :: grid
    real(8), dimension(:,:), intent(in) :: x_mat, y_mat
    integer, dimension(:), intent(in) :: hum_t

    integer, allocatable :: counter_matrix(:,:)
    integer :: i, j, gx, gy, counter_less, counter_more

    allocate(counter_matrix(grid%nx, grid%ny))
    counter_matrix = 0
    counter_less = 0
    counter_more = 0

    do j = 1, size(x_mat,2)
        do i = 1, hum_t(j)
            call calculate_grid_pos(x_mat(i,j), y_mat(i,j), gx, gy)

            if (gx > 0 .and. gy > 0 .and. gx <= grid%nx .and. gy <= grid%ny) then
                counter_matrix(gx,gy) = counter_matrix(gx,gy) + 1
            endif
        end do
    end do


    do i = 1, grid%nx
        do j = 1, grid%ny
            if (grid%cell(i,j)%number_of_agents < counter_matrix(i,j)) then
                counter_less = counter_less + 1
            endif
            if (grid%cell(i,j)%number_of_agents > counter_matrix(i,j)) then
                counter_more = counter_more + 1
            endif
        end do
    end do

    if (counter_less > 0) then 
        print*, "There are: ", counter_less, " many cells in which are less agents than calculated by x,y matrixes."
    endif

    if (counter_more > 0) then 
        print*, "There are: ", counter_more, " many cells in which are more agents than calculated by x,y matrixes."
    endif
end subroutine check_number_of_agents_in_grid_using_matrix

subroutine compare_number_of_agents_in_grid_matrix_agent_class(grid,x_mat,y_mat,agents_head)
    implicit none
    type(spatial_grid), pointer, intent(in) :: grid
    real(8), dimension(:,:), intent(in) :: x_mat, y_mat
    type(Node), pointer, intent(in) :: agents_head

    type(Node), pointer :: current_agent

    integer, allocatable :: counter_matrix_A(:,:)
    integer, allocatable :: counter_matrix_B(:,:)
    integer :: i, j, gx, gy, counter_less, counter_more

    allocate(counter_matrix_A(grid%nx, grid%ny))
    counter_matrix_A = 0
    allocate(counter_matrix_B(grid%nx, grid%ny))
    counter_matrix_B = 0
    counter_less = 0
    counter_more = 0

    current_agent => agents_head

    do i = 1, size(x_mat,1)
        do j = 1, size(x_mat,2)
            call calculate_grid_pos(x_mat(i,j), y_mat(i,j), gx, gy)

            if (gx > 0 .and. gy > 0 .and. gx <= grid%nx .and. gy <= grid%ny) then
                counter_matrix_A(gx,gy) = counter_matrix_A(gx,gy) + 1
            endif
        end do
    end do

    do while (associated(current_agent))
        call calculate_grid_pos(current_agent%pos_x, current_agent%pos_y, gx, gy)
        if (gx > 0 .and. gy > 0 .and. gx <= grid%nx .and. gy <= grid%ny) then
                counter_matrix_B(gx,gy) = counter_matrix_B(gx,gy) + 1
            endif
        current_agent => current_agent%next
    enddo

    do i = 1, grid%nx
        do j = 1, grid%ny
            if (counter_matrix_A(i,j) < counter_matrix_B(i,j)) then
                counter_more = counter_more + 1
            endif
            if (counter_matrix_A(i,j) > counter_matrix_B(i,j)) then
                counter_less = counter_less + 1
            endif
        end do
    end do
    

    if (counter_less > 0) then 
        print*, "There are: ", counter_less, " many cells in which" 
        print*, "there are less agents py their position than calculated by x,y matrixes."
    endif

    if (counter_more > 0) then 
        print*, "There are: ", counter_more, " many cells in which"
        print*, "there are more agents by their position than calculated by x,y matrixes."
    endif
end subroutine compare_number_of_agents_in_grid_matrix_agent_class

subroutine check_number_of_agents_in_grid(grid)
    type(spatial_grid), pointer, intent(in) :: grid
    
    integer :: a,b

    a = grid%agents_in_grid()

    b = count_agents()

    if (a /= b) then 
        print*, "There are: ", b, " many alive agents but there are only: ", a, " many agents in grid."
    endif
    
end subroutine check_number_of_agents_in_grid

subroutine check_duplicate_agents_in_cells(grid)
    type(spatial_grid), pointer, intent(in) :: grid

    integer :: i, j, nx, ny
    integer :: counter_duplicate
    type(pointer_node), pointer :: current_agent_ptr
    type(pointer_node), pointer :: compare_agent_ptr

    nx = grid%nx
    ny = grid%ny
    counter_duplicate = 0

    do i = 1, nx
        do j = 1, ny
            current_agent_ptr => grid%cell(i,j)%agents

            do while (associated(current_agent_ptr))
                compare_agent_ptr => current_agent_ptr%next
                do while (associated(compare_agent_ptr))
                    if (current_agent_ptr%node%id == compare_agent_ptr%node%id) then
                        counter_duplicate = counter_duplicate + 1
                        print*, "Duplicate agent found in cell (", i, ",", j, ") with id: ", current_agent_ptr%node%id
                    endif
                    compare_agent_ptr => compare_agent_ptr%next
                end do
                current_agent_ptr => current_agent_ptr%next
            end do
        end do
    end do

    if (counter_duplicate > 0) then 
        print*, "There are: ", counter_duplicate, " many duplicate agents in the grid. (Checked cell wise)"
    else 
        !print*, "No duplicate agents found in the grid."
    endif

end subroutine check_duplicate_agents_in_cells

subroutine check_if_agents_twice_in_grid(grid,agents_head)
    implicit none
    type(spatial_grid), pointer, intent(in) :: grid
    type(Node), pointer :: agents_head

    integer :: i, j, nx, ny
    integer :: counter
    type(Node), pointer :: current_agent
    type(pointer_node), pointer :: compare_agent_ptr

    nx = grid%nx
    ny = grid%ny
    counter = 0
    current_agent => agents_head

    !counter_b

    do while (associated(current_agent))

        do i = 1, nx
            do j = 1, ny
                compare_agent_ptr => grid%cell(i,j)%agents

                do while (associated(compare_agent_ptr))
                    if (current_agent%id == compare_agent_ptr%node%id) then
                        counter = counter + 1
                    endif
                    compare_agent_ptr => compare_agent_ptr%next
                end do

            enddo
        enddo

        if (counter > 1) then
            print*, "Agent with id: ", current_agent%id, " is ", counter, "many times in the grid."
        endif
        if (counter == 0) then
            print*, "Agent with id: ", current_agent%id, " is not in the grid."
        endif

        current_agent => current_agent%next
        counter = 0

    enddo



end subroutine check_if_agents_twice_in_grid
