subroutine check_matrix_compact(agents_matrix,num_hums_alive)
    implicit none

    type(Node), allocatable, intent(in) :: agents_matrix(:,:)
    integer, allocatable, intent(in) :: num_hums_alive(:)

    integer :: i,j, counter_1, counter_2

    counter_1 = 0
    counter_2 = 0

    do j = 1, size(agents_matrix,2)
        do i = 1, size(agents_matrix,1)

            if ((i-1)<num_hums_alive(j)) then
                if (agents_matrix(i,j)%is_dead) then
                    counter_1 = counter_1 + 1
                endif
            endif

            if (i > num_hums_alive(j)) then
                if (.not. agents_matrix(i,j)%is_dead) then
                    counter_2 = counter_2 + 1
                endif
            endif



        enddo

    enddo

    if (counter_1 > 0) then
        print*, "Error: There are ", counter_1, " many agents in matrix that are dead that should NOT be dead."
    endif

    if (counter_2 > 0) then
        print*, "Error: There are ", counter_2, " many agents in matrix that are NOT dead that should be dead."
    endif

end subroutine check_matrix_compact

subroutine check_number_of_agents_in_grid_new(grid,agents_matrix)
    implicit none
    type(spatial_grid), pointer, intent(in) :: grid
    type(Node), allocatable, target, intent(in) :: agents_matrix(:,:)
 
    integer, allocatable :: counter_matrix(:,:)
    integer :: i, j, gx, gy, counter_less, counter_more
    type(Node), pointer :: agent

    allocate(counter_matrix(grid%nx, grid%ny))
    counter_matrix = 0
    counter_less = 0
    counter_more = 0

    do j = 1, size(agents_matrix,2)
        do i = 1, size(agents_matrix,1)


            agent => agents_matrix(i,j)

            call calculate_grid_pos(agent%pos_x,agent%pos_y, gx, gy)

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
        print*, "There are: ", counter_less, " many cells in which are less agents than calculated by agents matrix."
    endif

    if (counter_more > 0) then 
        print*, "There are: ", counter_more, " many cells in which are more agents than calculated by agents matrix."
    endif
end subroutine check_number_of_agents_in_grid_new
