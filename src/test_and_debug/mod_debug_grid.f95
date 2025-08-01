module mod_debug_grid

use mod_grid
use mod_agent_class
use mod_calculations
    ! Uses: 
    !   - calculate_grid_pos


contains

subroutine check_grid_data(grid)
    type(spatial_grid), intent(in) :: grid

    integer :: nx, ny, i, j, mismatch_counter

    nx = grid%nx
    ny = grid%ny
    mismatch_counter = 0

    do i = 1, nx
        do j = 1, ny
            if (grid%cell(i,j)%number_of_agents /= grid%count_agents_in_cell(i,j)) then
                mismatch_counter = mismatch_counter + 1
                
            end if
        end do
    end do

    if (mismatch_counter > 0) then
        print*, "Error: There are ", mismatch_counter, " many cells that have more/less agents then they should."
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
            if (grid%cell(i,j)%area /= area_matrix(i,j)) then
                mismatch_counter = mismatch_counter + 1
                return
            end if
        end do
    end do

    if (mismatch_counter > 0) then
        print* , "Found: ", mismatch_counter, " many mismatches between area matrix and area of grid."
    endif

end subroutine check_area_of_grid

subroutine check_grid_for_dead_agents(grid)
    implicit none 
    type(spatial_grid), intent(in) :: grid

    integer :: nx,ny,i,j,dead_agent_counter, unassociated_agent_counter


    type(pointer_node), pointer :: current_agent

    dead_agent_counter = 0
    unassociated_agent_counter = 0

    do i = 1, nx 

        do j = 1, ny

            current_agent => grid%cell(i,j)%agents

            do while(associated(current_agent))
                if (.not. associated(current_agent%node)) then
                    print*, "Error: Agent in grid not associated!!!"
                    unassociated_agent_counter = unassociated_agent_counter + 1
                else 
                    if (current_agent%node%is_dead) then
                        dead_agent_counter = dead_agent_counter + 1
                    endif
                endif

                current_agent => current_agent%next
                
            enddo 

        enddo

    enddo
    if(dead_agent_counter > 0) then
                print*, "Error: There are: ", dead_agent_counter, " many dead agents in the grid"
    endif
    if(unassociated_agent_counter > 0) then
                print*, "Error: There are: ", unassociated_agent_counter, " many unassociated agents in the grid"
    endif
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

subroutine check_number_of_agents_in_grid(grid)
    type(spatial_grid), pointer, intent(in) :: grid
    
    integer :: a,b

    a = grid%agents_in_grid()

    b = count_agents()

    if (a /= b) then 
        print*, "There are: ", b, " many alive agents but there are only: ", a, " many agents in grid."
    endif
    
end subroutine check_number_of_agents_in_grid

end module mod_debug_grid