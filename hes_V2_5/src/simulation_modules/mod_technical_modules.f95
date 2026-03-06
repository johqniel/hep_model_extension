module mod_technical_modules
    use mod_agent_world
    
    implicit none

contains

    subroutine update_agent_age(agent_ptr)
        type(Agent), pointer, intent(inout) :: agent_ptr
        
        agent_ptr%age_ticks = agent_ptr%age_ticks + 1
        agent_ptr%age_years = int(agent_ptr%age_ticks / 52)

        agent_ptr%ticks_since_last_birth = agent_ptr%ticks_since_last_birth + 1

        if (agent_ptr%is_pregnant > 0 ) then
            agent_ptr%is_pregnant = agent_ptr%is_pregnant + 1
        end if
        
    end subroutine update_agent_age

    ! =========================================================================
    ! SUBROUTINE: update_density_and_hep_grid (GRID-CENTRIC)
    ! =========================================================================
    !
    ! Called ONCE PER TICK on the entire grid automatically (permanent module).
    ! Updates the base density computations across all cells.
    ! Calculates basic agents counts per cell, smoothed density using
    ! smooth_box_filter (watershed_smooth_radius), calculates flow fields,
    ! and sets basic hep_av.
    !
    ! =========================================================================
    subroutine update_density_and_hep_grid(w, t)
        use mod_constants, only: deg_km
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t
        
        integer :: jp
        type(Grid), pointer :: grid
        integer :: i, j, k, id, nx, ny
        real(8) :: flow_x_sum, flow_y_sum
        type(Agent), pointer :: agent_ptr
        real(8), allocatable :: raw_density(:,:), smoothed(:,:)

        grid => w%grid
        nx = grid%nx
        ny = grid%ny

        ! We process each population
        do jp = 1, w%config%npops
        
            ! 1. update the Pure Density
            call grid%update_density_pure()
            
        ! 2. Calculate agent flows
        do i = 1, nx
            do j = 1, ny
                flow_x_sum = 0.0d0
                flow_y_sum = 0.0d0
                    
                if (grid%cell(i,j)%number_of_agents > 0) then
                    do k = 1, grid%cell(i,j)%number_of_agents
                        id = grid%cell(i,j)%agents_ids(k)
                        if (id > 0) then
                            agent_ptr => get_agent(id, w)
                            if (associated(agent_ptr)) then
                                if (agent_ptr%population == jp) then
                                    flow_x_sum = flow_x_sum + agent_ptr%ux
                                    flow_y_sum = flow_y_sum + agent_ptr%uy
                                end if
                            end if
                        end if
                    end do
                end if

                ! Normalize flow by area
                if (grid%cell(i,j)%area > 0.0d0) then
                        grid%cell(i,j)%flow_x = flow_x_sum * 100.0d0 / grid%cell(i,j)%area
                        grid%cell(i,j)%flow_y = flow_y_sum * 100.0d0 / grid%cell(i,j)%area
                else
                        grid%cell(i,j)%flow_x = 0.0d0
                        grid%cell(i,j)%flow_y = 0.0d0
                end if
            end do
        end do
            
        ! 3. update smoothed density (human_density_smoothed)
        !    Uses smooth_box_filter from mod_watershed with watershed_smooth_radius.
        allocate(raw_density(nx, ny), smoothed(nx, ny))
        do j = 1, ny
            do i = 1, nx
                raw_density(i, j) = grid%cell(i, j)%human_density
            end do
        end do

        if (w%config%watershed_smooth_radius > 0) then
            call smooth_box_filter(raw_density, nx, ny, &
                w%config%watershed_smooth_radius, smoothed)
        else
            smoothed = raw_density
        end if


        ! update smoothed density in cells 
        do j = 1, ny
            do i = 1, nx
                grid%cell(i, j)%human_density_smoothed = smoothed(i, j)
            end do
        end do

        deallocate(raw_density, smoothed)
            
        ! 4. Base HEP transfer (hep_av = hep)
        grid%hep_av(:,:,jp) = grid%hep(:,:,jp, grid%t_hep)
            
        end do
            
    end subroutine update_density_and_hep_grid

    ! =========================================================================
    ! SUBROUTINE: smooth_box_filter
    ! =========================================================================
    !
    ! Simple box (mean) filter with half-width `radius`.
    ! Each cell becomes the average of the (2*radius+1)^2 neighbourhood.
    ! Handles boundaries by clamping indices to the valid domain.
    !
    ! =========================================================================
    subroutine smooth_box_filter(input, nx, ny, radius, output)
        implicit none
        integer, intent(in)  :: nx, ny, radius
        real(8), intent(in)  :: input(nx, ny)
        real(8), intent(out) :: output(nx, ny)

        integer :: i, j, di, dj, ni, nj, count
        real(8) :: total

        do j = 1, ny
            do i = 1, nx
                total = 0.0d0
                count = 0

                do dj = -radius, radius
                    nj = j + dj
                    if (nj < 1 .or. nj > ny) cycle

                    do di = -radius, radius
                        ni = i + di
                        if (ni < 1 .or. ni > nx) cycle

                        total = total + input(ni, nj)
                        count = count + 1
                    end do
                end do

                if (count > 0) then
                    output(i, j) = total / dble(count)
                else
                    output(i, j) = input(i, j)
                end if
            end do
        end do

    end subroutine smooth_box_filter

end module mod_technical_modules
