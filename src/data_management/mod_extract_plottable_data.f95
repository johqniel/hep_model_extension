module mod_extract_plottable_data

    use mod_agent_world
    use mod_grid_id
    use mod_config
    
    implicit none

    contains

    ! =================================================================================
    ! Get HEP data for a specific time step
    ! Returns: hep_array(dlon, dlat, npops)
    ! =================================================================================
    subroutine get_hep_at_time(world, t_hep, hep_array)
        class(world_container), intent(in) :: world
        integer, intent(in) :: t_hep
        real, allocatable, intent(out) :: hep_array(:,:,:)
        
        integer :: dlon, dlat, npops
        
        if (.not. allocated(world%grid%hep)) then
            print *, "Error: HEP array not allocated in grid."
            return
        endif

        dlon = size(world%grid%hep, 1)
        dlat = size(world%grid%hep, 2)
        npops = size(world%grid%hep, 3)
        
        ! Check bounds
        if (t_hep < lbound(world%grid%hep, 4) .or. t_hep > ubound(world%grid%hep, 4)) then
            print *, "Error: t_hep out of bounds: ", t_hep
            return
        endif

        allocate(hep_array(dlon, dlat, npops))
        
        hep_array = world%grid%hep(:, :, :, t_hep)

    end subroutine get_hep_at_time

    ! =================================================================================
    ! Get data for all alive agents
    ! Returns: count, x, y, pop
    ! =================================================================================
    subroutine get_alive_agents_data(world, count, x, y, pop)
        class(world_container), intent(in) :: world
        integer, intent(out) :: count
        real(8), allocatable, intent(out) :: x(:)
        real(8), allocatable, intent(out) :: y(:)
        integer, allocatable, intent(out) :: pop(:)
        
        integer :: jp, k, idx

        ! 1. Count alive agents
        count = 0
        do jp = 1, world%config%npops
            do k = 1, world%num_humans(jp)
                if (.not. world%agents(k, jp)%is_dead) then
                    count = count + 1
                endif
            end do
        end do

        ! 2. Allocate arrays
        allocate(x(count))
        allocate(y(count))
        allocate(pop(count))

        ! 3. Fill arrays
        idx = 1
        do jp = 1, world%config%npops
            do k = 1, world%num_humans(jp)
                if (.not. world%agents(k, jp)%is_dead) then
                    x(idx) = world%agents(k, jp)%pos_x
                    y(idx) = world%agents(k, jp)%pos_y
                    pop(idx) = world%agents(k, jp)%population
                    idx = idx + 1
                endif
            end do
        end do

    end subroutine get_alive_agents_data

end module mod_extract_plottable_data
