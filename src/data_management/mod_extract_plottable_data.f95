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
    subroutine get_alive_agents_data(world, count, x, y, pop, age, gender, resources, children, is_pregnant, avg_resources, &
                                     ux, uy, is_dead_out)
        class(world_container), intent(in) :: world
        integer, intent(out) :: count
        real(8), allocatable, intent(out) :: x(:)
        real(8), allocatable, intent(out) :: y(:)
        integer, allocatable, intent(out) :: pop(:)
        ! New attributes
        integer, allocatable, intent(out) :: age(:)
        character(len=1), allocatable, intent(out) :: gender(:)
        integer, allocatable, intent(out) :: resources(:)
        integer, allocatable, intent(out) :: children(:)
        integer, allocatable, intent(out) :: is_pregnant(:)
        real(8), allocatable, intent(out) :: avg_resources(:)
        
        ! Even newer attributes
        real(8), allocatable, intent(out) :: ux(:)
        real(8), allocatable, intent(out) :: uy(:)
        integer, allocatable, intent(out) :: is_dead_out(:)
        
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
        
        allocate(age(count))
        allocate(gender(count))
        allocate(resources(count))
        allocate(children(count))
        allocate(is_pregnant(count))
        allocate(avg_resources(count))
        
        allocate(ux(count))
        allocate(uy(count))
        allocate(is_dead_out(count))

        ! 3. Fill arrays
        idx = 1
        do jp = 1, world%config%npops
            do k = 1, world%num_humans(jp)
                if (.not. world%agents(k, jp)%is_dead) then
                    x(idx) = world%agents(k, jp)%pos_x
                    y(idx) = world%agents(k, jp)%pos_y
                    pop(idx) = world%agents(k, jp)%population
                    
                    age(idx) = world%agents(k, jp)%age
                    gender(idx) = world%agents(k, jp)%gender
                    resources(idx) = world%agents(k, jp)%resources
                    children(idx) = world%agents(k, jp)%number_of_children
                    is_pregnant(idx) = world%agents(k, jp)%is_pregnant
                    avg_resources(idx) = world%agents(k, jp)%avg_resources
                    
                    ux(idx) = world%agents(k, jp)%ux
                    uy(idx) = world%agents(k, jp)%uy
                    ! Since we filter for alive agents, this will always be 0 (false)
                    ! But we expose it for completeness or if logic changes
                    if (world%agents(k, jp)%is_dead) then
                        is_dead_out(idx) = 1
                    else
                        is_dead_out(idx) = 0
                    end if
                    
                    idx = idx + 1
                endif
            end do
        end do

    end subroutine get_alive_agents_data

end module mod_extract_plottable_data
