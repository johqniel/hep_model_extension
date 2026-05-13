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
                                     ux, uy, is_dead_out, cluster_out, creativity_out)
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
        integer, allocatable, intent(out) :: cluster_out(:)
        real(8), allocatable, intent(out) :: creativity_out(:)
        
        integer :: jp, k, idx, gx, gy, c_idx, c_id, rank
        integer :: n_clusters, i, j, temp
        integer, allocatable :: active_ids(:)

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
        allocate(cluster_out(count))
        allocate(creativity_out(count))

        ! Pre-compute active cluster IDs and sort them to establish rank
        if (allocated(world%cluster_store%clusters)) then
            n_clusters = world%cluster_store%n_clusters
        else
            n_clusters = 0
        end if
        
        if (n_clusters > 0) then
            allocate(active_ids(n_clusters))
            do i = 1, n_clusters
                active_ids(i) = world%cluster_store%clusters(i)%id
            end do
            ! Bubble sort
            do i = 1, n_clusters-1
                do j = 1, n_clusters-i
                    if (active_ids(j) > active_ids(j+1)) then
                        temp = active_ids(j)
                        active_ids(j) = active_ids(j+1)
                        active_ids(j+1) = temp
                    end if
                end do
            end do
        end if

        ! 3. Fill arrays
        idx = 1
        do jp = 1, world%config%npops
            do k = 1, world%num_humans(jp)
                if (.not. world%agents(k, jp)%is_dead) then
                    x(idx) = world%agents(k, jp)%pos_x
                    y(idx) = world%agents(k, jp)%pos_y
                    pop(idx) = world%agents(k, jp)%population
                    
                    age(idx) = world%agents(k, jp)%age_ticks
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
                    
                    ! Cluster Rank
                    cluster_out(idx) = 0
                    if (n_clusters > 0) then
                        gx = world%agents(k, jp)%gx
                        gy = world%agents(k, jp)%gy
                        c_idx = world%cluster_store%cell_cluster_idx(gx, gy)
                        if (c_idx > 0) then
                            c_id = world%cluster_store%clusters(c_idx)%id
                            rank = 0
                            do i = 1, n_clusters
                                if (active_ids(i) == c_id) then
                                    rank = i
                                    exit
                                end if
                            end do
                            cluster_out(idx) = rank
                        end if
                    end if

                    ! Creativity
                    creativity_out(idx) = world%agents(k, jp)%creativity
                    
                    idx = idx + 1
                endif
            end do
        end do

    end subroutine get_alive_agents_data

    ! =================================================================================
    ! Get data for all dead agents currently parked in the export array.
    ! Called by Python to extract dead agent data before clearing the buffer.
    ! Returns: count, id, x, y, pop, age, gender, resources, children
    ! =================================================================================
    subroutine get_dead_agents_data(world, count, id_out, x, y, pop, age, gender, resources, children, death_tick)
        class(world_container), intent(in) :: world
        integer, intent(out) :: count
        integer, allocatable, intent(out) :: id_out(:)
        real(8), allocatable, intent(out) :: x(:)
        real(8), allocatable, intent(out) :: y(:)
        integer, allocatable, intent(out) :: pop(:)
        integer, allocatable, intent(out) :: age(:)
        character(len=1), allocatable, intent(out) :: gender(:)
        integer, allocatable, intent(out) :: resources(:)
        integer, allocatable, intent(out) :: children(:)
        integer, allocatable, intent(out) :: death_tick(:)

        integer :: k

        count = world%num_dead_agents_export

        if (count == 0) return

        allocate(id_out(count))
        allocate(x(count))
        allocate(y(count))
        allocate(pop(count))
        allocate(age(count))
        allocate(gender(count))
        allocate(resources(count))
        allocate(children(count))
        allocate(death_tick(count))

        do k = 1, count
            id_out(k) = world%dead_agents_export(k)%id
            x(k) = world%dead_agents_export(k)%pos_x
            y(k) = world%dead_agents_export(k)%pos_y
            pop(k) = world%dead_agents_export(k)%population
            age(k) = world%dead_agents_export(k)%age_ticks
            gender(k) = world%dead_agents_export(k)%gender
            resources(k) = world%dead_agents_export(k)%resources
            children(k) = world%dead_agents_export(k)%number_of_children
            death_tick(k) = world%dead_agents_export(k)%death_tick
        end do

    end subroutine get_dead_agents_data

end module mod_extract_plottable_data
