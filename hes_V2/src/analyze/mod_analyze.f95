module mod_analyze
    use mod_agent_world
    implicit none

contains

    function count_agents_in_array(world) result(count)
        class(world_container), intent(in) :: world
        integer :: jp, i
        integer :: count(world%config%npops)
        
        count = 0
        do jp = 1, world%config%npops
            do i = 1, world%num_humans(jp)
                if (.not. world%agents(i,jp)%is_dead) then
                    count(jp) = count(jp) + 1
                end if
            end do
        end do
        
    end function count_agents_in_array

    integer function count_agents_in_grid(world)
        class(world_container), intent(in) :: world
        
        count_agents_in_grid = world%grid%agents_in_grid()
        
    end function count_agents_in_grid

end module mod_analyze
