module mod_setup

    use mod_agent_world
    use mod_functions

    implicit none
    
    contains

    subroutine generate_initial_agents_old(world)
        class(world_container), intent(inout) :: world
        
        integer :: jp, n, nh, j
        real(8) :: xic, yic, sip, siu
        real(8), allocatable :: wkx(:), wky(:), wku(:), wkv(:)
        type(Agent) :: new_agent
        type(Agent), pointer :: child_ptr

        ! for debug only: 
        integer :: gx, gy
        
        do jp = 1, world%config%npops
            do n = 1, world%config%ns
                nh = world%config%hum_0(n, jp)
                
                if (nh > 0) then
                    xic = world%config%x_ini_c(n, jp)
                    yic = world%config%y_ini_c(n, jp)
                    sip = world%config%ini_spread(n, jp)
                    siu = world%config%sigma_u(jp)
                    
                    allocate(wkx(nh), wky(nh), wku(nh), wkv(nh))
                    
                    call strt_distr_gaussian(nh, xic, yic, sip, wkx, wky, siu, wku, wkv)
                    
                    do j = 1, nh
                        ! Initialize new agent
                        new_agent = world%spawn_agent_hash(jp)
                        
                        ! Now overwrite pos/vel with Gaussian values
                        new_agent%pos_x = wkx(j)
                        new_agent%pos_y = wky(j)
                        new_agent%ux = wku(j)
                        new_agent%uy = wkv(j)
                        

                        call calculate_grid_pos(new_agent%pos_x, new_agent%pos_y, gx, gy,world%config)
                        !print*, "pos_x: ", new_agent%pos_x, "pos_y: ", new_agent%pos_y
                        !print*, "gx: ", gx, "gy: ", gy
                        ! Now add to world
                        call add_agent_to_array_hash(world,new_agent,jp,child_ptr)
                                                     
                    end do
                    
                    deallocate(wkx, wky, wku, wkv)
                end if
            end do
        end do
        
    end subroutine generate_initial_agents_old

end module mod_setup