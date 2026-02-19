module mod_initial_agents

    use mod_agent_world
    use mod_functions

    implicit none
    
    contains

    subroutine generate_initial_agents(world)
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
        
    end subroutine generate_initial_agents

    ! =================================================================
    ! SUBROUTINE: apply_age_distribution
    ! 
    ! Assigns ages to all currently active agents based on the 
    ! configured age distribution.
    ! =================================================================
    subroutine apply_age_distribution(world)
        class(world_container), target, intent(inout) :: world
        
        integer :: jp, k, age, i
        real(8) :: r, cum_prob
        type(Agent), pointer :: current_agent

        if (.not. world%config%age_distribution_set) then
            print *, "Warning: Age distribution not set. Skipping."
            return
        endif

        if (.not. allocated(world%config%age_distribution)) then
             print *, "Error: Age distribution set but array not allocated."
             return
        endif

        print *, "Applying age distribution to agents..."
        
        do jp = 1, world%config%npops
            do k = 1, world%num_humans(jp)
                current_agent => world%agents(k, jp)
                
                if (current_agent%is_dead) cycle

                ! Sample Age
                call random_number(r)
                
                ! Inverse Transform Sampling from PMF
                cum_prob = 0.0d0
                age = 0
                
                do i = 1, size(world%config%age_distribution)
                    cum_prob = cum_prob + world%config%age_distribution(i)
                    if (r <= cum_prob) then
                        age = i - 1 ! 0-indexed age for 1st bin
                        exit
                    endif
                end do
                
                ! If r > 1.0 (shouldn't happen if normalized) or end of array,
                ! assign max age
                if (age == 0 .and. cum_prob < r) then
                    age = size(world%config%age_distribution) - 1
                endif

                current_agent%age = age
            end do
        end do
        
        print *, "Age distribution applied."

    end subroutine apply_age_distribution

end module mod_initial_agents