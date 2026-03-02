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
    ! Calculates basic agents counts per cell, smoothed density depending
    ! on sigma_u, calculates flow fields, and sets basic hep_av.
    !
    ! =========================================================================
    subroutine update_density_and_hep_grid(w, t)
        use mod_constants, only: deg_km
        implicit none
        class(world_container), target, intent(inout) :: w
        integer, intent(in) :: t
        
        integer :: jp, pop_dens_adj
        type(Grid), pointer :: grid
        integer :: i, j, k, id
        real(8) :: flow_x_sum, flow_y_sum
        type(Agent), pointer :: agent_ptr

        grid => w%grid

        ! We process each population
        do jp = 1, w%config%npops
        
            ! 1. Pure Density
            call grid%update_density_pure()
            
            ! 2. Calculate agent flows
            do i = 1, grid%nx
                do j = 1, grid%ny
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
            
            ! 3. Smoothing (human_density_smoothed)
            if (w%config%delta_lat > 0.0d0 .and. deg_km > 0.0d0) then
                pop_dens_adj = int(w%config%sigma_u(jp) / 2.0d0 / (w%config%delta_lat * deg_km))
            else
                pop_dens_adj = 0
            end if
            
            if (pop_dens_adj > 0) then
                 call grid%apply_box_filter(pop_dens_adj)
            else
                 do i = 1, grid%nx
                    do j = 1, grid%ny
                        grid%cell(i,j)%human_density_smoothed = grid%cell(i,j)%human_density
                    end do
                 end do
            end if
            
            ! 4. Base HEP transfer (hep_av = hep)
            grid%hep_av(:,:,jp) = grid%hep(:,:,jp, grid%t_hep)
            
        end do
        
    end subroutine update_density_and_hep_grid

end module mod_technical_modules
