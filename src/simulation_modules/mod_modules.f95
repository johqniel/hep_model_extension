module mod_modules

    use mod_constants
    use mod_config
    use mod_hashmap
    use mod_rnorm
    use mod_grid_id
    use mod_agent_world
    use mod_calculations

    implicit none

    contains

subroutine agent_move_langevin(current_agent)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent
    type(world_config), pointer :: config
    type(Grid), pointer :: grid

    real(8) :: old_x, old_y, new_x, new_y
    real(8) :: ux_old, uy_old, ux_new, uy_new
    real(8) :: grad_x, grad_y, heploc_max
    real(8) :: heploc(0:8)
    integer :: gxx(0:8), gyy(0:8)
    integer :: gx, gy, il, iloc, jp
    real(8) :: ax, ay, sqdt
    
    ! Coefficients (Hardcoded for now as they are missing in config)
    ! Adjust these values to tune the Langevin dynamics
    real(8) :: cb1 = 500.0d0   ! Gradient Attraction Strength
    real(8) :: cb2 = 0.2d0     ! Friction / Damping
    real(8) :: cb3 = 20.0d0    ! Random Diffusion Strength

    ! 1. Pointer setup and state check
    if (current_agent%is_dead) return

    config => current_agent%world%config
    grid   => current_agent%grid
    jp     =  current_agent%population
    sqdt   =  sqrt(config%dt)

    old_x  = current_agent%pos_x
    old_y  = current_agent%pos_y
    ux_old = current_agent%ux
    uy_old = current_agent%uy

    ! 2. Identify current grid cell
    call calculate_grid_pos(old_x, old_y, gx, gy, config)

    ! 3. Gradient Calculation (Moore Neighborhood)
    ! Only calculate gradient if we aren't on the absolute edge of the grid
    if (gx > 1 .and. gx < config%dlon_hep .and. gy > 1 .and. gy < config%dlat_hep) then
        
        ! Map the 8 neighbors
        ! Use jp and grid%t_hep for HEP index
        heploc(0) = grid%hep(gx, gy, jp, grid%t_hep)
        gxx(0) = gx; gyy(0) = gy
        
        heploc(1) = grid%hep(gx-1, gy-1, jp, grid%t_hep); gxx(1) = gx-1; gyy(1) = gy-1
        heploc(2) = grid%hep(gx,   gy-1, jp, grid%t_hep); gxx(2) = gx;   gyy(2) = gy-1
        heploc(3) = grid%hep(gx+1, gy-1, jp, grid%t_hep); gxx(3) = gx+1; gyy(3) = gy-1
        heploc(4) = grid%hep(gx+1, gy,   jp, grid%t_hep); gxx(4) = gx+1; gyy(4) = gy
        heploc(5) = grid%hep(gx+1, gy+1, jp, grid%t_hep); gxx(5) = gx+1; gyy(5) = gy+1
        heploc(6) = grid%hep(gx,   gy+1, jp, grid%t_hep); gxx(6) = gx;   gyy(6) = gy+1
        heploc(7) = grid%hep(gx-1, gy+1, jp, grid%t_hep); gxx(7) = gx-1; gyy(7) = gy+1
        heploc(8) = grid%hep(gx-1, gy,   jp, grid%t_hep); gxx(8) = gx-1; gyy(8) = gy

        heploc_max = -9999.d0
        iloc = 0
        do il = 0, 8
            if (heploc(il) > heploc_max) then
                heploc_max = heploc(il)
                iloc = il
            end if
        end do

        ! Calculate the gradient towards the "best" local cell
        if (iloc == 0) then
            grad_x = 0.d0
            grad_y = 0.d0
        else
            ! Geographic distance scaling
            grad_x = (heploc_max - heploc(0)) / &
                     ((grid%lon_hep(gxx(iloc)) - old_x) * cos(old_y * deg_rad) * deg_km)
            grad_y = (heploc_max - heploc(0)) / &
                     ((grid%lat_hep(gyy(iloc)) - old_y) * deg_km)
        end if
    else
        grad_x = 0.d0
        grad_y = 0.d0
    end if

    ! 4. Generate Random Diffusion (Normal distribution)
    ax = rnorm_single(0.0d0, sqdt)
    ay = rnorm_single(0.0d0, sqdt)

    ! 5. Update Velocity (Langevin Equation)
    ! Formula: u_new = u_old + attraction - drag + random_diffusion
    ux_new = ux_old + (cb1 * grad_x) - (ux_old * cb2) + (cb3 * ax)
    uy_new = uy_old + (cb1 * grad_y) - (uy_old * cb2) + (cb3 * ay)

    ! 6. Calculate New Position
    new_x = old_x + ux_new / (deg_km * cos(old_y * deg_rad)) * config%dt
    new_y = old_y + uy_new / deg_km * config%dt

    ! 7. Boundary and Water Checks
    call calculate_grid_pos(new_x, new_y, gx, gy, config)

    ! Check if out of bounds
    if (gx < 1 .or. gx > config%dlon_hep .or. gy < 1 .or. gy > config%dlat_hep) then
        call current_agent%agent_dies(reason=3)
        return
    endif

    ! Check if in water
    ! grid%cell(i,j)%is_water == 1 means water
    if (grid%cell(gx, gy)%is_water == 1) then
        call current_agent%agent_dies(reason=3)
    else
        ! Move is valid: Update position and velocity
        call current_agent%update_pos(new_x, new_y)
        current_agent%ux = ux_new
        current_agent%uy = uy_new
    endif

end subroutine agent_move_langevin

end module mod_modules
