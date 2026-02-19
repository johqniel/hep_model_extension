module mod_modules_hash
    use mod_constants
    use mod_config
    use mod_hashmap
    use mod_rnorm
    use mod_grid_id
    use mod_agent_world
    use mod_calculations

contains



! YS, 18Feb2026
!*******************************************************
subroutine agent_motion(current_agent)
!-------------------------------------------------------
! Y Shao, 18Feb2026
! Update individual agent position
!-------------------------------------------------------
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
    real(8) :: ax, ay

    real(8) :: lon_min, lon_max, lat_min, lat_max                       ! YS, 17Feb2026 define boundaries
    integer :: gx_w, gy_w                                               ! YS, 17022026, local, test if surface is water

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

    old_x  = current_agent%pos_x
    old_y  = current_agent%pos_y
    ux_old = current_agent%ux
    uy_old = current_agent%uy

    ! 2. Identify current grid cell
    call calculate_grid_pos(old_x, old_y, gx, gy, config)

    ! 3. Gradient Calculation (Moore Neighborhood)
    call calculate_gradient(gx, gy, jp, old_y, current_agent, grad_x, grad_y)

    ! 4. Generate Random Diffusion (Normal distribution)
    ax = rnorm_single(0.0d0, config%sqdt)
    ay = rnorm_single(0.0d0, config%sqdt)

    ! 5. Update Velocity (Langevin Equation)
    ! Formula: u_new = u_old + attraction - drag + random_diffusion
    ux_new = ux_old + (cb1 * grad_x) - (ux_old * cb2) + (cb3 * ax)
    uy_new = uy_old + (cb1 * grad_y) - (uy_old * cb2) + (cb3 * ay)

    ! 6. Calculate New Position
    new_x = old_x + ux_new / (deg_km * cos(old_y * deg_rad)) * config%dt
    new_y = old_y + uy_new / deg_km * config%dt

    ! 7. Boundary Reflection
    lon_min = grid%lon_hep(1) - 0.5*config%delta_lon
    lon_max = grid%lon_hep(config%dlon_hep) + 0.5*config%delta_lon
    lat_min = grid%lat_hep(1) - 0.5*config%delta_lat
    lat_max = grid%lat_hep(config%dlat_hep) + 0.5*config%delta_lat

    if (new_x < lon_min) then
      new_x  = 2.*lon_min - new_x
      ux_new = 2.*(new_x - old_x)/config%dt - ux_old
    elseif (new_x > lon_max) then
      new_x  = 2.*lon_max - new_x
      ux_new = 2.*(new_x - old_x)/config%dt - ux_old
    endif

    if (new_y < lat_min) then
      new_y = 2.*lat_min - new_y
      uy_new = 2.*(new_y - old_y)/config%dt - uy_old
    elseif (new_y > lat_max) then
      new_y = 2.*lat_max - new_y
      uy_new = 2.*(new_y - old_y)/config%dt - uy_old
    endif

    ! 8. Water Surface Check, stay there, but change velocity and hope for new direction
    call calculate_grid_pos(new_x, new_y, gx, gy, config)
    if (grid%cell(gx, gy)%is_water == 1) then                 ! grid%cell(gx,gy)%is_water == 1 means water surface
      new_x = old_x
      new_y = old_y
      ux_new = cb3*ax
      uy_new = cb3*ay
      call calculate_grid_pos(new_x, new_y, gx, gy, config)
    endif

    ! 9. Final checks, but should not happen
    if ( (gx < 1 .or. gx > config%dlon_hep) .or. &
         (gy < 1 .or. gy > config%dlat_hep) .or. &
         (grid%cell(gx, gy)%is_water == 1 )     ) then
      call current_agent%agent_dies(reason=3)
      return
    endif

    ! 10. Update Position and Velocity for Valid Move
    call current_agent%update_pos(new_x, new_y)                ! YS, 17Feb2026, Why not include velocity in the update? Improve
    current_agent%ux = ux_new
    current_agent%uy = uy_new

end subroutine agent_motion

subroutine calculate_gradient(gx, gy, jp, ylat, current_agent, grad_x, grad_y)
!****************************************************************************
!----------------------------------------------------------------------------
! YS, 17Feb2026
! Calculates gradient toward maximum HEP value in Moore neighborhood
!----------------------------------------------------------------------------
    implicit none
   ! input variables
    integer, intent(in) :: gx, gy, jp
    real*8, intent(in) :: ylat

   ! Output variables
    real*8, intent(out) :: grad_x, grad_y

    type(Agent), pointer, intent(in) :: current_agent  ! Pass agent instead of grid/config

    ! Local variables
    type(Grid), pointer :: grid
    type(world_config), pointer :: config
    real*8 :: heploc(0:8), heploc_max
    integer :: gxx(0:8), gyy(0:8)
    integer :: il, iloc
    logical :: valid_neighbor(0:8)

    ! Get local grid and local config from the agent
    grid => current_agent%grid
    config => current_agent%world%config

    ! Initialize
    grad_x = 0.d0
    grad_y = 0.d0
    valid_neighbor = .false.
    heploc_max = -9999.d0
    iloc = 0

    ! Check if point is within grid
    if (gx < 1 .or. gx > config%dlon_hep .or. &
        gy < 1 .or. gy > config%dlat_hep) return

    ! Center point
    ! heploc(0) = grid%hep(gx, gy, jp, grid%t_hep)
    heploc(0) = grid%hep_av(gx, gy, jp)   ! hepC
    gxx(0) = gx
    gyy(0) = gy
    valid_neighbor(0) = .true.

    ! Neighbors (only if within bounds)
    ! Top-left
    if (gx-1 >= 1 .and. gy-1 >= 1) then
        ! heploc(1) = grid%hep(gx-1, gy-1, jp, grid%t_hep)
        heploc(1) = grid%hep_av(gx-1, gy-1, jp)
        gxx(1) = gx-1; gyy(1) = gy-1
        valid_neighbor(1) = .true.
    endif

    ! Top
    if (gy-1 >= 1) then
        ! heploc(2) = grid%hep(gx, gy-1, jp, grid%t_hep)
        heploc(2) = grid%hep_av(gx, gy-1, jp)
        gxx(2) = gx; gyy(2) = gy-1
        valid_neighbor(2) = .true.
    endif

    ! Top-right
    if (gx+1 <= config%dlon_hep .and. gy-1 >= 1) then
        ! heploc(3) = grid%hep(gx+1, gy-1, jp, grid%t_hep) 
        heploc(3) = grid%hep_av(gx+1, gy-1, jp)
        gxx(3) = gx+1; gyy(3) = gy-1
        valid_neighbor(3) = .true.
    endif

    ! Right
    if (gx+1 <= config%dlon_hep) then
        ! heploc(4) = grid%hep(gx+1, gy, jp, grid%t_hep)
        heploc(4) = grid%hep_av(gx+1, gy, jp)
        gxx(4) = gx+1; gyy(4) = gy
        valid_neighbor(4) = .true.
    endif

    ! Bottom-right
    if (gx+1 <= config%dlon_hep .and. gy+1 <= config%dlat_hep) then
        ! heploc(5) = grid%hep(gx+1, gy+1, jp, grid%t_hep)
        heploc(5) = grid%hep_av(gx+1, gy+1, jp)
        gxx(5) = gx+1; gyy(5) = gy+1
        valid_neighbor(5) = .true.
    endif

    ! Bottom
    if (gy+1 <= config%dlat_hep) then
        ! heploc(6) = grid%hep(gx, gy+1, jp, grid%t_hep)
        heploc(6) = grid%hep_av(gx, gy+1, jp)
        gxx(6) = gx; gyy(6) = gy+1
        valid_neighbor(6) = .true.
    endif

    ! Bottom-left
    if (gx-1 >= 1 .and. gy+1 <= config%dlat_hep) then
        ! heploc(7) = grid%hep(gx-1, gy+1, jp, grid%t_hep)
        heploc(7) = grid%hep_av(gx-1, gy+1, jp)
        gxx(7) = gx-1; gyy(7) = gy+1
        valid_neighbor(7) = .true.
    endif

    ! Left
    if (gx-1 >= 1) then
        ! heploc(8) = grid%hep(gx-1, gy, jp, grid%t_hep)
        heploc(8) = grid%hep_av(gx-1, gy, jp)
        gxx(8) = gx-1; gyy(8) = gy
        valid_neighbor(8) = .true.
    endif

    ! Find maximum among valid neighbors
    do il = 0, 8
        if (valid_neighbor(il) .and. heploc(il) > heploc_max) then
            heploc_max = heploc(il)
            iloc = il
        end if
    end do

    ! Calculate gradient toward maximum
    if (iloc /= 0) then
        if (iloc == 2 .or. iloc == 6) then
            ! North or South only
            grad_y = (heploc_max - heploc(0)) / &
                     ((grid%lat_hep(gyy(iloc)) - grid%lat_hep(gyy(0))) * deg_km)
        elseif (iloc == 4 .or. iloc == 8) then
            ! East or West only
            grad_x = (heploc_max - heploc(0)) / &
                     ((grid%lon_hep(gxx(iloc)) - grid%lon_hep(gxx(0))) * &
                      cos(ylat*deg_rad) * deg_km)
        else
            ! Diagonal or corner
            if (gxx(iloc) /= gxx(0)) then
                grad_x = (heploc_max - heploc(0)) / &
                         ((grid%lon_hep(gxx(iloc)) - grid%lon_hep(gxx(0))) * &
                          cos(ylat*deg_rad) * deg_km)
            endif
            if (gyy(iloc) /= gyy(0)) then
                grad_y = (heploc_max - heploc(0)) / &
                         ((grid%lat_hep(gyy(iloc)) - grid%lat_hep(gyy(0))) * deg_km)
            endif
        endif
    endif

end subroutine calculate_gradient

!**************************************************************
logical function agent_above_water(gx, gy, jp, t_hep, grid_ptr)
!**************************************************************
!----------------------------------------------------------------------
! Daniel N. (improvement YShao, 18Feb2026)
! logical function: agent_above_water
! Returns .true. if an agent from population jp that is in grid (gx, gy)
! that is a water surface
!-----------------------------------------------------------------------
  implicit none
  integer, intent(in) :: jp, gx, gy, t_hep
  type(Grid), pointer :: grid_ptr

  agent_above_water = .false.

  if ( .not. allocated(grid_ptr%hep) ) then
    print *, "t_hep is not associated."
    return
  endif

  if ( gx    < lbound(grid_ptr%hep,1) .or. gx    > ubound(grid_ptr%hep,1) .or. &
       gy    < lbound(grid_ptr%hep,2) .or. gy    > ubound(grid_ptr%hep,2) .or. &
       jp    < lbound(grid_ptr%hep,3) .or. jp    > ubound(grid_ptr%hep,3) .or. &
       t_hep < lbound(grid_ptr%hep,4) .or. t_hep > ubound(grid_ptr%hep,4) ) then
    print *, "Warning: index out of bounds gx:    ", gx,    lbound(grid_ptr%hep,1), ubound(grid_ptr%hep,1)
    print *, "Warning: index out of bounds gy:    ", gy,    lbound(grid_ptr%hep,2), ubound(grid_ptr%hep,2)
    print *, "Warning: index out of bounds jp:    ", jp,    lbound(grid_ptr%hep,3), ubound(grid_ptr%hep,3)
    print *, "Warning: index out of bounds t_hep: ", t_hep, lbound(grid_ptr%hep,4), ubound(grid_ptr%hep,4)
    return
  endif

  if ( grid_ptr%hep(gx, gy, jp, t_hep) == -1 )    then
    agent_above_water = .true.
  endif

end function agent_above_water

end module mod_modules_hash
