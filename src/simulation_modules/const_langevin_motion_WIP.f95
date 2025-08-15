!--------------------------------------------------------------- 
! Yaping Shao, 3 Jul 2025
! information needed for calling motion
!---------------------------------------------------------------

use constants

delta_lon = lon_hep(2) - lon_hep(1)
delta_lat = lat_hep(2) - lat_hep(1)
lon_0     = lon_hep(1) - 0.5*delta_lon
lat_0     = lat_hep(1) - 0.5*delta_lat
!
! set boundary to be out boundary of the simulation domain
! this is what should happen, but ....
!
!*    lon_min_out = lon_hep(1) - 0.5*delta_lon 
!*    lon_max_out = lon_hep(dlon_hep) + 0.5*delta_lon
!*    lat_min_out = lat_hep(1) - 0.5*delta_lat
!*    lat_max_out = lat_hep(dlat_hep) + 0.5*delta_lat
!
! it is coded in computing gradient and density only for gx = 2, dlon-1 and gy = 2, dlat-1
! it is more consistent to set the boundary to smaller
!

lon_min_out = lon_hep(1) + 0.5*delta_lon
lon_max_out = lon_hep(dlon_hep) - 0.5*delta_lon
lat_min_out = lat_hep(1) + 0.5*delta_lat
lat_max_out = lat_hep(dlat_hep) - 0.5*delta_lat

!********************************************************************
!--------------------------------------------------------------------
! YS: 02 Jul 2025
! (1) make sure that before calling motion, the coefficients 
! cb1, cb2 and cb3 for population jp are calculated
! jp is the population index, e.g., jp = 1, 3 
!--------------------------------------------------------------------
cb1(jp) = dt*cb1(jp)
cb2(jp) = dt/tau(jp)
cb3(jp) = sqrt( sigma_u(jp)**2/tau(jp) )          ! sqrt( sigma_u**2/tau )

cb1jp = cb1(jp)
cb2jp = cb2(jp)
cb3jp = cb3(jp)

!--------------------------------------------------------------------
! YS: 02 Jul 2025
! (2) make sure x0, y0, x, y, ux0, uy0, ux, uy are allocated
! (3) make sure hum_jp, number of humans of population jp is given
!--------------------------------------------------------------------

hep_jp(:,:) = hep_av(:,:,jp,t_hep)



call motion(x0, y0, x, y, ux0, uy0, ux, uy, hum_jp, cb1jp, cb2jp, cb3jp, sqdt, &
            hep_jp)



subroutine motion(x0, y0, x, y, ux0, uy0, ux, uy, hum, cb1, cb2, cb3, sqdt, &
&                 hep)

  !-------------------------------------------------------------------------------------
  ! Yaping Shao, 25 Jun 2025
  ! subroutine for agent motion by solving the constrained Langevin equation
  !
  ! Input: 
  ! x0, y0:      position vector of length hum_jp for hum_jp humans, at time step n 
  ! ux0, uy0:    velocity vector of length hum_jp for hum_jp humans, at time step n
  !
  ! Output: 
  ! x, y:        position vector of length hum_jp for hum_jp humans, at time step n+1
  ! ux, uy:      velocity vector of length hum_jp for hum_jp humans, at time step n+1
  !
  ! Parameters:
  ! cb1:       a parameter used in Langevin equation
  ! cb2:       a parameter used in Langevin equation
  ! cb3:       a parameter used in Langevin equation 
  ! sqdt:      square root of dt; dt is time step of agent numerical integration 
  !
  ! hum:       number of humans 
  !--------------------------------------------------------------------------------------------

  include "common_domain.inc"

  real(8), dimension(:), allocatable     :: Ax, Ay   ! random array for x and y component
  real(8), dimension(0:8) :: heploc
  real(8)                 :: heploc_max
  real(8)                 :: grad_x, grad_y, grad    ! gradient in x and y

  integer, dimension(0:8) :: gxx, gyy
  integer                 :: gx, gy, gx1, gy1
  integer                 :: i, il, iloc
  integer                 :: out_count_priv, drown_count_priv 

  allocate(Ax(1:hum), Ay(1:hum))
  Ax = rnorm_vec(hum, 0.d0, sqdt)
  Ay = rnorm_vec(hum, 0.d0, sqdt)

  !-------------------------------------------------------------------------------------------
  ! CHECK:
  ! if agent is alive, in previous version dead agents have coordinates (-1.0E3, -1.0E3)
  ! adjust this to the way Daniel is dealing with dead agents
  !-------------------------------------------------------------------------------------------
  humans:
  do i = 1, hum
    if ( (x0(i) <= -900.) .AND. (y0(i) <= -900.) ) then
      CYCLE
    endif

  !-------------------------------------------------------------------------------------------
  ! CHECK:
  ! if agent is outside of domain
  !-------------------------------------------------------------------------------------------
  if ((x0(i)<lon_min_out) .OR. (x0(i)>lon_max_out) .OR. &
  &    (y0(i)<lat_min_out) .OR. (y0(i)>lat_max_out)) then      ! count this out & place this in dead corner 
    x(i) = -1.0E3
    y(i) = -1.0E3
    out_count_priv = out_count_priv + 1
  else
  !------------------------------------------------------------------------------------------
  ! Find grid (gx, gy) 
  !------------------------------------------------------------------------------------------
    gx = floor( ( x0(i) - lon_0 ) / delta_lon ) + 1 
    gy = floor( ( y0(i) - lat_0 ) / delta_lat ) + 1

  !-------------------------------------------------------------------------------------------
  ! CHECK:
  ! whether human is above water, then counted as drowned. YS do not like this, redo
  !-------------------------------------------------------------------------------------------
    if (hep(gx, gy) <= 0. ) then
      x(i) = -1.0E3
      y(i) = -1.0E3
      drown_count_priv = drown_count_priv + 1
    else
                                            ! Calculate gradient with available HEP
      if ((gx /= 1) .and. (gx /= dlon_hep) .and. (gy /= 1) .and. (gy /= dlat_hep)) then
        heploc(0) = hep(gx,   gy  )   ! hepC
        gxx   (0) = gx
        gyy   (0) = gy
        heploc(1) = hep(gx-1, gy-1)   ! hepSW
        gxx   (1) = gx-1
        gyy   (1) = gy-1
        heploc(2) = hep(gx,   gy-1)   ! hepS
        gxx   (2) = gx
        gyy   (2) = gy-1
        heploc(3) = hep(gx+1, gy-1)   ! hepSE
        gxx   (3) = gx+1
        gyy   (3) = gy-1
        heploc(4) = hep(gx+1, gy  )   ! hepE
        gxx   (4) = gx+1
        gyy   (4) = gy
        heploc(5) = hep(gx+1, gy+1)   ! hepNE
        gxx   (5) = gx+1
        gyy   (5) = gy+1
        heploc(6) = hep(gx,   gy+1)   ! hepN
        gxx   (6) = gx
        gyy   (6) = gy+1
        heploc(7) = hep(gx-1, gy+1)   ! hepNW
        gxx   (7) = gx-1
        gyy   (7) = gy+1
        heploc(8) = hep(gx-1, gy  )   ! hepW
        gxx   (8) = gx-1
        gyy   (8) = gy

        heploc_max = -9999.

        do il = 0, 8
          if ( heploc(il) .gt. heploc_max ) then 
            heploc_max = heploc(il)
            iloc = il 
          endif
        enddo

        if ( iloc == 0 ) then
          grad_x = 0.d0
          grad_y = 0.d0 
        elseif ( iloc == 2 .or. iloc == 6 ) then 
          grad_x = 0.d0
          grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y0(i))*deg_km)
        elseif ( iloc == 4 .or. iloc == 8 ) then 
          grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x0(i))*cos(y0(i)*deg_rad) * deg_km)
          grad_y = 0.d0
        else 
          grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x0(i))*cos(y0(i)*deg_rad) * deg_km)
          grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y0(i))*deg_km)
        endif

  !------------------------------------------------------------------
  ! Update velocity
  !------------------------------------------------------------------
        ux(i) = ux0(i) + cb1*grad_x - ux0(i)*cb2 + cb3*Ax(i)
        uy(i) = uy0(i) + cb1*grad_y - uy0(i)*cb2 + cb3*Ay(i)

        x(i) = x0(i) + ux(i) / (deg_km * cos(y0(i) * deg_rad)) * dt
        y(i) = y0(i) + uy(i) / deg_km * dt

        include "boundary.inc"

        gx1 = floor( ( x(i) - lon_0 ) / delta_lon ) + 1
        gy1 = floor( ( y(i) - lat_0 ) / delta_lat ) + 1
  !
        if ((gx1 < 1) .or. (gx1 > dlon_hep) .or. (gy1 < 1) .or. (gy1 > dlat_hep)) then
          x(i) = -1.0E3
          y(i) = -1.0E3
        else
          if ( hep(gx1, gy1) <= 0. ) then           ! need better reflection scheme later
            x(i) = x0(i)
            y(i) = y0(i)
            ux(i) = cb3*Ax(i)
            uy(i) = cb3*Ay(i)
          endif
        endif

      else                                            ! agents are located at the boundary 
          x(i) = -1.0E3
          y(i) = -1.0E3
          out_count_priv = out_count_priv + 1         ! agents are considered out
      endif
    endif
  endif                     

  deallocate (Ax, Ay)

end SUBROUTINE