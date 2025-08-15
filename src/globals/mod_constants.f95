module mod_constants


!---------------------------------------------------------------------------
! Yaping Shao
! Institute for Geophysics and Meteorology
! University of Cologne, Germany
!---------------------------------------------------------------------------
! HISTORY:
! 19 Dec 2023: initial composition
!---------------------------------------------------------------------------
! Aim: Include file for constants used in N_HSM scheme
!---------------------------------------------------------------------------

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Physical parameters
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
    real(8), parameter :: pi      = 3.14159265358979
    real(8), parameter :: pi2     = 2*pi
    real(8), parameter :: R       = 6371.000785    ! Earth Radius        [km]
    real(8), parameter :: C_earth = 2*pi*R         ! Earth Circumference [km]
    real(8), parameter :: deg_km  = C_earth/360.   ! Convert degree to km (multiplication), km to degree (division) [km/deg°]
    real(8), parameter :: deg_rad = pi/180.        ! Convert degree to radiant (multiplication), radiant to degree (division)


end module mod_constants
