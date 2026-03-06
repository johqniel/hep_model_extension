module mod_constants
    implicit none
    
    ! Mathematical constants
    real(8), parameter :: pi = 3.14159265358979323846d0
    real(8), parameter :: deg_rad = pi / 180.0d0

    ! Physical constants
    real(8), parameter :: R_earth = 6371.0d0 ! Earth radius in km (Renamed from R to avoid single letter)
    real(8), parameter :: deg_km = 111.32d0 ! km per degree

end module mod_constants
