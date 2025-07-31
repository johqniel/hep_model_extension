module mod_calculations

use mod_setup_hep

    contains


subroutine calculate_grid_pos(x,y,gx,gy)
    integer, intent(out) :: gx,gy
    real, intent(in) :: x,y 

    real(8) :: lon_0, lat_0, delta_lat, delta_lon

    delta_lon = lon_hep(2)-lon_hep(1)
    delta_lat = lat_hep(2)-lat_hep(1)
    lon_0     = lon_hep(1) - 0.5*delta_lon
    lat_0     = lat_hep(1) - 0.5*delta_lat

    if (x < lon_hep(1) .or. x > size(lon_hep)) then
            print*, "Grid position is outside of grid. (x) "
            gx = -1
            gy = -1
            return
    endif
    if (y < lat_hep(1) .or. y > size(lat_hep)) then
            print*, "AGrid position is outside of grid. (y)"
            gx = -1 
            gy = -1
            return
    endif

    gx = floor( (x- lon_0)/delta_lon ) + 1
    gy = floor( (y - lat_0)/delta_lat ) + 1

end subroutine calculate_grid_pos

end module mod_calculations