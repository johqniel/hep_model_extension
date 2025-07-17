module mod_grid_utilities

    

    implicit none

function area_of_gridcell(i,j, lon_in, lat_in, R) result(area)
    implicit none
    integer, intent(in) :: i, j
    real(8), dimension(:), intent(in) :: lon_in, lat_in
    real(8), intent(in) :: R       ! Earth's radius [km]
    real(8) :: area                ! Resulting area [km^2]

    real(8) :: lat1, lat2, lon1, lon2
    real(8) :: haversine1, haversine2, dist_lon1, dist_lon2, haversine_lat, dist_lat
    real(8), parameter :: pi = 3.14159265358979

    ! Compute latitude borders
    if (j == 1) then
        lat1 = lat_in(1) - (lat_in(2) - lat_in(1)) / 2.0
    else
        lat1 = (lat_in(j) + lat_in(j-1)) / 2.0
    end if

    if (j == size(lat_in)) then
        lat2 = lat_in(j) + (lat_in(j) - lat_in(j-1)) / 2.0
    else
        lat2 = (lat_in(j+1) + lat_in(j)) / 2.0
    end if

    ! Compute longitude borders
    if (i == 1) then
        lon1 = lon_in(1) - (lon_in(2) - lon_in(1)) / 2.0
    else
        lon1 = (lon_in(i) + lon_in(i-1)) / 2.0
    end if

    if (i == size(lon_in)) then
        lon2 = lon_in(i) + (lon_in(i) - lon_in(i-1)) / 2.0
    else
        lon2 = (lon_in(i+1) + lon_in(i)) / 2.0
    end if

    ! Latitude distance using haversine
    haversine_lat = sin(abs(lat2 - lat1) * pi / (2.0 * 180.0))**2
    dist_lat = 2.0 * R * atan2(sqrt(haversine_lat), sqrt(1.0 - haversine_lat))

    ! Longitude distances at both latitudes
    haversine1 = cos(lat2 * pi / 180.0)**2 * sin(abs(lon2 - lon1) * pi / (2.0 * 180.0))**2
    dist_lon1 = 2.0 * R * atan2(sqrt(haversine1), sqrt(1.0 - haversine1))

    haversine2 = cos(lat1 * pi / 180.0)**2 * sin(abs(lon2 - lon1) * pi / (2.0 * 180.0))**2
    dist_lon2 = 2.0 * R * atan2(sqrt(haversine2), sqrt(1.0 - haversine2))

    ! Trapezoid-like area approximation
    area = (dist_lon1 + dist_lon2) / 2.0 * sqrt(dist_lat**2 - ((dist_lon1 - dist_lon2)/2.0)**2)

end function area_of_gridcell

end module mod_grid_utilities