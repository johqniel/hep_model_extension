module mod_calculations

use mod_globals
! Uses: lon_hep, lat_hep, 

    contains


subroutine calculate_grid_pos(x,y,gx,gy)
    integer, intent(out) :: gx,gy
    real(8), intent(in) :: x,y 

    real(8) :: lon_0, lat_0, delta_lat, delta_lon


    delta_lon = lon_hep(2)-lon_hep(1)
    delta_lat = lat_hep(2)-lat_hep(1)
    lon_0     = lon_hep(1) - 0.5*delta_lon
    lat_0     = lat_hep(1) - 0.5*delta_lat


    if (x < lon_hep(1) .or. x > lon_hep(size(lon_hep))) then
            !print*, "Grid position is outside of grid. (x) "
            gx = -1
            gy = -1
            return
    endif
    if (y < lat_hep(1) .or. y > lat_hep(size(lat_hep))) then
            !print*, "Grid position is outside of grid. (y)"
            gx = -1 
            gy = -1
            return
    endif



    gx = floor( (x- lon_0)/delta_lon ) + 1
    gy = floor( (y - lat_0)/delta_lat ) + 1


end subroutine calculate_grid_pos

  function random_sample(n, m) result(sample)
    ! Returns n random distinct integers between 1 and m
    integer, intent(in) :: n, m
    integer :: sample(n)
    integer :: pool(m)
    integer :: i, j, k
    real :: r

    if (n > m) then
       print *, "Error: n cannot be larger than m"
       stop
    end if

    ! Initialize pool with 1..m
    do i = 1, m
       pool(i) = i
    end do

    ! Fisher–Yates shuffle for the first n elements
    do i = 1, n
       call random_number(r)
       j = i + int(r * (m - i + 1))  ! pick index between i and m
       k = pool(i)
       pool(i) = pool(j)
       pool(j) = k
    end do

    ! First n numbers are the sample
    sample = pool(1:n)
  end function random_sample

end module mod_calculations