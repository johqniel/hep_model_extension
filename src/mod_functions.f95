module mod_functions

    use mod_kinds                               ! standadizedprecision of ints and reals 
    use mod_utility                             ! generall utility functions
    use mod_rnorm, only: dp, rnorm_vec          ! random gaussian num generator
    use omp_lib                                 ! OpenMP library for parallelization

    implicit none
    public
    contains

    subroutine strt_distr_gaussian(hum, xc, yc, spread, x0, y0, sig_u, ux0, uy0)
            !-------------------------------------------------------------------------
            ! Yaping Shao, 2 Jul 2024
            ! Generate Gaussian spatially-distribution of humans
            ! Input:
            ! hum: number of humans
            ! xc, yc: x- and y-locations of the population center
            ! spread: position std
            ! 
            ! Output:
            ! x0, y0: positions of humans
            ! ux0, uy0: velocity of humans
            !--------------------------------------------------------------------------
        implicit none
        integer, intent(in) :: hum
        integer :: IX, IERR
        real(8), allocatable, dimension(:) :: Ax, Ay
        real(8), intent(in) :: xc, yc, spread, sig_u
        real(8), intent(out), dimension(hum) :: x0, y0, ux0, uy0

        allocate(Ax(1:hum), Ay(1:hum))

        Ax = rnorm_vec(hum, 0.d0, spread)
        Ay = rnorm_vec(hum, 0.d0, spread)

        x0(:) = Ax(:) + xc
        y0(:) = Ay(:) + yc

        Ax = rnorm_vec(hum, 0.d0, sig_u)
        Ay = rnorm_vec(hum, 0.d0, sig_u)

        ux0(:) = Ax(:)
        uy0(:) = Ay(:)

        deallocate (Ax, Ay)
        return
    end subroutine strt_distr_gaussian

    subroutine calc_area_gridcell(lon_in, lat_in, R, area)
        !----------------------------------------------------------
        ! YS, Purpose: compute gridcell area for calculation of density
        ! Input
        ! lon_in, lat_in: longitude and latitude of grid cells
        ! R, radius of earth in [km]
        ! Output
        ! area, area of gridcell in [km^2]
        !
        ! called by main
        !
        ! |-----x------|------x-----|
        ! BDL   LON    BDL   LON    BDL
        !----------------------------------------------------------
        implicit none
        real(8), dimension(:), intent(in) :: lat_in, lon_in
        integer :: i, j
        integer :: dlat, dlon
        real(8) :: R
        real(8) :: haversine1, haversine2, dist_lon1, dist_lon2, haversine_lat, dist_lat
        real(8), allocatable, dimension(:) :: lat_borders
        real(8), allocatable, dimension(:) :: lon_borders
        real(8), allocatable, dimension(:,:), intent(out) :: area
        real(8), parameter :: pi = 3.14159265358979

        dlat = size(lat_in)
        dlon = size(lon_in)

        allocate(lat_borders(0:dlat))
        allocate(lon_borders(0:dlon))
        allocate(area(1:dlon, 1:dlat))

        do i = 1,dlat-1
            lat_borders(i) = (lat_in(i+1) + lat_in(i))/2.
        end do
        lat_borders(0) = lat_in(1)       - ( lat_in(2) - lat_in(1) )/2.    ! lower boundary has index 0, YS 28.07.2024 last term correction
        lat_borders(dlat) = lat_in(dlat) + ( lat_in(2) - lat_in(1) )/2.    ! upper boundary has last dlat

        do j = 1,dlon-1
            lon_borders(j) = (lon_in(j+1) + lon_in(j))/2.
        end do
        lon_borders(0)    = lon_in(1)    - ( lon_in(2) - lon_in(1) )/2. 
        lon_borders(dlon) = lon_in(dlon) + ( lon_in(2) - lon_in(1) )/2.   

        do j = 1, dlat
            haversine_lat = sin( abs(lat_borders(j) - lat_borders(j-1)) * pi / (2. * 180.)) **2
            dist_lat = 2. * R * atan2(sqrt(haversine_lat), sqrt(1 - haversine_lat))
            do i = 1,dlon
                haversine1 = cos(lat_borders(j) * pi / 180.)**2 * &
                             sin( abs(lon_borders(i) - lon_borders(i-1)) * pi / (2. * 180))**2
                dist_lon1 = 2. * R * atan2(sqrt(haversine1), sqrt(1 - haversine1))                             ! YS, [km], because R in km
                haversine2 = cos(lat_borders(j-1) * pi / 180.)**2 * &
                        sin( abs(lon_borders(i) - lon_borders(i-1)) * pi / (2. * 180))**2
                dist_lon2 = 2. * R * atan2(sqrt(haversine2), sqrt(1 - haversine2))                             ! YS, [km], because R in km 
                area(i,j) = (dist_lon1 + dist_lon2)/2. *  sqrt(dist_lat**2 - ((dist_lon1 - dist_lon2)/2.)**2)  ! YS, [km^2]
            end do
        end do

    end subroutine calc_area_gridcell

    subroutine pop_dens_flow_func(posx, posy, ux, uy, lon_in, lat_in, area, hep, water_hep, pop_dens_adj, hum_dens_adj, &
            hum_dens, irho, hum_flow)

        implicit none
        real(8), dimension(:), intent(in) :: posx, posy, ux, uy
        real(8), dimension(:), intent(in) :: lat_in, lon_in
        integer, intent(in) :: pop_dens_adj
        real(8), intent(in) :: water_hep
        real(8), dimension(:,:), intent(in) :: area, hep
        real(8), dimension(:,:), intent(inout) :: hum_dens, hum_dens_adj
        real(8), dimension(:,:,:), intent(inout) :: hum_flow
        integer, dimension(:,:), intent(inout) :: irho                          ! humber of humans in cell

        integer :: dlat, dlon, hum, gx, gy
        integer :: h, i, j, k, l, amo_grid, left_boundary, right_boundary, upper_boundary, lower_boundary

        real(8) :: delta_lon, delta_lat, lon_0, lat_0

        dlat = size(lat_in)
        dlon = size(lon_in)
        hum = size(posx)

        delta_lon = lon_in(2)-lon_in(1)
        delta_lat = lat_in(2)-lat_in(1)
        lon_0     = lon_in(1) - 0.5*delta_lon
        lat_0     = lat_in(1) - 0.5*delta_lat

        hum_dens(:,:) = 0.d0
        irho (:,:) = 0
        hum_dens_adj(:,:) = 0.d0
        hum_flow(:,:,:) = 0.d0

        do h = 1, hum                                                            ! ys do not like the boundaries, redo
            if ((posx(h) < lon_in(1)) .or. (posx(h) > lon_in(dlon))) then
                gx = -1
            else
                !                gx = minloc(abs(lon_in - posx(h)), dim=1)
              gx = floor( ( posx(h) - lon_0 ) / delta_lon ) + 1
            end if

            if ((posy(h) < lat_in(1)) .or. (posy(h) > lat_in(dlat))) then
                gy = -1
            else
            !                gy = minloc(abs(lat_in - posy(h)), dim=1)
              gy = floor( ( posy(h) - lat_0 ) / delta_lat ) + 1
            end if

            if ((gx /= -1) .and. (gy /= -1)) then
                irho(gx,gy)       = irho(gx,gy) + 1
            !                hum_dens(gx,gy)   = hum_dens(gx,gy) + 1
                hum_flow(1,gx,gy) = hum_flow(1,gx,gy) + ux(h)
                hum_flow(2,gx,gy) = hum_flow(2,gx,gy) + uy(h)
            end if
        end do

        hum_dens(:,:)   = irho(:,:)       * 100. / area(:,:)
        hum_flow(1,:,:) = hum_flow(1,:,:) * 100. / area(:,:)                   ! ys 1Aug2024, gives [No. of humans/100 km/yr], seems to be ok
        hum_flow(2,:,:) = hum_flow(2,:,:) * 100. / area(:,:)

        if (pop_dens_adj > 10) then
            hum_dens_adj(:,:) = 0
            do i = 1, size(lon_in)
                left_boundary = -pop_dens_adj
                right_boundary = pop_dens_adj
                if (i < pop_dens_adj + 1) then
                    left_boundary = 1 - i
                elseif (i > size(lon_in) - pop_dens_adj) then
                    right_boundary = size(lon_in) - i
                end if
                do j = 1, size(lat_in)
                    if (hep(i, j) == water_hep) then
                        cycle
                    end if
                    lower_boundary = -pop_dens_adj
                    upper_boundary = pop_dens_adj
                    if (j < pop_dens_adj + 1) then
                        lower_boundary = 1 - j
                    elseif (j > size(lat_in) - pop_dens_adj) then
                        upper_boundary = size(lat_in) - j
                    end if

                    amo_grid = (-left_boundary + right_boundary + 1) * (-lower_boundary + upper_boundary + 1)

                    do k = left_boundary, right_boundary
                        do l = lower_boundary, upper_boundary
                            if (hep(i+k, j+l) /= water_hep) then
                                hum_dens_adj(i, j) = hum_dens_adj(i, j) + hum_dens(i+k, j+l)
                            else
                                amo_grid = amo_grid - 1
                            end if
                        end do
                    end do
                    hum_dens_adj(i, j) = hum_dens_adj(i,j) / amo_grid
                end do
            end do
        else
            hum_dens_adj(:,:) = hum_dens(:,:)
        end if

    end subroutine pop_dens_flow_func

    subroutine pop_pressure_func(rho, hep, N_max, eta, epsilon, pop_pressure)
        !
        ! YS, 9 Aug 2024, checked
        ! Code by Konstantin Klein
        ! Returns pop_pressure for given rho and rho_c = hep * N_max 
        !
        implicit none
        real(8), dimension(:,:), intent(in)    :: rho       ! Population Density of the grid  [PDU]
        real(8), dimension(:,:), intent(in)    :: hep       ! Accessible HEP                  [   ]                 
        real(8), intent(in)                    :: N_max     ! Cultural carry capacity         [PDU]
        real(8), intent(in)                    :: eta       ! Weibull parameter
        real(8), intent(in)                    :: epsilon   ! Weilbull parameter

        real(8), dimension(:,:), allocatable   :: rho_c     ! Local carrying capacity         [PDU] 
        real(8), dimension(:,:), allocatable   :: delta_rho ! rho/rho_c                       [   ]                                
        integer, dimension(2) :: shape_rho
        real(8) :: max_pp                                   ! max of Weibull, occurs at epsilon(1-1/eta)^1/eta                           

        real(8), dimension(:,:), intent(out) :: pop_pressure

        shape_rho = shape(rho)

        allocate(rho_c(shape_rho(1),shape_rho(1)), delta_rho(shape_rho(1),shape_rho(2)))

        max_pp = (eta/epsilon) * (1.-1./eta)**(1.-1./eta) * exp(-(1.-1./eta))                ! ys, checked correct 9Aug2024
        rho_c = N_max * hep                                               
        delta_rho = rho/rho_c
        pop_pressure = (eta/epsilon) * (delta_rho/epsilon)**(eta-1) * exp(-(delta_rho/epsilon)**eta)&
                /max_pp

        where (hep <= 0)
            pop_pressure = 1.
        end where
        return
    end subroutine pop_pressure_func

end module mod_functions
