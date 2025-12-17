module mod_read_inputs

    use mod_config
    use mod_basic_config, only: npops_basic => npops, ns_basic => ns, &
        tyr_start, tstep_start, tyr_end, tyr_length, dt, Tn, save_t, delta_t_hep, &
        tau, eta, epsilon, rho_max, r_B, d_B, &
        dt_bdyr, dt_bd, eps, minpts, &
        water_hep, with_pop_pressure, &
        probability_vertilisation_per_tick, age_when_vertile_m, age_when_vertile_f, &
        age_until_vertile_m, age_until_vertile_f, pregnancy_minimum_length, &
        age_until_vertile_m, age_until_vertile_f, pregnancy_minimum_length, &
        birth_prob_after_min_length, hum_0, x_ini_c, y_ini_c, ini_spread, sigma_u, &
        hep_paths
    use netcdf

    implicit none
    
    type :: path_container
        character(len=256), allocatable :: hep_paths(:)
    end type path_container

    type :: hep_data_type
        integer :: npops
        integer :: dlon
        integer :: dlat
        integer :: dtime
        real, allocatable :: matrix(:,:,:,:) ! (lon, lat, pop, time)
        real, allocatable :: lat(:)
        real, allocatable :: lon(:)
    end type hep_data_type

    contains

    function read_world_config() result(config)
        implicit none
        type(world_config) :: config

        config%npops = npops_basic
        config%ns = ns_basic
        
        allocate(config%hum_0(ns_basic, npops_basic))
        allocate(config%x_ini_c(ns_basic, npops_basic))
        allocate(config%y_ini_c(ns_basic, npops_basic))
        allocate(config%ini_spread(ns_basic, npops_basic))
        allocate(config%sigma_u(npops_basic))
        
        config%hum_0 = hum_0
        config%x_ini_c = x_ini_c
        config%y_ini_c = y_ini_c
        config%ini_spread = ini_spread
        config%sigma_u = sigma_u

        ! New parameters
        allocate(config%tyr_start(npops_basic))
        allocate(config%tstep_start(npops_basic))
        allocate(config%tyr_end(npops_basic))
        allocate(config%tyr_length(npops_basic))
        allocate(config%tau(npops_basic))
        allocate(config%cb1(npops_basic))
        allocate(config%eta(npops_basic))
        allocate(config%epsilon(npops_basic))
        allocate(config%rho_max(npops_basic))
        allocate(config%r_B(npops_basic))
        allocate(config%d_B(npops_basic))
        allocate(config%hep_paths(npops_basic))

        config%tyr_start = tyr_start
        config%tstep_start = tstep_start
        config%tyr_end = tyr_end
        config%tyr_length = tyr_length
        config%dt = dt
        config%Tn = Tn
        config%save_t = save_t
        config%delta_t_hep = delta_t_hep
        config%tau = tau
        config%eta = eta
        config%epsilon = epsilon
        config%rho_max = rho_max
        config%r_B = r_B
        config%d_B = d_B
        config%dt_bdyr = dt_bdyr
        config%dt_bd = dt_bd
        config%eps = eps
        config%minpts = minpts
        config%water_hep = water_hep
        config%probability_vertilisation_per_tick = probability_vertilisation_per_tick
        config%age_when_vertile_m = age_when_vertile_m
        config%age_when_vertile_f = age_when_vertile_f
        config%age_until_vertile_m = age_until_vertile_m
        config%age_until_vertile_f = age_until_vertile_f
        config%pregnancy_minimum_length = pregnancy_minimum_length
        config%birth_prob_after_min_length = birth_prob_after_min_length
        config%with_pop_pressure = with_pop_pressure
        config%hep_paths = hep_paths

    end function read_world_config

    function read_hep_data(paths) result(hep_data)
        implicit none
        character(len=*), dimension(npops_basic), intent(in) :: paths
        type(hep_data_type) :: hep_data
        
        integer :: ncid, varid, dimid
        integer :: dlon_hep, dlat_hep, dt_hep
        integer :: jp
        integer :: status_code
        
        ! Temporary arrays
        real, allocatable :: hep_wk(:,:,:)

        ! Open the first file to get dimensions
        print *, "DEBUG: read_hep_data: Opening file (hardcoded): input/hep/europe/AUR.nc"
        print *, "DEBUG: nf90_nowrite = ", nf90_nowrite
        print *, "DEBUG: nf90_noerr = ", nf90_noerr
        
        ncid = -999
        print *, "DEBUG: Calling nf90_open..."
        status_code = nf90_open("input/hep/europe/AUR.nc", nf90_nowrite, ncid)
        print *, "DEBUG: nf90_open returned status: ", status_code
        print *, "DEBUG: ncid after open: ", ncid
        
        call check(status_code)

        ! Get dimensions
        print *, "DEBUG: Calling nf90_inq_dimid for lon..."
        status_code = nf90_inq_dimid(ncid, "lon", dimid)
        print *, "DEBUG: nf90_inq_dimid returned: ", status_code
        call check(status_code)
        call check(nf90_inquire_dimension(ncid, dimid, len=dlon_hep))

        print *, "DEBUG: Calling nf90_inq_dimid for lat..."
        call check(nf90_inq_dimid(ncid, "lat", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dlat_hep))

        print *, "DEBUG: Calling nf90_inq_dimid for time..."
        status_code = nf90_inq_dimid(ncid, "time", dimid)
        print *, "DEBUG: nf90_inq_dimid for time returned: ", status_code
        print *, "DEBUG: dimid for time: ", dimid
        call check(status_code)
        
        print *, "DEBUG: Calling nf90_inquire_dimension for time..."
        status_code = nf90_inquire_dimension(ncid, dimid, len=dt_hep)
        print *, "DEBUG: nf90_inquire_dimension returned: ", status_code
        call check(status_code)

        ! Set metadata
        hep_data%npops = npops_basic
        hep_data%dlon = dlon_hep
        hep_data%dlat = dlat_hep
        hep_data%dtime = dt_hep

        ! Allocate output arrays
        allocate(hep_data%lat(dlat_hep))
        allocate(hep_data%lon(dlon_hep))
        
        allocate(hep_data%matrix(dlon_hep, dlat_hep, npops_basic, dt_hep))
        allocate(hep_wk(dlon_hep, dlat_hep, dt_hep))

        ! Read Lat/Lon from the first file
        print *, "DEBUG: Calling nf90_inq_varid for lat..."
        status_code = nf90_inq_varid(ncid, "lat", varid)
        print *, "DEBUG: nf90_inq_varid for lat returned: ", status_code
        call check(status_code)
        
        print *, "DEBUG: dlat_hep = ", dlat_hep
        print *, "DEBUG: Reading lat variable..."
        call check(nf90_get_var(ncid, varid, hep_data%lat))

        print *, "DEBUG: Calling nf90_inq_varid for lon..."
        status_code = nf90_inq_varid(ncid, "lon", varid)
        print *, "DEBUG: nf90_inq_varid for lon returned: ", status_code
        call check(status_code)
        
        print *, "DEBUG: dlon_hep = ", dlon_hep
        print *, "DEBUG: Reading lon variable..."
        status_code = nf90_get_var(ncid, varid, hep_data%lon)
        print *, "DEBUG: nf90_get_var for lon returned: ", status_code
        call check(status_code)
        
        print *, "DEBUG: Closing file... ncid = ", ncid
        status_code = nf90_close(ncid)
        print *, "DEBUG: nf90_close returned: ", status_code
        call check(status_code)

        do jp = 1, npops_basic
             call check(nf90_open(trim(paths(jp)), nf90_nowrite, ncid))
             
             call check(nf90_inq_varid(ncid, "AccHEP", varid))
             call check(nf90_get_var(ncid, varid, hep_wk))
             
             hep_data%matrix(:,:,jp,:) = hep_wk(:,:,:)
             
             call check(nf90_close(ncid))
        end do
        
        deallocate(hep_wk)

    end function read_hep_data

    function read_hep_data_old(paths) result(hep_data)
        implicit none
        type(path_container), intent(in) :: paths
        type(hep_data_type) :: hep_data
        
        integer :: ncid, varid, dimid
        integer :: dlon_hep, dlat_hep, dt_hep
        integer :: jp
        
        ! Temporary arrays
        real, allocatable :: hep_wk(:,:,:)

        ! Open the file
        call check(nf90_open(trim(paths%hep_paths(1)), nf90_nowrite, ncid))

        ! Get dimensions
        call check(nf90_inq_dimid(ncid, "lon", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dlon_hep))

        call check(nf90_inq_dimid(ncid, "lat", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dlat_hep))

        call check(nf90_inq_dimid(ncid, "time", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dt_hep))

        ! Allocate output arrays
        allocate(hep_data%lat(dlat_hep))
        allocate(hep_data%lon(dlon_hep))

        
        allocate(hep_data%matrix(dlon_hep, dlat_hep, npops_basic, dt_hep))
        allocate(hep_wk(dlon_hep, dlat_hep, dt_hep))

        ! Read Lat/Lon from the first file (assuming all are same grid)
        call check(nf90_inq_varid(ncid, "lat", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lat))

        call check(nf90_inq_varid(ncid, "lon", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lon))
        
        ! Close the first file (opened above) to restart loop cleanly or just use it for first pop
        ! Actually, let's loop properly.
        call check(nf90_close(ncid))

        do jp = 1, npops_basic
            ! Construct path? Or assume paths are in the container.
            ! If container has `hep_paths(:)`, we use that.
            ! Let's define path_container with an array.
             call check(nf90_open(trim(paths%hep_paths(jp)), nf90_nowrite, ncid))
             
             call check(nf90_inq_varid(ncid, "AccHEP", varid))
             call check(nf90_get_var(ncid, varid, hep_wk))
             
             hep_data%matrix(:,:,jp,:) = hep_wk(:,:,:)
             
             call check(nf90_close(ncid))
        end do
        
        deallocate(hep_wk)

    end function read_hep_data_old

    subroutine check(status)
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
            print *, "Check function: NetCDF Error: ", trim(nf90_strerror(status))
            stop
        end if
    end subroutine check

    subroutine read_inputs(config, hep_data)
        implicit none
        type(world_config), intent(out) :: config
        type(hep_data_type), intent(out) :: hep_data

        ! 1. Read config from basic config
        config = read_world_config()

        ! 2. Read HEP data
        hep_data = read_hep_data(config%hep_paths)

        ! 3. Validate and Update
        ! Check npops (Critical)
        if (config%npops /= hep_data%npops) then
            print *, "CRITICAL WARNING: npops mismatch between config (", config%npops, ") and HEP data (", hep_data%npops, ")"
            ! The user requested a CRITICAL WARNING. Usually this implies stopping, but I will just print it loudly as requested.
            print *, "CRITICAL WARNING: This is likely a fatal error."
        end if

        ! Update dimensions if they differ (Prefer HEP)
        config%dlon_hep = hep_data%dlon
        config%dlat_hep = hep_data%dlat

        ! Calculate grid parameters from HEP data
        if (allocated(hep_data%lat) .and. allocated(hep_data%lon)) then
            if (size(hep_data%lat) > 1 .and. size(hep_data%lon) > 1) then
                config%lat_0 = hep_data%lat(1)
                config%lon_0 = hep_data%lon(1)
                
                ! Assuming uniform grid
                config%delta_lat = hep_data%lat(2) - hep_data%lat(1)
                config%delta_lon = hep_data%lon(2) - hep_data%lon(1)
            else
                print *, "Warning: HEP lat/lon arrays too small to calculate deltas."
            end if
        else
            print *, "Warning: HEP lat/lon arrays not allocated."
        end if
        
        print *, "HEP Data Dimensions: ", hep_data%dlon, "x", hep_data%dlat, "x", hep_data%dtime
        print *, "Grid Params: lat0=", config%lat_0, " lon0=", config%lon_0, &
                 " dlat=", config%delta_lat, " dlon=", config%delta_lon

    end subroutine read_inputs

end module mod_read_inputs