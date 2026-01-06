module mod_read_inputs

    use mod_config
    use mod_config
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
        integer, allocatable :: watermask(:,:) ! (lon, lat)
    end type hep_data_type

    character(len=256), save :: current_config_path = "input/config/basic_config.nml"
    
    ! HEP Path Storage (Must be set programmatically)
    character(len=256), allocatable, save :: stored_hep_paths(:)

    contains

    subroutine set_config_path(path)
        implicit none
        character(len=*), intent(in) :: path
        current_config_path = path
    end subroutine set_config_path

    subroutine set_hep_paths(paths)
        implicit none
        character(len=*), dimension(:), intent(in) :: paths
        integer :: n
        
        n = size(paths)
        if (allocated(stored_hep_paths)) deallocate(stored_hep_paths)
        allocate(stored_hep_paths(n))
        stored_hep_paths = paths
    end subroutine set_hep_paths

    subroutine read_config(cfg, filename)
        implicit none
        type(world_config), intent(out) :: cfg
        character(len=*), intent(in) :: filename
        
        integer :: unit, iostat
        integer :: npops, ns
        
        ! Local variables for namelist
        real(8), allocatable :: tyr_start(:), tyr_end(:), tyr_length(:)
        integer, allocatable :: tstep_start(:)
        real(8) :: dt
        integer :: Tn, save_t, delta_t_hep
        real(8), allocatable :: tau(:), sigma_u(:), eta(:), epsilon(:)
        real(8), allocatable :: rho_max(:), r_B(:), d_B(:), cb1(:)
        real(8) :: dt_bdyr, eps
        integer :: dt_bd, minpts
        logical :: with_pop_pressure
        real(8), allocatable :: x_ini_c(:,:), y_ini_c(:,:), ini_spread(:,:)
        integer, allocatable :: hum_0(:,:)
        real(8) :: water_hep
        real(8) :: probability_vertilisation_per_tick
        integer :: age_when_fertile_m, age_when_fertile_f
        integer :: age_until_fertile_m, age_until_fertile_f
        integer :: pregnancy_minimum_length
        real :: birth_prob_after_min_length
        ! hep_paths removed from namelist

        namelist /dims/ npops, ns
        
        namelist /config/ &
            tyr_start, tstep_start, tyr_end, tyr_length, &
            dt, Tn, save_t, delta_t_hep, &
            tau, sigma_u, &
            eta, epsilon, rho_max, r_B, d_B, cb1, &
            dt_bdyr, dt_bd, &
            eps, minpts, &
            with_pop_pressure, &
            x_ini_c, y_ini_c, ini_spread, hum_0, &
            water_hep, &
            probability_vertilisation_per_tick, &
            age_when_fertile_m, age_when_fertile_f, &
            age_until_fertile_m, age_until_fertile_f, &
            pregnancy_minimum_length, birth_prob_after_min_length

        ! Open file
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening config file: ", trim(filename)
            stop
        end if
        
        ! Read dimensions
        read(unit, nml=dims, iostat=iostat)
        if (iostat /= 0) then
            print *, "Error reading dims from config file."
            stop
        end if
        
        cfg%npops = npops
        cfg%ns = ns
        
        ! Allocate local arrays
        allocate(tyr_start(npops), tstep_start(npops), tyr_end(npops), tyr_length(npops))
        allocate(tau(npops), sigma_u(npops), eta(npops), epsilon(npops))
        allocate(rho_max(npops), r_B(npops), d_B(npops), cb1(npops))
        allocate(hum_0(ns, npops), x_ini_c(ns, npops), y_ini_c(ns, npops), ini_spread(ns, npops))
        
        ! Allocate config arrays
        allocate(cfg%tyr_start(npops), cfg%tstep_start(npops), cfg%tyr_end(npops), cfg%tyr_length(npops))
        allocate(cfg%tau(npops), cfg%sigma_u(npops), cfg%eta(npops), cfg%epsilon(npops))
        allocate(cfg%rho_max(npops), cfg%r_B(npops), cfg%d_B(npops), cfg%cb1(npops))
        allocate(cfg%hum_0(ns, npops), cfg%x_ini_c(ns, npops), cfg%y_ini_c(ns, npops), cfg%ini_spread(ns, npops))
        allocate(cfg%hep_paths(npops))

        ! Read config
        read(unit, nml=config, iostat=iostat)
        if (iostat /= 0) then
            print *, "Error reading config from config file."
            stop
        end if
        
        close(unit)
        
        ! Assign to config
        cfg%tyr_start = tyr_start
        cfg%tstep_start = tstep_start
        cfg%tyr_end = tyr_end
        cfg%tyr_length = tyr_length
        cfg%dt = dt
        cfg%Tn = Tn
        cfg%save_t = save_t
        cfg%delta_t_hep = delta_t_hep
        cfg%tau = tau
        cfg%sigma_u = sigma_u
        cfg%eta = eta
        cfg%epsilon = epsilon
        cfg%rho_max = rho_max
        cfg%r_B = r_B
        cfg%d_B = d_B
        cfg%cb1 = cb1
        cfg%dt_bdyr = dt_bdyr
        cfg%dt_bd = dt_bd
        cfg%eps = eps
        cfg%minpts = minpts
        cfg%with_pop_pressure = with_pop_pressure
        cfg%x_ini_c = x_ini_c
        cfg%y_ini_c = y_ini_c
        cfg%ini_spread = ini_spread
        cfg%hum_0 = hum_0
        cfg%water_hep = water_hep
        cfg%probability_vertilisation_per_tick = probability_vertilisation_per_tick
        cfg%age_when_fertile_m = age_when_fertile_m
        cfg%age_when_fertile_f = age_when_fertile_f
        cfg%age_until_fertile_m = age_until_fertile_m
        cfg%age_until_fertile_f = age_until_fertile_f
        cfg%pregnancy_minimum_length = pregnancy_minimum_length
        cfg%birth_prob_after_min_length = birth_prob_after_min_length
        
        ! Initialize technical parameters
        cfg%initial_hashmap_size = initial_hashmap_size
        cfg%initial_max_pop_size = initial_agent_array_size
        
        ! Assign HEP Paths from storage
        if (.not. allocated(stored_hep_paths)) then
            print *, "CRITICAL ERROR: HEP paths not set. Use set_hep_paths() before reading config."
            stop
        end if
        
        if (size(stored_hep_paths) == 1) then
            ! Broadcast single path to all populations
            cfg%hep_paths = stored_hep_paths(1)
        elseif (size(stored_hep_paths) == cfg%npops) then
            ! Use exact mapping
            cfg%hep_paths = stored_hep_paths
        else
            print *, "CRITICAL ERROR: Stored HEP paths count (", size(stored_hep_paths), &
                     ") does not match npops (", cfg%npops, ") or 1."
            stop
        end if
        
    end subroutine read_config

    function read_world_config() result(config)
        implicit none
        type(world_config) :: config
        ! Use the current config path
        call read_config(config, trim(current_config_path))
    end function read_world_config

    function read_hep_data(paths) result(hep_data)
        implicit none
        character(len=*), dimension(:), intent(in) :: paths
        type(hep_data_type) :: hep_data
        
        integer :: ncid, varid, dimid, status
        integer :: dlon_hep, dlat_hep, dt_hep
        integer :: jp, npops
        
        ! Temporary arrays
        real, allocatable :: hep_wk(:,:,:)

        npops = size(paths)

        ! Open the first file to get dimensions
        call check(nf90_open(trim(paths(1)), nf90_nowrite, ncid))

        ! Get dimensions
        call check(nf90_inq_dimid(ncid, "lon", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dlon_hep))

        call check(nf90_inq_dimid(ncid, "lat", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dlat_hep))

        call check(nf90_inq_dimid(ncid, "time", dimid))
        call check(nf90_inquire_dimension(ncid, dimid, len=dt_hep))

        ! Set metadata
        hep_data%npops = npops
        hep_data%dlon = dlon_hep
        hep_data%dlat = dlat_hep
        hep_data%dtime = dt_hep

        ! Allocate output arrays
        allocate(hep_data%lat(dlat_hep))
        allocate(hep_data%lon(dlon_hep))
        
        allocate(hep_data%matrix(dlon_hep, dlat_hep, npops, dt_hep))
        allocate(hep_wk(dlon_hep, dlat_hep, dt_hep))
        
        allocate(hep_data%watermask(dlon_hep, dlat_hep))

        ! Read Lat/Lon from the first file
        call check(nf90_inq_varid(ncid, "lat", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lat))

        call check(nf90_inq_varid(ncid, "lon", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lon))
        
        ! Try to read watermask
        status = nf90_inq_varid(ncid, "watermask", varid)
        if (status == nf90_noerr) then
            call check(nf90_get_var(ncid, varid, hep_data%watermask))
            
            ! Validate Watermask
            if (any(hep_data%watermask /= 0 .and. hep_data%watermask /= 1)) then
                print *, "CRITICAL WARNING: Watermask values must be 0 or 1."
                print *, "Found values outside {0, 1}."
            end if
        else
            print *, "WARNING: 'watermask' variable not found in HEP file. Assuming all land (1)."
            hep_data%watermask = 1
        end if
        
        call check(nf90_close(ncid))

        do jp = 1, npops
             call check(nf90_open(trim(paths(jp)), nf90_nowrite, ncid))
             
             call check(nf90_inq_varid(ncid, "AccHEP", varid))
             call check(nf90_get_var(ncid, varid, hep_wk))
             
             ! Check for values out of range
             if (any(hep_wk < 0.0 .or. hep_wk > 1.0)) then
                 print *, "CRITICAL WARNING: AccHEP values out of range [0, 1] in file: ", trim(paths(jp))
                 print *, "Min value: ", minval(hep_wk)
                 print *, "Max value: ", maxval(hep_wk)
             end if
             
             ! Apply Watermask
             ! If watermask == 0 (water), set HEP to -1
             where (spread(hep_data%watermask, 3, dt_hep) == 0)
                 hep_wk = -1.0
             end where
             
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
        integer :: jp, npops
        
        ! Temporary arrays
        real, allocatable :: hep_wk(:,:,:)

        npops = size(paths%hep_paths)

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

        
        allocate(hep_data%matrix(dlon_hep, dlat_hep, npops, dt_hep))
        allocate(hep_wk(dlon_hep, dlat_hep, dt_hep))

        ! Read Lat/Lon from the first file (assuming all are same grid)
        call check(nf90_inq_varid(ncid, "lat", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lat))

        call check(nf90_inq_varid(ncid, "lon", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lon))
        
        ! Close the first file (opened above) to restart loop cleanly or just use it for first pop
        ! Actually, let's loop properly.
        call check(nf90_close(ncid))

        do jp = 1, npops
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