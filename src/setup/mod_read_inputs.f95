module mod_read_inputs

    use mod_config
    use mod_basic_config, only: npops_basic => npops, ns_basic => ns, &
        tyr_start, tstep_start, tyr_end, tyr_length, dt, Tn, save_t, delta_t_hep, &
        hum_max_initial, tau, cb1, cb2, cb3, eta, epsilon, rho_max, r_B, d_B, &
        dt_bdyr, dt_bd, with_pop_pressure, with_birth_n_death, eps, minpts, &
        lon_min_out, lon_max_out, lat_min_out, lat_max_out, water_hep, &
        probability_vertilisation_per_tick, age_when_vertile_m, age_when_vertile_f, &
        age_until_vertile_m, age_until_vertile_f, pregnancy_minimum_length, &
        birth_prob_after_min_length, hum_0, x_ini_c, y_ini_c, ini_spread, sigma_u
    use netcdf

    implicit none
    
    type :: path_container
        character(len=256), allocatable :: hep_paths(:)
    end type path_container

    type :: hep_data_type
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
        allocate(config%hum_max_initial(npops_basic))
        allocate(config%tau(npops_basic))
        allocate(config%cb1(npops_basic))
        allocate(config%cb2(npops_basic))
        allocate(config%cb3(npops_basic))
        allocate(config%eta(npops_basic))
        allocate(config%epsilon(npops_basic))
        allocate(config%rho_max(npops_basic))
        allocate(config%r_B(npops_basic))
        allocate(config%d_B(npops_basic))

        config%tyr_start = tyr_start
        config%tstep_start = tstep_start
        config%tyr_end = tyr_end
        config%tyr_length = tyr_length
        config%dt = dt
        config%Tn = Tn
        config%save_t = save_t
        config%delta_t_hep = delta_t_hep
        config%hum_max_initial = hum_max_initial
        config%tau = tau
        config%cb1 = cb1
        config%cb2 = cb2
        config%cb3 = cb3
        config%eta = eta
        config%epsilon = epsilon
        config%rho_max = rho_max
        config%r_B = r_B
        config%d_B = d_B
        config%dt_bdyr = dt_bdyr
        config%dt_bd = dt_bd
        config%with_pop_pressure = with_pop_pressure
        config%with_birth_n_death = with_birth_n_death
        config%eps = eps
        config%minpts = minpts
        config%lon_min_out = lon_min_out
        config%lon_max_out = lon_max_out
        config%lat_min_out = lat_min_out
        config%lat_max_out = lat_max_out
        config%water_hep = water_hep
        config%probability_vertilisation_per_tick = probability_vertilisation_per_tick
        config%age_when_vertile_m = age_when_vertile_m
        config%age_when_vertile_f = age_when_vertile_f
        config%age_until_vertile_m = age_until_vertile_m
        config%age_until_vertile_f = age_until_vertile_f
        config%pregnancy_minimum_length = pregnancy_minimum_length
        config%birth_prob_after_min_length = birth_prob_after_min_length

    end function read_world_config

    function read_hep_data(paths) result(hep_data)
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

    end function read_hep_data

    subroutine check(status)
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
            print *, "NetCDF Error: ", trim(nf90_strerror(status))
            stop
        end if
    end subroutine check

end module mod_read_inputs