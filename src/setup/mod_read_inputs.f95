module mod_read_inputs

    use mod_config
    use mod_config
    ! use mod_basic_config ! Removed dependency
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
        real, allocatable :: matrix(:,:,:,:) ! (lon, lat, pop, slices_per_chunk)
        real, allocatable :: lat(:)
        real, allocatable :: lon(:)
        real, allocatable :: time(:)
        integer, allocatable :: watermask(:,:) ! (lon, lat)
        
        ! Dynamic chunking metadata
        integer :: slices_per_chunk = 1
        integer :: chunk_start_t = 0
        integer :: chunk_end_t = 0
        character(len=256), allocatable :: paths(:)
    end type hep_data_type

    character(len=256), save :: current_config_path = "input/config/basic_config.nml"
    
    ! HEP Path Storage (Restored for Python Interface)
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
        integer :: npops, ns, jp
        
        ! Local variables for namelist
        real(8), allocatable :: tyr_start(:), tyr_end(:), tyr_length(:)
        integer, allocatable :: tstep_start(:), tstep_end(:)
        real(8) :: dt
        integer :: Tn, delta_t_hep
        logical :: use_active_time_phases
        real(8), allocatable :: sigma_u(:), eta(:), epsilon(:)
        real(8), allocatable :: rho_max(:)
        logical :: with_pop_pressure
        real(8), allocatable :: x_ini_c(:,:), y_ini_c(:,:), ini_spread(:,:)
        integer, allocatable :: hum_0(:,:)


        ! Birth/Death - Clustered
        real(8), allocatable :: NC(:)
        ! Birth/Death/Preparation module parameters (mod_reviewed_modules)
        real(8) :: r, NC_Global, Kmin, Kmax
        ! Watershed clustering parameters
        integer :: human_density_smoothing_radius
        integer :: human_density_smoothing_iterations
        real(8) :: watershed_threshold
        integer :: cluster_update_interval
        integer :: c3_individual_update_interval
        integer :: c3_cluster_update_interval
        real(8) :: cb1, cb2, cb3
        ! C3 Creativity module parameters
        real(8) :: c3_Pmax1, c3_Alpha1, c3_Phi_l1, c3_tau1
        real(8) :: c3_Pmax2, c3_k2, c3_Phi_l2, c3_tau2
        real(8) :: c3_l, c3_R, c3_min_creativity, c3_max_creativity
        integer :: c3_search_r_cap
        integer :: max_high_creativity_fast
        character(len=256), allocatable :: hep_paths(:)
        logical :: allow_across_populations
        logical :: efficient_density_updates
        
        namelist /dims/ npops, ns
        
        namelist /config/ &
            tyr_start, tyr_end, &
            dt, use_active_time_phases, &
            sigma_u, &
            eta, epsilon, rho_max, &
            with_pop_pressure, &
            x_ini_c, y_ini_c, ini_spread, hum_0, &
            ! Clustering settings
            human_density_smoothing_radius, human_density_smoothing_iterations, &
            watershed_threshold, &
            cluster_update_interval, &
            c3_individual_update_interval, &
            c3_cluster_update_interval, &
            ! reviewed agent motion parameters
            cb1, cb2, cb3, &
            ! C3 Creativity module
            c3_Pmax1, c3_Alpha1, c3_Phi_l1, c3_tau1, &
            c3_Pmax2, c3_k2, c3_Phi_l2, c3_tau2, &
            c3_l, c3_R, c3_search_r_cap, &
            c3_min_creativity, c3_max_creativity, &
            max_high_creativity_fast, &
            ! Birth/Death - Clustered
            NC, &
            ! Birth/Death/Preparation module parameters (mod_reviewed_modules)
            r, NC_Global, Kmin, Kmax, &
            ! HEP Paths
            hep_paths, &
            ! Mating control
            allow_across_populations, &
            ! Efficient density updates
            efficient_density_updates

        ! Open file
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error opening config file: ", trim(filename)
            stop
        end if
        
        ! Read dimensions
        npops = 3
        ns = 1
        read(unit, nml=dims, iostat=iostat)
        if (iostat /= 0) then
            print *, "Error reading dims from config file."
            stop
        end if
        
        cfg%npops = npops
        cfg%ns = ns
        
        ! Allocate local arrays
        allocate(tyr_start(npops), tstep_start(npops), tstep_end(npops), tyr_end(npops), tyr_length(npops))
        allocate(sigma_u(npops), eta(npops), epsilon(npops))
        allocate(rho_max(npops), NC(npops))
        allocate(hum_0(ns, npops), x_ini_c(ns, npops), y_ini_c(ns, npops), ini_spread(ns, npops))
        hum_0 = 0
        x_ini_c = 0.0d0
        y_ini_c = 0.0d0
        ini_spread = 0.0d0
        use_active_time_phases = .false.
        ! Defaults for clustering settings
        human_density_smoothing_radius = 2
        human_density_smoothing_iterations = 1
        watershed_threshold = 0.05d0
        cluster_update_interval = 100
        c3_individual_update_interval = 100
        c3_cluster_update_interval = 100
        NC = 25.0d0
        cb1 = 500.0d0
        cb2 = 0.2d0
        cb3 = 20.0d0
        ! C3 Creativity defaults
        c3_Pmax1 = 0.1d0
        c3_Alpha1 = 2.0d0
        c3_Phi_l1 = 0.6d0
        c3_tau1 = 10.0d0
        c3_Pmax2 = 0.1d0
        c3_k2 = 3.0d0
        c3_Phi_l2 = 3.5d0
        c3_tau2 = 50.0d0
        c3_l = 0.00005d0
        c3_R = 300.0d0
        c3_search_r_cap = 5
        c3_min_creativity = 0.1d0
        c3_max_creativity = 10.0d0
        max_high_creativity_fast = 5
        allow_across_populations = .true.
        efficient_density_updates = .false.
        allocate(hep_paths(npops))
        
        ! Allocate config arrays
        allocate(cfg%tyr_start(npops), cfg%tstep_start(npops), cfg%tstep_end(npops), cfg%tyr_end(npops), cfg%tyr_length(npops))
        allocate(cfg%sigma_u(npops), cfg%eta(npops), cfg%epsilon(npops))
        allocate(cfg%rho_max(npops), cfg%NC(npops))
        allocate(cfg%hum_0(ns, npops), cfg%x_ini_c(ns, npops), cfg%y_ini_c(ns, npops), cfg%ini_spread(ns, npops))
        allocate(cfg%hep_paths(npops))

        ! Read config
        read(unit, nml=config, iostat=iostat)
        if (iostat /= 0) then
            print *, "Error reading config from config file."
            stop
        end if
        
        close(unit)
        
        ! Compute derived variables
        tyr_length = tyr_end - tyr_start
        Tn = nint((maxval(tyr_end) - minval(tyr_start)) / dt)
        if (use_active_time_phases) then
            do jp = 1, npops
                tstep_start(jp) = nint((tyr_start(jp) - minval(tyr_start)) / dt) + 1
                tstep_end(jp) = nint((tyr_end(jp) - minval(tyr_start)) / dt) + 1
            end do
        else
            tstep_start = 1
            tstep_end = Tn
        end if

        ! Assign to config
        cfg%tyr_start = tyr_start
        cfg%tstep_start = tstep_start
        cfg%tstep_end = tstep_end
        cfg%tyr_end = tyr_end
        cfg%tyr_length = tyr_length
        cfg%dt = dt
        cfg%sqdt = sqrt(dt)
        cfg%Tn = Tn
        cfg%use_active_time_phases = use_active_time_phases
        cfg%sigma_u = sigma_u
        cfg%eta = eta
        cfg%epsilon = epsilon
        cfg%rho_max = rho_max
        cfg%with_pop_pressure = with_pop_pressure
        cfg%x_ini_c = x_ini_c
        cfg%y_ini_c = y_ini_c
        cfg%ini_spread = ini_spread
        cfg%hum_0 = hum_0




        ! Mod: Birth/Death - Clustered
        cfg%NC = NC

        ! Mod: Birth/Death/Preparation (mod_reviewed_modules)
        cfg%r = r; cfg%NC_Global = NC_Global; cfg%Kmin = Kmin; cfg%Kmax = Kmax


        ! Clustering settings
        cfg%human_density_smoothing_radius = human_density_smoothing_radius
        cfg%human_density_smoothing_iterations = human_density_smoothing_iterations
        cfg%watershed_threshold = watershed_threshold
        cfg%cluster_update_interval = cluster_update_interval
        cfg%c3_individual_update_interval = c3_individual_update_interval
        cfg%c3_cluster_update_interval = c3_cluster_update_interval
        cfg%cb1 = cb1
        cfg%cb2 = cb2
        cfg%cb3 = cb3
        ! C3 Creativity
        cfg%c3_Pmax1  = c3_Pmax1
        cfg%c3_Alpha1 = c3_Alpha1
        cfg%c3_Phi_l1 = c3_Phi_l1
        cfg%c3_tau1   = c3_tau1
        cfg%c3_Pmax2  = c3_Pmax2
        cfg%c3_k2     = c3_k2
        cfg%c3_Phi_l2 = c3_Phi_l2
        cfg%c3_tau2   = c3_tau2
        cfg%c3_l      = c3_l
        cfg%c3_R      = c3_R
        cfg%c3_search_r_cap    = c3_search_r_cap
        cfg%c3_min_creativity  = c3_min_creativity
        cfg%c3_max_creativity  = c3_max_creativity
        cfg%max_high_creativity_fast = max_high_creativity_fast
        
        ! Initialize technical parameters
        cfg%initial_hashmap_size = initial_hashmap_size
        cfg%initial_max_pop_size = initial_agent_array_size
        
        ! Assign HEP Paths
        if (allocated(stored_hep_paths)) then
            ! Override with stored paths (from Python Interface)
            if (size(stored_hep_paths) == 1) then
                cfg%hep_paths = stored_hep_paths(1)
            elseif (size(stored_hep_paths) == cfg%npops) then
                cfg%hep_paths = stored_hep_paths
            else
                print *, "WARNING: Stored HEP paths count mismatch. Using config paths."
                cfg%hep_paths = hep_paths
            end if
        else
            cfg%hep_paths = hep_paths
        end if
        
        cfg%allow_across_populations = allow_across_populations
        cfg%efficient_density_updates = efficient_density_updates
        
    end subroutine read_config

    function read_world_config() result(config)
        implicit none
        type(world_config) :: config
        ! Use the current config path
        call read_config(config, trim(current_config_path))
    end function read_world_config

    function read_hep_data(paths, read_matrix) result(hep_data)
        implicit none
        character(len=*), dimension(:), intent(in) :: paths
        logical, intent(in), optional :: read_matrix
        type(hep_data_type) :: hep_data
        
        integer :: ncid, varid, dimid, status
        integer :: dlon_hep, dlat_hep, dt_hep
        integer :: jp, npops
        logical :: do_read_matrix
        
        ! Dynamic chunking variables
        integer :: slices_per_chunk
        integer(kind=8) :: file_sz
        integer, dimension(3) :: start, count
        
        ! Temporary arrays
        real, allocatable :: hep_wk(:,:,:)

        do_read_matrix = .true.
        if (present(read_matrix)) do_read_matrix = read_matrix

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

        ! Allocate output arrays (Metadata)
        allocate(hep_data%lat(dlat_hep))
        allocate(hep_data%lon(dlon_hep))
        allocate(hep_data%time(dt_hep))
        allocate(hep_data%watermask(dlon_hep, dlat_hep))

        ! Read Lat/Lon/Time from the first file
        call check(nf90_inq_varid(ncid, "lat", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lat))

        call check(nf90_inq_varid(ncid, "lon", varid))
        call check(nf90_get_var(ncid, varid, hep_data%lon))

        call check(nf90_inq_varid(ncid, "time", varid))
        call check(nf90_get_var(ncid, varid, hep_data%time))
        
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

        if (do_read_matrix) then
            ! 1. Query size of paths(1)
            inquire(file=trim(paths(1)), size=file_sz)
            if (file_sz <= 0) then
                ! Fallback variable sizing ( AccHEP float size )
                file_sz = int(dlon_hep, 8) * int(dlat_hep, 8) * int(dt_hep, 8) * 4
            end if
            
            ! 2. Calculate slice and chunk sizes
            ! Float slice size in bytes = dlon_hep * dlat_hep * 4
            slices_per_chunk = max(1, nint(10.0d0 * 1024.0d0 * 1024.0d0 / dble(dlon_hep * dlat_hep * 4)))
            slices_per_chunk = min(dt_hep, slices_per_chunk)
            
            print *, "HEP Chunk Slicing: File Size =", file_sz / (1024*1024), "MB"
            print *, "HEP Chunk Slicing: Chunk size =", slices_per_chunk, "slices (approx 10 MB)"
            
            hep_data%slices_per_chunk = slices_per_chunk
            hep_data%chunk_start_t = 1
            hep_data%chunk_end_t = slices_per_chunk
            
            allocate(hep_data%paths(npops))
            do jp = 1, npops
                hep_data%paths(jp) = trim(paths(jp))
            end do
            
            allocate(hep_data%matrix(dlon_hep, dlat_hep, npops, slices_per_chunk))
            allocate(hep_wk(dlon_hep, dlat_hep, slices_per_chunk))

            do jp = 1, npops
                 call check(nf90_open(trim(paths(jp)), nf90_nowrite, ncid))
                 
                 call check(nf90_inq_varid(ncid, "AccHEP", varid))
                 
                 start = [1, 1, 1]
                 count = [dlon_hep, dlat_hep, slices_per_chunk]
                 call check(nf90_get_var(ncid, varid, hep_wk, start=start, count=count))
                 
                 ! Check for values out of range
                 if (any(hep_wk < 0.0 .or. hep_wk > 1.0)) then
                     print *, "INFO: AccHEP values out of range [0, 1] (fill values detected) in file: ", trim(paths(jp))
                     print *, "Min value: ", minval(hep_wk), " Max value: ", maxval(hep_wk)
                     print *, "Replacing out-of-bounds/fill values with -1.0..."
                     where (hep_wk < 0.0 .or. hep_wk > 1.0)
                         hep_wk = -1.0
                     end where
                 end if
                 
                 ! Apply Watermask
                 ! If watermask == 0 (water), set HEP to -1
                 where (spread(hep_data%watermask, 3, slices_per_chunk) == 0)
                     hep_wk = -1.0
                 end where
                 
                 hep_data%matrix(:,:,jp,1:slices_per_chunk) = hep_wk(:,:,:)
                 
                 call check(nf90_close(ncid))
            end do
            
            deallocate(hep_wk)
        endif

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
              
             where (hep_wk < 0.0 .or. hep_wk > 1.0)
                 hep_wk = -1.0
             end where
             
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

    subroutine read_inputs(config, hep_data, read_matrix)
        implicit none
        type(world_config), intent(out) :: config
        type(hep_data_type), intent(out) :: hep_data
        logical, intent(in), optional :: read_matrix
        
        logical :: do_read_matrix
        
        do_read_matrix = .true.
        if (present(read_matrix)) do_read_matrix = read_matrix

        ! 1. Read config from basic config
        config = read_world_config()

        ! 2. Read HEP data
        hep_data = read_hep_data(config%hep_paths, do_read_matrix)

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
        
        ! Calculate delta_t_hep from time variable
        if (allocated(hep_data%time) .and. size(hep_data%time) > 1) then
            config%delta_t_hep = nint(abs(hep_data%time(2) - hep_data%time(1)) / config%dt)
        else
            config%delta_t_hep = 2000 ! fallback
        end if

        print *, "HEP Data Dimensions: ", hep_data%dlon, "x", hep_data%dlat, "x", hep_data%dtime
        print *, "Grid Params: lat0=", config%lat_0, " lon0=", config%lon_0, &
                 " dlat=", config%delta_lat, " dlon=", config%delta_lon
        print *, "Calculated delta_t_hep: ", config%delta_t_hep

    end subroutine read_inputs

    subroutine load_hep_chunk_from_file(self_grid, t_hep_target)
        use netcdf
        use mod_grid_id, only: Grid
        implicit none
        type(Grid), intent(inout) :: self_grid
        integer, intent(in) :: t_hep_target
        
        integer :: ncid, varid, jp
        integer :: chunk_start_t, chunk_end_t, count_slices
        integer, dimension(3) :: start, count
        real, allocatable :: temp_wk(:,:,:)
        integer :: i, j, k
        
        chunk_start_t = t_hep_target
        chunk_end_t = min(self_grid%nt, chunk_start_t + self_grid%slices_per_chunk - 1)
        count_slices = chunk_end_t - chunk_start_t + 1
        
        print *, "[HEP Slicing] Dynamic loading chunk starting at t =", chunk_start_t, "to", chunk_end_t
        
        self_grid%chunk_start_t = chunk_start_t
        self_grid%chunk_end_t = chunk_end_t
        
        allocate(temp_wk(self_grid%nx, self_grid%ny, count_slices))
        
        do jp = 1, self_grid%npops
             call check(nf90_open(trim(self_grid%config%hep_paths(jp)), nf90_nowrite, ncid))
             call check(nf90_inq_varid(ncid, "AccHEP", varid))
             
             start = [1, 1, chunk_start_t]
             count = [self_grid%nx, self_grid%ny, count_slices]
             call check(nf90_get_var(ncid, varid, temp_wk, start=start, count=count))
             
             ! Copy and cast to double precision, applying watermask
             do k = 1, count_slices
                 do j = 1, self_grid%ny
                     do i = 1, self_grid%nx
                         if (self_grid%cell(i,j)%is_water == 1) then
                             self_grid%hep(i,j,jp,k) = -1.0d0
                         else
                             if (temp_wk(i,j,k) < 0.0 .or. temp_wk(i,j,k) > 1.0) then
                                 self_grid%hep(i,j,jp,k) = -1.0d0
                             else
                                 self_grid%hep(i,j,jp,k) = dble(temp_wk(i,j,k))
                             end if
                         end if
                     end do
                 end do
             end do
             
             call check(nf90_close(ncid))
        end do
        
        deallocate(temp_wk)
        print *, "[HEP Slicing] Chunk load complete."
    end subroutine load_hep_chunk_from_file

end module mod_read_inputs