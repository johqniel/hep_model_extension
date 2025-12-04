module mod_hep

    use netcdf

    use mod_paths_filenames
        

    implicit none

    ! ==============================================================================
    ! CONFIGURATION SECTION
    ! Define all paths, filenames, and NetCDF variable names here.
    ! ==============================================================================
    
    ! Data Types
    integer, parameter :: dp = kind(1.0d0)





contains

    !> Subroutine to read HEP files.
    !> Dimensions and npops are determined dynamically from the hep_files above.
    subroutine load_hep_files(hep, lon_arr, lat_arr)
        implicit none
        ! Input Variables

        ! Output Variables
        real(kind=dp), allocatable, intent(out) :: hep(:,:,:,:) ! (lon, lat, pop, time)
        real(kind=dp), allocatable, intent(out) :: lon_arr(:)
        real(kind=dp), allocatable, intent(out) :: lat_arr(:)

        ! Local Variables
        integer :: ncid, varid, dimid
        integer :: nlon, nlat, ntime, npops
        integer :: i
        character(len=256) :: full_path
        
        ! Temporary buffer
        real(kind=dp), allocatable :: hep_temp(:,:,:) 

        ! ---------------------------------------------------------
        ! 0. Determine npops from the array size
        ! ---------------------------------------------------------
        npops = size(hep_files)

        print *, "--------------------------------------------------"
        print *, "Initializing HEP Data Loader"
        print *, "Detected Populations:", npops
        print *, "Input Directory:     ", dir_input
        print *, "Files to load:       ", hep_files

        ! ---------------------------------------------------------
        ! 1. Geometry Setup (Read dimensions from the first file)
        ! ---------------------------------------------------------
        full_path = dir_input // trim(hep_files(1))
        
        call check( nf90_open(trim(full_path), NF90_NOWRITE, ncid) )

        ! Query Dimensions
        call check( nf90_inq_dimid(ncid, nc_dim_lon, dimid) )
        call check( nf90_inquire_dimension(ncid, dimid, len=nlon) )

        call check( nf90_inq_dimid(ncid, nc_dim_lat, dimid) )
        call check( nf90_inquire_dimension(ncid, dimid, len=nlat) )

        call check( nf90_inq_dimid(ncid, nc_dim_time, dimid) )
        call check( nf90_inquire_dimension(ncid, dimid, len=ntime) )

        ! Allocate Output Arrays
        ! Note: The 3rd dimension is now 'npops', derived from hep_files size
        allocate(hep(nlon, nlat, npops, ntime))
        allocate(lon_arr(nlon))
        allocate(lat_arr(nlat))
        allocate(hep_temp(nlon, nlat, ntime))

        ! Read Coordinates
        call check( nf90_inq_varid(ncid, nc_var_lon, varid) )
        call check( nf90_get_var(ncid, varid, lon_arr) )

        call check( nf90_inq_varid(ncid, nc_var_lat, varid) )
        call check( nf90_get_var(ncid, varid, lat_arr) )

        call check( nf90_close(ncid) )

        print *, "Grid Setup: ", nlon, "x", nlat, " with", ntime, "timesteps."

        ! ---------------------------------------------------------
        ! 2. Data Loading Loop
        ! ---------------------------------------------------------
        do i = 1, npops

            full_path = dir_input // trim(hep_files(i))
            print *, "Loading Pop", i, ": ", trim(full_path)

            call check( nf90_open(trim(full_path), NF90_NOWRITE, ncid) )

            ! Get Variable ID
            call check( nf90_inq_varid(ncid, nc_var_data, varid) )

            ! Read Data
            call check( nf90_get_var(ncid, varid, hep_temp) )

            ! Store in main array
            hep(:,:,i,:) = hep_temp(:,:,:)

            call check( nf90_close(ncid) )

        end do

        deallocate(hep_temp)
        print *, "Initialization Complete."
        print *, "--------------------------------------------------"

    end subroutine load_hep_files

    !> Error Handler
    subroutine check(status)
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
        print *, "NetCDF Error: ", trim(nf90_strerror(status))
        stop
        end if
    end subroutine check

  ! --------------------------------------------------------------------------
  ! 0. Preprocess HEP Data (Cleaning)
  !    Purpose: Clean raw data before simulation starts.
  !    1. Water Masking: Sets negative values or NaNs to 'water_hep' (e.g., -10)
  !    2. Stability: Sets extremely small positive values to 0.01 to prevent
  !       math errors (like division by zero or log(0)).
  ! --------------------------------------------------------------------------
  subroutine preprocess_hep(hep, water_hep)
    implicit none
    real(kind=dp), dimension(:,:,:,:), intent(inout) :: hep ! 4D Input (Lon, Lat, Pop, Time)
    real(kind=dp),                     intent(in)    :: water_hep

    ! 1. Handle Water and NaNs (Not-a-Number)
    !    (hep /= hep) is a standard Fortran trick to detect NaNs
    where ( (hep < 0.0_dp) .or. (hep /= hep) )
       hep = water_hep
    end where

    ! 2. Handle instability for very small positive values
    !    Prevents "black holes" where survival is technically possible but math breaks
    where ( (hep > 0.0_dp) .and. (hep < 0.01_dp) )
       hep = 0.01_dp
    end where

  end subroutine preprocess_hep

  ! --------------------------------------------------------------------------
  ! 1. Calculate Local Carrying Capacity (rho_c)
  !    Formula: rho_c = N_max * HEP
  ! --------------------------------------------------------------------------
  subroutine get_carrying_capacity(hep, N_max, rho_c)
    implicit none
    real(kind=dp), dimension(:,:), intent(in)  :: hep    ! Input HEP Grid
    real(kind=dp),                 intent(in)  :: N_max  ! Max Cultural Capacity
    real(kind=dp), dimension(:,:), intent(out) :: rho_c  ! Output Capacity Grid

    ! Simple element-wise multiplication
    rho_c = hep * N_max
    
  end subroutine get_carrying_capacity


  ! --------------------------------------------------------------------------
  ! 2. Calculate Population Pressure using Weibull Distribution
  !    Inputs: 
  !      rho   = Current Human Density
  !      rho_c = Local Carrying Capacity
  !      hep   = Used to identify water (hep < 0)
  !      eta, epsilon = Weibull shape parameters
  ! --------------------------------------------------------------------------
  subroutine get_pop_pressure(rho, rho_c, hep, eta, epsilon, pop_pressure)
    implicit none
    real(kind=dp), dimension(:,:), intent(in)  :: rho, rho_c, hep
    real(kind=dp),                 intent(in)  :: eta, epsilon
    real(kind=dp), dimension(:,:), intent(out) :: pop_pressure

    ! Local variables
    real(kind=dp) :: max_pp
    real(kind=dp), dimension(size(rho,1), size(rho,2)) :: delta_rho ! Ratio rho/rho_c
    real(kind=dp) :: term_a, term_b
    
    ! 1. Calculate the normalization factor (max peak of Weibull)
    !    This constant ensures the max pressure is scaled correctly.
    !    Formula: (eta/eps) * (1 - 1/eta)^(1 - 1/eta) * exp(-(1 - 1/eta))
    term_a = 1.0_dp - (1.0_dp / eta)
    max_pp = (eta / epsilon) * (term_a**term_a) * exp(-term_a)

    ! 2. Calculate Density Ratio (delta_rho)
    !    Avoid division by zero if rho_c is 0 (though masked later, good for safety)
    where (rho_c > 1.0e-10_dp)
       delta_rho = rho / rho_c
    elsewhere
       delta_rho = 0.0_dp
    end where

    ! 3. Calculate Pressure
    !    Formula: (eta/eps) * (delta/eps)^(eta-1) * exp(-(delta/eps)^eta) / max_pp
    pop_pressure = (eta / epsilon) * &
                   ((delta_rho / epsilon)**(eta - 1.0_dp)) * &
                   exp( -((delta_rho / epsilon)**eta) ) / &
                   max_pp

    ! 4. Boundary Conditions (Water Mask)
    !    If HEP is water (<=0), pressure is set to 1.0 (neutral/baseline)
    where (hep <= 0.0_dp)
       pop_pressure = 1.0_dp
    end where

  end subroutine get_pop_pressure


  ! --------------------------------------------------------------------------
  ! 3. Calculate Available HEP (hep_av)
  !    Formula: hep_av = hep * pop_pressure
  ! --------------------------------------------------------------------------
  subroutine get_hep_av(hep, pop_pressure, hep_av)
    implicit none
    real(kind=dp), dimension(:,:), intent(in)  :: hep
    real(kind=dp), dimension(:,:), intent(in)  :: pop_pressure
    real(kind=dp), dimension(:,:), intent(out) :: hep_av

    ! The "real" attractiveness of a cell is the environment (hep)
    ! scaled by how crowded it is (pop_pressure).
    hep_av = hep * pop_pressure
    
    ! Ensure we don't accidentally make water attractive
    where (hep <= 0.0_dp)
        hep_av = hep ! Keep it as -10 or whatever water value is
    end where

  end subroutine get_hep_av


end module mod_hep