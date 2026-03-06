module mod_paths_filenames

    implicit none 



    ! ------------------------------------
    ! File Paths - relative to executable
    ! ------------------------------------


        character(len=*), parameter :: hep_input = "input/hep/"


    ! ------------------------------------
    ! File Names 
    ! ------------------------------------

        ! hep input files
        character(len=*), parameter, dimension(3) :: hep_files = &
            (/ "hep_aur.nc", &
               "hep_nea.nc",&
               "hep_mix.nc" /)



    ! ------------------------------------
    ! Variable Names 
    ! ------------------------------------

        ! Hep NetCDF Files:

        ! hep NetCDF Internal Variable Names
        character(len=*), parameter :: nc_var_data = "hep"   ! The main data variable
        character(len=*), parameter :: nc_var_lon  = "lon"   ! Longitude variable
        character(len=*), parameter :: nc_var_lat  = "lat"   ! Latitude variable

        ! hep NetCDF Dimension Names
        character(len=*), parameter :: nc_dim_lon  = "lon"
        character(len=*), parameter :: nc_dim_lat  = "lat"
        character(len=*), parameter :: nc_dim_time = "time"


end module mod_paths_filenames