module mod_paths_and_strings

use mod_constants
use mod_parameters
use mod_common_variables

!++++++++++++++++++++++++++
! HEP input file
!++++++++++++++++++++++++++
    character( len = 300), dimension(npops) :: path_in
    data path_in /"/data/hescor/yshao/hes_model/hep/AUR.nc", &
!    data path_in /"/data/hescor/yshao/hes_model/hep/AUR_Pica.nc", &
   &              "/data/hescor/yshao/hes_model/hep/NEA.nc",     &
   &              "/data/hescor/yshao/hes_model/hep/MIX.nc"  /

    character (len = *), parameter :: name_in_dlon = "lon"
    character (len = *), parameter :: name_in_dlat = "lat"
    character (len = *), parameter :: name_in_lat  = "lat"
    character (len = *), parameter :: name_in_lon  = "lon"
    character (len = *), parameter :: name_in_hep  = "AccHEP"                                          ! KK used Acc_HEP, CW and GHL used AccHEP
    character (len = *), parameter :: name_in_dt = 'time'                                              ! KK used interp,  CW and GHL used time

!++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Output files
!++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer             :: iseed
    character (len = 4) :: random_seed
    ! Human position
    character (len = 300), dimension(npops) :: path_out_pos 
    ! Population density
    character (len = 300), dimension(npops) :: path_out_dens

    ! Names for human position file
    character (len = *), parameter :: name_out_dim_t      = "t"
    character (len = *), parameter :: name_out_dim_hum    = "hum"
    character (len = *), parameter :: name_out_dim_lonlat = "lonlat"

    character (len = *), parameter :: name_out_var_t       = "time"
    character (len = *), parameter :: name_out_var_pos_hum = "pos_hum"
    character (len = *), parameter :: name_out_var_velo    = "u"
    character (len = *), parameter :: name_out_var_hum_id  = "hum_id"

    character (len = *), parameter :: output_pos_hum          = "Position of humans [deg]"
    character (len = *), parameter :: output_velo             = "Velocity of humans [km/yr]"
    character (len = *), parameter :: output_t                = "Timesteps []"
    character (len = *), parameter :: output_hum_id           = "ID of humans to track paths"
    character (len = *), parameter :: output_name_description = "Description"
    character (len = *), parameter :: output_name_longname    = "longname"
    character (len = *), parameter :: output_description_pos  = "Position of humans in [km]"

    ! Names for population density file
    logical, parameter :: save_out = .true.

    ! Names for population density file

    character (len = *), parameter :: name_out_dim_lat = "lat"
    character (len = *), parameter :: name_out_dim_lon = "lon"

    character (len = *), parameter :: name_out_var_lat  = "lat"
    character (len = *), parameter :: name_out_var_lon  = "lon"
    character (len = *), parameter :: name_out_var_dens = "dens"
    character (len = *), parameter :: name_out_var_flow = "human_flow"

    character (len = *), parameter :: output_lon  = "Longitude"
    character (len = *), parameter :: output_lat  = "Latitude"
    character (len = *), parameter :: output_dens = "Population density [PDU]"
    character (len = *), parameter :: output_flow = "Population flow [No./100 km/yr]"
    character (len = *), parameter :: output_description_dens = "Population density in [PDU]"
    character (len = *), parameter :: output_description_flow = "Population flow in [No./100 km/yr]"



end module mod_paths_and_strings