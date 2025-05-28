module mod_setup
!--------------------------------------------------------------------
! Yaping Shao, 3 Jul 2024
! University of Cologne, Germany
!
! Konstantin Klein, early development for PhD
!
! Purpose: 
! (1) Setup configuration of N-Population Human System Model (NP-HSM)
! (2) Read in HEP (Human Existence Potential) data
! (3) Define output paths
!--------------------------------------------------------------------
    use netcdf
    use mod_utility
    implicit none
    save

    include "netcdf.inc"
    include "constant.inc"    ! constants like: pi, etc.
    include "common.inc"      ! variables like: num of populations num of humans 

!++++++++++++++++++++++++++++++++++++++++++++++++++
! Model parameters
!++++++++++++++++++++++++++++++++++++++++++++++++++
! Times 
!--------------------------------------------------
    real(8), dimension(npops) :: tyr_start                ! Start year of simulation
    data tyr_start /-43000, -50000, -43000/               ! Start year of simulation for different populations for AUR, NEA, MIX
    integer, dimension(npops) :: tstep_start              ! Start time step of simulation
    data tstep_start /700001, 1, 700001   /               ! AUR starts 7000 yrs later than NEA
!    data tstep_start /1200001, 1, 1200001  /             ! Do not simulat AUR and MIX, NEA only
    !data tstep_start /700001, 1200001, 1200001  /          ! AUR onl

    real(8), dimension(npops) :: tyr_end                  ! End year of simualtion
    data tyr_end   /-38000, -38000, -38000/     
    real(8), dimension(npops) :: tyr_length               ! Length of simulation               [yrs]
    data tyr_length /5000,   12000,  5000 / 
!
    real(8), parameter :: dt = 0.01                       ! Time step                          [yrs]
    integer, parameter :: Tn = 1200000                    ! Number of time steps of Simulation [   ], largest tyr_length/dt

    integer, parameter :: save_t = Tn / 1000              ! Interval in time steps, at which output is saved, the 1000 corresponds to 10 years

!------------------------------------------------------
! Human-population dependent parameters: motion related
!------------------------------------------------------
    integer, parameter :: delta_t_hep = 2000              ! HEP intervals, same for all pops, e.g., 20 yrs, delta_t_hep=20/0.01 =2000 steps            [ ]
!
    integer, dimension(npops) :: hum_max                  ! Maximum number of humans
    data hum_max     /16000,   5000,   1000  /            ! e.g., 8000 AMHs, 5000 NEAs, 1000 MIXs; ys: do not see why hum_max is needed

    real(8), dimension(npops) :: tau                      ! Lagrangian time scale for human random motion velocity                          [yrs]
                                                          ! data tau /6., 6., 6./             ! e.g., 6 for AMH, 6 for NEA, 6 for MIX
    real(8), dimension(npops) :: sigma_u                  ! Individual mobility (std of random motion velocity)                             [km/yr]
                                                          ! data sigma_u /15.,10.,10./, e.g., 15 for AMH, 10 for NEA, 10 for MIX
    real(8), dimension(npops) :: cb1                      ! cb1 = u_max*G_d/D_T, for example, u_max=5 km/yr, G_d=500 km, D_T = 2 yr, gives 1250 [km^2/yr^2] 
                                                          ! no need to specify three parameters, cb1 will be read in 
                                                          ! real(8), dimension(npops) :: D_T     ! Macroscopic drift time scale; ys: check   [yrs]
                                                          ! data D_T         /2.,     2.,     2.    /      ! e.g., 2 for AMH, 2 for NEA, 2 for MIX
                                                          ! real(8), dimension(npops) :: G_d               ! Scaling distance for HEP gradient [km]
                                                          ! data G_d /250.,250.,250./  ! e.g., 250 for AMH and 250 for NEA
                                                          ! real(8), dimension(npops) :: u_max             ! Scaling velocity for HEP gradient [km/yr]
                                                          ! data u_max /5., 4., 4.5 /  ! e.g., 5 for AMH and 4 for NEA
!-------------------------------------------------------------------
! Human-population dependent parameters: population dynamics related
!-------------------------------------------------------------------
    real(8), dimension(npops) :: eta                      ! Scaling parameter of Weibull distribution (population pressure curve)           [PDU]
    data eta         /1.6, 1.6, 1.6 /                     ! e.g., 1.6 for AMH, 1.6 for NEA, 1.6 for MIX
    real(8), dimension(npops) :: epsilon                  ! Shape parameter of Weibull distribution (population pressure curve)             [ ]
    data epsilon     /0.4, 0.4, 0.4 /                     ! e.g., 0.4 for AMH, 0.4 for NEA, 0.4 for MIX
    real(8), dimension(npops) :: rho_max                  ! Cultural carrying capacity (max. population density)                            [PDU]
                                                          ! data rho_max /5.,  1.,  2.5 /       ! e.g., 7 for AMH, 4 for NEA, 7 for MIX
    real(8), dimension(npops) :: r_B                      ! Population growth rate (net)                                                    [1/yr]
                                                          ! data r_B     /0.03, 0.01, 0.02/     ! e.g., 0.03 for AMH, 0.01 for NEA
    real(8), dimension(npops) :: d_B                      ! Natural death rate                                                              [1/yr]
                                                          ! data r_B     /0.02, 0.025, 0.0225/  ! e.g., 0.02 for AMH (1/50), 0.025 for NEA (1/40)
    real(8), parameter :: dt_bdyr = 1.                    ! Time interval for considering birth-death module                                [yr]
    integer, parameter :: dt_bd = 100                     ! As dt_bdyr, but in timesteps, dt_bdyr/dt =1/0.01= 100 timesteps                 [timesteps]

! Parameters of simple equation:
    logical, parameter :: with_pop_pressure = .false.     ! .true.

    logical, parameter :: with_birth_n_death = .true.     ! if false hum_max should be equal to hum_0

    real(8), parameter :: eps = 10.                       ! For clustering: Eucledian space neighborhood, defines the radius of core points [km]
    integer, parameter :: minpts = 3                      ! For clustering: minimum points in the eps-neighborhood to define a core point [humans]

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Initial population distributions
!-----------------------------------------------------------
     integer, parameter :: ns = 4                         ! Number of initial human sources. Set to 4 flexible in specification

     real(8), dimension(ns, npops) :: x_ini_c, y_ini_c, ini_spread
     integer, dimension(ns, npops) :: hum_0
     DATA x_ini_c    /4.0, 4.0,  4.0,  4.0, &              ! AMH   / x_ini_c(1, 1), x_ini_c(2,1), x_ini_c(3,1), x_ini_c(4,1)  & 
    &                -8.6,-3.93,-0.72,-5.54,&              ! NEA   & x_ini_c(1, 2), x_ini_c(2,2), x_ini_c(3,2), x_ini_c(4,2)  &
    &                 4.0, 4.0,  4.0,  4.0  /              ! MIX   & x_ini_c(1, 3), x_ini_c(2,3), x_ini_c(3,3), x_ini_c(4,3)  /

     DATA y_ini_c    /44.08,44.08,44.08,44.08, &           ! AMH
    &                 39.2, 42.3, 40.03,36.66, &           ! NEA
    &                 44.08,44.08,44.08,44.08  /           ! MIX

     DATA ini_spread /0.5,  0.5,  0.5,  0.5, &             ! AMH
    &                 1.0,  1.0,  1.0,  1.0, &             ! NEA
    &                 0.5,  0.5,  0.5,  0.5  /             ! MIX   

     DATA hum_0      /200,  200,  200,  200, &             ! AMH total 800 
    &                 150,  150,  150,  150, &             ! NEA total 600
    &                 0,    0,    0,    0    /             ! MIX total 0, initially no mixed population

!+++++++++++++++++++++++++
! Define domain boundaries 
!+++++++++++++++++++++++++
!    real(8), parameter  :: lon_min_out = -10., lon_max_out = 5., lat_min_out = 35, lat_max_out = 48     ! Outer boundary of research area [degree]
    real(8) :: lon_min_out, lon_max_out, lat_min_out, lat_max_out     ! Outer boundary of research area [degree]

!++++++++++++++++++++++++++++
! Defauls water grid cell HEP
!++++++++++++++++++++++++++++ 
    real(8),  parameter :: water_hep = -1.                                                              ! Chosen HEP for the water grids

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
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Declaration of variables for netcdf
!++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer                   :: lat_id, lon_id, hep_id
    integer, dimension(npops) :: nc_id_hep
    integer, dimension(npops) :: nc_id_out_pos, nc_id_out_dens
    integer, dimension(npops) :: dim_t_id_out, dim_lonlat_id_out, dim_hum_id_out, dim_lon_id_out, dim_lat_id_out
    integer, dimension(npops) :: var_pos_hum_id_out, var_velo_id_out, var_hum_id_id_out, var_dens_id_out, var_flow_id_out
    integer, dimension(npops) :: var_t_id_out, var_lon_id_out, var_lat_id_out
    integer, dimension(2, npops) :: dim_hum_id_id_out
    integer, dimension(3, npops) :: dim_pos_hum_id_out, dim_dens_id_out
    integer, dimension(4, npops) :: dim_flow_id_out
    integer                      :: dlon_hep, dlat_hep, dt_hep              ! YS, dimension of HEP, same for all npops
    integer                      :: dlon_id, dlat_id, dt_id

    real(8), dimension(:), allocatable :: lat_hep, lon_hep                  ! YS, lat and lon for HEP file, same for all npos
    real(8), dimension(:,:,:,:), allocatable :: hep                         ! YS, dim 3 for npops, dim 4 for time
    real(8), dimension(:,:,:), allocatable   :: hep_wk                      ! YS, workfile for hep, dim3 for time

!-------------------------------------------------------
! local variables
!-------------------------------------------------------
    integer :: jp
 
    contains

    subroutine check(status,state,operation,variable,timestep,year)
        integer, intent(in) :: status
        integer, intent(in),optional :: state, timestep, year
        character (len=*),optional :: operation,variable
        if (status /= nf90_noerr) then
            print *,"Error detected, aborting program..."
            print *,"Information:"
            if (present(state)) then
                select case (state)
                case (1)
                    print *,"Programm stopped at mod_setup_load() while loading the input file."
                case (2)
                    print *,"Programm stopped at mod_setup_load() while creating the output file."
                case (3)
                    print *,"Programm stopped at mod_setup_load() while saving the initial state."
                case (4)
                    print *,"Programm stopped within mod_run.f95 while saving the outputSensityss."
                end select
            end if
            if (present(operation)) then
                print *,"The operation that failed was nf90_",operation,"."
            end if
            if (present(variable)) then
                print *,"The variable ",variable," was involved in the operation."
            end if
            if (present(timestep) .AND. present(year)) then
                print *,"The exception occured during the timestep ",timestep," within the modelled year ",year,"."
            end if
            print *,"The orignal Error statement follows:"
            print *,trim(nf90_strerror(status))
            stop 2
        end if
    end subroutine check

    subroutine setup_load()
    logical dir_a, dir_n, dir_m   

17     DO  
         call run_no1(iseed)
         if (iseed .ge. 1000 ) exit
       ENDDO
       write(random_seed,'(i4)') iseed

inquire(file="/data/hescor/yshao/hes_model/output/Sensity/AUR/size_5550020001avD15cb1250_"//random_seed//".dat", exist=dir_a)
inquire(file="/data/hescor/yshao/hes_model/output/Sensity/NEA/size_5550020001avD15cb1250_"//random_seed//".dat", exist=dir_n)
inquire(file="/data/hescor/yshao/hes_model/output/Sensity/MIX/size_5550020001avD15cb1250_"//random_seed//".dat", exist=dir_m)
if ( dir_a .or. dir_n .or. dir_m ) go to 17                 ! if file already exist, generate a new iseed for file name

! Output file names for AMH
path_out_pos(1)  = "/data/hescor/yshao/hes_model/output/Sensity/AUR/pos_aur_5550020001avD15cb1250_"//random_seed//".nc"
path_out_dens(1) = "/data/hescor/yshao/hes_model/output/Sensity/AUR/dens_aur_5550020001avD15cb1250_"//random_seed//".nc"
!
! Output file names for NEA
path_out_pos(2)  = "/data/hescor/yshao/hes_model/output/Sensity/NEA/pos_nea_5550020001avD15cb1250_"//random_seed//".nc"
path_out_dens(2) = "/data/hescor/yshao/hes_model/output/Sensity/NEA/dens_nea_5550020001avD15cb1250_"//random_seed//".nc"
!
! Output file names for MIX
path_out_pos(3)  = "/data/hescor/yshao/hes_model/output/Sensity/MIX/pos_mix_5550020001avD15cb1250_"//random_seed//".nc"
path_out_dens(3) = "/data/hescor/yshao/hes_model/output/Sensity/MIX/dens_mix_5550020001avD15cb1250_"//random_seed//".nc"
!
! Output file name for population size, birth, death, disappeared
open(101, file = "/data/hescor/yshao/hes_model/output/Sensity/AUR/size_5550020001avD15cb1250_"//random_seed//".dat")
open(102, file = "/data/hescor/yshao/hes_model/output/Sensity/NEA/size_5550020001avD15cb1250_"//random_seed//".dat")
open(103, file = "/data/hescor/yshao/hes_model/output/Sensity/MIX/size_5550020001avD15cb1250_"//random_seed//".dat")

        write(101,'(6a10)') "Time (yr)", "AUR Human No.", "Birth No.", "Death No.", "Drown No.", "Outed No."
        write(102,'(6a10)') "Time (yr)", "NEA Human No.", "Birth No.", "Death No.", "Drown No.", "Outed No."
        write(103,'(6a10)') "Time (yr)", "MIX Human No.", "Birth No.", "Death No.", "Drown No.", "Outed No."
!--------------------
! Load input HEP file
!--------------------
        DO jp = 1, npops
         call check(nf90_open( path_in(jp), nf90_nowrite, nc_id_hep(jp) ),          1, "open"                    )
!          print *, path_in(jp), nc_id_hep(jp)

        if ( jp == 1 ) then
          call check(nf90_inq_dimid( nc_id_hep(jp), name_in_dlon, dlon_id ),       1, "inq_dimid", name_in_dlon )
!          print *, "name_in_dlon", name_in_dlon
          call check(nf90_inquire_dimension(nc_id_hep(jp), dlon_id, len=dlon_hep), 1, "inquire_dimension", name_in_dlon)
!          print *, "dlon_hep", dlon_hep

          call check(nf90_inq_dimid(nc_id_hep(jp), name_in_dlat, dlat_id),         1, "inq_dimid", name_in_dlat)
!          print *, "name_in_dlat", name_in_dlat
          call check(nf90_inquire_dimension(nc_id_hep(jp), dlat_id, len=dlat_hep), 1, "inquire_dimension", name_in_dlat)
!          print *, "dlat_hep", dlat_hep

          call check(nf90_inq_dimid(nc_id_hep(jp), name_in_dt, dt_id),             1, "inq_dimid", name_in_dt)
!          print *, "name_in_dt", name_in_dt
          call check(nf90_inquire_dimension(nc_id_hep(jp), dt_id, len=dt_hep),     1, "inquire_dimension", name_in_dt)
!          print *, "dt_hep", dt_hep

          call check(nf90_inq_varid(nc_id_hep(jp), name_in_lat, lat_id),           1, "inq_varid", name_in_lat)
          allocate(lat_hep(dlat_hep))                                                              ! ys, allocated npops times, need reprog
          call check(nf90_get_var(nc_id_hep(jp), lat_id, lat_hep),                 1, "get_var", name_in_lat)
!          print *, "lat_hep", lat_hep 

          call check(nf90_inq_varid(nc_id_hep(jp), name_in_lon, lon_id),           1, "inq_varid", name_in_lon)
          allocate(lon_hep(dlon_hep))                                                              ! ys, allocated npops times, need reprog
          call check(nf90_get_var(nc_id_hep(jp), lon_id, lon_hep),                 1, "get_var", name_in_lon)
!          print *, "lon_hep", lon_hep

          allocate(hep_wk(dlon_hep, dlat_hep,        dt_hep))
          allocate(hep   (dlon_hep, dlat_hep, npops, dt_hep))
        endif

        call check(nf90_inq_varid(nc_id_hep(jp), name_in_hep, hep_id),             1, "inq_varid", name_in_hep)
        call check(nf90_get_var(nc_id_hep(jp), hep_id, hep_wk),                    1, "get_var", name_in_hep)

        hep(1:dlon_hep,1:dlat_hep,jp,1:dt_hep)=hep_wk(1:dlon_hep,1:dlat_hep,1:dt_hep)

        call check(nf90_close(nc_id_hep(jp) ))


        IF (save_out .eqv. .true.) THEN
!---------------------------------------
! Declare output file positon of humans
!---------------------------------------
        call check(nf90_create(path_out_pos(jp),NF90_NETCDF4,nc_id_out_pos(jp)),2,"create","path_output_pos")
        call check(nf90_def_dim(nc_id_out_pos(jp), name_out_dim_t, save_t+1, dim_t_id_out(jp)),                  &
       &           2, "def_dim", name_out_dim_t)
        call check(nf90_def_dim(nc_id_out_pos(jp), name_out_dim_hum, hum_max(jp), dim_hum_id_out(jp)),           &
       &           2, "def_dim", name_out_dim_hum)
        call check(nf90_def_dim(nc_id_out_pos(jp), name_out_dim_lonlat, 2, dim_lonlat_id_out(jp)),               &
       &           2, "def_dim", name_out_dim_lonlat)

        dim_pos_hum_id_out(:,jp) = (/dim_lonlat_id_out(jp),dim_hum_id_out(jp),dim_t_id_out(jp)/)

        call check(nf90_def_var(nc_id_out_pos(jp), name_out_var_t, NF90_DOUBLE, dim_t_id_out(jp), var_t_id_out(jp)), &
       &           2, "def_var", name_out_dim_t)
        call check(nf90_def_var(nc_id_out_pos(jp), name_out_var_pos_hum, NF90_DOUBLE, dim_pos_hum_id_out(:,jp),        &
       &           var_pos_hum_id_out(jp)), 2, "def_var", name_out_var_pos_hum)

        call check(nf90_put_att(nc_id_out_pos(jp), NF90_GLOBAL, output_name_description, output_description_pos),    &
       &           2, "put_att", "output_description")
        call check(nf90_put_att(nc_id_out_pos(jp), var_t_id_out(jp), output_name_longname, output_t),                &
       &           2, "put_att", output_t)
        call check(nf90_put_att(nc_id_out_pos(jp), var_pos_hum_id_out(jp), output_name_longname, output_pos_hum),    &
       &           2, "put_att", output_pos_hum)

        call check(nf90_enddef(nc_id_out_pos(jp)),2,"enddef")

        call check(nf90_put_var(nc_id_out_pos(jp), var_t_id_out(jp), 0, start=(/1/)), 3, "put_var", "time")

! Declare output file population density
        call check(nf90_create(path_out_dens(jp),NF90_NETCDF4,nc_id_out_dens(jp)),2,"create","path_output_dens")
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_t, save_t+1, dim_t_id_out(jp)),                     &
       &           2, "def_dim", name_out_dim_t)
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_lat, dlat_hep, dim_lat_id_out(jp)),                 &
       &           2, "def_dim", name_out_dim_lat)
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_lon, dlon_hep, dim_lon_id_out(jp)),                 &
       &           2, "def_dim", name_out_dim_lon)
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_lonlat, 2, dim_lonlat_id_out(jp)),                  &
       &           2, "def_dim", name_out_dim_lonlat)

        dim_dens_id_out(:,jp) = (/ dim_lon_id_out(jp), dim_lat_id_out(jp), dim_t_id_out(jp) /)
        dim_flow_id_out(:,jp) = (/ dim_lonlat_id_out(jp), dim_lon_id_out(jp), dim_lat_id_out(jp), dim_t_id_out(jp) /)

        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_t, NF90_DOUBLE, dim_t_id_out(jp),   &
       &           var_t_id_out(jp)), 2, "def_var", name_out_dim_t)
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_lon,NF90_DOUBLE,dim_lon_id_out(jp), & 
       &           var_lon_id_out(jp)), 2, "def_var", name_out_dim_lon)
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_lat,NF90_DOUBLE,dim_lat_id_out(jp), &
       &           var_lat_id_out(jp)), 2, "def_var", name_out_dim_lat)
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_dens,NF90_DOUBLE,dim_dens_id_out(:,jp), &
       &           var_dens_id_out(jp)), 2, "def_var", name_out_var_dens)
!
! For den and flow
!
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_flow,NF90_DOUBLE,dim_flow_id_out(:,jp), &
       &           var_flow_id_out(jp)), 2, "def_var", name_out_var_flow)
        call check(nf90_put_att(nc_id_out_dens(jp), NF90_GLOBAL, output_name_description, output_description_dens), &
       &           2, "put_att", "output_description")
        call check(nf90_put_att(nc_id_out_dens(jp),var_t_id_out(jp),   output_name_longname,output_t), 2, "put_att",output_t)
        call check(nf90_put_att(nc_id_out_dens(jp),var_lon_id_out(jp), output_name_longname,output_lon),2,"put_att",output_lon)
        call check(nf90_put_att(nc_id_out_dens(jp),var_lat_id_out(jp), output_name_longname,output_lat),2,"put_att",output_lat)
        call check(nf90_put_att(nc_id_out_dens(jp),var_dens_id_out(jp),output_name_longname,output_dens),2,"put_att",output_dens)
        call check(nf90_put_att(nc_id_out_dens(jp),var_flow_id_out(jp),output_name_longname,output_flow),2,"put_att",output_flow)
!
        call check(nf90_enddef(nc_id_out_dens(jp)),2,"enddef")
        call check(nf90_put_var(nc_id_out_dens(jp), var_t_id_out(jp),   0,       start=(/1/)), 3, "put_var", "time")
        call check(nf90_put_var(nc_id_out_dens(jp), var_lat_id_out(jp), lat_hep, start=(/1/)), 3, "put_var", "lat")
        call check(nf90_put_var(nc_id_out_dens(jp), var_lon_id_out(jp), lon_hep, start=(/1/)), 3, "put_var", "lon")
        
        ENDIF                                 ! endif (save_out .eqv. .true.)
      enddo                                   ! ENDDO npops

    end subroutine setup_load

end module mod_setup
