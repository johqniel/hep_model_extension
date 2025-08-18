module mod_parameters

!++++++++++++++++++++++++++++++++++++++++++++++++++
! Model parameters
!++++++++++++++++++++++++++++++++++++++++++++++++++
! Times 
!--------------------------------------------------
    integer, parameter :: npops = 3                    ! e.g., AMH, NEA, MIX
                                                       ! MIX = AMH cross with NEA; I assume MIX is same as AMH, having same HEP, rho_max etc 
                                                       ! The properties are specified in mod_setup.f95
                                                       !  



    real(8), dimension(npops) :: tyr_start                ! Start year of simulation
    data tyr_start /-43000, -50000, -43000/               ! Start year of simulation for different populations for AUR, NEA, MIX
    integer, dimension(npops) :: tstep_start              ! Start time step of simulation
!    data tstep_start /1, 1, 1 /                  ! for testing all populations start right away
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
    integer, parameter :: hum_max_initial(npops) = [16000,5000,1000]     ! Maximum initial number of humans per population
    
    ! e.g., 8000 AMHs, 5000 NEAs, 1000 MIXs; ys: do not see why hum_max is needed
    !                                        dn: I dont see that either we want to remove this eventually

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
    real(8), dimension(npops) :: cb2
    real(8), dimension(npops) :: cb3
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




end module mod_parameters