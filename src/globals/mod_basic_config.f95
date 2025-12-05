module mod_basic_config
    implicit none

    !++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Model parameters
    !++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Times 
    !--------------------------------------------------
    integer, parameter :: npops = 3                    ! e.g., AMH, NEA, MIX
                                                       ! MIX = AMH cross with NEA; I assume MIX is same as AMH, having same HEP, rho_max etc 
                                                       ! The properties are specified in mod_setup.f95
                                                       !  

    real(8), parameter :: tyr_start(npops) = [-43000.d0, -50000.d0, -43000.d0]
    integer, parameter :: tstep_start(npops) = [700001, 1, 700001]

    real(8), parameter :: tyr_end(npops) = [-38000.d0, -38000.d0, -38000.d0]
    real(8), parameter :: tyr_length(npops) = [5000.d0, 12000.d0, 5000.d0]

    real(8), parameter :: dt = 0.01
    integer, parameter :: Tn = 1200000

    integer, parameter :: save_t = Tn / 1000

    integer, parameter :: delta_t_hep = 2000

    integer, parameter :: hum_max_initial(npops) = [16000, 5000, 1000]

    real(8), parameter :: tau(npops) = [6.d0, 6.d0, 6.d0] ! Assumed from comments
    real(8), parameter :: sigma_u(npops) = [15.d0, 10.d0, 10.d0] ! Assumed from comments
    
    ! cb1, cb2, cb3 were not initialized in parameters.inc, leaving them as allocatable/variable if they are not constant?
    ! Wait, parameters.inc had them as `real(8), dimension(npops) :: cb1`. But no data statement.
    ! If they are not initialized, they can't be parameters.
    ! But the user said "extend the config container by all variables that are hard coded".
    ! If they are not hardcoded, they are just declarations.
    ! I will leave them as variables but initialized to 0.0 or something if needed, or just variables.
    ! But `mod_basic_config` is supposed to be "hard coded vars".
    ! If they are not initialized, they are not hardcoded values.
    ! I'll leave them as variables for now.
    
    real(8), dimension(npops) :: cb1, cb2, cb3

    real(8), parameter :: eta(npops) = [1.6d0, 1.6d0, 1.6d0]
    real(8), parameter :: epsilon(npops) = [0.4d0, 0.4d0, 0.4d0]
    real(8), parameter :: rho_max(npops) = [5.d0, 1.d0, 2.5d0] ! Assumed from comments
    real(8), parameter :: r_B(npops) = [0.03d0, 0.01d0, 0.02d0] ! Assumed from comments
    real(8), parameter :: d_B(npops) = [0.02d0, 0.025d0, 0.0225d0] ! Assumed from comments

    real(8), parameter :: dt_bdyr = 1.
    integer, parameter :: dt_bd = 100

    logical, parameter :: with_pop_pressure = .false.
    logical, parameter :: with_birth_n_death = .true.

    real(8), parameter :: eps = 10.
    integer, parameter :: minpts = 3

    integer, parameter :: ns = 4

    real(8), parameter :: x_ini_c(ns, npops) = reshape([4.0d0, 4.0d0, 4.0d0, 4.0d0, &
                                                        -8.6d0, -3.93d0, -0.72d0, -5.54d0, &
                                                        4.0d0, 4.0d0, 4.0d0, 4.0d0], [ns, npops])
                                                        
    real(8), parameter :: y_ini_c(ns, npops) = reshape([44.08d0, 44.08d0, 44.08d0, 44.08d0, &
                                                        39.2d0, 42.3d0, 40.03d0, 36.66d0, &
                                                        44.08d0, 44.08d0, 44.08d0, 44.08d0], [ns, npops])
                                                        
    real(8), parameter :: ini_spread(ns, npops) = reshape([0.5d0, 0.5d0, 0.5d0, 0.5d0, &
                                                           1.0d0, 1.0d0, 1.0d0, 1.0d0, &
                                                           0.5d0, 0.5d0, 0.5d0, 0.5d0], [ns, npops])

    integer, parameter :: hum_0(ns, npops) = reshape([200, 200, 200, 200, &
                                                      150, 150, 150, 150, &
                                                      0, 0, 0, 0], [ns, npops])

!+++++++++++++++++++++++++
! Define domain boundaries 
!+++++++++++++++++++++++++
!    real(8), parameter  :: lon_min_out = -10., lon_max_out = 5., lat_min_out = 35, lat_max_out = 48     ! Outer boundary of research area [degree]
    real(8) :: lon_min_out, lon_max_out, lat_min_out, lat_max_out     ! Outer boundary of research area [degree]
    



!++++++++++++++++++++++++++++
! Defauls water grid cell HEP
!++++++++++++++++++++++++++++ 
    real(8),  parameter :: water_hep = -1.                                                              ! Chosen HEP for the water grids



!++++++++++++++++++++++++++++++++++++++++++++++++++++
! Parameters for modues
!++++++++++++++++++++++++++++++++++++++++++++++++++++

    ! Birth Module Example

    real(8), parameter :: probability_vertilisation_per_tick = 0.02 ! probability of successful mating per time step, for birth-death module example
    integer, parameter :: age_when_vertile_m = 700                   ! = 14 years
    integer, parameter :: age_when_vertile_f = 700
    integer, parameter :: age_until_vertile_m = 3000
    integer, parameter :: age_until_vertile_f = 2000

    ! pregnancy module

    integer, parameter :: pregnancy_minimum_length = 37              ! minimum length of pregnancy in time steps, e.g., 37 timesteps
    real, parameter :: birth_prob_after_min_length = 0.6          ! probability of birth after minimum length of pregnancy per time step

end module mod_basic_config
