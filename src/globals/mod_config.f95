module mod_config

    implicit none

    ! =================================================================================
    ! World Config 
    ! =================================================================================
    
        ! Agents
        integer :: npops = 3
        integer :: ns = 4

        ! Grid
        integer :: nx
        integer :: ny

        type :: world_config

            ! Model parameters populations

            integer :: npops                   ! Number of populations


            ! Model parameters time
            real(8), allocatable :: tyr_start(:)
            integer, allocatable :: tstep_start(:)
            real(8), allocatable :: tyr_end(:)
            real(8), allocatable :: tyr_length(:)
            real(8) :: dt
            integer :: Tn                 ! Simulation time horizon
            integer :: save_t
            integer :: delta_t_hep

            ! Model parameters initial agent distribution

            integer :: ns                         ! Number of initial agent sources
            integer, allocatable :: hum_0(:,:)
            real(8), allocatable :: x_ini_c(:,:)
            real(8), allocatable :: y_ini_c(:,:)
            real(8), allocatable :: ini_spread(:,:)
            real(8), allocatable :: sigma_u(:)

            ! Age Distribution
            real(8), allocatable :: age_distribution(:)
            logical :: age_distribution_set = .false.

            ! Technical parameters

            integer :: initial_hashmap_size 
            integer :: initial_max_pop_size
            


            ! Variables for calculations

            real(8), allocatable :: tau(:)
            real(8), allocatable :: cb1(:)
            real(8), allocatable :: eta(:)
            real(8), allocatable :: epsilon(:)
            real(8), allocatable :: rho_max(:)
            real(8), allocatable :: r_B(:)
            real(8), allocatable :: d_B(:)


            real(8) :: dt_bdyr
            integer :: dt_bd

            real(8) :: eps
            integer :: minpts

            ! variables for the grid

            real(8) :: delta_lat, delta_lon
            real(8) :: lon_0, lat_0
            integer :: dlon_hep, dlat_hep

            ! variables for the hep
            real(8) :: water_hep
            logical :: with_pop_pressure

            ! variables for the demo birth module
            real(8) :: probability_vertilisation_per_tick
            integer :: age_when_fertile_m
            integer :: age_when_fertile_f
            integer :: age_until_fertile_m
            integer :: age_until_fertile_f
            integer :: pregnancy_minimum_length
            real :: birth_prob_after_min_length

            ! variables for the demo resources module
            integer :: min_resources_for_mating
            real(8) :: min_avg_resources_for_survival
            integer :: ressources_per_hep
            integer :: min_resources_per_gridcell
            
            ! =================================================================
            ! Mod: AGB Birth/Death (mod_birth_death_agb)
            ! =================================================================
            real(8) :: agb_f0                   ! Base natural death prob (0.0025)
            integer :: agb_M                    ! Multiplier inside log (200)
            integer :: agb_age_min              ! Age where risk starts increasing (40 years)
            integer :: agb_age_max              ! Age where risk peaks (80 years)

            ! =================================================================
            ! Mod: Langevin Move (mod_move)
            ! =================================================================
            real(8) :: langevin_gradient_strength ! cb1 (500.0)
            real(8) :: langevin_friction          ! cb2 (0.2)
            real(8) :: langevin_diffusion         ! cb3 (20.0)

            ! =================================================================
            ! Mod: Strict Birth/Death (mod_birth_death_strict)
            ! =================================================================
            real(8) :: strict_cc_scale            ! Carrying capacity scale (10.0)
            real(8) :: strict_growth_rate         ! Growth rate (0.05)

            ! =================================================================
            ! Mod: Probabilistic Birth/Death (mod_birth_death_probabilistic)
            ! =================================================================
            real(8) :: prob_death_alpha           ! Base mortality (1.0d-4)
            real(8) :: prob_death_beta            ! Gompertz scale (1.0d-5)
            real(8) :: prob_death_gamma           ! Gompertz shape (6.0d-2)
            real(8) :: prob_birth_cc_scale        ! CC scale (10.0)
            real(8) :: prob_birth_rate            ! Max birth rate (0.02)
            real(8) :: ticks_per_year             ! Conversion factor (365.0)

            ! =================================================================
            ! Mod: New Birth/Death/Preparation (mod_birth_death_new)
            ! =================================================================
            real(8) :: b1 = 0.0d0, b2 = 0.0d0, b3 = 0.0d0, b4 = 0.0d0, b5 = 0.0d0
            real(8) :: b6 = 0.0d0, b7 = 0.0d0, b8 = 0.0d0, b9 = 0.0d0, b10 = 0.0d0
            real(8) :: d1 = 0.0d0, d2 = 0.0d0, d3 = 0.0d0, d4 = 0.0d0, d5 = 0.0d0
            real(8) :: d6 = 0.0d0, d7 = 0.0d0, d8 = 0.0d0, d9 = 0.0d0, d10 = 0.0d0
            real(8) :: p1 = 0.0d0, p2 = 0.0d0, p3 = 0.0d0, p4 = 0.0d0, p5 = 0.0d0
            real(8) :: p6 = 0.0d0, p7 = 0.0d0, p8 = 0.0d0, p9 = 0.0d0, p10 = 0.0d0
            
            ! HEP Input Files
            character(len=256), allocatable :: hep_paths(:)

        end type world_config


    ! ================================================================================
    ! The Grid structure
    ! ================================================================================

    integer, parameter :: initial_array_size_for_agents_ids_in_gridcell = 17

    ! ===============================================================
    ! The hashmap <-> agent array structure
    ! ===============================================================

    ! parameters
    integer, parameter :: initial_agent_array_size = 1000
    integer, parameter :: agent_array_resize_factor = 2
    integer, parameter :: initial_hashmap_size = 2003
    integer, parameter :: hashmap_resize_factor = 2
    real, parameter :: MAX_LOAD_FACTOR = 0.75


    


end module mod_config
