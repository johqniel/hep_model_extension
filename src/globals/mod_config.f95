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
