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
            real(8) :: sqdt                ! sqrt(dt), precomputed
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

            real(8), allocatable :: eta(:)
            real(8), allocatable :: epsilon(:)
            real(8), allocatable :: rho_max(:)


            ! variables for the grid

            real(8) :: delta_lat, delta_lon
            real(8) :: lon_0, lat_0
            integer :: dlon_hep, dlat_hep

            ! variables for the hep
            logical :: with_pop_pressure

            ! Clustering settings
            integer :: clustering_algorithm = 1          ! 1=Watershed (future: 2=DBSCAN, etc.)
            integer :: human_density_smoothing_radius = 2       ! Box-filter half-width
            integer :: human_density_smoothing_iterations = 1   ! Number of passes
            real(8) :: watershed_threshold = 0.05d0      ! Ignore cells below this
            integer :: cluster_update_interval = 100     ! Ticks between re-clustering
            integer :: c3_individual_update_interval = 100 ! Ticks between individual creativity updates
            integer :: c3_cluster_update_interval = 100    ! Ticks between creativity NC_AV recompute
            integer :: kmeans_n_clusters = 5              ! Number of clusters for K-means
            real(8) :: dbscan_eps = 3.0d0                 ! DBSCAN neighbourhood radius (grid cells)
            integer :: dbscan_minpts = 3                  ! DBSCAN minimum points per cluster

            ! =================================================================
            ! Mod: C3 Creativity (mod_creativity)
            ! =================================================================
            real(8) :: c3_Pmax1  = 0.1d0       ! Max probability of forced creativity
            real(8) :: c3_Alpha1 = 2.0d0        ! Weibull shape parameter
            real(8) :: c3_Phi_l1 = 0.6d0        ! Weibull scale parameter [PDU]
            real(8) :: c3_tau1   = 10.0d0       ! Forced creativity time scale [yr]
            real(8) :: c3_Pmax2  = 0.1d0        ! Max probability of curiosity creativity
            real(8) :: c3_k2     = 3.0d0        ! Logistic shape parameter
            real(8) :: c3_Phi_l2 = 3.5d0        ! Logistic scale parameter [PDU]
            real(8) :: c3_tau2   = 50.0d0       ! Curiosity creativity time scale [yr]
            real(8) :: c3_l      = 0.00005d0    ! Learning capability [PDU yr]^-1
            real(8) :: c3_R      = 300.0d0      ! Interaction scale distance [km]
            integer :: c3_search_r_cap = 5      ! Max grid-cell search radius (caps c3_R)
            real(8) :: c3_min_creativity = 0.1d0
            real(8) :: c3_max_creativity = 10.0d0
            integer :: max_high_creativity_fast = 5     ! Max number of high creativity individuals to track per cell
            
            ! reviewed agent motion parameters
            real(8) :: cb1 = 500.0d0                     ! Gradient Attraction Strength
            real(8) :: cb2 = 0.2d0                       ! Friction / Damping
            real(8) :: cb3 = 20.0d0                      ! Random Diffusion Strength

            ! =================================================================
            ! Mod: Birth/Death - Clustered
            ! =================================================================
            real(8) :: NC = 25.0d0            ! Carrying capacity (units: People / 100 km^2)

            ! =================================================================
            ! Mod: Birth/Death/Preparation (mod_reviewed_modules)
            ! =================================================================
            real(8) :: r = 0.0d0, NC_Global = 0.0d0, Kmin = 0.0d0, Kmax = 0.0d0, b5 = 0.0d0
            real(8) :: b6 = 0.0d0, b7 = 0.0d0, b8 = 0.0d0, b9 = 0.0d0, b10 = 0.0d0
            real(8) :: d1 = 0.0d0, d2 = 0.0d0, d3 = 0.0d0, d4 = 0.0d0, d5 = 0.0d0
            real(8) :: d6 = 0.0d0, d7 = 0.0d0, d8 = 0.0d0, d9 = 0.0d0, d10 = 0.0d0
            real(8) :: p1 = 0.0d0, p2 = 0.0d0, p3 = 0.0d0, p4 = 0.0d0, p5 = 0.0d0
            real(8) :: p6 = 0.0d0, p7 = 0.0d0, p8 = 0.0d0, p9 = 0.0d0, p10 = 0.0d0

            ! =================================================================
            ! Mod: Reviewed Modules (mod_reviewed_modules)
            ! =================================================================
            real(8) :: r1 = 0.0d0, r2 = 0.0d0, r3 = 0.0d0, r4 = 0.0d0, r5 = 0.0d0
            real(8) :: r6 = 0.0d0, r7 = 0.0d0, r8 = 0.0d0, r9 = 0.0d0, r10 = 0.0d0
            
            ! HEP Input Files
            character(len=256), allocatable :: hep_paths(:)

            ! Population-specific mating control
            logical :: allow_across_populations = .true.

            ! Efficient cluster-restricted density and carrying capacity updates
            logical :: efficient_density_updates = .false.

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
