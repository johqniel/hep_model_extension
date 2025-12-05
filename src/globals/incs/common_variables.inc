    ! Global variables
    
    ! Time
    integer :: t_hep
    real(8) :: delta_lat
    
    ! Grid dimensions (global for now)
    integer :: dlon_hep
    integer :: dlat_hep
    real(8) :: lon_0
    real(8) :: lat_0
    real(8) :: delta_lon
    
    ! Arrays (Allocatable)
    real(8), allocatable :: hep(:,:,:,:)
    real(8), allocatable :: hep_av(:,:,:)
    real(8), allocatable :: dens(:,:,:)
    real(8), allocatable :: dens_adj(:,:,:)
    real(8), allocatable :: pop_pressure_arr(:,:,:)
    real(8), allocatable :: distm(:,:,:)
    real(8), allocatable :: flow(:,:,:,:)
    real(8), allocatable :: flow_acc(:,:,:,:)
    real(8), allocatable :: area_for_dens(:,:)
    integer, allocatable :: idens(:,:,:)
    
    real(8), allocatable :: lat_hep(:)
    real(8), allocatable :: lon_hep(:)
    
    real(8), allocatable :: ux(:,:)
    real(8), allocatable :: uy(:,:)
    real(8), allocatable :: x(:,:)
    real(8), allocatable :: y(:,:)
    integer, allocatable :: hum_t(:)
    
    ! Parameters
    integer :: N_max
    real(8) :: eta
    real(8) :: epsilon

    ! Bounds
    real(8) :: lon_min_out, lon_max_out, lat_min_out, lat_max_out
    
    ! Counters
    integer :: realised_birth_counter = 0
    integer :: pregnancy_counter = 0
    integer :: found_mates_counter = 0
    integer :: agents_born_counter = 0
    integer :: drown_count = 0
    integer :: out_count = 0
    integer :: death_count = 0
    integer :: out_count_a = 0
    integer :: out_count_b = 0
