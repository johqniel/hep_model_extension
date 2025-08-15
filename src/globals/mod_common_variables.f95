module mod_common_variables


use mod_constants
use mod_parameters


! +++++++++++++++++++++++++++++++++++++++++++++++ !
! Parameters that might change during runtime     !
! ++++++++++++++++++++++++++++++++++++++++++++++++!
integer :: hum_max(npops) = hum_max_initial
integer :: hum_max_A = maxval(hum_max_initial)

!---------------------------------------------------------------------------
! Yaping Shao 
! 06 Jul 2024 
! common block for the N_HSM 
!---------------------------------------------------------------------------
! number of populations
!---------------------------------------------------------------------------


    real(8), dimension(:), allocatable     :: Ax, Ay   ! random array for x and y component
    real(8), dimension(100)                :: rgauss   ! a Gaussian random number
    
    real(8), dimension(:,:), allocatable   :: x, y     ! agents position, dim 1 = no. of agents, dim 2=npops   [lon] and [lat] degree
    real(8), dimension(:,:), allocatable   :: x0, y0   ! initial agents position, dim as above                 [lon] and [lat] degree
    real(8), dimension(:,:), allocatable   :: ux, uy   ! agent velocity         , dim as above                 [km yr-1]
    real(8), dimension(:,:), allocatable   :: ux0,uy0  ! initial ux, uy         , dim as above                 [km yr-1]
    integer, dimension(:,:), allocatable   :: hum_id   ! agent identification number, dim as above
    integer, dimension(:,:), allocatable   :: hum_id_0 ! initial hum_id
!
    real(8), dimension(:,:,:), allocatable   :: dens, dens_acc, dens_adj, hep_av, pop_pressure, distm ! dim1,2=gird,dime3=npops
    real(8), dimension(:,:,:,:), allocatable :: flow, flow_acc                              ! dim4=npops       [N/(100 km)/yr]
    real(8), dimension(:,:), allocatable     :: area_for_dens                               ! grid ell area, independent of npops [km^2]
    integer, dimension(:,:,:), allocatable   :: idens                                       ! dens in PDU, idens in number of humans in cell
!
    integer, dimension(npops) :: drown_count, out_count, death_count, birth_count, hum_count
    integer, dimension(npops) :: drown_count_priv = 0, out_count_priv = 0, death_count_priv = 0
    integer, dimension(npops) :: status, amount_cluster, avg_cluster

    logical, dimension(:,:), allocatable   :: is_dead
    logical, dimension(:,:), allocatable   :: is_dead0



!+++++++++++++++++++++++++++++++++++++++++++++++++++++++!
! Variables used for the hep and netcdf
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++!


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
    ! DN : I think that lon and lat_hep are set once and then never changed so maybe the should be parameters or variables 
    !      but defined in a different file so that this is clear ( this = that they are not changed after initilization)
    real(8), dimension(:,:,:,:), allocatable :: hep                         ! YS, dim 3 for npops, dim 4 for time
    real(8), dimension(:,:,:), allocatable   :: hep_wk                      ! YS, workfile for hep, dim3 for time


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
! Time Variables                                         !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

integer :: t_hep
integer :: t

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
! Variables to keep track of stuff                       !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

integer, dimension(npops) :: out_count_priv_a
integer, dimension(npops) :: out_count_priv_b




end module mod_common_variables