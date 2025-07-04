!---------------------------------------------------------------------------
! Yaping Shao 
! 06 Jul 2024 
! common block for the N_HSM 
!---------------------------------------------------------------------------
! number of populations
!---------------------------------------------------------------------------
    integer, parameter :: npops = 3                    ! e.g., AMH, NEA, MIX
                                                       ! MIX = AMH cross with NEA; I assume MIX is same as AMH, having same HEP, rho_max etc 
                                                       ! The properties are specified in mod_setup.f95
                                                       !  
    integer            :: hum_max_A                    ! the maximum of all hum_max(npops)

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
