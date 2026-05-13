module mod_creativity
!----------------------------------------
! Y Shao, yshao@uni-koeln.de, 13 Jan 2026
! module for modelling creativity
!----------------------------------------

    use mod_kinds
    use mod_utility
    use mod_rnorm, only: dp, rnorm_vec
    implicit none

    ! Module parameters for creativity model
    !============================================================================================
    ! forced creativity: dh_1 = gamma_1 f_1(h) 
    real(8), parameter :: ysPmax1  = 0.1       ! maximum chance of force creativity, [0]
    real(8), parameter :: ysAlpha1 = 2.0       ! shape parameter                   , [0]    
    real(8), parameter :: ysPhi_l1 = 0.6       ! scale parameter                   , [PDU]
    real(8), parameter :: ystau1   = 10        ! forced creativity time scale      , [yr]

    ! curiosity creativity: dh_2 = gamma_2 f_2(h)
    real(8), parameter :: ysPmax2  = 0.1       ! maximum chance of curiosity-driven creativity, [0] 
    real(8), parameter :: ysk2     = 3         ! shape parameter                   , [0]
    real(8), parameter :: ysPhi_l2 = 3.5       ! scale parameter                   , [PDU]
    real(8), parameter :: ystau2   = 50        ! curiosity creativity time scale   , [yr]

    ! interactive creativity: 
    real(8), parameter ::  ysl     = 0.00005   ! ysl = l(a) = l, age dependent learning cabability [PDU yr]^-1
    real(8), parameter ::  ysR     = 300       ! human interaction scale distance,  [km]

    ! random creativity, using Levy flights 
    logical            ::  ysLevy = .false.

    real(8), parameter :: min_creativity  = 0.1d0  ! Minimum creativity level
    real(8), parameter :: max_creativity  = 10.0d0 ! Maximum creativity level
    real(8), parameter :: creativity_mean = 0.5d0  ! Initial mean creativity
    real(8), parameter :: creativity_std  = 0.1d0  ! Initial std dev for creativity

    ! Arrays to store creativity values
    logical, save :: initialized = .false.
    real(8), dimension(:), allocatable, save :: creativity       ! Creativity of each agent for each population
    real(8), dimension,    allocatable, save :: mean_creativity  ! Agent ensemble mean creativity

contains
    !------------------------------------------------------------
    ! Agents creativity
    !------------------------------------------------------------
    subroutine c3(x, y, hum_t, hep_av, dens, lon_hep, lat_hep, dlon_hep, dlat_hep, dt)
        real(8), dimension(:,:), intent(in) :: x, y        ! Agent, agent positions
        integer,                 intent(in) :: hum_t       ! Agent, number of agents
        real(8), dimension(:,:), intent(in) :: hep_av      ! Grid, available HEP
        real(8), dimension(:,:), intent(in) :: dens        ! Grid, population density
        real(8), dimension(:),   intent(in) :: lon_hep, lat_hep     ! Grid, longitude and latitude coordinates
        integer,                 intent(in) :: dlon_hep, dlat_hep   ! Grid, lon_hep and lat_hep dimensions
        real(8),                 intent(in) :: dt                   ! Time, time step
       
        real(8) :: ysP1, ysP2, gamma1, gamma2
        real(8) :: forced_term, curiosity_term, interaction_term, levy_term

        real(8) :: dlAB, dpAB, dxAB, dyAB
     
        ! Variables needed for finding maximum
        real(8) :: max_density
        integer :: max_dens_x, max_dens_y
        integer, dimension(2) :: max_loc

        integer :: i, j, gx, gy

        real(8) :: phi_av, phi_diff, gamma
        real(8) :: dist, c1, c2
        real(8) :: lon_0, lat_0, delta_lon, delta_lat
        real(8) :: rand_val
        
        ! Calculate grid cell size
        delta_lon = lon_hep(2) - lon_hep(1)
        delta_lat = lat_hep(2) - lat_hep(1)
        lon_0 = lon_hep(1) - 0.5*delta_lon
        lat_0 = lat_hep(1) - 0.5*delta_lat
        
        ! Find grid point with maximum density. YS: why is this needed?
        max_density = maxval(dens(1:dlon_hep, 1:dlat_hep))
        max_loc     = maxloc(dens(1:dlon_hep, 1:dlat_hep))
        max_dens_x  = max_loc(1)
        max_dens_y  = max_loc(2)

        ! Update creativity for each agent in this population
        do i = 1, hum_t

        ! Skip invalid agents (those that have been marked as dead/removed)
          if (x(i) <= -900.0d0 .or. y(i) <= -900.0d0) cycle
                
            ! Find grid cell in which agent is located 
            gx = floor((x(i) - lon_0) / delta_lon) + 1
            gy = floor((y(i) - lat_0) / delta_lat) + 1
                
            ! Skip agents outside the grid
            if (gx < 1 .or. gx > dlon_hep .or. gy < 1 .or. gy > dlat_hep) cycle
                
            ! Get phi_av at agent's location
              phi_av = hep_av(gx, gy)
                
            ! Calculate forced creativity probability
            ysP1 = ysPmax1 * (phi_av/ysPhi_l1)**(ysAlpha1 - 1) * exp( -(phi_av/ysPhi_l1)**ysAlpha1 )

            ! Calculate curiosity creativity probability
            ysP2 = ysPmax2/(1 + exp(-ysk2*(phi_av - ysPhi_l2)))

            ! Calculate gamma parameter using a probability based on phi_av
            call random_number(rand_val)
            if (rand_val <= ysP1 ) then
              gamma1 = 1.0d0
            else
              gamma1 = 0.0d0
            endif
            
            if (rand_val <= ysP2 ) then
              gamma2 = 1.0d0
            else
              gamma2 = 0.0d0
            endif

            forced_term    = gamma1 * creativity(i) / ystau1
            curiosity_term = gamma2 * creativity(i) / ystau2
                
            ! Calculate interaction term with other agents
            interaction_term = 0.0d0

            do j = 1, hum_t
              ! Skip self and invalid agents
              if (i == j) cycle
              if (x(j) <= -900.0d0 .or. y(j) <= -900.0d0) cycle
                    
              dlAB = x(i) - x(j)                                    ! d lambda
              dpAB = y(i) - y(j)                                    ! d phi
              dxAB = 111.3*dlAB*cos(dpAB*pi/180)                    ! dx km, 111.3 deg to km conversion factor
              dyAB = 111.3*dpAB                                     ! dy km
              dist = sqrt( dxAB**2 + dyAB**2 )

              ! Only interact if within radius and other agent has higher creativity
              if ( dist <= 3.*ysR .and. creativity(j) > creativity(i) ) then
                interaction_term = interaction_term +                              &
                ysl*creativity(i)*( creativity(j) - creativity(i) ) * exp( -dist / ysR )
              end if
               interaction_term = interaction_term * a3
            enddo

!------------------
! Levy flight to do
!------------------
 
            ! Update creativity
            creativity(i) = creativity(i) + (forced_term + curiosity_term + interaction_term) * dt

            ! Limit creativity to range ( min_creativity, max_creativity )  
            creativity(i) = max(min(creativity(i), max_creativity), min_creativity)
            end do                                                ! enddo j
        end do                                                    ! enddo i
        
        ! Calculate and store mean creativity across all populations
        call ensemble_c3(hum_t, npops)
    end subroutine c3
    
    !------------------------------------------------
    ! Calculate ensemble creativity across all agents
    !------------------------------------------------
    subroutine ensemble_c3(hum_t)
        integer, intent(in) :: hum_t        ! Number of agents in each population
        integer :: i
        real(8) :: mean_creativity
        
        ! Calculate mean creativity
        mean_creativity = 0.0d0
        do i = 1, hum_t
           mean_creativity = mean_creativity + creativity(i)
        end do
        
        if (hum_t > 0) then
            mean_creativity = mean_creativity / real(hum_t, 8)
        else
            mean_creativity = 0.0d0
        end if
    end subroutine ensemble_c3
    
end module mod_creativity
