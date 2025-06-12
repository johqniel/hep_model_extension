module mod_birth_death
!--------------------------------------------------------------------------------------------
! Yaping Shao 12 Jul 2024
! Note: in examining code of K Klein, I found the method he used may be incorrect, e.g., why 
! should there be only one birth and one death per time in a grid cell. The loggistic equation 
! is not satisfied. I developed a new module, suth that 
! (1) birth-death satisfies loggistic equation
! (2) randomness in birth death process
! (3) multi population birth death
!----------------------------------------------------------------------------------------------

    use mod_kinds
    use mod_utility
    use mod_rnorm, only: dp, rnorm_vec
    use omp_lib

    ! for agent class integration
    use mod_agent_class
    use mod_agent_matrix_merge
    
    ! for is dead matrix
    use mod_setup

    use mod_agent_matrix_merge


    implicit none
    public
    contains

  

    subroutine birth_death_euler1(x, y, ux, uy, sigma_u, irho, rho_adj, hep, lat_in, lon_in, r_B, d_B, N_max, hum_id, &
                                 hum_count, hum, death_count, birth_count,dt_yr,imus,ip,hum_t)
      !--------------------------------------
      ! Yaping Shao, 16 Jul 2024
      ! Birth death model for one population
      !--------------------------------------
        implicit none
        integer, dimension(npops) :: hum_t                    ! has to be updated thus we have to pass it

        integer, intent(in), dimension(:,:)    :: irho
        real(8), intent(in), dimension(:,:)    :: rho_adj, hep
        real(8), intent(in), dimension(:)      :: lat_in, lon_in
        real(8), intent(in)                    :: r_B       ! population growth rate in Verhurst
        real(8), intent(in)                    :: d_B       ! natural death rate
        real(8), intent(in)                    :: N_max     ! Maximal carrying capacity
        real(8), intent(in)                    :: dt_yr     ! time span in yr                [yr] 
        real(8), intent(in)                    :: sigma_u 

        real(8), dimension(:), intent(inout)   :: x, y, ux, uy       ! (x, y) position and velocity (ux, uy) of humans
        integer, dimension(:), intent(inout)   :: hum_id    ! to track humans
        integer, intent(inout)                 :: hum       ! no. of humans in the population
        integer, intent(inout)                 :: death_count, birth_count, hum_count

        integer, dimension(:,:), allocatable   :: hum_in_cell   ! (dlon x dlat), no. of humans in cell (j,k)

        real(8), dimension(:,:), allocatable   :: mu           ! (dlon x dlat), reproductivity capacity for humans in cell (j,k)
        real(8), dimension(113,110,3), save    :: mus          ! (dlon x dlat x dpop) hardwired, make sure mus has the same dimension as mu
        integer                                :: imus, ip     ! which time step, which population 

        integer :: i, i1, j, k, h
        integer :: pos_min
        integer :: dlat, dlon

        real(8), dimension(1000) :: rmux, rmuy, rmuux, rmuuy   ! Gaussian distributed random numbers
        real(8)                  :: rmu                        ! a uniform random number 
        real(8)                  :: sig_x, sig_y
        real(8)                  :: rBf                        ! modification factor for relative population growth rate 

        ! variables for the agents stuff: 
        integer :: pop_size_old
        pop_size_old = hum

        call random_seed()
        
        dlat = size(lat_in)
        dlon = size(lon_in)
        sig_x = 0.2*( lon_in(2) - lon_in(1) ) 
        sig_y = 0.2*( lat_in(2) - lat_in(1)) 

        allocate(hum_in_cell(dlon, dlat))
        hum_in_cell(:,:) = irho(:,:)                           ! ys. Nr. humans in cell, rho in PDU    

        allocate(mu(dlon, dlat))
        mu(:,:) = 0.

        if ( imus == 1 ) mus(:,:,:) = 0.

        do j = 1, dlon
          do k = 1, dlat
            if (hum_in_cell(j, k) == 0) then
                cycle
            end if

            if ( (hep(j,k) .gt. 0.) ) then 
               rBf = frB( rho_adj(j,k) )
        !              mu(j,k) = hum_in_cell(j,k) * r_B * rBf * (1 - rho_adj(j,k)/(hep(j,k) * N_max)) * dt_yr     ! YS, human increase in (j, k)
        !
        ! Alternative 1, better numerics 
                      mu(j,k) = hum_in_cell(j,k) *( exp( r_B * rBf * (1-rho_adj(j,k)/(hep(j,k)*N_max))*dt_yr ) - 1. ) 
        !--------------------------------------------------------------------------------------------------------
        ! Alternative 2: 
        ! 28 Nov 2024: Yaping Shao, I was thinking what happens if 
        !              d rho/dt = r |1 - rho/rho_max| ( 1 - rho/rho_max)
        !              i.e., making population growth rate non-linear 
        !--------------------------------------------------------------------------------------------------------
        !              mu(j,k) = hum_in_cell(j,k) *( exp( r_B * rBf * abs(1-rho_adj(j,k)/(hep(j,k)*N_max))* &
        !             &                                          (1-rho_adj(j,k)/(hep(j,k)*N_max))*dt_yr ) - 1. )
        !---------------------------------------------------------------------------------------------------------

              mus(j,k,ip) = mus(j,k,ip) + mu(j,k)                                                        

              if( mus(j,k,ip) .ge. 1. ) then                                                       ! Birth 
                i1 = floor( 0.7 * mus(j,k,ip) )                                                    ! Nr. of birth is integer of mus
                mus(j,k,ip) = mus(j,k,ip) - i1                                                     ! reduce mus by i1

                if ( i1 .gt. 1000 ) then
                  print *, "i1 > 1000 "
                  stop
                endif

                rmux  = rnorm_vec(1000, 0.d0, sig_x) 
                rmuy  = rnorm_vec(1000, 0.d0, sig_y)
                rmuux = rnorm_vec(1000, 0.d0, sigma_u) 
                rmuuy = rnorm_vec(1000, 0.d0, sigma_u)


                ! now the agents are born, first the matrix stuff and then the agent-class stuff
                do i = 1, i1
                  birth_count = birth_count + 1
                  hum_count = hum_count + 1
                  hum = hum + 1
 
              !                   call random_number(rmu)

                  x(hum) = lon_in(j) + rmux(i)
                  ux(hum) = rmuux(i)
                  y(hum) = lat_in(k) + rmuy(i)
                  uy(hum) = rmuuy(i)
                  hum_id(hum) = hum_count

                  call agent_born_from_matrix_calc(ip, hum_t) ! call the function to create the agent
                enddo
              elseif ( (mus(j,k,ip) .le. -1.) .and. (hum_in_cell(j,k) .gt. 0) ) then               ! death
                i1 = floor( - 0.7 * mus(j,k,ip) )
                mus(j,k,ip) = mus(j,k,ip) + i1 
                i1 = min( hum_in_cell(j,k), i1 )

                do i = 1, i1
                  death_count = death_count + 1
                !
                ! here KK's original code, slow, needs improvement
                !
                  pos_min = minloc(sqrt((lon_in(j) - x)**2 + (lat_in(k) - y)**2), dim=1)
                  x(pos_min) = -1.0E3
                  y(pos_min) = -1.0E3
                  !is_dead(pos_min,ip) = .true. ! better the function below it checks if the agent is already dead
                  call agent_die_from_matrix_calc(pos_min, ip)
                enddo 

              endif
            endif

          enddo
        enddo

        !        write(*,*) "Finish b_d:, ip, hum_count, hum, death_count, birth_count"
        !        write(*,*)              ip, hum_count, hum, death_count, birth_count

        return

    end subroutine birth_death_euler1

subroutine birth_death_mix(x, y, irho, rho_adj, hep, lat_in, lon_in, r_B, N_max, hum_id, &
                               hum_count, hum, birth_count, dt_yr, hum_t)
        !---------------------------------------------------------------------------
        ! Yaping Shao, 16 Jul 2024
        ! Birth death model for 3 populations
        ! npops = 3, interbreading of first two populations gives the 3rd population
        !---------------------------------------------------------------------------
        implicit none
        integer, dimension(npops) :: hum_t                     ! number of humans in each population
                                                               ! has to be updated thus we have to pass it                             
        integer, intent(in), dimension(:,:,:)  :: irho
        real(8), intent(in), dimension(:,:,:)  :: rho_adj, hep  
        real(8), intent(in), dimension(:)      :: lat_in, lon_in
        real(8), intent(in), dimension(:)      :: r_B                                           ! Birth_rate in function
        real(8), intent(in), dimension(:)      :: N_max                                         ! Maximal carrying capacity
        real(8), intent(in)                    :: dt_yr                                         ! time span in years

        real(8), intent(inout), dimension(:,:) :: x, y                                          ! x and y position of humans

        integer, intent(inout), dimension(:,:)   :: hum_id       ! to track humans
        integer, intent(inout), dimension(:)     :: hum
        integer, intent(inout), dimension(:)     :: birth_count, hum_count

        integer, dimension(:,:,:), allocatable   :: hum_in_cell  ! (dlon x dlat), no. of humans in cell (j,k), dim3 = npops
        real(8), dimension(:,:,:), allocatable   :: mu           ! (dlon x dlat), population increase in cell (j,k), dim3 = npops
        !
        ! local work variables
        !
        real(8) :: mu1, mu2, mu3, rmu
        integer :: i, i1, j, jp, k
        integer :: dlat, dlon, dpop                              ! dlon  dlat are No. of lon and lat grid points; dpop is No. of populations
        integer :: pos_min

        real(8) :: rBf                                           ! modification factor for relative population growth rate

        ! variables for the agents stuff: 
        integer :: pop_size_old
        pop_size_old = hum(3)

        dlat = size(lat_in)
        dlon = size(lon_in)
        dpop = size(r_B)
        !
        ! ys, 31jul2024, check dlat, dlon, dpop, ok
        !
        allocate(hum_in_cell(dlon, dlat, dpop))
        allocate(         mu(dlon, dlat, dpop))
        mu(:,:,:) = 0.
       
        do jp = 1, dpop
          hum_in_cell(:,:,jp) = irho(:,:,jp) 
        enddo	

        do j = 1, dlon
          do k = 1, dlat
            do jp = 1, 2                                         ! pop1 AMH, pop2 NEA, pop3 mix
               rBf = frB ( rho_adj(j, k, jp) )
        !              mu(j,k,jp) = hum_in_cell(j,k,jp)*r_B(jp) * (1 - rho_adj(j,k,jp)/(hep(j,k,jp)*N_max(jp) )) * dt_yr  ! No. of human increase in (j, k)
              mu(j,k,jp) = hum_in_cell(j,k,jp)*( exp( r_B(jp) * rBf * (1-rho_adj(j,k,jp)/(hep(j,k,jp)*N_max(jp)))*dt_yr ) - 1. )
            enddo

            mu1 = mu(j,k,1)
            mu2 = mu(j,k,2)

            if ( (mu1 .gt. 0.) .and. (mu2 .gt. 0.) ) then
               i1 = min( hum_in_cell(j,k,1), hum_in_cell(j,k,2) )

               do i = 1, i1
                 call random_number( rmu ) 
                 if ( rmu .le. 0.01 ) then                      ! 1% chance for admixture 
                   birth_count(3) = birth_count(3) + 1
                   hum_count(3) = hum_count(3) + 1
                   hum(3) = hum(3) + 1
                   x(hum(3),3) = lon_in(j)
                   y(hum(3),3) = lat_in(k)
                   hum_id(hum(3),3) = hum_count(3)
                    

                    call agent_born_from_matrix_calc(3, hum_t) ! call the function to create the agent

                 endif
              enddo
            endif
          enddo
        enddo

        return

end subroutine birth_death_mix

!    FUNCTION frB (rho) RESULT(rBf)
!!------------------------------------------------------------------------------------------
!!  Yaping Shao, 27.11.2024, Universität zu Köln
!!  The loggistic equation of Verhurst d rho/dt = rB rho (1 - rho/rho_c) is not longer valid
!!  if rho is smaller than a critical value, because rho is never 0 if r and rho_c are not
!!  zero. This problem is not realized before. Here, I assume that r to be piesewise.
!!------------------------------------------------------------------------------------------
!
!    IMPLICIT NONE
!    REAL(8)  :: rho, rBf
!    REAL(8), parameter :: rho00 = 0.01          ! PDU, 1 person/(100km*100km), at rho00, rBB reduces to 0
!    REAL(8), parameter :: rho0  = 0.1           ! PDU, 1 person/( 10km* 10km), at rho0,  rBB = rB
!
!    if ( rho .ge. rho0 ) then
!      rBf = 1.
!    elseif ( (rho .lt. rho0) .and. (rho .gt. rho00) ) then
!      rBf = (rho - rho00)/(rho0 - rho00)
!    else
!      rBf = -0.001
!    endif
!
!    END FUNCTION frB

  FUNCTION frB (rho) RESULT(rBf)
        !------------------------------------------------------------------------------------------
        !  Yaping Shao, 27.11.2024, Universität zu Köln
        !  The loggistic equation of Verhurst d rho/dt = rB rho (1 - rho/rho_c) is not longer valid
        !  if rho is smaller than a critical value, because rho is never 0 if r and rho_c are not
        !  zero. This problem is not realized before. Here, I assume that r to be Sigmoid
        !------------------------------------------------------------------------------------------

      IMPLICIT NONE
      REAL(8)  :: rho, rBf
      REAL(8), parameter :: rd = -0.1            ! when mating network breaks down, birth rate = rd * (optimal birth rate)
      REAL(8)  :: x
      
      x = 2.5 * log( rho/0.05 )                  ! = ln(rho) - ln(0.05) / 0.4, see spreadsheet for testing 
      rBf = (1. - rd) / (1. + exp( - x )) + rd

  END FUNCTION frB

end module mod_birth_death
