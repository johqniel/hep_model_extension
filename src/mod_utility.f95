MODULE mod_utility
!
!svn $Id: utility.F 8 2007-02-06 19:00:29Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2007 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module contains several all purpuse generic routines:          !
!                                                                      !
!  Routines:                                                           !
!                                                                      !
!    nrng            NSWC Gaussian random number generator.            !
!    urng            NSWC uniform random number generator.             !
!                                                                      !
!=======================================================================
!
    USE mod_kinds
    implicit none

    PUBLIC

    CONTAINS


    SUBROUTINE nrng (ix, a, n, ierr)
        !
        !=======================================================================
        !                                                                      !
        !  Gaussian random-number generator from the NSWC Library. It calls    !
        !  the NSWC uniform random-number generator, URNG.                     !
        !                                                                      !
        !  Modernised and included in ROMS by Mark Hadfield, NIWA.             !
        !                                                                      !
        !=======================================================================
        !
        !  Imported variable declarations.
        !
        integer, intent(in) :: n
        integer, intent(inout) :: ix
        integer, intent(out) :: ierr
        real(8), intent(out) :: a(n)
        !
        !  Local variable declarations.
        !
        integer :: i, m
        real(8) :: phi, ran
        real(8) :: tomp(1)
        real(8), parameter :: pi      = 3.14159265358979
        real(8), parameter :: pi2     = 2*pi
        !
        !-----------------------------------------------------------------------
        !  Generate Gaussian random numbers.
        !-----------------------------------------------------------------------
        !
        CALL urng (ix, a, n, ierr)
        !
        IF (ierr.ne.0) RETURN
        !
        IF (n.gt.1) THEN
            m=n/2
            m=m+m
            DO i=1,m,2
                ran=SQRT(-2.0*LOG(a(i)))
                phi=pi2*a(i+1)
                a(i  )=ran*COS(phi)
                a(i+1)=ran*SIN(phi)
            END DO
            IF (m.eq.n) RETURN
        END IF
        !
        CALL urng (ix, tomp, 1, ierr)
        !
        ran=SQRT(-2.0*LOG(a(n)))
        !
        a(n)=ran*COS(pi2*tomp(1))
        !
        RETURN
    END SUBROUTINE nrng


    SUBROUTINE urng (ix, x, n, ierr)
        !
        !=======================================================================
        !                                                                      !
        !  Uniform random-number generator from the NSWC Library               !
        !                                                                      !
        !  Uses the recursion ix = ix*a mod p, where 0 < ix < p                !
        !                                                                      !
        !  Written by Linus Schrage, University of Chicago. Adapted for NSWC   !
        !  Library by A. H. Morris. Modernised & included in ROMS by Mark      !
        !  Hadfield, NIWA.                                                     !
        !                                                                      !
        !=======================================================================
        !
        !  Imported variable declarations.
        !
        integer, intent(in) :: n

        integer, intent(inout) :: ix

        integer, intent(out) :: ierr

        ! #ifdef ASSUMED_SHAPE
        !       real(rk), intent(out) :: x(:)
        ! #else
        real(8), intent(out) :: x(n)
        ! #endif
        !
        !  Local variable declarations.
        !
        integer, parameter :: a = 16807          ! 7^5
        integer, parameter :: b15 = 32768        ! 2^15
        integer, parameter :: b16 = 65536        ! 2^16
        integer, parameter :: p = 2147483647     ! 2^31-1

        integer :: fhi, k, l, leftlo, xalo, xhi

        real(8), parameter :: s = 0.465661E-09
        !
        !-----------------------------------------------------------------------
        !  Generate random numbers.
        !-----------------------------------------------------------------------
        !
        IF (n.le.0) THEN
            ierr=1
            RETURN
        END IF
        IF ((ix.le.0).or.(ix.ge.p)) THEN
            ierr=2
            RETURN
        END IF
        !
        ierr=0
        !
        DO l=1,n
            !
            ! Get 15 high order bits of "ix".
            !
            xhi=ix/b16
            !
            ! Get 16 lower bits of ix and multiply with "a".
            !
            xalo=(ix-xhi*b16)*a
            !
            ! Get 15 high order bits of the product.
            !
            leftlo=xalo/b16
            !
            ! Form the 31 highest bits of "a*ix".
            !
            fhi=xhi*a+leftlo
            !
            ! Obtain the overflow past the 31st bit of "a*ix".
            !
            k=fhi/b15
            !
            ! Assemble all the parts and presubtract "p". The parentheses are
            ! essential.
            !
            ix=(((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16)+k
            !
            ! Add "p" if necessary.
            !
            IF (ix.lt.0) ix=ix+p
            !
            ! Rescale "ix", to interpret it as a value between 0 and 1.
            ! the scale factor "s" is selected to be as near "1/p" as is
            ! appropriate in order that the floating value for "ix = 1",
            ! namely "s", be roughly the same distance from 0 as "(p-1)*s"
            ! is from 1. The current value for "s" assures us that "x(l)"
            ! is less than 1 for any floating point arithmetic of 6
            ! or more digits.
            !
             x(l)=REAL(ix,rk)*s
        END DO
        RETURN
    END SUBROUTINE urng
   
   
    SUBROUTINE BIAN (A, N, STD, MEAN)
        ! ----------------------------------------------------------------------
        !  Transform Standart Normal Distribution with
        !  mean = 0 and variance = 1 also std = 1
        !  to
        !  Normal Distribution with
        !  mean = MEAN and std = STD
        !  Y. Shao 26-05-91
        ! ----------------------------------------------------------------------
        INTEGER, INTENT(IN)                 :: N
        REAL(8), INTENT(IN)                 :: STD, MEAN
        REAL(8), INTENT(INOUT)              :: A(N)
        REAL(8)                             :: astd, aave
        INTEGER                             :: i

        aave = A(1)
        astd = A(1)*A(1)
        DO i = 2, N
            aave = aave + A(i)
            astd = astd + A(i)*A(i)
        ENDDO
        aave = aave/real(N)
        astd = astd/real(N) - aave*aave
        astd = sqrt(astd)
        !
        DO I = 1,N
            A(I) = A(I) + MEAN - aave
            A(I) = STD*A(I)/astd
        ENDDO
        !
        RETURN
    END subroutine BIAN

    SUBROUTINE SWAP (I, J)
        REAL(kind=rk), INTENT(INOUT)         :: I, J
        I = I + J
        J = I - J
        I = I - J
        RETURN
    END SUBROUTINE SWAP

    SUBROUTINE RUN_NO(I, N)
      !
      ! Yaping Shao, get 4 digit random number array of length n
      !
          use,intrinsic :: ISO_Fortran_env
          integer       :: n
          real(REAL32)  :: r(n)
          integer       :: i(n)
          
          call random_number(r)
          i = floor( r*6000. )
    END SUBROUTINE RUN_NO

    SUBROUTINE RUN_NO1(I)
      use,intrinsic :: ISO_Fortran_env
      real(REAL32)  :: r
      integer       :: i

      call random_number(r)
      i = floor( r*6000. )

    END SUBROUTINE RUN_NO1

    SUBROUTINE calc_and_print_runtime(values_start, values_end)
        !input are variables: "values" for start and end dates, created with subroutine: date_and_time(date,time,zone,values)
        integer,dimension(8), intent(in) :: values_start, values_end
        integer,dimension(8) :: value_diff
        !      integer,dimension(12) :: days_month
        integer :: hour, minute, second, millisecond

        !    days_month = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
        value_diff = values_end - values_start


        if (value_diff(8) < 0) then
            millisecond = 1000 + value_diff(8)
            value_diff(7) = value_diff(7) - 1
        else
            millisecond = value_diff(8)
        end if
        if (value_diff(7) < 0) then
            second = 60 + value_diff(7)
            value_diff(6) = value_diff(6) - 1
        else
            second = value_diff(7)
        end if
        if (value_diff(6) < 0) then
            minute = 60 + value_diff(6)
            value_diff(5) = value_diff(5) - 1
        else
            minute = value_diff(6)
        end if
        if (value_diff(5) < 0) then
            hour = 24 + value_diff(5)
            value_diff(3) = value_diff(3) - 1
        else
            hour = value_diff(5)
        end if


        IF (value_diff(1) /= 0 .OR. value_diff(2) /= 0 .OR. value_diff(3) /= 0 .OR. value_diff(5) /= 0) THEN
          print *, "runtime:", value_diff(1),  "year diff" , value_diff(2), "month diff", value_diff(3), "day diff", &
                  hour, "h", minute, "min", second, "sec", millisecond, "ms"
        ELSE IF (value_diff(6) /= 0) THEN
          print *, "runtime:", minute, "min", second, "sec", millisecond, "ms"
        ELSE IF (value_diff(7) /= 0) THEN
          print *, "runtime:", second, "sec", millisecond, "ms"
        ELSE
          print *, "runtime:", millisecond, "ms"
        END IF

    end SUBROUTINE


    subroutine k_means_clustr ( x, d, dev, b, f, e, i, j, n, nz, k )

      !*****************************************************************************80
      !
      !! CLUSTR uses the K-means algorithm to cluster data.
      !
      !  Discussion:
      !
      !    Given a matrix of I observations on J variables, the
      !    observations are allocated to N clusters in such a way that the
      !    within-cluster sum of squares is minimised.
      !
      !  Modified:
      !
      !    23 January 2008
      !
      !  Author:
      !
      !    Original FORTRAN77 version by David Sparks.
      !    FORTRAN90 version by John Burkardt.
      !
      !  Reference:
      !
      !    David Sparks,
      !    Algorithm AS 58:
      !    Euclidean Cluster Analysis,
      !    Applied Statistics,
      !    Volume 22, Number 1, 1973, pages 126-130.
      !
      !  Parameters:
      !
      !    Input, real ( kind = 8 ) X(I,J), the observed data.
      !
      !    Input/output, real ( kind = 8 ) D(K,J), the cluster centers.
      !    On input, the user has chosen these.  On output, they have been
      !    updated.
      !
      !    Output, real ( kind = 8 ) DEV(K), the sums of squared deviations
      !    of observations from their cluster centers.
      !
      !    Output, integer ( kind = 4 ) B(I), indicates the cluster to which
      !    each observation has been assigned.
      !
      !    Workspace, real ( kind = 8 ) F(I).
      !
      !    Output, integer ( kind = 4 ) E(K), the number of observations assigned
      !    to each cluster.
      !
      !    Input, integer ( kind = 4 ) I, the number of observations.
      !
      !    Input, integer ( kind = 4 ) J, the number of variables.
      !
      !    Input, integer ( kind = 4 ) N, the number of clusters.
      !
      !    Input, integer ( kind = 4 ) NZ, the minimum number of observations
      !    which any cluster is allowed to have.
      !
      !    Input, integer ( kind = 4 ) K, the maximum number of clusters.
      !
        implicit none

        integer ( kind = 4 ) i
        integer ( kind = 4 ) k

        integer ( kind = 4 ) b(i)
        real(8), parameter :: big = 1.0D+10
        real(8)            :: d(k,j)
        real(8)            :: da, db, dc, de
        real(8)            :: dev(k)
        integer ( kind = 4 ) e(k)
        real(8) f(i)
        real(8)            :: fl, fm, fq
        integer ( kind = 4 ) ic
        integer ( kind = 4 ) id
        integer ( kind = 4 ) ie
        integer ( kind = 4 ) ig
        integer ( kind = 4 ) ii
        integer ( kind = 4 ) ij
        integer ( kind = 4 ) ik
        integer ( kind = 4 ) il
        integer ( kind = 4 ) in
        integer ( kind = 4 ) ip
        integer ( kind = 4 ) ir
        integer ( kind = 4 ) is
        integer ( kind = 4 ) it
        integer ( kind = 4 ) iu
        integer ( kind = 4 ) iw
        integer ( kind = 4 ) j
        integer ( kind = 4 ) n
        integer ( kind = 4 ) nz
        real(8)            :: x(i,j)

        e(1:n) = 0
      !
      !  For each observation, calculate the distance from each cluster
      !  center, and assign to the nearest.
      !
        do ic = 1, i

          f(ic) = 0.0D+00
          da = big

          do id = 1, n

            db = 0.0D+00
            do ie = 1, j
              dc = x(ic,ie) - d(id,ie)
              db = db + dc * dc
            end do

            if ( db < da ) then
              da = db
              b(ic) = id
            end if

          end do

          ig = b(ic)
          e(ig) = e(ig) + 1

        end do
      !
      !  Calculate the mean and sum of squares for each cluster.
      !
        dev(1:n) = 0.0D+00
        d(1:n,1:j) = 0.0D+00

        do ic = 1, i
          ig = b(ic)
          d(ig,1:j) = d(ig,1:j) + x(ic,1:j)
        end do

        do ij = 1, j
          do ii = 1, n
            d(ii,ij) = d(ii,ij) / real ( e(ii), kind = 8 )
          end do
        end do

        do ij = 1, j
          do ik = 1, i
            il = b(ik)
            da = x(ik,ij) - d(il,ij)
            db = da * da
            f(ik) = f(ik) + db
            dev(il) = dev(il) + db
          end do
        end do

        do ik = 1, i
          il = b(ik)
          fl = e(il)
          if ( 1 < e(il) ) then
            f(ik) = f(ik) * fl / ( fl - 1.0D+00 )
          end if
        end do
      !
      !  Examine each observation in turn to see if it should be
      !  reassigned to a different cluster.
      !
        do

          iw = 0

          do ik = 1, i

            il = b(ik)
            ir = il
      !
      !  If the number of cluster points is less than or equal to the
      !  specified minimum, NZ, then bypass this iteration.
      !
            if ( nz < e(il) ) then

              fl = e(il)
              dc = f(ik)

              do in = 1, n

                if ( in /= il ) then

                  fm = e(in)
                  fm = fm / ( fm + 1.0D+00 )

                  de = 0.0D+00
                  do ip = 1, j
                    da = x(ik,ip) - d(in,ip)
                    de = de + da * da * fm
                  end do

                  if ( de < dc ) then
                    dc = de
                    ir = in
                  end if

                end if

              end do
      !
      !  Reassignment is made here if necessary.
      !
              if ( ir /= il ) then

                fq = e(ir)
                dev(il) = dev(il) - f(ik)
                dev(ir) = dev(ir) + dc
                e(ir) = e(ir) + 1
                e(il) = e(il) - 1

                do is = 1, j
                  d(il,is) = ( d(il,is) * fl - x(ik,is) ) / ( fl - 1.0D+00 )
                  d(ir,is) = ( d(ir,is) * fq + x(ik,is) ) / ( fq + 1.0D+00 )
                end do

                b(ik) = ir

                do it = 1, i

                  ij = b(it)

                  if ( ij == il .or. ij == ir ) then
                    f(it) = 0.0D+00
                    do iu = 1, j
                      da = x(it,iu) - d(ij,iu)
                      f(it) = f(it) + da * da
                    end do
                    fl = e(ij)
                    f(it) = f(it) * fl / ( fl - 1.0D+00 )
                  end if

                end do

                iw = iw + 1

              end if

            end if

          end do
      !
      !  If any reassignments were made on this pass, then do another pass.
      !
          if ( iw == 0 ) then
            exit
          end if

        end do

        return
    end subroutine k_means_clustr

    subroutine find_closest_grid_point( posx, posy, lon, lat, gx, gy )
      !---------------------------------------------------------------------
      ! K Klein for PhD thesis
      ! Notes by Yaping Shao, 12 Jul 2024: this is a very slow way of finding
      ! the grid_point. Do not use. Instead, I have written a new subroutine
      ! which works much faster, see fin_grid
      !----------------------------------------------------------------------
              implicit none
              real(8), intent(in) :: posx, posy
              real(8), dimension(:), intent(in) :: lon, lat
              integer, intent(out)  :: gx, gy
              integer, dimension(2) :: g
              real(8), dimension(:,:), allocatable :: distm
              integer :: i
              real(8), parameter :: pi      = 3.14159265358979

              allocate(distm(size(lon), size(lat)))
              do i = 1,size(lon)
                  distm(i,:) = sqrt((111.3 * cos((lat(:) - posy)*pi/180) * abs(lon(i) - posx))**2 + &
                          (111.3 * abs(lat(:) - posy))**2)
              end do
              g = minloc(distm)
              gx = g(1)
              gy = g(2)

              return
    end subroutine find_closest_grid_point

    subroutine find_grid( posx, posy, lon, lat, gx, gy )
      !---------------------------------------------------------------------
      ! Yaping Shao, 12 Jul 2024
      ! Purpose: given position of a human (posx, posy), find in which grid
      !          cell the human is located
      ! Input
      ! posx, posy: longitude  and latitude of a human            [°]
      ! lon , lat : longitudes and latitudes of grids             [°] 
      ! Output
      ! gx, gy    : number of grid cell, in which the human is located
      !----------------------------------------------------------------------
              implicit none
              real(8), intent(in) :: posx, posy
              real(8), dimension(:), intent(in) :: lon, lat
              integer, intent(out)  :: gx, gy
              integer :: i
              real(8) :: dl, dp
              dl = lon(2) - lon(1)
              dp = lat(2) - lat(1)

              print *, dl, dp, lon(10)-lon(9), lat(15)-lat(14)

              gx = int( (posx - lon(1)) / dl + 0.5 )
              gy = int( (posy - lat(1)) / dp + 0.5 )

              return
    end subroutine find_grid

    subroutine sort_class(classi, p_in_class, amount_cluster)
        implicit none
        real(8), dimension(:,:), intent(inout) :: classi
        integer, dimension(:), intent(in) :: p_in_class
        integer, intent(in) :: amount_cluster

        real(8), dimension(4) :: class_help
        integer :: i, j, k, c, num_class

        c = 1
        k = 1
        do i = 0, amount_cluster
            j = 0
            if (i == 0) then
                num_class = p_in_class(size(p_in_class))
            else
                num_class = p_in_class(i)
            end if
            do while(j < num_class)
                if (classi(k,1) == i) then
                    if (c /= k) then
                        class_help = classi(c,:)
                        classi(c,:) = classi(k,:)
                        classi(k,:) = class_help
                    end if
                    j = j + 1
                    c = c + 1
                end if
                k = k + 1
            end do
            k = c
        end do

    end subroutine sort_class

    subroutine distance_matrix( xA, dimA, dimV, xB, dimB, D, iopt )
      !------------------------------------------------------------
      ! YS, 12 Jul 2024
      ! Purpose: compute distance maxtric xA(dimA) times xB(dimB)^T
      !                                   xA(1   )
      !                                   xA(....)     times ( xB(1), ..., xB(dimB) )
      !                                   xA(dimA)
      ! Input:
      ! xA(dimA, dimV) input position vector of length dimA; dimV is the dimension of the vector, i.e., xA(:,1) = x(:), xA(:,2)=y(:), xA(:,3)=z(:),...
      ! xB(dimB, dimV) 
      ! iopt: option 1 = Euclidean x in [km]
      !       option 2 = Distance on a globe x in deg° (not radian), ONLY for dimV = 2, xA(:,1) = lon, xA(:,2) = lat, both in degree
      ! Output:
      ! D(dimA, dimB) output distance matrix in [km]
      !-------------------------------------------------------------
              integer                                        :: dimA, dimB, dimV
              real(8), dimension(dimA, dimV), intent(in)     :: xA
              real(8), dimension(dimB, dimV), intent(in)     :: xB
              real(8), dimension(dimA, dimB), intent(out)    :: D           ! dimA rows, dimB columns
              integer :: i, j, k, iopt
              real(8) :: dxAB, dyAB, dlAB, dpAB
              real(8), parameter :: pi      = 3.14159265358979

              if ( iopt == 1 ) then
                do i = 1, dimA 
                  do j = 1, dimB
                    dxAB = 0.
                    do k = 1, dimV
                      dxAB = dxAB + ( xA(i,k) - xB(j,k) )**2
                    enddo
                    D(i,j) = sqrt( dxAB )
                  enddo
                enddo	
              elseif ( iopt == 2 ) then 
                do i = 1, dimA
                  do j = 1, dimB
                    dlAB = xA(i,1) - xB(j,1)                              ! d lambda   
                    dpAB = xA(i,2) - xB(j,2)                              ! d phi
                    dxAB = 111.3*dlAB*cos(dpAB*pi/180)                    ! dx km, 111.3 deg to km conversion factor
                    dyAB = 111.3*dpAB                                     ! dy km
                    D(i,j) = sqrt( dxAB**2 + dyAB**2 )
                  enddo
                enddo
              endif

              return
    end subroutine distance_matrix
!
    subroutine creation_matrix( rpA, dimA, rpB, dimB, D, rpAB, iopt )
      !------------------------------------------------------------
      ! YS, 12 Jul 2024
      ! Purpose: compute creation natrix rpAB(dimA, dimB)
      ! If a human with creation potential rpA is sufficiently close 
      ! to a human with creation potential rpB. They will have the 
      ! creation potential rpAB
      !
      ! Input:
      ! rpA(dimA) input creation potential of dimA humans
      ! rpB(dimB) input creation potential of dimB humans
      ! D(dimA, dimB) input distance matrix in [km] between humans
      ! iopt, option 1, rpA and rpB are for same humans
      !       option 2, rpA and rpB are for different humans
      ! Output:
      ! rpAB: creation matrix
      !-------------------------------------------------------------
              integer                                     :: dimA, dimB
              real(8), dimension(dimA), intent(in)        :: rpA
              real(8), dimension(dimB), intent(in)        :: rpB
              real(8), dimension(dimA, dimB), intent(in)  :: D                ! dimA rows, dimB columns
              real(8), dimension(dimA, dimB), intent(out) :: rpAB
              integer                                     :: i, j, iopt 
              real, parameter                             :: D_scale = 100.   ! scale distance for mating [km]
      !
              rpAB(:,:) = 0.

              if ( iopt == 1 ) then 
                do i = 1, dimA
                  do j = 1, dimB
                    if ( i == j ) then 
                      cycle
                    endif 
                    if ( (rpA(i) .gt. 0.) .and. (rpB(j) .gt. 0.) ) then
                      rpAB(i,j) = rpA(i)*rpB(j)*exp( -D(i,j)/D_scale )
                    endif
                  enddo
                enddo
              elseif ( iopt == 2 ) then
                do i = 1, dimA
                  do j = 1, dimB
                    if ( (rpA(i) .gt. 0.) .and. (rpB(j) .gt. 0.) ) then
                      rpAB(i,j) = rpA(i)*rpB(j)*exp( -D(i,j)/D_scale )
                    endif
                  enddo
                enddo
              endif

              return
    end subroutine creation_matrix

    subroutine dbscan_cluster( x, obs, var, minpts, eps, classi, p_in_classi, cluster_count )

        !*****************************************************************************************
        ! Dbscan cluster to organize data and filter noise
        !
        ! Algorithm is based on Ester et al. (1996): A Density-Based Algorithm for Discovering Clusters
        !
        !
        ! Input, real, x(obs, var) : input array of observations
        !
        ! Input, integer, obs : amount of observations
        !
        ! Input, integer, var : amount of variables
        !
        ! Input, real, eps : maximum distance between two samples for one to be considered as in the neighborhood of the other
        !
        ! Input, integer, minpts : minimum amount of points in an eps-neighborhood to define a cluster
        !
        !
        ! Output, integer, classi(obs) : classification of each observation to a cluster (noise = 0)
        !
        ! Output, integer, p_in_classi(obs) : number of points per classification, noise at p_in_classi(obs)
        !
        !***************************************************************************************

        implicit none

        integer(kind=4), intent(in) :: obs
        integer(kind=4), intent(in) :: var

        real(8), intent(in)    :: eps
        integer(kind=4), intent(in) :: minpts

        real(8), dimension(:,:),intent(in) :: x(obs, var)
        integer, dimension(:), intent(out) :: classi(obs)   ! unclassified = -1, noise = 0, clssification = integer
        integer, dimension(:), intent(out) :: p_in_classi(obs)
        real(8), dimension(:,:), allocatable :: distm

        integer, intent(out) :: cluster_count
        integer, dimension(:), allocatable :: seeds, seeds_of_seed
        integer :: seed_count, seed_of_seed_count
        integer :: i, j, k, l, m

        cluster_count = 1
        allocate(distm(obs,obs), seeds(obs), seeds_of_seed(obs))
        call distance_matrix(x, obs, var, x, obs, distm, 2)

        where (distm <= eps)
            distm = 1
        elsewhere
            distm = 0
        end where

        classi(:) = -1
        p_in_classi(:) = 0

        do i = 1, obs
            if (classi(i) == -1) then
                seed_count = 0
                seeds(:) = 0
                do j = 1, obs
                    if (distm(i,j) == 1) then
                        seed_count = seed_count + 1
                        seeds(seed_count) = j
                    end if
                end do
                if (seed_count < minpts) then
                    classi(i) = 0  ! Noise
                else
                    classi(seeds(1:seed_count)) = cluster_count

                    where (seeds == i) seeds = 0
                    do k = 1, seed_count
                        if (seeds(k) /= 0) then
                            seed_of_seed_count = 0
                            seeds_of_seed(:) = 0
                            do l = 1, obs
                                if (distm(seeds(k),l) == 1) then
                                    seed_of_seed_count = seed_of_seed_count + 1
                                    seeds_of_seed(seed_of_seed_count) = l
                                end if
                            end do

                            if (seed_of_seed_count >= minpts) then
                                do m = 1, seed_of_seed_count
                                    if ((classi(seeds_of_seed(m)) == -1) .or. (classi(seeds_of_seed(m)) == 0)) then
                                        if (classi(seeds_of_seed(m)) == -1) then
                                            seed_count = seed_count + 1
                                            seeds(seed_count) = seeds_of_seed(m)
                                        end if
                                        classi(seeds_of_seed(m)) = cluster_count
                                    end if
                                end do
                            end if
                            seeds(k) = 0
                        end if
                    end do
                    cluster_count = cluster_count + 1
                end if
            end if
        end do
        cluster_count = cluster_count - 1

        do i = 1, obs
            if (classi(i) == 0) then
                p_in_classi(obs) = p_in_classi(obs) + 1
            else
                p_in_classi(classi(i)) = p_in_classi(classi(i)) + 1
            end if
        end do

        return
    end subroutine dbscan_cluster

!    subroutine nc_write(path2file, varname, vardata, name_dim1, data_dim1, name_dim2, data_dim2,  name_dim3, data_dim3,&
!       name_dim4, data_dim4)
!
!      !use mod_functions
!      use mod_setup
!      use netcdf
!      !this subroutine saves a chosen variable to a nc-file
!      !path2file : filepath
!      !narmane: character variablename which is used in the nc-file
!      !dimnames: array containing names of dimension as characters
!      !vardata: real type data of the variable
!      !names for dimensions: characters, optional
!  implicit none
!
!  character (len = *), intent(in) :: path2file
!  character (len = *), intent(in) :: varname
!  real(kind=8), dimension(:,:), intent(in) :: vardata
!  character (len = *), intent(in), optional :: name_dim1
!  character (len = *), intent(in), optional :: name_dim2
!  character (len = *), intent(in), optional :: name_dim3
!  character (len = *), intent(in), optional :: name_dim4
!  character (len = 64) :: name_dim_1_used = "dim1"
!  character (len = 64) :: name_dim_2_used = "dim2"
!  character (len = 64) :: name_dim_3_used = "dim3"
!  character (len = 64) :: name_dim_4_used = "dim4"
!  real, dimension(:), intent(in), optional :: data_dim1, data_dim2, data_dim3, data_dim4
!  !character (len = 64) :: name_dim_1_used = "dim1"
!  !character (len = *), parameter :: name_dim_2_used! = "dim2"
!  !character (len = *), parameter :: name_dim_3_used! = "dim3"
!  !character (len = *), parameter :: name_dim_4_used! = "dim4"
!
!  ! Declaration of variables for netcdf
!  integer :: nc_id_out
!  integer :: var_id_out
!
!  !real, dimension(3,3) :: a = RESHAPE((/1,2,3,4,5,6,7,8,9/), (/3,3/))
!  integer :: i, j, size_dims, dim1_id, dim2_id, dim3_id, dim4_id
!  integer :: dim1_var_id, dim2_var_id, dim3_var_id, dim4_var_id
!  integer, dimension(:), allocatable :: shape_data, start_inds, count, dim_ids
!  real,  dimension(:), allocatable ::  dim1, dim2, dim3, dim4
!
!
!  !use given dim-names, if we have them
!if (present(name_dim1)) then
!  name_dim_1_used = name_dim1
!end if
!
!if (present(name_dim2)) then
!  name_dim_2_used = name_dim2
!end if
!
!if (present(name_dim3)) then
!  name_dim_3_used = name_dim3
!end if
!
!if (present(name_dim4)) then
!  name_dim_4_used = name_dim4
!end if
!
!
!
!  shape_data = SHAPE(vardata)
!  size_dims = SIZE(shape_data)
!
!  allocate(start_inds(size_dims))
!  allocate(count(size_dims))
!  allocate(dim_ids(size_dims))
!
!  start_inds = 1!all dimensions start at index 1
!  count = shape_data
!
!  ! Output file:
!          call check(nf90_create(path2file, NF90_NETCDF4, nc_id_out), 2, "create", "path_output_pos")
!
!              do i = 1,size_dims
!                      if (i == 1) then
!                              allocate(dim1(shape_data(i)))
!                                      if (present(data_dim1)) then ! use given dim-data, if we have them:
!                                            dim1 = data_dim1
!                                      else
!                                            dim1 = (/(j, j=1,shape_data(i), 1)/) !array with entries 1 to shape_data(i)
!                                      end if
!                              call check(nf90_def_dim(nc_id_out, name_dim_1_used, shape_data(i), dim1_id), 2, "def_dim", "dim1")
!                              call check(nf90_def_var(nc_id_out, name_dim_1_used, NF90_DOUBLE, dim1_id, dim1_var_id),&
!                                               2, "def_var", name_dim_1_used)
!                              call check(nf90_put_var(nc_id_out, dim1_var_id, dim1, start=(/1/), count=(/shape_data(i)/)),&
!                                               3, "put_var", "variable_dim1")
!                      else if (i == 2) then
!                              allocate(dim2(shape_data(i)))
!                                      if (present(data_dim2)) then ! use given dim-data, if we have them:
!                                              dim2 = data_dim2
!                                      else
!                                              dim2 = (/(j, j=1,shape_data(i), 1)/)
!                                      end if
!                              call check(nf90_def_dim(nc_id_out, name_dim_2_used, shape_data(i), dim2_id),&
!                                               2, "def_dim", "dim2")
!                              call check(nf90_def_var(nc_id_out, name_dim_2_used, NF90_DOUBLE, dim2_id, dim2_var_id),&
!                                               2, "def_var", name_dim_2_used)
!                              call check(nf90_put_var(nc_id_out, dim2_var_id, dim2, start=(/1/), count=(/shape_data(i)/)),&
!                                               3, "put_var", "variable_dim2")
!                      else if (i == 3) then
!                              allocate(dim3(shape_data(i)))
!                                       if (present(data_dim3)) then ! use given dim-data, if we have them:
!                                              dim3 = data_dim3
!                                       else
!                                              dim3 = (/(j, j=1,shape_data(i), 1)/)
!                                       end if
!                              call check(nf90_def_dim(nc_id_out, name_dim_3_used, shape_data(i), dim3_id), 2, "def_dim", "dim3")
!                              call check(nf90_def_var(nc_id_out, name_dim_3_used, NF90_DOUBLE, dim3_id, dim3_var_id),&
!                                              2, "def_var", name_dim_3_used)
!                              call check(nf90_put_var(nc_id_out, dim3_var_id, dim3, start=(/1/), count=(/shape_data(i)/)),&
!                                              3, "put_var", "variable_dim3")
!                      else if (i == 4) then
!                              allocate(dim4(shape_data(i)))
!                                      if (present(data_dim4)) then ! use given dim-data, if we have them:
!                                              dim4 = data_dim4
!                                      else
!                                              dim4 = (/(j, j=1,shape_data(i), 1)/)
!                                      end if
!                              call check(nf90_def_dim(nc_id_out, name_dim_4_used, shape_data(i), dim4_id), 2, "def_dim", "dim4")
!                              call check(nf90_def_var(nc_id_out, name_dim_4_used, NF90_DOUBLE, dim4_id, dim4_var_id),&
!                                              2, "def_var", name_dim_4_used)
!                              call check(nf90_put_var(nc_id_out, dim4_var_id, dim4, start=(/1/), count=(/shape_data(i)/)),&
!                                              3, "put_var", "variable_dim4")
!                      !else if (i == 5) then
!                      !        allocate(dim5(shape_data(i)))
!                      !        dim5 = (/(j, j=1,shape_data(i), 1)/)
!                      end if
!              end do
!
!          if (size_dims == 1) then
!              dim_ids = dim1_id
!              start_inds = 1
!          elseif (size_dims == 2) then
!              dim_ids = (/dim1_id, dim2_id/)
!              start_inds = (/1, 1/)
!          elseif (size_dims == 3) then
!              dim_ids = (/dim1_id, dim2_id, dim3_id/)
!              start_inds = (/1, 1, 1/)
!          elseif (size_dims == 4) then
!              dim_ids = (/dim1_id, dim2_id, dim3_id, dim4_id/)
!              start_inds = (/1, 1, 1, 1/)
!          end if
!
!          call check(nf90_def_var(nc_id_out, varname, NF90_DOUBLE, dim_ids, var_id_out)&
!                  , 2, "def_var", name_dim_1_used)
!
!          call check(nf90_enddef(nc_id_out),2,"enddef")
!          call check(nf90_put_var(nc_id_out, var_id_out, vardata, start=start_inds, count=count),3, "put_var", "test_variable")
!          !CALL nf90_put_var(nc_id_out, var_id_out, vardata)
!
!          call check(nf90_close(nc_id_out), 4, "close")
!          print *, "file: ", path2file, " created"
!
!  end subroutine

  SUBROUTINE DoStats(x, sum, avg, var, sdv)
        implicit none
        REAL(8), DIMENSION(:), INTENT(IN) :: x
        REAL(8), INTENT(OUT) :: sum
        REAL(8), INTENT(OUT) :: avg, var, sdv
        INTEGER              :: n, i
     
        sum = 0.
        var = 0.
 
        n = size( x )
        DO i = 1, n
            sum = sum + x(i)
        ENDDO

        avg = sum/n

        DO i = 1, n
            var = var + x(i)*x(i)
        END DO
        var = var/n - avg**2.
        sdv = sqrt( var )
  END SUBROUTINE

  subroutine smooth2d (x_av, p, q)
      !
      ! x_av and x are of the same size, x_av = hep_av, x = hep. Purpose is to smooth hep_av
      ! Two scalars p and q affect degree of smoothing. In general, for good results, set p = 0.50. 
      ! With p = 0.50, a q = -0.25 results in "light" smoothing and q = 0.25 "heavy" smoothing. 
      ! A q = 0.0 results in a 5-point local smoother.
      !
            implicit none
            real(8), dimension(:,:) :: x_av
            real(8), dimension(:,:), allocatable :: f, x
            real(8) :: p, q
            integer, dimension(2) :: shape_x
            integer :: dlon, dlat, dlonp1, dlatp1
            integer :: i, j

            shape_x = shape(x_av)
            dlon = shape_x(1)
            dlat = shape_x(2)

      !      dlonp1 = dlon + 1
      !      dlatp1 = dlat + 1
      !      allocate( f(0:dlonp1, 0:dlatp1) )
      !      f(0,     1:dlat) = x_av(1,     1:dlat)
      !      f(dlonp1,1:dlat) = x_av(dlon,  1:dlat)
      !      f(1:dlon,0)      = x_av(1:dlon,1)
      !      f(1:dlon,dlatp1) = x_av(1:dlon,dlat)
      !      f(0, 0)          = f(1, 0)
      !      f(0,dlatp1)      = f(1,dlatp1)
      !      f(dlonp1,0)      = f(dlon,0)
      !      f(dlonp1,dlatp1) = f(dlon,dlatp1)

            allocate( f(dlon, dlat), x(dlon, dlat) )
            f = x_av
            x = x_av

            where ( f .le. 0. )   ! set ocean values to 0. smoothing near coast
              f = 0.
            endwhere

            do i = 2, dlon-1
            do j = 2, dlat-1
              x_av(i,j) = f(i,j) + p*( f(i+1,j) + f(i,j+1) + f(i-1,j) + f(i,j-1) - 4.*f(i,j) )     &
          &                     + q*( f(i+1,j+1) + f(i-1,j+1) + f(i-1,j-1) + f(i+1,j-1) - 4.*f(i,j) )
            enddo
            enddo

            where ( x <= 0. )
              x_av = x
            endwhere

  end subroutine smooth2d

END MODULE mod_utility
