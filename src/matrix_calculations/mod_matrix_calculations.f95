!--------------------------------------------------------------- 
! Yaping Shao, 1 Jul 2024
! 
! N-population Human System Model
! 
! (1) New birth-death model: to 
! mod_functions.f95 - main modules and functions
! mod_utility.f95   - utility modules, Gaussian random noise
! mod_kinds.f95     - data type configuration
! omp_lib.f95       - library for parallel processing
!---------------------------------------------------------------

module mod_matrix_calculations

    use mod_globals
    ! in common_vars.inc:
    ! Uses:   - t_hep, hep
    !         - lon0, lat0, delta_lon, delta_lat
    !         - hum_t


    ! in parameters.inc:
    ! Uses:   - cb2, cb3


    use mod_utility                       ! various utility functions 
    !INCLUDES: 
      ! mod_kinds
    use mod_functions                     ! more functions  includes also mod_utility ??? why nested inclusion of files?
    !INCLUDES: 
      ! mod_kinds
      ! mod_utility
      ! mod_rnorm
      ! omp_lib
    use mod_setup_hep                         ! handels initialization, reading input data and defining ouput paths


    use mod_rnorm, only: dp, rnorm_vec     ! random number generator (gaussian)
    use netcdf                             ! netcdf library
    use omp_lib                            ! openmp library for parallel processing   






    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Declaration of model variables and parameters
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    implicit none
    integer                                         :: IERR, IX                  ! random seed
    integer                                         :: i, j, c
    integer, dimension(npops)                       :: pop_dens_adj
    integer, dimension(2)                           :: g

    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    integer, dimension(8) :: date_values1, date_values2
!---------------------------------
! YS, 2 Jul 2024, added variables
!---------------------------------
    integer :: n, jb, je
    real(8), dimension(:), allocatable             :: wkx, wky, wku, wkv
    real(8), dimension(:,:), allocatable           :: wkdens
    real(8), dimension(:,:,:), allocatable         :: wkflow
                                                                                !    real(8) :: hepNE, hepE, hepSE, hepN, hepC, hepS, hepNW, hepW, hepSW
  

    real(8) :: xic, yic, sip, siu
    integer :: jhum_0, nh
    integer :: ilon, ilat                                                       ! local working variable
    integer :: itimes = 0                                                       ! how many times birth module has been called

    real(8) :: rd                                                               ! a random number (uniform dist)
    logical :: protcl, exists                                                   ! protocol and file-exist

    real(8) :: s_sum, s_avg, s_var, s_sdv                                       ! some working variables for statistics, not influencing model 
    real(8), parameter :: pw = 0.5, qw = -0.25                                  ! smoothing factor, default weak smoothing, pw=0.5, qw=-0.25
    integer :: io

    real(8) :: sqdt





contains

subroutine allocate_memory_and_open_files()
    sqdt = sqrt( dt )    

  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Setup before calculation
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      call date_and_time(date,time,zone,date_values1) ! YS: fortran prog. get date/time from real-time system clock
                                                      ! All output, DATE in ccyymmdd; TIME hhmmss.sss; ZONE +-hhmm
                                                      ! VALUES(1): year; (2): month; (3): day; (4): time difference from UTC in min
                                                      ! VALUES(5): hour; (6): min;   (7):	sec; (8): millisec
      print *, "before setup_load"
      call setup_load()                               ! sub in mod_setup
      print *, "after setup_load"

      print *, "main: date, time, zone", date, time, zone
      print *, "Now we open hes.prt"

      open(11, file="bin/hes.prt")
      read(11, *)
      read(11, *)
      read(11, *)
      read(11, *) 
      read(11, *) rho_max(1), rho_max(2), rho_max(3)
      read(11, *) r_B(1),     r_B(2),     r_B(3)
      read(11, *) d_B(1),     d_B(2),     d_B(3)
      read(11, *) tau(1),     tau(2),     tau(3)
      read(11, *) sigma_u(1), sigma_u(2), sigma_u(3)
      read(11, *) cb1(1),     cb1(2),     cb1(3)
      read(11, *) protcl 

      ix = iseed


  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! The following used to be inside a .inc file (maybe do that again)
    do i = 1, 3
      rgauss = rnorm_vec(100, rho_max(i), 0.1*rho_max(i))
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then 
          rgauss(j) = rho_max(i) 
        endif
      enddo
      rho_max(i) = rgauss(50)

      rgauss = rnorm_vec(100, r_B(i),     0.1*r_B(i)    )
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = r_B(i)
        endif
      enddo
      r_B(i) = rgauss(50)

      rgauss = rnorm_vec(100, d_B(i),     0.1*d_B(i)    )
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = d_B(i)
        endif
      enddo
      d_B(i) = rgauss(50)

      rgauss = rnorm_vec(100, sigma_u(i), 0.1*sigma_u(i))
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = sigma_u(i)
        endif
      enddo
      sigma_u(i) = rgauss(50)

      rgauss = rnorm_vec(100, cb1(i),     0.1*cb1(i)    )
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = cb1(i)
        endif
      enddo
      cb1(i) = rgauss(50)
    enddo

    inquire(file="log.txt", exist=exists)
    if (exists) then
      open(newunit=io, file="log.txt", position="append", status="old", action="write")
    elseif(exists .eqv. .false.) then 
      open(file="log.txt", newunit=io, status="new")
    end if

    write(io, *) "Run 3220020001avD15cb1250_"//random_seed
    write(io, *) "Actual parameters used"
    write(io, *) "rho_max(1), rho_max(2), rho_max(3)"
    write(io, *)  rho_max(1), rho_max(2), rho_max(3)
    write(io, *) "r_B(1),     r_B(2),     r_B(3)"
    write(io, *) r_B(1),     r_B(2),     r_B(3)
    write(io, *) "d_B(1),     d_B(2),     d_B(3)"
    write(io, *) d_B(1),     d_B(2),     d_B(3)
    write(io, *) "tau(1),     tau(2),     tau(3)"
    write(io, *) tau(1),     tau(2),     tau(3)
    write(io, *) "sigma_u(1), sigma_u(2), sigma_u(3)"
    write(io, *) sigma_u(1), sigma_u(2), sigma_u(3)
    write(io, *) "cb1(1),     cb1(2),     cb1(3)"
    write(io, *) cb1(1),     cb1(2),     cb1(3)
    close(io)


  ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      hum_max_A = 0
      do jp = 1, npops
        hum_max_A = max(hum_max_A, hum_max(jp))       ! max of all populations
      enddo
      !......................................................
      if ( protcl ) write(9, *) "main: hum_max_A = ", hum_max_A
      !......................................................    

      print *, "Now we allocate the memory for the arrays"

      ! allocate memory for is_dead and is_dead0 DN 10.06.2025

      allocate(is_dead(hum_max_A, npops))
      allocate(is_dead0(hum_max_A, npops))


      allocate(Ax(hum_max_A), Ay(hum_max_A))          

      allocate(x0(hum_max_A, npops), y0(hum_max_A, npops))
      allocate(ux0(hum_max_A, npops), uy0(hum_max_A, npops))
      allocate(hum_id_0(hum_max_A, npops)) 
      allocate(x(hum_max_A, npops), y(hum_max_A, npops))
      allocate(ux(hum_max_A, npops), uy(hum_max_A, npops))
      allocate(hum_id(hum_max_A, npops))

      allocate(hep_av  (dlon_hep, dlat_hep, npops))
      allocate(dens    (dlon_hep, dlat_hep, npops))
      allocate(idens   (dlon_hep, dlat_hep, npops))
      allocate(dens_adj(dlon_hep, dlat_hep, npops))
      allocate(pop_pressure(dlon_hep, dlat_hep, npops))
      allocate(distm(dlon_hep, dlat_hep, npops)) 
      allocate(flow(2, dlon_hep, dlat_hep, npops))        
  !
      allocate(dens_acc(dlon_hep, dlat_hep, npops))   ! dens and flow average over output time interval
      allocate(flow_acc(2, dlon_hep, dlat_hep, npops))
      dens_acc = 0.d0
      flow_acc = 0.d0
  !
      allocate(wkdens(dlon_hep, dlat_hep)       )     ! work files related to dens
      allocate(wkflow(2, dlon_hep, dlat_hep)    )
  !
      allocate(area_for_dens(dlon_hep, dlat_hep))     ! cell area, same for all npops
  !
  ! HEP boundary conditions and maximal gradient of HEP
  !
      where ( (hep < 0) .or. (hep /= hep) )          ! case hep < 0 or hep = NaN, set the value to water_hep = -10 
          hep = water_hep
      end where
      where ( (hep > 0) .and. (hep < 0.01) )         ! avoid very small positive hep
          hep = 0.01
      end where

      delta_lon = lon_hep(2)-lon_hep(1)
      delta_lat = lat_hep(2)-lat_hep(1)
      lon_0     = lon_hep(1) - 0.5*delta_lon
      lat_0     = lat_hep(1) - 0.5*delta_lat
  !
  ! set boundary to be out boundary of the simulation domain
  ! this is what should happen, but ....
  !
  !*    lon_min_out = lon_hep(1) - 0.5*delta_lon 
  !*    lon_max_out = lon_hep(dlon_hep) + 0.5*delta_lon
  !*    lat_min_out = lat_hep(1) - 0.5*delta_lat
  !*    lat_max_out = lat_hep(dlat_hep) + 0.5*delta_lat
  !
  ! it is coded in computing gradient and density only for gx = 2, dlon-1 and gy = 2, dlat-1
  ! it is more consistent to set the boundary to smaller
  !
      lon_min_out = lon_hep(1) + 0.5*delta_lon
      lon_max_out = lon_hep(dlon_hep) - 0.5*delta_lon
      lat_min_out = lat_hep(1) + 0.5*delta_lat
      lat_max_out = lat_hep(dlat_hep) - 0.5*delta_lat
  !
  ! do not like adj, improve the code by deleting all related to adj, replace with 9-point spatial smoothing if needed, 
  !
      pop_dens_adj(:) = sigma_u(:) / 2 / (delta_lat * deg_km)

      call calc_area_gridcell(lon_hep, lat_hep, R, area_for_dens)
  !......................................................
  !    if ( protcl ) then 
  !       write(9, *) "main: delta_lon = ", delta_lon
  !       write(9, *) "main: delta_lat = ", delta_lat
  !       write(9, *) "main: pop_dens_adj = ", pop_dens_adj
  !       write(9, *) "main: lon_hep = ", lon_hep
  !       write(9, *) "main: lat_hep = ", lat_hep
  !       write(9, *) "main: R = ",       R
  !       write(9, *) "main:, area_for_dens = ", area_for_dens
  !    endif
  !......................................................
  !
      drown_count(:) = 0
      out_count  (:) = 0
      death_count(:) = 0
      birth_count(:) = 0
      hum_count  (:) = 0 
      amount_cluster(:) = 0

  !    call OMP_set_num_threads(6)
end subroutine allocate_memory_and_open_files

subroutine setup_initial_conditions()
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Setup initial conditions
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      x(:,:)  = -1.0E3 
      y(:,:)  = -1.0E3
      ux(:,:) = 0.
      uy(:,:) = 0.

      !---------- added 10.06.25 by DN ----------------------
      is_dead(:,:) = .true. ! initialize the is_dead array to true for all agents 
      !------------------------------------------------------
  !
  ! Loop through npops
  !
      pops0: do jp = 1, npops                     ! loop through npops
    
        jhum_0 = 0
        sources: do n = 1, ns                     ! loop through nsources
          nh  = hum_0    (n, jp)
          if ( nh .ne. 0 ) then
            xic = x_ini_c  (n, jp)
            yic = y_ini_c  (n, jp)
            sip = ini_spread(n, jp)
            siu = sigma_u(jp)

            allocate ( wkx(nh), wky(nh), wku(nh), wkv(nh) )

            call strt_distr_gaussian(nh, xic, yic, sip, wkx, wky, siu, wku, wkv) ! YS 29072024 tested ok
                                                                                ! do j=1,n
                                                                                ! write(18,*)xic,yic,sip,wkx(j),wky(j)
                                                                                ! enddo
            do j = 1, nh
              jhum_0 = jhum_0 + 1
              x0(jhum_0, jp)  = wkx(j)
              y0(jhum_0, jp)  = wky(j)
              ux0(jhum_0, jp) = wku(j)
              uy0(jhum_0, jp) = wkv(j)
              hum_id_0(jhum_0, jp) = jhum_0
              !---------- added 10.06.25 by DN ----------------------
              is_dead(jhum_0,jp) = .false. ! set is_dead to false for agents that are alive 
                                           ! maybe here we could use a "spawn_agent_function"
              !------------------------------------------------------
            enddo
          
            deallocate ( wkx, wky, wku, wkv )
          endif
        enddo sources 
  
        x(1:jhum_0, jp) = x0(1:jhum_0, jp)
        y(1:jhum_0, jp) = y0(1:jhum_0, jp)
        hum_id(1:jhum_0, jp) = hum_id_0(1:jhum_0, jp)
  !
  ! Initial population density and available HEP
  !
        call pop_dens_flow_func(x0(:,jp), y0(:,jp), ux0(:,jp), uy0(:,jp),          &
    &                        lon_hep, lat_hep, area_for_dens,                     &
    &                        hep(:,:,jp,1), water_hep,                            &
    &                        pop_dens_adj(jp), dens_adj(:,:,jp), dens(:,:,jp), idens(:,:,jp), flow(:,:,:,jp))
  !
        if (with_pop_pressure .eqv. .true.) then

          wkdens(:,:) = dens(:,:,jp)
          call smooth2d( wkdens, pw, qw )
          call pop_pressure_func(wkdens,           hep(:,:,jp,1),rho_max(jp),eta(jp),epsilon(jp),pop_pressure(:,:,jp))
  !        call pop_pressure_func(dens_adj(:,:,jp),hep(:,:,jp,1),rho_max(jp),eta(jp),epsilon(jp),pop_pressure(:,:,jp))
          hep_av(:,:,jp) = pop_pressure(:,:,jp)*hep(:,:,jp,1)                      ! hep_av [ ], dimensionless, over water = hep
        else 
          hep_av(:,:,jp) =                      hep(:,:,jp,1)                      ! no pop_pressure 
        endif
  !
  ! save initial condition at t = 0
  !
      if ( save_out .eqv. .true. ) then 
        call check(nf90_put_var(nc_id_out_pos(jp), var_pos_hum_id_out(jp), x(:,jp),     &
      &                        start=(/1, 1, 1/), count=(/1, hum_max(jp), 1/)), 3, "put_var", "pos_hum")
        call check(nf90_put_var(nc_id_out_pos(jp), var_pos_hum_id_out(jp), y(:,jp),     &
      &                        start=(/2, 1, 1/), count=(/1, hum_max(jp), 1/)), 3, "put_var", "pos_hum")

  !    call check(nf90_put_var(nc_id_out_pos, var_velo_id_out, ux, start=(/1, 1, 1/), count=(/1, hum_max, 1/))&          ! YS 17Dec2024, have not done
  !                     , 3, "put_var", "velo")
  !    call check(nf90_put_var(nc_id_out_pos, var_velo_id_out, uy, start=(/2, 1, 1/), count=(/1, hum_max, 1/))&
  !                     , 3, "put_var", "velo")
  !    call check(nf90_put_var(nc_id_out_pos, var_hum_id_id_out, hum_id, start=(/1, 1/), count=(/hum_max, 1/))&
  !                     , 3, "put_var", "hum_id")

          wkdens(:,:) = dens(:,:,jp)
          call check(nf90_put_var(nc_id_out_dens(jp), var_dens_id_out(jp), wkdens, start=(/1, 1, 1/),    &
      &             count=(/dlon_hep, dlat_hep, 1/)), 3, "put_var", "dens")
          wkflow(:,:,:) = flow(:,:,:,jp)
          call check(nf90_put_var(nc_id_out_dens(jp), var_flow_id_out(jp), wkflow, start=(/1, 1, 1, 1/), &
      &             count=(/2, dlon_hep, dlat_hep, 1/)), 3, "put_var", "flow")
        endif

        hum_t(jp) = sum( hum_0(:, jp ) )
  !.....................................................................
        if ( protcl ) then
          write(9, *) "initial number of humans for pop:", jp, hum_t(jp)
        endif
  !....................................................................
  !--------------------------------------------------------------------
  ! YS, 2 Jul 2024: do Langevin equation coefficients outside the loops
  !--------------------------------------------------------------------
        cb1(jp) = dt*cb1(jp)
        cb2(jp) = dt/tau(jp)
        cb3(jp) = sqrt( sigma_u(jp)**2/tau(jp) )          ! sqrt( sigma_u**2/tau )

      enddo pops0
  !....................................................................
  ! ys 29jul2024 seems to be ok
  !    if ( protcl ) then 
  !      do jp = 1, npops
  !        write(9, *) "Initialization done jp = ", jp
  !        write(9, *) "number of humans       = ", hum_t(jp)
  !        write(9, *) "x positions: ", x(1:hum_t(jp), jp) 
  !        write(9, *) "y positions: ", y(1:hum_t(jp), jp)
  !        write(9, *) "hum_id     : ", hum_id(1:hum_t(jp), jp) 
  !        write(9, *) "dens       : "
  !        do ilat = 1, dlat_hep
  !          write(9, *) (int(dens(ilon,ilat,jp)*100), ilon=1, dlon_hep)
  !        enddo
  !        write(9, *) "pop_pressure "
  !        do ilat = 1, dlat_hep
  !          write(9, *) (real(pop_pressure(ilon, ilat, jp)), ilon = 1, dlon_hep)
  !        enddo
  !        write(9, *) "hep          "
  !        do ilat = 1, dlat_hep
  !          write(9, *) (real(hep(ilon,ilat, jp, 1)), ilon = 1, dlon_hep)
  !        enddo
  !        write(9, *) " hep_av      "
  !        do ilat = 1, dlat_hep
  !          write(9, *) (real(hep_av(ilon, ilat, jp)), ilon = 1, dlon_hep)
  !        enddo
  !      enddo
  !    endif
  !....................................................................
end subroutine setup_initial_conditions


! {:

      ! before the population loop: 



      ! inside the population loop

          ! before the human loop


          ! inside the human loop



            ! contains: 





          ! after the human loop
          subroutine update_hep_human_density(jp)
            integer :: jp
                      !      !$OMP END DO
            !      !$OMP ATOMIC
            out_count(jp) = out_count(jp) + out_count_priv(jp)
            out_count_a(jp) = out_count_a(jp) + out_count_priv_a(jp)
            out_count_b(jp) = out_count_b(jp) + out_count_priv_b(jp)

            !      !$OMP ATOMIC
            drown_count(jp) = drown_count(jp) + drown_count_priv(jp)
            !      !$OMP END PARALLEL
            death_count(jp) = death_count(jp) + death_count_priv(jp)

            !!!        if ( jp .eq. 1 ) then
            !!!        print *, "t, jp, out_count(jp),   out_count_priv(jp)  ", t, jp, out_count(jp),   out_count_priv(jp)
            !!!        print *, "t, jp, drown_count(jp), drown_count_priv(jp)", t, jp, drown_count(jp), drown_count_priv(jp)
            !!!        print *, "t, jp, death_count(jp), death_count_priv(jp)", t, jp, death_count(jp), death_count_priv(jp)
            !!!        endif

                  ! Population density and new available HEP
            !        if (mod(t,10) == 0) then                                                                                    ! YS, CHECK mod(t,10) ISSUE 
            call pop_dens_flow_func(x(:,jp), y(:,jp), ux(:,jp), uy(:,jp), lon_hep, lat_hep, area_for_dens, &
                  &                        hep(:,:,jp,t_hep), water_hep, pop_dens_adj(jp), dens_adj(:,:,jp),      &
                  &                        dens(:,:,jp), idens(:,:,jp), flow(:,:,:,jp))

            if (with_pop_pressure .eqv. .true.) then

              wkdens(:,:) = dens(:,:,jp)
              call smooth2d( wkdens, pw, qw )
              call pop_pressure_func(wkdens,           hep(:,:,jp,t_hep),rho_max(jp),eta(jp),epsilon(jp),pop_pressure(:,:,jp))
              !            call pop_pressure_func(dens_adj(:,:,jp),hep(:,:,jp,t_hep),rho_max(jp),eta(jp),epsilon(jp),pop_pressure(:,:,jp))
              hep_av(:,:,jp) = pop_pressure(:,:,jp)*hep(:,:,jp,t_hep)
            else
              hep_av(:,:,jp) =                      hep(:,:,jp,t_hep)
            endif

          end subroutine update_hep_human_density




      !}

! }


subroutine safe_and_close_files()
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! End of calculation: closing files and conclusions
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if ( protcl ) then
        do jp = 1, npops
          write(*,*) 'Population', jp
          write(*,*) 'Total Amount of Humans:', hum_t(jp)
          write(*,*) 'Birth:', birth_count(jp)
          write(*,*) 'Death:', death_count(jp)
          write(*,*) 'Drowned:', drown_count(jp)
          write(*,*) 'Outside:', out_count(jp)
          write(*,*) 'Average amount of clusters:', amount_cluster*100/Tn
          write(*,*) 'Average amount of people per cluster:', avg_cluster*100/Tn
        enddo
      endif

      deallocate(Ax, Ay, x, y, x0, y0, ux, uy, ux0, uy0)

      do jp = 1, npops
        if ( save_out .eqv. .true. ) then
          call check(nf90_close(nc_id_out_pos(jp)), 4, "close")
          call check(nf90_close(nc_id_out_dens(jp)), 4, "close")
        endif
      enddo

      print *, "Finishing calculations"
      print *, "------------------------------------------"
      print *, "output files:"
      do jp = 1, npops
        if (save_out .eqv. .true.) then
          print *, path_out_pos(jp)
          print *, path_out_dens(jp)
        endif
      enddo
      print *, "------------------------------------------"

      call date_and_time(date,time,zone,date_values2)
      call calc_and_print_runtime(date_values1, date_values2)
end subroutine safe_and_close_files



end module mod_matrix_calculations



