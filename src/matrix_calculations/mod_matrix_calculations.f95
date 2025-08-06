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
    ! Includes: 
      ! netcdf
      ! mod_utility
    use mod_birth_death                   ! models birth and death of humans
    !INCLUDES: 
      ! mod_kinds
      ! mod_utility
      ! mod_rnorm
      ! omp_lib
    use mod_rnorm, only: dp, rnorm_vec     ! random number generator (gaussian)
    use netcdf                             ! netcdf library
    use omp_lib                            ! openmp library for parallel processing   

    !---------------------------- The Agent Class ----------------------------
    use mod_agent_class
    use mod_agent_matrix_merge

    ! ---------------------------- The Grid ----------------------------------

    use mod_grid


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Declaration of model variables and parameters
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    implicit none
    integer                                         :: IERR, IX                  ! random seed
    integer                                         :: t, t_hep
    integer                                         :: i, j, c
    real(8)                                         :: delta_lon, delta_lat, lon_0, lat_0
    integer, dimension(npops)                       :: hum_t                     ! number of humans in each population
    integer, dimension(npops)                       :: pop_dens_adj
    integer                                         :: gx, gy, gx1, gy1          ! position in grid
    integer, dimension(2)                           :: g
    real(8)                                         :: grad_x, grad_y, grad      ! gradient in x and y

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
    real(8), dimension(npops)                      :: cb2, cb3
                                                                                !    real(8) :: hepNE, hepE, hepSE, hepN, hepC, hepS, hepNW, hepW, hepSW
    real(8), dimension(0:8) :: heploc
    real(8) :: heploc_max
    integer, dimension(0:8) :: gxx, gyy
    integer :: iloc, il

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


    !! for debugging

        ! Added by DN for debugging

    integer, dimension(npops) :: out_count_priv_a
    integer, dimension(npops) :: out_count_priv_b

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

      include "ens_params.inc"     ! perturbing simulation parameters

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

subroutine setup_initial_conditions_nodes()
  type(Node), pointer :: current_agent

  integer :: i , j

  do while (associated(current_agent))
    i = current_agent%position_human
    j = current_agent%position_population
    current_agent%pos_x = x(i,j)
    current_agent%pos_y = y(i,j)
    current_agent%ux = ux(i,j)
    current_agent%uy = uy(i,j)    

    current_agent => current_agent%next 
  end do

end subroutine setup_initial_conditions_nodes

! {:

      ! before the population loop: 

      subroutine reset_old_help_vars(t)
        integer :: t
        t_hep = int( t/delta_t_hep ) + 1

        out_count_priv(:) = 0
        drown_count_priv(:) = 0
        death_count_priv(:) = 0
        out_count_priv_a(:) = 0
        out_count_priv_b(:) = 0

      end subroutine reset_old_help_vars

      ! inside the population loop

          ! before the human loop
          subroutine setup_update_human(jp)
            integer :: jp
            if ( mod(t, 1000) .eq. 0 ) print *, "main t, jp, hum_t, t_hep", t, jp, hum_t(jp), t_hep
            !print *, "main: update_population jp = ", jp ! more debugging, DN 28.05.25
            !
            ! Gaussian distributed random noise in u1 and u2 direction

            Ax = rnorm_vec(hum_max_A, 0.d0, sqdt)
            Ay = rnorm_vec(hum_max_A, 0.d0, sqdt)

            !        !$OMP PARALLEL PRIVATE(gx, gy, grad_x, grad_y) FIRSTPRIVATE(out_count_priv, drown_count_priv) &
            !        !$OMP& SHARED(out_count, drown_count)
            !        !$OMP DO

          end subroutine setup_update_human

          ! inside the human loop

                
          subroutine agent_move(i,jp)
              integer :: i, jp
              
              type(Node), pointer :: current_agent 
              real(8) :: new_x, new_y
              real(8) :: new_ux, new_uy

              real(8) :: old_x, old_y
              real(8) :: old_ux, old_uy

              integer :: grid_x, grid_y, grid_x_b, grid_y_b
              real :: gradient_x, gradient_y

              print*, "This function was disabled (cannot kill agents anymore). -agent_move"

              if (.not. allocated(population_agents_matrix)) then
                  print *, "agent_move: population_agents_matrix not associated"
                  return
              end if

              if (.not. associated(population_agents_matrix(i,jp)%node)) then
                  
                  print *, "agent_move: agent not associated", i, jp
                  return
              end if

              !print *, "we get here."
              current_agent => population_agents_matrix(i,jp)%node
              old_x = current_agent%pos_x
              old_y = current_agent%pos_y 
              old_ux = current_agent%ux
              old_uy = current_agent%uy

              !for debugging DN 16.07.
              if (.not. old_x == x0(i,jp) .or. .not. old_y == y0(i,jp)) then
                  print *, " old_x,y not equal to x0,yo, in agent_move"
              endif

              x0(i,jp) = old_x
              y0(i,jp) = old_y

              !print *, " we get here 2."
              if (current_agent%is_dead) then
                  return
              endif

              ! Check if a human left the research area, then counted as out
              if (.false. .eqv. in_research_area(old_x, old_y)) then
                  !print *, "agent_move: Agent left research area at position", old_x, old_y, "in population", jp
                  !call agent_die_from_matrix_calc(i,jp)
                  out_count_priv(jp) = out_count_priv(jp) + 1
              

                  return 
              endif


              ! I think this basically computes the grid position of the human in the HEP grid
              grid_x = floor( ( old_x - lon_0 ) / delta_lon ) + 1 
              grid_y = floor( ( old_y - lat_0 ) / delta_lat ) + 1

              ! Check if human above water, then counted as drowned            ! ys, do not like this, redo
              if (agent_above_water(grid_x,grid_y,jp,t_hep)) then
                  !print *, hep(grid_x, grid_y, jp, t_hep), " <= 0., agent drowned"
                  !print *, "agent_move: Agent drowned at position", old_x, old_y, "in population", jp
                  !call agent_die_from_matrix_calc(i,jp)
                  drown_count_priv(jp) = drown_count_priv(jp) + 1


                  return 
              endif

              !print *, "We get here 2.5"

              if ( grid_x == 1 .or. grid_x == dlon_hep .or. grid_y == 1 .or. grid_y == dlat_hep) then
                  ! DN : I dont exactly understand why we remove a agent if this is the case
                  !call agent_die_from_matrix_calc(i,jp)
                  print *, "agent_move: Agent at boundary, removed from simulation", i, jp
                  out_count_priv_a(jp) = out_count_priv_a(jp) + 1

                  return 

              end if

              call calculate_gradient(grid_x,grid_y,jp,gradient_x,gradient_y)


              new_ux = old_ux + cb1(jp)*gradient_x - old_ux*cb2(jp) + cb3(jp)*Ax(i)
              new_uy = old_uy + cb1(jp)*gradient_y - old_uy*cb2(jp) + cb3(jp)*Ay(i)

              new_x = old_x + new_ux / (deg_km * cos(old_y * deg_rad)) * dt
              new_y = old_y + new_uy / deg_km * dt

              
              call movement_at_boundary(old_x,old_y,old_ux,old_uy,new_x,new_y,new_ux,new_uy)

              grid_x_b = floor( ( new_x - lon_0 ) / delta_lon ) + 1
              grid_y_b = floor( ( new_y - lat_0 ) / delta_lat ) + 1
                            
                        

              if ((grid_x_b < 1) .or. (grid_x_b > dlon_hep) .or. (grid_y_b < 1) .or. (grid_y_b > dlat_hep)) then
                  !call agent_die_from_matrix_calc(i,jp)
                  print *, "count out three"
                  out_count_priv_b(jp) = out_count_priv_b(jp) + 1
          
                  return
              endif

              if ( hep(grid_x, grid_y, jp, t_hep) <= 0. ) then           ! need better reflection scheme later
                  new_x = old_x
                  new_y = old_y
                  new_ux = cb3(jp)*Ax(i)
                 new_uy = cb3(jp)*Ay(i)
              endif
                      
              !print *, " we get here 3."    

              !if (mod(current_agent%id,123) == 0) then
                  !print *, "agent_move: Agent ID:", current_agent%id, "Old Position:", old_x, old_y, "New Position:", new_x, new_y
                  !print *, "Agent Velocity:", old_ux, old_uy, "New Velocity:", new_ux, new_uy
              !endif

              current_agent%pos_x = new_x
              current_agent%pos_y = new_y

              current_agent%ux = new_ux
              current_agent%uy = new_uy
              x(i,jp) = new_x
              y(i,jp) = new_y
              ux(i,jp) = new_ux
              uy(i,jp) = new_uy



          end subroutine agent_move

            ! contains: 

                logical function in_research_area(pos_x,pos_y)
                    implicit none
                    real(8), intent(in) :: pos_x, pos_y

                    in_research_area = .true.

                    if ((pos_x<lon_min_out) .OR. (pos_x>lon_max_out)) then 
                       in_research_area = .false.
                    endif

                    if ((pos_y<lat_min_out) .OR. (pos_y>lat_max_out)) then
                        in_research_area = .false.
                    endif


                end function in_research_area

                logical function agent_above_water(gx, gy, jp,t_hep)
                    implicit none 
                    integer, intent(in) :: jp, gx, gy, t_hep
                    agent_above_water = .false.
                    if(hep(gx, gy, jp, t_hep) <= 0. )    then
                        agent_above_water = .true.
                    endif
                end function agent_above_water

                subroutine calculate_gradient(gx,gy,jp,grad_x,grad_y) ! In this function there is some kind of coordinate transformation I think we should isolate
                                                    ! that into a seperate function so that we can use it in other places as well 
                    integer, intent(in) :: gx,gy,jp
                    real, intent(inout) :: grad_x, grad_y

                    !real(8), dimension(0:8) :: heploc
                    !real(8) :: heploc_max
                    !integer, dimension(0:8) :: gxx, gyy
                    !integer :: iloc, il

                    heploc(0) = hep_av(gx,   gy,   jp)   ! hepC
                    gxx   (0) = gx
                    gyy   (0) = gy
                    heploc(1) = hep_av(gx-1, gy-1, jp)   ! hepSW
                    gxx   (1) = gx-1
                    gyy   (1) = gy-1
                    heploc(2) = hep_av(gx,   gy-1, jp)   ! hepS
                    gxx   (2) = gx
                    gyy   (2) = gy-1
                    heploc(3) = hep_av(gx+1, gy-1, jp)   ! hepSE
                    gxx   (3) = gx+1
                    gyy   (3) = gy-1
                    heploc(4) = hep_av(gx+1, gy,   jp)   ! hepE
                    gxx   (4) = gx+1
                    gyy   (4) = gy
                    heploc(5) = hep_av(gx+1, gy+1, jp)   ! hepNE
                    gxx   (5) = gx+1
                    gyy   (5) = gy+1
                    heploc(6) = hep_av(gx,   gy+1, jp)   ! hepN
                    gxx   (6) = gx
                    gyy   (6) = gy+1
                    heploc(7) = hep_av(gx-1, gy+1, jp)   ! hepNW
                    gxx   (7) = gx-1
                    gyy   (7) = gy+1
                    heploc(8) = hep_av(gx-1, gy,   jp)   ! hepW
                    gxx   (8) = gx-1
                    gyy   (8) = gy

                    heploc_max = -9999.
                    do il = 0, 8
                        if ( heploc(il) .gt. heploc_max ) then 
                            heploc_max = heploc(il)
                            iloc = il 
                        endif
                    enddo

                    if ( iloc == 0 ) then
                        grad_x = 0.d0
                        grad_y = 0.d0 
                    elseif ( iloc == 2 .or. iloc == 6 ) then 
                        grad_x = 0.d0
                        grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y(i,jp))*deg_km)
                    elseif ( iloc == 4 .or. iloc == 8 ) then 
                        grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x(i,jp))*cos(y(i,jp)*deg_rad) * deg_km)
                        grad_y = 0.d0
                    else 
                        grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x(i,jp))*cos(y(i,jp)*deg_rad) * deg_km)
                        grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y(i,jp))*deg_km)
                    endif


                end subroutine calculate_gradient

                subroutine movement_at_boundary(x0, y0, ux0, uy0, x, y, ux, uy)
                    real(8), intent(in) :: x0,y0,ux0,uy0
                    real(8), intent(inout) :: x,y,ux,uy
                    
                    ! 
                    ! do reflection
                    !
                    if ( (x < lon_min_out) ) then 
                        x  = 2.*lon_min_out - x
                        ux = 2.*(x - x0 )/dt - ux0
                    elseif ( (x > lon_max_out) ) then
                        x = 2.*lon_max_out - x
                        ux = 2.*(x - x0)/dt - ux0
                    endif 

                    if ( (y < lat_min_out) ) then
                        y = 2.*lat_min_out - y
                        uy = 2.*(y - y0 )/dt - uy0
                    elseif ( (y > lat_max_out) ) then
                        y = 2.*lat_max_out - y
                        uy = 2.*(y - y0 )/dt - uy0
                    endif


                end subroutine movement_at_boundary


          ! after the human loop
          subroutine update_hep_human_density(jp)
            integer :: jp
                      !      !$OMP END DO
            !      !$OMP ATOMIC
            out_count(jp) = out_count(jp) + out_count_priv(jp)
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

              !subroutine move active agent to beginning of matrix

              subroutine birth_death_old(jp)
                integer :: jp
                  
                !
                ! YS, have not worked on birth_and_death_cluster
                !                call birth_and_death_cluster(x, y, dens, hep(:,:,t_hep), lat_hep, lon_hep, r_B, rho_max, hum_max, eps, &
                !                        minpts, hum_id, hum_count, hum_t, death_count, birth_count, IX, amount_cluster, avg_cluster)
                !
                          
                itimes = itimes + 1
                call birth_death_euler1(x(:,jp),y(:,jp),ux(:,jp),uy(:,jp),sigma_u(jp),                                 &
                      &                        idens(:,:,jp),dens_adj(:,:,jp),hep_av(:,:,jp),                                    &  ! hep(:,:,jp,t_hep), 
                      &                        lat_hep, lon_hep, r_B(jp), d_B(jp), rho_max(jp), hum_id(:,jp), hum_count(jp),     &
                      &                        hum_t(jp), death_count(jp), birth_count(jp), dt_bdyr, itimes, jp,hum_t)


                if ( jp == 3 ) then
                  call birth_death_mix(x, y, idens, dens_adj, hep_av(:,:,:),  &  ! hep(:,:,:,t_hep),   
                        &                          lat_hep, lon_hep, r_B, rho_max,        &
                        &                          hum_id, hum_count, hum_t, birth_count, dt_bdyr, hum_t)
                elseif ( jp > 3 ) then 
                  print *, "for population > 3, not programed"
                  stop
                endif
              end subroutine birth_death_old

              !subroutine move active agent to beginning of matrix
          




      !{
          subroutine move_active_agents_to_beginning_of_matrix(jp)
                    integer :: jp
                    !print *, "move_active_agents_to_beginning_of_matrix" ! DN debugging 28.05.25
                    x0(:,:) = -1.0E3
                    y0(:,:) = -1.0E3
                    ux0(:,:) = 0.                                                        ! YS, make this random of std = sigma_u
                    uy0(:,:) = 0.
                    hum_id_0(:,:) = 0

                    is_dead0(:,:) = .true. ! initialize the is_dead array to true for all agents
                    call make_pop_array_empty(population_agents_matrix0) 

                    c = 0
                    do j = 1, hum_t(jp)
                      if (is_dead(j,jp) .eqv. .false.) then
                      !if (x(j,jp) /= -1.0E3) then
                        c = c + 1
                        x0(c,jp) = x(j,jp)
                        y0(c,jp) = y(j,jp)
                        ux0(c,jp) = ux(j,jp)
                        uy0(c,jp) = uy(j,jp)
                        hum_id_0(c,jp) = hum_id(j,jp)
                        is_dead0(c,jp) = is_dead(j,jp)
                        ! The following six lines were added to merge the linked list and the matrix representation of the humans
                        !print *, "c, jp, j", c, jp, j ! DN debugging 28.05.25
                        !population_agents_matrix0(c,jp) = population_agents_matrix(j,jp)                              
                        !population_agents_matrix0(c,jp)%node%position_human = c 
                        if (ASSOCIATED(population_agents_matrix(j,jp)%node)) THEN
                            population_agents_matrix0(c,jp) = population_agents_matrix(j,jp)                              
                            population_agents_matrix0(c,jp)%node%position_human = c 
                        else  
                          print *, "active agent to be moved to beginning of matrix is not associated, c, jp, j", c, jp, j ! DN debugging 10.06.25
                        endif                                    
                      else 
                        if (associated(population_agents_matrix(j,jp)%node)) then
                          ! print *, "agent is dead, c, jp, j", c, jp, j ! DN debugging 28.05.25
                          ! If the agent is dead, we have to call the agent_die() method to remove it from the linked list 
                          ! and move it to the list of dead agents. Since in the old program the "move active agents to beginning of matrix"
                          ! is called twice per time step, we have to check if the agent is still associated (or already dead)
                          ! This is necessary to avoid memory leaks and ensure that the agent is properly removed from the simulation.
                          ! This was added by Daniel Nogues 18.05.25
                          if (population_agents_matrix(j,jp)%node%is_dead .eqv. .true.) then
                            print *, "agent is already dead, c, jp, j", c, jp, j ! DN debugging 10.06.25
                          else
                            !print *, "agent is dead, c, jp, j", c, jp, j ! DN debugging 10.06.25
                            call population_agents_matrix(j,jp)%node%agent_die()
                            
                            
                          endif  
                          
                        endif                                      
                      endif
                      hum_t(jp) = c                                                                                              
                    enddo                                                                                          
                                                                                                   
                    hum_id(:,jp) = hum_id_0(:,jp)                                                                 
                    x(:,jp) = x0(:,jp)                                                                           
                    y(:,jp) = y0(:,jp)                                                                             
                    ux(:,jp) = ux0(:,jp)                                                                          
                    uy(:,jp) = uy0(:,jp)
                    is_dead(:,jp) = is_dead0(:,jp)                                                                            
                    ! The following line was added to merge the linked list and the matrix representation of the humans
                    population_agents_matrix(:,jp) = population_agents_matrix0(:,jp)

                    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!     Daniel Nogues 19.05.25
                    ! Since the locations of the humans in the arrays that stores their ! 
                    ! variables are changed we have to update the corresponing variables!
                    ! in the linked list that stores the humans.                        !
                    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

                    ! This is very nasty coding because it will be very difficult to understand
                    ! what is going on when somebody else (or even I) will read this code in the future.

                    ! But since I would have to change the whole code to merge the linked list 
                    ! and the matrix representation of the humans i will leave it like this for now. 



          end subroutine move_active_agents_to_beginning_of_matrix


          subroutine save_position_and_density(jp)
              integer :: jp 
              ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ! Save the position of the humans and the population density
              ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              if ( mod(t, Tn/save_t) /=0 ) then 
                dens_acc(:,:,jp) = dens_acc(:,:,jp) + dens(:,:,jp)
                flow_acc(:,:,:,jp) = flow_acc(:,:,:,jp) + flow(:,:,:,jp)
              elseif ( mod(t, Tn/save_t) == 0 ) then
                dens_acc(:,:,jp) = dens_acc(:,:,jp) + dens(:,:,jp)
                flow_acc(:,:,:,jp) = flow_acc(:,:,:,jp) + flow(:,:,:,jp)

                wkdens(:,:) = dens_acc(:,:,jp)/( Tn/save_t )
                wkflow(:,:,:) = flow_acc(:,:,:,jp)/( Tn/save_t )

                dens_acc(:,:,jp) = 0.d0
                flow_acc(:,:,:,jp) = 0.d0

                if ( save_out .eqv. .true. ) then 
                  call check(nf90_put_var(nc_id_out_pos(jp),var_t_id_out(jp), t*dt, &
                    &  start=(/t/(Tn/save_t)+1/)), 3, "put_var", "time")
                  call check(nf90_put_var(nc_id_out_pos(jp),var_pos_hum_id_out(jp),x(:,jp), & 
                    & start=(/1, 1, t/(Tn/save_t)+1/), &
                    &        count=(/1, hum_max(jp), 1/)), 3, "put_var", "pos_hum")
                  call check(nf90_put_var(nc_id_out_pos(jp),var_pos_hum_id_out(jp),y(:,jp),start=(/2, 1, t/(Tn/save_t)+1/), &
                    &           count=(/1, hum_max(jp), 1/)), 3, "put_var", "pos_hum")

                  !         call check(nf90_put_var(nc_id_out_pos, var_velo_id_out, ux0, start=(/1, 1, t/(Tn/save_t)+1/), &
                  !               count=(/1, hum_max, 1/)), 3, "put_var", "velo")
                  !         call check(nf90_put_var(nc_id_out_pos, var_velo_id_out, uy0, start=(/2, 1, t/(Tn/save_t)+1/), &
                  !               count=(/1, hum_max, 1/)), 3, "put_var", "velo")
                  !         call check(nf90_put_var(nc_id_out_pos, var_hum_id_id_out, hum_id_0, start=(/1, t/(Tn/save_t)+1/), count=(/hum_max, 1/))&
                  !                 , 3, "put_var", "hum_id")
                  call check(nf90_put_var(nc_id_out_dens(jp),var_t_id_out(jp), t*dt, start=(/t/(Tn/save_t)+1/)), & 
                           & 3, "put_var", "time")
                  call check(nf90_put_var(nc_id_out_dens(jp),var_dens_id_out(jp),wkdens,start=(/1, 1, t/(Tn/save_t)+1/),    &
                  &       count=(/dlon_hep, dlat_hep, 1/)), 3, "put_var", "dens")
                  call check(nf90_put_var(nc_id_out_dens(jp),var_flow_id_out(jp),wkflow,start=(/1, 1, 1, t/(Tn/save_t)+1/), &
                  &       count=(/2, dlon_hep, dlat_hep, 1/)), 3, "put_var", "flow")
                endif ! endif save_out

              endif   ! endif mod(t, ...)
              !.....................................................................
              if ( protcl ) then 
                if (mod(t-1,10000) == 0) then
                  write (9,*) (t + 1)/100, ' years'
                  write (9,*) 'Total Amount of Humans:', hum_t(jp)
                  write (9,*) 'Birth:', birth_count(jp)
                  write (9,*) 'Death:', death_count(jp)
                  write (9,*) 'Birth - death', birth_count(jp) - death_count(jp)
                  write (9,*) 'Drowned:', drown_count(jp)
                  write (9,*) 'Out:', out_count(jp)
                endif
              endif 
              !......................................................................
          end subroutine save_position_and_density

      !}

      subroutine safe_progress(t)
        integer :: t
        if (mod(t, Tn/save_t) == 0) then
          write(101, '(f10.3,5i10)') t/100., hum_t(1), birth_count(1), death_count(1), drown_count(1), out_count(1)
          write(102, '(f10.3,5i10)') t/100., hum_t(2), birth_count(2), death_count(2), drown_count(2), out_count(2)
          write(103, '(f10.3,5i10)') t/100., hum_t(3), birth_count(3), death_count(3), drown_count(3), out_count(3)
        endif 
      end subroutine safe_progress
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



