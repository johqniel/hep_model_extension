module mod_setup_hep
!--------------------------------------------------------------------
! Yaping Shao, 3 Jul 2024
! University of Cologne, Germany
!
! Konstantin Klein, early development for PhD
!
! Purpose: 
! (1) Setup configuration of N-Population Human System Model (NP-HSM)
! (2) Read in HEP (Human Existence Potential) data
! (3) Define output paths
!--------------------------------------------------------------------
    use netcdf
    use mod_utility

    use mod_globals
    ! Contains constants such as: pi, Earth parameters
    !
    ! Contains parameters of the model such as: tyr_start (start year of simulation)
    !
    ! Contains mostly strings that are constant
    !
    ! Contains variables that are used by multiple modules
    ! Uses:     - hep




    implicit none
    save

   !include "netcdf.inc"




!-------------------------------------------------------
! local variables
!-------------------------------------------------------
    integer :: jp
    character(len=256) :: filepath_temp
    character(len=20)  :: random_seed_temp
    character(len = 44) :: output_path = "/home/dnoguesk/Desktop/hep_extension/output/"
 
    contains

subroutine check(status,state,operation,variable,timestep,year)
        integer, intent(in) :: status
        integer, intent(in),optional :: state, timestep, year
        character (len=*),optional :: operation,variable
        if (status /= nf90_noerr) then
            print *,"Error detected, aborting program..."
            print *,"Information:"
            if (present(state)) then
                select case (state)
                case (1)
                    print *,"Programm stopped at mod_setup_load() while loading the input file."
                case (2)
                    print *,"Programm stopped at mod_setup_load() while creating the output file."
                case (3)
                    print *,"Programm stopped at mod_setup_load() while saving the initial state."
                case (4)
                    print *,"Programm stopped within mod_run.f95 while saving the outputSensityss."
                end select
            end if
            if (present(operation)) then
                print *,"The operation that failed was nf90_",operation,"."
            end if
            if (present(variable)) then
                print *,"The variable ",variable," was involved in the operation."
            end if
            if (present(timestep) .AND. present(year)) then
                print *,"The exception occured during the timestep ",timestep," within the modelled year ",year,"."
            end if
            print *,"The orignal Error statement follows:"
            print *,trim(nf90_strerror(status))
            stop 2
        end if
end subroutine check

subroutine setup_load()
    logical dir_a, dir_n, dir_m   

    iseed = 1111


    !DO  
    !    call run_no1(iseed)
    !    if (iseed .ge. 200 ) exit
    !    print *, "Random seed iseed", iseed
    !ENDDO


    write(random_seed,'(i4)') iseed

    filepath_temp = output_path // "Sensity/AUR/size_5550020001avD15cb1250_"//random_seed//".dat"

    inquire(file=filepath_temp, exist=dir_a)
    inquire(file=output_path // "Sensity/NEA/size_5550020001avD15cb1250_"//random_seed//".dat", exist=dir_n)
    inquire(file=output_path // "Sensity/MIX/size_5550020001avD15cb1250_"//random_seed//".dat", exist=dir_m)
    !if ( dir_a .or. dir_n .or. dir_m ) go to 17                 ! if file already exist, generate a new iseed for file name

    ! Output file names for AMH
    path_out_pos(1)  = output_path //"Sensity/AUR/pos_aur_5550020001avD15cb1250_"//random_seed//".nc"
    path_out_dens(1) = output_path //"Sensity/AUR/dens_aur_5550020001avD15cb1250_"//random_seed//".nc"
    !
    ! Output file names for NEA
    path_out_pos(2)  = output_path //"Sensity/NEA/pos_nea_5550020001avD15cb1250_"//random_seed//".nc"
    path_out_dens(2) = output_path //"Sensity/NEA/dens_nea_5550020001avD15cb1250_"//random_seed//".nc"
    !
    ! Output file names for MIX
    path_out_pos(3)  = output_path //"Sensity/MIX/pos_mix_5550020001avD15cb1250_"//random_seed//".nc"
    path_out_dens(3) = output_path //"Sensity/MIX/dens_mix_5550020001avD15cb1250_"//random_seed//".nc"
    !
    ! Output file name for population size, birth, death, disappeared
    open(101, file = output_path //"Sensity/AUR/size_5550020001avD15cb1250_"//random_seed//".dat")
    open(102, file = output_path //"Sensity/NEA/size_5550020001avD15cb1250_"//random_seed//".dat")
    open(103, file = output_path //"Sensity/MIX/size_5550020001avD15cb1250_"//random_seed//".dat")

    write(101,'(6a10)') "Time (yr)", "AUR Human No.", "Birth No.", "Death No.", "Drown No.", "Outed No."
    write(102,'(6a10)') "Time (yr)", "NEA Human No.", "Birth No.", "Death No.", "Drown No.", "Outed No."
    write(103,'(6a10)') "Time (yr)", "MIX Human No.", "Birth No.", "Death No.", "Drown No.", "Outed No."
    !--------------------
    ! Load input HEP file
    !--------------------
    DO jp = 1, npops
        call check(nf90_open( path_in(jp), nf90_nowrite, nc_id_hep(jp) ),          1, "open"                    )
        !          print *, path_in(jp), nc_id_hep(jp)

        if ( jp == 1 ) then
          call check(nf90_inq_dimid( nc_id_hep(jp), name_in_dlon, dlon_id ),       1, "inq_dimid", name_in_dlon )
            !          print *, "name_in_dlon", name_in_dlon
          call check(nf90_inquire_dimension(nc_id_hep(jp), dlon_id, len=dlon_hep), 1, "inquire_dimension", name_in_dlon)
            !          print *, "dlon_hep", dlon_hep

          call check(nf90_inq_dimid(nc_id_hep(jp), name_in_dlat, dlat_id),         1, "inq_dimid", name_in_dlat)
            !          print *, "name_in_dlat", name_in_dlat
          call check(nf90_inquire_dimension(nc_id_hep(jp), dlat_id, len=dlat_hep), 1, "inquire_dimension", name_in_dlat)
            !          print *, "dlat_hep", dlat_hep

          call check(nf90_inq_dimid(nc_id_hep(jp), name_in_dt, dt_id),             1, "inq_dimid", name_in_dt)
            !          print *, "name_in_dt", name_in_dt
          call check(nf90_inquire_dimension(nc_id_hep(jp), dt_id, len=dt_hep),     1, "inquire_dimension", name_in_dt)
            !          print *, "dt_hep", dt_hep

          call check(nf90_inq_varid(nc_id_hep(jp), name_in_lat, lat_id),           1, "inq_varid", name_in_lat)
          allocate(lat_hep(dlat_hep))                                                              ! ys, allocated npops times, need reprog
          call check(nf90_get_var(nc_id_hep(jp), lat_id, lat_hep),                 1, "get_var", name_in_lat)
            !          print *, "lat_hep", lat_hep 

          call check(nf90_inq_varid(nc_id_hep(jp), name_in_lon, lon_id),           1, "inq_varid", name_in_lon)
          allocate(lon_hep(dlon_hep))                                                              ! ys, allocated npops times, need reprog
          call check(nf90_get_var(nc_id_hep(jp), lon_id, lon_hep),                 1, "get_var", name_in_lon)
            !          print *, "lon_hep", lon_hep

          allocate(hep_wk(dlon_hep, dlat_hep,        dt_hep))
          allocate(hep   (dlon_hep, dlat_hep, npops, dt_hep))
        endif

        call check(nf90_inq_varid(nc_id_hep(jp), name_in_hep, hep_id),             1, "inq_varid", name_in_hep)
        call check(nf90_get_var(nc_id_hep(jp), hep_id, hep_wk),                    1, "get_var", name_in_hep)

        hep(1:dlon_hep,1:dlat_hep,jp,1:dt_hep)=hep_wk(1:dlon_hep,1:dlat_hep,1:dt_hep)

        call check(nf90_close(nc_id_hep(jp) ))


        IF (save_out .eqv. .true.) THEN
        !---------------------------------------
        ! Declare output file positon of humans
        !---------------------------------------
        call check(nf90_create(path_out_pos(jp),NF90_NETCDF4,nc_id_out_pos(jp)),2,"create","path_output_pos")
        call check(nf90_def_dim(nc_id_out_pos(jp), name_out_dim_t, save_t+1, dim_t_id_out(jp)),                  &
            &           2, "def_dim", name_out_dim_t)

        call check(nf90_def_dim(nc_id_out_pos(jp), name_out_dim_hum, hum_max(jp), dim_hum_id_out(jp)),           &
            &           2, "def_dim", name_out_dim_hum)

        call check(nf90_def_dim(nc_id_out_pos(jp), name_out_dim_lonlat, 2, dim_lonlat_id_out(jp)),               &
            &           2, "def_dim", name_out_dim_lonlat)

        dim_pos_hum_id_out(:,jp) = (/dim_lonlat_id_out(jp),dim_hum_id_out(jp),dim_t_id_out(jp)/)

        call check(nf90_def_var(nc_id_out_pos(jp), name_out_var_t, NF90_DOUBLE, dim_t_id_out(jp), var_t_id_out(jp)), &
            &           2, "def_var", name_out_dim_t)

        call check(nf90_def_var(nc_id_out_pos(jp), name_out_var_pos_hum, NF90_DOUBLE, dim_pos_hum_id_out(:,jp),        &
            &           var_pos_hum_id_out(jp)), 2, "def_var", name_out_var_pos_hum)

        call check(nf90_put_att(nc_id_out_pos(jp), NF90_GLOBAL, output_name_description, output_description_pos),    &
            &           2, "put_att", "output_description")

        call check(nf90_put_att(nc_id_out_pos(jp), var_t_id_out(jp), output_name_longname, output_t),                &
            &           2, "put_att", output_t)

        call check(nf90_put_att(nc_id_out_pos(jp), var_pos_hum_id_out(jp), output_name_longname, output_pos_hum),    &
            &           2, "put_att", output_pos_hum)

        call check(nf90_enddef(nc_id_out_pos(jp)),2,"enddef")

        call check(nf90_put_var(nc_id_out_pos(jp), var_t_id_out(jp), 0, start=(/1/)), 3, "put_var", "time")

        ! Declare output file population density
        call check(nf90_create(path_out_dens(jp),NF90_NETCDF4,nc_id_out_dens(jp)),2,"create","path_output_dens")
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_t, save_t+1, dim_t_id_out(jp)),                     &
            &           2, "def_dim", name_out_dim_t)
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_lat, dlat_hep, dim_lat_id_out(jp)),                 &
            &           2, "def_dim", name_out_dim_lat)
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_lon, dlon_hep, dim_lon_id_out(jp)),                 &
            &           2, "def_dim", name_out_dim_lon)
        call check(nf90_def_dim(nc_id_out_dens(jp), name_out_dim_lonlat, 2, dim_lonlat_id_out(jp)),                  &
            &           2, "def_dim", name_out_dim_lonlat)

        dim_dens_id_out(:,jp) = (/ dim_lon_id_out(jp), dim_lat_id_out(jp), dim_t_id_out(jp) /)
        dim_flow_id_out(:,jp) = (/ dim_lonlat_id_out(jp), dim_lon_id_out(jp), dim_lat_id_out(jp), dim_t_id_out(jp) /)

        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_t, NF90_DOUBLE, dim_t_id_out(jp),   &
            &           var_t_id_out(jp)), 2, "def_var", name_out_dim_t)
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_lon,NF90_DOUBLE,dim_lon_id_out(jp), & 
            &           var_lon_id_out(jp)), 2, "def_var", name_out_dim_lon)
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_lat,NF90_DOUBLE,dim_lat_id_out(jp), &
            &           var_lat_id_out(jp)), 2, "def_var", name_out_dim_lat)
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_dens,NF90_DOUBLE,dim_dens_id_out(:,jp), &
            &           var_dens_id_out(jp)), 2, "def_var", name_out_var_dens)
        !
        ! For den and flow
        !
        call check(nf90_def_var(nc_id_out_dens(jp),name_out_var_flow,NF90_DOUBLE,dim_flow_id_out(:,jp), &
            &           var_flow_id_out(jp)), 2, "def_var", name_out_var_flow)
        call check(nf90_put_att(nc_id_out_dens(jp), NF90_GLOBAL, output_name_description, output_description_dens), &
            &           2, "put_att", "output_description")
        call check(nf90_put_att(nc_id_out_dens(jp),var_t_id_out(jp),   output_name_longname,output_t), 2, "put_att",output_t)
        call check(nf90_put_att(nc_id_out_dens(jp),var_lon_id_out(jp), output_name_longname,output_lon),2,"put_att",output_lon)
        call check(nf90_put_att(nc_id_out_dens(jp),var_lat_id_out(jp), output_name_longname,output_lat),2,"put_att",output_lat)
        call check(nf90_put_att(nc_id_out_dens(jp),var_dens_id_out(jp),output_name_longname,output_dens),2,"put_att",output_dens)
        call check(nf90_put_att(nc_id_out_dens(jp),var_flow_id_out(jp),output_name_longname,output_flow),2,"put_att",output_flow)
        !
        call check(nf90_enddef(nc_id_out_dens(jp)),2,"enddef")
        call check(nf90_put_var(nc_id_out_dens(jp), var_t_id_out(jp),   0,       start=(/1/)), 3, "put_var", "time")
        call check(nf90_put_var(nc_id_out_dens(jp), var_lat_id_out(jp), lat_hep, start=(/1/)), 3, "put_var", "lat")
        call check(nf90_put_var(nc_id_out_dens(jp), var_lon_id_out(jp), lon_hep, start=(/1/)), 3, "put_var", "lon")
        
        ENDIF                                 ! endif (save_out .eqv. .true.)
      enddo                                   ! ENDDO npops

end subroutine setup_load

end module mod_setup_hep



