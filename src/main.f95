
program main_program

    use mod_matrix_calculations



    implicit none

    call allocate_memory_and_open_files()   

    call setup_initial_conditions()                



    

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !

    timesteps: do t = 1, Tn
        call update_old(t)
    enddo timesteps



    call safe_and_close_files()



end program main_program