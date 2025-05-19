
program main_program

    use mod_matrix_calculations
    use mod_agent_class
        ! initilize_agents_array
        ! initilize_dead_agents_array
    use mod_setup_agents



    implicit none


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the Matrix calculations
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    call allocate_memory_and_open_files()   

    call setup_initial_conditions()  

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the agent list
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    call initilize_agents_array()
    call initilize_dead_agents_array() 
    call setup_agents_from_matrix() ! This will create the linked list of agents             



    

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !

    timesteps: do t = 1, Tn
        call update_old(t)
    enddo timesteps



    call safe_and_close_files()



end program main_program