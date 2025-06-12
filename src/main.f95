
program main_program

    use mod_matrix_calculations
        ! allocate_memory_and_open_files
        ! setup_initial_conditions
    use mod_agent_class
        ! initilize_agents_array
        ! initilize_dead_agents_array
    use mod_setup_agents

    use mod_debug_agents
        ! compare_matrix_and_agent_matrix



    implicit none


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the Matrix calculations
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print *, "allocate memory and open files"
    call allocate_memory_and_open_files()   

    print *, "setup initial conditions"
    call setup_initial_conditions()  

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the agent list
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print *, "initilize agents array"
    call initilize_agents_array()
    print *, "initilize dead agents array"
    call initilize_dead_agents_array() 
    print *, "initilize hum_id_mirror array"
    call initilize_agent_array_mirror_of_hum_id(hum_max_A, npops) ! This will create the mirror array for the agents
    print *, "setup agents from matrix"
    call setup_agents_from_matrix() ! This will create the linked list of agents             



    

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
    print *, "begin main calculation"

    call count_agents_matrix() ! This will count the number of agents in the matrix
    call count_agents_list() ! This will count the number of agents in the list

    timesteps: do t = 1, Tn
        call update_old(t)

        ! test if everything works as intended: 

        call compare_matrix_and_agent_matrix()
        call check_is_dead_array()
        call check_dead_agents_list_for_alive_agents()
        call check_alive_agents_list_for_dead_agents()

        if (mod(t,10000) == 0) then
            call print_born_death_counter_matrix()
            call count_agents_matrix() ! This will count the number of agents in the matrix
            call count_dead_agents_list() ! This will count the number of dead agents in the list
            call compare_counters_of_agents()
        endif
        !call print_information_about_agents(t)
        !call print_dimensions_of_arrays(t)

    enddo timesteps



    call safe_and_close_files()



end program main_program