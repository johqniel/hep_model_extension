
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

    type(Node), pointer :: current_agent_ptr
    


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



    timesteps: do t = 1, Tn

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Development 
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            call update_old(t)
            
            ! #########################################################
            ! Loop over every agent via their population
            ! #########################################################

            PopulationLoop1: do jp = 1, npops

                    ! skip if population is supposed to enter simulation later
                    if ( t < tstep_start(jp) ) then 
                        CYCLE
                    endif 
                    


                    !############ old human movement ##################
                    call setup_update_human(jp)
                    HumanLoop: do i = 1, hum_t(jp)
                        call update_human(i)
                    enddo HumanLoop
                    call after_human_update(jp)



                    !############# old birth death ######################
                    if (mod(t, dt_bd) == 0) then
                        call move_active_agents_to_beginning_of_matrix(jp)
                        call birth_death_old(jp)
                    endif
                    call move_active_agents_to_beginning_of_matrix(jp)



                    !############# Data Management ######################
                    call save_position_and_density(jp)
                    !- this is not generally necissarry but specific for 
                    !            - old human movement
                    !            - old birth death

                    
            enddo PopulationLoop1

            ! #########################################################
            ! Loop over every population
            ! #########################################################

            PopulationLoop2: do jp = 1, npops
            enddo PopulationLoop2

            ! #########################################################
            ! Loop over every agent randomly (population vice)
            ! #########################################################

            ! #########################################################
            ! Loop over every agent randomly
            ! #########################################################

            ! #########################################################
            ! Loop over every agent semi-random 
            ! #########################################################

            

            ! #########################################################
            ! Loop over every agent via their positions
            ! #########################################################



            ! TODO DN 16.06. : 

                    ! A: structure the development section clearly like the test section below. Done DN 16.06.
                    !A2: document possible test_functions and send to GL.
                    ! B: extract the do loops from update_old()
                    ! C: extract the move_module from update_old()
                    ! D: write a simple example for new_birth_death
                    !D2:             ''             new_move_module



            ! do t = 1 .... large
                !do üpop = 1...
                ! ....
                 

                    ! call move_agent() todo DN 16.06.
                    !    Arguments: 
                    !           - old postion, old velocity, parameters
                    !    Return: 
                    !           - new posistion, new velocity
                    !    Notes: 
                    !           - This should work the same way like 
                    !             movement is done so far

            ! Once move_agent is done it can serve as a blueprint for other functions.

            ! TODO DN 16.06:
            ! Example Functions for learning: 
                ! call birth_death_example


            ! call birth_death_new   (needs to be implemented) 
            !  
        


        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Test for correctness
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            ! ########### Test 1 ####################################
                ! Description: We check if matrix for calculation is coherent
                !              with matrix for agent pointers
                call compare_matrix_and_agent_matrix() 

            ! ########### Test 2 ####################################
                ! Descritption: Check if array of dead agents is coherent 
                !               with the matrixes(?) TODO
                call check_is_dead_array()       

            ! ########### Test 3 ####################################
                ! Description: Checks whether there are any alive agents
                !              in the list of dead agents
                call check_dead_agents_list_for_alive_agents()

            ! ########### Test 4 ####################################
                ! Description: Checks whether there are any dead agents
                !              in the list of alive agents
                call check_alive_agents_list_for_dead_agents()

            ! ########### Test Idea #################################
                ! Description:  Test for xyz ############################
                ! call 

            
        ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Information of the state 
        ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
        ! Description: Here you find different functions that print information to the console.  
        !              You can toggle them by commenting them out 
        ! 
        !
        !   

            !---------------------------- Every 10.000 time ticks: ------------------------------

            if (mod(t,10000) == 0) then
                call print_born_death_counter_matrix()
                call count_agents_matrix() ! This will count the number of agents in the matrix
                call count_agents_list()
                call count_dead_agents_list() ! This will count the number of dead agents in the list
                call compare_counters_of_agents()
            endif

            ! --------------------------- Every 1000 time ticks: ----------------------------------
            if (mod(t,1000) == 0) then

                do jp = 1, npops
                    print *, "main t, jp, hum_t, t_hep", t, jp, hum_t(jp), t_hep
                enddo

            
            endif
        !call print_information_about_agents(t)
        !call print_dimensions_of_arrays(t)

    enddo timesteps



    call safe_and_close_files()



end program main_program