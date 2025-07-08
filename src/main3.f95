
program main_program


    use mod_agent_class
        ! initilize_agents_array
        ! initilize_dead_agents_array
    use mod_setup_agents

    use mod_debug_agents
        ! compare_matrix_and_agent_matrix

    use export_agents



    implicit none

    type(Node), pointer :: current_agent_ptr
    character(len=100) :: temp_string

    


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the Matrix calculations
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the agent list
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Tests before the Main Calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         ! ++++++++ Test 1 +++++++++++++++++++++++++++++++++++++++
            ! Description: Check if the hep is read correctly
            !call check_hep_read_correctly() ! TODO  
                                            ! This function is not implemented yet
                                            ! It should be implemented in the file : 
                                            ! src/test_and_debug/mod_test_hep.f95        

        ! ++++++++ Test 2 +++++++++++++++++++++++++++++++++++++++



    

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !


    

    timesteps: do t = 1, Tn

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Development 
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
            ! #########################################################
            ! Loop over every agent via their population
            ! #########################################################


            PopulationLoop1: do jp = 1, npops




                    HumanLoop: do i = 1, hum_t(jp)

                    enddo HumanLoop


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

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Matrix-Double-Linked-List Merge Management
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Test for correctness
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            ! ########### Test 1 ####################################
                ! Description: We check if matrix for calculation is coherent
                !              with matrix for agent pointers
                call compare_matrix_and_agent_matrix() 

            ! ########### Test 2 ####################################
                ! Descritption: Check if the matrix that indicates whether an agent
                !               is dead is coherent 
                call check_is_dead_matrix()       

            ! ########### Test 3 ####################################
                ! Description: Checks whether there are any alive agents
                !              in the list of dead agents
                call check_dead_agents_list_for_alive_agents()

            ! ########### Test 4 ####################################
                ! Description: Checks whether there are any dead agents
                !              in the list of alive agents
            !    call check_alive_agents_list_for_dead_agents()

            ! ########### Test 5 ####################################
                ! Description: Checks whether the agents_array contains actually 
                !              number_of_agents many agents
                call check_agents_array()

            ! ########### Test 6 ####################################
                ! Description: Checks whether population_agents_matrix contains entries that are
                !              not associated even though they should be
                call check_population_agents_matrix()
            ! ########### Test 7 ####################################
                ! Description: Checks whether the agent that we fin in the matrix in position (i,j)
                !              has i and j as position_human and position_population
                call check_position_in_matrix_consistency()


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
        if (t == 5) then
            !call print_information_about_agents()
            !call print_dimensions_of_arrays()
        endif

    enddo timesteps





end program main_program