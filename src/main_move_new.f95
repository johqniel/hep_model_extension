
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

    use mod_grid

    use mod_debug_grid

    use export_agents

    use mod_movement



    implicit none

    type(Node), pointer :: current_agent_ptr
    character(len=100) :: temp_string

    type(spatial_grid), target :: grid
    type(spatial_grid), pointer :: grid_ptr

    grid_ptr => grid


    


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


    
    
    

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup the Grid 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    grid%nx = dlon_hep
    grid%ny = dlat_hep

    call grid%allocate_grid()
    print *, "grid allocated"

    call grid%initialize_grid(head_agents)




    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Tests before the Main Calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print* , "Run Tests before Main Calculation ... "

        ! ######### Testing the hep #################################

            ! ++++++++ Test 1 +++++++++++++++++++++++++++++++++++++++
                ! Description: Check if the hep is read correctly
                !call check_hep_read_correctly() ! TODO  
                                                ! This function is not implemented yet
                                                ! It should be implemented in the file : 
                                                ! src/test_and_debug/mod_test_hep.f95        

        ! ######## Testing the agents ##############################

            ! ++++++++ Test 1 +++++++++++++++++++++++++++++++++++++++
                    ! Description: We check if matrix for calculation is coherent
                    !              with matrix for agent pointers
                    call compare_matrix_and_agent_matrix() 

            ! ++++++++ Test 2 +++++++++++++++++++++++++++++++++++++++
                    ! Descritption: Check if the matrix that indicates whether an agent
                    !               is dead is coherent 
                    call check_is_dead_matrix()       

            ! ++++++++ Test 3 +++++++++++++++++++++++++++++++++++++++
                    ! Description: Checks whether there are any alive agents
                    !              in the list of dead agents
                    call check_dead_agents_list_for_alive_agents()

            ! ++++++++ Test 4 +++++++++++++++++++++++++++++++++++++++
                    ! Description: Checks whether there are any dead agents
                    !              in the list of alive agents
                    call check_alive_agents_list_for_dead_agents()

            ! ++++++++ Test 5 +++++++++++++++++++++++++++++++++++++++
                    ! Description: Checks whether the agents_array contains actually 
                    !              number_of_agents many agents
                    call check_agents_array()

            ! ++++++++ Test 6 +++++++++++++++++++++++++++++++++++++++
                    ! Description: Checks whether population_agents_matrix contains entries that are
                    !              not associated even though they should be
                    call check_population_agents_matrix()
            ! ++++++++ Test 7 +++++++++++++++++++++++++++++++++++++++
                    ! Description: Checks whether the agent that we fin in the matrix in position (i,j)
                    !              has i and j as position_human and position_population
                    call check_position_in_matrix_consistency()


        ! ######### Testing the grid ################################

            ! ++++++++ Test 1 +++++++++++++++++++++++++++++++++++++++
                ! Description: Test whether area is calculated correctly for grid
                !              Uses the old area as a refference 
                call check_area_of_grid(grid,area_for_dens)

                ! ########### Test 1 ###################################
                    ! Descriptioin: Tests if there are dead agents in the grid
                    !               Also checks if there are unassociated agents in grid
                    call check_grid_for_dead_agents(grid)

                ! ########### Test 2 ###################################
                    ! Description: Tests if the position of the agents in the grid is consistent
                    !              with their actual positions. 
                    call check_consistency_grid_agents(grid)

                ! ########### Test 3 ##################################
                    ! Description: Counts agents in grid and checks whether there are as many agents 
                    !              in the grid as there are alive
                    call check_number_of_agents_in_grid(grid)

        !
    
    print* , "Tests before Main Calculation all done."

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Main calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !
    print *, "begin main calculation"



    timesteps: do t = 1, Tn

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Development 
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
            ! #########################################################
            ! Loop over every agent via their population
            ! #########################################################

            call reset_old_help_vars(t)    

            !call prepare_agent_move()
            PopulationLoop1: do jp = 1, npops

                    ! skip if population is supposed to enter simulation later
                    if ( t < tstep_start(jp) ) then 
                        CYCLE
                    endif 
                    


                    !############ old human movement ##################
                    call setup_update_human(jp)
                    HumanLoop: do i = 1, hum_t(jp)
                        !call update_human(i)
                        !call agent_move(i,jp)
                        call agent_move_grid(i,jp,grid_ptr)
                    enddo HumanLoop

                    ! ############ update hep using new human positions/ density ##########

                    ! To reuse old functions we have to write the new positions into matrizes:
                    !          - x,y positions of agents
                    !          - ux,uy velocities of agents

                    call write_new_positions_to_matrix(x(:,jp), y(:,jp), ux(:,jp), uy(:,jp),&
                                                       population_agents_matrix(:,jp),is_dead(:,jp))
                    

                    call update_hep_human_density(jp)




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

            if (sum(drown_count_priv) + sum(out_count_priv_a) + sum(out_count_priv_b) + sum(death_count_priv) > 50) then 
                print *, "t: ", t
                print *, "drowned: ", drown_count_priv
                print*,  "out a : ", out_count_priv_a
                print*,  "out b : ", out_count_priv_b

                print *, "natural_death: ", death_count_priv
            endif

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

                    ! A: structure the development section clearly like the test section below.        Done DN 16.06.
                    !       A2: document possible test_functions and send to GL.                       Done DN 04.07.25
                    ! B: extract the do loops from update_old()                                        Done DN 16.06.
                    ! C: extract the move_module from update_old()                                     Done DN 05.07.25
                    !       C2: rewrite move_module such that it is clear what is happening
                    ! D: write a simple example for new_birth_death
                    !       D2:             ''             new_move_module



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
        ! Matrix-Double-Linked-List Merge Management
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


        call update_agent_list_from_matrix(hum_t)

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Grid Management 
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        call grid%clean_grid_from_dead_agents()

        call grid%update_density_pure()

        ! Ideally we want this to be done on the go when agents die. 
        ! ATM the structure of the program is to messy to do that 
        ! But I am working on it :) DN 01.08.2025

        
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Saving the data
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        if (mod(t,100) == 0) then
            write(temp_string, '(I0)') t
            call write_agents_to_csv("data/agents_plotting_data_" // trim(temp_string) // ".csv")
        endif


        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Test for correctness
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            ! ++++++++ General Tests ++++++++++++++++++++++++++++++++++++

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
                    call check_alive_agents_list_for_dead_agents()

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

            ! +++++++++ Tests of the grid +++++++++++++++++++++++++++++++++++++++++++++++++++++

                ! ########### Test 1 ###################################
                    ! Descriptioin: Tests if there are dead agents in the grid
                    !               Also checks if there are unassociated agents in grid
                    call check_grid_for_dead_agents(grid)

                ! ########### Test 2 ###################################
                    ! Description: Tests if the position of the agents in the grid is consistent
                    !              with their actual positions. 
                    call check_consistency_grid_agents(grid)
                
                ! ########### Test 3 ##################################
                    ! Description: Counts agents in cells using x,y matrix and compares that
                    !              with the number of agents in grid%cells%number_of_agents
                    call check_number_of_agents_in_grid_using_matrix(grid,x,y)

                ! ########### Test 3.5 ##################################
                    ! Description: Counts agents in grid cells using x, y matrix and using 
                    !              the position of agents in agent%pos_x, agent%pos_y 
                    !              and compares the results
                    call compare_number_of_agents_in_grid_matrix_agent_class(grid,x,y,head_agents)

                ! ########### Test 4 ##################################
                    ! Description: Counts agents in grid and checks whether there are as many agents 
                    !              in the grid as there are alive
                    !call check_number_of_agents_in_grid(grid)
                    ! This test somehow doesnt work but it should because the other three tests,
                    ! Test 1 , 2, 4 work idk DN 01.08.2025


                ! ########### Test 5 ##################################
                    ! Description: Counts agents that are not in the cell in which they should be
                    !              aditionally prints error if it finds agents whose position is
                    !              outside of grid
                    call check_if_all_alive_agents_in_correct_cell(grid)

                ! ########### Test n ###################################
                    ! Description: Checks the data for each gridcell for consistency
                    !
                    ! Notes:       For now only checks number of agents. Eventually 
                    !              It should check mor

                    call check_grid_data(grid)
                    call check_area_of_grid(grid,area_for_dens)
                    !call check_density_of_grid(grid,sum(dens,dim=3),x,y)

                


            
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
                    print *, "main t, jp, hum_t, t_hep", t, jp, hum_t(jp)!, t_hep
                enddo

            
            endif
        !call print_information_about_agents(t)
        !call print_dimensions_of_arrays(t)

    enddo timesteps



    call safe_and_close_files()



end program main_program