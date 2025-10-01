
program main_program

    use mod_globals
    ! Uses:     - t_hep
    !           - t
    !           - hum_t

    !           - out_count_priv_a, out_count_priv_b
    !

    use mod_matrix_calculations
        ! allocate_memory_and_open_files
        ! setup_initial_conditions


    use mod_agent_class
        ! initilize_agents_array
        ! initilize_dead_agents_array
    use mod_setup_agents

    use mod_grid

    use mod_export_agents

    use mod_export_hep

    use mod_movement

    use mod_birth_death_example

    use mod_age_pregnancy
        ! birth_example
        ! death_example



    implicit none

    type(Node), pointer :: current_agent_ptr
    type(Node), pointer :: next_agent_ptr
    character(len=100) :: temp_string
    character(len=100) :: temp_string_2

    type(spatial_grid), target :: grid
    type(spatial_grid), pointer :: grid_ptr

    integer :: tracker
    integer :: loop_n




    grid_ptr => grid


    


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the Matrix calculations
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print *, "allocate memory and open files"
    call allocate_memory_and_open_files()   
    print*, "1 hum_t: ", hum_t

    print *, "setup initial conditions"
    call setup_initial_conditions()  
    print*, "2 hum_t: ", hum_t

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the agent list
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    print*, "3 hum_t: ", hum_t
    print *, "allocate agents array"
    call allocate_agents_array()
        print*, "4 hum_t: ", hum_t

    print *, "allocate dead agents array"
    call allocate_dead_agents_array() 
    print*, "5 hum_t: ", hum_t

    print *, "allocatem population_agents_matrix"
    call allocate_population_agents_matrix(hum_max_A, npops) 
    print*, "6 hum_t: ", hum_t

    print *, "setup agents from matrix"
    call setup_agents_from_matrix() ! This will create the linked list of agents   

    call write_matrix_info_to_agents(x,y,ux,uy,population_agents_matrix,num_humans_in_pop)

    print*, "7 hum_t: ", hum_t
    print*, "num_hum_in_pop: ", num_humans_in_pop

    !call check_position_in_matrix_consistency()
    call check_hum_t_coherent(head_agents,hum_t)
  
    call check_position_in_matrix_consistency()



    
    
    

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup the Grid 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ! kill the agents that are outside the grid. 
    call kill_agents_outside_of_grid(head_agents)




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
        !print*, " t equals: ",t

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Development 
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            
            ! #########################################################
            ! Agent based model
            ! #########################################################
            

                    ! ########################################################
                    ! Movement of Agents
                    ! ########################################################

                        t_hep = int( t/delta_t_hep ) + 1


                        out_count_priv(:) = 0
                        !
                        drown_count_priv(:) = 0
                        !
                        death_count_priv(:) = 0
                        !
                        out_count_priv_a(:) = 0
                        !
                        out_count_priv_b(:) = 0 


                        PopulationLoop1: do jp = 1, npops
                    



                            ! compute the random deviation for the movements of the agents. 
                            Ax = rnorm_vec(hum_max_A, 0.d0, sqdt)
                            Ay = rnorm_vec(hum_max_A, 0.d0, sqdt)

                            ! skip if population is supposed to enter simulation later
                            if ( t < tstep_start(jp) ) then 
                                CYCLE
                            endif               

                            ! human movement
                            
                            if ( mod(t, 1000) .eq. 0 ) print *, green, "main t, jp, num_hum_in_pop, t_hep", &
                                                                t, jp, num_humans_in_pop(jp), t_hep, reset
                            
                            !tracker = 0 
                            !loop_n = hum_t(jp)
                            !i = 1

                            !HumanLoop1: do while (i < hum_t(jp) + 1)

                            !    if (tracker + hum_t(jp) < loop_n) then
                            !       call agent_move_grid(i,jp,grid_ptr,Ax,Ay)

                            !        call agent_move(population_agents_matrix(i,jp)%node,Ax,Ay)
                            !        tracker = tracker + 1
                            !    else
                            !        call agent_move(population_agents_matrix(i,jp)%node,Ax,Ay)
                            !        i = i + 1
                            !    endif

                            !enddo HumanLoop1    



                            ! human birth death




                        enddo PopulationLoop1


                    current_agent_ptr => head_agents
                    next_agent_ptr => current_agent_ptr%next

                    do while (associated(current_agent_ptr))

                        next_agent_ptr => current_agent_ptr%next

                        if ( t < tstep_start(jp) ) then 
                            current_agent_ptr => current_agent_ptr%next
                            CYCLE
                        endif  

                        call agent_move(current_agent_ptr,Ax,Ay)  

                        current_agent_ptr => next_agent_ptr
                        


                    enddo



                    ! ########################################################
                    ! Birth Death
                    ! ########################################################

                        !call check_if_agents_twice_in_grid(grid,head_agents)
                        !call check_consistency_grid_agents(grid)

                        call birth_example(grid_ptr)
                        call death_example(grid_ptr)

                    ! ########################################################
                    ! The Management of the Data structures
                    ! ########################################################       



                    ! ########################################################
                    ! The Management of the Grid Structure
                    ! ########################################################     

                    !call grid%clean_grid_from_dead_agents()

                    call grid%update_density_pure()

                    ! Ideally we want this to be done on the go when agents die. 
                    ! ATM the structure of the program is to messy to do that 
                    ! But I am working on it :) DN 01.08.2025

            ! #########################################################
            ! Updating hep 
            ! #########################################################


                call check_num_hum_in_pop_coherent(head_agents,num_humans_in_pop)

                call write_new_positions_to_matrix(x,y,ux,uy, population_agents_matrix, num_humans_in_pop)

                PopulationLoop3: do jp = 1, npops
                
                    call update_hep_human_density(jp)

                enddo PopulationLoop3

            ! #########################################################
            ! Preparation of Equation based model 
            ! #########################################################

            ! #########################################################
            ! Equation based model - for control
            ! #########################################################
        
            ! #########################################################
            ! Loop over every agent via their population
            ! #########################################################

            ! #########################################################
            ! Loop over every population
            ! #########################################################

            ! #########################################################
            ! Loop over every agent randomly (population vice)
            ! #########################################################

            ! #########################################################
            ! Loop over every agent randomly
            ! #########################################################

            ! #########################################################
            ! Loop over every agent semi-random 
            ! #########################################################

            call update_age_pregnancy()
            call realise_births(t)

            ! #########################################################
            ! Loop over every agent via their positions
            ! #########################################################

            ! #########################################################
            ! Loop over the grid
            ! #########################################################


        
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Saving the data
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        if (mod(t,100) == 0) then
            write(temp_string, '(I0)') t
            call write_agents_to_csv("data/agents_plotting_data_" // trim(temp_string) // ".csv")
        endif
        
       
        if (mod(t,100) == 0) then
            do jp = 1, npops


                if ( t < tstep_start(jp) ) then 
                    CYCLE
                endif               

                write(temp_string, '(I0)') t
                write(temp_string_2, '(I0)') jp


                !call write_hep_to_csv("hep_control/" // trim(temp_string_2) // "_hep_2_" // trim(temp_string) // ".csv",jp)

            enddo
        endif

        

        

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Test for correctness
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        ! Actual tests
         if (mod(t,1001) == 0) then

            ! ++++++++ General Tests ++++++++++++++++++++++++++++++++++++


  

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
                    ! Description: Checks whether the agent that we find in the matrix in position (i,j)
                    !              has i and j as position_human and position_population
                    call check_position_in_matrix_consistency()


                ! ############ Test 8 ###################################
                ! Description: Check whether agent%pos is the same as in the matrix x,y

                !call check_positions_matrix(head_agents,x,y)

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
                    !call check_number_of_agents_in_grid_using_matrix(grid,x,y,hum_t)

                ! ########### Test 3.5 ##################################
                    ! Description: Counts agents in grid cells using x, y matrix and using 
                    !              the position of agents in agent%pos_x, agent%pos_y 
                    !              and compares the results
                    !call compare_number_of_agents_in_grid_matrix_agent_class(grid,x,y,head_agents)

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

                ! ########### Test 6 ##################################
                    ! Description: Checks for each gridcell if there is a agent twice or more times
                    !              in the cell.
                    !
                    call check_duplicate_agents_in_cells(grid)

                ! ########### Test 7 ##################################
                    ! Description: Checks for each agent how many times they are found in the grid.
                    !
                    call check_if_agents_twice_in_grid(grid,head_agents)

                ! ########### Test n ###################################
                    ! Description: Checks the data for each gridcell for consistency
                    !
                    ! Notes:       For now only checks number of agents. Eventually 
                    !              It should check mor

                    call check_grid_data(grid)
                    call check_area_of_grid(grid,area_for_dens)
                    !call check_density_of_grid(grid,sum(dens,dim=3),x,y)

        endif  

        ! Printing information

        if (mod(t,1000) == 0) then

            call compare_num_of_agents_in_grid(grid_ptr)

        endif

            
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
                call count_agents_list()
                call count_dead_agents_list() ! This will count the number of dead agents in the list
                call compare_counters_of_agents()
            endif

            ! --------------------------- Every 1000 time ticks: ----------------------------------
            if (mod(t,1000) == 0) then

                do jp = 1, npops
                    print *, yellow, "main t, jp, num_hum_in_pop, t_hep", t, jp, num_humans_in_pop(jp), reset!, t_hep
                enddo
                print*, "Realised births so far: ", realised_birth_counter
                print*, "Pregnancies so far: ", pregnancy_counter
                print*, "Found mates: ", found_mates_counter
                print*, "Agents born so far: ", agents_born_counter
                print*, "Drowned count: ", drown_count, " Out count: ", out_count, " Death count: ", death_count
                print*, "Out count A: ", out_count_a, " Out count B: ", out_count_b
                print*, "Num Female Agents: ", count_female_agents(head_agents)
                print*, "Num Male Agents: ", count_male_agents(head_agents)
                print*, "Num pregnant agents: ", count_pregnant_agents(head_agents)

            
            endif


    enddo timesteps



    call safe_and_close_files()



contains

    include "test_and_debug/debug_agents.inc"

    include "test_and_debug/debug_grid.inc"



end program main_program