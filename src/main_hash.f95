
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

    use mod_grid_id

    use mod_hashmap

    use mod_agent_core

    use mod_modules_hash

    use mod_export_agents_hash



    implicit none

    ! vars for command line arguments
    integer :: output_interval_visualization_data  
    real :: test_arg

    integer :: argc ! number of arguments passed
    character(len=100) :: arg
    character(len=100) :: value_str

    character(len=100) :: temp_string
    character(len=100) :: temp_string_2

    ! the grid
    type(Grid), target :: grid
    type(Grid), pointer :: grid_ptr

    ! the agents
    type(Agent), allocatable, target :: agents(:,:)
    type(t_int_map), target :: agent_id_index_map
    integer, allocatable, target :: num_humans_in_array(:)
    type(Agent), pointer :: current_agent => null()

    ! other
    integer :: t_hep_length

    ! For testing

    integer, allocatable, target :: num_dead_agents_in_array(:)


    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read Command-Line Arguments 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    output_interval_visualization_data = 1000  ! Default value
    test_arg = 0.
    

    argc = command_argument_count()




    ! --- Argument Parsing Loop ---
    argc = command_argument_count()
    i = 1
    do while (i <= argc)
        call get_command_argument(i, arg)

        if (trim(arg) == '--output_interval') then
            ! The value is the *next* argument
            i = i + 1
            call get_command_argument(i, value_str)
            read(value_str, *) output_interval_visualization_data
        
        else if (trim(arg) == '--test_arg') then
            i = i + 1
            call get_command_argument(i, value_str)
            read(value_str, *) test_arg
        
        ! Add more 'else if' blocks here for new parameters

        end if

        i = i + 1
    end do

    ! Print the final values to confirm they were read correctly
    print *, "--- Simulation Parameters ---"
    print *, "Output Interval: ", output_interval_visualization_data
    print *, "test_arg:  ", test_arg
    print *, "---------------------------"


    grid_ptr => grid


    


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the Matrix calculations
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print *, "allocate memory and open files"
    call allocate_memory_and_open_files()   
    !print*, "1 hum_t: ", hum_t

    print *, "setup initial conditions"
    call setup_initial_conditions()  
    !print*, "2 hum_t: ", hum_t

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the agent list
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !print*, "3 hum_t: ", hum_t
    print *, "allocate agents array"
    call allocate_agents(agents,num_humans_in_array,npops,hum_max_A)

    !    print*, "4 hum_t: ", hum_t



    print *, "setup agents from matrix"
    call setup_agents_from_matrix_hash(agents, agent_id_index_map, num_humans_in_array) 

    !call write_matrix_info_to_agents(x,y,ux,uy,population_agents_matrix,num_humans_in_pop)


  



    
    
    

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup the Grid 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ! kill the agents that are outside the grid. 
    !call kill_agents_outside_of_grid(head_agents)




    grid%nx = dlon_hep
    grid%ny = dlat_hep

    call grid%allocate_grid()
    print *, "grid allocated"

    call grid%initialize_grid(agents, num_humans_in_array)


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Safe hep 
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    t_hep_length = size(hep, 4)


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Tests before the Main Calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print* , "Run Tests before Main Calculation ... "

        ! ######### Testing the hep #################################


        ! ######## Testing the agents ##############################

        allocate(num_dead_agents_in_array(size(num_humans_in_array)))
        call count_dead_agents(agents,num_humans_in_array,num_dead_agents_in_array)

        print*, "Dead agents in array: ", num_dead_agents_in_array            


        ! ######### Testing the grid ################################


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
            ! Agent based model                                        <- Here we apply the modules to agents
            ! #########################################################

                    ! #########################################################
                    ! Set (debugging) variables
                    ! #########################################################
                        t_hep = int( t/delta_t_hep ) + 1



                    ! ########################################################
                    ! Modules that apply to all agents individually
                    ! ########################################################


                    do j = 1, npops
                        do i = 1, num_humans_in_array(j)

                            current_agent => agents(i,j)
                            if (current_agent%is_dead) then
                                cycle
                            endif
                            call agent_move(current_agent)
                        enddo
                    enddo


                        

                        !call apply_module_to_agents(update_age_pregnancy,t)

                        !call apply_module_to_agents(agent_move,t)

                        !call apply_module_to_agents(find_mate,t)

                        !call apply_module_to_agents(realise_births,t)

                        !call apply_module_to_agents(realise_natural_deaths,t)

                    call compact_agents(agents, agent_id_index_map, num_humans_in_array)
                    

                    if (mod(t,1000) == 0) then
                        print *, "Timestep: ", t, " Number of agents: ", num_humans_in_array
                        call count_dead_agents(agents,num_humans_in_array,num_dead_agents_in_array)

                        print *, "Counted dead agents: ", num_dead_agents_in_array

                    end if



                    ! ########################################################
                    ! Modules that apply to all agents in a cell 
                    ! ########################################################


                        !call death_example(grid_ptr)



                    ! ########################################################
                    ! The Management of the Grid Structure
                    ! ########################################################     


                        call grid%update_density_pure()

                    ! Ideally we want this to be done on the go when agents die. 
                    ! ATM the structure of the program is to messy to do that 
                    ! But I am working on it :) DN 01.08.2025

            ! #########################################################
            ! Updating hep 
            ! #########################################################


                call write_new_positions_to_matrix(x,y,ux,uy, agents, num_humans_in_array)


                PopulationLoop3: do jp = 1, npops
                
                    call update_hep_human_density(jp)

                enddo PopulationLoop3


        

        
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Saving the data
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        if (mod(t,output_interval_visualization_data) == 0) then
            write(temp_string, '(I0)') t
            call write_agents_to_csv_hash("data/agents_plotting_data_" // trim(temp_string) // ".csv",t &
                                          ,agents,num_humans_in_array,npops)
        endif

        

   


    enddo timesteps







contains


subroutine apply_module_to_agents()

end subroutine apply_module_to_agents



end program main_program