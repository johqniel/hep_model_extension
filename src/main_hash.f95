
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

    use mod_agent_hashmap

    use mod_agent_core



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
    type(spatial_grid), target :: grid
    type(spatial_grid), pointer :: grid_ptr

    ! the agents
    type(Agent), allocatable, target :: agents(:,:)
    type(t_int_map), target :: agent_id_index_map
    integer, allocatable, target :: num_humans_in_array(:)

    ! other
    integer :: t_hep_length


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
    call setup_agents_from_matrix_hash(agents, agent_id_index_map, num_humans_in_array) ! This will create the linked list of agents   

    call write_matrix_info_to_agents(x,y,ux,uy,population_agents_matrix,num_humans_in_pop)


  



    
    
    

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup the Grid 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ! kill the agents that are outside the grid. 
    !call kill_agents_outside_of_grid(head_agents)




    grid%nx = dlon_hep
    grid%ny = dlat_hep

    call grid%allocate_grid()
    print *, "grid allocated"

    !call grid%initialize_grid(head_agents)


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Safe hep 
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    t_hep_length = size(hep, 4)
    call write_hep_binary_with_dims("hep_control/hep.bin", "hep_control/hep_dims.txt", hep, lon_hep, lat_hep)


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Tests before the Main Calculation
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print* , "Run Tests before Main Calculation ... "

        ! ######### Testing the hep #################################


        ! ######## Testing the agents ##############################



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


                        out_count_priv(:) = 0
                        !
                        drown_count_priv(:) = 0
                        !
                        death_count_priv(:) = 0
                        !
                        out_count_priv_a(:) = 0
                        !
                        out_count_priv_b(:) = 0 
                    ! ########################################################
                    ! Modules that apply to all agents individually
                    ! ########################################################




                        

                        !call apply_module_to_agents(update_age_pregnancy,t)

                        !call apply_module_to_agents(agent_move,t)

                        !call apply_module_to_agents(find_mate,t)

                        !call apply_module_to_agents(realise_births,t)

                        !call apply_module_to_agents(realise_natural_deaths,t)



                    ! ########################################################
                    ! Modules that apply to all agents in a cell 
                    ! ########################################################


                        !call death_example(grid_ptr)



                    ! ########################################################
                    ! The Management of the Grid Structure
                    ! ########################################################     


                        !call grid%update_density_pure()

                    ! Ideally we want this to be done on the go when agents die. 
                    ! ATM the structure of the program is to messy to do that 
                    ! But I am working on it :) DN 01.08.2025

            ! #########################################################
            ! Updating hep 
            ! #########################################################



                !call write_new_positions_to_matrix(x,y,ux,uy, population_agents_matrix, num_humans_in_pop)

                PopulationLoop3: do jp = 1, npops
                
                    !call update_hep_human_density(jp)

                enddo PopulationLoop3

            ! #########################################################
            ! Preparation of Equation based model 
            ! #########################################################

            ! #########################################################
            ! Equation based model - for control
            ! #########################################################
        

        
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Saving the data                                             <- Here we export the data
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            if (mod(t,output_interval_visualization_data) == 0) then
                !write(temp_string, '(I0)') t
                !call write_agents_to_csv("data/agents_plotting_data_" // trim(temp_string) // ".csv",t)
            endif
            
        
            if (mod(t,100) == 0) then
                do jp = 1, npops


                    if ( t < tstep_start(jp) ) then 
                        CYCLE
                    endif               

                    write(temp_string, '(I0)') t
                    write(temp_string_2, '(I0)') jp



                enddo
            endif

        

        

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Test for correctness
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            ! Actual tests
            if (mod(t,1001) == 0) then

            ! ++++++++ General Tests ++++++++++++++++++++++++++++++++++++



            ! +++++++++ Tests of the grid +++++++++++++++++++++++++++++++++++++++++++++++++++++


            endif  

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! Printing information
        !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            if (mod(t,1000) == 0) then


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

            endif

            ! --------------------------- Every 1000 time ticks: ----------------------------------
            if (mod(t,1000) == 0) then

    

            
            endif


    enddo timesteps



    call safe_and_close_files()



contains

    include "test_and_debug/debug_agents.inc"

    include "test_and_debug/debug_grid.inc"

subroutine apply_module_to_agents(func,t)
    implicit none
    integer, intent(in) :: t
    interface
      subroutine func(agent_ptr)
        import :: Node
        type(Node), pointer, intent(inout) :: agent_ptr
      end subroutine func
    end interface

    type(Node), pointer :: current
    type(Node), pointer :: next

    current => head_agents

    next => current%next

    do while (associated(current))

        next => current%next

        jp = current%position_population

        if ( t < tstep_start(jp) ) then 
            current => current%next
            cycle
        endif  

        call func(current)

        current => next
                        


    enddo

end subroutine apply_module_to_agents



end program main_program