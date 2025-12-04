module mod_config

    implicit none

    ! =================================================================================
    ! World Config 
    ! =================================================================================
    
        ! Agents
        integer :: npops = 3

        ! Grid
        integer :: nx
        integer :: ny

        type :: world_config

            integer :: npops
            integer :: initial_hashmap_size
            integer :: initial_max_pop_size

        end type world_config


    ! ================================================================================
    ! The Grid structure
    ! ================================================================================

    integer, parameter :: initial_array_size_for_agents_ids_in_gridcell = 17

    ! ===============================================================
    ! The hashmap <-> agent array structure
    ! ===============================================================

    ! parameters
    integer, parameter :: initial_agent_array_size = 1000
    integer, parameter :: agent_array_resize_factor = 2
    integer, parameter :: initial_hashmap_size = 2003
    integer, parameter :: hashmap_resize_factor = 2
    real, parameter :: MAX_LOAD_FACTOR = 0.75


    


end module mod_config
