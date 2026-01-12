module mod_counter

    implicit none
    
    type :: world_debug_counters

        integer :: gxgy_out_counter = 0

        integer :: update_pos_calls = 0
        integer :: move_calls = 0
        
        ! Death Counters
        integer :: death_natural = 0
        integer :: death_starvation = 0
        integer :: death_out_of_bounds = 0
        integer :: death_conflict = 0  ! Placeholder
        integer :: death_random = 0
        
    end type world_debug_counters

end module mod_counter