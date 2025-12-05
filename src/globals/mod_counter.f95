module mod_counter

    implicit none
    
    type :: counter_container
        integer :: gxgy_out_counter = 0
        integer :: realised_birth_counter = 0
        integer :: pregnancy_counter = 0
        integer :: found_mates_counter = 0
        integer :: agents_born_counter = 0
        integer :: drown_count = 0
        integer :: out_count = 0
        integer :: death_count = 0
        integer :: out_count_a = 0
        integer :: out_count_b = 0
    end type counter_container

end module mod_counter