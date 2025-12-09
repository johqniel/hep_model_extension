module mod_counter

    implicit none
    
    type :: counter_container

        integer :: gxgy_out_counter = 0

        integer :: update_pos_calls = 0
        integer :: move_calls = 0
    end type counter_container

end module mod_counter