program main_program

    use mod_agent_world

    use mod_modules_hash

    implicit none

    type(world_container), target :: world



    call world%init_world()
    call world%setup_world()



    contains

    subroutine apply_modules_to_agents()

        implicit none

    end subroutine apply_modules_to_agents


end program main_program