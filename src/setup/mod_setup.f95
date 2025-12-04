module mod_setup

    use mod_config

    implicit none
    
    contains

    function read_world_config() result(config)
        implicit none
        type(world_config) :: config



    end function read_world_config

end module mod_setup