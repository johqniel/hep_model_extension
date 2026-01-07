program reproduce_error
    use mod_read_inputs
    use mod_config
    implicit none

    type(world_config) :: config
    type(hep_data_type) :: hep_data

    print *, "Attempting to read testing_config.nml..."
    
    call set_config_path("input/config/testing_config.nml")
    call read_inputs(config, hep_data)
    
    print *, "Success!"

end program reproduce_error
