module matrix_management
    use mod_setup
    use model_agent_class

    ! WORK IN PROGRESS


    ! This module manages the matrix to make sure that the matrix is consistent with the agent list
    ! It uses the followind matrices:

    ! - model specific variables: 
        ! - x: the x coordinate of the agents
        ! - y: the y coordinate of the agents
        ! - ux: the x velocity of the agents
        ! - uy: the y velocity of the agents
    ! - data managment specific variables: 
        ! - hum_id: the human id of the agents
        ! - is_dead: a logical array that indicates if an agent is dead
        ! - population_agents_array: an array that contains the agents of each population

    ! All of the matrices have the same dimensions: npop x hum_max_A, i.e. the number of populations times the maximum number of agents per population
    

    contains
    subroutine rearange_matrix_and_kill_agents()
        integer :: counter, jp , i 
        type(Node), pointer :: agent
        

        counter = 0
        ! This subroutine move all the active agents to the front 
        do jp = 1, npops
            do i = 1, hum_t(jp)
                agent => population_agents_array(i,jp)%node
                ! if the agent was mnot marked as dead it will be moved to the front of the matrix
                if (is_dead(i,jp) .eqv. .false.) then
                    ! count the number of active agents
                    c = c + 1

                    ! copy the variables of the model
                    x0(c,jp) = x(i,jp)
                    y0(c,jp) = y(i,jp)
                    ux0(c,jp) = ux(i,jp)
                    uy0(c,jp) = uy(i,jp)

                    ! copy the variables of the data management
                    if (ASSOCIATED(population_agents_array(j,jp)%node)) THEN
                            population_agents_array0(c,jp) = population_agents_array(j,jp)                              
                            population_agents_array0(c,jp)%node%position_human = c 
                        else  
                          print *, "active agent to be moved to beginning of matrix is not associated, c, jp, j", c, jp, j ! DN debugging 10.06.25
                    endif   
                    hum_id_0(c,jp) = hum_id(i,jp)
                    is_dead0(c,jp) = is_dead(i,jp)
                ! if the agent was marked as dead it has to be moved from the list of 
                else
                    if (.not. associated(agent))) then
                        print *, "Agent to be removed is not associated with the population_agents_array."
                    else 
                        call agent.agent_die()
                    endif
                endif
            enddo
            ! Set new number of agents in the population
            hum_t(jp) = c 
        
        enddo

        
        ! Now we have to copy the variables back to the original matrix    
            ! Model Variables:                                                  
            x(:,jp) = x0(:,jp)                                                                           
            y(:,jp) = y0(:,jp)                                                                             
            ux(:,jp) = ux0(:,jp)                                                                          
            uy(:,jp) = uy0(:,jp)

            ! Data Management Variables:
            hum_id(:,jp) = hum_id_0(:,jp)                                                                 
            is_dead(:,jp) = is_dead0(:,jp)                                                                            
            population_agents_array(:,jp) = population_agents_array0(:,jp)

    end subroutine rearange_matrix_and_kill_agents

    subroutine 


end module matrix_management