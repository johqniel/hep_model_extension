program main_grid_test

use mod_grid
use mod_matrix_calculations
    ! We use the following functions: 
    !     - allocate_memory_and_open_files
    !     - setup_initial_conditions
    !          
    !       These two functions allocate the hep variables etc.
use mod_setup_hep
    ! We use dlon_hep, dlat_hep (dimensions of hep grid)
    !        are_for_dens, (dimension dlon_hep x dlat_hep, contains area of gridcells)
    !                       area is computed in allocate_memory and open files
    !                       Ideally this is eventually computed in mod_grid
use mod_agent_class 
use mod_setup_agents

implicit none 

    type(spatial_grid) :: grid
    integer :: nx, ny
    type(Node), target :: agent_one
    type(Node), pointer :: agent_head 

        ! Define your agents
    type(Node), pointer :: agent1, agent2, agent3, agent4

    ! Temporary pointer_node pointer for manipulation
    type(pointer_node), pointer :: temp_list


    ! Define grid size 

    agent_head => agent_one

    nx = dlon_hep
    ny = dlat_hep

    ! Allocate hep etc. 
    print *, "allocate memory and open files"
    call allocate_memory_and_open_files()   

    print *, "setup initial conditions"
    call setup_initial_conditions()  

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Setup for the agent list
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    print *, "initilize agents array"
    call initilize_agents_array()
    print *, "initilize dead agents array"
    call initilize_dead_agents_array() 
    print *, "initilize hum_id_mirror array"
    call initilize_agent_array_mirror_of_hum_id(hum_max_A, npops) ! This will create the mirror array for the agents
    print *, "setup agents from matrix"
    call setup_agents_from_matrix() ! This will create the linked list of agents             


    

    call grid%allocate_grid()
    print *, "grid allocated"

    call grid%initialize_grid(agent_head)

    print*, "grid initilized"

    ! initilize each cell 



    ! Initialize agents' data
    agent1 => agents_array(1)%node
    agent2 => agents_array(2)%node
    agent3 => agents_array(3)%node

    agent4 => agents_array(4)%node


    print *, "=== Append siblings to agent1 ==="
    call append_pointer_node(agent1%siblings, agent2)
    call append_pointer_node(agent1%siblings, agent3)
    call append_pointer_node(agent1%siblings, agent4)
    call print_siblings(agent1)

    print *, "=== Search for agent3 in agent1's siblings ==="
    if (search_pointer_node(agent1%siblings, agent3)) then
        print *, "Agent3 found in siblings!"
    else
        print *, "Agent3 NOT found in siblings."
    endif

    print *, "=== Remove agent3 from agent1's siblings ==="
    call remove_pointer_node(agent1%siblings, agent3)
    call print_siblings(agent1)

    print *, "=== Remove agent2 (head) from agent1's siblings ==="
    call remove_pointer_node(agent1%siblings, agent2)
    call print_siblings(agent1)

    print *, "=== Remove agent4 (tail) from agent1's siblings ==="
    call remove_pointer_node(agent1%siblings, agent4)
    call print_siblings(agent1)

    print *, "=== Final siblings list should be empty ==="
    call print_siblings(agent1)

contains 


    subroutine print_siblings(agent)
        type(Node), intent(in) :: agent
        type(pointer_node), pointer :: current

        print *, "Siblings of agent with id=", agent%id
        current => agent%siblings
        if (.not. associated(current)) then
            print *, "  No siblings."
            return
        endif

        do while (associated(current))
            if (associated(current%node)) then
                print *, "  Sibling id: ", current%node%id
            else
                print *, "  Sibling node pointer is null."
            endif
            current => current%next
        end do
    end subroutine print_siblings

end program main_grid_test   