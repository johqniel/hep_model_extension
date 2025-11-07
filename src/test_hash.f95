

! =============================================================================
! Program: test_hash
!
! Author:  Gemini
! Date:    November 7, 2025
!
! Description:
!   A simple program to demonstrate the use of the `int_hash_map_module`.
!   It shows how to:
!   - Initialize and destroy a map
!   - Add (put) items of different types (real, string, integer)
!   - Force a resize
!   - Retrieve (get) items and safely check their type
!   - Check for key existence
!
! Depends on:
!   - int_hash_map_module.f90
! =============================================================================
program test_hash

  use mod_agent_hashmap
  use mod_agent_class

  implicit none

  ! --- Variable Declarations ---

  ! The Data structures to hold and sort agents
  type(t_int_map), target :: hash_map
  type(Agent), allocatable, target :: agents(:)

  ! Variables to orchestrate the data structure management
    integer :: num_agents_alive =  0
    integer :: num_agents_died_recently = 0


! temp variables
  type(Agent) :: temp_agent
  type(Agent), pointer :: temp_agent_ptr => null()
  integer :: i, j, id_counter
    integer :: num_hum_p_p

  id_counter = 0

  num_hum_p_p = 2000



  print *, "--- Initializing Map ---"
  call init_map(hash_map)
  print *, "Initial size:", get_size(hash_map)

  print*, " Allocating agents array..."

  allocate(agents(num_hum_p_p))
   

    do i = 1, num_hum_p_p

    

      call add_agent_to_array_hash(agents, hash_map, create_agent(id_counter),  num_agents_alive)

    end do

print *, " Number of agents alive:", num_agents_alive
print *, " Map size after adding agents:", get_size(hash_map)

    do t = 1, 10000

        !tests
        call check_agents_hashmap_consistency(agents, hash_map, num_agents_alive)

        call add_agent_to_array_hash(agents, hash_map, create_agent(id_counter),  num_agents_alive)

        ! Randomly remove a agent: 
        i = 1
        temp_agent_ptr => agents(1)
        do while (temp_agent_ptr%is_dead .and. i < size(agents))
            i = i + 1
            temp_agent_ptr => agents(i)
        enddo


        call agent_dies(temp_agent_ptr, hash_map, num_agents_died_recently)



        call add_agent_to_array_hash(agents, hash_map, create_agent(id_counter),  num_agents_alive)

        if (mod(t,100) == 0) then
        print*, ""
        print*, ""
            print *, "Time step:", t
            print*, "Size agents array:",  size(agents)
            print*, "Capacity hashmap: ", get_capacity(hash_map)
            print *, " Number of agents alive:", num_agents_alive
            print *, " Number of agents died recently:", num_agents_died_recently
            call compact_agents(agents, hash_map, num_agents_died_recently,num_agents_alive)

        end if

    enddo


  print *, "--- Destroying Map ---"
  call destroy_map(hash_map)
  print *, "Final size:", get_size(hash_map)



  contains



    function create_agent(static_id_counter) result(new_agent)
        implicit none
        integer, intent(inout):: static_id_counter

        type(Agent), target :: new_agent
        static_id_counter = static_id_counter + 1
        new_agent%id = static_id_counter
        new_agent%age = 0
        new_agent%is_dead = .false.

    end function create_agent

    include "test_and_debug/debug_hash_agents.inc"


end program test_hash