

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
  use mod_hashmap
  use mod_agent_class
  implicit none

  ! --- Variable Declarations ---

  ! The map itself
  type(t_int_map), target :: my_map

  ! Agents for testing: 

  type(Node) :: temp_agent
  integer :: i, j, id_counter
    integer :: num_hum_p_p

  id_counter = 0

  num_hum_p_p = 2000



  print *, "--- Initializing Map ---"
  call init_map(my_map)
  print *, "Initial size:", get_size(my_map)
   
  do i = 1 , num_hum_p_p

    id_counter = id_counter + 1

    temp_agent%id = id_counter
    temp_agent%age = temp_agent%age + temp_agent%id

   

  end do




  print *, "--- Destroying Map ---"
  call destroy_map(my_map)
  print *, "Final size:", get_size(my_map)

end program test_hash