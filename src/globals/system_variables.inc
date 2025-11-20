
! The grids data structure (array)
integer, parameter :: max_agents_per_cell = 17

integer, parameter :: initial_array_size_for_agents_ids_in_gridcell = 17

! ===============================================================
! The hashmap <-> agent array structure
! ===============================================================

! parameters
integer, parameter :: initial_agent_array_size = 1000
integer, parameter :: agent_array_resize_factor = 2
integer, parameter :: initial_hashmap_size = 2003
integer, parameter :: hashmap_resize_factor = 2
real, parameter :: MAX_LOAD_FACTOR = 0.75

