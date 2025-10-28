! main_benchmark.f90
! This program runs the benchmarks for the different calculation methods.
program test_runtime_main
  use mod_test_runtime ! Contains the subroutines to test
  implicit none


  ! --- Configuration ---
  ! Set the size of the matrices for testing
  ! Larger numbers give more stable timings but take longer.
  integer, parameter :: n_rows = 8000
  integer, parameter :: n_cols = 8000
  ! --- End Configuration ---

  ! Declare allocatable arrays for each test case
  real(8), allocatable, dimension(:, :) :: matrix_a
  real(8), allocatable, dimension(:, :) :: x_b, y_b, ux_b, uy_b
  type(container), allocatable, dimension(:, :) :: container_matrix
  type(container_node), pointer :: head_container_nodes => null()

  ! Variables for timing
  real(8) :: t_start, t_end

  ! Allocate all the matrices
  allocate(matrix_a(n_rows, n_cols))
  allocate(x_b(n_rows, n_cols), y_b(n_rows, n_cols), &
           ux_b(n_rows, n_cols), uy_b(n_rows, n_cols))
  allocate(container_matrix(n_rows, n_cols))

  print *, "Starting benchmarks with matrix size: ", n_rows, "x", n_cols
  print *, "================================================="

  ! --- Test 0: test_calculations_matrix ---
  print *, "Running Test Z (Single Matrix)..."
  call initialise_matrix(matrix_a, n_rows, n_cols)


  
  call cpu_time(t_start)
  call test_calculations_matrix_a(matrix_a)
  call cpu_time(t_end)
  
  print *, "  Time for Matrix: ", t_end - t_start, " seconds"
  print *, "-------------------------------------------------"


  ! --- Test 1: test_calculations_matrix x 4 ---
  print *, "Running Test A (4 x Single Matrix)..."
  call initialise_matrix(matrix_a, n_rows, n_cols)
  call initialise_matrix(x_b, n_rows, n_cols)
  call initialise_matrix(y_b, n_rows, n_cols)
  call initialise_matrix(ux_b, n_rows, n_cols)

  
  call cpu_time(t_start)
  call test_calculations_matrix_a(matrix_a)
  call test_calculations_matrix_a(x_b)
  call test_calculations_matrix_a(y_b)
  call test_calculations_matrix_a(ux_b)

  call cpu_time(t_end)
  
  print *, "  Time for 4x Matrix: ", t_end - t_start, " seconds"
  print *, "-------------------------------------------------"

  ! --- Test 2: test_calculations_matrix_b ---
  print *, "Running Test B (Four Separate Matrices)..."
  call initialise_matrix(x_b, n_rows, n_cols)
  call initialise_matrix(y_b, n_rows, n_cols)
  call initialise_matrix(ux_b, n_rows, n_cols)
  call initialise_matrix(uy_b, n_rows, n_cols)
  
  call cpu_time(t_start)
  call test_calculations_matrix_b(x_b, y_b, ux_b, uy_b)
  call cpu_time(t_end)
  
  print *, "  Time for Matrix B: ", t_end - t_start, " seconds"
  print *, "-------------------------------------------------"

    ! --- Test 3: test_calculations_container_matrix_pointer ---
  print *, "Running Test C (Matrix of Type container, entries passed as pointer to function)..."
  call initialise_ag_matrix(container_matrix,n_rows,n_cols)
  
  call cpu_time(t_start)
  call test_calculations_container_matrix_pointer(container_matrix)
  call cpu_time(t_end)
  
  print *, "  Time for Matrix B: ", t_end - t_start, " seconds"
  print *, "-------------------------------------------------"


  ! --- Test 4: test_calculations_container_matrix ---
  print *, "Running Test D (Matrix of Type container)..."
  call initialise_ag_matrix(container_matrix, n_rows, n_cols)
  
  call cpu_time(t_start)
  call test_calculations_container_matrix(container_matrix)
  call cpu_time(t_end)
  
  print *, "  Time for Agents Matrix C: ", t_end - t_start, " seconds"
  print *, "-------------------------------------------------"


    ! --- Test 4: test_calculations_container_matrix ---
  print *, "Running Test E (List of Type container)..."
  call initialize_container_list(head_container_nodes,container_matrix, n_rows, n_cols)
  call count_list_members(head_container_nodes)
  
  call cpu_time(t_start)
  call test_calculations_container_list(head_container_nodes)
  call cpu_time(t_end)
  
  print *, "  Time for Agents List E: ", t_end - t_start, " seconds"
  print *, "================================================="
  print *, "Benchmarks complete."


  ! Clean up memory
  deallocate(matrix_a)
  deallocate(x_b, y_b, ux_b, uy_b)
  deallocate(container_matrix)
  call deallocate_container_list(head_container_nodes)

end program test_runtime_main
