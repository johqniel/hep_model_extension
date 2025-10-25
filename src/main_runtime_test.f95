program test_runtime
  implicit none
  use mod_agent_class
  use mod_test_runtime

  ! Variables for clock
   integer :: count_0, count_1
   integer :: count_rate, count_max
   double precision :: time_init, time_final, elapsed_time

   ! Variables for test calculations

   real(8), allocatable(:,:) :: x,y,ux,uy
   type(Node), allocatable(:,:) :: a_matrix

  ! Allocate and fill Matrixes with data:
  
   call setup_matrix(x)
   call setup_matrix(y)
   call setup_matrix(ux)
   call setup_matrix(uy)

   call setup_a_matrix(a_matrix)
  

   ! Starting time
   call system_clock(count_0, count_rate, count_max)
   time_init = count_0*1.0/count_rate




   ! Ending time
   call system_clock(count_1, count_rate, count_max)
   time_final = count_1*1.0/count_rate
   ! Elapsed time
   elapsed_time = time_final - time_init

   ! Write elapsed time
   write(*,1003) int(elapsed_time),elapsed_time-int(elapsed_time)

      ! Starting time
   call system_clock(count_0, count_rate, count_max)
   time_init = count_0*1.0/count_rate




   ! Ending time
   call system_clock(count_1, count_rate, count_max)
   time_final = count_1*1.0/count_rate
   ! Elapsed time
   elapsed_time = time_final - time_init

   ! Write elapsed time
   write(*,1003) int(elapsed_time),elapsed_time-int(elapsed_time)



      ! Starting time
   call system_clock(count_0, count_rate, count_max)
   time_init = count_0*1.0/count_rate




   ! Ending time
   call system_clock(count_1, count_rate, count_max)
   time_final = count_1*1.0/count_rate
   ! Elapsed time
   elapsed_time = time_final - time_init

   ! Write elapsed time
   write(*,1003) int(elapsed_time),elapsed_time-int(elapsed_time)

end program
