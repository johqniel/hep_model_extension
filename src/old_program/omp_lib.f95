module omp_lib

    implicit none

    !Defines standard variable precisions of the OpenMP-implementation
    integer, parameter :: OMP_integer_kind   = 4
    integer, parameter :: OMP_logical_kind   = 4
    integer, parameter :: OMP_lock_kind      = 8
    integer, parameter :: OMP_nest_lock_kind = 8

    !Defines the OpenMP version: OpenMP Fortran API v2.0
    integer, parameter :: openmp_version = 200011

    !Gives the explicit interface for each routine of the run-time library
    interface
        subroutine OMP_set_num_threads(number_of_threads)
        integer(kind = 4), intent(in) :: number_of_threads
        end subroutine OMP_set_num_threads

        function OMP_get_num_threads()
        integer(kind = 4) :: OMP_get_num_threads
        end function OMP_get_num_threads

        function OMP_get_max_threads()
        integer(kind = 4) :: OMP_get_max_threads
        end function OMP_get_max_threads

        function OMP_get_thread_num()
        integer(kind = 4) :: OMP_get_thread_num
        end function OMP_get_thread_num

        function OMP_get_num_procs()
        integer(kind = 4) :: OMP_get_num_procs
        end function OMP_get_num_procs

        function OMP_in_parallel()
        logical(kind = 4) :: OMP_in_parallel
        end function OMP_in_parallel

        subroutine OMP_set_dynamic(enable)
        logical(kind = 4), intent(in) :: enable
        end subroutine OMP_set_dynamic

        function OMP_get_dynamic()
        logical(kind = 4) :: OMP_get_dynamic
        end function OMP_get_dynamic

        subroutine OMP_set_nested(enable)
        logical(kind = 4), intent(in) :: enable
        end subroutine OMP_set_nested

        function OMP_get_nested()
        logical(kind = 4) :: OMP_get_nested
        end function OMP_get_nested

        subroutine OMP_init_lock(var)
        integer(kind = 8), intent(out) :: var
        end subroutine OMP_init_lock

        subroutine OMP_init_nest_lock(var)
        integer(kind = 8), intent(out) :: var
        end subroutine OMP_init_nest_lock

        subroutine OMP_destroy_lock(var)
        integer(kind = 8), intent(inout) :: var
        end subroutine OMP_destroy_lock

        subroutine OMP_destroy_nest_lock(var)
        integer(kind = 8), intent(inout) :: var
        end subroutine OMP_destroy_nest_lock

        subroutine OMP_set_lock(var)
        integer(kind = 8), intent(inout) :: var
        end subroutine OMP_set_lock

        subroutine OMP_set_nest_lock(var)
        integer(kind = 8), intent(inout) :: var
        end subroutine OMP_set_nest_lock

        subroutine OMP_unset_lock(var)
        integer(kind = 8), intent(inout) :: var
        end subroutine OMP_unset_lock

        subroutine OMP_unset_nest_lock(var)
        integer(kind = 8), intent(inout) :: var
        end subroutine OMP_unset_nest_lock

        function OMP_test_lock(var)
        logical(kind = 4) :: OMP_test_lock
        integer(kind = 8), intent(inout) :: var
        end function OMP_test_lock

        function OMP_test_nest_lock(var)
        integer(kind = 4) :: OMP_test_nest_lock
        integer(kind = 8), intent(inout) :: var
        end function OMP_test_nest_lock

        function OMP_get_wtime()
        real(8) :: OMP_get_wtime
        end function OMP_get_wtime

        function OMP_get_wtick()
        real(8) :: OMP_get_wtick
        end function OMP_get_wtick
    end interface
end module omp_lib



