module mod_test_runtime
  implicit none

  contains:

    subroutine initialise_matrix(matrix,n,m)

    end subroutine initialise_matrix

    
    subroutine initialise_ag_matrix(matrix,n,m)

    end subroutine initialise_ag_matris


    subroutine test_calculations_matrix_a(matrix)
      implicit none
      real(8), dimension(:,:), intent(inout) :: matrix

      integer :: i,j

      do j = 1, size(matrix,2):
        do i = 1, size(matrix,1):
          matrix(i,j) = example_calculation(matrix(i,j))
        end do
      end do

    end subroutine

    subroutine test_calculations_matrix_b(x,y,ux,uy)
      implicit none
      real(8), dimension(:,:), intent(inout) :: x,y,ux,uy

      do j = 1, size(x,2)
        do i = 1, size(x,1)
          x(i,j) = examples_calculation(x(i,j))
          y(i,j) = examples_calculation(y(i,j))
          ux(i,j) = examples_calculation(ux(i,j))
          uy(i,j) = examples_calculation(uy(i,j))
        end do
      end do
    end subroutine

    subroutine test_calculations_agents_matrix(agents_matrix)
      implicit none
      type(Node), dimension(:,:), intent(inout) :: agents_matrix

      integer :: i,j

      do j=1, size(agents_matrix,2):
        do i = 1, size(agents_matrix,1):

          agents_matrix(i,j)%x = example_calculation(agents_matrix(i,j)%x)
          agents_matrix(i,j)%y = example_calculation(agents_matrix(i,j)%y)
          agents_matrix(i,j)%ux = example_calculation(agents_matrix(i,j)%ux)
          agents_matrix(i,j)%uy = example_calculation(agents_matrix(i,j)%uy)
          
        end do
      end do
      

    end subroutine test_calculations_agents_matrix

    function example_calculation(x) result(fx)
      implicit none
      real(8), intent(inout) :: x

      real(8) :: fx

      fx = x * x + 4

    end function example_calculation

end module mod_test_runtime
