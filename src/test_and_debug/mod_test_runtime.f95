module mod_test_runtime

  use mod_agent_class

  implicit none 

     ! Define the derived type 'container'
  type :: container
    real(8) :: pos_x = 0.0_8
    real(8) :: pos_y = 0.0_8
    real(8) :: ux = 0.0_8
    real(8) :: uy = 0.0_8
  end type container

  type :: container_node
    type(container), pointer :: data => null()
    type(container_node), pointer :: next => null()
  end type container_node



  contains

    subroutine initialize_container_list(head,container_matrix,n,m)
      implicit none
      type(container_node), pointer :: head
      type(container), dimension(:,:), intent(in) :: container_matrix
      integer, intent(in) :: n, m

      integer :: i, j
      type(container_node), pointer :: current, new_node

      head => null()
      current => null()

      do j = 1, m
        do i = 1, n
          allocate(new_node)
          allocate(new_node%data)
          call random_number(new_node%data%pos_x)
          call random_number(new_node%data%pos_y)
          call random_number(new_node%data%ux)
          call random_number(new_node%data%uy)

          if (.not. associated(head)) then
            head => new_node
            current => head
          else
            current%next => new_node
            current => new_node
          end if
        end do
      end do

    end subroutine initialize_container_list

    subroutine count_list_members(head)
      implicit none
      type(container_node), pointer :: head
      type(container_node), pointer :: current
      integer :: count

      count = 0
      current => head

      do while (associated(current))
        count = count + 1
        current => current%next
      end do

      print *, "  Total members in container list: ", count

    end subroutine count_list_members

    subroutine deallocate_container_list(head)
      implicit none
      type(container_node), pointer :: head
      type(container_node), pointer :: current, temp

      current => head

      do while (associated(current))
        temp => current%next
        if (associated(current%data)) then
          deallocate(current%data)
        end if
        deallocate(current)
        current => temp
      end do

      head => null()

    end subroutine deallocate_container_list

    ! Initialises a real(8) matrix with a starting value
    subroutine initialise_matrix(matrix, n, m)
      implicit none
      integer, intent(in) :: n, m
      real(8), dimension(n, m), intent(out) :: matrix
      
      integer :: i, j
      do j = 1, m
        do i = 1, n
          call random_number(matrix(i, j)) ! Assign a random value [0, 1)
        end do
      end do
    end subroutine initialise_matrix

    
    ! Initialises a matrix of type 'container' with starting values
    subroutine initialise_ag_matrix(container_matrix, n, m)
      implicit none
      integer, intent(in) :: n, m
      type(container), dimension(n, m), intent(out) :: container_matrix
      
      integer :: i, j
      do j = 1, m
        do i = 1, n
          call random_number(container_matrix(i, j)%pos_x)
          call random_number(container_matrix(i, j)%pos_y)
          call random_number(container_matrix(i, j)%ux)
          call random_number(container_matrix(i, j)%uy)
        end do
      end do
    end subroutine initialise_ag_matrix

    subroutine test_calculations_matrix_a(matrix)
      implicit none
      real(8), dimension(:,:), intent(inout) :: matrix

      integer :: i,j

      do j = 1, size(matrix,2)
        do i = 1, size(matrix,1)
          matrix(i,j) = example_calculation(matrix(i,j))
        end do
      end do

    end subroutine

    subroutine test_calculations_matrix_b(x,y,ux,uy)
      implicit none
      real(8), dimension(:,:), intent(inout) :: x,y,ux,uy

      integer :: i,j

      do j = 1, size(x,2)
        do i = 1, size(x,1)
          x(i,j) = example_calculation(x(i,j))
          y(i,j) = example_calculation(y(i,j))
          ux(i,j) = example_calculation(ux(i,j))
          uy(i,j) = example_calculation(uy(i,j))
        end do
      end do
    end subroutine

    subroutine test_calculations_container_matrix(container_matrix)
      implicit none
      type(container), dimension(:,:), intent(inout) :: container_matrix

      integer :: i,j

      do j=1, size(container_matrix,2)
        do i = 1, size(container_matrix,1)

          container_matrix(i,j)%pos_x = example_calculation(container_matrix(i,j)%pos_x)
          container_matrix(i,j)%pos_y = example_calculation(container_matrix(i,j)%pos_y)
          container_matrix(i,j)%ux = example_calculation(container_matrix(i,j)%ux)
          container_matrix(i,j)%uy = example_calculation(container_matrix(i,j)%uy)
          
        end do
      end do
      

    end subroutine test_calculations_container_matrix

    subroutine test_calculations_container_matrix_pointer(container_matrix)
      implicit none
      type(container), dimension(:,:), target, intent(inout) :: container_matrix

      type(container), pointer :: container_ptr
      integer :: i,j



      do j=1, size(container_matrix,2)
        do i = 1, size(container_matrix,1)

          container_ptr => container_matrix(i,j)

          call example_calculation_ptr(container_ptr)
          
        end do
      end do
      

    end subroutine test_calculations_container_matrix_pointer

    subroutine test_calculations_container_list(head)
      implicit none
      type(container_node), pointer :: head
      type(container_node), pointer :: current

      current => head

      do while (associated(current))
        call example_calculation_ptr(current%data)
        current => current%next
      end do

    end subroutine test_calculations_container_list

    function example_calculation(x) result(fx)
      implicit none
      real(8), intent(inout) :: x

      real(8) :: fx

      fx = x * x + 4

    end function example_calculation

    subroutine example_calculation_ptr(container_ptr) 
      implicit none
      type(container), pointer, intent(inout) :: container_ptr

      container_ptr%pos_x = example_calculation(container_ptr%pos_x)
      container_ptr%pos_y = example_calculation(container_ptr%pos_y)
      container_ptr%ux = example_calculation(container_ptr%ux)
      container_ptr%uy = example_calculation(container_ptr%uy)

    end subroutine example_calculation_ptr


end module mod_test_runtime
