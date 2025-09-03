module two_classes_module
    implicit none

    type :: ClassA
        type(ClassB), pointer :: b => null()
    contains
        procedure :: say_hello => A_say_hello
    end type ClassA

    type :: ClassB
        type(ClassA), pointer :: a => null()
    contains
        procedure :: say_hello => B_say_hello
    end type ClassB

contains

    subroutine A_say_hello(this)
        class(ClassA), intent(in) :: this
        print *, "Hello from ClassA"
        if (associated(this%b)) then
            print *, "ClassA sees its B and calls B's method:"
            call this%b%say_hello()
        end if
    end subroutine A_say_hello

    subroutine B_say_hello(this)
        class(ClassB), intent(in) :: this
        print *, "Hello from ClassB"
        if (associated(this%a)) then
            print *, "ClassB sees its A and calls A's method (no recursion here):"
            print *, "ClassA is linked!"
        end if
    end subroutine B_say_hello

end module two_classes_module


program main_test
    use two_classes_module
    implicit none

    type(ClassA), target :: objA
    type(ClassB), target :: objB

    ! connect the objects
    objA%b => objB
    objB%a => objA

    ! use them
    call objA%say_hello()
    call objB%say_hello()
end program main_test
