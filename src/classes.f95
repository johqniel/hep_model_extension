module classes
    use stdlib_kinds, only: int32, real32
    use stdlib_vector_type, only: vector_type
    implicit none

type :: human
    integer(int32) :: id
    integer(int32) :: age
    character(len=1) :: gender
    integer(int32) :: xpos
    integer(int32) :: ypos
end type human

