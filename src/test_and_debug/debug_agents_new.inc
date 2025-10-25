subroutine count_agents_new(agents_matrix, num_humans_in_pop)
    implicit none
    type(Node), intent(in) :: agents_matrix(:,:)
    integer, intent(in) :: num_humans_in_pop(:)

    integer :: i, population
    integer :: counter(size(num_humans_in_pop))

    counter = 0

    do population = 1, size(num_humans_in_pop)
        do i = 1, num_humans_in_pop(population)
            if (.not. agents_matrix(i,population)%is_dead) then
                counter(population) = counter(population) + 1
            end if
        end do
    end do

    print*, "Agent counts by population:", counter

end subroutine count_agents_new

subroutine count_dead_agents_new(agents_matrix, num_humans_in_pop)
    implicit none
    type(Node), intent(in) :: agents_matrix(:,:)
    integer, intent(in) :: num_humans_in_pop(:)

    integer :: i, population
    integer :: counter(size(num_humans_in_pop))

    counter = 0

    do population = 1, size(num_humans_in_pop)
        do i = 1, num_humans_in_pop(population)
            if (agents_matrix(i,population)%is_dead) then
                counter(population) = counter(population) + 1
                counter(population) = counter(population) + 1
            end if
        end do
    end do

    print*, "Dead Agent counts by population:", counter

end subroutine count_dead_agents_new