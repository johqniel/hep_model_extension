module mod_export_agents

use iso_fortran_env, only: wp => real64

use mod_agent_class

contains 

subroutine write_agents_to_csv(filename,t)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(in) :: t
    
    integer :: population
    type(Node), pointer :: current
    type(Node), pointer :: head   ! pointer to the head of the list
    integer :: unit_id
    logical :: exists

    head => head_agents

    inquire(file=filename, exist=exists)
    if (exists) then
        open(newunit=unit_id, file=filename, status='replace', action='write')
    else
        open(newunit=unit_id, file=filename, status='new', action='write')
    end if


    ! In your write subroutine





    ! Write header
    write(unit_id, '(A)') 'id,pos_x,pos_y,gender,age,population,is_pregnant'

    ! Loop through the list
    current => head
    do while (associated(current))
        population = current%position_population

        if (t < tstep_start(population)) then
            current => current%next
            cycle
        endif

        !x_position = population_agents_matrix(current%position_population, current%position_human)%node%pos_x
        write(unit_id, '(I0,1x,F6.2,1x,F6.2,1x,A1,1x,I0,1x,I0,1x,I0)') &
            current%id, current%pos_x, current%pos_y, current%gender, &
            current%age, current%position_population, current%is_pregnant 
        current => current%next
    end do

    ! Add this line to make sure data is written immediately
    FLUSH(unit_id)


    close(unit_id)
end subroutine write_agents_to_csv


subroutine write_agents_to_csv_new(filename,t,agents_matrix,num_humans_in_pop,n_populations)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(in) :: t
    type(Node), allocatable, target, intent(in) :: agents_matrix(:,:)
    integer, dimension(:), intent(in) :: num_humans_in_pop
    integer, intent(in) :: n_populations
    
    integer :: population
    type(Node), pointer :: current
    integer :: unit_id
    logical :: exists
    integer :: i,j


    inquire(file=filename, exist=exists)
    if (exists) then
        open(newunit=unit_id, file=filename, status='replace', action='write')
    else
        open(newunit=unit_id, file=filename, status='new', action='write')
    end if


    ! In your write subroutine





    ! Write header
    write(unit_id, '(A)') 'id,pos_x,pos_y,gender,age,population,is_pregnant'

    ! Loop through the matrix

    do j = 1, n_populations
        do i = 1, num_humans_in_pop(j)

            if (t < tstep_start(j)) cycle

            current => agents_matrix(i,j)

            write(unit_id, '(I0,1x,F6.2,1x,F6.2,1x,A1,1x,I0,1x,I0,1x,I0)') &
                current%id, current%pos_x, current%pos_y, &
                current%gender, current%age, j, current%is_pregnant 

        end do
    end do


    ! Add this line to make sure data is written immediately
    FLUSH(unit_id)


    close(unit_id)
end subroutine write_agents_to_csv_new


end module mod_export_agents