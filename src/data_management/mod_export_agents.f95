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

    ! Write header
    write(unit_id, '(A)') 'id,pos_x,pos_y,gender,age,population'

    ! Loop through the list
    current => head
    do while (associated(current))
        population = current%position_population

        if (t < tstep_start(population)) then
            current => current%next
            cycle
        endif

        !x_position = population_agents_matrix(current%position_population, current%position_human)%node%pos_x
        write(unit_id, '(I0,1x,F6.2,1x,F6.2,1x,A1,1x,I0,1x,I0)') &
            current%id, current%pos_x, current%pos_y, current%gender, current%age, current%position_population
        current => current%next
    end do

    close(unit_id)
end subroutine write_agents_to_csv


end module mod_export_agents