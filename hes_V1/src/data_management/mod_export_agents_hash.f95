module mod_export_agents_hash

use iso_fortran_env, only: wp => real64


use mod_agent_world


contains 


subroutine write_agents_to_csv_hash(filename,t,world)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(in) :: t
    class(world_container), intent(in) :: world
    
    integer :: population
    type(Agent), pointer :: current
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

    do j = 1, world%config%npops
        do i = 1, world%num_humans(j)

            if (t < world%config%tstep_start(j)) cycle

            ! current => world%agents(i,j)

            write(unit_id, '(I0,1x,F6.2,1x,F6.2,1x,A1,1x,I0,1x,I0,1x,I0)') &
                world%agents(i,j)%id, world%agents(i,j)%pos_x, world%agents(i,j)%pos_y, &
                world%agents(i,j)%gender, world%agents(i,j)%age, j, world%agents(i,j)%is_pregnant 
        end do
    end do


    ! Add this line to make sure data is written immediately
    FLUSH(unit_id)


    close(unit_id)
end subroutine write_agents_to_csv_hash



end module mod_export_agents_hash