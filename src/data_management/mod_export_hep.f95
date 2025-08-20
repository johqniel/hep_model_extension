module mod_export_hep

    use mod_globals

    use iso_fortran_env, only: wp => real64


contains

    subroutine write_hep_to_csv(filename,jp)
        implicit none
        character(len=*), intent(in) :: filename
        integer, intent(in) :: jp
        integer :: unit_id
        integer :: i, j, nrows, ncols



        nrows = size(hep_av(:,:,jp), 1)
        ncols = size(hep_av(:,:,jp), 2)

        ! Open the file (overwrite if exists)
        open(newunit=unit_id, file=filename, status='replace', action='write')

        ! Loop over rows
        do i = 1, nrows
            do j = 1, ncols
                if (j < ncols) then
                    write(unit_id, '(F12.6, ",")', advance="no") hep_av(i,j,jp)
                else
                    write(unit_id, '(F12.6)') hep_av(i,j,jp)
                end if
            end do
        end do

        close(unit_id)
    end subroutine write_hep_to_csv


end module mod_export_hep