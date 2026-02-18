module mod_export_hep

    use mod_agent_world

    use iso_fortran_env, only: wp => real64


contains

    subroutine write_hep_to_csv(filename,jp, world)
        implicit none
        character(len=*), intent(in) :: filename
        integer, intent(in) :: jp
        class(world_container), intent(in) :: world
        integer :: unit_id
        integer :: i, j, nrows, ncols



        nrows = size(world%grid%hep_av(:,:,jp), 1)
        ncols = size(world%grid%hep_av(:,:,jp), 2)

        ! Open the file (overwrite if exists)
        open(newunit=unit_id, file=filename, status='replace', action='write')

        ! Loop over rows
        do i = 1, nrows
            do j = 1, ncols
                if (j < ncols) then
                    write(unit_id, '(F12.6, ",")', advance="no") world%grid%hep_av(i,j,jp)
                else
                    write(unit_id, '(F12.6)') world%grid%hep_av(i,j,jp)
                end if
            end do
        end do

        close(unit_id)
    end subroutine write_hep_to_csv

subroutine write_hep_binary_with_dims(filename_data, filename_dims, world)
    implicit none
    character(len=*), intent(in) :: filename_data, filename_dims
    class(world_container), intent(in) :: world

    integer :: n, m, num_pop, t_hep
    integer :: unit_data, unit_dims

    ! Get dimensions
    n       = size(world%grid%hep, 1)
    m       = size(world%grid%hep, 2)
    num_pop = size(world%grid%hep, 3)
    t_hep   = size(world%grid%hep, 4)

    ! Write dimensions to text file
    open(newunit=unit_dims, file=filename_dims, status='replace', action='write')
    write(unit_dims, *) n, m, num_pop, t_hep, &
                        world%grid%lon_hep(1), world%grid%lon_hep(2), &
                        world%grid%lat_hep(1), world%grid%lat_hep(2)
    close(unit_dims)

    ! Write binary data
    open(newunit=unit_data, file=filename_data, status='replace', access='stream', form='unformatted')
    write(unit_data) world%grid%hep
    close(unit_data)
end subroutine write_hep_binary_with_dims

end module mod_export_hep