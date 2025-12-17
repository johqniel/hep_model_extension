program test_netcdf_minimal
    use netcdf
    implicit none
    integer :: ncid, status

    print *, "Testing NetCDF open..."
    status = nf90_open("input/hep/europe/AUR.nc", nf90_nowrite, ncid)
    if (status /= nf90_noerr) then
        print *, "Error: ", trim(nf90_strerror(status))
    else
        print *, "Success! ncid = ", ncid
        status = nf90_close(ncid)
    end if
end program test_netcdf_minimal
