contains 

logical function reals_equal(x,y,tol)
    ! Compare two reals for approximate equality
    real(8), intent(in) :: x, y
    real(8), intent(in), optional :: tol
    real(8) :: eps

    if (present(tol)) then
       eps = tol
    else
       eps = real_comparison_eps   ! default tolerance
    end if

    reals_equal = abs(x - y) < eps * max(1.0, abs(x), abs(y))

end function reals_equal