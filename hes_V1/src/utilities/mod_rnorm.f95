module mod_rnorm
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  contains


real function rnorm_single(mu,sigma) result(x)
  real(8), intent(in), optional :: mu, sigma


  x = rnorm()

  if (present(sigma)) x = x * sigma
  if (present(mu)) x = x + mu
end function rnorm_single

  function rnorm_vec(n,mu,sigma) result(variates)
    integer      , intent(in)           :: n
    real(kind=dp), intent(in), optional :: mu, sigma
    real(kind=dp)                       :: variates(n)
    integer                             :: i
    do i=1,n
      variates(i) = rnorm()
    end do
    if (present(sigma)) variates = sigma*variates
    if (present(mu)) variates = variates + mu
  end function rnorm_vec


  FUNCTION rnorm() RESULT(fn_val)

    !   Generate a random normal deviate using the polar method.
    !   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
    !              normal variables', Siam Rev., vol.6, 260-264, 1964.

    IMPLICIT NONE
    REAL(kind=dp)  :: fn_val

    ! Local variables

    REAL(kind=dp)   :: u, sum
    REAL(kind=dp), SAVE      :: v, sln
    LOGICAL, SAVE   :: second = .FALSE.
    REAL(kind=dp), PARAMETER :: one = 1.0_dp, vsmall = TINY( one )

    IF (second) THEN
    ! If second, use the second random number generated on last call

      second = .false.
      fn_val = v*sln

    ELSE
    ! First call; generate a pair of random normals

      second = .true.
      DO
        CALL RANDOM_NUMBER( u )
        CALL RANDOM_NUMBER( v )
        u = SCALE( u, 1 ) - one
        v = SCALE( v, 1 ) - one
        sum = u*u + v*v + vsmall         ! vsmall added to prevent LOG(zero) / zero
        IF(sum < one) EXIT
      END DO
      sln = SQRT(-SCALE(LOG(sum),1)/sum)
      fn_val = u*sln
    END IF
  END FUNCTION rnorm

  
end module mod_rnorm
