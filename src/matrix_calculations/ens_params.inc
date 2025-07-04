    do i = 1, 3
      rgauss = rnorm_vec(100, rho_max(i), 0.1*rho_max(i))
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then 
          rgauss(j) = rho_max(i) 
        endif
      enddo
      rho_max(i) = rgauss(50)

      rgauss = rnorm_vec(100, r_B(i),     0.1*r_B(i)    )
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = r_B(i)
        endif
      enddo
      r_B(i) = rgauss(50)

      rgauss = rnorm_vec(100, d_B(i),     0.1*d_B(i)    )
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = d_B(i)
        endif
      enddo
      d_B(i) = rgauss(50)

      rgauss = rnorm_vec(100, sigma_u(i), 0.1*sigma_u(i))
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = sigma_u(i)
        endif
      enddo
      sigma_u(i) = rgauss(50)

      rgauss = rnorm_vec(100, cb1(i),     0.1*cb1(i)    )
      do j = 1, 100
        if ( rgauss(j) .le. 0. ) then
          rgauss(j) = cb1(i)
        endif
      enddo
      cb1(i) = rgauss(50)
    enddo

    inquire(file="log.txt", exist=exists)
    if (exists) then
      open(newunit=io, file="log.txt", position="append", status="old", action="write")
    elseif(exists .eqv. .false.) then 
      open(file="log.txt", newunit=io, status="new")
    end if

    write(io, *) "Run 3220020001avD15cb1250_"//random_seed
    write(io, *) "Actual parameters used"
    write(io, *) "rho_max(1), rho_max(2), rho_max(3)"
    write(io, *)  rho_max(1), rho_max(2), rho_max(3)
    write(io, *) "r_B(1),     r_B(2),     r_B(3)"
    write(io, *) r_B(1),     r_B(2),     r_B(3)
    write(io, *) "d_B(1),     d_B(2),     d_B(3)"
    write(io, *) d_B(1),     d_B(2),     d_B(3)
    write(io, *) "tau(1),     tau(2),     tau(3)"
    write(io, *) tau(1),     tau(2),     tau(3)
    write(io, *) "sigma_u(1), sigma_u(2), sigma_u(3)"
    write(io, *) sigma_u(1), sigma_u(2), sigma_u(3)
    write(io, *) "cb1(1),     cb1(2),     cb1(3)"
    write(io, *) cb1(1),     cb1(2),     cb1(3)
    close(io)


