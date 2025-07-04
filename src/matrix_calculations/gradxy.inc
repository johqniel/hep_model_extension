                heploc(0) = hep_av(gx,   gy,   jp)   ! hepC
                gxx   (0) = gx
                gyy   (0) = gy
                heploc(1) = hep_av(gx-1, gy-1, jp)   ! hepSW
                gxx   (1) = gx-1
                gyy   (1) = gy-1
                heploc(2) = hep_av(gx,   gy-1, jp)   ! hepS
                gxx   (2) = gx
                gyy   (2) = gy-1
                heploc(3) = hep_av(gx+1, gy-1, jp)   ! hepSE
                gxx   (3) = gx+1
                gyy   (3) = gy-1
                heploc(4) = hep_av(gx+1, gy,   jp)   ! hepE
                gxx   (4) = gx+1
                gyy   (4) = gy
                heploc(5) = hep_av(gx+1, gy+1, jp)   ! hepNE
                gxx   (5) = gx+1
                gyy   (5) = gy+1
                heploc(6) = hep_av(gx,   gy+1, jp)   ! hepN
                gxx   (6) = gx
                gyy   (6) = gy+1
                heploc(7) = hep_av(gx-1, gy+1, jp)   ! hepNW
                gxx   (7) = gx-1
                gyy   (7) = gy+1
                heploc(8) = hep_av(gx-1, gy,   jp)   ! hepW
                gxx   (8) = gx-1
                gyy   (8) = gy

                heploc_max = -9999.
                do il = 0, 8
                  if ( heploc(il) .gt. heploc_max ) then 
                    heploc_max = heploc(il)
                    iloc = il 
                  endif
                enddo

                if ( iloc == 0 ) then
                  grad_x = 0.d0
                  grad_y = 0.d0 
                elseif ( iloc == 2 .or. iloc == 6 ) then 
                  grad_x = 0.d0
                  grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y0(i,jp))*deg_km)
                elseif ( iloc == 4 .or. iloc == 8 ) then 
                  grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x0(i,jp))*cos(y0(i,jp)*deg_rad) * deg_km)
                  grad_y = 0.d0
                else 
                  grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x0(i,jp))*cos(y0(i,jp)*deg_rad) * deg_km)
                  grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y0(i,jp))*deg_km)
                endif

!                  grad_x = ( heploc_max - heploc(0) ) / ( ( lon_hep(gxx(iloc)) - lon_hep(gxx(0)) )*cos(y0(i,jp)*deg_rad) * deg_km)
!                  grad_y = ( heploc_max - heploc(0) ) / ( ( lat_hep(gyy(iloc)) - lat_hep(gyy(0)) )*deg_km)


