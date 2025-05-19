! 
! do reflection
!
              if ( (x(i,jp) < lon_min_out) ) then 
                x(i,jp)  = 2.*lon_min_out - x(i,jp)
                ux(i,jp) = 2.*(x(i,jp) - x0(i,jp) )/dt - ux0(i,jp)
              elseif ( (x(i,jp) > lon_max_out) ) then
                x(i,jp) = 2.*lon_max_out - x(i,jp)
                ux(i,jp) = 2.*(x(i,jp) - x0(i,jp) )/dt - ux0(i,jp)
              endif 

              if ( (y(i,jp) < lat_min_out) ) then
                y(i,jp) = 2.*lat_min_out - y(i,jp)
                uy(i,jp) = 2.*(y(i,jp) - y0(i,jp) )/dt - uy0(i,jp)
              elseif ( (y(i,jp) > lat_max_out) ) then
                y(i,jp) = 2.*lat_max_out - y(i,jp)
                uy(i,jp) = 2.*(y(i,jp) - y0(i,jp) )/dt - uy0(i,jp)
              endif
