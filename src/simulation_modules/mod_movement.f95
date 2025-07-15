module mod_movement

    use mod_agent_class
    use mod_setup_hep
    use mod_agent_matrix_merge

    ! integer :: t_hep = 1
    ! t_hep = int( t/delta_t_hep ) + 1

    
       !
    ! Uses Vars from mod_setup_hep: ??? 

        ! integer, dimension(npops) :: out_count_priv, drown_count_priv 

    real(8), dimension(npops)  :: cb2, cb3
    !cb2(jp) = dt/tau(jp)
    !cb3(jp) = sqrt( sigma_u(jp)**2/tau(jp) )          ! sqrt( sigma_u**2/tau )
contains 


! TODO: implement a movement routine that updates the positions of the agents that works like the old movement routine
!       but doesnt use the matrix stuff. DN 08.07.2025




                logical function in_research_area(pos_x,pos_y)
                    implicit none
                    real, intent(in) :: pos_x, pos_y

                    in_research_area = .true.

                    if ((pos_x<lon_min_out) .OR. (pos_x>lon_max_out)) then 
                       in_research_area = .false.
                    endif

                    if ((pos_y<lat_min_out) .OR. (pos_y>lat_max_out)) then
                        in_research_area = .false.
                    endif


                end function in_research_area

                logical function agent_above_water(gx, gy, jp,t_hep)
                    implicit none 
                    integer, intent(in) :: jp, gx, gy, t_hep
                    agent_above_water = .false.
                    if(hep(gx, gy, jp, t_hep) <= 0. )    then
                        agent_above_water = .true.
                    endif
                end function agent_above_water

                subroutine calculate_gradient(gx,gy,jp,grad_x,grad_y) ! In this function there is some kind of coordinate transformation I think we should isolate
                                                    ! that into a seperate function so that we can use it in other places as well 
                    integer, intent(in) :: gx,gy
                    real, intent(inout) :: grad_x, grad_y

                    real(8), dimension(0:8) :: heploc
                    real(8) :: heploc_max
                    integer, dimension(0:8) :: gxx, gyy
                    integer :: iloc, il

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
                        grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y(i,jp))*deg_km)
                    elseif ( iloc == 4 .or. iloc == 8 ) then 
                        grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x(i,jp))*cos(y(i,jp)*deg_rad) * deg_km)
                        grad_y = 0.d0
                    else 
                        grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - x(i,jp))*cos(y(i,jp)*deg_rad) * deg_km)
                        grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - y(i,jp))*deg_km)
                    endif


                end subroutine calculate_gradient

                subroutine movement_at_boundary(x0, y0, ux0, uy0, x, y, ux, uy)
                    real, intent(in) :: x0,y0,ux0,uy0
                    real, intent(inout) :: x,y,ux,uy
                    
                    ! 
                    ! do reflection
                    !
                    if ( (x < lon_min_out) ) then 
                        x  = 2.*lon_min_out - x
                        ux = 2.*(x - x0 )/dt - ux0
                    elseif ( (x > lon_max_out) ) then
                        x = 2.*lon_max_out - x
                        ux = 2.*(x - x0)/dt - ux0
                    endif 

                    if ( (y < lat_min_out) ) then
                        y = 2.*lat_min_out - y
                        uy = 2.*(y - y0 )/dt - uy0
                    elseif ( (y > lat_max_out) ) then
                        y = 2.*lat_max_out - y
                        uy = 2.*(y - y0 )/dt - uy0
                    endif


                end subroutine movement_at_boundary


    subroutine agent_move(i,jp,t_hep)
        integer :: i, jp, t_hep
        
        type(Node), pointer :: current_agent 
        real :: new_x, new_y
        real :: new_ux, new_uy

        real :: old_x, old_y
        real :: old_ux, old_uy

        integer :: grid_x, grid_y 
        real :: grad_x, grad_y

        if (.not. allocated(population_agents_matrix)) then
            print *, "agent_move: population_agents_matrix not associated"
            return
        end if

        if (.not. associated(population_agents_matrix(i,jp)%node)) then
            print *, "agent_move: agent not associated", i, jp
            return
        end if

        current_agent => population_agents_matrix(i,jp)%node
        old_x = current_agent%pos_x
        old_y = current_agent%pos_y 

        
        if (current_agent%is_dead) then
            return
        endif

        ! Check if a human left the research area, then counted as out
        if (.false. .eqv. in_research_area(old_x, old_y)) then
            !print *, "agent_move: Agent left research area at position", old_x, old_y, "in population", jp
            call agent_die_from_matrix_calc(i,jp)
            out_count_priv(jp) = out_count_priv(jp) + 1
        

            return 
        endif


        ! I think this basically computes the grid position of the human in the HEP grid
        grid_x = floor( ( old_x - lon_0 ) / delta_lon ) + 1 
        grid_y = floor( ( old_y - lat_0 ) / delta_lat ) + 1

        ! Check if human above water, then counted as drowned            ! ys, do not like this, redo
        if (agent_above_water(grid_x,grid_y,jp,t_hep)) then
            !print *, hep(grid_x, grid_y, jp, t_hep), " <= 0., agent drowned"
            !print *, "agent_move: Agent drowned at position", old_x, old_y, "in population", jp
            call agent_die_from_matrix_calc(i,jp)
            drown_count_priv(jp) = drown_count_priv(jp) + 1


            return 
        endif


        if ( grid_x == 1 .or. grid_x == dlon_hep .or. grid_y == 1 .or. grid_y == dlat_hep) then
            ! DN : I dont exactly understand why we remove a agent if this is the case
            call agent_die_from_matrix_calc(i,jp)
            print *, "agent_move: Agent at boundary, removed from simulation", i, jp
            out_count_priv(jp) = out_count_priv(jp) + 1

            return 

        end if

        call calculate_gradient(grid_x,grid_y,jp,grad_x,grad_y)


        new_ux = old_ux + cb1(jp)*grad_x - old_ux*cb2(jp) + cb3(jp)*Ax(i)
        new_uy = old_uy + cb1(jp)*grad_y - old_uy*cb2(jp) + cb3(jp)*Ay(i)

        new_x = old_x + new_ux / (deg_km * cos(old_y * deg_rad)) * dt
        new_y = old_y + new_uy / deg_km * dt

        
        call movement_at_boundary(old_x,old_y,old_ux,old_uy,new_x,new_y,new_ux,new_uy)

        grid_x = floor( ( new_x - lon_0 ) / delta_lon ) + 1
        grid_y = floor( ( new_y - lat_0 ) / delta_lat ) + 1
                      
                  

        !if ((grid_x < 1) .or. (grid_x > dlon_hep) .or. (grid_y < 1) .or. (grid_y > dlat_hep)) then
        !    call agent_die_from_matrix_calc(i,jp)
        !    print *, "count out three"
        !    out_count_priv(jp) = out_count_priv(jp) + 1
    
        !    return
        !endif

        if ( hep(grid_x, grid_y, jp, t_hep) <= 0. ) then           ! need better reflection scheme later
            new_x = old_x
            new_y = old_y
            new_ux = cb3(jp)*Ax(i)
            new_uy = cb3(jp)*Ay(i)
        endif
                
              

        !if (mod(current_agent%id,123) == 0) then
            !print *, "agent_move: Agent ID:", current_agent%id, "Old Position:", old_x, old_y, "New Position:", new_x, new_y
            !print *, "Agent Velocity:", old_ux, old_uy, "New Velocity:", new_ux, new_uy
        !endif

        current_agent%pos_x = new_x
        current_agent%pos_y = new_y

        current_agent%ux = new_ux
        current_agent%uy = new_uy
        x(i,jp) = new_x
        y(i,jp) = new_y
        ux(i,jp) = new_ux
        uy(i,jp) = new_uy



    end subroutine agent_move
end module mod_movement