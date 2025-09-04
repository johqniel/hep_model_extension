module mod_movement

    use mod_globals

    use mod_globals
    ! Uses:     - cb2, cb3

    use mod_globals 
    ! Uses:     - t_hep
    !           - hep
    !           - delta_lon/lat, lon0/lat0
    !           - hep_av

    use mod_agent_class

    use mod_agent_tracking
    ! Uses:     - mark_agent_dead_remove_from_grid



    !use mod_matrix_calculations
    ! Uses the following functions/subroutines: 
    !
    !   - in_research_area
    !   - agent_above_water
    !   
    ! Uses the following variables: 
    !
    !   - hep
    !   - t_hep
    !
    ! Uses the following markers/counters: 
    !
    !   - out_count_priv_a
    !   - out_count_priv_b

    use mod_grid


    use mod_calculations
    ! Uses: 
    !
    !       subroutine calculate_grid_pos




contains


          subroutine agent_move_grid(i,jp,grid,Ax,Ay)
              integer, intent(in) :: i, jp
              type(spatial_grid), pointer :: grid
              real(8), allocatable, dimension(:), intent(in) :: Ax, Ay ! For randomness

              
              
              type(Node), pointer :: current_agent 
              real(8) :: new_x, new_y
              real(8) :: new_ux, new_uy

              real(8) :: old_x, old_y
              real(8) :: old_ux, old_uy

              integer :: grid_x, grid_y, grid_x_b, grid_y_b
              real :: gradient_x, gradient_y

              integer :: gx,gy,gx0,gy0 ! for grid movement. 





              ! probably we can clean this function like this: 
              ! gx,gy, and grid_x, and grid_y are the same but computet differently 
              ! eliminate one and uses function calculate_grid_pos 



              


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
              old_ux = current_agent%ux
              old_uy = current_agent%uy



              if (current_agent%is_dead) then
                    print*, "Trying to move a dead agent, skipping."
                  return
              endif

              ! Check if a human left the research area, then counted as out
              if (.false. .eqv. in_research_area(old_x, old_y)) then
                  call mark_agent_dead(i,jp)
                  out_count_priv(jp) = out_count_priv(jp) + 1
              

                  return 
              endif




              grid_x = floor( ( old_x - lon_0 ) / delta_lon ) + 1 
              grid_y = floor( ( old_y - lat_0 ) / delta_lat ) + 1

              ! Check if human above water, then counted as drowned            ! ys, do not like this, redo
              if (agent_above_water(grid_x,grid_y,jp,t_hep)) then
                  call mark_agent_dead(i,jp)
                  drown_count_priv(jp) = drown_count_priv(jp) + 1


                  return 
              endif


              if ( grid_x == 1 .or. grid_x == dlon_hep .or. grid_y == 1 .or. grid_y == dlat_hep) then
                  ! DN : I dont exactly understand why we remove a agent if this is the case
                  call mark_agent_dead(i,jp)
                  print *, "agent_move: Agent at boundary, removed from simulation", i, jp
                  out_count_priv_a(jp) = out_count_priv_a(jp) + 1

                  return 

              end if

              call calculate_gradient(grid_x,grid_y,old_x, old_y, i,jp,gradient_x,gradient_y)


              new_ux = old_ux + cb1(jp)*gradient_x - old_ux*cb2(jp) + cb3(jp)*Ax(i)
              new_uy = old_uy + cb1(jp)*gradient_y - old_uy*cb2(jp) + cb3(jp)*Ay(i)

              new_x = old_x + new_ux / (deg_km * cos(old_y * deg_rad)) * dt
              new_y = old_y + new_uy / deg_km * dt

              
              call movement_at_boundary(old_x,old_y,old_ux,old_uy,new_x,new_y,new_ux,new_uy)

              grid_x_b = floor( ( new_x - lon_0 ) / delta_lon ) + 1
              grid_y_b = floor( ( new_y - lat_0 ) / delta_lat ) + 1
                            
                        

              if ((grid_x_b < 1) .or. (grid_x_b > dlon_hep) .or. (grid_y_b < 1) .or. (grid_y_b > dlat_hep)) then
                  call mark_agent_dead(i,jp)
                  !print *, "count out three"
                  if (current_agent%age > 0) then
                    print*, "Age of out count priv b: ", current_agent%age
                endif
                  out_count_priv_b(jp) = out_count_priv_b(jp) + 1
          
                  return
              endif

              if ( hep(grid_x, grid_y, jp, t_hep) <= 0. ) then           ! need better reflection scheme later
                  new_x = old_x
                  new_y = old_y
                  new_ux = cb3(jp)*Ax(i)
                 new_uy = cb3(jp)*Ay(i)
              endif
                      

              current_agent%pos_x = new_x
              current_agent%pos_y = new_y

              current_agent%ux = new_ux
              current_agent%uy = new_uy


              call calculate_grid_pos(new_x, new_y, gx, gy)
              call calculate_grid_pos(old_x, old_y, gx0, gy0)

              if ((gx /= gx0) .or. (gy /= gy0)) then
                call grid%move_agent_to_cell(current_agent,gx0,gy0,gx,gy)
              endif 



          end subroutine agent_move_grid

          ! Uses the following functions: 

                !=======================================================================
                ! LOGICAL FUNCTION: in_research_area
                ! Returns .true. if the location (pos_x,pos_y) is in th research area.
                !
                !=======================================================================
                logical function in_research_area(pos_x,pos_y)
                    implicit none
                    real(8), intent(in) :: pos_x, pos_y

                    in_research_area = .true.

                    if ((pos_x<lon_min_out) .OR. (pos_x>lon_max_out)) then 
                       in_research_area = .false.
                    endif

                    if ((pos_y<lat_min_out) .OR. (pos_y>lat_max_out)) then
                        in_research_area = .false.
                    endif


                end function in_research_area


                !=======================================================================
                ! LOGICAL FUNCTION: agent_above_water
                ! Returns .true. if an agent from population jp that is in the grid in
                ! gx,gy is above water. 
                !
                !=======================================================================
                logical function agent_above_water(gx, gy, jp,t_hep)
                    implicit none 
                    integer, intent(in) :: jp, gx, gy, t_hep

                    agent_above_water = .false.

                    if(.not. allocated(hep)) then
                        agent_above_water = .false.
                        print*, "t_hep not associated."
                        return
                    endif

                    if ( gx < lbound(hep,1) .or. gx > ubound(hep,1) .or. &
                        gy < lbound(hep,2) .or. gy > ubound(hep,2) .or. &
                        jp < lbound(hep,3) .or. jp > ubound(hep,3) .or. &
                        t_hep < lbound(hep,4) .or. t_hep > ubound(hep,4) ) then

                        print *, "Index out of bounds:", gx, gy, jp, t_hep
                        return
                    endif

                    if(hep(gx, gy, jp, t_hep) <= 0. )    then
                        agent_above_water = .true.
                    endif
                end function agent_above_water


                subroutine calculate_gradient(gx,gy,pos_x,pos_y, i, jp,grad_x,grad_y) ! In this function there is some kind of coordinate transformation I think we should isolate
                                                    ! that into a seperate function so that we can use it in other places as well 
                    integer, intent(in) :: gx,gy,i,jp
                    real(8), intent(in) :: pos_x,pos_y
                    real, intent(inout) :: grad_x, grad_y

                    !real(8), dimension(0:8) :: heploc
                    !real(8) :: heploc_max
                    integer, dimension(0:8) :: gxx, gyy
                    integer :: iloc, il
                    real(8), dimension(0:8) :: heploc
                    real(8) :: heploc_max

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
                        grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - pos_y)*deg_km)
                    elseif ( iloc == 4 .or. iloc == 8 ) then 
                        grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - pos_x)*cos(pos_y*deg_rad) * deg_km)
                        grad_y = 0.d0
                    else 
                        grad_x = ( heploc_max - heploc(0) ) / ((lon_hep( gxx(iloc) ) - pos_x)*cos(pos_y*deg_rad) * deg_km)
                        grad_y = ( heploc_max - heploc(0) ) / ((lat_hep( gyy(iloc) ) - pos_y)*deg_km)
                    endif


                end subroutine calculate_gradient

                subroutine movement_at_boundary(x0, y0, ux0, uy0, x, y, ux, uy)
                    real(8), intent(in) :: x0,y0,ux0,uy0
                    real(8), intent(inout) :: x,y,ux,uy
                    
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

end module mod_movement

