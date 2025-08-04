module mod_movement

    use mod_agent_class

    use mod_matrix_calculations

    use mod_grid

    use mod_calculations
    ! Uses: 
    !
    !       subroutine calculate_grid_pos

contains


          subroutine agent_move_grid(i,jp,grid)
              integer :: i, jp
              type(spatial_grid), pointer :: grid
              
              
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

              ! Eventually we can probably remove the x and x0 matrixes. But before that we have to make the 
              ! Simulation independent from the old matrix structures. 

              if (.not. allocated(population_agents_matrix)) then
                  print *, "agent_move: population_agents_matrix not associated"
                  return
              end if

              if (.not. associated(population_agents_matrix(i,jp)%node)) then
                  
                  print *, "agent_move: agent not associated", i, jp
                  return
              end if

              !print *, "we get here."
              current_agent => population_agents_matrix(i,jp)%node
              old_x = current_agent%pos_x
              old_y = current_agent%pos_y 
              old_ux = current_agent%ux
              old_uy = current_agent%uy

              !for debugging DN 16.07.
              if (.not. old_x == x0(i,jp) .or. .not. old_y == y0(i,jp)) then
                  !print *, " old_x,y not equal to x0,yo, in agent move grid."
              endif

              !x0(i,jp) = old_x
              !y0(i,jp) = old_y

              !print *, " we get here 2."
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

              !print *, "We get here 2.5"

              if ( grid_x == 1 .or. grid_x == dlon_hep .or. grid_y == 1 .or. grid_y == dlat_hep) then
                  ! DN : I dont exactly understand why we remove a agent if this is the case
                  call agent_die_from_matrix_calc(i,jp)
                  print *, "agent_move: Agent at boundary, removed from simulation", i, jp
                  out_count_priv_a(jp) = out_count_priv_a(jp) + 1

                  return 

              end if

              call calculate_gradient(grid_x,grid_y,jp,gradient_x,gradient_y)


              new_ux = old_ux + cb1(jp)*gradient_x - old_ux*cb2(jp) + cb3(jp)*Ax(i)
              new_uy = old_uy + cb1(jp)*gradient_y - old_uy*cb2(jp) + cb3(jp)*Ay(i)

              new_x = old_x + new_ux / (deg_km * cos(old_y * deg_rad)) * dt
              new_y = old_y + new_uy / deg_km * dt

              
              call movement_at_boundary(old_x,old_y,old_ux,old_uy,new_x,new_y,new_ux,new_uy)

              grid_x_b = floor( ( new_x - lon_0 ) / delta_lon ) + 1
              grid_y_b = floor( ( new_y - lat_0 ) / delta_lat ) + 1
                            
                        

              if ((grid_x_b < 1) .or. (grid_x_b > dlon_hep) .or. (grid_y_b < 1) .or. (grid_y_b > dlat_hep)) then
                  call agent_die_from_matrix_calc(i,jp)
                  print *, "count out three"
                  out_count_priv_b(jp) = out_count_priv_b(jp) + 1
          
                  return
              endif

              if ( hep(grid_x, grid_y, jp, t_hep) <= 0. ) then           ! need better reflection scheme later
                  new_x = old_x
                  new_y = old_y
                  new_ux = cb3(jp)*Ax(i)
                 new_uy = cb3(jp)*Ay(i)
              endif
                      
              !print *, " we get here 3."    

              !if (mod(current_agent%id,123) == 0) then
                  !print *, "agent_move: Agent ID:", current_agent%id, "Old Position:", old_x, old_y, "New Position:", new_x, new_y
                  !print *, "Agent Velocity:", old_ux, old_uy, "New Velocity:", new_ux, new_uy
              !endif

              current_agent%pos_x = new_x
              current_agent%pos_y = new_y

              current_agent%ux = new_ux
              current_agent%uy = new_uy
              !x(i,jp) = new_x
              !y(i,jp) = new_y
              !ux(i,jp) = new_ux
              !uy(i,jp) = new_uy

              call calculate_grid_pos(new_x, new_y, gx, gy)
              call calculate_grid_pos(old_x, old_y, gx0, gy0)

              if ((gx /= gx0) .or. (gy /= gy0)) then
                call grid%move_agent_to_cell(current_agent,gx0,gy0,gx,gy)
              endif 



          end subroutine agent_move_grid

end module mod_movement

