module mod_modules_hash


    use mod_globals 
    ! Uses:     - t_hep
    !           - hep
        ! Uses:     - cb2, cb3

    !           - delta_lon/lat, lon0/lat0
    !           - hep_av

    use mod_agent_hashmap

    use mod_agent_core

    use mod_rnorm





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

    use mod_grid_id


    use mod_calculations
    ! Uses: 
    !
    !       subroutine calculate_grid_pos




contains

subroutine realise_natural_deaths(current_agent)
    type(Agent), pointer, intent(inout) :: current_agent
    real :: r ! random number 

    call random_number(r)

    if ( r < calc_natural_death_prob(current_agent%age)) then
        call agent_dies(current_agent)
    end if

end subroutine realise_natural_deaths

real function calc_natural_death_prob(age) result(prob)
    implicit none
    integer, intent(in) :: age ! in ticks

    integer :: x
    integer :: age_in_years

    age_in_years = age / 52



    if (age_in_years > 40 .and. age_in_years < 80) then
        x = (2 * 40 - age)
    endif

    if ( age_in_years > 79 ) then
        x = 0
    endif

    

    prob = 0.0025 * (1 / (log(200 * real(x + 1))+1))

    


    ! natural death prob per tick starts at x% per tick for newborns and then 

    ! goes down and ecentually goes back up 

    

end function calc_natural_death_prob




    subroutine update_age_pregnancy(current_agent)
        implicit none
        type(Agent), pointer, intent(inout) :: current_agent

        current_agent%age = current_agent%age + 1

        if (current_agent%is_pregnant > 0) then
            current_agent%is_pregnant = current_agent%is_pregnant + 1
        end if

    end subroutine update_age_pregnancy


subroutine realise_births(current_agent,agents, index_map, num_humans_per_pop)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent
    type(Agent),allocatable, dimension(:,:), target, intent(inout) :: agents
    integer, dimension(:), intent(inout) :: num_humans_per_pop
    type(t_int_map), intent(inout) :: index_map
    real :: r ! random number

    integer :: parent_one_id, parent_two_id, population


    if (current_agent%is_pregnant < pregnancy_minimum_length) then
        ! pregnancy is not done yet
        return
    endif


    call random_number(r)

    if (r > birth_prob_after_min_length) then
        ! the pregnancy is not done yet
        return
    endif


    parent_one_id = current_agent%id
    parent_two_id = current_agent%father_of_unborn_child

    population = current_agent%population


    ! birth occurs
    select type(grid_p => current_agent%grid)

    type is (spatial_grid)

        if (.not. associated(grid_p)) then
            print*, "agent has unassociated %grid in realise birth."
        endif
        
        call agent_born(agents, grid_p, index_map, num_humans_per_pop, population, parent_one_id, parent_two_id)



    class default
        print*, "current_agent%grid is not spatial grid."
        return
    end select

    current_agent%is_pregnant = 0
    current_agent%father_of_unborn_child  = -1

    realised_birth_counter = realised_birth_counter + 1


                

end subroutine realise_births

subroutine find_mate(female)
    implicit none
    type(Agent), pointer, intent(inout) :: female


    type(Agent), pointer :: selected_male
    integer :: i, j
    real :: r


    if (.not. associated(female)) then
        print*, "Error: Agent not associated in find mate."
        return
    endif

    if (female%is_dead) then
        print*, "Warning: agents should not be dead in this part of the program."
        return
    endif

    if (female%gender /= 'F') then
        ! "We use a female choice mating model. "
        return
    endif

    if (female%is_pregnant > 0 ) then
        ! Female already pregnant
        return
    endif

    if(female%age < age_when_vertile_f) then  
        !to young to be pregnant
        return
                    
    endif

    if (female%age > age_until_vertile_f) then
        ! female to old to be pregnant
        return
    endif



    if (.not. associated(female%grid)) then
        print*, "Error: Agent with  %grid not associated, in function find mate."
        return
    endif

    call calculate_grid_pos(female%pos_x, female%pos_y, i, j)
    current_agent_ptr => null()
    selected_male => null()


    select type(grid_p => female%grid)


    type is (spatial_grid)
        current_agent_ptr => grid_p%cell(i,j)%agents
    class default
        print*, "current_agent%grid is not spatial grid."
    end select


    do while (associated(current_agent_ptr))

        ! A: Check if potential partner associated
        if (.not. associated(current_agent_ptr%node)) then
            print*, "Problem in pointer list."
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! B: Check if potential partner male
        if (current_agent_ptr%node%gender /= 'M') then
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! C: Check if potential partner old enough
        if (current_agent_ptr%node%age < age_when_vertile_m) then
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! D: Check if potential partner to old
        if (current_agent_ptr%node%age > age_until_vertile_m) then
            current_agent_ptr => current_agent_ptr%next
            cycle
        endif

        ! E: Choose Partner
        selected_male => current_agent_ptr%node
        current_agent_ptr => null()
        
        

    
    enddo

    if (.not. associated(selected_male)) then
        ! No mate found in proximity of female
        return
    endif


    found_mates_counter = found_mates_counter + 1    

    ! D: Run probability if mating successfull
    call random_number(r)

    if (r > probability_vertilisation_per_tick ) then
        ! mating not successful
        return
    endif

   ! print*, "5"


    pregnancy_counter = pregnancy_counter + 1

    female%is_pregnant = 1
    !print*, "at origin"
    if (.not. associated(selected_male%children)) then

    endif

    female%father_of_unborn_child => selected_male
    pregnancy_counter = pregnancy_counter + 1



end subroutine find_mate





          subroutine agent_move(current_agent)
                implicit none
                type(Agent), pointer, intent(inout) :: current_agent

                real :: ax, ay ! for randomness

                integer :: i, jp
                type(spatial_grid), pointer :: grid

                
                real(8) :: new_x, new_y
                real(8) :: new_ux, new_uy
                real(8) :: mu

                real(8) :: old_x, old_y
                real(8) :: old_ux, old_uy

                integer :: grid_x, grid_y, grid_x_b, grid_y_b
                real :: gradient_x, gradient_y

                integer :: gx,gy,gx0,gy0 ! for grid movement. 


                mu = 0
                ax = rnorm_single(mu,sqrt(dt))
                ay = rnorm_single(mu,sqrt(dt))



                select type(g => current_agent%grid)

                type is (spatial_grid)

                    grid => g

                class default
                    print*, "Error: current_agent%grid is not spatial grid.", current_agent%id
                end select




                ! probably we can clean this function like this: 
                ! gx,gy, and grid_x, and grid_y are the same but computet differently 
                ! eliminate one and uses function calculate_grid_pos 



                old_x = current_agent%pos_x
                old_y = current_agent%pos_y 
                old_ux = current_agent%ux
                old_uy = current_agent%uy

                jp = current_agent%population



                if (current_agent%is_dead) then
                        print*, "Trying to move a dead agent, skipping. agent_move", current_agent%id
                    return
                endif

                ! Check if a human left the research area, then counted as out
                if (.false. .eqv. in_research_area(old_x, old_y)) then
                    call agent_dies(current_agent)
                    !out_count_priv(jp) = out_count_priv(jp) + 1
                

                    return 
                endif




                grid_x = floor( ( old_x - lon_0 ) / delta_lon ) + 1 
                grid_y = floor( ( old_y - lat_0 ) / delta_lat ) + 1

                ! Check if human above water, then counted as drowned            ! ys, do not like this, redo
                if (agent_above_water(grid_x,grid_y,jp,t_hep)) then
                    call agent_dies(current_agent)
                    !drown_count_priv(jp) = drown_count_priv(jp) + 1


                    return 
                endif


                if ( grid_x == 1 .or. grid_x == dlon_hep .or. grid_y == 1 .or. grid_y == dlat_hep) then
                    ! DN : I dont exactly understand why we remove a agent if this is the case
                    call agent_dies(current_agent)
                    !print *, "agent_move: Agent at boundary, removed from simulation", i, jp
                    !out_count_priv_a(jp) = out_count_priv_a(jp) + 1

                    return 

                end if

                call calculate_gradient(grid_x,grid_y,old_x, old_y,jp,gradient_x,gradient_y)


                new_ux = old_ux + cb1(jp)*gradient_x - old_ux*cb2(jp) + cb3(jp)*ax
                new_uy = old_uy + cb1(jp)*gradient_y - old_uy*cb2(jp) + cb3(jp)*ay

                new_x = old_x + new_ux / (deg_km * cos(old_y * deg_rad)) * dt
                new_y = old_y + new_uy / deg_km * dt

                
                call movement_at_boundary(old_x,old_y,old_ux,old_uy,new_x,new_y,new_ux,new_uy)

                grid_x_b = floor( ( new_x - lon_0 ) / delta_lon ) + 1
                grid_y_b = floor( ( new_y - lat_0 ) / delta_lat ) + 1
                                
                            

                if ((grid_x_b < 1) .or. (grid_x_b > dlon_hep) .or. (grid_y_b < 1) .or. (grid_y_b > dlat_hep)) then
                    call agent_dies(current_agent)

                    !out_count_priv_b(jp) = out_count_priv_b(jp) + 1
            
                    return
                endif

                if ( hep(grid_x, grid_y, jp, t_hep) <= 0. ) then           ! need better reflection scheme later
                    new_x = old_x
                    new_y = old_y
                    new_ux = cb3(jp)*ax
                    new_uy = cb3(jp)*ay
                endif
                        

                current_agent%pos_x = new_x
                current_agent%pos_y = new_y

                current_agent%ux = new_ux
                current_agent%uy = new_uy


                call calculate_grid_pos(new_x, new_y, gx, gy)
                call calculate_grid_pos(old_x, old_y, gx0, gy0)


                if ((gx /= gx0) .or. (gy /= gy0)) then
                    current_agent%gx = gx0
                    current_agent%gy = gy0
                    current_agent%recently_moved = .true.

                end if




          end subroutine agent_move


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

                        print *, "Warning: Index out of bounds:", gx, gy, jp, t_hep
                        return
                    endif

                    if(hep(gx, gy, jp, t_hep) <= 0. )    then
                        agent_above_water = .true.
                    endif
                end function agent_above_water


                subroutine calculate_gradient(gx,gy,pos_x,pos_y, jp,grad_x,grad_y) ! In this function there is some kind of coordinate transformation I think we should isolate
                                                    ! that into a seperate function so that we can use it in other places as well 
                    integer, intent(in) :: gx,gy,jp
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

end module mod_modules_hash

