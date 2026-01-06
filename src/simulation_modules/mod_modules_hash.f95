
module mod_modules_hash


    use mod_constants
    use mod_config
    use mod_config

    use mod_hashmap


    use mod_rnorm
    use mod_grid_id

    use mod_agent_world





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


subroutine realise_births(current_agent)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent

    type(Agent) :: new_agent
    type(Agent), pointer :: father_ptr

    real :: r ! random number

    type(Grid), pointer :: grid_p
    type(Agent) :: child
    type(Agent), pointer :: father, mother
    integer :: parent_one_id, parent_two_id, population

    if (current_agent%is_pregnant == 0) then
        ! agent is not pregnant
        return
    endif

    ! ELSE: 

    if (current_agent%is_pregnant < current_agent%world%config%pregnancy_minimum_length) then
        ! pregnancy is not done yet
        return
    endif


    call random_number(r)

    if (r > current_agent%world%config%birth_prob_after_min_length) then
        ! the pregnancy is not done yet
        return
    endif


    parent_one_id = current_agent%id
    parent_two_id = current_agent%father_of_unborn_child

    population = current_agent%population


    ! birth occurs

    father_ptr => get_agent(parent_two_id, current_agent%world)
    
    if (.not. associated(father_ptr)) then
        ! Father died during pregnancy.
        father_ptr => current_agent
    endif

    new_agent = generate_agent_born(current_agent%world, current_agent, father_ptr)

    call add_agent_to_array_hash(current_agent%world, new_agent, new_agent%population)
                

end subroutine realise_births


subroutine find_mate(current_agent)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent

    type(world_container), pointer :: world_ptr
    integer :: potential_partner_index_in_cell
    integer :: num_agents_in_cell
    real :: r
    type(Agent), pointer :: potential_partner
    type(Grid), pointer :: grid_ptr

    world_ptr => current_agent%world
    grid_ptr => current_agent%grid

    if (current_agent%gender == 'M') then
        ! Female search model
        return
    end if

    if (current_agent%is_pregnant > 0) then
        ! agent can not be twice pregnant
        return
    endif

    if (current_agent%age < world_ptr%config%age_when_fertile_f) then
        ! agent is to young to get pregnant

        return

    endif


    num_agents_in_cell = grid_ptr%cell(current_agent%gx, current_agent%gy)%number_of_agents

    if (num_agents_in_cell < 2) then
        ! no potential partners
        return
    endif

    call random_number(r)

    potential_partner_index_in_cell = int(r * num_agents_in_cell) + 1

    potential_partner => get_ith_agent_from_cell(world_ptr, potential_partner_index_in_cell, current_agent%gx, current_agent%gy)

    if (.not. associated(potential_partner)) then
        ! Should not happen if grid/map consistent, but safety first
        return
    endif

    ! misses with prob: 1 / num_agents_in_cell
    if (potential_partner%id == current_agent%id) then
        ! same agent
        return
    endif

    ! misses with prob: 1 / 2
    if (potential_partner%gender == 'F') then
        ! potential partner is female
        return
    endif

    ! misses with prob that depends on age distribution
    if (potential_partner%age < world_ptr%config%age_when_fertile_m) then
        ! potential partner is to young to get pregnant
        return
    endif

    ! probability of vertilisation per tick should be adjusted to the three miss probabilities above
    call random_number(r)
    if (r > world_ptr%config%probability_vertilisation_per_tick) then
        ! agent does not get pregnant
        return
    endif

    ! agent gets pregnant
    current_agent%is_pregnant = 1

    current_agent%father_of_unborn_child = potential_partner%id

    


end subroutine find_mate


          subroutine agent_move(current_agent)
                implicit none
                type(Agent), pointer, intent(inout) :: current_agent
                type(world_config), pointer :: config
                type(Grid), pointer :: grid

                real(8) :: old_x, old_y, new_x, new_y
                real(8) :: dx, dy
                integer :: gx, gy
                integer :: i, jp
                logical :: valid_pos
                real(8) :: sigma

                config => current_agent%world%config
                grid => current_agent%grid
                jp = current_agent%population


                if (current_agent%is_dead) return




                old_x = current_agent%pos_x
                old_y = current_agent%pos_y

                
                sigma = sqrt(config%sigma_u(jp)) * sqrt(config%dt) ! Scaling for time step

                valid_pos = .true.



                do i = 1, 10 ! Try 10 times
                    
                    dx = rnorm_single(0.0d0, sigma)
                    dy = rnorm_single(0.0d0, sigma)

                    
                    new_x = old_x + dx / (deg_km * cos(old_y * deg_rad))
                    new_y = old_y + dy / deg_km

                    call calculate_grid_pos(new_x, new_y, gx, gy, config)

                    if (gx < 1 .or. gx > config%dlon_hep .or. gy < 1 .or. gy > config%dlat_hep) then
                        call agent_dies(current_agent)
                        return ! Agent died, stop moving
                    endif

                    if (agent_above_water(gx, gy, jp, grid%t_hep, grid)) then
                        call agent_dies(current_agent)
                        valid_pos = .false.
                        exit
                    endif

                end do



                if (valid_pos) then


                    call current_agent%update_pos(new_x, new_y)
                endif

          end subroutine agent_move


          ! Uses the following functions:

                !=======================================================================
                ! LOGICAL FUNCTION: agent_above_water
                ! Returns .true. if an agent from population jp that is in the grid in
                ! gx,gy is above water. 
                !
                !=======================================================================
                logical function agent_above_water(gx, gy, jp,t_hep, grid_ptr)
                    implicit none 
                    integer, intent(in) :: jp, gx, gy, t_hep
                    type(Grid), pointer :: grid_ptr

                    agent_above_water = .false.

                    if(.not. allocated(grid_ptr%hep)) then
                        agent_above_water = .false.
                        print*, "t_hep not associated."
                        return
                    endif

                    if ( gx < lbound(grid_ptr%hep,1) .or. gx > ubound(grid_ptr%hep,1) .or. &
                        gy < lbound(grid_ptr%hep,2) .or. gy > ubound(grid_ptr%hep,2) .or. &
                        jp < lbound(grid_ptr%hep,3) .or. jp > ubound(grid_ptr%hep,3) .or. &
                        t_hep < lbound(grid_ptr%hep,4) .or. t_hep > ubound(grid_ptr%hep,4) ) then

                        print *, "Warning: Index out of bounds:", gx, gy, jp, t_hep
                        return
                    endif

                    if(grid_ptr%hep(gx, gy, jp, t_hep) == -1 )    then
                        agent_above_water = .true.
                    endif
                end function agent_above_water


                subroutine calculate_gradient(gx,gy,pos_x,pos_y, jp,grad_x,grad_y, grid_ptr) ! In this function there is some kind of coordinate transformation I think we should isolate
                                                    ! that into a seperate function so that we can use it in other places as well 
                    integer, intent(in) :: gx,gy,jp
                    real(8), intent(in) :: pos_x,pos_y
                    real, intent(inout) :: grad_x, grad_y
                    type(Grid), pointer:: grid_ptr

                    !real(8), dimension(0:8) :: heploc
                    !real(8) :: heploc_max
                    integer, dimension(0:8) :: gxx, gyy
                    integer :: iloc, il
                    real(8), dimension(0:8) :: heploc
                    real(8) :: heploc_max

                    heploc(0) = grid_ptr%hep_av(gx,   gy,   jp)   ! hepC
                    gxx   (0) = gx
                    gyy   (0) = gy
                    heploc(1) = grid_ptr%hep_av(gx-1, gy-1, jp)   ! hepSW
                    gxx   (1) = gx-1
                    gyy   (1) = gy-1
                    heploc(2) = grid_ptr%hep_av(gx,   gy-1, jp)   ! hepS
                    gxx   (2) = gx
                    gyy   (2) = gy-1
                    heploc(3) = grid_ptr%hep_av(gx+1, gy-1, jp)   ! hepSE
                    gxx   (3) = gx+1
                    gyy   (3) = gy-1
                    heploc(4) = grid_ptr%hep_av(gx+1, gy,   jp)   ! hepE
                    gxx   (4) = gx+1
                    gyy   (4) = gy
                    heploc(5) = grid_ptr%hep_av(gx+1, gy+1, jp)   ! hepNE
                    gxx   (5) = gx+1
                    gyy   (5) = gy+1
                    heploc(6) = grid_ptr%hep_av(gx,   gy+1, jp)   ! hepN
                    gxx   (6) = gx
                    gyy   (6) = gy+1
                    heploc(7) = grid_ptr%hep_av(gx-1, gy+1, jp)   ! hepNW
                    gxx   (7) = gx-1
                    gyy   (7) = gy+1
                    heploc(8) = grid_ptr%hep_av(gx-1, gy,   jp)   ! hepW
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
                        grad_y = ( heploc_max - heploc(0) ) / ((grid_ptr%lat_hep( gyy(iloc) ) - pos_y)*deg_km)
                    elseif ( iloc == 4 .or. iloc == 8 ) then 
                        grad_x = ( heploc_max - heploc(0) ) / ((grid_ptr%lon_hep( gxx(iloc) ) - pos_x)*cos(pos_y*deg_rad) * deg_km)
                        grad_y = 0.d0
                    else 
                        grad_x = ( heploc_max - heploc(0) ) / ((grid_ptr%lon_hep( gxx(iloc) ) - pos_x)*cos(pos_y*deg_rad) * deg_km)
                        grad_y = ( heploc_max - heploc(0) ) / ((grid_ptr%lat_hep( gyy(iloc) ) - pos_y)*deg_km)
                    endif


                end subroutine calculate_gradient



end module mod_modules_hash

