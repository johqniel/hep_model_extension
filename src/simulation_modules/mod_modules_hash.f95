
module mod_modules_hash


    use mod_constants
    use mod_config
    use mod_basic_config

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


subroutine realise_births(current_agent,agents, index_map, num_humans_per_pop)
    implicit none
    type(Agent), pointer, intent(inout) :: current_agent
    type(Agent),allocatable, dimension(:,:), target, intent(inout) :: agents
    integer, dimension(:), intent(inout) :: num_humans_per_pop
    type(t_int_map), intent(inout) :: index_map
    real :: r ! random number

    type(Grid), pointer :: grid_p
    type(Agent) :: child
    type(Agent), pointer :: father, mother
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

                

end subroutine realise_births




          subroutine agent_move(current_agent)
                implicit none
                type(Agent), pointer, intent(inout) :: current_agent
                type(world_config), pointer :: config

                real :: ax, ay ! for randomness
                integer :: i, jp
                type(Grid), pointer :: grid
                
                integer :: population

                real(8) :: new_x, new_y
                real(8) :: new_ux, new_uy
                real(8) :: mu

                real(8) :: old_x, old_y
                real(8) :: old_ux, old_uy

                real(8) :: cb1, cb2, cb3 

                integer :: grid_x, grid_y, grid_x_b, grid_y_b
                real :: gradient_x, gradient_y

                integer :: gx,gy,gx0,gy0 ! for grid movement. 

                config => current_agent%world%config
                population = current_agent%population

                !print*, "agent_move: Agent moving", current_agent%id

                cb1  = config%dt * 1250
                cb2 = config%dt / config%tau(population)
                cb3 = sqrt( config%sigma_u(population))


                mu = 0
                ax = rnorm_single(mu,sqrt(dt))
                ay = rnorm_single(mu,sqrt(dt))

                grid => current_agent%grid
                config => current_agent%world%config

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


                grid_x = floor( ( old_x - config%lon_0 ) / config%delta_lon ) + 1 
                grid_y = floor( ( old_y - config%lat_0 ) / config%delta_lat ) + 1

                ! Check if human above water, then counted as drowned            ! ys, do not like this, redo
                if (agent_above_water(grid_x,grid_y,jp,grid%t_hep, grid)) then
                    call agent_dies(current_agent)
                    current_agent%world%counter%drown_count = current_agent%world%counter%drown_count + 1
                    return 
                endif

                if ( grid_x == 1 .or. grid_x == config%dlon_hep .or. grid_y == 1 .or. grid_y == config%dlat_hep) then
                    ! DN : I dont exactly understand why we remove a agent if this is the case
                    call agent_dies(current_agent)
                    !print *, "agent_move: Agent at boundary, removed from simulation", i, jp
                    current_agent%world%counter%out_count_a = current_agent%world%counter%out_count_a + 1
                    return 
                end if

                call calculate_gradient(grid_x,grid_y,old_x, old_y,jp,gradient_x,gradient_y, grid)

                new_ux = old_ux + cb1*gradient_x - old_ux*cb2 + cb3*ax
                new_uy = old_uy + cb1*gradient_y - old_uy*cb2 + cb3*ay

                new_x = old_x + new_ux / (deg_km * cos(old_y * deg_rad)) * dt
                new_y = old_y + new_uy / deg_km * dt
                

                grid_x_b = floor( ( new_x - config%lon_0 ) / config%delta_lon ) + 1
                grid_y_b = floor( ( new_y - config%lat_0 ) / config%delta_lat ) + 1
                                
                if ((grid_x_b < 1) .or. (grid_x_b > config%dlon_hep) .or. (grid_y_b < 1) .or. (grid_y_b > config%dlat_hep)) then
                    call agent_dies(current_agent)
                    current_agent%world%counter%out_count_b = current_agent%world%counter%out_count_b + 1
                    return
                endif

                if ( grid%hep(grid_x, grid_y, jp, grid%t_hep) <= 0. ) then           ! need better reflection scheme later
                    new_x = old_x
                    new_y = old_y
                    new_ux = cb3*ax
                    new_uy = cb3*ay
                endif
                        
                current_agent%pos_x = new_x
                current_agent%pos_y = new_y

                current_agent%ux = new_ux
                current_agent%uy = new_uy

                call calculate_grid_pos(new_x, new_y, gx, gy, config)
                call calculate_grid_pos(old_x, old_y, gx0, gy0, config)

                if ((gx /= gx0) .or. (gy /= gy0)) then
                    current_agent%gx = gx0
                    current_agent%gy = gy0
                    current_agent%recently_moved = .true.
                end if

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

                    if(grid_ptr%hep(gx, gy, jp, t_hep) <= 0. )    then
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

