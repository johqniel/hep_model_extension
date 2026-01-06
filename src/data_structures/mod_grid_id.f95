module mod_grid_id

use mod_hashmap

use mod_calculations



use mod_config


implicit none

type :: grid_cell

    ! Technical variables
    integer :: i = 0! position in grid
    integer :: j = 0

    ! Constant properties of cell
    real(8) :: lon_in = 0
    real(8) :: lat_in = 0
    real(8) :: area = 0

    ! Variable properties of cell
    integer :: number_of_agents = 0
    real(8) :: human_density = 0
    real(8) :: human_density_smoothed = 0
    real(8) :: flow_x = 0
    real(8) :: flow_y = 0
    real(8) :: pop_pressure = 0
    integer :: is_water = 0

    integer, allocatable :: agents_ids(:) ! to store the ids of the agents in the cell


    !contains
    !
    !    procedure, pass(self) :: clear_cell => clear_cell
    !    procedure, pass(self) :: initialize_cell =>  initialize_cell

end type grid_cell

    



type :: Grid    
        type(grid_cell), dimension(:,:), allocatable :: cell
        integer :: nx = 0
        integer :: ny = 0
        integer :: npops = 0
        integer :: nt = 0
        integer :: t_hep = 1

        real(8), allocatable :: hep(:,:,:,:)     ! (nx, ny, npops, nt)
        real(8), allocatable :: hep_av(:,:,:)    ! (nx, ny, npops)
        real(8), allocatable :: dens(:,:,:)      ! (nx, ny, npops)
        real(8), allocatable :: dens_adj(:,:,:)  ! (nx, ny, npops)
        real(8), allocatable :: pop_pressure_arr(:,:,:) ! (nx, ny, npops)
        real(8), allocatable :: distm(:,:,:)     ! (nx, ny, npops)
        real(8), allocatable :: flow(:,:,:,:)    ! (2, nx, ny, npops)
        real(8), allocatable :: flow_acc(:,:,:,:)! (2, nx, ny, npops)
        real(8), allocatable :: area_for_dens(:,:) ! (nx, ny)
        integer, allocatable :: idens(:,:,:)     ! (nx, ny, npops)
        
        real(8), allocatable :: lon_hep(:)
        real(8), allocatable :: lat_hep(:)

        type(world_config), pointer :: config => null()



        contains
            



            ! procedures to manage individual cell
            procedure initialize_cell
            procedure clear_cell

            ! procedures to manage agents in grid
            procedure place_agent_in_cell
            procedure remove_agent_from_cell
            procedure move_agent_to_cell

            ! procedures to manage the grid
            procedure allocate_grid
            procedure clear_grid   
            procedure reset_grid
            procedure resize_agents_ids_array


            ! procedures that return information about the grid
            procedure is_in_grid 
            procedure count_agents_in_cell
            procedure is_agent_in_cell
            procedure agents_in_grid


            ! procedures that update the information in the cells
            procedure update_density_pure
            procedure smooth2d
            procedure pop_pressure_func
            procedure apply_box_filter



end type Grid

contains

! utilities



function area_of_gridcell(i,j, lon_in, lat_in, R) result(area)
    implicit none
    integer, intent(in) :: i, j
    real(8), dimension(:), intent(in) :: lon_in, lat_in
    real(8), intent(in) :: R       ! Earth's radius [km]
    real(8) :: area                ! Resulting area [km^2]

    real(8) :: lat1, lat2, lon1, lon2
    real(8) :: haversine1, haversine2, dist_lon1, dist_lon2, haversine_lat, dist_lat
    real(8), parameter :: pi = 3.14159265358979

    ! Compute latitude borders
    if (j == 1) then
        lat1 = lat_in(1) - (lat_in(2) - lat_in(1)) / 2.0
    else
        lat1 = (lat_in(j) + lat_in(j-1)) / 2.0
    end if

    if (j == size(lat_in)) then
        lat2 = lat_in(j) + (lat_in(j) - lat_in(j-1)) / 2.0
    else
        lat2 = (lat_in(j+1) + lat_in(j)) / 2.0
    end if

    ! Compute longitude borders
    if (i == 1) then
        lon1 = lon_in(1) - (lon_in(2) - lon_in(1)) / 2.0
    else
        lon1 = (lon_in(i) + lon_in(i-1)) / 2.0
    end if

    if (i == size(lon_in)) then
        lon2 = lon_in(i) + (lon_in(i) - lon_in(i-1)) / 2.0
    else
        lon2 = (lon_in(i+1) + lon_in(i)) / 2.0
    end if

    ! Latitude distance using haversine
    haversine_lat = sin(abs(lat2 - lat1) * pi / (2.0 * 180.0))**2
    dist_lat = 2.0 * R * atan2(sqrt(haversine_lat), sqrt(1.0 - haversine_lat))

    ! Longitude distances at both latitudes
    haversine1 = cos(lat2 * pi / 180.0)**2 * sin(abs(lon2 - lon1) * pi / (2.0 * 180.0))**2
    dist_lon1 = 2.0 * R * atan2(sqrt(haversine1), sqrt(1.0 - haversine1))

    haversine2 = cos(lat1 * pi / 180.0)**2 * sin(abs(lon2 - lon1) * pi / (2.0 * 180.0))**2
    dist_lon2 = 2.0 * R * atan2(sqrt(haversine2), sqrt(1.0 - haversine2))

    ! Trapezoid-like area approximation
    area = (dist_lon1 + dist_lon2) / 2.0 * sqrt(dist_lat**2 - ((dist_lon1 - dist_lon2)/2.0)**2)

end function area_of_gridcell

 


! Procedures of spatial_grid type
subroutine update_density_pure(self)
    implicit none
    class(Grid), intent(inout) :: self


    integer :: i,j, nx, ny

    real :: density

    nx = self%nx
    ny = self%ny

    do i = 1, nx
        do j = 1, ny
            density = self%cell(i,j)%number_of_agents * 100.0 / self%cell(i,j)%area

            self%cell(i,j)%human_density = density

        enddo
    enddo

    


end subroutine update_density_pure

subroutine clear_cell(self,i,j)
    implicit none
    class(Grid), intent(inout) :: self
    integer, intent(in) :: i,j
    self%cell(i,j)%number_of_agents = 0
    self%cell(i,j)%human_density = 0
    deallocate( self%cell(i,j)%agents_ids )
end subroutine clear_cell



subroutine initialize_cell(self,i,j)
    implicit none
    class(Grid), intent(inout) :: self
    integer, intent(in) :: i,j

    self%cell(i,j)%area = area_of_gridcell(i,j,self%lon_hep, self%lat_hep, 6371.0d0)

    allocate(self%cell(i,j)%agents_ids(initial_array_size_for_agents_ids_in_gridcell) )
    self%cell(i,j)%agents_ids = -1

end subroutine initialize_cell


integer function agents_in_grid(self) 
    implicit none
    class(Grid), intent(in) :: self

    integer :: i,j

    agents_in_grid = 0

    do i = 1, self%nx
        do j = 1, self%ny
            agents_in_grid = agents_in_grid + self%cell(i,j)%number_of_agents
        enddo
    enddo

end function agents_in_grid


integer function count_agents_in_cell(self,i,j) result(counter)
    implicit none
    class(Grid), intent(in) :: self
    integer, intent(in) :: i,j

    integer :: k 

    counter = 0

    do k = 1, size(self%cell(i,j)%agents_ids)

        if ( self%cell(i,j)%agents_ids(k) /= -1 ) then
            counter = counter + 1
        endif

    enddo
    

    if (counter /= self%cell(i,j)%number_of_agents) then
        print*, "Warning: count_agents_in_cell mismatch with number_of_agents variable."
    endif

end function count_agents_in_cell




logical function is_in_grid(self,gx,gy) 
    implicit none
    class(Grid), intent(in) :: self
    integer, intent(in) :: gx,gy

    is_in_grid = .true.

    if (gx < 1 .or. gx > self%nx .or. gy < 1 .or. gy > self%ny) then
        is_in_grid = .false.
    endif

end function is_in_grid

logical function is_agent_in_cell(self,id, gx,gy)
    implicit none
    class(Grid), intent(in) :: self
    integer, intent(in) :: id
    integer, intent(in) :: gx,gy

    integer :: k

    is_agent_in_cell = .false.

    do k = 1, size(self%cell(gx,gy)%agents_ids)
        if ( self%cell(gx,gy)%agents_ids(k) == id ) then
            is_agent_in_cell = .true.
            return
        endif
    enddo

end function is_agent_in_cell   

subroutine allocate_grid(self, npops_in, nt_in)
    class(Grid), intent(inout) :: self
    integer, intent(in), optional :: npops_in, nt_in
    integer :: i, j

    if (present(npops_in)) self%npops = npops_in
    if (present(nt_in)) self%nt = nt_in

    if (allocated(self%cell)) deallocate(self%cell)
    allocate(self%cell(self%nx,self%ny))
    
    if (self%npops > 0) then
        if (allocated(self%hep_av)) deallocate(self%hep_av)
        allocate(self%hep_av(self%nx, self%ny, self%npops))
        
        if (allocated(self%dens)) deallocate(self%dens)
        allocate(self%dens(self%nx, self%ny, self%npops))
        
        if (allocated(self%dens_adj)) deallocate(self%dens_adj)
        allocate(self%dens_adj(self%nx, self%ny, self%npops))
        
        if (allocated(self%pop_pressure_arr)) deallocate(self%pop_pressure_arr)
        allocate(self%pop_pressure_arr(self%nx, self%ny, self%npops))
        
        if (allocated(self%distm)) deallocate(self%distm)
        allocate(self%distm(self%nx, self%ny, self%npops))
        
        if (allocated(self%flow)) deallocate(self%flow)
        allocate(self%flow(2, self%nx, self%ny, self%npops))
        
        if (allocated(self%flow_acc)) deallocate(self%flow_acc)
        allocate(self%flow_acc(2, self%nx, self%ny, self%npops))
        
        if (allocated(self%idens)) deallocate(self%idens)
        allocate(self%idens(self%nx, self%ny, self%npops))
        
        if (self%nt > 0) then
            if (allocated(self%hep)) deallocate(self%hep)
            allocate(self%hep(self%nx, self%ny, self%npops, self%nt))
        endif
    endif
    
    if (allocated(self%area_for_dens)) deallocate(self%area_for_dens)
    allocate(self%area_for_dens(self%nx, self%ny))
    
    if (allocated(self%lon_hep)) deallocate(self%lon_hep)
    allocate(self%lon_hep(self%nx))
    
    if (allocated(self%lat_hep)) deallocate(self%lat_hep)
    allocate(self%lat_hep(self%ny))

    ! Initialize cells
    do i = 1, self%nx
        do j = 1, self%ny
            call self%initialize_cell(i,j)
        end do
    end do
    
end subroutine allocate_grid




subroutine clear_grid(self)
    implicit none
    class(Grid), intent(inout) :: self

    integer :: i,j


    do i = 1, self%nx
        do j = 1, self%ny
            call self%clear_cell(i,j)
        end do
    end do
end subroutine clear_grid

subroutine reset_grid(self)
    implicit none
    class(Grid), intent(inout) :: self
    integer :: i,j
    do i = 1, self%nx
        do j = 1, self%ny
            call self%clear_cell(i,j)
            call self%initialize_cell(i,j)
        end do
    end do
end subroutine reset_grid



    subroutine move_agent_to_cell(self,agent_id,gx_old,gy_old,gx_new,gy_new)
        implicit none
        class(Grid), intent(inout) :: self
        integer, intent(in) :: agent_id
        integer :: gx_new, gy_new, gx_old, gy_old


        call self%place_agent_in_cell(agent_id,gx_new,gy_new)
        call self%remove_agent_from_cell(agent_id, gx_old, gy_old)

    end subroutine move_agent_to_cell





    subroutine place_agent_in_cell(self,agent_id,gx,gy)
        implicit none
        class(Grid), intent(inout) :: self
        integer, intent(in) :: agent_id
        integer :: gx, gy

        !real :: pos_x, pos_y
        integer number_of_agents
        logical :: placed

        placed = .false.

        if (.not. self%is_in_grid(gx,gy)) then
            print*, "Warning: Ilegal position for agent, cant place."
            print*, " This should not happen -> agent should be killed."
            return
        endif
        
        if (self%cell(gx,gy)%number_of_agents >= size(self%cell(gx,gy)%agents_ids)) then
            call self%resize_agents_ids_array(gx,gy)
            !print*, "Error: trying to place agent in full cell. Agent id: ", agent%id
            !return
        endif

        number_of_agents = self%cell(gx,gy)%number_of_agents
        self%cell(gx,gy)%agents_ids(number_of_agents+1) = agent_id

        self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents + 1

    end subroutine place_agent_in_cell



    subroutine resize_agents_ids_array(self,gx,gy)
        implicit none
        class(Grid), intent(inout) :: self
        integer, intent(in) :: gx,gy

        integer :: old_size
        integer, allocatable :: temp_array(:)

        if (.not. self%is_in_grid(gx,gy)) then
            print*, "Warning: Trying to resize id array in cell that is not in grid."
            return
        endif

        if (.not. allocated(self%cell(gx,gy)%agents_ids)) then
            print*, "Warning: Trying to resize id array, but id array not allocated."
            return
        endif

        if ( size(self%cell(gx,gy)%agents_ids) == 0 ) then
            print*, "Warning: Trying to resize id array, but id array size is zero."
            return
        endif

        old_size = size(self%cell(gx,gy)%agents_ids)

        allocate( temp_array( old_size * 2 ) )
        temp_array = -1
        temp_array(1:old_size) = self%cell(gx,gy)%agents_ids
        deallocate( self%cell(gx,gy)%agents_ids )
        call move_alloc( temp_array, self%cell(gx,gy)%agents_ids )

    end subroutine resize_agents_ids_array



    subroutine remove_agent_from_cell(self,agent_id,gx,gy)
        implicit none
        class(Grid), intent(inout) :: self
        integer, intent(in) :: agent_id
        integer :: gx, gy, i, num_agents_old
        
        num_agents_old = self%cell(gx,gy)%number_of_agents

        if (num_agents_old == 0) then
            print*, "Waring: Trying to remove agent from empty cell. remove_agent_from_cell_new"
            return
            
            
        endif


        ! Find the agent in the agents_indeces array
        do i = 1, num_agents_old
            if ( self%cell(gx,gy)%agents_ids(i) == agent_id ) then
                ! Found the agent, now remove it by shifting the array
                if (i < num_agents_old) then
                    self%cell(gx,gy)%agents_ids(i:num_agents_old-1) = &
                        self%cell(gx,gy)%agents_ids(i+1:num_agents_old)
                end if

                self%cell(gx,gy)%agents_ids(num_agents_old) = -1
                self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents - 1
                return 
            end if
        end do
        
        print*, "Warning: Agent ", agent_id, " not found in cell ", gx, gy, " during removal."




    end subroutine remove_agent_from_cell




    ! ############################################################
    ! ################# grid utilities ###########################
    ! ############################################################










    subroutine apply_box_filter(self, adj)
        implicit none
        class(Grid), intent(inout) :: self
        integer, intent(in) :: adj
        
        integer :: i, j, k, l
        integer :: left, right, lower, upper
        integer :: amo_grid
        real(8), allocatable :: temp_dens(:,:)
        
        allocate(temp_dens(self%nx, self%ny))
        temp_dens = 0.0d0

        do i = 1, self%nx
            left = -adj
            right = adj
            if (i < adj + 1) left = 1 - i
            if (i > self%nx - adj) right = self%nx - i
            
            do j = 1, self%ny
                ! Check for water? Original checked hep(i,j) == water_hep.
                ! We don't have hep passed in here. 
                ! Assuming we smooth everywhere or need hep.
                ! For now, I'll smooth everywhere.
                
                lower = -adj
                upper = adj
                if (j < adj + 1) lower = 1 - j
                if (j > self%ny - adj) upper = self%ny - j
                
                amo_grid = (right - left + 1) * (upper - lower + 1)
                
                do k = left, right
                    do l = lower, upper
                        temp_dens(i,j) = temp_dens(i,j) + self%cell(i+k, j+l)%human_density
                    end do
                end do
                
                if (amo_grid > 0) then
                    temp_dens(i,j) = temp_dens(i,j) / real(amo_grid, 8)
                end if
            end do
        end do
        
        do i = 1, self%nx
            do j = 1, self%ny
                self%cell(i,j)%human_density_smoothed = temp_dens(i,j)
            end do
        end do
        
        deallocate(temp_dens)
    end subroutine apply_box_filter

    subroutine smooth2d(self, p, q)
        implicit none
        class(Grid), intent(inout) :: self
        real(8), intent(in) :: p, q
        
        real(8), allocatable :: f(:,:), x_new(:,:)
        integer :: i, j
        
        allocate(f(self%nx, self%ny))
        allocate(x_new(self%nx, self%ny))
        
        ! Copy current smoothed density (or raw if not smoothed yet) to f
        ! Usually smooth2d operates on a field. Let's assume it operates on human_density 
        ! and updates human_density_smoothed.
        
        do i = 1, self%nx
            do j = 1, self%ny
                f(i,j) = self%cell(i,j)%human_density
            end do
        end do
        
        x_new = f ! Initialize
        
        ! Apply smoothing (avoiding boundaries for simplicity or handling them)
        ! Original smooth2d loops 2 to dlon-1
        
        do i = 2, self%nx - 1
            do j = 2, self%ny - 1
                x_new(i,j) = f(i,j) + p * (f(i+1,j) + f(i,j+1) + f(i-1,j) + f(i,j-1) - 4.0d0*f(i,j)) &
                           + q * (f(i+1,j+1) + f(i-1,j+1) + f(i-1,j-1) + f(i+1,j-1) - 4.0d0*f(i,j))
            end do
        end do
        
        ! Update grid
        do i = 1, self%nx
            do j = 1, self%ny
                self%cell(i,j)%human_density_smoothed = x_new(i,j)
            end do
        end do
        
        deallocate(f, x_new)
    end subroutine smooth2d

    subroutine pop_pressure_func(self, hep, N_max, eta, epsilon)
        implicit none
        class(Grid), intent(inout) :: self
        real(8), dimension(:,:), intent(in) :: hep
        real(8), intent(in) :: N_max, eta, epsilon
        
        integer :: i, j
        real(8) :: rho, rho_c, delta_rho, max_pp
        
        max_pp = (eta/epsilon) * (1.0d0 - 1.0d0/eta)**(1.0d0 - 1.0d0/eta) * exp(-(1.0d0 - 1.0d0/eta))
        
        do i = 1, self%nx
            do j = 1, self%ny
                rho = self%cell(i,j)%human_density
                rho_c = N_max * hep(i,j)
                
                if (rho_c > 0.0d0) then
                    delta_rho = rho / rho_c
                    self%cell(i,j)%pop_pressure = (eta/epsilon) * (delta_rho/epsilon)**(eta-1.0d0) * &
                                                  exp(-(delta_rho/epsilon)**eta) / max_pp
                else
                    self%cell(i,j)%pop_pressure = 0.0d0 ! Or handle as needed
                end if
                
                if (hep(i,j) <= 0.0d0) then
                    self%cell(i,j)%pop_pressure = 1.0d0
                end if
            end do
        end do
        
    end subroutine pop_pressure_func

end module mod_grid_id

