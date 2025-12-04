module mod_grid_id

use mod_hashmap

use mod_calculations

use mod_globals
    ! Uses:             lon_hep
    !                   lat_ep 
    !                   R (Earth Radius)h


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
            procedure resize_agents_ids_array


            ! procedures that return information about the grid
            procedure is_in_grid 
            procedure count_agents_in_cell
            procedure is_agent_in_cell
            procedure agents_in_grid


            ! procedures that update the information in the cells
            procedure update_density_pure



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

    self%cell(i,j)%area = area_of_gridcell(i,j,lon_hep, lat_hep, R)

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

subroutine allocate_grid(self)
    class(Grid), intent(inout) :: self

    allocate(self%cell(self%nx,self%ny))
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

            end if
        end do


        self%cell(gx,gy)%number_of_agents = self%cell(gx,gy)%number_of_agents - 1




    end subroutine remove_agent_from_cell




    ! ############################################################
    ! ################# grid utilities ###########################
    ! ############################################################








end module mod_grid_id

