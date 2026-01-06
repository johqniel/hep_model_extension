! =============================================================================
! Module: int_hash_map_module
!
!
! Description:
!   Implements a simple hash map (dictionary) with:
!   - Integer keys
!   - "Any" value (using `class(*)`) <- change this to desired type 
!   - Automatic resizing (doubling) when the load factor is exceeded.
!
! Public Interface:
!   - type(t_int_map)       : The map object.
!   - init_map(map, size)   : Initializes (or clears) a map.
!   - destroy_map(map)      : Frees all allocated memory.
!   - put(map, key, value)  : Adds or updates a key-value pair.
!   - get(map, key)         : Returns the value as `class(*)`.
!   - contains_key(map, key): Returns .true. if the key exists.
!   - get_size(map)         : Returns the number of items.
! =============================================================================
module mod_agent_world

  use mod_config
  use mod_constants
  use mod_counter

  use mod_hashmap

  use mod_grid_id

  use mod_read_inputs

  implicit none
  


   type :: Agent
      integer :: id = -1                                      ! characteristics of the agents
      real(8) :: pos_x = - 1000                      ! x position of the agent
      real(8) :: pos_y = - 1000                      ! y position of the agent
      integer :: gx = -1
      integer :: gy = -1
      real(8) :: ux = 0                                     ! x velocity of the agent   
      real(8) :: uy = 0                                     ! y velocity of the agent 
      character(len=1):: gender = "F"      
      integer :: age = 1000                                        ! age of the agent in ticks
      integer :: number_of_children = 0
      logical :: is_dead = .true.
      integer :: is_pregnant = 0                            ! 0 = not pregnant, n>0 pregnant for n ticks
      integer :: population = -1




      ! the grid 
      type(Grid), pointer :: grid => null()         
      ! the hashmap
      type(t_int_map), pointer :: index_map => null()
      ! the agent array
      type(world_container), pointer :: world => null()


      ! Refferences to other agents:
      integer :: father_of_unborn_child = -1
      integer :: father = -1
      integer :: mother = -1

    contains    

        procedure, public :: update_pos
        procedure, public :: agent_dies

  end type Agent


    type :: world_container
        ! config
        type(world_config) :: config

        ! Data Structures 
        type(Agent), allocatable, dimension(:,:) :: agents
        type(Grid) :: grid
        type(t_int_map) :: index_map
        type(hep_data_type) :: hep_data

        ! Variables
        integer, dimension(:), allocatable :: num_humans
        integer, dimension(:), allocatable :: num_humans_marked_dead
        integer :: number_of_agents_all_time
        
        ! Counters
        type(counter_container) :: counter



        ! other
        type(world_container), pointer :: self

        contains 

            ! to be called in setup part of main pogramm
            procedure, public :: init_world
            procedure, public :: setup_world
            procedure, public :: reset_agents

            ! called by procedures above
            procedure, private :: setup_world_config
            procedure, private :: allocate_agents
            procedure, private :: setup_grid
            procedure, private :: initialize_index_map


            !to be called from agent via: call agent%world%...
            !procedure, public :: agent_spawn
            procedure, private :: get_agent_id
            procedure, public :: generate_agent_born ! we want to change this to private once old program is removed.
            procedure, public :: spawn_agent_hash    ! same 

            ! Usage of grid structure

            procedure, private :: is_agent_in_grid
            procedure, private :: place_agent_in_grid
            procedure, public :: pop_dens_flow_func
            procedure, public :: update_hep_density

        

    end type world_container


contains

  ! ==========================================================================
  !  Public procedures of World
  ! ==========================================================================

    subroutine init_world(world) 
        implicit none
        class(world_container), target :: world

        world%self => world


    end subroutine init_world 

    subroutine setup_world(self)
        class(world_container), intent(inout), target :: self

        integer :: npops
        integer :: pop_size
        integer :: i, j

        call self%setup_world_config()

        npops = self%config%npops
        pop_size = self%config%initial_max_pop_size

        ! allocates agents and num_humans (all agents dead by default)
        call self%allocate_agents(self%num_humans,&
                                self%num_humans_marked_dead, &
                                npops, &
                                pop_size)

        call self%setup_grid() 
        self%grid%config => self%config

        ! Populate grid with HEP data
        self%grid%hep = self%hep_data%matrix
        self%grid%lat_hep = self%hep_data%lat
        self%grid%lon_hep = self%hep_data%lon
        
        ! Populate is_water in grid cells
        if (allocated(self%hep_data%watermask)) then
            do i = 1, self%grid%nx
                do j = 1, self%grid%ny

                    if (self%hep_data%watermask(i,j) == 0) then
                        self%grid%cell(i,j)%is_water = 1
                    else
                        self%grid%cell(i,j)%is_water = 0
                    end if
                end do
            end do
        end if

        call self%initialize_index_map()
        


    end subroutine setup_world

    subroutine reset_agents(self)
        implicit none
        class(world_container), intent(inout) :: self
        
        ! Reset Grid
        call self%grid%reset_grid()

        ! Reset Agents Array
        if (allocated(self%agents)) then
            self%agents%is_dead = .true.
            self%agents%id = -1
        end if

        ! Reset Index Map
        call self%index_map%init_map(self%config%initial_hashmap_size)

        ! Reset Counters
        self%num_humans = 0
        self%num_humans_marked_dead = 0
        self%number_of_agents_all_time = 0

    end subroutine reset_agents

  ! ==========================================================================
  !  Private procedures of World
  ! ==========================================================================

    subroutine setup_world_config(self)
        implicit none
        class(world_container), intent(inout) :: self

        call read_inputs(self%config, self%hep_data)

    end subroutine setup_world_config

    subroutine allocate_agents(self,num_humans_per_pop, num_humans_marked_dead,n_pops, max_hum_per_pop)

        implicit none
        class(world_container), intent(inout) :: self
        integer, intent(in) :: n_pops
        integer, intent(in) :: max_hum_per_pop
        integer, allocatable, target, intent(inout) :: num_humans_per_pop(:) 
        integer, allocatable, target, intent(inout) :: num_humans_marked_dead(:)

        if (allocated(self%agents)) then
            print*, "Warning: agents already allocated."
            deallocate(self%agents)

        endif

        allocate(self%agents(max_hum_per_pop,n_pops))

        if (allocated(num_humans_per_pop)) then
            deallocate(num_humans_per_pop)
        endif
        allocate(num_humans_per_pop(n_pops))

        if (allocated(num_humans_marked_dead)) then
            deallocate(num_humans_marked_dead)
        endif
        allocate(num_humans_marked_dead(n_pops))
        num_humans_per_pop = 0
        num_humans_marked_dead = 0

    end subroutine allocate_agents

    subroutine setup_grid(self)
        implicit none
        class(world_container), intent(inout) :: self
        integer :: nt, nx, ny

        ! Use dimensions from HEP data
        if (allocated(self%hep_data%matrix)) then
            nx = self%hep_data%dlon
            ny = self%hep_data%dlat
            nt = self%hep_data%dtime
        else
            ! Fallback to config (should not happen if read_inputs called)
            nx = self%config%dlon_hep 
            ny = self%config%dlat_hep
            if (self%config%delta_t_hep > 0) then
                nt = int(self%config%Tn / self%config%delta_t_hep) + 1
            else
                nt = 1
            end if
        end if

        self%grid%nx = nx
        self%grid%ny = ny

        call self%grid%allocate_grid(self%config%npops, nt)

    end subroutine setup_grid 

    subroutine initialize_index_map(self)
        implicit none
        class(world_container), intent(inout) :: self

        call self%index_map%init_map(self%config%initial_hashmap_size)

    end subroutine initialize_index_map

    function get_agent_id(self) result(id)
        implicit none
        class(world_container), intent(inout) :: self
        integer :: id

        id = self%number_of_agents_all_time + 1
        self%number_of_agents_all_time = self%number_of_agents_all_time + 1
    end function get_agent_id

    function generate_agent_born(self,mother, father) result(agent_spawned)
        class(world_container) :: self
        type(Agent), pointer, intent(in) :: mother
        type(Agent), pointer, intent(in) :: father


        integer :: population
        type(Agent) :: agent_spawned

        real(8) :: pos_x, pos_y, ux, uy
        real :: r


        if (mother%population == father%population) then 
            population = mother%population
        else 
            population = 3
        endif


        ! Set gender, id and age
        agent_spawned = self%spawn_agent_hash(population)

        ! reset age: 

        agent_spawned%age = 0

        ! Set position and velo = mothers pos, velo
        agent_spawned%pos_x = mother%pos_x
        agent_spawned%pos_y = mother%pos_y
        agent_spawned%ux = mother%ux
        agent_spawned%uy = mother%uy


        ! Set population
        agent_spawned%population = population 




      ! #########    add new agent to child list of father and mother ########

            ! TODO

      ! ################## Gene propagation ####################################

            ! TODO

            ! call get_genes(father)
            ! call get_genes(mother)

            ! call gene_model()
            
            !self.genes = new_genes()
      
    end function generate_agent_born

        function spawn_agent_hash(self,population) result(agent_spawned)
            class(world_container), target, intent(inout) :: self
            integer , intent(in) :: population ! the number of the population and the number of the agent

            type(Agent) :: agent_spawned

            
            integer :: agent_id 
            real :: r
            
            ! ==================================
            ! 1 Initialize agent
            ! ==================================

            agent_spawned%world => self

            agent_id = self%get_agent_id()
            agent_spawned%id = agent_id

            ! ==================================
            ! 2 Set population, is_dead and is_out
            ! ==================================

            agent_spawned%population = population
            agent_spawned%is_dead = .false.

            ! ==================================
            ! 3 Set gender and age
            ! ==================================
            
            call random_number(r)
            if (r < 0.5) then
            agent_spawned%gender = 'M'
            else
            agent_spawned%gender = 'F'
            end if

            call random_number(r)
            agent_spawned%age = int(r * 3500) ! Random age between 0-70
            
            

        end function spawn_agent_hash



  ! ==========================================================================
  !  Public procedures of Agent
  ! ==========================================================================


    subroutine agent_dies(self)
      implicit none
      class(Agent), intent(inout) :: self

      type(t_int_map), pointer :: index_map
      integer :: population

      population = self%population


        if (self%is_dead) then
            print *, "Warning: Attempting to kill an agent that is already dead!"
            return
        end if

        index_map => self%index_map

        if (.not. associated(index_map)) then
            print *, "Warning: Attempting to die an agent with no index map associated!"
            return
        end if


        if (.false. .eqv. contains_key(index_map,self%id)) then
            print *, "Warning: Attempting to die an agent that is not in index map!"
            return
        end if

        if (associated(self%world)) then
             if (self%population < 1 .or. self%population > self%world%config%npops) then
                 print*, "Warning: Unvalid population of agent to be removed."
             endif
        endif




      self%is_dead = .true.
      self%world%num_humans_marked_dead(population) = self%world%num_humans_marked_dead(population) + 1


    end subroutine agent_dies



    subroutine update_pos(self,pos_x, pos_y)
        class(Agent), intent(inout) :: self
        real(8), intent(in) :: pos_x, pos_y

        integer :: gx_new, gy_new, gx_old, gy_old

        call calculate_grid_pos(self%pos_x,self%pos_y,gx_old,gy_old, self%world%config)
        call calculate_grid_pos(pos_x,pos_y,gx_new,gy_new, self%world%config)

        self%pos_x = pos_x
        self%pos_y = pos_y

        self%world%counter%update_pos_calls = self%world%counter%update_pos_calls + 1

        if (.not. (gx_old == gx_new .and. gy_old == gy_new)) then

            if (.not. self%grid%is_in_grid(gx_new,gy_new)) then
                ! Agent moved out of grid
                print*, "Warning: Agent moved out of grid, agent id: ", self%id
                call self%agent_dies()
                return
            end if

            call self%grid%move_agent_to_cell(self%id,&
                                         gx_old,&
                                         gy_old,&
                                         gx_new,&
                                         gy_new)

            self%gx = gx_new
            self%gy = gy_new

        endif



    end subroutine update_pos





  ! ===========================================================================
  ! Data strucutre organising procedures | hashmap <=> agent array
  ! ===========================================================================

  subroutine resize_agent_array_hash(world)
    type(world_container), target, intent(inout) :: world


    type(Agent), allocatable :: new_agents(:,:)
    integer :: fixed_size
    integer :: new_size
    integer :: old_size


    fixed_size = size(world%agents,2)
    old_size = size(world%agents,1)


    new_size = agent_array_resize_factor * old_size
 

    if (new_size == 0) then
        print*, "Warning: new_size = 0, initial size: ", initial_agent_array_size
        new_size = initial_agent_array_size
    end if

    allocate(new_agents(new_size,fixed_size))

    new_agents(1:old_size,1:fixed_size) = world%agents(1:old_size,1:fixed_size)

    deallocate(world%agents)

    call move_alloc(from=new_agents, to=world%agents)


    print*, "Agent array resized to: ", size(world%agents,1)

  end subroutine resize_agent_array_hash

  subroutine compact_agents(self)
    type(world_container), target, intent(inout) :: self

    integer :: population
    integer :: left, right
    type(Agent), pointer :: agent_ptr => null()
    type(Agent), pointer :: replacement_ptr => null()

    ! Loop over each population
    do population = 1, self%config%npops

      if (self%num_humans(population) == 0) cycle

      left = 1
      right = self%num_humans(population)

      do while (left <= right)
        
        agent_ptr => self%agents(left, population)

        if (agent_ptr%is_dead) then
            ! Found a dead agent at 'left'. Clean it up.
            
            ! Remove from grid
            if (associated(agent_ptr%grid) .and. agent_ptr%gx > 0 .and. agent_ptr%gy > 0) then
                call agent_ptr%grid%remove_agent_from_cell(agent_ptr%id, agent_ptr%gx, agent_ptr%gy)
            endif
            
            ! Remove from index map
            call remove(self%index_map, agent_ptr%id)

            ! Now find a living replacement from the right
            do while (right > left)
                replacement_ptr => self%agents(right, population)
                if (replacement_ptr%is_dead) then
                    ! The candidate at 'right' is also dead. Clean it up and keep looking.
                    if (associated(replacement_ptr%grid) .and. replacement_ptr%gx > 0 .and. replacement_ptr%gy > 0) then
                        call replacement_ptr%grid%remove_agent_from_cell(replacement_ptr%id, replacement_ptr%gx, replacement_ptr%gy)
                    endif
                    call remove(self%index_map, replacement_ptr%id)
                    
                    right = right - 1
                else
                    ! Found a living agent at 'right'.
                    exit
                endif
            end do

            if (left == right) then
                ! We met in the middle, and the current slot (left) was dead and cleaned up.
                ! We are done. The new size is left - 1.
                right = right - 1
                exit
            endif

            ! Move the living agent from 'right' to 'left'
            self%agents(left, population) = self%agents(right, population)
            
            ! Update index map for the moved agent
            call update(self%index_map, self%agents(left, population)%id, left)
            
            ! Mark the old slot as dead (optional, but good for safety)
            self%agents(right, population)%is_dead = .true.
            
            ! Decrement right, increment left
            right = right - 1
            left = left + 1

        else
            ! Agent at 'left' is alive. Move on.
            left = left + 1
        endif

      end do

      ! Update the count
      self%num_humans(population) = right
      self%num_humans_marked_dead(population) = 0

    enddo

  end subroutine compact_agents

  subroutine add_agent_to_array_hash(world, new_agent, population, child)
    class(world_container), target, intent(inout) :: world

    type(Agent), intent(inout) :: new_agent

    integer, intent(in) :: population
    type(Agent), pointer, optional :: child


    integer, pointer:: num_agents(:)
    type(Agent), pointer :: agents(:,:)
    type(t_int_map), pointer :: index_map
    type(Grid), pointer :: grid_ptr

    num_agents => world%num_humans
    agents => world%agents
    index_map => world%index_map
    grid_ptr => world%grid


    ! ====================================
    ! 1. connect agent to data structures
    ! ====================================

    new_agent%index_map => index_map     ! Set index map for new agent
    new_agent%world => world             ! Set World for new agent
    new_agent%grid => grid_ptr           ! Set Grid for new agent 


    ! ====================================
    ! 2. insert agent into agent data structures
    ! ====================================

    num_agents(population) = num_agents(population) + 1

    if (num_agents(population) > size(agents,1)) then
        call resize_agent_array_hash(world)
        ! in resize array we move alloc so we have to update agents pointer
        agents => world%agents
    end if


    agents(num_agents(population),population) = new_agent
    agents(num_agents(population),population)%population = population
    agents(num_agents(population),population)%is_dead = .false.


    if (population < 1) then
      print*, "Warning: In add_agent, popuation < 1 !"
    endif

    ! ====================================
    ! 3. insert agent into index map
    ! ====================================

    !print*, "Putting agent in index map..."
    call put(index_map, new_agent%id, population , num_agents(population))

    ! ====================================
    ! 4. insert agent into grid
    ! ====================================

    ! We must call place_agent_in_grid on the element IN THE ARRAY, 
    ! because place_agent_in_grid updates the agent's gx/gy and grid pointer.
    ! If we pass 'new_agent', only the local copy is updated, and the array element remains stale.
    call world%place_agent_in_grid(agents(num_agents(population),population))

    ! Check if agent died during placement (e.g. invalid position)
    if (agents(num_agents(population),population)%is_dead) then
        print *, "Warning: Agent died during placement in add_agent_to_array_hash. Removing from alive list."
        ! Remove from index map
        call remove(index_map, new_agent%id)
        ! Decrement count
        num_agents(population) = num_agents(population) - 1
        ! We don't need to clean up the array slot, it will be overwritten next time.
        ! But we should ensure the caller knows? 
        ! The caller usually expects the agent to be added.
        ! But if it's dead, it's effectively not added.
    end if


    ! ====================================
    ! 5. return agent pointer if needed
    ! ====================================
    
    ! If we need the generated agent as pointer afterward then we can pass a empty pointer, child and 
    ! This subroutine will return it as a pointer to the new agent
    if (present(child)) then
      child => agents(num_agents(population), population)
    endif

  end subroutine add_agent_to_array_hash

  ! ===========================================================================
  ! Return information functions
  ! ===========================================================================

  subroutine count_dead_agents(agents,num_agents, dead_agents)

    type(Agent), allocatable, dimension(:,:), target, intent(in) :: agents
    integer, intent(in) :: num_agents(:)
    integer, intent(out) :: dead_agents(:)

    integer :: i, population
    type(Agent), pointer :: agent_ptr => null()

    dead_agents = 0

    do population = 1, size(num_agents)

      do i = 1, num_agents(population)

        agent_ptr => agents(i,population)

        if (agent_ptr%is_dead) then
            dead_agents(population) = dead_agents(population) + 1
        end if

      end do

    end do

  end subroutine count_dead_agents


  ! ===========================================================================
  ! Using the Hashmap (hashmap exists to get agent by id)
  ! ===========================================================================


  function get_agent(id, world_ptr) result(agent_ptr)
    integer, intent(in) :: id
    type(world_container),pointer, intent(in) :: world_ptr


    type(t_int_map), pointer :: id_map
    type(Agent), pointer, dimension(:,:) :: agents


    type(Agent), pointer :: agent_ptr
    integer :: index, population = 0

    id_map => world_ptr%index_map
    agents => world_ptr%agents

    call get_index_and_pop(id_map, id, index, population)

    if (index == -1 .or. population == -1) then
        ! Agent not found in map
        agent_ptr => null()
        return
    endif

    agent_ptr => agents(index,population)

  end function get_agent

      function get_ith_agent_from_cell(self,i ,gx,gy) result(agent_ptr)
        implicit none
        type(world_container),target, intent(inout) :: self
        integer, intent(in) :: gx, gy, i

        type(Agent), pointer :: agent_ptr
        integer :: agents_id
        type(Grid), pointer :: grid

        grid => self%grid

        if (i < 1) then
            print*, "Error: i is out of bounds, i = ", i
            return
        endif

        if (i > grid%cell(gx,gy)%number_of_agents) then
            print*, "Error: i is out of bounds, i = ", i
            return
        endif

        agents_id = grid%cell(gx,gy)%agents_ids(i)

        agent_ptr => get_agent(agents_id, self)


    end function

  ! ===========================================================================
  ! Using the Grid
  ! ===========================================================================

    logical function is_agent_in_grid(self,agent_ptr)
        implicit none
        class(world_container), intent(in), target :: self
        type(Agent), intent(in) :: agent_ptr

        type(Grid), pointer :: grid
        integer :: gx,gy,k,i,j
        logical :: found

        grid => self%grid
        is_agent_in_grid = .false.

        if (.not. associated(agent_ptr%grid)) then
            print *, "Warning: agent is not associated to any grid."
            return
        endif

        if (.not. associated(agent_ptr%grid,grid) ) then
            print*, "Warning: agent is in a different grid."
            return
        endif

        call calculate_grid_pos(agent_ptr%pos_x, agent_ptr%pos_y, gx, gy, self%config)
        
        found = grid%is_agent_in_cell(agent_ptr%id,gx,gy)

        if (found) then
            is_agent_in_grid = .true.
            return
        endif

        do i = 1, grid%nx
            do j = 1, grid%ny
                found = grid%is_agent_in_cell(agent_ptr%id,i,j)
                if (found) then
                    print*, "Warning: agent found in different cell than expected. Cell: ", i, ",", j
                    is_agent_in_grid = .true.
                    return
                endif
            enddo
        enddo

        print*, "Warning: agent not found in any cell of the grid."


    end function is_agent_in_grid

    subroutine place_agent_in_grid(self,agent_ptr)
        implicit none
        class(world_container), intent(inout), target :: self
        type(Agent), intent(inout) :: agent_ptr

        type(Grid), pointer :: grid
        
        integer :: gx,gy

        grid => self%grid

        call calculate_grid_pos(agent_ptr%pos_x, agent_ptr%pos_y,gx,gy, self%config) 
        ! If agents position is within grid this function writes
        ! the grid koordinates into gx and gy

        if (gx == -1 .or. gy == -1) then
            print*, "gx == -1 or gy == -1"
            print*, "Warning: Ilegal position for agent, cant place -> agent should be killed."
            call agent_ptr%agent_dies()
            self%counter%gxgy_out_counter = self%counter%gxgy_out_counter + 1
            return
        endif

        if (.not. grid%is_in_grid(gx,gy)) then
            print*, "Warning: Ilegal position for agent, cant place -> agent should be killed."
            call agent_ptr%agent_dies()

            return
        endif

        if (.not. associated(agent_ptr%grid, self%grid)) then
            print*, "Warning: Agent to be placed is not associated to any grid. (placing anyway)"

        endif

        agent_ptr%grid => grid ! set grid pointer in agent
        agent_ptr%gx = gx
        agent_ptr%gy = gy

        call grid%place_agent_in_cell(agent_ptr%id,gx,gy)
        
        


    end subroutine place_agent_in_grid

    subroutine pop_dens_flow_func(self, pop_id, pop_dens_adj)
        implicit none
        class(world_container), target, intent(inout) :: self
        integer, intent(in) :: pop_id
        integer, intent(in) :: pop_dens_adj
        
        type(Grid), pointer :: grid
        integer :: i, j, k, id
        real(8) :: flow_x_sum, flow_y_sum
        type(Agent), pointer :: agent_ptr

        grid => self%grid

        ! Update density first (pure density based on counts)
        call grid%update_density_pure()

        ! Calculate flow
        do i = 1, grid%nx
            do j = 1, grid%ny
                flow_x_sum = 0.0d0
                flow_y_sum = 0.0d0
                
                if (grid%cell(i,j)%number_of_agents > 0) then
                    do k = 1, grid%cell(i,j)%number_of_agents
                        id = grid%cell(i,j)%agents_ids(k)
                        if (id > 0) then
                            ! Get agent pointer using module function
                            agent_ptr => get_agent(id, self)
                            if (associated(agent_ptr)) then
                                if (agent_ptr%population == pop_id) then
                                    flow_x_sum = flow_x_sum + agent_ptr%ux
                                    flow_y_sum = flow_y_sum + agent_ptr%uy
                                end if
                            end if
                        end if
                    end do
                end if

                ! Normalize flow by area
                if (grid%cell(i,j)%area > 0.0d0) then
                    grid%cell(i,j)%flow_x = flow_x_sum * 100.0d0 / grid%cell(i,j)%area
                    grid%cell(i,j)%flow_y = flow_y_sum * 100.0d0 / grid%cell(i,j)%area
                else
                    grid%cell(i,j)%flow_x = 0.0d0
                    grid%cell(i,j)%flow_y = 0.0d0
                end if
            end do
        end do

        ! Smoothing
        if (pop_dens_adj > 0) then
             call grid%apply_box_filter(pop_dens_adj)
        else
             do i = 1, grid%nx
                do j = 1, grid%ny
                    grid%cell(i,j)%human_density_smoothed = grid%cell(i,j)%human_density
                end do
             end do
        end if

    end subroutine pop_dens_flow_func

    subroutine update_hep_density(self, pop_id)
        implicit none
        class(world_container), target, intent(inout) :: self
        integer, intent(in) :: pop_id
        
        integer :: pop_dens_adj
        real(8) :: pw, qw
        real(8), allocatable :: wkdens(:,:)
        type(Grid), pointer :: grid

        grid => self%grid

        ! Calculate pop_dens_adj
        ! Formula: sigma_u / 2 / (delta_lat * deg_km)
        ! delta_lat and deg_km are global
        ! sigma_u is in config
        if (self%config%delta_lat > 0.0d0 .and. deg_km > 0.0d0) then
             pop_dens_adj = int(self%config%sigma_u(pop_id) / 2.0d0 / (self%config%delta_lat * deg_km))
        else
             pop_dens_adj = 0
        end if

        ! Call pop_dens_flow_func
        call self%pop_dens_flow_func(pop_id, pop_dens_adj)

        ! Update hep_av
        if (self%config%with_pop_pressure) then
            pw = 0.5d0
            qw = -0.25d0
            
            ! Smooth density (using smooth2d on grid)
            call grid%smooth2d(pw, qw)
            
            ! Calculate population pressure
            ! hep is now in grid: grid%hep(:,:,pop_id, t_hep)
            ! N_max, eta, epsilon are in config
            call grid%pop_pressure_func(grid%hep(:,:,pop_id, grid%t_hep), &
                                        self%config%rho_max(pop_id), &
                                        self%config%eta(pop_id), &
                                        self%config%epsilon(pop_id))
            
            ! Update hep_av
            ! hep_av is now in grid: grid%hep_av
            call update_hep_av_from_grid(grid, pop_id)
            
        else
            ! hep_av is now in grid
            grid%hep_av(:,:,pop_id) = grid%hep(:,:,pop_id, grid%t_hep)
        end if

    end subroutine update_hep_density

    subroutine update_hep_av_from_grid(grid_in, pop_id)
        implicit none
        type(Grid), intent(inout) :: grid_in
        integer, intent(in) :: pop_id
        integer :: i, j
        
        do i = 1, grid_in%nx
            do j = 1, grid_in%ny
                grid_in%hep_av(i,j,pop_id) = grid_in%cell(i,j)%pop_pressure * grid_in%hep(i,j,pop_id, grid_in%t_hep)
            end do
        end do
    end subroutine update_hep_av_from_grid

end module mod_agent_world