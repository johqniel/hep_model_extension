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

  use mod_hashmap

  use mod_grid_id

  use mod_setup

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
      logical :: recently_moved = .false.
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

        ! Data Structures 
        type(Agent), allocatable, dimension(:,:) :: agents
        type(Grid) :: grid
        type(t_int_map) :: index_map

        ! Variables
        integer, dimension(:), allocatable :: num_humans
        integer, dimension(:), allocatable :: num_humans_marked_dead
        integer :: number_of_agents_all_time

        ! config

        type(world_config) :: config


        ! other

        type(world_container), pointer :: self

        contains 

            ! to be called in setup part of main pogramm
            procedure, public :: init_world
            procedure, public :: setup_world

            ! called by procedures above
            procedure, private :: setup_world_config
            procedure, private :: allocate_agents
            procedure, private :: setup_grid
            procedure, private :: initialize_index_map


            !to be called from agent via: call agent%world%...
            !procedure, public :: agent_spawn
            procedure, public :: agent_born
            procedure, private :: get_agent_id
            procedure, public :: generate_agent_born ! we want to change this to private once old program is removed.
            procedure, public :: spawn_agent_hash    ! same 

            ! Usage of grid structure

            procedure, private :: is_agent_in_grid
            procedure, private :: place_agent_in_grid

        

    end type world_container


contains

  ! ==========================================================================
  !  Public procedures of World
  ! ==========================================================================

    subroutine init_world(world) 
        implicit none
        class(world_container), target :: world

        world%self => world

        world%config = read_world_config()

    end subroutine init_world 

    subroutine setup_world(self)
        class(world_container), intent(inout) :: self

        integer :: npops
        integer :: pop_size

        call self%setup_world_config()

        npops = self%config%npops
        pop_size = self%config%initial_max_pop_size

        ! allocates agents and num_humans (all agents dead by default)
        call self%allocate_agents(self%num_humans,&
                                self%num_humans_marked_dead, &
                                npops, &
                                pop_size)

        call self%setup_grid()

        call self%initialize_index_map()


    end subroutine setup_world

  ! ==========================================================================
  !  Private procedures of World
  ! ==========================================================================

    subroutine setup_world_config(self)
        implicit none
        class(world_container), intent(inout) :: self


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

        allocate(num_humans_per_pop(n_pops))
        allocate(num_humans_marked_dead(n_pops))
        num_humans_per_pop = 0
        num_humans_marked_dead = 0

    end subroutine allocate_agents

    subroutine setup_grid(self)
        implicit none
        class(world_container), intent(inout) :: self

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


    subroutine agent_born(self, population, parent_one_id, parent_two_id)  
        implicit none 
        class(world_container), target, intent(inout) :: self

        integer, intent(in) :: parent_one_id, parent_two_id ! parent_one = mother
        integer, intent(in) :: population

        type(Agent), pointer :: agents(:,:)
        type(t_int_map), pointer :: index_map
        type(Grid), pointer :: grid_ptr
        


        integer :: population_size
        type(Agent) :: new_agent
        type(Agent), pointer :: father
        type(Agent), pointer :: mother
        type(Agent), pointer :: child
        real(8) :: pos_x, pos_y

        integer :: gx,gy

        !print*, " We are in function."
        index_map => self%index_map
        grid_ptr => self%grid
        population_size = self%num_humans(population) ! get the size of the population
      
        if (population_size + 1 > size(self%agents,1) ) then
            print*, "Error: Population size exceeded maximum in population ", population
            print*, "pop_size = ", population_size, " max_size = ", size(self%agents,1 )
            return
        end if

    
        father => get_agent(parent_one_id, index_map, agents)
        mother => get_agent(parent_two_id, index_map, agents)

        
        new_agent = self%generate_agent_born(mother, father, population)

        call add_agent_to_array_hash(self%agents,index_map,new_agent,self%num_humans,population,child) ! optional argument to get pointer to new agent

      

        pos_x = new_agent%pos_x     
        pos_y = new_agent%pos_y




        ! PLacemend of the agent in the grid
        !child%grid => grid ! linked agent to the grid this is done in place agent in subroutine
        call self%place_agent_in_grid(child)

        agents_born_counter = agents_born_counter + 1

    end subroutine agent_born

    function generate_agent_born(self,mother, father, population) result(agent_spawned)
        class(world_container) :: self
        type(Agent), pointer, intent(in) :: father
        type(Agent), pointer, intent(in) :: mother
        integer, intent(in) :: population



        type(Agent) :: agent_spawned

        real(8) :: pos_x, pos_y, ux, uy
        real :: r


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
            class(world_container), intent(inout) :: self
            integer , intent(in) :: population ! the number of the population and the number of the agent

            type(Agent) :: agent_spawned

            
            integer :: agent_id 
            real :: r
            
            agent_spawned%population = population


            agent_id = self%get_agent_id()

            agent_spawned%id = agent_id


            
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

        if (self%population < 1 .or. self%population > self%world%config%npops) then
            print*, "Warning: Unvalid population of agent to be removed."
        endif




      self%is_dead = .true.
      self%world%num_humans_marked_dead(population) = self%world%num_humans_marked_dead(population) + 1


    end subroutine agent_dies



    subroutine update_pos(self,pos_x, pos_y)
        class(Agent), intent(inout) :: self
        real(8), intent(in) :: pos_x, pos_y

        integer :: gx_new, gy_new, gx_old, gy_old

        call calculate_grid_pos(self%pos_x,self%pos_y,gx_old,gy_old)
        call calculate_grid_pos(pos_x,pos_y,gx_new,gy_new)

        self%pos_x = pos_x
        self%pos_y = pos_y


        if (.not. (gx_old == gx_new .and. gy_old == gy_new)) then

            call self%grid%move_agent_to_cell(self%id,&
                                         gx_old,&
                                         gy_old,&
                                         gx_new,&
                                         gy_new)

        endif

    end subroutine update_pos





  ! ===========================================================================
  ! Data strucutre organising procedures | hashmap <=> agent array
  ! ===========================================================================

  subroutine resize_agent_array_hash(agents)
    type(Agent), allocatable, dimension(:,:), intent(inout) :: agents

    type(Agent), allocatable :: new_agents(:,:)
    integer :: fixed_size
    integer :: new_size
    integer :: old_size


    fixed_size = size(agents,2)
    old_size = size(agents,1)

    new_size = agent_array_resize_factor * old_size

    if (new_size == 0) then
        new_size = initial_agent_array_size
    end if

    allocate(new_agents(new_size,fixed_size))

    new_agents(1:old_size,1:fixed_size) = agents(1:old_size,1:fixed_size)

    deallocate(agents)

    call move_alloc(from=new_agents, to=agents)


  end subroutine resize_agent_array_hash

  subroutine compact_agents(agents, index_map, num_agents)
    type(Agent), allocatable, dimension(:,:), target, intent(inout) :: agents
    type(t_int_map), intent(inout) :: index_map
    integer, intent(inout) :: num_agents(:)

    integer, allocatable :: free_indeces(:)
    integer, allocatable :: agents_to_move(:)
    integer :: dead_agents(size(num_agents))



    integer :: i, j, population ,n_agents
    integer :: new_index, old_index
    integer :: found_counter 
    logical :: found = .false.
    type(Agent), pointer :: agent_ptr => null()

    call count_dead_agents(agents, num_agents, dead_agents)

    n_agents = size(agents,1)



    do population = 1, size(dead_agents)


      ! For debugging
      !if ( (2 > num_agents(population)) .and. (dead_agents(population) > 0) ) then 
        !print*, "Population: ", population
        !print*, "Num agents: ", num_agents(population)
        !print*, "Num_dead_agents: ", dead_agents(population)
      !endif


      if (dead_agents(population) == 0) then
        cycle
      end if

      if( (num_agents(population) == 0)) then
        print*, "Warning: In compact_agents: num_agents is zero but dead_agents > 0"
        cycle
      end if

      if( num_agents(population) < dead_agents(population) ) then
        print*, "Warning: num_agents = ", num_agents(population), " < ", dead_agents(population), " = dead_agents"
      endif

      allocate(free_indeces(dead_agents(population)))
      allocate(agents_to_move(dead_agents(population)))

      ! By default these two are 0. so we now if they are 0 => no free indeces // => no agents to move
      agents_to_move = 0
      free_indeces = 0

      ! find indeces of dead agents
      j = 0
      i = 1

      do while (j < dead_agents(population))

        agent_ptr => agents(i,population)
        ! Check if agent is dead
        if (agent_ptr%is_dead) then
            
          
          call remove(index_map, agent_ptr%id)

          j = j + 1
          free_indeces(j) = i 


        end if

        i = i + 1


      end do


      if (j /= dead_agents(population)) then
        print*, "Error: in compact_agents: dead_agents count mismatch "
      end if



      j = 0
      found_counter = 0
      do i = 1, dead_agents(population)

        !print*, "Num agents in pop ", population, " : ", num_agents(population)
        !print*, "Num dead agents in pop ", population, " : ", dead_agents(population)
        !print*, "Dead agents found: so far ", found_counter

        found = .false.

        do while ( (found .eqv. .false.) .and. (j < num_agents(population)) )

          if (agents(num_agents(population) - j,population)%is_dead .eqv. .false.) then
              agents_to_move(i) = num_agents(population) - j 
              found = .true.
              j = j + 1
              found_counter = found_counter + 1
              !print*, "found agent."
          else
            j = j + 1
            if (j == num_agents(population)) then
              print*, "Warning: Did not find dead agent. Search index = ", j, " #humans = ", num_agents(population)
            endif
          endif

        end do

      end do

      if (found_counter /= dead_agents(population)) then
        print*, "Error: #found dead agents = ", found_counter, " /= ", dead_agents(population), " = #expected dead agents."
      end if



      !print*, " found agents to move .. " 
      if (num_agents(population) == dead_agents(population)) then
        !print*, "We do not have to move agents."
        ! We ensure that this is not done by setting agents_to_move to 0 by default. 
        ! Then old_index < new_index by default and only larger if there actually is a agent to move.
      endif

      do i = 1, dead_agents(population)

        new_index = free_indeces(i)
        old_index = agents_to_move(i)

        if (old_index < new_index) then
          cycle 
        endif


        agent_ptr => agents(old_index,population)

        call update(index_map, agent_ptr%id , new_index)

        agents(new_index,population) = agents(old_index,population)
        agents(old_index,population)%is_dead = .true.

      end do


      num_agents(population) = num_agents(population) - dead_agents(population)
      dead_agents(population) = 0

      deallocate(agents_to_move)
      deallocate(free_indeces)
    enddo


  end subroutine compact_agents

  subroutine add_agent_to_array_hash(agents, index_map, new_agent, num_agents, population, child)
    type(Agent), allocatable, dimension(:,:), intent(inout), target :: agents
    type(t_int_map), intent(inout), target :: index_map
    type(Agent), intent(inout) :: new_agent
    integer, intent(inout) :: num_agents(:)
    integer, intent(in) :: population
    type(Agent), pointer, optional :: child

    new_agent%index_map => index_map

    num_agents(population) = num_agents(population) + 1

    if (num_agents(population) > size(agents,1)) then
        call resize_agent_array_hash(agents)
    end if

    agents(num_agents(population),population) = new_agent
    agents(num_agents(population),population)%population = population
    agents(num_agents(population),population)%is_dead = .false.


    if (population < 1) then
      print*, "Warning: In add_agent, popuation < 1 !"
    endif

    call put(index_map, new_agent%id, population , num_agents(population))

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


  function get_agent(id, id_map, agents) result(agent_ptr)
    integer, intent(in) :: id
    type(t_int_map), intent(in) :: id_map
    type(Agent), dimension(:,:), target, intent(in) :: agents


    type(Agent), pointer :: agent_ptr
    integer :: index, population = 0

    call get_index_and_pop(id_map, id, index, population)

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

        agent_ptr => get_agent(agents_id, self%index_map, self%agents)


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

        call calculate_grid_pos(agent_ptr%pos_x, agent_ptr%pos_y, gx, gy)
        
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

        call calculate_grid_pos(agent_ptr%pos_x, agent_ptr%pos_y,gx,gy) 
        ! If agents position is within grid this function writes
        ! the grid koordinates into gx and gy

        if (gx == -1 .or. gy == -1) then
            print*, "gx == -1 or gy == -1"
            print*, "Warning: Ilegal position for agent, cant place -> agent should be killed."
            call agent_ptr%agent_dies()
            return
        endif

        if (.not. grid%is_in_grid(gx,gy)) then
            print*, "Warning: Ilegal position for agent, cant place -> agent should be killed."
            call agent_ptr%agent_dies()

            return
        endif

        if (associated(agent_ptr%grid)) then
            print*, "Warning: Agent to be placed in grid is already in a grid. (placing anyway)"

        endif

        agent_ptr%grid => grid ! set grid pointer in agent

        call grid%place_agent_in_cell(agent_ptr%id,gx,gy)
        

        


    end subroutine place_agent_in_grid





end module mod_agent_world