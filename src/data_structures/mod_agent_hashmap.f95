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
module mod_agent_hashmap

  use mod_globals

  implicit none
  
  type, abstract :: dummy_grid

    contains
  end type dummy_grid


  ! --- Public Types and Procedures ---
  public :: init_map, destroy_map, put, update, get, remove, contains_key, get_size, get_capacity


  ! --- Private Type Definitions ---

  ! This type represents one "slot" or "bucket" in our map.
  type :: t_bucket
    private
    integer :: key = -1
    integer :: value = -1  
    integer :: population = -1    
    logical :: occupied = .false. ! Is this slot in use?
  end type t_bucket

  ! This is the main map type the user will interact with.
  type :: t_int_map
    private
    type(t_bucket), allocatable :: buckets(:)
    integer :: count = 0      ! Number of items currently stored
    integer :: capacity = 0   ! Max size of the 'buckets' array

    contains
    ! Resize is an internal, type-bound procedure
      procedure :: resize_internal  
  end type t_int_map


   type :: Agent
      integer :: id = -1                                      ! characteristics of the agents
      real(8) :: pos_x = - 1000                             ! x position of the agent
      real(8) :: pos_y = - 1000                             ! y position of the agent
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
      class(dummy_grid), pointer :: grid => null()         
      ! the hashmap
      type(t_int_map), pointer :: index_map => null()
      ! the agent array
      type(Agent), pointer :: agent_array => null()


      ! Refferences to other agents:
      integer :: father_of_unborn_child = -1


     !The following will be replaced by ids.

      !type(Node), pointer :: father => null()             ! pointer to the father
      !type(Node), pointer :: mother => null()             ! pointer to the mother


      !type(pointer_node), pointer :: children => null()   ! self is supposed to be the head of a pointer list or null
      !type(pointer_node), pointer :: siblings => null()   ! self is supposed to be the head of a pointer list or null

      

    contains



  end type Agent




contains



  ! ==========================================================================
  ! Agent type bound procedures
  ! ==========================================================================

    subroutine agent_dies(agent_ptr)
      implicit none
      type(Agent), pointer, intent(inout) :: agent_ptr

      type(t_int_map), pointer :: index_map


      if (agent_ptr%is_dead) then
          print *, "Warning: Attempting to kill an agent that is already dead!"
          return
      end if

      index_map => agent_ptr%index_map

      if (.not. associated(index_map)) then
          print *, "Warning: Attempting to die an agent with no index map associated!"
          return
      end if


      if (.false. .eqv. contains_key(index_map,agent_ptr%id)) then
          print *, "Warning: Attempting to die an agent that is not in index map!"
          return
      end if

      if (agent_ptr%population < 1) then
        print*, "Warning: Unvalid population of agent to be removed."
      endif



      agent_ptr%is_dead = .true.


    end subroutine agent_dies




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


  ! ===========================================================================
  ! Hashmap Public Procedures
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Initializes a map. If called on an existing map, it clears it.
  ! ---------------------------------------------------------------------------
  subroutine init_map(this, initial_size)
    class(t_int_map), intent(inout) :: this
    integer, intent(in), optional :: initial_size
    integer :: size_to_alloc
    
    size_to_alloc = initial_hashmap_size
    if (present(initial_size)) then
      size_to_alloc = initial_size
    end if
    
    ! If map is already allocated, destroy it first
    if (allocated(this%buckets)) call destroy_map(this)
    
    this%capacity = size_to_alloc
    this%count = 0
    allocate(this%buckets(this%capacity))
    
  end subroutine init_map
  
  ! ---------------------------------------------------------------------------
  ! Frees all memory associated with the map and its values.
  ! ---------------------------------------------------------------------------
  subroutine destroy_map(this)
    class(t_int_map), intent(inout) :: this
    integer :: i
    
    if (.not. allocated(this%buckets)) return
    

    ! Deallocate the bucket array itself
    deallocate(this%buckets)
    this%capacity = 0
    this%count = 0
  end subroutine destroy_map

  ! ---------------------------------------------------------------------------
  ! Adds a new (key, value) if key doesnt exists.
  ! ---------------------------------------------------------------------------
  subroutine put(this, key, population, value)
    class(t_int_map), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: population
    integer, intent(in) :: value ! replace `class(*)` with desired type
    integer :: index
    !0. Check if population valid
    if (population < 1) then
      print*, "Warning: Putting entry in hashmap with population = 1."
    endif


    ! 1. Initialize if this is the first `put`
    if (this%capacity == 0) call init_map(this)
    
    ! 2. Check if we need to grow the array 
    if (this%count + 1 > int(this%capacity * MAX_LOAD_FACTOR)) then
      call this%resize_internal()  ! <-- FIX: Renamed from _resize
    end if
    
    ! 3. Find the correct slot for this key
    index = find_slot(this, key)
    
    ! 4. Insert
    if (this%buckets(index)%occupied) then
      ! Key already exists
      if (this%buckets(index)%key == key) then
        print*, "Error: index hashmap: Key already exists in put."
        return
      endif

      print*, "Error: attemps to overwrite existing key ??."
      
    else
      ! New entry
      this%buckets(index)%occupied = .true.
      this%buckets(index)%key = key
      this%buckets(index)%value = value
      this%buckets(index)%population = population
      this%count = this%count + 1
    end if
  end subroutine put

  ! ---------------------------------------------------------------------------
  ! Updates the value if the key exists.
  ! ---------------------------------------------------------------------------
  subroutine update(this, key, value)
    class(t_int_map), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: value ! replace `class(*)` with desired type
    integer :: index
    


    ! 1. Find the correct slot for this key
    index = find_slot(this, key)
    
    ! 2. update
    if (.false. .eqv. this%buckets(index)%occupied) then
      ! Key doesnt exist
      print*, "Error: index hashmap: Key does not exist in update."
      return
    else
      this%buckets(index)%value = value
    end if
  end subroutine update

  ! ---------------------------------------------------------------------------
  ! Removes a key-value pair from the map.
  ! ---------------------------------------------------------------------------
  subroutine remove(this, key)
    class(t_int_map), intent(inout) :: this
    integer, intent(in) :: key
    
    integer :: index, next_index, buckets_size, index_hole
    logical :: can_move
    integer :: temp_value, temp_key, next_hash
    
    if (this%count == 0) return ! Map is empty, nothing to remove
    

    buckets_size = size(this%buckets)
    index = find_slot(this, key)

    if (.not. this%buckets(index)%occupied) then
      ! Nothing to remove
      print*, "Warning: Tried to remove key from hashmap that is not in hashmap."
      return
    endif

    if (this%buckets(index)%key /= key) then
      print*, "Warning: found sloth occupied but different key this should not happen."
      return
    endif

    ! clear the bucket 
    this%buckets(index)%occupied = .false.
    this%buckets(index)%value = -1
    this%buckets(index)%key = -1
    this%count = this%count - 1

    ! rehash following cluster 
    if (index == buckets_size) then
      print*, "Warning: index == capacity == buckets_soize"
      print*, " I am pretty sure this will create bugs and also this shouldnt happen."

    endif


    index_hole = index
    next_index = index_hole + 1
    can_move = .false.

    if (next_index > buckets_size) then
      ! reached end of internal array go to beginning
      next_index = mod(next_index,buckets_size)
    endif

    do while (this%buckets(next_index)%occupied)
      temp_key = this%buckets(next_index)%key
      temp_value = this%buckets(next_index)%value 

      next_hash = hash_function(this%buckets(next_index)%key,this%capacity)

      can_move = .true.


      ! it can not move if its chain started after the index hole.
      if (index_hole < next_index) then
        if (next_hash > index_hole .and. next_hash <= next_index) then
          can_move = .false.
        endif
      else
        if (next_hash > index_hole .or. next_hash <= next_index) then
          can_move = .false.
        endif

      endif


      if (can_move) then
        ! make next_index_ new hole. 
        this%buckets(next_index)%occupied = .false.
        this%buckets(next_index)%key = -1
        this%buckets(next_index)%value = -1

        ! move item to hole
        if (this%buckets(index_hole)%occupied) then
          print*, "Warning: in rehashing the hole is not free. This shouldnht happen."
        endif

        this%buckets(index_hole)%key = temp_key
        this%buckets(index_hole)%value = temp_value
        this%buckets(index_hole)%occupied = .true.

        !update hole index:

        index_hole = next_index
      endif 

      next_index = next_index + 1



      if (next_index > buckets_size) then
          ! rehash following cluster 
        !print*, "Warning: next_index == capacity == buckets_size"
        !print*, " I am pretty sure this will create bugs and also this shouldnt happen."
        next_index = 1
      endif

      if (next_index == index) then
        print*, "Warning: Went all around once when rehashing i think this shouldnt happen. "
      endif
      

    end do
      



  end subroutine remove
  ! ---------------------------------------------------------------------------
  ! Gets a value by its key. Returns an unallocated `class(*)` if not found.
  ! ---------------------------------------------------------------------------
  function get(this, key) result(value)
    class(t_int_map), intent(in) :: this
    integer, intent(in) :: key
    integer :: value
    
    integer :: index

    
    if (this%count == 0) return ! Map is empty, return unallocated
    
    index = find_slot(this, key)
    
    ! Check if the slot is occupied AND the key matches
    if (this%buckets(index)%occupied .and. this%buckets(index)%key == key) then
      ! Found it. Allocate the result and copy the value.
      value = this%buckets(index)%value
    end if
    ! If not found, 'value' remains unallocated
  end function get

  ! ---------------------------------------------------------------------------
  ! Gets index and population by its key. Returns an unallocated `class(*)` if not found.
  ! For combined use of hashmap and agent matrix (with population dimension)
  ! ---------------------------------------------------------------------------
  subroutine get_index_and_pop(this, key, position, population) 
    class(t_int_map), intent(in) :: this
    integer, intent(in) :: key
    integer, intent(out) :: position, population
    
    integer :: index

    
    if (this%count == 0) return ! Map is empty, return unallocated
    
    index = find_slot(this, key)
    
    ! Check if the slot is occupied AND the key matches
    if (this%buckets(index)%occupied .and. this%buckets(index)%key == key) then
      ! Found it. Allocate the result and copy the value.
      position = this%buckets(index)%value
      population = this%buckets(index)%population
    end if
    ! If not found, 'value' remains unallocated
  end subroutine get_index_and_pop

  ! ---------------------------------------------------------------------------
  ! Checks if a key exists in the map.
  ! ---------------------------------------------------------------------------
  function contains_key(this, key) result(exists)
    class(t_int_map), intent(in) :: this
    integer, intent(in) :: key
    logical :: exists
    
    integer :: index
    
    if (this%count == 0) then
      exists = .false.
      return
    end if
    
    index = find_slot(this, key)
    
    ! Key exists only if the slot is occupied AND the key matches
    exists = (this%buckets(index)%occupied .and. this%buckets(index)%key == key)
  end function contains_key

  ! ---------------------------------------------------------------------------
  ! Returns the current number of items in the map.
  ! ---------------------------------------------------------------------------
  pure function get_size(this) result(count)
    class(t_int_map), intent(in) :: this
    integer :: count
    count = this%count
  end function get_size


  ! ---------------------------------------------------------------------------
  ! Returns the current capacity of the map.
  ! ---------------------------------------------------------------------------
  pure function get_capacity(this) result(capa)
    class(t_int_map), intent(in) :: this
    integer :: capa
    capa = this%capacity
  end function get_capacity


  ! ===========================================================================
  ! Hashmap internal (Private) Procedures
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Finds the correct index for a key.
  !
  ! Returns:
  ! - The index of the key, if it exists.
  ! - The index of the *first empty slot* to use, if the key doesn't exist.
  ! ---------------------------------------------------------------------------
  function find_slot(this, key) result(index)
    class(t_int_map), intent(in) :: this
    integer, intent(in) :: key
    integer :: index
    
    integer :: hash
    integer :: i
    
    ! Simple hash function: (key mod capacity) + 1 (for 1-based index of fortran)
    hash = hash_function(key, this%capacity)
    
    ! Linear probing loop
    do i = 0, this%capacity - 1
      index = mod(hash + i - 1, this%capacity) + 1
      
      ! If slot is empty, this is where the key *should* be.
      if (.not. this%buckets(index)%occupied) then
        return
      end if
      
      ! If slot is occupied, check if it's the key we want.
      if (this%buckets(index)%key == key) then
        return
      end if
      
      ! If not, continue probing...
    end do
    
    ! This should be impossible if resizing works correctly.
    stop "ERROR (int_hash_map_module): Map is full and no slot found."
  end function find_slot

  ! ---------------------------------------------------------------------------
  ! Internal routine to grow the map (doubles capacity) and re-hash all items.
  ! This is a type-bound procedure.
  ! ---------------------------------------------------------------------------
  subroutine resize_internal(this)  
    class(t_int_map), intent(inout) :: this
    
    integer :: new_capacity
    type(t_bucket), allocatable :: old_buckets(:)
    type(t_bucket), allocatable :: new_buckets(:)
    integer :: i
    
    ! Fulfill the "double" requirement
    new_capacity = this%capacity * hashmap_resize_factor
    if (new_capacity == 0) new_capacity = initial_hashmap_size
    
    ! print *, "Resizing map from ", this%capacity, " to ", new_capacity
      ! print *, "Resizing map from ", this%capacity, " to ", new_capacity
    
    ! 1. Allocate new table
    allocate(new_buckets(new_capacity))
    
    ! 2. Swap old and new using move_alloc. 
    call move_alloc(from=this%buckets, to=old_buckets)
    call move_alloc(from=new_buckets, to=this%buckets)
    
    this%capacity = new_capacity
    this%count = 0 ! `put` will increment this back up
    
    ! 3. Re-hash all items from the old table into the new one
    do i = 1, size(old_buckets)
      if (old_buckets(i)%occupied) then
        ! Call the public `put` to re-hash the item correctly
        call put(this, old_buckets(i)%key, old_buckets(i)%population, old_buckets(i)%value)

      end if
    end do
    
    ! 4. Free the old bucket array
    deallocate(old_buckets)
    
  end subroutine resize_internal 

  function hash_function(key, capacity) result(hash)
    integer, intent(in) :: key
    integer, intent(in) :: capacity
    integer :: hash

    hash = mod(abs(key), capacity) + 1

  end function hash_function 

end module mod_agent_hashmap