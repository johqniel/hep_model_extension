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
module mod_hashmap

  use mod_agent_class ! for dummy grid

  implicit none
  private

  ! --- Public Types and Procedures ---
  public :: t_int_map
  public :: init_map, destroy_map, put, get, contains_key, get_size

  ! --- Parameters ---
  integer, parameter :: DEFAULT_INITIAL_SIZE = 8
  
  ! We resize when the map is 75% full.
  real, parameter :: MAX_LOAD_FACTOR = 0.75

  ! --- Private Type Definitions ---

  ! This type represents one "slot" or "bucket" in our map.
  type :: t_bucket
    private
    integer :: key
    integer :: value         
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
      integer :: position_in_array                       ! position in the agents_array, used for quick access    
      integer :: number_of_children = 0
      logical :: is_dead = .false.
      logical :: recently_moved = .false.
      integer :: is_pregnant = 0                            ! 0 = not pregnant, n>0 pregnant for n ticks




      ! Position in the arrays used for computation
      integer :: position_population = -1         ! in old code often indexed by the var: jp
      integer :: position_human = -1             ! in old code often indexed by the var: i    
      integer :: hum_id                      ! agent id in the old code    
      !
      class(dummy_grid), pointer :: grid => null()         ! pointer to the grid the agent is currently in

    
      type(Node), pointer :: self_ => null()

     !The following will be replaced by ids.

      !type(Node), pointer :: father_of_unborn_child => null() 

      !type(Node), pointer :: father => null()             ! pointer to the father
      !type(Node), pointer :: mother => null()             ! pointer to the mother


      !type(pointer_node), pointer :: children => null()   ! self is supposed to be the head of a pointer list or null
      !type(pointer_node), pointer :: siblings => null()   ! self is supposed to be the head of a pointer list or null

      

    contains



  end type Agent

contains

  ! ===========================================================================
  !
  ! ===========================================================================


  subroutine resize_agent_array_hash(agents,new_size)
    type(Agent), allocatable, dimension(:), intent(inout) :: agents
    integer, intent(in) :: new_size

    type(Agent), allocatable :: new_agents(:)
    integer :: old_size

    old_size = size(agents)
    if (new_size <= old_size) then
        print *, "Error: New size must be greater than old size in resize_agents_array."
        stop
    end if

    allocate(new_agents(new_size))

    new_agents(1:old_size) = agents(1:old_size)

    deallocate(agents)

    call move_alloc(from=new_agents, to=agents)


  end subroutine resize_agent_array_hash

  subroutine compact_agents(agents, index_map ,dead_agents, num_agents)
    type(Agent), allocatable, dimension(:), intent(inout) :: agents
    type(t_int_map), intent(inout) :: index_map
    integer, intent(in) :: dead_agents
    integer, intent(in) :: num_agents

    integer, allocatable :: free_indeces(:)
    integer, allocatable :: agents_to_move(:)


    integer :: i, j ,n_agents
    logical :: found = .false.


    ! find indeces of dead agents
    n_agents = size(agents)
    j = 1
    do i = 1, n_agents
        if (agents(i)%is_dead) then
            free_indeces(j) = i
            j = j + 1
        end if
    end do

    ! Find agents to move (last #dead_agents alive agents in array)

    j = 0
    do i = 1, dead_agents

      found = .false.

      do while (found .eqv. .false.)

        if (agents(num_agents - j)%is_dead .eqv. .false.) then
            agents_to_move(i) = num_agents - j 
            found = .true.
            j = j + 1
        else
          j = j + 1
        endif

      end do

    end do

    do i = 1, dead_agents

      agents(free_indeces(i)) = agents(agents_to_move(i))
      call put(index_map, agents(free_indeces(i))%id , free_indeces(i))

    end do

  end subroutine compact_agents




  ! ===========================================================================
  ! 
  ! ===========================================================================


  function get_agent(id, id_map, agents) result(agent_ptr)
    integer, intent(in) :: id
    type(t_int_map), intent(in) :: id_map
    type(Agent), dimension(:), target, intent(in) :: agents


    type(Agent), pointer :: agent_ptr
    integer :: agent_index

    agent_index =  get(id_map, id)

    agent_ptr => agents(agent_index)

  end function get_agent


  ! ===========================================================================
  ! Public Procedures
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Initializes a map. If called on an existing map, it clears it.
  ! ---------------------------------------------------------------------------
  subroutine init_map(this, initial_size)
    class(t_int_map), intent(inout) :: this
    integer, intent(in), optional :: initial_size
    integer :: size_to_alloc
    
    size_to_alloc = DEFAULT_INITIAL_SIZE
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
  ! Adds a new (key, value) pair or updates the value if the key exists.
  ! ---------------------------------------------------------------------------
  subroutine put(this, key, value)
    class(t_int_map), intent(inout) :: this
    integer, intent(in) :: key
    integer, intent(in) :: value ! replace `class(*)` with desired type
    integer :: index
    
    ! 1. Initialize if this is the first `put`
    if (this%capacity == 0) call init_map(this)
    
    ! 2. Check if we need to grow the array 
    if (this%count + 1 > int(this%capacity * MAX_LOAD_FACTOR)) then
      call this%resize_internal()  ! <-- FIX: Renamed from _resize
    end if
    
    ! 3. Find the correct slot for this key
    index = find_slot(this, key)
    
    ! 4. Insert or update
    if (this%buckets(index)%occupied) then
      ! Key already exists, just update the value
      this%buckets(index)%value = value
    else
      ! New entry
      this%buckets(index)%occupied = .true.
      this%buckets(index)%key = key
      this%buckets(index)%value = value
      this%count = this%count + 1
    end if
  end subroutine put

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

  ! ===========================================================================
  ! Internal (Private) Procedures
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
    hash = mod(abs(key), this%capacity) + 1
    
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
  subroutine resize_internal(this)  ! <-- FIX: Renamed from _resize
    class(t_int_map), intent(inout) :: this
    
    integer :: new_capacity
    type(t_bucket), allocatable :: old_buckets(:)
    type(t_bucket), allocatable :: new_buckets(:)
    integer :: i
    
    ! Fulfill the "double" requirement
    new_capacity = this%capacity * 2
    if (new_capacity == 0) new_capacity = DEFAULT_INITIAL_SIZE
    
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
        call put(this, old_buckets(i)%key, old_buckets(i)%value)
      end if
    end do
    
    ! 4. Free the old bucket array
    deallocate(old_buckets)
    
  end subroutine resize_internal  ! <-- FIX: Renamed from _resize

end module mod_hashmap