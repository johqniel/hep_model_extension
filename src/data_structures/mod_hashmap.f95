module mod_hashmap

    use mod_config


    implicit none


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
        procedure :: init_map
    end type t_int_map

    contains

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

        
        if (this%count == 0) then
             position = -1
             population = -1
             return ! Map is empty, return unallocated
        endif
        
        index = find_slot(this, key)
        
        ! Check if the slot is occupied AND the key matches
        if (this%buckets(index)%occupied .and. this%buckets(index)%key == key) then
            ! Found it. Allocate the result and copy the value.
            position = this%buckets(index)%value
            population = this%buckets(index)%population
        else
            position = -1
            population = -1
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


end module mod_hashmap