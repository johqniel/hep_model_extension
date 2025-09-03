module mod_agent_class 

  !use mod_agent_procedures
  use mod_calculations

  implicit none


!=======================================================================
! Module: mod_agent_class
!
! Data-structures: 

!   - Node: The double linked list of agents. First and last agents are: 
!           - head_agents and tail_agents. For list of alive agents.
!           - head_dead_agents and tail_dead_agents. For list of dead agents.
!     
!
!   - pointer_node: A data type that encapsulates a pointer to a Node
!       - can be a double linked list or a array.
!       - used as double linked list to connect parents and children

!   - agents_array: Array of type pointer_node that contains pointers
!                   to all agents that are alive
!  
!   - dead_agents_array: Array of type pointer_node that contains pointers
!                        to all agents that are dead
!
!   - population_agents_matrix: 2-D Array of type pointer_node that contains 
!                              pointers to all agents alive. For each 
!                              population ther is a column.
!
! Functions for external use: 
!
!=======================================================================



!=======================================================================
! Type: dummy_grid
!   dummy type to avoid circular dependencies of modules
!   Is extended by type "spatial_grid" in mod_grid.f95
!=======================================================================

type, abstract :: dummy_grid

  contains
      procedure(remove_agent_from_grid_procedure), deferred :: remove_agent_from_grid
end type dummy_grid

!=======================================================================
! Type: Node
!   Represents on agent. Agents are linked in a double linked list.
!
! Details:
!   They are linked to their parents and children.
!   Also they are stored in varios arrays of pointers to access them quickly.
!
! Notes:
!   pos_x and pos_y are calculated and stored in matrix -> remove?.
!   -> Then we have to adjust agent born function.
!=======================================================================
  type :: Node
      integer :: id = -1                                      ! characteristics of the agents
      real(8) :: pos_x = - 1000                             ! x position of the agent
      real(8) :: pos_y = - 1000                             ! y position of the agent
      real(8) :: ux                                         ! x velocity of the agent   
      real(8) :: uy                                         ! y velocity of the agent 
      character(len=1):: gender = "F"      
      integer :: age                                        ! age of the agent in ticks
      integer :: position_in_array                       ! position in the agents_array, used for quick access    
      integer :: number_of_children = 0
      logical :: is_dead = .false.
      integer :: is_pregnant = 0                            ! 0 = not pregnant, n>0 pregnant for n ticks



      ! Position in the arrays used for computation
      integer :: position_population         ! in old code often indexed by the var: jp
      integer :: position_human              ! in old code often indexed by the var: i    
      integer :: hum_id                      ! agent id in the old code    
      !
      class(dummy_grid), pointer :: grid => null()         ! pointer to the grid the agent is currently in

      type(Node), pointer :: father_of_unborn_child => null() 

      type(Node), pointer :: next => null()               ! agents themselves are linked in a double linked list
      type(Node), pointer :: prev => null()

      type(Node), pointer :: father => null()             ! pointer to the father
      type(Node), pointer :: mother => null()             ! pointer to the mother

      type(Node), pointer :: self_ => null()

      type(pointer_node), pointer :: children => null()   ! self is supposed to be the head of a pointer list or null
      type(pointer_node), pointer :: siblings => null()   ! self is supposed to be the head of a pointer list or null

      

    contains


      procedure :: add_child 
      procedure :: remove_child 
      procedure :: search_child 
      procedure :: agent_die ! moves agent to the dead agents list




  end type Node




  type :: pointer_node
    type(Node), pointer :: node => null()
    type(pointer_node), pointer :: next => null()
    type(pointer_node), pointer :: prev => null()
  end type pointer_node

  type(Node), pointer :: head_agents => null()
  type(Node), pointer :: tail_agents => null()

  type(Node), pointer :: head_dead_agents => null()
  type(Node), pointer :: tail_dead_agents => null()


  type(pointer_node), allocatable :: agents_array(:)
  type(pointer_node), allocatable :: dead_agents_array(:)

  type(pointer_node), allocatable :: population_agents_matrix(:,:)
  type(pointer_node), allocatable :: population_agents_matrix0(:,:)

  type(Node), pointer :: agentzero => null()
  integer :: number_of_agents = 0
  integer :: max_agents = 100 
  integer :: max_dead_agents = 100
  integer :: number_of_dead_agents = 0
  integer :: number_of_agents_all_time = 0



abstract interface
    subroutine remove_agent_from_grid_procedure(self, agent)
        import :: Node, dummy_grid
        class(dummy_grid), intent(inout) :: self
        type(Node), pointer, intent(inout) :: agent
    end subroutine remove_agent_from_grid_procedure
end interface


contains





!========================================================================
!========================================================================
!============ subroutines that manage the agent_pointer_lists ===========
!========================================================================
!========================================================================



  !=======================================================================
  ! FUNCTION: append_ptr_node
  ! appends a pointer_node to a pointer_node list
  !
  ! Arguments:
  !   The agent the pointer_node should point to
  !   The head of the pointer_node list the pointer_node should be inserted into
  !
  !
  !
  ! Notes:
  !   
  !=======================================================================
  subroutine append_ptr_node_old(agent_ptr, ptr_node_head)
    type(Node), pointer, intent(in) :: agent_ptr
    type(pointer_node), pointer, intent(inout) :: ptr_node_head
    type(pointer_node), pointer :: new_node

    if (.not. associated(ptr_node_head)) then
      print*, "ERROR: Trying to append ptr_node to un-associated head. Instead we now allocate and initilize head."
      allocate(new_node)
      new_node%node => agent_ptr
      new_node%next => null()
      new_node%prev => null()
      ptr_node_head => new_node
      return
    endif

    allocate(new_node)
    new_node%node => agent_ptr
    new_node%prev => ptr_node_head
    new_node%next => ptr_node_head%next
    if (associated(ptr_node_head%next)) then
      ptr_node_head%next%prev => new_node
    end if
    ptr_node_head%next => new_node
    
  end subroutine append_ptr_node_old

  !=======================================================================
  ! FUNCTION: remove_ptr_node
  ! Removes a pointer_node from a pointer_node list.
  !
  ! Arguments:
  !   ptr_node  [POINTER] - The pointer_node to be removed
  !
  ! Notes: 
  !   can not remove the head of the list. -> before calling check if 
  !   you are removing the last element. 
  !=======================================================================
  subroutine remove_ptr_node_old(head_ptr_node,ptr_node)
    type(pointer_node), pointer, intent(inout) :: ptr_node
    type(pointer_node), pointer, intent(in) :: head_ptr_node

    if (.not. associated(ptr_node)) then
      print*, "Error: mod_agent_class, removing pointer node that doesnt exist."
  
      return
    endif

    if (associated(head_ptr_node,ptr_node)) then
      print*, "Error: Trying to remove the head of a pointer list with remove_ptr_node:"
      return
    endif

    if (.not. associated(head_ptr_node)) then
      print*, "Trying to remove pointer node from empty pointer node list."
      return
    endif

    if (associated(ptr_node%prev)) then
      ptr_node%prev%next => ptr_node%next
    end if 

    if (associated(ptr_node%next)) then
      ptr_node%next%prev => ptr_node%prev
    end if

    deallocate(ptr_node)
    !ptr_node => null()
  end subroutine remove_ptr_node_old

  !=======================================================================
  ! FUNCTION: clear_ptr_list
  ! Clears a pointer_node list by deallocating all nodes.
  !
  ! Arguments:
  !   ptr_node [POINTER] - The head of the pointer_node list to be cleared
  !
  !=======================================================================
  subroutine clear_ptr_list(ptr_node)
    type(pointer_node), pointer :: ptr_node
    type(pointer_node), pointer :: temp

    temp => ptr_node
    do while (associated(temp))
      temp => temp%next
      deallocate(ptr_node)
      ptr_node => temp
    end do
  end subroutine clear_ptr_list


    !==============================================================
    subroutine append_pointer_node(head_pointer_node, agent)
        type(pointer_node), pointer, intent(inout) :: head_pointer_node
        type(Node), pointer, intent(in) :: agent
        type(pointer_node), pointer :: new_node, current


        !print*, "Append new pointer node"

        if (.not. associated(head_pointer_node)) then
            ! List is empty: allocate head and attach agent
            allocate(head_pointer_node)
            head_pointer_node%node => agent
            head_pointer_node%next => null()
            head_pointer_node%prev => null()
            return
        endif

        ! Traverse to the end of the list
        current => head_pointer_node
        do while (associated(current%next))
            current => current%next
        end do

        ! Allocate new node and link it
        allocate(new_node)
        new_node%node => agent
        current%next => new_node
        new_node%prev => current
        new_node%next => null()
    end subroutine append_pointer_node

    !==============================================================
    subroutine remove_pointer_node(head_pointer_node, agent)
        type(pointer_node), pointer, intent(inout) :: head_pointer_node
        type(Node), pointer, intent(in) :: agent
        type(pointer_node), pointer :: current, temp


        ! When this function is called, we need to manually check whether the head pointer is the last
        ! node in the list. If that is the case then we need to nullify it outside of this function. 
        !
        ! If you dont do that the memory the head pointer points to will be deallocated but the head pointer 
        ! itself will still point to that memory. 
        !
        ! So if this function causes trouble (segfaults) then this is likely the reason. 
        !
        !
        ! I dont now why i cant nullify the head pointer from this function if i pass it as intent(inout).
        !
        ! Okay apparently this is not the problem. I just check no before calling this function if the 
        ! head pointer is dangling. This check is positive once at the beginning of the simulation: 
        ! I still dont understand why this is the case. DN 06.08.

        !print*, "enter."

        current => head_pointer_node


        if (.not. associated(current)) then
            print *, "Error: Trying to remove pointer node from empty list."
            return
        endif


        if (.not. associated(agent)) then
            print *, "Error: Trying to remove a null agent from pointer node list."
            return  
        endif




        do while (associated(current))
            !print*, "Enter while."
            if (.not. associated(current%node)) then
                print *, "Error: Pointer node in list is not associated with a Node."
                cycle
            endif
            if (associated(current%node, agent)) then
                ! Found the node to remove
                if (associated(current%prev)) then
                    current%prev%next => current%next
                else
                    ! Removing the head
                    head_pointer_node => current%next
                endif
                if (associated(current%next)) then
                    current%next%prev => current%prev
                endif

                current%next => null()
                current%prev => null()
                current%node => null()
                deallocate(current)
                if (.not. associated(head_pointer_node)) then
                    !print*, "List is now empty, Head pointer nullified."
                    head_pointer_node => null()
                endif
                return
            endif
            current => current%next
        end do
    end subroutine remove_pointer_node

    !==============================================================
    logical function search_pointer_node(head_pointer_node, agent)
        type(pointer_node), pointer, intent(in) :: head_pointer_node
        type(Node), pointer, intent(in) :: agent
        type(pointer_node), pointer :: current

        search_pointer_node = .false.
        current => head_pointer_node
        do while (associated(current))
            if (associated(current%node, agent)) then
                search_pointer_node = .true.
                return
            endif
            current => current%next
        end do
    end function search_pointer_node







!========================================================================
!========================================================================
!===========  Functions that manage the agents_array ====================
!========================================================================
!========================================================================

  !=======================================================================
  ! FUNCTION: initilize_agents_array
  ! allocated the memory for the agents_array
  !=======================================================================
  subroutine initilize_agents_array()
      allocate(agents_array(max_agents))
  end subroutine initilize_agents_array

  !=======================================================================
  ! FUNCTION: remove_agent_from_array
  ! Removes an agent from the agents_array.
  !
  ! Arguments:
  !   agent_ptr [POINTER] - Pointer to the agent to be removed
  !
  ! Notes:
  !   To make sure that no holes with unassociated pointers are left in the array,
  !   the last agent in the array is moved to the position of the removed agent.
  !=======================================================================
  subroutine remove_agent_from_array(agent_ptr)
      type(Node), pointer :: agent_ptr

      integer :: position_to_remove

      if (.not. associated(agent_ptr)) then
          print *, "Error: agent_ptr to be removed from agents array is not associated!"
          return
      end if
      if (number_of_agents < 0) then
          print *, "Error: number of agents == 0 but wants me to remove agent!"
          return
      end if
      if (number_of_agents == 1) then
          print*, "Removing last agent from agents array!"
          deallocate(agents_array)
          number_of_agents = 0
          return
      end if
      if (number_of_agents > size(agents_array)) then
          print *, "Error: number of agents > size of agents array!" !should not happen at this point, leftover from debugging i guess
      end if
      
      position_to_remove = agent_ptr%position_in_array
      if (position_to_remove > size(agents_array)) then
          print *, "Error: position to remove is out of bounds!", position_to_remove 
          print *, "size: ", size(agents_array)
          print *, "number_of_agents: ", number_of_agents
          return
      end if

      ! agents_array management: moves last element to the position of the one that is being removed
      if (.not. associated(agents_array(position_to_remove)%node)) then
          print *, "Error: agents_array(position_to_remove) is not associated!" ! this will cause a seg fault later.
      end if
      agents_array(position_to_remove)%node => agents_array(number_of_agents)%node
      agents_array(position_to_remove)%node%position_in_array = position_to_remove

      agents_array(number_of_agents)%node => null() 
      ! temp is a null agent because => null() throws error?
      number_of_agents = number_of_agents - 1
      ! end of agents_array management

  end subroutine remove_agent_from_array

  !=======================================================================
  ! FUNCTION: add_agent_to_array
  ! adds an agent to the agents_array.
  !
  ! Arguments:
  !   agent_ptr [POINTER] - Pointer to the agent to be added
  !
  ! Notes:
  ! If the agents_array is full, it will be resized to accommodate more agents.
  !=======================================================================
  subroutine add_agent_to_array(agent_ptr)
      type(Node), pointer :: agent_ptr

      if (.not. associated(agent_ptr)) then
          print *, "Error: agent_ptr to be added to agents array is not associated!"
          return
      end if
        

      if ( .not. number_of_agents < max_agents) then
          call resize_agents_array()
          !print *, "Error: number of agents > size of agents array!"
          
      end if

      number_of_agents = number_of_agents + 1
      agents_array(number_of_agents)%node => agent_ptr
      agent_ptr%position_in_array = number_of_agents
  end subroutine add_agent_to_array

  !=======================================================================
  ! FUNCTION: resize_agents_array
  ! Resizes the agents_array to accommodate more agents.
  !
  !=======================================================================
  subroutine resize_agents_array()

      ! unfortunately i wasnt able to figure out how to do a array of pointers that we can expand 
      ! dynamically, so we have to decide in the beginning how many agents we want to have 
      ! at most at one point. Maybe we could trick fortran by using the node_ptr class. 
      ! but i havent tried that yet.
      type(pointer_node), allocatable :: temp(:)
      integer :: i
      if (.not. allocated(agents_array)) then
        print *, "Error: Agent array not allocated!"
        error stop
      end if

      if (.not. size(agents_array) == max_agents) then
        print *, "Error: Agent array size does not match the desired size!"
        error stop
      end if
    
      max_agents = max_agents * 2
      if (max_agents == 0) then
        print *, "Error: max agents == 0?"

        max_agents = 1
      end if

      allocate(temp(max_agents))
      if (allocated(agents_array)) then
          do i = 1, number_of_agents
              if (.not. associated(agents_array(i)%node)) then
                  print *, "Error: agents_array(i) is not associated!" ! this will cause a seg fault later.
              else
                temp(i)%node => agents_array(i)%node
              end if
          end do
          deallocate(agents_array)
      else
          print *, "Error: agents_array not allocated!"
          error stop
      end if

    

      allocate(agents_array(max_agents))
      do i = 1, number_of_agents
              if (.not. associated(temp(i)%node)) then
                  print *, "Error: temp(i) is not associated!" ! this will cause a seg fault later.
              else
                agents_array(i)%node => temp(i)%node
              end if
          end do
      deallocate(temp)


      ! resize other arrays. 

  end subroutine resize_agents_array




!========================================================================
!========================================================================
!===========  Functions that manage the dead_agents_array ===============
!========================================================================
!========================================================================

  !=======================================================================
  ! FUNCTION: initilize_dead_agents_array
  ! allocated the memory for the dead_agents_array
  !=======================================================================
  subroutine initilize_dead_agents_array()
      allocate(dead_agents_array(max_dead_agents))
  end subroutine initilize_dead_agents_array


  !=======================================================================
  ! FUNCTION: add_agent_to_array_of_dead
  ! adds an agent to the dead_agents_array.
  !
  ! Arguments:
  !   agent_ptr [POINTER] - Pointer to the agent to be added
  !
  ! Notes:
  ! If the agents_array is full, it will be resized to accommodate more agents.
  !=======================================================================
  subroutine add_agent_to_array_of_dead(agent_ptr)
      type(Node), pointer :: agent_ptr

      if (.not. associated(agent_ptr)) then
          print *, "Error: agent_ptr to be added to dead agents array is not associated!"
          return
      end if
        

      if ( .not. number_of_dead_agents < max_dead_agents) then
          call resize_dead_agents_array()
          !print *, "Error: number of agents > size of agents array!"
          
      end if

      number_of_dead_agents = number_of_dead_agents + 1
      dead_agents_array(number_of_dead_agents)%node => agent_ptr
      agent_ptr%position_in_array = number_of_dead_agents
  end subroutine add_agent_to_array_of_dead
  !=======================================================================
  ! FUNCTION: resize_dead_agents_array
  ! Resizes the dead_agents_array to accommodate more agents.
  !
  !=======================================================================
  subroutine resize_dead_agents_array()

      ! unfortunately i wasnt able to figure out how to do a array of pointers that we can expand 
      ! dynamically, so we have to decide in the beginning how many agents we want to have 
      ! at most at one point. Maybe we could trick fortran by using the node_ptr class. 
      ! but i havent tried that yet.
      type(pointer_node), allocatable :: temp(:)
      integer :: i
      if (.not. allocated(dead_agents_array)) then
        print *, "Error: dead Agent array not allocated!"
        error stop
      end if

      if (.not. size(dead_agents_array) == max_dead_agents) then
        print *, "Error: dead Agent array size does not match the desired size!", max_dead_agents - size(dead_agents_array)
        print *, "above difference, max_dead_agents: ", max_dead_agents
        error stop
      end if
    
      max_dead_agents = max_dead_agents * 2

      if (max_dead_agents == 0) then
        print *, "Error: max dead agents == 0?"

        max_dead_agents = 1
      end if

      allocate(temp(max_dead_agents))
      if (allocated(dead_agents_array)) then
          do i = 1, number_of_dead_agents
              if (.not. associated(dead_agents_array(i)%node)) then
                  print *, "Error: dead_agents_array(i) is not associated!" ! this will cause a seg fault later.
              else
                temp(i)%node => dead_agents_array(i)%node
              end if
          end do
          deallocate(dead_agents_array)
      else
          print *, "Error: agents_array not allocated!"
          error stop
      end if

    

      allocate(dead_agents_array(max_dead_agents))
      do i = 1, number_of_dead_agents
              if (.not. associated(temp(i)%node)) then
                  print *, "Error: temp(i) is not associated!" ! this will cause a seg fault later.
              else
                dead_agents_array(i)%node => temp(i)%node
              end if
          end do
      deallocate(temp)
  end subroutine resize_dead_agents_array

!===========================================================================
!================= Utility Functions =======================================
!===========================================================================

  include "agent_utilities.inc"

  include "agent_ptr_utilities.inc"




!=======================================================================
!========================================================================
!===========  Functions that manage the agent list =====================
!========================================================================
!=======================================================================

  !=======================================================================
  !==================== First type bound procedures ======================
  !=======================================================================

    include "agent_procedures.inc"



  !=======================================================================
  !============ Global callable functions for agents management ============
  !=======================================================================

    !=======================================================================
    ! FUNCTION: initialize_agents
    ! Initializes the head and tail of the agents linked list.
    !
    ! Notes:
    !   - This function should be called before any agents are added.
    !   - It sets up the head and tail nodes of the linked list.
    !   - The head node has id 0 and points to the tail node.
    !   - The tail node has id -1 and points to null.
    !========================================================================
    subroutine initialize_agents()
      allocate(head_agents)
      allocate(tail_agents)

    
      head_agents%id = 0
      head_agents%next => tail_agents
      head_agents%prev => null()

      tail_agents%id = -1
      tail_agents%next => null()
      tail_agents%prev => head_agents
    end subroutine initialize_agents

    !=======================================================================
    ! FUNCTION: append
    ! Appends a new agent to the end of the agents linked list.
    !
    ! Arguments:
    !   agent_id [INTEGER] - The ID of the agent to be added
    !
    ! Notes:
    !   - This function allocates a new Node for the agent and links it to the tail of the list.
    !   - It is not intended to be called directly; 
    !     instead, use the agent_born function to create new agents.
    !========================================================================
    subroutine append(agent_id)
      integer, intent(in) :: agent_id
      type(Node), pointer :: new_node

      allocate(new_node)
      new_node%id = agent_id
      new_node%next => null()
      new_node%prev => tail_agents
      new_node%self_ => new_node

      if (associated(tail_agents)) then
        tail_agents%next => new_node
      else
        head_agents => new_node
      end if

      tail_agents => new_node

      ! print *, "appending agent with id: ", agent_id   ! For Debugging purposes DN 10.06.2025
      !manage the agents array
      !if (.not. number_of_agents < max_agents) then
        !print *, "reached macimum number of agents in the array"
        !call resize_agents_array()
      !end if     
      ! this is done in the add_agent_to_array function
      call add_agent_to_array(new_node)
    end subroutine append

    !=======================================================================
    ! FUNCTION: remove
    ! Removes an agent from the agents linked list.
    !
    ! Arguments:
    !   agent_ptr [TYPE(Node), POINTER] - Pointer to the agent to be removed
    !
    ! Notes:
    !   - This function deallocates the agent node and updates the linked list pointers.
    !   - It can probably be deleted and should not be called directly. 
    !     Instead, use the agent_die function to remove agents.
    !========================================================================
    subroutine remove(agent_ptr)
      type(Node), pointer :: agent_ptr

      if (.not. associated(agent_ptr)) then
        print *, "Error: agent_ptr to be removed is not associated!"
        return
      end if

      if (associated(agent_ptr%prev)) then
        agent_ptr%prev%next => agent_ptr%next
      else
        head_agents => agent_ptr%next
      end if

      if (associated(agent_ptr%next)) then
        agent_ptr%next%prev => agent_ptr%prev
      else
        tail_agents => agent_ptr%prev
      end if

      ! agent array management:
      call remove_agent_from_array(agent_ptr)
      if (associated(agent_ptr)) then
        deallocate(agent_ptr)
      end if
    end subroutine remove


    !=======================================================================
    ! FUNCTION: clear_list
    ! Clears the agents linked list and deallocates all nodes.
    !
    ! Notes:
    !   - This function should be called when the simulation ends or when you want to reset the agent list.
    !   - It deallocates all nodes in the linked list and clears the agents_array.
    !   - It also deallocates the agents_array.
    !========================================================================
    subroutine clear_list()
      type(Node), pointer :: current, temp
      current => head_agents
      do while (associated(current))
        temp => current%next
        deallocate(current)
        current => temp
        deallocate(temp)
      end do
      head_agents => null()
      tail_agents => null()

      ! manage the agents array
      deallocate(agents_array)

    end subroutine clear_list

    !=======================================================================
    ! FUNCTION: agent_born
    ! Creates a new agent as a child of the given parents.
    !
    ! Arguments:
    !   father_ptr [TYPE(Node), POINTER] - Pointer to the father agent
    !   mother_ptr [TYPE(Node), POINTER] - Pointer to the mother agent
    !
    ! Notes:
    !   - This function creates a new agent at the average position of the parents.
    !   - atm it spawns the agent at the origin if both parents are not associated.
    ! TODO:
    !   - Add more sophisticated logic for agent birth, such as genetic traits or random mutations.
    !   - Deal different with the case where one parent is not associated.
    !   - Make sure that parents are associated in the first place :)
    !========================================================================
    subroutine agent_born(father_ptr, mother_ptr)
      integer :: agent_id
      !integer :: agent_id
      type(Node), pointer :: father_ptr
      type(Node), pointer :: mother_ptr
      real(8) :: pos_x, pos_y
      real :: r

      if (.not. associated(father_ptr)) then
        print *, "Error: father_ptr is not associated!"
        if (.not. associated(mother_ptr)) then
          print *, "Error: mother_ptr is not associated!"
          call agent_spawn(0.0d0, 0.0d0) ! spawn a new agent at the origin if father or mother is not associated
          return
        else
          call agent_spawn(mother_ptr%pos_x, mother_ptr%pos_y) ! spawn a new agent at the mother's position if father is not associated
          return
        endif
      end if
    
      if (.not. associated(mother_ptr)) then
        print *, "Error: motjher_ptr is not associated!"
        if (.not. associated(father_ptr)) then
          print *, "Error: father_ptr is not associated!"
          call agent_spawn(0.0d0, 0.0d0) ! spawn a new agent at the origin if father or mother is not associated
          return
        else
          call agent_spawn(father_ptr%pos_x, father_ptr%pos_y) ! spawn a new agent at the father's position if father is not associated
          return
        endif
      end if

      
      pos_x = father_ptr%pos_x + (mother_ptr%pos_x - father_ptr%pos_x) * 0.5
      pos_y = father_ptr%pos_y + (mother_ptr%pos_y - father_ptr%pos_y) * 0.5
      
      agent_id = get_agent_id()

      call append(agent_id)

      
      tail_agents%pos_x = pos_x
      tail_agents%pos_y = pos_y
      tail_agents%age = 0
      tail_agents%father => father_ptr
      tail_agents%mother => mother_ptr
      tail_agents%children => null()
      tail_agents%siblings => null()

      
      

      ! ################# Gender ############################################

      call random_number(r)
      if (r < 0.5) then
        tail_agents%gender = 'M'
      else
        tail_agents%gender = 'F'
      end if

      ! #########    add new agent to child list of father and mother ########

      call father_ptr%add_child(tail_agents)
      call mother_ptr%add_child(tail_agents)

      ! ################## Gene propagation ####################################

      ! call get_genes(father)
      ! call get_genes(mother)

      ! call gene_model()
      
      !self.genes = new_genes()
      
    end subroutine agent_born

    !===========================================================================
    ! FUNCTION: agent_spawn
    ! Like agent born but without involvment of father and mother. 
    !===========================================================================
    subroutine agent_spawn(pos_x,pos_y)
      real(8), intent(in) :: pos_x, pos_y
      integer :: agent_id 
      real :: r

      agent_id = get_agent_id()
      call append(agent_id)

      tail_agents%pos_x = pos_x
      tail_agents%pos_y = pos_y
      tail_agents%age = 0
      tail_agents%father => null()
      tail_agents%mother => null()
      tail_agents%children => null()
      tail_agents%siblings => null()
    

      
      call random_number(r)
      if (r < 0.5) then
        tail_agents%gender = 'M'
      else
        tail_agents%gender = 'F'
      end if

      ! management of the agent array
      ! is already done in append function

      !call add_agent_to_array(tail_agents)

    end subroutine agent_spawn



!========================================================================
!========================================================================
!===========  Functions that manage the dead agents list ================
!========================================================================
!========================================================================
  
    !===================================================================
    ! FUNCTION: move_dead_agent(agent_ptr)
    ! Moves a agent from the list of alive agents to the list of dead agents
    !
    ! Arguments: agent_ptr [Type(Node), pointer] the agent to be moved
    ! 
    !
    !===================================================================
    subroutine move_dead_agent(agent_ptr)
      type(Node), pointer :: agent_ptr

      

      if (.not. associated(agent_ptr)) then
        print *, "Error: agent_ptr to be moved to stack of dead agents is not associated!"
        return
      end if
      if (associated(agent_ptr%prev)) then
        agent_ptr%prev%next => agent_ptr%next
      else
        head_agents => agent_ptr%next
      end if
      if (associated(agent_ptr%next)) then
        agent_ptr%next%prev => agent_ptr%prev
      else
        tail_agents => agent_ptr%prev
      end if
      

      agent_ptr%next => head_dead_agents
      agent_ptr%prev => null()
      head_dead_agents => agent_ptr

      agent_ptr%is_dead = .true.
      
      call add_agent_to_array_of_dead(agent_ptr)
    end subroutine move_dead_agent



end module mod_agent_class