module mod_agent_class 

  implicit none





! data type that represents the agents in the simulation

  type :: Node
    integer :: id                                      ! characteristics of the agents
    real :: pos_x                                      ! maybe we dont need this anymore 
    real :: pos_y                                      ! BECAUSE we have the position in the matrix
    character(len=1):: gender
    integer :: age
    integer :: position_in_array
    integer :: number_of_children = 0
    logical :: is_dead = .false.

    ! Position in the arrays used for computation
    integer :: position_population         ! in old code often indexed by the var: jp
    integer :: position_human              ! in old code often indexed by the var: i    
    integer :: hum_id                      ! agent id in the old code    
    !

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



! data type that lets us save refferences to the siblings and children of the agents dynamically

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

  type(pointer_node), allocatable :: population_agents_array(:,:)
  type(pointer_node), allocatable :: population_agents_array0(:,:)

  type(Node), pointer :: agentzero => null()
  integer :: number_of_agents = 0
  integer :: max_agents = 100 
  integer :: max_dead_agents = 100
  integer :: number_of_dead_agents = 0
  integer :: number_of_agents_all_time = 0




contains


! first all function used for the agent_pointer list

  subroutine append_ptr_node(agent_ptr, ptr_node_head)
    type(Node), pointer :: agent_ptr
    type(pointer_node), pointer :: ptr_node_head
    type(pointer_node), pointer :: new_node


    allocate(new_node)
    new_node%node => agent_ptr
    new_node%prev => ptr_node_head
    new_node%next => ptr_node_head%next
    if (associated(ptr_node_head%next)) then
      ptr_node_head%next%prev => new_node
    end if
    
  end subroutine append_ptr_node


  subroutine remove_ptr_node(ptr_node)
    type(pointer_node), pointer :: ptr_node

    if (.not. associated(ptr_node)) return

    if (associated(ptr_node%prev)) then
      ptr_node%prev%next => ptr_node%next
    end if 

    if (associated(ptr_node%next)) then
      ptr_node%next%prev => ptr_node%prev
    end if

    deallocate(ptr_node)
  end subroutine remove_ptr_node

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

! now functions that manage the agents_array

subroutine initilize_agents_array()
    allocate(agents_array(max_agents))
end subroutine initilize_agents_array

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

end subroutine resize_agents_array

! Now the functions to manage the array of dead agents

subroutine initilize_dead_agents_array()
    allocate(dead_agents_array(max_dead_agents))
end subroutine initilize_dead_agents_array

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


! now all function used for the agent list 

! type-bound-procedures for node thing


subroutine agent_die(self)
        class(Node), intent(inout), TARGET :: self
        type(pointer_node), pointer :: temp_child
        type(Node), pointer :: self_ptr

        !print *, "Entering agent_die subroutine." ! This should *always* print.            ! Debugging line DN 10.06.2025
        !print *, "Checking self object. ID: ", self%id ! <<< ADD THIS LINE                 ! Debugging line DN 10.06.2025
                                                ! If this crashes, 'self' is the issue.         

        !print *, "agent dies"  ! for debugging purposes DN 10.06.2025                      ! Debugging line DN 10.06.2025

      
        self_ptr => self
        !self_ptr => self%self_                                                             ! Debugging line DN 10.06.2025
        !print *, "something"
        !print *, "self_ is null: ", .not. associated(self_ptr)

        if (.not. associated(self_ptr)) then
            print *, "Error: self_ptr is not associated!"
            return
        end if
        !print *, "we get here"                                                              ! Debugging line DN 10.06.2025    
        ! agents_array management: moves last element to the position of the one that is being removed
        call remove_agent_from_array(self_ptr)
        ! end of agents_array management

        call move_dead_agent(self_ptr)
end subroutine agent_die

subroutine remove_parent_from_children(self)
  class(Node), intent(inout) :: self
  type(pointer_node), pointer :: temp_child
  

  temp_child => self%children
  do while (associated(temp_child))
            if (self%gender == 'M') then
                temp_child%node%father => null()
            else
                temp_child%node%mother => null()
            end if
            if(associated(temp_child%next)) then
                temp_child => temp_child%next
            else
                temp_child => null()
            end if


        end do
end subroutine remove_parent_from_children

function search_child(self, child_ptr) result(temp_child)
        class(Node), intent(in) :: self
        type(Node), pointer :: child_ptr
        integer :: id_to_find
        type(pointer_node), pointer :: temp_child
        logical :: found = .false.

        if (.not. associated(child_ptr)) then
            print *, "Error: child_ptr is not associated!"
            return
        end if

        id_to_find = child_ptr%id

        temp_child => null()
        if (associated(self%children)) then
        temp_child => self%children
        do while (associated(temp_child))
            if (temp_child%node%id == id_to_find) then
              found = .true.
              exit
            else
              temp_child => temp_child%next
            end if
        end do
        end if   

        if (.not. found) then
        temp_child => null()
        end if 
end function search_child


    subroutine remove_child(self, child_ptr)
        class(Node), intent(inout) :: self
        type(Node), pointer :: child_ptr
        type(pointer_node), pointer :: temp_child
        if (.not. associated(child_ptr)) then
          print *, "Attempting to remove a child that is not associated!"
          return
        end if
        if (.not. associated(self%children)) then
          if (self%number_of_children == 0) then
            print *, "Attempting to remove a child from a agent that has no children!"
          else
            print *, "Attempting to remove a child from a agent whose link to his children has been lost!"
          end if
          return
        end if

        if (child_ptr%id == self%children%node%id) then
          temp_child => self%children
          self%children => self%children%next
          call remove_ptr_node(temp_child)
        else 
          temp_child => self%search_child(child_ptr)
          if (.not. associated(temp_child)) then
            print *, "Attempting to remove a child that is not in the list of children of the parent!"
            return
          else
            call remove_ptr_node(temp_child) 
          end if    
        end if
    end subroutine remove_child



    subroutine add_child(self,child_ptr)
        class(Node), intent(inout) :: self
        type(Node), pointer :: child_ptr
        type(pointer_node), pointer :: new_node

        if (.not. associated(self%children)) then
          allocate(new_node)
          new_node%node => child_ptr
          new_node%next => self%children
          new_node%prev => null()
          self%children => new_node
          self%number_of_children = self%number_of_children + 1
        else
          call append_ptr_node(child_ptr, self%children)
          self%number_of_children = self%number_of_children + 1
        end if
    end subroutine add_child




! global callable functions


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

  subroutine move_agent(agent_ptr, new_x, new_y)
    type(Node), pointer :: agent_ptr
    real, intent(in) :: new_x, new_y

    if (.not. associated(agent_ptr)) return

    agent_ptr%pos_x = new_x
    agent_ptr%pos_y = new_y
  end subroutine move_agent

  subroutine agent_age(agent_ptr)
    type(Node), pointer :: agent_ptr

    if (.not. associated(agent_ptr)) return

    agent_ptr%age = agent_ptr%age + 1
  end subroutine agent_age

  subroutine agent_born(father_ptr, mother_ptr)
    integer :: agent_id
    !integer :: agent_id
    type(Node), pointer :: father_ptr
    type(Node), pointer :: mother_ptr
    real :: pos_x, pos_y
    real :: r

    if (.not. associated(father_ptr)) then
      print *, "Error: father_ptr is not associated!"
      if (.not. associated(mother_ptr)) then
        print *, "Error: mother_ptr is not associated!"
        call agent_spawn(0.0, 0.0) ! spawn a new agent at the origin if father or mother is not associated
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
        call agent_spawn(0.0, 0.0) ! spawn a new agent at the origin if father or mother is not associated
        return
      else
        call agent_spawn(father_ptr%pos_x, father_ptr%pos_y) ! spawn a new agent at the mother's position if father is not associated
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
    
    call random_number(r)
    if (r < 0.5) then
      tail_agents%gender = 'M'
    else
      tail_agents%gender = 'F'
    end if

    ! add new agent to child list of father and mother

    call father_ptr%add_child(tail_agents)
    call mother_ptr%add_child(tail_agents)

    ! management of the agent array
    ! is already done in append function 
    !call add_agent_to_array(tail_agents)
    
  end subroutine agent_born

  subroutine agent_spawn(pos_x,pos_y)
    real, intent(in) :: pos_x, pos_y
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


  function get_agent_id() result(id)
    integer :: id
    id = number_of_agents_all_time + 1
    number_of_agents_all_time = number_of_agents_all_time + 1
  end function get_agent_id

  ! Randomly selects two different agents from the list
  subroutine select_random_agents_distinct(agentOne, agentTwo)
    type(Node), pointer :: agentOne, agentTwo
    integer :: idx1, idx2
    real :: r
    type(Node), pointer :: temp_agent
    integer :: count

    count = number_of_agents ! number of agents in the array


      ! Check if there are enough agents to select two
    if (count < 2) then
       print *, "Not enough agents to select from. (select_distinct function)"
       agentOne => null()
       agentTwo => null()   
       return
    end if

    ! Select two random agents
    call random_seed()
    call random_number(r)
    idx1 = int(r * count) + 1
    call random_seed()
    call random_number(r)
    idx2 = int(r * count) + 1

   
      ! Ensure the two indices are different
    do while (idx1 == idx2)
       call random_seed()
       call random_number(r)
       idx2 = int(r * count) + 1

    end do

      ! Ensure the indices are within bounds
    if (idx1 > count .or. idx2 > count) then
       print *, "Random index out of bounds. (select_distinct function)"
       agentOne => null()
       agentTwo => null()   
       return
    end if


    agentOne => agents_array(idx1)%node
    if (.not. associated(agentOne)) then
      print *, "Error: randomly selected agentOne from array is not associated! (select_distinct function)"
      print *, "idx1: ", idx1 , "max_agents: ", max_agents
      print *, "number_of_agents: ", number_of_agents
      return
    end if 
    
    agentTwo => agents_array(idx2)%node
    if (.not. associated(agentTwo)) then
      print *, "Error: randomly selected agentTwo from array is not associated! (select_distinct function)"
      print *, "idx2: ", idx2 , "max_agents: ", max_agents
      print *, "number_of_agents: ", number_of_agents
      return
    end if 



  end subroutine select_random_agents_distinct

  ! Randomly selects two different agents from the list
  subroutine select_random_agents(agent1, agent2)
    type(Node), pointer :: agent1, agent2
    integer :: idx1, idx2
    real r
    type(Node), pointer :: temp_agent
    integer :: count

    count = number_of_agents ! number of agents in the array


      ! Check if there are enough agents to select two
    if (count < 2) then
       print *, "Not enough agents to select from. (select function)"
       agent1 => null()
       agent2 => null()   
       return
    end if

    ! Select two random agents
    call random_seed()
    call random_number(r)
    idx1 = int(r * count) + 1

    call random_seed()
    call random_number(r)
    idx2 = int(r * count) + 1

   

      ! Ensure the indices are within bounds
    if (idx1 > count .or. idx2 > count) then
       print *, "Random index out of bounds."
       agent1 => null()
       agent2 => null()   
       return
    end if


   agent1 => agents_array(idx1)%node
    if (.not. associated(agent1)) then
      print *, "Error: randomly selected agentOne from array is not associated!"
      print *, "idx1: ", idx1 , "max_agents: ", max_agents
      print *, "number_of_agents: ", number_of_agents
      return
    end if 
    
    agent2 => agents_array(idx2)%node
    if (.not. associated(agent2)) then
      print *, "Error: randomly selected agentTwo from array is not associated!"
      print *, "idx2: ", idx2 , "max_agents: ", max_agents
      print *, "number_of_agents: ", number_of_agents
      return
    end if 

  end subroutine select_random_agents

  ! MANAGMENT OF DEAD AGENTS

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


function count_agents() result(count)
    integer :: count
    type(Node), pointer :: current

    count = 0
    current => head_agents
    do while (associated(current))
      count = count + 1
      current => current%next
    end do
end function count_agents

end module mod_agent_class