

    !=======================================================================
    ! FUNCTION: agent_die
    ! Moves the agent to the dead agents list and removes it from the agents array.
    ! Adds it to the dead_agents_array.
    !
    !  ! Arguments:
    !   self [TYPE(Node), POINTER] - Pointer to the agent that is dying
    !
    ! Notes:
    !   - This function is called when an agent dies.
    !========================================================================
    subroutine agent_die(self)
            class(Node), intent(inout), TARGET :: self
            type(pointer_node), pointer :: temp_child
            type(Node), pointer :: self_ptr


            self_ptr => self


            if (.not. associated(self_ptr)) then
                print *, "Error: self_ptr is not associated!"
                !return
            end if
            
            ! agents_array management: moves last element to the position of the one that is being removed
            call remove_agent_from_array(self_ptr)
            ! end of agents_array management
            call remove_agent_from_population_matrix(self_ptr)

            call move_dead_agent(self_ptr)

            ! remove agent from grid
            if (associated(self_ptr%grid)) then
                call self_ptr%grid%remove_agent_from_grid(self_ptr)
                !self_ptr%grid => null() ! remove grid pointer from agent is done in remove_agent_from_grid function
            else
                print *, "Warning: Attempting to remove an agent from a grid, but the agent is not associated to any grid!"
            end if
    end subroutine agent_die

    !=======================================================================
    ! FUNCTION: remove_parent_from_children
    ! Removes the parent pointer from all children of the agent.
    !  ! Arguments:
    !   self [TYPE(Node), POINTER] - Pointer to the agent whose children should be updated
    ! Notes:
    !   - This function is called when an agent dies to ensure that children no longer reference the dead parent.
    !   - Is not used anymore, might be deleted. 
    !========================================================================
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

    !=======================================================================
    ! FUNCTION: search_child
    ! Searches for a child in the agent's children list.
    !
    ! Arguments:
    !   self [TYPE(Node), POINTER] - Pointer to the agent whose children should be searched
    !   child_ptr [TYPE(Node), POINTER] - Pointer to the child to be searched for
    !
    ! Returns:
    !   temp_child [TYPE(pointer_node), POINTER] - Pointer to the found child or null if not found
    !
    !========================================================================
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




    !=======================================================================
    ! FUNCTION: remove_child
    ! Removes a child from the agent's children list.
    !
    ! Arguments:
    !   self [TYPE(Node), POINTER] - Pointer to the agent whose child should be removed
    !   child_ptr [TYPE(Node), POINTER] - Pointer to the child to be removed
    !
    ! Notes: THIS FUNCTION DOESNT WORK ATM
    !========================================================================
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
            !call remove_ptr_node(temp_child)
          else 
            temp_child => self%search_child(child_ptr)
            if (.not. associated(temp_child)) then
              print *, "Attempting to remove a child that is not in the list of children of the parent!"
              return
            else
              !call remove_ptr_node(temp_child) 
            end if    
          end if
      end subroutine remove_child



    !=======================================================================
    ! FUNCTION: add_child
    ! Adds a child to the agent's children list.
    !
    ! Arguments:
    !   self [TYPE(Node), POINTER] - Pointer to the agent to which the child should be added
    !   child_ptr [TYPE(Node), POINTER] - Pointer to the child to be added
    !=========================================================================
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
          call append_ptr_node_old(child_ptr, self%children)
          self%number_of_children = self%number_of_children + 1
        end if
    end subroutine add_child



