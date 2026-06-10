module mod_counter

    implicit none
    
    public :: t_world_debug_counters
    public :: t_tick_accumulators

    integer, parameter :: MAX_POPS = 5


! Counter Variables for debugging and performance tracking

    type :: t_world_debug_counters

        integer :: gxgy_out_counter = 0

        integer :: update_pos_calls = 0
        integer :: move_calls = 0
        
        ! Death Counters
        integer :: death_natural = 0
        integer :: death_starvation = 0
        integer :: death_out_of_bounds = 0
        integer :: death_conflict = 0  ! Placeholder
        integer :: death_random = 0
        
        ! Function failure counters
        integer :: failed_get_agent_from_cell = 0
        
    end type t_world_debug_counters

! Accumulators for tracking sums and averages over ticks

  type :: t_tick_accumulators
      ! Variables initialize to 0 automatically when a new struct is created
      real(8) :: phi_death_acc(MAX_POPS) = 0.0d0
      real(8) :: phi_birth_acc(MAX_POPS) = 0.0d0
      integer :: n_alive_acc(MAX_POPS) = 0
      ! --> Add any new variables right here, and you are done. <--
  end type t_tick_accumulators


    type :: t_dynamic_state
        ! This type holds any dynamic state var that is not a accumulator 
        real(8) :: K_fertility(MAX_POPS) = 1.0d0
        ! Shared fertility scale for _shared_MC modules:
        ! one value per cluster, computed from the population-weighted average MC_cl_AV
        real(8) :: K_fertility_shared = 1.0d0
    end type t_dynamic_state

end module mod_counter