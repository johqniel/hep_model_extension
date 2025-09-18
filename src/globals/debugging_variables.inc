! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! ++++++++++++ Variables used for debugging and testing ++++++++++++++++++++
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



! ++++++++++++ Used in agent tracking and age_pregnancy modules +++++++++++++++++++

integer :: realised_birth_counter = 0            ! For age_pregnancy module DN 09.25

integer :: agents_born_counter = 0               ! For agent tracking module DN 09.25 

integer :: pregnancy_counter = 0                  ! For age_pregnancy module DN 09.25


! ++++++++++++ These are used in the movement module    ++++++++++++++++++++


integer, dimension(npops) :: out_count_priv_a = 0
integer, dimension(npops) :: out_count_priv_b = 0
integer, dimension(npops) :: drown_count_priv = 0, out_count_priv = 0, death_count_priv = 0

! These below are used in mod_matrix calculations but are closly related to the above
! the above are basically each time tick and then the below are cumulative totals 
integer, dimension(npops) :: drown_count = 0
integer, dimension(npops) :: out_count = 0
integer, dimension(npops) :: death_count = 0
integer, dimension(npops) :: birth_count = 0
integer, dimension(npops) :: hum_count = 0 

integer, dimension(npops) :: out_count_a = 0
integer, dimension(npops) :: out_count_b = 0
