!     Last change:  HN    9 Apr 2009    7:59 pm
module share_profile
USE types
 implicit none
 SAVE
! INTEGER :: number_of_profiles
 TYPE (profile)            ,allocatable , DIMENSION (:)        :: old_pr !,old_profile
 TYPE (PROFILE )           ,allocatable , DIMENSION (:),TARGET :: BANKPROFILES
 TYPE (finite_element_node),allocatable , DIMENSION (:),save   :: fenodes  ! it will conflicts with subroutines argument (fenodes) in bank evolution
 logical                                                       :: BANKEVOLUTION
end module
