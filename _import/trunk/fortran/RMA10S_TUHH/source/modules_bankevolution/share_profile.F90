!     Last change:  HN    9 Apr 2009    7:59 pm
module share_profile
USE types
 implicit none
 SAVE
! INTEGER :: number_of_profiles
!,old_profile
 TYPE (profile)            ,allocatable , DIMENSION (:)        :: old_pr 
 TYPE (PROFILE )           ,allocatable , DIMENSION (:),TARGET :: BANKPROFILES
! it will conflicts with subroutines argument (fenodes) in bank evolution
 TYPE (finite_element_node),allocatable , DIMENSION (:),save   :: fenodes  
 logical                                                       :: BANKEVOLUTION
end module
