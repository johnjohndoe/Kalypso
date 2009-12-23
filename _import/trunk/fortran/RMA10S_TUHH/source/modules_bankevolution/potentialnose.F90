!     Last change:  HN    9 Apr 2009   12:39 pm
! This program is to compute the location of water level among the profile nodes and its diatnce to the origin of the current profile.
! this information will be used in unsteady case where the overhang of the bank is submerged and due to tensile failure the submerged area
! will be failed.

subroutine mk_potentialnose(temp_prof,pot_nos,p)


USE types
use share_profile

implicit none

TYPE (potential_nose), INTENT (INout) :: pot_nos
TYPE (profile)       , INTENT (IN)    :: temp_prof
INTEGER              , INTENT (IN)    :: p
REAL(KIND = 8) :: d1, d2, z1, z2,distance
INTEGER :: j

! In the following the position of potential nose, when it is located between two profile nodes is computed
j = pot_nos%nextnode
d1 = temp_prof%prnode(j-p)%distance
d2 = temp_prof%prnode(j)%distance - d1
z1 = temp_prof%prnode(j)%elevation - temp_prof%prnode(j-p)%elevation
z2  = temp_prof%water_elev - temp_prof%prnode(j-p)%elevation

distance = d1 + d2 * z2/z1
pot_nos%dist = distance
pot_nos%node=0

end subroutine

