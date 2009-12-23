!     Last change:  HN    9 Apr 2009    4:45 pm
module types

! the data defined as types for profiles and their nodes are defined here.
implicit none
      integer , parameter :: single=4
      integer , parameter :: DOUBLE=8

! declaration block of data types.

    TYPE :: profile_node
      integer             :: Fe_nodenumber
      real (KIND=DOUBLE)  :: distance
      real (KIND=DOUBLE)  :: elevation
      real (KIND=DOUBLE)  :: Water_elev
!(Profile,Nose,Overhang,Front,basepoint,edge)
      CHARACTER(LEN=9)    :: attribute          
    END TYPE profile_node

    TYPE :: profile
     integer              :: CL_number
! maximum prnodes in each profile, which might be less than 100 but not more than it
     INTEGER              :: max_nodes          
     Type (profile_node), dimension (200) :: PRnode
     real (KIND=DOUBLE)   :: water_elev
! the profile's node number of the nose of the right overhang
     integer              :: Rnose              
     integer              :: Lnose
! The profile's node number of the undercutting front of the right bank
     integer              :: Rfront             
     integer              :: Lfront
!(.TRUE. or .FALSE.)
     logical              :: Activation         

    END TYPE profile

    TYPE :: Finite_element_node
!(corner,midside)
     CHARACTER (LEN =7)   :: typ                
     INTEGER              :: Node_Number
     INTEGER              :: Profile_Number
! IT CAN BE A TYPE OF COORDIATE; FOR EXAPMPLE TYPE (COORDINATE) :: POINT; AND COORDINATE IS REAL ::(X;Y;Z)
     REAL (KIND=DOUBLE)   :: elevation, water_level          
     REAL (KIND=DOUBLE)   :: sed_source
!(activate, deactivate)
     CHARACTER (LEN =10)  :: statuss            
    END TYPE finite_element_node

   TYPE :: potential_nose
     INTEGER              :: node, nextnode
     REAL(kind = DOUBLE)  :: dist
    END TYPE potential_nose
end module
