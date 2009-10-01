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
      CHARACTER(LEN=9)    :: attribute          !(Profile,Nose,Overhang,Front,basepoint,edge)
    END TYPE profile_node

    TYPE :: profile
     integer              :: CL_number
     INTEGER              :: max_nodes          ! maximum prnodes in each profile, which might be less than 100 but not more than it
     Type (profile_node), dimension (200) :: PRnode
     real (KIND=DOUBLE)   :: water_elev
     integer              :: Rnose              ! the profile's node number of the nose of the right overhang
     integer              :: Lnose
     integer              :: Rfront             ! The profile's node number of the undercutting front of the right bank
     integer              :: Lfront
     logical              :: Activation         !(.TRUE. or .FALSE.)

    END TYPE profile

    TYPE :: Finite_element_node
     CHARACTER (LEN =7)   :: typ                !(corner,midside)
     INTEGER              :: Node_Number
     INTEGER              :: Profile_Number
     REAL (KIND=DOUBLE)   :: elevation, water_level          ! IT CAN BE A TYPE OF COORDIATE; FOR EXAPMPLE TYPE (COORDINATE) :: POINT; AND COORDINATE IS REAL ::(X;Y;Z)
     REAL (KIND=DOUBLE)   :: sed_source
     CHARACTER (LEN =10)  :: statuss            !(activate, deactivate)
    END TYPE finite_element_node

   TYPE :: potential_nose
     INTEGER              :: node, nextnode
     REAL(kind = DOUBLE)  :: dist
    END TYPE potential_nose
end module
