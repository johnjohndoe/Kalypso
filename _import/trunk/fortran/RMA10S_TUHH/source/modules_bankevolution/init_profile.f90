!     Last change:  HN    9 Apr 2009    5:49 pm
MODULE INIT_TYPE

contains

SUBROUTINE INIT_PROFILE(PROFIL,SIZE,PNODE,fenode)

USE TYPES

IMPLICIT NONE
INTEGER                                     , INTENT (IN)              :: SIZE
TYPE ( PROFILE )         , DIMENSION (SIZE) , INTENT (INOUT), OPTIONAL :: PROFIL
TYPE ( PROFILE_NODE )    , DIMENSION (SIZE) , INTENT (INOUT), OPTIONAL :: PNODE
TYPE(finite_element_node), DIMENSION (SIZE) , INTENT (INOUT), OPTIONAL :: Fenode
INTEGER                                                               :: I,j

!initialize temp_pr and ava_pr

DO I = 1,SIZE

IF ( PRESENT ( PROFIL ) ) THEN

 PROFIL(i)%cl_number = 0
 PROFIL(i)%max_nodes = 0

 do j = 1,200
  PROFIL(i)%prnode(j)%fe_nodenumber = 0
  PROFIL(i)%prnode(j)%distance = 0.
  PROFIL(i)%prnode(j)%elevation = 0.
  PROFIL(i)%prnode(j)%water_elev = 0.
  PROFIL(i)%prnode(j)%attribute = ''

 end do

 PROFIL(i)%water_elev = 0.
 PROFIL(i)%Rnose = 0
 PROFIL(i)%Lnose = 0
 PROFIL(i)%Rfront = 0
 PROFIL(i)%Lfront = 0
 PROFIL(i)%activation = .TRUE.

ELSEIF (PRESENT ( PNODE )) THEN

  pnode(I)%fe_nodenumber = 0
  pnode(I)%distance = 0.
  pnode(I)%elevation = 0.
  pnode(I)%water_elev = 0.
  pnode(I)%attribute = ''

ELSEIF (PRESENT ( fenode )) THEN
  
  fenode(i).Node_Number = 0
  fenode(i).Profile_Number = 0
  fenode(i).elevation = 0.
  fenode(i).water_level = 0.          
  fenode(i).sed_source = 0.
  fenode(i).typ ='' 
  fenode(i).statuss =''

END IF

end do

END SUBROUTINE  INIT_PROFILE
END MODULE  INIT_TYPE
