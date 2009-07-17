!     Last change:  HN    9 Apr 2009    7:47 pm
! Cantilever version 1.02
module cantilever

USE types
USE share_profile
use param
USE BLKSANMOD, ONLY :  &
  &       EFFECTIVE_COHESION ,FRICTION_ANGLE, MATRIC_ANGLE

implicit none

TYPE(profile),save :: CANTI_PR        ! CANTI_PR is the profile after CANTILEVER FAILURE calculations

CONTAINS

subroutine cantilever_failure( AVAL_PR , WATERELEV , UNSATURATED_SLOPE , EffectVolume , fenode )

USE types
use share_profile
USE INIT_TYPE
USE PARAM
implicit none

TYPE (finite_element_node), DIMENSION (:), INTENT (IN):: fenode   
REAL (KIND = 8)           , DIMENSION (2), INTENT (OUT)  :: EffectVolume            ! the effective volume of collapsed overhang

TYPE (profile) , INTENT (IN)      :: aval_pr
REAL (KIND = 8), INTENT (IN)      :: waterelev
REAL           , INTENT (IN)      :: unsaturated_slope

REAL (KIND = 8)    ,DIMENSION (2) :: SAFTEYFACTOR=1.1, source                               ! source is the old method of computation of source term based on the total area of of each overhang in cross section. Now the EffectiveVolume is computed instead:
INTEGER            ,DIMENSION (2) :: INTSECT_NO 
TYPE (profile_node),DIMENSION (2) :: INTSECT_NODE
TYPE (profile)     ,DIMENSION (1) :: INITIATE_PROFILE

TYPE (PROFILE)                    :: TEMP_PROFILE , INPUT
TYPE (profile_node)               :: FRONT
REAL (KIND = 8)                   :: DIST, elev , POLYGON_AREA , WEIGHT , AREA 
REAL (KIND = 8)                   :: SLIP_LENGTH , BASE_SUCTION , TOP_SUCTION , MID_SUCTION 
REAL (KIND = 8)                   :: MATRICHEAD , MATRIC_SUCTION , MATRIC_FORCE  
REAL (KIND = 8)                   :: EFFECTIVE_COHESION_FORCE , SHEAR_STRENGTH
!REAL (KIND = 8)                   :: UNSATURATED_RADIAN
INTEGER                           :: i, j, indeks, start, K ,  r , N , JTEMP , LR

LOGICAL            ,DIMENSION (2) :: COINCIDE 


Write (*,*) ' Entering to Cantilever failure Modelling ....'

! Initialize variables
SOURCE = 0.
INTSECT_NO = 0

CALL INIT_PROFILE ( PNODE = INTSECT_NODE  , SIZE = 2 )
CALL INIT_PROFILE ( PROFIL= INITIATE_PROFILE , SIZE = 1 )
CANTI_PR     = INITIATE_PROFILE(1)
! 30APRIL2009, 12:33;  CANTI_PR     = AVAL_PR
TEMP_PROFILE = INITIATE_PROFILE(1)

COINCIDE = .FALSE.                                            ! COINCIDENCE OF INTERSECTION POINT OF EXTRAPOLATION LINE WITH A PROFILE NODE.

! determine if there are one bank or two banks available.
! if the right bank is totally submerged, it implies there is no rightbnak morphological evolution
! meant.

if (aval_pr%water_elev > aval_pr%prnode(aval_pr%max_nodes)%elevation ) then
   LR = 1                  ! only left bank is available.
else 
   LR = 2                  ! Left and rigtht banks area available.
end if      

MN: DO i  = 1,LR                                                ! loop over two possible overhangs.


 SELECT CASE (i)

    CASE (1)

      START = aval_pr%Lfront
      N     = 1
      R     = -1

      IF (START == 0) CYCLE MN

      FRONT =aval_pr%prnode(START)

    CASE (2)

      START = aval_pr%Rfront
      N     = aval_pr%max_nodes
      R     = 1

      IF (START == 0) CYCLE MN

      FRONT = aval_pr%prnode(START)

 END SELECT

! IN THE FOLLOWING THE INTERSECTION BETWEEN THE EXTRAPOLATION LINE, STARING FROM FRONT AND ENDING ON THE TOP OF THE OVERHANG,
! IS COMPUTED ( DIST AND ELEV ARE COORDINATE OF THIS POINT.
 !UNSATURATED_RADIAN = PI*UNSATURATED_SLOPE/180.

 CALL INTERSECT ( AVAL_PR , FRONT , UNSATURATED_SLOPE , START , N , R , DIST , ELEV , INDEKS )


 INTSECT_NODE(I)%DISTANCE      = DIST
 INTSECT_NODE(I)%ELEVATION     = ELEV
 INTSECT_NODE(I)%FE_NODENUMBER = 0
 INTSECT_NODE(I)%WATER_ELEV    = 0.                                   !SINCE, IT HAS NO CONJUGATED FE NODE TO ASSIGN A COMPUTED WATER ELEVATION TO IT.
 INTSECT_NODE(I)%ATTRIBUTE     = 'profile'


 INTSECT_NO (I) = INDEKS                                              ! the profile node number of intersecting point OR the node before intersection, when it
                                                                      ! doesn't coincide with an already existing profile node.
 N              = INDEKS

 IF ( ABS ( AVAL_PR%PRNODE(N)%DISTANCE - DIST ) <= 0.01 ) THEN                     ! therefore, the intersection point concides with a profile node.

 SOURCE(I)  = POLYGON_AREA ( START, N , R , aval_pr )                      ! ASSIGNMENT OF THE LOST AREA/ VOLUME AS SOURCE TERM.
 WEIGHT     = SOURCE (I) * RHOS * GRAVITY
 COINCIDE(I)= .TRUE.
 
 INTSECT_NODE(I)               =     AVAL_PR%PRNODE(N)                         ! 30 APRIL2009, 11:50 ; WHEN THE INTERSECTION IS EQUAL TO A PROFILE NODE , THEN THE PROFILENODE SHOULD BE ASSIAGNED TO INTERSECTION POINT.
 INTSECT_NODE(I)%FE_NODENUMBER = abs(AVAL_PR%PRNODE(N)%FE_NODENUMBER)           ! ASSIGNMENT OF THE FE NODE NUMBER TO THE INTERSECTION POINT.

 ELSE

 TEMP_PROFILE%PRNODE(1)           = FRONT
 TEMP_PROFILE%PRNODE(2)           = AVAL_PR%PRNODE(N)
 TEMP_PROFILE%PRNODE(3)%DISTANCE  = DIST
 TEMP_PROFILE%PRNODE(3)%ELEVATION = ELEV

 AREA       = POLYGON_AREA ( 1, 3 , 1 , TEMP_PROFILE )                  ! THE AREA BETWEEN THE INTERSECTION POINT, FRONT AND LAST PROFILE NODE
 AREA       = AREA + POLYGON_AREA ( START, N , R , AVAL_PR )            ! THE AREA OF THE WHOLE OVERHANG.

 SOURCE (I) = AREA
 WEIGHT     = AREA * RHOS * GRAVITY                                      ! RHOS CAN BE LATER COMPUTED BASED ON THE PROSITY OF THE SOIL AND WATER CONTENT.
 end if

 ! COMPUTATION OF APPARENT COHESION ALONG THE SLIP SURFACE; WHICH IS ASSUMED TO BE ALONG THE EXTRAPOLATION LINE:
 ! LATER A MORE REALISTIC MODEL OF SLIP SURFACE CAN BE ADDED; IN WHICH THIS SLOPE IS DETERMINED BASED ON THE MOST
 ! CRITICAL PLANE.

 SLIP_LENGTH     = ( ( DIST - FRONT%DISTANCE ) ** 2 + ( ELEV - FRONT%ELEVATION ) ** 2 ) ** 0.5

! COMPUTATION OF MATRIC SUCTION:
 BASE_SUCTION    = SUCTIONHEIGHT (WATERELEV , FRONT%ELEVATION )
 
 TOP_SUCTION     = SUCTIONHEIGHT ( WATERELEV , ELEV )

 IF ( (TOP_SUCTION == - 1.0) .AND. (BASE_SUCTION > - 1.0 ) ) THEN
  MID_SUCTION    = - 1.0

  MATRICHEAD     = ( BASE_SUCTION + MID_SUCTION ) * ( 1 + WATERELEV - FRONT%ELEVATION ) / 2.
  MATRICHEAD     = MATRICHEAD + TOP_SUCTION * ( ELEV - ( WATERELEV + 1.0 ) )
  MATRIC_SUCTION = RHO * GRAVITY * MATRICHEAD

 ELSE

  MATRICHEAD     = ( BASE_SUCTION + TOP_SUCTION ) * ( ELEV - FRONT%ELEVATION ) / 2.
  MATRIC_SUCTION = RHO * GRAVITY * MATRICHEAD

 END IF

 MATRIC_FORCE             = - MATRIC_SUCTION * TAN( MATRIC_ANGLE * PI / 180.)
 EFFECTIVE_COHESION_FORCE = EFFECTIVE_COHESION * SLIP_LENGTH
 SHEAR_STRENGTH           = EFFECTIVE_COHESION_FORCE + MATRIC_FORCE + TAN ( FRICTION_ANGLE * PI /180. ) * ( WEIGHT * COS ( UNSATURATED_SLOPE * PI /180.) )       ! AIR PRESSURE IS EQUAL TO ZERO.

 SAFTEYFACTOR (I) = SHEAR_STRENGTH / ( WEIGHT * SIN ( UNSATURATED_SLOPE * PI /180. ) )

 ! check if the intersection coincides with a node, if not creat the node and sort the profile betwwen front
 ! and intersection in a temporary allocatable profile and compute the failure test on this profile, in the case
 ! a failure happens, then remove the nodes between front and intersection point and sort the new profile in a new
 !  variable (TYPE : profile).

end do MN
! SORTING THE PROFILE NODES INTO A NEW PROFILE
J = 0
K = 0

SRT: DO

  J = J + 1
  K = K + 1
  
  IF ( J > AVAL_PR%MAX_NODES) EXIT

  IF ( ( J == INTSECT_NO (1) ) .AND. ( SAFTEYFACTOR (1) < 1.0 ) ) THEN

    CALL COINCID           ( K , J , 1 , CANTI_PR)                                      ! INCLUDE THE INTERSECTION POINT OF EXTRAPOLATION LINE WITH TOP OF THE PROFILE IN CANTI_PR.
    K = K + 1                                            !30APRIL2009, 12:12
    CALL SIMPLE_PROJECTION ( J  , AVAL_PR%PRNODE(AVAL_PR%LFRONT)%DISTANCE , 1 , K )                                ! include the fe_nodes on the extrapolation line as new profile nodes.
    K = K - 1 !30APRIL2009, 16:29
   
   call EffectiveVolume (J ,AVAL_PR%LFRONT,1, AVAL_PR%PRNODE(1)%DISTANCE, EffectVolume(1)) 
   
    J = AVAL_PR%LFRONT

    IF ( AVAL_PR%PRNODE(J)%FE_NODENUMBER /= 0 ) THEN
    K = K + 1
    CANTI_PR%PRNODE(K) = AVAL_PR%PRNODE(J)                                               ! THE NEXT NODE AFTER INTERSECTION POINT IS THE OLD FRONT; SINCE THE OVERHANG HAS COLLAPSED SF<1.0
    CANTI_PR%PRNODE(K)%ATTRIBUTE = 'profile'                                             ! OMIT THE ATTRIBUTE FRONT ; SINCE IT EXISTS NO MORE DUE TO THE FAILURE.
    ENDIF
    
    CANTI_PR%LFRONT = 0
    CANTI_PR%LNOSE  = 0

  ELSEIF ( ( J == AVAL_PR%RFRONT ) .AND. ( SAFTEYFACTOR (2) < 1.0 ) ) THEN
!----------------------------------------------
!30APRIL2009, 18:22
 !   IF ( AVAL_PR%PRNODE(J)%FE_NODENUMBER /= 0 ) THEN
 !   CANTI_PR%PRNODE(K) = AVAL_PR%PRNODE(J)
 !   CANTI_PR%PRNODE(K)%ATTRIBUTE = 'profile'                                              ! OMIT THE ATTRIBUTE FRONT ; SINCE IT EXISTS NO MORE DUE TO THE FAILURE.
 !   K = K + 1
 !   ENDIF
!30APRIL2009, 18:22
!----------------------------------------------
    CANTI_PR%RFRONT = 0
    CANTI_PR%RNOSE  = 0
    
!    J = J + 1
    
!30APRIL2009, 12:47    CALL SIMPLE_PROJECTION ( J , INTSECT_NO (2)- 1 , 1 , K )                          ! include the fe_nodes on the extrapolation line as new profile nodes.
 !30APRIL2009, 19:14   CALL SIMPLE_PROJECTION ( J , INTSECT_NO (2) , 1 , K )                                !30APRIL2009, 12:47
    CALL SIMPLE_PROJECTION ( J , INTSECT_NODE(2)%DISTANCE , 1 , K )
! CALL SIMPLE_PROJECTION ( J , 1 , K , INTERSECTION_POINT = INTSECT_NODE(2) )
 !30APRIL2009, 12:47   K = K + 1
    J = INTSECT_NO (2)
    JTEMP = J
    CALL COINCID ( K , J , 2 , CANTI_PR)
    
    IF ( JTEMP < J ) J = J - 1    !30APRIL2009, 16:03     ; BECAUSE IN COINCID IN THE CASE THAT THE INTERSECTION POINT COINCIDES WITH A PROFILE NODE, J IS INCREASED BY 1; HOWEVER THE CONTROL AT THE BEGINING OF THE CURRENT LOOP WILL DO THIS INCREMENT AGAIN.
    
    call EffectiveVolume (J, AVAL_PR%RFRONT,-1, AVAL_PR%PRNODE(AVAL_PR%MAX_NODES)%DISTANCE, EffectVolume(2)) 
  
  ELSE

   CANTI_PR%PRNODE(K) = AVAL_PR%PRNODE(J)

  END IF
   
   
   CANTI_PR%MAX_NODES = K

 END DO  SRT

   CANTI_PR%ACTIVATION = .FALSE.
   CANTI_PR%CL_NUMBER  = AVAL_PR%CL_NUMBER
   CANTI_PR%WATER_ELEV = AVAL_PR%WATER_ELEV

!------------------------  INCLUDED PROCEDURE  ---------------------------------
CONTAINS
SUBROUTINE COINCID (L, N , M , OUTPROFILE )

INTEGER        , INTENT (IN)    :: L , M !,N    !30APRIL2009, 12:47
INTEGER        , INTENT (INOUT) :: N            !30APRIL2009, 12:47
TYPE (PROFILE) , INTENT (INOUT) :: OUTPROFILE
! IN FACT THE FOLLOWING IF - STRUCTURES SEEMS NOT TO BE NECESSARY SINCE; IN THE CASE OF COINCIDENCE, INTSECT-NODE COMPRISES THE PROFILENODE ITSELF.
  IF ( .NOT.COINCIDE(M) )  THEN                             ! IF THE CURRENT NODE IS THE ONE FOLLOWING INTERSECTION POINT AND THIS POINT DOESNOT COINCIDES WITH A PROFILE NODE:
                                                            ! THEN INCLUDE THIS NODE AS AN EXTRANODE.
   OUTPROFILE%PRNODE(L) = INTSECT_NODE(M)

  ELSEIF ( COINCIDE(M) )  THEN

   OUTPROFILE%PRNODE(L) = AVAL_PR%PRNODE(N)
   OUTPROFILE%PRNODE(L)%FE_NODENUMBER = ABS (AVAL_PR%PRNODE(N)%FE_NODENUMBER)
   N = N + 1                                                                !30APRIL2009, 12:47  

  END IF
 
END SUBROUTINE COINCID
!-----------------------------------------------------------------------------
!SUBROUTINE SIMPLE_PROJECTION ( BEGIN  , INCREMENT , CURRENT_CANTI_INDEX, , FINISH , INTERSECTION_POINT )
SUBROUTINE SIMPLE_PROJECTION ( BEGIN  ,UPPERBOUND ,INCREMENT , CURRENT_CANTI_INDEX )
! In this subroutine the fenodes along projection line between front and intersection on top of the profile
! will be regenerated with new fenode elevations.

!TYPE (PROFILE_NODE) , INTENT (IN), OPTIONAL ::  INTERSECTION_POINT 
!INTEGER        , INTENT (IN) , OPTIONAL :: FINISH

INTEGER         , INTENT (IN)    :: BEGIN , INCREMENT 
REAL ( KIND = 8), INTENT (IN)    :: UPPERBOUND
INTEGER         , INTENT (INOUT) :: CURRENT_CANTI_INDEX

REAL ( KIND = 8)                 :: RELATIVE_DISTANCE , LOWERBOUND  
INTEGER                          :: O , Q , ENDINDEX

  LOWERBOUND           = AVAL_PR%PRNODE(BEGIN)%DISTANCE  !- AVAL_PR%PRNODE(1)%DISTANCE
  
  
 ! IF ( PRESENT (FINISH) ) THEN 
  !ENDINDEX  = FINISH
  !UPPERBAND = AVAL_PR%PRNODE(FINISH)%DISTANCE !- AVAL_PR%PRNODE(1)%DISTANCE          
  !ELSE
  ENDINDEX  = AVAL_PR%MAX_NODES
  !UPPERBAND = INTERSECTION_POINT%DISTANCE
  !END IF
  
  DO O = BEGIN , ENDINDEX , INCREMENT
   
    Q = ABS(AVAL_PR%PRNODE(O)%FE_NODENUMBER)    
    
    IF ( Q == 0 )CYCLE 
    
 !30APRIL2009, 18:57   RELATIVE_DISTANCE   = AVAL_PR%PRNODE(O)%DISTANCE !- AVAL_PR%PRNODE(1)%DISTANCE 
    RELATIVE_DISTANCE   =  AVAL_PR%PRNODE(O)%DISTANCE - UPPERBOUND
    IF ( ( RELATIVE_DISTANCE  > 0.01  ) .OR. ( ABS (RELATIVE_DISTANCE) <= 0.01  ) ) EXIT !30APRIL2009, 18:57
    if (AVAL_PR%RFRONT /=0) then
    IF ( ( ABS(AVAL_PR%PRNODE(O)%DISTANCE - AVAL_PR%PRNODE(AVAL_PR%RFRONT)%DISTANCE)  <= 0.01  ) .AND. ( ABS(AVAL_PR%PRNODE(O)%ELEVATION - AVAL_PR%PRNODE(BEGIN)%ELEVATION)  <= 0.01 ) ) CYCLE !02MAY2009, 13:37,  IN THE CASE THAT A PROFILE NODE IN TH EOVERHANG LOCATES OVER THE RIGHT FRONT, CYCLE THE LOOP, SINCE THE STARTING POINT HAS BEEN ALREADY INCLUDED.
    end if
!30APRIL2009, 18:57    IF ( ( RELATIVE_DISTANCE  <= UPPERBAND  ) .AND. ( RELATIVE_DISTANCE >= LOWERBAND  ) ) THEN
 !30APRIL2009, 12:12   CURRENT_CANTI_INDEX = CURRENT_CANTI_INDEX + 1
    
 !30APRIL2009, 15:40   CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)               = AVAL_PR%PRNODE(O)       ! COPY THE NEXT AVAL_PR NODE AS PROJECTED NODE ON EXTRAPOLATION LINE TO CANTI_PR.
   
 !30APRIL2009, 15:50   IF ( ABS ( CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%ELEVATION - fenode( Q )%ELEVATION ) > 0.01 ) THEN   ! IT IS INCULDED TO PROHIBIT DUPLICATION OF INTERSECTION POINT, WHEN IT COINCIDES WITH A PROFILE NODE.
 !30APRIL2009, 18:36   IF ( ABS ( AVAL_PR%PRNODE(O)%ELEVATION - fenode( Q )%ELEVATION ) > 0.01 ) THEN !30APRIL2009, 15:50
  IF (  AVAL_PR%PRNODE(O)%DISTANCE  >= LOWERBOUND  )  THEN !30APRIL2009, 19:59
    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)               = AVAL_PR%PRNODE(O)       ! 30APRIL2009, 15:40

    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%ELEVATION     = fenode( Q )%ELEVATION  ! SINCE THE ELEVATION OF THE fenode ON EXTRAPOLATION LINE HAS BEEN ALREADY CALCULATED IN SUBROUTINE PROJECTION, THEY ARE USED AS PROJECTED ELEVATION OF AVAL_PR NODE.
    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%ATTRIBUTE     = 'profile'               ! CORRECT THE ATTRIBUTE OF PROFIEL NODE ON THE EXTRAPOLATION LINE AS ' PROFILE' , NOMORE OVERHANG.
    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%FE_NODENUMBER = Q                       ! ASSIGN THE POSITIVE FENODE NUMBER TO THE PROJECTED NODE ON THE FORMER EXTRAPOLATION LINE.
    CURRENT_CANTI_INDEX = CURRENT_CANTI_INDEX + 1             !30APRIL2009, 15:50
!30APRIL2009, 18:36    END IF
!30APRIL2009, 15:50    CURRENT_CANTI_INDEX = CURRENT_CANTI_INDEX + 1     !30APRIL2009, 12:12
 !30APRIL2009, 18:57   END IF 
  END IF !30APRIL2009, 19:59
  END DO
 
END SUBROUTINE SIMPLE_PROJECTION

!-----------------------------------------------------------------------------
REAL (KIND = 8)  FUNCTION SUCTIONHEIGHT ( WATER_LEVEL , ELEVATION)

implicit none
REAL (KIND = 8), INTENT (IN) :: WATER_LEVEL , ELEVATION 

SUCTIONHEIGHT = WATER_LEVEL - ELEVATION

IF ( SUCTIONHEIGHT < -1.0 ) SUCTIONHEIGHT = -1.0 


END FUNCTION SUCTIONHEIGHT
!-----------------------------------------------
subroutine EffectiveVolume (start,endd,inc, origin, OverhangVolume) 
! In this subroutine the effective volume of overhnag between intersection point and front is calculated.
! The effective volume is computed based on the effective area computed in subroutine EffectiveArea multiplied by the
! thickness of overhang over FENODE's elevation on projection line, when the node is between intersection point and front;
! However, when the profile node is beyond front this thickness is computed between the profile node on the overhnag and the lower edge of the
! overhang.
! this subroutine computes the mentioned volume for left and right bank depending on the values passed to subroutine
! start:: is the profile node number of intersection point of projection line with top of the overhang profile.
! endd :: is the profile node number of left or right front.
! inc  :: is the increment of the loop, which is 1 for left bank and -1 for right bank.
! origin :: is the origin to which relative distance is measured for left bank is the firt profile node and for the right bank is last one.
integer       , Intent (IN) :: start, endd, inc
REAL (KIND =8), Intent (IN) :: origin            
REAL (KIND =8), Intent (OUT):: OverhangVolume

REAL (KIND =8)              :: RelDist, ContributeArea, DelatZ
integer                     :: p ,i

OverhangVolume = 0.
do i = start,endd, inc

 ! relative distance to the origin
   RelDist = ABS (AVAL_PR%PRNODE(i)%DISTANCE - origin)
   
   P = ABS(AVAL_PR%PRNODE(i)%FE_NODENUMBER)    
     
   if (p ==0 ) cycle 
   ! compute the effective area.
      call EffectiveArea (ContributeArea,P)
      
      if ( RelDist <= AVAL_PR%PRNODE(ENDD)%DISTANCE) then                    ! if the node has not reached front (left or right)
       DelatZ = AVAL_PR%PRnode(i)%ELEVATION - fenode( P )%ELEVATION
      else                                                                   ! if the node is beyond front (left or right)
       DelatZ = AVAL_PR%PRnode(i)%ELEVATION - AVAL_PR%PRNODE(ENDD)%ELEVATION ! The overhang thickness between the profile node and lower edge of the overhang 
      end if                                                                 ! which is equal to the elevation of front (Prnode(ENDD)).
 
   ! now compute the volume of mass waste according to the contrbutin area area (a part of area of each element connencted
  ! to a node , which may influence that node).
     
      OverhangVolume = OverhangVolume + DelatZ * ContributeArea               

enddo 

 END SUBROUTINE EffectiveVolume

!----------------------------------------------
end subroutine cantilever_failure

end module cantilever
!-----------------------------------------------------------------
 subroutine EffectiveArea (NEWTRIBAREA,N)
 ! computes the contributing area of elemnts connecting to a Node.
 
 USE BLK10MOD  , ONLY : NCORN, AREA
 USE BLKSANMOD , ONLY : ELTON
 
 !Real (kind = 8)  , DIMENSION(NP), INTENT (out) :: NEWTRIBAREA
 INTEGER          , INTENT (in)  :: N               ! the node number to which the contributing area is going to be computed.
 REAL (kind = 8)  , INTENT (out) :: NEWTRIBAREA     ! the contributing area of the all connected elements to node N.
 
 Real    :: FACT
 INTEGER :: I ,S
 
 NEWTRIBAREA =0.
  ! compute the contributing area of the connected elements to the node n
  DO I=1,12         ! over all neighboured elements I to node N
    S = ELTON(N,I)   !Elton is the element number´connected to node N (I =1,12 elements can be connected to a node)
    IF(S .GT. 0) THEN
      IF(NCORN(S) .EQ. 8) THEN    ! for quadrangle elements (NCORN = number of corner nodes including midside nodes)
        FACT=4.
      ELSE                        ! for triangle elements
        FACT=3.
      ENDIF

      if(AREA(S)> 0.)then
        NEWTRIBAREA = NEWTRIBAREA+ AREA(S)/FACT
      endif
    ENDIF
  ENDDO
 END SUBROUTINE  EffectiveArea
!--------------------------------- COMPUTATION OF THE AREA OF A POLYGON  ------------------------------------------

REAL FUNCTION POLYGON_AREA ( START, N , R , PROFIL )

USE TYPES

IMPLICIT NONE

TYPE ( PROFILE ) , INTENT (IN) :: PROFIL
INTEGER          , INTENT (IN) :: START , N , R
INTEGER                        :: J
! this program is to compute the area of a polygon :
! Area = 0.5 * Sigma(Xi.Yi+1 -Xi+1.Yi)
  POLYGON_AREA = 0.

  DO J = START , N , R         ! change starting and ending indeces from 1 (front) to the last (intersection)

   IF ( ABS (J - START) < ABS ( N - START ) ) THEN

    POLYGON_AREA = POLYGON_AREA + ( PROFIL%PRNODE( J )%DISTANCE * PROFIL%PRNODE( J + R )%ELEVATION - &

                 & PROFIL%PRNODE( J + R )%DISTANCE * PROFIL%PRNODE( J )%ELEVATION )

   ELSE

    POLYGON_AREA = POLYGON_AREA + ( PROFIL%PRNODE( N )%DISTANCE * PROFIL%PRNODE( START )%ELEVATION - &

                 & PROFIL%PRNODE( START )%DISTANCE * PROFIL%PRNODE( N )%ELEVATION )

   END IF

  END DO

    POLYGON_AREA = abs(POLYGON_AREA * 0.5)

END FUNCTION POLYGON_AREA
!------------------------------------------------------------------------------


