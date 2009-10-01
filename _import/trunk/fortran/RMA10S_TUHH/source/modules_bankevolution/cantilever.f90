!     Last change:  HN    9 Apr 2009    7:47 pm
! Cantilever version 1.02
module cantilever

USE types
USE share_profile
use param
USE BLKSANMOD, ONLY :  &
  &       EFFECTIVE_COHESION ,FRICTION_ANGLE, MATRIC_ANGLE, ROOT  &
  &       , EXPO1, EXPO2, EXPO3

implicit none

!TYPE(profile),save :: CANTI_PR        ! CANTI_PR is the profile after CANTILEVER FAILURE calculations

CONTAINS

subroutine cantilever_failure( CANTI_PR, AVAL_PR , WATERELEV , UNSATURATED_SLOPE , EffectVolume , fenode, Submerged_Overhang )

USE types
use share_profile
USE INIT_TYPE
USE PARAM
implicit none

TYPE (finite_element_node), DIMENSION (:), INTENT (IN)   :: fenode   
TYPE(profile)                            , INTENT (INOUT):: CANTI_PR
REAL (KIND = 8)           , DIMENSION (2), INTENT (OUT)  :: EffectVolume            ! the effective volume of collapsed overhang
TYPE (profile) , INTENT (INOUT)   :: aval_pr
REAL (KIND = 8), INTENT (IN)      :: waterelev
REAL           , INTENT (IN)      :: unsaturated_slope
LOGICAL   , INTENT (IN), OPTIONAL :: Submerged_Overhang            

REAL (KIND = 8)    ,DIMENSION (2) :: SAFTEYFACTOR, source                               ! source is the old method of computation of source term based on the total area of of each overhang in cross section. Now the EffectiveVolume is computed instead:
INTEGER            ,DIMENSION (2) :: INTSECT_NO 
TYPE (profile_node),DIMENSION (2) :: INTSECT_NODE
TYPE (profile)     ,DIMENSION (1) :: INITIATE_PROFILE

TYPE (PROFILE)                    :: TEMP_PROFILE , INPUT
TYPE (profile_node)               :: FRONT
REAL (KIND = 8)                   :: DIST, elev , POLYGON_AREA , WEIGHT , AREA 
REAL (KIND = 8)                   :: SLIP_LENGTH , BASE_SUCTION , TOP_SUCTION , MID_SUCTION 
REAL (KIND = 8)                   :: MATRICHEAD , MATRIC_SUCTION , MATRIC_FORCE  
REAL (KIND = 8)                   :: EFFECTIVE_COHESION_FORCE , SHEAR_STRENGTH
REAL (KIND = 8)                   :: ypsilon, func
INTEGER                           :: i, j, indeks, start, K ,  r , N , JTEMP , LR

LOGICAL            ,DIMENSION (2) :: COINCIDE 


Write (*,*) ' Entering to Cantilever failure Modelling ....'

! Initialize variables
SOURCE = 0.
INTSECT_NO = 0
SAFTEYFACTOR=1.0

CALL INIT_PROFILE ( PNODE = INTSECT_NODE  , SIZE = 2 )
CALL INIT_PROFILE ( PROFIL= INITIATE_PROFILE , SIZE = 1 )
CANTI_PR     = INITIATE_PROFILE(1)
TEMP_PROFILE = INITIATE_PROFILE(1)

COINCIDE = .FALSE.                                            ! COINCIDENCE OF INTERSECTION POINT OF EXTRAPOLATION LINE WITH A PROFILE NODE.

if(PRESENT(Submerged_Overhang) ) then
  ypsilon = 0.001
else
  ypsilon = 0.01
end if    

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

 CALL INTERSECT ( AVAL_PR , FRONT , UNSATURATED_SLOPE , START , N , R , DIST , ELEV , INDEKS, 0.001 )


 INTSECT_NODE(I)%DISTANCE      = DIST
 INTSECT_NODE(I)%ELEVATION     = ELEV
 INTSECT_NODE(I)%FE_NODENUMBER = 0
 INTSECT_NODE(I)%WATER_ELEV    = 0.                                   !SINCE, IT HAS NO CONJUGATED FE NODE TO ASSIGN A COMPUTED WATER ELEVATION TO IT.
 INTSECT_NODE(I)%ATTRIBUTE     = 'profile'

 INTSECT_NO (I) = INDEKS                                              ! the profile node number of intersecting point OR the node before intersection, when it
                                                                      ! doesn't coincide with an already existing profile node.
 N              = INDEKS

 IF ( ABS ( AVAL_PR%PRNODE(N)%DISTANCE - DIST ) <= ypsilon ) THEN                     ! therefore, the intersection point concides with a profile node.
!---------------------------------------------------------------------------------------------------

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


  IF (.NOT.PRESENT(Submerged_Overhang)) THEN

 ! COMPUTATION OF APPARENT COHESION ALONG THE SLIP SURFACE; WHICH IS ASSUMED TO BE ALONG THE EXTRAPOLATION LINE:
 ! LATER A MORE REALISTIC MODEL OF SLIP SURFACE CAN BE ADDED; IN WHICH THIS SLOPE IS DETERMINED BASED ON THE MOST
 ! CRITICAL PLANE.

 SLIP_LENGTH     = ( ( DIST - FRONT%DISTANCE ) ** 2 + ( ELEV - FRONT%ELEVATION ) ** 2 ) ** 0.5


! COMPUTATION OF MATRIC SUCTION:
!-----------------------------------------------------------------------------
! IN THE CASE OF LINEAR DISTRIBUTION, ASSUMES A MAXIMUM NEGATIVE PORE PRESSURE
! OF -EXPO1 AT ELEVATION OF 1m ABOVE WATER TABLE, BEYOND WHICH IT REMAINS CONSTANT. 
 
 IF( ( EXPO3 == 0.).AND. (EXPO2 == 0.) ) THEN
 
   BASE_SUCTION    = SUCTIONHEIGHT (WATERELEV , FRONT%ELEVATION )
 
   TOP_SUCTION     = SUCTIONHEIGHT ( WATERELEV , ELEV )

   IF ( (TOP_SUCTION == - 1.0 * EXPO1) .AND. (BASE_SUCTION > - 1.0 * EXPO1 ) ) THEN

    MID_SUCTION    = - 1.0 * EXPO1
    MATRICHEAD     = ( BASE_SUCTION + MID_SUCTION ) * ( 1. + WATERELEV - FRONT%ELEVATION ) / 2.
    MATRICHEAD     = MATRICHEAD + TOP_SUCTION * ( ELEV - ( WATERELEV + 1.0 ) )

   ELSE

    MATRICHEAD     = ( BASE_SUCTION + TOP_SUCTION ) * ( ELEV - FRONT%ELEVATION ) / 2.

   END IF
 
 ELSE

  BASE_SUCTION = SUCTIONHEIGHT (WATERELEV , FRONT%ELEVATION )
  
  MATRICHEAD   = SUCTIONHEIGHT ( WATERELEV , ELEV )
  MATRICHEAD   = MATRICHEAD - BASE_SUCTION
  MATRICHEAD   = func(ELEV- WATERELEV, EXPO3,EXPO2,EXPO1) * (ELEV - FRONT%ELEVATION)- MATRICHEAD
  MATRICHEAD   = -1.0 * MATRICHEAD
  
 ENDIF

 MATRIC_SUCTION = RHO * GRAVITY * MATRICHEAD
 MATRIC_FORCE             = - MATRIC_SUCTION * TAN( MATRIC_ANGLE * PI / 180.)
 EFFECTIVE_COHESION_FORCE = (EFFECTIVE_COHESION + ROOT )* SLIP_LENGTH
 SHEAR_STRENGTH           = EFFECTIVE_COHESION_FORCE + MATRIC_FORCE + TAN ( FRICTION_ANGLE * PI /180. ) * ( WEIGHT * COS ( UNSATURATED_SLOPE * PI /180.) )       ! AIR PRESSURE IS EQUAL TO ZERO.

 SAFTEYFACTOR (I) = SHEAR_STRENGTH / ( WEIGHT * SIN ( UNSATURATED_SLOPE * PI /180. ) )

ELSE

 SAFTEYFACTOR(i) = 0.9

END IF 

end do MN


! SORTING THE PROFILE NODES INTO A NEW PROFILE
J = 0
K = 0
! RETAIN THE NOSE AND FRONT IF IT IS NOT CHANGED LATER IN THE FOLLOWING IF BLOCK.
CANTI_PR%LFRONT     = AVAL_PR%LFRONT 
CANTI_PR%LNOSE      = AVAL_PR%LNOSE
CANTI_PR%RFRONT     = AVAL_PR%RFRONT
CANTI_PR%RNOSE      = AVAL_PR%RNOSE
CANTI_PR%ACTIVATION = .FALSE.
CANTI_PR%CL_NUMBER  = AVAL_PR%CL_NUMBER
CANTI_PR%WATER_ELEV = AVAL_PR%WATER_ELEV

SRT: DO

  J = J + 1
  K = K + 1
  
  IF ( J > AVAL_PR%MAX_NODES) EXIT

  IF ( ( J == INTSECT_NO (1) ) .AND. ( SAFTEYFACTOR (1) < 1.0 ) ) THEN

    CALL COINCID           ( K , J , 1 , CANTI_PR)                                      ! INCLUDE THE INTERSECTION POINT OF EXTRAPOLATION LINE WITH TOP OF THE PROFILE IN CANTI_PR.
    K = K + 1                                            !30APRIL2009, 12:12
    CALL SIMPLE_PROJECTION ( J  , AVAL_PR%PRNODE(AVAL_PR%LFRONT)%DISTANCE , 1 , K )                                ! include the fe_nodes on the extrapolation line as new profile nodes.
    K = K - 1 !30APRIL2009, 16:29
   
 !  call EffectiveVolume (J ,AVAL_PR%LFRONT,1, AVAL_PR%PRNODE(1)%DISTANCE, EffectVolume(1)) 
    EffectVolume(1) = source(1)
    J = AVAL_PR%LFRONT

    IF ( AVAL_PR%PRNODE(J)%FE_NODENUMBER /= 0 ) THEN
    K = K + 1
    CANTI_PR%PRNODE(K) = AVAL_PR%PRNODE(J)                                               ! THE NEXT NODE AFTER INTERSECTION POINT IS THE OLD FRONT; SINCE THE OVERHANG HAS COLLAPSED SF<1.0
    CANTI_PR%PRNODE(K)%ATTRIBUTE = 'profile'                                             ! OMIT THE ATTRIBUTE FRONT ; SINCE IT EXISTS NO MORE DUE TO THE FAILURE.
    ENDIF
    
    CANTI_PR%LFRONT = 0
    CANTI_PR%LNOSE  = 0

  ELSEIF ( ( J == AVAL_PR%RFRONT ) .AND. ( SAFTEYFACTOR (2) < 1.0 ) ) THEN

    CANTI_PR%RFRONT = 0
    CANTI_PR%RNOSE  = 0
    
    CALL SIMPLE_PROJECTION ( J , INTSECT_NODE(2)%DISTANCE , 1 , K )

    J = INTSECT_NO (2)
    JTEMP = J

    CALL COINCID ( K , J , 2 , CANTI_PR)
    
    IF ( JTEMP < J ) J = J - 1    !30APRIL2009, 16:03     ; BECAUSE IN COINCID IN THE CASE THAT THE INTERSECTION POINT COINCIDES WITH A PROFILE NODE, J IS INCREASED BY 1; HOWEVER THE CONTROL AT THE BEGINING OF THE CURRENT LOOP WILL DO THIS INCREMENT AGAIN.
    
 !   call EffectiveVolume (J, AVAL_PR%RFRONT,-1, AVAL_PR%PRNODE(AVAL_PR%MAX_NODES)%DISTANCE, EffectVolume(2)) 
   EffectVolume(2) = source(2) 
  ELSE

     IF (TRIM( ADJUSTL (AVAL_PR%PRNODE(J)%ATTRIBUTE) ) == 'basepoint') THEN
      IF ( AVAL_PR%PRNODE(J)%FE_NODENUMBER == 0) THEN
    
        K = K - 1
        cycle SRT

      ELSE 

        AVAL_PR%PRNODE(J)%ATTRIBUTE = 'profile'
     
      END IF    
     END iF
   
     CANTI_PR%PRNODE(K) = AVAL_PR%PRNODE(J)
     
     IF ( K > 1 ) THEN
   
       DIST = SQRT ( ( CANTI_PR%PRNODE(K)%DISTANCE - CANTI_PR%PRNODE(K - 1)%DISTANCE)** 2 &
       &            +( CANTI_PR%PRNODE(K)%ELEVATION - CANTI_PR%PRNODE(K - 1)%ELEVATION)**2 )
   
       IF ( DIST  <= 0.01) THEN
         
         IF ( (TRIM(ADJUSTL(CANTI_PR%PRNODE(K)%ATTRIBUTE))  == 'front') .OR. &
          &   (TRIM(ADJUSTL(CANTI_PR%PRNODE(K)%ATTRIBUTE))  == 'nose' ) .OR. &
          &   (TRIM(ADJUSTL(CANTI_PR%PRNODE(K-1)%ATTRIBUTE))== 'front') .OR. &
          &   (TRIM(ADJUSTL(CANTI_PR%PRNODE(K-1)%ATTRIBUTE))== 'nose' ) ) THEN 
    
           CANTI_PR%MAX_NODES = K
     
           CYCLE SRT
         
         END IF  

          IF ((CANTI_PR%PRNODE(K - 1)%FE_NODENUMBER /= 0 ).AND. &
            & (CANTI_PR%PRNODE(K)%FE_NODENUMBER == 0 ) )  THEN
              
              CAll SHIFTNOSE 
              K = K - 1
   
          ELSEIF ( (CANTI_PR%PRNODE(K)%FE_NODENUMBER /= 0 ).AND. &
           &       (CANTI_PR%PRNODE(K - 1)%FE_NODENUMBER == 0 )) THEN   
   
              CAll SHIFTNOSE
              CANTI_PR%PRNODE(K - 1) = CANTI_PR%PRNODE(K)
              K = K - 1
   
          ELSEIF ( (CANTI_PR%PRNODE(K)%FE_NODENUMBER /= 0 ).AND. &
           &       (CANTI_PR%PRNODE(K - 1)%FE_NODENUMBER /= 0 ) )THEN  
   
               WRITE (*,*) ' WARNING!!, IN CONTILINE NUMBER ' ,CANTI_PR%CL_NUMBER , & 
                   ' THE PROFILE NODES ', K - 1,' AND ', K , ' ARE ', &
               &   ' CLOSER THAN 1 CM TOGETHER '
          ELSE
               
               CAll SHIFTNOSE
               K = K - 1   
   
          END IF   
   
       END IF  
   
     END IF
  
  END IF
   
   
   CANTI_PR%MAX_NODES = K

 END DO  SRT


!------------------------  INCLUDED PROCEDURES  ---------------------------------
CONTAINS

SUBROUTINE  SHIFTNOSE 

               IF ( k < CANTI_PR%LNOSE) THEN
                
                CANTI_PR%LNOSE  = CANTI_PR%LNOSE - 1
                CANTI_PR%LFRONT = CANTI_PR%LFRONT - 1

                IF (CANTI_PR%RNOSE /= 0) THEN
                  CANTI_PR%RNOSE  = CANTI_PR%RNOSE - 1 
                  CANTI_PR%RFRONT = CANTI_PR%RFRONT - 1
                ENDIF
                  
              ELSEIF (CANTI_PR%RNOSE /= 0) THEN
               
               IF (k < CANTI_PR%RNOSE ) THEN 
                CANTI_PR%RNOSE  = CANTI_PR%RNOSE - 1
                CANTI_PR%RFRONT = CANTI_PR%RFRONT - 1
               ENDIF
              
              ENDIF

END SUBROUTINE SHIFTNOSE
 


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
    

    RELATIVE_DISTANCE   =  AVAL_PR%PRNODE(O)%DISTANCE - UPPERBOUND
    IF ( ( RELATIVE_DISTANCE  > 0.01  ) .OR. ( ABS (RELATIVE_DISTANCE) <= 0.01  ) ) EXIT 
    if (AVAL_PR%RFRONT /=0) then
    IF ( ( ABS(AVAL_PR%PRNODE(O)%DISTANCE - AVAL_PR%PRNODE(AVAL_PR%RFRONT)%DISTANCE)  <= 0.01  ) .AND. ( ABS(AVAL_PR%PRNODE(O)%ELEVATION - AVAL_PR%PRNODE(BEGIN)%ELEVATION)  <= 0.01 ) ) CYCLE !02MAY2009, 13:37,  IN THE CASE THAT A PROFILE NODE IN TH EOVERHANG LOCATES OVER THE RIGHT FRONT, CYCLE THE LOOP, SINCE THE STARTING POINT HAS BEEN ALREADY INCLUDED.
    end if

  IF (  AVAL_PR%PRNODE(O)%DISTANCE  >= LOWERBOUND  )  THEN 
    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)               = AVAL_PR%PRNODE(O)      

    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%ELEVATION     = fenode( Q )%ELEVATION   ! SINCE THE ELEVATION OF THE fenode ON EXTRAPOLATION LINE HAS BEEN ALREADY CALCULATED IN SUBROUTINE PROJECTION, THEY ARE USED AS PROJECTED ELEVATION OF AVAL_PR NODE.
    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%ATTRIBUTE     = 'profile'               ! CORRECT THE ATTRIBUTE OF PROFIEL NODE ON THE EXTRAPOLATION LINE AS ' PROFILE' , NOMORE OVERHANG.
    CANTI_PR%PRNODE(CURRENT_CANTI_INDEX)%FE_NODENUMBER = Q                       ! ASSIGN THE POSITIVE FENODE NUMBER TO THE PROJECTED NODE ON THE FORMER EXTRAPOLATION LINE.
    CURRENT_CANTI_INDEX = CURRENT_CANTI_INDEX + 1            
  END IF
  END DO
 
END SUBROUTINE SIMPLE_PROJECTION

!-----------------------------------------------------------------------------
REAL (KIND = 8)  FUNCTION SUCTIONHEIGHT ( WATER_LEVEL , ELEVATION)

implicit none

REAL (KIND = 8), INTENT (IN) :: WATER_LEVEL , ELEVATION 
REAL (KIND = 8)              :: AREA_UNDER_CURVE, REL_HEIGHT_START, REL_HEIGHT_END


SUCTIONHEIGHT = 0.

IF( (EXPO3==0.).and.(EXPO2==0.) )THEN

SUCTIONHEIGHT = (WATER_LEVEL - ELEVATION) * EXPO1

IF ( SUCTIONHEIGHT < -1.0 * expo1 ) SUCTIONHEIGHT = -1.0 * EXPO1

ELSE

REL_HEIGHT_START = 0.
REL_HEIGHT_END   = ELEVATION - WATER_LEVEL

CALL INTEGRAL (REL_HEIGHT_START, REL_HEIGHT_END ,EXPO3, EXPO2, EXPO1 , AREA_UNDER_CURVE)

SUCTIONHEIGHT =  AREA_UNDER_CURVE

ENDIF

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

REAL (KIND= 8) FUNCTION POLYGON_AREA ( START, N , R , PROFIL )

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
subroutine integral(n1,n2,a,b,c,s)

implicit none

REAL (kind = 8)  , INTENT (IN) ::  n1, n2, a ,b,c
REAL (kind = 8)  , INTENT (OUT):: s
REAL( kind = 8)                :: x,olds,tolerance
INTEGER :: i,j

tolerance = 1.e-5
j=200
s=0.
olds=0.

do I = 1,j

call integral_trapz(n1,n2,I,s, a, b, c)

if (s == 0. .and. olds== 0. ) exit
if (ABS(s-olds)<= tolerance) exit

olds=s

end do

end subroutine

subroutine integral_trapz(n1,n2,n,s,a,b,c)

IMPLICIT NONE

 INTEGER      , INTENT (IN)    :: n
 REAL(kind=8) , INTENT (IN)    :: n1,n2,a,b,c
 REAL(kind=8) , intent (INOUT) :: s
 
 REAL(kind=8)                  :: func, del, summ, tnm, x
 INTEGER                       :: it, j


 if (n== 1)  then
 
   s = 0.5*(n2-n1) * (func(n1,a,b,c) + func(n2,a,b,c))
 
 else
   
   it = 2**(n-2)
   tnm = it *1.0
   del= (n2-n1)/tnm
   x= n1+0.5 *del
   SUMm=0.
   
   do j=1,it
    summ=summ+func(x,a,b,c)
    x=x+del
   end do

    s = 0.5*(s+(n2-n1)*summ/tnm)       ! It replces s with its refined value
 end if
 
 return

end subroutine

function func(x,a,b,c)

REAL(kind=8),INTENT(IN)  :: x, a , b, c
REAL(kind=8)             :: func

func = a * x**3 + b * x**2 + c * x

end function func

