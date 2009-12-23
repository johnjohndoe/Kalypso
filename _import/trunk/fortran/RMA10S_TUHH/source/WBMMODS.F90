!Nis LAST UPDATE JUN XX 2006 Changes for the use in Lahey
!*************************************************************************************************
!
!  Module                 : WBMMODS.f90
!  Purpose          : Contains Routines and functions associated with WBM Modifications to RMA10S Code              
!  Author           : David Wainwright
!  Initiation Date  : 19/01/04
!
!*************************************************************************************************
Module WBMMODS
Implicit None
!*************************************************************************************************
!
!  Includes Necessary Common Block to interface with F77 fixed format Code
!
!*************************************************************************************************
!*************************************************************************************************
!
!  Module Level Variables
!
!*************************************************************************************************
LOGICAL(4)                 :: wbm_Initiated       
LOGICAL(4)                 :: wbm_InitCons = .FALSE.
LOGICAL(4)                 :: wbm_ScourLim
Character(500)             :: wbm_InitConsFile
Character(500)             :: wbm_ScourLimFile
real(8), ALLOCATABLE      :: wbm_MANNTRANS(:)
real(8), ALLOCATABLE      :: wbm_MANNTRANSOLD(:)
real(8), ALLOCATABLE      :: wbm_BedHeight(:)
real(8), ALLOCATABLE      :: wbm_NewConditions(:,:)
real(8), ALLOCATABLE      :: wbm_ScourLims(:)
integer                :: wbm_NodeCounter
integer                :: wbm_IT
!*************************************************************************************************
!
!  Module Procedures
!
!*************************************************************************************************
Contains
!*************************************************************************************************



!*************************************************************************************************
!
!  Subroutine        : NewRough
!  Purpose           : Renews Roughness Values of Each Element, based on average roughness for 
!                      each node      
!  Author            : David Wainwright based on direction from Dean Patterson.
!  Initiation Date   : 19/01/04
!
!***********************************************************************************************
SUBROUTINE NewRough(TMANN,NOP,NE, wbm_MANNTRANS,WBM_IT,IT,TSNO,ITNNO,NNOD)

IMPLICIT NONE
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INPUTS
!
!*************************************************************************************************
!Nodal Roughnesses
real(8), INTENT(INOUT), ALLOCATABLE   ::  wbm_MANNTRANS(:)   
!Elemental Roughnesses
real(8), INTENT(INOUT), ALLOCATABLE   ::  TMANN(:)           
!Element Nodal Connections
integer (kind = 4), INTENT(IN), ALLOCATABLE   ::  NOP(:,:)           
!Number of Elements
integer, INTENT(IN)                ::  NE                 
!Time Step Number
integer, INTENT(IN)                ::  TSNO               
!Iteration Number
integer, INTENT(IN)                ::  ITNNO              
!Number of Elements
integer, INTENT(INOUT)             ::  wbm_IT             
!CurrentIteration
integer, INTENT(IN)                ::  IT                 
!NumberofNodes
integer, INTENT(IN)                ::  NNOD               
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INTERIM
!
!*************************************************************************************************
integer                             :: Divider
integer                             :: I
integer                             :: J
integer                             :: NODE1
integer                             :: NODE2
integer                             :: NODE3
integer                             :: NODE4
integer                                                   :: COUNT
integer                             :: UPPERBOUND
real(8)                                :: NODE1N
real(8)                                :: NODE2N
real(8)                                :: NODE3N
real(8)                                :: NODE4N
real(8)                                :: CHECK
LOGICAL(4)                             :: MYLOG
LOGICAL(4)                             :: EXT
real(8), ALLOCATABLE                           :: TMANNB(:)
Character(500)                                       :: FILENAME
!*************************************************************************************************
!
!  Loops Through all elements, updating as we go
!
!*************************************************************************************************
UPPERBOUND = UBOUND(TMANN,1)
ALLOCATE(TMANNB(UPPERBOUND))
TMANNB = TMANN
!*************************************************************************************************
!
!  If it is the first time step opens an initial roughnesses.txt file to write the initial roughnesses
!
!*************************************************************************************************
!IF (wbm_IT == 0) THEN
!  OPEN(8866,FILE='INITROUGH.TXT')
!  DO I= 1, UPPERBOUND
!    WRITE(8866,*) I,TMANN(I)
!  END DO
!  CLOSE(8866)
!END IF

!*************************************************************************************************
!
!  Transfers Nodal Roughnesses to elements
!
!*************************************************************************************************
DO I = 1,NE
  IF (TSNO == 3) THEN
    DIVIDER = 0
  END IF
  NODE1 = NOP(I,1)
  NODE2 = NOP(I,3)
  NODE3 = NOP(I,5)
  NODE4 = NOP(I,7)
  Divider = 0
  TMANN(I) = 0
  IF (wbm_MANNTRANS(NODE1) /= 0) THEN
    TMANN(I) = TMANN(I) + wbm_MANNTRANS(NODE1)
    Divider = Divider+1
  END IF
  IF (wbm_MANNTRANS(NODE2) /= 0) THEN
    TMANN(I) = TMANN(I) + wbm_MANNTRANS(NODE2)
    Divider = Divider+1
  END IF
  IF (wbm_MANNTRANS(NODE3) /= 0) THEN
    TMANN(I) = TMANN(I) + wbm_MANNTRANS(NODE3)
    Divider = Divider+1
  END IF
  IF (NODE4 /= 0) THEN
    IF (wbm_MANNTRANS(NODE4) /= 0) THEN
      TMANN(I) = TMANN(I) + wbm_MANNTRANS(NODE4)
        Divider = Divider+1
      END IF
  END IF
  IF (Divider /= 0) Then
    TMANN(I) = TMANN(I)/Divider
  END IF
!  
!  Checks for remaining Zero Values and resets them to previous value   
!  
  IF (TMANN(I) == 0.0) TMANN(I) = TMANNB(I)
!  
!  Check to Ensure Mannings Value is Not too small or too big  
!  
  IF (TMANN(I) > 0.036) TMANN(I) = 0.036
  IF (TMANN(I) < 0.015) TMANN(I) = 0.015
!  
!  Check to Ensure Mannings Value has not changed too significantly this time step  
!  
  
!?  
!?  Needs to be done on a step by step basis, not on an iteration basis : complete later if required  
!?  

END DO
!*************************************************************************************************
!
!  If it is the first time step opens an initial roughnesses.txt file to write the initial roughnesses
!
!*************************************************************************************************
!If (IT == 14) Then
      wbm_IT = wbm_IT+1
!      CHECK = wbm_IT
!  CHECK = (INT(CHECK/24))-(CHECK/24)

!  Override
   Check = 0

      IF (ABS(CHECK) < 0.0001) THEN
!        
!  Works Out if TS File is Connected To     
!     
     INQUIRE (FILE='RoughTS.txt', OPENED=mylog)
!     
! Opens TS file if it doesn't exist     
!     
     If ( .NOT. mylog) Then
       INQUIRE (FILE='RoughTS.txt', EXIST=EXT)
       If ( .NOT. EXT) THEN
         OPEN(8867,FILE='RoughTS.txt')    
       End If
     End If
!     
!  Time series output for given Locations : Based on the Murray Model       
!     
!     IF (I == 1512) Then
     WRITE(8867,*) wbm_IT,TSNO,ITNNO, "GoolwaChannel",TMANN(430)
     WRITE(8867,*) wbm_IT,TSNO,ITNNO, "EntranceThroat",TMANN(1701)
     WRITE(8867,*) wbm_IT,TSNO,ITNNO, "TauwitchereChannel(DS)",TMANN(1007)
     WRITE(8867,*) wbm_IT,TSNO,ITNNO, "TauwitchereChannel(US)",TMANN(1436)
!     End If
!     
!     
!     
!     Write(Filename,'(I5,a10)')wbm_IT,"roughs.txt"
!        OPEN(8866,FILE=Filename)
!        DO I= 1, UPPERBOUND
!          WRITE(8866,*) I,TMANN(I)
!        END DO
!    CLOSE(8866)
    END IF
!End If
!*************************************************************************************************
DO I = 1, NNOD
  IF (wbm_MANNTRANS(I) /= 0.0) THEN
! keeps a value ofzero if the mannings hasn't been changed
    wbm_MANNTRANSOLD(I) = wbm_MANNTRANS(I)  
    IF (wbm_MANNTRANSOLD(I) > 0.036) wbm_MANNTRANSOLD(I) = 0.036
    IF (wbm_MANNTRANSOLD(I) < 0.015) wbm_MANNTRANSOLD(I) = 0.015
  END IF
END DO
wbm_MANNTRANS = 0.0
DEALLOCATE(TMANNB) 
!*************************************************************************************************
!WRITE(*,*) "In Routine"
!*************************************************************************************************
!
!  End of New Rough Subroutine
!
!*************************************************************************************************
END SUBROUTINE NewRough
!*************************************************************************************************




!*************************************************************************************************
!
!  Subroutine        : BedRough
!  Purpose           : Calculates Roughness Value for Each Node based on the Van Rijn Relationships
!  Author            : David Wainwright based on direction from Dean Patterson.
!  Initiation Date   : 19/01/04
!
!*************************************************************************************************
SUBROUTINE BedRough(TAUCR,hd,d90mm,d50mm,ugv,NodeNo, NodeRough, ksc)
IMPLICIT NONE
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INPUTS
!
!*************************************************************************************************
! Critical Bed Shear Stress
real(8), INTENT(IN)                   ::  TAUCR          
! Depth
real(8), INTENT(IN)                   ::  hd             
! 90 percentile Grain Size
real(8), INTENT(IN)                   ::  d90mm          
! Median Grain Size
real(8), INTENT(IN)                   ::  d50mm          
! Median Grain Size
real(8), INTENT(IN)                   ::  ugv            
! Current Node for which roughness is being calculated
INTEGER, INTENT(IN)                   ::  NodeNo         
!Nodal Roughnesses
real(8), INTENT(INOUT)                ::  NodeRough      
! Ripple Height
real(8), INTENT(INOUT)                ::  KSC            
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INTERIM
!
!*************************************************************************************************
! Grain Related Shear Stress?
real(8)                               ::  TAUGR          
! Grain Related Chezy Constant
real(8)                               ::  CHEZYGR        
! Bed Shear Stress Parameter
real(8)                               ::  T              
! Dune Height
real(8)                               ::  DD             
! Ripple Height
real(8)                               ::  RD             
! Chezy Coefficient
real(8)                               ::  CC             
! Local Mannings Coefficient
real(8)                               ::  LMANN          
! D90 in m
real (KIND = 8)                               ::  D90M           
! D50 in m
real(8)                               ::  D50M           
!*************************************************************************************************
!
!     Body of Routine
!
!*************************************************************************************************
IF (NodeNo == 770 .OR. NodeNo == 782 .OR. NodeNo == 1552 .OR. NodeNo == 1553) Then
  chezygr = 0
END IF
!
!  Converts Grain Dimensions
!
d90m=d90mm/1000
d50m=d50mm/1000
!
CHEZYGR = 18 * log10((12*hd)/(3*d90m))
TAUGR = 1030*9.81*((ugv/CHEZYGR)**2)
T = (TAUGR-TAUCR)/TAUCR
!IF (T > 1.0) THEN  !  djw 19/01/05
IF (T > 0.01) THEN
   DD = 0.11 * hd * ((d50m/hd)**0.3)*((1-EXP(-0.5*T)))*(25-T)
   IF (DD < 0.005) THEN
     DD = 0.005
   END IF
   RD = 0.02 * hd * (1 - EXP(-0.1*T))*(10-T)
   IF (RD < 0.005) THEN
     RD = 0.005
   END IF
   KSC = (3 * d90m) + (0.7 * RD) + (1.1*0.7*DD*(1-EXP((-25*DD)/(20*DD))))
   CC = 18 * LOG10((12*hd)/KSC)
   LMANN = (hd**(0.16666666667))/CC
   NodeRough=LMANN
ELSE 
! A Value of Zero later on is used as a flag to indicate that 
   NodeRough=0                   
! No Change should be made in subroutine new Rough                                  
END IF
!*************************************************************************************************
!
!  End of New Rough Subroutine
!
!*************************************************************************************************
END SUBROUTINE BedRough
!*************************************************************************************************!





!*************************************************************************************************
!
!  Subroutine        : BedRoughInitiate
!  Purpose           : Initiates the solution system so that the bed form roughness updates can 
!                    : be successfully undertaken
!  Author            : David Wainwright based on direction from Dean Patterson.
!  Initiation Date   : 19/01/04
!
!*************************************************************************************************
SUBROUTINE BedRoughInitiate(NumNodes,wbm_Initiated,wbm_MannTrans,wbm_NodeCounter,wbm_IT, wbm_MannTransOld, wbm_BedHeight)
IMPLICIT NONE
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INPUTS
!
!*************************************************************************************************
LOGICAL(4), INTENT(INOUT)               :: wbm_Initiated       
real(8), ALLOCATABLE, INTENT(INOUT)    :: wbm_MANNTRANS(:)
real(8), ALLOCATABLE, INTENT(INOUT)    :: wbm_MANNTRANSOLD(:)
real(8), ALLOCATABLE, INTENT(INOUT)    :: wbm_BedHeight(:)
integer, INTENT(INOUT)              :: wbm_NodeCounter
integer                             :: NumNodes
integer, INTENT(INOUT)              :: wbm_IT
!*************************************************************************************************
!
!     Body of Routine
!
!*************************************************************************************************
ALLOCATE(wbm_MANNTRANS(NumNodes))
ALLOCATE(wbm_MANNTRANSOLD(NumNodes))
ALLOCATE(wbm_BedHeight(NumNodes))
wbm_MANNTRANS = 0.00
wbm_MANNTRANSOLD = 0.00
wbm_BedHeight = 0.00
wbm_Initiated = .TRUE.
wbm_NodeCounter = 0
wbm_IT = 0
WRITE(*,*) "In Routine"
!*************************************************************************************************
!
!  End of Bed Rough Initiate Subroutine
!
!*************************************************************************************************
END SUBROUTINE BedRoughInitiate
!*************************************************************************************************
!
!  Subroutine        : wbm_AssignInits
!  Purpose           : Sets Initial Values for Sediment, Temperature and Salinity
!  Author            : David Wainwright
!  Initiation Date   : 20/07/04
!
!
!  The Initial Version of this code assumes that the following columns and formats are in the MID file:
!     A3 - Specifier indicating whether it is a corner, midside or element
!     I5 - Providing for Node or Element Number
!     F20.5 - X Coordinate of Node
!     F20.5 - Y Coordinate of Node
!     F10.10 - Level of Node
!     F10.10 - Salinity at Node
!     F10.10 - Temperature at Node
!     F10.10 - Sediment Concentration at Node
!
!     All Items are comma delimited
!
!*************************************************************************************************
SUBROUTINE wbm_AssignInits
IMPLICIT NONE
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INPUTS
!
!*************************************************************************************************
!NiS,jun06: Changes for usage with Lahey
!Character(500)          :: Line
Character(500)          :: Line, temp_line
!-
integer              :: CurrentNode
integer             :: i
integer             :: StartPos
integer             :: EndPos
Character(50)           :: LineParts(10)
!NiS,jun06: Changes for usage with Lahey
INTEGER :: istat
!-
!*************************************************************************************************
!
!     Body of Routine
!
!*************************************************************************************************
!
!  Opens File
!
!*************************************************************************************************
OPEN (3300, File = wbm_InitConsFile)
!
!  Loops Through Each Line and assigns to array
!
!NiS,jun06: Changes for usage with Lahey
!Do While ( .NOT. EOF(3300))
!  Read (3300,'(a)') Line
reading: Do

  Read (3300,'(a)', IOSTAT=istat) temp_line
  if (istat /= 0) EXIT reading

  Line = temp_line
!-

  If (Line(2:3) /= 'GE ') Then
     StartPos = 1
     EndPos = 1
     Do i = 1, 8
       EndPos = INDEX(Line,',')-1
       LineParts(i) = Line(StartPos:EndPos)
!       
!  Trims Line Down       
!       
       Line = Line(EndPos+2:500)
     End Do
     LineParts(i-1) = Line
     Read(LineParts(2),*) CurrentNode
     Read(LineParts(6),*) wbm_newConditions(4,CurrentNode)
     Read(LineParts(7),*) wbm_newConditions(5,CurrentNode)
     Read(LineParts(8),*) wbm_newConditions(6,CurrentNode)
     Read(LineParts(8),*) wbm_newConditions(7,CurrentNode)
  End If
!NiS,jun06: Changes for usage with Lahey
End Do reading
!-
Close (3300)
!*************************************************************************************************

!*************************************************************************************************
!
!  End of Initial Conditions from mif file routine
!
!*************************************************************************************************
END SUBROUTINE wbm_AssignInits
!*************************************************************************************************

!*************************************************************************************************
!
!  Subroutine        : wbm_InitScourLims
!  Purpose           : Sets a scour limit level for each node from a .mif file
!  Author            : David Wainwright
!  Initiation Date   : 20/07/04
!
!
!  The Initial Version of this code assumes that the following columns and formats are in the MID file:
!     A3 - Specifier indicating whether it is a corner, midside or element
!     I5 - Providing for Node or Element Number
!     F20.5 - X Coordinate of Node
!     F20.5 - Y Coordinate of Node
!     F10.10 - Level of Node
!     F10.10 - Ultimate Scour Level of Node
!
!     All Items are comma delimited
!
!*************************************************************************************************
SUBROUTINE wbm_InitScourLims(NumNodes,wbm_ScourLims)
IMPLICIT NONE
!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - INPUTS
!
!*************************************************************************************************
real(8), ALLOCATABLE, INTENT(INOUT)    :: wbm_ScourLims(:)
integer                             :: NumNodes

!*************************************************************************************************
!
!     VARIABLE DECLARATIONS - LOCAL VARIABLES
!
!*************************************************************************************************
!NiS,jun06: Changes for usage with Lahey
!Character(500)          :: Line
Character(500)          :: Line, temp_line
!-
integer              :: CurrentNode
integer             :: i
integer             :: StartPos
integer             :: EndPos
Character(50)           :: LineParts(10)
real(8)                :: CurrentBedLevel
!NiS,jun06: Changes for usage with Lahey
INTEGER :: istat
!-
!*************************************************************************************************
!
!     Body of Routine
!
!*************************************************************************************************
!
!     Dimensions Array and sets large scour depth
!
!*************************************************************************************************
ALLOCATE(wbm_ScourLims(NumNodes))
wbm_ScourLims = -999999.99
!*************************************************************************************************
!
!  Opens File
!
!*************************************************************************************************

OPEN (3301, File = wbm_ScourLimFile)
!
!  Loops Through Each Line and assigns to array
!
!NiS,jun06: Changes for usage with Lahey
!Do While ( .NOT. EOF(3301))
!  Read (3301,'(a)') Line
reading: Do
  Read (3301,'(a)',IOSTAT=istat) temp_line
  if (istat/=0) exit reading

  Line = temp_line
!-

  If (Line(2:3) /= 'GE ') Then
     StartPos = 1
     EndPos = 1
     Do i = 1, 6
       EndPos = INDEX(Line,',')-1
       LineParts(i) = Line(StartPos:EndPos)
!       
!  Trims Line Down       
!       
       Line = Line(EndPos+2:500)
     End Do
     LineParts(i-1) = Line
     Read(LineParts(2),*) CurrentNode
     Read(LineParts(5),*) CurrentBedLevel
     Read(LineParts(6),*) wbm_ScourLims(CurrentNode)
! converts to an available thickness
     wbm_ScourLims(CurrentNode)  = CurrentBedLevel - wbm_ScourLims(CurrentNode) 
  End If
!NiS,jun06: Changes for usage with Lahey
End Do reading
!-
Close (3301)
!*************************************************************************************************

!*************************************************************************************************
!
!  End of Initial Conditions from mif file routine
!
!*************************************************************************************************
END SUBROUTINE wbm_InitScourLims
!*************************************************************************************************







!*************************************************************************************************
!
!  End of Module WBMMODS
!
!*************************************************************************************************
END MODULE WBMMODS
!*************************************************************************************************
