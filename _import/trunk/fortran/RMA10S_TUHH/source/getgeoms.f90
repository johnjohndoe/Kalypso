module mod_getgeo
contains
!******************************************************************************************
!  subroutine getgeo.sub generates the Finite Element mesh. It calls the reading subroutine
!    and sets up some general parameters like
!     - flow direction fixitation at 1D/2D transitions
!     - mesh size
!     - etc.
!
!  Besides this subroutine is supposed to partly generate the 3D mesh. These options are
!    deactivated. Developers can reactivate it for later use.
!
!  It calls the following subroutines
!   - rdkalypso
!   - Marsh
!   - ginpt
!   - ErrorMessageAndStop
!
!******************************************************************************************
SUBROUTINE GETGEO (m_SimModel)

use mod_Model
use mod_fileHandler
use mod_RDKalypso_routines

USE BLK10MOD, only: &
&  ifile, lout, lin, icfl, id, dlin,       &
&  maxp, maxe, maxlt, maxt,                &
&  np, ne, le, lp, npm, nem,               &
&  ncrn, ncorn, ncn,                       &
&  ncl, line, lmt, translines,             &
&  netyp, jpoint,                          &
&  nop, nops, imat, th, alfak,             &
&  cord, ao, aorig,                        &
&  width, wids, ss1, ss2, chez, zmann,     &
&  elev, hmin, hmnn,                       &
&  vel,                                    &
&  xscale, yscale, zscale,                 &
&  roavg, den, tempi, grav, igf,           &
&  ndp, ndep, fileControl, StorageElts
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!     +                    thlay, umin, pwerin, nfix, edd1, edd2, edd3, thl, vmin, power,
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!meaning of the variables
!------------------------
!IFILE   model geometry (in *.2d-format)
!LIN     unit of control file (control.r10)
!LOUT    the general output unit (.out-file)
!TODO    examine, how this works
!icfl    unit number definition of the console
!id      character variable into which the first 8 digits of any input line from a file are read
!dlin    character variable into which the last 72 digits of any input line from a file are read
!maxp    maximum number of points (surface points; includes midsides)
!maxe    maximum number of elements
!maxlt   maximum number of 1D/2D transition elements for 1D/2D line-2-element transitions
!maxt    maximum number of 1D/2D transition elements for 1D/2D element-2-element transitions
!ne      maximum element number
!le      minimum element number
!np      maximum node number
!lp      minimum node number
!ncorn   number of nodes per element
!ncrn    copy of the ncorn array
!ncn     local copy of the number of nodes in the current element
!ncl     number of continuity lines
!line    continuity line definition; stores all the nodes 
!lmt     number of nodes within continuity line
!translines data of transition lines; transitioning line, 1D-element, and node
!netyp   determines the sort of an element (1D-type, 2D-type, 3D-type, transition-type)
!jpoint  determines connections of nodes in 1D/2D and 2D/3D-vertical elements
!nop     nodes of an element, anticlockwise (corner - midside - sequence)
         !TODO:  Are nops and nop both necessary?
!nops    the same as nop
!imat    material type of an element
!        TODO: What is the main axis?
!th      general direction angle of an element; determined by the main axis
!alfak   Fixed flow direction angle at a certain node; Value 0.0 zero means, that flow direction not restricted
!        TODO: What actually is the 3rd coordinate? Is it the same as ao
!cord    x-, y- and ?-coordinates of a node
!ao      bed elevation of a node
!aorig   original value of the bed elevation of a node; necessary for bed evolution calculations
!width   Channel bed width of a 1D-node within the trapezoidal geometry approach
!wids    storage width (flood plain width) of a 1D-node within the trapezoidal geometry approach
!ss1     1st Channel side slope of a 1D-node within the trapezoidal geometry approach
!ss2     2nd Channel side slope of a 1D-node within the trapezoidal geometry approach
!chez    (optional) Chezy-coefficient of an element
!zmann   (optional) Manning's n coefficient of an elements
!elev    Initial elevation at not restarted simulation startup; defined in control.r10
!hmin    3 meanings
!        positive value - minimum water depth at not restarted simulation startup, defined in control.r10
!        zero value     - no minimum water depth applied at the beginning; if elev is less than ao at a certain node, the node becomes dry at startup
!        negative value - all water depth values ar overwritten with the absolute of the negative hmin
!hmnn    Actually a working copy of hmin for internal purposes to identify the method, that should be applied for hmin
!vel     The degree of freedom field; it's the key array
!        1 - x-velocity
!        2 - y-velocity
!        3 - water depth
!        4 - salinity
!        5 - temperature
!        6 - suspended sediment
!        TODO: what is vel(7,x)
!        7 - ??? perhaps water column potential by element
!xscale  scaling of all coordinates in x-direction by this factor
!yscale  scaling of all coordinates in y-direction by this factor
!zscale  scaling of all bed-coordinates in z-direction by this factor
!roavg   average density of the fluid
!den     density array for each node
!tempi   input temperature (from input file; is used here to determine the average density)
!grav    gravitational constant
!igf     switch unit system
!        1 - American units
!        2 - SI units
!ndp     control for usage of layer definition; 3D application
!        0 - no layers - no 3D
!        1 - definition at suface nodes (line LD1)
!       -1 - definition at corner nodes (line LD2)
!        2 - definition at corner nodes (line LD3)
!       -N - number of layers to be applied is N
!ndep    number of layers per nodes; that means depending nodes on original 2D-node
USE BLK11MOD, only: ort, chi, npm11
USE BLKSUBMOD, only: whgt, wlen, transel
use parakalyps, only: mcord
!meaning of the variables
!------------------------
!ort:     material type's parameters (18 or something)
!         TODO: What is chi?
!chi:     constant parameter
!npm11:   copy of npm
!         TODO: Determine the weir data
!whgt:    what sort of weir data?
!wlen:    what sort of weir data?
!transel: what sort of weir data?
!mcord:   centre coordinate of an element
!transel: what sort of weir data?
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!      USE BLKDRMOD, only: akp, ado, adt, adb
!      USE PARAMMOD, only: nlaymx
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------

implicit none
save

!arguments
type (simulationModel), pointer :: m_SimModel

!local variables
!---------------
!NiS,jul06: Consistent data types for passing parameters
integer :: n, m, a, lt, dummy1, dummy2, dummy3, dummy4, dummy5
integer (kind = 4) :: k, i, j
integer (kind = 4) :: na, n1, n2, n3
integer (kind = 4) :: immt, translength
real (kind = 8) :: a1, a2, a3
real (kind = 8) :: void
real (kind = 8) :: watt, sl, sr, ws
real (kind = 8) :: ang, widto, alfak_temp
real (kind = 8) :: twht, twln, trael
real (kind = 8) :: dx, dy
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!integer (kind = 4) :: nts, tklay, l
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!meaning of the variables
!------------------------
!        TODO: Determine the meaning of the local variables
!n:      number of nodes; given back from rdkalypso.sub
!m:      number of elements; given back from rdkalypso.sub
!a:      number of arcs; given back from rdkalypso.sub
!lt:     number of linetransitions; given back from rdkalypso.sub
!dummy1, dummy2, dummy3: dummy arguments to overgive parameters to rdkalypso in a correct way;
!        no meaning at geometry set up
!k, i, j:counters
!na:     local copy of a continuity line's length
!n1, n2, n3: local copies of node numbers
!immt:   local copy of an element's material type
!translength: geometric length of the chord of a 1D/2D element-2-line transition
!
!a1,a2,a3:coefficients for the calculation of the average density utilizing the temperature
!void:   A very small number -1.0e-20;
!watt:   read-in parameter - Channel bed width of a 1D-node within the trapezoidal geometry approach
!ws:     read-in parameter - storage width (flood plain width) of a 1D-node within the trapezoidal geometry approach
!sl:     read-in parameter - 1st (left) Channel side slope of a 1D-node within the trapezoidal geometry approach
!sr:     read-in parameter - 2nd (right) Channel side slope of a 1D-node within the trapezoidal geometry approach
!        read-in parameter - 
!ang:    an angle calculation
!widto:  copy of the channel width of a 1D-node within the trapezoidal geometry approach; storing to write out old and
!        new value after adaptation at 1D/2D element-2-element transitions
!alfak_temp: local working copy of a velocity direction fix; used to generate the direction at 1D/2D element-2-element transitions
!        TODO: What weir data is read in? Exact description!
!twht:   read-in parameter - weir data
!twln:   read-in parameter - weir data
!trael:  read-in parameter - weir data
type (StorageElement), pointer :: localStorElt(:) => null()


DATA VOID/-1.E20/
DATA A1,A2,A3/1.939938,5.588599E-5,-1.108539E-5/


!dummy initialisation
!--------------------
dummy1 = 1
dummy2 = 1
dummy3 = 1

!some informational output
!-------------------------
WRITE(*,*)' IFILE= ', IFILE
WRITE(*,*)' MaxP= ', MAXP
WRITE(*,*)' MaxE= ', MAXE

!read the geometry from the file
!-------------------------------
!TODO: Why is there a check, whether the geometry file is present?
if (ifile /= 0) call rdkalyps (n, m, a, lt, dummy1, dummy2, dummy3, dummy4, dummy5, 0, m_SimModel)

!write read continuity lines to the output file (*.out)
!------------------------------------------------------
!reading of continuity line block is positioned before geometry is read, because lines are needed
!for the geometry, if there was any line transition
IF (NCL > 0) THEN
  !header
  WRITE (LOUT, 6120)
  !the lines
  throughLines: DO J = 1, NCL
    !number of nodes within line j
    NA = LMT (J)
    !write out line's nodes
    IF (NA > 0) WRITE (LOUT, *) J, (LINE (J, K), K = 1, NA)
  enddo throughLines
ELSE
  !write information, there is no CCL
  WRITE(LOUT,6115)
ENDIF


!read element data from the input file
!-------------------------------------
readELdata: do
  !if there is data, read and evaluate it
  if(ID(1:2) .EQ. 'EL') then
    !read data for the element
    read(DLIN,5040) J,(NOP(J,K),K=1,8),IMAT(J),TH(J)
    !read next input file line
    call ginpt(lin,id,dlin)
  !otherwise leave the loop
  else
    exit readELdata
  endif
enddo readELdata

!Assign roughness parameters to elements
!---------------------------------------
AssignRoughnesses: DO I = 1, MAXE
  !nis,nov08: bugfix - elements with negative material types are just dry and not
  !           inoperative. They should be considered at roughness type assignment.
  !           So changed imat(i)>0 into imat(i)/=0.
  IF (IMAT (I) /= 0) THEN
    !copy the nodes
    DO J = 1, 8
      NOPS (I, J) = NOP (I, J)
    ENDDO

    !local copy of the (dry) material type
    IMMT = abs (IMAT (I))
    !get the last 2 digits to obtain the raw material type 
    J = MOD (IMMT, 100)
    !control structures: overgive the original type again with its own roughness
    IF (IMMT > 903) J = imat (i)
    !1D/2D element-2-element transitions: overgive the original type again with its own roughness
    if (NOP (i, 6) == 0) J = imat (i)

    !skip polynomial approach elements (immt = 89)
    if (immt /= 89 .and. immt < 900) then
      !assign Chezy-coefficient, if the parameter > 1.0
      if (ORT (J, 5) > 1.) then
        CHEZ (I) = ORT (J, 5)
      !assign Manning's N, if the parameter is between 0.0 and 1.0
      elseif (0.0 < ORT (J, 5) .and. ORT (j, 5) <= 1.0) then
        ZMANN (I) = ORT (J, 5)
      !continue on Darcy-Weisbach equation utilisation
      elseif (ort (j, 5) == -1.0) then
        continue
      !if parameter was defined wrong, stop on ERROR
      !reading of data block should control, that it doesn't happen like this
      else
        call ErrorMessageAndStop (1209, i, mcord(i,1), mcord(i,2))
      ENDIF
    ENDIF
  ENDIF
ENDDO AssignRoughnesses

!Examine the mesh dimensions
!----------------------------------
! - maximum element number (ne)
! - minimum element number (le)
! - maximum node number (np)
! - minimum node number (lp)
! - number of corner nodes per element (ncorn, ncrn)
NP = 0
FindMaxEltNumbers: do J = 1, MAXE
  !nis,sep07: bugfix - elements with negative material types are just dry and not
  !           inoperative. They should be considered at roughness type assignment.
  !           So changed imat(i)>0 into imat(i)/=0.
  if (imat (j) /= 0) then
    !remember highest element number occuring
    NE = J
    !remember lowest element number occuring
    if (J < LE) LE = J
    
    NCORN (J) = 0
    !remember the number of corner nodes of the element j
    ExamineElements: do K = 1, 20
      !stop evaluating the 
      if (NOP (J, K) == 0) exit ExamineElements
      !remember the highest node number
      if (NOP (J, K) > NP ) NP = NOP(J,K)
      !remember the lowest node number
      if (NOP (J, K) < LP ) LP = NOP(J,K)
      NCORN (J) = k
    enddo ExamineElements
    !count occurances of element-2-element transitions
    if (ncorn(j) == 5) MaxT = MaxT + 1
    !copy number of corner nodes
    NCRN (J) = NCORN (J)
  endif
enddo FindMaxEltNumbers



!Establish preliminary element types
!-----------------------------------
EstablishPreliminaryEltType: DO J = 1, MAXE
  !for operative elements (dry or wet)
  if (imat (j) /= 0) then
    !covers all 2D elements
    !covers junction elements, if more than 5 strands come together
    IF (NCORN (J) > 5) THEN
      NETYP (J) = 16
    !covers junction elements (901, 902, 903) with less or equal 5 strands coming together
    !  can't reach junctions with more than 5 strands coming together, because
    !  they're catched above
    !covers control structure elements (904+)
    ELSEIF (IMAT (J) > 900) THEN
      NETYP (J) = 17
    !Insert pointers to inform about junction elements
    !reference 3rd node at 1st one
    JPOINT (NOP (J, 1)) = NOP (J, 3)
    !reference 1st node at 3rd one
    JPOINT (NOP (J, 3)) = NOP (J, 1)
    !reference 3rd node at 2nd one
    JPOINT (NOP (J, 2)) = NOP (J, 3)
    !covers 1D/2D element-2-element transition element
    ELSEIF (NCORN (J) > 3) THEN
      NETYP(J)=18
    !covers all normal 1D flow elements
    ELSE
      NETYP(J)=6
    ENDIF
  ENDIf
enddo EstablishPreliminaryEltType


!copy highest mesh numbers
!-------------------------
NPM = NP
NEM = NE

!write console output about the mesh size
!TODO: Examine, how unit=6 can be the console output!
WRITE (icfl, 6210) NE, LE, NP, LP

!Read cord data for nodes from input file
!----------------------------------------
readNDdata: do
  IF (ID (1:2) == 'ND') THEN
    !evaluate line        
    READ (DLIN, 5035) J, (CORD (J, K), K = 1, 2), AO (J)
    !read next input file line
    call ginpt(lin,id,dlin)
  else
    exit readNDdata
  ENDIF
enddo readNDdata


!Preliminary initialisation of VEL(3,  ) for BLINE
!-------------------------------------------------
!TODO: Calculation of preliminary water depth is very unprecise
PrelimVel3Init: DO J = 1, NPM
  !calculation of water depth as difference between initial water elevation and bed elevation
  VEL (3, J) = ELEV - AO (J)
  !if water depth is less than given minimum water depth, then adapt the water depth
  !hmin = 0.0 is not considered; in advance changed to -1.0e+20
  IF (VEL (3, J) < HMIN) VEL (3, J) = HMIN
  !if user gives hmin less then zero, all water depth will be overwritten with the absolute value of that
  IF (HMNN < 0.) VEL (3, J) = -HMNN
  !TODO: Because of adaptation, there need to be 2 variables for hmin in following constellations:
  !      hmin > 0.0; hmnn = hmin
  !      hmin = -1.0e+20; hmnn = 0.0
  !      hmin < 0.0; hmnn = hmin
enddo PrelimVel3Init


!Enter width values for one-dim nodes
!------------------------------------
Profile1Ddata: do
  if (ID (1:3) == 'WD ') then
  read (DLIN, 5044) J, WATT, SL, SR, WS
  WIDTH (J) = WATT
  WIDS (J) = WS
  SS1 (J) = SL
  SS2 (J) = SR
  !read next input file line
  call ginpt(lin,id,dlin)
else
  exit Profile1Ddata
endif
enddo Profile1Ddata


!enter data for weirs
!--------------------
Weirdata: do
  IF (ID (1:3) == 'WDT') THEN
    READ (DLIN, 5044) J, TWHT, TWLN, TRAEL
    !read data, if element number is correct
    IF (J > 0) THEN
      WHGT (J) = TWHT
      WLEN (J) = TWLN
      TRANSEL (J) = TRAEL
    !give ERROR on wrong weir element number
    ELSE
      call ErrorMessageAndStop (1210, 0, 0.0d0, 0.0d0)
    ENDIF
    !control output to console and control file
    WRITE (LOUT, 6150) J, WHGT (J), WLEN (J), TRANSEL (J)
    WRITE (  75, 6150) J, WHGT (J), WLEN (J), TRANSEL(J)
 6150   FORMAT(/'WEIR ELEVATION, SECTION LENGTH AND TRANSEL FOR NODE',I6,' =',3F12.3)
    !read next input file line
    call ginpt(lin,id,dlin)
    !increase counter
  else
    exit Weirdata
  ENDIF
enddo Weirdata

!Read Volume Waterlevel Relationships of Storage Elements
!--------------------------------------------------------

if (associated (fileControl.volWlFil)) then
  localStorElt => StorageElts
  call readVolumeWaterlevelFile (fileControl.volWlFil, localStorElt)
endif 



!Adapt position of transitioning nodes at 1D/2D element-2-element transitions
!-------------------------------------------------------------------------
DO J = 1, NE
  !nis,nov08: bugfix - use abs(imat) to check for special elements and use imat(j)/=0
  !           instead of imat(j)>0, because dry elements should be also considered.
  IF (IMAT (J) /= 0 .AND. abs (IMAT (J)) < 901) THEN
    !copy corner nodes
    NCN = NCORN (J)
    !Test for 1d - 2d transition
    IF (NCN == 5) THEN
      !left 2D-arc's node
      N1 = NOP (J, 4)
      !transitioning node
      N2 = NOP (J, 3)
      !right 2D-arc's node
      N3 = NOP (J, 5)
      !recalculate the be elevation, because it must be a linear bed shape
      AO (N2) = 0.5 * (AO (N1) + AO (N3))
      !move the coordinates into the middle of the transitioning arc, if there were no coordinates yet.
      IF (CORD (N2, 1) <= VOID) THEN
        !x-coordinate
        CORD (N2, 1) = 0.5 * (CORD (N1, 1) + CORD (N3, 1))
        !y-coordinate
        CORD (N2, 2) = 0.5 * (CORD (N1, 2) + CORD (N3, 2))
        !x-distance of arc
        dx = CORD (N1, 1) - CORD (N3, 1)
        !y-distance of arc
        dy = CORD (N1, 2) - CORD (N3, 2)
        !length of arc
        WIDTH (N2) = SQRT (dx**2 + dy**2)
      ENDIF
    ENDIF
  ENDIF
ENDDO


!Scale coordinates
!-----------------
ScaleCoordinates: DO J = 1, NP
  CORD (J, 1) = CORD (J, 1) * XSCALE
  CORD (J, 2) = CORD (J, 2) * YSCALE
  AO (J) = AO (J) * ZSCALE
enddo ScaleCoordinates

!set up average density for unit system
!--------------------------------------
!calculate the average density of the fluid by a function depending on the input temperature
!roavg = 1.939938 + 5.588599E-5 * T - 1.108539E-5 * T**2
ROAVG = A1 + A2 * TEMPI + A3 * TEMPI**2
!SI-units (metric units)
IF (GRAV < 32.) THEN
  ROAVG = ROAVG * 516.
  write(*,*) roavg
  IGF = 2
!american units
ELSE
  CHI = CHI/ 239.87
ENDIF

!Set up Marsh-parameters for each node
!-------------------------------------
CALL MARSH

!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!set up 3D-mesh
!--------------
!
!DEACTIVATE FOR RMA·KALYPSO
!nis,nov08: The following part is for the setting up the 3D-mesh, basing on the layer control
!           The layer control (NDP) is switch of and now be set to zero by default!
if (ndp /= 0) ndp = 0
!C-
!C......Read number of layers at each node for 3d cases
!C-
!        IF(NDP .NE. 0) THEN
!          IF(ID(1:2) .EQ. 'VD') THEN
!CIPK SEP96 CHANGE TO READ EDD1 SEPARATELY
!            READ(DLIN,5021) VMIN,POWER,UMIN,PWERIN
!            WRITE(LOUT,6040) VMIN,POWER,UMIN,PWERIN
!            WRITE(*,*) 'FOUND VMIN'
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!            call ginpt(lin,id,dlin)
!cipk sep96 add variable distribution of edd1's etc
!            WRITE(LOUT,6041)
! 113        IF(ID(1:2) .EQ. 'TD') THEN
!              READ(DLIN,'(I8,3F8.0)') K,EDD1(K),EDD2(K),EDD3(K)
!              WRITE(LOUT,6042) K,EDD1(K),EDD2(K),EDD3(K)
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!              call ginpt(lin,id,dlin)
!              GO TO 113
!            ENDIF
! 6040 FORMAT(///9X,'BOUNDARY CONDITION FUNCTIONAL DISTRIBUTION'//
!     1'  EXTERNAL BOUNDARIES'/
!     25X,'MINIMUM VELOCITY',T22, F9.4/5X,'POWER FUNCTION',T21,F10.4//
!     3'  INTERNAL BOUNDARIES'/
!     45X,'MINIMUM VELOCITY',T22, F9.4/5X,'POWER FUNCTION',T21,F10.4//)
! 6041       FORMAT(
!     +      10X,'PARAMETERS FOR VERTICAL EDDY COEFFICIENTS'//
!     +      5X,'MAT NO   EDDY1            EDDY2            EDDY3'//)
! 6042 format(i10,3f12.4)
!          ELSE
!C            WRITE(*,*) 'LINE TYPE -VD- NOT FOUND'
!C            WRITE(*,*) 'EXECUTION STOPPED'
!C            STOP
!             VMIN=1.
!             POWER=1.
!             UMIN=1.
!             PWERIN=1.
!             WRITE(LOUT,*) 'DEFAULT VD VALUES USED'
!             WRITE(*,*) 'DEFAULT VD VALUES USED'
!          ENDIF
!          IF(NDP .LT. 0) THEN
!C-
!C      NDP .lt. -1 says use this value globally
!C-
!            IF(NDP .LT. -1) THEN
!              DO 814 J=1,NP
!                NDEP(J) = -NDP-1
!  814         CONTINUE
!
!            ELSE
!C-
!C      NDP = -1 says read values by node with weighting factors
!C-
!  815         CONTINUE
!              WRITE(*,'(1X,A3)') ID
!              IF(ID(1:3) .EQ. 'LD2') THEN
!                READ(DLIN,'(2I8)') J,NTS
!cipk feb99
!cWP Feb 2006, Change NLAYM to NLAYMX
!                if(nts .gt. nlaymx) then
!CIPK SEP04 CREATE ERROR FILE
!  	            CLOSE(75)
!                  OPEN(75,FILE='ERROR.OUT')
!
!cWP Feb 2006, Change NLAYM to NLAYMX	  
!                 WRITE(75,*) 'Too many layers Increase NLAYMX in PARAM.'
!     +            ,'COM'
!                 WRITE(*,*) 'Too many layers Increase NLAYMX in PARAM.C'
!     +            ,'OM'
!                  stop 'Too many layers defined'
!                endif
!
!                N2=NTS
!                IF(N2 .GT. 7) N2=7
!                READ(DLIN,'(2I8,7F8.0)') J,NTS,(THL(N),N=1,N2)
!                IF(NTS .GT. 7) THEN
!cipk jan99                  N1=-2
!                  N1=-1
! 8151             N1=N1+9
!                  N2=N1+8
!CIPK JAN99
!                  IF(N2 .GT. NTS) N2=NTS
!
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!                  call ginpt(lin,id,dlin)
!                  IF(ID(1:4) .EQ. 'LD2A') THEN 
!                    READ(DLIN,'(9F8.0)') (THL(N),N=N1,N2)
!                    IF(NTS .GT. N2) GO TO 8151
!                  ENDIF
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!                  call ginpt(lin,id,dlin)
!                ELSE
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!                  call ginpt(lin,id,dlin)
!                ENDIF
!C-
!C      J > MAXP skips out
!C-
!                IF(J .GT. MAXP) GO TO 819
!C-
!C      Test for limit violation on MLAY parameter
!C-
!
!cWP Feb 2006, Change MLAY to NLAYMX
!                IF (J .GT. NLAYMX)  THEN
!CIPK SEP04 CREATE ERROR FILE
!  	            CLOSE(75)
!                  OPEN(75,FILE='ERROR.OUT')
!                  WRITE(*,*)  ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
!                  WRITE(75,*) ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
!                  STOP
!                ENDIF
!CIPK JAN99 ALLOW FOR J=0
!                IF(J .EQ. 0) THEN
!                  DO J=1,NP
!
!                    DO  N=1,NTS
!                      THLAY(J,N)=THL(N)
!                      IF(THL(N) .LE. 0.) THLAY(J,N)=1.0
!                    ENDDO
!                    NDEP(J)=NTS
!                  ENDDO
!                  GO TO 815
!                ENDIF
!
!                DO  N=1,NTS
!                  THLAY(J,N)=THL(N)
!                  IF(THL(N) .LE. 0.) THLAY(J,N)=1.0
!                ENDDO
!                NDEP(J)=NTS
!                GO TO 815
!              ENDIF
!              GO TO 819
!            ENDIF
!C-
!C      NDP = 1  Read a complete block of values for all 2d nodes
!C-
!          ELSEIF(NDP .EQ. 1) THEN
!            IF(ID(1:3) .EQ. 'LD1') THEN
!              READ(DLIN,5015) (NDEP(J),J=1,9)
!              N1=1
!  816         N1=N1+9
!              N2=N1+8
!              IF(N2 .GT. NP) N2=NP
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!              call ginpt(lin,id,dlin)
!              IF(ID(1:4) .EQ. 'LD1') THEN
!                READ(DLIN,5015) (NDEP(J),J=N1,N2)
!                GO TO 816
!              ENDIF
!            ELSE
!CIPK SEP04 CREATE ERROR FILE
!  	        CLOSE(75)
!              OPEN(75,FILE='ERROR.OUT')
!              WRITE(*,*) 'ERROR -- NO LD1 LINES IN DATA SET'
!              WRITE(75,*) 'ERROR -- NO LD1 LINES IN DATA SET'
!              STOP
!            ENDIF
!C-
!C      NDP = 2  Read layer data with elevations set
!C-
!          ELSEIF(NDP .EQ. 2) THEN
!  817       IF(ID(1:3) .EQ. 'LD3') THEN
!              READ(DLIN,'(2I8)') J,NTS
!cipk feb99
!cWP Feb 2006, Change NLAYM to NLAYMX
!                if(nts .gt. nlaymx) then
!CIPK SEP04 CREATE ERROR FILE
!  	            CLOSE(75)
!                  OPEN(75,FILE='ERROR.OUT')
!                 WRITE(75,*) 'Too many layers Increase NLAYMX in PARAM.'
!     +            ,'COM'
!                 WRITE(*,*) 'Too many layers Increase NLAYMX in PARAM.C'
!     +            ,'OM'
!
!                  stop 'Too many layers defined'
!                endif
!
!              N2=NTS
!              IF(N2 .GT. 7) N2=7
!              READ(DLIN,'(2I8,7F8.0)') J,NTS,(THL(N),N=1,N2)
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!              call ginpt(lin,id,dlin)
!              IF(NTS .GT. 7) THEN
!CIPK JAN99                N1=-2
!                N1=-1
!  818           N1=N1+9
!                N2=N1+8
!CIPK JAN99
!                IF(N2 .GT. NTS) N2=NTS
!                IF(ID(1:4) .EQ. 'LD3A') THEN 
!                  READ(DLIN,'(9F8.0)') (THL(N),N=N1,N2)
!cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
!                  call ginpt(lin,id,dlin)
!                  IF(NTS .GT. N2) GO TO 818
!                ENDIF
!              ENDIF
!              IF(J .EQ. 0) WRITE(LOUT,6007) (THL(N),N=1,NTS)
!C
!C    Permit reading of a single list of depths
!C    when the node number is read in as zero.  A 15% margin is
!C    applied at the bottom element to avoid small elements.
!C
!              IF(J .EQ. 0) THEN
!                DO J=1,NPM
!C-
!C    Test for NTS =0
!C
!                  IF(NTS .GT. 0) THEN
!                    DO N=1,NTS
!                      IF(N .EQ. 1) THEN
!                        IF(N .EQ. NTS) THEN
!                          THLAY(J,1)=1.0
!                          NDEP(J)=1
!                          GO TO 8171
!                        ENDIF
!                        TKLAY=ELEV-THL(N)
!                        IF(ELEV-TKLAY*1.15 .LT. AO(J)) THEN
!                          THLAY(J,1)=1.0
!                          NDEP(J)=1
!                          GO TO 8171
!                        ELSE
!                          THLAY(J,N)= (ELEV-THL(N))/(ELEV-AO(J))
!                        ENDIF
!                      ELSEIF(N .EQ. NTS) THEN
!                        TKLAY= THL(N-1)-AO(J)
!                        THLAY(J,N)=TKLAY/(ELEV-AO(J))
!                        NDEP(J)=N
!                        GO TO 8171
!                      ELSE
!                        TKLAY=THL(N-1)-THL(N)
!                        IF(THL(N-1)-TKLAY*1.15 .LT. AO(J)) THEN
!                          TKLAY= THL(N-1)-AO(J)
!                          THLAY(J,N)=TKLAY/(ELEV-AO(J))
!                          NDEP(J)=N
!                          GO TO 8171
!                        ELSE
!                          THLAY(J,N)= (THL(N-1)-THL(N))/(ELEV-AO(J))
!                        ENDIF
!                      ENDIF
!                    ENDDO
!                  ELSE
!                    NDEP(J)=0
!                  ENDIF
! 8171             CONTINUE
!                ENDDO
!                GO TO 817
!C
!C     Process individual nodal values
!C
!              ELSEIF(J .LE. MAXP) THEN
!
!cWP Feb 2006, Change MLAY to NLAYMX
!                IF (J .GT. NLAYMX)  THEN
!CIPK SEP04 CREATE ERROR FILE
!  	            CLOSE(75)
!                  OPEN(75,FILE='ERROR.OUT')
!                  WRITE(*,*)  ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
!                  WRITE(75,*) ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
!                  STOP
!                ENDIF
!                IF(J .GT. 0) THEN
!                  DO  N=1,NTS
!                    IF(N .EQ. 1) THEN
!                      IF(N .EQ. NTS) THEN
!                        THLAY(J,1)=1.0
!                        NDEP(J)=1
!CIPK JUL01
!                          GO TO 817
!                      ELSE
!                        TKLAY=ELEV-THL(N)
!                        IF(ELEV-TKLAY*1.15 .LT. AO(J)) THEN
!                          THLAY(J,1)=1.0
!                          NDEP(J)=1
!CIPK JUL01
!                          GO TO 817
!                        ELSE
!                          THLAY(J,N)= (ELEV-THL(N))/(ELEV-AO(J))
!                        ENDIF
!                      ENDIF
!                    ELSEIF(N .EQ. NTS) THEN
!                      TKLAY= THL(N-1)-AO(J)
!                      THLAY(J,N)=TKLAY/(ELEV-AO(J))
!                      NDEP(J)=N
!CIPK JUL01
!                          GO TO 817
!                    ELSE
!                      TKLAY=THL(N-1)-THL(N)
!                      IF(THL(N-1)-TKLAY*1.15 .LT. AO(J)) THEN
!                        TKLAY= THL(N-1)-AO(J)
!                        THLAY(J,N)=TKLAY/(ELEV-AO(J))
!                        NDEP(J)=N
!CIPK AUG00
!                        GO TO 817
!                      ELSE
!                        THLAY(J,N)= (THL(N-1)-THL(N))/(ELEV-AO(J))
!                      ENDIF
!                    ENDIF
!                  ENDDO
!                ELSE
!                  THLAY(J,1)=1.0
!                ENDIF
!                NDEP(J)=NTS
!                GO TO 817
!              ENDIF
!            ENDIF
!          ENDIF
!        ENDIF
!
!CIPK DEC99 ADD TEST TO SKIP PAST UNUSED 'VD' LINE
!        IF(ID(1:2) .EQ. 'VD') THEN
!          call ginpt(lin,id,dlin)
!        ENDIF
!
!  819   CONTINUE
!
!C-
!C       Print the NDEP values
!C-
!        IF(NDP .NE. 0) THEN
!          WRITE(LOUT,6877) (NDEP(J),J=1,NP)
!        ENDIF
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------


!Reset NDEP AND DEN
!------------------
DO J = 1, NP
  DEN (J) = 0.
  NDEP (J) = NDEP (J) + 1
enddo          


!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!C-
!C....... Create new node for junctions of 2 and 3D systems (vertically)
!C-
!        DO 83 J=1,NE
!          IF(NETYP(J) .EQ. 18) THEN
!            K=NOP(J,3)
!            IF(NDEP(K) .GT. 1) THEN
!C-
!C...... nop(j,19) is used to store the common node
!C-
!              NOP(J,19)=K
!              NP=NP+1
!              NOP(J,3)=NP
!              NDEP(NP)=NDEP(K)
!              WRITE(LOUT,6878) NP
!              CORD(NP,1)=CORD(K,1)
!              CORD(NP,2)=CORD(K,2)
!              AO(NP)=AO(K)
!              WIDTH(NP)=WIDTH(K)
!              NOPS(J,3)=NP
!
!CIPK JAN99
!CIPK JUL00 REDEFINE PROPS OF ADDED POINTS
!              AKP(NP)=AKP(K)
!              ADO(NP)=ADO(K)
!              ADT(NP)=ADT(K)
!              ADB(NP)=ADB(K)
!CIPK MAR00 ADD COMPUTATION OF LAYER INFO FOR NEW NODE
!              DO L=1,NDEP(K)
!                THLAY(NP,L)=THLAY(K,L)
!              ENDDO
!
!              NPM=NP
!            ENDIF
!          ENDIF
!   83   CONTINUE
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------


!Set direction for 1D/2D transitions and adjust for other purposes
!-----------------------------------
TransitionsDirections: DO N = 1, NE

!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
!C
!C....... Establish speical conditions for junctions when 2dv
!C
!        IF(IMAT(N) .GT. 900) THEN
!          N1=NOP(N,1)
!          IF(NDEP(N1) .GT. 1) THEN
!            N1=NOP(N,1)
!            N2=NOP(N,2)
!
!C.......  Force widths and slopes equal for main stem nodes
!            width(n1)=(width(n1)+width(n2))/2.
!            width(n2)=width(n1)
!            ss1(n1)=(ss1(n1)+ss1(n2))/2.
!            ss1(n2)=ss1(n1)
!            ss2(n1)=(ss2(n1)+ss2(n2))/2.
!            ss2(n2)=ss2(n1)
!
!            NCN=NCRN(N)
!            DO 224 K=3,NCN
!              N3=NOP(N,K)
!              NFIX(N3)=01200
!              JPOINT(N3)=-ABS(JPOINT(N3))
!  224       CONTINUE
!          ENDIF
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------

  !1D/2D element-2-element transitions
  !-----------------------------------
  ! - direction fix for transitioning node
  ! - adjust position
  ! - adjust width
  ! - adjust side slope
  !
  ! - reactivate elseif, if 3D option is switched on again (upper control flow)
  !ELSEIF (NCRN(N) .EQ. 5 .AND. IMAT(N) .LT. 901) THEN
  IF (NCRN (N) == 5 .AND. IMAT (N) < 901) THEN
    !left 2D-arc's node
    N1 = NOP (n, 4)
    !transitioning node
    N2 = NOP (n, 3)
    !right 2D-arc's node
    N3 = NOP (n, 5)

    !x-distance of transitioning arc
    dx = CORD (N1, 1) - CORD (N3, 1)
    !y-distance of transitioning arc
    dy = CORD (N1, 2) - CORD (N3, 2)
    !angle of the flow direction
    ANG = ATAN2 (DX, -DY)
    !Turn vector-direction into 1. or 4. quadrant of Cartesian coordinate system
    IF (ANG > 1.5707963) ANG = ANG - 3.1415926
    IF (ANG < -1.5707963) ANG = ANG + 3.1415926

    !Set midside at halfway point
    !----------------------------
    !recalculate the be elevation, because it must be a linear bed shape
    AO (N2) = 0.5 * (AO (N1) + AO (N3))
    !x-coordinate
    CORD (N2, 1) = 0.5 * (CORD (N1, 1) + CORD (N3, 1))
    !y-coordinate
    CORD (N2, 2) = 0.5 * (CORD (N1, 2) + CORD (N3, 2))

    !only for geometry approach
    if (imat (n) /= 89) then
      !original length, given by the input
      widto = width (n2)
      !length of the transitioning arc
      width (n2) = SQRT (DX**2 + DY**2)
      !control output
      WRITE(*,*) ' SETTING WIDTH, OLD WIDTH',N3, WIDTH(N3), WIDTO
      !force to be a rectangular cross section
      IF (SS1 (N3) /= 0. .OR.  SS2 (N3) /= 0.) THEN
        !warning output
        WRITE(*,*)' ATTENTION - SIDE SLOPES AT NODE ', N3
        write(*,*)' OR ', n1, 'NON-ZERO - VALUES FORCED TO ZERO'
        !force them to be zero
        SS1 (N3) = 0.0
        SS2 (N3) = 0.0
      ENDIF
    end if

    !set angle for fixed flow direction of transition;
    IF (ANG /= 0.) THEN
      ALFAK (N3) = ANG
    ELSE
      !if angle is by accident exactly zero, the model wouldn't recognize a reduction
      !  of the degree of freedoms. Thus the direction is turned at a minimum (it is
      !  measured in RAD, so 0.0001 is nothing!)
      ALFAK(N3)=0.0001
    ENDIF
    !control output
    WRITE(*,*) ' SETTING ALFAK',N3, alfak(n3)
    
  !1D/2D line-2-element transitions
  !--------------------------------
  ELSEIF (MaxLT /= 0) then

    Transitiontest: do i = 1, MaxLT
      !n is the actual element number in loop
      if (n == TransLines (i, 1)) then
        !look, whether length is non-zero
        if (lmt (TransLines (i,2)) <= 1) CALL errorMessageAndStop (1211, lmt (TransLines (i, 2)), 0.0d0, 0.0d0)
        !start- and ending-node of coupling
        N1 = Line ( TransLines (i, 2) , 1)
        N2 = Line ( TransLines (i, 2) , LMT (TransLines (i, 2)) )
        !x- and y-distances
        DX = cord (n1, 1) - cord (n2, 1)
        DY = cord (n1, 2) - cord (n2, 2)
        !chord-length
        translength = sqrt (DX**2 + DY**2)
        !velocity-direction of the nodes in between
        alfak_temp = atan2 (Dx, -DY)
        !correction to bring it into quadrants 1 or 4
        if (alfak_temp > 1.5707963) then
          alfak_temp = alfak_temp - 3.1415926
        elseif (alfak_temp < -1.5707963) then
          alfak_temp = alfak_temp + 3.1415926
        endif
        !assign the value to the nodes of the line-coupling, the midside nodes will be filled in check.subroutine
        assigningAlfaK: do j = 2, lmt (Translines (i, 2)) - 1
          alfak (Line (Translines (i, 2), j)) = alfak_temp
        enddo assigningAlfaK
        !jump out of loop over all transitions
        exit Transitiontest
      endif
      !Assigning the directions to the CORNER-nodes of the coupling. Problem: This is in getgeo. The line-construct still consists only of
      !corner nodes of connected 2D-elements. In the subroutine check it is widened to the midside node. So the assignment changes to that
      !subroutine: CHECK.subroutine
    enddo Transitiontest
  endif
enddo TransitionsDirections

!copy number of nodes
!--------------------
NPM11 = NPM

!store original bed elevation of all nodes
!-----------------------------------------
do N = 1, NP
  AORIG (N) = AO (N)
enddo

return

 5035 FORMAT (I8, 3E8.0)
 5040 FORMAT (10I6, F8.0)
 5044 FORMAT (I8, 4F8.0)
 6115 FORMAT( /// 10X, 'NO CONTINUITY CHECKS REQUESTED.....' )
 6120 FORMAT(   //// 10X, 'CONTINUITY CHECKS TO BE MADE ALONG THE FOLLOWING LINES'  // 6X, 'LINE',10X,'NODES')
 6125 FORMAT( I10, 4X, 20I5 )
 6210 FORMAT( // 10X, '2D ..NETWORK INPUT COMPLETE..' // &
     & 15X, 'MAX ELEMENT NUM  ', I14 / 15X, 'MIN ELEMENT NUM  ',I14 / &
     & 15X, 'MAX NODE NUM  ', I17 / 15X, 'MIN NODE NUM  ' ,I17 )
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------
! 5015 FORMAT (9I8)
! 5021 FORMAT (9F8.0)
! 6007 FORMAT (' THE FOLLOWING LAYER ELEVATIONS HAVE BEEN SPECIFIED'
!     +      /(8F9.2))
! 6877 FORMAT(///10X,'    INPUT VALUES OF NDEP BY NODE'/
!     1          (10I5))
! 6878       FORMAT(' NODE',I5,' ADDED AT 2D TO 3D JUNCTION')
!---------------------------------------------------------------------
!3D-source-code: Just activate, adapt for allocatables, debug and run!
!---------------------------------------------------------------------

end subroutine
!***

end module
