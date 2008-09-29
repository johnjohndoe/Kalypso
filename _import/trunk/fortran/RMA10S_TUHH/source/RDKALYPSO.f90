!-----------------------------------------------------------------------
!nis,dec06: Adding TLcnt as counter for number of 1D-2D-Transition lines
!SUBROUTINE RDKALYPS(nodecnt,elcnt,arccnt,KSWIT)
SUBROUTINE RDKALYPS(nodecnt,elcnt,arccnt, PolySplitCountA, PolySplitCountQ, PolySplitCountB, TLcnt,KSWIT)
!nis,feb07: Allow for counting the midside nodes of FFF elements
!SUBROUTINE RDKALYPS(nodecnt,elcnt,arccnt,TLcnt,FFFcnt,Addcnt,KSWIT)
!-
!-
!
! This Subroutine reads the model data to run FE-net-data in Kalypso-2D-format
! within RMA10S. It is developed out of the subroutine 'modell_lesen', that
! is part of the Kalypso-2D's program-code as described below. Some changes
! were implemented to turn the outputdata of this subroutine in the way, that
! the array-values within RMA10S are properly installed. The main change is,
! that the subroutine is called with a parameter KSWIT, which is responsable
! for the work of the subroutine, that is either reading geometry dimensions,
! reading geometry or reading restart informations.
!                                             may 2006 NiS
!
! This Subroutine is reading the model data in the (1995 developed by Wyrwa)
! format for Microstation (.asc).
! In this file the geometry data that used to be saved in a 'geo'-file
! and (if available) the restart data is saved.
! During the conversion of the mesh from arc-structure to element-wise
! structure the data is controlled regarding consistency and mesh errors.
! The reordering is called from this subroutine. It must not be called
! from earlier executed parts of the code.
!                                             Jun 2004 Ploeger
!
! Die Subroutine "modell_lesen" liesst die Modelldaten
! in dem neuen (Microstation) .asc Format;
! in einem solchen File liegen sowohl die Geometriedaten,
! die frueher auf dem geo-file gespeichert wurden, als auch
! die restart-Daten, falls vorhanden.
! Bei der Umwandlung der Darstellungsweise des Netzes von der
! Kantenstruktur in die elementweise Speicherform wird auf
! Datenkonsistenz und Netzfehler uberprueft.
! das reordering wird von hier aus aufgerufen. Es muss nicht mehr wi
! frueher vorab ausgefuehrt werden.
!                                             Nov 1995 Wyrwa
!-----------------------------------------------------------------------

! DECLARATIONS ---------------------------------------------------------

!NiS,may06: Replacing old blocks and common-files with modules

!"old" blocks
!INCLUDE "common.cfg"
!COMMON / raus / rausv (4, mnd), iauslp, iausnpm, zeigma (mnd)

!"new modules"
USE BLK10MOD    !cord(i,1), cord(i,2), ao(i), tett, MaxE, MaxP, irk, rk_zeile
USE BLKDRMOD
USE ParaKalyps  !new module for KALYPSO-specific globals and neighbourhood relations
USE BLKSUBMOD   !for weir structures
USE Para1DPoly  !Modul für Teschke-1D-Elemente
!-

!NiS,may06: Former variable declaration is sorted and the variables are described; new entered variables are EXPLICITLY pointed out

REAL                   :: x, y, z                      !temporary coordinate-spaces for generation of new midside-nodes
REAL                   :: vekkant(2),vekpu1(2),kreuz   !variables for checking-sequence of mesh
REAL                   :: wsp                          !???
real                   :: hhmin_loc
!NiS,mar06: Comments added to:
!REAL 	:: x, y, z, vekkant (2), vekpu1 (2), kreuz, wsp

INTEGER                :: i, j, k, l		       !loop-counters with different meanings
INTEGER                :: m, n                         !copy of nodecount and elementcount at the end of the subroutine; the need is not clear!
INTEGER                :: elzaehl, mittzaehl           !real element and midside counters
INTEGER,ALLOCATABLE    :: arc(:,:)  	               !local array for saving the arc-informations and passing them to the sorting process
!nis,dec06: for neighbourhood relations at line couplings
INTEGER, ALLOCATABLE   :: nop_temp (:)
INTEGER                :: ncorn_temp
!-

!nis,dec06: Adding TLcnt
INTEGER                :: arccnt,elcnt,nodecnt         !nominal arc-, element- and nodecounter, it shows maximum of real counter and maximum used ID
INTEGER                :: TLcnt                        !nominal 1D-2D-Transitionline counter, it shows maximum of real counter and maximum used ID
INTEGER                :: PolySplitCountA, PolySplitCountQ, polySplitCountB
      						       !the purpose is, not to generate errors with numerical gaps in defintion lists.

!-
!nis,feb07: Adding numberation for Flow 1D FE element midside nodes
!INTEGER                :: fffcnt, AddCnt               !counter for midside nodes of flow1D-FE elements, so that they can also be enumberated
!-
INTEGER, allocatable :: elem (:, :)                    !local array for element informations ([number of arcs +1]; node-numbers of corners); dependent
                                              	       !on the type and shape of element; the first entry can also show 1D-ELEMENT with negative value
INTEGER              :: elkno(5), mikno(4)           !local array for loop, that generates midsides for elements where no midside is assigned
INTEGER, allocatable :: elfix (:)                      !for reordering subroutine of RMA1, not used in RMA10S
INTEGER              :: istat                        !IOSTAT-Value that becomes non-zero while input-reading-errors

!nis,dec06: Local supporting variable for reading Continuity lines
INTEGER                :: DefLine
!-

!nis,aug08: Reordering sequence renewing
logical :: ReorderingNotDone

!NiS,mar06: Comments added and array sizes changed to:
!INTEGER :: i, j, k, l, m, n, elzaehl, mittzaehl, arc (mnd, 5),&
!	  & elem (mel, 6), arccnt, elcnt, elkno (5), mikno (4), elfix (mel)
!INTEGER :: istat

!In order of global conflicts id is renamed to id_local
!CHARACTER(LEN=2)       :: id
CHARACTER (LEN=2)       :: id_local                     !line data identifier for reading input/restart-file
CHARACTER (LEN=2400)    :: linie                        !temporary input space for reading input/restart-file
CHARACTER (LEN=20)      :: inquiretest
!LOGICAL                :: inquiretest

!NiS,mar06: the following variables were already used in the KALYPSO-2D version, but were never declared. Now they are.
INTEGER                :: iakt         	               !temp: stands for an active element (number) in a calculation run
INTEGER                :: ibot,itop                    !temp: first(BOTtom) and last(TOP) definition nodes of an arc
INTEGER                :: ilft,irgt                    !temp: left and right positioned element of an arc
INTEGER                :: jnum                         !temp: shows the number of nodes assigned to an element; shows implicitly shape of element

!Other old variables
CHARACTER(LEN=80)      :: restart                      !space for the name of restart file, where the run started from; no global use
INTEGER                :: qlist (2, 160)               !list for reordering sequence
REAL                   :: sumx, sumy = 0.0             !temporary summation of coordinates of elements points, to find center of element by average
INTEGER                :: node1, node2                 !temporary node spaces for neighbourhood relation calculation

!NiS,mar06 NEW DECLARATIONS:
INTEGER                :: temparc(5)                   !reading an arc at the option of KSWIT=1, when the ARC-array is not yet assigned
INTEGER                :: arcs_without_midside         !counting the number of arcs that have no explicitly defined midside to increase the number of
                                		       !MAXP at the end of the dimensioning run for array allocation in initl.subroutine
INTEGER                :: midsidenode_max              !comparison variable for the highest necessary node number, so it can be tested, whether
                                      		       !the node list is complete
!NiS,apr06: NEW SWITCH AND FILE READING CONTROL OPTIONS
INTEGER                :: KSWIT                        !Switch for Run-option of this subroutine;
						       !  0:      read whole geometry info and install values into the proper arrays
                                                       !  1:  read just the dimension informations about geometry
                                                       !  2:  read just the resart values

INTEGER                :: linestat
INTEGER                :: unit_nr                      !internal unit number, because this subroutine works as modell and restart data reader

INTEGER                :: weircnt                      !LF Nov06 to count the weir elements
!nis,jan07: temporary variables for connection handling
INTEGER                :: ConnNumber
!-
!NiS,mar06: the following common block has been replaced by global module called ParaKalyps


!tm Thomas Maurer, 15.05,1998
!tm linie von *80 auf *120 verlaengert                                  
!tm Anpassung an "verlaengerten" *.2D file, d.h.                        
!tm VA-Zeile hat noch eine neue Position auf von Spalte 86 bis 96 bekomm
!tm VA         1           0.9858658           0.0481516           2.217


! INITIALISIERUNGSBLOCK -----------------------------------------------------------------------

allocate (elem (MaxE, 6), elfix (MaxE))

IF (KSWIT==1) THEN        !In the first case the value MAXA has to be found, the allocation of
  MAXA = 0                  !arc(i,j) is not necessary for the first run, so that it is allocated
  MAXE = 0                  !EFa Nov06
  ALLOCATE (arc (MAXA, 5))   !just pro forma, it is deallocated at the end of this run.
ELSE                      !In the second run, the value of MAXA is known and the arc-array is
  ALLOCATE (arc (MAXA, 5))   !allocated again; it is deallocated at the end of the run.
ENDIF                     !NiS,mar06
arcs_without_midside = 0
midsidenode_max = 0

!nis,jun07: Initializing the qlist array
do i = 1, 160
  qlist(1, i) = 0
  qlist(2, i) = 0
end do
!-

!Nis,may06,com: initializing the I/O error space
istat =0

nodecnt = 0
arccnt  = 0
elcnt   = 0
PolySplitCountA = 0
PolySplitCountQ = 0
PolySplitCountB = 0

!nis,dec06: adding TLcnt for 1D-2D-Transition-lines
TLcnt   = 0
!-
!nis,feb07: Initializing the number of midside nodes of Flow1DFE elements
!fffCnt = 0
!AddCnt = 0
!-

!nis,dec06: Initilaizing CCL supporting variable
DefLine = 0

!LF nov06: initialize the number of weir elements
weircnt=0

!NiS,mar06: temparc is an array for reading arc informations in the dimensioning run (KSWIT.eq.1)
DO i=1,5
  temparc (i) = 0
ENDDO

!NiS,mar06: changed allocation of arc; the former allocation with the number of nodes was too big, so the MaxA (for maximum number of arcs) is used:
!           changed mnd to MaxA; additionally the array is firstly initialized, when the geometry is read (KSWIT.eq.0)
IF (KSWIT == 0) THEN
  outer1: DO i = 1, MaxA
    inner1: DO j = 1, 5
      arc (i, j) = 0
    ENDDO inner1
  ENDDO outer1
  !NiS,mar06: variable name changed; changed mel to MaxE
  outer2: DO i = 1, MaxE
    DO j = 1, 6
      elem (i, j) = 0
    END DO
    DO j = 1, 8
      nop (i, j) = 0
    END DO
    imat (i) = 0
  END DO outer2
ENDIF

! LESESCHLEIFE -----------------------------------------------------------------

!NiS,apr06: chosing file unit
!           for restart option unit = nb
!           for modell input   unit = ifile
IF (KSWIT /= 2) THEN
  unit_nr = ifile
ELSE
    unit_nr = nb
ENDIF

!INQUIRE(unit_nr, opened=inquiretest)
INQUIRE (unit_nr, form = inquiretest)
WRITE (*,*) 'inquiretest: ', inquiretest
WRITE (*,*) 'unit_nr: ', unit_nr
REWIND (unit_nr)
!-

!NiS,mar06: original reading sequence has a little bit changed; the necessary input-options are sorted
!           to the top of the do loop
!This endless loop is only stopped, when the end of file condition is reached
reading: do

  !Read next line in IFILE (input-geometry-file)
  !NiS,mar06: unit number changed; changed '12' to unit_nr
  READ (unit_nr, '(a)', IOSTAT = istat) linie
!  IF(kswit.eq.2)  WRITE(*,*) linie
  !end of file condition
  if (istat == -1) EXIT reading

  !ERROR - reading error
  if (istat /= 0) call ErrorMessageAndStop(1001, unit_nr, 0.0d0, 0.0d0)

!NiS,apr06: first half of reading-DO-LOOP enables to read geometry informations (KSWIT.eq.{0,1}), later the RESTART informations are enabled
!           to be read (KSWIT.eq.2); some ID-lines are disabled, that are necessary for Kalypso-2D; against that some new ID-lines are added for
!           the use of RMA10S, especially for aspect of 1D-applications, for example the 1D-node informations concerning the cross section
!           of the simulated river.


! OVER ALL INFORMATIONS ----------------------------------------------
    IF (linie (1:2) .eq.'00') THEN
      !NiS,mar06: changed id to id_local; global conflict
      READ (linie, '(a2,a80)') id_local, title 
    ENDIF


  KSWITTEST: IF (KSWIT.eq.0 .or. KSWIT.eq.1) THEN

! GEOMETRY INFORMATIONS ----------------------------------------------

    !NODE DEFINITIONS ---
    IF (linie (1:2) .eq.'FP') THEN
      !NiS,mar06: Find out maximum NODE number
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local,i
        if (i.gt.nodecnt) nodecnt = i
      !NiS,mar06: Read NODE geometry informations
      ELSE
        istat=0
        !TODO: Format differentiation
        READ (linie, *,IOSTAT=istat) id_local, i, cord (i, 1) , cord (i, 2), ao (i),kmx(i)
        if (istat.eq.0) then
          WRITE(lout,*)'Die Kilometrierung von Knoten',i,'wurde eingelesen:',kmx(i)
        end if
        IF (i.gt.nodecnt) nodecnt = i
        !ERROR
        IF (i <= 0) call ErrorMessageAndStop (1002, i, 0.0d0, 0.0d0)
      ENDIF
    ENDIF

    !ARC DEFINITIONS ---
    IF (linie (1:2) .eq.'AR') then
      !Find out maximum ARC number
      IF (KSWIT == 1) then
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,6i10)') id_local,i, (temparc(j),j=1,5)
        IF (i  > arccnt) arccnt = i
        !Remember, whether ARC has no midside node
        IF (temparc(5) == 0) then
          arcs_without_midside = arcs_without_midside + 1
        !Remember the maximum midside ID-number, if midside is defined
        ELSEIF (temparc(5) > 0) then
          midsidenode_max = MAX (midsidenode_max,temparc(5))
        else
          !ERROR
          call ErrorMessageAndStop (1101, temparc(5), cord(temparc(1), 1), cord(temparc(1), 2))
        ENDIF
      !Read ARC geometry informations
      ELSE
        READ (linie,'(a2,6i10)') id_local, i, (arc(i,j), j=1, 5)
        !Look for errors in enumeration
        IF (i > arccnt) arccnt = i
        !ERROR - negative arc number
        IF (i <= 0) call ErrorMessageAndStop (1301, i, 0.0d0, 0.0d0)
      ENDIF
    ENDIF

    !Interpolation information for elements
    IF (linie(1:2) == 'IP') THEN
      IF (kswit == 1) THEN
        READ (linie, '(a2, i10, i10)') id_local, i, j
        NodesToIntPol = NodesToIntPol + j
        if (j > MaxIntPolElts) MaxIntPolElts = j
      ELSEIF (kswit == 0) then
        READ (linie, '(a2, i10, i10)') id_local, i, IntPolNo(i)
      ENDIF
    ENDIF

    !nis,aug08: Valid range of polynomials is not needed anymore
    IF (linie(1:2) .eq. 'MM') continue

    !nis,nov07: new range line ID (polynom range PR)
    !id_local  = 'PR'
    !       i  = node-ID
    !       j  = number of polynom splitting parts
    !polyrange = maximum values
    if (linie(1:3) .eq. 'PRA') then
      IF (KSWIT == 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j
        if (PolySplitCountA < j) PolySplitCountA = j
      else
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, polySplitsA(i), hhmin_loc, (polyrangeA(i, k), k=1, polySplitsA(i))
        hhmin(i) = max (hhmin(i), hhmin_loc)
        hhmax(i) = min (hhmax(i), polyrangeA (i, polySplitsA(i)))
      endif
    end if
    if (linie(1:3) .eq. 'PRQ') then
      IF (KSWIT == 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j
        if (PolySplitCountQ < j) PolySplitCountQ = j
      else
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, polySplitsQ(i), hhmin_loc, (polyrangeQ(i, k), k=1, polySplitsQ(i))
        hhmin(i) = max (hhmin (i), hhmin_loc)
        hhmax(i) = min (hhmax (i), polyrangeQ (i, polySplitsQ(i)))
      endif
    end if
    if (linie(1:3) .eq. 'PRB') then
      IF (KSWIT == 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j
        if (PolySplitCountB < j) PolySplitCountB = j
      else
        !nis,aug08: Just read polynomial range of Boussinesq-polynomial, if it's used.
        if (beient /= 0 .and. beient /= 3) then
          linestat = 0
          read (linie, *, iostat = linestat) id_local, i, polySplitsB(i), hhmin_loc, (polyrangeB(i, k), k=1, polySplitsB(i))
          hhmin(i) = max (hhmin(i), hhmin_loc)
          hhmax(i) = min (hhmax(i), polyrangeB (i, polySplitsB(i)))
          hbordv (i) = polyrangeB (i, 1)
        endif
      endif
    end if
    !nis,nov07: new range line ID (polynom range PR)
    !id_local  = 'AP '
    !       i  = node-ID
    !       j  = number of polynom splitting parts
    if (linie(1:3) .eq. 'AP ') then
      IF (KSWIT /= 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j, (apoly(j, i, k), k = 0, 12)
      endif
    end if
    if (linie(1:3) .eq. 'QP ') then
      IF (KSWIT /= 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j, qgef(i), (qpoly(j, i, k), k = 0, 12)
        !remember, that the node is a 1D polynomial approach node
        if (.not. IsPolynomNode (i)) IsPolynomNode (i) = .true.
      endif
    end if
    if (linie(1:3) .eq. 'ALP') then
      if (beient /= 0 .and. beient /= 3) then
        IF (KSWIT /= 1) then
          linestat = 0
          read (linie, *, iostat = linestat) id_local, i, j, (alphapoly(j, i, k), k = 0, 12)
        endif
      endif
    end if
    if (linie(1:3) .eq. 'BEP') then
      if (beient /= 0 .and. beient /= 3) then
        IF (KSWIT /= 1) then
          linestat = 0
          read (linie, *, iostat = linestat) id_local, i, j, (betapoly(j, i, 0), k = 0, 12)
        endif
      endif
    end if

    !ELEMENT DEFINITIONS ---
    IF (linie (1:2) .eq.'FE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local, i
        IF (i > elcnt) elcnt = i
      !Read geometry into arrays
      ELSE
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,4i10)') id_local, i, imat (i), imato (i), nfixh (i)

        !LF,nov06: Read again the FE line for the starting node of a weir element
        if (imat(i) .gt. 903 .and. imat(i) .lt. 990) then
          weircnt = weircnt + 1
          read (linie, '(a2,5i10)') id_local, i, imat(i), imato(i), nfixh(i), reweir(weircnt,1)
          !ERROR - no starting node for weir element definition was found
          if (reweir (weircnt, 1) <= 0) call ErrorMessageAndStop (1003, i, 0.0d0, 0.0d0)
          reweir (weircnt, 2) = i
        end if
        IF (i > elcnt) elcnt = i
        IF (i <= 0) call ErrorMessageAndStop (1004, i, 0.0d0, 0.0d0)
      ENDIF
    ENDIF

    !1D JUNCTION ELEMENT DEFINITIONS ---
    IF (linie (1:2) .eq.'JE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local,i
        IF (i > elcnt) elcnt = i
      ELSE
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,9i10)') id_local, i, (nop(i, j), j=1, 8)
        IF (i > elcnt) elcnt = i
        IF (i <= 0) call ErrorMessageAndStop (1005, i, 0.0d0, 0.0d0)
      ENDIF
    ENDIF

    !ROUGHNESS CORRECTION LAYER ---
    if (linie (1:2) == 'RC') then
      if (KSWIT == 1) CYCLE reading
      read (linie, '(a2, i10, 3(f10.6))') id_local, i, correctionKS(i), correctionAxAy(i), correctionDp(i)
    end if

!    !ROUGHNESS CLASS INFORMATIONS ---
!    IF (linie (1:2) .eq.'RK') THEN
!      !not interesting for dimension reading
!      if (KSWIT == 1) CYCLE reading
!      irk = irk + 1
!      rk_zeile (irk) = linie (1:)
!    ENDIF

    !NiS,may06: cross section reading for 1D-ELEMENTS
    !CROSS SECTIONAL INFORMATIONS FOR 1D-NODES ---
    IF (linie (1:2) .eq.'CS') THEN
      !Only interesting for geometry reading run
      IF (KSWIT.eq.1) CYCLE reading
      !read informations into proper arrays
      READ (linie, '(a2,i10,6F10.0)') id_local, i, width(i), ss1(i), ss2(i), wids(i), widbs(i), wss(i)
    END IF
    !-

    !NiS,may06: Transition elements between 1D- and 2D- network parts
    !1D-2D-TRANSITION ELEMENTS ---
    IF (linie (1:2) .eq.'TE') THEN
      IF (KSWIT.eq.1) THEN
        READ (linie, '(a2,i10)') id_local, i
        IF (i.gt.elcnt) elcnt = i
      ELSE
        READ (linie, *) id_local, i, (nop(i,k),k=1,5)
        !Increase number of 1D-2D-TRANSITION ELEMENTS
        MaxT = MaxT + 1
      END IF
    END IF
    !-

    !NiS,nov06: Transition line elements between 1D- and 2D-networks with an element to line connection
    !           TransLines(i,1): transitioning element
    !           TransLines(i,2): transitioning line
    !           TransLines(i,3): node, which is connected on the 1D side of the transition element
    !           TransLines(i,4):
    if (linie(1:2).eq.'TL') then
      IF (KSWIT.eq.1) THEN
        READ (linie,'(a2,i10)') id_local, i
        WRITE(*,*) 'getting here, TLcnt definition'
        IF (i.gt.TLcnt) TLcnt = i
      ELSE
        WRITE(*,*) MaxLT
        READ (linie, '(a2,5i10)') id_local, i, (TransLines (i, k), k = 1, 4)
        !Apply default TransLines (i)
        if (istat /= 0 .and. TransLines(i, 4) == 0 &
        &   .or. &
        &   (TransLines (i, 4) /= 1 .and. TransLines (i, 4) /= 2 .and. TransLines (i, 4) /= 3) ) then
          TransLines (i, 4) = 1
        end if
        TransitionElement (TransLines(i, 1)) = .true.
      END IF
    end if
    !-

    !nis,dec06: 'Continuity lines' can also be defined in the network file. The purpose is to make use of the already implemented structure
    !           of CCLs for defining line geometry at transitions. For reordering purposes it is necessary to be able to read in Continuity
    !           lines at the beginning together with the network.
    if (linie(1:2).eq.'CC') then
        read (linie,'(a2,i1,i5)') id_local,DefLine,i
      if (kswit.eq.1) then
        if (DefLine == 1) WRITE(*,*) 'Continuity Line',i, 'defined in network file.'
      else
        if (DefLine == 1) then
          read (linie(9:80),'(9i8)') (line(i,k),k=1,9)
          !Remember Line number
          if (i.gt.NCL) NCL = i
        ELSEIF (DefLine.gt.1) then
          if (line(i,(DefLine-1)*9-1).eq.0) then
            WRITE(*   ,*) 'Continuity line',i,'not properly defined. Previous read values in line will be deleted'
            WRITE(*   ,*) 'Error occurs at defintion line CC',DefLine,'in geometry file.'
            WRITE(Lout,*) 'Continuity line',i,'not properly defined. Previous read values in line will be deleted'
            WRITE(Lout,*) 'Error occurs at defintion line CC',DefLine,'in geometry file.'
            do k=1,350
              line(i,k) = 0
            end do
          else
            read (linie(9:80),'(9i8)') (line(i,k),k=(DefLine-1)*9+1,k+8)
          end if
        end if
      end if
    end if
    !-

  ELSEIF (KSWIT == 2) THEN

! RESTART INFORMATIONS ----------------------------------------------

    !INITIAL CWR-VALUES ---
    IF (linie (1:2) == 'CW') THEN
      READ (linie, '(a2,1x,a32)') id_local, name_cwr
    ENDIF

    !DATE INFORMATIONS IN RMA10S FORMAT ---
    IF (linie (1:2) == 'DA') THEN
      READ (linie, '(a2,i10,f20.7)') id_local, iyrr, tett
    ENDIF

    !INITIAL VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP ---
    IF (linie (1:2) .eq.'VA') then
      READ(linie,'(a2,i10,4f20.7)') id_local, i, (vel(j,i), j=1, 3), rausv (3, i)
      !ERROR - restart values can't be applied to node out of zero-maxp-Range
      !nis,aug08: If node number is zero, it has no coordinates; use dummy coordinates 0.0
      IF (i > MaxP .or. i <= 0) call ErrorMessageAndStop (1601, i, 0.0d0, 0.0d0)
    ENDIF

    !INITIAL VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP; ONLY FOR INTERPOLATED PROFILES/ NODES ---
    IF (linie (1:2) .eq.'IR') then
      READ(linie,'(a2, i10, 4f20.7)') id_local, i, (vel(j,i), j=1, 3), rausv (3, i)
      !ERROR - restart values can't be applied to node out of zero-maxp-Range
      IF (i > MaxP .or. i <= 0) call ErrorMessageAndStop (1601, i, cord (i, 1), cord (i, 2))
      !ERROR - TRYING TO APPLY RESULT OF INTERPOLATED PROFILE/ NODE TO A REAL NODE
      if (.not. (IntPolProf (i))) call ErrorMessageAndStop (1602, i, cord (i, 1), cord (i, 2))
    ENDIF

    !NiS,may06: these degrees of freedom are missing in Kalypso-2D, because they are not used there; adding for application in RMA10S
    !INITIAL VALUES FOR DEGREES OF FREEDOM 4 TO 7 ---
    IF (linie(1:2) == 'DF') THEN
      READ(linie,'(a2,i10,4f20.7)') id_local, i, (vel(j,i), j=4,7)
      !ERROR - restart values can't be applied to node out of zero-maxp-Range
      IF (i > MaxP .or. i <= 0) call ErrorMessageAndStop (1601, i, cord (i, 1), cord (i, 2))
    END IF

    !MD: read flow resistance for Sediment-Transport
    IF (linie(1:2) .eq. 'FR') THEN
      READ(linie,'(a2,i10,4f15.7)') id_local, i, lambdaTot(i), lambdaKS(i), lambdaP(i), lambdaDunes(i)
      !MD: read flow resistance results for elements
      !ERROR - restart values can't be applied to element out of zero-maxe-Range
      !nis,aug08: Function must be called with the correct number of arguments; use dummy arguments 0.0 as coordinates)
      IF (i > MaxE .or. i <= 0) call ErrorMessageAndStop (1603, i, 0.0d0, 0.0d0)
    end if

    !INITIAL GRADIENTS OF VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP ---
    IF (linie (1:2) == 'GA') then
      READ (linie, '(a2,i10,3f20.7)') id_local, i, (vdot(j,i),j=1,3)
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on negative NODE number
      IF (i.le.0) stop 'Knotennummer.le.0'
    ENDIF

    !VALUES OF VELOCITIES AND WATER DEPTH OF OLD TIME STEP ---
    IF (linie (1:2) == 'VO') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvvo = 1
      READ (linie, '(a2,i10,3f20.7)') id, i, (vold (j, i) , j=1,3)             !vold muss NICHT gelesen werden
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      IF (i > MaxP) stop 'i > MaxP'
      !Stop program execution on negative NODE number
      IF (i <= 0) stop 'Knotennummer <= 0'
    ENDIF

    !GRADIENTS OF VELOCITIES AND WATER DEPTH OF OLD TIME STEP ---
    IF (linie (1:2) == 'GO') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvvo = 1
      READ (linie, '(a2,i10,3f20.7)') id, i, (vdoto (j, i) , j=1,3)            !vdoto muss NICHT gelesen werden
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on negative NODE number
      IF (i.le.0) stop 'Knotennummer.le.0'
    ENDIF

!    !ADDITIONAL INFORMATIONS FOR EVERY NODE ---
!    IF (linie (1:2) .eq.'ZU') then
!      !NiS,apr06: variables deactivated for RMA10S
!      !cvzu = 1
!      !NiS,apr06: only hel(i) and hdet(i) have to be read; changing read command
!      !READ (linie, '(a2,i10,i6,4f15.7)') id, i, ndry(i), hel(i), hol(i), hdet(i), hdot(i)
!      READ(linie,'(a2,i10,6x,2(f15.7,15x))')id_local,i,hel(i),hdet(i)
!
!      !NiS,mar06: name of variable changed; changed mnd to MaxP
!      !Stop program execution on nodenumber higher than MaxP; could normally not happen
!      IF (i.gt.MaxP) stop 'i.gt.MaxP'
!      !Stop program execution on negative NODE number
!      IF (i.le.0) stop 'Knotennummer.le.0'
!    ENDIF

! TEST BLOCK FOR ERRORS ------------------------------------------------------------------------

  ELSE !other values of KSWIT will generate an error, this can't happen normally

    call ErrorMessageAndStop (1006, kswit, 0.0d0, 0.0d0)

  ENDIF KSWITTEST 

END DO reading
WRITE(*,*) ' Schaffe die Leseschleife'


!nis,dec06: Form Continuity lines length arrays, but just, when KSWIT == 0
if (KSWIT == 0) then
  CClineforming: do i=1,50
    if (line(i,1).eq.0) CYCLE CClineforming
    getfirstzeroentry: do j=2,350
      if (line(i,j).EQ.0) then
        lmt(i) = j-1
        CYCLE CClineforming
      end if
      !if 350 nodes are in the line the entry has to be made after loop here.
      lmt(i) = 350
    end do getfirstzeroentry
  end do CClineforming
endif
!-

!ENDBLOCK FOR THE CASE OF DIMENSION READING (KSWIT.eq.1) ----------------
!NiS,mar06: leave subroutine, if FE-net dimensions are ascertained
IF (KSWIT == 1) THEN

  !ERROR - midside node ID was defined but it has no coordinates-definition. This is a problem, because the count of the nodes is then wrong
  IF (midsidenode_max > nodecnt) call ErrorMessageAndStop (1108, midsidenode_max, 0.0d0, 0.0d0)

  !INFORMATIONS
  WRITE(*,*) '          In RDKALYPS.Info '
  WRITE(*,*) '          **************** '
  WRITE(*,*) '            number of arcs: ', arccnt
  WRITE(*,*) '          hightest node ID: ', nodecnt
  WRITE(*,*) ' arcs without midside node: ', arcs_without_midside
  nodecnt = nodecnt + arcs_without_midside

  REWIND (IFILE)
  DEALLOCATE (arc)                                                       !the pro forma allocation of arc(i,j) is stopped
  RETURN                                                    	        !If the dimension reading option is chosen (that means KSWIT=1), this
                                                                        !subroutine can be returned at this point.

!ENDBLOCK FOR THE CASE OF RESTART INFORMATION READING (KSWIT.eq.2) ------
!NiS,apr06: leave subroutine, if restart inforamtions are read
ELSEIF (KSWIT == 2) THEN
  !NiS,may06: REWINDING might be needful; at the moment not necessary
  REWIND (NB)
  WRITE(*,*)' Leaving the KALYPSO-2D restart file.'
  RETURN
ENDIF


!     TRANSLATION TO KALYPSO-2D FORMAT -----------------------------------------------------
!     UMSETZUNG AUF KALYPSO-2D FORMAT ------------------------------------------------------

!     Die Datenstruktur, die die Verknuepfung der Knoten mit den
!     Elementen ueber die Kanten bewerkstelligt, wird nun in die
!     im Berechnungsmodul verwendete Struktur der direkten
!     Zuordnung der Knotennummern zu den Elementen umgesetzt.
!     Dabei wird auf Datenkonsistenz und Netzfehler geprueft.
!
!     Baue das Feld ELEM(j,i) auf (Kantennummern am Element):
!     - ELEM(j,1) Kantenanzahl; this is not correct!!! it is as follows because
!     of direct access to the array elements 2...5
!     NiS,mar06
!     - ELEM(j,1) Kantenanzahl + 1
!     - ELEM(j,2-4/5) Kantennummern
!     mit j=Elementnummer
!NiS,may06: every arc's element(s) will be saved in the elem-array, which shows after Loop-Execution the grouping of nodes belonging to an element.
!           The stored values are: On the first place the number of nodes associated with the element and on the 2nd to 5th place the nodes (unsorted)
!
!           Changed the if-clauses in the way that, 1D, 1D-2D-transitions and 2D-elements can be recognized
DO i = 1, arccnt
  !1D-ELEMENT, 1D-2D-TRANSITION-ELEMENT or DEAD ARC
  IF (arc(i,3) == arc(i,4)) THEN
    !SKIP DEAD ARCS WITH NO ELEMENT NUMBERS ASSIGNED
    IF (arc(i,3) == 0) THEN
      WRITE (Lout,9003) i
      WRITE (*   ,9003) i

    !1D (TRANSITION ELEMENTS AND NORMAL ELEMENTS)
    ELSE
      !TODO: These checks are not 100 percent consistent
      j = arc(i,3)
      !Error, if the potential 1D-NODE is used, which means (elem(j,1).ne.0)
      IF (elem(j,1) /= 0) THEN
        !ERROR - 1D-element is used twice which is not possible
        call ErrorMessageAndStop (1302, j, 0.5 * (cord (arc (i,1), 1) + cord (arc (i, 2), 1)), &
                                &          0.5 * (cord (arc (i,1), 2) + cord (arc (i, 2), 2)))
      !assign identification for 1D-NODES (elem(j,1).eq.-1) for normal 1D-ELEMENTS and (elem(j,1).eq.-2) for 1D-2D-transition elements; remember
      !the ARC, that defines the 1D-ELEMENT for later NODE extraction
      ELSE
        !No 4th NODE at normal 1D-ELEMENTS
        IF (nop(j,4).eq.0) THEN
          elem(j,1) = -1
        !4th NODE at 1D-2D-transition ELEMENTS; nodes for 1D-2D-TRANSITION ELEMENTS were already assigned in the reading section
        ELSE
          elem(j,1) = -2
        ENDIF
        !Remember the ARC of the 1D-ELEMENT
        elem(j,2) = i
      ENDIF
    ENDIF

  !2D-ELEMENT(S)
  ELSE !implicitly arc(i,3) .ne. arc(i,4), which means one can be zero.
    !linkes Element k = 3; rechtes Element k = 4
    DO k = 3, 4
      j = arc (i, k)
      !Testing, whether placeholder has element informations
      IF (j.gt.0) then
        !ERROR - element is used twice which is not possible
        IF (elem (j, 1) == -1) call ErrorMessageAndStop (1302, j,                  &
                             & 0.5 * (cord (arc (i,1), 1) + cord (arc (i, 2), 1)), &
                             & 0.5 * (cord (arc (i,1), 2) + cord (arc (i, 2), 2)))
        !Testing, whether it is the first defining arc
        IF (elem (j, 1) .eq.0) elem (j, 1) = 1
        !Increase number of assigned ARCS to ELEMENT by increment =1
        elem (j, 1) = elem (j, 1) + 1
        !ERROR - Element is defined with more than 4 arcs
        IF (elem (j, 1) > 5) call ErrorMessageAndStop (1202, j,                      &
                               & 0.5 * (cord (arc (i,1), 1) + cord (arc (i, 2), 1)), &
                               & 0.5 * (cord (arc (i,1), 2) + cord (arc (i, 2), 2)))
        ! Dem Feld ELEM(j,2...5) wird die Nummer der Kante zugewiesen. (z.B.) ELEM(1000,2)=45
        elem (j, elem (j, 1) ) = i
      ENDIF
    ENDDO
  ENDIF
ENDDO
!-

!NiS,may06: For that no error message is disturbing while reading the code above, it is written at the end of the Loop
 9003 format ('Just informational:',/' No elements assigned to ARC ',i6,/'DEAD ARC!')

!NiS,mar06: unit name changed; changed iout to Lout
WRITE (Lout, 110 )
WRITE ( *  , 110 )
110 FORMAT (//1X, '--------- Checking the mesh -------------')

! ASSIGNING NODES TO ELEMENTS WITHOUT SORTING ------------------------------

!adaptation of all_elem-DO-LOOP for 1D-ELEMENTS; some changes.
! Knotennummern am Element (linksherum) einordnen
! dazu alle Elemente durchgehen
elzaehl = 0

!WP Schleife ueber alle Elemente (1...elcnt)
all_elem: DO i = 1, elcnt                                         !In the loop for every element in elcnt the following steps are made:
  !WP Initialisieren des Elementes
  kno: DO j = 1, 4
    elkno (j) = 0
    mikno (j) = 0
  END DO kno
  elkno (5) = 0
  ! leere Elementnummern uebergehen:
  IF (elem (i, 1) .eq. 0) CYCLE all_elem                          !if the element with the actual number i is 0 then cycle and try the next

  !count the number of NOT-empty entries of elcnt
  elzaehl = elzaehl + 1

  !normal 1D-elements --------------------
  dimensionif: IF (elem(i,1) .eq. -1) THEN
    !for normal 1D-elements, the number of nodes is 3 and the number of corner nodes is 2
    jnum = 2
    ncorn(i) = 3

    !Passing corner nodes to node array
    nop(i,1) = arc(elem(i,2),1)
    nop(i,3) = arc(elem(i,2),2)

    !giving over midsidenode, if present, to temporary node array
    IF (arc(elem(i,2),5) .gt. 0) THEN
      nop(i,2) = arc(elem(i,2),5)
    ENDIF

  !1D-2D-transition elements -------------
  ELSEIF (elem(i,1) .EQ. -2) THEN
    !for transition elements, the corner nodes were defined, with an eventual exception of the midside node
    jnum = 2
    ncorn(i) = 5

  !2D-elements ---------------------------
  ELSE
    ! Anzahl Elementkanten => jnum
    !WP Entspricht nicht ELEM(i,1), sondern ELEM(i,1)-1 (siehe oben)
    jnum = elem (i, 1) - 1                                          !jnum = 3 for triangle; jnum = 4 for quadrilateral; other values are errors
    !WP Anzahl der Knoten im Element (inklusive Mittseitenknoten)   !(=number of corner nodes; midside nodes or arcs as the user wants)
    ncorn (i) = jnum * 2                                            !ncorn = 6 for triangle and ncorn = 8 for quadrilateral; other values are errors
                                                                    !(=number of nodes, midsides and corners, within the actual elements)

    !NiS,may06: With 1D-ELEMENTS error only occurs, if (jnum.eq.1 or .eq.2), because if (jnum.lt.0), it's an 1D-ELEMENT and (jnum.eq.0) was cycled.
    !IF (jnum.lt.3) THEN

    !ERROR - element has less than 3 arcs, which is not possible
    IF (jnum == 1 .or. jnum == 2) call ErrorMessageAndStop (1203, i, cord(arc (elem (i, 2), 5), 1) , cord(arc (elem (i, 2), 5), 2))

    ! erste Kante:                                                  !starting with the first arc, the element's nodes in anticlockwise direction
    l = 1                                                           !will be saved in a temporary array to write them later into the array nop.

    ! akt. Element links der Kante .und. unten-Knoten beginnt:      !the two arrays for temporary saving are:
    IF (arc (elem (i, 2), 3) .eq.i) THEN                            !       elkno(1...5)    =       corner nodes of element
      elkno (1) = arc (elem (i, 2), 1)                              !       mikno(1...4)    =       midside nodes of element, if defined
      elkno (2) = arc (elem (i, 2), 2)
      IF (arc (elem (i, 2), 5) .gt. 0) THEN                         !In dependency of the side the actual element is positioned in relation to
        mikno (1) = arc (elem (i, 2), 5)                            !the first arc, the nodes are saved in elkno(1) and elkno(2)
      ENDIF
    ! akt. Element rechts der Kante .und. oben-Knoten beginnt:
    ELSE
      elkno (1) = arc (elem (i, 2), 2)
      elkno (2) = arc (elem (i, 2), 1)
      IF (arc (elem (i, 2), 5) .gt. 0) THEN
        mikno (1) = arc (elem (i, 2), 5)
      ENDIF
    ENDIF

!TODO: Introduce modern do-loop
    ! weitere Kanten:                                       	    !The other two or three arcs defining the actual element i are analysed from
2222   l = l + 1                                               	    !this point on. The jumpmark 2222 is somthing like a do loop.
    IF (l.gt.jnum) THEN                                       	    !The first if-case checks whether the actual arc l is the last one to define
      IF (elkno (1) .ne. elkno (l)) call ErrorMessageAndStop (1204, i, 0.0d0, 0.0d0)

      GOTO 2444
    END IF
    iakt = elkno (l)                                        	    !this is not necessary becaus elkno(l) can be used in the following if-construct
    elem_arc: DO j = 2, jnum                                        !For every left arc with exception of the first, dealt with above, it is checked,
      ! Element links der Kante .und. unten-Knoten knuepft an?      !whether it is the one that is connected to the last node of the last arc.
      left: IF ((arc (elem (i,j+1),3) .eq. i) .AND. (arc (elem (i,j+1),1) .eq. iakt)) then
        elkno (l + 1) = arc (elem (i, j + 1), 2)                    !In dependency of the side the actual element is positioned in relation
        IF (arc (elem (i, j + 1), 5) .gt.0) then                    !to the arc j, the node that could be connected with the last one of the last
          mikno (l) = arc (elem (i, j + 1), 5)                      !arc is checked, whether it is connected. If so the procedure jumps to the
        END if                                                      !next arc and increases the number of l
        GOTO 2222
      END IF left
      ! Element rechts der Kante .und. oben-Knoten knuepft an?
      right: IF ((arc (elem (i,j+1),4) .eq. i) .AND. (arc (elem (i,j+1),2) .eq. iakt)) then
        elkno (l + 1) = arc (elem (i, j + 1), 1)
        IF (arc (elem (i, j + 1), 5) .gt.0) then
          mikno (l) = arc (elem (i, j + 1), 5)
        END if
        GOTO 2222
      END IF right
    END DO elem_arc


    !ERROR - Element is not forming a linear ring
    call errorMessageAndStop (1204, i, 0.0d0, 0.0d0)

  ! Element O.K.
2444 CONTINUE                                                       !If everything is okay, the cornernodes will be entered in the nop-array
    ! => Eckknotennummern ins nop-feld eintragen -------------------!in every second position starting with the first. If defined yet the midside
    DO j = 1, jnum                                                  !nodes will be entered in every second position starting with the second slot
      nop (i, j * 2 - 1) = elkno (j)                                !of the nop-array.
    END DO

    ! => Mittseiten-knotennummern ins nop-feld eintragen -----------
    DO j = 1, jnum
      IF (mikno (j) .gt.0) then

        !ERROR - midside node ID is higher than maximum node ID; that doesn't work
        IF (mikno (j) > nodecnt) call errorMessageAndStop (1109, mikno (j), 0.0d0, 0.0d0)

        nop (i, j * 2) = mikno (j)
      ENDIF
    END DO
    
    !nis,jul08: Copy nodes to mops array
    do j = 1, 8
      nops (i, j) = nop (i, j)
    enddo

    ! Kantenkreuzung,Verdrehung abfragen  --------------------------!the actual element is checked for crossing arcs or something else,
    crossing_outer: DO j = 1, jnum - 2                                !not congruent.

      vekkant (1) = cord (nop (i, (j + 1) * 2 - 1), 1) - cord (nop (i, j * 2 - 1), 1)
      vekkant (2) = cord (nop (i, (j + 1) * 2 - 1), 2) - cord (nop (i, j * 2 - 1), 2)

      crossing_inner: DO k = j + 2, jnum

        vekpu1 (1) = cord (nop (i, k * 2 - 1), 1) - cord (nop (i, j * 2 - 1), 1)
        vekpu1 (2) = cord (nop (i, k * 2 - 1), 2) - cord (nop (i, j * 2 - 1), 2)
        kreuz = vekkant (1) * vekpu1 (2) - vekkant (2) * vekpu1 (1)

        ! write(*,*)'Element ',i, 'kreuz(',j,',',k,')=',kreuz
        IF (kreuz <= 0.0) call ErrorMessageAndStop (1205, i, 0.5 * (cord (nop (i, 1), 1) + cord (nop (i, 3), 1)), &
                                                                 & (cord (nop (i, 1), 2) + cord (nop (i, 3), 2)))

      END DO crossing_inner
    END DO crossing_outer
  ENDIF dimensionif
END DO all_elem

!LF nov06 renumbering the nodes around weir elements
if (weircnt .gt. 0) then
  do i = 1, weircnt
    if (nop (reweir (i, 2), 1) .ne. reweir (i, 1)) call reweir2dKALYPSO (i)
  end do
end if
!-

! CONTROLOUTPUT -------------------------------------------------------------------------------------------

!Controloutput
WRITE (Lout,103) elcnt, nodecnt, arccnt
WRITE ( *  ,103) elcnt, nodecnt, arccnt
WRITE (Lout,102) elzaehl
WRITE (*   ,102) elzaehl
103 format (1X, 'dimension of element-array   : ', I6 / &
          & 1X, 'dimension of cornernodes     : ', I6 / &
          & 1X, 'dimension of arcs            : ', I6 /)
102 format (1X, 'Number of activ elements     : ', I6 / &
          & 1X, 'Checking mesh O.K.!'/)


! GENARATION OF REQUIRED MIDSIDE NODES ---------------------------------------------------------------------

!Erzeugen der erforderlichen Mittseitenknoten
!initializing the counter variable for additional midside nodes.
mittzaehl = 0
                                                                        
!NiS,mar06: Changed DO-LOOP to cycle it clearly
!DO 1400 i = 1, arccnt
all_arcs: DO i=1,arccnt

  !dead arcs have to be skipped
  if (arc(i,1).eq.0) CYCLE all_arcs

  ! Mittseitenknoten vorhanden?
  !NiS,expand test for defined midside nodes in ARC-array but without coordinate-definitions; this was a logical gap
  IF ((arc(i,5).gt.0) .and. (arc(i,5).le.nodecnt)) THEN
    IF ((cord (arc (i, 5), 1) /= 0.0) .and. (cord (arc (i, 5), 2) /= 0.0)) THEN
      if (ao (arc (i, 5)) + 9999.0 < 1.0e-3) then
        WRITE(lout,*) 'recalculating elevation        '
        ao (arc (i, 5)) = 0.5 * (ao (arc (i, 1)) + ao (arc (i, 2)))
        if (kmx (arc(i,1)) /= 0.0 .and. kmx (arc(i, 2)) /= 0.0) then
          kmx (arc (i, 5)) = 0.5 * (kmx (arc (i, 1)) + kmx (arc (i, 2)))
        end if
      end if
      CYCLE all_arcs
    ELSE
      !Test for distances
      a = SQRT ( ( cord (arc (i,1), 1) - cord ( arc (i,5), 1) )**2 + ( cord (arc (i,1), 2) - cord (arc (i,5), 2) )**2)
      b = SQRT ( ( cord (arc (i,2), 1) - cord ( arc (i,5), 1) )**2 + ( cord (arc (i,2), 2) - cord (arc (i,5), 2) )**2)
      c = SQRT ( ( cord (arc (i,1), 1) - cord ( arc (i,2), 1) )**2 + ( cord (arc (i,1), 2) - cord (arc (i,2), 2) )**2)
      IF (a.lt.c .or. b.lt.c) THEN
        WRITE (*,1234) arc(i,5), i, arc(i,5)
1234    FORMAT (' The NODE ', I5,' is defined in ARC ', i5,'. The Coordinates are the origin (0.0/0.0), but this seems '/ &
              & ' not be define but the default initialized value, because the distance between the corner nodes of the '/&
              & ' arc is shorter than one of the distances between the midside node and the corner nodes! Therefore the '/&
              & ' coordinates of the node ', I56, ' are recalculated.')
        !Recalculation with Linear interpolation of coordinates for nodes, that were not logical before
        cord (arc(i,5),1) = 0.5 * (cord (arc(i,1),1) + cord (arc(i,2),1) )
        cord (arc(i,5),2) = 0.5 * (cord (arc(i,1),2) + cord (arc(i,2),2) )
          ao (arc(i,5)  ) = 0.5 * (  ao (arc(i,1)  ) +   ao (arc(i,2)  ) )
        if (kmx (arc(i,1)) /= 0.0 .and. kmx (arc(i, 2)) /= 0.0) then
          kmx (arc (i, 5)) = 0.5 * (kmx (arc (i, 1)) + kmx (arc (i, 2)))
        end if
      ENDIF
    ENDIF
  ELSE
  !If a new midside node is generated, the program is told which ID to take for that midsidenode; the ID
  !is the result of increasing the actual maximum active node number by 1.
  nodecnt = nodecnt + 1          
  !Increase the counter for added new midside nodes
  mittzaehl = mittzaehl + 1

  !These lines could be economized with using the arc-array directly, where it is needed; WHY COPY?
  ibot = arc (i, 1)
  itop = arc (i, 2)
  ilft = arc (i, 3)
  irgt = arc (i, 4)
  !NiS,may06: Test for dead arcs, so the DO-LOOP may be cycled:
  IF (ilft.eq.irgt .and. ilft.eq.0) CYCLE all_arcs

  !coordinates of generated midside node
  x = (cord (ibot, 1) + cord (itop, 1) ) / 2.0
  y = (cord (ibot, 2) + cord (itop, 2) ) / 2.0
  z = (ao (ibot) + ao (itop) ) / 2.0
                                                                        
  !Install the temporary values in the proper global array lines; these lines could be economized with calculating directly without locals
  cord (nodecnt, 1) = x
  cord (nodecnt, 2) = y
  ao   (nodecnt)    = z

  !NiS,may06: test for 1D- or 2D-ARC
  !1D-ELEMENT ARC or 1D-2D-TRANSITION ELEMENT ARC:
  IF (ilft == irgt) THEN
    !1D-2D-TRANSITION-ELEMENTS may have a midside node already
    IF (nop(ilft,2) /= 0) CYCLE all_arcs
    !all other normal 1D-ELEMENTS or 1D-2D-TRANSITION-ELEMENTS get a midside node
    nop(ilft, 2) = nodecnt

  !2D-ELEMENT ARC:
  ELSE
    IF (ilft > 0) THEN
      jnum = elem (ilft, 1) - 1
      LeftElt: DO j = 1, jnum
        IF (nop (ilft, 2 * j - 1) == ibot) THEN
          nop (ilft, 2 * j) = nodecnt
          exit LeftElt
        ENDIF
      enddo LeftElt
    ENDIF

    IF (irgt > 0) THEN
    !changes see above
    !jnum = elem (irgt, 1)
        jnum = elem (irgt, 1) - 1
        DO j = 1, jnum
          IF (nop (irgt, 2 * j - 1) == itop) THEN
            nop (irgt, 2 * j) = nodecnt
            CYCLE all_arcs
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  !NiS,may06: IF clause for coordinate test
  ENDIF
END DO all_arcs

!NiS,mar06: unit name changed; changed iout in Lout
!user informations
WRITE (Lout,106) mittzaehl
WRITE (*   ,106) mittzaehl
106 FORMAT (1X, 'Number of added     '/ &
          & 1X, 'midside nodes:      ', I6/)

!NiS,may06: Checking the cross section informations at corner nodes of 1D-ELEMENTS and interpolating them for midside nodes of 1D-ELEMENTS
! CHECKING/INTERPOLATION OF CROSS SECTION INFORMATIONS -----------------------------------------------------------------------------------

DO i = 1, arccnt
  IF (arc(i,3) == arc(i,4) .and. arc(i,3) /= 0) then
    if (imat(arc(i,3)) < 900) THEN

    if (imat (arc (i,3)) /= 89) then
      checkwidths: do j = 1, 3, 2
        ND = nop (arc (i, 3), j)
        !ERROR -  if one of the two corner nodes does not have cross sectional informations
        IF (WIDTH (nd) == 0.0) CALL ErrorMessageAndStop (1103, nd, cord (nd, 1), cord (nd, 2))
      end do checkwidths

      IF (width (nop (arc (i, 3), 2)) == 0.0) THEN
        width  (nop (arc (i, 3), 2)) = 0.5 * (width (nop (arc (i, 3), 1)) + width (nop (arc (i, 3), 3)))
        ss1    (nop (arc (i, 3), 2)) = 0.5 * (ss1   (nop (arc (i, 3), 1)) + ss1   (nop (arc (i, 3), 3)))
        ss2    (nop (arc (i, 3), 2)) = 0.5 * (ss2   (nop (arc (i, 3), 1)) + ss2   (nop (arc (i, 3), 3)))
        wids   (nop (arc (i, 3), 2)) = 0.5 * (wids  (nop (arc (i, 3), 1)) + wids  (nop (arc (i, 3), 3)))
        widbs  (nop (arc (i, 3), 2)) = 0.5 * (widbs (nop (arc (i, 3), 1)) + widbs (nop (arc (i, 3), 3)))
        wss    (nop (arc (i, 3), 2)) = 0.5 * (wss   (nop (arc (i, 3), 1)) + wss   (nop (arc (i, 3), 3)))
      ENDIF
      endif
    ENDIF
  ENDIF
ENDDO


!TODO: This should call either with alphapoly or betapoly depending on what is used!
call InterpolateProfs (statElSz, statNoSz, MaxP, MaxE, maxIntPolElts, IntPolNo, NeighProf, ncorn, nop, &
                    & cord, ao, kmx, kmWeight, IntPolProf, IntPolElts, qgef, imat, TransitionElement, &
                    & MaxLT, TransLines, IsPolynomNode)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,mar06: COMMENTS
!The following calculations are Kalypso-2D specific things. For adaption of RMA10S for some aspects of Kalypso-2D (COLEBROOK WHITE rougness,
!PASCHE/LINDNER tree roughness, turbulence modells etc.) the following processes will remain included in this subroutine.
!
!The compilation of the network is complete up to this point. There are some optimizations possible above.
!
!In the control lines of RMA10S is a new switch for CVFEM included. So later this code could be changed for CVFEM and the subroutines already
!dealing with the FEM-switch are currently correctly running with the default value for GALERKIN method (default FEM.eq.0).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!NiS,mar06: Why copy? sense not clear!
n  = nodecnt
m  = elcnt
np = MaxP
ne = MaxE

!---------------------------------------------------------------------------------------------------------------------------------------------
!nis,jan07: Work on Line transitions
!---------------------------------------------------------------------------------------------------------------------------------------------
!Turning the transitioning elements, if necessary; while doing this: Testing, whether transition is properly defined
if (maxlt /= 0) then
  !run through all possible transitionings
  elementturning: do i = 1, maxlt
    !if transition is dead, go to next one
    if (TransLines (i, 1) == 0) CYCLE elementturning
    !test for correct order of nodes in transitioning element
    if (nop (TransLines (i, 1), 3) .ne. TransLines (i, 3)) then
      !nis,jan07: Checking, whether node is defined in element on the other slot (slot 1)
      if (nop (TransLines (i, 1), 1) /= TransLines (i, 3)) then!&
     !&  call ErrorMessageAndStop (1403, i, cord (nop (TransLines (i, 1), 3), 1), cord (nop (TransLines (i, 1), 3), 1))
      call ErrorMessageAndStop (1403, i, cord (nop (TransLines (i, 1), 3), 1), cord (nop (TransLines (i, 1), 3), 1))
      endif
    end if
  end do elementturning
endif

! REORDERING -------------------------------------------------------------------
DO i = 1, elcnt
  elfix (i) = 0
END do

ReorderingNotDone = .true.
DO i = 1, elcnt
  IF (nfixh (i) <= 0 .or. nfixh (i) > MaxE) then
    !NiS,mar06: unit name changed; changed iout to Lout
    WRITE (Lout,105)
    WRITE ( * , 105)
    !NiS,may06: In RMA10S a subroutine called reord.subroutin exists; changed reord to reord_Kalyps
    CALL start_node (qlist, k, MaxP)
    call reord_Kalyps (qlist)
    ReorderingNotDone = .false.
  endif
  IF (imat (nfixh (i)) > 0) elfix (nfixh (i)) = elfix (nfixh (i)) + 1
END DO

if (ReorderingNotDone) then
  DO i = 1, elcnt
    IF ((imat (i) > 0) .and. (elfix (i) /= 1) ) then
      !NiS,mar06: unit name changed; changed iout to Lout
      WRITE (Lout,105)
      WRITE ( * , 105)
      !NiS,may06: In RMA10S a subroutine called reord.subroutin exists; changed reord to reord_Kalyps
      CALL start_node (qlist, k, MaxP)
      call reord_Kalyps (qlist)
    endif
  END DO
endif 

write (Lout, 107)
write ( * ,  107)

!Output formats
105 format (1X, 'Reordering has to be done.')
107 format (1X, 'Order of processing (NFIXH) is sufficiently correct')


! CALCULATION OF CENTERS OF ELEMENTS  ----------------------------------------------------------------
GetMiddleCoord: do i = 1, ne
  if (imat (i) == 0) cycle getMiddleCoord
  sumx  = 0.0
  sumy  = 0.0
  !loop over all nodes, including midside nodes of an all elements
  !to determine the coordinates of the geometrical center of the element
  do j = 1, ncorn(i)
    sumx = sumx + cord(nop(i,j),1)
    sumy = sumy + cord(nop(i,j),2)
  end do
  !averaging of the values
  mcord(i,1) = sumx/ncorn(i)
  mcord(i,2) = sumy/ncorn(i)
END do GetMiddleCoord


! NEIGHBOURHOOD RELATIONS OF NODES -------------------------------------------------------------------
!Initialize arrays
do i = 1, MaxP
  !neighb(i, j) : ID-number of j-th neighbour of node i
  !nconnect (i) : number of neighbourhood connections
  !It's assumed that a node has maximum 30 neighbours
  do j = 1, 30
    neighb (i, j) = 0
  end do
  nconnect(i) = 0       
end do

!run through all elements
neighbours: do i = 1, MaxE
  !Skip empty elements
  if (elem (i, 1) /= 0) then
    !Check for 1D-elements that are connecting to a 2D-model part
    !Initializing ConnNumber
    ConnNumber = 0
    !Look, whether element is part of any transition
    findconnection: do j = 1, MaxLT
      if (TransLines (j, 1) == i) then
        ConnNumber = j
        ConnLine = TransLines(j,2)
        EXIT findconnection
      end if
      if (j == MaxLT) ConnNumber = 0
    end do findconnection
    !If there is a connection assigned to that element process, store the 2D-neighbours
    if (ConnNumber /= 0) then
      !number of connections at this special element (including all 2D-transitioning nodes and the 1D-element's node)
      ncorn_temp = lmt (ConnLine) + 3
      !this is the temporary nop-array for the '1D-2D-transitionline-element'
      ALLOCATE (nop_temp (1 : ncorn_temp))
      !overgive the three nodes of the 1D-part; they are already sorted:
      !1: connection to next 1D-element
      !2: midside node
      !3: connection to transition line
      do k = 1, 3
        nop_temp(k) = nop(i,k)
      end do
      !overgiving the nodes into the temporary array nop_temp
      nodeassigning: do j = 1, lmt (ConnLine)
        !Add 3, because they are reserved for the 1D-element
        nop_temp (j + 3) = line (ConnLine, j)
      end do nodeassigning
      !store neighbourhood relations, it's nearly the same loop as in the original loop, shown below
      outerLT: do j = 1, ncorn_temp
        node1 = nop_temp (j)
        innerLT: do l = 1, ncorn_temp
          node2 = nop_temp (l)
          if (node1 /= node2) then
            nconnect (node1) = nconnect (node1) + 1
            neighb (node1, nconnect(node1)) = node2
          end if
        end do innerLT
      end do outerLT
      !array resetting for next transition, that is probably be from other size
      DEALLOCATE(nop_temp)
    endif
    !Read all node numbers of adjacent element
    outer: do j = 1, ncorn (i)
      !For increasing speed of program, bring line to outside of loop
      node1 = nop (i, j)
      inner: do l = 1, ncorn (i)
        node2 = nop (i, l)
        IF (node1 /= node2) THEN
          nconnect(node1) = nconnect(node1) + 1
          neighb (node1, nconnect (node1)) = node2
        END if
      end do inner
    end do outer
  end if
end do neighbours

!Delete doubled entries in the nconnect array
do i = 1, nodecnt
  j = 1
  !Run through each entry
  do
    if (j >= nconnect(i)) exit
    k = j + 1 
    vergleich: do
      !If entries on j-th position and k-th position are the same,
      !delete the j-th entry, push all entries after the k-th on step forward
      if (neighb(i,j) == neighb(i,k)) then
        !push k-th and upward
        do l = k, nconnect (i) - 1
          neighb (i, l) = neighb (i, l + 1)
        end do
        !Reduce the number of neighbouring nodes
        nconnect(i) = nconnect(i)-1
        EXIT vergleich
      end if
      if (k >= nconnect(i)) exit
      k = k + 1
    end do vergleich
    j = j + 1
  end do
end do

!generate upward knowledge, store elements connected to nodes
BelongingElement: do i = 1, MaxE
  !Skip emtpy elements
  if (ncorn(i) == 0) CYCLE belongingElement
  !run through corner nodes
  throughNodes: do j = 1, ncorn(i)
    throughPossibleEntries: do k = 1, 12
      if (IsNodeOfElement(nop(i, j), k) == i) EXIT throughPossibleEntries
      if (IsNodeOfElement(nop(i, j), k) == 0) then
        IsNodeOfElement(nop(i, j), k) = i
        !first column is counter
        IsNodeOfElement(nop(i, j), 0) = isNodeOfElement(nop(i, j), 0) + 1
        CYCLE throughNodes
      end if
    enddo throughPossibleEntries
    !ERROR - There are too many elements connected to one node
    call ErrorMessageAndStop (1110, nop (i, j), cord (nop (i, j), 1), cord (nop (i, j), 2))
  end do throughNodes
ENDDO BelongingElement


!NiS,mar06: unit name changed; changed iout to Lout
WRITE (Lout, 111 )
WRITE ( *  , 111 )
  111 FORMAT (/1X, 'Reading model finished',/ &
            &  1X, '-----------------------------------------'//)

!deallocation of temporary arrays
DEALLOCATE (arc, reweir)
!Rewind for possible RESTART
REWIND(IFILE)

RETURN

END SUBROUTINE RDKALYPS
                                                                        


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine InterpolateProfs (statElSz, statNoSz, MaxP, MaxE, MaxIntPolElts, IntPolNo, NeighProf, ncorn, nop, &
                          & cord, ao, kmx, kmWeight, IntPolProf, IntPolElts, qgef, imat, TransitionElement, MaxLT, &
                          & TransLines, IsPolynomNode)

implicit none
INTEGER, intent (IN)    :: statElSz, statNoSz, MaxP, MaxE, MaxLT, MaxIntPolElts
integer, intent (in)    :: IntPolNo (1: MaxE)
INTEGER, intent (INOUT) :: IntPolElts (1: MaxE, 1: MaxIntPolElts)
integer, intent (inout) :: NeighProf (1: MaxP, 1: 2)
integer, intent (inout) :: ncorn (MaxE)
integer, intent (inout) :: nop (1: MaxE, 1: 8), imat (1: MaxE)
real, intent (inout)    :: cord (1: MaxP, 1: 2)
REAL, intent (inout)    :: ao (1: MaxP), kmx (1: MaxP), kmWeight (1: MaxP)
REAL, INTENT (INOUT)    :: qgef (1: MaxP)
LOGICAL, INTENT (INOUT) :: IntPolProf (1: MaxP), IsPolynomNode (1: MaxP), TransitionElement (1: MaxE)
INTEGER, INTENT (INOUT) :: TransLines (1: MaxLT, 1: 3)
!
!*********local ones***********
INTEGER :: i, j
integer :: NewNode, NewElt, TransLine
real (kind = 8) :: DX, DY, DH, DIST, origx, origy


DIST = 0.0D0
NewNode = statNoSz
NewElt = statElSz
!nis,jan08: Interpolate new profiles into mesh
do i = 1, statElSz
  !here an interpolation should take place
  if (IntPolNo (i) /= 0 .and. ncorn (i) < 4) then
    DX = (cord (nop (i, 3), 1) - cord (nop (i, 1), 1)) / (IntPolNo (i) + 1)
    DY = (cord (nop (i, 3), 2) - cord (nop (i, 1), 2)) / (IntPolNo (i) + 1)
    origx = (cord (nop (i, 1), 1) + cord (nop (i, 3), 1)) / 2.0d0
    origy = (cord (nop (i, 1), 2) + cord (nop (i, 3), 2)) / 2.0d0
    DH = ( ao (nop (i, 3)) - ao (nop (i, 1))) / (IntPolNo (i) + 1)
    if (kmx (nop(i, 1)) >= 0.0 .and. kmx (nop (i, 3)) >= 0.0) then
      DIST = (kmx (nop (i, 3)) - kmx (nop (i, 1))) / (IntPolNo (i) + 1)
    else
      DIST = 0.0D0
    end if

    do j = 1, IntPolNo (i)
      !transform the first intersection element
      if (j == 1) then
        nop (i, 4) = nop (i, 3)
        NewNode = NewNode + 1
        nop (i, 3) = NewNode

        call GenNewNode (NewNode, i, origx, origy, nop(i, 1), nop (i, 4), NeighProf (NewNode, :), IntPolProf (NewNode), &
                      &  qgef(nop(i, 1)), qgef (nop (i, 4)), qgef (NewNode), IsPolynomNode (NewNode))
        cord (nop (i, 3), 1) = cord (nop (i, 1), 1) + DX * j
        cord (nop (i, 3), 2) = cord (nop (i, 1), 2) + DY * j
        ao (NewNode) = ao (nop (i, 1)) + DH
        kmWeight (NewNode) = j / REAL(IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNode) = kmx (nop (i, 1) ) + j * DIST
        else
          kmx (NewNode) = -1.0D0
        end if
        !recalculate the position of the midside node at first element
        call GenNewNode (nop (i, 2), i, origx, origy, nop (i, 1), nop (i, 4), NeighProf (nop (i, 2), :), IntPolProf (nop (i, 2)), &
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (nop (i, 2)), IsPolynomNode (nop (i, 2)))
        cord (nop (i, 2), 1) = 0.5 * (cord (nop (i, 1), 1) + cord (nop (i, 3), 1))
        cord (nop (i, 2), 2) = 0.5 * (cord (nop (i, 1), 2) + cord (nop (i, 3), 2))
        ao (nop (i, 2)) = 0.5 * (ao (nop (i, 1)) + ao (nop (i, 3)))
        kmWeight (nop (i, 2)) = 0.5 / REAL (IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (nop (i, 2)) = kmx (nop (i, 1)) + 0.5 * DIST
        end if
      !generate the middle elements of interpolation, it's first interesting, if there are more than 2 intersections
      elseif (j > 1) then
        NewElt = NewElt + 1
        IntPolElts (i, j - 1) = NewElt
        imat (NewElt) = imat (i)
        if (j == 2) then
          nop (NewElt, 1) = nop (i, 3)
        else
          nop (NewElt, 1) = nop (NewElt - 1, 3)
        end if
        NewNode = NewNode + 1
        nop (NewElt, 3) = NewNode
        call GenNewNode (NewNode, i, origx, origy, nop (i, 1), nop (i, 4), NeighProf (NewNode, :), IntPolProf (NewNode), &
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (NewNode), IsPolynomNode (NewNode))
        cord (NewNode, 1) = cord (nop (i, 1), 1) + DX * j
        cord (NewNode, 2) = cord (nop (i, 1), 2) + DY * j
        ao (NewNode) = ao (nop (i, 1)) + DH * j
        kmWeight (NewNode) = j / REAL(IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNode) = kmx (nop (i, 1) ) + j * DIST
        else
          kmx (NewNode) = -1.0D0
        end if
        !generate midside node
        NewNode = NewNode + 1
        nop (NewElt, 2) = NewNode
        call GenNewNode (NewNode, i, origx, origy, nop (i, 1), nop (i, 4), NeighProf (NewNode, :), IntPolProf (NewNode), &
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (NewNode), IsPolynomNode (NewNode))
        cord (nop (NewElt, 2), 1) = 0.5 * (cord (nop (NewElt, 1), 1) + cord (nop (NewElt, 3), 1))
        cord (nop (NewElt, 2), 2) = 0.5 * (cord (nop (NewElt, 1), 2) + cord (nop (NewElt, 3), 2))
        ao (NewNode) = 0.5 * (ao (nop (NewElt, 1)) + ao (nop (NewElt, 3)))
        kmWeight (NewNode) = (j - 0.5) / REAL (IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNode) = kmx (nop (i, 1)) + (j - 0.5) * DIST
        end if
      end if
      !Generate the last element
      if (j == IntPolNo (i) ) then
        NewElt = NewElt + 1

        if (TransitionElement(i)) then
          findtransition: do TransLine = 1, MaxLT
            if (TransLines (TransLine, 3) == nop (i, 4)) then
              TransitionElement(i) = .false.
              TransitionElement(NewElt) = .true.
              TransLines (TransLine, 1) = NewElt
              EXIT findtransition
            end if
          end do findtransition
        end if

        IntPolElts (i, j) = NewElt
        imat (NewElt) = imat (i)
        if (j == 1) then
          nop (NewElt, 1) = nop (i, 3)
        else
          nop (NewElt, 1) = nop (NewElt - 1, 3)
        end if
        nop (NewElt, 3) = nop (i, 4)
        nop (i, 4) = 0
        !generate midside
        NewNode = NewNode + 1
        nop (NewElt, 2) = NewNode
        call GenNewNode (NewNode, i, origx, origy, nop (i, 1), nop (NewElt, 3), NeighProf (NewNode, :), IntPolProf (NewNode), &
                      &  qgef (nop (i, 1)), qgef (nop (NewElt, 3)), qgef (NewNode), IsPolynomNode (NewNode))
        cord (NewNode, 1) = 0.5 * (cord (nop (NewElt, 1), 1) + cord (nop (NewElt, 3), 1))
        cord (NewNode, 2) = 0.5 * (cord (nop (NewElt, 1), 2) + cord (nop (NewElt, 3), 2))
        ao (NewNode) = 0.5 * (ao (nop (NewElt, 1)) + ao (nop (NewElt, 3)))
        kmWeight (NewNode) = (j + 0.5) / REAL (IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNode) = kmx (nop (i, 1)) + (j + 0.5) * DIST
        end if
      end if
    end do
  end if
end do
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine GenNewNode (NewNode, OrigElt, cordx, cordy, neighbour1, neighbour2, NewNeighProf, LogIntPolProf, qgef1, qgef2, qgefOut, &
&                      IsPolynomNode)
implicit none

!input
integer, intent (in)          :: NewNode
INTEGER, INTENT (IN)          :: OrigElt
REAL (KIND = 8), INTENT (IN)  :: cordx, cordy
REAL (KIND = 8), INTENT (IN)  :: qgef1, qgef2
INTEGER, INTENT (IN)          :: neighbour1, neighbour2

!output
integer, intent (out)         :: NewNeighProf (1:2)
REAL (KIND = 8), INTENT (OUT) :: qgefOut
Logical, intent (out)         :: LogIntPolProf, IsPolynomNode

NewNeighProf(1) = neighbour1
NewNeighProf(2) = neighbour2

if (qgef1 /= qgef2) then
  WRITE(*,*) neighbour1, neighbour2
  call ErrorMessageAndStop (1111, OrigElt, cordx, cordy)
else
  qgefOut = qgef1
end if
LogIntPolProf = .true.
IsPolynomNode = .true.
END subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

