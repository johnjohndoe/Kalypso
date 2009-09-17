module mod_RDKalypso_routines

contains

!     Last change:  MD   10 Jun 2009    5:16 pm
SUBROUTINE RDKALYPS(nodecnt, elcnt, arccnt, PolySplitCountA, PolySplitCountQ, PolySplitCountB, TLcnt, psConn, maxSE, KSWIT, m_SimModel)
!nis,feb07: Allow for counting the midside nodes of FFF elements
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
!data type modules
use mod_Nodes
use mod_Model
use mod_meshModelFE

use blk10mod , only: &
&   maxe, maxa, maxp, maxt, maxlt, maxps, ncl, &
&   width, ss1, ss2, wids, widbs, wss, &
&   ifile, nb, lout, id, &
&   title, &
&   nop, nops, ncorn, imat, nfixh, &
&   cord, ao, &
&   line, lmt, &
&   translines, PipeSurfConn, ConnectedElt, &
&   StorageElts, &
&   vel, vold, vdot, vdoto, &
&   iyrr, tett, &
&   ccls
!meaning of the variables
!------------------------
!maxp       maximum number of points (surface points; includes midsides)
!maxe       maximum number of elements
!maxa       maximum number of arcs (auxiliary mean to generate mesh)
!maxt       maximum number of 1D/2D transition elements for 1D/2D element-2-element transitions
!           maxt is set up in getgeo, because only if the geometry of all elements is recognized
!           it can be determined, whether and how many 1D/2D node-2-element transitions are present
!maxlt      maximum number of 1D/2D transition elements for 1D/2D line-2-element transitions
!maxps      maximum number of pipe surface connections
!ncl        number of continuity lines
!width      Channel bed width of a 1D-node within the trapezoidal geometry approach
!wids       storage width (flood plain width) of a 1D-node within the trapezoidal geometry approach
!ss1        1st Channel side slope of a 1D-node within the trapezoidal geometry approach
!ss2        2nd Channel side slope of a 1D-node within the trapezoidal geometry approach
!ifile      model geometry (in *.2d-format)
!lout       after 1st call of file.sub unit of output file (.ech)
!           after 2nd call of file.sub LOUT becomes the general output unit (.out-file)
!nb         restart input file (binary or ASCII)
!id         character variable into which the first 8 digits of any input line from a file are read
!title      title entry
!nop        nodes of an element, anticlockwise (corner - midside - sequence)
            !TODO: Are nops and nop both necessary?
!nops       the same as nop
!ncorn      number of corner nodes per element
!imat       material type of an element
!nfixh      reordering number; position at which element will be solved
!cord       x-, y- and ?-coordinates of a node
!ao         bed elevation of a node
!line       continuity line definition; stores all the nodes 
!lmt        number of nodes within continuity line
!translines data of transition lines; transitioning line, 1D-element, and node
!PipeSurfConn
!           data of pipe surface connections
!           1 - Surface element ID
!           2 - pipe element ID
!vel        The degree of freedom field; it's the key array
!vdot       derivative of the degrees of freedom
!vold       The degree of freedom field from the last time step; i.e. storage of old results
!vdoto      derivative of the degrees of freedom from the last time step; i.e. storage of old results
!for all counts:
!           1 - x-velocity
!           2 - y-velocity
!           3 - water depth
!           4 - salinity
!           5 - temperature
!           6 - suspended sediment
            !TODO: what is vel(7,x)
!           7 - ??? perhaps water column potential by element

use blkdrmod, only: imato, ndry
!meaning of the variables
!------------------------
!imato      old material type of an element from the last time step


use parakalyps , only: &
&   correctionks, correctionaxay, correctiondp, &
&   lambdatot, lambdaks, lambdap, lambdadunes, &
&   intpolprof, ispolynomnode, transitionelement, &
&   nodestointpol, maxintpolelts, &
&   intpolno, intpolelts, statelsz, statnosz,  &
&   neighprof, kmweight, isnodeofelement, &
!&   neighprof, kmweight, isnodeofelement, &
&   name_cwr,&
&   rausv, &
&   mcord
!meaning of the variables
!------------------------
!correctionks     correction scaling factor for the ks value of an element; default must be 1.0d0
!correctionaxay   correction scaling factor for the ax and ay values of an element; default must be 1.0d0
!correctiondp     correction scaling factor for the dp values of an element; default must be 1.0d0
!lambdatot        total calculated lambda value of an element
!lambdaks         lambda due to bed roughness (ks)
!lambdap          lambda due to vegetation resistance (ax, ay, dp)
!lambdadunes      lambda due to dunes resistance (not operative yet)
!intpolprof       switch for sort of 1D node within polynomial approach
!                 .t. - node is an interpolation profile
!                 .f. - node is an original profile
!ispolynomnode    switch for type of 1D node
!                 .t. - 1D-node described via polynomials
!                 .f. - 1D-node described via geometry definition (trapezoidal approach)
!transitionelement  switch for information about 1D element's nature
!                 .t. - is the 1D part of a 1D/2D line-2-element transition
!                 .f. - is a normal 1D element
!nodestointpol    number of nodes to be added via interpolation (only 1D-nodes)
!maxintpolelts    maximum number of elements to be added within one original element via interpolation
!statelsz         element number to start counting the interpolated element ID numbers at; actually it's maxe+1
!statnosz         node number to start counting the interpolated node ID numbers at; actually it's maxp+1
!intpolno         number of interpolation nodes within an element
!intpolelts       stores the IDs of the interpolated elements in their sequence for each original element
!nconnect         number of connected nodes to another node
!neighb           stores the IDs of the nodes that are connected to a certain node
!neighprof        stores the two neighbouring original profiles' flow kilometers to an interpolated node
!kmweight         stores the weighted influence of two neighbouring original nodes to interpolated ones via the distance
!isnodeofelement  stores all element IDs, where a node is inside; restriction is that nodes are having maximum 12 elements, where they are part of
!name_cwr         name string of the cwr-data input file, if restarting
!mcord            centre coordinates of an element
                  !TODO: Probably not needed this way any longer
!rausv            is the water surface elevation at startup
!

use para1dpoly, only: &
&   kmx, hhmin, hhmax, hbordv, &
&   polysplitsa, polyrangea, apoly, &
&   polysplitsq, polyrangeq, qpoly, qgef, &
&   polysplitsb, polyrangeb, alphapoly, betapoly, &
&   beient
!meaning of the variables
!------------------------
!kmx              kilometer of a 1D-profile
!hhmin            minimum water depth of the valid range of polynomials
!hhmax            maximum water detph of the valid range of polynomials
!hbordv           bankful elevation to determine the minimum water depth to start calculating with flow coefficient
!polysplitsa      number of intersections of the A(h)-polynomial description at a profile
!polyrangea       all the lower border water depth values for the intersections of A(h)-polynomials at a profile
!apoly            all the coefficients for all A(h) polynomials of all intersections at a profile
!polysplitsq      number of intersections of the Q(h)-polynomial description at a profile
!polyrangeq       all the lower border water depth values for the intersections of Q(h)-polynomials at a profile
!qpoly            all the coefficients for all Q(h) polynomials of all intersections at a profile
!qgef             reference slope for the q-curve (Schluesselkurve)
!polysplitsb      number of intersections of the alpha/beta(h)-polynomial description at a profile
!polyrangeb       all the lower border water depth values for the intersections of alpha/beta(h)-polynomials at a profile
!alphapoly        all the coefficients for all alpha(h) polynomials of all intersections at a profile
!betapoly         all the coefficients for all beta(h) polynomials of all intersections at a profile
!beien            decision switch for the usage of the flow coefficient/ of the convective term at all
!

implicit none

!input variables
!----------------
integer (kind = 4), intent (in) :: kswit
!output variables
!-----------------
integer (kind = 4), intent (out) :: nodecnt, elcnt, arccnt, tlcnt, psConn, maxSE
integer (kind = 4), intent (out) :: polysplitcounta, polysplitcountq, polysplitcountb
!meaning of the variables
!------------------------
!kswit            switch for the usage of rdkalypso
!                 1 - read mesh dimensions
!                 0 - read mesh into proper arrays and form them
!                 2 - read restart informations
!nodecnt          maximum necessary ID number of the nodes
!elcnt            maximum necessary ID number of the elements
!arccnt           maximum necessary ID number of the arcs
!tlcnt            maximum necessary ID number of the 1D/2D line-2-element transition constructs
!psConn           maximum necessary ID number of pipe surface connections
!maxSE            maximum necessary ID number of storage elements
!polysplitcounta  maximum number of intersections for the A(h) polynomial
!polysplitcountq  maximum number of intersections for the Q(h) (Schluesselkurve) polynomial
!polysplitcountb  maximum number of intersections for the alpha/beta(h) polynomial

!local variables
!----------------
integer (kind = 4)                              :: unit_nr
integer (kind = 4)                              :: istat
integer (kind = 4)                              :: linestat
character (len = 2)                             :: id_local
character (len = 512)                           :: linie
integer                                         :: weircnt
integer (kind = 4), allocatable, dimension (:,:):: reweir (:, :)
real (kind = 8), dimension (1:2)                :: vekkant, vekpu1
real (kind = 8)                                 :: kreuz
integer (kind = 4)                              :: i, j, k, l
real (kind = 8)                                 :: x, y, z
real (kind = 8)                                 :: a, b, c
real (kind = 8)                                 :: hhmin_loc
integer (kind = 4)                              :: elzaehl, mittzaehl
integer (kind = 4)                              :: arcs_without_midside, midsidenode_max
integer (kind = 4), allocatable, dimension (:,:):: localArc
integer (kind = 4), dimension (5)               :: temparc
integer (kind = 4)                              :: ibot, itop
integer (kind = 4)                              :: ilft, irgt
integer (kind = 4)                              :: jnum
integer (kind = 4), dimension (5)               :: elkno
integer (kind = 4), dimension (4)               :: mikno
integer (kind = 4), allocatable, dimension (:)  :: nop_temp
integer (kind = 4)                              :: ncorn_temp
integer (kind = 4)                              :: node1, node2, nd
integer (kind = 4)                              :: connline
integer (kind = 4)                              :: connnumber
integer (kind = 4), allocatable, dimension (:,:):: elem
real (kind = 8)                                 :: sumx, sumy
logical                                         :: ReorderingNotDone
integer (kind = 4), dimension (2, 3535)         :: qlist
integer (kind = 4), allocatable, dimension (:)  :: elfix
integer (kind = 8)                              :: noDerivs
type (node), pointer                            :: tmpNode, newFENode, nodeOrigin
type (linkedNode), pointer                      :: newNeighb
integer (kind = 4)                              :: CCLID
type (simulationModel), pointer :: m_SimModel
!meaning of the variables
!------------------------
                        !TODO: geometry and restart file become one
!unit_nr                unit number to read from; it can be either the geometry or the restart file
!istat                  i/o status specifier
!linestat               i/o status specifier
!id_local               first digits of any read line to examine what sort of line type is present
!linie                  rest of any read line starting from id_local, to evaluate the data written in the line
!weircnt                counts all control structure occurances locally
!reweir                 for turning purposes of 2D control structure elements; the first node of the weir must be always upstream left; reweir stores the first node of an element and initiats a turning algorithm later on
!vekkant                reference arc of an element interpreted as a vector to check for twisted element via cross product
!vekpu1                 check-against arc of an element interpreted as a vector to check for twisted element via cross product
!kreuz                  cross product result for twisted element check
!i, j, k, l             different counter variables
!x, y, z                local variables, if calculating coordinates
!a, b, c                local variables, if calculating triangles; c - hypotenuse; a, b - cathetus
!hhmin_loc              read out variable for the minimum water depth of the valid range of a polynomial
!elzaehl                element counter that checks for the really used element numbers. During element set up it's just incrementally increased, if an element really has data
!mittzaehl              counter of midside nodes that are generated during mesh set up
!arcs_without_midside   counter for midside nodes without arcs
!midsidenode_max        counter for midside nodes
!localArc               !shows definition parts of an arc; local storage for creating mesh geometry
!                       1 - first  (bottom) node
!                       2 - second (top) node
!                       3 - left element
!                       4 - right element
!                       5 - midside node
!temparc                !shows definition parts of an arc during read procedure
!                       0 - not used
!                       1 - first  (bottom) node
!                       2 - second (top) node
!                       3 - left element
!                       4 - right element
!                       5 - midside node
!ibot                   First(bottom) definition node of an arc
!itop                   Last(top) definition node of an arc
!ilft                   left positioned element of an arc
!irgt                   right positioned element of an arc
!jnum                   shows the number of nodes assigned to an element; shows implicitly shape of element
!elkno                  stores all three or four corner nodes of an element; last entry (4th or 5th in 2D-geometry must fit to the 1st again to build a ring)
!mikno                  stores all three or four midside nodes of an element
!nop_temp               temporary node storage for only for 1D/2D line-2-element transitions (includes 1D nodes as well as line's nodes)
!ncorn_temp             temporary node number storage that are connected to a 1D/2D line-2-element transition
!node1                  used as local storage of reference node, when creating neighbourhood relations
!node2                  used as local storage of neighbouring node, when creating neighbourhood relations
!nd                     used as local storage of a node
!connline               local storage of a continuity line's ID when running through the 1D/2D line-2-element transitions
!connnumber             ID number of a 1D/2D line-2-element transition
!elem                   arc definitions of the elements
!                       0 - not used
!                       1 - number of arcs + 1
!                       2 - 1st arc ID
!                       3 - 2nd arc ID
!                       4 - 3rd arc ID
!                       5 - 4th arc ID
!                       6 - not used
!sumx                   auxiliary sum for calculating an element's center x-coordinate
!sumy                   auxiliary sum for calculating an element's center y-coordinate
!ReorderingNotDone      switch to start reordering
!                       .t. - reodering has to be done
!                       .f. - reordering has not to be done
!                       TODO: array is bad conditioned, because 2nd subscript is never used and size doesn't fit a continuity line
!qlist                  list of nodes to start reordering from; comes from startnode.sub; is bad conditioned
!                       1 - list that will be applied
!                       2 - not used
!elfix                  shows, whether an element has a reodering number or not; examines, whether reordering has to be done



! INITIALISIERUNGSBLOCK -----------------------------------------------------------------------

!TODO: Initializations are not good, because zero subscripts are not used and size of elem doesn't fit
allocate (elem (MaxE, 6), elfix (MaxE))

sumx = 0.0d0
sumy = 0.0d0
  

if (kswit == 1) then        !in the first case the value maxa has to be found, the allocation of
  maxa = 0                  !arc(i,j) is not necessary for the first run, so that it is allocated
  maxe = 0                  !efa nov06
endif                     !nis,mar06
allocate (localArc (maxa, 5))   !just pro forma, it is deallocated at the end of this run.

arcs_without_midside = 0
midsidenode_max = 0

!TODO: 
do i = 1, 3535
  qlist (1, i) = 0
  qlist (2, i) = 0
end do

!initialisations of locals for i/o purposes
!------------------------------------------
nodecnt = 0
arccnt  = 0
elcnt   = 0
TLcnt   = 0
psConn  = 0
maxSE   = 0
PolySplitCountA = 0
PolySplitCountQ = 0
PolySplitCountB = 0

!initialisations of local workers
!--------------------------------
istat = 0
weircnt = 0

!local allocations and initialisations
!-------------------------------------
allocate (reweir (maxE, 2))
do i = 1, maxe
  do j = 1, 2
    reweir (maxe, j) = 0
  enddo
enddo

!LF nov06: initialize the number of weir elements

!temparc is an array for reading arc informations in the dimensioning run (KSWIT==1)
DO i=1,5
  temparc (i) = 0
ENDDO

!NiS,mar06: changed allocation of arc; the former allocation with the number of nodes was too big, so the MaxA (for maximum number of arcs) is used:
!           changed mnd to MaxA; additionally the array is firstly initialized, when the geometry is read (KSWIT==0)
IF (KSWIT == 0) THEN
  outer1: DO i = 1, MaxA
    inner1: DO j = 1, 5
      localArc (i, j) = 0
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

! READING SEQUENCE --------------------------------------------------------------

!set proper unit number
!----------------------
!geometry size or geometry
unit_nr = ifile
if (kswit == 2) unit_nr = nb

!endless reading loop until end of file condition is reached
!-----------------------------------------------------------
reading: do

  !read next line from model/restart file
  read (unit_nr, '(a)', iostat = istat) linie

  !exit loop, if reaching end of file condition
  if (istat == -1) exit reading

  !ERROR - reading error
  if (istat /= 0) call errormessageandstop (1001, unit_nr, 0.0d0, 0.0d0)

  !title informations
  !------------------
  if (linie (1:2) == '00') then
    read (linie, '(a2,a80)') id_local, title 
  endif

  !reading the mesh dimension or the mesh itself
  !---------------------------------------------
  kswittest: if (kswit == 0 .or. kswit == 1) then

    !NODE DEFINITIONS ---
    IF (linie (1:2) =='FP') THEN
      !mesh dimension
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
        nodecnt = max (i, nodecnt)
      !mesh geometry read
      ELSE
        istat=0
        !TODO: Format differentiation
        READ (linie, *,IOSTAT=istat) id_local, i, cord (i, 1) , cord (i, 2), ao (i),kmx(i)
        !check for kilometer
        if (istat == 0) WRITE (lout, *) 'Die Kilometrierung von Knoten', i, 'wurde eingelesen:', kmx (i)
        nodecnt = max (i, nodecnt)
        !add new node to model
        newFENode => newNode (i, cord (i,1), cord (i,2), ao(i))
        call addNodeToMesh (m_SimModel.FEmesh, newFENode)

        IF (i <= 0) call ErrorMessageAndStop (1002, i, 0.0d0, 0.0d0)
      ENDIF

    !ARC DEFINITIONS ---
    ELSEIF (linie (1:2) =='AR') then
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
        READ (linie,'(a2,6i10)') id_local, i, (localArc(i,j), j=1, 5)
        !Look for errors in enumeration
        IF (i > arccnt) arccnt = i
        !ERROR - negative arc number
        IF (i <= 0) call ErrorMessageAndStop (1301, i, 0.0d0, 0.0d0)
      ENDIF

    !INTERPOLATION PROFILE informations
    ELSEIF (linie(1:2) == 'IP') THEN
      IF (kswit == 1) THEN
        READ (linie, '(a2, i10, i10)') id_local, i, j
        NodesToIntPol = NodesToIntPol + j
        if (j > MaxIntPolElts) MaxIntPolElts = j
      ELSEIF (kswit == 0) then
        READ (linie, '(a2, i10, i10)') id_local, i, IntPolNo(i)
      ENDIF

    !POLYNOMIAL RANGE CROSS SECTIONAL AREA
    !id_local  = 'PR'
    !       i  = node-ID
    !       j  = number of polynom splitting parts
    !polyrange = maximum values
    elseif (linie(1:3) == 'PRA') then
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

    !POLYNOMIAL RANGE Q (SCHLUESSELKURVE)
    ELSEIF (linie(1:3) == 'PRQ') then
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

    !POLYNOMIAL RANGE BOUSSINESQ COEFFICIENT
    ELSEIF (linie(1:3) == 'PRB') then
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

    !nis,nov07: new range line ID (polynom range PR)
    !id_local  = 'AP '
    !       i  = node-ID
    !       j  = number of polynom splitting parts
    ELSEIF (linie(1:3) == 'AP ') then
      IF (KSWIT /= 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j, (apoly(j, i, k), k = 0, 12)
      endif
    ELSEIF (linie(1:3) == 'QP ') then
      IF (KSWIT /= 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j, qgef(i), (qpoly(j, i, k), k = 0, 12)
        !remember, that the node is a 1D polynomial approach node
        if (.not. IsPolynomNode (i)) IsPolynomNode (i) = .true.
      endif
    ELSEIF (linie(1:3) == 'ALP') then
      if (beient /= 0 .and. beient /= 3) then
        IF (KSWIT /= 1) then
          linestat = 0
          read (linie, *, iostat = linestat) id_local, i, j, (alphapoly(j, i, k), k = 0, 12)
        endif
      endif
    ELSEIF (linie(1:3) == 'BEP') then
      if (beient /= 0 .and. beient /= 3) then
        IF (KSWIT /= 1) then
          linestat = 0
          read (linie, *, iostat = linestat) id_local, i, j, (betapoly(j, i, 0), k = 0, 12)
        endif
      endif

    !FINITE ELEMENT DEFINITIONS ---
    ELSEIF (linie (1:2) =='FE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local, i
        IF (i > elcnt) elcnt = i
      !Read geometry into arrays
      ELSE
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,4i10)') id_local, i, imat (i), imato (i), nfixh (i)

        !LF,nov06: Read again the FE line for the starting node of a weir element
        if (imat(i) > 903 .and. imat(i) < 990) then
          weircnt = weircnt + 1
          read (linie, '(a2,5i10)') id_local, i, imat(i), imato(i), nfixh(i), reweir(weircnt,1)
          !ERROR - no starting node for weir element definition was found
          if (reweir (weircnt, 1) <= 0) call ErrorMessageAndStop (1003, i, 0.0d0, 0.0d0)
          reweir (weircnt, 2) = i
        end if
        IF (i > elcnt) elcnt = i
        IF (i <= 0) call ErrorMessageAndStop (1004, i, 0.0d0, 0.0d0)
      ENDIF

    !1D JUNCTION ELEMENT DEFINITIONS ---
    ELSEIF (linie (1:2) =='JE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local,i
        elcnt = max (i, elcnt)
      ELSE
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,9i10)') id_local, i, (nop(i, j), j=1, 8)
        elcnt = max (i, elcnt)
        IF (i <= 0) call ErrorMessageAndStop (1005, i, 0.0d0, 0.0d0)
      ENDIF

    !ROUGHNESS CORRECTION LAYER ---
    ELSEIF (linie (1:2) == 'RC') then
      if (KSWIT == 1) CYCLE reading
      read (linie, '(a2, i10, 3(f10.6))') id_local, i, correctionKS(i), correctionAxAy(i), correctionDp(i)

    !NiS,may06: cross section reading for 1D-ELEMENTS
    !CROSS SECTIONAL INFORMATIONS FOR 1D-NODES ---
    ELSEIF (linie (1:2) =='CS') THEN
      !Only interesting for geometry reading run
      IF (KSWIT==1) CYCLE reading
      !read informations into proper arrays
      READ (linie, '(a2,i10,6F10.0)') id_local, i, width(i), ss1(i), ss2(i), wids(i), widbs(i), wss(i)
    !-

    !NiS,may06: Transition elements between 1D- and 2D- network parts
    !1D-2D-TRANSITION ELEMENTS ---
    ELSEIF (linie (1:2) =='TE') THEN
      IF (KSWIT==1) THEN
        READ (linie, '(a2,i10)') id_local, i
        elcnt = max (i, elcnt)
      ELSE
        READ (linie, *) id_local, i, (nop(i,k),k=1,5)
        !Increase number of 1D-2D-TRANSITION ELEMENTS
        MaxT = MaxT + 1
      END IF
    !-

    !TRANSITION LINE ---
    !NiS,nov06: Transition line elements between 1D- and 2D-networks with an element to line connection
    !           TransLines(i,1): transitioning element
    !           TransLines(i,2): transitioning line
    !           TransLines(i,3): node, which is connected on the 1D side of the transition element
    !           TransLines(i,4):
    ELSEIF (linie(1:2)=='TL') then
      IF (KSWIT==1) THEN
        READ (linie,'(a2,i10)') id_local, i
        IF (i>TLcnt) TLcnt = i
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
    
    !PIPE SURFACE CONNECTION (PIPE)
    ELSEIF (linie(1:2) == 'PS') then
      if (kswit == 1) then
        read (linie, '(a2,i10)') id_local, i
        if (i > psConn) psConn = i
      else
        write(*,*) psconn
        read (linie, '(a2,3i10)') id_local, i, PipeSurfConn(i).SurfElt, PipeSurfConn(i).pipeElt
        ConnectedElt (PipeSurfConn(i).SurfElt) = PipeSurfConn(i).pipeElt
        ConnectedElt (PipeSurfConn(i).pipeElt) = PipeSurfConn(i)%SurfElt
      endif
      
    !PIPE SURFACE CONNECTION GEOMETRY
    elseif (linie(1:4) == 'PP_G') then
      if (kswit /= 1) then
        read (linie, *) id_local, i, PipeSurfConn(i).manholeDef.ks, PipeSurfConn(i).manholeDef.diameter
      endif
    !PIPE SURFACE CONNECTION LOSS COEFFICIENTS
    elseif (linie(1:4) == 'PP_L') then
      if (kswit /= 1) then
        read (linie, *) id_local, i, PipeSurfConn(i).manholeDef.zetaInflowUpper, PipeSurfConn(i).manholeDef.zetaOutflowUpper,&
        & PipeSurfConn(i).manholeDef.zetaInflowLower, PipeSurfConn(i).manholeDef.zetaOutflowLower
      endif
    elseif (linie(1:4) == 'PP_M') then
      if (kswit /= 1) then
        read (linie, *) id_local, i, PipeSurfConn(i).manholeDef.mue
      endif
    
    !STORAGE ELEMENT
    elseif (linie(1:2) == 'SE') then
      if (kswit == 1) then
        read (linie, *) id_local, i
        if (i > maxSE) maxSE= i
      else
        read (linie, *) id_local, i, CCLID, StorageElts(i).storageContent
        StorageElts(i).ID = i
        ccls(CCLID).storageElt => StorageElts(i)
      endif
    endif
    
    

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
    IF (linie (1:2) =='VA') then
      READ(linie,'(a2,i10,2f20.14,2f20.13)') id_local, i, (vel(j,i), j=1, 3), rausv (3, i)
      !ERROR - restart values can't be applied to node out of zero-maxp-Range
      !nis,aug08: If node number is zero, it has no coordinates; use dummy coordinates 0.0
      IF (i > MaxP .or. i <= 0) call ErrorMessageAndStop (1601, i, 0.0d0, 0.0d0)
    ENDIF

    !INITIAL VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP; ONLY FOR INTERPOLATED PROFILES/ NODES ---
    IF (linie (1:2) =='IR') then
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
    IF (linie(1:2) == 'FR') THEN
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
      IF (i>MaxP) stop 'i>MaxP'
      !Stop program execution on negative NODE number
      IF (i<=0) stop 'Knotennummer<=0'
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
      IF (i>MaxP) stop 'i>MaxP'
      !Stop program execution on negative NODE number
      IF (i<=0) stop 'Knotennummer<=0'
    ENDIF

!    !ADDITIONAL INFORMATIONS FOR EVERY NODE ---
    IF (linie (1:2) =='ZU') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvzu = 1
      !NiS,apr06: only hel(i) and hdet(i) have to be read; changing read command
      !READ (linie, '(a2,i10,i6,4f15.7)') id, i, ndry(i), hel(i), hol(i), hdet(i), hdot(i)
      !READ(linie,'(a2,i10,6x,2(f15.7,15x))')id_local,i,hel(i),hdet(i)
      READ (linie, '(a2,i10,i6)') id, i, ndry(i)

      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      IF (i>MaxP) stop 'i>MaxP'
      !Stop program execution on negative NODE number
      IF (i<=0) stop 'Knotennummer<=0'
    ENDIF

! TEST BLOCK FOR ERRORS ------------------------------------------------------------------------

  ELSE !other values of KSWIT will generate an error, this can't happen normally

    call ErrorMessageAndStop (1006, kswit, 0.0d0, 0.0d0)

  ENDIF KSWITTEST 

END DO reading
WRITE(*,*) ' Schaffe die Leseschleife'


!ENDBLOCK FOR THE CASE OF DIMENSION READING (KSWIT==1) ----------------
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
  DEALLOCATE (localArc)                                                 !the pro forma allocation of arc(i,j) is stopped
  RETURN                                                    	        !If the dimension reading option is chosen (that means KSWIT=1), this
                                                                        !subroutine can be returned at this point.

!ENDBLOCK FOR THE CASE OF RESTART INFORMATION READING (KSWIT==2) ------
!NiS,apr06: leave subroutine, if restart inforamtions are read
ELSEIF (KSWIT == 2) THEN
  !NiS,may06: REWINDING might be needful; at the moment not necessary
  REWIND (NB)
  WRITE(*,*)' Leaving the KALYPSO-2D restart file.'
  RETURN
ENDIF


!     TRANSLATION TO KALYPSO-2D FORMAT -----------------------------------------------------


!NiS,may06: every arc's element(s) will be saved in the elem-array, which shows after Loop-Execution the grouping of nodes belonging to an element.
!           The stored values are: On the first place the number of nodes associated with the element and on the 2nd to 5th place the nodes (unsorted)
!
!           Changed the if-clauses in the way that, 1D, 1D-2D-transitions and 2D-elements can be recognized
DO i = 1, arccnt

  !DEAD ARCS
  if (localArc (i, 3) == 0 .and. localArc (i, 4) == 0) then
    write (lout, 9003) i
    write (*   , 9003) i
    
  !1D-ELEMENT or 1D-2D-TRANSITION-ELEMENT
  elseif ((localArc (i, 3) == localArc (i, 4)) .and. localArc (i, 3) /= 0) then
    !TODO: these checks are not 100 percent consistent
    j = localArc (i, 3)
    
    !ERROR, if 1D-node is already in use
    if (elem (j, 1) /= 0) then
      call errormessageandstop (1302, j, 0.5 * (cord (localArc (i,1), 1) + cord (localArc (i, 2), 1)), &
                                &          0.5 * (cord (localArc (i,1), 2) + cord (localArc (i, 2), 2)))

    !assign identification for 1D-nodes
    !  elem (j, 1) == -1) - normal 1D-elements
    !  elem (j, 1) == -2) - 1D/2D transition elements
    !remember the arc, that defines the 1D-element for later node extraction
    else
      !no 4th node at normal 1d-elements
      if (nop (j, 4) == 0) then
        elem (j, 1) = -1
      !4th node at 1d-2d-transition elements; nodes for 1d-2d-transition elements were already assigned in the reading section
      else
        elem (j, 1) = -2
      endif
      !remember the arc of the 1d-element
      elem (j, 2) = i
    endif

  !2D-ELEMENTS
  else
    !left element k = 3
    !right element k = 4
    DO k = 3, 4
      !get element number
      j = localArc (i, k)
      !Testing for the existance of the element
      IF (j > 0) then
        !ERROR - element is used twice
        IF (elem (j, 1) == -1) call ErrorMessageAndStop (1302, j,                  &
                             & 0.5 * (cord (localArc (i,1), 1) + cord (localArc (i, 2), 1)), &
                             & 0.5 * (cord (localArc (i,1), 2) + cord (localArc (i, 2), 2)))
        !Testing, whether it is the first defining arc
        IF (elem (j, 1) == 0) elem (j, 1) = 1
        !Increase number of assigned ARCS to ELEMENT by increment =1
        elem (j, 1) = elem (j, 1) + 1
        !ERROR - Element is defined with more than 4 arcs (only 3 or 4 is possible)
        IF (elem (j, 1) > 5) call ErrorMessageAndStop (1202, j,                      &
                               & 0.5 * (cord (localArc (i,1), 1) + cord (localArc (i, 2), 1)), &
                               & 0.5 * (cord (localArc (i,1), 2) + cord (localArc (i, 2), 2)))
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

!loop over all elements
all_elem: DO i = 1, maxe

  !WP Initialisieren des Elementes
  kno: DO j = 1, 4
    elkno (j) = 0
    mikno (j) = 0
  END DO kno
  elkno (5) = 0

  !cycle empty elements
  IF (elem (i, 1) == 0 .and. (imat(i) < 901 .or. imat (i) > 903)) CYCLE all_elem

  !count the number of NOT-empty entries of elcnt
  elzaehl = elzaehl + 1

  !normal 1D-elements --------------------
  dimensionif: if (imat(i) >= 901 .and. imat (i) <= 903) then
    findJunctions: do j = 8, 1, -1
      if (nop(i, j) /= 0) then
        ncorn (i) = j
        exit findJunctions
      end if
    end do findJunctions
  
  ELSEIF (elem (i, 1) == -1) THEN
    !for normal 1D-elements, the number of nodes is 3 and the number of corner nodes is 2
    jnum = 2
    ncorn(i) = 3

    !Passing corner nodes to node array
    nop (i, 1) = localArc (elem (i, 2), 1)
    nop (i, 3) = localArc (elem (i, 2), 2)

    !giving over midsidenode, if present, to temporary node array
    IF (localArc(elem(i,2),5) > 0) THEN
      nop(i,2) = localArc(elem(i,2),5)
    ENDIF

  !1D-2D-transition elements -------------
  ELSEIF (elem(i,1) == -2) THEN
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

    !NiS,may06: With 1D-ELEMENTS error only occurs, if (jnum==1 or ==2), because if (jnum<0), it's an 1D-ELEMENT and (jnum==0) was cycled.
    !IF (jnum<3) THEN

    !ERROR - element has less than 3 arcs, which is not possible
    IF (jnum == 1 .or. jnum == 2) call ErrorMessageAndStop (1203, i, cord(localArc (elem (i, 2), 5), 1) , cord(localArc (elem (i, 2), 5), 2))

    ! erste Kante:                                                  !starting with the first arc, the element's nodes in anticlockwise direction
    l = 1                                                           !will be saved in a temporary array to write them later into the array nop.

    ! akt. Element links der Kante .und. unten-Knoten beginnt:      !the two arrays for temporary saving are:
    IF (localArc (elem (i, 2), 3) ==i) THEN                            !       elkno(1...5)    =       corner nodes of element
      elkno (1) = localArc (elem (i, 2), 1)                              !       mikno(1...4)    =       midside nodes of element, if defined
      elkno (2) = localArc (elem (i, 2), 2)
      IF (localArc (elem (i, 2), 5) > 0) THEN                         !In dependency of the side the actual element is positioned in relation to
        mikno (1) = localArc (elem (i, 2), 5)                            !the first arc, the nodes are saved in elkno(1) and elkno(2)
      ENDIF
    ! akt. Element rechts der Kante .und. oben-Knoten beginnt:
    ELSE
      elkno (1) = localArc (elem (i, 2), 2)
      elkno (2) = localArc (elem (i, 2), 1)
      IF (localArc (elem (i, 2), 5) > 0) THEN
        mikno (1) = localArc (elem (i, 2), 5)
      ENDIF
    ENDIF

!TODO: Introduce modern do-loop
    ! weitere Kanten:                                       	    !The other two or three arcs defining the actual element i are analysed from
2222   l = l + 1                                               	    !this point on. The jumpmark 2222 is somthing like a do loop.
    IF (l>jnum) THEN                                       	    !The first if-case checks whether the actual arc l is the last one to define
      IF (elkno (1) /= elkno (l)) call ErrorMessageAndStop (1204, i, 0.0d0, 0.0d0)

      GOTO 2444
    END IF
    elem_arc: DO j = 2, jnum                                        !For every left arc with exception of the first, dealt with above, it is checked,
      ! Element links der Kante .und. unten-Knoten knuepft an?      !whether it is the one that is connected to the last node of the last arc.
      left: IF ((localArc (elem (i,j+1),3) == i) .AND. (localArc (elem (i,j+1),1) == elkno(l))) then
        elkno (l + 1) = localArc (elem (i, j + 1), 2)                    !In dependency of the side the actual element is positioned in relation
        IF (localArc (elem (i, j + 1), 5) >0) then                    !to the arc j, the node that could be connected with the last one of the last
          mikno (l) = localArc (elem (i, j + 1), 5)                      !arc is checked, whether it is connected. If so the procedure jumps to the
        END if                                                      !next arc and increases the number of l
        GOTO 2222
      END IF left
      ! Element rechts der Kante .und. oben-Knoten knuepft an?
      right: IF ((localArc (elem (i,j+1),4) == i) .AND. (localArc (elem (i,j+1),2) == elkno(l))) then
        elkno (l + 1) = localArc (elem (i, j + 1), 1)
        IF (localArc (elem (i, j + 1), 5) >0) then
          mikno (l) = localArc (elem (i, j + 1), 5)
        END if
        GOTO 2222
      END IF right
    END DO elem_arc


    !ERROR - Element is not forming a linear ring
    call errorMessageAndStop (1204, i, 0.0d0, 0.0d0)

  ! Element O.K.
2444 CONTINUE

    !set up nop-field
    !----------------
    !enter corner nodes
    DO j = 1, jnum
      nop (i, j * 2 - 1) = elkno (j)
    END DO
    !enter midside nodes
    DO j = 1, jnum
      IF (mikno (j) >0) then
        !ERROR - midside node ID is higher than maximum node ID; that doesn't work
        IF (mikno (j) > maxp) call errorMessageAndStop (1109, mikno (j), 0.0d0, 0.0d0)
        nop (i, j * 2) = mikno (j)
      ENDIF
    END DO
    
    !copy nop to nops
    !----------------
    do j = 1, 8
      nops (i, j) = nop (i, j)
    enddo

    !check mesh for twisted elements
    !-------------------------------
    crossing_outer: do j = 1, jnum - 2
      !get reference vector to check twisting
      vekkant (1) = cord (nop (i, (j + 1) * 2 - 1), 1) - cord (nop (i, j * 2 - 1), 1)
      vekkant (2) = cord (nop (i, (j + 1) * 2 - 1), 2) - cord (nop (i, j * 2 - 1), 2)
      !run through connected arcs
      crossing_inner: do k = j + 2, jnum
        !get vector to check for
        vekpu1 (1) = cord (nop (i, k * 2 - 1), 1) - cord (nop (i, j * 2 - 1), 1)
        vekpu1 (2) = cord (nop (i, k * 2 - 1), 2) - cord (nop (i, j * 2 - 1), 2)
        !generate cross product
        kreuz = vekkant (1) * vekpu1 (2) - vekkant (2) * vekpu1 (1)
        if (kreuz <= 0.0) call errormessageandstop (1205, i, 0.5 * (cord (nop (i, 1), 1) + cord (nop (i, 3), 1)), &
                                                           & 0.5 * (cord (nop (i, 1), 2) + cord (nop (i, 3), 2)))
      end do crossing_inner
    end do crossing_outer
  endif dimensionif
end do all_elem

!LF nov06 renumbering the nodes around weir elements
if (weircnt > 0) then
  do i = 1, weircnt
    if (nop (reweir (i, 2), 1) /= reweir (i, 1)) call reweir2dKALYPSO (i, reweir(i,:))
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
                                                                        

all_arcs: DO i=1,arccnt

  !dead arcs have to be skipped
  if (localArc(i,1)==0) CYCLE all_arcs

  ! Mittseitenknoten vorhanden?
  !NiS,expand test for defined midside nodes in ARC-array but without coordinate-definitions; this was a logical gap
  IF ((localArc(i,5)>0) .and. (localArc(i,5)<=nodecnt)) THEN
    IF ((cord (localArc (i, 5), 1) /= 0.0) .and. (cord (localArc (i, 5), 2) /= 0.0)) THEN
      if (ao (localArc (i, 5)) + 9999.0 < 1.0e-3) then
        WRITE(lout,*) 'recalculating elevation        '
        ao (localArc (i, 5)) = 0.5 * (ao (localArc (i, 1)) + ao (localArc (i, 2)))
        if (kmx (localArc(i,1)) /= 0.0 .and. kmx (localArc(i, 2)) /= 0.0) then
          kmx (localArc (i, 5)) = 0.5 * (kmx (localArc (i, 1)) + kmx (localArc (i, 2)))
        end if
      end if
      CYCLE all_arcs
    ELSE
      !Test for distances
      a = SQRT ( ( cord (localArc (i,1), 1) - cord ( localArc (i,5), 1) )**2 + ( cord (localArc (i,1), 2) - cord (localArc (i,5), 2) )**2)
      b = SQRT ( ( cord (localArc (i,2), 1) - cord ( localArc (i,5), 1) )**2 + ( cord (localArc (i,2), 2) - cord (localArc (i,5), 2) )**2)
      c = SQRT ( ( cord (localArc (i,1), 1) - cord ( localArc (i,2), 1) )**2 + ( cord (localArc (i,1), 2) - cord (localArc (i,2), 2) )**2)
      IF (a<c .or. b<c) THEN
        WRITE (*,1234) localArc(i,5), i, localArc(i,5)
1234    FORMAT (' The NODE ', I5,' is defined in ARC ', i5,'. The Coordinates are the origin (0.0/0.0), but this seems '/ &
              & ' not be define but the default initialized value, because the distance between the corner nodes of the '/&
              & ' arc is shorter than one of the distances between the midside node and the corner nodes! Therefore the '/&
              & ' coordinates of the node ', I56, ' are recalculated.')
        !Recalculation with Linear interpolation of coordinates for nodes, that were not logical before
        cord (localArc(i,5),1) = 0.5 * (cord (localArc(i,1),1) + cord (localArc(i,2),1) )
        cord (localArc(i,5),2) = 0.5 * (cord (localArc(i,1),2) + cord (localArc(i,2),2) )
        ao (localArc(i,5)  ) = 0.5 * (  ao (localArc(i,1)  ) +   ao (localArc(i,2)  ) )
        if (kmx (localArc(i,1)) /= 0.0 .and. kmx (localArc(i, 2)) /= 0.0) then
          kmx (localArc (i, 5)) = 0.5 * (kmx (localArc (i, 1)) + kmx (localArc (i, 2)))
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
  ibot = localArc (i, 1)
  itop = localArc (i, 2)
  ilft = localArc (i, 3)
  irgt = localArc (i, 4)
  !NiS,may06: Test for dead arcs, so the DO-LOOP may be cycled:
  IF (ilft==irgt .and. ilft==0) CYCLE all_arcs

  !coordinates of generated midside node
  x = (cord (ibot, 1) + cord (itop, 1) ) / 2.0
  y = (cord (ibot, 2) + cord (itop, 2) ) / 2.0
  z = (ao (ibot) + ao (itop) ) / 2.0
                                                                        
  !Install the temporary values in the proper global array lines; these lines could be economized with calculating directly without locals
  cord (nodecnt, 1) = x
  cord (nodecnt, 2) = y
  ao   (nodecnt)    = z
  
  !add new node to model
  newFENode => newNode (nodecnt, x, y, z)
  call addNodeToMesh (m_SimModel.FEmesh, newFENode)

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
106 FORMAT (1X, 'Number of added     '/ 1X, 'midside nodes:      ', I6/)



!checking/interpolation of cross section informations on 1D elements under geometry approach (trapezoidal)
do i = 1, arccnt
  if (localArc(i,3) == localArc(i,4) .and. localArc(i,3) /= 0) then
    if (imat(localArc(i,3)) < 900) then

      if (imat (localArc (i,3)) /= 89) then
        checkwidths: do j = 1, 3, 2
          nd = nop (localArc (i, 3), j)
          !error -  if one of the two corner nodes does not have cross sectional informations
          if (width (nd) == 0.0) call errormessageandstop (1103, nd, cord (nd, 1), cord (nd, 2))
        end do checkwidths
  
        if (width (nop (localArc (i, 3), 2)) == 0.0) then
          width  (nop (localArc (i, 3), 2)) = 0.5 * (width (nop (localArc (i, 3), 1)) + width (nop (localArc (i, 3), 3)))
          ss1    (nop (localArc (i, 3), 2)) = 0.5 * (ss1   (nop (localArc (i, 3), 1)) + ss1   (nop (localArc (i, 3), 3)))
          ss2    (nop (localArc (i, 3), 2)) = 0.5 * (ss2   (nop (localArc (i, 3), 1)) + ss2   (nop (localArc (i, 3), 3)))
          wids   (nop (localArc (i, 3), 2)) = 0.5 * (wids  (nop (localArc (i, 3), 1)) + wids  (nop (localArc (i, 3), 3)))
          widbs  (nop (localArc (i, 3), 2)) = 0.5 * (widbs (nop (localArc (i, 3), 1)) + widbs (nop (localArc (i, 3), 3)))
          wss    (nop (localArc (i, 3), 2)) = 0.5 * (wss   (nop (localArc (i, 3), 1)) + wss   (nop (localArc (i, 3), 3)))
        endif
      endif
    endif
  endif
enddo


!TODO: This should call either with alphapoly or betapoly depending on what is used!
call InterpolateProfs (m_SimModel.femesh, statElSz, statNoSz, MaxP, MaxE, maxIntPolElts, IntPolNo, NeighProf, ncorn, nop, &
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
!dealing with the FEM-switch are currently correctly running with the default value for GALERKIN method (default FEM==0).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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
    if (nop (TransLines (i, 1), 3) /= TransLines (i, 3)) then
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

!examine, whether reordering has to be done
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
GetMiddleCoord: do i = 1, maxe
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

        !linear search; really to slow
        nodeOrigin => findNodeInMeshByID (m_SimModel.FEmesh, node1)

        innerLT: do l = 1, ncorn_temp
          node2 = nop_temp (l)
          if (node1 /= node2) then
            tmpNode => findNodeInMeshByID (m_SimModel.FEmesh, node2)
            newNeighb => makeNodeALinkedNode (tmpNode)
            if (.not. (isContainedInList (nodeOrigin, tmpNode))) call addNeighbour (nodeOrigin, newNeighb)
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
      !linear search; really to slow
      nodeOrigin => findNodeInMeshByID (m_SimModel.FEmesh, node1)

      inner: do l = 1, ncorn (i)
        node2 = nop (i, l)
        IF (node1 /= node2) THEN
          tmpNode => findNodeInMeshByID (m_SimModel.FEmesh, node2)
          newNeighb => makeNodeALinkedNode (tmpNode)
          if (.not. (isContainedInList (nodeOrigin, tmpNode))) call addNeighbour (nodeOrigin, newNeighb)
        END if
      end do inner
    end do outer
  end if
end do neighbours

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
DEALLOCATE (localArc, reweir)
!Rewind for possible RESTART
REWIND(IFILE)

RETURN

END SUBROUTINE RDKALYPS
                                                                        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine InterpolateProfs (FE_Mesh, statElSz, statNoSz, MaxP, MaxE, MaxIntPolElts, IntPolNo, NeighProf, ncorn, nop, &
                          & cord, ao, kmx, kmWeight, IntPolProf, IntPolElts, qgef, imat, TransitionElement, MaxLT, &
                          & TransLines, IsPolynomNode)
                          
use mod_Nodes
use mod_meshModelFE

implicit none
!arguments
type (femesh), pointer :: FE_Mesh
INTEGER (kind = 4), intent (IN)    :: statElSz, statNoSz, MaxP, MaxE, MaxLT, MaxIntPolElts
integer, intent (in)    :: IntPolNo (1: MaxE)
INTEGER, intent (INOUT) :: IntPolElts (1: MaxE, 1: MaxIntPolElts)
integer, intent (inout) :: NeighProf (1: MaxP, 1: 2)
integer (kind = 4), intent (inout) :: ncorn (MaxE)
integer (kind = 4), intent (inout) :: nop (1: MaxE, 1: 8), imat (1: MaxE)
real, intent (inout)    :: cord (1: MaxP, 1: 2)
REAL, intent (inout)    :: ao (1: MaxP), kmx (1: MaxP), kmWeight (1: MaxP)
REAL, INTENT (INOUT)    :: qgef (1: MaxP)
LOGICAL, INTENT (INOUT) :: IntPolProf (1: MaxP), IsPolynomNode (1: MaxP), TransitionElement (1: MaxE)
INTEGER (kind = 4), INTENT (INOUT) :: TransLines (1: MaxLT, 1: 3)
!
!local variables
type (Node), pointer :: NewFENode
INTEGER :: i, j
integer :: NewNodeID, NewElt, TransLine
real (kind = 8) :: DX, DY, DH, DIST, origx, origy


NewFENode => null()
DIST = 0.0D0
NewNodeID = statNoSz
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
        NewNodeID = NewNodeID + 1
        nop (i, 3) = NewNodeID

        call GenNewNode (NewNodeID, i, origx, origy, nop(i, 1), nop (i, 4), NeighProf (NewNodeID, :), IntPolProf (NewNodeID), &
                      &  qgef(nop(i, 1)), qgef (nop (i, 4)), qgef (NewNodeID), IsPolynomNode (NewNodeID))
        cord (nop (i, 3), 1) = cord (nop (i, 1), 1) + DX * j
        cord (nop (i, 3), 2) = cord (nop (i, 1), 2) + DY * j
        ao (NewNodeID) = ao (nop (i, 1)) + DH
        kmWeight (NewNodeID) = j / REAL(IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNodeID) = kmx (nop (i, 1) ) + j * DIST
        else
          kmx (NewNodeID) = -1.0D0
        end if
        newFENode => NewNode (NewNodeID, cord (NewNodeID,1), cord (NewNodeID,2), ao(NewNodeID))
        call addNodeToMesh (FE_Mesh, newFENode)


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

        newFENode => NewNode (nop(i,2), cord (nop(i,2),1), cord (nop(i,2),2), ao(nop(i,2)))
        call addNodeToMesh (FE_Mesh, newFENode)

      !generate the middle elements of interpolation, it's first interesting, if there are more than 2 intersections
      elseif (j > 1) then
        NewElt = NewElt + 1
        IntPolElts (i, j - 1) = NewElt
        imat (NewElt) = imat (i)
        ncorn(NewElt) = 3
        if (j == 2) then
          nop (NewElt, 1) = nop (i, 3)
        else
          nop (NewElt, 1) = nop (NewElt - 1, 3)
        end if
        NewNodeID = NewNodeID + 1
        nop (NewElt, 3) = NewNodeID
        call GenNewNode (NewNodeID, i, origx, origy, nop (i, 1), nop (i, 4), NeighProf (NewNodeID, :), IntPolProf (NewNodeID), &
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (NewNodeID), IsPolynomNode (NewNodeID))
        cord (NewNodeID, 1) = cord (nop (i, 1), 1) + DX * j
        cord (NewNodeID, 2) = cord (nop (i, 1), 2) + DY * j
        ao (NewNodeID) = ao (nop (i, 1)) + DH * j
        kmWeight (NewNodeID) = j / REAL(IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNodeID) = kmx (nop (i, 1) ) + j * DIST
        else
          kmx (NewNodeID) = -1.0D0
        end if
        newFENode => NewNode (NewNodeID, cord (NewNodeID,1), cord (NewNodeID,2), ao(NewNodeID))
        call addNodeToMesh (FE_Mesh, newFENode)

        !generate midside node
        NewNodeID = NewNodeID + 1
        nop (NewElt, 2) = NewNodeID
        call GenNewNode (NewNodeID, i, origx, origy, nop (i, 1), nop (i, 4), NeighProf (NewNodeID, :), IntPolProf (NewNodeID), &
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (NewNodeID), IsPolynomNode (NewNodeID))
        cord (nop (NewElt, 2), 1) = 0.5 * (cord (nop (NewElt, 1), 1) + cord (nop (NewElt, 3), 1))
        cord (nop (NewElt, 2), 2) = 0.5 * (cord (nop (NewElt, 1), 2) + cord (nop (NewElt, 3), 2))
        ao (NewNodeID) = 0.5 * (ao (nop (NewElt, 1)) + ao (nop (NewElt, 3)))
        kmWeight (NewNodeID) = (j - 0.5) / REAL (IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNodeID) = kmx (nop (i, 1)) + (j - 0.5) * DIST
        end if
        newFENode => NewNode (NewNodeID, cord (NewNodeID,1), cord (NewNodeID,2), ao(NewNodeID))
        call addNodeToMesh (FE_Mesh, newFENode)
      end if

      !Generate the last element
      if (j == IntPolNo (i) ) then
        NewElt = NewElt + 1
        ncorn(NewElt) = 3

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
        NewNodeID = NewNodeID + 1
        nop (NewElt, 2) = NewNodeID
        call GenNewNode (NewNodeID, i, origx, origy, nop (i, 1), nop (NewElt, 3), NeighProf (NewNodeID, :), IntPolProf (NewNodeID), &
                      &  qgef (nop (i, 1)), qgef (nop (NewElt, 3)), qgef (NewNodeID), IsPolynomNode (NewNodeID))
        cord (NewNodeID, 1) = 0.5 * (cord (nop (NewElt, 1), 1) + cord (nop (NewElt, 3), 1))
        cord (NewNodeID, 2) = 0.5 * (cord (nop (NewElt, 1), 2) + cord (nop (NewElt, 3), 2))
        ao (NewNodeID) = 0.5 * (ao (nop (NewElt, 1)) + ao (nop (NewElt, 3)))
        kmWeight (NewNodeID) = (j + 0.5) / REAL (IntPolNo (i) + 1, KIND = 8)
        if (DIST /= 0.0) then
          kmx (NewNodeID) = kmx (nop (i, 1)) + (j + 0.5) * DIST
        end if
        newFENode => NewNode (NewNodeID, cord (NewNodeID,1), cord (NewNodeID,2), ao(NewNodeID))
        call addNodeToMesh (FE_Mesh, newFENode)
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


end module


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE Read_KALYP_Bed

USE BLK10mod
USE BLKSEDMOD
USE BLKSANMOD

! local variables
INTEGER:: NO_GOK
INTEGER:: ISUSLAY, IACTIV_SL
INTEGER:: IBEDLAY, IACTIV_BL
INTEGER:: istat          !Variable for I/O errors

INTEGER:: NLAY_OLD, NLAYO_OLD
REAL(KIND=8):: BSHEAR_OLD, UST_OLD, DEPRAT_OLD, VS_OLD, EDOT_OLD, SERAT_OLD
REAL(KIND=8):: AO_BED_OLD, TMSED_OLD, SUMTHICK_OLD
REAL(KIND=8):: THICK_OLD(1:10), THICKO_OLD(1:10)
REAL(KIND=8):: SUMTHICK
! ----------------------

! erste Zeile "Zeitschrittweite" und zwei Zeilen Text
READ(IREBED,*)
READ(IREBED,*)
READ(IREBED,*)
NO_GOK = 0

! Reading Bed results in file
! endless reading loop until end of file condition is reached
!-----------------------------------------------------------
 Bed_reading: do
   !read next line from bed-restart file
   read (IREBED, * , iostat = istat)  &
                  NN, BSHEAR_OLD, UST_OLD, DEPRAT_OLD, VS_OLD, &
       &          EDOT_OLD, SERAT_OLD, AO_BED_OLD, TMSED_OLD, NLAY_OLD, NLAYO_OLD,SUMTHICK_OLD,&
       &          (THICK_OLD(L),L=1,NLAY_OLD),(THICKO_OLD(L),L=1,NLAYO_OLD)

   !exit loop, if reaching end of file condition
   if (istat /= 0) exit bed_reading

   IF (NN.gt.MaxP) THEN
     WRITE(*,*) ' Bed-Restart ist nicht kompatibel mit der Modell-Restart Datei.'
     WRITE(*,*) ' >> Programmabbruch da Knotenanzahl nicht uebereinstimmt!'
     WRITE(75,*) ' Bed-Restart ist nicht kompatibel mit der Modell-Restart Datei.'
     WRITE(75,*) ' >> Programmabbruch da Knotenanzahl nicht uebereinstimmt!'
     STOP
   END IF

   IF (AO_BED_OLD.ne.AO(NN) .and. NO_GOK==0) THEN
     WRITE(*,*) ' Achtung: Sohlhoehe aus Restart-Bed nicht identisch mit Modell: ',NN,' .'
     WRITE(75,*) ' Achtung: Sohlhoehe aus Restart-Bed nicht identisch mit Modell: ',NN,' .'
     NO_GOK = 1
     ! Vorbereitung fuer einlesen neuer Sohlgeometrie (jetzt aber noch nicht verfuegbar)
   END IF

   ! Check Suspended Layer formation
   ! ---------------------------------
   IF (NLAY_OLD.ne.NLAYTND(NN)) THEN
     ISUSLAY = 1
     WRITE(*,*) ' Achtung: Anzahl Sus-Layer ', L ,' am Knoten',NN,' aus Restart-Bed ist falsch.'
     WRITE(75,*) ' Achtung: Anzahl Sus-Layer ', L ,' am Knoten',NN,' aus Restart-Bed ist falsch.'

   ElseIF (NLAY_OLD.eq.NLAYTND(NN)) THEN
     ISUSLAY = 0
     IACTIV_SL = 0
     DO L = 1, NLAY_OLD
       ! Change unit
       THICK_OLD(L) = THICK_OLD(L)/1000.0

       IF (THICK_OLD(L).eq.0.0 .and. IACTIV_SL.eq.0) THEN
         THICK(NN,L) = THICK_OLD(L)
         ! Layer sind leer

       ElseIF (THICK_OLD(L).gt.0.0 .and. IACTIV_SL.eq.0) THEN
         IACTIV_SL = 1
         ! Suspended Layer ab hier voll
         IF (THICK_OLD(L).gt.TLAYND(NN,L)) THEN
           TLAYND(NN,L) = THICK_OLD(L)
           ! Ersetzen mit dickerem Layer
           THICK(NN,L) = THICK_OLD(L)
           ! Auffuellen des Susp. Layers
           WRITE(75,*) ' Achtung: Sus-Layer ', L ,' am Knoten',NN,' aus Restart-Bed ist groesser.'
         ElseIF (THICK_OLD(L).lt.TLAYND(NN,L)) THEN
           THICK(NN,L) = THICK_OLD(L)
           ! Neubelegung des Susp. Layers halbvoll
         ElseIF (THICK_OLD(L).eq.TLAYND(NN,L)) THEN
           THICK(NN,L) = THICK_OLD(L)
           ! Neubelegung des Susp. Layers ganzvoll
         Endif
       ElseIF (THICK_OLD(L).gt.0.0 .and. IACTIV_SL.eq.1) THEN
         IACTIV_SL = 1
         ! Suspended Layer ab hier voll
         IF (THICK_OLD(L).gt.TLAYND(NN,L)) THEN
           TLAYND(NN,L) = THICK_OLD(L)
           ! Ersetzen mit dickerem Layer
           THICK(NN,L) = THICK_OLD(L)
           ! Auffuellen des Susp. Layers
           WRITE(75,*) ' Achtung: Sus-Layer ', L ,' am Knoten',NN,' aus Restart-Bed ist groesser.'
         ElseIF (THICK_OLD(L).lt.TLAYND(NN,L)) THEN
           THICK(NN,L) = THICK_OLD(L)
           ! Neubelegung des Susp. Layers halbvoll
         ElseIF (THICK_OLD(L).eq.TLAYND(NN,L)) THEN
           THICK(NN,L) = THICK_OLD(L)
           ! Neubelegung des Susp. Layers ganzvoll
         Endif
       ElseIF (THICK_OLD(L).eq.0.0 .and. IACTIV_SL.gt.1) THEN
         IACTIV_SL = 2
         ! Layer ab hier leer sind leer
       Else
         WRITE(*,*) ' Error: Sus-Layer ', L ,' am Knoten',NN,' aus Restart-Bed fehlerhaft.'
         WRITE(*,*) ' >> Programmabbruch'
         WRITE(75,*) ' Error: Sus-Layer ', L ,' am Knoten',NN,' aus Restart-Bed fehlerhaft.'
         WRITE(75,*) ' >> Programmabbruch'
         STOP
       END IF
     END DO
   Endif



   ! Check Bed Layer formation
   ! ---------------------------------
   IF (NLAYO_OLD.ne.NLAYO(NN)) THEN
     IBEDLAY = 1
   ElseIF (NLAYO_OLD.eq.NLAYO(NN)) THEN
     IBEDLAY = 0
     IACTIV_BL = 0
     DO L = 1, NLAYO_OLD
       ! Change unit
       THICKO_OLD(L) = THICKO_OLD(L)/1000.0

       IF (THICKO_OLD(L).eq.0.0 .and. IACTIV_BL.eq.0) THEN
         THICKO(NN,L) = THICKO_OLD(L)
         ! Bed Layer sind leer

       ElseIF (THICKO_OLD(L).gt.0.0 .and. IACTIV_BL.eq.0) THEN
         IACTIV_BL = 1
         ! Suspended Layer ab hier voll
         IF (THICKO_OLD(L).gt.THICKOND(NN,L)) THEN
           THICKOND(NN,L) = THICKO_OLD(L)
           ! Ersetzen mit dickerem Layer
           THICKO(NN,L) = THICKO_OLD(L)
           ! Auffuellen des Susp. Layers
           WRITE(75,*) ' Achtung: Bed-Layer ', L ,' am Knoten',NN,' aus Restart-Bed ist groesser.'
         ElseIF (THICKO_OLD(L).lt.THICKOND(NN,L)) THEN
           THICKO(NN,L) = THICKO_OLD(L)
           ! Neubelegung des Susp. Layers halbvoll
         ElseIF (THICKO_OLD(L).eq.THICKOND(NN,L)) THEN
           THICKO(NN,L) = THICKO_OLD(L)
           ! Neubelegung des Susp. Layers ganzvoll
         Endif
       ElseIF (THICKO_OLD(L).gt.0.0 .and. IACTIV_BL.eq.1) THEN
         IACTIV_BL = 1
         ! Suspended Layer ab hier voll
         IF (THICKO_OLD(L).gt.THICKOND(NN,L)) THEN
           THICKOND(NN,L) = THICKO_OLD(L)
           ! Ersetzen mit dickerem Layer
           THICKO(NN,L) = THICKO_OLD(L)
           ! Auffuellen des Susp. Layers
           WRITE(75,*) ' Achtung: Bed-Layer ', L ,' am Knoten',NN,' aus Restart-Bed ist groesser.'
         ElseIF (THICKO_OLD(L).lt.THICKOND(NN,L)) THEN
           THICKO(NN,L) = THICKO_OLD(L)
           ! Neubelegung des Susp. Layers halbvoll
         ElseIF (THICKO_OLD(L).eq.THICKOND(NN,L)) THEN
           THICKO(NN,L) = THICKO_OLD(L)
           ! Neubelegung des Susp. Layers ganzvoll
         Endif

  !MD: Dieser Fall noch nicht möglich, aber bald:
  !MD:     ElseIF (THICKO_OLD(L).eq.0.0 .and. IACTIV_BL.gt.1) THEN
  !MD:       IACTIV_BL = 2
  !MD:       ! Layer ab hier leer sind leer
       Else
         WRITE(*,*) ' Error: Bed-Layer ', L ,' am Knoten',NN,' aus Restart-Bed fehlerhaft.'
         WRITE(*,*) ' >> Programmabbruch'
         WRITE(75,*) ' Error: Bed-Layer ', L ,' am Knoten',NN,' aus Restart-Bed fehlerhaft.'
         WRITE(75,*) ' >> Programmabbruch'
         STOP
       END IF
     END DO
   Endif

 END DO Bed_reading
 WRITE(*,*) ' Ende des Einlesens der Bed-Datei'


CLOSE (IREBED, STATUS='keep')

!--------------- Formatdefinition: -----------------------------
!MD:   3000 format (1X, 'Opening output file failed. (IOSTAT =',I2,')',/ &
!MD:     &        1X, 'Stopping Program...')


END SUBROUTINE Read_KALYP_Bed

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

