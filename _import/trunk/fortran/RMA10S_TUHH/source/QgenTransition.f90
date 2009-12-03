!     Last change:  WP   26 Feb 2008    3:53 pm
      SUBROUTINE QGENtrans (TLine,TNode,QREQ,THET, waspi)

      !nis,jan07: Overgiven variables
      ! J    = number of CCL
      ! QREQ = total discharge across the CCL J
      ! THET = direction of total discharge crossing the CCL; measured from the principal x-axis in ANTICLOCKWISE RADIANS)

      !modules
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE ParaKalyps

      !every variable should be defined
      implicit none

      SAVE

      !real onedimensional arrays
      REAL          :: amec (3535), Conveyance (3535), dxl (3535), specdischarge(3535)

      !real variables 
      REAL          :: cwr_line
      REAL (KIND=8) :: lambda, NikuradseRoughness
      REAL (KIND=8) :: tmpvx,tmpvy,tmpdepth,tmpwl
      REAL (KIND=8) :: fliesstiefe, waspi
      REAL (KIND=8) :: d1,d3,d1v,d3v,d2_kind8
      !nis,aug07: dummy parameters for passing
      REAL (KIND=8), DIMENSION(1:3) :: dummy
      REAL (KIND = 8) :: NullVal
      REAL          :: d2
      REAL          :: vecq, vest
      REAL          :: cos_in_grad, sin_in_grad
      REAL          :: thet, qreq, total
      REAL          :: dx, dy, xl, dx1, dy1, dx2, dy2
      real          :: suma, wurz, sumx, sumy
      REAL (KIND=8) :: TDep, TDepv
      REAL          :: DUM1, DUM2

      !integer variables
      INTEGER       :: maxL, mel
!      INTEGER       :: iostaterror
      INTEGER       :: no, k, l, i, Comp
      INTEGER       :: nod
      INTEGER       :: TLine, TNode
      INTEGER       :: na, nc, nbb, na1, na2, ncc
      REAL (KIND=8) :: weight(0:2), di(0:2)
      REAL (KIND=8) :: dxline, dyline

      !character variables
      CHARACTER (LEN=27)  :: filename

!nis,jan07,test output file handling
!filename = 'Kopplungsoutput.txt'
!-

NullVal = 0.0

!!!!!!!!!!!!!!!!!!!!!!!!!!
!initialization block
!!!!!!!!!!!!!!!!!!!!!!!!!!

!initializing for every possible node of transition line
DO k = 1, lmt (TLine)
  !Conveyance factor
  Conveyance (k) = 0.0
  !segment length
  dxl (k) = 0.0
END DO

!weights for simpson rule
weight(0) = 1.0
weight(1) = 4.0
weight(2) = 1.0

!iostaterror = 0

!!!!!!!!!!!!!!!!!!!!!!!!!!
!working block
!!!!!!!!!!!!!!!!!!!!!!!!!!

!get number of nodes at line; zero-values can not occur, this was tested in the calling subroutine
maxL = LMT(TLine)

!transition line must contain more than one line segment
if (maxL < 2) STOP 'ERROR - transition contains of less then 2 segments'

!starting node, ending node
na = line (TLine, 1)
nc = line (TLine, maxL)

!x- and y- chord length of transition line
dxline = cord (nc, 1) - cord (na, 1)
dyline = cord (nc, 2) - cord (na, 2)

!Initialize factor suma
suma = 0.

!For next loop the maximum value is the penultimate CORNER node
maxL = lmt (TLine) - 2

!nis,sep06,com: Loop over all corner nodes till the last one of CCL to get the Conveyance factor of the single parts of line.
ThroughNodesOfLine: DO k = 1, maxL, 2
  !nis,sep06,com: Get the first (na), midside (ncc), and endnode (nc) of the actual line segment
  na  = line (TLine, k)
  ncc = line (TLine, k + 1)
  nc  = line (TLine, k + 2)

  !nis,sep06,com: Calculate the x- and y- length of the actual CCL-segment
  dx = cord (nc, 1) - cord (na, 1)
  dy = cord (nc, 2) - cord (na, 2)

  !counting sixth length of segment projected on the chord of the transition length (it's vector product for angle calculation)
  xl = ABS((dx * dxline + dy * dyline) / (6 * SQRT (dxline**2 + dyline**2)))

  !nis,jun07: switched off Marsh-Algorithm:
  IF (idnopt == 0) then
    !waterdepth
    di(0) = waspi - ao (na)
    di(2) = waspi - ao (nc)

  !nis,jun07: Marsh-Algorithm:
  ELSE
    !waterdepth
    d1v = waspi - ado (na)
    d3v = waspi - ado (nc)

    !Transform to Marsh-depth
    CALL amf (d1v, di(0), akp (na), adt (na), adb (na), amec (k), dum1, 1)
    CALL amf (d3v, di(2), akp (nc), adt (nc), adb (nc), amec (k), dum1, 1)
  ENDIF
  !-

  !Set depth to 0.0, if it was smaller zero
  IF (di(0) <= 0.0) di(0) = 0.0
  IF (di(2) <= 0.0) di(2) = 0.0

  !Average the water depth for the midside node
  di(1) = (di(0) + di(2)) / 2.0

  do no = 0, 2
    !Darcy-Weisbach:
    IF (ort (lineimat (TLine, k + 1), 15) > 0.) then

      !actual node
      nod = line (TLine, k+no)

      !calculate the absolute flow-velocity of beginning corner node of the actual CCL-segment
      vecq = sqrt (vel (1, nod)**2 + vel (2, nod) **2)

      !default vegetation cwr-value
      cwr_line = 1.0

      !get lambda
      NikuradseRoughness = ort(lineimat(TLine, k+1), 15)
      CALL darcy (lambda, vecq, di(no), NikuradseRoughness, NullVal, NullVal, 0,  0, gl_bedform, mel, cwr_line, 2, &
     &            dummy(1), dummy(2), dummy(3),dset)

      !Correct roughness, if there is a material (imat) factor (when marsh-option is active)
      if (idnopt /= 0 .and. di(no) < akp(nod) * adb(nod)) then
        lambda = lambda * (ort (lineimat (TLine, k + 1), 12)**2 - 1.) * (akp(nod) * adb(nod) - di(no)) / (akp(nod) * adb(nod)) + 1.0
      end if

      !Conveyance represents the Conveyance factor of the element part.
      Conveyance(k + no) = Conveyance(k + no) + xl * weight(no) * di(no)**(3./2.) * ((78.48/lambda)**0.5)

      !Calculate the influence width of the nodes of the segment. Corner nodes are a combination of the two adjacent elemtns
      dxl (k + no) = dxl (k + no) + xl * weight(no)

    !Manning-Strickler:
    ELSE

      !Calculating the Conveyance factors with the law of Manning Strickler
      Conveyance (k+no) = Conveyance (k+no) + xl * weight(no) * (di(no)** (5. / 3.) ) * ( - 1. * ort (lineimat (TLine, k + 1), 5))

    ENDIF
  enddo
END DO ThroughNodesOfLine

!Refresh the max-value with the number of nodes of the actual CCL
maxL = lmt (TLine)

!Summing of the Conveyance factors
DO k = 1, maxL
  suma = suma + Conveyance (k)
END DO

!Testing of the Conveyance factors
suma = abs (suma)
IF (abs (suma / qreq) .lt.0.0000001) then
  WRITE (Lout, * ) 'no flow at transition'
  WRITE (Lout, * ) 'no flow depth at  transition-line: ', TLine
  STOP 'no flow at transition line'
ENDIF

!Due to the formula of the Conveyance factor vest is the root of the energy slope (Q=sqrt(I)*K; K=Conveyance factor)
vest = qreq / suma

!Set values for the flow velocity at every node of transition

!Run through every node of the CCL for applying the specified discharge
AssignVelocities: DO k = 1, maxL

  !Get the actual node number
  na = line (TLine, k)
  !Skip non existing point
  IF (na.le.0) cycle AssignVelocities

  !Process on midsidenodes to get their depths
  IF (mod (k, 2) == 0) then
    !Get the adjacent corner nodes
    na1 = line (TLine, k - 1)
    na2 = line (TLine, k + 1)

    !If wetting/drying is activated, just calculate the waterdepths of the two adjacent corner nodes of the actual midside node
    IF (idnopt.eq.0) then
      d1 = waspi - ao (na1)
      d3 = waspi - ao (na2)
    !If marsh-algorithm is applied, calculate the waterdepths of the two adjacent corner nodes of the actual midsied node and
    !  transform them afterwards
    ELSE
      d1v = waspi - ado (na1)
      d3v = waspi - ado (na2)

      CALL amf (d1v, d1, akp (na1), adt (na1), adb (na1), amec (k - 1), d2, 1)
      CALL amf (d3v, d3, akp (na2), adt (na2), adb (na2), amec (k + 1), d2, 1)
    ENDIF

    !Set to 0, if neglectable depth
    IF (d1 <= 0.0) d1 = 0.0
    IF (d3 <= 0.0) d3 = 0.0
    !Average the values (question remark: Shouldn't that be the waterdepth of the midside node? e.g. d2)
    d1 = (d1 + d3) / 2.0

  !Process on corner nodes to get their flow depth
  ELSE
    !no special drying/wetting algorithm
    IF (idnopt == 0) then
      d1  = waspi - ao (na)
    !marsh active
    ELSE
      d1v = waspi - ado (na)
      CALL amf (d1v, d1, akp (na), adt (na), adb (na), amec (k), d2, 1)
    ENDIF
  ENDIF

  !Set depth 0, if ngelectable
  IF (d1 <= 0.0) d1 = 0.0

  specdischarge (k) = vest * Conveyance (k) / dxl (k)

  !Set the absolute velocities at 2D transition nodes and copy specific discharges to global arrays
  if (d1 > 0.0) then
    TransSpec(k) = specdischarge(k)
  else
    TransSpec(k) = 0.0
  end if

ENDDO assignvelocities

END
