!     Last change:  WP    9 Jul 2007    8:16 am
      SUBROUTINE QGENtrans (TLine,TNode,QREQ,THET, TDep)

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
      REAL          :: amec (350), Conveyance (350), dxl (350), specdischarge(350,2)

      !real variables 
      REAL          :: lambda, cwr_line
      REAL (KIND=8) :: tmpvx,tmpvy,tmpdepth,tmpwl
      REAL (KIND=8) :: fliesstiefe, waspi
      REAL (KIND=8) :: d1,d3,d1v,d3v,d2_kind8
      REAL          :: d2
      REAL          :: vecq, vest
      REAL          :: cos_in_grad, sin_in_grad
      REAL          :: thet, qreq, total
      REAL          :: dx, dy, xl, dx1, dy1, dx2, dy2
      real          :: alp, suma, wurz, sumx, sumy
      REAL          :: TDep, TDepv
      REAL          :: DUM1, DUM2

      !integer variables
      INTEGER       :: maxL, mel
!      INTEGER       :: iostaterror
      INTEGER       :: k, l, i, Comp
      INTEGER       :: TLine, TNode
      INTEGER       :: na, nc, nbb, na1, na2, ncc

      !character variables
      CHARACTER (LEN=27)  :: filename

!nis,jan07,test output file handling
!filename = 'Kopplungsoutput.txt'
!-

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
dx = cord (nc, 1) - cord (na, 1)
dy = cord (nc, 2) - cord (na, 2)

!global angle between x-axis and chord
alp = atan2 (dx, dy)

!Initializing average waterlevel at the line
waspi = 0.0

!initialize water surface elevation waspi at transition
!marsh-option not active
if (idnopt == 0) then
  !calculate water level
  waspi = ao(TNode) + TDep

!marsh-option active
else
  !calculate water level with marsh-option, first transform virtual depth to "real" flow depth and then add bottom elevation of marsh-range
  CALL amf (TDepv, TDep, akp (TNode), adt (TNode), adb (TNode), dum1, dum2, 0)
  waspi = ado(TNode) + TDepV

end if

!testfile ouput
WRITE(999,*) 'Average waterlevel at transition:', waspi
WRITE(999,*) 'required discharge:              ', qreq
WRITE(999,*) 'inflow angle:                    ', thet
!-

!Initialize factor suma
suma = 0.

!nis,sep06,com: For next loop the maximum value is the penultimate CORNER node
maxL = lmt (TLine) - 2


!nis,sep06,com: Loop over all corner nodes till the last one of CCL to get the Conveyance factor of the single parts of line.
ThroughNodesOfLine: DO k = 1, maxL, 2
  !nis,sep06,com: Get the first (na), midside (ncc), and endnode (nc) of the actual line segment
  na  = line (TLine, k)
  ncc = line (TLine, k + 1)
  nc  = line (TLine, k + 2)

  !nis,sep06,com: Calculate the x- and y- length of the actual CCL-segment
  dx = cord (nc, 1) - cord (na, 1)
  dy = - (cord (nc, 2) - cord (na, 2) )
  !nis,sep06,com: Calculate the sixth part of the Euklidean length of the actual CCL-segment, it's used for the Simpson Integration rule
  xl = sqrt (dx**2 + dy**2) * 1 / 6

  !nis,sep06,com: Calculate the direction angle of the actual CCL-segment
  alp = atan2 (dx, dy)

  !nis,jun07: switched off Marsh-Algorithm:
  IF (idnopt == 0) then
    !waterdepth
    d1 = waspi - ao (na)
    d3 = waspi - ao (nc)

  !nis,jun07: Marsh-Algorithm:
  ELSE
    !waterdepth
    d1v = waspi - ado (na)
    d3v = waspi - ado (nc)

    !Transform to Marsh-depth
    CALL amf (d1v, d1, akp (na), adt (na), adb (na), amec (k), d2, 1)
    CALL amf (d3v, d3, akp (nc), adt (nc), adb (nc), amec (k), d2, 1)
  ENDIF
  !-

  !Set depth to 0.0, if it was smaller zero
  IF (d1 <= 0.0) d1 = 0.0
  IF (d3 <= 0.0) d3 = 0.0

  !Average the water depth for the midside node
  d2 = (d1 + d3) / 2.0

  !nis,jan07,testing
  WRITE(999,*) 'Nikuradse:', ort(lineimat(TLine,k+1),15)
  WRITE(999,*) lineimat(TLine,k+1)

  !If roghness-value.gt.0 applied Darcy-Weisbach-law (ks-value)
  !IF (cniku (lineimat (TLine, k + 1)) .gt.0.) then
  IF (ort(lineimat(TLine,k+1),15) .gt.0.) then
  !Darcy-Weisbach:
                                                                        
    !Beginning corner node of the actual CCL-segment (1/6 of the element width)
    !*****************************************************************************************
    !calculate the absolute flow-velocity of beginning corner node of the actual CCL-segment
    vecq = sqrt (vel (1, na) **2 + vel (2, na) **2)

    !Calculate the flow resistance of the beginning corner node of the actual CCL-segment
    !Replacing 1.0 by cwr_line for intentinout property of the Darcy-Subroutine and adding variable setting before
    !  usage of gl_bedform is necessary because, the parameter is already in the Darcy-Subroutine, but there are no
    !  calculations for that yet.
    cwr_line = 1.0
    CALL darcy (lambda, vecq, d1, ort(lineimat(TLine, k+1), 15), 0., 0., 0,  0, gl_bedform, mel, cwr_line, 2)

    !Correct roughness, if there is a material (imat) factor
    if (idnopt /= 0 .and. d1 < akp(na) * adb(na)) then
      lambda = lambda * (ort(lineimat(TLine,k+1), 12)**2 - 1.) * (akp(na) * adb(na) - d1) / (akp(na) * adb(na)) + 1.0
    end if

    !Conveyance represents the Conveyance factor of the element part.
    Conveyance(k) = Conveyance(k) + xl*(d1** (3./2.)) * ((78.48/lambda)**0.5)

    !nis,jan07,testing
    WRITE(999,*) 'Inputgeschwindigkeit am Knoten: ', na, ' entspricht Linienknoten Nr. ',k,' :', sqrt(vel(1,na)**2+vel(2,na)**2)
    WRITE(999,*) 'Geschwindigkeitskomponenten:  ', vel(1,na), vel(2,na)
    WRITE(999,*) 'Einflussbreite: ', 2*xl, ' Wassertiefe: ', d1
    WRITE(999,*) 'aktueller Fliesswiderstandsbeiwert lambda: ', lambda

    !nis,sep06,com: Initialize the total flow velocity
    vecq = 0.0

    !nis,sep06,com: midside node of the actual CCL-segment (4/6 of the element width)
    !********************************************************************************
    !nis,sep06,com: Calculate the flow velocity at the midside node of the actual CCL-segment
    IF (ncc.gt.0) vecq = sqrt (vel(1,ncc)**2 + vel(2,ncc)**2)
    !Calculate the flow resistance of the midside node of the actual CCL-segment
    !Replacing 1.0 by cwr_line for intentinout property of the Darcy-Subroutine and adding variable setting before,
    !  usage of gl_bedform is necessary because, the parameter is already in the Darcy-Subroutine, but there are no
    !  calculations for that yet.
    !  d2 must be KIND=4, so it is set by a temporary variable.
    cwr_line = 1.0
    d2_kind8 = d2
    CALL darcy(lambda, vecq, d2_kind8, ort(lineimat(TLine,k+1),15), 0., 0., 0,  0, gl_bedform, mel, cwr_line, 2)
    d2 = d2_kind8

    !Correct roughness, if there is a material factor
    if (idnopt /= 0 .and. d2 < akp(ncc) * adb(ncc)) then
      lambda = lambda * (ort(lineimat(TLine,k+1), 12)**2 - 1.) * (akp(ncc) * adb(ncc) - d2) / (akp(ncc) * adb(ncc)) + 1.0
    end if

    !nis,sep06,com: Calculate the Conveyance factor of the midside node of the actual CCL-segment
    Conveyance (k + 1) = xl * 4 * (d2** (3. / 2.) ) * ((78.48/lambda)**0.5)

    !nis,jan07,testing
    WRITE(999,*) 'Inputgeschwindigkeit am Knoten: ', ncc, ' entspricht Linienknoten Nr. ',k+1,' :', vecq
    WRITE(999,*) 'vektorielle Geschwindigkeiten:  ', vel(1,ncc), vel(2,ncc)
    WRITE(999,*) 'Einflussbreite: ', 4*xl, ' Wassertiefe: ', d2
    WRITE(999,*) 'aktueller Rauhigkeitsbeiwert: ', lambda

    !nis,sep06,com: Initialize the total flow velocity
    vecq = 0.0

    !Ending corner node of the actual CCL-segment (1/6 of the element width)
    !**************************************************************************************
    !Calculate the total velocity of the ending corner node of the actual CCL-segment
    vecq = sqrt (vel (1, nc) **2 + vel (2, nc) **2)
    !Calculate the flow resistance of the ending corner node of the actual CCL-segment
    !Replacing 1.0 by cwr_line for intentinout property of the Darcy-Subroutine and adding variable setting before,
    !  usage of gl_bedform is necessary because, the parameter is already in the Darcy-Subroutine, but there are no
    !  calculations for that yet.
    cwr_line = 1.0
    CALL darcy (lambda, vecq, d3, ort(lineimat(TLine,k+1),15), 0., 0., 0,  0, gl_bedform, mel, cwr_line, 2)

    !Correct roughness, if there is a material factor
    if (idnopt /= 0 .and. d3 < akp(nc) * adb(nc)) then
      lambda = lambda * (ort(lineimat(TLine, k+1), 12)**2 - 1.) * (akp(nc) * adb(nc) - d3) / (akp(nc) * adb(nc)) + 1.0
    end if

    !Calculate the Connyance factor of the ending cornder node of the actual CCL-segment
    Conveyance (k + 2) = Conveyance (k + 2) + xl * (d3**(3./2.)) * ((78.48/lambda)**0.5)

  ELSE
  !Manning-Strickler:

    !Calculating the Conveyance factors for beginning, midside and ending nodes of the actual elements
    !  with the law of Manning Strickler
    Conveyance (k)     = Conveyance (k) + xl * (d1** (5. / 3.) ) * ( - 1. * ort (lineimat (TLine, k + 1), 5) )
    Conveyance (k + 1) = xl * 4. * (d2** (5. / 3.) ) * ( - 1. *  ort (lineimat (TLine, k + 1), 5) )
    Conveyance (k + 2) = Conveyance (k + 2) + xl * (d3** (5./3.))* ( - 1. * ort (lineimat (TLine, k + 1), 5) )

  ENDIF

  !Calculate the influence width of the nodes of the segment. Corner nodes are a combination of the two adjacent elemtns
  dxl (k) = dxl (k) + xl
  dxl (k + 1) = xl * 4
  dxl (k + 2) = dxl (k + 2) + xl

END DO ThroughNodesOfLine

!Refresh the max-value with the number of nodes of the actual CCL
maxL = lmt (TLine)

!testfile ouput
do i = 1, maxL
 WRITE(999,*) 'Conveyance Factor of node ', i,':',Conveyance(i)
end do

!Summing of the Conveyance factors
DO k = 1, maxL
  suma = suma + Conveyance (k)
END DO

!testfile ouput
WRITE(999,*) 'Conveyance Factor sum test:', suma
WRITE(999,*) '                 overall Q:', qreq

!Testing of the Conveyance factors
suma = abs (suma)
IF (abs (suma / qreq) .lt.0.0000001) then
  WRITE (Lout, * ) 'no flow at transition'
  WRITE (Lout, * ) 'no flow depth at  transition-line: ', TLine
  STOP 'no flow at transition line'
ENDIF

!Calculation of estimated flow velocity
!(question remark: Due to the formula of the Conveyance factor vest is the slope??? Q=sqrt(I)*K; K=Conveyance factor)
vest = qreq / suma

!testing
WRITE(999,*) 'Slope is: ', vest ,'(',qreq, suma, ')'


!Set values for the flow velocity at every node of transition

!Run through every node of the CCL for applying the specified discharge
AssignVelocities: DO k = 1, maxL
  !Get the actual node number
  na = line (TLine, k)
  !Skip non existing point
  IF (na.le.0) cycle AssignVelocities

  !Initialize values
  dx1 = 0.0
  dy1 = 0.0
  dx2 = 0.0
  dy2 = 0.0

  !Testing whetherc node is the first of the actual CCL
  IF (k.eq.1) then
    !Calculate the x-distance to the second node of the actual CCL
    dx1 = cord (line (TLine, k + 1), 1) - cord (line (TLine, k), 1)
    !Calculate the y-distance to the second node of the actual CCL
    dy1 = cord (line (TLine, k + 1), 2) - cord (line (TLine, k), 2)
    !Copy the values
    dx2 = dx1
    dy2 = dy1

  !node is the last of the actual CCL
  ELSEIF (k.eq.maxL) then
    !Calculate the x-distance to the penultimate node of the actual CCL
    dx1 = cord (line (TLine, k), 1) - cord (line (TLine, k - 1), 1)
    !Calculate the y-distance to the penultimate node of the actual CCL
    dy1 = cord (line (TLine, k), 2) - cord (line (TLine, k - 1), 2)
    !Copy the values
    dx2 = dx1
    dy2 = dy1

  !node is in between the first and last of the actual CCL
  ELSE
    !Calculate the x- and y- distances to the node before the actual one of the actual CCL
    IF (line (TLine, k - 1) .gt.0) then
      dx1 = cord (line (TLine, k), 1) - cord (line (TLine, k - 1), 1)
      dy1 = cord (line (TLine, k), 2) - cord (line (TLine, k - 1), 2)
    ENDIF
    !Calculate the x- and y- distances to the node after the actual one of the actual CCL
    IF (line (TLine, k + 1) .gt.0) then
      dx2 = cord (line (TLine, k + 1), 1) - cord (line (TLine, k), 1)
      dy2 = cord (line (TLine, k + 1), 2) - cord (line (TLine, k), 2)
    ENDIF
  ENDIF
                                                                        
  !Calculate the squared Euclidic distance to the node before the actual one
  wurz = (dx1**2 + dy1**2) ![m²]

  !If the squared length is not neglectable, calculate the normalized values of the x- and y- distances.
  !  The result is the divided normalized vector parallel to the actual segment direction
  IF (wurz.gt.0.00001) then
    wurz = wurz**0.5            ![m]
    dx1 = ABS(dx1 / wurz)       ![m/m] = [-]
    dy1 = abs(dy1 / wurz)       ![m/m] = [-]
  ENDIF
                                                                        
  !Calculate the squared Euclidic distance to the node after the actual one
  wurz = (dx2**2 + dy2**2)

  !If the squared length is not neglectable, calculate the normalized values of the x- and y- distances.
  !  The result is the divided normalized vector parallel to the actual segment direction
  IF (wurz.gt.0.00001) then
    wurz = wurz**0.5
    dx2 = abs(dx2 / wurz)
    dy2 = abs(dy2 / wurz)
  ENDIF
                                                                        
  !Process on midsidenodes
  IF (mod (k, 2) .eq.0) then
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

  !Process on corner nodes
  ELSE
    !Calculate the water depth of the actual node of the actual node of actual CCL
    !  due to the drying/wetting algorithm or marsh-algorithm
    !  na is still the actual node number of actual node of actual CCL
    IF (idnopt == 0) then
      d1 = waspi - ao (na)
    ELSE
      d1v = waspi - ado (na)

      CALL amf (d1v, d1, akp (na), adt (na), adb (na), amec (k), d2, 1)
    ENDIF
  ENDIF
                                                                        
  !Set depth 0, if ngelectable
  IF (d1 <= 0.0) d1 = 0.0

  !testing
  WRITE(999,*) 'lengths: ', dx1, dy1, dx2, dy2, dxl(k)

  !Thet defines the inflow angle of the discharge, 0 means it is perpendicular to the chord of the line-segment
  IF (thet.eq.0.0) then
    !Stroemungsrichtung senkrecht zum jeweiligen Liniensegment

    !Checking the angle between the segment(part)s at the actual node. The question comes from the relation:
    !   dx1   dx2
    !   --- = --- => dx1 * dy2 - dx2 * dy1 = 0, if the angle is (nearly .lt. 0.0001.) the same.
    !   dy1   dy2
    IF (abs (dy2 * dx1 - dy1 * dx2) .lt.0.0001) then
      !Not clear yet (25092006)
      IF ( (dx1**2 + dy1**2) .gt.0.9) then
        WRITE(999,*) 'stelle1'
        specdischarge(k,1) = vest * (Conveyance (k) / dxl (k) ) * ( dy1)
        specdischarge(k,2) = vest * (Conveyance (k) / dxl (k) ) * ( dx1)
      ELSEIF ( (dx2**2 + dy2**2) .gt.0.9) then
        WRITE(999,*) 'stelle2'
        specdischarge(k,1) = vest * (Conveyance (k) / dxl (k) ) * ( dy2)
        specdischarge(k,2) = vest * (Conveyance (k) / dxl (k) ) * ( dx2)
      ENDIF

    !If the angle is not the same, the discharge direction is specified with the direction of the bisector
    ELSE
      !im Knick zwischen den Elementseiten wird der Durchfluss
      !so gesetzt, dass senkrecht ueber jede Seite der verlangte
      !Abfluss eintritt. Die Richtung ergibt sich somit zur Winkel-
      !halbierenden, der Betrag des gesetzten Abflusses ist groesser
      !als der vorgegebenen Abfluss.

      WRITE(999,*) 'stelle3'
      specdischarge(k,1) = vest * (Conveyance (k) / dxl (k) ) * ((dx2 - dx1) / (dx1 * dy2 - dx2 * dy1) )
      specdischarge(k,2) = vest * (Conveyance (k) / dxl (k) ) * ((dy2 - dy1) / (dx1 * dy2 - dx2 * dy1) )
    ENDIF
  ELSE

    !Stroemungsrichtung im Winkel ',thet,' Grad zur X-Richtung

    !29.01.98 cosd/sind unbekannt ?
    !cosd/sind - liefert Vinkel in Grad
    !cos/sin   - liefert Vinkel in Radian
    !Umrechnung vom Radian zu Grad:  cos(alpha)*180¹/pi

    !Calculate the Cosinus of the inflow angle relative to the inflow line
    !  and then evaluate the specific inflow at the node, vest seems to be the averaged slope
    cos_in_grad = (cos (thet) * 180) / 3.141592654
    specdischarge(k,1) = vest * (Conveyance(k) / dxl(k)) * cos_in_grad
    !Calculate the Sinus of the inflow angle relative to the inflow line
    !  and then evaluate the specific inflow at the node, vest seems to be the averaged slope
    sin_in_grad = (sin (thet) * 180) / 3.141592654
    specdischarge(k,2) = vest * (Conveyance(k) / dxl(k)) * sin_in_grad
  ENDIF

  !Set the absolute velocities at 2D transition nodes and copy specific discharges to global arrays
  if (d1 > 0.0) then
    TransitionVels (k) = SQRT( (specdischarge (k,1) / d1)**2 + (specdischarge (k,2) / d1)**2)
    TransSpec(k, 1) = specdischarge(k, 1)
    TransSpec(k, 2) = specdischarge(k, 2)
  else
    TransitionVels (k) = 0.0
    TransSpec(k, 1) = 0.0
    TransSpec(k, 2) = 0.0
  end if
ENDDO assignvelocities

!Write to test output file
WRITE(999,*) 'Output of velocity and specific discharge distribution'
WRITE(999,*) 'node,     qx     ,     qy     ,     v    ,   b   '
do i = 1, maxL
  WRITE(999,'(i6,4(1x,f12.7),1x,f8.4)') line(TLine,i), (TransSpec(i, Comp), Comp = 1, 2), TransitionVels(i), dxl(i)
end do

!Checking the continuity or rather the correct transformation of the boundary condition
!controlling and correction

!Initializing sumx and sumy
sumx = 0.0
sumy = 0.0

!Set the number of segment-starting-nodes in the CCL
maxL = lmt (TLine) - 2

!Run through all Corner nodes of the the CCL
GetControlDischarge: DO k = 1, maxL, 2

  !Get the first corner node of segment
  na = line (TLine, k)

  !Get the midside node of the segment
  nbb = line (TLine, k + 1)

  !Cycling the loop, if segment is dry
  IF (ndry (nbb) .ne.1) CYCLE GetControlDischarge
  IF (nbb.le.0) CYCLE GetControlDischarge

  !Get the segment-ending-node of the CCL
  nc = line (TLine, k + 2)

  !Calculate the sixth of the segment length
  dx = ABS((cord (nc, 1) - cord (na, 1) ) / 6.)
  dy = ABS((cord (nc, 2) - cord (na, 2) ) / 6.)

  !Calculate the discharge-part of the absolute CCL-discharge at the actual segment,
  !  using the Simpson-rule
  !  Simpsonformel integriert Parabeln exakt
  sumx = sumx + dy * (specdischarge(k, 1) + 4.0 * specdischarge (k+1, 1) + specdischarge (k+2, 1))
  sumy = sumy + dx * (specdischarge(k, 2) + 4.0 * specdischarge (k+1, 2) + specdischarge (k+2, 2))

END DO GetControlDischarge

!Calculate Correction-factor total of discharge
!  korrekte Summation der Durchflusskomponenten auch fuer nicht gerade (polygon) Raender:
total = sumx + sumy

WRITE(999,*) 'qreq: ', qreq, ' total: ', total
WRITE(999,*) 'sumx: ', sumx, ' sumy: ', sumy

!correction factor
total = abs (qreq) / abs (total)

!Correct the discharge at the nodes by scaling with correction-factor
DO k = 1, maxL
  na = line(TLine, k)
  IF (na.gt.0) then
    TransitionVels (k) = TransitionVels (k) * total
    TransSpec(k, 1) = TransSpec(k, 1) * total
    TransSpec(k, 2) = TransSpec(k, 2) * total
  ENDIF
END DO
                                                                        
!Control output
DO k = 1, maxL
  WRITE (999,9898)  TLine, line(TLine,k), TransSpec(k, 1), TransSpec(k, 2), TransitionVels(k)
END DO
!WRITE (Lout,*) ' gesetzter Durchfluss an Linie', TLine, ' ist: ',qreq, ' Durchflussverhaeltnis  ist: ', total
WRITE (999 ,*) ' gesetzter Durchfluss an Linie', TLine, ' ist: ',qreq, ' Durchflussverhaeltnis  ist: ', total

9898  format (1x,'Q-Linie',i6.6,' Knoten: ',i6.6,' spec-X= ',f8.5, ' spec-Y= ', f8.5,' abs. nod. vel:',f8.5)
                                                                        
RETURN
END
