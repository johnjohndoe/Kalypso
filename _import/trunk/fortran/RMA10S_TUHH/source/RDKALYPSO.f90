!     Last change:  WP   14 Jan 2008    3:15 pm
!-----------------------------------------------------------------------
! This code, data_in.f90, performs reading and validation of model
! inputa data in the library 'Kalypso-2D'.
! Copyright (C) 2004  SEBASTIAN RATH & WOLF PLOEGER.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Wolf Ploeger:     phone: +49 40 42878 4305 mail: ploeger@tuhh.de
! Sebastian Rath:   phone: +49 40 42878 4180 mail: s.rath@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-2D'.
!
! Sebastian Rath, 09 August 2004
! Research Associate
!




!-----------------------------------------------------------------------
!NiS,mar06: Name-Conflict in RMA10S, so this specific Kalypso-2D subroutine is renamed
!      SUBROUTINE check
SUBROUTINE check_Kalypso
!
! This Subroutine is checking the lines of continuity and
! adding the mid-side nodes in the first run.
! In all the additional runs (at the end of a converged time
! step) the discharge over the different continuity lines
! is calculated.


!NiS,mar06: change the module-access for global variables to the RMA10S-construction
!INCLUDE "common.cfg"
USE BLK10MOD
USE BLKDRMOD

!NiS,mar06: missing - lineimat, ndry


! Local variables:
INTEGER, DIMENSION (0:60) :: itemp
DATA ncall / 0 /

! If there are no lines of continuity defined
IF (ncl .le. 0) return

! If this subroutine is NOT called for the first time
first_call: IF (ncall.le.0) THEN

  ncall = 1

  !-.....augment continuity lists.....
  !  Es wird davon Ausgegangen, dass bei den Kontinuitaetslinien nur
  !  die Eckknoten eingegeben wurden. Daher werden hier beim ersten Aufruf
  !  dieser Subroutine die Mittseitenknoten ergaenzt.

  lines: DO j = 1, ncl

    ! LMT(j) = Number of nodes in line of continuity
    m = lmt (j)

    ! Eckknotennummern der Linie (line) in itemp umspeichern:
    ! line = Knotennummern der Kontinuitaetslinien (l : Liniennummer , h: Knotenzaehler)
    DO k = 1, m
      itemp (k) = line (j, k)
    END DO

    ! Knotenanzahl der Linie (lmt) mit Mittseitenknoten errechnen:
    lmt (j) = 2 * lmt (j) - 1
    nn = lmt (j)
    n = 0

    IF (m .eq. 1) CYCLE lines

    !Initialize line array (nodes of continuity lines; first just corner nodes, later midsides are filled in!)
    DO l = 1, nn
      line (j, l) = 0
    END DO

    !  Nun werden alle Eckknoten der Linie bis zum Vorletzten durgegangen:
    nodes: DO l = 1, nn - 2, 2

      n = n + 1
      ! l ist die stelle in der vollstaendigen linie
      ! und n in der unvollstaendigen.
      na = itemp (n)
      nc = itemp (n + 1)
      line (j, l) = na
      line (j, l + 2) = nc
      !  na und nc sind die Eckknotennummern zwischen denen ein
      !  Mittseitenknoten gefunden werden soll:
      !  Dazu werden alle Elemente ne durchsucht, ab ein Rand die
      !  Eckknoten na/nc besitzt:

      elements: DO jj = 1, ne

        IF (imat (jj) .eq. 0) CYCLE elements
        ncn = ncorn (jj)

        DO 117 kk = 1, ncn, 2
          kkk = mod (kk + 2, ncn)
          IF (kkk.eq.0) kkk = 3
          n1 = iabs (nop (jj, kk) )
          n2 = iabs (nop (jj, kkk) )
          IF (na.eq.n1.and.nc.eq.n2) goto 115
          IF (nc.eq.n1.and.na.eq.n2) goto 115
          GOTO 117

          115  CONTINUE
          ! ein passendes Element (jj) wurde gefunden:
          ! der Mittseitenknoten wird in die linie eingefuegt:
          line (j, l + 1) = iabs (nop (jj, kk + 1) )
          !NiS,mar06: roughness assignments to continuity lines is deactivated:
          ! die Rauhigkeitsklasse des Elementes wird notiert:
          !lineimat (j, l + 1) = ABS( imat (jj) )
          !-

          CYCLE nodes

        117 END DO

      END DO elements

    END DO nodes

    !checking continuity line assignments
    DO l = 1, nn
      !ERROR - zero entry in continuity line
      IF (line (j, l) == 0) call ErrorMessageAndStop (1401, j, cord (line (j, 1), 1), cord (line (j, 1), 2))

    END DO
                                                                        
  END DO lines

  RETURN


! ab dem 2. Aufruf dieser Subroutine wird der vorangegangene Teil       
! der Subroutine uebergangen                                            
END if first_call


!NiS,mar06: unit name changed; changed iout to Lout
WRITE (Lout, 6030)
write (*,6030)
6030 FORMAT( // 10x, 'Durchfluss-Berechnung',  &
           & // 10x, 'Liniennummer  Durchfluss [m^3/s]  Prozent von Linie 1' )

nd = nb

DO 180 j = 1, ncl

  ! ntl=1
  sumx = 0.0
  sumy = 0.0
  IF (lmt (j) .eq.1) then
    ! Linie hat nur einen Knoten:
    na = line (j, 1)
    sumx = sqrt (vel (1, na) **2 + vel (2, na) **2) * vel (3, na) &
            & * (2. * width (na) + (ss1 (na) + ss2 (na) ) * vel (3, na) ) / 2.
  ELSE
    ! Linie mit mehreren Knoten
    max = lmt (j) - 2

    DO 159 k = 1, max, 2
      na = line (j, k)
      nbb = line (j, k + 1)
      IF (ndry (nbb) .ne.1) goto 159
      IF (nbb.le.0) goto 159
      nc = line (j, k + 2)
      dx = (cord (nc, 1) - cord (na, 1) ) / 6.
      dy = (cord (nc, 2) - cord (na, 2) ) / 6.
      d1 = vel (3, na)
      d3 = vel (3, nc)
      d2 = (d1 + d3) / 2.
      ! Simpsonformel integriert Parabeln exakt
      sumx = sumx + dy * (vel (1, na) * d1 + 4.0 * vel (1, nbb)   &
       & * d2 + vel (1, nc) * d3)
      sumy = sumy + dx * (vel (2, na) * d1 + 4.0 * vel (2, nbb)   &
       & * d2 + vel (2, nc) * d3)
      ! Bestimmung der Durchfluszkomponenten bei linearer Verteilung
      ! der Fliesztiefe, geradem Elementrand und mittigem
      ! Mittseitenknoten o.k.
    159 END DO

  ENDIF

  ! korrekte Summation der Durchflusskomponenten auch fuer
  ! nicht gerade (polygon) Raender:
  total = sumx - sumy
  IF (j.eq.1) ref = total
  IF (abs (ref) .lt.0.0001) ref = 1.
  pct = 100.0 * total / ref
  ! mx = lmt(j)
  !NiS,mar06: unit name changed; changed iout to Lout
  WRITE (Lout, 6035) j, total, pct
  WRITE (*, 6035) j, total, pct

180 END DO

nb = nd

6035 FORMAT( 13x, i4, 3x, e15.4, 7x, f10.2 )

!NiS,apr06: changed name of subroutine because of global conflict
END SUBROUTINE check_Kalypso




!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                                                                       
      SUBROUTINE reord_Kalyps (knot, elem, qlist)
!
!     Der reordering-Algorithmus von rma1                               
!
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
USE BLK10MOD
USE BLK2
USE BLKASTEPH
USE BLK10MOD

parameter (maxcn = 60)

!NiS,mar06: variable names changed; changed mel to MaxE and mnd to MaxP
!array kntimel must be allocatable, because the size is vacant while calling the reordering sequence
INTEGER, ALLOCATABLE ::  kntimel (:,:)
integer :: qlist (2, 160), knot, elem
INTEGER :: icon (MaxP, maxcn)
!maxsize shows the maximum size of the kntimel allocation, so the largest element
INTEGER :: maxsize, JunctionSize

!nis,dec06: Allocating the kntimel-array for storage of connected nodes to one element. The background is the new number nodes, that might be
!           connected to a Transition line
!test for all possible line transitions
if (MaxLT /= 0) then
  !get the maximum element size. The maximum size is obtained by the longest 1D 2D transitioning number + 2 for the number of nodes of the
  !transitioning 1D part
  maxsize = 0
  !the longest continuity line is taken into account
  do i = 1, 50
    if (lmt(i) + 2 > maxsize) maxsize = lmt(i) + 2
  end do
else
  !if there is no transitioning, the allocation runs in normal way; the number should be decreasable from 8 to 4.
  maxsize = 8
endif

!allocate it, the size must be two slots bigger, because the two cornder nodes of the connecting 1D-element must be included
ALLOCATE (kntimel (MaxE, MaxSize))
!nis,jun07: Initializing the kntimel-array
do i = 1, MaxE
  do j = 1, maxsize
    kntimel (i, j) = 0
  end do
enddo
!-


!nis,may07: Information output
WRITE(*,*) 'Start of Reordering sequence with ', Maxlt, ' couplings'
!-

!NiS,mar06: Allocating the arrays of module blkasteph
ALLOCATE (mlist(500))
ALLOCATE (msn(MaxP))
ALLOCATE (icol(100,5000))
ALLOCATE (nadm(5000))
ALLOCATE (inum(MaxP))
!-
!NiS,mar06:     Allocating the arrays of module blk2
ALLOCATE (wd(MaxP))
ALLOCATE (iel(MaxP))
ALLOCATE (list(MaxP))
ALLOCATE (ihold(5000))
ALLOCATE (paxis(MaxE))
!-

ne = elem
np = knot

!nis,jun07: Initializing the allocated variables
do i = 1, maxp
  list(i) = 0
  msn(i) = 0
end do
do i = 1, 5000
  ihold(i) = 0
end do
!-

!get the node numbers into temporary memory
StoreConnectivity: DO i = 1, ne

  IF (nop (i, 1) == 0) CYCLE StoreConnectivity

  IF (imat (i) < 901 .or. imat(i) > 903) THEN

    !node number (corner nodes) is 2 for 1D elements
    k = 2
    !if it is a triangular element increase number of corner nodes to 3
    IF (nop (i, 5) > 0) k = 3
    !if it is a quadrilateral element increase number of corner nodes to 4
    IF (nop (i, 7) > 0) k = 4
    !save the node numbers
    DO j = 1, k
      l = 2 * j - 1
      kntimel (i, j) = nop (i, l)
    ENDDO

    !TODO: This is not efficient, there must be another way to find out membership in 1D-2D-Transition line
    !if there is a 1D 2D transition line, then save the nodes of the line in the element array kntimel
    if (maxlt > 0 .and. k == 2) then
      !first the correct line has to be found, by running through all possible lines
      FindLine: do j = 1, MaxLT
        !stop, if it is the correct line
        IF (TransLines (j, 1) == i) then
          !run through line defintion and transfer nodes to kntimel
          do l = 1, lmt( TransLines (j,2))
            !copy line node into kntimel. If it is the connecting node, jump over, because it is only allowed to occur once
            IF (line (TransLines (j,2), l) /= nop (i, 3)) kntimel (i, l+2) = line( TransLines (j,2), l)
          ENDDO
          EXIT FindLine
        ENDIF
      ENDDO FindLine
    ENDIF

  ELSE
    JunctionSize = 1
    HowManyNodes: DO
      IF (nop (i, JunctionSize) == 0) EXIT HowManyNodes
      JunctionSize = JunctionSize + 1
    ENDDO HowManyNodes
    DO j = 1, JunctionSize
      kntimel (i, j) = nop(i, j)
    ENDDO
  ENDIF

ENDDO StoreConnectivity

!maximum number of nodal connections is reduced to 60 (see definition of maxcn=60 above)
maxc = maxcn

!The size is in general 4, but if there was a line transition in the network, this number increases to maxsize
if (MaxLT /= 0) then
  ncn = maxsize
else
  ncn  = 4
end if

!tm Thomas Maurer, 15.05,1998
!tm in folgender Zeile befindet sich eine Variable idx,                 
!tm die nie initialisiert wurde und sonst im gesamten Programm          
!tm nicht auftaucht.                                                    
!tm -> was sollen wir tun, noch nichts geaendert                        
!tm (weiter unten gibt es die Variable idxx (noch ein "x"))             
!SR                                                                     
!SR Da nprt nicht genutzt wird, ist auch idx hinfaellig: deaktiviert 09.
!SR      nprt = idx-1                                                   
!SR                                                                     

!Initialize array which shows nodes connected to others
DO n = 1, np
  DO m = 1, maxc
    icon (n, m) = 0
  enddo
enddo

!This loop fills in all nodes, that have direct connection to others into icon array.
ConnectsOuter: DO n = 1, ne

  DO m = 1, ncn
    !Get the reference node
    i = kntimel (n, m)

    !if node is zero go to next element
    IF (i == 0) CYCLE ConnectsOuter
    !run through all nodes of the actual element
    ConnectsInner: DO k = 1, ncn

      !get a node of the element's definition list
      l = kntimel (n, k)

      !cycle process if the the second node choice is the same as the first one. They don't need to be connected
      IF (l == i) CYCLE ConnectsInner

      !Check, whether node is neither zero nor the same as already inserted in the connection matrix (icon); if not fill in the actual node
      !number connection
      GetConnection: DO j = 1, maxc
        IF (icon (i, j) == 0) exit GetConnection
        IF (icon (i, j) == l) CYCLE ConnectsInner

        !ERROR
        if (j == maxc) call ErrorMessageAndStop (1501, i, cord (l, 1), cord (l, 2))

      END DO GetConnection
      icon (i, j) = l
    END DO ConnectsInner
  END DO

END DO ConnectsOuter

nepem = 0
DO n = 1, np
  IF (icon (n, 1) > 0) nepem = nepem + 1
END DO
mpq = 0
list (1) = 1
mp = 1
allnodes: DO n = 1, np
  IF (icon (n, 1) == 0) CYCLE allnodes
  is = 0
  DO m = 1, mp
    IF (n == list (m) ) is = 1
    IF (is == 1) list (m) = list (m + 1)
  END DO

  IF (is == 1) mp = mp - 1

  allconnects: DO j = 1, maxc
    i = icon (n, j)
    IF (i == 0) EXIT allconnects
    IF (i <= n) CYCLE allconnects

    DO m = 1, mp
      IF (i == list (m) ) CYCLE allconnects
    ENDDO     

    !only if every node is not in list(m)
    !NiS,mar06: name of variable changed; changed mnd to MaxP
    IF (mp == MaxP) mp = mp - 1
    mp = mp + 1
    list (mp) = i

    !NiS,mar06: unit name changed; changed iout to Lout
    !    if( mp .eq. mnd ) write(Lout,80)
    !80 FORMAT( / 10x, 'over 1000 terms in list' )

  END DO allconnects

  !NiS,mar06: unit name changed; changed iout to Lout
  !      if(nprt.eq.3) write(Lout,'(25i4)') (list(m),m=1,mp)
  !      mpq=mpq+mp*mp
  mpq = mpq + mp
ENDDO allnodes


!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,99) mpq
!   99 FORMAT(1h1//10x,'for initial order, reordering sum =',i10)
mpp = mpq
                                                                        
! korrektur von mpp                                                     
                                                                        
orderloop: DO

  DO i = 1, np
    DO j = 1, maxc
      icon (i, j) = iabs (icon (i, j) )
    ENDDO
  ENDDO

  CALL order (idxx, kntimel, maxsize, qlist, icon)

  DO k = 1, 160
    qlist (1, k) = qlist (2, k)
  END DO

  IF (idxx >= 99999) EXIT orderloop

ENDDO orderloop

!ERROR - Reordering could not be fullfilled
IF (mpp.le.mpq) then
  call ErrorMessageAndStop (3501, 0, 0.0d0, 0.0d0)
ENDIF

DO n = 1, nepem
  iel (n) = msn (n)
END DO
!-                                                                      
!......zero arrarys                                                     
!-                                                                      
      DO n = 1, ne
        nfixh (n) = 0 
        list (n) = 0
      enddo

      DO n = 1, np 
        DO m = 1, maxc
          icon (n, m) = 0
        ENDDO
      ENDDO
!-                                                                      
!......form nodes connected to elements array                           
!-                                                                      
      Nodes2ConnElts: DO n = 1, ne
        N2CEltsOuter: DO m = 1, ncn

          i = kntimel (n, m) 

          IF (i == 0) cycle Nodes2ConnElts

          N2CEltsInner: DO j = 1, maxc

            IF (icon (i, j) .ne.0) cycle N2CEltsInner
            icon (i, j) = n 
            cycle N2CEltsOuter

          END DO N2CEltsInner

        END DO N2CEltsOuter
      END DO Nodes2ConnElts

!-
!......form list of elements to be formed                               
!-                                                                      
      k = 0 
      FormList: DO n = 1, nepem
        i = iel (n) 
        IF (i.eq.0) EXIT FormList
        FormListInner: DO j = 1, maxc
          m = icon (i, j) 
          IF (m.eq.0) CYCLE FormList
          IF (list (m) .gt.0) cycle FormListInner
          k = k + 1 
          nfixh (k) = m 
          list (m) = k 
        END DO FormListInner
      END DO FormList

      AssignNfixh: DO n = 1, ne
        IF (list (n) /= 0) cycle AssignNfixh
        k = k + 1 
        nfixh (k) = n 
      END DO AssignNfixh

!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,98) (nfixh(k),k=1,ne)
!   98 FORMAT(//10x,'selected element order is listed below'//(5x,10i10))

!NiS,mar06: Deallocating the arrays of module blkasteph
      DEALLOCATE (mlist)
      DEALLOCATE (msn)
      DEALLOCATE (icol)
      DEALLOCATE (nadm)
      DEALLOCATE (inum)
!-
!NiS,mar06: Deallocating the arrays of module blk2
      DEALLOCATE (wd)
      DEALLOCATE (iel)
      DEALLOCATE (list)
      DEALLOCATE (ihold)
      DEALLOCATE (paxis)
!-

      RETURN
      END SUBROUTINE reord_Kalyps


                                                                        
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
SUBROUTINE order (n, kntimel, maxsize, qlist, icon)


USE BLK10MOD
USE BLK2
USE BLKASTEPH

parameter (maxcn = 60)

INTEGER :: ia
INTEGER :: maxsize
INTEGER :: counter

INTEGER :: kntimel (MaxE, maxsize), qlist (2, 160)
INTEGER :: icon (MaxP, maxcn)
DIMENSION nlist (160)


!initializing
ia = 0

counter = 0
maxc  = maxcn
mpo   = mpq
mpq   = 0
isum  = 0

!node list to start reordering sequence from
DO j = 1, 160
  nlist (j) = qlist (1, j)
END DO

nelt = 2
n = nlist (1)

!If starting node number higher then maximum node number, stop ordering and go back to reordering
IF (n > MaxP) return

!INFORMATION: unit name changed; changed iout to Lout
WRITE (Lout, 6002) n
WRITE (*,    6002) n
6002 FORMAT (/1x,'Starting Node:      ', I6)

nprt = 1
!NiS,mar06: unit name changed; changed iout to Lout
!      if(nprt.gt.0) write(Lout,6040)
! 6040 format(//10x,'intermediate steps printed below'/                 
!     *       10x,'node, bandwidth, frontwidth, and connections')       
 6040 FORMAT(//10x,'Zwischenschritt siehe unten:'/  &
            &  10x,'Knoten, Bandbreite, Frontbreite, und Verknuepfungen')

nel = 1
iel (1) = n
mp = 0
nod = 1

!recalculate icon array to set those references neqative, that are in a later position related to active node
allConnects: DO i = 1, maxc
  jj = icon (n, i)
  IF (jj == 0) CYCLE allConnects
  DO m = 1, maxc
    !if active, then already processed, so set following reference negative
    IF (icon (jj, m) == n) then
      icon (jj, m) = - icon (jj, m)
      CYCLE allConnects
    end if
  END DO
END DO allConnects

!set values for each new point eliminated
MainLoop: do
  counter = counter + 1
  IF (nel /= 1 .or. counter /= 1) then

    !initialization of icol
    DO i = 1, 41
      DO j = 1, 40
        icol (i, j) = 0
      ENDDO
    ENDDO

    nod = nod + 1
    !TODO: mist and nr are always of the same value; therefore nadm(nr) can be used as a single value, because only nadm(1) is used
    mist = 0
    nr = 1
    nadm (nr) = 100

    DO m = 1, mp
      ii = list (m)
      CALL adjpt (ii, m, kntimel, maxsize, icon)
    END DO

    m = icol (1, 1)
    n = list (m)

    IF (ihold (1) >= 249) THEN
      m = 1
      n = list (1)
    ENDIF

    ia = ihold (m)
    nel = nel + 1

    IF (nlist (nelt) /= 0) THEN
      nelt = nelt + 1
      n = nlist (nel)
      allMPs: DO mm = 1, mp
        m = mm
        IF (list (m) == n) EXIT allMPs

        IF (mm == mp) THEN
          mp = mp + 1
          list (mp) = n
          m = mp
        ENDIF

      END DO allMPs
    endif

    iel (nel) = n

    DO i = 1, m
      ihold (i) = ihold (i) + 1
    ENDDO

    mp = mp - 1

    IF (m <= mp) THEN

      DO i = m, mp
        ihold (i) = ihold (i + 1) + 1
        list (i) = list (i + 1)
      ENDDO
    ENDIF
  ENDIF

  !add to column adjacent point of eliminated point
  addColumn: DO j = 1, maxc
    ii = icon (n, j)
    IF (ii <= 0) CYCLE addColumn
    mp = mp + 1
    ihold (mp) = 1
    list (mp) = ii
  END DO addColumn

  mpq = mp + mpq
  isum = isum + ia

!NiS,mar06: unit name changed; changed iout to Lout
!if(nprt.gt.0) write(Lout,'(3i5)') n,ia,mp
!if(nprt.gt.1) write(Lout,'(20x,25i4)')  (list(j),j=1,mp)

  iconAssignouter: DO i = 1, maxc
    jj = icon (n, i)

    IF (jj <= 0) CYCLE iconAssignouter

    iconAssignInner: DO m = 1, maxc
      k = iabs (icon (jj, m) )

      IF (k == 0) CYCLE iconAssignInner

      DO mm = 1, maxc
        IF (icon (k, mm) == jj) then
          icon (k, mm) = - icon (k, mm)
          cycle iconAssignInner
        ENDIF
      END DO

    END DO iconAssignInner

  END DO iconAssignouter

  IF (nod >= nepem) EXIT MainLoop
ENDDO MainLoop

!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,6050) mpq,isum
! 6050 format(//10x,'reordering sum =',i10,'  band sum =',i8// )        
! 6050 format(//10x,'reordering Summe =',i10,'  Band Summe =',i8// )    
IF (mpq.ge.mpo) mpq = mpo
IF (mpq.eq.mpo) return
DO n = 1, nepem
  msn (n) = iel (n)
END DO
RETURN
END SUBROUTINE order
                                                                        
                                                                        
                                                                        
!-----------------------------------------------------------------------
      SUBROUTINE adjpt (ii, m, kntimel, maxsize, icon)
!                                                                       
!      ii == actual node number of open nodes in heading list
!       m == running number of the actual node in heading list
! kntimel == connection array (like nop-array)
! maxsize == maximum connections of one node inside kntimel (depends on Transition line)
!    icon == connectivity array: stores every node to node connection
!
! purpose of subroutine:
!
!
!
!-----------------------------------------------------------------------
!-
USE BLK10MOD
USE BLK2
USE BLKASTEPH

parameter (maxcn = 60)

INTEGER :: maxsize
INTEGER :: kntimel (MaxE, maxsize)
INTEGER :: icon (MaxP, maxcn)

maxc = maxcn
nad  = 0
                                                                        
!through all possible connections
countloop: DO i = 1, maxc
  !get connected node number
  jj = icon (ii, i)
  !jump over negative node references (they are deactivated!)
  IF (jj <= 0) cycle countloop

  !?What is mist?
  IF (jj == mist) cycle countloop

  !increase number of adject nodes
  nad = nad+1
END DO countloop
                                                                        
IF (nad - nadm (nr) < 0) then
  nadm (nr) = nad
  icol (nr, 1) = m
  nc = 1
ELSEIF (nad - nadm(nr) == 0) then
  nc = nc + 1
  icol (nr, nc) = m
endif

RETURN
END SUBROUTINE adjpt
                                                                        
                                                                        
                                                                        

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
      USE BLK10MOD    	!cord(i,1), cord(i,2), ao(i), tett, MaxE, MaxP, irk, rk_zeile
      USE BLKDRMOD
      USE ParaKalyps    !new module for KALYPSO-specific globals and neighbourhood relations
      USE BLKSUBMOD     !for weir structures
      USE Para1DPoly  !Modul für Teschke-1D-Elemente
!-

!NiS,may06: Former variable declaration is sorted and the variables are described; new entered variables are EXPLICITLY pointed out

REAL                   :: x, y, z                      !temporary coordinate-spaces for generation of new midside-nodes
REAL                   :: vekkant(2),vekpu1(2),kreuz   !variables for checking-sequence of mesh
REAL                   :: wsp                          !???
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
INTEGER                :: elem (MaxE, 6)               !local array for element informations ([number of arcs +1]; node-numbers of corners); dependent
                                              	       !on the type and shape of element; the first entry can also show 1D-ELEMENT with negative value
INTEGER                :: elkno(5), mikno(4)           !local array for loop, that generates midsides for elements where no midside is assigned
INTEGER                :: elfix (MaxE)                 !for reordering subroutine of RMA1, not used in RMA10S
INTEGER                :: istat                        !IOSTAT-Value that becomes non-zero while input-reading-errors

!nis,dec06: Local supporting variable for reading Continuity lines
INTEGER                :: DefLine
!-

!NiS,mar06: Comments added and array sizes changed to:
!INTEGER :: i, j, k, l, m, n, elzaehl, mittzaehl, arc (mnd, 5),&
!	  & elem (mel, 6), arccnt, elcnt, elkno (5), mikno (4), elfix (mel)
!INTEGER :: istat

!In order of global conflicts id is renamed to id_local
!CHARACTER(LEN=2)       :: id
CHARACTER (LEN=2)      :: id_local                     !line data identifier for reading input/restart-file
CHARACTER (LEN=2400)    :: linie                        !temporary input space for reading input/restart-file
CHARACTER (LEN=20)     :: inquiretest
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
INTEGER 	              :: midsidenode_max              !comparison variable for the highest necessary node number, so it can be tested, whether
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



!SR:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!SR Zusaetzliche Schalter-Variablen fuer die Nachvermaschung des CVFEM-A
!SR Diese Schalter wurden in den Lesevorgang implementiert:             
                                                                        
!WP bei traditionell GFEM nicht noetig, aber erst mal nicht auskommentiert
COMMON / vnach / cvva, cvga, cvvo, cvgo, cvzu, cvfe, cvarccnt
!SR:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!tm Thomas Maurer, 15.05,1998
!tm linie von *80 auf *120 verlaengert                                  
!tm Anpassung an "verlaengerten" *.2D file, d.h.                        
!tm VA-Zeile hat noch eine neue Position auf von Spalte 86 bis 96 bekomm
!tm VA         1           0.9858658           0.0481516           2.217


! INITIALISIERUNGSBLOCK -----------------------------------------------------------------------

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
!-

!WP Auch Initialisierung bei traditionell GFEM nicht noetig, aber
!   erst mal dringelassen
cvva = 0
cvga = 0
cvvo = 0
cvgo = 0
cvzu = 0
cvfe = 0
!WP Ende

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

      !NiS,mar06: Find out maximum ARC number
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
      !NiS,mar06: Read ARC geometry informations
      ELSE
        READ (linie,'(a2,6i10)') id_local, i, (arc(i,j), j=1, 5)
        IF (i > arccnt) arccnt = i

        !ERROR - negative arc number
        IF (i <= 0) call ErrorMessageAndStop (1301, i, 0.0d0, 0.0d0)

      ENDIF
    ENDIF

    !nis,jan08: Interpolation information for elements
    IF (linie(1:2) == 'IP') THEN
      IF (kswit == 1) THEN
        READ (linie, '(a2, i10, i10)') id_local, i, j
        NodesToIntPol = NodesToIntPol + j
        if (j > MaxIntPolElts) MaxIntPolElts = j
      ELSEIF (kswit == 0) then
        READ (linie, '(a2, i10, i10)') id_local, i, IntPolNo(i)
      ENDIF
    ENDIF

    !EFa Nov06, Gültigeitsgrenzen der Polynome
    IF (linie(1:2) .eq. 'MM') THEN
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
      else
        read (linie, '(a2,i10,2f20.7)') id_local, i, hhmin(i), hhmax(i)
!        PolyRangeQ (i, 1) = hhmax (i)
!        PolyRangeA (i, 1) = hhmax (i)
      endif
    end if

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
        read (linie, *, iostat = linestat) id_local, i, polySplitsA(i), hhmin(i), (polyrangeA(i, k), k=1, polySplitsA(i))
        hhmax(i) = polyrangeA (i, polySplitsA(i))
      endif
    end if
    if (linie(1:3) .eq. 'PRQ') then
      IF (KSWIT == 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j
        if (PolySplitCountQ < j) PolySplitCountQ = j
      else
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, polySplitsQ(i), hhmin(i), (polyrangeQ(i, k), k=1, polySplitsQ(i))
        hhmax(i) = polyrangeQ (i, polySplitsQ(i))
      endif
    end if
    if (linie(1:3) .eq. 'PRB') then
      IF (KSWIT == 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j
        if (PolySplitCountB < j) PolySplitCountB = j
      else
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, polySplitsB(i), hhmin(i), (polyrangeB(i, k), k=1, polySplitsB(i))
        hhmax(i) = polyrangeB (i, polySplitsB(i))
        hbordv (i) = polyrangeB (i, 1)
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
      endif
    end if
    if (linie(1:3) .eq. 'ALP') then
      IF (KSWIT /= 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j, (alphapoly(j, i, k), k = 0, 12)
      endif
    end if
    if (linie(1:3) .eq. 'BEP') then
      IF (KSWIT /= 1) then
        linestat = 0
        read (linie, *, iostat = linestat) id_local, i, j, (betapoly(j, i, 0), k = 0, 12)
      endif
    end if

    !nis,nov07: old way of defining area-polynomial
    !EFa Nov06, Flächenpolynom
    if (linie (1:3) .eq. 'AP1') then

      IF (kswit == 1) THEN
        if (PolysplitCountA == 0) PolySplitCountA = 1
      ELSE
        read (linie, '(a3,i9,5f20.7)') id_local, i,(apoly(1, i, j), j = 0, 4)
        polySplitsA (i) = 1
      endif
    end if
    if (linie (1:3) .eq. 'AP2') then
      IF (KSWIT /= 1) then
        read (linie, '(a3,i9,5f20.7)') id_local, i,(apoly(1, i, j), j = 5, 9)
      endif
    end if
    if (linie (1:3) .eq. 'AP3') then
      IF (KSWIT /= 1) then
        read (linie, '(a3,i9,3f20.7)') id_local, i, (apoly(1, i, j), j = 10, 12)
      endif
    end if

    !nis,nov07: old way of defining discharge-polynomial
    !EFa Nov06, Q(h)-Kurve
    if (linie (1:3) .eq. 'QP1') then
      if (kswit == 1) then
        if (PolysplitCountQ == 0) PolySplitCountQ = 1
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i, qgef(i),(qpoly(1, i, j), j = 0, 3)
        polySplitsQ (i) = 1
      endif
    end if
    if (linie (1:3) .eq. 'QP2') then
      IF (KSWIT /= 1) then
        read (linie, '(a3,i9,5f20.7)') id_local, i, (qpoly(1, i, j), j = 4, 8)
      endif
    end if
    if (linie (1:3) .eq. 'QP3') then
      IF (KSWIT /= 1) then
        read (linie, '(a3,i9,4f20.7)') id_local, i, (qpoly(1, i, j), j = 9, 12)
      endif
    end if


    !EFa Nov06, h-Bordvoll
    if (linie(1:2) .eq. 'HB') then
      IF (KSWIT == 1) then
        read (linie,'(a2,i10)') id_local,i
      else
        read (linie,'(a2,i10,f20.7)') id_local, i, hbordv (i)
        PolyRangeB(i, 1) = hbordv (i)
        !write (*,*) 'HB', i, hbordv(i)
      endif
    end if

    !EFa Nov06, alpha-Übergang bis h
    if (linie (1:2) .eq. 'AD') then
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
        if (PolysplitCountB == 0) PolySplitCountB = 3
      else
        read (linie, '(a2,i10,5f20.7)') id_local, i, PolyRangeB(i, 2), (alphapoly (2, i, j), j = 0, 3)
        !fill alphapoly (1,:,:)
        if (PolysplitCountB == 0) PolySplitCountB = 1
        alphapoly (1, i, 0) = 1.0
        polySplitsB (i) = 3
      endif
    end if

    !EFa Nov06, alpha-Beiwertpolynomkoeffizienten
    if (linie (1:3) .eq. 'AK1') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i, (alphapoly (3, i, j), j = 0, 4)
        !write (*,*) 'AK1',i,(alphapk(i,j), j=1,5)
      endif
    end if

    if (linie (1:3) .eq. 'AK2') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i, (alphapoly (3, i, j), j = 5, 9)
        !write (*,*) 'AK2',i,(alphapk(i,j), j=6,10)
      endif
    end if

    if (linie (1:3) .eq. 'AK3') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,3f20.7)') id_local, i, (alphapoly (3, i, j), j = 10, 12)
        !write (*,*) 'AK3',i,(alphapk(i,j), j=11,13)
      endif
    end if

    !EFa Nov06, beta-Übergang bis h
    if (linie (1:2) .eq. 'BD') then
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
        if (PolysplitCountB == 0) PolySplitCountB = 3
      else
        read (linie, '(a2,i10,5f20.7)') id_local, i, PolyRangeB(i, 2), (betapoly (2, i, j), j = 0, 3)
        !fill alphapoly (1,:,:)
        betapoly (1, i, 0) = 1.0
        polySplitsB (i) = 3
      endif
    end if

    !EFa Nov06, beta-Beiwertpolynomkoeffizienten
    if (linie (1:3) .eq. 'BK1') then
       IF (KSWIT == 1) then
         read (linie, '(a3,i9)') id_local, i
       else
         read (linie, '(a3,i9,5f20.7)') id_local, i, (betapoly (3, i, j), j = 0, 4)
         !write (*,*) 'BK1',i,(betapk(i,j), j=1,5)
       endif
    end if

    if (linie (1:3) .eq. 'BK2') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i, (betapoly (3, i, j), j = 5, 9)
        !write (*,*) 'BK2',i,(betapk(i,j), j=6,10)
      endif
    end if

    if (linie (1:3) .eq. 'BK3') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,3f20.7)') id_local, i, (betapoly (3, i, j), j = 10, 12)
        !write (*,*) 'BK3',i,(betapk(i,j), j=11,13)
      endif
    end if

    !ELEMENT DEFINITIONS ---
    IF (linie (1:2) .eq.'FE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local, i
        IF (i > elcnt) elcnt = i
      ELSE
        cvfe = 1
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,4i10)') id_local, i, imat (i), imato (i), nfixh (i)

!LF nov06: read again the FE line for the starting node of a weir element
        if (imat(i) .gt. 903 .and. imat(i) .lt. 990) then
          weircnt = weircnt + 1
          read (linie, '(a2,5i10)') id_local, i, imat(i), imato(i), nfixh(i), reweir(weircnt,1)

          !ERROR - no starting node for weir element definition was found
          if (reweir (weircnt, 1) <= 0) call ErrorMessageAndStop (1003, i, 0.0d0, 0.0d0)

          reweir (weircnt, 2) = i
        end if
!-
        IF (i > elcnt) elcnt = i

        IF (i <= 0) call ErrorMessageAndStop (1004, i, 0.0d0, 0.0d0)
      ENDIF
    ENDIF

    !1D JUNCTION ELEMENT DEFINITIONS ---
    IF (linie (1:2) .eq.'JE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local,i
        IF (i.gt.elcnt) elcnt = i
      ELSE
        cvfe = 1

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

    !ROUGHNESS CLASS INFORMATIONS ---
    IF (linie (1:2) .eq.'RK') THEN
      !not interesting for dimension reading
      if (KSWIT == 1) CYCLE reading
      irk = irk + 1
      rk_zeile (irk) = linie (1:)
    ENDIF

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
        READ (linie, '(a2,6i10)') id_local, i, (nop(i,k),k=1,5)
        !Increase number of 1D-2D-TRANSITION ELEMENTS
        MaxT = MaxT + 1
      END IF
    END IF
    !-

    !NiS,nov06: Transition line elements between 1D- and 2D-networks with an element to line connection
    !           TransLines(i,1) : transitioning element
    !           TransLines(i,2) : transitioning line
    !           TransLines(i,3) : node, which is connected on the 1D side of the transition element
    if (linie(1:2).eq.'TL') then
      IF (KSWIT.eq.1) THEN
        READ (linie,'(a2,i10)') id_local, i
        WRITE(*,*) 'getting here, TLcnt definition'
        IF (i.gt.TLcnt) TLcnt = i
      ELSE
        WRITE(*,*) MaxLT
        READ (linie, '(a2,4i10)') id_local, i, (TransLines(i,k),k=1,3)
        WRITE(*,*) id_local, i, Translines(i,1), TransLines(i,3)
        WRITE(*,*) linie
!        MaxLT = MaxLT + 1
      END IF
    end if
    !-

    !nis,dec06: 'Continuity lines' can also be defined in the network file. The purpose is to make use of the already implemented structure
    !           of CCLs for defining line geometry at transitions. For reordering purposes it is necessary to be able to read in Continuity
    !           lines at the beginning together with the network.
    if (linie(1:2).eq.'CC') then

        read (linie,'(a2,i1,i5)') id_local,DefLine,i
      if (kswit.eq.1) then
        if (DefLine.eq.1) WRITE(*,*) 'Continuity Line',i, 'defined in network file.'
      else
        if (DefLine.eq.1) then
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

    !NiS,apr06:
    !DATE INFORMATIONS IN RMA10S FORMAT ---
    IF (linie (1:2) == 'DA') THEN
      READ (linie, '(a2,i10,f20.7)') id_local, iyrr, tett
    ENDIF
    !-

    !INITIAL VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP ---
    IF (linie (1:2) .eq.'VA') then
      !NiS,apr06: variables deactivated for RMA10S
      !wsp = 0.0
      !cvva = 1
      !-
      READ(linie,'(a2,i10,4f20.7)') id_local, i, (vel(j,i), j=1, 3), rausv (3, i)
      !ERROR - restart values can't be applied to node out of zero-maxp-Range
      IF (i > MaxP .or. i <= 0) call ErrorMessageAndStop (1601, i, cord (i, 1), cord (i, 2))
    ENDIF
    !INITIAL VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP; ONLY FOR INTERPOLATED PROFILES/ NODES ---
    IF (linie (1:2) .eq.'IR') then
      !NiS,apr06: variables deactivated for RMA10S
      !wsp = 0.0
      !cvva = 1
      !-
      READ(linie,'(a2, i10, 4f20.7)') id_local, i, (vel(j,i), j=1, 3), rausv (3, i)
      !ERROR - restart values can't be applied to node out of zero-maxp-Range
      IF (i > MaxP .or. i <= 0) call ErrorMessageAndStop (1601, i, cord (i, 1), cord (i, 2))
      !ERROR - TRYING TO APPLY RESULT OF INTERPOLATED PROFILE/ NODE TO A REAL NODE
      if (.not. (IntPolProf (i))) call ErrorMessageAndStop (1602, i, cord (i, 1), cord (i, 2))
    ENDIF

!    !NiS,may06: these degrees of freedom are missing in Kalypso-2D, because they are not used there; adding for application in RMA10S
!    !INITIAL VALUES FOR DEGREES OF FREEDOM 4 TO 7 ---
!    IF (linie(1:2) .eq. 'DF') THEN
!      READ(linie,'(a2,i10,4f20.7)') id_local, i, (vel(j,i), j=4,7)
!    END IF
!    !-
!
!    !INITIAL GRADIENTS OF VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP ---
!    IF (linie (1:2) .eq.'GA') then
!      !NiS,apr06: variables deactivated for RMA10S
!      !cvga = 1
!      !-
!      READ (linie, '(a2,i10,3f20.7)') id_local, i, (vdot(j,i),j=1,3)
!      !NiS,mar06: name of variable changed; changed mnd to MaxP
!      !Stop program execution on nodenumber higher than MaxP; could normally not happen
!      IF (i.gt.MaxP) stop 'i.gt.MaxP'
!      !Stop program execution on negative NODE number
!      IF (i.le.0) stop 'Knotennummer.le.0'
!    ENDIF
!
!    !VALUES OF VELOCITIES AND WATER DEPTH OF OLD TIME STEP ---
!    IF (linie (1:2) .eq.'VO') then
!      !NiS,apr06: variables deactivated for RMA10S
!      !cvvo = 1
!      READ (linie, '(a2,i10,3f20.7)') id, i, (vold (j, i) , j=1,3)             !vold muss NICHT gelesen werden
!      !NiS,mar06: name of variable changed; changed mnd to MaxP
!      !Stop program execution on nodenumber higher than MaxP; could normally not happen
!      IF (i.gt.MaxP) stop 'i.gt.MaxP'
!      !Stop program execution on negative NODE number
!      IF (i.le.0) stop 'Knotennummer.le.0'
!    ENDIF
!
!    !GRADIENTS OF VELOCITIES AND WATER DEPTH OF OLD TIME STEP ---
!    IF (linie (1:2) .eq.'GO') then
!      !NiS,apr06: variables deactivated for RMA10S
!      !cvvo = 1
!      READ (linie, '(a2,i10,3f20.7)') id, i, (vdoto (j, i) , j=1,3)            !vdoto muss NICHT gelesen werden
!      !NiS,mar06: name of variable changed; changed mnd to MaxP
!      !Stop program execution on nodenumber higher than MaxP; could normally not happen
!      IF (i.gt.MaxP) stop 'i.gt.MaxP'
!      !Stop program execution on negative NODE number
!      IF (i.le.0) stop 'Knotennummer.le.0'
!    ENDIF
!
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

! DEACTIVATED ID-LINES IN KALYPSO-2D GEOMETRY FILE ---------------------------------------------

    !TITEL INFORMATIONS ---
    !IF (linie (1:2) .eq.'TI') then
    !  READ (linie, '(a2,f20.7,i10)') id, tet, nitsv                            !tet muss NICHT gelesen werden
    !ENDIF

    !RESTART FILE INFORMATIONS ---
    !IF (linie (1:2) .eq.'RS') then
    !  READ (linie, '(a2,a80)') id, restart
    !ENDIF

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

!NiS,mar06: file is closed in the calling subroutine getgeo; it can't be closed here because it is used twice
!CLOSE (12)

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


!     write(iout,*)' goto 900: nodecnt=',nodecnt

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
        IF (nop(arc(i,3),4).eq.0) THEN
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
!-

!NiS,mar06: unit name changed; changed iout to Lout
WRITE (Lout, 110 )
WRITE ( *  , 110 )
110 FORMAT (//1X, '--------- Checking the mesh -------------')

! ASSIGNING NODES TO ELEMENTS WITHOUT SORTING ------------------------------

!NiS,may06: adaption of all_elem-DO-LOOP for 1D-ELEMENTS; some changes.
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

  !EFa Nov06, 1D-Teschke-Element
  ELSEIF (elem(i,1) .EQ. -3) then
    !EFa Nov06, for 1D_Teschke-elements, the number of nodes is 3 and the number of corner nodes is 2
    jnum = 2
    ncorn(i) = 3

    !EFa Nov06, Passing corner nodes to nop array
    nop (i, 1) = arc (elem(i, 2), 1)
    nop (i, 3) = arc (elem(i, 2), 2)
    !Mittseitenknoten
    nop (i, 2) = arc (elem(i, 2), 5)

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

!     Erzeugen der erforderlichen Mittseitenknoten
!initializing the counter variable for additional midside nodes.
mittzaehl = 0

!SR::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!SR Speichern der Kantenanzahl fuer Eckknotenspezifische Fehler-
!SR ausgabe
cvarccnt = arccnt
!SR::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                                                                        
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

!WP 03.04.03 Anfang
!
! Aenderungen von SR, Funktion der Abfrage nicht ganz klar.
! Erst ab CVFEM-Version vorhanden. Wird erstmal drin gelassen.
!
    IF (x.lt.0.000001.and.y.lt.0.000001.and.z.lt.0.000001) then
      !NiS,mar06: unit name changed; changed iout to Lout
      WRITE (Lout, 3702) i, ibot, itop, mittzaehl
 3702 FORMAT      (4i10)
    ENDIF
!
!WP 03.04.03 Ende
!
                                                                        
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
      IF (ilft.gt.0) THEN
        jnum = elem (ilft, 1) - 1
        DO 1401 j = 1, jnum
          IF (nop (ilft, 2 * j - 1) .eq.ibot) THEN
            nop (ilft, 2 * j) = nodecnt
            GOTO 1300
          ENDIF
1401   ENDDO
      ENDIF

1300  IF (irgt > 0) THEN
!NiS,mar06: changes see above
!        jnum = elem (irgt, 1)
        jnum = elem (irgt, 1) - 1

        DO j = 1, jnum
          IF (nop (irgt, 2 * j - 1) .eq.itop) THEN
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
  IF (arc(i,3) == arc(i,4) .and. arc(i,3) /= 0 .and. imat(arc(i,3)) < 900) THEN

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

    ENDIF
  ENDIF
ENDDO


call InterpolateProfs (statElSz, statNoSz, MaxP, MaxE, maxIntPolElts, IntPolNo, NeighProf, ncorn, nop, &
                    & cord, ao, kmx, kmWeight, IntPolProf, IntPolElts, qgef, imat)

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
!The other subrotines for 'NACHVERMASCHUNG' in the case of CVFEM and the REORDERING in Kalypso-2D are deactivated for RMA10S. They are re-
!named with the additions 'KalypsoSpecial', so that no collidation with original subroutins in RMA10S can occur.
!NiS,mar06
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!NiS,mar06: Why copy? sense not clear!
      n  = nodecnt
      m  = elcnt
      !np = nodecnt
      !ne = elcnt
      np = MaxP
      ne = MaxE

!NiS,mar06: unit name changed; changed iout to Lout
!     write(Lout,*)'  1402:  nodecnt=',nodecnt
                                                                        
! ----------------------------------------------------------------------
!     Kopie des Hoehenfeldes
!NIS,may06: variable name changed; changed mnd to MaxP
      DO i = 1, MaxP
        aour (i) = ao (i)
      END DO
                                                                        
! ----------------------------------------------------------------------------------------------------------------
!     Initialize check / ncl Linien zusammensetzen:
      !NiS,mar06: Checking of Continuity lines is not possible; the data is not available at this runtime; DEACTIVATED for RMA10S, it will be checked
      !           in RMA10S run
!      CALL check

!WP 04.04.03 Anfang ---------------------------------
!
!   Aufrufen von NACH bei trad. GFEM (FEM = 0) nicht nötig
!      IF (fem.ne.0) then
!SR -----------------------------------------------------
!SR jetzt die Nachvermaschung zum CVFEM-Netz durchfuehren
!NiS,mar06: unit name changed; changed iout to Lout
!      WRITE (Lout,104)
!      WRITE ( *  ,104)
!      104 format (1X, 'Nachvermaschung aufgerufen...')
!SR Die Nachvermaschung muss auch dann aufgerufen werden, wenn bereits
!SR nachvermascht wurde (Restart), denn es wird die Eckknotenanzahl
!SR weitergegeben, die benoetigt wird, um den CVFehler auszugeben.
!SR      if (nfixh(1)==0) then
!SR Deshalb keine Beschraenkung auf die reine Teichloesungccc
!        CALL nach
!SRTest      end if
!      ENDIF
!SR
!WP 04.04.03 Ende --------------------------------------
                                                                        
!---------------------------------------------------------------------------------------------------------------------------------------------
!nis,jan07: Work on Line transitions
!---------------------------------------------------------------------------------------------------------------------------------------------
!nis,jan07: Turning the transitioning elements, if necessary; while doing this: Testing, whether transition is properly defined
if (maxlt.ne.0) then
  !run through all possible transitionings
  elementturning: do i = 1, maxlt

    !if transition is dead, go to next one
    if (TransLines (i, 1) == 0) CYCLE elementturning

    !test for correct order of nodes in transitioning element
    if (nop(TransLines(i,1),3) .ne. TransLines(i,3)) then

      !nis,jan07: Checking, whether node is defined in element on the other slot (slot 1)
      if (nop (TransLines (i, 1), 1) /= TransLines (i, 3)) &
     &  call ErrorMessageAndStop (1403, i, cord (nop (TransLines (i, 1), 3), 1), cord (nop (TransLines (i, 1), 3), 1))

    end if
  end do elementturning
endif

! REORDERING -------------------------------------------------------------------
DO i = 1, elcnt
  elfix (i) = 0
END do

DO i = 1, elcnt
  IF (nfixh (i) .le.0) goto 4003
  !NiS,mar06: name of variable changed; changed mel to MaxE
  IF (nfixh (i) .gt.MaxE) goto 4003
  IF (imat (nfixh (i) ) .gt.0) elfix (nfixh (i) ) = elfix (nfixh (i) ) + 1
END DO


DO i = 1, elcnt
  IF ( (imat (i) .gt. 0) .and. (elfix (i) .ne. 1) ) goto 4003
END DO
!NiS,mar06: unit name changed; changed iout to Lout
write (Lout, 107)
write ( * ,  107)
107 format (1X, 'Order of processing (NFIXH) is sufficiently correct')
! i.e. all elements exist only once.

GOTO 4004
                                                                        
4003 CONTINUE
!NiS,mar06: unit name changed; changed iout to Lout
WRITE (Lout,105)
WRITE ( * , 105)
105 format (1X, 'Reordering has to be done.')

!NiS,may06: In RMA10S a subroutine called reord.subroutin exists; changed reord to reord_Kalyps
CALL start_node (qlist, k, MaxP)
CALL reord_Kalyps (MaxP, MaxE, qlist)


4004 CONTINUE

! CALCULATION OF CENTERS OF ELEMENTS  ----------------------------------------------------------------
! WP 13.04.2004
! Calculation of center coordinates of each element

do i=1,ne
  sumx  = 0.0
  sumy  = 0.0
  ! Schleife ueber alle Knoten des Elementes (auch Mittseitenknoten!)
  ! zur Bestimmung des Elementmittelpunktes und dem Mittelwert
  ! der drei Freiheitsgerade
  !NiS,mar06: unit name changed; changed iout to Lout
  !write (Lout,*) ' Anzahl Knoten (Element ',i,'): ', ncorn(i)
  !EFa Nov06, für die 1D-Teschke-Elemente werden für die Mittelung nur die Eckknoten betrachtet
  if (elem(i,1).EQ.-3) then
    do j =1, 2
      sumx = sumx + cord(nop(i,(j*2-1)),1)
      sumy = sumy + cord(nop(i,(j*2-1)),1)
    end do
    mcord(i,1) = sumx/2
    mcord(i,2) = sumy/2
  else
    do j = 1,ncorn(i)
      sumx = sumx + cord(nop(i,j),1)
      sumy = sumy + cord(nop(i,j),2)
    end do
    ! Mittelung der Werte
    mcord(i,1) = sumx/ncorn(i)
    mcord(i,2) = sumy/ncorn(i)
  end if
END do


! NEIGHBOURHOOD RELATIONS OF NODES -------------------------------------------------------------------
!

! Initialisieren der Nachbarschaftsbeziehungen zwischen den Knoten
do i=1,MaxP
  do j=1,30
    neighb(i,j) = 0 	! Nummer der j-ten benachbarten Knotens von Knoten i
  end do
  nconnect(i) = 0       ! Anzahl der Nachbarknoten
end do


! Jedes Element durchgehen
neighbours: do i=1,MaxE

  ! leere Elementnummern uebergehen:
  if (elem(i,1).ne.0) then

    !nis,jun07: Initializing ConnNumber
    ConnNumber = 0

    !nis,jan07: Get the transition number and the transitioning CCL
    findconnection: do j= 1, MaxLT
      if (TransLines(j,1) == i) then
        ConnNumber = j
        ConnLine = TransLines(j,2)
        EXIT findconnection
      end if
      if (j.eq.MaxLT) ConnNumber = 0
    end do findconnection
    !-

    !nis,dec06: For 1D-2D-transition-lines the connectivity of the nodes differs from the original 2D-connectivity,
    !           that can be also used for simple 1D-networks and simple 1D-2D-elementToelement-transitions
    !           For those line-transitions, the following two loops are inserted
    !

    if (ConnNumber /= 0) then
      !the slope is just connected to the CORNER nodes of the transition line, the midside nodes are not effecting!
      !number of nodes at that 1D-2D-transtion without the midsidenodes of the line
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
      !-

      !Adding shows the point, from which on the nodes of the connected line have to be stored in the nop_temp array
      adding = 3

      !overgiving the nodes into the temporary array nop_temp
      nodeassigning: do j = 1, lmt (ConnLine)
        nop_temp (j + adding) = line (ConnLine, j)
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


      !nis,feb07: Here is a conflict!!!

!nis,may07
!Add midside node for polynom approach; at the moment there neighbourhood relation between the nodes must not be calculated
    !nis,may07 Change if-question for numbered Polynom elements
    !EFa Nov06, gesonderte Berechnung für 1D-Teschke-Elemente
    !elseIF(nop(i,2).EQ.-9999) then
    !ELSEIF(nop(i,2) < -1000) then
    ELSEIF(imat(i) == 89) then
    !-
!Add midside node for polynom approach
!-
      node1 = nop(i,1)
      node2 = nop(i,3)
      nconnect(node1) = nconnect(node1)+1
      nconnect(node2) = nconnect(node2)+1
      neighb(node1,nconnect(node1)) = node2
      neighb(node2,nconnect(node2)) = node1
    ELSE
      ! Lesen aller Knotennummern eines Elementes
      outer: do j=1,ncorn(i)

!nis,dec06: for increasing speed of program, bring line to outside of loop
        node1 = nop(i,j)
!-

        inner: do l=1,ncorn(i)

!nis,dec06: for increasing speed of program, bring line to outside of loop
!          node1 = nop(i,j)
!-
          node2 = nop(i,l)

          IF (node1 .ne. node2) THEN
            nconnect(node1) = nconnect(node1) + 1
            neighb(node1,nconnect(node1)) = node2
          END if
        end do inner
      end do outer

    !nis,dec06: endif for the test of whether line transition or not
    ENDIF
    !-

  end if

end do neighbours

! Ausduennung des urspruenglich mehrfach bestimmten Feldes
! (Jedem Knoten wurde ein anderer Knoten mehrfach als Nachbar zugewiesen)

do i=1,nodecnt

  j=1
  ! Gehe jeden Eintrag durch
  do

    if (j >= nconnect(i)) exit

    k=j+1

    vergleich: do

      ! Falls Eintrag an j-ter und k-ter Stelle identisch
      ! dann lösche den j-ten Eintrag und verschiebe die
      ! anderen nach vorne
      if (neighb(i,j) == neighb(i,k)) then

        ! Verschiebe alle ab der Uebereinstimmung um eine Position nach vorne
        do l=k,nconnect(i)-1
          neighb(i,l) = neighb(i,l+1)
        end do

        ! Reduziere die Anzahl der Nachbarknoten um 1
        nconnect(i) = nconnect(i)-1

        EXIT vergleich
      end if

      if (k >= nconnect(i)) exit

      k = k + 1

    end do vergleich

    j = j + 1

  end do

end do

!nis,aug07: generate upward knowledge, store elements connected to nodes
BelongingElement: do i = 1, MaxE

  if (ncorn(i) == 0) CYCLE belongingElement

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

!NiS,mar06: deallocate temporary array
DEALLOCATE(arc)

!LF nov06: deallocate weir renumbering array
DEALLOCATE (reweir)
!-

!NiS,may06: Rewind for possible RESTART
REWIND(IFILE)

RETURN

END SUBROUTINE RDKALYPS
                                                                        

!SR---------------------------------------------------------------------
      SUBROUTINE nach 
!SR                                                                     
!SR Schleife zur Wandlung des FE-Netzes in ein CVFEM-Netz.Rechtecke werd
!SR durch Hinzufuegen eines neuen Punktes in zwei Dreiecke zerlegt.     
!SR                                                                     
!SR                                                      Dez.2001, S. Ra
!SR---------------------------------------------------------------------
                                                                        
      !NiS,mar06: 	change the module-access for global variables to the RMA10S-construction
      !INCLUDE "common.cfg"
      USE BLK10MOD

      !Nis,mar06:       The definition of the common-block raus was changed into
      !                 module ParaKalyps; for that the module is installed and the
      !                 common-block deactivated
      USE ParaKalyps
      !      COMMON / raus / rausv (4, mnd), iauslp, iausnpm, zeigma (mnd)

      COMMON / vnach / cvva, cvga, cvvo, cvgo, cvzu, cvfe, cvarccnt
!SR Zusaetzlicher Schalter fuer eckknotenspezifische Fehlerausgabe des C
      COMMON / cveck / cvcornernodes 
      INTEGER cvcornernodes 
      INTEGER newel, elcnt

!NiS,mar06: additional declarations, so that no undeclared variables are used
      INTEGER nodecnt

                                                                        
      elcnt = ne 
      nodecnt = np 
      WRITE ( *, * ) elcnt, nodecnt 
                                                                        
      DO 1500 i = 1, ne 
                                                                        
!SR Pruefe, ob Viereck vorliegt: Wandlung in CV-Dreieck; Dreiecke werden
!SR Kriterium sind Knoten 7 und 8: falls belegt => Viereck              
        IF (nop (i, 7) .gt.0) then 
          IF (nop (i, 8) .gt.0) then 
                                                                        
            elcnt = elcnt + 1 
            nodecnt = nodecnt + 1 
                                                                        
!SR Erstes Dreieck namens newel anstelle altem Viereck generieren: neuen
!SR errechnen und Viereckspositionen mit Null belegen ------------------
            newel = elcnt 
            nop (newel, 1) = nop (i, 1) 
            nop (newel, 2) = nodecnt 
            nop (newel, 3) = nop (i, 5) 
            nop (newel, 4) = nop (i, 6) 
            nop (newel, 5) = nop (i, 7) 
            nop (newel, 6) = nop (i, 8) 
            nop (newel, 7) = 0 
            nop (newel, 8) = 0 
                                                                        
!SR Zweites Dreieck anstelle altem Viereck generieren: Drehrichtung gege
!SR Dazu neuen Mittseitenknoten im Innern des alten Vierecks mit neuer g
!SR Knotennummer belegen und unerwuenschte Gleichungsnummern des Viereck
!SR Dreieck entsteht, alte und unveraenderte Positionen bleiben wie geha
            nop (i, 6) = nodecnt 
            nop (i, 7) = 0 
            nop (i, 8) = 0 
                                                                        
!SR Fuer neuen Mittseitknoten des ersten Dreiecks x/y/z-Koordinaten bere
            cord (nodecnt,1) = (cord (nop (i,1),1) + cord (nop (i,5),1) ) / 2
            cord (nodecnt,2) = (cord (nop (i,1),2) + cord (nop (i,5),2) ) / 2
            ao (nodecnt)   = (ao (nop (i,1) ) + ao (nop (i,5) ) ) / 2
            aour (nodecnt) = (ao (nop (i,1) ) + ao (nop (i,5) ) ) / 2
                                                                        
!SR Ausgabe alter und neuer Elementnummer falls neu vermascht-----------
            WRITE ( *, 5599) i, elcnt, nodecnt 
 5599 FORMAT      ('   alt Elem.: ',i6,'   neu Elem.:',i6,'   MSK.:',i6) 
                                                                        
!SR Eine lineare Interpolation vorhandener Restart-Informationen am neue
!SR ist nicht noetig, da beim einlesen von Daten eines nachvermaschten M
!SR bereits angelegt sind: Deaktiviertcccccccccccccccccccccccccccccccccc
!            if (cvva .eq. 1) then                                      
!                write(iout,*) '  NACH: VEL,RAUSV ermitteltc '          
!SR Drei Freiheitsgrade (vel) u. Ausgabewert des Wasserstandes (rausv) a
!               do 1900 j=1,3                                           
!               vel(j,nodecnt)=(vel(j,nop(i,1))+vel(j,nop(i,5)))/2      
!               rausv(3,nodecnt)=(rausv(3,nop(i,1))+rausv(3,nop(i,5)))/2
!1900           continue
!            endif                                                      
!                                                                       
!            if (cvga .eq. 1) then                                      
!                write(iout,*) '  NACH: VDOT ermitteltc'                
!SR Drei aktuelle Zeitgradienten (vdot) am neuen Mittseitenknoten       
!               do 1910 j=1,3                                           
!               vdot(j,nodecnt)=(vdot(j,nop(i,1))+vdot(j,nop(i,5)))/2   
!1910           continue                                                
!            endif                                                      
!                                                                       
!            if (cvvo .eq. 1) then                                      
!               write(iout,*) '  NACH: VOLD ermitteltc '                
!SR Drei Freiheitsgrade des vergangenen Zeitschritts (vold) am neuen Mit
!               do 1920 j=1,3                                           
!               vold(j,nodecnt)=(vold(j,nop(i,1))+vold(j,nop(i,5)))/2   
!1920           continue                                                
!            endif                                                      
!                                                                       
!            if (cvgo .eq. 1) then                                      
!                write(iout,*) '  NACH: VDOTO ermitteltc '              
!SR Drei Zeitgradienten des vergangenen Zeitschritts (vdoto) am neuen Mi
!               do 1930 j=1,3                                           
!               vdoto(j,nodecnt)=(vdoto(j,nop(i,1))+vdoto(j,nop(i,5)))/2
!1930           continue                                                
!            endif                                                      
!                                                                       
!            if (cvzu .eq. 1) then                                      
!                write(iout,*) ' NACH: NDRY,HEL,HOL,HDET,HDOT ermitteltc
!SR Zusatzinformationen am neuen Element                                
!               ndry(nodecnt)=(ndry(nop(i,1))+ndry(nop(i,5)))/2         
!               hel(nodecnt)=(hel(nop(i,1))+hel(nop(i,5)))/2            
!               hol(nodecnt)=(hol(nop(i,1))+hol(nop(i,5)))/2            
!               hdet(nodecnt)=(hdet(nop(i,1))+hdet(nop(i,5)))/2         
!               hdot(nodecnt)=(hdot(nop(i,1))+hdot(nop(i,5)))/2         
!            endif                                                      
!SR Ende Deaktivierung, da beim Restartfile eingelesencccccccccccccccccc
                                                                        
!WP 03.04.03 Anfang                                                     
!                                                                       
! Die Anweisung IMAT(NEWEL)=IMAT(I) muss bei GFEM immer durchlaufen werd
! bei CVFEM aber nur wenn CVFEM = 1 gilt:
!                                                                       
!            IF (fem.eq.0) then
              imat (newel) = imat (i) 
!            ELSEIF (cvfe.eq.1) then
!SR Rauhigkeitsinformationen und Bearbeitungsreihenfolge fuer neues Elem
!              imat (newel) = imat (i)
!            ENDIF
!                                                                       
!WP 03.04.03 Ende                                                       
!                                                                       
                                                                        
!SR                                                                     
          ELSE 
            GOTO 1500 
          ENDIF 
        ELSE 
          GOTO 1500 
        ENDIF 
                                                                        
 1500 END DO 
                                                                        
                                                                        
!SR   Alle Elemente haben fortan 6 Knoten                               
      DO 1501 i = 1, elcnt 
        ncorn (i) = 6 
 1501 END DO 
                                                                        
                                                                        
      np = nodecnt 
      ne = elcnt 
      cvcornernodes = np - cvarccnt 
!NiS,mar06: unit name changed; changed iout to Lout
      WRITE (Lout, * ) 'Nachvermaschung beendetc'
      WRITE (Lout, * ) 'Uebergebene El.-anzahl=', ne
      WRITE (Lout, * ) 'Uebergebene Gesamtknotenanzahl(incl. MSK)=', np
      WRITE (Lout, * ) 'Uebergebene Eckknotenanzahl=', cvcornernodes
      RETURN 
      END SUBROUTINE nach                           
                                                                        
                                                                        
!-----------------------------------------------------------------------
!                                                                       
SUBROUTINE start_node (qlist, k, n)
!                                                                       
! Hier wird die Liste der Knoten generiert,
! bei denen das reordering startet.
!                                                                       
!-----------------------------------------------------------------------

!NiS,mar06: change the module-access for global variables to the RMA10S-construction
!INCLUDE "common.cfg"
USE BLK10MOD

INTEGER qlist (2, 160)


IF (lmt (1) .gt.0) then

  DO i = 1, lmt (1)
    qlist (1, i) = line (1, i)
  END DO

ELSE

  read_totalnr_nodes: do
    write (*,*) 'Wieviele Knoten werden als Startknoten gegeben?'
    read (*,*) k
    if (k .gt. 0) exit read_totalnr_nodes
  end do read_totalnr_nodes

  all_nodes: DO i = 1, k

    WRITE ( * , * ) 'Knotennummer ', i,': '

    read_nr: do
      READ ( * , '(i5)') qlist (1, i)
      IF ( (qlist (1, i) .le.0) .or. (qlist (1, i) .gt. n) ) then
        WRITE ( * , * ) 'Knotennummer unzulaessig, Neueingabe:'
        CYCLE read_nr
      else
        EXIT read_nr
      ENDIF
    end do read_nr

  END DO all_nodes

ENDIF

qlist (2, 1) = 9999999

RETURN

END SUBROUTINE start_node

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine InterpolateProfs (statElSz, statNoSz, MaxP, MaxE, MaxIntPolElts, IntPolNo, NeighProf, ncorn, nop, &
                          & cord, ao, kmx, kmWeight, IntPolProf, IntPolElts, qgef, imat)
implicit none
INTEGER, intent (IN) :: statElSz, statNoSz, MaxP, MaxE, MaxIntPolElts
integer, intent (in) :: IntPolNo (1: MaxE)
INTEGER, intent (INOUT) :: IntPolElts (1: MaxE, 1: MaxIntPolElts)
integer, intent (inout) :: NeighProf (1: MaxP, 1: 2)
integer, intent (inout) :: ncorn (MaxE)
integer, intent (inout) :: nop (1: MaxE, 1: 8), imat (1: MaxE)
real, intent (inout) :: cord (1: MaxP, 1: 2)
REAL, intent (inout) :: ao (1: MaxP), kmx (1: MaxP), kmWeight (1: MaxP)
REAL, INTENT (INOUT) :: qgef (1: MaxP)
LOGICAL, INTENT (INOUT) :: IntPolProf (1: MaxP)
!
!*********local ones***********
INTEGER :: i, j
integer :: NewNode, NewElt
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
                      &  qgef(nop(i, 1)), qgef (nop (i, 4)), qgef (NewNode))
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
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (nop (i, 2)))
        cord (nop (i, 2), 1) = 0.5 * (cord (nop (i, 1), 1) + cord (nop (i, 3), 1))
        cord (nop (i, 2), 2) = 0.5 * (cord (nop (i, 1), 2) + cord (nop (i, 3), 2))
        ao (nop (i, 2)) = 0.5 * (ao (nop (i, 1)) + ao (nop (i, 3)))
        kmWeight (nop (i, 2)) = 0.5 / REAL (IntPolNo (i) + 1, KIND = 8)
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
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (NewNode))
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
                      &  qgef (nop (i, 1)), qgef (nop (i, 4)), qgef (NewNode))
        cord (nop (NewElt, 2), 1) = 0.5 * (cord (nop (NewElt, 1), 1) + cord (nop (NewElt, 3), 1))
        cord (nop (NewElt, 2), 2) = 0.5 * (cord (nop (NewElt, 1), 2) + cord (nop (NewElt, 3), 2))
        ao (NewNode) = 0.5 * (ao (nop (NewElt, 1)) + ao (nop (NewElt, 3)))
        kmWeight (NewNode) = (j - 0.5) / REAL (IntPolNo (i) + 1, KIND = 8)

      end if
      !Generate the last element
      if (j == IntPolNo (i) ) then
        NewElt = NewElt + 1
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
                      &  qgef (nop (i, 1)), qgef (nop (NewElt, 3)), qgef (NewNode))
        cord (NewNode, 1) = 0.5 * (cord (nop (NewElt, 1), 1) + cord (nop (NewElt, 3), 1))
        cord (NewNode, 2) = 0.5 * (cord (nop (NewElt, 1), 2) + cord (nop (NewElt, 3), 2))
        ao (NewNode) = 0.5 * (ao (nop (NewElt, 1)) + ao (nop (NewElt, 3)))
        kmWeight (NewNode) = (j + 0.5) / REAL (IntPolNo (i) + 1, KIND = 8)
      end if
    end do
  end if
end do
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine GenNewNode (NewNode, OrigElt, cordx, cordy, neighbour1, neighbour2, NewNeighProf, LogIntPolProf, qgef1, qgef2, qgefOut)
implicit none
integer, intent (in)  :: NewNode
integer, intent (out) :: NewNeighProf (1:2)
INTEGER, INTENT (IN)  :: OrigElt
REAL (KIND = 8), INTENT (IN)  :: cordx, cordy
REAL (KIND = 8), INTENT (IN)  :: qgef1, qgef2
REAL (KIND = 8), INTENT (OUT) :: qgefOut
INTEGER, INTENT (IN)  :: neighbour1, neighbour2
Logical, intent (out) :: LogIntPolProf

NewNeighProf(1) = neighbour1
NewNeighProf(2) = neighbour2

if (qgef1 /= qgef2) then
  WRITE(*,*) neighbour1, neighbour2
  call ErrorMessageAndStop (1111, OrigElt, cordx, cordy)
else
  qgefOut = qgef1
end if
LogIntPolProf = .true.
END subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

