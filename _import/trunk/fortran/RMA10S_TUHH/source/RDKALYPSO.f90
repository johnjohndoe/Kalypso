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

    ! Feld mit den Liniennummern initialisiern:
    DO l = 1, nn
      line (j, l) = 0
      !NiS,mar06: material assignment deactivated
      !lineimat (j, l) = 0
      !-
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

    DO l = 1, nn
      IF (line (j, l) .eq. 0) then
        write (*,6000) j
        6000 format (/1X, '----------------------------------------', /, &
                    & 1X, 'Error in line of continuity no.  ', I3, ' !', /, &
                    & 1X, 'Fehler in Kontinuitaetslinie Nr. ', I3, ' !', /, &
                    & 1X, '----------------------------------------')
        STOP
      ENDIF
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
!NiS,mar06: Name-Conflict in RMA10S, so this specific Kalypso-2D subroutine is renamed
!      SUBROUTINE reord (knot, elem, qlist)
      SUBROUTINE reord_Kalyps (knot, elem, qlist)
!
!     Der reordering-Algorithmus von rma1                               
!                                                                       
!                                                                       
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      !NiS,mar06: change the module-access for global variables to the RMA10S-construction
      !INCLUDE "common.cfg"
      USE BLK10MOD
      !NiS,mar06: the following two modules replace the two common blocks used originally in
      !this subroutine
      USE BLK2
      USE BLKASTEPH
      !-
      !nis,dec06: Using module that includes the definition of the continuity lines
      USE BLK10MOD
      !-

      PARAMETER (maxcn = 60, adjmax = 20, adjdepth = 2000)
                                                                        
!NiS,mar06: variable names changed; changed mel to MaxE and mnd to MaxP
      !array kntimel must be allocatable, because the size is vacant while calling the reordering sequence
      INTEGER, ALLOCATABLE ::  kntimel (:,:)
      integer :: qlist (2, 160), knot, elem
      INTEGER :: icon (MaxP, maxcn)
      !maxsize shows the maximum size of the kntimel allocation, so the largest element
      INTEGER :: maxsize

!NiS,mar06: the common blocks are replaced with modules dealing with allocatable arrays. These modules are inserted above and allocated below.
!      COMMON / blkasteph / mpq, nepem, nr, nc, nprt, mist, mlist (500),
!      msn (mnd), icol (100, 5000), nadm (5000), inum (mnd), alpha (mnd)
!      COMMON / blk2 / wd (mnd), iel (mnd), list (mnd), ihold (5000),    &
!      paxis (mel)
!-

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
       !allocate it, the size must be two slots bigger, because the two cornder nodes of the connecting 1D-element must be included
        ALLOCATE (kntimel (MaxE, MaxSize))
      else
        !if there is no transitioning, the allocation runs in normal way; the number should be decreasable from 8 to 4.
        ALLOCATE (kntimel (MaxE, 8))
      endif
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

!nis,feb07,testing
WRITE(*,*) 'Reordering started'
pause
!-

!get the node numbers into temporary memory
DO i = 1, ne
  !node number (corner nodes) is 2 for 1D elements
  k = 2
  !if it is a triangular element increase number of corner nodes to 3
  IF (nop (i, 5) > 0) k = 3
  !if it is a quadrilateral element increase number of corner nodes to 4
  if (nop (i, 7) > 0) k = 4
  !save the node numbers
  DO j = 1, k
    l = 2 * j - 1
    kntimel (i, j) = nop (i, l)
  enddo

  !nis,feb07,testing
  !WRITE(*,*) maxlt
  !WRITE(*,*) 'Element: ',(kntimel(i,j), j=1, k)
  !-

  !if there is a 1D 2D transition line, then save the nodes of the line in the element array kntimel
  if (maxlt > 0 .and. k == 2) then
    !first the correct line has to be found, by running through all possible lines
    FindLine: do j = 1, MaxLT
      !stop, if it is the correct line
      if (TransLines (j, 1) == i) then
        !run through line defintion and transfer nodes to kntimel
        do l = 1, lmt( TransLines (j,2))
          !copy line node into kntimel. If it is the connecting node, jump over, because it is only allowed to occur once
          IF (line (TransLines (j,2), l) /= nop (i, 3)) kntimel (i, l+2) = line( TransLines (j,2), l)
        enddo
        EXIT FindLine
      end if
    end do FindLine
    !WRITE(*,*) i, (kntimel(i,l),l=1,lmt(TransLines(j,2)))
    !pause

  end if
enddo
!nis,feb07,testing
!pause
!-

maxc = maxcn

!The size is in general 4, but if there was a line transition in the network, this number increases to maxsize
if (MaxLT /= 0) then
  ncn = maxsize
else
  ncn  = 4
end if

!nis,feb07,testing
WRITE(*,*) ncn, 'maximale Elementgroesse'
pause
!-

ierr = 0

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
!      WRITE(*,*) m, i, l
!      pause

      !cycle process if the the second node choice is the same as the first one. They don't need to be connected
      IF (l == i) CYCLE ConnectsInner

      !Check, whether node is either not zero nor the same as already inserted in the connection matrix (icon); if not fill in the actual node
      !number connection
      GetConnection: DO j = 1, maxc
        IF (icon (i, j) == 0) exit GetConnection
        IF (icon (i, j) == l) CYCLE ConnectsInner
        if (j == maxc) then
          ierr = 1
          !NiS,mar06: unit name changed; changed iout to Lout
          WRITE (Lout, 6090) maxc, i
        ENDIF
      END DO GetConnection
      icon (i, j) = l
      !write(*,*) 'Weise Knoten ', i, 'Knoten ', l, 'zu'
    END DO ConnectsInner
  END DO

END DO ConnectsOuter
!pause

 6090 FORMAT( // 10x, '***error-more than',i5,  ' nodes connected to node', i5 )

IF (ierr.eq.1) stop 5

      nepem = 0 
      DO 510 n = 1, np 
        IF (icon (n, 1) .gt.0) nepem = nepem + 1 
  510 END DO 
      mpq = 0 
      list (1) = 1 
      mp = 1 
      DO 700 n = 1, np 
        IF (icon (n, 1) .eq.0) goto 700 
        is = 0 
        DO 530 m = 1, mp 
          IF (n.eq.list (m) ) is = 1
          IF (is.eq.1) list (m) = list (m + 1)
  530   END DO 
        IF (is.eq.1) mp = mp - 1 
        DO 600 j = 1, maxc 
          i = icon (n, j) 
          IF (i.eq.0) goto 610 
          IF (i.le.n) goto 600 
          DO 550 m = 1, mp 
            IF (i.eq.list (m) ) goto 560 
  550     END DO 
!NiS,mar06: unit name changed; changed iout to Lout
!      if( mp .eq. mnd ) write(Lout,80)
   80 FORMAT( / 10x, 'over 1000 terms in list' ) 

!NiS,mar06: name of variable changed; changed mnd to MaxP
          IF (mp.eq.MaxP) mp = mp - 1
          mp = mp + 1
          list (mp) = i 
  560     CONTINUE 
  600   END DO 
  610   CONTINUE 
!NiS,mar06: unit name changed; changed iout to Lout
!      if(nprt.eq.3) write(Lout,'(25i4)') (list(m),m=1,mp)
!      mpq=mpq+mp*mp                                                    
        mpq = mpq + mp 
  700 END DO 
!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,99) mpq
   99 FORMAT(1h1//10x,'for initial order, reordering sum =',i10) 
      mpp = mpq 
                                                                        
! korrektur von mpp                                                     
                                                                        
  790 CONTINUE 
      DO 780 i = 1, np 
        DO 780 j = 1, maxc 
  780 icon (i, j) = iabs (icon (i, j) ) 

      CALL order (idxx, kntimel, qlist, icon) 

      DO 785 k = 1, 160 
        qlist (1, k) = qlist (2, k) 
  785 END DO 
!                                                                       
      IF (idxx.lt.99999) goto 790 

      !nis,feb07,testing
      WRITE(*,*) mpp, mpq
      WRITE(*,*) 'vor Fehler'
      pause
      !-
      IF (mpp.le.mpq) stop 6 
      DO 810 n = 1, nepem 
        iel (n) = msn (n) 
  810 END DO 
!-                                                                      
!......zero arrarys                                                     
!-                                                                      
      DO 820 n = 1, ne 
        nfixh (n) = 0 
  820 list (n) = 0 
      DO 840 n = 1, np 
        DO 840 m = 1, maxc 
  840 icon (n, m) = 0 
!-                                                                      
!......form nodes connected to elements array                           
!-                                                                      
      DO 900 n = 1, ne 
        DO 880 m = 1, ncn 
          i = kntimel (n, m) 
          IF (i.eq.0) goto 900 
          DO 860 j = 1, maxc 
            IF (icon (i, j) .ne.0) goto 860 
            icon (i, j) = n 
            GOTO 880 
  860     END DO 
  880   END DO 
  900 END DO 
      DO 910 n = 1, np 
  910 END DO 
!-                                                                      
!......form list of elements to be formed                               
!-                                                                      
      k = 0 
      DO 960 n = 1, nepem 
        i = iel (n) 
        IF (i.eq.0) goto 980 
        DO 940 j = 1, maxc 
          m = icon (i, j) 
          IF (m.eq.0) goto 960 
          IF (list (m) .gt.0) goto 940 
          k = k + 1 
          nfixh (k) = m 
          list (m) = k 
  940   END DO 
  960 END DO 
  980 CONTINUE 
      DO 1020 n = 1, ne 
        IF (list (n) .ne.0) goto 1020 
        k = k + 1 
        nfixh (k) = n 
 1020 END DO 
!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,98) (nfixh(k),k=1,ne)
   98 FORMAT(//10x,'selected element order is listed below'//(5x,10i10))

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
      SUBROUTINE order (n, kntimel, qlist, icon)
!-
      !NiS,mar06: change the module-access for global variables to the RMA10S-construction
      !INCLUDE "common.cfg"
      USE BLK10MOD
      !NiS,mar06: the following two modules replace the two common blocks used orinally in
      !this subroutine
      USE BLK2
      USE BLKASTEPH
!-
!NiS,mar06: name of variable has changed in RMA10S
!      INTEGER kntimel (mel, 8), qlist (2, 160)
      INTEGER kntimel (MaxE, 8), qlist (2, 160) 
                                                                        
      PARAMETER (maxcn = 60, adjmax = 20, adjdepth = 2000) 
                                                                        
!NiS,mar06: name of variable has changed in RMA10S
!      INTEGER icon (mnd, maxcn)
      INTEGER icon (MaxP, maxcn)
!-

!NiS,mar06: 	the common blocks are replaced with modules dealing with allocatable
!               arrays. These modules are inserted above and allocated in subroutine reorder
!      COMMON / blkasteph / mpq, nepem, nr, nc, nprt, mist, mlist (500),
!                           msn (mnd), icol (100, 5000), nadm (5000), inum (mnd), alpha (mnd)
!NiS,mar06: name of variable has changed in RMA10S
!      COMMON / blk2 / wd (mnd), iel (mnd), list (mnd), ihold (5000),    &
!                      paxis (mel)
!-
      DIMENSION nlist (160)
      maxc = maxcn 
      mpo = mpq 
      mpq = 0 
      isum = 0
!-                                                                      
      DO 210 j = 1, 160 
        nlist (j) = qlist (1, j) 
  210 END DO 
                                                                        
  225 CONTINUE 
      nelt = 2
      n = nlist (1) 
!NiS,mar06: name of variable changed; changed mnd to MaxP
      IF (n.gt.MaxP) return

!NiS,mar06: unit name changed; changed iout to Lout
!      write(*,*) 'Lout in reord =',Lout
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
      DO 1095 i = 1, maxc 
        jj = icon (n, i) 
        IF (jj.eq.0) goto 1095 
        DO 1090 m = 1, maxc 
          IF (icon (jj, m) .eq.n) goto 1093 
 1090   END DO 
        GOTO 1095 
 1093   icon (jj, m) = - icon (jj, m) 
 1095 END DO 
      IF (nel.eq.1) goto 1070 
!     set values for each new poiunt eliminated                         
!                                                                       
  240 DO 250 i = 1, 41 
        DO 250 j = 1, 40 
  250 icol (i, j) = 0 
      nod = nod+1 
      mist = 0 
      nr = 1 
      nadm (nr) = 100 
      DO 600 m = 1, mp 
        ii = list (m) 
        CALL adjpt (ii, m, kntimel, icon) 
  600 END DO 
 1000 m = icol (1, 1) 
!      print *,'m=',m                                                   
 1005 n = list (m) 
      IF (ihold (1) .lt.249) goto 1010 
      m = 1 
      n = list (1) 
 1010 CONTINUE 
      ia = ihold (m) 
      nel = nel + 1 
      IF (nlist (nelt) .eq.0) goto 1035 
      nelt = nelt + 1 
      n = nlist (nel) 
      DO 1030 mm = 1, mp
        m = mm 
        IF (list (m) .eq.n) goto 1035 
 1030 END DO 
      mp = mp + 1 
      list (mp) = n 
      m = mp 
 1035 CONTINUE 
      iel (nel) = n 
      DO 1040 i = 1, m 
 1040 ihold (i) = ihold (i) + 1 
      mp = mp - 1 
      IF (m.gt.mp) goto 1070 
      DO 1050 i = m, mp 
        ihold (i) = ihold (i + 1) + 1 
        list (i) = list (i + 1) 
 1050 END DO 
 1070 CONTINUE 
!     add to column adjacent point of eliminated point                  
!                                                                       
      DO 270 j = 1, maxc
        !nis,feb07,testing
        WRITE(*,*) j, ii, icon(n,j)
        !-
        ii = icon (n, j) 
        IF (ii.le.0) goto 270 
        mp = mp + 1 
        ihold (mp) = 1 
        list (mp) = ii 
  270 END DO 
!      mpq=mp*mp+mpq                                                    
      mpq = mp + mpq 
      isum = isum + ia 
!NiS,mar06: unit name changed; changed iout to Lout
!      if(nprt.gt.0) write(Lout,'(3i5)') n,ia,mp
!      if(nprt.gt.1) write(Lout,'(20x,25i4)')  (list(j),j=1,mp)
      DO 1200 i = 1, maxc 
        jj = icon (n, i)
        IF (jj.le.0) goto 1200 
        DO 1150 m = 1, maxc 
          k = iabs (icon (jj, m) ) 
          IF (k.eq.0) goto 1150 
          DO 1100 mm = 1, maxc 
            IF (icon (k, mm) .eq.jj) goto 1105 
 1100     END DO 
          GOTO 1150 
 1105     icon (k, mm) = - icon (k, mm) 
 1150   END DO 
 1200 END DO 
      IF (nod.lt.nepem) goto 240 
!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,6050) mpq,isum
! 6050 format(//10x,'reordering sum =',i10,'  band sum =',i8// )        
! 6050 format(//10x,'reordering Summe =',i10,'  Band Summe =',i8// )    
      IF (mpq.ge.mpo) mpq = mpo 
      IF (mpq.eq.mpo) return 
      DO 1400 n = 1, nepem 
        msn (n) = iel (n) 
 1400 END DO 
      RETURN 
      END SUBROUTINE order                          
                                                                        
                                                                        
                                                                        
!-----------------------------------------------------------------------
      SUBROUTINE adjpt (ii, m, kntimel, icon)
!                                                                       
!     Hier wird ???  WP                                                 
!                                                                       
!-----------------------------------------------------------------------
!-
      !NiS,mar06: change the module-access for global variables to the RMA10S-construction
      !INCLUDE "common.cfg"
      USE BLK10MOD
      !NiS,mar06: the following two modules replace the two common blocks used orinally in
      !this subroutine
      USE BLK2
      USE BLKASTEPH
!-

!NiS,mar06: name of variable has changed in RMA10S
!      INTEGER kntimel (mel, 8) 
      INTEGER kntimel (MaxE, 8)
      PARAMETER (maxcn = 60, adjmax = 20, adjdepth = 2000)
!      INTEGER icon (mnd, maxcn)
      INTEGER icon (MaxP, maxcn)

!-
!NiS,mar06: 	the common blocks are replaced with modules dealing with allocatable
!               arrays. These modules are inserted above and allocated in subroutine reorder
!      COMMON / blkasteph / mpq, nepem, nr, nc, nprt, mist, mlist (500),
!      msn (mnd), icol (100, 5000), nadm (5000), inum (mnd), alpha (mnd)
!      COMMON / blk2 / wd (mnd), iel (mnd), list (mnd), ihold (5000),    &
!      paxis (mel)
!-

      maxc = maxcn 
      nad = 0 
                                                                        
      DO 100 i = 1, maxc 
        jj = icon (ii, i) 
        IF (jj.le.0) goto 100 
        IF (jj.eq.mist) goto 100 
        nad = nad+1 
  100 END DO 
                                                                        
      IF (nad-nadm (nr) ) 200, 300, 400 
  200 nadm (nr) = nad 
      icol (nr, 1) = m 
      nc = 1 
      GOTO 400 
  300 nc = nc + 1 
      icol (nr, nc) = m 
  400 CONTINUE 
      RETURN 
      END SUBROUTINE adjpt                          
                                                                        
                                                                        
                                                                        

!-----------------------------------------------------------------------
!nis,dec06: Adding TLcnt as counter for number of 1D-2D-Transition lines
!SUBROUTINE RDKALYPS(nodecnt,elcnt,arccnt,KSWIT)
SUBROUTINE RDKALYPS(nodecnt,elcnt,arccnt,TLcnt,KSWIT)
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
      USE ParaFlow1dFE  !Modul für Teschke-1D-Elemente
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
      						       !the purpose is, not to generate errors with numerical gaps in defintion lists.
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
CHARACTER (LEN=120)    :: linie                        !temporary input space for reading input/restart-file
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
INTEGER 	       :: midsidenode_max              !comparison variable for the highest necessary node number, so it can be tested, whether
                                      		       !the node list is complete
!NiS,apr06: NEW SWITCH AND FILE READING CONTROL OPTIONS
INTEGER                :: KSWIT                        !Switch for Run-option of this subroutine;
						       !  0:      read whole geometry info and install values into the proper arrays
                                                       !  1:  read just the dimension informations about geometry
                                                       !  2:  read just the resart values

INTEGER                :: unit_nr                      !internal unit number, because this subroutine works as modell and restart data reader

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
  MAXA=0                  !arc(i,j) is not necessary for the first run, so that it is allocated
  maxe=0                  !EFa Nov06
  ALLOCATE(arc(MAXA,5))   !just pro forma, it is deallocated at the end of this run.
ELSE                      !In the second run, the value of MAXA is known and the arc-array is
  ALLOCATE(arc(MAXA,5))   !allocated again; it is deallocated at the end of the run.
ENDIF                     !NiS,mar06
arcs_without_midside = 0
midsidenode_max = 0

!Nis,may06,com: initializing the I/O error space
istat =0

nodecnt = 0
arccnt  = 0
elcnt   = 0
!nis,dec06: adding TLcnt for 1D-2D-Transition-lines
TLcnt   = 0
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

!NiS,mar06: temparc is an array for reading arc informations in the dimensioning run (KSWIT.eq.1)
DO i=1,5
  temparc(i)=0
ENDDO

!NiS,mar06: changed allocation of arc; the former allocation with the number of nodes was too big, so the MaxA (for maximum number of arcs) is used:
!           changed mnd to MaxA; additionally the array is firstly initialized, when the geometry is read (KSWIT.eq.0)
IF (KSWIT==0) THEN
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

!NiS,mar06: geometry file is open, opening is not necessary. Addionally the units are no more applicable for RMA10S
!OPEN (12, FILE=modellein, FORM='formatted', STATUS='OLD', ACTION='READ', IOSTAT=istat)
!if (istat /= 0) then
!  write (*,200) istat
!  stop
!end if
!200 format (1X, 'Opening data file failed. (IOSTAT =',I2,')',/ &
!          & 1X, 'Stopping Program...')
!-

!NiS,apr06: chosing file unit
!           for restart option unit = nb
!           for modell input   unit = ifile
IF (KSWIT /= 2) THEN
  unit_nr = ifile
ELSE
    unit_nr = nb
ENDIF

!INQUIRE(unit_nr, opened=inquiretest)
INQUIRE(unit_nr, form=inquiretest)
WRITE(*,*) 'inquiretest',inquiretest
WRITE(*,*) 'unit_nr',unit_nr
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
  !error handling for read errors
  if (istat /= 0) then                          !error handling for reading errors
    !NiS,mar06: unit name changed; changed modellein to unit_nr; should be the file name, not the unit number
    write (*,*) ' Error while reading file ', unit_nr                             !give error-message and stop the program instantly
    stop
  end if

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
        !NiS,mar06: changed id to id_local; global conflict
        !READ (linie, '(a2,i10,3f20.7)') id_local, i, cord (i, 1) , cord (i, 2), ao (i)
        !EFa Nov06, einlesen der Kilometrierung
        !read (linie, '(a2,i10,4f20.7)') id_local, i, cord (i, 1) , cord (i, 2), ao (i), kmx(i)
        istat=0

        READ (linie, *,IOSTAT=istat) id_local, i, cord (i, 1) , cord (i, 2), ao (i),kmx(i)
        !WRITE(*,*)'FP ',id_local,i,cord(i,1),cord(i,2),ao(i),kmx(i)

        if (istat.eq.0) then
          WRITE(lout,*)'Die Kilometrierung von Knoten',i,'wurde eingelesen:',kmx(i)
        end if

        IF (i.gt.nodecnt) nodecnt = i
        !NiS,mar06: Net dimension was checked before; there's no Restriction concerning maximum array size; ERROR test deactivated
        !  IF (nodecnt.gt.mnd) THEN
        !    write (*,7000) mnd
        !    7000 format (/1X, 'ERROR: Number of nodes in mesh is larger than allowed maximum!', /, &
        !               &  1X, '       Please increase MND in common.cfg (actual value MND = ',I7, ')')
        !    stop
        !  END if
        !-
        !Stop program execution on negative NODE number
        IF (i.le.0) stop 'Knotennummer.le.0'                                  
      ENDIF
    ENDIF

    !ARC DEFINITIONS ---
    IF (linie (1:2) .eq.'AR') then

      !NiS,mar06: Find out maximum ARC number
      IF (KSWIT == 1) then
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,6i10)') id_local,i, (temparc(j),j=1,5)
        IF (i  > arccnt) arccnt = i

        !NiS,mar06: Net dimension was checked before; there's no Restriction concerning maximum array size; ERROR test deactivated
        !  IF (arccnt.gt.mnd) THEN
        !    write (*,7001) mnd
        !    7001 format (/1X, 'ERROR: Number of arcs in mesh is larger than allowed maximum!', /, &
        !               &  1X, '       Please increase MND in common.cfg (actual value MND = ',I7, ')')
        !    STOP
        !  ENDIF
        !-
        !Remember, whether ARC has no midside node
        IF (temparc(5) == 0) then
          arcs_without_midside = arcs_without_midside + 1
        !Remember the maximum midside ID-number, if midside is defined
        ELSE
          midsidenode_max = MAX (midsidenode_max,temparc(5))
        ENDIF
      !NiS,mar06: Read ARC geometry informations
      ELSE
        READ (linie,'(a2,6i10)') id_local, i, (arc(i,j), j=1, 5)
        IF (i > arccnt) arccnt = i
        !Stop program execution on negative ARC number
        IF (i.le.0) stop 'Kantennummer.le.0'                                  
      ENDIF
    ENDIF

    !EFa Nov06, Gültigeitsgrenzen der Polynome
    if (linie(1:2) .eq. 'MM') then
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
      else
        read (linie, '(a2,i10,2f20.7)') id_local, i, hhmin(i), hhmax(i)
        !write (*,*) 'MM',i,hhmin(i),hhmax(i)
      endif
    end if

    !EFa Nov06, Flächenpolynom
    if (linie (1:3) .eq. 'AP1') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,(apoly(i,j), j=1,5)
        !write (*,*) 'AP1',i,(apoly(i,j), j=1,5)
      endif
    end if

    if (linie (1:3) .eq. 'AP2') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local,i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,(apoly(i,j), j=6,10)
        !write (*,*) 'AP2',i,(apoly(i,j), j=6,10)
      endif
    end if

    if (linie (1:3) .eq. 'AP3') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,3f20.7)') id_local, i,(apoly(i,j), j=11,13)
        !write (*,*) 'AP3',i,(apoly(i,j), j=11,13)
      endif
    end if

    !EFa Nov06, Q(h)-Kurve
    if (linie (1:3) .eq. 'QP1') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,qgef(i),(qpoly(i,j), j=1,4)
        !write (*,*) 'QP1',i,qgef(i),(qpoly(i,j), j=1,4)
      endif
    end if

    if (linie (1:3) .eq. 'QP2') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,(qpoly(i,j), j=5,9)
        !write (*,*) 'QP2',i,(qpoly(i,j), j=5,9)
      endif
    end if

    if (linie (1:3) .eq. 'QP3') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,4f20.7)') id_local, i,(qpoly(i,j), j=10,13)
        !write (*,*) 'QP3',(qpoly(i,j), j=10,13)
      endif
    end if

    !EFa Nov06, h-Bordvoll
    if (linie(1:2) .eq. 'HB') then
      IF (KSWIT == 1) then
        read (linie,'(a2,i10)') id_local,i
      else
        read (linie,'(a2,i10,f20.7)') id_local,i,hbordv(i)
        !write (*,*) 'HB', i, hbordv(i)
      endif
    end if

    !EFa Nov06, alpha-Übergang bis h
    if (linie (1:2) .eq. 'AD') then
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
      else
        read (linie, '(a2,i10,5f20.7)') id_local, i,alphah(i),(alphad(i,j), j=1,4)
        !write (*,*) 'AD',i,alphah(i),(alphad(i,j), j=1,4)
      endif
    end if

    !EFa Nov06, alpha-Beiwertpolynomkoeffizienten
    if (linie (1:3) .eq. 'AK1') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,(alphapk(i,j), j=1,5)
        !write (*,*) 'AK1',i,(alphapk(i,j), j=1,5)
      endif
    end if

    if (linie (1:3) .eq. 'AK2') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,(alphapk(i,j), j=6,10)
        !write (*,*) 'AK2',i,(alphapk(i,j), j=6,10)
      endif
    end if

    if (linie (1:3) .eq. 'AK3') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,3f20.7)') id_local, i,(alphapk(i,j), j=11,13)
        !write (*,*) 'AK3',i,(alphapk(i,j), j=11,13)
      endif
    end if

    !EFa Nov06, beta-Übergang bis h
    if (linie (1:2) .eq. 'BD') then
      IF (KSWIT == 1) then
        read (linie, '(a2,i10)') id_local, i
      else
        read (linie, '(a2,i10,5f20.7)') id_local, i,betah(i),(betad(i,j), j=1,4)
        !write (*,*) 'BD',i,betah(i),(betad(i,j), j=1,4)
      endif
    end if

    !EFa Nov06, beta-Beiwertpolynomkoeffizienten
    if (linie (1:3) .eq. 'BK1') then
       IF (KSWIT == 1) then
         read (linie, '(a3,i9)') id_local, i
       else
         read (linie, '(a3,i9,5f20.7)') id_local, i,(betapk(i,j), j=1,5)
         !write (*,*) 'BK1',i,(betapk(i,j), j=1,5)
       endif
    end if

    if (linie (1:3) .eq. 'BK2') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,5f20.7)') id_local, i,(betapk(i,j), j=6,10)
        !write (*,*) 'BK2',i,(betapk(i,j), j=6,10)
      endif
    end if

    if (linie (1:3) .eq. 'BK3') then
      IF (KSWIT == 1) then
        read (linie, '(a3,i9)') id_local, i
      else
        read (linie, '(a3,i9,3f20.7)') id_local, i,(betapk(i,j), j=11,13)
        !write (*,*) 'BK3',i,(betapk(i,j), j=11,13)
      endif
    end if

    !ELEMENT DEFINITIONS ---
    IF (linie (1:2) .eq.'FE') then
      !NiS,mar06: Find out maximum ELEMENT number
      IF (KSWIT == 1) THEN
        READ (linie, '(a2,i10)') id_local,i
        IF (i.gt.elcnt) elcnt = i
      ELSE
        cvfe = 1
        !NiS,mar06: changed id to id_local; global conflict
        READ (linie, '(a2,4i10)') id_local, i, imat (i), imato (i), nfixh (i)
        IF (i.gt.elcnt) elcnt = i
        !NiS,mar06: Net dimension was checked before; there's no Restriction concerning maximum array size; ERROR test deactivated
        !  IF (elcnt.gt.mel) then
        !    write (*,7002) mel
        !    7002 format (/1X, 'ERROR: Number of elements in mesh is larger than allowed maximum!', /, &
        !               &  1X, '       Please increase MEL in common.cfg (actual value MEL = ',I7, ')')
        !    stop
        !  END if
        !Stop program execution on negative ELEMENT number
        IF (i.le.0) stop 'Elementnummer.le.0'
      ENDIF
    ENDIF

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
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      !Stop program execution on negative NODE number
      IF (i.le.0) stop 'Knotennummer.le.0'
      !NiS,apr06: This command is not necessary, deactivated
      ! restart als komplett vorhanden ansehen. Keine Teichloesung:
      !nb = 62
      !-
    ENDIF

    !NiS,may06: these degrees of freedom are missing in Kalypso-2D, because they are not used there; adding for application in RMA10S
    !INITIAL VALUES FOR DEGREES OF FREEDOM 4 TO 7 ---
    IF (linie(1:2) .eq. 'DF') THEN
      READ(linie,'(a2,i10,4f20.7)') id_local, i, (vel(j,i), j=4,7)
    END IF
    !-

    !INITIAL GRADIENTS OF VELOCITIES AND WATER DEPTH OF ACTIVE TIME STEP ---
    IF (linie (1:2) .eq.'GA') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvga = 1
      !-
      READ (linie, '(a2,i10,3f20.7)') id_local, i, (vdot(j,i),j=1,3)
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on negative NODE number
      IF (i.le.0) stop 'Knotennummer.le.0'
    ENDIF

    !ADDITIONAL INFORMATIONS FOR EVERY NODE ---
    IF (linie (1:2) .eq.'ZU') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvzu = 1
      !NiS,apr06: only hel(i) and hdet(i) have to be read; changing read command
      !READ (linie, '(a2,i10,i6,4f15.7)') id, i, ndry(i), hel(i), hol(i), hdet(i), hdot(i)
      READ(linie,'(a2,i10,6x,2(f15.7,15x))')id_local,i,hel(i),hdet(i)

      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on negative NODE number
      IF (i.le.0) stop 'Knotennummer.le.0'
    ENDIF

! TEST BLOCK FOR ERRORS ------------------------------------------------------------------------

  ELSE !other values of KSWIT will generate an error
    stop 'KSWIT has wrong value'

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

    !VALUES OF VELOCITIES AND WATER DEPTH OF OLD TIME STEP ---
    !IF (linie (1:2) .eq.'VO') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvvo = 1
      !READ (linie, '(a2,i10,3f20.7)') id, i, (vold (j, i) , j=1,3)             !vold muss NICHT gelesen werden
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      !IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on negative NODE number
      !IF (i.le.0) stop 'Knotennummer.le.0'
    !ENDIF

    !GRADIENTS OF VELOCITIES AND WATER DEPTH OF OLD TIME STEP ---
    !IF (linie (1:2) .eq.'GO') then
      !NiS,apr06: variables deactivated for RMA10S
      !cvvo = 1
      !READ (linie, '(a2,i10,3f20.7)') id, i, (vdoto (j, i) , j=1,3)            !vdoto muss NICHT gelesen werden
      !NiS,mar06: name of variable changed; changed mnd to MaxP
      !Stop program execution on nodenumber higher than MaxP; could normally not happen
      !IF (i.gt.MaxP) stop 'i.gt.MaxP'
      !Stop program execution on negative NODE number
      !IF (i.le.0) stop 'Knotennummer.le.0'
    !ENDIF

END DO reading
WRITE(*,*) ' Schaffe die Leseschleife'


!nis,dec06: Form Continuity lines length arrays
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
!-

!NiS,mar06: file is closed in the calling subroutine getgeo; it can't be closed here because it is used twice
!CLOSE (12)

!ENDBLOCK FOR THE CASE OF DIMENSION READING (KSWIT.eq.1) ----------------
!NiS,mar06: leave subroutine, if FE-net dimensions are ascertained
IF (KSWIT == 1) THEN                                                    !midside nodes that are not explicitly definied also require space
  IF (midsidenode_max > nodecnt) THEN                                   !in the nodes' arrays. Problem is, that later on, when the not de-
    WRITE(*,*)' There is an error with the midside nodes. The node: '   !fined midside nodes are generated, the ID-numbers of the nodes
    WRITE(*,*)' ',midsidenode_max,' is defined in an arc but not in'    !are generated as sequential numbers based upon the original number of
    WRITE(*,*)' the node list. Stop!'                                   !nodecnt. Therefore, the following run of this subroutine to place
    STOP	                                                        !the geometry into the proper arrays has to find out the nodecnt
  ENDIF                                                                 !again without the addition of arcs_without_midside. There are two
  WRITE(*,*)' In RDKALYPS.Info: '                                       !problems with that. First the calculation time increases marginally
  WRITE(*,*)' Kantenzahl: ',arccnt                                      !(that means it is not really a problem) and second the input dimensions
  WRITE(*,*)' höchste definierte Knotennummer: ',nodecnt                !are installed twice so that there might be an error occurance. But
  WRITE(*,*)' Kanten ohne midside: ', arcs_without_midside              !normally everything should work properly because the commands and the
  nodecnt = nodecnt + arcs_without_midside                              !geometry won't change between the runs with KSWIT=1 and KSWIT=0.

  REWIND(IFILE)
  DEALLOCATE(arc)                                                       !the pro forma allocation of arc(i,j) is stopped
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
!-


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
!				of direct access to the array elements 2...5
!				NiS,mar06
!     - ELEM(j,1) Kantenanzahl + 1
!     - ELEM(j,2-4/5) Kantennummern
!     mit j=Elementnummer


!NiS,may06: every arc's element(s) will be saved in the elem-array, which shows after Loop-Execution the grouping of nodes belonging to an element.
!           The stored values are: On the first place the number of nodes associated with the element and on the 2nd to 5th place the nodes (unsorted)
!
!           Changed the if-clauses in the way that, 1D, 1D-2D-transitions and 2D-elements can be recognized
DO i = 1, arccnt

  !1D-ELEMENT, 1D-2D-TRANSITION-ELEMENT or DEAD ARC
  IF (arc(i,3) .eq. arc(i,4)) THEN

    !SKIP DEAD ARCS WITH NO ELEMENT NUMBERS ASSIGNED
    IF (arc(i,3) .eq. 0) THEN
      WRITE (Lout,9003) i
      WRITE (*   ,9003) i

    !1D (TRANSITION ELEMENTS AND NORMAL ELEMENTS)
    ELSE
      j = arc(i,3)
      !Error, if the potential 1D-NODE is used, which means (elem(j,1).ne.0)
      IF (elem(j,1) .ne. 0) THEN
        CLOSE(75)
        OPEN (75, FILE = 'ERROR.DAT')
        WRITE (75,9002) j
        WRITE (* ,9002) j
        STOP
      !assign identification for 1D-NODES (elem(j,1).eq.-1) for normal 1D-ELEMENTS and (elem(j,1).eq.-2) for 1D-2D-transition elements; remember
      !the ARC, that defines the 1D-ELEMENT for later NODE extraction
      ELSE
        !No 4th NODE at normal 1D-ELEMENTS
        IF (nop(arc(i,3),4).eq.0) THEN
          if (arc(i,5).EQ.-9999) then
            !EFa Nov06, Teschke (nur Eckknoten)
            elem(j,1)=-3
          else
            !EFa Nov06, mit Mittseitenknoten
            elem(j,1)=-1
          end if
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
        !Testing whether element is already used as 1D-ELEMENT
        IF (elem(j,1) == -1) THEN
          CLOSE(75)!Fehler TO WRITE
          OPEN (75, FILE = 'ERROR.DAT')
          WRITE (75,9002) j
          WRITE (* ,9002) j
          STOP
        ENDIF
        !Testing, whether it is the first defining arc
        IF (elem (j, 1) .eq.0) elem (j, 1) = 1
        !Increase number of assigned ARCS to ELEMENT by increment =1
        elem (j, 1) = elem (j, 1) + 1
        !Test whether to much ARCS are assigned to ELEMENT
        IF (elem (j, 1) .gt.5) then
          !Error if more than 4 ARCS are assigned to ELEMENT
          CLOSE(75)
          OPEN (75, FILE = 'ERROR.DAT')
          WRITE (75,9001) j
          WRITE (* ,9001) j
          STOP
        ENDIF
        ! Dem Feld ELEM(j,2...5) wird die Nummer der Kante zugewiesen. (z.B.) ELEM(1000,2)=45
        elem (j, elem (j, 1) ) = i
      ENDIF
    ENDDO
  ENDIF
ENDDO
!-

!NiS,may06: For that no error message is disturbing while reading the code above, it is written at the end of the Loop
 9001 format ('Element',i6,' mehr als 4 Kanten -> STOP')
 9002 format ('ERROR - ELEMENT ', i6, ' IS USED TWICE! NOT POSSIBLE!',/ 'EXECUTION TERMINATED!')
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

    !EFa Nov06, Passing corner nodes to node array
    nop(i,1) = arc(elem(i,2),1)
    nop(i,3) = arc(elem(i,2),2)
    nop(i,2) = -9999


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
    IF (jnum.eq.1 .or. jnum.eq.2) THEN
      !NiS,mar06: unit name changed; changed iout to Lout
      WRITE (Lout,*)'Element ', i, ' weniger als 3 Kanten! -> STOP!'
      WRITE (   *,*)'Element ', i, ' weniger als 3 Kanten! -> STOP!'
      STOP
    ENDIF
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
    ! weitere Kanten:                                       	    !The other two or three arcs defining the actual element i are analysed from
2222   l = l + 1                                               	    !this point on. The jumpmark 2222 is somthing like a do loop.
    IF (l.gt.jnum) THEN                                       	    !The first if-case checks whether the actual arc l is the last one to define
      IF (elkno (1) .ne. elkno (l) ) THEN                     	    !the actual element i. If so and the last node is not identical with the first
        !NiS,mar06: unit name changed; changed iout to Lout         !one, the program will stop because the actual element is not defined properly.
        WRITE (Lout,*) ' Element nicht rund. -> STOP!'
        WRITE ( *  ,*) ' Element nicht rund. -> STOP!'
        STOP
      ENDIF
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
    !NiS,mar06: unit name changed; changed iout to Lout
    WRITE (Lout, 100) i        !error-message, if the elements has no closed border
    WRITE ( * ,  100) i
100 format (1X, 'Element Nr.: ', I5, ' nicht geschlossen! verknuepft! -> STOP!')
    STOP
  ! Element O.K.
2444 CONTINUE                                                       !If everything is okay, the cornernodes will be entered in the nop-array
    ! => Eckknotennummern ins nop-feld eintragen -------------------!in every second position starting with the first. If defined yet the midside
    DO j = 1, jnum                                                  !nodes will be entered in every second position starting with the second slot
      nop (i, j * 2 - 1) = elkno (j)                                !of the nop-array.
    END DO
    ! => Mittseiten-knotennummern ins nop-feld eintragen -----------
    DO j = 1, jnum
      IF (mikno (j) .gt.0) then
        IF (mikno (j) .gt.nodecnt) stop 'mikno(j).gt.nodecnt'       !If the actual midside node is greater than the nodecnt, it can't be defined
        nop (i, j * 2) = mikno (j)                                  !in the node list, what is followed by an error message.
      ENDIF
    END DO
    ! write(*,*)'Element',i,' Knoten:',(elkno(j),j=1,5)

    ! Kantenkreuzung,Verdrehung abfragen  --------------------------!the actual element is checked for crossing arcs or something else,
    crossing_outer: DO j = 1, jnum - 2                                !not congruent.

      vekkant(1) = cord (nop (i,(j+1)*2-1),1)-cord(nop (i,j*2-1),1)
      vekkant(2) = cord (nop (i,(j+1)*2-1),2)-cord(nop (i,j*2-1),2)

      crossing_inner: DO k = j + 2, jnum

        vekpu1 (1) = cord(nop(i,k*2 - 1),1) - cord(nop(i,j*2-1),1)
        vekpu1 (2) = cord(nop(i,k*2 - 1),2) - cord(nop(i,j*2-1),2)
        kreuz = vekkant (1) * vekpu1 (2) - vekkant (2) * vekpu1 (1)
        ! write(*,*)'Element ',i, 'kreuz(',j,',',k,')=',kreuz
        IF (kreuz.le.0.0) then
        !NiS,mar06: unit name changed; changed iout to Lout
          WRITE (Lout, *) i
          write (   *, *) i
   101    format (1X, 'Element ', I5, 'verdreht, Kantenkreuzung, Ueberlappung o.ae.',/, &
                & 1X, 'Korrektur des Netzes notwendig')
          STOP
        ENDIF
      END DO crossing_inner
    END DO crossing_outer
  ENDIF dimensionif
END DO all_elem

! CONTROLOUTPUT -------------------------------------------------------------------------------------------

!NiS,mar06: Controloutput changed
!Old:
!WRITE (iout,102) elzaehl
!WRITE (*   ,102) elzaehl
!WRITE (iout,103) elcnt, nodecnt, arccnt
!WRITE ( *  ,103) elcnt, nodecnt, arccnt
!102 format (1X, 'Number of activ elements: ', I6 / &
!          & 1X, 'Checking mesh O.K.!'/)
!103 format (1X, 'Number of elements: ', I6/ &
!          & 1X, 'Number of nodes:    ', I6/ &
!          & 1X, 'Number of arcs:     ', I6/)
!NEW:
WRITE (Lout,103) elcnt, nodecnt, arccnt
WRITE ( *  ,103) elcnt, nodecnt, arccnt
WRITE (Lout,102) elzaehl
WRITE (*   ,102) elzaehl
103 format (1X, 'dimension of element-array   : ', I6 / &
          & 1X, 'dimension of cornernodes     : ', I6 / &
          & 1X, 'dimension of arcs            : ', I6 /)
102 format (1X, 'Number of activ elements     : ', I6 / &
          & 1X, 'Checking mesh O.K.!'/)
!-

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

  !nis,dec06: dead arcs have to be skipped
  if (arc(i,1).eq.0) CYCLE all_arcs
  !-

  !EFa Nov06, Mittseitenknoten für 1D-Teschke-Element werden nicht berechnet
  if (arc(i,5).NE.-9999) then
  
  ! Mittseitenknoten vorhanden?
  !NiS,expand test for defined midside nodes in ARC-array but without coordinate-definitions; this was a logical gap
  !IF ( (arc (i, 5) .gt.0) .and. (arc (i, 5) .le.nodecnt) ) goto 1402
  IF ((arc(i,5).gt.0) .and. (arc(i,5).le.nodecnt)) THEN
    IF ((cord(arc(i,5),1)/=0.0) .and. (cord(arc(i,5),2)/=0.0)) THEN
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
    nodecnt = nodecnt + 1          !If a new midside node is generated, the program is told which ID to take for that midsidenode; the ID
                                   !is the result of increasing the actual maximum active node number by 1.
    mittzaehl = mittzaehl + 1      !Increase the counter for added new midside nodes

    !These lines could be economized with using the arc-array directly, where it is needed; WHY COPY?
    ibot = arc (i, 1)
    itop = arc (i, 2)
    ilft = arc (i, 3)
    irgt = arc (i, 4)
    !NiS,may06: Test for dead arcs, so the DO-LOOP may be cycled:
    IF (ilft.eq.irgt .and. ilft.eq.0) CYCLE all_arcs
    !-

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
    IF (ilft.eq.irgt) THEN
      !1D-2D-TRANSITION-ELEMENTS may have a midside node already
      IF (nop(ilft,2).ne.0) CYCLE all_arcs
      !all other normal 1D-ELEMENTS or 1D-2D-TRANSITION-ELEMENTS get a midside node
      nop(ilft,2) = nodecnt

    !2D-ELEMENT ARC:
    ELSE
      IF (ilft.gt.0) THEN
!NiS,mar06      the first element gives the number of arcs + 1 because
!		of the direct access to the following elements elem(j,i=2...5)
!		which are the arcs 1...4.
!NiS,mar06: elem(i,1) gives the number of corner nodes +1; replaced by subtracting 1
!            jnum = elem (ilft, 1)
        jnum = elem (ilft, 1) - 1
!-
        DO 1401 j = 1, jnum
          IF (nop (ilft, 2 * j - 1) .eq.ibot) THEN
            nop (ilft, 2 * j) = nodecnt
            GOTO 1300
          ENDIF
1401   ENDDO
      ENDIF

1300  IF (irgt.gt.0) THEN
!NiS,mar06: changes see above
!        jnum = elem (irgt, 1)
        jnum = elem (irgt, 1) - 1
!-
        DO j = 1, jnum
          IF (nop (irgt, 2 * j - 1) .eq.itop) THEN
            nop (irgt, 2 * j) = nodecnt
            !NiS,may06: Cycling LOOP clearly:
            !GOTO 1400
            CYCLE all_arcs
            !-
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  !NiS,may06: IF clause for coordinate test
  ENDIF

ENDIF
!NiS,may06: 1402 jump mark has no more use
!1402 CONTINUE
!NiS,may: changed do LOOP to cycle it clearly
!1400 END DO all_arcs
END DO all_arcs
!-

!NiS,mar06: unit name changed; changed iout in Lout
!user informations
WRITE (Lout,106) mittzaehl
WRITE (*   ,106) mittzaehl
106 FORMAT (1X, 'Number of added     '/ &
          & 1X, 'midside nodes:      ', I6/)

!NiS,may06: Checking the cross section informations at corner nodes of 1D-ELEMENTS and interpolating them for midside nodes of 1D-ELEMENTS
! CHECKING/INTERPOLATION OF CROSS SECTION INFORMATIONS -----------------------------------------------------------------------------------

DO i = 1, arccnt
  !Only 1D-ELEMENT-ARCS stand this test
  !EFa Nov06, nicht für 1D-Teschke-Elemente
  IF (arc(i,3).eq.arc(i,4) .and. arc(i,3).ne.0.and. arc(i,5).NE.-9999) THEN
    !error if one of the two corner nodes does not have cross sectional informations
    IF (WIDTH(nop(arc(i,3),1)).eq.0 .or. WIDTH(nop(arc(i,3),3)).eq.0) THEN
      CLOSE (75)
      OPEN(75,FILE = 'ERROR.DAT')
      WRITE (75,9004) nop(arc(i,3),1), nop(arc(i,3),3)
9004  FORMAT ('ERROR - 1D-NODE ', I6, ' OR ', I6, ' HAS NO CROSS SECTIONAL INFORMATIONS.',/'EXECUTION OF PROGRAM TERMINATED!')
      CLOSE(75)
      STOP
    ELSE
      !Interpolate cross sectional informations for midside nodes, if necessary
      !EFa Nov06, Korrektur der Knoten (2.Knoten wird zwischen dem 1. und 3. Knoten interpoliert)
      IF(WIDTH (nop(arc(i,3),2)) .eq. 0) THEN
        width (nop(arc(i,3),2)) = 0.5 * (width (nop(arc(i,3),1)) + width (nop(arc(i,3),3)))
        ss1 (nop(arc(i,3),2))   = 0.5 * (ss1 (nop(arc(i,3),1))   + ss1 (nop(arc(i,3),3)))
        ss2 (nop(arc(i,3),2))   = 0.5 * (ss2 (nop(arc(i,3),1))   + ss2 (nop(arc(i,3),3)))
        wids (nop(arc(i,3),2))  = 0.5 * (wids (nop(arc(i,3),1))  + wids (nop(arc(i,3),3)))
        widbs (nop(arc(i,3),2)) = 0.5 * (widbs (nop(arc(i,3),1)) + widbs (nop(arc(i,3),3)))
        wss (nop(arc(i,3),2))   = 0.5 * (wss (nop(arc(i,3),1))   + wss (nop(arc(i,3),3)))
      END IF
    END IF
  ENDIF
ENDDO
!-
 

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
      n = nodecnt
      m = elcnt
      np = nodecnt
      ne = elcnt
!NiS,mar06: unit name changed; changed iout to Lout
!     write(Lout,*)'  1402:  nodecnt=',nodecnt
                                                                        
! ----------------------------------------------------------------------
!     Kopie des Hoehenfeldes
!NIS,may06: variable name changed; changed mnd to MaxP
      DO 3000 i = 1, MaxP
        aour (i) = ao (i)
 3000 END DO
                                                                        
! ----------------------------------------------------------------------------------------------------------------
!     Initialize check / ncl Linien zusammensetzen:
      !NiS,mar06: Checking of Continuity lines is not possible; the data is not available at this runtime; DEACTIVATED for RMA10S, it will be checked
      !           in RMA10S run
!      CALL check

!WP 04.04.03 Anfang ---------------------------------
!
!   Aufrufen von NACH bei trad. GFEM (FEM = 0) nicht nötig
      IF (fem.ne.0) then
!SR -----------------------------------------------------
!SR jetzt die Nachvermaschung zum CVFEM-Netz durchfuehren
!NiS,mar06: unit name changed; changed iout to Lout
      WRITE (Lout,104)
      WRITE ( *  ,104)
      104 format (1X, 'Nachvermaschung aufgerufen...')

!SR Die Nachvermaschung muss auch dann aufgerufen werden, wenn bereits
!SR nachvermascht wurde (Restart), denn es wird die Eckknotenanzahl
!SR weitergegeben, die benoetigt wird, um den CVFehler auszugeben.
!SR      if (nfixh(1)==0) then
!SR Deshalb keine Beschraenkung auf die reine Teichloesungccc
        CALL nach
!SRTest      end if
      ENDIF
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
    if (TransLines(i,1).eq.0) CYCLE elementturning

    !test for correct order of nodes in transitioning element
    if (nop(TransLines(i,1),3) .ne. TransLines(i,3)) then

      !nis,jan07: Checking, whether node is defined in element on the other slot (slot 1)
      if (nop(TransLines(i,1),1) .ne. TransLines(i,3)) then
        WRITE(*,*) 'ERROR - The transitioning node is not defined in element'
        WRITE(*,*) 'Problem occured in line ', i
        WRITE(*,*) 'Check the defined transition node in TL-line with the two'
        WRITE(*,*) 'corner nodes of the transitioning 1D-element'
        WRITE(*,*) 'Program can not be executed'
        WRITE(*,*) 'Nodes are: '
        WRITE(*,*) nop(TransLines(i,1),1), nop(TransLines(i,1),3), TransLines(i,3)
        WRITE(*,*) nop(61,1), nop(61,3), TransLines(i,3)
        WRITE(*,*) TransLines(i,1)
        WRITE(*,*) 'STOP'
        stop
      end if
      !-

      !turn the corner nodes around
      noptemp = nop(TransLines(i,1),1)
      nop(TransLines(i,1),1) = nop(TransLines(i,1),3)
      nop(TransLines(i,1),3) = noptemp

    end if
  end do elementturning
endif
!-

!nis,nov06: Check, whether all 1D-2D-line-Transitions are connected. The Question ist, whether connecting node is part of the transition line:
if (MaxLT.ne.0) then
  call check_linetransition
end if
!-

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
CALL start_node (qlist, k, np)
!CALL reord (np, ne, qlist)
CALL reord_Kalyps (np, ne, qlist)
!-


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
do i=1,nodecnt
  do j=1,30
    neighb(i,j) = 0 	! Nummer der j-ten benachbarten Knotens von Knoten i
  end do
  nconnect(i) = 0       ! Anzahl der Nachbarknoten
end do


! Jedes Element durchgehen
neighbours: do i=1,elcnt

  ! leere Elementnummern uebergehen:
  if (elem(i,1).ne.0) then

    !nis,jan07: Get the transition number and the transitioning CCL
    findconnection: do j= 1, MaxLT
      if (TransLines(j,1).eq.i) then
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
  if (Transmember(ConnNumber).gt.0 .and. ConnNumber.ne.0) then
    !the slope is just connected to the CORNER nodes of the transition line, the midside nodes are not effecting!
    !number of nodes at that 1D-2D-transtion without the midsidenodes of the line
    ncorn_temp = lmt(ConnLine)+Transmember(ConnNumber)+1

    !nis,dec06,testing
    !WRITE(*,*) 'Element: ', i
    !-

    !this is the temporary nop-array for the '1D-2D-transitionline-element'
    ALLOCATE (nop_temp(1:ncorn_temp))

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
    nodeassigning: do j=1,lmt(ConnLine)
      !if the connecting node is also part of the line-defintion, this placeholder has to skipped. For next run through loop because j is increased,
      !  adding has to be decreased
      if (line(ConnLine,j) .eq. TransLines(ConnNumber,3)) then
        adding = 2
        CYCLE nodeassigning
      endif
      !overgive the connected node to the temporary array
      nop_temp(j+adding)=line(ConnLine,j)
      !nis,dec06: testing
      !WRITE(*,*) j+adding, nop_temp(j+adding)
      !-
    end do nodeassigning

    !store neighbourhood relations, it's nearly the same loop as in the original loop, shown below
    outerLT: do j = 1, ncorn_temp
      node1 = nop_temp(j)
      innerLT: do l = 1, ncorn_temp
        node2 = nop_temp(l)
        if (node1.ne.node2) then
          nconnect(node1) = nconnect(node1) + 1
          neighb(node1,nconnect(node1)) = node2
        end if
      end do innerLT
    end do outerLT
    !array resetting for next transition, that is probably be from other size
    DEALLOCATE(nop_temp)


    !nis,feb07: Here is a conflict!!!    

    !EFa Nov06, gesonderte Berechnung für 1D-Teschke-Elemente
    elseIF(nop(i,2).EQ.-9999) then
      node1 = nop(i,1)
      node2 = nop(i,3)
      nconnect(node1) = nconnect(node1)+1
      nconnect(node2) = nconnect(node2)+1
      neighb(node1,nconnect(node1)) = node2
      neighb(node2,nconnect(node2)) = node1
    else
    ! Lesen aller Knotennummern eines Elementes
    outer: do j=1,ncorn(i)

!      if (i == 2306) then
!        write (*,*) 'Lebe noch! i=', i, '   j=', j, '   nop(i,j)=', nop(i,j), 'ncorn(i): ', ncorn(i)
!      end if

!nis,dec06: for increasing speed of program, bring line to outside of loop
        node1 = nop(i,j)
!-

      inner: do l=1,ncorn(i)

!nis,dec06: for increasing speed of program, bring line to outside of loop
!        node1 = nop(i,j)
!-
        node2 = nop(i,l)

!        if (i == 2306) then
!          write (*,*) 'node1: ', node1, '   node2: ', node2
!        end if

        if (node1 .ne. node2) then
          nconnect(node1) = nconnect(node1) + 1
          neighb(node1,nconnect(node1)) = node2
!          if (i == 2306) then
!            write (*,*) 'nconnect(node1) = nconnect(',node1,'):', nconnect(node1)
!            write (*,*) 'neighb(node1,',nconnect(node1),'): ', neighb(node1,nconnect(node1))
!          end if

        END if

      end do inner

    end do outer

  !nis,dec06: endif for the test of whether line transition or not
  endif
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


!NiS,mar06: unit name changed; changed iout to Lout
WRITE (Lout, 111 )
WRITE ( *  , 111 )
  111 FORMAT (/1X, 'Reading model finished',/ &
            &  1X, '-----------------------------------------'//)

!NiS,mar06: deallocate temporary array
DEALLOCATE(arc)

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
            IF (fem.eq.0) then
              imat (newel) = imat (i) 
            ELSEIF (cvfe.eq.1) then 
!SR Rauhigkeitsinformationen und Bearbeitungsreihenfolge fuer neues Elem
              imat (newel) = imat (i) 
!               imato(newel)=imato(i)                                   
!               nfixh(newel)=newel                                      
            ENDIF 
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


