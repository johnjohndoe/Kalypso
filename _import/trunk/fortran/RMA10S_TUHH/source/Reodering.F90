!     Last change:  WP   28 Jul 2008    5:20 pm
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


!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                                                                       
SUBROUTINE reord_Kalyps (qlist)
!
!     Der reordering-Algorithmus von rma1                               
!
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
USE BLK10MOD
USE BLK2
USE BLKASTEPH

implicit none

!maxsize shows the maximum size of the kntimel allocation, so the largest element
INTEGER :: maxsize, JunctionSize
integer :: lastentry
integer :: maxc, maxcn
integer :: qlist (2, 3535)
INTEGER, allocatable :: icon (:, :), kntimel (:, :)
parameter (maxcn = 60)
!NiS,mar06: variable names changed; changed mel to MaxE and mnd to MaxP
!array kntimel must be allocatable, because the size is vacant while calling the reordering sequence

!locals
integer :: i, j, k, l, n, m
integer :: mp, idxx, is
integer (kind = 8) :: mpp

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
  if (maxps == 0) then
!if there is no transitioning, the allocation runs in normal way; the number should be decreasable from 8 to 4.  
    maxsize = 8
  else
!might be a doubled dependency    
    maxsize = 16
  endif
endif

allocate (icon (1: MaxP, 1: 60), kntimel (1: MaxE, 1: maxsize))

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
ALLOCATE (mlist(3535))
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

!nis,jun07: Initializing the allocated variables
do i = 1, MaxP
  list(i) = 0
  msn(i) = 0
end do
do i = 1, 5000
  ihold(i) = 0
end do
!-

!get the node numbers into temporary memory
StoreConnectivity: DO i = 1, MaxE

  IF (nop (i, 1) == 0) CYCLE StoreConnectivity

  IF (imat (i) < 901 .OR. imat(i) > 903) THEN

!node number (corner nodes) is 2 for 1D elements    
    k = 2
!if it is a triangular element increase number of corner nodes to 3    
    IF (nop (i, 5) > 0) k = 3
!if it is a quadrilateral element increase number of corner nodes to 4    
    IF (nop (i, 7) > 0) k = 4
!save the node numbers    
    lastentry = 0
    DO j = 1, k
      l = 2 * j - 1
      kntimel (i, j) = nop (i, l)
      lastEntry = j
    ENDDO
    if (maxps /= 0) then
      if (ConnectedElt (i) /= 0) then
        do j = 1, ncorn(ConnectedElt(i))
          kntimel (i, lastEntry + j) = nop (ConnectedElt(i), j)
        enddo
      endif
    endif

!TODO: This is not efficient, there must be another way to find out membership in 1D-2D-Transition line    
!if there is a 1D 2D transition line, then save the nodes of the line in the element array kntimel    
    if (maxlt > 0 .AND. k == 2) then
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
DO n = 1, MaxP
  DO m = 1, maxc
    icon (n, m) = 0
  enddo
enddo

!This loop fills in all nodes, that have direct connection to others into icon array.
ConnectsOuter: DO n = 1, MaxE

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
DO n = 1, MaxP
  IF (icon (n, 1) > 0) nepem = nepem + 1
END DO

mpq = 0
list (1) = 1
mp = 1
allnodes: DO n = 1, MaxP
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

  END DO allconnects

  mpq = mpq + mp
ENDDO allnodes

mpp = mpq
                                                                        
! korrektur von mpp                                                     
orderloop: DO

  DO i = 1, MaxP
    DO j = 1, maxc
      icon (i, j) = iabs (icon (i, j) )
    ENDDO
  ENDDO

  CALL order (idxx, kntimel, maxsize, qlist, icon)

  DO k = 1, 3535
    qlist (1, k) = qlist (2, k)
  END DO

  IF (idxx >= 99999) EXIT orderloop

ENDDO orderloop

!ERROR - Reordering could not be fullfilled
IF (mpp <= mpq) then
  call ErrorMessageAndStop (3501, 0, 0.0d0, 0.0d0)
ENDIF

DO n = 1, nepem
  iel (n) = msn (n)
END DO
!-                                                                      
!......zero arrarys                                                     
!-                                                                      
      DO n = 1, MaxE
        nfixh (n) = 0 
        list (n) = 0
      enddo

      DO n = 1, MaxP 
        DO m = 1, maxc
          icon (n, m) = 0
        ENDDO
      ENDDO
!-                                                                      
!......form nodes connected to elements array                           
!-                                                                      
      Nodes2ConnElts: DO n = 1, MaxE
        N2CEltsOuter: DO m = 1, ncn

          i = kntimel (n, m) 

          IF (i == 0) cycle Nodes2ConnElts

          N2CEltsInner: DO j = 1, maxc

            IF (icon (i, j) /= 0) cycle N2CEltsInner
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
        IF (i == 0) EXIT FormList
        FormListInner: DO j = 1, maxc
          m = icon (i, j) 
          IF (m == 0) CYCLE FormList
          IF (list (m) > 0) cycle FormListInner
          k = k + 1 
          nfixh (k) = m 
          list (m) = k 
        END DO FormListInner
      END DO FormList

      AssignNfixh: DO n = 1, MaxE
        IF (list (n) /= 0) cycle AssignNfixh
        k = k + 1 
        nfixh (k) = n 
      END DO AssignNfixh

!NiS,mar06: unit name changed; changed iout to Lout
!      write(Lout,98) (nfixh(k),k=1,elem)
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
      deallocate (icon, kntimel)

      RETURN
      END SUBROUTINE reord_Kalyps


                                                                        
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
SUBROUTINE order (n, kntimel, maxsize, qlist, icon)


USE BLK10MOD
USE BLK2
USE BLKASTEPH

implicit none

INTEGER :: ia
INTEGER :: maxsize
INTEGER :: counter

INTEGER :: qlist (2, 3535)
!INTEGER :: icon (:,:), kntimel (:,:)
INTEGER :: icon (1:MaxP, 1:60), kntimel (1:MaxE, 1:maxsize)
integer :: maxcn, maxc
integer (kind = 8) :: mpo, isum
integer :: i, j, k, m, n, ii, jj, mm
integer :: nod, nelt, nel, mp
integer (kind = 4) :: nlist (1:3535)



parameter (maxcn = 60)

!initializing
ia = 0

counter = 0
maxc  = maxcn
mpo   = mpq
mpq   = 0
isum  = 0

!node list to start reordering sequence from
DO j = 1, 3535
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
!      if(nprt > 0) write(Lout,6040)
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
  IF (nel /= 1 .OR. counter /= 1) then

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
!if(nprt > 0) write(Lout,'(3i5)') n,ia,mp
!if(nprt > 1) write(Lout,'(20x,25i4)')  (list(j),j=1,mp)

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
IF (mpq >= mpo) mpq = mpo
IF (mpq == mpo) return
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

INTEGER :: maxsize
INTEGER :: kntimel (1:MaxE, 1:maxsize)
INTEGER :: icon (1:MaxP, 1:60)
integer :: maxcn

parameter (maxcn = 60)


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

INTEGER :: qlist (2, 3535)


IF (lmt (1) > 0) then

  DO i = 1, lmt (1)
    qlist (1, i) = line (1, i)
  END DO

ELSE

  read_totalnr_nodes: do
    write (*,*) 'Wieviele Knoten werden als Startknoten gegeben?'
    read (*,*) k
    if (k > 0) exit read_totalnr_nodes
  end do read_totalnr_nodes

  all_nodes: DO i = 1, k

    WRITE ( * , * ) 'Knotennummer ', i,': '

    read_nr: do
      READ ( * , '(i5)') qlist (1, i)
      IF ( (qlist (1, i) <= 0) .OR. (qlist (1, i) > n) ) then
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
                                                                        

