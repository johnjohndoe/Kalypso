!     Last change:  K    27 Feb 2007    4:21 pm
!-----------------------------------------------------------------------------
! This code, data_out.f90, performs writing and validation of model
! output data in the library 'Kalypso-2D'.
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
!

!NiS,apr06: changed name for application in RMA10S; intent is to make it a meaningful name
SUBROUTINE write_KALYPSO
!-

!
! Die Subroutine "modell_schreiben" schreibt die Modelldaten
! in dem neuen (Microstation) .asc Format;
! in einem solchen File liegen sowohl die Geometriedaten,
! die frueher auf dem geo-file gespeichert wurden, als auch
! die restart-Daten, falls vorhanden.
! Dabei Umwandlung der Darstellungsweise des Netzes von der
! elementweisen Speicherform in die Kantenstruktur.
!
!-----------------------------------------------------------------------------

!NiS,apr06: Replace common blocks and include files with modules
!INCLUDE "common.cfg"
!COMMON / raus / rausv (4, mnd), iauslp, iausnpm, zeigma (mnd)
USE BLK10mod
USE BLK11mod
USE PARAKalyps
USE BlkDRmod
!EFa Dec06, neues Modul für 1d-Teschke-Elemente
USE PARAFlow1dFE
!-

!NiS,mar06: Find undeclared variables
implicit none

!NiS,apr06: Comments added and array declarations changed, ARC changed to ARC_TMP because of global declaration conflicts in RMA10S.
!INTEGER :: arc (mnd, 5), arcmid (mnd, 4), arccnt, elcnt, nnum, i, j, k, nbot, ntop, nelem
INTEGER, ALLOCATABLE :: arc_tmp (:,:), arcmid (:,:)    !local arrays for ARC-handling
INTEGER              :: arccnt, elcnt                  !counter for ARCS and ELEMENTS
INTEGER              :: nnum                           !number of nodes of element, dependent on type and shape of element
INTEGER              :: i, j                           !counters for loops
INTEGER              :: k                              !node while preparing arc array for writing
INTEGER              :: nbot, ntop                     !the two nodes of an element
INTEGER              :: nelem                          !number of element counter for LOOP

!NiS,apr06: changing length of variable, because the length of filenames in RMA10S is 96.
!CHARACTER(LEN=80) :: nameout, namein
CHARACTER(LEN=96)    :: nameout, namein

INTEGER              :: istat                          !Variable for I/O errors
!NiS,apr06: Adding missing declarations for good programming practice:
INTEGER              :: nodecnt                        !nodenumber in network
INTEGER              :: iicyc                          !local variable for intermediat saving original time step number

!NEW DECLARATIONS FOR 1D-2D-TRANSITION ELEMENTS
INTEGER, ALLOCATABLE :: Trans_nodes(:,:)               !informations for writing specific informations about transition elements
INTEGER              :: trans_els                      !counter and name-giver for transition-elements

!NiS,apr06: allocating the two local arrays for arc-handling with the size of MaxP
ALLOCATE (arc_tmp (maxp,5), arcmid(maxp,5))            !EFa Dec06, Spaltenanzahl von arcmid für 1d-Teschke-Elemente auf 5 erhöht
!NiS,may06: allocate 1D-2D-TRANSITION ELEMENTS array (1: number; 2-6: nodes)
ALLOCATE (Trans_nodes(MaxT,6))

! INITIALIZING ---------------------------------------------------------

!NiS,may06: initializing the trans_els variable
trans_els = 0

nameout = ' '
namein = ' '
                                                                        
!--------------- Umwandlung in Kantenstruktur --------------------------
                                                                        
nodecnt = np
elcnt = ne

DO i = 1, np
  DO j = 1, 5
    !NiS,apr06: Changed arc to arc_tmp because of global conflict
    arc_tmp (i, j) = 0
    arcmid (i, j) = 0
  END DO
END DO
                                                                        
! Zu jedem Midseitenknoten eine Kante erzeugen:                         
!     (Kantennummer =Mittseitenknotennummer)                            
!     an allen Elementen :                                              
      !Run through every element
      DO 300 nelem = 1, ne 
        !Initialize nnum for first estimation of quadrilateral element to 8 nodes
        nnum = 8
        !Decrease nnum for triangular to 6 nodes
        IF (nop (nelem, 7) .eq.0) nnum = 6 
        !NiS,may06: Decrease nnum for 1D-2D-transition elements to 5 nodes
        IF (nop (nelem, 6) .eq.0) nnum = 5
        !NiS,may06: Decrease nnum for normal 1D-elements to 3 nodes
        IF (nop (nelem, 4) .eq.0) nnum = 3
!       leere Elementnummern uebergehen:
        IF (nop (nelem, 1) .eq.0) nnum = 0 
!        if(imat(nelem).le.0)nnum=0                                     
!       Mittseitenknoten durchgehen:
!NiS,may06: Differ between element types:

        !1D-ELEMENTS or 1D-2D-TRANSITION ELEMENTS
        IF (nnum .eq. 3 .or. nnum .eq. 5) THEN
          !midside node
          k = nop (nelem, 2)
          !no midside node assigned to arcmid yet:

          !nis,feb07: Allow for numbered FFF midsides
          !IF (arcmid (k, 3) .eq. 0.and.k.NE.-9999) THEN
          IF (arcmid (k, 3) .eq. 0 .and. k > -1000) THEN
          !-
            arcmid (k, 1) = nop (nelem, 1)
            arcmid (k, 2) = nop (nelem, 3)
            arcmid (k, 3) = nelem
            arcmid (k, 4) = nelem
          !EFa Dec06, Test der Fallunterscheidung
          !nis,feb07: Allow for numbered FFF midsides
          !ELSEIF (arcmid (nelem, 3) .eq. 0 .and. k.eq.-9999) then
          ELSEIF (arcmid (nelem, 3) .eq. 0 .and. k < -1000) then
          !-
            arcmid (nelem, 1) = nop (nelem, 1)
            arcmid (nelem, 2) = nop (nelem, 3)
            arcmid (nelem, 3) = nelem
            arcmid (nelem, 4) = nelem
            !nis,feb07: Enter the negative midside number
            !arcmid (nelem, 5) = -9999
            arcmid (nelem, 5) = k
            !-
          ENDIF
          !Save informations for 1D-2D-TRANSITION ELEMENTS
          IF (nnum .eq. 5) THEN
            !NiS,may06: testing transition elements
            WRITE(*,*) ' Element ', nelem
            !-
            trans_els = trans_els + 1
            IF (trans_els .gt. maxp) THEN
              CLOSE (75)
              OPEN (75, FILE = 'ERROR.DAT')
              WRITE (75,9001)
              write (* ,9001)
              9001 FORMAT('ERROR - TOO MANY TRANSITION ELEMENTS',/ 'PROGRAM EXECUTION STOPPED')
              STOP
            ENDIF
            Trans_nodes(trans_els, 1) = nelem
            DO i = 2,6
              Trans_nodes(trans_els, i) = nop(nelem,i-1)
            ENDDO
          ENDIF

        !2D-ELEMENTS
        ELSE
          DO 301 j = 2, nnum, 2
            k = nop (nelem, j)
                                                                        
            IF (arcmid (k, 3) .eq.0) then
!             neue Kante, aktuelles Element links:
              arcmid (k, 3) = nelem
!             Knoten unten:
              nbot = nop (nelem, j - 1)
              arcmid (k, 1) = nbot
              IF (j.eq.nnum) then
                ntop = nop (nelem, 1)
              ELSE
                ntop = nop (nelem, j + 1)
              ENDIF
!             Knoten oben:
              arcmid (k, 2) = ntop
                                                                        
            ELSE
!             Kante vorhanden, zweites Element rechts einfuegen und pruefe
              IF (arcmid (k, 4) .ne.0) then
                PRINT * , ' Am Mittseitenknoten ', k
                PRINT * , ' mehr als 2 Elemente'
                PRINT * , ' entdeckt bei Element: ', nelem
                STOP
              ENDIF
              arcmid (k, 4) = nelem

              ntop = nop (nelem, j - 1)
              IF (j.eq.nnum) then
                nbot = nop (nelem, 1)
              ELSE
                nbot = nop (nelem, j + 1)
              ENDIF
              IF ( (arcmid (k, 1) .ne.nbot) .or. (arcmid (k, 2) .ne.ntop) ) then
                PRINT * , 'Elementverknuepfung ', arcmid (k, 3) , arcmid (k, 4)
                STOP 'fehlerhaft'
              ENDIF
                                                                        
            ENDIF
    301   END DO
       ENDIF
 300 END DO
                                                                        

! ----------------------------------------------------------------------------------
! Kantennummern von 1 aufsteigend nummerieren:                          
arccnt = 0
DO i = 1, np
  IF (arcmid (i, 3) .ne.0) then
    arccnt = arccnt + 1
    DO j = 1, 5
      !Save 1st and 2nd node as well as elemts associated to the arc
      !NiS,apr06: Changed arc to arc_tmp because of global conflict
      arc_tmp (arccnt, j) = arcmid (i, j)
    END DO
    !Save midside node to ARC array
    !NiS,apr06: Changed arc to arc_tmp because of global conflict!
    !EFa Dec06, Fallunterscheidung für 1d-Teschke-Elemente
    !nis,feb07: Allow for numbered FFF midsides
    !if (arcmid(i,5).NE.-9999) then
    if (arcmid(i,5) > -1000) then
    !-
      arc_tmp (arccnt, 5) = i
    end if
  ENDIF
END DO
                                                                        
                                                                        
                                                                        

! -----------------------------------------------------------------------------------
!NiS,may06: Creation of output/solution file names has a little bit changed in logic order,
!           because there were some problems with steady state and results after iteration
!           The old version is completly deleted, it can be read in the code of Kalypso-2D.
      !after solution is converged or maximum number of iterations is reached
      IF (maxn == 0) THEN

        !for steady states
        IF (icyc == 0) THEN
          !output after steady state solution
          WRITE (nameout,'(a,a1,a)') 'steady', '.', modellaus
          IF (nb > 0) THEN
            !Restart file information after restarted steady state solution
            WRITE (namein,'(a)') modellrst
          ELSE
            !Restart file information after initial steady state solution
            WRITE (namein,'(a)') 'none, new 2D-Calculation'
          ENDIF

        !for dynamic solutions after first time step
        ELSEIF (icyc == 1 .and. iaccyc <= 1) THEN
          !output for dynamic solutions after first time step
          WRITE (nameout,'(a,i4.4,a1,a)') ct, icyc, '.', modellaus
          !Restart file information after user specified restart
          IF (niti == 0 .and. nb > 0) THEN
            WRITE (namein,'(a)') modellrst
          !No Restart, bloody new calculation; 'Teichlösung'
          ELSEIF (niti == 0 .AND. nb == 0) THEN
            WRITE (namein,'(a)') 'none, new 2D-Calculation'
          !Starting from steady state solution, that was calculated before
          ELSE
            WRITE (namein,'(a,a1,a)') 'steady','.', modellaus
          ENDIF

        !for dynamic solution after first timestep run restarted later time step than first
        ELSEIF (icyc == 1 .AND. iaccyc > 1) THEN
          !output file
          WRITE (nameout,'(a,i4.4,a1,a)') ct, icyc+iaccyc-1, '.', modellaus
          !Restart file information
          WRITE (namein, '(a)') modellein
        !for dynamic solution after calculated time step, that is not the first in current run
        ELSE
          !output file
          WRITE (namein,'(a,i4.4,a1,a)')  ct, icyc+iaccyc-2, '.', modellaus
          !Starting from the solution of time step before
          WRITE (nameout,'(a,i4.4,a1,a)') ct, icyc+iaccyc-1, '.', modellaus
        ENDIF

      !after iteration step, if wanted
      ELSE
        !input file name of output after iteration is not relevant
        WRITE(namein,'(a)') 'no regular result file, just iteration output'
        IF (icyc == 0) THEN
          !output filename after iteration in steady loop
          WRITE(nameout,'(a,i3.3,a1,a)') 'steady_Ite_', maxn, '.', modellaus
        ELSE
          !output filename after iteration in dynamic loop at time step (icyc)
          WRITE(nameout,'(a,i4.4,a,i3.3,a1,a)') ct, icyc,'_Ite',maxn,'.', modellaus
        ENDIF
      ENDIF
!-

!NiS,apr06: Write informational the generated output name; Changed iout to Lout
          WRITE ( * , * ) ' '
          WRITE ( * , * ) 'Name der Modell-Ausgabedatei: ', nameout
          WRITE (Lout, * ) ' '
          WRITE (Lout, * ) 'Name der Modell-Ausgabedatei: ', nameout
!-

! WRITING PART --------------------------------------------------------------------------

!NiS,apr06: Changed the unit name in the whole writing part from 12 to IKALYPSOFM

! Writing results in file
OPEN (IKALYPSOFM, FILE = nameout, FORM = 'formatted', STATUS = 'REPLACE', IOSTAT = istat)
if (istat /= 0) then
  write (*,2000) istat
  stop
end if


!WP 01.09.2004
!WP string = '00'//title
!WP WRITE (12, '(a80)') string

!WP Problem with length of title-character (78 or 80 ?)
!WP See SUB output and SUB modell_lesen
!WP Sometimes the calculation crashes after > 100 time steps
!WP Changing the format of output!
write (IKALYPSOFM, 7010) title
7010 format ('00', A80)

!WP string = 'RS'//namein
!WP WRITE (12, '(a80)') string
!NiS,may06: Giving out the Geometryfile and Restartfile; the length of the filenames are maximum 50 digits
!write (IKALYPSOFM, 7011) namein
!7011 format ('RS', A80)
write (IKALYPSOFM, 7011) modellein, modellrst
7011 format ('RS', 2A40)
!-
!WP 01.09.2004

! Time + step counter
!NiS,may06: nitsv in Kalypso-2D has the same meaning as icyc in RMA10S; changed nitsv to icyc
WRITE (IKALYPSOFM, 7008) tet, icyc
!-

!NiS,apr06: writing RMA10S-date format
TETT=(DAYOFY-1)*24.+TET
WRITE (IKALYPSOFM, 7012) iyrr, tett

! Node: Coordinates + Degrees of freedoms + Gradients + others:
write_nodes: DO i = 1, np

  !NiS,may06: The cord array is initialized with -1.e20, so every node with coordinates less than -1e19 that are skipped for writing
  IF (cord(i,1) .lt. -1.e19) CYCLE write_nodes
  !-
  if (kmx(i).NE.-1) then
    write (IKALYPSOFM, 7036) i, cord (i, 1), cord (i, 2), aour(i),kmx (i)  !EFa Dec06, Ausgabe der Kilometrierung, wenn vorhanden
  else
    WRITE (IKALYPSOFM, 7000) i, cord (i, 1), cord (i, 2), aour (i)
  endif
  WRITE (IKALYPSOFM, 7003) i, (vel (j, i), j = 1, 3) , rausv (3, i)
  !NiS,may06: All degrees of freedom have to be written and read for restart
  WRITE (IKALYPSOFM, 7015) i, (vel (j, i), j = 4, 7)
  !-

  IF (tet.ne.0.0) then
    WRITE (IKALYPSOFM, 7004) i, (vdot (j, i), j = 1, 3)
    WRITE (IKALYPSOFM, 7005) i, (vold (j, i), j = 1, 3)
    WRITE (IKALYPSOFM, 7006) i, (vdoto (j, i), j = 1, 3)
  ENDIF

  WRITE (IKALYPSOFM, 7007) i, ndry (i), hel (i), hol (i), hdet (i), hdot (i)

  !NiS,may06: Write cross sectional node information, if present
  IF (width (i) .ne. 0) THEN
    WRITE (IKALYPSOFM, 7013) i, width(i), ss1(i), ss2(i), wids(i), widbs(i), wss(i)
  END IF

  !EFa Dec06, weitere Daten einlesen für 1d-Teschke-Elemente
  teschke(i)=0
  do j=1,13
    if (apoly(i,j).ne.0) then
      teschke(i)=1
    end if
  end do

  if (teschke(i).eq.1) then
    WRITE (IKALYPSOFM, 7020) i, hhmin(i),hhmax(i)
    WRITE (IKALYPSOFM, 7021) i, (apoly(i,j), j=1,5)
    WRITE (IKALYPSOFM, 7022) i, (apoly(i,j), j=6,10)
    WRITE (IKALYPSOFM, 7023) i, (apoly(i,j), j=11,13)
    WRITE (IKALYPSOFM, 7024) i, qgef(i),(qpoly(i,j), j=1,4)
    WRITE (IKALYPSOFM, 7025) i, (qpoly(i,j), j=5,9)
    WRITE (IKALYPSOFM, 7026) i, (qpoly(i,j), j=10,13)
    WRITE (IKALYPSOFM, 7027) i, hbordv(i)
    WRITE (IKALYPSOFM, 7028) i, alphah(i),(alphad(i,j), j=1,4)
    WRITE (IKALYPSOFM, 7029) i, (alphapk(i,j), j=1,5)
    WRITE (IKALYPSOFM, 7030) i, (alphapk(i,j), j=6,10)
    WRITE (IKALYPSOFM, 7031) i, (alphapk(i,j), j=11,13)
    WRITE (IKALYPSOFM, 7032) i, betah(i),(betad(i,j), j=1,4)
    WRITE (IKALYPSOFM, 7033) i, (betapk(i,j), j=1,5)
    WRITE (IKALYPSOFM, 7034) i, (betapk(i,j), j=6,10)
    WRITE (IKALYPSOFM, 7035) i, (betapk(i,j), j=11,13)
  end if

END DO write_nodes
                                                                        
! Arcs:
write_arcs: DO i = 1, arccnt
  WRITE (IKALYPSOFM, 7001) i, (arc_tmp (i, j), j = 1, 5)
end do write_arcs

!NiS,apr06: the array-value of fehler(2,i) is not so important for the displaying of results at first; later it might be interesting to insert the option
!           of displaying the error in results file. At the moment this option is not used. In addition to that the format descriptor at the end is replaced.

! Elements:
write_elements: DO i = 1, ne
  WRITE (IKALYPSOFM, 7002) i, imat (i), imato (i), nfixh (i) !, fehler (2, i)
END do write_elements

!NiS,may06: Write informations of
! 1D-2D-TRANSITION ELEMENTS

write_trans_els: do i = 1, MaxT
  write (IKALYPSOFM,7014) (trans_nodes(i,k),k=1,6)
ENDDO write_trans_els

! Roughness classes:
write_roughness: DO i = 1, irk
  WRITE (IKALYPSOFM, '(a)') rk_zeile (i)
END do write_roughness

!NiS,apr06: unit name changed; replaced 12 by IKALYPSOFM:
!      CLOSE (12)
      CLOSE (IKALYPSOFM, STATUS='keep')
!-


!--------------- Formatdefinition: -----------------------------
 2000 format (1X, 'Opening output file failed. (IOSTAT =',I2,')',/ &
     &        1X, 'Stopping Program...')

!                              Name des Restartfiles,
!                              von dem dieser Lauf gestartet wurde:     
 7009 FORMAT ('RS',A)
                                                                        

!NiS,apr06: Adding new option for saving the date in RMA10S form (Hour in year and year)
!                              actual time starting the restart:
!                              IYRR: calculation year
!                              TETT: time in year
 7012 FORMAT ('DA',i10, f20.7)
!-

!                              aktueller Zeitpunkt und Schrittzaehler:  
 7008 FORMAT ('TI',f20.7,i10)
                                                                        
!                              Knoten; Nummer und raeumliche Lage (x,y,z
 7000 FORMAT ('FP', i10,3f20.7)

 !EFa Dec06, neues Format für Ausgabe der Knotendaten, wenn Kilometrierung vorhanden
 7036 FORMAT ('FP', i10,4f20.7)

!                              aktuelle Freiheitsgrade; Knotennummer,   
!                              x-, y-Geschwindigkeit, Wasserspiegel):   
 7003 FORMAT ('VA', i10,4f20.7)

!                              aktuelle Zeitgradienten:                 
 7004 FORMAT ('GA', i10,3f20.7)
                                                                        
!                              Freiheitsgrade des vergangenen Zeitschrit
!                              (Geschwindigkeiten und Wasserspiegel):   
 7005 FORMAT ('VO', i10,3f20.7) 
                                                                        
!                              Zeitgradienten des vergangenen Zeitschrit
 7006 FORMAT ('GO', i10,3f20.7) 
                                                                        
!                              Zusatzinformationen am Knoten:           
!                                                                       
 7007 FORMAT ('ZU',i10,i6,4f15.7)


!                              Kanten; Nummer,Knoten-unten Knoten-oben  
!                                     ,Element-links,Element-rechts     
!                                     ,Mittseitenknoten:                
 7001 FORMAT ('AR', 6i10) 
                                                                        
!                              Elemente; Nummer, Rauhigkeitsklase_urspru
!                                        Rauhigkeitsklase_im Zeitschritt
!                                        Bearbeitungsreihenfolge (nfixh)
!                                        Fehlermass2                    

!NiS,apr06:     Because of excluding the array-value of fehler(2,i) the
!               fomat descriptor must change (see above)
! 7002 FORMAT ('FE', 4i10,f15.7) 
 7002 FORMAT ('FE', 4i10)

!NiS,may06: new identification line for cross sectional informations:
!                              node, width, ss1, ss2, wids, widbs, wss
 7013 FORMAT ('CS',i10,f10.1,2f10.3,3f10.2)
!-

!NiS,may06: Add 1D-2D-TRANSITION ELEMENTS INFORMATIONS
!                              Elementnumber, 1st node, midside node, 2nd node
!                              (at same time midside of 2D-element arc), right
!                              corner node of 2D-element arc, left corner node
!                              of 2D-element arc
 7014 FORMAT ('TE', 6i10)
!-

!NiS,may06: Calculation does only properly work with all degrees of freedom, read in
!                              Freiheitsgrade 4 bis 7; Knotennummer,
!                              salinity, temperature etc.):
 7015 FORMAT ('DF', i10,4f20.7)
!-

!EFa Dec06, neue Formate für 1d-Teschke-Elemente
 7020 FORMAT ('MM', i10,2f20.7)

 7021 FORMAT ('AP1', i9,5f20.7)

 7022 FORMAT ('AP2', i9,5f20.7)

 7023 FORMAT ('AP3', i9,3f20.7)

 7024 FORMAT ('QP1', i9,5f20.7)

 7025 FORMAT ('QP2', i9,5f20.7)

 7026 FORMAT ('QP3', i9,4f20.7)

 7027 FORMAT ('HB', i10,f20.7)

 7028 FORMAT ('AD', i10,5f20.7)

 7029 FORMAT ('AK1' ,i9,5f20.7)

 7030 FORMAT ('AK2' ,i9,5f20.7)

 7031 FORMAT ('AK3' ,i9,3f20.7)

 7032 FORMAT ('BD', i10,5f20.7)

 7033 FORMAT ('BK1' ,i9,5f20.7)

 7034 FORMAT ('BK2' ,i9,5f20.7)

 7035 FORMAT ('BK3' ,i9,3f20.7)



!--------------- deallocation section -----------------------------
!NiS,apr06      to clean the memory, the arrays, allocated and only used
!               in this subroutine, are deallocated again.
      DEALLOCATE  (arc_tmp, arcmid)
!-

      RETURN

      END SUBROUTINE write_KALYPSO


