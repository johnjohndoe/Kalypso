!     Last change:  MD   14 Jan 2009   10:55 am
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
SUBROUTINE write_KALYPSO (nameout, resulttype)
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
!EFa Dec06, neues Modul f�r 1d-Teschke-Elemente
USE Para1DPoly
USE ParaKalyps
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
INTEGER              :: DefLine                        !Number of lines, that need to be written for a continuity line definition
INTEGER              :: lile, Rest, Startnode, NoLines

!NiS,apr06: changing length of variable, because the length of filenames in RMA10S is 96.
CHARACTER (LEN = 96), INTENT (IN) :: nameout !, namein
CHARACTER (LEN =  4), INTENT (IN) :: resulttype

INTEGER              :: istat                          !Variable for I/O errors
!NiS,apr06: Adding missing declarations for good programming practice:
INTEGER              :: nodecnt                        !nodenumber in network
INTEGER              :: iicyc                          !local variable for intermediat saving original time step number

!NEW DECLARATIONS FOR 1D-2D-TRANSITION ELEMENTS
INTEGER, ALLOCATABLE :: Trans_nodes(:,:)               !informations for writing specific informations about transition elements
INTEGER              :: trans_els                      !counter and name-giver for transition-elements

CHARACTER (LEN = 1000) :: dataline

!nis,may07
!Add midside node for polynom approach
!!NiS,apr06: allocating the two local arrays for arc-handling with the size of MaxP
!!ALLOCATE (arc_tmp (maxp,5), arcmid(LPPoly:maxp,5))            !EFa Dec06, Spaltenanzahl von arcmid f�r 1d-Teschke-Elemente auf 5 erh�ht
ALLOCATE (arc_tmp (maxp,5), arcmid(1:maxp,4))
!Add midside node for polynom approach
!-
!NiS,may06: allocate 1D-2D-TRANSITION ELEMENTS array (1: number; 2-6: nodes)
ALLOCATE (Trans_nodes(MaxT,6))

! INITIALIZING ---------------------------------------------------------

!NiS,may06: initializing the trans_els variable
trans_els = 0


!--------------- Umwandlung in Kantenstruktur --------------------------
                                                                        
nodecnt = np
elcnt = ne

DO i = 1, np
  DO j = 1, 5
    !NiS,apr06: Changed arc to arc_tmp because of global conflict
    arc_tmp (i, j) = 0
  END DO
END DO

!nis,may07
!Add midside node for polynom approach
DO i = 1, np
  DO j = 1, 4
    arcmid  (i, j) = 0
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
          if ( (.not. IntPolProf (nop (nelem, 1))) .and. (.not. IntPolProf (nop (nelem, 3)))) then
            !!midside node
            k = nop (nelem, 2)

            arcmid (k, 1) = nop (nelem, 1)
            arcmid (k, 2) = nop (nelem, 3)
            arcmid (k, 3) = nelem
            arcmid (k, 4) = nelem
          elseif ( (.not. IntPolProf (nop (nelem, 1))) .and. (IntPolProf (nop (nelem, 3)))) then
            k = nop (nelem, 2)
            arcmid (k, 1) = nop (nelem, 1)
            arcmid (k, 2) = nop (IntPolElts (nelem, IntPolNo (nelem)), 3)
            arcmid (k, 3) = nelem
            arcmid (k, 4) = nelem
          end if

          !Save informations for 1D-2D-TRANSITION ELEMENTS
          IF (nnum .eq. 5) THEN

            trans_els = trans_els + 1

            !ERROR - too many elements
            IF (trans_els .gt. maxp) call ErrorMessageAndStop (1206, 0, 0.0D0, 0.0D0)

            Trans_nodes(trans_els, 1) = nelem
            DO i = 2,6
              Trans_nodes(trans_els, i) = nop(nelem,i-1)
            ENDDO
          ENDIF

        !2D-ELEMENTS
        ELSE
          DO 301 j = 2, nnum, 2
            !midside node of an arc
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

!nis,may07
DO i = 1, np
  IF (arcmid (i, 3) .ne.0) then
    arccnt = arccnt + 1
    DO j = 1, 4
      !Save 1st and 2nd node as well as elements associated to the arc
      !NiS,apr06: Changed arc to arc_tmp because of global conflict
      arc_tmp (arccnt, j) = arcmid (i, j)
    END DO
    arc_tmp (arccnt, 5) = i
  ENDIF
END DO


!NiS,apr06: Write informational the generated output name; Changed iout to Lout
WRITE ( * , * ) ' '
WRITE ( * , * ) 'Name der Modell-Ausgabedatei: ', TRIM(nameout)
WRITE (Lout, * ) ' '
WRITE (Lout, * ) 'Name der Modell-Ausgabedatei: ', TRIM(nameout)
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
!WP string = 'RS'//namein
!WP WRITE (12, '(a80)') string
!NiS,may06: Giving out the Geometryfile and Restartfile; the length of the filenames are maximum 50 digits
!write (IKALYPSOFM, 7011) namein
!7011 format ('RS', A80)
!write (IKALYPSOFM, 7011) modellein, modellrst
!7011 format ('RS', 2A40)
!-

!WP Problem with length of title-character (78 or 80 ?)
!WP See SUB output and SUB modell_lesen
!WP Sometimes the calculation crashes after > 100 time steps
!WP Changing the format of output!
write (IKALYPSOFM, 7010) title
7010 format ('00', A80)

!WP 01.09.2004

! Time + step counter
!NiS,may06: nitsv in Kalypso-2D has the same meaning as icyc in RMA10S; changed nitsv to icyc
if (resultType == 'resu') then
  WRITE (IKALYPSOFM, 7008) tet, icyc
end if
!-

!NiS,apr06: writing RMA10S-date format
TETT=(DAYOFY-1)*24.+TET
WRITE (IKALYPSOFM, 7012) iyrr, tett

! Node: Coordinates + Degrees of freedoms + Gradients + others:
write_nodes: DO i = 1, np

  !NiS,may06: The cord array is initialized with -1.e20, so every node with coordinates less than -1e19 that are skipped for writing
  IF (cord(i,1) .lt. -1.e19) CYCLE write_nodes

  if (.not. (IntPolProf (i)) .and. kmx (i) /= -1.0) then
    write (IKALYPSOFM, 6999) i, cord (i, 1), cord (i, 2), ao(i), kmx (i)  !EFa Dec06, Ausgabe der Kilometrierung, wenn vorhanden
  ELSEIF (.not. (IntPolProf (i)) .and. kmx (i) == -1.0) then
    WRITE (IKALYPSOFM, 7000) i, cord (i, 1), cord (i, 2), ao(i)
  !for interpolated profiles
  ELSEIF (IntPolProf (i) .AND. kmx (i) /= -1.0) then
    write (IKALYPSOFM, 7043) i, cord (i, 1), cord (i, 2), ao(i), kmx (i)  !EFa Dec06, Ausgabe der Kilometrierung, wenn vorhanden
  ELSEIF (IntPolProf (i)) then
    WRITE (IKALYPSOFM, 7044) i, cord (i, 1), cord (i, 2), ao(i)
  endif

  if (resultType == 'resu') THEN
    if (.not. (IntPolProf (i)) ) then
      WRITE (IKALYPSOFM, 7003) i, (vel (j, i), j = 1, 3) , rausv (3, i)
      !NiS,may06: All degrees of freedom have to be written and read for restart
      WRITE (IKALYPSOFM, 7015) i, (vel (j, i), j = 4, 7)
    else
      WRITE (IKALYPSOFM, 7045) i, (vel (j, i), j = 1, 3) , rausv (3, i)
    end if
  ELSEIF ( resultType == 'mini') THEN
    WRITE (IKALYPSOFM, 7003) i, (minvel (j, i), j = 1, 3) , minrausv (i)
  ELSEIF (resultType == 'maxi') THEN
    WRITE (IKALYPSOFM, 7003) i, (maxvel (j, i), j = 1, 3) , maxrausv (i)
  ENDIF


  !only for real results not for minmax-result-files
  !if (resultType == 'resu') then
  if (.not. IntPolProf (i)) then
    IF (icyc .ne. 0) then
      WRITE (IKALYPSOFM, 7004) i, (vdot (j, i), j = 1, 3)
      WRITE (IKALYPSOFM, 7005) i, (vold (j, i), j = 1, 3)
      WRITE (IKALYPSOFM, 7006) i, (vdoto (j, i), j = 1, 3)
    ENDIF

    WRITE (IKALYPSOFM, 7007) i, ndry (i), hel (i), hol (i), hdet (i), hdot (i)

    !NiS,may06: Write cross sectional node information, if present
    IF (width (i) .ne. 0) THEN
      WRITE (IKALYPSOFM, 7013) i, width(i), ss1(i), ss2(i), wids(i), widbs(i), wss(i)
    END IF
  !end if


    if (IsPolynomNode (i) .and. (.not. IntPolProf (i))) then
      !WRITE (IKALYPSOFM, 7020) i, hhmin(i),hhmax(i)
      !WRITE (IKALYPSOFM, 7021) i, hbordv(i)

      WRITE (dataline, *) 'PRA', i, polySplitsA(i), hhmin(i), (polyrangeA(i, k), k=1, polySplitsA(i))
      WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
      WRITE (dataline, *) 'PRQ', i, polySplitsQ(i), hhmin(i), (polyrangeQ(i, k), k=1, polySplitsQ(i))
      WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
      if (PolySplitsB(i) /= 0) then
        WRITE (dataline, *) 'PRB', i, polySplitsB(i), hhmin(i), (polyrangeB(i, k), k=1, polySplitsB(i))
        WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
      endif

      do j = 1, PolySplitsA(i)

        WRITE (dataline, *) 'AP ', i, j, apoly(j, i, :)
        WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
      end do
      do j = 1, PolySplitsQ(i)
        WRITE (dataline, *) 'QP ', i, j, qgef(i), qpoly(j, i, :)
        WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
      end do
      do j = 1, PolySplitsB(i)
        WRITE (dataline, *) 'ALP', i, j, alphapoly(j, i, :)
        WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
        WRITE (dataline, *) 'BEP', i, j, betapoly(j, i, :)
        WRITE (IKALYPSOFM, '(a)') dataline (2: LEN (TRIM (dataline)))
      end do
    end if
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
  !for weir elements
  if (imat(i) >= 901 .and. imat(i) <= 903) then
    WRITE (IKALYPSOFM, 7019) i, (nop(i, j), j= 1, ncorn(i))
  !for 1D or 2D elements; interpolated elements are excluded; no necessary informations
  ELSE
    if (imat (i) /= 89) then

        WRITE (IKALYPSOFM, 7002) i, imat (i), imato (i), nfixh (i) !, fehler (2, i)

      !write material types and reordering number
      if (imat(i) .gt. 903 .and. imat(i) .lt. 990) then
        BACKSPACE(IKALYPSOFM)
        WRITE (IKALYPSOFM, 7016) i, imat (i), imato (i), nfixh (i), nop(i,1)
      end if
      !write roughness corrections
      if (CorrectionKS(i) /= 1.0 .or. CorrectionAxAy(i) /= 1.0 .or. CorrectionDp(i) /= 1.0) then
        WRITE (IKALYPSOFM, 7017) i, CorrectionKS(i), CorrectionAxAy(i), CorrectionDp(i)
      end if
    !write number of profiles to interpolate in between
    elseif (imat (i) == 89 .and. (.NOT.(IntPolProf (nop (i, 1))))) then

      WRITE (IKALYPSOFM, 7002) i, imat (i), imato (i), nfixh (i)

      write (IKALYPSOFM, 7046) i, IntPolNo (i)
    end if

    !only for real results, not for minmax-results
    if (resultType == 'resu') then
      WRITE (IKALYPSOFM, 7018) i, lambdaTot(i), lambdaKS(i), lambdaP(i), lambdaDunes(i)
    end if
  end if
END do write_elements

!NiS,may06: Write informations of
! 1D-2D-TRANSITION ELEMENTS

write_trans_els: do i = 1, MaxT
  write (IKALYPSOFM,7040) (trans_nodes(i,k),k=1,6)
ENDDO write_trans_els

!nis,apr07: write informations of transition lines
write_trans_lines: do i = 1, MaxLT
  WRITE(IKALYPSOFM, 7041) i, (translines(i,j), j = 1, 3)
end do write_trans_lines

write_PipeSurfConn: do i = 1, MaxPS
  write(IKALYPSOFM, 7049) i, PipeSurfConn(i)%SurfElt, PipeSurfConn(i)%PipeElt
enddo write_PipeSurfConn

! Roughness classes:
write_roughness: DO i = 1, irk
  WRITE (IKALYPSOFM, '(a)') rk_zeile (i)
END do write_roughness

!NiS,apr06: unit name changed; replaced 12 by IKALYPSOFM:
CLOSE (IKALYPSOFM, STATUS='keep')
!-


!--------------- Formatdefinition: -----------------------------
 2000 format (1X, 'Opening output file failed. (IOSTAT =',I2,')',/ &
     &        1X, 'Stopping Program...')


 !EFa Dec06, neues Format f�r Ausgabe der Knotendaten, wenn Kilometrierung vorhanden
 6999 FORMAT ('FP', i10,4f20.7)
 !Knoten; Nummer und raeumliche Lage (x,y,z
 7000 FORMAT ('FP', i10,3f20.7)

 !Kanten; Nummer,Knoten-unten Knoten-oben, Element-links,Element-rechts, Mittseitenknoten:
 7001 FORMAT ('AR', 6i10) 
                                                                        
 !Elemente; Nummer, Rauhigkeitsklase_ursprung; Rauhigkeitsklase_im Zeitschritt; Bearbeitungsreihenfolge (nfixh); Fehlermass2
 !NiS,apr06: Because of excluding the array-value of fehler(2,i) the fomat descriptor must change (see above)
 !7002 FORMAT ('FE', 4i10,f15.7)
 7002 FORMAT ('FE', 4i10)

 !aktuelle Freiheitsgrade; Knotennummer, x-, y-Geschwindigkeit, Wasserspiegel):
 7003 FORMAT ('VA', i10, 2f20.14, 2f20.13)
 
 !aktuelle Zeitgradienten:
 7004 FORMAT ('GA', i10,3f20.7)
                                                                        
 !Freiheitsgrade des vergangenen Zeitschritt (Geschwindigkeiten und Wasserspiegel):
 7005 FORMAT ('VO', i10,3f20.7) 
                                                                        
 !Zeitgradienten des vergangenen Zeitschrit
 7006 FORMAT ('GO', i10,3f20.7) 
                                                                        
 !Zusatzinformationen am Knoten:
 7007 FORMAT ('ZU',i10,i6,4f15.7)

 !aktueller Zeitpunkt und Schrittzaehler:
 7008 FORMAT ('TI',f20.7,i10)

 !Name des Restartfiles, von dem dieser Lauf gestartet wurde:
 7009 FORMAT ('RS',A)

 !NiS,apr06: date in RMA10S form; actual time starting the restart: IYRR - calculation year; TETT - time in year
 7012 FORMAT ('DA',i10, f20.7)

 !NiS,may06: new identification line for cross sectional informations: node, width, ss1, ss2, wids, widbs, wss
 7013 FORMAT ('CS',i10,f10.1,2f10.3,3f10.2)

 !NiS,may06: rest of degrees of freedom for 3D and constituents, read in Freiheitsgrade 4 bis 7; Knotennummer, salinity, temperature etc.):
 7015 FORMAT ('DF', i10,4f20.7)

 !LF nov06: adding starting node for weir structure
 7016 FORMAT ('FE', 5i10)

 !nis,aug07: writing roughness correction layer data
 7017 FORMAT ('RC', i10, 3(f10.6))

 !nis,aug07: writing out flow resistance results for elements
 7018 FORMAT ('FR', i10, 4f15.7)

 !nis,aug07: writing out 1D junction elements
 7019 FORMAT ('JE', i10, 8i10)

 !min-max-range of waterstages for 1D-polynom
 7020 FORMAT ('MM', i10,2f20.7)

 !bord full elevation for polynom approach to divide flow coeficient polynoms
 7021 FORMAT ('HB', i10,f20.7)

 !NiS,may06: 1D-2D-Transition Element: Elementnumber, 1st node, midside node, 2nd node
 !(at same time midside of 2D-element arc), right corner node of 2D-element arc, left corner node of 2D-element arc
 7040 FORMAT ('TE', 6i10)

 !nis,apr07: 1D-2D-Transition line: 1D-element, Continuity line, coupling 1D node
 7041 FORMAT ('TL', 4i10)

 !nis,apr07: continuity line definition
 7042 FORMAT ('CC', i1, 9i8)

 !nis,jan08: interpolated profiles in 1D polynomial approach
 7043 FORMAT ('IN', i10, 4f20.7)
 !nis,jan08: interpolated nodes in 1D polynomial approach
 7044 FORMAT ('IN', i10, 3f20.7)
 !nis,jan08: results of interpolated nodes or profiles; like VA-line
 7045 FORMAT ('IR', i10,4f20.7)
 !nis,jan08: no. of interpolated profiles
 7046 FORMAT ('IP', 2i10)
 !nis,jan08: for Calculation units
 7048 format ('CU', 2i10, '(a128)')
 !nis,feb09: for pipe surface connections
 7049 format ('PS', 3i10)


!--------------- deallocation section -----------------------------
!NiS,apr06      to clean the memory, the arrays, allocated and only used
!               in this subroutine, are deallocated again.
DEALLOCATE  (arc_tmp, arcmid)
!-

RETURN

END SUBROUTINE write_KALYPSO
!**********************************************************


SUBROUTINE write_KALYP_Bed (bedout)

!MD: character (LEN = 96), INTENT (inOUT) :: resultBed, inputBed
USE BLK10mod
USE BLK11mod
USE PARAKalyps
USE BlkDRmod
USE BLKSEDMOD
USE BLKSANMOD

! local variables
CHARACTER*256 LINE256
CHARACTER (LEN = 96), INTENT (IN) :: bedout
INTEGER:: IDX
INTEGER:: istat      !Variable for I/O errors
REAL(KIND=8):: SUMTHICK

! ----------------------

!NiS,apr06: Write informational the generated bed output name; Changed iout to Lout
WRITE ( * , * ) ' '
WRITE ( * , * ) 'Name der Modell-Ausgabedatei: ', TRIM(bedout)
WRITE (Lout, * ) ' '
WRITE (Lout, * ) 'Name der Modell-Ausgabedatei: ', TRIM(bedout)
!-

! WRITING PART --------------------------------------------------------------------------
! Writing results in file
OPEN (IKALYPSOFM, FILE = bedout, FORM = 'formatted', STATUS = 'REPLACE', IOSTAT = istat)
if (istat /= 0) then
  WRITE (*,3000) istat
  stop
end if


WRITE(LINE256,'(A)') '  Node Bed Shear Bed-elev   SedMass  SumLayer SusLayer-Thickness '
IDX = MAX(65,47+NLAYT*10) + 3

IF (NLAYO(1) .GT. 0) THEN
  WRITE(LINE256(IDX:IDX+26),'(A)') 'BedLayer-Thickness (mm) '
ENDIF
WRITE(IKALYPSOFM,'(/A)') LINE256(1:IDX+26)


WRITE(LINE256,'(A,12I10)') '          (N/m2)    (m)       (Kg/m2)   (mm) ',&
&                        (L,L=1,NLAYT),(L,L=1,NLAYO(1))
WRITE(IKALYPSOFM,'(A)') LINE256



DO NN=1,NPM
  NLAYT=NLAYTND(NN)
  !MD: Berechnung Gesamt-Layerdicke
  SUMTHICK = 0.
  DO L=1,NLAY(NN)
    SUMTHICK = SUMTHICK + THICK(NN,L)
  ENDDO

  IF(NLAYO(NN) .GT. 0) THEN
    DO L=1,NLAYO(NN)
      SUMTHICK = SUMTHICK + THICKO(NN,L)
    ENDDO
  ENDIF

  !MD: Ausgabe der Layerdicken
  IF (NLAYT.GT.0 .OR. NLAYO(NN).GT.0) THEN
    MLAYRS = NLAY(NN)
    WRITE(IKALYPSOFM,'(I5,F10.4,F10.6, F10.2, 20F10.3)')  &
       &          NN, BSHEAR(NN), AO(NN), TMSED(NN), SUMTHICK*1000.,&
       &             (1000. * THICK(NN,L),L=1,NLAYT),&
       &             (1000. * THICKO(NN,L),L=1,NLAYO(NN))
  ENDIF
Enddo


CLOSE (IKALYPSOFM, STATUS='keep')

!--------------- Formatdefinition: -----------------------------
3000 format (1X, 'Opening output file failed. (IOSTAT =',I2,')',/ &
     &        1X, 'Stopping Program...')



END SUBROUTINE write_KALYP_Bed


!**********************************************************

SUBROUTINE GenerateOutputFileName (sort, niti_local, timeStep, iteration, outsuffix, inname, rstname, prefix, restartunit, &
           &                   resultName, inputName, resultBed, inputBed)

implicit none
INTEGER, INTENT (IN) :: timeStep, iteration, restartUnit
INTEGER, INTENT (IN) :: niti_local
character (LEN = 96), INTENT (inOUT) :: resultName, inputName
CHARACTER (LEN = 32), INTENT (IN)  :: inname, rstname
character (len = *) , intent (in) :: outsuffix
CHARACTER (LEN = 1), INTENT (IN)   :: prefix
CHARACTER (LEN = 4), INTENT (IN)   :: sort
!MD: new for kohesive Bed
character (LEN = 96), INTENT (OUT) :: resultBed, inputBed

! ------------------------------------------------------------------------------------------
! NiS,may06: Creation of output/solution file names has a little bit changed in logic order,
!            because there were some problems with steady state and results after iteration
!            The old version is completly deleted, it can be read in the code of Kalypso-2D.
      !after solution is converged or maximum number of iterations is reached
if (sort == 'inst' .or. sort == 'stat') then
  IF (iteration == 0) THEN

    !for steady states
    IF (timeStep == 0) THEN

      !output after steady state solution
      WRITE (resultName,'(a,a1,a)') 'steady', '.', outsuffix
      !MD: new for kohesive Bed
      WRITE (resultBed,'(a,a1,a)') 'steady', '.', 'bed'

      !if restarting, then particular name is used, otherwise not
      IF (restartUnit > 0) THEN
        WRITE (inputName,'(a)') rstname
        !MD: new for kohesive Bed
        WRITE (inputBed,'(a)') 'restart', '.', 'bed'
      ELSE
        WRITE (inputName,'(a)') 'none, new 2D-Calculation'
        WRITE (inputBed,'(a)') 'none, new 2D-Calculation'
      ENDIF

    !for dynamic solution after calculated time step, that is not the first in current run
    ELSE
      !output file
      WRITE (inputName,'(a,i4.4,a1,a)')  prefix, timeStep, '.', outsuffix
      !Starting from the solution of time step before
      WRITE (resultName,'(a,i4.4,a1,a)') prefix, timeStep, '.', outsuffix

      !MD: new for kohesive Bed
      WRITE (inputBed,'(a,i4.4,a1,a)')  prefix, timeStep, '.', 'bed'
      WRITE (resultBed,'(a,i4.4,a1,a)') prefix, timeStep, '.', 'bed'
    ENDIF

  !after iteration step, if wanted
  ELSE
    !input file name of output after iteration is not relevant
    WRITE (inputName,'(a)') 'no regular result file, just iteration output'
    IF (timeStep == 0) THEN
      !output filename after iteration in steady loop
      WRITE (resultName,'(a,i3.3,a1,a)') 'steady_Ite_', iteration, '.', outsuffix
    ELSE
      !output filename after iteration in dynamic loop at time step (timeStep)
      WRITE (resultName,'(a,i4.4,a,i3.3,a1,a)') prefix, timeStep,'_Ite',iteration,'.', outsuffix
    ENDIF
  ENDIF
!min-max- of time transient calculation
ELSEIF (sort == 'mini' .or. sort == 'maxi') then
  WRITE (resultName, '(a4,a1,a2)') sort, '.', outsuffix
  WRITE (inputName, '(a)') 'min-max-file; no other information'
ENDIF
!-

end subroutine

