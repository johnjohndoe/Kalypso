!     Last change:  MD    6 Jan 2010    2:43 pm
!--------------------------------------------------------------------------
! This code, zeila.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - zeila
!
! Copyright (C) 2004  ULF TESCHKE & WOLF PLOEGER.
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
! Ulf Teschke:      phone: +49 40 42878 3895 mail: teschke@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 18 August 2004
! Research Associate
!***********************************************************************



! ----------------------------------------------------------------
SUBROUTINE zeila (unit5, nprof, pfad2, NAME_OUT_LAENGS, NAME_OUT_QLAENGS)
! SUBROUTINE zeila (unit5, nprof, pfad2, mark, NAME_OUT_LAENGS)
!
! geschrieben:  p.koch     01.08.1989
!                                                                       
! geaendert:    w. ploeger 10.05.2005
!               w. ploeger 26.04.2006
!                                                                       
! ---------------------------------------------------------------
!                                                                       
! Dieses unterprogramm wird benutzt, um plotgerechte ausgabefiles
! der laengsschnitte zu erzeugen:
!
! - bei wasserspiegellagenberechnung mit konstantem q:   .wsl-file  (Blockformat)
!                                                        laengsschnitt.txt (Tabelle)
! - bei bordvoll-berechnung (schrittweise variiertes q): .wsp-file
!                                                        .qbv-file
! ---------------------------------------------------------------


!WP 01.02.2005
USE DIM_VARIABLEN
USE ZEIT
USE AUSGABE_LAENGS
USE KONSTANTEN
USE IO_UNITS
USE MOD_ERG
USE MOD_INI
                                                                        
!WP -- Calling Variables -------------------------------------------------------------------------------------
CHARACTER(LEN=nch80), INTENT(IN) :: pfad2	! Pfad- und Dateinamen für zu erstellende Dateien
CHARACTER(LEN=nch80), INTENT(IN) :: unit5       ! Pfad- und Dateinamen für Laengsschnittdatei (z.B. Fawl0001.001)
INTEGER, INTENT(IN) :: nprof                    ! Anzahl Profile
! INTEGER, INTENT(IN) :: mark                   ! Art der Berechnung:
						! 1 = Spiegellinie, bordvoll,gleich-/ungleichfoermig etc.

!ST 29.03.2005
!Variable für Laengschnitt.txt
CHARACTER(LEN=nch80), INTENT(IN) :: NAME_OUT_LAENGS     ! Pfad- und Dateinamen fuer Ergebnisstabelle \Dath\laengsschnitt.txt
CHARACTER(LEN=nch80), INTENT(IN) :: NAME_OUT_QLAENGS    ! Variable für Q_LangSchnitt.txt
!ST


! COMMON-Block /NR_AUS/ --------------------------------------------------------
! Commonblock fuer Profilnummerausgabe im *.wsl -File
CHARACTER(LEN = 1) :: nr_ausg
COMMON / nr_aus / nr_ausg
! -----------------------------------------------------------------------------



!WP -- Local variables -------------------------------------------------------------------------------------

CHARACTER(LEN=20) :: bete1   	! Fuer Ausdruck in WSP-Laengsschnittdatei
CHARACTER(LEN=20) :: bete12  	! Fuer Ausdruck in WSP-Laengsschnittdatei

INTEGER :: ierr  		! Fehlerkontrolle beim Schreiben in Datei

!WP 07.05.2004  Hilfsvariablen fuer die Block-Ausgabe
INTEGER :: vzeil, rest, m, n

INTEGER :: ianz  		! Anzahl der zu zeichnenden linien

INTEGER, DIMENSION (1:maxger) :: nopwx = 0
INTEGER, DIMENSION (1:maxger) :: nopwy = 0

!WP Ergaenzung
REAL, DIMENSION (maxger) :: yss = 0.0

INTEGER :: lityp (15), nzeiyl (15), nzeist (15), nzeiho (15)
INTEGER :: wnull, weins, w7, w6, w5, w9
INTEGER :: ischacht (maxger)


w9 = 0
wnull = 0
weins = 1
                                                                        

! Die Datei Laengsschnitt.txt wird nur erzeugt, falls mark = 1 (normale Spiegellinienberechnung)
!MD  if (mark == 1) then
!MD  Laengsschnitt wird auch fuer mark = 2 (stat. ungleichf. Berechnung) erzeugt
!MD  if (mark.eq.1 .or. mark.eq.2) then
!MD:  If (BERECHNUNGSMODUS /= 'BF_UNIFORM' .AND. FLIESSGESETZ /= 'MANNING_STR') then
If (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

  UNIT_OUT_LAENGS = ju0gfu ()  ! Holen einer neuen Dateinummer

  ! Oeffnen der laengsschnitt.txt-Datei
  open (unit = UNIT_OUT_LAENGS, file = NAME_OUT_LAENGS, status = 'REPLACE', ACTION='WRITE', IOSTAT = ierr)
  IF (ierr /= 0) then
    WRITE (*,*) 'ZEILA: Fehler beim Oeffnen von Laengsschnitt.txt'
    GOTO 9801
  ENDIF                                                                                               

end if


!HB   Holen einer neuen Dateinummer                                     
UNIT_OUT_QB1 = ju0gfu ()
!write (*,*) 'In ZEILA. unit5 = ', unit5
                                                                        
OPEN (unit = UNIT_OUT_QB1, file = unit5, status = 'REPLACE', IOSTAT = ierr)
IF (ierr /= 0) then
     PRINT * , 'ZEILA: Fehler beim Oeffnen von ', unit5
     GOTO 9801
ENDIF
                                                                        

!HB   ** Nur bei ungleichfoermiger Bordvollberechnung ***************** 
!HB   Oeffnen der Datei *.qb2, damit in diese geschrieben werden kann   
If (BERECHNUNGSMODUS == 'BF_NON_UNI' .or. BERECHNUNGSMODUS == 'REIB_KONST' .OR. BERECHNUNGSMODUS == 'BF_UNIFORM') then
!MD  IF (mark.eq.2.or.mark.eq.4) then

  UNIT_OUT_QB2 = ju0gfu ()
  !write (*,*) 'In ZEILA. pfad2 = ', pfad2
  OPEN (unit = UNIT_OUT_QB2, file = pfad2, status = 'REPLACE', IOSTAT = ierr)
  IF (ierr /= 0) then
    PRINT * , 'ZEILA: Fehler beim Oeffnen von ', pfad2
    GOTO 9801
  ENDIF

ENDIF                                                                   


!********************************************************************** 

nprof2 = 2 * nprof + 1              ! Doppelte Profilanzahl + 1
nschacht = 0
                                                                        
DO i = 1, nprof                                        ! ST: Falls es einen minimalen Hoehenunterschied zwischen
  IF (abs (hming1 (i) - sohl1 (i) ) .gt.1.e-06) then   ! der minimalen Gelaendehoehe und der minimalen Sohlhoehe
    IF (i.lt.nprof) then                               ! gibt und die Profile mindestens einen Meter auseinander-
      iF (abs (xss (i + 1) - xss (i) ) .gt.1.) then    ! liegen, erhalten nschacht und ischacht Werte, ansonsten
        nschacht = nschacht + 1                        ! sind sie gleich null = Abbruchkriterium fuer spaetere
        ischacht (nschacht) = i                        ! DO-Schleifen
      ENDIF
    ELSE
      nschacht = nschacht + 1
      ischacht (nschacht) = i
    ENDIF
  ENDIF
END DO
                                                                        
! ST 30.03.2005
!---Start--- Kopf der *.wsl-Datei
WRITE (UNIT_OUT_QB1, 5) unit5
WRITE (UNIT_OUT_QB1, 5) ' '
WRITE (UNIT_OUT_QB1, 5) ' '
WRITE (UNIT_OUT_QB1, 5) ' '
WRITE (UNIT_OUT_QB1, 5) ' '
WRITE (UNIT_OUT_QB1, 5) ' '

!MD  IF (mark.eq.1) then
If (BERECHNUNGSMODUS == 'WATERLEVEL') then
  WRITE (UNIT_OUT_QB1, '(a)') 'SPIEGELLINIEN-LAENGSSCHNITT '
  WRITE (UNIT_OUT_QB1, 5) ' '
!MD ELSEIF (mark.eq.2) then
ElseIf (BERECHNUNGSMODUS == 'BF_NON_UNI' .or. BERECHNUNGSMODUS == 'REIB_KONST') then
  WRITE (UNIT_OUT_QB1, '(a)') 'BORDVOLLER ABFLUSS'
  WRITE (UNIT_OUT_QB1, '(a)') 'STATIONAER-UNGLEICHFOERMIG'
!MD ELSEIF (mark.eq.4) then
ElseIf (BERECHNUNGSMODUS == 'BF_UNIFORM') then
  WRITE (UNIT_OUT_QB1, '(a)') 'BORDVOLLER ABFLUSS'
  WRITE (UNIT_OUT_QB1, '(a)') 'STATIONAER-GLEICHFOERMIG'
ENDIF

WRITE (UNIT_OUT_QB1, 5) ' '
WRITE (UNIT_OUT_QB1, 5) ' '
WRITE (UNIT_OUT_QB1, 7) MMTTJJ , HHMMSS

                                                                        
!HB***************************************                              
WRITE (UNIT_OUT_QB1, 6) 'B- 2 0 0 0 0 0'
WRITE (UNIT_OUT_QB1, 5) ' '
!---Ende--- Kopf der *.wsl-Datei
                                                                        

!MD  IF (mark.eq.1.or.mark.eq.3) then
!MD  Laengsschnitt wird auch fuer mark = 2 (stat. ungleichf. Berechnung) erzeugt
!MD  IF (mark.eq.1 .or. mark.eq.3 .or. mark.eq.2) then
If (BERECHNUNGSMODUS == 'WATERLEVEL' .or. BERECHNUNGSMODUS == 'BF_NON_UNI' .or. BERECHNUNGSMODUS == 'REIB_KONST') then
  yhmin = 10000.
  DO i = 1, nprof            ! minimale Sohlhoehe von allen Profilen
    IF (sohl1 (i) .lt.yhmin) then
      yhmin = sohl1 (i)
    ENDIF
  END DO

  yhmax = 0.
  DO i = 1, nprof            ! maximale Wasserspiegellage von allen Profilen
    hneu = max (boli1 (i), bore1 (i), wsp1 (i) )
    IF (hneu.gt.yhmax) then
      yhmax = hneu
    ENDIF
  END DO

  !  rundung auf fuenf-meter schritte

  ihmin = int (yhmin / 5.)
  yhmin = float (ihmin) * 5.0

  ihmax = int (yhmax / 5.)
  yhmax = float (ihmax) * 5.0

  IF (nschacht.gt.0) then
    IF (nr_ausg.eq.'n') then
      ianz = 10
      WRITE (UNIT_OUT_QB1, '(11i4)') ianz, (nprof, i = 1, ianz - 1)
    ELSE
      ianz = 11
      WRITE (UNIT_OUT_QB1, '(11i4)') ianz, (nprof, i = 1, ianz - 2)
    ENDIF
  ELSE
    IF (nr_ausg.eq.'n') then
      ianz = 8
      WRITE (UNIT_OUT_QB1, '(11i4)') ianz, (nprof, i = 1, ianz)
    ELSE
      ianz = 9
      WRITE (UNIT_OUT_QB1, '(11i4)') ianz, (nprof, i = 1, ianz - 1)
    ENDIF
  ENDIF

  !ST -------------------------------------------------------
  !ST 30.03.2005
  ! Do-Schleife für Übergabe Stationierung für write in laengsschnitt.txt
  DO i =1, nprof
    stat1(i) = (-1) * xss(i) / 1000
  END DO
  !ST -------------------------------------------------------

  ! Massstab festlegen
  xmass = 1000.0
  gx = 0.0
  bzgh = yhmin
  ymass = 100.0
  7002   gy = (yhmax - yhmin) / ymass * 100. + 7.0
  IF (gy.gt.29.7) then
    ymass = ymass * 5.0
    GOTO 7002
  ENDIF
  gy = 29.7
  yo = 0.0
  WRITE (UNIT_OUT_QB1, '(6f8.2,i2)') yo, gx, bzgh, xmass, ymass, gy, wnull

  !  ******************************************************************************
  !   mark=1: darstellungsform 1 - sohlhoehe, boe. links, boe. rechts, wsp bordvoll
  !             es entsteht ein .wsp-file
  !  ******************************************************************************

  imax = 13

  !ST -------------------------------------------------------
  !ST 30.03.2005
  ! Füge in die nachfolgende Do-Schleife Übergaben für write in laengsschnitt.txt ein
  !ST -------------------------------------------------------

  DO 204 j = 1, imax
    bete12 = ' '
    IF (j.eq.1) then
      IF (nschacht.eq.0) goto 204
      bete1 = 'Gelaende   mNN    '
      DO i = 1, nprof
           yss (i) = hming1 (i)
      END DO
    ELSEIF (j.eq.2) then
      bete1 = 'Sohlhoehe mNN     '
      DO i = 1, nprof
          yss (i) = sohl1 (i)
      END DO
    ELSEIF (j.eq.3) then
      IF (nschacht.eq.0) then
           bete1 = 'Bordvoll-Hoehe mNN '
      ELSE
           bete1 = 'Bordvoll-Hoehe/ mNN'
           bete12 = 'Rohrscheitel-Hoehe'
      ENDIF

      DO i = 1, nprof
           yss (i) = min (boli1 (i), bore1 (i) )
           !ST
           BV1 (i) = min (boli1 (i), bore1 (i) )
      END DO
    ELSEIF (j.eq.4) then
         GOTO 204
    ELSEIF (j.eq.5) then
      bete1 = 'Laenge     m    '
      DO i = 2, nprof
           dif = abs (xss (i - 1) - xss (i) )
           idif = int (dif * 2. + 0.5)
           yss (i - 1) = float (idif) / 2.
      END DO
      yss (nprof) = 0.
    ELSEIF (j.eq.6) then
      IF (nschacht.eq.0) goto 204
      bete1 = 'Rohrdurchmesser DN'
      DO i = 1, nprof
           IF (abs (hming1 (i) - sohl1 (i) ) .gt.1.e-06) then
                hhoch = min (boli1 (i), bore1 (i) )
                ys = (hhoch - sohl1 (i) ) * 1000.
                iy = int (ys + 0.5)
                yss (i) = iy
                IF (i.lt.nprof.and.abs (xss (i + 1) - xss (i) ) .lt.1.) then
                     yss (i) = 0
                ENDIF
           ELSE
                yss (i) = 0.0
           ENDIF
      END DO
    ELSEIF (j.eq.7) then
      bete1 = 'Gefaelle  0/00   '
      DO i = 2, nprof
           yss (i - 1) = gef1 (i) * 1000.
      END DO
      yss (nprof) = 0.
    ELSEIF (j.eq.8) then
      IF (nschacht.gt.0) then
           bete1 = 'Wasserspiegel mNN'
           bete12 = EREIGNISNAME(1:20)
           bete12 = 'Druckhoehe'
      ELSE
           bete1 = 'Wasserspiegel mNN    '
           bete12 = EREIGNISNAME (1:20)
      ENDIF
      DO i = 1, nprof
           IF (BERECHNUNGSMODUS == 'WATERLEVEL') then
             yss (i) = wsp1 (i)
             !ST
             !WSP (i) = wsp1 (i)
           ELSE
             yss (i) = hwsp1 (i)
             !ST
             !WSP (i) = hwsp1 (i)
           ENDIF
      END DO
    ELSEIF (j.eq.9) then
      bete1 = 'v-mittel  m/s    '
      IF (BERECHNUNGSMODUS == 'BF_NON_UNI') then
          DO i = 2, nprof
               yss (i - 1) = vbv1 (i)
          END DO
          yss (nprof) = 0.
      ELSE
           DO i = 1, nprof
           yss (i) = vbv1 (i)
           END DO
      ENDIF
    ELSEIF (j.eq.13) then
      IF (nschacht.eq.0) goto 204
      bete1 = ' '
      WRITE (UNIT_OUT_QB1, 5) bete1
      WRITE (UNIT_OUT_QB1, 5) bete12
      w7 = 2
      WRITE (UNIT_OUT_QB1, 10) (wnull, i = 1, 8), w7
      WRITE (UNIT_OUT_QB1, '(''1'',i4,'' 2'',i4,i4)') nprof, nprof, nschacht
      WRITE (UNIT_OUT_QB1, '(40i3)') (ischacht (jj) , jj = 1, nschacht)
      WRITE (UNIT_OUT_QB1, '(40i3)') (ischacht (jj) , jj = 1, nschacht)
      GOTO 204
    ELSEIF (j.eq.10) then
      IF (nschacht.eq.0) goto 204
      if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
      	    bete1 = 'k-b    mm'
      	    factor = 1000.
      ELSE
      	    bete1 = 'K-St  '
      	    factor = 1.
      ENDIF

      IF (BERECHNUNGSMODUS == 'BF_UNIFORM') then
            DO i = 2, nprof
                 IF (abs (hming1 (i) - sohl1 (i) ) .gt.1.e-06) then
                      yss (i - 1) = k_ks1 (i) * 10. * factor
                      iys = int (yss (i - 1) + 0.5)
                      yss (i - 1) = float (iys) / 10.
                      IF (abs (xss (i) - xss (i - 1) ) .lt.1.) then
                           yss (i - 1) = 0.
                      ENDIF
                 ELSE
                      yss (i - 1) = 0.
                 ENDIF
            END DO
            xss (nprof) = 0.
            yss (nprof) = 0.
      ELSE
           DO i = 1, nprof
                yss (i) = k_ks1 (i) * 10. * factor
                iys = int (yss (i) + 0.5)
                yss (i) = float (iys) / 10.
           END DO
      ENDIF
    ELSEIF (j.eq.11) then
      IF (nschacht.ne.0) goto 204
      bete1 = 'Boeschung-links mNN '
      DO i = 1, nprof
           yss (i) = boli1 (i)
      END DO

    ELSEIF (j.eq.12) then
      IF (nschacht.ne.0) goto 204
      bete1 = 'Boeschung-rechts mNN '
      DO i = 1, nprof
           yss (i) = bore1 (i)
      END DO

    ENDIF


    nzeiyl (j) = 0
    IF (j.eq.1) then
           nzeist (j) = 0
    ELSE
           nzeist (j) = 1
    ENDIF


    IF (j.eq.5.or.j.eq.6.or.j.eq.7.or.j.eq.9.or.j.eq.10) then
      lityp (j) = 0
      nzeist (j) = 1

      IF (BERECHNUNGSMODUS /= 'BF_UNIFORM' .and. (j.eq.9.or.j.eq.10) ) then
           w5 = 1
           w7 = 0
      ELSE
           w7 = 2
           w5 = 2
      ENDIF
      IF (j.eq.6) then
           nzeiho (j) = 1
      ELSE
           nzeiho (j) = 2
      ENDIF

      DO i1 = 1, nprof
           IF (j.eq.5) then
                IF (yss (i1) .lt.1.0) then
                     nopwx (i1) = 1
                     nopwy (i1) = 1
                ELSE
                     nopwx (i1) = 0
                     nopwy (i1) = 0
                ENDIF
           ELSEIF (j.eq.6.or.j.eq.7) then
                IF (yss (i1) .lt.1.e-05) then
                     nopwx (i1) = 1
                     nopwy (i1) = 1
                ELSE
                     nopwx (i1) = 0
                     nopwy (i1) = 0
                ENDIF
           ELSE
                nopwx (i1) = 1
                nopwy (i1) = 1
           ENDIF
      END DO
    ELSE
      nzeiho (j) = 2
      w5 = 1

      !  J.Csocsan 10.01.96
      IF (j.eq.11) then
           lityp (j) = 8
      ELSEIF (j.eq.12) then
           lityp (j) = 9
      ELSEIF (j.eq.8) then
           lityp (j) = 2
      ELSE
           lityp (j) = 1
      ENDIF

      w7 = 0
      IF (j.eq.2.and.nschacht.eq.0) nzeist (j) = 0

    ENDIF


    WRITE (UNIT_OUT_QB1, 5) bete1
    WRITE (UNIT_OUT_QB1, 5) bete12
    WRITE (UNIT_OUT_QB1, 10) lityp (j), nzeiyl (j), nzeist (j), nzeiho (j), w5, wnull, w7, (wnull, i1 = 1, 2)
    WRITE (UNIT_OUT_QB1, 20) (wnull, xss (jj), jj = 1, nprof)
    WRITE (UNIT_OUT_QB1, 15) (wnull, yss (jj), jj = 1, nprof)

  204 END DO


  IF (nr_ausg.eq.'j') then
    DO ii = 1, 2
      WRITE (UNIT_OUT_QB1, 5)
    END DO

    w9 = 10
    ibdat = 1
    itext = nprof
    ilage = 1

    WRITE (UNIT_OUT_QB1, 10) (wnull, i1 = 1, 8), w9
    WRITE (UNIT_OUT_QB1, 11) ibdat, itext, ilage

    DO  ii = 1, nprof
      WRITE (UNIT_OUT_QB1, 23) ii, num5 (ii)
    END DO

  ENDIF

  !WP Kopfzeile der Ausgabedatei "laengsschnitt.txt"
  ! WRITE (UNIT_OUT_LAENGS, 80) 'Stat', 'Kenn', 'Abfluss', 'Sohle', 'h_WSP', 'hen', 'h_BV', 'Boe_li', 'Boe_re', 'v_m', &
  !                        & 'tau_fl', 'lamb_li', 'lamb_fl', 'lamb_re', 'f_li', 'f_fl', 'f_re', 'br_li', 'br_fl', 'br_re', &
  !                        & 'WehrOK', 'BrueckOK', 'BrueckUK', 'BrueckB', 'RohrDN'

  ! WRITE (UNIT_OUT_LAENGS, 80) 'km'  ,  '-', 'm^3/s', 'mNN'  , 'mNN'  , 'mNN', 'mNN' , 'mNN'   , 'mNN'   , 'm/s', &
  !                        & 'N/m^2', '-', '-', '-', 'm^2', 'm^2', 'm', 'm', 'm', 'm', &
  !                        & 'mNN',    'mNN',      'mNN',      'm',       'm'

  WRITE (UNIT_OUT_LAENGS, 81) 'Stat', 'Kenn', 'Abfluss', 'Sohle', 'h_WSP', 'hen', 'h_BV', 'Boe_li', 'Boe_re', 'v_m', &
                          & 'tau_fl',    'Q_li',    'Q_fl',    'Q_re', 'lamb_li', 'lamb_fl', 'lamb_re', &
                          & 'f_li', 'f_fl', 'f_re', 'br_li', 'br_fl', 'br_re', &
                          & 'WehrOK', 'BrueckOK', 'BrueckUK', 'BrueckB', 'RohrDN' , 'AlphaIW', 'AlphaEW',  'I_Reib'

  WRITE (UNIT_OUT_LAENGS, 81) 'km'  ,  '-', 'm^3/s', 'mNN'  , 'mNN'  , 'mNN', 'mNN' , 'mNN'   , 'mNN'   , 'm/s', &
                          & 'N/m^2',   'm^3/s',   'm^3/s',   'm^3/s', '-', '-', '-', 'm^2', 'm^2', 'm', 'm', 'm', 'm', &
                          & 'mNN',    'mNN',      'mNN',      'm',       'm',       '-',       '-',       '-'

  IF (BERECHNUNGSMODUS == 'BF_NON_UNI' .or. BERECHNUNGSMODUS == 'REIB_KONST') then
    WRITE (UNIT_OUT_QLAENGS, 81) 'Stat', 'Kenn', 'Abfluss', 'Sohle', 'h_WSP', 'hen', 'h_BV', 'Boe_li', 'Boe_re', 'v_m', &
                            & 'tau_fl',    'Q_li',    'Q_fl',    'Q_re', 'lamb_li', 'lamb_fl', 'lamb_re', &
                            & 'f_li', 'f_fl', 'f_re', 'br_li', 'br_fl', 'br_re', &
                            & 'WehrOK', 'BrueckOK', 'BrueckUK', 'BrueckB', 'RohrDN' , 'AlphaIW', 'AlphaEW',  'I_Reib'

    WRITE (UNIT_OUT_QLAENGS, 81) 'km'  ,  '-', 'm^3/s', 'mNN'  , 'mNN'  , 'mNN', 'mNN' , 'mNN'   , 'mNN'   , 'm/s', &
                            & 'N/m^2',   'm^3/s',   'm^3/s',   'm^3/s', '-', '-', '-', 'm^2', 'm^2', 'm', 'm', 'm', 'm', &
                            & 'mNN',    'mNN',      'mNN',      'm',       'm',       '-',       '-',       '-'
  END IF                          

 IF (BERECHNUNGSMODUS /= 'WATERLEVEL') then
   do j = 1, anz_q     ! Anzahl der Abfluesse
     do i = 1, anz_prof(j)   ! Anzahl der Profile

     !WP 16.06.2006
     !WP Durch die komplizierten Iterationsschleifen bei der Lambda-Berechnung kann
     !WP Lambda sehr groß werden. Das ist unrealistisch. Um das Format zu wahren,
     !WP werden die Lambda-Werte bei der Ausgabe auf 99.9999 begrenzt!

      do k = 1, 3
        if (out_IND(i,j,k)%lambda > 99.9999) then
          out_IND(i,j,k)%lambda = 99.9999
        end if
      end do

      IF (FLIESSGESETZ == 'MANNING_STR') THEN
        out_PROF(i,nr_q)%tau = 0.0         ! Keine Berechnung des Sohlschubspannung da kein Lambda vorhanden [N/m2]
        IF (i .eq. 1) THEN
          WRITE (*,*) 'DRUCKTAB: Tau wird zu Null gesetzt fuer Manning_Str. '
        END IF
      END IF

      if (out_PROF(i,j)%tau > 999.9999) then
        out_PROF(i,j)%tau = 999.9999
      end if

      write (UNIT_OUT_QLAENGS, 91) out_PROF(i,j)%stat, out_PROF(i,j)%chr_kenn, out_PROF(i,j)%qges, &
                             & out_PROF(i,j)%sohle, out_PROF(i,j)%wsp, &
                             & out_PROF(i,j)%hen, out_PROF(i,j)%hbv, out_PROF(i,j)%boeli, out_PROF(i,j)%boere, &
                             & out_PROF(i,j)%vm, out_PROF(i,j)%tau, &
                             & out_IND(i,j,1)%Q, out_IND(i,j,2)%Q, out_IND(i,j,3)%Q, &
                             & out_IND(i,j,1)%lambda, out_IND(i,j,2)%lambda, out_IND(i,j,3)%lambda, &
                             & out_IND(i,j,1)%A, out_IND(i,j,2)%A, out_IND(i,j,3)%A, &
                             & out_IND(i,j,1)%B, out_IND(i,j,2)%B, out_IND(i,j,3)%B, &
                             & out_PROF(i,j)%WehrOK, out_PROF(i,j)%BrueckOK, out_PROF(i,j)%BrueckUK, &
                             & out_PROF(i,j)%BrueckB, out_PROF(i,j)%RohrD, &
                             & out_PROF(i,j)%alphaIW, out_PROF(i,j)%alphaEW, out_PROF(i,j)%gefaelle
      END DO    ! Ueber alle Abfluesse
    END DO    ! Ueber alle Profile
  ENDIF
 ! ELSEIF (BERECHNUNGSMODUS == 'WATERLEVEL') then
  do i = 1, anz_prof(1)   ! Anzahl der Profile
    do k = 1, 3
      if (out_IND(i,1,k)%lambda > 99.9999) then
        out_IND(i,1,k)%lambda = 99.9999
      end if
    end do

    IF (FLIESSGESETZ == 'MANNING_STR') THEN
      out_PROF(i,1)%tau = 0.0         ! Keine Berechnung des Sohlschubspannung da kein Lambda vorhanden [N/m2]
      IF (i .eq. 1) THEN
        WRITE (*,*) 'DRUCKTAB: Tau wird zu Null gesetzt fuer Manning_Str. '
      END IF
    END IF

    if (out_PROF(i,1)%tau > 999.9999) then
      out_PROF(i,1)%tau = 999.9999
    end if

    write (UNIT_OUT_LAENGS, 91) out_PROF(i,1)%stat, out_PROF(i,1)%chr_kenn, out_PROF(i,1)%qges, &
                             & out_PROF(i,1)%sohle, out_PROF(i,1)%wsp, &
                             & out_PROF(i,1)%hen, out_PROF(i,1)%hbv, out_PROF(i,1)%boeli, out_PROF(i,1)%boere, &
                             & out_PROF(i,1)%vm, out_PROF(i,1)%tau, &
                             & out_IND(i,1,1)%Q, out_IND(i,1,2)%Q, out_IND(i,1,3)%Q, &
                             & out_IND(i,1,1)%lambda, out_IND(i,1,2)%lambda, out_IND(i,1,3)%lambda, &
                             & out_IND(i,1,1)%A, out_IND(i,1,2)%A, out_IND(i,1,3)%A, &
                             & out_IND(i,1,1)%B, out_IND(i,1,2)%B, out_IND(i,1,3)%B, &
                             & out_PROF(i,1)%WehrOK, out_PROF(i,1)%BrueckOK, out_PROF(i,1)%BrueckUK, &
                             & out_PROF(i,1)%BrueckB, out_PROF(i,1)%RohrD, &
                             & out_PROF(i,1)%alphaIW, out_PROF(i,1)%alphaEW, out_PROF(i,1)%gefaelle


  !  write (UNIT_OUT_LAENGS, 90) out_PROF(i,1)%stat, out_PROF(i,1)%chr_kenn, out_PROF(i,1)%qges, &
  !                         & out_PROF(i,1)%sohle, out_PROF(i,1)%wsp, &
  !                         & out_PROF(i,1)%hen, out_PROF(i,1)%hbv, out_PROF(i,1)%boeli, out_PROF(i,1)%boere, &
  !                         & out_PROF(i,1)%vm, out_PROF(i,1)%tau, &
  !                         & out_IND(i,1,1)%lambda, out_IND(i,1,2)%lambda, out_IND(i,1,3)%lambda, &
  !                         & out_IND(i,1,1)%A, out_IND(i,1,2)%A, out_IND(i,1,3)%A, &
  !                         & out_IND(i,1,1)%B, out_IND(i,1,2)%B, out_IND(i,1,3)%B, &
  !                         & out_PROF(i,1)%WehrOK, out_PROF(i,1)%BrueckOK, out_PROF(i,1)%BrueckUK, &
  !                         & out_PROF(i,1)%BrueckB, out_PROF(i,1)%RohrD
  END DO
 ! ENDIF

  7  FORMAT (1X, A8, ', ', A8, ' Uhr')

  8  FORMAT (1X, 7A10,   A8,   A8,   3A8,   3A10,   3A10)
  9  FORMAT (1x, 7F10.4, F8.3, F8.2, 3F8.4, 3F10.3, 3F10.3)

 ! 80 FORMAT (1X, A10,   A5, 7A10,   A8,   A8,   3A8,   3A10,   3A10,   5A10)
  81 FORMAT (1X, A10,   A5, 7A10,   A8,   A8,   3A10,   3A8,   3A10,   3A10,   8A10)
 ! 90 format (1X, F10.4, A5, 7F10.3, F8.3, F8.2, 3F8.4, 3F10.3, 3F10.3, 5F10.3)
  91 format (1X, F10.4, A5, 7F10.3, F8.3, F8.2, 3F10.3, 3F8.4, 3F10.3, 3F10.3, 5F10.3, 3F10.5)
  ! ST --------------------------------------------------------------------

ENDIF !MD Ende fuer BERECHNUNGSMODUS == 'WATERLEVEL'.or.'BF_NON_UNI'.or.'REIB_KONST'

!MD  die qb2 wird fuer stat. ungleichf. und gleichf. Berechnung erzeugt
!MD  ELSEIF (mark.gt.1) then
If (BERECHNUNGSMODUS /= 'WATERLEVEL') then

  PRINT *
  PRINT *
  mode = 0

  if (RUN_MODUS /= 'KALYPSO') then

    DO 130 WHILE(mode.le.0.or.mode.gt.2)
         write ( * , 1601)
         1601 FORMAT (//  &
         & 1X, '**********************************************',/, &
         & 1X, '*       --------------------------------     *',/, &
         & 1X, '*          EINHEIT DES DURCHFLUSSES          *',/, &
         & 1X, '*       --------------------------------     *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '*  Bitte waehlen Sie zwischen (1) und (2)    *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '*    -  Dimension in qm/s              (1)   *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '*    -  Dimension in l/s               (2)   *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '**********************************************',//)

         write (*, *) ' --> '
         READ ( * , '(i2)', err = 130) mode
         write (*, *) mode
         145   PRINT *

    130 CONTINUE

  else
    mode = 1

  end if

  IF (mode.eq.2) then
       DO i1 = 1, nprof
            qbv1 (i1) = qbv1 (i1) * 1000.
            iqss = int (qbv1 (i1) + 0.5)
            qbv1 (i1) = float (iqss)
       END DO
  ENDIF


  qmin = 10000.
  DO i = 1, nprof
      IF (qbv1 (i) .lt.qmin) then
      qmin = qbv1 (i)
      ENDIF
  END DO

  qmax = 0.
  DO i = 1, nprof
       IF (qbv1 (i) .gt.qmax) then
       qmax = qbv1 (i)
       ENDIF
  END DO

  !     rundung auf fuenf-meter schritte
  iqmin = int (qmin / 5.)
  qmin = float (iqmin) * 5.0

  iqmax = int (qmax / 5.)
  qmax = float (iqmax) * 5.0

  WRITE (UNIT_OUT_QB2, 5) pfad2
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, '(a)') 'ABFLUSSLAENGSSCHNITT '
  WRITE (UNIT_OUT_QB2, 5) ' '

  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 5) ' '
  WRITE (UNIT_OUT_QB2, 6) 'B- 2 0 0 0 0 0'
  WRITE (UNIT_OUT_QB2, 5) ' '

  ianz = 2
  WRITE (UNIT_OUT_QB2, '(3i5)') ianz, nprof2, nprof2

  7007   y0 = 0.5
  xmass = 1000.0
  gx = 0.0
  bzgh = qmin
  IF (bzgh.le.10.) bzgh = 0.0
  ymass = 100.0
  7004   gy = (qmax - bzgh) / ymass * 100. + 7.0
  IF (gy.gt.29.7) then
       ymass = ymass * 5.0
       GOTO 7004
  ENDIF
  gy = 29.7
  yo = 0.0
  WRITE (UNIT_OUT_QB2, '(6f8.2,i2)') yo, gx, bzgh, xmass, ymass, gy, wnull

  !     ************************************************************
  !     mark=2: darstellungsform 2 - bordvoller abflusz, kennung
  !             es entsteht ein .qbv-file
  !     ************************************************************

  DO j = 1, ianz

       bete12 = ' '
       IF (j.eq.1) then
           bete1 = 'BORDVOLL ABFLUSS  qm/s'
           DO i = 1, nprof
                yss (i) = qbv1 (i)
           END DO
       ENDIF

       IF (j.eq.2) then
            bete1 = 'KENNUNG         '
            bete12 = ' - '
            DO i = 1, nprof
                 yss (i) = 1. * jkenn1 (i)
            END DO
       ENDIF

       nzeiho (j) = 2
       lityp (j) = j
       IF (j.eq.2) lityp (j) = 0
       nzeiyl (j) = 2
       nzeist (j) = 1
       nzeist (1) = 2
       i1 = 0
       i2 = 1
       i3 = 3

       WRITE (UNIT_OUT_QB2, 5) bete1
       WRITE (UNIT_OUT_QB2, 5) bete12
       WRITE (UNIT_OUT_QB2, 10) lityp (j), nzeiyl (j), nzeist (j), nzeiho (j), weins, (wnull, i4 = 1, 4)
       WRITE (UNIT_OUT_QB2, 21) i1, xss (1), (i1, xss (jj), i3, xss (jj), jj = 1, nprof)

       IF (j.eq.2) then
            yssa = 0.
       ELSE
       !     yssa=bzgh
            yssa = 0.
       ENDIF

       WRITE (UNIT_OUT_QB2, 16) i1, yssa, i2, yss (1), (i2, yss (jj), i1, yss (jj), jj = 2, nprof - 1), i2, yss (nprof), i2,&
             yss (nprof), i1, yssa
  END DO


  yhmin = 10000.
  DO i = 1, nprof
       IF (sohl1 (i) .lt.yhmin) then
            yhmin = sohl1 (i)
       ENDIF
  END DO

  yhmax = 0.
  DO i = 1, nprof
       hneu = max (boli1 (i), bore1 (i), hming1 (i) )
       IF (hneu.gt.yhmax) then
            yhmax = hneu
       ENDIF
  END DO

  !     rundung auf fuenf-meter schritte

  ihmin = int (yhmin / 5.)
  yhmin = float (ihmin) * 5.0

  ihmax = int (yhmax / 5.)
  yhmax = float (ihmax) * 5.0


  IF (nschacht.eq.0) then
       IF (nr_ausg.eq.'n') then
            ianz = 6
            WRITE (UNIT_OUT_QB1, '(i2,10i4)') ianz, (nprof, i2 = 1, ianz)
       ELSE
            ianz = 7
            WRITE (UNIT_OUT_QB1, '(i2,10i4)') ianz, (nprof, i2 = 1, ianz - 1)
       ENDIF
  ELSE
       IF (nr_ausg.eq.'n') then
            ianz = 10
            WRITE (UNIT_OUT_QB1, '(i2,10i4)') ianz, (nprof, i2 = 1, ianz - 1)
       ELSE
            ianz = 11
            WRITE (UNIT_OUT_QB1, '(i2,10i4)') ianz, (nprof, i2 = 1, ianz - 2)
       ENDIF
  ENDIF

  y0 = 0.5
  xmass = 1000.0
  gx = 0.0
  bzgh = yhmin
  ymass = 100.0
  8006   gy = (yhmax - yhmin) / ymass * 100. + 7.0

  IF (gy.gt.29.7) then
       ymass = ymass * 5.0
       GOTO 8006
  ENDIF

  gy = 29.7
  yo = 0.0
  WRITE (UNIT_OUT_QB1, '(6f8.2,i2)') yo, gx, bzgh, xmass, ymass, gy, wnull


  !     ************************************************************
  !     mark=4: darstellungsform 4 - sohlhoehe, boe. links,
  !                                  boe. rechts,bordv. Abfluss,
  !                                  Fliessg. bei bordvoll
  !     ************************************************************

  imax = 10

  DO 301 j = 1, imax
       bete12 = ' '
       IF (j.eq.1) then
            IF (nschacht.eq.0) goto 301
            bete1 = 'Gelaende   mNN    '
            DO i = 1, nprof
                 yss (i) = hming1 (i)
            END DO
       ELSEIF (j.eq.2) then
            bete1 = 'Sohlhoehe mNN     '
            DO i = 1, nprof
                 yss (i) = sohl1 (i)
            END DO
       ELSEIF (j.eq.3) then
            bete1 = 'Laenge     m    '
            DO i = 2, nprof
                 dif = abs (xss (i - 1) - xss (i) )
                 idif = int (dif * 2. + 0.5)
                 yss (i - 1) = float (idif) / 2.
            END DO

            IF (nprof.gt.1) yss (nprof) = yss (nprof - 1)
       ELSEIF (j.eq.4) then
            IF (nschacht.eq.0) goto 301
            bete1 = 'Rohrdurchmesser DN'
            DO i = 1, nprof
                 IF (abs (hming1 (i) - sohl1 (i) ) .gt.1.e-06) then
                      ys = (hwsp1 (i) - sohl1 (i) ) * 1000.
                      iy = int (ys + 0.5)
                      yss (i) = iy
                      IF (i.lt.nprof.and.abs (xss (i + 1) - xss (i) ) .lt.1) then
                           yss (i) = 0
                      ENDIF
                 ELSE
                      yss (i) = 0.0
                 ENDIF
            END DO
       ELSEIF (j.eq.5) then
            bete1 = 'Gefaelle  0/00   '
            DO i = 2, nprof
                 yss (i - 1) = gef1 (i) * 1000.
            END DO

            IF (nprof.gt.1) yss (nprof) = yss (nprof - 1)
       ELSEIF (j.eq.6) then
            bete1 = 'Wsp-bv    mNN    '
            DO i = 1, nprof
                 yss (i) = hwsp1 (i)
            END DO
       ELSEIF (j.eq.7) then
            IF (mode.eq.1) then
                 bete1 = 'Q-bv       qm/s '
            ELSE
                 bete1 = 'Q-bv       l/s'
            ENDIF

            IF (BERECHNUNGSMODUS == 'BF_NON_UNI') then
                 DO i = 2, nprof
                      yss (i - 1) = qbv1 (i)
                 END DO
                 IF (nprof.gt.1) yss (nprof) = yss (nprof - 1)
            ELSE
                 DO i = 1, nprof
                      yss (i) = qbv1 (i)
                 END DO
            ENDIF
       ELSEIF (j.eq.8) then
            bete1 = 'v-bv       m/s    '

            IF (BERECHNUNGSMODUS == 'BF_NON_UNI') then
                 DO i = 2, nprof
                      yss (i - 1) = vbv1 (i)
                 END DO
                 IF (nprof.gt.1) yss (nprof) = yss (nprof - 1)
            ELSE
                 DO i = 1, nprof
                      yss (i) = vbv1 (i)
                 END DO
            ENDIF
       ELSEIF (j.eq.10) then
            IF (nschacht.eq.0) goto 301
            bete1 = ' '
            WRITE (UNIT_OUT_QB1, 5) bete1
            WRITE (UNIT_OUT_QB1, 5) bete12
            w7 = 2
            WRITE (UNIT_OUT_QB1, '(9i2)') (wnull, i1 = 1, 8) , w7
            WRITE (UNIT_OUT_QB1, '(''1'',i4,'' 2'',i4,i4)') nprof, nprof, nschacht
            WRITE (UNIT_OUT_QB1, '(40i3)') (ischacht (jj) , jj = 1, nschacht)
            WRITE (UNIT_OUT_QB1, '(40i3)') (ischacht (jj) , jj = 1, nschacht)
            GOTO 301
       ELSEIF (j.eq.9) then
            IF (nschacht.eq.0) goto 301
            if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
                 bete1 = 'k-b    mm'
                 factor = 1000.
            ELSE
                 bete1 = 'K-St  '
                 factor = 1.
            ENDIF
            DO i = 2, nprof
                 IF (abs (hming1 (i) - sohl1 (i) ) .gt.1.e-06) then
                      yss (i - 1) = k_ks1 (i) * 10. * factor
                      iys = int (yss (i - 1) + 0.5)
                      yss (i - 1) = float (iys) / 10.
                      IF (abs (xss (i) - xss (i - 1) ) .lt.1.) then
                           yss (i - 1) = 0.
                      ENDIF
                 ELSE
                      yss (i - 1) = 0.
                 ENDIF
            END DO
            !       xss(nprof) = 0.
            !UT        ACHTUNG: MUSS ES NICHT NPROF STATT npro HEISSEN?
            !**        UT, 18.08.2000, GEAENDERT AM 28.09.2000
            IF (nprof.gt.1) yss (nprof) = yss (nprof - 1)

       ENDIF


       !             call ucase(bete1)
       nzeiyl (j) = 0
       IF (j.eq.1) then
            nzeist (j) = 0
       ELSE
            nzeist (j) = 1
       ENDIF

       IF (j.ge.3.and.j.le.9) then
            IF (j.eq.6) then
                 lityp (j) = 1
                 w7 = 0
                 w5 = 1
            ELSE
                 lityp (j) = 0
                 w7 = 2
                 w5 = 2
            ENDIF

            w6 = 0
            nzeist (j) = 1

            IF (j.eq.4) then
                 nzeiho (j) = 21
            ELSE
                 nzeiho (j) = 2
            ENDIF

            DO i1 = 1, nprof

                 IF (j.eq.3) then

                      IF (yss (i1) .lt.1.0) then
                           nopwx (i1) = 1
                           nopwy (i1) = 1
                      ELSE
                           nopwx (i1) = 0
                           nopwy (i1) = 0
                      ENDIF
                 ELSEIF (j.ge.3.and.j.le.9) then
                      IF (yss (i1) .lt.1.e-05) then
                           nopwx (i1) = 1
                           nopwy (i1) = 1
                      ELSE
                           nopwx (i1) = 0
                           nopwy (i1) = 0
                      ENDIF
                 ELSE
                      nopwx (i1) = 0
                      nopwy (i1) = 0
                 ENDIF
            END DO

            ELSE

            nzeiho (j) = 0
            w5 = 1
            w6 = 0
            lityp (j) = 1
            w7 = 0
            IF (j.eq.2.and.nschacht.eq.0) nzeist (j) = 0

       ENDIF

       WRITE (UNIT_OUT_QB1, 5) bete1
       WRITE (UNIT_OUT_QB1, 5) bete12

       WRITE (UNIT_OUT_QB1, 10) lityp (j), nzeiyl (j), nzeist (j), nzeiho (j), w5, w6, w7, wnull, wnull

       !WP 07.05.2004
       !WP Urspruengliche Datenausgabe

       !WP          write(UNIT_OUT_QB1,20)(nopwx(jj),xss(jj),jj=1,nprof)
       !WP          write(UNIT_OUT_QB1,15)(nopwy(jj),yss(jj),jj=1,nprof)

       !WP Es ist auch unklar, warum das ueberhaupt bisher funktioniert hat,
       !WP da Format-Anweisungen normalerweise nicht in die naechst Zeilen
       !WP springen koennen.

       vzeil = INT (nprof / 8)
       rest = MOD (nprof, 8)
       !             PRINT *, vzeil, rest

       DO m = 1, vzeil

            WRITE (UNIT = UNIT_OUT_QB1, FMT = 20, IOSTAT = ierr) nopwx ( (m - 1) * 8 + 1), xss ( (m - 1) * 8 + 1), nopwx ((m -1)&
                   * 8 + 2), xss ( (m - 1) * 8 + 2), nopwx ( (m - 1) * 8 + 3), xss ( (m - 1) * 8 + 3), nopwx ( (m - 1) * 8&
                   + 4), xss ( (m - 1) * 8 + 4), nopwx ( (m - 1) * 8 + 5), xss ( (m - 1) * 8 + 5), nopwx ( (m - 1) * 8 + 6)&
                   , xss ( (m - 1) * 8 + 6), nopwx ( (m - 1) * 8 + 7), xss ( (m - 1) * 8 + 7), nopwx ( (m - 1) * 8 + 8), &
                   xss ( (m -  1) * 8 + 8)
            IF (ierr /= 0) then
                 WRITE ( * , * ) 'Fehler bei Datenausgabe in ZEILA (XSS(i):', ierr
                 STOP
            ENDIF

       END DO

       WRITE (UNIT = UNIT_OUT_QB1, FMT = 20, IOSTAT = ierr) (nopwx (jj), xss (jj), jj = (vzeil * 8 + 1), nprof)
       IF (ierr /= 0) then
            WRITE ( * , * ) 'Fehler bei Datenausgabe in ZEILA (XSS(i):', ierr
            STOP
       ENDIF

       DO m = 1, vzeil

            WRITE (UNIT = UNIT_OUT_QB1, FMT = 15, IOSTAT = ierr) nopwx ( (m - 1) * 8 + 1), yss ( (m - 1) * 8 + 1),nopwx((m -1) *&
                   8 + 2), yss ( (m - 1) * 8 + 2), nopwx ( (m - 1) * 8 + 3), yss ( (m -  1) * 8 + 3), nopwx ( (m - 1) * 8 +&
                   4), yss ( (m - 1) * 8 + 4), nopwx ( (m - 1) * 8 + 5), yss ( (m - 1) * 8 + 5), nopwx ( (m - 1) * 8 + 6),&
                   yss ( (m - 1) * 8 + 6), nopwx ( (m - 1) * 8 + 7), yss ( (m - 1) * 8 + 7), nopwx ( (m - 1) * 8 + 8), yss &
                   ( (m - 1) * 8 + 8)
            IF (ierr /= 0) then
                  WRITE ( * , * ) 'Fehler bei Datenausgabe in ZEILA (YSS(i):',ierr
                  STOP
            ENDIF


       END DO

       WRITE (UNIT = UNIT_OUT_QB1, FMT = 15, IOSTAT = ierr) (nopwx (jj), yss (jj), jj = (vzeil * 8 + 1), nprof)
       IF (ierr /= 0) then
            WRITE ( * , * ) 'Fehler bei Datenausgabe in ZEILA (YSS(i):', ierr
            STOP
       ENDIF


  301 END DO

  IF (nr_ausg.eq.'j') then

       DO ii = 1, 2
            WRITE (UNIT_OUT_QB1, 5)
       END DO

       w9 = 10
       ibdat = 1
       itext = nprof
       ilage = 1

       WRITE (UNIT_OUT_QB1, 10) (wnull, i1 = 1, 8), w9
       WRITE (UNIT_OUT_QB1, 11) ibdat, itext, ilage

       DO ii = 1, nprof
            WRITE (UNIT_OUT_QB1, 23) ii, num5 (ii)
       END DO

  ENDIF

ENDIF
                                                                        
!WP 10.05.2004
CLOSE (UNIT_OUT_QB1)
CLOSE (UNIT_OUT_QB2)
CLOSE (UNIT_OUT_LAENGS)
!WP 10.05.2004
                                                                        
RETURN
                                                                        
 5 FORMAT(a)
 6 FORMAT(a16,i2)
10 FORMAT(16i3)
11 FORMAT(4i5)
15 FORMAT(8(I2,F8.2))
16 FORMAT(8(I2,F8.2))
20 FORMAT(8(I2,f11.1))
21 FORMAT(8(i2,f8.0))
23 FORMAT(i3,1x,a10)
                                                                        
9801 PRINT * , 'Fehler beim Oeffnen der Laengsschnittdatei'
STOP 'stop in SUB zeila'
                                                                        
END SUBROUTINE zeila
