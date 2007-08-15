!     Last change:  MD   15 Aug 2007    9:43 am
!--------------------------------------------------------------------------
! This code, qbordv.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - qbordv
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



!------------------------------------------------------------------------------
SUBROUTINE qbordv ()
!
!  geschrieben: P. Koch     Juli 1990
!  gaendert: 	W. Ploeger  Juni 2006
!
!  Programmbeschreibung:
!  ---------------------
!  Dieses Programm steuert die Bordvoll-Berechnung, d.h. von hier
!  aus wird das Berechnungsprogramm WSPBER fuer das jeweilige q neu
!  gestartet.
!
!  Ergebnis-files der Bordvoll-Berechnung:
!
!  plot-files:
!  -  abflusslaengsschnitt            flussname.qbv
!  -  wasserspiegellaengsschnitt jeweils mit eintrag des bordvollen
!     wasserspiegels (nur zu kontrollzwecken
!                     hwsp(bordvoll)=min h(boeschungskante))
!                                     flussname.wsp
!
!  daten-files:
!  - wasserstands-abflussbeziehung    flussname(station).pro
!  - wasserspiegellage fuer jedes q   flussname(q).tab
!
!
!   IN DIESER SUBROUTINE VERWENDETE VARIABLEN
!   -----------------------------------------
!
!   bolip   --      Höhe Bordvollpunkt links
!   bordvoll--      Art der Bordvollbemessung
!   borep   --      Höhe Bordvollpunkt rechts
!   dif     --      Differenz
!   hbord   --      Wasserspiegelhöhe bei Bordvoll
!   hbv     --      Wasserspiegelhöhe bei Bordvoll
!   hen     --      Energiehöhe
!   hmingp  --      minimale Geländehöhe
!   hwsp    --      Wasserspiegelhöhe
!   ianz    --      Schrittanzahl
!   idr2    --      Abfrage nach Erstellen von Ergebnislisten
!   ilen    --      Länge einer Zeichenkette
!   isstat  --
!   k_kp    --
!   nbv     --      Nummer des Bordvollereignisses
!   nprof   --      Anzahl der Profilpunkte
!   q       --      jeweiliger Abfluß
!   qbord   --      Abfluß bei Bordvoll
!   qschritt--      jeweiliger Abfluß
!   qvar    --      jeweiliger Abfluß
!   sohlp   --      Höhe Sohle
!   stat    --
!   vbord   --      mittlere Geschwindigkeit bei Bordvoll
!   vbv     --      mittlere Geschwindigkeit bei Bordvoll
!   vmp     --      mittlere Geschwindigkeit bei Bordvoll
!
!JK   AUFGERUFENE SUBROUTINEN:                                          
!JK     bovog1                                                          
!JK     wspber                                                          
!JK     lapro1                                                          
!     ******************************************************************



!------------------------------------------------------------------------------
! VEREINBARUNGEN
!------------------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS
USE IO_NAMES
USE MOD_ERG
USE MOD_INI


! COMMON-Block /BV/ -----------------------------------------------------------
CHARACTER(LEN=1) :: idr1        ! Erstellen der Wasserstands-Abflussbeziehungen
CHARACTER(LEN=1) :: idr2  	! Abfrage nach Erstellen von Ergebnislisten
INTEGER 	 :: nprof       ! Anzahl der Profilpunkte
INTEGER          :: isch        ! ???? Iterationsvariable
REAL 		 :: qvar        ! Jeweiliger Abfluss
COMMON / bv / idr1, idr2, nprof, qvar, isch
!     idr1 = 'j'  -->  erstellen der wasserstandsabflussbeziehungen,
!                      d.h. erstellen der '.pro'-files  fuer jede       
!                      station                                          
!                      beispiel: westerbach2100.pro                     
!                                = wasserstandsabflussbeziehung fuer den
!                                  westerbach an station km 2.100       
!     idr2 = 'j'  -->  erstellen der wasserspiegellinien fuer jedes be-
!                      rechnete q, d.h. erstellen der '.tab'-files      
!                      beispiel: westerbach50.tab                       
!                                = wasserspiegellinie fuer den          
!                                  westerbach fuer q = 50 m**3/s        
! -----------------------------------------------------------------------------


! COMMON-Block /BV_IT/ --------------------------------------------------------
INTEGER 	:: prof_it (maxger)
COMMON / bv_it / prof_it
!   kennung der iterierten profile fuer die ungleich-
!   foermige bordvollberechnung. Common-block wird aufgerufen in:
!   - wspber
!   - bordvoll
! -----------------------------------------------------------------------------


! COMMON-Block /DATTEXT/ ------------------------------------------------------
CHARACTER(LEN=nch80) :: text0, satz (idim)
CHARACTER(LEN=36)    :: text1, text11, text2, text22, text23, text3, text32
CHARACTER(LEN=36)    :: text33, text7
CHARACTER(LEN=16)    :: text4
CHARACTER(LEN=8)     :: text6
CHARACTER(LEN=12)    :: text66
CHARACTER(LEN=20)    :: text5
INTEGER 	     :: textq (3, 2), nquer (3)
COMMON / dattext / text0, text1, text11, text2, text22, text23,   &
       & text3, text32, text33, text7, text4, text5, text6, text66, textq, &
       & nquer, satz
! -----------------------------------------------------------------------------


! COMMON-Block /ERG/ ----------------------------------------------------------
REAL 		:: wsp (maxger), hen (maxger), qs (maxger), fgesp (maxger)
REAL 		:: froudp (maxger), hvs (maxger), hrs (maxger), hs (maxger)
REAL 		:: fp (maxger, maxkla), up (maxger, maxkla), vp (maxger, maxkla)
REAL 		:: qtp (maxger, maxkla), rkp (maxger, maxkla), fbwp (maxger, maxkla)
REAL 		:: brp (maxger, maxkla)
REAL		:: vmp (maxger), hbors (maxger), hein (maxger), hort (maxger), brg (maxger)
INTEGER 	:: igrenz (maxger)
COMMON / erg / wsp, hen, qs, fgesp, froudp, hvs, hrs, hs, fp, up, &
             & vp, qtp, rkp, fbwp, brp, vmp, hbors, hein, hort, igrenz, brg
! -----------------------------------------------------------------------------


! COMMON-Block /ERG_KM/ -------------------------------------------------------
INTEGER :: ind (maxger), indf (maxger)
REAL 	:: brp1 (maxger, maxkla), qtp1 (maxger, maxkla), fp1 (maxger, maxkla)
COMMON / erg_km / ind, indf, brp1, qtp1, fp1
! -----------------------------------------------------------------------------


! COMMON-Block /K_M/ ----------------------------------------------------------
CHARACTER(LEN=1) :: km          ! Kalinin - Miljukov - Parameter ( = 'j' oder = 'n')
INTEGER 	 :: i_km        ! Zaehler fuer stationaer-gleichfoermige Berechnung
COMMON / k_m / km, i_km
! Common-bloecke fuer Kalinin - Miljukov - Parameter, aufgerufen in:
! - qbordv
! - wspber
! - speicher
! - wsp
! -----------------------------------------------------------------------------


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 	:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 	:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /P1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: ereignis, fnam1, fluss
CHARACTER(LEN=1) :: bordvoll
COMMON / p1 / ereignis, fnam1, bordvoll, fluss
! -----------------------------------------------------------------------------


! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! COMMON-Block /P11/ ----------------------------------------------------------
REAL :: wsanf, qstat (merg), qwert (merg), staanf, staend
INTEGER :: nq
COMMON / p11 / wsanf, nq, qstat, qwert, staanf, staend
! -----------------------------------------------------------------------------


! COMMON-Block /PRO1/ ---------------------------------------------------------
REAL 	:: qbord(maxger), hbord (maxger), vbord (maxger)
INTEGER :: jkenn (maxger)
COMMON / pro1 / qbord, hbord, jkenn, vbord
!     Vorbelegung:
!     ------------
!     jkenn - kennung dafuer, ob der bordvoll wert ermittelt worden ist 
!             oder nicht                                                
!             jkenn=0 - bordvoller abfluss noch nicht ermittelt         
!             jkenn=1 - bordvoller abfluss ermittelt                    
! -----------------------------------------------------------------------------


! COMMON-Block /PROF/ ---------------------------------------------------------
REAL 	:: hwsp (100, maxger)
COMMON / prof / hwsp
! -----------------------------------------------------------------------------


! COMMON-Block /PROFILNUMMER/ -------------------------------------------------
CHARACTER(LEN=10) :: num (maxger)
COMMON / profilnummer / num
! -----------------------------------------------------------------------------


! COMMON-Block /QERG/ ---------------------------------------------------------
INTEGER :: ikennung (maxger, maxger)
COMMON / qerg / ikennung
! -----------------------------------------------------------------------------


! COMMON-Block /UEBER_KM/ -----------------------------------------------------
REAL 	:: qq (100, maxger), bb (100, maxger), fb (100, maxger)
REAL 	:: vfl (100, maxger), qvor (100, maxger), bvor (100, maxger)
REAL 	:: fvor (100, maxger)
INTEGER :: ischbv (maxger)
COMMON / ueber_km / qq, bb, fb, vfl, qvor, bvor, fvor, ischbv
! -----------------------------------------------------------------------------



! Local variables
! -----------------------------------------------------------------------------

!HB   wird fuer den Aufruf von SUB LAPRO1 benoetigt (Dateipfade)
CHARACTER(LEN=nch80) :: unit5

INTEGER :: istat

INTEGER :: i, erlen
INTEGER :: np (ipro)
INTEGER :: int (merg)
INTEGER :: indfl, indmax

REAL :: feldr (merg)

CHARACTER(LEN=nch80) :: char (merg)
CHARACTER(LEN=nch80) :: dummy
CHARACTER(LEN=nch80) :: unit1, unit2, unit3, unit4, unit7

!HB   Pfadnamen Bordvolldateien *.qb1 und *.qb2
CHARACTER(LEN=nch80) :: pfad_qb1, pfad_qb2
CHARACTER(LEN=nch80) :: nr
REAL :: dif (maxger)


REAL :: qschritt (maxger)
!     wird nur zum ausdruck des kontrollfiles bei q-bordvoll genutzt
REAL :: vbv (100, maxger)



!------------------------------------------------------------------
! BERECHNUNGEN
!------------------------------------------------------------------


unit1 	= NAME_PFAD_DATH
ilen 	= LEN_TRIM (unit1)
ifllen 	= LEN_TRIM (FLUSSNAME)

IF (ifllen.gt.4) then
  ifllen = 4
ENDIF

unit1(ilen+1 : ) = FLUSSNAME(1:ifllen)

!HB   Anfuegen der Endung '.tab' an den Pfad unit1.
!HB   Es wird von Position 'ilen+5' bis zur 80. Position, da
!HB   'nch80'=80, '.tab' angehaengt.
ilen = LEN_TRIM (unit1)
unit1(ilen+1 : ) = '.tab'

NAME_OUT_WSL = unit1
unit2 	     = unit1
write (*,*) 'NAME_OUT_WSL = ', NAME_OUT_WSL


!JK   WENN STATIONAER-UNGLEICHFOERMIGE ABFLUSSVERHAELTNISSE
!--------------------------------------------------------------------------------------------------
If (BERECHNUNGSMODUS == 'BF_NON_UNI' .OR. BERECHNUNGSMODUS == 'REIB_KONST') then
!MD  if (BERECHNUNGSMODUS == 'BF_NON_UNI') then

  ! Eroeffnen der neuen AusgabeDatei 'Q_LangSchnitt.txt'
  ilen = LEN_TRIM(NAME_PFAD_DATH)
  NAME_OUT_QLAENGS = NAME_PFAD_DATH(1:ilen) // 'Q_LangSchnitt.txt'
  UNIT_OUT_QLAENGS = ju0gfu ()
  OPEN (unit = UNIT_OUT_QLAENGS, file = NAME_OUT_QLAENGS, status = 'REPLACE', iostat = istat)
  if (istat /= 0) then
    write (*, 9009) NAME_OUT_QLAENGS
    9009 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
               & 1X, 'Programm wird beendet!')
    call stop_programm(0)
  end if

  ! Eroeffnen der Ausgabedatei 'Laengs_QWehre.txt' und 'HOW-Qwehr-HUW'
  If (BERECHNUNGSMODUS == 'REIB_KONST') then
    ilen = LEN_TRIM(NAME_PFAD_DATH)
    NAME_OUT_WEHR = NAME_PFAD_DATH(1:ilen) // 'Laengs_QWehre.txt'
    NAME_OUT_QWEHR = NAME_PFAD_DATH(1:ilen) // 'HOW_QWehr_HUW.txt'
    NAME_OUT_BRUECKE = NAME_PFAD_DATH(1:ilen) // 'Laengs_QBruecke.txt'
    NAME_OUT_QBRUECKE = NAME_PFAD_DATH(1:ilen) // 'HOW_QBruecke_HUW.txt'

    UNIT_OUT_WEHR = ju0gfu ()
    OPEN (unit = UNIT_OUT_WEHR, file = NAME_OUT_WEHR, status = 'REPLACE', iostat = istat)
    if (istat /= 0) then
      write (*, 9007) NAME_OUT_WEHR
      9007 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
         & 1X, 'Programm wird beendet!')
      call stop_programm(0)
    end if

    UNIT_OUT_QWEHR = ju0gfu ()
    OPEN (unit = UNIT_OUT_QWEHR, file = NAME_OUT_QWEHR, status = 'REPLACE', iostat = istat)
    if (istat /= 0) then
      write (*, 9008) NAME_OUT_QWEHR
      9008 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
        & 1X, 'Programm wird beendet!')
      call stop_programm(0)
    end if
    WRITE (UNIT_OUT_QWEHR, '(5x,''Profil'',6x,''Q_OW'',4x,''Q-wehr'',6x,''h_ow'',6x,''h_uw'',3x,''Ü-Art'')')

    UNIT_OUT_BRUECKE  = ju0gfu ()
    OPEN (unit = UNIT_OUT_BRUECKE , file = NAME_OUT_BRUECKE , status = 'REPLACE', iostat = istat)
    if (istat /= 0) then
      write (*, 9010) NAME_OUT_BRUECKE
      9010 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
         & 1X, 'Programm wird beendet!')
      call stop_programm(0)
    end if

    UNIT_OUT_QBRUECKE  = ju0gfu ()
    OPEN (unit = UNIT_OUT_QBRUECKE , file = NAME_OUT_QBRUECKE , status = 'REPLACE', iostat = istat)
    if (istat /= 0) then
      write (*, 9011) NAME_OUT_QBRUECKE
      9011 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
         & 1X, 'Programm wird beendet!')
      call stop_programm(0)
    end if
    WRITE (UNIT_OUT_QBRUECKE, '(5x,''Profil'',6x,''Q_OW'',6x,''Q-UW'',6x,''h_ow'',6x,''h_uw'',6x,''h_en'')')

  end if


  DO i = 1, maxger
    jkenn (i) = 0
  END DO

  ! ******************************************************************
  ! Berechnung der schrittanzahl fuer die bordvollberechnung
  ! ******************************************************************

  ianz = ifix ( (MAX_Q - MIN_Q) / DELTA_Q + 0.1)
  ianz = ianz + 1

  ! Zuweisung der Anzahl der Abfluesse zu dem globalen Modul MOD_ERG
  anz_q = ianz

  ! anfangswert fuer q:
  qvar = MIN_Q
  qschritt (1) = qvar


  ! ******************************************************************
  ! Schleife zur Bordvoll-Berechung, ISCH ist Zaehler fuer Abflusses
  ! ******************************************************************

  DO isch = 1, ianz

    ! Zuweisung der Abfluss-Nummer zu dem globalen Modul MOD_ERG
    nr_q = isch

    q = qvar

    nbv = 0

    write (    *       , 1001) qvar
    write (    0       , 1001) qvar
    write (UNIT_OUT_LOG, 1001) qvar
    1001 format (//1X, '*************************************************', /, &
                 & 1X, 'Bordvoll-Berechung --> Q=', F10.3, ' m**3/s', /, &
                 & 1X, '*************************************************', /)


    !JK  ERSTELLEN WASSERSPIEGELLINIEN
    IF (idr2.eq.'j') then

      ! dateiname fuer ausgabedatei flussname..q..tab:
      WRITE (EREIGNISNAME, '(f9.3)') qvar

      erlen = LEN_TRIM (EREIGNISNAME)

      DO i = 1, erlen
        IF (EREIGNISNAME (i:i) .eq.'.') then
          EREIGNISNAME (i:i) = '-'
        ENDIF
      END DO
      EREIGNISNAME = ADJUSTL (EREIGNISNAME)

      unit1 = NAME_PFAD_DATH
      ilen = LEN_TRIM (unit1)
      ifllen = LEN_TRIM (FLUSSNAME)

      IF (ifllen.gt.2) then
        ifllen = 2
      ENDIF

      unit1 (ilen + 1:nch80) = FLUSSNAME(1:ifllen)
      unit2 = unit1
      ilen = LEN_TRIM (unit1)
      unit1 (ilen + 1:nch80) = EREIGNISNAME
      ilen = LEN_TRIM (unit1)
      unit1 (ilen + 1:nch80) = '.tb'

      !write (*,*) 'In QBORDV. unit1 = ', unit1

      NAME_OUT_TAB = unit1

      !write (*,*) 'In QBORDV. unit2 = ', unit2

    ENDIF

    IF (qvar.gt.MAX_Q) then
      qvar = MAX_Q
    ENDIF


    !--------------------------------------------------------------
    CALL wspber ()
    !--------------------------------------------------------------



    !************************************************
    !**    Ergaenzung vom 27.10.92 J. Csocsan
    !************************************************

    !write (*,*) 'Anzahl Profile = ', nprof

    DO j = 1, nprof

      IF (prof_it (j) .eq.0) then
        nbv = nbv + 1
        hwsp (isch, nbv) = wsp (j)
        vbv (isch, nbv) = vmp (j)
        stat (nbv) = stat (j)

        IF (isch.eq.ianz) then
          hbv (nbv) = hbv (j)

          num (nbv) = num (j)
          bolip (nbv) = bolip (j)
          borep (nbv) = borep (j)
          sohlp (nbv) = sohlp (j)
          hen (nbv) = hen (j)
          isstat (nbv) = isstat (j)
          hmingp (nbv) = hmingp (j)
          k_kp (nbv) = k_kp (j)
        ENDIF

        ! K - M Parameter

        IF (km.eq.'j') then

          indfmax = ind (j)
          indfl = indf (j)
          qq (isch, nbv) = qtp1 (nbv, indfl)
          bb (isch, nbv) = brp1 (nbv, indfl)
          fb (isch, nbv) = fp1 (nbv, indfl)
          !**  17.11.98 Csocsan
          vfl (isch, nbv) = vp (nbv, indfl)
          !** ende
          qvor (isch, nbv) = 0.0
          bvor (isch, nbv) = 0.0
          fvor (isch, nbv) = 0.0

          IF (indfmax.gt.1) then

            IF (indfl.gt.1) then

              DO j1 = 1, indfl - 1
                qvor (isch, nbv) = qvor (isch, nbv) + qtp1 (nbv, j1)
                bvor (isch, nbv) = bvor (isch, nbv) + brp1 (nbv, j1)
                fvor (isch, nbv) = fvor (isch, nbv) + fp1 (nbv, j1)
              END DO
            ENDIF
          ENDIF
          !**  18.11.98 Csocsan
          IF (indfl.lt.indfmax) then

            DO j1 = indfl + 1, indfmax
              qvor (isch, nbv) = qvor (isch, nbv) + qtp1 (nbv, j1)
              bvor (isch, nbv) = bvor (isch, nbv) + brp1 (nbv, j1)
              fvor (isch, nbv) = fvor (isch, nbv) + fp1 (nbv, j1)
            END DO

          ENDIF

        ENDIF

      !JK   ENDIF ZU (prof_it(j).eq.0)
      ENDIF

    END DO

    !************************************************
    !**    Ergaenzung vom 27.10.92 Ende
    !************************************************
    qvar = qvar + DELTA_Q

    IF (isch.ne.ianz) qschritt (isch + 1) = qvar

  END DO
  !JK ENDE BERECHNUNGSSCHLEIFE BORDVOLL-------------------------

  write (*, 1010)
  1010 format (//1X, 'Kontrolle der berechneten Abfluesse', /, &
               & 1X, '-----------------------------------')


  DO ji = 1, nbv

    ikenn = 0
    ischnitt = 0

    DO i = 1, ianz

      dif (i) = hwsp (i, ji) - hbv (ji)
      IF (i.eq.1.) then
        IF (dif (i) .ge.0.) then
          ischnitt = ischnitt + 1
        ENDIF

      ELSE

        IF (abs (dif (i) ) .le.1.e-06) then

          ischnitt = ischnitt + 1
          ikenn = i

        ELSEIF (dif (i - 1) .le.0..and.dif (i) .ge.0.) then

          ischnitt = ischnitt + 1

          IF (ischnitt.gt.1) then
            CONTINUE
            ! d.h. nimm den kleinsten bordvoll-wert
          ELSE
            IF (abs (dif (i - 1) ) .gt.abs (dif (i) ) ) then
              ikenn = i
            ELSE
              ikenn = i - 1
            ENDIF
          ENDIF

        ENDIF

      ENDIF

    END DO

    IF (ikenn.ne.0) then

      hbord (ji) = hwsp (ikenn, ji)
      !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !**  qbord(ji)=qbv(ikenn,ji)
      qbord (ji) = qschritt (ikenn)
      vbord (ji) = vbv (ikenn, ji)
      !**  WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !**  print *,ji,ikenn,hbord(ji),qbord(ji)
      jkenn (ji) = 0

      !EP Aenderung Pasche vom 03.06.2002
      !EP ischbv wird bei Bovog-Subroutine gebraucht, um die Stelle im Pro....
      !EP zu identifizieren, bei der Bordvoller Abfluss auftritt

      ischbv (ji) = ikenn

    ELSE

      hbord (ji) = 0.
      qbord (ji) = 0.
      vbord (ji) = 0.
      jkenn (ji) = 1

    ENDIF

    write (UNIT_OUT_LOG, 1002) stat(ji), 'Q-Werte', 'Differenz', 'Kennung'
    1002 format (//1X, '*****************************************************', /, &
                 & 1X, 'Ergebnisse Bordvollberechnung an Station ', f12.4, /, &
                 & 1X, '*****************************************************', //, &
                 & 1X, A12, A12, A12)

    DO isch = 1, ianz
      WRITE (UNIT_OUT_LOG, 1003) qschritt(isch), dif(isch), ikennung(isch,ji)
    END DO
    1003 format (1X, F12.3, F12.3, I12)


    IF (ikenn.ne.0) then
      WRITE (UNIT_OUT_LOG, 1004) ikenn, qschritt(ikenn)
      1004 format (/1X, '--> Bordvoll beim ',I4,'-ten Schritt mit Q-Bordvoll = ', F12.3, ' m3/s.')
      write (*,1005) stat(ji), qschritt(ikenn)
      1005 format (/1X, 'Bei Station ', F12.4, ' liegt bordvoller Abfluss bei Q = ', F12.3, ' m3/s')
    ELSE
      WRITE (UNIT_OUT_LOG, 1006)
      1006 format (/1X, '--> Bordvoller Abfluss nicht gefunden!')
      write (*, 1007) stat(ji)
      1007 format (/1X, 'Achtung! Bordvoller Abfluss bei Station ', F12.4, ' nicht gefunden!')
    ENDIF

  END DO

  mark = 2



!----------------------------------------------------------------------------------------------
ELSE
  ! Stationaer-gleichfoermige Bordvoll Berechnung

  ilen = LEN_TRIM (unit1)

  DO i1 = 1, maxger
    jkenn (i1) = 1
  END DO

  IF (km .ne. 'j') then         !WP O H N E  Kalinin-Mijukov-Parameter

    !ilen = ju0nch (unit1)
    ilen = LEN_TRIM (unit1)

    !WP 10.05.2005
    !write (*,*) 'In QBORDV:  km    = ',km
    !write (*,*) 'In QBORDV:  ilen  = ',ilen
    !write (*,*) 'In QBORDV:  unit1 = ',unit1

    CALL wspber ()

    mark = 4
    nbv = nprof

  ELSE                          !WP M I T  Kalinin-Mijukov-Parameter

    ilen = LEN_TRIM (unit1)

    DO 800 isch = 1, 6
      i_km = isch

      IF (i_km.eq.1) then
        unit1 (ilen + 1:nch80) = '_1.tab'
      ELSEIF (i_km.eq.2) then
        unit1 (ilen + 1:nch80) = '_2.tab'
      ELSEIF (i_km.eq.3) then
        unit1 (ilen + 1:nch80) = '_3.tab'
      ELSEIF (i_km.eq.4) then
        unit1 (ilen + 1:nch80) = '.tab'
      ELSEIF (i_km.eq.5) then
        unit1 (ilen + 1:nch80) = '_5.tab'
      ELSE
        unit1 (ilen + 1:nch80) = '_6.tab'
      ENDIF

      CALL wspber ()
      mark = 4

      IF (i_km.eq.4) then
        DO jj = 1, nprof
          qbord (jj) = qs (jj)
          hbord (jj) = wsp (jj)
          ischbv (jj) = i_km
        END DO
      ENDIF

      nbv = 0


      DO jn = 1, nprof

        IF (prof_it (jn) .eq.0) nbv = nbv + 1

        indfmax = ind (jn)
        indfl = indf (jn)
        hwsp (isch, nbv) = wsp (jn)

        qq (isch, nbv) = qtp1 (nbv, indfl)
        bb (isch, nbv) = brp1 (nbv, indfl)
        fb (isch, nbv) = fp1 (nbv, indfl)

        !**  17.11.98 Csocsan
        vfl (isch, nbv) = vp (nbv, indfl)

        qvor (isch, nbv) = 0.0
        bvor (isch, nbv) = 0.0
        fvor (isch, nbv) = 0.0

        IF (indfmax.gt.1) then
          IF (indfl.gt.1) then
            DO j1 = 1, indfl - 1
              qvor (isch, nbv) = qvor (isch, nbv) + qtp1 (nbv, j1)
              bvor (isch, nbv) = bvor (isch, nbv) + brp1 (nbv, j1)
              fvor (isch, nbv) = fvor (isch, nbv) + fp1 (nbv, j1)
            END DO
          !**  17.11.98 Csocsan
          ENDIF
        ENDIF

        !**  17.11.98 Csocsan
        IF (indfl.lt.indfmax) then
          DO j1 = indfl + 1, indfmax
            qvor (isch, nbv) = qvor (isch, nbv) + qtp1 (nbv, j1)
            bvor (isch, nbv) = bvor (isch, nbv) + brp1 (nbv, j1)
            fvor (isch, nbv) = fvor (isch, nbv) + fp1 (nbv, j1)
          END DO
        ENDIF

      END DO

    800 END DO

  !JK   ENDIF ZU (km.ne."j")
  ENDIF


ENDIF


! ------------------------------------------------------------------
! Bestimmung von K-M Parameter
! ------------------------------------------------------------------

IF (km.eq.'j') then

  ilen = LEN_TRIM(NAME_OUT_WSL)
  NAME_OUT_GER = NAME_OUT_WSL(1:ilen) // '.ger'
  write (*,*) 'NAME_OUT_GER = ', NAME_OUT_GER

  UNIT_OUT_GER = ju0gfu ()
  OPEN (UNIT=UNIT_OUT_GER, FILE=NAME_OUT_GER, STATUS='REPLACE', ACTION='WRITE', IOSTAT=istat)
  if (istat/=0) then
    write (*,*) 'Fehler beim Oeffnen von ', NAME_OUT_GER
    stop
  end if


  ilen = LEN_TRIM(NAME_PFAD_DATH)
  ilen2 = LEN_TRIM(FLUSSNAME)
  NAME_OUT_LOG_KM = NAME_PFAD_DATH(1:ilen) // 'out.'// FLUSSNAME(1:ilen2)
  write (*,*) 'NAME_OUT_LOG_KM = ', NAME_OUT_LOG_KM

  UNIT_OUT_LOG_KM = ju0gfu ()
  OPEN (UNIT=UNIT_OUT_LOG_KM, FILE=NAME_OUT_LOG_KM, STATUS='REPLACE', ACTION='WRITE', IOSTAT=istat)
  if (istat/=0) then
    write (*,*) 'Fehler beim Oeffnen von ', NAME_OUT_LOG_KM
    stop
  end if


  IF (BERECHNUNGSMODUS /= 'BF_NON_UNI') then
    DELTA_Q = 0.0
    MAX_Q = 1.e+06
    MIN_Q = 1.e-06
  ENDIF

  ! WP Neue Ausgabe fuer die Berechnung der KM-Paramter --------------------
  call schreib_erg_stat_unglf(ianz, nprof, DELTA_Q, MAX_Q, MIN_Q, stat)
  ! WP ---------------------------------------------------------------------

  ! ---------------------------------------------------------------
  ! AUFRUF KM-VERFAHREN
  CALL bovog1 (nbv, DELTA_Q, MAX_Q, MIN_Q)
  ! ---------------------------------------------------------------

  CLOSE (UNIT_OUT_GER)
  CLOSE (UNIT_OUT_LOG_KM)

ENDIF



! ------------------------------------------------------------------
! Erstellen '.qbv'-file
! ------------------------------------------------------------------


write (*,9000)
9000 format (///1X, 'Der Abflusslaengsschnitt fuer die Bordvoll-Berechnung wird erstellt.', /, &
             & /1X, 'Es werden 2 Dateien angelegt:', /, &
             &  1X, '   1. Datei (*.qb1):  Hydrologische Laengsschnitt ', /, &
             &  1X, '   2. Datei (*.qb2):  Hydraulischer Laengsschnitt ', /)


unit2 = NAME_PFAD_DATH
ilen = LEN_TRIM(unit2)

ifllen = LEN_TRIM(FLUSSNAME)                     ! FLUSS ist Gewaessername aus Oberflaeche z.B. Stoer

IF (ifllen.gt.8) then
  ifllen = 8
ENDIF


unit2 (ilen + 1:nch80) = FLUSSNAME(1:ifllen)    ! Es soll der vollstaendige Dateiname *.qb1 und *.qb2
						! in dem Unterordner \DATH erzeugt werden.
ilen = LEN_TRIM(unit2)                          ! Neue Laenge des Dateinamens
unit2 (ilen + 1:nch80) = '.qb1'                 ! Datei wird mit der Endung .qb1 versehen
WRITE ( * , '('' Name 1.Datei: '',a)') unit2 (1:ilen + 5)

!HB   damit der Pfad (unit2) dieser ersten Datei (*.qb1) nach SUB zeila
!HB   uebergeben werden kann, wird er umbenannt.
!HB   Das Problen war, dass unit2 ein weiteres mal fuer Datei (*.qb2)
!HB   verwendet wird.
pfad_qb1 = unit2                                ! Vollstaendiger Dateiname der *.qb1-Datei
!write (*,*) 'In QBORDV. pfad_qb1 = ', pfad_qb1


!Hier wird dem Dateipfad unit2 eine neue Endung gegeben
unit2 (ilen + 1:nch80) = '.qb2'
WRITE ( * , '('' Name 2.Datei: '',a)') unit2 (1:ilen + 5)
!HB   damit der Pfad (unit2) dieser zweiten Datei (*.qb2) nach SUB zeila
!HB   ohne Verwechslungen uebergeben werden kann, wird er umbenannt.
pfad_qb2 = unit2
!write (*,*) 'In QBORDV. pfad_qb2 = ', pfad_qb2



! Zuweisung des Pfades fuer NAME_OUT_LAENGS in MAIN (=wsp.f90)
!MD CALL lapro1 (pfad_qb1, pfad_qb2, nbv, mark, NAME_OUT_LAENGS)
CALL lapro1 (pfad_qb1, pfad_qb2, nbv, NAME_OUT_LAENGS, NAME_OUT_QLAENGS)

END SUBROUTINE qbordv





!------------------------------------------------------------------------------------
subroutine schreib_erg_stat_unglf(ianz, nprof, qstep, rqmax, rqmin, stat)
!
! Beschreibung:
! -------------
! Die Ergebnisse der Spiegellinienberechnung mit variablem Abfluss werden
! für jedes einzelne Profil in die entsprechende *.km Datei im Ordner /PROF/
! ausgegeben. Hierbei muss die die Berechnung von Kalinin-Miljukow-Parametern
! aktiviert sein.
!                              Wolf Plöger, 11.03.2006
! -----------------------------------------------------------------------------------

USE DIM_VARIABLEN
USE AUSGABE_LAENGS
USE MOD_ERG
USE IO_UNITS

implicit none

! Calling variables
INTEGER, INTENT(IN) 	:: ianz                 ! Anzahl der Abfluesse
INTEGER, INTENT(IN) 	:: nprof                ! Anzahl der Profile
REAL, INTENT(IN)        :: qstep                ! Schrittweite der Abfluesse
REAL, INTENT(IN)        :: rqmax                ! Maximaler Abfluss
REAL, INTENT(IN)        :: rqmin                ! Minimaler Abfluss
REAL, INTENT(IN)        :: stat(maxger)         ! Stationierung der Profile


! COMMON-Block /DATEINAME_PROF/ ----------------------------------------------------
CHARACTER(LEN=nch80) :: dateiname(maxger)
COMMON / dateiname_prof / dateiname
! ----------------------------------------------------------------------------------


! COMMON-Block /UEBER_KM/ -----------------------------------------------------
REAL 	:: qq (100, maxger), bb (100, maxger), fb (100, maxger)
REAL 	:: vfl (100, maxger), qvor (100, maxger), bvor (100, maxger)
REAL 	:: fvor (100, maxger)
INTEGER :: ischbv (maxger)
COMMON / ueber_km / qq, bb, fb, vfl, qvor, bvor, fvor, ischbv
! -----------------------------------------------------------------------------


! COMMON-Block /PROF/ ---------------------------------------------------------
REAL 	:: hwsp (100, maxger)
COMMON / prof / hwsp
! -----------------------------------------------------------------------------


! Local variables
INTEGER :: i, j, ilen, istat, anz_prof_orig, nr_prof
INTEGER :: ju0gfu
CHARACTER(LEN=nch80) :: filename_out
CHARACTER(LEN=nch80) :: datei_km_out
REAL    :: I_wsp
REAL  	:: Q_fluss_temp, Q_vorland_temp
REAL  	:: A_fluss_temp, A_vorland_temp
REAL  	:: B_fluss_temp, B_vorland_temp
REAL    :: WSP_temp


! WP 13.03.2006, Bemerkungen
! --------------------------
! Bei unterschiedlichen Abfluessen können automatisch Profile interpoliert
! worden sein (z.B. bei Bruecken, aber auch wenn keine Konvergenz erzielt werden kann).
! Da die urspruengliche Variable fuer die Profilanzahl NPROF kein Array ist,
! kann sie diese Variabilität nicht beruecksichtigen
! Durch die Einfuehrung der globalen Variablen ANZ_PROF(i) wird jedem
! Abflusszustand die Anzahl der Profile zugewisen.
! Bei der Datenausgabe sollen *.KM-Dateien fuer die wirklich physikalisch
! vorhandenen Profile ausgegeben werden.

anz_prof_orig = 0     ! Anzahl der "echten" nicht-interpolierten Profile

! Fuer das Suchen der "echten" Profile reicht es, nur einen
! Abfluss zu durchsuchen, da die Anzahl fuer alle Abfluesse
! gleich sein muss.
do i = 1, anz_prof(1)

  if (out_PROF(i,1)%interpol) cycle      ! Wenn Profil interpoliert

  anz_prof_orig = anz_prof_orig + 1

end do

!write (*,*) ' Anzahl der original Profile = ', anz_prof_orig, ' Anzahl Abfluesse = ', anz_q

Schieben: do j = 1, anz_q

  !write (*,*) ' Anzahl Profile bei Abfluss ', j, ' = ', anz_prof(j)

  nr_prof = 1  ! Profil Nummer

  do

    if (nr_prof > anz_prof(j)) EXIT

    if (out_PROF(nr_prof,j)%interpol) then
      ! Bei Abfluss j ist Profil nr_prof interpoliert
      ! -> verschieben aller Eintraege von out_PROF um ein Profil nach links
      !write (*,*) 'In Profil ', nr_prof, ' muss bei Abfluss ', j, ' geschoben werden.'
      call shift_left_array_out(nr_prof, j)

      ! Nocheinmal von Anfang an durchgehen
      nr_prof = 1

      CYCLE

    end if

    nr_prof = nr_prof + 1

  end do

end do Schieben






! -------------------------------------------------------------------------------------
! Hauptschleife
alle_profile: do i = 1, anz_prof_orig

  ilen = LEN_TRIM(dateiname(i))
  datei_km_out = dateiname(i)
  datei_km_out(ilen-2:ilen) = 'km '
  UNIT_OUT_KM = ju0gfu()

  open (UNIT=UNIT_OUT_KM, FILE=datei_km_out, STATUS='REPLACE', ACTION='WRITE', IOSTAT=istat)
  if (istat /= 0) then
    write (*,9001)
    9001 format (/1X, 'Problem beim Oeffnen der KM-OUT Datei')
    CYCLE
  end if

  write (UNIT_OUT_KM, 1001) stat(i), out_PROF(i,1)%hbv
  1001 format (1X, F12.4, '  Station [km]', F15.4, '  Bordvolle Hoehe [mNN]'/)


  write (UNIT_OUT_KM, 1002) 'NR', ' Wasserspiegel- ', '    Abfluss     ', '  Abfluss   ', '    Flaeche    ', '  Flaeche   ', &
                             &    '    Breite     ', '  Breite   ', ' Wasserspiegel- '
  write (UNIT_OUT_KM, 1002) ' ',  '     hoehe      ', '  Flussschlauch ', '  Vorland   ', ' Flussschlauch ', '  Vorland   ', &
                             &    ' Flussschlauch ', '  Vorland  ', '   gefaelle     '
  write (UNIT_OUT_KM, 1002) ' ',  '     [mNN]      ', '     [m3/s]     ', '   [m3/s]   ', '     [m2]      ', '    [m2]    ', &
                             &    '      [m]      ',  '   [m]    ', '       [-]      '
  1002 format (1X, A5, 8A16)

  Alle_Abfluesse: do j = 1, ianz

    ! Für das erste Profil eines Abschnittes wird das Reibungsgefaelle bzw. die gesamte
    ! Verlusthoehe nicht berechnet. Da fuer das Kalinin-Miljukow-Verfahren jedoch
    ! nur das Wasserspiegelgefaelle von Interesse ist (zur Bestimmung des vorhandenen
    ! Wasservolumens) kann es auch direkt aus der Wasserspiegelhoehe und dem Profilabstand
    ! berechnet werden. Fuer das erste Profil wird das Gefaelle zwischen ersten und zweitem
    ! Profil berechnet, fuer die folgenden Profile immer das Gefaelle zwischen
    ! aktuellem und unterhalb liegendem Profil.
    if (i == 1) then
      ! Falls es sich um das erste Profil eines Abschnittes handelt
      I_wsp = (out_PROF(i+1,j)%wsp - out_PROF(i,j)%wsp) / (1000.0 * ABS (out_PROF(i+1,j)%stat - out_PROF(i,j)%stat) )
    else
      ! Fuer alle anderen Profile
      I_wsp = (out_PROF(i,j)%wsp - out_PROF(i-1,j)%wsp) / (1000.0 * ABS (out_PROF(i,j)%stat - out_PROF(i-1,j)%stat) )
    end if

    WSP_temp = out_PROF(i,j)%wsp

    Q_fluss_temp = out_IND(i,j,2)%Q
    Q_vorland_temp = out_IND(i,j,1)%Q + out_IND(i,j,3)%Q
    A_fluss_temp = out_IND(i,j,2)%A
    A_vorland_temp = out_IND(i,j,1)%A + out_IND(i,j,3)%A
    B_fluss_temp = out_IND(i,j,2)%B
    B_vorland_temp = out_IND(i,j,1)%B + out_IND(i,j,3)%B

    !write (UNIT_OUT_KM, 1003) j, hwsp(j,i), qq(j,i), qvor(j,i), fb(j,i), fvor(j,i), bb(j,i), bvor(j,i), I_wsp
    write (UNIT_OUT_KM, 1003) j, WSP_temp, &
                            & Q_fluss_temp, Q_vorland_temp, &
                            & A_fluss_temp, A_vorland_temp, &
                            & B_fluss_temp, B_vorland_temp, &
                            & I_wsp                           
    1003 format (1X, I5, F16.4, F16.4, F16.4, F16.4, F16.4, F16.4, F16.4, F16.10)

  end do Alle_Abfluesse

  close (UNIT_OUT_KM)

end do alle_profile


end subroutine schreib_erg_stat_unglf



!------------------------------------------------------------------------------------
subroutine shift_left_array_out(nr_prof, j)
!
! Beschreibung:
! -------------
! Aufgrund der Komplexitaet der verwendeten Berechnungsmethoden, werden
! in bestimmten Faellen (z.B. Bruecken) Profile interpoliert. Die Anzahl
! der interpolierten Profile haengt von dem jeweiligen Abflussereignis.
!
! AbflussNr   Stat[km]  Stat[km]  Stat[km]  Stat[km] Stat[km]  Stat[km]  Stat[km]  Stat[km]
!    1         1.000     1.500     1.520     1.600    1.980     2.000     2.020     2.500
!    2         1.000     1.500     1.600     1.980    2.000     2.020     2.500
!    3         1.000     1.500     1.600     2.000    2.500
!    4         1.000     1.500     1.520     1.600    1.980     2.000     2.500
!    5         1.000     1.500     1.600     1.980    2.000     2.500
!
! In diesem Beispiel sind alle ungeraden Werte (1.520, 1.980, 2.020)
! interpolierte Profile. Die Urspruenglich 5 Profile wurden bei Abfluss
! Nr. 1 auf 8 Profile erweitert.
! Fuer die Ausgabe von WQ-Beziehungen bzw. Tabellen fuer die Kalinin-Miljukow
! Berechnung muessen die interpolierten Werte entfernt werden und der
! Rest der Zeile nach links geschoben werden.
!
!                              Wolf Plöger, 11.03.2006
! -----------------------------------------------------------------------------------

USE MOD_ERG

implicit none

! Calling variables
INTEGER, INTENT(IN) :: nr_prof
INTEGER, INTENT(IN) :: j

! Local variables
INTEGER :: i, k

! Ausgehend von dem zu loeschenden Eintrag nr_prof werden alle folgenden Eintraege
! im Array nach links verschoben
do i = nr_prof, anz_prof(j)-1

  do k = 1, 3
    out_IND(i,j,k)%lambda = out_IND(i+1,j,k)%lambda
    out_IND(i,j,k)%formb  = out_IND(i+1,j,k)%formb
    out_IND(i,j,k)%A      = out_IND(i+1,j,k)%A
    out_IND(i,j,k)%B      = out_IND(i+1,j,k)%B
    out_IND(i,j,k)%lu     = out_IND(i+1,j,k)%lu
    out_IND(i,j,k)%v      = out_IND(i+1,j,k)%v
    out_IND(i,j,k)%Q      = out_IND(i+1,j,k)%Q     
  end do

  out_PROF(i,j)%stat 		= out_PROF(i+1,j)%stat
  out_PROF(i,j)%wsp 		= out_PROF(i+1,j)%wsp
  out_PROF(i,j)%hen 		= out_PROF(i+1,j)%hen
  out_PROF(i,j)%hbv 		= out_PROF(i+1,j)%hbv
  out_PROF(i,j)%sohle 		= out_PROF(i+1,j)%sohle
  out_PROF(i,j)%qges 		= out_PROF(i+1,j)%qges
  out_PROF(i,j)%boeli 		= out_PROF(i+1,j)%boeli
  out_PROF(i,j)%boere 		= out_PROF(i+1,j)%boere
  out_PROF(i,j)%vm 		= out_PROF(i+1,j)%vm
  out_PROF(i,j)%tau 		= out_PROF(i+1,j)%tau
  out_PROF(i,j)%hvm 		= out_PROF(i+1,j)%hvm
  out_PROF(i,j)%hrm 		= out_PROF(i+1,j)%hrm
  out_PROF(i,j)%hein 		= out_PROF(i+1,j)%hein
  out_PROF(i,j)%hort 		= out_PROF(i+1,j)%hort
  out_PROF(i,j)%hm 		= out_PROF(i+1,j)%hm
  out_PROF(i,j)%WehrOK 		= out_PROF(i+1,j)%WehrOK
  out_PROF(i,j)%BrueckOK	= out_PROF(i+1,j)%BrueckOK
  out_PROF(i,j)%BrueckUK  	= out_PROF(i+1,j)%BrueckUK
  out_PROF(i,j)%BrueckB  	= out_PROF(i+1,j)%BrueckB
  out_PROF(i,j)%RohrD  		= out_PROF(i+1,j)%RohrD
  out_PROF(i,j)%interpol	= out_PROF(i+1,j)%interpol
  out_PROF(i,j)%chr_kenn	= out_PROF(i+1,j)%chr_kenn
  out_PROF(i,j)%alphaIW         = out_PROF(i+1,j)%alphaIW
  out_PROF(i,j)%alphaEW         = out_PROF(i+1,j)%alphaEW
  out_PROF(i,j)%gefaelle        = out_PROF(i+1,j)%gefaelle
end do

anz_prof(j) = anz_prof(j) - 1

!write (*,*)
!write (*,*) ' Nach Verschiebung noch ',anz_prof(j),' uebrig:'
!do i = 1, anz_prof(j)
!  write (*,*) i,' station = ', out_PROF(i,j)%stat
!end do

end subroutine shift_left_array_out

