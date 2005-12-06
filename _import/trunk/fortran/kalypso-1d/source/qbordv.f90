!     Last change:  WP    6 Dec 2005    6:09 pm
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

SUBROUTINE qbordv (rqmax, rqmin, qstep, unit1, ibruecke, wehr)

!***********************************************************************
!
!     geschrieben: p.koch    juli'90                                    
!     ------------------------------
!     programmbeschreibung:                                             
!                                                                       
!     Dieses Programm steuert die Bordvoll-Berechnung, d.h. von hier
!     aus wird das Berechnungsprogramm WSPBER fuer das jeweilige q neu
!     gestartet.                                                        
!
!     Ergebnis-files der Bordvoll-Berechnung:
!                                                                       
!     plot-files:                                                       
!     -  abflusslaengsschnitt            flussname.qbv                  
!     -  wasserspiegellaengsschnitt jeweils mit eintrag des bordvollen  
!        wasserspiegels (nur zu kontrollzwecken                         
!                        hwsp(bordvoll)=min h(boeschungskante))         
!                                        flussname.wsp                  
!                                                                       
!     daten-files:                                                      
!     - wasserstands-abflussbeziehung    flussname(station).pro         
!     - wasserspiegellage fuer jedes q   flussname(q).tab               
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
!   jw8     --      Name des Kontrollfiles
!   k_kp    --
!   lein    --      Art des Ergebnisausdruckes
!   nbv     --      Nummer des Bordvollereignisses
!   nprof   --      Anzahl der Profilpunkte
!   q       --      jeweiliger Abfluß
!   qbord   --      Abfluß bei Bordvoll
!   qschritt--      jeweiliger Abfluß
!   qstep   --      Schrittweite des Abflusses
!   qvar    --      jeweiliger Abfluß
!   rqmax   --      maximaler Abfluß
!   rqmin   --      minimaler Abfluß
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
!JK     lu0trf                                                          
!JK     u0ljst                                                          
!     ******************************************************************



!------------------------------------------------------------------------------
! VEREINBARUNGEN
!------------------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

! Calling Variables -----------------------------------------------------------
REAL, INTENT(INOUT) 		:: rqmax               ! Maximaler Abfluss
REAL, INTENT(INOUT) 		:: rqmin               ! Minimaler Abfluss
REAL, INTENT(INOUT) 		:: qstep               ! Minimaler Abfluss
CHARACTER(LEN=1), INTENT(IN) 	:: ibruecke            ! Kennung, ob Bruecke gerechnet werden soll (= 'j' oder 'n')
CHARACTER(LEN=1), INTENT(IN) 	:: wehr                ! Kennung, ob Wehr gerechnet werden soll (= 'j' oder 'n')


! COMMON-Block /AUSGABEART/ ---------------------------------------------------
INTEGER :: lein                 ! Art der Kontrollausgabe
!   lein=1    --> einfacher ergebnisausdruck
!   lein=2    --> erweiterter ergebnisausdruck
!   lein=3    --> erstellung kontrollfile
INTEGER :: jw8                  ! UNIT der Kontrolldatei
COMMON / ausgabeart / lein, jw8
! -----------------------------------------------------------------------------


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

INTEGER :: i
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

!ST 29.03.2005
!Variable für Laengschnitt.txt
CHARACTER(LEN=nch80) :: file_laengs

REAL :: qschritt (maxger)
!     wird nur zum ausdruck des kontrollfiles bei q-bordvoll genutzt
REAL :: vbv (100, maxger)

file_laengs = ' '


!------------------------------------------------------------------
! BERECHNUNGEN
!------------------------------------------------------------------

!write (*,*) 'Jetzt in QBORDV. lein = ', lein

IF (lein.ne.3) then
  ! ACHTUNG: Nur wenn Kontrolldatei NICHT aktiviert ist, wird eine neue aufgerufen!

  ! Kontrollfile anlegen
  jw8 = ju0gfu ()       		! Eine freie UNIT-Nummer suchen
  ierr = 0
  unit7 = fnam1
  ilen = LEN_TRIM (unit7)
  unit7 (ilen - 4:ilen - 1) = 'dath'
  unit7 (ilen + 1:nch80) = 'Kontroll'
  OPEN (UNIT = jw8, FILE = unit7, STATUS = 'REPLACE', IOSTAT = ierr)

  write (*,1000)
  1000 format (/1X, ' KONTROLL-Datei fuer Bordvollberechnung wird angelegt.', /)

ENDIF

!write (*,*) 'FNAM1 = ', fnam1
!write (*,*) 'UNIT1 = ', unit1

unit1 = fnam1

ilen = LEN_TRIM (unit1)
unit1 (ilen - 4:ilen - 1) = 'dath'
ifllen = LEN_TRIM (fluss)

IF (ifllen.gt.4) then
  ifllen = 4
ENDIF

unit1 (ilen + 1:nch80) = fluss (1:ifllen)

!HB   Anfuegen der Endung '.tab' an den Pfad unit1.
!HB   Es wird von Position 'ilen+5' bis zur 80. Position, da
!HB   'nch80'=80, '.tab' angehaengt.
unit1 (ilen + 5:nch80) = '.tab'

unit2 = unit1



!JK   WENN STATIONAER-UNGLEICHFOERMIGE ABFLUSSVERHAELTNISSE
!--------------------------------------------------------------------------------------------------
IF (bordvoll.eq.'u') then

  DO i = 1, maxger
    jkenn (i) = 0
  END DO

  ! ******************************************************************
  ! Berechnung der schrittanzahl fuer die bordvollberechnung
  ! ******************************************************************

  ianz = ifix ( (rqmax - rqmin) / qstep + 0.1)
  ianz = ianz + 1

  ! anfangswert fuer q:

  qvar = rqmin
  qschritt (1) = qvar


  ! ******************************************************************
  ! Schleife zur Bordvoll-Berechung, ISCH ist Zaehler fuer Abflusses
  ! ******************************************************************

  DO isch = 1, ianz

    q = qvar

    nbv = 0

    write (*, 1001) qvar
    write (0, 1001) qvar
    !JK SCHREIBEN IN KONTROLLFILE
    IF (lein.eq.3) then
      write (jw8, 1001) qvar
    ENDIF
    1001 format (//1X, '*************************************************', /, &
                 & 1X, 'Bordvoll-Berechung --> Q=', F10.3, ' m**3/s', /, &
                 & 1X, '*************************************************', /)


    !JK  ERSTELLEN WASSERSPIEGELLINIEN
    IF (idr2.eq.'j') then

      ! dateiname fuer ausgabedatei flussname..q..tab:
      WRITE (ereignis, '(f9.3)') qvar

      erlen = LEN_TRIM (ereignis)

      DO i = 1, erlen
        IF (ereignis (i:i) .eq.'.') then
          ereignis (i:i) = '-'
        ENDIF
      END DO

      ereignis = ADJUSTL (ereignis)
      unit1 = fnam1
      ilen = LEN_TRIM (unit1)
      unit1 (ilen - 4:ilen - 1) = 'dath'
      ifllen = LEN_TRIM (fluss)

      IF (ifllen.gt.2) then
        ifllen = 2
      ENDIF

      unit1 (ilen + 1:nch80) = fluss (1:ifllen)
      unit2 = unit1
      ilen = LEN_TRIM (unit1)
      unit1 (ilen + 1:nch80) = ereignis
      ilen = LEN_TRIM (unit1)
      unit1 (ilen + 1:nch80) = '.tb'

    ENDIF

    IF (qvar.gt.rqmax) then
      qvar = rqmax
    ENDIF

    CALL wspber (unit1, ibruecke, wehr)

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
    qvar = qvar + qstep

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

    write (jw8, 1002) stat(ji), 'Q-Werte', 'Differenz', 'Kennung'
    1002 format (//1X, '*****************************************************', /, &
                 & 1X, 'Ergebnisse Bordvollberechnung an Station ', f12.4, /, &
                 & 1X, '*****************************************************', //, &
                 & 1X, A12, A12, A12)

    DO isch = 1, ianz
      WRITE (jw8, 1003) qschritt(isch), dif(isch), ikennung(isch,ji)
    END DO
    1003 format (1X, F12.3, F12.3, I12)


    IF (ikenn.ne.0) then
      WRITE (jw8, 1004) ikenn, qschritt(ikenn)
      1004 format (/1X, '--> Bordvoll beim ',I4,'-ten Schritt mit Q-Bordvoll = ', F12.3, ' m3/s.')
      write (*,1005) stat(ji), qschritt(ikenn)
      1005 format (/1X, 'Bei Station ', F12.4, ' liegt bordvoller Abfluss bei Q = ', F12.3, ' m3/s')
    ELSE
      WRITE (jw8, 1006)
      1006 format (/1X, '--> Bordvoller Abfluss nicht gefunden!')
      write (*, 1007) stat(ji)
      1007 format (/1X, 'Achtung! Bordvoller Abfluss bei Station ', F12.4, ' nicht gefunden!')
    ENDIF

  END DO

  mark = 2


!JK   ELSE ZU (bordvoll.eq.'u') -> STATIONAER-GLEICHFOERMIG
!----------------------------------------------------------------------------------------------
ELSE

  !write (*,*) 'BORDVOLL = ', bordvoll

  !ilen = ju0nch (unit1)
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

    CALL wspber (unit1, ibruecke, wehr)

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

      CALL wspber (unit1, ibruecke, wehr)
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


!JK   ENDIF ZU (bordvoll.eq.'u')
ENDIF


! ------------------------------------------------------------------
! Bestimmung von K-M Parameter
! ------------------------------------------------------------------

IF (km.eq.'j') then
  unit3 = unit2
  ilen3 = LEN_TRIM (unit3)
  unit3 (ilen3 + 1:nch80) = '.ger'

  iw11 = ju0gfu ()
  OPEN (unit = iw11, file = unit3, status = 'unknown')

  unit4 = fnam1
  ilen4 = LEN_TRIM (unit4)
  unit4 (ilen4 - 4:ilen4 - 1) = 'dath'
  unit4 (ilen4 + 1:ilen4 + 4) = 'out.'
  unit4 (ilen4 + 5:nch80) = fluss

  iw12 = ju0gfu ()
  OPEN (unit = iw12, file = unit4, status = 'unknown')

  IF (bordvoll.ne."u") then
    qstep = 0.0
    rqmax = 1.e+06
    rqmin = 1.e-06
  ENDIF

  call schreib_erg_stat_unglf(ianz, nprof, qstep, rqmax, rqmin, fnam1, stat)

  ! ---------------------------------------------------------------
  ! AUFRUF KM-VERFAHREN
  CALL bovog1 (iw11, iw12, nbv, qstep, rqmax, rqmin)
  ! ---------------------------------------------------------------

  CLOSE (iw11)
  CLOSE (iw12)

ENDIF



! ------------------------------------------------------------------
! Erstellen '.qbv'-file
! ------------------------------------------------------------------


write (*,9000)
9000 format (///1X, 'Der Abflusslaengsschnitt fuer die Bordvoll-Berechnung wird erstellt.', /, &
             & /1X, 'Es werden 2 Dateien angelegt:', /, &
             &  1X, '   1. Datei (*.qb1):  Hydrologische Laengsschnitt ', /, &
             &  1X, '   2. Datei (*.qb2):  Hydraulischer Laengsschnitt ', /)



unit2 = fnam1                                   ! FNAM1 ist der Projektpfad ohne /DATH und /PROF


ilen = LEN_TRIM(unit2)

unit2 (ilen - 4:ilen - 1) = 'dath'              ! Vollstaendiger Pfad zu dem Unterordner \DATH
ifllen = LEN_TRIM(fluss)                        ! FLUSS ist Gewaessername aus Oberflaeche z.B. Stoer

IF (ifllen.gt.8) then
  ifllen = 8
ENDIF

unit2 (ilen + 1:nch80) = fluss (1:ifllen)       ! Es soll der vollstaendige Dateiname *.qb1 und *.qb2
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

!     Hier wird dem Dateipfad unit2 eine neue Endung gegeben
unit2 (ilen + 1:nch80) = '.qb2'


WRITE ( * , '('' Name 2.Datei: '',a)') unit2 (1:ilen + 5)


!HB   damit der Pfad (unit2) dieser zweiten Datei (*.qb2) nach SUB zeila
!HB   ohne Verwechslungen uebergeben werden kann, wird er umbenannt.
pfad_qb2 = unit2
!write (*,*) 'In QBORDV. pfad_qb2 = ', pfad_qb2

!write (*,*) 'In QBORDV. file_laengs = ', file_laengs


!HB   alt:   call lapro1(iw1,iw2,nbv,mark,antwort)
!HB   Aenderung von iw1 in pfad_qb1, da lapro einen Dateipfad und
!HB   keine Dateinummer erwartet. So wird der Dateipfad der
!HB   Datei *.qb1 durch pfad_qb1 an lapro uebergeben.
!HB   Aenderung von iw2 in pfad_qb2, da lapro einen Dateipfad und
!HB   keine Dateinummer erwartet. So wird der Dateipfad der
!HB   Datei *.qb2 durch pfad_qb2 an lapro uebergeben.

!ST 29.03.2005--------------------------------------
!CALL lapro1 (pfad_qb1, pfad_qb2, nbv, mark, antwort)
CALL lapro1 (pfad_qb1, pfad_qb2, nbv, mark, file_laengs)
!ST ------------------------------------------------

IF (lein.ne.3) close (jw8)
                                                                        
END SUBROUTINE qbordv




!------------------------------------------------------------------------------------
subroutine schreib_erg_stat_unglf(ianz, nprof, qstep, rqmax, rqmin, fnam1, stat)

USE DIM_VARIABLEN
USE AUSGABE_LAENGS

implicit none

! Calling variables
INTEGER, INTENT(IN) 	:: ianz
INTEGER, INTENT(IN) 	:: nprof
REAL, INTENT(IN)        :: qstep
REAL, INTENT(IN)        :: rqmax
REAL, INTENT(IN)        :: rqmin
CHARACTER(LEN=nch80), INTENT(IN) :: fnam1
REAL, INTENT(IN)        :: stat(maxger)


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
INTEGER :: i, j, ilen, ierr
INTEGER :: ju0gfu
INTEGER :: unit_out, unit_km_out
CHARACTER(LEN=nch80) :: filename_out
CHARACTER(LEN=nch80) :: datei_km_out
REAL    :: I_wsp

unit_out = ju0gfu ()

filename_out = fnam1
ilen = LEN_TRIM (filename_out)
filename_out(ilen - 4:ilen - 1) = 'dath'
filename_out(ilen + 1:ilen + 9) = 'unglf.out'

OPEN (UNIT = unit_out, FILE = filename_out, STATUS = 'REPLACE', IOSTAT=ierr)
if (ierr /= 0) then
  write (0, 9000) filename_out
  write (*, 9000) filename_out
  9000 format (1X, 'Problem beim Oeffnen der Ausgabedatei ', A)
  return
end if

write (unit_out,1000) ianz, nprof, qstep, rqmax, rqmin
1000 format (1X, 'IANZ  = ', I12, /, &
           & 1X, 'NPROF = ', I12, /, &
           & 1X, 'QSTEP = ', F12.3, /, &
           & 1X, 'RQMAX = ', F12.3, /, &
           & 1X, 'RQMIN = ', F12.3, /)

do i = 1, nprof

  ilen = LEN_TRIM(dateiname(i))
  datei_km_out = dateiname(i)
  datei_km_out(ilen-2:ilen) = 'km '
  unit_km_out = ju0gfu()
  open (UNIT=unit_km_out, FILE=datei_km_out, STATUS='REPLACE', IOSTAT=ierr)
  if (ierr /= 0) then
    write (*,9001) datei_km_out
    9001 format (/1X, 'Problem beim Oeffnen der KM-OUT Datei', A)
    CYCLE
  end if

  write (unit_km_out, 1001) stat(i)
  write (unit_out, 1001) stat(i)
  1001 format (1X, F12.4, '  Station [km]', /)

  write (unit_km_out, 1002) 'NR', ' Wasserspiegel- ', '    Abfluss     ', '  Abfluss   ', '    Flaeche    ', '  Flaeche   ', &
                             &    '    Breite     ', '  Breite   ', ' Wasserspiegel- '
  write (unit_km_out, 1002) ' ',  '     hoehe      ', '  Flussschlauch ', '  Vorland   ', ' Flussschlauch ', '  Vorland   ', &
                             &    ' Flussschlauch ', '  Vorland  ', '   gefaelle     '
  write (unit_km_out, 1002) ' ',  '     [mNN]      ', '     [m3/s]     ', '   [m3/s]   ', '     [m2]      ', '    [m2]    ', &
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
      I_wsp = (hwsp(j,i+1)-hwsp(j,i)) / (1000.0 * ABS(stat(i+1)-stat(i)))
    else
      ! Fuer alle anderen Profile
      I_wsp = (hwsp(j,i) - hwsp(j,i-1)) / (1000.0 * ABS(stat(i)-stat(i-1)))
    end if

    write (unit_km_out, 1003) j, hwsp(j,i), qq(j,i), qvor(j,i), fb(j,i), fvor(j,i), bb(j,i), bvor(j,i), I_wsp
    1003 format (1X, I5, F16.4, F16.4, F16.4, F16.4, F16.4, F16.4, F16.4, F16.10)

  end do Alle_Abfluesse

  close (unit_km_out)

end do

close (unit_out)

end subroutine schreib_erg_stat_unglf

