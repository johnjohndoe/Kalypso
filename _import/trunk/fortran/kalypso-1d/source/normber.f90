!     Last change:  MD    8 Jul 2009    8:20 pm
!--------------------------------------------------------------------------
! This code, normb.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - normber
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



!-----------------------------------------------------------------------
SUBROUTINE normber (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax, &
                 & psiein, psiort, hgrenz, ikenn, froud, nblatt, nz, Q_Abfrage)

! geschrieben: P. Koch, 	Maerz 1990
! geaendert:   W. Ploeger, 	Juni 2006
!
! Programmbeschreibung:
! ---------------------
! Dieses Programm ermittelt den Wasserspiegel an einem Profil.
!
! ALS ERSTES WERDEN DIE BASISGEOMETRIEGROESSEN BERECHNET (SUB
! ABSKT), DANN BERECHNUNG DER GRENZTIEFE (SUB GRNZH). ES FOLGT DIE
! SCHAETZUNG DES ANFANGSWASSERSPIEGELS (SUB ANF), ANSCHLIESSEND
! WERDEN VERLUSTE (SUB VERLUSTE) BERECHNET, DER ANFANGS-
! ITERATIONSWERT hranf WIRD FESTGELEGT, ES WIRD NACH NEWTON (SUB
! NEWTON) ODER PEGASUS (SUB PEGASUS) ITERIERT, BERECHNUNG DER
! VERLUSTE, ERMITTELN EINZUSETZENDES GEFAELLE, AUFRUF VON SUB
! STATION, JE NACH GEFAELLEFALL UND OB STATIONAER-GLEICHFOERMIG.
!
!
! DIREKT UEBERGEBENE VARIABLEN
! ----------------------------
! str    - Abstand zwischen 2 Profilen
! q      - Durchfluss in bestimmten Faellen [m**3/s]
! q1     - alter Q-Wert in WSPBER [m**3/s]
! i      - ?
! hr     -
! hv     -
! rg     -
! hvst   -
! hrst   -
! indmax -
! psiein - VERLUSTE?
! psiort - OERTLICHE VERLUSTE?
! hgrenz -
! ikenn  -
! froud  -
! nblatt -
! nz     -
!
! PARAMETER
! ---------
! itmax  - max. Anzahl der Iterationsschritte
! err    - Genauigkeitsschranke fuer wsp-ermittlung bei Iteration
!
! AUFGERUFENE SUBROUTINEN
! -----------------------
! abskst(nknot,x1,xi,h1,hi,s)
! anf(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,
!     psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl,nblatt,nz)
! grnzh(q,indmax,hgrenz,xi,hi,s,ifgrnz)
! kopf(nblatt,nz,jw5,jw7,idr1)
! newton(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,
!        psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl)
! pegasus(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,
!         psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl)
! station(sgef,i,hr,q,hr,hv,rg,indmax,hvst,
!         hrst,psiein,psiort,jw5,
!         hi,xi,s,ikenn,froud,str,ifehl,nblatt,nz)
! verluste(str,q,q1,i,hr,hv,rg,hvst,
!          hrst,indmax,psiein,psiort,
!          jw5,hi,xi,s,istat,froud,ifehlg,1)
!
!********************************************************************
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS
USE MOD_INI

! Calling variables
REAL, INTENT(INOUT) :: str              ! Abstand zwischen zwei Profilen in [m]
REAL, INTENT(INOUT) :: q                ! Durchfluss in bestimmten Faellen [m3/s] (inout?)
REAL, INTENT(INOUT) :: q1               ! alter Q-Wert in WSPBER [m**3/s] (inout?)
INTEGER, INTENT(IN) :: i                ! entspricht nprof in WSPBER
REAL, INTENT(INOUT) :: hr               ! Verlusthoehe
REAL, INTENT(INOUT) :: hv               ! Verlusthoehe
REAL, INTENT(INOUT) :: rg               ! Verlusthoehe
REAL, INTENT(INOUT) :: hvst             ! Verlusthoehe
REAL, INTENT(INOUT) :: hrst             ! Verlusthoehe
INTEGER, INTENT(INOUT) :: indmax        ! Anzahl der Rauheitsabschnitte (1-3)
REAL, INTENT(INOUT) :: psiein           ! Verluste
REAL, INTENT(INOUT) :: psiort           ! Oertliche Verluste
REAL, INTENT(OUT) :: hgrenz             ! Grenztiefe am Profil
INTEGER, INTENT(INOUT) :: ikenn         ! Zustandskennung (= 0 fuer Normalberechnung, = 1 fuer Schiessen mit Grenztiefe)
REAL, INTENT(INOUT) :: froud            ! Froudezahl
INTEGER, INTENT(INOUT) :: nblatt        ! Anzahl der Seiten in Tabellenausgabe
INTEGER, INTENT(INOUT) :: nz        	! Anzahl der Zeilen in aktueller Seite in Tabellenausgabe


! COMMON-Block /ALT/ ---------------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
! ----------------------------------------------------------------------------------


! COMMON-Block /BV/ ----------------------------------------------------------------
CHARACTER(LEN=1):: idr1, idr2
INTEGER         :: nprof, isch
REAL            :: qvar
COMMON / bv / idr1, idr2, nprof, qvar, isch
! ----------------------------------------------------------------------------------


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


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! COMMON-Block /PEG/ ----------------------------------------------------------
REAL            :: h1x, h2x, f1, f2
COMMON / peg / h1x, h2x, f1, f2
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /ROHR/ ---------------------------------------------------------
INTEGER         :: idruck
COMMON / rohr / idruck
! -----------------------------------------------------------------------------


! COMMON-Block /VORT/ ---------------------------------------------------------
REAL 		:: hborda, heins, horts
COMMON / vort / hborda, heins, horts
! -----------------------------------------------------------------------------


! Local variables
REAL :: xi (maxkla), hi (maxkla), s (maxkla)
CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     !Abfrage fuer Ende der Inneren Q-Schleife

! ------------------------------------------------------------------
! werte vom vorhergehenden profil umspeichern
! (q-wert ist schon in wspber umgespeichert worden:)
!  q  - neuer q-wert
!  q1 - q-wert vorheriges profil
!  ws1,wsp - wasserspiegel
! ------------------------------------------------------------------


idruck = 0

! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------

if (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

  ws1 = wsp (i - 1)
  hv1 = hv
  rg1 = rg
  vmp1 = vmp (i - 1)
  fges1 = fges

  ikenn1 = ikenn
  ikenn = 0
  ifnew = 0
  ifpgs = 0
  ifgrnz = 0

  !**   siehe SUB STATIONAER

  istat = 0

  !**   /* stationaer ungleichfoermig

  izz = 0

  !**   Vorbelegungen fuer die Iteration:
  jschl = 0
  dx = 0.05

  !  ------------------------------------------------------------------
  !  Berechnung von Basisgeometriegroessen : Sehnen, Abstand zwischen
  !  den Profilpunkten (horizontal und veritkal)
  !  ------------------------------------------------------------------
  ! Input: nknot,x1,h1,maxkla
  ! Output: xi,hi,s
  CALL abskst (nknot, x1, xi, h1, hi, s, maxkla)


  ! ------------------------------------------------------------------
  ! Berechnung der Grenztiefe hgrenz
  ! ------------------------------------------------------------------
  CALL grnzh (q, indmax, hgrenz, xi, hi, s, Q_Abfrage)

  irdruck = idruck


  ! ------------------------------------------------------------------
  ! Schaetzung Anfangswasserspiegel
  ! ------------------------------------------------------------------

  !**    ifehl=0 --> Einschlussintervall in stroemendem Bereich gefunden
  ifehl = 0

  !MD: Neu Abfragen von ws1 unter GOK!!
  if (ws1.le.hmin) then
   ws1 = hgrenz
   hr = ws1
  end if
  !MD: Neu Abfragen von ws1 unter GOK!!

  !JK      WENN BERECHNUNG UEBER EINSCHLUSSINTERVALL
  if (ITERATIONSART=='EXACT ') then
    CALL anf (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax, &
           & psiein, psiort, ikenn, froud, xi, hi, s, ifehl, nblatt,  &
           & nz, istat, Q_Abfrage)
  ELSE
    hr = hgrenz + 0.2
  end if


  !**    ifehl=1 --> Einschlussintervall in schiessendem bereich gefunden
  IF (ifehl.eq.1) then

    !JK  SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '('' schiessender abfluss --> mit grenztiefe aus normber'')')
    WRITE (UNIT_OUT_LOG, '('' grenztiefe = '',f15.3)') hgrenz

    ikenn = 1
    froud = 1.

    hr = hgrenz

    CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax, &
                 & psiein, psiort, hi, xi, s, istat, froud, ifehlg, 1, Q_Abfrage)
    !JK     ZU DATENUEBERGABE, DA SCHIESSENDER BEREICH ABGESCHLOSSEN
    GOTO 9999


  !** ELSEIF (ifehl.eq.1)
  !** ifehl=2 --> kein Einschlussintervall gefunden
  ELSEIF (ifehl.eq.2) then

    !JK UEBERSPRINGEN STROEMENDER BEREICH
    GOTO 9000


  !**  ELSE (ifehl.eq.1)
  !JK  ifehl=0 --> EINSCHLUSSINTERVALL IM STROEMENDEN BEREICH
  ELSE

    !**  Festlegen des Anfangsiterationswert
    !JK  WENN NORMALPROFIL
    IF (iprof.eq.' ') then
      hranf = hr

      !JK SCHREIBEN IN KONTROLLFILE
      ! halt = ws1+str*q1/rg1      /* anfangs-wsp altes programm
      WRITE (UNIT_OUT_LOG, '('' ermittelter anfangsiterationswert'',f15.3 /)') hr

    !**  ELSE (iprof .eq. ' ')
    ELSE

      IF (abs (hr - hmin) .lt.0.10) then
        hr = hmin + 0.1
        !MD: Error
        !MD: hr = hmax + 0.1
        !MD: Error
        hr = MAX (hr, ws1)
      ENDIF

      !JK  SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '('' iprof = '',a1,'' --> anfwsp festgelegt zu'', f15.3,/,&
            & '' ws1 = '',f15.3,'' hmax = '',f15.3)') iprof, hr, ws1, hmax

    !**     ENDIF (iprof.eq.' ')
    ENDIF



    !JK  BERECHNUNG WSP NACH NEWTON
    !JK  ----------------------------------------------------------
    CALL newton (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
               & psiein, psiort, froud, xi, hi, s, ifehl, istat, Q_Abfrage)

    IF (ifehl.eq.0) then

      IF (froud.ge.1.0.and.idruck.eq.0) then
        !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
        ! ikenn=4
        ikenn = 1
        hr = hgrenz
        itere1 = 1

        !JK Ermitteln der verlustwerte
        !JK --------------------------

        !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
        WRITE (UNIT_OUT_LOG, '(/,'' d.h. steiles gefaelle yn<ygr !!'')')
        WRITE (UNIT_OUT_LOG, '(/,'' ->  weiter mit grenztiefe hr= '', f12.4)') hr

        CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
          & indmax, psiein, psiort, hi, xi, s, istat, froud, &
          & ifehlg, itere1, Q_Abfrage)

      ENDIF

      ifnew = 0

      !JK  ZU DATENUEBERGABE, DA STROEMENDER BEREICH ABGESCHLOSSEN
      GOTO 9999

    !**  ELSE (ifehl.eq.1)
    ELSE

      ! ifehl=1:keine konvergenz in newton-->versuch mit pegasus

      ifnew = 100

      !JK  BERECHNUNG WSP NACH PEGASUS
      !JK  ---------------------------
      CALL pegasus (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,&
      psiein, psiort, ikenn, froud, xi, hi, s, ifehl, istat, Q_Abfrage)

      IF (ifehl.ne.0) then
        ! weiter mit stationaer gleichfoermig
        ifpgs = 1000

        !JK  ANNAHME STATIONAER-GLEICHFOERMIG
        !JK  BERECHNUNG MIT station
        GOTO 9000

      !**             ELSE (ifehl.ne.0)
      ELSE

        ! ifehl=0 --> wsp ermittelt in pegasus

        IF (froud.ge.1.0.and.irdruck.ne.0) then

          ikenn = 4
          hr = hgrenz
          itere1 = 1

          !JK Ermitteln der verlustwerte
          !JK --------------------------

          !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
          !**  write(UNIT_OUT_LOG,'(/,'' grenztiefe aus stationaer '')')
          WRITE (UNIT_OUT_LOG, '(/,'' d.h. steiles gefaelle yn<ygr !!'')')
          WRITE (UNIT_OUT_LOG, '(/,'' ->  weiter mit grenztiefe hr= '', f12.4)') hr

          CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst,      &
            & hrst, indmax, psiein, psiort, hi, xi, s, istat, &
            & froud, ifehlg, itere1, Q_Abfrage)

        !**  ENDIF (froud.ge.1.0.and.irdruck.ne.0)
        ENDIF

        ifpgs = 0

      !**  ENDIF (ifehl.ne.0)
      ENDIF

    !**  ENDIF (ifehl.eq.0)
    ENDIF

  !** ENDIF (ifehl.eq.1)
  ENDIF


  9999  continue

  ikenn = ikenn + ifgrnz + ifnew + ifpgs

  RETURN


  !**  SCHREIBEN IN KONTROLLFILE
  9000 continue

  !write (UNIT_OUT_LOG, *) ' In NORMBER. Zeile 482. i = ', i

  WRITE (UNIT_OUT_LOG, 1000)
  1000 format (/1X, 'Keine Konvergenz in NORMBER!', /, &
              & 1X, '-> weiter mit stationaer gleichfoermig')

  !**  SCHREIBEN IN *.tab
  WRITE (UNIT_OUT_TAB, 1001)
  1001 format (/10X, 'Kein Einfluss vom Unterwasser!')

  !**  Ermitteln einzusetzendes Gefaelle
  nz = nz + 5
  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  WRITE (UNIT_OUT_TAB, 1002)
  1002 format (/10X, 'Annahme stationaer-gleichfoermiger Abfluss')


  !**  ERMITTLUNG DES SOHLGEFAELLES sgef, sohlp=SOHLPUNKT,
  !**  str=ABSTAND ZWISCHEN ZWEI PROFILEN,
  !**  hmin= MIN. GELAENDEHOEHE DES PROFILES
  sgef = (sohlp (i - 1) - hmin) / str


  IF (sgef.ge.0.) then

    !**  WO WIRD sohlg FESTGELEGT?, 14.01.00, UT
    !**  WAS BEZEICHNET ES GENAU? UNTERSCHIED ZU sgef
    !**  UEBERNAHME AUS PROFIL, WELCHEN WERT?

    !JK  WENN TRAPEZ-,KREIS-,MAUL-,EIPROFIL OHNE SOHLGEFAELLE
    IF (iprof.ne.' '.and.sohlg.lt.0.) then

      !JK  SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, 1003) sgef
      1003 format (5X, 'Ermitteltes Sohlgefaelle I = ', F15.4,/,&
                 & 5X, 'Steigung entgegen der Fliessrichtung.',/,&
                 & 5X, '=> Uebernehme Gefaelle aus Profil.')

      sgef = sohlg

      WRITE (UNIT_OUT_TAB, 1004) sgef
      1004 format (/10X, 'Berechnung der Spiegellinie mit Energieliniengefaelle.',/,&
                  & 10X, 'I = ', F10.5, '(= angegebenes Sohlgefaelle)')

    !**  ELSE (iprof.ne.' '.and sohlge.lt.0)
    ELSE

      !JK   SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, 1005) sgef
      1005 format (1X, 'Ermitteltes Sohlgefaelle I = ', F15.4,  &
                 &     ' (-> Steigung!)', /,&
                 & 1X, '=> Ermittlung Energieliniengefaelle')


      ! Weiter mit energieliniengefaelle,wenn die
      ! letzten beiden profile in ordnung waren
      IF (i.ge.3) then

        IF (igrenz (i - 2) .ne.1.and.igrenz (i - 1)               &
        .ne.1.and.igrenz (i - 2) .ne.2.and.igrenz (i - 1) .ne.2)  &
        then

          !**  WAR BEREITS DEAKTIVIERT, 12.01.00, UT
          !**  sgef=(hen(i-2)-hen(i-1))/(stat(i-2)-stat(i-1))
          sgef = (hen (i - 2) - hen (i - 1) ) / str

          IF (sgef.ge.0.) then

            !JK  SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(1x,''- ermitteltes energieliniengefaelle i = '',f15.4,/,&
              &  '' > 0.0 => gefaelle wird angesetzt zu -0.001'')') sgef

            sgef = - 0.001

            WRITE (UNIT_OUT_TAB, '(/,t10,''Berechnung der Spiegellinie mit Energieliniengefaelle '',/, &
            & t10,''i = '', f10.5)') sgef
          ELSE

            WRITE (UNIT_OUT_TAB, '(/,t10,''Berechnung der Spiegellinie mit Energieliniengefaelle '',/, &
            & t10,''i = '', f10.5)') sgef
          ENDIF

        !**  ELSE (igrenz(i-2).ne.1.and.igrenz(i-1).ne.1.and.
        !**    & igrenz(i-2).ne.2.and.igrenz(i-1).ne.2) then
        ELSE

          WRITE (UNIT_OUT_LOG, '(''ueberpruefung der letzten beiden'', &
                    &   '' profile ergab grenztiefe -->'')')
          WRITE (UNIT_OUT_LOG, '(/,''gefaelle wird angesetzt'',        &
                    &   '' zu -0.001'')')

          sgef = - 0.001

          WRITE (UNIT_OUT_TAB, '(/,t10,''Berechnung der Spiegellinie'',              &
                        &     '' mit Energieliniengefaelle  '',/,&
                        & t10,''i = '', f10.5)') sgef

        !**   ENDIF (igrenz(i-2).ne.1.and.igrenz(i-1).ne.1.and.
        !**     & igrenz(i-2).ne.2.and.igrenz(i-1).ne.2) then
        ENDIF

      !**   ELSE (i.ge.3)
      ELSE

        WRITE (UNIT_OUT_LOG, '(''ueberpruefung der letzten beiden'',   &
              &       '' profile nicht moeglich i<3 -->'')')
        WRITE (UNIT_OUT_LOG, '(/,''gefaelle wird angesetzt'',          &
                &       '' zu -0.001'')')

        sgef = - 0.001
        WRITE (UNIT_OUT_TAB, 1020) sgef
        1020 format (/10X, 'Berechnung der Spiegellinie mit Energieliniengefaelle',/,&
                    & 10X, 'I = ', F10.5)
      !**  ENDIF (i.ge.3)
      ENDIF

    !**  ENDIF (iprof.ne.' '.and sohlge.lt.0)
    ENDIF

  !** ELSE (sgef.eg.0.)
  ELSE

    WRITE (UNIT_OUT_TAB, '(/,t10,''Berechnung der Spiegellinie mit Energieliniengefaelle '',/, &
               & t10,''i = '', f10.5)') sgef

    WRITE (UNIT_OUT_LOG, '(''gefaelle wird angesetzt zu '',f15.5, ''(sohlgefaelle)'')') sgef

  !**  ENDIF (sgef.eg.0.)
  ENDIF


  !     ueberpruefen, ob sgef in einem sinnvollen bereich liegt:
  !      if (abs(sgef).ge.0.5) then
  !         sgef=-0.001
  !         write(jw5,'(/,t10,''angsetztes sohlgefaelle unsinnig -->'',
  !     +              '' gewaehlt i = '',f15.5)') sgef
  !         if (lein.eq.3) then
  !         write(UNIT_OUT_LOG,'(''gefaelle unsinnig--> gewaehlt i='',f15.5)') sgef
  !         endif
  !      endif

  CALL station (sgef, i, hgrenz, q, hr, hv, rg, indmax, hvst,     &
    & hrst, psiein, psiort, hi, xi, s, ikenn, froud, str, ifehl, &
    & nblatt, nz, idr1, Q_Abfrage)

  hrst = 0.
  hvst = 0.

  ikenn = ikenn + ifgrnz + ifnew + ifpgs


ELSE

  !write (*,*) 'In NORMBER. stat.-gleichf. Bordvoll.'

  ! Bordvoll-Berechnung fuer stationaer gleichfoermig
  str = 0.
  sgef = isstat (i)

  CALL station (sgef, i, hr, q, hr, hv, rg, indmax, hvst, hrst,   &
    & psiein, psiort, hi, xi, s, ikenn, froud, str, ifehl,   &
    & nblatt, nz, idr1, Q_Abfrage)

  q1 = hvst
  hvst = 0.

ENDIF

END SUBROUTINE normber                                                                             
