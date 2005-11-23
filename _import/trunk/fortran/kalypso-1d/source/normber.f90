!     Last change:  WP   25 Aug 2005    6:14 pm
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


SUBROUTINE normber (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,&
                 & psiein, psiort, jw5, hgrenz, ikenn, froud, nblatt, nz)

!***********************************************************************
!**                                                                    *
!**   SUBROUTINE NORMBER ZUGEHOERIG ZU WSPWIN                          *
!**                                                                    *
!**   geschrieben: P. Koch, Maerz 1990                                 *
!**                                                                    *
!**   Programmbeschreibung:                                            *
!**   Dieses Programm ermittelt den Wasserspiegel an einem Profil.     *
!**                                                                    *
!**   BERECHNUNG                                                       *
!**   ----------                                                       *
!**   ALS ERSTES WERDEN DIE BASISGEOMETRIEGROESSEN BERECHNET (SUB      *
!**   ABSKT), DANN BERECHNUNG DER GRENZTIEFE (SUB GRNZH). ES FOLGT DIE *
!**   SCHAETZUNG DES ANFANGSWASSERSPIEGELS (SUB ANF), ANSCHLIESSEND    *
!**   WERDEN VERLUSTE (SUB VERLUSTE) BERECHNET, DER ANFANGS-           *
!**   ITERATIONSWERT hranf WIRD FESTGELEGT, ES WIRD NACH NEWTON (SUB   *
!**   NEWTON) ODER PEGASUS (SUB PEGASUS) ITERIERT, BERECHNUNG DER      *
!**   VERLUSTE, ERMITTELN EINZUSETZENDES GEFAELLE, AUFRUF VON SUB      *
!**   STATION, JE NACH GEFAELLEFALL UND OB STATIONAER-GLEICHFOERMIG.   *
!**                                                                    *
!**                                                                    *
!**   DIREKT UEBERGEBENE VARIABLEN                                     *
!**   ----------------------------                                     *
!**   str    - Abstand zwischen 2 Profilen                             *
!**   q      - Durchfluss in bestimmten Faellen [m**3/s]               *
!**   q1     - alter Q-Wert in WSPBER [m**3/s]                         *
!**   i      - ?                                                       *
!**   hr     -                                                          
!**   hv     -                                                          
!**   rg     -                                                          
!**   hvst   -                                                          
!**   hrst   -                                                          
!**   indmax -                                                          
!**   psiein - VERLUSTE?                                                
!**   psiort - OERTLICHE VERLUSTE?                                      
!**   jw5    - NAME DER ERGEBNISDATEI .TAB?                             
!**   hgrenz -                                                          
!**   ikenn  -                                                          
!**   froud  -                                                          
!**   nblatt -                                                          
!**   nz     -                                                         *
!**                                                                     
!**   PARAMETER                                                        *
!**   ---------                                                        *
!**   a_m    - ?                                                        
!**   itmax  - max. Anzahl der Iterationsschritte                      *
!**   err    - Genauigkeitsschranke fuer wsp-ermittlung bei Iteration  *
!**                                                                    *
!**   AUFGERUFENE SUBROUTINEN                                          *
!**   -----------------------                                          *
!**   abskst(nknot,x1,xi,h1,hi,s)                                      *
!**   anf(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,                        *
!**       psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl,nblatt,nz)       *
!**   grnzh(q,indmax,hgrenz,xi,hi,s,ifgrnz)                            *
!**   kopf(nblatt,nz,jw5,ifg,jw7,idr1)                                 *
!**   newton(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,                     *
!**          psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl)              *
!**   pegasus(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,                    *
!**           psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl)             *
!**   station(sgef,i,hr,q,hr,hv,rg,indmax,hvst,                        *
!**           hrst,psiein,psiort,jw5,                                  *
!**           hi,xi,s,ikenn,froud,str,ifehl,nblatt,nz)                 *
!**   verluste(str,q,q1,i,hr,hv,rg,hvst,                               *
!**            hrst,indmax,psiein,psiort,                              *
!**            jw5,hi,xi,s,istat,froud,ifehlg,1)                       *
!**                                                                    *
!***********************************************************************
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN


! COMMON-Block /ALT/ ---------------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
! ----------------------------------------------------------------------------------


! COMMON-Block /AUSGABEART/ --------------------------------------------------------
! lein=1    --> einfacher ergebnisausdruck
! lein=2    --> erweiterter ergebnisausdruck
! lein=3    --> erstellung kontrollfile
! jw8       --> NAME KONTROLLFILE
INTEGER 	:: lein
INTEGER 	:: jw8
COMMON / ausgabeart / lein, jw8
! ----------------------------------------------------------------------------------


! COMMON-Block /BLOED/ -------------------------------------------------------------
INTEGER 	:: nr
COMMON / bloed / nr
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


! COMMON-Block /P4/ -----------------------------------------------------------
INTEGER         :: ifg
REAL            :: betta
COMMON / p4 / ifg, betta
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


! COMMON-Block /W_A/ ----------------------------------------------------------
INTEGER 	:: a_m
COMMON / w_a / a_m
! -----------------------------------------------------------------------------


! Local variables
REAL :: xi (maxkla), hi (maxkla), s (maxkla)


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

IF (bordvoll .ne. 'g') then

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

  !**   ------------------------------------------------------------------
  !**   Berechnung von Basisgeometriegroessen : Sehnen, Abstand zwischen
  !**   den Profilpunkten (horizontal und veritkal)
  !**   ------------------------------------------------------------------
  !**   Input: nknot,x,h, Output: xi,hi,s
  CALL abskst (nknot, x1, xi, h1, hi, s, maxkla)


  !**   AM 12.01.00 DIE BEIDEN FOLGENDEN ZEILEN SO DEAKTIVIERT?, UT
  !     berechnung der tiefsten sohle und der boeschungskanten (s. profil)
  !     gebraucht werden - boli(i),bore(i),sohl(i)


  !**   ------------------------------------------------------------------
  !**   Berechnung der Grenztiefe hgrenz
  !**   ------------------------------------------------------------------

  CALL grnzh (q, indmax, hgrenz, xi, hi, s)

  irdruck = idruck

  !**        WAR BEREITS DEAKTIVIERT, 12.014.00, UT
  !          if (idruck.eq.1) then
  !              print *,'keine grenztiefe ermittelt '
  !              hgrenz=0.
  !          endif


  !**   ------------------------------------------------------------------
  !**   Schaetzung Anfangswasserspiegel
  !**   ------------------------------------------------------------------

  !**    ifehl=0 --> Einschlussintervall in stroemendem Bereich gefunden
  ifehl = 0


  !JK      WENN BERECHNUNG UEBER EINSCHLUSSINTERVALL
  IF (a_m .eq. 2) then

    !ep  call anf(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax, &
    !ep         & psiein,psiort,jw5,ikenn,froud,xi,hi,s,ifehl,nblatt,nz, istat)
    !ep  Korrektur, da istat in anf erwartet wird, dieser Parameter aber

    CALL anf (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax, &
           & psiein, psiort, jw5, ikenn, froud, xi, hi, s, ifehl, nblatt,  &
           & nz, istat)

  !JK  WENN BERECHNUNG UEBER EINFACHE ITERATION
  ELSE

    hr = hgrenz + 0.2

  ENDIF



  !**    ifehl=1 --> Einschlussintervall in schiessendem bereich gefunden
  IF (ifehl.eq.1) then

    !JK  SCHREIBEN IN KONTROLLFILE
    IF (lein.eq.3) then
      WRITE (jw8, '('' schiessender abfluss --> mit grenztiefe aus normber'')')
      WRITE (jw8, '('' grenztiefe = '',f15.3)') hgrenz
    ENDIF

    ikenn = 1
    froud = 1.

    hr = hgrenz

    CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax, &
                 & psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg, 1)
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
      IF (lein.eq.3) then
        ! halt = ws1+str*q1/rg1      /* anfangs-wsp altes programm
        WRITE (jw8, '('' ermittelter anfangsiterationswert'',f15.3 /)') hr
      ENDIF

    !**  ELSE (iprof .eq. ' ')
    ELSE

      IF (abs (hr - hmin) .lt.0.10) then
        hr = hmax + 0.1
        hr = MAX (hr, ws1)
      ENDIF

      !JK  SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then
        WRITE (jw8, '('' iprof = '',a1,'' --> anfwsp festgelegt zu'', f15.3,/,&
            & '' ws1 = '',f15.3,'' hmax = '',f15.3)') iprof, hr, ws1, hmax
      ENDIF

    !**     ENDIF (iprof.eq.' ')
    ENDIF



    !JK  BERECHNUNG WSP NACH NEWTON
    !JK  ----------------------------------------------------------
    CALL newton (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
               & psiein, psiort, jw5, froud, xi, hi, s, ifehl, istat)

    IF (ifehl.eq.0) then

      IF (froud.ge.1.0.and.idruck.eq.0) then
        !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
        ! ikenn=4
        ikenn = 1
        hr = hgrenz
        itere1 = 1

        !JK Ermitteln der verlustwerte
        !JK --------------------------

        IF (lein.eq.3) then
          !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
          !**  write(jw8,'(/,'' grenztiefe aus stationaer '')')
          WRITE (jw8, '(/,'' d.h. steiles gefaelle yn<ygr !!'')')
          WRITE (jw8, '(/,'' ->  weiter mit grenztiefe hr= '', f12.4)') hr
        ENDIF

        CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
          & indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, &
          & ifehlg, itere1)

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
      psiein, psiort, jw5, ikenn, froud, xi, hi, s, ifehl, istat)

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
          IF (lein.eq.3) then
            !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
            !**  write(jw8,'(/,'' grenztiefe aus stationaer '')')
            WRITE (jw8, '(/,'' d.h. steiles gefaelle yn<ygr !!'')')
            WRITE (jw8, '(/,'' ->  weiter mit grenztiefe hr= '', f12.4)') hr
          ENDIF

          CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst,      &
            & hrst, indmax, psiein, psiort, jw5, hi, xi, s, istat, &
            & froud, ifehlg, itere1)

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
  IF (lein.eq.3) then
    WRITE (jw8, 1000)
  ENDIF
  1000 format (/1X, 'Keine Konvergenz in NORMBER!', /, &
              & 1X, '-> weiter mit stationaer gleichfoermig')

  !**  SCHREIBEN IN *.tab
  WRITE (jw5, 1001)
  1001 format (/10X, 'Kein Einfluss vom Unterwasser!')

  !**  Ermitteln einzusetzendes Gefaelle
  nz = nz + 5
  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, jw5, ifg, jw7, idr1)
  ENDIF

  WRITE (jw5, 1002)
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
      IF (lein.eq.3) then
        WRITE (jw8, 1003) sgef
        1003 format (5X, 'Ermitteltes Sohlgefaelle I = ', F15.4,/,&
                   & 5X, 'Steigung entgegen der Fliessrichtung.',/,&
                   & 5X, '=> Uebernehme Gefaelle aus Profil.')
      ENDIF

      sgef = sohlg

      WRITE (jw5, 1004) sgef
      1004 format (/10X, 'Berechnung der Spiegellinie mit Energieliniengefaelle.',/,&
                  & 10X, 'I = ', F10.5, '(= angegebenes Sohlgefaelle)')

    !**  ELSE (iprof.ne.' '.and sohlge.lt.0)
    ELSE

      !JK   SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then
        WRITE (jw8, 1005) sgef
        1005 format (1X, 'Ermitteltes Sohlgefaelle I = ', F15.4,  &
                   &     ' (-> Steigung!)', /,&
                   & 1X, '=> Ermittlung Energieliniengefaelle')
      ENDIF

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
            IF (lein.eq.3) then
              WRITE (jw8, '(1x,''- ermitteltes energieliniengefaelle i = '',f15.4,/,&
              &  '' > 0.0 => gefaelle wird angesetzt zu -0.001'')') sgef
              sgef = - 0.001
            ENDIF

            WRITE (jw5, '(/,t10,''Berechnung der Spiegellinie mit Energieliniengefaelle '',/, &
            & t10,''i = '', f10.5)') sgef
          ELSE

            WRITE (jw5, '(/,t10,''Berechnung der Spiegellinie mit Energieliniengefaelle '',/, &
            & t10,''i = '', f10.5)') sgef
          ENDIF

        !**  ELSE (igrenz(i-2).ne.1.and.igrenz(i-1).ne.1.and.
        !**    & igrenz(i-2).ne.2.and.igrenz(i-1).ne.2) then
        ELSE

          IF (lein.eq.3) then
            WRITE (jw8, '(''ueberpruefung der letzten beiden'', &
                      &   '' profile ergab grenztiefe -->'')')
            WRITE (jw8, '(/,''gefaelle wird angesetzt'',        &
                      &   '' zu -0.001'')')
          ENDIF

          sgef = - 0.001

          WRITE (jw5, '(/,t10,''Berechnung der Spiegellinie'',              &
                        &     '' mit Energieliniengefaelle  '',/,&
                        & t10,''i = '', f10.5)') sgef

        !**   ENDIF (igrenz(i-2).ne.1.and.igrenz(i-1).ne.1.and.
        !**     & igrenz(i-2).ne.2.and.igrenz(i-1).ne.2) then
        ENDIF

      !**   ELSE (i.ge.3)
      ELSE

        IF (lein.eq.3) then
          WRITE (jw8, '(''ueberpruefung der letzten beiden'',   &
                &       '' profile nicht moeglich i<3 -->'')')
          WRITE (jw8, '(/,''gefaelle wird angesetzt'',          &
                  &       '' zu -0.001'')')
        ENDIF

        sgef = - 0.001
        WRITE (jw5, 1020) sgef
        1020 format (/10X, 'Berechnung der Spiegellinie mit Energieliniengefaelle',/,&
                    & 10X, 'I = ', F10.5)
      !**  ENDIF (i.ge.3)
      ENDIF

    !**  ENDIF (iprof.ne.' '.and sohlge.lt.0)
    ENDIF

  !** ELSE (sgef.eg.0.)
  ELSE

   WRITE (jw5, '(/,t10,''Berechnung der Spiegellinie mit Energieliniengefaelle '',/, &
              & t10,''i = '', f10.5)') sgef

    IF (lein.eq.3) then
      WRITE (jw8, '(''gefaelle wird angesetzt zu '',f15.5, ''(sohlgefaelle)'')') sgef
    ENDIF

  !**  ENDIF (sgef.eg.0.)
  ENDIF


  !     ueberpruefen, ob sgef in einem sinnvollen bereich liegt:
  !      if (abs(sgef).ge.0.5) then
  !         sgef=-0.001
  !         write(jw5,'(/,t10,''angsetztes sohlgefaelle unsinnig -->'',
  !     +              '' gewaehlt i = '',f15.5)') sgef
  !         if (lein.eq.3) then
  !         write(jw8,'(''gefaelle unsinnig--> gewaehlt i='',f15.5)') sgef
  !         endif
  !      endif

  CALL station (sgef, i, hgrenz, q, hr, hv, rg, indmax, hvst,     &
    & hrst, psiein, psiort, jw5, hi, xi, s, ikenn, froud, str, ifehl, &
    & nblatt, nz, idr1)

  hrst = 0.
  hvst = 0.

  ikenn = ikenn + ifgrnz + ifnew + ifpgs


!**  ELSE (bordvoll.ne.'g') ALSO AB HIER STAT. GLEICHF BORDVOLL
ELSE

  !**  Bordvoll-Berechnung fuer stationaer gleichfoermig
  str = 0.
  sgef = isstat (i)

  CALL station (sgef, i, hr, q, hr, hv, rg, indmax, hvst, hrst,   &
    & psiein, psiort, jw5, hi, xi, s, ikenn, froud, str, ifehl,   &
    & nblatt, nz, idr1)

  q1 = hvst
  hvst = 0.

!**  ENDIF (bordvoll.ne.'g')
ENDIF


RETURN

END SUBROUTINE normber                                                                             
