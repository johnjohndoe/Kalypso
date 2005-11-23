!     Last change:  WP   11 Nov 2005    2:24 pm
!--------------------------------------------------------------------------
! This code, wspber.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - wspber
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



SUBROUTINE wspber (unit1, ibruecke, wehr)
!
!   geschrieben: P. Koch, Maerz 1990
!   geaendert:   W. Ploeger, Mai 2005
!
!   Programmbeschreibung:
!   ---------------------
!   Dieses Programm ist das Hauptprogramm fuer die stationaer
!   ungleichfoermige Wasserspiegellagenberechnung.
!   Fuer jede Station wird in der Subroutine PROEIN der zugehoerige
!   Profildatensatz eingelesen.
!   Fuer die erste Station wird der Anfangswasserspiegel in Abhaengig-
!   keit von der Angabe von wsanf im Steuerprogramm berechnet.
!   Die normale Wasserspiegellagenberechnung findet in NORMBER statt.
!   Die Ergebnisse werden in Speicher abgespeichert.
!
!   DIREKT UEBERGEBENE VARIABLEN
!   ----------------------------
!   unit1    - DATEINAME, Filename .tab-file
!   ibruecke - SOLL BRUECKE GERECHNET WERDEN
!   wehr     - SOLL WEHR GERECHNET WERDEN
!
!   ERLAEUTERUNG VON PARAMETERN
!   ---------------------------
!
!   Commonblock fuer die Gewaessergeometrie
!
!   Variablenbeschreibung
!
!   x1     r4(1...maxkla)   x-wert der Profilpunkte
!   h1     r4(1...maxkla)   Hoehenwert der Profilpunkte
!   rau    r4(1...maxkla)   Rauhigkeitsbeiwert (k-s o. k-st) der profi
!   nknot  i4               Anzahl Profilpunkte
!   iprof  a1               Kennung zur bezeichnung des Profiltyps
!                           iprof = ' ' : Normalprofil
!                                   't' : Trapezprofil
!                                   'k' : Kreisprofil
!                                   'e' : Eiprofil
!                                   'm' : Maulprofil
!   durchm r4               fuer iprof = ' ' : nicht belegt
!                                        't' : breite der Sohle
!                                        'k' : durchmesser
!                                        'e' : max. breite
!                                        'm' : max. breite
!   hd     r4               fuer iprof = ' ' : nicht belegt
!                                        't' : vertikales Lichtmass
!                                        'k' : nicht belegt
!                                        'e' : vertikales Lichtmass
!                                        'm' : vertikales Lichtmass
!   steig  r4               Steigung beider Profilseiten
!                           nur fuer iprof = 't' belegt
!   sohlg  r4               Sohlgefaelle
!                           bei Normalprofil z.Zt. nicht belegt.
!   boli   r4               Gelaendehoehe an linker Vorland/Flusschlau
!                           trennflaeche
!   bore   r4               Gelaendehoehe an rechter Vorland/Flusschla
!                           trennflaeche
!   hmin   r4               minimale Gelaendehoehe des Profils
!   hmax   r4               maximale Gelaendehoehe des Profils
!   ianf   i4               punkt-nr. der linken Vorland/Flusschlauch-
!                           trennflaeche
!   iend   i4               punkt-nr. der rechten Vorland/Flusschlauch
!                           trennflaeche
!
!HB   ***************************************************************** 
!HB   26.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB   alph_aus -  Projektpfad der Ausgabedatei Beiwerte.AUS             
!HB   alpha_EW -  Energiestrombeiwert auf Profilnummer bezogen          
!HB   alpha_IW -  Boussinesq-Beiwert auf Profilnummer bezogen           
!HB   alpha_ja -  Steuerparameter, ob Beiwerte.AUS erstellt werden soll 
!HB   gesamt_a -  gesamte durchstroemte Flaeche                         
!HB   nr_alph  -  Interne Dateinummer fuer Beiwerte.AUS                 
!HB   pn_alpha -  Profilnummer (=nprof)                                 
!HB   st_alpha -  Stationskilometer (=staso)                            
!HB   ***************************************************************** 
!
!   Beachte Parameteranweisung maxkla - Anzahl der Punkte im Profil!
!
!   Benutzte unit-nr.:
!   ------------------
!   - jw1   -  bis jetzt noch nicht benutzt! - ''fluss.dat''
!   - jw2   -  datei 'flussname.dat',  z.b. ''westerbach.dat''
!   - jw4   -  Profildatei prof....dat, z.b. ''prof0001.dat''
!   - jw5   -  Ergebnisdatei ....tab, z.b. ''westerhq2.tab''
!                                  oder  '' wester250.tab''(bei
!                                  bordvoll->d.h. q=250 m**3/s)
!   - jw6   -  Ergebnisdatei Wasserspiegellaengschnitt
!             'flussname'.wsl ,z.b. wester.wsl
!   - jw7   -  Ergebnisdatei Wasserstands-Abflussbeziehung
!   - jw8   -  Kontrollfile
!
!   - unit1 -  Filename .tab-file
!   - unit4
!
!   AUFGERUFENE ROUTINEN
!   --------------------
!   drucktab (nprof,indmax,nz,jw5,nblatt,stat,jw7,idr1)
!   intdat (staso,ifehl)
!   kopf (nblatt,nz,jw5,ifg,jw7,idr1)
!   lapro1 (jw6,jw6,nprof,mark)
!   lcase (unit4)
!   lu0trf (jw5)
!   proe_pro (jw4,text33,filename)
!   proein (jw4)
!   speicher (nprof,hbv_gl,hv,hvst,hrst,q,stat(nprof),indmax,ikenn)
!
!***********************************************************************


! ------------------------------------------------------------------
! PARAMETERVEREINBARUNG UND COMMONBLOECKE
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

! Calling variables
CHARACTER(LEN=nch80), INTENT(IN) :: unit1
CHARACTER(LEN=1), INTENT(IN)     :: ibruecke
CHARACTER(LEN=1), INTENT(IN)     :: wehr


! COMMON-Block /ALPH_PF/ -----------------------------------------------------------
INTEGER 		:: nr_alph
CHARACTER(LEN=nch80)    :: alph_aus     ! Projektpfad fuer Beiwerte.AUS
COMMON / alph_pf / alph_aus, nr_alph
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


! COMMON-Block /BRUECK/ ------------------------------------------------------------
! fuer BRUECKENBERECHNUNG, AUFRUF IN INTDAT, IMPULS, WSPBER
INTEGER 	:: iwl, iwr, nuk, nok
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, ibridge
! ----------------------------------------------------------------------------------


! COMMON-Block /BRUECK3/ -----------------------------------------------------------
! Gelaende Oberkante Grenze
INTEGER 	:: iokl, iokr
COMMON / brueck3 / iokl, iokr
! ----------------------------------------------------------------------------------


! COMMON-Block /BV/ ----------------------------------------------------------------
CHARACTER(LEN=1):: idr1, idr2
INTEGER         :: nprof, isch
REAL            :: qvar
COMMON / bv / idr1, idr2, nprof, qvar, isch
! ----------------------------------------------------------------------------------


! COMMON-Block /BV_IT/ -------------------------------------------------------------
! ENTHAELT KENNUNG DER ITERIERTEN PROFILE FUER DIE
! UNGLEICHF. BORDVOLLRECHNUNG, AUFRUF IN WSPBER, BORDVOLL
INTEGER 	:: prof_it (maxger)
COMMON / bv_it / prof_it
! ----------------------------------------------------------------------------------


! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
! ----------------------------------------------------------------------------------


! COMMON-Block /DATEINAME_PROF/ ----------------------------------------------------
CHARACTER(LEN=nch80) :: dateiname(maxger)
COMMON / dateiname_prof / dateiname
! ----------------------------------------------------------------------------------

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


! COMMON-Block /FUNIT/ --------------------------------------------------------
INTEGER 	:: jw1, jw2
COMMON / funit / jw1, jw2
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
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
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /NR_ALPHA/ -----------------------------------------------------
! Variablen zur Uebergabe der Profilnummern und Stationskilometer
! fuer die Energiestrom- und Boussinesq-beiwertberechnung
INTEGER 	:: pn_alpha
REAL 		:: st_alpha, gesamt_a (maxkla), alpha_EW (maxkla), alpha_IW (maxkla)
COMMON / nr_alpha / pn_alpha, st_alpha, gesamt_a, alpha_EW, alpha_IW
! -----------------------------------------------------------------------------


! COMMON-Block /OB_ALPHA/ -----------------------------------------------------
! Uebergabe eines Steuerparameters, ob die Datei Beiwerte.AUS
! erzeugt und in sie geschrieben werden soll.
INTEGER 	:: alpha_ja
COMMON / ob_alpha / alpha_ja
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


! COMMON-Block /P3/ -----------------------------------------------------------
INTEGER 	:: isohl, iming
REAL            :: hming
COMMON / p3 / isohl, hming, iming
! -----------------------------------------------------------------------------


! COMMON-Block /P4/ -----------------------------------------------------------
INTEGER         :: ifg
REAL            :: betta
COMMON / p4 / ifg, betta
! -----------------------------------------------------------------------------


! COMMON-Block /P11/ ----------------------------------------------------------
REAL 		:: wsanf, qstat (merg), qwert (merg), staanf, staend
INTEGER 	:: nq
COMMON / p11 / wsanf, nq, qstat, qwert, staanf, staend
! -----------------------------------------------------------------------------


! COMMON-Block /PRO1/ ---------------------------------------------------------
REAL 	:: qbord(maxger), hbord (maxger), vbord (maxger)
INTEGER :: jkenn (maxger)
COMMON / pro1 / qbord, hbord, jkenn, vbord
!     Vorbelegung:
!     ------------
!     jkenn - kennung dafuer, ob der bordvoll wert ermittelt worden ist oder nicht
!     jkenn=0 - bordvoller abfluss noch nicht ermittelt
!     jkenn=1 - bordvoller abfluss ermittelt
! -----------------------------------------------------------------------------


! COMMON-Block /PROF/ ---------------------------------------------------------
REAL 		:: hwsp (100, maxger)
COMMON / prof / hwsp
! -----------------------------------------------------------------------------


! COMMON-Block /PROFILNUMMER/ -------------------------------------------------
CHARACTER(LEN=10) :: num (maxger)
COMMON / profilnummer / num
! -----------------------------------------------------------------------------


! COMMON-Block /PSIWERT/ ------------------------------------------------------
! ENTHAELT VARIABLEN FUER OERTLICHE VERLUSTE
REAL 		:: psistat (merg), psiein (merg), psiort (merg)
INTEGER 	:: jpsi
COMMON / psiwert / psistat, psiein, psiort, jpsi
! -----------------------------------------------------------------------------


! COMMON-Block /TXT32/ --------------------------------------------------------
CHARACTER(LEN=36):: text321
COMMON / txt32 / text321
! -----------------------------------------------------------------------------


! COMMON-Block /WEHR/ ---------------------------------------------------------
REAL 		:: xokw (maxkla), hokw (maxkla)
INTEGER 	:: nokw, iwmin
REAL            :: hokwmin
CHARACTER(LEN=1):: iwehr
REAL 		:: xtrw (maxw), htrw (maxw)
INTEGER         :: nwfd
INTEGER 	:: iendw (maxw), ianfw (maxw)
COMMON / wehr / xokw, hokw, nokw, iwmin, hokwmin, iwehr, xtrw, htrw, nwfd, iendw, ianfw
! -----------------------------------------------------------------------------




! Local variables ---------------------------------------------------------------------------------
LOGICAL :: is_open

INTEGER :: jw6, jw7
INTEGER :: fseek
INTEGER :: np (ipro)
INTEGER :: int (merg)

INTEGER :: nblatt       ! Blattnr. bei der Ausgabe der Textdatei in Tabellenform
INTEGER :: nz

INTEGER :: ikenn        ! Zustandskennung (= 0 fuer Normalberechnung, = 1 fuer Schiessen mit Grenztiefe)
INTEGER :: indmax       ! Anzahl der Rauhigkeitzonen im aktiven Profil

INTEGER :: ih, i        ! Schleifenzaehler

CHARACTER(LEN=26) :: prof_nr

CHARACTER(LEN=nch80) :: char (merg)
CHARACTER(LEN=nch80) :: unit4, dummy, text
CHARACTER(LEN=nch80) :: unit5, unit7
CHARACTER(LEN=nch80) :: pfad2   	! Dateipfad wird Zeichenkette zugewiesen
CHARACTER(LEN=nch80) :: nr          	! Beinhaltet den Namen der Profildatei (z.b. St000150.prf)
!ST 29.03.2005
!Variable f�r Laengschnitt.txt
CHARACTER(LEN=nch80) :: file_laengs

REAL :: psieins, psiorts        	! Zur Beruecksichtigung oertlicher Verluste
REAL :: hgrenz, strbr
REAL :: hrst, rg                        ! Reibungsverlusthoehe
REAL :: hvst, hv                        ! Geschwindigkeitsverlusthoehe
REAL :: hr                              ! Wasserspiegelhoehe
REAL :: statgem                         ! Station gemerkt ( = stat(nprof) )
REAL :: stat1                           ! Station des letzten Profils ( = stat(nprof-1) )
REAL :: strecke                         ! Distanz zwischen zwei Profilen
REAL :: br1                             ! Breite Nettoprofil '1' (bei Brueckenberechnung)
REAL :: delta
REAL :: delta1                          ! Abstand des moeglichen Profils 'a' vom Brueckenprofil
REAL :: str1


REAL :: feldr (merg)
REAL :: xl (maxkla), hl (maxkla), raul (maxkla)

!**   Hilfsparameter fuer das Vorland, einschlisslich rau1(maxkla)
REAL :: x11 (maxkla), h11 (maxkla), ax1 (maxkla), ay1 (maxkla), dp1 (maxkla), rau1 (maxkla)


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------

!WP 10.05.2005
!WP Bei der normalen Spiegellinienberechnung wird der Dateiname
!WP PFAD2 nicht verwendet. MARK wird zu 1 gesetzt und dann ganz
!WP am Ende LAPRO1 aufgerufen. Trotzdem muss die CHARACTER-Variable
!WP PFAD2 einen gueltgen Wert bekommen
pfad2 = ' '

!**   Vorbelegungen:

akges = 0.0


!**   izz = Zaehler fuer die mit q-wert belegten Stationen (q-werte aus
!**   qwert.dat, eingelesen im Steuerprogramm)
izz = 1

!**   nprof = Zaehler fuer die Anzahl der Profile im Berechnungsablauf
nprof = 0

!**   Eroeffnen der Ausgabedatei 'Flussname.tab' (dateiname-unit1)
jw5 = ju0gfu ()

OPEN (unit = jw5, file = unit1, status = 'unknown')

!HB   *****************************************************************
!HB   26.11.2001 - H.Broeker
!HB   ---------------------------------------------------------------
!HB   STEUERPARAMETER
!HB   ---------------
!HB   Setzen des Steuerparameters, ob die Datei Beiwerte.AUS erzeugt
!HB   und spaeter in sie geschrieben werden soll.
!HB   Wenn alpha_ja=1, wird die Datei erzeugt und in sie geschrieben
alpha_ja = 1
!HB   Wenn alpha_ja=0, wird die Datei NICHT erzeugt und NICHT in sie
!HB   geschrieben
!      alpha_ja=0
!HB   ---------------------------------------------------------------
!HB   Die IF_Schleife wird durchlaufen, wenn die Datei Beiwerte.AUS
!HB   erzeugt werden soll und in sie geschrieben werden soll.
IF (alpha_ja.eq.1) then
  !HB     Oeffnen der Ergebnisausgabedatei fuer die Impuls- und Energie-
  !HB     strombeiwert-Auswertung (zusaetzliche Ausgabedatei: Beiwerte.AUS

  !HB     Einholen einer Dateinummer fuer Datei
  nr_alph = ju0gfu ()
  !HB     Oeffnen der Datei Beiwerte.AUS
  OPEN (UNIT = nr_alph, IOSTAT = ierr, FILE = alph_aus, STATUS = 'replace')

  !HB     Schreiben der Kopfzeile der Ausgabedatei Beiwert.AUS
  WRITE (nr_alph, 9911)
  9911 FORMAT  (/,'Ergebnisdatei BEIWERTE.AUS',//,                       &
    &'1.Zeile je Profil: auf den Gesamtquerschnitt bezogene Groessen',/&
    &,'Lambda   = (Gefaelle*Flaeche**3*8*g)/(Umfang*Durchfluss**2)',/  &
    &,'Flaeche  = gesamte durchstroemte Flaeche',/                     &
    &,'Umfang   = gesamter benetzter Umfang inklusiv Trennflaechen !',/&
    &,'Geschw.  = mittlere Geschwindigkeit',/                          &
    &,'Durchfl. = Gesamtdurchfluss',/                                  &
    &,'alpha_EW = Energiestrombeiwert',/                               &
    &,'alpha_IW = Impulsstrombeiwert bzw. Boussinesq-Beiwert',/        &
    &,'Gefaelle = Anfangssohlgefaelle = Sohlgefaelle zwischen den Profi&
    &len = Energieliniengefaelle,da stationaer gleichfoermige Berechnun&
    &g vorausgesetzt',//                                               &
    &'2.Zeile je Profil: auf die Teilquerschnitte bezogene Groessen',/ &
    &,'Umfang   = benetzter Umfang der Teilquerschnitte (ohne Trennflae&
    &che)',/                                                           &
    &,'Lambda   = uebliche Bestimmung je Teilquerschnitt',/            &
    &'-----------------------------------------------------------------&
    &------------------------------------------------------------------&
    &--------------'//,                                                &
    &3x,'Stat-km',7x,'Wsp-m+NN',7x,                                    &
    &'hen-m+NN',1x,'IND',7x,'Lambd',9x,'Fl-qm',9x,'Umfg-m',5x,         &
    &'v-m/s',5x,'Q-cbm/s',5x,'alpha_EW',5x,'alpha_IW',4x,'Gefaelle')
ENDIF

!**   /* stutze file ab pointer
text = ' '
!HB   Aenderung von 15 Leerzeichen in 6 (frueher: text(15:nch80)= )
text (6:nch80)  = 'Ergebnis der Wasserspiegellagenberechnung fuer '

!ilen = ju0nch (text)
ilen = LEN_TRIM (text)

IF (bordvoll.eq.'n') then

  text (ilen + 2:nch80) = ereignis

ELSEIF (bordvoll.eq.'u') then

  WRITE (ereignis, '(f8.3)') qvar

  ereignis = ADJUSTL (ereignis)

  text (ilen + 2:nch80) = ereignis

  ilen = LEN_TRIM (text)
  text (ilen + 1:nch80) = ' m**3/s'

ELSE

  text = 'Bordvollberechnung stationaer-gleichfoermig'

ENDIF

!**   jw5 = Flussname.tab'?
WRITE (jw5, '(a)') text


!**   Schreiben des Kopfes der Datei 'flussname.tab'
nblatt = 1
IF (idr1.ne.'j') then
  CALL kopf (nblatt, nz, jw5, ifg, jw5, idr1)
ENDIF

!**   nz wird in kopf zu 1 gesetzt, maxger ist Anzahl der
!**   Gerinneabschnitte
!**   REWIND setzt jw2 auf Anfangspunkt zurueck
REWIND (jw2)                                                                                   


!WP -----------------------------------------------------------------------------------
!WP Beginn der Hauptschleife �ber alle Profile
!WP -----------------------------------------------------------------------------------

Hauptschleife: DO i = 1, maxger
                                                                        
                                                                        
  !**   Einlesen aus flussname.dat:
  READ (jw2, '(a)', end = 5000) dummy
                                                                        
  CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)

  IF (ifehl.ne.0.or.ianz.eq.0) then
    PRINT * , 'Fehlerhaftes Format in der Profil.dat-datei.'
    PRINT * , 'Es wurde keine Station gelesen.'
    PRINT * , 'Eingelesen wurde : ', dummy
    STOP 0
  ENDIF

  statles = feldr (1)

  !ilen = ju0nch (char (1) )
  ilen = LEN_TRIM (char (1) )

  nr = char (1) (1:ilen)        ! NR beinhaltet den Namen der Profildatei (z.b. St000150.prf)

  !** ENDE DER EINLESESCHLEIFE wenn das eingelesene FELD KLEINER ALS ANFANGSFELD?
  !** 5000 FAST PROGRAMMENDE, statles=feldr(1)
  IF (statles .lt. staanf) CYCLE Hauptschleife
  IF (statles .gt. staend) EXIT Hauptschleife
                                                                        
                                                                        
  !**   Anzahl der Profile fuer die WSP-Berechnung:
  nprof = nprof + 1
  stat (nprof) = statles
  prof_it (nprof) = 0
                                                                        
                                                                        
  !**   ------------------------------------------------------------------
  !**   Einlesen der Profildatei prof(nr).dat
  !**   ------------------------------------------------------------------
                                                                        
  !ilen = ju0nch (fnam1)
  ilen = LEN_TRIM (fnam1)

  unit4 = fnam1

  unit4 (ilen + 1:nch80) = nr           ! Kompletter Pfad zur Profildatei
  CALL lcase (unit4)

  jw4 = ju0gfu ()
  ierr = 0


  !**   lein=3 im Falle der Erstellung des Kontrollfiles
  IF (lein.eq.3) then
    write (jw8, '(/,''----------------------------------------------------------------------'')')
    write (jw8, '(''Oeffnen Profildatei '',a)') unit4
  END if

  ! Zuweisung des kompletten Dateinamens der Profildatei zu einem Array
  dateiname(i) = unit4

  OPEN (unit = jw4, iostat = ierr, file = unit4, status = 'old')
  IF (ierr.ne.0) then
    PRINT * , ' ', unit4, ' existiert nicht !!'

    !**       SCHREIBEN DER FEHLERMELDUNG IN KONTROLLFILE,
    !**       WENN lein=3 UND ES ANGELEGT WIRD
    IF (lein.eq.3) then
      WRITE (jw8, '(a,'' existiert nicht !!'')') unit4
      WRITE (jw8, '(''--> Abbruch des Programms'')')
    ENDIF

    !UT  ERLAEUTERUNG WARUM UND WO ABBRUCH
    PRINT * , ' Abbruch des Programms in WSPber, Es fehlt Datei: ', unit4
    GOTO 999
  ENDIF

  !**   ------------------------------------------------------------------
  !**   Eroeffnen der Ausgabedatei *.pro bei Bordvollberechnung
  !**   --> Wasserstands-Abflussbeziehung fuer jede Station
  !**   ------------------------------------------------------------------

  IF (idr1.eq.'j') then

    WRITE (text, '(f11.4)') statles
    !jlen = ju0nch (text)
    jlen = LEN_TRIM (text)

    DO k = 1, jlen
      IF (text (k:k) .eq.'.') then
        text (k:k) = '-'
      ENDIF
    END DO

    !CALL u0ljst (text)
    text = ADJUSTL (text)

    unit7 = fnam1
    !ilen = ju0nch (unit7)
    ilen = LEN_TRIM (unit7)
    unit7 (ilen - 4:ilen - 1) = 'dath'
    !iflen = ju0nch (fluss)
    iflen = LEN_TRIM (fluss)

    IF (iflen.gt.2) then
      iflen = 2
    ENDIF

    unit7 (ilen + 1:nch80) = fluss (1:iflen)
    !ilen = ju0nch (unit7)
    ilen = LEN_TRIM (unit7)
    unit7 (ilen + 1:nch80) = text
    !ilen = ju0nch (unit7)
    ilen = LEN_TRIM (unit7)
    unit7 (ilen + 1:nch80) = '.pro'
    jw7 = ju0gfu ()

    OPEN (unit = jw7, file = unit7, status = 'unknown')


    IF (isch.eq.1) then

      !**  /* stutze file ab pointer
      CALL lu0trf (jw7)
      CALL kopf (nblatt, nz, jw7, ifg, jw7, idr1)

    ELSE

      INQUIRE (UNIT = jw7, OPENED = is_open, IOSTAT = ierr)
      if (ierr /= 0) then
        write (*,*) ' In WSPBER ist ein Fehler beim Untersuchen des Zustandes'
        write (*,*) ' einer UNIT aufgetreten (INQUIRE-Befehl), ca. Zeile 697'
        write (*,*) ' -> Versuche weiterzurechnen...'
        GOTO 999
      end if

      IF (is_open) then
        fehler_id = fseek (jw7, 0, 2)
        if (fehler_id /= 0) then
          write (*,*) ' In WSPBER ist ein Fehler aufgetreten beim Platzieren eines'
          WRITE (*,*) ' Pointers in einer Datei! (Nach FSEEK ca. Zeile 706)'
          write (*,*) ' -> Versuche weiterzurechnen...'
          stop
        end if
      end if

    ENDIF

  ! ENDIF OB BORDVOLL (idr1.eq.'j')
  ENDIF


  !**   ------------------------------------------------------------------
  !**   Einlesen der Daten in Sub proein und Weiterverarbeitung in intdat
  !**   ------------------------------------------------------------------

  CALL proe_pro (jw4, text33, unit4)

  text32 = text321


  !**   ------------------------------------------------------------------
  !**   Definition des Profils
  !**   ------------------------------------------------------------------

  !**   KONVERTIERUNG DES TEXTES text 32 VON KLEIN- ZU GROSSBUCHSTABEN
  CALL lcase (text32)

  find_l: DO i6 = 1, 36
    IF (text32 (i6:i6) .eq.'l') EXIT find_l
  END DO find_l

  prof_nr = text32 (i6 + 1:36)          ! PROF_NR beinhaltet nur die Nummer des Profils aus der Oberflaeche,
                                        ! hat keine Bedeutung!

  !ilen = ju0nch (prof_nr)
  ilen = LEN_TRIM (prof_nr)

  find_space: DO i7 = 1, 26
    IF (prof_nr (i7:i7) .ne.' ') EXIT find_space
  END DO find_space

  prof_nr = prof_nr (i7:i7 + ilen)

  CLOSE (jw4)

  staso = stat (nprof)

  CALL intdat (staso, ifehl)

  IF (ifehl.ne.0) then
    STOP
  ENDIF


  !HB*******************************************************************
  !HB      26.11.2001 - H.Broeker
  !HB      ----------------------
  !HB      Zuweisung von Profilnummer und Stationskilometer
  !HB      fuer die Energiestrom- und Boussinesq-beiwertberechnung
  !HB      Uebergabe erfolgt durch COMMON/nr_alpha/pn_alpha,st_alpha
  !HB      an SUB EB2KS (siehe Vereinbarungsteil).
  pn_alpha = nprof
  st_alpha = staso
  !HB*******************************************************************


  write (*,1000) staso
  write (0,1000) staso
  1000 format (/1X, 'Berechnung an Profil: ', F8.4)



  !**   SCHREIBEN VON STASO IN KONTROLLFILE
  IF (lein.eq.3) write (jw8, '(''Bearbeiten Station km '',f12.4)') staso

  !     ******************************************************************
  !     ausdruck der eingelesenen daten im .dat-file
  !     ******************************************************************
  !     call druckles
  !     ******************************************************************
  !     q-wert-zuordnung
  !     ******************************************************************


  !**   Abspeichern alter Q-Wert:
  IF (nprof.gt.1) q1 = q

  IF (bordvoll.eq.'n'.and. nq.gt.1) then

    IF (nprof.gt.1) then
      dif1 = qstat (izz) - stat (nprof - 1)
      dif2 = qstat (izz) - stat (nprof)

      IF (dif1.gt.0..and.dif2.le.0.) then

        WRITE (jw5, 13) stat (nprof), qwert (izz)
        13 FORMAT (/,5x,'Durchflussaenderung bei Station km ',f7.3, &
                & ' :  Q = ',f7.2,' m**3/s')

        !** Bildschirmausausgabe deaktiviert
        !** write(*,13) stat(nprof),qwert(izz)

        !** SCHREIBEN IN KONTROLLFILE jw8
        IF (lein.eq.3) write (jw8, 13) stat (nprof), qwert (izz)

        nz = nz + 2
        q = qwert (izz)

        IF (izz.ne.merg) izz = izz + 1

      !**  ENDIF ZU if (dif1.gt.0.and.dif2.le.0.) then
      ENDIF

    !** ELSE ZU if (nprof.gt.1)
    ELSE

      !**          nprof=1:
      17 continue

      IF (izz.le. (nq - 1) ) then
        dif1 = qstat (izz) - stat (nprof)
        dif2 = qstat (izz + 1) - stat (nprof)

        IF (dif1.le.0..and.dif2.gt.0.) then
          WRITE (jw5, 14) stat (nprof), qwert (izz)


          IF (lein.eq.3) write (jw8, 14) stat (nprof), qwert (izz)
          14 FORMAT (/,5x,'Durchfluss bei Station km ',f7.3,  &
                  & ' :  Q = ',f7.2,' m**3/s')
          q = qwert (izz)
          nz = nz + 3
          izz = izz + 1

        ELSEIF (izz + 1.eq.nq.AND.qstat (izz + 1) .le.stat (nprof) ) then
          WRITE (jw5, 14) stat (nprof), qwert (izz + 1)
          q = qwert (izz + 1)
          nz = nz + 3

        ELSE

          izz = izz + 1
          GOTO 17

        ENDIF

      !**   ELSE ZU (izz.le.(nq-1))
      ELSE

        IF (dif1.lt.0.and.dif2.ge.0) then
          WRITE (jw5, 114) stat (nprof), qwert (izz)

          IF (lein.eq.3) write (jw8, 114) stat (nprof), qwert (izz)
          114 FORMAT (/,5x,'Durchfluss bei Station km ',f7.3, &
                   & ' :  Q = ',f7.2,' m**3/s')

          q = qwert (izz)
          izz = izz
          nz = nz + 3

        !**            ENDIF ZU (dif1.lt.0 .and. dif2.ge.0) then
        ENDIF

      !**          ENDIF ZU (izz.le.(nq-1))
      ENDIF

    !**      ENDIF ZU (nprof.gt.1)
    ENDIF

  !**   ELSEIF ZU (bordvoll.eq.'n'.and.nq.gt.1)
  ELSEIF (bordvoll.ne.'g') then
    q = qvar

    IF (nq.eq.1) then
      q = qwert (1)
    ENDIF

  !**   ENDIF ZU (bordvoll.eq.'n'.and.nq.gt.1)
  ENDIF


  !**   ******************************************************************
  !**   Ergaenzung zur Beruecksichtigung oert. Verluste 13.09.90 E. Pasche
  psieins = 0.
  psiorts = 0.

  oertl_verluste: DO ih = 1, jpsi
    IF (abs (psistat (ih) - staso) .lt.1.e-06) then
      psieins = psiein (ih)
      psiorts = psiort (ih)
      EXIT oertl_verluste
    ENDIF
  END DO oertl_verluste

  !**   Ende Ergaenzung v. 13.09.90  E. Pasche
  !**   ******************************************************************


  !**   ------------------------------------------------------------------
  !**   Anfangswasserspiegel fuer das erste Profil
  !**   ------------------------------------------------------------------

  IF (bordvoll.ne.'g') then

    IF (nprof.eq.1) then

      num (nprof) = prof_nr (1:10)

      !**      Berechnung Anfangswasserspiegel je nach Vorgabe von wsanf:
      hgrenz = 0.
      strbr = 0.

      CALL wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, &
        & hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt,  &
        & nz, idr1)

    !**   ABFRAGE ZUR BRUECKENBERECHNUNG
    ELSEIF (ibridge.eq.'b' .and. ibruecke.eq.'j') then


      !**   ------------------------------------------------------------------
      !**   Brueckenberechnung
      !**   ------------------------------------------------------------------

      print *

      ! SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then

        write (jw8, 22) x1(iwl), iwl, h1(iwl), x1(iwr), iwr, h1(iwr), hukmax, hmin, breite

        22 format(/1X, 'Fuer die Brueckenberechnung eingelesenen Werte:',/, &
                 & 1X, '-----------------------------------------------',/, &
                 & 1X, '  linkes Widerlager  X1(IWL) = ', F12.3,/, &
                 & 1X, '                     IWL     = ', I12,/,   &
                 & 1X, '                     H1(IWL) = ', F12.3,/, &
                 & 1X, '  rechtes Widerlager X1(IWR) = ', F12.3,/, &
                 & 1X, '                     IWR     = ', I12,/,   &
                 & 1X, '                     H1(IWR) = ', F12.3,/, &
                 & 1X, '  Brueckenunterkante HUKMAX  = ', F12.3,/, &
                 & 1X, '  Sohlhoehe HMIN             = ', F12.3,/, &
                 & 1X, '  Brueckenbreite BREITE      = ', F12.3/)

      ENDIF


      statgem = stat (nprof)              !WP Station gemerkt?
      stat1 = stat (nprof - 1)
      strecke = (statgem - stat1) * 1000. !WP Distanz zwischen Bruecke und Unterwasser

      !     SCHREIBEN IN KONTROLLFILE
      IF (lein .eq. 3) then
        write (jw8, 23) stat (nprof)
        WRITE (jw5, 23) stat (nprof)
      else
        WRITE (jw5, 23) stat (nprof)
      END if
      23 FORMAT (/,5x,'Brueckenberechnung an Station km   ',f7.3,' : ',/)



      ! 1.) Berechnung Profilwerte/Profilabstand Profil 'a' (uw)
      !     (zur beruecksichtigung aufweitungsverlust --> profilabstand
      !     < 4*(bges(nprof-1)-bnetto(bruecke profil '1'))
      ! -----------------------------------------------------------------------------------      --

      ! Breite Nettoprofil '1': /* siehe beschreibung und hec
      br1   = x1 (iwr) - x1 (iwl)
      delta = 4. * (brg (nprof - 1) - br1)    ! brg(): Breite gesamt an einem Profil

      IF ( (delta - 5.) .lt. 0.01) delta = 5.

      ! Brueckenbreite: Falls nicht definiert BREITE = 10 m
      IF (breite .lt. 1.e-6) then
        PRINT * , 'Brueckenbreite in Profildatei nicht definiert.'
        PRINT * , 'Default-Wert der Breite von 10 m in weiterer Be-'
        PRINT * , 'rechnung angesetzt!!!'
        breite = 10.
      ENDIF

      delta1 = delta + breite  ! Abstand des moeglichen Profils 'a' vom Brueckenprofil

      if (lein .eq. 3) then
        write (jw8, 24) delta1, strecke
      end if
      24 format (1X, 'Delta1: ', F10.3, '  Strecke: ', F10.3)


      ! Falls es zu einem Nennenswerten Aufweitungsverlust kommt, muss am Unterwasser
      ! ein Profil ('a') eingefuegt werden.
      IF (strecke .gt. delta1) then

        stat(nprof) = stat1 + (strecke-delta1) / 1000.
        str1 = (stat (nprof) - stat (nprof - 1) ) * 1000.

        prof_it(nprof) = 1

        num (nprof) = '0'



        PRINT * , 'Profil a : Station km ', stat (nprof)


        WRITE (jw5, '(/t10,'' Profil "a": '')')

        ! SCHREIBEN IN KONTROLLFILE
        IF (lein .eq. 3) then
          WRITE (jw8, '(''Profil "a" Station km '',f12.4)') stat (nprof)
        ENDIF

        dif = hmin - sohlp(nprof - 1)
        d1 = dif / strecke * delta
        d2 = ( (brg (nprof - 1) - br1) * delta / strecke+br1)  / br1
        x1wr = x1 (iwr)

        if (lein .eq. 3) then
          write (jw8, 25) dif, d1, d2, x1wr

        end if
        25 format (/1X, 'DIF:  ', F12.4, /, &
                  & 1X, 'D1:   ', F12.4, /, &
                  & 1X, 'D2:   ', F12.4, /, &
                  & 1X, 'X1WR: ', F12.4, / )

        DO i1 = 1, nknot

          h1 (i1) = h1 (i1) - d1

          IF (i1.gt.iwl.and.i1.le.iwr) then
            x1 (i1) = x1 (iwl) + (x1 (i1) - x1 (iwl) ) * d2
          ELSEIF (i1.gt.iwr) then
            x1 (i1) = (x1 (i1) - x1wr) + x1 (iwr)
          ENDIF

        END DO

        hmin  = hmin  - d1
        boli  = boli  - d1
        bore  = bore  - d1
        hrbv  = hrbv  - d1
        hming = hming - d1
        htrre = htrre - d1
        htrli = htrli - d1

        CALL normber (str1, q, q1, nprof, hr, hv, rg, hvst, hrst, &
        indmax, psieins, psiorts, jw5, hgrenz, ikenn, froud,      &
        nblatt, nz)

        IF (lein.eq.3) write (jw8, 26) hmin, hr
        26 format (/1X, 'Sohlhoehe HMIN: ', F12.4,/, &
                 &  1X, 'WSP:            ', F12.4,/)


        IF (nz .gt. 50) then
          nblatt = nblatt + 1
          CALL kopf (nblatt, nz, jw5, ifg, jw7, idr1)
        ENDIF

        CALL speicher (nprof, hr, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)

        CALL drucktab (nprof, indmax, nz, jw5, nblatt, stat, jw7, idr1)

        !**  Original Profilwerte herstellen:
        IF (ifg.eq.1) then
          DO i1 = 1, nknot
            ax (i1) = 0.0
            ay (i1) = 0.0
            dp (i1) = 0.0
          END DO
        ENDIF

        CALL intdat (staso, ifehl)

        IF (ifehl.ne.0) then
          STOP
        ENDIF

        nprof = nprof + 1

      ENDIF



      !**      Profil '3':
      stat (nprof) = statgem - breite / 1000.

      str1 = (stat (nprof) - stat (nprof - 1) ) * 1000.

      prof_it (nprof) = 1

      num (nprof) = '0'



      PRINT * , 'Profil 3 : Station km ', stat (nprof)

      !        SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then
        write (jw8, 27) stat (nprof)
      ENDIF
      27 format (/,1X, 'Berechnung in Subroutine Bruecke:', /, &
                &  1X, '---------------------------------', /, &
                &/,1X, 'Berechnung WSP im Unterwasser,   ', /, &
                &  1X, 'Profil "3" (interpoliert) bei Station km ',F12.4,/)


      CALL br_konv (staso, str1, q, q1, nprof, hr, hv, rg, hvst,  &
      hrst, indmax, psieins, psiorts, jw5, nblatt,  &
      nz, jw7, idr1, hgrenz, ikenn, ifroud, iwehrb, lein, jw8)

      ! mit vorgebenem wsp hr3 in wspanf --> berechnen wsp unverbautes

      staso = stat (nprof)
      IF (ifg.eq.1) then
        DO i1 = 1, nknot
          ax (i1) = 0.0
          ay (i1) = 0.0
          dp (i1) = 0.0
        END DO
      ENDIF

      CALL intdat (staso, ifehl)

      stat (nprof) = statgem

      prof_it (nprof) = 0

      num (nprof) = prof_nr (1:10)


      !**   ------------------------------------------------------------------
      !**   Zusammensetzen der Profillinie (mit '*' gekennzeichnete Linie)
      !**   ------------------------------------------------------------------
      !
      !     *************....................****************
      !     ............*.......... .........*..............
      !               . *         . .       **
      !                 ****      . .     *
      !                     *     . .    *
      !                     *************
      !

      CALL linier (hokmax, x1, h1, rau, nknot, iwl, iwr, npl, xl, hl, raul)

      !**   ------------------------------------------------------------------
      !**   Berechnung des WSP im ungestoerten, eingeengten Profil '3'
      !**   ------------------------------------------------------------------
      !     Commonblock fuer die Gewaessergeometrie ueberschreiben
      !     x1,h1,rau,boli,bore .... festlegen
      !        h1(1)      = hokmax
      !        x1(1)      bleibt
      !        rau(1)     = raub
      !        h1(npl+2)  = hokmax
      !        x1(npl+2)  = x1(nknot)
      !        rau(npl+2) = raub
      !        nknot      = npl+2

      !**     hsohl WIRD NICHT WEITER BENUTZT, 12.01.00, UT
      hsohl = hmin
      hmax = hokmax

      !**    SCHREIBEN IN KONTROLLFILE
      IF (lein .eq. 3) then
        WRITE (jw8, 28) iokl, iokr
      ENDIF
      28 format (/1X, 'IOKL (Nummer des Punktes der Oberkante links):  ', I3, /, &
                & 1X, 'IOKR (Nummer des Punktes der Oberkante rechts): ', I3)

      i1 = iokl
      i2 = iokl + npl

      IF (ifg .eq. 1) then
        DO j = i1, i2
          ax (j) = 0.0
          ay (j) = 0.0
          dp (j) = 0.0
        END DO
      ENDIF

      IF (iokl .gt. 1) then

        DO j = 1, iokl
          rau (j) = rau (j)
          IF (j.eq.iokl) rau (j) = raub
        END DO

      ELSE
        raul (1) = raub
      ENDIF


      IF (iokr .lt. nknot) then

        DO j = iokr, nknot

          x11 (j) = x1 (j)
          h11 (j) = h1 (j)
          rau1 (j) = rau (j)

          !**      dp1 WIRD NICHT WEITER BENUTZT, 12.01.00, UT
          IF (ifg.eq.1) then
            ax1 (j) = ax (j)
            ay1 (j) = ay (j)
            dp1 (j) = dp (j)
          ENDIF

        END DO

        DO j = iokr, nknot

          i2 = i2 + 1
          x1 (i2) = x11 (j)
          h1 (i2) = h11 (j)
          rau (i2) = rau1 (j)

          IF (ifg.eq.1) then
            ax (i2) = ax1 (j)
            ay (i2) = ay1 (j)
            dp (i2) = dp (j)
          ENDIF

        END DO


      !**   ELSE ZU (iokr.lt.nknot)
      ELSE

        i2 = i2 + 1
        x1 (i2) = x1 (nknot)
        h1 (i2) = h1 (nknot)
        rau (i2) = rau (nknot)

      !**   END ZU (iokr.lt.nknot)
      ENDIF


      nknot = i2

      ianf = 2

      iend = npl
      hminn = 1000.
      hming = 1.e+06


      DO ii1 = 1, npl

        i1 = i1 + 1
        x1 (i1) = xl (ii1)
        h1 (i1) = hl (ii1)
        rau (i1) = raul (ii1)

        IF (ii1.eq.npl) rau (i1) = raub

        IF (ii1.ge.ianf.and.ii1.le.iend) then
          IF (h1 (i1) .lt.hminn) then
            isohl = i1
            hminn = h1 (i1)
          ENDIF
        ENDIF

        IF (hming.gt.h1 (i1) ) then
          hming = h1 (i1)
          iming = i1
        ENDIF

      END DO


      hming = hmin

      ianf = iokl + 1
      iend = iokl + npl


      !**    SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then
        WRITE (jw8, '('' ianf='',i3,'' iend='',i3,)') ianf, iend
      ENDIF


      boli = h1 (ianf)
      bore = h1 (iend)
      hrbv = hokmax

      itrli = iokl + 1
      itrre = iokl + npl

      htrli = h1 (itrli)
      htrre = h1 (itrre)


      !      SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then
        WRITE (jw8, '('' itrli='',i3,'' itrre='',i3)') itrli, itrre
      ENDIF

      !**   SCHREIBEN IN KONTROLLFILE jw8
      DO j = 1, nknot
        IF (lein.eq.3) then

          IF (ifg.eq.1) then

            WRITE (jw8, '(''i='',i3,''x1='',f8.3,'' h1='',f8.3,'' rau='',f8.3, &
             & '' ax='',f8.3,'' ay='',f8.3,'' dp='',f8.3)') j, x1 (j), &
             & h1 (j) , rau (j) , ax (j) , ay (j) , dp (j)
          ELSE
            WRITE (jw8, '(''i='',i3,''x1='',f8.3,'' h1='',f8.3,''rau='',f8.3)') &
             & j, x1 (j) , h1 (j) , rau (j)
          ENDIF

        ENDIF

      END DO


      WRITE (jw5, '(/,t10,'' Profil "1":'')')


      !**       Bestimmung des Wasserspiegels im Profil "1"
      !**       --> hr oder hgrenz (wsanf = 0.)
      IF (ifroud.eq.1) then
        strbr = breite
        wsanf1 = 0.
      ELSE
        strbr = 0.
        wsanf1 = hr
      ENDIF

      !**       iwehr WIRD NIE GESETZT, WO WIRD ES UEBERGEBEN?, 12.01.00, UT?
      IF (iwehrb.eq.0) then

        CALL wspanf (wsanf1, strbr, q, q1, hr, hv, rg, indmax,    &
        hvst, hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn,  &
        nblatt, nz, idr1)

      ELSE

        henow = hr

        CALL wspow (henow, strbr, q, q1, hr, hv, rg, indmax, hvst,&
        hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt,&
        nz, idr1)

      !**  ENDIF ZU (iwehr.eq.q)
      ENDIF

    !**   ELSEIF ZU (nprof.eq.1)
    ELSEIF (iwehr.eq.'w'.and.wehr.eq.'j') then



      !**   --------------------------------------------------------------
      !**   Wehrberechnung
      !**   --------------------------------------------------------------

      WRITE (jw5, 323) stat (nprof)
      323 FORMAT   (/,5x,'Wehrberechnung an Station km   ',f7.3,' : ',/)

      nz = nz + 3

      !**      SCHREIBEN IN KONTROLLFILE OHNE VARIABLE?
      IF (lein.eq.3) write (jw8, 323)

      num (nprof) = prof_nr (1:10)

      CALL w_ber (henw, q, nprof, nz, jw5, ifg, jw7, idr1, nblatt)

      !**      Wasserspiegel im Oberwasser, WELCHE VARIABLE WIRD GESCHRIEBEN?
      WRITE (jw5, 324)

      !**      SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) write (jw8, 324)
      324 FORMAT   (/,5x,'Oberwasser:')

      nz = nz + 2

      strbr = 0.

      CALL wspow (henw, strbr, q, q1, hr, hv, rg, indmax, hvst,   &
       & hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt,  &
       & nz, idr1)

    !UT   ELSE FUER nprof=1, im FOLGENDEN nprof groesser 1!
    ELSE


      !**   ---------------------------------------------------------------
      !**   Normale Berechnung fuer alle weiteren Profile ausser nprof=1
      !**   ---------------------------------------------------------------

      num (nprof) = prof_nr (1:10)


      222 CONTINUE

      !**   str = abstand zwischen 2 Profilen?
      str = (stat (nprof) - stat (nprof - 1) ) * 1000.

      CALL normber (str, q, q1, nprof, hr, hv, rg, hvst, hrst,    &
       & indmax, psieins, psiorts, jw5, hgrenz, ikenn, froud, nblatt,&
       & nz)

    !**   ENDIF ZU (nprof.eq.1)
    ENDIF



    !**   ------------------------------------------------------------------
    !**   Abspeichern der Ergebnisse
    !**   ------------------------------------------------------------------

    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, jw5, ifg, jw7, idr1)
    ENDIF

    CALL speicher (nprof, hr, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)

    !**   ------------------------------------------------------------------
    !**   Ausdrucken der Ergebisse im .tab-file
    !**   ------------------------------------------------------------------

    CALL drucktab (nprof, indmax, nz, jw5, nblatt, stat, jw7, idr1)


    IF (nprof.gt.1) then
      IF (ibridge.eq.'b'.and.ibruecke.eq.'j') then

        !     Einlesen aus flussname.dat:
        READ (jw2, '(a)', end = 5000) dummy
        CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)
        IF (ifehl.ne.0.or.ianz.eq.0) then
          PRINT * , 'Fehlerhaftes Format in der Profil.dat-datei.'
          PRINT * , 'Es wurde keine Station gelesen.'
          PRINT * , 'Eingelesen wurde : ', dummy
          STOP 0
        ENDIF

        statneu = feldr (1)

        BACKSPACE (jw2)

        statgrz = stat (nprof) + 5. / 1000.

        IF (statneu.gt.statgrz + 0.001) then

          staso = stat (nprof)

          CALL intdat (staso, ifehl)

          ibridge = ' '

          nprof = nprof + 1

          prof_it (nprof) = 1

          num (nprof) = '0'

          stat (nprof) = stat (nprof - 1) + 5. / 1000.
          WRITE (jw5, '(/,t10,'' Profil "b":'')')

          PRINT * , 'Profil b : Station km ', stat (nprof)

          !             SCHREIBEN IN KONTROLLFILE
          IF (lein.eq.3) then
            WRITE (jw8, '(''Profil "b" Station km '',f12.4)') stat (nprof)
          ENDIF

          d1 = 0.05
          hminn = 1000.

          DO i1 = 1, nknot
            h1 (i1) = h1 (i1) + d1
            IF (i1.ge.ianf.and.i1.le.iend) then
              IF (h1 (i1) .lt.hminn) then
                isohl = i1
                hminn = h1 (i1)
              ENDIF
            ENDIF
          END DO

          hsohl = hmin + d1
          hmax = hmax + d1
          htrre = htrre+d1
          htrli = htrli + d1


          boli = h1 (ianf)
          bore = h1 (iend)
          hrbv = hrbv + d1
          hming = hming + d1
          hmin = hmin + d1

          !**            goto 222
          !**-----------------------------------------------

          str = (stat (nprof) - stat (nprof - 1) ) * 1000.

          CALL normber (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
           & indmax, psieins, psiorts, jw5, hgrenz, ikenn, froud,  &
           & nblatt, nz)

          !**   ------------------------------------------------------------------
          !**   Abspeichern der Ergebnisse
          !**   ------------------------------------------------------------------

          if (nz.gt.50) then
            blatt = nblatt + 1
            call kopf (nblatt, nz, jw5, ifg, jw7, idr1)
          endif

          CALL speicher (nprof, hr, hv, hvst, hrst, q, stat(nprof), indmax, ikenn)

          !**   -----------------------------------------------------------
          !**   Ausdrucken der Ergebisse im .tab-file
          !**   -----------------------------------------------------------

          CALL drucktab (nprof, indmax, nz, jw5, nblatt, stat, jw7, idr1)

        !**    ELSE ZU (statneu.gt.statgrz+0.001)???
        ELSE

          !  ENDE DER PROFILSCHLEIFE AM ENDE DES PROGRAMMS
          CYCLE Hauptschleife

        !   ENDIF ZU (ibridge.eq.'b'.and.ibruecke.eq.'j')???
        ENDIF

      !       ENDIF ZU (nprof.gt.1)???
      ENDIF


      IF (iprof.eq.' ') then

        IF (igrenz (nprof) .eq.1.or.igrenz (nprof) .eq.2) then

          IF (igrenz(nprof-1).ne.1 .and. igrenz(nprof-1) .ne. 2) then

            !**   Einlesen aus flussname.dat:
            READ (jw2, '(a)', end = 5000) dummy

            CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)

            IF (ifehl.ne.0.or.ianz.eq.0) then
              PRINT * , 'Fehlerhaftes Format in der Profil.dat-datei.'
              PRINT * , 'Es wurde keine Station gelesen.'
              PRINT * , 'Eingelesen wurde : ', dummy
              STOP 0
            ENDIF

            statneu = feldr (1)

            !**   POINTER IN DATEI UM EINEN RECORD ZURUECKSETZEN
            BACKSPACE (jw2)

            statgrz = stat (nprof) + 5. / 1000.


            IF (statneu.gt.statgrz + 0.001) then

              nprof = nprof + 1

              difs = 5.

              stat (nprof) = stat (nprof - 1) + difs / 1000.

              prof_it (nprof) = 1

              num (nprof) = '0'


              write (*,1000) stat(nprof)

              IF (lein.eq.3) then
                WRITE (jw8, '(/,''Bearbeiten Station km '',f12.4)') stat (nprof)
              ENDIF

              IF (iprof.ne.' ') then
                d1 = abs (sohlg) * difs
              ELSE
                d1 = 0.05
              ENDIF


              hmin = 1000.

              DO i1 = 1, nknot
                h1 (i1) = h1 (i1) + d1
                IF (i1.ge.ianf.and.i1.le.iend) then
                  !**             if (h1(i1).lt.hminn) then
                  IF (h1 (i1) .lt.hmin) then
                    isohl = i1
                    !**     hminn=h1(i1)
                    hmin = h1 (i1)
                  ENDIF
                ENDIF
              END DO


              hsohl = hmin + d1
              hmax = hmax + d1
              htrre = htrre+d1
              htrli = htrli + d1

              boli = boli + d1
              bore = bore+d1
              hrbv = hrbv + d1
              hming = hming + d1

              str = (stat (nprof) - stat (nprof - 1) ) * 1000.

              CALL normber (str, q, q1, nprof, hr, hv, rg, hvst,     &
               & hrst, indmax, psieins, psiorts, jw5, hgrenz, ikenn, &
               & froud, nblatt, nz)

              !**   --------------------------------------------------------------
              !**   Abspeichern der Ergebnisse
              !**   --------------------------------------------------------------

              IF (nz.gt.50) then
                nblatt = nblatt + 1
                CALL kopf (nblatt, nz, jw5, ifg, jw7, idr1)
              ENDIF

              CALL speicher (nprof, hr, hv, hvst, hrst, q, stat(nprof), indmax, ikenn)

              !**   ----------------------------------------------------------
              !*    Ausdrucken der Ergebisse im .tab-file
              !**   ----------------------------------------------------------

              CALL drucktab (nprof, indmax, nz, jw5, nblatt, stat, jw7, idr1)

            !**     ENDIF ZU (statneu.gt.statgrz+0.001)
            ENDIF

          !**     Die folgenden ENDIFs STEHEN FUER:
          !**     if (iprof.eq.' ')then
          !**      if (igrenz(nprof).eq.1.or.igrenz(nprof).eq.2) then
          !**       if (igrenz(nprof-1).ne.1.and.igrenz(nprof-1).ne.2) then
          ENDIF

        ENDIF

      ENDIF

    !**    ENDIF ZU (statneu.gt.statgrz+0.001)?
    ENDIF

    !**     idr=j IM FALL VON BORDVOLLBERECHNUNG
    IF (idr1.eq.'j') close (jw7)


  !**    ELSE FUER BORDVOLLBERECHNUNG STATIONAER GLEICHFOERMIG?
  ELSE


    !**     Bestimmung des Sohlgefaelles
    hbv (nprof) = MIN (boli, bore)
    num (nprof) = prof_nr (1:10)

    IF (nprof.gt.1) then

      !**   BERECHNUNG DES ABSTANDES ZWISCHEN 2 STATIONEN IN [m]
      difstat = (stat (nprof) - stat (nprof - 1) ) * 1000.

      IF (difstat.gt.5.) then

        isstat (nprof - 1) = (hmin - hmin2) / difstat

        IF (isstat (nprof - 1) .lt.1.e-05) then

          !**        SCHREIBEN IN KONTROLLFILE jw8 WENN lein=3
          IF (lein.eq.3) then
            WRITE (jw8, '(''Warnung.    &
             & Sohlgefaelle an Station'',f10.4,'' kleiner gleich Null'')'&
             &) stat (nprof)
            WRITE (jw8, '(''Setze Sohlgefaelle = 0.00001 '')')
          ENDIF

          !**        DAS GEFAELLE WIRD ZU EINEM HUNDERSTEL PROMILLE GESETZT
          isstat (nprof - 1) = 0.00001

        !**       ENDIF ZU (isstat(nprof-1).lt.1.e-05)
        ENDIF

        !**      isstat(nprof-1) = -1.*isstat(nprof-1)

      !**     ELSE ZU (difstat.gt.5.)
      ELSE

        !**        DAS GEFAELLE WIRD ZU NULL GESETZT
        isstat (nprof - 1) = 0.0

      !**     ENDIF ZU (difstat.gt.5.)
      ENDIF


      !**   Das Gefaelle am letzten Profil wird gleich dem am vorherigen
      !**   Profil gesetzt.
      isstat (nprof) = isstat (nprof - 1)


    !**   ELSE ZU (nprof.gt.1)
    ELSE

      !**        Gefaelle am ersten Profil
      isstat (nprof) = 0.0

    !**   ENDIF ZU (nprof.gt.1)
    ENDIF

    hmin2 = hmin

    str = 0.

    IF (km.eq."j") then

      hdiff = hbv (nprof) - hmin

      IF (i_km.eq.1) then

        hh1 = hmin + 0.1
        hh2 = hmin + 0.15 * hdiff
        hbv_gl = min (hh1, hh2)

      ELSEIF (i_km.eq.2) then

        hh1 = hmin + 0.2
        hh2 = hmin + 0.3 * hdiff
        hbv_gl = min (hh1, hh2)


      ELSEIF (i_km.eq.3) then

        hbv_gl = 0.5 * (hwsp (2, nprof) + hbv (nprof) )

        !**   hbv_gl = 0.5*(hbv_gl + hbv(nprof))

      ELSEIF (i_km.eq.4) then

        hbv_gl = hbv (nprof)

      ELSEIF (i_km.eq.5) then

        hbv_gl = hbv (nprof) + 0.2

        IF (iprof.ne.' ') hbv_gl = hbv (nprof)

      !**         ELSE ZU (i_km.eq.1)
      ELSE

        !**         18.11.98 Csocsan hh1 = hrmax
        !**         18.11.98 Csocsan hh2 = hwsp(5,nprof) + 0.2
        !**         18.11.98 Csocsan hbv_gl = max(hh1,hh2)
        hh1 = hmax
        hh2 = hwsp (5, nprof) + 2
        hbv_gl = min (hh1, hh2)

        !**           18.11.98 Csocsan
        IF (hbv_gl.le. (hbv (nprof) + 0.2) ) then
          hbv_gl = hbv (nprof) + 0.4
        ENDIF
        !**           Ende 18.11.98

        IF (iprof.ne.' ') hbv_gl = hbv (nprof)

      !**         ENDIF ZU (i_km.eq.1)
      ENDIF

      CALL normber (str, q, vmbv, nprof, hbv_gl, hv, rg, hvst,    &
       & hrst, indmax, psieins, psiorts, jw5, hgrenz, ikenn, froud,  &
       & nblatt, nz)

      IF (i_km.eq.4) then

        !**    SETZE DIE ERHALTENEN WERTE DEN FELDERN GLEICH
        qbord (nprof) = q
        hbord (nprof) = hbv_gl
        vbord (nprof) = vmbv

      ENDIF

    !**     ELSE ZU (km.eq."j")
    ELSE

      hbv_gl = hbv (nprof)

      CALL normber (str, q, vmbv, nprof, hbv_gl, hv, rg, hvst,    &
      & hrst, indmax, psieins, psiorts, jw5, hgrenz, ikenn, froud,  &
      & nblatt, nz)

      !**      SETZE DIE ERHALTENEN WERTE DEN FELDERN GLEICH
      qbord (nprof) = q
      hbord (nprof) = hbv_gl
      vbord (nprof) = vmbv

    !**     ENDIF ZU (km.eq."j")
    ENDIF


    !**   -------------------------------------------------------------
    !**   Abspeichern der Ergebnisse
    !**   -------------------------------------------------------------

    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, jw5, ifg, jw7, idr1)
    ENDIF

    CALL speicher (nprof, hbv_gl, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)


    !**   ------------------------------------------------------------------
    !**   Ausdrucken der Ergebisse im .tab-file
    !**   ------------------------------------------------------------------

    CALL drucktab (nprof, indmax, nz, jw5, nblatt, stat, jw7, idr1)

  !**    ENDIF ZU (bordvoll.eq.g)?
  ENDIF                                                                                    
                                                                        


!** DO-SCHLEIFE ZUM EINLESEN DER GERINNEABSCHNITTE
END DO Hauptschleife
                                                                        
!**   ------------------------------------------------------------------
!**   Berechnungsende                                                   
                                                                        



5000 CONTINUE

CLOSE (jw5)
                                                                        
!HB   ***************************************************************** 
!HB   26.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB   Schliessen der Datei Beiwerte.AUS                                 
CLOSE (nr_alph)
!HB   ***************************************************************** 
                                                                        
!UT    Erzeugen des Wasserspiegellaengsschnittes (.wsl-file)            
999 CONTINUE


IF (bordvoll.eq.'n') then
  !**   ------------------------------------------------------------------
  !**   Erzeugen des Wasserspiegellaengsschnittes (.wsl-file)
  !**   ------------------------------------------------------------------
                                                                        
  unit5  = fnam1
  ilen   = LEN_TRIM (unit5)
  unit5 (ilen - 4:ilen - 1) = 'dath'
  iflen  = LEN_TRIM (fluss)

  IF (iflen.gt.4) then
    iflen = 4
  ENDIF

  unit5 (ilen + 1:nch80) = fluss (1:iflen)
  ilen   = LEN_TRIM (unit5)
  unit5 (ilen + 1:nch80) = '_'
  ilen   = LEN_TRIM (unit5)
  ierlen = LEN_TRIM (ereignis)

  IF (ierlen.gt.3) then
    ierlen = 3
  ENDIF

  unit5 (ilen + 1:nch80) = ereignis (1:ierlen)
  ilen   = LEN_TRIM (unit5)
  unit5 (ilen + 1:nch80) = '.wsl'
  jw6    = ju0gfu ()

  OPEN (unit = jw6, file = unit5, status = 'unknown')

  !ST------------------------------------------------
  !ST 29.03.2005
  ! Erzeuge Pfad- und Dateinamen f�r 'Laengsschnitt.txt'
  file_laengs = fnam1
  ilen = LEN_TRIM (file_laengs)
  file_laengs (ilen - 4:ilen - 1) = 'dath'
  file_laengs (ilen + 1:nch80) = 'laengsschnitt.txt'
  !ST------------------------------------------------

  mark = 1

  !**     Erstellung eines .wsl-files

  CALL lapro1 (unit5, pfad2, nprof, mark, file_laengs)

  CLOSE (jw6)

!**   ENDIF ZU (bordvoll.eq.'n')
ENDIF                                                           
                                                                        
                                                                        
9999 RETURN
                                                                        

END SUBROUTINE wspber
