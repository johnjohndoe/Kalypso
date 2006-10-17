!     Last change:  WP    1 Aug 2006   11:12 am
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



SUBROUTINE wspber ()
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
!HB   UNIT_OUT_ALPHA  -  Interne Dateinummer fuer Beiwerte.AUS                 
!HB   pn_alpha -  Profilnummer (=nprof)
!HB   st_alpha -  Stationskilometer (=staso)                            
!HB   ***************************************************************** 
!
!   Beachte Parameteranweisung maxkla - Anzahl der Punkte im Profil!
!
!   AUFGERUFENE ROUTINEN
!   --------------------
!   drucktab (nprof,indmax,nz,jw5,nblatt,stat,jw7,idr1)
!   intdat (staso,ifehl)
!   kopf (nblatt,nz,jw5,jw7,idr1)
!   lcase (unit4)
!   proe_pro (jw4,text33,filename)
!   speicher (nprof,hbv_gl,hv,hvst,hrst,q,stat(nprof),indmax,ikenn)
!
!***********************************************************************


! ------------------------------------------------------------------
! PARAMETERVEREINBARUNG UND COMMONBLOECKE
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE VERSION
USE IO_UNITS
USE IO_NAMES
USE MOD_ERG
USE MOD_INI

implicit none

! COMMON-Block /ALPH_PF/ -----------------------------------------------------------
INTEGER 		:: nr_alph
CHARACTER(LEN=nch80)    :: alph_aus     ! Projektpfad fuer Beiwerte.AUS
COMMON / alph_pf / alph_aus, nr_alph
! ----------------------------------------------------------------------------------


! COMMON-Block /BRUECK/ ------------------------------------------------------------
INTEGER 	:: iwl, iwr, nuk, nok
INTEGER 	:: iokl, iokr           ! Gelaende Oberkante Grenze
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk, hokmin
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, iokl, iokr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, hokmin, ibridge
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
!CHARACTER(LEN=nch80) :: ereignis, fnam1, fluss
!CHARACTER(LEN=1) :: bordvoll
!COMMON / p1 / ereignis, fnam1, bordvoll, fluss
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
INTEGER :: istat
LOGICAL :: is_open

INTEGER :: izz, ilen, ilen2, iflen, jlen, ifehl, iint, ichar, ianz
INTEGER :: iwehrb, ifroud, mark
INTEGER :: npl

INTEGER :: np (ipro)
INTEGER :: int (merg)

INTEGER :: nblatt       ! Blattnr. bei der Ausgabe der Textdatei in Tabellenform
INTEGER :: nz

INTEGER :: ikenn        ! Zustandskennung (= 0 fuer Normalberechnung, = 1 fuer Schiessen mit Grenztiefe)
INTEGER :: indmax       ! Anzahl der Rauhigkeitzonen im aktiven Profil

INTEGER :: ih, i, j, k, i1, i2, i6, i7, ii1     ! Schleifenzaehler

INTEGER :: ju0gfu       ! Funktion zum Holen einer freien UNIT

CHARACTER(LEN=26) :: prof_nr

CHARACTER(LEN=nch80) :: char (merg)
CHARACTER(LEN=nch80) :: unit4, dummy, text
CHARACTER(LEN=nch80) :: unit5, unit7
CHARACTER(LEN=nch80) :: pfad2   	! Dateipfad wird Zeichenkette zugewiesen
CHARACTER(LEN=nch80) :: nr          	! Beinhaltet den Namen der Profildatei (z.b. St000150.prf)
!ST 29.03.2005
!Variable für Laengschnitt.txt
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
REAL :: statles
REAL :: staso
REAL :: q, q1                           ! Abfluesse
REAL :: dif1, dif2, str                 ! Abstand zwischen zwei Profilen
REAL :: dif, d1, d2, x1wr
REAL :: froud                           ! Froudzahl am Profil
REAL :: hsohl, hminn
REAL :: wsanf1
REAL :: henow, henw
REAL :: statneu, statgrz
REAL :: difs, difstat
REAL :: hmin2, hdiff, hh1, hh2
REAL :: hbv_gl, vmbv

REAL :: feldr (merg)
REAL :: xl (maxkla), hl (maxkla), raul (maxkla)

!**   Hilfsparameter fuer das Vorland, einschlisslich rau1(maxkla)
REAL :: x11 (maxkla), h11 (maxkla), ax1 (maxkla), ay1 (maxkla), dp1 (maxkla), rau1 (maxkla)



! ------------------------------------------------------------------
! VORBELEGUNGEN
! ------------------------------------------------------------------

!WP 10.05.2005
!WP Bei der normalen Spiegellinienberechnung wird der Dateiname
!WP PFAD2 nicht verwendet. MARK wird zu 1 gesetzt und dann ganz
!WP am Ende LAPRO1 aufgerufen. Trotzdem muss die CHARACTER-Variable
!WP PFAD2 einen gueltgen Wert bekommen
pfad2 = ' '

akges = 0.0

! izz = Zaehler fuer die mit q-wert belegten Stationen (q-werte aus
! qwert.dat, eingelesen im Steuerprogramm)
izz = 1

! nprof = Zaehler fuer die Anzahl der Profile im Berechnungsablauf
nprof = 0



! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------

! Eroeffnen der Ausgabedatei 'Flussname.tab' (dateiname-unit1)
UNIT_OUT_TAB = ju0gfu ()

OPEN (unit = UNIT_OUT_TAB, file = NAME_OUT_TAB, status = 'REPLACE', ACTION='WRITE', IOSTAT = istat)
if (istat /= 0) then
  write (*, 8900) NAME_OUT_TAB
  8900 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
             & 1X, 'Programm wird beendet!')
  call stop_programm(0)
end if

! Wenn BEIWERTE.AUS erzeugt wird
IF (alpha_ja.eq.1) then

  !HB     Schreiben der Kopfzeile der Ausgabedatei Beiwert.AUS
  WRITE (UNIT_OUT_ALPHA, 9911)
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


! -----------------------------------------------------------------------
! Erste Zeile in Ausgabedatei (Tabelle)
! -----------------------------------------------------------------------
text = ' '

text (6:nch80)  = 'Ergebnis der Wasserspiegellagenberechnung fuer '

ilen = LEN_TRIM (text)


IF (BERECHNUNGSMODUS == 'WATERLEVEL') then

  text (ilen + 2:nch80) = EREIGNISNAME

ELSEIF (BERECHNUNGSMODUS == 'BF_NON_UNI') then

  WRITE (EREIGNISNAME, '(f8.3)') qvar

  EREIGNISNAME = ADJUSTL (EREIGNISNAME)

  text (ilen + 2:nch80) = EREIGNISNAME

  ilen = LEN_TRIM (text)
  text (ilen + 1:nch80) = ' m**3/s'

ELSE

  text = 'Bordvollberechnung stationaer-gleichfoermig'

ENDIF


WRITE (UNIT_OUT_TAB, '(a)') text


! -----------------------------------------------------------------------
! Kopfzeile in Ausgabedatei (Tabelle)
! -----------------------------------------------------------------------
nblatt = 1
IF (idr1.ne.'j') then
  CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_TAB, idr1)
ENDIF

!**   nz wird in kopf zu 1 gesetzt, maxger ist Anzahl der
!**   Gerinneabschnitte
!**   REWIND setzt Strangtabelle auf Anfangspunkt zurueck
REWIND (UNIT_EIN_STR)

do i = 1, maxger
  out_PROF(i,nr_q)%interpol 	= .FALSE.
  out_PROF(i,nr_q)%chr_kenn 	= 'n'
  out_PROF(i,nr_q)%WehrOK 	= -999.999
  out_PROF(i,nr_q)%BrueckOK 	= -999.999
  out_PROF(i,nr_q)%BrueckUK 	= -999.999
  out_PROF(i,nr_q)%BrueckB 	= -999.999
  out_PROF(i,nr_q)%RohrD        = -999.999
  out_PROF(i,nr_q)%qges         = 0.0
end do



!WP -----------------------------------------------------------------------------------
!WP Beginn der Hauptschleife über alle Profile
!WP -----------------------------------------------------------------------------------

Hauptschleife: DO i = 1, maxger

  if (RUN_MODUS == 'KALYPSO') then

    read (UNIT_EIN_STR, *, IOSTAT=istat) nr, statles
    ilen=LEN_TRIM(nr)

    if (istat/=0) EXIT Hauptschleife

  else
    ! Einlesen aus Strangtabelle
    READ (UNIT_EIN_STR, '(a)', end = 5000) dummy

    CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ifehl.ne.0.or.ianz.eq.0) then
      write (*, 9000) dummy
      9000 format (/1X, 'Fehlerhaftes Format in der Profildatei!', /, &
                  & 1X, 'Es wurde keine Station gelesen -> Abbruch!', /, &
                  &/1X, '(gelesen wurde: ', A, ')' )
      call stop_programm(0)
    ENDIF

    ! Station gelesen
    statles = feldr (1)

    ilen = LEN_TRIM (char (1) )                                            

    nr = char (1) (1:ilen)        ! NR beinhaltet den Namen der Profildatei (z.b. St000150.prf)

  end if


  !WP Wenn gelesene Station kleiner als Anfangsstation, dann nochmal lesen
  IF (statles .lt. ANFANGSSTATION) CYCLE Hauptschleife

  !WP Wenn gelesene Station größer als Endstation, dann Programm beenden
  IF (statles .gt. ENDSTATION) EXIT Hauptschleife

  ! Anzahl der Profile fuer die WSP-Berechnung:
  nprof = nprof + 1
  stat (nprof) = statles

  prof_it (nprof) = 0

  out_PROF(nprof,nr_q)%stat = stat(nprof)


  ! ------------------------------------------------------------------
  ! Einlesen der Profildatei NR (z.B. St000150.prf)
  ! ------------------------------------------------------------------
                                                                        
  ilen  = LEN_TRIM (NAME_PFAD_PROF)
  ilen2 = LEN_TRIM (nr)

  NAME_EIN_PROF(i) = NAME_PFAD_PROF(1:ilen) // nr(1:ilen2)
  CALL lcase (NAME_EIN_PROF(i))

  UNIT_EIN_PROF = ju0gfu ()                       ! Leere Unit holen
  istat = 0


  write (UNIT_OUT_LOG, '(///,''----------------------------------------------------------------------'')')
  write (UNIT_OUT_LOG, '(''Oeffnen Profildatei '',a)') NAME_EIN_PROF(i)

  ! Zuweisung des kompletten Dateinamens der Profildatei zu einem Array
  dateiname(i) = NAME_EIN_PROF(i)

  OPEN (unit = UNIT_EIN_PROF, file = NAME_EIN_PROF(i), status = 'old', iostat = istat)
  IF (istat.ne.0) then
    write (*, 9001) NAME_EIN_PROF(i)
    write (UNIT_OUT_LOG, 9001) NAME_EIN_PROF(i)
    9001 format (/1X, 'Fehler: Datei existiert nicht: ', A, /, &
                & 1X, '-> Programm wird beendet.')
    GOTO 999
  ENDIF



  ! ------------------------------------------------------------------
  ! Eroeffnen der Ausgabedatei *.pro bei Bordvollberechnung
  ! --> Wasserstands-Abflussbeziehung fuer jede Station
  ! ------------------------------------------------------------------
  IF (idr1.eq.'j') then

    WRITE (text, '(f11.4)') stat(nprof)
    jlen = LEN_TRIM (text)

    DO k = 1, jlen
      IF (text (k:k) .eq.'.') then
        text (k:k) = '-'
      ENDIF
    END DO

    text = ADJUSTL (text)

    unit7 = NAME_PFAD_DATH

    iflen = LEN_TRIM (FLUSSNAME)

    IF (iflen.gt.2) then
      iflen = 2
    ENDIF

    unit7 (ilen + 1:nch80) = FLUSSNAME (1:iflen)
    ilen = LEN_TRIM (unit7)
    unit7 (ilen + 1:nch80) = text
    ilen = LEN_TRIM (unit7)
    unit7 (ilen + 1:nch80) = '.pro'
    UNIT_OUT_PRO = ju0gfu ()
    NAME_OUT_PRO(i) = unit7

    !WP isch ist Zaehler fuer Abflussereignis bei stationaer-ungl. Berechnung
    IF (isch.eq.1) then

      OPEN (UNIT=UNIT_OUT_PRO, FILE=NAME_OUT_PRO(i), STATUS='REPLACE', ACTION='WRITE', IOSTAT=istat)
      CALL kopf (nblatt, nz, UNIT_OUT_PRO, UNIT_OUT_PRO, idr1)

    ELSE

      OPEN (UNIT=UNIT_OUT_PRO, FILE=NAME_OUT_PRO(i), ACTION='WRITE', POSITION='APPEND', IOSTAT=istat)

      if (istat /= 0) then
        ! 9002
        write (*,*) ' In WSPBER ist ein Fehler beim Untersuchen des Zustandes'
        write (*,*) ' einer UNIT aufgetreten (INQUIRE-Befehl), ca. Zeile 697'
        write (*,*) ' -> Versuche weiterzurechnen...'
        GOTO 999
      end if

    ENDIF

  ENDIF



  ! ------------------------------------------------------------------
  ! Einlesen der Daten in Sub proein und Weiterverarbeitung in intdat
  ! ------------------------------------------------------------------

  CALL proe_pro (text33, 	& 	! -> Station km als Zeichenkette (Character)
               & unit4)                 ! <- Kompletter Pfad zur Profildatei

  text32 = text321


  ! ------------------------------------------------------------------
  ! Definition des Profils
  ! ------------------------------------------------------------------

  ! KONVERTIERUNG DES TEXTES text 32 VON KLEIN- ZU GROSSBUCHSTABEN
  CALL lcase (text32)

  find_l: DO i6 = 1, 36
    IF (text32 (i6:i6) .eq.'l') EXIT find_l
  END DO find_l

  prof_nr = text32 (i6 + 1:36)          ! PROF_NR beinhaltet nur die Nummer des Profils aus der Oberflaeche,
                                        ! hat keine Bedeutung!

  ilen = LEN_TRIM (prof_nr)

  find_space: DO i7 = 1, 26
    IF (prof_nr (i7:i7) .ne.' ') EXIT find_space
  END DO find_space

  prof_nr = prof_nr (i7:i7 + ilen)

  CLOSE (UNIT_EIN_PROF)

  staso = stat (nprof)

  CALL intdat (staso, ifehl)

  IF (ifehl.ne.0) then
    write (*, 9004) stat(nprof)
    9004 format (/1X, 'Fehler in INTDAT bei Station ', F12.4, '.', /, &
                & 1X, '-> Programm wird beendet.')
    call stop_programm(0)
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


  if (iprof .ne. ' ') then
    out_PROF(nprof,nr_q)%chr_kenn = iprof   	! Sonderprofil
    out_PROF(nprof,nr_q)%RohrD = durchm         ! Wenn Kreis -> Durchmesser, Wenn Trapez -> Breite
  end if


  ! --------------------------------------------------------------------------------------------------
  write (   *  ,      1000) staso
  write (   0  ,      1000) staso
  write (UNIT_OUT_LOG,1000) staso
  1000 format (/1X, 'Starte Berechnung an Profil: ', F10.4)


  !**   Abspeichern alter Q-Wert:
  IF (nprof.gt.1) q1 = q

  IF (BERECHNUNGSMODUS == 'WATERLEVEL' .and. nq.gt.1) then

    IF (nprof.gt.1) then

      dif1 = qstat (izz) - stat (nprof - 1)
      dif2 = qstat (izz) - stat (nprof)

      IF (dif1.gt.0. .and. dif2.le.0.) then

        WRITE (UNIT_OUT_TAB, 13) stat (nprof), qwert (izz)

        nz = nz + 2  ! Anzahl der Zeilen auf Seite in Dateo mit Tabellenausgabe (UNIT_OUT_TAB)

        write (UNIT_OUT_LOG, 13) stat (nprof), qwert (izz)
        13 FORMAT (/,5x,'Durchflussaenderung bei Station km ',f7.3, &
                & ' :  Q = ',f7.2,' m**3/s')

        q = qwert (izz)

        IF (izz.ne.merg) izz = izz + 1

      ENDIF

    ELSE

      !**          nprof=1:
      17 continue

      IF (izz.le. (nq - 1) ) then
        dif1 = qstat (izz) - stat (nprof)
        dif2 = qstat (izz + 1) - stat (nprof)

        IF (dif1.le.0..and.dif2.gt.0.) then
          WRITE (UNIT_OUT_TAB, 14) stat (nprof), qwert (izz)


          write (UNIT_OUT_LOG, 14) stat (nprof), qwert (izz)
          14 FORMAT (/,5x,'Durchfluss bei Station km ',f7.3,  &
                  & ' :  Q = ',f7.2,' m**3/s')
          q = qwert (izz)
          nz = nz + 3
          izz = izz + 1

        ELSEIF (izz + 1.eq.nq.AND.qstat (izz + 1) .le.stat (nprof) ) then
          WRITE (UNIT_OUT_TAB, 14) stat (nprof), qwert (izz + 1)
          q = qwert (izz + 1)
          nz = nz + 3

        ELSE

          izz = izz + 1
          GOTO 17

        ENDIF

      !**   ELSE ZU (izz.le.(nq-1))
      ELSE

        IF (dif1.lt.0.and.dif2.ge.0) then
          WRITE (UNIT_OUT_TAB, 114) stat (nprof), qwert (izz)

          write (UNIT_OUT_LOG, 114) stat (nprof), qwert (izz)
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

  ELSEIF (BERECHNUNGSMODUS /= 'BF_UNIFORM') then
    q = qvar

    IF (nq.eq.1) then
      q = qwert (1)
    ENDIF

  ENDIF

  out_PROF(nprof,nr_q)%qges = q         ! Abspeichern des Abflusses am aktuellen Profil

  ! ------------------------------------------------------------------------------------------------
  ! Ergaenzung zur Beruecksichtigung oert. Verluste 13.09.90 E. Pasche
  psieins = 0.
  psiorts = 0.

  oertl_verluste: DO ih = 1, jpsi
    IF (abs (psistat (ih) - staso) .lt.1.e-06) then
      psieins = psiein (ih)
      psiorts = psiort (ih)
      EXIT oertl_verluste
    ENDIF
  END DO oertl_verluste
  ! Ende Ergaenzung v. 13.09.90  E. Pasche
  ! ------------------------------------------------------------------------------------------------



  ! ------------------------------------------------------------------------------------------------
  ! Anfangswasserspiegel fuer das erste Profil

  IF (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

    IF (nprof.eq.1) then

      num (nprof) = prof_nr (1:10)

      !**      Berechnung Anfangswasserspiegel je nach Vorgabe von wsanf:
      hgrenz = 0.
      strbr = 0.

      CALL wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, &
        & hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt,  &
        & nz, idr1)

    !**   ABFRAGE ZUR BRUECKENBERECHNUNG
    ELSEIF (ibridge.eq.'b' .and. MIT_BRUECKEN) then


      ! ------------------------------------------------------------------
      ! Brueckenberechnung
      ! ------------------------------------------------------------------

      ! SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, 22) x1(iwl), iwl, h1(iwl), x1(iwr), iwr, h1(iwr), hukmax, hmin, breite
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


      statgem = stat (nprof)              !WP Station gemerkt?
      stat1 = stat (nprof - 1)
      strecke = (statgem - stat1) * 1000. !WP Distanz zwischen Bruecke und Unterwasser

      !     SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, 23) stat (nprof)
      WRITE (UNIT_OUT_TAB, 23) stat (nprof)
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

      write (UNIT_OUT_LOG, 24) delta1, strecke
      24 format (1X, 'Delta1: ', F10.3, '  Strecke: ', F10.3)



      ! ----------------------------------------------------------------------------------------------
      ! P R O F I L   "A"
      ! =================
      ! Falls es zu einem Nennenswerten Aufweitungsverlust kommt, muss am Unterwasser
      ! ein Profil ('a') eingefuegt werden.
      IF (strecke .gt. delta1) then

        stat(nprof) = stat1 + (strecke-delta1) / 1000.
        str1 = (stat (nprof) - stat (nprof - 1) ) * 1000.

        prof_it(nprof) = 1

        out_PROF(nprof,nr_q)%stat = stat(nprof)         !WP Zuweisung der Stationierung in globalen Array
                                                        !WP Hinweis: Je nach Abflusszustand kann sich die
                                                        !WP Anzahl der interpolierten Profile aendern
        out_PROF(nprof,nr_q)%interpol = .TRUE.  	!WP In dem globalen Ergebnis TYPE wird sich gemerkt,
        out_PROF(nprof,nr_q)%chr_kenn = 'i'         	!WP dass dieses Profil bei dem aktuellen Abfluss
                                                        !WP interpoliert wurde.
        out_PROF(nprof,nr_q)%qges = q
        !out_IND(nprof,nr_q,1)%lambda = 0.0
        !out_IND(nprof,nr_q,2)%lambda = 0.0
        !out_IND(nprof,nr_q,3)%lambda = 0.0

        num (nprof) = '0'


        write (   *  ,      1001) stat (nprof)
        write (   0  ,      1001) stat (nprof)
        write (UNIT_OUT_LOG,1001) stat (nprof)
        1001 format (/1X, '       Profil "a" (Bruecke): ', F10.4)

        WRITE (UNIT_OUT_TAB, '(/t10,'' Profil "a": '')')


        dif = hmin - sohlp(nprof - 1)
        d1 = dif / strecke * delta
        d2 = ( (brg (nprof - 1) - br1) * delta / strecke+br1)  / br1
        x1wr = x1 (iwr)

        write (UNIT_OUT_LOG, 25) dif, d1, d2, x1wr
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
         & indmax, psieins, psiorts, hgrenz, ikenn, froud,      &
         & nblatt, nz)

        write (UNIT_OUT_LOG, 26) hmin, hr
        26 format (/1X, 'Sohlhoehe HMIN: ', F12.4,/, &
                 &  1X, 'WSP:            ', F12.4,/)


        IF (nz .gt. 50) then
          nblatt = nblatt + 1
          CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
        ENDIF

        CALL speicher (nprof, hr, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)

        CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat, UNIT_OUT_PRO, idr1)

        !**  Original Profilwerte herstellen:
        if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
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



      ! ----------------------------------------------------------------------------------------------
      ! P R O F I L   "3"
      ! =================

      stat (nprof) = statgem - breite / 1000.

      str1 = (stat (nprof) - stat (nprof - 1) ) * 1000.

      prof_it (nprof) = 1

      num (nprof) = '0'

      out_PROF(nprof,nr_q)%stat = stat(nprof)   !WP Zuweisung der Stationierung in globalen Array
                                                !WP Hinweis: Je nach Abflusszustand kann sich die
                                                !WP Anzahl der interpolierten Profile aendern

      out_PROF(nprof,nr_q)%interpol = .TRUE.  	!WP In dem globalen Ergebnis TYPE wird sich gemerkt,
      out_PROF(nprof,nr_q)%chr_kenn = 'i'   	!WP dass dieses Profil bei dem aktuellen Abfluss
                                                !WP interpoliert wurde.
      out_PROF(nprof,nr_q)%qges = q

      write (   *  ,      1002) stat (nprof)
      write (   0  ,      1002) stat (nprof)
      write (UNIT_OUT_LOG,1002) stat (nprof)
      1002 format (/1X, '       Profil "3" (Bruecke): ', F10.4)

      write (UNIT_OUT_LOG, 27) stat (nprof)
      27 format (/,1X, 'Berechnung in Subroutine Bruecke:', /, &
                &  1X, '---------------------------------', /, &
                &/,1X, 'Berechnung WSP im Unterwasser,   ', /, &
                &  1X, 'Profil "3" (interpoliert) bei Station km ',F12.4,/)


      CALL br_konv (staso, str1, q, q1, nprof, hr, hv, rg, hvst,  &
       & hrst, indmax, psieins, psiorts, nblatt,  &
       & nz, idr1, hgrenz, ikenn, ifroud, iwehrb)

      !write (*,*) 'In WSPBER. zurueck aus BR_KONV.'

      ! mit vorgebenem wsp hr3 in wspanf --> berechnen wsp unverbautes

      staso = stat (nprof)
      if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
        DO i1 = 1, nknot
          ax (i1) = 0.0
          ay (i1) = 0.0
          dp (i1) = 0.0
        END DO
      ENDIF

      CALL intdat (staso, ifehl)

      stat (nprof) = statgem                          !WP Original Brueckenstation (Profil i)!
      out_PROF(nprof,nr_q)%stat = stat(nprof)         !WP Zuweisung der Stationierung in globalen Array
                                                      !WP Hinweis: Je nach Abflusszustand kann sich die
                                                      !WP Anzahl der interpolierten Profile aendern

      out_PROF(nprof,nr_q)%BrueckOK = hokmin
      out_PROF(nprof,nr_q)%BrueckUK = hukmax
      out_PROF(nprof,nr_q)%BrueckB = breite

      out_PROF(nprof,nr_q)%chr_kenn = 'b'

      out_PROF(nprof,nr_q)%qges = q

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
      WRITE (UNIT_OUT_LOG, 28) iokl, iokr
      28 format (/1X, 'IOKL (Nummer des Punktes der Oberkante links):  ', I3, /, &
                & 1X, 'IOKR (Nummer des Punktes der Oberkante rechts): ', I3)

      i1 = iokl
      i2 = iokl + npl

      if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
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
          if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
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

          if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
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
      WRITE (UNIT_OUT_LOG, '('' ianf='',i3,'' iend='',i3,)') ianf, iend

      boli = h1 (ianf)
      bore = h1 (iend)
      hrbv = hokmax

      itrli = iokl + 1
      itrre = iokl + npl

      htrli = h1 (itrli)
      htrre = h1 (itrre)


      !      SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '('' itrli='',i3,'' itrre='',i3)') itrli, itrre

      !**   SCHREIBEN IN KONTROLLFILE UNIT_OUT_LOG
      DO j = 1, nknot

        if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
          WRITE (UNIT_OUT_LOG, '(''i='',i3,''x1='',f8.3,'' h1='',f8.3,'' rau='',f8.3, &
           & '' ax='',f8.3,'' ay='',f8.3,'' dp='',f8.3)') j, x1 (j), &
           & h1 (j) , rau (j) , ax (j) , ay (j) , dp (j)
        ELSE
          WRITE (UNIT_OUT_LOG, '(''i='',i3,''x1='',f8.3,'' h1='',f8.3,''rau='',f8.3)') &
           & j, x1 (j) , h1 (j) , rau (j)
        ENDIF

      END DO


      WRITE (UNIT_OUT_TAB, '(/,t10,'' Profil "1":'')')




      ! ----------------------------------------------------------------------------------------------
      ! P R O F I L   "1"
      ! =================

      !**       --> hr oder hgrenz (wsanf = 0.)
      IF (ifroud.eq.1) then
        strbr = breite
        wsanf1 = 0.
      ELSE
        strbr = 0.
        wsanf1 = hr
      ENDIF

      !write (*,*) 'In WSPBER. Vor Berechnung Profil 1, IWEHRB = ', iwehrb

      ! iwehrb kommt aus BR_KONV, ist 0 oder 1
      IF (iwehrb.eq.0) then

        CALL wspanf (wsanf1, strbr, q, q1, hr, hv, rg, indmax,    &
         & hvst, hrst, psieins, psiorts, nprof, hgrenz, ikenn,  &
         & nblatt, nz, idr1)

      ELSE

        henow = hr

        CALL wspow (henow, strbr, q, q1, hr, hv, rg, indmax, hvst,&
         & hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt,&
         & nz, idr1)

      ENDIF

      !write (*,*) 'In WSPBER. Nach Berechnung Profil 1.'

    !**   ELSEIF ZU (nprof.eq.1)
    ELSEIF (iwehr.eq.'w'.and. MIT_WEHREN) then



      ! ---------------------------------------------------------------------------------
      ! Wehrberechnung
      ! ---------------------------------------------------------------------------------

      WRITE (UNIT_OUT_TAB, 323) stat (nprof)
      write (UNIT_OUT_LOG, 323)
      323 FORMAT   (/,5x,'Wehrberechnung an Station km   ',f7.3,' : ',/)

      nz = nz + 3

      num (nprof) = prof_nr (1:10)

      CALL w_ber (henw, q, nprof, nz, idr1, nblatt)


      WRITE (UNIT_OUT_TAB, 324)
      write (UNIT_OUT_LOG, 324)
      324 FORMAT   (/,5x,'Oberwasser:')

      nz = nz + 2

      strbr = 0.

      CALL wspow (henw, strbr, q, q1, hr, hv, rg, indmax, hvst,   &
       & hrst, psieins, psiorts, nprof, hgrenz, ikenn, nblatt,  &
       & nz, idr1)

      out_PROF(nprof,nr_q)%WehrOK   = hokwmin   !WP Speichern der Wehroberkante dieses Profils
      out_PROF(nprof,nr_q)%chr_kenn = 'w'

      out_PROF(nprof,nr_q)%qges = q

    !UT   ELSE FUER nprof=1, im FOLGENDEN nprof groesser 1!
    ELSE



      ! ---------------------------------------------------------------
      ! Normale Berechnung fuer alle weiteren Profile ausser nprof=1
      ! ---------------------------------------------------------------

      num (nprof) = prof_nr (1:10)


      222 CONTINUE

      !**   str = abstand zwischen 2 Profilen?
      str = (stat (nprof) - stat (nprof - 1) ) * 1000.

      CALL normber (str, q, q1, nprof, hr, hv, rg, hvst, hrst,    &
       & indmax, psieins, psiorts, hgrenz, ikenn, froud, nblatt,&
       & nz)

    !**   ENDIF ZU (nprof.eq.1)
    ENDIF





    ! ------------------------------------------------------------------
    ! Abspeichern der Ergebnisse
    ! ------------------------------------------------------------------

    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF

    CALL speicher (nprof, hr, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)

    ! ------------------------------------------------------------------
    ! Ausdrucken der Ergebisse im .tab-file
    ! ------------------------------------------------------------------

    CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat, UNIT_OUT_PRO, idr1)


    IF (nprof.gt.1) then

      IF (ibridge.eq.'b'.and. MIT_BRUECKEN) then

        !     Einlesen aus flussname.dat:
        READ (UNIT_EIN_STR, '(a)', end = 5000) dummy
        CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)
        IF (ifehl.ne.0.or.ianz.eq.0) then
          PRINT * , 'Fehlerhaftes Format in der Profil.dat-datei.'
          PRINT * , 'Es wurde keine Station gelesen.'
          PRINT * , 'Eingelesen wurde : ', dummy
          STOP 0
        ENDIF

        statneu = feldr (1)

        BACKSPACE (UNIT_EIN_STR)

        statgrz = stat (nprof) + 5. / 1000.


        ! ----------------------------------------------------------------------------------------------
        ! P R O F I L   "B"
        ! =================

        IF (statneu.gt.statgrz + 0.001) then

          staso = stat (nprof)

          CALL intdat (staso, ifehl)

          ibridge = ' '

          nprof = nprof + 1

          prof_it (nprof) = 1

          num (nprof) = '0'

          stat (nprof) = stat (nprof - 1) + 5. / 1000.

          out_PROF(nprof,nr_q)%stat = stat(nprof)	!WP Zuweisung der Stationierung in globalen Array
                                                        !WP Hinweis: Je nach Abflusszustand kann sich die
                                                        !WP Anzahl der interpolierten Profile aendern

          out_PROF(nprof,nr_q)%interpol = .TRUE.  	!WP In dem globalen Ergebnis TYPE wird sich gemerkt,
          out_PROF(nprof,nr_q)%chr_kenn = 'i'       	!WP dass dieses Profil bei dem aktuellen Abfluss
                                                        !WP interpoliert wurde.
          out_PROF(nprof,nr_q)%qges = q

          write (   *  ,      1003) stat (nprof)
          write (   0  ,      1003) stat (nprof)
          write (UNIT_OUT_LOG,1003) stat (nprof)
          1003 format (/1X, '       Profil "B" (Bruecke): ', F10.4)

          WRITE (UNIT_OUT_TAB, '(/,t10,'' Profil "b":'')')


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

          str = (stat (nprof) - stat (nprof - 1) ) * 1000.

          CALL normber (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
           & indmax, psieins, psiorts, hgrenz, ikenn, froud,  &
           & nblatt, nz)

          ! ------------------------------------------------------------------
          ! Abspeichern der Ergebnisse
          ! ------------------------------------------------------------------

          if (nz.gt.50) then
            nblatt = nblatt + 1
            call kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          endif

          CALL speicher (nprof, hr, hv, hvst, hrst, q, stat(nprof), indmax, ikenn)

          ! -----------------------------------------------------------
          ! Ausdrucken der Ergebisse im .tab-file
          ! -----------------------------------------------------------

          CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat, UNIT_OUT_PRO, idr1)

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
            READ (UNIT_EIN_STR, '(a)', end = 5000) dummy

            CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)

            IF (ifehl.ne.0.or.ianz.eq.0) then
              PRINT * , 'Fehlerhaftes Format in der Profil.dat-datei.'
              PRINT * , 'Es wurde keine Station gelesen.'
              PRINT * , 'Eingelesen wurde : ', dummy
              STOP 0
            ENDIF

            statneu = feldr (1)

            !**   POINTER IN DATEI UM EINEN RECORD ZURUECKSETZEN
            BACKSPACE (UNIT_EIN_STR)

            statgrz = stat (nprof) + 5. / 1000.


            IF (statneu.gt.statgrz + 0.001) then

              nprof = nprof + 1

              difs = 5.

              stat (nprof) = stat (nprof - 1) + difs / 1000.

              prof_it (nprof) = 1

              num (nprof) = '0'

              out_PROF(nprof,nr_q)%stat = stat(nprof)   !WP Zuweisung der Stationierung in globalen Array
                                                        !WP Hinweis: Je nach Abflusszustand kann sich die
                                                        !WP Anzahl der interpolierten Profile aendern

              out_PROF(nprof,nr_q)%interpol = .TRUE.  	!WP In dem globalen Ergebnis TYPE wird sich gemerkt,
              out_PROF(nprof,nr_q)%chr_kenn = 'i'   	!WP dass dieses Profil bei dem aktuellen Abfluss
                                                        !WP interpoliert wurde.
              out_PROF(nprof,nr_q)%qges = q

              write (    *       ,1000) stat(nprof)
              write (    0       ,1000) stat(nprof)
              write (UNIT_OUT_LOG,1000) stat(nprof)


              IF (iprof.ne.' ') then
                d1 = abs (sohlg) * difs
              ELSE
                d1 = 0.05
              ENDIF


              hmin = 1000.

              DO i1 = 1, nknot
                h1 (i1) = h1 (i1) + d1
                IF (i1.ge.ianf.and.i1.le.iend) then
                  IF (h1 (i1) .lt.hmin) then
                    isohl = i1
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
               & hrst, indmax, psieins, psiorts, hgrenz, ikenn, &
               & froud, nblatt, nz)

              ! --------------------------------------------------------------
              ! Abspeichern der Ergebnisse
              ! --------------------------------------------------------------

              IF (nz.gt.50) then
                nblatt = nblatt + 1
                CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
              ENDIF

              CALL speicher (nprof, hr, hv, hvst, hrst, q, stat(nprof), indmax, ikenn)

              ! ----------------------------------------------------------
              ! Ausdrucken der Ergebisse im .tab-file
              ! ----------------------------------------------------------

              CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat, UNIT_OUT_PRO, idr1)

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
    IF (idr1.eq.'j') close (UNIT_OUT_PRO)

  ELSE

    !write (*,*) 'In WSPBER. Stat.-gleichf. Bordvollberechnung.'

    !**     Bestimmung des Sohlgefaelles
    hbv (nprof) = MIN (boli, bore)
    num (nprof) = prof_nr (1:10)

    IF (nprof.gt.1) then

      !**   BERECHNUNG DES ABSTANDES ZWISCHEN 2 STATIONEN IN [m]
      difstat = (stat (nprof) - stat (nprof - 1) ) * 1000.

      IF (difstat.gt.5.) then

        isstat (nprof - 1) = (hmin - hmin2) / difstat

        IF (isstat (nprof - 1) .lt.1.e-05) then

          !**        SCHREIBEN IN KONTROLLFILE UNIT_OUT_LOG WENN lein=3
          WRITE (UNIT_OUT_LOG, '(''Warnung.    &
           & Sohlgefaelle an Station'',f10.4,'' kleiner gleich Null'')'&
           &) stat (nprof)
          WRITE (UNIT_OUT_LOG, '(''Setze Sohlgefaelle = 0.00001 '')')

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

    !write (*,*) 'In WSPBER. isstat(',nprof,') = ', isstat(nprof)

    hmin2 = hmin

    str = 0.

    IF (km .eq. 'j') then

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
       & hrst, indmax, psieins, psiorts, hgrenz, ikenn, froud,  &
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
      & hrst, indmax, psieins, psiorts, hgrenz, ikenn, froud,  &
      & nblatt, nz)

      !**      SETZE DIE ERHALTENEN WERTE DEN FELDERN GLEICH
      qbord (nprof) = q
      hbord (nprof) = hbv_gl
      vbord (nprof) = vmbv

    !**     ENDIF ZU (km.eq."j")
    ENDIF

    !write (*,*) 'In WSPBER. Profilberechnung fertig.'

    ! -------------------------------------------------------------
    ! Abspeichern der Ergebnisse
    ! -------------------------------------------------------------

    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF

    CALL speicher (nprof, hbv_gl, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)


    ! ------------------------------------------------------------------
    ! Ausdrucken der Ergebisse im .tab-file
    ! ------------------------------------------------------------------

    CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat, UNIT_OUT_PRO, idr1)

  ENDIF
                                                                        
!** DO-SCHLEIFE ZUM EINLESEN DER GERINNEABSCHNITTE
END DO Hauptschleife
                                                                        
! ------------------------------------------------------------------
! Berechnungsende
                                                                        



5000 CONTINUE

CLOSE (UNIT_OUT_TAB)


!UT    Erzeugen des Wasserspiegellaengsschnittes (.wsl-file)            
999 CONTINUE



!WP Zuweisung der Anzahl der Profile fuer diesen Abflusszustand
!WP (kann durch Interpolation an Bruecken variieren!)
anz_prof(nr_q) = nprof



IF (BERECHNUNGSMODUS == 'WATERLEVEL') then
  ! ------------------------------------------------------------------
  ! Erzeugen des Wasserspiegellaengsschnittes (.wsl-file)
  ! ------------------------------------------------------------------
                                                                        
  NAME_OUT_WSL = NAME_OUT_TAB

  ilen = LEN_TRIM(NAME_OUT_WSL)
  NAME_OUT_WSL(ilen-2:ilen) = 'wsl'

  mark = 1

  CALL lapro1 (NAME_OUT_WSL, pfad2, nprof, mark, NAME_OUT_LAENGS)

ENDIF


9999 RETURN
                                                                        

END SUBROUTINE wspber
