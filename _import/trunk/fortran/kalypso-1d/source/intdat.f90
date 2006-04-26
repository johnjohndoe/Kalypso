!     Last change:  WP   26 Apr 2006    1:57 pm
!--------------------------------------------------------------------------
! This code, intda.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - intdat
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
SUBROUTINE intdat (staso, ierl)
!
! geaendert :     W. Ploeger  18.07.2005
!
! Beschreibung
! ------------
! Uebergabe der Daten aus der Eingabemaske
! in die FORTRAN-Programmstruktur
!
! ipro    - Anzahl der von proein max. einlesbaren Wertebloecke
! maxw    - Anzahl der wehrfelder
!
!-----------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN

REAL, INTENT(IN)     :: staso           ! Station [km]
INTEGER, INTENT(OUT) :: ierl            ! Fehlernummer


! COMMON-Block /BRUECK/ ------------------------------------------------------------
INTEGER 	:: iwl, iwr, nuk, nok
INTEGER 	:: iokl, iokr           ! Gelaende Oberkante Grenze
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk, hokmin
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, iokl, iokr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, hokmin, ibridge
! ----------------------------------------------------------------------------------


! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: bete1 (idim2), bete12 (idim2)
COMMON / datlin1 / bete1, bete12
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), npr, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, npr, np
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


! COMMON-Block /FLEXI/ -------------------------------------------------------------
! DK, 21/06/01 - The user-defined value of MEI (for Kouwen procedure)
REAL 		:: mei (maxkla)
COMMON / flexi / mei
! ----------------------------------------------------------------------------------


! COMMON-Block /MAEANDER/ ------------------------------------------
REAL 			:: sm
CHARACTER(LEN=10) 	:: ger_art
COMMON / maeander / sm, ger_art
! ------------------------------------------------------------------


! COMMON-Block /ROUGH/ --------------------------------------------------------
! DK, 21/06/01 - Parameters of bottom roughness type and grass type!
CHARACTER(LEN=4) :: tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
COMMON / rough / tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
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


! COMMON-Block /WEHR2/ --------------------------------------------------------
REAL 		:: beiw (maxw), rkw (maxw), lw (maxw)
CHARACTER(LEN=2):: wart                         ! = 'bk', 'rk', 'bw', 'sk'
COMMON / wehr2 / beiw, rkw, lw, wart
! -----------------------------------------------------------------------------


! -----------------------------------------------------------------------------
! common-block fuer die gewaessergeometrie:
! -----------------------------------------
! variablenbeschreibung
!
! x1     r4(1...maxkla)   x-wert der profilpunkte
! h1     r4(1...maxkla)   hoehen-wert der profilpunkte
! rau    r4(1...maxkla)   rauhigkeitsbeiwert (k-s/k-st/h-g/d-50)
!                            d. profilpkte
! nknot  i4               anzahl profilpunkte
! iprof  a1               kennung zur bezeichnung des profiltyps
!                         iprof = ' ' : normalprofil
!                                 't' : trapezprofil
!                                 'k' : kreisprofil
!                                 'e' : eiprofil
!                                 'm' : maulprofil
! durchm r4               fuer iprof = ' ' : nicht belegt
!                                      't' : breite der sohle
!                                      'k' : durchmesser
!                                      'e' : max. breite
!                                      'm' : max. breite
! hd     r4               fuer iprof = ' ' : nicht belegt
!                                      't' : vertikales lichtmasz
!                                      'k' : nicht belegt
!                                      'e' : vertikales lichtmasz
!                                      'm' : vertikales lichtmasz
! steig  r4               steigung beider profilseiten
!                         nur fuer iprof = 't' belegt
! sohlg  r4               sohlgefaelle
!                         bei normalprofil z.zt. nicht belegt.
! boli   r4               gelaendehoehe an linker vorland/
!                         fluszschlauch-trennflaeche
! bore   r4               gelaendehoehe an rechter vorland/
!                         fluszschlauch-trennflaeche
! hmin   r4               minimale gelaendehoehe des profils
! hmax   r4               maximale gelaendehoehe des profils
! ianf   i4               punkt-nr. der linken vorland/
!                         fluszschlauch-trennflaeche
! iend   i4               punkt-nr. der rechten vorland/
!                         fluszschlauch-trennflaeche
! xanf   r4               x-Wert der linken Grenze d. durchstroemten
!                         Bereiches
! xend   r4               x-Wert der rechten Grenze d. durchstroemte
!                         Bereiches
! itrli  i4               punkt-nr. der linken Trennflaeche (Pasche-
! itrre  i4               punkt-nr. der rechten Trennflaeche(Pasche-
! htrli  r4               Gelaende-Hoehe an der linken Trennflaeche
! htrre  r4               Gelaende-Hoehe an der rechten Trennflaeche
! itr_typ_li Char*2       Index, der Typ der linken Trennflaeche fes
!                         itr_typ_li = 'vo'    Vorland-Trennflaeche
!                         itr_typ_li = 'bo'    Boeschungs-Trennflaec
! itr_typ_re Char*2       Index, der Typ der rechten Trennflaeche fe
!                         itr_typ_re = 'vo'    Vorland-Trennflaeche
!                         itr_typ_re = 'bo'    Boeschungs-Trennflaec
!
!
!
! BEACHTE bei DATENDEFINITION IN PROFIL-DATEI
!
! 1. Die Reihenfolge der Parameterdefinition in der Profildatei ist
!    wie folgt festgelegt:
!
!       1. Falls es sich um ein Sonderprofil handelt, wie Kreis-,
!          Trapez-, Ei- oder Maulprofil, so sind die zugehoerigen
!          Geometrie-Paramter als erstes zu definieren.
!          Hierzu gehoeren (siehe auch Dokumentation zu Programm PRO
!             - 1. Spalte : Bezeichnung des Sonderprofils
!                              TRAP     fuer Trapez
!                              KREI     fuer Kreisprofil
!                              MAUL     fuer Maulprofil
!                              EI       fuer Eiprofil
!             - 2. Spalte : Geometrie Parameter
!                              Durchmesser
!                              Profilhoehe (nur Trapez-, Ei- und Mau
!                              Neig. Boeschung (nur Trapezprofil)
!                              Sohlgefaelle
!               Diese Daten sind nur durch ein Leerzeichen getrennt
!               einander einzugeben.
!             - 3. Spalte : x-Station des tiefsten Profilpunktes
!                           Bei Trapez-Profil Mitte der Sohle
!             - 4. Spalte : geod. Hoehe des tiefsten Profilpunktes
!
!             Bei Sonderprofilen ist in der Profillinie RAUHIGKEIT
!             nur ein Rauhigkeitswert und zwar fuer die Station des
!             tiefsten Sohlpunktes anzugeben.
!             Die Profillinien TRENNFLAECHE und BORDVOLL sollten nic
!             definiert werden, da sie automatisch vom Programm fest
!             gesetzt werden. Andernfalls kann es zu Fehlern fuehren
!             Die Profillinie GELAENDE kann auch bei Sonderprofilen
!             definiert werden. Sie ist jedoch nach der Profillinie,
!             das Sonderprofil beschreibt, in der Profildatei anzuor
!             Die Profillinie GELAENDE stellt bei Sonderprofillinie
!             ueber dem Profil liegende Gelaende dar.
!
!       2. Profillinie GELAENDE (entspricht der Profil-Geometrie)
!
!       3. alle weiteren koennen beliebig sein. Hierzu gehoeren:
!             - Profillinie 'RAUHIGKEIT'
!             - Profillinie 'DP'  (Durchmes. des durchstr. Bewuchses
!             - Profillinie 'AX'  (x-Abstand des durchstr. Bewuchses
!             - Profillinie 'AY'  (y-Abstand des durchstr. Bewuchses
!             - Profillinie 'TRENNFLAECHE'
!             - Profillinie 'BORDVOLL'
!             - Profillinie 'DURCHSTROEMTE'
!             - Profillinie 'UK-BRUECKE'
!             - Profillinie 'OK-BRUECKE'
!
! 2. Bei dem Darcy-Weisbach Fliessgesetz ist die Trennflaeche nicht
!    gleich den Boeschungsknickpunkten zwischen Vorland und Fluszsch
!    D.h. der Bordvolle Abflusz wird nicht mehr automatisch durch di
!    Trennflaeche definiert. Die Trennflaeche im Darcy-Weisbach-Gese
!    kiert die Trennungslinie zwischen dem langsam und dem schnell
!    flieszenden Querschnittsteil. Das entspricht zwar haeufig auch
!    Boeschungsknickpunkten, kann aber z.B. bei Bewuchs auf der Boes
!    innerhalb des Fluszschlauches liegen.
!    Um diesem Sachverhalt Rechnung zu tragen wurde in dem Programm
!    renziert zwischen Trennflaeche und Bordvoll-Punkt. Erstere wird
!    gewohnt in der Profil-Datei ueber die Profillinie 'TRENNFLAECHE
!    finiert. Dabei ist wird der Typ der Trennflaeche wie folgt diff
!    renziert:
!    -  linke Trennflaeche Vorland-Fluszschauch : y(1,jtrenn) = 1
!    -  rechte Trennflaeche Vorland-Fluszschauch: y(2,jtrenn) = 2
!    -  linke Trennflaeche
!         Boeschgs.Bewuchs-freier Fluszquerschn.: y(1,jtrenn) = 3
!    -  rechte Trennflaeche
!         Boeschgs.Bewuchs-freier Fluszquerschn.: y(2,jtrenn) = 4
!
! 3. Optional kann zur Definition der Grenzen des Bordvoll-Bereiches
!    eine weitere Profillinie mit der Bezeichnung 'BORDVOLL' in der
!    datei definiert werden.
!    Dabei wird der linke Boeschungs-Knickpunkt definiert durch die
!    x-Station und die Kennziffer 1. in der y-Spalte
!    Der rechte Boeschungs-Knickpunkt wird durch die x-Station und d
!    Kennziffer 2. in der y-Spalte definiert.
!    Wird keine Profillinie mit der Bezeichnung 'BORDVOLL' definiert
!    wird der Boeschungs-Knickpunkt mit der Lage der Trennflaeche, w
!    in der Profillinie 'TRENNFLAECHE' definiert ist, gleichgesetzt.
!    Die Reihenfolge, in der die Profillinie TRENNFLAECHE und BORDVO
!    zueinander stehen kann beliebig sein.
!
! -----------------------------------------------------------------------------


CHARACTER(LEN=nch80) 	:: dummy
CHARACTER(LEN=nch80) 	:: char (idim)
CHARACTER(LEN=1) 	:: fehler

REAL 			:: xokw1 (maxkla), hokw1 (maxkla)
REAL 			:: feldr (merg)

REAL, PARAMETER         :: toleranz = 1.E-06

INTEGER :: j, i1, i2, istat
INTEGER :: int (idim)
integer :: intprf = 0


! ------------------------------------------------------------------
! PROGRAMMANFANG
! ------------------------------------------------------------------
                                                                        
jrau = 0

jdp = 0
jax = 0
jay = 0

nknot = 0
ianf = 0
iend = 0
iprof = ' '
ibridge = ' '

iwehr = ' '
nwfd = 1
ierl = 0

jmei = 0
xanf = x (1, 1)

xend = x (np (1), 1)

CALL ucase (text33)

istelle = index (text33, 'KM')


IF (istelle.eq.0) then
  istelle = index (text33, 'STATION')

  IF (istelle.eq.0) then

    ! 9000
    PRINT * , 'fehler bei station ', staso
    PRINT * , 'profildatei', text32, ' ohne station definiert'
    PRINT * , 'kontrollieren sie den folgenden text in der '
    PRINT * , 'profildatei: solltext: station km .....'
    PRINT * , 'vorhandener text:     ', text33
    PRINT * , 'programmabbruch.'

    ierl = 1
  ENDIF

  istelle = istelle+7

ELSE

  istelle = istelle+2

ENDIF


dummy = text33 (istelle:20)
i = 1

dummy = ADJUSTL (dummy)

111 continue
IF (dummy (i:i) .eq.'0'.or.dummy (i:i) .eq.'o') then
  dummy (i:i) = ' '
  i = i + 1
  GOTO 111
ELSE
  dummy = ADJUSTL (dummy)
ENDIF

READ (dummy (1:8) , '(f8.3)', IOSTAT=istat) stat
if (istat /= 0) then
  write (*, 9001)
  write (0, 9001)
  9001 format (//1X, 'Fehler beim Ueberpruefen des Stationswertes.', /, &
               & 1X, 'Bitte pruefen Sie den Stationswert in der Profildatei.', /, &
               & 1X, '-> Programmabbruch!')
  call stop_programm(0)
end if

IF (abs (staso - stat) .gt.1.e-3) then
  ! 9002
  PRINT * , 'falsche zuordnung der stationen!'
  PRINT * , 'soll=', stat
  PRINT * , 'ist =', staso, 'in ', text32
  ierl = 2
ENDIF

! Ermitteln durchstroemter bereich:
!JK ---------------------------------

DO 1005 j = 1, npr

  CALL ucase (bete1 (j) )

  IF (bete1 (j) (1:13) .eq.'DURCHSTROEMTE') then

    IF (np (j) .le.1) then
      ! 9003
      PRINT * , 'warnung. durchstroemte zone zwar '
      PRINT * , 'definiert. aber keine grenzen angegeben.'
      PRINT * , 'pruefe profildatei mit station ', staso
      PRINT * , 'es wird der gesamte querschnitt als durch-'
      PRINT * , 'stroemt angenommen.'

      np (j) = 2
      x (1, j) = x (1, 1)
      x (2, j) = x (np (1), 1)
    ENDIF

    IF (np (j) .ne.2) then
      ! 9004
      PRINT * , 'warnung !! --> durchstroemter bereich'
      PRINT * , 'muss durch 2 werte characterisiert werden'
      PRINT * , '(anfang/ende)'
      PRINT * , 'hier : anzahl der werte = ', np (j)
      PRINT * , 'es wird der gesamte querschnitt als durch-'
      PRINT * , 'stroemt angenommen.'

      np (j) = 2
      x (1, j) = x (1, 1)
      x (2, j) = x (np (1), 1)
    ENDIF

    xanf = x (1, j)
    xend = x (2, j)

  !WP 28.02.2005
  !WP Also RAUHEIT is now valid as label for roughness parameters
  ELSEIF ( bete1(j)(1:10).eq.'RAUHIGKEIT' .or. bete1(j)(1:7).eq.'RAUHEIT') then

    jrau = j
    !***********************************************************************
    !     DK, 21/06/01
    !     Defining type of bottom roughness!
    !     There are three possibilities:
    !         1) sandy bed ('k-s ')
    !         2) grassy bed ('h-g ')
    !         3) gravelly bed ('d-50')
    !     Variables:
    !         tbr1 - type of bottom roughness in left floodplain
    !         tbr2 - type of bottom roughness in main channel
    !         tbr3 - type of bottom roughness in right floodplain

    !HB   09.07.2001*******************************************************
    !HB   Damit die Berechnungsverfahren fuer die Vorlaender und den
    !HB   FlussSchlauch aus der PRF_Datei richtig eingelesen werden koennen,
    !HB   muessen die folgenden 3 Zeilen wieder aktiviert werden und die
    !HB   Einstellung nach Darcy-Weissbach (k-s) deaktiviert werden.
    !HB   Dies ist jedoch erst moeglich, wenn die Eingabemaske um entspr.
    !HB   Eingabefelder erweitert wurde, damit die PRF-Datei die Werte dann
    !HB   enthaelt.
    !              tbr1 = bete12(j)(1:4)
    !              tbr2 = bete12(j)(6:9)
    !              tbr3 = bete12(j)(11:14)
    !HB   Setzen der Berechnung nach Darcy-Weissbach (k-s) fuer alle Abschni
    tbr1 = 'k-s '
    tbr2 = 'k-s '
    tbr3 = 'k-s '
    !     Defining type of grass!
    !     There are three possibilities for calculation of MEI:
    !         1) dead grass ('dead')
    !         2) grass in vegetation phase ('veg ')
    !         3) user-defined value ('user')
    !     Variables:
    !         tgr1 - type of grass in left floodplain
    !         tgr2 - type of grass in main channel
    !         tgr3 - type of grass in right floodplain

    !HB   09.07.2001*******************************************************
    !HB   Damit die Beiwerte fuer Gras aus der PRF_Datei richtig eingelesen
    !HB   werden koennen, muessen die folgenden 3 Zeilen wieder aktiviert
    !HB   werden und das Nullsetzen der Variablen deaktiviert werden.
    !HB   Dies ist jedoch erst moeglich, wenn die Eingabemaske um entspr.
    !HB   Eingabefelder erweitert wurde, damit die PRF-Datei die Werte dann
    !HB   enthaelt.
    !              tgr1 = bete12(j)(16:19)
    !              tgr2 = bete12(j)(21:24)
    !              tgr3 = bete12(j)(26:29)
    !HB   Nullsetzen der "Gras-Beiwerte"
    tgr1 = "leer"
    tgr2 = "leer"
    tgr3 = "leer"
    !HB   *****************************************************************

  ELSEIF (bete1 (j) (1:9) .eq.'STIFFNESS') then
    jmei = j
  ELSEIF (bete1 (j) (1:2) .eq.'DP') then
    jdp = j
  ELSEIF (bete1 (j) (1:2) .eq.'AX') then
    jax = j
  ELSEIF (bete1 (j) (1:2) .eq.'AY') then
    jay = j
  ELSEIF (bete1 (j) (1:8) .eq.'GELAENDE') then
    jgel = j

  !**********************************************************************
  !JK  PROGRAMMERWEITERUNG, 23. JUNI 2000, JANA KIEKBUSCH
  !JK  ------------------------------------------------------------------
  !JK
  !JK   EINLESEN SINUOSITAET UND GERINNEART

  ELSEIF (bete1 (j) (1:11) .eq.'SINUOSITAET') then

    CALL ju0chr (bete12 (j), feldr, ianz, CHAR, ichar ,INT, iint, ifehl)

    IF (ianz.lt.2) then

      ianz = 2
      CHAR (1) = 'NEIN'
      feldr (1) = 0.0

    ENDIF

    IF (ifehl.eq.0.and.ianz.ge.1) then

      ger_art = CHAR (1)
      sm = feldr (1)

    ENDIF
    !JK  ENDE PROGRAMMERWEITERUNG
    !**********************************************************************

    !***********************************************************************
    !     DK, 22/06/01 - Input data for meandering option are given below,
    !     until 'SINUOSITAET' data block is given in the input file!
    ger_art = 'gegliedert'
    !      ger_art='kompakt'
    sm = 1.00
    !***********************************************************************

  ENDIF

1005 END DO


IF (intprf.eq.1) then
  npgel = np (jgel)
  xanf = x (1, jgel)
  xend = x (npgel, jgel)
ENDIF                                                                        
                                                                        


!JK   ------------------------------------------------------------------------
!JK   ZUORDNEN DER PROFILWERTE
!JK   ------------------------------------------------------------------------
DO 1000 j = 1, npr

  IF (np (j) .gt.maxkla) then

    ! 9005
    PRINT * , 'fehler an station ', staso
    PRINT * , 'maxkla nicht ausreichend definiert.'
    PRINT * , 'notwendige groesze =', np (j)

    ierl = 3
  ENDIF

  ! zuordnung der werte

  !JK       TRAPEZPROFIL
  !JK       ------------
  IF (bete1 (j) (1:4) .eq.'TRAP') then

    iprof = 't'

    !JK        WAR SCHON DEAKTIVIERT, 01.05.00, JK
    !          do 1010 i1=1,np(j)
    !                x1(i1)=x(i1,j)
    !                h1(i1)=y(i1,j)
    ! 1010     continue

    nknot = 2

    CALL ju0chr (bete12 (j), feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ianz.lt.6) then
      ! 9006
      PRINT * , 'fehler in der variablen-definition des'
      PRINT * , 'Trapezprofils.'
      PRINT * , 'es sollten 6 variablen definiert werden.'
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'Programmabbruch intdat'

      !JK           WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !             print *,'berechnung wird fortgesetzt.'
      !             print *,'Setzte Rohrgefaelle zu Null.'
      !             feldr(4) = 0.0
      !             ianz = 4

    ENDIF

    IF (ifehl.eq.0.and.ianz.ge.4) then
      durchm = feldr (1)
      hd = feldr (2)
      steig = feldr (3)
      sohlg = feldr (4)

      sohlg = sohlg / ( - 1000.)
      x1 (1) = feldr (5)
      h1 (1) = feldr (6)
      np (j) = 1
      hmin = h1 (1)
      isohl = 1
      hmax = h1 (1) + hd
      boli = hmax
      bore = hmax
      hrbv = hmax
      ianf = 1
      iend = nknot
      itrli = 1
      itrre = 2
      htrli = hmax
      htrre = hmax

    ELSEIF (ianz.ne.6) then
      ! 9007
      PRINT * , 'fehler in der variablen-definition des'
      PRINT * , ' trapezprofiles.'
      PRINT * , 'es muessen 6 variablen definiert werden.'
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'berechnung wird fortgesetzt.'
    ELSE
      ! 9008
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'programmabbruch.'

      ierl = 4
    ENDIF


  !JK KREISPROFIL
  !JK -----------
  ELSEIF (bete1 (j) (1:4) .eq.'KREI') then

    iprof = 'k'

    nknot = 2

    CALL ju0chr (bete12 (j), feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ianz.le.3) then
      ! 9009
      write (0,9009) stat
      write (*,9009) stat
      9009 format (//1X, 'Fehler in der Variablen-definition des Kreisprofils.', /, &
                   & 1X, 'Es sollten 4 Variablen definiert werden.', /, &
                   & 1X, 'Bitte pruefen Sie das Profil an Station ', F10.4, /, &
                   & 1X, '-> Programmabruch!')
      call stop_programm(0)

    ENDIF

    IF (ifehl.eq.0 .and. ianz.ge.2) then
      durchm = feldr (1)
      sohlg = feldr (2)

      sohlg = sohlg / ( - 1000.)
      x1 (1) = feldr (3)
      h1 (1) = feldr (4)
      np (j) = 1
      hmin = h1 (1)
      isohl = 1
      hmax = h1 (1) + durchm
      boli = hmax
      bore = hmax
      hrbv = hmax
      ianf = 1
      iend = nknot
      itrli = 1
      itrre = 2
      htrli = hmax
      htrre = hmax
    ELSE
      ! 9010
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'programmabbruch.'
      ierl = 5
    ENDIF

  !JK       MAULPROFIL
  !JK       ----------
  ELSEIF (bete1 (j) (1:4) .eq.'MAUL') then
    iprof = 'm'

    !JK        WAR SCHON DEAKTIVIERT, 01.05.00, JK
    !          do 1025 i1=1,np(j)
    !                x1(i1)=x(i1,j)
    !                h1(i1)=y(i1,j)
    ! 1025     continue

    nknot = 2

    CALL ju0chr (bete12 (j), feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ianz.lt.4) then
      write (*,9011) stat
      write (0,9011) stat
      9011 format (//1X, 'Fehler in der Variablendefinition des Maulprofils.', /, &
                   & 1X, 'Es sollten 4 Variablen definiert werden.', /, &
                   & 1X, 'Bitte pruefen Sie das Profil an Station ', F10.4, /, &
                   & 1X, '-> Programmabruch!')
      call stop_programm(0)
    ENDIF

    IF (ifehl.eq.0.and.ianz.ge.3) then
      durchm = feldr (1)
      hd = feldr (2)
      sohlg = feldr (3)

      sohlg = sohlg / ( - 1000.)
      x1 (1) = feldr (3)
      h1 (1) = feldr (4)
      np (j) = 1
      hmin = h1 (1)
      isohl = 1
      hmax = h1 (1) + hd
      boli = hmax
      bore = hmax
      hrbv = hmax
      ianf = 1
      iend = nknot
      itrli = 1
      itrre = 2
      htrli = hmax
      htrre = hmax

    ELSEIF (ianz.ne.4) then
      ! 9012
      PRINT * , 'fehler in der variablen-definition des'
      PRINT * , ' maulprofiles.'
      PRINT * , 'es muessen 4 variablen definiert werden.'
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'berechnung wird fortgesetzt.'
    ELSE
      ! 9013
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'programmabbruch.'
      ierl = 6
    ENDIF

    IF (abs (hd-0.75 * durchm) .gt.0.01 * durchm) then
      ! 9014
      PRINT * , 'warnung!! querschnittsgeometrie entspricht'
      PRINT * , ' dem normverhaeltnis.'
      PRINT * , 'das verhaeltnis hoehe/breite musz 0.75 sein.'
    ENDIF

  !JK       EIPROFIL
  !JK       --------
  ELSEIF (bete1 (j) (1:2) .eq.'EI') then
    iprof = 'e'

    !JK        WAR SCHON DEAKTIVIERT, 01.05.00, JK
    !          do 1030 i1=1,np(j)
    !                x1(i1)=x(i1,j)
    !                h1(i1)=y(i1,j)
    ! 1030     continue

    nknot = 2

    CALL ju0chr (bete12 (j), feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ianz.lt.5) then
      write (*,9015) stat
      write (0,9015) stat
      9015 format (//1X, 'Fehler in der Variablendefinition des Eiprofils.', /, &
                   & 1X, 'Es sollten 5 Variablen definiert werden.', /, &
                   & 1X, 'Bitte pruefen Sie das Profil an Station ', F10.4, /, &
                   & 1X, '-> Programmabruch!')
      call stop_programm(0)
    ENDIF

    IF (ifehl.eq.0.and.ianz.ge.3) then
      durchm = feldr (1)
      hd = feldr (2)
      sohlg = feldr (3)

      sohlg = sohlg / ( - 1000.)
      x1 (1) = feldr (4)
      h1 (1) = feldr (5)
      np (j) = 1
      hmin = h1 (1)
      isohl = 1
      hmax = h1 (1) + hd
      boli = hmax
      bore = hmax
      hrbv = hmax
      ianf = 1
      iend = nknot
      itrli = 1
      itrre = 2
      htrli = hmax
      htrre = hmax

    ELSEIF (ianz.ne.3) then
      ! 9016
      PRINT * , 'fehler in der variablen-definition des'
      PRINT * , ' eiprofiles.'
      PRINT * , 'es muessen 3 variablen definiert werden.'
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'berechnung wird fortgesetzt.'
    ELSE
      ! 9017
      PRINT * , 'ueberpruefe profil der station', stat
      PRINT * , 'programmabbruch.'
      ierl = 8
    ENDIF

    IF (abs (hd-1.5 * durchm) .gt.0.01 * durchm) then
      ! 9018
      PRINT * , 'Warnung!! Querschnittsgeometrie entspricht'
      PRINT * , ' nicht dem Normverhaeltnis.'
      PRINT * , 'Das Verhaeltnis Hoehe/Breite musz 1.50 sein.'
    ENDIF

  !JK       NORMALPROFIL
  !JK       ------------
  ELSEIF (bete1 (j) (1:8) .eq.'GELAENDE') then
    iprof = ' '
    nknot = 0

    !JK           ZUORDNUNG EINGABEDATEN DEN PUNKTEN
    DO 76 i1 = 1, np (j)
      IF (x (i1, j) .ge.xanf.and.x (i1, j) .le.xend) then

        IF (jrau.gt.0 .and. y(i1,jrau) .eq. 0.) then

          !WP 16.11.2005 neue Fehlermeldung, Programm wird nicht fortgesetzt!
          write (*,90181) stat, i1
          90181 format (/1X, 'Fehler bei der Definition von Rauheiten im Profil!', /, &
                       & 1X, 'Bitte pruefen Sie das Profil an Station ', F10.4,',', /, &
                       & 1X, 'der Fehler ist aufgetreten an Knoten ',I4,'.', /, &
                       & 1X, '-> Programmabruch!')
          !goto 76
          call stop_programm(0)

        end if

        nknot = nknot + 1
        rau (nknot) = y (i1, jrau)

        mei (nknot) = 0.0
        IF (jmei.gt.0) mei (nknot) = y (i1, jmei)

        dp (nknot) = 0.0
        ax (nknot) = 0.0
        ay (nknot) = 0.0

        IF (jdp.gt.0) dp (nknot) = y (i1, jdp)
        IF (jax.gt.0) ax (nknot) = y (i1, jax)
        IF (jay.gt.0) ay (nknot) = y (i1, jay)

        x1 (nknot) = x (i1, j)
        h1 (nknot) = y (i1, j)
      ENDIF
    76 END DO

  !JK       WEITERES
  !JK       --------

  !WP 28.02.2005
  !WP Also RAUHEIT is now valid as label for roughness parameters
  ELSEIF ( bete1(j)(1:10).eq.'RAUHIGKEIT' .or. bete1(j)(1:7).eq.'RAUHEIT') then
  !ELSEIF (bete1 (j) (1:10) .eq.'RAUHIGKEIT') then
    CONTINUE
  ELSEIF (bete1 (j) (1:9) .eq.'STIFFNESS') then
    CONTINUE
  ELSEIF (bete1 (j) (1:13) .eq.'DURCHSTROEMTE') then
    CONTINUE

    !JK       TRENNFLAECHEN BEI NORMALPROFIL
    !JK       ------------------------------
  ELSEIF (bete1 (j) (1:6) .eq.'TRENNF') then
    IF (iprof.eq.' ') then
      itrli = 0
      itrre = 0
      htrli = 0.
      htrre = 0.

      !JK          WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !            ianf = 0
      !            iend = 0

      DO 1071 ii = 1, np (j)
        !JK                   DEFINITION LINKE TRENNFLAECHE
        DO 1070 i = 1, nknot

          IF (abs( x1(i) - x(ii,j) ) .lt. 1.e-06) then

            IF ( y(ii,j) .eq.1. .or. y(ii,j) .eq. 3.) then

              IF ( h1(i) .gt. htrli .or. itrli.eq.0 ) then
                itrli = i
                htrli = h1 (i)
                !JK  TRENNFLAECHENTYP
                IF (y (ii, j) .eq.3.) then
                  !JK  BOESCHUNGSTRENNFLAECHE
                  itr_typ_li = 'bo'
                ELSE
                  !JK  VORLANDTRENNFLAECHE
                  itr_typ_li = 'vo'
                ENDIF

                IF (ianf.eq.0) then
                  ianf = i
                  boli = h1 (i)
                ENDIF
                  !JK  ZU RECHTE TRENNFLAECHE
                GOTO 1073
              ENDIF
            ENDIF
          ENDIF

        1070 END DO


        !JK  DEFINITION RECHTE TRENNFLAECHE
        1073 CONTINUE

        DO 1074 i = nknot, 1, - 1

          IF (abs (x1 (i) - x (ii, j) ) .lt.1.e-06) then

            IF (y (ii, j) .eq.2..or.y (ii, j) .eq.4.) then

              IF (h1 (i) .gt.htrre.or.itrre.eq.0) then
                itrre = i
                htrre = h1 (i)
                !JK  TRENNFLAECHENTYP
                IF (y (ii, j) .eq.4.) then
                  !JK  BOESCHUNGSTRENNFLAECHE
                  itr_typ_re = 'bo'
                ELSE
                  !JK  VORLANDTRENNFLAECHE
                  itr_typ_re = 'vo'
                ENDIF

                IF (iend.eq.0) then
                  iend = i
                  bore = h1 (i)
                ENDIF
                !JK  ZU ENDE TRENNFLAECHEN
                GOTO 1071
              ENDIF
            ENDIF
          ENDIF
        1074 END DO

      1071 END DO

    !JK  ENDIF ZU (iprof.eq.' ')
    ENDIF


  !JK       BORDVOLL
  !JK       --------
  ELSEIF (bete1 (j) (1:6) .eq.'BORDVO') then
    IF (iprof.eq.' ') then
      ianf = 0
      iend = 0

      !JK   DEFINITION GELAENDEHOEHE VORLAND/FLUSSSCHLAUCH-TRENNFL
      DO 2071 ii = 1, np (j)

        DO 2070 i = 1, nknot
          IF (abs (x1 (i) - x (ii, j) ) .lt.1.e-06) then
            !JK  AUF DER LINKEN SEITE
            IF (y (ii, j) .eq.1.) then
              IF (h1 (i) .gt.boli.or.ianf.eq.0) then
                ianf = i
                boli = h1 (i)
              ENDIF
              !JK  AUF DER RECHTEN SEITE
            ELSEIF (y (ii, j) .eq.2.) then
              IF (h1 (i) .gt.bore.or.iend.eq.0) then
                iend = i
                bore = h1 (i)
              ENDIF
            ELSE
              ! 9019
              PRINT * , 'fehler in der trennflaechen-def.'
              PRINT * , '1-4 variablen moeglich !!'
              PRINT * , 'ueberpruefe profil der station', stat
              PRINT * , 'berechnung wird fortgesetzt.'
            ENDIF
          ENDIF
        2070 END DO
      2071 END DO
    !JK  ENDIF ZU (iprof.eq.' ')
    ENDIF

  !JK       UNTERKANTE BRUECKE
  !JK       ------------------
  ELSEIF (bete1 (j) (1:10) .eq.'UK-BRUECKE') then

    CALL ju0chr (bete12 (j), feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ifehl.eq.0.and.ianz.eq.4) then
      hsuw = feldr (1)
      breite = feldr (2)
      raub = feldr (3)
      xk = feldr (4)
    ELSE

      !JK   WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !     print *,'fehler bei brueckendefinition an station: ',stat
      !     if (ifehl.eq.0) then
      !       print *,'- string zur bestimmung von bru.breite und '
      !       print *,'  sohlhoehe im uw ist leer !!!'
      !     else
      !       print *,'anzahl der werte im string : ',ianz
      !     endif

      CONTINUE
    ENDIF

    ibridge = 'b'
    nuk = 0

    !JK  KOORDINATEN UK BRUECKE
    DO i1 = 1, np (j)

      IF (x (i1, j) .ge.xanf.and.x (i1, j) .le.xend) then
        nuk = nuk + 1
        xuk (nuk) = x (i1, j)
        huk (nuk) = y (i1, j)
      ENDIF
    END DO


  !JK       OBERKANTE BRUECKE
  !JK       -----------------
  ELSEIF (bete1 (j) (1:10) .eq.'OK-BRUECKE') then
    nok = 0
    ibridge = 'b'

    !JK           KOORDINATEN OK BRUECKE
    Koordinaten_OK: DO i1 = 1, nknot
      IF (x1 (i1) .eq.x (1, j) .and.h1 (i1) .eq.y (1, j) ) EXIT Koordinaten_OK
      IF (x1 (i1) .le.x (1, j) .and.x1 (i1) .ge.xanf) then
        nok = nok + 1
        xok (nok) = x1 (i1)
        hok (nok) = h1 (i1)
      ENDIF
    END DO Koordinaten_OK


    DO i1 = 1, np (j)
      IF (x (i1, j) .ge.xanf.and.x (i1, j) .le.xend) then
        nok = nok + 1
        xok (nok) = x (i1, j)
        hok (nok) = y (i1, j)
      ENDIF
    END DO

    Knoten_OK: DO i1 = 1, nknot
      IF (x1 (i1) .ge.x (np (j), j) .and.x1 (i1) .le.xend) then
        IF ( x1(i1).eq.x(np(j),j) .and. h1(i1).eq.y(np(j),j) ) CYCLE Knoten_OK
        nok = nok + 1
        xok (nok) = x1 (i1)
        hok (nok) = h1 (i1)
      ENDIF
    END DO Knoten_OK



  ! ------------------------------------------------------------------------------------
  ! OBERKANTE WEHR
  ! ------------------------------------------------------------------------------------
  ELSEIF (bete1 (j) (1:7) .eq.'OK-WEHR') then
    iwehr = 'w'
    CALL ju0chr (bete12 (j), feldr, ianz, char, ichar, int, iint, ifehl)

    IF (ifehl.eq.0) then

      ilen = LEN_TRIM (char (1) )
      CALL lcase (char (1) )

      !JK   ZUORDNUNG WEHRART

      !HW   Scharfkantiges Wehr
      IF (char (1) (1:ilen) .eq.'scharfkantig') then

        wart = 'sk'

      !HW   rundkroniges Wehr
      ELSEIF (char (1) (1:ilen) .eq.'rundkronig') then

        wart = 'rk'

        !JK  WAR SCHON DEAKTIVIERT, 01.05.00, JK
        !**  rkr = feldr(1)

        !HW  Zuordnung des Radius der Wehrkrone zu allen Wehrpu
        DO i1 = 1, ianz
          rkw (i1) = feldr (i1)
        END DO

      !HW   Wehreingabe mit Beiwert
      ELSEIF (char (1) (1:ilen) .eq.'beiwert') then

        wart = 'bw'

        !JK   WAR SCHON DEAKTIVIERT, 01.05.00, JK
        !**   beiw=feldr(1)

        !HW   Zuordnung des Überfallbeiwerts zu allen Wehrpunkte
        DO i1 = 1, ianz
          beiw (i1) = feldr (i1)
        END DO

      !HW              Breitkroniges Wehr
      ELSEIF (char (1) (1:ilen) .eq.'breitkronig') then

        wart = 'bk'

        !JK   WAR SCHON DEAKTIVIERT, 01.05.00, JK
        !**   lw=feldr(1)

        !HW   Zuordnung der Breite der Wehrkrone zu allen Wehrpu
        DO i1 = 1, ianz
          lw (i1) = feldr (i1)
        END DO

      ELSE

        write (*,9020)
        write (0,9020)
        9020 format (//1X, 'Fehler in der Eingabe der Wehrart.')
        call stop_programm(1)
        iwehr = ' '

        write (*,9100)
        write (0,9100)

        GOTO 1000

      ENDIF

      !HW  Oberkante Wehr wird aus der Eingabemaske eingelesen
      nokw = np (j)

      DO i1 = 1, nokw
        xokw (i1) = x (i1, j)
        hokw (i1) = y (i1, j)
      END DO

    !JK  ELSE ZU (ifehl.eq.0)
    ELSE
      write (*,9021)
      write (0,9021)
      9021 format (//1X, 'Fehler in der Eingabe der Wehrparameter!')
      call stop_programm(1)
      iwehr = ' '

      write (*,9100)
      write (0,9100)

      GOTO 1000
    !JK           ENDIF ZU (ifehl.eq.0)
    ENDIF

  !JK       TRENNLINIE (WEHR)
  !JK       ----------------
  ELSEIF (bete1 (j) (1:8) .eq.'TRENNLIN') then

    !HW           Festlegung der Wehrfelderanzahl
    nwfd = nwfd+np (j)

    !HW   Auslesen der Koordinaten der Trennlinien
    DO i1 = 1, nwfd-1
      xtrw (i1) = x (i1, j)
      !HW   z-Koordinate der Trennlinie wird nie genutzt!!!
      htrw (i1) = y (i1, j)
    END DO

  ENDIF                                                                         
                                                                        
1000 END DO
                                                                        


! ------------------------------------------------------------------------------------
! WP 02.02.2006
! Kontrolle der Bewuchsparameter
!
!write (*,9200)
!9200 format (/1X, 'Kontrolle der Eingabe der Bewuchsparameter')

! Globaler Check fuer alle Punkte, ob Dateieingabe konsistent
do i = 1, nknot

  ! Ausgehend von DP
  if (dp(i) > toleranz) then
    if (ax(i) < toleranz .or. ay(i) < toleranz) then
      write (*,9201) staso, i, ax(i), ay(i), dp(i)
      ax(i) = 0.0
      ay(i) = 0.0
      dp(i) = 0.0
    end if

  ! Ausgehend von AX
  else if (ax(i) > toleranz) then
    if (dp(i) < toleranz .or. ay(i) < toleranz) then
      write (*,9201) staso, i, ax(i), ay(i), dp(i)
      ax(i) = 0.0
      ay(i) = 0.0
      dp(i) = 0.0
    end if

  ! Ausgehend von AY
  else if (ay(i) > toleranz) then
    if (ax(i) < toleranz .or. dp(i) < toleranz) then
      write (*,9201) staso, i, ax(i), ay(i), dp(i)
      ax(i) = 0.0
      ay(i) = 0.0
      dp(i) = 0.0
    end if

  end if

end do

9201 format (/1X, 'Warnung! In Profil km ', F10.4, ' sind die Bewuchsparameter', /, &
            & 1X, 'nicht richtig gesetzt! Fehler tritt bei Punkt ', I5, ' auf.', /, &
            & 1X, '(AX = ', F10.4, ' AY = ', F10.4, ' DP = ', F10.4, ')', /, &
            & 1X, '-> Loesche Bewuchsparameter fuer diesen Punkt! Bitte Pruefen!' )


! Lokaler Check der Dateneingabe fuer den Flussschlauch
do i = itrli, itrre-1

  if (ax(i) > toleranz .or. ay(i) > toleranz .or. dp(i) > toleranz) then
    write (*,9202) staso, i, ax(i), ay(i), dp(i)
    9202 format (/1X, 'Warnung! In Profil km ', F10.4, ' sind zwischen den', /, &
                & 1X, 'Trennflaechen bei Punkt ', I5, ' Bewuchsparameter gesetzt!', /, &
                & 1X, '(AX = ', F10.4, ' AY = ', F10.4, ' DP = ', F10.4, ')', /, &
                & 1X, '-> Loesche Bewuchsparameter fuer diesen Punkt! Bitte Pruefen!' )
    ax(i) = 0.0
    ay(i) = 0.0
    dp(i) = 0.0

  end if

end do
!
! Ende Kontrolle Bewuchsparameter
! ------------------------------------------------------------------------------------




! ------------------------------------------------------------------------------------
! Kontrolle der Wehrlinien
! ------------------------------------------------------------------------------------
                                                                        
      IF (iwehr.eq.'w') then 
        ianf_w = 0 
        iend_w = 0 
                                                                        
        !JK        BESTIMMUNG ANFANGSPUNKT OK WEHR
        Wehr_anf_p: DO j = 1, nokw
          !HW        der Anfangspunkt des Wehres muss mit der Trennfläche links zu
          IF (xokw (j) .eq.x1 (ianf) ) then 
            IF (hokw (j) .eq.h1 (ianf) ) then 
              ianf_w = j 
              EXIT Wehr_anf_p
            ENDIF 
          ENDIF 
        END DO Wehr_anf_p
                                                                        
        !JK        BESTIMMUNG ENDPUNKT OK WEHR
        Wehr_ende_p: DO j = nokw, 1, - 1
          !HW        der Endpunkt des Wehres muss mit der Trennfläche rechts zusam
          IF (xokw (j) .eq.x1 (iend) ) then 
            IF (hokw (j) .eq.h1 (iend) ) then 
              iend_w = j 
              EXIT Wehr_ende_p
            ENDIF 
          ENDIF
        END DO Wehr_ende_p
                                                                        
        IF (ianf_w.eq.0.or.iend_w.eq.0) then
          write (*,9022)
          write (0,9022)
          9022 format (//1X, 'Fehlerhafte Bestimmung der Wehrlinie!', /, &
                       & 1X, 'Trennflaechen muessen mit Wehranfangspunkt', /, &
                       & 1X, 'und -endpunkt uebereinstimmen!')
          call stop_programm(1)

          iwehr = ' '

          write (*,9100)
          write (0,9100)

          GOTO 532 
                                                                        
        ELSE 
          !HW            das Wehr erstreckt sich über das ganze Profil
          IF (ianf_w.eq.1) then 
            IF (iend_w.eq.nokw) goto 530 
          ENDIF 
                                                                        
          !HW            Umnummerieren der Wehrpunkte
                                                                        
          j1 = 0 
                                                                        
          DO j = ianf_w, iend_w
                                                                        
            j1 = j1 + 1 
            xokw1 (j1) = xokw (j) 
            hokw1 (j1) = hokw (j) 
                                                                        
          END DO
                                                                        
          nokw = j1 
                                                                        
          !HW            Änderung in der folgenden Schleife xokw(j1)->xokw(j),hokw
                                                                        
          DO j = 1, nokw
                                                                        
            xokw (j) = xokw1 (j) 
            hokw (j) = hokw1 (j) 
                                                                        
          END DO
                                                                        
        ENDIF 
                                                                        
        !HW        Kontrolle der Wehrfeldanzahl
                                                                        
        530 CONTINUE

        IF (nwfd.eq.1) then
          !HW        Nur ein Wehrfeld -> Keine Trennlinien nötig!
          ianfw (1) = 1 
          iendw (1) = nokw 
                                                                        
        ELSE 
          !HW        Mehrere Felder
          DO 531 j = 1, nwfd-1 
                                                                        
            n_fehl = 1 
                                                                        
            DO j1 = 1, nokw
                                                                        
              !HW                    Feststellung der Lage der Trennlinie im Wehr
              IF (xokw (j1) .eq.xtrw (j) ) then 
                n_fehl = 0 
                iendw (j) = j1 
                ianfw (j + 1) = j1 
                GOTO 531 
                                                                        
              ENDIF 

            END DO
                                                                        
            IF (n_fehl.eq.1) then 

              write (*,9023)
              write (0,9023)
              9023 format (//1X, 'Fehlerhafte Bestimmung der Trennlinien im Wehr!')
              call stop_programm(2)

              iwehr = ' '

              write (*,9100)
              write (0,9100)

              GOTO 532 
            ENDIF 
                                                                        
          531 END DO
                                                                        
          iendw (nwfd) = nokw 
          ianfw (1) = 1 
                                                                        
        ENDIF 
                                                                        
      !JK    ENDIF ZU (iwehr.eq.'w')
      ENDIF 
                                                                        

      ! -------------------------------------------------------------------
      ! Bestimmen d. niedrigsten Gelaendepktes im Fluszschlauch (beim Gerin
      ! oder des ueber dem Kanal liegenden Gelaendes
      ! Groesze dient zur Unterscheidung, ob Kanal oder Gerinne
      ! -------------------------------------------------------------------
                                                                        
      532 CONTINUE

      hming = 1.e+06 
      IF (iprof.eq.' ') then 
        istart = ianf 
        iend = iend 
      ELSE 
        istart = 1 
        iend = np (jgel) 
      ENDIF 
                                                                        
      DO i1 = istart, iend
        IF (i1.gt.0.AND.jgel.gt.0) then 
          IF (hming.gt.y (i1, jgel) ) then 
            hming = y (i1, jgel) 
            iming = i1 
          ENDIF 
        ENDIF 
      END DO
                                                                        
      !JK   AN DIESER STELLE IM ALTEN QUELLCODE:
      !JK   BESTIMMUNG DES TIEFSTEN PUNKTES IM WEHRFELD
      IF (iwehr.eq.'w') then 
        hokwmin = 1.e+06 
                                                                        
        DO i1 = 1, nokw
          IF (hokw (i1) .lt.hokwmin) then 
            hokwmin = hokw (i1) 
            iwmin = i1 
          ENDIF 
        END DO
      ENDIF 
                                                                        



      ! ******************************************************************
      ! Bestimmen des linken und rechten widerlagers fuer die bruecke:
      ! -  Der punkt vor dem ersten punkt der nicht mehr auf der gelaende-
      !    linie liegt, ist der linke widerlagerpunkt (rechts ebenso)
      ! -  bestimmen der pfeilerpunkte,hukmax,hokmax
      ! ******************************************************************
                                                                        
      gena = 0.001 
                                                                        
      IF (ibridge.eq.'b') then 

        !WP 12.01.2005
        !write (*,*) 'Jetzt bei Kontrolle der Brueckenwiderlager...'
        !write (*,*) 'NKNOT: ', nknot

        ikenn = 0 
        iwr = 0 
        iwl = 0 
                                                                        
        !JK LINKES WIDERLAGER
        DO 83 i2 = 1, nknot 

          IF (rau (i2) .le. 0.001) goto 83
          p0 = x1 (i2) 
          y0 = h1 (i2)
          !WP 12.01.05
          !write (*,*) 'I2:', i2, '  X1(i2): ', p0, ',   H1(i2): ', y0
                                                                        
          DO 82 i1 = 1, nuk - 1 
            p1 = xuk (i1) 
            p2 = xuk (i1 + 1) 
            !write (*,*) 'I1:', i1, '  XUK(i1): ', p1, ',   XUK(i1+1): ', p2

            IF (p0.lt.p1.or.p0.gt.p2) goto 82 
            y1 = huk (i1) 
            y2 = huk (i1 + 1) 
                                                                        
            IF (p0.eq.p1.and.y0.eq.y1) then 
              ikenn = 1 
              GOTO 83 
            ELSEIF (p0.eq.p2.and.y0.eq.y2) then 
              ikenn = 1 
              GOTO 83 
            ELSE 
              a = ( (p1 - p2) * (y1 + y2) + (p2 - p0) * (y2 + y0)       &
              + (p0 - p1) * (y0 + y1) ) / 2.                            
                                                                        
              IF (abs (a) .lt.gena) then 
                ! punkt liegt auf der profillinie
                ! --> untersuchen naechster profilpunkt
                ikenn = 1 
                GOTO 83 
              ENDIF 
            ENDIF 
          82 END DO
                                                                        
          IF (ikenn.ne.0) then 
            iwl = i2 - 1 
            GOTO 84 
          ENDIF 
                                                                        
        83 END DO
                                                                        
        84   ikenn = 0
                                                                        
        !JK       RECHTES WIDERLAGER
        DO 85 i2 = nknot, iwl, - 1 

          IF (rau (i2) .le.0.001) goto 85 
          p0 = x1 (i2) 
          y0 = h1 (i2) 
                                                                        
          DO 86 i1 = nuk, 1, - 1 
            p1 = xuk (i1) 
            p2 = xuk (i1 + 1) 
            IF (p0.lt.p1.or.p0.gt.p2) goto 86 
            y1 = huk (i1) 
            y2 = huk (i1 + 1) 
                                                                        
            IF (p0.eq.p1.and.y0.eq.y1) then 
              ikenn = 1 
              GOTO 85 
            ELSEIF (p0.eq.p2.and.y0.eq.y2) then 
              ikenn = 1 
              GOTO 85 
            ELSE 
              a = ( (p1 - p2) * (y1 + y2) + (p2 - p0) * (y2 + y0)       &
                & + (p0 - p1) * (y0 + y1) ) / 2.
                                                                        
              IF (abs (a) .lt.gena) then 
                IF (p0.ge.p1.and.p0.le.p1) then 
                  ! Punkt liegt auf der profillinie
                  ! --> untersuchen naechster profilpunkt
                  ikenn = 1 
                  GOTO 85 
                ENDIF 
              ENDIF 
            ENDIF 
          86 END DO
                                                                        
          IF (ikenn.ne.0) then 
            iwr = i2 + 1 
            GOTO 87 
          ENDIF 

        85 END DO
                                                                        
        !JK       EINSORTIEREN WIDERLAGERPUNKT
        87 continue
        IF (iwr.eq.0.or.iwl.eq.0) then

          write (*,9024) iwl, iwr
          write (0,9024) iwl, iwr
          9024 format (//1X, 'Fehlerhafte Bestimmung der Brueckenwiderlager in INTDAT!', /, &
                       & 1X, '  IWL = ', I4, /, &
                       & 1X, '  IWR = ', I4)
          call stop_programm(1)

          ierl = 9
        ENDIF 
                                                                        
        hukmax = - 1.e+12 
                                                                        
        DO 93 i1 = 1, nuk 
          IF ( (xuk (i1) - x1 (iwl) ) .lt.0.) goto 93 
          IF ( (xuk (i1) - x1 (iwr) ) .gt.0.) goto 93 
          IF (hukmax.lt.huk (i1) ) hukmax = huk (i1) 
        93 END DO
        ! widerlagerpunkte in oberkante einsortieren:
        nokn = nok 



        88 continue
        nok = nokn
                                                                        

        !JK       BESTIMMUNG DER AUSSENPUNKT OK BRUECKE
        !JK       -------------------------------------
        iokr = 0 
        iokl = 0 
                                                                        
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ikenn = 0 
        ! ???????????????????????????????????
                                                                        
        !JK       LINKER PUNKT OK BRUECKE
        DO 283 i2 = 1, nknot 
          IF (rau (i2) .le.0.001) goto 283 
          p0 = x1 (i2) 
          y0 = h1 (i2) 
                                                                        
          DO 282 i1 = 1, nok - 1 
            p1 = xok (i1) 
            p2 = xok (i1 + 1) 
            IF (p0.lt.p1.or.p0.gt.p2) goto 282 
            y1 = hok (i1) 
            y2 = hok (i1 + 1) 
                                                                        
            IF (p0.eq.p1.and.y0.eq.y1) then 
              ikenn = 1 
              GOTO 283 
            ELSEIF (p0.eq.p2.and.y0.eq.y2) then 
              ikenn = 1 
              GOTO 283 
            ELSE 
              a = ( (p1 - p2) * (y1 + y2) + (p2 - p0) * (y2 + y0)       &
              + (p0 - p1) * (y0 + y1) ) / 2.                            
                                                                        
              IF (abs (a) .lt.gena) then 
                ! Punkt liegt auf der profillinie
                ! --> untersuchen naechster profilpunkt
                ikenn = 1 
                GOTO 283 
              ENDIF 
            ENDIF 
          282 END DO
                                                                        
          IF (ikenn.ne.0) then 
            iokl = i2 - 1 
            GOTO 284 
          ENDIF 
                                                                        
        283 END DO
                                                                        
        284 continue
        ikenn = 0
                                                                        
        !JK       RECHTER PUNKT OK BRUECKE
        DO 285 i2 = nknot, iokl, - 1 
          IF (i2.gt.0) then 
            IF (rau (i2) .le.0.001) goto 285 
            p0 = x1 (i2) 
            y0 = h1 (i2) 
                                                                        
            DO 286 i1 = nok, 1, - 1 
              p1 = xok (i1) 
              p2 = xok (i1 + 1) 
              IF (p0.lt.p1.or.p0.gt.p2) goto 286 
              y1 = hok (i1) 
              y2 = hok (i1 + 1) 
                                                                        
              IF (p0.eq.p1.and.y0.eq.y1) then 
                ikenn = 1 
                GOTO 285 
              ELSEIF (p0.eq.p2.and.y0.eq.y2) then 
                ikenn = 1 
                GOTO 285 
              ELSE 
                a = ( (p1 - p2) * (y1 + y2) + (p2 - p0) * (y2 + y0)     &
                + (p0 - p1) * (y0 + y1) ) / 2.                          
                                                                        
                IF (abs (a) .lt.gena) then 
                  IF (p0.ge.p1.and.p0.le.p1) then 
                    ! Punkt liegt auf der profillinie
                    ! --> untersuchen naechster profilpunkt
                    ikenn = 1 
                    GOTO 285 
                  ENDIF 
                ENDIF 
              ENDIF 
            286 END DO
                                                                        
            IF (ikenn.ne.0) then 
              iokr = i2 + 1 
              GOTO 287 
            ENDIF 
          ENDIF 
        285 END DO
                                                                        
        287 continue
        IF (iokr.eq.0.or.iokl.eq.0) then

          write (*,9025) iokl, iokr
          write (0,9025) iokl, iokr
          9025 format (//1X, 'Fehlerhafte Bestimmung der Brueckenoberkante in INTDAT!', /, &
                       & 1X, '  IOKL = ', I4, /, &
                       & 1X, '  IOKR = ', I4)
          call stop_programm(1)

          ierl = 9
        ENDIF 

        !JK       EINSORTIEREN PUNKT OK BRUECKE
        DO 98 i1 = 1, nok 
          IF (xok (i1) .eq.x1 (iwl) .and.hok (i1) .eq.h1 (iwl) ) goto 98
          IF (xok (i1) .eq.x1 (iwr) .and.hok (i1) .eq.h1 (iwr) ) goto 98
                                                                        
          IF (xok (i1) .lt.x1 (iwl) .and.xok (i1 + 1) .gt.x1 (iwl) ) then
            ! einsortieren
            hges = (hok (i1) - hok (i1 + 1) ) / (xok (i1) - xok (i1 + 1) ) &
                     & * (x1 (iwl) - xok (i1) )
            hges = hok (i1) + hges 
            nokn = nokn + 1 
                                                                        
            DO i2 = nokn, i1 + 1, - 1
              xok (i2) = xok (i2 - 1) 
              hok (i2) = hok (i2 - 1) 
            END DO
                                                                        
            xok (i1 + 1) = x1 (iwl) 
            hok (i1 + 1) = hges 
            GOTO 88 
                                                                        
          ELSEIF (xok (i1) .lt.x1 (iwr) .and.xok (i1 + 1) .gt.x1 (iwr) ) then
            ! einsortieren
            hges = (hok (i1) - hok (i1 + 1) ) / (xok (i1) - xok (i1 + 1) ) &
                     & * (x1 (iwr) - xok (i1) )
            hges = hok (i1) + hges 
            nokn = nokn + 1 
                                                                        
            DO i2 = nokn, i1 + 1, - 1
              xok (i2) = xok (i2 - 1) 
              hok (i2) = hok (i2 - 1) 
            END DO
                                                                        
            xok (i1 + 1) = x1 (iwr) 
            hok (i1 + 1) = hges 
            GOTO 88 
          ELSE 
            CONTINUE 
          ENDIF 
        98 END DO
                                                                        
        !JK       ERMITTLUNG MIN/MAX HOEHE OK BRUECKE
        !JK       -----------------------------------
        hokmin = 1.e+12 
        hokmax = - 1.e+12 
                                                                        
        DO 94 i1 = 1, nok 
          IF ( (xok (i1) - x1 (iwl) ) .lt.0.) goto 94 
          IF ( (xok (i1) - x1 (iwr) ) .gt.0.) goto 94 
          IF (hokmin.gt.hok (i1) ) hokmin = hok (i1) 
          IF (hokmax.lt.hok (i1) ) hokmax = hok (i1) 
        94   END DO
                                                                        
        IF (hokmin.eq.1.e+12.or.hokmax.eq. - 1.e+12) then 

          write (*,9026) hokmin, hokmax
          write (0,9026) hokmin, hokmax
          9026 format (//1X, 'Fehlerhafte Bestimmung der Brueckenoberkante in INTDAT!', /, &
                       & 1X, '  HOKMIN = ', F10.3, /, &
                       & 1X, '  HOKMAX = ', F10.3)
          call stop_programm(0)

        ENDIF
      !JK   ENDIF ZU (ibridge.eq.'b')
      ENDIF 
                                                                        
      !     festlegen minimale gelaendehoehe, maximale gelaendehoehe
      !JK   --------------------------------------------------------
      IF (iprof.eq.' ') then 
        hmin = 1.e+12 
        hmax = - 1.e+12 
                                                                        
        !JK       ERMITTLUNG MIN/MAX GELAENDEHOEHE
        DO 102 i1 = 1, nknot 
          !JKV        ABFRAGE AUS PASCHE-VERSION
          IF (ianf.gt.0) then 
            !JKV        ENDE
            IF (x1 (i1) .lt.x1 (ianf) ) goto 102 
          !JKV        ABFRAGE AUS PASCHE-VERSION
          ENDIF 
          !JKV        ENDE
          !JKV        ABFRAGE AUS PASCHE-VERSION
          IF (iend.gt.0) then 
            !JKV        ENDE
            IF (x1 (i1) .gt.x1 (iend) ) goto 102 
            !JKV        ABFRAGE AUS PASCHE-VERSION
          ENDIF 
          !JKV        ENDE
          IF (hmin.gt.h1 (i1) ) then 
            hmin = h1 (i1) 
            isohl = i1 
            !JKV              ZUSATZ AUS PASCHE-VERSION
            hming = hmin 
            iming = isohl 
            !JKV              ENDE
          ENDIF 
          IF (hmax.lt.h1 (i1) ) hmax = h1 (i1) 
        102 END DO
                                                                        
        !JK       ERMITTLUNG WSP BORDVOLL
        IF ( (boli.gt.1.e-06.and.bore.gt.1.e-06) ) then 
          hrbv = MIN (boli, bore)
        ELSEIF (boli.gt.1.e-06) then 
          hrbv = boli 
        ELSEIF (bore.gt.1.e-06) then 
          hrbv = bore 
        ELSE 
          hrbv = hmax 
        ENDIF 
                                                                        
        IF (itrli.eq.0.or.itrre.eq.0) then 
          ! 9027
          PRINT * , 'fehler in der trennflaechendefinition.' 
          PRINT * , 'ueberpruefe die datei ', text32 
          PRINT * , 'die x-werte der trennflaechen haben keinen' 
          PRINT * , 'korrespondierenden x-wert in der profilgeo' 
          PRINT * , 'metrie.' 
          PRINT * , 'die berechnung wird fortgesetzt mit ' 
          PRINT * , 'default-trennflaechen.' 
                                                                        
          htrli = h1 (1) 
                                                                        
          IF (nknot.gt.0) then 
            htrre = h1 (nknot) 
          ENDIF 
                                                                        
          itrli = 1 
          itrre = nknot 
        ENDIF 
                                                                        
      ENDIF 
                                                                        
      !JK   FEHLERMELDUNG + DATENUEBERGABE
      !JK   ------------------------------
                                                                        
      !WP 18.05.2005 IF (jrau.le.0.and.jraueintrag.ne.1) then
      !WP Löschen von jraueintrag, da nie definiert!
      IF (jrau.le.0) then
        ! 9028
        PRINT * , 'keine rauhigkeitswerte im profil definiert.' 
        PRINT * , 'ueberpruefe profil an station', staso 
      ENDIF 
                                                                        
      IF (nknot.le.0) then 
        ! 9029
        PRINT * , 'keine gelaendehoehen im profil definiert.' 
        PRINT * , 'ueberpruefe profil an station', staso 
      ENDIF 
                                                                        
      9100 format (/1X, '->Profil wird ohne Wehr berechnet!')

      END SUBROUTINE intdat
