!     Last change:  WP    5 Nov 2007   11:46 am
!--------------------------------------------------------------------------
! This code, wsp.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Copyright (C) 2004 - 2007 ULF TESCHKE & WOLF PLOEGER.
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
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! See our web page: www.tuhh.de/wb
!
! or
!
! EPK-Engineers, Kasernenstr. 12, 21073 Hamburg, Germany
! Wolf Ploeger 
! wolf.ploeger@epk2.de
! See our web page: www.epk2.de
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 12 November 2007
!--------------------------------------------------------------------------




!--------------------------------------------------------------------------
PROGRAM WSP
!
! geschrieben: P. Koch, Maerz 1990
! geaendert: W. Ploeger, 2004-2007
!
! Programmbeschreibung:
! ---------------------
! Dieses Programm steuert die Bildschirmeingabe der fuer die Wasser-
! spiegellagenberechnung notwendigen Angaben. Im einzelnen werden
! angegeben:
! - Art der Berechnung : bordvoll oder normale Wsp-Berechnung
! - Auswahl des zu berechnenden Abflussereignisses
! - Auswahl des Berechnungsabschnittes
! - Art der Berechnung des Anfangswasserspiegels
! - Auswahl des Ausgabeumfangs
! - Auswahl des 'Verzoegerungsbeiwertes'
!
! Das Steuerprogramm ruft dann das Hauptprogramm WSPBER auf.
!
! VERWENDETE PARAMETER
! --------------------
! ipro     - MAXIMALE ANZAHL DATENSAETZE IM PROFIL
! pfad_aus - Pfad ?:\Projekt\Projektname\dath\variaaus.AUS
! pfad_les - Pfad ?:\Projekt\Projektname\dath\WSPvaria.EIN
! pfad_ver - Pfad ?:\Projekt\Projektname\dath\verlauf1.WSP
! proj_les - Pfad ?:\Projekt\Projektname\dath\
!
! *****************************************************************
! 26.11.2001 - H.Broeker
! ----------------------
! alph_aus - Pfad ?:\Projekt\Projektname\dath\Beiwerte.AUS
! i_alph   - Funktionswert
! *****************************************************************
!
! AUFGERUFENE UNTERPROGRAMME
! --------------------------
! ju0gfu ()
! lcase (antwort)
! wspber (unit1,ibruecke,wehr,imeth,antwort)
! qbordv ()
!
!--------------------------------------------------------------------------
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE ZEIT
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


! COMMON-Block /AUSGABELAMBDA/ -----------------------------------------------------
REAL, DIMENSION(maxkla) :: lambda_teilflaeche
CHARACTER(LEN=nch80) 	:: lambdai
COMMON / ausgabelambda / lambda_teilflaeche, lambdai
! ----------------------------------------------------------------------------------


! COMMON-Block /BV/ ----------------------------------------------------------------
CHARACTER(LEN=1):: idr1, idr2
INTEGER         :: nprof, isch
REAL            :: qvar
COMMON / bv / idr1, idr2, nprof, qvar, isch
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), npr, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, npr, np
! ----------------------------------------------------------------------------------


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


! COMMON-Block /NR_AUS/ --------------------------------------------------------
! Commonblock fuer Profilnummerausgabe im *.wsl -File
CHARACTER(LEN = 1) :: nr_ausg
COMMON / nr_aus / nr_ausg
! -----------------------------------------------------------------------------


! COMMON-Block /OB_ALPHA/ -----------------------------------------------------
! Uebergabe eines Steuerparameters, ob die Datei Beiwerte.AUS
! erzeugt und in sie geschrieben werden soll.
INTEGER 	:: alpha_ja
COMMON / ob_alpha / alpha_ja
! -----------------------------------------------------------------------------


! COMMON-Block /P11/ ----------------------------------------------------------
REAL 		:: wsanf, qstat (merg), qwert (merg), staanf, staend
INTEGER 	:: nq
COMMON / p11 / wsanf, nq, qstat, qwert, staanf, staend
! -----------------------------------------------------------------------------


! COMMON-Block /PSIWERT/ ------------------------------------------------------
! ENTHAELT VARIABLEN FUER OERTLICHE VERLUSTE
REAL 		:: psistat (merg), psiein (merg), psiort (merg)
INTEGER 	:: jpsi
COMMON / psiwert / psistat, psiein, psiort, jpsi
! -----------------------------------------------------------------------------


! COMMON-Block /TEILDAT/ ------------------------------------------------------
REAL 		:: anftg (merg)
INTEGER 	:: numitg (merg), ikg
COMMON / teildat / anftg, numitg, ikg
! -----------------------------------------------------------------------------


! COMMON-Block /TEILGEBIET/ ---------------------------------------------------
INTEGER         :: ikitg, itg
COMMON / teilgebiet / ikitg, itg
! -----------------------------------------------------------------------------


! Local variables -------------------------------------------------------------
INTEGER :: i, j, i1, ianz, jjanz, ifehl, iint, mode, ireal, ichara
INTEGER :: iq, iqelen, iflulen

INTEGER :: istat                ! Check in IOSTAT-clause while opening files
INTEGER :: lein

INTEGER :: ilen, ilen2

INTEGER :: anq (merg), int (maxkla)
INTEGER :: zint (merg)
INTEGER :: imp_ber
! imp_ber = 0 Bernoulli - Gleichung
! imp_ber = 1 Berechnung mit Impulsbilanz
INTEGER :: ifg
! ifg = 0  Manning-Strickler
! ifg = 1  Darcy-Weisbach
INTEGER :: a_m
INTEGER :: nr
INTEGER :: rg_vst

INTEGER :: ju0gfu

!HB   Projektpfade fuer WSPvaria.ein und variaaus.AUS, Verlauf1.WSP     
CHARACTER(LEN=nch80) :: proj_les, pfad_les, pfad_aus, pfad_ver, zchar (merg)
CHARACTER(LEN=nch80) :: unit6, unit4, unit1, unit13, string, nproj
CHARACTER(LEN=nch80) :: aereignis (merg), char (maxkla), dummy, unit7
CHARACTER(LEN=nch80) :: ereign (merg)
CHARACTER(LEN=nch80) :: dfluss, unit2
CHARACTER(LEN=nch80) :: FALLNAME
CHARACTER(LEN=nch80) :: pfadconfigdatei
CHARACTER(LEN=1)     :: ibruecke, wehr
CHARACTER(LEN=1)     :: antwort

REAL :: zreal (merg)
REAL :: aqwert (maxkla, merg), aqstat (maxkla, merg), awsanf (merg)
REAL :: feldr (maxkla)

!UT   FUER SUB WHEN                                                     
CHARACTER(LEN=11) :: WSPSTART
CHARACTER(LEN=8)  :: DATSTART


!HB   ***************************************************************** 
!HB   26.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB   Projektpfad und Nr. fuer Beiwerte.AUS,                            
!HB   Uebergabe an SUB ALPHA_DRU und SUB WSP                            
INTEGER :: i_alph
!HB   *****************************************************************

LOGICAL :: lexist, lopen
                                                                        

! ------------------------------------------------------------------
! Initialisierungen
! ------------------------------------------------------------------
idr1 = 'n'              ! Sollen WQ-Dateien fuer jedes Profil erstellt werden?
idr2 = 'n'              ! Sollen Ergebnislisten erstellt werden?
km = 'n'                ! Sollen die KALININ-MILJUKOV-Parameter bestimmt werden?

imp_ber = 0             ! Berechnung nach Bernoulli-Gleichung
                                                                        
nr = 0

alpha_ja = 1            ! Soll Datei Beiwerte.aus erzeugt werden? (0=nein, 1=ja)

DURCHFLUSS_EINHEIT = 'M'! Einheit der Ausgabe des Durchflusses ("M"=m3/s, "L"=l/s)

! ------------------------------------------------------------------
! AUFRUF DER SUB-ROUTINE WHEN ZUM ABFRAGEN DER AKTUELLEN UHRZEIT
! ------------------------------------------------------------------
                                                                        
CALL WHEN (MMTTJJ, HHMM, HHMMSS, HHMMSSHH)
                                                                        
!UT   SPEICHERN DER STARTUHRZEIT                                        
WSPSTART = HHMMSSHH
DATSTART = MMTTJJ



! ------------------------------------------------------------------
! Berechnungen
! ------------------------------------------------------------------

WRITE ( *, 1000) VERSIONNR, VERSIONDATE, HHMM, MMTTJJ
1000 FORMAT (////, &
     & 1x, '**********************************************************',/,  &
     & 1x, '**********************************************************',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****       TU HAMBURG-HARBURG, AB WASSERBAU           ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****             K A L Y P S O - 1 D                  ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****             SPIEGELLINIENBERECHNUNG              ****',/,  &
     & 1x, '****          IN GERINNEN UND ROHRLEITUNGEN           ****',/,  &
     & 1x, '****     FUER STATIONAER-UNGLEICHFOERMIGEN ABFLUSS    ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****',        A29,            3X,          A17,     ' ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '****                                                  ****',/,  &
     & 1x, '**********************************************************',/,  &
     & 1x, '**********************************************************',/,  &
     & 1x, /, &
     & 1x, /, &
     & 1x, /, &
     & 1x, 'DAS PROGRAMM WIRD STAENDIG WEITER ENTWICKELT!', /, &
     & 1x, /, &
     & 1x, 'ES IST JETZT ', A5, ' UHR AM ', A8, '. ', /, &
     & 1x, /, &
     & 1x, 'BITTE PRUEFEN SIE DIE GERADE BENUTZTE VERSIONSNUMMER!')



! ---------------------------------------------------------------------------------
! 1. Zeile in BAT.001
! -------------------
! WP 18.05.2005, ab Version 1.0.8
! Aenderung bei der Maskeneingabe! Es wird angenommen, dass
! die Eingabe IMMER durch eine Maske (was auch immer das heißt)
! durchgefuehrt wird. Damit gilt dann immer ANTWORT = 'J'!
! Die Subroutine SELECT wird nie wieder aufgerufen und damit komplett
! geloescht!
write (*, 1001)
1001 format (//1X, 'Organisierung der Eingabedaten durch Maskeneingabe ?')
READ ( * , '(a)') antwort
write (*, *) antwort

CALL lcase (antwort)
if (antwort == 'n') then
  RUN_MODUS = 'KALYPSO'
  read (*,*) pfadconfigdatei
  call read_config_file(ADJUSTL(pfadconfigdatei),LEN_TRIM(pfadconfigdatei))

  !MD  if (BERECHNUNGSMODUS == 'BF_NON_UNI') then
  !MD  neue Berechnungsvarainte mit konstanten Reibungsgefaelle
  IF (BERECHNUNGSMODUS == 'BF_NON_UNI' .or. BERECHNUNGSMODUS == 'REIB_KONST') then
    km   = 'j'  !MD  Kalinin - Miljukov - Parameter werden erstellt (Deaktivieren??)
    idr1 = 'j'  !MD  WQ-Dateien sollen erstellt werden
    idr2 = 'j'  !MD  Ergebnislisten sollen erstellt werden
  end if

else
  RUN_MODUS = 'WSPWIN '
end if


! ---------------------------------------------------------------------------------
! 2. Zeile in BAT.001
! -------------------
! EINLESEN PFADNAME DES PROJEKTES
if (RUN_MODUS /='KALYPSO') then
  write (*,1002)
  1002 format (/1X, 'Gib Pfadname des Projektes -->')
  READ ( * , '(a)') nproj
  !write (*,*) nproj
  !WP New configuration in MOD_INI
  PROJEKTPFAD = nproj
end if

! ---------------------------------------------------------------------------------
! 3. Zeile in BAT.001
! -------------------
! EINLESEN DATEINAME: NAME DES FLUSSES.DAT
! Diese Datei wird durch die Oberflaeche erzeugt (z.B. Stoer.001), liegt
! im /PROF/ Ordner und wird nach Abschluss der Berechnung wieder geloescht.
! Dort stehen die Dateinamen der Profile und die dazu gehoerenden
! Stationierungen. Z.B.:
!
! St000053.prf 58.4500
! St000054.prf 58.4930
! St000055.prf 58.5500
! St000004.prf 58.6000

if (RUN_MODUS /='KALYPSO') then

  write (*, 1003)
  1003 format (/1X, 'Gib Name der fluss.dat - Datei -->')
  READ ( * , '(a)') dfluss

  STRANGDATEI = dfluss

end if


Teilung_Name: DO i = nch80, 1, - 1
  IF (STRANGDATEI(i:i) .eq.'.') then        	! Strangdatei z.B. "Stoer.001"
    FALLNAME (1:) = STRANGDATEI(i + 1:nch80)    ! Berechnungsfall z.B. "001"
    FLUSSNAME (1:) = STRANGDATEI(1:i - 1)       ! Flussname z.B. "Stoer"
    EXIT Teilung_Name
  ENDIF
END DO Teilung_Name


! ERWEITERUNG PFADNAME UM ORDNER \PROF
ilen = LEN_TRIM (PROJEKTPFAD)

NAME_PFAD_PROF = PROJEKTPFAD(1:ilen) // '\PROF\'
NAME_PFAD_DATH = PROJEKTPFAD(1:ilen) // '\DATH\'

ilen  = LEN_TRIM(NAME_PFAD_PROF)
ilen2 = LEN_TRIM(STRANGDATEI)
NAME_EIN_STR = NAME_PFAD_PROF(1:ilen) // STRANGDATEI(1:ilen2)


! UNIT holen
UNIT_EIN_STR = ju0gfu ()
! Oeffnen der Datei mit den Profilenamen (z.B. Stoer.001), siehe 3. Zeile in BAT.001
OPEN (unit = UNIT_EIN_STR, file = NAME_EIN_STR, iostat = istat, status = 'old')
IF (istat /= 0) then
   write (*, 9000) NAME_EIN_STR
   9000 format (//1X, 'Problem beim Oeffnen der Datei ', A, /, &
               &  1X, '-> PROGRAMMABBRUCH!')
   STOP
ENDIF                                                                 



! ---------------------------------------------------------------------------------
! 4. Zeile in BAT.001
! -------------------
! EINLESEN ANFANGSSTATION
if (RUN_MODUS /='KALYPSO') then
  write (*, 1004)
  1004 format (/1X, 'Anfangsstation (z.b. 1.5000) -->')
  ! F6.3 reicht nicht aus: Stationen sind auf 4 Stellen genau -> F9.4!
  READ ( * , '(F10.4)') staanf
  write (* , '(F10.4)') staanf

  ANFANGSSTATION = staanf
end if

! ---------------------------------------------------------------------------------
! 5. Zeile in BAT.001
! -------------------
! EINLESEN ENDSTATION
if (RUN_MODUS /='KALYPSO') then
  write (*, 1005)
  1005 format (/1X, 'Endstation -->')
  READ ( * , '(F10.4)') staend

  ENDSTATION = staend
end if



ilen  = LEN_TRIM(NAME_PFAD_DATH)
NAME_OUT_ALPHA = NAME_PFAD_DATH(1:ilen) // 'Beiwerte.aus'


IF (alpha_ja.eq.1) then
  !HB     Oeffnen der Ergebnisausgabedatei fuer die Impuls- und Energie-
  !HB     strombeiwert-Auswertung (zusaetzliche Ausgabedatei: Beiwerte.AUS

  UNIT_OUT_ALPHA = ju0gfu ()
  !HB     Oeffnen der Datei Beiwerte.AUS
  OPEN (UNIT = UNIT_OUT_ALPHA, FILE = NAME_OUT_ALPHA, STATUS = 'replace', IOSTAT = istat)
  if (istat /=0) then
    write (*,*) 'In WSP. Fehler beim Oeffnen von ', NAME_OUT_ALPHA
    stop
  end if
END IF


! Kommentar wird immer angezeigt!
antwort = 'n'
                                                                        
if (RUN_MODUS /='KALYPSO') then
  write ( * , 1100)
  1100 FORMAT (//,                                                        &
     & 1x,'**********************************************',/,     &
     & 1x,'*       --------------------------------     *',/,     &
     & 1x,'*          BERECHNUNGSMOEGLICHKEITEN         *',/,     &
     & 1x,'*       --------------------------------     *',/,     &
     & 1x,'*                                            *',/,     &
     & 1x,'*  Bitte waehlen Sie zwischen (1) und (5)    *',/,     &
     & 1x,'*                                            *',/,     &
     & 1x,'*  - Wasserspiegellagenberechnung            *',/,     &
     & 1x,'*    - mit Rauhigkeitsbeiwerten nach         *',/,     &
     & 1x,'*      Manning-Strickler (kst)         (1)   *',/,     &
     & 1x,'*    - mit aequivalenten Sandrauhig-         *',/,     &
     & 1x,'*      keiten nach Darcy-Weisbach (ks) (2)   *',/,     &
     & 1x,'*                                            *',/,     &
     & 1x,'*  - Bordvoll-Berechnung                     *',/,     &
     & 1x,'*    - mit Rauhigkeitsbeiwerten nach         *',/,     &
     & 1x,'*      Manning-Strickler (kst)         (3)   *',/,     &
     & 1x,'*    - mit aequivalenten Sandrauhig-         *',/,     &
     & 1x,'*      keiten nach Darcy-Weisbach      (4)   *',/,     &
     & 1x,'*                                            *',/,     &
     & 1x,'* -  Ende (5)                                *',/,     &
     & 1x,'*                                            *',/,     &
     & 1X,'**********************************************',//)


  ! ---------------------------------------------------------------------------------
  ! 6. Zeile in BAT.001
  ! -------------------
  ! EINLESEN ANFANGSSTATION
  write (*, 1006)
  1006 format (/1X, 'Berechnungsart -->')
  READ ( * , *) mode
  write (* , *) mode

  ! Vorbelegung fuer alle Faelle (wird spaeter eventl. geaendert)
  ABFLUSSEREIGNIS = 'qwert.001'

  IF (mode.eq.1.or.mode.eq.2) then

    ! WASSERSPIEGELLAGENBERECHNUNG
    if (mode.eq.1) ifg = 0              ! Manning-Strickler
    IF (mode.eq.2) ifg = 1        	! Darcy-Weisbach

    !bordvoll = 'n'

    BERECHNUNGSMODUS = 'WATERLEVEL'

    ! WP 24.05.2005
    ! Von der Oberflaeche wird eine Datei QWERT.001 erzeugt, die nach Ablauf der
    ! Berechnung wieder geloescht wird! Diese wird jetzt eingelesen!

    ilen = LEN_TRIM (NAME_PFAD_PROF)
    ilen2 = LEN_TRIM(FALLNAME)

    ABFLUSSEREIGNIS = 'qwert.' // FALLNAME(1:ilen2)
    NAME_EIN_QWERT = NAME_PFAD_PROF(1:ilen) // 'qwert.' // FALLNAME(1:ilen2)

    UNIT_EIN_QWERT = ju0gfu ()

    OPEN (UNIT = UNIT_EIN_QWERT, FILE = NAME_EIN_QWERT, IOSTAT = istat, STATUS = 'OLD', ACTION='READ')

    IF (istat.ne.0) then

      write (*,*)

      WRITE ( * , '(''qwert.dat existiert nicht auf der ebene'', a,'' !!'')') NAME_PFAD_PROF(1:ilen)
      CLOSE (UNIT_EIN_QWERT)

      1901 continue
      write (*,*) 'Gib Durchfluss [in qm/s] am 1. Profil an -->'
      jjanz = 1
      READ ( * , '(f10.0)', err = 1901) aqwert (1, jjanz)
      write (*,*) aqwert (1, jjanz)

      1902 continue
      write (*,*) 'Gib Station [in km]  des 1. Profils -->'
      READ ( * , '(f10.0)', err = 1902) aqstat (1, jjanz)
      write (*,*) aqstat (1, jjanz)

      write (*,*) 'Gib Bezeichnung Abfluss-Ereignis (max. 8 Z.) -->'
      READ ( * , '(a)') aereignis (jjanz)
      write (*,*) aereignis (jjanz)

      awsanf (jjanz) = 0.
      anq (jjanz) = 1

    ELSE

      jjanz = 0
      DO j = 1, merg
        ereign (j) = ' '
      END DO

      DO 95 j = 1, merg
        aereignis (j) = ' '
        anq (j) = 0
        aqstat (1, j) = 0.
        aqwert (1, j) = 0.
        awsanf (j) = 0.

        READ (UNIT_EIN_QWERT, '(a)', end = 92) dummy

        CALL ju0chr (dummy, zreal, ireal, zchar, ichara, zint, iint, ifehl)

        IF (ifehl.eq.1) goto 92

        aereignis (j) = zchar (1)
        anq (j) = zint (1)
        jjanz = jjanz + 1

        !**      anq = Anzahl der Stationen mit q-Wert-Wechsel
        IF (anq (j) .eq.0) then
           PRINT * , 'anzahl der stationen mit q-wert-wechsel=', nq, '!'
           CLOSE (UNIT_EIN_QWERT)
           GOTO 9999
        ELSEIF (anq (j) .gt.merg) then
           PRINT * , 'Fehler. Zuviele Q-Wechsel in QWERT-DATEI.'
           PRINT * , 'Erhoehe Parameter MERG auf ', anq (j)
           CLOSE (UNIT_EIN_QWERT)
           GOTO 9999
        ENDIF

        DO 90 i = 1, anq (j)

          READ (UNIT_EIN_QWERT, '(a)', end = 92) dummy
          CALL ju0chr (dummy, zreal, ireal, zchar, ichara, zint, iint, ifehl)
          IF (ireal.lt.2) then
            PRINT * , 'Fehler in QWERT.DAT - Datei '
            PRINT * , 'Kein Wertepaar selektiert in Zeile ', i
            CLOSE (UNIT_EIN_QWERT)
            GOTO 9999
          ENDIF

          aqstat (i, j) = zreal (1)
          aqwert (i, j) = zreal (2)

          IF (ireal.eq.3.and.i.eq.1) then
            awsanf (j) = zreal (3)
          ENDIF

        90 END DO

      95 END DO

      CLOSE (UNIT_EIN_QWERT)

    !**      endif(ierr.ne.0)
    ENDIF

    92 continue

    DO i = 1, jjanz
      ereign (i) = aereignis (i)
    END DO


  ! BORDVOLLBERECHNUNG, mode=3 FUER STRICKLER, mode=4 FUER DARCY
  ELSEIF (mode.eq.3.or.mode.eq.4) then

    if (mode.eq.3) ifg = 0              ! Manning-Strickler
    IF (mode.eq.4) ifg = 1        	! Darcy-Weisbach

    write ( * , 1200)
    1200 FORMAT (// &
       & 1X, '**********************************************',/,  &
       & 1X, '*       --------------------------------     *',/,  &
       & 1X, '*         ART DER BORDVOLLBERECHNUNG         *',/,  &
       & 1X, '*       --------------------------------     *',/,  &
       & 1X, '*                                            *',/,  &
       & 1X, '*  Bitte waehlen Sie zwischen                *',/,  &
       & 1X, '*                                            *',/,  &
       & 1X, '*    - mit Wasserspiegelgefaelle             *',/,  &
       & 1X, '*      parallel Sohlgefaelle           (1)   *',/,  &
       & 1X, '*                                            *',/,  &
       & 1X, '*    - bei stationaer-ungleich-              *',/,  &
       & 1X, '*      foermigem Abfluss               (2)   *',/,  &
       & 1X, '*                                            *',/,  &
       & 1X, '*    - unter konstantem Reibungsgefälle      *',/,  &
       & 1X, '*      bei stationaer-gleichf. Abfluss (3)   *',/,  &
       & 1X, '*                                            *',/,  &
       & 1X, '*    - Ende                            (4)   *',/,  &
       & 1X, '*                                            *',/,  &
       & 1X, '**********************************************',//)

    ! ---------------------------------------------------------------------------------
    ! 6a. Zeile in BAT.001
    ! --------------------
    ! EINLESEN ART DER BORDVOLLBERECHNUNG
    write (*, 1061)
    1061 format (/1X, 'Bordvollberechnung -->')
    READ ( * , *) mode
    write (* , *) mode

    145 continue

    IF (mode.eq.1) then

      BERECHNUNGSMODUS = 'BF_UNIFORM'
      ART_RANDBEDINGUNG = 'UNIFORM_BOTTOM_SLOPE'

      write ( * , 1300)
      1300 FORMAT (//  &
       & 1X, '**********************************************',/, &
       & 1X, '*   --------------------------------------   *',/, &
       & 1X, '*              BORDVOLLBERECHNUNG            *',/, &
       & 1X, '*   BEI STATIONAER-GLEICHFOERMIGEM ABFLUSS   *',/, &
       & 1X, '*   --------------------------------------   *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '**********************************************',//)

   !MD  ELSEIF (mode.eq.2) then
   !MD  Neu: mode = 3 = Reibungsgefaelle
   ELSEIF (mode.eq.2.or.mode.eq.3) then
      if (mode.eq.2) then

         BERECHNUNGSMODUS = 'BF_NON_UNI'

         write ( * , 1400)
         1400 FORMAT (// &
         & 1X, '**********************************************',/, &
         & 1X, '*  ----------------------------------------  *',/, &
         & 1X, '*              BORDVOLLBERECHNUNG            *',/, &
         & 1X, '*  BEI STATIONAER-UNGLEICHFOERMIGEM ABFLUSS  *',/, &
         & 1X, '*  ----------------------------------------  *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '*  Bitte geben Sie fuer die Bordvollbe-      *',/, &
         & 1X, '*  rechnung die Variationsbreite fuer die    *',/, &
         & 1X, '*  Durchfluesse an:                          *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '**********************************************',//)
      elseif (mode.eq.3) then

         BERECHNUNGSMODUS = 'REIB_KONST'

         write ( * , 1401)
         1401 FORMAT (// &
         & 1X, '**********************************************',/, &
         & 1X, '*  ----------------------------------------  *',/, &
         & 1X, '*   STATIONAER-UNGLEICHFOERMIGE BERECHNUNG   *',/, &
         & 1X, '*     UNTER KONSTANTEM REIBUNGSGEFÄLLE       *',/, &
         & 1X, '*  ----------------------------------------  *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '*  Bitte geben Sie fuer die Bordvollbe-      *',/, &
         & 1X, '*  rechnung die Variationsbreite fuer die    *',/, &
         & 1X, '*  Durchfluesse an:                          *',/, &
         & 1X, '*                                            *',/, &
         & 1X, '**********************************************',//)

      end if
      1191 continue

      ! ---------------------------------------------------------------------------------
      ! 6b. Zeile in BAT.001
      ! --------------------
      ! EINLESEN DER VARIATIONSBREITE FUER DIE BORDVOLLBERECHNUNG (MAX)
      write (*, 1062)
      1062 format (/1X, 'Maximaler Abfluss -->')
      READ ( * , *, err = 1191) MAX_Q
      WRITE ( * , '(f10.2)') MAX_Q

      1192 continue

      ! ---------------------------------------------------------------------------------
      ! 6c. Zeile in BAT.001
      ! --------------------
      ! EINLESEN DER VARIATIONSBREITE FUER DIE BORDVOLLBERECHNUNG (MIN)
      write (*, 1063)
      1063 format (/1X, 'Minimaler Abfluss -->')
      READ ( * , *, err = 1192) MIN_Q
      WRITE ( * , '(f10.2)') MIN_Q

      1193 continue

      ! ---------------------------------------------------------------------------------
      ! 6d. Zeile in BAT.001
      ! --------------------
      ! EINLESEN DER VARIATIONSBREITE FUER DIE BORDVOLLBERECHNUNG (SCHRITTWEITE)
      write (*, 1064)
      1064 format (/1X, 'Schrittweite Abflussvariation -->')
      READ ( * , *, err = 1193) DELTA_Q
      WRITE ( * , '(f10.2)') DELTA_Q

      ! ---------------------------------------------------------------------------------
      ! 6e. Zeile in BAT.001
      ! --------------------
      ! EINLESEN WQ-DATEIEN (j/n)
      write (*, 1065)
      1065 format (/1X, 'Sollen WQ-Dateien erstellt werden (j/n) ?')
      READ ( * , *) idr1
      WRITE (* , *) idr1


      ! ---------------------------------------------------------------------------------
      ! 6f. Zeile in BAT.001
      ! --------------------
      ! EINLESEN ERGEBNISLISTEN
      write (*, 1066)
      1066 format (/1X, 'Sollen Ergebnislisten erstellt werden (j/n) ?')
      READ ( * , *) idr2
      WRITE (* , *) idr2


      ! -----------------------
      ! Anfangswasserspiegel
      ! -----------------------
      6603 CONTINUE

      write ( * , 1500)
      1500 FORMAT (// &
       &'**********************************************',/, &
       &'*       --------------------------------     *',/, &
       &'*             Anfangswasserspiegel           *',/, &
       &'*       --------------------------------     *',/, &
       &'*                                            *',/, &
       &'*  Bitte waehlen Sie zwischen                *',/, &
       &'*                                            *',/, &
       &'* -  Berechnung des Ausgangswasserspie-      *',/, &
       &'*    gels als Grenztiefe                (1)  *',/, &
       &'*                                            *',/, &
       &'* -  Berechnung des Ausgangswasserspie-      *',/, &
       &'*    gels unter Annahme stationaer-          *',/, &
       &'*    gleichfoermiger Abflussverhaelt-        *',/, &
       &'*    nisse                              (2)  *',/, &
       &'*                                            *',/, &
       &'* -  Ende                               (3)  *',/, &
       &'*                                            *',/, &
       &'**********************************************',//)

      ! ---------------------------------------------------------------------------------
      ! 6g. Zeile in BAT.001
      ! --------------------
      ! EINLESEN ANFANGSWASSERSPIEGEL BORDVOLLBERECHNUNG
      write (*, 1067)
      1067 format (/1X, 'Anfangswasserspiegel -->')
      READ ( * , *, err = 6603) mode
      WRITE (* , *  ) mode


      IF (mode.eq.1) then

        ART_RANDBEDINGUNG = 'CRITICAL_WATER_DEPTH'

        wsanf = 0.0

      ELSEIF (mode.eq.2) then

        ART_RANDBEDINGUNG = 'UNIFORM_BOTTOM_SLOPE'

        6221 CONTINUE

        ! ---------------------------------------------------------------------------------
        ! 6g_1. Zeile in BAT.001
        ! ----------------------
        ! EINLESEN ANFANGSWASSERSPIEGEL BORDVOLLBERECHNUNG
        write (*, 10671)
        10671 format (/1X, 'Bitte geben Sie das Gefaelle ein -->')
        READ ( * , *, err = 6221) wsanf
        WRITE (* , *  ) wsanf

        GEFAELLE = wsanf

        wsanf = - 1. * wsanf

      ELSEIF (mode.eq.3) then

        write (*, 9010)
        9010 format (1X, 'Programmende!')
        STOP

      ELSE

        GOTO 6603

      ENDIF

    ELSE

      write (*, 9011)
      9011 format (1X, 'Programmende!')
      STOP

    ENDIF

  ELSE

    write (*, 9012)
    9012 format (1X, 'Programmende!')
    STOP

  ENDIF

ENDIF

! Read Discharge file (e.g. 'qwert.001')
if (RUN_MODUS == 'KALYPSO' .and. BERECHNUNGSMODUS == 'WATERLEVEL') then

  jjanz = 1                             ! Anzahl verfuegbarer Abflussereignisse

  ilen = LEN_TRIM (NAME_PFAD_PROF)
  ilen2 = LEN_TRIM(ABFLUSSEREIGNIS)
  NAME_EIN_QWERT = NAME_PFAD_PROF(1:ilen) // ABFLUSSEREIGNIS(1:ilen2)

  UNIT_EIN_QWERT = ju0gfu ()

  OPEN (UNIT = UNIT_EIN_QWERT, FILE = NAME_EIN_QWERT, IOSTAT = istat, STATUS = 'OLD', ACTION='READ')
  if (istat /= 0) then
    write (*,*) ' Fehler beim Oeffnen von ', NAME_EIN_QWERT
    write (*,*) ' Programm wird beendet.'
    stop
  end if

  read (UNIT_EIN_QWERT, *) EREIGNISNAME, nq
  do i = 1, nq
    read (UNIT_EIN_QWERT, *) qstat(i), qwert(i)
  end do

  close (UNIT_EIN_QWERT)

end if

IF (RUN_MODUS == 'KALYPSO') THEN
  if (ART_RANDBEDINGUNG == 'WATERLEVEL          ') then
    wsanf = ANFANGSWASSERSPIEGEL
  else if (ART_RANDBEDINGUNG == 'CRITICAL_WATER_DEPTH') then
    wsanf = 0.0
  else if (ART_RANDBEDINGUNG == 'UNIFORM_BOTTOM_SLOPE') then
    wsanf = -1.0 * GEFAELLE
  end if

END IF



! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH KALININ-MILJUKOV BERECHNUNG
! -------------------------------------------------------------------------------------
if (RUN_MODUS /='KALYPSO') then

  if (BERECHNUNGSMODUS /= 'WATERLEVEL') then

    !EP Reaktivierung von Kalinin-Miljukov     03.06.2002

    ! -----------------------------------------------------------------
    ! 6h. Zeile in BAT.001
    ! --------------------
    ! EINLESEN KALININ-MILJUKOV PARAMETER
    write (*, 1068)
    1068 format (/1X, 'Bestimmung der Kalinin-Miljukov-Parameter (j/n) ?')
    READ ( * , *) km
    WRITE (* , *) km


    IF (km.eq.'j') then

      ikitg = 1

      ilen = LEN_TRIM (NAME_PFAD_PROF)
      ilen2 = LEN_TRIM(FALLNAME)

      NAME_EIN_KM = NAME_PFAD_PROF(1:ilen) // 'teilg.' // FALLNAME(1:ilen2)
      UNIT_EIN_KM = ju0gfu ()

      OPEN (UNIT=UNIT_EIN_KM, FILE=NAME_EIN_KM, ACTION='READ', IOSTAT=istat)

      IF (istat.ne.0) then

        WRITE ( * , '(''teilg.dat existiert nicht auf der ebene'', A )')  NAME_PFAD_PROF
        CLOSE (UNIT_EIN_KM)

        3901 continue
        ! -----------------------------------------------------------------
        ! 6i. Zeile in BAT.001
        ! --------------------
        ! EINLESEN TEILGEBIETSNUMMER
        write (*, 1069)
        1069 format (/1X, 'Bitte geben Sie die Teilgebietnummer an -->')
        READ ( * , *, err = 3901) itg
        write (*, *) itg
        ikitg = 0

      ELSE

        ikg = 0

        read (UNIT_EIN_KM, '(I10)', IOSTAT=istat) ikg
        if (istat/=0) then
          write (*,10690)
          10690 format (//1X, 'Fehler beim Lesen der KM-Datei (teilg.***) in /PROF/ !', /, &
                        & 1X, 'Bitte pruefen Sie diese Datei bzw. die Eingabe in WSPWIN.', //, &
                        & 1X, 'Hinweis: Eventl. muss die Eingabe der Teilgebiete in WSPWIN', /, &
                        & 1X, 'nur einmal geaendert werden, so dass eine neue Datei erzeugt wird.', /, &
                        & 1X, '-> Programmabruch.')
          stop
        end if
        !write (*,*) 'In WSP. Anzahl Teilgebiete = ', ikg

        do i = 1, ikg
          read (UNIT_EIN_KM, *, IOSTAT=istat) anftg(i), numitg(i)
          !write (*,*) 'ANFTG(',i,') = ', anftg(i), ' numitg(',i,') = ', numitg(i)
        end do


        IF (ikg.eq.0) then

          WRITE ( * , '(''keine Teilgebietbestimmung in '',a,'' !!'')') NAME_EIN_KM
          CLOSE (UNIT_EIN_KM)

          3904 continue
          ! -----------------------------------------------------------------
          ! 6i. Zeile in BAT.001
          ! --------------------
          ! EINLESEN TEILGEBIETSNUMMER
          write (*, 1070)
          1070 format (/1X, 'Bitte geben Sie die Teilgebietnummer an -->')
          READ ( * , *, err = 3904) itg
          write (*, *) itg
          ikitg = 0

        ENDIF

      ENDIF

    !**   ENDIF ZU (km.eq."j")
    ENDIF

  !**   ENDIF ZU (bordvoll.ne."n")
  ENDIF

!**   ENDIF ZU (RUN_MODUS /= 'KALYPSO')
end if




! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH OERTLICHER VERLUSTE
! -------------------------------------------------------------------------------------

! Programmerweiterung vom 13.09.90 ,    E. Pasche
! einlesen von oertlichen Verlustbeiwerten aus der Datei psiver.dat

! WP 24.05.2005
! Von der Oberflaeche wird eine Datei PSIVER.001 erzeugt, die nach Ablauf der
! Berechnung wieder geloescht wird! Diese wird jetzt eingelesen!
! Einlesen der oertlichen Verluste:

if (RUN_MODUS /= 'KALYPSO') then

  ilen  = LEN_TRIM (NAME_PFAD_PROF)
  ilen2 = LEN_TRIM (FALLNAME)
  NAME_EIN_PSI = NAME_PFAD_PROF(1:ilen) // 'psiver.' // FALLNAME(1:ilen2)
  EINZELVERLUSTE = 'psiver.' // FALLNAME(1:ilen2)

else

  ilen  = LEN_TRIM (NAME_PFAD_PROF)
  ilen2 = LEN_TRIM (EINZELVERLUSTE)
  NAME_EIN_PSI = NAME_PFAD_PROF(1:ilen) // EINZELVERLUSTE(1:ilen2)

end if


INQUIRE (FILE=NAME_EIN_PSI, EXIST=lexist, IOSTAT=istat)

if (.not. lexist) then

  write (*, 9020) NAME_PFAD_PROF(1:ilen)
  9020 format (/1X, 'Datei mit oertlichen Verlusten ist nicht vorhanden in ', /, &
              & 1X, A, /, &
              & 1X, '-> Es treten keine oertliche Verluste auf!')
  jpsi = 0

else

  UNIT_EIN_PSI = ju0gfu ()

  OPEN (UNIT=UNIT_EIN_PSI, FILE=NAME_EIN_PSI, STATUS='OLD', ACTION='READ', IOSTAT=istat)
  if (istat /= 0) then
    write (*,*) 'Fehler beim Oeffnen von ', NAME_EIN_PSI
    stop
  end if

  jpsi = 0

  read_psiver: DO j = 1, maxger

    READ (UNIT_EIN_PSI, '(a)', IOSTAT=istat) string
    if (istat /= 0) EXIT read_psiver

    CALL ju0chr (string, feldr, ianz, char, ichara, int, iint, ifehl)

    IF (ifehl.eq.0 .and. ichara.eq.ianz) then

      jpsi = jpsi + 1
      psistat (jpsi) = 0.
      psiein (jpsi) = 0.
      psiort (jpsi) = 0.

      DO i1 = 1, ianz

        ilen = LEN_TRIM (char (i1) )

        CALL lcase (char (i1) )
        IF (char (i1) (1:ilen) .eq.'station') then
          psistat (jpsi) = feldr (i1)
        ELSE IF (char (i1) (1:ilen) .eq.'einlauf') then
          psiein (jpsi) = feldr (i1)
        ELSE IF (char (i1) (1:ilen) .eq.'kruemmer') then
          psiort (jpsi) = psiort (jpsi) + feldr (i1)
        ELSE IF (char (i1) (1:ilen) .eq.'rechen') then
          psiort (jpsi) = psiort (jpsi) + feldr (i1)
        ELSE IF (char (i1) (1:ilen) .eq.'zusatzverlust') then
          psiort (jpsi) = psiort (jpsi) + feldr (i1)
        ELSE IF (char (i1) (1:ilen) .eq.'auslauf') then
          psiort (jpsi) = psiort (jpsi) + feldr (i1)
        ELSE
          write (*,9021) char(i1)(1:ilen)
          9021 format (1X, 'Hinweis: Verlustart ',A , ' undefiniert!' )
          psiort (jpsi) = psiort (jpsi) + feldr (i1)
        ENDIF

      END DO

    !**         ELSE ZU (ifehl.eq.0.and.ichara.eq.ianz)
    ELSE

      write (*, 9022) psistat (jpsi)
      9022 format (/1X, 'Fehlerhafte Definition der oertlichen Verluste!', /, &
                  & 1X, 'Bitte pruefen Sie die Angabe von oertlichen Verlusten', /, &
                  & 1X, 'bei Station: ', F10.4, '!', /, &
                  & 1X, '-> PROGRAMMABBRUCH!' )
      CLOSE (UNIT_EIN_PSI)
      STOP

    !**         ENDIF ZU (ifehl.eq.0.and.ichara.eq.ianz)
    ENDIF

  END DO read_psiver

  CLOSE (UNIT_EIN_PSI)                                                                         
                                                                        
end if

!191 continue




! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH NORMALER SPIEGELLINIENBERECHNUNG
! -------------------------------------------------------------------------------------

if (RUN_MODUS /= 'KALYPSO') then

  !IF (bordvoll.eq.'n') then
  if (BERECHNUNGSMODUS == 'WATERLEVEL') then

    ! Falls es mehr als ein Abflussereignis gibt
    IF (jjanz.gt.1) then

      write (*, 1600)
      1600 format (//, &
             & 1X, '*****************************************************', /, &
             & 1X, '*        Verfuegbare Abflussereignisse              *', /, &
             & 1X, '*****************************************************')


      DO i1 = 1, jjanz

        ilen = LEN_TRIM (ereign (i1) )

        WRITE (*, 1601) ereign (i1) (1:ilen) , i1
        1601 format (1X, '*      ', A, T50, I2, T54, '*')

      END DO

      write (*, 1602)
      1602 FORMAT(1X, '*****************************************************')


      ! ---------------------------------------------------------------------------------
      ! 7. Zeile in BAT.001
      ! -------------------
      ! EINLESEN DES ABFLUSSEREIGNISSES
      do

        write (*, 1007)
        1007 format (/1X, 'Nr. des Abflussereignisses -->')
        READ ( * , *) nr
        write (* , *) nr

        if (nr.le.0 .or. nr.gt.jjanz) then
          write (*, 9001)
          9001 format (1X, 'Dies ist keine gueltige Nummer, bitte geben sie nocheinmal', /, &
                     & 1X, 'die Nummer des Abflussereignisses an!', /)
          CYCLE
        end if

        EXIT

      end do
      !-----------------------------------------------------------------------------------

    ELSE

      nr = 1

    ENDIF


    EREIGNISNAME = aereignis (nr)

    nq = anq (nr)

    write (*, 1603)
    1603 format (//, &
            & 1X, '*****************************************************', /, &
            & 1X, '*          Gewaehltes Abflussereignis               *', /, &
            & 1X, '*****************************************************')

    DO j = 1, nq

      qstat (j) = aqstat (j, nr)
      qwert (j) = aqwert (j, nr)

      write (*, 1604) j, qwert(j), qstat(j)
      1604 format (1X, '*   Q(', I2, ') = ', F10.3, '       an Station ', F10.3, '  *')

    END DO

    write (*, 1605)
    1605 format (1X, '*****************************************************')




    603 continue

    write ( * , 1700)
    1700 FORMAT (//  &
       & 1x,'**********************************************',/,     &
       & 1x,'*       --------------------------------     *',/,     &
       & 1x,'*             Anfangswasserspiegel           *',/,     &
       & 1x,'*       --------------------------------     *',/,     &
       & 1x,'*                                            *',/,     &
       & 1x,'*  Bitte waehlen Sie zwischen (1) und (4)    *',/,     &
       & 1x,'*                                            *',/,     &
       & 1x,'*  - Einlesen eines bekannten Anfangs-       *',/,     &
       & 1x,'*    wasserspiegels aus Datei qwert.dat (1)  *',/,     &
       & 1x,'*                                            *',/,     &
       & 1x,'* -  Berechnung des Ausgangswasserspie-      *',/,     &
       & 1X,'*    gels als Grenztiefe                (2)  *',/,     &
       & 1x,'*                                            *',/,     &
       & 1x,'* -  Berechnung des Ausgangswasserspie-      *',/,     &
       & 1x,'*    gels unter Annahme stationaer-          *',/,     &
       & 1x,'*    gleichfoermiger Abflussverhaelt-        *',/,     &
       & 1x,'*    nisse                              (3)  *',/,     &
       & 1x,'*                                            *',/,     &
       & 1x,'* -  Ende                               (4)  *',/,     &
       & 1x,'*                                            *',/,     &
       & 1X,'**********************************************',/)


    ! ---------------------------------------------------------------------------------
    ! 8. Zeile in BAT.001
    ! -------------------
    ! EINLESEN ANFANGSWASSERSPIEGEL
    write (*, 1008)
    1008 format (/1X, 'Art des Anfangswasserspiegels -->')
    READ ( * , *) mode
    write (* , *) mode

    IF (mode.eq.1) then

      ART_RANDBEDINGUNG = 'WATERLEVEL          '

      wsanf = awsanf (nr)

      IF (abs (wsanf) .lt. 1.e-04) then

        write (*,9002)
        9002 format (/1X, 'Warnung! Kein Wasserstand in QWERT-Datei angegeben.')

        do

          write (*, 1018)
          1018 format (1X, 'Bitte geben Sie den Ausgangswasserstand ein -->')
          read (UNIT=*, FMT=*, IOSTAT=istat) wsanf
          if (istat /= 0) CYCLE
          write (*, '(F10.4)') wsanf
          ANFANGSWASSERSPIEGEL = wsanf

          EXIT

        end do

      ENDIF

    ELSEIF (mode.eq.2) then

      ART_RANDBEDINGUNG = 'CRITICAL_WATER_DEPTH'

      wsanf = 0.0

    ELSEIF (mode.eq.3) then

      ART_RANDBEDINGUNG = 'UNIFORM_BOTTOM_SLOPE'

      do

        write (*, 1028)
        1028 format (1X, 'Bitte geben Sie das Gefaelle ein -->')
        read (UNIT=*, FMT=*, IOSTAT=istat) wsanf
        if (istat /= 0) CYCLE
        write (*, '(F10.4)') wsanf
        GEFAELLE = wsanf

        EXIT

      end do

      wsanf = - 1. * wsanf

    ELSE IF (mode.eq.4) then

      STOP 'Programmende'

    ELSE

      GOTO 603

    ENDIF

  ENDIF
                                                                        
end if




! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH ERGEBNISAUSGABE (zukuenftig obsolent)
! -------------------------------------------------------------------------------------

if (RUN_MODUS /= 'KALYPSO') then

  write (*, 1800)
  1800 format (// &
        & 1X, '****************************************************', /, &
        & 1X, '*               ---------------------              *', /, &
        & 1X, '*               AUSGABEMOEGLICHKEITEN              *', /, &
        & 1X, '*               ---------------------              *', /, &
        & 1X, '*  Bitte waehlen Sie zwischen den folgenden        *', /, &
        & 1X, '*  Ausgabemoeglichkeiten:                          *', /, &
        & 1X, '*                                                  *', /, &
        & 1X, '*  - Einfacher Ergebnisausdruck                    *', /, &
        & 1X, '*    - Erstellung "TAB" und "WSL"-Files (1)        *', /, &
        & 1X, '*                                                  *', /, &
        & 1X, '*  - Erweiterter Ergebnisausdruck                  *', /, &
        & 1X, '*    - Ausgabe eines Kontrollfiles      (2)        *', /, &
        & 1X, '*                                                  *', /, &
        & 1X, '*  - Berechnungsende                    (0)        *', /, &
        & 1X, '*                                                  *', /, &
        & 1X, '****************************************************')

  ! ---------------------------------------------------------------------------------
  ! 9. Zeile in BAT.001
  ! -------------------
  ! EINLESEN AUSGABEMOEGLICHKEITEN
  write (*, 1009)
  1009 format (/1X, 'Ausgabeart -->')
  READ ( * , *) lein
  write (* , *) lein

  ! Kennung fuer die Ausgabe des Ergebnisausdruckes:
  IF (lein.eq.0) then

    !UT LABEL 9999 = ENDE DES PROGRAMMES
    GOTO 9999

  ENDIF                                                                               

end if


!WP 10.03.2006 ----------------------------------------------------------------------
! Kontrollfile wird ab sofort  I M M E R  angelegt!

ilen = LEN_TRIM (NAME_PFAD_DATH)
NAME_OUT_LOG = NAME_PFAD_DATH(1:ilen) // 'Kontroll.log'

UNIT_OUT_LOG = ju0gfu ()

OPEN (unit = UNIT_OUT_LOG, file = NAME_OUT_LOG, status = 'REPLACE', iostat = istat)
if (istat /= 0) then
  write (*, 9003) NAME_OUT_LOG
  9003 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
             & 1X, 'Programm wird beendet!')
  call stop_programm(0)
end if                                                                       
!WP 10.03.2006 ----------------------------------------------------------------------




! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH VERZOEGERUNGSVERLUST
! -------------------------------------------------------------------------------------

if (RUN_MODUS /= 'KALYPSO') then

  if (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

    write (*, 1900)
    1900 format (1X, // &
         & 1X, '******************************************************', /, &
         & 1X, '*               ____________________                 *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '*               Verzoegerungsverlust                 *', /, &
         & 1X, '*               ____________________                 *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '*  Bitte waehlen Sie zwischen den folgenden          *', /, &
         & 1X, '*  Moeglichkeiten:                                   *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '*  - nach DVWK (betta=2/3)             (1)           *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '*  - nach BJOERNSEN (betta=0.5)        (2)           *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '*  - nach DFG (betta=2/(1+Ai-1/Ai)     (3)           *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '*  - ohne Verzoegerungsverlust         (4)           *', /, &
         & 1X, '*                                                    *', /, &   
         & 1X, '*  - Berechnungsende                   (0)           *', /, &
         & 1X, '*                                                    *', /, &
         & 1X, '******************************************************')


    ! ---------------------------------------------------------------------------------
    ! 10. Zeile in BAT.001
    ! -------------------
    ! EINLESEN VERZOEGERUNGSVERLUST
    write (*, 1010)
    1010 format (/1X, 'Verlust -->')
    READ ( * , *) iq
    write (* , *) iq

    !UT    LABEL 9999 = PROGRAMMENDE
    IF (iq.eq.0) goto 9999

    IF (iq.eq.1) VERZOEGERUNGSVERLUST = 'DVWK'
    IF (iq.eq.2) VERZOEGERUNGSVERLUST = 'BJOE'
    IF (iq.eq.3) VERZOEGERUNGSVERLUST = 'DFG '
    IF (iq.eq.4) VERZOEGERUNGSVERLUST = 'NON '

  else

    ! Vorbelegung
    VERZOEGERUNGSVERLUST = 'BJOE'

  ENDIF                                                                                  

end if


!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades fnam1, z.B.
!HB "C:\projektordner\flussname\prof\" und Zuweisung an ilen
ilen = LEN_TRIM (NAME_PFAD_DATH)
NAME_OUT_TAB = NAME_PFAD_DATH(1:ilen)

!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades fluss, z.B.
!HB "flussname" und Zuweisung an ifulen
iflulen = LEN_TRIM (FLUSSNAME)
                                                                        
!HB Wenn der Flussname mehr als 4 Stellen besitzt, wird dieser auf
!HB 4 Stellen begrenzt
IF (iflulen.gt.4) then
  iflulen = 4
ENDIF
                                                                        
!HB Anfuegen von den ersten 4 Buchstaben (maximal) des Flussnamens
!HB an den Pfad unit1. Der Name wird von Position 1 bis 'iflulen'
!HB geschrieben.
NAME_OUT_TAB (ilen+1 : ) = FLUSSNAME(1:iflulen)
                                                                        
!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.B.
!HB "C:\projektordner\flussname\dath\reduzierterflussname" und
!HB Zuweisung an ilen
ilen = LEN_TRIM (NAME_OUT_TAB)
                                                                        
!HB Bei Wasserspiegellagenberechnung wird dieser Dateipfad angelegt
IF (BERECHNUNGSMODUS == 'WATERLEVEL') then
  !HB        Anfuegen von '_' an den Pfad unit1. Es wird von Position
  !HB        'ilen+1' bis zur 80. Position' geschrieben. (durch das Angebe
  !HB        der 80. Position werden zusaetzlich unnötige Reste geloescht)
  NAME_OUT_TAB (ilen + 1 : ) = '_'
  !HB        Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.
  !HB        "C:\projektordner\flussname\dath\reduzierterflussname_" und
  !HB        Zuweisung an ilen
  ilen = LEN_TRIM (NAME_OUT_TAB)
  !HB        Zaehlen aller Buchstaben bzw. Positionen von ereignis, z.B.
  !HB        "AbfussereignisHQ" und Zuweisung an iqelen
  !write (*,*) 'Ereignis name = ', EREIGNISNAME

  iqelen = LEN_TRIM (EREIGNISNAME)
  !HB           Reduzierung von iqelen auf 3 Stellen
  IF (iqelen.gt.3) then
    iqelen = 3
  ENDIF
  !HB        Anfuegen von den ersten 3 Buchstaben (maximal) des
  !HB        Abflussereignisses an den Pfad unit1. Der Name wird von
  !HB        Position 1 bis 'iqelen' geschrieben.
  !HB        Es wird von Position 'ilen+1' bis zur 80. Position, da
  !HB        'nch80'=80, ereignis angehaengt.
  NAME_OUT_TAB (ilen + 1 : ) = EREIGNISNAME (1:iqelen)
  !HB        Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.
  !HB        "C:\projektordner\flussname\dath\redflussname_Abflusereignis"
  !HB        und Zuweisung an ilen.
  ilen = LEN_TRIM (NAME_OUT_TAB)
  !HB      Ende IF-Schleife fuer Wasserspiegelberechnung
ENDIF
                                                                        
!HB Anfuegen der Endung '.tab' an den Pfad unit1.
!HB Es wird von Position 'ilen+1' bis zur 80. Position, da
!HB 'nch80'=80, '.tab' angehaengt.
NAME_OUT_TAB (ilen+1 : ) = '.tab'

!write (*,*) 'NAME_OUT_TAB = ', NAME_OUT_TAB






! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH BAUWERKE
! -------------------------------------------------------------------------------------

!**  Kein Eintarg fuer GLEICHFOERMIGE BORDVOLLBERECHNUNG vorhanden
IF (BERECHNUNGSMODUS == 'BF_UNIFORM') then

  MIT_BRUECKEN 	= .FALSE.
  MIT_WEHREN 	= .FALSE.

ELSE
                                                                        
  if (RUN_MODUS /= 'KALYPSO') then

    ! ---------------------------------------------------------------------------------
    ! 11. Zeile in BAT.001
    ! -------------------
    ! EINLESEN BRUECKEN
    write (*, 1011)
    1011 format (/1X, 'Sollen die Bruecken mit berechnet werden (j/n) -->')
    READ ( * , *) ibruecke
    write (* , *) ibruecke
    CALL lcase (ibruecke)         ! Konvertiert die Eingabe in Kleinbuchstaben

    if (ibruecke == 'j') then
      MIT_BRUECKEN = .TRUE.
    else
      MIT_BRUECKEN = .FALSE.
    end if

    ! ---------------------------------------------------------------------------------
    ! 12. Zeile in BAT.001
    ! -------------------
    ! EINLESEN WEHRE
    write (*, 1012)
    1012 format (/1X, 'Sollen die Wehre mit berechnet werden (j/n) -->')
    READ ( * , *) wehr
    write ( *, *) wehr
    CALL lcase (wehr)             ! Konvertiert die Eingabe in Kleinbuchstaben          

    if (wehr == 'j') then
      MIT_WEHREN = .TRUE.
    else
      MIT_WEHREN = .FALSE.
    end if

  end if

ENDIF                                                                  

                                                                        

if (RUN_MODUS /= 'KALYPSO') then

  ! ---------------------------------------------------------------------------------
  ! 13. Zeile in BAT.001
  ! -------------------
  ! EINLESEN WSL-FILE ERZEUGEN
  write (*, 1013)
  1013 format (/1X, 'Sollen die Profilnummern im .wsl-file dargestellt werden (j/n) -->')
  READ ( * , *) nr_ausg
  write (* , *) nr_ausg
  CALL lcase (nr_ausg)            ! Konvertiert die Eingabe in Kleinbuchstaben            

else

  nr_ausg = 'n'

end if



! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH WIDERSTANDSGESETZ
! -------------------------------------------------------------------------------------

if (RUN_MODUS /= 'KALYPSO') then

  ! Wenn Darcy Weisbach, Abfrage nach Formeinfluss
  IF (ifg.eq.1) then

    1306 continue

    write ( * , 2000)
    2000 FORMAT  (//, &
      & 1X, '*************************************************',/, &
      & 1X, '*          --------------------------------     *',/, &
      & 1X, '*            ART DES WIDERSTANDGESETZES         *',/, &
      & 1X, '*          --------------------------------     *',/, &
      & 1X, '*                                               *',/, &
      & 1X, '*     Bitte waehlen Sie zwischen (1) und (2)    *',/, &
      & 1X, '*                                               *',/, &
      & 1X, '*                                               *',/, &
      & 1X, '*       -  COLEBROOK-WHITE (ROHRSTROEMG.) (1)   *',/, &
      & 1X, '*                                               *',/, &
      & 1X, '*       -  COLEBROOK-WHITE MIT                  *',/, &
      & 1X, '*          FORMEINFLUSS                   (2)   *',/, &
      & 1X, '*                                               *',/, &
      & 1X, '*                                               *',/, &
      & 1X, '*************************************************')

    ! ---------------------------------------------------------------------------------
    ! 14. Zeile in BAT.001
    ! -------------------
    ! EINLESEN WIDERSTANDSGESETZ
    write (*, 1014)
    1014 format (/1X, 'Widerstandsgesetz -->')
    READ ( * , *) mode
    write (* , *) mode

    IF (mode.eq.1) then
      FLIESSGESETZ = 'DW_O_FORMBW'
    ELSE IF (mode.eq.2) then
      FLIESSGESETZ = 'DW_M_FORMBW'
    ELSE
      write (*, 9004)
      9004 format (1X, 'Falsche Eingabe! -> Es wird Methode (2) angenommen!')
      FLIESSGESETZ = 'DW_M_FORMBW'
    ENDIF

  ELSE

    FLIESSGESETZ = 'MANNING_STR'

  ENDIF                                                                                   

end if




! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH REIBUNGSVERLUST UND GENAUIGKEIT
! -------------------------------------------------------------------------------------

! Vorbelegung
rg_vst = 1
REIBUNGSVERLUST = 'TRAPEZ'
ITERATIONSART = 'SIMPLE'

if (RUN_MODUS /= 'KALYPSO') then

  if (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

    write ( * , 2100)
    2100 FORMAT (// &
       & 1X, '**********************************************',/, &
       & 1X, '*  --------------------------------------    *',/, &
       & 1X, '*  ART DER BERECHNUNG VON REIBUNGSVERLUST    *',/, &
       & 1X, '*  --------------------------------------    *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*  Bitte waehlen Sie zwischen                *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*  - nach Trapezformel                (1)    *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*  - durch geometrische Mittelung     (2)    *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '**********************************************')

    ! ---------------------------------------------------------------------------------
    ! 15. Zeile in BAT.001
    ! -------------------
    ! EINLESEN REIBUNGSVERLUST
    write (*, 1015)
    1015 format (/1X, 'Reibungsverlust -->')
    READ ( * , *) rg_vst
    write ( *, *) rg_vst

    IF (rg_vst.ne.1 .and. rg_vst.ne.2) then
      write (*, 9005)
      9005 format (1X, 'Falsche Eingabe! -> Es wird Methode (1) angenommen!')
      rg_vst = 1
    ENDIF

    if (rg_vst == 1) then
      REIBUNGSVERLUST = 'TRAPEZ'
    else if (rg_vst == 2) then
      REIBUNGSVERLUST = 'GEOMET'
    end if


    write ( * , 2200)
    2200 FORMAT (//  &
       & 1X, '**********************************************',/, &
       & 1X, '*  --------------------------------------    *',/, &
       & 1X, '*      BERECHNUNG DES WASSERSPIEGELS         *',/, &
       & 1X, '*  --------------------------------------    *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*  Bitte waehlen Sie zwischen                *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*   - Einfache Iteration              (1)    *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '*   - Genaue Berechnung durch                *',/, &
       & 1X, '*     Einschlussintervall             (2)    *',/, &
       & 1X, '*                                            *',/, &
       & 1X, '**********************************************')

    ! ---------------------------------------------------------------------------------
    ! 16. Zeile in BAT.001
    ! -------------------
    ! EINLESEN BERECHNUG WASSERSPIEGEL
    write (*, 1016)
    1016 format (/1X, 'Berechnungsmethode -->')
    READ ( * , *) a_m
    write (* , *) a_m

    IF (a_m.ne.1 .and. a_m.ne.2) then
      write (*, 9006)
      9006 format (1X, 'Falsche Eingabe! -> Es wird Methode (1) angenommen!')
      a_m = 1
    ENDIF

    if (a_m == 1) then
      ITERATIONSART = 'SIMPLE'
    else if (a_m == 2) then
      ITERATIONSART = 'EXACT '
    end if

  ENDIF

end if


! -----------------------------------------------------------------------------
! ZUSAETZLICHE KONFIGURATIONSDATEI IM /PROF/ ORDNER
! WP 14.11.2007
! -----------------------------------------------------------------------------

!WP Die fogenden Einstellungen koennen ab sofort in
!WP der Datei Kalypso_add.cfg festegelgt werden.

!USE_EXTREM_ROUGH = .false.
!MD  TEST TEST TEST TEST
!VERZOEGERUNGSVERLUST = 'NON '
!BERECHNUNGSMODUS = 'REIB_KONST'
!MD  TEST TEST TEST TEST


IF (RUN_MODUS /= 'KALYPSO') THEN
  ilen = LEN_TRIM(NAME_PFAD_PROF)
  NAME_EIN_ADD_INI = NAME_PFAD_PROF(1:ilen) // 'Kalypso_add.cfg'

  INQUIRE(FILE=NAME_EIN_ADD_INI, EXIST = lexist)
  IF (lexist) THEN
    write (*,1030) NAME_EIN_ADD_INI(1:ilen)
    1030 format (//1X, 'Im Projektordner ', A, /, &
                 & 1X, 'liegt die Datei "Kalypso_add.cfg" mit zusaetzlichen Konfigurationen vor!')
    call read_config_file(ADJUSTL(NAME_EIN_ADD_INI),LEN_TRIM(NAME_EIN_ADD_INI))     
  END IF  

END IF



! -------------------------------------------------------------------------------------
! ENDE DATENEINGABE
! -------------------------------------------------------------------------------------

write (*, 1017)
1017 format (//1X, '***************************************************', /, &
             & 1X, '*   Eingabe beendet, Berechnung wird gestartet!   *', /, &
             & 1X, '***************************************************', ////)


write (*,1020) PROJEKTPFAD, STRANGDATEI, BERECHNUNGSMODUS, FLIESSGESETZ, &
             & ANFANGSSTATION, ENDSTATION, ART_RANDBEDINGUNG, &
             & ANFANGSWASSERSPIEGEL, GEFAELLE, &
             & VERZOEGERUNGSVERLUST, ITERATIONSART, REIBUNGSVERLUST, &
             & MIT_BRUECKEN, MIT_WEHREN, USE_EXTREM_ROUGH, &
             & ABFLUSSEREIGNIS, &
             & EINZELVERLUSTE, &
             & MIN_Q, MAX_Q, DELTA_Q, DURCHFLUSS_EINHEIT

1020 format (//1X, 'KONFIGURATIONSDATEN', /, &
             & 1X, '--------------------------------------------------', /, &
             & 1X, 'PROJEKTPFAD = ', A, /, &
             & 1X, 'STRANGDATEI = ', A, //, &
             & 1X, 'BERECHNUNGSMODUS = ', A10, /, &
             & 1X, 'FLIESSGESETZ     = ', A11, //, &
             & 1X, 'ANFANGSSTATION = ', F12.4, /, &
             & 1X, 'ENDSTATION     = ', F12.4, //, &
             & 1X, 'ART_RANDBEDINGUNG = ', A20, //, &
             & 1X, 'ANFANGSWASSERSPIEGEL = ', F12.4, /, &
             & 1X, 'GEFAELLE             = ', F12.7, //, &
             & 1X, 'VERZOEGERUNGSVERLUST = ', A4, /, &
             & 1X, 'ITERATIONSART        = ', A5, /, &
             & 1X, 'REIBUNGSVERLUST      = ', A6, //, &
             & 1X, 'MIT_BRUECKEN    = ', L1,/, &
             & 1X, 'MIT_WEHREN      = ', L1, /, &
             & 1X, 'MIT_EXTREM_RAUH = ', L1, //, &
             & 1X, 'ABFLUSSEREIGNIS = ', A, /, &
             & 1X, 'EINZELVERLUSTE  = ', A, //, &
             & 1X, 'MIN_Q   = ', F10.4, /, &
             & 1X, 'MAX_Q   = ', F10.4, /, &
             & 1X, 'DELTA_Q = ', F10.4, //, &
             & 1X, 'DURCHFLUSS_EINHEIT = ', A1, /)



!WP -----------------------------------------------------------------------------------
! Erzeuge Pfad- und Dateinamen für 'Laengsschnitt.txt'
ilen = LEN_TRIM(NAME_PFAD_DATH)
NAME_OUT_LAENGS = NAME_PFAD_DATH(1:ilen) // 'laengsschnitt.txt'
!WP -----------------------------------------------------------------------------------


!WP -----------------------------------------------------------------------------------
!WP 11.11.2005
!WP Es wird eine Datei zum Ausgeben der einzelnen Lambda-Werte
!WP ueber den gesamten Querschnitt angelegt.

ilen = LEN_TRIM (NAME_PFAD_DATH)
NAME_OUT_LAMBDA_I = NAME_PFAD_DATH(1:ilen) // 'lambda_i.txt'

UNIT_OUT_LAMBDA_I = ju0gfu ()

OPEN (unit = UNIT_OUT_LAMBDA_I, file = NAME_OUT_LAMBDA_I, status = 'REPLACE', iostat = istat)
if (istat /= 0) then
  write (*, 9007) NAME_OUT_LAMBDA_I
  9007 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
             & 1X, 'Programm wird beendet!')
  call stop_programm(0)
end if

write (UNIT_OUT_LAMBDA_I, 9008) 'PROFIL', 'PUNKT', 'X [m]', 'H [m]', 'LAMBDA [-]', 'V [m/s]'
9008 format (1X, A10, A7, A10, A10, A12, A10)
!WP -----------------------------------------------------------------------------------




! --------------------------------------------------------------------------------------------
! Aufruf der Subroutinen WSPBER und QBORDV
! --------------------------------------------------------------------------------------------

if (BERECHNUNGSMODUS /= 'WATERLEVEL') then

  !HB wenn Bordvollberechnung mit parallelem Sohlgefälle (bordvoll=g)
  !HB gewaehlt wurde, werden die gesetzten Werte (qstep,rqmax,rqmin)
  !HB fuer die Berechnung zwar nicht benoetigt, jedoch muss fuer die
  !HB Uebergabe nach SUB qbordv eine Zuweisung erfolgt sein
  IF (BERECHNUNGSMODUS == 'BF_UNIFORM') then
    DELTA_Q = 0.0
    MAX_Q = 1.e+06
    MIN_Q = 1.e-06
  ENDIF
                                                                        
  !HB Aufruf der Bordvollberechnung
  CALL qbordv ()

  IF (km.eq.'j' .and. ikitg.eq.1) then
    CLOSE (UNIT_EIN_KM)
  ENDIF

ELSE

  nr_q = 1
  anz_q = 1
  CALL wspber ()

ENDIF
                                                                        
if (RUN_MODUS /= 'KALYPSO') then
  CLOSE (UNIT_EIN_STR, STATUS = 'DELETE')
else
  CLOSE (UNIT_EIN_STR)
end if


9999 CONTINUE

call close_units()

WRITE ( *, 1099) VERSIONNR
1099 FORMAT (//1X, 'Es wurde mit ', A29, ' erfolgreich gerechnet.',//     &
             & 1X, 'PROGRAMMENDE!')

END PROGRAM WSP
                                                                        

