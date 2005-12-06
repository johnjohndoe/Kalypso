!     Last change:  WP    6 Dec 2005    5:18 pm
!--------------------------------------------------------------------------
! This code, wsp.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
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



PROGRAM WSP
!**   ------------------------------------------------------------------
!**   Berechung von Wasserspiegellagen                                  
!**                                                                     
!**   geschrieben: P. Koch, Maerz 1990                                  
!**   ------------------------------------------------------------------
!**   Progarammbeschreibung:                                            
!**                                                                     
!**   Dieses Programm steuert die Bildschirmeingabe der fuer die Wasser-
!**   spiegellagenberechnung notwendigen Angaben. Im einzelnen werden   
!**   angegeben:                                                        
!**   - Art der Berechnung : bordvoll oder normale Wsp-Berechnung       
!**   - Auswahl des zu berechnenden Abflussereignisses                  
!**   - Auswahl des Berechnungsabschnittes                              
!**   - Art der Berechnung des Anfangswasserspiegels                    
!**   - Auswahl des Ausgabeumfangs                                      
!**   - Auswahl des 'Verzoegerungsbeiwertes'                            
!**                                                                     
!**   Das Steuerprogramm ruft dann das Hauptprogramm WSPBER auf.        
!**   ------------------------------------------------------------------
!**   Parameter: merg   - max. Anzahl der HW-Ergeignisse in qwert.dat   
!**              maxkla - max. Anzahl der Wertepaare(x,h) im Profil     
!**              maxger - Anzahl der Gerinneabschn. im qfr-Datensatz?   
!**   ------------------------------------------------------------------
!**                                                                     
!**   VERWENDETE PARAMETER                                              
!**   --------------------                                              
!**   ipro     - MAXIMALE ANZAHL DATENSAETZE IM PROFIL                  
!**   i_les    - Funktionswert fuer WSPvaria.EIN                        
!**   i1_les   - Funktionswert fuer WSPvaria.EIN                        
!**   i_aus    - Funktionswert fuer variaaus.AUS                        
!**   i_ver    - Funktionswert fuer verlauf1.WSP                        
!**   pfad_aus - Pfad ?:\Projekt\Projektname\dath\variaaus.AUS          
!**   pfad_les - Pfad ?:\Projekt\Projektname\dath\WSPvaria.EIN          
!**   pfad_ver - Pfad ?:\Projekt\Projektname\dath\verlauf1.WSP          
!**   proj_les - Pfad ?:\Projekt\Projektname\dath\                      
!**                                                                     
!HB   ***************************************************************** 
!HB   26.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB   alph_aus - Pfad ?:\Projekt\Projektname\dath\Beiwerte.AUS          
!HB   i_alph   - Funktionswert                                          
!HB   nr_alph  - Interne Dateinummer fuer Beiwerte.AUS                  
!HB   ***************************************************************** 
!**                                                                     
!**   AUFGERUFENE UNTERPROGRAMME                                        
!**   --------------------------                                        
!**   ju0chr(dummy,zreal,ireal,zchar,ichara,zint,iint,ifehl)
!**   ju0gfu ()                                                         
!**   lcase (antwort)
!**   wspber (unit1,ibruecke,wehr,imeth,antwort)
!JK   qbordv ()                                                         
!UT   XVERSWSP - VERSIONSABFRAGE
!**                                                                     
!***********************************************************************
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE ZEIT
USE VERSION


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


! COMMON-Block /AUSGABELAMBDA/ -----------------------------------------------------
INTEGER         	:: jw_lambdai
REAL, DIMENSION(maxkla) :: lambda_teilflaeche
CHARACTER(LEN=nch80) 	:: lambdai
COMMON / ausgabelambda / jw_lambdai, lambda_teilflaeche, lambdai
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


! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), npr, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, npr, np
! ----------------------------------------------------------------------------------


! COMMON-Block /FLG_TYP/ -----------------------------------------------------------
CHARACTER(LEN=6):: i_typ_flg
COMMON / flg_typ / i_typ_flg
! ----------------------------------------------------------------------------------


! COMMON-Block /FUNIT/ --------------------------------------------------------
INTEGER 	:: jw1, jw2
COMMON / funit / jw1, jw2
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


! COMMON-Block /NR_AUS/ --------------------------------------------------------
! Commonblock fuer Profilnummerausgabe im *.wsl -File
CHARACTER(LEN = 1) :: nr_ausg
COMMON / nr_aus / nr_ausg
! -----------------------------------------------------------------------------


! COMMON-Block /P1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: ereignis, fnam1, fluss
CHARACTER(LEN=1) :: bordvoll
COMMON / p1 / ereignis, fnam1, bordvoll, fluss
! -----------------------------------------------------------------------------


! COMMON-Block /P4/ -----------------------------------------------------------
INTEGER :: ifg
REAL 	:: betta
COMMON / p4 / ifg, betta
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


! COMMON-Block /REIB/ ---------------------------------------------------------
! Berechnungsart des Reibungsverlustes
INTEGER 	:: rg_vst
COMMON / reib / rg_vst
! -----------------------------------------------------------------------------


! COMMON-Block /TEILDAT/ ------------------------------------------------------
REAL 		:: anftg (merg)
INTEGER 	:: iw13, numitg (merg), ikg
COMMON / teildat / iw13, anftg, numitg, ikg
! -----------------------------------------------------------------------------


! COMMON-Block /TEILGEBIET/ ---------------------------------------------------
INTEGER         :: ikitg, itg
COMMON / teilgebiet / ikitg, itg
! -----------------------------------------------------------------------------


! COMMON-Block /W_A/ ----------------------------------------------------------
INTEGER 	:: a_m
COMMON / w_a / a_m
! -----------------------------------------------------------------------------




INTEGER :: anq (merg), int (maxkla)
INTEGER :: zint (merg)
INTEGER :: ZAEHLOPE
INTEGER :: imp_ber
!**   imp_ber = 0 Bernoulli - Gleichung
!**   imp_ber = 1 Berechnung mit Impulsbilanz                           


!HB   Projektpfade fuer WSPvaria.ein und variaaus.AUS, Verlauf1.WSP     
CHARACTER(LEN=nch80) :: proj_les, pfad_les, pfad_aus, pfad_ver, zchar (merg)
CHARACTER(LEN=nch80) :: unit6, unit4, unit1, unit13, string, nproj
CHARACTER(LEN=nch80) :: aereignis (merg), char (maxkla), dummy, unit7
CHARACTER(LEN=nch80) :: ereign (merg)
CHARACTER(LEN=nch80) :: dfluss, unit2
CHARACTER(LEN=nch80) :: fall
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
INTEGER i_alph
!HB   *****************************************************************
                                                                        

! ------------------------------------------------------------------
! Initialisierungen
! ------------------------------------------------------------------
idr1 = 'n'              ! Sollen WQ-Dateien fuer jedes Profil erstellt werden?
idr2 = 'n'              ! Sollen Ergebnislisten erstellt werden?
km = "n"                ! Sollen die KALININ-MILJUKOV-Parameter bestimmt werden?

imp_ber = 0             ! Berechnung nach Bernoulli-Gleichung
                                                                        
nr = 0  		! COMMON-Block BLOED


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
! die Eingabe IMMER durch eine Maske (was auch immer das heiﬂt)
! durchgefuehrt wird. Damit gilt dann immer ANTWORT = 'J'!
! Die Subroutine SELECT wird nie wieder aufgerufen und damit komplett
! geloescht!
write (*, 1001)
1001 format (//1X, 'Organisierung der Eingabedaten durch Maskeneingabe ?')
READ ( * , '(a)') antwort
write (*, *) antwort


! ---------------------------------------------------------------------------------
! 2. Zeile in BAT.001
! -------------------
! EINLESEN PFADNAME DES PROJEKTES
write (*,1002)
1002 format (/1X, 'Gib Pfadname des Projektes -->')
READ ( * , '(a)') nproj
write (*,*) nproj


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
!
write (*, 1003)
1003 format (/1X, 'Gib Name der fluss.dat - Datei -->')
READ ( * , '(a)') dfluss
write (*,*) dfluss


! ---------------------------------------------------------------------------------
! 4. Zeile in BAT.001
! -------------------
! EINLESEN ANFANGSSTATION
write (*, 1004)
1004 format (/1X, 'Anfangsstation (z.b. 1.5000) -->')
! F6.3 reicht nicht aus: Stationen sind auf 4 Stellen genau -> F9.4!
READ ( * , '(F10.4)') staanf
write (* , '(F10.4)') staanf


! ---------------------------------------------------------------------------------
! 5. Zeile in BAT.001
! -------------------
! EINLESEN ANFANGSSTATION
write (*, 1005)
1005 format (/1X, 'Endstation -->')
! F6.3 reicht nicht aus: Stationen sind auf 4 Stellen genau -> F9.4!
READ ( * , '(F10.4)') staend
write (* , '(F10.4)') staend



! ERWEITERUNG PFADNAME UM ORDNER \PROF
Erweiterung_um_prof: DO i = nch80, 1, - 1
  IF (dfluss (i:i) .eq.'.') then
    fall (1:) = dfluss (i + 1:nch80)
    fluss (1:) = dfluss (1:i - 1)
    EXIT Erweiterung_um_prof
  ENDIF
END DO Erweiterung_um_prof


ilen = LEN_TRIM (nproj)

nproj (ilen + 1:) = '\prof\'

fnam1 = nproj           ! FNAM1 ist Absoluter Projektpfad, z.B. c:\wspwin\will_pad\prof
unit2 = fnam1

ilen1 = LEN_TRIM (unit2)
unit2 (ilen1 + 1:) = dfluss


! UNIT holen
jw2 = ju0gfu ()
! Oeffnen der Datei mit den Profilenamen (z.B. Stoer.001), siehe 3. Zeile in BAT.001
OPEN (unit = jw2, iostat = ierr, file = unit2, status = 'old')
IF (ierr /= 0) then
   write (*, 9000) unit2
   9000 format (//1X, 'Problem beim Oeffnen der Datei ', A, /, &
               &  1X, '-> PROGRAMMABBRUCH!')
   STOP
ENDIF                                                                 


! Setzen und Uebergabe der Pfad-Parameter, die zur Dateioeffnung
! von WSPvaria.ein in "LESDATIN" benoetigt werden.

! Der bereits in der if-schleife eingelesene Dateipfad 'nproj' wird
! uebernommen und der neuen Variable proj_les zugewiesen.
! In nproj ist enthalten: c:\Projekt\Projektname\prof
proj_les = nproj
i_les = LEN_TRIM (proj_les)

! Da der uebernommene Projektpfad nach 'prof' verweist, wird dieser
! um 5 Stellen reduziert und um '\dath\' ergaenzt.
proj_les (i_les - 5:) = '\dath\'
! Der Projektpfad proj_les wird dem Pfad pfad_les zugewiesen.
! Fuer Eingabedatei-Pfad.
pfad_les = proj_les
! Der Projektpfad proj_les wird dem Pfad pfad_aus zugewiesen.
! Fuer Kontroll-Ausgabedatei-Pfad.
pfad_aus = proj_les
! Der Projektpfad proj_les wird dem Pfad pfad_ver zugewiesen.
! Fuer Verlaufsdatei-Pfad.
pfad_ver = proj_les
i1_les = LEN_TRIM (pfad_les)

! Der Pfad pfad_les wird um die Einlesedatei erweitert.
pfad_les (i1_les + 1:) = 'WSPvaria.ein'
i_aus = LEN_TRIM (pfad_aus)

! Der Pfad pfad_aus wird um die Kontroll-Ausgabedatei erweitert.
pfad_aus (i_aus + 1:) = 'variaaus.AUS'
i_ver = LEN_TRIM (pfad_ver)
! Der Pfad pfad_ver wird um die Ausgabedatei Verlauf1.WSP erweitert.
pfad_ver (i_ver + 1:) = 'Verlauf1.WSP'
                                                                        
!HB   ***************************************************************** 
!HB   26.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB   Der Projektpfad proj_les wird dem Pfad alph_aus zugewiesen.       
!HB   Fuer Ausgabedatei-Pfad des Impuls-und Energiestrombeiwertes.      
alph_aus = proj_les
i_alph = LEN_TRIM (alph_aus)
!HB   Der Pfad alph_aus wird um des Namen der Ausgabedatei erweitert.   
alph_aus (i_alph + 1:) = 'Beiwerte.AUS'
!HB   ***************************************************************** 
                                                                        
!HB   Aufruf der SUB LESDATIN und Uebergabe der Pfade, wo die
!HB   Eingabedatei WSPvaria.ein und die Kontroll-Ausgabedatei           
!HB   variaaus.AUS zu finden sind.                                      
!HB   Aufruf von "lesdatin.for", welches die Steuerparameter aus        
!HB   "WSPvaria.ein" einliesst und durch Commonbloecke an die           
!HB   entsprechenden Subroutinen uebergibt.                             

! Kommentar wird immer angezeigt!
antwort = 'n'
                                                                        
                                                                        
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



! WENN WASSERSPIEGELLAGENBERECHNUNG
IF (mode.eq.1.or.mode.eq.2) then

  ifg = 0
  IF (mode.eq.2) ifg = 1

  bordvoll = 'n'
                                                                        
  ! WP 24.05.2005
  ! Von der Oberflaeche wird eine Datei QWERT.001 erzeugt, die nach Ablauf der
  ! Berechnung wieder geloescht wird! Diese wird jetzt eingelesen!
  !ilen = ju0nch (fnam1)
  ilen = LEN_TRIM (fnam1)
  unit4 = fnam1
  unit4 (ilen + 1:nch80) = 'qwert.'
  !ilen = ju0nch (unit4)
  ilen = LEN_TRIM (unit4)
  unit4 (ilen + 1:nch80) = fall
  jw3 = ju0gfu ()
  ierr = 0                          

  OPEN (UNIT = jw3, IOSTAT = ierr, FILE = unit4, STATUS = 'OLD', ACTION='READ')
                                                                        
  IF (ierr.ne.0) then

    write (*,*)

    WRITE ( * , '(''qwert.dat existiert nicht auf der ebene'', a,'' !!'')') fnam1(1:ilen-1)
    CLOSE (jw3)
                                                                        
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

      READ (jw3, '(a)', end = 92) dummy

      CALL ju0chr (dummy, zreal, ireal, zchar, ichara, zint, iint, ifehl)

      IF (ifehl.eq.1) goto 92

      aereignis (j) = zchar (1)
      anq (j) = zint (1)
      jjanz = jjanz + 1

      !**      anq = Anzahl der Stationen mit q-Wert-Wechsel
      IF (anq (j) .eq.0) then
         PRINT * , 'anzahl der stationen mit q-wert-wechsel=', nq, '!'
         CLOSE (jw3)
         GOTO 9999
      ELSEIF (anq (j) .gt.merg) then
         PRINT * , 'Fehler. Zuviele Q-Wechsel in QWERT-DATEI.'
         PRINT * , 'Erhoehe Parameter MERG auf ', anq (j)
         CLOSE (jw3)
         GOTO 9999
      ENDIF

      DO 90 i = 1, anq (j)

        READ (jw3, '(a)', end = 92) dummy
        CALL ju0chr (dummy, zreal, ireal, zchar, ichara, zint, iint, ifehl)
        IF (ireal.lt.2) then
          PRINT * , 'Fehler in QWERT.DAT - Datei '
          PRINT * , 'Kein Wertepaar selektiert in Zeile ', i
          CLOSE (jw3)
          GOTO 9999
        ENDIF

        aqstat (i, j) = zreal (1)
        aqwert (i, j) = zreal (2)

        IF (ireal.eq.3.and.i.eq.1) then
          awsanf (j) = zreal (3)
        ENDIF

      90 END DO                                                                 
                                                                        
    95 END DO
                                                                        
    CLOSE (jw3)
                                                                        
  !**      endif(ierr.ne.0)
  ENDIF
                                                                        
  92 continue

  DO i = 1, jjanz
    ereign (i) = aereignis (i)
  END DO
                                                                        
  !     ------------------------------------------------------------------
  !     Programmerweiterung vom 13.09.90 ,    E. Pasche
  !     einlesen von oertlichen Verlustbeiwerten aus der Datei psiver.dat
  !     bis ca. Programmzeile 650
                                                                        

! BORDVOLLBERECHNUNG, mode=3 FUER STRICKLER, mode=4 FUER DARCY
ELSEIF (mode.eq.3.or.mode.eq.4) then
                                                                        
  IF (mode.eq.4) then
    ifg = 1
  ELSE
    ifg = 0
  ENDIF
                                                                        
  130 continue

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
     & 1X, '*    - Ende                            (3)   *',/,  &
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

    bordvoll = 'g'

    write ( * , 1300)
    1300 FORMAT (//  &
     & 1X, '**********************************************',/, &
     & 1X, '*   --------------------------------------   *',/, &
     & 1X, '*              BORDVOLLBERECHNUNG            *',/, &
     & 1X, '*   BEI STATIONAER-GLEICHFOERMIGEM ABFLUSS   *',/, &
     & 1X, '*   --------------------------------------   *',/, &
     & 1X, '*                                            *',/, &
     & 1X, '**********************************************',//)
                                                                        
  ELSEIF (mode.eq.2) then

    bordvoll = 'u'

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
                                                                        
    1191 continue

    ! ---------------------------------------------------------------------------------
    ! 6b. Zeile in BAT.001
    ! --------------------
    ! EINLESEN DER VARIATIONSBREITE FUER DIE BORDVOLLBERECHNUNG (MAX)
    write (*, 1062)
    1062 format (/1X, 'Maximaler Abfluss -->')
    READ ( * , *, err = 1191) rqmax
    WRITE ( * , '(f10.2)') rqmax
                                                                        
    1192 continue

    ! ---------------------------------------------------------------------------------
    ! 6c. Zeile in BAT.001
    ! --------------------
    ! EINLESEN DER VARIATIONSBREITE FUER DIE BORDVOLLBERECHNUNG (MIN)
    write (*, 1063)
    1063 format (/1X, 'Minimaler Abfluss -->')
    READ ( * , *, err = 1192) rqmin
    WRITE ( * , '(f10.2)') rqmin

    1193 continue

    ! ---------------------------------------------------------------------------------
    ! 6d. Zeile in BAT.001
    ! --------------------
    ! EINLESEN DER VARIATIONSBREITE FUER DIE BORDVOLLBERECHNUNG (SCHRITTWEITE)
    write (*, 1064)
    1064 format (/1X, 'Schrittweite Abflussvariation -->')
    READ ( * , *, err = 1193) qstep
    WRITE ( * , '(f10.2)') qstep


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

      wsanf = 0.0

    ELSEIF (mode.eq.2) then

      6221 CONTINUE

      ! ---------------------------------------------------------------------------------
      ! 6g_1. Zeile in BAT.001
      ! ----------------------
      ! EINLESEN ANFANGSWASSERSPIEGEL BORDVOLLBERECHNUNG
      write (*, 10671)
      10671 format (/1X, 'Bitte geben Sie das Gefaelle ein -->')
      READ ( * , *, err = 6221) wsanf
      WRITE (* , *  ) wsanf

      wsanf = - 1. * wsanf

    ELSEIF (mode.eq.3) then

      IF (lein.eq.3) close (jw8)
      write (*, 9010)
      9010 format (1X, 'Programmende!')
      STOP

    ELSE

      GOTO 6603

    ENDIF
                                                                        
  ELSE

    IF (lein.eq.3) close (jw8) 
    write (*, 9011)
    9011 format (1X, 'Programmende!')
    STOP

  ENDIF
                                                                        
ELSE

  IF (lein.eq.3) close (jw8)
  write (*, 9012)
  9012 format (1X, 'Programmende!')
  STOP
                                                                        
ENDIF
                                                                        



! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH KALININ-MILJUKOV BERECHNUNG
! -------------------------------------------------------------------------------------

IF (bordvoll.ne.'n') then

  !UT ******Berechnung ohne Kalinin-Miljukov **********
  !UT goto 3999
  !UT - OBIGES WAR BEREITS DEAKTIVIERT, 12.02.00, UT
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
                                                                        
    !ilen = ju0nch (fnam1)
    ilen = LEN_TRIM (fnam1)
    unit13 = fnam1
    unit13 (ilen + 1:nch80) = 'teilg.'
    !ilen = ju0nch (unit13)
    ilen = LEN_TRIM (unit13)
    unit13 (ilen + 1:nch80) = fall
    iw13 = ju0gfu ()
    ierr = 0

    OPEN (UNIT=iw13, IOSTAT=ierr, FILE=unit13, STATUS='OLD', ACTION='READ')


    IF (ierr.ne.0) then

      WRITE ( * , '(''teilg.dat existiert nicht auf der ebene'', A )')  fnam1 (1:ilen - 1)
      CLOSE (iw13)

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

      3902 continue

      READ (iw13, '(a)', end = 3903) dummy
      CALL ju0chr (dummy, feldr, ianz, char, ichar, int, iint, ifehl)
      IF (ifehl.ne.0) then
        GOTO 3902
      ELSEIF (ianz.ne.1.or.iint.ne.1) then
        GOTO 3902
      ELSE
        ikg = ikg + 1
        anftg (ikg) = feldr (1)
        numitg (ikg) = int (1)
        GOTO 3902
      ENDIF

      3903 continue
      IF (ikg.eq.0) then

        WRITE ( * , '(''keine Teilgebietbestimmung in '',a,'' !!'')') unit13
        CLOSE (iw13)

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
                                                                        
                                                                        
write (*, *)

! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH OERTLICHER VERLUSTE
! -------------------------------------------------------------------------------------

! WP 24.05.2005
! Von der Oberflaeche wird eine Datei PSIVER.001 erzeugt, die nach Ablauf der
! Berechnung wieder geloescht wird! Diese wird jetzt eingelesen!
! Einlesen der oertlichen Verluste:
!ilen = ju0nch (fnam1)
ilen = LEN_TRIM (fnam1)
unit6 = fnam1
unit6 (ilen + 1:nch80) = 'psiver.'
!ilen = ju0nch (unit6)
ilen = LEN_TRIM (unit6)
unit6 (ilen + 1:nch80) = fall
jw4 = ju0gfu ()
                                                                        

OPEN (UNIT=jw4, IOSTAT=ierr, FILE=unit6, STATUS='OLD', ACTION='READ')
                                                                        
IF (ierr.ne.0.) then
  write (*, 9020) fnam1(1:ilen-1)
  9020 format (/1X, 'Datei mit oertlichen Verlusten ist nicht vorhanden in ', /, &
              & 1X, A, /, &
              & 1X, '-> Es treten keine oertliche Verluste auf!')

  jpsi = 0
  GOTO 191
                                                                        
ENDIF
                                                                        
jpsi = 0


DO j = 1, maxger
                                                                        
  READ (jw4, '(a)', end = 191) string

  CALL ju0chr (string, feldr, ianz, char, ichar, int, iint, ifehl)

  IF (ifehl.eq.0.and.ichar.eq.ianz) then

    jpsi = jpsi + 1
    psistat (jpsi) = 0.
    psiein (jpsi) = 0.
    psiort (jpsi) = 0.

    DO i1 = 1, ianz

      !ilen = ju0nch (char (i1) )
      ilen = LEN_TRIM (char (i1) )
      CALL lcase (char (i1) )
      IF (char (i1) (1:ilen) .eq.'station') then
        psistat (jpsi) = feldr (i1)
      ELSEIF (char (i1) (1:ilen) .eq.'einlauf') then
        psiein (jpsi) = feldr (i1)
      ELSEIF (char (i1) (1:ilen) .eq.'kruemmer') then
        psiort (jpsi) = psiort (jpsi) + feldr (i1)
      ELSEIF (char (i1) (1:ilen) .eq.'rechen') then
        psiort (jpsi) = psiort (jpsi) + feldr (i1)
      ELSEIF (char (i1) (1:ilen) .eq.'zusatzverlust') then
        psiort (jpsi) = psiort (jpsi) + feldr (i1)
      ELSE
        write (*,9021) char(i1)(1:ilen)
        9021 format (1X, 'Hinweis: Verlustart ',A , ' undefiniert!' )
        psiort (jpsi) = psiort (jpsi) + feldr (i1)
      ENDIF

    END DO

  !**         ELSE ZU (ifehl.eq.0.and.ichar.eq.ianz)
  ELSE

    write (*, 9022) psistat (jpsi)
    9022 format (/1X, 'Fehlerhafte Definition der oertlichen Verluste!', /, &
                & 1X, 'Bitte pruefen Sie die Angabe von oertlichen Verlusten', /, &
                & 1X, 'bei Station: ', F10.4, '!', /, &
                & 1X, '-> PROGRAMMABBRUCH!' )
    CLOSE (jw4)
    STOP

  !**         ENDIF ZU (ifehl.eq.0.and.ichar.eq.ianz)
  ENDIF                                                                          
                                                                        
END DO

CLOSE (jw4) 
                                                                        


! -------------------------------------------------------------------------------------
! ABFRAGEN BEZUEGLICH NORMALER SPIEGELLINIENBERECHNUNG
! -------------------------------------------------------------------------------------

191 continue

IF (bordvoll.eq.'n') then
                                                                        
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
                                                                        

  ereignis = aereignis (nr)

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
    210 continue

    write (*,*)

    wsanf = awsanf (nr)

    IF (abs (wsanf) .lt. 1.e-04) then

      write (*,9002)
      9002 format (1X, 'Warnung! Kein Wasserstand in QWERT-Datei angegeben.')

      do

        write (*, 1018)
        1018 format (1X, 'Bitte geben Sie den Ausgangswasserstand ein -->')
        read (UNIT=*, FMT=*, IOSTAT=ierr) wsanf
        if (ierr /= 0) CYCLE
        write (*, '(F10.4)') wsanf
        EXIT
         
      end do

    ENDIF
                                                                        
  ELSEIF (mode.eq.2) then

    220 continue
    wsanf = 0.0
                                                                        
  ELSEIF (mode.eq.3) then

    221 continue


    do

      write (*, 1028)
      1028 format (1X, 'Bitte geben Sie das Gefaelle ein -->')
      read (UNIT=*, FMT=*, IOSTAT=ierr) wsanf
      if (ierr /= 0) CYCLE
      write (*, '(F10.4)') wsanf
      EXIT

    end do

    wsanf = - 1. * wsanf
                                                                        
  ELSEIF (mode.eq.4) then

    IF (lein.eq.3) close (jw8)
    STOP 'Programmende'
                                                                        
  ELSE

    GOTO 603 
                                                                        
  ENDIF
                                                                        

ELSEIF (bordvoll.eq.'u'.or.bordvoll.eq.'g') then
!**   andere maske mit anfangswsp --> kein wsp aus qwert.dat            
                                                                        
ENDIF
                                                                        


240 continue


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


                                                                        
!UT   UMBENNUNG VON lein von 2 auf 3, da 2 bei Eingabe gewaehlt wurde
IF (lein.eq.2) lein = 3
                                                                        

!**   Kennung fuer die Ausgabe des Ergebnisausdruckes:
IF (lein.eq.0) then

  !UT LABEL 9999 = ENDE DES PROGRAMMES
  GOTO 9999

ELSEIF (lein.eq.3) then

  ! Kontrollfile anlegen
  jw8 = ju0gfu ()
  ierr = 0
  unit7 = fnam1
  ilen = LEN_TRIM (unit7)

  unit7 (ilen - 4:ilen - 1) = 'dath'
  unit7 (ilen + 1:nch80) = 'Kontroll.log'

  OPEN (unit = jw8, file = unit7, status = 'REPLACE', iostat = ierr)
  if (ierr /= 0) then
    write (*, 9003) unit7
    9003 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
               & 1X, 'Programm wird beendet!')
    call stop_programm(0)
  end if

  write (*,*) 'Kontrolldatei "KONTROL.LOG" wird angelegt!'

ENDIF
                                                                        


IF (bordvoll.ne.'g') then
                                                                        
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
  IF (iq.eq.1) betta = 2. / 3.
  IF (iq.eq.2) betta = 0.5
  IF (iq.eq.3) betta = 0.0
                                                                        
ENDIF
                                                                        

!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades fnam1, z.B.
!HB "C:\projektordner\flussname\prof\" und Zuweisung an ilen
ilen = LEN_TRIM (fnam1)
unit1 = fnam1
!HB Anfuegen von 'dath' an den Pfad unit1
!HB Es wird von Position 'ilen-4' bis 'ilen-1' geschrieben
unit1 (ilen - 4:ilen - 1) = 'dath'
!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.B.
!HB "C:\projektordner\flussname\dath\" und Zuweisung an ilen
ilen = LEN_TRIM (unit1)
!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades fluss, z.B.
!HB "flussname" und Zuweisung an ifulen
iflulen = LEN_TRIM (fluss)
                                                                        
!HB Wenn der Flussname mehr als 4 Stellen besitzt, wird dieser auf
!HB 4 Stellen begrenzt
IF (iflulen.gt.4) then
  iflulen = 4
ENDIF
                                                                        
!HB Anfuegen von den ersten 4 Buchstaben (maximal) des Flussnamens
!HB an den Pfad unit1. Der Name wird von Position 1 bis 'iflulen'
!HB geschrieben.
!HB Es wird von Position 'ilen+1' bis zur 80.Position, da
!HB'nch80'=80, fluss angehaengt.
unit1 (ilen + 1:nch80) = fluss (1:iflulen)
                                                                        
!HB Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.B.
!HB "C:\projektordner\flussname\dath\reduzierterflussname" und
!HB Zuweisung an ilen
ilen = LEN_TRIM (unit1)
                                                                        
!HB Bei Wasserspiegellagenberechnung wird dieser Dateipfad angelegt
IF (bordvoll.eq.'n') then
  !HB        Anfuegen von '_' an den Pfad unit1. Es wird von Position
  !HB        'ilen+1' bis zur 80. Position' geschrieben. (durch das Angebe
  !HB        der 80. Position werden zusaetzlich unnˆtige Reste geloescht)
  unit1 (ilen + 1:nch80) = '_'
  !HB        Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.
  !HB        "C:\projektordner\flussname\dath\reduzierterflussname_" und
  !HB        Zuweisung an ilen
  ilen = LEN_TRIM (unit1)
  !HB        Zaehlen aller Buchstaben bzw. Positionen von ereignis, z.B.
  !HB        "AbfussereignisHQ" und Zuweisung an iqelen
  iqelen = LEN_TRIM (ereignis)
  !HB           Reduzierung von iqelen auf 3 Stellen
  IF (iqelen.gt.3) then
    iqelen = 3
  ENDIF
  !HB        Anfuegen von den ersten 3 Buchstaben (maximal) des
  !HB        Abflussereignisses an den Pfad unit1. Der Name wird von
  !HB        Position 1 bis 'iqelen' geschrieben.
  !HB        Es wird von Position 'ilen+1' bis zur 80. Position, da
  !HB        'nch80'=80, ereignis angehaengt.
  unit1 (ilen + 1:nch80) = ereignis (1:iqelen)
  !HB        Zaehlen aller Buchstaben bzw. Positionen des Pfades unit1, z.
  !HB        "C:\projektordner\flussname\dath\redflussname_Abflusereignis"
  !HB        und Zuweisung an ilen.
  ilen = LEN_TRIM (unit1)
  !HB      Ende IF-Schleife fuer Wasserspiegelberechnung
ENDIF
                                                                        
!HB Anfuegen der Endung '.tab' an den Pfad unit1.
!HB Es wird von Position 'ilen+1' bis zur 80. Position, da
!HB 'nch80'=80, '.tab' angehaengt.
unit1 (ilen + 1:nch80) = '.tab'



!**   GLEICHFOERMIGE BORDVOLLBERECHNUNG                                 
IF (bordvoll.eq.'g') then

  ibruecke = 'n' 
  wehr = 'n'

!**   BERECHNUNGEN DIE NICHT GLEICHFOERMIGE BORDVOLLRECHNUNGEN SIND
ELSE
                                                                        
  ! ---------------------------------------------------------------------------------
  ! 11. Zeile in BAT.001
  ! -------------------
  ! EINLESEN BRUECKEN
  write (*, 1011)
  1011 format (/1X, 'Sollen die Bruecken mit berechnet werden (j/n) -->')
  READ ( * , *) ibruecke
  write (* , *) ibruecke
  CALL lcase (ibruecke)         ! Konvertiert die Eingabe in Kleinbuchstaben


  ! ---------------------------------------------------------------------------------
  ! 12. Zeile in BAT.001
  ! -------------------
  ! EINLESEN WEHRE
  write (*, 1012)
  1012 format (/1X, 'Sollen die Wehre mit berechnet werden (j/n) -->')
  READ ( * , *) wehr
  write ( *, *) wehr
  CALL lcase (wehr)             ! Konvertiert die Eingabe in Kleinbuchstaben

!**   ENDIF (bordvoll.eq.'g')
ENDIF                                                                  
                                                                        

! ---------------------------------------------------------------------------------
! 13. Zeile in BAT.001
! -------------------
! EINLESEN WSL-FILE ERZEUGEN
write (*, 1013)
1013 format (/1X, 'Sollen die Profilnummern im .wsl-file dargestellt werden (j/n) -->')
READ ( * , *) nr_ausg
write (* , *) nr_ausg
CALL lcase (nr_ausg)            ! Konvertiert die Eingabe in Kleinbuchstaben



!**    ifg=1 bei bordvoll nach Darcy
IF (ifg.eq.1) then

  1306 continue

  write ( * , 2000)
  2000 FORMAT  (// &
    &, 1X, '*************************************************',/, &
    &, 1X, '*          --------------------------------     *',/, &
    &, 1X, '*            ART DES WIDERSTANDGESETZES         *',/, &
    &, 1X, '*          --------------------------------     *',/, &
    &, 1X, '*                                               *',/, &
    &, 1X, '*     Bitte waehlen Sie zwischen (1) und (2)    *',/, &
    &, 1X, '*                                               *',/, &
    &, 1X, '*                                               *',/, &
    &, 1X, '*       -  COLEBROOK-WHITE (ROHRSTROEMG.) (1)   *',/, &
    &, 1X, '*                                               *',/, &
    &, 1X, '*       -  COLEBROOK-WHITE MIT                  *',/, &
    &, 1X, '*          FORMEINFLUSS                   (2)   *',/, &
    &, 1X, '*                                               *',/, &
    &, 1X, '*                                               *',/, &
    &, 1X, '*************************************************')

  ! ---------------------------------------------------------------------------------
  ! 14. Zeile in BAT.001
  ! -------------------
  ! EINLESEN WIDERSTANDSGESETZ
  write (*, 1014)
  1014 format (/1X, 'Widerstandsgesetz -->')
  READ ( * , *) mode
  write (* , *) mode

  IF (mode.eq.1) then
    i_typ_flg = 'colebr'
  ELSEIF (mode.eq.2) then
    i_typ_flg = 'pasche'
  ELSE
    write (*, 9004)
    9004 format (1X, 'Falsche Eingabe! -> Es wird Methode (2) angenommen!')
    i_typ_flg = 'pasche'
  ENDIF

ENDIF
                                                                        


!**   UNGLEICHF÷RMIGE BORDVOLLBERECHNUNG, bordvoll.ne.g                 
IF (bordvoll.ne.'g') then
                                                                        
  1313 CONTINUE

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
                                                                        

  2600 CONTINUE

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
                                                                        
ENDIF
                                                                        


write (*, 1017)
1017 format (//1X, '***************************************************', /, &
             & 1X, '*   Eingabe beendet, Berechnung wird gestartet!   *', /, &
             & 1X, '***************************************************', ////)


!WP -----------------------------------------------------------------------------------
!WP 11.11.2005
!WP Es wird eine Datei zum Ausgeben der einzelnen Lambda-Werte
!WP ueber den gesamten Querschnitt angelegt.
jw_lambdai = ju0gfu ()
ierr = 0
lambdai = fnam1
ilen = LEN_TRIM (lambdai)

lambdai (ilen - 4:ilen - 1) = 'dath'
lambdai (ilen + 1:nch80) = 'lambda_i.txt'

OPEN (unit = jw_lambdai, file = lambdai, status = 'REPLACE', iostat = ierr)
if (ierr /= 0) then
  write (*, 9007) lambdai
  9007 format (1X, 'Fehler beim Oeffnen der Datei ', A, /, &
             & 1X, 'Programm wird beendet!')
  call stop_programm(0)
end if

write (jw_lambdai, 9008) 'PROFIL', 'PUNKT', 'X [m]', 'H [m]', 'LAMBDA [-]', 'V [m/s]'
9008 format (1X, A10, A7, A10, A10, A12, A10)
!WP -----------------------------------------------------------------------------------



! --------------------------------------------------------------------------------------------
! Aufruf der Subroutinen WSPBER und QBORDV
! --------------------------------------------------------------------------------------------

IF (bordvoll.ne.'n') then
                                                                        
  !HB wenn Bordvollberechnung mit parallelem Sohlgef‰lle (bordvoll=g)
  !HB gewaehlt wurde, werden die gesetzten Werte (qstep,rqmax,rqmin)
  !HB fuer die Berechnung zwar nicht benoetigt, jedoch muss fuer die
  !HB Uebergabe nach SUB qbordv eine Zuweisung erfolgt sein
  IF (bordvoll.ne.'u') then
    qstep = 0.0
    rqmax = 1.e+06
    rqmin = 1.e-06
  ENDIF
                                                                        
  !HB Aufruf der Bordvollberechnung
  CALL qbordv (rqmax, rqmin, qstep, unit1, ibruecke, wehr)

  IF (km.eq.'j' .and. ikitg.eq.1) then
    CLOSE (iw13)
  ENDIF

ELSE

  CALL wspber (unit1, ibruecke, wehr)

ENDIF
                                                                        
CLOSE (jw2, STATUS = 'DELETE')
                                                                        
!**   lein=3: Erstellung eines Kontrollfiles, dann Datei jw8 schlieﬂen  
9999 CONTINUE

IF (lein.eq.3) close (jw8)

WRITE ( *, 1099) VERSIONNR
1099 FORMAT (//1X, 'Es wurde mit ', A29, ' erfolgreich gerechnet.',//     &
             & 1X, 'PROGRAMMENDE!')

call stop_programm(0)

END PROGRAM WSP
                                                                        

