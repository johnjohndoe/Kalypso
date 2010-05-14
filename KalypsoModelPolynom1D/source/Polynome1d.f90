!     Last change:  MD   12 Jun 2008    1:50 pm

!***************************************************************************************************
!**   MODUL VARIABLEN                                                                            ***
!***************************************************************************************************

      MODULE VARIABLEN
      IMPLICIT NONE
      SAVE

      INTEGER, parameter :: maxwert = 100000      ! Max. Anzahl von Werten insgesamt
      INTEGER, parameter :: maxprf = 1000        ! Max. Anzahl von Profilen

!**   CHARACTERANWEISUNGEN
!**   --------------------
      CHARACTER (LEN=2) MARKER(0:20)             ! ABFRAGE DER CODIERUNG AUS DER INI-DATEI
      CHARACTER (LEN=1) ABFRAGE                  ! ABFRAGE bei PolynomgenerierungsProblemen  MIT "J" ODER "N"
      CHARACTER (LEN=1) WASTUN                   ! ABFRAGE ob 3-teiliges Polynom MIT "J" ODER "N"
      CHARACTER (LEN=1) PROFINTERVALL            ! ABFRAGE ob Polynomgenerierung fuer bestimmtes Profil-Intervall MIT "J" ODER "N"
      CHARACTER (LEN=1) AUSGFUNKT                ! ABFRAGE ob besondere AusgabeDatei AUS DER INI-DATEI
      CHARACTER (LEN=1) WSPWERTE                 ! ABFRAGE ob besondere AusgabeDatei AUS DER INI-DATEI
      CHARACTER (LEN=1) KONTROLLE                ! ABFRAGE ob besondere AusgabeDatei AUS DER INI-DATEI
      CHARACTER (LEN=1) AUSREISSER               ! ABFRAGE ob Ausreißer in den Wertepaaren entfernt werden sollen MIT "J" ODER "N"
      CHARACTER (LEN=100) QLANGSSCHNITT          ! AUSGELESENE PFADLAENGE AUS INI-DATEI
      CHARACTER (LEN=100) AUSGABEPFAD            ! AUSGELESENE PFADLAENGE AUS INI-DATEI
      CHARACTER (LEN=1) AUTOBORDV                ! ABFRAGE ob automatische Erkennung der Bordvollpunkte ueber Steigungsaenderung (J)
                                                 !  oder ggbn Bordvollpunkte (N) fuer die 2Teilung der Stuetzstellen verwenden.

      CHARACTER (LEN=9) PROFILNAME(0:maxwert)    ! BEHAELT DIE PROFILNUMMER ALS NAMEN xxxx.xxxx
      CHARACTER (LEN=6) NAME_1                   ! BEHAELT DIE PROFILNUMMER ALS NAMEN x.xxxx
      CHARACTER (LEN=7) NAME_2                   ! BEHAELT DIE PROFILNUMMER ALS NAMEN xx.xxxx
      CHARACTER (LEN=8) NAME_3                   ! BEHAELT DIE PROFILNUMMER ALS NAMEN xxx.xxxx
      CHARACTER (LEN=3) NUMMER_1                 ! ERGAENZUNG DER FEHLENDEN NULLEN = '000'
      CHARACTER (LEN=2) NUMMER_2                 ! ERGAENZUNG DER FEHLENDEN NULLEN = '00'
      CHARACTER (LEN=1) NUMMER_3                 ! ERGAENZUNG DER FEHLENDEN NULLEN = '0'

      CHARACTER (LEN=9) PROFNAME(0:maxprf)       ! BEHAELT DIE PROFILNUMMER ALS NAMEN (68.000)
      CHARACTER (LEN=4) TXT                      ! DATEI-TYP
      CHARACTER (LEN=1) KENNUNG(0:maxwert)       ! ProfilKennung aus dem Laengsschnitt
      CHARACTER (LEN=1) KENN(0:maxprf)           ! ProfilKennung fuer die Ausgabe

!**   INTEGERANWEISUNGEN
!**   ------------------
      INTEGER :: N, NN, M, K, YY, KKK, INIZ    ! ZAEHLER ZUR EINLESE UND AUSGABE SCHLEIFE
      INTEGER :: NMAX, AA,PPP,QQQ              ! MAXIMALERZAEHLER DER EINGELESENEN PROFILDATEN
      INTEGER :: MMAX, XMAX                    ! MAXIMALERZAEHLER DER EINGELESENEN ABFLUSSEREIGNISSE
      INTEGER :: XEND, RED                     ! MAXIMALERZAEHLER DES ABFLUSSES FUER DIE POLYNOMERSTELLUNG
      INTEGER :: Grenze                        ! SCHALTER FUER PLAUSIBLE EINGANGSWERTE: Hi < H(i+1)
                                               ! WENN Grenze = 0; DANN BEDINGUNG ERFÜLLT; SONST = 1
      INTEGER :: WASGRAD                       ! ABFRAGE zum gewuenschten Polynomgrad "2, 3 ODER 4"
      INTEGER :: TEXTAUSG                      ! LAENGE DES PFAD DES AUSGABEORDNERS MIT RICHTIGER LAENGE
      INTEGER :: TEXTLGSS                      ! LAENGE DES PFAD ZUR Q-LAENGSCHNITT-DATEI
      INTEGER :: TEXT12, TEXT13, TEXT14        ! LAENGE DES PFAD
      INTEGER :: TEXT15, TEXT16, TEXT17        ! LAENGE DES PFAD
      INTEGER :: TEXT18, TEXT19, TEXT20        ! LAENGE DES PFAD

      INTEGER :: MQ_1(0:maxprf,0:maxprf), MQX_1(0:maxprf)        ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer Q(h) u A(h)
      INTEGER :: MQ_2(0:maxprf,0:maxprf), MQX_2(0:maxprf)        ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer Q(h) u A(h)
      INTEGER :: MQQ_1(0:maxprf,0:maxprf), MQQX_1(0:maxprf)      ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer Q(h)
      INTEGER :: MQQ_2(0:maxprf,0:maxprf), MQQX_2(0:maxprf)      ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer Q(h)
      INTEGER :: MAA_1(0:maxprf,0:maxprf), MAAX_1(0:maxprf)      ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer A(h)
      INTEGER :: MAA_2(0:maxprf,0:maxprf), MAAX_2(0:maxprf)      ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer A(h)
      INTEGER :: MA_1(0:maxprf,0:maxprf), MAX_1(0:maxprf)        ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer a(h)
      INTEGER :: MA_2(0:maxprf,0:maxprf), MAX_2(0:maxprf)        ! Zeahler fuer die Ausreisser-Belegung je Abfluss fuer a(h)
      INTEGER :: ABSNUMMER(0:maxprf,0:maxprf,1:3)                ! ABFRAGE ZU DEN BENETZTEN ABSCHNITTEN (1=VORLAND 2=FLUSS 3=VORLAND)
      INTEGER :: POLYZAHL                              ! ZAEHLER ÜBER DIE POLYNOMFUNKTIONEN
      INTEGER :: SPLINEZAHL                            ! ZAEHLER ÜBER DIE SPLINEFUNKTIONEN
      INTEGER :: istat                                 ! Zaehlernummer fuer Zeilen
      INTEGER :: POLY1(0:maxprf)                 ! ZAEHLER fuer Q A stuetzstellen je Profil bis inkl. Bordvollem Wasserstand
      INTEGER :: POLY2(0:maxprf)                 ! ZAEHLER fuer Q A stuetzstellen je Profil ueber Bordvollem Wasserstand
      INTEGER :: POLYQ1(0:maxprf)                ! ZAEHLER fuer Q-stuetzstellen je Profil bis Bordvollem Wasserstand
      INTEGER :: POLYQ2(0:maxprf)                ! ZAEHLER fuer Q-stuetzstellen je Profil ueber Bordvollem Wasserstand
      INTEGER :: POLYA1(0:maxprf)                ! ZAEHLER fuer A-stuetzstellen je Profil bis Bordvollem Wasserstand
      INTEGER :: POLYA2(0:maxprf)                ! ZAEHLER fuer A-stuetzstellen je Profil ueber Bordvollem Wasserstand
      INTEGER :: POLYALP1(0:maxprf)              ! ZAEHLER fuer alpha-stuetzstellen je Profil bis Bordvollem Wasserstand
      INTEGER :: POLYALP2(0:maxprf)              ! ZAEHLER fuer alpha-stuetzstellen je Profil ueber Bordvollem Wasserstand
      INTEGER :: POLYGRAD(0:maxprf)              ! GRAD DES POLYNOMS
      INTEGER :: GRAD_Q1(0:maxprf)               ! GRAD DES POLYNOMS Q1
      INTEGER :: GRAD_Q2(0:maxprf)               ! GRAD DES POLYNOMS Q2
      INTEGER :: GRAD_Q3(0:maxprf)               ! GRAD DES POLYNOMS Q3 = 3
      INTEGER :: GRAD_A1(0:maxprf)               ! GRAD DES POLYNOMS A1
      INTEGER :: GRAD_A2(0:maxprf)               ! GRAD DES POLYNOMS A2
      INTEGER :: GRAD_A3(0:maxprf)               ! GRAD DES POLYNOMS A3 = 3
      INTEGER :: GRAD_ALPHA1(0:maxprf)           ! GRAD DES POLYNOMS ALPHA1
      INTEGER :: GRAD_ALPHA2(0:maxprf)           ! GRAD DES POLYNOMS ALPHA2
      INTEGER :: GRAD_ALPHA3(0:maxprf)           ! GRAD DES POLYNOMS ALPHA3 = 3
      INTEGER :: GRENZMAX(0:maxprf)              ! MAXIMALE GRENZE DER STUETZSTELLEN FUER Q UND A
      INTEGER :: ALPHA_END(0:maxprf)             ! Nummer der Stützstelle bei der alpha > 1,07
      INTEGER :: DATEIZAEHLER                    ! NUMMER DER DATEI DIE JE PROFIL ANGELEGT WIRD

!**   REALANWEISUNGEN UND GEOMETRIE
!**   ---------------------------------------------------------------------
      REAL (KIND=8)::STARTPROF                  ! PROFILNUMMER DAS ERSTE PROFIL FUER DAS POLYNOME GENERIERT WERDEN SOLLEN
      REAL (KIND=8)::ENDPROF                    ! PROFILNUMMER DAS LETZTE PROFIL FUER DAS POLYNOME GENERIERT WERDEN SOLLEN
      REAL (KIND=8)::IMPULS                     ! Vorgegebener Grenz-Impulsstrombeiwert aus der INI-Datei
      REAL (KIND=8)::DQ_LIMIT                   ! Vorgegebener Q-Grenz-Steigungswert aus der INI-Datei
      REAL (KIND=8)::DA_LIMIT                   ! Vorgegebener A-Grenz-Steigungswert aus der INI-Datei
      REAL (KIND=8)::DALP_LIMIT                 ! Vorgegebener ALPHA-Grenz-Steigungswert aus der INI-Datei
      REAL (KIND=8)::WFAKTOR                    ! Vorgegebener Wichtungsfaktor aus der INI-Datei

      REAL (KIND=8)::PROFIL(0:maxwert)          ! PROFILNUMMER AUS DEM lAENGSSCHNITT
      REAL (KIND=8)::ABFLUSS(0:maxwert)         ! AbFLUSS AUS DEM lAENGSSCHNITT
      REAL (KIND=8)::SOHLE(0:maxwert)           ! TIEFSTER SOHLPUNKT AUS DEM lAENGSSCHNITT
      REAL (KIND=8)::H_BORD(0:maxwert)          ! BORDVOLLER WASSERSTAND mNN AUS DEM lAENGSSCHNITT

      REAL (KIND=8)::TIEFE(0:maxprf,0:maxprf)    ! WASSERTIEFE JE PROFIL UND ABFLUSS
      REAL (KIND=8)::SOHLPROF(0:maxprf)          ! TIEFSTER SOHLPUNKT JE pROFIL
      REAL (KIND=8)::WSP_BORD(0:maxprf)          ! BORDVOLLER WASSERSTAND JE pROFIL
      REAL (KIND=8)::H_BORDVOLL(0:maxprf)        ! BoRDVOLLE WASSERTIEFE JE pROFIL
      REAL (KIND=8)::HQ_BORDVOLL(0:maxprf)       ! Automatisch erkannte BoRDVOLLE WASSERTIEFE fuer Q JE pROFIL
      REAL (KIND=8)::HA_BORDVOLL(0:maxprf)       ! Automatisch erkannte BoRDVOLLE WASSERTIEFE fuer A JE pROFIL
      REAL (KIND=8)::HALP_BORDVOLL(0:maxprf)     ! Automatisch erkannte BoRDVOLLE WASSERTIEFE fuer alpha JE pROFIL
      REAL (KIND=8)::PROF(0:maxprf,0:maxprf)     ! PROFILNUMMER (PROFILZAEHLER; ABFLUSSZAEHLER)
      REAL (KIND=8)::Q_WERT(0:maxprf)            ! ABFRAGE FUER STEIGERUNG DES ABFLUSSES UM EINEN ZAEHLER
      REAL (KIND=8)::UNTERGRENZE                 ! UNTERE GRENZWERT FUER ABFLUSS z.b.: 0.5 m3/s
      REAL (KIND=8)::OBERGRENZE                  ! OBERE GRENZWERT FUER ABFLUSS z.b.: 400 m3/s
      REAL (KIND=8)::WSP(0:maxprf,0:maxprf)      ! WASSERSPIEGELHOEHE IN mNN
      REAL (KIND=8)::HEN(0:maxprf,0:maxprf)      ! ENERGIEHOEHE IN mNN

      REAL (KIND=8)::FL1(0:maxprf,0:maxprf)        ! DURCHSTROEMTE Teil-FLAECHE Links IN QM
      REAL (KIND=8)::FL2(0:maxprf,0:maxprf)        ! DURCHSTROEMTE Teil-FLAECHE Fluss IN QM
      REAL (KIND=8)::FL3(0:maxprf,0:maxprf)        ! DURCHSTROEMTE Teil-FLAECHE rechts IN QM
      REAL (KIND=8)::FL_QM(0:maxprf,0:maxprf)      ! GESAMTE DURCHSTROEMTE FLAECHE IN QM
      REAL (KIND=8)::TAU(0:maxprf,0:maxprf)        ! Schubspannung IN N/qm
      REAL (KIND=8)::V(0:maxprf,0:maxprf)          ! MITTLERE FLIESSGESCHWINDIGKEIT IN M/S
      REAL (KIND=8)::Q(0:maxprf,0:maxprf)          ! GESAMTABFLUSS IN M3/S
      REAL (KIND=8)::ALP_E(0:maxprf,0:maxprf)      ! IMPULSSTROMBEIWERT
      REAL (KIND=8)::ALP_I(0:maxprf,0:maxprf)      ! IMPULSSTROMBEIWERT
      REAL (KIND=8)::FROUDE(0:maxprf,0:maxprf)     ! FROUDE-ZAHL = V / SQRT(g*h)
      REAL (KIND=8)::SLOPE(0:maxprf,0:maxprf)      ! STATIONAER GLEICHFOERMIGES GEFAELLE

      REAL (KIND=8)::DQ1(0:maxprf,0:maxprf)        ! Steigung zwischen zwei Stützstellen Q-Wasserstand
      REAL (KIND=8)::DA1(0:maxprf,0:maxprf)        ! Steigung zwischen zwei Stützstellen A-Wasserstand
      REAL (KIND=8)::DALP1(0:maxprf,0:maxprf)      ! Steigung zwischen zwei Stützstellen alpha-Wasserstand
      REAL (KIND=8)::DQ(0:maxprf,0:maxprf)         ! Quotient aus Steigungen Q-Wasserstand
      REAL (KIND=8)::DA(0:maxprf,0:maxprf)         ! Quotient aus Steigungen A-Wasserstand
      REAL (KIND=8)::DALP(0:maxprf,0:maxprf)       ! Quotient aus Steigungen alpha-Wasserstand

      REAL (KIND=8)::H_n(0:maxprf,0:maxprf)        ! Umsortierung des Wasserstaende mit entfernten Ausreissern
      REAL (KIND=8)::H_n_akt(0:maxprf)             ! aktuelle Wassertiefe fuer die Ausreisser-analyse
      REAL (KIND=8)::Ha_n(0:maxprf,0:maxprf)       ! Umsortierung des Wasserstaende mit entfernten Ausreissern fuer alpha
      REAL (KIND=8)::Ha_n_akt(0:maxprf)            ! aktuelle Wassertiefe fuer die Ausreisser-analyse
      REAL (KIND=8)::Q_n(0:maxprf,0:maxprf)        ! Umsortierung der Abfluesse mit entfernten Ausreissern
      REAL (KIND=8)::A_n(0:maxprf,0:maxprf)        ! Umsortierung der Abfluesse mit entfernten Ausreissern
      REAL (KIND=8)::alp_n(0:maxprf,0:maxprf)      ! Umsortierung der Abfluesse mit entfernten Ausreissern

      REAL (KIND=8)::ALPHA(0:4)                   ! PARAMETER DER POLYNOMFUNKTION
      REAL (KIND=8)::KOEF_Q1(0:maxprf,0:4)        ! PARAMETER DER POLYNOMFUNKTION 1
      REAL (KIND=8)::KOEF_Q2(0:maxprf,0:4)        ! PARAMETER DER POLYNOMFUNKTION 2
      REAL (KIND=8)::KOEF_Q3(0:maxprf,0:4)        ! PARAMETER DER SPLINEFUNKTION
      REAL (KIND=8)::KOEF_A1(0:maxprf,0:4)        ! PARAMETER DER POLYNOMFUNKTION 1
      REAL (KIND=8)::KOEF_A2(0:maxprf,0:4)        ! PARAMETER DER POLYNOMFUNKTION 2
      REAL (KIND=8)::KOEF_A3(0:maxprf,0:4)        ! PARAMETER DER SPLINEFUNKTION
      REAL (KIND=8)::KOEF_ALPHA1(0:maxprf,0:4)    ! PARAMETER DER POLYNOMFUNKTION 1 = 1.00000
      REAL (KIND=8)::KOEF_ALPHA2(0:maxprf,0:4)    ! PARAMETER DER POLYNOMFUNKTION 2
      REAL (KIND=8)::KOEF_ALPHA3(0:maxprf,0:4)    ! PARAMETER DER SPLINEFUNKTION
      REAL (KIND=8)::POLY_Q1(0:maxprf,0:maxprf)     ! POLYNOMWERT ZUM ABFLUSS AUS Q1(H) in M3/S bis bordvoll
      REAL (KIND=8)::POLY_Q2(0:maxprf,0:maxprf)     ! POLYNOMWERT ZUM ABFLUSS AUS Q2(H) in M3/S über bordvoll+1
      REAL (KIND=8)::POLY_Q3(0:maxprf,0:maxprf)     ! SPLINEWERT ZUM ABFLUSS AUS Q2(H) in M3/S zwischen bis bordvoll und bordvoll+1
      REAL (KIND=8)::POLY_A1(0:maxprf,0:maxprf)     ! POLYNOMWERT ZUR DURCHSTROEMTE FLAECHE AUS A1(H) in qm bis bordvoll
      REAL (KIND=8)::POLY_A2(0:maxprf,0:maxprf)     ! POLYNOMWERT ZUR DURCHSTROEMTE FLAECHE AUS A2(H) in qm über bordvoll+1
      REAL (KIND=8)::POLY_A3(0:maxprf,0:maxprf)     ! SPLINEWERT ZUM DURCHSTROEMTE FLAECHE AUS A3(H) in M3/S zwischen bis bordvoll und bordvoll+1
      REAL (KIND=8)::POLY_ALP1(0:maxprf,0:maxprf)   ! IMPULSSTROMBEIWERT AUS ALP(H) = 1
      REAL (KIND=8)::POLY_ALP2(0:maxprf,0:maxprf)   ! IMPULSSTROMBEIWERT AUS ALP(H) > 1
      REAL (KIND=8)::POLY_ALP3(0:maxprf,0:maxprf)   ! IMPULSSTROMBEIWERT AUS ALP(H) zwischen 1 und ein Wert drüber

      REAL (KIND=8)::ABLEIT_Q1(0:maxprf,0:maxprf)   ! Ableitung der Funktion Q1(H) an der Stelle bordvoll
      REAL (KIND=8)::ABLEIT_Q2(0:maxprf,0:maxprf)   ! Ableitung der Funktion Q2(H) an der Stelle bordvoll+1
      REAL (KIND=8)::ABLEIT_A1(0:maxprf,0:maxprf)   ! Ableitung der Funktion A1(H) an der Stelle bordvoll
      REAL (KIND=8)::ABLEIT_A2(0:maxprf,0:maxprf)   ! Ableitung der Funktion A2(H) an der Stelle bordvoll+1
      REAL (KIND=8)::ABLEIT_ALP1(0:maxprf,0:maxprf) ! Ableitung der Funktion alpha1(H) an der Stelle alpha = 1
      REAL (KIND=8)::ABLEIT_ALP2(0:maxprf,0:maxprf) ! Ableitung der Funktion alpha2(H) an der Stelle alpha > 1
      REAL (KIND=8)::ABL2_Q1(0:maxprf,0:maxprf)     ! Ableitung der Funktion Q1(H) an der Stelle bordvoll
      REAL (KIND=8)::ABL2_Q2(0:maxprf,0:maxprf)     ! Ableitung der Funktion Q2(H) an der Stelle bordvoll+1
      REAL (KIND=8)::ABL2_A1(0:maxprf,0:maxprf)     ! Ableitung der Funktion A1(H) an der Stelle bordvoll
      REAL (KIND=8)::ABL2_A2(0:maxprf,0:maxprf)     ! Ableitung der Funktion A2(H) an der Stelle bordvoll+1
      REAL (KIND=8)::ABL2_ALP1(0:maxprf,0:maxprf)   ! Ableitung der Funktion alpha1(H) an der Stelle alpha = 1
      REAL (KIND=8)::ABL2_ALP2(0:maxprf,0:maxprf)   ! Ableitung der Funktion alpha1(H) an der Stelle alpha > 1

      REAL (KIND=8)::DELT_Q(0:maxprf,0:maxprf)      ! DELTA GESAMTABFLUSS AUS Q(H) IN M3/S
      REAL (KIND=8)::DELT_A(0:maxprf,0:maxprf)      ! DELTA DURCHSTROEMTE FLAECHE  AUS A(H) IN M2
      REAL (KIND=8)::DELT_ALP(0:maxprf,0:maxprf)    ! DELTA IMPULSSTROMBEIWERT  AUS ALP(H)
      REAL (KIND=8)::ABW_Q(0:maxprf,0:maxprf)       ! DELTA^2 GESAMTABFLUSS AUS Q(H) IN M3/S
      REAL (KIND=8)::ABW_A(0:maxprf,0:maxprf)       ! DELTA^2 DURCHSTROEMTE FLAECHE  AUS A(H) IN M2
      REAL (KIND=8)::ABW_ALP(0:maxprf,0:maxprf)     ! DELTA^2 IMPULSSTROMBEIWERT  AUS ALP(H)
      REAL (KIND=8)::MAXI_Q(0:maxprf)             ! MAXIMALE ABWEICHUNG
      REAL (KIND=8)::MAXI_A(0:maxprf)             ! MAXIMALE ABWEICHUNG
      REAL (KIND=8)::MAXI_ALP(0:maxprf)           ! MAXIMALE ABWEICHUNG

      REAL (KIND=8)::X11, X12, X13, X14         ! komponenten der 1.Gleichung für die Splinefunktionen  F(Hbv)
      REAL (KIND=8)::X21, X22, X23, X24         ! komponenten der 2.Gleichung für die Splinefunktionen  F(Hbv+1)
      REAL (KIND=8)::X31, X32, X33, X34         ! komponenten der 3.Gleichung für die Splinefunktionen  F'(Hbv)
      REAL (KIND=8)::X41, X42, X43, X44         ! komponenten der 4.Gleichung für die Splinefunktionen  F'(Hbv+1)
      REAL (KIND=8)::X51, X52, X53, X54         ! komponenten der 5.Gleichung für die Splinefunktionen  F''(Hbv) = 0
      REAL (KIND=8)::X61, X62, X63, X64         ! komponenten der 5.Gleichung für die Splinefunktionen  F''(Hbv+1) = 0
      REAL (KIND=8)::V1, V2, V3, V4, V5, V6     ! Gleichungsloesungen für die Splinefunktionen
      REAL (KIND=8)::MATRIX(1:4,1:4)            ! Matrix für die Splinefunktionen
      REAL (KIND=8)::VEKTOR(1:4)                ! Lösungsvektor für die Splinefunktionen
      END MODULE !VARIABLEN

     
!*****************************************************************************************************
!**   PolynomFunktion PROGRAMM                                                                     ***
!**   VERSION 001                                                                                  ***
!*****************************************************************************************************

PROGRAM Polynomfunktion

USE VARIABLEN
IMPLICIT NONE

CALL INILESEN
CALL INICHECK
CALL WASSERTIEFE
CALL ZWISCHENAUS

IF (AUTOBORDV .EQ.'N') THEN
  CALL PUNKTANALYSE
ELSEIF (AUTOBORDV .EQ.'J') THEN
  CALL AUTOANALYSE
Endif

CALL POLYKERN
CALL SPLINES

IF (AUSREISSER .EQ.'N') THEN
  CALL POLYSORTIERT_1
ELSEIF (AUSREISSER .EQ.'J') THEN
  CALL POLYSORTIERT_2
ENDIF

CALL DELTAS
CALL ABWEICHUNG
IF (AUSREISSER .EQ. 'J') THEN
  CALL AUSREISS
ENDIF
CALL PROFILAUSGABE

END PROGRAM            ! ENDE HAUPTPROGRAMM

!***************************************************************************************************
!**   SUBROUTINE INILESEN                                                                        ***
!**   ----------------------                                                                     ***
!**   EINLESEN DER STEUERDATEN AUS DER INIDATEI                                                  ***
!***************************************************************************************************

SUBROUTINE INILESEN

USE VARIABLEN
IMPLICIT NONE

write (*,*) ' -----------------------------------------------------'
write (*,*) ' *          Version 0.2.9 zur Polynomerzeugung        '
write (*,*) ' *                                                    '
write (*,*) ' *          Stand: 12.06.2008                         '
write (*,*) ' -----------------------------------------------------'


OPEN (UNIT=1, FILE='steuerpoly.ini', STATUS='OLD', ACTION='READ', IOSTAT=istat) ! EINLESEN der ini-datei
IF (istat /= 0) THEN   ! wenn datei nicht gelesen werden konnte, dann istat
  write (*,*) ' Fehler beim Oeffnen der Datei: steuerpoly.ini'
  write (*,*) ' (Hilfe: Ist die Datei wirklich vorhanden?)'
  write (*,*) ' Programm wird beendet...'
  stop
end if
REWIND(1)

READ(UNIT=1,*)
READ(UNIT=1,*) ! ersten 2 Zeilen ueberspringen

INIZ = 0
DO
  READ (UNIT=1,'(A2)',IOSTAT=istat) MARKER(INIZ)
  if (istat < 0 ) exit   ! Check for end of data
  IF (MARKER(INIZ) .EQ. '02') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(22X,A100)',IOSTAT=istat) QLANGSSCHNITT
  ELSEIF (MARKER(INIZ) .EQ. '03') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(19X,I1)',IOSTAT=istat) WASGRAD
  ELSEIF (MARKER(INIZ) .EQ. '04') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(17X,A1)',IOSTAT=istat) WASTUN
  ELSEIF (MARKER(INIZ) .EQ. '05') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(19X,A1)',IOSTAT=istat) ABFRAGE
  ELSEIF (MARKER(INIZ) .EQ. '06') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(22X,A1)',IOSTAT=istat) PROFINTERVALL
  ELSEIF (MARKER(INIZ) .EQ. '07') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(24X,F9.4)',IOSTAT=istat) STARTPROF
  ELSEIF (MARKER(INIZ) .EQ. '08') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(22X,F9.4)',IOSTAT=istat) ENDPROF
  ELSEIF (MARKER(INIZ) .EQ. '09') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(26X,A1)',IOSTAT=istat) AUSGFUNKT
  ELSEIF (MARKER(INIZ) .EQ. '10') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(24X,A1)',IOSTAT=istat) WSPWERTE
  ELSEIF (MARKER(INIZ) .EQ. '11') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(25X,A1)',IOSTAT=istat) KONTROLLE
  ELSEIF (MARKER(INIZ) .EQ. '12') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(21X,A100)',IOSTAT=istat) AUSGABEPFAD
  ELSEIF (MARKER(INIZ) .EQ. '13') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(19X,A1)',IOSTAT=istat) AUSREISSER
  ELSEIF (MARKER(INIZ) .EQ. '14') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(24X,F7.4)',IOSTAT=istat) IMPULS
  ELSEIF (MARKER(INIZ) .EQ. '15') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(21X,A1)',IOSTAT=istat) AUTOBORDV
  ELSEIF (MARKER(INIZ) .EQ. '16') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(23X,F7.4)',IOSTAT=istat) DQ_LIMIT
  ELSEIF (MARKER(INIZ) .EQ. '17') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(23X,F7.4)',IOSTAT=istat) DA_LIMIT
  ELSEIF (MARKER(INIZ) .EQ. '18') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(25X,F7.4)',IOSTAT=istat) DALP_LIMIT
  ELSEIF (MARKER(INIZ) .EQ. '19') THEN
    BACKSPACE(UNIT=1)
    READ (UNIT=1,'(30X,F7.2)',IOSTAT=istat) WFAKTOR
  ENDIF
  INIZ = INIZ + 1
END DO
CLOSE (1)

TEXTLGSS = LEN_TRIM (QLANGSSCHNITT)
TEXTAUSG = LEN_TRIM (AUSGABEPFAD)

TEXT12 = TEXTAUSG + 12        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE AUSWSPWERTE
TEXT13 = TEXTAUSG + 14        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE AUSSOHLE
TEXT14 = TEXTAUSG + 18        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE AUSQ_FUNKT
TEXT15 = TEXTAUSG + 18        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE AUSA_FUNKT
TEXT16 = TEXTAUSG + 22        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE AUSALPHA_FUNKT
TEXT17 = TEXTAUSG + 12        ! PFAD DER DATEI MIT RICHTIGER LAENGE  AUSPOLYNOME
TEXT18 = TEXTAUSG + 13        ! PFAD DER DATEI MIT RICHTIGER LAENGE  AUSKONTROLL
TEXT19 = TEXTAUSG + 4         ! PFAD DER DATEI MIT RICHTIGER LAENGE  AUSPROF
TEXT20 = TEXTAUSG + 17        ! PFAD DER DATEI MIT RICHTIGER LAENGE  DATEINAME(0:2000)

END SUBROUTINE !SUB INILESEN


!***************************************************************************************************
!**   SUBROUTINE INICHECK                                                                        ***
!**   ----------------------                                                                     ***
!**   PRUEFEN DER STEUERDATEN AUS DER INIDATEI                                                   ***
!***************************************************************************************************
SUBROUTINE INICHECK

USE VARIABLEN
IMPLICIT NONE

CHARACTER (LEN=TEXTLGSS) LAENGS     ! PFAD DER Q-LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE


IF (TEXTLGSS .EQ. 0) THEN
  write (*,*) ' Fehler lesen des Pfades der Q_Langsschnitt-Datei'
  WRITE (*,*) ' '
  stop
END IF
IF (TEXTAUSG .EQ. 0) THEN
  write (*,*) ' Fehler lesen des AUSGABE-Pfades'
  write (*,*) ' Die Ergbenisse werden automatisch in den Ordner der exe geschrieben'
  WRITE (*,*) ' '
END IF

LAENGS = QLANGSSCHNITT

IF (WASGRAD .LT. 2 .OR. WASGRAD .GT. 4) THEN
  WRITE(*,*) ' Ihre Angabe fuer den Polynomgrad war falsch !!!!!'
  WRITE(*,*) ' Es wird automtisch ein Polynomgrad von 4 angesetzt.'
  WRITE(*,*) ' '
  WASGRAD = 4
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer 3-Teilung der Polynome
! -----------------------------------------------------------------------------
IF (WASTUN .eq. 'j') THEN
  WASTUN = 'J'
ELSEIF (WASTUN .eq. 'n') THEN
  WASTUN = 'N'
end if

IF (WASTUN .NE. 'J' .AND. WASTUN .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die 3-Teilung der Polynome war falsch !!!!!'
  WRITE(*,*) ' Es wird automtisch der einfache Ansatz gewaehlt !!!'
  WRITE(*,*) ' '
  WASTUN = 'N'
ELSEIF (WASTUN .EQ. 'J') THEN
  WRITE(*,*) ' Sie wollen 3-teilige Polynome fuer A(h) und Q(h) erzeugen'
  WRITE(*,*) ' '
ELSEIF (WASTUN .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen KEINE 3-teilige Polynome fuer A(h) und Q(h) erzeugen'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer Reduzierung des Polynomgrades
! -------------------------------------------------------------------------------------
IF (ABFRAGE .eq. 'j') THEN
  ABFRAGE = 'J'
ELSEIF (ABFRAGE .eq. 'n') THEN
  ABFRAGE = 'N'
end if

IF (ABFRAGE .NE. 'J' .AND. ABFRAGE .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die Reduzierung des Polynomgrades war falsch!!!!!'
  WRITE(*,*) ' Bei Problemen waehrend der Koeffizientenbestimmung wird der Polynomgrad automtisch je um eins reduziert'
  WRITE(*,*) ' '
  ABFRAGE = 'J'
ELSEIF (ABFRAGE .EQ. 'J') THEN
  WRITE(*,*) ' Sie wollen bei Problemen waehrend der Koeffizientenbestimmung den Polynomgrad je um eins reduzieren'
  WRITE(*,*) ' '
ELSEIF (ABFRAGE .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen bei Problemen waehrend der Koeffizientenbestimmung das Polynom nicht weiter erzeugen'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer das Profilintervall
! -------------------------------------------------------------------------------------
IF (PROFINTERVALL .eq. 'j') THEN
  PROFINTERVALL = 'J'
ELSEIF (PROFINTERVALL .eq. 'n') THEN
  PROFINTERVALL = 'N'
end if

IF (PROFINTERVALL .NE. 'J' .AND. PROFINTERVALL .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe (J/N) fuer das Profilintervall war falsch !!!!!'
  WRITE(*,*) ' Es werden automtisch keine Einschraenkungen angenommen !!!'
  WRITE(*,*) ' '
  PROFINTERVALL = 'N'
ELSEIF (PROFINTERVALL .EQ. 'J') THEN
  if (STARTPROF .GE. 0.0D0 .AND. STARTPROF .LE. ENDPROF) then
    WRITE(*,*) ' Sie wollen die Polynome von Profil',STARTPROF,' bis Profil ',ENDPROF,' erzeugen'
    WRITE(*,*) ' '
  elseif (STARTPROF .GT. ENDPROF) then
    WRITE(*,*) ' Fehler: Ihre Profil-Angabe fuer das Profilintervall war falsch !!!!!'
    WRITE(*,*) ' Ihr Start-Profil',STARTPROF,' ist GROESSER als das End-Profil ',ENDPROF,'!!'
    WRITE(*,*) ' '
    stop
  elseif (STARTPROF .LT. 0.0D0) then
    WRITE(*,*) ' Fehler: Ihre Profil-Angabe fuer das Profilintervall war falsch !!!!!'
    WRITE(*,*) ' Ihr Start-Profil ',STARTPROF,' ist NEGATIV!!'
    WRITE(*,*) ' '
    stop
  endif
ELSEIF (PROFINTERVALL .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen die Polynome fuer allen verfuegbaren Profil erzeugen'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Grenze des Impulstrombeiwert
! -------------------------------------------------------------------------------------

if (IMPULS .GE. 1.002D0 .AND. IMPULS.LT.5.0D0) then
  WRITE(*,*) ' Sie haben den maximal zulaessigen Impulsbeiwert fuer die alpha-Polynom-Erzeugung '
  WRITE(*,*) ' mit',IMPULS,' vorgegeben.'
  WRITE(*,*) ' '
ELSEIF (IMPULS .GE. 5.0D0 .AND. IMPULS.LT.100.0D0) then
  WRITE(*,*) ' WARNUNG: !!!!!!!!!!!!!!!! '
  WRITE(*,*) ' Der gewaehlte maximal zulaessige Impulsbeiwert liegt außerhalb des Erfahrungsbereiches: '
  WRITE(*,*) ' Sie haben den maximal zulaessigen Impulsbeiwert mit',IMPULS,' vorgegeben.'
  WRITE(*,*) ' '
  WRITE(*,*) ' Hinweis:'
  WRITE(*,*) '   maxi Impulsbeiwert fuer Kanalquerschnitte: 1.03 bis 1.07 '
  WRITE(*,*) '   maxi Impulsbeiwert fuer natuerliche Querschnitte: 1.05 bis 1.17 '
  WRITE(*,*) '   maxi Impulsbeiwert fuer weite Auen: 1.17 bis 1.33 '
  WRITE(*,*) ' '
ELSE
  WRITE(*,*) ' Sie haben KEINEN maximal zulaessigen Impulsbeiwert fuer die alpha-Polynome-Erzeugung '
  WRITE(*,*) ' vorgegeben. Die Grenze wird automatisch auf 1.17 festgelegt'
  IMPULS = 1.17
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer die Einzel-Ausgabe der Funktionen
! -------------------------------------------------------------------------------------
IF (AUSGFUNKT .eq. 'j') THEN
  AUSGFUNKT = 'J'
ELSEIF (AUSGFUNKT .eq. 'n') THEN
  AUSGFUNKT = 'N'
end if

IF (AUSGFUNKT .NE. 'J' .AND. AUSGFUNKT .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die Einzel-Ausgabe der Polynom-Funktionen war falsch!!!!!'
  WRITE(*,*) ' Die Einzel-Ausgaben der Funktionen werden erzeugt'
  WRITE(*,*) ' '
  AUSGFUNKT = 'J'
ELSEIF (AUSGFUNKT .EQ. 'J') THEN
  WRITE(*,*) ' Sie wollen eine Einzel-Ausgabe der Polynom-Funktionen'
  WRITE(*,*) ' '
ELSEIF (AUSGFUNKT .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen KEINE Einzel-Ausgabe der Polynom-Funktionen'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer die Korrektur der AUSREISSER
! -------------------------------------------------------------------------------------
IF (AUSREISSER .eq. 'j') THEN
  AUSREISSER = 'J'
ELSEIF (AUSREISSER .eq. 'n') THEN
  AUSREISSER = 'N'
end if

IF (AUSREISSER .NE. 'J' .AND. AUSREISSER .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die Korrektur der Ausreisser war falsch!!!!!'
  WRITE(*,*) ' Die Ausreisser der Wertepaare für die Funktionen werden nicht entfernt'
  WRITE(*,*) ' '
  AUSREISSER = 'N'
ELSEIF (AUSREISSER .EQ. 'J') THEN
  WRITE(*,*) ' Die Ausreisser der Wertepaare für die Funktionen werden entfernt'
  WRITE(*,*) ' '
ELSEIF (AUSREISSER .EQ. 'N') THEN
  WRITE(*,*) ' Die Ausreisser der Wertepaare für die Funktionen werden nicht entfernt'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer die Ausgabe der eingelesenen Ergebnisse aus WSPM
! --------------------------------------------------------------------------------------------------------
IF (WSPWERTE .eq. 'j') THEN
  WSPWERTE = 'J'
ELSEIF (WSPWERTE .eq. 'n') THEN
  WSPWERTE = 'N'
end if

IF (WSPWERTE .NE. 'J' .AND. WSPWERTE .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die Ausgabe der eingelesenen Ergebnisse aus WSPM war falsch!!!!!'
  WRITE(*,*) ' Die Ausgabe der eingelesenen Ergebnisse aus WSPM werden erzeugt'
  WRITE(*,*) ' '
  WSPWERTE = 'J'
ELSEIF (WSPWERTE .EQ. 'J') THEN
  WRITE(*,*) ' Sie wollen eine Ausgabe der eingelesenen Ergebnisse aus WSPM'
  WRITE(*,*) ' '
ELSEIF (WSPWERTE .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen KEINE Ausgabe der eingelesenen Ergebnisse aus WSPM'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben fuer die Ausgabe der Differenzen zwischen Stuetzstelle und Polynom
! ---------------------------------------------------------------------------------------------------------------------
IF (KONTROLLE .eq. 'j') THEN
  KONTROLLE = 'J'
ELSEIF (KONTROLLE .eq. 'n') THEN
  KONTROLLE = 'N'
end if

IF (KONTROLLE .NE. 'J' .AND. KONTROLLE .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die Ausgabe der Differenzen zwischen Stuetzstelle und Polynom war falsch!!!!!'
  WRITE(*,*) ' Die Ausgabe der Differenzen zwischen Stuetzstelle und Polynom werden erzeugt'
  WRITE(*,*) ' '
  KONTROLLE = 'J'
ELSEIF (KONTROLLE .EQ. 'J') THEN
  WRITE(*,*) ' Sie wollen eine Ausgabe der Differenzen zwischen Stuetzstelle und Polynom'
  WRITE(*,*) ' '
ELSEIF (KONTROLLE .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen KEINE Ausgabe der Differenzen zwischen Stuetzstelle und Polynom'
  WRITE(*,*) ' '
END IF


! Korrektur fuer Kleinschreibung in Grossbuchstaben automatische Erkennung Steigungsaenderung
! --------------------------------------------------------------------------------------------
IF (AUTOBORDV .eq. 'j') THEN
  AUTOBORDV = 'J'
ELSEIF (AUTOBORDV .eq. 'n') THEN
  AUTOBORDV = 'N'
end if

IF (AUTOBORDV .NE. 'J' .AND. AUTOBORDV .NE. 'N') THEN
  WRITE(*,*) ' Ihre Angabe fuer die automatische Erkennung der Bordvollpunkte '
  WRITE(*,*) ' ueber Steigungsaenderung fuer die Polynome war falsch !!!!!'
  WRITE(*,*) ' Es werden automtisch die ggbn Bordvollpunkte fuer die 2Teilung gewaehlt !!'
  WRITE(*,*) ' '
  AUTOBORDV = 'N'
ELSEIF (AUTOBORDV .EQ. 'J') THEN
  WRITE(*,*) ' Sie wollen eine automatische Erkennung der Bordvollpunkte verwenden'
  WRITE(*,*) ' '
ELSEIF (AUTOBORDV .EQ. 'N') THEN
  WRITE(*,*) ' Sie wollen die ggbn Bordvollpunkte fuer die Polynome verwenden'
  WRITE(*,*) ' '
END IF

! Korrektur fuer Grenze der Steigungsaenderungen
! --------------------------------------------------------------------------------------------
if (DQ_LIMIT .GE. 1.002D0 .AND. DQ_LIMIT.LT.100.0D0) then
  WRITE(*,*) ' Sie haben die Grenze fuer die maximal zulaessige Q-Steigungsaenderungen fuer die '
  WRITE(*,*) ' Q-Polynom-Erzeugung mit',DQ_LIMIT,' vorgegeben.'
  WRITE(*,*) ' '
ELSE
  WRITE(*,*) ' Sie haben KEINE zulaessige Q-Steigungsaenderungen fuer die Q-Polynom-Erzeugung '
  WRITE(*,*) ' vorgegeben. Der Werte muss groesser 1 sein!! '
  WRITE(*,*) ' Die Grenze wird automatisch auf 2.00 festgelegt!!!'
  DQ_LIMIT = 2.0
END IF

if (DA_LIMIT .GE. 1.002D0 .AND. DA_LIMIT.LT.100.0D0) then
  WRITE(*,*) ' Sie haben die Grenze fuer die maximal zulaessige A-Steigungsaenderungen fuer die '
  WRITE(*,*) ' Q-Polynom-Erzeugung mit',DA_LIMIT,' vorgegeben.'
  WRITE(*,*) ' '
ELSE
  WRITE(*,*) ' Sie haben KEINE zulaessige A-Steigungsaenderungen fuer die A-Polynom-Erzeugung '
  WRITE(*,*) ' vorgegeben. Der Werte muss groesser 1 sein!! '
  WRITE(*,*) ' Die Grenze wird automatisch auf 2.00 festgelegt!!!'
  DA_LIMIT = 2.0
END IF

if (DALP_LIMIT .GE. 1.002D0 .AND. DALP_LIMIT.LT.100.0D0) then
  WRITE(*,*) ' Sie haben die Grenze fuer die maximal zulaessige Alpha-Steigungsaenderungen fuer die '
  WRITE(*,*) ' Q-Polynom-Erzeugung mit',DALP_LIMIT,' vorgegeben.'
  WRITE(*,*) ' '
ELSE
  WRITE(*,*) ' Sie haben KEINE zulaessige alpha-Steigungsaenderungen fuer die alpha-Polynom-Erzeugung '
  WRITE(*,*) ' vorgegeben. Der Werte muss groesser 1 sein!! '
  WRITE(*,*) ' Die Grenze wird automatisch auf 2.00 festgelegt!!!'
  DALP_LIMIT = 2.0
END IF

! Korrektur fuer Angabe der Wichtungsfaktoren fuer die SplinePunkte
! --------------------------------------------------------------------------------------------
if (WFAKTOR .GE. 1.00D0 .AND. WFAKTOR.LT.10000.0D0) then
  WRITE(*,*) ' Sie haben den Wichtungsfaktor fuer die jeweiligen Splinepunkte bei der '
  WRITE(*,*) ' Polynom-Erzeugung mit',WFAKTOR,' vorgegeben.'
  WRITE(*,*) ' '
ELSE
  WRITE(*,*) ' Sie haben KEINEN zulaessigen Wichtungsfaktor fuer die Polynom-Erzeugung '
  WRITE(*,*) ' vorgegeben. Der Werte muss groesser gleich 1 sein!! '
  WRITE(*,*) ' Die Grenze wird automatisch auf 1.00 festgelegt!!!'
  WRITE(*,*) ' Info: Die Wichtung mit 1.00 = Einheitliche Wichtung'
  WFAKTOR = 1.0
END IF


!-------------------------------------------
! Aufruf der Einlese-routine

CALL EINLESEN (LAENGS)
!-------------------------------------------

END SUBROUTINE !SUB INILESEN

!***************************************************************************************************
!**   SUBROUTINE EINLESEN                                                                        ***
!**   ----------------------                                                                     ***
!**   EINLESEN DER PROFILDATEN                                                                   ***
!***************************************************************************************************
SUBROUTINE EINLESEN (LAENGS)

USE VARIABLEN
IMPLICIT NONE

CHARACTER (LEN=TEXTLGSS) LAENGS     ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE


OPEN (UNIT=10, FILE=LAENGS, STATUS='OLD', ACTION='READ', IOSTAT=istat) ! EINLESEN der Knoten
IF (istat /= 0) THEN   ! wenn datei nicht gelesen werden konnte, dann istat
  write (*,*) ' Fehler beim Oeffnen der Datei: 01Eingang\laengsschnitt.txt'
  write (*,*) ' (Hilfe: Ist die Datei wirklich vorhanden?)'
  write (*,*) ' Programm wird beendet...'
  stop
end if
REWIND(10)

READ(UNIT=10,*) ! ersten 2 Zeilen ueberspringen
READ(UNIT=10,*) ! ersten 2 Zeilen ueberspringen

YY = 0
NUMMER_1 = '000'
NUMMER_2 = '00'
NUMMER_3 = '0'
DO
 ! READ (UNIT=10,'(2X,F9.4,4X,A1,2X,F8.3,22X,F8.3)',IOSTAT=istat) PROFIL(YY),KENNUNG(YY), SOHLE(YY), H_BORD(YY)
 ! fuer alte Laengsschnitte aus WspWin

 ! fuer neue Laengsschnitte aus WSPM
  READ (UNIT=10,'(2X,F9.4,4X,A1,1X,F9.3,2X,F8.3,22X,F8.3)',IOSTAT=istat) PROFIL(YY),KENNUNG(YY),ABFLUSS(YY), SOHLE(YY), H_BORD(YY)

!  IF (YY.ne.0) THEN
!    IF (ABFLUSS(YY).ne.ABFLUSS(YY-1)) EXIT
!  END IF

  BACKSPACE(UNIT=10)
  IF (PROFIL(YY) .LT. 10.0D0) THEN
    READ (UNIT=10,'(5X,A6)',IOSTAT=istat) NAME_1
    if (istat < 0 ) exit   ! Check for end of data
    PROFILNAME(YY) =  NUMMER_1 // NAME_1
  ELSEIF (PROFIL(YY) .LE. 100.0D0 .and. PROFIL(YY) .GE. 10.0D0) THEN
    READ (UNIT=10,'(4X,A7)',IOSTAT=istat) NAME_2
    if (istat < 0 ) exit   ! Check for end of data
    PROFILNAME(YY) =  NUMMER_2 // NAME_2
  ELSEIF (PROFIL(YY) .LE. 1000.0D0 .and. PROFIL(YY) .GE. 100.0D0) THEN
    READ (UNIT=10,'(3X,A8)',IOSTAT=istat) NAME_3
    if (istat < 0 ) exit   ! Check for end of data
    PROFILNAME(YY) =  NUMMER_3 // NAME_3
  ELSEIF (PROFIL(YY) .GE. 1000.0D0) THEN
    READ (UNIT=10,'(2X,A9)',IOSTAT=istat) PROFILNAME(YY)
    if (istat < 0 ) exit   ! Check for end of data
  END IF

  if (KENNUNG(YY) .NE. 'i') then
    YY = YY+1
  end if
END DO
XMAX = YY-1
  write (*,*) ' das letzte ist Profil' ,PROFIL(XMAX), '.'
  write (*,*) ' es gibt' ,XMAX, 'zulaessige Profile.'
CLOSE (10)


!-------------------------- EINLESEN DER RELEVANTEN DATEN------------------------------------
!----------------------------------------------------------------------------------------

OPEN (UNIT=10, FILE=LAENGS, STATUS='OLD', ACTION='READ', IOSTAT=istat) ! EINLESEN der Knoten
IF (istat /= 0) THEN   ! wenn datei nicht gelesen werden konnte, dann istat
  write (*,*) ' Fehler beim Oeffnen der Datei: 01Eingang\laengsschnitt.txt'
  write (*,*) ' (Hilfe: Ist die Datei wirklich vorhanden?)'
  write (*,*) ' Programm wird beendet...'
  stop
end if
REWIND(10)

READ(UNIT=10,*) ! ersten 2 Zeilen ueberspringen
READ(UNIT=10,*) ! ersten 2 Zeilen ueberspringen


NN =0
M = 0
DO
  READ (UNIT=10,'(2X,F9.4,6X,F9.3,11X,F9.3,1X,F9.3,31X,F7.3,1X,F7.2,54X,3(F10.3),80X,3(F10.5))',IOSTAT=istat) &
       & PROF(NN,M),Q(NN,M),WSP(NN,M),HEN(NN,M),V(NN,M),TAU(NN,M),FL1(NN,M),FL2(NN,M),FL3(NN,M),ALP_E(NN,M),ALP_I(NN,M),SLOPE(NN,M)
  if (istat < 0 ) exit             ! Check for end of data

  ! Summation der Teilflaechen
  FL_QM(NN,M) = FL1(NN,M)+FL2(NN,M)+FL3(NN,M)

  write (*,*) ' Daten von profil-nr',PROF(NN,M),'bei Abfluss',Q(NN,M),'gelesen'

  IF (FL1(NN,M).NE.0) THEN
    ABSNUMMER(NN,M,1) = 1
  ELSE
    ABSNUMMER(NN,M,1) = 0
  ENDif

  IF (FL2(NN,M).NE.0) THEN
    ABSNUMMER(NN,M,2) = 2
  ELSEIF(FL2(NN,M).EQ.0) THEN
    ABSNUMMER(NN,M,2) = 0
    write (*,*) ' Keine Benetzte Flaeche im Hauptgerinne bei',PROF(NN,M),'unter Abfluss',Q(NN,M),'vorhanden'
    write (*,*) ' Bitte Prüfen Sie ihre Eingangsdaten in Q_langschnitt.txt'
  ENDif

  IF (FL3(NN,M).NE.0) THEN
    ABSNUMMER(NN,M,3) = 3
  ELSE
    ABSNUMMER(NN,M,3) = 0
  ENDif

  IF (M .EQ. 0) THEN                 ! NUR WEITERZAEHLEN, WENN IM PROFIL-ITERVALL
    UNTERGRENZE = Q(NN,M)
  ELSEIF (M .NE. 0) THEN
    OBERGRENZE = Q(NN,M)
  END IF


  IF (PROFINTERVALL .EQ. 'N') THEN
    YY = 0
    DO YY = 0, XMAX
      if (PROF(NN,M) .EQ. PROFIL(YY)) then
        SOHLPROF(NN) = SOHLE(YY)
        WSP_BORD(NN) = H_BORD(YY)
        PROFNAME(NN) = PROFILNAME(YY)
        KENN(NN) = KENNUNG(YY)
        IF (M .EQ. 0) then
          NN = NN + 1
        ELSEIF (M .GT. 0) then
          NN = NN + 1
        END IF
        NMAX = NN            ! PROFILZAHELER
      End if
    END DO
  ELSEIF (PROFINTERVALL.EQ.'J' .AND. PROF(NN,M).GE.STARTPROF .AND. PROF(NN,M).LE.ENDPROF) THEN
    YY = 0
    DO YY = 0, XMAX
      if (PROF(NN,M) .EQ. PROFIL(YY)) then
        SOHLPROF(NN) = SOHLE(YY)
        WSP_BORD(NN) = H_BORD(YY)
        PROFNAME(NN) = PROFILNAME(YY)
        KENN(NN) = KENNUNG(YY)
        IF (M .EQ. 0) then
          NN = NN + 1
        ELSEIF (M .GT. 0) then
          NN = NN + 1
        END IF
        NMAX = NN            ! PROFILZAHELER
      End if
    END DO
  ENDIF

  IF (PROFINTERVALL .EQ. 'N' .and. PROF((NN-1),M) .EQ. PROFIL(XMAX)) THEN
    MMAX = M      ! ABFLUSSZAHELER
    NMAX = NN - 1 ! Wieder einen zurückzaehlen
    write (*,*) ' Daten bei Abfluss',Q((NN-1),M),'gelesen ueber' ,NMAX, 'Profile'
    NN = 0
    M = M + 1
  ELSEIF (PROFINTERVALL .EQ. 'J' .AND. NN .GT. 0) THEN
    IF (PROF((NN-1),M) .EQ. PROFIL(XMAX) .AND. ENDPROF.NE.PROFIL(XMAX) .AND. PROF((NN-1),M).GE.STARTPROF) THEN
      MMAX = M       ! ABFLUSSZAHELER
      NMAX = NN - 1  ! Wieder einen zurückzaehlen
      write (*,*) ' Daten bei Abfluss',Q((NN-1),M),'gelesen ueber' ,NMAX, 'Profile'
      NN = 0
      M = M + 1
    ELSEIF (PROF((NN-1),M).EQ.ENDPROF .AND. ENDPROF.eq.PROFIL(XMAX) .AND. PROF((NN-1),M).GE.STARTPROF) THEN
      MMAX = M        ! ABFLUSSZAHELER
      NMAX = NN - 1   ! Wieder einen zurückzaehlen
      write (*,*) ' Daten bei Abfluss',Q((NN-1),M),'gelesen ueber' ,NMAX, 'Profile'
      NN = 0
      M = M + 1
    ENDIF
  END IF
END DO

CLOSE (UNIT=10)

if (Q(NMAX,M) .gt. 0.0) then
  MMAX = M
ELSEIF (Q(NMAX,(M-1)) .gt. 0.0) then
  MMAX = M-1
end if

write (*,*) ' AUSLESEN FUER ABFLUSS' ,OBERGRENZE, 'IST ABGESCHLOSSEN'
write (*,*) ' es gibt' ,NMAX, 'Profile'
write (*,*) ' mit je' ,MMAX, 'Abflussintervallen'
END SUBROUTINE !SUB EINLESEN



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   BERECHNUNG DER WASSERTIEFE BEZOGEN AUF DEN TIEFSTEN SOHLPUNKT                              ***
!***************************************************************************************************
SUBROUTINE WASSERTIEFE

USE VARIABLEN
IMPLICIT NONE

DO NN = 0, NMAX
  DO M = 0, MMAX
    TIEFE(NN,M) = WSP(NN,M) - SOHLPROF(NN)
    H_BORDVOLL(NN) = WSP_BORD(NN) - SOHLPROF(NN)
    FROUDE(NN,M) = V(NN,M) / (SQRT(9.81 * TIEFE(NN,M) ))

  !  IF (NN .le. NMAX) THEN
  !    I_EN(NN,M) = (HEN(NN,M) - HEN((NN+1),M)) / (( (PROF(NN,0) - PROF((NN+1),0)) * 1000)
  !    I_EN(NN,M) = SQRT (I_EN(NN,M) ** 2)
  !  END IF

  END DO
END DO

END SUBROUTINE !SUB WASSERTIEFE

!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   AUSGABE DER PROFILDATEN                                                                    ***
!***************************************************************************************************
SUBROUTINE ZWISCHENAUS
USE VARIABLEN
IMPLICIT NONE

CHARACTER (LEN=TEXTAUSG) AUSGABE       ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT12) AUSWSPWERTE     ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT13) AUSSOHLE        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE

AUSGABE = AUSGABEPFAD(1:TEXTAUSG)
AUSWSPWERTE = AUSGABE // 'WSPWERTE.txt'
AUSSOHLE    = AUSGABE // 'Sohlpunkte.txt'
!!  DATEINAME(NN) = '02Ausgang\PROF' // PROFNAME(NN,0)  // TXT

write (*,*) ' Es wurde folgender Ausgabe-Pfad definiert' ,AUSWSPWERTE , '.'
write (*,*) ' Es wurde folgender Ausgabe-Pfad definiert' ,AUSSOHLE , '.'

IF (WSPWERTE .eq. 'J') THEN         ! wird nur erzeugt, wenn in der ini-datei gewuenscht
  ! OPEN (12, FILE='02Ausgang\WSPWERTE.TXT', STATUS = 'replace')
  OPEN (12, FILE=AUSWSPWERTE, STATUS = 'replace')
  NN = 0
  M = 0
  DO NN = 0, NMAX      ! UEBER ALLE PROFILE
    WRITE(12,*)' '                                                                     ! ZEILE 1
    WRITE(12,*)' AUSGABEDATEI FUER DIE WERTEPAARE DES PROFILS', PROF(NN,0), '!!'
    WRITE(12,*)' Tiefster Sohlpunkt  ', SOHLPROF(NN),'in mNN'
    WRITE(12,*)' Bordvolle Wasserhöhe', WSP_BORD(NN),'in mNN'
    WRITE(12,*)' -----------------------------------------------------------------------------------'
    WRITE(12,*)'  WSP  mNN    TIEFE  m        A  qm   V  m/s     Q  m3/s   ALPHA [-]  SLOPE [-]  Fr [-]    Nr 1 - 2 - 3  '
    WRITE(12,*)' --***.****---***.****--*******.****--***.****--****.****--*.******---*.******---*.******--*************   '
    DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE
      WRITE(12,'(2X,F8.4,2X,F8.4,2X,F12.4,2X,F8.4,2X,F9.4,2X,F8.6,2X,F11.7,2X,F9.6,2X,I1,2X,I1,2X,I1)')  &
              & WSP(NN,M),TIEFE(NN,M),FL_QM(NN,M), V(NN,M),Q(NN,M),ALP_E(NN,M), SLOPE(NN,M),              &
              & FROUDE(NN,M),ABSNUMMER(NN,M,1),ABSNUMMER(NN,M,2),ABSNUMMER(NN,M,3)
    END DO
  END DO
  Close (12)
END IF

! OPEN (13, FILE='02Ausgang\Sohlpunkte.TXT', STATUS = 'replace')
OPEN (13, FILE=AUSSOHLE, STATUS = 'replace')
NN = 0
M = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  if (NN .eq.0) then
    WRITE(13,*)' '                                            ! ZEILE 1
    WRITE(13,*)'   PROFIL km    Bordvoll mNN  Sohlpunkt mNN  '
    WRITE(13,*)' --***.****-----*****.****----*****.****---  '
  end if
  WRITE(13,'(2X,F8.4,5X,F10.4,5X,F10.4)') PROF(NN,0), WSP_BORD(NN), SOHLPROF(NN)
END DO
Close (13)
END SUBROUTINE !SUB ZWISCHENAUS



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   Zerlegung in Teilpolynome je Profil fuer A(h) und Q(h)                                     ***
!***************************************************************************************************
SUBROUTINE PUNKTANALYSE
USE VARIABLEN
IMPLICIT NONE


NN = 0
M = 0
IF (WASTUN .EQ. 'J') THEN      ! Dreiteilige Polynome sollen erzeugt werden
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    POLYQ1(NN) = 0
    POLYQ2(NN) = 0
    POLYA1(NN) = 0
    POLYA2(NN) = 0
    Grenze = 0
    DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE

      IF (AUSREISSER .EQ. 'N') THEN
        IF (M .EQ. 0) then
          IF (TIEFE(NN,M) .LE. H_BORDVOLL(NN)) THEN
            POLYQ1(NN) = POLYQ1(NN) + 1
            POLYA1(NN) = POLYQ1(NN)
          ELSEIF (TIEFE(NN,M) .GT. H_BORDVOLL(NN)) THEN
            POLYQ2(NN) = POLYQ2(NN) + 1
            POLYA2(NN) = POLYQ2(NN)
          ENDIF

        ELSEIF (TIEFE(NN,M) .GT. TIEFE(NN,(M-1)) .AND. Grenze.eq.0) then
          IF (TIEFE(NN,M) .LE. H_BORDVOLL(NN) .and. POLYQ2(NN).eq.0) THEN
            POLYQ1(NN) = POLYQ1(NN) + 1
            POLYA1(NN) = POLYQ1(NN)
          ELSEIF (TIEFE(NN,M) .GT. H_BORDVOLL(NN)) THEN
            POLYQ2(NN) = POLYQ2(NN) + 1
            POLYA2(NN) = POLYQ2(NN)
          ENDIF
        ELSE
          Grenze = 1
        end if

      ELSEIF (AUSREISSER .EQ. 'J') THEN
        IF (POLYQ1(NN).ne.0 .and. POLYQ2(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))
        ELSEIF (POLYQ1(NN).eq.0 .and. POLYQ2(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))
        ELSEIF (POLYQ2(NN).eq.0 .and. POLYQ1(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))
        END IF

        IF (M .EQ. 0) then
          IF (TIEFE(NN,M) .LE. H_BORDVOLL(NN)) THEN
            H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
            Q_n(NN,POLYQ1(NN)) = Q(NN,M)
            A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
            MQQ_1(NN,M) = M
            POLYQ1(NN) = POLYQ1(NN) + 1
            MAA_1(NN,M) = MQQ_1(NN,M)
            POLYA1(NN)  = POLYQ1(NN)
          ELSEIF (TIEFE(NN,M) .GT. H_BORDVOLL(NN)) THEN
            H_n(NN,(POLYQ1(NN)+POLYQ2(NN))) = TIEFE(NN,M)
            Q_n(NN,(POLYQ1(NN)+POLYQ2(NN))) = Q(NN,M)
            A_n(NN,(POLYA1(NN)+POLYA2(NN))) = FL_QM(NN,M)
            MQQ_2(NN,M) = M
            POLYQ2(NN) = POLYQ2(NN) + 1
            MAA_2(NN,M) = MQQ_2(NN,M)
            POLYA2(NN)  = POLYQ2(NN)
          ENDIF
        ELSEIF (TIEFE(NN,M) .GT. H_n_akt(NN)) then
          IF (TIEFE(NN,M) .LE. H_BORDVOLL(NN) .and. POLYQ2(NN).eq.0) THEN
            H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
            Q_n(NN,POLYQ1(NN)) = Q(NN,M)
            A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
            MQQ_1(NN,M) = M
            POLYQ1(NN) = POLYQ1(NN) + 1
            MAA_1(NN,M) = MQQ_1(NN,M)
            POLYA1(NN)  = POLYQ1(NN)
          ELSEIF (TIEFE(NN,M) .GT. H_BORDVOLL(NN)) THEN
            H_n(NN,(POLYQ1(NN)+POLYQ2(NN))) = TIEFE(NN,M)
            Q_n(NN,(POLYQ1(NN)+POLYQ2(NN))) = Q(NN,M)
            A_n(NN,(POLYA1(NN)+POLYA2(NN))) = FL_QM(NN,M)
            MQQ_2(NN,M) = M
            POLYQ2(NN) = POLYQ2(NN) + 1
            MAA_2(NN,M) = MQQ_2(NN,M)
            POLYA2(NN)  = POLYQ2(NN)
          ENDIF
        end if
      ENDIF
    END DO
    if (POLYQ1(NN) .gt. 0) then
      POLYQ1(NN) =  POLYQ1(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
      POLYA1(NN) =  POLYA1(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
    END if
    if (POLYQ2(NN) .gt. 0) then
      POLYQ2(NN) =  POLYQ2(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
      POLYA2(NN) =  POLYA2(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
    END if
    write (*,*) 'Die Zerlegung in Teilpolynome für Profil',PROF(NN,0),' ist abgeschlossen'
    write (*,*) 'Polynom Q1 A1 hat',POLYQ1(NN)+1 ,'Stuetzstellen'
    write (*,*) 'Polynom Q2 A2 hat',POLYQ2(NN)+1 ,'Stuetzstellen'
    write (*,*) 'Der Grenzabfluss ist',Q(NN,(POLYQ2(NN)+ POLYQ1(NN)+ 1)),'m3/s'
  END DO


ELSEIF (WASTUN .EQ. 'N') THEN     ! Einteiliges Polynom sollen erzeugt werden
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    POLYQ1(NN) = 0
    POLYQ2(NN) = 0
    POLYA1(NN) = 0
    POLYA2(NN) = 0
    Grenze = 0
    DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE
      IF (AUSREISSER .EQ. 'N') THEN
        IF (M .EQ. 0) then
          POLYQ1(NN) = POLYQ1(NN) + 1
          POLYA1(NN) = POLYQ1(NN)
        ELSEIF (TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze.EQ.0) then
          POLYQ1(NN) = POLYQ1(NN) + 1
          POLYA1(NN) = POLYQ1(NN)
        Else
          Grenze = 1
        end if

      ELSEIF (AUSREISSER .EQ. 'J') THEN
        IF (M .EQ. 0) then
          H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
          Q_n(NN,POLYQ1(NN)) = Q(NN,M)
          A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
          MQQ_1(NN,M) = M
          POLYQ1(NN) = POLYQ1(NN) + 1
          MAA_1(NN,M) = MQQ_1(NN,M)
          POLYA1(NN)  = POLYQ1(NN)
        ELSEIF (TIEFE(NN,M) .GT. H_n(NN,(POLYQ1(NN)-1))) then
          H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
          Q_n(NN,POLYQ1(NN)) = Q(NN,M)
          A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
          MQQ_1(NN,M) = M
          POLYQ1(NN) = POLYQ1(NN) + 1
          MAA_1(NN,M) = MQQ_1(NN,M)
          POLYA1(NN)  = POLYQ1(NN)
        end if
      ENDIF

    END DO
    if (POLYQ1(NN) .gt. 0) then
      POLYQ1(NN) =  POLYQ1(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
      POLYA1(NN) =  POLYA1(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
    END if
    write (*,*) 'Der Gueltigkeitsbereich für Profil',PROF(NN,0),' ist selektiert'
    write (*,*) 'Das Polynom Q und A hat',POLYQ1(NN)+1 ,'Stuetzstellen'
    write (*,*) 'Der Grenzabfluss ist',Q(NN,(POLYQ1(NN)+ 1)),'m3/s'
  END DO
END IF

!--------------   Zerlegung in Teilpolynome je Profil fuer Alpha(h)            ------------
!------------------------------------------------------------------------------------------
NN = 0
M = 0
DO NN = 0, NMAX   ! UEBER ALLE PROFILE
  POLYALP1(NN) = 0
  POLYALP2(NN) = 0
  ALPHA_END(NN) = 0
  Grenze = 0
  DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE

    IF (AUSREISSER .EQ. 'N') THEN
      IF (POLYALP1(NN).EQ.0 .and. POLYALP2(NN).EQ.0) then
        IF (TIEFE(NN,M).LE.H_BORDVOLL(NN) .AND. ALP_E(NN,M).LE.IMPULS) THEN
          POLYALP1(NN) = POLYALP1(NN) + 1
        ELSEIF (TIEFE(NN,M).GT.H_BORDVOLL(NN) .AND. ALP_E(NN,M).LE.IMPULS) THEN
          POLYALP2(NN) = POLYALP2(NN) + 1
        ENDIF
      ELSEIF (TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze.EQ.0) then
        IF (TIEFE(NN,M).LE.H_BORDVOLL(NN) .AND. ALP_E(NN,M).LE.IMPULS) THEN
          POLYALP1(NN) = POLYALP1(NN) + 1
        ELSEIF (TIEFE(NN,M).GT.H_BORDVOLL(NN) .AND. ALP_E(NN,M).LE.IMPULS) THEN
          POLYALP2(NN) = POLYALP2(NN) + 1
        ENDIF
      ELSEIF (ALP_E(NN,M) .GT. IMPULS) THEN
        Grenze = 1
      else
        Grenze = 1
      end if

    ELSEIF (AUSREISSER .EQ. 'J') THEN
      IF (POLYALP1(NN).ne.0 .and. POLYALP2(NN).ne.0) THEN
        Ha_n_akt(NN) = Ha_n(NN, (POLYALP1(NN)+POLYALP2(NN)-1) )
      ELSEIF (POLYALP1(NN).eq.0 .and. POLYALP2(NN).ne.0) THEN
        Ha_n_akt(NN) = Ha_n(NN, (POLYALP1(NN)+POLYALP2(NN)-1) )
      ELSEIF (POLYALP2(NN).eq.0 .and. POLYALP1(NN).ne.0) THEN
        Ha_n_akt(NN) = Ha_n(NN, (POLYALP1(NN)+POLYALP2(NN)-1) )
      END IF

      IF (POLYALP1(NN).EQ.0 .and. POLYALP2(NN).EQ.0) then
        IF (TIEFE(NN,M).LE.H_BORDVOLL(NN) .AND. ALP_E(NN,M).LE.IMPULS) THEN
          Ha_n(NN,POLYALP1(NN)) = TIEFE(NN,M)
          alp_n(NN,POLYALP1(NN)) = ALP_E(NN,M)
          MA_1(NN,M) = M
          POLYALP1(NN) = POLYALP1(NN) + 1
        ELSEIF (TIEFE(NN,M).GT.H_BORDVOLL(NN) .AND. ALP_E(NN,M) .LE. IMPULS) THEN
          Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN))) = TIEFE(NN,M)
          alp_n(NN,(POLYALP1(NN)+POLYALP2(NN))) = ALP_E(NN,M)
          MA_2(NN,M) = M
          POLYALP2(NN) = POLYALP2(NN) + 1
        ENDIF

      ELSEIF (TIEFE(NN,M) .GT. Ha_n_akt(NN)) then
        IF (TIEFE(NN,M) .LE. H_BORDVOLL(NN) .and. POLYALP2(NN).eq.0 .AND. ALP_E(NN,M).LE.IMPULS) THEN
          Ha_n(NN,POLYALP1(NN)) = TIEFE(NN,M)
          alp_n(NN,POLYALP1(NN)) = ALP_E(NN,M)
          MA_1(NN,M) = M
          POLYALP1(NN) = POLYALP1(NN) + 1
        ELSEIF (TIEFE(NN,M) .GT. H_BORDVOLL(NN) .AND. ALP_E(NN,M).LE.IMPULS) THEN
          Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN))) = TIEFE(NN,M)
          alp_n(NN,(POLYALP1(NN)+POLYALP2(NN))) = ALP_E(NN,M)
          MA_2(NN,M) = M
          POLYALP2(NN) = POLYALP2(NN) + 1
        ENDIF
      end if
    ENDIF
  END DO

  if (POLYALP1(NN) .gt. 0) then
    POLYALP1(NN) =  POLYALP1(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
  END if
  if (POLYALP2(NN) .gt. 0) then
    POLYALP2(NN) =  POLYALP2(NN) - 1   ! Da Zaehler für die Q und A-Werte bei null beginnt
  END if
  write (*,*) ' Die Zerlegung in alpha-Teilpolynome fuer Profil',PROF(NN,0),' ist abgeschlossen'
  write (*,*) ' Polynom alpha1 hat',POLYALP1(NN)+1,' Stuetzstellen'
  write (*,*) ' Polynom alpha2 hat',POLYALP2(NN)+1,' Stuetzstellen'
  write (*,*) ' Der Grenzabfluss ist',Q(NN,(POLYALP2(NN)+ POLYALP1(NN)+ 1)),' m3/s'
END DO

IF (AUSREISSER .EQ. 'N') THEN
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    MQQX_1(NN) = POLYQ1(NN)
    MQQX_2(NN) = POLYQ2(NN)
    MAAX_1(NN) = POLYA1(NN)
    MAAX_2(NN) = POLYA2(NN)
    MAX_1(NN) = POLYALP1(NN)
    MAX_2(NN) = POLYALP2(NN)
  END DO
ELSEIF (AUSREISSER .EQ. 'J') THEN
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    IF (MAXVAL(MQQ_1(NN,0:MMAX)).eq.0) THEN
      MQQX_1(NN) = MAXVAL(MQQ_1(NN,0:MMAX))
      MQQX_2(NN) = MAXVAL(MQQ_2(NN,0:MMAX)) - MQQX_1(NN)
    ELSE
      MQQX_1(NN) = MAXVAL(MQQ_1(NN,0:MMAX))
      MQQX_2(NN) = MAXVAL(MQQ_2(NN,0:MMAX)) - MQQX_1(NN) - 1
    end if

    IF (MAXVAL(MAA_1(NN,0:MMAX)).eq.0) THEN
      MAAX_1(NN) = MAXVAL(MAA_1(NN,0:MMAX))
      MAAX_2(NN) = MAXVAL(MAA_2(NN,0:MMAX)) - MAAX_1(NN)
    ElSE
      MAAX_1(NN) = MAXVAL(MAA_1(NN,0:MMAX))
      MAAX_2(NN) = MAXVAL(MAA_2(NN,0:MMAX)) - MAAX_1(NN) - 1
    END IF

    IF (MAXVAL(MA_1(NN,0:MMAX)).eq.0) THEN
      MAX_1(NN) = MAXVAL(MA_1(NN,0:MMAX))
      MAX_2(NN) = MAXVAL(MA_2(NN,0:MMAX)) - MAX_1(NN)
    ElSE
      MAX_1(NN) = MAXVAL(MA_1(NN,0:MMAX))
      MAX_2(NN) = MAXVAL(MA_2(NN,0:MMAX)) - MAX_1(NN) - 1
    END IF
  END DO
ENDIF

write (*,*) 'Zerlegung in Teilpolynome je Profil abgeschlossen'

END SUBROUTINE !SUB PUNKTANALYSE


!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   Automatische Zerlegung in Teilpolynome je Profil fuer A(h) und Q(h)                        ***
!***************************************************************************************************

SUBROUTINE AUTOANALYSE
USE VARIABLEN
IMPLICIT NONE


NN = 0
M = 0
IF (WASTUN .EQ. 'J') THEN      ! Dreiteilige Polynome sollen erzeugt werden
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    POLYQ1(NN) = 0
    POLYQ2(NN) = 0
    POLYA1(NN) = 0
    POLYA2(NN) = 0
    Grenze = 0
    DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE

      IF (AUSREISSER .EQ. 'N') THEN
        IF (M .EQ. 0) then
          POLYQ1(NN) = POLYQ1(NN) + 1
          POLYA1(NN) = POLYA1(NN) + 1
        ELSEIF (POLYQ1(NN).EQ.1 .and. TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze.eq.0) then
          POLYQ1(NN) = POLYQ1(NN) + 1
          POLYA1(NN) = POLYA1(NN) + 1
        ELSEIF (POLYQ1(NN).GE.2 .and. TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze.eq.0) then
          DQ1(NN,(M-1)) = (Q(NN,(M-1)) - Q(NN,(M-2))) / (TIEFE(NN,(M-1)) - TIEFE(NN,(M-2)))
          DQ1(NN,M)     = (Q(NN,M) - Q(NN,(M-1)))     / (TIEFE(NN,M) - TIEFE(NN,(M-1)))
          DQ(NN,M)      = (DQ1(NN,(M-1)) /DQ1(NN,M))

          DA1(NN,(M-1)) = (FL_QM(NN,(M-1)) - FL_QM(NN,(M-2))) / (TIEFE(NN,(M-1)) - TIEFE(NN,(M-2)))
          DA1(NN,M)     = (FL_QM(NN,M) - FL_QM(NN,(M-1)))     / (TIEFE(NN,M) - TIEFE(NN,(M-1)))
          DA(NN,M)      = (DA1(NN,(M-1)) /DA1(NN,M))

          IF (DQ(NN,M).GE.(1/DQ_LIMIT) .and. DQ(NN,M).LE.DQ_LIMIT .and. POLYQ2(NN).eq.0) THEN
            POLYQ1(NN) = POLYQ1(NN) + 1
          ELSE
            IF (POLYQ2(NN).eq.0) THEN
              HQ_BORDVOLL(NN) = TIEFE(NN,(M-1))
            END IF
            POLYQ2(NN) = POLYQ2(NN) + 1
          ENDIF
          IF (DA(NN,M).GE.(1/DA_LIMIT) .and. DA(NN,M).LE.DA_LIMIT .and. POLYA2(NN).eq.0) THEN
            POLYA1(NN) = POLYA1(NN) + 1
          ELSE
            IF (POLYA2(NN).eq.0) THEN
              HA_BORDVOLL(NN) = TIEFE(NN,(M-1))
            END IF
            POLYA2(NN) = POLYA2(NN) + 1
          ENDIF
        ELSE
          Grenze = 1
        end if

      ELSEIF (AUSREISSER .EQ. 'J') THEN
        IF (POLYQ1(NN).ne.0 .and. POLYQ2(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))
        ELSEIF (POLYQ1(NN).eq.0 .and. POLYQ2(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))
        ELSEIF (POLYQ2(NN).eq.0 .and. POLYQ1(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))
        END IF

        IF (POLYA1(NN).ne.0 .and. POLYA2(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYA1(NN)+POLYA2(NN)-1))
        ELSEIF (POLYA1(NN).eq.0 .and. POLYA2(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYA1(NN)+POLYA2(NN)-1))
        ELSEIF (POLYA2(NN).eq.0 .and. POLYA1(NN).ne.0) THEN
          H_n_akt(NN) = H_n(NN,(POLYA1(NN)+POLYA2(NN)-1))
        END IF

        IF (M .EQ. 0) then
          H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
          H_n(NN,POLYA1(NN)) = TIEFE(NN,M)
          Q_n(NN,POLYQ1(NN)) = Q(NN,M)
          A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
          MQQ_1(NN,M) = M
          POLYQ1(NN) = POLYQ1(NN) + 1
          MAA_1(NN,M) = M
          POLYA1(NN) = POLYA1(NN) + 1

        ELSEIF (POLYQ1(NN).EQ.1 .and. POLYA1(NN).EQ.1 .and. TIEFE(NN,M).GT.H_n_akt(NN)) then
          H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
          H_n(NN,POLYA1(NN)) = TIEFE(NN,M)
          Q_n(NN,POLYQ1(NN)) = Q(NN,M)
          A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
          MQQ_1(NN,M) = M
          POLYQ1(NN) = POLYQ1(NN) + 1
          MAA_1(NN,M) = M
          POLYA1(NN) = POLYA1(NN) + 1

        ELSEIF (POLYQ1(NN).GE.2 .and. POLYA1(NN).GE.2 .and. TIEFE(NN,M).GT.H_n_akt(NN)) then

          H_n(NN,(POLYQ1(NN)+POLYQ2(NN))) = TIEFE(NN,M)
          Q_n(NN,(POLYQ1(NN)+POLYQ2(NN))) = Q(NN,M)
          H_n(NN,(POLYA1(NN)+POLYA2(NN))) = TIEFE(NN,M)
          A_n(NN,(POLYA1(NN)+POLYA2(NN))) = FL_QM(NN,M)

          DQ1(NN,(POLYQ1(NN)+POLYQ2(NN)-1)) = (Q_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1)) - Q_n(NN,(POLYQ1(NN)+POLYQ2(NN)-2))) / &
                                           &  (H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1)) - H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-2)))
          DQ1(NN,(POLYQ1(NN)+POLYQ2(NN)))   = (Q_n(NN,(POLYQ1(NN)+POLYQ2(NN)))   - Q_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1))) / &
                                           &  (H_n(NN,(POLYQ1(NN)+POLYQ2(NN)))   - H_n(NN,(POLYQ1(NN)+POLYQ2(NN)-1)))
          DQ(NN,(POLYQ1(NN)+POLYQ2(NN)))    =  DQ1(NN,(POLYQ1(NN)+POLYQ2(NN)-1)) / DQ1(NN,(POLYQ1(NN)+POLYQ2(NN)))

          DA1(NN,(POLYA1(NN)+POLYA2(NN)-1)) = (A_n(NN,(POLYA1(NN)+POLYA2(NN)-1)) - A_n(NN,(POLYA1(NN)+POLYA2(NN)-2))) / &
                                           &  (H_n(NN,(POLYA1(NN)+POLYA2(NN)-1)) - H_n(NN,(POLYA1(NN)+POLYA2(NN)-2)))
          DA1(NN,(POLYA1(NN)+POLYA2(NN)))   = (A_n(NN,(POLYA1(NN)+POLYA2(NN)))   - A_n(NN,(POLYA1(NN)+POLYA2(NN)-1))) / &
                                           &  (H_n(NN,(POLYA1(NN)+POLYA2(NN)))   - H_n(NN,(POLYA1(NN)+POLYA2(NN)-1)))
          DA(NN,(POLYA1(NN)+POLYA2(NN)))    =  DA1(NN,(POLYA1(NN)+POLYA2(NN)-1)) / DA1(NN,(POLYA1(NN)+POLYA2(NN)))

          IF ( DQ(NN,(POLYQ1(NN)+POLYQ2(NN))).GE.(1/DQ_LIMIT) .and. &
                         & DQ(NN,(POLYQ1(NN)+POLYQ2(NN))).LE.DQ_LIMIT .and. POLYQ2(NN).eq.0) THEN

            MQQ_1(NN,M) = M
            POLYQ1(NN) = POLYQ1(NN) + 1
          ELSE
            IF (POLYQ2(NN).eq.0) THEN
              HQ_BORDVOLL(NN) = H_n(NN,(POLYQ1(NN)-1))
            END IF
            MQQ_2(NN,M) = M
            POLYQ2(NN) = POLYQ2(NN) + 1
          ENDIF

          IF ( DA(NN,(POLYA1(NN)+POLYA2(NN))).GE.(1/DA_LIMIT) .and.  &
                         & DA(NN,(POLYA1(NN)+POLYA2(NN))).LE.DA_LIMIT .and. POLYA2(NN).eq.0) THEN

            MAA_1(NN,M) = M
            POLYA1(NN) = POLYA1(NN) + 1
          ELSE
            IF (POLYA2(NN).eq.0) THEN
              HA_BORDVOLL(NN) = H_n(NN,(POLYA1(NN)-1))
            END IF
            MAA_2(NN,M) = M
            POLYA2(NN) = POLYA2(NN) + 1
          ENDIF
        end if
      ENDIF
    END DO

    if (POLYQ1(NN) .gt. 0) then
      POLYQ1(NN) =  POLYQ1(NN) - 1   ! Da Zaehler für die Q-Werte bei null beginnt
    END if
    if (POLYQ2(NN) .gt. 0) then
      POLYQ2(NN) =  POLYQ2(NN) - 1   ! Da Zaehler für die Q-Werte bei null beginnt
    END if
    if (POLYA1(NN) .gt. 0) then
      POLYA1(NN) =  POLYA1(NN) - 1   ! Da Zaehler für die A-Werte bei null beginnt
    END if
    if (POLYA2(NN) .gt. 0) then
      POLYA2(NN) =  POLYA2(NN) - 1   ! Da Zaehler für die A-Werte bei null beginnt
    END if
    write (*,*) ' Die AUTOMATISCHE Zerlegung in Teilpolynome für Profil',PROF(NN,0),' ist abgeschlossen'
    write (*,*) ' Polynom Q1 hat',POLYQ1(NN)+1,' Stuetzstellen'
    write (*,*) ' Polynom Q2 hat',POLYQ2(NN)+1,' Stuetzstellen'
    write (*,*) ' Polynom A1 hat',POLYA1(NN)+1,' Stuetzstellen'
    write (*,*) ' Polynom A2 hat',POLYA2(NN)+1,' Stuetzstellen'
    write (*,*) ' Der Grenzabfluss ist',Q(NN,(POLYQ2(NN)+ POLYQ1(NN)+ 1)),'m3/s'
  END DO




ELSEIF (WASTUN .EQ. 'N') THEN     ! Einteiliges Polynom sollen erzeugt werden
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    POLYQ1(NN) = 0
    POLYQ2(NN) = 0
    POLYA1(NN) = 0
    POLYA2(NN) = 0
    Grenze = 0
    DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE
      IF (AUSREISSER .EQ. 'N') THEN    ! Polynom bis zum ersten Ausreisser
        IF (M .EQ. 0) then
          POLYQ1(NN) = POLYQ1(NN) + 1
          POLYA1(NN) = POLYA1(NN) + 1
        ELSEIF (TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze .EQ. 0) then
          POLYQ1(NN) = POLYQ1(NN) + 1
          POLYA1(NN) = POLYA1(NN) + 1
        Else
          Grenze = 1
        end if

      ELSEIF (AUSREISSER .EQ. 'J') THEN
        IF (M .EQ. 0) then
          H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
          H_n(NN,POLYA1(NN)) = TIEFE(NN,M)
          Q_n(NN,POLYQ1(NN)) = Q(NN,M)
          A_n(NN,POLYQ1(NN)) = FL_QM(NN,M)
          MQQ_1(NN,M) = M
          POLYQ1(NN) = POLYQ1(NN) + 1
          MAA_1(NN,M) = M
          POLYA1(NN) = POLYA1(NN) + 1
        ELSEIF (TIEFE(NN,M).GT.H_n(NN,(POLYQ1(NN)-1))) then
          H_n(NN,POLYQ1(NN)) = TIEFE(NN,M)
          H_n(NN,POLYA1(NN)) = TIEFE(NN,M)
          Q_n(NN,POLYQ1(NN)) = Q(NN,M)
          A_n(NN,POLYA1(NN)) = FL_QM(NN,M)
          MQQ_1(NN,M) = M
          POLYQ1(NN) = POLYQ1(NN) + 1
          MAA_1(NN,M) = M
          POLYA1(NN) = POLYA1(NN) + 1
        end if
      ENDIF

    END DO
    if (POLYQ1(NN) .gt. 0) then
      POLYQ1(NN) =  POLYQ1(NN) - 1   ! Da Zaehler für die Q-Werte bei null beginnt
    END if
    if (POLYA1(NN) .gt. 0) then
      POLYA1(NN) =  POLYA1(NN) - 1   ! Da Zaehler für die A-Werte bei null beginnt
    END if
    write (*,*) 'Der Gueltigkeitsbereich für Profil',PROF(NN,0),' ist selektiert'
    write (*,*) 'Das Q Polynom hat',POLYQ1(NN)+1 ,'Stuetzstellen'
    write (*,*) 'Das A Polynom hat',POLYA1(NN)+1 ,'Stuetzstellen'
    write (*,*) 'Der Grenzabfluss ist',Q(NN,(POLYQ1(NN)+ 1)),'m3/s'
  END DO
END IF

!--------------   Automatische Zerlegung in Teilpolynome je Profil fuer Alpha(h)    ------------
!-----------------------------------------------------------------------------------------------
NN = 0
M = 0
DO NN = 0, NMAX   ! UEBER ALLE PROFILE
  POLYALP1(NN) = 0
  POLYALP2(NN) = 0
  ALPHA_END(NN) = 0
  Grenze = 0
  DO M = 0, MMAX    ! UEBER ALLE ABFLUESSE

    IF (AUSREISSER.EQ.'N') THEN
      IF (POLYALP1(NN).EQ.0 .AND. ALP_E(NN,M).LE.IMPULS) then
        POLYALP1(NN) = POLYALP1(NN) + 1
      ELSEIF (POLYALP1(NN).EQ.1 .and. TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze.eq.0 .AND. ALP_E(NN,M).LE.IMPULS) then
        POLYALP1(NN) = POLYALP1(NN) + 1
      ELSEIF (POLYALP1(NN).GE.2 .and. TIEFE(NN,M).GT.TIEFE(NN,(M-1)) .AND. Grenze.eq.0 .and. ALP_E(NN,M).LE.IMPULS) then

        IF (ALP_E(NN,M).gt.1.001D0) THEN
          DALP1(NN,(M-1)) = (ALP_E(NN,(M-1)) - ALP_E(NN,(M-2))) / (TIEFE(NN,(M-1)) - TIEFE(NN,(M-2)))
          DALP1(NN,M)     = (ALP_E(NN,M) - ALP_E(NN,(M-1)))     / (TIEFE(NN,M) - TIEFE(NN,(M-1)))
          IF (DALP1(NN,M).eq.0 .or. DALP1(NN,(M-1)).eq.0) THEN
            DALP(NN,M) = 1.0
          ELSE
            DALP(NN,M) = (DALP1(NN,(M-1)) /DALP1(NN,M))
          END IF

        ELSEIF (ALP_E(NN,M).le.1.001D0) THEN
          DALP(NN,M) = 1.0
        END IF

        IF (DALP(NN,M).GE.(1/DALP_LIMIT) .and. DALP(NN,M).LE.DALP_LIMIT .and. POLYALP2(NN).eq.0) THEN
          POLYALP1(NN) = POLYALP1(NN) + 1
        ELSE
          IF (POLYALP2(NN).eq.0) THEN
            HALP_BORDVOLL(NN) = TIEFE(NN,(M-1))
          END IF
          POLYALP2(NN) = POLYALP2(NN) + 1
        ENDIF

      ELSEIF (ALP_E(NN,M).GT.IMPULS) THEN
        Grenze = 1
      ELSE
        Grenze = 1
      end if


    ELSEIF (AUSREISSER .EQ. 'J') THEN
      IF (POLYALP1(NN).ne.0 .and. POLYALP2(NN).ne.0) THEN
        Ha_n_akt(NN) = Ha_n(NN, (POLYALP1(NN)+POLYALP2(NN)-1) )
      ELSEIF (POLYALP1(NN).eq.0 .and. POLYALP2(NN).ne.0) THEN
        Ha_n_akt(NN) = Ha_n(NN, (POLYALP1(NN)+POLYALP2(NN)-1) )
      ELSEIF (POLYALP2(NN).eq.0 .and. POLYALP1(NN).ne.0) THEN
        Ha_n_akt(NN) = Ha_n(NN, (POLYALP1(NN)+POLYALP2(NN)-1) )
      END IF

      IF (POLYALP1(NN).EQ. 0 .AND. ALP_E(NN,M).LE.IMPULS) then
        Ha_n(NN,POLYALP1(NN)) = TIEFE(NN,M)
        alp_n(NN,POLYALP1(NN)) = ALP_E(NN,M)
        MA_1(NN,M) = M
        POLYALP1(NN) = POLYALP1(NN) + 1

      ELSEIF (POLYALP1(NN).EQ.1 .AND. ALP_E(NN,M).LE.IMPULS .and. TIEFE(NN,M).GT.Ha_n_akt(NN)) then
        Ha_n(NN,POLYALP1(NN)) = TIEFE(NN,M)
        alp_n(NN,POLYALP1(NN)) = ALP_E(NN,M)
        MA_1(NN,M) = M
        POLYALP1(NN) = POLYALP1(NN) + 1

      ELSEIF (POLYALP1(NN).GE.2 .and. TIEFE(NN,M).GT.Ha_n_akt(NN) .and. ALP_E(NN,M).LE.IMPULS) then
        Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN))) = TIEFE(NN,M)
        alp_n(NN,(POLYALP1(NN)+POLYALP2(NN))) = ALP_E(NN,M)

        IF (ALP_E(NN,M).gt.1.001D0) THEN
          DALP1(NN,(POLYALP1(NN)+POLYALP2(NN)-1)) =    &
               & (alp_n(NN,(POLYALP1(NN)+POLYALP2(NN)-1)) - alp_n(NN,(POLYALP1(NN)+POLYALP2(NN)-2))) / &
               &  (Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN)-1)) -  Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN)-2)))
          DALP1(NN,(POLYALP1(NN)+POLYALP2(NN))) =    &
               & (alp_n(NN,(POLYALP1(NN)+POLYALP2(NN)))  - alp_n(NN,(POLYALP1(NN)+POLYALP2(NN)-1))) / &
               &  (Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN)))  -  Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN)-1)))


          IF (DALP1(NN,(POLYALP1(NN)+POLYALP2(NN)-1)).eq.0 .or. DALP1(NN,(POLYALP1(NN)+POLYALP2(NN))).eq.0) THEN
            DALP(NN,(POLYALP1(NN)+POLYALP2(NN))) = 1.0
          ELSE
            DALP(NN,(POLYALP1(NN)+POLYALP2(NN))) = &
              & DALP1(NN,(POLYALP1(NN)+POLYALP2(NN)-1)) / DALP1(NN,(POLYALP1(NN)+POLYALP2(NN)))
          END IF

        ELSEIF (ALP_E(NN,M).le.1.001D0) THEN
          DALP(NN,(POLYALP1(NN)+POLYALP2(NN))) = 1.0
        END IF


        IF (DALP(NN,(POLYALP1(NN)+POLYALP2(NN))).GE.(1/DALP_LIMIT) .and. &
                       &  DALP(NN,(POLYALP1(NN)+POLYALP2(NN))).LE.DALP_LIMIT .and. POLYALP2(NN).eq.0) Then
          MA_1(NN,M) = M
          POLYALP1(NN) = POLYALP1(NN) + 1
        ELSE
          IF (POLYALP2(NN).eq.0) THEN
            HALP_BORDVOLL(NN) = Ha_n(NN,(POLYALP1(NN)-1))
          END IF
          MA_2(NN,M) = M
          POLYALP2(NN) = POLYALP2(NN) + 1
        ENDIF

      ENDIF

    ENDIF
  END DO

  if (POLYALP1(NN) .gt. 0) then
    POLYALP1(NN) =  POLYALP1(NN) - 1   ! Da Zaehler für die alpha-Werte bei null beginnt
  END if
  if (POLYALP2(NN) .gt. 0) then
    POLYALP2(NN) =  POLYALP2(NN) - 1   ! Da Zaehler für die alpha-Werte bei null beginnt
  END if
  write (*,*) ' Die AUTOMTAISCHE Zerlegung in alpha-Teilpolynome fuer Profil',PROF(NN,0),' ist abgeschlossen'
  write (*,*) ' Polynom 1 hat',POLYALP1(NN)+1,' Stuetzstellen'
  write (*,*) ' Polynom 2 hat',POLYALP2(NN)+1,' Stuetzstellen'
  write (*,*) ' Der Grenzabfluss ist',Q(NN,(POLYALP2(NN)+ POLYALP1(NN)+ 1)),'m3/s'
END DO

IF (AUSREISSER .EQ. 'N') THEN
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    MQQX_1(NN) = POLYQ1(NN)
    MQQX_2(NN) = POLYQ2(NN)
    MAAX_1(NN) = POLYA1(NN)
    MAAX_2(NN) = POLYA2(NN)
    MAX_1(NN) = POLYALP1(NN)
    MAX_2(NN) = POLYALP2(NN)
  END DO
ELSEIF (AUSREISSER .EQ. 'J') THEN
  DO NN = 0, NMAX   ! UEBER ALLE PROFILE
    IF (MAXVAL(MQQ_1(NN,0:MMAX)).eq.0) THEN
      MQQX_1(NN) = MAXVAL(MQQ_1(NN,0:MMAX))
      MQQX_2(NN) = MAXVAL(MQQ_2(NN,0:MMAX)) - MQQX_1(NN)
    ELSE
      MQQX_1(NN) = MAXVAL(MQQ_1(NN,0:MMAX))
      MQQX_2(NN) = MAXVAL(MQQ_2(NN,0:MMAX)) - MQQX_1(NN) - 1
    end if

    IF (MAXVAL(MAA_1(NN,0:MMAX)).eq.0) THEN
      MAAX_1(NN) = MAXVAL(MAA_1(NN,0:MMAX))
      MAAX_2(NN) = MAXVAL(MAA_2(NN,0:MMAX)) - MAAX_1(NN)
    ELSE
      MAAX_1(NN) = MAXVAL(MAA_1(NN,0:MMAX))
      MAAX_2(NN) = MAXVAL(MAA_2(NN,0:MMAX)) - MAAX_1(NN) - 1
    end if

    IF (MAXVAL(MA_1(NN,0:MMAX)).eq.0) THEN
      MAX_1(NN) = MAXVAL(MA_1(NN,0:MMAX))
      MAX_2(NN) = MAXVAL(MA_2(NN,0:MMAX)) - MAX_1(NN)
    ElSE
      MAX_1(NN) = MAXVAL(MA_1(NN,0:MMAX))
      MAX_2(NN) = MAXVAL(MA_2(NN,0:MMAX)) - MAX_1(NN) - 1
    END IF
  END DO
ENDIF

write (*,*) 'AUTOMTAISCHE Zerlegung in Teilpolynome je Profil abgeschlossen'

END SUBROUTINE !SUB AUTOANALYSE

!***************************************************************************************************
!**   ----------------------                                                                     ***
!**    ERZEUGUNG DER POLYNOME fuer A(h) und Q(h) und alpha(h)                                    ***
!***************************************************************************************************
SUBROUTINE POLYKERN
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE        ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT14 ) AUSQ_FUNKT      ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT15 ) AUSA_FUNKT      ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT16 ) AUSALPHA_FUNKT  ! PFAD DER LAENGSSCHNITT-DATEI MIT RICHTIGER LAENGE

AUSGABE = AUSGABEPFAD(1:TEXTAUSG)
AUSQ_FUNKT     = AUSGABE // 'Q-H-FUNKTIONEN.txt'
AUSA_FUNKT     = AUSGABE // 'A-H-FUNKTIONEN.txt'
AUSALPHA_FUNKT = AUSGABE // 'ALPHA-H-FUNKTIONEN.txt'


NN = 0
M = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  DO POLYZAHL= 1, 2  ! START DER POLYNOMSCHLEIFE
    IF (POLYZAHL.EQ.1) THEN
      XEND = POLYQ1(NN)
    ElseIF (POLYZAHL.EQ.2) THEN
      XEND = POLYA1(NN)
    Endif

    If (XEND .gt. 0) then
      CALL POLYNOMFKT(XEND)  ! AUFRUF DER SUBROUTINE ZUM ERSTELLEN DER FUNKTIONEN


      IF (POLYZAHL.EQ.1) THEN   ! ZURUECKHOLEN DER TATSÄCHLICHEN ANZAHL DER DATENTUPEL NACH EINER
        XEND = POLYQ1(NN)        ! EVTUELLEN REDUKTION FUER DIE POLYNOM-ERZEUGUNG
      ElseIF (POLYZAHL.EQ.2) THEN
        XEND = POLYA1(NN)
      Endif
    ELSE
      ALPHA(0) = 0.0D0
      ALPHA(1) = 0.0D0
      ALPHA(2) = 0.0D0
      ALPHA(3) = 0.0D0
      ALPHA(4) = 0.0D0
      POLYGRAD(NN) = 0
    End if

    IF (NN.EQ.0 .AND. POLYZAHL.EQ.1 .AND. AUSGFUNKT .EQ. 'J') THEN   ! NUR EINMAL AUSGEBEN
      ! OPEN (14, FILE='02Ausgang\Q-H-FUNKTIONEN.txt' , STATUS = 'replace')
      OPEN (14, FILE=AUSQ_FUNKT , STATUS = 'replace')
      WRITE(14,*)' '
      WRITE(14,*)' AUSGABEDATEI DER Q1-H-FUNKTIONEN DES PROFILS'
      WRITE(14,*)' -----------------------------------------------------------------'    ! ZEILE 3
      WRITE(14,*)' PROFIL -- Stützstellen -- Polynomgrad -- Q-H-FUNKTIONENMIT Q(H) = ..... '

      ! OPEN (15, FILE='02Ausgang\A-H-FUNKTIONEN.txt' , STATUS = 'replace')
      OPEN (15, FILE=AUSA_FUNKT , STATUS = 'replace')
      WRITE(15,*)' '
      WRITE(15,*)' AUSGABEDATEI DER A1-H-FUNKTION FUER FLUSSMITTE DES PROFILS'
      WRITE(15,*)' -----------------------------------------------------------------'    ! ZEILE 3
      WRITE(15,*)' PROFIL -- Stützstellen -- Polynomgrad -- A-H-FUNKTIONEN MIT A(H) = ..... '

      ! OPEN (16, FILE='02Ausgang\ALPHA-H-FUNKTIONEN.txt' , STATUS = 'replace')
      OPEN (16, FILE=AUSALPHA_FUNKT , STATUS = 'replace')
      WRITE(16,*)' '
      WRITE(16,*)' AUSGABEDATEI DER ALPHA1-H-FUNKTION DES PROFILS'
      WRITE(16,*)' -----------------------------------------------------------------'    ! ZEILE 3
      WRITE(16,*)' PROFIL -- Stützstellen -- Polynomgrad -- ALPHA-H-FUNKTIONEN MIT  ALPHA(H) = ..... '
    END IF

    IF (POLYZAHL .EQ. 1) THEN
      write (*,*) 'Berechnung Q1(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN            ! wird nur erzeugt, wenn in der ini-datei gewuenscht
        WRITE(14,'(1X,F8.4,3X,I4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,4X)')  PROF(NN,0), POLYQ1(NN)+1, &
           &        POLYGRAD(NN), ALPHA(0), ALPHA(1),ALPHA(2),ALPHA(3), ALPHA(4)
      END IF

      AA = 0
      DO AA = 0, MQQX_1(NN)
        POLY_Q1(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,AA)) + (ALPHA(2)* (TIEFE(NN,AA)**2.D0 )) + &
                     & (ALPHA(3)* (TIEFE(NN,AA)**3.D0 )) + (ALPHA(4)* (TIEFE(NN,AA)**4.D0 ))
        IF (AA .eq. MQQX_1(NN)) then
          ABLEIT_Q1(NN,AA) = ALPHA(1) + (2* ALPHA(2)* (TIEFE(NN,AA)))  &
                     & + (3.D0  * ALPHA(3)* (TIEFE(NN,AA)**2.D0 ))  &
                     & + (4.D0  * ALPHA(4)* (TIEFE(NN,AA)**3.D0 ) )
          ABL2_Q1(NN,AA) = (2* ALPHA(2))  &
                     & + (6.D0  * ALPHA(3)* TIEFE(NN,AA))   &
                     & + (12.D0  * ALPHA(4)* (TIEFE(NN,AA)**2.D0 ))
        end if
      END DO

      KOEF_Q1(NN,0) = ALPHA(0)
      KOEF_Q1(NN,1) = ALPHA(1)
      KOEF_Q1(NN,2) = ALPHA(2)
      KOEF_Q1(NN,3) = ALPHA(3)
      KOEF_Q1(NN,4) = ALPHA(4)
      GRAD_Q1(NN) = POLYGRAD(NN)
    ELSE IF (POLYZAHL .EQ. 2) THEN
      write (*,*) 'Berechnung A1(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN            ! wird nur erzeugt, wenn in der ini-datei gewuenscht
        WRITE(15,'(1X,F8.4,3X,I4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,4X)')  PROF(NN,0), POLYA1(NN)+1, &
           &        POLYGRAD(NN), ALPHA(0), ALPHA(1),ALPHA(2),ALPHA(3), ALPHA(4)
      ENDIF

      AA = 0
      DO AA = 0, MAAX_1(NN)
        POLY_A1(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,AA)) &
                     & + (ALPHA(2)* (TIEFE(NN,AA)**2.D0 ))   &
                     & + (ALPHA(3)* (TIEFE(NN,AA)**3.D0 ))   &
                     & + (ALPHA(4)* (TIEFE(NN,AA)**4.D0 ))
        IF (AA .eq. MAAX_1(NN)) then
          ABLEIT_A1(NN,AA) = ALPHA(1) + (2.D0* ALPHA(2)* (TIEFE(NN,AA)))  &
                     & + (3.D0  * ALPHA(3)* (TIEFE(NN,AA)**2.D0 )) &
                     & + (4.D0  * ALPHA(4)* (TIEFE(NN,AA)**3.D0 ))
          ABL2_A1(NN,AA) = (2.D0 * ALPHA(2))    &
                     & + (6.D0  * ALPHA(3)* TIEFE(NN,AA))   &
                     & + (12.D0  * ALPHA(4)* (TIEFE(NN,AA)**2.D0 ) )
        End if
      END DO

      KOEF_A1(NN,0) = ALPHA(0)
      KOEF_A1(NN,1) = ALPHA(1)
      KOEF_A1(NN,2) = ALPHA(2)
      KOEF_A1(NN,3) = ALPHA(3)
      KOEF_A1(NN,4) = ALPHA(4)
      GRAD_A1(NN) = POLYGRAD(NN)
    END IF
  END DO   ! ENDE DER POLYNOM-SCHLEIFE FUER DIE FUNKTIONEN
END DO   ! ENDE DER DATEI-AUFRUF-SCHLEIFE FUER ALLE PROFILE

! ERZEUGUNG DER POLYNOME A-H UND Q-H FÜR groesser BORDVOLL
!------------------------------------------------------------------------------------------
NN = 0
M = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  DO POLYZAHL= 11, 12   ! START DER POLYNOMSCHLEIFE
    IF (POLYZAHL.EQ.11) THEN
      XEND = POLYQ2(NN)
    ElseIF (POLYZAHL.EQ.12) THEN
      XEND = POLYA2(NN)
    Endif

    If (XEND.gt.0 .AND. WASTUN.EQ.'J') then
      CALL POLYNOMFKT(XEND) ! AUFRUF DER SUBROUTINE ZUM ERSTELLEN DER V-Q-FUNKTIONEN
      XEND = POLY2(NN)      ! ZURUECKHOLEN DER TATSÄCHLICHEN ANZAHL DER DATENTUPEL NACH EINER
                            ! EVTUELLEN REDUKTION FUER DIE POLYNOM-ERZEUGUNG
    ELSE
      ALPHA(0) = 0.0D0
      ALPHA(1) = 0.0D0
      ALPHA(2) = 0.0D0
      ALPHA(3) = 0.0D0
      ALPHA(4) = 0.0D0
      POLYGRAD(NN) = 0
    End if

    IF (POLYZAHL .EQ. 11 .AND. WASTUN .EQ. 'J') THEN
      IF (NN .EQ. 0 .AND. AUSGFUNKT .EQ. 'J')  THEN
        WRITE(14,*)' AUSGABEDATEI DER Q2-H-FUNKTIONEN DES PROFILS'
        WRITE(14,*)' -----------------------------------------------------------------'
      Endif
      write (*,*) 'Berechnung Q2(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN              ! wird nur erzeugt, wenn in der ini-datei gewuenscht
        WRITE(14,'(1X,F8.4,3X,I4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,4X)')  PROF(NN,0), POLYQ2(NN)+1, &
           &       POLYGRAD(NN), ALPHA(0), ALPHA(1),ALPHA(2),ALPHA(3), ALPHA(4)
      ENDIF

      AA = 0
      DO AA = 0, MQQX_2(NN)
        IF (POLYQ1(NN) .EQ. 0 .or. POLYQ2(NN) .EQ. 0) THEN
          POLY_Q2(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,(AA +MQQX_1(NN)))) &
                         & + (ALPHA(2)* (TIEFE(NN,(AA +MQQX_1(NN)))**2.D0 ))  &
                         & + (ALPHA(3)* (TIEFE(NN,(AA +MQQX_1(NN)))**3.D0 ))  &
                         & + (ALPHA(4)* (TIEFE(NN,(AA +MQQX_1(NN)))**4.D0 ))
        ELSEIF (POLYQ1(NN) .GT. 0 .and. POLYQ2(NN) .GT. 0) THEN
          POLY_Q2(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,(AA +MQQX_1(NN) +1))) &
                         & + (ALPHA(2)* (TIEFE(NN,(AA +MQQX_1(NN) +1))**2.D0 ))  &
                         & + (ALPHA(3)* (TIEFE(NN,(AA +MQQX_1(NN) +1))**3.D0 ))  &
                         & + (ALPHA(4)* (TIEFE(NN,(AA +MQQX_1(NN) +1))**4.D0 ))
        END IF

        if (AA .eq. 0) then
          IF (POLYQ1(NN) .EQ. 0 .or. POLYQ2(NN) .EQ. 0) THEN
            POLY_Q2(NN,AA) = ALPHA(0) + (ALPHA(1) *H_n(NN,POLYQ1(NN)))  &
                         & + (ALPHA(2)* (H_n(NN,POLYQ1(NN))**2.D0 ))  &
                         & + (ALPHA(3)* (H_n(NN,POLYQ1(NN))**3.D0 ))  &
                         & + (ALPHA(4)* (H_n(NN,POLYQ1(NN))**4.D0 ))
            ABLEIT_Q2(NN,AA) = ALPHA(1) + (2.D0 * ALPHA(2)* H_n(NN,POLYQ1(NN))) &
                         & + (3.D0  * ALPHA(3)* (H_n(NN,POLYQ1(NN))**2.D0 ))  &
                         & + (4.D0  * ALPHA(4)* (H_n(NN,POLYQ1(NN))**3.D0 ))
            ABL2_Q2(NN,AA) = (2.D0 * ALPHA(2))  &
                         & + (6.D0  * ALPHA(3)* H_n(NN,POLYQ1(NN)))   &
                         & + (12.D0  * ALPHA(4)* (H_n(NN,POLYQ1(NN))**2.D0 ))
          ELSEIF (POLYQ1(NN) .GT. 0 .and. POLYQ2(NN) .GT. 0) THEN
            POLY_Q2(NN,AA) = ALPHA(0) + (ALPHA(1) *H_n(NN,(POLYQ1(NN)+1)))  &
                         & + (ALPHA(2)* (H_n(NN,(POLYQ1(NN)+1))**2.D0 ))   &
                         & + (ALPHA(3)* (H_n(NN,(POLYQ1(NN)+1))**3.D0 ))   &
                         & + (ALPHA(4)* (H_n(NN,(POLYQ1(NN)+1))**4.D0 ))
            ABLEIT_Q2(NN,AA) = ALPHA(1) + (2.D0 * ALPHA(2)* H_n(NN,(POLYQ1(NN)+1)))  &
                         & + (3.D0  * ALPHA(3)* (H_n(NN,(POLYQ1(NN)+1)) **2.D0 )) &
                         & + (4.D0  * ALPHA(4)* (H_n(NN,(POLYQ1(NN)+1)) **3.D0 ))
            ABL2_Q2(NN,AA) = (2.D0 * ALPHA(2))   &
                         & + (6.D0  * ALPHA(3)* H_n(NN,(POLYQ1(NN)+1)))  &
                         & + (12.D0  * ALPHA(4)* (H_n(NN,(POLYQ1(NN)+1)) **2.D0 ))
          ENDIF
        end if
      END DO

      KOEF_Q2(NN,0) = ALPHA(0)
      KOEF_Q2(NN,1) = ALPHA(1)
      KOEF_Q2(NN,2) = ALPHA(2)
      KOEF_Q2(NN,3) = ALPHA(3)
      KOEF_Q2(NN,4) = ALPHA(4)
      GRAD_Q2(NN) = POLYGRAD(NN)

    ELSE IF (POLYZAHL.EQ.12 .AND. WASTUN .EQ.'J') THEN
      IF (NN .EQ. 0 .AND. AUSGFUNKT .EQ. 'J') THEN
        WRITE(15,*)' AUSGABEDATEI DER A2-H-FUNKTIONEN DES PROFILS'
        WRITE(15,*)' -----------------------------------------------------------------'
      Endif
      write (*,*) 'Berechnung A2(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN
        WRITE(15,'(1X,F8.4,3X,I4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,4X)')  PROF(NN,0), POLYA2(NN)+1, &
           &       POLYGRAD(NN), ALPHA(0), ALPHA(1),ALPHA(2),ALPHA(3), ALPHA(4)
      ENDIF

      AA = 0
      DO AA = 0, MAAX_2(NN)
        IF (POLYA1(NN) .EQ. 0 .and. POLYA2(NN) .gt. 0) THEN
          POLY_A2(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,(AA +MAAX_1(NN))))  &
                         & + (ALPHA(2)* (TIEFE(NN,(AA +MAAX_1(NN)))**2.D0 ))  &
                         & + (ALPHA(3)* (TIEFE(NN,(AA +MAAX_1(NN)))**3.D0 ))  &
                         & + (ALPHA(4)* (TIEFE(NN,(AA +MAAX_1(NN)))**4.D0 ))
        ELSEIF (POLYA1(NN) .GT. 0 .and. POLYA2(NN) .GT. 0) THEN
          POLY_A2(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,(AA +MAAX_1(NN) +1)))  &
                         & + (ALPHA(2)* (TIEFE(NN,(AA +MAAX_1(NN) +1))**2.D0 ))   &
                         & + (ALPHA(3)* (TIEFE(NN,(AA +MAAX_1(NN) +1))**3.D0 ))   &
                         & + (ALPHA(4)* (TIEFE(NN,(AA +MAAX_1(NN) +1))**4.D0 ))
        ENDIF
        if (AA .eq. 0) then
          IF (POLYA1(NN) .EQ. 0 .and. POLYA2(NN) .gt. 0) THEN
            POLY_A2(NN,AA) = ALPHA(0) + (ALPHA(1) *H_n(NN,POLYA1(NN)))  &
                         & + (ALPHA(2)* (H_n(NN,POLYA1(NN))**2.D0 ))  &
                         & + (ALPHA(3)* (H_n(NN,POLYA1(NN))**3.D0 ))  &
                         & + (ALPHA(4)* (H_n(NN,POLYA1(NN))**4.D0 ))
            ABLEIT_A2(NN,AA) = ALPHA(1) + (2.D0 * ALPHA(2)* H_n(NN,POLYA1(NN)))  &
                         &  + (3.D0  * ALPHA(3)* (H_n(NN,POLYA1(NN))**2.D0 ))  &
                         &  + (4.D0  * ALPHA(4)* (H_n(NN,POLYA1(NN))**3.D0 ))
            ABL2_A2(NN,AA) = (2.D0 * ALPHA(2))  &
                         & + (6.D0  * ALPHA(3)* H_n(NN,POLYA1(NN)))     &
                         & + (12.D0  * ALPHA(4)* (H_n(NN,POLYA1(NN))**2.D0 ))
          ELSEIF (POLYA1(NN) .GT. 0 .and. POLYA2(NN) .GT. 0) THEN
            POLY_A2(NN,AA) = ALPHA(0) + (ALPHA(1) *H_n(NN,(POLYA1(NN)+1)))  &
                         & + (ALPHA(2)* (H_n(NN,(POLYA1(NN)+1))**2.D0 ))   &
                         & + (ALPHA(3)* (H_n(NN,(POLYA1(NN)+1))**3.D0 ))   &
                         & + (ALPHA(4)* (H_n(NN,(POLYA1(NN)+1))**4.D0 ))
            ABLEIT_A2(NN,AA) = ALPHA(1) + (2.D0 * ALPHA(2)* H_n(NN,(POLYA1(NN)+1)))  &
                         & + (3.D0  * ALPHA(3)* (H_n(NN,(POLYA1(NN)+1))**2.D0 ))   &
                         & + (4.D0  * ALPHA(4)* (H_n(NN,(POLYA1(NN)+1))**3.D0 ))
            ABL2_A2(NN,AA) = (2.D0 * ALPHA(2))  &
                         & + (6.D0  * ALPHA(3)* H_n(NN,(POLYA1(NN)+1)))   &
                         & + (12.D0  * ALPHA(4)* (H_n(NN,(POLYA1(NN)+1))**2.D0 ))
          ENDIF
        end if
      END DO

      KOEF_A2(NN,0) = ALPHA(0)
      KOEF_A2(NN,1) = ALPHA(1)
      KOEF_A2(NN,2) = ALPHA(2)
      KOEF_A2(NN,3) = ALPHA(3)
      KOEF_A2(NN,4) = ALPHA(4)
      GRAD_A2(NN) = POLYGRAD(NN)
    END IF
  END DO   ! ENDE DER POLYNOM-SCHLEIFE FUER DIE FUNKTIONEN
END DO   ! ENDE DER DATEI-AUFRUF-SCHLEIFE FUER ALLE PROFILE



!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
! ERZEUGUNG DEs POLYNOMs Alpha-H FÜR kleiner H_BORDVOLL
!------------------------------------------------------------------------------------------
NN = 0
M = 0
DO NN = 0, NMAX     ! UEBER ALLE PROFILE
  XEND = POLYALP1(NN)
  DO POLYZAHL= 13, 13     ! START DER POLYNOMSCHLEIFE
    If (XEND .gt. 0) then
      CALL POLYNOMFKT(XEND)  ! AUFRUF DER SUBROUTINE ZUM ERSTELLEN DER FUNKTION 4ten GRADES
      XEND = POLYALP1(NN)     ! ZURUECKHOLEN DER TATSÄCHLICHEN ANZAHL DER DATENTUPEL NACH EINER
                                 ! EVTUELLEN REDUKTION FUER DIE POLYNOM-ERZEUGUNG
    ELSE
      ALPHA(0) = 0.0D0
      ALPHA(1) = 0.0D0
      ALPHA(2) = 0.0D0
      ALPHA(3) = 0.0D0
      ALPHA(4) = 0.0D0
      POLYGRAD(NN) = 0
    end if

    IF (NN .EQ. 0 .AND. AUSGFUNKT .EQ. 'J') THEN
      WRITE(16,*)' AUSGABEDATEI DER Alpha1-H-FUNKTIONEN DES PROFILS'
      WRITE(16,*)' -----------------------------------------------------------------'
    END IF
    write (*,*) 'Berechnung ALPHA1(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
    IF (AUSGFUNKT .EQ. 'J') THEN              ! wird nur erzeugt, wenn in der ini-datei gewuenscht
      WRITE(16,'(1X,F8.4,3X,I4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,4X)')  PROF(NN,0), POLYALP1(NN)+1, &
         &       POLYGRAD(NN), ALPHA(0), ALPHA(1),ALPHA(2),ALPHA(3), ALPHA(4)
    ENDIF

    AA = 0
    DO AA = 0, MAX_1(NN)
      POLY_ALP1(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,AA)) &
                    & + (ALPHA(2)* (TIEFE(NN,AA)**2.D0 ))   &
                    & + (ALPHA(3)* (TIEFE(NN,AA)**3.D0 ))   &
                    & + (ALPHA(4)* (TIEFE(NN,AA)**4.D0 ))
      IF (AA .eq. MAX_1(NN)) then
        ABLEIT_ALP1(NN,AA) = ALPHA(1) + (2.D0* ALPHA(2)* (TIEFE(NN,AA)))  &
                    & + (3.D0  * ALPHA(3)* (TIEFE(NN,AA)**2.D0 )) &
                    & + (4.D0  * ALPHA(4)* (TIEFE(NN,AA)**3.D0 ))
      End if
    END DO

    KOEF_ALPHA1(NN,0) = ALPHA(0)
    KOEF_ALPHA1(NN,1) = ALPHA(1)
    KOEF_ALPHA1(NN,2) = ALPHA(2)
    KOEF_ALPHA1(NN,3) = ALPHA(3)
    KOEF_ALPHA1(NN,4) = ALPHA(4)
    GRAD_ALPHA1(NN) = POLYGRAD(NN)

  END DO   ! ENDE DER POLYNOM-SCHLEIFE FUER DIE FUNKTIONEN
END DO   ! ENDE DER DATEI-AUFRUF-SCHLEIFE FUER ALLE PROFILE


! ERZEUGUNG DEs POLYNOMs Alpha-H FÜR groesser H_BORDVOLL
!------------------------------------------------------------------------------------------
NN = 0
M = 0
DO NN = 0, NMAX     ! UEBER ALLE PROFILE
  XEND = POLYALP2(NN)
  DO POLYZAHL= 14, 14     ! START DER POLYNOMSCHLEIFE
    If (XEND .gt. 0) then
      CALL POLYNOMFKT(XEND)  ! AUFRUF DER SUBROUTINE ZUM ERSTELLEN DER FUNKTION 4ten GRADES
      XEND = POLYALP2(NN)     ! ZURUECKHOLEN DER TATSÄCHLICHEN ANZAHL DER DATENTUPEL NACH EINER
                                 ! EVTUELLEN REDUKTION FUER DIE POLYNOM-ERZEUGUNG
    ELSE
      ALPHA(0) = 0.0D0
      ALPHA(1) = 0.0D0
      ALPHA(2) = 0.0D0
      ALPHA(3) = 0.0D0
      ALPHA(4) = 0.0D0
      POLYGRAD(NN) = 0
    end if

    IF (NN .EQ. 0 .AND. AUSGFUNKT .EQ. 'J') THEN
      WRITE(16,*)' AUSGABEDATEI DER Alpha2-H-FUNKTIONEN DES PROFILS'
      WRITE(16,*)' -----------------------------------------------------------------'
    END IF
    write (*,*) 'Berechnung ALPHA2(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
    IF (AUSGFUNKT .EQ. 'J') THEN              ! wird nur erzeugt, wenn in der ini-datei gewuenscht
       WRITE(16,'(1X,F8.4,3X,I4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,4X)')  PROF(NN,0), POLYALP2(NN)+1, &
          &       POLYGRAD(NN), ALPHA(0), ALPHA(1),ALPHA(2),ALPHA(3), ALPHA(4)
    ENDIF

    AA = 0
    DO AA = 0, MAX_2(NN)
      IF (POLYALP1(NN) .EQ. 0 .and. POLYALP2(NN) .gt. 0) THEN
      POLY_ALP2(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,(AA +MAX_1(NN)))) &
                   & + (ALPHA(2)* (TIEFE(NN,(AA +MAX_1(NN)))**2.D0 )) &
                   & + (ALPHA(3)* (TIEFE(NN,(AA +MAX_1(NN)))**3.D0 )) &
                   & + (ALPHA(4)* (TIEFE(NN,(AA +MAX_1(NN)))**4.D0 ))

      ELSEIF (POLYALP1(NN) .GT. 0 .and. POLYALP2(NN) .GT. 0) THEN
      POLY_ALP2(NN,AA) = ALPHA(0) + (ALPHA(1) *TIEFE(NN,(AA +MAX_1(NN) +1))) &
                   & + (ALPHA(2)* (TIEFE(NN,(AA +MAX_1(NN) +1))**2.D0 )) &
                   & + (ALPHA(3)* (TIEFE(NN,(AA +MAX_1(NN) +1))**3.D0 )) &
                   & + (ALPHA(4)* (TIEFE(NN,(AA +MAX_1(NN) +1))**4.D0 ))
      ENDIF

      IF (AA .eq. 0) then
        IF (POLYALP1(NN) .EQ. 0 .and. POLYALP2(NN) .gt. 0) THEN
        POLY_ALP2(NN,AA) = ALPHA(0) + (ALPHA(1) *Ha_n(NN,POLYALP1(NN))) &
                   & + (ALPHA(2)* (Ha_n(NN,POLYALP1(NN))**2.D0 )) &
                   & + (ALPHA(3)* (Ha_n(NN,POLYALP1(NN))**3.D0 )) &
                   & + (ALPHA(4)* (Ha_n(NN,POLYALP1(NN))**4.D0 ))
        ABLEIT_ALP2(NN,AA) = ALPHA(1) + (2.D0 * ALPHA(2)* Ha_n(NN,POLYALP1(NN)))  &
                   & + (3.D0  * ALPHA(3)* (Ha_n(NN,POLYALP1(NN))**2.D0 )) &
                   & + (4.D0  * ALPHA(4)* (Ha_n(NN,POLYALP1(NN))**3.D0 ))
        ELSEIF (POLYALP1(NN) .GT. 0 .and. POLYALP2(NN) .GT. 0) THEN
        POLY_ALP2(NN,AA) = ALPHA(0) + (ALPHA(1) *Ha_n(NN,(POLYALP1(NN)+1))) &
                   & + (ALPHA(2)* (Ha_n(NN,(POLYALP1(NN)+1))**2.D0 )) &
                   & + (ALPHA(3)* (Ha_n(NN,(POLYALP1(NN)+1))**3.D0 )) &
                   & + (ALPHA(4)* (Ha_n(NN,(POLYALP1(NN)+1))**4.D0 ))
        ABLEIT_ALP2(NN,AA) = ALPHA(1) + (2.D0 * ALPHA(2)* Ha_n(NN,(POLYALP1(NN)+1)))  &
                   & + (3.D0  * ALPHA(3)* (Ha_n(NN,(POLYALP1(NN)+1))**2.D0 )) &
                   & + (4.D0  * ALPHA(4)* (Ha_n(NN,(POLYALP1(NN)+1))**3.D0 ))
        endif
      END if
    END DO

    KOEF_ALPHA2(NN,0) = ALPHA(0)
    KOEF_ALPHA2(NN,1) = ALPHA(1)
    KOEF_ALPHA2(NN,2) = ALPHA(2)
    KOEF_ALPHA2(NN,3) = ALPHA(3)
    KOEF_ALPHA2(NN,4) = ALPHA(4)
    GRAD_ALPHA2(NN) = POLYGRAD(NN)

  END DO   ! ENDE DER POLYNOM-SCHLEIFE FUER DIE FUNKTIONEN
END DO   ! ENDE DER DATEI-AUFRUF-SCHLEIFE FUER ALLE PROFILE

END SUBROUTINE !SUB POLYKERN


!***************************************************************************************************
!**   SUBROUTINE SPLINES                                                                         ***
!**   ----------------------                                                                     ***
!**   ERZEUGEN DER  SPLINEFUNKTIONEN                                                             ***
!***************************************************************************************************

SUBROUTINE SPLINES
  USE VARIABLEN
  IMPLICIT NONE
  REAL (KIND=8) :: XX(1:4)       ! GESUCHTER Loesungsvektor
  REAL (KIND=8) :: A(1:4,1:4)    ! 2-dimensional array A(1:LDA,1:N); the matrix A
  REAL (KIND=8) :: Y(1:4)        ! N-vector Y(1:N); the right hand side of the system
  REAL (KIND=8) :: D(1:4)        ! N-vector
  INTEGER IPIVOT2(1:4)
  INTEGER LDA, NM                ! Zeilenanzahl der Matrix
  INTEGER MARK                   ! FEHLERPARAMETER CHOLESKY-VERFAHREN

!------------------------------------------------------------------------------------------
! ERZEUGUNG der SPLINES Fuer A-H UND Q-H und alpha
!------------------------------------------------------------------------------------------
NN = 0
DO NN = 0, NMAX     ! UEBER ALLE PROFILE
  DO SPLINEZAHL= 1, 3  ! START DER SPLINESCHLEIFE
    X11 = 1.0D0
    X21 = 1.0D0
    X31 = 0.0D0
    X41 = 0.0D0

    IF (SPLINEZAHL.EQ.1 .AND. WASTUN.EQ.'J' .AND. POLYQ1(NN).NE.0 .AND. POLYQ2(NN).NE.0) then   ! fuer Dreiteilige Polynome
      IF (AUSREISSER.EQ.'N') THEN
        X12 = TIEFE(NN,MQQX_1(NN))
        X13 = TIEFE(NN,MQQX_1(NN)) ** 2.0D0
        X14 = TIEFE(NN,MQQX_1(NN)) ** 3.0D0
        X22 = TIEFE(NN,(MQQX_1(NN)+1))
        X23 = TIEFE(NN,(MQQX_1(NN)+1)) ** 2.0D0
        X24 = TIEFE(NN,(MQQX_1(NN)+1)) ** 3.0D0
        X32 = 1.0D0
        X33 = 2.0D0 * TIEFE(NN,MQQX_1(NN))
        X34 = 3.0D0 * (TIEFE(NN,MQQX_1(NN)) ** 2.0D0)
        X42 = 1.0D0
        X43 = 2.0D0 * TIEFE(NN,(MQQX_1(NN)+1))
        X44 = 3.0D0 * (TIEFE(NN,(MQQX_1(NN)+1)) ** 2.0D0)
      ELSEIF (AUSREISSER .EQ. 'J') THEN
        X12 = H_n(NN,POLYQ1(NN))
        X13 = H_n(NN,POLYQ1(NN)) ** 2.0D0
        X14 = H_n(NN,POLYQ1(NN)) ** 3.0D0
        X22 = H_n(NN,(POLYQ1(NN)+1))
        X23 = H_n(NN,(POLYQ1(NN)+1)) ** 2.0D0
        X24 = H_n(NN,(POLYQ1(NN)+1)) ** 3.0D0
        X32 = 1.0D0
        X33 = 2.0D0 * H_n(NN,POLYQ1(NN))
        X34 = 3.0D0 * (H_n(NN,POLYQ1(NN)) ** 2.0D0)
        X42 = 1.0D0
        X43 = 2.0D0 * H_n(NN,(POLYQ1(NN)+1))
        X44 = 3.0D0 * (H_n(NN,(POLYQ1(NN)+1)) ** 2.0D0)
      END IF

      V1 =  POLY_Q1(NN,MQQX_1(NN))
      V2 =  POLY_Q2(NN,0)
      V3 =  ABLEIT_Q1(NN,MQQX_1(NN))
      V4 =  ABLEIT_Q2(NN,0)
      CALL UMFORMEN

   ELSEIF (SPLINEZAHL.EQ.2 .AND. WASTUN.EQ.'J' .AND. POLYA1(NN).NE.0 .AND. POLYA2(NN).NE.0) then   ! fuer Dreiteilige Polynome
      IF (AUSREISSER.EQ.'N') THEN
        X12 = TIEFE(NN,MAAX_1(NN))
        X13 = TIEFE(NN,MAAX_1(NN)) ** 2.0D0
        X14 = TIEFE(NN,MAAX_1(NN)) ** 3.0D0
        X22 = TIEFE(NN,(MAAX_1(NN)+1))
        X23 = TIEFE(NN,(MAAX_1(NN)+1)) ** 2.0D0
        X24 = TIEFE(NN,(MAAX_1(NN)+1)) ** 3.0D0
        X32 = 1.0D0
        X33 = 2.0D0 * TIEFE(NN,MAAX_1(NN))
        X34 = 3.0D0 * (TIEFE(NN,MAAX_1(NN)) ** 2.0D0)
        X42 = 1.0D0
        X43 = 2.0D0 * TIEFE(NN,(MAAX_1(NN)+1))
        X44 = 3.0D0 * (TIEFE(NN,(MAAX_1(NN)+1)) ** 2.0D0)
      ELSEIF (AUSREISSER .EQ. 'J') THEN
        X12 = H_n(NN,POLYA1(NN))
        X13 = H_n(NN,POLYA1(NN)) ** 2.0D0
        X14 = H_n(NN,POLYA1(NN)) ** 3.0D0
        X22 = H_n(NN,(POLYA1(NN)+1))
        X23 = H_n(NN,(POLYA1(NN)+1)) ** 2.0D0
        X24 = H_n(NN,(POLYA1(NN)+1)) ** 3.0D0
        X32 = 1.0D0
        X33 = 2.0D0 * H_n(NN,POLYA1(NN))
        X34 = 3.0D0 * (H_n(NN,POLYA1(NN)) ** 2.0D0)
        X42 = 1.0D0
        X43 = 2.0D0 * H_n(NN,(POLYA1(NN)+1))
        X44 = 3.0D0 * (H_n(NN,(POLYA1(NN)+1)) ** 2.0D0)
      END IF

      V1 =  POLY_A1(NN,MAAX_1(NN))
      V2 =  POLY_A2(NN,0)
      V3 =  ABLEIT_A1(NN,MAAX_1(NN))
      V4 =  ABLEIT_A2(NN,0)
      CALL UMFORMEN

    ELSEIF (SPLINEZAHL.EQ.3 .AND. POLYALP1(NN).NE.0 .AND. POLYALP2(NN).NE.0) then    ! SPLINE fuer die Alpha(h)-Funktion
      IF (AUSREISSER .EQ. 'N') THEN
        X12 = TIEFE(NN,MAX_1(NN))
        X13 = TIEFE(NN,MAX_1(NN)) ** 2.0D0
        X14 = TIEFE(NN,MAX_1(NN)) ** 3.0D0
        X22 = TIEFE(NN,(MAX_1(NN)+1))
        X23 = TIEFE(NN,(MAX_1(NN)+1)) ** 2.0D0
        X24 = TIEFE(NN,(MAX_1(NN)+1)) ** 3.0D0
        X32 = 1.0D0
        X33 = 2.0D0 * TIEFE(NN,MAX_1(NN))
        X34 = 3.0D0 * (TIEFE(NN,MAX_1(NN)) ** 2.0D0)
        X42 = 1.0D0
        X43 = 2.0D0 * TIEFE(NN,(MAX_1(NN)+1))
        X44 = 3.0D0 * (TIEFE(NN,(MAX_1(NN)+1)) ** 2.0D0)
      ELSEIF (AUSREISSER .EQ. 'J') THEN
        X12 = Ha_n(NN,POLYALP1(NN))
        X13 = Ha_n(NN,POLYALP1(NN)) ** 2.0D0
        X14 = Ha_n(NN,POLYALP1(NN)) ** 3.0D0
        X22 = Ha_n(NN,(POLYALP1(NN)+1))
        X23 = Ha_n(NN,(POLYALP1(NN)+1)) ** 2.0D0
        X24 = Ha_n(NN,(POLYALP1(NN)+1)) ** 3.0D0
        X32 = 1.0D0
        X33 = 2.0D0 * Ha_n(NN,POLYALP1(NN))
        X34 = 3.0D0 * (Ha_n(NN,POLYALP1(NN)) ** 2.0D0)
        X42 = 1.0D0
        X43 = 2.0D0 * Ha_n(NN,(POLYALP1(NN)+1))
        X44 = 3.0D0 * (Ha_n(NN,(POLYALP1(NN)+1)) ** 2.0D0)
      ENDIF
      V1 =  POLY_ALP1(NN,MAX_1(NN))
      V2 =  POLY_ALP2(NN,0)
      V3 =  ABLEIT_ALP1(NN,MAX_1(NN))
      V4 =  ABLEIT_ALP2(NN,0)

      CALL UMFORMEN
    End if


 ! UEBERGABE DER EINGANGSPARAMETER:
 !------------------------------------------------------------------------------
    IF (WASTUN.EQ.'J' .AND. SPLINEZAHL.eq.1 .AND. POLYQ1(NN).NE.0 .AND. POLYQ2(NN).NE.0) then
      NM = 4
      LDA = 4
      PPP = 0
      QQQ = 0
      DO PPP = 1,4
        DO QQQ = 1,4
          A(PPP,QQQ) = MATRIX(PPP,QQQ)
          Y(PPP) = VEKTOR(PPP)
        END DO
      END DO
      CALL GAUSSP(NM,A,LDA,IPIVOT2,MARK,D,Y,XX)
      POLYGRAD(NN) = 3
    ELSEIF (WASTUN.EQ.'J' .AND. SPLINEZAHL.eq.1 .AND. (POLYQ1(NN).EQ.0 .OR. POLYQ2(NN).EQ.0)) then
      DO PPP = 1,4
        XX(PPP) = 0.0D0
        POLYGRAD(NN) = 0
      END DO

    ElseIF (WASTUN.EQ.'J' .AND. SPLINEZAHL.eq.2 .AND. POLYA1(NN).NE.0 .AND. POLYA2(NN).NE.0) then
      NM = 4
      LDA = 4
      PPP = 0
      QQQ = 0
      DO PPP = 1,4
        DO QQQ = 1,4
          A(PPP,QQQ) = MATRIX(PPP,QQQ)
          Y(PPP) = VEKTOR(PPP)
        END DO
      END DO
      CALL GAUSSP(NM,A,LDA,IPIVOT2,MARK,D,Y,XX)
      POLYGRAD(NN) = 3
    ELSEIF (WASTUN.EQ.'J' .AND. SPLINEZAHL.eq.2 .AND. (POLYA1(NN).EQ.0 .OR. POLYA2(NN).EQ.0)) then
      DO PPP = 1,4
        XX(PPP) = 0.0D0
        POLYGRAD(NN) = 0
      END DO

    ELSEIF (SPLINEZAHL.EQ.3 .AND. POLYALP1(NN) .NE. 0 .AND. POLYALP2(NN) .NE. 0) then
      NM = 4
      LDA = 4
      PPP = 0
      QQQ = 0
      DO PPP = 1,4
         DO QQQ = 1,4
          A(PPP,QQQ) = MATRIX(PPP,QQQ)
          Y(PPP) = VEKTOR(PPP)
        END DO
      END DO
      CALL GAUSSP(NM,A,LDA,IPIVOT2,MARK,D,Y,XX)
      POLYGRAD(NN) = 3
    ELSEIF (SPLINEZAHL .EQ. 3 .AND. (POLYALP1(NN) .EQ. 0 .OR. POLYALP2(NN) .EQ. 0)) then
      DO PPP = 1,4
        XX(PPP) = 0.0D0
        POLYGRAD(NN) = 0
      END DO
    END IF


    IF (SPLINEZAHL.EQ.1 .AND. WASTUN.EQ.'J') THEN
      write (*,*) 'Berechnung SPLINE-Q3(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN
        WRITE(14,'(1X,F8.4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7)') PROF(NN,0),POLYGRAD(NN), XX(1), XX(2), XX(3), XX(4)
      ENDIF

      AA = 0
      DO AA = MQQX_1(NN), (MQQX_1(NN)+1)
        POLY_Q3(NN,AA) = XX(1) + (XX(2) *TIEFE(NN,AA)) + (XX(3)* (TIEFE(NN,AA)**2)) + &
                     & (XX(4)* (TIEFE(NN,AA)**3))
      END DO
      KOEF_Q3(NN,0) = XX(1)
      KOEF_Q3(NN,1) = XX(2)
      KOEF_Q3(NN,2) = XX(3)
      KOEF_Q3(NN,3) = XX(4)
      KOEF_Q3(NN,4) = 0.0D0
      GRAD_Q3(NN)   = POLYGRAD(NN)


    ELSE IF (SPLINEZAHL.EQ.2  .AND. WASTUN.EQ.'J') THEN
      write (*,*) 'Berechnung SPLINE-A3(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN
        WRITE(15,'(1X,F8.4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7)')  PROF(NN,0),POLYGRAD(NN),XX(1),XX(2),XX(3),XX(4)
      ENDIF

      AA = 0
      DO AA = MAAX_1(NN), (MAAX_1(NN)+1)
        POLY_A3(NN,AA) = XX(1) + (XX(2) *TIEFE(NN,AA)) + (XX(3)* (TIEFE(NN,AA)**2)) + &
                     & (XX(4)* (TIEFE(NN,AA)**3))
      END DO
      KOEF_A3(NN,0) = XX(1)
      KOEF_A3(NN,1) = XX(2)
      KOEF_A3(NN,2) = XX(3)
      KOEF_A3(NN,3) = XX(4)
      KOEF_A3(NN,4) = 0.0D0
      GRAD_A3(NN)   = POLYGRAD(NN)


    ELSE IF (SPLINEZAHL .EQ. 3) THEN
      write (*,*) 'Berechnung SPLINE-ALPHA3(h) FUER profil' ,PROF(NN,0), 'IST ABGESCHLOSSEN'
      IF (AUSGFUNKT .EQ. 'J') THEN
        WRITE(16,'(1X,F8.4,3X,I2,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7)')  PROF(NN,0),POLYGRAD(NN),XX(1),XX(2),XX(3),XX(4)
      ENDIF

      AA = 0
      DO AA = MAX_1(NN), (MAX_1(NN)+1)
        POLY_ALP3(NN,AA) = XX(1) + (XX(2) *TIEFE(NN,AA)) + (XX(3)* (TIEFE(NN,AA)**2)) + &
                     & (XX(4)* (TIEFE(NN,AA)**3))
      END DO
      KOEF_ALPHA3(NN,0) = XX(1)
      KOEF_ALPHA3(NN,1) = XX(2)
      KOEF_ALPHA3(NN,2) = XX(3)
      KOEF_ALPHA3(NN,3) = XX(4)
      KOEF_ALPHA3(NN,4) = 0.0D0
      GRAD_ALPHA3(NN)   = POLYGRAD(NN)
    END IF
  END DO  ! ENDE SCHEILFE UEBER DIE SPLINEFUNKTIONEN
END DO   ! ENDE DER DATEI-AUFRUF-SCHLEIFE FUER ALLE PROFILE
write (*,*) 'Generierung aller Polynome und Splines je Profil abgeschlossen'

IF (AUSGFUNKT .EQ. 'J') THEN
  CLOSE(14)    ! SCHLIESSEN DER H-Q-FUNKTIONEN-AUSGABEDATEI
  CLOSE(15)    ! SCHLIESSEN DER A-Q-FUNKTIONEN-AUSGABEDATEI
  CLOSE(16)    ! SCHLIESSEN DER alpha-Q-FUNKTIONEN-AUSGABEDATEI
ENDIF

END SUBROUTINE !SUB EINLESEN


!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   BERECHNUNG DER MATRIX MIT AUFLOESUNG NACH GAUSS                                            ***
!***************************************************************************************************

SUBROUTINE UMFORMEN
USE VARIABLEN
IMPLICIT NONE

! GLEICHUNG TEIL1 = glch1
  MATRIX(1,1) = X11
  MATRIX(1,2) = X12
  MATRIX(1,3) = X13
  MATRIX(1,4) = X14
  VEKTOR(1) = V1

! GLEICHUNG TEIL2 = (glch1 - glch2)
  MATRIX(2,1) = X21
  MATRIX(2,2) = X22
  MATRIX(2,3) = X23
  MATRIX(2,4) = X24
  VEKTOR(2) = V2

! GLEICHUNG TEIL3 = (glch3 - glch4)
  MATRIX(3,1) = X31
  MATRIX(3,2) = X32
  MATRIX(3,3) = X33
  MATRIX(3,4) = X34
  VEKTOR(3) = V3

! GLEICHUNG TEIL4 = (glch5 - glch6)
  MATRIX(4,1) = X41
  MATRIX(4,2) = X42
  MATRIX(4,3) = X43
  MATRIX(4,4) = X44
  VEKTOR(4) = V4

END SUBROUTINE !SUB UMFORMEN



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   AUSGABE DER POLYNOMFUNKTIONEN SORTIERT                                                     ***
!***************************************************************************************************

SUBROUTINE POLYSORTIERT_1
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE        ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT17 ) AUSPOLYNOME     ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=17 ) POLYNTXT            ! PFAD DER DATEI MIT RICHTIGER LAENGE

POLYNTXT = 'Polynome.TXT'
AUSGABE = AUSGABEPFAD(1:TEXTAUSG)
AUSPOLYNOME  = AUSGABE // POLYNTXT


! OPEN (17, FILE='02Ausgang\Polynome.TXT', STATUS = 'replace')
OPEN (17, FILE=AUSPOLYNOME, STATUS = 'replace')
NN = 0
M = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  IF (NN .EQ. 0) THEN

    WRITE(17,*)' '                                                                     ! ZEILE 1
    WRITE(17,*)' AUSGABEDATEI FUER DER POLYNOMKOEFFIZIENTEN JE PROFIL!!'
    WRITE(17,*)' -----------------------------------------------------------------------------------'
  WRITE(17,*)'   PROFIL   Poly  N   TYP Grad Tiefemin    Tiefemax     Koeff 0               Koeff 1                &
              & Koeff 2               Koeff 3               Koeff 4               '
  WRITE(17,*)' -***.****--****--**--**--****-*****.***---*****.***----**.****************---**.****************--&
              &-**.****************---**.****************---**.****************'
  END IF

  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Q1 (h)',KENN(NN),'Q',GRAD_Q1(NN),TIEFE(NN,0),TIEFE(NN,MQQX_1(NN)), &
     & KOEF_Q1(NN,0),KOEF_Q1(NN,1),KOEF_Q1(NN,2),KOEF_Q1(NN,3),KOEF_Q1(NN,4)

  IF (WASTUN .EQ. 'J' .AND. POLYQ1(NN).GT.0 .AND. POLYQ2(NN).GT.0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_Q ',KENN(NN),'Q',GRAD_Q3(NN),TIEFE(NN,MQQX_1(NN)),TIEFE(NN,(MQQX_1(NN)+1)), &
       & KOEF_Q3(NN,0),KOEF_Q3(NN,1),KOEF_Q3(NN,2),KOEF_Q3(NN,3),KOEF_Q3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Q2 (h)',KENN(NN),'Q',GRAD_Q2(NN),TIEFE(NN,(MQQX_1(NN)+1)),TIEFE(NN,(MQQX_1(NN)+MQQX_2(NN)+1)), &
       & KOEF_Q2(NN,0),KOEF_Q2(NN,1),KOEF_Q2(NN,2),KOEF_Q2(NN,3),KOEF_Q2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYQ1(NN).EQ.0 .AND. POLYQ2(NN).GT.0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_Q ',KENN(NN),'Q',GRAD_Q3(NN),TIEFE(NN,MQQX_1(NN)),TIEFE(NN,(MQQX_1(NN))), &
       & KOEF_Q3(NN,0),KOEF_Q3(NN,1),KOEF_Q3(NN,2),KOEF_Q3(NN,3),KOEF_Q3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Q2 (h)',KENN(NN),'Q',GRAD_Q2(NN),TIEFE(NN,(MQQX_1(NN))),TIEFE(NN,(MQQX_1(NN)+MQQX_2(NN))), &
       & KOEF_Q2(NN,0),KOEF_Q2(NN,1),KOEF_Q2(NN,2),KOEF_Q2(NN,3),KOEF_Q2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYQ2(NN).EQ.0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_Q ',KENN(NN),'Q',GRAD_Q3(NN),TIEFE(NN,MQQX_1(NN)),TIEFE(NN,MQQX_1(NN)), &
       & KOEF_Q3(NN,0),KOEF_Q3(NN,1),KOEF_Q3(NN,2),KOEF_Q3(NN,3),KOEF_Q3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Q2 (h)',KENN(NN),'Q',GRAD_Q2(NN),TIEFE(NN,MQQX_1(NN)),TIEFE(NN,MQQX_1(NN)), &
       & KOEF_Q2(NN,0),KOEF_Q2(NN,1),KOEF_Q2(NN,2),KOEF_Q2(NN,3),KOEF_Q2(NN,4)
  END IF


  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'A1 (h)',KENN(NN),'A',GRAD_A1(NN),TIEFE(NN,0),TIEFE(NN,MAAX_1(NN)), &
     & KOEF_A1(NN,0),KOEF_A1(NN,1),KOEF_A1(NN,2),KOEF_A1(NN,3),KOEF_A1(NN,4)

  IF (WASTUN .EQ. 'J' .AND. POLYA1(NN).gt.0 .AND. POLYA2(NN).GT.0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_A ',KENN(NN),'A',GRAD_A3(NN),TIEFE(NN,MAAX_1(NN)),TIEFE(NN,(MAAX_1(NN)+1)), &
       & KOEF_A3(NN,0),KOEF_A3(NN,1),KOEF_A3(NN,2),KOEF_A3(NN,3),KOEF_A3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'A2 (h)',KENN(NN),'A',GRAD_A2(NN),TIEFE(NN,(MAAX_1(NN)+1)),TIEFE(NN,(MAAX_1(NN)+MAAX_2(NN)+1)), &
       & KOEF_A2(NN,0),KOEF_A2(NN,1),KOEF_A2(NN,2),KOEF_A2(NN,3),KOEF_A2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYA1(NN).EQ.0 .AND. POLYA2(NN).GT.0) THEN
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_A ',KENN(NN),'A',GRAD_A3(NN),TIEFE(NN,MAAX_1(NN)),TIEFE(NN,(MAAX_1(NN))), &
       & KOEF_A3(NN,0),KOEF_A3(NN,1),KOEF_A3(NN,2),KOEF_A3(NN,3),KOEF_A3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'A2 (h)',KENN(NN),'A',GRAD_A2(NN),TIEFE(NN,(MAAX_1(NN))),TIEFE(NN,(MAAX_1(NN)+MAAX_2(NN))), &
       & KOEF_A2(NN,0),KOEF_A2(NN,1),KOEF_A2(NN,2),KOEF_A2(NN,3),KOEF_A2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYA2(NN) .EQ. 0) THEN
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_A ',KENN(NN),'A',GRAD_A3(NN),TIEFE(NN,MAAX_1(NN)),TIEFE(NN,MAAX_1(NN)), &
       & KOEF_A3(NN,0),KOEF_A3(NN,1),KOEF_A3(NN,2),KOEF_A3(NN,3),KOEF_A3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'A2 (h)',KENN(NN),'A',GRAD_A2(NN),TIEFE(NN,MAAX_1(NN)),TIEFE(NN,MAAX_1(NN)), &
       & KOEF_A2(NN,0),KOEF_A2(NN,1),KOEF_A2(NN,2),KOEF_A2(NN,3),KOEF_A2(NN,4)
  END IF


  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al1(h)',KENN(NN),'a',GRAD_ALPHA1(NN),TIEFE(NN,0),TIEFE(NN,MAX_1(NN)), &
     & KOEF_ALPHA1(NN,0),KOEF_ALPHA1(NN,1),KOEF_ALPHA1(NN,2),KOEF_ALPHA1(NN,3),KOEF_ALPHA1(NN,4)

  if (POLYALP1(NN) .gt. 0 .AND. POLYALP2(NN) .gt. 0) then
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Spl_al',KENN(NN),'a',GRAD_ALPHA3(NN),TIEFE(NN,MAX_1(NN)),TIEFE(NN,(MAX_1(NN)+1)), &
     & KOEF_ALPHA3(NN,0),KOEF_ALPHA3(NN,1),KOEF_ALPHA3(NN,2),KOEF_ALPHA3(NN,3),KOEF_ALPHA3(NN,4)
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al2(h)',KENN(NN),'a',GRAD_ALPHA2(NN),TIEFE(NN,(MAX_1(NN)+1)),TIEFE(NN,(MAX_1(NN)+MAX_2(NN)+1))&
     & ,KOEF_ALPHA2(NN,0),KOEF_ALPHA2(NN,1),KOEF_ALPHA2(NN,2),KOEF_ALPHA2(NN,3),KOEF_ALPHA2(NN,4)

  elseif (POLYALP1(NN) .eq. 0 .AND. POLYALP2(NN) .gt. 0) then
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Spl_al',KENN(NN),'a',GRAD_ALPHA3(NN),TIEFE(NN,MAX_1(NN)),TIEFE(NN,MAX_1(NN)), &
     & KOEF_ALPHA3(NN,0),KOEF_ALPHA3(NN,1),KOEF_ALPHA3(NN,2),KOEF_ALPHA3(NN,3),KOEF_ALPHA3(NN,4)
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al2(h)',KENN(NN),'a',GRAD_ALPHA2(NN),TIEFE(NN,(MAX_1(NN))),TIEFE(NN,(MAX_1(NN)+MAX_2(NN)))&
     & ,KOEF_ALPHA2(NN,0),KOEF_ALPHA2(NN,1),KOEF_ALPHA2(NN,2),KOEF_ALPHA2(NN,3),KOEF_ALPHA2(NN,4)

  elseif (POLYALP2(NN) .EQ. 0) then
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Spl_al',KENN(NN),'a',GRAD_ALPHA3(NN),TIEFE(NN,MAX_1(NN)),TIEFE(NN,MAX_1(NN)), &
     & KOEF_ALPHA3(NN,0),KOEF_ALPHA3(NN,1),KOEF_ALPHA3(NN,2),KOEF_ALPHA3(NN,3),KOEF_ALPHA3(NN,4)
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al2(h)',KENN(NN),'a',GRAD_ALPHA2(NN),TIEFE(NN,MAX_1(NN)),TIEFE(NN,MAX_1(NN))&
     & ,KOEF_ALPHA2(NN,0),KOEF_ALPHA2(NN,1),KOEF_ALPHA2(NN,2),KOEF_ALPHA2(NN,3),KOEF_ALPHA2(NN,4)
  endif

END DO
Close (17)

END SUBROUTINE !SUB POLYSORTIERT



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   AUSGABE DER POLYNOMFUNKTIONEN SORTIERT                                                     ***
!***************************************************************************************************

SUBROUTINE POLYSORTIERT_2
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE        ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT17 ) AUSPOLYNOME     ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=17 ) POLYNTXT            ! PFAD DER DATEI MIT RICHTIGER LAENGE

POLYNTXT = 'Polynome.TXT'
AUSGABE = AUSGABEPFAD(1:TEXTAUSG)
AUSPOLYNOME  = AUSGABE // POLYNTXT


! OPEN (17, FILE='02Ausgang\Polynome.TXT', STATUS = 'replace')
OPEN (17, FILE=AUSPOLYNOME, STATUS = 'replace')
NN = 0
M = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  IF (NN .EQ. 0) THEN

    WRITE(17,*)' '                                                                     ! ZEILE 1
    WRITE(17,*)' AUSGABEDATEI FUER DER POLYNOMKOEFFIZIENTEN JE PROFIL!!'
    WRITE(17,*)' -----------------------------------------------------------------------------------'
  WRITE(17,*)'   PROFIL   Poly  N   TYP Grad Tiefemin    Tiefemax     Koeff 0               Koeff 1                &
              & Koeff 2               Koeff 3               Koeff 4               '
  WRITE(17,*)' -***.****--****--**--**--****-*****.***---*****.***----**.****************---**.****************--&
              &-**.****************---**.****************---**.****************'
  END IF

  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Q1 (h)',KENN(NN),'Q',GRAD_Q1(NN),TIEFE(NN,0),H_n(NN,POLYQ1(NN)), &
     & KOEF_Q1(NN,0),KOEF_Q1(NN,1),KOEF_Q1(NN,2),KOEF_Q1(NN,3),KOEF_Q1(NN,4)

  IF (WASTUN .EQ. 'J' .AND. POLYQ1(NN).GT.0 .AND. POLYQ2(NN) .GT. 0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_Q ',KENN(NN),'Q',GRAD_Q3(NN),H_n(NN,POLYQ1(NN)),H_n(NN,(POLYQ1(NN)+1)), &
       & KOEF_Q3(NN,0),KOEF_Q3(NN,1),KOEF_Q3(NN,2),KOEF_Q3(NN,3),KOEF_Q3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Q2 (h)',KENN(NN),'Q',GRAD_Q2(NN),H_n(NN,(POLYQ1(NN)+1)),H_n(NN,(POLYQ1(NN)+POLYQ2(NN)+1)), &
       & KOEF_Q2(NN,0),KOEF_Q2(NN,1),KOEF_Q2(NN,2),KOEF_Q2(NN,3),KOEF_Q2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYQ1(NN) .EQ. 0 .AND. POLYQ2(NN) .GT. 0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_Q ',KENN(NN),'Q',GRAD_Q3(NN),H_n(NN,POLYQ1(NN)),H_n(NN,POLYQ1(NN)), &
       & KOEF_Q3(NN,0),KOEF_Q3(NN,1),KOEF_Q3(NN,2),KOEF_Q3(NN,3),KOEF_Q3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Q2 (h)',KENN(NN),'Q',GRAD_Q2(NN),H_n(NN,POLYQ1(NN)),H_n(NN,(POLYQ1(NN)+POLYQ2(NN))), &
       & KOEF_Q2(NN,0),KOEF_Q2(NN,1),KOEF_Q2(NN,2),KOEF_Q2(NN,3),KOEF_Q2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYQ2(NN) .EQ. 0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_Q ',KENN(NN),'Q',GRAD_Q3(NN),H_n(NN,POLYQ1(NN)),H_n(NN,POLYQ1(NN)), &
       & KOEF_Q3(NN,0),KOEF_Q3(NN,1),KOEF_Q3(NN,2),KOEF_Q3(NN,3),KOEF_Q3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Q2 (h)',KENN(NN),'Q',GRAD_Q2(NN),H_n(NN,POLYQ1(NN)),H_n(NN,POLYQ1(NN)), &
       & KOEF_Q2(NN,0),KOEF_Q2(NN,1),KOEF_Q2(NN,2),KOEF_Q2(NN,3),KOEF_Q2(NN,4)
  END IF


  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'A1 (h)',KENN(NN),'A',GRAD_A1(NN),TIEFE(NN,0),H_n(NN,POLYA1(NN)), &
     & KOEF_A1(NN,0),KOEF_A1(NN,1),KOEF_A1(NN,2),KOEF_A1(NN,3),KOEF_A1(NN,4)

  IF (WASTUN .EQ. 'J' .AND. POLYA1(NN).gt.0 .AND. POLYA2(NN) .GT. 0) THEN     ! Dreiteiliges Polynom sollen erzeugt werden
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_A ',KENN(NN),'A',GRAD_A3(NN),H_n(NN,POLYA1(NN)),H_n(NN,(POLYA1(NN)+1)), &
       & KOEF_A3(NN,0),KOEF_A3(NN,1),KOEF_A3(NN,2),KOEF_A3(NN,3),KOEF_A3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'A2 (h)',KENN(NN),'A',GRAD_A2(NN),H_n(NN,(POLYA1(NN)+1)),H_n(NN,(POLYA1(NN)+POLYA2(NN)+1)), &
       & KOEF_A2(NN,0),KOEF_A2(NN,1),KOEF_A2(NN,2),KOEF_A2(NN,3),KOEF_A2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYA1(NN).EQ.0 .AND. POLYA2(NN) .GT. 0) THEN
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_A ',KENN(NN),'A',GRAD_A3(NN),H_n(NN,POLYA1(NN)),H_n(NN,POLYA1(NN)), &
       & KOEF_A3(NN,0),KOEF_A3(NN,1),KOEF_A3(NN,2),KOEF_A3(NN,3),KOEF_A3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'A2 (h)',KENN(NN),'A',GRAD_A2(NN),H_n(NN,POLYA1(NN)),H_n(NN,(POLYA1(NN)+POLYA2(NN))), &
       & KOEF_A2(NN,0),KOEF_A2(NN,1),KOEF_A2(NN,2),KOEF_A2(NN,3),KOEF_A2(NN,4)

  ELSEIF (WASTUN .EQ. 'J' .AND. POLYA2(NN).EQ.0) THEN
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'Spl_A ',KENN(NN),'A',GRAD_A3(NN),H_n(NN,POLYA1(NN)),H_n(NN,POLYA1(NN)), &
       & KOEF_A3(NN,0),KOEF_A3(NN,1),KOEF_A3(NN,2),KOEF_A3(NN,3),KOEF_A3(NN,4)
    WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
       & PROF(NN,0),'A2 (h)',KENN(NN),'A',GRAD_A2(NN),H_n(NN,POLYA1(NN)),H_n(NN,POLYA1(NN)), &
       & KOEF_A2(NN,0),KOEF_A2(NN,1),KOEF_A2(NN,2),KOEF_A2(NN,3),KOEF_A2(NN,4)
  END IF


  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al1(h)',KENN(NN),'a',GRAD_ALPHA1(NN),TIEFE(NN,0),Ha_n(NN,POLYALP1(NN)), &
     & KOEF_ALPHA1(NN,0),KOEF_ALPHA1(NN,1),KOEF_ALPHA1(NN,2),KOEF_ALPHA1(NN,3),KOEF_ALPHA1(NN,4)

  if (POLYALP1(NN) .gt. 0 .AND. POLYALP2(NN) .gt. 0) then
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Spl_al',KENN(NN),'a',GRAD_ALPHA3(NN),Ha_n(NN,POLYALP1(NN)),Ha_n(NN,(POLYALP1(NN)+1)), &
     & KOEF_ALPHA3(NN,0),KOEF_ALPHA3(NN,1),KOEF_ALPHA3(NN,2),KOEF_ALPHA3(NN,3),KOEF_ALPHA3(NN,4)
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al2(h)',KENN(NN),'a',GRAD_ALPHA2(NN),Ha_n(NN,(POLYALP1(NN)+1)),Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN)+1))&
     & ,KOEF_ALPHA2(NN,0),KOEF_ALPHA2(NN,1),KOEF_ALPHA2(NN,2),KOEF_ALPHA2(NN,3),KOEF_ALPHA2(NN,4)

  elseif (POLYALP1(NN) .eq. 0 .AND. POLYALP2(NN) .gt. 0) then
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Spl_al',KENN(NN),'a',GRAD_ALPHA3(NN),Ha_n(NN,POLYALP1(NN)),Ha_n(NN,POLYALP1(NN)), &
     & KOEF_ALPHA3(NN,0),KOEF_ALPHA3(NN,1),KOEF_ALPHA3(NN,2),KOEF_ALPHA3(NN,3),KOEF_ALPHA3(NN,4)
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al2(h)',KENN(NN),'a',GRAD_ALPHA2(NN),Ha_n(NN,POLYALP1(NN)),Ha_n(NN,(POLYALP1(NN)+POLYALP2(NN)))&
     & ,KOEF_ALPHA2(NN,0),KOEF_ALPHA2(NN,1),KOEF_ALPHA2(NN,2),KOEF_ALPHA2(NN,3),KOEF_ALPHA2(NN,4)

  elseif (POLYALP2(NN) .EQ. 0) then
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'Spl_al',KENN(NN),'a',GRAD_ALPHA3(NN),Ha_n(NN,POLYALP1(NN)),Ha_n(NN,POLYALP1(NN)), &
     & KOEF_ALPHA3(NN,0),KOEF_ALPHA3(NN,1),KOEF_ALPHA3(NN,2),KOEF_ALPHA3(NN,3),KOEF_ALPHA3(NN,4)
  WRITE(17,'(2X,F8.4,3X,A6,2X,A1,2X,A1,2X,I2,3X,F9.3,3X,F9.3,3X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12,2X,D20.12)') &
     & PROF(NN,0),'al2(h)',KENN(NN),'a',GRAD_ALPHA2(NN),Ha_n(NN,POLYALP1(NN)),Ha_n(NN,POLYALP1(NN))&
     & ,KOEF_ALPHA2(NN,0),KOEF_ALPHA2(NN,1),KOEF_ALPHA2(NN,2),KOEF_ALPHA2(NN,3),KOEF_ALPHA2(NN,4)
  endif

END DO
Close (17)

END SUBROUTINE !SUB POLYSORTIERT



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   GEGENUEBERSTELLUNG DER PROFILDATEN                                                         ***
!***************************************************************************************************

SUBROUTINE DELTAS
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE         ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT18 )  AUSKONTROLL     ! PFAD DER DATEI MIT RICHTIGER LAENGE

AUSGABE      = AUSGABEPFAD
AUSKONTROLL  = AUSGABE // 'KONTROLLE.TXT'

NN = 0
AA = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  DO AA = 0, MQQX_1(NN)
    DELT_Q(NN,AA) = POLY_Q1(NN,AA) - Q(NN,AA)
    GRENZMAX(NN) = MQQX_1(NN)
  END DO

  DO AA = 0, MAAX_1(NN)
    DELT_A(NN,AA) = POLY_A1(NN,AA) - FL_QM(NN,AA)
    GRENZMAX(NN) = MAAX_1(NN)
  END DO


  AA = 0
  DO AA = 0, MQQX_2(NN)
    IF (POLYQ1(NN) .EQ. 0 .and. POLYQ2(NN) .GT. 0) THEN
      DELT_Q(NN,(AA +MQQX_1(NN))) = POLY_Q2(NN,AA) - Q(NN,(AA +MQQX_1(NN)))
      GRENZMAX(NN) = (MQQX_1(NN) + MQQX_2(NN))
    ELSEIF (POLYQ1(NN) .GT. 0 .and. POLYQ2(NN) .GT. 0) THEN
      DELT_Q(NN,(AA +MQQX_1(NN) +1)) = POLY_Q2(NN,AA) - Q(NN,(AA +MQQX_1(NN) +1))
      GRENZMAX(NN) = (MQQX_1(NN) + MQQX_2(NN) + 1)
    END IF
  END DO

  DO AA = 0, MAAX_2(NN)
    IF (POLYA1(NN) .EQ. 0 .and. POLYA2(NN) .GT. 0) THEN
      DELT_A(NN,(AA +MAAX_1(NN))) = POLY_A2(NN,AA) - FL_QM(NN,(AA +MAAX_1(NN)))
      GRENZMAX(NN) = (MAAX_1(NN) + MAAX_2(NN))
    ELSEIF (POLYA1(NN) .GT. 0 .and. POLYA2(NN) .GT. 0) THEN
      DELT_A(NN,(AA +MAAX_1(NN) +1)) = POLY_A2(NN,AA) - FL_QM(NN,(AA +MAAX_1(NN) +1))
      GRENZMAX(NN) = (MAAX_1(NN) + MAAX_2(NN) + 1)
    END IF
  END DO

  AA = 0
  GRENZMAX(NN) = GRENZMAX(NN) + 1
  DO AA = GRENZMAX(NN), MMAX
    DELT_Q(NN,AA) = 0.0D0 - Q(NN,AA)
    DELT_A(NN,AA) = 0.0D0 - FL_QM(NN,AA)
  END DO
  GRENZMAX(NN) = GRENZMAX(NN) - 1

  AA = 0
  DO AA = 0, MAX_1(NN)
    DELT_ALP(NN,AA) = POLY_ALP1(NN,AA) - ALP_E(NN,AA)
    ALPHA_END(NN) = MAX_1(NN)
  END DO

  AA = 0
  DO AA = 0, MAX_2(NN)
     IF (POLYALP1(NN) .EQ. 0 .and. POLYALP2(NN) .gt. 0) THEN
       DELT_ALP(NN,(AA +MAX_1(NN))) = POLY_ALP2(NN,AA) - ALP_E(NN,(AA +MAX_1(NN)))
       ALPHA_END(NN) = MAX_1(NN) + MAX_2(NN)
     ELSEIF (POLYALP1(NN) .GT. 0 .and. POLYALP2(NN) .GT. 0) THEN
       DELT_ALP(NN,(AA +MAX_1(NN) +1)) = POLY_ALP2(NN,AA) - ALP_E(NN,(AA +MAX_1(NN) +1))
       ALPHA_END(NN) = MAX_1(NN) + MAX_2(NN) + 1
     ENDIF
  END DO

  ALPHA_END(NN) = ALPHA_END(NN) + 1
  AA = 0
  DO AA = ALPHA_END(NN), MMAX
    DELT_ALP(NN,AA) = 0.0D0 - ALP_E(NN,AA)
  END DO
  ALPHA_END(NN) = ALPHA_END(NN) - 1


END DO

IF (KONTROLLE .eq. 'J') THEN        ! wird nur erzeugt, wenn in der ini-datei gewuenscht
  OPEN (18, FILE= AUSKONTROLL, STATUS = 'replace')
  NN = 0
  M = 0
  DO NN = 0, NMAX      ! UEBER ALLE PROFILE
    WRITE(18,*)' '                                                                     ! ZEILE 1
    WRITE(18,*)' AUSGABEDATEI FUER DIE WERTEPAARE DES PROFILS', PROF(NN,0), '!!'
    WRITE(18,*)' -----------------------------------------------------------------------------------'
    WRITE(18,*)'   WSP in mNN      TIEFE in m        A in qm         Q in m3/s         ALPHA [-]        DEL A            &
                   & DEL Q            DEL ALPHA  '
    WRITE(18,*)' --**.********-------**.********----**.********-------**.********-------**.********-------**.********---- &
                 & ---**.********- -----**.********'
    DO M = 0, MMAX      ! UEBER ALLE ABFLUESSE
      WRITE(18,'(2X,F8.4,5X,F8.4,5X,F12.4,5X,F9.4,5X,F8.6,2X,D16.7,2X,D16.7,2X,D16.7)') WSP(NN,M),TIEFE(NN,M),FL_QM(NN,M), &
              & Q(NN,M),ALP_E(NN,M),DELT_A(NN,M),DELT_Q(NN,M),DELT_ALP(NN,M)
    END DO
  END DO
  Close (18)
END IF
 
END SUBROUTINE !SUB DELTAS



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   AUSWERTUNG DER DIFFERENZEN                                                                 ***
!***************************************************************************************************

SUBROUTINE ABWEICHUNG
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE         ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT18 )  AUSKONTROLL     ! PFAD DER DATEI MIT RICHTIGER LAENGE

AUSGABE      = AUSGABEPFAD
AUSKONTROLL  = AUSGABE // 'KONTRMAXI.TXT'

NN = 0
AA = 0
DO NN = 0, NMAX      ! UEBER ALLE PROFILE
  DO AA = 0, GRENZMAX(NN)
    ABW_Q(NN,AA) = DELT_Q(NN,AA) ** 2
    ABW_A(NN,AA) = DELT_A(NN,AA) ** 2
  END DO
    MAXI_Q(NN) = MAXVAL (ABW_Q(NN,0:GRENZMAX(NN)))
    MAXI_A(NN) = MAXVAL (ABW_A(NN,0:GRENZMAX(NN)))
  DO AA = 0, ALPHA_END(NN)
    ABW_ALP(NN,AA) = DELT_ALP(NN,AA) ** 2
  END DO
    MAXI_ALP(NN) = MAXVAL (ABW_ALP(NN,0:ALPHA_END(NN)))
END DO


IF (KONTROLLE .eq. 'J') THEN        ! wird nur erzeugt, wenn in der ini-datei gewuenscht
  OPEN (19, FILE= AUSKONTROLL, STATUS = 'replace')
  NN = 0
  M = 0
  DO NN = 0, NMAX      ! UEBER ALLE PROFILE
    IF (NN .EQ. 0) THEN
      WRITE(19,*)'  PROFIL       Q in m3/s     MAX (DEL A)      MAX (DEL Q)     MAX (DEL ALPHA) '
      WRITE(19,*)' -****.*****---***.****-----**.*********-----**.*********-----**.************'
    END IF
    DO AA = 0, MMAX      ! UEBER ALLE ABFLUESSE
      IF (ABW_Q(NN,AA).EQ.MAXI_Q(NN) .and. MAXI_Q(NN).ne.0.0D0) THEN
        WRITE(19,'(2X,F8.4,2X,A4,2X,F9.4,2X,D18.9,2X,D18.9,2X,D18.9)') PROF(NN,0),'maxQ',Q(NN,AA), &
                                 & ABW_A(NN,AA), MAXI_Q(NN), ABW_ALP(NN,AA)
      ENDIF
      IF (ABW_A(NN,AA) .EQ. MAXI_A(NN) .and. MAXI_A(NN).ne.0.0D0) THEN
        WRITE(19,'(2X,F8.4,2X,A4,2X,F9.4,2X,D18.9,2X,D18.9,2X,D18.9)') PROF(NN,0),'maxA', Q(NN,AA), &
                                 & MAXI_A(NN), ABW_Q(NN,AA), ABW_ALP(NN,AA)
      END IF
      IF (ABW_ALP(NN,AA) .EQ. MAXI_ALP(NN) .and. MAXI_ALP(NN).ne.0.0D0) THEN
        WRITE(19,'(2X,F8.4,2X,A4,2X,F9.4,2X,D18.9,2X,D18.9,2X,D18.9)') PROF(NN,0), 'maxa', Q(NN,AA), &
                                 & ABW_A(NN,AA), ABW_Q(NN,AA), MAXI_ALP(NN)
      END IF
    END DO
  END DO
  Close (19)
END IF
 
END SUBROUTINE !SUB ABWEICHUNG



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   AUSWERTUNG FUER AUSREISSER                                                                 ***
!***************************************************************************************************

SUBROUTINE AUSREISS
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE         ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT18 )  AUSAUSREISS     ! PFAD DER DATEI MIT RICHTIGER LAENGE

AUSGABE      = AUSGABEPFAD
AUSAUSREISS  = AUSGABE // 'AUSREISER.TXT'


IF (KONTROLLE .eq. 'J') THEN        ! wird nur erzeugt, wenn in der ini-datei gewuenscht
  OPEN (9, FILE= AUSAUSREISS, STATUS = 'replace')
  WRITE(9,*)'  PROFIL     Q in m3/s  Tiefe m    Poly   '
  WRITE(9,*)' -****.****--****.****--***.*****--****-'
  NN = 0
  DO NN = 0, NMAX
    DO M = 0, MMAX
      IF (MQQ_1(NN,M).EQ.M .OR. MQQ_2(NN,M).EQ.M) THEN
        ! keine Ausgabe
      elseIF (MQQ_1(NN,M).ne.M .and. M.le.MQQX_1(NN)) THEN
        WRITE(9,'(2X,F9.4,2X,F9.4,2X,F9.5,2X,9X,4X,A1)') PROF(NN,0),Q(NN,M), TIEFE(NN,M), '1'
      ELSEIF (MQQ_2(NN,M).ne.M .and. M.gt.MQQX_1(NN) .and. M.LE.(MQQX_2(NN)+MQQX_1(NN)) ) THEN
        WRITE(9,'(2X,F9.4,2X,F9.4,2X,F9.5,2X,9X,4X,A1)') PROF(NN,0),Q(NN,M), TIEFE(NN,M), '2'
      ELSEIF(M.gt.(MQQX_2(NN)+MQQX_1(NN))) THEN
        WRITE(9,'(2X,F9.4,2X,F9.4,2X,F9.5,2X,9X,4X,A1)') PROF(NN,0),Q(NN,M), TIEFE(NN,M), 'x'
      END IF
    END DO
  END DO

  WRITE(9,*)'  PROFIL     Q in m3/s  alpha      Poly '
  WRITE(9,*)' -****.****--****.****--**.******--****-'
  NN = 0
  M = 0
  DO NN = 0, NMAX
    DO M = 0, MMAX
      IF (MA_1(NN,M).EQ.M .OR. MA_2(NN,M).EQ.M) THEN
        ! keine Ausgabe
      ELSEIF (MA_1(NN,M).ne.M .and. M.le.MAX_1(NN)) THEN
        WRITE(9,'(2X,F9.4,2X,F9.4,2X,9X,2X,F9.5,4X,A1)') PROF(NN,0),Q(NN,M), ALP_E(NN,M), '1'
      ELSEIF (MA_2(NN,M).ne.M .and. M.gt.MAX_1(NN) .and. M.LE.(MAX_2(NN)+MAX_1(NN)) ) THEN
        WRITE(9,'(2X,F9.4,2X,F9.4,2X,9X,2X,F9.5,4X,A1)') PROF(NN,0),Q(NN,M), ALP_E(NN,M), '2'
      ELSEIF(M.gt.(MAX_2(NN)+MAX_1(NN))) THEN
        WRITE(9,'(2X,F9.4,2X,F9.4,2X,9X,2X,F9.5,4X,A1)') PROF(NN,0),Q(NN,M), ALP_E(NN,M), 'x'
      END IF
    END DO
  END DO
 Close (9)
END IF
 
END SUBROUTINE !SUB AUSREISS



!***************************************************************************************************
!**   ----------------------                                                                     ***
!**   DATENAUSGABE EINZELN JE PROFIL                                                             ***
!***************************************************************************************************

SUBROUTINE PROFILAUSGABE
USE VARIABLEN
IMPLICIT NONE
CHARACTER (LEN=TEXTAUSG) AUSGABE              ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT19 ) AUSPROF               ! PFAD DER DATEI MIT RICHTIGER LAENGE
CHARACTER (LEN=TEXT20 ) DATEINAME(0:2000)     ! PFAD DER DATEI MIT RICHTIGER LAENGE

AUSGABE  = AUSGABEPFAD
AUSPROF  = AUSGABE // 'PROF'

! ERSTELLEN DES AUSGABENAMEN
!------------------------------------
TXT = '.txt'
DATEIZAEHLER = 20
DO NN = 0, NMAX
  DATEINAME(NN) = AUSPROF // PROFNAME(NN)  // TXT
  ! DATEINAME(NN) = '02Ausgang\PROF' // PROFNAME(NN)  // TXT
END DO

NN = 0
M = 0

IF (AUTOBORDV .EQ.'N') THEN
  DO NN = 0, NMAX      ! UEBER ALLE PROFILE
    DATEIZAEHLER = DATEIZAEHLER + 1   ! NUMMERIERUNG DER DATEI VON 20 BIS .....
    OPEN (UNIT=DATEIZAEHLER, FILE=DATEINAME(NN), STATUS = 'replace')
    WRITE(UNIT=DATEIZAEHLER,*)' '
  WRITE(UNIT=DATEIZAEHLER,*)' AUSGABEDATEI FUER DIE WERTE DES PROFILS', PROF(NN,0), '!!'
  WRITE(UNIT=DATEIZAEHLER,*)' --------------------------------------------------------------------------------------------------'
  WRITE(UNIT=DATEIZAEHLER,*)'   bordvolle    Stuetzstellen  Stuetzstellen  maxi Q fuer   maxi H fuer   Sohlhoehe   stat [-]   '
  WRITE(UNIT=DATEIZAEHLER,*)'   Tiefe [m]    < bordvoll     > bordvoll     polyn m3/s    polyn m       mNN         gefaelle  '
  WRITE(UNIT=DATEIZAEHLER,*)' --***.****-----**********-----**********-----****.***-----**.*******-----***.****** --***.******'
  WRITE(UNIT=DATEIZAEHLER,'(A2,2X,F8.4,9X,I4,9X,I4,9X,F9.4,5X,F8.6,5X,F9.4,5X,F8.4)')  &
       &  'CC', H_BORDVOLL(NN), MQQX_1(NN)+1, MQQX_2(NN)+1, Q(NN,GRENZMAX(NN)), TIEFE(NN,GRENZMAX(NN)), SOHLPROF(NN), SLOPE(NN,0)

  WRITE(UNIT=DATEIZAEHLER,*)' '
  WRITE(UNIT=DATEIZAEHLER,*)' --------------------------------------------------------------------------------------------------'
  WRITE(UNIT=DATEIZAEHLER,*)'   bordvolle    Stuetzstellen  Stuetzstellen  maxi Q fuer  maxi alpha   maxi H fuer    '
  WRITE(UNIT=DATEIZAEHLER,*)'   Tiefe [m]    < bordvoll     > bordvoll     polyn m3/s   fuer polyn   polyn m '
  WRITE(UNIT=DATEIZAEHLER,*)' --***.****-----**********-----**********-----****.***-----**.******---*****.****---'
    WRITE(UNIT=DATEIZAEHLER,'(A2,2X,F8.4,9X,I4,9X,I4,9X,F9.4,5X,F8.6,5X,F8.4)')  &
        &  'CC', H_BORDVOLL(NN), MAX_1(NN)+1, MAX_2(NN)+1 &
        &  ,Q(NN,ALPHA_END(NN)), ALP_E(NN,ALPHA_END(NN)),TIEFE(NN,ALPHA_END(NN))

  WRITE(UNIT=DATEIZAEHLER,*)' '
  WRITE(UNIT=DATEIZAEHLER,*)' --------------------------------------------------------------------------------------------------'
  WRITE(UNIT=DATEIZAEHLER,*)'   WSP in mNN      TIEFE in m        A in qm         Q in m3/s         ALPHA [-]        DEL A        &
                               &     DEL Q            DEL ALPHA  '
  WRITE(UNIT=DATEIZAEHLER,*)' --**.********-------**.********----**.********-------**.********-------**.********-------**.********&
                               & -------**.********- -----**.********'
    DO M = 0, MMAX      ! UEBER ALLE ABFLUESSE
      WRITE(UNIT=DATEIZAEHLER,'(1X,D12.4,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7)') &
       &   WSP(NN,M),TIEFE(NN,M),FL_QM(NN,M), Q(NN,M),ALP_E(NN,M),DELT_A(NN,M),DELT_Q(NN,M),DELT_ALP(NN,M)
    END DO
    Close (UNIT=DATEIZAEHLER)
  END DO

ELSEIF (AUTOBORDV .EQ.'J') THEN
  DO NN = 0, NMAX      ! UEBER ALLE PROFILE
    DATEIZAEHLER = DATEIZAEHLER + 1   ! NUMMERIERUNG DER DATEI VON 20 BIS .....
    OPEN (UNIT=DATEIZAEHLER, FILE=DATEINAME(NN), STATUS = 'replace')
    WRITE(UNIT=DATEIZAEHLER,*)' '
  WRITE(UNIT=DATEIZAEHLER,*)' AUSGABEDATEI FUER DIE WERTE DES PROFILS', PROF(NN,0), '!!'
  WRITE(UNIT=DATEIZAEHLER,*)' --------------------------------------------------------------------------------------------------'
  WRITE(UNIT=DATEIZAEHLER,*)'   bordvolle    Stuetzstellen  Stuetzstellen  maxi Q fuer   maxi H fuer   Sohlhoehe   stat [-]   '
  WRITE(UNIT=DATEIZAEHLER,*)'   Tiefe [m]    < bordvoll     > bordvoll     polyn m3/s    polyn m       mNN         gefaelle  '
  WRITE(UNIT=DATEIZAEHLER,*)' --***.****-----**********-----**********-----****.***-----**.*******-----***.****** --***.******'
  WRITE(UNIT=DATEIZAEHLER,'(A2,2X,F8.4,9X,I4,9X,I4,9X,F9.4,5X,F8.6,5X,F9.4,5X,F8.4)')  &
       &  'CC', HQ_BORDVOLL(NN), MQQX_1(NN)+1, MQQX_2(NN)+1, Q(NN,GRENZMAX(NN)), TIEFE(NN,GRENZMAX(NN)), SOHLPROF(NN), SLOPE(NN,0)

  WRITE(UNIT=DATEIZAEHLER,*)' '
  WRITE(UNIT=DATEIZAEHLER,*)' --------------------------------------------------------------------------------------------------'
  WRITE(UNIT=DATEIZAEHLER,*)'   bordvolle    Stuetzstellen  Stuetzstellen  maxi Q fuer  maxi alpha   maxi H fuer    '
  WRITE(UNIT=DATEIZAEHLER,*)'   Tiefe [m]    < bordvoll     > bordvoll     polyn m3/s   fuer polyn   polyn m '
  WRITE(UNIT=DATEIZAEHLER,*)' --***.****-----**********-----**********-----****.***-----**.******---*****.****---'
    WRITE(UNIT=DATEIZAEHLER,'(A2,2X,F8.4,9X,I4,9X,I4,9X,F9.4,5X,F8.6,5X,F8.4)')  &
        &  'CC', HALP_BORDVOLL(NN), MAX_1(NN)+1, MAX_2(NN)+1 &
        &  ,Q(NN,ALPHA_END(NN)), ALP_E(NN,ALPHA_END(NN)),TIEFE(NN,ALPHA_END(NN))

  WRITE(UNIT=DATEIZAEHLER,*)' '
  WRITE(UNIT=DATEIZAEHLER,*)' --------------------------------------------------------------------------------------------------'
  WRITE(UNIT=DATEIZAEHLER,*)'   WSP in mNN      TIEFE in m        A in qm         Q in m3/s         ALPHA [-]        DEL A        &
                               &     DEL Q            DEL ALPHA  '
  WRITE(UNIT=DATEIZAEHLER,*)' --**.********-------**.********----**.********-------**.********-------**.********-------**.********&
                               & -------**.********- -----**.********'
    DO M = 0, MMAX      ! UEBER ALLE ABFLUESSE
      WRITE(UNIT=DATEIZAEHLER,'(1X,D12.4,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7,2X,D16.7)') &
       &   WSP(NN,M),TIEFE(NN,M),FL_QM(NN,M), Q(NN,M),ALP_E(NN,M),DELT_A(NN,M),DELT_Q(NN,M),DELT_ALP(NN,M)
    END DO
    Close (UNIT=DATEIZAEHLER)
  END DO
ENDIf

END SUBROUTINE !SUB PROFILAUSGABE


!***************************************************************************************************
!**   SUBROUTINE POLYNOMFUNKTION                                                                 ***
!**   ----------------------                                                                     ***
!**   BERECHNUNG DER POLYNOMFUNKTION                                                             ***
!***************************************************************************************************

 SUBROUTINE POLYNOMFKT

 USE VARIABLEN
 IMPLICIT NONE

 REAL (KIND=8) :: X(0:XEND)         ! X-WERTE DER DATEN
 REAL (KIND=8) :: F(0:XEND)         ! FUNKTIONSWERTE DER DATEN
 REAL (KIND=8) :: W(0:XEND)         ! WICHTUNGSFAKTOR
 REAL (KIND=8) :: A(0:XEND,0:XEND)    ! HILFSPARAMETER
 REAL (KIND=8) :: B(0:XEND)         ! HILFSPARAMETER
 REAL (KIND=8) :: Y(0:XEND)         ! HILFSPARAMETER
 REAL (KIND=8) :: Z(0:XEND)         ! HILFSPARAMETER
 REAL (KIND=8) :: C(0:XEND)         ! POLYNOMKOEFFIZIENTEN
 INTEGER LDA            ! POLYNOMGRAD
 INTEGER IERR           ! FEHLERPARAMETER (0 - ALLES IN ORDNUNG)
 ! INTEGER M              ! ANZAHL DER DATENTUPEL
 INTEGER MARK1          ! FEHLERPARAMETER CHOLESKY-VERFAHREN
 ! INTEGER N,NN


 !---------------------------------------------------------------------------------------------
 ! BERECHNUNG DER KOEFFIZIENTEN DER KOMPRESSORKENNLINIEN
 !---------------------------------------------------------------------------------------------
 N = 0
 RED = 0
 POLYGRAD(NN) = 0
 ALPHA(0) = 0.0D0
 ALPHA(1) = 0.0D0
 ALPHA(2) = 0.0D0
 ALPHA(3) = 0.0D0
 ALPHA(4) = 0.0D0
 55 DO N = 0, XEND    ! UEBER ALLE ABFLUESSE BIS BORDVOLL
     IF (POLYZAHL.EQ.1 .and. AUSREISSER.EQ.'N') THEN
       F(N) = Q(NN,N)
       X(N) = TIEFE(NN,N)
     ELSEIF (POLYZAHL.EQ.1 .and. AUSREISSER.EQ.'J') THEN
       F(N) = Q_n(NN,N)
       X(N) = H_n(NN,N)

     ELSEIF (POLYZAHL.EQ.2 .and. AUSREISSER.EQ.'N') THEN
       F(N) = FL_QM(NN,N)
       X(N) = TIEFE(NN,N)
     ELSEIF (POLYZAHL.EQ.2 .and. AUSREISSER.EQ.'J') THEN
       F(N) = A_n(NN,N)
       X(N) = H_n(NN,N)

     ELSEIF (POLYZAHL.EQ.13 .and. AUSREISSER.EQ.'N') THEN
       F(N) = ALP_E(NN,N)
       X(N) = TIEFE(NN,N)
     ELSEIF (POLYZAHL.EQ.13 .and. AUSREISSER.EQ.'J') THEN
       F(N) = alp_n(NN,N)
       X(N) = Ha_n(NN,N)

     ELSEIF (POLYZAHL.EQ.14  .and. AUSREISSER.EQ.'N') THEN
       If (POLYALP1(NN).eq.0) then
         F(N) = ALP_E(NN,(N +POLYALP1(NN)))
         X(N) = TIEFE(NN,(N +POLYALP1(NN)))
       elseif (POLYALP1(NN) .gt.0) then
         F(N) = ALP_E(NN,(N +POLYALP1(NN) +1))
         X(N) = TIEFE(NN,(N +POLYALP1(NN) +1))
       endif

     ELSEIF (POLYZAHL.EQ.14 .and. AUSREISSER.EQ.'J') THEN
       if (POLYALP1(NN).eq.0) then
         F(N) = alp_n(NN,(N +POLYALP1(NN)))
         X(N) = Ha_n(NN,(N +POLYALP1(NN)))
       elseif (POLYALP1(NN) .gt.0) then
         F(N) = alp_n(NN,(N +POLYALP1(NN) +1))
         X(N) = Ha_n(NN,(N +POLYALP1(NN) +1))
       end if

     ELSEIF (POLYZAHL.EQ.11 .and. AUSREISSER.EQ.'N') THEN
       if (POLYQ1(NN).eq.0 ) then
         F(N) = Q(NN,(N +POLYQ1(NN)))
         X(N) = TIEFE(NN,(N +POLYQ1(NN)))
       elseif (POLYQ1(NN).gt.0 ) then
         F(N) = Q(NN,(N +POLYQ1(NN) +1))
         X(N) = TIEFE(NN,(N +POLYQ1(NN) +1))
       end if
     ELSEIF (POLYZAHL.EQ.11 .and. AUSREISSER.EQ.'J') THEN
       if (POLYQ1(NN).eq.0 ) then
         F(N) = Q_n(NN,(N +POLYQ1(NN)))
         X(N) = H_n(NN,(N +POLYQ1(NN)))
       elseif (POLYQ1(NN).gt.0 ) then
         F(N) = Q_n(NN,(N +POLYQ1(NN) +1))
         X(N) = H_n(NN,(N +POLYQ1(NN) +1))
       end if

     ELSE IF (POLYZAHL.EQ.12 .and. AUSREISSER.EQ.'N') THEN
       if (POLYA1(NN) .eq. 0) then
         F(N) = FL_QM(NN,(N +POLYA1(NN)))
         X(N) = TIEFE(NN,(N +POLYA1(NN)))
       elseif (POLYA1(NN) .gt. 0) then
         F(N) = FL_QM(NN,(N +POLYA1(NN) +1))
         X(N) = TIEFE(NN,(N +POLYA1(NN) +1))
       END if
     ELSE IF (POLYZAHL.EQ.12 .and. AUSREISSER.EQ.'J') THEN
       if (POLYA1(NN) .eq. 0) then
         F(N) = A_n(NN,(N +POLYA1(NN)))
         X(N) = H_n(NN,(N +POLYA1(NN)))
       elseif (POLYA1(NN) .gt. 0) then
         F(N) = A_n(NN,(N +POLYA1(NN) +1))
         X(N) = H_n(NN,(N +POLYA1(NN) +1))
       END if
     END IF

     C(N) = 0.0D0
     W(N) = 1.0D0

     ! WICHTUNGSFUNKTION FUER ERSTEN UND LETZTEN WERT ERHOEHEN
     !--------------------------------------------------------------------
     IF (POLYZAHL.EQ.1 .or. POLYZAHL.EQ.2 .or. POLYZAHL.EQ.13) THEN
       W(XEND) = WFAKTOR
     ELSEIF (POLYZAHL.EQ.11 .or. POLYZAHL.EQ.12 .or. POLYZAHL.EQ.14) THEN
       W(0) = WFAKTOR
     END IF
   END DO


 IF (XEND .LT. 4) THEN
   N = XEND      ! POLYNOMGRAD ENTSPRECHEND DER ANZAHL DER DATENTUPEL
   POLYGRAD(NN) = N
 ELSEIF (RED .EQ. 0 .AND. XEND .GE. 4) THEN
   ! N = 4
   N = WASGRAD      ! POLYNOMGRAD aus der Vorgabe des Benutzer aus SubR EINSTELLUNGEN
   POLYGRAD(NN) = N
 ELSE
   N = RED
   POLYGRAD(NN) = N
 END IF
 M = XEND        ! DATENTUPELANZAHL - 1
 LDA = N + 1

 IF (N .EQ. 1) then
   ALPHA(1) = (F(1)-F(0)) / (X(1)-X(0))
   ALPHA(0) = F(1) - (ALPHA(1) * X(1))
   ALPHA(2) = 0.0D0
   ALPHA(3) = 0.0D0
   ALPHA(4) = 0.0D0
   RETURN
 ELSEIF (N .EQ. 0) then
   ALPHA(0) = 0.0D0
   ALPHA(1) = 0.0D0
   ALPHA(2) = 0.0D0
   ALPHA(3) = 0.0D0
   ALPHA(4) = 0.0D0
   RETURN
 End if


 ! POLYNOMBERECHNUNGSROUTINE AUFRUFEN
 ! --------------------------------------------------------------------------
 CALL POLY_GADESM(M,X,F,W,LDA,N,C,A,B,Y,Z,IERR,MARK1)

   IF (IERR .eq. 1) THEN
     WRITE(*,*) ' FEHLER: POLYNOMKOEFFIZIENTEN KONNTEN NICHT BERECHNET WERDEN !!! '
     WRITE(*,*) ' PROBLEM TRAT BEI Profil' ,PROF(NN,0), 'AUF '
     WRITE(*,*) ' '
   END IF
 ! UEBERPRUEFEN, OB POLYNOMKOEFFIZIENTEN AUSGERECHNET WERDEN KONNTEN
   IF (MARK1 .NE. 1) THEN
      WRITE(*,*) ' FEHLER: DIE POLYNOMKOEFFIZIENTEN KONNTEN NICHT BERECHNET WERDEN !!! '
      WRITE(*,*) ' '
      WRITE(*,*) ' PROBLEM TRAT BEI Profil' ,PROF(NN,0), 'AUF '
      WRITE(*,*) ' bordvoller wasserstand ist' ,H_BORDVOLL(NN),'m '
      WRITE(*,*) ' Anzahl der Wertepaare groeßer bordvoll' ,POLY2(NN)+1
      DO N = 0, XEND
        WRITE(*,*) ' BEI FOLGENDER FLIESSTIEFE' ,X(N), 'AUF '
        WRITE(*,*) ' BEI FOLGENDEM Q ODER A' ,F(N), 'AUF '
      END DO
   END IF

 ! Neues Polynom mit geringerem Grad oder alle Koeffi's zu Null
 !------------------------------------------------------------------
 IF (POLYGRAD(NN) .GT. 2 .and. MARK1 .NE. 1 .and. ABFRAGE .eq. 'J') THEN
   RED = POLYGRAD(NN) - 1        ! POLYNOMGRAD
   XEND = XEND -1
   GOTO 55
 ELSEIF (MARK1 .EQ. 1 .and. POLYGRAD(NN) .eq. WASGRAD) THEN
   DO N= 0, WASGRAD
     ALPHA(N) = C(N)   ! AUSGABE DER POLYNOMKOEFFIZIENTEN C
   END DO
 ELSEIF (MARK1 .EQ. 1 .and. POLYGRAD(NN) .lt. WASGRAD) THEN
   DO N=0, POLYGRAD(NN)
     ALPHA(N) = C(N)   ! AUSGABE DER POLYNOMKOEFFIZIENTEN C
   END DO
   DO N = (POLYGRAD(NN)+1), 4
     ALPHA(N) = 0.0D0    ! AUSGABE DER POLYNOMKOEFFIZIENTEN C
   END DO
 ELSE
   N = 0
   DO N= 0, 4
     ALPHA(N) = 0.0D0    ! AUSGABE DER POLYNOMKOEFFIZIENTEN C
   END DO
   POLYGRAD(NN) = 0
 END IF



 ! ABFRAGE AUF LOKALE EXTREMA:
 !-----------------------------------------------

! TERM_A = ALPHA(4)* 12.0D0
! TERM_B = ALPHA(3)* 6.0D0
! TERM_C = ALPHA(2)* 2.0D0

! XX_MIN = ( (-TERM_B) - SQRT(TERM_B**2 - 4*TERM_A*TERM_C)) /(2.0D0*TERM_A)
! XX_MAX = ( (-TERM_B) + SQRT(TERM_B**2 - 4*TERM_A*TERM_C)) /(2.0D0*TERM_A)

! IF (X(0).lt.XX_MIN .or. X(XEND).gt.XX_MIN) THEN
!   DO N = 0, XEND
!     DIFF_MIN(N) = X(0) - XX_MIN
!     LIMIT_MIN = MINVAL(DIFF_MIN(0:XEND))
!     ..............
!   END DO

! ELSEIF (X(0).lt.XX_MAX .or. X(XEND).gt.XX_MAX) THEN
!    ............
! END IF


 END SUBROUTINE !SUB POLYNOM_K


!***************************************************************************************************
!**                                                                                              ***
!**   SUBROUTINE POLY_CHOKY                                                                      ***
!**   ---------------------                                                                      ***
!**   Solving a linear system of equations  A * X = RS                                           ***
!**   for a symmetric, positive definite matrix A the Cholesky-method.                           ***
!**                                                                                              ***
!**   INPUT PARAMETERS:                                                                          ***
!**   =================                                                                          ***
!**   N    : order of the square matrix A                                                        ***
!**   A    : 2-dimensional array A(1:LDA,1:N) containing the symmetric, positive definite        ***
!**          matrix A. It is sufficient to specify only the elements of the upper triangle       ***
!**          of A (including the elements of the main diagonal); only the elemente in A's        ***
!**          upper triangle will be processed.                                                   ***
!**   LDA  : leading dimension of A as defined in the calling program                            ***
!**   RS   : N-vector RS(1:N) containing the right hand side                                     ***
!**                                                                                              ***
!**   OUTPUT PARAMETERS:                                                                         ***
!**   ==================                                                                         ***
!**   A    : 2-dimensional array A(1:LDA,1:N) which contains the Cholesky factors of             ***
!**          A = R(TRANSP) * D * R                                                               ***
!**          for a diagonal D and a unit upper triangular matrix R. The elements of R,           ***
!**          excluding the diagonal ones, are stored in the upper triangle of A. The             ***
!**          elements of D appear on the main diagonal of A.                                     ***
!**   X    : N-vector X(1:N) containing the solution of the system of equations                  ***
!**   MARK : error parameter                                                                     ***
!**          MARK= 1 : ok                                                                        ***
!**          MARK= 0 : numerically the matrix A is not stongly                                   ***
!**                    nonsingular                                                               ***
!**          MARK=-1 : A is not positive definite                                                ***
!**                                                                                              ***
!**   NOTE : If MARK = 1 , then the determinant of A can be calculated as follows:               ***
!**             DET A = A(1,1) * A(2,2) * ... * A(N,N)                                           ***
!**                                                                                              ***
!**   AUXILIARY PARAMETER:                                                                       ***
!**   ====================                                                                       ***
!**   Z    : N-vector Z(1:N)                                                                     ***
!**                                                                                              ***
!**----------------------------------------------------------------------------------------------***
!**                                                                                              ***
!**   subroutines required: MACHPD                                                               ***
!**                                                                                              ***
!***************************************************************************************************

      SUBROUTINE POLY_CHOKY (N,A,LDA,RS,X,Z,MARK)

      IMPLICIT NONE

      INTEGER N,LDA,MARK,I,J,K,MACHPD
      REAL (KIND=8) A(LDA,N),RS(N),X(N),Z(N),S,H,FMACHP,EPS

      MARK = 1

      !---------------------------------------------------------------------------------------------
      ! Cholesky factorization of the matrix A
      !---------------------------------------------------------------------------------------------

      ! calculating the machine constant
      FMACHP = 1.0D0
   10 FMACHP = 0.5D0 * FMACHP
      IF (MACHPD(1.0D0+FMACHP) .EQ. 1) GOTO 10
      FMACHP = FMACHP * 2.0D0

      ! determining the relative error bound
      EPS = 4.0D0 * FMACHP

      ! calculating the absolute row sums of the matrix A, with storage in the auxiliary vector Z
      DO 20 I=1,N
         S = 0.0D0
         DO 30 K=I,N
            S = S + DABS(A(I,K))
   30    CONTINUE
         DO 40 K=1,I-1
            S = S + DABS(A(K,I))
   40    CONTINUE
         IF (S .EQ. 0.0D0) THEN
            MARK = 0
            RETURN
         ENDIF
         Z(I) = 1.0D0/S
   20 CONTINUE

      ! Factoring the matrix A
      DO 50 J=1,N
         DO 60 I=1,J-1
            H = A(I,J)
            A(I,J) = H/A(I,I)
            DO 60 K=I+1,J
               A(K,J) = A(K,J) - H * A(I,K)
   60    CONTINUE
         IF (A(J,J) .LT. 0.0D0) THEN
            MARK = -1
            RETURN
         ELSEIF (DABS(A(J,J))*Z(J) .LE. EPS) THEN
            MARK = 0
            RETURN
         END IF
   50 CONTINUE

      !---------------------------------------------------------------------------------------------
      ! Updating and backsustitution
      !---------------------------------------------------------------------------------------------

      IF (MARK .EQ. 1) THEN
         ! updating
         Z(1) = RS(1)
         RS(1) = Z(1)/A(1,1)
         DO 15 J=2,N,1
            Z(J) = RS(J)
            DO 25 I=1,J-1,1
               Z(J) = Z(J) - A(I,J) * Z(I)
   25       CONTINUE
            RS(J) = Z(J)/A(J,J)
   15    CONTINUE
         ! backsubstitution
         X(N) = RS(N)
         DO 35 I=N-1,1,-1
            X(I) = RS(I)
            DO 35 J=I+1,N,1
               X(I) = X(I) - A(I,J) * X(J)
   35    CONTINUE
      END IF ! (MARK .EQ. 1)

      END SUBROUTINE ! POLY_CHOKY


!***************************************************************************************************
!**                                                                                              ***
!**   SUBROUTINE POLY_GADESM                                                                     ***
!**   ----------------------                                                                     ***
!**                                                                                              ***
!**   SUBROUTINE POLY_GADESM determines the coefficients of a polynomial of degree N that        ***
!**   approximates a function f at the given nodes in the discrete Gaussian least squares sense. ***
!**   The linear system of normal equations is solved using the Cholesky method.                 ***
!**                                                                                              ***
!**                                                                                              ***
!**   INPUT PARAMETERS:                                                                          ***
!**   =================                                                                          ***
!**   M        : index of the last node.                                                         ***
!**   X        : (N+1)-vector X(0:M) containing the nodes.                                       ***
!**   F        : (N+1)-vector F(0:M) containing the function values at the nodes.                ***
!**   W        : (N+1)-vector W(0:M) containing the weights.                                     ***
!**   LDA      : leading dimension of auxiliary matrix A as defined                              ***
!**              in the calling program, LDA >= N+1.                                             ***
!**   N        : degree of the approximating polynomial, 2 <= N <= M.                            ***
!**                                                                                              ***
!**                                                                                              ***
!**   OUPUT PARAMETERS:                                                                          ***
!**   =================                                                                          ***
!**   C        : (N+1)-vector C(0:N) containing coefficients of the approximating polynomial.    ***
!**   IERR     : = 0, no error.                                                                  ***
!**              = 1, incorrect input parameter.                                                 ***
!**              = 2, error in SUBROUTINE POLY_CHOKY.                                            ***
!**                                                                                              ***
!**                                                                                              ***
!**   HELP PARAMETER:                                                                            ***
!**   ===============                                                                            ***
!**   A        : 2-dim. array A(1:LDA,1:N+1).                                                    ***
!**   B        : (N+1)-vector B(1:N+1).                                                          ***
!**   Y        : (N+1)-vector Y(1:N+1).                                                          ***
!**   Z        : (N+1)-vector Z(1:N+1).                                                          ***
!**                                                                                              ***
!**----------------------------------------------------------------------------------------------***
!**                                                                                              ***
!**   subroutines required:  POLY_CHOKY                                                          ***
!**                                                                                              ***
!***************************************************************************************************

      SUBROUTINE POLY_GADESM(M,X,F,W,LDA,N,C,A,B,Y,Z,IERR,MARK)

      IMPLICIT NONE
      INTEGER IERR,M,N,LDA,I,J,J1,K,K1,L,MARK
      REAL (KIND=8) X(0:M),F(0:M),W(0:M),C(0:N),A(1:LDA,1:N+1),B(1:N+1),Y(1:N+1),Z(1:N+1)
      REAL (KIND=8) DUMMY

      ! testing the input parameter.
      IERR=1
      IF(N .LT. 2 .OR. N+1 .GT. LDA .OR. M .LT. N) RETURN
      IERR=0

      ! compute the first column of the system matrix for the normal equations
      ! and the right-hand side.

      A(1,1)=0.0D0
      B(1)=0.0D0
      DO 10 I=0,M
         A(1,1)=A(1,1)+W(I)
         B(1)=B(1)+W(I)*F(I)
   10 CONTINUE
      DO 20 J=1,N
         J1=J+1
         A(J1,1)=0.0D0
         B(J1)=0.0D0
         DO 30 I=0,M
            DUMMY=W(I)*X(I)**J
            A(J1,1)=A(J1,1)+DUMMY
            B(J1)=B(J1)+DUMMY*F(I)
   30    CONTINUE
   20 CONTINUE

      ! compute the last row.

      DO 40 K=1,N
         K1=K+1
         L=K+N
         A(J1,K1)=0.0D0
         DO 50 I=0,M
            A(J1,K1)=A(J1,K1)+W(I)*X(I)**L
   50    CONTINUE
   40 CONTINUE

      ! complete the matrix.

      DO 60 K=1,N
         DO 70 I=1,N
            A(I,K+1)=A(I+1,K)
   70    CONTINUE
   60 CONTINUE

      ! solve the system of normal equations. (The system matrix is positive definite).

      CALL POLY_CHOKY(N+1,A,LDA,B,Y,Z,MARK)
      IF(MARK .EQ. 1) THEN
         DO 80 J=0,N
            C(J)=Y(J+1)
   80    CONTINUE
      ELSE

         IERR=2
      END IF

      END SUBROUTINE ! POLY_GADESM



!*******************************************************************************************************
!                                                                                                      *
!  Factoring the matrix A into the product of two matrices L and  R so that  P * A = L * R,            *
!  where P = permutation matrix,  L = unit lower triangular matrix and R = upper triangular            *
!  matrix by applying the Gauss-elimination method with scaling and column pivot search.               *
!                                                                                                      *
!  INPUT PARAMETERS:                                                                                   *
!  =================                                                                                   *
!  N        : order of the system of equations.                                                        *
!  A        : 2-d array A(1:LDA,1:N); the system matrix of the system of equations, (A = A(ORG)).      *
!  LDA      : leading dimension of A as defined in the calling program.                                *
!                                                                                                      *
!  OUTPUT PARAMETERS:                                                                                  *
!  ==================                                                                                  *
!  A        : 2-dimensional array A(1:LDA,1:N), containing the factors L and R with                    *
!             P * A(ORG) = L * R  for P a permutation matrix. The upper triangular R is stored in      *
!             the upper triangle of A. The unit lower triangular matrix L, except for the diagonal     *
!             ones, is stored in the lower triangle of A.                                              *
!  IPIVOT   : N-vector IPIVOT(1:N); it indicates the row permutations of the scaled column pivot       *
!             search algorithm and thus defines the permutation matrix P. If e.g. IPIVOT(2) = 7,       *
!             then the 7th row of A(ORG) has become the 2nd row of P * A(ORG).                         *
!  MARK     : = 1, even number of row permutations.                                                    *
!             =-1, odd number of row permutations.                                                     *
!             = 0, system matrix A is numerically singular.                                            *
!             The determinant of A is:  DET(A(ORG)) = MARK * A(1,1) * ... * A(N,N).                    *
!  D        : N-vector D(1:N); the reciprocals of the row sum norms of A(ORG) that serve as scaling    *
!             factors: D(I) = 1./(ABS(A(I,1)) + ... + ABS(A(I,N)))  for I = 1, ..., N.                 *
!                                                                                                      *
!------------------------------------------------------------------------------------------------------*
!  subroutines required: MACHPD                                                                        *
!                                                                                                      *
!*******************************************************************************************************
!  authors   : Gisela Engeln-Muellges, Guido Dubois                                                    *
!  date      : 04.25.88                                                                                *
!                                                                                                      *
!*******************************************************************************************************

      SUBROUTINE GAUSSP(NM,A,LDA,IPIVOT,MARK,D,Y,XX)

      USE VARIABLEN
      IMPLICIT NONE

      REAL (KIND=8) A(1:LDA,1:NM),Y(1:NM),XX(1:NM),D(1:NM)
      REAL (KIND=8) MERKY1,RELERR,FMACHP,FAK,SUMIT,DUMMY,PIVOT
      INTEGER IFLAG,I,KM,J,IPVT,LDA
      INTEGER MARK,MACHPD,IPIVOT(1:NM),NM

!**   Local storage of error parameter RELERR in case that the SUBROUTINE is called repeatedly.

      SAVE RELERR,IFLAG
      DATA IFLAG /0/
      MARK=1

!**   Calculation of the machine constant and initializing the relative error.

      IF(IFLAG .EQ. 0) THEN
         IFLAG=1
         FMACHP=1.0D0
   10    FMACHP=0.5D0*FMACHP
         IF(MACHPD(1.0D0+FMACHP) .EQ. 1) GOTO 10
         RELERR=8.0D0*FMACHP
      END IF

!**   Calculation of row sum norms of A and initializing the PIVOT vector.

      DO I=1,NM
         IPIVOT(I)=I
         SUMIT=DABS(A(I,1))
         DO KM=2,NM
            IF (A(I,KM) .NE. 0.0D0) THEN   ! ABFRAGE ZUR BESCHLEUNIGUNG DER RECHNUNG
            SUMIT=SUMIT+DABS(A(I,KM))
            END IF
         END DO !KM=2,NM
         IF(SUMIT .EQ. 0.0D0) THEN
            MARK=0
            RETURN
         ELSE
            D(I)=1.0D0/SUMIT
         END IF
      END DO !I=1,NM
      IF(NM .EQ. 1) RETURN

      ! DREIECKS-FAKTORISIERUNG
      DO I=1,NM-1

         ! PIVOTREIHE BESTIMMEN (=REIHE MIT GROESSTEM ELEMENT FUER BETREFFENDE SPALTE)
         PIVOT=DABS(A(I,I))*D(I)
         IPVT=I
         DO J=I+1,NM
            DUMMY=DABS(A(J,I))*D(J)
            IF(DUMMY .GT. PIVOT) THEN
               PIVOT=DUMMY
               IPVT=J
            END IF
         END DO !J=I+1,NM

         IF(PIVOT .LT. RELERR) THEN
            MARK=0
            RETURN
         ELSE
            IF(IPVT .NE. I) THEN
               ! TAUSCHEN DER ZEILE IPVT MIT DER I-TEN ZEILE
               MARK=-MARK
               J=IPIVOT(I)
               IPIVOT(I)=IPIVOT(IPVT)
               IPIVOT(IPVT)=J
               DUMMY=D(I)
               D(I)=D(IPVT)
               D(IPVT)=DUMMY
               DO J=1,NM
                  DUMMY=A(I,J)
                  A(I,J)=A(IPVT,J)
                  A(IPVT,J)=DUMMY
               END DO !J=1,N
               ! TAUSCHEN DER RECHTEN SEITE
               MERKY1 = Y(I)
               Y(I) = Y(IPVT)
               Y(IPVT) = MERKY1
            END IF

            !**   ELIMINATION DURCHFUEHREN
            DO J=I+1,NM
               IF (A(J,I) .NE. 0.0D0) THEN            ! ABFRAGE ZUR BESCHLEUNIGUNG DER RECHNUNG
                  A(J,I)=A(J,I)/A(I,I)                ! I=SPALTE ; J=ZEILE
                  FAK=A(J,I)
                  ! BERECHNEN DER RECHTEN SEITE
                  Y(J) = Y(J) - FAK * Y(I)
                  DO KM=I+1,NM
                     A(J,KM)=A(J,KM)-FAK*A(I,KM)      ! KM=SPALTE ; J=ZEILE;
                  END DO                              ! I=ZEILE = CONST. FUER SCHLEIFE
               END IF !(A(J,I) .NE. 0.0D0)
            END DO !J=I+1,NM
         END IF !(PIVOT .LT. RELERR)
      END DO !I=1,NM-1

      IF(DABS(A(NM,NM)) .LT. RELERR) MARK=0

      IF (MARK .EQ. 0) THEN
      PRINT*,''
      PRINT*,'FEHLER: FEHLER BEIM LOESEN DES LINEAREN GLEICHUNGSSYSTEMS !!!'
      PRINT*,''
      STOP
      END IF

      ! WERTE VON Y AUF X UEBERTRAGEN
      DO N=1,NM
      XX(N) = Y(N)
      END DO

      IF(NM .EQ. 1) THEN
         XX(1)=Y(1)/A(1,1)
         RETURN
      END IF

      ! BERECHNUNG DES LÖSUNGSVEKTORS DURCH RUECKSUBSTITUTION
      XX(NM)=XX(NM)/A(NM,NM)
      DO I=NM-1,1,-1
         SUMIT=0.0D0
         DO KM=NM,I+1,-1
            IF (A(I,KM) .NE. 0.0D0) THEN       ! ABFRAGE ZUR BESCHLEUNIGUNG DER RECHNUNG
            SUMIT=SUMIT+A(I,KM)*XX(KM)
            END IF
         END DO !KM=NM,I+1,-1
         XX(I)=(XX(I)-SUMIT)/A(I,I)
      END DO !I=NM-1,1,-1

   END SUBROUTINE !SUB GAUSSP

!***************************************************************************************************
!**   FUNCTION MACHPD(X)                                                                         ***
!**   ------------------                                                                         ***
!***************************************************************************************************

      INTEGER FUNCTION MACHPD(X)
      IMPLICIT NONE

      REAL (KIND=8) X
      MACHPD = 0
      IF (1.0D0.LT.X) MACHPD = 1

      END FUNCTION ! MACHPD


