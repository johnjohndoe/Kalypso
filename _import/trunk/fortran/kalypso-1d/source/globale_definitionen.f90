!     Last change:  MD   20 Aug 2009    4:35 pm
!--------------------------------------------------------------------------
! This code, globale_definitionen.f90, contains the shared memory modules
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Copyright (C) 2005  WOLF PLOEGER.
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
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 23 November 2005
! Research Associate
!***********************************************************************


!----------------------------------------------------------------------------
module VERSION

implicit none

save

CHARACTER(LEN=29), parameter :: VERSIONNR   = ' KALYPSO-1D, VERSION 2.1.5.0'
CHARACTER(LEN=17), parameter :: VERSIONDATE = 'Stand: 20.08.2009'

end module VERSION


!----------------------------------------------------------------------------
module DIM_VARIABLEN

implicit none

save

INTEGER, parameter :: nch80 = 250      ! Max. Laenge von Zeichenketten, z.B. Dateinamen
INTEGER, parameter :: merg = 50        ! Max. Anzahl von Abflussereignissen
INTEGER, parameter :: maxkla = 1000    ! Max. Anzahl von Punkten pro Profil
INTEGER, parameter :: maxger = 750    ! Max. Anzahl von Profilen
INTEGER, parameter :: maxabfluesse = 100    ! Max. Anzahl von Abfluessen bei stat.-ungl. Berechnung
INTEGER, parameter :: ipro = 16        ! Max. Anzahl von Bloecken in Profildatei (? WP)
INTEGER, parameter :: itmax = 99       ! Max. Anzahl von Iterationen in versch. Subroutinen
INTEGER, parameter :: maxw = 50        ! Max. Anzahl von Wehrfeldern
INTEGER, parameter :: mpts = 1000       ! ?? WP wahrscheinlich = maxkla
INTEGER, parameter :: max2 = 50         ! ?? WP wird nur in wenigen SUBs verwendet,
					! vor allem bei Bruecken (Polygonzugberechnung in AlWSP.f90)!
INTEGER, parameter :: min2 = 1000        ! ?? WP wird nur in wenigen SUBs verwendet
					! vor allem bei Bruecken (Polygonzugberechnung in AlWSP.f90)!

INTEGER, parameter :: idim = 1000       ! ?? WP wird nur in wenigen SUBs verwendet, wahrscheinlich = maxkla
INTEGER, parameter :: idim4 = 20       ! ?? WP wird nur in wenigen SUBs verwendet
INTEGER, parameter :: idim2 = 16       ! ?? WP wird nur in wenigen SUBs verwendet muss gleich ipro sein!!!

INTEGER, parameter :: grnz_it = 15     ! Max. Anzahl der Iterationen fuer Grenztiefenberechnung
INTEGER, parameter :: grnz_sch = 5     ! Max. Anzahl der Schleifendurchlaeufe bei Grenztiefenberechnung

end module DIM_VARIABLEN


!-----------------------------------------------------------------------------
module KONSTANTEN

implicit none

save

REAL, parameter :: nue  = 1.30E-06
REAL, parameter :: e    = 2.71828182846
REAL, parameter :: g    = 9.80665
REAL, parameter :: rho  = 1000.0
REAL, parameter :: epsi = 1.0E-03
REAL, parameter :: err  = 0.005
REAL, parameter :: pi   = 3.1415926

end module KONSTANTEN



!-----------------------------------------------------------------------------
module IO_UNITS

implicit none

save

INTEGER	:: UNIT_EIN_PROF        ! Profildatei (z.B. ..\PROF\st000042.prf)
INTEGER	:: UNIT_EIN_STR      	! Strangtabelle
INTEGER	:: UNIT_EIN_QWERT       ! Abflussdatei
INTEGER	:: UNIT_EIN_KM          ! Teilgebietedatei
INTEGER	:: UNIT_EIN_PSI         ! (Einzel-)Verlustbeiwerte
INTEGER	:: UNIT_EIN_INI         ! KALYPSO-Konfigurationsdatei
INTEGER :: UNIT_EIN_ADD_INI     ! Zusaetzliche Konfigurationsdatei im /PROF Ordner

INTEGER	:: UNIT_OUT_LOG         ! Kontroll.log
INTEGER	:: UNIT_OUT_LAMBDA_I    ! lambda_i.txt
INTEGER	:: UNIT_OUT_GER         ! Gerinnedatei für den Austausch der KM-Paramter mit NA-Modell
INTEGER	:: UNIT_OUT_LOG_KM      ! out.*, Detaillierte LOG-Datei der KM-Berechnung
INTEGER	:: UNIT_OUT_KM          ! z.B. st000042.km, WQ-Tabelle für jedes einzelne Profil bei stat-ungl. Berechnung
INTEGER	:: UNIT_OUT_TAB         ! Hauptausgabedatei, Tabelle mit Ergebnissen einer stat. Berechnung (z.B. sttb0001.001)
INTEGER	:: UNIT_OUT_ALPHA       ! Beiwerte.aus, Ausgabe der Energiestrom und Impulsstrombeiwerte
INTEGER	:: UNIT_OUT_PRO         ! WQ-Tabelle altes WspWin Format
INTEGER	:: UNIT_OUT_WSL         ! Laengschnitt im WspWin Blockformat
INTEGER	:: UNIT_OUT_LAENGS      ! Neuer Laengsschnitt im Tabellenformat (leangsschnitt.txt)
INTEGER	:: UNIT_OUT_QLAENGS     ! Neuer Abfluss-Laengsschnitt im Tabellenformat (Qschnitt.txt)
INTEGER	:: UNIT_OUT_QB1         ! Laengschnitt im WspWin Blockformat bei Bordvoll-Berechnung
INTEGER	:: UNIT_OUT_QB2         ! Laengschnitt im WspWin Blockformat bei Bordvoll-Berechnung
INTEGER	:: UNIT_OUT_WEHR        ! Ausgabedatei fuer innere Abflussstaffelung am Wehr
INTEGER	:: UNIT_OUT_QWEHR       ! Ausgabedatei fuer Wasserstand-Abfluss-Beziehung ums Wehr
INTEGER	:: UNIT_OUT_HEWEHR      ! Ausgabedatei fuer Energiehoehe-Abfluss-Beziehung ums Wehr
INTEGER	:: UNIT_OUT_BRUECKE     ! Ausgabedatei fuer innere Abflussstaffelung an Bruecken
INTEGER	:: UNIT_OUT_QBRUECKE    ! Ausgabedatei fuer Wasserstand-Abfluss-Beziehung an Bruecken
INTEGER	:: UNIT_OUT_HEBRUECKE   ! Ausgabedatei fuer Energiehoehe-Abfluss-Beziehung an Bruecken

end module IO_UNITS


!-----------------------------------------------------------------------------
module IO_NAMES

USE DIM_VARIABLEN

implicit none

save

CHARACTER(LEN=nch80) 			:: NAME_PFAD_PROF       ! Absoluter Pfad incl. \PROF\
CHARACTER(LEN=nch80) 			:: NAME_PFAD_DATH       ! Absoluter Pfad incl. \DATH\

CHARACTER(LEN=nch80),DIMENSION(1:maxger):: NAME_EIN_PROF        ! Profildatei (z.B. ..\PROF\st000042.prf)
CHARACTER(LEN=nch80) 			:: NAME_EIN_STR      	! Strangtabelle
CHARACTER(LEN=nch80) 			:: NAME_EIN_QWERT       ! Abflussdatei
CHARACTER(LEN=nch80) 			:: NAME_EIN_KM          ! Teilgebietedatei
CHARACTER(LEN=nch80) 			:: NAME_EIN_PSI         ! (Einzel-)Verlustbeiwerte
CHARACTER(LEN=nch80) 			:: NAME_EIN_INI         ! KALYPSO-Konfigurationsdatei
CHARACTER(LEN=nch80) 			:: NAME_EIN_ADD_INI     ! Zusaetzliche Konfigurationsdatei im /PROF Ordner

CHARACTER(LEN=nch80) 			:: NAME_OUT_LOG         ! Kontroll.log
CHARACTER(LEN=nch80) 			:: NAME_OUT_LAMBDA_I    ! lambda_i.txt
CHARACTER(LEN=nch80) 			:: NAME_OUT_GER         ! Gerinnedatei für den Austausch der KM-Paramter mit NA-Modell
CHARACTER(LEN=nch80) 			:: NAME_OUT_LOG_KM      ! out.*, Detaillierte LOG-Datei der KM-Berechnung
CHARACTER(LEN=nch80),DIMENSION(1:maxger):: NAME_OUT_KM          ! z.B. st000042.km, WQ-Tabelle für jedes einzelne Profil
								! bei stat-ungl. Berechnung
CHARACTER(LEN=nch80) 			:: NAME_OUT_TAB         ! Hauptausgabedatei, Tabelle mit Ergebnissen einer
								! stat. Berechnung (z.B. sttb0001.001)
CHARACTER(LEN=nch80) 			:: NAME_OUT_ALPHA       ! Beiwerte.aus, Ausgabe der Energiestrom und Impulsstrombeiwerte
CHARACTER(LEN=nch80),DIMENSION(1:maxger):: NAME_OUT_PRO         ! WQ-Tabelle altes WspWin Format
CHARACTER(LEN=nch80) 			:: NAME_OUT_WSL         ! Laengschnitt im WspWin Blockformat
CHARACTER(LEN=nch80) 			:: NAME_OUT_LAENGS      ! Neuer Laengsschnitt im Tabellenformat (leangsschnitt.txt)
CHARACTER(LEN=nch80) 			:: NAME_OUT_QLAENGS     ! Neuer Abfluss-Laengsschnitt im Tabellenformat (Q_LangSchnitt.txt)
CHARACTER(LEN=nch80) 			:: NAME_OUT_QB1         ! Laengschnitt im WspWin Blockformat bei Bordvoll-Berechnung
CHARACTER(LEN=nch80) 			:: NAME_OUT_QB2         ! Laengschnitt im WspWin Blockformat bei Bordvoll-Berechnung
CHARACTER(LEN=nch80) 			:: NAME_OUT_WEHR        ! Ausgabedatei fuer innere Abflussstaffelung am Wehr
CHARACTER(LEN=nch80) 			:: NAME_OUT_QWEHR       ! Ausgabedatei fuer Wasserstand-Abfluss-Beziehung ums Wehr
CHARACTER(LEN=nch80) 			:: NAME_OUT_HEWEHR      ! Ausgabedatei fuer Energiehoehe-Abfluss-Beziehung ums Wehr
CHARACTER(LEN=nch80) 			:: NAME_OUT_BRUECKE     ! Ausgabedatei fuer innere Abflussstaffelung an Bruecken
CHARACTER(LEN=nch80) 			:: NAME_OUT_QBRUECKE    ! Ausgabedatei fuer Wasserstand-Abfluss-Beziehung an Bruecken
CHARACTER(LEN=nch80) 			:: NAME_OUT_HEBRUECKE   ! Ausgabedatei fuer Energiehoehe-Abfluss-Beziehung an Bruecken
end module IO_NAMES



!-----------------------------------------------------------------------------
module MOD_ERG
!
! In diesem Modul werden alle Ergebnisse global abgelegt, damit
! sie fuer die unterschiedlichen Ausgaben jederzeit zur Verfuegung
! stehen.
!                         Wolf Ploeger, 11.03.2006
!-----------------------------------------------------------------------------

USE DIM_VARIABLEN

implicit none

save

TYPE :: ergebnis_teilabschnitte
  REAL :: lambda        ! Widerstandsbeiwert [-]
  REAL :: formb         ! Formbeiwert [-]
  REAL :: A             ! Fliessquerschnitt [m2]
  REAL :: B             ! Wasserspiegelbreite [m]
  REAL :: lu            ! benetzter Unfang [m]
  REAL :: v             ! Fliessgeschwindigkeit [m/s]
  REAL :: Q             ! Abfluss [m3/s]
END TYPE ergebnis_teilabschnitte
TYPE (ergebnis_teilabschnitte), DIMENSION(1:maxger, 1:maxabfluesse, 1:3) :: out_IND

TYPE :: ergebnis_profil
  REAL :: stat          ! Station [km]
  REAL :: wsp           ! Wasserspiegelhoehe [mNN]
  REAL :: hen           ! Energiehpehe [mNN]
  REAL :: hbv           ! Bordvolle Hoehe des Profils [mNN]
  REAL :: sohle         ! Hoehe des tiefsten Punktes im Profil [m]
  REAL :: qges          ! Gesamtabfluss [m3/s] (links + mitte + rechts)
  REAL :: boeli         ! Boeschungsoberkante links [mNN]
  REAL :: boere         ! Boeschungsoberkante rechts [mNN]
  REAL :: vm            ! Mittlere Fliessgeschwindigkeit [m/s]
  REAL :: tau           ! Schubspannung im Flussschlauch [N/m2]
  REAL :: hvm           ! Verlusthoehe [m]
  REAL :: hrm           ! Verlusthoehe [m]
  REAL :: hein          ! Verlusthoehe [m]
  REAL :: hort          ! Verlusthoehe [m]
  REAL :: hm            ! Verlusthoehe [m]
  REAL :: WehrOK        ! Wehroberkante [mNN], falls kein Wehr, dann -999.99
  REAL :: BrueckOK      ! Brueckenoberkante [mNN], falls keine Bruecke, dann -999.99
  REAL :: BrueckUK      ! Brueckenunterkante [mNN], falls keine Bruecke, dann -999.99
  REAL :: BrueckB       ! Brueckenbreite [m], falls keine Bruecke, dann -999.99
  REAL :: RohrD         ! Rohrdurchmesser [m], falls kein Rohr, dann -999.99
  REAL :: alphaIW       ! Impulsstrombeiwert [-]
  REAL :: alphaEW       ! Energiestrombeiwert [-]
  REAL :: gefaelle      ! Reibungsgefaelle [-]
  LOGICAL :: interpol   ! Profil ist interpoliert (.TRUE.) oder urspruenglich (.FALSE.), wichtig bei Bruecken
  CHARACTER(LEN=1) :: chr_kenn ! Kennung fuer das Profil (i = Interpoliert, n = Normal, b = Bruecke, w = Wehr, d = Durchlass)
END TYPE ergebnis_profil
TYPE (ergebnis_profil), DIMENSION(1:maxger, 1:maxabfluesse) :: out_PROF


! Neu fuer Innere Abflussschleife ueber Bruecken
! -------------------------------------------------
TYPE :: ergebnis_teilabschnitte_Qin
  REAL :: lambda        ! Widerstandsbeiwert [-]
  REAL :: formb         ! Formbeiwert [-]
  REAL :: A             ! Fliessquerschnitt [m2]
  REAL :: B             ! Wasserspiegelbreite [m]
  REAL :: lu            ! benetzter Unfang [m]
  REAL :: v             ! Fliessgeschwindigkeit [m/s]
  REAL :: Q             ! Abfluss [m3/s]
END TYPE ergebnis_teilabschnitte_Qin
TYPE (ergebnis_teilabschnitte_Qin), DIMENSION(1:maxger, 1:maxabfluesse, 1:maxabfluesse,1:3) :: out_Qin_IND

TYPE :: ergebnis_profil_Qin
  REAL :: stat          ! Station [km]
  REAL :: wsp           ! Wasserspiegelhoehe [mNN]
  REAL :: hen           ! Energiehpehe [mNN]
  REAL :: hbv           ! Bordvolle Hoehe des Profils [mNN]
  REAL :: sohle         ! Hoehe des tiefsten Punktes im Profil [m]
  REAL :: qges          ! Gesamtabfluss [m3/s] (links + mitte + rechts)
  REAL :: boeli         ! Boeschungsoberkante links [mNN]
  REAL :: boere         ! Boeschungsoberkante rechts [mNN]
  REAL :: vm            ! Mittlere Fliessgeschwindigkeit [m/s]
  REAL :: tau           ! Schubspannung im Flussschlauch [N/m2]
  REAL :: hvm           ! Verlusthoehe [m]
  REAL :: hrm           ! Verlusthoehe [m]
  REAL :: hein          ! Verlusthoehe [m]
  REAL :: hort          ! Verlusthoehe [m]
  REAL :: hm            ! Verlusthoehe [m]
  REAL :: WehrOK        ! Wehroberkante [mNN], falls kein Wehr, dann -999.99
  REAL :: BrueckOK      ! Brueckenoberkante [mNN], falls keine Bruecke, dann -999.99
  REAL :: BrueckUK      ! Brueckenunterkante [mNN], falls keine Bruecke, dann -999.99
  REAL :: BrueckB       ! Brueckenbreite [m], falls keine Bruecke, dann -999.99
  REAL :: RohrD         ! Rohrdurchmesser [m], falls kein Rohr, dann -999.99
  REAL :: alphaIW       ! Impulsstrombeiwert [-]
  REAL :: alphaEW       ! Energiestrombeiwert [-]
  REAL :: gefaelle      ! Reibungsgefaelle [-]
  LOGICAL :: interpol   ! Profil ist interpoliert (.TRUE.) oder urspruenglich (.FALSE.), wichtig bei Bruecken
  CHARACTER(LEN=1) :: chr_kenn ! Kennung fuer das Profil (i = Interpoliert, n = Normal, b = Bruecke, w = Wehr, d = Durchlass)
END TYPE ergebnis_profil_Qin
TYPE (ergebnis_profil_Qin), DIMENSION(1:maxger, 1:maxabfluesse, 1:maxabfluesse) :: out_Qin_PROF



INTEGER :: nr_q         	! Laufende Nummer des aktuellen Abflusses
INTEGER :: anz_q        	! Gesamtanzahl der unterschiedlichen Abfluesse

INTEGER, DIMENSION(1:maxabfluesse) :: anz_prof	! Die Anzahl der Profile kann sich während der Berechnung
                                        	! durch Interpolation je nach Abflusszustand ändern.

end module MOD_ERG



!-----------------------------------------------------------------------------
module BEWUCHS

implicit none

save

REAL, parameter :: c2 = 0.071
REAL, parameter :: alpha = 3.289
REAL, parameter :: beta = 0.946
REAL, parameter :: a1 = 3.27
REAL, parameter :: b1 = 2.85
REAL, parameter :: c3 = 0.072
REAL, parameter :: c4 = 1.066

end module BEWUCHS


!-----------------------------------------------------------------------------
module EXTREME_ROUGHNESS

implicit none

save

REAL, parameter :: alpha_t = 6.8
REAL, parameter :: beta_w = 0.3

end module EXTREME_ROUGHNESS



!----------------------------------------------------------------------------
module ZEIT

implicit none

save

CHARACTER(LEN=11) :: HHMMSSHH
CHARACTER(LEN=8)  :: HHMMSS, MMTTJJ
CHARACTER(LEN=5)  :: HHMM

end module ZEIT




!----------------------------------------------------------------------------
module AUSGABE_LAENGS

USE DIM_VARIABLEN

implicit none

save

INTEGER :: ns
INTEGER :: nstat

REAL 	:: yhmin
REAL	:: yhmax
REAL 	:: qmin
REAL	:: qmax

INTEGER, DIMENSION(maxger) 	:: jkenn1 = 0.0
REAL, DIMENSION(maxger)         :: xss    = 0.0
REAL, DIMENSION(maxger)         :: stat1  = 0.0                 ! Station
REAL, DIMENSION(maxger)         :: hwsp1  = 0.0
REAL, DIMENSION(maxger)         :: qbv1   = 0.0
REAL, DIMENSION(maxger)         :: vbv1   = 0.0
REAL, DIMENSION(maxger)         :: gef1   = 0.0                 ! Gefaelle [-]
REAL, DIMENSION(maxger)         :: k_ks1  = 0.0
REAL, DIMENSION(maxger)         :: bv1    = 0.0                 ! Bordvolle Hoehe
REAL, DIMENSION(maxger)         :: boli1  = 0.0                 ! Bordvollpunkt links
REAL, DIMENSION(maxger)         :: bore1  = 0.0                 ! Bordvollpunkt rechts
REAL, DIMENSION(maxger)         :: sohl1  = 0.0
REAL, DIMENSION(maxger)         :: hen1   = 0.0
REAL, DIMENSION(maxger)         :: wsp1   = 0.0
REAL, DIMENSION(maxger)         :: hming1 = 0.0
REAL, DIMENSION(maxger)         :: vp_li, vp_fl, vp_re = 0.0
REAL, DIMENSION(maxger)         :: fp_li, fp_fl, fp_re = 0.0
REAL, DIMENSION(maxger)         :: rk_li, rk_fl, rk_re = 0.0
REAL, DIMENSION(maxger)         :: br_li, br_fl, br_re = 0.0
REAL, DIMENSION(maxger)         :: tau_fl = 0.0
REAL, DIMENSION(maxger)         :: Alpha_IW = 0.0
REAL, DIMENSION(maxger)         :: Alpha_EW = 0.0

CHARACTER(LEN=10) :: num5 (maxger)

end module AUSGABE_LAENGS



! -----------------------------------------------------------
module MOD_INI
!
! Description:
! ------------
! This module defines the general configuration data given by
! the control file that is created from the KALYPSO-framework.
! All variables are stored, but during calculation not all
! of them may be neccessary.
!                                   Wolf Ploeger, 24. May 2006

USE DIM_VARIABLEN

implicit none

CHARACTER(LEN=7) :: RUN_MODUS           	! = "KALYPSO" oder "WSPWIN"

CHARACTER(LEN=nch80) :: PROJEKTPFAD
CHARACTER(LEN=nch80) :: STRANGDATEI

CHARACTER(LEN=10) :: BERECHNUNGSMODUS           ! = WATERLEVEL
                                                ! = BF_UNIFORM
                                                ! = BF_NON_UNI

CHARACTER(LEN=11) :: FLIESSGESETZ               ! = DW_M_FORMBW
                                                ! = DW_O_FORMBW
                                                ! = MANNING_STR
REAL :: ANFANGSSTATION
REAL :: ENDSTATION

CHARACTER(LEN=20) :: ART_RANDBEDINGUNG          ! = CRITICAL_WATER_DEPTH
                                                ! = UNIFORM_BOTTOM_SLOPE
                                                ! = WATERLEVEL

REAL :: ANFANGSWASSERSPIEGEL
REAL :: GEFAELLE

CHARACTER(LEN=4) :: VERZOEGERUNGSVERLUST        ! = DVWK
                                                ! = BJOE
                                                ! = DFG
                                                ! = NON

CHARACTER(LEN=5) :: ITERATIONSART               ! = SIMPL
                                                ! = EXACT

CHARACTER(LEN=6) :: REIBUNGSVERLUST             ! = TRAPEZ
                                                ! = GEOMET

LOGICAL :: MIT_BRUECKEN
LOGICAL :: MIT_WEHREN
LOGICAL :: USE_EXTREM_ROUGH
LOGICAL :: CALC_KM_INTERN

CHARACTER(LEN=nch80) :: ABFLUSSEREIGNIS
CHARACTER(LEN=nch80) :: EINZELVERLUSTE

REAL :: MIN_Q
REAL :: MAX_Q
REAL :: DELTA_Q

CHARACTER(LEN=1) :: DURCHFLUSS_EINHEIT          ! = "M" oder "L"

!-----------------------------
! Weitere interne Definitionen
CHARACTER(LEN=nch80) :: EREIGNISNAME
CHARACTER(LEN=nch80) :: FLUSSNAME

end module MOD_INI



