!     Last change:  WP   13 Mar 2006    3:36 pm
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

CHARACTER(LEN=29), parameter :: VERSIONNR   = ' KALYPSO - 1D, VERSION 1.0.17'
CHARACTER(LEN=17), parameter :: VERSIONDATE = 'Stand: 10.03.2006'

end module VERSION


!----------------------------------------------------------------------------
module DIM_VARIABLEN

implicit none

save

INTEGER, parameter :: nch80 = 250      ! Max. Laenge von Zeichenketten, z.B. Dateinamen
INTEGER, parameter :: merg = 10        ! Max. Anzahl von Abflussereignissen
INTEGER, parameter :: maxkla = 1000    ! Max. Anzahl von Punkten pro Profil
INTEGER, parameter :: maxger = 1000    ! Max. Anzahl von Profilen
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

INTEGER, parameter :: grnz_it = 15     ! Max. Anzahl der Iterationen fuer Grenzteifenberechnung
INTEGER, parameter :: grnz_sch = 5     ! Max. Anzahl der Schleifendurchlaeufe bei Grenztiefenberechnung

end module DIM_VARIABLEN


!-----------------------------------------------------------------------------
module KONSTANTEN

implicit none

save

REAL, parameter :: nue  = 1.0E-06
REAL, parameter :: e    = 2.71828
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

INTEGER :: UNIT_EIN_PROF        ! Profildatei (z.B. ..\PROF\st000042.prf)
INTEGER :: UNIT_EIN_STR      	! Strangtabelle
INTEGER :: UNIT_EIN_QWERT       ! Abflussdatei
INTEGER :: UNIT_EIN_KM          ! Teilgebietedatei
INTEGER :: UNIT_EIN_PSI         ! (Einzel-)Verlustbeiwerte

INTEGER :: UNIT_OUT_LOG         ! Kontroll.log
INTEGER :: UNIT_OUT_LAMBDA_I    ! lambda_i.txt
INTEGER :: UNIT_OUT_GER         ! Gerinnedatei für den Austausch der KM-Paramter mit NA-Modell
INTEGER :: UNIT_OUT_LOG_KM      ! out.*, Detaillierte LOG-Datei der KM-Berechnung
INTEGER :: UNIT_OUT_KM          ! z.B. st000042.km, WQ-Tabelle für jedes einzelne Profil bei stat-ungl. Berechnung
INTEGER :: UNIT_OUT_TAB         ! Hauptausgabedatei, Tabelle mit Ergebnissen einer stat. Berechnung (z.B. sttb0001.001)
INTEGER :: UNIT_OUT_ALPHA       ! Beiwerte.aus, Ausgabe der Energiestrom und Impulsstrombeiwerte
INTEGER :: UNIT_OUT_PRO         ! WQ-Tabelle altes WspWin Format
INTEGER :: UNIT_OUT_WSL         ! Laengschnitt im WspWin Blockformat
INTEGER :: UNIT_OUT_LAENGS      ! Neuer Laengsschnitt im Tabellenformat (leangsschnitt.txt)
INTEGER :: UNIT_OUT_QB1         ! Laengschnitt im WspWin Blockformat bei Bordvoll-Berechnung
INTEGER :: UNIT_OUT_QB2         ! Laengschnitt im WspWin Blockformat bei Bordvoll-Berechnung

end module IO_UNITS



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
  REAL :: sohle         ! Hoehe des tiefsten Punktes im Profil [m]
  REAL :: qges          ! Gesamtabfluss [m3/s] (links + mitte + rechts)
  REAL :: h_bv          ! Bordvolle Hoehe des Profils [mNN]
  REAL :: boeli         ! Boeschungsoberkante links [mNN]
  REAL :: boere         ! Boeschungsoberkante rechts [mNN]
  REAL :: tau           ! Schubspannung im Flussschlauch [N/m2]
  REAL :: hvm           ! Verlusthoehe [m]
  REAL :: hrm           ! Verlusthoehe [m]
  REAL :: hein          ! Verlusthoehe [m]
  REAL :: hort          ! Verlusthoehe [m]
  REAL :: hm            ! Verlusthoehe [m]
  LOGICAL :: interpol   ! Profil ist interpoliert (.TRUE.) oder urspruenglich (.FALSE.), wichtig bei Bruecken
END TYPE ergebnis_profil
TYPE (ergebnis_profil), DIMENSION(1:maxger, 1:maxabfluesse) :: out_PROF

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

CHARACTER(LEN=10) :: num5 (maxger)

end module AUSGABE_LAENGS


