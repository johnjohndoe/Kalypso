!     Last change:  WP   16 Nov 2005    3:12 pm
module DIM_VARIABLEN

implicit none

save

INTEGER, parameter :: nch80 = 250      ! Max. Laenge von Zeichenketten, z.B. Dateinamen
INTEGER, parameter :: merg = 10        ! Max. Anzahl von Abflussereignissen
INTEGER, parameter :: maxkla = 1000    ! Max. Anzahl von Punkten pro Profil
INTEGER, parameter :: maxger = 1000    ! Max. Anzahl von Profilen
INTEGER, parameter :: ipro = 16        ! Max. Anzahl von Bloecken in Profildatei (? WP)
INTEGER, parameter :: itmax = 99       ! Max. Anzahl von Iterationen in versch. Subroutinen
INTEGER, parameter :: maxw = 50        ! Max. Anzahl von Wehrfeldern
INTEGER, parameter :: mpts = 1000       ! ?? WP wahrscheinlich = maxkla
INTEGER, parameter :: max2 = 10        ! ?? WP wird nur in wenigen SUBs verwendet
INTEGER, parameter :: min2 =100        ! ?? WP wird nur in wenigen SUBs verwendet

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
module VERSION

implicit none

save

CHARACTER(LEN=29), parameter :: VERSIONNR   = ' KALYPSO - 1D, VERSION 1.0.14'
CHARACTER(LEN=17), parameter :: VERSIONDATE = 'Stand: 16.11.2005'

end module VERSION



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


