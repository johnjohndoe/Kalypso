
module mod_get_cwr

contains

!---------------------------------------------------------------------------------------------
function GET_CW_UN(v_vor, d_p) result (CW_UN)
! Calculation of drag coefficient for a single
! cylindrical object, depending on Reynolds number.
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8)  :: CW_UN

!input parameters
REAL (KIND = 8), INTENT(IN) :: v_vor      ! Fließgeschwindigkeit
REAL (KIND = 8), INTENT(IN) :: d_p        ! Durchmesser des Bewuchselementes

! Local parameters
REAL (KIND = 8)  :: Re_p                   ! Reynolds-Zahl, bezogen auf den Bewuchs

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
Re_p = (v_vor * d_p) / nu_10

if (Re_p <= 800) then
  CW_UN = 3.07 * Re_p ** (-0.168)
else if (Re_p > 800 .and. Re_p <= 8000) then
  CW_UN = 1.0
else
  CW_UN = 1.2
end if

end function GET_CW_UN




!---------------------------------------------------------------------------------------------
function GET_A_NL(c_wr_un, d_p, I_R, v_vor, a_NL_anf) result (A_NL)
! Calculation of wake length
!---------------------------------------------------------------------------------------------
use mod_globalconstants
use mod_Warning
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8)             ::  A_NL

!input parameters
REAL (KIND = 8), INTENT(IN) :: c_wr_un    ! Widerstandsbeiwert
REAL (KIND = 8), INTENT(IN) :: d_p        ! Durchmesser des Bewuchselementes
REAL (KIND = 8), INTENT(IN) :: I_R        ! Stationäres Gefälle
REAL (KIND = 8), INTENT(IN) :: v_vor      ! Maßgebende Fließgeschwindigkeit
REAL (KIND = 8), INTENT(IN) :: a_NL_anf   ! Anfangswert für Iteration ( = 2 * ax )

! Local parameters
INTEGER          :: i
REAL (KIND = 8)  :: a_NL2
type (warning)   :: warningSign
type (error)     :: errorSign

!initializations block
!--------------------------------------
a_NL  = a_NL_anf
a_NL2 = 2 * a_NL_anf

i = 0                              ! Zähler der Iteration von a_NL

!execution block
!--------------------------------------
iteration_a_NL: do i = 1, 50                 ! Iteration bis Konvergenzkriterium für a_NL erfüllt ist

  if ( ABS(1-(a_NL/a_NL2)) < 0.001 ) then 
    exit iteration_a_NL
  endif

  if (i == 50) then
  ! no convergence after 30 iterations
    a_NL2 = 0
    warningSign = newWarning (w_IterationANL)
    call printWarningMessage (warningSign)
    exit iteration_lambda
  END if

  a_NL  = a_NL2
  a_NL2 = (a_NL + 128.9 * c_wr_un * d_p * (1 + (g * a_NL * I_R)/((v_vor**2)/2))**(-2.14)) / 2

end do iteration_a_NL              ! Iteration von a_NL erfolgreich!

A_NL = a_NL2                    ! Zuweisung des endgültigen Wertes


END function GET_A_NL




!---------------------------------------------------------------------------------------------
function GET_A_NB(c_wr_un, d_p, a_NL) result (A_NB)
! Calculation of wake width
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8)             :: A_NB

!input parameters
REAL (KIND = 8), INTENT(IN) :: c_wr_un    ! Widerstandsbeiwert
REAL (KIND = 8), INTENT(IN) :: d_p        ! Durchmesser des Bewuchselementes
REAL (KIND = 8), INTENT(IN) :: a_NL       ! Nachlauflänge

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
A_NB = 0.24 * (a_NL ** 0.59) * (c_wr_un * d_p) ** 0.41

END function GET_A_NB

END module !get_cwr



!---------------------------------------------------------------------------------------------
function GET_CWR(I_R, h_m, a_x, d_p, lambda_s) result (cw_r)
! The drag coefficient C_WR of vegetation in an element with the
! characteristic properties
! - slope of water surface (I_R),
! - mean flow depth (H_M),
! - mean distance between trees/branches (A_X),
! - mean diameter of trees/branches (D_P) and
! - friction factor of bottom surface
! is calculated.
! The method of LINDNER (modified by PASCHE) like published in
! DVWK 220: Hydraulische Berechnung von Fließgewaessern" 1991 is applied.
! This method contains some nested iterations that may lead to undesirable
! convergence problems espacially with very small values for the slope and
! the water depth. Therefor some limitations have been implemented to
! always get realistic results.
!---------------------------------------------------------------------------------------------
use mod_globalconstants
use mod_get_cwr
use mod_Error
use mod_Warning

implicit none
!DEC$ ATTRIBUTES DLLEXPORT::GET_CWR
!definition block
!-------------------------------------
!function result
REAL (kind = 8)                        :: cw_r     ! Drag coefficient for the vegetation

!input parameters
REAL (kind = 8), INTENT(IN)            :: I_R      ! Slope
REAL (KIND = 8), INTENT(IN)            :: h_m      ! Mean flow depth
REAL (kind = 8), INTENT(IN)            :: a_x      ! distance of trees/branches
REAL (kind = 8), INTENT(IN)            :: d_p      ! diameter of trees/branches
REAL (KIND = 8), INTENT(IN)            :: lambda_s ! friction factor by COLBROOK/WHITE

! Local parameters
REAL (KIND = 8)  :: a_y
REAL (KIND = 8)  :: c_wr2   = 0.0          ! temp c_wr for iteration
REAL (KIND = 8)  :: v_vor   = 0.0          ! mean flow velocity
REAL (KIND = 8)  :: c_wr_un = 0.0          ! drag coefficient for a single cylinder
REAL (KIND = 8)  :: a_NL    = 0.0          ! wake length
REAL (KIND = 8)  :: a_NL_anf= 0.0          ! temp wake length
REAL (KIND = 8)  :: a_NB    = 0.0          ! wake width
REAL (KIND = 8)  :: vn_vvor2= 0.0          ! velocity ratio
REAL (KIND = 8)  :: y_stern = 0.0          ! water depth ratio
REAL (KIND = 8)  :: c_temp  = 0.0          ! c_temp = a_y / (a_y -d_p)
REAL (KIND = 8)  :: lambda_g= 0.0          ! temporary total lambda
REAL (KIND = 8)  :: lambda_p= 0.0          ! temporary vegeation lambda
REAL (KIND = 8)  :: Fr1     = 0.0          ! Froudenumber 1
REAL (KIND = 8)  :: Fr2     = 0.0          ! Froudenumber 2
REAL (KIND = 8)  :: delta_cw= 0.0          ! Delta C_W

type (Error)     :: errorSign
type (Warning)   :: warningSign

INTEGER :: i, j


!initializations block
!--------------------------------------
a_y = a_x                               ! Symetrical arangement of plants
cw_r  = 1.0                             ! Schätzung von C_WR = 1.0
c_wr2 = 1.5                             ! Schätzung von C_WR2 = 1.5 (nur am Anfang, damit
                                        ! CW_R und CW_R2 unterschiedliche Startwerte haben)
a_NL_anf  = 2 * a_x                     ! Schätzung von a_NL_anf = 2 * ax (Für Funktion GET_A_NL)
c_temp = a_y / (a_y -d_p)               ! Temporäre Variable, nur zur Vereinfachung der
                                        ! folgenden Berechnungen
j = 0                                   ! Zähler der äußeren Iteration von C_WR

!execution block
!--------------------------------------

if (I_R * h_m * a_x * d_p * lambda_s == 0) then
  errorSign = newError (e_ParametersMissing)
  call printErrorMessage (errorSign)
  stop
endif

iteration_cwr: do                       !Iteration bis Konvergenzkriterium für C_WR erfüllt ist
    Fr1 = 0.0                           ! Bei jeder neuen Iteration müssen die beiden Froudezahlen
    Fr2 = 0.0                           ! neu bestimmt werden -> Y_STERN
    j = j + 1                           ! Inkrementieren des Schleifenzählers
                                        ! Abbruchkriterium für die Konvergenz, ist jetzt willkürlich
                                        ! auf 0.002 gesetzt, kann auch erhöht oder verringert werden.
                                        ! (Komprimiss Geschwindigkeit <-> Genauigkeit)

    if ( ABS(1-(cw_r/c_wr2)) < 0.002 ) exit iteration_cwr
        if (c_wr2 > 2.5) then           !zu prüfen! erscheint unlogisch
          cw_r = 1.30
          EXIT iteration_cwr
        end if
    cw_r = c_wr2

        !WP
        ! Mit Hilfe von LAMBDA_SO wird ein neuer Gesamtwiderstandsbeiwert
        ! durch lineare Überlagerung berechnet. Damit wird eine neue
        ! Fließgeschwindigkeit auf dem Vorland V_VOR ermittelt.
        ! -> Nötig für Berechnung von C_WR_UN und a_NL!
        !WP

    lambda_p = (4 * cw_r * h_m * d_p)/(a_x * a_y)
    lambda_g = lambda_p + lambda_s
    v_vor = 1/SQRT(lambda_g) * SQRT(8 * g * I_R * h_m)
    c_wr_un = GET_CW_UN(v_vor, d_p)                         ! Bestimmung von C_WR_UN
    a_NL = GET_A_NL(c_wr_un, d_p, I_R, v_vor, a_NL_anf)     ! Bestimmung von a_NL, gibt 0.0 zurueck,
                                                            ! falls Fehler aufgetreten ist.

    a_NB = GET_A_NB(c_wr_un, d_p, a_NL)                     ! Bestimmung derNachlaufbreite
    vn_vvor2 = 1.15 * (a_NL/a_x) ** (-0.48) + 0.5 * (a_NB/a_y) ** 1.1
    Fr1 = v_vor / SQRT(g * h_m)                             ! Bestimmung der Froudezahl der Strömung
    y_stern = 1.00
    i = 0
    iteration_y_stern: do               !Iteration für Fr2 über y_stern
            i = i+1

            !WP
            ! Mehrere Versuche haben gezeigt, dass sich bei der
                ! Iteration von Fr nach y_stern Problemem bei sehr
                ! niederigen Froudzahlen einstellen! Da die zu lösende
                ! Gleichung quadratisch ist, und der Wert von y_stern
                ! nahe 1 liegen muss, ist gerade am Anfang in sehr
                ! kleinen Schritten vorzugehen!
                ! Bei Werten von Fr1 < 0.1 wird zur Erhöhung der
                ! Stabilität von einem konstanten y_stern = 0.999 ausgegangen.
            !WP

            if (Fr1 < 0.1 ) then
              y_stern = 0.9999
              exit iteration_y_stern
            else if (Fr1 < 0.3 .and. Fr1 >= 0.1) then
              y_stern = y_stern - 0.0001
            else if (Fr1 < 0.6 .and. Fr1 >= 0.3) then
              y_stern = y_stern - 0.001
            else
              y_stern = y_stern - 0.01
            end if

            Fr2 = (y_stern * (y_stern**2 -1)) / (2 * (y_stern - c_temp))
            if (fr2<0.0) then 
              y_stern = 0.9999
              exit iteration_y_stern
            endif
            Fr2 = SQRT(Fr2)

            if ( ABS(1-(Fr1/Fr2)) < 0.1) exit iteration_y_stern

            if (i > 100) then
              y_stern = 0.9999
              exit iteration_y_stern
            END if

    end do iteration_y_stern             ! Iteration für Fr2 über y_stern

        !WP
        ! Während der ersten fünf Iterationszyclen wird zunächst
        ! "ungebremst" iteriert, um eine möglichst schnelle
        ! Annäherung an den Zielwert zu erreichen.
        ! Es hat sich gezeigt, dass sich die Iteration kurz
        ! vor Erreichen des Zielwertes manchmal in einer stabilen
        ! Schwingung "verfängt". Deshalb wird ab der fünften
        ! Iteration eine Begrenzung des Iterationsschrittes
        ! eingebaut.
        !WP

    if (j < 5) then
          delta_cw = 2 * (1-y_stern) / (Fr1**2)
    else if (j >= 5 .AND. j < 10) then
          delta_cw = (delta_cw + (2 * (1-y_stern) / (Fr1**2))) / 2
        else if (j >= 10) then
          delta_cw = (2*delta_cw + (2 * (1-y_stern) / (Fr1**2))) / 3
    end if

    ! Auch hier wird die Veränderung des Iterationsparameters durch Einbeziehung
    ! des vorherigen Wertes abgebremst!
    c_wr2 = (cw_r + (1.3124 * c_wr_un * vn_vvor2 + delta_cw)) / 2

    if (j > 50) then               ! Keine Konvergenz nach 100 Iterationen
          cw_r = 1.30
          warningSign = newWarning (w_IterationCWR)
          call printWarningMessage (warningSign)
          EXIT iteration_cwr
    END if

end do iteration_cwr               ! Iteration bis Konvergenzkriterium für C_WR erfüllt ist

end function GET_CWR


