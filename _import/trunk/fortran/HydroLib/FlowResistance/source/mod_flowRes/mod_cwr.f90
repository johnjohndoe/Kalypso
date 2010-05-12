
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
REAL (KIND = 8), INTENT(IN) :: v_vor      ! Flieﬂgeschwindigkeit
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
REAL (KIND = 8), INTENT(IN) :: I_R        ! Station‰res Gef‰lle
REAL (KIND = 8), INTENT(IN) :: v_vor      ! Maﬂgebende Flieﬂgeschwindigkeit
REAL (KIND = 8), INTENT(IN) :: a_NL_anf   ! Anfangswert f¸r Iteration ( = 2 * ax )

! Local parameters
INTEGER          :: i
REAL (KIND = 8)  :: a_NL2
type (warning)   :: warningSign

!initializations block
!--------------------------------------
a_NL  = a_NL_anf
a_NL2 = 2 * a_NL_anf

i = 0                              ! Z‰hler der Iteration von a_NL

!execution block
!--------------------------------------
iteration_a_NL: do i = 1, 50                 ! Iteration bis Konvergenzkriterium f¸r a_NL erf¸llt ist

  if ( ABS(1-(a_NL/a_NL2)) < 0.001 ) then 
    exit iteration_a_NL
  endif

  if (i == 50) then
  ! no convergence after 30 iterations
    a_NL2 = 0
    warningSign = newWarning (w_IterationANL)
    call printWarningMessage (warningSign)
    exit iteration_a_NL
  END if

  a_NL  = a_NL2
  a_NL2 = (a_NL + 128.9 * c_wr_un * d_p * (1 + (g * a_NL * I_R)/((v_vor**2)/2))**(-2.14)) / 2

end do iteration_a_NL              ! Iteration von a_NL erfolgreich!

A_NL = a_NL2                    ! Zuweisung des endg¸ltigen Wertes
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
REAL (KIND = 8), INTENT(IN) :: a_NL       ! Nachlaufl‰nge

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
A_NB = 0.24 * (a_NL ** 0.59) * (c_wr_un * d_p) ** 0.41

END function GET_A_NB

END module !get_cwr