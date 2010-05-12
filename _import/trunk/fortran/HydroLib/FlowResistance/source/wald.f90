

!---------------------------------------------------------------------------------------------
function wald (h, a, dp, cwr,aplha_lat) result (lambda_p)
!ermittelt den Darcy-Weisbach Koeffizienten lambda der die Flieszwiederstaende von durchstroemtem Groszbewuchs
!darstellt, durch Umlegen des Einzelwiderstandes eines als Zylinder mit dem Durchmesser dp gedachten 
!Widerstandselementes(Baum) auf die es umgebende Flaeche (a**2).
!---------------------------------------------------------------------------------------------
use mod_globalconstants
use mod_Error
use mod_Warning

implicit none

!DEC$ ATTRIBUTES DLLEXPORT::wald
!definition block
!-------------------------------------
!function result
REAL (kind = 8)                :: lambda_p

!input parameters
REAL (kind = 8), INTENT(IN)    :: dp               !Durchmesser des Bewuchses
REAL (kind = 8), INTENT(IN)    :: a                !Mittlerer Durchmesser des Bewuchses
REAL (kind = 8), INTENT(IN)    :: cwr              !Formwiderstandsbeiwert mit Gruppenwirkung des Bewuchses
REAL (KIND = 8), INTENT(IN)    :: h                !Fliesstiefe
REAL (KIND = 8), INTENT(IN)    :: aplha_lat        !Boeschungsneigung in Grad

!local parameters
integer (kind = 4)             :: errorcode
integer (kind = 4)             :: warningcode
type (Error)                   :: errorSign
type (Warning)                 :: warningSign

!initializations block
!--------------------------------------

!execution block
!--------------------------------------

!missing entry
if (dp * a * cwr * h == 0.0) then
  errorSign = newError (e_ParametersMissing)
  call printErrorMessage (errorSign)
  stop
endif

if (dp < 0.001) then
! no plants
  lambda_p = 0.0
  warningSign = newWarning (w_noPlants)
  call printWarningMessage (warningSign)
  return
else
  IF (dp > a) then
  ! Diameter of trees is larger than the distance
    errorSign = newError (e_DiaPlantsToBig)
    call printErrorMessage (errorSign)
    stop
  END if
  lambda_p = ( (4.0 * h * dp) / (a**2) ) * cwr * cos(aplha_lat*Pi/180)
end if

!Restrict lambda to be maximum 1000.0
if (lambda_p > 0.0) then
  lambda_p = min (lambda_p, 1000.0)
  warningSign = newWarning (w_LambdaRestriction)
  call printWarningMessage (warningSign)
endif

END function wald

