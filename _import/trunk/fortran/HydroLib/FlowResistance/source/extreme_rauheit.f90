

!---------------------------------------------------------------------------------------------
function AgPeFuentes (alpha, beta, dm, rhy) result (lambdaExtrRau)
!Berechnung Fliesswiderstand bei extremen Rauheiten
!---------------------------------------------------------------------------------------------
use mod_globalconstants
use mod_Error
use mod_Warning

implicit none

!DEC$ ATTRIBUTES DLLEXPORT::AgPeFuentes
!definition block
!-------------------------------------
!function result
real (kind = 8) :: lambdaExtrRau

!input parameters
REAL (kind = 8), INTENT(IN) :: alpha       !Textur-Parameter, der Form und Anordnung der Rauheitselemente beruecksichtigt
REAL (kind = 8), INTENT(IN) :: beta        !Wake-Parameter beschreibt Nachlaufzone
REAL (kind = 8), INTENT(IN) :: dm          !mittlerer Durchmesser
REAL (kind = 8), INTENT(IN) :: rhy         !hydraulischer Radius

!local parameters
type (Error)                :: errorSign
type (Warning)              :: warningSign

!initializations block
!--------------------------------------

!execution block
!--------------------------------------

!missing entry
if (alpha * beta * dm * rhy == 0.0) then
  errorSign = newError (e_ParametersMissing)
  call printErrorMessage (errorSign)
  stop
endif

!Diameter of roughness elements may be to big
if (dm >= 0.5) then
  warningSign = newWarning (w_RoughDiaBig)
  call printWarningmessage (warningSign)
endif

!Diameter of roughness elements may be to small
if (dm <= 0.5 .and. dm < 0.0) then
  warningSign = newWarning (w_RoughDiaSmall)
  call printWarningmessage (warningSign)
endif

lambdaExtrRau = (0.88 * beta * dm / rhy + 2.03 * log10(11.1 * rhy / alpha / dm))**(-2)

!Restrict lambda to be maximum 1000.0
if (lambdaExtrRau > 0.0) then
  lambdaExtrRau = min (lambdaExtrRau, 1000.0)
  warningSign = newWarning (w_LambdaRestriction)
  call printWarningMessage (warningSign)
endif

return

end function AgPeFuentes

