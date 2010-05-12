!---------------------------------------------------------------------------------------------
function kouwen (hG, rhy, IE, h, vegType) result (lambdakouwen)
! Calculation of Kouwen coefficient LAMBDAKOUWEN (1988)
!---------------------------------------------------------------------------------------------
use mod_globalconstants
use mod_kouwen
use mod_Error
use mod_Warning

implicit none
!DEC$ ATTRIBUTES DLLEXPORT::kouwen
!definition block
!-------------------------------------
!function result
real (kind = 8) :: lambdakouwen

!input parameters
REAL (kind = 8), INTENT(IN) :: hG          !Bewuchshoehe
REAL (kind = 8), INTENT(IN) :: rhy         !Hydraulischer Radius
REAL (kind = 8), INTENT(IN) :: IE          !Gefaelle
REAL (kind = 8), INTENT(IN) :: h           !Fliesstiefe
INTEGER (KIND = 4), INTENT(IN) :: vegType  !Vegetationstyp

!loacal parameters
REAL (KIND = 8) :: kG                                  !Bewuchshoehe gelegt
REAL (KIND = 8) :: a, b                                !Kouwen Parameter
REAL (KIND = 8) :: MEI                                 !Bewuchssteifigkeit
REAL (KIND = 8) :: tauSo                               !Sohlschubspannung
REAL (KIND = 8) :: v_schub, v_schub_crit,rel_v_crit    !Schubspannungsgeschwindigkeiten
integer (kind = 4) :: errorcode
integer (kind = 4) :: warningcode
type (Error)       :: errorSign
type (Warning)     :: warningSign

!initializations block
!--------------------------------------

!execution block
!--------------------------------------

!missing entry
if (hG * rhy * IE * h == 0.0) then
  errorSign = newError (e_ParametersMissing)
  call printErrorMessage (errorSign)
  stop
endif

!Vegetation Type is not valid
if (VegType /= 1 .or. VegType /= 2) then
  errorSign = newError (e_InvalidVegType)
  call printErrorMessage (errorSign)
endif

MEI = fMEI (hG, vegType)                     !Berechnung Bewuchssteifigkeit
tauSo = ftauSo (rhy, IE)                     !Berechnung Sohlschubspannung
kG = fkG (MEI, tauSo, hG)                    !Berechnung Bewuchshoehe gelegt
v_schub = fvschub (h, IE)                    !Berechnung Schubspannungsgeschwindigkeiten
v_schub_crit = fvschubkrit (MEI)             !Berechnung Schubspannungsgeschwindigkeiten
rel_v_crit = v_schub/ v_schub_crit           !Quotient aus Schubspannungsgeschwindigkeiten
a = fa(rel_v_crit)                           !Ermittlung der Parameter aus Tabelle
b = fb(rel_v_crit)                           !Ermittlung der Parameter aus Tabelle
lambdakouwen = (a + b * log10 (h / kG))**(-2)!Berechnung des Fliesswiderstands nach Kouwen

!Restrict lambda to be maximum 1000.0
if (lambdakouwen > 0.0) then
  lambdakouwen = min (lambdakouwen, 1000.0)
  warningSign = newWarning (w_LambdaRestriction)
  call printWarningMessage (warningSign)
endif

return

end function kouwen

