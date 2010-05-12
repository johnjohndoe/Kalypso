
module mod_kouwen

!Vegetation type enumerations
integer (kind = 4), parameter :: enum_VegInVegPhase = 1 !Gras in Vegetationsphase
integer (kind = 4), parameter :: enum_VegInWinter = 2   !totes Gras

contains

!---------------------------------------------------------------------------------------------
function fvschub (h, IE) result (vschub)
!Berechnung der Schubspannungsgeschwindigkeit
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8) :: vschub            !Schubspannungsgeschwindigkeit

!input parameters
REAL (KIND = 8), INTENT(IN) :: h     !Fliesstiefe
REAL (KIND = 8), INTENT(IN) :: IE    !Gefaelle
!initializations block
!-------------------------------------

!execution block
!--------------------------------------

vschub = (g * h * iE)**0.5           !Berechnung der Schubspannungsgeschwindigkeit

return

end function fvschub

!---------------------------------------------------------------------------------------------
function fvschubkrit (MEI) result (vschubkrit)
!Berechnung der kritischen Schubspannungsgeschwindigkeit
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8) :: vschubkrit             !Kritische Schubspannungsgeschwindigkeit

!input parameters

!local parameters
REAL (KIND = 8) :: MEI                    !Bewuchssteifigkeit

!initializations block
!-------------------------------------

!execution block
!--------------------------------------

if (0.028 + 6.33 * MEI < 0.23 * MEI**0.106) then   !Berechnung der kritischen Schubspannungsgeschwindigkeit
    vschubkrit = (0.028 + 6.33 * MEI)
  else 
    vschubkrit = 0.23 * MEI**0.106
endif

return
end function fvschubkrit


!---------------------------------------------------------------------------------------------
function fMEI (hG, vegType) result (MEI)
!Berechnung der Bewuchssteifigkeit
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8) :: MEI                     !Bewuchssteifigkeit

!input parameters
REAL (KIND = 8), INTENT(IN) :: hG          !Bewuchshoehe
INTEGER (KIND = 4), INTENT(IN) :: vegType  !Vegetationstyp
!initializations block
!--------------------------------------

!execution block
!--------------------------------------
select case (vegType)              !Wahl der Vegetationsphase und Berechnung der Bewuchssteifigkeit
  case (enum_vegInVegPhase)
    MEI = 319 * hG**3.3
  case (enum_vegInWinter)
    MEI = 25.4 * hG**2.26
end select

return
end function fMEI

!---------------------------------------------------------------------------------------------
function fkG (MEI, tauSo, hG) result (kG)
!Berechnung der Bewuchshoehe gelegt
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8) :: kG                       !Bewuchshoehe gelegt

!input parameters
REAL (KIND = 8), INTENT(IN) :: hG           !Bewuchshoehe

!local parameters
REAL (KIND = 8), INTENT(IN) :: MEI          !Bewuchssteifigkeit
REAL (KIND = 8), INTENT(IN) :: tauSo        !Sohlschubspannung

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
kG = 0.14 * hG * (((MEI/tauSo)**0.25)/hG)**1.59  !Berechnung der Bewuchshoehe gelegt

return
end function fkG

!---------------------------------------------------------------------------------------------
function ftauSo (rhy, IE) result (tauSo)
!Berechnung der Sohlschubspannung
!---------------------------------------------------------------------------------------------
use mod_globalconstants
implicit none

!definition block
!-------------------------------------
!function result
REAL (KIND = 8) :: tauSo                !Sohlschunbspannung

!input parameters
REAL (KIND = 8), INTENT(IN) :: IE       !Gefaelle
REAL (KIND = 8), INTENT(IN) :: rhy      !Hydraulischer Radius
!initializations block
!--------------------------------------

!execution block
!--------------------------------------
tauSo = rho * g * rhy * IE              !Berechnung der Sohlschubspannung

return
end function ftauSo

!---------------------------------------------------------------------------------------------
function fa (rel_v_crit) result (a)
!Ermittlung des Parameters a
!---------------------------------------------------------------------------------------------
use mod_globalconstants

implicit none
!function result
real (kind = 8) :: a              !Parameter

!input parameters
real (kind = 8) :: rel_v_crit     !Quotient aus den Schubspannungsgeschwindigkeiten

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
if (rel_v_crit < 1) then                                  !Ermittlung des Parameters a aus Tabelle
    a = 0.15d0
  elseif (1.0 <= rel_v_crit .and. rel_v_crit <= 1.5) then
    a = 0.2d0
  elseif (1.5 <= rel_v_crit .and. rel_v_crit <= 2.5) then
    a = 0.28d0
  else 
    a = 0.29d0
endif

return
end function fa

!---------------------------------------------------------------------------------------------
function fb (rel_v_crit) result (b)
!Ermittlung des Parameters b
!---------------------------------------------------------------------------------------------
use mod_globalconstants

implicit none
!function result
real (kind = 8) :: b             !Parameter

!input parameters
real (kind = 8) :: rel_v_crit    !Quotient aus den Schubspannungsgeschwindigkeiten

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
if (rel_v_crit < 1) then                               !Ermittlung des Parameters b aus Tabelle
    b = 1.85d0
  elseif (1.0 <= rel_v_crit .and. rel_v_crit <= 1.5) then
    b = 2.7d0
  elseif (1.5 <= rel_v_crit .and. rel_v_crit <= 2.5) then
    b = 3.08d0
  else
    b = 3.5d0
endif

return
end function fb

!---------------------------------------------------------------------------------------------
function kouwen (hG, rhy, IE, h, vegType) result (lambdakouwen)
! Calculation of Kouwen coefficient LAMBDAKOUWEN (1988)
!---------------------------------------------------------------------------------------------
use mod_globalconstants
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



end module !mod_kouwen
