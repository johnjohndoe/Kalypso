!---------------------------------------------------------------------------------------------
function GET_CWR(I_R, h_m, a_x, d_p, lambda_s) result (cwr)
! The drag coefficient C_WR of vegetation in an element with the
! characteristic properties
! - slope of water surface (I_R),
! - mean flow depth (H_M),
! - mean distance between trees/branches (A_X),
! - mean diameter of trees/branches (D_P) and
! - friction factor of bottom surface
! is calculated.
! The method of LINDNER (modified by PASCHE) like published in
! DVWK 220: Hydraulische Berechnung von Flieﬂgewaessern" 1991 is applied.
! This method contains some nested iterations that may lead to undesirable
! convergence problems espacially with very small values for the slope and
! the water depth. Therefor some limitations have been implemented to
! always get realistic results.
!---------------------------------------------------------------------------------------------
use mod_cwr

implicit none
!DEC$ ATTRIBUTES DLLEXPORT::GET_CWR
!definition block
!-------------------------------------
!function result
REAL (kind = 8)                        :: cwr     ! Drag coefficient for the vegetation

!input parameters
REAL (kind = 8), INTENT(IN)            :: I_R      ! Slope
REAL (KIND = 8), INTENT(IN)            :: h_m      ! Mean flow depth
REAL (kind = 8), INTENT(IN)            :: a_x      ! distance of trees/branches
REAL (kind = 8), INTENT(IN)            :: d_p      ! diameter of trees/branches
REAL (KIND = 8), INTENT(IN)            :: lambda_s ! friction factor by COLBROOK/WHITe

cwr = CW_R (I_R, h_m, a_x, d_p, lambda_s)

end function GET_CWR


