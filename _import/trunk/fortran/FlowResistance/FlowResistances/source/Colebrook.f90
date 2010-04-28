


!---------------------------------------------------------------------------------------------
function cole (vecq, rhy, ks, minRe) result (lambda_s)
!                                                                       
! Calculation of DARCY-WEISBACH coefficient LAMBDA_S using the
! COLEBROOK-WHITE formular with the equivelent sand roughness ks.
! This is an iterative process considering the REYNOLDS number.
!---------------------------------------------------------------------------------------------
use globalconstants
implicit none
!DEC$ ATTRIBUTES DLLEXPORT::cole
!definition block
!-------------------------------------
!function result
REAL (kind = 8) :: lambda_s

!input parameters
real (kind = 8), intent(in)  :: ks             ! equivelent sand roughness ks-value
real (kind = 8), intent(in)  :: vecq           ! velocity
real (kind = 8), intent(in)  :: rhy            ! flow depth in wide channels, hydraulic radius
real (kind = 8), intent(in), optional :: minRe !Mindestreynoldszahl

!Local variables
REAL (kind = 8) :: re, lalt, dhy

! For the shape-influence of open channels, a shape parameter is
! introduced (see BOLLRICH, "Technische Hydromechanik", p.252)
!
!                                      f_g     f_r
! -----------------------------------------------------------
! for rectangular channels b = h      2.80    3.45
! for rectangular channels b = 2*h    2.90    3.30
! for wide channels                   3.05    3.05
! for half filled circular pipes      2.60    3.60
! for filled circular pipes           2.51    3.71

!REAL, PARAMETER  :: f_g = 3.05
!REAL, PARAMETER  :: f_r = 3.05
real, parameter  :: f_g = 2.51
real, parameter  :: f_r = 3.71
integer          :: i

!initializations block
!--------------------------------------
lambda_s = 0.0d0
lalt = 10.0**6    ! initial lambda
dhy  = 4.0 * rhy  ! hydraulic diameter


!execution block
!--------------------------------------
!prevent division by zero
if (ks == 0.0d0) return 

!formula by COLEBROOK/WHITE under consideration of the hydraulic smooth term wrt to Reynolds-number
lambda_s = ( -2.03 * log10 (ks / (dhy * f_r))) ** (-2.0)

!Reynolds number
!If Reynoldsnumber less 2000 we have laminar flow. But this will never occur
!in natural rivers. It only happens in case of very small velocities or
!very shallow water. But then the influence is negligible.
!(RE < 2000 may lead to inconvergence behaviour!)
if (vecq > 0.0d0) then 
  re = vecq * dhy / nu_10
  if (present (minre)) then
    IF (re < minre) re = minre
  endif
  iteration_lambda: do i = 1, 30
    if ( ABS((lambda_s/lalt)-1.0) <= 0.0001 ) exit iteration_lambda
    IF (i == 30) then
      !no convergence after 30 iterations
      lambda_s = 0.05
      exit iteration_lambda
    endif
    lalt = lambda_s
    !formula by COLEBROOK/WHITE under consideratio of the hydraulic smooth term wrt to Reynolds-number
    lambda_s = ( -2.03 * log10 (f_g / (re * lalt**0.5) + ks / (dhy * f_r) ) ) ** (-2.0)
    !Restrict lambda to be maximum 1000.0
    if (lambda_s > 0.0) lambda_s = min (lambda_s, 1000.0)
  end do iteration_lambda
endif

return

end function cole

