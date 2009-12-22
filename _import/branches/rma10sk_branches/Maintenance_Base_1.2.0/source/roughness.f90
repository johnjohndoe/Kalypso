!     Last change:  WP   29 Aug 2007   12:13 pm
!-----------------------------------------------------------------------
! This code, roughness.f90, computes the resistance factor lambda according
! the Darcy-Weissbahc-resistance law in the library 'Kalypso-2D'.
! Copyright (C) 2004  SEBASTIAN RATH & WOLF PLOEGER.
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
! Sebastian Rath:   phone: +49 40 42878 4180 mail: s.rath@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-2D'.
!
! Sebastian Rath, 09 August 2004
! Research Associate
!
!

!nis,may07: Add switch for sort of calculation 1D or 2D
!SUBROUTINE darcy (lambda, vecq, h, ks, a, dp, nn, morph, bedform, mel, c_wr)
SUBROUTINE darcy (lambda, vecq, h, ks, a, dp, nn, morph, bedform, mel, c_wr, approxdim, lambdasand, lambdawald, lambdabedform, dset)
!-
!
!
!-----------------------------------------------------------------------
! Some additional information of relevance:
!-----------------------------------------------------------------------
! Berechnet den Darcy-Weissbach-Sohlschubspannungsbeiwert LAMBDA.
! Vereinfachend wird angenommen, dass sich die Fliesswiderstaende
! infolge von Sohlrauhigkeit (COLE), den Wasserkoerper
! vollstaendig durchkreuzendem Grossbewuchs (WALD) und Sohlstrukturen
! (FORMRAUHEIT) additiv ueberlagern lassen.
!-----------------------------------------------------------------------
IMPLICIT none


! Calling variables
REAL (kind = 8), INTENT(OUT)                   :: lambda
REAL (kind = 8), INTENT(OUT)                   :: lambdasand
real (kind = 8) :: cole
real (kind = 8), intent(out)                   :: lambdawald, lambdabedform
INTEGER, INTENT(IN)                            :: morph, nn, mel
REAL, INTENT(IN)                               :: vecq
real (kind = 8), INTENT(IN)                    :: ks, a, dp
REAL(KIND=8), intent (in)                      :: h
REAL (KIND = 8), DIMENSION(1:mel,1:4), INTENT(IN) :: bedform
REAL (KIND = 8), INTENT(INOUT)                    :: c_wr
real (kind = 8), intent (in)                   :: dset

!Add switch for approximation decision
INTEGER                                        :: approxdim
!-

! initializations
lambdasand	  = 0.0
lambdawald    = 0.0
lambdabedform = 0.0

!NiS,may06: testing
 !IF (ks.le.0.0) stop 'ks.le.0'
IF (ks.le.0.0) then
  WRITE(*,*) nn
  stop 'ks.le.0'
endif
!-

IF (h < dset) then
  lambda = 1000.0
  RETURN
ENDIF

!function to calculate lambda due to equivalent sand roughness ks
lambdasand = cole (vecq, h, ks)

!hint: h is in case of 1D used as the hydraulic radius

if (approxdim == 2) then
  ! Bedform is not calculated when trees occur!
  if (nn/=0 .AND. morph /= 0 .and. dp /= 0.0) then
    CALL formrauhheit (lambdabedform, nn, bedform, mel, h)
  else
    lambdabedform = 0.0
  end if


  if (dp > 0.0 .and. h > 0.0) then
    CALL wald (lambdawald, h, a, dp, c_wr)
  else
    lambdawald = 0.0
    c_wr = 0.0
  end if
endif

lambda = lambdasand + lambdawald + lambdabedform

!nis, prohibit mathematical lambda values
if (lambda > 0.0) lambda = min (1000.0, lambda)

END SUBROUTINE darcy




!---------------------------------------------------------------------------------------------
function cole (vecq, rhy, ks) result (lambda)
!                                                                       
! Calculation of DARCY-WEISBACH coefficient LAMBDA using the
! COLEBROOK-WHITE formular with the equivelent sand roughness ks.
! This is an iterative process considering the REYNOLDS number.
!---------------------------------------------------------------------------------------------

implicit none

!function result
REAL (kind = 8) :: lambda

!input parameters
REAL (kind = 8), INTENT(IN)  :: ks     ! ks-value
REAL, INTENT(IN)             :: vecq   ! velocity
REAL(KIND=8), INTENT(IN)     :: rhy      ! flow depth, hydraulic radius

!Local variables
REAL 			                     :: re, lalt, dhy

! kinematic viskosity at 10 C
REAL, PARAMETER :: nue = 1.3e-06


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
REAL, PARAMETER  :: f_g = 2.51
REAL, PARAMETER  :: f_r = 3.71
INTEGER 	 :: i

!initializations
lambda = 0.0d0
lalt = 10.0**6  	! initial lambda
dhy  = 4.0 * rhy  ! hydraulic diameter


!prevent division by zero
if (ks == 0.0d0) return 

!formula by COLEBROOK/WHITE under consideration of the hydraulic smooth term wrt to Reynolds-number
lambda = 1/ ( -2.03 * log10 (ks / (dhy * f_r))) ** 2.0

!WP Reynolds number
!WP ---------------
!WP If Reynoldsnumber less 2000 we have laminar flow. But this will never occur
!WP in natural rivers. It only happens in case of very small velocities or
!WP very shallow water. But than the influence is negligible.
!WP (RE < 2000 may lead to inconvergence behaviour!)
if (vecq > 0.0d0) then 
  re = vecq * dhy / nue
  IF (re .lt. 2000.0) re = 2000.0


  iteration_lambda: do i = 1, 30

    if ( ABS((lambda/lalt)-1.0) .LE. 0.0001 ) EXIT iteration_lambda
    IF (i == 30) then
      !no convergence after 30 iterations
      lambda = 0.05
      EXIT iteration_lambda
    ENDIF

    lalt = lambda
    !formula by COLEBROOK/WHITE under consideratio of the hydraulic smooth term wrt to Reynolds-number
    lambda = ( -2.03 * log10 (f_g / (re * lalt**0.5) + ks / (dhy * f_r) ) ) ** 2.0

    IF (lambda .eq. 0.0) lambda = 0.0001
    lambda = 1.0 / lambda
    !nis,oct08: Restrict lambda to be maximum 1000.0
    if (lambda > 0.0) lambda = min (lambda, 1000.0)

  end do iteration_lambda
endif
  

return


1000 format (/1X,'No convergence in Element No. ', I5, ' after 20 iterations'/ &
           & 1X, 'lambda: ', F15.7/ &
           & 1X, 'lalt:   ', F15.7/ &
           & 1X, 'h:      ', F15.7/ &
           & 1X, 'vecq:   ', F15.7/ &
           & 1X, 'Re:     ', F15.7/ &
           & 1X, 'dhy:    ', F15.7/ &
           & 1X, 'ks:     ', F15.7/ &
           & 1X, 'ATTENTION: lambda is set to 0.1!'/)
1001 format (1X, 'It. Nr. ', I3, '  lambda: ', F15.7, ' lalt: ', F15.7)
1002 format (1X, 'Element: ', I6, '  lambda_anf: ', F15.7)


END function cole                           
                                                                        
                                                                        
                                                                        
!-----------------------------------------------------------------------
SUBROUTINE wald (lambda, h, a, dp, cwr)
!                                                                       
!     ermittelt den Darcy-Weisbach Koeffizienten lambda der             
!     die Flieszwiederstaende von durchstroemtem Groszbewuchs           
!     darstellt, durch Umlegen des Einzelwiderstandes eines als         
!     Zylinder mit dem Durchmesser dp gedachten Widerstandselementes    
!     (Baum) auf die es umgebende Flaeche (a**2).                       
!     Ermittlung des Wiederstandsbeiwertes cwr des Einzelwiderstandes   
!     nach Pasche.                                                      
!-----------------------------------------------------------------------
                                                                        
implicit none

! Calling variables
!NiS,jul06: Consistent variable types!
!REAL, INTENT(IN) 	:: h, a, dp, cwr
REAL (kind = 8), INTENT(IN) 	:: a, dp
REAL (kind = 8), INTENT(IN) 	            :: cwr
REAL(KIND=8), INTENT(IN)      :: h
!-
REAL (kind = 8), INTENT(OUT) 	:: lambda

! Local variables
REAL :: ax, ay, dur
                                                                        
if (dp < 0.001) then

  ! No plants/trees
  lambda = 0.0
  return

else

  ax = abs (a)
  ay = abs (a)
  dur = abs (dp)
  IF (dp .gt. ax) then
    write (*,1000) dp, ax
    stop
  END if
                                                                        
  lambda = ( (4.0 * h * dur) / (ax * ay) ) * cwr
                                                                        
end if

!nis,oct08: Restrict lambdaWald less than 1000.0
if (lambda > 0.0) lambda = min (lambda, 1000.0)

1000 format (1X, 'Diameter of trees is larger than the distance'/ &
	   & 1X, 'between trees:'/ &
           & 1X, 'D_p = ', F10.4,' > A_x = A_y = ', F10.4/ &
           & 1X, 'ABORTING program!')

END SUBROUTINE wald
                                                                        

                                                                        
                                                                        

!SK---------------------------------------------------------------------------------------------------------------------------------
subroutine formrauhheit(lambdasohle, nn, bedform, mel, h)
!SK
!SK Subroutine zur Berechnung der Formrauhigkeit
!SK
!SK---------------------------------------------------------------------------------------------------------------------------------

! Subroutine wird aufgerufen aus der subroutine 'Darcy'

implicit none

! Calling variables
INTEGER, INTENT(IN) 				:: nn, mel
!NiS,jul06: Consistent variable types!
REAL (KIND=8), INTENT(IN)                       :: h
!-
REAL (kind = 8), INTENT(OUT)                      :: lambdasohle
REAL (kind = 8), DIMENSION(1:mel,1:4), INTENT(IN) :: bedform

! Local variables
REAL (kind = 8) :: lambda_s_r = 0.0
real (kind = 8) :: lambda_s_d = 0.0

! Formrauhigkeit Riffel
if (bedform(nn,4) <= 0.0001) then
    lambda_s_r = 0.0
else
    lambda_s_r = (8 / (2 * h) ) * bedform(nn,3) * (bedform(nn,4)**2)
end if

! Formrauhigkeit aus Duenen
if (bedform(nn,2) <= 0.0001) then
    lambda_s_d = 0.0
else
    lambda_s_d = (8 / (2 * h) ) * bedform(nn,1) * (bedform(nn,2)**2)

END if


! Gesamtrauhigkeit

lambdasohle = lambda_s_d + lambda_s_r


end subroutine formrauhheit





