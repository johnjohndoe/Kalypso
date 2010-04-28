

!---------------------------------------------------------------------------------------------
function wald (h, a, dp, cwr,aplha_lat) result (lambda_p)
!ermittelt den Darcy-Weisbach Koeffizienten lambda der die Flieszwiederstaende von durchstroemtem Groszbewuchs
!darstellt, durch Umlegen des Einzelwiderstandes eines als Zylinder mit dem Durchmesser dp gedachten 
!Widerstandselementes(Baum) auf die es umgebende Flaeche (a**2).
!---------------------------------------------------------------------------------------------
use globalconstants
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

!initializations block
!--------------------------------------

!execution block
!--------------------------------------
if (dp < 0.001) then
! kein Bewuchs
  lambda_p = 0.0
  return

else
  IF (dp > a) then
  !Bewuchsdurchmesser ueberschneiden sich.
    write (*,1000) dp, a
    stop
  END if

  lambda_p = ( (4.0 * h * dp) / (a**2) ) * cwr * cos(aplha_lat*Pi/180)

end if
!Restrict lambda to be maximum 1000.0
if (lambda_p > 0.0) lambda_p = min (lambda_p, 1000.0)

1000 format (1X, 'Diameter of trees is larger than the distance'/ &
           & 1X, 'between trees:'/ &
           & 1X, 'D_p = ', F10.4,' > a = ', F10.4/ &
           & 1X, 'ABORTING program!')

END function wald

