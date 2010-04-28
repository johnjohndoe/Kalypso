

!---------------------------------------------------------------------------------------------
function AgPeFuentes (alpha, beta, dm, rhy) result (lambdaExtrRau)
!Berechnung Fliesswiderstand bei extremen Rauheiten
!---------------------------------------------------------------------------------------------
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


!initializations block
!--------------------------------------

!execution block
!--------------------------------------
lambdaExtrRau = (0.88 * beta * dm / rhy + 2.03 * log10(11.1 * rhy / alpha / dm))**(-2)
return

end function AgPeFuentes

