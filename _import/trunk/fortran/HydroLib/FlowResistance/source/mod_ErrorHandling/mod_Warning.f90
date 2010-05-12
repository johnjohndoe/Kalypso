module mod_Warning

use mod_Message

type Warning
  integer (kind = 4) :: codeID
  type (Message) :: warningMessage
  integer (kind = 4) :: WarningID
end type

integer (kind = 4), parameter :: w_LambdaRestriction   = 2001
integer (kind = 4), parameter :: w_RoughDiaSmall       = 2002
integer (kind = 4), parameter :: w_RoughDiaBig         = 2003
integer (kind = 4), parameter :: w_NoPlants            = 2004
integer (kind = 4), parameter :: w_IterationLambda     = 2005
integer (kind = 4), parameter :: w_IterationANL        = 2006
integer (kind = 4), parameter :: w_IterationCWR        = 2007

contains
!--------------------------------------------------------------------------------
function newWarning (codeID, warningText)
!--------------------------------------------------------------------------------
implicit none

!function definition
type (Warning), pointer :: newWarning

!arguments
integer (kind = 4) :: codeID
character (len = *), optional :: warningText

!local variables
type (Warning), pointer :: new => null()

allocate (new)
new%codeID = codeID
if (present (warningText)) then
  new%warningMessage = newMessage (warningText)
else
  new%warningMessage = getWarningMessageByID (codeID)
endif

!TODO: Fehlerstatus an Benutzer!
new%WarningID = 0

newWarning => new
return
end function


!-----------------------------------------------------------------------------
function getWarningMessageByID (codeID)
!-----------------------------------------------------------------------------
implicit none

!function definition
type (Message), pointer :: getWarningMessageByID
!arguments
integer (kind = 4), intent(in) :: codeID
!local variables
type (Message), pointer :: tmpMessage => null()

select case (codeID)
case (w_LambdaRestriction)
  tmpMessage = newMessage ('The value for lambda has been set to 1000 due to an even higher value from the calculation. Please check all parameter values.')
case (w_RoughDiaSmall)
  tmpMessage = newMessage ('The diameter of the roughness elements (dm) is to small to get acceptable results. You should think about using another calculation method. For example the COLEBROOK-WHITE formular with the equivelent sand roughness (ks).')
case (w_RoughDiaBig)
  tmpMessage = newMessage ('The diameter of the roughness elements (dm) is to big to get acceptable results. You should think about using another calculation method. For example the calculation of the DARCY-WEISBACH coefficient lambda by using the Formwiderstand of cylinders.')
case (w_NoPlants)
  tmpMessage = newMessage ('There are wether no plants or the diameter of trees and branches is to small to calculate the DARCY-WEISBACH coefficient lambda by using the Formwiderstand of cylinders. lambda is set to 0.0')
case (w_IterationLambda)
  tmpMessage = newMessage ('After 30 steps of iteration no convergence occured. Lambda has been set to 0.05.')
case (w_IterationANL)
  tmpMessage = newMessage ('After 50 steps of iteration no convergence occured. a_NL has been set to 0.0.')
case (w_IterationCWR)
  tmpMessage = newMessage ('After 50 steps of iteration no convergence occured. cw_r has been set to 1.3.')
end select


getWarningMessageByID => tmpMessage

end function

!  subroutine printWarningMessage (warnung)
!    call printMessage
!  end subroutine

end module