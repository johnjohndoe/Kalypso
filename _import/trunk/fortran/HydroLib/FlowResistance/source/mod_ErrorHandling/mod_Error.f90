module mod_Error

use mod_Message

type Error
  integer (kind = 4) :: codeID
  type (Message) :: ErrorMessage
  integer (kind = 4) :: ErrorID
end type

integer (kind = 4), parameter :: e_ParametersMissing   = 1001
integer (kind = 4), parameter :: e_DiaPlantsToBig    = 1002
integer (kind = 4), parameter :: e_InvalidVegType      = 1003

contains
!--------------------------------------------------------------------------------
function newError (codeID, ErrorText)
!--------------------------------------------------------------------------------
implicit none

!function definition
type (Error), pointer :: newError

!arguments
integer (kind = 4) :: codeID
character (len = *), optional :: ErrorText

!local variables
type (Error), pointer :: new => null()

allocate (new)
new%codeID = codeID
if (present (ErrorText)) then
  new%ErrorMessage = newMessage (ErrorText)
else
  new%ErrorMessage = getErrorMessageByID (codeID)
endif

!TODO: Fehlerstatus an Benutzer!
new%ErrorID = 0

newError => new
return
end function


!-----------------------------------------------------------------------------
function getErrorMessageByID (codeID)
!-----------------------------------------------------------------------------
implicit none

!function definition
type (Message), pointer :: getErrorMessageByID

!arguments
integer (kind = 4), intent(in) :: codeID
!local variables
type (Message), pointer :: tmpMessage => null()

select case (codeID)
case (e_ParametersMissing)
  tmpMessage = newMessage ('One or more parameters are missing or don´t have a valid value. Please enter a valid value for all non-optional Parameters.')
case (e_DiaPlantsToBig)
  tmpMessage = newMessage ('The Diameter of trees is bigger than the distance between trees. Please enter a valid value for both diameter of trees (dp) and distance between trees (a).')
case (e_InvalidVegType)
  tmpMessage = newMessage ('The vegetaiontype has not been set correctly. You can wether choose living grass (vegType = 1) or dead grass (vegType = 2).')
end select


getErrorMessageByID => tmpMessage

end function

!  subroutine printErrorMessage (Error)
!    call printMessage
!  end subroutine

end module