module Errors

contains
!---------------------------------------------------------------------------------------------
subroutine Error (ErrorCode)
!---------------------------------------------------------------------------------------------
implicit none

integer (kind = 4), intent (in) :: ErrorCode
!integer (kind = 4) :: Er_displaycode

!diplaycode = 2000 + errorcode
!if (errorcode > 0) then
!  write (unit = *, fmt = wa_displaycode)
!endif

if (errorCode == 1) then
  write(unit = *, fmt = 1001)
elseif (errorCode == 2) then
  write (unit = *, fmt = 1002)
elseif (errorCode == 3) then
  write (unit = *, fmt = 1003)

endif

pause

1001 format (1X, 'One or more parameters are missing or don´t have a valid value.'/ &
           & 1X, 'Please enter a valid value for all non-optional Parameters. ')

1002 format (1X, 'The Diameter of trees is larger than the distance between trees.'/ &
           & 1X, 'Please enter a valid value for both diameter of trees (dp) and '/ &
           & 1X, 'distance between trees (a).'/ &
           & 1x, 'dp = ', F10.4,' > a = ', F10.4)

1003 format (1X, 'The vegetaiontype has not been set correctly.'/ &
           & 1X, 'You can wether choose living grass (vegType = 1)'/ &
           & 1X, 'or dead grass (vegType = 2).')

end subroutine

end module