module Warnings

contains
!---------------------------------------------------------------------------------------------
subroutine warning (warningCode)
!---------------------------------------------------------------------------------------------
implicit none

integer (kind = 4), intent (in) :: warningCode
!integer (kind = 4) :: wa_displaycode

!diplaycode = 2000 + warningcode
!if (warningcode > 0) then
!  write (unit = *, fmt = wa_displaycode)
!endif

if (warningCode == 1) then
  write(unit = *, fmt = 2001)
elseif (warningCode == 2) then
  write (unit = *, fmt = 2002)
elseif (warningcode == 3) then
  write (unit = *, fmt = 2003)
elseif (warningcode == 4) then
  write (unit = *, fmt = 2004)
elseif (warningcode == 5) then
  write (unit = *, fmt = 2005)
elseif (warningcode == 6) then
  write (unit = *, fmt = 2006)
elseif (warningcode == 7) then
  write (unit = *, fmt = 2007)
endif

pause



2001 format (1X, 'The value for lambda has been set to 1000'/ &
           & 1x, 'due to an even higher value from the calculation.'/ &
           & 1x, 'Please check all parameter values.')

2002 format (1X, 'The diameter of the roughness elements (dm) is to small to get acceptable results.'/ &
           & 1X, 'You should think about using another calculation method.'/ &
           & 1X, 'For example the COLEBROOK-WHITE formular with the equivelent sand roughness (ks).)'

2003 format (1X, 'The diameter of the roughness elements (dm) is to big to get acceptable results.'/ &
           & 1X, 'You should think about using another calculation method.'/ &
           & 1X, 'For example the calculation of the DARCY-WEISBACH coefficient lambda by using' / &
           & 1x, 'the Formwiderstand of cylinders.')

2004 format (1X, 'There are wether no plants or the diameter of trees and branches is to small to  '/ &
           & 1X, 'calculate the DARCY-WEISBACH coefficient lambda by using the Formwiderstand of cylinders.'/ &
           & 1x, 'dp = ', F10.4, 1x, 'lambda is set to 0.0')

2005 format (1X, 'After 30 steps of iteration no convergence occured.'/ &
           & 1X, 'Lambda is set to 0.05.')

2006 format (1X, 'After 50 steps of iteration no convergence occured.'/ &
           & 1X, 'a_NL is set to 0.0.')

2007 format (1X, 'After 50 steps of iteration no convergence occured.'/ &
           & 1X, 'cw_r is set to 1.3.')
           
end subroutine

end module
