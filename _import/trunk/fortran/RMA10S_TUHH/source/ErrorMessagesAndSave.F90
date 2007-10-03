!     Last change:  WP    2 Oct 2007    8:48 pm
subroutine ErrorMessageAndStop (ErrorID, Object, coorx, coory)

implicit none

INTEGER, INTENT(IN)          :: ErrorID, Object
INTEGER                      :: istat
REAL (KIND = 8), INTENT (IN) :: coorx, coory
INTEGER                      :: ErrorUnit

!This subroutine generates an Error function and writes it to the ERROR.OUT file. Afterwards it stops the program.
!This is good for bringing together all the Errors in a list.

ErrorUnit = 75

CLOSE (75)
istat = 0
OPEN  (unit = ErrorUnit, FILE = 'ERROR.OUT', iostat = istat)
if (istat /= 0) STOP 'error'

if (ErrorID == 1001) then
  WRITE (ErrorUnit, 1001) Object
  WRITE (        *, 1001) Object
elseif (ErrorID == 1002) then
  WRITE (ErrorUnit, 1002) Object
  WRITE (        *, 1002) Object
elseif (ErrorID == 1101) then
  WRITE (ErrorUnit, 1101) Object
  WRITE (        *, 1101) Object
elseif (ErrorID == 1102) then
  WRITE (ErrorUnit, 1102) Object
  WRITE (        *, 1102) Object
elseif (ErrorID == 1201) then
  WRITE (ErrorUnit, 1201) Object
  WRITE (        *, 1201) Object
elseif (ErrorID == 1301) then
  WRITE (ErrorUnit, 1301) Object
  WRITE (        *, 1301) Object
elseif (ErrorID == 1401) then
  WRITE (ErrorUnit, 1401) Object
  WRITE (        *, 1401) Object
elseif (ErrorID == 1402) then
  WRITE (ErrorUnit, 1402) Object
  WRITE (        *, 1402) Object
elseif (ErrorID == 1501) then
  WRITE (ErrorUnit, 1501) Object
  WRITE (        *, 1501) Object
elseif (ErrorID == 3601) then
  WRITE (ErrorUnit, 3601) Object
  WRITE (        *, 3601) Object
elseif (ErrorID == 4001) then
  WRITE (ErrorUnit, 4001) Object
  WRITE (        *, 4001) Object
end if


WRITE (ErrorUnit, 999) coorx, coory
WRITE (        *, 999) coorx, coory

!999    Position of Error
!***
  999 FORMAT (1x, 'Error occurs at or near to position: ' // &
            & 1x, 'Rechtswert: ', f13.4, '; Hochwert: ', f13.4 // &
            & 1x, 'EXECUTION TERMINATED')

!1000   Input Errors in general
!****
 !rdkalypso
 1001 FORMAT (1x, 'ERROR - while reading file ', I5)
 !rdkalypso
 1002 FORMAT (1x, 'ERROR - node ', I5, 'shall not be zero or negative')

!1100  Errors with nodes
!****
 !rdkalypso
 1101 FORMAT (1x, 'ERROR - illegal midside node number. Node ', I5, ' is negative.')
 !getgeo1
 1102 FORMAT (1x, 'ERROR - SUSPICIOUSLY LARGE NODE NUMBER',I10, ' DETECTED'// &
            & 1x, "Please decrease the number of nodes! Model can't deal with" // &
            & 1x, 'the actual number of nodes')

!1200  Errors with elements
!****
 1201 FORMAT (1x, 'ERROR - SUSPICIOUSLY LARGE ELEMENT NUMBER',I10,' DETECTED'// &
            & 1x, "Please decrease the number of elements! Model can't deal with" // &
            & 1x, 'the actual number of elements')

!1300  Errors with arcs
!****
 !rdkalypso
 1301 FORMAT (1x, 'ERROR - illegal arc number. Arc ', I5, ' is negative')


!1400  Errors with continuity line
!****
 !rdkalypso
 1401 FORMAT (1X, 'ERROR - continuity line', I3, 'includes zero-entries!')
 !getbc
 1402 FORMAT (1x, 'ERROR - Error in dynamic boundary condition files')

!1500  Errors with connectivity
!****
 !rdkalypso
 1501 FORMAT (1x, 'ERROR - More than 60 nodes connected to node', I5 )

!2000   Output Errors
!3000   Mesh Errors
 !TransVelDistribution
 3601 FORMAT (1x, 'ERROR - Transition line, ', I4 ,' is too curved' / &
            & 1x, 'flow direction through line is not unique defined' / &
            & 1x, 'change model!' // &
            & 1x, 'execution of program terminated - STOP')
!4000   Calculation Errors
 !update
 4001 FORMAT (1x, 'ERROR - EXECUTION TERMINATED BY EXCESS CHANGES' / &
            & 1x, 'At node ', I6, ' changes are too much. Please decrease' / &
            & 1x, 'relaxation, change time step or boundary condition' / &
            & 1x, 'values in general')




CLOSE (75)
!stop program
STOP

end subroutine
