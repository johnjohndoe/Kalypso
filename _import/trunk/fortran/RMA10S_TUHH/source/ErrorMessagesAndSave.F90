!     Last change:  WP    3 Sep 2007    3:59 pm
subroutine ErrorMessageAndStop (ErrorID, Object)

INTEGER, INTENT(IN) :: ErrorID, Object
INTEGER             :: ErrorUnit

!This subroutine generates an Error function and writes it to the ERROR.OUT file. Afterwards it stops the program.
!This is good for bringing together all the Errors in a list.

ErrorUnit = 75

CLOSE (75)
OPEN  (ErrorUnit, FILE = 'ERROR.OUT')

WRITE (ErrorUnit, ErrorID) Object
WRITE (        *, ErrorID) Object


!1000   Input Errors in general
!****
 1001 FORMAT (1x, 'ERROR - while reading file ', I5)
 1002 FORMAT (1x, 'ERROR - node ', I5, 'shall not be zero or negative')
!1100  Errors with nodes
!****
 1101 FORMAT (1x, 'ERROR - illegal midside node number. Node ', I5, ' is negative.')

!       1200  Errors with elements
!       1300  Errors with arcs
 1301 FORMAT (1x, 'ERROR - illegal arc number. Arc ', I5, ' is negative')


!1400  Errors with continuity line
!****
 1401 FORMAT (1X, 'ERROR -  continuity line', I3, 'includes zero-entries!')

!1500  Errors with connectivity
!****
 1501 FORMAT (1x, 'ERROR - More than 60 nodes connected to node', I5 )

!2000   Output Errors
!3000   Mesh Errors
 3601 FORMAT (1x, 'ERROR - Transition line, ', I4 ,' is too curved' // &
            & 1x, 'flow direction through line is not unique defined' // &
            & 1x, 'change model!' // &
            & 1x, 'execution of program terminated - STOP')
!4000   Calculation Errors


CLOSE (75)
!stop program
STOP

end subroutine
