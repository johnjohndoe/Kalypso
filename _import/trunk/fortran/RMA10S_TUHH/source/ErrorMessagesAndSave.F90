!     Last change:  WP   28 Aug 2007    3:02 pm
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
 1001 FORMAT (1x, 'ERROR - while reading file ', i5)
 1002 FORMAT (1x, 'ERROR - node ', i5, 'shall not be zero or negative')
!1100  Errors with nodes
!****
 1101 FORMAT (1x, 'ERROR - illegal midside node number. Node ', i5, ' is negative.')

!       1200  Errors with elements
!       1300  Errors with arcs
 1301 FORMAT (1x, 'ERROR - illegal arc number. Arc ', i5, ' is negative')


!1400  Errors with continuity line
!****
 1401 FORMAT (1X, 'ERROR -  continuity line', I3, 'includes zero-entries!')

!1500  Errors with connectivity
!****
 1501 FORMAT (1x, 'ERROR - More than 60 nodes connected to node', i5 )

!2000   Output Errors
!3000   Mesh Errors
!4000   Calculation Errors


CLOSE (75)
!stop program
STOP

end subroutine
