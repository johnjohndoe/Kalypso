!     Last change:  WP   28 Nov 2007   11:34 am
subroutine ErrorMessageAndStop (ErrorID, ObjectID, coorx, coory)

implicit none

INTEGER, INTENT(IN)          :: ErrorID, ObjectID
INTEGER                      :: istat
REAL (KIND = 8), INTENT (IN) :: coorx, coory
INTEGER                      :: ErrorUnit

!This subroutine generates an Error function and writes it to the ERROR.OUT file. Afterwards it stops the program.
!This is good for bringing together all the Errors in a list.

ErrorUnit = 75


CLOSE (75)
istat = 0
OPEN  (unit = ErrorUnit, FILE = 'ERROR.OUT', iostat = istat)
if (istat /= 0) STOP 'error with ERRORFile'

if (ErrorID == 1001) then
  WRITE (ErrorUnit, 1001) ObjectID
  WRITE (        *, 1001) ObjectID

elseif (ErrorID == 1002) then
  WRITE (ErrorUnit, 1002) ObjectID
  WRITE (        *, 1002) ObjectID

elseif (ErrorID == 1003) then
  WRITE (ErrorUnit, 1003) ObjectID
  WRITE (        *, 1003) ObjectID

elseif (ErrorID == 1004) then
  WRITE (ErrorUnit, 1004) ObjectID
  WRITE (        *, 1004) ObjectID

elseif (ErrorID == 1005) then
  WRITE (ErrorUnit, 1005) ObjectID
  WRITE (        *, 1005) ObjectID

elseif (ErrorID == 1006) then
  WRITE (ErrorUnit, 1006) ObjectID
  WRITE (        *, 1006) ObjectID

elseif (ErrorID == 1101) then
  WRITE (ErrorUnit, 1101) ObjectID
  WRITE (        *, 1101) ObjectID

elseif (ErrorID == 1102) then
  WRITE (ErrorUnit, 1102) ObjectID
  WRITE (        *, 1102) ObjectID

elseif (ErrorID == 1102) then
  WRITE (ErrorUnit, 1103) ObjectID
  WRITE (        *, 1103) ObjectID

elseif (ErrorID == 1104) then
  WRITE (ErrorUnit, 1104) ObjectID
  WRITE (        *, 1104) ObjectID

elseif (ErrorID == 1105) then
  WRITE (ErrorUnit, 1105) ObjectID
  WRITE (        *, 1105) ObjectID

elseif (ErrorID == 1106) then
  WRITE (ErrorUnit, 1104) ObjectID
  WRITE (        *, 1104) ObjectID
  WRITE (ErrorUnit, 1105) ObjectID
  WRITE (        *, 1105) ObjectID

elseif (ErrorID == 1107) then
  WRITE (ErrorUnit, 1107) ObjectID
  WRITE (        *, 1107) ObjectID

elseif (ErrorID == 1108) then
  WRITE (ErrorUnit, 1108) ObjectID
  WRITE (        *, 1108) ObjectID

elseif (ErrorID == 1109) then
  WRITE (ErrorUnit, 1109) ObjectID
  WRITE (        *, 1109) ObjectID

elseif (ErrorID == 1110) then
  WRITE (ErrorUnit, 1110) ObjectID
  WRITE (        *, 1110) ObjectID

elseif (ErrorID == 1201) then
  WRITE (ErrorUnit, 1201) ObjectID
  WRITE (        *, 1201) ObjectID

elseif (ErrorID == 1202) then
  WRITE (ErrorUnit, 1202) ObjectID
  WRITE (        *, 1202) ObjectID

elseif (ErrorID == 1203) then
  WRITE (ErrorUnit, 1203) ObjectID
  WRITE (        *, 1203) ObjectID

elseif (ErrorID == 1204) then
  WRITE (ErrorUnit, 1204) ObjectID
  WRITE (        *, 1204) ObjectID

elseif (ErrorID == 1205) then
  WRITE (ErrorUnit, 1205) ObjectID
  WRITE (        *, 1205) ObjectID

elseif (ErrorID == 1206) then
  WRITE (ErrorUnit, 1206)
  WRITE (        *, 1206)

elseif (ErrorID == 1301) then
  WRITE (ErrorUnit, 1301) ObjectID
  WRITE (        *, 1301) ObjectID

elseif (ErrorID == 1401) then
  WRITE (ErrorUnit, 1401) ObjectID
  WRITE (        *, 1401) ObjectID

elseif (ErrorID == 1402) then
  WRITE (ErrorUnit, 1402) ObjectID
  WRITE (        *, 1402) ObjectID

elseif (ErrorID == 1403) then
  WRITE (ErrorUnit, 1403) ObjectID
  WRITE (        *, 1403) ObjectID

elseif (ErrorID == 1501) then
  WRITE (ErrorUnit, 1501) ObjectID
  WRITE (        *, 1501) ObjectID

elseif (ErrorID == 1601) then
  WRITE (ErrorUnit, 1601) ObjectID
  WRITE (        *, 1601) ObjectID

ELSEIF (ErrorID == 3501) then
  WRITE (ErrorUnit, 3501)
  WRITE (        *, 3501)

elseif (ErrorID == 3601) then
  WRITE (ErrorUnit, 3601) ObjectID
  WRITE (        *, 3601) ObjectID

elseif (ErrorID == 4001) then
  WRITE (ErrorUnit, 4001) ObjectID
  WRITE (        *, 4001) ObjectID

elseif (ErrorID == 4101) then
  WRITE (ErrorUnit, 4101)
  WRITE (        *, 4101)
end if


!Write coordinates or not
if (coorx <= 0.001 .and. coory <= 0.001) then
  write (ErrorUnit, 998)
else
  WRITE (ErrorUnit, 999) coorx, coory
  WRITE (        *, 999) coorx, coory
end if


!999    Position of Error
!***
  998 FORMAT (1x, 'No coordinates of ERROR available')
  999 FORMAT (1x, 'Error occurs at or near to position: ' / &
            & 1x, 'Rechtswert: ', f13.4, '; Hochwert: ', f13.4 / &
            & 1x, 'EXECUTION TERMINATED')

!1000   Input Errors in general
!****
 !rdkalypso
 1001 FORMAT (1x, 'ERROR - while reading file ', I5)
 1002 FORMAT (1x, 'ERROR - node ', I5, 'shall not be zero or negative')
 1003 FORMAT (1x, 'ERROR - starting node for weir element definition is missing' / &
            & 1x, 'Please check the weir', I5, '.')
 1004 FORMAT (1x, 'ERROR - element ', I5, 'shall not be zero or negative')
 1005 FORMAT (1x, 'ERROR - junction element ', I5, 'shall not be zero or negative')
 1006 FORMAT (1x, 'ERROR - KSWIT has the value ', i5, ', but it can only have the' / &
            & 1x, 'values 0, 1, 2. This must be an implementation error.')

!1100  Errors with nodes
!****
 !rdkalypso
 1101 FORMAT (1x, 'ERROR - illegal midside node number. Node ', I5, ' is negative.')
 !getgeo1
 1102 FORMAT (1x, 'ERROR - SUSPICIOUSLY LARGE NODE NUMBER',I10, ' DETECTED'/ &
            & 1x, "Please decrease the number of nodes! Model can't deal with" / &
            & 1x, 'the actual number of nodes.')
 !coef1, coef1nt
 1103 FORMAT (1x, 'ERROR - Width is missing for a node in element', I5, '. Please '/ &
            & 1x, 'assign width data to both of the nodes!')
 !coef1DPoly
 1104 format (1x, 'ERROR - cross sectional area polynomial is missing for node ', I5 / &
            & 1x, 'Please assign polynomial data for that node!')
 1105 format (1x, 'ERROR - discharge polynomial is missing for node ', I5 / &
            & 1x, 'Please assign polynomial data for that node!')
 !cstrc, cstrc2d
 1107 format (1x, 'ERROR - undefined levee data for node', I5)
 !rdkalypso
 1108 format (1x, 'ERROR - The node ', i5, ' is defined in an arc but not in the node list.' / &
            & 1x, "Either you can define the node's coordinates and the occurance as an arc's" / &
            & 1x, 'midside node or you leave both open, because RMA10S is doing it.')
 1109 format (1x, 'ERROR - Midside node number', i5, ' is higher than maximum node number defined.' / &
            & 1x, "That doesn't work. Please change numbering!")
 1110 format (1x, 'ERROR - There are too many elements connected to node ', i5, '. Change mesh!')

!1200  Errors with elements
!****

 1201 FORMAT (1x, 'ERROR - SUSPICIOUSLY LARGE ELEMENT NUMBER',I10,' DETECTED'/ &
            & 1x, "Please decrease the number of elements! Model can't deal with" / &
            & 1x, 'the actual number of elements')
 !rdkalypso
 1202 FORMAT (1x, 'ERROR - Maximum arc number for 2D elements is 4. Element', / &
            & 1x, i5, 'has more than 4 arcs. Change geometry!')
 1203 format (1x, 'ERROR - Minimum arc number for 2D elements is 3. Element', / &
            & 1x, i5, 'has less than 3 arcs. Change geometry!')
 1204 format (1x, 'ERROR - Element ', i5, " doesn't form a linear ring. Please "/ &
            & 1x, 'check the geometry!')
 1205 format (1x, 'ERROR - Element ', i5, ' is twisted. Rearrange the element!')
 1206 format (1x, 'ERROR - Too many transition elements', / 'Execution terminated!')

!1300  Errors with arcs
!****
 !rdkalypso
 1301 FORMAT (1x, 'ERROR - Illegal arc number. Arc ', I5, ' is negative')
 1302 format (1x, 'ERROR - Element', i5, ' is defined twice. That is not possible.' / &
            & 1x, 'Rearrange mesh!')


!1400  Errors with continuity line
!****
 !rdkalypso
 1401 FORMAT (1X, 'ERROR - continuity line', I3, 'includes zero-entries!')
 !getbc
 1402 FORMAT (1x, 'ERROR - Error in dynamic boundary condition files')
 !rdkalypso
 1403 format (1x, 'ERROR - Transitioning node is not defined in 1D-element' / &
            & 1x, 'Problem occured in line ', i5, '. Compare the defined transition' / &
            & 1x, 'node in TL-line with the two corner nodes of the transition 1D-element.')

!1500  Errors with connectivity
!****
 !rdkalypso
 1501 FORMAT (1x, 'ERROR - More than 60 nodes connected to node', I5 )

!1600  Errors during restart
 !rdkalypso
 1601 FORMAT (1x, "ERROR - Restart values can't be applied to node,", i5, ',' / &
            & 1x, 'because it is less or equal zero or it is higher than the' / &
            & 1x, 'maximum node number')

!2000   Output Errors
!3000   Mesh Errors
 !3500  Reordering problems
 !reord_Kalyps
 3501 format (1x, 'ERROR - Reordering could not be fullfilled.')
 !3600  TransVelDistribution
 3601 FORMAT (1x, 'ERROR - Transition line, ', I4 ,' is too curved' / &
            & 1x, 'flow direction through line is not unique defined' / &
            & 1x, 'change model!' / &
            & 1x, 'execution of program terminated - STOP')
!4000   Calculation Errors
 !update
 4001 FORMAT (1x, 'ERROR - Execution termintated by excess changes' / &
            & 1x, 'at node ', I6, '. Please increase' / &
            & 1x, 'URFC, change time step or boundary condition' / &
            & 1x, 'values in general')

!4100   Autoconverge Errors
 4101 format (1x, 'ERROR - Autoconverge not successful')




CLOSE (75)
!stop program
STOP

end subroutine
