subroutine ErrorMessageAndStop (ErrorID, ObjectID, coorx, coory, objectName)

implicit none

integer (kind = 4), intent(in) :: ErrorID, ObjectID
character (len = *), intent (in), optional  :: objectName
integer                        :: istat
real (kind = 8), intent (in)   :: coorx, coory
integer                        :: errorunit

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

elseif (ErrorID == 1007) then
  WRITE (ErrorUnit, 1007)
  WRITE (        *, 1007)

elseif (ErrorID == 1008) then
  WRITE (ErrorUnit, 1008)
  WRITE (        *, 1008)

elseif (ErrorID == 1009) then
  WRITE (ErrorUnit, 1009)
  WRITE (        *, 1009)
  
elseif (ErrorID == 1010) then
  WRITE (ErrorUnit, 1010)
  WRITE (        *, 1010)
elseif (ErrorID == 1011) then
  WRITE (ErrorUnit, 1011)
  WRITE (        *, 1011)
elseif (ErrorID == 1012) then
  WRITE (ErrorUnit, 1012)
  WRITE (        *, 1012)
elseif (ErrorID == 1013) then
  WRITE (ErrorUnit, 1013)
  WRITE (        *, 1013)
elseif (ErrorID == 1014) then
  WRITE (ErrorUnit, 1014)
  WRITE (        *, 1014)


elseif (ErrorID == 1101) then
  WRITE (ErrorUnit, 1101) ObjectID
  WRITE (        *, 1101) ObjectID

elseif (ErrorID == 1102) then
  WRITE (ErrorUnit, 1102) ObjectID
  WRITE (        *, 1102) ObjectID

elseif (ErrorID == 1103) then
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

elseif (ErrorID == 1111) then
  WRITE (ErrorUnit, 1111) ObjectID
  WRITE (        *, 1111) ObjectID

elseif (ErrorID == 1112) then
  WRITE (ErrorUnit, 1112) ObjectID
  WRITE (        *, 1112) ObjectID

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

elseif (ErrorID == 1207) then
  WRITE (ErrorUnit, 1207) ObjectID
  WRITE (        *, 1207) ObjectID

elseif (ErrorID == 1208) then
  WRITE (ErrorUnit, 1208) ObjectID
  WRITE (        *, 1208) ObjectID

elseif (ErrorID == 1209) then
  WRITE (ErrorUnit, 1209) ObjectID
  WRITE (        *, 1209) ObjectID

elseif (ErrorID == 1210) then
  WRITE (ErrorUnit, 1210)
  WRITE (        *, 1210)

elseif (ErrorID == 1211) then
  WRITE (ErrorUnit, 1211) ObjectID
  WRITE (        *, 1211) ObjectID

elseif (ErrorID == 1301) then
  WRITE (ErrorUnit, 1301) ObjectID
  WRITE (        *, 1301) ObjectID

elseif (ErrorID == 1302) then
  WRITE (ErrorUnit, 1302) ObjectID
  WRITE (        *, 1302) ObjectID

elseif (ErrorID == 1401) then
  WRITE (ErrorUnit, 1401) ObjectID
  WRITE (        *, 1401) ObjectID

elseif (ErrorID == 1402) then
  WRITE (ErrorUnit, 1402) ObjectID
  WRITE (        *, 1402) ObjectID

elseif (ErrorID == 1403) then
  WRITE (ErrorUnit, 1403) ObjectID
  WRITE (        *, 1403) ObjectID

elseif (ErrorID == 1404) then
  WRITE (ErrorUnit, 1404) ObjectID
  WRITE (        *, 1404) ObjectID

elseif (ErrorID == 1405) then
  WRITE (ErrorUnit, 1405) ObjectID
  WRITE (        *, 1405) ObjectID

elseif (ErrorID == 1406) then
  WRITE (ErrorUnit, 1406)
  WRITE (        *, 1406)

elseif (ErrorID == 1407) then
  WRITE (ErrorUnit, 1407) ObjectID
  WRITE (        *, 1407) ObjectID

elseif (ErrorID == 1408) then
  WRITE (ErrorUnit, 1408)
  WRITE (        *, 1408)
  
elseif (ErrorID == 1409) then
  WRITE (ErrorUnit, 1409)
  WRITE (        *, 1409)

elseif (ErrorID == 1410) then
  WRITE (ErrorUnit, 1410)
  WRITE (        *, 1410)


elseif (ErrorID == 1501) then
  WRITE (ErrorUnit, 1501) ObjectID
  WRITE (        *, 1501) ObjectID

elseif (ErrorID == 1601) then
  WRITE (ErrorUnit, 1601) ObjectID
  WRITE (        *, 1601) ObjectID

elseif (ErrorID == 1602) then
  WRITE (ErrorUnit, 1602) ObjectID
  WRITE (        *, 1602) ObjectID

elseif (ErrorID == 1603) then
  WRITE (ErrorUnit, 1603) ObjectID
  WRITE (        *, 1603) ObjectID

elseif (ErrorID == 1604) then
  write (ErrorUnit, 1604)
  write (        *, 1604)

elseif (ErrorID == 1701) then
  WRITE (ErrorUnit, 1701) ObjectID
  WRITE (        *, 1701) ObjectID

elseif (ErrorID == 1702) then
  WRITE (ErrorUnit, 1702) ObjectID
  WRITE (        *, 1702) ObjectID

elseif (ErrorID == 1801) then
  WRITE (ErrorUnit, 1801) objectName
  WRITE (        *, 1801) objectName



ELSEIF (ErrorID == 3501) then
  WRITE (ErrorUnit, 3501)
  WRITE (        *, 3501)

elseif (ErrorID == 3601) then
  WRITE (ErrorUnit, 3601) ObjectID
  WRITE (        *, 3601) ObjectID

elseif (ErrorID == 4001) then
  WRITE (ErrorUnit, 4001) ObjectID
  WRITE (        *, 4001) ObjectID

elseif (ErrorID == 4002) then
  WRITE (ErrorUnit, 4002)
  WRITE (        *, 4002)

elseif (ErrorID == 4003) then
  WRITE (ErrorUnit, 4003)
  WRITE (        *, 4003)
  
elseif (ErrorID == 4004) then
  write (ErrorUnit, 4004)
  write (      *, 4004)

elseif (ErrorID == 4005) then
  write (ErrorUnit, 4005) ObjectID
  write (      *, 4005) ObjectID

elseif (ErrorID == 4101) then
  WRITE (ErrorUnit, 4101)
  WRITE (        *, 4101)

elseif (ErrorID == 4201) then
  WRITE (ErrorUnit, 4201) ObjectID
  WRITE (        *, 4201) ObjectID

elseif (ErrorID == 4202) then
  WRITE (ErrorUnit, 4202) ObjectID
  WRITE (        *, 4202) ObjectID

elseif (ErrorID == 4203) then
  WRITE (ErrorUnit, 4203) ObjectID
  WRITE (        *, 4203) ObjectID
  
elseif (ErrorID == 4204) then
  write (ErrorUnit, 4204)
  write (        *, 4204)

else
  WRITE (ErrorUnit, *) 'Error No.', ErrorID, ' not documented. Please contact programmer!'
  WRITE (        *, *) 'Error No.', ErrorID, ' not documented. Please contact programmer!'
end if


!Write coordinates or not
if (coorx <= 0.001 .AND. coory <= 0.001) then
  write (ErrorUnit, 998)
  write (        *, 998)
else
  WRITE (ErrorUnit, 999) coorx, coory
  WRITE (        *, 999) coorx, coory
end if
  write (ErrorUnit, 1000)
  write (        *, 1000)


!999    Position of Error
!***
  998 FORMAT (1x, 'No coordinates for ERROR available')
  999 FORMAT (1x, 'Error occurs at or near to position: ' / &
            & 1x, 'Rechtswert: ', f13.4, '; Hochwert: ', f13.4 ) 
 1000 format (1x, 'EXECUTION TERMINATED')

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
 !elflows
 1007 format (1x, 'ERROR - TOO MANY ELEMENT INFLOWS.' / &
            & 1x, 'Reduce the number of element inflows! Program will be stopped')
 !input
 1008 format (1x, 'ERROR - too many possible iterations for steady step defined. '/ &
            & 1x, 'maximum number is 90')
 1009 format (1x, 'ERROR - too many possible iterations for unsteady steps defined. '/ &
            & 1x, 'maximum number is 90')
 !file
 1010 format (1x, ' ERROR - additional input data line -CONTROL- is missing. '/ &
            & 1x, ' EXECUTION TERMINATED!')
 1011 format (1x, ' ERROR - additional input data line -CONTROL- could not be evaluated. '/ &
            & 1x, ' Please check the control file! - EXECUTION TERMINATED!')
 1012 format (1x, " ERROR - Can't open control file! - EXECUTION TERMINATED!")
 1013 format (1x, ' ERROR - Execution terminated after errors opening file(s) - EXECUTION TERMINATED!')
 1014 format (1x, ' ERROR - NO OUTPUT FILE DEFINED! - EXECUTION TERMINATED!')




!1100  Errors with nodes
!****
 !rdkalypso
 1101 FORMAT (1x, 'ERROR - illegal midside node number. Node ', I5, ' is negative.')
 !getgeo1
 1102 FORMAT (1x, 'ERROR - SUSPICIOUSLY LARGE NODE NUMBER',I10, ' DETECTED'/ &
            & 1x, "Please decrease the number of nodes! Model can't deal with" / &
            & 1x, 'the actual number of nodes.')
 !coef1, coef1nt
 1103 FORMAT (1x, 'ERROR - Width data is missing for a node in element', I5, '. Please '/ &
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
 !GenNewNode
 1111 format (1x, 'ERROR - If you want to interpolate between profiles, these profiles must have the'/&
            & 1x, ' same roughness slope, otherwise a reference Q calculation does not make sense!'/&
            & 1x, 'Error occured at element ', i5, ' Program stopped.')
 !rdkalypso
 1112 FORMAT (1x, 'ERROR - Width data is missing for a node', I5, '!')
            

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
 !incstrc
 1207 format (1x, 'ERROR - Table of control structure with the no', i5,/ &
            & 1x, ' is not sorted in the correct way. Check your',/ &
            & 1x, 'control structure data', / 'Execution terminated')
 1208 format (1x, 'ERROR - ', i5, ' is not a valid control structure type number', / &
            & 1x, 'execution terminated!' )
 !getgeo
 1209 format (1x, 'ERROR - Roughness data of element ', I5, ' are not valid. Check', / &
            & 1x, 'element! - EXECUTION TERMINATED!')
 1210 format (1x, 'ERROR - Weir data definition in control file not correct.', / &
            & 1x, 'EXECUTION TERMINATED!')
 1211 format (1x, 'ERROR - Transition line ', i5, ' has insufficient number of nodes (<= 1). '/ &
            & 1x, 'Change the definition of that line! - EXECUTION TERMINATED!' )
 1212 format (1x, 'ERROR - Bad defined element (no.', i5, '), because midside node is less or ', / &
            & 1x, 'equal to zero. EXECUTION TERMINATED!')
 


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
 1404 format (1x, 'ERROR - No CC1 block found for line, ', i5, '. Please check control file.')
 1405 format (1x, 'ERROR - First node of continuity line definition (No. ', i5, ') is 0. Please' / &
            & 1x, 'check control file!')
 1406 format (1x, 'ERROR - TOO MANY ELEVATION SPECS!')
 1407 format (1x, 'ERROR - Transition Line', i5, ' has specified value! There can be no ' / &
            &     'specified WATERSTAGES. Leave Transition line without conditions!')
 1408 format (1x, 'ERROR - TOO MANY DISCHARGE SPECS!')
 1409 format (1x, 'ERROR - Transition Line has specified value! Discharge specification is not allowed there!')
 1410 format (1x, 'ERROR - Q-input line not properly defined!')


 

!1500  Errors with connectivity
!****
 !rdkalypso
 1501 FORMAT (1x, 'ERROR - More than 60 nodes connected to node', I5 )

!1600  Errors during restart
 !rdkalypso
 1601 FORMAT (1x, "ERROR - Restart values can't be applied to node,", i5, ',' / &
            & 1x, 'because it is less or equal zero or it is higher than the' / &
            & 1x, 'maximum node number')
 1602 format (1x, 'ERROR - TRYING TO APPLY RESTART RESULT FOR INTERPOLATED NODE' / &
            & 1x, 'TO A REAL PROFILE/ NODE. THERE MUST BE A MODEL FILE ERROR' / &
            & 1x, 'PLEASE CHECK YOUR MESH, THE CURRENT ERROR OCCURS AT NODE', I5 / &
            & 1x, 'EXECUTION TERMINATED!')
 1603 FORMAT (1x, "ERROR - Restart values for lambda can't be applied to element,", i5, ',' / &
            & 1x, 'because it is less or equal zero or it is higher than the' / &
            & 1x, 'maximum element number')
 1604 format (1x, 'You are about to restart from a later time step, but you ordered to start with steady', / &
            & 1x, 'state calculation. Please change that in C5!', / &
            & 1x, 'EXYECUTION TERMINATED!')
            
!1700   Model definition errors
!****
 !volume water level realtionship of storage elements
 1701 format (1x, 'ERROR - Storage element ', i5, ' already contains a Volume Waterlevel',/ &
            & 1x, 'relationship. Please check the input files!')
 
 1702 format (1x, 'ERROR - Storage element ', i5, ' does not have a Volume Waterlevel',/ &
            & 1x, 'relationship. Please check the input files!')
            
            
!1800   Time Step defintiion errors
!****
 1801 format (1x, 'ERROR - Unable to find line: ', a)


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
 4001 FORMAT (1x, 'ERROR - Execution terminated by excess changes' / &
            & 1x, 'at node ', I6, '. Please increase' / &
            & 1x, 'URFC, change time step or boundary condition' / &
            & 1x, 'values in general')
 !stfltab: stage flow boundaries are problematic
 4002 format (1x, 'ERROR - SURFACE ELEVATION BELOW LOWEST TABLE ENTRY' / &
!            & 1x, 'ILINTB,SRFEL,ELEVTBL(ILINTB,1)', ILINTB, SRFEL
!     +                 ,ELEVTBL(ILINTB,1)
            & 1x, 'EXECUTION TERMINATED')
 4003 format (1x, 'ERROR - WATER SURFACE ELEVATION ABOVE HIGHEST TABLE ENTRY' / &
!      WRITE(75,*)'ILINTB,SRFEL,ELEVTBL(ILINTB,1)',ILINTB,SRFEL
!     +                 ,ELEVTBL(ILINTB,1)
            & 1x, 'EXECUTION TERMINATED')
 4004 format (1x, 'ERROR - defined wwQ-relationship of control structure not big enough.' / &
            & 1x, 'EXECUTION TERMINATED')
 4005 format (1x, 'ERROR - defined polynomial of node ', I6, ' not big enough.' / &
            & 1x, 'EXECUTION TERMINATED')

!4100   Autoconverge Errors
 4101 format (1x, 'ERROR - Autoconverge not successful')
 
!4200 Errors during execution of Pardiso

 4201 format (1x, 'ERROR - PARDISO could not release memory; Error-code: ', I5, /&
            & 1x, 'EXECUTION TERMINATED')
 4202 format (1x, 'ERROR - PARDISO could not Analyse equation system (Phase 1); Error-code: ', I5, /&
            & 1x, 'EXECUTION TERMINATED')
 4203 format (1x, 'ERROR - PARDISO could not factorize or solve equation system (Phase 2 or 3); ',/&
            & 1x, 'Error-code: ', I5, /&
            & 1x, 'EXECUTION TERMINATED')
 4204 format (1x, 'ERROR - Sorting of matrix to prepare for PARDISO did not succeed; This is ', /&
            & 1x, 'a serious problem. Please save model and contact developers!', /&
            & 1x, 'EXECUTION TERMINATED')

CLOSE (75)
call flush (75)
!stop program
STOP 'EXECUTION OF RMA-Kalypso TERMINATED'

end subroutine