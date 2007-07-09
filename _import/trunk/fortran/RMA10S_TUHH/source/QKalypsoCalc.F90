!     Last change:  K     4 Jul 2007    7:01 pm
!purpose of the subroutine is to calculate the average water level along a CCL.

subroutine getLineAverageWaterLevel(CCL, waspi)

USE BLK10MOD
USE PARAKALYPS
USE BLKDRMOD

implicit none


!number of continuityline
INTEGER, INTENT (IN)  :: CCL
!waterlevel
real (KIND=8), INTENT (OUT) :: waspi

!temp linesegment number; number of drown nodes
INTEGER               :: na, lump
!counters
INTEGER               :: k
!geometric waterdepth, Marsh-waterdepth
REAL (KIND=8)         :: fliesstiefe, d3
real (KIND=4)         :: d2
!new variables
REAL                  :: amec (350)

!nis,jun07: Adding some initializations
waspi = 0

!Counter of dry nodes
lump = lmt (CCL)

!CCL-Fliesstiefen control output
WRITE(999,*) 'Fliesstiefenkontrolle an der Linie',CCL
WRITE(999,*) 'Anzahl der Knoten ist', lmt(CCL)
WRITE(999,*)
WRITE(999,'(a7,2a11)') '   node', '    calcFt.', '   marshFt.'

!Process every node of CCL
DO k = 1, lmt (CCL)
  !Initialize temporary flow depth for node k
  fliesstiefe = 0.0

  !Get node k
  na = line (CCL, k)

  !Test whether node exists, if so get the flow depth
  IF (na > 0) fliesstiefe = vel (3, na)

  if (idnopt == 0) then
  !nis,sep06,com: Very small flow depth
    IF (fliesstiefe <= 0.001) then
      !nis,sep06,com: Decrease the number of wetted nodes (lump) and overgive estimated 0.0 depth to the temporary depth d3
      lump = lump - 1
      d3 = 0.0
    !nis,sep06,com: If flow depth is not small
    ELSE
!nis,jun07: trying different formulation with marsh-approach
!    !nis,sep06,com: Calculate the true depth over node k
!!nis,sep06: Zeigmarsh is not working in RMA10S yet
!!   CALL amf (d3, fliesstiefe, akp (na), adt (na), adb (na),amec (k), d2, 0, zeigmarsh)
!    CALL amf (d3, fliesstiefe, akp (na), adt (na), adb (na), amec (k), d2, 0)
!!-
!    !nis,sep06,com: Sum the waterlevel elevation, that is the flow depth plus bottom elevation
!    d3 = d3 + ao (na)
!    !nis,sep06,controloutput: CCL-Fließtiefen
!     !nis,jan07,testing
!     !WRITE(999,'(1x,i6,2(1x,f10.7))') na, fliesstiefe, (d3-ao(na))
!     WRITE(999,'(1x,i6,4(1x,f10.7))') na, fliesstiefe, (d3-ao(na)), d3, ao(na)
!     !-
!    !-
!-
      d3 = ao(na) + vel(3,na)
    ENDIF
    !nis,sep06,com: Summed waterlevel elevation over all not drown nodes
    waspi = waspi + d3
  ELSE
    CALL amf (d3, fliesstiefe, akp (na), adt (na), adb (na), amec (k), d2, 0)
    waspi = waspi + d3 + ado(na)
  ENDIF

END DO


!Test for the whole CCL, whether it is dry
IF (lump.eq.0) then
  write ( *  , 9000) CCL
  write (Lout, 9000) CCL
 9000       format (/1X, 'Error: Inflow line No. ', I3,' is dry!', / 'Stopping program!')
  stop
END if

!Calculate average flow depth over all not drown elements
waspi = waspi / lump
WRITE (Lout, *) 'average marsh-waterlevel at inflow line', CCL, ' is: ', waspi

end subroutine
