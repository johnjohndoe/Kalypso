!     Last change:  JAJ   7 Jan 2007    4:53 pm
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
!nis,sep06: new variables
REAL                  :: amec (350)

!nis,sep06,com: Initializing the counter for not drown nodes of CCL
lump = lmt (CCL)

!nis,sep06,controloutput: CCL-Flieﬂtiefen
WRITE(999,*) 'Fliesstiefenkontrolle an der Linie',CCL
WRITE(999,*) 'Anzahl der Knoten ist', lmt(CCL)
WRITE(999,*)
WRITE(999,'(a7,2a11)') '   node', '    calcFt.', '   marshFt.'
!-


!nis,sep06,com: Objective for every node of CCL
DO k = 1, lmt (CCL)
  !nis,sep06,com: Initialize temporyry flow depth for node k
  fliesstiefe = 0.0
  !nis,sep06,com: Get node k
  na = line (CCL, k)
  !nis,sep06,com: Test whether node exists, if so get the flow depth
  IF (na.gt.0) fliesstiefe = vel (3, na)
  !nis,sep06,com: Very small flow depth
  IF (fliesstiefe.le.0.001) then
    !nis,sep06,com: Decrease the number of wetted nodes (lump) and overgive estimated 0.0 depth to the temporary depth d3
    lump = lump - 1
    d3 = 0.0
  !nis,sep06,com: If flow depth is not small
  ELSE
    !nis,sep06,com: Calculate the true depth over node k
!nis,sep06: Zeigmarsh is not working in RMA10S yet
!   CALL amf (d3, fliesstiefe, akp (na), adt (na), adb (na),amec (k), d2, 0, zeigmarsh)
    CALL amf (d3, fliesstiefe, akp (na), adt (na), adb (na), amec (k), d2, 0)
!-
    !nis,sep06,com: Sum the waterlevel elevation, that is the flow depth plus bottom elevation
    d3 = d3 + ao (na)
    !nis,sep06,controloutput: CCL-Flieﬂtiefen
     !nis,jan07,testing
     WRITE(999,'(1x,i6,2(1x,f10.7))')na,fliesstiefe,(d3-ao(na))
     !WRITE(999,'(1x,i6,4(1x,f10.7))')na,fliesstiefe,(d3-ao(na)),d3, ao(na)
     !-
    !-
  ENDIF
  !nis,sep06,com: Summed waterlevel elevation over all not drown nodes
  waspi = waspi + d3
END DO


!nis,sep06,com: Test for the whole CCL, whether it is dry
IF (lump.eq.0) then
  write ( *  , 9000) CCL
  write (Lout, 9000) CCL
 9000       format (/1X, 'Error: Inflow line No. ', I3,' is dry!', / 'Stopping program!')
  stop
END if

!nis,sep06,com: calculate average flow depth over all not drown elements
waspi = waspi / lump
WRITE (Lout, *) 'average marsh-waterlevel at inflow line', CCL, ' is: ', waspi

end subroutine
