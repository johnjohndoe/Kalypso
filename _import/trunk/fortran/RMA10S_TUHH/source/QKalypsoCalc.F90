!     Last change:  WP    1 Jul 2008    6:05 pm
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
real (KIND=8)         :: d2
!new variables
REAL                  :: amec (3535)

!nis,jun07: Adding some initializations
waspi = 0.0

!Counter of dry nodes
lump = lmt (CCL)
if (lump == 0) return

!Process every node of CCL
AddNodeWSLL: DO k = 1, lmt (CCL)

  IF (mod (k,2) == 0) then
    lump = lump - 1
    CYCLE AddNodeWSLL

  ELSE
    !Initialize temporary flow depth for node k
    fliesstiefe = 0.0

    !Get node k
    na = line (CCL, k)

    !Test whether node exists, if so get the flow depth
    IF (na > 0) fliesstiefe = vel (3, na)


    IF (fliesstiefe <= 0.001) THEN
      !nis,sep06,com: Decrease the number of wetted nodes (lump) and overgive estimated 0.0 depth to the temporary depth d3
      lump = lump - 1
      d3 = 0.0

    !nis,sep06,com: If flow depth is not small
    ELSE
      IF (idnopt == 0) THEN
        !nis,sep06,com: Very small flow depth
        d3 = ao(na) + vel(3,na)
        !nis,sep06,com: Summed waterlevel elevation over all not drown nodes
        waspi = waspi + d3
      ELSE
        CALL amf (d3, fliesstiefe, akp (na), adt (na), adb (na), amec (k), d2, 0)
        waspi = waspi + d3 + ado(na)
      ENDIF
    ENDIF
  ENDIF

ENDDO AddNodeWSLL

!Test for the whole CCL, whether it is dry
IF (lump == 0) then
  write ( *  , 9000) CCL
  write (Lout, 9000) CCL
 9000       format (/1X, 'Error: Inflow line No. ', I3,' is dry!', / 'Stopping program!')
  stop
END if

!Calculate average flow depth over all not drown elements
waspi = waspi / lump
WRITE (Lout, *) 'average marsh-waterlevel at inflow line', CCL, ' is: ', waspi
WRITE (   *, *) 'average marsh-waterlevel at inflow line', CCL, ' is: ', waspi

end subroutine