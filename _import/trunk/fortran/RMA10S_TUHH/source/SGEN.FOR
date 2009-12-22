cipk  last update feb 09 2001 allow for zero length cc lines

      SUBROUTINE SGEN(J,A1)
      USE BLK10MOD
      SAVE
C-
C...... Generate specified quality boundary conditions
C-
      DIMENSION A1(3)
C-
C...... Insert values into SPEC and NFIX arrays
C-
      IF(J.LT.1) RETURN
      MAX=LMT(J)
CIPK FEB01
	IF(MAX .EQ. 0) THEN
	  WRITE(75,*) 'ATTEMPT TO SET QUALITY FOR NON-EXISTENT LINE',J
	  WRITE(75,*) 'EXECUTION TERMINATED'
	  WRITE(*,*) 'ATTEMPT TO SET QUALITY FOR NON-EXISTENT LINE',J
	  WRITE(*,*) 'EXECUTION TERMINATED'
	ENDIF
      DO 300 K=1,MAX
        NA=LINE(J,K)
        SPEC(NA,4)=A1(1)
        IF(MOD(NFIX(NA),100)/10 .LT. 2) THEN
          NFIX(NA)=(NFIX(NA)/100)*100+20
        ENDIF
        SPEC(NA,5)=A1(2)
        IF(MOD(NFIX(NA),10) .LT. 2) THEN
          NFIX(NA)=(NFIX(NA)/10)*10+2
        ENDIF
        SPEC(NA,6)=A1(3)
        IF(NFIX1(NA) .LT. 2) THEN
          NFIX1(NA)=2
        ENDIF
  300 CONTINUE
      RETURN
      END
