!ipk  last update feb 09 2001 allow for zero length cc lines
!
      SUBROUTINE SGEN(J,A1)
      USE BLK10MOD
      SAVE
!-
!...... Generate specified quality boundary conditions
!-
      DIMENSION A1(3)
!-
!...... Insert values into SPEC and NFIX arrays
!-
      IF(J < 1) RETURN
      MAX=LMT(J)
!IPK FEB01
      IF(MAX == 0) THEN
        WRITE(75,*) 'ATTEMPT TO SET QUALITY FOR NON-EXISTENT LINE',J
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET QUALITY FOR NON-EXISTENT LINE',J
        WRITE(*,*) 'EXECUTION TERMINATED'
      ENDIF
      DO 300 K=1,MAX
        NA=LINE(J,K)
        SPEC(NA,4)=A1(1)
        IF(MOD(NFIX(NA),100)/10 < 2) THEN
          NFIX(NA)=(NFIX(NA)/100)*100+20
        ENDIF
        SPEC(NA,5)=A1(2)
        IF(MOD(NFIX(NA),10) < 2) THEN
          NFIX(NA)=(NFIX(NA)/10)*10+2
        ENDIF
        SPEC(NA,6)=A1(3)
        IF(NFIX1(NA) < 2) THEN
          NFIX1(NA)=2
        ENDIF
  300 CONTINUE
      RETURN
      END
