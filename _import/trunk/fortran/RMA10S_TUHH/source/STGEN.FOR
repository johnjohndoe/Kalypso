cipk  last update feb 09 2001 allow for zero length cc lines

      SUBROUTINE STGEN(J,A1,A2,A3,A4,A5,QQAL)
      USE BLK10MOD
      SAVE
C-
C...... Generate specified stage flow boundary conditions
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
      DIMENSION QQAL(3)
C-
C...... Save pointer to define which nodes are stage flow
C-
      MAX=LMT(J)
CIPK FEB01
	IF(MAX .EQ. 0) THEN
	  WRITE(75,*) 'ATTEMPT TO SET STAGE-FLOW FOR NON-EXISTENT LINE',J
	  WRITE(75,*) 'EXECUTION TERMINATED'
	  WRITE(*,*) 'ATTEMPT TO SET STAGE-FLOW FOR NON-EXISTENT LINE',J
	  WRITE(*,*) 'EXECUTION TERMINATED'
	ENDIF
      DO 300 K=1,MAX
        NA=LINE(J,K)
        ISTLIN(NA)=J
  300 CONTINUE
C-
C...... Save constants for later use
C-
      STQ(J)=A1
      STQA(J)=A2
      STQE(J)=A3
      STQC(J)=A4
      STQT(J)=A5
C-
C...... Define a nominal Q and generate B-C's
C-
      QREQ=A1+A2*(VEL(3,NA)+AO(NA)-A3)**A4
      IF(MAX .EQ. 1) QREQ=A1
      CALL QGEN(J,QREQ,A5,QQAL)
      RETURN
      END
