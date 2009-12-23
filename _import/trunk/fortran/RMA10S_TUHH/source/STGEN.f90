!     Last change:  JAJ  26 Aug 2007    8:32 pm
!ipk  last update feb 09 2001 allow for zero length cc lines
!
      SUBROUTINE STGEN(J,A1,A2,A3,A4,A5,QQAL)
      USE BLK10MOD
      SAVE
!-
!...... Generate specified stage flow boundary conditions
!-
      DIMENSION QQAL(3)
!-
!...... Save pointer to define which nodes are stage flow
!-
      MAXL = LMT(J)
!IPK FEB01
      IF(MAXL == 0) THEN
        WRITE(75,*) 'ATTEMPT TO SET STAGE-FLOW FOR NON-EXISTENT LINE',J
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET STAGE-FLOW FOR NON-EXISTENT LINE',J
        WRITE(*,*) 'EXECUTION TERMINATED'
      ENDIF
      DO 300 K=1,MAXL
        NA=LINE(J,K)
        ISTLIN(NA)=J
  300 CONTINUE
!-
!...... Save constants for later use
!-
      STQ(J)=A1
      STQA(J)=A2
      STQE(J)=A3
      STQC(J)=A4
      STQT(J)=A5
!-
!...... Define a nominal Q and generate B-C's
!-
!EFa jul07, changes with respect to RMA2      
!QREQ=A1+A2*(VEL(3,NA)+AO(NA)-A3)**A4      
      QREQ=A1
      IF(A1 == 0.) QREQ=1.
!IF(MAXL == 1) QREQ=A1      
!-      
      CALL QGEN(J,QREQ,A5,QQAL)
      RETURN
      END
