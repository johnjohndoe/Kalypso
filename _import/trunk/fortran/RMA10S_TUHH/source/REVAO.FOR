CIPK  NEW ROUTINE FEB 27 2004

      SUBROUTINE REVAO
      USE BLK10MOD
      USE BLKDRMOD
	
!NiS,jul06: Consistent data type for passing parameters
      REAL(KIND=8) :: VTEMP
!-

	DATA ITIME/0/

	IF(ITIME .EQ. 0) THEN

cipk sep04 skip lowest point if rederence is defined
       IF(SADEL .EQ. 0.) THEN    
C     Find lowest bed point for reference

        AOMIN=1.E+10
        DO M=1,NE
	    IF(IMAT(M) .NE. 0) THEN
	      DO J=1,NCORN(M)
	        N=NOP(M,J)
	        AOMIN=MIN(AOMIN,AO(N))
	        ADOKP(N)=ADO(N)
	        AOKP(N)=AO(N)
	      ENDDO
	    ENDIF
	  ENDDO
	 ELSE
	  AOMIN=SADEL
	  DO N=1,NP
	    ADOKP(N)=ADO(N)
	    AOKP(N)=AO(N)
	  ENDDO 
	 ENDIF
	ENDIF

	DO J=1,NP
	  AO(J)=AOMIN+(AOKP(J)-AOMIN)*SADX
	  ADO(J)=AO(J)+ADOKP(J)-AOKP(J)
        IF(ITIME .EQ. 0) THEN
          IF(NB .NE. 0) THEN
	      HEL(J)=WSLL(J)-ADO(J)
	    ELSE
            HEL(J)=ELEV1-ADO(J)
	    ENDIF
          CALL AMF(HEL(J),VTEMP,AKP(J),ADT(J),ADB(J),D1,D2,1)
	    VEL(3,J)=VTEMP
          IF(VEL(3,J) .LT. HMIN) VEL(3,J)=HMIN
          VOLD(3,J)=VEL(3,J)
          HOL(J)=HEL(J)
	    IF(J .EQ. NP) ITIME=1
        ENDIF
      ENDDO
      RETURN

	END
