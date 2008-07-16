cipk  last update feb 09 2001 allow for zero length cc lines
C     Last change:  IPK   5 Oct 98    1:49 pm

      SUBROUTINE QGEN(J,QREQ,THET,QQAL)
      USE BLK10MOD
      USE BLK11MOD
      SAVE
C-
C-..... Generate specified total flow boundary conditions
C-
      DIMENSION QQAL(3)
cipk aug05      INCLUDE 'BLK10.COM'
C-
      DATA VOID/-1.E20/
C-
C...... Calculate total projected area
C-
      MAX=LMT(J)
CIPK FEB01
	IF(MAX .EQ. 0) THEN
	  WRITE(75,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
	  WRITE(75,*) 'EXECUTION TERMINATED'
	  WRITE(*,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
	  WRITE(*,*) 'EXECUTION TERMINATED'
	ENDIF
      STQT(J)=THET
      IF(MAX .EQ. 1) THEN
C-
C...... This is for 1-D element
C-
        K=0
        DO 120 M=1,NE
          K=K+1
          IF(NOP(K,3) .EQ. LINE(J,1)) THEN
            NA=NOP(K,3)
            NC=NOP(K,1)
            IF(NC .GT. NPM) GO TO 120
            GO TO 127
          ENDIF
          IF(NOP(K,1) .EQ. LINE(J,1)) THEN
            NA=NOP(K,1)
            NC=NOP(K,3)
            IF(NC .GT. NPM) GO TO 120
            GO TO 127
          ENDIF
  120   CONTINUE
  127   NM=NOP(K,2)
        IF(CORD(NM,1) .GT. VOID) THEN
          DX=2.*(CORD(NM,1)-CORD(NA,1))-0.5*(CORD(NC,1)-CORD(NA,1))
          DY=2.*(CORD(NM,2)-CORD(NA,2))-0.5*(CORD(NC,2)-CORD(NA,2))
        ELSE
          DX=CORD(NC,1)-CORD(NA,1)
          DY=CORD(NC,2)-CORD(NA,2)
        ENDIF
        BEETA=ATAN2(DY,DX)
        IF(ABS(BEETA-THET) .GT. 1.570796  .AND.
     +     ABS(BEETA-THET) .LT. 4.712394) BEETA=BEETA+3.141596
        SPEC(NA,1)=QREQ*COS(BEETA)
        SPEC(NA,2)=QREQ*SIN(BEETA)
        NFIX(NA)=31000
        IF(QQAL(1) .GE. 0.) THEN
          NFIX(NA)=NFIX(NA)+10
          SPEC(NA,4)=QQAL(1)
          VEL(4,NA)=QQAL(1)
CIPK OCT01
        ELSE
          NFIX(NA)=NFIX(NA)+20
          SPEC(NA,4)=-QQAL(1)
        ENDIF
        IF(QQAL(2) .GE. 0.) THEN
          NFIX(NA)=NFIX(NA)+1
          SPEC(NA,5)=QQAL(2)
          VEL(5,NA)=QQAL(2)
CIPK OCT01
        ELSE
          NFIX(NA)=NFIX(NA)+2
          SPEC(NA,5)=-QQAL(2)
        ENDIF
        IF(QQAL(3) .GE. 0.) THEN
          NFIX1(NA)=1
          SPEC(NA,6)=QQAL(3)
          VEL(6,NA)=QQAL(3)
CIPK OCT01
        ELSE
          NFIX1(NA)=2
          SPEC(NA,6)=-QQAL(3)
        ENDIF
cipk oc98        VM=SQRT(VEL(1,NA)**2+VEL(2,NA)**2)
C       VEL(1,NA)=VM*COS(BEETA)
C       VEL(2,NA)=VM*SIN(BEETA)
      ELSE
C-
C...... IBN contains counter for number of references to a node
C-
        DO 103 N=1,NPM
          IBN(N)=0
  103   CONTINUE
        DO 105 N=1,NE
          IF(IMAT(N) .LT. 1) GO TO 105
cipk oct98 update to f90
          NM=ABS(IMAT(N))
          IF(NM .GT. 900) GO TO 105
          IF(ORT(NM,1) .EQ. 0.) GO TO 105
          NCN=NCORN(N)
          IF(NCN .EQ. 5) NCN=3
          DO 104 M=1,NCN
cipk oct98 update to f90
            K=ABS(NOP(N,M))
            IBN(K)=IBN(K)+1
  104     CONTINUE
  105   CONTINUE
        SUMA=0.
        MAX = LMT(J)-2
        DO 150 K = 1, MAX, 2
          NA = LINE(J,K)
          NC = LINE(J,K+2)
          DX=CORD(NC,1)-CORD(NA,1)
          DY=-(CORD(NC,2)-CORD(NA,2))
          XL=SQRT(DX**2+DY**2)
          ALP=ATAN2(DX,DY)
          D1=VEL(3,NA)
          D3=VEL(3,NC)
          D2=(D1+D3)/2.
          SUMA = SUMA+XL*COS(ALP-THET)*D2
  150   CONTINUE
C-
C...... Compute velocity required
C-
        SUMA=ABS(SUMA)
        VEST=QREQ/SUMA
C-
C...... Insert values into SPEC  and  NFIX arrays
C-
        MAX=MAX+2
        DO 300 K=1,MAX
          NA=LINE(J,K)
          D1=VEL(3,NA)
          SPEC(NA,1)=VEST*D1*COS(THET)
          SPEC(NA,2)=VEST*D1*SIN(THET)
          IF(SPEC(NA,1) .NE. 0.) THEN
            NFIX(NA)=31000
          ELSE
            NFIX(NA)=13000
          ENDIF
          IF(QQAL(1) .GE. 0.) THEN
            NFIX(NA)=NFIX(NA)+10
            SPEC(NA,4)=QQAL(1)
            VEL(4,NA)=QQAL(1)
CIPK OCT01
          ELSE
            NFIX(NA)=NFIX(NA)+20
            SPEC(NA,4)=-QQAL(1)
          ENDIF
          IF(QQAL(2) .GE. 0.) THEN
            NFIX(NA)=NFIX(NA)+1
            SPEC(NA,5)=QQAL(2)
            VEL(5,NA)=QQAL(2)
CIPK OCT01
          ELSE
            NFIX(NA)=NFIX(NA)+2
            SPEC(NA,5)=-QQAL(2)
          ENDIF
          IF(QQAL(3) .GE. 0.) THEN
            NFIX1(NA)=1
            SPEC(NA,6)=QQAL(3)
            VEL(6,NA)=QQAL(3)
CIPK OCT01
          ELSE
            NFIX1(NA)=2
            SPEC(NA,6)=-QQAL(3)
          ENDIF
  300   CONTINUE
C-
C...... Find correct components for boundary nodes
C-
        DO 400 K=1,MAX,MAX-1
          NA=LINE(J,K)
C-
C...... Locate element with this node
C-
          DO 340 N=1,NE
            IF(IMAT(N) .EQ. 0) GO TO 340
            NCN=NCORN(N)
            DO 310 MM=1,NCN
              M=MM
cipk oct98 update to f90
              IF(ABS(NOP(N,M)) .EQ. NA) GO TO 315
  310       CONTINUE
            GO TO 340
C-
C....... Found a match node. Now determine side for parallel flow
C-
  315       IF(K .EQ. 1) THEN
              KK=2
            ELSE
              KK=MAX-1
            ENDIF
            MLW=M-1
            IF(MM .EQ. 1) THEN
              MLW=NCN
            ELSE
              MLW=M-1
            ENDIF
cipk oct98 update to f90

cipk oct98            MLX=ABS(NOP(N,MLW))
            MSL=MOD(MLW+2,NCN)
            IF(MSL .EQ. 0) MSL=NCN
            MFR=MOD(MLW+3,NCN)
cipk oct98 update to f90
            MSL=ABS(NOP(N,MSL))
            IF(MSL .EQ. LINE(J,KK)) GO TO 317
            IF(IBN(MSL) .EQ. 1) GO TO 330
  317       CONTINUE
            MLW=MOD(M+1,NCN)
            IF(MLW .EQ. 0) MLW=NCN
cipk oct98 update to f90
cipk oct98            MLX=ABS(NOP(N,MLW))
            IF(MLW .GT. 2) THEN
              MSL=MLW-2
              MFR=MLW-3
            ELSE
              MSL=NCN
              MFR=NCN-1
            ENDIF
cipk oct98 update to f90
            MSL=ABS(NOP(N,MSL))
            IF(MSL .EQ. LINE(J,KK)) GO TO 340
            IF(IBN(MSL) .EQ. 1) GO TO 330
            GO TO 340
  330       CONTINUE
cipk oct98 update to f90
            MFR=ABS(NOP(N,MFR))
C-
C...... Now compute boundary angle
C-
            DX=2.*(CORD(MSL,1)-CORD(NA,1))-0.5*(CORD(MFR,1)-CORD(NA,1))
            DY=2.*(CORD(MSL,2)-CORD(NA,2))-0.5*(CORD(MFR,2)-CORD(NA,2))
            BEETA=ATAN2(DY,DX)
C-
C...... Find angle of side that flow crosses
C-
            IF(K .EQ. 1) THEN
              NAD=LINE(J,3)
            ELSE
              NAD=LINE(J,MAX-2)
            ENDIF
            DXA=CORD(NAD,1)-CORD(NA,1)
            DYA=-(CORD(NAD,2)-CORD(NA,2))
            AALFA=ATAN2(DXA,DYA)
            IF(ABS(AALFA-THET) .GT. 1.570796  .AND.
     +         ABS(AALFA-THET) .LT. 4.712394) THEN
              AALFA=AALFA+3.141596
            ENDIF
            IF(ABS(AALFA-BEETA) .GT. 1.570796  .AND.
     +         ABS(AALFA-BEETA) .LT. 4.712394) THEN
              BEETA=BEETA+3.141596
            ENDIF
C-
C...... Finally adjust flows
C-
            IF(COS(THET) .NE. 0.) THEN
            QMAG=SPEC(NA,1)*COS(AALFA-THET)/(COS(THET)*COS(AALFA-BEETA))
            ELSE
            QMAG=SPEC(NA,2)*COS(AALFA-THET)/(SIN(THET)*COS(AALFA-BEETA))
            ENDIF
            SPEC(NA,1)=QMAG*COS(BEETA)
            SPEC(NA,2)=QMAG*SIN(BEETA)
            GO TO 400
  340     CONTINUE
  400   CONTINUE
      ENDIF
      IF(ICYC .GT. 0) THEN
        DO 500 K=1,MAX
          NA=LINE(J,K)
          CALL BFORM(NA)
  500   CONTINUE
      ENDIF
      RETURN
      END
