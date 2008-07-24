CIPK  KAST UODATE JAN 5 2004 CHANGE VMIN TO VVMIN

C     Last change:  IPK   3 Oct 98    4:03 pm
cipk  last update July 22 1996
      FUNCTION IACTVBC(ND)
      USE BLK10MOD
      SAVE
C
C  Determine if flow at b.c. node is into or out of the network
C
C  IACTVBC = 0  -->     bc not applied
C  IACTVBC = 1  -->     apply bc
C
C  JBC =  0     -->     IACT = 0   always
C  JBC = -1     -->     IACT = 1   always
C  JBC = +1     -->     IACT = 1   when flow into system
C  JBC = +2     -->     IACT = 1   when flow into system,  Special BC
C  JBC = -2     -->     IACT = 1   always,  Special BC
C
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
C-
C-.....FIND ACTIVE NODES.....
      DIMENSION  SV(3)
      DATA   SV /0.02, 0.52, 0.98 /
C-
      PI2 = 2.*ATAN(1.)
CIPK JAN04 CHANGE VMIN TO VVMIN
      VVMIN = 1.0E-9
C
      I = ND
C
      IACTVBC = 0

C
CIPK OCT98 CONVER TO F90      ISURF = IABS(NSURF(I))
      ISURF = ABS(NSURF(I))
c     write(75,*) 'i,nsurf(i)',i,nsurf(i)
C
C  Check for zero slip bottom node, use above nodal vel for bc test
      IF (NDEP(ISURF) .GT.  0)  THEN
        IF (ND .EQ. NREF(ISURF) + NDEP(ISURF) - 1)  I = ND - 1
      ENDIF 
c
c     Test for zero velocity case
c
CIPK JAN04 CHANGE VMIN TO VVMIN
      IF (ABS(VEL(1,I)).LT. VVMIN .AND. ABS(VEL(2,I)).LT. VVMIN) THEN
        IACTVBC = 0
        RETURN
      ENDIF
C
CIPK OCT98 CONVER TO F90      ISURF = IABS(NSURF(I))
      ISURF = ABS(NSURF(I))
C
      SUMQ = 0.0
      DO 200 J = 1, NEM
        IF( IMAT(J) .EQ. 0 ) GO TO 200
        IF (IMAT(J) .GT. 900  .AND.  IMAT(J) .LT. 5000) GO TO 200
        NCN = NCRN(J)
C
C  See if node in this column of elements
        DO K=1,NCN
          IF (ISURF .EQ. NOPS(J,K))  GO TO 110
        END DO
        GO TO 200
C
  110   CONTINUE
c       write(75,*) 'i,j,k,nops(j,k)',i,j,k,nops(j,k),ncn
        SUM = 0.0
C
        IF (NCN .EQ. 5)  NCN = 3
C
        IF (NCN .LT. 6) THEN
          N1 = NOP(J,1)
          X1 = CORD(N1,1)
          Y1 = CORD(N1,2)
          N2 = NOP(J,2)
          X2 = CORD(N2,1)
          Y2 = CORD(N2,2)
          N3 = NOP(J,3)
          X3 = CORD(N3,1)
          Y3 = CORD(N3,2)
        ENDIF
C
            
        DO 190 K = 1, NCN
          IF ( ISURF .NE. NOPS(J,K) ) GO TO 190
C
C  1-D surface elements
          IF (NCN .LT. 6) THEN
            IF (SUMQ .NE. 0.) THEN
              IACTVBC = 1
              RETURN
            ENDIF
CZZZ
            IF (K .LE. 3)  THEN
              S = SV(K)
              XN1 = 1.0-3.0*S+2.0*S**2
              XN2 = 4.0*S*(1.0-S)
              XN3 = S*(2.0*S-1.0)
              X = XN1*X1 + XN2*X2 + XN3*X3
              Y = XN1*Y1 + XN2*Y2 + XN3*Y3
              IF (K .EQ. 1) THEN
                DX = X-CORD(I,1)
                DY = Y-CORD(I,2)
              ELSE
                DY = CORD(I,2)-Y
                DX = CORD(I,1)-X
              ENDIF
              IF (ABS(DX) .LT. 1.E-05) THEN
                ALF = SIGN(DY,PI2)
              ELSE
                ALF = ATAN2(DY,DX)
              ENDIF
C
              SUM = (VEL(1,I)*COS(ALF) + VEL(2,I)*SIN(ALF)) 
     +           * vel(3,i)*( 2.*WIDTH(I) 
     +           + (SS1(I)+SS2(I))*VEL(3,I) )/2.
              IF(K .EQ. 3) SUM=-SUM
            ENDIF
C
          ELSE
c           write(75,*) 'k',k
C
C  2-D surface elements
            IF( MOD(K,2) .NE. 0 ) GO TO 140
C-
C-.....MID SIDE NODE.....
C-
            NA = K
            SUM = 0.0
            ICQ = 1
            GO TO 170
C-
C-.....CORNER-FIND PROPER FACE.....
C-
  140       NFB = K - 1
            IF( NFB .LT. 1 ) NFB = NCN
            NFF = K + 1
            IFB = 0
            IFC = 0
            DO 160 L = 1, NEM
              IF ( IMAT(L) .EQ. 0 ) GO TO 160
              NCX = 6
              IF( NOPS(L,7) .NE. 0 ) NCX = 8
              DO 150 M = 2,NCX,2
                IF( NOPS(L,M) .EQ. NOPS(J,NFB) ) IFB = IFB + 1
                IF( NOPS(L,M) .EQ. NOPS(J,NFF) ) IFC = IFC + 1
  150         CONTINUE
  160       CONTINUE
            ICQ = 0
            SUM = 0.0
            NA = NFB
            IF ( IFB .EQ. 1 ) GO TO 170
 165        ICQ = ICQ + 1
            NA = NFF
            IF( IFC .NE. 1 ) GO TO 175
C
  170       N1 = NA - 1
            N2 = MOD(NA+1,NCN)
            N1 = NOPS(J,N1)
            N2 = NOPS(J,N2)
            NA = NOPS(J,NA)
c           write(75,*) 'n1,n2,na',n1,n2,na
C-
C-.....CHECK FOR CURVED SIDES.....
C-
            DX = CORD(N2,1) - CORD(N1,1)
            DY = CORD(N2,2)  - CORD(N1,2)
            DX2=sqrt(dx**2+dy**2)
            XMID = (CORD(N1,1) + CORD(N2,1))/2.0
            YMID = (CORD(N1,2)+CORD(N2,2))/2.0
c           write(75,*) 'xmid,cord(na,1)',xmid,cord(na,1)
c           write(75,*) 'ymid,cord(na,2)',ymid,cord(na,2)
            IF( ABS(XMID-CORD(NA,1)) .GT. 0.01*DX2 ) GOTO 175
            IF( ABS(YMID-CORD(NA,2)) .GT. 0.01*DX2 ) GO TO 175
C
c           write(75,*) 'vel',vel(2,i),vel(1,i),i
            SUM  = VEL(2,I)*DX - VEL(1,I)*DY + SUM
C
  175       continue
c           write(75,*) 'dx,dy,icq,sum',dx,dy,icq,sum
            IF( ICQ .EQ. 0 ) GO TO 165
          ENDIF
  190   CONTINUE
        SUMQ = SUMQ + SUM
  200 CONTINUE
C
        IF ( SUMQ .gt. 0.0001 )  IACTVBC = 1

        RETURN
C
      END
