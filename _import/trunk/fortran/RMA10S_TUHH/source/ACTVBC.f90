!IPK  KAST UODATE JAN 5 2004 CHANGE VMIN TO VVMIN
!
!     Last change:  IPK   3 Oct 98    4:03 pm
!ipk  last update July 22 1996
      FUNCTION IACTVBC(ND)
      USE BLK10MOD
      SAVE
!
!  Determine if flow at b.c. node is into or out of the network
!
!  IACTVBC = 0  -->     bc not applied
!  IACTVBC = 1  -->     apply bc
!
!  JBC =  0     -->     IACT = 0   always
!  JBC = -1     -->     IACT = 1   always
!  JBC = +1     -->     IACT = 1   when flow into system
!  JBC = +2     -->     IACT = 1   when flow into system,  Special BC
!  JBC = -2     -->     IACT = 1   always,  Special BC
!
!-.....FIND ACTIVE NODES.....
      REAL, PARAMETER :: SV(3) = (/0.02, 0.52, 0.98 /)
!-
      PI2 = 2.*ATAN(1.)
!IPK JAN04 CHANGE VMIN TO VVMIN
      VVMIN = 1.0E-9
!
      I = ND
!
      IACTVBC = 0
!
!
!IPK OCT98 CONVER TO F90      ISURF = IABS(NSURF(I))
      ISURF = ABS(NSURF(I))
!     write(75,*) 'i,nsurf(i)',i,nsurf(i)
!
!  Check for zero slip bottom node, use above nodal vel for bc test
      IF (NDEP(ISURF) > 0)  THEN
        IF (ND == NREF(ISURF) + NDEP(ISURF) - 1)  I = ND - 1
      ENDIF 
!
!     Test for zero velocity case
!
!IPK JAN04 CHANGE VMIN TO VVMIN
      IF (ABS(VEL(1,I)) < VVMIN .AND. ABS(VEL(2,I)) < VVMIN) THEN
        IACTVBC = 0
        RETURN
      ENDIF
!
!IPK OCT98 CONVER TO F90      ISURF = IABS(NSURF(I))
      ISURF = ABS(NSURF(I))
!
      SUMQ = 0.0
      DO 200 J = 1, NEM
        IF( IMAT(J) == 0 ) GO TO 200
        IF (IMAT(J) > 900 .AND. IMAT(J) < 5000) GO TO 200
        NCN = NCRN(J)
!
!  See if node in this column of elements
        DO K=1,NCN
          IF (ISURF == NOPS(J,K))  GO TO 110
        END DO
        GO TO 200
!
  110   CONTINUE
!       write(75,*) 'i,j,k,nops(j,k)',i,j,k,nops(j,k),ncn
        SUM = 0.0
!
        IF (NCN == 5)  NCN = 3
!
        IF (NCN < 6) THEN
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
!
!
        DO 190 K = 1, NCN
          IF ( ISURF /= NOPS(J,K) ) GO TO 190
!
!  1-D surface elements
          IF (NCN < 6) THEN
            IF (SUMQ /= 0.) THEN
              IACTVBC = 1
              RETURN
            ENDIF
!ZZZ
            IF (K <= 3)  THEN
              S = SV(K)
              XN1 = 1.0-3.0*S+2.0*S**2
              XN2 = 4.0*S*(1.0-S)
              XN3 = S*(2.0*S-1.0)
              X = XN1*X1 + XN2*X2 + XN3*X3
              Y = XN1*Y1 + XN2*Y2 + XN3*Y3
              IF (K == 1) THEN
                DX = X-CORD(I,1)
                DY = Y-CORD(I,2)
              ELSE
                DY = CORD(I,2)-Y
                DX = CORD(I,1)-X
              ENDIF
              IF (ABS(DX) < 1.E-05) THEN
                ALF = SIGN(DY,PI2)
              ELSE
                ALF = ATAN2(DY,DX)
              ENDIF
!
              SUM = (VEL(1,I)*COS(ALF) + VEL(2,I)*SIN(ALF))             &
     &           * vel(3,i)*( 2.*WIDTH(I)                               &
     &           + (SS1(I)+SS2(I))*VEL(3,I) )/2.
              IF(K == 3) SUM=-SUM
            ENDIF
!
          ELSE
!           write(75,*) 'k',k
!
!  2-D surface elements
            IF( MOD(K,2) /= 0 ) GO TO 140
!-
!-.....MID SIDE NODE.....
!-
            NA = K
            SUM = 0.0
            ICQ = 1
            GO TO 170
!-
!-.....CORNER-FIND PROPER FACE.....
!-
  140       NFB = K - 1
            IF( NFB < 1 ) NFB = NCN
            NFF = K + 1
            IFB = 0
            IFC = 0
            DO 160 L = 1, NEM
              IF ( IMAT(L) == 0 ) GO TO 160
              NCX = 6
              IF( NOPS(L,7) /= 0 ) NCX = 8
              DO 150 M = 2,NCX,2
                IF( NOPS(L,M) == NOPS(J,NFB) ) IFB = IFB + 1
                IF( NOPS(L,M) == NOPS(J,NFF) ) IFC = IFC + 1
  150         CONTINUE
  160       CONTINUE
            ICQ = 0
            SUM = 0.0
            NA = NFB
            IF ( IFB == 1 ) GO TO 170
 165        ICQ = ICQ + 1
            NA = NFF
            IF( IFC /= 1 ) GO TO 175
!
  170       N1 = NA - 1
            N2 = MOD(NA+1,NCN)
            N1 = NOPS(J,N1)
            N2 = NOPS(J,N2)
            NA = NOPS(J,NA)
!           write(75,*) 'n1,n2,na',n1,n2,na
!-
!-.....CHECK FOR CURVED SIDES.....
!-
            DX = CORD(N2,1) - CORD(N1,1)
            DY = CORD(N2,2)  - CORD(N1,2)
            DX2=sqrt(dx**2+dy**2)
            XMID = (CORD(N1,1) + CORD(N2,1))/2.0
            YMID = (CORD(N1,2)+CORD(N2,2))/2.0
!           write(75,*) 'xmid,cord(na,1)',xmid,cord(na,1)
!           write(75,*) 'ymid,cord(na,2)',ymid,cord(na,2)
            IF( ABS(XMID-CORD(NA,1)) > 0.01*DX2 ) GOTO 175
            IF( ABS(YMID-CORD(NA,2)) > 0.01*DX2 ) GO TO 175
!
!           write(75,*) 'vel',vel(2,i),vel(1,i),i
            SUM  = VEL(2,I)*DX - VEL(1,I)*DY + SUM
!
  175       continue
!           write(75,*) 'dx,dy,icq,sum',dx,dy,icq,sum
            IF( ICQ == 0 ) GO TO 165
          ENDIF
  190   CONTINUE
        SUMQ = SUMQ + SUM
  200 CONTINUE
!
        IF ( SUMQ > 0.0001 )  IACTVBC = 1
!
        RETURN
!
      END
