
      SUBROUTINE DISFCT
      USE BLK10MOD
      SAVE
C-
C......SUBROUTINE TO COMPUTE FACTORS AT EACH NODE
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
      INCLUDE 'BLKH.COM'
      DIMENSION XN1(4),XN2(4),XN3(4)
      DIMENSION IH(20,4),IL(20,4)
      DATA IH/0,3,0,5,0,7,0,1,13,15,17,19,0,15,0,17,0,19,0,13,
     1        0,3,0,5,0,1,10,12,14,0,12,0,14,0,10,0,0,0,0,0,
     2        0,3,0,5,0,1,10,10,10,0,0,0,0,0,0,0,0,0,0,0,
     3        0,3,0,5,0,1,9,11,0,11,0,11,9,0,0,0,0,0,0,0/
      DATA IL/0,1,0,3,0,5,0,7,1,3,5,7,0,13,0,15,0,17,0,19,
     1        0,1,0,3,0,5,1,3,5,0,10,0,12,0,14,0,0,0,0,0,
     2        0,1,0,3,0,5,1,3,5,0,0,0,0,0,0,0,0,0,0,0,
     3        0,1,0,3,0,5,1,3,0,9,0,5,5,0,0,0,0,0,0,0/
C-
      DATA THRESH/1.0E-3/
      DIST(N1,N2)=(CORD(N1,1)-CORD(N2,1))**2+(CORD(N1,2)-CORD(N2,2))**2
C-
C....... Shape functions along a line at Gauss points
C-
      DO 200 INT=1,4
        XN1(INT)=(1.-AFACT(INT))*(1.-2.*AFACT(INT))
        XN2(INT)=4.*(1.-AFACT(INT))*AFACT(INT)
        XN3(INT)=(2.*AFACT(INT)-1.)*AFACT(INT)
  200 CONTINUE
C-
C...... Set distribution factors to initial value of 1.0
C-
      DO 210 N=1,NPSAV
        UDST(N)=FCTV(N)
        VDST(N)=FCTV(N)
        UUDST(N)=1.0
        UVDST(N)=1.0
        VVDST(N)=1.0
        SDST(N)=1.0
        TDST(N)=1.0
        SEDST(N)=1.0
  210  CONTINUE
C-
C...... Develop distribution factors for each node
C-
C...... Loop on all surface nodes
C-
      DO 350 N=1,NPM
        K=NREF(N)+1
        IF(K .EQ. 1) GO TO 350
        L=K+NDEP(N)-2
        IF( L .LT. K) GO TO 350
        MO=N
C-
C......LOOP DOWNWARDS AT THIS SURFACE POINT
C-
        USUM=0.
        VSUM=0.
        SSUM=0.
        TSUM=0.
        SESUM=0.
        UUSUM=0.
        UVSUM=0.
        VVSUM=0.
        DO 300 M=K,L,2
          IF(M .EQ. K) THEN
            M1=N
          ELSE
            M1=M-1
          ENDIF
          M3=M+1
          MO=M
          XL=(CORD(M1,3)-CORD(M3,3))/((ELEV-AO(N))*2.)
          DO 250 INT=1,4
            U=VEL(1,M1)*XN1(INT)+VEL(1,M)*XN2(INT)+VEL(1,M3)*XN3(INT)
            V=VEL(2,M1)*XN1(INT)+VEL(2,M)*XN2(INT)+VEL(2,M3)*XN3(INT)
            S=VEL(4,M1)*XN1(INT)+VEL(4,M)*XN2(INT)+VEL(4,M3)*XN3(INT)
            T=VEL(5,M1)*XN1(INT)+VEL(5,M)*XN2(INT)+VEL(5,M3)*XN3(INT)
            E=VEL(6,M1)*XN1(INT)+VEL(6,M)*XN2(INT)+VEL(6,M3)*XN3(INT)
            USUM=USUM+U*HFACT(INT)*XL
            VSUM=VSUM+V*HFACT(INT)*XL
            SSUM=SSUM+S*HFACT(INT)*XL
            TSUM=TSUM+T*HFACT(INT)*XL
            SESUM=SESUM+E*HFACT(INT)*XL
            UUSUM=UUSUM+U**2*HFACT(INT)*XL
            UVSUM=UVSUM+U*V*HFACT(INT)*XL
            VVSUM=VVSUM+V**2*HFACT(INT)*XL
  250     CONTINUE
  300   CONTINUE
        IF(ABS(USUM) .GT. THRESH) UUDST(N)=UUSUM/USUM**2
        IF(ABS(USUM*VSUM) .GT. THRESH) UVDST(N)=UVSUM/USUM*VSUM
        IF(ABS(VSUM) .GT. THRESH) VVDST(N)=VVSUM/VSUM**2
C-
C...... Loop down through nodes to setup coefficients
C-
        K=NREF(N)
        L=K+NDEP(N)-1
        MO=N
        DO 325 M=K,L
          IF(M .EQ. K) THEN
            M1=N
          ELSE
            M1=M
          ENDIF
          DO 320 J=1,NDF
            NBC(M1,J)=NBC(N,J)
  320     CONTINUE
          IF(NBC(M1,3) .EQ. 0) GO TO 325
          IF(UDST(M1) .EQ. 1.) THEN
            IF(ABS(USUM) .GT. THRESH) UDST(M1)=VEL(1,M1)/USUM
            IF(ABS(VSUM) .GT. THRESH) VDST(M1)=VEL(2,M1)/VSUM
          ENDIF
          IF(ABS(SSUM) .GT. THRESH) SDST(M1)=VEL(4,M1)/SSUM
          IF(ABS(TSUM) .GT. THRESH) TDST(M1)=VEL(5,M1)/SSUM
          IF(ABS(SESUM) .GT. THRESH) SEDST(M1)=VEL(6,M1)/SSUM
  325   CONTINUE
  350 CONTINUE
C-
C....... Distribute to mid side nodes
C-
      DO 480 N=1,NESAV
        IF(IMAT(N) .GT. 0  .AND.  IMAT(N) .LT. 1000
     1 .OR.  IMAT(N) .GT. 5000) THEN
          NCN=NCORN(N)
          IK=1
          IF(NCN .EQ. 15) IK=2
          IF(NCN .EQ. 13) IK=4
          IF(NCN .EQ. 10) IK=3
          DO 460 K=1,NCN
            IF(IH(K,IK) .NE. 0) THEN
              NL=IH(K,IK)
              NR=IL(K,IK)
              N1=NOP(N,NL)
              N2=NOP(N,K)
              N3=NOP(N,NR)
              IF(DIST(N1,N3) .GT. 1.E-3) THEN
                UDST(N2)=(UDST(N1)+UDST(N3))/2.
                VDST(N2)=(VDST(N1)+VDST(N3))/2.
                SDST(N2)=(SDST(N1)+SDST(N3))/2.
                TDST(N2)=(TDST(N1)+TDST(N3))/2.
                SEDST(N2)=(SEDST(N1)+SEDST(N3))/2.
                IF(N2 .LE. NPM) THEN
                  UUDST(N2)=(UUDST(N1)+UUDST(N3))/2.
                  UVDST(N2)=(UVDST(N1)+UVDST(N3))/2.
                  VVDST(N2)=(VVDST(N1)+VVDST(N3))/2.
                ENDIF
              ENDIF
            ENDIF
  460     CONTINUE
        ENDIF
  480 CONTINUE
      RETURN
      END
