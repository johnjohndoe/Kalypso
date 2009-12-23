!
      SUBROUTINE DISFCT
      USE IHILMOD
      USE BLKHMOD
      USE BLK10MOD
      SAVE
!-
!......SUBROUTINE TO COMPUTE FACTORS AT EACH NODE
!-
      DIMENSION XN1(4),XN2(4),XN3(4)
!-
      DATA THRESH/1.0E-3/
      DIST(N1,N2)=(CORD(N1,1)-CORD(N2,1))**2+(CORD(N1,2)-CORD(N2,2))**2
!-
!....... Shape functions along a line at Gauss points
!-
      DO 200 INT=1,4
        XN1(INT)=(1.-AFACT(INT))*(1.-2.*AFACT(INT))
        XN2(INT)=4.*(1.-AFACT(INT))*AFACT(INT)
        XN3(INT)=(2.*AFACT(INT)-1.)*AFACT(INT)
  200 CONTINUE
!-
!...... Set distribution factors to initial value of 1.0
!-
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
!-
!...... Develop distribution factors for each node
!-
!...... Loop on all surface nodes
!-
      DO 350 N=1,NPM
        K=NREF(N)+1
        IF(K == 1) GO TO 350
        L=K+NDEP(N)-2
        IF( L < K) GO TO 350
        MO=N
!-
!......LOOP DOWNWARDS AT THIS SURFACE POINT
!-
        USUM=0.
        VSUM=0.
        SSUM=0.
        TSUM=0.
        SESUM=0.
        UUSUM=0.
        UVSUM=0.
        VVSUM=0.
        DO 300 M=K,L,2
          IF(M == K) THEN
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
        IF(ABS(USUM) > THRESH) UUDST(N)=UUSUM/USUM**2
        IF(ABS(USUM*VSUM) > THRESH) UVDST(N)=UVSUM/USUM*VSUM
        IF(ABS(VSUM) > THRESH) VVDST(N)=VVSUM/VSUM**2
!-
!...... Loop down through nodes to setup coefficients
!-
        K=NREF(N)
        L=K+NDEP(N)-1
        MO=N
        DO 325 M=K,L
          IF(M == K) THEN
            M1=N
          ELSE
            M1=M
          ENDIF
          DO 320 J=1,NDF
            NBC(M1,J)=NBC(N,J)
  320     CONTINUE
          IF(NBC(M1,3) == 0) GO TO 325
          IF(UDST(M1) == 1.) THEN
            IF(ABS(USUM) > THRESH) UDST(M1)=VEL(1,M1)/USUM
            IF(ABS(VSUM) > THRESH) VDST(M1)=VEL(2,M1)/VSUM
          ENDIF
          IF(ABS(SSUM) > THRESH) SDST(M1)=VEL(4,M1)/SSUM
          IF(ABS(TSUM) > THRESH) TDST(M1)=VEL(5,M1)/SSUM
          IF(ABS(SESUM) > THRESH) SEDST(M1)=VEL(6,M1)/SSUM
  325   CONTINUE
  350 CONTINUE
!-
!....... Distribute to mid side nodes
!-
      DO 480 N=1,NESAV
        IF(IMAT(N) > 0 .AND. IMAT(N) < 1000                             &
     & .OR. IMAT(N) > 5000) THEN
          NCN=NCORN(N)
          IK=1
          IF(NCN == 15) IK=2
          IF(NCN == 13) IK=4
          IF(NCN == 10) IK=3
          DO 460 K=1,NCN
            IF(IH(K,IK) /= 0) THEN
              NL=IH(K,IK)
              NR=IL(K,IK)
              N1=NOP(N,NL)
              N2=NOP(N,K)
              N3=NOP(N,NR)
              IF(DIST(N1,N3) > 1.E-3) THEN
                UDST(N2)=(UDST(N1)+UDST(N3))/2.
                VDST(N2)=(VDST(N1)+VDST(N3))/2.
                SDST(N2)=(SDST(N1)+SDST(N3))/2.
                TDST(N2)=(TDST(N1)+TDST(N3))/2.
                SEDST(N2)=(SEDST(N1)+SEDST(N3))/2.
                IF(N2 <= NPM) THEN
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
