C     Last change:  WP   25 Sep 2007   11:42 am

      SUBROUTINE REWET
      USE BLK10MOD
      USE BLKDRMOD
      SAVE
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
C-
      DATA VOID/1.E20/,VDRY/0./
      DIST(N1,N2)=(CORD(N2,1)-CORD(N1,1))**2+(CORD(N2,2)-CORD(N1,2))**2
      DO 200 N=1,NP
        DREC(N)=VOID
        !IF(NDRY(N) .EQ. 1) GO TO 200
        IF(NDRY(N) .EQ. 1 .and. vel(3, n) /= 0.0) GO TO 200
        NDRY(N)=2
        VEL(3,N)=VDRY
  200 CONTINUE
      DO 205 M=1,NE
cipk oct98 update to f90
        IMAT(M)=ABS(IMAT(M))
        NCN=NCORN(M)
        IF(NCN .LT. 9  .AND.  IMAT(M) .LT. 1000) THEN
          DO 204 K=1,NCN
            L=NOP(M,K)
            IF(L .EQ. 0) GO TO 204
cipk oct98 update to f90
            IF(NDRY(L) .NE. 1) IMAT(M)=-ABS(IMAT(M))
  204     CONTINUE
        ENDIF
  205 CONTINUE
C-
C......SET NUMBER OF PASSES
C-
      NPAS=19
      DO 600 NPA=1,NPAS
        IL=0
        ILO=0
        DO 500 N=1,NE
          IF(IMAT(N) .GE. 0) GO TO 500
          NCN=NCORN(N)
          ELD=VOID
          DO 220 M=1,NCN,2
cipk oct98 update to f90
            K=ABS(NOP(N,M))
            IF(NDRY(K) .NE. 2) GO TO 220
            DO 210 MM=1,NCN,2
cipk oct98 update to f90
              KK=ABS(NOP(N,MM))
              IF(NDRY(KK) .EQ. 2) GO TO 210
              IF(VEL(3,KK) .LT. DSET) GO TO 210
              ELD=SQRT(DIST(K,KK))
              IF(ELD .GT. DREC(K)) GO TO 210
              DREC(K)=ELD
C-
C......-HF- IS BEING COMPUTED TO ESTIMATE HEAD LOSS WITH A 0.2 VEL
C-
              IF(ZMANN(N) .EQ. 0.) GO TO 207
              HF=3./(VEL(3,KK)-DSET)*(1./DSET**0.333-1./VEL(3,KK)**.333)
              HLOS=HF*(0.2/1.486)**2*ZMANN(N)**2*ELD
              GO TO 208
c  207         HF=1./(VEL(3,KK)-DSET)*ALOG(VEL(3,KK)/DSET)
  207         HF=1./(VEL(3,KK)-DSET)*LOG(VEL(3,KK)/DSET)
              HLOS=HF*0.04*ELD/CHEZ(N)**2
  208         CONTINUE
              DEP=VEL(3,KK)+AO(KK)-AO(K)-HLOS
              IF(DEP .LE. 0.) GO TO 210
              VEL(3,K)=DEP
              VDOT(3,K)=ALTM*(VEL(3,K)-VOLD(3,K))
              VDOTO(3,K)=0.
              IL=IL+1
              LISTEL(IL)=N
              IF(IL .GT. 1  .AND.  LISTEL(IL) .EQ. LISTEL(IL-1) )IL=IL-1
  210       CONTINUE
  220     CONTINUE
          IF(ELD .EQ. VOID) GO TO 500
          IF(IL .EQ. ILO) GO TO 500
          ILO=IL
  500   CONTINUE
        IF(IL .EQ. 0) GO TO 700
        DO 550 I=1,IL
          NN=LISTEL(I)
          NCN=NCORN(NN)
          IF(NCN .LT. 9  .AND.  ABS(IMAT(NN)) .LT. 1000) THEN
            DO 530 M=1,NCN,2
cipk oct98 update to f90
              N=ABS(NOP(NN,M))
              IF(NDRY(N) .NE. 2) GO TO 530
              IF(VEL(3,N) .GT. 0) NDRY(N)=-1
  530       CONTINUE
          ENDIF
  550   CONTINUE
  600 CONTINUE
  700 CONTINUE
      RETURN
      END
