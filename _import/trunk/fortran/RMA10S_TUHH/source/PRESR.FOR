C     Last change:  MD   12 May 2009    4:12 pm

      SUBROUTINE PRESR
      USE BLK10MOD
      SAVE
C-
C......SUBROUTINE TO COMPUTE PRESSURE AT EACH NODE
C-
C......COMPUTE DENSITY AT EACH NODE
C-
      DO 250 N=1,NP
          RHO1=FDEN(VEL(4,N),IGF)
          RHO2=FDN(VEL(5,N),IGF)
          RHO3=FDSED(VEL(6,N),IGF)
C
          IF (GRAV < 32.)  THEN
            IF(IPASS1 == 1) THEN
              RHO1=1.935*516.
            ENDIF
            IF(IPASS2 == 1) THEN
              RHO2=1.935*516.
            ENDIF
            IF(IPASS3 == 1) THEN
              RHO3=1.935*516.
            ENDIF
            DEN(N)=RHO1+RHO2+RHO3 - 3.87*516.
          ELSE
            IF(IPASS1 == 1) THEN
              RHO1=1.935
            ENDIF
            IF(IPASS2 == 1) THEN
              RHO2=1.935
            ENDIF
            IF(IPASS3 == 1) THEN
              RHO3=1.935
            ENDIF

            DEN(N)=RHO1+RHO2+RHO3-3.87

C            DEN(N)= - CORD(N,3)+1000.0

          ENDIF
  250 CONTINUE
C-
C......LOOP ON ALL SURFACE NODES
C-
c      DO 350 N=1,NPM
c      PRESS(N)=0.
c      K=NREF(N)+1
c      IF(K == 1) GO TO 350
c      L=K+NDEP(N)-2
c      IF( L < K) GO TO 350
c      MO=N
cC-
cC......LOOP DOWNWARDS AT THIS SURFACE POINT
cC-
c      XHT=ELEV-AO(N)
c      DO 300 M=K,L
c      PRESS(M)=PRESS(MO)+(DEN(M)+DEN(MO))/2.*GRAV*(CORD(MO,3)
c     1-CORD(M,3))/XHT*VEL(3,N)
cC      PRESS(M-1)=(PRESS(MO)+PRESS(M))/2.
cC      PRESS(M) = GRAV*CORD(M,3)*(CORD(M,3)/2.-1000.0)
cC      PRESS(M) = GRAV*CORD(M,3)*(CORD(M,3)/2.)
c      MO=M
c  300 CONTINUE
c  350 CONTINUE
      RETURN
      END
