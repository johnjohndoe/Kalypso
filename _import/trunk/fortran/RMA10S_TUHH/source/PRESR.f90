!     Last change:  MD   12 May 2009    4:12 pm
!
      SUBROUTINE PRESR
      USE BLK10MOD
      SAVE
!-
!......SUBROUTINE TO COMPUTE PRESSURE AT EACH NODE
!-
!......COMPUTE DENSITY AT EACH NODE
!-
      DO 250 N=1,NP
          RHO1=FDEN(VEL(4,N),IGF)
          RHO2=FDN(VEL(5,N),IGF)
          RHO3=FDSED(VEL(6,N),IGF)
!
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
!
            DEN(N)=RHO1+RHO2+RHO3-3.87
!
!            DEN(N)= - CORD(N,3)+1000.0
!
          ENDIF
  250 CONTINUE
!-
!......LOOP ON ALL SURFACE NODES
!-
!      DO 350 N=1,NPM
!      PRESS(N)=0.
!      K=NREF(N)+1
!      IF(K == 1) GO TO 350
!      L=K+NDEP(N)-2
!      IF( L < K) GO TO 350
!      MO=N
!C-
!C......LOOP DOWNWARDS AT THIS SURFACE POINT
!C-
!      XHT=ELEV-AO(N)
!      DO 300 M=K,L
!      PRESS(M)=PRESS(MO)+(DEN(M)+DEN(MO))/2.*GRAV*(CORD(MO,3)
!     1-CORD(M,3))/XHT*VEL(3,N)
!C      PRESS(M-1)=(PRESS(MO)+PRESS(M))/2.
!C      PRESS(M) = GRAV*CORD(M,3)*(CORD(M,3)/2.-1000.0)
!C      PRESS(M) = GRAV*CORD(M,3)*(CORD(M,3)/2.)
!      MO=M
!  300 CONTINUE
!  350 CONTINUE
      RETURN
      END
