CIPK LAST UPDATE SEP 05 2006  ADD DEPRATO
cipk last update JUNE 5 2006  add test marshing
CIPK  LAST UPDATE SEP 29 2005 MAKE DEPRAT LINEAR
C_____________________________________________________________________
      SUBROUTINE DEPSN
      USE BLK10MOD, only: np, npm, imid, ndep, nref, wsll, ao, it
      USE BLKSEDMOD, only: deprato, deprat, vs, taucd, taucdnd, bshear
      
      implicit none
      
      integer :: n, i, nn

C**********************************************************************
C
C       THIS SUBROUTINE COMPUTES THE RATE OF DEPOSITION IF ANY AT
C     EACH NODE POINT FOR THE TIME STEP.  THE COEFFICIENT DEPRAT
C     IS RETURNED.
C
C  *********************************************************************
C
      DO N=1,NP
CIPK SEP06
        DEPRATO(N)=DEPRAT(N)
        !MD: change: DEPRAT(N)=VS(N)
        DEPRAT(N)=0.0
      ENDDO
C
      DO N=1,NPM
       IF(IMID(N,1) .EQ. 0) THEN

         !MD:  kritische Depo-Schubspannung aus SPROP
         TAUCD=TAUCDND(N)
         I=N
         IF(NDEP(N) .GT. 1) I=NREF(N)+NDEP(N)-1
         DEPRAT(I)=0.0
         
cipk jun06  test for marshing
         if(wsll(i) .lt. ao(i)) go to 500

         IF(TAUCD .GT. BSHEAR(I)) THEN
C          DEPRAT(I)= VS(I)*(1.-BSHEAR(I)/TAUCD)/WD(I)
           DEPRAT(I)= VS(I)*(1.-BSHEAR(I)/TAUCD)
CIPK SEP06
         !MD:  IF(IT .EQ. 1) THEN
         !MD:    DEPRATO(I)=DEPRAT(I)
         !MD:  ENDIF
         ENDIF
       ENDIF
  500  continue       
      ENDDO

CIPK SEP06
      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          DEPRAT(NN)=(DEPRAT(IMID(NN,1))+DEPRAT(IMID(NN,2)))/2.
          DEPRATO(NN)=(DEPRATO(IMID(NN,1))+DEPRATO(IMID(NN,2)))/2.
        ENDIF
      ENDDO

      RETURN
      END
