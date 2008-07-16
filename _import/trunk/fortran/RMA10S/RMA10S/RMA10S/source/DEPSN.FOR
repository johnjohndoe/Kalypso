CIPK  LAST UPDATE SEP 29 2005 MAKE DEPRAT LINEAR
C_____________________________________________________________________
      SUBROUTINE DEPSN
      USE BLK10MOD
      USE BLKSEDMOD
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
C-
C**********************************************************************
C
C       THIS SUBROUTINE COMPUTES THE RATE OF DEPOSITION IF ANY AT
C     EACH NODE POINT FOR THE TIME STEP.  THE COEFFICIENT DEPRAT
C     IS RETURNED.
C
C  *********************************************************************
C
      DO N=1,NP
        DEPRAT(N)=VS(N)
      ENDDO
C
      DO N=1,NPM
       IF(IMID(N,1) .EQ. 0) THEN
         TAUCD=TAUCDND(N)
         I=N
         IF(NDEP(N) .GT. 1) I=NREF(N)+NDEP(N)-1
         DEPRAT(I)=0.
         IF(TAUCD .GT. BSHEAR(I)) THEN
C          DEPRAT(I)= VS(I)*(1.-BSHEAR(I)/TAUCD)/WD(I)
           DEPRAT(I)= VS(I)*(1.-BSHEAR(I)/TAUCD)
         ENDIF
       ENDIF
      ENDDO
      RETURN
      END
