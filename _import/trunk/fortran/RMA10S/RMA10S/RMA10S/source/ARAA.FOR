
      SUBROUTINE ARAA(NN)
      USE BLK10MOD
      SAVE
C
CIPK AUG05      INCLUDE 'BLK10.COM'
      INCLUDE 'BLKH.COM'
C-
      DIMENSION NCON(8),XL(3),YL(3),DNX(3)
C-
C-.....ASSIGN PROPER COEFS.....
C-
      AREA(NN)=0.
      IF(ITEQV(MAXN) .EQ. 5) THEN
        DO 61 N=1,8
          NCON(N)=NOPS(NN,N)
          IF(NCON(N) .NE. 0) NCN=N
   61   CONTINUE
      ELSE
        NCN=NCORN(NN)
        DO 63 N=1,NCN
          NCON(N)=NOP(NN,N)
   63   CONTINUE
      ENDIF
      IF(NCN .EQ. 5  .AND.  IMAT(NN) .NE. 901) NCN=3
C-
C- INITIALIZE MATRICES AND VARIABLES
C-
      CXX=COS(TH(NN))
      SAA=SIN(TH(NN))
      NGP=4
C-
C-.....COMPUTE LOCAL CORDS.....
C-
      NR=NCON(1)
      DO 100 K = 1, NCN
        N=NCON(K)
        DX=CORD(N,1)-CORD(NR,1)
        DY=CORD(N,2)-CORD(NR,2)
        XL(K)=DX*CXX+DY*SAA
        YL(K)=-DX*SAA+DY*CXX
  100 CONTINUE
C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
      DO 500 I = 1, NGP
        TEMP=(DNAL(2,I)*XL(2)+DNAL(3,I)*XL(3))
        DNX(2)=(4.-8.*AFACT(I))/TEMP
        DNX(3)=(4.*AFACT(I)-1.)/TEMP
        DYDX=YL(2)*DNX(2)+YL(3)*DNX(3)
        ALF=ATAN(DYDX)
        CSALF=COS(ALF)
        TEMP=TEMP/CSALF
        AMW=ABS(TEMP)*HFACT(I)/2.
        AREA(NN)=AREA(NN)+AMW
C-
C...... END GAUSS DO LOOP
C-
  500 CONTINUE
      RETURN
      END
