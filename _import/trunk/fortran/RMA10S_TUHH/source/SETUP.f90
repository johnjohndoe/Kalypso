C     Last change:  WP   10 Jan 2008    7:07 pm
CIPK  LAST UPDATE SEP 6 2004  add error file
CIPK  LAST UPDATE JAN 12 1999 ADD LOGIC FOR 2DV JUNCTIONS
      SUBROUTINE SETUP
      USE BLK10MOD
      USE BLK11MOD
      SAVE
C-
C......DIMENSION STATEMENTS
C-
CIPK OCT98      DIMENSION IM(20,2)
cipk oct98      DATA IM/1,0,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,
CIPK OCT98     1        1,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0/
C-
C-.....COPY HEAD SPECS AND FIX BOUNDARY CONDITIONS.....
C-
      NDF=1
C-
C-    SET UP NBC ARRAY
C-
      DO 197 N=1,NP
      NTHREE(N)=1
  197 NBC(N,1)=-1
      DO 199 J=1,NE
      IF(IMAT(J) < 1) GO TO 199
      IF(IMAT(J) > 5000) GO TO 1975
      IF(IMAT(J) > 1000) GO TO 199
 1975 CONTINUE
cipk oct98 update to f90
      IMMT=IMAT(J)
      L=MOD(IMMT,100)
      !nis,jan08: Exclude polynomial approach
      if (l /= 89) then
        IF(ORT(L,1) == 0.) GO TO 199
      end if

cipk oct98 update to f90
CIPK OCT98      ILK=1
      NCN=NCORN(J)
CIPK OCT98      IF(NCN == 15) ILK=2
CIPK OCT98      IF(NCN == 10) ILK=2

      DeactNBC: DO K=1,NCN
cipk oct98 update to f90
        !N=ABS(NOP(J,K))
        N = NOP(J,K)

!nis,may07
!Add midside node for polynom approach
!        if (n < -1000) CYCLE deactNBC
!Add midside node for polynom approach
!-

        n = ABS(n)
        IF(NCN < 9) NTHREE(N)=0
C        IF(IM(K,ILK) == 0) GO TO 198
        NBC(N,1)=0
!  198 CONTINUE
      ENDDO deactNBC

  199 CONTINUE

C
C                                       FORM DEGREE OF FREEDOM ARRAY
C
      DO 200 N=1,NP
  200 NBC(N,1)=NBC(N,1)+1
      DO 220 NN=1,NPM
      NBC(NN,1)=0
      L=NDEP(NN)+NREF(NN)-1
      IF(L > 0) THEN
        NBC(L,1)=0
      ENDIF
  220 CONTINUE
C
C                                       REARRANGE ARRAY
      NSZF=0
      N = 0
      DO 300 NN = 1, NP
      N = N + 1
      IF( NBC(N,1) /= 1 ) GO TO 300
      NSZF=NSZF+1
      NBC(N,1)=NSZF
  300 CONTINUE
C-
      WRITE(ICFL,6007) NSZF
 6007 FORMAT( // 20X, '.....TOTAL NUMBER OF ACTIVE SYSTEM EQUATIONS =',
     1  I6 )
C
C     FIND LAST APPEAREANCE OF EACH NODE REMOVED
C

C
      IF( NSZF <= IR1MAX ) RETURN
cipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,6008) IR1MAX
      WRITE(75,6008) IR1MAX
 6008 FORMAT( / 10X, 'TOO MANY EQUATIONS..STOP CALLED..',i15 )
      STOP
      END
