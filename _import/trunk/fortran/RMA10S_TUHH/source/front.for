CIPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES 
CIPK  LAST UPDATE MAR 25 2005
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE FEB 10 2004 IMPROVE DIAGNOSTIC
CIPK  LAST UPDATE DEC 16 2003 ADD IEDSW DEPENDENCE
CIPK  LAST UPDATE MAR 18 2003 add diffusion switch ( default of  0 uses old formulations
cipk  last update Mar 22 2001 revise to keep solution and NBC for equation dropout
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
cipk  last update Nov 18 1999 add capability to collapse elements to 2-D
cipk  last update Feb 4 1998 reduce output
cipk  last update Jan 21 97  add option for Smagorinksy in 2-D
      SUBROUTINE FRONT(NRX)
      USE BLK10
      USE BLK10MOD
      USE BLKSANMOD
      SAVE
C
C
C     FRONTAL ELIMINATION ROUTINE USING FULL PIVOTING
C
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK4.COM'
CIPK 
CIPK AUG05      INCLUDE 'BLKSAND.COM'
C      INCLUDE 'RKEP.COM'
C-
      COMMON/BLKE/ ESTIFM(80,80)
C-
CAUG93IPK  COMMON LDEST(80),NK(80)
      COMMON NK(80)
CIPK AUG05       DIMENSION QR(MFW),NCON(20)
      DIMENSION NCON(20)
C-
CIPK AUG05      COMMON/BIGONE/ EQ(MFW,MFW),LHED(MFW),QQ(MFW),PVKOL(MFW)
C-
c     LBMAX=MBUF

      NMAX = MFW
      CALL SECOND(ASEC)
      ISHRK=0
      TSURC=0
      TCOEFS=0
      TCOEF2=0
      TCOEF1=0

CIPK MAR0 setup switch that says salinity is active

      IF(ITEQV(MAXN) .EQ. 2  .OR.  ITEQV(MAXN) .EQ. 8
     +                       .OR.  ITEQV(MAXN) .EQ. 9) THEN
	  IF(IDIFSW .EQ. 0) THEN
	    ISLP=0
	  ELSE
          ISLP=1
	  ENDIF
      ELSE
        ISLP=0
      ENDIF

      WRITE(ICFL,6198) ASEC
 6198 FORMAT(/10X,' TIME STARTING FRONT =',F10.3)
 6199 FORMAT( /10X,'DELTA T =',F10.3, '  TOTAL T =',F10.3)
      WRITE(*,*)
     +'ELEMENTS-PROCESSED EQNS-PROCESSED CURRENT-FRONT MAX-FRONT'
     +,' PRESENT-BUFFER'
      NEC=0
      IRTC=0
      LQ=0
      LCMAX=0
C
C     PREFRONT
C
C
C...... Find last appeareance of each node moved from LOAD3
C
      DO J=1,NSZF
        NLSTEL(J)=0
      ENDDO
      K=NESAV+1
      DO NN=1,NESAV
        K=K-1
        N=NFIXH(K)
        IF(N .LE. NE  .AND.  N .GT. 0) THEN
         IF(ICOLLAPE(N) .EQ. 1  .AND.  IMAT(N)/1000 .NE. 1) GO TO 480

          IF(IMAT(N) .GT. 0) THEN

          ncn=20
          IF(ITEQV(MAXN) .EQ. 5) THEN
            DO  I=1,8
              NCON(I)=NOPS(N,I)
              IF(NCON(I) .GT. 0) NCN=I
            ENDDO
          ELSE
            DO I=1,NCN
              NCON(I)=NOP(N,I)
            ENDDO
          ENDIF
          MRC=N
cipk oct98 update to f90
          NTYP=NETYP(N)
          IF(MOD(NTYP,10) .EQ. 7) THEN
CIPK JAN99 SKIP OUT FOR 2DV
            if(ntyp .NE. 17) THEN
C-
C...... Search to see if any entry in NLSTEL
C-
              DO M=1,NCN
                L=NCON(M)
C SKIP OUT FOR ZERO
                if(l .GT. 0) THEN
                  DO I=1,7
                    J=NBC(L,I)
                    IF(J .GT. 0) THEN
                      IF(NLSTEL(J) .GT. 0) THEN
                        IF(NREORD(NLSTEL(J)) .GT. NREORD(MRC)) THEN
                          MRC=NLSTEL(J)
                        ENDIF
                        NLSTEL(J)=0
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDIF
  421     CONTINUE
          DO M=1,NCN
            L=NCON(M)
CIPK JAN99 SKIP OUT FOR 2DV
            if(l .GT. 0) THEN
CIPK MAY02 SWITCH NDF TO 7
              DO I=1,7
                J=NBC(L,I)
                IF(J .NE. 0) THEN
                  IF(NLSTEL(J) .EQ. 0) THEN
                    NLSTEL(J)=MRC
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
       ENDIF
  480 CONTINUE
      ENDDO

      NELL=0
      NELM=0
c      DO N=1,NP
c        WRITE(75,*) 'nbc',nrx,N,(NBC(N,M),M=1,4)
c      ENDDO
c      do n=1,nszf
c        write(75,*) 'nlst',n,nlstel(n),netyp(n)
c      enddo

C
C     ASSEMBLY
C
      DO 15 N=1,NSZF
      IPOINT(N)=0
CC      rkeep(n)=0.
CC      ekeep(j)=0.
   15 R1(N)=0.
C-
C......ESTABLISH DENSITIES AND PRESSURES
C-
      IF(NRX .EQ. 1) THEN
	CALL PRESR
	NDF=4
      ENDIF
      LCOL=0
   18 NELL=NELL+1
      NELM=NELM+1
      IF(NELL.GT.NE) GO TO 380
      N=NFIXH(NELM)

c      write(75,*) 'front',n,nelm,netyp(n),imat(n)
      IF (N .EQ. 0)  GO TO 380
      IF(IMAT(N) .LT. 1) GO TO 18
      CALL SECOND(SINC)

CIPK DEC03 ADD IEDSW DEPENDENCE

      NMATYP=(MOD(IMAT(N),1000))
	IEDSW=IEDSW1(NMATYP)
	TBFACT=TBFACT1(NMATYP)
	TBMIN=TBMIN1(NMATYP)

      IF(ITEQV(MAXN) .NE. 5) THEN
          
	IF(IMAT(N) .GT. 1000  .AND.  IMAT(N) .LT. 5000) THEN
	  IF(NRX .EQ. 2) GO TO 18

CIPK NOV99     Either process surface integrals or collapse to 2-d

          IF(ICOLLAPE(N) .EQ. 0) THEN
     	    CALL SURCOF(N,NRX)
          ELSEIF(IMAT(N) .LT. 2000) THEN
            IF(NETYP(N)/10 .LT. 1) THEN
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF1(N,NRX)
              ELSE
                CALL COEF1NT(N,NRX)
	      ENDIF

C     Modify tests to allows for IDIFSW

            ELSEIF(IUTUB .EQ. 1  .and.  iedsw .eq. 2  .AND. ISLP .EQ. 0)
     +	      THEN
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF2D(N,NRX)
	      ELSE
                CALL COEF2DNT(N,NRX)
	      ENDIF
            ELSEIF(ISLP .EQ. 1  .AND.  IUTUB .EQ. 1 .AND. IDIFSW .EQ. 2)
     +        THEN
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF2D(N,NRX)
	      ELSE
                CALL COEF2DNT(N,NRX)
	      ENDIF
      	  ELSE
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF2(N,NRX)
	      ELSE
	        CALL COEF2NT(N,NRX)
	      ENDIF
            ENDIF
          ELSE
            GO TO 18
          ENDIF
	  CALL SECOND(SOUC)
	  TSURC=SOUC-SINC+TSURC
	ELSE
	  IF(NETYP(N)/10 .EQ. 2) THEN

cipk nov99 skip if collapsing to 2-d

            IF(ICOLLAPE(N) .EQ. 1 .AND. NRX .NE. 2) GO TO 18

C     Process   threed element
cipk jan97
            if(iutub .eq. 1  .AND.  IEDSW .EQ. 2) then
              CALL COEF3D(N,NRX)
            else
              call coef3(n,nrx)
            endif
cipk jan97 end changes
	    CALL SECOND(SOUC)
	    TCOEFS=TCOEFS+SOUC-SINC
	  ELSE
	    IF(NETYP(N)/10 .EQ. 1) THEN

C     Process   twod elements

CIPK JUN05
	      IF(IMAT(N)  .LT. 900  .OR.  NCORN(N) .GT. 5) THEN

C     Process   horizontal 2d

		IF(NRX .EQ. 2) GO TO 18
cipk jan97
            if(iutub .eq. 1  .AND.  IEDSW .EQ. 2  .AND. ISLP .EQ. 0)
     +        then
                    if(nell .eq. 1)  then
                      write(75,*) 'going to smag'
                    endif
CIPK MAR05
                    IF(INOTR .EQ. 0) THEN
                      CALL COEF2D(N,NRX)
	            ELSE
	              CALL COEF2DNT(N,NRX)
	            ENDIF
            ELSEIF(ISLP .EQ. 1  .AND.  IUTUB .EQ. 1 .AND. IDIFSW .EQ. 2)
     +        THEN
CIPK MAR05
                    IF(INOTR .EQ. 0) THEN
                      CALL COEF2D(N,NRX)
	            ELSE
	              CALL COEF2DNT(N,NRX)
	            ENDIF
                  else
CIPK MAR05
                    IF(INOTR .EQ. 0) THEN
                      CALL COEF2(N,NRX)
	            ELSE
	              CALL COEF2NT(N,NRX)
	            ENDIF
                  endif
cipk jan97 end changes
	      ELSE
CIPK JAN99
                IF(IMAT(N) .GT. 900  .AND.  IMAT(N) .LT. 1000) GO TO 18

C      Process vertical 2d


cipk nov99 skip if collapsing to 2-d

                IF(ICOLLAPE(N) .EQ. 1  .AND.  NRX .NE. 2) GO TO 18

		CALL COEFV(N,NRX)
	      ENDIF
	      CALL SECOND(SOUC)
	      TCOEF2=SOUC-SINC+TCOEF2
            ELSE

C      Process one-d elements

	      IF(NRX .EQ. 2) GO TO 18
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF1(N,NRX)
              ELSE
                CALL COEF1NT(N,NRX)
              ENDIF
	      CALL SECOND(SOUC)
	      TCOEF1=SOUC-SINC+TCOEF1
	    ENDIF
	  ENDIF
	ENDIF
      ELSE
C-
C...... The calls below are for the case of vertical averaging
C-
   19   IF(N .LE. NE) GO TO 20
	NELM=NELM+1
	N=NFIXH(NELM)
	GO TO 19
   20   CONTINUE
	IF(NOPS(N,6) .GT. 0) THEN
CIPK MAR05
          IF(INOTR .EQ. 0) THEN
            CALL COEF2(N,NRX)
          ELSE
            CALL COEF2NT(N,NRX)
          ENDIF
	  CALL SECOND(SOUC)
	  TCOEF2=TCOEF2+SOUC-SINC
	ELSE
CIPK MAR05
          IF(INOTR .EQ. 0) THEN
            CALL COEF1(N,NRX)
          ELSE
            CALL COEF1NT(N,NRX)
          ENDIF
	  CALL SECOND(SOUC)
	  TCOEF1=TCOEF1+SOUC-SINC
	ENDIF
      ENDIF
cipk jan99

      IF(NETYP(N) .EQ. 15  .OR.  NETYP(N) .EQ. 16) THEN
        IF(NOP(N,14) .NE. 0) NCN=14
        IF(NOP(N,18) .NE. 0) NCN=18
       
      ENDIF

      NBN = NCN*NDF

      DO 21 LK=1,NBN
	LDEST(LK)=0
	NK(LK)=0
   21 CONTINUE
      KC=0
      DO 23 J=1,NCN
	IF(ITEQV(MAXN) .EQ. 5) THEN
	  I=NOPS(N,J)
	ELSE
	  I=NOP(N,J)
	ENDIF
	DO 22 L=1,NDF
	  KC=KC+1
CIPK JAN99
          if(i .eq. 0) go to 22
	  LL=NBC(I,L)
	  NK(KC)=LL
	  IF(LL .NE. 0) THEN
	    IF(NLSTEL(LL) .EQ. N) NK(KC)=-LL
	  ENDIF
   22   CONTINUE
   23 CONTINUE
C-
C...... Set up heading vectors
C-
      LFZ=1
      DO 52 LK=1,NBN
	NODE=NK(LK)
	IF(NODE.EQ.0) GO TO 52
	LM=IABS(NODE)
	LL=IPOINT(LM)
	IF(LL .NE. 0) THEN
	  LDEST(LK)=LL
	  IF(NODE .LT. 0) LHED(LL)=NODE
	ELSE
C
C     Look for vacant slot
C
	  DO 35 L=LFZ,LCOL
	    IF(LHED(L) .EQ. 0) THEN
	      IPOINT(LM)=L
	      LHED(L)=NODE
	      LDEST(LK)=L
	      LFZZ=L
	      ISHRK=ISHRK-1
	      GO TO 40
	    ENDIF
   35     CONTINUE
	  LCOL=LCOL+1
	  LDEST(LK)=LCOL
	  LHED(LCOL)=NODE
	  IPOINT(LM)=LCOL
	  LFZZ=LCOL
	  DO 38 L=1,LCOL
	    EQ(L,LCOL)=0.
	    EQ(LCOL,L)=0.
   38     CONTINUE
   40     LFZ=LFZZ
	ENDIF
   52 CONTINUE
CIPK FEB04
      DO L=1,LCOL
	  LEQ=ABS(LHED(L))
	  IF(NLSTEL(LEQ) .EQ. N) LHED(L)=-ABS(LHED(L))
      ENDDO

      IF(LCOL .GT. LCMAX) LCMAX=LCOL
      IF(MOD(NELL,1000) .EQ. 0) THEN
        WRITE(*,'(I18,I15,I14,I10,I15)') NELL,NEC,LCOL,LCMAX,LQ
      ENDIF
      IF(LCOL.LE.NMAX) GO TO 54
      NERROR=2
CIPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,417)NERROR
      WRITE(75,417)NERROR
      WRITE(*,6008)   NFIXH(NELL)
      WRITE(75,6008)   NFIXH(NELL)
 6008 FORMAT( // 10X, '..STOP AT ELEMENT', I10 )
      WRITE(75,9820) (LHED(L),L=1,LCOL)
      WRITE(75,9822) (EQ(L,L),L=1,LCOL)
CIPK FEB04
 9823   FORMAT('  OFFENDING EQN      NODE   DEG OF FR')
 9824   FORMAT(I15,I10,I12)
        WRITE(75,9823)
        DO L=1,LCOL
	    NEQN=ABS(LHED(L))
	    IF(NEQN .GT. 0) THEN
   	      DO N=1,NP
              DO M=1,3
                IF(NBC(N,M) .EQ. NEQN) THEN
                  WRITE(75,9824) NEQN,N,M
	            GO TO 53
                ENDIF 
	        ENDDO
	      ENDDO
	    ENDIF
   53     CONTINUE
	  ENDDO
      WRITE(75,9821) (N,(NBC(N,M),M=1,4),N=1,NP)
      STOP
   54 CONTINUE
      DO 57 L=1,NBN
	IF(NK(L) .NE. 0) THEN
	  LL=LDEST(L)
	  DO 56 K=1,NBN
	  IF(NK(K) .NE. 0) THEN
	    KK=LDEST(K)
	    EQ(LL,KK)=EQ(LL,KK)+ESTIFM(K,L)
	  ENDIF
   56     CONTINUE
	ENDIF
   57 CONTINUE
C
C     FIND OUT WHICH MATRIX ELEMENTS ARE FULLY SUMMED
C
   60 LPIVCO=0
      PIVOT=0.
      DO 64 L=1,LCOL
	IF(LHED(L) .GT. -1) GO TO 64
C     WRITE(*,*) 'NELL,LHED,EQ',NELL,LHED(L),EQ(L,L)
	PIVA=EQ(L,L)
	IF(ABS(PIVA) .LT. ABS(PIVOT)) GO TO 64
	PIVOT=PIVA
	LPIVCO=L
   64 CONTINUE
      IF(LPIVCO.EQ.0) GO TO 18
C     WRITE(*,*) 'LPIVCO,PIVOT',LPIVCO,PIVOT
      IF( ABS(PIVOT) .LT. 1.0E-8 .AND. NELL .LE. NE ) GO TO 18
C
C     NORMALISE PIVOTAL ROW
C
      LCO=IABS(LHED(LPIVCO))
C     IF(ABS(PIVOT).LT.1E-08)WRITE(ICFL,476)
CTEMP     pivtin=1.0/pivot
      DO 80 L=1,LCOL
	QQ(L)=EQ(L,LPIVCO)/PIVOT
CTEMP        QQ(L)=EQ(L,LPIVCO)*pivtin
	QR(L)=EQ(LPIVCO,L)
   80 CONTINUE
      QR(LPIVCO)=0.0
      QQ(LPIVCO)=0.0
      RHS=R1(LCO)/PIVOT
CTEMP      RHS=R1(LCO)*pivtin
      R1(LCO)=RHS
      PVKOL(LPIVCO)=PIVOT
C
C     ELIMINATE THEN DELETE PIVOTAL ROW AND COLUMN
C
      DO 100 K=1,LCOL
	IF(QR(K) .EQ. 0.) GO TO 100
	KRW=IABS(LHED(K))
	R1(KRW)=R1(KRW)-QR(K)*RHS
	DO 90 L=1,LCOL
	  EQ(L,K)=EQ(L,K)-QR(K)*QQ(L)
   90   CONTINUE
  100 CONTINUE  
C
C     WRITE PIVOTAL EQUATION ON DISC
C
      NEC=NEC+1
      LCS(NEC)=LCOL
      LPS(NEC)=LPIVCO
      DO 105 L=1,LCOL
	LQ=LQ+1
	LHS(LQ)=LHED(L)
	QS(LQ)=QQ(L)
  105 CONTINUE
      IF(LQ .LT. LBMAX-NMAX) GO TO 108
      CALL XWRT(ND1,-1,NRR)
      IRTC=IRTC+1
      LQ=0
 108  CONTINUE
      DO 109 L=1,LCOL
	EQ(L,LPIVCO)=0.
	EQ(LPIVCO,L)=0.
  109 CONTINUE
      LHED(LPIVCO)=0
C
C     REARRANGE HEADING VECTORS
C
C      IF(ISHRK .GT. LCOL/10+4) THEN
       IF(ISHRK .GE. 40) THEN
C
CAUG93  Changes start here
c       KM=0
c       DO 120 K=1,LCOL
c         IF(LHED(K) .NE. 0) THEN
c           KM=KM+1
c           LM=0
c           DO 110 L=1,LCOL
c             IF(LHED(L) .NE. 0) THEN
c               LM=LM+1
c               EQ(LM,KM)=EQ(L,K)
c             ENDIF
c 110       CONTINUE
c         ENDIF
c 120   CONTINUE
c       KM=0
c       DO 125 K=1,LCOL
c         IF(LHED(K) .NE. 0) THEN
c           KM=KM+1
c           LHED(KM)=LHED(K)
c           IPOINT(ABS(LHED(K)))=KM
c         ENDIF
c 125   CONTINUE
c       ISHRK=0
c       LCOL=KM
	KM=0
	DO 110 K=1,LCOL
	  IF(LHED(K) .NE. 0) THEN
	    KM=KM+1
	    LHED(KM)=LHED(K)
	    IPOINT(ABS(LHED(K)))=KM
	    LDEST(KM)=K
	  ENDIF
  110   CONTINUE
	LCOL=KM
C
	DO 120 K=1,LCOL
	    KM=LDEST(K)
	    DO 115 L=1,LCOL
		LM=LDEST(L)
		EQ(L,K)=EQ(LM,KM)
  115       CONTINUE
  120   CONTINUE
	ISHRK=0
C
CAUG93 changes end here
      ELSE
	ISHRK=ISHRK+1
      ENDIF
C
C     DETERMINE WHETHER TO ASSEMBLE,ELIMINATE,OR BACKSUBSTITUTE
C
      IF( LCOL .GT. 0) GO TO 60
      IF( NELL .LT. NE) GO TO 18
  380 CONTINUE
      IF(LCOL .GT. 0) THEN
	DO 400 L=1,LCOL
	  IF(LHED(L) .NE. 0) GO TO 405
  400   CONTINUE
	GO TO 420
  405   CONTINUE
CIPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')

	WRITE(75,479) LCOL,ISHRK
	WRITE(*,479) LCOL,ISHRK
  479   FORMAT( '  UNSATISFIED ELIMINATION ERROR STOP'/
     1  '   LCOL =',I5,' ISHRK =',I5)
	WRITE(75,9820) (LHED(L),L=1,LCOL)
	WRITE(75,9822) (EQ(L,L),L=1,LCOL)
 9820   FORMAT('  CONTENTS OF LHED ARE'/(5I8))
	WRITE(75,9821) (N,(NBC(N,M),M=1,4),N=1,NP)
 9821   FORMAT('  NBC ARRAY IS '/(5I8))
 9822   FORMAT('  EQ IS'/(1P5E12.4))
	CALL ZVRS(1)
	STOP 'Unsatisfied elimination error'
      ENDIF
  420 CONTINUE
      TTOTA=TSURC+TCOEF2+TCOEFS+TCOEF1
      WRITE(*,7804) TCOEF1,TSURC,TCOEF2,TCOEFS,TTOTA,LCMAX,IRTC,LQ
      WRITE(75,7804) TCOEF1,TSURC,TCOEF2,TCOEFS,TTOTA,LCMAX,IRTC,LQ
 7804 FORMAT(20X,'TIME IN 1-DIM   ELEMENTS',F10.3/
     +       20X,'TIME IN SURFACE ELEMENTS',F10.3/
     1       20X,'TIME IN 2-DIM   ELEMENTS',F10.3/
     2       20X,'TIME IN 3-DIM   ELEMENTS',F10.3/
     3       20X,'TOTAL TIME IN   ELEMENTS',F10.3/
     4       20X,'MAXIMUM FRONT WIDTH     ',I10/
     5       20X,'BUFFER BLOCKS WRITTEN   ',I10/
     6       20x,'FINAL LQ SIZE           ',I10)
      CALL SECOND(TA)
      ADT=TA-ASEC
      WRITE(75,6199) ADT,TA
      WRITE(*,6199) ADT,TA
C      WRITE(LOUT,7805) ADT,TTOTA,LCMAX,IRTC
C 7805 FORMAT(20X,'TIME IN FORWARD SECTION OF FRONT',F10.3/
C     3       20X,'TOTAL TIME IN   ELEMENTS        ',F10.3/
C     4       20X,'MAXIMUM FRONT WIDTH             ',I10/
C     5       20X,'BUFFER BLOCKS WRITTEN           ',I10)
C
C     BACK SUBSTITUTION
C
      NEC=NSZF+1
      DO 600 IV=1,NSZF
      NEC=NEC-1
      LCOL=LCS(NEC)
      LPIVCO=LPS(NEC)
      LQ=LQ-LCOL
      IF(LQ .GT. -1) GO TO 450
      CALL XRED(ND1,IRTC,NRR)
C     CALL RED(ND1,-1)
      LQ=LQ-LCOL
  450 DO 460 L=1,LCOL
      LQ=LQ+1
      LHED(L)=IABS(LHS(LQ))
  460 QQ(L)=QS(LQ)
      LQ=LQ-LCOL
      LCO=LHED(LPIVCO)
      GASH=0.
      QQ(LPIVCO)=0.
      DO 580 L=1,LCOL
      ITMM=LHED(L)
      IF(ITMM .NE. 0) THEN
	GASH=GASH-QQ(L)*R1(ITMM)
      ENDIF
  580 CONTINUE
      R1(LCO)=R1(LCO)+GASH
  600 CONTINUE
      CALL SECOND(TAA)
      ADT=TAA-TA
      WRITE(ICFL,6199) ADT,TAA

cipk mar01  Save results and NBC for equation dropout
      IF(IDRPT .GT. 0  .AND. NRX .EQ. 1) THEN
        do j=1,nszf
          RKEEP(J)=R1(J)
        enddo
	  DO J=1,NP
  	    DO K=1,NDF
	      NBCKP(J,K)=NBC(J,K)
	    ENDDO
	  ENDDO
	ENDIF

C      do n=1,np
C	do m=1,ndf
C	  if(nbc(n,m) .gt. 0) then
C          write(73,*) n,m,nbc(n,m),rkeepeq(nbc(n,m))
C        endif
C	enddo
C	enddo

  417 FORMAT(/' NERROR =',I5//
     1 'MFW IS NOT LARGE ENOUGH TO PERMIT ASSEMBLY OF THE NEXT EL'
     2,'EMENT'/'  INCREASE MFW IN PARAM.COM OR LOOK FOR ERROR IN'
     3,' ELEMENT ELIMINATION ORDER')
  476 FORMAT(' WARNING-MATRIX SINGULAR OR ILL CONDITIONED')
      IF(NRX .EQ. 1) NDF=6
      RETURN
      END
