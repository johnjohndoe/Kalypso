CIPK  LAST UPDATE SEP 6 2004 RENAME FILE ENTRY NAME  add error file
CIPK  LAST UPDATE OCT 17 2001 ADD MORE OPTIONS FOR POWER STATION RECYCLE
CIPK  LAST UPDATE MAR 18 2001   ADD FOR POWER STATION RECYCLING
C     Last change:  WP    5 Nov 2007    3:26 pm
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
cipk  last update Jun 29 1998
CIPK  LAST UPDATE DEC 16 1997
cipk  last update Nov 26 1997
CIPK  LAST UPDATE APRIL 28 1996 MANY CHANGES
      !EFa apr09, added qdir for input of external boundary conditions, again jun09
      SUBROUTINE GETQ(ILAYR,N,QF,IYRR,DAYNOW,TIME,TETH,QQAL,QDIR)
      USE PARAMMOD
      use blk_ifu
!      USE BLK10MOD
      SAVE
CIPK DEC97 MOVE TO PARAM.COM      PARAMETER (NCQBS=20,NCQOBS=200)
C

!NiS,jul06: Add IWINDIN for correct memory interpretation
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,IWINDIN
!-

CIPK DEC97 CHANGE NAME OF LIMIT PARAMETER
CIPK AUG05      INCLUDE 'PARAM.COM'

CIPK MAR01
!TOASK
!nis,oct07: replace by using blk10mod
      INCLUDE 'BLKP.COM'
!-

      ALLOCATABLE  NCLIN(:),NHY(:),TA(:,:),
     +           ILAYRQ(:,:),IYDAT(:),DYQ(:,:),
     +           QA(:,:),QD(:,:,:),
     +           QDIRA(:,:) !EFa jun09, added for qdir in external file
CIPK SEP04
      CHARACTER*32 FNAMT
!NiS,jul06: Adjust length of string variable for proper parameter passing
!      CHARACTER*80 HTITLE,DLIN
      CHARACTER*80 HTITLE
      CHARACTER*72 DLIN
!-
      CHARACTER*8 ID
      DIMENSION QQAL(*)
      DATA NHYD/0/,ITIMEH/0/
      WRITE(*,*) 'N,TIME',N,TIME
      IF(IQUNIT .EQ. 0) THEN
        WRITE(*,*) 'Enter filename for continuity line hydrographs'
CIPK SEP04
        READ(*,4800) FNAMT
        IQUNIT=11
        OPEN(UNIT=11,FILE=FNAMT,STATUS='OLD')
      ENDIF
      IF(ITIMEH .EQ. 0) THEN
        ALLOCATE (NCLIN(NQLDS),NHY(NQLDS),TA(NCQOBS,NQLDS),
     +           ILAYRQ(NCQOBS,NQLDS),IYDAT(NQLDS),DYQ(NCQOBS,NQLDS),
     +           QA(NCQOBS,NQLDS),QD(NCQOBS,NQLDS,3),
     +           QDIRA(NCQOBS,NQLDS))!EFa jun09, added for qdir in external file
        NCLIN=0
        NHY=0
        TA=0.
        ILAYRQ=0
        IYDAT=0
        DYQ=0.
        QA=0.
        QDIRA=0. !EFa jun09, added for qdir in external file
        QD=0.
        ITIMEH=1
      ENDIF
      IF(NHYD .EQ. 0) THEN
c
c     set starting time in hours of the year
c     teth contains the first time step

        TSTARTS=(DAYNOW-1)*24.+TIME-TETH

  100   READ(IQUNIT,'(A8,A72)') ID,HTITLE
cipk nov97        READ(IQUNIT,'(A8,A72)') ID,DLIN
        call ginpt(iqunit,id,dlin)
        IF(ID(1:3) .EQ. 'CLQ') THEN
  101     NHYD=NHYD+1
	    IF(NHYD .GT. NQLDS) THEN
            CLOSE(75)
            OPEN(75,FILE='ERROR.OUT')
	      WRITE(75,*) 'ERROR STOP TOO MANY INFLOWS'
	      STOP 'ERROR STOP TOO MANY INFLOWS'
	    ENDIF
          READ(DLIN,'(2I8)') NCLIN(NHYD),IYDAT(NHYD)
          IYD=IYDAT(NHYD)
          DO 120 I=1,NCQOBS
cipk nov97            READ(IQUNIT,'(A8,A72)') ID,DLIN
            call ginpt(iqunit,id,dlin)
            IF(ID(1:2) .EQ. 'QD') THEN
              READ(ID(5:8),'(F4.0)') DYQ(I,NHYD)
              READ(DLIN,'(F8.0,I8,4F8.2,F8.2)') !EFa jun09, added  f8.2 for adding qdir in external file,changed 4F8.2 to 4F8.0 (for SMALL Q)
     +        TA(I,NHYD),ILAYRQ(I,NHYD),QA(I,NHYD),(QD(I,NHYD,K),K=1,3),
     +        QDIRA(I,NHYD) !EFa jun09, added for qdir in external file
              READ(ID(5:8),'(F4.0)') DYQ(I,NHYD) 
              NHY(NHYD)=NHY(NHYD)+1
              IF(I .EQ. 1) THEN
C
C      reduce input time to time since that set to start simulation
C
  110           CONTINUE
                IF(MOD(IYD,4) .EQ. 0) THEN
                  ILP=1
                ELSE
                  ILP=0
                ENDIF
                IF(IYD .EQ. IYRR) THEN
C
C      If now for for the same year
C
                  TCUR1=(DYQ(I,NHYD)-1.)*24.+TA(I,NHYD)
C
C      set time as the difference
C
                  TA(I,NHYD)=TCUR1-TPRVH-TSTARTS
                ELSEIF(IYD .LT. IYRR) THEN
                  IF(MOD(IYD,4) .EQ. 0) THEN
                    TPRVH=TPRVH+366.*24.
                  ELSE
                    TPRVH=TPRVH+365.*24.
                  ENDIF
                  IYD=IYD+1
                  GO TO 110
                ELSE
                  CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                  WRITE(*,*) ' Hydrograph for wrong year'
                  WRITE(*,*)  ' Excution stopped'
                  WRITE(75,*) ' Hydrograph for wrong year'
                  WRITE(75,*)  ' Excution stopped'
                  STOP
                ENDIF
              ELSE
                IF(DYQ(I,NHYD) .LT. DYQ(I-1,NHYD)) THEN
                  TCUR1=TCUR1-365.*24.
                  IF(ILP .EQ. 1) TCUR1=TCUR1-24.
                  IYD=IYD+1
                  IF(MOD(IYD,4) .EQ. 0) THEN
                    ILP=1
                  ELSE
                    ILP=0
                  ENDIF
                ENDIF
                TCUR=(DYQ(I,NHYD)-1.)*24.+TA(I,NHYD)
                TA(I,NHYD)=TA(I-1,NHYD)+TCUR-TCUR1
                TCUR1=TCUR
              ENDIF
cipk jun98   replace CLH with CLQ
            ELSEIF(ID(1:3) .EQ. 'CLQ') THEN
              NHY(NHYD)=NHY(NHYD)+1
              DYQ(NHY(NHYD),NHYD)=1.E+6
              TA(NHY(NHYD),NHYD)=1.E+8
              QA(NHY(NHYD),NHYD)=QA(NHY(NHYD)-1,NHYD)
              !EFa jun09, added for qdir in external file
              QDIRA(NHY(NHYD),NHYD)=QDIRA(NHY(NHYD)-1,NHYD) 
              !-
              ILAYRQ(NHY(NHYD),NHYD)=ILAYRQ(NHY(NHYD)-1,NHYD)
              DO K=1,3
                QD(NHY(NHYD),NHYD,K)=QD(NHY(NHYD)-1,NHYD,K)
              ENDDO
              GO TO 101
            ELSE
              NHY(NHYD)=NHY(NHYD)+1
              DYQ(NHY(NHYD),NHYD)=1.E+6
              TA(NHY(NHYD),NHYD)=1.E+8
              QA(NHY(NHYD),NHYD)=QA(NHY(NHYD)-1,NHYD)
              !EFa jun09, added for qdir in external file
              QDIRA(NHY(NHYD),NHYD)=QDIRA(NHY(NHYD)-1,NHYD)
              !- 
              ILAYRQ(NHY(NHYD),NHYD)=ILAYRQ(NHY(NHYD)-1,NHYD)
              DO K=1,3
                QD(NHY(NHYD),NHYD,K)=QD(NHY(NHYD)-1,NHYD,K)
              ENDDO
              GO TO 200
            ENDIF
  120     CONTINUE
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'More than dimensioned lines in hydrograph'
          WRITE(*,*) 'Execution terminated'
          WRITE(75,*) 'More than dimensioned lines in hydrograph'
          WRITE(75,*) 'Execution terminated'
          stop
        ENDIF
      ENDIF
  200 CONTINUE
      CLOSE (IQUNIT)

C
C     INTERPOLATE TO OBTAIN HYDROGRAPH
C
      CTIM=TETH
      DO 300 J=1,NHYD
        IF(N .EQ. NCLIN(J)) THEN
          DO 260 I=2,NHY(J)
            TIM=TA(I-1,J)
            TIN=TA(I,J)
            IF(CTIM .LT. TIN) THEN
              QF=QA(I-1,J)+(QA(I,J)-QA(I-1,J))*
     +        (CTIM-TIM)/(TIN-TIM)
              !EFa jun09, added for qdir in external file
              QDIR=QDIRA(I-1,J)+(QDIRA(I,J)-QDIRA(I-1,J))*
     +        (CTIM-TIM)/(TIN-TIM)
              !-
              DO K=1,3
                QQAL(K)=QD(I-1,J,K)+(QD(I,J,K)-QD(I-1,J,K))*
     +          (CTIM-TIM)/(TIN-TIM)
              ENDDO
              ILAYR = ILAYRQ(I-1,J)
CIPK MAR01   ADD FOR POWER STATION RECYCLING
              IF(NOUTCC(N) .NE. 0) THEN
	          IF(AVES(NOUTCC(N)) .GT. -100.) QQAL(1)=AVES(NOUTCC(N))
     +		  *(1.+ADDSAL(N))
 	          IF(NADTYP(N) .EQ. 0) THEN
	            IF(AVET(NOUTCC(N)) .GT. -100.) THEN
                    TINC=1000.*ADDTMP(N,1)/(ADDTMP(N,3)*
     +		  	  (1000.-0.0178*(ABS(AVET(NOUTCC(N))-4.0))**1.7)*
     +              QF*4.19)
	              IF(TINC .GT. ADDMAX(N)) TINC=ADDMAX(N)
		  	      QQAL(2)=AVET(NOUTCC(N))+TINC
	WRITE(75,*) 'POWER',N,ADDTMP(N,1),ADDTMP(N,3),QF,TINC
                  ENDIF
                ELSEIF(NADTYP(N) .EQ. 1) THEN
                  QQAL(2)=AVET(NOUTCC(N))+QQAL(2)
	          ENDIF
	          IF(AVESD(NOUTCC(N)) .GT. -100.) QQAL(3)=AVESD(NOUTCC(N))
     +          *(1.+ADDSED(N))
	        ENDIF
                            
C
              RETURN
            ENDIF
  260     CONTINUE
        ENDIF
  300 CONTINUE
 4800 FORMAT(A32)
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      write(*,*) 'Unable to interpolate data value from hydrograph'
      write(75,*) 'Unable to interpolate data value from hydrograph'
      STOP
      END
