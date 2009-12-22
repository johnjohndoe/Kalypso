CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE SEP 6 2004 RENAME FILE ENTRY NAME
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
CIPK  LAST UPDATE DEC 16 1997
cipk  last update Nov 26 1997
CIPK  LAST UPDATE NOV 20 1997 CHANGE TO DEFINE LN
cipk  last update April 27 1996  many changes
cipk  last update Jan 23 1996
cipk  last update jan 9 1996
      SUBROUTINE GETEQ(N,QF,QQAL,IYRR,DAYNOW,TIME,TETH,NESU,LN)
      USE BLK11MOD
      USE PARAMMOD
      use blk_ifu
      SAVE
cipk nov97 note danger, do not add BLK10.COM

!NiS,jul06: Add IWINDIN for correct memory interpretation
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,INWINDIN
!-

CIPK DEC97  MOVE TO PARAM.COM      PARAMETER (NELDS=20,NDPTS=200)
      ALLOCATABLE NCLIN(:),NHY(:),TA(:,:),HT(:,:)
     +         ,DYQ(:,:),IYDAT(:),NEST(:)
     +         ,HD(:,:,:),ILAYRH(:,:)
CIPK SEP04
      CHARACTER*32 FNAMT
      CHARACTER*8 ID
      CHARACTER*72 QHTITLE,DLIN
      DIMENSION QQAL(*)
      DATA NQHYD/0/,ITIMEH/0/
      IF(IQEUNIT .EQ. 0) THEN
        WRITE(*,*) 'Filename for element inflow missing'
        WRITE(*,*) 'Enter filename for element inflows'
CIPK SEP04
        READ(*,4800) FNAMT
        IQEUNIT=14
        OPEN(UNIT=14,FILE=FNAMT,STATUS='OLD')
      ENDIF
      IF(ITIMEH .EQ. 0) THEN
        ALLOCATE(NCLIN(NELDS),NHY(NELDS),TA(NDPTS,NELDS),HT(NDPTS,NELDS)
     +         ,DYQ(NDPTS,NELDS),IYDAT(NELDS),NEST(NELDS)
     +         ,HD(NDPTS,NELDS,3),ILAYRH(NDPTS,NELDS))
        NCLIN=0
        NHY=0
        TA=0.
        HT=0.
        DYQ=0.
        IYDAT=0
        NEST=0
        HD=0.
        ILAYRH=0
        ITIMEH=1
      ENDIF
      IF(NQHYD .EQ. 0) THEN
c
c     set starting time in hours of the year
c     teth contains the first time step

        TSTARTS=(DAYNOW-1)*24.+TIME-TETH
      
  100   READ(IQEUNIT,'(A8,A72)') ID,QHTITLE
cipk nov97        READ(IQEUNIT,'(A8,A72)') ID,DLIN
        call ginpt(iqeunit,id,dlin)
        IF(ID(1:3) .EQ. 'QEI') THEN
  101     NQHYD=NQHYD+1
          NHY(NQHYD)=0
          READ(DLIN,'(3I8)') NCLIN(NQHYD),NEST(NQHYD),IYDAT(NQHYD)
          IF(NCLIN(NQHYD) .EQ. 0) NCLIN(NQHYD)=-9999
c
          IYD=IYDAT(NQHYD)
          DO 120 I=1,ndpts-1
cipk nov97            READ(IQEUNIT,'(A8,A72)') ID,DLIN
            call ginpt(iqeunit,id,dlin)
            IF(ID(1:3) .EQ. 'QE ') THEN
cipk jan96 add day of year to logic
              READ(ID(5:8),'(F4.0)') DYQ(I,NQHYD)
              READ(DLIN,'(F8.0,I8,4F8.0)')
     +        TA(I,NQHYD),ILAYRH(I,NQHYD),HT(I,NQHYD)
     +      ,(HD(I,NQHYD,K),K=1,3)
              NHY(NQHYD)=NHY(NQHYD)+1
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
                  TCUR1=(DYQ(I,NQHYD)-1.)*24.+TA(I,NQHYD)
C
C      set time as the difference
C
                  TA(I,NQHYD)=TCUR1-TPRVH-TSTARTS
                ELSEIF(IYD .LT. IYRR) THEN
                  IF(MOD(IYD,4) .EQ. 0) THEN
                    TPRVH=TPRVH+366.*24.
                  ELSE
                    TPRVH=TPRVH+365.*24.
                  ENDIF
                  IYD=IYD+1
                  GO TO 110
                ELSE
                  WRITE(*,*) ' Element inflows for wrong year'
CIPK SEP04 CREATE ERROR FILE
                  CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                  WRITE(*,*)  ' Execution stopped'
                  WRITE(75,*) ' Element inflows for wrong year'
                  WRITE(75,*)  ' Excution stopped'
                  STOP
                ENDIF
              ELSE
                IF(DYQ(I,NQHYD) .LT. DYQ(I-1,NQHYD)) THEN
                  TCUR1=TCUR1-365.*24.
                  IF(ILP .EQ. 1) TCUR1=TCUR1-24.
                  IYD=IYD+1
                  IF(MOD(IYD,4) .EQ. 0) THEN
                    ILP=1
                  ELSE
                    ILP=0
                  ENDIF
                ENDIF
                TCUR=(DYQ(I,NQHYD)-1.)*24.+TA(I,NQHYD)
                TA(I,NQHYD)=TA(I-1,NQHYD)+TCUR-TCUR1
                TCUR1=TCUR
              ENDIF
            ELSEIF(ID(1:3) .EQ. 'QEI') THEN
              NHY(NQHYD)=NHY(NQHYD)+1
cipk jan96 add day of year to logic
              DYQ(NHY(NQHYD),NQHYD)=1.E+6
              TA(NHY(NQHYD),NQHYD)=1.E+8
              HT(NHY(NQHYD),NQHYD)=HT(NHY(NQHYD)-1,NQHYD)
              DO K=1,3
                HD(NHY(NQHYD),NQHYD,K)=HD(NHY(NQHYD)-1,NQHYD,K)
              ENDDO
              GO TO 101
            ELSE
              NHY(NQHYD)=NHY(NQHYD)+1
              DYQ(NHY(NQHYD),NQHYD)=1.E+6
              TA(NHY(NQHYD),NQHYD)=1.E+8
              HT(NHY(NQHYD),NQHYD)=HT(NHY(NQHYD)-1,NQHYD)
              DO K=1,3
                HD(NHY(NQHYD),NQHYD,K)=HD(NHY(NQHYD)-1,NQHYD,K)
              ENDDO
              GO TO 200
            ENDIF
  120     CONTINUE
CIPK SEP04 CREATE ERROR FILE
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*)
     +   'Execution terminated more lines than allowed in element graph'
          WRITE(75,*)
     +   'Execution terminated more lines than allowed in element graph'
          stop
        ENDIF
      ENDIF
  200 CONTINUE
C
C     INTERPOLATE TO OBTAIN HYDROGRAPH
C
cipk jan96 add day of year to logic
      CTIM=TETH
      DO 300 J=1,NQHYD
        IF(N .EQ. NCLIN(J)) THEN
          DO 260 I=2,NHY(J)
            TIM=TA(I-1,J)
            TIN=TA(I,J)
            IF(CTIM .LT. TIN) THEN
cipk nov97 define frac to determine  LN
              FRAC=(CTIM-TIM)/(TIN-TIM)
              QF=HT(I-1,J)+(HT(I,J)-HT(I-1,J))*FRAC
CIPK NOV97     +        (CTIM-TIM)/(TIN-TIM)
              IF(FRAC .LT. 0.5) THEN
                LN=ILAYRH(I-1,J)
              ELSE
                LN=ILAYRH(I,J)
              ENDIF
              DO K=1,3
                QQAL(K)=HD(I-1,J,K)+(HD(I,J,K)-HD(I-1,J,K))*FRAC
CIPK NOV97     +          (CTIM-TIM)/(TIN-TIM)
              ENDDO
              NESU=NEST(J)
              RETURN
            ENDIF
  260     CONTINUE
        ENDIF
  300 CONTINUE
 4800 FORMAT(A32)
CIPK SEP04 CREATE ERROR FILE
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
      write(*,*) 'Unable to interpolate data value from element inflow'
     +, ' hydrograph'
      write(75,*) 'Unable to interpolate data value from element inflow'
     +, ' hydrograph'
      STOP
      END
