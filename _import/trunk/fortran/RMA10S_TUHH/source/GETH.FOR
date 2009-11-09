CIPK  LAST UPDATE SEP 6 2004 RENAME FILE ENTRY NAME  add error file
C     Last change:  IPK  19 Sep 2000   11:29 am
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
CIPK  MAST UPDATE AUG 1 2000 ADD SECOND ELEVATION HF2,FOR INTERPOLATION
CIPK  LAST UPDATE DEC 16 1997
cipk  lst update Nov 26 1997
cipk  last update April 26 1996  many changes
      !EFa oct09, hfd
      !SUBROUTINE GETH(LNO,N,HF,HF2,IYRR,DAYNOW,TIME,TETH,QQAL)
      SUBROUTINE GETH(LNO,N,HF,HF2,IYRR,DAYNOW,TIME,TETH,QQAL,hfd)
      !-
      USE PARAMMOD
      USE BLK11MOD
      use blk_ifu
      SAVE
CIPK AUG05      INCLUDE 'PARAM.COM'
cipk nov97  do not include BLK10.COM
cipk dec97 move to param.com      PARAMETER (NCHBS=10,NCHOBS=200)

!NiS,jul06: Add IWINDIN for correct memory interpretation
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,INWINDIN
!-

CIPK AUG05      INCLUDE 'BLK11.COM'
C
CIPK DEC97 CHANGE LIMIT TO NHDS
      ALLOCATABLE NCLIN(:),NHY(:),TA(:,:),
     +          HT(:,:),HD(:,:,:),
     +          ILAYRH(:,:)
     +         ,DYQ(:,:),IYDAT(:)
     +         ,HT2(:,:)
      !EFa oct09, added htd for hfd
     +         ,htd(:,:)
      !-

      DIMENSION QQAL(*)
CIPK SEP04
      CHARACTER*32 FNAMT
!NiS,jul06: Adjust length of string variable for proper parameter passing
!      CHARACTER*80 HTITLE,DLIN
      CHARACTER*80 HTITLE
      CHARACTER*72 DLIN
!-
      CHARACTER*8 ID
      DATA NHYD/0/,ITIMEH/0/
C      WRITE(*,*) 'N,TIME',N,TIME
      IF(IHUNIT .EQ. 0) THEN
        WRITE(*,*) 'Filename for tidalgraphs not defined'
        WRITE(*,*) 'Enter filename for continuity line tidalgraphs'
CIPK SEP04
        READ(*,4800) FNAMT
        IHUNIT=12
        OPEN(UNIT=12,FILE=FNAMT,STATUS='OLD')
      ENDIF
      IF(ITIMEH .EQ. 0) THEN
        ALLOCATE (NCLIN(NHDS),NHY(NHDS),TA(NCHOBS,NHDS),
     +          HT(NCHOBS,NHDS),HD(NCHOBS,NHDS,3),
     +          ILAYRH(NCHOBS,NHDS)
     +         ,DYQ(NCHOBS,NHDS),IYDAT(NHDS)
      !EFa oct09, added htd for hfd
      !+         ,HT2(NCHOBS,NHDS))
     +         ,HT2(NCHOBS,NHDS),htd(nchobs,nhds))
      !-
        ITIMEH=1
        NCLIN=0
        NHY=0
        TA=0.
        HT=0.
        HD=0.
        ILAYRH=0
        DYQ=0.
        IYDAT=0
        HT2=0.
        !EFa oct09, added htd for hfd
        htd = 0.
        !-
      ENDIF
      IF(NHYD .EQ. 0) THEN
c
c     set starting time in hours of the year
c     teth contains the first time step

        TSTARTS=(DAYNOW-1)*24.+TIME-TETH
  100   READ(IHUNIT,'(A8,A72)') ID,HTITLE
cipk nov97        READ(IHUNIT,'(A8,A72)') ID,DLIN
        call ginpt(ihunit,id,dlin)
        IF(ID(1:3) .EQ. 'CLH') THEN
  101     NHYD=NHYD+1
CIPKSEP00 TEST FOR LIMIT
          IF(NHYD .GT. NHDS) THEN
cipk sep04
            CLOSE(75)
            OPEN(75,FILE='ERROR.OUT')
            WRITE(75,*) 'ERROR STOP TOO MANY TIDAL GRAPHS'
            STOP 'ERROR STOP TOO MANY TIDAL GRAPHS'
          ENDIF
          NHY(NHYD)=0
          READ(DLIN,'(2I8)') NCLIN(NHYD),IYDAT(NHYD)
c
          IYD=IYDAT(NHYD)
          DO 120 I=1,NCHOBS
cipk nov97            READ(IHUNIT,'(A8,A72)') ID,DLIN
            call ginpt(ihunit,id,dlin)
            IF(ID(1:2) .EQ. 'HD') THEN
              READ(ID(5:8),'(F4.0)') DYQ(I,NHYD)
              !EFa oct09, added htd for hfd
              !READ(DLIN,'(F8.0,I8,5F8.0)')
              READ(DLIN,'(F8.0,I8,6F8.0)')
     +        TA(I,NHYD),ILAYRH(I,NHYD),HT(I,NHYD),(HD(I,NHYD,K),K=1,3)
      !+        ,HT2(I,NHYD)
     +        ,HT2(I,NHYD),htd(i,nhyd)
     
CIPK AUG00 ADD HT2
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
                  WRITE(*,*) ' Tidal graph for wrong year'
                  WRITE(*,*)  ' Excution stopped'
                  WRITE(75,*) ' Tidal graph for wrong year'
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
            ELSEIF(ID(1:3) .EQ. 'CLH') THEN
              NHY(NHYD)=NHY(NHYD)+1
              DYQ(NHY(NHYD),NHYD)=1.E+6
              TA(NHY(NHYD),NHYD)=1.E+8
              HT(NHY(NHYD),NHYD)=HT(NHY(NHYD)-1,NHYD)
CIPK AUG00
              HT2(NHY(NHYD),NHYD)=HT2(NHY(NHYD)-1,NHYD)
              !EFa oct09, added htd for hfd
              htd(NHY(NHYD),NHYD)=htd(NHY(NHYD)-1,NHYD)
              !-
              DO K=1,3
                HD(NHY(NHYD),NHYD,K)=HD(NHY(NHYD)-1,NHYD,K)
              ENDDO
              GO TO 101
            ELSE
              NHY(NHYD)=NHY(NHYD)+1
              DYQ(NHY(NHYD),NHYD)=1.E+6
              TA(NHY(NHYD),NHYD)=1.E+8
              HT(NHY(NHYD),NHYD)=HT(NHY(NHYD)-1,NHYD)
CIPK AUG00
              HT2(NHY(NHYD),NHYD)=HT2(NHY(NHYD)-1,NHYD)
              !EFa oct09, added htd for hfd
              htd(NHY(NHYD),NHYD)=htd(NHY(NHYD)-1,NHYD)
              !-
              DO K=1,3
                HD(NHY(NHYD),NHYD,K)=HD(NHY(NHYD)-1,NHYD,K)
              ENDDO
              GO TO 200
            ENDIF
  120     CONTINUE
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'More than dimensioned lines in tidal graph'
          WRITE(*,*) 'Execution terminated'
          WRITE(75,*) 'More than dimensioned lines in tidal graph'
          WRITE(75,*) 'Execution terminated'
          stop
        ENDIF
      ENDIF
  200 CONTINUE
      CLOSE (IHUNIT)
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
              HF=HT(I-1,J)+(HT(I,J)-HT(I-1,J))*
     +        (CTIM-TIM)/(TIN-TIM)
CIPK AUG00
              HF2=HT2(I-1,J)+(HT2(I,J)-HT2(I-1,J))*
     +        (CTIM-TIM)/(TIN-TIM)
              !EFa oct09, added hfd
              hfd=htd(I-1,J)+(htd(I,J)-htd(I-1,J))*
     +        (CTIM-TIM)/(TIN-TIM)              
              !-
              DO K=1,3
                QQAL(K)=HD(I-1,J,K)+(HD(I,J,K)-HD(I-1,J,K))*
     +          (CTIM-TIM)/(TIN-TIM)
              ENDDO
              RETURN
            ENDIF
  260     CONTINUE
        ENDIF
  300 CONTINUE
 4800 FORMAT(A32)
      write(*,*) 'Unable to interpolate data value from tidal graph'
      write(75,*) 'Unable to interpolate data value from tidal graph'
      STOP
      END
