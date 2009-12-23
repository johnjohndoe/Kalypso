!IPK  LAST UPDATE SEP 6 2004 RENAME character variable FNAM to FNAMT
!
      SUBROUTINE XWRT(ND1,N,NRR)
      USE CHR1MOD
      USE BLK10
      SAVE
!
      LOGICAL EX
      DATA IPASS/0/
        IF(IPASS == 0) MAXFIL=0
!-
      IF(IVRSID < 4 .OR. IVRSID == 5) THEN
        IF(IPASS == 0) THEN
          IF(IVRSID == 1) THEN
!
!       NSIZ in bytes
!
            NSIZ=8*NBS+4
!
          ELSEIF(IVRSID == 2) THEN
!
!       NSIZ for short words (2 bytes)
!
            NSIZ=4*NBS+2
!
          ELSEIF(IVRSID == 3) THEN
!
!       NSIZ for long words (4 bytes)
!
            NSIZ=2*NBS+1
!
          ELSE
!
!       NSIZ for 64 bit machines recording record size in bytes
!
            NSIZ=16*NBS+8
          ENDIF
!
          OPEN(ND1,STATUS='SCRATCH',ACCESS='DIRECT',                    &
     &    FORM='UNFORMATTED',RECL=NSIZ)
          IPASS=1
          NRR=0
        ENDIF
        IF( N == 0 ) RETURN
        NRR=NRR+1
        WRITE(ND1,REC=NRR) LQ,LHS,QS
!
      ELSE
        IF(IPASS == 0) THEN
!
!    Develop a name for the scratch file
!
        DO 300 NSCT=1,999
          WRITE(TNUM,3001) NSCT
 3001     FORMAT(I3.3)
          FHED='H'//TNUM(1:3)
          IF(IVRSID == 4) THEN
!IPK SEP04
            FNAMT=FHED//'.001'
            INQUIRE(FILE=FNAMT,EXIST=EX)
          ELSE
            FNBM=FHED//'001'
            INQUIRE(FILE=FNBM,EXIST=EX)
          ENDIF
          IF(EX) GO TO 300
          GO TO 301
  300   CONTINUE
!ipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(75,3002)
        WRITE(*,3002)
 3002   FORMAT(' UNABLE TO OPEN SCRATCH FILE WITH SUITABLE NAME'/       &
     &  ' EXECUTION TERMINATED')
        STOP
  301   CONTINUE
        IPASS=1
        WRITE(*,3003) FHED
 3003   FORMAT( ' HEADER ON SCRATCH FILE IS',3X,A4)
!
!    Open temporary sequential file
!
        ENDIF
        NRR=NRR+1
        WRITE(TSUB,3000) NRR
 3000   FORMAT(I3.3)
        IF(MAXFIL < NRR) MAXFIL=NRR
        IF(IVRSID == 4) THEN
!IPK SEP04
          FNAMT=FHED//'.'//TSUB(1:3)
          IF(NRR > 1 .OR. IPASS > 0)   CLOSE (ND1)
          OPEN(ND1,FILE=FNAMT,STATUS='UNKNOWN',                         &
     &    FORM='UNFORMATTED')
        ELSE
          FNBM=FHED//TSUB(1:3)
          IF(NRR > 1 .OR. IPASS > 0)   CLOSE (ND1)
          OPEN(ND1,FILE=FNBM,STATUS='UNKNOWN',                          &
     &    FORM='UNFORMATTED')
        ENDIF
!
        IF( N == 0 ) RETURN
!
        WRITE(ND1) LQ,LHS,QS
!
      ENDIF
      RETURN
      END
