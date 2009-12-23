!IPK  LAST UPDATE SEP 6 2004 RENAME character variable FNAM to FNAMT
      SUBROUTINE XRED(ND1,IRTC,NRR)
      USE CHR1MOD
      USE BLK10
      SAVE
!-
!.......   Subroutine to read data from mass storage
!-
      DATA  IPASS/0/
      IF(IPASS == 0) THEN
        MAXFIL=0
        IPASS=1
      ENDIF
!-
      IF(IVRSID == 4 .OR. IVRSID == 6) THEN
!
!    Read sequential file
!
        WRITE(TSUB,3000) NRR
 3000   FORMAT(I3.3)
        CLOSE (ND1)
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
        REWIND ND1
        READ(ND1) LQ,LHS,QS
        REWIND ND1
        NRR=NRR-1
!
      ELSE
!
        READ(ND1,REC=NRR) LQ,LHS,QS
        NRR = NRR - 1
      ENDIF
      RETURN
      END
