C     Last change:  WP    5 May 2008   10:46 am
CIPK  LAST UPDATE MAR 29 2005  RESET CORRECTION FOR CONSISTENCY WITH RMA2
cipk  last update Mar 13 1998   fix initialization for no marsh case
CIPK  NEW ROUTINE NOVEMBER 13 1997
      SUBROUTINE MARSH
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      SAVE
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK APR96 ADD BLK11
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'

C
C     Initialize marsh values
C
      IF(IDNOPT .EQ. 0) THEN
C
C     IDNOPT = 0  No marsh
C
        DO 5037 N=1,NP
cipk mar98          ADB(N)=AO(N)
cipk mar98          ADT(N)=AO(N)
          ADB(N)=0.
          ADT(N)=0.
          AKP(N)=1.0
          ADO(N)=AO(N)
 5037   CONTINUE
        WRITE(LOUT,6170)
 6170   FORMAT(//'  MARSH ELEMENTS INOPERATIVE')
      ELSE
        IF(IDNOPT .EQ. -1) THEN
C
C     IDNOPT = -1  use default
C
          AC3=0.04
          IF(GRAV .LT. 10.) THEN
            AC1=1.5
            AC2=0.67
          ELSE
            AC1=4.5
            AC2=2.
          ENDIF
          WRITE(LOUT,6171) AC1,AC2,AC3
 6171   FORMAT(//'  DEFAULT VALUES FOR MARSH ELEMENTS USED',
     +  /  '      DEPTH SHIFT', F8.2
     +  /  '      RANGE      ', F8.2
     +  /  '      KAPPA      ', F8.2)
        ELSE
C
C     IDNOPT = -2  Read default/ individual marsh values
C

cipk apr97 fix to read MP correctly
          IF(ID(1:3) .EQ. 'MP ') THEN
            READ(DLIN,5030) J,AC1,AC2,AC3,AC4
 5030       FORMAT(I8,4F8.0)
            call ginpt(lin,id,dlin)
            NDATLN=NDATLN+1
            WRITE(LOUT,6172) AC1,AC2,AC3,AC4
 6172     FORMAT(//'  INPUT VALUES FOR MARSH ELEMENTS USED',
     +    /  '      DEPTH SHIFT', F8.2
     +    /  '      RANGE      ', F8.2
     +    /  '      KAPPA      ', F8.2
     +    /, '      ABS A0     ', F8.2)
          ELSE
C
C       Define baseline defaults for no MP line
C
            AC3=0.04
            IF(GRAV .LT. 10.) THEN
              AC1=1.5
              AC2=0.67
            ELSE
              AC1=4.5
              AC2=2.
            ENDIF
          ENDIF
        ENDIF

        DO N=1,NP
C
C..... Define ADB temporarily as elevation at bottom of range
C..... Define ADT temporarily as elevation at top of range
C..... Define AKP as equivalent porosity
C..... Define ADO as elevation at bottom of slot
C
          ADB(N)=AO(N)-AC2/2.
          ADT(N)=ADB(N)+AC2
          AKP(N)=AC3
          IF (AC1 .EQ. 0.0) THEN
            ADO(N) = AC4
          ELSE
            ADO(N) = ADB(N) - AC1
          ENDIF
CIPK MAR05
          ACOR(N)=AO(N)-ADO(N)
        ENDDO
 5141   CONTINUE
        IF(ID(1:3) .EQ. 'MP1') THEN
          WRITE(LOUT,6173)
 6173     FORMAT(//'  OVERRIDING PARAMETERS USED FOR MARSH ELEMENTS'//
     +'      NODE   DEPTH-SHIFT',9X,'RANGE',9X,'KAPPA',8X,'ABS A0'/)
          READ(DLIN,5030) J,AC1,AC2,AC3,AC4
          call ginpt(lin,id,dlin)
          NDATLN=NDATLN+1
          WRITE(LOUT,6174) J,AC1,AC2,AC3,AC4
 6174     FORMAT(I10,4F14.2)
          ADB(J)=AO(J)-AC2/2.

          !TOASK: Why is N instead of J used here?
          !ADT(N)=ADB(N)+AC2
          ADT(J)=ADB(J)+AC2

          AKP(J)=AC3
          IF (AC1 .EQ. 0.0) THEN
            ADO(J) = AC4
          ELSE
            ADO(J) = ADB(J) - AC1
          ENDIF
CIPK MAR05
          ACOR(J)=AO(J)-ADO(J)

          GO TO 5141
        ENDIF
C
C...... Loop around nodes of an element to set ADO so that for an element
C       that is below adjacent AO
C
        DO 5043 J=1,NE
          IF(IMAT(J) .NE. 0) THEN
            NCN=NCORN(J)
          IF(IMAT(J) .GT. 900) GO TO 5043
          IF(NCN .EQ. 5) NCN=3
          IF(NCN .EQ. 9) NCN=8
            DO K=1,NCN,2
              N=NOP(J,K)
              DO  L=1,NCN,2
                M=NOP(J,L)
CIPK MAR05
                IF(ADO(N) .GT. AO(M)-ACOR(M)) ADO(N)=AO(M)-ACOR(M)
              ENDDO
            ENDDO
          ENDIF
 5043   CONTINUE
C
C...... Now loop to set final values
C
        DO N=1,NP
          RANGE=ADT(N)-ADB(N)
          IF(AKP(N) .LT. 0.95) THEN
             ADB(N)=ADB(N)+(ADT(N)-ADO(N)
     +             -RANGE/2.)*AKP(N)/(1.-AKP(N))
C
C             This adjusts ADB to account for storage below
C
          ENDIF
C
C..... Now adjust to make ADB and ADT depths
C
          ADB(N) = ADB(N) - ADO(N)
          ADT(N) = ADB(N) + RANGE
        ENDDO
      ENDIF
      RETURN
      END

