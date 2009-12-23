C     Last change:  NIS  14 May 2008    7:20 pm
CIPK  LAST UPDATE MAR 29 2005  RESET CORRECTION FOR CONSISTENCY WITH RMA2
cipk  last update Mar 13 1998   fix initialization for no marsh case
CIPK  NEW ROUTINE NOVEMBER 13 1997
      SUBROUTINE MARSH
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE ParaKalyps
      SAVE
C
C     Initialize marsh values
C
      IF (IDNOPT == 0) THEN
C
C     IDNOPT = 0  No marsh
C
        DO N = 1, NP
          ADB(N)=0.
          ADT(N)=0.
          AKP(N)=1.0
          ADO(N)=AO(N)
        ENDDO

        WRITE(LOUT,6170)
 6170   FORMAT(//'  MARSH ELEMENTS INOPERATIVE')


      ELSE
        IF(IDNOPT == -1) THEN
C
C     IDNOPT = -1  use default
C
          AC3=0.04
          IF(GRAV < 10.) THEN
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
          IF (ID (1:3) == 'MP ') THEN
            READ(DLIN,5030) J,AC1,AC2,AC3,AC4
 5030       FORMAT(I8,4F8.0)
            call ginpt(lin,id,dlin)
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
            IF(GRAV < 10.) THEN
              AC1=1.5
              AC2=0.67
            ELSE
              AC1=4.5
              AC2=2.
            ENDIF
          ENDIF
        ENDIF

        AssignValuesToNodes: DO N=1,NP


          !nis,may08: For 1D polynomial nodes this should not be applied
          if (IsPolynomNode (N)) then
            ADB(N)=0.
            ADT(N)=0.
            AKP(N)=1.0
            ADO(N)=AO(N)
          else
C..... Define ADB temporarily as elevation at bottom of range
C..... Define ADT temporarily as elevation at top of range
C..... Define AKP as equivalent porosity
C..... Define ADO as elevation at bottom of slot
C
            ADB(N)=AO(N)-AC2/2.
            ADT(N)=ADB(N)+AC2
            AKP(N)=AC3
            IF (AC1 == 0.0) THEN
              ADO(N) = AC4
            ELSE
              ADO(N) = ADB(N) - AC1
            ENDIF
          endif
CIPK MAR05
          ACOR(N)=AO(N)-ADO(N)
        ENDDO AssignValuesToNodes

        !READ INDIVIDUAL NODAL MARSH PARAMETERS
        !**************************************
        NodalIndividualParameters: DO
          !if no MP1 line is present, get out of here
          IF (ID (1:3) /= 'MP1') EXIT NodalIndividualParameters

          !Write header line
          WRITE(LOUT,6173)
 6173     FORMAT(//'  OVERRIDING PARAMETERS USED FOR MARSH ELEMENTS'//
     +'      NODE   DEPTH-SHIFT',9X,'RANGE',9X,'KAPPA',8X,'ABS A0'/)
          READ(DLIN,5030) J,AC1,AC2,AC3,AC4
          call ginpt(lin,id,dlin)

          !nis,may08: Ignore nodes from 1D polynomial approach
          if (IsPolynomNode (j)) then
            WRITE (LOUT, *) 'Node ', J, ' will be ignored, although' //
     +      'individual Marsh parameters are given to the node. ' //
     +      'Marsh algorithm inoperative for 1D polynomial nodes'
            CYCLE NodalIndividualParameters

          !otherwise write down the parameters to be set to a certain node
          else
            WRITE(LOUT,6174) J,AC1,AC2,AC3,AC4
 6174       FORMAT(I10,4F14.2)
          end if

          !Finally assign the values

          ADB (J) = AO (J) - AC2/ 2.
          !TOASK: Why is N instead of J used here?
          !ADT(N)=ADB(N)+AC2
          ADT (J) = ADB (J) + AC2
          AKP (J) = AC3

          IF (AC1 == 0.0) THEN
            ADO (J) = AC4
          ELSE
            ADO (J) = ADB(J) - AC1
          ENDIF
CIPK MAR05
          ACOR (J) = AO (J) - ADO (J)
        ENDDO NodalIndividualParameters


        !ADAPT DEACTIVATION DEPTH (ADO) WITHIN AN ELEMENT TO HIGHEST ELEVATION
        !*********************************************************************
C
C...... Loop around nodes of an element to set ADO so that for an element
C       that is below adjacent AO
C
        EqualizeBottomLevel: DO J = 1, NE
          IF(IMAT(J) /= 0) THEN
            NCN=NCORN(J)
          IF(IMAT(J) > 900) cycle EqualizeBottomLevel

          !nis,may08: nodes of polynomial approach are only in such elements; don't process those elements
          if (IsPolynomNode (nop (J, 1))) CYCLE EqualizeBottomLevel

          IF(NCN == 5) NCN=3
          IF(NCN == 9) NCN=8
            DO K=1,NCN,2
              N=NOP(J,K)
              DO  L=1,NCN,2
                M=NOP(J,L)
CIPK MAR05
                IF(ADO(N) > AO(M)-ACOR(M)) ADO(N)=AO(M)-ACOR(M)
              ENDDO
            ENDDO
          ENDIF
        ENDDO EqualizeBottomLevel

        !SET FINAL VALUES
        !****************
C
C...... Now loop to set final values
C
        SetFinalValues: DO N=1,NP

          !nis,may08: Don't consider 1D polynomial approach nodes
          if (IsPolynomNode (N)) CYCLE SetFinalValues

          TransitionRange = ADT (N) - ADB (N)
          IF(AKP(N) < 0.95) THEN
             ADB(N)=ADB(N)+(ADT(N)-ADO(N)
     +             - TransitionRange/ 2.) * AKP(N)/(1.-AKP(N))
C
C             This adjusts ADB to account for storage below
C
          ENDIF
C
C..... Now adjust to make ADB and ADT depths
C
          ADB(N) = ADB(N) - ADO(N)
          ADT(N) = ADB(N) + TransitionRange
        ENDDO SetFinalValues
      ENDIF
      RETURN
      END

