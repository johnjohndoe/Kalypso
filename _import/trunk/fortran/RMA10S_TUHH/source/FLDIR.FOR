CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
      SUBROUTINE FLDIR
      USE BLK10MOD
      SAVE
C-
C...... Routine to establish flow direcions for one- dimensional elements
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      DIMENSION ITAB(MNP)
C-
C...... Analyse the elements for connections
C-
      !NiS,may06m,com: initialize the ITAB-array
      DO 180 N=1,NPM
        ITAB(N)=0
  180 CONTINUE

      !NiS,may06,com: 1D-elements or control-structure elements (IGTP.ne.0 gives "GATE TYPE")
      DO 220 M=1,NEM
        IF(IMAT(M) .GT. 0) THEN

CIPK DEC00 ALLOW FOR GATE STRUCTURE

          IF(IMAT(M) .LT. 900  .OR.  IMAT(M) .GT. 5000  .OR.
     +       IGTP(M) .NE. 0) THEN
            IF(NOPS(M,6) .EQ. 0) THEN
              DO 200 K=1,3,2
                N1=NOPS(M,K)
                ITAB(N1)=M
  200         CONTINUE
            ENDIF
          ENDIF
        ENDIF
  220 CONTINUE

      !NiS,may06,com: special control-structure elements
      DO 500 N=1,NEM
        IF(IMAT(N) .GT. 900  .AND.  IMAT(N) .LT. 1000) THEN
C-
C...... Select a node to establish direction
C-
          DO 400 K=1,8
            NRF=NOPS(N,K)
            IF(NRF .EQ. 0) GO TO 450
C-
C...... Search other elements for junction nodes
C-
            M=ITAB(NRF)
CIPK DEC00 ALLOW STRUCTURE ELEMENTS
            IF(M .EQ. 0  .AND.
     +      (IMAT(N) .GT. 903  .AND.  IMAT(N) .LT. 1000)) GO TO 400
            IF(M .EQ. 0  .OR.  M .GT. NE) GO TO 700
                DO 250 L=1,3,2
                  IF(NOPS(M,L) .EQ. NRF) THEN
C-
C...... When match is found get slope with respect to 's'
C-
                    L2=NOPS(M,2)
                    IF(L .EQ. 1) THEN
                      L3=NOPS(M,3)
                    ELSE
                      L3=NOPS(M,1)
                    ENDIF
                    DX=(CORD(L3,1)-CORD(NRF,1))/2.
                    DY=(CORD(L3,2)-CORD(NRF,2))/2.
                    IF(ABS(DX) .GT. ABS(DY)) THEN
                      IF(DX .LT. 0.) THEN
                        DIR(NRF)=-1.
                      ELSE
                        DIR(NRF)=1.
                      ENDIF
                    ELSE
                      IF(DY .LT. 0.) THEN
                        DIR(NRF)=-1.
                      ELSE
                        DIR(NRF)=1.
                      ENDIF
                      IF(TF(ALFA(NRF)) .LT. 0.) DIR(NRF)=-DIR(NRF)
                    ENDIF
                  ENDIF
  250           CONTINUE
  400     CONTINUE
  450     CONTINUE
        ENDIF
  500 CONTINUE

      !NiS,may06,com: für jeden 2D-horizontal averaged elements; alle Knoten mit mehr als einem Layer (NDEP(N).gt.1)
      DO 600 N=1,NPM
        IF(NDEP(N) .GT. 1) THEN
          DO 570 K=2,NDEP(N)
            L=NREF(N)+K-1
            DIR(L)=DIR(N)
  570     CONTINUE
        ENDIF
  600 CONTINUE
      RETURN
  700 WRITE(*,720) NRF,M,(N,ITAB(N),N=1,NP)
  720 FORMAT('  ERROR IN ELEMENT CONNECTIONS DETECTED IN FLDIR'/
     +       '  MATCHING NODE',I6,'  ELEMENT APPARENTLY',I5/
     +       '  TABLE OF ELEMENTS CONNECTED TO NODES AS FOLLOWS'/
     +       5(I8,I6))
CIPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(75,720) NRF,M,(N,ITAB(N),N=1,NP)
      STOP
      END
