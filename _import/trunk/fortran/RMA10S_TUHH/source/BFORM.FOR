CIPK  LAST UPDATE JUNE 27 2005  ADDCONTROL STRUCTURES
cipk  last update Aug 14 1998   fix occasional bug when restarting
CIPK  LAST UPDATE NOV 12 1996
CIPK  LAST UPDATE SEPT 26 1996
      SUBROUTINE BFORM(KK)
      USE BLK10MOD
      USE BLKSUBMOD
      SAVE
C-
C...... Routine to setup boundary condtions
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK JUN05
CIPK AUG05      INCLUDE 'BLKSUB.COM'
C-
C-
C-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
C-
      IF(KK .EQ. 0) THEN
        NTA=NP
        NLA=1
      ELSE
        NLA=KK
        NTA=KK
      ENDIF
      DO 550 J = NLA,NTA
c       write(75,*) j,nfix(j)
cipk change lines to add vdot
c      IF(MOD(NFIX(J),100)/10 .EQ. 1) VEL(4,J)=SPEC(J,4)
c      IF(MOD(NFIX(J),10)     .EQ. 1) VEL(5,J)=SPEC(J,5)
c      IF(    NFIX1(J)        .EQ. 1) VEL(6,J)=SPEC(J,6)
cipk aug98 test for skipped steady state 
      IF(NITI .NE. 0  .OR. KK .NE. 0) THEN 
        IF(MOD(NFIX(J),100)/10 .EQ. 1) THEN
          VEL(4,J)=SPEC(J,4)
          VDOT(4,J)=ALTM*(VEL(4,J)-VOLD(4,J))-(ALPHA-1.)*VDOTO(4,J)
          IESPC(4,J)=1
        ENDIF
        IF(MOD(NFIX(J),10)     .EQ. 1) THEN
          VEL(5,J)=SPEC(J,5)
          VDOT(5,J)=ALTM*(VEL(5,J)-VOLD(5,J))-(ALPHA-1.)*VDOTO(5,J)
          IESPC(5,J)=1
C        write(75,*) 'in bform j',j,vdot(5,j),vel(5,j),vold(5,j)
        ENDIF
        IF(    NFIX1(J)        .EQ. 1) THEN
          VEL(6,J)=SPEC(J,6)
          VDOT(6,J)=ALTM*(VEL(6,J)-VOLD(6,J))-(ALPHA-1.)*VDOTO(6,J)
          IESPC(6,J)=1
        ENDIF
      ENDIF
cipk aug98 end changes
      IF(NFIX(J) .GE. 13000) GO TO 540
      IF(NFIX(J)/10000 .EQ. 1) THEN
CIPK JUN05
        ISUBM(J)=0
        ALFA(J)=0.
        VEL(1,J)=SPEC(J,1)
CIPK NOV96 INDICATE A ZERO PROJECTION PARAMETER
        IESPC(1,J)=1
        IF(MOD(NFIX(J),10000)/1000 .EQ. 1) THEN
          VEL(2,J)=SPEC(J,2)
CIPK NOV96 INDICATE A ZERO PROJECTION PARAMETER
        IESPC(2,J)=1
        ENDIF
      ENDIF
      IF(NFIX(J)/1000 .EQ. 0) ALFA(J)=0.0
      IF(MOD(NFIX(J)/100,10).NE.0) SPEC(J,3)=SPEC(J,3)-AO(J)
      GO TO 549
  540 IF(NFIX(J)/1000 .NE. 13) GO TO 542
CIPK JUN05
      ISUBM(J)=0
      SPEC(J,1)=SPEC(J,2)
      SPEC(J,2)=0.
      IF(NB .LE. 0) VEL(2,J)=SPEC(J,1)/VEL(3,J)
      ALFA(J)=0.
      GO TO 549
  542 CONTINUE
CIPK JUN05
      ISUBM(J)=0
      VT=SQRT(SPEC(J,1)**2+SPEC(J,2)**2)
      ALFA(J)=0.
      IF(SPEC(J,1) .EQ. 0.) THEN
        ALFA(J)=1.570796
        SPEC(J,1)=VT
        IF(SPEC(J,2) .LT. 0.) THEN
          SPEC(J,2)=-1.570795
csep93 ipk
        ELSEIF(SPEC(J,2) .GT. 0.) THEN
          SPEC(J,2)=1.570796
        ELSE
          SPEC(J,2)=0.0
C          SPEC(J,2)=1.570796
csep93 ipk end changes
        ENDIF
      ELSE
        IF(VT .GT. 0.) ALFA(J)=ATAN(SPEC(J,2)/SPEC(J,1))
        SPEC(J,2)=ATAN2(SPEC(J,2),SPEC(J,1))
        SPEC(J,1)=VT
      ENDIF
  549 CONTINUE
  550 CONTINUE
      RETURN
      END
