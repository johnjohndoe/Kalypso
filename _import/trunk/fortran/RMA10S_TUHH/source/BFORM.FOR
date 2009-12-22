C     Last change:  WP    9 May 2008   10:44 am
CIPK  LAST UPDATE JUNE 27 2005  ADDCONTROL STRUCTURES
cipk  last update Aug 14 1998   fix occasional bug when restarting
CIPK  LAST UPDATE NOV 12 1996
CIPK  LAST UPDATE SEPT 26 1996
      SUBROUTINE BFORM(KK)
      USE BLK10MOD
      USE BLKSUBMOD
      USE BLKDRMOD
      SAVE
C-
C...... Routine to setup boundary condtions
C-
!nis,may08: Include locals for boundary conditions regarding Marsh-option
      REAL (KIND = 8) :: RealDepth, VirtDepth
      !two passing dummies
      REAL (KIND = 8) :: dum1, dum2
!-
C-
C-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
C-
      !nis,may08: Initialize boundary condition's auxiliary variables
      RealDepth = 0.0d0
      VirtDepth = 0.0d0
      dum1      = 0.0
      dum2      = 0.0
      !-

      IF(KK .EQ. 0) THEN
        NLA=1
        NTA=NP
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
      !nis,jun07: In the old way test is not successful, because niti might be unequal 0 outside unsteady state; test with icyc, which is 0 in steady state is better
      !IF(NITI .NE. 0  .OR. KK .NE. 0) THEN
      IF(icyc .NE. 0  .and. KK .NE. 0) THEN
      !-
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

      !nis,may08 What about the Marsh-option, shouldn't the boundary conditions also be transformed with marsh option?
      !IF(MOD(NFIX(J)/100,10).NE.0) SPEC(J,3)=SPEC(J,3)-AO(J)
      IF(MOD(NFIX(J)/100,10).NE.0) then
        !Intially spec (j, 3) includes water stages in absolute values; at the end of this if-logic, spec (j, 3) contains the water depth
        !regarding ADO (Marsh) or AO (not Marsh)
        !
        !Marsh option active: Adapt water stage boundary conditions regarding the deep slot bed elevation ADO
        if (idnopt < 0) then
          RealDepth = SPEC(j, 3) - ado (j)
          call amf(RealDepth, VirtDepth, akp(j), adt(j), adb(j),
     +             dum1, dum2, 1)
          SPEC(j, 3) = VirtDepth
        !Marsh option not active: Adapt water stage boundary conditions regarding the normal bed elevation AO
        else
          SPEC(J,3)=SPEC(J,3)-AO(J)
        end if
        !EFa oct09, testing hfd
        if (spec(j,8).ne.0.0) then
          alfa(j) = spec(j,8)
        endif
        !-
      endif
      !-
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
