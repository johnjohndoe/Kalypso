CIPK  NEWLY ADDED TO RMA10S FROM RMA2  JUNE 27 2005
CIPK  LAST UPDATE JAN 11 2005 ALLOW FOR NEG HLOS IN OPTION 2
CIPK  LAST UPDATE SEP 06 2004 ADD ERROR FILE
CIPK  LAST UPDATE JULY 5 2004 ALLOW FOR ENGLISH UNITS
cipk  last update oct 2 2003 compute embankment width
CIPK  LAST UPDATE SEP 23 2003 TEST FOR MISSING LEVEE DATA ADDED
CIPK  LAST UPDATE SEP 13 2002 ADJUST FOR DIST FUNCTION
CIPK  LAST UPDATE JUL 30 200 REVIE WEIR TESTING
cipk  last update may 3 2000 add submerged weir element
cipk  last update Apr 8 1997 fix integer*2 for msoft compiler
cipk  last update Mar 9 1997  add more control structures
Cipk  last update Sept 9 1996  fix lhs error for type 4
cipk  last update Apr 1 1996
cipk  last update nov 10 1995
      SUBROUTINE CSTRC2D(NN)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
C

CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLKSUB.COM'
	INCLUDE 'BLKE.COM'
C-
      REAL*8 F
      COMMON F(80),
     1 XN(8),DNX(8),DNY(8),XM(4),DMX(4),DMY(4),XL(8),YL(8)
     2,XO(8),DOX(8),DOY(8),
     3  WAITX(16),DL(2,2)
     4 ,VXX(8),VY(8),VDX(8),VDY(8),ST(8),SDT(8),UBFC(8),VBFC(8)

cipk may00 add cross velocity

      DIMENSION U(8),DAFL(8),V(8)
cipk apr97
cipk sep02      integer*2 n1,n2
      data pid2/1.5708/
      data pid32/4.7124/
      Dist(n1,n2)=sqrt((cord(n1,1)-cord(n2,1))**2 + 
     +                 (cord(n1,2)-cord(n2,2))**2)
cipk mar97 3 lines above modified

cipk mar00 initialize
      do j=1,32
        do k=1,32
          estifm(j,k)=0.
        enddo
        f(j)=0.
      enddo

cipk sep00
C-
C...... Determine type of control structure
C-
      NM=IMAT(NN)-900

C-
C...... Det up cutoff values
C-
      IF(GRAV .LT. 32.) THEN
        HCUT=0.001
        QCUT=0.05
        ucut=0.01
      ELSE
        HCUT=0.003
        QCUT=0.15
        ucut=0.03
      ENDIF
      NCN=NCORN(NN)
      DO 200 N=1,NCN
        NA=NOP(NN,N)
        IF(NA .GT. 0) THEN
cipk sep00
          altmp=alfa(na)
          U(N)=VEL(1,NA)*COS(altmp)+VEL(2,NA)*SIN(altmp)
CIPK MAY00 ADD V COMPONENT
          V(N)=VEL(2,NA)*COS(altmp)-VEL(1,NA)*SIN(altmp)
        ENDIF
  200 CONTINUE
C-
C...... One of the conditions is flow balance
C-
      IF(NJT(NM) .GT. 1) THEN
        A11=0.
      ELSE
        A11=-AJ(NM)
      ENDIF
      DO  300 KK=1,3
cipk nov95 remove iabs
        N1=NOP(NN,KK)
        IF(N1 .EQ. 0) GO TO  300

        if(isubm(n1) .eq. 2) go to 300

cipk nov95 remove iabs
        N2=NOP(NN,8-KK)
        NA=(KK-1)*NDF+1
        NB=(7-KK)*NDF+1
        Q1=U(KK)
        Q2=U(8-KK)
c       IF(Q1 .EQ. 0.) THEN
c         Q1=QCUT
c       ELSEIF(ABS(Q1) .LT. QCUT) THEN
c         Q1=SIGN(QCUT,Q1)
c       ENDIF
c       IF(Q2 .EQ. 0.) THEN
c         Q2=QCUT
c       ELSEIF(ABS(Q2) .LT. QCUT) THEN
c         Q2=SIGN(QCUT,Q2)
c       ENDIF
        IF(KK .NE. 2) THEN
          ESTIFM(NA,NA)=VEL(3,N1)
          ESTIFM(NA,NB)=-VEL(3,N2)
          ESTIFM(NA,NA+2)=Q1
          ESTIFM(NA,NB+2)=-Q2
          F(NA)=A11-VEL(3,N1)*U(KK)+VEL(3,N2)*U(8-KK)
        ELSE
          N3=NOP(NN,1)
          N4=NOP(NN,3)
          ESTIFM(NA,NA)=(VEL(3,N3)+VEL(3,N4))/2.
          ESTIFM(NA,NA-2)=Q1/2.
          ESTIFM(NA,NA+6)=Q1/2.
          N3=NOP(NN,5)
          N4=NOP(NN,7)
          ESTIFM(NA,NB)=-(VEL(3,N3)+VEL(3,N4))/2.
          ESTIFM(NA,NB-2)=-Q2/2.
          ESTIFM(NA,NB+6)=-Q2/2.
          F(NA)=A11-ESTIFM(NA,NA)*U(KK)-ESTIFM(NA,NB)*U(8-KK)
         ENDIF
  300 CONTINUE
C-
C...... The other is some kind elevation relationship
C-
cipk apr00  First do midsides as a balance
c
c         now do the middle of the element
c
         N1=NOP(NN,8)
         N2=NOP(NN,1)

         if(isubm(n2) .eq. 2) go to 302
         N3=NOP(NN,7)
         F(29)=U(8)-(U(7)+U(1))/2.
         F(30)=V(8)-(V(7)+V(1))/2.
         ESTIFM(29,1)=0.5
         ESTIFM(29,25)=0.5
         ESTIFM(29,29)=-1.0
         ESTIFM(30,2)=0.5
         ESTIFM(30,26)=0.5
         ESTIFM(30,30)=-1.0
  302 continue
         N1=NOP(NN,4)
         N2=NOP(NN,3)

         if(isubm(n2) .eq. 2) go to 304

         N3=NOP(NN,5)
         F(13)=U(4)-(U(3)+U(5))/2.
         F(14)=V(4)-(V(3)+V(5))/2.
         ESTIFM(13,9)=0.5
         ESTIFM(13,17)=0.5
         ESTIFM(13,13)=-1.0
         ESTIFM(14,10)=0.5
         ESTIFM(14,18)=0.5
         ESTIFM(14,14)=-1.0
  304 continue

      IF(NJT(NM) .EQ. 1) THEN
C-
C...... Balance total head
C-
        DO 310 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
            Q1=U(KK)
            Q2=U(8-KK)
            IF(Q1 .EQ. 0.) THEN
              Q1=QCUT
            ELSEIF(ABS(Q1) .LT. QCUT) THEN
              Q1=SIGN(QCUT,Q1)
            ENDIF
            IF(Q2 .EQ. 0.) THEN
              Q2=QCUT
            ELSEIF(ABS(Q2) .LT. QCUT) THEN
              Q2=SIGN(QCUT,Q2)
            ENDIF
          IF(KK .NE. 2) THEN
            TH1=HEL(N1)+ADO(N1)+U(KK)**2/(2.*GRAV)
            TH2=HEL(N2)+ADO(N2)+U(8-KK)**2/(2.*GRAV)
            F(NB)=TH1-TH2
            ESTIFM(NB,NA)=-Q1/GRAV
            ESTIFM(NB,NB)=+Q2/GRAV
            ESTIFM(NB,NA+2)=-1.
            ESTIFM(NB,NB+2)=+1.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            TH1=WS1+U(KK)**2/(2.*GRAV)
            TH2=WS2+U(8-KK)**2/(2.*GRAV)
            F(NB)=TH1-TH2
            ESTIFM(NB,NA)=-Q1/GRAV
            ESTIFM(NB,NB)=+Q2/GRAV
            ESTIFM(NB,NA-2)=-0.5
            ESTIFM(NB,NA+6)=-0.5
            ESTIFM(NB,NB-2)=0.5
            ESTIFM(NB,NB+6)=0.5
          ENDIF
  310   CONTINUE
C
      ELSEIF(NJT(NM) .EQ. 2) THEN
C-
C...... Reversible Q = function of head loss ( h1 -h2 -c)
C-
        DO 320 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=ABS(WS1-WS2)
            HLOS=HLOS-CJ(NM)
            HLD=SIGN(1.,WS1-WS2)
cipk jan05
            IF(HLOS .LT. 1.E-06) THEN
		    F(NB)=Q-AJ(NM)
		  ELSE              
              F(NB)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
	      ENDIF
            IF(HLOS .LT. HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -U(KK)/2.
            ESTIFM(NB,NB+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -U(8-KK)/2.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=ABS(WS1-WS2)
            HLOS=HLOS-CJ(NM)
            HLD=SIGN(1.,WS1-WS2)
cipk jan05
            IF(HLOS .LT. 1.E-06) THEN
		    F(NB)=Q-AJ(NM)
		  ELSE              
              F(NB)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
	      ENDIF
            IF(HLOS .LT. HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
            ESTIFM(NB,NA+6)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
            ESTIFM(NB,NB-2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
            ESTIFM(NB,NB+6)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
          ENDIF
  320   CONTINUE
C-
      ELSEIF(NJT(NM) .EQ. 3) THEN
C-
C...... Non-reversible Q = function of head loss ( h1 -h2 -c)
C-
        DO 330 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=WS1-WS2-CJ(NM)
            IF(HLOS .LT. 0.) THEN
              F(NB)=Q
            ELSE
              F(NB)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
              IF(HLOS .LT. HCUT)   HLOS=HCUT
              ESTIFM(NB,NA+2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +                 -U(KK)/2.
              ESTIFM(NB,NB+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -U(8-KK)/2.
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=WS1-WS2-CJ(NM)
            IF(HLOS .LT. 0.) THEN
              F(NB)=Q
            ELSE
              F(NB)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
              IF(HLOS .LT. HCUT)   HLOS=HCUT
              ESTIFM(NB,NA-2)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
              ESTIFM(NB,NA+6)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
              ESTIFM(NB,NB-2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
              ESTIFM(NB,NB+6)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)/2.
     +               -U(8-KK)/4.
            ENDIF
          ENDIF
  330   CONTINUE
C-
      ELSEIF(NJT(NM) .EQ. 4) THEN
C-
C......  Q = function of head  ( h1 )
C-
        DO 340 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            WS1=HEL(N1)+ADO(N1)-CJ(NM)
            F(NB)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
     +               -U(KK)/2.
            ESTIFM(NB,NB+2)=
     +               -U(8-KK)/2.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
cipk mar96 CJ is missing        WS1=(HEL(N3)+AO(N3)+HEL(N4)+AO(N4))/2.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-CJ(NM)
            F(NB)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
cipk sep96 fix actor of 2 error     +               -U(KK)/2.
            ESTIFM(NB,NA+6)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
     +               -U(KK)/4.
cipk sep96 fix factor of 2 error     +               -U(KK)/2.
            ESTIFM(NB,NB-2)=
     +               -U(8-KK)/4.
            ESTIFM(NB,NB+6)=
     +               -U(8-KK)/4.
          ENDIF
  340   CONTINUE
C-
      ELSEIF(NJT(NM) .EQ. 5) THEN
C-
C...... Reversible   Head loss ( h1 -h2) = function of Q
C-

        DO 350 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            SQ=SIGN(1.,Q)
cipk jul94 add 2 line
            call amf(h,vel(3,n1),akp(n1),adt(n1),adb(n1),am1,dam1,0)
            call amf(h,vel(3,n2),akp(n2),adt(n2),adb(n2),am2,dam2,0)
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=WS1-WS2
            HLD=SIGN(1.,WS1-WS2)
            F(NB)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
            IF(Q .EQ. 0.) THEN
              Q=QCUT
            ELSEIF(ABS(Q) .LT. QCUT) THEN
              Q=SIGN(QCUT,Q)
            ENDIF
            ESTIFM(NB,NA)=
     +    BJ(NM)*VEL(3,N1)/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
            ESTIFM(NB,NB)=
     +    BJ(NM)*VEL(3,N2)/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NA+2)=-1.
            ESTIFM(NB,NA+2)=-(1.+dam1)
     +   +BJ(NM)*U(KK)/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NB+2)=1.
            ESTIFM(NB,NB+2)=(1.+dam2)
     +   +BJ(NM)*U(8-KK)/2.*Q*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            SQ=SIGN(1.,Q)
cipk jul94 add 4 lines
            call amf(h,vel(3,n3),akp(n3),adt(n3),adb(n3),am3,dam3,0)
            call amf(h,vel(3,n4),akp(n4),adt(n4),adb(n4),am4,dam4,0)
            call amf(h,vel(3,n5),akp(n5),adt(n5),adb(n5),am5,dam5,0)
            call amf(h,vel(3,n6),akp(n6),adt(n6),adb(n6),am6,dam6,0)
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=WS1-WS2
            F(NB)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
            IF(Q .EQ. 0.) THEN
              Q=QCUT
            ELSEIF(ABS(Q) .LT. QCUT) THEN
              Q=SIGN(QCUT,Q)
            ENDIF
            ESTIFM(NB,NA)=BJ(NM)*(VEL(3,N3)+VEL(3,N4))/4.
     +                  *GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
            ESTIFM(NB,NB)=BJ(NM)*(VEL(3,N5)+VEL(3,N6))/4.
     +                  *GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94           ESTIFM(NB,NA-1)=-0.5
            ESTIFM(NB,NA-2)=-0.5*(1.0+dam3)
     +   +BJ(NM)*U(KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NA+5)=-0.5
            ESTIFM(NB,NA+6)=-0.5*(1.0+dam4)
     +   +BJ(NM)*U(KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NB-1)=0.5
            ESTIFM(NB,NB-2)=0.5*(1.0+dam5)
     +   +BJ(NM)*U(8-KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
cipk jul94            ESTIFM(NB,NB+5)=0.5
            ESTIFM(NB,NB+6)=0.5*(1.0+dam6)
     +   +BJ(NM)*U(8-KK)/4.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
          ENDIF
  350   CONTINUE
C-
      ELSEIF(NJT(NM) .EQ. 6) THEN
C-
C...... Reversible Q = function of head loss of h1 if h >c
C......                =0   if h < c
C-
        DO 360 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
            WS1=HEL(N1)+ADO(N1)-BJ(NM)
            WS2=HEL(N2)+ADO(N2)-BJ(NM)
c            WRITE(75,*) 'C WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
            ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
              ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              IF(WS1 .GT. WS2) THEN
                AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
                F(NB)=Q-AJN*(WS1-WS2)**CJ(NM)
c                write(75,*) 'ajn,q,F(NB)',ajn,q,F(NB)
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                          +AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
                ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)
     +                          -AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
              ELSEIF(WS1 .EQ. WS2) THEN
                F(NB)=Q
                ajn=aj(nm)
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                          +AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)
                ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)
     +                          -AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)
              ELSE
                AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
                F(NB)=-Q-AJN*(WS2-WS1)**CJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
     +                          -AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
                ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                          +AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
              ENDIF
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=-U(KK)/4.
            ESTIFM(NB,NA+6)=-U(KK)/4.
            ESTIFM(NB,NB-2)=-U(8-KK)/4.
            ESTIFM(NB,NB+6)=-U(8-KK)/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-BJ(NM)
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.-BJ(NM)
c            WRITE(75,*) 'M,WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
              ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
            ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
              ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
              ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
              ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              IF(WS1 .GT. WS2) THEN
                AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
                F(NB)=Q-AJN*(WS1-WS2)**CJ(NM)
c                write(75,*) 'ajn,q,F(NB)',ajn,q,F(NB)
                ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB-2)=ESTIFM(NB,NB-2)
     +                   -AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB+6)=ESTIFM(NB,NB+6)
     +                   -AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)/2.
              ELSEIF(WS1 .EQ. WS2) THEN
                F(NB)=Q
                ajn=aj(nm)
                ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB-2)=ESTIFM(NB,NB-2)
     +                   -AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB+6)=ESTIFM(NB,NB+6)
     +                   -AJN*CJ(NM)*(hcut)**(CJ(NM)-1.)/2.
              ELSE
                AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
                F(NB)=-Q-AJN*(WS2-WS1)**CJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
     +                       -AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
     +                       -AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB-1)=-ESTIFM(NB,NB-1)
     +                   +AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
                ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                   +AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)/2.
              ENDIF
            ENDIF
          ENDIF
cc          f(nb)=f(nb)/2.
  360   CONTINUE

C-
      ELSEIF(NJT(NM) .EQ. 7) THEN
C-
C...... Reversible Q = function of head loss of h1 if h >c
C......                =0   if h < c
C-
        DO 370 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
            WS1=HEL(N1)+ADO(N1)-BJ(NM)
            WS2=HEL(N2)+ADO(N2)-BJ(NM)
c            WRITE(75,*) 'C WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
            ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
              ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
            ELSE
C    Flow submerged
              IF(WS1 .GT. WS2) THEN
C    Flow controlled from upstream N1
                F(NB)=Q-AJ(NM)*WS1**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*CJ(NM)*WS1**(CJ(NM)-1.)*DJ(NM)
              ELSEIF(WS1 .EQ. WS2) THEN
                F(NB)=Q
                ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                          +AJ(NM)*DJ(NM)
                ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)
     +                          -AJ(NM)*DJ(NM)
              ELSE
C    Flow controlled from upstream N2
                F(NB)=-Q-AJ(NM)*WS2**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
                ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*CJ(NM)*WS2**(CJ(NM)-1.)*DJ(NM)
              ENDIF
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=-U(KK)/4.
            ESTIFM(NB,NA+6)=-U(KK)/4.
            ESTIFM(NB,NB-2)=-U(8-KK)/4.
            ESTIFM(NB,NB+6)=-U(8-KK)/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-BJ(NM)
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.-BJ(NM)
c            WRITE(75,*) 'M,WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
              ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
            ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
              ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
              ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
              ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
            ELSE
C    Flow submerged
              IF(WS1 .GT. WS2) THEN
C    Flow controlled from upstream N1
                F(NB)=Q-AJ(NM)*WS1**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA-6)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*CJ(NM)*WS1**(CJ(NM)-1.)/2.*DJ(NM)
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*CJ(NM)*WS1**(CJ(NM)-1.)/2.*DJ(NM)
              ELSEIF(WS1 .EQ. WS2) THEN
                F(NB)=Q
                ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*DJ(NM)
                ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*DJ(NM)
                ESTIFM(NB,NB-2)=ESTIFM(NB,NB-2)
     +                   -AJ(NM)*DJ(NM)
                ESTIFM(NB,NB+6)=ESTIFM(NB,NB+6)
     +                   -AJ(NM)*DJ(NM)
              ELSE
C    Flow controlled from upstream N2
                F(NB)=-Q-AJ(NM)*WS2**CJ(NM)*DJ(NM)
                ESTIFM(NB,NA)=-ESTIFM(NB,NA)
                ESTIFM(NB,NB)=-ESTIFM(NB,NB)
                ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
                ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
                ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*CJ(NM)*(WS2**(CJ(NM)-1.))/2.*DJ(NM)
                ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*CJ(NM)*(WS2**(CJ(NM)-1.))/2.*DJ(NM)
              ENDIF
            ENDIF
          ENDIF
cc          f(nb)=f(nb)/2.
  370   CONTINUE
cipk mar97 add new loss function
C-
      ELSEIF(NJT(NM) .EQ. 8) THEN
C-
C.......If Depth < set value
C...... Reversible Q = C1*AFL*Sqrt( h1 -h2)
C......       or   q = C1*AFL/w *sqrt(h1-h2)
C......            A = function of depth
C-
C...... If Depth > set value 
C.      Reversible Q = C2*AFL*Sqrt(h1 -h2)
C.            or   q = C2*AFL/w*sqrt(h1-h2)
C. ...             A = constant at set value depth
C-
C.....  Get w first by averaging 1 to 3 and 5 to 7
        w=(dist(nop(nn,1),nop(nn,3))+dist(nop(nn,5),nop(nn,7)))/2.
C.......Get A based on average water depth
        avedep=(vel(3,nop(nn,1))+vel(3,nop(nn,3))+
     +          vel(3,nop(nn,5))+vel(3,nop(nn,7)))/4.
        wbas=cj(nm)
        ssl=gamj(nm)
        dnomin=dj(nm)
c        write(75,*) 'w,avedep,ssl,dnomin',w,avedep,ssl,dnomin
        if(avedep .lt. dnomin ) then
          afl=avedep*(wbas+ avedep*ssl)
          dafl(1)=wbas/4.0+ssl*avedep/2.0
          dafl(3)=wbas/4.0+ssl*avedep/2.0
          dafl(5)=wbas/4.0+ssl*avedep/2.0
          dafl(7)=wbas/4.0+ssl*avedep/2.0
          cof1=aj(nm)
        else
          afl=dnomin*(wbas+ dnomin*ssl)
          dafl(1)=0.
          dafl(3)=0.
          dafl(5)=0
          dafl(7)=0.
          cof1=bj(nm)
        endif
c        write(75,*) 'afl,dafl(1),cof1',afl,dafl(1),cof1
        DO 390 KK=1,3
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            WS1=HEL(N1)+ADO(N1)
            WS2=HEL(N2)+ADO(N2)
            HLOS=ABS(WS1-WS2)
            HLD=SIGN(1.,WS1-WS2)
c            write(75,*) 'hlos Q',hlos,Q
C            if(hlos .eq. 0.) hlos=0.0001
            F(NB)=Q-cof1*AFL*hld*SQRT(HLOS)/W
            IF(HLOS .LT. HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=+cof1*(AFL/SQRT(HLOS)*0.5
     +                        +SQRT(HLOS)*DAFL(KK))/W-U(KK)/2.
            ESTIFM(NB,NB+2)=-cof1*(AFL/SQRT(HLOS)*0.5
     +                        +SQRT(HLOS)*DAFL(8-KK))/W-U(8-KK)/2.
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.
            HLOS=ABS(WS1-WS2)
            HLD=SIGN(1.,WS1-WS2)
c            write(75,*) 'hlos1 Q',hlos,Q
            F(NB)=Q-COF1*AFL*HLD*SQRT(HLOS)/W
            IF(HLOS .LT. HCUT)   HLOS=HCUT
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(KK-1))/W- U(KK)/4.
            ESTIFM(NB,NA+6)=cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(KK+1))/W- U(KK)/4.
            ESTIFM(NB,NB-2)=-cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(9-KK))/W- U(8-KK)/4.
            ESTIFM(NB,NB+6)=-cof1*(AFL/SQRT(HLOS)*0.25
     +                        +SQRT(HLOS)*DAFL(7-KK))/W- U(8-KK)/4.
          ENDIF
  390   CONTINUE
C-
C-
      ELSEIF(NJT(NM) .EQ. 9) THEN
C-
C...... Reversible Q = function of head loss of h1 if h >c
C......                =0   if h < c
C-
        DO 420 KK=1,3
cipk nov95 remove iabs 2 lines
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1
          IF(KK .NE. 2) THEN
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.
            UM=(U(KK)+U(8-KK))/2.
            call amf(h,vel(3,n1),akp(n1),adt(n1),adb(n1),am1,dam1,0)
            call amf(h,vel(3,n2),akp(n2),adt(n2),adb(n2),am2,dam2,0)
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
            WS1=HEL(N1)+ADO(N1)-BJ(NM)
            WS2=HEL(N2)+ADO(N2)-BJ(NM)
c            WRITE(75,*) 'C WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)
     +                        +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
            ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA+2)=-ESTIFM(NB,NA+2)
              ESTIFM(NB,NB+2)=-ESTIFM(NB,NB+2)
     +                       +AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              if(q .eq. 0) q=qcut
              IF(Q .GT. 0.) THEN
                if(abs(ws1-ws2) .lt. 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS1-WS2-AJN*UM**CJN
c                write(75,*) 'ajn,U,F(NB)',ajn,UM,F(NB)
                if(um .eq. 0.) um=ucut
                ESTIFM(NB,NA)=+AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NB)=+AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NA+2)=-1.0
CC     +                          +AJN*CJN*UM**(CJN-1.)*U(KK)/2.
                ESTIFM(NB,NB+2)=+1.0
CC     +                          +AJN*CJN*UM**(CJN-1.)*U(8-KK)/2.
              ELSEIF(Q .EQ. 0.) THEN
                F(NB)=WS1-WS2
                ESTIFM(NB,NA)=0.
                ESTIFM(NB,NB)=0.
                ESTIFM(NB,NA+2)=-1.0
                ESTIFM(NB,NB+2)=+1.0
              ELSE
                if(abs(ws1-ws2) .lt. 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS2-WS1-AJN*(-UM)**CJN
                ESTIFM(NB,NA)=-AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NB)=-AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NA+2)=+1.0
CC     +                        -AJN*CJN*(-Q)**(CJN-1.)*U(KK)/2.
                ESTIFM(NB,NB+2)=-1.0
CCC    +                        -AJN*CJN*(-Q)**(CJN-1.)*U(8-KK)/2.
              ENDIF
            ENDIF
          ELSE
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            UM=(U(KK)+U(8-KK))/2.
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
            ESTIFM(NB,NA-2)=-U(KK)/4.
            ESTIFM(NB,NA+6)=-U(KK)/4.
            ESTIFM(NB,NB-2)=-U(8-KK)/4.
            ESTIFM(NB,NB+6)=-U(8-KK)/4.
            WS1=(HEL(N3)+ADO(N3)+HEL(N4)+ADO(N4))/2.-BJ(NM)
            WS2=(HEL(N5)+ADO(N5)+HEL(N6)+ADO(N6))/2.-BJ(NM)
c            WRITE(75,*) 'M,WS1,WS2,Q',NN,WS1,WS2,Q
            IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN
C    No Flow case
              F(NB)=Q
            ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        
C    Flow controlled from upstream N1
              F(NB)=Q-AJ(NM)*WS1**GAMJ(NM)
              ESTIFM(NB,NA-2)=ESTIFM(NB,NA-2)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
              ESTIFM(NB,NA+6)=ESTIFM(NB,NA+6)
     +                       +AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)/2.
            ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        
C    Flow controlled from upstream N2
              F(NB)=-Q-AJ(NM)*WS2**GAMJ(NM)
              ESTIFM(NB,NA)=-ESTIFM(NB,NA)
              ESTIFM(NB,NB)=-ESTIFM(NB,NB)
              ESTIFM(NB,NA-2)=-ESTIFM(NB,NA-2)
              ESTIFM(NB,NA+6)=-ESTIFM(NB,NA+6)
              ESTIFM(NB,NB-2)=-ESTIFM(NB,NB-2)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
              ESTIFM(NB,NB+6)=-ESTIFM(NB,NB+6)
     +                       +AJ(NM)*GAMJ(NM)*(WS2**(GAMJ(NM)-1.))/2.
            ELSE
C    Flow controlled by H1-H2  Get new AJ
              if(q .eq. 0.) q=qcut
              IF(Q .GT. 0.) THEN
                if(abs(ws1-ws2) .lt. 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS1-WS2-AJN*UM**CJN
c                write(75,*) 'ajn,q,F(NB)',ajn,q,F(NB)
                if(um .eq. 0.) um=ucut
                ESTIFM(NB,NA)=
     +                 +AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NB)=
     +                 +AJN*CJN*UM**(CJN-1.)/2.
                ESTIFM(NB,NA-2)=-0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NA+6)=-0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NB-2)=0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(8-KK)/4.
                ESTIFM(NB,NB+6)=0.5
CC     +                      +AJN*CJN*Q**(CJN-1.)*U(8-KK)/4.
              ELSEIF(Q .EQ. 0.) THEN
                F(NB)=WS1-WS2
                ESTIFM(NB,NA)=0.1
                ESTIFM(NB,NB)=0.1
                ESTIFM(NB,NA-2)=-0.5
                ESTIFM(NB,NA+6)=-0.5
                ESTIFM(NB,NB-2)=0.5
                ESTIFM(NB,NB+6)=0.5
              ELSE
                if(abs(ws1-ws2) .lt. 1.e-6) then
                  ajn=1./aj(nm)
                else
                  AJN=1.0/(AJ(NM)*(abs(WS1-WS2))**(GAMJ(NM)-CJ(NM)))
                endif
                CJN=1./CJ(NM)
                F(NB)=WS2-WS1-AJN*(-UM)**CJN
                ESTIFM(NB,NA)=
     +                 -AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NB)=
     +                 -AJN*CJN*(-UM)**(CJN-1.)/2.
                ESTIFM(NB,NA-2)=+0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NA+6)=+0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(KK)/4.
                ESTIFM(NB,NB-2)=-0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(8-KK)/4.
                ESTIFM(NB,NB+6)=-0.5
CC     +                      -AJN*CJN*(-Q)**(CJN-1.)*U(8-KK)/4.
              ENDIF
            ENDIF
          ENDIF
          f(nb)=f(nb)/2.
  420   CONTINUE


      ELSEIF(NJT(NM) .EQ. 10) THEN
C-
C...... Reversible Q = weir flow 
C......                =0   if h < c
C-
c
c          F = (u1*h1+u2*h2) - f(u1,h1,h2) = 0

        ITP=1

        DO  KK=1,3
C
C      get nodal locations
C
          N1=NOP(NN,KK)
          N2=NOP(NN,8-KK)
cipk oct03  calculate overall embnakment width
          widem=sqrt((cord(n2,2)-cord(n1,2))**2+
     +                (cord(n2,1)-cord(n1,1))**2)

cipk sep00
          altmp=atan2(cord(n2,2)-cord(n1,2),
     +                    cord(n2,1)-cord(n1,1))
          if(abs(alfa(n1)) .lt. pid2) then
            if(alfa(n1) + pid2 .lt. altmp  .or.
     +         alfa(n1) - pid2  .gt. altmp) then
              srevr=-1.
            else
              srevr=1.
            endif
          elseif(alfa(n1) .gt. 0.) then
            if(alfa(n1)-pid2 .gt. altmp .and.
     +           alfa(n1)-pid32 .lt. altmp) then
              srevr=-1.
            else
              srevr=1.
            endif
          else
            if(alfa(n1)+pid2 .lt. altmp .and.
     +           alfa(n1)+pid32 .gt. altmp) then
              srevr=-1.
            else
              srevr=1.
            endif
          endif

          NA=(KK-1)*NDF+1
          NB=(7-KK)*NDF+1

cipk oct00
          if(isubm(n1) .eq. 2) go to 500

     
C      set sign for DH

          IF(VEL(3,N1) .GE. 0.) THEN
            DH=0.0100
          ELSE
            DH=0.0100
          ENDIF
 
          IF(KK .NE. 2) THEN
c
c      This is a corner node
c
     
            Q=(U(KK)*VEL(3,N1)+U(8-KK)*VEL(3,N2))/2.

c
c      Derivative with respect to u1 and u2
c
            ESTIFM(NB,NA)=-VEL(3,N1)/2.
            ESTIFM(NB,NB)=-VEL(3,N2)/2.

            IF(NTMREF(IMAT(NN)) .NE. 0) THEN
              CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET)
	        IF(ISWTOF .EQ. 1) THEN
	          Q1T=0.0
			  F(NB)=Q-Q1T
                GO TO 500
	        ENDIF
	      ENDIF


c
c      Derivative with respect to h1 and h2  (first term)
c
            ESTIFM(NB,NA+2)=-U(KK)/2.
            ESTIFM(NB,NB+2)=-U(8-KK)/2.
c
c      Prepare for call to get function
c
            WH1=VEL(3,N1)
            WH2=VEL(3,N2)
            WS1=WSLL(N1)
            WS2=WSLL(N2)
            WV1=SQRT(VEL(1,N1)**2+VEL(2,N1)**2)
            WV2=SQRT(VEL(1,N2)**2+VEL(2,N2)**2)
            WEC=WHGT(N1)
            WLN=WLEN(N1)
CIPK SEP03 
	      IF(WEC .LT. -9999.  .and.  NCTREF(IMAT(NN)) .eq. 0) THEN
CIPK SEP04
	        CLOSE(75)
	        OPEN(75,file='ERROR.OUT')
	        WRITE(*,*) 'ERROR   UNDEFINED LEVEE DATA FOR NODE',N1
	        WRITE(*,*) 'EXECUTION TERMINATED'
	        WRITE(75,*) 'ERROR   UNDEFINED LEVEE DATA FOR NODE',N1
	        WRITE(75,*) 'EXECUTION TERMINATED'
	        STOP
            ENDIF
ccc            write(69,*) 'wform element node',nn,n1,widem 
            IWTYP=N1
cipk jul04
	      IF(GRAV .GT. 30.) THEN
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM(Q1T,WH1/3.2808,WS1/3.2808,WV1/3.2808,WH2/3.2808
     +        ,WS2/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808,ITP,IWTYP
     +                ,widem/3.2808)
	        Q1T=Q1T*10.7636    
             ELSE
	        CALL WTFORM(Q1T,NCTREF(IMAT(NN)),WS1,WS2)
	       ENDIF
	      ELSE
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)
	       ELSE
	        CALL WTFORM(Q1T,NCTREF(IMAT(NN)),WS1,WS2)
	       ENDIF
	      ENDIF
            q1t=q1t*srevr
            F(NB)=Q-Q1T
cipk oct03 add widem
            IWTYP=NN
cipk jul04
	      IF(GRAV .GT. 30.) THEN
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q1DH1,(WH1+DH/2.)/3.2808,(WS1+DH/2.)/3.2808,WV1/3.2808
     +        ,WH2/3.2808,WS2/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
	        Q1DH1=Q1DH1*10.7636    
	       ELSE
	        CALL WTFORM(Q1DH1,NCTREF(IMAT(NN)),WS1+DH/2.,WS2)
	       ENDIF
	      ELSE
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q1DH1,WH1+DH/2.,WS1+DH/2.,WV1,WH2,WS2,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)
	       ELSE
	        CALL WTFORM(Q1DH1,NCTREF(IMAT(NN)),WS1+DH/2.,WS2)
	       ENDIF
	      ENDIF
            IWTYP=N1
cipk jul04
	      IF(GRAV .GT. 30.) THEN
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q2DH1,(WH1-DH/2.)/3.2808,(WS1-DH/2.)/3.2808,WV1/3.2808
     +        ,WH2/3.2808,WS2/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
	        Q2DH1=Q2DH1*10.7636
	       ELSE
	        CALL WTFORM(Q2DH1,NCTREF(IMAT(NN)),WS1-DH/2.,WS2)
	       ENDIF
		  ELSE    
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q2DH1,WH1-DH/2.,WS1-DH/2.,WV1,WH2,WS2,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)
	       ELSE
	        CALL WTFORM(Q2DH1,NCTREF(IMAT(NN)),WS1-DH/2.,WS2)
	       ENDIF
	      ENDIF
            q1dh1=q1dh1*srevr
            q2dh1=q2dh1*srevr
            DQDH1=(Q1DH1-Q2DH1)/DH
            ESTIFM(NB,NA+2)=ESTIFM(NB,NA+2)+DQDH1
cipk oct03 add widem
            IWTYP=NN
cipk jul04
	      IF(GRAV .GT. 30.) THEN
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q1DH2,WH1/3.2808,WS1/3.2808,WV1/3.2808,(WH2+DH/2.)/3.2808
     +        ,(WS2+DH/2.)/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
	        Q1DH2=Q1DH2*10.7636
	       ELSE
	        CALL WTFORM(Q1DH2,NCTREF(IMAT(NN)),WS1,WS2+DH/2.)
	       ENDIF
            ELSE
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q1DH2,WH1,WS1,WV1,WH2+DH/2.,WS2+DH/2.,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)
	       ELSE
	        CALL WTFORM(Q1DH2,NCTREF(IMAT(NN)),WS1,WS2+DH/2.)
	       ENDIF
	      ENDIF
            IWTYP=N1
	      IF(GRAV .GT. 30.) THEN
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q2DH2,WH1/3.2808,WS1/3.2808,WV1/3.2808,(WH2-DH/2.)/3.2808
     +        ,(WS2-DH/2.)/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
	        Q2DH2=Q2DH2*10.7636
	       ELSE
	        CALL WTFORM(Q2DH2,NCTREF(IMAT(NN)),WS1,WS2+DH/2.)
	       ENDIF
            ELSE
	       IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q2DH2,WH1,WS1,WV1,WH2-DH/2.,WS2-DH/2.,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)
	       ELSE
	        CALL WTFORM(Q2DH2,NCTREF(IMAT(NN)),WS1,WS2+DH/2.)
	       ENDIF
            ENDIF
C
            q1dh2=q1dh2*srevr
            q2dh2=q2dh2*srevr
            DQDH2= (Q1DH2-Q2DH2)/DH
c            IF(n1 .eq. 1407) then
c              write(167,7890) 
c     +maxn,n1,n2,iwtyp,dqdh2,q1dh2,Q2DH2,ws1,ws2+dh/2.,ws2-dh/2.,wh1,wh2
c	      ENDIF
            ESTIFM(NB,NB+2)=ESTIFM(NB,NB+2)+DQDH2

          ELSE
C
C     This is a mid-side node
C
            N3=NOP(NN,1)
            N4=NOP(NN,3)
            N5=NOP(NN,5)
            N6=NOP(NN,7)

cipk oct00
            if(isubm(n1) .eq. 2) go to 500

     
            Q=(U(KK)*(VEL(3,N3)+VEL(3,N4))/2.
     +       +U(8-KK)*(VEL(3,N5)+VEL(3,N6))/2.)/2.
            QM=(U(1)*VEL(3,N3)+U(3)*VEL(3,N4))/4.
     +        +(U(7)*VEL(3,N6)+U(5)*VEL(3,N5))/4.
C DFDU2
            ESTIFM(NB,NA)=-(VEL(3,N3)+VEL(3,N4))/4.
C DFDU6
            ESTIFM(NB,NB)=-(VEL(3,N5)+VEL(3,N6))/4.
C DFDH1
            ESTIFM(NB,NA-2)=-(U(KK)/4.-U(1)/4.)
C DFDH3
            ESTIFM(NB,NA+6)=-(U(KK)/4.-U(3)/4.)
C DFDH5
            ESTIFM(NB,NB-2)=-(U(8-KK)/4.-U(5)/4.)
C DFDH7
            ESTIFM(NB,NB+6)=-(U(8-KK)/4.-U(7)/4.)
C DFDU1
            ESTIFM(NB,NA-4)=VEL(3,N3)/4.
C DFDU3
            ESTIFM(NB,NA+4)=VEL(3,N4)/4.
C DFDU5
            ESTIFM(NB,NB-4)=VEL(3,N5)/4.
C DFDU7
            ESTIFM(NB,NB+4)=VEL(3,N6)/4.
            F(NB)=Q-QM
          ENDIF

  500   continue
        ENDDO
C-




      ENDIF

      RETURN
      END
