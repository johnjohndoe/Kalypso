      SUBROUTINE CSTRC(NN)
      USE BLK10MOD
C
CIPK AUG05      INCLUDE 'BLK10.COM'
C-
      INCLUDE 'BLKE.COM'

      COMMON F(80)

      DIMENSION U(8)

C-
C...... Det up cutoff values
C-
      IF(GRAV .LT. 32.) THEN
        HCUT=0.001
      ELSE
        HCUT=0.003
      ENDIF
      NCN=NCORN(NN)

c     form ave velocity

      DO N=1,NCN
        NA=NOP(NN,N)
        IF(NA .GT. 0) THEN
          U(N)=VEL(1,NA)*COS(ALFA(NA))+VEL(2,NA)*SIN(ALFA(NA))
        ENDIF
      ENDDO

C-
C...... Determine type of control structure
C-
      NM=IMAT(NN)-900
C-
C...... One of the conditions is flow balance
C-
      IF(NJT(NM) .GT. 1) THEN
        F(1)=0.
      ELSE
        F(1)=AJ(NM)
      ENDIF

c     skip mid-side in loop

      DO  KK=1,NCN,2
        N1=NOP(NN,KK)
        IF(N1 .NE. 0) THEN
          NA=(KK-1)*NDF+1
          ESTIFM(1,NA)=DIR(N1)*(WIDTH(N1)+(SS1(N1)+SS2(N1))/2.
     +    *VEL(3,N1))*VEL(3,N1)
          CX=COS(ALFA(N1))
          SA=SIN(ALFA(N1))
          R=VEL(1,N1)*CX+VEL(2,N1)*SA
          ESTIFM(1,NA+2)=DIR(N1)*(WIDTH(N1)+(SS1(N1)+SS2(N1))
     +    *VEL(3,N1))*R
          F(1)=F(1)-ESTIFM(1,NA)*R
        ENDIF
      ENDDO


c      allow for mid-side node
c
c      now do the middle of the element
c
      F(5)=U(2)-(U(3)+U(1))/2.
      ESTIFM(5,1)=0.5
      ESTIFM(5,9)=0.5
      ESTIFM(5,5)=-1.0

C-
C...... The other is some kind elevation relationship
C-
      IF(NJT(NM) .EQ. 1) THEN
C-
C...... Balance total head
C-
        NRX=NOP(NN,1)
        RX=VEL(1,NRX)*COS(ALFA(NRX))+VEL(2,NRX)*SIN(ALFA(NRX))
        THN=HEL(  NRX)+AO(NRX)+RX**2/(2.*GRAV)
        DO  KK=3,NCN
          N1=NOP(NN,KK)
          RY=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
          TH1=HEL(  N1)+AO(N1)+RY**2/(2.*GRAV)
          NA=(KK-1)*NDF+1
          ESTIFM(NA,1)=RX/GRAV
          ESTIFM(NA,3)=1.0
          ESTIFM(NA,NA)=-RY/GRAV
          ESTIFM(NA,NA+2)=-1.0
          F(NA)=TH1-THN
        ENDDO
C
      ELSEIF(NJT(NM) .EQ. 2) THEN
C-
C...... Reversible Q = function of head loss ( h1 -h2 -c)
C-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N1)-QD(NM)) .LT. 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
        N2=NOP(NN,2)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NA=NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
        HLOS=ABS(WS1-WS2)
        HLOS=HLOS-CJ(NM)
        HLD=SIGN(1.,WS1-WS2)
        F(NA)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
        IF(HLOS .LT. HCUT)   HLOS=HCUT
        ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
        ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.
        ESTIFM(NA,3)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -RX1*RSWT1*AWD1/2.
        ESTIFM(NA,NA+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

        F(NA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NA+3,4)=-1.
        ESTIFM(NA+3,NA+3)=1.

C-
      ELSEIF(NJT(NM) .EQ. 3) THEN
C-
C...... Non-reversible Q = function of head loss ( h1 -h2 -c)
C-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N1)-QD(NM)) .LT. 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
        N2=NOP(NN,2)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
        HLOS=WS1-WS2-CJ(NM)
        IF(HLOS .LT. 0.) THEN
          F(NA)=Q
          ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.

        ELSE
          F(NA)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
          IF(HLOS .LT. HCUT)   HLOS=HCUT
          ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.
          ESTIFM(NA,3)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
          ESTIFM(NA,NA+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +                 -RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

          F(NA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NA+3,4)=-1.
          ESTIFM(NA+3,NA+3)=1.

        ENDIF
C-
      ELSEIF(NJT(NM) .EQ. 4) THEN
C-
C......  Q = function of head  ( h1 )
C-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N1)-QD(NM)) .LT. 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
        N2=NOP(NN,2)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NA=NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)-CJ(NM)
        F(NA)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
        ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
        ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.
        ESTIFM(NA,3)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
        ESTIFM(NA,NA+2)=-RX2*RSWT2*AWD2/2.
C-
      ELSEIF(NJT(NM) .EQ. 5) THEN
C-
C...... Reversible   Head loss ( h1 -h2) = function of Q
C-
        N1=NOP(NN,1)
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N1)-QD(NM)) .LT. 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
        N2=NOP(NN,2)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NA=NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        SQ=SIGN(1.,Q)
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
        HLOS=WS1-WS2
        HLD=SIGN(1.,WS1-WS2)
        F(NA)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
        ESTIFM(NA,1)=
     +    BJ(NM)*ACR1*RSWT1/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NA,NA)=
     +    BJ(NM)*ACR2*RSWT2/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NA,3)=-1.
     +   +BJ(NM)*AWD1*RSWT1/2.*RX1*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NA,NA+2)=1.
     +   +BJ(NM)*AWD2*RSWT2/2.*RX2*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
C-
      ELSEIF(NJT(NM) .EQ. 6) THEN
C-
C......  Q = function of head  ( h1 -c)
C-
        N1=NOP(NN,1)
c      RX1 is velocity in the 1-d direction at N1
c      RX2 is velocity in the 1-d direction at N2
c      RSWT1 and RSWT2 are direction multipliers
c       AWD1 and AWD2 are area/depth
        RX1=VEL(1,N1)*COS(ALFA(N1))+VEL(2,N1)*SIN(ALFA(N1))
        IF(ABS(ALFA(N1)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N1)-QD(NM)) .LT. 4.713388) THEN
          RSWT1=-1.
        ELSE
          RSWT1=1.
        ENDIF
        N2=NOP(NN,2)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NA=NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)-BJ(NM)
        WS2=HEL(N2)+AO(N2)-BJ(NM)
        WRITE(75,*) 'WS1,WS2',WS1,WS2
        IF(WS1 .LE. 0.   .AND.  WS2 .LE. 0.) THEN

C    No Flow case

          F(NA)=Q
          ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.
          ESTIFM(NA,3)= -RX1*RSWT1*AWD1/2.
          ESTIFM(NA,NA+2)=-RX2*RSWT2*AWD2/2.
        ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN        

C    Flow controlled from upstream N1

          F(NA)=Q-AJ(NM)*WS1**GAMJ(NM)
          ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.
          ESTIFM(NA,3)=AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
          ESTIFM(NA,NA+2)=-RX2*RSWT2*AWD2/2.
        ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        

C    Flow controlled from upstream N2

          F(NA)=-Q-AJ(NM)*WS2**GAMJ(NM)
          ESTIFM(NA,1)=+(ACR1*RSWT1)/2.
          ESTIFM(NA,NA)=+(ACR2*RSWT2)/2.
          ESTIFM(NA,3)=-AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
     +                 +RX1*RSWT1*AWD1/2.
          ESTIFM(NA,NA+2)=+RX2*RSWT2*AWD2/2.
        ELSE

C    Flow controlled by H1-H2  Get new AJ

          IF(WS1 .GE. WS2) THEN
            AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
            F(NA)=Q-AJN*(WS1-WS2)**CJ(NM)
            ESTIFM(NA,1)=-(ACR1*RSWT1)/2.
            ESTIFM(NA,NA)=-(ACR2*RSWT2)/2.
            ESTIFM(NA,3)=+AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
            ESTIFM(NA,NA+2)=-AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
     +                 -RX2*RSWT2*AWD2/2.
          ELSE
            AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
            F(NA)=-Q-AJN*(WS2-WS1)**CJ(NM)
            ESTIFM(NA,1)=+(ACR1*RSWT1)/2.
            ESTIFM(NA,NA)=+(ACR2*RSWT2)/2.
            ESTIFM(NA,3)=+AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
     +                 +RX1*RSWT1*AWD1/2.
            ESTIFM(NA,NA+2)=+AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
     +                 +RX2*RSWT2*AWD2/2.
          ENDIF
        ENDIF
C-
      ENDIF
      RETURN
      END
