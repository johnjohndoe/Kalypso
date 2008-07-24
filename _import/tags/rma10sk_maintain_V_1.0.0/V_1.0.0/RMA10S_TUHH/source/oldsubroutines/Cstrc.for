C     Last change:  NIS  21 Feb 2008    8:15 am
cipk  last update nov 28 2006 allow for all 1-d control structure options
      SUBROUTINE CSTRC(NN)
      USE BLK10MOD
      USE BLKSUBMOD
      USE BLK11MOD
      USE Para1DPoly
      USE parakalyps

C
CIPK AUG05      INCLUDE 'BLK10.COM'
C-
      INCLUDE 'BLKE.COM'

      REAL (KIND = 8) :: F
      COMMON F(80)

      DIMENSION U(8)

!NiS,jul06: Declaring waterlevel H for proper call of amf-subroutine
      REAL (KIND = 8) :: WH1, WH2
      REAL (KIND = 8) :: calcPolynomial, calcPolynomial1stDerivative
      REAL (KIND = 8) :: Q1t
      INTEGER :: PolyPos(1:2), findpolynom
      INTEGER :: NodA, NodB
      !nis,feb08
      REAL (KIND = 8) :: DX, DY, CosinusAlpha

!-
C-
C...... Det up cutoff values
C-

      IF(GRAV .LT. 32.) THEN
        HCUT=0.001
      ELSE
        HCUT=0.003
      ENDIF
      NCN=NCORN(NN)

      if (width(nop(nn,1)) == 0.0) then
        !init Q(h)
        qh(nop(nn,1)) = 0.0
        qh(nop(nn,3)) = 0.0

        PolyPos(1) = findPolynom (PolyRangeA (nop(nn, 1), :),
     +               vel (3, nop(nn, 1)), PolySplitsA (nop(nn, 1)))
        PolyPos(2) = findPolynom (PolyRangeA (nop(nn, 3), :),
     +               vel (3, nop(nn, 3)), PolySplitsA (nop(nn, 1)))

        !A(h)
        ah(nop(nn,1))
     +   = calcPolynomial
     +       (apoly (PolyPos(1), nop(nn, 1), 0:12), vel(3, nop(nn,1)))
        ah(nop(nn,3))
     +   = calcPolynomial
     +       (apoly (PolyPos(2), nop(nn, 3), 0:12), vel(3, nop(nn,3)))
        !testing
        !WRITE(*,*) 'Flaechen: ', ah(nop(nn, 1)), ah(nop(nn, 3))
        !pause
        !testing-
        !dA(h)/dh
        dahdh(nop(nn,1))
     +   = calcPolynomial1stDerivative
     +       (apoly (PolyPos(1), nop(nn,1), 0:12), vel(3, nop(nn,1)))
        dahdh(nop(nn,3))
     +   = calcPolynomial1stDerivative
     +       (apoly (PolyPos(2), nop(nn,3), 0:12), vel(3, nop(nn,3)))
      endif

c     form ave velocity
      DO N=1,NCN
        NodA=NOP(NN,N)
        IF(NodA .GT. 0) THEN
          U(N) = VEL (1, NodA) * COS (ALFA (NodA)) +
     +           VEL (2, NodA) * SIN (ALFA (NodA))
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

      !testing
      WRITE(*,*) 'general informations'
      WRITE(*,*) 'Evaluating equation 1'
      WRITE(*,*) '   Element: ', nn
      WRITE(*,*) '  upstream: ', NOP(nn, 1)
      WRITE(*,*) 'downstream: ', NOP(nn, 3)
      !testing-

c     skip mid-side in loop
      DO  KK = 1, NCN, 2
        N1 = NOP (NN, KK)

        !we need a global direction
        if (kK == 1) then
          dirfact = 1.0d0
        else
          dirfact = -1.0d0
        end if
        !-

        IF (N1 /= 0) THEN
          NodA = (KK - 1) * NDF + 1
          !EFa Apr07, if-clause
          if (width(n1) == 0.0) then
            !estifm (1, NodA) = dir (n1) * ah (n1)
            estifm (1, NodA) = - dirfact * ah (n1) * u (kk) / ABS (u (kk))


            !testing
            WRITE(*,*) '     node: ', n1, '(', KK, ')'
            WRITE(*,*) '*****************************'
            !WRITE(*,*) 'direction: ', dir (n1)
            WRITE(*,*) 'direction: ', dirfact
            WRITE(*,*) 'water st.: ', vel(3, n1) + ao(n1)
            WRITE(*,*) '     area: ', ah (n1)
            WRITE(*,*) '    dF/dv: ', estifm (1, noda)
            WRITE(*,*)
            !testing-

          else
            !ESTIFM (1, NodA) = DIR(N1)*(WIDTH(N1)+(SS1(N1)+SS2(N1))/2.
            ESTIFM (1, NodA) = DIRfact*(WIDTH(N1)+(SS1(N1)+SS2(N1))/2.
     +      *VEL(3,N1))*VEL(3,N1)
          end if
          !-
          CX=COS(ALFA(N1))
          SA=SIN(ALFA(N1))
          R=VEL(1,N1)*CX+VEL(2,N1)*SA
          !EFa Apr07,if-clause
          if (width(n1) == 0.0) then
            !estifm(1,NodA+2)=dir(n1)*dahdh(n1)*r
            estifm (1, NodA + 2) = dirfact * dahdh(n1) * u(kk)
          else
            !ESTIFM(1,NodA+2)=DIR(N1)*(WIDTH(N1)+(SS1(N1)+SS2(N1))
            ESTIFM(1,NodA+2)=DIRfact*(WIDTH(N1)+(SS1(N1)+SS2(N1))
     +      *VEL(3,N1))*u(kk)
          end if

          !testing
          WRITE(*,*) '    dahdh: ', dahdh (n1)
          WRITE(*,*) ' velocity: ', u(kk)
          WRITE(*,*) '    dF/dh: ', ESTIFM (1, noda)
          !testing-

          F(1) = F (1) - ESTIFM (1, NodA) * u (kk)

          !testing
          WRITE(*,*) ' residual: ', ESTIFM (1, noda) * u(kk)
          WRITE(*,*) ' tot res : ', f(1)
          WRITE(*,*) '---'
          !testing-

        ENDIF
      ENDDO

      !testing
      !pause
      !testing-

c      allow for mid-side node
c
c      now do the middle of the element
c
        !testing
        !WRITE(*,*) u(1), u(2), u(3)
        !pause
        !testing-
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
          NodA=(KK-1)*NDF+1
          ESTIFM(NodA,1)=RX/GRAV
          ESTIFM(NodA,3)=1.0
          ESTIFM(NodA,NodA)=-RY/GRAV
          ESTIFM(NodA,NodA+2)=-1.0
          F(NodA)=TH1-THN
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
CIPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)  
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
cipk nov06 revise
        NodA=2*NDF+1
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
        F(NodA)=Q-AJ(NM)-BJ(NM)*HLD*HLOS**GAMJ(NM)
        IF(HLOS .LT. HCUT)   HLOS=HCUT
        ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
        ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
        ESTIFM(NodA,3)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -RX1*RSWT1*AWD1/2.
        ESTIFM(NodA,NodA+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +               -RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

        F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NodA+3,4)=-1.
        ESTIFM(NodA+3,NodA+3)=1.

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
CIPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
        NodA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)
        WS2=HEL(N2)+AO(N2)
        HLOS=WS1-WS2-CJ(NM)
        IF(HLOS .LT. 0.) THEN
          F(NodA)=Q
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.

        ELSE
          F(NodA)=Q-AJ(NM)-BJ(NM)*HLOS**GAMJ(NM)
          IF(HLOS .LT. HCUT)   HLOS=HCUT
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)=BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=-BJ(NM)*GAMJ(NM)*HLOS**(GAMJ(NM)-1.)
     +                 -RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

          F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NodA+3,4)=-1.
          ESTIFM(NodA+3,NodA+3)=1.

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
CIPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)        
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
cipk nov06 revise NodA
        NodA=2*NDF+1
        AWD1=WIDTH(N1)+(SS1(N1)+SS2(N1))/2.*VEL(3,N1)
        AWD2=WIDTH(N2)+(SS1(N2)+SS2(N2))/2.*VEL(3,N2)
        ACR1=AWD1*VEL(3,N1)
        ACR2=AWD2*VEL(3,N2)
        Q=(ACR1*RX1*RSWT1+ACR2*RX2*RSWT2)/2.
        WS1=HEL(N1)+AO(N1)-CJ(NM)
        F(NodA)=Q-AJ(NM)-BJ(NM)*WS1**GAMJ(NM)
        ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
        ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
        ESTIFM(NodA,3)=BJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
        ESTIFM(NodA,NodA+2)=-RX2*RSWT2*AWD2/2.
C-
C       Set temp/sal equation for equality

        F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NodA+3,4)=-1.
        ESTIFM(NodA+3,NodA+3)=1.

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
CIPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
cipk nov06 revise NodA
        NodA=2*NDF+1
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
        F(NodA)=HLOS-AJ(NM)-BJ(NM)*SQ*(ABS(Q))**GAMJ(NM)
        ESTIFM(NodA,1)=
     +    BJ(NM)*ACR1*RSWT1/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NodA,NodA)=
     +    BJ(NM)*ACR2*RSWT2/2.*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NodA,3)=-1.
     +   +BJ(NM)*AWD1*RSWT1/2.*RX1*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
        ESTIFM(NodA,NodA+2)=1.
     +   +BJ(NM)*AWD2*RSWT2/2.*RX2*GAMJ(NM)*(ABS(Q))**(GAMJ(NM)-1.)
C-
C       Set temp/sal equation for equality

        F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
        ESTIFM(NodA+3,4)=-1.
        ESTIFM(NodA+3,NodA+3)=1.

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
CIPK NOV06  RESET TO NOP(NN,3) FROM NOP(NN,2)        
        N2=NOP(NN,3)
        RX2=VEL(1,N2)*COS(ALFA(N2))+VEL(2,N2)*SIN(ALFA(N2))
        IF(ABS(ALFA(N2)-QD(NM)) .GT. 1.570796  .AND.
     +     ABS(ALFA(N2)-QD(NM)) .LT. 4.713388) THEN
          RSWT2=-1.
        ELSE
          RSWT2=1.
        ENDIF
cipk nov06 revise NodA
        NodA=2*NDF+1
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

          F(NodA)=Q
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)= -RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=-RX2*RSWT2*AWD2/2.
        ELSEIF(WS1 .GT. 0.  .AND.  WS2 .LE. 0.) THEN

C    Flow controlled from upstream N1

          F(NodA)=Q-AJ(NM)*WS1**GAMJ(NM)
          ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)=AJ(NM)*GAMJ(NM)*WS1**(GAMJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=-RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

          F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NodA+3,4)=-1.
          ESTIFM(NodA+3,NodA+3)=1.

        ELSEIF(WS1 .LE. 0.  .AND.  WS2 .GT. 0.) THEN        

C    Flow controlled from upstream N2

          F(NodA)=-Q-AJ(NM)*WS2**GAMJ(NM)
          ESTIFM(NodA,1)=+(ACR1*RSWT1)/2.
          ESTIFM(NodA,NodA)=+(ACR2*RSWT2)/2.
          ESTIFM(NodA,3)=-AJ(NM)*GAMJ(NM)*WS2**(GAMJ(NM)-1.)
     +                 +RX1*RSWT1*AWD1/2.
          ESTIFM(NodA,NodA+2)=+RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

          F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
          ESTIFM(NodA+3,4)=-1.
          ESTIFM(NodA+3,NodA+3)=1.

        ELSE

C    Flow controlled by H1-H2  Get new AJ

          IF(WS1 .GE. WS2) THEN
            AJN=AJ(NM)*(WS1-WS2)**(GAMJ(NM)-CJ(NM))
            F(NodA)=Q-AJN*(WS1-WS2)**CJ(NM)
            ESTIFM(NodA,1)=-(ACR1*RSWT1)/2.
            ESTIFM(NodA,NodA)=-(ACR2*RSWT2)/2.
            ESTIFM(NodA,3)=+AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
     +                 -RX1*RSWT1*AWD1/2.
            ESTIFM(NodA,NodA+2)=-AJN*CJ(NM)*(WS1-WS2)**(CJ(NM)-1.)
     +                 -RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

            F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
            ESTIFM(NodA+3,4)=-1.
            ESTIFM(NodA+3,NodA+3)=1.

          ELSE
            AJN=AJ(NM)*(WS2-WS1)**(GAMJ(NM)-CJ(NM))
            F(NodA)=-Q-AJN*(WS2-WS1)**CJ(NM)
            ESTIFM(NodA,1)=+(ACR1*RSWT1)/2.
            ESTIFM(NodA,NodA)=+(ACR2*RSWT2)/2.
            ESTIFM(NodA,3)=+AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
     +                 +RX1*RSWT1*AWD1/2.
            ESTIFM(NodA,NodA+2)=+AJN*CJ(NM)*(WS2-WS1)**(CJ(NM)-1.)
     +                 +RX2*RSWT2*AWD2/2.

C       Set temp/sal equation for equality

            F(NodA+3)= VEL(ICK,N1)-VEL(ICK,N2)
            ESTIFM(NodA+3,4)=-1.
            ESTIFM(NodA+3,NodA+3)=1.

          ENDIF
        ENDIF
C-
      ELSEIF(NJT(NM) .EQ. 10) THEN
C-
C...... Reversible Q = weir flow 
C......                =0   if h < c
C-
c
!          F = (u1*h1+u2*h2) - f(u1,h1,h2) = 0

        ITP=1

C
C      get nodal locations
C
        N1 = NOP (NN, 1)
        N2 = NOP (NN, 3)
cipk oct03  calculate overall embankment width
        widem=sqrt((cord(n2,2)-cord(n1,2))**2+
     +                (cord(n2,1)-cord(n1,1))**2)
        NodA = 1
        NodB = 9

CIPK JUN04          if(WSLL(n1) .gt. transel(n1)) go to 500
cipk oct00
        if(isubm(n1) .eq. 2) go to 500

C      set sign for DH

        IF(VEL(3,N1) .GE. 0.) THEN
          DH=0.002
          DV = 0.01
          !DH = 0.01
        ELSE
          DH=0.002
          DV = 0.01
          !DH = 0.01
        ENDIF
 
c
c      This is a corner node
c
        IF(WSLL(N1) .LT. TRANSEL(N1)) THEN

          !testing
          WRITE(*,*) 'velocities'
          WRITE(*,*) u(1), u(3)
          WRITE(*,*)
          !testing-

          if (width(n1) == 0) then
            Q = (U(1) * ah(n1) + U(3) * ah(N2)) / 2.
          else
            Q = (U(1) * VEL(3, N1) + U(3) * VEL(3, N2)) / 2.
          end if
     
c
c      Derivative with respect to u1 and u2
c

          if (width (n1) == 0.0) then
            ESTIFM (NodB, NodA) = -ah(n1) / 2. * u(1) / ABS (u (1))
            ESTIFM (NodB, NodB) = -ah(n2) / 2. * u(3) / ABS (u (3))
          else
            ESTIFM (NodB, NodA) = -VEL(3, N1) / 2.
            ESTIFM (NodB, NodB) = -VEL(3, N2) / 2.
          end if

CIPK JAN06 ADD QFACT1

          !initialize QFACT1
          QFACT1 = 1.0

          DX = cord (nop (nn, 3), 1) - cord (nop (nn, 1), 1)
          DY = cord (nop (nn, 3), 2) - cord (nop (nn, 1), 1)
          !Calculate the angle between the element direction and the global x-axis to get the sign of Q
          CosinusAlpha = (DX * 1.0d0 + DY * 0.0d0) /
     +      (SQRT(DX**2 + DY**2) * SQRT(1.0d0**2 + 2.0d0**2))
          !testing
          !WRITE(*,*) CosinusAlpha
          !pause
          !testing-
          if (CosinusAlpha < 0.0d0) QFACT1 = -1.0

          IF(NTMREF(IMAT(NN)) .NE. 0) THEN
            CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET,QFACT1)
            IF(ISWTOF .EQ. 1) THEN
              Q1T=0.0
              F(NodB)=Q-Q1T
C              ESTIFM(NodB,NodA+2)=ESTIFM(NodB,NodA+2)+1.E6
C              ESTIFM(NodB,NodB+2)=ESTIFM(NodB,NodB+2)+1.E6
              VEL(1,NOP(NN,1))=0.
              VEL(2,NOP(NN,1))=0.
              VEL(1,NOP(NN,2))=0.
              VEL(2,NOP(NN,2))=0.
              VEL(1,NOP(NN,3))=0.
              VEL(2,NOP(NN,3))=0.
              ESTIFM(1,1)=1.E6              
              ESTIFM(9,9)=1.E6              
              GO TO 500
            ENDIF
          ENDIF


c
c      Derivative with respect to h1 and h2  (first term)
c
          if (width (n1) == 0.0d0) then
            ESTIFM (NodB, NodA + 2) = -U (1) * dahdh (n1)/ 2.
            ESTIFM (NodB, NodB + 2) = -U (3) * dahdh (n2)/ 2.
          else
            ESTIFM (NodB, NodA + 2) = -U (1) / 2.
            ESTIFM (NodB, NodB + 2) = -U (3) / 2.
          end if

c
c      Prepare for call to get function
c
          WH1=VEL(3,N1)
          WH2=VEL(3,N2)

          !HACK
          !WS1=WSLL(N1)
          !WS2=WSLL(N2)
          WS1 = AO (n1) + vel(3, n1)
          ws2 = ao (n2) + vel(3, n2)
          !testing
          !WRITE(*,*) 'Hoehen'
          !WRITE(*,*) n1, n2
          !WRITE(*,*) WH1, ao(n1), ws1
          !WRITE(*,*) wh2, ao(n2), ws2
          !pause
          !testing-

          WV1=SQRT(VEL(1,N1)**2+VEL(2,N1)**2)
          WV2=SQRT(VEL(1,N2)**2+VEL(2,N2)**2)
          WEC=WHGT(N1)
          WLN=WLEN(N1)

          IF(WEC .LT. -9999.) THEN
CIPK SEP04
          !ERROR - UNDEFINED LEVEE DATA FOR NODE
            call ErrorMessageAndStop (1107, N1, cord (N1, 1),
     +                                          cord(N1, 2))

          ENDIF
ccc   
cipk oct03 add widem
cipk jul04
          IF(GRAV .GT. 30.) THEN
CIPK JUN05
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM(Q1T,WH1/3.2808,WS1/3.2808,WV1/3.2808,WH2/3.2808
     +        ,WS2/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808,ITP,IWTYP
     +        ,widem/3.2808)
              Q1T = Q1T * 10.7636
CIPK JUN05
            ELSE

              CALL WTFORM(Q1T,NCTREF(IMAT(NN)),WS1,WS2)
            ENDIF

!*********************
!SI-units: Q(huw, how)
          ELSE

            !for weir formula
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM(Q1T,WH1,WS1,WV1,WH2,WS2,WV2,WEC,WLN,ITP,IWTYP
     +        ,widem)

            !***********************************
            !for table- or function defined weir
            !***********************************
            ELSE
              !calculate the energy at upper and lower part of weir
              if (UseEnergyCstrc == 1) then
                WS1 = WS1 + WV1**2 / (2 * grav)
                WS2 = WS2 + WV2**2 / (2 * grav)
              end if

              !WRITE(*,*) 'Calculate Q at cstrc'
              CALL WTFORM(Q1T,NCTREF(IMAT(NN)),WS1,WS2)

            ENDIF
!end SI-units: Q(huw, how)
!*********************

          ENDIF

          !Scale this part of the numerical derivative by the direction
          Q1T = Q1T * QFACT1

          !WRITE(*,*) Q, Q1T
          F(NodB) = Q - Q1T

          !testing
          WRITE(*,*) 'how: ', ws1
          WRITE(*,*) 'huw: ', ws2
          WRITE(*,*) 'Ist-Abfluss und Soll-Abfluss'
          WRITE(*,*) Q, Q1T
          WRITE(*,*) 'Residuum der 9. Gleichung'
          WRITE(*,*) F(NodB)
          !pause
          !testing-

cipk oct03 add widem
cipk jul04
          IF(GRAV .GT. 30.) THEN
CIPK JUN05
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q1DH1,(WH1+DH/2.)/3.2808,(WS1+DH/2.)/3.2808,WV1/3.2808
     +        ,WH2/3.2808,WS2/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
              Q1DH1=Q1DH1*10.7636
CIPKJUN05
            ELSE
              CALL WTFORM(Q1DH1,NCTREF(IMAT(NN)),WS1+DH/2.,WS2)
            ENDIF

!*********************
!SI-units: dQ/+dhow
          ELSE

            !for weir formula
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q1DH1,WH1+DH/2.,WS1+DH/2.,WV1,WH2,WS2,WV2,WEC,WLN,ITP,IWTYP
     +        ,widem)

            !***********************************
            !for table- or function defined weir
            !***********************************
            ELSE
              !WRITE(*,*) 'Calculate dQ/+dhow at cstrc', ws1, ws1+dh/2.
              CALL WTFORM(Q1DH1,NCTREF(IMAT(NN)),WS1+DH/2.,WS2)
            ENDIF
          ENDIF

          !Scale this part of the numerical derivative by the direction
          Q1DH1 = Q1DH1 * QFACT1


!end SI-units: dQ/+dhow
!*********************

          IWTYP=N1
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

!*********************
!SI- units: dQ/-dhow
          ELSE

            !for weir formula
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q2DH1,WH1-DH/2.,WS1-DH/2.,WV1,WH2,WS2,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)

            !***********************************
            !for table- or function defined weir
            !***********************************
            ELSE
              !WRITE(*,*) 'Calculate dQ/-dhow at cstrc', ws1, ws1-dh/2.
              CALL WTFORM(Q2DH1,NCTREF(IMAT(NN)),WS1-DH/2.,WS2)
            ENDIF
          ENDIF

          !Scale this part of the numerical derivative by the direction
          Q2DH1 = Q2DH1 * QFACT1


!end SI-units: dQ/-dhow
!*********************

          !calculate dQ/dh
          DQDH1 = (Q1DH1 - Q2DH1) / DH

          ESTIFM (NodB, NodA + 2) = ESTIFM (NodB, NodA + 2) + DQDH1

          !testing
          WRITE(*,*) '---'
          WRITE(*,*) 'dQdh1, Oberwasser'
          WRITE(*,*) '-----------------'
          WRITE(*,*) 'part1: ', Q1DH1
          WRITE(*,*) 'part2: ', Q2DH1
          WRITE(*,*) 'total: ', DQDH1
          WRITE(*,*) 'estifm ', estifm (nodb, NodA + 2)
          WRITE(*,*) '---'
          !testing-

cipk oct03 add widem
cipk jul04
          IF(GRAV .GT. 30.) THEN
CIPK JUN05
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q1DH2,WH1/3.2808,WS1/3.2808,WV1/3.2808,(WH2+DH/2.)/3.2808
     +        ,(WS2+DH/2.)/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
              Q1DH2=Q1DH2*10.7636

            !***********************************
            !for table- or function defined weir
            !***********************************
            ELSE
              CALL WTFORM(Q1DH2,NCTREF(IMAT(NN)),WS1,WS2+DH/2.)
            ENDIF

!*********************
!SI- units: dQ/-dhow
          ELSE
CIPK JUN05
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q1DH2,WH1,WS1,WV1,WH2+DH/2.,WS2+DH/2.,WV2,WEC,WLN,ITP,IWTYP
     +        ,widem)

            !***********************************
            !for table- or function defined weir
            !***********************************
            ELSE
              !WRITE(*,*) 'Calculate dQ/+dhuw at cstrc', ws2, ws2+dh/2.
              CALL WTFORM(Q1DH2,NCTREF(IMAT(NN)),WS1,WS2+DH/2.)
            ENDIF
          ENDIF

          !Scale this part of the numerical derivative by the direction
          Q1DH2 = Q1DH2 * QFACT1

!end SI- units: dQ/-dhow
!*********************

          IWTYP=N1
cipk jul04
          IF(GRAV .GT. 30.) THEN
CIPK JUN05
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +        (Q2DH2,WH1/3.2808,WS1/3.2808,WV1/3.2808,(WH2-DH/2.)/3.2808
     +        ,(WS2-DH/2.)/3.2808,WV2/3.2808,WEC/3.2808,WLN/3.2808
     +        ,ITP,IWTYP,widem/3.2808)
              Q2DH2=Q2DH2*10.7636
CIPK JAN05
            ELSE
CIPK NOV06   FIX BUG IN SIGN
              CALL WTFORM(Q2DH2,NCTREF(IMAT(NN)),WS1,WS2-DH/2.)
            ENDIF

!*********************
!end SI- units: dQ/-dhow
          ELSE
CIPK JUN05
            IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
              CALL WFORM
     +      (Q2DH2,WH1,WS1,WV1,WH2-DH/2.,WS2-DH/2.,WV2,WEC,WLN,ITP,IWTYP
     +                ,widem)

            !***********************************
            !for table- or function defined weir
            !***********************************
            ELSE
CIPK NOV06   FIX BUG IN SIGN
              !WRITE(*,*) 'Calculate dQ/-dhuw at cstrc', ws2, ws2-dh/2.
              CALL WTFORM(Q2DH2,NCTREF(IMAT(NN)),WS1,WS2-DH/2.)
            ENDIF
          ENDIF

          !Scale this part of the numerical derivative by the direction
          Q2DH2 = Q2DH2 * QFACT1
!end SI- units: dQ/-dhow
!*********************

CIPK JUN04            DQDH2=(Q1DH2-Q1T)/DH
          DQDH2 = (Q1DH2 - Q2DH2)/ DH
          ESTIFM (NodB, NodB + 2) = ESTIFM (NodB, NodB + 2) + DQDH2

          !testing
          WRITE(*,*) '---'
          WRITE(*,*) 'dQdh2, Unterwasser'
          WRITE(*,*) '------------------'
          WRITE(*,*) 'part1: ', Q1DH2
          WRITE(*,*) 'part2: ', Q2DH2
          WRITE(*,*) 'total: ', DQDH2
          WRITE(*,*) 'estifm ', estifm (nodb, Nodb + 2)
          WRITE(*,*) '---'
          !testing-

!**********************************
!end SI- units: dQ/dvow and dQ/dvuw
          IF(GRAV < 30. .and. UseEnergyCstrc == 1) THEN
            !***********************************
            !for table- or function defined weir
            !***********************************
            !dQ/+dvow
            WS1PlusDv = vel (3, nop (nn, 1)) + ao (nop (nn, 1))
     +                + (u (1) + DV)**2. / (2. * grav)

            !testing
            WRITE(*,*) 'WS1:    ', WS1
            WRITE(*,*) 'WS1+dV: ', WS1PlusDV
            !testing-

            CALL WTFORM (Q1DV1, NCTREF (IMAT (NN)), WS1PlusDV, WS2)
            Q1DV1 = QFACT1 * Q1DV1

            !dQ/-dvow
            WS1MinusDv = vel(3, nop (nn,1)) + ao (nop (nn, 1))
     +                 + (u (1) - DV)**2. / (2. * grav)
            !testing
            WRITE(*,*) 'WS1-dV: ', WS1MinusDV
            !testing-

            CALL WTFORM (Q2DV1, NCTREF (IMAT (NN)), WS1MinusDV, WS2)
            Q2DV1 = QFACT1 * Q2DV1

            !Scale this part of the numerical derivative by the direction
            !DQDV1 = (Q1DV1 - Q2DV1)/ 2.
            DQDV1 = (Q1DV1 - Q2DV1)/ (2. * DV)
            estifm (NodB, NodA) = estifm (NodB, NodA) + DQDV1

            !testing
            WRITE(*,*) '---'
            WRITE(*,*) 'dQdv1, Oberwasser'
            WRITE(*,*) '-----------------'
            WRITE(*,*) 'part1: ', Q1DV1
            WRITE(*,*) 'part2: ', Q2DV1
            WRITE(*,*) 'total: ', DQDV1
            WRITE(*,*) 'estifm ', estifm (nodb, NodA)
            WRITE(*,*) '---'
            !testing-

            !dQ/+dvuw
            WS2PlusDv = vel(3, nop (nn, 3)) + ao (nop (nn, 3))
     +                + (u (3) + DV)**2. / (2. * grav)
            !testing
            WRITE(*,*) 'WS2   : ', WS2
            WRITE(*,*) 'WS2+dV: ', WS2PlusDV
            !testing-

            CALL WTFORM (Q1DV2, NCTREF (IMAT (NN)), WS1, WS2PlusDV)
            Q1DV2 = QFACT1 * Q1DV2

            !dQ/-dvuw
            WS2MinusDv = vel(3, nop (nn, 3)) + ao (nop (nn, 3))
     +                 + (u (3) - DV)**2. / (2. * grav)

            !testing
            WRITE(*,*) 'WS2-DV: ', WS2MinusDV
            !testing-

            CALL WTFORM (Q2DV2, NCTREF (IMAT (NN)), WS1, WS2MinusDV)
            Q2DV2 = QFACT1 * Q2DV2

            !Scale this part of the numerical derivative by the direction
            !DQDV2 = (Q1DV2 - Q2DV2)/ 2.
            DQDV2 = (Q1DV2 - Q2DV2)/ (2. * DV)
            estifm (NodB, NodB) = estifm (NodB, NodB) + DQDV2
            !testing
            WRITE(*,*) '---'
            WRITE(*,*) 'dQdv2, Unterwasser'
            WRITE(*,*) '------------------'
            WRITE(*,*) 'part1: ', Q1DV2
            WRITE(*,*) 'part2: ', Q2DV2
            WRITE(*,*) 'total: ', DQDV2
            WRITE(*,*) 'estifm ', estifm (nodb, Nodb)
            WRITE(*,*) '---'
            pause
            !testing-
          ENDIF

!end SI- units: dQ/dvow and dQ/dvuw
!**********************************

!*********************
!something else
!|||||||||||||||||||||
!vvvvvvvvvvvvvvvvvvvvv
        ELSE

          WS1=WSLL(N1)
          WS2=WSLL(N2)
cipk oct00
          if(isubm(n1) .eq. 2) go to 500

          F(NodB)=WS1-WS2-(U(1)+U(3))**2/20.
c
c      Derivative with respect to u1 and u2
c
          ESTIFM(NodB,NodA)=(U(1)+U(3))/10.
          ESTIFM(NodB,NodB)=(U(1)+U(3))/10.
c
c      Derivative with respect to h1 and h2  (first term)
c
          ESTIFM(NodB,NodA+2)=-1.
          ESTIFM(NodB,NodB+2)=+1.

        ENDIF
  500   CONTINUE

      ENDIF
      RETURN
      END
