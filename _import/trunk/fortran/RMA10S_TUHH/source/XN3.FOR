
      FUNCTION XN3(IT,I,X,Y,Z,LT)
      SAVE
C
C
C
C     function to evaluate shape function for three dimensions
C
C      IT is element type 1 10 point tetrahedron
C                    type 2 15 point prism
C                    type 3 20 point parallelipiped
C                    type 4 13 point rectangular base pyramid
C      I  is shape function number
C      X, Y, Z  are cordinates of point to be evaluated in local coord
C      LT identifies shape function
C                    type 1 is function itself
C                    type 2 is x derivative
C                    type 3 is y derivative
C                    type 4 is z derivative
C
C-
C-
      DIMENSION ITM(20),SI(20,3),IRF(10,2)
      DIMENSION A(5),B(5),C(5),XT(3),YT(3),JX(3),KX(3),XM(5)
     1         ,SN(2),SNX(2),SNY(2),SNZ(2),SHPP(15,4)
C-
      DATA ITM/ 1,0,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0/
      DATA SI/-1.,0.,1.,1.,1.,0.,-1.,-1.,-1.,1.,1.,-1.,
     1        -1.,0.0,1.0,1.0,1.0,0.0,-1.,-1.,
     2        -1.,-1.,-1.,0.0,1.0,1.0,1.0,0.0,-1.,-1.,1.0,1.0,
     3        -1.,-1.,-1.,0.0,1.0,1.0,1.0,0.0,
     4        8*-1.0,4*0.0,8*1.0/
      DATA IRF/1,1,2,2,3,3,1,2,3,4
     1        ,1,2,2,3,3,1,4,4,4,4/
      DATA XT/0.0,1.0,0.0/,YT/-1.0,0.0,1.0/,JX/2,3,1/,KX/3,1,2/
      DATA SHPP/0.0,0.0,1.0,12*0.0,
     +      0.25,-1.0,3.0,-1.0,0.25,4*0.0,0.25,-1.0,0.0,-1.0,0.25,0.0,
     +      0.25,-1.0,0.0,1.0,-0.25,4*0.0,0.25,-1.0,0.0,1.0,-0.25,0.0,
     +      0.25,-1.0,0.0,-1.0,0.25,4*0.0,-0.25,1.0,0.0,1.0,-0.25,0.0/
      DATA NCALL/0/
      IF(IT .EQ. 4) GO TO 80
      IF( IT - 2 ) 500,80,300
C-
C-.....SHAPE FUNCTIONS FOR RIGHT PRISM AND PYRAMID.....
C-
   80 NCALL = NCALL + 1
      IF( NCALL .GT. 1 ) GO TO 125
C-
C-.....CALCULATE INVARIENT TRIANGULAR FUNCTIONS.....
C-
      N = 0
      DO 100 J = 1,5,2
      N = N + 1
      JJ = JX(N)
      KK = KX(N)
      A(J) = XT(JJ)*YT(KK) - XT(KK)*YT(JJ)
      B(J) = YT(JJ) - YT(KK)
      C(J) = XT(KK) - XT(JJ)
  100 CONTINUE
C-
C-.....SHAPE FUNCTION CALCULATIONS.....
C-
  125 DO 130 K = 1, 5, 2
      XM(K) = 0.5*(A(K)+B(K)*X+C(K)*Y)
  130 CONTINUE
C-
C-.....SET INDEXES AND NORMALIZE COORDINATES.....
C-
      ZOOO = Z
      IF( I .LE. 6 ) ZOOO = -Z
      IF(IT .EQ. 2) GO TO 132
      if(x .eq. 1.0) go to 250
C      IF(X .EQ. 1.0) X=0.999
      IF(X .NE. 1.) ZOOO=ZOOO/(1.-X)
      IF(X .EQ. 1.) ZOOO=SIGN(X,ZOOO)
      IF(I .EQ. 3) GO TO 200
  132 IF( I .GT. 6 .AND. I .LT. 10 ) GO TO 170
      L = I
      IF( I .GT. 9 ) L = I - 9
      IF( MOD(L,2) .EQ. 0 ) GO TO 150
C-
C-.....CORNER NODES.....
C-
      GO TO (142,144,146,148),LT
  142 SF =  XM(L)*((2.0*XM(L)-1.0)*(1.0+ZOOO)-(1.0-ZOOO**2))/2.0
      IF(IT .EQ. 4) SF=SF+XM(L)/2.0*X*(1.-ZOOO**2)
      GO TO 180
  144 SF = 0.5*B(L)*((1.0+ZOOO)*(2.0*XM(L)-0.5)-0.5*(1.0-ZOOO**2))
      IF(IT .EQ. 4) THEN
        SF=SF+XM(L)/2.*((2.*XM(L)-1.)*ZOOO/(1.-X)+ZOOO**2+1.)
        SF=SF+B(L)/4.*(1.-ZOOO**2)*X
      ENDIF
      GO TO 180
  146 SF = 0.5*C(L)*((1.0+ZOOO)*(2.0*XM(L)-0.5)-0.5*(1.0-ZOOO**2))
      IF(IT .EQ. 4) SF=SF+C(L)/4.*(1-ZOOO**2)*X
      GO TO 180
  148 SF = XM(L)*(XM(L)+ZOOO-0.5)
      IF(IT .EQ. 4) THEN
        SF=(SF-ZOOO*XM(L))/(1.-X)+XM(L)*ZOOO
      ENDIF
      IF(I .LE. 6) SF = -SF
      GO TO 180
C-
C-.....MID SIDE NODE.....
C-
  150 N1 = L - 1
      N2 = MOD(L+1,6)
      GO TO(152,154,156,158),LT
  152 SF = 2.0*XM(N1)*XM(N2)*(1.0+ZOOO)
      GO TO 180
  154 SF = (1.0+ZOOO)*(XM(N1)*B(N2) + XM(N2)*B(N1))
      IF(IT .EQ. 4) THEN
        SF=SF+2.*XM(N1)*XM(N2)*ZOOO/(1.-X)
      ENDIF
      GO TO 180
  156 SF = (1.0+ZOOO)*( XM(N1)*C(N2)+XM(N2)*C(N1))
      GO TO 180
  158 SF = 2.0*XM(N1)*XM(N2)
      IF(I .LE. 6) SF = -SF
      IF(IT .EQ. 2) GO TO 180
      SF=SF/(1.-X)
      GO TO 180
C-
C-.....MID SIDE RECTANGLE.....
C-
  170 N1 = 1
      IF( I .EQ. 8 ) N1 = 3
      IF( I .EQ. 9 ) N1 = 5
      GO TO (172,174,176,178),LT
  172 SF = XM(N1)*(1.0-ZOOO**2)
      IF(IT .EQ. 4) SF=SF*(1.-X)
      GO TO 180
  174 SF = 0.5*(1.0-ZOOO**2)*B(N1)
      IF(IT .EQ. 4)
     1 SF=SF*(1.-X)-XM(N1)*(ZOOO**2+1.)
      GO TO 180
  176 SF = 0.5*(1.0-ZOOO**2)*C(N1)
      IF(IT .EQ. 4) SF=SF*(1.-X)
      GO TO 180
  178 SF = -2.0*XM(N1)*ZOOO
  180 XN3 = SF
      RETURN
C-
C......SPECIAL CASE FOR NO 3 SHAPE FUNCTION OF PYRAMID
C-
  200 L=I
      GO TO (220,224,228,232),LT
  220 SF=XM(L)*(2.*XM(L)-1.0)
      GO TO 240
  224 SF=B(L)/2.*(4.*XM(L)-1.0)
      GO TO 240
  228 SF=C(L)/2.*(4.*XM(L)-1.0)
      GO TO 240
  232 SF=0.0
  240 XN3=SF
      RETURN
C
C     Special case when x exactly equals 1.0
C
  250 CONTINUE
      XN3=SHPP(I,LT)
      RETURN
C-
C-.....SHAPE FUNCTIONS FOR CUBE.....
C-
  300 CONTINUE
      SX=SI(I,1)*X
      SY=SI(I,2)*Y
      SZ=SI(I,3)*Z
      IF(ITM(I).EQ.0) GO TO 350
C-
C......CORNER NODES
C-
      GO TO (310,320,330,340),LT
  310 XN3=(1.+SX)*(1.+SY)*(1.+SZ)*(SX+SY+SZ-2.)/8.
      RETURN
  320 XN3=(1.+SY)*(1.+SZ)*(2.*SX+SY+SZ-1.)/8.*SI(I,1)
      RETURN
  330 XN3=(1.+SZ)*(1.+SX)*(2.*SY+SZ+SX-1.)/8.*SI(I,2)
      RETURN
  340 XN3=(1.+SX)*(1.+SY)*(2.*SZ+SX+SY-1.)/8.*SI(I,3)
      RETURN
C-
C......MID-SIDE NODES
C-
  350 IF(SI(I,1).EQ.0.) SX=-(X*X)
      IF(SI(I,2).EQ.0.) SY=-(Y*Y)
      IF(SI(I,3).EQ.0.) SZ=-(Z*Z)
      GO TO (360,370,380,390),LT
  360 XN3=(1.+SX)*(1.+SY)*(1.+SZ)/4.
      RETURN
  370 SX=SI(I,1)
      IF(SI(I,1).EQ.0.) SX=-2.*X
      XN3=SX*(1.+SY)*(1.+SZ)/4.
      RETURN
  380 SY=SI(I,2)
      IF(SI(I,2).EQ.0.) SY=-2.*Y
      XN3=SY*(1.+SZ)*(1.+SX)/4.
      RETURN
  390 SZ=SI(I,3)
      IF(SI(I,3).EQ.0.) SZ=-2.*Z
      XN3=SZ*(1.+SX)*(1.+SY)/4.
      RETURN
  500 CONTINUE
C-
C......SECTION FOR TETRAHEDRON
C-
      I1=IRF(I,1)
      I2=IRF(I,2)
      IA=I1
      DO 550 K=1,2
      GO TO (505,515,525,535),IA
  505 SN(K)=1.-X-Y-Z
      SNX(K)=-1.
      SNY(K)=-1.
      SNZ(K)=-1.
      GO TO 550
  515 SN(K)=Z
      SNX(K)=0.
      SNY(K)=0.
      SNZ(K)=1.
      GO TO 550
  525 SN(K)=X
      SNX(K)=1.
      SNY(K)=0.
      SNZ(K)=0.
      GO TO 550
  535 SN(K)=Y
      SNX(K)=0.
      SNY(K)=1.
      SNZ(K)=0.
  550 IA=I2
C-
C......TEST FOR CORNER NODES
C-
      IF(I1 .EQ. I2) GO TO 650
C-
C......FOR MIDSIDES EVALUATE FUNCTION ETC
C-
      GO TO (560,570,580,590),LT
  560 SF=4.*SN(1)*SN(2)
      GO TO 750
  570 SF=4.*(SNX(1)*SN(2)+SN(1)*SNX(2))
      GO TO 750
  580 SF=4.*(SNY(1)*SN(2)+SN(1)*SNY(2))
      GO TO 750
  590 SF=4.*(SNZ(1)*SN(2)+SN(1)*SNZ(2))
      GO TO 750
  650 CONTINUE
C-
C......CORNER NODE EVALUATION
C-
      GO TO (660,670,680,690),LT
  660 SF=SN(1)*(2.*SN(1)-1.)
      GO TO 750
  670 SF=SNX(1)*(4.*SN(1)-1.)
      GO TO 750
  680 SF=SNY(1)*(4.*SN(1)-1.)
      GO TO 750
  690 SF=SNZ(1)*(4.*SN(1)-1.)
C-
C......FINAL STEP
C-
  750 XN3=SF
      RETURN
      END
