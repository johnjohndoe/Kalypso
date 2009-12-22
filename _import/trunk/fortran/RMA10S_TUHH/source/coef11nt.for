CIPK  LAST UPDATE AUG 22 2007 UPDATE TO BLKECOM
cipk  last update feb 26 2006 add logic for loads from 1-d structures
cipk  last update nov 28 2006 allow for 1-d control structures
cipk  last update jul 05 2006 use perim for hydraulic radius in friction
CIPK  LAST UPDATE MAY 30 2006 CORRECT FRICTION SUBSCRIPT AND H DEFINITION
CNis  LAST UPDATE APR XX 2006 Adding flow equation of Darcy-Weisbach
CIPK  LAST UPDATE mar 23 2006 correct ice initial values 
CIPK  LAST UPDATE SEP 26 2004  ADD MAH AND MAT OPTION
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE MAY 03 2004 ALLOW FOR LOAD APPLIED ONLY AS MASS
CIPK  LAST UPDATE AUG 06 2003 ADD TEST TO REMOVE STRESSES WHEN DRY
cipk  last update jan 13 2002 add momentum to element sources
CIPK  LAST UPDATE OCT  4 2002 ADD ICE FORMULATION
CIPK  LAST UPDATE AUG 28 2002 SET WIND STRESS AND WAVE STRESS TO ZERO BELOW A THRESHOLD DEPTH
CIPK  LAST UPDATE MAY 28 2002 ADD SURFACE STRESS INPUT
cipk  last update mar 26 2001 set sidf zero for dry nodes
CIPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
CIPK  LAST UPDATE DEC 21 2000 Setup for gate structure
cipk  last update Nov 12 1999 allow for collapsing 3-d to 2-d
CIPK  LAST UPDATE APRIL 27 1999 Fix to use mat instead of nr for material type test
cipk  last update Jan 6 1999 initialize AKE correctly
cipk  last update Nov 12 add surface friction
cipk  last update Aug 6 1998 complete division by xht for transport eqn
C     Last change:  WP    2 Jun 2008    9:55 am
CIPK  LAST UPDATED NOVEMBER 13 1997
CIPK  LAST UPDATED MAY 1 1996
CIPK LAST UPDATED SEP 7 1995
      SUBROUTINE COEF1NT(NN,NTX)
      USE COEF1MOD
      USE BLKHMOD
      USE ICE2MOD
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSSTMOD
      USE BLKSANMOD
      USE BLKSUBMOD
CIPK AUG07
      USE BLKECOM
!NiS,apr06: adding block for DARCY-WEISBACH friction
      USE PARAKalyps
      USE Para1DPoly
!-
      SAVE

!NiS,jul06: There's a problem with the data types while calling amf. In other subroutines amf is called by
!           passing the value directly as vel(3,n) (real kind=8). In this subroutine the vel(3,n) value is
!           stored in a local copy that is implicitly real, kind=4. All the temporary values are now declared
!           also as real, kind=8.
      REAL (KIND = 8) :: HS, HD, HD1, HDX, DUM1, HS1, HSX
!-

C
      INTEGER :: PolyPos, findpolynom
      REAL (KIND = 8) :: calcpolynomial
      REAL (KIND = 8) :: h, rhy
      REAL (KIND = 8) :: lamP, lamKS, lamDunes, lamTot

cipk dec00 add Q1
C-
CIPK AUG07      DIMENSION NCON(20)

C-
cipk sep95 add real*8
      REAL*8 SALT
      DATA FCOEF/14.57/
C
cipk jan99 initialize AKE
      AKE=1.0

      IF (GRAV .LT. 32.)  THEN
        FCOEF = GRAV
      ELSE
        FCOEF = GRAV/2.208
      ENDIF

      !testoutput variable
      !0 : switched of
      !1 : switched on
      !the outputs are very rudimentary
      testoutput = 0


C-
C-.....ASSIGN PROPER COEFS.....
C-
C  Test for width > 0
      N1 = NOP(NN,1)
      N3 = NOP(NN,3)

      !check, whether necessary informations are present
      IF ((WIDTH(N1) .LE. 0.0  .OR.  WIDTH(N3) .LE. 0.0) .and.
     +     imat(nn) < 901)  THEN

CIPK SEP04 CREATE ERROR FILE
        !ERROR - WIDTH MISSING FOR NODES
        call ErrorMessageAndStop(1103, NN,
     +       0.5 * (cord (nop (nn, 1), 1) + cord (nop (nn, 3), 1)),
     +       0.5 * (cord (nop (nn, 1), 2) + cord (nop (nn, 3), 2)))

      ELSEIF (imat(nn) > 903) then
        WRITE(*,*) 'Processing control structure'
        WRITE(*,*) 'Element: ', nn, '; Material: ', imat(nn)
      else
        WRITE(*,*) 'Processing junction element: ', nn
      ENDIF
C
      IF(ITEQV(MAXN) .EQ. 5) CALL ARAA(NN)
      TEL=AREA(NN)
      AREA(NN)=0.
cipk nov97
      TVOL(NN)=0.

      IF(ITEQV(MAXN) .EQ. 5) THEN
        DO 61 N=1,8
          NCON(N)=NOPS(NN,N)
          IF(NCON(N) .NE. 0) NCN=N
   61   CONTINUE
      ELSE
        NCN=NCORN(NN)
        DO 63 N=1,NCN
          NCON(N)=NOP(NN,N)
   63   CONTINUE
      ENDIF
      IF(NCN .EQ. 5  .AND.  IMAT(NN) .LT. 900) NCN=3
cipk nov97
      ncnx=2
c
c     Initialize AME and DAME
c
      IF (IDNOPT.LT.0) THEN
         DO M = 1, NCNX
           MC = 2 * M - 1
           N = NOP(NN,MC)
           HS = VEL(3,N)
           ISWT = 0
           CALL AMF(DUM1,HS,AKP(N),ADT(N),ADB(N),AME(M),DAME(M),ISWT)
         END DO
      ENDIF

CIPK OCT02  add logic to make ice cover functions linear
      IF(ICESW .GT. 0) THEN
        THKI(1)=ICETHK(NOP(NN,1))
        THKI(3)=ICETHK(NOP(NN,3))
        QWLI(1)=QICE(NOP(NN,1))
        QWLI(3)=QICE(NOP(NN,3))
      ELSE
cipk mar06      
        THKI(1)=0.
        QWLI(1)=0.
        THKI(3)=0.
        QWLI(3)=0.
      ENDIF

C-
C- INITIALIZE MATRICES AND VARIABLES
C-
      NEF=NCN*NDF
      DO 77 I=1,NEF
      F(I) = 0.0
      DO 77 J=1,NEF
   77 ESTIFM(I,J) = 0.0
cipk oct98 update to f90
      IMMT=IMAT(NN)
C
Cipknov99     revise to test for collapse from 2-d vertical
C
      MR=MOD(IMMT,1000)

cipk dec00 allow gate type elements

      IF(MR .GT. 900  .AND. IGTP(NN) .EQ. 0) GO TO 2000

      IF(MR .LT. 900) THEN
       
        MR=MOD(IMMT,100)
c      ELSEIF(IGTP(NN) .GT. 0.) THEN
c        NGT=MR-900
c        DO N=1,3
c          NJ=NOP(NN,N)
c          ACR=(2.*WIDTH(NJ)+VEL(3,NJ)*(SS1(NJ)+SS2(NJ)))/2.*VEL(3,NJ)
c          Q1(N)=ACR*SQRT(VEL(1,NJ)**2+VEL(2,NJ)**2)
c        ENDDO
c        QFLOW=(Q1(1)+Q1(3)+4.*Q1(2))/6.
c        IF(QFLOW .GT. AJ(NGT)) THEN
c          CJ(NGT)=(QFLOW/AJ(NGT))**4* CJ(NGT)
c        ELSEIF(CJ(NGT) .GT. 1.) THEN
c          CJ(NGT)=(QFLOW/AJ(NGT))**4* CJ(NGT)
c        ENDIF
c        IF(CJ(NGT) .LT. 1.) CJ(NGT)=1.0
cipk dec00        WRITE(75,*) 'CJ, QFLOW', CJ(NGT),QFLOW, AJ(NGT)
      ENDIF

      MAT=MR

cipk dec00 skip out for active gate closure

      IF(IGTP(NN) .NE. 0) THEN
        IF(IGTCL(NN) .EQ. 1) THEN

          NGT=IMAT(NN)-900
          IF(NTX .EQ. 1) THEN
            AREA(NN)=TEL
            RETURN
	    ENDIF
	  ENDIF
      ENDIF

C
      ROAVG=1.935
      IF (GRAV .LT. 32.)  ROAVG = 516. * 1.935
C
      IF(NTX .EQ. 0) THEN
        N1=NCON(1)
        N2=NCON(3)
        TH(NN)=ATAN2(CORD(N2,2)-CORD(N1,2),CORD(N2,1)-CORD(N1,1))
      ELSE
C-
C......COMPUTE AVERAGE DENSITY
C-
C     ICT=0
C     ROAVG=0.
C     DO 70 J=1,NCN,2
C     M=NCON(J)
C     ROAVG=ROAVG+DEN(M)
C     ICT=ICT+1
C  70 CONTINUE
C     ROAVG=ROAVG/FLOAT(ICT)
c        EPSX = ORT(MR,1)/ROAVG
        EPSX = EEXXYY(1,NN)/ROAVG
        FFACT=0.

cipk nov98 adjust for top friction
cipk apr999 fix nr to MAT
      IF(ORT(MAT,5) .GT. 1.  .or.  ort(MAT,13) .gt. 1.) then
        FFACT = GRAV/(CHEZ(NN)+ort(MAT,13))**2
      endif

CIPK        DIFX=ORT(MR,8)
        DIFX=EEXXYY(5,NN)
      ENDIF
      CXX=COS(TH(NN))
      SAA=SIN(TH(NN))
      NGP=4
      NCNX=2
C-
C-.....COMPUTE LOCAL CORDS.....
C-
      NR=NCON(1)
      DO 100 K = 1, NCN
      N=NCON(K)
      !nis,oct06,com: angular difference between nodal tangent and element direction
      ANGDIF=TH(NN)-ALFA(N)
      !nis,oct06,com: fixing direction factor in dependency of nodal vector direction compared to element direction
      IF(ABS(ANGDIF) .GT. 1.5708  .AND.  ABS(ANGDIF) .LT. 4.71779) THEN
        !nis,oct06,com: flow direction in opposite to element definition direction
        QFACT(K)=-1.0
        QQFACT(K)=-1.0
      ELSE
        !nis,oct06,com: flow direction in equal direction to element definition direction
        QFACT(K)=1.0
        QQFACT(K)=1.0
      ENDIF
      !nis,oct06,com: distance from first node to actual node (1,2,3) in components
      DX=CORD(N,1)-CORD(NR,1)
      DY=CORD(N,2)-CORD(NR,2)
      !nis,oct06,com: Projection onto the chord between the corner nodes
      !               XL becomes length of chord-component
      !               YL becomes distance of node from chord
      XL(K)=DX*CXX+DY*SAA
      YL(K)=-DX*SAA+DY*CXX
  100 CONTINUE

!nis,oct06,com: boundary condition
CIPK JAN03
      EINA=EINX(NN)*CX+EINY(NN)*SA

!nis,may07: deactivating loop because it doesn't do anything
!      DO 107 K=1,NCN
!C     IF(QFACT(K) .LT. 1.) WRITE(*,4599) NN,K,NCON(K)  ,QFACT(K)
!C4599 FORMAT('FOR ELEMENT NN K NOP  QFACT'/(3I5,F10.2)
!  107 CONTINUE
!-

C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
      TFR=TEL/ABS(XL(3))
      IF(NTX .NE. 0) XL(2)=XL(3)/2.


CIPK MAY04 RESET ELEMENT INFLOW

      IF(INOFLOW(NN) .EQ. 0) THEN
        SIDFQ=SIDF(NN)
      ELSE
        SIDFQ=0.
      ENDIF
      SIDFQQ=SIDF(NN)

!*******************************************************************************
!GAUSS NODE LOOP GAUSS NODE LOOP GAUSS NODE LOOP GAUSS NODE LOOP GAUSS NODE LOOP
!*******************************************************************************

      !nis,oct06,com: Loop over Gauss nodes
      DO 500 I = 1, NGP
      TEMP=(DNAL(2,I)*XL(2)+DNAL(3,I)*XL(3))
C-
C......DEFINE SHAPE FUNCTIONS
C-

      !nis,oct06,com: quadratic weighting function (for example Reddy[1984],p.255)
      !nis,oct06,com: (1-xi)(1-2xi) = 1-xi-2xi+2x^2 = psi1 (xi=0   => psi=1)
      !nis,oct06,com: (1-xi)*4*xi   = 4xi - 4xi^2   = psi2 (xi=1/2 => psi=1)
      !nis,oct06,com: (2xi-1)*xi    = 2xi^2 - xi    = psi3 (xi=1   => psi=1)
      XN(1)=(1.-AFACT(I))*(1.-2.*AFACT(I))
      XN(2)=(1.-AFACT(I))*4.*AFACT(I)
      XN(3)=(2.*AFACT(I)-1.)*AFACT(I)

      !nis,oct06,com: derivatives of the weighting functions devided by 'TEMP'
      !nis,oct06,com: TEMP is not yet clear
      DNX(1)=(4.*AFACT(I)-3.)/TEMP
      DNX(2)=(4.-8.*AFACT(I))/TEMP
      DNX(3)=(4.*AFACT(I)-1.)/TEMP
      IF(NTX .EQ. 0) THEN
        DYDX=YL(2)*DNX(2)+YL(3)*DNX(3)
        ALF=ATAN(DYDX)
        CSALF=COS(ALF)
        TEMP=TEMP/CSALF
      ELSE
        TEMP=TEMP*TFR
      ENDIF
      IF(NTX .NE. 0) THEN
        DO 240 J=1,3
          DNX(J)=DNX(J)/TFR
  240   CONTINUE
      ENDIF

      !nis,oct06,com: linear weighting functions
      !               1-xi = psi1 => psi(0) = 1
      !               xi   = psi2 => psi(1) = 1
      XM(1)=1.-AFACT(I)
      XM(2)=AFACT(I)
      !nis,oct06,com: derivatives of linear weighting functions
      !nis,oct06,com: TEMP is not yet clear
      DMX(1)=-1./TEMP
      DMX(2)=1./TEMP
      IF(NSTRT(NCON(2),1) .EQ. 0) THEN
        DO 242 J=1,3
          XO(J)=XN(J)
          DOX(J)=DNX(J)
  242   CONTINUE
      ELSE
        XO(1)=XM(1)
        XO(3)=XM(2)
        DOX(1)=DMX(1)
        DOX(3)=DMX(2)
      ENDIF
      !nis,oct06,com: Calculate the cross sectional values at actual GAUSS point
      N1=NCON(1)
      N2=NCON(3)
      WID=WIDTH(N1)*XM(1)+WIDTH(N2)*XM(2)
      WIDSTR=WIDS(N1)*XM(1)+WIDS(N2)*XM(2)                              APR86
c 	  SSLOP2 side slope side 2
      SSLOP2=XM(1)*SS2(N1)+XM(2)*SS2(N2)

c 	  SSLOP1 side slope side 1
      SSLOP1=XM(1)*SS1(N1)+XM(2)*SS1(N2)

c       SSLOP total side slope 
      SSLOP=XM(1)*(SS1(N1)+SS2(N1))+XM(2)*(SS1(N2)+SS2(N2))
      DWIDX=(WIDTH(N2)-WIDTH(N1))/TEMP
      DSLOX=(SS1(N2)+SS2(N2)-SS1(N1)-SS2(N1))/TEMP
      DSLOX1=(SS1(N2)-SS1(N1))/TEMP
      DSLOX2=(SS2(N2)-SS2(N1))/TEMP

CIPK MAY06 
	IF(NTX .NE. 0) THEN
        H=VEL(3,N1)*XM(1)+VEL(3,N2)*XM(2) 
cipk jul06 define dhdx earlier         
        DHDX=VEL(3,N1)*DMX(1)+VEL(3,N2)*DMX(2)
        DUM=1.
      ELSE
        H=1.0
        DHDX=0.
      ENDIF

c           PERIM Wetted perimeter	      
      PERIM=WID+H*(SQRT(1.+SSLOP1**2)+SQRT(1.+SSLOP2**2))
c           DPERMH rate of change of wetted perimeter wrt H
      DPERMH=SQRT(1.+SSLOP1**2)+SQRT(1.+SSLOP2**2)
c           WSRF water surface width
      WSRF=WID+SSLOP*H

c           WTOT total water surface width including overbank
      WTOT=WSRF+WIDSTR    

c           DACR rate of change of cross-section wrt X
      DACR=H*DWIDX+H**2/2.*DSLOX+DHDX*WSRF

c           ACR cross section area
      ACR=H*(WSRF+WID)*0.5

c           DACRH rate of change of cross-section wrt X wrt H
      DACRH=DWIDX+H*DSLOX+SSLOP*DHDX

c           DACRI rate of change of cross-section wrt X wrt DHDX
      DACRI=WSRF

c           DASHDX rate of change of (cross-sec area/H)wrt X times h**2/2 
      DASHDX=H**2/2.*(DWIDX+(DSLOX*H+SSLOP*DHDX)/2.)

c           DASHDXH rate of change of (cross-sec area)*H/2 wrt X wrt H
      DASHDXH=DWIDX*H+3*H**2*DSLOX/4.+SSLOP*DHDX*H/2.

c           DASHDXI rate of change of (cross-sec area)*H/2 wrt DHDX
      DASHDXI=H**2*SSLOP/4.

c           DAHDH2 = rate of change of (cross-sec area)*H/2 wrt H
      DAHDH2=WID*H+0.75*SSLOP*H**2



CIPK OCT02 GET GAUSS POINT ICE VALUES
      !nis,jun07: The equations add something to an old value what makes it not properly calculated; might be error because of copy and paste
      !GSICE=GSICE+THKI(1)*XM(1)+THKI(3)*XM(2)
      !GSQLW=GSQLW+QWLI(1)*XM(1)+QWLI(3)*XM(2)
      GSICE = THKI(1)*XM(1) + THKI(3)*XM(2)
      GSQLW = QWLI(1)*XM(1) + QWLI(3)*XM(2)
      !-
CIPK FEB07
      EXTL=EXTLDEL(NN)
	IF(NTX .NE. 0) THEN
        H=VEL(3,N1)*XM(1)+VEL(3,N2)*XM(2)
      ELSE
        H=1.0
      ENDIF
      AMW=ABS(TEMP)*HFACT(I)/2.
      AREA(NN)=AREA(NN)+AMW
      IF(NTX .EQ. 0) GO TO 500
cipk nov97
      IF(NTX .EQ. 3) GO TO 276
C-
C...... Change to no multiplication by roavg
C-
C     AMS=AMW
C-
C.....COMPUTE R AND H AND THEIR DERIVATIVES.....
C-
      R = 0.0
      DRDX = 0.0
      BETA1 = 0.0
      SALT=0.0
      DSALDT= 0.0
      DSALDX=0.0
CIPK MAY02
      GAIN=0.
C-
C......ESTABLISH VELOCITIES
C-
      !nis,oct06,com: Calculate the nodal velocities and their derivatives
      DO 250 M=1,NCN
        MR=NCON(M)
        !nis,oct06,com: calculate vx and vy; for 1D, udst and vdst are .eq. 1
        VX(M)=VEL(1,MR)/UDST(MR)
        VY(M)=VEL(2,MR)/VDST(MR)
        ST(M)=VEL(ICK,MR)/SDST(MR)
        !nis,oct06,com: calculate the derivatives of vx and vy
        VDX(M)=VDOT(1,MR)/UDST(MR)
        VDY(M)=VDOT(2,MR)/VDST(MR)
        SDT(M)=VDOT(ICK,MR)/SDST(MR)
        IF(ITEQV(MAXN) .EQ. 5  .AND.  NDEP(MR) .GT. 1) THEN
          NBOT=NREF(MR)+NDEP(MR)-1
          UBFC(M)=UDST(NBOT)
        ELSE
          UBFC(M)=1.0
        ENDIF
  250 CONTINUE

      !nis,oct06,com: Calculate the velocity and further
      DO 270 M=1,NCN
        !nis,oct06,com: elementnode
        MR=NCON(M)
        !nis,oct06,com: angulardifference
        ANGDIF=TH(NN)-ALFA(MR)
        !nis,oct06,com: cos and sin of direction
        CX=COS(ALFA(MR))
        SA=SIN(ALFA(MR))
        !nis,oct06,com: addition of nodal velocities with weighting
        R = R + XN(M) * (VX(M)*CX + VY(M)*SA) * QFACT(M)
        !nis,oct06,com: addition of nodal derivative, derivative is in approximation function
        DRDX = DRDX + DNX(M) * (VX(M)*CX + VY(M)*SA) * QFACT(M)
        !nis,oct06,com: concentration things
        IF(NSTRT(MR,1) .EQ. 0) THEN
          SALT=SALT+XO(M)*ST(M)
          DSALDT=DSALDT+XO(M)*SDT(M)
          DSALDX=DSALDX+DOX(M)*ST(M)
CIPK MAY02
        GAIN=GAIN+XO(M)*GAN(MR)
CIPK FEB07
          IF(ICK .EQ. 6) THEN
            EXTL=EXTL+XO(M)*EXTLD(MR)
          ENDIF
        ENDIF
        IF(ICYC.LT.1) GO TO 270
        !time derivative of velocity
        BETA1 = BETA1 + XN(M) * (VDX(M)*CX + VDY(M)*SA) * QFACT(M)
  270 CONTINUE
C-
C...... AKE is the momentum correction coefficient
C       UBF and VBF are corrections to bottom velocity
C-
      AKE=0.0
      UBF=0.0
      VBF=0.0
      DHDX = 0.0
      DAODX = 0.0
      SIGMAX = 0.0
      BETA3 = 0.0
      RHO=0.0
      DRODX=0.0

cipk APR99 add for more flexible width term
      bsel=0.
      abed=0.
      wssg=0.

      DO 275 M=1,NCNX
        MC = 2*M - 1
        MR=NCON(MC)
        AKE=AKE+XM(M)*UUDST(MR)
        UBF=UBF+XM(M)*UBFC(MC)
        BETA3 = BETA3 + XM (M) * VDOT (3, MR)
        DHDX = DHDX + DMX(M)*VEL(3,MR)
cipk nov97      DAODX = DAODX + DMX(M)*AO(MR)
        IF (IDNOPT.GE.0) THEN
          DAODX = DAODX + DMX(M)*AO(MR)
          abed=abed+xm(m)*ao(mr)
        ELSE
          DAODX = DAODX + DMX(M)*(AME(M)+ADO(MR))
cipk mar01 fix ao to ado
          abed=abed+xm(m)*(ado(mr)+ame(m))
        ENDIF
cipk apr99 add line for storage sideslope
        bsel=bsel+xm(m)*widbs(mr)
        wssg=wssg+xm(m)*wss(mr)
        RHO=RHO+XM(M)*DEN(MR)
        DRODX=DRODX+DMX(M)*DEN(MR)
c      IF(ICYC .LT.1) GO TO 275
cipk feb05      SIGMAX=SIGMAX+XM(M)*(SIGMA(MR,1)*CXX+SIGMA(MR,2)*SAA)
CIPK MAY02  ADD STRESS TERM
        SIGMAX = SIGMAX+XM(M)*((SIGMA(MR,1)+STRESS(MR,1))*CXX
     +                        +(SIGMA(MR,2)+STRESS(MR,2))*SAA)
  275 CONTINUE
CIPK NOV97
  276 CONTINUE
      AZER=AO(N1)*XM(1)+AO(N2)*XM(2)
      XHT=ELEV-AZER
      AMS=AMW*RHO
      AMT=AMW*RHO
      AMU=AMW

cipk aug03 ADD TEST TO REOVE STRESSES IF DRY
      IF(H+ABED .LT. AZER) THEN
	  SIGMAX=0.
	ENDIF

cipk apr99 add for sloping storage

      if(h+abed .lt. bsel) then
        widstrt=0.
        astc=0.
      elseif(bsel .gt. abed+1.e-05) then
        widstrt=(-bsel+h+abed)*wssg
        astc=(h+azer-bsel)*widstrt/2.
        if(wssg .gt. 0.) then
          if(widstrt .gt. widstr) then
            excesht=(widstrt-widstr)/wssg
            astc=astc-excesht*(widstrt-widstr)/2.
          endif
        endif
      else
        widstrt=widstr
        astc=widstr*h
      endif
      if(widstrt .lt. widstr) widstr=widstrt

C-
C...... Reset momentum coefficient for later use
C-
      AKE=2.*AKE-1.0
c      WSRF=WID+SSLOP*H
c      WTOT=WSRF+WIDSTR                                                  APR86
c      DACR=H*DWIDX+H**2/2.*DSLOX+DHDX*WSRF
c      ACR=H*(WSRF+WID)*0.5
CIPK NOV97
      TVOL(NN)=TVOL(NN)+AMW
      IF(NTX .EQ. 3) GO TO 500
CIPK AUG95 ADD DEFINITION FOR TOTAL AREA INCLUDING STORAGE
cipk apr99      AST=ACR+H*WIDSTR
      ast=acr+astc
      IF(ICK .EQ. 4) THEN
        DRDS=DRODS(SALT,IGF)
CIPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ELSEIF(ICK .EQ. 5) THEN
        DRDS=DRODTM(SALT,IGF)
CIPK AUG95 GET RATES
        deltt=delt
        CALL MKTEMP(SALT,H,0.,SRCSNK,GRATE,DELTT,MAT,NETYP(NN))
CIPK MAY02
      ELSEIF(ICK .EQ. 6) THEN
C
C     Set up sand transport variables
C
        ALP1=0.0
        ALP2=0.0
        DO M=1,NCN
          MR=NOP(NN,M)
          ALP1=ALP1+ALPHA1(MR)*XN(M)
          ALP2=ALP2+ALPHA2(MR)*XN(M)
        ENDDO

        GRATE=0.
        SRCSNK=0.
        IF(LSAND .GT. 0) THEN
          CALL MKSAND(SALT,H,VSN,SRCSNK,GRATE,NETYP(NN))
	  ENDIF
        DRDS=DRODSD(SALT,IGF)
      ELSE
        DRDS=DRODSD(SALT,IGF)
CIPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
cipk mar05
        DRDS=0.        
      ENDIF
      DRODX = DRDS*DSALDX
      SIGMAX = SIGMAX/RHO

CIPK AUG02 TEST FOR SHALLOW OR NEGATIVE DEPTH TO SET STRESS TO ZERO.
      IF(H .LT. ZSTDEP) THEN
        SIGMAX=0.
      ENDIF

      !nis,jun07: unused
      GHC = GRAV*H
      !-

      VECQ = ABS(R)
      IF(H .LE. 0.) H=0.001

!NiS,apr06: adding possibility of FrictionFactor calculation with
!           Colebrook white to apply DARCY-WEISBACH equation: Therefore,
!           the if-clause has also to be changed because surface friction
!           is deactivated!
!-
cipk nov98 adjust for surface friction
CIPK APR99 ADJUST NR TO MAT
  !NiS,apr06: changing test:
  !    IF(ORT(MAT,5) .GT. 0.  .OR.  ORT(MAT,13) .GT. 0.) THEN
      IF(ORT(MAT,5) .GT. 0.  .OR.  (ORT(MAT,13) .GT. 0. .and.
     +   ORT(MAT,5) /= -1.0)) THEN
  !-
        IF(ORT(MAT,5) .LT. 1.0  .AND.  ORT(MAT,13) .LT. 1.0) then

CIPK MAR01  ADD POTENTIAL FOR VARIABLE MANNING N
          IF(MANMIN(MAT) .GT. 0.) THEN
	      IF(H+ABED .LT. ELMMIN(MAT) ) THEN 
cipk jul06 use perim	      
              FFACT=(MANMIN(MAT))**2*FCOEF/((ACR/PERIM)**0.333)
	      ELSEIF(H+ABED .GT. ELMMAX(MAT) ) THEN 
cipk jul06 use perim	      
              FFACT=(MANMAX(MAT))**2*FCOEF/((ACR/PERIM)**0.333)
	      ELSE
	        FSCL=(H+ABED-ELMMIN(MAT))/(ELMMAX(MAT)-ELMMIN(MAT))
cipk jul06 use perim	      
              FFACT=(MANMIN(MAT)+FSCL*(MANMAX(MAT)-MANMIN(MAT)))**2
     +     	       *FCOEF/((ACR/PERIM)**0.333)
	      ENDIF
CIPK SEP04  ADD MAH OPTION
          ELSEIF(HMAN(MAT,2) .GT. 0  .OR. HMAN(MAT,3) .GT. 0.) THEN
	      TEMAN=0.
            IF(HMAN(MAT,2) .GT. 0) THEN 
	        TEMAN=HMAN(MAT,3)*EXP(-H/HMAN(MAT,2))
	      ENDIF
	      TEMAN=TEMAN+HMAN(MAT,1)/H**HMAN(MAT,4)
cipk jul06 use perim	      
            FFACT=TEMAN**2*FCOEF/((ACR/PERIM)**0.333)
          ELSEIF(MANTAB(MAT,1,2) .GT. 0.) THEN
	      DO K=1,4
	        IF(H .LT. MANTAB(MAT,K,1)) THEN
	          IF(K .EQ. 1) THEN
	            TEMAN=MANTAB(MAT,1,2)
	          ELSE
	            FACT=(H-MANTAB(MAT,K-1,1))/
     +                  (MANTAB(MAT,K,1)-MANTAB(MAT,K-1,1))
	            TEMAN=MANTAB(MAT,K-1,2)
     +            +FACT*(MANTAB(MAT,K,2)-MANTAB(MAT,K-1,2))
	          ENDIF
	          GO TO 280
	        ENDIF
	      ENDDO
	      TEMAN=MANTAB(MAT,4,2)
  280       CONTINUE
cipk jul06 use perim	      
            FFACT=TEMAN**2*FCOEF/((ACR/PERIM)**0.333)
          ELSE
!**************************************************************
!
!   DJW 09/02/03 : Friction Factor Modification to adjust for Roughness Calcs
!
!**************************************************************
!
!           FFACT=(ORT(MAT,5)+ORT(MAT,13))**2*FCOEF/(H**0.333)
CIPK MAY06 REPLACE NR WITH MAT
cipk jul06 use perim	      
            FFACT=(ZMANN(NN)+ORT(MAT,13))**2*FCOEF/((ACR/PERIM)**0.333)
!
!**************************************************************
!
!        End DJW Changes
!
!**************************************************************
          ENDIF
        ENDIF


      !NiS,apr06: adding RESISTANCE LAW form COLEBROOK-WHITE for DARCY-WEISBACH-equation:
      ELSEIF (ORT(MAT,5) < 0.0) THEN

        !getting the hydraulic radius
        PERIM = WID + H* (SQRT (1. + SSLOP1**2) + SQRT (1. + SSLOP2**2))
        rhy = ACR/ PERIM

        !calculate lambda
        !nis,aug07: Introducing correction factor for roughness parameters, if Darcy-Weisbach is used
        call darcy(lamTot, vecq, rhy,
     +             ort(imat(nn),15) * correctionKS(nn),
     +             abst(nn)         * correctionAxAy(nn),
     +             durchbaum(nn)    * correctionDp(nn),
     +             nn, morph, gl_bedform, mel, c_wr(nn), 1,
                   !store values for output
     +             lamKS,
     +             lamP,
     +             lamDunes, dset)

        !calculation of friction factor for roughness term in differential equation

        FFACT = lamTot / 8.0

        !initialize in new run
        if (i == 1) then
          lambdaKS (nn) = 0.0
          lambdaP (nn) = 0.0
          lambdaDunes (nn) = 0.0
          lambdaTot (nn) = 0.0
        ENDIF

        !accumulate lambda of element weighted with Gauss integration weights
        lambdaTot (nn) = lambdaTot (nn) + lamTot * hfact(i) / 2.0
        lambdaP (nn) = lambdaP (nn) + lamP * hfact(i) / 2.0
        lambdaKS (nn) = lambdaKS (nn) + lamKS * hfact(i) / 2.0
        lambdaDunes (nn) = lambdaDunes (nn) + lamDunes * hfact(i) / 2.0
      !-

      ENDIF

cipk dec00 modify friction for high flow gates

CIPK MAR01      IF(IGTP(NN) .GT. 0.) THEN
CIPK MAR01        FFACT=FFACT*CJ(NGT)
CIPK MAR01      ENDIF

      TFRIC = 0.0
      IF( VECQ .GT. 1.0E-6 ) TFRIC = FFACT
cipk oct98 update to f90
      IF(MOD(IMMT,100) .GT. 90) GO TO 291
cipk mar01 Clean logic abd modify to set side flows zero for dry locations
      IF( ICYC .GT. 0 ) then
        FRN=ACR*BETA1
      ELSE
        FRN = 0.0
      ENDIF
      IF(H+ABED .GT. AZER) THEN
CIPK MAY04
        SIDFT=SIDFQ
      ELSE
	SIDFT=0.0
	SIDFQQ=0.0
      ENDIF
C
C.....EVALUATE THE BASIC EQUATIONS WITH PRESENT VALUES.....
C
C.....MOMENTUM TERMS.....
C
  
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT  
CIPK JAN03  ADD MOMENTUM 
CIPK MAY04
      FRN = FRN + ACR*AKE*R*DRDX + SIDFT*R - SIDFQ*eina
C
C.....VISCOUS TERMS.....
C
CMAY93      FRN=FRN+EPSX*DRDX*(DACR-ACR*DAODX/XHT)
cipk jan97      FRN=FRN-EPSX*DRDX*ACR*DAODX/XHT
      FRNX=EPSX*DRDX*ACR
C
C.....SURFACE AND BOTTOM SLOPE (PRESSURE) TERMS.....
C
      FRN=FRN+GRAV*(DAODX*ACR-DASHDX)
      FRNX=FRNX-GRAV*ACR*H/2.
C
C.....BOTTOM FRICTION TERMS.....
C
      FRN = FRN + FFACT*VECQ*R*PERIM*UBF**2

cipk mar01   add drag force terms
      FRN = FRN + DRAGX(MAT)*GRAV*ACR*VECQ*R*UBF**2
C-
C-..... WIND TERMS
C-
      FRN=FRN-SIGMAX*WSRF
C
C.....MOTION EQUATIONS.....
C
      IA=1-NDF
      DO 285 M = 1, NCN
      !nis,oct06,com: motion equation
      !               IA becomes: 1, 1+NDF, 1+2*NDF
      !               It is always the equation number of the motion equation no.1
      !-
      IA = IA + NDF
      F(IA) = F(IA) - AMS*(XN(M)*FRN + DNX(M)*FRNX)*QFACT(M)
  285 CONTINUE

      if (testoutput == 1) then
        WRITE(*,*) 'Momentum equation, element: ', nn, 'GaussPt.: ', I
        WRITE(*,*) 'FRN'

        WRITE(*,*) ' Term D: ', + ACR*AKE*R*DRDX
        WRITE(*,*) 'Term E1: ', - GRAV*DASHDX
        WRITE(*,*) ' Term F: ', + FFACT*VECQ*R*PERIM*UBF**2
        WRITE(*,*) ' Term G: ', + GRAV*DAODX*ACR
        WRITE(*,*) ' Term H: ', + SIDFT*R - SIDFQ*eina
        WRITE(*,*) 'DragTer: ', + DRAGX(MAT)*GRAV*ACR*VECQ*R*UBF**2
        WRITE(*,*) 'WindTer: ', - SIGMAX*WSRF

        WRITE(*,*)
        WRITE(*,*) 'FRNX'
        WRITE(*,*) 'Term E2: ', - GRAV * ACR * H / 2.

        WRITE(*,*) 'Einzelterme: ', FRN, FRNX
        do m = 1, 3
          WRITE(*,*) 'weighting: ', m, xn(m), dnx(m)
          WRITE(*,*) 'assembled value without weighting: ',
     +    (xn(l) * FRN + dnx(l) * FRNX)
          WRITE(*,*) 'assembled value    with weighting: ',
     +    ams * (xn(l) * FRN + dnx(l) * FRNX)
        end do
        !pause
      endif

C
C.....CONTINUITY EQUATION.....
C
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT
      FRNC = ACR * DRDX + DACR * R - SIDFT
      IF (ICYC > 0) FRNC = FRNC + BETA3 * WTOT
      DO M = 1, NCNX
        IA = 3 + 2 * NDF * (M - 1)
        !nis,oct,com: continuity equation, just for the corner nodes
        !             IA becomes: 3, 3+2NDF
        F (IA) = F(IA) - AMW * XM (M) * FRNC
      ENDDO



C-
C......THE SALINITY EQUATION
C-
  291 CONTINUE

CIPK MAY02
      IF(ICK .EQ. 7) THEN
        DIFX=0.
ccc        WRITE(110,'(I8,6F15.6)')NN,DSALDX,DSALDT,GAIN,SALT,DIFX,R
        FRNX=AMU*DIFX*DSALDX*ACR

        FRN=AMU*(AST*(-GAIN)+ACR*R*DSALDX
     +              -SIDFT*(SIDQ(NN,ICK-4)-SALT)-EXTL)
CIPK FEB07 ADD EXTL      
        IF( ICYC .GT. 0) FRN=FRN+AMU*DSALDT*AST

        IA=-4
        DO M=1,NCNX
          IA=IA+8

          F(IA)=F(IA)-(XM(M)*FRN+DMX(M)*FRNX)
        ENDDO
      ELSE
        FRNX=AMU*DIFX*DSALDX*ACR
CMAY93      FRN=AMU*(ACR*R*DSALDX+DIFX*DACR*DSALDX)-AMW*DIFX*DSALDX*ACR*DAODX
CMAY93     +    -AMU*ACR*SIDF(NN)*SIDQ(NN,ICK-3)
CAUG95      FRN=AMU*ACR*R*DSALDX-AMW*DIFX*DSALDX*ACR*DAODX
CAUG95     +    -AMU*ACR*SIDF(NN)*SIDQ(NN,ICK-3)
CAUG95     +    -AMU*AST*(SRCSNK+GRATE*SALT)
CIPK NOV97 ADJUST EQUATION FOR NEW UNITS
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT THEN SIDFQQ IN MAY04
      FRN=AMU*(AST*(-SRCSNK-GRATE*SALT)+ACR*R*DSALDX
     +              -SIDFQQ*(SIDQ(NN,ICK-3)-SALT)-EXTL)
cipk nov97      FRN=AMU*(AST*(-SRCSNK-GRATE*SALT)+ACR*(R*DSALDX
cipk nov97     +              -SIDF(NN)*(SIDQ(NN,ICK-3)-SALT)))
cipk jan97     +    -AMW*DIFX*DSALDX*ACR*DAODX
CIPK MAY96 ADD SIDF*SALT IN ABOVE
CIPK AUG95 LINES ABOVE ALTERED FOR SOURCES CHANGE LINE BELOW TO MULT BY AST
      IF( ICYC .GT. 0) FRN=FRN+AMU*DSALDT*AST
C      IF( ICYC .GT. 0) FRN=FRN+AMU*DSALDT*ACR
      IA=0
      DO 295 M=1,NCN
      IA=IA+4
      IF(NSTRT(NCON(M),1) .EQ. 0) THEN
cipk aug98
        F(IA)=F(IA)-(XO(M)*FRN+DOX(M)*FRNX)
      ENDIF
  295 CONTINUE
      ENDIF
cipk oct98 update to f90
      IF(MOD(IMMT,100) .GT. 90) GO TO 380
C
C.....FORM THE X MOTION EQUATIONS.....
C
C.....FLOW TERMS.....
C
C.....INERTIAL COMPONENTS.....
C
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT
      T1=AMS*(AKE*DRDX*ACR+TFRIC*2.*VECQ*UBF**2*PERIM
     +   +SIDFT+ 2.*VECQ*UBF**2*ACR*GRAV*DRAGX(MAT))
CIPK MAR01 ADD DRAG     +   +SIDF(NN))
CMAY93      T2=AMS*(AKE*ACR*R+EPSX*(DACR-ACR*DAODX/XHT))
cipk jan97      T2=AMS*(AKE*ACR*R-EPSX*ACR*DAODX/XHT)
      T2=AMS*AKE*ACR*R
      T5=AMS*EPSX*ACR
      IB=1-NDF
      DO 310 N=1,NCN
      IB=IB+NDF
      FEEAN=(XN(N)*T1+DNX(N)*T2)*QQFACT(N)
      FEEBN=T5*DNX(N)*QQFACT(N)
C-
C-.....FORM THE TIME TERMS.....
C-
      IF( ICYC .EQ. 0 ) GO TO 304
      FEEAN=FEEAN+AMS*XN(N)*ACR*ALTM*QQFACT(N)
  304 CONTINUE
      IA=1-NDF
      DO 305 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB) +(XN(M)*FEEAN + DNX(M)*FEEBN)*QFACT(M)
  305 CONTINUE
  310 CONTINUE
C
C.....FORM THE HEAD TERMS.....
C
CMAY93      T1=AMS*((AKE*R*DRDX+GRAV*DAODX)*WSRF+SSLOP/2.*TFRIC*VECQ*R*UBF**2
CMAY93     1      -GRAV*(H*DWIDX+0.75*H**2*DSLOX+DHDX*0.5*SSLOP*H)
CMAY93     1      +EPSX*DRDX*(DWIDX+H*DSLOX+SSLOP*DHDX-DAODX*WSRF/XHT))
CMAY93     +      +GRAV*DAODX*AMT*(ACR+WSRF*H)/2.
cipk jan97      T1=AMS*((AKE*R*DRDX+GRAV*DAODX)*WSRF+SSLOP/2.*TFRIC*VECQ*R*UBF**2
cipk jan97     1      -GRAV*(H*DWIDX+0.75*H**2*DSLOX+DHDX*0.5*SSLOP*H)
cipk jan97     +      -EPSX*DRDX*DAODX*WSRF/XHT)
CIPK MAR01      T1=AMS*((AKE*R*DRDX+GRAV*DAODX)*WSRF+SSLOP/2.*TFRIC*VECQ*R*UBF**2
CIPK MAR01     1      -GRAV*(H*DWIDX+0.75*H**2*DSLOX+DHDX*0.5*SSLOP*H))
CIPK MAR01     +      +GRAV*DAODX*AMT*(ACR+WSRF*H)/2.

CIPK MAR01 ADD DRAG TERM
      T1=AMS*((AKE*R*DRDX+GRAV*DAODX)*WSRF+DPERMH*TFRIC*VECQ*R*UBF**2
     1      -GRAV*DASHDXH
     +      +WSRF*GRAV*VECQ*R*UBF**2*DRAGX(MAT)     )


C    +      +GRAV*DAODX*AMT*(ACR*DHDX+WSRF*H)/2.
CMAY93      T2=AMS*(DRDX*EPSX*WSRF-0.25*GRAV*SSLOP*H**2)
      T2=-AMS*GRAV*DASHDXI
      T4=AMS*(EPSX*DRDX*WSRF-GRAV*DAHDH2)
      IB=3-2*NDF
      DO 325 N=1,NCNX
      IB=IB+2*NDF
cipk nov97      FEEAN=XM(N)*T1+DMX(N)*T2
      IF (IDNOPT.GE.0) THEN
        FEEAN=XM(N)*T1+DMX(N)*T2
      ELSE
        FEEAN=XM(N)*T1+DMX(N)*(T2+AMS*GRAV*ACR*DAME(N))
      ENDIF
      FEEBN=XM(N)*T4
C-
C-.....FORM THE TIME TERMS.....
C-
      IF( ICYC .LE. 0 ) GO TO 317
      FEEAN=FEEAN+AMS*XM(N)*BETA1*WSRF
  317 CONTINUE
      IA=1-NDF
      DO 320 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB) +(XN(M)*FEEAN + DNX(M)*FEEBN)*QFACT(M)
  320 CONTINUE
  325 CONTINUE
C-
C......FORM THE SALINITY TERMS
C-
      TAB=-AMU*H/2.*DRDS*GRAV*ACR
      TAC=DRDS*AMU*ACR*(R*DRDX+GRAV*DAODX)
     +    -AMU*GRAV*H**2/2.*DRDS*DASHDX
      IF(ICYC .GT. 0) TAC=TAC+AMU*ACR*BETA1*DRDS
      IB=4-NDF
      DO 330 N=1,NCN
        IB=IB+NDF
        IF(NSTRT(NCON(N),1) .EQ. 0) THEN
          FEEAN= XO(N)*TAB
          FEEBN= XO(N)*TAC
          IA=1-NDF
          DO 329 M=1,NCN
            IA=IA+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+(DNX(M)*FEEAN+XN(M)*FEEBN)*
     +                    QFACT(M)
  329     CONTINUE
        ENDIF
  330 CONTINUE
C
C.....FORM THE CONTINUITY EQUATIONS.....
C
      TA = AMW * ACR
      TX = AMW * DACR
      TB = AMW * (DRDX * WSRF + R * DACRH)
      TC = AMW * R * DACRI
      IF (ICYC /= 0) TB = TB + AMW * (ALTM * WSRF + BETA3 * SSLOP)
     +                   + AMW * ALTM * WIDSTR                               APR86
      IA = 3 - 2 * NDF

      DO M = 1, NCNX
        IA = IA + 2 * NDF
        IB = 1 - NDF
        EA = XM (M) * TA
        EB = XM (M) * TX
        !continuity over velocity
        DO N = 1, NCN
          IB=IB+NDF
          ESTIFM (IA, IB) = ESTIFM (IA, IB)
     +                    + (EA * DNX (N) + EB * XN (N)) * QFACT(N)
        ENDDO

        EA = XM (M) * TB
        EB = XM (M) * TC
        IB = 3 - 2 * NDF
        !continuity over depth
        DO N = 1, NCNX
          IB = IB + 2 * NDF
          ESTIFM (IA, IB) = ESTIFM (IA, IB) + XM (N) * EA + DMX (N) * EB
        ENDDO
      ENDDO
C-
C......FORM THE SALINITY EQUATION
C-
C......VELOCITY AND HEAD TERMS
C-
  380 CONTINUE
      T1=AMU*ACR*DSALDX
      T3=AMU*DIFX*DSALDX*WSRF
CMAY93      T5=AMU*(R*DSALDX*WSRF+DIFX*DSALDX*(DWIDX+DSLOX*H+SSLOP*DHDX-DAODX*
CMAY93     +   WSRF/XHT))
cipk jan97      T5=AMU*(R*DSALDX*WSRF-DIFX*DSALDX*DAODX*WSRF/XHT)
      T5=AMU*R*DSALDX*WSRF
      IF(ICYC .GT. 0) T5=T5+AMU*DSALDT*WSRF
      IA=4-NDF
      DO 400 M=1,NCN
        IA=IA+NDF
        IF(NSTRT(NCON(M),1) .EQ. 0) THEN
cipk aug98
          FEEAN=XO(M)*T1
CMAY93          FEECN=XO(M)*T3
          FEEEN=(XO(M)*T5 + DOX(M)*T3)
          IB=1-NDF
          DO 385 N=1,NCN
            IB=IB+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEAN*QFACT(N)
  385     CONTINUE
          IB=3-2*NDF
          DO 390 N=1,NCNX
            IB=IB+2*NDF
CMAY93           ESTIFM(IA,IB)=ESTIFM(IA,IB)+DMX(N)*FEECN+XM(N)*FEEEN
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*FEEEN
  390     CONTINUE
        ENDIF
  400 CONTINUE
C-
C......FOR SALINITY TERMS
C-
CIPK MAY02
      IF(ICK .EQ. 7) THEN
	  T1=-AMU*AST
        IA=-4
        DO M=1,NCNX
          IA=IA+8
          FEEAN=XM(M)*T1
          IB=-4
          DO N=1,NCNX
            IB=IB+8
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+FEEAN*XM(N)+FEEBN*DMX(N)
          ENDDO
  	  ENDDO
	ELSE
CIPK AUG95 REPLACE 2 LINES BELOW TO CHANGE TO AST AND ADD GRATE
CIPK MAY96 ADD SIDF
CIPK NOV97 CHANGE UNITS FOR SIDF
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT THEN WITH SIDFQQ MAY04
      T1= -AMU*(AST*GRATE-SIDFQQ)
      IF(ICYC .GT. 0) T1=T1+AMU*ALTM*AST
C      T1=0.
C      IF(ICYC .GT. 0) T1=AMU*ALTM*ACR
      T2=AMU*DIFX*ACR
CMAY93      T5=AMU*(R*ACR+DIFX*(DACR-ACR*DAODX/XHT))
cipk jan97      T5=AMU*(R*ACR-DIFX*ACR*DAODX/XHT)
      T5=AMU*R*ACR
      IA=0
      DO 420 M=1,NCN
      IA=IA+4
      IF(NSTRT(NCON(M),1) .EQ. 0) THEN
cipk aug98
        FEEAN=XO(M)*T1
        FEEBN=(DOX(M)*T2+XO(M)*T5)
        IB=0
        DO 410 N=1,NCN
          IB=IB+4
          IF(NSTRT(NCON(N),1) .EQ. 0) THEN
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+FEEAN*XO(N)+FEEBN*DOX(N)
          ENDIF
  410   CONTINUE
      ENDIF
  420 CONTINUE
      ENDIF
C-
C......END GAUSS DO LOOP
C-
  500 CONTINUE
      IF(NTX .EQ. 0) RETURN
CIPK NOV97
      IF(NTX .EQ. 3) RETURN
cipk oct98 update to f90
      IF(MOD(IMMT,100) .GT. 90) GO TO 1305
c     WRITE(*,7777) NN,((ESTIFM(I,J),J=1,11,2),I=1,11,2)
c7777 FORMAT(I12/(1P6E12.4))
c     WRITE(*,7777) NN,(F(I),I=1,12)
C-
C...... Compute boundary forces
C-
      DO 650 L=1,NCN,2
      N1=NCON(L)
      IF(MOD(NFIX(N1)/100,10) .EQ. 2) THEN
        XHT=ELEV-AO(N1)
        NA=(L-1)*NDF+1
        RHO=DEN(N1)
        PPL=RHO*GRAV*(WIDTH(N1)+(SS1(N1)+SS2(N1))*SPEC(N1,3)/2.)
     +      *QFACT(L)
        IF(L .EQ. 1) PPL=-PPL
!        WRITE(*,*) 'Rand: ', na, f(na)
        F(NA)=F(NA)-PPL*(SPEC(N1,3)-VEL(3,N1)/2.)*SPEC(N1,3)
        ESTIFM(NA,NA+2)=ESTIFM(NA,NA+2)-PPL*SPEC(N1,3)/2.
      ELSEIF((IBN(N1) .EQ. 1  .OR.  IBN(N1) .GE. 3)
     +    .AND. ISUBM(N1) .EQ. 0) THEN

        IF(NREF(N1) .EQ. 0) THEN
          NA=(L-1)*NDF+1
c         WRITE(*,*) 'IBN=',IBN(N1),NN,NA
          DO 6667 KK=1,NEF
            ESTIFM(NA,KK)=0.
 6667     CONTINUE
          F(NA)=0.
        ENDIF
      ENDIF
  650 CONTINUE
C-
C......Insert boundary flows
C-
      DO 1300 N=1,NCN
      M=NCON(N)
C-
C...... Test for and then retrieve stage flow constants
C-
      IF(ISTLIN(M) .NE. 0) THEN
        J=ISTLIN(M)
        AC1=STQ(J)
        AC2=STQA(J)
        E0=STQE(J)
        CP=STQC(J)
      ELSE
        AC2=0.
      ENDIF
      IF(NFIX(M)/1000.LT.13) GO TO 1300
      IRW=(N-1)*NDF+1
      IRH=IRW+2
      CX=COS(ALFA(M))
      SA=SIN(ALFA(M))
      VT=VEL(1,M)*CX+VEL(2,M)*SA
      AWIDT=(WIDTH(M)*2.+(SS1(M)+SS2(M))*VEL(3,M))/2.
      DO 1200 J=1,NEF
 1200 ESTIFM(IRW,J)=0.
      ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)*AWIDT
      ESTIFM(IRW,IRH)=AREA(NN)*VT*(WIDTH(M)+(SS1(M)+SS2(M))*VEL(3,M))
      F(IRW)=AREA(NN)*(SPEC(M,1)-AWIDT*VT*VEL(3,M))
      IF(AC2 .NE. 0.) THEN
cipk nov97        ESTIFM(IRW,IRH)=ESTIFM(IRW,IRH)-AREA(NN)*(AC2
cipk nov97     +  *CP*(VEL(3,M)+AO(M)-E0)**(CP-1.0))
cipk nov97        F(IRW)=F(IRW)+AREA(NN)*(AC2        *(VEL(3,M)+AO(M)-E0)**CP)
c lcr eq por
        IF (IDNOPT.LT .0) THEN
            HD=VEL(3,M)
          CALL AMF(HS,HD,AKP(M),ADT(M),ADB(M),AML,DUM2,0)
          WSEL = ADO(M)+HS
        ELSE
          WSEL = VEL(3,M)+AO(M)
        ENDIF
        ESTIFM(IRW,IRH)=ESTIFM(IRW,IRH)-AREA(NN)*(AC2
     +                *CP*(WSEL-E0)**(CP-1.0))
        F(IRW)=F(IRW)+AREA(NN)*(AC2*(WSEL-E0)**CP)
      !EFa aug07, stage-flow boundaries (table)
      ELSEIF (istab(m).gt.0.) then
        if (spec(m,1).lt.0.) then
          adir = -1.
        else
          adir = 1.
        end if
        srfel = hel(m) + ao(m)
        call stfltab(m,srfel,dfdh,ff,1)
        estifm(irw,irh) = estifm(irw,irh) - area(nn)*dfdh*adir
        f(irw) = f(irw) + area(nn) * (ff * adir - spec(m,1))
      !-
      ENDIF
 1300 CONTINUE
 1305 CONTINUE
 1320 CONTINUE

      !Correction for Coupling
      CouplingCorrection: do l = 1, ncn, 2

        !get node number
        M = NCON(l)

        !check for Transition membership
        if (TransitionMember (M)) then

          !Find line number and exit, if found
          throughLines: do i = 1, MaxLT
            if (TransLines(i, 3) == M) then
              LiNo = TransLines(i, 2)
              TrID = i
              EXIT throughLines
            end if
          enddo throughLines

          !this is the former momentum equation of the coupling node
          IRW = (l-1) * NDF + 1
          IRH = IRW + 2

          !get flow direction
          CX = COS(alfa(m))
          SA = SIN(alfa(m))
          !get velocity
          VT = vel(1,m) * CX + vel(2,m) * sa

          !reset the Jacobian
          do j = 1, nef
            ESTIFM (irw, j) = 0.0
          end do

          !set residual entry for 1D-node - 2D-line identity
          f (irw) =  vel(3,m) * (width(m)
     +             + 0.5 * (ss1(m) + ss2(m)) * vel(3,m)) * VT
     +             - q2D(TrID)
          !set derivative over velocity
          estifm (irw, irw) = - vel(3,m) * (width(m)
     +             + 0.5 * (ss1(m) + ss2(m)) * vel(3,m))
          !set derivative over depth
          estifm (irw, irh) = - VT * (width(m)+(ss1(m)+ss2(m))*vel(3,m))
        end if
      end do CouplingCorrection
      !-

      !nis,Oct,com: Install element residual values into global vector. NCN.eq.3 for 1D-elements and 1D-2D-elements.
      DO 1050 I=1,NCN
        !nis,Oct06,com: Get actual node number
        J=NCON(I)
        !nis,Oct06,com: Get the first element equation number; first means x-velocity; ndf is fixed depending on what should be calculated.
        IA=NDF*(I-1)
        !nis,Oct06,com: Do for every nodal degree of freedom
        DO 1050 K=1,NDF
          !nis,Oct06,com: Increasing by one means next nodal degree of freedom
          IA=IA+1
          !nis,Oct06,com: Get the global equation number of node-degree-of-freedom
          JA=NBC(J,K)
          !nis,Oct06,com: Jump over deactivated node-degree-of-freedom
          IF(JA.EQ.0) GO TO 1050
          !nis,Oct06,com: Install element residuum F(IA) into global residuum R1(JA)
          R1(JA)=R1(JA)+F(IA)
 1050 CONTINUE
C     WRITE(*,7777) NN,((ESTIFM(I,J),J=1,12),I=1,12)
C     WRITE(*,7777) NN,(F(I),I=1,12)
C     WRITE(*,7778) (R1(N),N=1,NSZF)
C7778 FORMAT(1P5E12.4)

C      DO I=1,12
C        IF(F(I) .NE. 0.) THEN
C          WRITE(235,7780) NN,I,F(I)
C 7780     FORMAT(2I8,1PE15.6)          
C        ENDIF
C        DO J=1,12
C      
C          IF(ESTIFM(I,J) .NE. 0.) THEN
C            WRITE(234,7779) NN,I,J,ESTIFM(I,J)
C 7779       FORMAT(3I8,1PE15.6)        
C          ENDIF
C        ENDDO
C      ENDDO

!estifm-testoutput
!      if (testoutput > -1) then
!        WRITE(9919,*) 'Element ', nn, 'coef1Pol'
!              WRITE(9919, 1233) ( nbc (nop(nn,1), j), j=1,  3, 2),
!     +        nbc (nop(nn,2), 1), ( nbc (nop(nn,3), j), j=1,  3, 2)
!        writematrix: do i = 1, 11, 2
!          if (i == 7) CYCLE writematrix
!
!          if (MOD(i,4) == 1 .or. MOD(i,4) == 2) then
!            WRITE(9919, 1234) nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4))
!     +      , (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
!          elseif (MOD(i,4) == 3 ) then
!            WRITE(9919, 1234) nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4))
!     +      , (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
!          ELSE
!            WRITE(9919, 1234) nbc( nop(nn, i/4 ), 4),
!     +       (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
!          endif
!        end do writematrix
!        WRITE(9919,*)
!        WRITE(9919,*)
! 1233 format (6x, 5(1x, i10))
! 1234 format (i6, 6(1x, f10.2))
!      endif
!-

      RETURN
    
*-
*...... Special case for junction element
*-
 2000 CONTINUE

cipk dec00
C-
C...... Special cases for control structures or junction sources
C-
      IF(IMAT(NN) .GT. 903) THEN
        CALL CSTRC(NN)
        GO TO 1320
      ENDIF

*-
*...... Special case for junction element
*-

      NCN=NCORN(NN)
c     WRITE(*,*) NN,NCN
      F(1)=0.
      N1=NCON(1)
      XHT=1.0
      DO 2010 KK=1,NCN
        N1=NCON(KK)
        IF(N1 .EQ. 0) GO TO 2010
        NA=(KK-1)*NDF+1

        CX = COS (ALFA(N1))
        SA = SIN (ALFA(N1))
        R  = VEL(1, N1) * CX + VEL(2, N1) * SA

        !using geometry-approach (means trapezoidal channel)
        IF (width(n1) /= 0.0) THEN
          !derivative over velocity
          ESTIFM(1, NA) = DIR(N1)
     +      * (WIDTH(N1) + (SS1(N1) + SS2(N1)) / 2. * VEL(3, N1))
     +      * VEL(3,N1) * XHT
          !derivative over depth
          ESTIFM(1, NA+2) = DIR(N1)
     +      * (WIDTH(N1) + (SS1(N1) + SS2(N1)) * VEL(3,N1))
     +      * R * XHT
        !using polynom approach
        ELSE
          PolyPos = findPolynom (PolyRangeA (n1,:), vel(3, n1),
     +                           PolySplitsA (n1), cord (n1, 1),
     +                           cord (n1, 2), n1)
          ah(n1) = calcPolynomial (apoly (PolyPos, n1, 0: 4),
     +                             vel(3, n1), ubound (apoly, 3))
          !derivative over velocity
          ESTIFM(1, NA) = DIR(N1) * ah(n1) * xht
          !derivative over depth
          ESTIFM(1, NA+2) = DIR(N1) * ah(n1) / vel(3, n1) * r * xht
        ENDIF
        !residual error
        F(1) = F(1) - ESTIFM(1, NA) * R

c     write(*,*) 'KK,N1,NA,R,ESTIFM(1,NA+2),F(1)',
c    +  KK,N1,NA,R,ESTIFM(1,NA+2),F(1)

 2010 CONTINUE
      NRX=NCON(1)
      DO 2020 KK=2,NCN
        N1=NCON(KK)
        IF(N1 .EQ. 0) GO TO 2020
        NA=(KK-1)*NDF+1
        ESTIFM(NA,3)=XHT
        ESTIFM(NA,NA+2)=-XHT
CIPK NOV97        F(NA)=XHT*((VEL(3,N1)-VEL(3,NRX))+(AO(N1)-AO(NRX)))
        IF (IDNOPT .LT. 0) THEN           
          HD1 = VEL(3,N1)
          CALL AMF(HS1,HD1,AKP(N1),ADT(N1),ADB(N1),AML,DUM2,0)
          WSEL1 = ADO(N1)+HS1
          HDX = VEL(3,NRX)
          CALL AMF(HSX,HDX,AKP(NRX),ADT(NRX),ADB(NRX),AML,DUM2,0)
          WSELX = ADO(NRX)+HSX
          ELSE
            WSEL1=AO(N1)+VEL(3,N1)
            WSELX=AO(NRX)+VEL(3,NRX)
        ENDIF
        F(NA)=XHT*(WSEL1-WSELX)
 2020 CONTINUE
      GO TO 1320
      END
