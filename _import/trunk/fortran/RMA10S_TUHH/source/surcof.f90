!IPK  LAST UPDATE AUG 22 2007 UPDATE TO BLKECOM
!ipk  last update may 23 2005 fix wind direction bug
!IPK  LAST UPDATE SEP 26 2004  ADD MAH AND MAT OPTION
!IPK  LAST UPDATE MAY 03 2004 ALLOW FOR LOAD APPLIED ONLY AS MASS
!ipk  LAST UPDATE jun 29 2003 add STRESS component
!IPK  LAST UPDATE OCT  4 2002 ADD ICE FORMULATION
!IPK  LAST UPDATE AUG 28 2002 SET WIND STRESS AND WAVE STRESS TO ZERO BELOW A THRESHOLD DEPTH
!IPK  LAST UPDATE MAY 28 2002 ADD SURFACE STRESS INPUT
!IPK  LAST UPDATE JAN 21 2002 REVISE TO CREATE VELOCITY COMPONENTS 
!ipk  last update oct 10 2001 reformualte to improve integration for type 3 and 4 elements
!IPK  LAST UPDATE MAR 26 2001  ADD SIDF FOR GLOBAL LOADING AS SURFACE FLOW
!IPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
!ipk  last update Dec 8 1999 add computation of element areas for 2-d surface elts
!IPK  LAST UPDATE JAN12 1999 ADD LOGIC FOR 2DV JUNCTIONS
!IPK  LAST UPDATE NOVEMBER 11 1998
!     Last change:  WP   29 Aug 2007    3:45 pm
!ipk last update Aug 5 1998 fix heat budget term
!IPK LAST UPDATED SPE 8 1995
      SUBROUTINE SURCOF(NN,NTX)
      USE COEF2MOD, only:XN,DNX,DNY,XM,DMX,DMY,XL,YL,WAITX
      USE BLKHMOD
      USE BLKSMOD
      USE WATPMOD
      USE ICE2MOD
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSSTMOD
      USE BLKECOM
      SAVE
!
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: h
!
!-
!IPK JUN03
!      COMMON /STR/
!     +  STRESS(MNP,2),STR11(MNP),STR21(MNP),STR10(MNP),STR20(MNP)
      REAL QFACT(3)
!-
      DIMENSION SFACT(4,2),SLOD(2)
!ipk jan99
!
      DIMENSION  WIDTHZ(8)
!
      REAL J11,J12,J21,J22
      DATA SFACT/-1.,1.,-1.,-1.,+1.,-1.,+1.,+1./
!-
!-.....ASSIGN PROPER COEFS.....
!-
      NGP=16
      NCN=NCORN(NN)
      NCNX=NCN/2
!ipk oct98 update to f90
      IMMT=IMAT(NN)
      NM=MOD(IMMT,100)
      CX=COS(TH(NN))
      SAN=SIN(TH(NN))
      NTYPE=IMAT(NN)/1000
      CZ=0.0
      IF(NTYPE == 3) THEN
        CZ=ORT(NM,11)
!ipk nov98 add top friction
      ELSEIF(NTYPE == 1) THEN
        CZ=ORT(NM,13)
      ELSE
        CZ=ORT(NM,5)
      ENDIF
      SLOD(1)=SFACT(NTYPE,1)
      SLOD(2)=SFACT(NTYPE,2)
!-
!- INITIALIZE MATRICES AND VARIABLES
!-
      NEF=NDF*NCN
      DO 77 I=1,NEF
      F(I) = 0.0
      DO 77 J=1,NEF
   77 ESTIFM(I,J) = 0.0
!IPK JULY 1995 MOVE THIS      IF(MOD(IMAT(NN),100) > 90) RETURN
!-
!......TEST FOR NO FRICTION CASE
!-
      IF(NTYPE == 2) THEN
        IF(CZ == 0.) RETURN
      ENDIF
!
!IPK MAY04 RESET ELEMENT INFLOW
!
      IF(INOFLOW(NN) == 0) THEN
        SIDFQ=SIDF(NN)
      ELSE
        SIDFQ=0.
      ENDIF
      SIDFQQ=SIDF(NN)
!
!-
!......SHIFT IF ONE-D ELEMENT
!-
      IF(NCN == 3) GO TO 600
!
!ipk dec99 initialize area
      area(nn)=0.
!
!ipk oct98 update to f90
      IMMT=IMAT(NN)
      IF(MOD(IMMT,100) > 90) RETURN
!c      IF(NTX == 0) RETURN
!-
!......SET REFERENCE LENGTH
!-
!ipk oct98 update to f90
      N1=NOP(NN,1)
      N2=NOP(NN,3)
      N3=NOP(NN,5)
      XL1=SQRT((CORD(N2,1)-CORD(N1,1))**2+(CORD(N2,2)-CORD(N1,2))**2)
      XL2=SQRT((CORD(N3,1)-CORD(N2,1))**2+(CORD(N3,2)-CORD(N2,2))**2)
      IF(XL2 > XL1) XL1=XL2
!-
!-.....COPY PROPER WEIGHTING FUNCTIONS.....
!-
!IPK SEP02  add logic to make ice cover functions linear
      DO I=1,NCN
        IF(ICESW > 0) THEN
          IF(MOD(I,2) == 0 .AND. I < NCN) THEN
            THKI(I)=(ICETHK(NOP(NN,I-1))+ICETHK(NOP(NN,I+1)))/2.
            QWLI(I)=(QICE(NOP(NN,I-1))+QICE(NOP(NN,I+1)))/2.
          ELSEIF(MOD(I,2) == 0 .AND. I == NCN) THEN
            THKI(I)=(ICETHK(NOP(NN,I-1))+ICETHK(NOP(NN,1)))/2.
            QWLI(I)=(QICE(NOP(NN,I-1))+QICE(NOP(NN,1)))/2.
          ELSE
            THKI(I)=ICETHK(NOP(NN,I))
            QWLI(I)=QICE(NOP(NN,I))
          ENDIF
        ELSE
          THKI(I)=0.
          QWLI(I)=0.
        ENDIF
      ENDDO
      NGP = 16
      IF( NCN < 8 ) THEN
        DO 85 M = 1, NGP
          WAITX(M)=WAITTH(M)
   85   CONTINUE
      ELSE
        DO 90 M = 1, NGP
          WAITX(M) = WAITRH(M)
   90   CONTINUE
      ENDIF
!-
!-.....COPY SHAPE FUNCTIONS
!-
      CALL SB2(NCN,NGP)
!-
!......SET UP PASS LIMIT DEPENDING ON ELEMENT TYPE
!-
   93 NPASS=2
      N1=1
!IPK OCT01
      N1A=2
      N2=3
      IF(NTYPE < 3) NPASS=1
!ipk oct01 get direction for type 3 or 4
      if(ntype > 2) then
        DX=CORD(NOP(NN,5),1)-CORD(NOP(NN,1),1)        
        DY=CORD(NOP(NN,5),2)-CORD(NOP(NN,1),2)
        AGD=ATAN2(DY,DX)+1.0
      ELSE
        AGD=0.
      endif
!
!
      DO 550 NPA=1,NPASS
      IF(NPASS == 1 ) THEN
        N1=1
       N2=2
      ENDIF
!-
!-.....COMPUTE LOCAL CORDS.....
!-
!ipk oct98 update to f90
      NR=NOP(NN,1)
!      DXS=0.
!      DYS=0.
!IPK OCT01 CHANGE TO CATCH K=1 CASE
      DO 100 K = 1, NCN
!ipk oct98 update to f90
        N=NOP(NN,K)
        DY=CORD(N,N2)-CORD(NR,N2)
!        DYS=DYS+ABS(DY)
        YL(K)=DY
!
!IPK OCT01  TEST FOR ELEMENT TYPE
        IF(NTYPE < 3) THEN
          DX=CORD(N,N1)-CORD(NR,N1)
!          DXS=DXS+ABS(DX)
          XL(K)=DX
        ELSE
          IF(NPA == 1) THEN
            XL(K)=(CORD(N,1)-CORD(NR,1))*COS(AGD)                       &
     &        +(CORD(N,2)-CORD(NR,2))*SIN(AGD)
          ELSE
            XL(K)=-(CORD(N,1)-CORD(NR,1))*SIN(AGD)                      &
     &        +(CORD(N,2)-CORD(NR,2))*COS(AGD)
          ENDIF
        ENDIF
!
  100 CONTINUE
!-
!-.....COMPUTE ELEMENT EQUATIONS.....
!-
      DO 500 I = 1, NGP
!-
!-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
!-
      J11 = 0.0
      J12 = 0.0
      J21 = 0.0
      J22 = 0.0
      DO 130 K = 2, NCN
      J11 = J11 + DA(K,I) * XL(K)
      J12 = J12 + DA(K,I) * YL(K)
      J21 = J21 + DB(K,I) * XL(K)
      J22 = J22 + DB(K,I) * YL(K)
  130 CONTINUE
      DETJ = J11 * J22 - J12 * J21
!IPK OCT01  CHANGE TRANSFER TO 500
      IF(ABS(DETJ) < 1.E-8*XL1) GO TO 500
      DO 135 J = 1, NCN
      XN(J) = XNX(J,I)
      DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / DETJ
      DNY(J) = ( J11 * DB(J,I) - J21 * DA(J,I) ) / DETJ
  135 CONTINUE
      AMW = WAITX(I) * DETJ
!
!ipk dec99 get area then skip out
      area(nn)=area(nn)+amw
      if(ntx == 0) go to 500
!
!-
!-     REPEAT FOR LINEAR FUNCTION
!-
      JJ=0
      DO 145 J=1,NCN,2
      JJ=JJ+1
      XM(JJ)=XMX(JJ,I)
      DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/DETJ
      DMY(JJ)=(J11*CB(JJ,I)-J21*CA(JJ,I))/DETJ
  145 CONTINUE
!-
!.....COMPUTE U, V, H AND THEIR DERIVATIVES.....
!-
!IPK AUG95 ADD LOGIC TO GET SAL/TEMP/SED
      U = 0.0
      V = 0.0
      W=0.
      SALT=0.
      DUDX = 0.0
      DUDY = 0.0
      DVDX = 0.0
      DVDY = 0.0
      DO 270 M=1,NCN
!ipk oct98 update to f90
      MR=NOP(NN,M)
      W=W+XN(M)*VVEL(MR)
!IPK JAN02 CHANGE TO VELOCITY COMPONENTS
      U=U+XN(M)*(VEL(1,MR)*cos(agd) + vel(2,mr)*sin(agd))
      V=V+XN(M)*(-vel(1,mr)*sin(agd) + VEL(2,MR)*cos(agd))
      SALT=SALT+XN(M)*VEL(ICK,MR)
      DUDX=DUDX+DNX(M)*(VEL(1,MR)*cos(agd) + vel(2,mr)*sin(agd))
      DUDY=DUDY+DNY(M)*(VEL(1,MR)*cos(agd) + vel(2,mr)*sin(agd))
      DVDX=DVDX+DNX(M)*(-vel(1,mr)*sin(agd) + VEL(2,MR)*cos(agd))
      DVDY=DVDY+DNY(M)*(-vel(1,mr)*sin(agd) + VEL(2,MR)*cos(agd))
!IPK JAN02      U=U+XN(M)*VEL(1,MR)
!IPK JAN02      V=V+XN(M)*VEL(2,MR)
!IPK JAN02      SALT=SALT+XN(M)*VEL(ICK,MR)
!IPK JAN02      DUDX=DUDX+DNX(M)*VEL(1,MR)
!IPK JAN02      DUDY=DUDY+DNY(M)*VEL(1,MR)
!IPK JAN02      DVDX=DVDX+DNX(M)*VEL(2,MR)
!IPK JAN02      DVDY=DVDY+DNY(M)*VEL(2,MR)
  270 CONTINUE
      H = 0.0
      DHDT=0.
      SPC=0.
      DHDX = 0.0
      DHDY = 0.0
      A0=0.
!IPK AUG02
      WSELL=0.
      DAODX = 0.0
      DAODY = 0.0
      SIGMAX=0.0
      SIGMAY=0.0
      Z=0.0
!      P=0.0
      RHO=0.0
      RHOS=0.0
!ipk sep02 add ice parameters
      GSICE=0.
      GSQLW=0.
      DO M=1,NCNX
        MC = 2*M - 1
!ipk oct98 update to f90
        MR=NOP(NN,MC)
        H = H + XM(M)*VEL(3,MR)
        DHDT=DHDT+XM(M)*VDOT(3,MR)
        A0=A0+XM(M)*AO(MR)
!IPK AUG02
        WSELL=WSELL+XM(M)*WSLL(MR)
        Z=Z+XM(M)*CORD(MR,3)
!        P=P+XM(M)*PRESS(MR)
        RHO=RHO+XM(M)*DEN(MR)
        SPC=SPC+XM(M)*SPEC(MR,3)
        DHDX = DHDX + DMX(M)*(VEL(3,MR)+AO(MR))
        DHDY = DHDY + DMY(M)*(VEL(3,MR)+AO(MR))
        DAODX = DAODX + DMX(M)*AO(MR)
        DAODY = DAODY + DMY(M)*AO(MR)
!
!ipk jun03 add STRESS component
!ipk may05 fix wind direction bug
!        SIGMAX=SIGMAX+XM(M)*((SIGMA(MR,1)+stress(mr,1))*CX
!     +                        +(SIGMA(MR,2)+stress(mr,2))*SAN)
!        SIGMAY=SIGMAY+XM(M)*(-(SIGMA(MR,1)+stress(mr,1))*SAN
!     +                        +(SIGMA(MR,2)+stress(mr,2))*CX)
!
        SIGMAX=SIGMAX+XM(M)*((SIGMA(MR,1)+stress(mr,1))*cos(agd)        &
     &                        +(SIGMA(MR,2)+stress(mr,2))*sin(agd))
        SIGMAY=SIGMAY+XM(M)*(-(SIGMA(MR,1)+stress(mr,1))*sin(agd)       &
     &                        +(SIGMA(MR,2)+stress(mr,2))*cos(agd))
!
!IPK APR05
        IF(WSELL < A0) THEN
          SIGMAX=0.
          SIGMAY=0.
        ENDIF
        IF(MR > NPM) MR=NSURF(MR)
        RHOS=RHOS+XM(M)*DEN(MR)
!
!IPK SEP02 GET GAUSS POINT ICE VALUES
        GSICE=GSICE+XM(M)*THKI(MC)
        GSQLW=GSQLW+XM(M)*QWLI(MC)
      ENDDO
!  275 CONTINUE
      XHT=ELEV-A0
!IPK      SIGMAX = SIGMAX*(ELEV-A0)
!IPK      SIGMAY = SIGMAY*(ELEV-A0)
!IPK SEP02 ADD AN ICE THICKNESS TEST FOR WIND STRESS
!
      IF(GSICE <= 0.0001) THEN
        SIGMAX = SIGMAX*H
        SIGMAY = SIGMAY*H
      ELSE
        SIGMAX=0.0
        SIGMAY=0.0
      ENDIF
!IPK SPE02      sigmax=sigmax*h
!IPK SEP02      sigmay=sigmay*h
!
!IPK AUG02 TEST FOR SHALLOW OR NEGATIVE DEPTH TO SET STRESS TO ZERO.
      IF(WSELL-A0 < ZSTDEP) THEN
        SIGMAX=0.
        SIGMAY=0.
      ENDIF
!IPK NOV98      IF(NTYPE == 1) GO TO 400
      IF(NTYPE == 2 ) THEN
!
!
!     BED ELEMENT
!
!
!...... APPLY CONTINUITY CONDITION
!
       W=U*DAODX+V*DAODY
        IA=3-2*NDF
        DO 284 M=1,NCNX
          IA=2*NDF+IA
          TEMPX=-XM(M)*AMW*XHT/XHT
          IB=1-NDF
          DO 283 N=1,NCN
            IB=IB+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*DAODX*XN(N)
            ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+TEMPX*DAODY*XN(N)
  283     CONTINUE
          F(IA)=F(IA)-TEMPX*W
  284   CONTINUE
      ENDIF
      IF(NTYPE < 4) THEN
!
!-
!......INSERT BOTTOM FRICTION
!-
         FAC=0.0
         IF (GRAV < 32.)  THEN
          IF(CZ > 1.0) FAC=GRAV*AMW/CZ**2*XHT*RHO
          IF(CZ <= 1.0 .AND. CZ > 0.) THEN
!IPK MAR01  ADD VARIABLE WALL FRICTION BASED ON ELEVATION
            IF(MANMIN(NM) > 0.) THEN
              IF(H+A0 < ELMMIN(NM) ) THEN 
                FAC = GRAV*AMW* MANMIN(NM)**2*XHT*RHO/(H**0.3333)
              ELSEIF(H+A0 > ELMMAX(NM) ) THEN 
                FAC = GRAV*AMW* MANMAX(NM)**2*XHT*RHO/(H**0.3333)
              ELSE
                FSCL=(H+A0-ELMMIN(NM))/(ELMMAX(NM)-ELMMIN(NM))
                FAC = GRAV*AMW*(MANMIN(NM)+                             &
     &              FSCL*(MANMAX(NM)-MANMIN(NM)))**2*XHT*RHO/(H**0.3333)
              ENDIF
!IPK SEP04  ADD MAH AND MAT OPTION
            ELSEIF(HMAN(NM,2) > 0 .OR. HMAN(NM,3) > 0.) THEN
              TEMAN=0.
              IF(HMAN(NM,2) > 0) THEN 
                  TEMAN=HMAN(NM,3)*EXP(-H/HMAN(NM,2))
              ENDIF
              TEMAN=TEMAN+HMAN(NM,1)/H**HMAN(NM,4)
              FAC=GRAV*AMW*TEMAN**2*XHT*RHO/(H**0.3333)
            ELSEIF(MANTAB(NM,1,2) > 0.) THEN
                DO K=1,4
                IF(H < MANTAB(NM,K,1)) THEN
                  IF(K == 1) THEN
                    TEMAN=MANTAB(NM,1,2)
                  ELSE
                    FACT=(H-MANTAB(NM,K-1,1))/                          &
     &                  (MANTAB(NM,K,1)-MANTAB(NM,K-1,1))
                     TEMAN=MANTAB(NM,K-1,2)                             &
     &            +FACT*(MANTAB(NM,K,2)-MANTAB(NM,K-1,2))
                  ENDIF
                  GO TO 285
                ENDIF
              ENDDO
              TEMAN=MANTAB(NM,4,2)
  285         CONTINUE
              FAC=GRAV*AMW*TEMAN**2*XHT*RHO/(H**0.3333)
!
            ELSE
!
                FAC=GRAV*AMW*CZ**2*XHT*RHO/(H**0.3333)
            ENDIF
          ENDIF
        ELSE
          IF(CZ > 1.0) FAC=GRAV*AMW/CZ**2*XHT*RHO
          IF(CZ <= 1.0 .AND. CZ > 0.) THEN
!IPK MAR01  ADD VARIABLE WALL FRICTION BASED ON ELEVATION
            IF(MANMIN(NM) > 0.) THEN
              IF(H+A0 < ELMMIN(NM) ) THEN 
                FAC = GRAV*AMW* MANMIN(NM)**2*XHT*RHO/(2.21*H**0.3333)
              ELSEIF(H+A0 > ELMMAX(NM) ) THEN 
                FAC = GRAV*AMW* MANMAX(NM)**2*XHT*RHO/(2.21*H**0.3333)
              ELSE
                FSCL=(H+A0-ELMMIN(NM))/(ELMMAX(NM)-ELMMIN(NM))
                FAC = GRAV*AMW*(MANMIN(NM)+FSCL*                        &
     &              (MANMAX(NM)-MANMIN(NM)))**2*XHT*RHO/(2.21*H**0.3333)
              ENDIF
!IPK SEP04  ADD MAH AND MAT OPTION
            ELSEIF(HMAN(NM,2) > 0 .OR. HMAN(NM,3) > 0.) THEN
              TEMAN=0.
              IF(HMAN(NM,2) > 0) THEN 
                  TEMAN=HMAN(NM,3)*EXP(-H/HMAN(NM,2))
              ENDIF
              TEMAN=TEMAN+HMAN(NM,1)/H**HMAN(NM,4)
              FAC=GRAV*AMW*TEMAN**2*XHT*RHO/(2.21*H**0.3333)
            ELSEIF(MANTAB(NM,1,2) > 0.) THEN
                DO K=1,4
                IF(H < MANTAB(NM,K,1)) THEN
                  IF(K == 1) THEN
                    TEMAN=MANTAB(NM,1,2)
                  ELSE
                    FACT=(H-MANTAB(NM,K-1,1))/                          &
     &                  (MANTAB(NM,K,1)-MANTAB(NM,K-1,1))
                     TEMAN=MANTAB(NM,K-1,2)                             &
     &            +FACT*(MANTAB(NM,K,2)-MANTAB(NM,K-1,2))
                  ENDIF
                  GO TO 286
                ENDIF
              ENDDO
              TEMAN=MANTAB(NM,4,2)
  286         CONTINUE
              FAC=GRAV*AMW*TEMAN**2*XHT*RHO/(2.21*H**0.3333)
            ELSE
!
                    FAC=GRAV*AMW*CZ**2*XHT*RHO/(2.21*H**0.3333)
            ENDIF
          ENDIF
        ENDIF
!       IF(CZ <= 1.0) FAC=GRAV*AMW*CZ**2*XHT*RHO/(2.21*H**0.3333)*1.44
        IF(NTYPE == 3) THEN
          FAC=FAC/XHT*H
          IF(AMW < 0.) FAC=-FAC
        ENDIF
!
!      Set initial values for friction
!
        IF(MAXN > 1 .OR. KRESTF == 1 ) THEN
          UF=U
          VF=V
        ELSE
          UF=UINP
          VF=VINP
        ENDIF
        VL=SQRT(U**2+V**2)
        TEMPX=FAC*U*VL
        TEMPY=FAC*V*VL
        VL=SQRT(UF**2+VF**2)
        IF(ABS(VL) < 0.001) GO TO 296
        TEMPXX=FAC/VL*(2.*UF**2+VF**2)
        TEMPXY=FAC/VL*UF*VF
        TEMPYY=FAC/VL*(UF**2+2.*VF**2)
        IA=1-NDF
        DO 295 N=1,NCN
          IA=IA+NDF
          F(IA)=F(IA)-XN(N)*TEMPX
          F(IA+1)=F(IA+1)-XN(N)*TEMPY
          TXXN=TEMPXX*XN(N)
          TXYN=TEMPXY*XN(N)
          TYYN=TEMPYY*XN(N)
          IB=1-NDF
          DO 290 M=1,NCN
            IB=IB+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(M)*TXXN
            ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+XN(M)*TXYN
            ESTIFM(IA+1,IB)=ESTIFM(IA+1,IB)+XN(M)*TXYN
            ESTIFM(IA+1,IB+1)=ESTIFM(IA+1,IB+1)+XN(M)*TYYN
  290     CONTINUE
  295   CONTINUE
  296   CONTINUE
      ENDIF
      IF(NTYPE == 2) THEN
!-
!...... Insert bottom pressure
!-
!        ZA=(ELEV-Z)/XHT
!        PD=P-RHO*GRAV*ZA*H
        AMS=AMW*DAODX
        DO 315 IC=1,2
          IF(IC == 2) AMS=AMW*DAODY
          IA=IC
          TEMP=AMS*H*RHOS*GRAV*H/2.
          DO 310 N=1,NCN
            F(IA)=F(IA)-TEMP*XN(N)
            TEMPX=AMS*RHOS*GRAV*H*XN(N)
            IB=3-2*NDF
            DO 305 M=1,NCNX
              IB=IB+2*NDF
              ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*XM(M)
  305       CONTINUE
            IA=IA+NDF
  310     CONTINUE
  315   CONTINUE
        GO TO 500
      ENDIF
!
!IPK NOV98
      IF(NTYPE == 1) GO TO 400
!
  320 CONTINUE
!      ZA=(ELEV-Z)/XHT
!      PD=P-RHO*GRAV*ZA*H
      TEMP=SLOD(NPA)*AMW*H*RHOS*GRAV*H/2.
      IF(NTYPE == 4) TEMP=AMW*SLOD(NPA)*2.*SPC*RHOS*GRAV*H/2.-TEMP
      IA=2
      IF(NPA == 2) IA=1
      DO 350 N=1,NCN
      F(IA)=F(IA)-TEMP*XN(N)
      IF(NTYPE == 4) GO TO 345
      TEMPX=SLOD(NPA)*AMW*RHOS*GRAV*H*XN(N)
      IB=3-2*NDF
      DO 340 M=1,NCNX
      IB=IB+2*NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*XM(M)
  340 CONTINUE
  345 IA=IA+NDF
  350 CONTINUE
      GO TO 500
!-
!      PROCESS TYPE 1(SURFACE) ELEMENTS
!-
  400 IA=1-NDF
      DO 430 N=1,NCN
      IA=IA+NDF
      F(IA)=F(IA)+XN(N)*SIGMAX*AMW
      F(IA+1)=F(IA+1)+XN(N)*SIGMAY*AMW
  430 CONTINUE
      IA=3-2*NDF
      DO 450 M=1,NCNX
      IA=IA+NDF*2
      TEMPX=AMW*XHT*DHDX*XM(M)/XHT
      TEMPY=AMW*XHT*DHDY*XM(M)/XHT
      IB=1-NDF
      DO 440 N=1,NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*XN(N)
      ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+TEMPY*XN(N)
  440 CONTINUE
      IB=3-2*NDF
      IF(ICYC == 0) GO TO 446
      DO 445 N=1,NCNX
      IB=IB+NDF*2
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+AMW*ALTM*XM(M)*XM(N)
  445 CONTINUE
  446 CONTINUE
      IB=3-2*NDF
      DO 447 N=1,NCNX
        IB=IB+NDF*2
        ESTIFM(IA,IB)=ESTIFM(IA,IB)+AMW*XM(M)*(U*DMX(N)+V*DMY(N))
  447 CONTINUE
!IPK MAR01  ADD SIDF FOR GLOBAL LOADING AS SURFACE FLOW
      F(IA)=F(IA)-XM(M)*AMW*(XHT*(DHDX*U+DHDY*V+DHDT)/XHT - SIDFQ)
!IPK MAY04 USE SIDFQ
  450 CONTINUE
!IPK AUG95 ADD LOGIC FOR SURFACE RATE TERMS FOR HEAT BUDGET
!IPK MAR01 AMEND TO ALLOW FOR SIDF TERMS
      IF(ICK == 5) THEN
        CALL MKTEMP(SALT,H,0.,SRCSNK,GRATE,DELT,NM,NETYP(NN))
      ELSE
        SRCSNK=0.0
        GRATE=0.0
      ENDIF
      IA=4-NDF
      DO M=1,NCN
        IA=IA+NDF
        IB=4-NDF
        DO N=1,NCN
          IB=IB+NDF
!IPK MAR01  ADD SIDF TERMS USE SIDFQQ MAY04
          ESTIFM(IA,IB)=ESTIFM(IA,IB)-AMW*(H*GRATE/XHT+SIDFQQ)          &
     &                     *XN(M)*XN(N)
        ENDDO
!IPK MAR01  ADD SIDF TERMS USE SIDFQQ MAY04
          F(IA)=F(IA)+AMW*(H*(SRCSNK+GRATE*SALT)/XHT+                   &
     &        SIDFQQ*(SIDQ(NN,ICK-3)-SALT))*XN(M)
!ipk aug98 divide by xht in lines above
      ENDDO
!IPK AUG95 END ADDITIONS
  500 CONTINUE
  525 N1=2
      N1A=1
  550 CONTINUE
!
!ipk dec99 add ntx=0 test
      if(ntx == 0) return
!
      GO TO 800
!-
!......PROCESS ONE-D ELEMENTS
!-
  600 CONTINUE
      IF(NTX > 0) THEN
        TEL=AREA(NN)
      ELSE
        AREA(NN)=0.
      ENDIF
      N1=NOP(NN,1)
      N2=NOP(NN,2)
      N3=NOP(NN,3)
!IPK JAN99
      WIDTHZ(1) = WIDTH(N1) + (SS1(N1)+SS2(N1))*(CORD(N1,3)-AO(N1))     &
     &                    * (VEL(3,N1)/(ELEV-AO(N1)) ) 
      WIDTHZ(2) = WIDTH(N3) + (SS1(N3)+SS2(N3))*(CORD(N3,3)-AO(N3))     &
     &                    * (VEL(3,N3)/(ELEV-AO(N3)) )
!IPK OCT02
      THKI(1)=ICETHK(NOP(NN,1))
      THKI(3)=ICETHK(NOP(NN,3))
      QWLI(1)=QICE(NOP(NN,1))
      QWLI(3)=QICE(NOP(NN,3))
      IF(NTX == 0) THEN
        IF(NTYPE > 2) THEN
          KR=NOP(NN,19)
          TH(NN)=TH(KR)
          RETURN
        ENDIF
        DXF=CORD(N3,1)-CORD(N1,1)
        DXM=CORD(N2,1)-CORD(N1,1)
        DYF=CORD(N3,2)-CORD(N1,2)
        DYM=CORD(N2,2)-CORD(N1,2)
        TH(NN)=ATAN2(DYF,DXF)
        CX=COS(TH(NN))
        SAN=SIN(TH(NN))
        XLM= DXM*CX+DYM*SAN
        XLF= DXF*CX+DYF*SAN
        YLM=-DXM*SAN+DYM*CX
        YLF=-DXF*SAN+DYF*CX
        DO 605 I = 1, 4
          TEMP=DNAL(2,I)*XLM+DNAL(3,I)*XLF
!-
!......Define shape functions for length computation
!-
          DNX(2)=(4.-8.*AFACT(I))/TEMP
          DNX(3)=(4.*AFACT(I)-1.)/TEMP
          DYDX=YLM*DNX(2)+YLF*DNX(3)
          ALF=ATAN(DYDX)
          CSALF=COS(ALF)
          TEMP=TEMP/CSALF
          AREA(NN)=AREA(NN)+ABS(TEMP)*HFACT(I)/2.
  605   CONTINUE
        RETURN
      ENDIF
      DO 606 K=1,3
        N=NOP(NN,K)
        ANGDIF=TH(NN)-ALFA(N)
        IF(ABS(ANGDIF) > 1.5708 .AND. ABS(ANGDIF) < 4.7124) THEN
          QFACT(K)=-1.0
        ELSE
          QFACT(K)=1.0
        ENDIF
  606 CONTINUE
      IF(NTYPE == 4) THEN
        XL(1)=CORD(N2,3)-CORD(N1,3)
        XL(2)=CORD(N3,3)-CORD(N1,3)
        TFR=1.
      ELSE
        XL(2)=(CORD(N3,1)-CORD(N1,1))*CX+(CORD(N3,2)-CORD(N1,2))*SAN
        XL(1)=XL(2)/2.
        TFR=TEL/ABS(XL(2))
      ENDIF
      DAODX=(AO(N3)-AO(N1))/(XL(2)*TFR)
      DHDX=(VEL(3,N3)-VEL(3,N1))/(XL(2)*TFR)+DAODX
!      PDN(1)=PRESS(N1)-DEN(N1)*GRAV*VEL(3,N1)*(ELEV-CORD(N1,3))/
!     +         (ELEV-AO(N1))
!      PDN(3)=PRESS(N3)-DEN(N3)*GRAV*VEL(3,N3)*(ELEV-CORD(N3,3))/
!     +         (ELEV-AO(N3))
      DO 750 N=1,4
      XM(1)=1.-AFACT(N)
      XM(2)=AFACT(N)
!IPK JAN99
      WID=XM(1)*WIDTHz(1)+XM(2)*WIDTHz(2)
!
      DHDT = XM(1)*VDOT(3,N1) + XM(2)*VDOT(3,N3)
!
      H=XM(1)*VEL(3,N1)+XM(2)*VEL(3,N3)
      XN(1)=XM(1)*(1.-2.*XM(2))
      XN(2)=XM(1)*4.*XM(2)
      XN(3)=(2.*XM(2)-1.)*XM(2)
      RHO=XM(1)*DEN(N1)+XM(2)*DEN(N3)
!IPK JAN99      NS1=NSURF(N1)
!IPK JAN99      NS3=NSURF(N3)
!IPK JAN99      RHOS=XM(1)*DEN(NS1)+XM(2)*DEN(NS3)
!
      IF (N1 > NPM) THEN
         NN1 = NSURF(N1)
         NN3 = NSURF(N3)
      ELSE
         NN1 = N1
         NN3 = N3
      ENDIF
      RHOS=XM(1)*DEN(NN1)+XM(2)*DEN(NN3)
!
!      ZC=XN(1)*CORD(N1,3)+XN(2)*CORD(N2,3)+XN(3)*CORD(N3,3)
!      RHO=1000.-ZC
!      RHOS=1000.
      AMW=(DNAL(2,N)*XL(1)+DNAL(3,N)*XL(2))/2.*HFACT(N)*WID*TFR
      AMR=AMW*RHO
      DMX(1)=-1./(XL(2)*TFR)
      DMX(2)=1./(XL(2)*TFR)
      A0=XM(1)*AO(N1)+XM(2)*AO(N3)
!IPK AUG02
      WSELL=XM(1)*WSLL(N1)+XM(2)*WSLL(N3)
      XHT=ELEV-A0
      IF(NTYPE == 4) GO TO 730
      U=0.
!IPK AUG95 ADD LOGIC TO GET SALT
      SALT=0.
      DO 610 I=1,NCN
      MR=NOP(NN,I)
      CXA=COS(ALFA(MR))
      SXA=SIN(ALFA(MR))
      U=U+XN(I)*(CXA*VEL(1,MR)+SXA*VEL(2,MR))*QFACT(I)
      SALT=SALT+XN(I)*VEL(ICK,MR)
  610 CONTINUE
!IPK NOV98      IF(NTYPE == 1) GO TO 700
      IF(NTYPE == 2) THEN
!C      IF(NTYPE == 1) GO TO 700
!
!     For bottom elements apply continuity boundary integrals
!
!
        W=U*DAODX
        IA=3-2*NDF
        DO 618 M=1,2
          IA=2*NDF+IA
          TEMPX=-XM(M)*AMW
          IB=1-NDF
          DO 615 I=1,NCN
            IB=IB+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*DAODX*XN(I)*QFACT(I)
  615     CONTINUE
          F(IA)=F(IA)-TEMPX*W
  618   CONTINUE
      ENDIF
!-
!......BOTTOM FRICTION
!-
!IPK JAN99 MOVE JUMP LOCATION
      IF(CZ == 0. ) GO TO 641
!
      TEMP=0.
      IF (GRAV < 32.)  THEN
        IF(CZ > 1.0) TEMP=GRAV*AMR /CZ**2*XHT
        IF(CZ <= 1.0 .AND. CZ > 0.) THEN
!IPK MAR01  ADD VARIABLE WALL FRICTION BASED ON ELEVATION
          IF(MANMIN(NM) > 0.) THEN
            IF(H+A0 < ELMMIN(NM) ) THEN 
              TEMP = GRAV*AMR* MANMIN(NM)**2*XHT/(H**0.3333)
            ELSEIF(H+A0 > ELMMAX(NM) ) THEN 
              TEMP = GRAV*AMR* MANMAX(NM)**2*XHT/(H**0.3333)
            ELSE
              FSCL=(H+A0-ELMMIN(NM))/(ELMMAX(NM)-ELMMIN(NM))
              TEMP = GRAV*AMR*(MANMIN(NM)+                              &
     &             FSCL*(MANMAX(NM)-MANMIN(NM)))**2*XHT/(H**0.3333)
            ENDIF
!IPK SEP04  ADD MAH AND MAT OPTION
          ELSEIF(HMAN(NM,2) > 0 .OR. HMAN(NM,3) > 0.) THEN
            TEMAN=0.
            IF(HMAN(NM,2) > 0) THEN 
                TEMAN=HMAN(NM,3)*EXP(-H/HMAN(NM,2))
            ENDIF
            TEMAN=TEMAN+HMAN(NM,1)/H**HMAN(NM,4)
            TEMP=GRAV*AMR*TEMAN**2*XHT/(H**0.3333)
          ELSEIF(MANTAB(NM,1,2) > 0.) THEN
            DO K=1,4
              IF(H < MANTAB(NM,K,1)) THEN
                IF(K == 1) THEN
                  TEMAN=MANTAB(NM,1,2)
                ELSE
                  FACT=(H-MANTAB(NM,K-1,1))/                            &
     &                (MANTAB(NM,K,1)-MANTAB(NM,K-1,1))
                   TEMAN=MANTAB(NM,K-1,2)                               &
     &            +FACT*(MANTAB(NM,K,2)-MANTAB(NM,K-1,2))
                ENDIF
                GO TO 622
              ENDIF
            ENDDO
            TEMAN=MANTAB(NM,4,2)
  622       CONTINUE
            TEMP=GRAV*AMR*TEMAN**2*XHT/(H**0.3333)
!
          ELSE
!
            TEMP=GRAV*AMR*CZ**2*XHT/(H**0.3333)
          ENDIF
        ENDIF
      ELSE
        IF(CZ > 1.0) TEMP=GRAV*AMR /CZ**2*XHT
        IF(CZ <= 1.0 .AND. CZ > 0.) THEN
!IPK MAR01  ADD VARIABLE WALL FRICTION BASED ON ELEVATION
          IF(MANMIN(NM) > 0.) THEN
            IF(H+A0 < ELMMIN(NM) ) THEN 
              TEMP = GRAV*AMR* MANMIN(NM)**2*XHT/(2.21*H**0.3333)
            ELSEIF(H+A0 > ELMMAX(NM) ) THEN 
              TEMP = GRAV*AMR* MANMAX(NM)**2*XHT/(2.21*H**0.3333)
            ELSE
              FSCL=(H+A0-ELMMIN(NM))/(ELMMAX(NM)-ELMMIN(NM))
              TEMP = GRAV*AMR*(MANMIN(NM)+                              &
     &             FSCL*(MANMAX(NM)-MANMIN(NM)))**2*XHT/(2.21*H**0.3333)
            ENDIF
!IPK SEP04  ADD MAH AND MAT OPTION
          ELSEIF(HMAN(NM,2) > 0 .OR. HMAN(NM,3) > 0.) THEN
            TEMAN=0.
            IF(HMAN(NM,2) > 0) THEN 
                TEMAN=HMAN(NM,3)*EXP(-H/HMAN(NM,2))
            ENDIF
            TEMAN=TEMAN+HMAN(NM,1)/H**HMAN(NM,4)
            TEMP=GRAV*AMR*TEMAN**2*XHT/(2.21*H**0.3333)
          ELSEIF(MANTAB(NM,1,2) > 0.) THEN
            DO K=1,4
              IF(H < MANTAB(NM,K,1)) THEN
                IF(K == 1) THEN
                  TEMAN=MANTAB(NM,1,2)
                ELSE
                  FACT=(H-MANTAB(NM,K-1,1))/                            &
     &                (MANTAB(NM,K,1)-MANTAB(NM,K-1,1))
                   TEMAN=MANTAB(NM,K-1,2)                               &
     &            +FACT*(MANTAB(NM,K,2)-MANTAB(NM,K-1,2))
                ENDIF
                GO TO 624
              ENDIF
            ENDDO
            TEMAN=MANTAB(NM,4,2)
  624       CONTINUE
            TEMP=GRAV*AMR*TEMAN**2*XHT/(2.21*H**0.3333)
          ELSE
!
            TEMP=GRAV*AMR*CZ**2*XHT/(2.21*H**0.3333)
          ENDIF
        ENDIF
      ENDIF
!     IF(CZ <= 1.0) TEMP=GRAV*AMR*CZ**2*XHT/(2.21*H**0.3333)*1.44
      IF(MAXN > 1 .OR. KRESTF == 1) THEN
        UF=U
      ELSE
        UF=UINP
      ENDIF
      TEMPXX=TEMP *2.*ABS(UF)
      TEMPX=TEMP *U*ABS(U)
      IA=1-NDF
      DO 640 I=1,NCN
      IA=IA+NDF
      F(IA)=F(IA)-XN(I)*TEMPX*QFACT(I)
      TEMPA=TEMPXX*XN(I)*QFACT(I)
      IB=1-NDF
      DO 630 J=1,NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPA*XN(J)*QFACT(J)
  630 CONTINUE
  640 CONTINUE
!IPK JAN99 ADD NEW LOCATION
  641 CONTINUE
!IPK NOV98
      IF(NTYPE == 1) GO TO 700
      GO TO 750
!-
!......Surface elements apply wind stress
!-
  700 SIGMAX=0.
      DO 710 I=1,NCN
      MR=NOP(NN,I)
!
!ipk jun03 add STRESS component
!
        SIGMAX=SIGMAX+XN(I)*((SIGMA(MR,1)+stress(mr,1))*CX              &
     &                        +(SIGMA(MR,2)+stress(mr,2))*SAN)
  710 CONTINUE
      sigmax=sigmax*h
!
!IPK AUG02 TEST FOR SHALLOW OR NEGATIVE DEPTH TO SET STRESS TO ZERO.
      IF(WSELL-A0 < ZSTDEP) THEN
        SIGMAX=0.
      ENDIF
!
      IA=1-NDF
      DO 715 I=1,NCN
      IA=IA+NDF
      F(IA)=F(IA)+AMW *XN(I)*SIGMAX*QFACT(I)
  715 CONTINUE
!-
!......Surface elements apply boundary conditions for continuity equation
!-
      IA=3-2*NDF
      DO 725 M=1,2
      IA=IA+NDF*2
      TEMPX=AMW*DHDX*XM(M)
      IB=1-NDF
      DO 718 I=1,NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*XN(I)*QFACT(I)
  718 CONTINUE
      IB=3-2*NDF
      IF(ICYC /= 0) THEN
        DO 720 I=1,2
          IB=IB+NDF*2
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+AMW*ALTM*XM(M)*XM(I)
  720   CONTINUE
      ENDIF
      IB=3-2*NDF
      DO 723 I=1,2
        IB=IB+NDF*2
        ESTIFM(IA,IB)=ESTIFM(IA,IB)+AMW*XM(M)*U*DMX(I)
  723 CONTINUE
!      F(IA)=F(IA)-XM(M)*AMW*DHDX*U
!
!IPK MAR01  ADD SIDF FOR GLOBAL LOADING AS SURFACE FLOW USE SIDFQ MAY04
      F(IA) = F(IA) - XM(M)*AMW*(DHDX*U + DHDT-SIDFQ)
!
  725 CONTINUE
!IPK AUG95 ADD LOGIC FOR SURFACE RATE TERMS FOR HEAT BUDGET
!IPK MAR01 ADD LOGIC FOR SURFACE SOURCE.LOSS
      IF(ICK == 5) THEN
!
!IPK OCT02 GET GAUSS POINT ICE VALUES
        GSICE=GSICE+THKI(1)*XM(1)+THKI(3)*XM(2)
        GSQLW=GSQLW+QWLI(1)*XM(1)+QWLI(3)*XM(2)
!
        CALL MKTEMP(SALT,H,0.,SRCSNK,GRATE,DELT,NM,NETYP(NN))
      ELSE
        SRCSNK=0.
        GRATE=0.
      ENDIF
      IA=4-NDF
      DO M=1,NCN
        IA=IA+NDF
        IB=4-NDF
        DO I=1,NCN
          IB=IB+NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)-AMW*(H*GRATE/XHT-SIDFQQ)          &
     &                         *XN(M)*XN(I)
!IPK MAY04 USE SIDFQQ
        ENDDO
        F(IA)=F(IA)+AMW*(H*(SRCSNK+GRATE*SALT)/XHT+                     &
     &        SIDFQQ*(SIDQ(NN,ICK-3)-SALT)  )*XN(M)
!IPK MAY04 USE SIDFQQ
!ipk aug98 divide by xht in lines above
      ENDDO
!IPK AUG95 END ADDITIONS
      GO TO 750
!
!     End elements apply boundary pressures
!
  730 CONTINUE
      Z=(1.-AFACT(N))*CORD(N1,3)+AFACT(N)*CORD(N3,3)
!      ZA=(ELEV-Z)/XHT
      SPC=(1.-AFACT(N))*SPEC(N1,3)+AFACT(N)*SPEC(N3,3)
!      P=(1.-AFACT(N))*PRESS(N1)+AFACT(N)*PRESS(N3)
      H=VEL(3,N1)
!      PD=(1.-AFACT(N))*PDN(1)+AFACT(N)*PDN(3)
!      PD=P-RHO*GRAV*ZA*H
!
!      RHO=1000.-ZC
!      RHOS=1000.
      TEMP=-AMW*(2.*SPC-H)*RHOS*GRAV*H/2.
      IA=1
      DO 740 I=1,NCN
        F(IA)=F(IA)-TEMP*XN(I)*QFACT(I)
        IA=IA+NDF
  740 CONTINUE
  750 CONTINUE
!
!
!     Bottom elements
!
      IF(NTYPE == 2) THEN
        XL(1)=CORD(N2,3)-CORD(N1,3)
        XL(2)=CORD(N3,3)-CORD(N1,3)
        DO 790 N=1,4
          XM(1)=1.-AFACT(N)
          XM(2)=AFACT(N)
          XN(1)=XM(1)*(1.-2.*XM(2))
          XN(2)=XM(1)*4.*XM(2)
          XN(3)=(2.*XM(2)-1.)*XM(2)
          WID=XM(1)*WIDTH(N1)+XM(2)*WIDTH(N3)
          A0=XM(1)*AO(N1)+XM(2)*AO(N3)
          Z=XM(1)*CORD(N1,3)+XM(2)*CORD(N3,3)
          RHO=XM(1)*DEN(N1)+XM(2)*DEN(N3)
!IPK JAN99          NS1=NSURF(N1)
!IPK JAN99          NS3=NSURF(N3)
!IPK JAN99          RHOS=XM(1)*DEN(NS1)+XM(2)*DEN(NS3)
!
          IF (N1 > NPM) THEN
             NN1 = NSURF(N1)
             NN3 = NSURF(N3)
          ELSE
             NN1 = N1
             NN3 = N3
          ENDIF
          RHOS=XM(1)*DEN(NN1)+XM(2)*DEN(NN3)
!
          H=XM(1)*VEL(3,N1)+XM(2)*VEL(3,N3)
!          P=XM(1)*PRESS(N1)+XM(2)*PRESS(N3)
!          PD=(1.-AFACT(N))*PDN(1)+AFACT(N)*PDN(3)
!
!          ZC=XN(1)*CORD(N1,3)+XN(2)*CORD(N2,3)+XN(3)*CORD(N3,3)
!          P=GRAV*ZC*(ZC/2.-1000.)
!          PD=GRAV*ZC**2/2.
!          RHO=1000.-ZC
!          RHOS=1000.
!          PD=P-RHO*GRAV*ZA*H
!
          AMW=(DNAL(2,N)*XL(1)+DNAL(3,N)*XL(2))/2.*HFACT(N)*WID
          AMR=AMW*RHO
          U=0.
          DO 760 I=1,NCN
            MR=NOP(NN,I)
            CXA=COS(ALFA(MR))
            SXA=SIN(ALFA(MR))
            U=U+XN(I)*(CXA*VEL(1,MR)+SXA*VEL(2,MR))*QFACT(I)
  760     CONTINUE
          XHT=ELEV-A0
!          ZA=(ELEV-Z)/XHT
!-
!......BOTTOM FRICTION
!-
          TEMP=0.
            IF(CZ == 0.) GO TO 790
!
          IF (GRAV < 32.)  THEN
            IF(CZ > 1.0 ) TEMP=GRAV*AMR /CZ**2*XHT
            IF(CZ <= 1.0 ) TEMP=GRAV*AMR *CZ**2*XHT/(H**0.3333)
          ELSE
            IF(CZ > 1.0 ) TEMP=GRAV*AMR /CZ**2*XHT
            IF(CZ <= 1.0 ) TEMP=GRAV*AMR *CZ**2*XHT/(2.21*H**0.3333)
          ENDIF
          TEMPXX=TEMP *2.*ABS(U)
          TEMPX=TEMP *U*ABS(U)
          IA=1-NDF
          DO 770 I=1,NCN
            IA=IA+NDF
            F(IA)=F(IA)-XN(I)*TEMPX*QFACT(I)
            TEMPA=TEMPXX*XN(I)*QFACT(I)
            IB=1-NDF
            DO 765 J=1,NCN
              IB=IB+NDF
              ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPA*XN(J)*QFACT(J)
  765       CONTINUE
  770     CONTINUE
!
!     Pressure integral
!
          TEMP=AMW*H*RHOS*GRAV*H/2.
          IA=1
          DO 775 I=1,NCN
            F(IA)=F(IA)-TEMP*XN(I)*QFACT(I)
            TEMPX=AMW*XN(I)*RHOS*GRAV*H*QFACT(I)
            IB=3-NDF*2
            DO 772 M=1,2
              IB=IB+NDF*2
              ESTIFM(IA,IB)=ESTIFM(IA,IB)+TEMPX*XM(M)
  772       CONTINUE
            IA=IA+NDF
  775     CONTINUE
  790   CONTINUE
      ENDIF
!C      IF(NTYPE == 4) GO TO 1001
!C      IF(MOD(NFIX(N1)/100,10) /= 2 .AND. NFIX(N1)/10000 == 0) THEN
!C        N1S=NSURF(N1)
!C      WRITE(*,*) 'N1,N1S,IBN(N1S)',N1,N1S,IBN(N1S)
!C        IF(IBN(N1S) == 1) THEN
!C          F(1)=0.
!C          DO 792 I=1,NEF
!C            ESTIFM(1,I)=0.
!C  792     CONTINUE
!C        ENDIF
!C      ENDIF
!C      IF(MOD(NFIX(N3)/100,10) /= 2 .AND. NFIX(N3)/10000 == 0) THEN
!C        N3S=NSURF(N3)
!C        WRITE(*,*) 'N3,N3S,IBN(N3S)',N3,N3S,IBN(N3S)
!C        IF(IBN(N3S) == 1) THEN
!C          F(9)=0.
!C          DO 794 I=1,NEF
!C           ESTIFM(9,I)=0.
!C  794     CONTINUE
!C        ENDIF
!C      ENDIF
      GO TO 1001
  800 CONTINUE
!-
!- APPLY TRANSFORMATIONS TO STIFFNESS AND FORCE MATRICES FOR SLOPING B. C.
!-
      DO 1000 N=1,NCN
!ipk oct98 update to f90
      N1=NOP(NN,N)
!IPK OCT01  ADD AGD TRANSFORMATION
      AFA=ALFA(N1)-ADIF(N1)-AGD
      IF(AFA) 820,1000,820
  820 CX=COS(AFA)
      SA=SIN(AFA)
      IB=NDF*(N-1)+1
      DO 840 M=1,NCN
      DO 840 MM=1,NDF
      IA=NDF*(M-1)+MM
      TEMP=ESTIFM(IA,IB)*CX + ESTIFM(IA,IB+1)*SA
      ESTIFM(IA,IB+1)=-ESTIFM(IA,IB)*SA + ESTIFM(IA,IB+1)*CX
      ESTIFM(IA,IB)=TEMP
  840 CONTINUE
      DO 860 M=1,NCN
      DO 860 MM=1,NDF
      IA=NDF*(M-1)+MM
      TEMP=ESTIFM(IB,IA)*CX + ESTIFM(IB+1,IA)*SA
      ESTIFM(IB+1,IA)=-ESTIFM(IB,IA)*SA + ESTIFM(IB+1,IA)*CX
      ESTIFM(IB,IA)=TEMP
  860 CONTINUE
      TEMP=CX*F(IB) + SA*F(IB+1)
      F(IB+1)=-F(IB)*SA + F(IB+1)*CX
      F(IB)=TEMP
 1000 CONTINUE
 1001 CONTINUE
!-
!......APPLY VERTCAL SHAPE FUNCTION FACTORS
!-
      JA=1-NDF
      DO 1010 JJ=1,NCN
      J=NOP(NN,JJ)
      JA=JA+NDF
      FT=FCTV(J)
      IF(FT == 1.) GO TO 1010
      IA=1-NDF
      DO 1005 II=1,NCN
      I=NOP(NN,II)
      IA=IA+NDF
      ESTIFM(IA  ,JA  )=ESTIFM(IA  ,JA  )*FT
      ESTIFM(IA  ,JA+1)=ESTIFM(IA  ,JA+1)*FT
      ESTIFM(IA+1,JA  )=ESTIFM(IA+1,JA  )*FT
      ESTIFM(IA+1,JA+1)=ESTIFM(IA+1,JA+1)*FT
      ESTIFM(IA+2,JA  )=ESTIFM(IA+2,JA  )*FT
      ESTIFM(IA+2,JA+1)=ESTIFM(IA+2,JA+1)*FT
 1005 CONTINUE
 1010 CONTINUE
      DO 1030 K=1,NCN
!ipk oct98 update to f90
      N=NOP(NN,K)
      IF(NFIX(N) < 13000) GO TO 1030
      IRW=(K-1)*NDF+1
      IF(NFIX(N) == 13000) IRW=IRW+1
      DO 1025 J=1,NEF
 1025 ESTIFM(IRW,J)=0.
      F(IRW)=0.
 1030 CONTINUE
!-
!...... For 2D - 3D junctions adjust equation for direction
!-
      DO 1045 N=1,NCN,2
!ipk oct98 update to f90
        M=NOP(NN,N)
        IF(ADIF(M) /= 0.) THEN
!         WRITE(*,*) NN,N,M,ADIF(M)
          NEQ=NDF*NCN
          IA=NDF*(N-1)+1
          DO 1040 I=1,NEQ
            ESTIFM(I,IA)=ESTIFM(I,IA)+ESTIFM(I,IA+1)*SIN(ADIF(M))       &
     &                   /COS(ADIF(M))
!           ESTIFM(IA,I)=ESTIFM(IA,I)/COS(ADIF(M))
 1040     CONTINUE
!         F(IA)=F(IA)/COS(ADIF(M))
        ENDIF
 1045 CONTINUE
      DO 1050 I=1,NCN
!ipk oct98 update to f90
      J=NOP(NN,I)
      IA=NDF*(I-1)
      DO 1050 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA == 0) GO TO 1050
           if (ja < 0)  then
              write(*,*) ' surcof ', nn,ja
              ja = iabs(ja)
              goto 1050
            endif
      rkeepeq(ja)=rkeepeq(ja)+f(ia)
      ekeep(ja)=ekeep(ja)+estifm(ia,ia)
!
      R1(JA)=R1(JA)+F(IA)
 1050 CONTINUE
!      if(imat(nn) > 4000) then
!        WRITE(75,7700) NN,(F(I),I=1,NEF)
!        WRITE(75,7701) ((ESTIFM(I,J),J=1,NEF,4),I=1,NEF,4)
! 7700 FORMAT(I5,'F'/(1PE12.4))
! 7701 FORMAT(' ESTIFM'/(1p4e12.4))
!      endif
      RETURN
      END
