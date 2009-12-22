CIPK  LAST UPDATE AUG 22 2007 UPDATE TO BLKECOM
CIPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
cipk  last update jan 2 1999 add variable width with depth
C     Last change:  IPK   5 Oct 98    3:35 pm
cipk  last update Aug 6 1998 complete division by xht for transport eqn
CIPK  LAST UPDATED NOV 20 1997
cipk  last updated Oct 1 1996
CIPK  LAST UPDATED SEP 8 1995
      SUBROUTINE COEFV(NN,NTX)
      USE BLKHMOD
      USE BLKSMOD
      USE WATPMOD
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKECOM
      SAVE

      real*8 vx
      
CIPK JAN99 EXPAND TO F TO 80
CIPK AUG07
      DIMENSION XN(8),DNX(8),DNZ(8),XM(4),DMX(4),DMZ(4),XL(8),ZL(8),
     1          WAITX(16),NLC(8),QFACT(8),VTRB(8),VTMP(8)
CAUG93IPK      DIMENSION PDN(8),DPDNR(8)
CIPK OCT98      DIMENSION IND(80)
cipk jan99
C
      DIMENSION  WIDTHZ(8)
      REAL J11,J12,J21,J22
cipk sep95 add real*8
      REAL*8 S
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: h
!-

CIPK OCT98      DATA IND/1,0,2,0,1,0,2,0,1,0,2,0,3,0,4,0,5,0,6,0,5,0,6,0,5,0,6,0
CIPK OCT98     +        ,3,0,4,0,48*0/
C-
C-.....ASSIGN PROPER COEFS.....
C-
      NCN=NCORN(NN)
      IF(NTX .EQ. 0) THEN
        AREA(NN)=0.
      ELSE
        TEL=AREA(NN)
cipknov97 add tvol
        TVOL(NN)=0.
      ENDIF
cipk oct98 update to f90
      IMMT=IMAT(NN)
CIPK JAN99
      IF(NETYP(NN) .EQ. 17) GO TO 2000

      NR = MOD(IMMT,100)

      IF(MOD(IMMT,1000) .GT. 900) GO TO 70
ccc      EPSX=ORT(NR,1)
C
CIPK      EPSX = EEXXYY(1,NN)/ROAVG
      EPSX = EEXXYY(1,NN)
C
      NGP=7
      NCNX=NCN/2
      NEF=NCN*NDF
cipk nov97 revise test
      IF(NTX .EQ. 0  .OR. NTX .EQ. 3) GO TO 70
C-
C-....CHANGE VERT DIFF IF STRATIFIED.....
C-
cipk jan99

!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to obsolete unit nipt
!nipt is obsolete
!-

      DFACT=1.
      IF( NDF.LT.4 ) GO TO 65
      DCRIT =  1.1335E-6
      DRDYC=0.
      NDTC=0
      DO 60 K=1,NCN,2
CIPK OCT98 CONVERT TO F90
        N1=ABS(NOP(NN,K))
        N2=MOD(K+2,NCN)
CIPK OCT98 CONVERT TO F90
        N2=ABS(NOP(NN,N2))
        DY=(CORD(N2,3)-CORD(N1,3))*VEL(3,N1)/(ELEV-AO(N1))
        IF(ABS(DY) .GT. 0.1) THEN
          IF(DY.GT.0.0) THEN
            DSDY=(VEL(4,N2)-VEL(4,N1))/DY
            DTDY=(VEL(5,N2)-VEL(5,N1))/DY
            DSDDY=(VEL(6,N2)-VEL(6,N1))/DY
          ENDIF
          IF(DY.LT.0.0) THEN
            DSDY=(VEL(4,N1)-VEL(4,N2))/ABS(DY)
            DTDY=(VEL(5,N1)-VEL(5,N2))/ABS(DY)
            DSDDY=(VEL(6,N1)-VEL(6,N2))/ABS(DY)
          ENDIF
          NDTC=NDTC+1
          RHO1=FDEN(VEL(4,N1),IGF)
          DRODT1=DRODS(VEL(4,N1),IGF)
          RHO2=FDN(VEL(5,N1),IGF)
          DRODT2=DRODTM(VEL(5,N1),IGF)
          RHO3=FDSED(VEL(6,N1),IGF)
          DRODT3=DRODSD(VEL(6,N1),IGF)
          IF (GRAV .LT. 32.)  THEN
            IF(IPASS1 .EQ. 1) THEN
              RHO1=1.94*516.
              DRODT1=0.
            ENDIF
            IF(IPASS2 .EQ. 1) THEN
              RHO2=1.94*516.
              DRODT2=0.
            ENDIF
            IF(IPASS3 .EQ. 1) THEN
              RHO3=1.94*516.
              DRODT3=0.
            ENDIF
            RHO=RHO1+RHO2+RHO3 - 3.88*516.
          ELSE
            IF(IPASS1 .EQ. 1) THEN
              RHO1=1.94
              DRODT1=0.
            ENDIF
            IF(IPASS2 .EQ. 1) THEN
              RHO2=1.94
              DRODT2=0.
            ENDIF
            IF(IPASS3 .EQ. 1) THEN
              RHO3=1.94
              DRODT3=0.
            ENDIF
            RHO=RHO1+RHO2+RHO3-3.88
          ENDIF
          DRDYC=DRDYC-(DSDY*DRODT1+DTDY*DRODT2+DSDDY*DRODT3)/RHO
        ENDIF
   60 CONTINUE
      DFACT=1.
      IF(NDTC .GT. 0) THEN
        DRDYC=DRDYC/FLOAT(NDTC)
        IF(GRAV .LT. 32.) THEN
cipk mar94 correct to divide by 3.2808
c         DRDYC=DRDYC*3.2808
          DRDYC=DRDYC/3.2808
cipk mar94 end change
        ENDIF

        IF(DRDYC .GT. DCRIT) DFACT=0.6888E-4*DRDYC**(-0.7)
        IF(DFACT .LT. .10) DFACT=.10
cc        IF(DRDYC .LT. -0.1E-04) DFACT=10.0
      ENDIF
        DFCT(NN)=DFACT
cc        write(75,*) 'drdyc',maxn,nn,drdyc,dfact
   65 CONTINUE
C-
CIPK      DIFX=ORT(NR,8)
      DIFX = EEXXYY(5,NN)
c      WRITE(*,9876) NN,DFACT,DIFX
c 9876 FORMAT(I10,F12.4,1PE12.4)
C-
C...... Set up switches for active degrees of freedom
C-
      IF(ITEQV(MAXN) .EQ. 2  .OR.  ITEQV(MAXN) .GT. 7) THEN
        IVLP=0
      ELSE
        IVLP=1
      ENDIF
      IF(ITEQV(MAXN) .LT. 2  .OR.  (ITEQV(MAXN) .GT. 4
     +                      .AND.   ITEQV(MAXN) .LT. 8)) THEN
        IHLP=1
      ELSE
        IHLP=0
      ENDIF
      IF(ITEQV(MAXN) .EQ. 1  .OR.  ITEQV(MAXN) .EQ. 4) THEN
        ISLP=0
      ELSE
        ISLP=1
      ENDIF
C-
C- INITIALIZE MATRICES AND VARIABLES
C-
   70 DO 72 I=1,NCN
CIPK OCT98 CONVERT TO F90
        NLC(I)=ABS(NOP(NN,I))
C        G(I)=0.
cipk jan99
        m=nlc(i)
        widthz(i) = width(m) + (ss1(m)+ss2(m))*(cord(m,3)-ao(m))
     &                  * (vel(3,m)/(elev-ao(m)))
        IF(NOP(NN,I+11) .NE. 0) THEN
          ITRB=IABS(NOP(NN,I+11))
          MR=NOP(NN,I)
          CXX=COS(ALFA(ITRB))
          SAA=SIN(ALFA(ITRB))
          VTRB(I)=-(VEL(1,ITRB)*CXX+VEL(2,ITRB)*SAA)*DIR(ITRB)
C
        ELSE
          VTRB(I)=0.
        ENDIF



CAUG93IPK  M=NOP(NN,I)
CAUG93IPK  PDN(I)=-(PRESS(M)-DEN(M)*GRAV*VEL(3,M)*(ELEV-CORD(M,3))/
CAUG93IPK     +         (ELEV-AO(M)))
CAUG93IPK  DPDNR(I)=-GRAV*VEL(3,M)*(ELEV-CORD(M,3))/(ELEV-AO(M))
C        DO 72 J=1,NCN
C          ESTIFL(I,J)=0.
   72 CONTINUE

cipk jan99

      do 73 j=2,ncn,2
         n1 = j-1
         n3 = j+1 
         if (j .eq. ncn) n3 = 1
         if (abs(vtrb(j)) .gt. 1.e-7)  then
            vtmp(n1) = (vtrb(n1) + 2.*vtrb(j))/3.
            vtmp(n3) = (vtrb(n3) + 2.*vtrb(j))/3.
         endif
   73 continue
      do 74 j=1,ncn,2
         vtrb(j) = vtmp(j)
   74 continue

cipk jan99 end

      IF(NTX .EQ. 0) THEN
        IF(NCN .EQ. 8) THEN
          N1=NLC(3)
          N2=NLC(4)
          N3=NLC(5)
        ELSEIF(NLC(1)+2 .EQ. NLC(3)) THEN
          N1=NLC(3)
          N2=NLC(4)
          N3=NLC(5)
        ELSE
          N1=NLC(1)
          N2=NLC(2)
          N3=NLC(3)
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
        DO 75 I = 1, 4
          TEMP=DNAL(2,I)*XLM+DNAL(3,I)*XLF
C-
C......Define shape functions for length computation
C-
          DNX(2)=(4.-8.*AFACT(I))/TEMP
          DNX(3)=(4.*AFACT(I)-1.)/TEMP
          DYDX=YLM*DNX(2)+YLF*DNX(3)
          ALF=ATAN(DYDX)
          CSALF=COS(ALF)
          TEMP=TEMP/CSALF
          AREA(NN)=AREA(NN)+ABS(TEMP)*HFACT(I)/2.

   75   CONTINUE

CIPK JAN99
C
C.......Store tributary length
C
        DO 76 I=1,NCN
          J=NLC(I)
          XTLN(J)=AREA(NN)
   76   CONTINUE

CIPK JAN99 END ADDITION
        RETURN
      ENDIF
CIPK JAN99 CHANGE LIMITS TO 80
      DO 77 I=1,80
      F(I) = 0.0
      DO 77 J=1,80
   77 ESTIFM(I,J) = 0.0
      IF(NETYP(NN) .EQ. 17) GO TO 2000
      MR=NLC(1)
      CX=COS(TH(NN))
      SAN=SIN(TH(NN))
      DO 78 K = 1, NCN
        N=NLC(K)
        ANGDIF=TH(NN)-ALFA(N)
        IF(ABS(ANGDIF) .GT. 1.5708  .AND.  ABS(ANGDIF) .LT. 4.7124) THEN
          QFACT(K)=-1.0
        ELSE
          QFACT(K)=1.0
        ENDIF
        DX=CORD(N,1)-CORD(MR,1)
        DY=CORD(N,2)-CORD(MR,2)
        XL(K)=DX*CX+DY*SAN
        ZL(K)=CORD(N,3)-CORD(MR,3)
   78 CONTINUE
      IF(NCN .EQ. 8) THEN
        XL(4)=XL(5)/2.
        XL(8)=XL(7)/2.
      ELSEIF(NLC(1)+2 .EQ. NLC(3)) THEN
        XL(4)=XL(5)/2.
        XL(6)=XL(5)/2.
      ELSE
        XL(2)=XL(3)/2.
        XL(6)=XL(3)/2.
      ENDIF
      TFR=TEL/ABS(XL(5))
      IF(NTX .EQ. 0) GO TO 220

C
C...... OBTAIN BASIS FUNCTIONS AT NODES
C
      CALL SA2(NCN)

cipk jan99
C
C  Find drodxn for top surface nodes
C
      IF (NN .LE. NEM)  THEN
C
        DO  II=1,2
          IF (II .EQ. 1) I = 7
          IF (II .EQ. 2) I = 1
          J11 = 0.0
          J12 = 0.0
          J21 = 0.0
          J22 = 0.0
          DO  K = 2, NCN
            J11 = J11 + DA(K,I) * XL(K)
            J12 = J12 + DA(K,I) * ZL(K)
            J21 = J21 + DB(K,I) * XL(K)
            J22 = J22 + DB(K,I) * ZL(K)
           ENDDO
          DETJ = J11 * J22 - J12 * J21
          DO J = 1, NCN
            XN(J) = XNX(J,I)
            DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / (DETJ*TFR)
          ENDDO
C-
C-     REPEAT FOR LINEAR FUNCTION
C-
          JJ=0
          DO J=1,NCN,2
             JJ=JJ+1
             XM(JJ)=XMX(JJ,I)
             DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/(DETJ*TFR)
          ENDDO
C
          DRODXN(NLC(I))=0.0
          DO M=1,NCNX
            MR=NLC(2*M-1)
            DRODXN(NLC(I))=DRODXN(NLC(I))+DMX(M)*DEN(MR)
          ENDDO
          IF(NCN .EQ. 6)  STOP
        ENDDO
C
      ENDIF

CIPK JAN99 END CHANGES

C
C...... WORK ONLY ON NODES AT BOTTOM CONNECTIVITY
C
      DO  100 I=3,5,2
        J11 = 0.0
        J12 = 0.0
        J21 = 0.0
        J22 = 0.0
        DO 79 K = 2, NCN
          J11 = J11 + DA(K,I) * XL(K)
          J12 = J12 + DA(K,I) * ZL(K)
          J21 = J21 + DB(K,I) * XL(K)
          J22 = J22 + DB(K,I) * ZL(K)
   79   CONTINUE
        DETJ = J11 * J22 - J12 * J21
        DO 80 J = 1, NCN
          XN(J) = XNX(J,I)
          DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / (DETJ*TFR)
   80   CONTINUE
C-
C-     REPEAT FOR LINEAR FUNCTION
C-
        JJ=0
        DO 81 J=1,NCN,2
        JJ=JJ+1
        XM(JJ)=XMX(JJ,I)
        DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/(DETJ*TFR)
   81   CONTINUE
        DRODXN(NLC(I))=0.0
        DO 95 M=1,NCNX
          MR=NLC(2*M-1)
          DRODXN(NLC(I))=DRODXN(NLC(I))+DMX(M)*DEN(MR)
   95   CONTINUE
        IF(NCN .EQ. 6) GO TO 101
  100 CONTINUE
C
C ..... INTEGRATE VERTICALLY TO FORM NODAL INT OF DRODX
C
  101 CONTINUE
      IF(NLC(1) .LE. NPM) DROXIN(NLC(1))=0.
      IF(NLC(NCN-1) .LE. NPM) DROXIN(NLC(NCN-1))=0.
C
C     FOR 8 NODED ELEMENT
C
      IF(NCN .EQ. 8) THEN
        DROXIN(NLC(3))=DROXIN(NLC(1))+(DRODXN(NLC(1))+DRODXN(NLC(3)))/
     +  2.*VEL(3,NLC(3))/(ELEV-AO(NLC(3)))*(-ZL(3))
        DROXIN(NLC(5))=DROXIN(NLC(7))+(DRODXN(NLC(5))+DRODXN(NLC(7)))/
     +  2.*VEL(3,NLC(5))/(ELEV-AO(NLC(5)))*(ZL(7)-ZL(5))
      ELSEIF(NLC(1)+2 .EQ. NLC(3)) THEN
C
C     FOR 6 NODES VERTICAL LINE BELOW 1
C      
        DROXIN(NLC(3))=DROXIN(NLC(1))+(DRODXN(NLC(1))+DRODXN(NLC(3)))/
     +  2.*VEL(3,NLC(3))/(ELEV-AO(NLC(3)))*(-ZL(3))
      ELSE
C
C     FOR 6 NODES VERTICAL LINE BELOW 5
C
cAUG93ipk       DROXIN(NLC(3))=DROXIN(NLC(3))+(DRODXN(NLC(3))+DRODXN(NLC(5)))/
        DROXIN(NLC(3))=DROXIN(NLC(5))+(DRODXN(NLC(3))+DRODXN(NLC(5)))/
     +  2.*VEL(3,NLC(3))/(ELEV-AO(NLC(3)))*(ZL(5)-ZL(3))
C
      ENDIF
C-
C-.....COPY WEIGHTING FUNCTIONS AT GAUSS POINTS.....
C-
cipk oct98 update to f90
      IF(MOD(IMMT,5000) .LT. 100 ) THEN
        IF( NCN .LT. 8 ) THEN
          NGP = 7
          DO 186 M = 1, NGP
            WAITX(M) = WAITT(M)
  186     CONTINUE
        ELSEIF(ITLVL(MAXN) .EQ. 1) THEN
          NGP=4
          DO 188 M=1,4
            WAITX(M)=1.0
  188     CONTINUE
        ELSE
          NGP = 9
          DO 190 M = 1, NGP
            WAITX(M) = WAITR(M)
  190     CONTINUE
        ENDIF
      ELSE
        NGP = 16
        IF( NCN .LT. 8 ) THEN
          DO 192 M = 1, NGP
            WAITX(M)=WAITTH(M)
  192     CONTINUE
        ELSE
          DO 194 M = 1, NGP
            WAITX(M) = WAITRH(M)
  194     CONTINUE
        ENDIF
      ENDIF
C-
C-.....COPY SHAPE FUNCTIONS
C-
      CALL SB2(NCN,NGP)
C-
C-.....COMPUTE LOCAL CORDS.....
C-
C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
  220 DO 500 I = 1, NGP
C-
C-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
C-
      J11 = 0.0
      J12 = 0.0
      J21 = 0.0
      J22 = 0.0
      DO 230 K = 2, NCN
        J11 = J11 + DA(K,I) * XL(K)
        J12 = J12 + DA(K,I) * ZL(K)
        J21 = J21 + DB(K,I) * XL(K)
        J22 = J22 + DB(K,I) * ZL(K)
  230 CONTINUE
      DETJ = J11 * J22 - J12 * J21
      DO 235 J = 1, NCN
      XN(J) = XNX(J,I)
      DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / (DETJ*TFR)
      DNZ(J) = ( J11 * DB(J,I) - J21 * DA(J,I) ) / DETJ
  235 CONTINUE
      AMW = WAITX(I) * ABS(DETJ) * TFR
      IF(NTX .EQ. 0) GO TO 500
C-
C-     REPEAT FOR LINEAR FUNCTION
C-
      JJ=0
      DO 245 J=1,NCN,2
      JJ=JJ+1
      XM(JJ)=XMX(JJ,I)
      DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/(DETJ*TFR)
      DMZ(JJ)=(J11*CB(JJ,I)-J21*CA(JJ,I))/DETJ
  245 CONTINUE
C
C..... IOPTZD IS CASE OF QUADRATIC MELLOR-YAMADA
C
      IF(IOPTZD .EQ. 1) THEN
        EPSXZ = 0.
        DIFZ  = 0.
        DO 269 M=1,NCN
          MR=NLC(M)
          EPSXZ=EPSXZ+XN(M)*EVISXZ(MR)
          DIFZ=DIFZ+XN(M)*DVISZ(MR)
  269   CONTINUE
        epsxz=abs(epsxz)
      ENDIF
cipk nov97 skip forward for ntx=3
      IF(NTX .EQ. 3) GO TO 273
C-
C-
C.....COMPUTE R, S, H AND THEIR DERIVATIVES.....
      DVDZ = 0.0
      U = 0.0
      W=0.0
      Z = 0.0
      DUDX = 0.0
      DUDZ = 0.0
      BETA1 = 0.0
CIPK OCT98      BETA2 = 0.0
      S = 0.0
      DSDT=0.0
      DSDX=0.0
      DSDZ=0.0
CAUG93IPK      P=0.0
CAUG93IPK      DPDX=0.
CAUG93IPK      PD=0.
CAUG93IPK      DPDR=0.
      DO 270 M=1,NCN
      MR=NLC(M)
      CXA=COS(ALFA(MR))
      SXA=SIN(ALFA(MR))
      XNA=XN(M)
      U=U+XNA*(VEL(1,MR)*CXA+VEL(2,MR)*SXA)*QFACT(M)
      W=W+XNA*VVEL(MR)
      Z=Z+XNA*CORD(MR,3)
      DUDX=DUDX+DNX(M)*(VEL(1,MR)*CXA+VEL(2,MR)*SXA)*QFACT(M)
      DUDZ=DUDZ+DNZ(M)*(VEL(1,MR)*CXA+VEL(2,MR)*SXA)*QFACT(M)
      S=S+XNA*VEL(ICK,MR)
      DSDX=DSDX+DNX(M)*VEL(ICK,MR)
      DSDZ=DSDZ+DNZ(M)*VEL(ICK,MR)
CAUG93IPK P=P+XNA*PRESS(MR)
CAUG93IPK PD=PD+XNA*PDN(M)
CAUG93IPK DPDR=DPDR+XNA*DPDNR(M)
CAUG93IPK DPDX=DPDX+DNX(M)*PRESS(MR)
      IF(ICYC.LT.1) GO TO 270
      DSDT=DSDT+XNA*VDOT(ICK,MR)
      BETA1=BETA1+XNA*(VDOT(1,MR)*CXA+VDOT(2,MR)*SXA)*QFACT(M)
  270 CONTINUE
C
C..... EDDY VISCOSITY BASED ON LINEAR MELLOR-YAMADA
C
      IF(IOPTZD .EQ. 2) THEN
        EPSXZ = 0.
        DIFZ  = 0.
        DO 272 M=1,NCNX
          IF(XM(M) .EQ. 0.) GO TO 272
          MR=NLC(2*M-1)
          EPSXZ=EPSXZ+XM(M)*EVISXZ(MR)
          DIFZ=DIFZ+XM(M)*DVISZ(MR)
  272   CONTINUE
      ENDIF
CIPK NOV97
  273 CONTINUE  
      H = 0.0
      DHDX = 0.0
      A0=0.0
      WID=0.
      DAODX = 0.0
      DWDX=0.
CIPK OCT98      SIGMAX = 0.0
      BETA3 = 0.0
      RHO=0.0
CAUG93IPK DRODX=0.0
      DRODZ=0.0
      DRDXIN=0.0
      RHOS=0.0
      DRODXS=0.0
cipk jan99
      SIDL=0.
      SIDS=0.
      dwdz=0.
      DO 275 M=1,NCNX
      IF(XM(M) .EQ. 0.) GO TO 275
      MR=NLC(2*M-1)
      A0=A0+XM(M)*AO(MR)
cipk jan99      WID=WID+XM(M)*WIDTH(MR)
cipk jan99
      WID = WID + XM(M)*WIDTHZ(2*M-1)
      DWDZ = DWDZ + DMZ(M)*WIDTHZ(2*M-1)
      H = H + XM(M)*VEL(3,MR)
      BETA3=BETA3+XM(M)*VDOT(3,MR)
      DHDX = DHDX + DMX(M)*VEL(3,MR)
CIPK JAN99
      DWDX = DWDX + DMX(M)*WIDTHZ(2*M-1)
      DAODX = DAODX + DMX(M)*AO(MR)
      RHO=RHO+XM(M)*DEN(MR)
CAUG93IPK DRODX=DRODX+DMX(M)*DEN(MR)
      DRODZ=DRODZ+DMZ(M)*DEN(MR)
      DRDXIN=DRDXIN+XM(M)*DROXIN(MR)
Cipk jan99

         SIDL = SIDL + XM(M)*SIDN(MR)*VTRB(2*M-1)

      IF(MR .GT. NPM) MR=NSURF(MR)
      RHOS=RHOS+XM(M)*DEN(MR)
      DRODXS=DRODXS+DMX(M)*DEN(MR)

  275 CONTINUE
      AMW=AMW*WID
      AMR=AMW*RHO
      XHT=ELEV-A0
cipk nov97 add tvol and skip out fot ntx=3
      TVOL(NN)=TVOL(NN)+AMW/XHT
      IF(NTX .EQ. 3) GO TO 500

CIPK JAN99

C
C  Calculate dfact at Gauss point
      
      dfactz = 1.
      dfact = 1.
c
      if (ndf .gt. 3)  then
cjan94
          if(grav .lt. 32.) then
c     correction to make it in feet units
            drodz=drodz/3.2808
          endif

          dcrit =  1.1335E-6
          if (drodz/rho .ge. 1.e-8)  then
              dfactz = 10.
              dfact = 1.
          else
             drdyc =  -drodz/rho
             if (grav .lt. 10.)  drdyc = drdyc * .3048
             if (drdyc .gt. dcrit)  
     &            dfact = 0.6888e-4 * drdyc**(-.7)
             if (dfact .lt. .05)  dfact = .05
          endif
      endif
c     
cipk jan99 end additions

      ZIN=(Z-A0)/XHT
      ZA=(ELEV-Z)/XHT
CIPK AUG95 ADD DEFINITION FOR DGP AND DF1
      DGP=ZA*H
C
C    Scale diffusion for near surface elements
C
cipk oct96      IF(GRAV .GT. 30.) THEN
cipk oct96        DMIX= 3.*3.2808
cipk oct96      ELSE
cipk oct96        DMIX=3.
cipk oct96      ENDIF
      IF(DGP .LT. DMIX) THEN
        DF1=(1.+ (DMIX-DGP)/DMIX*32.)
        IF(DFACT .GT. 1.) THEN
          IF(DF1 .GT. DFACT) DFACT=DF1
        ELSE
          DFACT=DF1*DFACT
        ENDIF
      ENDIF
C      
C..... GAUSSIAN BASED MELLOR-YAMADA
C
      IF(IOPTZD .EQ. 3) THEN
        CALL NELLII(DRODZ,DUDZ,DVDZ,H,A0,Z,EPSXZ,EPSYZ,DIFZ,RHO,
     +              ELEV,GRAV)
      ENDIF
      IF(IOPTZD .EQ. 0) THEN
cipk oct96      EPSXZ=ORT(NR,6)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT
        EPSXZ=
     +      ORT(NR,6)*(EDD1(NR)+ZIN*(EDD2(NR)+ZIN*EDD3(NR)))*DFACT*XHT
cipk jan99
        DIFZ=ORT(NR,10)*DFACT*XHT*dfactz
c        EPSXZ=ORT(NR,6)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT/H
c        DIFZ=ORT(NR,10)*DFACT*XHT/H
       ELSE
cwlp    IF(DIFZ .LT. 0.) DIFZ=0.
cwlp    IF(EPSXZ .LT. 0.) EPSXZ=0.
cwlp    EPSXZ=(ORT(NR,6)+EPSXZ)*XHT/H
cwlp    DIFZ=(ORT(NR,10)+DIFZ)*XHT/H
      ENDIF
      IF(ICK .EQ. 4) THEN
        DRDS=DRODS(S,IGF)
CIPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ELSEIF(ICK .EQ. 5) THEN
        DRDS=DRODTM(S,IGF)
CIPK AUG95 GET RATES
        IF(ICYC .GT. 0) THEN
          CALL MKTEMP(S,H,DGP,SRCSNK,GRATE,DELT,NR,NETYP(NN))
        ELSE
          GRATE=0.
          SRCSNK=0.
        ENDIF
      ELSE
        DRDS=DRODSD(S,IGF)
CIPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ENDIF
CC        WRITE(75,*) 'IN COEFV' NN,SRCSNK,GRATE,S
C
C.....Evaluate the basic equations with present values.....
C
      A1=XHT*DAODX
      B1=-ELEV/XHT*DAODX
      D1=DAODX/XHT
      TX=A1+B1*H-A0*DHDX+(D1*H+DHDX)*Z
      WUT=W*XHT-U*TX-(Z-A0)*BETA3
C-
C...... First the momentum equations
C-
      IF(IVLP .EQ. 1) THEN
      FRN = 0.0
      IF( ICYC .LE. 0 ) GO TO 279
      FRN=AMR*BETA1*H
C-
C.....MOMENTUM VISCOUS AND PRESSURE TERMS
C
  279 CONTINUE
C      EPSXZ=EPSXZ*XHT
      IF(NTX .EQ. 2) GO TO 466
CMAY93      FRN = FRN +AMR*(H* U*DUDX+WUT*DUDZ)
CMAY93     1 +AMW*(EPSX*DHDX*DUDX+
CMAY93     2  GRAV*RHOS*H*DAODX+GRAV*DRDXIN*H)
CMAY93     3 -AMW*GRAV*H**2/2.*(DWDX/WID*RHOS+DRODXS)
CIPKJAN99      FRN = FRN +AMR*(H* U*DUDX+WUT*DUDZ)
CIPKJAN99     1 +AMW*(-EPSX*DAODX*DUDX*H/XHT+
CIPKJAN99     2  GRAV*RHOS*H*DAODX+GRAV*DRDXIN*H)
CIPKJAN99     3 -AMW*GRAV*H**2/2.*(DWDX/WID*RHOS+DRODXS)
      FRN = FRN +AMR*(H* U*DUDX+WUT*DUDZ)
     1 +AMW*(EPSX*DHDX*DUDX+
     2  GRAV*RHOS*H*DAODX+GRAV*DRDXIN*H)
     3 -AMW*GRAV*H**2/2.*(DWDX/WID*RHOS+DRODXS)
     + +AMR*U*SIDF(NN)
CIPK NOV97 ADD SIDEFLOW
      FRNX=H*AMW*(EPSX*DUDX-RHOS*GRAV*H/2.)
      FRNZ=AMW*EPSXZ*DUDZ*XHT

C-
C...... Friction terms
C-
cipk jan99
      wdfac = sqrt(1. + dwdz**2)
	IF(GRAV .LT. 30.) WDFAC=WDFAC*2.21
      IF(ORT(NR,11) .GT. 1.) THEN
cipk jan99        FAC=2.*GRAV*AMR*H/(WID*ORT(NR,11)**2)
        FAC=2.*GRAV*AMR*wdfac*H/(WID*ORT(NR,11)**2)
      ELSEIF(ORT(NR,11) .GT. 0.) THEN

CIPK MAR01  ADD VARIABLE WALL FRICTION BASED ON ELEVATION
        IF(MANMIN(NR) .GT. 0.) THEN
	    IF(H+A0 .LT. ELMMIN(NR) ) THEN 
            FAC = 2.*GRAV*AMR* wdfac* H**0.66667*MANMIN(NR)**2
     &                                                /(2.21*WID)
	    ELSEIF(H+A0 .GT. ELMMAX(NR) ) THEN 
            FAC = 2.*GRAV*AMR* wdfac* H**0.66667*MANMAX(NR)**2
     &                                                /(2.21*WID)
	    ELSE
	      FSCL=(H+A0-ELMMIN(NR))/(ELMMAX(NR)-ELMMIN(NR))
            FAC = 2.*GRAV*AMR* wdfac* H**0.66667*(MANMIN(NR)+
     +	       FSCL*(MANMAX(NR)-MANMIN(NR)))**2/(2.21*WID)
	    ENDIF
        ELSE

cipk jan99        FAC=2.*GRAV*AMR*H**0.66667*ORT(NR,11)**2/(2.21*WID)
          FAC = 2.*GRAV*AMR* wdfac* H**0.66667*ORT(NR,11)**2
     &                                                /(2.21*WID)
	  ENDIF
	ELSE
	  FAC=0.0
      ENDIF
      FRN=FRN+FAC*U*ABS(U)

CIPK MAR01 ADD DRAG TERMS

      FRN = FRN + AMR*GRAV*DRAGX(NR)*U*ABS(U)*H

C
C.....Motion equations.....
C
      DO 285 M = 1, NCN
      IA = 1 + NDF*(M-1)
      F(IA) = F(IA) - (XN(M)*FRN + DNX(M)*FRNX
     1 + DNZ(M)*FRNZ)*QFACT(M)
  285 CONTINUE
      ENDIF
C
C.....Continuity equation.....
C
      IF(IHLP .EQ. 1) THEN
cipk jan99      FRNC=AMW*(DUDX*H+U*H*DWDX/WID-DUDZ*TX)/XHT
cipk jan99     +     -AMW*SIDF(NN)/XHT
      FRNC=AMW*(DUDX*H+U*H*DWDX/WID-(DUDZ+u*dwdz/wid)*TX)/XHT
     +     -AMW*SIDF(NN)/XHT
     +     -AMW*SIDL*H/(WID*XHT)
cipk jan99 add line above
cipk nov97 add term for sidf
      DO 290 M=1,NCNX
      IF(XM(M) .EQ. 0.) GO TO 290
      IA = 2*NDF*(M-1)+3
      F(IA) = F(IA) - XM(M)*FRNC
  290 CONTINUE
      ENDIF
C-
C......The salinity equation
C-
      IF(ISLP .EQ. 1) THEN
      FRNX=AMW*DIFX*DSDX*H
      FRNZ=AMW*DIFZ*DSDZ*XHT
CMAY93      FRN=AMW*(H*U*DSDX+WUT*DSDZ+DIFX*DSDX*(DHDX+H*DWDX/WID))
      FRN=AMW*(H*U*DSDX+WUT*DSDZ-DIFX*DSDX*DAODX*H/XHT)
     +    -AMW*H*(SRCSNK+GRATE*S)

CIPK JAN99
      IF(SIDF(NN) .GT. 0) FRN=FRN 
     +    -AMW*SIDF(NN)*(SIDQ(NN,ICK-3)-S)
CIPK NOV97 ADD SIDE FLOE TERMS IN LINE ABOVE
CIPK AUG95 ADD RATE TERMS IN LINE ABOVE
      IF( ICYC .GT. 0) FRN=FRN+AMW*DSDT*H
      IA=0
      DO 295 M=1,NCN
      IA=IA+NDF
      F(IA)=F(IA)-(XN(M)*FRN+DNX(M)*FRNX+DNZ(M)*FRNZ)/xht
cipk update to new structure of equations
  295 CONTINUE
      ENDIF
C
C.....Form the x motion equations.....
C
      IF(IVLP .EQ. 1) THEN
CIPK MAR01 ADD DRAG TERM AND FIX FAC MULTIPLIER
      T1=AMR*(H*DUDX-TX*DUDZ)+2.*FAC*ABS(U)
     + +AMR*(SIDF(NN)+GRAV*H*DRAGX(NR)*2.*ABS(U))

CIPK NOV97 ADD SIDEFLOW
CMAY93      T2=AMW*(RHO*U*H+DHDX*EPSX)
CIPKJAN99      T2=AMW*(RHO*U*H-DAODX*EPSX*H/XHT)
      T2=AMW*(RHO*U*H+DHDX*EPSX)
      T8=AMR*WUT
      T5=AMW*EPSX*H
      T7=AMW*EPSXZ*XHT
      IB=1-NDF
      DO 310 N=1,NCN
      IB=IB+NDF
      FEEAN=(XN(N)*T1+DNX(N)*T2+DNZ(N)*T8)*QFACT(N)
      FEEBN=T5*DNX(N)*QFACT(N)
      FEEEN=T7*DNZ(N)*QFACT(N)
C-
C-.....Form the time terms.....
C-
      IF( ICYC .EQ. 0 ) GO TO 304
      FEEAN=FEEAN+AMR*XN(N)*ALTM*H*QFACT(N)
  304 CONTINUE
      IA=1-NDF
      DO 305 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + (XN(M)*FEEAN + DNX(M)*FEEBN
     1  + DNZ(M)*FEEEN)*QFACT(M)
  305 CONTINUE
  310 CONTINUE
C
C.....Form the head terms.....
C
      IF(IHLP .EQ. 1) THEN
CMAY93      T1=AMR*(U*DUDX-DUDZ*(U*(B1+D1*Z)))
CMAY93     +  +AMW*(GRAV*(RHOS* DAODX + DRDXIN))
CMAY93     +  -AMW*H*GRAV*(DWDX/WID*RHOS+DRODXS)
CIPKJAN99      T1=AMR*(U*DUDX-DUDZ*(U*(B1+D1*Z)))
CIPKJAN99     +  +AMW*(GRAV*(RHOS* DAODX + DRDXIN))
CIPKJAN99     +  -AMW*H*GRAV*(DWDX/WID*RHOS+DRODXS)
CIPKJAN99     +  -AMW*EPSX*DAODX*DUDX/XHT
CIPK MAR01 ADD DRAG TERM
      T1=AMR*(U*DUDX-DUDZ*(U*(B1+D1*Z)))
     +  +AMW*(GRAV*(RHOS* DAODX + DRDXIN))
     +  -AMW*H*GRAV*(DWDX/WID*RHOS+DRODXS)
     +  +AMR*GRAV*DRAGX(NR)*U*ABS(U)
CMAY93      T2=AMW*(EPSX*DUDX-RHO*U*DUDZ*(Z-A0))
CIPKJAN99      T2=-AMW*RHO*U*DUDZ*(Z-A0)

      T2=AMW*(EPSX*DUDX-RHO*U*DUDZ*(Z-A0))

      T4=AMW*(EPSX*DUDX-RHOS*GRAV*H)
      IB=3-NDF*2
      DO 325 N=1,NCNX
        IB=IB+NDF*2
        IF(XM(N) .NE. 0.) THEN
          FEEAN=XM(N)*T1+DMX(N)*T2
          FEEBN=XM(N)*T4
C-
C-.....Form the time terms.....
C-
          IF( ICYC .GT. 0 ) THEN
            FEEAN=FEEAN+AMR*XM(N)*(BETA1-ALTM*(Z-A0)*DUDZ)
          ENDIF
          IA=1-NDF
          DO 320 M = 1, NCN
            IA=IA+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+(XN(M)*FEEAN + DNX(M)*FEEBN)*
     +                    QFACT(M)
  320     CONTINUE
        ENDIF
  325 CONTINUE
      IF(ISLP .EQ. 1) THEN
C-
C......Form the salinity terms
C-
c      TAC=AMW*DRDS*(GRAV*(-H**2*0.5*DWDX/WID+H*DAODX)+H*U*DUDX+WUT*DUDZ)
c     +     +AMW*GRAV*H*ZA*(DHDX+H*DWDX/WID)*DRDS
c      TAB=AMW*GRAV*H**2*DRDS*(ZA-0.5)
c      TAA=-AMW*DRDS*H*(DPDR+GRAV*H/2.)
      TAC=AMW*DRDS*(H*U*DUDX+WUT*DUDZ)
      IF(ICYC .GT. 0) TAC=TAC+AMW*DRDS*H*BETA1
      IB=4-NDF
      DO 330 N=1,NCN
      IB=IB+NDF
c      FEEAN= XN(N)*TAA
c      FEEBN=DNX(N)*TAB+XN(N)*TAC
      FEEBN=XN(N)*TAC
      IA=1-NDF
      DO 329 M=1,NCN
        IA=IA+NDF
c        ESTIFM(IA,IB)=ESTIFM(IA,IB)+(DNX(M)*FEEAN+XN(M)*FEEBN)*QFACT(M)
        ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(M)*FEEBN*QFACT(M)
  329 CONTINUE
  330 CONTINUE
      ENDIF
      ENDIF
      ENDIF
C
C.....FORM THE CONTINUITY EQUATIONS.....
C
      IF(IHLP .EQ. 1) THEN
      IF(IVLP .EQ. 1) THEN
      T1=AMW*H/XHT
      T1A=AMW*H*DWDX/(WID*XHT)
cipk jan99
      t1a = t1a - amw*(dwdz/wid)*tx/xht
      T2A=AMW*TX/XHT
      IA=3-NDF*2
      DO 361 M=1,NCNX
      IA=IA+NDF*2
      IF(XM(M) .EQ. 0.) GO TO 361
      FEEAN=XM(M)*T1
      FEEBN=XM(M)*T1A
      FEEDN=-XM(M)*T2A
      IB=1-NDF
      DO 360 N = 1, NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+(FEEAN*DNX(N)+FEEBN*XN(N)+FEEDN*DNZ(N)
     +              )*QFACT(N)

CIPK JAN99
                  MR = NLC(N)
                  IF (SIDN(MR) .NE. 0.) THEN
                     IC = IB+NDF*11
                     ntrb = nop(nn,n+11)
                     ESTIFM(IA,IC) = ESTIFM(IA,IC) +
     +                  AMW*XM(M)*H/WID*SIDN(MR)*dir(ntrb)*XN(N)/XHT
C                     write(76,*) 'est',nn,i,ia,ic,estifm(ia,ic)
                  ENDIF


  360 CONTINUE
  361 CONTINUE
      ENDIF
CIPK JAN99 CHANGE LINE BELOW
      T4=AMW*(DUDX+(U*DWDX-SIDL)/WID-(B1+D1*Z)*DUDZ)/XHT
      T5=AMW*(A0-Z)*DUDZ/XHT
      IA=3-NDF*2
      DO 365 M=1,NCNX
      IA=IA+NDF*2
      IB=3-NDF*2
      FEEAN=XM(M)*T4
      FEEBN=XM(M)*T5
      DO 363 N=1,NCNX
      IB=IB+NDF*2
      IF(XM(N) .EQ. 0.) GO TO 363
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*FEEAN+DMX(N)*FEEBN
  363 CONTINUE
  365 CONTINUE
      ENDIF
C-
C......FORM THE SALINITY EQUATION
C-
C-
C......VELOCITY AND HEAD TERMS
C-
      IF(ISLP .EQ. 1) THEN
      IF(IVLP .EQ. 1) THEN
      T1=AMW*(H*DSDX-TX*DSDZ)
      IA=4-NDF
      DO 387 M=1,NCN
      IA=IA+NDF
cipk aug98
      FEEAN=XN(M)*T1/xht
      IB=1-NDF
      DO 385 N=1,NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEAN*QFACT(N)
  385 CONTINUE
  387 CONTINUE
      ENDIF
      IF(IHLP .EQ. 1) THEN
      IA=4-NDF
      DO 400 M=1,NCN
      IA=IA+NDF
CMAY93      T3=AMW*(DIFX*DSDX-U*DSDZ*(Z-A0))
      T3=-AMW*U*DSDZ*(Z-A0)
CMAY93      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)+DIFX*DSDX*DWDX/WID)
      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)-DIFX*DAODX*DSDX/XHT)
      IF(ICYC .GT. 0) T5=T5+AMW*(DSDT-ALTM*(Z-A0)*DSDZ)
cipk aug98
      FEECN=XN(M)*T3/xht
      FEEEN=(XN(M)*T5 + DNX(M)*AMW*DIFX*DSDX)/xht
      IB=3-2*NDF
      DO 390 N=1,NCNX
      IB=IB+2*NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+DMX(N)*FEECN+XM(N)*FEEEN
  390 CONTINUE
  400 CONTINUE
      ENDIF
C-
C......FOR SALINITY TERMS
C-
CIPK AUG 95 REDEFINE T1
      T1 = -AMW*H*GRATE
CIPK JAN99 MAKE SIDEFLOW CONDITONAL
      IF(SIDF(NN) .GT. 0.) T1=T1
     +      +AMW*SIDF(NN)
CIPK NOV97 ADD LINE ABOVE FOR SIDEFLOW
C      T1=0.
      IF(ICYC .GT. 0) T1=T1 + AMW*ALTM*H
      T2=AMW*DIFX*H
      T4=AMW*DIFZ*XHT
CMAY93      T5=AMW*(U*H+DIFX*(DHDX+H*DWDX/WID))
      T5=AMW*(U*H-DIFX*DAODX*H/XHT)
      T7=AMW*WUT
      IA=0
      DO 420 M=1,NCN
      IA=IA+4
      FEEAN=XN(M)*T1
      FEEBN=DNX(M)*T2+XN(M)*T5
      FEEDN=DNZ(M)*T4+XN(M)*T7
      IB=0
      DO 410 N=1,NCN
      IB=IB+4
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+(FEEAN*XN(N)+FEEBN*DNX(N)+FEEDN*DNZ(N)
     +      )/xht
  410 CONTINUE
  420 CONTINUE
      ENDIF
C-
C......COMPUTE STIFFNESS MATRIX FOR VERTICAL VELOCITIES
C-
      GO TO 500
  466 CONTINUE
      IF(ITEQV(MAXN) .NE. 5) THEN
        T1=(DUDX+U*DWDX/WID)*H-TX*DUDZ
CIPK JAN99
        T1 = T1 - SIDL*H/WID-SIDF(NN)
        T1=T1*AMW

      DO 470 M=1,NCN
        F(M)=F(M)-DNZ(M)*T1
cipk jan99      T3=AMW*DNZ(M)*XHT
        T3 = AMW*(DNZ(M)*XHT + XN(M)*(DWDZ/WID)*XHT)
        DO 450 N=1,NCN
          ESTIFM(M,N)=ESTIFM(M,N)+T3*DNZ(N)
  450   CONTINUE
  470 CONTINUE
      ENDIF
  500 CONTINUE
CIPK OCT98      NCNO=NCN
CIPK OCT98      NGPO=NGP
CIPK NOV97 REVISE TO SKIP OUT FOR NTX=3
      IF(NTX .EQ. 0  .OR.  NTX .EQ. 3) RETURN
      IF(NTX .EQ. 2) GO TO 1566
CCCCCCCCCCCCCCCCCCCCC TEMPORARY NOT GOOD FOR TRIANGLES
CIPK JAN99 DELETE OLD JUNCTION LOGIC
CC      DO 650 L=2,NCN,4
CC        N1=NOP(NN,L)
CC        N2=NSURF(N1)
c       WRITE(*,*) 'N1,N2,IBN(N2)',N1,N2,IBN(N2)
CC        IF(N2 .LT. 1) GO TO 650
CC        IF(MOD(NFIX(N1)/100,10) .NE. 2  .AND.  IBN(N2) .EQ. 1) THEN
CC          NA=(L-1)*NDF+1
CC          NB=NA-NDF
CC          IF(L .LT. NCN) THEN
CC            NC=NA+NDF
CC          ELSE
CC            NC=1
CC          ENDIF
c         WRITE(*,*) 'IBN=1',N1,N2,NA
CC          DO 6667 KK=1,NEF
CC            ESTIFM(NA,KK)=0.
CC            ESTIFM(NB,KK)=0.
CC            ESTIFM(NC,KK)=0.
CC 6667     CONTINUE
CC          F(NA)=0.
CC          F(NB)=0.
CC          F(NC)=0.
CC        ENDIF
CC  650 CONTINUE
C-
C...... For 1D - 2D junctions adjust equation for direction
C-
C      DO 1050 N=1,NCN,2
C        M=IABS(NOP(NN,N))
C        IF(ADIF(M) .NE. 0.) THEN
CC         WRITE(*,*) NN,N,M,ADIF(M)
C          NEQ=NDF*NCN
C          IA=NDF*(N-1)+1
C          DO 1040 I=1,NEQ
C            ESTIFM(I,IA)=ESTIFM(I,IA)/COS(ADIF(M))
C            ESTIFM(IA,I)=ESTIFM(IA,I)/COS(ADIF(M))
C 1040     CONTINUE
C          F(IA)=F(IA)/COS(ADIF(M))
C        ENDIF
C 1050 CONTINUE
C-
C......INSERT EXPERIMENTAL UPSTREAM BOUNDARY FLOWS
C-
C-
C......APPLY VERTICAL SHAPE FUNCTION FACTORS
C-
      JA=1-NDF
      DO 1010 JJ=1,NCN
      J=NOP(NN,JJ)
      JA=JA+NDF
      FT=FCTV(J)
      IF(FT .EQ. 1.) GO TO 1010
      IA=1-NDF
      DO 1005 II=1,NCN
      I=NOP(NN,II)
      IA=IA+NDF
      ESTIFM(IA  ,JA  )=ESTIFM(IA  ,JA  )*FT
      ESTIFM(IA+2,JA  )=ESTIFM(IA+2,JA  )*FT
 1005 CONTINUE
 1010 CONTINUE
      DO 1300 N=1,NCN
      M=NOP(NN,N)
      IF(NFIX(M).LT.13000) GO TO 1300
      IRW=(N-1)*NDF+1
      IRH=IRW+2
CIPK JAN99      VX=SQRT(VEL(1,M)**2+VEL(2,M)**2)*WIDTH(M)
      VX = SQRT(VEL(1,M)**2 + VEL(2,M)**2) * WIDTHZ(N)

      IF(NFIX(M)/1000 .EQ. 31)
     1VX=SIGN(VX,VEL(1,M))
      IF(NFIX(M)/1000 .EQ. 13)
     1VX=SIGN(VX,VEL(2,M))
      DO 1200 J=1,NEF
 1200 ESTIFM(IRW,J)=0.
      IF(MOD(N,2) .EQ. 0) GO TO 1250
CIPK JAN99      ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)*WIDTH(M)
         ESTIFM(IRW,IRW) = AREA(NN)*VEL(3,M)*WIDTHZ(N)
      ESTIFM(IRW,IRH)=AREA(NN)*VX
CIPK JAN99      F(IRW)=AREA(NN)*(SPEC(M,1)-VX*VEL(3,M))
         WDZAVG = (2.*WIDTH(M) + (SS1(M)+SS2(M))*VEL(3,M))/2.
         F(IRW) = AREA(NN)*((widthz(n)/wdzavg)*SPEC(M,1)
     &                                          - VX*VEL(3,M))
      GO TO 1300
 1250 CONTINUE
      N4=NOP(NN,N-1)
      N2=MOD(N+1,NCN)
      N3=NOP(NN,N2)
CIPK JAN99      ESTIFM(IRW,IRW)=AREA(NN)*(VEL(3,N4)+VEL(3,N3))/2.*WIDTH(M)
         ESTIFM(IRW,IRW) = AREA(NN)*(VEL(3,N4) + VEL(3,N3))/2.*WIDTHZ(N)
      IRH=IRW-NDF+2
      ESTIFM(IRW,IRH)=AREA(NN)*VX/2.
      IRH=(N2-1)*NDF+3
      ESTIFM(IRW,IRH)=AREA(NN)*VX/2.
CIPK JAN99      F(IRW)=AREA(NN)*(SPEC(M,1)-VX*(VEL(3,N4)+VEL(3,N3))/2.)
         WDZAVG = (2.*WIDTH(M) + (SS1(M)+SS2(M))*VEL(3,M))/2.
         F(IRW) = AREA(NN)*((widthz(n)/wdzavg)*SPEC(M,1)
     &                        - VX*(VEL(3,N4) + VEL(3,N3))/2.)
 1300 CONTINUE
 1320 CONTINUE
C-
C-***********ENTER REDUCTION OF ESTIFM AND F FOR BOUNDARY ELEMENTS
C-
C
      IF(MOD(IMMT,100) .GT. 90) THEN
        DO 1500 I=1,NCN
        IA=(I-1)*NDF
        DO 1500 J=1,3
          IA=IA+1
          F(IA)=0.0
          DO 1490 K=1,NCN
            IB=(K-1)*NDF
            DO 1490  L=1,4
              IB=IB+1
              ESTIFM(IA,IB)=0.
 1490     CONTINUE
 1500 CONTINUE
      ENDIF
c     IF(NCN .EQ. 8) CALL COLLAP(80,ESTIFM,F,IND)
      DO 1410 I=1,NCN
CIPK OCT98 CONVERT TO F90
      J=ABS(NOP(NN,I))
      IA=NDF*(I-1)
      DO 1400 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA.EQ.0) GO TO 1400
      R1(JA)=R1(JA)+F(IA)

      
C      rkeep(ja)=rkeep(ja)+f(ia)
C      ekeep(ja)=ekeep(ja)+estifm(ia,ia)


 1400 CONTINUE
 1410 continue
C     IF(NN .LT.12) WRITE(*,9999) NN,(F(J),J=1,NEF)
C9999 FORMAT('COEFV',I12/(1P4E12.4))
C     IF(NN .EQ. 11) WRITE(*,9998) ESTIFM(16,16)
C9998 FORMAT(1PE15.4)
C-
C......SAVE VELOCITY COEFFICIENT MATRIX
C-
C     IF(ITEQV(MAXN) .LT. 5) THEN
C     WRITE(IVS) NN,((ESTIFL(I,J),J=1,NCN),G(I),I=1,NCN)
C     ENDIF
      RETURN
C-
C......RESTORE VELOCITY COEFFICIENT MATRIX
C-
C1450 READ(IVS) NM,((ESTIFM(I,J),J=1,NCN),F(I),I=1,NCN)
C     IF(NM .NE. NN) GO TO 2000
 1566 CONTINUE
      DO 1600 I=1,NCN
      J=NOP(NN,I)
      IA=NDF*(I-1)
      DO 1600 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA.EQ.0) GO TO 1600
      R1(JA)=R1(JA)+F(IA)
      
      rkeep(ja)=rkeep(ja)+f(ia)
      ekeep(ja)=ekeep(ja)+estifm(ia,ia)

 1600 CONTINUE
      RETURN
*-
*...... Special case for junction element
*-
 2000 return
CIPK DELETE JUNCTION COMPUTATION
cipk jan99   IF(NTX .EQ. 2) RETURN
cc      NCN=NCORN(NN)
c     WRITE(*,*) NN,NCN
cc      F(1)=0.
cc      N1=NOP(NN,1)
cc      XHT=1.0
cc      DO 2010 KK=1,NCN
cc        N1=NOP(NN,KK)
c       WRITE(*,*) 'IBN(N1)',N1,IBN(N1)
cc        IF(N1 .EQ. 0) GO TO 2010
cc        NA=(KK-1)*NDF+1
cc        ESTIFM(1,NA)=DIR(N1)*WIDTH(N1)*VEL(3,N1)
cc        CX=COS(ALFA(N1))
cc        SA=SIN(ALFA(N1))
cc        R=VEL(1,N1)*CX+VEL(2,N1)*SA
cc        ESTIFM(1,NA+2)=DIR(N1)*WIDTH(N1)*R
cc        F(1)=F(1)-ESTIFM(1,NA)*R
cc 2010 CONTINUE
cc      NRX=NOP(NN,1)
cc      CX=COS(ALFA(NRX))
cc      SA=SIN(ALFA(NRX))
cc      RX=VEL(1,NRX)*CX+VEL(2,NRX)*SA
cc      DO 2020 KK=2,NCN
cc        N1=NOP(NN,KK)
cc        IF(N1 .EQ. 0) GO TO 2020
cc        CX=COS(ALFA(N1))
cc        SA=SIN(ALFA(N1))
cc        R=VEL(1,N1)*CX+VEL(2,N1)*SA
cc        NA=(KK-1)*NDF+1
cc        ESTIFM(NA,1)=XHT*RX/GRAV
cc        ESTIFM(NA,3)=XHT
cc        ESTIFM(NA,NA+2)=-XHT
cc        ESTIFM(NA,NA)=-XHT*R/GRAV
cc        F(NA)=XHT*((VEL(3,N1)-VEL(3,NRX))+(AO(N1)-AO(NRX))+
cc     +  (R**2-RX**2)/(2.*GRAV))
c       WRITE(*,*) 'KK,N1,NA,ESTIFM(NA,3),ESTIFM(NA,NA+2),F(NA)',
c    +    KK,N1,NA,ESTIFM(NA,3),ESTIFM(NA,NA+2),F(NA)
cc 2020 CONTINUE
cc      GO TO 1320
C2000 WRITE(LOUT,6500) IVS,NN,NM
C6500 FORMAT(//10X,'EXECUTION TERMINATED'//
C    1 10X,'ERROR READING FILE',I5,/10X,'ELEMENTS NUMBERED',I5,' AND',
C    2 I5,' DO NOT MATCH')
C     RETURN
      END
