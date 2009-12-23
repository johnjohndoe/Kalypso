!IPK  LAST UPDATE AUG 22 2007 UPDATE TO BLKECOM
!IPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
!ipk  last update jan 2 1999 add variable width with depth
!     Last change:  IPK   5 Oct 98    3:35 pm
!ipk  last update Aug 6 1998 complete division by xht for transport eqn
!IPK  LAST UPDATED NOV 20 1997
!ipk  last updated Oct 1 1996
!IPK  LAST UPDATED SEP 8 1995
      SUBROUTINE COEFV(NN,NTX)
      USE BLKHMOD
      USE BLKSMOD
      USE WATPMOD
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKECOM
      SAVE
!
      real*8 vx
!
!IPK JAN99 EXPAND TO F TO 80
!IPK AUG07
      DIMENSION XN(8),DNX(8),DNZ(8),XM(4),DMX(4),DMZ(4),XL(8),ZL(8),    &
     &          WAITX(16),NLC(8),QFACT(8),VTRB(8),VTMP(8)
!AUG93IPK      DIMENSION PDN(8),DPDNR(8)
!IPK OCT98      DIMENSION IND(80)
!ipk jan99
!
      DIMENSION  WIDTHZ(8)
      REAL J11,J12,J21,J22
!ipk sep95 add real*8
      REAL*8 S
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: h
!-
!
!IPK OCT98      DATA IND/1,0,2,0,1,0,2,0,1,0,2,0,3,0,4,0,5,0,6,0,5,0,6,0,5,0,6,0
!IPK OCT98     +        ,3,0,4,0,48*0/
!-
!-.....ASSIGN PROPER COEFS.....
!-
      NCN=NCORN(NN)
      IF(NTX == 0) THEN
        AREA(NN)=0.
      ELSE
        TEL=AREA(NN)
!ipknov97 add tvol
        TVOL(NN)=0.
      ENDIF
!ipk oct98 update to f90
      IMMT=IMAT(NN)
!IPK JAN99
      IF(NETYP(NN) == 17) GO TO 2000
!
      NR = MOD(IMMT,100)
!
      IF(MOD(IMMT,1000) > 900) GO TO 70
!cc      EPSX=ORT(NR,1)
!
!IPK      EPSX = EEXXYY(1,NN)/ROAVG
      EPSX = EEXXYY(1,NN)
!
      NGP=7
      NCNX=NCN/2
      NEF=NCN*NDF
!ipk nov97 revise test
      IF(NTX == 0 .OR. NTX == 3) GO TO 70
!-
!-....CHANGE VERT DIFF IF STRATIFIED.....
!-
!ipk jan99
!
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to obsolete unit nipt
!nipt is obsolete
!-
!
      DFACT=1.
      IF( NDF < 4 ) GO TO 65
      DCRIT =  1.1335E-6
      DRDYC=0.
      NDTC=0
      DO 60 K=1,NCN,2
!IPK OCT98 CONVERT TO F90
        N1=ABS(NOP(NN,K))
        N2=MOD(K+2,NCN)
!IPK OCT98 CONVERT TO F90
        N2=ABS(NOP(NN,N2))
        DY=(CORD(N2,3)-CORD(N1,3))*VEL(3,N1)/(ELEV-AO(N1))
        IF(ABS(DY) > 0.1) THEN
          IF(DY > 0.0) THEN
            DSDY=(VEL(4,N2)-VEL(4,N1))/DY
            DTDY=(VEL(5,N2)-VEL(5,N1))/DY
            DSDDY=(VEL(6,N2)-VEL(6,N1))/DY
          ENDIF
          IF(DY < 0.0) THEN
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
          IF (GRAV < 32.)  THEN
            IF(IPASS1 == 1) THEN
              RHO1=1.94*516.
              DRODT1=0.
            ENDIF
            IF(IPASS2 == 1) THEN
              RHO2=1.94*516.
              DRODT2=0.
            ENDIF
            IF(IPASS3 == 1) THEN
              RHO3=1.94*516.
              DRODT3=0.
            ENDIF
            RHO=RHO1+RHO2+RHO3 - 3.88*516.
          ELSE
            IF(IPASS1 == 1) THEN
              RHO1=1.94
              DRODT1=0.
            ENDIF
            IF(IPASS2 == 1) THEN
              RHO2=1.94
              DRODT2=0.
            ENDIF
            IF(IPASS3 == 1) THEN
              RHO3=1.94
              DRODT3=0.
            ENDIF
            RHO=RHO1+RHO2+RHO3-3.88
          ENDIF
          DRDYC=DRDYC-(DSDY*DRODT1+DTDY*DRODT2+DSDDY*DRODT3)/RHO
        ENDIF
   60 CONTINUE
      DFACT=1.
      IF(NDTC > 0) THEN
        DRDYC=DRDYC/FLOAT(NDTC)
        IF(GRAV < 32.) THEN
!ipk mar94 correct to divide by 3.2808
!         DRDYC=DRDYC*3.2808
          DRDYC=DRDYC/3.2808
!ipk mar94 end change
        ENDIF
!
        IF(DRDYC > DCRIT) DFACT=0.6888E-4*DRDYC**(-0.7)
        IF(DFACT < .10) DFACT=.10
!c        IF(DRDYC < -0.1E-04) DFACT=10.0
      ENDIF
        DFCT(NN)=DFACT
!c        write(75,*) 'drdyc',maxn,nn,drdyc,dfact
   65 CONTINUE
!-
!IPK      DIFX=ORT(NR,8)
      DIFX = EEXXYY(5,NN)
!      WRITE(*,9876) NN,DFACT,DIFX
! 9876 FORMAT(I10,F12.4,1PE12.4)
!-
!...... Set up switches for active degrees of freedom
!-
      IF(ITEQV(MAXN) == 2 .OR. ITEQV(MAXN) > 7) THEN
        IVLP=0
      ELSE
        IVLP=1
      ENDIF
      IF(ITEQV(MAXN) < 2 .OR. (ITEQV(MAXN) > 4                          &
     &                     .AND.  ITEQV(MAXN) < 8)) THEN
        IHLP=1
      ELSE
        IHLP=0
      ENDIF
      IF(ITEQV(MAXN) == 1 .OR. ITEQV(MAXN) == 4) THEN
        ISLP=0
      ELSE
        ISLP=1
      ENDIF
!-
!- INITIALIZE MATRICES AND VARIABLES
!-
   70 DO 72 I=1,NCN
!IPK OCT98 CONVERT TO F90
        NLC(I)=ABS(NOP(NN,I))
!        G(I)=0.
!ipk jan99
        m=nlc(i)
        widthz(i) = width(m) + (ss1(m)+ss2(m))*(cord(m,3)-ao(m))        &
     &                  * (vel(3,m)/(elev-ao(m)))
        IF(NOP(NN,I+11) /= 0) THEN
          ITRB=IABS(NOP(NN,I+11))
          MR=NOP(NN,I)
          CXX=COS(ALFA(ITRB))
          SAA=SIN(ALFA(ITRB))
          VTRB(I)=-(VEL(1,ITRB)*CXX+VEL(2,ITRB)*SAA)*DIR(ITRB)
!
        ELSE
          VTRB(I)=0.
        ENDIF
!
!
!
!AUG93IPK  M=NOP(NN,I)
!AUG93IPK  PDN(I)=-(PRESS(M)-DEN(M)*GRAV*VEL(3,M)*(ELEV-CORD(M,3))/
!AUG93IPK     +         (ELEV-AO(M)))
!AUG93IPK  DPDNR(I)=-GRAV*VEL(3,M)*(ELEV-CORD(M,3))/(ELEV-AO(M))
!        DO 72 J=1,NCN
!          ESTIFL(I,J)=0.
   72 CONTINUE
!
!ipk jan99
!
      do 73 j=2,ncn,2
         n1 = j-1
         n3 = j+1 
         if (j == ncn) n3 = 1
         if (abs(vtrb(j)) > 1.e-7)  then
            vtmp(n1) = (vtrb(n1) + 2.*vtrb(j))/3.
            vtmp(n3) = (vtrb(n3) + 2.*vtrb(j))/3.
         endif
   73 continue
      do 74 j=1,ncn,2
         vtrb(j) = vtmp(j)
   74 continue
!
!ipk jan99 end
!
      IF(NTX == 0) THEN
        IF(NCN == 8) THEN
          N1=NLC(3)
          N2=NLC(4)
          N3=NLC(5)
        ELSEIF(NLC(1)+2 == NLC(3)) THEN
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
!
   75   CONTINUE
!
!IPK JAN99
!
!.......Store tributary length
!
        DO 76 I=1,NCN
          J=NLC(I)
          XTLN(J)=AREA(NN)
   76   CONTINUE
!
!IPK JAN99 END ADDITION
        RETURN
      ENDIF
!IPK JAN99 CHANGE LIMITS TO 80
      DO 77 I=1,80
      F(I) = 0.0
      DO 77 J=1,80
   77 ESTIFM(I,J) = 0.0
      IF(NETYP(NN) == 17) GO TO 2000
      MR=NLC(1)
      CX=COS(TH(NN))
      SAN=SIN(TH(NN))
      DO 78 K = 1, NCN
        N=NLC(K)
        ANGDIF=TH(NN)-ALFA(N)
        IF(ABS(ANGDIF) > 1.5708 .AND. ABS(ANGDIF) < 4.7124) THEN
          QFACT(K)=-1.0
        ELSE
          QFACT(K)=1.0
        ENDIF
        DX=CORD(N,1)-CORD(MR,1)
        DY=CORD(N,2)-CORD(MR,2)
        XL(K)=DX*CX+DY*SAN
        ZL(K)=CORD(N,3)-CORD(MR,3)
   78 CONTINUE
      IF(NCN == 8) THEN
        XL(4)=XL(5)/2.
        XL(8)=XL(7)/2.
      ELSEIF(NLC(1)+2 == NLC(3)) THEN
        XL(4)=XL(5)/2.
        XL(6)=XL(5)/2.
      ELSE
        XL(2)=XL(3)/2.
        XL(6)=XL(3)/2.
      ENDIF
      TFR=TEL/ABS(XL(5))
      IF(NTX == 0) GO TO 220
!
!
!...... OBTAIN BASIS FUNCTIONS AT NODES
!
      CALL SA2(NCN)
!
!ipk jan99
!
!  Find drodxn for top surface nodes
!
      IF (NN <= NEM)  THEN
!
        DO  II=1,2
          IF (II == 1) I = 7
          IF (II == 2) I = 1
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
!-
!-     REPEAT FOR LINEAR FUNCTION
!-
          JJ=0
          DO J=1,NCN,2
             JJ=JJ+1
             XM(JJ)=XMX(JJ,I)
             DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/(DETJ*TFR)
          ENDDO
!
          DRODXN(NLC(I))=0.0
          DO M=1,NCNX
            MR=NLC(2*M-1)
            DRODXN(NLC(I))=DRODXN(NLC(I))+DMX(M)*DEN(MR)
          ENDDO
          IF(NCN == 6)  STOP
        ENDDO
!
      ENDIF
!
!IPK JAN99 END CHANGES
!
!
!...... WORK ONLY ON NODES AT BOTTOM CONNECTIVITY
!
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
!-
!-     REPEAT FOR LINEAR FUNCTION
!-
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
        IF(NCN == 6) GO TO 101
  100 CONTINUE
!
! ..... INTEGRATE VERTICALLY TO FORM NODAL INT OF DRODX
!
  101 CONTINUE
      IF(NLC(1) <= NPM) DROXIN(NLC(1))=0.
      IF(NLC(NCN-1) <= NPM) DROXIN(NLC(NCN-1))=0.
!
!     FOR 8 NODED ELEMENT
!
      IF(NCN == 8) THEN
        DROXIN(NLC(3))=DROXIN(NLC(1))+(DRODXN(NLC(1))+DRODXN(NLC(3)))/  &
     &  2.*VEL(3,NLC(3))/(ELEV-AO(NLC(3)))*(-ZL(3))
        DROXIN(NLC(5))=DROXIN(NLC(7))+(DRODXN(NLC(5))+DRODXN(NLC(7)))/  &
     &  2.*VEL(3,NLC(5))/(ELEV-AO(NLC(5)))*(ZL(7)-ZL(5))
      ELSEIF(NLC(1)+2 == NLC(3)) THEN
!
!     FOR 6 NODES VERTICAL LINE BELOW 1
!      
        DROXIN(NLC(3))=DROXIN(NLC(1))+(DRODXN(NLC(1))+DRODXN(NLC(3)))/  &
     &  2.*VEL(3,NLC(3))/(ELEV-AO(NLC(3)))*(-ZL(3))
      ELSE
!
!     FOR 6 NODES VERTICAL LINE BELOW 5
!
!AUG93ipk       DROXIN(NLC(3))=DROXIN(NLC(3))+(DRODXN(NLC(3))+DRODXN(NLC(5)))/
        DROXIN(NLC(3))=DROXIN(NLC(5))+(DRODXN(NLC(3))+DRODXN(NLC(5)))/  &
     &  2.*VEL(3,NLC(3))/(ELEV-AO(NLC(3)))*(ZL(5)-ZL(3))
!
      ENDIF
!-
!-.....COPY WEIGHTING FUNCTIONS AT GAUSS POINTS.....
!-
!ipk oct98 update to f90
      IF(MOD(IMMT,5000) < 100 ) THEN
        IF( NCN < 8 ) THEN
          NGP = 7
          DO 186 M = 1, NGP
            WAITX(M) = WAITT(M)
  186     CONTINUE
        ELSEIF(ITLVL(MAXN) == 1) THEN
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
        IF( NCN < 8 ) THEN
          DO 192 M = 1, NGP
            WAITX(M)=WAITTH(M)
  192     CONTINUE
        ELSE
          DO 194 M = 1, NGP
            WAITX(M) = WAITRH(M)
  194     CONTINUE
        ENDIF
      ENDIF
!-
!-.....COPY SHAPE FUNCTIONS
!-
      CALL SB2(NCN,NGP)
!-
!-.....COMPUTE LOCAL CORDS.....
!-
!-
!-.....COMPUTE ELEMENT EQUATIONS.....
!-
  220 DO 500 I = 1, NGP
!-
!-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
!-
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
      IF(NTX == 0) GO TO 500
!-
!-     REPEAT FOR LINEAR FUNCTION
!-
      JJ=0
      DO 245 J=1,NCN,2
      JJ=JJ+1
      XM(JJ)=XMX(JJ,I)
      DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/(DETJ*TFR)
      DMZ(JJ)=(J11*CB(JJ,I)-J21*CA(JJ,I))/DETJ
  245 CONTINUE
!
!..... IOPTZD IS CASE OF QUADRATIC MELLOR-YAMADA
!
      IF(IOPTZD == 1) THEN
        EPSXZ = 0.
        DIFZ  = 0.
        DO 269 M=1,NCN
          MR=NLC(M)
          EPSXZ=EPSXZ+XN(M)*EVISXZ(MR)
          DIFZ=DIFZ+XN(M)*DVISZ(MR)
  269   CONTINUE
        epsxz=abs(epsxz)
      ENDIF
!ipk nov97 skip forward for ntx=3
      IF(NTX == 3) GO TO 273
!-
!-
!.....COMPUTE R, S, H AND THEIR DERIVATIVES.....
      DVDZ = 0.0
      U = 0.0
      W=0.0
      Z = 0.0
      DUDX = 0.0
      DUDZ = 0.0
      BETA1 = 0.0
!IPK OCT98      BETA2 = 0.0
      S = 0.0
      DSDT=0.0
      DSDX=0.0
      DSDZ=0.0
!AUG93IPK      P=0.0
!AUG93IPK      DPDX=0.
!AUG93IPK      PD=0.
!AUG93IPK      DPDR=0.
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
!AUG93IPK P=P+XNA*PRESS(MR)
!AUG93IPK PD=PD+XNA*PDN(M)
!AUG93IPK DPDR=DPDR+XNA*DPDNR(M)
!AUG93IPK DPDX=DPDX+DNX(M)*PRESS(MR)
      IF(ICYC < 1) GO TO 270
      DSDT=DSDT+XNA*VDOT(ICK,MR)
      BETA1=BETA1+XNA*(VDOT(1,MR)*CXA+VDOT(2,MR)*SXA)*QFACT(M)
  270 CONTINUE
!
!..... EDDY VISCOSITY BASED ON LINEAR MELLOR-YAMADA
!
      IF(IOPTZD == 2) THEN
        EPSXZ = 0.
        DIFZ  = 0.
        DO 272 M=1,NCNX
          IF(XM(M) == 0.) GO TO 272
          MR=NLC(2*M-1)
          EPSXZ=EPSXZ+XM(M)*EVISXZ(MR)
          DIFZ=DIFZ+XM(M)*DVISZ(MR)
  272   CONTINUE
      ENDIF
!IPK NOV97
  273 CONTINUE  
      H = 0.0
      DHDX = 0.0
      A0=0.0
      WID=0.
      DAODX = 0.0
      DWDX=0.
!IPK OCT98      SIGMAX = 0.0
      BETA3 = 0.0
      RHO=0.0
!AUG93IPK DRODX=0.0
      DRODZ=0.0
      DRDXIN=0.0
      RHOS=0.0
      DRODXS=0.0
!ipk jan99
      SIDL=0.
      SIDS=0.
      dwdz=0.
      DO 275 M=1,NCNX
      IF(XM(M) == 0.) GO TO 275
      MR=NLC(2*M-1)
      A0=A0+XM(M)*AO(MR)
!ipk jan99      WID=WID+XM(M)*WIDTH(MR)
!ipk jan99
      WID = WID + XM(M)*WIDTHZ(2*M-1)
      DWDZ = DWDZ + DMZ(M)*WIDTHZ(2*M-1)
      H = H + XM(M)*VEL(3,MR)
      BETA3=BETA3+XM(M)*VDOT(3,MR)
      DHDX = DHDX + DMX(M)*VEL(3,MR)
!IPK JAN99
      DWDX = DWDX + DMX(M)*WIDTHZ(2*M-1)
      DAODX = DAODX + DMX(M)*AO(MR)
      RHO=RHO+XM(M)*DEN(MR)
!AUG93IPK DRODX=DRODX+DMX(M)*DEN(MR)
      DRODZ=DRODZ+DMZ(M)*DEN(MR)
      DRDXIN=DRDXIN+XM(M)*DROXIN(MR)
!ipk jan99
!
         SIDL = SIDL + XM(M)*SIDN(MR)*VTRB(2*M-1)
!
      IF(MR > NPM) MR=NSURF(MR)
      RHOS=RHOS+XM(M)*DEN(MR)
      DRODXS=DRODXS+DMX(M)*DEN(MR)
!
  275 CONTINUE
      AMW=AMW*WID
      AMR=AMW*RHO
      XHT=ELEV-A0
!ipk nov97 add tvol and skip out fot ntx=3
      TVOL(NN)=TVOL(NN)+AMW/XHT
      IF(NTX == 3) GO TO 500
!
!IPK JAN99
!
!
!  Calculate dfact at Gauss point
!
      dfactz = 1.
      dfact = 1.
!
      if (ndf > 3)  then
!jan94
          if(grav < 32.) then
!     correction to make it in feet units
            drodz=drodz/3.2808
          endif
!
          dcrit =  1.1335E-6
          if (drodz/rho >= 1.e-8)  then
              dfactz = 10.
              dfact = 1.
          else
             drdyc =  -drodz/rho
             if (grav < 10.)  drdyc = drdyc * .3048
             if (drdyc > dcrit)                                         &
     &            dfact = 0.6888e-4 * drdyc**(-.7)
             if (dfact < .05)  dfact = .05
          endif
      endif
!     
!ipk jan99 end additions
!
      ZIN=(Z-A0)/XHT
      ZA=(ELEV-Z)/XHT
!IPK AUG95 ADD DEFINITION FOR DGP AND DF1
      DGP=ZA*H
!
!    Scale diffusion for near surface elements
!
!ipk oct96      IF(GRAV > 30.) THEN
!ipk oct96        DMIX= 3.*3.2808
!ipk oct96      ELSE
!ipk oct96        DMIX=3.
!ipk oct96      ENDIF
      IF(DGP < DMIX) THEN
        DF1=(1.+ (DMIX-DGP)/DMIX*32.)
        IF(DFACT > 1.) THEN
          IF(DF1 > DFACT) DFACT=DF1
        ELSE
          DFACT=DF1*DFACT
        ENDIF
      ENDIF
!      
!..... GAUSSIAN BASED MELLOR-YAMADA
!
      IF(IOPTZD == 3) THEN
        CALL NELLII(DRODZ,DUDZ,DVDZ,H,A0,Z,EPSXZ,EPSYZ,DIFZ,RHO,        &
     &              ELEV,GRAV)
      ENDIF
      IF(IOPTZD == 0) THEN
!ipk oct96      EPSXZ=ORT(NR,6)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT
        EPSXZ=                                                          &
     &      ORT(NR,6)*(EDD1(NR)+ZIN*(EDD2(NR)+ZIN*EDD3(NR)))*DFACT*XHT
!ipk jan99
        DIFZ=ORT(NR,10)*DFACT*XHT*dfactz
!        EPSXZ=ORT(NR,6)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT/H
!        DIFZ=ORT(NR,10)*DFACT*XHT/H
       ELSE
!wlp    IF(DIFZ < 0.) DIFZ=0.
!wlp    IF(EPSXZ < 0.) EPSXZ=0.
!wlp    EPSXZ=(ORT(NR,6)+EPSXZ)*XHT/H
!wlp    DIFZ=(ORT(NR,10)+DIFZ)*XHT/H
      ENDIF
      IF(ICK == 4) THEN
        DRDS=DRODS(S,IGF)
!IPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ELSEIF(ICK == 5) THEN
        DRDS=DRODTM(S,IGF)
!IPK AUG95 GET RATES
        IF(ICYC > 0) THEN
          CALL MKTEMP(S,H,DGP,SRCSNK,GRATE,DELT,NR,NETYP(NN))
        ELSE
          GRATE=0.
          SRCSNK=0.
        ENDIF
      ELSE
        DRDS=DRODSD(S,IGF)
!IPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ENDIF
!C        WRITE(75,*) 'IN COEFV' NN,SRCSNK,GRATE,S
!
!.....Evaluate the basic equations with present values.....
!
      A1=XHT*DAODX
      B1=-ELEV/XHT*DAODX
      D1=DAODX/XHT
      TX=A1+B1*H-A0*DHDX+(D1*H+DHDX)*Z
      WUT=W*XHT-U*TX-(Z-A0)*BETA3
!-
!...... First the momentum equations
!-
      IF(IVLP == 1) THEN
      FRN = 0.0
      IF( ICYC <= 0 ) GO TO 279
      FRN=AMR*BETA1*H
!-
!.....MOMENTUM VISCOUS AND PRESSURE TERMS
!
  279 CONTINUE
!      EPSXZ=EPSXZ*XHT
      IF(NTX == 2) GO TO 466
!MAY93      FRN = FRN +AMR*(H* U*DUDX+WUT*DUDZ)
!MAY93     1 +AMW*(EPSX*DHDX*DUDX+
!MAY93     2  GRAV*RHOS*H*DAODX+GRAV*DRDXIN*H)
!MAY93     3 -AMW*GRAV*H**2/2.*(DWDX/WID*RHOS+DRODXS)
!IPKJAN99      FRN = FRN +AMR*(H* U*DUDX+WUT*DUDZ)
!IPKJAN99     1 +AMW*(-EPSX*DAODX*DUDX*H/XHT+
!IPKJAN99     2  GRAV*RHOS*H*DAODX+GRAV*DRDXIN*H)
!IPKJAN99     3 -AMW*GRAV*H**2/2.*(DWDX/WID*RHOS+DRODXS)
      FRN = FRN +AMR*(H* U*DUDX+WUT*DUDZ)                               &
     & +AMW*(EPSX*DHDX*DUDX+                                            &
     &  GRAV*RHOS*H*DAODX+GRAV*DRDXIN*H)                                &
     & -AMW*GRAV*H**2/2.*(DWDX/WID*RHOS+DRODXS)                         &
     & +AMR*U*SIDF(NN)
!IPK NOV97 ADD SIDEFLOW
      FRNX=H*AMW*(EPSX*DUDX-RHOS*GRAV*H/2.)
      FRNZ=AMW*EPSXZ*DUDZ*XHT
!
!-
!...... Friction terms
!-
!ipk jan99
      wdfac = sqrt(1. + dwdz**2)
      IF(GRAV < 30.) WDFAC=WDFAC*2.21
      IF(ORT(NR,11) > 1.) THEN
!ipk jan99        FAC=2.*GRAV*AMR*H/(WID*ORT(NR,11)**2)
        FAC=2.*GRAV*AMR*wdfac*H/(WID*ORT(NR,11)**2)
      ELSEIF(ORT(NR,11) > 0.) THEN
!
!IPK MAR01  ADD VARIABLE WALL FRICTION BASED ON ELEVATION
        IF(MANMIN(NR) > 0.) THEN
          IF(H+A0 < ELMMIN(NR) ) THEN 
            FAC = 2.*GRAV*AMR* wdfac* H**0.66667*MANMIN(NR)**2          &
     &                                                /(2.21*WID)
          ELSEIF(H+A0 > ELMMAX(NR) ) THEN 
            FAC = 2.*GRAV*AMR* wdfac* H**0.66667*MANMAX(NR)**2          &
     &                                                /(2.21*WID)
          ELSE
            FSCL=(H+A0-ELMMIN(NR))/(ELMMAX(NR)-ELMMIN(NR))
            FAC = 2.*GRAV*AMR* wdfac* H**0.66667*(MANMIN(NR)+           &
     &             FSCL*(MANMAX(NR)-MANMIN(NR)))**2/(2.21*WID)
          ENDIF
        ELSE
!
!ipk jan99        FAC=2.*GRAV*AMR*H**0.66667*ORT(NR,11)**2/(2.21*WID)
          FAC = 2.*GRAV*AMR* wdfac* H**0.66667*ORT(NR,11)**2            &
     &                                                /(2.21*WID)
        ENDIF
      ELSE
        FAC=0.0
      ENDIF
      FRN=FRN+FAC*U*ABS(U)
!
!IPK MAR01 ADD DRAG TERMS
!
      FRN = FRN + AMR*GRAV*DRAGX(NR)*U*ABS(U)*H
!
!
!.....Motion equations.....
!
      DO 285 M = 1, NCN
      IA = 1 + NDF*(M-1)
      F(IA) = F(IA) - (XN(M)*FRN + DNX(M)*FRNX                          &
     & + DNZ(M)*FRNZ)*QFACT(M)
  285 CONTINUE
      ENDIF
!
!.....Continuity equation.....
!
      IF(IHLP == 1) THEN
!ipk jan99      FRNC=AMW*(DUDX*H+U*H*DWDX/WID-DUDZ*TX)/XHT
!ipk jan99     +     -AMW*SIDF(NN)/XHT
      FRNC=AMW*(DUDX*H+U*H*DWDX/WID-(DUDZ+u*dwdz/wid)*TX)/XHT           &
     &     -AMW*SIDF(NN)/XHT                                            &
     &     -AMW*SIDL*H/(WID*XHT)
!ipk jan99 add line above
!ipk nov97 add term for sidf
      DO 290 M=1,NCNX
      IF(XM(M) == 0.) GO TO 290
      IA = 2*NDF*(M-1)+3
      F(IA) = F(IA) - XM(M)*FRNC
  290 CONTINUE
      ENDIF
!-
!......The salinity equation
!-
      IF(ISLP == 1) THEN
      FRNX=AMW*DIFX*DSDX*H
      FRNZ=AMW*DIFZ*DSDZ*XHT
!MAY93      FRN=AMW*(H*U*DSDX+WUT*DSDZ+DIFX*DSDX*(DHDX+H*DWDX/WID))
      FRN=AMW*(H*U*DSDX+WUT*DSDZ-DIFX*DSDX*DAODX*H/XHT)                 &
     &    -AMW*H*(SRCSNK+GRATE*S)
!
!IPK JAN99
      IF(SIDF(NN) > 0) FRN=FRN                                          &
     &    -AMW*SIDF(NN)*(SIDQ(NN,ICK-3)-S)
!IPK NOV97 ADD SIDE FLOE TERMS IN LINE ABOVE
!IPK AUG95 ADD RATE TERMS IN LINE ABOVE
      IF( ICYC > 0) FRN=FRN+AMW*DSDT*H
      IA=0
      DO 295 M=1,NCN
      IA=IA+NDF
      F(IA)=F(IA)-(XN(M)*FRN+DNX(M)*FRNX+DNZ(M)*FRNZ)/xht
!ipk update to new structure of equations
  295 CONTINUE
      ENDIF
!
!.....Form the x motion equations.....
!
      IF(IVLP == 1) THEN
!IPK MAR01 ADD DRAG TERM AND FIX FAC MULTIPLIER
      T1=AMR*(H*DUDX-TX*DUDZ)+2.*FAC*ABS(U)                             &
     & +AMR*(SIDF(NN)+GRAV*H*DRAGX(NR)*2.*ABS(U))
!
!IPK NOV97 ADD SIDEFLOW
!MAY93      T2=AMW*(RHO*U*H+DHDX*EPSX)
!IPKJAN99      T2=AMW*(RHO*U*H-DAODX*EPSX*H/XHT)
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
!-
!-.....Form the time terms.....
!-
      IF( ICYC == 0 ) GO TO 304
      FEEAN=FEEAN+AMR*XN(N)*ALTM*H*QFACT(N)
  304 CONTINUE
      IA=1-NDF
      DO 305 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + (XN(M)*FEEAN + DNX(M)*FEEBN       &
     &  + DNZ(M)*FEEEN)*QFACT(M)
  305 CONTINUE
  310 CONTINUE
!
!.....Form the head terms.....
!
      IF(IHLP == 1) THEN
!MAY93      T1=AMR*(U*DUDX-DUDZ*(U*(B1+D1*Z)))
!MAY93     +  +AMW*(GRAV*(RHOS* DAODX + DRDXIN))
!MAY93     +  -AMW*H*GRAV*(DWDX/WID*RHOS+DRODXS)
!IPKJAN99      T1=AMR*(U*DUDX-DUDZ*(U*(B1+D1*Z)))
!IPKJAN99     +  +AMW*(GRAV*(RHOS* DAODX + DRDXIN))
!IPKJAN99     +  -AMW*H*GRAV*(DWDX/WID*RHOS+DRODXS)
!IPKJAN99     +  -AMW*EPSX*DAODX*DUDX/XHT
!IPK MAR01 ADD DRAG TERM
      T1=AMR*(U*DUDX-DUDZ*(U*(B1+D1*Z)))                                &
     &  +AMW*(GRAV*(RHOS* DAODX + DRDXIN))                              &
     &  -AMW*H*GRAV*(DWDX/WID*RHOS+DRODXS)                              &
     &  +AMR*GRAV*DRAGX(NR)*U*ABS(U)
!MAY93      T2=AMW*(EPSX*DUDX-RHO*U*DUDZ*(Z-A0))
!IPKJAN99      T2=-AMW*RHO*U*DUDZ*(Z-A0)
!
      T2=AMW*(EPSX*DUDX-RHO*U*DUDZ*(Z-A0))
!
      T4=AMW*(EPSX*DUDX-RHOS*GRAV*H)
      IB=3-NDF*2
      DO 325 N=1,NCNX
        IB=IB+NDF*2
        IF(XM(N) /= 0.) THEN
          FEEAN=XM(N)*T1+DMX(N)*T2
          FEEBN=XM(N)*T4
!-
!-.....Form the time terms.....
!-
          IF( ICYC > 0 ) THEN
            FEEAN=FEEAN+AMR*XM(N)*(BETA1-ALTM*(Z-A0)*DUDZ)
          ENDIF
          IA=1-NDF
          DO 320 M = 1, NCN
            IA=IA+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+(XN(M)*FEEAN + DNX(M)*FEEBN)*   &
     &                    QFACT(M)
  320     CONTINUE
        ENDIF
  325 CONTINUE
      IF(ISLP == 1) THEN
!-
!......Form the salinity terms
!-
!      TAC=AMW*DRDS*(GRAV*(-H**2*0.5*DWDX/WID+H*DAODX)+H*U*DUDX+WUT*DUDZ)
!     +     +AMW*GRAV*H*ZA*(DHDX+H*DWDX/WID)*DRDS
!      TAB=AMW*GRAV*H**2*DRDS*(ZA-0.5)
!      TAA=-AMW*DRDS*H*(DPDR+GRAV*H/2.)
      TAC=AMW*DRDS*(H*U*DUDX+WUT*DUDZ)
      IF(ICYC > 0) TAC=TAC+AMW*DRDS*H*BETA1
      IB=4-NDF
      DO 330 N=1,NCN
      IB=IB+NDF
!      FEEAN= XN(N)*TAA
!      FEEBN=DNX(N)*TAB+XN(N)*TAC
      FEEBN=XN(N)*TAC
      IA=1-NDF
      DO 329 M=1,NCN
        IA=IA+NDF
!        ESTIFM(IA,IB)=ESTIFM(IA,IB)+(DNX(M)*FEEAN+XN(M)*FEEBN)*QFACT(M)
        ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(M)*FEEBN*QFACT(M)
  329 CONTINUE
  330 CONTINUE
      ENDIF
      ENDIF
      ENDIF
!
!.....FORM THE CONTINUITY EQUATIONS.....
!
      IF(IHLP == 1) THEN
      IF(IVLP == 1) THEN
      T1=AMW*H/XHT
      T1A=AMW*H*DWDX/(WID*XHT)
!ipk jan99
      t1a = t1a - amw*(dwdz/wid)*tx/xht
      T2A=AMW*TX/XHT
      IA=3-NDF*2
      DO 361 M=1,NCNX
      IA=IA+NDF*2
      IF(XM(M) == 0.) GO TO 361
      FEEAN=XM(M)*T1
      FEEBN=XM(M)*T1A
      FEEDN=-XM(M)*T2A
      IB=1-NDF
      DO 360 N = 1, NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+(FEEAN*DNX(N)+FEEBN*XN(N)+FEEDN*DNZ(N)&
     &              )*QFACT(N)
!
!IPK JAN99
                  MR = NLC(N)
                  IF (SIDN(MR) /= 0.) THEN
                     IC = IB+NDF*11
                     ntrb = nop(nn,n+11)
                     ESTIFM(IA,IC) = ESTIFM(IA,IC) +                    &
     &                  AMW*XM(M)*H/WID*SIDN(MR)*dir(ntrb)*XN(N)/XHT
!                     write(76,*) 'est',nn,i,ia,ic,estifm(ia,ic)
                  ENDIF
!
!
  360 CONTINUE
  361 CONTINUE
      ENDIF
!IPK JAN99 CHANGE LINE BELOW
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
      IF(XM(N) == 0.) GO TO 363
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*FEEAN+DMX(N)*FEEBN
  363 CONTINUE
  365 CONTINUE
      ENDIF
!-
!......FORM THE SALINITY EQUATION
!-
!-
!......VELOCITY AND HEAD TERMS
!-
      IF(ISLP == 1) THEN
      IF(IVLP == 1) THEN
      T1=AMW*(H*DSDX-TX*DSDZ)
      IA=4-NDF
      DO 387 M=1,NCN
      IA=IA+NDF
!ipk aug98
      FEEAN=XN(M)*T1/xht
      IB=1-NDF
      DO 385 N=1,NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEAN*QFACT(N)
  385 CONTINUE
  387 CONTINUE
      ENDIF
      IF(IHLP == 1) THEN
      IA=4-NDF
      DO 400 M=1,NCN
      IA=IA+NDF
!MAY93      T3=AMW*(DIFX*DSDX-U*DSDZ*(Z-A0))
      T3=-AMW*U*DSDZ*(Z-A0)
!MAY93      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)+DIFX*DSDX*DWDX/WID)
      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)-DIFX*DAODX*DSDX/XHT)
      IF(ICYC > 0) T5=T5+AMW*(DSDT-ALTM*(Z-A0)*DSDZ)
!ipk aug98
      FEECN=XN(M)*T3/xht
      FEEEN=(XN(M)*T5 + DNX(M)*AMW*DIFX*DSDX)/xht
      IB=3-2*NDF
      DO 390 N=1,NCNX
      IB=IB+2*NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+DMX(N)*FEECN+XM(N)*FEEEN
  390 CONTINUE
  400 CONTINUE
      ENDIF
!-
!......FOR SALINITY TERMS
!-
!IPK AUG 95 REDEFINE T1
      T1 = -AMW*H*GRATE
!IPK JAN99 MAKE SIDEFLOW CONDITONAL
      IF(SIDF(NN) > 0.) T1=T1                                           &
     &      +AMW*SIDF(NN)
!IPK NOV97 ADD LINE ABOVE FOR SIDEFLOW
!      T1=0.
      IF(ICYC > 0) T1=T1 + AMW*ALTM*H
      T2=AMW*DIFX*H
      T4=AMW*DIFZ*XHT
!MAY93      T5=AMW*(U*H+DIFX*(DHDX+H*DWDX/WID))
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
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+(FEEAN*XN(N)+FEEBN*DNX(N)+FEEDN*DNZ(N)&
     &      )/xht
  410 CONTINUE
  420 CONTINUE
      ENDIF
!-
!......COMPUTE STIFFNESS MATRIX FOR VERTICAL VELOCITIES
!-
      GO TO 500
  466 CONTINUE
      IF(ITEQV(MAXN) /= 5) THEN
        T1=(DUDX+U*DWDX/WID)*H-TX*DUDZ
!IPK JAN99
        T1 = T1 - SIDL*H/WID-SIDF(NN)
        T1=T1*AMW
!
      DO 470 M=1,NCN
        F(M)=F(M)-DNZ(M)*T1
!ipk jan99      T3=AMW*DNZ(M)*XHT
        T3 = AMW*(DNZ(M)*XHT + XN(M)*(DWDZ/WID)*XHT)
        DO 450 N=1,NCN
          ESTIFM(M,N)=ESTIFM(M,N)+T3*DNZ(N)
  450   CONTINUE
  470 CONTINUE
      ENDIF
  500 CONTINUE
!IPK OCT98      NCNO=NCN
!IPK OCT98      NGPO=NGP
!IPK NOV97 REVISE TO SKIP OUT FOR NTX=3
      IF(NTX == 0 .OR. NTX == 3) RETURN
      IF(NTX == 2) GO TO 1566
!CCCCCCCCCCCCCCCCCCCC TEMPORARY NOT GOOD FOR TRIANGLES
!IPK JAN99 DELETE OLD JUNCTION LOGIC
!C      DO 650 L=2,NCN,4
!C        N1=NOP(NN,L)
!C        N2=NSURF(N1)
!       WRITE(*,*) 'N1,N2,IBN(N2)',N1,N2,IBN(N2)
!C        IF(N2 < 1) GO TO 650
!C        IF(MOD(NFIX(N1)/100,10) /= 2 .AND. IBN(N2) == 1) THEN
!C          NA=(L-1)*NDF+1
!C          NB=NA-NDF
!C          IF(L < NCN) THEN
!C            NC=NA+NDF
!C          ELSE
!C            NC=1
!C          ENDIF
!         WRITE(*,*) 'IBN=1',N1,N2,NA
!C          DO 6667 KK=1,NEF
!C            ESTIFM(NA,KK)=0.
!C            ESTIFM(NB,KK)=0.
!C            ESTIFM(NC,KK)=0.
!C 6667     CONTINUE
!C          F(NA)=0.
!C          F(NB)=0.
!C          F(NC)=0.
!C        ENDIF
!C  650 CONTINUE
!-
!...... For 1D - 2D junctions adjust equation for direction
!-
!      DO 1050 N=1,NCN,2
!        M=IABS(NOP(NN,N))
!        IF(ADIF(M) /= 0.) THEN
!C         WRITE(*,*) NN,N,M,ADIF(M)
!          NEQ=NDF*NCN
!          IA=NDF*(N-1)+1
!          DO 1040 I=1,NEQ
!            ESTIFM(I,IA)=ESTIFM(I,IA)/COS(ADIF(M))
!            ESTIFM(IA,I)=ESTIFM(IA,I)/COS(ADIF(M))
! 1040     CONTINUE
!          F(IA)=F(IA)/COS(ADIF(M))
!        ENDIF
! 1050 CONTINUE
!-
!......INSERT EXPERIMENTAL UPSTREAM BOUNDARY FLOWS
!-
!-
!......APPLY VERTICAL SHAPE FUNCTION FACTORS
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
      ESTIFM(IA+2,JA  )=ESTIFM(IA+2,JA  )*FT
 1005 CONTINUE
 1010 CONTINUE
      DO 1300 N=1,NCN
      M=NOP(NN,N)
      IF(NFIX(M) < 13000) GO TO 1300
      IRW=(N-1)*NDF+1
      IRH=IRW+2
!IPK JAN99      VX=SQRT(VEL(1,M)**2+VEL(2,M)**2)*WIDTH(M)
      VX = SQRT(VEL(1,M)**2 + VEL(2,M)**2) * WIDTHZ(N)
!
      IF(NFIX(M)/1000 == 31)                                            &
     &VX=SIGN(VX,VEL(1,M))
      IF(NFIX(M)/1000 == 13)                                            &
     &VX=SIGN(VX,VEL(2,M))
      DO 1200 J=1,NEF
 1200 ESTIFM(IRW,J)=0.
      IF(MOD(N,2) == 0) GO TO 1250
!IPK JAN99      ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)*WIDTH(M)
         ESTIFM(IRW,IRW) = AREA(NN)*VEL(3,M)*WIDTHZ(N)
      ESTIFM(IRW,IRH)=AREA(NN)*VX
!IPK JAN99      F(IRW)=AREA(NN)*(SPEC(M,1)-VX*VEL(3,M))
         WDZAVG = (2.*WIDTH(M) + (SS1(M)+SS2(M))*VEL(3,M))/2.
         F(IRW) = AREA(NN)*((widthz(n)/wdzavg)*SPEC(M,1)                &
     &                                          - VX*VEL(3,M))
      GO TO 1300
 1250 CONTINUE
      N4=NOP(NN,N-1)
      N2=MOD(N+1,NCN)
      N3=NOP(NN,N2)
!IPK JAN99      ESTIFM(IRW,IRW)=AREA(NN)*(VEL(3,N4)+VEL(3,N3))/2.*WIDTH(M)
         ESTIFM(IRW,IRW) = AREA(NN)*(VEL(3,N4) + VEL(3,N3))/2.*WIDTHZ(N)
      IRH=IRW-NDF+2
      ESTIFM(IRW,IRH)=AREA(NN)*VX/2.
      IRH=(N2-1)*NDF+3
      ESTIFM(IRW,IRH)=AREA(NN)*VX/2.
!IPK JAN99      F(IRW)=AREA(NN)*(SPEC(M,1)-VX*(VEL(3,N4)+VEL(3,N3))/2.)
         WDZAVG = (2.*WIDTH(M) + (SS1(M)+SS2(M))*VEL(3,M))/2.
         F(IRW) = AREA(NN)*((widthz(n)/wdzavg)*SPEC(M,1)                &
     &                        - VX*(VEL(3,N4) + VEL(3,N3))/2.)
 1300 CONTINUE
 1320 CONTINUE
!-
!-***********ENTER REDUCTION OF ESTIFM AND F FOR BOUNDARY ELEMENTS
!-
!
      IF(MOD(IMMT,100) > 90) THEN
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
!     IF(NCN == 8) CALL COLLAP(80,ESTIFM,F,IND)
      DO 1410 I=1,NCN
!IPK OCT98 CONVERT TO F90
      J=ABS(NOP(NN,I))
      IA=NDF*(I-1)
      DO 1400 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA == 0) GO TO 1400
      R1(JA)=R1(JA)+F(IA)
!
!
!      rkeep(ja)=rkeep(ja)+f(ia)
!      ekeep(ja)=ekeep(ja)+estifm(ia,ia)
!
!
 1400 CONTINUE
 1410 continue
!     IF(NN < 12) WRITE(*,9999) NN,(F(J),J=1,NEF)
!9999 FORMAT('COEFV',I12/(1P4E12.4))
!     IF(NN == 11) WRITE(*,9998) ESTIFM(16,16)
!9998 FORMAT(1PE15.4)
!-
!......SAVE VELOCITY COEFFICIENT MATRIX
!-
!     IF(ITEQV(MAXN) < 5) THEN
!     WRITE(IVS) NN,((ESTIFL(I,J),J=1,NCN),G(I),I=1,NCN)
!     ENDIF
      RETURN
!-
!......RESTORE VELOCITY COEFFICIENT MATRIX
!-
!1450 READ(IVS) NM,((ESTIFM(I,J),J=1,NCN),F(I),I=1,NCN)
!     IF(NM /= NN) GO TO 2000
 1566 CONTINUE
      DO 1600 I=1,NCN
      J=NOP(NN,I)
      IA=NDF*(I-1)
      DO 1600 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA == 0) GO TO 1600
      R1(JA)=R1(JA)+F(IA)
!
      rkeep(ja)=rkeep(ja)+f(ia)
      ekeep(ja)=ekeep(ja)+estifm(ia,ia)
!
 1600 CONTINUE
      RETURN
!-
!...... Special case for junction element
!-
 2000 return
!IPK DELETE JUNCTION COMPUTATION
!ipk jan99   IF(NTX == 2) RETURN
!c      NCN=NCORN(NN)
!     WRITE(*,*) NN,NCN
!c      F(1)=0.
!c      N1=NOP(NN,1)
!c      XHT=1.0
!c      DO 2010 KK=1,NCN
!c        N1=NOP(NN,KK)
!       WRITE(*,*) 'IBN(N1)',N1,IBN(N1)
!c        IF(N1 == 0) GO TO 2010
!c        NA=(KK-1)*NDF+1
!c        ESTIFM(1,NA)=DIR(N1)*WIDTH(N1)*VEL(3,N1)
!c        CX=COS(ALFA(N1))
!c        SA=SIN(ALFA(N1))
!c        R=VEL(1,N1)*CX+VEL(2,N1)*SA
!c        ESTIFM(1,NA+2)=DIR(N1)*WIDTH(N1)*R
!c        F(1)=F(1)-ESTIFM(1,NA)*R
!c 2010 CONTINUE
!c      NRX=NOP(NN,1)
!c      CX=COS(ALFA(NRX))
!c      SA=SIN(ALFA(NRX))
!c      RX=VEL(1,NRX)*CX+VEL(2,NRX)*SA
!c      DO 2020 KK=2,NCN
!c        N1=NOP(NN,KK)
!c        IF(N1 == 0) GO TO 2020
!c        CX=COS(ALFA(N1))
!c        SA=SIN(ALFA(N1))
!c        R=VEL(1,N1)*CX+VEL(2,N1)*SA
!c        NA=(KK-1)*NDF+1
!c        ESTIFM(NA,1)=XHT*RX/GRAV
!c        ESTIFM(NA,3)=XHT
!c        ESTIFM(NA,NA+2)=-XHT
!c        ESTIFM(NA,NA)=-XHT*R/GRAV
!c        F(NA)=XHT*((VEL(3,N1)-VEL(3,NRX))+(AO(N1)-AO(NRX))+
!c     +  (R**2-RX**2)/(2.*GRAV))
!       WRITE(*,*) 'KK,N1,NA,ESTIFM(NA,3),ESTIFM(NA,NA+2),F(NA)',
!    +    KK,N1,NA,ESTIFM(NA,3),ESTIFM(NA,NA+2),F(NA)
!c 2020 CONTINUE
!c      GO TO 1320
!2000 WRITE(LOUT,6500) IVS,NN,NM
!6500 FORMAT(//10X,'EXECUTION TERMINATED'//
!    1 10X,'ERROR READING FILE',I5,/10X,'ELEMENTS NUMBERED',I5,' AND',
!    2 I5,' DO NOT MATCH')
!     RETURN
      END
