!IPK  LAST UPDATE AUG 22 2007 UPDATE TO BLKECOM
!IPK  LAST UPDATE APR 20 2001 MOVE INITIALIZATION OF VOLS TO LATER ON 
!IPK  LAST UPDATE MAR 5 2001 ADD DRAG
!ipk  last change Aug 2 2000 initialize epsxz etc.
!ipk  last update Nov 18 1999 clean up test for DFCT
!     Last change:  WP   29 Aug 2007    3:07 pm
!ipk  last update Aug 6 1998 complete division by xht for transport eqn
!ipk  last update Feb 4 1998 allow for vscale from HCN input
!IPK  LAST UPDATE NOV 21 1997
!ipk  last update Oct 31 1996 revamp DROXIN and DROYIN
!ipk  last update Oct 9 1996 correct formulations for vertical eddy etc
!ipk  last update Oct 1 1996 add new formulations for EXX and EYY
!ipk  last update Aug27 1996  Modify horizontal diffusion formulation
      SUBROUTINE COEF3(NN,NTX)
      USE IHILMOD
      USE COEF3MOD
      USE BLKHMOD
      USE SACMOD
      USE WTPRMOD
      USE BLK10
      USE BLK11MOD
      USE BLK10MOD
      USE BLKECOM
      SAVE
!-
!ycw aug94 add double precision salt
      REAL*8 S
      REAL J11,J12,J13,J21,J22,J23,J31,J32,J33
!-
!ipk jan95 add double precision for basis function values
!     DOUBLE PRECISION DA,DB,DC
!-
      REAL WAITX(64)
!
!AUG93IPK     1            ,WAITX(64),IH(20,4),IL(20,4),PDN(20),DPDNR(20)
      DIMENSION PROJL(20)
!IPK SEP96 ADD PROJL
!IPK OCT98      DATA FNCNO/0/,NGPO/0/
!
!-
!IPK OCT98      DATA IPASS/0/
!-
!-.....ASSIGN PROPER COEFS.....
!-
!
!IPK AUG00
      EPSXZ=0.
      EPSYZ=0.
      DIFZ=0.
!
      IF (GRAV < 32.)  THEN
        CVF2=3.28
        CVFCT=516./3.28
      ELSE
        CVF2=1.0
        CVFCT=1.0
      ENDIF
      ROAVG=1.935
      IF (GRAV < 32.)  ROAVG = 516. * 1.935
      NCN=NCORN(NN)
!c      WRITE(75,*) 'ENTERING COEF37',NN,NTX,NCN,IMAT(NN)
      ILK=1
      IF(NCN == 15) ILK=2
      IF(NCN == 10) ILK=3
      IF(NCN == 13) ILK=4
      IF(NTX == 0) GO TO 65
!      IF(NTX == 2) GO TO 1400
!-
!-....CHANGE VERT DIFF IF STRATIFIED.....
!-
      IF( NDF < 4 ) GO TO 65
      DCRIT =  1.1335E-6
      DRDYC=0.
      NDTC=0
      DO 60 K=1,NCN
        IF(IL(K,ILK) == 0) GO TO 60
        N1=IL(K,ILK)
!IPK OCT98 CONVERT TO F90
        N1=ABS(NOP(NN,N1))
        N2=IH(K,ILK)
!IPK OCT98 CONVERT TO F90
        N2=ABS(NOP(NN,N2))
        DY=(CORD(N2,3)-CORD(N1,3))*VEL(3,N1)/(ELEV-AO(N1))
        DX=SQRT((CORD(N2,1)-CORD(N1,1))**2+(CORD(N2,2)-CORD(N1,2))**2)
        IF(ABS(DY) > 0.1 .AND. ABS(DX) < 1.00) THEN
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
!ipk mar94 change multiply to divide
!         DRDYC=DRDYC*3.2808
          DRDYC=DRDYC/3.2808
!ipk mar94 end change
        ENDIF
        IF(DRDYC > DCRIT) DFACT=0.6888E-4*DRDYC**(-0.7)
        IF(DFACT < .10) DFACT=.10
        if(drdyc < -1.e-8) dfact=10.
        if(drdyc < -1.e-7) dfact=100.
      ENDIF
      if(maxn == 1 .AND. ntx == 1) then
        DFCT(NN)=DFACT
!ipk nov99 refine test to catch zero dfct
      elseif(dfct(nn) == 0.) then
        dfct(nn)=dfact
      else
        dfact=dfct(nn)
      endif
!c       WRITE(75,9876) NN,DFACT,drdyc,dfct(nn),maxn,ntx
!c 9876 FORMAT('dfact',I10,F12.4,1p2e13.5,2i5)
   65 CONTINUE
      NGP=21
      IF(NCN == 20) NGP=27
      IF(NCN == 13) NGP=27
      IF(IMAT(NN) > 100) NGP=64
      IF(ITLVL(MAXN) == 1 .AND. NCN == 20) NGP=8
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
!-.....ASSIGN PROPER COEFS.....
!-
!IPK APR 01 INITIALISATION MOVED FURTHER ON 
!      AREA(NN)=0.
!IPK NOV97
!      TVOL(NN)=0.
!ipk oct98 update to f90
      IMMT=IMAT(NN)
      MT = MOD(IMMT,100)
!      IF(MT > 90) THEN
!        IVLP=0
!        IHLP=0
!        ISLP=1
!      ENDIF
!
!ipk  sep96 add new logic for control of horizontal diffusion
!
      IF(IEDSW == 0 .OR. IEDSW == 1) THEN
        CX=COS(TH(NN))
        SAN=SIN(TH(NN))
!
      ELSEIF   ((IEDSW == 3 .AND. ITEQV(MAXN) == 2)                     &
     & .OR. IEDSW == 4 .OR. IEDSW == 2) THEN
!ipk aug96 redefine princ. direction when modeling salinityy only
!C      IF(ISLP == 1 .AND. (IVLP == 0 .AND. IHLP == 0)) THEN
!     Find a direction based on velocities at the corner nodes
        DIRX=0.
        DIRY=0.
        DO K=1,NCN
         IF(IL(K,ILK) == 0) THEN
!     Normalize velocity vector length
            VNORM=SQRT(VEL(1,NOP(NN,K))**2 + VEL(2,NOP(NN,K))**2)
            IF(VNORM > 0.) THEN
              DIRX=DIRX+VEL(1,NOP(NN,K))/VNORM
              DIRY=DIRY+VEL(2,NOP(NN,K))/VNORM
            ENDIF
          ENDIF
      ENDDO
      IF(DIRX /= 0. .AND. DIRY /= 0.) THEN
        CX=DIRX/SQRT(DIRX**2+DIRY**2)
        SAN=DIRY/SQRT(DIRX**2+DIRY**2)
          IF(IEDSW == 4 .OR. IEDSW == 2)                                &
     &    TH(NN)=ATAN2(SAN,CX)
      ELSE
          CX=COS(TH(NN))
          SAN=SIN(TH(NN))
      ENDIF
      ELSE
        CX=COS(TH(NN))
        SAN=SIN(TH(NN))
      ENDIF
!IPK AUG96      CX=COS(TH(NN))
!IPK AUG96      SAN=SIN(TH(NN))
!ipk sep96 move up this computation
!-
!-.....COMPUTE LOCAL CORDS.....
!-
      NR=NOP(NN,1)
      DO  K = 2, NCN
        N=NOP(NN,K)
        DX=CORD(N,1)-CORD(NR,1)
        DY=CORD(N,2)-CORD(NR,2)
        XL(K)=DX*CX+DY*SAN
        YL(K)=-DX*SAN+DY*CX
        ZL(K)=CORD(N,3)-CORD(NR,3)
      ENDDO
!
      IF(NTX == 0) GO TO 79
!
!
!
!IPK      EPSX = EEXXYY(1,NN)/ROAVG
!IPK      EPSXY = EEXXYY(2,NN)/ROAVG
!IPK      EPSYX = EEXXYY(3,NN)/ROAVG
!IPK      EPSY = EEXXYY(4,NN)/ROAVG
!IPK SEP96 ADD TESTS FOR IEDSW
      IF(IEDSW /= 2 .AND. IEDSW /= 4) THEN
!       write(75,*) 'iedsw',nn,iedsw
        EPSX = EEXXYY(1,NN)
        EPSXY = EEXXYY(2,NN)
        EPSYX = EEXXYY(3,NN)
        EPSY = EEXXYY(4,NN)
!IPK JAN97      ELSEIF(IEDSW == 4) THEN
      ELSE
! get element lengths in the projected direction
        PROJL(1)=0.
        DO N=2,NCN
!ipk oct96 correct PROJL          PROJL(N)=XL(N)*CX + YL(N)*SAN
          PROJL(N)=XL(N)
        ENDDO
        XLMAX=0.
        XLMIN=0.
        DO N=2,NCN
          IF(PROJL(N) > XLMAX) XLMAX=PROJL(N)
          IF(PROJL(N) < XLMIN) XLMIN=PROJL(N)
        ENDDO
!       write(75,*) "cvfct", xlmax,xlmin,cvfct,mt,ort(mt,1)
        EPSX=ABS(ORT(MT,1)*(XLMAX-XLMIN))*CVFCT
        EPSXY=EPSX*ABS(ORT(MT,2))
        EPSYX=EPSXY
        EPSY=EPSXY
      ENDIF
!
!cc      EPSX = ORT(MT,1)
!cc      EPSXY = ORT(MT,2)
!cc      EPSYX = ORT(MT,3)
!cc      EPSY = ORT(MT,4)
!IPK      DIFX = ORT(MT,8)
!IPK      DIFY = ORT(MT,9)
!ipk sep96 rewrite logic for DIFX and DIFY
      IF(IEDSW < 2) THEN
        DIFX = EEXXYY(5,NN)
        DIFY = EEXXYY(6,NN)
      ELSEIF(IEDSW == 3) THEN
        DIFX = EEXXYY(5,NN)
        DIFY = DIFX*ABS(ORT(MT,9))
!IPK JAN97      ELSEIF(IEDSW == 4) THEN
      ELSEIF(IEDSW == 4 .OR. IEDSW == 2) THEN
        DIFX = ABS(ORT(MT,8)*(XLMAX-XLMIN))/CVF2
        DIFY = DIFX*ABS(ORT(MT,9))
      ENDIF
!-
!- INITIALIZE MATRICES AND VARIABLES
!-
      DO 72 J=1,NCN
        G(J)=0.
!AUG93IPK        M=NOP(NN,J)
!AUG93IPK        PDN(J)=PRESS(M)-DEN(M)*GRAV*VEL(3,M)*(ELEV-CORD(M,3))/
!AUG93IPK     +         (ELEV-AO(M))
!AUG93IPK        DPDNR(J)=-GRAV*VEL(3,M)*(ELEV-CORD(M,3))/(ELEV-AO(M))
      DO 72 I=1,NCN
   72 ESTIFL(I,J)=0.0
      NEF=NCN*NDF
      DO 75 J=1,NEF
      F(J)=0.
      DO 75 I=1,NEF
      ESTIFM(I,J) = 0.0
   75 CONTINUE
!-
!...... Check for any active equations
!-
      DO 78 I=1,NCN
        M=NOP(NN,I)
        DO 77 J=1,NDF
          IF(NBC(M,J) > 0) GO TO 79
   77   CONTINUE
   78 CONTINUE
      RETURN
   79 CONTINUE
!IPK APR 01 SET INITIAL VALUES AFTER KICKOUT FOR DROPOUT
      AREA(NN)=0.
      TVOL(NN)=0.
!
!-
!- COMPUTE DRO  DRODX VALUES
!-
!ipk oct96 now skip ROIN
!ipk      IF(NTX == 1) CALL ROIN(NN)
!-
!-.....COPY PROPER WEIGHTING FUNCTIONS.....
!-
!      IF(NCNO == NCN .AND. NGPO == NGP) GO TO 95
      IF(NGP < 64) GO TO 89
      CALL SB(NCN,NGP)
      DO 85 M=1,NGP
      WAITX(M)=WAITR(M)*8.
      IF(NCN == 15) WAITX(M)=WAITPH(M)*2.
   85 CONTINUE
      GO TO 95
   89 CONTINUE
      DO 90 M=1,NGP
      WAITX(M)=WAITQ(M)/5832.*8.
      IF( NCN == 15 ) WAITX(M) = WAITP(M)
      IF( NCN == 10 ) WAITX(M) = WAITT(M)
      IF( NCN == 13 ) WAITX(M) = WAITS(M)
!-
!...... This is using 8 point integration
!-
      IF(NGP == 8) WAITX(M)=1.0
   90 CONTINUE
      CALL SA(NCN,NGP)
   95 CONTINUE
!-
!- COMPUTE INTERMEDIATE COEFS. AND ELEMENT TRANSPORT MATRIX
!-
      DO 500 I=1,NGP
!-
!-.....FORM THE JACOBIAN.....
!-
      J11 = 0.0
      J12 = 0.0
      J13=0.
      J21 = 0.0
      J22 = 0.0
      J23=0.
      J31=0.
      J32=0.
      J33=0.
      DO 130 K = 2, NCN
      J11 = J11 + DA(K,I) * XL(K)
      J12 = J12 + DA(K,I) * YL(K)
      J13=J13+DA(K,I)*ZL(K)
      J21 = J21 + DB(K,I) * XL(K)
      J22 = J22 + DB(K,I) * YL(K)
      J23=J23+DB(K,I)*ZL(K)
      J31=J31+DC(K,I)*XL(K)
      J32=J32+DC(K,I)*YL(K)
      J33=J33+DC(K,I)*ZL(K)
  130 CONTINUE
      A11= J22*J33-J23*J32
      A12=-J12*J33+J13*J32
      A13=J12*J23-J13*J22
      A21=-J21*J33+J23*J31
      A22=J11*J33-J13*J31
      A23=-J11*J23+J13*J21
      A31=J21*J32-J22*J31
      A32=-J11*J32+J12*J31
      A33=J11*J22-J12*J21
      DETJ=J11*A11+J12*A21+J13*A31
      DO 135 J = 1, NCN
      XN(J) = XNX(J,I)
      XO(J)=XN(J)
      DNX(J)=(A11*DA(J,I)+A12*DB(J,I)+A13*DC(J,I))/DETJ
      DOX(J)=DNX(J)
      DNY(J)=(A21*DA(J,I)+A22*DB(J,I)+A23*DC(J,I))/DETJ
      DOY(J)=DNY(J)
      DNZ(J)=(A31*DA(J,I)+A32*DB(J,I)+A33*DC(J,I))/DETJ
      DOZ(J)=DNZ(J)
  135 CONTINUE
      AMW = WAITX(I) * DETJ
      AREA(NN)=AREA(NN)+AMW
!IP NOV97      IF(NTX == 0) GO TO 500
!-
!-     REPEAT FOR LINEAR FUNCTION
!-
      DO 145 J=1,NCN
        XM(J)=0.
        DMX(J)=0.
        DMY(J)=0.
        DMZ(J)=0.
  145 CONTINUE
!-
      DO 160 J=1,NCN
        IF(IL(J,ILK) /= 0) THEN
          ML=IL(J,ILK)
          MH=IH(J,ILK)
          XM(ML)=XM(ML)+XN(J)/2.
          XM(MH)=XM(MH)+XN(J)/2.
          DMX(ML)=DMX(ML)+DNX(J)/2.
          DMX(MH)=DMX(MH)+DNX(J)/2.
          DMY(ML)=DMY(ML)+DNY(J)/2.
          DMY(MH)=DMY(MH)+DNY(J)/2.
          DMZ(ML)=DMZ(ML)+DNZ(J)/2.
          DMZ(MH)=DMZ(MH)+DNZ(J)/2.
!-
!...... Insert straight side shape function
!-
          IF(NSTRT(NOP(NN,J),1) /= 0) THEN
            XO(ML)=XO(ML)+XN(J)/2.
            XO(MH)=XO(MH)+XN(J)/2.
            DOX(ML)=DOX(ML)+DNX(J)/2.
            DOX(MH)=DOX(MH)+DNX(J)/2.
            DOY(ML)=DOY(ML)+DNY(J)/2.
            DOY(MH)=DOY(MH)+DNY(J)/2.
            DOZ(ML)=DOZ(ML)+DNZ(J)/2.
            DOZ(MH)=DOZ(MH)+DNZ(J)/2.
          ENDIF
        ELSE
          XM(J)=XM(J)+XN(J)
          DMX(J)=DMX(J)+DNX(J)
          DMY(J)=DMY(J)+DNY(J)
          DMZ(J)=DMZ(J)+DNZ(J)
        ENDIF
  160 CONTINUE
!IPK NOV97
      IF(NTX == 0) GO TO 273
      IF(IOPTZD == 1) THEN
        EPSXZ=0.
        EPSYZ=0.
        DIFZ=0.
        DO 269 M=1,NCN
          MR=NOP(NN,M)
          XNA=XN(M)
          EPSXZ=EPSXZ+XNA*EVISXZ(MR)
          EPSYZ=EPSYZ+XNA*EVISYZ(MR)
          DIFZ=DIFZ+XNA*DVISZ(MR)
!tmp          EPSXZ=EPSXZ+XNA*LOG10(EVISXZ(MR))
!tmp          EPSYZ=EPSYZ+XNA*LOG10(EVISYZ(MR))
!tmp          DIFZ=DIFZ+XNA*LOG10(DVISZ(MR))
  269   CONTINUE
!tmp          EPSXZ=10**EPSXZ
!tmp          EPSYZ=10**EPSYZ
!tmp          DIFZ=10**DIFZ
!ipk oct96 enditempchanges
      ENDIF
!-
!.....COMPUTE R, S, H AND THEIR DERIVATIVES.....
!-
      U = 0.0
      V = 0.0
      W=0.0
      Z = 0.0
      S = 0.0
      DSDX = 0.0
      DSDY = 0.0
      DSDZ = 0.0
      DSDT = 0.0
      DUDX = 0.0
      DUDY = 0.0
      DUDZ = 0.0
      DVDX = 0.0
      DVDY = 0.0
      DVDZ = 0.0
      BETA1 = 0.0
      BETA2 = 0.0
      DO 270 M=1,NCN
      MR=NOP(NN,M)
      XNA=XN(M)
      U=U+XNA*(VEL(1,MR)*CX+VEL(2,MR)*SAN)
      V=V+XNA*(-VEL(1,MR)*SAN+VEL(2,MR)*CX)
      W=W+XNA*VVEL(MR)
      Z=Z+XNA*CORD(MR,3)
      DUDX=DUDX+DNX(M)*(VEL(1,MR)*CX+VEL(2,MR)*SAN)
      DUDY=DUDY+DNY(M)*(VEL(1,MR)*CX+VEL(2,MR)*SAN)
      DUDZ=DUDZ+DNZ(M)*(VEL(1,MR)*CX+VEL(2,MR)*SAN)
      DVDX=DVDX+DNX(M)*(-VEL(1,MR)*SAN+VEL(2,MR)*CX)
      DVDY=DVDY+DNY(M)*(-VEL(1,MR)*SAN+VEL(2,MR)*CX)
      DVDZ=DVDZ+DNZ(M)*(-VEL(1,MR)*SAN+VEL(2,MR)*CX)
      IF(NSTRT(MR,1) == 0) THEN
        S=S+XO(M)*VEL(ICK,MR)
        DSDX=DSDX+DOX(M)*VEL(ICK,MR)
        DSDY=DSDY+DOY(M)*VEL(ICK,MR)
        DSDZ=DSDZ+DOZ(M)*VEL(ICK,MR)
        DSDT=DSDT+XO(M)*VDOT(ICK,MR)
      ENDIF
      IF(ICYC < 1) GO TO 270
      BETA1=BETA1+XNA*(VDOT(1,MR)*CX+VDOT(2,MR)*SAN)
      BETA2=BETA2+XNA*(-VDOT(1,MR)*SAN+VDOT(2,MR)*CX)
  270 CONTINUE
      IF(IOPTZD == 2) THEN
        EPSXZ=0.
        EPSYZ=0.
        DIFZ=0.
        DO 272 M=1,NCN
          IF(XM(M) == 0.) GO TO 272
          MR=NOP(NN,M)
          XNA=XM(M)
          EPSXZ=EPSXZ+XNA*EVISXZ(MR)
          EPSYZ=EPSYZ+XNA*EVISYZ(MR)
          DIFZ=DIFZ+XNA*DVISZ(MR)
  272   CONTINUE
      ENDIF
!
!IPK MAR01  ALLOW FOR DRAG
      VECQ=SQRT(U**2+V**2+W**2)
      IF(VECQ > 1.E-6) THEN
        TDRAGX=GRAV*DRAGX(IMMT)/VECQ
        TDRAGY=GRAV*DRAGY(IMMT)/VECQ
      ELSE
        TDRAGX=0.
        TDRAGY=0.
      ENDIF
!
!IPK NOV97
  273 CONTINUE
      H = 0.0
      DHDX = 0.0
      DHDY = 0.0
      A0=0.0
      DAODX = 0.0
      DAODY = 0.0
      RHO = 0.0
!AUG93IPK      DRODX = 0.0
!AUG93IPK      DRODY = 0.0
      DRODZ = 0.0
!AUG93IPK      P = 0.0
!AUG93IPK      DPDX = 0.0
!AUG93IPk      DPDY = 0.0
!AUG93IPK      PD=0.0
!AUG93IPK  DPDR = 0.0
!IPK OCT98      SIGMAX = 0.0
!IPK OCT98      SIGMAY = 0.0
      BETA3 = 0.0
!IPK OCT98      GAMMA = 0.0
      DRDXIN=0.0
      DRDYIN=0.0
      RHOS=0.0
      DRODXS=0.0
      DRODYS=0.0
!ipk oct96 set MM
      MM=0
      DO 275 M=1,NCN
      IF(XM(M) == 0.) GO TO 275
!ipk oct96 increment MM
      MM=MM+1
      MR=NOP(NN,M)
      A0=A0+XM(M)*AO(MR)
      H = H + XM(M)*VEL(3,MR)
      RHO=RHO+XM(M)*DEN(MR)
!AUG93IPK      P=P+XM(M)*PRESS(MR)
!AUG93IPK      PD=PD+XM(M)*PDN(M)
!AUG93IPK      DPDR=DPDR+XM(M)*DPDNR(M)
      BETA3=BETA3+XM(M)*VDOT(3,MR)
!AUG93IPK      DRODX=DRODX+DMX(M)*DEN(MR)
!AUG93IPK      DRODY=DRODY+DMY(M)*DEN(MR)
      DRODZ=DRODZ+DMZ(M)*DEN(MR)
      DHDX = DHDX + DMX(M)*VEL(3,MR)
      DHDY = DHDY + DMY(M)*VEL(3,MR)
      DAODX = DAODX + DMX(M)*AO(MR)
      DAODY = DAODY + DMY(M)*AO(MR)
!ipk oct96      DRDXIN=DRDXIN+XM(M)*DROXIN(MR)
!ipk oct96      DRDYIN=DRDYIN+XM(M)*DROYIN(MR)
!ipk oct96 Use previously computed DROXINS
      DRDXIN=DRDXIN+XM(M)*DROXINS(NN,MM)
      DRDYIN=DRDYIN+XM(M)*DROYINS(NN,MM)
      IF(MR > NPM) MR=NSURF(MR)
      RHOS=RHOS+XM(M)*DEN(MR)
      DRODXS=DRODXS+DMX(M)*DEN(MR)
      DRODYS=DRODYS+DMY(M)*DEN(MR)
  275 CONTINUE
!
      if(h <= 0. .AND. ntx /= 0) then
      write(*,*) 'negative h for element',nn,i,h
      write(75,*) 'negative h for element',nn,i,h
      do n=1,ncn
        write(75,*) 'node',nop(nn,n),vel(3,nop(nn,n))
        write(*,*) 'node',nop(nn,n),vel(3,nop(nn,n))
      enddo
      stop
      endif
!
!ipk nov96 transform to cx,san
      TEMP1=DRDXIN*CX+DRDYIN*SAN
      DRDYIN=-DRDXIN*SAN+DRDYIN*CX
      DRDXIN=TEMP1
!
!     Gaussian based Mellor-Yamada
!
!IPK NOV97
      IF(IOPTZD == 3 .AND. NTX == 1) THEN
        CALL NELLII(DRODZ,DUDZ,DVDZ,H,A0,Z,EPSXZ,EPSYZ,DIFZ,RHO,        &
     &              ELEV,GRAV)
        IF(I == 1) THEN
          WRITE(75,*) 'NELLI',NN,EPSXZ
        ENDIF
      ENDIF
      AMR=RHO*AMW
      XHT=ELEV-A0
!IPK NOV97
      TVOL(NN)=TVOL(NN)+AMW/XHT
      IF(NTX == 0) GO TO 500
      ZIN=(Z-A0)/XHT
!     IF(XHT == 0.) WRITE(*,*) NN,ELEV,Z,XHT,ZA
      ZAT=(ELEV-Z)
      ZAM=1./XHT
      za=zat*zam
!IPK SEP95 ADD DEFINITION FOR DGP AND DF1
!
      DGP=ZA*H
!
!    Scale diffusion for near surface elements
!ipk sep96 make dmix input
!ipk sep96      IF(GRAV > 30.) THEN
!ipk sep96        DMIX= 3.*3.2808
!ipk sep96      ELSE
!ipk sep96        DMIX=0.5
!ipk sep96      ENDIF
      IF(DGP < DMIX) THEN
        DF1=(1.+ (DMIX-DGP)/DMIX*32.)
        IF(DFACT > 1.) THEN
          IF(DF1 > DFACT) DFACT=DF1
        ELSE
          DFACT=DF1*DFACT
        ENDIF
      ENDIF
!
!      PD=P-RHO*GRAV*H*ZA
      IF(IOPTZD == 0) THEN
!ipk oct96 change functional rep of edd1 etc
!       EPSXZ=ORT(MT,6)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT
!       EPSYZ=ORT(MT,7)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT
        EPSXZ=ORT(MT,6)*(EDD1(mt)+ZIN*(EDD2(mt)+ZIN*EDD3(mt)))*DFACT*XHT
        EPSYZ=ORT(MT,7)*(EDD1(mt)+ZIN*(EDD2(mt)+ZIN*EDD3(mt)))*DFACT*XHT
        DIFZ=ORT(MT,10)*DFACT*XHT
! the next three lines represent values when not scaled by h
!       changed by IPK instructions 27.8.92
!        EPSXZ=ORT(MT,6)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT/H
!        EPSYZ=ORT(MT,7)*(EDD1+ZIN*(EDD2+ZIN*EDD3))*DFACT*XHT/H
!        DIFZ=ORT(MT,10)*DFACT*XHT/H
      ELSE
        IF(DIFZ < 0.)DIFZ=0.
        IF(EPSXZ < 0.)EPSXZ=0.
        IF(EPSYZ < 0.)EPSYZ=0.
        EPSXZ=(ORT(MT,6)+EPSXZ)*XHT/H
        EPSYZ=(ORT(MT,7)+EPSYZ)*XHT/H
        DIFZ=(ORT(MT,10)+DIFZ)*XHT/H
      ENDIF
      A1=XHT*DAODX
      A2=XHT*DAODY
      B1=-ELEV/XHT*DAODX
      B2=-ELEV/XHT*DAODY
      D1=DAODX/XHT
      D2=DAODY/XHT
      TX=A1+B1*H-A0*DHDX+(D1*H+DHDX)*Z
      TY=A2+B2*H-A0*DHDY+(D2*H+DHDY)*Z
      IF(NTX == 2) GO TO 430
      WUT=W*XHT-U*TX-V*TY-(Z-A0)*BETA3
      IF(ICK == 4) THEN
        DRDS=DRODS(S,IGF)
!IPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ELSEIF(ICK == 5) THEN
        DRDS=DRODTM(S,IGF)
!IPK AUG95 GET RATES
        IF(ICYC > 0) THEN
          CALL MKTEMP(S,H,DGP,SRCSNK,GRATE,DELT,MT,NETYP(NN))
!c          IF(NN == 1) WRITE(76,*)'MKTEMP',GRATE,SRCSNK,GRATE*S+SRCSNK
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
!
!.....EVALUATE THE BASIC EQUATIONS WITH PRESENT VALUES.....
!
      IF(IVLP == 1) THEN
      FRN = 0.0
      FSN = 0.0
      IF( ICYC <= 0 ) GO TO 279
      FRN=AMR*BETA1*H
      FSN=AMR*BETA2*H
!
!.....MOMENTUM VISCOUS AND PRESSURE TERMS
!
  279 CONTINUE
!MAY93 FRN = FRN +AMR*(H*(U*DUDX+V*DUDY)+WUT*DUDZ)+AMW*(H*RHOS*DAODX*GRAV
!MAY931 +EPSX*DHDX*DUDX+EPSXY*DHDY*DUDY+GRAV*DRDXIN*H
!MAY932 -GRAV*H**2/2.*DRODXS)
!ipk jan97      FRN = FRN +AMR*(H*(U*DUDX+V*DUDY)+WUT*DUDZ)+AMW*(H*RHOS*DAODX*GRAV
!ipk jan97     1 -EPSX*DAODX*DUDX*H/XHT-EPSXY*DAODY*DUDY*H/XHT+GRAV*DRDXIN*H
      FRN = FRN +AMR*(H*(U*DUDX+V*DUDY)+WUT*DUDZ)+AMW*(H*RHOS*DAODX*GRAV&
     & +GRAV*DRDXIN*H                                                   &
     & -GRAV*H**2/2.*DRODXS)                                            &
     & +AMR*U*SIDF(NN)
!IPK NOV97 ADD SIDEFLOW
!
!MAY93FSN = FSN +AMR*(H*(U*DVDX+V*DVDY)+WUT*DVDZ)+AMW*(H*RHOS*DAODY*GRAV
!MAY931 +EPSYX*DHDX*DVDX+EPSY*DHDY*DVDY+GRAV*DRDYIN*H
!MAY932 -GRAV*H**2/2.*DRODYS)
!ipk jan97      FSN = FSN +AMR*(H*(U*DVDX+V*DVDY)+WUT*DVDZ)+AMW*(H*RHOS*DAODY*GRAV
!ipk jan97     1 -EPSYX*DAODX*DVDX*H/XHT-EPSY*DAODY*DVDY*H/XHT+GRAV*DRDYIN*H
      FSN = FSN +AMR*(H*(U*DVDX+V*DVDY)+WUT*DVDZ)+AMW*(H*RHOS*DAODY*GRAV&
     & +GRAV*DRDYIN*H                                                   &
     & -GRAV*H**2/2.*DRODYS)                                            &
     & +AMR*V*SIDF(NN)
!IPK NOV97 ADD SIDEFLOW
!
      FRNX=AMW*H*(EPSX*DUDX-RHOS*GRAV*H/2.)
      FRNY=AMW*EPSXY*DUDY*H
      FRNZ=AMW*EPSXZ*DUDZ*XHT
      FSNX=AMW*EPSYX*DVDX*H
      FSNY=AMW*H*(EPSY*DVDY-RHOS*GRAV*H/2.)
      FSNZ=AMW*EPSYZ*DVDZ*XHT
!
!IPK MAR01  ADD DRAG
!
      FRN = FRN + AMR*GRAV*H*DRAGX(IMMT)*U*VECQ
      FSN = FSN + AMR*GRAV*H*DRAGY(IMMT)*V*VECQ
!-
!......CORIOLIS TERMS
!-
!      FRN = FRN - OMEGA*V*AMR
!      FSN = FSN + OMEGA*U*AMR
      FRN = FRN - OMEGA*V*AMR*h
      FSN = FSN + OMEGA*U*AMR*h
!
!.....MOTION EQUATIONS.....
!
      DO 285 M = 1, NCN
      IA = 1 + NDF*(M-1)
      F(IA) = F(IA) - XN(M)*FRN - DNX(M)*FRNX - DNY(M)*FRNY             &
     & - DNZ(M)*FRNZ
      IA = IA + 1
      F(IA) = F(IA) - XN(M)*FSN - DNX(M)*FSNX - DNY(M)*FSNY             &
     & - DNZ(M)*FSNZ
  285 CONTINUE
      ENDIF
!
!.....CONTINUITY EQUATION.....
!
      IF(IHLP == 1) THEN
      FRNC=AMW*((DUDX+DVDY)*H-DUDZ*TX-DVDZ*TY)/XHT                      &
     &     -AMW*SIDF(NN)/XHT
!ipk nov97 add term for sidf
      DO 290 M=1,NCN
      IF(XM(M) == 0.) GO TO 290
      IA = NDF*(M-1)+3
      F(IA) = F(IA) - XM(M)*FRNC
  290 CONTINUE
      ENDIF
!-
!......THE SALINITY EQUATION
!-
      IF(ISLP == 1) THEN
      FRNX=AMW*DIFX*DSDX*H
      FRNY=AMW*DIFY*DSDY*H
      FRNZ=AMW*DIFZ*DSDZ*XHT
!ipk jan97      FRN=AMW*(H*(U*DSDX+V*DSDY)+WUT*DSDZ+DIFX*DHDX*DSDX+DIFY*DHDY*DSDY)
      FRN=AMW*(H*(U*DSDX+V*DSDY)+WUT*DSDZ)                              &
     &    -AMW*H*(SRCSNK+GRATE*S)                                       &
     &    -AMW*SIDF(NN)*(SIDQ(NN,ICK-3)-S)
      IF(SIDF(NN) == 0. .AND. SIDQ(NN,ICK-3) > 0.) THEN
        FRN=FRN-AMW*SIDQ(NN,ICK-3)
      ENDIF
!IPK NOV97 ADD SIDE FLOW TERMS IN LINE ABOVE
!IPK AUG95 ADD RATE TERM ABOVE
      IF( ICYC > 0) FRN=FRN+AMW*DSDT*H
      IA=0
      DO 295 M=1,NCN
      IA=IA+NDF
      IF(NSTRT(NOP(NN,M),1) == 0) THEN
        F(IA)=F(IA)-(XO(M)*FRN+DOX(M)*FRNX+DOY(M)*FRNY+DOZ(M)*FRNZ)/xht
!ipk orig        F(IA)=F(IA)-XO(M)*FRN-DOX(M)*FRNX-DOY(M)*FRNY-DOZ(M)*FRNZ
      ENDIF
  295 CONTINUE
      ENDIF
!
!.....FORM THE X MOTION EQUATIONS.....
!
      IF(IVLP == 1) THEN
!
!IPK MAR01  ADD DRAG
!
      T1=AMR*(H*DUDX-TX*DUDZ)                                           &
     &  +AMR*(SIDF(NN)+TDRAGX*H*(2.*U**2+V**2+W**2))
!IPK NOV97 ADD SIDEFLOW
!MAY93      T2=AMW*(RHO*U*H+DHDX*EPSX)
!MAY93      T3=AMW*(RHO*V*H+DHDY*EPSXY)
!ipk jan97      T2=AMW*(RHO*U*H-DAODX*EPSX*H/XHT)
!ipk jan97      T3=AMW*(RHO*V*H-DAODY*EPSXY*H/XHT)
      T2=AMW*RHO*U*H
      T3=AMW*RHO*V*H
      T8=AMR*WUT
!IPK MAR01 ADD DRAG
      T4=AMR*(H*(DUDY-OMEGA)-TY*DUDZ+ TDRAGX*H*U*V)
      T5=AMW*EPSX*H
      T6=AMW*EPSXY*H
      T7=AMW*EPSXZ*XHT
      IB=1-NDF
      DO 310 N=1,NCN
      IB=IB+NDF
      FEEAN=XN(N)*T1+DNX(N)*T2+DNY(N)*T3+DNZ(N)*T8
      FEEDN=XN(N)*T4
      FEEBN=T5*DNX(N)
      FEECN=T6*DNY(N)
      FEEEN=T7*DNZ(N)
!-
!-.....FORM THE TIME TERMS.....
!-
      IF( ICYC == 0 ) GO TO 304
      FEEAN=FEEAN+AMR*XN(N)*ALTM*H
  304 CONTINUE
      IA=1-NDF
      DO 305 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN        &
     &  + DNY(M)*FEECN + DNZ(M)*FEEEN
      ESTIFM(IA,IB+1) = ESTIFM(IA,IB+1) + XN(M)*FEEDN
  305 CONTINUE
  310 CONTINUE
!
!.....FORM THE HEAD TERMS.....
!
      IF(IHLP == 1) THEN
!MAY93      T1=AMR*(-omega*v+U*DUDX+V*DUDY-DUDZ*(U*(B1+D1*Z)+V*(B2+D2*Z)))
!MAY93     +  +AMW*(GRAV*(RHOS*DAODX+DRDXIN))
!MAY93     +  -AMW*GRAV*H*DRODXS
!MAY93      T2=AMW*(EPSX*DUDX-U*DUDZ*(Z-A0)*RHO)
!MAY93      T3=AMW*(EPSXY*DUDY-V*DUDZ*(Z-A0)*RHO)
!IPK MAR01 ADD DRAG      T1=AMR*(-omega*v+U*DUDX+V*DUDY-DUDZ*(U*(B1+D1*Z)+V*(B2+D2*Z)))
      T1=AMR*(-omega*v+U*DUDX+V*DUDY-DUDZ*(U*(B1+D1*Z)+V*(B2+D2*Z))     &
     &    + GRAV*DRAGX(IMMT)*U*VECQ)                                    &
     &  +AMW*(GRAV*(RHOS*DAODX+DRDXIN))                                 &
     &  -AMW*GRAV*H*DRODXS
!ipk jan97     +  +AMW*(-GRAV*H*DRODXS-DAODX*EPSX*DUDX/XHT-DAODY*EPSXY*DUDY/XHT)
      T2=AMW*(-U*DUDZ*(Z-A0)*RHO)
      T3=AMW*(-V*DUDZ*(Z-A0)*RHO)
      T4=AMW*(EPSX*DUDX-RHOS*GRAV*H)
      T5=AMW*EPSXY*DUDY
      IB=3-NDF
      DO 325 N=1,NCN
         IB=IB+NDF
         IF(XM(N) == 0.) GO TO 325
         FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*T3
         FEEBN=XM(N)*T4
         FEECN=XM(N)*T5
!-
!-.....FORM THE TIME TERMS.....
!-
         IF( ICYC <= 0 ) GO TO 317
         FEEAN=FEEAN+AMR*XM(N)*(BETA1-ALTM*(Z-A0)*DUDZ)
  317    CONTINUE
         IA=1-NDF
         DO 320 M = 1, NCN
            IA=IA+NDF
            ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN  &
     &        + DNY(M)*FEECN
  320    CONTINUE
  325 CONTINUE
      ENDIF
!-
!.....FORM THE SALINITY TERMS
!-
      IF(ISLP == 1) THEN
!      TAA=-AMW*H*DRDS*(GRAV*H/2.+DPDR)
!      TAB=AMW*GRAV*H**2*DRDS*(ZA-0.5)
!      TAC= AMW*DRDS*(H*(GRAV*DAODX-DPDR*DHDX+U*DUDX+V*DUDY)+WUT*DUDZ)
       TAC= AMW*DRDS*(H*(U*DUDX+V*DUDY)+WUT*DUDZ)
      IF(ICYC > 0) TAC=TAC+AMW*DRDS*H*BETA1
      IB=4-NDF
      DO 330 N=1,NCN
        IB=IB+NDF
        IF(NSTRT(NOP(NN,N),1) == 0) THEN
!          FEEAN= XO(N)*TAA
!          FEEBN=DOX(N)*TAB+XO(N)*TAC
          FEEBN=XO(N)*TAC
          IA=1-NDF
          DO 329 M=1,NCN
            IA=IA+NDF
!            ESTIFM(IA,IB)=ESTIFM(IA,IB)+DNX(M)*FEEAN+XN(M)*FEEBN
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(M)*FEEBN
  329     CONTINUE
        ENDIF
  330 CONTINUE
      ENDIF
      ENDIF
!
!.....FORM THE Y MOTION EQUATIONS.....
!
!.....FLOW INERTIA AND VISCOUS TERMS.....
!
      IF(IVLP == 1) THEN
!IPK MAR01  ADD DRAG
!
      T1=AMR*(DVDY*H-TY*DVDZ)                                           &
     &   +AMR*(SIDF(NN)+TDRAGY*H*(2.*V**2+U**2+W**2))
!IPK NOV97 ADD SIDEFLOW
!
!MAY93      T2=AMW*(RHO*U*H+EPSYX*DHDX)
!MAY93      T3=AMW*(RHO*V*H+EPSY*DHDY)
!ipk jan97      T2=AMW*(RHO*U*H-EPSYX*DAODX*H/XHT)
!ipk jan97      T3=AMW*(RHO*V*H-EPSY*DAODY*H/XHT)
      T2=AMW*RHO*U*H
      T3=AMW*RHO*V*H
      T8=AMR*WUT
!IPK MAR01 ADD DRAG
      T4=AMR*(h*(DVDX+OMEGA)-TX*DVDZ+ TDRAGY*H*U*V)
      T5=AMW*EPSYX*H
      T6=AMW*EPSY*H
      T7=AMW*EPSYZ*XHT
      IB=1-NDF
      DO 340 N=1,NCN
      IB=IB+NDF
!
!.....INERTIAL COMPONENTS.....
!
      FEEAN=XN(N)*T1+DNX(N)*T2+DNY(N)*T3+DNZ(N)*T8
      FEEDN=XN(N)*T4
      FEEBN=DNX(N)*T5
      FEECN=DNY(N)*T6
      FEEEN=DNZ(N)*T7
!-
!-.....FORM THE TIME TERMS.....
!-
      IF( ICYC <= 0 ) GO TO 334
      FEEAN=FEEAN+AMR*XN(N)*ALTM*H
  334 CONTINUE
      IA=2-NDF
      DO 335 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEDN
      ESTIFM(IA,IB+1) = ESTIFM(IA,IB+1) + XN(M)*FEEAN + DNX(M)*FEEBN    &
     &  + DNY(M)*FEECN + DNZ(M)*FEEEN
  335 CONTINUE
  340 CONTINUE
!
!.....HEAD TERMS.....
!
      IF(IHLP == 1) THEN
!MAY93      T1=AMR*(omega*u+U*DVDX+V*DVDY-DVDZ*(U*(B1+D1*Z)+V*(B2+D2*Z)))
!MAY93     +  +AMW*(GRAV*(RHOS*DAODY+DRDYIN)
!MAY93     +  -H*GRAV*DRODYS)
!MAY93      T2=AMW*(EPSYX*DVDX-U*DVDZ*RHO*(Z-A0))
!MAY93      T3=AMW*(EPSY*DVDY-V*DVDZ*RHO*(Z-A0))
!IPK MAR01 ADD DRAG      T1=AMR*(omega*u+U*DVDX+V*DVDY-DVDZ*(U*(B1+D1*Z)+V*(B2+D2*Z)))
      T1=AMR*(omega*u+U*DVDX+V*DVDY-DVDZ*(U*(B1+D1*Z)+V*(B2+D2*Z))      &
     &    + GRAV*DRAGY(IMMT)*V*VECQ)                                    &
     &  +AMW*(GRAV*(RHOS*DAODY+DRDYIN)                                  &
     &  -H*GRAV*DRODYS)
!ipk jan97     +  -H*GRAV*DRODYS-EPSYX*DAODX*DVDX/XHT-EPSY*DAODY*DVDY/XHT)
      T2=AMW*(-U*DVDZ*RHO*(Z-A0))
      T3=AMW*(-V*DVDZ*RHO*(Z-A0))
      T4=AMW*EPSYX*DVDX
      T5=AMW*(EPSY*DVDY-RHOS*GRAV*H)
      IB=3-NDF
      DO 355 N=1,NCN
         IB=IB+NDF
         IF(XM(N) == 0.) GO TO 355
         FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*T3
         FEEBN=XM(N)*T4
         FEECN=XM(N)*T5
!-
!-.....FORM THE TIME TERMS
!-
         IF(ICYC <= 0) GO TO 347
!ipkjan95 remove rho      FEEAN=FEEAN+AMR*XM(N)*(BETA2-ALTM*(Z-A0)*RHO*DVDZ)
         FEEAN=FEEAN+AMR*XM(N)*(BETA2-ALTM*(Z-A0)*DVDZ)
  347    CONTINUE
         IA=2-NDF
         DO 350 M=1,NCN
            IA=IA+NDF
            ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN  &
     &        + DNY(M)*FEECN
  350    CONTINUE
  355 CONTINUE
      ENDIF
!-
!......FORM THE SALINITY TERMS
!-
      IF(ISLP == 1) THEN
!      TAA=-AMW*H*DRDS*(GRAV*H/2.+DPDR)
!      TAB=AMW*GRAV*H**2*(ZA-0.5)*DRDS
!      TAC= AMW*DRDS*(H*DAODY*GRAV+H*U*DVDX+H*V*DVDY+WUT*DVDZ
!     +     -DPDR*DHDY)
      TAC= AMW*DRDS*(H*U*DVDX+H*V*DVDY+WUT*DVDZ)
      IF(ICYC > 0) TAC=TAC+AMW*DRDS*H*BETA2
      IB=4-NDF
      DO 359 N=1,NCN
        IB=IB+NDF
        IF(NSTRT(NOP(NN,N),1) == 0) THEN
!          FEEAN= XO(N)*TAA
!          FEEBN=DOY(N)*TAB+XO(N)*TAC
          FEEBN=XO(N)*TAC
          IA=2-NDF
          DO 357 M=1,NCN
            IA=IA+NDF
!            ESTIFM(IA,IB)=ESTIFM(IA,IB)+DNY(M)*FEEAN+XN(M)*FEEBN
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(M)*FEEBN
  357     CONTINUE
        ENDIF
  359 CONTINUE
      ENDIF
      ENDIF
!
!.....FORM THE CONTINUITY EQUATIONS.....
!
      IF(IHLP == 1) THEN
      IF(IVLP == 1) THEN
      T1=AMW*H/XHT
      T2A=AMW*TX/XHT
      T3A=AMW*TY/XHT
      IA=3-NDF
      DO 361 M=1,NCN
        IA=IA+NDF
        IF(XM(M) == 0.) GO TO 361
        FEEAN=XM(M)*T1
        FEEDN=-XM(M)*T2A
        FEEEN=-XM(M)*T3A
        IB=1-NDF
        DO 360 N = 1, NCN
          IB=IB+NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+FEEAN*DNX(N)+    FEEDN*DNZ(N)
          ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+FEEAN*DNY(N)                  &
     &                    +FEEEN*DNZ(N)
  360   CONTINUE
  361 CONTINUE
      ENDIF
      T4=AMW*(DUDX+DVDY-(B1+D1*Z)*DUDZ-(B2+D2*Z)*DVDZ)/XHT
      T5=AMW*(A0-Z)*DUDZ/XHT
      T6=AMW*(A0-Z)*DVDZ/XHT
      IA=3-NDF
      DO 365 M=1,NCN
        IA=IA+NDF
        IF(XM(M) == 0.) GO TO 365
        IB=3-NDF
        FEEAN=XM(M)*T4
        FEEBN=XM(M)*T5
        FEECN=XM(M)*T6
!       IF(ICYC > 0) FEEAN=FEEAN+XM(M)*AMW*ALTM
        DO 363 N=1,NCN
          IB=IB+NDF
          IF(XM(N) == 0.) GO TO 363
       ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*FEEAN+DMX(N)*FEEBN+DMY(N)*FEECN
  363   CONTINUE
  365 CONTINUE
      ENDIF
!-
!......FORM THE SALINITY EQUATION
!-
!......VELOCITY AND HEAD TERMS
!-
      IF(ISLP == 1) THEN
      IF(IVLP == 1) THEN
      T1=AMW*(H*DSDX-TX*DSDZ)
      T2=AMW*(H*DSDY-TY*DSDZ)
      IA=4-NDF
      DO 389 M=1,NCN
        IA=IA+NDF
        IF(NSTRT(NOP(NN,M),1) == 0) THEN
!ipk aug98
          FEEAN=XO(M)*T1/xht
          FEEBN=XO(M)*T2/xht
          IB=1-NDF
          DO 385 N=1,NCN
            IB=IB+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEAN
  385     CONTINUE
          IB=2-NDF
          DO 387 N=1,NCN
            IB=IB+NDF
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEBN
  387     CONTINUE
        ENDIF
  389 CONTINUE
      ENDIF
      IF(IHLP == 1) THEN
      T3A=AMW*DIFX*DSDX
      T4A=AMW*DIFY*DSDY
!MAY93      T3=T3A-AMW*U*DSDZ*(Z-A0)
!MAY93      T4=T4A-AMW*V*DSDZ*(Z-A0)
!MAY93      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)+V*DSDY-DSDZ*V*(B2+D2*Z))
      T3=-AMW*U*DSDZ*(Z-A0)
      T4=-AMW*V*DSDZ*(Z-A0)
      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)+V*DSDY-DSDZ*V*(B2+D2*Z))
!ipk jan97      T5=AMW*(U*DSDX-DSDZ*U*(B1+D1*Z)+V*DSDY-DSDZ*V*(B2+D2*Z)-
!ipk jan97     +  DAODX*DIFX/XHT*DSDX-DAODY*DIFY/XHT*DSDY)
      IF(ICYC > 0) THEN
        T5=T5+AMW*(DSDT-ALTM*(Z-A0)*DSDZ)
      ENDIF
      IA=4-NDF
      DO 400 M=1,NCN
        IA=IA+NDF
        IF(NSTRT(NOP(NN,M),1) == 0) THEN
!ipk aug98
          FEECN=XO(M)*T3/xht
          FEEDN=XO(M)*T4/xht
          FEEEN=(XO(M)*T5 + DOX(M)*T3A + DOY(M)*T4A)/xht
          IB=3-NDF
          DO 390 N=1,NCN
            IB=IB+NDF
            IF(XM(N) /= 0.) THEN
              ESTIFM(IA,IB)=ESTIFM(IA,IB)                               &
     &                     +DMX(N)*FEECN+DMY(N)*FEEDN+XM(N)*FEEEN
            ENDIF
  390     CONTINUE
        ENDIF
  400 CONTINUE
      ENDIF
!-
!......FORM SALINITY TERMS
!-
!IPK AUG95 REDEFINE T1 TERMS FOR RATE
      T1 = -AMW*H*GRATE                                                 &
     &      +AMW*SIDF(NN)
!IPK NOV97 ADD LINE ABOVE FOR SIDEFLOW
!      T1=0.
      IF(ICYC > 0) T1= T1 + AMW*ALTM*H
      T2=AMW*DIFX*H
      T3=AMW*DIFY*H
      T4=AMW*DIFZ*XHT
!MAY93      T5=AMW*(U*H+DIFX*DHDX)
!MAY93      T6=AMW*(V*H+DIFY*DHDY)
!ipk jan97      T5=AMW*(U*H-DIFX*DAODX*H/XHT)
!ipk jan97      T6=AMW*(V*H-DIFY*DAODY*H/XHT)
      T5=AMW*U*H
      T6=AMW*V*H
      T7=AMW*WUT
      IA=0
      DO 420 M=1,NCN
        IA=IA+4
        IF(NSTRT(NOP(NN,M),1) == 0) THEN
          FEEAN=XO(M)*T1
          FEEBN=DOX(M)*T2+XO(M)*T5
          FEECN=DOY(M)*T3+XO(M)*T6
          FEEDN=DOZ(M)*T4+XO(M)*T7
          IB=0
          DO 410 N=1,NCN
            IB=IB+4
            IF(NSTRT(NOP(NN,N),1) == 0) THEN
              ESTIFM(IA,IB)=ESTIFM(IA,IB)                               &
     &   +(FEEAN*XO(N)+FEEBN*DOX(N)+FEEDN*DOZ(N)+FEECN*DOY(N))/xht
!ipk exp      +            +FEEAN*XO(N)+FEEBN*DOX(N)+FEEDN*DOZ(N)+FEECN*DOY(N)
            ENDIF
  410     CONTINUE
        ENDIF
  420 CONTINUE
      ENDIF
      GO TO 500
!-
!......COMPUTE STIFFNESS MATRIX FOR VERTICAL VELOCITIES
!-
  430 CONTINUE
      IF(ITEQV(MAXN) /= 5) THEN
      T1=DUDX*H-TX*DUDZ
      T2=DVDY*H-TY*DVDZ
      T1=(T1+T2)*AMW
      DO 470 M=1,NCN
      F(M)=F(M)-DNZ(M)*T1
      T3=AMW*DNZ(M)*XHT
      DO 450 N=1,NCN
      ESTIFM(M,N)=ESTIFM(M,N)+T3*DNZ(N)
  450 CONTINUE
  470 CONTINUE
      ENDIF
  500 CONTINUE
!IPK OCT98      NCNO=NCN
!IPK OCT98      NGPO=NGP
      IF(NTX == 0) RETURN
      IF(NTX == 2) GO TO 1400
!-
!- APPLY TRANSFORMATIONS TO STIFFNESS AND FORCE MATRICES FOR SLOPING B. C.
!-
      DO 1000 N=1,NCN
      N1=NOP(NN,N)
      AFA=ALFA(N1)-TH(NN)-ADIF(N1)
      IF(AFA) 820,1000,820
  820 CX=COS(AFA)
      SAN=SIN(AFA)
      IB=NDF*(N-1)+1
      DO 900 M=1,NCN
      DO 900 MM=1,NDF
      IA=NDF*(M-1)+MM
      TEMP=ESTIFM(IA,IB)*CX + ESTIFM(IA,IB+1)*SAN
      ESTIFM(IA,IB+1)=-ESTIFM(IA,IB)*SAN + ESTIFM(IA,IB+1)*CX
      ESTIFM(IA,IB)=TEMP
  900 CONTINUE
      DO 990 M=1,NCN
      DO 990 MM=1,NDF
      IA=NDF*(M-1)+MM
      TEMP=ESTIFM(IB,IA)*CX + ESTIFM(IB+1,IA)*SAN
      ESTIFM(IB+1,IA)=-ESTIFM(IB,IA)*SAN + ESTIFM(IB+1,IA)*CX
      ESTIFM(IB,IA)=TEMP
  990 CONTINUE
      TEMP=CX*F(IB) + SAN*F(IB+1)
      F(IB+1)=-F(IB)*SAN + F(IB+1)*CX
      F(IB)=TEMP
 1000 CONTINUE
!
!......APPLY VERTICAL SHAPE FUNCTION FACTORS
!-
      JA=1-NDF
      DO 1010 JJ=1,NCN
      J=NOP(NN,JJ)
      JA=JA+NDF
      FT=FCTV(J)
      FS=FCTS(J)
      IF(FT == 1. .AND. FS == 1.) GO TO 1010
      IA=1-NDF
      DO 1005 II=1,NCN
      I=NOP(NN,II)
      IA=IA+NDF
      ESTIFM(IA  ,JA  )=ESTIFM(IA  ,JA  )*FT
      ESTIFM(IA  ,JA+1)=ESTIFM(IA  ,JA+1)*FT
      ESTIFM(IA  ,JA+3)=ESTIFM(IA  ,JA+3)*FS
      ESTIFM(IA+1,JA  )=ESTIFM(IA+1,JA  )*FT
      ESTIFM(IA+1,JA+1)=ESTIFM(IA+1,JA+1)*FT
      ESTIFM(IA+1,JA+3)=ESTIFM(IA+1,JA+3)*FS
      ESTIFM(IA+2,JA  )=ESTIFM(IA+2,JA  )*FT
      ESTIFM(IA+2,JA+1)=ESTIFM(IA+2,JA+1)*FT
      ESTIFM(IA+2,JA+3)=ESTIFM(IA+2,JA+3)*FS
      ESTIFM(IA+3,JA  )=ESTIFM(IA+3,JA  )*FT
      ESTIFM(IA+3,JA+1)=ESTIFM(IA+3,JA+1)*FT
      ESTIFM(IA+3,JA+3)=ESTIFM(IA+3,JA+3)*FS
 1005 CONTINUE
 1010 CONTINUE
!
!ipk feb98 apply sclae factors for elevtaion bc's if necessary
!-
!...... Apply scale factors to velocities for special boundaries
!-
      DO N=1,NCN
        M=NOP(NN,N)
        IF(VSCALE(M) /= 0.) THEN
          NEQ=NDF*NCN
          IA=NDF*(N-1)+1
          DO I=1,NEQ
            ESTIFM(I,IA)=ESTIFM(I,IA)*VSCALE(M)
          ENDDO
        ENDIF
      ENDDO
!ipk feb98 end changes
!-
!...... For 2D - 3D junctions adjust equation for direction
!-
      DO 1045 N=1,NCN
        IF(IH(N,ILK) == 0) THEN
          M=NOP(NN,N)
          IF(ADIF(M) /= 0.) THEN
            IA=NDF*(N-1)+1
            DO 1040 I=1,NEF
               ESTIFM(I,IA)=ESTIFM(I,IA)+ESTIFM(I,IA+1)*SIN(ADIF(M))    &
     &                     /COS(ADIF(M))
 1040       CONTINUE
          ENDIF
        ENDIF
 1045 CONTINUE
      DO 1300 N=1,NCN
      M=NOP(NN,N)
      IF(NFIX(M) < 13000) GO TO 1300
      IRW=(N-1)*NDF+1
      IRH=IRW+2
      IF(NFIX(M) <= 13010) IRW=IRW+1
!      VX=SQRT(VEL(1,M)**2+VEL(2,M)**2)
!      IF(NFIX(M) >= 31000)
!     1VX=SIGN(VX,VEL(1,M))
!      IF(NFIX(M) <= 13010)
!     1VX=SIGN(VX,VEL(2,M))
      VX=VEL(1,M)*COS(ALFA(M))+VEL(2,M)*SIN(ALFA(M))
      DO 1200 J=1,NEF
 1200 ESTIFM(IRW,J)=0.
      N1=IL(N,ILK)
      IF(N1 /= 0) GO TO 1250
      ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)
      ESTIFM(IRW,IRH)=AREA(NN)*VX
      F(IRW)=AREA(NN)*(SPEC(M,1)-VX*VEL(3,M))
      GO TO 1300
 1250 N2=IH(N,ILK)
      N4=NOP(NN,N1)
      N3=NOP(NN,N2)
      ESTIFM(IRW,IRW)=AREA(NN)*(VEL(3,N4)+VEL(3,N3))/2.
      ESTIFM(IRW,NDF*(N1-1)+3)=AREA(NN)*VX/2.
      ESTIFM(IRW,NDF*(N2-1)+3)=AREA(NN)*VX/2.
      F(IRW)=AREA(NN)*(SPEC(M,1)-VX*(VEL(3,N4)+VEL(3,N3))/2.)
 1300 CONTINUE
!
!-
!...... Enter reduction of estifm and f for boundary elements
!-
!ipk oct98 update to f90
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
      DO 1100 I=1,NCN
         J=NOP(NN,I)
         IA=NDF*(I-1)
         DO 1050 K=1,NDF
            IA=IA+1
            JA=NBC(J,K)
            IF(JA > 0) THEN
              R1(JA)=R1(JA)+F(IA)
!              rkeepeq(ja)=rkeepeq(ja)+f(ia)
            ENDIF
 1050    CONTINUE
 1100 CONTINUE
!-
!......SAVE VELOCITY COEFFICIENT MATRIX
!-
!     WRITE(IVS) NN,((ESTIFL(I,J),J=1,NCN),G(I),I=1,NCN)
      RETURN
!-
!......RESTORE VELOCITY COEFFICIENT MATRIX
!-
! 1400 READ(IVS) NM,((ESTIFM(I,J),J=1,NCN),F(I),I=1,NCN)
!      IF(NM /= NN) GO TO 2000
 1400 DO 1520 I=1,NCN
      J=NOP(NN,I)
      IA=NDF*(I-1)
      DO 1520 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA <= 0) GO TO 1520
      R1(JA)=R1(JA)+F(IA)
!      rkeepeq(ja)=rkeepeq(ja)+f(ia)
 1520 CONTINUE
      RETURN
! 2000 WRITE(LOUT,6500) IVS,NN,NM
! 6500 FORMAT(//10X,'EXECUTION TERMINATED'//
!     1 10X,'ERROR READING FILE',I5,/10X,'ELEMENTS NUMBERED',I5,' AND',
!     2 I5,' DO NOT MATCH')
!      RETURN
      END
