!     Last change:  WP   22 Jul 2008    5:01 pm
!
      SUBROUTINE BSLOP(NN)
      USE COEF2MOD
      USE BLKSMOD
      USE WATPMOD
      USE BLK10MOD
      USE blkecom
      SAVE
!-
      DIMENSION ASWT(8,2)
!-
      REAL :: J11,J12,J21,J22
!-
      DATA ASWT/1.,3.,1.,3.,1.,3.,0.,0.,                                &
     &          1.,2.,1.,2.,1.,2.,1.,2./
!-
!
!-.....ASSIGN PROPER COEFS.....
!-
      NCN=NCORN(NN)
!IPK OCT98      NCNX=NCN/2
!
!IPK OCT98 CONVERT TO LF90
      NMTYP=IMAT(NN)
      NM=MOD(NMTYP,100)
      CX=COS(TH(NN))
      SAN=SIN(TH(NN))
!IPK OCT98      NTYPE=IMAT(NN)/1000
!-
!......SHIFT IF ONE-D ELEMENT
!-
      IF(NCN == 3) GO TO 600
      IF(IMAT(NN) > 5000) GO TO 900
!-
!......SET REFERENCE LENGTH
!-
!IPK OCT98 CONVERT TO F90
      N1=ABS(NOP(NN,1))
      N2=ABS(NOP(NN,3))
      N3=ABS(NOP(NN,5))
      XL1=SQRT((CORD(N2,1)-CORD(N1,1))**2+(CORD(N2,2)-CORD(N1,2))**2)
      XL2=SQRT((CORD(N3,1)-CORD(N2,1))**2+(CORD(N3,2)-CORD(N2,2))**2)
      IF(XL2 > XL1) XL1=XL2
!-
!-.....COPY PROPER WEIGHTING FUNCTIONS.....
!-
      IF( NCN < 8 ) THEN
        NGP=7
        KK=1
        DO 80 M = 1, NGP
          WAITX(M)=WAITT(M)
   80   CONTINUE
      ELSE
        NGP=9
        KK=2
        DO 85 M = 1, NGP
          WAITX(M) = WAITR(M)
   85   CONTINUE
      ENDIF
!-
!-.....COPY SHAPE FUNCTIONS
!-
      CALL SB2(NCN,NGP)
!-
!-
!......SET UP PASS LIMIT DEPENDING ON ELEMENT TYPE
!-
      NPASS=3
      N1=2
      N2=3
      DO 550 NPA=1,NPASS
      IF(NPA < 3) GO TO 94
      N1=1
      N2=2
   94 CONTINUE
!-
!-.....COMPUTE LOCAL CORDS.....
!-
!IPK OCT98 CONVERT TO F90
      NR=ABS(NOP(NN,1))
      DXS=0.
      DYS=0.
      DO 100 K = 2, NCN
!IPK OCT98 CONVERT TO F90
      N=ABS(NOP(NN,K))
      DX=CORD(N,N1)-CORD(NR,N1)
      DY=CORD(N,N2)-CORD(NR,N2)
      DXS=DXS+ABS(DX)
      DYS=DYS+ABS(DY)
      XL(K)=DX
      YL(K)=DY
  100 CONTINUE
!-
!-.....COMPUTE ELEMENT AREA...
!-
      ARA=0.
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
      IF(ABS(DETJ) < 1.E-4*XL1) GO TO 525
      AMW = WAITX(I) * DETJ
      ARA=ARA+AMW
  500 CONTINUE
      DO 510 N=1,NCN
        IA=NOP(NN,N)
        FC(IA,NPA)=FC(IA,NPA)+ARA*ASWT(N,KK)/12.
  510 CONTINUE
  525 N2=1
      N1=3
  550 CONTINUE
      GO TO 900
  600 N1=NOP(NN,1)
      N3=NOP(NN,3)
      N2=NOP(NN,2)
      XL1=(CORD(N3,1)-CORD(N1,1))*CX+(CORD(N3,2)-CORD(N1,2))*SAN
      XY=CORD(N3,3)-CORD(N1,3)
!     if(cx < 0.) xy=-xy
      FC(N1,3)=FC(N1,3)+XL1/6.
      FC(N2,3)=FC(N2,3)+XL1*0.6667
      FC(N3,3)=FC(N3,3)+XL1/6.
      FC(N1,1)=FC(N1,1)-XY/6.*CX
      FC(N2,1)=FC(N2,1)-XY*0.6667*CX
      FC(N3,1)=FC(N3,1)-XY/6.*CX
      FC(N1,2)=FC(N1,2)-XY/6.*SAN
      FC(N2,2)=FC(N2,2)-XY*0.6667*SAN
      FC(N3,2)=FC(N3,2)-XY/6.*SAN
  900 CONTINUE
      RETURN
      END
