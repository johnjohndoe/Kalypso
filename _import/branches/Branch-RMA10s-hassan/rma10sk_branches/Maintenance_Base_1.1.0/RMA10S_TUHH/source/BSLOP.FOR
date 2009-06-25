C     Last change:  WP   22 Jul 2008    5:01 pm

      SUBROUTINE BSLOP(NN)
      USE BLK10MOD
      USE blkecom
      SAVE
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
      INCLUDE 'BLKH.COM'
      INCLUDE 'BLKS.COM'
      REAL (KIND = 8) :: WAITX,WAITT,WAITR,WAITTH,WAITRH
      COMMON/WATP/ WAITT(7),WAITR(9),WAITTH(16),WAITRH(16)
C-
      COMMON
     1 XN(8),DNX(8),DNY(8),XM(4),DMX(4),DMY(4),XL(8),YL(8),
     1  WAITX(16)
C-
      DIMENSION ASWT(8,2)
C-
      REAL :: J11,J12,J21,J22
C-
      DATA ASWT/1.,3.,1.,3.,1.,3.,0.,0.,
     +          1.,2.,1.,2.,1.,2.,1.,2./
C-

C-.....ASSIGN PROPER COEFS.....
C-
      NCN=NCORN(NN)
CIPK OCT98      NCNX=NCN/2
C
CIPK OCT98 CONVERT TO LF90
      NMTYP=IMAT(NN)
      NM=MOD(NMTYP,100)
      CX=COS(TH(NN))
      SAN=SIN(TH(NN))
CIPK OCT98      NTYPE=IMAT(NN)/1000
C-
C......SHIFT IF ONE-D ELEMENT
C-
      IF(NCN .EQ. 3) GO TO 600
      IF(IMAT(NN) .GT. 5000) GO TO 900
C-
C......SET REFERENCE LENGTH
C-
CIPK OCT98 CONVERT TO F90
      N1=ABS(NOP(NN,1))
      N2=ABS(NOP(NN,3))
      N3=ABS(NOP(NN,5))
      XL1=SQRT((CORD(N2,1)-CORD(N1,1))**2+(CORD(N2,2)-CORD(N1,2))**2)
      XL2=SQRT((CORD(N3,1)-CORD(N2,1))**2+(CORD(N3,2)-CORD(N2,2))**2)
      IF(XL2 .GT. XL1) XL1=XL2
C-
C-.....COPY PROPER WEIGHTING FUNCTIONS.....
C-
      IF( NCN .LT. 8 ) THEN
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
C-
C-.....COPY SHAPE FUNCTIONS
C-
      CALL SB2(NCN,NGP)
C-
C-
C......SET UP PASS LIMIT DEPENDING ON ELEMENT TYPE
C-
      NPASS=3
      N1=2
      N2=3
      DO 550 NPA=1,NPASS
      IF(NPA .LT. 3) GO TO 94
      N1=1
      N2=2
   94 CONTINUE
C-
C-.....COMPUTE LOCAL CORDS.....
C-
CIPK OCT98 CONVERT TO F90
      NR=ABS(NOP(NN,1))
      DXS=0.
      DYS=0.
      DO 100 K = 2, NCN
CIPK OCT98 CONVERT TO F90
      N=ABS(NOP(NN,K))
      DX=CORD(N,N1)-CORD(NR,N1)
      DY=CORD(N,N2)-CORD(NR,N2)
      DXS=DXS+ABS(DX)
      DYS=DYS+ABS(DY)
      XL(K)=DX
      YL(K)=DY
  100 CONTINUE
C-
C-.....COMPUTE ELEMENT AREA...
C-
      ARA=0.
      DO 500 I = 1, NGP
C-
C-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
C-
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
      IF(ABS(DETJ) .LT. 1.E-4*XL1) GO TO 525
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
c     if(cx .lt. 0.) xy=-xy
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
