!     Last change:  IPK   5 Oct 98    1:42 pm
!
      SUBROUTINE PLANAR(NN)
      USE COEF2MOD, only: DNX, DNY, XL, YL
      USE BLKSMOD
      USE BLK10MOD
      SAVE
!-
      DIMENSION CNAL(3,3)
!-
      REAL J11,J12,J21,J22
!-
      DATA CNAL/-3.,4.,-1.,-1.,0.,1.,1.,-4.,3./
!-
      NCN=NCORN(NN)
      IF(NCN == 3) GO TO 600
!-
!......SET REFERENCE LENGTH
!-
!ipk oct98 update to f90
      N1=ABS(NOP(NN,1))
      N2=ABS(NOP(NN,3))
      N3=ABS(NOP(NN,5))
      XL1=SQRT((CORD(N2,1)-CORD(N1,1))**2+(CORD(N2,2)-CORD(N1,2))**2)
      XL2=SQRT((CORD(N3,1)-CORD(N2,1))**2+(CORD(N3,2)-CORD(N2,2))**2)
      IF(XL2 > XL1) XL1=XL2
!-
!-.....COPY PROPER WEIGHTING FUNCTIONS.....
!-
      CALL SA2(NCN)
      N1=1
      N2=2
!-
!-.....COMPUTE LOCAL CORDS.....
!-
!ipk oct98 update to f90
      NR=ABS(NOP(NN,1))
      DXS=0.
      DYS=0.
      DO 100 K = 2, NCN
!ipk oct98 update to f90
      N=ABS(NOP(NN,K))
      DX=CORD(N,N1)-CORD(NR,N1)
      DY=CORD(N,N2)-CORD(NR,N2)
      DXS=DXS+ABS(DX)
      DYS=DYS+ABS(DY)
      XL(K)=DX
      YL(K)=DY
  100 CONTINUE
!-
!-.....COMPUTE ELEMENT EQUATIONS.....
!-
      DO 500 I = 1, NCN
      MN=NOP(NN,I)
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
      IF(ABS(DETJ) < 1.E-4*XL1) GO TO 550
      DO 135 J = 1, NCN
      DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / DETJ
      DNY(J) = ( J11 * DB(J,I) - J21 * DA(J,I) ) / DETJ
  135 CONTINUE
!-
!.....COMPUTE THE DERIVATIVES OF A AND H .....
!
      DO 275 M=1,NCN
!ipk oct98 update to f90
      MR=ABS(NOP(NN,M))
      DDHDX(MN) = DDHDX(MN) + DNX(M)*(VEL(3,MR)+AO(MR))
      DDHDY(MN) = DDHDY(MN) + DNY(M)*(VEL(3,MR)+AO(MR))
      DDAODX(MN) = DDAODX(MN) + DNX(M)*AO(MR)
      DDAODY(MN) = DDAODY(MN) + DNY(M)*AO(MR)
  275 CONTINUE
      DIVID(MN)=DIVID(MN)+1.
  500 CONTINUE
  550 CONTINUE
      RETURN
!-
!......PROCESS ONE-D ELEMENT
!-
  600 N1=NOP(NN,1)
      N2=NOP(NN,2)
      N3=NOP(NN,3)
      AL=SQRT((CORD(N3,1)-CORD(N1,1))**2+(CORD(N3,2)-CORD(N1,2))**2)
      CX=(CORD(N3,1)-CORD(N1,1))/AL
      SA=(CORD(N3,2)-CORD(N1,2))/AL
      XL(1)=(CORD(N2,1)-CORD(N1,1))*CX+(CORD(N2,2)-CORD(N1,2))*SA
      XL(2)=(CORD(N3,1)-CORD(N1,1))*CX+(CORD(N3,2)-CORD(N1,2))*SA
      DO 750 N=1,NCN
      MN=NOP(NN,N)
      TEMP=CNAL(2,N)*XL(1)+CNAL(3,N)*XL(2)
      if(cx < 0.) temp=-temp 
      DNX(1)=CNAL(1,N)/TEMP
      DNX(2)=CNAL(2,N)/TEMP
      DNX(3)=CNAL(3,N)/TEMP
      DO 640 M=1,NCN
!ipk oct98 update to f90
      MR=ABS(NOP(NN,M))
      DDHDX(MN)=DDHDX(MN)+DNX(M)*(VEL(3,MR)+AO(MR))
      DDAODX(MN)=DDAODX(MN)+DNX(M)*AO(MR)
  640 CONTINUE
      DIVID(MN)=DIVID(MN)+1.
  750 CONTINUE
      RETURN
      END
