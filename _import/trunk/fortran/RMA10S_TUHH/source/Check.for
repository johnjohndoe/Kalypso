CIPK  LAST UPDATE sEP 10 2001 FIX BUG FOR MULTIPLE TRIANGLES
cipk  last update apr 17 2001 fix case for laterally averaged flows
cipk  last update apr 12 2001 fix averages for zero flow case
cipk  LAST UPDATE mar 18 2001 add constituents for average concentration 
cipk  last update feb 09 2001 allow for zero length cc lines
cipk  last update jan 12 1999  add ss1 terms
C     Last change:  AF   17 Jul 2006    1:04 pm
cipk  last update Jan 21 1998
      SUBROUTINE CHECK
      USE BLK10MOD
      USE BLKDRMOD
      USE BLK11MOD
      SAVE
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK JUL01
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK MAR01
      INCLUDE 'BLKH.COM'
      INCLUDE 'BLKS.COM'
C-
cipk jan98 increment to 200
      DIMENSION ITEMP(200),FLW(2),AREAC(2),FLX1(2,3),FLX2(2,3),
     +FLX3(2,3),FLX4(2,3),FLX5(2,3),FLX6(2,3),FLX7(2,3),FLX8(2,3)
     +  ,TOTAL(200)
cipk jul01 add line above

!NiS,jul06: Adding WAITTH and WAITRH for consistency in common block defintion
!      REAL*8 WAITX,WAITT,WAITR
      REAL*8 WAITX,WAITT,WAITR,WAITTH,WAITRH
!-

      COMMON /WATP/ WAITT(7),WAITR(9),WAITTH(16),WAITRH(16)
C-
      COMMON 
     1 XN(8),WAITX(16)

      DATA NCALL/0/
      IF( NCL .LE. 0 ) RETURN
      IF( NCALL .GT. 0 ) GO TO 140
      NCALL = 1
C-
C-..... Augment continuity lists.....
C-
      DO 125 J = 1, NCL
        M = LMT(J)
cipk feb01  skip out for zero lines
        IF(M .EQ. 0) GO TO 125
        DO 113 K = 1, M
          ITEMP(K) = LINE(J,K)
  113   CONTINUE
        LMT(J) = 2*LMT(J)-1
        NN = LMT(J)
        N = 0
        IF(M .EQ. 1) GO TO 125
        DO 124 L = 1, NN, 2
          N = N + 1
          NA = ITEMP(N)
          NC = ITEMP(N+1)
          LINE(J,L) = NA
          LINE(J,L+2) = NC
          DO 123 JJ = 1, NE
            NCN=NCORN(JJ)
            DO 117 KK = 1, NCN, 2
              KKK = MOD(KK+2,NCN)
              IF(KKK .EQ. 0) KKK=3
CIPK OCT98 CONVERT TO F90
              N1 = ABS(NOP(JJ,KK))
              N2 = ABS(NOP(JJ,KKK))
              IF( NA .EQ. N1 .AND. NC .EQ. N2 ) GO TO 115
              IF( NC .EQ. N1 .AND. NA .EQ. N2 ) GO TO 115
              GO TO 117
CIPK OCT98 CONVERT TO F90
  115         LINE(J,L+1) = ABS(NOP(JJ,KK+1))
              GO TO 124
  117       CONTINUE
  123     CONTINUE
  124   CONTINUE
  125 CONTINUE

CIPK JUL01
      IF(IOCON .GT. 0) THEN
	  WRITE(IOCON,6200) (J,J=1,NCL)
 6200	  FORMAT('      TOTAL CONTINUITY LINE FLOWS BY CONTINUITY LINE NUM
     +BER'//'  DAY      HOUR',I9,19I11)
      ENDIF
      IF(NB .EQ. 0) RETURN
  140 CONTINUE
      WRITE(LOUT,6030)
      ND = NB
      DO 180 J = 1, NCL
CIPK OCT98        NTL=1
        SUMX = 0.0
        SUMY = 0.0
cipk jul01
        TOTAL(J) =0.0
	  SUMXS= 0.0
        SUMXT= 0.0
        SUMXSD=0.0
	  SUMYS= 0.0
        SUMYT= 0.0
        SUMYSD=0.0
cipk feb01  skip out for zero lines
        IF(LMT(j) .EQ. 0) THEN
	    GO TO 180
        ELSEIF(LMT(J) .EQ. 1) THEN
          NA=LINE(J,1)
          IF(NDEP(NA) .LT. 2) THEN
            SUMX=SQRT(VEL(1,NA)**2+VEL(2,NA)**2)*VEL(3,NA)*
     +           (2.*WIDTH(NA)+(SS1(NA)+SS2(NA))*VEL(3,NA))/2.
cipk mar01 add constituents
	      SUMXS=SUMX*VEL(4,NA)
            SUMXT=SUMX*VEL(5,NA)
	      SUMXSD=SUMX*VEL(6,NA)
          ELSE
            K=NREF(NA)
            L=K+NDEP(NA)-3
            SUMX=0.0
            XHT=ELEV-AO(NA)
            DO 160 M=K,L,2
              IF(M .EQ. K) THEN
                M1=NA
              ELSE
                M1=M
              ENDIF
              FL=(CORD(M1,3)-CORD(M+2,3))/XHT
              CAS=COS(ALFA(NA))
              SN=SIN(ALFA(NA))
              V1=VEL(1,M1)*CAS+VEL(2,M1)*SN
              V2=4.*(VEL(1,M+1)*CAS+VEL(2,M+1)*SN)
              V3=VEL(1,M+2)*CAS+VEL(2,M+2)*SN
CIPK JAN99
              XFC = VEL(3,NA)/XHT 
              V1 = V1*( WIDTH(NA) + XFC*(SS1(NA)+SS2(NA))*
     &                 (CORD(M1,3)-AO(M1) - xht*fl/6.) )

              V2 = V2*(WIDTH(NA) + XFC*(SS1(NA)+SS2(NA))*
     &                     (CORD(M+1,3)-AO(M+1)) )

              V3 = V3*(WIDTH(NA) + XFC*(SS1(NA)+SS2(NA))*
     &                     (CORD(M+2,3)-AO(M+2)  + xht*fl/6.) )

CIPK JAN99              SUMX=SUMX+(V1+V2+V3)/6.*VEL(3,NA)*FL*WIDTH(NA)
CIPK MAR01  ADD OTHER CONSTITUENTS
	        S1=V1*VEL(4,M1)/6.*VEL(3,NA)*FL
	        T1=V1*VEL(5,M1)/6.*VEL(3,NA)*FL
              SD1=V1*VEL(6,M1)/6.*VEL(3,NA)*FL
cip apr01 fix error using V1 for lines below
	        S2=V2*VEL(4,M+1)/6.*VEL(3,NA)*FL
	        T2=V2*VEL(5,M+1)/6.*VEL(3,NA)*FL
              SD2=V2*VEL(6,M+1)/6.*VEL(3,NA)*FL
	        S3=V3*VEL(4,M+2)/6.*VEL(3,NA)*FL
	        T3=V3*VEL(5,M+2)/6.*VEL(3,NA)*FL
              SD3=V3*VEL(6,M+2)/6.*VEL(3,NA)*FL
              TEMP=(V1+V2+V3)/6.*VEL(3,NA)*FL
              SUMX=SUMX+TEMP
	        SUMXS=SUMXS+(S1+S2+S3)
	        SUMXT=SUMXT+(T1+T2+T3)
	        SUMXSD=SUMXSD+(SD1+SD2+SD3)
  160       CONTINUE
          ENDIF
        ELSE
          MAX = LMT(J)-2
          DO 700 K = 1, MAX, 2
            NA = LINE(J,K)
            NB = LINE(J,K+1)
            NC = LINE(J,K+2)
            DX=(CORD(NC,1)-CORD(NA,1))
            DY=(CORD(NC,2)-CORD(NA,2))
            IF(NDEP(NB) .GT. 1) THEN
              IDPI=NDEP(NA)
              IDPK=NDEP(NC)
              I1=NA
              J1=NB
              K1=NC
C-
C...... Count elements downward
C-
              NEL=NDEP(NA)-2
              IF(NDEP(NC)-2 .GT. NEL) NEL=NDEP(NC)-2
              DO 500 M=1,NEL,2
C-
C...... Test for triangle
C-
                IF(IDPK .GT. 1) THEN
                  IF(IDPI .GT. 1) THEN
C-
C...... This is quadrilateral
C-
                    I2=NREF(NA)+M
                    I3=I2+1
                    J2=NREF(NB)+(M+1)/2
                    K2=NREF(NC)+M
                    K3=K2+1
                    VF1=VEL(3,NA)/(ELEV-AO(NA))
                    VF2=VEL(3,NC)/(ELEV-AO(NC))
                    VDIST=((CORD(I1,3)-CORD(I3,3))*VF1
     +                    +(CORD(K1,3)-CORD(K3,3))*VF2)/2.
                    AREAC(1)=VDIST*DY
                    AREAC(2)=VDIST*DX
                    DO  LA=1,2
                      FLW(LA)=((VEL(LA,I2)+VEL(LA,J2)+VEL(LA,K2)
     +                +VEL(LA,J1))/3.-(VEL(LA,I1)+VEL(LA,I3)+VEL(LA,K3)
     +                +VEL(LA,K1))/12.)*AREAC(LA)
CIPK MAR01   ADD OTHER CONSTITUENTS

C-
C-.....COPY PROPER WEIGHTING FUNCTIONS.....
C-
                      NGP = 9
	                NCN = 8
C-
C-.....COPY SHAPE FUNCTIONS
C-
                      CALL SB2(NCN,NGP)
C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
                      DO I = 1, NGP
                        WAITX(I) = WAITR(I)/4.
                        DO JJ = 1, NCN
                          XN(JJ) = XNX(JJ,I)
                        ENDDO

                  VX=VEL(LA,I1)*XN(1)+VEL(LA,I2)*XN(2)+VEL(LA,I3)*XN(3)
     +              +VEL(LA,J2)*XN(4)+VEL(LA,K3)*XN(5)+VEL(LA,K2)*XN(6)
     +              +VEL(LA,K1)*XN(7)+VEL(LA,J1)*XN(8)
                  S1=VEL(4,I1)*XN(1)+VEL(4,I2)*XN(2)+VEL(4,I3)*XN(3)
     +              +VEL(4,J2)*XN(4)+VEL(4,K3)*XN(5)+VEL(4,K2)*XN(6)
     +              +VEL(4,K1)*XN(7)+VEL(4,J1)*XN(8)
                  T1=VEL(5,I1)*XN(1)+VEL(5,I2)*XN(2)+VEL(5,I3)*XN(3)
     +              +VEL(5,J2)*XN(4)+VEL(5,K3)*XN(5)+VEL(5,K2)*XN(6)
     +              +VEL(5,K1)*XN(7)+VEL(5,J1)*XN(8)
                  SD1=VEL(6,I1)*XN(1)+VEL(6,I2)*XN(2)+VEL(6,I3)*XN(3)
     +              +VEL(6,J2)*XN(4)+VEL(6,K3)*XN(5)+VEL(6,K2)*XN(6)
     +              +VEL(6,K1)*XN(7)+VEL(6,J1)*XN(8)
	                  IF(LA .EQ. 1) THEN
                          SUMXS=SUMXS+AREAC(LA)*VX*S1*WAITX(I)
                          SUMXT=SUMXT+AREAC(LA)*VX*T1*WAITX(I)
                          SUMXSD=SUMXSD+AREAC(LA)*VX*SD1*WAITX(I)
	                  ELSE
                          SUMYS=SUMYS+AREAC(LA)*VX*S1*WAITX(I)
                          SUMYT=SUMYT+AREAC(LA)*VX*T1*WAITX(I)
                          SUMYSD=SUMYSD+AREAC(LA)*VX*SD1*WAITX(I)
	                  ENDIF
	                ENDDO

	              ENDDO
                    SUMX=SUMX+FLW(1)
                    SUMY=SUMY+FLW(2)
cipk jul01
                    TOTAL(J)=TOTAL(J)+FLW(1)-FLW(2)
                    IDPI=IDPI-2
                    IDPK=IDPK-2
                    I1=I3
                    J1=J2
                    K1=K3
                  ELSE
C-
C...... Triangle type 1
C-
                    J2=NREF(NB)+(M+1)/2
                    K2=NREF(NC)+M
                    K3=K2+1
                    VDIST=(CORD(K1,3)-CORD(K3,3))*VEL(3,NC)/(2.*
     +              (ELEV-AO(NC)))
                    AREAC(1)=VDIST*DY
                    AREAC(2)=VDIST*DX
                    DO 425 LA=1,2
                      FLW(LA)=(VEL(LA,J1)+VEL(LA,J2)+VEL(LA,K2))/3.
     +                        *AREAC(LA)
CIPK MAR01   ADD OTHER CONSTITUENTS

C-
C-.....COPY PROPER WEIGHTING FUNCTIONS.....
C-
                      NGP = 7
	                NCN = 6
C-
C-.....COPY SHAPE FUNCTIONS
C-
                      CALL SB2(NCN,NGP)
C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
                      DO I = 1, NGP
                        WAITX(I) = WAITT(I)*2.
                        DO JJ = 1, NCN
                          XN(JJ) = XNX(JJ,I)
                        ENDDO

                  VX=VEL(LA,I3)*XN(1)+VEL(LA,J2)*XN(2)+VEL(LA,K3)*XN(3)
     +              +VEL(LA,K2)*XN(4)+VEL(LA,K1)*XN(5)+VEL(LA,J1)*XN(6)
                  S1=VEL(4,I3)*XN(1)+VEL(4,J2)*XN(2)+VEL(4,K3)*XN(3)
     +              +VEL(4,K2)*XN(4)+VEL(4,K1)*XN(5)+VEL(4,J1)*XN(6)
                  T1=VEL(5,I3)*XN(1)+VEL(5,J2)*XN(2)+VEL(5,K3)*XN(3)
     +              +VEL(5,K2)*XN(4)+VEL(5,K1)*XN(5)+VEL(5,J1)*XN(6)
                  SD1=VEL(6,I3)*XN(1)+VEL(6,J2)*XN(2)+VEL(6,K3)*XN(3)
     +              +VEL(6,K2)*XN(4)+VEL(6,K1)*XN(5)+VEL(6,J1)*XN(6)
	                  IF(LA .EQ. 1) THEN
                          SUMXS=SUMXS+AREAC(LA)*VX*S1*WAITX(I)
                          SUMXT=SUMXT+AREAC(LA)*VX*T1*WAITX(I)
                          SUMXSD=SUMXSD+AREAC(LA)*VX*SD1*WAITX(I)
	                  ELSE
                          SUMYS=SUMYS+AREAC(LA)*VX*S1*WAITX(I)
                          SUMYT=SUMYT+AREAC(LA)*VX*T1*WAITX(I)
                          SUMYSD=SUMYSD+AREAC(LA)*VX*SD1*WAITX(I)
	                  ENDIF
	                ENDDO

  425               CONTINUE
                    SUMX=SUMX+FLW(1)
                    SUMY=SUMY+FLW(2)
CIPK JUL01
                    TOTAL(J)=TOTAL(J)+FLW(1)-FLW(2)
CIPK SEP01
                    IDPK=IDPK-2
                    J1=J2
                    K1=K3

CIPK SEP01                    GO TO 700
                  ENDIF
                ELSE
C-
C...... Triangle type 2
C-
                  J2=NREF(NB)+(M+1)/2
                  I2=NREF(NA)+M
                  I3=I2+1
                  VDIST=(CORD(I1,3)-CORD(I3,3))*VEL(3,NA)/
     +                  (2.*(ELEV-AO(NA)))
                  AREAC(1)=VDIST*DY
                  AREAC(2)=VDIST*DX
                  DO 475 LA=1,2
                    FLW(LA)=(VEL(LA,J1)+VEL(LA,J2)+VEL(LA,I2))/
     +                      3.*AREAC(LA)
CIPK MAR01   ADD OTHER CONSTITUENTS

C-
C-.....COPY PROPER WEIGHTING FUNCTIONS.....
C-
                    NGP = 7
	              NCN = 6
C-
C-.....COPY SHAPE FUNCTIONS
C-
                    CALL SB2(NCN,NGP)
C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
                    DO I = 1, NGP
                      WAITX(I) = WAITT(I)*2.
                      DO JJ = 1, NCN
                        XN(JJ) = XNX(JJ,I)
                      ENDDO

                  VX=VEL(LA,I1)*XN(1)+VEL(LA,I2)*XN(2)+VEL(LA,I3)*XN(3)
     +              +VEL(LA,J2)*XN(4)+VEL(LA,K3)*XN(5)+VEL(LA,J1)*XN(6)
                  S1=VEL(4,I1)*XN(1)+VEL(4,I2)*XN(2)+VEL(4,I3)*XN(3)
     +              +VEL(4,J2)*XN(4)+VEL(4,K3)*XN(5)+VEL(4,J1)*XN(6)
                  T1=VEL(5,I1)*XN(1)+VEL(5,I2)*XN(2)+VEL(5,I3)*XN(3)
     +              +VEL(5,J2)*XN(4)+VEL(5,K3)*XN(5)+VEL(5,J1)*XN(6)
                  SD1=VEL(6,I1)*XN(1)+VEL(6,I2)*XN(2)+VEL(6,I3)*XN(3)
     +              +VEL(6,J2)*XN(4)+VEL(6,K3)*XN(5)+VEL(6,J1)*XN(6)
	                IF(LA .EQ. 1) THEN
                        SUMXS=SUMXS+AREAC(LA)*VX*S1*WAITX(I)
                        SUMXT=SUMXT+AREAC(LA)*VX*T1*WAITX(I)
                        SUMXSD=SUMXSD+AREAC(LA)*VX*SD1*WAITX(I)
                      ELSE
                        SUMYS=SUMYS+AREAC(LA)*VX*S1*WAITX(I)
                        SUMYT=SUMYT+AREAC(LA)*VX*T1*WAITX(I)
                        SUMYSD=SUMYSD+AREAC(LA)*VX*SD1*WAITX(I)
	                ENDIF
	              ENDDO

  475             CONTINUE
                  SUMX=SUMX+FLW(1)
                  SUMY=SUMY+FLW(2)
CIPK JUL01
                  TOTAL(J)=TOTAL(J)+FLW(1)-FLW(2)
CIPK SEP01
                    IDPI=IDPI-2
                    J1=J2
                    I1=I3
CIPK SEP01                  GO TO 700
                ENDIF
  500         CONTINUE
            ELSE
C-
C........ Two dimensional element
C-
              D1=VEL(3,NA)
              D3=VEL(3,NC)
              IF(D1 .LE. DSET  .OR.  D3 .LE. DSET) GO TO 700
              D2=(D1+D3)/2.
CIPK MAR01  ADD OTHER CONSTITUENTS
              TEMPX=DY*(VEL(1,NA)*D1+4.0*VEL(1,NB)*D2+VEL(1,NC)*D3)
     1             /6.
              TEMPY=DX*(VEL(2,NA)*D1+4.0*VEL(2,NB)*D2+VEL(2,NC)*D3)
     1             /6.
              NGP=4
              DO I = 1, NGP
C-
C......DEFINE SHAPE FUNCTIONS AND ACCUMULATE
C-
                XN(1)=(1.-AFACT(I))*(1.-2.*AFACT(I))
                XN(2)=(1.-AFACT(I))*4.*AFACT(I)
                XN(3)=(2.*AFACT(I)-1.)*AFACT(I)
                VX=VEL(1,NA)*XN(1)+VEL(1,NB)*XN(2)+VEL(1,NC)*XN(3)
                VY=VEL(2,NA)*XN(1)+VEL(2,NB)*XN(2)+VEL(2,NC)*XN(3)
                DP=VEL(3,NA)*XN(1)+VEL(3,NB)*XN(2)+VEL(3,NC)*XN(3)
                S1=VEL(4,NA)*XN(1)+VEL(4,NB)*XN(2)+VEL(4,NC)*XN(3)
                T1=VEL(5,NA)*XN(1)+VEL(5,NB)*XN(2)+VEL(5,NC)*XN(3)
                SD1=VEL(6,NA)*XN(1)+VEL(6,NB)*XN(2)+VEL(6,NC)*XN(3)
                SUMXS=SUMXS+DY*VX*DP*S1*HFACT(I)/2.
                SUMXT=SUMXT+DY*VX*DP*T1*HFACT(I)/2.
                SUMXSD=SUMXSD+DY*VX*DP*SD1*HFACT(I)/2.
                SUMYS=SUMYS+DX*VY*DP*S1*HFACT(I)/2.
                SUMYT=SUMYT+DX*VY*DP*T1*HFACT(I)/2.
                SUMYSD=SUMYSD+DX*VY*DP*SD1*HFACT(I)/2.
              ENDDO

              SUMX=SUMX+TEMPX
              SUMY=SUMY+TEMPY
            ENDIF
  700     CONTINUE
        ENDIF
CIPK JUL01
        TOTAL(J) = SUMX - SUMY
cipk apr01
CIPK JUL01
        IF(TOTAL(J) .NE. 0.) THEN
	  AVES(J) = (SUMXS - SUMYS)/TOTAL(J)
	  AVET(J) = (SUMXT - SUMYT)/TOTAL(J)
	  AVESD(J)= (SUMXSD - SUMYSD)/TOTAL(J)
        ELSE
          AVES(J)=0.
          AVET(J)=0.
          AVESD(J)=0.
        ENDIF
CIPK JUL01
        IF( J .EQ. 1 ) REF = TOTAL(J)
        IF(ABS(REF) .LT. 0.0001) REF=1.
CIPK JUL01
        PCT = 100.0*TOTAL(J)/REF
CIPK OCT98        MX = LMT(J)
        WRITE(LOUT,6035) 
     +	  J,TOTAL(J),SUMX,SUMY,PCT,AVES(J),AVET(J),AVESD(J)
  180 CONTINUE
      NB = ND
CIPK JUL01
      IF(IOCON .GT. 0) THEN
	  DO JJ=1,NCL,20
	    JT=MIN(JJ+19,NCL)
          IF(JJ .EQ. 1) THEN
	      WRITE(IOCON,'(I5,F10.2,1P20E11.3)')
     +		 DAYOFY,TET,(TOTAL(J),J=JJ,JT)
          ELSE
	      WRITE(IOCON,'(15X,1P20E11.3)') (TOTAL(J),J=JJ,JT)
	    ENDIF
	  ENDDO
	ENDIF
      RETURN
 6030 FORMAT( // 10X, 'CONTINUITY CHECKS' //  10X, 'LINE          TOTAL
     1         X FLOW         Y FLOW   PERCENT  AVE-SALT  AVE-TEMP   AVE
     +-SED' )
 6035 FORMAT( 10X, I4, 1P3E15.3, 0PF10.1,3F10.2 )
      END
