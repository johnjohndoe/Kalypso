cipk  last update dec 2003   add iedsw dependence
CIPK  LAST UPDATE MAR 18 2003 add diffusion switch ( default of  0 uses old formulations
cipk  last update Jan 12 2001 add test for IMAT
CIPK  LAST UPDATE dEC 21 2000 ALLOW FOR GATE STRUCTURE
cipk  last update Dec 4 1998
CIPK  LAST UPDATE NOV 27 1997
cipk  last updated April 23 1997
cipk  last update Oct 1 1996 Add options for horizontal turbulence
      SUBROUTINE ANGLEN
      USE BLK10MOD
      USE BLK11MOD
C
      DIMENSION   DD(4), X(8),Y(8)
C
      DO 90 LL=1,NE
      !nis,may07
      if (imat(ll) == 89) GO TO 90
      !-

      IF (IMAT(LL) == 0) GO TO 90

cipk dec03 add iedsw depedence
        LLL = abs(MOD(IMAT(LL),1000))
        IEDSW=IEDSW1(LLL)

cipk OCT 96 add new options
cipk nov97 clarify options see below      IF(IEDSW == 0  ) GO TO 90
CIPK OCT96      IF (EEXXYY(1,LL) >= 0.) GO TO 90  
      IF(IEDSW == 0 .AND. EEXXYY(1,LL) >= 0.) GO TO 90

      !EFa may07, necessary for new turbulence subroutine
      IF(IEDSW >= 10 .AND. EEXXYY(1,LL) >= 0.) GO TO 90
      !-

C
C  1-D ELEMENTS
       IF (NOPS(LL,6) == 0)  then
CIPK DEC00
        IF (IMAT(LL) >= 900 .AND. IGTP(LL) == 0)  GOTO 90
C
          N1 = NOPS(LL,1)
          N3 = NOPS(LL,3)
          X(1) = CORD(N1,1)
          X(3) = CORD(N3,1)
          Y(1) = CORD(N1,2)
          Y(3) = CORD(N3,2)
          xla = SQRT((Y(1)-Y(3))**2 + (X(1)-X(3))**2)
CIPK NOV97 MAKE THIS ABSOLUTE
          EEXXYY(1,LL) = ABS(EEXXYY(1,LL))*XLA
          EEXXYY(5,LL) = ABS(EEXXYY(5,LL))*XLA
C
C  Convert for metric problems
C
         IF (GRAV < 32.)  THEN
            CVFCT = 516./(3.28)
            DO 8 I=1,4
               EEXXYY(I,LL) = CVFCT * EEXXYY(I,LL)
  8         CONTINUE
               EEXXYY(5,LL) = EEXXYY(5,LL)/3.28
               EEXXYY(6,LL) = EEXXYY(6,LL)/3.28
         ENDIF
          GOTO 90
       ENDIF 
cipk oct96 skip for iedsw=4
C
C  Get element area
       CALL COEF2(LL,0)
cipk apr97
       ELAREA(LL)=AREA(LL)

CIPK MAR03 add diffusion switch ( default of  0 uses old formulations)

       IF(IEDSW == 4) THEN
         IF(IDIFSW == 0 .OR. IDIFSW == 5) THEN
           GO TO 90
         ENDIF
       ENDIF
C
      DO 10 N=1,8
         ND = NOP(LL,N)
         IF (ND == 0)  GOTO 10
         X(N) = CORD(ND,1)
         Y(N) = CORD(ND,2)
 10   CONTINUE
C
C.... FIND LENGTH OF EACH SIDE
      DD(1) = SQRT((Y(1)-Y(3))**2 + (X(1)-X(3))**2)
      DD(2) = SQRT((Y(5)-Y(3))**2 + (X(5)-X(3))**2)
      IF (NOP(LL,7) == 0) THEN
         DD(4)=0.
         DD(3) = SQRT((Y(5)-Y(1))**2 + (X(5)-X(1))**2)
      ELSE
         DD(3) = SQRT((Y(5)-Y(7))**2 + (X(5)-X(7))**2)
         DD(4) = SQRT((Y(7)-Y(1))**2 + (X(7)-X(1))**2)
      ENDIF
C
C.... FIND LONGEST SIDE
      DMX=AMAX1(DD(1),DD(2),DD(3),DD(4))
      DO 20 J=1,4
         IF (ABS(DD(J)-DMX) <= 0.001) THEN
           DD(J)=0.
           L1=J
           GO TO 22
         ENDIF
 20   CONTINUE
C
C.... FIND NEXT LONGEST
 22   DMX=AMAX1(DD(1),DD(2),DD(3),DD(4))
      DO 24 J=1,4
         IF (ABS(DD(J)-DMX) <= 0.001) THEN
            L2=J
            GO TO 26
         ENDIF
 24   CONTINUE
 26   CONTINUE
      IF(L1 > L2) THEN
         K=L1
         L1=L2
         L2=K
      ENDIF
C
C.... DETERMINE LENGTH AND ANGLE OF MAJOR AXIS
      IF(NOP(LL,7) == 0) THEN
C.... TRIANGLES
         IF(L1 == 1 .AND. L2 == 3) THEN
            XX = X(1) - (X(3)+X(5))/2.
            YY = Y(1) - (Y(3)+Y(5))/2.
         ELSEIF(L1 == 1 .AND. L2 == 2) THEN
            XX = X(3) - (X(1)+X(5))/2.
            YY = Y(3) - (Y(1)+Y(5))/2.
         ELSE
            XX = X(5) - (X(1)+X(3))/2.
            YY = Y(5) - (Y(1)+Y(3))/2.
         ENDIF
C
      ELSE
C.... QUADRILATERALS
         IF(L1 == 1 .AND. L2 == 2) THEN
            XX = X(3) - (X(1)+X(5))/2.
            YY = Y(3) - (Y(1)+Y(5))/2.
         ELSEIF(L1 == 2 .AND. L2 == 3) THEN
            XX = X(5) - (X(3)+X(7))/2.
            YY = Y(5) - (Y(3)+Y(7))/2.
         ELSEIF(L1 == 3 .AND. L2 == 4) THEN
            XX = X(7) - (X(1)+X(5))/2.
            YY = Y(7) - (Y(1)+Y(5))/2.
         ELSEIF(L1 == 1 .AND. L2 == 4) THEN
            XX = X(1) - (X(3)+X(7))/2.
            YY = Y(1) - (Y(3)+Y(7))/2.
         ELSEIF(L1 == 1 .AND. L2 == 3) THEN
            XX = (X(7)+X(1))/2. - (X(3)+X(5))/2.
            YY = (Y(7)+Y(1))/2. - (Y(3)+Y(5))/2.
         ELSE
            XX = (X(3)+X(1))/2. - (X(7)+X(5))/2.
            YY = (Y(3)+Y(1))/2. - (Y(7)+Y(5))/2.
         ENDIF
      ENDIF
C
      ANG=ATAN2(YY,XX)
cccc      IF(EEXXYY(1,LL) < 0.) THEN
         XLA=SQRT(XX**2+YY**2)
         YLA=AREA(LL)/XLA
         IF(NOP(LL,7) == 0) YLA=YLA*2.
         IF(XLA < YLA) THEN
            TA=AREA(LL)
            IF(NOP(LL,7) == 0) TA=TA*2.
            XLA=SQRT(TA)
            YLA=XLA
            ANG=0.
         ENDIF
CIPK NOV97 MAKE THESE ABSOLUTE
         EEXXYY(1,LL)=ABS(EEXXYY(1,LL))*XLA
         EEXXYY(2,LL)=ABS(EEXXYY(2,LL))*YLA
         EEXXYY(3,LL)=ABS(EEXXYY(3,LL))*XLA
         EEXXYY(4,LL)=ABS(EEXXYY(4,LL))*YLA
         EEXXYY(5,LL)=ABS(EEXXYY(5,LL))*XLA
         EEXXYY(6,LL)=ABS(EEXXYY(6,LL))*YLA
c         T1=15.
c         T2=10.
         T1=0.
         T2=0.
         T3=0.
crrr August 1994 Experimental computation of minimum t1 and t2 not general
c                not currently used  i.e. t1=t2=0.
c          tt = sqrt(xla**2 + yla**2)
c          if (tt <= 0.0) stop
c          t1 = 02.5 * xla/tt
c          t2 = 02.5 * yla/tt
c
c          if (mod(imat(ll),1000) == 3)  then
c             t1 = 01.0 * xla/tt
c             t2 = 01.0 * yla/tt
c          endif
c
c
c
         IF(EEXXYY(1,LL) < T1) EEXXYY(1,LL)=T1
         IF(EEXXYY(2,LL) < T2) EEXXYY(2,LL)=T2
         IF(EEXXYY(3,LL) < T1) EEXXYY(3,LL)=T1
         IF(EEXXYY(4,LL) < T2) EEXXYY(4,LL)=T2
         IF(EEXXYY(5,LL) < T3) EEXXYY(5,LL)=T3
         IF(EEXXYY(6,LL) < T3) EEXXYY(6,LL)=T3
C
C  Convert for metric problems
C
         IF (GRAV < 32.)  THEN
c            CVFCT = 516./(3.28**2)
            CVFCT = 516./(3.28)
            DO 30 I=1,4
               EEXXYY(I,LL) = CVFCT * EEXXYY(I,LL)
 30         CONTINUE
               EEXXYY(5,LL) = EEXXYY(5,LL)/3.28
               EEXXYY(6,LL) = EEXXYY(6,LL)/3.28
          ENDIF
C      
cccc      ENDIF
      IF(TH(LL) == 0.) TH(LL)=ANG
 90   CONTINUE
                                                                                
CIPK NOV97     open(75,file='elemdir.dat')
         open(76,file='elemxycoef.dat')
         do 11  j=1,ne
           !nis,may07
           if (imat(j) /= 89) then
           !-
cipk jan01 put in test
             if(imat(j) > 0) then
               angx = th(j)
c              if (angx < 0.)  angx = angx + 3.14159
CC             write(75,1500) j, (nop(j,k),k=1,8), nfixh(j),angx
               write(76,1600) j, (eexxyy(k,j),k=1,6),ort(imat(j),5)
             endif
           !nis,may07
           end if
11       continue
1500     format(10i5,f10.3)
1600     format(i10,6f10.2,f10.3)
CCC      stop  '   anglen complete '
                                                                                
C         write(76,*) 'anglen ivrsid',ivrsid
                                                                                
      RETURN
      END
