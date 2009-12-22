C     Last change:  WP    8 Nov 2007   11:11 am
cipk  last update sep 04 2004 add error message
CIPK  LAST UPDATE DEC 10 2002 rearrange boundaries near the inflow level, reset magnitude
cipk  last update feb 09 2001 allow for zero length cc lines
      SUBROUTINE QGENSPCL(IKEY,nl,J,QREQ,THET,QQAL)
      USE BLK10MOD
      USE BLK11MOD
      USE Para1DPoly
      SAVE
C-
C-..... Generate specified total flow boundary conditions for 3-d layer
C-
      DIMENSION QQAL(3)
C-
      ALLOCATABLE IQSPCL(:)
      DATA   IFIRST /  0 /
c
      DATA VOID/-1.E20/
C
      IF (IFIRST == 0)  THEN
         ALLOCATE (IQSPCL(MAXP))
         IQSPCL = 0
         IFIRST = 1
      ENDIF

      IF (IKEY == 1)  GO TO 600
C
C
      IF (IFIRST == 0)  THEN
         ALLOCATE (IQSPCL(MAXP))
         IQSPCL = 0
         IFIRST = 1
      ENDIF
C-
C...... Calculate total projected area
C-
      MAX = LMT(J)
CIPK FEB01
      IF(MAX == 0) THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(75,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET FLOW FOR NON-EXISTENT LINE',J
        WRITE(*,*) 'EXECUTION TERMINATED'
        stop
      ENDIF
      STQT(J) = THET
      IF (MAX == 1) THEN
C-
C...... This is for 1-D element
C-
         K = 0
         DO 120 M=1,NE
            K = K + 1
            IF (NOP(K,3) == LINE(J,1)) THEN
               NA = NOP(K,3)
               NC = NOP(K,1)
               IF (NC > NPM) GO TO 120
               GO TO 127
             ENDIF
C
             IF (NOP(K,1) == LINE(J,1)) THEN
                NA = NOP(K,1)
                NC = NOP(K,3)
                IF (NC > NPM) GO TO 120
                GO TO 127
             ENDIF
  120    CONTINUE
C
  127    NM = NOP(K,2)
C
         NDP = NDEP(NA)
         N1 = NREF(NA) + 1
         NV = NDEP(NA) + N1 - 2
         IF (N1 > 1)  THEN
            IQSPCL(NA) = 0
            DO I=N1,NV
               IQSPCL(I) = 0
            ENDDO
c
            LDX = NREF(NA) + 1 + 2*(nl-1) 
   
            IQSPCL(LDX) = 1 
            IQSPCL(NA) = -1
         ENDIF
C

!nis,may07
!Add midside node for polynom approach
!           !EFa Nov06, Sonderfall für Teschke-1D-Elemente
!           !nis,feb07: Allow for numbered FFF midsides
!           !if (nm == -9999) then
!           if (nm < -1000) then
!           !-
!             dx=cord(nc,1)-cord(na,1)
!             dy=cord(nc,2)-cord(na,2)
!           else
!Add midside node for polynom approach
!-

             IF(CORD(NM,1) > VOID) THEN
               DX=2.*(CORD(NM,1)-CORD(NA,1))-0.5*(CORD(NC,1)-CORD(NA,1))
               DY=2.*(CORD(NM,2)-CORD(NA,2))-0.5*(CORD(NC,2)-CORD(NA,2))
             ELSE
               DX=CORD(NC,1)-CORD(NA,1)
               DY=CORD(NC,2)-CORD(NA,2)
             ENDIF
         BEETA = ATAN2(DY,DX)
         WRITE(*,*) 'NA,NM,NC,DX,DY,,BEETA',NA,NM,NC,DX,DY,BEETA
         IF (ABS(BEETA-THET) > 1.570796 .AND. 
     +         ABS(BEETA-THET) < 4.712394) BEETA=BEETA+3.141596
         SPEC(NA,1) = QREQ * COS(BEETA)
         SPEC(NA,2) = QREQ * SIN(BEETA)
         NFIX(NA) = 31000
C
         IF (QQAL(1) >= 0.) THEN
            NFIX(NA) = NFIX(NA) + 10
            SPEC(NA,4) = QQAL(1)
            VEL(4,NA) = QQAL(1)
         ENDIF
C
         IF (QQAL(2) >= 0.) THEN
            NFIX(NA) = NFIX(NA) + 1
            SPEC(NA,5) = QQAL(2)
            VEL(5,NA) = QQAL(2)
         ENDIF
C
         IF (QQAL(3) >= 0.) THEN
            NFIX1(NA) = 1
            SPEC(NA,6) = QQAL(3)
            VEL(6,NA) = QQAL(3)
         ENDIF
C
cipk oct98         VM = SQRT(VEL(1,NA)**2+VEL(2,NA)**2)
C
C         VEL(1,NA)=VM*COS(BEETA)
C         VEL(2,NA)=VM*SIN(BEETA)
      ELSE
C-
C...... IBN contains counter for number of references to a node
C-
         DO 103 N=1,NPM
            IBN(N) = 0
  103    CONTINUE
C
         DO 105 N=1,NE
            IF (IMAT(N) < 1) GO TO 105
cipk oct98 update to f90
            NM = ABS(IMAT(N))
            IF (NM > 900) GO TO 105
            IF (ORT(NM,1) == 0.) GO TO 105
            NCN = NCORN(N)
            IF (NCN == 5) NCN=3
            DO 104 M=1,NCN
cipk oct98 update to f90

!nis,may07
!Add midside node for polynom approach
!              !EFa Nov06, Sonderfall für Teschke-1D-Elemente
!              !nis,feb07: Allow for numbered FFF midsides
!              !if (nop(n,2) /= -9999) then
!              if (nop(n,2) > -1000) then
!              !-
              K=ABS(NOP(N,M))
              IBN(K)=IBN(K)+1
!              end if
!Add midside node for polynom approach
!-

  104       CONTINUE
  105    CONTINUE
C
         SUMA = 0.
         MAX = LMT(J)-2
         DO 150 K = 1, MAX, 2
            NA = LINE(J,K)
            NC = LINE(J,K+2)
            DX = CORD(NC,1)-CORD(NA,1)
            DY =-(CORD(NC,2)-CORD(NA,2))
            !EFa Nov06, Sonderfall für Teschke-1D-Elemente mit Kilometrierung
            if (kmx(na) /= -1 .AND. kmx(nc) /= -1) then
              !nis,feb07: Scaling kilometers to meters
              !xl=ABS(kmx(na)-kmx(nc))
              xl=ABS(kmx(na)-kmx(nc)) * 1000
              !-
            else
              XL=SQRT(DX**2+DY**2)
            end if
            ALP = ATAN2(DX,DY)
            D1 = VEL(3,NA)
            D3 = VEL(3,NC)
            D2 = (D1+D3)/2.
            SUMA = SUMA+XL*COS(ALP-THET)*D2
c
         
            DO KK=1,3
               ND = LINE(J,K-1+KK)
               NDP = NDEP(ND)
               N1 = NREF(ND) + 1
               NV = NDEP(ND) + N1 - 2
               IF (N1 > 1)  THEN
                  IQSPCL(ND) = 0
                  DO I=N1,NV
                     IQSPCL(I) = 0
                  ENDDO
c
                  IF (MOD(KK,2) /= 0)   THEN
                     LDX = NREF(ND) + 1 + 2*(nl-1) 
                     IQSPCL(LDX) = 1 
                  ENDIF 
                  IQSPCL(ND) = -2
               ENDIF
            ENDDO


  150    CONTINUE
C-
C...... Compute velocity required
C-
         SUMA = ABS(SUMA)
         VEST = QREQ/SUMA
C-
C...... Insert values into SPEC  and  NFIX arrays
C-
         MAX = MAX + 2
         DO 300 K=1,MAX
            NA = LINE(J,K)
            D1 = VEL(3,NA)
            SPEC(NA,1) = VEST*D1*COS(THET)
            SPEC(NA,2) = VEST*D1*SIN(THET)
            IF (SPEC(NA,1) /= 0.) THEN
               NFIX(NA) = 31000
            ELSE
               NFIX(NA) = 13000
            ENDIF
            IF (QQAL(1) >= 0.) THEN
               NFIX(NA) = NFIX(NA)+10
               SPEC(NA,4) = QQAL(1)
               VEL(4,NA) = QQAL(1)
            ENDIF
            IF (QQAL(2) >= 0.) THEN
               NFIX(NA) = NFIX(NA)+1
               SPEC(NA,5) = QQAL(2)
               VEL(5,NA) = QQAL(2)
            ENDIF
C
            IF (QQAL(3) >= 0.) THEN
               NFIX1(NA) = 1
               SPEC(NA,6) = QQAL(3)
               VEL(6,NA) = QQAL(3)
            ENDIF
  300    CONTINUE
C-
C...... Find correct components for boundary nodes
C-
         DO 400 K=1,MAX,MAX-1
            NA = LINE(J,K)
C-
C...... Locate element with this node
C-
            DO 340 N=1,NE
               IF (IMAT(N) == 0) GO TO 340
               NCN = NCORN(N)
               DO 310 MM=1,NCN
                  M = MM
cipk oct98 update to f90
                  IF (ABS(NOP(N,M)) == NA) GO TO 315
  310          CONTINUE
               GO TO 340
C-
C....... Found a match node. Now determine side for parallel flow
C-
  315          IF (K == 1) THEN
                  KK = 2
               ELSE
                  KK = MAX-1
               ENDIF
               MLW = M-1
               IF (MM == 1) THEN
                  MLW = NCN
               ELSE
                  MLW = M-1
               ENDIF
C
cipk oct98 update to f90
cipk oct98               MLX = ABS(NOP(N,MLW))
               MSL = MOD(MLW+2,NCN)
               IF (MSL == 0) MSL=NCN
               MFR = MOD(MLW+3,NCN)
cipk oct98 update to f90
               MSL = ABS(NOP(N,MSL))
               IF (MSL == LINE(J,KK)) GO TO 317
               IF (IBN(MSL) == 1) GO TO 330
C
  317          CONTINUE
C
               MLW = MOD(M+1,NCN)
               IF (MLW == 0) MLW=NCN
cipk oct98 update to f90
cipk oct98               MLX = ABS(NOP(N,MLW))
C
               IF (MLW > 2) THEN
                  MSL = MLW-2
                  MFR = MLW-3
               ELSE
                  MSL = NCN
                  MFR = NCN-1
               ENDIF
cipk oct98 update to f90
               MSL = ABS(NOP(N,MSL))
               IF (MSL == LINE(J,KK)) GO TO 340
               IF (IBN(MSL) == 1) GO TO 330
C
               GO TO 340
C
  330          CONTINUE
C
cipk oct98 update to f90
               MFR = ABS(NOP(N,MFR))
C-
C...... Now compute boundary angle
C-
               DX = 2.*(CORD(MSL,1)-CORD(NA,1))
     &                        -0.5*(CORD(MFR,1)-CORD(NA,1))
               DY = 2.*(CORD(MSL,2)-CORD(NA,2))
     &                        -0.5*(CORD(MFR,2)-CORD(NA,2))
               BEETA = ATAN2(DY,DX)
C-
C...... Find angle of side that flow crosses
C-
               IF (K == 1) THEN
                  NAD = LINE(J,3)
               ELSE
                  NAD = LINE(J,MAX-2)
               ENDIF
               DXA = CORD(NAD,1)-CORD(NA,1)
C
               DYA =-(CORD(NAD,2)-CORD(NA,2))
               AALFA = ATAN2(DXA,DYA)
               IF (ABS(AALFA-THET) > 1.570796 .AND. 
     +             ABS(AALFA-THET) < 4.712394) THEN
                      AALFA=AALFA+3.141596
               ENDIF
               IF (ABS(AALFA-BEETA) > 1.570796 .AND. 
     +             ABS(AALFA-BEETA) < 4.712394) THEN
                      BEETA=BEETA+3.141596
               ENDIF
            WRITE(*,*) 'ADJUST AALFA BEETA THET', NA,AALFA,BEETA,THET
C-
C...... Finally adjust flows
C-
               IF (COS(THET) /= 0.) THEN
                  QMAG = SPEC(NA,1)*COS(AALFA-THET)
     &                        /(COS(THET)*COS(AALFA-BEETA))
               ELSE
                  QMAG = SPEC(NA,2)*COS(AALFA-THET)
     &                        /(SIN(THET)*COS(AALFA-BEETA))
               ENDIF
C
               SPEC(NA,1) = QMAG*COS(BEETA)
               SPEC(NA,2) = QMAG*SIN(BEETA)
C
               GO TO 400
C
  340       CONTINUE
  400    CONTINUE
      ENDIF
C
      IF (ICYC > 0) THEN
         DO 500 K=1,MAX
            NA = LINE(J,K)
            CALL BFORM(NA)
  500     CONTINUE
      ENDIF
C
      RETURN
C
C
  600 CONTINUE
C
C  Assign flow to proper layer
      DO 610 N=1,NPM
         IF (IQSPCL(N) == -1 .OR. IQSPCL(N) == -2)  THEN
C

            Q1 = SPEC(N,1)
            Q2 = SPEC(N,2)
C
            NDP = NDEP(N)
            N1 = NREF(N) + 1
cipk dec02 fix NA should be N
            NV = NDEP(N) + N1 - 2
C
            IF (N1 > 1)  THEN
C
               SUMZ = 0.
C
               DO 620 K=N1,NV,2
                  NT = K-1
cipk dec02 fix NA should be N
                  IF (K == N1)  NT = N
                  NM = K
                  NB = K+1
                  DZ = CORD(NT,3)-CORD(NB,3)
                  SUMZ = SUMZ + DZ
                  IF (IQSPCL(K) == 1) CZ = DZ
  620          CONTINUE
C
               QFCTR = SUMZ/CZ
               IF (IQSPCL(N) == -1)   THEN
                  QFCTR = QFCTR * 1.5
               ELSEIF (IQSPCL(N) == -2)   THEN
c                  QFCTR = QFCTR * 1.5/.8777
                  QFCTR = QFCTR * 1.5
               ELSE
cipk sep04
                  CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                  write(75,*) ' Error in QGENSPCL '
                  STOP ' QGENSPCL '
               ENDIF
               
CIPK DEC02
               IF(IQSPCL(N1) == 1) THEN
                 SPEC(N,1)=0.
                 SPEC(N,2)=0.
               ELSE
                 SPEC(N,1) = 0. 
                 SPEC(N,2) = 0.
                 NFIX(N)=31000
                 NFIX1(N)=0
               ENDIF
               NSPL(N)=2
C
               DO 630 K=N1,NV
                  IF (IQSPCL(K) == 1)  THEN
                     SPEC(K,1) = QFCTR * Q1
CFES   1/16/95                     SPEC(K,2) = QFCTR * Q2
CFES   1/16/95 NEXT LINE ADDED AS PER R.RACHIELE RECOMMENDATION
                     SPEC(K,2) =  Q2
cipk dec02
                  ELSEIF(IQSPCL(K-1) == 1) THEN
                    SPEC(K,1)=0.
                    SPEC(K,2)=0.
                  ELSE
                     SPEC(K,1) = 0.0
                     SPEC(K,2) = 0.0
                     NFIX(K)=31000
                     NFIX1(K)=0
                     VEL(4,K)=VOLD(4,K)
                     VEL(5,K)=VOLD(5,K)
                     VEL(6,K)=VOLD(6,K)
                  ENDIF
  630          CONTINUE
C
            ENDIF
         ENDIF
  610 CONTINUE

      END
