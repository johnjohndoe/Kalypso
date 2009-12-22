CIPK  LAST UPDATE DEC 1 UPDATE TO F90
crrr  last update Aug 30 1997
CIPK  FIRST ADDED JUNE 2 1997
      SUBROUTINE WSHEAR
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKFTMOD
C
      ALLOCATABLE INODX(:),WSHR(:)
C
      DATA IFIRST / 0 /
C
C  Add wind induced shear
C  By element type
C
      IF(IFIRST == 0) THEN
        ALLOCATE (INODX(MAXP), WSHR(MAXP))
        IFIRST=1      
      ENDIF
      DO J=1,NP
        INODX(J) = 0
        WAVVEL(J) = 0.
        FW(J) = 0.
        WAVHIT(J) = 0.
        WAVPER(J) = 0.
      ENDDO   
C
C   Process each element
C
      DO NN=1,NE
c
CIPK DEC97 UPDATE TO F90
        MR = IMAT(NN)
        MR = MOD(MR,1000)
        IF (MR > 0)  THEN
C
          WSPD = WNDSPDM(MR)
          IF (WSPD > 0.01)  THEN
C
            NCN = NCORN(NN)
C
C     Work around the nodes
C
            DO  K=1,NCN
              ND = NOP(NN,K)
              IF (ND > 0)  THEN

                DEPJ = VEL(3,ND)

                IF (INODX(ND) /= MR .OR. DEPJ < 0.001)  THEN
C
                  IF (DEPJ < .1)  DEPJ = .1 
C
                  J = ND
                  WNDSPDJ = WNDSPDM(MR)
                  FETCHJ = FETCHM(MR)
                  XMANNJ = XMANN(MR)
                     if (nd == 5) then
                       xxx = 1.
                     endif
C
                  CALL JONFW(J,WNDSPDJ,FETCHJ,DEPJ,XMANNJ,
     &                           WAVVELJ, FWJ,WAVHITJ,WAVPERJ)
C
                  IF (WAVVELJ > WAVVEL(ND))  THEN
                    WAVVEL(ND) = WAVVELJ
                    FW(ND) = FWJ
                    WAVHIT(ND) = WAVHITJ
                    WAVPER(ND) = WAVPERJ
                  ENDIF
C
                  INODX(ND) = MR
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C  Add in wind shear
C
      DO  J=1,NP
crrraug97         IF (WAVVEL(ND) > 0.01)  THEN
         IF (WAVVEL(J) > 0.001)  THEN
CIPK SEP05       
           GAW=GAWND(J)
           FWC = FW(J)
           WSHR(J) = 0.5*FWC*GAW* 0.5*WAVVEL(J)**2
           IF (WSHR(J) >= 0.6 .AND. VEL(3,J) <= 2.0)  WSHR(J) = .6
           fct = 1.
           if (VEL(3,j) < .1) then
              fct = (VEL(3,j)/.1)
           endif
           BSHEAR(J) = BSHEAR(J) + WSHR(J)*fct

         ENDIF
      ENDDO
C
C     if (nstep == 2)  then

c      WRITE(75,998)
C      DO 60 J=1,NP
C        IF (WAVVEL(J) < .01)  GO TO 60
c        WRITE(75,999) J,WAVHIT(J), WAVPER(J), WAVVEL(J), RYNOLD,
c     &                        FW(J), WSHR(J), BSHEAR(J), DEP(J)
C   60 CONTINUE
C      endif
C
      RETURN
C
C  998 FORMAT(//,1(3X,'NODE',1X,'WHITE',2X,'WPER',2X,'WVEL',1X,
C     &            'REYNOLD'))
C 999  FORMAT(1(3X,I4,2(2X,F4.2),2X,F4.2,2X,F6.1,3F7.3,F7.1))
C
      END
*
*
*********************************************************************
*
C>>>      INCLUDE 'RAEJONFW.f'

      SUBROUTINE JONFW(J,WNDSPDJ,FETCHJ,DEPJ,XMANNJ,
     &                                WAVVELJ,FWJ,WAVHITJ,WAVPERJ)
*
*  THIS IS A REVISED VERSION OF THE ORIGINAL 'JONFW' TO COMPUTE THE 
*  WAVE FRICTION COEFFICIENT BASED ON JONSSON(1965). IT CORRECTS 
*  SEVERAL THINGS, AS INDICATED IN THE CODE.
*
*
*   THIS VERSION WILL FIND THE WAVE PARAMETERS FOR BOTH 
*   SHALLOW-TRANSITIONAL WATER WAVES & DEEP WATER WAVES
*
*
*   CHANGES BY R EVANS, WES HE-P, MARCH 1989.
*    (NOTE: MY COMMENTS ARE ALWAYS WITH AN '*' IN COLUMN 1)
********************************************************************
*
*
      USE BLK10MOD
      USE BLKSEDMOD
C
      DIMENSION KSW(20)

      LOGICAL FIRST
          DATA IRD/-1/
      DATA FIRST /.TRUE./
     &     , ROUGH / 10000. /
C
crrr
      DATA  KSW / 20*0 /
      KSW(14) = 0
C
      ACGR = 9.81
      XNVALUJ = XMANNJ
C      EFDRJ = RKS*1000./2.5
      DSJ = 0.
      RYNOLDJ = 0.
      WAVHITJ = 0.
      WAVPERJ = 0.
      WAVVELJ = 0.
      FWJ = 0.
c
      CRIT=0.02
      MAXIT=25
      COEF=1.
C
*
*  DEFINE THE EQUIVALENT BED ROUGHNESS (METERS) AND THE
*  TRANSITIONAL DEPTH JUST ONCE
*  THE EQUATION FOR TRANSITION DEPTH COMES FROM SETTING
*  EQUATION 3-33 & EQUATION 3-39 (SHORE PROTECTION MANUAL) &
*  SOLVING FOR THE DEPTH (WIND SPEED IN MILES/HOUR, DTRAN IN
*  METERS).
*
      DTRANJ = 0.11972 * (WNDSPDJ * 0.447222)**2.46
C      IF (EFDRJ <= 0.)  THEN
      IF(RKS <= 0.) THEN
*
*  COMPUTE THE EFFECTIVE ROUGHNESS FROM THE MANNING'S NUMBER
*  NOTE THAT XNVALU(I) IS LIMITED TO BE NO GREATER
*  THAN 0.020, SINCE WE ARE DEALING WITH CLAYS
         IF (XNVALUJ >= 0.020) THEN
            DSJ = 0.0220
         ELSE
           DSJ = 3.44E8 * XNVALUJ**6.0
         END IF
*
      ELSE
*
*  USE THE INPUT VALUE, CONVERTED TO METERS
*  EQUI. ROUGHNESS = 2.5 * GRAIN SIZE
C         DSJ = 2.5 * EFDRJ * 0.001
          DSJ= RKS
      END IF
C
      IF (KSW(14) >= 2)  THEN
         WRITE(*,9714)  J,WNDSPDJ,DTRANJ,RKS,XNVALUJ,DSJ
     *        ,FETCHJ
      ENDIF
9714  FORMAT(' I=',I5,' W=',F5.1,' DT=',F7.2,' EF=',F7.3,
     * ' N=',F5.3,' DS=',F7.5,' F=',F5.1)
C
        FIRST = .FALSE.
*
******************************************************************
*
C
c      IF (KSW(14) /= 0)  WRITE(*,1180)
C
      PII = 4.*ATAN(1.)
      TP = 2.*PII
C
C
      IF (FETCHJ > 0.)  THEN
         IF (DEPJ < DTRANJ)  THEN

*
*  USE SHALLOW-TRANSITIONAL WATER EQUATIONS...
*
            CALL WAVE(FETCHJ,WNDSPDJ,DEPJ,H,PERIOD)
            WAVHITJ = H
            WAVPERJ = PERIOD
*
            SIGSQ = (TP/PERIOD)**2.
            CCOEF = ACGR*TP/SIGSQ
            CK = H/DEPJ
            IF (CK > 0.78) H = .78*DEPJ
            CC = TP*DEPJ
* 
*   THIS IS THE BEGINNING OF AN ITERATIVE PROCESS TO DETERMINE THE
*   WAVELENGTH, WL. 
*   WL1 = AN APPROXIMATION OF WL (INITIALLY BASED ON SHALLOW WATER)
*   ACGR = ACCELERATION DUE TO GRAVITY
*   WL2 IS COMPUTED USING THE EQUATION FOR TRANSITIONAL WATER, I.E.,
*        DEPTH/WAVELENGTH > 1/25 & < 1/2
*
*
            WL1 = SQRT(ACGR*DEPJ) * PERIOD
 1040       WL2 = CCOEF*TANH(CC/WL1)
            CHG = WL2 - WL1
            CHG = ABS(CHG)
            CHG = CHG/WL2
            WL1 = WL2
            IF (CHG > 0.05)  GOTO 1040
            WL = WL2
*
*   AT THIS POINT THE WAVELENGTH HAS BEEN DETERMINED & THE ROUTINE
*   IS COMPUTING THE WATER PARTICLE HORIZONTAL DISPLACEMENT AMPLITUDE &
*   THE WATER PARTICLE MAXIMUM HORIZONTAL SPEED. IT COMPUTES THE MAXIMUM
*   DISPLACEMENT USING EQUATION 2-22 FROM CERC'S "SHORE PROTECTION
*   MANUAL, VOL 1" WITH THE VALUES COMPUTED AT THE BOTTOM. IT THEN
*   ASSUMES THE MOTION IN THE HORIZONTAL TO BE SINUSOIDAL AS FOLLOWS:
*          X = A * sin(w * t), with A = FW(J), w = 2*PI / period &
*   THEREFORE, U = dx/dt = A * 2*PI / period * cos(w * t), WHICH 
*   GIVES WAVVEL(J) = FW(J) * TP / PERIOD 
*
* 
            FWJ = 0.5 * H/SINH(TP*DEPJ/WL)
            WAVVELJ = FWJ*TP/PERIOD
*
         ELSE
*
*  USE THE EQUATIONS FOR  DEEP WATER WAVES (MUCH SIMPLER)
*
            CALL DEEPWV(FETCHJ,WNDSPDJ,H,PERIOD)
            WL = ACGR * PERIOD * PERIOD / TP
            FWJ = 0.5 * H * EXP(-TP * DEPJ / WL)
            WAVVELJ = TP * FWJ / PERIOD
         ENDIF
      ENDIF
*
      IF (FWJ < 0.001) FWJ = 0.001
*
crrr
c
C
*  DS = THE EQUIVALENT BED ROUGHNESS SIZE (METERS)
*       DEFINED ALREADY IN AN ARRAY!
*
C>>>>        DS=EFDR(I) * 0.001
crrr        IF(DEP(J) <= 0.)THEN
crrr           FW(J) = -1.0
      IF (DEPj <= 0. )  THEN
         FWj = -1.0
         GOTO 1130
      ENDIF
C
      MSG1 = 0
      MSG2 = 0
      ORBWAV = FWJ
      UWAV = WAVVELJ

      vvec = sqrt(vel(1,j)**2 + vel(2,j)**2)

      VELRB = COEF*VVEC
      FW2 = 0.
C        DECIV=VELRB * 0.1
*
*   CHECK IF THE RATIO OF ORBITAL AMPLITUDE TO ROUGHNESS IS LESS
*   THAN 1.57 - IF SO, THEN SET FW2 = 0.30 AND SKIP OVER THE
*   ITERATIONS (JONSSON, 1975, 1976a)
*
      IF (ORBWAV/DSJ < 1.57)  THEN
          FW2 = 0.30
          GOTO 1110
      END IF
*
      CONST = 3.327 * ORBWAV/DSJ
*
*
*********************************************************************
*
*  RE = REYNOLDS NUMBER 
*
      RE = UWAV*ORBWAV/XNU(J)
*
      IF (ORBWAV/DSJ > 100.)  THEN 
*
*  TRY AN APPROXIMATION TO FW FOR ORBWAV/DS > 100, AS 
*  SUGGESTED BY JONSSON, 1967.
*
         FW1 = 0.09 / RE**0.2
*
      ELSE
*
*  THE FOLLOWING EQUATION FOR FW1 IS A FIRST APPROXIMATION OF THE
*  WAVE FRICTION COEFFICIENT, FW. IT IS BASED ON THE EQUATION BY
*  KAMPHUIS (1975).
*
          FW1 = 0.4*((DSJ / ORBWAV)**0.75)
      ENDIF
*
      RYNOLDJ = RE
*
* CHECK IF THE BOTTOM IS CONSIDERED 'ROUGH'
*
      IF (RE > ROUGH)  GOTO 1080
*
*  THIS IS NOT ROUGH!
        MSG1 = MSG1 + 1
        IF (MSG1 > 15)  GOTO 1080
        IF (KSW(14) < 2)  GOTO 1070
        WRITE(*,1150) J,RE
        IF (MSG1 == 15)  WRITE(*,1160)
 1070   CONTINUE
*
 1080 K = 0
*
*  THIS IS THE BEGINNING OF AN INTERATIVE METHOD TO DETERMINE THE
*  VALUE OF FW(*).
*
 1090 FW2 = 1./((4.*ALOG10(CONST*SQRT(FW1)))**2.)
      K = K + 1
      CHANGE = (FW1-FW2)/FW1
      CHANGA = ABS(CHANGE)
      IF (CHANGA < CRIT)  GOTO 1110
      FW1 = FW1*(1.-0.5*CHANGE)
      IF (K < MAXIT)  GOTO 1090 

*   END OF THE ITERATIONS...
* 
      MSG2 = MSG2 + 1
      IF (MSG2 > 15)  GOTO 1110
      IF (KSW(14) < 2)  GOTO 1100
      WRITE(*,1170) MAXIT,J
      IF (MSG2 == 15) WRITE(*,1160)
 1100 CONTINUE
C
 1110 FWJ = FW2
      IF (KSW(14) /= 0) WRITE(*,1190) J,UWAV,ORBWAV,VELRB,RE,FW2
C
 1130 CONTINUE
c      WRITE(*,*) '  >>>>    DEBUG IN JONFW   <<<<'
      IF (KSW(14) >= 2)  THEN 
         WRITE(*,998) 
  998    FORMAT(//,4(3X,'NODE',1X,'WHITE',2X,'WPER',2X,'WVEL',1X,
     &            'REYNOLD'))
C
         WRITE(*,999) J,WAVHITJ,WAVPERJ,WAVVELJ,RYNOLDJ
 999     FORMAT(4(3X,I4,2(2X,F4.2),2X,F4.2,2X,F6.1),/)
      ENDIF
C
c      WRITE(*,*) '  >>>>   END OF DEBUG STATEMENTS IN JONFW  <<<<'
C
 201  CONTINUE
      RETURN
C
 1150 FORMAT(1X,'*** WARNING -- WAVE FLOW NOT ROUGH AT NODE',I6,
     * '  RE= ',F12.6,'  IN SUB. JONFW ***')
 1160 FORMAT(//1X,' THIS WARNING WILL NOT BE PRINTED AGAIN...')
 1170 FORMAT(1X,'*** WARNING -- AFTER',I3,' ITERATIONS AT NODE',I6,
     * ' WAVE FRIC. FACTOR CALCS. HAVE NOT CONVERGED ***',/14X,
     * '*** NOW PROCEEDING WITH FINAL CALCULATED VALUE ***')
cccr 1180 FORMAT('1',///,'    *****ENTER SUBROUTINE JONFW*****',//5X,
 1180 FORMAT(/5X,
     * '     NODE   UWAV      ORBWAV     VELRB        RE        FW',/)
 1190 FORMAT(4X,I5,5F12.4)
      END
***************************************************************************
*
      SUBROUTINE WAVE(FETCH,WNDSPD,DEPTH,HEIGHT,PERIOD)
*
*  THIS ROUTINE WILL COMPUTE THE WAVE HEIGHT & PERIOD FOR
*  SHALLOW WATER WAVES, GIVEN THE FETCH, WIND SPEED & 
*  WATER DEPTH.
*
*   BASED ON EQUATIONS 3-39 & 3-40 IN THE "SHORE PROTECTION 
*   MANUAL, VOL 1", COASTAL ENGINEERING RESEARCH CENTER,
*   VICKSBURG, MS., (SECTION VI, PGS 3-55 TO 3-66).
*
*   CODED BY BOB EVANS, WES HE-P, MARCH 1989.
*
***********************************************************
*
*   INPUTS:
*    FETCH = THE WIND FETCH, IN STATUTE MILES.
*    WNDSPD = THE WIND SPEED, IN MILE/HOUR
*    DEPTH = WATER DEPTH (METERS)
*    
*   OUTPUTS:
*    HEIGHT = WAVE HEIGHT (METERS)
*    PERIOD = WAVE PERIOD (SECONDS)
*
************************************************************
*
      DATA THIRD / 0.333333333/
*
      G = 9.81
      F = FETCH * 1610.0
      SPD = WNDSPD * 0.447222222
      UA = 0.71 * SPD**1.23
      UA2 = UA * UA
      D = DEPTH
      C1 = G * D / UA2
      C2 = G * F / UA2
      C3 = TANH(0.530 * C1**0.750)
      C4 = TANH(0.833 * C1**0.375)
*
      A1 = 0.00565 * SQRT(C2)
      HEIGHT = (UA2/G) * 0.283 * C3 * TANH(A1 / C3)
*
      A2 = 0.0379 * C2**THIRD
      PERIOD = (UA/G) * 7.54 * C4 * TANH(A2 / C4)
*
      RETURN
      END

*
*
****************************************************************************
*
*
      SUBROUTINE DEEPWV(FETCH,WNDSPD,HEIGHT,PERIOD)
*
*  THIS ROUTINE WILL COMPUTE THE WAVE HEIGHT & PERIOD FOR
*  DEEP WATER WAVES, GIVEN THE FETCH AND WIND SPEED.
*
*   BASED ON EQUATIONS 3-33a, 3-34a, 3-36a & 3-37a 
*   IN THE "SHORE PROTECTION  MANUAL, VOL 1",
*     COASTAL ENGINEERING RESEARCH CENTER,
*   VICKSBURG, MS., (SECTION VI, PGS 3-55 TO 3-66).
*
*   CODED BY BOB EVANS, WES HE-P, MARCH 1989.
*
***********************************************************
*
*   INPUTS:
*    FETCH = THE WIND FETCH, IN STATUTE MILES.
*    WNDSPD = THE WIND SPEED, IN MILE/HOUR
*    
*   OUTPUTS:
*    HEIGHT = WAVE HEIGHT (METERS)
*    PERIOD = WAVE PERIOD (SECONDS)
*
************************************************************
*
      DATA THIRD / 0.333333333/
*
      G = 9.81
*
*  CONPUTE FETCH IN METERS & WIND SPEED IN M/SEC
*
      F = FETCH * 1610.0
      SPD = WNDSPD * 0.447222222
      UA = 0.71 * SPD**1.23
      UA2 = UA * UA
*
*  COMPUTE THE VALUES FOR A "FULLY DEVELOPED" SEA. THESE ARE THE 
*  MAXIMUM ALLOWABLE VALUES.
*
      HMAX = 0.02482 * UA2
      PMAX = 0.830 * UA
*
*  NOW COMPUTE THE VALUES FOR THE FETCH LIMITED CASE
*
      HEIGHT = 0.0005112 * UA * SQRT(F)
      PERIOD = 0.06238 * (UA * F)**THIRD
*
*  NOW COMPARE
*
      IF(HEIGHT > HMAX)HEIGHT = HMAX
      IF(PERIOD > PMAX)PERIOD = PMAX
*
      RETURN
      END
C







