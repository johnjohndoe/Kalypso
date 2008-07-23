C     Last change:  WP   14 Jan 2008    3:18 pm
CIPK  LAST UPDATE OCT 4 2002 ADD ICE THICKNESS TO OUTPUT
CIPK  LAST UPDATE JAN 12 20010 CHANGE AME TO AME1
CIPK  LAST UPDATE MAR 22 2000 ADD WSLL
CIPK LAST UPDATE NOV27 1997
cIPK  LAST UPDATE APR 27 1996
      SUBROUTINE OUTPUT(IDP)
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSANMOD
!NiS,apr06: adding module for Kalypso-specific calculations
      USE PARAKalyps
      !EFa Dec06, neues Modul für 1d-Teschke-Elemente
      USE Para1DPoly
!-
      SAVE
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK APR96
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05	INCLUDE 'BLKSAND.COM'
C-
      WRITE(LOUT,6000)
      IF( IDP .EQ. 0 ) WRITE(LOUT,6035)
 6035 FORMAT( / 5X, '..... INITIAL CONDITIONS .....' )
      IF(ITEQV(MAXN) .EQ. 0) WRITE(LOUT,6020)
      IF(ITEQV(MAXN) .EQ. 1) WRITE(LOUT,6021)
      IF(ITEQV(MAXN) .EQ. 2) WRITE(LOUT,6022)
      IF(ITEQV(MAXN) .EQ. 3) WRITE(LOUT,6023)
      IF(ITEQV(MAXN) .EQ. 4) WRITE(LOUT,6024)
      IF(ITEQV(MAXN) .EQ. 5) WRITE(LOUT,6025)
      IF(ITEQV(MAXN) .EQ. 6) WRITE(LOUT,6026)
      IF(ITEQV(MAXN) .EQ. 7) WRITE(LOUT,6027)
      IF(ITEQV(MAXN) .EQ. 8) WRITE(LOUT,6028)
      IF(ITEQV(MAXN) .EQ. 9) WRITE(LOUT,6029)
CIPK MAY02 ADD ICK=7
      IF(ITEQV(MAXN) .EQ. 10) WRITE(LOUT,6032)
      IF(ITEQV(MAXN) .EQ. 11) WRITE(LOUT,6030)
      IF(ITEQV(MAXN) .EQ. 12) WRITE(LOUT,6031)
      WRITE(LOUT,6001) TITLE

      WRITE(LOUT,6003) ICYC,IYRR,DAYOFY,TET,MAXN
cipk apr96
ccc      WRITE(LOUT,6003) ICYC,TET,MAXN
      WRITE(LOUT,6015)
      INT=(NP-LP)/2+1
CCC      INTT=INT+LP-1
C-
C-.....COMPUTE VALUES FOR SECONDARY OUTPUT.....
C-
      DO 233 J = 1, NP
      DO 230 K=1,7
      XVEL(K,J) = 0.0
  230 CONTINUE
  233 CONTINUE
      N=0
      DO 245 J = 1, NPM
      N=N+1
      XVEL(1,N) = VEL(1,J)
      XVEL(2,N) = VEL(2,J)
      XVEL(3,N) = VEL(3,J)
CIPK NOV97     XVEL(4,N) = VEL(3,J) + AO(J)
C      IF (IDNOPT.NE.0) THEN
C        HS = VEL(3,J)
C        ISWT = 0
cipk jan01  change AME to AME1
C        CALL AMF(H,HS,AKP(J),ADT(J),ADB(J),AME1,D2,ISWT)
C        XVEL(4,N) = H + ADO(J)
C      ELSE
C        XVEL(4,N) = VEL(3,J) + AO(J)
C      ENDIF
C	XHTEMP=XVEL(4,N)
CIPK MAR00
C     WSLL(J)=XHTEMP
      XVEL(4,N)=WSLL(J)

      XVEL(5,N) = VEL(4,J)
      XVEL(6,N) = VEL(5,J)
      XVEL(7,N) = VEL(6,J)
      XVEL(8,N) =VVEL(J)
      XVEL(9,N) = VEL(7,J)
CIPK OCT02
      XVEL(10,N)= ICETHK(J)*1000.
      LAB(N)=J
      K=NREF(J)+1
      IF(K .EQ. 1) GO TO 245
      L=NREF(J)+NDEP(J)-1
      IF(L .LT. K) GO TO 245
      DO 242 M=K,L
      N=N+1
      XVEL(1,N)=VEL(1,M)
      XVEL(2,N)=VEL(2,M)
      XVEL(3,N)=VEL(3,M)
C      XVEL(4,N)=XHTEMP
      XVEL(4,N)=WSLL(M)
      XVEL(5,N)=VEL(4,M)
      XVEL(6,N)=VEL(5,M)
      XVEL(7,N)=VEL(6,M)
      XVEL(8,N)=VVEL(M)
      XVEL(9,N)=VEL(7,M)
CIPK OCT02
      XVEL(10,N)= 0.0
      LAB(N)=M
  242 CONTINUE
  245 CONTINUE

!NiS,apr06: transformation of output for subroutine write_Kalypso and cwr-calculation. Part taken from Kalypso-2D; and changed
!           with proper arrays
      ! Knotenwerte (2D-Knoten) zur Ausgabe vorbereiten:
      DO i = 1, np
      	! velocity in x-direction
        rausv (1, i) = xvel (1, i)
      	! velocity in y-direction
        rausv (2, i) = xvel (2, i)
      	! watersurface elevation
        !rausv (3, i) = xvel (3, i)
        rausv (3, i) = WSLL(i)
      	! real flow depth
        rausv (4, i) = vel (3, i)
      END DO
!-

CIPK OCT02
      !nis,jan08: Write output data for every node in output.out only, if the user explicitly wants it (Control line C7)
      IF (WriteNodeBlock == 1) then
        IF(ICESW .EQ. 0) THEN

          IF(LSAND .GT. 0) THEN
            WRITE(LOUT,6018)
            WRITE(LOUT,6019) (LAB(J),(XVEL(K,J),K=1,9),
     1      J=LP,NP)
          ELSE
            WRITE(LOUT,6118)
            WRITE (LOUT, 6119) (LAB(J), (XVEL(K,J), K= 1,8), J = LP,NP)
          ENDIF
        ELSE
          IF(LSAND .GT. 0) THEN
            WRITE(LOUT,6038)
            WRITE(LOUT,6039) (LAB(J),(XVEL(K,J),K=1,10),
     1      J=LP,NP)
          ELSE
            WRITE(LOUT,6138)
            WRITE(LOUT,6139) (LAB(J),(XVEL(K,J),K=1,8),XVEL(10,J),
     1      J=LP,NP)
          ENDIF
        ENDIF
      endif
C
C..... Define flows for 1-D element node locations
C
      JJ=0
      DO 300 J=1,NPM
        !EFa Dec06, Fallunterscheidung für 1d-Teschke-Elemente
        IF(ah(j).gt.0.and.ndep(j).lt.2)then
          jj=jj+1
          lab(jj)=j
          XVEL(1,JJ)=(VEL(1,J)*COS(ALFA(J))+VEL(2,J)*SIN(ALFA(J)))
     +    *VEL(3,J)*ah(j)/vel(3,j)
        elseif(WIDTH(J) .GT. 0.  .AND. NDEP(J) .LT. 2) THEN
          JJ=JJ+1
          LAB(JJ)=J
          XVEL(1,JJ)=(VEL(1,J)*COS(ALFA(J))+VEL(2,J)*SIN(ALFA(J)))
     +    *VEL(3,J)*(2.*WIDTH(J)+(SS1(J)+SS2(J))*VEL(3,J))/2.
        ENDIF
  300 CONTINUE
      IF(IDP .EQ. 2) THEN
      WRITE(LOUT,6036)
        INT=(JJ-1)/5+1
        DO 450 I=1,INT
          WRITE(LOUT,6040) (LAB(J), XVEL(1,J),J=I,JJ,INT)
  450   CONTINUE
      ENDIF
      RETURN
 6000 FORMAT( 1H1  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRA
     1M RMA-10 '/ 10X, 'THREE-DIMENSIONAL HYDRODYNAMICS WITH SALINITY-TE
     2MPERATURE-SEDIMENT')
 6001 FORMAT( / 5X, A72 )
 6003 FORMAT( / 5X, 'RESULTS AFTER',I4, ' TIME STEPS YEAR =',I5,
     1'  DAY = 'I3,'  HOUR = 'F6.2,'  ITERATION CYCLE = ', I4 )
CIPK APR96
C 6003 FORMAT( / 5X, 'RESULTS AT THE END OF',I4, ' TIME STEPS...TOTAL TIM
C     1E =',F8.2 , ' HOURS....ITERATION CYCLE IS' , I5 )
 6015 FORMAT(// 20X,'NODAL VELOCITY,DEPTH,ELEVATION AND CONCENTRATION'/)
 6018 FORMAT('    NODE   X-VEL   Y-VEL    DEPTH     ELEV      SALT      
     +TEMP      SAND     V-VEL     E-POT')
 6118 FORMAT('    NODE   X-VEL   Y-VEL    DEPTH     ELEV      SALT      
     +TEMP       SED     V-VEL')
 6019 FORMAT(I8,2F8.4,2F9.3,3F10.3,F10.5,F10.3)
 6119 FORMAT(I8,2F8.4,2F9.3,3F10.3,F10.5)
 6020 FORMAT(10X,'VELOCITY HEAD AND SALINITY SIMULATED')
 6021 FORMAT(10X,'VELOCITY AND HEAD SIMULATED')
 6022 FORMAT(10X,'SALINITY SIMULATED')
 6023 FORMAT(10X,'VELOCITY AND SALINITY SIMULATED')
 6024 FORMAT(10X,'VELOCITY SIMULATED')
 6025 FORMAT(10X,'SYSTEM FORCED TO 1-D/2-D APPROXIMATION')
 6026 FORMAT(10X,'VELOCITY,HEAD AND TEMPERATURE SIMULATED')
 6027 FORMAT(10X,'VELOCITY,HEAD AND SEDIMENT SIMULATED')
 6028 FORMAT(10X,'TEMPERATURE SIMULATED')
 6029 FORMAT(10X,'SEDIMENT SIMULATED')
 6030 FORMAT(10X,'VELOCITY AND TEMPERATURE SIMULATED')
 6031 FORMAT(10X,'VELOCITY AND SEDIMENT SIMULATED')
 6032 FORMAT(10X,'BED LOAD SEDIMENT SIMULATED')
 6036 FORMAT(/'  FLOWS AT NODES FOR 1-D ELEMENTS'//)
 6038 FORMAT('    NODE    X-VEL    Y-VEL    DEPTH     ELEV      SALT    
     +  TEMP       SED     V-VEL     E-POT   ICE-THK')
 6039 FORMAT(I8,2F8.4,2F9.3,3F10.3,F10.5,F10.3,F10.2)
 6138 FORMAT('    NODE    X-VEL    Y-VEL    DEPTH     ELEV      SALT    
     +  TEMP       SED     V-VEL   ICE-THK')
 6139 FORMAT(I8,2F8.4,2F9.3,3F10.3,F10.5,F10.2)
 6040 FORMAT(5(I6,F9.3))
      END
