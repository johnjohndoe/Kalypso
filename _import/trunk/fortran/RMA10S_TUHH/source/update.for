C     Last change:  EF   26 Mar 2007   11:11 am
CIPK  LAST UPDATE SEP 6 2004  add error file
CIPK  LAST UPDATE AUG 22 2001 REORGANIZE CONVERGENCE TESTING
CIPK  LAST UYPDATE APRIL 03  2001 ADD UPDATE OF WATER SURFACE ELEVATION 
CIPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
cipk  last update Jan 16 2000 fix bug from SL condition
CIPK  LAST UPDATED NOVEMBER 13 1997
CIPK  LAST UPDATE APR 30 1996
      SUBROUTINE UPDATE
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      !EFa Jan07, neues Modul für Teschke-Elemente
      USE PARAFlow1dFE
      SAVE
C-
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
!NiS,jul06: Consistent data types for passing paramters
      REAL(KIND=8) H, H1, VT, HS
!-
!nis,feb07: Cross section of Flow1dFE elements
      REAL (KIND=8) :: NodalArea
!-
cipk apr05
      common /epor/ efpor
C-
CIPK MAY02 EXPAND TO 7
      COMMON EMAX(7),EAVG(7),NMX(7)
C-
      DIMENSION IH(20,4),IL(20,4),NKCONV(100)
      CHARACTER*4 IVAR(7,2)
C-
      DATA IH/0,3,0,5,0,7,0,1,13,15,17,19,0,15,0,17,0,19,0,13,
     1        0,3,0,5,0,1,10,12,14,0,12,0,14,0,10,0,0,0,0,0,
     2        0,3,0,5,0,1,10,10,10,0,0,0,0,0,0,0,0,0,0,0,
     3        0,3,0,5,0,1,9,11,0,11,0,11,9,0,0,0,0,0,0,0/
      DATA IL/0,1,0,3,0,5,0,7,1,3,5,7,0,13,0,15,0,17,0,19,
     1        0,1,0,3,0,5,1,3,5,0,10,0,12,0,14,0,0,0,0,0,
     2        0,1,0,3,0,5,1,3,5,0,0,0,0,0,0,0,0,0,0,0,
     3        0,1,0,3,0,5,1,3,0,9,0,5,5,0,0,0,0,0,0,0/
C-
      DATA IVAR/'(X-F','(Y-F','(DEP','(SAL','(TEM','(SED','(BED',
     1          'LOW)','LOW)','TH) ',')   ','P)  ',')   ',')   '/

      DATA ITIMS/0/
!nis,jan07: Getting the information, whether network is only 1D or has other dimensions. This test needs only to be done in first iteration
      if (maxn.eq.1) then
        testfor1d: do i=1, ne
          if (ncorn(i) .gt. 3) then
            ONLY1D = 0
            WRITE(*,*) 'mehr als 1D-element:', i
            EXIT testfor1d
          end if
        end do testfor1d
      end if
!-
!nis,jan07,testing
!      do i = 1, 30
!        write(*,*) 'Knoten: ', i
!        WRITE(*,*) 'Richtungsbed.:', Alfa(i)
!        WRITE(*,*) 'Aend.: ', R1(nbc(i,1)), r1(nbc(i,2)), r1(nbc(i,3))
!        WRITE(*,*) 'Verknuepfung: ', nbc(i,1)
!      end do
      !pause
!-

C-
C-.....SETUP FOR SOLUTION CORRECTIONS.....
C-
      IF(MAXN .EQ. 1) THEN
cipk aug01
CIPK MAY02 EXPAND TO 7
        DO K=1,7
          NCNV(K)=9999
        ENDDO
        !nis,jan07: NITN is always the number of iterations in unsteady state. For steady state this would be NITI. The generalization for this
        !           loop would be using the NITA which is always a copy of the actual number
        !DO I=1,NITN
        DO I=1,NITA
        !-
 	    IF(ITEQV(I) .EQ. 0) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(3)=1        
	      NCNV(4)=1        
	    ELSEIF(ITEQV(I) .EQ. 1) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(3)=1        
	    ELSEIF(ITEQV(I) .EQ. 2) THEN
	      NCNV(4)=1
	    ELSEIF(ITEQV(I) .EQ. 3) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(4)=1        
	    ELSEIF(ITEQV(I) .EQ. 4) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	    ELSEIF(ITEQV(I) .EQ. 6) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(3)=1        
          NCNV(5)=1
	    ELSEIF(ITEQV(I) .EQ. 7) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(3)=1        
          NCNV(6)=1
	    ELSEIF(ITEQV(I) .EQ. 8) THEN
	      NCNV(5)=1
	    ELSEIF(ITEQV(I) .EQ. 9) THEN
	      NCNV(6)=1
CIPK MAY02
	    ELSEIF(ITEQV(I) .EQ. 10) THEN
          NCNV(7)=1
CIPK MAY02	    ELSEIF(ITEQV(I) .EQ. 10) THEN
	    ELSEIF(ITEQV(I) .EQ. 11) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(5)=1        
CIPK MAY02	    ELSEIF(ITEQV(I) .EQ. 11) THEN
	    ELSEIF(ITEQV(I) .EQ. 12) THEN
	      NCNV(1)=1
	      NCNV(2)=1
	      NCNV(6)=1
          ENDIF		         
        ENDDO

      !nis,jan07: If the network is just 1D, the second degree of freedom must not be calculated. Reactivating it:
      !if (ONLY1d .eq. 1) then
      !  NCNV(2) = 9999
      !end if
      !-

      WRITE(75,*) 'NCNV',MAXN,(NCNV(I),I=1,7)
CIPK AUG01 END UPDATE
      

        DO 80 N=1,100
          NKCONV(N)=9999
   80   CONTINUE
      ENDIF
      WRITE(75,*) 'NCNV',MAXN,(NCNV(I),I=1,7)
      URFC=1.0-FLOAT(IURVL(MAXN))*0.1
CIPK MAY02 EXPAND TO 7
      DO 100 J = 1, 7
      EAVG(J) = 0.0
      EMAX(J) = 0.0
      NMX(J)=0
  100 CONTINUE
C-
C-.....COMPUTE SOLUTION CORRECTIONS....
C-
CIPK MAY02 EXPAND TO 7
      DO 120 K=1,7
      EMAX(K) = 0.0
      EAVG(K) = 0.0
      NMX(K)=0
  120 CONTINUE
      NCONV=1
      IF (NDF .GT. 4) THEN
        NDFM=4
      ELSE
        NDFM=NDF
      ENDIF

      DO 160 KK = 1, NDFM
      K=KK
      COUNT = 0.0
      NP=NPSAV
      NE=NESAV
      IF(KK .GT. 3) K=ICK
cipk dec97 define urfcc
      URFCC =1.0
      ITEST=0
      DO 150 J = 1, NP
	IF(J .EQ. 1332) THEN
	AAA=0
	ENDIF
      IF( NBC(J,KK) ) 150, 150, 125
  125 I = NBC(J,KK)
      IF(ITEST .EQ. 0) THEN
CIPK AUG01 WE HAVE A NEW ACTIVE CONSTITUENT ASSUME ITS CONVERGED
        NCNV(K)=0
        ITEST=1
      ENDIF
      COUNT = COUNT + 1.0
c
cipk dec97 add factor for distributed elevation
c
      IF(K .EQ. 1) THEN
        IF(VSCALE(J) .EQ. 0.) THEN
          URFCC=1.0
        ELSE
          URFCC=VSCALE(J)
        ENDIF
      ENDIF
      EX=R1(I)*URFC*urfcc
cik dec97 end changes
      AEX = ABS( EX )
      EAVG(K) = EAVG(K) + AEX

cipk jun05
	if(k .ne. 3) then
        IF(AEX.LT.ABS(EMAX(K))) GO TO 128
        EMAX(K)=EX
        NMX(K)=J
  128   CONTINUE
	endif

      IF(ITEQV(MAXN) .EQ. 5) THEN
        IF(K .EQ. 1) FCA=UDST(J)
        IF(K .EQ. 2) FCA=VDST(J)
        IF(K .EQ. 3) FCA=1.0
        IF(K .GE. 4) FCA=SDST(J)
      ELSEIF(K .LT. 3) THEN
        FCA=FCTV(J)
      ELSEIF(K .GE. 4) THEN
        FCA=FCTS(J)
      ELSE
        FCA=1.0
      ENDIF
      EX=EX*FCA
      IF( K .GT. 2 ) GO TO 140
      IF(ADIF(J) .NE. 0.) EX=EX/COS(ADIF(J))

      IF( ALFA(J)  ) 130, 140, 130
cipk jan00  130 VEL(K,J) = VEL(K,J) + EX*COS(ALFA(J) )
cipk jan00      VEL(K+1,J) = VEL(K+1,J) + EX*SIN(ALFA(J) )
cipk jan00      GO TO 150
  130 CONTINUE
      IF(K .EQ. 1) THEN
        VEL(K,J)   = VEL(K,J)   + EX*COS( ALFA(J) )
        VEL(K+1,J) = VEL(K+1,J) + EX*SIN( ALFA(J) )
      ELSE
        VEL(K-1,J) = VEL(K-1,J) - EX*SIN( ALFA(J) )
        VEL(K,J)   = VEL(K  ,J) + EX*COS( ALFA(J) )
      ENDIF
      GO TO 150
CIPK NOV97
  140 IF(K .NE. 3) GO TO 149
CIPK APR05
      H1=VEL(3,J)
      CALL AMF(H,H1,AKP(J),ADT(J),ADB(J),AAT,D1,0)
      if(inotr .eq. 1) then
        EX=EX*EFPOR
	endif
      VN=VEL(3,J)+EX
c
c      Check sign of depth change
c
      IF(AKP(J) .GT. 0.9999) THEN
        VEL(3,J)=VN
      ELSE
c
c      Test for results passing through transition points
c
        ADTT=ADT(J)-(ADB(J)+ADT(J))/2.*(1.-AKP(J))
        ADBB=ADB(J)*AKP(J)
        IF(EX .LT. 0.) THEN
          IF(VEL(3,J) .GT. ADTT .AND. VN .LT. ADTT ) THEN
            vel(3,j)=adtt-0.00001
          ELSEIF(VEL(3,J) .GT. ADBB .AND. VN .LT. ADBB ) THEN
             VEL(3,J)=ADBB-0.00001
          ELSE
            VEL(3,J)=VN
          ENDIF
        ELSE
          IF(VEL(3,J) .LT. ADBB .AND. VN .GT. ADBB ) THEN
            VEL(3,J)=ADBB+0.00001
          ELSEIF(VEL(3,J) .LT. ADTT .AND. VN .GT. ADTT ) THEN
            VEL(3,J)=ADTT+0.00001
          ELSE
            VEL(3,J)=VN
          ENDIF
        ENDIF
      ENDIF

cipk jun05
      horg=hel(j)
	vt=vel(3,j)
      CALL AMF(HEL(J),VT,AKP(J),ADT(J),ADB(J),D1,D2,0)
      aex=abs(hel(j)-horg)
      IF(AEX.GT.ABS(EMAX(K))) THEN
        emax(k)=hel(j)-horg
        NMX(K)=J
      ENDIF




CIPK APR01 UPDATE WATER SURFACE ELEVATION

      IF (IDNOPT.NE.0) THEN
        HS = VEL(3,J)
        ISWT = 0
        CALL AMF(H,HS,AKP(J),ADT(J),ADB(J),AME1,D2,ISWT)
        WSLL(J) = H + ADO(J)
      ELSE
        WSLL(J) = VEL(3,J) + AO(J)
      ENDIF

      GO TO 150
CIPK MAY02 ALLOW FOR ICK=7
  149 CONTINUE
      IF(K .EQ. 7) THEN
	  GAN(J) = GAN(J)+EX
      ELSE
        VEL(K,J) = VEL(K,J) + EX
	ENDIF
  150 CONTINUE
      IF(COUNT .EQ. 0.) COUNT=1.E20
      EAVG(K) = EAVG(K) / COUNT
      IF(ABS(EMAX(K)) .GT. CONV(K) ) NCONV=0
CIPK AUG01
      IF(ABS(EMAX(K)) .GT. CONV(K)) NCNV(K)=1      
  160 CONTINUE
      WRITE(75,'(''NCNV-160'',9i5)') MAXN,(NCNV(I),I=1,7),nconv
C-
C-.....OUTPUT RESULTS OF CHANGES.....
C-
C      WRITE(LOUT,6003) ICYC,TET,MAXN
C      WRITE(LOUT,6005)
CIPK MAY02 EXPAND TO 7
      DO 200 J = 1, 7
C      WRITE(LOUT,6010) J,EAVG(J),EMAX(J),NMX(J),IVAR(J,1),IVAR(J,2)
CAUG93IPK  WRITE(*,6011) J,EAVG(J),EMAX(J),NMX(J),IVAR(J,1),IVAR(J,2),ICYC
        IF( J .EQ. 1) THEN
          WRITE(*,6011) J,EAVG(J),EMAX(J),NMX(J),IVAR(J,1),IVAR(J,2)
     +    ,ICYC,MAXN
        ELSE
          WRITE(*,6010) J,EAVG(J),EMAX(J),NMX(J),IVAR(J,1),IVAR(J,2)
        ENDIF
  200 CONTINUE
      IF(ITIMS .EQ. 0) THEN
        ITIMS=1
        WRITE(LITR,6041)
      ENDIF
 6041 FORMAT('                                MAX CHANGES
     +                                                           AVE CHA
     +NGES'/
     +       '    TIME STEP IT  NSZF     X-VEL NODE     Y-VEL NODE     D
     +EPTH NODE       SAL NODE      TEMP NODE       SED NODE    X-VEL   
     + Y-VEL    DEPTH      SAL     TEMP      SED')
      IF(MAXN .EQ. 1) WRITE(LITR,'(2X)')
CIPK MAY02 EXPAND TO 7
      WRITE(LITR,6040)TET,ICYC,MAXN,NSZF,(EMAX(J),NMX(J),J=1,7),
     +   (EAVG(J),J=1,7)

!NiS,mar06: Change of format-descriptor because number of descriptors does
!           not fit the number of written Variables; increasing number
!           from 6 to 7
! 6040 FORMAT(F8.1,I5,I3,I6,6(F10.4,I5),6(F9.4))
 6040  FORMAT(F8.1,I5,I3,I6,7(F10.4,I5),7(F9.4))


C      NKCONV(MAXN)=NCONV
C      IF(NCONV .EQ. 1) THEN
C        DO 205 MB=MAXN-1,1,-1
C          IF(NKCONV(MB) .NE. 9999) THEN
C            IF(NKCONV(MB) .EQ. 1) THEN
C              NCONV=2
C            ELSE
C              GO TO 206
C            ENDIF
C          ENDIF
C  205   CONTINUE
C
CIPK AUG01     TEST FOR CONVERGENCE THIS ITERATION, IF SO ARE WE FULLY CONVERGED?
      IF(NCONV .EQ. 1) THEN
CIPK MAY02 EXPAND TO 7
        DO MB=1,7
          IF(NCNV(MB) .NE. 9999) THEN
            IF(NCNV(MB) .EQ. 1) THEN
              GO TO 206
            ENDIF
          ENDIF
        ENDDO
        NCONV=2
        GO TO 207
C        IF(NCONV .EQ. 2) GO TO 207
  206   IF(MAXN .LT. NITA) THEN
          IF(ITEQV(MAXN+1) .EQ. ITEQV(MAXN)  .AND.
     +       ITEQS(MAXN+1) .EQ. ITEQS(MAXN)       ) THEN
            MAXN=MAXN+1
            GO TO 206
          ENDIF
        ENDIF
CIPK FEB03    RESET ALL FOR NON-CONVERGENCE
      ELSE
	  DO MB=1,7
	    IF(NCNV(MB) .NE. 9999) THEN
	      NCNV(MB)=1
	    ENDIF
	  ENDDO
      ENDIF
!*************************************************************************DJW 04/08/04
!
!     Checks Salinity Values against maximum and minimum permissible values and resets
!     to keep within an appropriate range as appropriate
!
!*************************************************************************
cipk feb07
      SalLowPerm = 0.0000
	SalHighPerm = 250
      Do J = 1,NP
	  If (Vel(4,J).LT.SalLowPerm) Then
          Vel(4,J) = SalLowPerm
        End If
	  If (Vel(4,J).GT.SalHighPerm) Then ! djw Salinity High Overide can be commented out if a crash is to be forced to 
          Vel(4,J) = SalHighPerm          ! Enable debugging
	  End If
	End Do
!*************************************************************************END DJW 04/08/04

  207 CONTINUE
      write(*,*) nconv,conv(1),conv(2),conv(3)
C
C...... DRPOUT forms list of equations eligible for dropout
C
c     CALL DRPOUT
C-
C......UPDATE MIDSIDE VALUES
C-
      DO 240 N=1,NE
      !EFa Dec06, Fallunterscheidung für 1d-Teschke-Elemente
      !nis,feb07: Allow for numbered FFF midsides
      !if (nop(n,2).EQ.-9999) GOTO 240 !EFa Dec06
      if (nop(n,2) < -1000) GOTO 240 !EFa Dec06
      !-
      IF(IMAT(N) .GT. 5000) GO TO 236

CIPK DEC00 ALLOW FOR GATE STRUCTURE

      IF(IMAT(N) .GT. 900  .and.  IGTP(N) .EQ. 0) GO TO 240
  236 CONTINUE
      ILK=1
      NCN=NCORN(N)
      IF(NCN .EQ. 5) NCN=3
      IF(NCN .EQ. 15  .OR.  NCN .EQ. 6 ) ILK=2
      IF(NCN .EQ. 10) ILK=3
      IF(NCN .EQ. 13) ILK=4
      DO 238 M=1,NCN
      N1=IL(M,ILK)
      IF(N1 .EQ. 0) GO TO 238
CIPK OCT98
      MM=NOP(N,M)
      N1=NOP(N,N1)
      N2=IH(M,ILK)
      N2=NOP(N,N2)
      VEL(3,MM)=(VEL(3,N1)+VEL(3,N2))/2.
CIPK MAY02
      GAN(MM)=(GAN(N1)+GAN(N2))/2.

CIPK APR01 ADD WATER SURFACE ELEVATION UPDATE
      WSLL(MM)=(WSLL(N1)+WSLL(N2))/2.

      IF(NSTRT(MM,1) .NE. 0) THEN
      VEL(4,MM)=(VEL(4,N1)+VEL(4,N2))/2.
      VEL(5,MM)=(VEL(5,N1)+VEL(5,N2))/2.
      VEL(6,MM)=(VEL(6,N1)+VEL(6,N2))/2.
      ENDIF
  238 CONTINUE
  240 CONTINUE
C-
C......PRINT AND/OR TERMINATE ON LARGE HEAD CHANGES
C-
      IF(    ABS(EMAX(3)) .GT. 100.  .OR.  ABS(EMAX(1)) .GT.  50.
     +  .OR. ABS(EMAX(2)) .GT.  50.) THEN
        CALL OUTPUT(2)
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,*) 'EXECUTION TERMINATED BY EXCESS CHANGES'
        WRITE(75,*) 'EXECUTION TERMINATED BY EXCESS CHANGES'
        STOP
      ENDIF
C-
 6000 FORMAT( 1H1  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRA
     1M RMA-10 '/ 10X, 'THREE-DIMENSIONAL HYDRODYNAMICS WITH SALINITY-TE
     2MPERATURE-SEDIMENT')
 6001 FORMAT( / 5X, 20A4 )
 6003 FORMAT( / 5X, 'RESULTS AT THE END OF',I4, ' TIME STEPS...TOTAL TIM
     1E =',F8.2 , ' HOURS....ITERATION CYCLE IS' , I5 )
 6005 FORMAT( // 10X,   'CONVERGENCE PARAMETERS'  // 8X, 'DF        AVG
     1CHG        MAX CHG     LOCATION')
 6010 FORMAT(I10,2F15.4,I10,4X,2A4)
C AUG93IPK 6011 FORMAT(I10,2F15.4,I10,4X,2A4,'  CYCLE NO ',I5)
 6011 FORMAT(I10,2F15.4,I10,4X,2A4,'STEP',I4,' ITER',I3)
C 6020 FORMAT(10X,'VELOCITY HEAD AND SALINITY SIMULATED')
C 6021 FORMAT(10X,'VELOCITY AND HEAD SIMULATED')
C 6022 FORMAT(10X,'SALINITY SIMULATED')
C 6023 FORMAT(10X,'VELOCITY AND SALINITY SIMULATED')
C 6024 FORMAT(10X,'VELOCITY SIMULATED')
C 6025 FORMAT(10X,'SYSTEM FORCED TO 1-D/2-D APPROXIMATION')
C 6026 FORMAT(10X,'VELOCITY,HEAD AND TEMPERATURE SIMULATED')
C 6027 FORMAT(10X,'VELOCITY,HEAD AND SEDIMENT SIMULATED')
C 6028 FORMAT(10X,'TEMPERATURE SIMULATED')
C 6029 FORMAT(10X,'SEDIMENT SIMULATED')
C 6030 FORMAT(10X,'VELOCITY AND TEMPERATURE SIMULATED')
C 6031 FORMAT(10X,'VELOCITY AND SEDIMENT SIMULATED')
      RETURN
      END
