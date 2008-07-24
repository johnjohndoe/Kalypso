CIPK DEC02 ADD TEST FOR STRAY DIRECTIONS
cipk  new routine June 15 2002

      SUBROUTINE WAVECM(HSV,TP,WDIR,UGV,CDIR,D50MM,D90MM,RNU,SGSA,
     +RHOW,HD,GP,WS,ust,RC,RW,TRLOC,CA,ISWT,node)

c                       input node number                               Node
c input the significant wave height                                     hsv
c                     , peak period                                     tp
c                       wave direction (rad)                            wdir
c                       total current                                   ugv
c                       current direction (rad)                         cdir
c                       Median particle size of bed (mm)                d50mm
c                       90 % particle size (mm)                         d90mm
c                       Kinematic viscosity coefficent (m)              rnu
c                       Sediment specific gravity                       rhos
c                       Fluid density (kg/m3)                           rhow
c                       water depth  (m)                                hd
c                       equilibrium condentrattion                      gp
c                       settling velocity of sand (m/s)                 ws
C                       u-star                                          ust
c                       current related roughness(m)                    rc
c                       wave related roughness(m)                       rw
c                       tranportaion rate                               trloc
c                       concentraton at base of susp-sed                ca
      logical bo
      dimension gp(*)
      data itim/0/
c
c     preset ratio of sediment and fluid mixing (BETA)
c
c
      BETA=1.0
      PI=4.*ATAN(1.)

c
c     convert units
c
      D50=D50MM/1000.
      D90=D90MM/1000.
      RHOS=RHOW*SGSA
      CANG=180./PI*CDIR
      WANG=180./PI*WDIR

CIPK DEC02 ADD TEST FOR STRAY DIRECTIONS

   50 IF(WANG .LT. 0.) THEN
	  WANG=WANG+360.
	  GO TO 50
	ENDIF
   55 IF(WANG .GT. 360.) THEN
	  WANG=WANG-360.
	  GO TO 55
	ENDIF
   60 IF(CANG .LT. 0.) THEN
	  CANG=CANG+360.
	  GO TO 60
	ENDIF
   65 IF(CANG .GT. 360.) THEN
	  CANG=CANG-360.
	  GO TO 65
	ENDIF

ccc      write(109,'(i5,5f10.2)') 
ccc     +   node,cdir/pi*180.,wdir/pi*180.,cang,wang,phi
      pi=4.*atan(1.)
c      if(hsv.le.0.05) hsv=0.05
c      if(tp.le.1.1) tp=1.1
c      if(wang.le.0.01) wang=0.01
      if(UGV.eq.0.) UGV=0.000001
      if(cang.eq.0.) cang=0.000001

C      WS = ( 10.* RNU / (0.8 * D50) ) * (SQRT(1.+(0.01 * (RHOS/RHOW-1.) 
C     +* 9.81 * (0.8 * D50 ) ** 3) / RNU ** 2 ) -1.)

      PHI = ABS((180.-CANG) + WANG)
      IF(PHI.GE.180.) PHI = 360. - PHI         
CIPK JUN02      PHI=WANG-CANG
CIPK JUN02	IF(ABS(PHI) .LT. 0.001) PHI=0.
CIPK JUN02	IF(PHI .LT. 0.) PHI=PHI+360.

C
C  INITIALIZING SOME NUMERICAL AND PHYSICAL PARAMETERS
C
      NN     = 12
      G      = 9.81
      DEL=(RHOS-RHOW)/RHOW
      RKAP=.4
      DSTER=D50*(DEL*G/RNU**2)**(1./3.)
        
C
C   COMPUTATION OF WAVE LENGTH AND WAVE NUMBER
C
c     IF(HD .LE.0.)STOP 'WATER DEPTH .LE. 0'

cipk dec02 add test for wave height
      IF (TP .GT. 0.  .AND.  HSV .GT. 0.1) THEN

         Y=4.02*HD/TP/TP
c         print*,'Y=',Y
         POL=1.+Y*(.666+Y*(.355+Y*(.161+Y*(.0632+Y*(.0218+.00654*Y)))))
c         print*,'POL=',POL
         WAVENR=SQRT(Y**2+Y/POL)/HD
c         print*,'WAVENR=',WAVENR
         RLS=2.*PI/WAVENR
         ARG=WAVENR*HD
         IF (ARG.GT.50.) THEN
           ABW=0.
           FW=0.
           UBW=0.
         ELSE
           ABW=HSV/(2.*SINH(ARG))
           PSI = 4.02 * (ABW / TP) ** 2 / ((RHOS/RHOW -1) * D50)
c      print*,'abw,arg,d50,PSI',abw,arg,d50,psi
CIPK JUN02
C           if(rc .ne. 0) go to 100

CIPK MAR01 UPDATE TO DIVIDE BY 2 BASED ON NIELSEN 4/10/00
           ATEST1 = (21. * PSI ** (-1.85) * ABW)/2.
c      print*,'atest1',atest1

* For high wave energy conditions with a flat bed :

CIPK MAR01 UPDATE TO DIVIDE BY 2 BASED ON NIELSEN 4/10/00
           ATEST2 = 0.072 * ABW * (ABW / (3. * D90)) ** (-0.25)
c      print*,'atest2',atest2
 
           IF(PSI.LE.12) THEN
CIPK MAR01 UPDATE TO DIVIDE BY 2 BASED ON NIELSEN 4/10/00
             A = (0.275 - 0.022 * SQRT(PSI)) * ABW/2.
           ELSE 
             IF(ATEST1.GE.ATEST2) THEN
               A = ATEST1
             ELSE
               A = ATEST2
             ENDIF
           ENDIF


c              A  = ripple height
c              ABW = peak value of the near bed wave orbital excursion

c      print*,'a',a
CIPK MAR01 UPDATE TO MULTIPLY BY 6 BASED ON NIELSEN 4/10/00

           DS = 6. * A
CDJW
C		 DS = 0.05 

CDJW  6 SEEMS TOO LARGE HERE
           RC = 6. * A
           RW = 6. * A

cipk apr03
C           RC = A
C           RW = A


		 RC = MAX(RC,0.2) 
C		 RW = MAX(RW,0.2)   !Allow it smaller at present
C  LATER TRY RW = MAX(RW, RC)   ???????????

C
C  DJW 14/04/03
C		 
		 DSCHK = (HD*0.5)
           RC = MIN(RC, DSCHK)
           RW = MIN(RW, DSCHK)
C           DS = RC
		 DS = MIN(3*RW,DSCHK)
           A = RC





C	     IF (DS.GT.DSCHK) THEN
C		    DS = 0.5 * HD
C	        RW = 0.5 * HD
C	     END IF
C DJW APR03
C		 IF (RC.GT.DSCHK) THEN
C		    RC = MIN(DSCHK,0.1)
C	     END IF
CDJW OVERRIDE OF LOGIC 210403
C		 RC = MIN(DSCHK,0.1)
C		 RW = MIN(A,DSCHK)
C	     IF (A.GT.DSCHK) THEN
C			A = DSCHK
C		 END IF
C		 DS = A
	



CDJW		 
c              RW = effective wave related bed roughness 
c              RC = effective current related bed roughness 
c              DS = mixing layer thickness
CIPK JUN02
           go to 120
  100      DS=MAX(RC,RW)
           A=DS
  120      CONTINUE

           FW=EXP(-6.+5.2*(ABW/RW)**(-0.19))

c               FW = friction factor

c      print*,'fw',fw
           IF(FW.GT.0.3 )FW=0.3
           UBW  = 2.*PI/TP*ABW
c          print*,'fw,ubw',fw,ubw
C                      
C        STOKES 2ND ORDER CORRECTIONS FOR WAVE-RELATED TRANSPORT.
C
           UCRECT=7.4*HSV*HSV/(TP*RLS*(SINH(6.28*HD/RLS))**4)

* ABMIN cannot go negative - this is due to waves on the verge
* of breaking
* prevent this from happening 
* CAA 9/5/00

           UBMAX=UBW+UCRECT
           UBMIN=UBW-UCRECT

           if(ubmin.le.0.) ubmin=0.0000001

           ABMAX=UBMAX*TP/6.283
           ABMIN=UBMIN*TP/6.283

           if(abmin.le.0.) abmin=0.0000001

c          print*,'ubmax,ubmin,abmax,abmin',ubmax,ubmin,abmax,abmin
           DELWMX=0.216*ABMAX/(ABMAX/RW)**0.25
           DELWMN=0.216*ABMIN/(ABMIN/RW)**0.25

c          print*,'delwmx,delwmn',delwmx,delwmn
         ENDIF
c        print*,'ok'
C         A=0.1
         if(A.le.0.0001) a=0.0001
c        print*,A
      ELSE
         A=0.00
         ABW=0.
         ABR=0.
         FW=0.
         UBW=0.
      ENDIF
C
C  CRITICAL SHEAR STRESS SHIELDS
C
      IF(DSTER.LE.4.)THETCR=.24/DSTER
      IF(4. .LT.DSTER.AND.DSTER.LE.10.)THETCR=.14*DSTER**(-.64)
      IF(10..LT.DSTER.AND.DSTER.LE.20.)THETCR=.04*DSTER**(-.1 )
      IF(20..LT.DSTER.AND.DSTER.LE.150.)THETCR=.013*DSTER**(.29 )
      IF(DSTER.GT.150.)THETCR=.055
      TAUCR=(RHOS-RHOW)*G*D50*THETCR

C
C  COMPUTATION OF REFERENCE CONCENTRATION CA
C
      TOLD=0.0
  200 CONTINUE


      if(RC.le.0.001) RC=0.001
c      print*,RC
      CC=18.*ALOG10(12.*HD/RC)
      FC=0.24*ALOG10(12.*HD/RC)**(-2)
      FC1=0.24*ALOG10(12.*HD/3./D90)**(-2)
      RMUC=FC1/FC
      RMUW=0.6/DSTER
      UST=G**0.5*ABS(UGV)/CC
      DELS=0.
      IF(ABW.GT.0.)THEN
         DELS=0.216*ABW*(ABW/RW)**(-0.25)
      ENDIF
      HULP10=UBW/UGV
      GAMMA=0.
      IF(HULP10.GE.2.5)HULP10=2.5
      IF(0 .LE. PHI  .AND.  PHI .LE. 180.)THEN
         GAMMA=.75
         IF(PHI.GT.90.)GAMMA=GAMMA+(PHI-90.)/90.*.35
      ENDIF
      RA=EXP(GAMMA*HULP10)*RC
      FCA=0.24*ALOG10(12.*HD/RA)**(-2)
      IF(DELS.LE.RC/10.)THEN
        ALFAW=1.
      ELSE
        ALFAW= (ALOG(30.*DELS/RA)/ALOG(30.*DELS/RC))**2
      ENDIF
      TAUC=0.125*RHOW*FCA*UGV*UGV
      TAUW=0.25*RHOW*FW*UBW*UBW
      TAUCEF=RMUC*ALFAW*TAUC
      TAUWEF=RMUW*TAUW
      TAUCWE=TAUCEF+TAUWEF
      THET1=TAUCWE/(RHOS-RHOW)/G/D50
      T=(THET1-THETCR)/THETCR
      T=MAX(.0001,T)
c	write(112,'(i6,7g15.6)') node,thet1,thetcr,tauc,rmuc,alfaw,fca,ra
C
C     Get ripple height for no wave case
C
C	***********************************************************
C
C	Code Commented by DJW 3/04/03 Routine Rewritten below to ensure
C	That RC & A are recalculated for all nodes, regardless of wave conditions
C	And that iteration occurs in calculating T.  Also, a routine for calculating
C	BETA interactively was added
C
C	***********************************************************
C
      IF (TP .le. 0.  .or.  HSV .le. 0.1) THEN
CIPK JUN02
CDJW APR03        IF(RC .GT. 0.) GO TO 220
        IF(DSTER .LT. 10.) THEN 
          IF(T .LE. 3.) THEN
C
C     Mini ripples
C
            A=150*D50
          ELSEIF(T .LE. 25.) THEN
C
C     Dunes
C     
            A= 0.11*(D50/HD)**0.3*(1.-EXP(-0.5*T))*(25.-T)*HD
		  A= MAX(0.1,A)
          ELSE
C
C     Plane bed
C
CDJW APR03            A=0.002
C
		  A = 0.02
C END DJW APR03
          ENDIF
        ELSE
C
C       Dunes
C
          A= 0.11*(D50/HD)**0.3*(1.-EXP(-0.5*T))*(25-T)*HD
	    A= MAX(0.1,A)
C
        ENDIF
CDJW APR03
C	  DS = 0.05
C	  RW = 0.05
	  DSCHK = (HD*0.5)
C	  IF (DS.GT.DSCHK) THEN
C	    DS = 0.5 * HD
C	    RW = 0.5 * HD
C	  END IF
	
CDJW APR03      DS = 3. * A
C        RC = 3. * A
CDJW APR03
C        RW = 3. * A

Cipk APR03 
C        RC = A
C        RW = A
CDJW APR03
C	  DS = A
C	  IF (A.GT.DSCHK) THEN
C	    A = DSCHK
C		DS = DSCHK
C	    RC = DSCHK
C		RW = DSCHK
C	  END IF

          A = MIN(A, DSCHK)
          RC = MIN(1.5*A, DSCHK)
          RW = A
          DS = A

C	  RW = MAX(RW,0.2)
CIPK JUN02
C****************************************************************
CDJW APR03
C****************************************************************
C        TC = (TAUCEF - TAUCR)/TAUCR
C	  IF (TP .le. 0.  .or.  HSV .le. 0.3) THEN
C		A= 0.11*(D50/HD)**0.3*(1.-EXP(-0.5*TC))*(25.-TC)*HD
C	    IF (A.LT.0.02) THEN 
C	      A = 0.02
C         END IF
C	    RC = 3*A
C		DS = RW
C       END IF
CEND DJW APRO3
C****************************************************************
	  GO TO 240
  220   CONTINUE      
        DS=MAX(RC,RW)
	  A=DS
  240   CONTINUE
        IF(ABS(TOLD-T) .GT. 0.2) THEN
          TOLD=T
          GO TO 200
        ENDIF
      ENDIF
C****************************************************************
C
CDJW APR03 BETA CALCULATIONS 
C
C****************************************************************

	IF (HSV.LE.0.1) THEN
	  APPROXCHEZY = 18 * LOG10(12*HD/RC)
	  USTAR = (UGV*(9.81**0.5))/APPROXCHEZY
	  BETA = 1 + 2 * ((WS/USTAR)**2)
	  IF (BETA.GT.2.0) THEN
	    BETA = 2.0
        END IF
	END IF
C****************************************************************
C
C  ADJUSTS "A" DOWNWARDS TO ACCOUNT FOR MAJORITY OF LOAD BEING ALONG
C  THE BED
C 
C**************************************************************** 
C	A = 0.05
	CHK = 0.5*HD
	IF (DS.GT.CHK) THEN
	  DS = CHK
      END IF
	IF (A.GT.CHK) THEN
	  A = CHK
      END IF
	IF (DS.LT.0.001) THEN
	  DS = 0.001
      END IF
	IF (A.LT.0.001) THEN
	  A = 0.001
      END IF
C****************************************************************
CDJW APR03 SETS CONSTANT VALUE OF 0.1 FOR RW IF HSV > 0.1 ALSO
C CHECKS TO MAKE SURE IT IS NOT GREATER THAN THE WAVE HEIGHT
C****************************************************************
C	IF (HSV.GT.0.1) THEN
C	  RW = 0.1
C	END IF
	CHK = 0.75 * HD
	IF (RW.GT.CHK) THEN
	  RW = CHK
	END IF
C****************************************************************
CEND DJW APR03
C****************************************************************
C     print*,'ok'
      CA=0.015*D50/A*DSTER**(-.3)*T**1.5
C
C      FOR WAVE RELATED TRANSPORT
C
      TAUWMX=0.25*RHOW*FW*UBMAX*UBMAX
      TWMXEF=RMUW*TAUWMX
      TAUWMN=0.25*RHOW*FW*UBMIN*UBMIN
      TWMNEF=RMUW*TAUWMN
      TCWMAX=(TAUCEF*TAUCEF+TWMXEF*TWMXEF+2*TAUCEF*TWMXEF*COS(PHI*P
     1I/180.))**0.5
      TCWMIN=(TAUCEF*TAUCEF+TWMNEF*TWMNEF-2*TAUCEF*TWMNEF*COS(PHI*P
     1I/180.))**0.5
      THTMAX=TCWMAX/(RHOS-RHOW)/G/D50
      TMAX=(THTMAX-THETCR)/THETCR
      THTMIN=TCWMIN/(RHOS-RHOW)/G/D50
      TMIN=(THTMIN-THETCR)/THETCR
      TMIN=MAX(0.0001,TMIN)
      TMAX=MAX(0.0001,TMAX)

      CAMAX=0.015*D50/A*DSTER**(-0.3)*TMAX**1.5      
      CAMIN=0.015*D50/A*DSTER**(-0.3)*TMIN**1.5
C
C     CALCULATE WAVE-RELATED TRANSPORT
C   
      QWMAX=0.3*UBMAX*DELWMX*(CAMAX-CA)                
      QWMIN=0.3*UBMIN*DELWMN*(CA-CAMIN)
      QW=(QWMAX-QWMIN)/1000.
C                
C NUMERICAL INTEGRATION OF U*C OVER VERTICAL, TO STARTING POINT X=A
C
      JTAL= 8
      NN  = JTAL*NN
      DYM = CA/NN
      DXM = HD/NN
      DYX = DYM/DXM
      BO  = .FALSE.
      HULP30=-1.+ALOG(30.*HD/RA)
      IF(DELS.GT.0.)THEN
         UDEL=UGV*ALOG(30.*DELS/RA)/HULP30
      ENDIF
C
C  COMPUTATION OF DERIVATIVE DC/DY OR DC/DX
C
      ABR=MAX(3.*(HSV/HD)-.8,1.)
      EBW=.004*DSTER*ABR*DS*UBW
      IF (TP.GT.1.E-4) THEN
        EMAXW=0.035*ABR*HD*HSV/TP
      ELSE
        EMAXW=0.
      ENDIF
      IF(EMAXW .LE. EBW)EMAXW=EBW
      EMAXC=0.25*RKAP*UST*HD*BETA
      C=CA
      Z=A
      IF(Z.LE.DS)ESW=EBW
      IF(Z.GT.DS.AND.Z.LE.0.5*HD)ESW=EBW+(EMAXW-EBW)*((Z-DS)/
     *(0.5*HD-DS))
      IF(Z.GE.0.5*HD)ESW=EMAXW
      IF(Z.GE.0.5*HD)ESC=EMAXC
      IF(Z.LT.0.5*HD)ESC=EMAXC-EMAXC*(1.-2.*Z/HD)**2
      ES=(ESW**2.+ESC**2.)**0.5
      fcc=0.
      IF (C.GT.1.E-8) THEN
        IF(Z.GE.A)fcc=-WS/ES*C*(1.-C)**5.
      ENDIF
      YPRIME=fcc
      FF = 1./CA*YPRIME
      IF(DELS.GT.0.)THEN
         UC=UDEL*ALOG(30.*A/RC)/ALOG(30.*DELS/RC)
      ENDIF

      IF(A.GE.DELS)UC=UGV*ALOG(30.*A/RA)/HULP30
C
C  FURTHER INTEGRATION
C
      Y = CA
      TERM1=UC*Y
      XEND=A
      SS=0.
      NTEL = 0
      IT   = 2
C
  400 CONTINUE
      NTEL = NTEL+1
      XOLD = XEND
      YOLD = Y
      IF (-YPRIME .GT. DYX) THEN
         Y = YOLD-DYM
         IF (Y .LT. 2./3.*YOLD) Y = 2./3.*YOLD
         XEND = XOLD+ALOG(Y/YOLD)/FF
      ELSE
         XEND = XOLD+DXM
         IF (XEND .GE. HD) THEN
            XEND = HD
            BO = .TRUE.
         ENDIF
         Y = EXP(ALOG(YOLD)+(XEND-XOLD)*FF)
      ENDIF
      C=Y
      Z=XEND
      IF(Z .LE. DS)ESW=EBW
      IF(Z .GT. DS  .AND.  Z .LE. 0.5*HD) ESW=EBW+(EMAXW-EBW)*((Z-DS)/
     *(0.5*HD-DS))
      IF(Z.GE.0.5*HD)ESW=EMAXW
      IF(Z.GE.0.5*HD)ESC=EMAXC
      IF(Z.LT.0.5*HD)ESC=EMAXC-EMAXC*(1.-2.*Z/HD)**2
      ES=(ESW**2.+ESC**2.)**0.5
      fcc=0.
      IF (C.GT.1.E-8) THEN
        IF(Z.GE.A)fcc=-WS/ES*C*(1.-C)**5.
      ENDIF
      YPRIME=fcc
      FF = 1./Y*YPRIME
      IF(DELS.GT.0.)THEN
         UC=UDEL*ALOG(30.*XEND/RC)/ALOG(30.*DELS/RC)
      ENDIF
      IF(XEND.GE.DELS)UC=UGV*ALOG(30.*XEND/RA)/HULP30
      TERM2=UC*Y
      SS=SS+(XEND-XOLD)*(TERM1+TERM2)/2.
      TERM1=TERM2
      IF (NTEL .EQ. NN/JTAL  .OR.  BO) THEN
         IT = IT+1
         
      ENDIF
      IF (.NOT. BO) GOTO 400
      USTER1=SQRT(TAUCEF/RHOW)
      SBF=0.25*D50*USTER1*T**1.5/DSTER**0.3*RHOS/1000.
      SBD=0.25*D50*USTER1*T**1.5/DSTER**0.3
      SSR      = SS*RHOS/1000.
C
C      TOTAL CURRENT-RELATED SS AND BL TRANSPORT 
C              AND WAVE-RELATED TRANSPORT QW
C
cipk jan03      QT=((SSR+SBF)**2+QW*QW+2*(SSR+SBF)*QW*COS(PHI*PI/180))**0.5              
      QT=SSR+SBF
CDJW APR03 ENGELUND & HANSEN OVERRIDE****************************
C	
C	QTENGHANS	:  Transport as calculated using Engelund & Hansen (kg/m/s)
C	HD			:  Local Nodal Depth (m)	
C     CC			:  Approximate Chezy Coefficient = 18log(12*HD/RC)
C	RHOS		:  Density of Sand (kg/cu.m)
C	RHOW		:  Density of Water (kg/cu.m)
C	SGSA		:  Specific Gravity of Sand 
C	UGV			:  Local Velocity (m/s)
C	D50			:  Median Sediment Grain Size (m)
C	

	ehNUMERATOR = 0.05 * (UGV**5)
	DENOMINATOR = (((RHOS/RHOW)-1)**2)*(9.81**(0.5))*D50*(CC**3)
C /1000 SEEMS CONSISTENT WITH IPK CODE
	QTENGHANS = RHOS * (ehNUMERATOR/DENOMINATOR)/(1000)
C
C	ENGULAND & HANSEN IS PROBABLY MORE APPROPRIATE FOR AREAS
C	WHERE CURRENT IS THE PRIMARY TRANSPORTING PROCESS
C     PARTICULARLY FOR SOFTER, UNCONSOLIDATED SEDIMENTS THEREFORE
C     OVERRIDE VAN RIJN IN AREAS WHERE ENGULAND HANSEN PREDICTS A HIGHER
C	RATE OF TRANSPORT.
C
!      if(NODE .eq. 1850  .or.  NODE .eq. 1991) then
!	  wriTe(170,'(I6,5F17.4)') NODE,QT*1000.,QTENGHANS*1000.,UGV,HD,CC
!	endif
	IF (QTENGHANS.GT.QT) THEN
	  QT = QTENGHANS
      END IF
C****************************************************************
CEND DJW APR03
C****************************************************************
      GP(1)=QT/(HD*UGV)
	TRLOC=QT*1000.

c	write(111,'(i6,6g15.6)') node,ca,t,ugv,hd,trloc,hsv

CIPK JUN02
      IF(abs(ISWT) .ne. 1) RETURN

      if(iswt .eq. 1) then
 
      WRITE(75,'('' HD   = WATER DEPTH [ M ] =                     ''
     +,E10.4)') HD
      WRITE(75,'('' UGV  = MEAN CURRENT VELOCITY [ M/S ] =         ''
     +,E10.4)') UGV
      WRITE(75,'('' HSV  = SIGNIFICANT WAVE HEIGHT [ M ] =         ''
     +,E10.4)') HSV
      WRITE(75,'('' TP   = PEAK WAVE PERIOD        [ S ] =         ''
     +,E10.4)') TP
      WRITE(75,'('' PHI  = ANGLE CURRENT AND WAVES 0-180 [ DEG ]   ''
     +,E10.4)') PHI
      WRITE(75,'('' D50  = MEDIAN PARTICLE SIZE OF BED [ M ] =     ''
     +,E10.4)') D50
      WRITE(75,'('' D90  = 90 0/0 PARTICLE SIZE [ M ] =            ''
     +,E10.4)') D90
      WRITE(75,'('' WS   = FALL VELOCITY SUSP. SEDIMENT [ M/S ] =  ''
     +,E10.4)') WS
      WRITE(75,'('' RC   = CURRENT-RELATED ROUGHNESS [ M ] =       ''
     +,E10.4)') RC
      WRITE(75,'('' RW   = WAVE-RELATED ROUGHNESS [ M ] =          ''
     +,E10.4)') RW
      WRITE(75,'('' DS   = MIXING LAYER THICKNESS NEAR BED [ M ] = ''
     +,E10.4)') DS
      WRITE(75,'('' A    = REFERENCE LEVEL [ M ] =                 ''
     +,E10.4)') A
      WRITE(75,'('' RNU  = KINEMATIC VISCOSITY COEFFICIENT [ M ] = ''
     +,E10.4)') RNU
      WRITE(75,'('' RHOW = FLUID DENSITY [ KG/M3 ] =               ''
     +,E10.4)') RHOW
      WRITE(75,'('' RHOS = SEDIMENT DENSITY [ KG/M3 ] =            ''
     +,E10.4)') RHOS
      WRITE(75,'('' BETA = RATIO SEDIMENT AND FLUID MIXING [ - ] = ''
     +,E10.4)') BETA
 
 
      WRITE(75,'(A)')CHAR(12)
      WRITE(75,'('' PHYSICAL PARAMETERS :'',/)')
      WRITE(75,'('' DSTER = PARTICLE PARAMETER               [   -  ] ''
     *,E10.4)')DSTER
      WRITE(75,'('' L     = WAVE LENGTH                      [  M   ] ''
     *,E10.4)')RLS
      WRITE(75,'('' UBW   = PEAK ORBITAL VELOCITY            [ M/S  ] ''
     *,E10.4)')UBW
      WRITE(75,'('' ABW   = PEAK ORBITAL EXCURSION AT BED    [  M   ] ''
     *,E10.4)')ABW
      WRITE(75,'('' TAUW  = WAVE-RELATED BED-SHEAR STRESS    [ N/M2 ] ''
     *,E10.4)')TAUW
      WRITE(75,'('' TAUC  = CURRENT-RELATED BED-SHEAR STRESS [ N/M2 ] ''
     *,E10.4)')TAUC
      WRITE(75,'('' FW    = WAVE-RELATED FRICTION COEFFICIENT[  -   ] ''
     *,E10.4)')FW
      WRITE(75,'('' FC    = CURR-RELATED FRICTION COEFFICIENT[  -   ] ''
     *,E10.4)')FC
      WRITE(75,'('' C     = CHEZY COEFFICIENT                [M0.5/S] ''
     *,E10.4)')CC
      WRITE(75,'('' RA    = APPARENT ROUGHNESS               [  M   ] ''
     *,E10.4)')RA
      WRITE(75,'('' ALFAW = WAVE-CURRENT COEFFICIENT         [  -   ] ''
     *,E10.4)')ALFAW
      WRITE(75,'('' TAUCR = CRITICAL BED-SHEAR STRESS        [ N/M2 ] ''
     *,E10.4)')TAUCR
      WRITE(75,'('' UC    = CURR-RELATED EFFICIENCY FACTOR   [  -   ] ''
     *,E10.4)')RMUC
      WRITE(75,'('' UW    = WAVE-RELATED EFFICIENCY FACTOR   [  -   ] ''
     *,E10.4)')RMUW
      WRITE(75,'('' T     = BED-SHEAR STRESS PARAMETER       [  -   ] ''
     *,E10.4)')T
      WRITE(75,'(///)')
      WRITE(75,'('' NUMERICAL  : SS=SUSPENDED LOAD TRANSPORT [ KG/SM]''
     *,E10.4)')SSR*1000.
c      WRITE(75,'('' FORMULA    : SS=SUSPENDED LOAD TRANSPORT [ KG/SM]''
c     *,E10.4)')SSF
      WRITE(75,'(''            : SB=BED LOAD TRANSPORT       [ KG/SM]''
     *,E10.4)')SBF*1000.
	else
c	  write(109,'(i5,6f10.4,1f10.2)') 
c     +   node,trloc,qw*1000.,ssr*1000.,sbf*1000.,rc,rw,wdir*180./pi
	endif

      return
      end
