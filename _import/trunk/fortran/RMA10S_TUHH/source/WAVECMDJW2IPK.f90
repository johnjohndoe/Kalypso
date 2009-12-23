!IPK DEC02 ADD TEST FOR STRAY DIRECTIONS
!ipk  new routine June 15 2002
!
      SUBROUTINE WAVECM(HSV,TP,WDIR,UGV,CDIR,D50MM,D90MM,RNU,SGSA,      &
     &RHOW,HD,GP,WS,ust,RC,RW,TRLOC,CA,ISWT,node)
!
!                       input node number                               Node
! input the significant wave height                                     hsv
!                     , peak period                                     tp
!                       wave direction (rad)                            wdir
!                       total current                                   ugv
!                       current direction (rad)                         cdir
!                       Median particle size of bed (mm)                d50mm
!                       90 % particle size (mm)                         d90mm
!                       Kinematic viscosity coefficent (m)              rnu
!                       Sediment specific gravity                       rhos
!                       Fluid density (kg/m3)                           rhow
!                       water depth  (m)                                hd
!                       equilibrium condentrattion                      gp
!                       settling velocity of sand (m/s)                 ws
!                       u-star                                          ust
!                       current related roughness(m)                    rc
!                       wave related roughness(m)                       rw
!                       tranportaion rate                               trloc
!                       concentraton at base of susp-sed                ca
      logical bo
      dimension gp(*)
      data itim/0/
!
!     preset ratio of sediment and fluid mixing (BETA)
!
!
      BETA=1.0
      PI=4.*ATAN(1.)
!
!
!     convert units
!
      D50=D50MM/1000.
      D90=D90MM/1000.
      RHOS=RHOW*SGSA
      CANG=180./PI*CDIR
      WANG=180./PI*WDIR
!
!IPK DEC02 ADD TEST FOR STRAY DIRECTIONS
!
   50 IF(WANG < 0.) THEN
        WANG=WANG+360.
        GO TO 50
      ENDIF
   55 IF(WANG > 360.) THEN
        WANG=WANG-360.
        GO TO 55
      ENDIF
   60 IF(CANG < 0.) THEN
        CANG=CANG+360.
        GO TO 60
      ENDIF
   65 IF(CANG > 360.) THEN
        CANG=CANG-360.
        GO TO 65
      ENDIF
!
!cc      write(109,'(i5,5f10.2)') 
!cc     +   node,cdir/pi*180.,wdir/pi*180.,cang,wang,phi
      pi=4.*atan(1.)
!      if(hsv <= 0.05) hsv=0.05
!      if(tp <= 1.1) tp=1.1
!      if(wang <= 0.01) wang=0.01
      if(UGV == 0.) UGV=0.000001
      if(cang == 0.) cang=0.000001
!
!      WS = ( 10.* RNU / (0.8 * D50) ) * (SQRT(1.+(0.01 * (RHOS/RHOW-1.) 
!     +* 9.81 * (0.8 * D50 ) ** 3) / RNU ** 2 ) -1.)
!
      PHI = ABS((180.-CANG) + WANG)
      IF(PHI >= 180.) PHI = 360. - PHI         
!IPK JUN02      PHI=WANG-CANG
!IPK JUN02      IF(ABS(PHI) < 0.001) PHI=0.
!IPK JUN02      IF(PHI < 0.) PHI=PHI+360.
!
!
!  INITIALIZING SOME NUMERICAL AND PHYSICAL PARAMETERS
!
      NN     = 12
      G      = 9.81
      DEL=(RHOS-RHOW)/RHOW
      RKAP=.4
      DSTER=D50*(DEL*G/RNU**2)**(1./3.)
!
!
!   COMPUTATION OF WAVE LENGTH AND WAVE NUMBER
!
!     IF(HD <= 0.)STOP 'WATER DEPTH <= 0'
!
!ipk dec02 add test for wave height
      IF (TP > 0. .AND. HSV > 0.1) THEN
!
         Y=4.02*HD/TP/TP
!         print*,'Y=',Y
         POL=1.+Y*(.666+Y*(.355+Y*(.161+Y*(.0632+Y*(.0218+.00654*Y)))))
!         print*,'POL=',POL
         WAVENR=SQRT(Y**2+Y/POL)/HD
!         print*,'WAVENR=',WAVENR
         RLS=2.*PI/WAVENR
         ARG=WAVENR*HD
         IF (ARG > 50.) THEN
           ABW=0.
           FW=0.
           UBW=0.
         ELSE
           ABW=HSV/(2.*SINH(ARG))
           PSI = 4.02 * (ABW / TP) ** 2 / ((RHOS/RHOW -1) * D50)
!      print*,'abw,arg,d50,PSI',abw,arg,d50,psi
!IPK JUN02
!           if(rc /= 0) go to 100
!
!IPK MAR01 UPDATE TO DIVIDE BY 2 BASED ON NIELSEN 4/10/00
           ATEST1 = (21. * PSI ** (-1.85) * ABW)/2.
!      print*,'atest1',atest1
!
! For high wave energy conditions with a flat bed :
!
!IPK MAR01 UPDATE TO DIVIDE BY 2 BASED ON NIELSEN 4/10/00
           ATEST2 = 0.072 * ABW * (ABW / (3. * D90)) ** (-0.25)
!      print*,'atest2',atest2
!
           IF(PSI <= 12) THEN
!IPK MAR01 UPDATE TO DIVIDE BY 2 BASED ON NIELSEN 4/10/00
             A = (0.275 - 0.022 * SQRT(PSI)) * ABW/2.
           ELSE 
             IF(ATEST1 >= ATEST2) THEN
               A = ATEST1
             ELSE
               A = ATEST2
             ENDIF
           ENDIF
!
!
!              A  = ripple height
!              ABW = peak value of the near bed wave orbital excursion
!
!      print*,'a',a
!IPK MAR01 UPDATE TO MULTIPLY BY 6 BASED ON NIELSEN 4/10/00
!
           DS = 6. * A
!DJW
!             DS = 0.05 
!
!DJW  6 SEEMS TOO LARGE HERE
           RC = 6. * A
           RW = 6. * A
!
!ipk apr03
!           RC = A
!           RW = A
!
!
             RC = MAX(RC,0.2) 
!Allow it smaller at present
!             RW = MAX(RW,0.2)   
!  LATER TRY RW = MAX(RW, RC)   ???????????
!
!
!  DJW 14/04/03
!             
             DSCHK = (HD*0.5)
           RC = MIN(RC, DSCHK)
           RW = MIN(RW, DSCHK)
!           DS = RC
             DS = MIN(3*RW,DSCHK)
           A = RC
!
!
!
!
!
!           IF (DS > DSCHK) THEN
!                DS = 0.5 * HD
!              RW = 0.5 * HD
!           END IF
! DJW APR03
!             IF (RC > DSCHK) THEN
!                RC = MIN(DSCHK,0.1)
!           END IF
!DJW OVERRIDE OF LOGIC 210403
!             RC = MIN(DSCHK,0.1)
!             RW = MIN(A,DSCHK)
!           IF (A > DSCHK) THEN
!                  A = DSCHK
!             END IF
!             DS = A
!
!
!
!
!DJW             
!              RW = effective wave related bed roughness 
!              RC = effective current related bed roughness 
!              DS = mixing layer thickness
!IPK JUN02
           go to 120
  100      DS=MAX(RC,RW)
           A=DS
  120      CONTINUE
!
           FW=EXP(-6.+5.2*(ABW/RW)**(-0.19))
!
!               FW = friction factor
!
!      print*,'fw',fw
           IF(FW > 0.3 )FW=0.3
           UBW  = 2.*PI/TP*ABW
!          print*,'fw,ubw',fw,ubw
!                      
!        STOKES 2ND ORDER CORRECTIONS FOR WAVE-RELATED TRANSPORT.
!
           UCRECT=7.4*HSV*HSV/(TP*RLS*(SINH(6.28*HD/RLS))**4)
!
! ABMIN cannot go negative - this is due to waves on the verge
! of breaking
! prevent this from happening 
! CAA 9/5/00
!
           UBMAX=UBW+UCRECT
           UBMIN=UBW-UCRECT
!
           if(ubmin <= 0.) ubmin=0.0000001
!
           ABMAX=UBMAX*TP/6.283
           ABMIN=UBMIN*TP/6.283
!
           if(abmin <= 0.) abmin=0.0000001
!
!          print*,'ubmax,ubmin,abmax,abmin',ubmax,ubmin,abmax,abmin
           DELWMX=0.216*ABMAX/(ABMAX/RW)**0.25
           DELWMN=0.216*ABMIN/(ABMIN/RW)**0.25
!
!          print*,'delwmx,delwmn',delwmx,delwmn
         ENDIF
!        print*,'ok'
!         A=0.1
         if(A <= 0.0001) a=0.0001
!        print*,A
      ELSE
         A=0.00
         ABW=0.
         ABR=0.
         FW=0.
         UBW=0.
      ENDIF
!
!  CRITICAL SHEAR STRESS SHIELDS
!
      IF(DSTER <= 4.)THETCR=.24/DSTER
      IF(4. < DSTER .AND. DSTER <= 10.)THETCR=.14*DSTER**(-.64)
      IF(10. < DSTER .AND. DSTER <= 20.)THETCR=.04*DSTER**(-.1 )
      IF(20. < DSTER .AND. DSTER <= 150.)THETCR=.013*DSTER**(.29 )
      IF(DSTER > 150.)THETCR=.055
      TAUCR=(RHOS-RHOW)*G*D50*THETCR
!
!
!  COMPUTATION OF REFERENCE CONCENTRATION CA
!
      TOLD=0.0
  200 CONTINUE
!
!
      if(RC <= 0.001) RC=0.001
!      print*,RC
      CC=18.*ALOG10(12.*HD/RC)
      FC=0.24*ALOG10(12.*HD/RC)**(-2)
      FC1=0.24*ALOG10(12.*HD/3./D90)**(-2)
      RMUC=FC1/FC
      RMUW=0.6/DSTER
      UST=G**0.5*ABS(UGV)/CC
      DELS=0.
      IF(ABW > 0.)THEN
         DELS=0.216*ABW*(ABW/RW)**(-0.25)
      ENDIF
      HULP10=UBW/UGV
      GAMMA=0.
      IF(HULP10 >= 2.5)HULP10=2.5
      IF(0 <= PHI .AND. PHI <= 180.)THEN
         GAMMA=.75
         IF(PHI > 90.)GAMMA=GAMMA+(PHI-90.)/90.*.35
      ENDIF
      RA=EXP(GAMMA*HULP10)*RC
      FCA=0.24*ALOG10(12.*HD/RA)**(-2)
      IF(DELS <= RC/10.)THEN
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
!      write(112,'(i6,7g15.6)') node,thet1,thetcr,tauc,rmuc,alfaw,fca,ra
!
!     Get ripple height for no wave case
!
!      ***********************************************************
!
!      Code Commented by DJW 3/04/03 Routine Rewritten below to ensure
!      That RC & A are recalculated for all nodes, regardless of wave conditions
!      And that iteration occurs in calculating T.  Also, a routine for calculating
!      BETA interactively was added
!
!      ***********************************************************
!
      IF (TP <= 0. .OR. HSV <= 0.1) THEN
!IPK JUN02
!DJW APR03        IF(RC > 0.) GO TO 220
        IF(DSTER < 10.) THEN 
          IF(T <= 3.) THEN
!
!     Mini ripples
!
            A=150*D50
          ELSEIF(T <= 25.) THEN
!
!     Dunes
!     
            A= 0.11*(D50/HD)**0.3*(1.-EXP(-0.5*T))*(25.-T)*HD
              A= MAX(0.1,A)
          ELSE
!
!     Plane bed
!
!DJW APR03            A=0.002
!
              A = 0.02
! END DJW APR03
          ENDIF
        ELSE
!
!       Dunes
!
          A= 0.11*(D50/HD)**0.3*(1.-EXP(-0.5*T))*(25-T)*HD
          A= MAX(0.1,A)
!
        ENDIF
!DJW APR03
!        DS = 0.05
!        RW = 0.05
        DSCHK = (HD*0.5)
!        IF (DS > DSCHK) THEN
!          DS = 0.5 * HD
!          RW = 0.5 * HD
!        END IF
!
!DJW APR03      DS = 3. * A
!        RC = 3. * A
!DJW APR03
!        RW = 3. * A
!
!ipk APR03 
!        RC = A
!        RW = A
!DJW APR03
!        DS = A
!        IF (A > DSCHK) THEN
!          A = DSCHK
!            DS = DSCHK
!          RC = DSCHK
!            RW = DSCHK
!        END IF
!
          A = MIN(A, DSCHK)
          RC = MIN(1.5*A, DSCHK)
          RW = A
          DS = A
!
!        RW = MAX(RW,0.2)
!IPK JUN02
!****************************************************************
!DJW APR03
!****************************************************************
!        TC = (TAUCEF - TAUCR)/TAUCR
!        IF (TP <= 0. .OR. HSV <= 0.3) THEN
!            A= 0.11*(D50/HD)**0.3*(1.-EXP(-0.5*TC))*(25.-TC)*HD
!          IF (A < 0.02) THEN 
!            A = 0.02
!         END IF
!          RC = 3*A
!            DS = RW
!       END IF
!END DJW APRO3
!****************************************************************
        GO TO 240
  220   CONTINUE      
        DS=MAX(RC,RW)
        A=DS
  240   CONTINUE
        IF(ABS(TOLD-T) > 0.2) THEN
          TOLD=T
          GO TO 200
        ENDIF
      ENDIF
!****************************************************************
!
!DJW APR03 BETA CALCULATIONS 
!
!****************************************************************
!
      IF (HSV <= 0.1) THEN
        APPROXCHEZY = 18 * LOG10(12*HD/RC)
        USTAR = (UGV*(9.81**0.5))/APPROXCHEZY
        BETA = 1 + 2 * ((WS/USTAR)**2)
        IF (BETA > 2.0) THEN
          BETA = 2.0
        END IF
      END IF
!****************************************************************
!
!  ADJUSTS "A" DOWNWARDS TO ACCOUNT FOR MAJORITY OF LOAD BEING ALONG
!  THE BED
! 
!**************************************************************** 
!      A = 0.05
      CHK = 0.5*HD
      IF (DS > CHK) THEN
        DS = CHK
      END IF
      IF (A > CHK) THEN
        A = CHK
      END IF
      IF (DS < 0.001) THEN
        DS = 0.001
      END IF
      IF (A < 0.001) THEN
        A = 0.001
      END IF
!****************************************************************
!DJW APR03 SETS CONSTANT VALUE OF 0.1 FOR RW IF HSV > 0.1 ALSO
! CHECKS TO MAKE SURE IT IS NOT GREATER THAN THE WAVE HEIGHT
!****************************************************************
!      IF (HSV > 0.1) THEN
!        RW = 0.1
!      END IF
      CHK = 0.75 * HD
      IF (RW > CHK) THEN
        RW = CHK
      END IF
!****************************************************************
!END DJW APR03
!****************************************************************
!     print*,'ok'
      CA=0.015*D50/A*DSTER**(-.3)*T**1.5
!
!      FOR WAVE RELATED TRANSPORT
!
      TAUWMX=0.25*RHOW*FW*UBMAX*UBMAX
      TWMXEF=RMUW*TAUWMX
      TAUWMN=0.25*RHOW*FW*UBMIN*UBMIN
      TWMNEF=RMUW*TAUWMN
      TCWMAX=(TAUCEF*TAUCEF+TWMXEF*TWMXEF+2*TAUCEF*TWMXEF*COS(PHI*     &
     &PI/180.))**0.5
      TCWMIN=(TAUCEF*TAUCEF+TWMNEF*TWMNEF-2*TAUCEF*TWMNEF*COS(PHI*     &
     &PI/180.))**0.5
      THTMAX=TCWMAX/(RHOS-RHOW)/G/D50
      TMAX=(THTMAX-THETCR)/THETCR
      THTMIN=TCWMIN/(RHOS-RHOW)/G/D50
      TMIN=(THTMIN-THETCR)/THETCR
      TMIN=MAX(0.0001,TMIN)
      TMAX=MAX(0.0001,TMAX)
!
      CAMAX=0.015*D50/A*DSTER**(-0.3)*TMAX**1.5      
      CAMIN=0.015*D50/A*DSTER**(-0.3)*TMIN**1.5
!
!     CALCULATE WAVE-RELATED TRANSPORT
!   
      QWMAX=0.3*UBMAX*DELWMX*(CAMAX-CA)                
      QWMIN=0.3*UBMIN*DELWMN*(CA-CAMIN)
      QW=(QWMAX-QWMIN)/1000.
!                
! NUMERICAL INTEGRATION OF U*C OVER VERTICAL, TO STARTING POINT X=A
!
      JTAL= 8
      NN  = JTAL*NN
      DYM = CA/NN
      DXM = HD/NN
      DYX = DYM/DXM
      BO  = .FALSE.
      HULP30=-1.+ALOG(30.*HD/RA)
      IF(DELS > 0.)THEN
         UDEL=UGV*ALOG(30.*DELS/RA)/HULP30
      ENDIF
!
!  COMPUTATION OF DERIVATIVE DC/DY OR DC/DX
!
      ABR=MAX(3.*(HSV/HD)-.8,1.)
      EBW=.004*DSTER*ABR*DS*UBW
      IF (TP > 1.E-4) THEN
        EMAXW=0.035*ABR*HD*HSV/TP
      ELSE
        EMAXW=0.
      ENDIF
      IF(EMAXW <= EBW)EMAXW=EBW
      EMAXC=0.25*RKAP*UST*HD*BETA
      C=CA
      Z=A
      IF(Z <= DS)ESW=EBW
      IF(Z > DS .AND. Z <= 0.5*HD)ESW=EBW+(EMAXW-EBW)*((Z-DS)/          &
     &(0.5*HD-DS))
      IF(Z >= 0.5*HD)ESW=EMAXW
      IF(Z >= 0.5*HD)ESC=EMAXC
      IF(Z < 0.5*HD)ESC=EMAXC-EMAXC*(1.-2.*Z/HD)**2
      ES=(ESW**2.+ESC**2.)**0.5
      fcc=0.
      IF (C > 1.E-8) THEN
        IF(Z >= A)fcc=-WS/ES*C*(1.-C)**5.
      ENDIF
      YPRIME=fcc
      FF = 1./CA*YPRIME
      IF(DELS > 0.)THEN
         UC=UDEL*ALOG(30.*A/RC)/ALOG(30.*DELS/RC)
      ENDIF
!
      IF(A >= DELS)UC=UGV*ALOG(30.*A/RA)/HULP30
!
!  FURTHER INTEGRATION
!
      Y = CA
      TERM1=UC*Y
      XEND=A
      SS=0.
      NTEL = 0
      IT   = 2
!
  400 CONTINUE
      NTEL = NTEL+1
      XOLD = XEND
      YOLD = Y
      IF (-YPRIME > DYX) THEN
         Y = YOLD-DYM
         IF (Y < 2./3.*YOLD) Y = 2./3.*YOLD
         XEND = XOLD+ALOG(Y/YOLD)/FF
      ELSE
         XEND = XOLD+DXM
         IF (XEND >= HD) THEN
            XEND = HD
            BO = .TRUE.
         ENDIF
         Y = EXP(ALOG(YOLD)+(XEND-XOLD)*FF)
      ENDIF
      C=Y
      Z=XEND
      IF(Z <= DS)ESW=EBW
      IF(Z > DS .AND. Z <= 0.5*HD) ESW=EBW+(EMAXW-EBW)*((Z-DS)/         &
     &(0.5*HD-DS))
      IF(Z >= 0.5*HD)ESW=EMAXW
      IF(Z >= 0.5*HD)ESC=EMAXC
      IF(Z < 0.5*HD)ESC=EMAXC-EMAXC*(1.-2.*Z/HD)**2
      ES=(ESW**2.+ESC**2.)**0.5
      fcc=0.
      IF (C > 1.E-8) THEN
        IF(Z >= A)fcc=-WS/ES*C*(1.-C)**5.
      ENDIF
      YPRIME=fcc
      FF = 1./Y*YPRIME
      IF(DELS > 0.)THEN
         UC=UDEL*ALOG(30.*XEND/RC)/ALOG(30.*DELS/RC)
      ENDIF
      IF(XEND >= DELS)UC=UGV*ALOG(30.*XEND/RA)/HULP30
      TERM2=UC*Y
      SS=SS+(XEND-XOLD)*(TERM1+TERM2)/2.
      TERM1=TERM2
      IF (NTEL == NN/JTAL .OR. BO) THEN
         IT = IT+1
!
      ENDIF
      IF ( .NOT. BO) GOTO 400
      USTER1=SQRT(TAUCEF/RHOW)
      SBF=0.25*D50*USTER1*T**1.5/DSTER**0.3*RHOS/1000.
      SBD=0.25*D50*USTER1*T**1.5/DSTER**0.3
      SSR      = SS*RHOS/1000.
!
!      TOTAL CURRENT-RELATED SS AND BL TRANSPORT 
!              AND WAVE-RELATED TRANSPORT QW
!
!ipk jan03      QT=((SSR+SBF)**2+QW*QW+2*(SSR+SBF)*QW*COS(PHI*PI/180))**0.5              
      QT=SSR+SBF
!DJW APR03 ENGELUND & HANSEN OVERRIDE****************************
!      
!      QTENGHANS      :  Transport as calculated using Engelund & Hansen (kg/m/s)
!      HD                  :  Local Nodal Depth (m)      
!     CC                  :  Approximate Chezy Coefficient = 18log(12*HD/RC)
!      RHOS            :  Density of Sand (kg/cu.m)
!      RHOW            :  Density of Water (kg/cu.m)
!      SGSA            :  Specific Gravity of Sand 
!      UGV                  :  Local Velocity (m/s)
!      D50                  :  Median Sediment Grain Size (m)
!      
!
      ehNUMERATOR = 0.05 * (UGV**5)
      DENOMINATOR = (((RHOS/RHOW)-1)**2)*(9.81**(0.5))*D50*(CC**3)
! /1000 SEEMS CONSISTENT WITH IPK CODE
      QTENGHANS = RHOS * (ehNUMERATOR/DENOMINATOR)/(1000)
!
!      ENGULAND & HANSEN IS PROBABLY MORE APPROPRIATE FOR AREAS
!      WHERE CURRENT IS THE PRIMARY TRANSPORTING PROCESS
!     PARTICULARLY FOR SOFTER, UNCONSOLIDATED SEDIMENTS THEREFORE
!     OVERRIDE VAN RIJN IN AREAS WHERE ENGULAND HANSEN PREDICTS A HIGHER
!      RATE OF TRANSPORT.
!
!      if(NODE == 1850 .OR. NODE == 1991) then
!        wriTe(170,'(I6,5F17.4)') NODE,QT*1000.,QTENGHANS*1000.,UGV,HD,CC
!      endif
      IF (QTENGHANS > QT) THEN
        QT = QTENGHANS
      END IF
!****************************************************************
!END DJW APR03
!****************************************************************
      GP(1)=QT/(HD*UGV)
      TRLOC=QT*1000.
!
!      write(111,'(i6,6g15.6)') node,ca,t,ugv,hd,trloc,hsv
!
!IPK JUN02
      IF(abs(ISWT) /= 1) RETURN
!
      if(iswt == 1) then
!
      WRITE(75,'('' HD   = WATER DEPTH [ M ] =                     ''   &
     &,E10.4)') HD
      WRITE(75,'('' UGV  = MEAN CURRENT VELOCITY [ M/S ] =         ''   &
     &,E10.4)') UGV
      WRITE(75,'('' HSV  = SIGNIFICANT WAVE HEIGHT [ M ] =         ''   &
     &,E10.4)') HSV
      WRITE(75,'('' TP   = PEAK WAVE PERIOD        [ S ] =         ''   &
     &,E10.4)') TP
      WRITE(75,'('' PHI  = ANGLE CURRENT AND WAVES 0-180 [ DEG ]   ''   &
     &,E10.4)') PHI
      WRITE(75,'('' D50  = MEDIAN PARTICLE SIZE OF BED [ M ] =     ''   &
     &,E10.4)') D50
      WRITE(75,'('' D90  = 90 0/0 PARTICLE SIZE [ M ] =            ''   &
     &,E10.4)') D90
      WRITE(75,'('' WS   = FALL VELOCITY SUSP. SEDIMENT [ M/S ] =  ''   &
     &,E10.4)') WS
      WRITE(75,'('' RC   = CURRENT-RELATED ROUGHNESS [ M ] =       ''   &
     &,E10.4)') RC
      WRITE(75,'('' RW   = WAVE-RELATED ROUGHNESS [ M ] =          ''   &
     &,E10.4)') RW
      WRITE(75,'('' DS   = MIXING LAYER THICKNESS NEAR BED [ M ] = ''   &
     &,E10.4)') DS
      WRITE(75,'('' A    = REFERENCE LEVEL [ M ] =                 ''   &
     &,E10.4)') A
      WRITE(75,'('' RNU  = KINEMATIC VISCOSITY COEFFICIENT [ M ] = ''   &
     &,E10.4)') RNU
      WRITE(75,'('' RHOW = FLUID DENSITY [ KG/M3 ] =               ''   &
     &,E10.4)') RHOW
      WRITE(75,'('' RHOS = SEDIMENT DENSITY [ KG/M3 ] =            ''   &
     &,E10.4)') RHOS
      WRITE(75,'('' BETA = RATIO SEDIMENT AND FLUID MIXING [ - ] = ''   &
     &,E10.4)') BETA
!
!
      WRITE(75,'(A)')CHAR(12)
      WRITE(75,'('' PHYSICAL PARAMETERS :'',/)')
      WRITE(75,'('' DSTER = PARTICLE PARAMETER               [   -  ] ''&
     &,E10.4)')DSTER
      WRITE(75,'('' L     = WAVE LENGTH                      [  M   ] ''&
     &,E10.4)')RLS
      WRITE(75,'('' UBW   = PEAK ORBITAL VELOCITY            [ M/S  ] ''&
     &,E10.4)')UBW
      WRITE(75,'('' ABW   = PEAK ORBITAL EXCURSION AT BED    [  M   ] ''&
     &,E10.4)')ABW
      WRITE(75,'('' TAUW  = WAVE-RELATED BED-SHEAR STRESS    [ N/M2 ] ''&
     &,E10.4)')TAUW
      WRITE(75,'('' TAUC  = CURRENT-RELATED BED-SHEAR STRESS [ N/M2 ] ''&
     &,E10.4)')TAUC
      WRITE(75,'('' FW    = WAVE-RELATED FRICTION COEFFICIENT[  -   ] ''&
     &,E10.4)')FW
      WRITE(75,'('' FC    = CURR-RELATED FRICTION COEFFICIENT[  -   ] ''&
     &,E10.4)')FC
      WRITE(75,'('' C     = CHEZY COEFFICIENT                [M0.5/S] ''&
     &,E10.4)')CC
      WRITE(75,'('' RA    = APPARENT ROUGHNESS               [  M   ] ''&
     &,E10.4)')RA
      WRITE(75,'('' ALFAW = WAVE-CURRENT COEFFICIENT         [  -   ] ''&
     &,E10.4)')ALFAW
      WRITE(75,'('' TAUCR = CRITICAL BED-SHEAR STRESS        [ N/M2 ] ''&
     &,E10.4)')TAUCR
      WRITE(75,'('' UC    = CURR-RELATED EFFICIENCY FACTOR   [  -   ] ''&
     &,E10.4)')RMUC
      WRITE(75,'('' UW    = WAVE-RELATED EFFICIENCY FACTOR   [  -   ] ''&
     &,E10.4)')RMUW
      WRITE(75,'('' T     = BED-SHEAR STRESS PARAMETER       [  -   ] ''&
     &,E10.4)')T
      WRITE(75,'(///)')
      WRITE(75,'('' NUMERICAL  : SS=SUSPENDED LOAD TRANSPORT [ KG/SM]'' &
     &,E10.4)')SSR*1000.
!      WRITE(75,'('' FORMULA    : SS=SUSPENDED LOAD TRANSPORT [ KG/SM]''
!     *,E10.4)')SSF
      WRITE(75,'(''            : SB=BED LOAD TRANSPORT       [ KG/SM]'' &
     &,E10.4)')SBF*1000.
      else
!        write(109,'(i5,6f10.4,1f10.2)') 
!     +   node,trloc,qw*1000.,ssr*1000.,sbf*1000.,rc,rw,wdir*180./pi
      endif
!
      return
      end
