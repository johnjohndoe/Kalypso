!IPK  LAST UPDATE DEC 1 UPDATE TO F90
!rrr  last update Aug 30 1997
!IPK  FIRST ADDED JUNE 2 1997
      SUBROUTINE WSHEAR1
      USE BLK10MOD
      USE BLKSSTMOD
      USE BLKSEDMOD
      USE BLKSANMOD
!
!
      DATA PI/3.141592/TEMPMX/3./
!
!
!     SPWP(N)    Spectral peak wave period (hours)
!
!     AWL(N)     Average wave length (m)
!
!     SWH(N)     Significant wave height (m)
!
!     WVDR(N)    Wave direction measure counter clockwise from the x -axis.
!       This is the direction the waves are blowing to (deg)
!
!
!     Get kinematic viscosity
!
      CALL KINVIS
!
!
!     FOR EACH NODE CALCULATE SHEAR STRESS
!
      DO N=1,NP
      IF(AWL(N) > 1.) THEN
       BSHEAR(N)=0.
       IF(WSLL(N) > AO(N)) THEN
!
!
!     CALCULATE PEAK NEAR BED ORBITAL VELOCITY
!
        UPK=SWH(N)/2.*GRAV*SPWP(N)/AWL(N)                               &
     &                 *1./(COSH(2.*PI*VEL(3,N)/AWL(N)))
!
!     CALCULATE FREE STREAM AMPLITUDE
!
        FSA=UPK*SPWP(N)/(2.*PI)
!
!     CALCULATE AMPLITUDE REYNOLDS NUMBER
!
        RA=UPK*FSA/XNU(N)
!
!     CALCULATE SIZE OF ROUGHNESS ELEMENTS
!
        XKN=3.*D90
!
        IF(RA < 1.E6 .AND. FSA/XKN > 1000.) THEN
!
!     IF WAVE REGIME IS SMOOTH CALCULATE SMOOTH FRICTION FACTOR
!
!NiS,Nov06: Invalid operator without brackets at exponent
          XFW=0.035*RA**(-0.16)
!-
!
!     IF WAVE REGIME IS NOT SMOOTH CALCULATE ROUGH FRICTION FACTOR
!
        ELSE
!NiS,Nov06: Invalid operator without brackets at exponent
          XFW=0.04*(FSA/XKN)**(-0.25)
!-
        ENDIF
!
!
!     CALCULATE WAVE RELATED BED SHEAR STRESS
!
        TBW=0.25*DEN(N)*XFW*UPK**2
!
!     DETERMINE CURRENT RELATED FRICTION FACTOR
!
!
        chz=VEL(3,N)**0.16667/XNMANN(N)
        XFC=12.*VEL(3,N)/10.**(chz/18.)
!
!     ADJUST CURRENT RELATED FRICTION FACTOR FOR WAVE EFFECTS
!
        CDIR=57.2958*ATAN2(VEL(1,N),VEL(2,N))
        IF(CDIR < 0.) THEN
          CDIR=CDIR+360.
        ENDIF
        IF(WVDR(N) < 0.) THEN
          WVDR(N)=WVDR(N)+360.
        ENDIF
!
        IF(ABS(CDIR-WVDR(N)) < 90. .OR.                                 &
     &    ABS(CDIR-WVDR(N)) > 270.) THEN
            GAMMA=0.75  
        ELSE
          GAMMA=1.1
        ENDIF
        AVEV=SQRT(VEL(1,N)**2+VEL(2,N)**2)
        IF(AVEV > .0) THEN
          TEMP=GAMMA*UPK/AVEV
          IF(TEMP > TEMPMX) TEMP=TEMPMX
          XFA=XFC*EXP(TEMP)
        ELSE
          XFA=XFC
        ENDIF
!
!     CALCULATE SHEAR STRESS WITH ADJUSTED FRICTION FACTOR
!
        CCF=18.*LOG10(12.*VEL(3,N)/XFA)
!
        TBA=1./CCF**2*DEN(N)*GRAV*(VEL(1,N)**2+VEL(2,N)**2)
!
!     SELECT SHEAR STRESS BASED ON MAXIMUM CACULATED   
!     FROM WAVE/CURR COMB OR WAVES ONLY
!
        BSHEAR(N)=MAX(TBW,TBA)
       ENDIF
       IF(N == 1)  WRITE(198,*) 'STARTING',ICYC,MAXN
      ENDIF
!       IF(N == 6332) THEN
!         WRITE(198,'(I7,5F12.5,6F9.2)')
!     +N,BSHEAR(N),TBW,TBA,XFA,XFC,CHZ,CCF,VEL(3,N),AVEV,AO(N),WSLL(N)
!       ENDIF
      ENDDO
!
      DO N=1,NPM
        IF(IMID(N,1) > 0) THEN
          NN = N
          IF (NDEP(N) > 1) NN = NREF(N) + NDEP(N)-1
          BSHEAR(NN)=(BSHEAR(IMID(NN,1))+BSHEAR(IMID(NN,2)))/2.
          BSHEAR(N)=BSHEAR(NN)
        ENDIF
      ENDDO        
!
      RETURN
      END
