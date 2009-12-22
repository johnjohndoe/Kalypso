CIPK  LAST UPDATE DEC 1 UPDATE TO F90
crrr  last update Aug 30 1997
CIPK  FIRST ADDED JUNE 2 1997
      SUBROUTINE WSHEAR1
      USE BLK10MOD
      USE BLKSSTMOD
      USE BLKSEDMOD
      USE BLKSANMOD

C
      DATA PI/3.141592/TEMPMX/3./

      
C     SPWP(N)    Spectral peak wave period (hours)

C     AWL(N)     Average wave length (m)

C     SWH(N)     Significant wave height (m)

C     WVDR(N)    Wave direction measure counter clockwise from the x -axis.
C       This is the direction the waves are blowing to (deg)

C
C     Get kinematic viscosity
C
      CALL KINVIS

C
C     FOR EACH NODE CALCULATE SHEAR STRESS
C
      DO N=1,NP
	IF(AWL(N) .GT. 1.) THEN
	 BSHEAR(N)=0.
	 IF(WSLL(N) .GT.  AO(N)) THEN
	    
C
C     CALCULATE PEAK NEAR BED ORBITAL VELOCITY
C
        UPK=SWH(N)/2.*GRAV*SPWP(N)/AWL(N)
     +                 *1./(COSH(2.*PI*VEL(3,N)/AWL(N)))
C
C     CALCULATE FREE STREAM AMPLITUDE
C
        FSA=UPK*SPWP(N)/(2.*PI)
C
C     CALCULATE AMPLITUDE REYNOLDS NUMBER
C
        RA=UPK*FSA/XNU(N)
C
C     CALCULATE SIZE OF ROUGHNESS ELEMENTS
C
        XKN=3.*D90

        IF(RA .LT. 1.E6  .AND.  FSA/XKN .GT. 1000.) THEN
C
C     IF WAVE REGIME IS SMOOTH CALCULATE SMOOTH FRICTION FACTOR
C
!NiS,Nov06: Invalid operator without brackets at exponent
          XFW=0.035*RA**(-0.16)
!-
C
C     IF WAVE REGIME IS NOT SMOOTH CALCULATE ROUGH FRICTION FACTOR
C
        ELSE
!NiS,Nov06: Invalid operator without brackets at exponent
          XFW=0.04*(FSA/XKN)**(-0.25)
!-
	  ENDIF

C
C     CALCULATE WAVE RELATED BED SHEAR STRESS
C
        TBW=0.25*DEN(N)*XFW*UPK**2
C
C     DETERMINE CURRENT RELATED FRICTION FACTOR
C

        chz=VEL(3,N)**0.16667/XNMANN(N)
        XFC=12.*VEL(3,N)/10.**(chz/18.)
C
C     ADJUST CURRENT RELATED FRICTION FACTOR FOR WAVE EFFECTS
C
        CDIR=57.2958*ATAN2(VEL(1,N),VEL(2,N))
        IF(CDIR .LT. 0.) THEN
	    CDIR=CDIR+360.
        ENDIF
        IF(WVDR(N) .LT. 0.) THEN
	    WVDR(N)=WVDR(N)+360.
        ENDIF
	
	  IF(ABS(CDIR-WVDR(N)) .LT. 90.  .OR.
     +    ABS(CDIR-WVDR(N)) .GT. 270.) THEN
		GAMMA=0.75  
        ELSE
	    GAMMA=1.1
	  ENDIF
	  AVEV=SQRT(VEL(1,N)**2+VEL(2,N)**2)
        IF(AVEV .GT. .0) THEN
	    TEMP=GAMMA*UPK/AVEV
	    IF(TEMP .GT. TEMPMX) TEMP=TEMPMX
	    XFA=XFC*EXP(TEMP)
	  ELSE
	    XFA=XFC
	  ENDIF
C
C     CALCULATE SHEAR STRESS WITH ADJUSTED FRICTION FACTOR
C
        CCF=18.*LOG10(12.*VEL(3,N)/XFA)

	  TBA=1./CCF**2*DEN(N)*GRAV*(VEL(1,N)**2+VEL(2,N)**2)
C
C     SELECT SHEAR STRESS BASED ON MAXIMUM CACULATED   
C     FROM WAVE/CURR COMB OR WAVES ONLY
C
        BSHEAR(N)=MAX(TBW,TBA)
       ENDIF
       IF(N .EQ. 1)  WRITE(198,*) 'STARTING',ICYC,MAXN
	ENDIF
c       IF(N .eq. 6332) THEN
c 	  WRITE(198,'(I7,5F12.5,6F9.2)')
c     +N,BSHEAR(N),TBW,TBA,XFA,XFC,CHZ,CCF,VEL(3,N),AVEV,AO(N),WSLL(N)
c       ENDIF
	ENDDO

      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          BSHEAR(NN)=(BSHEAR(IMID(NN,1))+BSHEAR(IMID(NN,2)))/2.
          BSHEAR(N)=BSHEAR(NN)
        ENDIF
      ENDDO        

      RETURN
	END
