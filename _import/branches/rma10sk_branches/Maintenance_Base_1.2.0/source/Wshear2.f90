! IAT (WBM) version of wshear1
!
Subroutine Wshear2

USE BLK10MOD
USE BLKSSTMOD
USE BLKSEDMOD
USE BLKSANMOD

REAL::CRDR,PHICW

DATA PI/3.141592/TEMPMX/3./

      
! SPWP(N)	Spectral peak wave period (s)
! AWL(N)	Average wave length (m)
! SWH(N)	Significant wave height (m)
! WVDR(N)	Wave direction measure counter clockwise from the x -axis.
!			This is the direction the waves are blowing to (deg)

! FOR EACH NODE CALCULATE SHEAR STRESS
!
DO N=1,NP


	       RATIO=AWL(N)/VEL(3,N)
  IF (RATIO .GT. 0.01) THEN
!	Save current only bed shear stress in variable TBA
	TBA=BSHEAR(N)
!	Initialise wave only bed shear stress
	TBW=0.
	IF (WSLL(N) .GT. AO(N)) THEN
!		Make sure wave heights are realistic i.e. SWH/Depth < 0.8
!		IF (SWH(N) > VEL(3,N) * 0.8) SWH(N) = VEL(3,N) * 0.8
!ipk AUG06   replace 0.8 with breaker index
        XLO=1.56*SPWP(N)**2
        BI=0.16-0.28*ALOG10(SWH(N)/XLO)
        IF(BI .GT. 0.8) BI=0.8
		IF (SWH(N) > VEL(3,N) * BI) SWH(N) = VEL(3,N)*BI
!		Calculate peak near bed orbital velocity
		UPK=SWH(N)/2.*GRAV*SPWP(N)/AWL(N) &
			*1./(COSH(2.*PI*VEL(3,N)/AWL(N)))
!		Calculate free stream excursion amplitude

!ipk ADD THEN REMOVED        IF(VEL(3,N) .LT. 0.5  .AND. UPK .GT. 0.2) UPK=0.2

		FSA=UPK*SPWP(N)/(2.*PI)
		IF (FSA .LT. 0.001) CYCLE
!		Calculate amplitude Reynolds number
		RA=UPK*FSA/XNU(N)
!		Calcualte size of roughness elements
		XKN=2.5*D90
!		If wave boundary layer turbulence regime is smooth calculate smooth friction factor
        IF(RA .LT. 1.E6  .AND.  FSA/XKN .GT. 1000.) THEN
!NiS,Nov06: Invalid operator without brackets for exponent
			XFW=0.035*RA**(-0.16)
!-
!		Else if wave regime is rough calculate rough friction factor (Swart formula)
		ELSE
			XFW=EXP(5.21*(XKN/FSA)**0.194-5.98)
		END IF
!		Calculate peak wave related bed shear stress
		TBW=0.5*GAW*XFW*UPK**2
!		Adjust current related shear stress for wave interaction effects using Soulsby (1997)
		TBA=TBA*(1.+1.2*(TBW/(TBA+TBW))**3.2)
!		Calculate angle between waves and current
		CRDR = ATAN2(VEL(2,N),VEL(1,N))
		PHICW = WVDR(N) - CRDR
!		Calculate peak bed shear stress from combined wave & current flows
		BSHEAR(N)=SQRT((TBA+TBW*COS(PHICW))**2+(TBW*SIN(PHICW))**2)
	END IF
  ENDIF
  
9000 CONTINUE  
  if(it .eq. 1) then
    esro(N)=bshear(N)
  endif
  
END DO
      DO N=1,NPM
        IF(IMID(N,1) .GT. 0) THEN
          NN = N
          IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
          BSHEAR(NN)=(BSHEAR(IMID(NN,1))+BSHEAR(IMID(NN,2)))/2.
          BSHEAR(N)=BSHEAR(NN)
          if(it .eq. 1) then
	 	    esro(N)=bshear(N)
		  endif
        ENDIF
!cc        IF(BSHEAR(N) .GT. 0.2) BSHEAR(N)=0.2
      ENDDO
RETURN
END
