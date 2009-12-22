C     Last change:  MD   22 Oct 2008   12:30 pm
CIPK LAST UPDATE MAR 07 2006 fix gpmax setup
CIPK LAST UPDATE MAR 05 2006 UPDATE TO ALLOW FOR NODAL VALUES
CIPK  LAST UPDATE AUG 09 2005 ADD bshear
C
C------------------------------------------------------------
C
      SUBROUTINE BEDXCG
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSANMOD
      !MD neu USe BLKSEDMOD
      USE BLKSEDMOD
C
C  evaluate sedimentation or erosion at the bed based upon the
C  difference between the actual and equilibrium (carrying)
C  concentrations of sediment.
C

      character*4 KPO(7)
      DATA ITB/3/
      DATA KPO(1)/'NODE'/,KPO(2)/'BED '/,KPO(3)/'ELEV'/,KPO(4)/'ALPH'/,
     * KPO(5)/'A1  '/,KPO(6)/'ALPH'/,KPO(7)/'A2  '/
C        NOTE.  THE SOURCE TERM IS  +  IF EROSION, - IF DEPOSITION
C
      CDT=0.5*DELT
      POSA=.4
C     RHOF=1000.
      CONP=1.-POSA
CIPK MAR06 DELETED    SGPT=1000.*SGSA
CIPK MAR06 MOVED      RHOB=RHOF*CONP*SGSA
      CODT=DELT*(1.-ALPHASN)
CIPK MAR06 DELETED    COEFF=CDT/(CONP*RHOF*SGSA)
 1030 CONTINUE

C --------------------------------------------------------
C     CHECK TO SEE IF WE HAVE MULTILAYER PROBLEM
C --------------------------------------------------------
      IF(NP .GT. NPM) THEN
C
C     SETUP SETTLING RATE
C
        DO N=1,NP
	  IF(IBNA(N) .GT. 0  .and.  WSLL(N) .GT. AO(N)) THEN
CIPK MAR06            
            IF(VSANDND(N) .GT. 1E-20)THEN
              CST=CLDEND(N)*VEL(3,N)/VSANDND(N)
            ELSE
              CST=0.
            ENDIF
            IF(CST.LT.DELT) CST=DELT
            ALPHA1(N)=-VEL(3,N)/CST
            ALPHA2(N)=0.
	  ELSE
            ALPHA1(N)=0.
            ALPHA2(N)=0.
	  ENDIF
        ENDDO
      ENDIF

C --------------------------------------------------------
C     NOW PROCESS BOTTOM OR FOR 2DH
C --------------------------------------------------------
      DO 1130 NN=1,NPM
	IF(IBNA(NN).GT.0  .and.  WSLL(NN).GT.AO(NN)) THEN
          gpbsav(nn)=gpb(nn,1)   ! Save Geschiebetransport VEL(7,N)
          N=NN


          IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1
cipk apr99      IF(WD(N) .GT. DSET) THEN
          GPBSAV(N)=GPB(N,1)
          RHOB=RHOF*CONP*SGSAND(N)
        
          if(WSLL(n) .gt. ao(n)) then
CIPK MAR06
            VSN=VSANDND(N)
            AVU=SQRT(VEL(1,N)**2+VEL(2,N)**2)
            D=VEL(3,N)
            COEF=D/CONP
            DO 1120 I=IGS,LGS
              DELSAC=(VEL(6,N)-GPB(N,I))/1000.

c	      DELSAC is concentration difference kg/m3 ve means deposition
C             --> NOTE. DEPOSITION IS OCCURRING. ------------------------------
              !MD: Wenn Geschiebe-C gpb() groesser als Schwebstoff-C vel(6)
              IF(DELSAC .GE. 0.) THEN
C
C               IF(VS(N).GT.1E-20)THEN
C               CST=CLDE*D/VS(N)
                IF(VSN .GT. 1E-20)THEN
CIPK JAN03        CST=CLDE*D/VSN
CDJW APR03        CST=D/VSN
C		  CST = (0.15*D)/VSN
CDJW MAY03
	          !MDMD_test  CST = (0.25*D)/VSN
                  CST=CLDEND(N)*VEL(3,N)/VSANDND(N)
CEND DJWAPR03
                ELSE
                  CST=0.
                ENDIF
Cipk jan03        NOTE.  SOURCE TERM = (GP - CONC)/CST
CIPK JAN03              IF(CST.LT.DELT) CST=DELT
CIPK MAR06
                !MDMD  CST = IF(CST.LT.CLDEND(N)*3600.) CST=CLDEND(N)*3600.
                !MDMD  DELT ist schon sekunden!!

                !MDMD_neu  !MDMD_neu
                IF (CST.LE.0.0) THEN   !MDMD_neu
                  ALPHA1(N)=0.0
                  ALPHA2(N)=0.0
	          DEP=0.0
                Else                 !Ende MDMD_neu
                  ALPHA1(N)=-VEL(3,N)/CST
                  ALPHA2(N)=-GPB(N,I)*ALPHA1(N)
	          DEP=ALPHA1(N)*VEL(6,N)+ALPHA2(N)
                END IF

                !MD Vergleiche Geschiebe gpb() mit Schwebstoff vel(6)
	        if(gpb(n,i) .eq. 0.) then
	          beta=1.0
	        else
	          if(vel(6,n) .gt. 0) then
	            ratio=gpb(n,i)/vel(6,n)
	          else
	            ratio=1.0
	          endif
                  BETA=1.0-ratio**0.2
	        endif
	        EYSRAT=BETA*CAI(N)*VSN

!MD                !MD!MD: Anfang der Testausgabe
!MD                IF (n==1) then
!MD                 Write(LOUT,*) 'Ausgabe aus BEDXCG: DEPO'
!MD                 write(LOUT,*) '    N,   DEP,            vel(6,n),
!MD     +      gpb(n),       alpha1(n),
!MD     +      alpha2(n),     XNU(n),      CST,        BSHEAR(N)'
!MD                Endif

!MD	        WRITE(LOUT,'(I6,8G15.5)')
!MD     +            N,DEP,vel(6,n),gpb(n,i),alpha1(n),alpha2(n),XNU(n),
!MD     +            CST, BSHEAR(N)
!MD                !MD!MD: Ende der Testausgabe


C
C             --> SCOUR IS OCURRING.  CHANGE THE SIGN TO COMPUTE TSACB.
              !MD: Wenn Geschiebe-C gpb() kleiner als Schwebstoff-C vel(6)
              ELSE   !MD: Wenn DELSAC < 0    ---------------------------------
c	 -'ve means erosion

cipk jan98 check for zero velocity
                if(avu .lt. 1e-20) then
	          cst=delt
                else
CIPK MAR06              
                  CST=CLERND(N)*VEL(3,N)/AVU
                endif
CIPK MAR06             
                PORSD=SDND(1,N)*CONP/1000.
c	        PORSD is effective diameter of pore space
                RTO=DELT/CST
                IF(RTO.GT.1.)RTO=1.

c	        RTO is apparent change rate over time step
c               forced to be not more tham 1

                TSACB=DELSAC*RTO
c	        TCSACB is predicted change
                DYBED=-VEL(3,N)*TSACB/(RHOB-VEL(6,N)/1000.+TSACB)
c		DYBED is change in bed thickness allowing
c               suspended material in water column

                IF(TTHICK(N)-PORSD-DYBED .LT. 0.) THEN
c		This is the case of bed being exhausted

                  DYBED=0.
                  IF(VEL(6,N).GT.0.)
     .              DYBED=TTHICK(N)-PORSD*(VEL(6,N)/GPB(N,I))**0.3333
c		  Assume the material goes into suspesion only

                  IF(DYBED.LE.0.) THEN
c		  No material for that set rates to zero
                    DELSAC=0.
                    alpha1(n)=0.
                    alpha2(n)=0.
                    go to 1120
                  ELSE
                    DELSAC=-DYBED*(RHOB-VEL(6,N)/1000.)/
     +                                          ((D+DYBED)*RTO)
c		    DELSAC is recomputed to just set bed to zero
                  ENDIF
                  GPB(N,I)=VEL(6,N)-DELSAC*1000.
c                 GPB reset to be the a new effective equilibrium
                ENDIF
Cipk jan03      NOTE.  SOURCE TERM = (GP - CONC)/CST
cipk jan03         IF(CST.LT.DELT) CST=DELT
cipk jan03     Use CLER as a resuspension time in hours
cccc              IF(CST .LT. DELT/5.) CST=DELT/5.
CIPK MAR06
                !MDMD  CST= CLERND(N)*3600.
                !MDMD  DELT ist schon sekunden!!
                ALPHA1(N)=-VEL(3,N)/CST
                ALPHA2(N)=-GPB(N,I)*ALPHA1(N)
c	        ALPHA1 is rate term
c               ALPHA1*SED+ALPHA2 is the total source

!MD  MD: Testausgabe - TEST
!MD                IF (n==1) then
!MD                   Write(LOUT,*) 'Ausgabe aus BEDXCG: DEPO'
!MD                   write(LOUT,*) '    N,   DEP,      vel(6,n),
!MD     +    gpb(n),       alpha1(n),
!MD     +      alpha2(n),            BSHEAR(N)'
!MD                Endif
!MD	        WRITE(LOUT,'(I6,8G15.5)')
!MD     +            N,DEP,vel(6,n),gpb(n,i),alpha1(n),alpha2(n),BSHEAR(N)
!MD  MD: Testausgabe - TEST


C     NOTE.DEPTH OF SEDIMENT LESS THAN REQUIRED TO SATISFY TRANSPORT
C     CAPACITY.  REDUCE THE TRANSPORT CAPACITY.
              ENDIF
C        NOTE.  SOURCE TERM = (GP - CONC)/CST
cipk jan03    IF(CST.LT.DELT) CST=DELT
cipk jan03    ALPHA1(N)=-VEL(3,N)/CST
cipk jan03    ALPHA2(N)=-GPB(N,I)*ALPHA1(N)
 1120       CONTINUE

          else
            alpha1(n)=0.
            alpha2(n)=0.
          ENDIF
	ELSE
          alpha1(nn)=0.
          alpha2(nn)=0.
	ENDIF
 1130 CONTINUE

cipk experimental april 1999 set distribution linear
cipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
        if(imat(n) .gt. 0  ) then
          if(imat(n) .lt. 1000  .and.  ncorn(n) .lt. 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 2) then
            ncn=ncorn(n)
          endif
          if(ncn .gt. 0) then
            if(ncn .eq. 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              alpha1(nop(n,j))=(alpha1(nop(n,j1))+alpha1(nop(n,j2)))/2.
              alpha2(nop(n,j))=(alpha2(nop(n,j1))+alpha2(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 

      RETURN
      END
! -------------------------------------------------------------------------



! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
      SUBROUTINE SANDX
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKTSMOD
	!      
	!  DJW 19/01/04 Include WBM Modules
	!
	USE WBMMODS
	!
	!  End of DJW Changes 19/01/04
	!
C
C  this subroutine will calculate the sediment carrying capacity 
C  as kg of sediment per m3 of water.
C
C  ismode = 1   use Ackers-White (1973) method
C  ismode = 2   use van Rijn (1993) method
C  ismode = 3   use Brownlie (1981) method
c  ismode = 4   use updated van Rijn method
C
	!
	!  DJW 19/01/04 Variable Declarations
	!
	LOGICAL WBM
	INTEGER*4 AD
      !
      !  End of DJW Changes 19/01/04
      !
C      integer ismode
C
      DATA IAW/5/
C
      WBM = .FALSE. ! DJW 19/01/04 Sets WBM Modifications in motion
      IRR=1
C
C  POSA is the porosity of the sediment when
C  on the bed. Typically POSA = 0.4
C
      CONP=1.-POSA
C
C  note that PPMMAX is not actually in ppm but, rather, is the
C  concentration in kg of sediment per kg of fluid. 
C
CIPK MAR06 MOVED      PPMMAX=SGSA*CONP/(POSA+CONP*SGSA)
C
      DO NN=1,NPM     ! Ueber alle Knoten
CIPK DEC02	 if(ibna(nn) .gt. 0  .and.  WSLL(NN) .GT. AO(NN)) then
      ! IF (NN.eq.6) THEN
      !   WRITE(*,*) 'Kennung Rauheitsklasse je Knoten: IBNA', ibna(nn)
      !   WRITE(*,*) 'WSLL(NN): ', WSLL(NN)
      !   WRITE(*,*) 'A0(NN): ', AO(NN)
      !   WRITE(*,*) 'Check: WSLL(NN) .GT. AO(NN) am Knoten?????'
      !   WRITE(*,*) 'Wasserstand am Knoten 6: VEL(3,NN)', VEL(3,NN)
      !   WRITE(*,*) 'DSET am Knoten 6: ', DSET
      !   WRITE(*,*) 'Check: VEL(3,N) .le. DSET am Knoten?????'
      ! END IF


!MD    Falsche Abfrage: Knoten sollen Nass sein!!
!MD       if(ibna(nn) .gt. 0  .and.  (WSLL(NN) .GT. AO(NN) .OR.
!MD   +     VEL(3,NN) .LE. DSET)) THEN
        if(ibna(nn).gt.0  .and.  WSLL(NN).GT.AO(NN)) THEN
          N=NN
          IF(NDEP(NN) .GT. 1) N=NREF(NN)+NDEP(NN)-1

CIPK MAR06
          PPMMAX=SGSAND(N)*CONP/(POSA+CONP*SGSAND(N))

cipk dec02 IF(VEL(3,N) .LE. DSET)GOTO 1130
C          D35=EFDT(N)
CIPK MAR06
          D35=SDND(1,NN)
          IRR=N
          EFD=VEL(3,NN)
          EXNU=XNU(NN)
          USTAR=UST(N)

cipk dec02  experiment with smoothing around nodes

c       if(n .eq. 3974  .or.  n .eq. 3981  .or.  n .eq. 3980) then
c	  idebg=1
c	endif
          divide=0.
	  vxl=0.
	  vyl=0.
	  do j=1,np
	    idone(j)=0
    	  enddo
          do k=1,12
  	    neadj=elton(n,k)
  	    if(neadj .ne. 0) then
  	      do j=1,ncorn(neadj)
	        nadj=nop(neadj,j)
	        if(idone(nadj) .eq. 0) then
	          vxl=vxl+vel(1,nadj)
	          vyl=vyl+vel(2,nadj)
	          idone(nadj)=1
	          divide=divide+1.
  	        endif
	      enddo
            endif
          enddo
	  IF(DIVIDE .GT. 0.) THEN
            VXL=VXL/DIVIDE
	    VYL=VYL/DIVIDE
	  ENDIF
          VELN=SQRT(VEL(1,N)**2+VEL(2,N)**2)
          VELS=SQRT(VXL**2+VYL**2)
c         if(n .eq. 3974  .or.  n .eq. 3981  .or.  n .eq. 3980) then
c	    write(166,*) n,veln,vels
c	  endif

CIPK JUN00 ADD COMPUTATION OF CURRENT DIRECTION FOR NEW OPTION

          IF(VELS .GT. 0.) THEN
C           CDIR=ATAN2(VEL(2,N),VEL(1,N))
            CDIR=ATAN2(VYL,VXL)
          ELSE
            CDIR=0.
          ENDIF

          HSV= WAVEHT(N)
          TP=  PEAKPRD(N)
          WVDIR=WAVEDR(N)
	  IF(HSV .GT. 0.7*EFD) HSV=0.7*EFD

          ISUBTR=1
C
C          WRITE(LOUT,1100)N,D35,VELS,EFD,USTAR,AVRRO,OUTNU,AVCON
C 1100     FORMAT(//,' PARAMETERS FOR SAND TRANSPORT POTENTIAL',/,
C       * 5X,' NN    D35     VELOCITY     DEPTH          U*     RHO    ',
C       * '     NU(10**5)      CONC.',/,
C       * 5X,I3,F8.6,5F12.4,F12.6)
C
C  use the method of choice for sediment transport
C
CIPK MAR06

!MD neu: 29.07.2008
!MD calculation of FAKT_KN for each node based on lamdbaTot for each element
          IF (NN.eq.1) THEN
            WRITE(75,*) 'Friction factors are calculated for all NODES'
            call GET_FFACT
          endif
!MD neu: 29.07.2008  (nur einmal GET_FFACT aufrufen!!)

          IF(ITSTMOD .EQ. 1) THEN
            vels=TSTVAR(1)
          ENDIF
          if(vels .gt. 0.00001) then
cipk mar06  add node nunbers to calls below
            ISMODE=ISMODEND(NN)

	    if ( ismode.eq.1 ) then
              !MD: Acker-White-Formel fuer Geschiebetransport
              call awhite(VELS,EFD,EXNU,ustar,nn)

	    else if ( ismode.eq.2 ) then
              !MD: Van-Rijn-Formel fuer Geschiebe- und Schwebstofftransport
              call vanrijn(VELS,EFD,EXNU,ustar,nn)
c             IF(NN .EQ. 21) THEN
c       	WRITE(112,*) MAXN,NN,VELS,EFD,EXNU,USTAR,GP(1)*1.E6
c	      ENDIF
	    else if ( ismode.eq.3 ) then
              !MD: Brownlie-Formel fuer Geschiebetransport
              call brownlie(VELS,EFD,EXNU,ustar,nn)

cipk jun00 add new call for Nielsen option
	    else if ( ismode.eq.4 ) then
            !MD: Geschiebe mit Wellen nach Engel-und Hansen
	      RC=RCAN(NN)
	      RW=RWAN(NN)
              iswtt = -1
cc	      if(nn .eq. 4361) iswtt=1
	!     DJW 19/01/04 : Ensuring that mannings roughness modification variables are initiated before
	!     wavecm is called
	!
	      IF (WBM) THEN
!NiS,Nov06:   Mixing logical type with arithmetic is not possible in Lahey
!	      IF (wbm_Initiated.EQ..FALSE.) THEN
                IF (.not.wbm_Initiated) THEN
!-
                  CALL BedRoughInitiate(NP,wbm_Initiated,wbm_MannTrans,
     +          wbm_NodeCounter,wbm_IT,wbm_MannTransOld, wbm_BedHeight)
 	        END IF
              END IF
	!     End DJW 19/01/04
CIPK MAR06  
              SGSA=SGSAND(NN)
              D50=D50ND(NN)
              D90=D90ND(NN)
              VSAND=VSANDND(NN)
              call wavecm (hsv,tp,WVDIR,VELS,CDIR,D50,D90,EXNU,
     +          SGSA,RHOF,EFD,GP,VSAND,USTAR,
     +          RC,RW,TRLOC,CA,ISWTT,nn)
     +!         RC,RW,TRLOC,CA,ISWTT,nn,RWFACT,RWMIN) ! djw 19/01/04 includes passing of roughnesses

            
	    else if ( ismode.eq.5 ) then
            !MD: Geschiebe- und Schwebstofftransport mit Wellen
	      RC=RCAN(NN)
	      RW=RWAN(NN)
!MD  Unsinn --> kann raus
      	      IF(NN.EQ.1249 .or. nn.eq.1586 .or. nn.eq.1615
     +                    .or. nn.eq.1265) THEN
	        ISWTT=2
	      ELSE
	        ISWTT=0
	      ENDIF
!MD Ende Unsinn --> kann raus
              iswtt = -1
CIPK MAR06
              SGSA=SGSAND(NN)
              D50=D50ND(NN)
              D90=D90ND(NN)
              VSAND=VSANDND(NN)
              call TRANSPOR(hsv,tp,WVDIR,VELS,CDIR,D50,D90,EXNU,
     +           SGSA,RHOF,EFD,GP,VSAND,USTAR,
     +           RC,RW,TRLOC,ISWTT,nn)
	       if(iswtt .eq. 2) then
	        write(75,*) 'back from transpor',nn,vels,efd,gp(1)
	      endif
	    else
	      write(*,'(/,'' error in specifying smode '')')
	      stop
	    end if
            I=1

!MD: Auswertung aller Ergebnisse aus den einzelnen Geschiebe-Transport-Berechnungen
! --------------------------------------------------------------------------------
C  now multiply concentration (1kg(sand)/1kg(of water) = Mg/m3) by 1000000 to get g/m3 or mg/l
C
c at the end of rma-sandx ust(n) is in metric => assign in subroutines
c ackwht, brownlie, etc.

            UST(N)= USTAR
CIPK AUG05 ADD bshear
	    BSHEAR(N)=RHOF * (USTAR**2.0)
            GPB(N,I)=GP(I) * (10.0**6.0)
cipk mar06
            GPMAX=GPMAXND(N)
	    IF(GPB(N,I) .GT. GPMAX) GPB(N,I)=GPMAX
	    TRRAT(N)=TRLOC
	    CAI(N)=CA*1000.
	  else
	    ust(n)=0.
CIPK AUG05  ADD bshear
	    BSHEAR(N)=0.
	    gpb(n,1)=0.
	    TRRAT(N)=0.
	    CAI(N)=0.
	  endif

 1130     CONTINUE
        else
cipk mar03 change n to nn
	  ust(nn)=0.
	  gpb(nn,1)=0.
          TRRAT(NN)=0.
	  CAI(NN)=0.
        endif
      end do


      !  -------------------------------------
      !  DJW 19/01/04 : Calling Routines that Modify the Roughness Based on Roughness Characteristics
      !  Also calls routine to initiate the calculation
      If (WBM) Then

!NiS,Nov06: Mixing Logical type with arithmetic is not possible in Lahey
!       IF (wbm_Initiated.EQ..FALSE.) THEN
        IF (.not.wbm_Initiated) THEN
          CALL BedRoughInitiate(NP,wbm_Initiated,wbm_MannTrans,wbm_Node
     +	    Counter,wbm_IT, wbm_MannTransOld, wbm_BedHeight)
	END IF
        Call NewRough(ZMANN,NOP,NE,wbm_MannTrans,wbm_IT,IT,ICYC,MAXN,NP)
      End If
      !
      !  End DJW Changes 19/01/04

cipk experimental april 1999 set distribution linear
cipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
        if(imat(n) .gt. 0  ) then
          if(imat(n) .lt. 1000  .and.  ncorn(n) .lt. 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 .eq. 1  .or.  imat(n)/1000 .eq. 2) then
            ncn=ncorn(n)
          endif
          if(ncn .gt. 0) then
            if(ncn .eq. 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              GPB(nop(n,j),1)=(GPB(nop(n,j1),1)+GPB(nop(n,j2),1))/2.
              TRRAT(nop(n,j))=(TRRAT(nop(n,j1))+TRRAT(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 
cipk may02
CIPK TEMP***********************************************************
C      DO N=1,21
C	GPB(N,1)=20.+(N-1)*5
C	ENDDO
ccc      DO N=2,20,2
ccc	GPB(N,1)=(GPB(N-1,1)+GPB(N+1,1))/2.
ccc	ENDDO
CIPK TEMP***********************************************************

c     copy potential into 7th position and get time derivative

      DO n=1,np
C       VEL(7,N)=GPB(N,1)
        if(lbed .GT. 0) then
C	  vel(7,n)=gpb(n,1)

          IF(MAXN .EQ. 0) THEN
	    VOLD(7,N)=GPB(N,1)
            VDOTO(7,N)=0.
	    ENDIF

          IF(MAXN .EQ. 1) THEN
            VOLD(7,N)=VEL(7,N)
	    VDOTO(7,N)=VDOT(7,N)
	    ENDIF
          VEL(7,N)=GPB(N,1)
          VDOT(7,n)=ALTM*(VEL(7,n)-VOLD(7,n))-VDOTO(7,n)*(ALPHA-1.)

          if(delt .gt. 0) then
	    VDOT(7,N)=(VEL(7,N)-VOLD(7,N))/DELT
	    else
	    vdot(7,n)=0.
	    endif

          IF (n==1) then
            write(LOUT,*) 'Ausgabe aus SANDX:'
            write(LOUT,*) 'Belegung von vel(7,n), vold(7,n), vdot(7,n)'
          Endif

	  ELSE
	  VEL(7,N)=GPB(N,1)
	  endif
      enddo
C
      RETURN
      END
! -------------------------------------------------------------------------



! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
CIPK MAR06 ADD NODE NUMBER TO CALL
	subroutine awhite (VELS,EFD,EXNU,u_star,NODNUM)
c wlp u_star not being passed back
C
C  sediment transport by Ackers-White method
C        REF = HY 118 ASCE, NOV73, PP 2041-2021.
C
C  David Luketina   26/5/96
C
C  input variables are:
C  
C  VELS		mean velocity
C  EFD		depth
C  D35		d35 size (m) of sediment mixture
C  EXNU		kinematic viscosity (m2/s)
C
      USE BLKSANMOD

	integer i,ie,is,loops,j
      real gravity,sed_rd,root32,diam,vels,efd
	real depth,viscosity,ten,u_bar,awa,awn,awm,awc
	real part_a,part_b,c_bar,exnu,gnsg,clog,dgr,dgrlog
	real stress_norm,fgr,u_star,gptot,gd35
	real tmpsd
	logical first_pass
C
        data ten/10./
C
	gravity = ACGR
CIPK MAR06
	sed_rd = SGSAND(NODNUM)
	viscosity = EXNU
	depth = EFD
	u_bar = VELS
C
C  start calculations
C
	root32 = SQRT(32.0)
	gnsg = (sed_rd - 1) * gravity
C
C  the outside loop has either one or two loops.
C
C  if there is only one grain size there will be a single
C  outside loop.
C
C  if there is more than one grain size, the first pass of the 
C  outside loop will handle the d35 case and the other (second)
C  pass will handle the other grain sizes (from array pointer
C  IGS to LGS).
C
C  the inside loop, loops once for each grain size (obviously,
C  in the d35 case there can only be a single inner loop).
C
C  set parameters for the first pass - d35 case
C
	first_pass = .true.
	gptot = 0.0
CIPK MAR06
	tmpsd = SDND(1,NODNUM)
	SDND(1,NODNUM) = D35ND(NODNUM)
	is = 1
	ie = 1
C
	if ( igs.eq.lgs ) then
	  loops = 1
	else
	  loops = 2
	end if
C
        i = 0
        do j = 1, loops
          do i = is, ie  ! ueber alle Bodenlayer
CIPK MAR06
            diam=SDND(i,NODNUM)/1000.
c           wlp Remember diameter is in mm!!
C
C           find non-dimensional grain size
            dgr = diam *(gnsg**(1./3.))/ (viscosity**(2./3.))
            dgrlog = ALOG10(dgr)
C
C           now find ackers-white coefficients

!MD	    if (dgr.gt.60.) then
!MD            awn = 0.
!MD            awa = 0.17
!MD            !MD awm = 1.50  alter Ansatz nach 1973
!MD            awm = 1.78
!MD            awc = 0.025

!MD  Neuer Ansatz fuer dgr > 60 mit glattem Ubergang zu den
!MD  Funktionen groesser und kleiner 60
	    if (dgr.gt.60.0) then
              IF (dgr .lt. (10.0**(1.0/0.56))) THEN
                awn = 1.00 - (0.56 * ALOG10(dgr))
              Else
                awn = 0.00
              END IF
              awa = (0.23/SQRT(60.0)) +0.14
              awm = (6.83/60.0) +1.67
              clog = (2.79*ALOG10(60.0))-(0.98*(ALOG10(60.0)**2.0))-3.46
              awc = 10.**clog

C  find non-dimensional grain size
	    else if (dgr.le.60.0) then
C             find n coefficient (equation 16)
              awn = 1.00 - (0.56*dgrlog)
C
C             find a coefficient (equation 17)
              awa = (0.23/SQRT(dgr)) +0.14
C
cipk Aug01 experimental addition of modified Ackers White coefficients
C             find m coefficient (equation 20)
              awm = (6.83/dgr) +1.67
C
C             find c coefficient (equation 22)
              !MD  clog = 2.79*dgrlog-0.98*dgrlog-3.46
              !MD  11.08.2008: Formel korrigiert!!
              clog = (2.79*dgrlog) - (0.98*(dgrlog**2.0)) - 3.46
C
cipk Aug01 below are dropped coefficients
C             find m coefficient (equation 20)
c             awm = 9.66/dgr+1.34
C             find c coefficient (equation 22)
c             clog = 2.86*dgrlog-dgrlog**2.0-3.53
              awc = 10.**clog

              IF (dgr.le.1) THEN
                write(75,*) ' Warnung aus Acker-White:'
                write(75,*) ' Gueltigkeitsbereich fuer der Gelichung'
                write(75,*) '  von dgr > 1 wurde unterschritten!'
              END IF
	    end if


C  get u_star via Manning's equation
CIPK MAR06 ADD NODNUM TO CALL
	    call get_u_star(depth,u_star,u_bar,NODNUM)
C
C  determine mobility number
	    part_a = u_star**awn / SQRT(diam*gnsg)
	    part_b = u_bar / (root32*ALOG10(10.0*depth/diam))
	    fgr = part_a * part_b**(1.0-awn)
C
C  determine effective normalised working stress
C  and normalised sediment transport (equation 15)
	    stress_norm = ( fgr - awa ) / awa
	    if ( stress_norm.lt.0.0 ) then
              stress_norm = 0.0
              Write(75,*) ' Achtung: stress_norm = 0.0'
              Write(75,*) ' --> Geschiebetransport wird  zu 0.0'
            Endif
	    ggr = awc * (stress_norm**awm)
C
C  determine sediment concentration as mass of sediment per mass of water
	    if ( u_star.eq.0.0 ) then
	      c_bar = 0.0
	    else
	      c_bar = ggr*sed_rd*diam
	      c_bar = c_bar / ( depth * (u_star/u_bar)**awn )
	    end if

	    if ( c_bar.gt.ppmmax ) c_bar = ppmmax
	    gp(i) = c_bar
	    gptot = gptot + gp(i)
	  end do
C
!MD  Ablegen der Werte fuer den ersten Durchlauf bzw.
!MD   fuer D35 als charateritischen Durchmesser
	  if ( first_pass ) then
            gd35 =GP(1)
            SDND(1,NODNUM)=TMPSD
	    gptot = 0.0
	    is = igs
	    ie = lgs
	    first_pass = .false.
	  end if
	end do
C
C  normalise the transport at each grain size so that
C  the total transport must equal that predicted when a 
C  d35 grain size is used for the whole mixture
C
	if ( igs.ne.lgs ) then
C       ie  more than one grain size
	  do i = igs, lgs
	    gp(i) = (gd35/gptot) * gp(i)
	  end do
        !MD fuer nur eine Kornfraktion
        Elseif (igs.eq.lgs) then
          gp(1) = gd35
	end if
C

c c_bar is in kg of sediment per kg of water - WLP 31.5.96
c This is consistent with the original RMA formulation
C	write(77,*) loops,dgr,awn,awa,awm,awc,fgr,u_star,ggr,
C     + c_bar,gp(1),igs,lgs
      return
      end
! -------------------------------------------------------------------------



! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
CIPK MAR06  ADD NODNUM TO CALL
      subroutine get_u_star(depth,u_star,u_bar,NODNUM)
C
C  this subroutine will calculate the shear velocity based
C  upon Manning's equation and tau = rho*g*y*Sf combined
C  which yields:
C
C  u* = n g^1/2 u / y^1/6
C
C  David Luketina    26/5/96
C
      USE BLKSANMOD
	real gravity,xmanning_n,u_bar,depth,u_star
C
	gravity = ACGR
CIPK MAR06

!MD  Deactivating the extra use of, MANNING's for Sediment.
!MD  Use now, the already given Friction law and Friction
!MD  Parameters:
!MD    - ORT(NR,5) > 1          : CHEZY with C
!MD      --> FFACT = g / C²
!MD    - ORT(NR,5) < 0 and < 1  : MANNING's N
!MD      --> FFACT = n² g / (H^(1/3))
!MD    - ORT(NR,5) > 0          : DARCY ks
!MD      --> FFACT = lambdaTot / 8.0
!MD          with lambdaTot = Trees + Dunes + Bed

!MD Deactivated: 28.07.2008
!MD	xmanning_n = AMANNND(NODNUM)
!MD     u_star = xmanning_n * SQRT(gravity) * u_bar
!MD     u_star = u_star / depth**(1.0/6.0)

        u_star = SQRT(FFACT_KN(NODNUM)) * u_bar

c	write(*,*) u_star,depth,gravity,u_bar,amann
c	read(*,*) fred
C
      return
      end
C--------------------------------------------------------------



C--------------------------------------------------------------
C--------------------------------------------------------------
CIPK LAST UPDATE JUNE 15 2000 ALLOW FOR NEW WAVE OPTION
cipk last update apr 7 2000 correct interpolation function for 3-d
cipk last update apr18 1999 setup for linear distribution
cipk  last update Sep 9 1998 fix to PORSD units
cipk  last update Jan 14 1998   allow for zero velocity
      SUBROUTINE KINVIS
      USE BLK10MOD
      USE BLKSANMOD
C
C  this subroutine calculates the kinematic veisocity of
C  water as a function of temperature (currently at 20
C  deg C).
C
C         KINVIS
C
C      DIMENSION WTC(LNP),XNU(LNP)
C
C               NOTE. FORM OF EQUATION   NU= A/(B+C*TDC+D*TDC**2)
C        NOTE.  UNITS ARE M**2/SEC (A'ENGLISH'=.00001918)
C
      A=0.0000017819
      B=.4712
      C=.01435
      D=.0000682
      C1=B+32.0*C+1024*D
      C2=1.8*C+115.2*D
      C3=3.24*D
      TDC=20.0
      DO I=1,NP
        !MD neu: Vermeide Temperatur = 0 fuer Sedimentberechnung
        IF (VEL(5,I).gt.0) THEN
          TDC=VEL(5,I)
        END IF
        !MD TDC=VEL(5,I)
        XNU(I)=A/(C1+C2*TDC+C3*TDC*TDC)
	ENDDO
      RETURN
      END
