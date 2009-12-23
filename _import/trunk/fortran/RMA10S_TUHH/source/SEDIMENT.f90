!     Last change:  MD   22 Oct 2008   12:30 pm
!IPK LAST UPDATE MAR 07 2006 fix gpmax setup
!IPK LAST UPDATE MAR 05 2006 UPDATE TO ALLOW FOR NODAL VALUES
!IPK  LAST UPDATE AUG 09 2005 ADD bshear
!
!------------------------------------------------------------
!
      SUBROUTINE BEDXCG
      USE BLK10MOD
      USE BLKDRMOD
      USE BLKSANMOD
!MD neu USe BLKSEDMOD      
      USE BLKSEDMOD
!
!  evaluate sedimentation or erosion at the bed based upon the
!  difference between the actual and equilibrium (carrying)
!  concentrations of sediment.
!
!
      character*4 KPO(7)
      DATA ITB/3/
      DATA KPO(1)/'NODE'/,KPO(2)/'BED '/,KPO(3)/'ELEV'/,KPO(4)/'ALPH'/, &
     & KPO(5)/'A1  '/,KPO(6)/'ALPH'/,KPO(7)/'A2  '/
!        NOTE.  THE SOURCE TERM IS  +  IF EROSION, - IF DEPOSITION
!
      CDT=0.5*DELT
      POSA=.4
!     RHOF=1000.
      CONP=1.-POSA
!IPK MAR06 DELETED    SGPT=1000.*SGSA
!IPK MAR06 MOVED      RHOB=RHOF*CONP*SGSA
      CODT=DELT*(1.-ALPHASN)
!IPK MAR06 DELETED    COEFF=CDT/(CONP*RHOF*SGSA)
 1030 CONTINUE
!
! --------------------------------------------------------
!     CHECK TO SEE IF WE HAVE MULTILAYER PROBLEM
! --------------------------------------------------------
      IF(NP > NPM) THEN
!
!     SETUP SETTLING RATE
!
        DO N=1,NP
        IF(IBNA(N) > 0 .AND. WSLL(N) > AO(N)) THEN
!IPK MAR06            
            IF(VSANDND(N) > 1E-20)THEN
              CST=CLDEND(N)*VEL(3,N)/VSANDND(N)
            ELSE
              CST=0.
            ENDIF
            IF(CST < DELT) CST=DELT
            ALPHA1(N)=-VEL(3,N)/CST
            ALPHA2(N)=0.
        ELSE
            ALPHA1(N)=0.
            ALPHA2(N)=0.
        ENDIF
        ENDDO
      ENDIF
!
! --------------------------------------------------------
!     NOW PROCESS BOTTOM OR FOR 2DH
! --------------------------------------------------------
      DO 1130 NN=1,NPM
      IF(IBNA(NN) > 0 .AND. WSLL(NN) > AO(NN)) THEN
! Save Geschiebetransport VEL(7,N)
          gpbsav(nn)=gpb(nn,1)   
          N=NN
!
!
          IF(NDEP(NN) > 1) N=NREF(NN)+NDEP(NN)-1
!ipk apr99      IF(WD(N) > DSET) THEN
          GPBSAV(N)=GPB(N,1)
          RHOB=RHOF*CONP*SGSAND(N)
!
          if(WSLL(n) > ao(n)) then
!IPK MAR06
            VSN=VSANDND(N)
            AVU=SQRT(VEL(1,N)**2+VEL(2,N)**2)
            D=VEL(3,N)
            COEF=D/CONP
            DO 1120 I=IGS,LGS
              DELSAC=(VEL(6,N)-GPB(N,I))/1000.
!
!            DELSAC is concentration difference kg/m3 ve means deposition
!             --> NOTE. DEPOSITION IS OCCURRING. ------------------------------
!MD: Wenn Geschiebe-C gpb() groesser als Schwebstoff-C vel(6)              
              IF(DELSAC >= 0.) THEN
!
!               IF(VS(N) > 1E-20)THEN
!               CST=CLDE*D/VS(N)
                IF(VSN > 1E-20)THEN
!IPK JAN03        CST=CLDE*D/VSN
!DJW APR03        CST=D/VSN
!              CST = (0.15*D)/VSN
!DJW MAY03
!MDMD_test  CST = (0.25*D)/VSN                
                  CST=CLDEND(N)*VEL(3,N)/VSANDND(N)
!END DJWAPR03
                ELSE
                  CST=0.
                ENDIF
!ipk jan03        NOTE.  SOURCE TERM = (GP - CONC)/CST
!IPK JAN03              IF(CST < DELT) CST=DELT
!IPK MAR06
!MDMD  CST = IF(CST < CLDEND(N)*3600.) CST=CLDEND(N)*3600.                
!MDMD  DELT ist schon sekunden!!                
!
!MDMD_neu  !MDMD_neu                
!MDMD_neu
                IF (CST <= 0.0) THEN   
                  ALPHA1(N)=0.0
                  ALPHA2(N)=0.0
                DEP=0.0
!Ende MDMD_neu
                Else                 
                  ALPHA1(N)=-VEL(3,N)/CST
                  ALPHA2(N)=-GPB(N,I)*ALPHA1(N)
                DEP=ALPHA1(N)*VEL(6,N)+ALPHA2(N)
                END IF
!
!MD Vergleiche Geschiebe gpb() mit Schwebstoff vel(6)                
              if(gpb(n,i) == 0.) then
                beta=1.0
              else
                if(vel(6,n) > 0) then
                  ratio=gpb(n,i)/vel(6,n)
                else
                  ratio=1.0
                endif
                  BETA=1.0-ratio**0.2
              endif
              EYSRAT=BETA*CAI(N)*VSN
!
!MD                !MD!MD: Anfang der Testausgabe
!MD                IF (n==1) then
!MD                 Write(LOUT,*) 'Ausgabe aus BEDXCG: DEPO'
!MD                 write(LOUT,*) '    N,   DEP,            vel(6,n),
!MD     +      gpb(n),       alpha1(n),
!MD     +      alpha2(n),     XNU(n),      CST,        BSHEAR(N)'
!MD                Endif
!
!MD              WRITE(LOUT,'(I6,8G15.5)')
!MD     +            N,DEP,vel(6,n),gpb(n,i),alpha1(n),alpha2(n),XNU(n),
!MD     +            CST, BSHEAR(N)
!MD                !MD!MD: Ende der Testausgabe
!
!
!
!             --> SCOUR IS OCURRING.  CHANGE THE SIGN TO COMPUTE TSACB.
!MD: Wenn Geschiebe-C gpb() kleiner als Schwebstoff-C vel(6)              
!MD: Wenn DELSAC < 0    ---------------------------------
              ELSE   
!       -'ve means erosion
!
!ipk jan98 check for zero velocity
                if(avu < 1e-20) then
                cst=delt
                else
!IPK MAR06              
                  CST=CLERND(N)*VEL(3,N)/AVU
                endif
!IPK MAR06             
                PORSD=SDND(1,N)*CONP/1000.
!              PORSD is effective diameter of pore space
                RTO=DELT/CST
                IF(RTO > 1.)RTO=1.
!
!              RTO is apparent change rate over time step
!               forced to be not more tham 1
!
                TSACB=DELSAC*RTO
!              TCSACB is predicted change
                DYBED=-VEL(3,N)*TSACB/(RHOB-VEL(6,N)/1000.+TSACB)
!            DYBED is change in bed thickness allowing
!               suspended material in water column
!
                IF(TTHICK(N)-PORSD-DYBED < 0.) THEN
!            This is the case of bed being exhausted
!
                  DYBED=0.
                  IF(VEL(6,N) > 0.)                                     &
     &              DYBED=TTHICK(N)-PORSD*(VEL(6,N)/GPB(N,I))**0.3333
!              Assume the material goes into suspesion only
!
                  IF(DYBED <= 0.) THEN
!              No material for that set rates to zero
                    DELSAC=0.
                    alpha1(n)=0.
                    alpha2(n)=0.
                    go to 1120
                  ELSE
                    DELSAC=-DYBED*(RHOB-VEL(6,N)/1000.)/                &
     &                                          ((D+DYBED)*RTO)
!                DELSAC is recomputed to just set bed to zero
                  ENDIF
                  GPB(N,I)=VEL(6,N)-DELSAC*1000.
!                 GPB reset to be the a new effective equilibrium
                ENDIF
!ipk jan03      NOTE.  SOURCE TERM = (GP - CONC)/CST
!ipk jan03         IF(CST < DELT) CST=DELT
!ipk jan03     Use CLER as a resuspension time in hours
!ccc              IF(CST < DELT/5.) CST=DELT/5.
!IPK MAR06
!MDMD  CST= CLERND(N)*3600.                
!MDMD  DELT ist schon sekunden!!                
                ALPHA1(N)=-VEL(3,N)/CST
                ALPHA2(N)=-GPB(N,I)*ALPHA1(N)
!              ALPHA1 is rate term
!               ALPHA1*SED+ALPHA2 is the total source
!
!MD  MD: Testausgabe - TEST
!MD                IF (n==1) then
!MD                   Write(LOUT,*) 'Ausgabe aus BEDXCG: DEPO'
!MD                   write(LOUT,*) '    N,   DEP,      vel(6,n),
!MD     +    gpb(n),       alpha1(n),
!MD     +      alpha2(n),            BSHEAR(N)'
!MD                Endif
!MD              WRITE(LOUT,'(I6,8G15.5)')
!MD     +            N,DEP,vel(6,n),gpb(n,i),alpha1(n),alpha2(n),BSHEAR(N)
!MD  MD: Testausgabe - TEST
!
!
!     NOTE.DEPTH OF SEDIMENT LESS THAN REQUIRED TO SATISFY TRANSPORT
!     CAPACITY.  REDUCE THE TRANSPORT CAPACITY.
              ENDIF
!        NOTE.  SOURCE TERM = (GP - CONC)/CST
!ipk jan03    IF(CST < DELT) CST=DELT
!ipk jan03    ALPHA1(N)=-VEL(3,N)/CST
!ipk jan03    ALPHA2(N)=-GPB(N,I)*ALPHA1(N)
 1120       CONTINUE
!
          else
            alpha1(n)=0.
            alpha2(n)=0.
          ENDIF
      ELSE
          alpha1(nn)=0.
          alpha2(nn)=0.
      ENDIF
 1130 CONTINUE
!
!ipk experimental april 1999 set distribution linear
!ipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
        if(imat(n) > 0  ) then
          if(imat(n) < 1000 .AND. ncorn(n) < 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 == 1 .OR. imat(n)/1000 == 2) then
            ncn=ncorn(n)
          endif
          if(ncn > 0) then
            if(ncn == 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              alpha1(nop(n,j))=(alpha1(nop(n,j1))+alpha1(nop(n,j2)))/2.
              alpha2(nop(n,j))=(alpha2(nop(n,j1))+alpha2(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 
!
      RETURN
      END
! -------------------------------------------------------------------------
!
!
!
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
!
!  this subroutine will calculate the sediment carrying capacity 
!  as kg of sediment per m3 of water.
!
!  ismode = 1   use Ackers-White (1973) method
!  ismode = 2   use van Rijn (1993) method
!  ismode = 3   use Brownlie (1981) method
!  ismode = 4   use updated van Rijn method
!
!      
!  DJW 19/01/04 Variable Declarations      
!      
      LOGICAL WBM
      INTEGER*4 AD
!      
!  End of DJW Changes 19/01/04      
!      
!      integer ismode
!
      DATA IAW/5/
!
! DJW 19/01/04 Sets WBM Modifications in motion
      WBM = .FALSE. 
      IRR=1
!
!  POSA is the porosity of the sediment when
!  on the bed. Typically POSA = 0.4
!
      CONP=1.-POSA
!
!  note that PPMMAX is not actually in ppm but, rather, is the
!  concentration in kg of sediment per kg of fluid. 
!
!IPK MAR06 MOVED      PPMMAX=SGSA*CONP/(POSA+CONP*SGSA)
!
! Ueber alle Knoten
      DO NN=1,NPM     
!IPK DEC02       if(ibna(nn) > 0 .AND. WSLL(NN) > AO(NN)) then
! IF (NN == 6) THEN      
!   WRITE(*,*) 'Kennung Rauheitsklasse je Knoten: IBNA', ibna(nn)      
!   WRITE(*,*) 'WSLL(NN): ', WSLL(NN)      
!   WRITE(*,*) 'A0(NN): ', AO(NN)      
!   WRITE(*,*) 'Check: WSLL(NN) > AO(NN) am Knoten?????'      
!   WRITE(*,*) 'Wasserstand am Knoten 6: VEL(3,NN)', VEL(3,NN)      
!   WRITE(*,*) 'DSET am Knoten 6: ', DSET      
!   WRITE(*,*) 'Check: VEL(3,N) <= DSET am Knoten?????'      
! END IF      
!
!
!MD    Falsche Abfrage: Knoten sollen Nass sein!!
!MD       if(ibna(nn) > 0 .AND. (WSLL(NN) > AO(NN) .OR. 
!MD   +     VEL(3,NN) <= DSET)) THEN
        if(ibna(nn) > 0 .AND. WSLL(NN) > AO(NN)) THEN
          N=NN
          IF(NDEP(NN) > 1) N=NREF(NN)+NDEP(NN)-1
!
!IPK MAR06
          PPMMAX=SGSAND(N)*CONP/(POSA+CONP*SGSAND(N))
!
!ipk dec02 IF(VEL(3,N) <= DSET)GOTO 1130
!          D35=EFDT(N)
!IPK MAR06
          D35=SDND(1,NN)
          IRR=N
          EFD=VEL(3,NN)
          EXNU=XNU(NN)
          USTAR=UST(N)
!
!ipk dec02  experiment with smoothing around nodes
!
!       if(n == 3974 .OR. n == 3981 .OR. n == 3980) then
!        idebg=1
!      endif
          divide=0.
        vxl=0.
        vyl=0.
        do j=1,np
          idone(j)=0
            enddo
          do k=1,12
            neadj=elton(n,k)
            if(neadj /= 0) then
              do j=1,ncorn(neadj)
              nadj=nop(neadj,j)
              if(idone(nadj) == 0) then
                vxl=vxl+vel(1,nadj)
                vyl=vyl+vel(2,nadj)
                idone(nadj)=1
                divide=divide+1.
                endif
            enddo
            endif
          enddo
        IF(DIVIDE > 0.) THEN
            VXL=VXL/DIVIDE
          VYL=VYL/DIVIDE
        ENDIF
          VELN=SQRT(VEL(1,N)**2+VEL(2,N)**2)
          VELS=SQRT(VXL**2+VYL**2)
!         if(n == 3974 .OR. n == 3981 .OR. n == 3980) then
!          write(166,*) n,veln,vels
!        endif
!
!IPK JUN00 ADD COMPUTATION OF CURRENT DIRECTION FOR NEW OPTION
!
          IF(VELS > 0.) THEN
!           CDIR=ATAN2(VEL(2,N),VEL(1,N))
            CDIR=ATAN2(VYL,VXL)
          ELSE
            CDIR=0.
          ENDIF
!
          HSV= WAVEHT(N)
          TP=  PEAKPRD(N)
          WVDIR=WAVEDR(N)
        IF(HSV > 0.7*EFD) HSV=0.7*EFD
!
          ISUBTR=1
!
!          WRITE(LOUT,1100)N,D35,VELS,EFD,USTAR,AVRRO,OUTNU,AVCON
! 1100     FORMAT(//,' PARAMETERS FOR SAND TRANSPORT POTENTIAL',/,
!       * 5X,' NN    D35     VELOCITY     DEPTH          U*     RHO    ',
!       * '     NU(10**5)      CONC.',/,
!       * 5X,I3,F8.6,5F12.4,F12.6)
!
!  use the method of choice for sediment transport
!
!IPK MAR06
!
!MD neu: 29.07.2008
!MD calculation of FAKT_KN for each node based on lamdbaTot for each element
          IF (NN == 1) THEN
            WRITE(75,*) 'Friction factors are calculated for all NODES'
            call GET_FFACT
          endif
!MD neu: 29.07.2008  (nur einmal GET_FFACT aufrufen!!)
!
          IF(ITSTMOD == 1) THEN
            vels=TSTVAR(1)
          ENDIF
          if(vels > 0.00001) then
!ipk mar06  add node nunbers to calls below
            ISMODE=ISMODEND(NN)
!
          if ( ismode == 1 ) then
!MD: Acker-White-Formel fuer Geschiebetransport              
              call awhite(VELS,EFD,EXNU,ustar,nn)
!
          else if ( ismode == 2 ) then
!MD: Van-Rijn-Formel fuer Geschiebe- und Schwebstofftransport              
              call vanrijn(VELS,EFD,EXNU,ustar,nn)
!             IF(NN == 21) THEN
!             WRITE(112,*) MAXN,NN,VELS,EFD,EXNU,USTAR,GP(1)*1.E6
!            ENDIF
          else if ( ismode == 3 ) then
!MD: Brownlie-Formel fuer Geschiebetransport              
              call brownlie(VELS,EFD,EXNU,ustar,nn)
!
!ipk jun00 add new call for Nielsen option
          else if ( ismode == 4 ) then
!MD: Geschiebe mit Wellen nach Engel-und Hansen            
            RC=RCAN(NN)
            RW=RWAN(NN)
              iswtt = -1
!c            if(nn == 4361) iswtt=1
!     DJW 19/01/04 : Ensuring that mannings roughness modification variables are initiated before      
!     wavecm is called      
!      
            IF (WBM) THEN
!NiS,Nov06:   Mixing logical type with arithmetic is not possible in Lahey
!            IF (wbm_Initiated == .FALSE.) THEN
                IF ( .NOT. wbm_Initiated) THEN
!-
                  CALL BedRoughInitiate(NP,wbm_Initiated,wbm_MannTrans, &
     &          wbm_NodeCounter,wbm_IT,wbm_MannTransOld, wbm_BedHeight)
               END IF
              END IF
!     End DJW 19/01/04      
!IPK MAR06  
              SGSA=SGSAND(NN)
              D50=D50ND(NN)
              D90=D90ND(NN)
              VSAND=VSANDND(NN)
              call wavecm (hsv,tp,WVDIR,VELS,CDIR,D50,D90,EXNU,         &
     &          SGSA,RHOF,EFD,GP,VSAND,USTAR,                           &
     &          RC,RW,TRLOC,CA,ISWTT,nn)
!         RC,RW,TRLOC,CA,ISWTT,nn,RWFACT,RWMIN) ! djw 19/01/04 includes passing of roughnesses
          else if ( ismode == 5 ) then
!MD: Geschiebe- und Schwebstofftransport mit Wellen            
            RC=RCAN(NN)
            RW=RWAN(NN)
!MD  Unsinn --> kann raus
                  IF(NN == 1249 .OR. nn == 1586 .OR. nn == 1615         &
     &                   .OR. nn == 1265) THEN
              ISWTT=2
            ELSE
              ISWTT=0
            ENDIF
!MD Ende Unsinn --> kann raus
              iswtt = -1
!IPK MAR06
              SGSA=SGSAND(NN)
              D50=D50ND(NN)
              D90=D90ND(NN)
              VSAND=VSANDND(NN)
              call TRANSPOR(hsv,tp,WVDIR,VELS,CDIR,D50,D90,EXNU,        &
     &           SGSA,RHOF,EFD,GP,VSAND,USTAR,                          &
     &           RC,RW,TRLOC,ISWTT,nn)
             if(iswtt == 2) then
              write(75,*) 'back from transpor',nn,vels,efd,gp(1)
            endif
          else
            write(*,'(/,'' error in specifying smode '')')
            stop
          end if
            I=1
!
!MD: Auswertung aller Ergebnisse aus den einzelnen Geschiebe-Transport-Berechnungen
! --------------------------------------------------------------------------------
!  now multiply concentration (1kg(sand)/1kg(of water) = Mg/m3) by 1000000 to get g/m3 or mg/l
!
! at the end of rma-sandx ust(n) is in metric => assign in subroutines
! ackwht, brownlie, etc.
!
            UST(N)= USTAR
!IPK AUG05 ADD bshear
          BSHEAR(N)=RHOF * (USTAR**2.0)
            GPB(N,I)=GP(I) * (10.0**6.0)
!ipk mar06
            GPMAX=GPMAXND(N)
          IF(GPB(N,I) > GPMAX) GPB(N,I)=GPMAX
          TRRAT(N)=TRLOC
          CAI(N)=CA*1000.
        else
          ust(n)=0.
!IPK AUG05  ADD bshear
          BSHEAR(N)=0.
          gpb(n,1)=0.
          TRRAT(N)=0.
          CAI(N)=0.
        endif
!
 1130     CONTINUE
        else
!ipk mar03 change n to nn
        ust(nn)=0.
        gpb(nn,1)=0.
          TRRAT(NN)=0.
        CAI(NN)=0.
        endif
      end do
!
!
!  -------------------------------------      
!  DJW 19/01/04 : Calling Routines that Modify the Roughness Based on Roughness Characteristics      
!  Also calls routine to initiate the calculation      
      If (WBM) Then
!
!NiS,Nov06: Mixing Logical type with arithmetic is not possible in Lahey
!       IF (wbm_Initiated == .FALSE.) THEN
        IF ( .NOT. wbm_Initiated) THEN
          CALL BedRoughInitiate(NP,wbm_Initiated,wbm_MannTrans,wbm_NodeCounter, &
     &         wbm_IT, wbm_MannTransOld, wbm_BedHeight)
      END IF
        Call NewRough(ZMANN,NOP,NE,wbm_MannTrans,wbm_IT,IT,ICYC,MAXN,NP)
      End If
!      
!  End DJW Changes 19/01/04      
!
!ipk experimental april 1999 set distribution linear
!ipk apr00 corect for 3-d using top and bottom element only
      ncn=0
      do n=1,ne
        if(imat(n) > 0  ) then
          if(imat(n) < 1000 .AND. ncorn(n) < 9) then
            ncn=ncorn(n)
          elseif(imat(n)/1000 == 1 .OR. imat(n)/1000 == 2) then
            ncn=ncorn(n)
          endif
          if(ncn > 0) then
            if(ncn == 5) ncn=3
            do j=2,ncn,2
              j1=j-1
              j2=mod(j,ncn)+1
              GPB(nop(n,j),1)=(GPB(nop(n,j1),1)+GPB(nop(n,j2),1))/2.
              TRRAT(nop(n,j))=(TRRAT(nop(n,j1))+TRRAT(nop(n,j2)))/2.
            enddo
          endif
        endif
      enddo 
!ipk may02
!IPK TEMP***********************************************************
!      DO N=1,21
!      GPB(N,1)=20.+(N-1)*5
!      ENDDO
!cc      DO N=2,20,2
!cc      GPB(N,1)=(GPB(N-1,1)+GPB(N+1,1))/2.
!cc      ENDDO
!IPK TEMP***********************************************************
!
!     copy potential into 7th position and get time derivative
!
      DO n=1,np
!       VEL(7,N)=GPB(N,1)
        if(lbed > 0) then
!        vel(7,n)=gpb(n,1)
!
          IF(MAXN == 0) THEN
          VOLD(7,N)=GPB(N,1)
            VDOTO(7,N)=0.
          ENDIF
!
          IF(MAXN == 1) THEN
            VOLD(7,N)=VEL(7,N)
          VDOTO(7,N)=VDOT(7,N)
          ENDIF
          VEL(7,N)=GPB(N,1)
          VDOT(7,n)=ALTM*(VEL(7,n)-VOLD(7,n))-VDOTO(7,n)*(ALPHA-1.)
!
          if(delt > 0) then
          VDOT(7,N)=(VEL(7,N)-VOLD(7,N))/DELT
          else
          vdot(7,n)=0.
          endif
!
          IF (n==1) then
            write(LOUT,*) 'Ausgabe aus SANDX:'
            write(LOUT,*) 'Belegung von vel(7,n), vold(7,n), vdot(7,n)'
          Endif
!
        ELSE
        VEL(7,N)=GPB(N,1)
        endif
      enddo
!
      RETURN
      END
! -------------------------------------------------------------------------
!
!
!
! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
!IPK MAR06 ADD NODE NUMBER TO CALL
      subroutine awhite (VELS,EFD,EXNU,u_star,NODNUM)
! wlp u_star not being passed back
!
!  sediment transport by Ackers-White method
!        REF = HY 118 ASCE, NOV73, PP 2041-2021.
!
!  David Luketina   26/5/96
!
!  input variables are:
!  
!  VELS            mean velocity
!  EFD            depth
!  D35            d35 size (m) of sediment mixture
!  EXNU            kinematic viscosity (m2/s)
!
      USE BLKSANMOD
!
      integer i,ie,is,loops,j
      real gravity,sed_rd,root32,diam,vels,efd
      real depth,viscosity,ten,u_bar,awa,awn,awm,awc
      real part_a,part_b,c_bar,exnu,gnsg,clog,dgr,dgrlog
      real stress_norm,fgr,u_star,gptot,gd35
      real tmpsd
      logical first_pass
!
        data ten/10./
!
      gravity = ACGR
!IPK MAR06
      sed_rd = SGSAND(NODNUM)
      viscosity = EXNU
      depth = EFD
      u_bar = VELS
!
!  start calculations
!
      root32 = SQRT(32.0)
      gnsg = (sed_rd - 1) * gravity
!
!  the outside loop has either one or two loops.
!
!  if there is only one grain size there will be a single
!  outside loop.
!
!  if there is more than one grain size, the first pass of the 
!  outside loop will handle the d35 case and the other (second)
!  pass will handle the other grain sizes (from array pointer
!  IGS to LGS).
!
!  the inside loop, loops once for each grain size (obviously,
!  in the d35 case there can only be a single inner loop).
!
!  set parameters for the first pass - d35 case
!
      first_pass = .true.
      gptot = 0.0
!IPK MAR06
      tmpsd = SDND(1,NODNUM)
      SDND(1,NODNUM) = D35ND(NODNUM)
      is = 1
      ie = 1
!
      if ( igs == lgs ) then
        loops = 1
      else
        loops = 2
      end if
!
        i = 0
        do j = 1, loops
! ueber alle Bodenlayer
          do i = is, ie  
!IPK MAR06
            diam=SDND(i,NODNUM)/1000.
!!
!           wlp Remember diameter is in mm
!
!           find non-dimensional grain size
            dgr = diam *(gnsg**(1./3.))/ (viscosity**(2./3.))
            dgrlog = ALOG10(dgr)
!
!           now find ackers-white coefficients
!
!MD          if (dgr > 60.) then
!MD            awn = 0.
!MD            awa = 0.17
!MD            !MD awm = 1.50  alter Ansatz nach 1973
!MD            awm = 1.78
!MD            awc = 0.025
!
!MD  Neuer Ansatz fuer dgr > 60 mit glattem Ubergang zu den
!MD  Funktionen groesser und kleiner 60
          if (dgr > 60.0) then
              IF (dgr < (10.0**(1.0/0.56))) THEN
                awn = 1.00 - (0.56 * ALOG10(dgr))
              Else
                awn = 0.00
              END IF
              awa = (0.23/SQRT(60.0)) +0.14
              awm = (6.83/60.0) +1.67
              clog = (2.79*ALOG10(60.0))-(0.98*(ALOG10(60.0)**2.0))-3.46
              awc = 10.**clog
!
!  find non-dimensional grain size
          else if (dgr <= 60.0) then
!             find n coefficient (equation 16)
              awn = 1.00 - (0.56*dgrlog)
!
!             find a coefficient (equation 17)
              awa = (0.23/SQRT(dgr)) +0.14
!
!ipk Aug01 experimental addition of modified Ackers White coefficients
!             find m coefficient (equation 20)
              awm = (6.83/dgr) +1.67
!
!             find c coefficient (equation 22)
!MD  clog = 2.79*dgrlog-0.98*dgrlog-3.46              
!MD  11.08.2008: Formel korrigiert!!              
              clog = (2.79*dgrlog) - (0.98*(dgrlog**2.0)) - 3.46
!
!ipk Aug01 below are dropped coefficients
!             find m coefficient (equation 20)
!             awm = 9.66/dgr+1.34
!             find c coefficient (equation 22)
!             clog = 2.86*dgrlog-dgrlog**2.0-3.53
              awc = 10.**clog
!
              IF (dgr <= 1) THEN
                write(75,*) ' Warnung aus Acker-White:'
                write(75,*) ' Gueltigkeitsbereich fuer der Gelichung'
                write(75,*) '  von dgr > 1 wurde unterschritten!'
              END IF
          end if
!
!
!  get u_star via Manning's equation
!IPK MAR06 ADD NODNUM TO CALL
          call get_u_star(depth,u_star,u_bar,NODNUM)
!
!  determine mobility number
          part_a = u_star**awn / SQRT(diam*gnsg)
          part_b = u_bar / (root32*ALOG10(10.0*depth/diam))
          fgr = part_a * part_b**(1.0-awn)
!
!  determine effective normalised working stress
!  and normalised sediment transport (equation 15)
          stress_norm = ( fgr - awa ) / awa
          if ( stress_norm < 0.0 ) then
              stress_norm = 0.0
              Write(75,*) ' Achtung: stress_norm = 0.0'
              Write(75,*) ' --> Geschiebetransport wird  zu 0.0'
            Endif
          ggr = awc * (stress_norm**awm)
!
!  determine sediment concentration as mass of sediment per mass of water
          if ( u_star == 0.0 ) then
            c_bar = 0.0
          else
            c_bar = ggr*sed_rd*diam
            c_bar = c_bar / ( depth * (u_star/u_bar)**awn )
          end if
!
          if ( c_bar > ppmmax ) c_bar = ppmmax
          gp(i) = c_bar
          gptot = gptot + gp(i)
        end do
!
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
!
!  normalise the transport at each grain size so that
!  the total transport must equal that predicted when a 
!  d35 grain size is used for the whole mixture
!
      if ( igs /= lgs ) then
!       ie  more than one grain size
        do i = igs, lgs
          gp(i) = (gd35/gptot) * gp(i)
        end do
!MD fuer nur eine Kornfraktion        
        Elseif (igs == lgs) then
          gp(1) = gd35
      end if
!
!
! c_bar is in kg of sediment per kg of water - WLP 31.5.96
! This is consistent with the original RMA formulation
!      write(77,*) loops,dgr,awn,awa,awm,awc,fgr,u_star,ggr,
!     + c_bar,gp(1),igs,lgs
      return
      end
! -------------------------------------------------------------------------
!
!
!
! -------------------------------------------------------------------------
! -------------------------------------------------------------------------
!IPK MAR06  ADD NODNUM TO CALL
      subroutine get_u_star(depth,u_star,u_bar,NODNUM)
!
!  this subroutine will calculate the shear velocity based
!  upon Manning's equation and tau = rho*g*y*Sf combined
!  which yields:
!
!  u* = n g^1/2 u / y^1/6
!
!  David Luketina    26/5/96
!
      USE BLKSANMOD
      real gravity,xmanning_n,u_bar,depth,u_star
!
      gravity = ACGR
!IPK MAR06
!
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
!
!MD Deactivated: 28.07.2008
!MD      xmanning_n = AMANNND(NODNUM)
!MD     u_star = xmanning_n * SQRT(gravity) * u_bar
!MD     u_star = u_star / depth**(1.0/6.0)
!
        u_star = SQRT(FFACT_KN(NODNUM)) * u_bar
!
!      write(*,*) u_star,depth,gravity,u_bar,amann
!      read(*,*) fred
!
      return
      end
!--------------------------------------------------------------
!
!
!
!--------------------------------------------------------------
!--------------------------------------------------------------
!IPK LAST UPDATE JUNE 15 2000 ALLOW FOR NEW WAVE OPTION
!ipk last update apr 7 2000 correct interpolation function for 3-d
!ipk last update apr18 1999 setup for linear distribution
!ipk  last update Sep 9 1998 fix to PORSD units
!ipk  last update Jan 14 1998   allow for zero velocity
      SUBROUTINE KINVIS
      USE BLK10MOD
      USE BLKSANMOD
!
!  this subroutine calculates the kinematic veisocity of
!  water as a function of temperature (currently at 20
!  deg C).
!
!         KINVIS
!
!      DIMENSION WTC(LNP),XNU(LNP)
!
!               NOTE. FORM OF EQUATION   NU= A/(B+C*TDC+D*TDC**2)
!        NOTE.  UNITS ARE M**2/SEC (A'ENGLISH'=.00001918)
!
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
        IF (VEL(5,I) > 0) THEN
          TDC=VEL(5,I)
        END IF
!MD TDC=VEL(5,I)        
        XNU(I)=A/(C1+C2*TDC+C3*TDC*TDC)
      ENDDO
      RETURN
      END
