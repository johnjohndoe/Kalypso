!     Last change:  MD   13 Jan 2009   12:24 pm
!IPK LAST UPDATE MARCH 25 2006 ADD TESTMODE
!ipk last update apr 7 2000 correct interpolation function for 3-d
!ipk  last update Mar 14 2000 make shear stress linear
!IPK  LAST UPDATE APR18 1999 TRAP VERY SMALL DEPTH
!_______________________________________________________________________
      SUBROUTINE SHEAR
!***********************************************************************
!     THIS SUBROUTINE COMPUTES THE NODE POINT FRICTION VELOCITIES
!   AND THE BED SHEARS USING THE LOG LAWS FOR ROUGH AND SMOOTH BEDS.
!                   VK=VON KARMANN'S CONSTANT
!                   RKS=ROUGHNESS HEIGHT (METERS)
!                   UN=KINEMATIC VISCOSITY OF SUSPENSION (M**2/SEC)
!                   GAW=DENSITY OF SUSPENSION (KG/M**3)
!     NP=#OF NODES, XVEL=VELOCITY COMPONENTS, DEP=DEPTH OF FLOW
!     UST =FRICTION VELOCITY, BSHEAR=BED SHEAR (N/M**2)
!
!     BASED ON THE METHOD OF
!                                      RANJAN ARIATHURAI
!                                      RESOURCE MANAGEMENT ASSOCIATES
!                                      AUGUST 1981
!***********************************************************************
!-
      USE BLK10MOD
      USE BLKSEDMOD
      USE BLKSANMOD
!IPK MAR06
      USE BLKTSMOD      
!-
!......................................................................
!    CHECK TO SEE IF SMOOTH OR ROUGH BED (RKS=0 MEANS SMOOTH)
!   IF CONSTANTS ARE ZERO SET DEFAULT VALUES
!......................................................................
      VK=0.4
!MD: IF(GAW == 0.) GAW=1000.      
      CON=2.3/VK
      IF(RKS <= 0.0001)THEN
!......................................................................
!          ****   SMOOTH BED   *****
!......................................................................
!MD: Basis-Variable: Wasserdichte
!MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx
!     Je nach Vorgabe der SI-Einheiten unter "IGRV"
      ROAVG=1.935
      IF (GRAV < 32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte
!
        DO  N=1,NPM
          IF (LSS > 0) THEN
            IF (VEL(6,N) >= 0.) THEN
              GAWND(N)=(VEL(6,N)/1000.)*(1.-ROAVG/GACND(N)) + ROAVG
            Else
              GAWND(N)= ROAVG
            END IF
            GAW=GAWND(N)
          Endif
          IF(GAW <= 0.) then
            GAW=ROAVG
          END IF
!MD  use original data          
!MD: UN=XNU(N)          
!MD: IF(UN == 0.) UN=1.142E-6          
          IF(XNU(N) == 0.) XNU(N)=1.142E-6
          K1=N
          IF(NDEP(N) > 1) K1=NREF(N)+NDEP(N)-1
          U=VEL(1,K1)
          V=VEL(2,K1)
!MD    UV=SQRT(U*U+V*V)      
!MD use original data      
          UV=SQRT(VEL(1,K1)*VEL(1,K1) + VEL(2,K1)*VEL(2,K1))
!IPK MAR06
          IF(ITSTMOD == 1) THEN
            UV=TSTVAR(1)
          ENDIF
!
!****** AVERAGE VEL BASED ON BOTTOM VEL / UMIN... NOT CORRECT SINCE
!****** SINCE VEL / UMIN = SURFACE VELOCITY
!          IF(K1 /= N)  UV=UV/UMIN
          IF(UV == 0. .OR. VEL(3,k1) < 0.01) THEN
            USTA=0.
            GO TO 150
          ENDIF
!
!**** USE NEWTON'S METHOD TO COMPUTE BED SHEAR
!
          USTA=.03*UV
        WDTP=VEL(3,K1)
!MD:  A1=CON*ALOG10(3.32*WDTP/UN)      
!MD  use original data      
          A1=CON*LOG10(3.32*VEL(3,K1)/XNU(K1))
          UOLD=USTA
          DO  NIT=1,10
            IF(USTA <= 0.) USTA=UOLD/2.
            UOLD=USTA
            USTA=UOLD-(UOLD*(A1+CON*LOG10(UOLD))-UV)/                   &
     &      (A1+CON+CON*LOG10(UOLD))
            IF(ABS(USTA-UOLD) < 0.001) GO TO 150
          ENDDO
          WRITE(LOUT,778)K1,UOLD
  778     FORMAT(' SHEAR CALC. DID NOT CONVERGE FOR NODE ',I5           &
     &    ' USING BEST GUESS',E15.5)
          USTA=UOLD
  150     BSHEAR(K1)=USTA*USTA*GAW
          BSHEAR(N)=BSHEAR(K1)
          UST(K1)=USTA
          UST(N)=USTA
          IF(IT == 1) THEN
            ESRO(K1)=BSHEAR(K1)
            ESRO(N)=BSHEAR(N)
          ENDIF          
        ENDDO
!.......................................................................
!                  ***** ROUGH BED  ******
!.....................................................................
      ELSE
        DO N=1,NPM
          I=N
          IF(NDEP(N) > 1) I=NREF(N)+NDEP(N)-1
          U=VEL(1,I)
          V=VEL(2,I)
!MD    UV=SQRT(U*U+V*V)      
!MD use original data      
          UV=SQRT(VEL(1,I)*VEL(1,I) + VEL(2,I)*VEL(2,I))
!IPK MAR06
          IF(ITSTMOD == 1) THEN
            UV=TSTVAR(1)
          ENDIF
!****** AVERAGE VEL BASED ON BOTTOM VEL / UMIN... NOT CORRECT SINCE
!****** SINCE VEL / UMIN = SURFACE VELOCITY
!          WRITE(78,*) N,I,U,V,UV,UMIN
!          IF(I /= N) UV=UV/UMIN
          IF(UV <= 0.)THEN
            UST(I)=0.
            GO TO 200
          ENDIF
        WDTMP=VEL(3,I)
!MD: 05.11.2008: no water --> no shear stress!!!      
!MD:   IF(WDTMP <= 0.) WDTMP=.001      
!MD:   UST(I)=UV/(CON*ALOG10(12.27*WDTMP/RKS))      
!MD:   UST(N)=UST(I)      
          IF(VEL(3,I) < 0.01) Then
            UST(I)=0.0
          Else
            UST(I)=UV/(CON* LOG10(12.27*VEL(3,I)/RKS))
          Endif
          UST(N)=UST(I)
  200     BSHEAR(I)=UST(I)*UST(I)*GAW
          BSHEAR(N)=BSHEAR(I)
        ENDDO
      ENDIF
!
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
!
!
!ipk Mar 2000 Adjustment to make linear interpolation of bed shears
!ip apr2000 finalize adjustments
      do n=1,ne
        ncn=0
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
              nod=nop(n,j)
              k1=nod
              if(ndep(n) > 1) k1=nref(nod)+ndep(nod)-1
              bshear(nod)=(bshear(nop(n,j1))+bshear(nop(n,j2)))/2.
              ust(nod)=(ust(nop(n,j1))+ust(nop(n,j2)))/2.
              if(k1 /= nod) then
                bshear(k1)=bshear(nod)
                ust(k1)=ust(nod)
              endif
              if(it == 1) then
                esro(k1)=bshear(k1)
                esro(nod)=bshear(nod)
              endif
            enddo
          endif
        endif
      enddo 
!
!ipk Mar 2000 end changes
!
      RETURN
      END
