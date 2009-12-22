!     Last change:  MD   13 May 2009   11:28 am
!----------------------------------------------------------------------
SUBROUTINE KALYP_SHEAR

!***********************************************************************
!MD: 05.11.2008
!  ROUTINE computes the node POINT friction velocities u* = UST and the
!  BEDSHEARs using LOG laws based on Darcy-Weisbach law (ks+pasche trees)
!  The FFACT = lambda is used form SubR Roughness. Thereforee the
!  SubR GET_FFACT provides lambda from elements and converts it to nodes
!
!     GAW = DENSITY OF SUSPENSION (kg/m**3)
!     NP  = Number OF NODES
!     VELS    = RMS VELOCITY COMPONENTS [m/s]
!     VEL(3,N)= DEPTH OF FLOW
!     USTA    = FRICTION VELOCITY = u* [m/s]
!     BSHEAR  = BED SHEAR (N/m**2)
!
!
!***********************************************************************

USE BLK10MOD
USE BLKSEDMOD
USE BLKSANMOD
USE BLKTSMOD

REAL(KIND=8) :: VELS


!---------------------------------------------------
!MD: Basis-Variable: Wasserdichte
!MD:  NEU Wasserdichte ROAVG [kg/m^3] anlehnend an COEFxx
!     Je nach Vorgabe der SI-Einheiten unter "IGRV"
ROAVG=1.935
IF (GRAV.LT.32.) ROAVG=516.*1.935
!MD: Basis-Variable: Wasserdichte

DO N=1,NPM
  IF (LSS .GT. 0) THEN
    IF (VEL(6,N).ge.0.) THEN
      GAWND(N)=(VEL(6,N)/1000.)*(1.-(ROAVG/GACND(N))) + ROAVG
    Else
      GAWND(N)= ROAVG
    END IF
    GAW=GAWND(N)
  Endif
  IF(GAW.LE.0.) then
    GAW=ROAVG
  END IF

  K1=N
  !MD: Deactivate 3d option!
  !MD:  IF(NDEP(N) .GT. 1) K1=NREF(N)+NDEP(N)-1

  if(wsll(N) .ge. ao(N)) then !Nur wenn knoten nass!
    VELS=SQRT(VEL(1,K1)**2.0 + VEL(2,K1)**2.0)
    UST(K1)=SQRT(FFACT_KN(N)) * VELS

    IF(VELS.EQ.0. .or. VEL(3,K1).lt.0.01) THEN
      UST(K1)=0.0
    ENDIF
  else !Wenn knoten trocken!
    UST(K1)=0.0
  endif

  BSHEAR(K1)=UST(K1)*UST(K1)*GAW

  !MD: IF(IT .EQ. 1) THEN
  !MD:  ESRO(K1)=BSHEAR(K1)
  !MD: ENDIF

  IF (N.eq.NPM) THEN
    WRITE(75,*) 'All BedShears are calculated till NODE ',N
  END IF
!MD:  IF (N.eq.222 .or. N.eq.223) THEN   !TEST!!
!MD:    WRITE(75,*) 'BedShear(N=222 bzw. 223): ', BSHEAR(K1)
!MD:    WRITE(75,*) 'UST(N=222 bzw. 223)     : ', UST(K1)
!MD:    WRITE(75,*) 'FFACT_KN(N=222 bzw. 223): ', FFACT_KN(K1)
!MD:    WRITE(75,*) 'GAW: ', GAW
!MD:  END IF

ENDDO

DO N=1,NPM
  IF(IMID(N,1) .GT. 0) THEN
    NN = N
    IF (NDEP(N) .GT. 1) NN = NREF(N) + NDEP(N)-1
    BSHEAR(NN)=(BSHEAR(IMID(NN,1))+BSHEAR(IMID(NN,2)))/2.
    ESRO(NN)=(ESRO(IMID(NN,1))+ESRO(IMID(NN,2)))/2.
    UST(NN)=(UST(IMID(NN,1))+UST(IMID(NN,2)))/2.
    BSHEAR(N)=BSHEAR(NN)
  ENDIF
ENDDO


!---------------------------------------------------
!MD: Mar 2000 Adjustment to make linear interpolation of bed shears
do n=1,ne
  ncn=0
  if(imat(n).gt.0 ) then
    if(imat(n).lt.1000 .and. ncorn(n).lt.9) then
      ncn=ncorn(n)
    elseif(imat(n)/1000.eq.1 .or. imat(n)/1000.eq.2) then
      ncn=ncorn(n)
    endif

    if(ncn .gt. 0) then
      if(ncn .eq. 5) ncn=3
      do j=2,ncn,2
        j1=j-1
        j2=mod(j,ncn)+1
        nod=nop(n,j)
        k1=nod
        if(ndep(n) .gt. 1) k1=nref(nod)+ndep(nod)-1
        bshear(nod)=(bshear(nop(n,j1))+bshear(nop(n,j2)))/2.
        ust(nod)=(ust(nop(n,j1))+ust(nop(n,j2)))/2.
        if(k1 .ne. nod) then
          bshear(k1)=bshear(nod)
          ust(k1)=ust(nod)
        endif
        if(it .eq. 1) then
          esro(k1)=bshear(k1)
          esro(nod)=bshear(nod)
        endif
      enddo
    endif

  endif
enddo

RETURN
END
