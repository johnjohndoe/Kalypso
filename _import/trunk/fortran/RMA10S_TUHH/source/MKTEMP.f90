!IPK LAST UPDATE Jan 06 2003 update for equilibrium formulation
!ipk last update Sep 30 2002 update for ice cover computation
!ipk last update july 3 2001 update for dew point and clarify performance for various units
!ph  last update january 2000 based on update from Najarian re English units
!    Note that the model operates in deg C at all times
!IPK  LAST UPDATED FEB 14 1998 UPDATE TO ADD VPAIR
!IPK  LAST UPDATED sept 8 1995
!**********************************************************************
!
      SUBROUTINE MKTEMP(T,D,DGP,SRCSNK,GRATE,DELT,MAT,NETT)
!
!      Calculate source sink term for Temp
      USE ICE2MOD
      USE BLK11MOD
      USE BLKMETMOD
!
      INTEGER :: NETT
!NiS,jul06: For calling compatibility D must also be real, kind=8
      REAL(KIND=8) :: D
!-
      REAL*8 T
!-
      DIMENSION ITINC(24)
      DATA ITINC/1,2,0,0,0,3,3,3,0,0,                                   &
     &           1,2,0,0,4,3,0,3,0,0,                                   &
     &           4,4,4,4/
!      WRITE(*,*) T,D,DGP,SRCSNK,GRATE,DELT,MAT,NETT
!
!ipk jan03 Add equilibrium temperature formulation
!
!     USE NETYP TO DETERMINE OUR ELEMENT TYPE
!
      IETP=ITINC(NETT)
!
      IF(METEQ == 1) THEN
!
        IF(IETP == 3) THEN
!
!                     E. Compute net heat flux  1-D/2-D ELEMENTS
          GRATE=-XKRAT/(998.0*4.1865*3600.*D)
          SRCSNK=XKRAT*(EQTEMP-T)/(998.0*4.1865*3600.*D)-GRATE*T
!
        ELSEIF(IETP == 1) THEN
!
!                     E. Compute net heat flux on surface.elements of 3-D
!
          IF(EXTING == 0.0) THEN
            GRATE=XKRAT/(998.0*4.1865*3600.)
            SRCSNK=XKRAT*(EQTEMP-T)/(998.0*4.1865*3600.)-GRATE*T
          ELSE
            GRATE=0.0
            SRCSNK=0.0
          ENDIF
!
        ELSEIF(IETP == 4) THEN
!
!                     E. Compute net heat flux below surface FOR 3-D
!
!                        Compute correction factor so that all heat enters
!                        the water column
!
          IF(EXTING == 0.0) THEN
            GRATE=0.0
            SRCSNK=0.0
          ELSE
            CORR = 1.0/(1.0-EXP(-EXTING*D))
            HFACT = CORR*EXP(-EXTING*DGP)
            GRATE=HFACT*XKRAT/(998.0*4.1865*3600.)
            SRCSNK=HFACT*XKRAT*(EQTEMP-T)/(998.0*4.1865*3600.)-GRATE*T
          ENDIF
        ELSE
          GRATE=0.0
          SRCSNK=0.0
        ENDIF
        RETURN
      ENDIF
!
!IPK JAN03 END ADDITIONS
!
      IF(MMET == 0) THEN
        SRCSNK=0.
        GRATE=0.
        RETURN
      ENDIF
!
!
!     USE NETYP TO DETERMINE OUR ELEMENT TYPE
!
      IETP=ITINC(NETT)
!
!     IETP = 4 FOR 2DV OR 3D
!          = 2 FOR BOTTOM ELEMENTS
!          = 1 FOR TOP ELEMENTS
!          = 3 FOR 2DH 
!
      IF(IETP == 0) RETURN
!      MAT = MOD(IMAT(NN),100)
!
      IF(IETP == 2) THEN
        GRATE=0.
        SRCSNK=0.
        RETURN
      ENDIF
!
!ipk sep02  add ice logic
!
      IF(GSICE > 0.) THEN
        IF(IETP == 3) THEN
!
!     2D HORIZONTAL
!
          SRCSNK=GSQLW/(D*998.0*4.1865)
!
        ELSEIF(IETP == 4) THEN
!
!     3D OR 2DV
!
          SRCSNK=0.
        ELSEIF(IETP == 1) THEN
!
!     SUFACE ELEMENT
!
          SRCSNK=GSQLW/(998.0*4.1865)
!
        ENDIF
        GRATE=0.0
        RETURN
      ENDIF
!
      DSAV=DELT
      IF(DELT == 0.) DELT=86400.
!
      DELTH = DELT/3600.0
!     GET DERIVATIVE OF HEAT WITH RESPECT TO T
!
      IF(METRIC == 0) THEN
        tw = t*1.8 + 32
        twd = tw+0.9
      else
        tw = t
        twd = tw+0.5
      endif
!
!                     D.2 Water surface back radiation (HB).
!
      IF(METRIC == 0) THEN
        VPW=0.1001*EXP(0.03*TW)-0.0837
!
!
!ipk jul01  add dew point option  Dew point stored in WETBLB
        IF(IDPT(MAT) == 1) THEN
          VPAIR=0.180*(exp(0.03739*(WETBLB(MAT)-32.)))
!
        ELSE
          VPWB=0.1001*EXP(0.03*WETBLB(MAT))-0.0837
          VPAIR=VPWB-0.000367*ATMPR(MAT)*(DRYBLB(MAT)-WETBLB(MAT))      &
     &     *(1.0+(WETBLB(MAT)-32.0)/1571.0)
        ENDIF
!
        HB=0.97*1.73E-09*(TW+460.0)**4
        VPWD=0.1001*EXP(0.03*TWD)-0.0837
        HBD=0.97*1.73E-09*(TWD+460.0)**4
      ELSE
!
        IF(IDPT(MAT) == 1) THEN
          VPAIR=0.180*(exp(0.03739*9./5.*WETBLB(MAT)))*1000./29.53
!
        ELSE
          vpwb=8.8534*exp(0.054*wetblb(MAT))-2.8345
          VPAIR=VPWB-0.0006606*ATMPR(MAT)*(DRYBLB(MAT)-WETBLB(MAT))     &
     &     *(1.0+WETBLB(MAT)/872.78)
!
        ENDIF
!
        TWF=TW*1.8+32.
        TWDF=TWD*1.8+32.
        vpw=(8.8534*exp(0.054*tw)-2.8345)
        vpwd=(8.8534*exp(0.054*twd)-2.8345)
        HB=0.97*2.0412e-07*(TW+273.0)**4
        HBD=0.97*2.0412E-07*(TWD+273.0)**4
      ENDIF
!
!                     D.3 Evaporation (HE).
!
      IF(METRIC == 0) THEN
        EVAP=62.4*(AE(MAT)+BE(MAT)*WIND(MAT))
        HE=EVAP*(VPW-VPAIR)*(1084.0-0.5*TW)
        HED=EVAP*(VPWD-VPAIR)*(1084.0-0.5*TWD)
      ELSE
        EVAP1=62.4*3.2808*1000./29.53*(AE(MAT)+BE(MAT)*WIND(MAT))
        EVAP=998.0*(AE(MAT)+BE(MAT)*WIND(MAT))
        HE=EVAP*(VPW-VPAIR)*(2400.0-0.9*TW)
        HED=EVAP*(VPWD-VPAIR)*(2400.0-0.9*TWD)
      ENDIF
!
!                     D.4 Conduction (HC).
!
      IF(METRIC == 0) THEN
        HC=0.01*EVAP*(DRYBLB(MAT)-TW)*(1084.0-0.5*TW)
        HCD=0.01*EVAP*(DRYBLB(MAT)-TWD)*(1084.0-0.5*TWD)
      ELSE
        HC=0.6096*EVAP*(DRYBLB(MAT)-TW)*                                &
     &         (2400.0-0.9*TW)
        HCD=0.6096*EVAP*(DRYBLB(MAT)-TWD)*                              &
     &         (2400.0-0.9*TWD)
!        if(nn == 1) write(75,*) 'hc',hc,hcd    
      ENDIF
!
      IF(IETP == 3) THEN
!
!                     E. Compute net heat flux from all sources.1-D/2-D ELEMENTS
!
        HSNET = TSOLHR(MAT) + HA(MAT) + (HC - HB - HE)*DELTH
!
        DELTAHS = (HCD-HBD-HED-HC+HB+HE)*DELTH
!
!       Contribution to SRCSNK is net radiation/(depth*density*heat cap)
!
        IF(METRIC == 0) THEN
!ph jan00 remove 3.2808         SRCSNK = (HSNET/(D*3.2808)/62.4/DELT)/1.8
!ph jan00 remove 3.2808          GRATE = (DELTAHS/(D*3.2808)/62.4/DELT)/1.8*2.
          SRCSNK = HSNET/(D*62.4*DELT*1.8)
          GRATE = DELTAHS/(D*62.4*DELT*1.8)*2.
!ipk jul01 put back 3.2808 for metric geometry
          IF(IMGEOM == 1) THEN
            SRCSNK=SRCSNK/3.2808
            GRATE=GRATE/3.2808
          ENDIF
          SRCSNK = SRCSNK-GRATE*T
        ELSE
          SRCSNK = HSNET/(D*998.0*4.1865*DELT)
          GRATE = DELTAHS/(D*998.0*4.1865*DELT)*2.
          IF(IMGEOM == 0) THEN
            SRCSNK=SRCSNK*3.2808
            GRATE=GRATE*3.2808
          ENDIF
          SRCSNK = SRCSNK-GRATE*T
        ENDIF
      ELSEIF(IETP == 1) THEN
!
!                     E. Compute net heat flux on surface.elements of 3-D
!
        HSNET = HA(MAT) + (HC - HB - HE)*DELTH
!
        DELTAHS = (HCD-HBD-HED-HC+HB+HE)*DELTH
!
!       Contribution to SRCSNK is net radiation/(density*heat cap)
!
        IF(METRIC == 0) THEN
          SRCSNK = (HSNET/62.4/DELT)/1.8
          GRATE = (DELTAHS/62.4/DELT)/1.8*2.
!ipk jul01 put back 3.2808 for metric geometry
          IF(IMGEOM == 1) THEN
            SRCSNK=SRCSNK/3.2808
            GRATE=GRATE/3.2808
          ENDIF
          SRCSNK = SRCSNK-GRATE*T
        ELSE
          SRCSNK = HSNET/(998.0*4.1865*DELT)
          GRATE = DELTAHS/(998.0*4.1865*DELT)*2.
          IF(IMGEOM == 0) THEN
            SRCSNK=SRCSNK*3.2808
            GRATE=GRATE*3.2808
          ENDIF
          SRCSNK = SRCSNK-GRATE*T
        ENDIF
      ELSEIF(IETP == 4) THEN
!
!                     E. Compute net heat flux below surface FOR 3-D
!
!       Get light extinction coefficient
!
        XLMB = EXTINC
!
!                        Compute correction factor so that all heat enters
!                        the water column
!
        CORR = 1.0/(1.0-EXP(-XLMB*D))
        HSNET = XLMB*TSOLHR(MAT)*CORR*EXP(-XLMB*DGP)
!
!       Contribution to SRCSNK is net radiation/(density*heat cap)
!
!
        IF(METRIC == 0) THEN
          SRCSNK = HSNET/(62.4*DELT*1.8)
!ipk jul01 put back 3.2808 for metric geometry
          IF(IMGEOM == 1) THEN
            SRCSNK=SRCSNK/3.2808
            GRATE=GRATE/3.2808
          ENDIF
          GRATE = 0.
        ELSE
          SRCSNK = HSNET/(998.0*4.1865*DELT)
          IF(IMGEOM == 0) THEN
            SRCSNK=SRCSNK*3.2808
          ENDIF
          GRATE = 0.
        ENDIF
!
      ENDIF
      DELT=DSAV
      RETURN
      END
