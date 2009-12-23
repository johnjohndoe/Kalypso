!
      SUBROUTINE nellii(drodz,dudz,dvdz,h,a0,z,epsxz,epsyz,difz,        &
     &                  rho,elev,grav)
      SAVE
! 
!      THIS SUBROUTINE IS THE MELLOR YAMADA LEVEL II BOUNDARY
!      LAYER TURBULENCE MODEL, IT CALCULATES VERTICAL TURBULENT
!      EDDY VISCOSITIES AND DIFFUSION COEFFICIENTS; THE SCHEME
!      IS ALGEBRAIC.  THE CODE LOOPS THROUGH ALL NODES IN WHICH
!      THERE IS VERTICAL RESOLUTION.  THIS MODEL IS BASED UPON
!      MELLOR AND YAMADA "DEVELOPMENT OF A TURBULENCE CLOSURE
!      MODEL FOR GEOPHYSICAL FLUID PROBLEMS" REVIEWS OF
!      GEOPHYSICS AND SPACE PHYSICS, VOL 20 NO. 4 PAGES851-875
!      NOVEMBER 1982: AND ADAMS AND WEATHERLY "SOME EFFECTS OF
!      SUSPENDED SEDIMENT STRATIFICATION ON AN OCEANIC BOTTOM
!      BOUNDARY LAYER" JOURNAL OF GEOPHYSICAL RESEARCH, VOL 86
!      NO. C5 PAGES 4161-4172 MAY 20, 1981.
!      CHARLIE BERGER JULY 1990
! 
!      dimension drodz(64),dudz(64),dvdz(64),h(64),a0(64),z(64),
!     .          epsxz(64),epsyz(64),difz(64),rho(64)
! 
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: h
!-
!      COEFFICIENTS THAT COME FROM MELLOR YAMADA
! 
      DATA ITM/0/
      VTHRESH=1.e-6
      A1 = 0.92
      B1 = 16.6
      A2 = 0.74
      B2 = 10.1
      C1 = 0.08
!                CALCULATE EDDY VISCOSITY AND VERTICAL DIFFUSION
!      do 100 i=1,ngp
      htemp=h
      aotemp=a0
      conver=(elev-aotemp)/htemp
      depel=1.030*htemp
      delh=0.030*htemp
      dengr=-grav*drodz/rho*conver
      dv2=dudz*dudz+dvdz*dvdz
      dv2=dv2*conver*conver
      if (dv2 < vthresh)dv2=vthresh
      gri=dengr/dv2
      dudzt=dudz*conver
      dvdzt=dvdz*conver
      zc=(z-aotemp)/conver+delh
      zcabs=abs(1.-zc/depel)
      elngth=0.40*zc*sqrt(zcabs)
      gamma1=0.333333-(2.*a1/b1)
      gamma2=(b2+6.*a1)/b1
      sh=3.*a2*gamma1
      sm=(sh*a1/a2)*(b1*(gamma1-c1)/(b1*gamma1))
      q2=b1*elngth*elngth*(sm*dv2)
!      q2=b1*elngth*elngth*(sm*dv2-sh*dengr)
      if(q2 < 0.)q2=0.0
      q=sqrt(q2)
!      IF(IWIND /= 0)THEN
!
!     TEMPORRAILY COMMMENTED OUT
!
!        IF(IWIND /= 0)THEN
!           CE-QUAL-W2 ADJUSTMENT FOR WIND WAVES
!           TAKEN FROM REPORT E-86-5 (USACE WES)
!           G. NAIL 12 JULY 1991
!
!           A CONSTANT FETCH OF 5000 METERS USED
!c              FTCH=2000.0
!c              TERM1=0.0695*(FTCH**0.233)*(TWM**0.534)
!c              WKN=4.0*9.8696/(9.81*(TERM1*TERM1))
!           COMPUTE DEPTH FACTOR
!c              TERM2=-2.*WKN*ABS(elev-z)*0.3048/conver
!           COMPUTE EDDY VISCOSITY CONTRIBUTION
!           SET FLAG FOR DUDZ OR DVDZ VERY SMALL (POTENTIAL OVERFLOW)
!c              IF (ABS(DUDZT) < 0.000001)DUDZT=0.000001
!c              WINDXZ=ABS(SIGMA(1,1)*EXP(TERM2)/DUDZT)
!c              IF (ABS(DVDZT) < 0.000001)DVDZT=0.000001
!c              WINDYZ=ABS(SIGMA(1,2)*EXP(TERM2)/DVDZT)
!rcb jan 92 I assumed constant wind stress using node 1
!           SET LIMITS ON THE EDDY VISCOSITY FOR STABILITY
!c              IF(ABS(WINDXZ) > .10)WINDXZ=.10
!c              IF(ABS(WINDYZ) > .10)WINDYZ=.10
!c              WINDZ=0.14*SQRT(WINDXZ*WINDXZ+WINDYZ*WINDYZ)
!c        ELSE
              WINDXZ=0.0
              WINDYZ=0.0
              WINDZ=0.0
!c        ENDIF
!   **** HENDERSON-SELLERS ADJUSTMENT
      if(gri < 0.)gri=0.
      if(grav < 10.)  then
        Epsxz = 1000. *(WINDXZ+SM*ELNGTH*Q)/(1.+0.74*GRI)
        Epsyz = 1000. *(WINDYZ+SM*ELNGTH*Q)/(1.+0.74*GRI)
        Difz = (WINDZ+SH * ELNGTH * Q) / (1. + 37. * GRI*GRI)
      else
        Epsxz = 1.94 *(WINDXZ+SM*ELNGTH*Q)/(1.+0.74*GRI)
        Epsyz = 1.94 *(WINDYZ+SM*ELNGTH*Q)/(1.+0.74*GRI)
        Difz = (WINDZ+SH * ELNGTH * Q) / (1. + 37. * GRI*GRI)
      endif
       if(difz <= 0.) then 
         write(*,*) 'difz = 0. at z =',z
       endif
!      if(icyc == 2)then
!      if(h(i) > 30.)then
!      write(*,*)'z,h,drodz,twm',z(i),h(i),drodz(i),twm
!      write(*,*)'dudz,dvdz',dudz(i),dvdz(i)
!      write(*,*)'windxz,epsxz',windxz,epsxz(i)
!      endif
!      endif
!  100 CONTINUE
      RETURN 
      END
