
      SUBROUTINE nellii(drodz,dudz,dvdz,h,a0,z,epsxz,epsyz,difz,
     .                  rho,elev,grav)
      SAVE
C 
C      THIS SUBROUTINE IS THE MELLOR YAMADA LEVEL II BOUNDARY
C      LAYER TURBULENCE MODEL, IT CALCULATES VERTICAL TURBULENT
C      EDDY VISCOSITIES AND DIFFUSION COEFFICIENTS; THE SCHEME
C      IS ALGEBRAIC.  THE CODE LOOPS THROUGH ALL NODES IN WHICH
C      THERE IS VERTICAL RESOLUTION.  THIS MODEL IS BASED UPON
C      MELLOR AND YAMADA "DEVELOPMENT OF A TURBULENCE CLOSURE
C      MODEL FOR GEOPHYSICAL FLUID PROBLEMS" REVIEWS OF
C      GEOPHYSICS AND SPACE PHYSICS, VOL 20 NO. 4 PAGES851-875
C      NOVEMBER 1982: AND ADAMS AND WEATHERLY "SOME EFFECTS OF
C      SUSPENDED SEDIMENT STRATIFICATION ON AN OCEANIC BOTTOM
C      BOUNDARY LAYER" JOURNAL OF GEOPHYSICAL RESEARCH, VOL 86
C      NO. C5 PAGES 4161-4172 MAY 20, 1981.
C      CHARLIE BERGER JULY 1990
C 
c      dimension drodz(64),dudz(64),dvdz(64),h(64),a0(64),z(64),
c     .          epsxz(64),epsyz(64),difz(64),rho(64)
C 
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: h
!-
C      COEFFICIENTS THAT COME FROM MELLOR YAMADA
C 
      DATA ITM/0/
      VTHRESH=1.e-6
      A1 = 0.92
      B1 = 16.6
      A2 = 0.74
      B2 = 10.1
      C1 = 0.08
C                CALCULATE EDDY VISCOSITY AND VERTICAL DIFFUSION
c      do 100 i=1,ngp
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
c      q2=b1*elngth*elngth*(sm*dv2-sh*dengr)
      if(q2 < 0.)q2=0.0
      q=sqrt(q2)
c      IF(IWIND /= 0)THEN
c
c     TEMPORRAILY COMMMENTED OUT
c
C        IF(IWIND /= 0)THEN
C           CE-QUAL-W2 ADJUSTMENT FOR WIND WAVES
C           TAKEN FROM REPORT E-86-5 (USACE WES)
C           G. NAIL 12 JULY 1991
C
C           A CONSTANT FETCH OF 5000 METERS USED
cc              FTCH=2000.0
cc              TERM1=0.0695*(FTCH**0.233)*(TWM**0.534)
cc              WKN=4.0*9.8696/(9.81*(TERM1*TERM1))
C           COMPUTE DEPTH FACTOR
cc              TERM2=-2.*WKN*ABS(elev-z)*0.3048/conver
C           COMPUTE EDDY VISCOSITY CONTRIBUTION
C           SET FLAG FOR DUDZ OR DVDZ VERY SMALL (POTENTIAL OVERFLOW)
cc              IF (ABS(DUDZT) < 0.000001)DUDZT=0.000001
cc              WINDXZ=ABS(SIGMA(1,1)*EXP(TERM2)/DUDZT)
cc              IF (ABS(DVDZT) < 0.000001)DVDZT=0.000001
cc              WINDYZ=ABS(SIGMA(1,2)*EXP(TERM2)/DVDZT)
crcb jan 92 I assumed constant wind stress using node 1
C           SET LIMITS ON THE EDDY VISCOSITY FOR STABILITY
cc              IF(ABS(WINDXZ) > .10)WINDXZ=.10
cc              IF(ABS(WINDYZ) > .10)WINDYZ=.10
cc              WINDZ=0.14*SQRT(WINDXZ*WINDXZ+WINDYZ*WINDYZ)
cc        ELSE
              WINDXZ=0.0
              WINDYZ=0.0
              WINDZ=0.0
cc        ENDIF
C   **** HENDERSON-SELLERS ADJUSTMENT
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
c      if(icyc == 2)then
c      if(h(i) > 30.)then
c      write(*,*)'z,h,drodz,twm',z(i),h(i),drodz(i),twm
c      write(*,*)'dudz,dvdz',dudz(i),dvdz(i)
c      write(*,*)'windxz,epsxz',windxz,epsxz(i)
c      endif
c      endif
c  100 CONTINUE
      RETURN 
      END
