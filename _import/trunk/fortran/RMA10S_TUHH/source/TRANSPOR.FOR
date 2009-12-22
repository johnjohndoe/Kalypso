CIPK LAST UPDATE MAR 05 2006 REMOVE PRIBT STATEMENTS
CIPK DEC02 ADD TEST FOR STRAY DIRECTIONS

      SUBROUTINE TRANSPOR(HS,TPS,WDIR,VRR,CDIR,D50MM,D90MM,RNU,SGSA,RHOW
     +                   ,HDS,GP,WS,UST,RC,RW,TRLOC,ISWT,node)
      USE NIOREZMOD
C
C     COMPUTATION OF CONCENTRATION VERTICALS AND SUSPENDED LOAD TRANSPORT
C     THROUGH NUMERICAL INTEGRATION AND FORMULAE APPROXIMATION
C
      LOGICAL BO
      DIMENSION RMAT(25,6)
      dimension gp(*)

      external fwl,tolx
      logical log
      DATA ITIM/0/

      TP=TPS
	HD=HDS
      VR=VRR
	D50=D50MM/1000.
	D90=D90MM/1000.
      if(hs.eq.0.)then
        ur=0.
        ub=0.
      endif
      if(tp.gt.0..and.tp.lt.1.)tp=1.
CIPK JUN02
      PI     = 4.*ATAN(1.)
      CANG=180./PI*CDIR
cipk jan02 reverse wave diretion
c      WANG=180./PI*WDIR-180.
      WANG=180./PI*WDIR

CIPK DEC02 ADD TEST FOR STRAY DIRECTIONS
   50 IF(WANG .LT. 0.) THEN
	  WANG=WANG+360.
	  GO TO 50
	ENDIF
   55 IF(WANG .GT. 360.) THEN
	  WANG=WANG-360.
	  GO TO 55
	ENDIF
   60 IF(CANG .LT. 0.) THEN
	  CANG=CANG+360.
	  GO TO 60
	ENDIF
   65 IF(CANG .GT. 360.) THEN
	  CANG=CANG-360.
	  GO TO 65
	ENDIF

      if(cang.eq.0.) cang=0.000001
	IF(WANG .LT. 0.) WANG=WANG+360
CIPK JUN02      PHI = ABS((180.-CANG) + WANG)
CIPK JUN02      IF(PHI.GE.180.) PHI = 360. - PHI         
      PHI=WANG-CANG
	IF(ABS(PHI) .LT. 0.001) PHI=0.
	IF(PHI .LT. 0.) PHI=PHI+360.
      if(phi.eq.0.)phi=0.1
      if(phi.eq.180.)phi=179.9
      if(phi.eq.360.)phi=359.9
CIPK MAR06
C      write(108,'(i5,5f10.2)') 
C     +   node,cdir/pi*180.,wdir/pi*180.,cang,wang,phi
      NN     = 12
      G      = 9.81
      phi    = phi/180.*pi
CIPK JUN02      cl=(sa-0.03)/1.805
CIPK JUN02      rhow=1000.+1.455*cl-0.0065*(te-4.+0.4*cl)**2.
CIPK JUN02      rhos=2650.
CIPK JUN02      rnu=(4.e-5)/(20.+te)
      RHOS=SGSA*RHOW
      DEL=(RHOS-RHOW)/RHOW
CIPK JUN02      dsh=0.01*g*del*dss**3./rnu/rnu
      d50h=0.01*g*del*d50**3./rnu/rnu
CIPK JUN02      if(dss.lt.0.0001)ws=(del*g*dss*dss)/(18.*rnu)
      if(d50.lt.0.0001)wsb=(del*g*d50*d50)/(18.*rnu)
CIPK JUN02      if(dss.ge.0.0001.and.dss.lt.0.001)
CIPK JUN02     * ws=(10.*rnu/dss)*((1.+dsh)**0.5-1.)
      if(d50.ge.0.0001.and.d50.lt.0.001)
     * wsb=(10.*rnu/d50)*((1.+d50h)**0.5-1.)
CIPK JUN02      if(dss.ge.0.001)ws=1.1*(del*g*dss)**0.5
      if(d50.ge.0.001)wsb=1.1*(del*g*d50)**0.5
      RKAP=.4
CIPK JUN02      if(rc.lt.0.01)rc=0.01
CIPK JUN02      if(rw.lt.0.01)rw=0.01
      DSTER=D50*(DEL*G/RNU**2)**(1./3.)
C
C   COMPUTATION OF WAVE PARAMETERS
C
      IF(HD.LE.0.)STOP 'WATER DEPTH .LE. 0'

cipk dec02 add test for wave height
      IF (TP.GE.1.  .and.  hs .gt. 0.1) THEN

        xx=.1
        yy=1.8*tp*tp
        uugg=VR
        VR=0.
        call zeroin(xx,yy,log,fwl,tolx)
        if(.not.log)then
          stop 'zeroin no wave length'
        else
          yy=2.0*xx
          xx=.2*xx
          VR=uugg
          call zeroin(xx,yy,log,fwl,tolx)
          if(.not.log)then
CIPK DEC02             stop 'zeroin no wave length'

CIPK DEC02
             Y=4.02*HD/TP/TP
             POL=1.+
     +           Y*(.666+Y*(.355+Y*(.161+Y*(.0632+Y*(.0218+.00654*Y)))))
             WAVENR=SQRT(Y**2+Y/POL)/HD
             RLS=2.*PI/WAVENR
             ARG=WAVENR*HD
	       GO TO 200
          else
            rls=xx
          endif
        endif
        arg=2.*pi*hd/rls

200     CONTINUE
        tp1=tp/(1.-(VR*tp*cos(phi))/rls)
        IF (ARG.GT.50.) THEN
          ABW=0.
          FW=0.
          FW1=0.
          UBW=0.
        ELSE
          ABW=HS/(2.*SINH(ARG))
          IF(RC  .EQ. 0.  .OR.  RW .EQ. 0.) THEN
            PSI = 4.02 * (ABW / TP) ** 2 / ((RHOS/RHOW -1) * D50)
            ATEST1 = (21. * PSI ** (-1.85) * ABW)

* For high wave energy conditions with a flat bed :

            ATEST2 = 0.072 * ABW * (ABW / (3. * D90)) ** (-0.25)*2.
 
            IF(PSI.LE.12) THEN
              A = (0.275 - 0.022 * SQRT(PSI)) * ABW
            ELSE 
              IF(ATEST1.GE.ATEST2) THEN
                A = ATEST1
              ELSE
                A = ATEST2
              ENDIF
            ENDIF

c              A  = ripple height
c              ABW = peak value of the near bed wave orbital excursion

CIPK JUN02           DS = 6. * A
            RC = 6. * A
            RW = 6. * A

c              RW = effective wave related bed roughness 
c              RC = effective current related bed roughness 
c              DS = mixing layer thickness

          ENDIF
 
          FW=0.
          FW1=0.
          IF(ABW.GT.0.)FW=EXP(-6.+5.2*(ABW/RW)**(-0.19))
          IF(ABW.GT.0.)FW1=EXP(-6.+5.2*(ABW/(3.*D90))**(-0.19))
          IF(FW.GT.0.3 )FW=0.3
          IF(FW1.GT.0.3)FW1=0.3
          UBW  = 2.*PI/TP1*ABW
        ENDIF
      ELSE
        ABW=0.
        RLS=0.
        FW=0.
        FW1=0.
        UBW=0.
        tp1=tp
CIPK JUN02
        if(rc.lt.0.01)rc=0.01
        if(rw.lt.0.01)rw=0.01

      ENDIF
      ubw1=0.
      if(rls.gt.0.)
     *  ubw1=(3.*pi*pi*hs*hs)/(4.*tp1*rls*(sinh(arg)**4.))
      rhs=hs/hd
      h1=1.+0.3*rhs
      hlimit=0.01*g*tp1*tp1
      if(hd.ge.hlimit)then
        ubwfor=ubw+ubw1
        ubwback=ubw-ubw1
      endif
      if(hd.lt.hlimit)then
        ubwfor=h1*ubw
        ubwback=(2.-h1)*ubw
      endif
      ufmax=1.2*ubw
      ubmin=0.8*ubw
      if(ubwfor.ge.ufmax)ubwfor=ufmax
      if(ubwback.le.ubmin)ubwback=ubmin
      asym=0.
      if(abw.gt.0.)asym=ubwfor/(ubwfor+ubwback)
      htrough=hd*(0.95-0.35*(hs/hd))
CIPK JUN02      if(ur.eq.9.)ur=-0.125*g**0.5*hs**2./(hd**0.5*htrough)
CIPK JUN02      if(ub.eq.9.)ub=(0.05-(asym-0.5))*ubw
      ur=-0.125*g**0.5*hs**2./(hd**0.5*htrough)
      ub=(0.05-(asym-0.5))*ubw
C
C  CRITICAL SHEAR STRESS SHIELDS
C
      IF(DSTER.LE.4.)THETCR=.24/DSTER
      IF(4. .LT.DSTER.AND.DSTER.LE.10.)THETCR=.14*DSTER**(-.64)
      IF(10..LT.DSTER.AND.DSTER.LE.20.)THETCR=.04*DSTER**(-.1 )
      IF(20..LT.DSTER.AND.DSTER.LE.150.)THETCR=.013*DSTER**(.29 )
      IF(DSTER.GT.150.)THETCR=.055
      TAUCR=(RHOS-RHOW)*G*D50*THETCR
      umcr=5.75*(del*g*d50)**0.5*(thetcr)**0.5*alog10(4.*hd/d90)
      ubwcr=0.
      if(tp.gt.0.)then
       if(d50.le.0.0005)then
             ubwcr=(0.12*del*g*d50**0.5*tp**0.5)**0.667
       else
             ubwcr=(1.09*del*g*d50**0.75*tp**0.25)**0.57
       endif
      endif
C
C  COMPUTATION OF REFERENCE CONCENTRATION CA
C
      a=max(rc,rw)
      CC=18.*ALOG10(12.*HD/RC)
      CC1=18.*ALOG10(12.*HD/3./D90)
      FC=0.24*ALOG10(12.*HD/RC)**(-2)
      FC1=0.24*ALOG10(12.*HD/3./D90)**(-2)
      vrr1=(VR**2+ur**2)**0.5
      RMUC=FC1/FC
      RMUWA=0.6/DSTER
      if(dster.gt.10.)RMUWA=0.06
      rmuw=0.
      if(fw.ne.0.)rmuw=fw1/fw
      UST=G**0.5*ABS(vrr1)/CC
      DELm=0.
      delw=0.
      IF(ABW.GT.0.)THEN
         DELW=0.072*ABW*(ABW/RW)**(-0.25)
         delm=3.*delw
      ENDIF
      delm=max(delm,rc)
      GAMMA=0.
      h2=phi
      if(h2.gt.pi)h2=2.*pi-phi
      gamma=0.8+h2-0.3*h2*h2
	if(vrr1 .lt. 0.0001) then
	  uratio=5.
	else
        uratio=ubw/vrr1
	endif
      if(uratio.gt.5.)uratio=5.
      RA=EXP(GAMMA*uratio)*RC
      RCC=10.*RC
      IF(RA.GE.RCC)RA=RCC
      FCA=0.24*ALOG10(12.*HD/RA)**(-2)
      IF(DELm.LE.Ra/29.9)delm=ra/29.9
      h3=(ALOG(30.*DELM/RA)/ALOG(30.*DELM/RC))**2
      h4=((-1.+alog(30.*hd/rc))/(-1.+alog(30.*hd/ra)))**2
      alfaw=h3*h4
      TAUC=0.125*RHOW*FC*vrr1*vrr1
      TAUW=0.25*RHOW*FW*UBW*UBW
      tauce=rmuc*tauc
      TAUCEF=RMUC*ALFAW*TAUC
      TAUWEF=RMUWA*TAUW
      TAUCWE=TAUCEF+TAUWEF
      if(taucwe.le.tauce)taucwe=tauce
      uster1=sqrt(taucef/rhow)
      uster2=sqrt(tauce/rhow)
      IF(USTER1.LE.USTER2)USTER1=USTER2
      TA=(TAUCWE-TAUCR)/TAUCR
      TA=MAX(.0001,TA)
      CA=0.015*D50/A*DSTER**(-.3)*TA**1.5
      if(ca.ge.0.60)ca=0.6
      taub=rmuc*alfaw*tauc+rmuw*tauw
      if(taub.le.tauce)taub=tauce
      t=(taub-taucr)/taucr
      t=max(0.0001,t)
C
C SUSPENDED LOAD TRANSPORT IN CURRENT DIRECTION
C
C COMPUTATION OF NUMERICAL PARAMETERS AT Z=A
C
      JTAL= 8
      NN  = JTAL*NN
      DYM = CA/NN
      DXM = HD/NN
      DYX = DYM/DXM
      BO  = .FALSE.
      HULP30=-1.+ALOG(30.*HD/RA)
      IF(DELM.GT.0.)THEN
         UDEL=VR*ALOG(30.*DELM/RA)/HULP30
      ENDIF
C
C  COMPUTATION OF DERIVATIVE DC/DY OR DC/DX
C
      beta=1.+2.*(ws/ust)**2.
      if(beta.ge.1.5)beta=1.5
      ds=0.3*hd*(hs/hd)**0.5
      if(ds.gt.0.2)ds=0.2
      if(ds.le.0.05)ds=0.05
      EBW=.004*DSTER*DS*UBW
      IF (TP.GE.1.) THEN
        EMAXW=0.035*HD*HS/TP
      ELSE
        EMAXW=0.
      ENDIF
      IF(EMAXW.LE.EBW)EMAXW=EBW
      EMAXC=0.25*RKAP*UST*HD*BETA
      C=CA
      Z=A
      IF(Z.LE.DS)ESW=EBW
      IF(Z.GT.DS.AND.Z.LE.0.5*HD)ESW=EBW+(EMAXW-EBW)*((Z-DS)/
     *(0.5*HD-DS))
      IF(Z.GE.0.5*HD)ESW=EMAXW
      IF(Z.GE.0.5*HD)ESC=EMAXC
      IF(Z.LT.0.5*HD)ESC=EMAXC-EMAXC*(1.-2.*Z/HD)**2
      ES=(ESW**2.+ESC**2.)**0.5
      cm=0.65
      fcc=0.
      IF (C.GT.1.E-8) THEN
        IF(Z.GE.A)then
            fi =1.+(c/cm)**.8-2.*(c/cm)**.4
            fcc=-WS/ES/fi*C*(1.-C)**5.
        endif
      ENDIF
      YPRIME=fcc
      FF = 1./CA*YPRIME
      IF(DELM.GT.0.)THEN
         UC=UDEL*ALOG(30.*A/RC)/ALOG(30.*DELM/RC)
      ENDIF
      IF(A.GE.DELM)UC=VR*ALOG(30.*A/RA)/HULP30
      if(a.le.rc/30.)uc=0.
C
C  STORAGE OF RESULTS IN MATRIX RMAT
C
      RMAT(1,1)=Z
      RMAT(1,2)=CA*RHOS
      RMAT(1,3)=UC
      RMAT(1,4)=0.
C
C INTEGRATION FROM Z=A TO SURFACE
C
      Y = CA
      TERM1=UC*Y
      XEND=A
      SSC=0.
      NTEL = 0
      IT   = 2
C
  100 CONTINUE
      NTEL = NTEL+1
      XOLD = XEND
      YOLD = Y
      IF (-YPRIME .GT. DYX) THEN
         Y = YOLD-DYM
         IF (Y .LT. 2./3.*YOLD) Y = 2./3.*YOLD
         XEND = XOLD+ALOG(Y/YOLD)/FF
      ELSE
         XEND = XOLD+DXM
         IF (XEND .GE. HD) THEN
            XEND = HD
            BO = .TRUE.
         ENDIF
         Y = EXP(ALOG(YOLD)+(XEND-XOLD)*FF)
      ENDIF
      C=Y
      Z=XEND
      IF(Z.LE.DS)ESW=EBW
      IF(Z.GT.DS.AND.Z.LE.0.5*HD)ESW=EBW+(EMAXW-EBW)*((Z-DS)/
     *(0.5*HD-DS))
      IF(Z.GE.0.5*HD)ESW=EMAXW
      IF(Z.GE.0.5*HD)ESC=EMAXC
      IF(Z.LT.0.5*HD)ESC=EMAXC-EMAXC*(1.-2.*Z/HD)**2
      ES=(ESW**2.+ESC**2.)**0.5
      fcc=0.
      IF (C.GT.1.E-8) THEN
        IF(Z.GE.A)then
            fi =1.+(c/cm)**.8-2.*(c/cm)**.4
            fcc=-WS/ES/fi*C*(1.-C)**5.
        endif
      ENDIF
      YPRIME=fcc
      FF = 1./Y*YPRIME
      IF(DELM.GT.0.)THEN
         UC=UDEL*ALOG(30.*XEND/RC)/ALOG(30.*DELM/RC)
      ENDIF
      IF(XEND.GE.DELM)UC=VR*ALOG(30.*XEND/RA)/HULP30
      if(xend.le.rc/30.)uc=0.
      TERM2=UC*Y
      TERM1=TERM2
C
C  STORAGE OF RESULTS IN MATRIX RMAT
C
      IF (NTEL .EQ. NN/JTAL.OR.BO) THEN
         RMAT(IT,1)=Z
         dz=rmat(it,1)-rmat(it-1,1)
         RMAT(IT,2)=Y*RHOS
         ssc=ssc+(rmat(it,2)*uc+rmat(it-1,2)*rmat(it-1,3))/2.*dz
         RMAT(IT,3)=UC
         RMAT(IT,4)=SSC
         IT = IT+1
         NTEL      = 0
      ENDIF
      IF (.NOT. BO) GOTO 100
c
c  SUSPENDED LOAD TRANSPORT IN WAVE DIRECTION
c
       ssw=0.
       const=(ur/VR)
       uz1=const*rmat(1,3)
       rmat(1,5)=uz1
       rmat(1,6)=0.
       do 250 i=2,it-1
           uz2=const*rmat(i,3)
           rmat(i,5)=uz2
           dz=rmat(i,1)-rmat(i-1,1)
           torm=(uz1*rmat(i-1,2)+uz2*rmat(i,2))/2.*dz/rhos
           ssw=ssw+torm*rhos
           rmat(i,6)=ssw
           uz1=uz2
  250 continue
c
c  INSTANTANEOUS BED LOAD TRANSPORT 
c
      vrdelm=vr*alog(30.*delm/ra)/(-1.+alog(30.*hd/ra))
      if(abs(vr) .gt. 0.0001) then
	  urdelm=(ur/vr)*vrdelm
	else
	  urdelm=0.
	endif
      IF(DELM.LE.RA/30.)THEN
        vrdelm=0.
        urdelm=0.
      endif
      ubtot=ub+urdelm
      rmfor=(ubwfor+ubtot)**2./(del*g*d50)
      rmback=(-ubwback+ubtot)**2./(del*g*d50)
      rmcr=ubwcr**2./(del*g*d50)
      ntime=201
      dtt=tp1/(ntime-1)
      tfor=0.
      if(abw.gt.0.)tfor=ubwback/(ubwfor+ubwback)*tp1
      tback=tp1-tfor
      profact=((-1.+alog(30.*hd/rc))/alog(30.*delm/rc))**2.
      fc11=0.25*fc1*profact
	if(abs(ubw)+abs(vrdelm) .gt. 0.0001) then
        acw=abs(vrdelm)/(abs(ubw)+abs(vrdelm))
	else
	  acw=1.
      endif
      fcw1=acw*fc11+(1.-acw)*fw1
      sbx=0.
      sby=0.
      tau1som=0.
      tau1x=0.
      tau1y=0.
      nnn=0
  150 continue
      time=nnn*dtt
      udt=0.
      if(abw.gt.0.)then
      if(hd.ge.hlimit)then
         udt=ubw*sin(2.*pi*time/tp1)+ubw1*sin(4.*pi*time/tp1-0.5*pi)
      else
         if(time.lt.tfor)then
         udt=ubwfor*sin(pi*time/tfor)
      else
         udt=-ubwback*sin(pi*(time-tfor)/tback)
      endif
      endif
      endif
      udtx=udt*cos(phi)
      udty=udt*sin(phi)
      ubtotx=ubtot*cos(phi)
      ubtoty=ubtot*sin(phi)
      uxt=vrdelm+udtx+ubtotx
      uyt=udty+ubtoty
      utvec=(uxt**2.+uyt**2.)**0.5
      tau1som=tau1som+0.5*rhow*fcw1*utvec*utvec
      tau1t=0.5*rhow*fcw1*utvec*utvec
      tt=(tau1t-taucr)/taucr
      tt=max(0.0001,tt)
      uster1t=(tau1t/rhow)**0.5
      coeff=0.25*(1.-(hs/hd)**0.5)
      if(coeff.le.0.075)coeff=0.075
      sbt=coeff*d50*rhos*uster1t*tt**1.5/(dster**0.3)
      if(utvec .gt. 0.0001) then
	  sbx=sbx+uxt/utvec*sbt
        sby=sby+uyt/utvec*sbt
        tau1x=tau1x+uxt/utvec*tau1t
        tau1y=tau1y+uyt/utvec*tau1t
	endif
      nnn=nnn+1
      if(nnn.le.ntime)goto 150
      tau1m=tau1som/(ntime-1)
      tau1x=tau1x/(ntime-1)
      tau1y=tau1y/(ntime-1)
      tau1net=(tau1x**2.+tau1y**2.)**0.5
	if(tau1y .ne. 0.  .and.  tau1x .eq. 0) then
	  phi1t=0.
      else
cipk jun02        phi1t=(180./pi)*atan(tau1y/tau1x)
        phi1t=(180./pi)*atan2(tau1y,tau1x)
	endif
      sbx=sbx/(ntime-1)
      sby=sby/(ntime-1)
c
c   TRANSPORT VECTORS
c
c   SUSPENDED LOAD TRANSPORT
c
      ssx=ssw*cos(phi)+ssc
      ssy=ssw*sin(phi)
      ssvec=(ssx**2.+ssy**2.)**0.5
      phis1=(180./pi)*atan(ssy/ssx)
      phis2=(180./pi)*phi-phis1
c
c   BED LOAD TRANSPORT
c
      sbvec=(sbx**2.+sby**2.)**0.5
      sbc=sbx-sby/tan(phi)
      sbw=sby/sin(phi)
cipk jun02      phib1=(180./pi)*atan(sby/sbx)
      if(sbx .eq. 0.  .and.  sby .eq. 0.) then
        phib1=0.
	else
        phib1=(180./pi)*atan2(sby,sbx)
	endif
      phib2=(180./pi)*phi-phib1
c
c   TOTAL LOAD TRANSPORT
c
      stx=ssx+sbx
      sty=ssy+sby
      stvec=(stx**2.+sty**2.)**0.5
cipk jun02      phit1=(180./pi)*atan(sty/stx)
      if(stx .eq. 0.  .and.  sty .eq. 0.) then
	  phit1=0.
	else
        phit1=(180./pi)*atan2(sty,stx)
	endif
      phit2=(180./pi)*phi-phit1
      stc=stx-sty/tan(phi)
      stw=sty/sin(phi)
	if(stx .gt. 0.) then
        stcc=stx
	else
	  stcc=0.
	endif
c           HDTM=0.5
c	      IF(HDS .LT. HDTM) HDTM=HDS
c           GP(1)=STVEC/(HDTM*VRR*1000./5.)

ccc      GP(1)=STVEC/(HDS*VRR*1000.)

      GP(1)=STCC/(HDS*VRR*1000.)
	TRLOC=STCC


CIPK JUN02
      IF(abs(ISWT) .ne. 1) RETURN

      if(iswt .eq. 1) then
      WRITE(75,'('' DSTER = PARTICLE PARAMETER               [   -  ] ''
     *,E10.4)')DSTER
      WRITE(75,'('' L     = WAVE LENGTH                      [  M   ] ''
     *,E10.4)')RLS
      WRITE(75,'('' TR    = RELATIVE WAVE PERIOD             [  S   ] ''
     *,E10.4)')TP1
      WRITE(75,'('' UBW   = PEAK ORBITAL VELOCITY            [ M/S  ] ''
     *,E10.4)')UBW
      WRITE(75,'('' UBWF  = PEAK ORBITAL VELOCITY FORWARD    [ M/S  ] ''
     *,E10.4)')UBWFOR
      WRITE(75,'('' UBWB  = PEAK ORBITAL VELOCITY BACKWARD   [ M/S  ] ''
     *,E10.4)')UBWBACK
      WRITE(75,'('' ASYM  = ASYMMETRY FACTOR PEAK ORB. VEL.  [  -   ] ''
     *,E10.4)')ASYM
      WRITE(75,'('' ABW   = PEAK ORBITAL EXCURSION AT BED    [  M   ] ''
     *,E10.4)')ABW
      WRITE(75,'('' DELW  = THICKNESS WAVE BOUNDARY LAYER    [  M   ] ''
     *,E10.4)')DELW
      WRITE(75,'('' DELM  = THICKNESS WAVE MIXING LAYER      [  M   ] ''
     *,E10.4)')DELM
      WRITE(75,'('' DS    = THICKNESS SEDIMENT MIXING LAYER  [  M   ] ''
     *,E10.4)')DS
      WRITE(75,'('' TAUW  = WAVE-RELATED BED-SHEAR STRESS    [ N/M2 ] ''
     *,E10.4)')TAUW
      WRITE(75,'('' TAUC  = CURRENT-RELATED BED-SHEAR STRESS [ N/M2 ] ''
     *,E10.4)')TAUC
      WRITE(75,'('' FW    = WAVE-RELATED FRICTION COEFFICIENT[  -   ] ''
     *,E10.4)')FW
      WRITE(75,'('' FW1   = WAVE-RELATED GRAIN FRIC. COEFF.  [  -   ] ''
     *,E10.4)')FW1 
      WRITE(75,'('' FC    = CURR-RELATED FRICTION COEFFICIENT[  -   ] ''
     *,E10.4)')FC
      WRITE(75,'('' FC1   = CURR-RELATED GRAIN FRIC.COEFF.   [  -   ] ''
     *,E10.4)')FC1
      WRITE(75,'('' C     = CHEZY COEFFICIENT                [M0.5/S] ''
     *,E10.4)')CC
      WRITE(75,'('' C1    = CHEZY GRAIN COEFFICIENT          [M0.5/S] ''
     *,E10.4)')CC1
      WRITE(75,'('' RA    = APPARENT ROUGHNESS               [  M   ] ''
     *,E10.4)')RA
      WRITE(75,'('' ALFAW = WAVE-CURRENT COEFFICIENT         [  -   ] ''
     *,E10.4)')ALFAW
      WRITE(75,'('' RHOW  = FLUID DENSITY                    [KG/M3 ] ''
     *,E10.4)')RHOW
      WRITE(75,'('' RHOS  = DENSITY SEDIMENT MATERIAL        [KG/M3 ] ''
     *,E10.4)')RHOS
      WRITE(75,'('' WS    = FALL VELOCITY SUSP. MATERIAL     [ M/S  ] ''
     *,E10.4)')WS
      WRITE(75,'('' WSB   = FALL VELOCITY BED MATERIAL       [ M/S  ] ''
     *,E10.4)')WSB
      WRITE(75,'('' TAUCR = CRITICAL BED-SHEAR STRESS        [ N/M2 ] ''
     *,E10.4)')TAUCR
      WRITE(75,'('' UCR   = CRITICAL DEPTH-AVERAGED VELOCITY [ M/S  ] ''
     *,E10.4)')UMCR
      WRITE(75,'('' UBWCR = CRITICAL PEAK ORBITAL VELOCITY   [ M/S  ] ''
     *,E10.4)')UBWCR
      WRITE(75,'('' UC    = C-RELATED EFFICIENCY FACTOR      [  -   ] ''
     *,E10.4)')RMUC
      WRITE(75,'('' UWA   = W-RELATED EFF.FAC. CONCENTRATION [  -   ] ''
     *,E10.4)')RMUWA
      WRITE(75,'('' UW    = W-RELATED EFF.FAC. BED LOAD TR.  [  -   ] ''
     *,E10.4)')RMUW
      WRITE(75,'('' TA    = BED-SHEAR STRESS PAR.CONCENTR.   [  -   ] ''
     *,E10.4)')TA
      WRITE(75,'('' T     = BED-SHEAR STRESS PAR.BED LOAD TR [  -   ] ''
     *,E10.4)')T
      WRITE(75,'('' MFOR  = MOBILITY NUMBER FORWARD          [  -   ] ''
     *,E10.4)')RMFOR
      WRITE(75,'('' MBACK = MOBILITY NUMBER BACKWARD         [  -   ] ''
     *,E10.4)')RMBACK
      WRITE(75,'('' UR    = RETURN VELOCITY IN WAVE DIR.      [ M/S ] ''
     *,E10.4)')UR
      WRITE(75,'('' UB    = NEAR-BED VELOCITY IN WAVE DIR.    [ M/S ] ''
     *,E10.4)')UB
      WRITE(75,'('' VRDELM= VEL.AT EDGE W.MIX.LAYER, CURR DIR [ M/S ] ''
     *,E10.4)')VRDELM
      WRITE(75,'('' URDELM= VEL.AT EDGE W.MIX.LAYER, WAVE DIR [ M/S ] ''
     *,E10.4)')URDELM
      WRITE(75,'('' UBTOT = UB+URDELM=TOTAL VEL. AT EDGE W.M.L[ M/S ] ''
     *,E10.4)')UBTOT
      WRITE(75,'(///)')
      WRITE(75,'(''1. BED LOAD TRANSPORT '')')
      WRITE(75,'(/)')
      WRITE(75,'(''   NET GRAIN BED SHEAR STRESS VECTOR    (N/M2)   ='',
     *E10.4)')TAU1NET
      WRITE(75,'(''   ANGLE BETWEEN CURRENT DIR. AND STRESS VECTOR  ='',
     *E10.4)')PHI1T
      WRITE(75,'(''   BED LOAD TRANSPORT VECTOR            (KG/SM)  ='',
     *E10.4)')SBVEC
      WRITE(75,'(''   ANGLE BETWEEN CURRENT DIR. AND B.L.TR. VECTOR ='',
     *E10.4)')PHIB1
      WRITE(75,'(''   ANGLE BETWEEN B.L.TR.VECTOR AND WAVE DIRECTION='',
     *E10.4)')PHIB2
      WRITE(75,'(''   BED L. TRANSP.COMPONENT IN CURR. DIR.(KG/SM)  ='',
     *E10.4)')SBC
      WRITE(75,'(''   BED L. TRANSP.COMPONENT IN WAVE DIR. (KG/SM)  ='',
     *E10.4)')SBW
      WRITE(75,'('' ( POS. ANGLE = ANTI-CLOCKWISE,NEG.=CLOCKWISE ) '')')
      WRITE(75,'(//)')
      WRITE(75,'(''2. SUSPENDED LOAD TRANSPORT '')')
      WRITE(75,'(/)')
      WRITE(75,'(''   SUSPENDED LOAD TRANSPORT VECTOR      (KG/SM)  ='',
     *E10.4)')SSVEC
      WRITE(75,'(''   ANGLE BETWEEN CURRENT DIR. AND S.L.TR. VECTOR ='',
     *E10.4)')PHIS1
      WRITE(75,'(''   ANGLE BETWEEN S.L.TR.VECTOR AND WAVE DIRECTION='',
     *E10.4)')PHIS2
      WRITE(75,'(''   SUSP. L. TR. COMPONENT IN CURR. DIR. (KG/SM)  ='',
     *E10.4)')SSC
      WRITE(75,'(''   SUSP. L. TR. COMPONENT IN WAVE DIR.  (KG/SM)  ='',
     *E10.4)')SSW
      WRITE(75,'(//)')
      WRITE(75,'(''3. TOTAL LOAD TRANSPORT '')')
      WRITE(75,'(/)')
      WRITE(75,'(''   TOTAL LOAD TRANSPORT VECTOR           (KG/SM) ='',
     *E10.4)')STVEC
      WRITE(75,'(''   ANGLE BETWEEN CURRENT DIR. AND T.L.TR.VECTOR  ='',
     *E10.4)')PHIT1
      WRITE(75,'(''   ANGLE BETWEEN T.L.TR.VECTOR AND WAVE DIRECTION='',
     *E10.4)')PHIT2
      WRITE(75,'(''   TOTAL L. TR. COMPONENT IN CURRENT DIR.(KG/SM) ='',
     *E10.4)')STC
      WRITE(75,'(''   TOTAL L. TR. COMPONENT IN WAVE DIR.   (KG/SM) ='',
     *E10.4)')STW
	else
CIPK MAR06
C	  write(108,'(i5,4f10.3,3f10.2)') 
C     +   node,stvec,stx,sty,stc,stw,STCC/(HDS*VRR)*1000.,rmat(1,2)
	endif
	RETURN
      END

      SUBROUTINE ZEROIN(X,Y,LOG,F,TOLX)
C***********************************************************************
C
C     ZEROIN LEVERT IN X DE BESTE BENADER ING VAN DE WORTEL VAN
C     DE VERGELIJKING F(X)=0. OP HET INTERVAL (X,Y) GEGEVEN IN DE BEGIN
C     WAARDEN VAN X ENY.
C     INDIEN ER GEEN WORTEL IS LOG=.FALSE.
C     ZEROIN ROEPT FLAMBDA EN TOLLD
C
C***********************************************************************
      LOGICAL LOG
      A=X
      FA=F(X)
      B=Y
      X=Y
      FB=F(X)
C INTERPOLATE
    1 C=A
      FC=FA
C EXTRAPOLATE:
    2 IF(ABS(FC).GE.ABS(FB))GOTO 19
      A=B
      FA=FB
      X=C
      B=C
      FB=FC
      C=A
      FC=FA
C END INTERCHANGE
   19 TOL=TOLX(X)
      RM=(C+B)*.5
      IF(ABS(RM-B).LE.TOL)GOTO 200
      P=(B-A)*FB
      IF(P.LT.0.)GOTO 5
      Q=FA-FB
      GOTO 6
    5 Q=FB-FA
      P=-P
    6 A=B
      FA=FB
      IF(P.GT.ABS(Q)*TOL)GOTO 8
      IF(C.LE.B)GOTO 10
      B=B+TOL
      X=B
      GOTO 11
   10 B=B-TOL
      X=B
   11 GOTO 12
    8 IF(P.GE.(RM-B)*Q)GOTO 14
      B=P/Q+B
      X=B
      GOTO 12
   14 B=RM
      X=B
   12 FB=F(X)
      IF(FC.LT.0.)GOTO 16
      IF(FB.LT.0.)GOTO 18
      GOTO 1
   18 GOTO 2
   16 IF(FB.GT.0.)GOTO 21
      GOTO 1
   21 GOTO 2
  200 Y=C
      IF(FC.LT.0.)GOTO 31
      IF(FB.GT.0.)GOTO 33
      LOG=.TRUE.
      GOTO 300
   33 LOG=.FALSE.
      GOTO 300
   31 IF(FB.LT.0.)GOTO 35
      LOG=.TRUE.
      GOTO 300
   35 LOG=.FALSE.
  300 RETURN
      END
      function fwl(rls)
      USE NIOREZMOD
      fwl=(rls/tp-vr*cos(phi))**2-g*rls/2./pi*tanh(2.*pi*hd/rls)
      return
      end
      function tolx(x)
      tolx=.001*abs(x)+1.e-5
      return
      end
