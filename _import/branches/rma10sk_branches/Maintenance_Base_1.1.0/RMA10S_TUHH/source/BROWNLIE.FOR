C     Last change:  MD    2 Sep 2008    1:50 pm
CIPK  LAST UPDATE MAR 05 2006 ADD NODNUM TO CALL AND USE NODAL PROPS
	subroutine brownlie (VELS,EFD,EXNU,u_star,nodnum)
C
C  sediment transport by van Rijn (1993) method
C
C  David Luketina   26/5/96
C
C  input variables are:
C  
C  VELS		mean velocity
C  EFD		depth
C  D50		d50 size (m) of sediment mixture
C  D90		d90 size (m) of sediment mixture
C  EXNU		kinematic viscosity (m2/s)
C
      USE BLKSANMOD
CIPK AUG05      include 'PARAM.COM'
CIPK AUG05      include 'BLKSAND.COM'
	integer i,ie,is,loops,j
        real gravity,xmanning_n,sed_rd,diam,vels,efd
	real depth,u_bar,cF,fluid_rho,slope
	real Fg,Fg_crit,exnu,gnsg,Y,gptot,gd50
	real tmpsd,theta_crit,c_bar,geom_dev
	real u_star
c not used   ^^^^^^  WLP
c d's are defined in mm in water quality file
	real d50m
	real d90m
	logical first_pass
CIPK MAR06
        TMPSD=sdND(1,NODNUM)

C
	u_star=0
	gravity = ACGR
CIPK MAR06
	xmanning_n = AMANNND(nodnum)
	sed_rd = SGSAND(nodnum)
	viscosity = EXNU
	depth = EFD
	u_bar = VELS
	fluid_rho = RHOF
CIPK MAR06
      	d50m=d50ND(NODNUM)/1000.
	d90m=d90ND(NODNUM)/1000.
C
C  get u_star via Manning's equation
C
CIPK MAR06  ADD CALL TO NODNUM
      call get_u_star(depth,u_star,u_bar,NODNUM)
C
C  start calculations
C
	gnsg = (sed_rd - 1) * gravity
	cF = 1.268
C
C  estimate geometric standard deviation
C
	geom_dev = ( ALOG(d90m) - ALOG(d50m) ) / 1.29
C
C  the outside loop has either one or two loops.
C
C  if there is only one grain size there will be a single
C  outside loop.
C
C  if there is more than one grain size, the first pass of the 
C  outside loop will handle the d50 case and the other (second)
C  pass will handle the other grain sizes (from array pointer
C  IGS to LGS).
C
C  the inside loop, loops once for each grain size (obviously,
C  in the d50 case there can only be a single inner loop).
C
C  set parameters for the first pass - d50 case
C
	first_pass = .true.
	gptot = 0.0
	is = 1
	ie = 1
CIPK MAR06
	tmpsd = SDND(1,NODNUM)
	SDND(1,NODNUM) = d50ND(NODNUM)
C
	if ( igs.eq.lgs ) then
	  loops = 1
	else
	  loops = 2
	end if
C
        do j = 1, loops
          do i = is, ie
C
c reinvoke following line of multiple sediment sizes are used
            diam=SDND(i,NODNUM)/1000.
!MD            diam=d50m
C
C  stop if sediment diameters are such
C  that the formulae is out of range
C
cc	    if ( diam.lt.0.00006 ) then
cc	      write(*,'(/,'' sediment diameter less than'',
cc     1		'' 0.060 mm encountered '')')
cc	      stop 112
cc	    end if
C
C  find friction slope
C
	    slope = ( xmanning_n * u_bar / depth**(2.0/3.0) )**2.0
C
C  find Y parameter
C
	    Y = ( ( gnsg * diam**3.0 )**0.5 / viscosity )**(-0.6)
C
C  find entrainment values as per Shields diagram
C
	    theta_crit = 0.22*Y + 0.06*10.0**(-7.7*Y)
C
C  find mobility
C
	    Fg = u_bar / ( gnsg * diam )**0.5
C
C  find threshold mobility
C
	    if (slope.le.0) then
		Fg_crit=0
	        c_bar = 0.0
	    else
		Fg_crit = 4.596 * theta_crit**0.5293 / slope**0.1405
		Fg_crit = Fg_crit / geom_dev**0.1606
C
C  find sediment concentration in ppm where the
C  concentration is mass per unit time of sediment
C  over mass per unit time of fluid (eq 6.9)
C
		c_bar = 7100.0 * cF * slope**(2.0/3.0)
		c_bar = c_bar * (Fg - Fg_crit)**2.0
		c_bar = c_bar * (depth/diam)**(-1.0/3.0)
	    end if
C
C  convert to kg sediment per kg fluid from ppm
C
	    c_bar = c_bar / 1.0e6
	    if ( c_bar.gt.ppmmax ) c_bar = ppmmax
C
c  the original RMA code was set up for multiple sediment sizes then cut down.
c  All I can do is just make sure one sediment size works correctly.
c  WLP  31.5.96
	    iI=1
	    gp(iI) = c_bar
	    gptot = gptot + gp(i)
C
	  end do
C
	  if ( first_pass ) then
            gd50 =GP(1)
CIPK MAR06
            SDND(1,NODNUM)=TMPSD
	    gptot = 0.0
	    is = igs
	    ie = lgs
	    first_pass = .false.
	  end if
C
	end do
C
C  normalise the transport at each grain size so that
C  the total transport must equal that predicted when a 
C  d35 grain size is used for the whole mixture
C
	if ( igs.ne.lgs ) then
C
C  ie  more than one grain size
	  do i = igs, lgs
	    gp(i) = (gd50/gptot) * gp(i)
	  end do

        !MD fuer nur eine Kornfraktion
        Elseif (igs.eq.lgs) then
          gp(1) = gd50
	end if
C
c	write(lout,*) Y,gnsg,diam,viscosity,cF,slope,Fg,Fg_crit,depth,
c     + c_bar,gp(1),igs,lgs
	return
	end
