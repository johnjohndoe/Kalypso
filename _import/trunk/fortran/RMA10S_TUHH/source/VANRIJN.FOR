C     Last change:  MD   12 Aug 2008    8:12 pm
CIPK LAST UPDATE MAR 05 2006 UPDATE TO ALLOW FOR NODAL VALUES ADD NODNUM
	subroutine vanrijn (VELS,EFD,EXNU,u_star,NODNUM)
C
C  sediment transport by van Rijn (1993) method
C  Principles of Sediment Transport in Rivers
C  Estuaries and Coastal Seas, Aqua Publications, Rotterdam.
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
c implemented within RMA-11 by Bill Peirson    31.5.96
c bug free (I think!) but not very efficient
      USE BLKSANMOD

	integer i,ie,is,loops,j
        real gravity,xmanning_n,sed_rd,diam,vels,efd
	real depth,u_bar,tau_crit,fluid_rho
	real part_a,part_b,c_bar,exnu,gnsg,dgr
	real Tstress,tau,u_star,gptot,gd50
	real tmpsd,chezy_c,theta_crit,u_crit
	real c_bar_bed,c_bar_susp
	logical first_pass
c d's are defined in mm in water quality file   WLP 31.5.96
	real d50m
	real d90m
C
	gravity = ACGR
CIPKMAR06
	xmanning_n = AMANNND(NODNUM)
	sed_rd = SGSAND(NODNUM)
	viscosity = EXNU
	depth = EFD
	u_bar = VELS
	fluid_rho = RHOF
CIPK MAR06
      !MD  d50m=d50ND(NODNUM)/1000.
      !MD  Umrechnung spaeter!!
	d90m=d90ND(NODNUM)/1000.
C
C  start calculations
C
	gnsg = (sed_rd - 1) * gravity
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
CIPK MAR06
	tmpsd = SDND(1,NODNUM)
	SDND(1,NODNUM) = d50ND(NODNUM)
	is = 1
	ie = 1
C
	if ( igs.eq.lgs ) then
	  loops = 1
	else
	  loops = 2
	end if
C
        do j = 1, loops
          do i = is, ie
CIPK MAR06
      !MD   diam=SDND(i,NODNUM)
            diam=SDND(i,NODNUM)/1000.
c           diameter is in mm!!
C  find non-dimensional grain size
C
            dgr = diam *(gnsg**(1./3.)) / (viscosity**(2./3.))
C
C-----------------
C    BED LOAD
C-----------------
C
C  get u_star via Manning's equation
C
CIPK MAR06 ADD NODNUM TO CALL
	    call get_u_star(depth,u_star,u_bar,NODNUM)
C

!MD  Deactivated: 28.07.2008
!MD      chezy_C = depth**(1.0/6.0) / xmanning_n

!MD  Deactivating the extra use of, MANNING's for Sediment.
!MD  Use now, the already given Friction law and Friction
!MD  Parameters:

!MD  Find Chezy C and applied stress tau
         IF (FFACT_KN(NODNUM) .le. 0) THEN
           chezy_C = 0.0
           tau = 0.0
         Else
           chezy_C = SQRT (gravity / FFACT_KN(NODNUM))
           tau = fluid_rho * gravity * (u_bar/chezy_C)**2.0
         END IF
! MD  tau = fluid_rho * gravity * (u_bar/chezy_C)**2.0

C  find value of entrainment function from Shields diagram
c  reconstructed by WLP
!MD  Deaktivierung der alten Shieldsfunktion
!MD	    if ( dgr.le.1.0 ) then
!MD	      write(*,'(/,'' non dimensional grain size'',
!MD     1		'' is too small in Van Rijn '')')
!MD	      stop 110
!MD	    else if ( dgr.le.4.0 ) then
!MD	      theta_crit = 0.24 * dgr**(-1.0)
!MD	    else if ( dgr.le.10.0 ) then
!MD	      theta_crit = 0.14 * dgr**(-0.64)
!MD	    else if ( dgr.le.20.0 ) then
!MD	      theta_crit = 0.04 * dgr**(-0.1)
!MD	    else if ( dgr.le.150.0 ) then
!MD	      theta_crit = 0.013 * dgr**0.29
!MD	    else
!MD	      theta_crit = 0.055
!MD	    end if

!MD  Neuer Ansatz fuer Shieldsfunktion mit glattem Ubergang
!MD  der Funktionen groesser und kleiner der Grenzwerte
	    if ( dgr.le.1.0 ) then
	      write(*,'(/,'' non dimensional grain size'',
     1		'' is too small in Van Rijn '')')
	      stop 110
	    else if ( dgr.le. ((2.4/1.4)**(1.0/0.36))) then
	      theta_crit = 0.24 * dgr**(-1.0)
	    else if ( dgr.le. ((1.4/0.4)**(1.0/0.54))) then
	      theta_crit = 0.14 * dgr**(-0.64)
	    else if ( dgr.le. ((4.0/1.3)**(1.0/0.39))) then
	      theta_crit = 0.04 * dgr**(-0.1)
	    else if ( dgr.LE. ((5.5/1.3)**(1.0/0.29))) then
	      theta_crit = 0.013 * dgr**0.29
	    else
	      theta_crit = 0.055
	    end if
C
C  find critical stress
C
            !MD  fuer alle Fraktionen!!
	    !MD  tau_crit = fluid_rho * gnsg * d50m * theta_crit
            tau_crit = fluid_rho * gnsg * diam * theta_crit
C  determine effective normalised working stress T
C
	    Tstress = ( tau - tau_crit ) / tau_crit 
	    if ( Tstress.lt.0.0 ) Tstress = 0.0
C
C  determine bed sediment transport as m2s-1 - eq 7.2.44
C
!MD	    if ( Tstress.lt.3.0 ) then

!MD  Neuer Ansatz fuer Tstress mit glattem Ubergang der
!MD  Funktionen groesser und kleiner Tstress
	    if ( Tstress .LT. ((1.0/0.53)**(1.0/0.6))) then
            !MD  c_bar = 0.053 * gnsg**0.5 * d50m**1.5
	      c_bar = 0.053 * (gnsg**0.5) * (diam**1.5)
	      c_bar = c_bar * dgr**(-0.3)
	      c_bar = c_bar * Tstress**2.1
c combining the above two lines created a bug  WLP 31.5.96
	    else
	    !MD  c_bar = 0.1 * gnsg**0.5 * d50m**1.5
              c_bar = 0.1 * gnsg**0.5 * diam**1.5
	      c_bar = c_bar * dgr**(-0.3 )
	      c_bar = c_bar * Tstress**1.5
	    end if
	    c_bar_bed = c_bar
C
C----------------------
C    SUSPENDED LOAD
C----------------------
C
C  find critical velocity - eq 7.2.47
C
	    u_crit =  ALOG10( 12.0 * depth / ( 3.0*d90m ) )
C
	    if ( diam.lt.0.0001 ) then
	      write(*,'(/,'' ## non dimensional grain size'',
     1		'' is too small ## '')')
	      stop 111
	    else if ( diam.lt.0.0005 ) then
	      u_crit = u_crit * 0.19 * diam**0.1
	    else if ( diam.lt.0.002 ) then
	      u_crit = u_crit * 8.5 * diam**0.6
	    else
	      write(*,'(/,'' ## non dimensional grain size'',
     1		'' is too large ## '')')
	      stop 114
	    end if
C
C  calculate suspended load in m2/s - eq 7.3.46
C
	    part_a = 0.012 * u_bar * depth * (diam/depth) * dgr**(-0.6)
            IF(U_BAR .GT. U_CRIT) THEN
  	      part_b = ( ( u_bar - u_crit )/(gnsg * diam)**0.5 )**2.4
            ELSE
              PART_B=0.
            ENDIF
	    c_bar_susp = part_a * part_b
C
C----------------------
C    COMBINED LOAD
C----------------------
C
	    c_bar = c_bar_susp + c_bar_bed
C
C  convert to kg sediment per kg fluid from m2/s
C
            IF(U_BAR .LT. 0.0001) THEN
            C_BAR=0.
            ELSE
	    c_bar = c_bar * sed_rd / ( u_bar * depth )
            ENDIF
C
C  note that ppmax is really in units of kg sediment per kg fluid
C
	    if ( c_bar.gt.ppmmax ) c_bar = ppmmax
C
	    gp(i) = c_bar
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
c	write(lout,*) dgr,u_crit ,gnsg,c_susp,theta_crit,Tstress
c     + ,tau,tau_crit,fluid_rho,c_bar,gp(1),igs,lgs
c     + ,c_bar_bed, c_bar_susp
C
	return
	end
