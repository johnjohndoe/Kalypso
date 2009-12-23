!     Last change:  MD    2 Sep 2008    1:50 pm
!IPK  LAST UPDATE MAR 05 2006 ADD NODNUM TO CALL AND USE NODAL PROPS
      subroutine brownlie (VELS,EFD,EXNU,u_star,nodnum)
!
!  sediment transport by van Rijn (1993) method
!
!  David Luketina   26/5/96
!
!  input variables are:
!  
!  VELS            mean velocity
!  EFD            depth
!  D50            d50 size (m) of sediment mixture
!  D90            d90 size (m) of sediment mixture
!  EXNU            kinematic viscosity (m2/s)
!
      USE BLKSANMOD
!
      integer i,ie,is,loops,j
        real gravity,xmanning_n,sed_rd,diam,vels,efd
      real depth,u_bar,cF,fluid_rho,slope
      real Fg,Fg_crit,exnu,gnsg,Y,gptot,gd50
      real tmpsd,theta_crit,c_bar,geom_dev
      real u_star
! not used   ^^^^^^  WLP
! d's are defined in mm in water quality file
      real d50m
      real d90m
      logical first_pass
!IPK MAR06
        TMPSD=sdND(1,NODNUM)
!
!
      u_star=0
      gravity = ACGR
!IPK MAR06
      xmanning_n = AMANNND(nodnum)
      sed_rd = SGSAND(nodnum)
      viscosity = EXNU
      depth = EFD
      u_bar = VELS
      fluid_rho = RHOF
!IPK MAR06
            d50m=d50ND(NODNUM)/1000.
      d90m=d90ND(NODNUM)/1000.
!
!  get u_star via Manning's equation
!
!IPK MAR06  ADD CALL TO NODNUM
      call get_u_star(depth,u_star,u_bar,NODNUM)
!
!  start calculations
!
      gnsg = (sed_rd - 1) * gravity
      cF = 1.268
!
!  estimate geometric standard deviation
!
      geom_dev = ( ALOG(d90m) - ALOG(d50m) ) / 1.29
!
!  the outside loop has either one or two loops.
!
!  if there is only one grain size there will be a single
!  outside loop.
!
!  if there is more than one grain size, the first pass of the 
!  outside loop will handle the d50 case and the other (second)
!  pass will handle the other grain sizes (from array pointer
!  IGS to LGS).
!
!  the inside loop, loops once for each grain size (obviously,
!  in the d50 case there can only be a single inner loop).
!
!  set parameters for the first pass - d50 case
!
      first_pass = .true.
      gptot = 0.0
      is = 1
      ie = 1
!IPK MAR06
      tmpsd = SDND(1,NODNUM)
      SDND(1,NODNUM) = d50ND(NODNUM)
!
      if ( igs == lgs ) then
        loops = 1
      else
        loops = 2
      end if
!
        do j = 1, loops
          do i = is, ie
!
! reinvoke following line of multiple sediment sizes are used
            diam=SDND(i,NODNUM)/1000.
!MD            diam=d50m
!
!  stop if sediment diameters are such
!  that the formulae is out of range
!
!c          if ( diam < 0.00006 ) then
!c            write(*,'(/,'' sediment diameter less than'',
!c     1            '' 0.060 mm encountered '')')
!c            stop 112
!c          end if
!
!  find friction slope
!
          slope = ( xmanning_n * u_bar / depth**(2.0/3.0) )**2.0
!
!  find Y parameter
!
          Y = ( ( gnsg * diam**3.0 )**0.5 / viscosity )**(-0.6)
!
!  find entrainment values as per Shields diagram
!
          theta_crit = 0.22*Y + 0.06*10.0**(-7.7*Y)
!
!  find mobility
!
          Fg = u_bar / ( gnsg * diam )**0.5
!
!  find threshold mobility
!
          if (slope <= 0) then
            Fg_crit=0
              c_bar = 0.0
          else
            Fg_crit = 4.596 * theta_crit**0.5293 / slope**0.1405
            Fg_crit = Fg_crit / geom_dev**0.1606
!
!  find sediment concentration in ppm where the
!  concentration is mass per unit time of sediment
!  over mass per unit time of fluid (eq 6.9)
!
            c_bar = 7100.0 * cF * slope**(2.0/3.0)
            c_bar = c_bar * (Fg - Fg_crit)**2.0
            c_bar = c_bar * (depth/diam)**(-1.0/3.0)
          end if
!
!  convert to kg sediment per kg fluid from ppm
!
          c_bar = c_bar / 1.0e6
          if ( c_bar > ppmmax ) c_bar = ppmmax
!
!  the original RMA code was set up for multiple sediment sizes then cut down.
!  All I can do is just make sure one sediment size works correctly.
!  WLP  31.5.96
          iI=1
          gp(iI) = c_bar
          gptot = gptot + gp(i)
!
        end do
!
        if ( first_pass ) then
            gd50 =GP(1)
!IPK MAR06
            SDND(1,NODNUM)=TMPSD
          gptot = 0.0
          is = igs
          ie = lgs
          first_pass = .false.
        end if
!
      end do
!
!  normalise the transport at each grain size so that
!  the total transport must equal that predicted when a 
!  d35 grain size is used for the whole mixture
!
      if ( igs /= lgs ) then
!
!  ie  more than one grain size
        do i = igs, lgs
          gp(i) = (gd50/gptot) * gp(i)
        end do
!
!MD fuer nur eine Kornfraktion        
        Elseif (igs == lgs) then
          gp(1) = gd50
      end if
!
!      write(lout,*) Y,gnsg,diam,viscosity,cF,slope,Fg,Fg_crit,depth,
!     + c_bar,gp(1),igs,lgs
      return
      end
