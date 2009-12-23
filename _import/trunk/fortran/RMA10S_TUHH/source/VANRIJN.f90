!     Last change:  MD   12 Aug 2008    8:12 pm
!IPK LAST UPDATE MAR 05 2006 UPDATE TO ALLOW FOR NODAL VALUES ADD NODNUM
      subroutine vanrijn (VELS,EFD,EXNU,u_star,NODNUM)
!
!  sediment transport by van Rijn (1993) method
!  Principles of Sediment Transport in Rivers
!  Estuaries and Coastal Seas, Aqua Publications, Rotterdam.
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
! implemented within RMA-11 by Bill Peirson    31.5.96
!) but not very efficient
! bug free (I think
      USE BLKSANMOD
!
      integer i,ie,is,loops,j
        real gravity,xmanning_n,sed_rd,diam,vels,efd
      real depth,u_bar,tau_crit,fluid_rho
      real part_a,part_b,c_bar,exnu,gnsg,dgr
      real Tstress,tau,u_star,gptot,gd50
      real tmpsd,chezy_c,theta_crit,u_crit
      real c_bar_bed,c_bar_susp
      logical first_pass
! d's are defined in mm in water quality file   WLP 31.5.96
      real d50m
      real d90m
!
      gravity = ACGR
!IPKMAR06
      xmanning_n = AMANNND(NODNUM)
      sed_rd = SGSAND(NODNUM)
      viscosity = EXNU
      depth = EFD
      u_bar = VELS
      fluid_rho = RHOF
!IPK MAR06
!MD  d50m=d50ND(NODNUM)/1000.      
!MD  Umrechnung spaeter!!      
      d90m=d90ND(NODNUM)/1000.
!
!  start calculations
!
      gnsg = (sed_rd - 1) * gravity
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
!IPK MAR06
      tmpsd = SDND(1,NODNUM)
      SDND(1,NODNUM) = d50ND(NODNUM)
      is = 1
      ie = 1
!
      if ( igs == lgs ) then
        loops = 1
      else
        loops = 2
      end if
!
        do j = 1, loops
          do i = is, ie
!IPK MAR06
!MD   diam=SDND(i,NODNUM)      
            diam=SDND(i,NODNUM)/1000.
!!
!           diameter is in mm
!  find non-dimensional grain size
!
            dgr = diam *(gnsg**(1./3.)) / (viscosity**(2./3.))
!
!-----------------
!    BED LOAD
!-----------------
!
!  get u_star via Manning's equation
!
!IPK MAR06 ADD NODNUM TO CALL
          call get_u_star(depth,u_star,u_bar,NODNUM)
!
!
!MD  Deactivated: 28.07.2008
!MD      chezy_C = depth**(1.0/6.0) / xmanning_n
!
!MD  Deactivating the extra use of, MANNING's for Sediment.
!MD  Use now, the already given Friction law and Friction
!MD  Parameters:
!
!MD  Find Chezy C and applied stress tau
         IF (FFACT_KN(NODNUM) <= 0) THEN
           chezy_C = 0.0
           tau = 0.0
         Else
           chezy_C = SQRT (gravity / FFACT_KN(NODNUM))
           tau = fluid_rho * gravity * (u_bar/chezy_C)**2.0
         END IF
! MD  tau = fluid_rho * gravity * (u_bar/chezy_C)**2.0
!
!  find value of entrainment function from Shields diagram
!  reconstructed by WLP
!MD  Deaktivierung der alten Shieldsfunktion
!MD          if ( dgr <= 1.0 ) then
!MD            write(*,'(/,'' non dimensional grain size'',
!MD     1            '' is too small in Van Rijn '')')
!MD            stop 110
!MD          else if ( dgr <= 4.0 ) then
!MD            theta_crit = 0.24 * dgr**(-1.0)
!MD          else if ( dgr <= 10.0 ) then
!MD            theta_crit = 0.14 * dgr**(-0.64)
!MD          else if ( dgr <= 20.0 ) then
!MD            theta_crit = 0.04 * dgr**(-0.1)
!MD          else if ( dgr <= 150.0 ) then
!MD            theta_crit = 0.013 * dgr**0.29
!MD          else
!MD            theta_crit = 0.055
!MD          end if
!
!MD  Neuer Ansatz fuer Shieldsfunktion mit glattem Ubergang
!MD  der Funktionen groesser und kleiner der Grenzwerte
          if ( dgr <= 1.0 ) then
            write(*,'(/,'' non dimensional grain size'',                &
     &            '' is too small in Van Rijn '')')
            stop 110
          else if ( dgr <= ((2.4/1.4)**(1.0/0.36))) then
            theta_crit = 0.24 * dgr**(-1.0)
          else if ( dgr <= ((1.4/0.4)**(1.0/0.54))) then
            theta_crit = 0.14 * dgr**(-0.64)
          else if ( dgr <= ((4.0/1.3)**(1.0/0.39))) then
            theta_crit = 0.04 * dgr**(-0.1)
          else if ( dgr <= ((5.5/1.3)**(1.0/0.29))) then
            theta_crit = 0.013 * dgr**0.29
          else
            theta_crit = 0.055
          end if
!
!  find critical stress
!
!MD  fuer alle Fraktionen!!            
!MD  tau_crit = fluid_rho * gnsg * d50m * theta_crit          
            tau_crit = fluid_rho * gnsg * diam * theta_crit
!  determine effective normalised working stress T
!
          Tstress = ( tau - tau_crit ) / tau_crit 
          if ( Tstress < 0.0 ) Tstress = 0.0
!
!  determine bed sediment transport as m2s-1 - eq 7.2.44
!
!MD          if ( Tstress < 3.0 ) then
!
!MD  Neuer Ansatz fuer Tstress mit glattem Ubergang der
!MD  Funktionen groesser und kleiner Tstress
          if ( Tstress < ((1.0/0.53)**(1.0/0.6))) then
!MD  c_bar = 0.053 * gnsg**0.5 * d50m**1.5            
            c_bar = 0.053 * (gnsg**0.5) * (diam**1.5)
            c_bar = c_bar * dgr**(-0.3)
            c_bar = c_bar * Tstress**2.1
! combining the above two lines created a bug  WLP 31.5.96
          else
!MD  c_bar = 0.1 * gnsg**0.5 * d50m**1.5          
              c_bar = 0.1 * gnsg**0.5 * diam**1.5
            c_bar = c_bar * dgr**(-0.3 )
            c_bar = c_bar * Tstress**1.5
          end if
          c_bar_bed = c_bar
!
!----------------------
!    SUSPENDED LOAD
!----------------------
!
!  find critical velocity - eq 7.2.47
!
          u_crit =  ALOG10( 12.0 * depth / ( 3.0*d90m ) )
!
          if ( diam < 0.0001 ) then
            write(*,'(/,'' ## non dimensional grain size'',             &
     &            '' is too small ## '')')
            stop 111
          else if ( diam < 0.0005 ) then
            u_crit = u_crit * 0.19 * diam**0.1
          else if ( diam < 0.002 ) then
            u_crit = u_crit * 8.5 * diam**0.6
          else
            write(*,'(/,'' ## non dimensional grain size'',             &
     &            '' is too large ## '')')
            stop 114
          end if
!
!  calculate suspended load in m2/s - eq 7.3.46
!
          part_a = 0.012 * u_bar * depth * (diam/depth) * dgr**(-0.6)
            IF(U_BAR > U_CRIT) THEN
              part_b = ( ( u_bar - u_crit )/(gnsg * diam)**0.5 )**2.4
            ELSE
              PART_B=0.
            ENDIF
          c_bar_susp = part_a * part_b
!
!----------------------
!    COMBINED LOAD
!----------------------
!
          c_bar = c_bar_susp + c_bar_bed
!
!  convert to kg sediment per kg fluid from m2/s
!
            IF(U_BAR < 0.0001) THEN
            C_BAR=0.
            ELSE
          c_bar = c_bar * sed_rd / ( u_bar * depth )
            ENDIF
!
!  note that ppmax is really in units of kg sediment per kg fluid
!
          if ( c_bar > ppmmax ) c_bar = ppmmax
!
          gp(i) = c_bar
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
!MD fuer nur eine Kornfraktion        
        Elseif (igs == lgs) then
          gp(1) = gd50
      end if
!      write(lout,*) dgr,u_crit ,gnsg,c_susp,theta_crit,Tstress
!     + ,tau,tau_crit,fluid_rho,c_bar,gp(1),igs,lgs
!     + ,c_bar_bed, c_bar_susp
!
      return
      end
