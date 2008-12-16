SUBROUTINE RMA10SUB

USE BLK10MOD, only: &
&  niti, nita, maxn, nitn, itpas, iaccyc, icyc, ncyc, it, &
&  ioutrst, ioutfreq, ioutrwd, nprtf, nprti, &
&  maxp, maxe, maxlt, ne, np, npm, nem, nmat, &
&  ncrn, nops, imat, &
&  ao, &
&  vel, vdot, vold, vdoto, v2ol, vvel, &
&  hel, hol, hdet, hdot, h2ol, wsll, &
&  icpu, idrpt, idnopt, ioptzd, iproj, iespc, &
&  iutub, krestf, &
&  icfl, lout, ikalypsofm, ifile, nb, incstr, iwvfc, iwvin, intims, icordin, itimfl, nscr, nd1, &
&  fnam, modellein, modellrst, ct, modellaus, & 
&  iteqs, iteqv, ick, ndf, &
&  nconv, iactv, &
&  tett, tet, iyrr, &
&  atim, &
&  delt, altm, alphasn, alpha, &
&  sidff, &
&  mxsedlay, &
&  itransit, ndep, nref, dfct
!meaning of the variables
!------------------------
!niti                   number of steady state iterations
!nitn                   number of unsteady iterations read for each time step
!nita                   local copy of current maximum number of iterations in steady or unsteady run
!maxn                   current iteration cycle
!itpas                  ???
!                       TODO: delete everything, connected to ioutrst
!iaccyc:                time step to start the calculation from; counting through the list of time steps to reach the iaccyc-th step
!                       TODO: delete everything, connected to ioutrst
!icyc                   counter for really processed time steps
!ncyc                   maximum number of time steps
!it                     actual 'ID' of the time step
!ioutrst                frequency to write out restart results (obsolete?)
!                       TODO: Examine what is the difference between those two lines
!ioutfreq               frequency to write out results
!ioutrwd                frequency to rewind output file LOUT
!nprtf                  frequency of time steps to write out full results
!nprti                  frequency of iterations within nprtf to write out full results
!maxp:                  maximum number of points (surface points; includes midsides)
!maxe:                  maximum number of elements
!maxlt:                 maximum number of 1D/2D transition elements for 1D/2D line-2-element transitions
!ne                     maximum element number
!np                     maximum node number
!npm                    maximum node number; copy for 3D purposes
!nem                    maximum element number; copy for 3D purposes
!nmat                   number of material types
!ncrn                   copy of the ncorn array
!nops                   the same as nop
!imat                   material type of an element
!ao                     bed elevation of a node
!vel                    The degree of freedom field; it's the key array
!vdot                   derivative of the degrees of freedom
!vold                   The degree of freedom field from the last time step; i.e. storage of old results
!vdoto                  derivative of the degrees of freedom from the last time step; i.e. storage of old results
!v2ol                   The degree of freedom field from the penultimate time step; i.e. storage of old results
!for all counts:
!                       1 - x-velocity
!                       2 - y-velocity
!                       3 - water depth
!                       4 - salinity
!                       5 - temperature
!                       6 - suspended sediment
                        !TODO: what is vel(7,x)
!                       7 - ??? perhaps water column potential by element
!vvel                   vertical velocity; for 3D applications
!hel                    real depth; important for Marsh-nodes
!hol                    real depth of last time step; important for Marsh-nodes
!hdet                   derivative of real depth over time; important for Marsh-nodes
!hdot                   derivative of real depth over time in last time step; important for Marsh-nodes
!h2ol                   real depth of penultimate time step; important for Marsh-nodes
!wsll                   water surface elevation of node
!icpu                   switch for controlling of equation solver
!                       0  - use frontal solution scheme
!                       1+ - use pardiso solver from the MKL; icpu gives the number of cores/threads to be utilized
!ioptzd                 switch for turbulence closure method
!                       0 - original RMA-10
!                       1 - quadratic functions
!                       2 - linear functions
!idrpt                  switch for equation dropout
!                       0 - not operative
!                       TODO: Find out exactly what equation dropout does
!                       1 - dropout equations but bring all of them back, if active set of constituents changes
!                       TODO: Find out exactly what advanced equation dropout does
!                       2 - dropout advanced method; 
!idnopt                 switch for Marsh algorithm
!                       0 - use simple element drop out
!                      -1 - use default Marsh parameters (
!                      -2 - use user defined Marsh parameters
!iproj                  switch for time projection method
!                       0 - project in time based on time derivative of variable (time derivative is not changed!)
!                       1 - do not project in time
!                       2 - project based on current and previous value (purpose? faster convergence?)
!iespc                  ????      
!                       TODO: examine, how ICFL works
!iutub                  switch to switch on other turbulence options then constant eddies
!                       0 - means that certain turbulence options can not be executed.
!                       1 - means all turbulence options can be executed, i.e. after correct restart or after to iterations with constant eddies.
!                       TODO: Meaning of krestf
!krestf                 switch for situation of restarting
!                       0 - not restarted and ???
!                       1 - restarted or ???; the value is edited in the rma10.sub, too. The meaning is not clear. It is used in surcof.
!icfl:                  unit number definition of the console
!LOUT                   after 1st call of file.sub unit of output file (.ech)
!                       after 2nd call of file.sub LOUT becomes the general output unit (.out-file)
!IKALYPSOFM             unit number of the results ouput files
!IFILE                  model geometry (in *.2d-format)
!NB                     restart input file (binary or ASCII)
!INCSTR                 control structure data
!IWVFC                  input surface stress data file
!IWVIN                  input wave data file
!INTIMS                 on/off controlling of constrol structure time series
!ITIMFL                 processing time data
!                       TODO: Make one unit number from nscr and nd1
!nscr                   scratch file unit for solver purposes
!nd1                    copy of scratch file unit nscr
!fnam                   global output file name prefix for (.itr, .out, .ech, MESS.ech)
!modellein              name (string) of the input geometry file
!modellrst              name (string of the restart file
!ct                     prefix-digit of output result files in *.2d-format
!modellaus              suffix (string) of the results file in *.2d format
!iteqs                  active constituents in the iteration
!iteqv                  active equations in the iteration
!                       TODO: Examine meaning of ick
!ick                    ??? is connected to iteqs
!ndf                    active number of degrees of freedom
!nconv                  switch, that says, whehther model is converged in the time step
!                       0 - model is not converged
!                       1 - model is converged up to the variables, that had to be calculated to the iteration maxn; there might be another iteration, that
!                             has to be done later in the time step, where another variable is active and is not converged
!                       2 - all variables of the time step are fully converged
!                       TODO: Examine the usage of iactv
!iactv                  stores for each node and each degree of freedom a switch for being active
!                       0 - ???
!                       1 - ???
!                      10 - ???
!tett                   date in hours
!tet                    hours of the current day
!iyrr                   year
!atim                   array to store processing times for 1D, 2D and so on; it is used for the time-output file
!delt                   time step length [hours]
!alpha                  factor for time derivative of variables; centering (default 1.8) for hydrodynamic values
!altm                   = alpha/ delt
!alphasn                factor for time centering for constituents (default 2.0)
!sidff                  side inflow to an element
!mxsedlay               ??? - sediment purposes
!nref                   reference node, that is the top/bottom (?) node of a node column
!ndep                   number of layers per nodes; that means depending nodes on original 2D-node
!itransit               ??? - 3D purposes
!dfct                   ??? - 3D purposes

USE BLK11MOD, only: &
&  dayofy, &
&  icesw, &
&  icethk, icethkol, &
&  tmed
!meaning of the variables
!-----------------------
!dayofy     day of the year
!icesw      ice usage switch
!           0 - no usage of ice cover
!           1 - use Ashton formulation of temperature slope calculation
!           2 - use RMA10 formulation of temperature slope calculation
!icethk     ice thickness from current time step
!icethkol   ice thickness from previous time step
!tmed       medium temperature to examine status of freezing/melting

USE BLKDRMOD, only: idswt, akp, adt, adb, ado
!meaning of the variables
!-----------------------
!           TODO: How does the dry node modification testing really work?
!idswt      frequency of testing for dry node modification
!           0 - eliminate testing
!          1+ - frequency
!akp        porosity for Marsh algorithm
!adt        top elevation of the transition range in operative Marsh algorithm
!adb        bottom elevation of the transition range in operative Marsh algorithm
!ado        slot bed elevation in operative Marsh algorithm



USE PARAKalyps, only: ivegetation, itefreq, c_wr
!meaning of the variables
!------------------------
!ivegetation      calculate resistance with Darcy approach according to big vegetation elements
!itefreq          frequency to write out results between iterations
!c_wr             cWR value for each element

USE BLKSEDMOD, only: lss, nlay, thick, sst, nlayo, thicko, gbo, ssto, smval, bedorig
USE BLKSANMOD, only: lbed, lsand, delbed, elevb, tthick

implicit none

integer (kind = 4) :: ndff, ndg, idryc, iprtf
integer (kind = 4) :: i, j, k, l, m, n, kk, ll
integer (kind = 4) :: n1, n2
integer (kind = 4) :: temp_maxn, ndl
real (kind = 8) :: ta, sallowperm, salhighperm
real (kind = 8) :: d1, d2, dtfac, ame1
integer (kind = 4) :: iswt
real (kind = 8) :: thetcn
integer (kind = 4) :: ibin
!cipk aug98 add character statement
CHARACTER*48 FRST
CHARACTER*6  INUM
!nis,feb07,testing: Writing matrix
!CHARACTER (LEN = 25) :: matrixname
!CHARACTER (LEN = 30) :: matrixformat
!-
!NiS,jul06: Consistent data types for passing parameters
REAL(KIND=8) :: VTM, HTP, VH, H, HS
CHARACTER (LEN = 96) :: outputFileName, inputFileName

!nis,jan07: iostat variable for test writing purposes
INTEGER :: teststat


!called subroutines
!SECOND       : get the processor time to examine the calculation speed ???
!INITL        : allocation and initialization of the global variables defined in the modules
!INCSTRC      : reading and organizing in the control structure data from external file
!INTIMES      : reading and organizing in time series data from external file
!input        : reading all input data and managing the storage of that data in the proper arrays
!autoconverge : using autoconverge to automatically adapt the calculation parameters leading to convergence
!FileHeaders  : writes headers into different output files; moved, because of reading purposes within this subroutine
!GENT         : reads in external network parts from another file.
!cwr_init     : initializes the cwr-values during restart, if there are some values, that are already calculated and written
!FLDIR        : calculates the flow direction of 1D-elements


!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!EFa jun07, necessary for autoconverge
!      extranita = 50.
!      temp_iteqv = 0.
!      temp_iteqs = 0.
!      temp_itlvl = 0.
!      temp_iurvl = 0.
!      temp_iurvl1 = 0.
!      temp_maxnn = 0.
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!gets somehow time of execution for benchmarking
!TODO: What to do with the benchmarking time
!TA is not used at all
CALL SECOND(TA)

!allocate and initalise all variables
!------------------------------------
CALL INITL

!initialisations of global variables
!-----------------------------------
!maxn needs to be initialized first for other initialisation purposes in input.sub and following
MAXN = 1
NDF  = 6
ND1 = NSCR
ICFL  = 6

!initialisations of local variables
!----------------------------------
NDFF  = 9
IDRYC = 0
NDG   = 5

!factors concerning time derivatives
!-----------------------------------
!alpha = 1.6
ALPHA = 1.8
!alpha = 2.0
thetcn = 1./ alpha

!get input data from control file
!--------------------------------
call input (ibin)

!Input control structure data
!----------------------------
if (incstr == 20) call incstrc
!Input time series data
!----------------------------
if (intims == 22) call intimes

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !beiauto = switch for autoconverge usage; Why is it a type real switch and not integer?
!      !EFa jun07, output for autoconverge
!!      if (beiauto /= 0.) call autoconverge (-1.0)
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!nis,nov08: REMOVE FOR RMA·KALYPSO
!Remove writing to following units:
!  irmafm, ibedotismsfm, ismsfm1, ismsfm2, iwavot

!setup stress weighting; get external net
!----------------------------------------
if (icordin /= 0) call gent

!get initial cWR values, if vegetation shall be considered
!---------------------------------------------------------
if (ivegetation /= 0) then
  write(*,*)'going to cwr_init'
  call cwr_init
endif

!Establish flow directions for one dimensional elements
!------------------------------------------------------
CALL FLDIR

!------------------------------------------------------------------------------
!STEADY STATE CALCULATION   STEADY STATE CALCULATION   STEADY STATE CALCULATION
!------------------------------------------------------------------------------!C-

      !TEST FOR STEADY STATE; otherwise jump over steady calculation logic
      IF (NITI == 0) GO TO 350
      NITA = NITI

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!!      if (beiauto /= 0.) call autoconverge (1.)
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!iterate steady case; maxn is the iteration counter initilised with zero
!-----------------------------------------------------------------------
maxn = 0
SteadyIterationCycle: do
  maxn = maxn + 1

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jun07, autoconverge
!!      if (beiauto /= 0.) call autoconverge (2.)
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

  !if there is any 1D/2D line-2-element transition, calculate velocity distribution
  !--------------------------------------------------------------------------------
  if (MaxLT /= 0) call TransVelDistribution

  !Process dry nodes
  !-----------------
    IF(IDSWT /= 0) THEN
      IF(IDRYC == 0) THEN
        WRITE(*,*) 'ENTERING REWET'
        CALL REWET
        WRITE(*,*) 'ENTERING DRY'
        CALL DEL
        IDRYC=IDSWT
      ENDIF
    ENDIF

  !equation/ node dropout logic
  !-----------------------------
  IF (IDRPT == 0) THEN
    !if no dropout is desired; then set all degrees of freedom at each node
    !potentially active
    DO J = 1, NP
      DO K = 1, NDF
        !set it active
        IACTV (J, K) = 10
      ENDDO
    ENDDO
  ELSE
    !if dropout is desired go to the drpout logic
    CALL DRPOUT
  ENDIF

  !set up boundary line of active element set
  !------------------------------------------
  WRITE (*,*) 'ENTERING BLINE'
  CALL BLINE(MAXN)







      ICK=ITEQS(MAXN)+4

!cipk nov99 add optional call for transitioning

!CIPK JAN02 SET SIDFF TO ZERO
      DO N=1,NE
        SIDFF(N)=0.
      ENDDO

!cipk aug00 experimental
      IF(ITRANSIT == 1  .and. maxn < 4) CALL TWODSW

      WRITE (*,*) 'ENTERING LOAD.subroutine'
      CALL LOAD

!C-
!C......  Compute areas of continuity lines for stage flow input
!C-
      CALL AGEN
!c      DO 250 N=1,NE
!c       DFCT(N)=1.
!c  250 CONTINUE
      IF (ITEQV (MAXN) /= 5  .AND.  IOPTZD > 0 .AND. IOPTZD < 3) CALL MELLII


      !nis,feb07,testing: Write whole matrix
      !write (matrixname, '(a6,i3.3,a4)') 'matrix',maxn,'.txt'
      !teststat = 0
      !open (9919, matrixname, iostat = teststat)
      !if (teststat /= 0) STOP 'ERROR - while opening matrix file'
      !WRITE(9919,*) 'maxn', maxn
      !-


!cipk aug07
      call SECOND(ATIM(2))
      IF(ICPU == 0) THEN
        CALL FRONT(1)
      ELSE
        !stop 'option not stable, please use frontal scheme'
        CALL FRONT_PARDISO(1)
      ENDIF
      IF(ITIMFL > 0) THEN
        CALL SECOND(ATIM(3))
        WRITE(ITIMFL,6100) MAXN,ATIM(3)-ATIM(2),ATIM(3)-ATIM(1)
 6100   FORMAT('ITERATION',I5,'  TIME IN FRONT-HORZ',F12.2, ' TOTAL TIME TO DATE FOR RUN =', F12.2)
      ENDIF        

      !close testfile
      !close (9919, status = 'keep')
      !-

!CIPK JAN97
      IF(MAXN .GE. 2) THEN
        IUTUB=1
      ENDIF
!CIPK JAN97 END CHANGES

      IDRYC=IDRYC-1

      CALL UPDATE

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!      if (exterr==1.0) then
!
!        call autoconverge(3.)
!
!        cycle SteadyIterationCycle
!
!      end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!C      CALL CHECK
!C     REWIND IVS
      IF(ITEQV(MAXN) /= 5  .AND.  ITEQV(MAXN) /= 2 .AND.  ITEQV(MAXN) < 8) CALL VRTVEL
!CIPK AUG04 REVISE TEST

      IF(NPRTI == 0) THEN
        IPRTF=NITA
      ELSE
        IPRTF=NPRTI
      ENDIF
!CIPK AUG04      IPRTF=IABS(NPRTF)

      IF(MOD(MAXN,IPRTF) == 0  .OR.  MAXN == NITA .OR. NCONV == 1) THEN
         CALL OUTPUT(2)

      !testing
      WRITE(*,*) 'vor check'
      !testing-
         CALL CHECK
      !testing
      WRITE(*,*) 'nach check'
      !testing-
!CIPK MAR00
!CIPK DEC00      ELSE
!CIPK DEC00
        ENDIF

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jul07, necessary for autoconverge
!        !if (beiauto/=0.) then
!        !  if (temp_nan==1.0) then
!        !    maxn = 0.
!        !    cycle SteadyIterationCycle
!        !  end if
!        !end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!CIPK DEC00      ENDIF
!CIPK MAY96 RESTORE TETT AS HOURS IN YEAR
      TETT=(DAYOFY-1)*24.+TET


!nis,nov08: REMOVE FOR RMA·Kalypso
!Remove writing to following units:
!  NLL
!-


!CIPK NOV97      IF(NCONV == 1) GO TO 350
      IF(NCONV == 2) GO TO 350
  300 CONTINUE
      KRESTF=1
!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE STEADY STATE ITERATION, IF NOT CONVERGED
      IF (itefreq/=0) THEN
        IF (mod(maxn,itefreq)==0) THEN
          IF (IKALYPSOFM /= 0) THEN
            WRITE(*,*)' Entering write_Kalypso, steady state, after Iteration = ',maxn
           call generateOutputFileName('stat', niti, 0, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
            CALL write_KALYPSO (outputFileName, 'resu')
            WRITE(*,*)'back from write_kalypso'
          END IF
        ENDIF
      ENDIF
!-
!NiS,apr06: calculating the cwr-values for trees WITHIN THE ITERATION.
!           Calculation of actualized tree-parameters; file is only written in dependency of itefreq
        IF (IVEGETATION /= 0) THEN
          !testing
          WRITE(*,*) 'vor get_element_cwr'
          !testing-
          CALL get_element_cwr
          !testing
          WRITE(*,*) 'nach get_element_cwr'
          !testing-
        END IF

  !stop iteration, if maximum number of iterations is reached
  !----------------------------------------------------------
  IF(MAXN >= NITA) exit SteadyIterationCycle

end do SteadyIterationCycle
!C-
!C......TEST ON MAXIMUM TIME
!C-
!cipk dec97  MOVE SKIP   350 IF(NCYC > 0) GO TO 400
  350 CONTINUE
      if(niti == 0) go to 400
!*************************************************************************DJW 04/08/04
!
!     Checks Salinity Values against maximum and minimum permissible values and resets
!     to keep within an appropriate range as appropriate
!
!*************************************************************************
      SalLowPerm = 0.0001
      SalHighPerm = 300
      Do J = 1,NP
        If (Vel(4,J)<SalLowPerm) Then
          Vel(4,J) = SalLowPerm
        End If
        If (Vel(4,J)>SalHighPerm) Then
          Vel(4,J) = SalHighPerm
        End If
      End Do
!*************************************************************************END DJW 04/08/04

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,apr06: calculating the cwr-values for trees; adding temporary simulation of maxn==0
!           Updating the cwr-values for trees after convergence
        temp_maxn = maxn
        maxn = 0.
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
        maxn = temp_maxn
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if(niti == 0) go to 400

!nis,nov08: REMOVE FOR RMA·KALYPSO
!remove vsing-array for output


!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jun07, autoconverge
!        if (beiauto/=0.) then
!
!          call autoconverge(4.)
!
!          if (autoindex==1.) then
!
!            autoindex = 0.
!
!            cycle SteadyIterationCycle
!
!          end if
!
!        endif
!        !-
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!nis,nov08: REMOVE FOR RMA·KALYPSO
!Remove writing to following units:
!  irmafm
!-


!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE STEADY STATE SOLUTION
      IF (IKALYPSOFM /= 0) THEN
        !NiS,may06: initializing MaxN shows subroutine write_Kalypso, that
        !           the call is for solution printing
        temp_maxn = maxn
        MAXN = 0.
        WRITE(*,*)' Entering write_Kalypso for STEADY STATE SOLUTION.'
        call generateOutputFileName('stat', niti, 0, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
        CALL write_KALYPSO (outputFileName, 'resu')
        WRITE(*,*)'back from write_kalypso'
        MAXN = temp_maxn
      END IF
!-
!CIPK AUG02

!nis,nov08: REMOVE FOR RMA·KALYPSO
!Remove writing to following units:
!  nopt, ismsfm, ismsfm1
!-

!NiS,apr06,comment: End option, if just steady state is desired

      IF(NCYC > 0) GO TO 400
      CALL ZVRS(1)

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!      do i=1,ncl
!
!        do k=1,3
!
!          specccold(i,k)=specccfut(i,k)
!
!        end do
!
!      end do
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------


!CIPK JUL01      STOP
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,apr06: END OF STEADY STATE BLOCK!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C-
!C......DYNAMIC SOLUTION DESIRED
!C-
!C 400 NCYC=TMAX*3600./DELT+0.5
  400 CONTINUE

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jun07, autoconverge
!      if (beiauto/=0.) then
!
!        call autoconverge(5.)
!
!      end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

      IDRYC=0
!CIPK JUN02
      MAXN=0

!C-
!C......LOOP ON NUMBER OF TIME STEPS
!C-
        IF(LBED > 0) THEN
          CALL KINVIS
          CALL SANDX
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,may06: For the case of Kalypso-2D format geometry input file, there might be a later time step than the first to strart from. If so,
!           the boundary condition update is cycled till the correct boundary conditions of the step to start from is reached.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!NiS,mar06: Renaming the 800-dO-LOOP to Main_dynamic_Loop; to leave and cycle it easier.
!      DO 800 N=1,NCYC
      Main_dynamic_Loop: do n=1,ncyc
!-
!NiS,may06: calculation is only started with Kalypso-2D-geometry file, if the cycle iaccyc is reached; the user gives iaccyc in control file
!           as timestep to start from
        LaterTimestep: IF (IFILE == 60 .and. n < iaccyc) THEN
          !NiS,may06: This part of the if-clause is for simulating the update of the boundary conditions. The purpose is to read through
          !           time step date lines, that are not interesting, when starting from a later time step than the first. After reading through
          !           that data, the next time step can be read, as long as firstly the wanted starting cycle exceeds the current cycle number.
          ICYC = ICYC + 1
          CALL INPUTD(IBIN)
          WRITE(*,*) 'cycle main loop'
          CYCLE Main_dynamic_Loop
          !-
        ELSE
!CIPK MAR06  REWIND OUTPUT FILE AT SPECIFIED INTERVAL
        IF(MOD(N,IOUTRWD) == 0) THEN
           REWIND LOUT
         ENDIF
         write(*,*) 'starting cycle',n
!CIPK JUN02
        MAXN=1
        IT=N

!cipk nov99 initialize DFCT
        do j=1,ne
          dfct(j)=0.0
        enddo

!CIPK SEP96 UPDATE AT START
!CIPK REVISE TO SETUP HEL
        DO J=1,NP
          ! write(75,*) 'RMA10_778: NDF=',NDF
          DO K=1,NDF
!cipk dec00
            V2OL(K,J)=VDOTO(K,J)
            VOLD(K,J)=VEL(K,J)
            VDOTO(K,J)=VDOT(K,J)
            IESPC(K,J)=0
          ENDDO
          H2OL(J)=HDOT(J)
          HOL(J)=HEL(J)
          HDOT(J)=HDET(J)
        ENDDO
!NiS,may06,com: ICYC was initiated in INITL.subroutine
        ICYC=ICYC+1
!C-
!C...... UPDATE OF BOUNDARY CONDITIONS
!C-
          CALL INPUTD(IBIN)
          

        ENDIF LaterTimestep
!-

!CIPK AUG95 ADD A CALL TO UPDATE MET VALUES
!cipk oct02 move heatex to use projections for heat budget
!C        CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET)

        IF((LSAND > 0  .OR.  LBED > 0)  .AND. IWVIN > 0) THEN
          CALL GETWAVE
        ENDIF

        IF(IWVFC == 102) THEN
          CALL GETSST
        ELSEIF(IWVFC == 104) THEN
          CALL GETDRSST
        ENDIF

        CALL SWANDT

!CIPK AUG95 USE ALPHA=1.8
!nis,may08: Use 1.6 again
!cipk dec99 test for delta = 0
        if(delt > 0.) then

          !TODO: This was already done in the inputd.subroutine. It shouldn't occur twice!
    	  ALTM = ALPHA/ DELT
!CIPK      ALTM=2.0/DELT
!CIPK MAY02
          ALPHASN=1.8
          ALPHASN=2.0
        else
          altm=0.
        endif


      !REMOVE FOR RMA·KALYPSO
      !nis,nov08: Remove reading from file unit nipt
      !nipt is obsolete
      !-
      NDL=1


        IF (ITEQV(MAXN) /= 5  .AND.  IOPTZD > 0 .AND. IOPTZD < 3 ) CALL MELLII


        !nis,apr08,com: updating the time derivatives

        ForallNodes: DO J=1,NP
!cipk mar98 add logic to update HEL

          !vel(3, j) : virtual depth (calculation depth) of node j
          !vtm       : copy of virtual depth (calculation depth) of node j
          !hel (j)   : transformed depth (depth over slot minimum of node j

          !get the transformed depth
          VTM=VEL(3,J)
          CALL AMF(HEL(J),VTM,AKP(J),ADT(J),ADB(J),D1,D2,0)

          !NDL : Lowest degree of freedom to work on, is set if something is read from an external file (?); normally NDL = 1
          !NDF : Highest degree of freedom to work on
          !IESPC(k, j) : (???)
          !VEl (k, j) : k-th degree of freedom of node j (velocities, virtual water depth, concentrations)
          !thetcn:

          !check for all degrees of freedom

          !write(75,*) 'RMA10_899: NDF=',NDF, 'NDF=', NDL
          ForAllDOFs: DO K = NDL, NDF
!CIPK SEP96        VOLD(K,J)=VEL(K,J)
!CIPK SEP96        VDOTO(K,J)=VDOT(K,J)
            IF (IESPC (K, J) == 0) THEN
!CIPK DEC00            VEL(K,J)=VEL(K,J)+DELT*VDOT(K,J)

!cipk dec00 rewrite projection approach add switch


              !calculate the current value of variable and the derivative basing on the approach to be used
              !apply transformation of variable, but don't use update of time derivative
              IF(IPROJ == 0) THEN
                VEL (K, J) = VEL (K, J) + DELT * VDOT (K, J)

              !use projection for the variable of current run by the old variable and the variable of the penultimate calculation step
              ELSEIF(IPROJ == 2) THEN
                dtfac         = thetcn * vdot (k, j) + (1. - thetcn) * v2ol (k, j)
                vel (k, j)    = vel (k, j) + delt * dtfac
                VDOT (K, J)   = ALTM * (VEL (K, J) - VOLD (K, J)) - VDOTO (K, J) * (ALPHA - 1.)

              !classical method without time projection, but just calculating the time derivative based on the Finite Difference Scheme
              !IPROJ == 1 (normally)
              ELSE
                VDOT (K, J) = ALTM * (VEL (K, J) - VOLD (K, J)) - VDOTO (K, J) * (ALPHA - 1.)
              ENDIF

            ENDIF
          ENDDO ForAllDOFs


          IF(NDL == 1) THEN
            IF(IESPC(3,J) == 0) THEN

!cipk dec00 add projection option

              IF(IPROJ == 0) THEN
                HEL(J)=HEL(J)+DELT*HDET(J)
                CALL AMF(HEL(J),HTP,AKP(J),ADT(J),ADB(J),D1,D2,1)
                VEL(3,J)=HTP
              ELSEIF(IPROJ == 2) THEN
                dtfac=thetcn*hdet(j)+(1.-thetcn)*h2ol(j)
                hel(j)=hel(j)+ delt*dtfac
!c               HEL(J)=HEL(J)*2-H2OL(J)
!
!cipk apr01 insert consistent argument

                VH=VEL(3,J)
                CALL AMF(HEL(J),VH,AKP(J),ADT(J),ADB(J),D1,D2,1)
              ENDIF
              IF(DELT > 0.) THEN
              VDOT(3,J)=ALTM*(VEL(3,J)-VOLD(3,J))-(ALPHA-1.)*VDOTO(3,J)
              ELSE
                VDOT(3,J)=0.
              ENDIF
            ENDIF
          ENDIF
        ENDDO ForallNodes

!cipk dec00 set water surface elevation

        DO  J = 1, NPM
          IF (IDNOPT/=0) THEN
            HS = VEL(3,J)
            ISWT = 0
!cipk jan01  change AME to AME1
            CALL AMF(H,HS,AKP(J),ADT(J),ADB(J),AME1,D2,ISWT)
            WSLL(J) = H + ADO(J)
          ELSE
            WSLL(J) = VEL(3,J) + AO(J)
          ENDIF
          K=NREF(J)+1
          IF(K /= 1) THEN
            L=NREF(J)+NDEP(J)-1
            IF(L .GE. K) THEN
              DO M=K,L
                WSLL(M)=WSLL(J)
              ENDDO
            ENDIF
          ENDIF
        ENDDO

!CIPK APR01  NOW RESET FOR MID-SIDES
        DO K=1,NEM
          IF(NCRN(K) > 0) THEN

            DO L=2,NCRN(K),2
              IF(IMAT(K) < 901  .OR.  IMAT(K) > 903) THEN
                J=NOPS(K,L)
                !EFa Apr07, not for mid-sides of Flow1dFE-elements
                if (j>0) then
                N1=NOPS(K,L-1)
                  IF(MOD(L,NCRN(K)) == 0) THEN
                    N2=1
                  ELSE
                    N2=NOPS(K,L+1)
                  ENDIF
                  WSLL(J) = (WSLL(N1)+WSLL(N2))/2.
                  KK=NREF(J)+1
                  IF(KK /= 1) THEN
                    LL=NREF(J)+NDEP(J)-1
                    IF(LL .GE. KK) THEN
                      DO M=KK,LL
                        WSLL(M)=WSLL(J)
                      ENDDO
                    ENDIF
                  ENDIF
                endif
              ENDIF
            ENDDO
          ENDIF
        ENDDO

!CIPK OCT02  SET ICETHKOL
        DO J=1,NPM
          ICETHKOL(J)=ICETHK(J)
        ENDDO

!CIPK JUN03

        CALL GETSTRESS
!C-
!C       TET=TET+DELT/3600.

!MD: Gesamter LSS>0 Block verschoben: 05.11.2008
!MD  --> siehe unten im Iteration LOOP!!

!C        DO NNN=1,NPM
!C        WRITE(240,'(I6,6E15.5)') NNN,BSHEAR(NNN),SERAT(NNN),EDOT(NNN)
!C     +   ,THICK(NNN,1),THICK(NNN,2),DEPRAT(NNN)
!C        ENDDO
!
!C----------------------------
!C......ITERATION LOOP
!C-----------------------------
        !NiS,apr06: starting iteration sequence
        !initialization:
        !NITA = maximum number of iterations of timestep, local copy
        !NITN = maximum number of iterations of timestep, global value
        !ITPAS= ???
        NITA=NITN
        MAXN=0
        ITPAS=0

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jun07, necessary for autoconverge
!        if (beiauto/=0.0) then
!          call autoconverge(6.)
!        end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

        !MAXN = actual iteration number; first initialized, then incremented
  465   MAXN=MAXN+1

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!       !EFa jun07, autoconverge
!       if (beiauto/=0.) then
!         call autoconverge(7.)
!       end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!cipk oct02
        IF(MAXN == 1) THEN
          write(75,*) 'going to heatex-535',n,maxn,TET,itpas
!CIPK AUG05          CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
          CALL HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
          ITPAS=1
        ELSEIF(ITEQV(MAXN) == 8) THEN
          write(75,*) 'going to heatex-540',n,maxn,TET,itpas
!CIPK AUG05          CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
          CALL HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
        ENDIF
!C     DO 700 MAXN=1,NITN
!C     REWIND IVS

!nis,jun07: Activating transition for dynamic loop
      if (MaxLT /= 0) call TransVelDistribution
!-

!C-
!C......  Process dry nodes
!C-
        IF(IDSWT /= 0) THEN
          IF(IDRYC == 0) THEN
            WRITE(*,*) 'ENTERING REWET'

            CALL REWET
            CALL DEL
            IDRYC=IDSWT
          ENDIF
        ENDIF

!cipk APR97  Add dropout
!cipk mar01 rearrange dropout logic to allow for idrpt = 2
!c          move logic into DRPOUT

        IF(IDRPT == 0) THEN

!c     set IACTV to be active for all

          DO J=1,NP
            DO K=1,NDF
              IACTV(J,K)=10
            ENDDO
          ENDDO
        ELSE
          CALL DRPOUT
        ENDIF
!cipk mar01 end change

!cipkapr97 end changes

        CALL BLINE(MAXN)

        ICK=ITEQS(MAXN)+4

!cipk nov99 add optional call for transitioning

!cipk aug00 experimental
        IF(ITRANSIT == 1  .and. maxn < 4) CALL TWODSW

!CIPK MAY02 UPDATE SHEARS ETC
        IF((LSAND > 0  .OR.  LBED > 0) .and. ick == 6) THEN
          CALL KINVIS
!c          CALL SHEAR
          WRITE(*,*) 'GOING TO SANDX'
          CALL SANDX
          CALL BEDXCG
        ENDIF

!MD: Gesamter LSS>0 Block in Iterationschleife verschoben: 05.11.2008
!MD -----------------------------------------------------------------
        IF (LSS > 0) THEN
          CALL SETVEL
          CALL KINVIS
!ciat mar06 adding new wbm bedshear stress subroutines for cohesive sediment calcs
!NiS,Nov06: Seems, that name of first shear-Subr is not correct. Change SHEAR1 to WSHEAR1
!          CALL SHEAR1

!MDMD:  Aufruf nur dann, wenn wirklich Wellen vorhanden!!
!MD Neu:   Abfrage ueber Kennungen
          IF (IWVIN == 101 .or. IWVFC==102 .or. IWVFC==104) THEN
            CALL WSHEAR1
            CALL WSHEAR2
          Else
            !MD: Reaktivierung von SubR SHEAR
            !MD neu:   Sohlschub-Berechnung mit Kalypso (ks-werten/ Lambda!)
            IF (IKALYPSOFM /= 0) THEN
              WRITE(*,*) 'Aufruf KALYP_SHEAR'
              CALL GET_FFACT
              CALL KALYP_SHEAR
            ELSE !MD: Wenn nicht Kalypso, dann KING-Ansatz
              CALL SHEAR
            ENDIF
          END IF
!c         CALL SHEAR
!c         CALL WSHEAR1

          CALL DEPSN
          CALL MEROSN(1)
          CALL SEROSN(1)
        ENDIF
!MD ---------------------------------------------------------------

!CIPK OCT02
  470   CONTINUE

        CALL LOAD
!C-
!C......  Compute areas of continuity lines for stage flow input
!C-
        CALL AGEN
!c      DO 470 J=1,NE
!c      DFCT(J)=1.
!c  470 CONTINUE
!c     write(*,*) 'In main maxn,iteqv(maxn),ioptzd',maxn,iteqv(maxn)
!c    +,    ioptzd
        IF (MAXN .GE. 2 .AND. ITEQV(MAXN) /= 5 .AND. ITEQV(MAXN) /= 2  .AND.  IOPTZD > 0 .AND. IOPTZD < 3) CALL MELLII
!CIPK JAN97
        IF(IUTUB == 0) THEN
          IF(MAXN > 2  .or. n > 1) IUTUB=1
        ENDIF
!CIPK JAN97 END CHANGES

        !nis,feb07,testing: Write whole matrix
        !if (maxn > -1) then
        !  write
        !+      (matrixname,'(a3,i3.3,a1,i3.3a4)')'mat',maxn,'_',icyc,'.txt'
        !  teststat = 0
        !  open (9919, matrixname, iostat = teststat)
        !  if (teststat /= 0) STOP 'ERROR - while opening matrix file'
        !  WRITE(9919,*) 'maxn', maxn
        !endif
        !-

!cipk aug07
        call SECOND(ATIM(2))
        IF(ICPU == 0) THEN
          CALL FRONT(1)
        ELSE
          !stop 'option not stable, please use frontal scheme'
          CALL FRONT_PARDISO(1)
        ENDIF
        IF(ITIMFL > 0) THEN
          CALL SECOND(ATIM(3))
          WRITE(ITIMFL,6101) N,MAXN,ATIM(3)-ATIM(2),ATIM(3)-ATIM(1)
 6101     FORMAT('STEP',I5,' ITERATION',I5,'  TIME IN FRONT-HORZ',F12.2, ' TOTAL TIME TO DATE FOR RUN =', F12.2)
        ENDIF        
        IDRYC=IDRYC-1
        !close testfile
        !close (9919, status = 'keep')
        !-


        CALL UPDATE
        call FindMinMaxValues (MaxP)


!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!      if (exterr==1.0) then
!        call autoconverge(8.)
!        GOTO 465
!
!      end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!C      CALL CHECK
!C     REWIND IVS
        IF(ITEQV(MAXN) /= 5  .AND.  ITEQV(MAXN) /= 2 .AND.  ITEQV(MAXN) < 8) CALL VRTVEL

          IF(NPRTF > 0) THEN
            IF(MOD(MAXN,NPRTF) == 0  .OR.  MAXN == NITA .OR. NCONV == 1) THEN
              CALL OUTPUT(2)
              CALL CHECK
            ENDIF

          ELSE
            IPRTF=IABS(NPRTF)

            IF(MOD(N,IPRTF) == 0) THEN
              IF(MAXN == NITA  .OR. NCONV == 2) THEN
                CALL OUTPUT(2)
                CALL CHECK
              ENDIF
            ENDIF
          ENDIF


!C-
!C......UPDATE TIME DERIVATIVE
!C-
!CIPK NOV97
        write(75,*) 'rma10-646'

        DO  J=1,NP

!CIPK MAR98
          VTM=VEL(3,J)
          CALL AMF(HEL(J),VTM,AKP(J),ADT(J),ADB(J),D1,D2,0)

          !write(75,*) 'RMA10_1250: NDF=',NDF
          DO  K=1,NDF
            VDOT(K,J)=ALTM*(VEL(K,J)-VOLD(K,J))-(ALPHA-1.)*VDOTO(K,J)
          ENDDO
          HDET(J)=ALTM*(HEL(J)-HOL(J))-(ALPHA-1.)*HDOT(J)
        ENDDO

  550   CONTINUE
!CIPK OCT02  CHECK FOR NEGATIVE TEMPS

      IF(ICESW > 0  .AND. ITEQV(MAXN) == 8  .AND. ITPAS < 4)THEN
          ITPAS=ITPAS+1
          DO J=1,NPM
            IF(VEL(5,J) < TMED) GO TO 570
          ENDDO
        ENDIF
        GO TO 580
  570   CONTINUE
        WRITE(75,*) 'GOING TO HEATEX-570',N,MAXN,TET,ITPAS
!CIPK AUG05        CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
        CALL HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
        GO TO 470

  580   CONTINUE
        IF(ITEQV(MAXN) == 8  .AND.  ITPAS == 0) ITPAS=1 
!C-
!C......SAVE RESTART CONDITIONS ON FILE
!C-
!CIPK MAY96 RESTORE TETT AS HOURS IN YEAR
        TETT=(DAYOFY-1)*24.+TET
        WRITE(75,*) 'TET,DAYOFY',TET,TETT,DAYOFY

        !NiS,apr06,comment: If convergent result files for this timestep have to be written
        IF(NCONV == 2) GO TO 750
  700   CONTINUE


!C      END OF ITERATION LOOP

!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE DYNAMIC RUN ITERATION, IF NOT CONVERGED
        IF (itefreq/=0) THEN
          IF (MOD(maxn,itefreq)==0) THEN
            IF (IKALYPSOFM /= 0) THEN
            WRITE(*,*)' Entering write_Kalypso', ' dynamic at time step ', icyc+iaccyc-1, ' after Iteration = ',maxn
            call generateOutputFileName ('inst', niti, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
            CALL write_KALYPSO (outputFileName, 'resu')
            WRITE(*,*)'back from write_kalypso'
            ENDIF
          ENDIF
        ENDIF
!-
!NiS,apr06: calculating the cwr-values for trees WITHIN THE ITERATION.
!           Calculation of actualized tree-parameters; file is only written in dependency of itefreq
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
!-

        !NiS,apr06,comment: Start next iteration, until maximum number of iterations is reached
        IF (MAXN < NITA) GO TO 465
        !NiS,apr06,comment: Writing results after the timestep
  750   CONTINUE
        write(75,*) 'rma10-668 at 750 continue'
!CIPK MAY02 UPDATE BED INFORMATION FOR SAND CASE
        IF(LSAND > 0) THEN
          write(75,*) 'rma10: going to bedsur'
          CALL BEDSUR

        ELSEIF(LSS > 0) THEN
!C-      IF COHESIVE SED IS SIMULATED, CALCULATE BED CHANGE
!MD:    Erneuter Aufruf der Erosionsroutinen mit II = 2
!MD       zur Berechnung der Sohlanderung infolge ersosion
          CALL MEROSN(2)
          CALL SEROSN(2)
          CALL NEWBED(2)
        ENDIF

      !MD Aufruf der Sohlentwicklung nur einmal fuer sandige Sohle
        IF(LBED>0 .and. LSAND.le.0) THEN
        write(75,*) 'rma10-674 going to bedsur'

          !MD 11.08.2008: BEDLBED wurde deaktiviert, da Routine veraltet und fehlerhaft:
          !MD   alle dort berechneten Werte GAN0 und GAN sind unsinnig, da immer = null
          !MD   BEDLBED wurde mit der aktuelleren Routine BEDSUR ersetzt
          !MD CALL BEDLBED
          CALL BEDSUR
        ENDIF


!NiS,apr06: calculating the cwr-values for trees.
!           This option is only activated, if VEGETA is entered in input file at proper place
        temp_maxn = maxn
        maxn=0
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
        maxn=temp_maxn
!-

!CIPK SEP02   LOGIC FOR RESTART FILE MOVED DOWN
!C-
!C......SAVE RESTART CONDITIONS ON FILE
!C-
!CIPK MAY96 RESTORE TETT AS HOURS IN YEAR
        TETT=(DAYOFY-1)*24.+TET
        WRITE(75,*) 'TET,DAYOFY',TET,TETT,DAYOFY

!REMOVE FOR RMA·Kalypso
!nis,nov08: Remove writing to restart output file with unit NLL
!NLL is obsolete
!-

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jun07, autoconverge
!        if (beiauto/=0.) then
!          call autoconverge(9.)
!          if (autoindex==2.) then
!            autoindex = 0.
!            GOTO 465
!          end if
!        endif
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!C-
!C......SAVE ON RESULTS FILE
!C-

!NiS,apr06: Adding KALYPS-2D results file as option
!CIPK MAR00

!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to unit irmafm
!irmafm is obsolete
!nis,nov08: Remove writing to obsolete unit nopt
!nopt is obsolete
!nis,nov08: Remove writing to obsolete unit ibedot
!ibedot is obsolete
!nis,nov08: Remove writing to obsolete unit ismsfm
!ismsfm is obsolete
!nis,nov08: Remove writing to obsolete unit ismsfm1
!ismsfm1 is obsolete
!nis,nov08: Remove writing to obsolete unit ismsfm2
!ismsfm2 is obsolete
!nis,nov08: Remove writing to obsolete unit iwavot
!iwavot is obsolete
!-
        IF(IKALYPSOFM > 0) THEN
!-

!Cipk mar03 add option that allows output at a set frequency

          IF(MOD(N,IOUTFREQ) == 0) THEN

!REMOVE FOR RMA·KALYPSO
!nis,nov08: remove vsing-array for output
!vsing is obsolete
!-

!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to unit irmafm
!irmafm is obsolete
!-

!C       Output RMA results file contains
 
!c       1   time in hours (Julian)
!c       2   number of nodes
!c       3   obsolete counter of degrees of freedom (set to 5) NDF=6
!c       3a  number of elements
!c       4   year
!c       5   VSING subscript(1)  x-vel by node
!c       6   VSING subscript(2)  y-vel by node
!c       7   VSING subscript(3)  depth by node
!c       8   VSING subscript(4)  salinity by node
!c       9   VSING subscript(5)  temperature by node
!c      10   VSING subscript(6)  sus-sed by node
!c      11   VVEL                w-vel by node
!c      12   WSLL                water surface elevation
!c      13   DFCT                stratification multiplier by element
!c      14   VSING subscript(7)  water column potential by node

!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE DYNAMIC RUN
            IF (IKALYPSOFM /= 0) THEN
              MAXN = 0.
              WRITE(*,*)' Entering write_Kalypso', ' after dynamic time step ', icyc+iaccyc-1
              call generateOutputFileName ('inst', niti, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
              CALL write_KALYPSO (outputFileName, 'resu')
              WRITE (*,*)'back from write_kalypso'
              !every timestep min and max result files are overwritten to have the last
              call generateOutputFileName ('mini', 0, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
              call write_Kalypso (outputFileName, 'mini')
              call generateOutputFileName ('maxi', 0, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
              call write_Kalypso (outputFileName, 'maxi')
            END IF
!-
!CIPK AUG02

!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to unit ismsfm
!ismsfm is obsolete
!nis,nov08: Remove writing to unit ismsfm1
!ismsfm1 is obsolete
!nis,nov08: Remove writing to unit ismsfm2
!ismsfm2 is obsolete
!nis,nov08: Remove writing to unit nopt
!nopt is obsolete
!-

!C     Output results file contains

!c     1   time in hours (Julian)
!c     2   number of nodes
!c     3   obsolete counter of degrees of freedom (set to 5) NDF=6
!c     3a  number of elements
!c     4   year
!c     5   VSING subscript(1)  x-vel by node
!c     6   VSING subscript(2)  y-vel by node
!c     7   VSING subscript(3)  depth by node
!c     8   VSING subscript(4)  salinity by node
!c     9   VSING subscript(5)  temperature by node
!c    10   VSING subscript(6)  sus-sed by node
!c    11   VVEL                w-vel by node
!c    12   DFCT                stratification multiplier by element
!c    13   VSING subscript(7)  water column potential by node

!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to obsolete unit ibedot
!ibedot is obsolete
!nis,nov08: Remove writing to obsolete unit iwavot
!iwavot is obsolete
!-


!C     Output bed results file contains

!c     1   TETT                Time in hours (Julian)
!c     2   NQL                 Number of constituents
!c     3   NP                  Number of nodes
!c     4   IYRR                Year
!c     5   VSING subscript(1)  x-vel by node
!c     6   VSING subscript(2)  y-vel by node
!c     7   VVEL                z-velocity
!c     8   VSING subscript(3)  depth by node
!c     9   WSLL                water surface elevation by node
!c    10   VSING subscript(6)  water column concentration by node 1
!c    11   VSING subscript(7)  water column potential by node 2
!c    12   VSING subscript(8)  Bed elevation by node 3
!c    13   DELBED              Bed change by node 4
!cipk aug05 correct to get BSHEAR OUTPUT
!c    14   BSHEAR              Shear stress by node 5

!CIPK AUG98
!C  Save restart file every IOURST timesteps
            IF (MOD(N,IOUTRST) == 0)  THEN
              CLOSE (131)
!CIPK SEP04
!              WRITE(INUM,'(I4.4)') N
              WRITE(INUM,'(I6.6)') N ! Override djw 31/10/05 enables write of more than 9990 TS restart file
              FRST=trim(fnam) // 'RST'//INUM//'.RST'
              OPEN(131,FILE=FRST,FORM='UNFORMATTED',STATUS='UNKNOWN')
              IF(LSAND == 0   .AND.  LSS == 0) THEN
                WRITE(131) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7) ,VVEL(J),J=1,NP) ,(hel(j),hdet(j),j=1,np)

              ELSEIF(LSAND > 0) THEN
                WRITE(131) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7) ,VVEL(J),J=1,NP) ,(hel(j),hdet(j),j=1,np) ,(DELBED(J),ELEVB(J),TTHICK(J),J=1,NP)
              ELSE
                WRITE(131) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7) ,VVEL(J),J=1,NP) ,(hel(j),hdet(j),j=1,np) ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,MXSEDLAY),I=1,NP) ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J) ,SMVAL(I,J),J=1,MXSEDLAY),BEDORIG(I),I=1,NP)

              ENDIF	    
              IF(ICESW > 0) THEN
                WRITE(131) (ICETHK(J) ,J=1,NPM)
                ENDIF
!CIPK SEP02 ADD RESTART DATA FOR BED
            ENDIF 

          ENDIF
        ENDIF
!NiS,may06: Renaming 800-DO-Loop to Main_dynamic_Loop
!  800 CONTINUE

      ENDDO Main_dynamic_Loop
!-
      CALL ZVRS(1)
!CIPK JUL01      STOP
      RETURN
      END


!**************************************************
      subroutine FindMinMaxValues(Points)
      USE blk10mod
      USE parakalyps

      implicit none

      INTEGER :: i
      INTEGER, intent (IN) :: Points

      do i = 1, Points
        if (SQRT (vel(1,i)**2 + vel(2,i)**2) > SQRT (maxvel(1,i)**2 + maxvel(2,i)**2)) then
          maxvel (1, i) = vel (1, i)
          maxvel (2, i) = vel (2, i)
        end if
        if (SQRT (vel(1,i)**2 + vel(2,i)**2) < SQRT (minvel(1,i)**2 + minvel(2,i)**2)) then
          minvel (1, i) = vel (1, i)
          minvel (2, i) = vel (2, i)
        end if
        if (vel (3, i) > maxvel (3, i)) then
          maxvel (3, i) = vel (3, i)
          maxrausv (i) = rausv (3, i)
        end if
        if (vel (3, i) < minvel (3, i)) then
          minvel (3, i) = vel (3, i)
          minrausv (i) = rausv (3, i)
        end if
      end do

      end subroutine

