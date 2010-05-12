module mainRoutines

contains


subroutine RMA_Kalypso (modelName)

!global constants
!----------------
!use const_modelConvConstants
!use type modules
!----------------
use mod_input
use mod_initl
use mod_vegetation

use mod_Model
use mod_fileType
use mod_fileType_lists
use SchwarzIterationControl_Helper

use check_mod, only: check

!global definitions (bad and ugly!)
!----------------------------------
USE BLK10MOD, only: &
&  niti, nita, maxn, nitn, itpas, iaccyc, icyc, ncyc, it, &
&  ioutrwd, nprtf, nprti, nprtmetai, irsav, irMiniMaxiSav, &
&  maxp, maxe, maxlt, maxps, maxse, ne, np, npm, nem, nmat, ncl, &
&  ncrn, nops, imat, &
&  ao, cord, ccls, &
&  vel, vdot, vold, vdoto, v2ol, vvel, &
&  nfix, nfixp, spec, alfa, &
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
&  itransit, ndep, nref, dfct, &
&  storageElts ,&
!HN. Sept2009
&  IPROFIN   
!meaning of the variables
!------------------------
!niti                   number of steady state iterations
!nitn                   number of unsteady iterations read for each time step
!nita                   local copy of current maximum number of iterations in steady or unsteady run
!maxn                   current iteration cycle
!itpas                  ???
!iaccyc:                time step to start the calculation from; counting through the list of time steps to reach the iaccyc-th step
!icyc                   counter for really processed time steps
!ncyc                   maximum number of time steps
!it                     actual 'ID' of the time step
!ioutrwd                frequency to rewind output file LOUT
!nprtf                  frequency of time steps to write out full results
!nprti                  frequency of iterations within nprtf to write out full results
!irsav                  frequency to print results to output file after iteration
!maxp:                  maximum number of points (surface points; includes midsides)
!maxe:                  maximum number of elements
!maxlt:                 maximum number of 1D/2D transition elements for 1D/2D line-2-element transitions
!maxps:                 maximum number of pipe surface connections
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
!IPROFIN                UNIT NUMBER OF INPUT PROFILE DATA FILR FOR SIMULATION OF RIVERBANK EVOLUTION
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
!alpha                  factor for time centering (default 1.6) for hydrodynamic values
!altm                   = alpha/ delt
!alphasn                factor for time centering for constituents (default 2.0)
!sidff                  side inflow to an element
!mxsedlay               ??? - sediment purposes
!nref                   reference node, that is the top/bottom (?) node of a node column
!ndep                   number of layers per nodes; that means depending nodes on original 2D-node
!itransit               ??? - 3D purposes
!dfct                   ??? - 3D purposes


USE BLK11MOD, only: dayofy, icethkol, icethk, icesw, tmed
USE BLKDRMOD, only: idswt, akp, adt, adb, ado
!meaning of the variables
!------------------------
!idswt      switch to control the drying wetting algorithm
!           0 - don't deactivate any nodes
!           X - X is the frequency of testing for wetting/ drying
!akp
!adt
!adb
!ado
USE BLKSEDMOD, only: lss, nlay, thick, sst, nlayo, thicko, gbo, ssto, smval, bedorig
USE BLKSANMOD, only: lbed, lsand, delbed, elevb, tthick
USE PARAKalyps, only: ivegetation, c_wr, mcord
use mod_storageElt
! HN. June2009
USE share_profile, ONLY : BANKEVOLUTION   
use mod_ContiLines

implicit none

!arguments
character (len = 1000) :: modelName

!local variables
integer (kind = 4) :: idryc, iprtf, iprti, iprtMetai
integer (kind = 4) :: i, j, k, l, m, n, kk, ll, pp
integer (kind = 4) :: n1, n2
integer (kind = 4) :: temp_maxn, SchwarzIt
integer (kind = 4) :: ndl
integer (kind = 4) :: teststat
integer (kind = 4) :: ibin
integer (kind = 4) :: CallCounter
integer (kind = 4) :: improvementAlgo
real (kind = 8) :: vtm, htp, vh, h, hs
real (kind = 8) :: ta
real (kind = 8) :: d1, d2, ame1
real (kind = 8) :: dtfac
real (kind = 8) :: sallowperm, salhighperm
real (kind = 8) :: thetcn
character (len = 96) :: outputfilename, inputfilename
type (contiLine), pointer :: tmpCCL => null()
logical :: allAssigned = .false.
type (file), pointer :: convergenceStatusFile => null()
character (len = 96) :: convergenceStatusFileName
integer (kind = 4) :: continueCommand
type (linked_List), pointer :: bcFiles
type (SimulationModel), pointer :: m_SimModel

!meaning of the variables
!------------------------
!idryc            is something like a count down variable to process drying/ wetting
!iprtf            local variable to get time step frequency to write out results (only transient calculations)
!iprti            local variable to get iteration frequency to write out results within steady or iprtf-controlled transient calculations
!nprtMetai        local variables to get iteration frequency to write out results to the output file (not result model file)
!i, j, k, l, m, n local counter variables
!kk, ll           local counter variables; all connected to logic concerned about 3D applications
!n1, n2           local copies of nodes
!temp_maxn        local temporary storage for current iteration cycle
!ndl              lowest degree of freedom; becomes only the value 1
!teststat         i/o status variable
!ibin             unit number of the control file; it will be set, if input.sub was called
!vtm, htp, vh, h, hs    local copies for variables that are used for elevation transformation during operative MARSH algorithm
!ta               time for processing time measuring purposes
!d1, d2           dummy values for calling the transformation subroutine for the MARSH algorithm
!ame1             dummy value for calling the transformation subroutine for the MARSH algorithm
!dtfac            used as temporary variable to calculate the time derivative with the iproj-option 2
!sallowperm       ???
!salhighperm      ???
!thetcn           local reciprocal value of alpha to calculate time derivatives of the variables
!outputfilename   outputfilename character variable for 2D result files
!inputfilename    inputfilename character variable for 2D result files (informational data) ???
! Callcounter     HN. June2009, counter for counting the number of times the bank evolution subroutine is called.
! Callcont        HN. June2009, counter for counting the number of times the MakeFenode subroutine is called.

!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------
!CHARACTER (LEN = 25) :: matrixname
!CHARACTER (LEN = 30) :: matrixformat
!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
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

m_SimModel => newSimulationModel (modelName)
m_SimModel%FEmesh => newFEMesh ()


      
!get informations concerning time consumption
!--------------------------------------------
call second (ta)

!initialise all the global arrays and variables
!----------------------------------------------
call initl (m_SimModel)

!initialisations of global variables
!-----------------------------------
!dummy value for maxn, for call of input.sub
maxn = 1
!number of active degrees of freedom
ndf  = 6
!copy scratch file unit number for solver purpose
nd1 = nscr
!console unit number definition
!TOASK: Why is unit 6 the console?
icfl  = 6

!initialisations of local variables
!----------------------------------
idryc = 0

!for time derivative of primary hydrodynamic variables
!-----------------------------------------------------
!alpha = 1.6
alpha = 1.8
!alpha = 2.0
thetcn = 1./ alpha
! HN, June 2009, initilizing bank evolution counters.
CallCounter = 0

!input geometry etc
!------------------
call input (ibin, m_SimModel)

!input control structure data, if input file for that is present
!----------------------------
IF (INCSTR == 20) CALL INCSTRC
!input time series from a file, if input file for that is present
!-----------------------------
IF (INTIMS == 22) CALL INTIMES

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !beiauto = switch for autoconverge usage; Why is it a type real switch and not integer?
!      !EFa jun07, output for autoconverge
!!      if (beiauto /= 0.) call autoconverge (-1.0)
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!get external network data, if file for that is present
!-------------------------
IF (ICORDIN /= 0) CALL GENT

!initialise vegetation parameters, if algorithm should be used
!--------------------------------
if (ivegetation /= 0) then
  write(*,*)'going to cwr_init'
  call cwr_init
else
  do i = 1, maxe
    c_wr (i) = 1.0
  enddo
endif

!Establish flow directions for one dimensional elements
!------------------------------------------------------
call fldir


!Check, whether model has inner boundaries, so Schwarz iteration has to be done!
m_SimModel%hasInnerBoundaries = checkForInnerBCs (ccls(:), ncl)
!In the case of distributed calculations, neighbour-IDs need to be read
if (m_simModel%hasInnerBoundaries) call readDistributedModelIDs (m_simModel)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 STEADY STATE CALCULATION                        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
!jump to transient calculation part, if there are no steady iterations on demand
!----------------------------------
steadyCalculation: if (niti /= 0) then

!copy current number of steady iterations to be processed  
!--------------------------------------------------------  
  nita = niti
!get prinout frequency for results; if not specified (iprtf = 0), then print after last iteration  
!---------------------------------  
  iprti = abs (nprti)
  if (iprti == 0) iprti = nita
!get prinout frequency for results to output file (not model result file)  
!---------------------------------  
  iprtMetai = abs (nprtMetai)
  if (iprtMetai == 0) iprtMetai = nita

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      if (beiauto /= 0.) call autoconverge (1.)
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!PROCESS SCHWARZ ITERATION FOR DISTRIBUTED CALCULATIONS WITH KALYPSO SERVICE  
!---------------------------------------------------------------------------  
!Initialize for the case no Schwarz iteration has to be done  
  m_SimModel%isSchwarzConv = .true.
  m_SimModel%isNewtonConv = .false.
  
  SchwarzIt = 0
  steadySchwarzCycle: Do
    SchwarzIt = SchwarzIt + 1
    
!if model has inner boundaries, there must be a Schwarz iteration with neighbouring models    
!-----------------------------------------------------------------------------------------    
    if (m_SimModel%hasInnerBoundaries) then
!Get Continue Command      
!--------------------      
      continueCommand = getSchwarzIterationCommand (schwarzIt, icyc)
      contCom: select case (continueCommand)
        case (enum_stop)
!FIXME: Stop handling must be done!          
          stop 'Divergence in any part of the entire model!'
        case (enum_nextStep)
!nullify the boundary condition status of the inner boundary nodes          
          call nullifyInnerBCstatus (ccls, nfix, nfixp, alfa, ncl)
          exit steadySchwarzCycle
        case (enum_continueStep)
          continue
      end select contCom
!Write the own inner conditions to an output file, to overgive it to the neighbours      
!----------------------------------------------------------------------------------      
      call writeInnerBoundaryConditons (m_simModel, schwarzIt, icyc, ccls, ncl, maxp, vel, wsll, cord)
!Re-Initialize the model convergency status, i.e. assume it is converged and check assumption consecutively      
!----------------------------------------------------------------------------------------------------------      
      if (m_SimModel%hasInnerBoundaries) m_SimModel%isSchwarzConv = .true.
!Generate BC file filenames list      
!-------------------------------      
      bcFiles => getNeighbourBCFiles (schwarzIt, icyc, m_SimModel)
!store boundary conditions from last Schwarz iteration      
!-----------------------------------------------------      
      call storeOldInnerBoundaryConditions (ccls, spec, ncl, maxp)
!Read the incoming boundary conditions from the neighbours      
!---------------------------------------------------------      
      call getNeighbourBCs (bcFiles, ccls, ncl, maxp, spec, nfix)

!Improve boundary conditions by nested intervals (variables are still local)      
!-----------------------------------------------      
!TODO: Make it more flexible, invoking different algorithms!      
      improvementAlgo = 1
! defined in Mod_ContiLines: enum_nestedIntervals = 1      
      if (schwarzIt <= 2) call improveBCs (improvementAlgo, ccls, ncl, spec, maxp)
      
!write header of console output      
!------------------------------      
      write (*,'(a15,i4,a14,f8.6)') 'Schwarz-cycle: ', schwarzIt, ' Conv-Border: ', m_SimModel%schwarzConv
      write (*,'(a)') 'CCL    vx-conv max    vy-conv max    h-conv max'
      write (*,'(a)') '----------------------------------------------------'
!Check for Schwarz convergence and reinitialize under circumstances Newton Convergence      
!-------------------------------------------------------------------------------------      
      m_SimModel%isSchwarzConv = checkSchwarzConvergence (ccls, ncl, m_SimModel%schwarzConv)
      if ( .NOT. (m_SimModel%isSchwarzConv)) m_SimModel%isNewtonConv = .false.
    endif

!PROCESS TIME STEP (STEADY IN THIS CASE) CALCULATION    
!---------------------------------------------------    
!start calculation with zero iteration entry    
    MAXN = 0
!Iterate steady state    
!--------------------    
    steadyCycle: Do
    
!If boundary conditions did not change, model is already Newton converged      
!------------------------------------------------------------------------      
      if (m_SimModel%isNewtonConv) then
!Write out some info        
!-------------------        
        write(*,*) 'Schwarz iteration cycle: ', SchwarzIt
        write(*,*) 'Boundary conditions are temporarily converged: Nothing to do!'
        write(*,*) 'Waiting for next Schwarz Cycle'
!Announce the Schwarz convergence status (Schwarz Converged)        
!---------------------------------------        
        if (m_simModel%hasInnerBoundaries) call writeConvStatus (case_SchwarzConv, m_simModel%ID, SchwarzIt, Icyc)
!nothing to iterate, so end steady calculation cycle        
!---------------------------------------------------        
        exit steadyCycle
      endif

!Count iterations      
      maxn = maxn + 1

!----------------------------------------------------------------    
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE    
!----------------------------------------------------------------    
!      !EFa jun07, autoconverge    
!!      if (beiauto /= 0.) call autoconverge (2.)    
!----------------------------------------------------------------    
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE    
!----------------------------------------------------------------    

!drying/ wetting, if desired      
!---------------------------      
      if (idswt /= 0) then
        if (idryc == 0) then
!get potentially rewetted nodes          
          write(*,*) 'entering rewet'
          call rewet
!dry wet and potentially rewetted nodes          
          write(*,*) 'entering dry'
          call del
!after processing drying wetting, reset countdown for drying/wetting to the original frequency          
          idryc = idswt
        endif
      endif

!equation dropout      
!----------------      
      if (idrpt /= 0) then
        call drpout
!if inoperative, set all DOFs to be active      
      else
        do j = 1, np
          do k = 1, ndf
            iactv (j, k) = 10
          enddo
        enddo
      endif

!set up boundary line, based on active mesh parts      
!--------------------      
      call bline (maxn)

!Calculate 1D/2D line-2-element transition distribution      
!------------------------------------------------------      
      if (MaxLT /= 0) call TransVelDistribution
      if (maxps /= 0) call PipeSurfaceConnectionQs

!???      
!---      
      ick = iteqs (maxn) + 4

!set side inflows zero (sink/source)      
!-----------------------------------      
      do n = 1, ne
        sidff (n) = 0.
      enddo
!-------------------------------------------------------------------------------------------  
!only operative in 3D applications; i.e. at the moment not in RMA�Kalypso, but only in RMA10  
!-------------------------------------------------------------------------------------------  
!!3D to 2D collapse; experimental  
!!-------------------------------  
!if (itransit == 1 .AND. maxn < 4) call twodsw  
!-------------------------------------------------------------------------------------------  
!only operative in 3D applications; i.e. at the moment not in RMA�Kalypso, but only in RMA10  
!-------------------------------------------------------------------------------------------  

!load the DOFs, i.e. set up the equations system  
!-----------------------------------------------  
  call load

!Compute cross sectional areas of continuity lines for stage flow input based on present water depths  
!----------------------------------------------------------------------------------------------------  
  call agen

!Initialise Mellor Yamada turbulence formulation  
!-----------------------------------------------  
  if (iteqv (maxn) /= 5 .AND. ioptzd > 0 .AND. ioptzd < 3) call mellii
!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------
!nis,feb07,testing: Write whole matrix
!write (matrixname, '(a6,i3.3,a4)') 'matrix', maxn, '.txt'
!teststat = 0
!open (9919, file = matrixname, iostat = teststat)
!if (teststat /= 0) STOP 'ERROR - while opening matrix file'
!WRITE(9919,*) 'maxn', maxn
!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------

!get some time informations for time monitoring  
!----------------------------------------------  
  call second (atim (2))
  
!call the solver  
!---------------  
  if (icpu == 0) then
!frontal solution scheme    
!by Irons, B.: A Frontal solution program for Finite Element analysis. In: Internaltiional Journal for    
!numerical Methods in Engineering. Vol. 2, p. 5-32. 1970.    
    !call front_pardiso (1)
    call front (1)
  else
!Pardiso solver from the Intel MKL library    
!by Schenk, O., G�rtner, K.: Solving unsymmetric sparse systems of linear equations with PARDISO. In:    
! Jorunal of Future Generation Computer Systems, Vol. 20 Iss. 3, p. 475-487. 2004.    
    call front_pardiso (1)
    !call front_petsc(1)
  endif
  

!Write consumption informations to the time file, if desired  
!-----------------------------------------------  
  if (itimfl > 0) then
    call second (atim (3))
    write (itimfl, 6100) maxn, atim (3) - atim (2), atim (3) - atim (1)
  endif        
 6100 format('iteration',i5,'  time in front-horz',f12.2, ' total time to date for run =', f12.2)

!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------
!close testfile
!close (9919, status = 'keep')
!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------

!set information, that advanced turbulence models can be used  
!------------------------------------------------------------  
  if (maxn >= 2) iutub = 1

!countdown for drying/wetting  
!----------------------------  
  idryc = idryc - 1

!update degrees of freedom and check for convergence  
!---------------------------------------------------  
  call update (schwarzIt)

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!      if (exterr==1.0) then
!
!        call autoconverge(3.)
!
!        GOTO 200
!
!      end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!calculate vertical velocity distribution      
!----------------------------------------  
  if (iteqv (maxn) /= 5 .AND. iteqv (maxn) /= 2 .AND. iteqv (maxn) < 8) call vrtvel

!print out result at freqency match, after last iteration (always) and convergence before reaching last iteration  
!----------------  
  if (mod (maxn, iprtMetai) == 0 .OR. maxn == nita .OR. nconv == 2) then
!generate and write some output data    
    call output (2)
!check for continuity at CCLs    
    call check
  endif

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jul07, necessary for autoconverge
!        !if (beiauto/=0.) then
!        !  if (temp_nan==1.0) then
!        !    maxn = 0.
!        !    GOTO 200
!        !  end if
!        !end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!restore tett as hours in year  
  tett = (dayofy - 1) * 24. + tet

!if model is Newton Converged, announce this!  
!--------------------------------------------  
  if (nconv == 2) then
!Set model Newton Converged    
!--------------------------    
    m_SimModel%isNewtonConv = .true.
!Announce the Schwarz convergence status (not Schwarz Converged)    
!---------------------------------------    
    if (m_SimModel%hasInnerBoundaries) call writeConvStatus (case_NotSchwarzConv, m_simModel%ID, SchwarzIt, Icyc)
    
!nothing to iterate any more, so get out of this steady cycle    
!------------------------------------------------------------    
    exit steadyCycle
  endif

    krestf = 1

!write result after iteration cycle, if desired    
!----------------------------------------------    
    if (nprti /= 0) then
      if (mod (maxn, nprti) == 0 .AND. ikalypsofm /= 0) then
!generate output file name        
        call generateoutputfilename ('stat', niti, 0, maxn, modellaus, modellein, modellrst, ct, nb, outputfilename, inputfilename)
!write the result        
        call write_kalypso (outputfilename, 'resu')

!MD: only for kohesive Sediment        
        IF (LSS > 0) THEN
          call generateOutputFileName ('stat', niti, 0, maxn, 'bed', modellein, modellrst, ct, nb, outputFileName, inputFileName)
          CALL write_KALYP_Bed (outputFileName)
        END IF
      endif
    endif

!get vegetation parameter, if calculation with vegetation is desired    
!-------------------------------------------------------------------    
    if (ivegetation /= 0) call get_element_cwr (m_SimModel)

!If not converged yet, but number of iteration exceeds, end steady calculation    
!-----------------------------------------------------------------------------    
    if (maxn >= nita) then
!Announce the Schwarz convergence status (Not Schwarz Converged)      
!---------------------------------------      
      if (m_SimModel%hasInnerBoundaries) call writeConvStatus (case_NotSchwarzConv, m_simModel%ID, schwarzIt, icyc)
!nothing to do any more, so get out of steady cycle      
!--------------------------------------------------      
      exit steadycycle
    end if
      
!end of loop of steady calculation  
  end do steadyCycle

!terminate Schwarz iteration, if it doesn't have inner boundaries for Schwarz iteration  
  if ( .NOT. (m_SimModel%hasInnerBoundaries)) exit steadySchwarzCycle

!end of Schwarz iteration
end do steadySchwarzCycle


!--------------------------------------------------------------------
!CALCULATION IS FINISHED, NOW DO SOME CALCULATION STEP POSTPROCESSING
!--------------------------------------------------------------------


!Checks Salinity Values against maximum and minimum permissible values and resets
! to keep within an appropriate range as appropriate
!-------------------------------------------------------------------------
SalLowPerm = 0.0001
SalHighPerm = 300.0
Do J = 1,NP
  If (Vel(4,J)<SalLowPerm) Then
    Vel(4,J) = SalLowPerm
  End If
  If (Vel(4,J)>SalHighPerm) Then
    Vel(4,J) = SalHighPerm
  End If
End Do

!calculating the cwr-values for trees; adding temporary storage of maxn = 0; Updating the cwr-values for trees after convergence  
!------------------------------------  
  temp_maxn = maxn
  maxn = 0
  if (ivegetation /= 0) call get_element_cwr (m_SimModel)
  maxn = temp_maxn

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jun07, autoconverge
!        if (beiauto/=0.) then
!          call autoconverge(4.)
!          if (autoindex==1.) then
!            autoindex = 0.
!            GOTO 200
!          end if
!        endif
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!write result after finished steady calculation  
!----------------------------------------------  
  if (ikalypsofm /= 0) then
!generate output file name    
    call generateoutputfilename('stat', niti, 0, 0, modellaus, modellein, modellrst, ct, nb, outputfilename, inputfilename)
!write results    
    call write_kalypso (outputfilename, 'resu')
  
!MD: only for kohesive Sediment    
    IF (LSS > 0) THEN
      call generateOutputFileName ('stat', niti, 0, 0, 'bed', modellein, modellrst, ct, nb, outputFileName, inputFileName)
      CALL write_KALYP_Bed (outputFileName)
    END IF
  end if

endif steadyCalculation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                END OF STEADY STATE BLOCK                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Check, if transient calculation is desired
!------------------------------------------
if (ncyc <= 0) then
  call zvrs (1)
  return
endif

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!IT'S TOTALLY WRONG HERE
!      do i=1,ncl
!        do k=1,3
!          specccold(i,k)=specccfut(i,k)
!        end do
!      end do
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 TRANSIENT CALCULATION                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      if (beiauto/=0.) then
!        call autoconverge(5.)
!      end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!initialise global variables
!---------------------------
!drying/wetting countdown
idryc = 0
!iteration counter
maxn = 0

!What is it for, i.e. when is it executed?
!-----------------------------------------
if(lbed > 0) then
!Calculate kinematic viscosity  
  call kinvis
!Compute equilibrium transport (transport capacity)  
  call sandx
endif


!run through transient time steps
!--------------------------------
DynamicTimestepCycle: do n = 1, ncyc

!Jump over all time steps, until given initial step (desired by user) is reached  
  LaterTimestep: if (ifile == 60 .AND. n < iaccyc) then
!increase current time step    
    icyc = icyc + 1
!read the input data lines to continue reading the control file    
    call inputd(ibin)
!get next time step    
    cycle DynamicTimestepCycle

!Process the following time steps  
  else
!Rewind output file, if rewinding is desired by the user    
    if (mod (n, ioutrwd) == 0) rewind lout

!start calcualtion cycle    
    write(*,*) 'starting cycle',n

!reinitialise global variables    
!-----------------------------    
    MAXN = 1
    IT = N
    do j = 1, ne
      dfct (j) = 0.0
    enddo

!Shift local degrees of freedom forward at storage to update for next current step    
    do j = 1, np
      do k = 1, ndf
!penultimate time step        
        v2ol (k, j) = vdoto (k, j)
!last time step        
        vold (k, j) = vel (k, j)
        vdoto (k, j) = vdot (k, j)
!???        
        iespc (k, j) = 0
      enddo
!penultimate time step      
      h2ol (j) = hdot (j)
!last time step      
      hol (j) = hel (j)
      hdot (j) = hdet (j)
    enddo

    icyc = icyc + 1

!Read boundary conditions for current time step    
    call inputd (ibin)
  endif LaterTimestep
  
!get wave data, only required, if desired  
!----------------------------  
  if ((lsand > 0 .OR. lbed > 0) .AND. iwvin > 0) call getwave

!get surface stress data, if desired  
!------------------------  
  if (iwvfc == 102) then
    call getsst
  elseif (iwvfc == 104) then
    call getdrsst
  endif

!get SWAN data, if desired  
!-------------  
  call swandt


!set up factors for time derivatives  
!-----------------------------------  
  if (delt > 0.) then
    altm = alpha/ delt
!following is for sediment transport    
!alphasn = 1.8    
    alphasn = 2.0
  else
    altm = 0.
  endif

!lowest degree of freedom??? Yes! Thanks a lot 'know-it-all' :), sustainably ensured?  
  NDL = 1
  
!Initialise Mellor Yamada turbulence formulation  
!-----------------------------------------------  
  if (iteqv (maxn) /= 5 .AND. ioptzd > 0 .AND. ioptzd < 3) call mellii

!updating the time derivatives  
!-----------------------------  
  forallnodes: do j = 1, np
!get transformed depth regarding MARSH-algorithm    
    vtm = vel (3, j)
    call amf (hel (j), vtm, akp (j), adt (j), adb (j), d1, d2, 0)

!update for all degrees of freedom    
!TODO: Is this necessary? Reasons for questions: 1. water stage/ depth is revised in next logic; 2. alpha is    
!overwritten for sediment (alphasn) stuff and applied later again    
    ForAllDOFs: do k = ndl, ndf
      if (iespc (k, j) == 0) then
!TODO: This can't work, because there will never be a time derivative value unequal zero        
!project the variable used within this turn based on the current value and the time derivative        
!don't calculate a time derivative        
        if (iproj == 0) then
          vel (k, j) = vel (k, j) + delt * vdot (k, j)

!project the variable used within this turn use projection for the variable of current run by the old        
!variable and the variable of the penultimate calculation step        
!calculate the time derivative        
        elseif(iproj == 2) then
          dtfac       = thetcn * vdot (k, j) + (1. - thetcn) * v2ol (k, j)
          vel (k, j)  = vel (k, j) + delt * dtfac
          vdot (k, j) = altm * (vel (k, j) - vold (k, j)) - vdoto (k, j) * (alpha - 1.)

!IPROJ == 1 (normal way)        
!classical method without time projection, but just calculating the time derivative based on the Finite Difference Scheme        
        else
          vdot (k, j) = altm * (vel (k, j) - vold (k, j)) - vdoto (k, j) * (alpha - 1.)
        endif
      endif
    enddo ForAllDOFs

!update for water depths    
    if (ndl == 1) then
      if (iespc (3, j) == 0) then
!project in time, if desired        
        if (iproj == 0) then
          hel (j) = hel (j) + delt * hdet (j)
          call amf (hel (j), htp, akp (j), adt (j), adb (j), d1, d2, 1)
          vel (3, j) = htp
        elseif (iproj == 2) then
          dtfac = thetcn * hdet (j) + (1. - thetcn) * h2ol (j)
          hel (j) = hel (j) + delt * dtfac
          vh = vel(3,j)
          call amf (hel (j), vh, akp (j), adt (j), adb (j), d1, d2, 1)
        endif
!calculate the derivatives        
        if (delt > 0.) then
          vdot (3, j) = altm * (vel (3, j) - vold (3, j)) - (alpha - 1.) * vdoto (3, j)
        else
          vdot (3, j) = 0.
        endif
      endif
    endif
  enddo forallnodes

!set up water surface elevations  
!-------------------------------  
  do j = 1, npm
!calculate current water stage, depending on MARSH algorithm being operative or not    
    if (idnopt /= 0) then
      hs = vel (3, j)
      call amf (h, hs, akp (j), adt (j), adb (j), ame1, d2, 0)
      wsll (j) = h + ado (j)
    else
      wsll (j) = vel (3, j) + ao (j)
    endif
    
!water surface update for 3D purposes    
    k = nref (j) + 1
    if (k /= 1) then
      l = nref (j) + ndep (j) - 1
      if (l >= k) then
        do m = k, l
          wsll (m) = wsll (j)
        enddo
      endif
    endif
  enddo

!update water suface elevations for midside nodes  
!------------------------------------------------  
  do k = 1, nem
!if active element    
    if (ncrn (k) > 0) then
!run through midside nodes      
      do l = 2, ncrn (k), 2
!if not a 1D junction element        
        if (imat (k) < 901 .OR. imat (k) > 903) then
!get the current midside node from the sequence          
          j = nops (k, l)

!stop on wrongly defined elements; shouldn't happen          
          if (j <= 0) call ErrorMessageAndStop (1212, k, mcord (k, 1), mcord (k, 2))

!get the neigbouring forenode          
          n1 = nops (k, l - 1)
!get the neighbouring afternode          
          if (mod (l, ncrn (k)) == 0) then
            n2 = 1
          else
            n2 = nops (k, l + 1)
          endif
!Calculate midside's water surface elevation as average of neighbours          
          wsll (j) = 0.5 * (wsll (n1) + wsll (n2))

!update for 3D elements, too          
          kk = nref (j) + 1
          if (kk /= 1) then
            ll = nref (j) + ndep (j) - 1
            if (ll >= kk) then
              do m = kk, ll
                wsll (m) = wsll (j)
              enddo
            endif
          endif
        endif
      enddo
    endif
  enddo

!set ice thickness value  
  do j = 1, npm
    icethkol (j) = icethk (j)
  enddo

!get external stress data  
!------------------------  
  call getstress

!MD: Gesamter LSS>0 Block verschoben: 05.11.2008
!MD  --> siehe unten im Iteration LOOP!!


!copy current number of iterations to be processed within transient time steps  
!-----------------------------------------------------------------------------  
  nita = nitn
  
!output of solution after each desired time step  
!-----------------------------------------------  
!get time step frequency to write out full results  
  iprtf = abs (nprtf)
!if frequency is not given, chose output for each step (iprtf = 1)  
  if (iprtf == 0) iprtf = 1

!get iteration frequency  
  iprti = abs (nprti)
!if frequency is not given, chose only to give out after last iteration  
  if (iprti == 0) iprti = nita
!get iteration frequency to write out   
  iprtMetai = abs (nprtmetai)
  if (iprtMetai == 0) iprtMetai = nita

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


!PROCESS SCHWARZ ITERATION FOR DISTRIBUTED CALCULATIONS WITH KALYPSO SERVICE  
!---------------------------------------------------------------------------  
!Initialize for the case no Schwarz iteration has to be done  
  m_SimModel%isSchwarzConv = .true.
  m_SimModel%isNewtonConv = .false.


!Schwarz iteration loop  
!----------------------  
  SchwarzIt = 0
  SchwarzCycle: Do
    SchwarzIt = SchwarzIt + 1
    
!if model has inner boundaries, there must be a Schwarz iteration with neighbouring models    
!-----------------------------------------------------------------------------------------    
    if (m_SimModel%hasInnerBoundaries) then
!Get Continue Command      
!--------------------      
      continueCommand = getSchwarzIterationCommand (schwarzIt, icyc)
      contComUnsteady: select case (continueCommand)
        case (enum_stop)
!FIXME: Stop handling must be done!          
          stop 'Divergence in any part of the entire model!'
        case (enum_nextStep)
!nullify the boundary condition status of the inner boundary nodes          
          call nullifyInnerBCstatus (ccls, nfix, nfixp, alfa, ncl)
          exit SchwarzCycle
        case (enum_continueStep)
          continue
      end select contComUnsteady
!Write the own inner conditions to an output file, to overgive it to the neighbours      
!----------------------------------------------------------------------------------      
      call writeInnerBoundaryConditons (m_simModel, schwarzIt, icyc, ccls, ncl, maxp, vel, wsll, cord)
!Re-Initialize the model convergency status, i.e. assume it is converged and check assumption consecutively      
!----------------------------------------------------------------------------------------------------------      
      if (m_SimModel%hasInnerBoundaries) m_SimModel%isSchwarzConv = .true.
!Generate BC file filenames list      
!-------------------------------      
      bcFiles => getNeighbourBCFiles (schwarzIt, icyc, m_SimModel)
!store boundary conditions from last Schwarz iteration      
!-----------------------------------------------------      
      call storeOldInnerBoundaryConditions (ccls, spec, ncl, maxp)
!Read the incoming boundary conditions from the neighbours      
!---------------------------------------------------------      
      call getNeighbourBCs (bcFiles, ccls, ncl, maxp, spec, nfix)

!Improve boundary conditions by nested intervals (variables are still local)      
!-----------------------------------------------      
!TODO: Make it more flexible, invoking different algorithms!      
      improvementAlgo = 1
! defined in Mod_ContiLines: enum_nestedIntervals = 1      
      if (schwarzIt <= 2) call improveBCs (improvementAlgo, ccls, ncl, spec,maxp)

!write header of console output      
!------------------------------      
      write (*,'(a15,i4,a14,f8.6)') 'Schwarz-cycle: ', schwarzIt, ' Conv-Border: ', m_SimModel%schwarzConv
      write (*,'(a)') 'CCL    vx-conv max    vy-conv max    h-conv max'
      write (*,'(a)') '----------------------------------------------------'
!Check for Schwarz convergence and reinitialize under circumstances Newton Convergence      
!-------------------------------------------------------------------------------------      
      m_SimModel%isSchwarzConv = checkSchwarzConvergence (ccls, ncl, m_SimModel%schwarzConv)
      if ( .NOT. (m_SimModel%isSchwarzConv)) m_SimModel%isNewtonConv = .false.
    endif

!reinitialise global variables    
!-----------------------------    
    maxn = 0
    itpas = 0

!Iterate transient calculation    
!-----------------------------    
    DynamicIterationCycle: Do

!Check for convergence status      
      if (m_SimModel%isNewtonConv) then
!Write some information output        
!-----------------------------        
        write(*,*) 'Schwarz iteration cycle: ', SchwarzIt
        write(*,*) 'Boundary conditions are temporarily converged: Nothing to do!'
        write(*,*) 'Waiting for next Schwarz Cycle'
!Announce the convergence status (Schwarz converged)        
!---------------------------------------------------        
        call writeConvStatus (case_SchwarzConv, m_simModel%ID, SchwarzIt, Icyc)
!nothing to do in this cycle, so get out of here        
!-----------------------------------------------        
        exit DynamicIterationCycle
      endif

!counter for newton iterations      
      maxn = maxn + 1
      
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!       if (beiauto/=0.) call autoconverge(7.)
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!heat exchange?    
!--------------    
    if (maxn == 1) then
      write (75, *) 'going to heatex-535',n,maxn,tet,itpas
      call heatex (nmat, delt, lout, iyrr, tet, itpas)
      itpas = 1
    elseif (iteqv (maxn) == 8) then
      write (75, *) 'going to heatex-540', n, maxn, tet, itpas
      call heatex (nmat, delt, lout, iyrr, tet, itpas)
    endif

!Calculate 1D/2D line-2-element transition distribution    
!------------------------------------------------------    
    if (MaxLT /= 0) call TransVelDistribution
    if (maxps /= 0) call PipeSurfaceConnectionQs

!drying/ wetting, if desired    
!---------------------------    
    if (idswt /= 0) then
      if (idryc == 0) then
!get potentially rewetted nodes        
        write(*,*) 'entering rewet'
        call rewet
!dry wet and potentially rewetted nodes        
        write(*,*) 'entering dry'
        call del
!after processing drying wetting, reset countdown for drying/wetting to the original frequency        
        idryc = idswt
      endif
    endif

!equation dropout    
!----------------    
    if (idrpt /= 0) then
      call drpout
!if inoperative, set all DOFs to be active    
    else
      do j = 1, np
        do k = 1, ndf
          iactv (j, k) = 10
        enddo
      enddo
    endif

!set up boundary line, based on active mesh parts    
!--------------------    
    call bline (maxn)

!That's clear: Question is, what is exactly done here; is there any other READABLE way of executing this section?    
!----------------------------------------------------------------------------------------------------------------    
!HN. 12June2009    
!ICK is a flag to define the degree of freedom, for example:     
!ICK = 4 is Salinity (iteqs = 0).    
!ICK = 5 is Temprature(iteqs = 1).    
!ICK = 6 is concentration(iteqs = 2).    
!ICK = 7 is the bed change/equilibrium bed load(iteqs = 3).      
!iteqs is defined in control file by user for each iteration. iteqs = 3 is bed deformation, which is not included in RMA10s    
! User Guide version 3.5E.    
! HN end.    
!---    
    ick = iteqs (maxn) + 4

!3D to 2D collapse; experimental    
!-------------------------------    
    if (itransit == 1 .AND. maxn < 4) call twodsw

!update shears etc.    
!------------------    
    if ((lsand > 0 .OR. lbed > 0) .AND. ick == 6) then
      call kinvis
!call shear      
      write(*,*) 'going to sandx'
!compute equilibrium transport (transport capacity)      
      call sandx
! compute deposition or erosion rate based on transport capacity theory.      
      call bedxcg
    endif

!MD: Gesamter LSS>0 Block in Iterationschleife verschoben: 05.11.2008    
!MD -----------------------------------------------------------------    
    if (lss > 0) then
      call setvel
      call kinvis
!ciat mar06 adding new wbm bedshear stress subroutines for cohesive sediment calcs      
!NiS,Nov06: Seems, that name of first shear-Subr is not correct. Change SHEAR1 to WSHEAR1      
!call shear1      

!MDMD:  Aufruf nur dann, wenn wirklich Wellen vorhanden!!      
!MD Neu:   Abfrage ueber Kennungen      
      if (iwvin == 101 .OR. iwvfc==102 .OR. iwvfc==104) then
        call wshear1
        call wshear2
      else
!MD: Reaktivierung von SubR SHEAR        
!MD neu:   Sohlschub-Berechnung mit Kalypso (ks-werten/ Lambda!)        
        if (ikalypsofm /= 0) then
          write(*,*) 'aufruf kalyp_shear'
          call get_ffact
          call kalyp_shear
!MD: Wenn nicht Kalypso, dann KING-Ansatz
        else 
          call shear
        endif
      end if
!call shear      
!call wshear1      
      call depsn
      call merosn (1)
      call serosn (1)
    endif
!MD ---------------------------------------------------------------    

!CIPK OCT02    
    470   CONTINUE

!load the DOFs, i.e. set up the equations system    
!-----------------------------------------------    
    call load

!Compute areas of continuity lines for stage flow input    
!------------------------------------------------------    
    call agen

!Initialise Mellor Yamada turbulence formulation    
!-----------------------------------------------    
    if (maxn >= 2 .AND. iteqv (maxn) /= 5 .AND. iteqv (maxn) /= 2 .AND. ioptzd > 0 .AND. ioptzd < 3) call mellii

!set information, that advanced turbulence models can be used    
!------------------------------------------------------------    
    if (iutub == 0) then
      if (maxn > 2 .OR. n > 1) iutub = 1
    endif

!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------
!nis,feb07,testing: Write whole matrix
!if (maxn > -1) then
!  write (matrixname,'(a3,i3.3,a1,i3.3a4)')'mat',maxn,'_',icyc,'.txt'
!  teststat = 0
!  open (9919, file = matrixname, iostat = teststat)
!  if (teststat /= 0) STOP 'ERROR - while opening matrix file'
!  WRITE(9919,*) 'maxn', maxn
!endif
!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------

!get some time informations    
!--------------------------    
    call second (atim (2))
  
!call the solver    
!---------------    
    if (icpu == 0) then
!frontal solution scheme      
!by Irons, B.: A Frontal solution program for Finite Element analysis. In: Internaltiional Journal for numerical      
! Methods in Engineering. Vol. 2, p. 5-32. 1970.      
      !call front_pardiso (1)
      call front (1)
    else
!Pardiso solver from the Intel MKL library      
!by Schenk, O., G�rtner, K.: Solving unsymmetric sparse systems of linear equations with PARDISO. In: Jorunal of      
! Future Generation Computer Systems, Vol. 20 Iss. 3, p. 475-487. 2004.      
      call front_pardiso (1)
      !call front_petsc(1)
    endif
  

!Write consumption informations to the time file, if desired    
!-----------------------------------------------    
    if (itimfl > 0) then
      call second (atim (3))
      write (itimfl, 6101) n, maxn, atim (3) - atim (2), atim (3) - atim (1)
    endif        
   6101 format ('step', i5, ' iteration', i5, '  time in front-horz', f12.2, ' total time to date for run =', f12.2)
   
!countdown for drying/wetting    
!----------------------------    
    idryc = idryc - 1

!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------
!close testfile
!close (9919, status = 'keep')
!-----------------------------------------------------------
!reserve for testoutput of matrices
!-----------------------------------------------------------

!update degrees of freedom and check for convergence    
!---------------------------------------------------    
    call update (schwarzIt)

!Find the maximum values at each node    
!------------------------------------    
    call FindMinMaxValues (MaxP)

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!      !EFa jul07, necessary for autoconverge
!      if (exterr==1.0) then
!        call autoconverge(8.)
!        cycle DynamicIterationCycle ???
!
!      end if
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

!calculate vertical velocity distribution        
!----------------------------------------    
    if (iteqv (maxn) /= 5 .AND. iteqv (maxn) /= 2 .AND. iteqv (maxn) < 8) call vrtvel

!Write results, if 1. time step matches the frequency and 2. iteration matches the frequency or convergence    
    if (mod (icyc, iprtf) == 0 .AND. (mod (maxn, iprtMetai) == 0 .OR. maxn == nita .OR. nconv == 1 .OR. nconv == 2)) then
      call output(2)
      call check
    endif

!update time derivatives    
!-----------------------    
    do j = 1, np
!transform depths      
      vtm = vel (3, j)
      call amf (hel (j), vtm, akp (j), adt (j), adb (j), d1, d2, 0)
!run through the degrees of freedom and calculate the time derivatives      
      do  k = 1, ndf
        vdot (k, j) = altm * (vel (k, j) - vold (k, j)) - (alpha - 1.) * vdoto (k, j)
      enddo
!and for the water stage      
      hdet (j) = altm * (hel (j) - hol (j)) - (alpha - 1.) * hdot (j)
    enddo


!--------------------------------------------------------------------
!TODO: Following needs to be updated, but more knowledge is necessary
!--------------------------------------------------------------------
!CIPK OCT02  CHECK FOR NEGATIVE TEMPS

      IF(ICESW > 0 .AND. ITEQV(MAXN) == 8 .AND. ITPAS < 4)THEN
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
        IF(ITEQV(MAXN) == 8 .AND. ITPAS == 0) ITPAS=1 
!--------------------------------------------------------------------
!TODO: Following needs to be updated, but more knowledge is necessary
!--------------------------------------------------------------------


!restore tett as hours in year    
    tett = (dayofy - 1) * 24. + tet
    write (75, *) 'tet, dayofy', tet, tett, dayofy

!If fully converged time step, get out of iteration cyle    
    if(nconv == 2) then
!Set Newton convergence status      
!-----------------------------      
      m_simModel%isNewtonConv = .true.
!Announce Schwarz Convergence status      
!-----------------------------------      
      if (m_simModel%hasInnerBoundaries) call writeConvStatus (case_NotSchwarzConv, m_simModel%ID, SchwarzIt, Icyc)
      
!noting to do here, so get out of here      
!-------------------------------------      
      exit DynamicIterationCycle
    end if

!write result after iteration cycle, if desired    
!----------------------------------------------    
    if (nprti /= 0) then
      if (mod (icyc, iprtf) == 0 .AND. mod (maxn, iprti) == 0) then
!generate file name        
      call generateOutputFileName ('inst', niti, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName,inputFileName)
!write result after iteration        
        call write_kalypso (outputfilename, 'resu')

!MD: only for kohesive Sediment         
         IF (LSS > 0) THEN
           call generateOutputFileName ('inst',niti, icyc, maxn, 'bed', modellein, modellrst, ct,nb, outputFileName, inputFileName)
           CALL write_KALYP_Bed (outputFileName)
         END IF
      endif
    endif


!get vegetation parameter, if calculation with vegetation is desired      
!-------------------------------------------------------------------      
      if (ivegetation /= 0) call get_element_cwr (m_SimModel)

!If not converged yet, but number of iteration exceeds, end dynamic calculation      
!------------------------------------------------------------------------------      
      if (maxn >= nita) then
!Announce Schwarz Convergence status        
!-----------------------------------        
        if (m_SimModel%hasInnerBoundaries) call writeConvStatus (case_NotSchwarzConv, m_simModel%ID, SchwarzIt, Icyc)
!nothing to do anymore, so get out of here        
!-----------------------------------------        
        exit DynamicIterationCycle
      end if

!end of dynamic newton cycle    
!---------------------------    
    enddo DynamicIterationCycle
    
!if model is not distributed (DOMAIN PARTITIONING), do not schwarz iterate    
    if ( .NOT. (m_SimModel%hasInnerBoundaries)) exit SchwarzCycle
  
!If not Schwarz converged it can also not be newton converged    
    if ( .NOT. (m_SimModel%isSchwarzConv)) m_SimModel%isNewtonConv = .false.
    
  enddo SchwarzCycle 
  
!-----------------------------------------------------------------------------------------------
!CALCULATION IS FINISHED - NOW DO SOME ADDITIONAL RESULTS POSTPROCESSING AT THE END OF TIME STEP
!-----------------------------------------------------------------------------------------------

!update bed information for sand case  
!------------------------------------  
  if (lsand > 0) then
     write(75,*) 'rma10: going to bedsur'
     call bedsur
  elseif(lss > 0) then
!C-      IF COHESIVE SED IS SIMULATED, CALCULATE BED CHANGE    
!MD:    Erneuter Aufruf der Erosionsroutinen mit II = 2    
!MD       zur Berechnung der Sohlanderung infolge ersosion    
    call merosn(2)
    call serosn(2)
    call newbed(2)
  endif

!MD Aufruf der Sohlentwicklung nur einmal fuer sandige Sohle  
!------------------------------------  
  if (lbed > 0 .AND. lsand <= 0) then
    write(75,*) 'rma10-674 going to bedsur'
!MD 11.08.2008: BEDLBED wurde deaktiviert, da Routine veraltet und fehlerhaft:    
!MD   alle dort berechneten Werte GAN0 und GAN sind unsinnig, da immer = null    
!MD   BEDLBED wurde mit der aktuelleren Routine BEDSUR ersetzt    
!MD CALL BEDLBED    
    call bedsur
  endif

!----------------------------------------------------------------------------
! HN JUNE 2009  
!CCL - contiLines; ccls  
!Fe_node information (x,y,z = ao(i))  
!Elements connecting to each nodes  
!Wasserstand - wsll(Knoten)  
!-----------------------------  
!Einbau des Hassan-Algorithmus  
!-----------------------------  
!  
! if there is a profile data file or no profile data file but  
! contlines in the form of profile then run bank_evolution once.  
    
  if (BANKEVOLUTION) then
! Restart bank profiles   
   if ( (IPROFIN == 731) .AND. (CallCounter == 0) ) then
    CallCounter = iaccyc
   else 
    CallCounter = CallCounter + 1
   endif
    
    call bank_evolution (CallCounter)
  endif 
!-------------------------------------------------------------------------------'

!calculating the cwr-values for trees; adding temporary storage of maxn = 0; Updating the cwr-values for trees after convergence  
!------------------------------------  
  temp_maxn = maxn
  maxn = 0
  if (ivegetation /= 0) call get_element_cwr (m_SimModel)
  maxn = temp_maxn

!TODO: Isn't this doubled?  
!-------------------------  
  tett = (dayofy - 1) * 24. + tet
  write (75, *) 'tet, dayofy', tet, tett, dayofy

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!        !EFa jun07, autoconverge
!        if (beiauto/=0.) then
!          call autoconverge(9.)
!          if (autoindex==2.) then
!            autoindex = 0.
!465 is the now the initial point of the transient calculation, starting at 'DynamicIterationCycle'-do logic             
!            GOTO 465
!          end if
!        endif
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

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

!save results file  
!-----------------  
  if (ikalypsofm > 0 .AND. mod (icyc, iprtf) == 0) then
    MAXN = 0
    if (icyc >= irsav) then
!generate file name for result      
      call generateOutputFileName ('inst', niti, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
!calculate the content of the storage elements      
      call calcAllStorageContents (StorageElts)
!write result      
      call write_kalypso (outputfilename, 'resu')
!MD: only for kohesive Sediment      
      IF (LSS > 0) THEN
        call generateOutputFileName ('inst', niti, icyc, maxn, 'bed', modellein, modellrst, ct, nb, outputFileName, inputFileName)
        CALL write_KALYP_Bed (outputFileName)
      END IF
    endif
    
    if (icyc >= irMiniMaxiSav) then
!MD: keine Ausgabe koh. bed fuer Mini & Maxi      
!generate file name for minimum values       
      call generateOutputFileName ('mini', 0, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
!write minimum values file      
      call write_Kalypso (outputFileName, 'mini')
!generate file name for maximum values       
      call generateOutputFileName ('maxi', 0, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
!write maximum values file      
      call write_Kalypso (outputFileName, 'maxi')
    endif 
  end if


enddo DynamicTimestepCycle


call zvrs (1)
return

end subroutine RMA_Kalypso


!**************************************************
subroutine FindMinMaxValues(Points)
USE blk10mod
USE parakalyps

implicit none

INTEGER :: i
INTEGER (kind = 4), intent (IN) :: Points

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
    maxrausv (i) = wsll(i)
  end if
  if (vel (3, i) < minvel (3, i)) then
    minvel (3, i) = vel (3, i)
    minrausv (i) = wsll(i)
  end if
end do

end subroutine

end module
