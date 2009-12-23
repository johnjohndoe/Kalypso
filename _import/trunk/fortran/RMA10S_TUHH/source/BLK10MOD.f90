      module BLK10MOD
!
      use ManholeDefinitions
      use mod_storageElt
      use mod_fileHandler
      use mod_contiLines
      use mod_controlStructure
!
!some general strings      
!--------------------      
      character (len = 72) :: title
      character (len = 1000) :: header
!meaning of the variables      
!------------------------      
!title      title entry      
!header     title header      
!
!i/o file and unit definitions (up to date); for description look into file.sub      
!------------------------------------------------------------------------------      
!
      type (fileCtrl), save :: fileControl
!obsolete files      
      integer (kind = 4) :: icfl
      integer (kind = 4) :: lin, lout, litr, imesout
      integer (kind = 4) :: ikalypsofm, ifile, nb, ibup
      integer (kind = 4) :: incstr, insfl, IREBED
      integer (kind = 4) :: iwvfc, instr, iowgt, icordin, iocon, iwvin
      integer (kind = 4) :: intims, inwgt, imassout, itimfl
      integer (kind = 4) :: nscr, nd1
      integer (kind = 4) :: IPROFIN
!meaning of the variables      
!------------------------      
!           TODO: examine, how ICFL works      
!icfl:      unit number definition of the console      
!LIN        unit of control file (control.r10)      
!LOUT       after 1st call of file.sub unit of output file (.ech)      
!           after 2nd call of file.sub LOUT becomes the general output unit (.out-file)      
!LITR       unit of iteration file (.itr)      
!IMESOUT    unit of Message file (MESS.ech)      
!IKALYPSOFM unit number of the results ouput files      
!IFILE      model geometry (in *.2d-format)      
!NB         restart input file (binary or ASCII)      
!IREBED     Bed-restart input file (in *.bed-format)      
!ibup       external boundary condition file      
!INCSTR     control structure data      
!insfl      input control stage flow relationship file      
!IWVFC      input surface stress data file      
!INSTR      surface traction from external grid      
!IOWGT      Mesh weighting output      
!ICORDIN    external grid data      
!IOCON      continuity line hydrograph file      
!IWVIN      input wave data file      
!IPROFIN    INPUT DATA FILE OF PROFILES, REQUIRED FOR SIMULATION OF BANK EVOLUTION      
!INTIMS     on/off controlling of constrol structure time series      
!INWGT      external inpolation weighting data file      
!IMASSOUT   ???      
!ITIMFL     processing time data      
!           TODO: Make one unit number from nscr and nd1      
!nscr       scratch file unit for solver purposes      
!nd1        copy of scratch file unit nscr      
!IRPOFIN   INPUT PROFILE DATA FILE FOR SIMULATION OF BANK EVOLUTION       
!other units      
!-----------      
!unit definitions in other blocks      
!--------------------------------      
!integer (kind = 4) :: unitSWAN (BLK_SWANF)      
!integer (kind = 4) :: IIQ, IOMET (BLK11MOD)      
!
!obsolet unit definitions      
!------------------------      
!integer (kind = 4) :: NLL, NIPT, NOPT, NB, IFIT, IFOT      
!integer (kind = 4) :: NAINPT, IRMAFM, JBEDOT, ISMSFM, ISMSFM1      
!integer (kind = 4) :: ISMSGN, ISMSFM2, IMESHOUT, IWAVOT, IBEDOT, INBNWGT      
!integer (kind = 4) :: INBNSTR, IOBNWGT      
!
!primary execution control parameters      
!------------------------------------      
      integer (kind = 4) :: icpu
      integer (kind = 4) :: ioptzd
      integer (kind = 4) :: idrpt
      integer (kind = 4) :: idnopt
      integer (kind = 4) :: iproj
      integer (kind = 4), allocatable, dimension (:,:) :: IESPC
!Meaning of the variables      
!------------------------      
!icpu       switch for controlling of equation solver      
!           0  - use frontal solution scheme      
!           1+ - use pardiso solver from the MKL; icpu gives the number of cores/threads to be utilized      
!ioptzd     switch for turbulence closure method      
!           0 - original RMA-10      
!           1 - quadratic functions      
!           2 - linear functions      
!idrpt      switch for equation dropout      
!           0 - not operative      
!           TODO: Find out exactly what equation dropout does      
!           1 - dropout equations but bring all of them back, if active set of constituents changes      
!           TODO: Find out exactly what advanced equation dropout does      
!           2 - dropout advanced method;       
!idnopt     switch for Marsh algorithm      
!           0 - use simple element drop out      
!          -1 - use default Marsh parameters (      
!          -2 - use user defined Marsh parameters      
!iproj      switch for time projection method      
!           0 - project in time based on time derivative of variable (time derivative is not changed!)      
!           1 - do not project in time      
!           2 - project based on current and previous value (purpose? faster convergence?)      
!iespc      ????            
!      
!      
!
!secondary execution control parameters; initialised in the file opening process      
!--------------------------------------      
      integer (kind = 4) :: iutub, krestf
!Meaning of the variables      
!------------------------      
!iutub : switch (0 or 1) - 0 means that certain turbulence options can not be executed.      
!                          1 means all turbulence options can be executed, i.e. after correct restart or after to iterations with constant eddies.      
!TODO: Meaning of krestf      
!krestf: switch (0 or 1) - 0 means not restarted and ???      
!                          1 means restarted or ???; the value is edited in the rma10.sub, too. The meaning is not clear. It is used in surcof.      
!
!model name i/o parameters      
!-------------------------      
      character (len = 96) :: fnam
      character (len = 32) :: modellein, modellrst
      character (len = 1)  :: ct
      character (len = 10) :: modellaus
      character (len = 8)  :: id
      character (len = 72) :: dlin
!meaning of the variables      
!------------------------      
!fnam        global output file name prefix for (.itr, .out, .ech, MESS.ech)      
!modellein   name (string) of the input geometry file      
!modellrst   name (string of the restart file      
!ct          prefix-digit of output result files in *.2d-format      
!modellaus   suffix (string) of the results file in *.2d format      
!id          character variable into which the first 8 digits of any input line from a file are read      
!dlin        character variable into which the last 72 digits of any input line from a file are read      
!
!model dimension parameters      
!--------------------------      
      integer (kind = 4) :: maxP, maxE, maxA, maxT, maxLT, maxps, ncl,  &
     &                      maxweir
      integer (kind = 4) :: maxSE
      integer (kind = 4) :: np, ne, lp, le
      integer (kind = 4) :: npm, nem
      integer (kind = 4) :: nmat
      integer (kind = 4) :: maxPolyA, maxPolyQ, maxPolyB
!meaning of the variables      
!-----------------------------      
!maxp:      maximum number of points (surface points; includes midsides)      
!maxe:      maximum number of elements      
!maxa:      maximum number of arcs (auxiliary mean to generate mesh)      
!maxt:      maximum number of 1D/2D transition elements for 1D/2D element-2-element transitions      
!           maxt is set up in getgeo, because only if the geometry of all elements is recognized      
!           it can be determined, whether and how many 1D/2D node-2-element transitions are present      
!maxlt:     maximum number of 1D/2D transition elements for 1D/2D line-2-element transitions      
!maxps:     maximum number of pipe-surface connections      
!ncl:       number of continuity lines      
!ne         maximum element number      
!le         minimum element number      
!np         maximum node number      
!lp         minimum node number      
!npm        maximum node number; copy for 3D purposes      
!nem        maximum element number; copy for 3D purposes      
!nmat       number of material types      
!maxPolyA:  maximum number of polynomials to describe A(h) at any node      
!maxPolyQ:  maximum number of polynomials to describe Q(h) at any node      
!maxPolyB:  maximum number of polynomials to describe alpha(h)/ beta(h) at any node      
!
!element definition data      
!-----------------------      
      integer (kind = 4)                               :: ncn
      integer (kind = 4), allocatable, dimension (:)   :: ncorn, ncrn
      integer (kind = 4), allocatable, dimension (:,:) :: nop, nops
      integer (kind = 4), allocatable, dimension (:)   :: imat
      integer (kind = 4), allocatable, dimension (:)   :: netyp
      integer (kind = 4), allocatable, dimension (:)   :: nfixh
      real (kind = 8), allocatable, dimension (:)      :: th
      real (kind = 8), allocatable, dimension (:)      :: chez, zmann
!meaning of the variables      
!------------------------      
!ncorn   number of corner nodes per element      
!ncrn    copy of the ncorn array      
!ncn     local copy of the number of nodes in the current element      
!nop     nodes of an element, anticlockwise (corner - midside - sequence)      
!TODO: Are nops and nop both necessary?               
!nops    the same as nop      
!imat    material type of an element      
!netyp   determines the sort of an element (1D-type, 2D-type, 3D-type, transition-type)      
!        TODO: What is the main axis?      
!th      general direction angle of an element; determined by the main axis      
!chez    (optional) Chezy-coefficient of an element      
!zmann   (optional) Manning's n coefficient of an elements      
!nfixh      reordering number; position at which element will be solved      
!
!general node geometry definition data      
!-------------------------------------      
      real (kind = 8), allocatable, dimension (:,:)  :: cord
      real (kind = 8), allocatable, dimension (:)    :: ao, aorig
!
!general node calculation value data definition      
!----------------------------------------------      
      real (kind = 8), allocatable, dimension (:,:) :: vel, vdot
      real (kind = 8), allocatable, dimension (:,:) :: vold, vdoto
      real (kind = 8), allocatable, dimension (:,:) :: v2ol
      real (kind = 8), allocatable, dimension (:)   :: vvel
      real (kind = 8), allocatable, dimension (:)   :: alfak
      real (kind = 8), allocatable, dimension (:)   :: den
      real (kind = 8), allocatable, dimension (:)   :: hel, hol
      real (kind = 8), allocatable, dimension (:)   :: hdet, hdot
      real (kind = 8), allocatable, dimension (:)   :: h2ol
      real (kind = 8), allocatable, dimension (:)   :: wsll
!meaning of the variables      
!------------------------      
!cord       x-, y- and ?-coordinates of a node      
!ao         bed elevation of a node      
!aorig      original value of the bed elevation of a node; necessary for bed evolution calculations      
!vel        The degree of freedom field; it's the key array      
!vdot       derivative of the degrees of freedom      
!vold       The degree of freedom field from the last time step; i.e. storage of old results      
!vdoto      derivative of the degrees of freedom from the last time step; i.e. storage of old results      
!v2ol       The degree of freedom field from the penultimate time step; i.e. storage of old results      
!for all counts:      
!           1 - x-velocity      
!           2 - y-velocity      
!           3 - water depth      
!           4 - salinity      
!           5 - temperature      
!           6 - suspended sediment      
!TODO: what is vel(7,x)                  
!           7 - ??? perhaps water column potential by element      
!vvel       vertical velocity; for 3D applications      
!alfak      Fixed flow direction angle at a certain node; Value 0.0 zero means, that flow direction not restricted      
!TODO: What actually is the 3rd coordinate? Is it the same as ao                  
!den        density at each node      
!hel        real depth; important for Marsh-nodes      
!hol        real depth of last time step; important for Marsh-nodes      
!hdet       derivative of real depth over time; important for Marsh-nodes      
!hdot       derivative of real depth over time in last time step; important for Marsh-nodes      
!h2ol       real depth of penultimate time step; important for Marsh-nodes      
!wsll       water surface elevation of node      
!
!
!1D-node definition data (geometry approach)      
!-------------------------------------------      
      real (kind = 4), allocatable, dimension (:) :: width, wids
      real (kind = 4), allocatable, dimension (:) :: ss1, ss2
      real (kind = 4), allocatable, dimension (:) :: widbs, wss
!meaning of the variables      
!------------------------      
!width      Channel bed width of a 1D-node within the trapezoidal geometry approach      
!wids       storage width (flood plain width) of a 1D-node within the trapezoidal geometry approach      
!ss1        1st Channel side slope of a 1D-node within the trapezoidal geometry approach      
!ss2        2nd Channel side slope of a 1D-node within the trapezoidal geometry approach      
!widbs      lowest point of off-channel storage      
!wss        side slope of the off-channel storage      
!
!continuity line definition data      
!-------------------------------      
      integer (kind = 4) :: line (50, 3535)
      integer (kind = 4) :: lmt (50)
!
      type (contiLine), allocatable, target :: ccls (:)
!meaning of the variables      
!------------------------      
!line    continuity line definition; stores all the nodes       
!lmt     number of nodes within continuity line      
!
!dimension transition data      
!-------------------------      
      integer (kind = 4), allocatable, dimension (:,:) :: TransLines
      integer (kind = 4), allocatable, dimension (:)   :: jpoint
!meaning of the variables      
!------------------------      
!translines   data of transition lines; transitioning line, 1D-element, and node      
!jpoint       determines connections of nodes in 1D/2D and 2D/3D-vertical elements      
!
!pipe surface connections      
!------------------------      
!
      type PipeSurfaceConnection
        integer (kind = 4) :: SurfElt
        integer (kind = 4) :: pipeElt
        real (kind = 8) :: PipeFlow
        real (kind = 8) :: SurfFlow
        real (kind = 8) :: DflowWRTv_upper, DflowWRTv_lower
        real (kind = 8) :: DflowWRTh_upper, DflowWRTh_lower
        type (pipeManhole) :: manholeDef
      end type
!
      type (PipeSurfaceConnection), allocatable,                        &
     &                              dimension (:) :: PipeSurfConn
      integer (kind = 4), allocatable, dimension (:) :: ConnectedElt
!
      type (StorageElement), allocatable, target, dimension (:) ::      &
     &  StorageElts     
!
!input control parameters - elevations      
!-------------------------------------      
      real (kind = 8) :: elev, hmin, hmnn
!meaning of the variables      
!------------------------      
!elev:   Initial elevation at not restarted simulation startup; defined in control.r10      
!hmin:   3 meanings      
!        positive value - minimum water depth at not restarted simulation startup, defined in control.r10      
!        zero value     - no minimum water depth applied at the beginning; if elev is less than ao at a certain node, the node becomes dry at startup      
!        negative value - all water depth values ar overwritten with the absolute of the negative hmin      
!hmnn:   Actually a working copy of hmin for internal purposes to identify the method, that should be applied for hmin      
!
!input control parameters - scaling      
!----------------------------------      
      real (kind = 8) :: xscale, yscale, zscale
!meaning of the variables      
!------------------------      
!xscale: scaling of all coordinates in x-direction by this factor      
!yscale: scaling of all coordinates in y-direction by this factor      
!zscale: scaling of all bed-coordinates in z-direction by this factor      
!
!global constituents (constants)      
!-------------------      
      real (kind = 8) :: roavg, tempi, grav
!meaning of the variables      
!------------------------      
!roavg:  average density of the fluid      
!tempi:  input temperature (from input file; is used here to determine the average density)      
!grav:   gravitational constant      
!
!general execution control      
!-------------------------      
      integer (kind = 4) :: igf
      integer (kind = 4) :: ndp
!meaning of the variables      
!------------------------      
!igf:    switch unit system      
!        1 - American units      
!        2 - SI units      
!ndp:    control for usage of layer definition; 3D application      
!        0 - no layers - no 3D      
!        1 - definition at suface nodes (line LD1)      
!       -1 - definition at corner nodes (line LD2)      
!        2 - definition at corner nodes (line LD3)      
!       -N - number of layers to be applied is N      
!
!simulation time parameter      
!-------------------------      
      integer (kind = 4) :: iyrr
      real (kind = 8)    :: tett, tet
!meaning of the variables      
!------------------------      
!iyrr       year      
!tett       date in hours      
!tet        hours of the current day      
!
!real time parameters      
!--------------------      
      real (kind = 8), dimension (10) :: ATIM
!meaning of the variables      
!------------------------      
!atim       array to store processing times for 1D, 2D and so on; it is used for the time-output file      
!
!
!iteration and time step control      
!-------------------------------      
      integer (kind = 4) :: niti, nitn, nita
      integer (kind = 4) :: maxn
      integer (kind = 4) :: itpas
      integer (kind = 4) :: iaccyc
      integer (kind = 4) :: icyc, ncyc
      integer (kind = 4) :: it
!meaning of the variables
!------------------------
!niti                   number of steady state iterations
!nitn                   number of unsteady iterations read for each time step
!nita                   local copy of current maximum number of iterations in steady or unsteady run
!maxn                   current iteration cycle
!itpas                  ???
!iaccyc: time step to start the calculation from; counting through the list of time steps to reach the iaccyc-th step
!icyc                   counter for really processed time steps
!ncyc                   maximum number of time steps
!it                     actual 'ID' of the time step
!
!
!file handling and outwriting frequencies      
!----------------------------------------      
      integer (kind = 4) :: ioutrwd
      integer (kind = 4) :: nprti, nprtf, nprtmetai
      integer (kind = 4) :: irsav, irMiniMaxiSav
!meaning of the variables
!------------------------
!ioutrwd                frequency to rewind output file LOUT
!nprtf                  frequency of time steps to write out full results
!nprti                  frequency of iterations within nprtf to write out full results
!irsav                  time step to start with writing of results
!irMiniMaxiSav          time step to start with writing of minimum and maximum results
!
!controlling of active degrees of freedom      
!----------------------------------------      
      integer (kind = 4), allocatable, dimension (:) :: iteqs, iteqv
      integer (kind = 4)                             :: ick
      integer (kind = 4)                             :: ndf
!meaning of the variables      
!------------------------      
!iteqs      active constituents in the iteration      
!iteqv      active equations in the iteration      
!           TODO: Examine meaning of ick      
!ick        ??? is connected to iteqs      
!ndf        active number of degrees of freedom      
!
!convergence and equation dropout      
!--------------------------------      
      integer (kind = 4)                               :: nconv
      integer (kind = 4), allocatable, dimension (:,:) :: iactv
!meaning of the variables      
!------------------------      
!nconv            switch, that says, whehther model is converged in the time step      
!                 0 - model is not converged      
!                 1 - model is converged up to the variables, that had to be calculated to the iteration maxn; there might be another iteration, that      
!                       has to be done later in the time step, where another variable is active and is not converged      
!                 2 - all variables of the time step are fully converged      
!                 TODO: Examine the usage of iactv      
!iactv            stores for each node and each degree of freedom a switch for being active      
!                 0 - ???      
!                 1 - ???      
!                10 - ???      
!
!time derivative purposes      
!------------------------      
      real (kind = 8) :: delt
      real (kind = 8) :: alpha
      real (kind = 8) :: ALTM
      real (kind = 8) :: alphasn
!meaning of the variables      
!------------------------      
!delt       time step length [hours]      
!alpha      factor for time centering (default 1.6) for hydrodynamic values      
!altm       = alpha/ delt      
!alphasn    factor for time centering for constituents (default 2.0)      
!
!boundary conditions and inflow values      
!-------------------------------------      
      real (kind = 8), allocatable, dimension (:) :: SIDFF
!meaning of the variables      
!------------------------      
!sidff      side inflow to an element      
!
!
!morpho purposes      
!---------------      
      integer (kind = 4) :: mxsedlay
!meaning of the variables      
!------------------------      
!mxsedlay         ??? - morpho purposes      
!
!3D purposes      
!-----------      
      integer (kind = 4), allocatable, dimension (:) :: nref
      integer (kind = 4), allocatable, dimension (:) :: ndep
      integer (kind = 4)                             :: itransit
      real (kind = 8), allocatable, dimension (:)    :: dfct
!meaning of the variables      
!------------------------      
!nref       reference node, that is the top/bottom (?) node of a node column      
!ndep       number of layers per nodes; that means depending nodes on original 2D-node      
!itransit   ??? - 3D purposes      
!dfct       ??? - 3D purposes      
!
!boundary condition purposes      
!---------------------------      
      integer (kind = 4), allocatable, dimension (:) :: nfix, nfix1
      real (kind = 8), allocatable, dimension (:,:)  :: spec
!meaning of the variables      
!------------------------      
!nfix       switch for boundary condition type      
!nfix1      switch for boundary conditions due to concentration calculations      
!spec       condition value      
!           1 - velocity      
!           2 - direction      
!           3 - water head      
!           4 - salinity [?]      
!           5 - temperature [K]      
!           6 - concentration [mg/l]; cohesive or non-cohesive sediment      
!           7 - empirical 'Geschiebeanteil' [mg/l]      
!
!for Control structures      
!----------------------      
      type(ControlStructure), allocatable, target :: contrStructures (:)
!
!viscosities for output purposes      
!-------------------------------      
      real (kind = 8), allocatable :: epsx_nn (:), epsz_nn (:)
      real (kind = 8), allocatable :: epsxz_nn(:), epszx_nn (:)
!
!residual output for visualiuzation in Microstation      
!--------------------------------------------------      
      real (kind = 8), allocatable :: fehler (:,:)
!
!-
      INTEGER NSZF                                                      &
     &        ,NCBC                                                     &
     &         ,NLP,NS                                                  &
     &          ,IVS,IR1MAX                                             &
     &          ,NPSAV,NESAV,ISALBC,ISTRT,IZB                           &
     &          ,IPASS1,IPASS2,IPASS3                                   &
     &          ,ibinrst,iyend,idye,nscrin,ISVS,NMETF                   &
     &          ,nrbd,iedsw,istch                                       &
     &          ,ioptim,IHGNN,IZERS                                     &
     &          ,NODETR                                                 &
     &          ,IDIFSW                                                 &
!removed: lnnam
!     +          ,IOV,IYKK,IDTM,TTEM,LNNAM,INOTR
     &          ,IOV,IYKK,IDTM,TTEM,INOTR                               &
     &          ,INEWBED,IDEBUG                                         &
     &          ,ICNSV,IAVEL,IREALLCT,MR1
!-
!
!nis,sep06: Adding switch for chosing approach of boundary condition transformation
      INTEGER          :: BSWIT
!NiS,apr:       former variables of the common.cfg-file in the KALYPSO-2D version move to this module because they
!               are used here, if those informations about material and so on are used:
      INTEGER           :: irk
      CHARACTER(LEN=120):: rk_zeile(100)
!-
!
!IPK MAY06 ASS IMASSOUT     
      REAL                                                              &
     &           OMEGA                                                  &
     &          ,TMAX,FACT                                              &
     &          ,POWER,PWERIN,UMIN,VMIN                                 &
     &          ,UINP,VINP,TSTART                                       &
     &          ,UNOM,SALI,SEDI                                         &
     &          ,hrend,teth                                             &
     &          ,drfact,prcnt,dmix                                      &
     &          ,tbfact,tbmin,TRANSIT                                   &
     &          ,ELEV1,TRELEV,TRFACT                                    &
     &          ,ZSTDEP                                                 &
!EFa may07, dispersion-coefficient for turbulence modell
     &          ,p_bottom
!
!
      REAL      CINT(100),CPOW(100),AJ(100),BJ(100),CJ(100)             &
     &         ,GAMJ(100),QD(100),DJ(100)
!ITLVL(90)
      INTEGER   JLIN(50),                                               &
!     +         ,IURVL(90),NCNV(7)
     &          NCNV(7)                                                 &
!Nis,sep06: adding lineimat(50,3535) for storage of material of line segments
     &         ,lineimat(50,3535), lineelement (50, 3535)
!
!-
!EFa jun07, changed dimension for autoconverge      
!+         ,IURVL(65),ITLVL(65)      
      INTEGER,ALLOCATABLE :: IURVL(:),ITLVL(:)
!-      
      REAL      STQ(50),STQA(50),STQE(50),STQC(50),STQT(50),ALN(50)     &
     &         ,CONV(7)
!
      INTEGER   NJT(100),NDUPJ(100),NDDNJ(100),NDFLJ(100)
!
!IPK MAR01 ADD FOR POWER STATION RECYCLING  THIS BLOCK IS ALSO IN "BLK10.COM"
!IPK MAR01  LINE ABOVE ADD FOR KEEPING TRACK OF AVERAGE CCLINE CONCENTRATIONS
      REAL      AVES(50),AVET(50),AVESD(50)                             &
     &          ,ADDTMP(50,3),ADDSAL(50),ADDSED(50)                     &
     &          ,ADDMAX(50)
      INTEGER   NADTYP(50),NOUTCC(50)
!
!
!     integer*2 IBN,NOPSS,NFIXHS
      INTEGER :: IBN, NOPSS, NFIXHS
!
!
!Nis,mar06      specialized declaration of ALFA, so that the input-reading with binary files don't cause an error
      REAL(KIND=4) :: ALFA
!-
!
!
      ALLOCATABLE CORDS(:,:),NBC(:,:),ITAB(:),                          &
     &              ALFA(:),                                            &
     &              NLOC(:)                                             &
     &             ,NSURF(:),NBTN(:),IBN(:)                             &
     &             ,FCTV(:),FCTS(:),NTHREE(:)                           &
     &             ,FC(:,:)                                             &
     &             ,XSLP(:),YSLP(:),NFIXK(:)                            &
     &             ,NFIXP(:),ADIF(:)                                    &
     &             ,VOUTN(:)                                            &
     &             ,DIR(:)                                              &
     &             ,PRESS(:),NODC(:),NSTRT(:,:)                         &
     &           ,NDROP(:,:),ICPON(:),NSPL(:)                           &
     &           ,DROXIN(:),DRODXN(:),DROYIN(:),DRODYN(:)               &
     &           ,irbd(:),nfixsav(:)                                    &
     &           ,SIDN(:),XTLN(:)                                       &
     &           ,ICOLLAP(:)                                            &
     &           ,NBCKP(:,:),NFIXSV1(:)                                 &
     &           ,WAT(:,:),NODWT(:,:)                                   &
     &           ,GAN(:),GAN0(:)                                        &
     &           ,DGN(:),IBNA(:),XNU(:),XNMANN(:),ISTLIN(:)             &
     &             ,VSCALE(:),DDHDX(:),DDHDY(:),DDAODX(:),DDAODY(:)     &
     &            ,DIVID(:),XVEL(:,:),LAB(:)                            &
     &            ,EVISXZ(:),EVISYZ(:),DVISZ(:)                         &
     &            ,UDST(:),VDST(:),UUDST(:),UVDST(:)                    &
     &            ,VVDST(:),SDST(:),TDST(:),SEDST(:),IMID(:,:),IPASST(:)&
     &            ,IOVLDN(:)                                            &
!EFa jul07, added istab for stage-flow boundaries (table)
     &            ,istab(:)
!ipk sep06 add IOVLDN
!ipk apr06 add IPASST
!-
!
!
      REAL MANMIN,MANMAX,MANTAB
!
      ALLOCATABLE  AREA(:),IMATL(:)                                     &
     &             ,LNO(:),SIDF(:),SIDQ(:,:)                            &
     &             ,EDD1(:),EDD2(:),EDD3(:),nelord(:)                   &
     &             ,DROXINS(:,:),DROYINS(:,:)                           &
     &             ,ELAREA(:),neref(:),nedep(:),TVOLC(:)                &
     &             ,TVOL(:),ICOLLAPE(:),NRELSF(:)                       &
     &             ,NOPSS(:,:),NFIXHS(:),EINX(:),EINY(:)                &
     &             ,IEDSW1(:),TBFACT1(:),TBMIN1(:)                      &
     &             ,INOFLOW(:)                                          &
     &             ,NCTREF(:),NTMREF(:),NREORD(:),EEXXYY(:,:)           &
     &             ,IGTP(:),igtcl(:)                                    &
     &             ,ELMMIN(:),MANMIN(:),ELMMAX(:),MANMAX(:)             &
     &             ,DRAGX(:),DRAGY(:),IMAN(:),HMAN(:,:)                 &
     &             ,MANTAB(:,:,:)
!
!NCTREF :: reference number of the weir type; especially control structures in 2D can consist of      
!          more than one element with identical material type; material type is referencing to      
!          weir data in tabular data file      
!
!
!     LAYER VARIABLE
      ALLOCATABLE  THL(:),THLAY(:,:)
!
!NiS,apr06: for data exchange with kalypso-reading and writing modules
!      REAL(KIND=8), ALLOCATABLE :: fehler(:,:),
!-
!
!nis,nov06: Adding arrays
!The scaling factors for vx, vy and h, that comes into the global matrix      
      REAL, ALLOCATABLE, DIMENSION (:,:)   :: EqScale
!-
!nis,dec06: Connection node of the coupling elements      
!INTEGER, ALLOCATABLE                 :: ConnectNode (:)      
!-      
!
!nis,jan07: The velocities at the transition have to be stored temporarily; allocation takes place at the calling of the qgen-subroutine with      
!           BSWIT == 3; the size is then exactly of the transition size; it is deallocated directly after everery usage      
      REAL, ALLOCATABLE, DIMENSION (:)   :: TransitionVels
      REAL, ALLOCATABLE, DIMENSION (:)   :: TransSpec
!-      
!
!testing variables      
      REAL (KIND = 8), allocatable   :: matrix(:,:), vector(:)
!
!EFa sept08, switch for the output of waterstage and discharge at all continuity lines      
!CCLout = 1 output of waterstage and discharge      
!       = 0 no output      
!input of CCLout in line 'SCL' of control.r10 in input.for      
      INTEGER                        :: CCLout
!-            
!
      END MODULE
!
