      module BLK10MOD

      !some general strings
      !--------------------
      character (len = 72) :: title
      character (len = 1000) :: header
      !meaning of the variables
      !------------------------
      !title      title entry
      !header     title header

      !i/o file and unit definitions (up to date); for description look into file.sub
      !------------------------------------------------------------------------------
      integer (kind = 4) :: ICFL
      integer (kind = 4) :: LIN, LOUT, LITR, IMESOUT
      integer (kind = 4) :: IKALYPSOFM, IFILE, NB, ibup
      integer (kind = 4) :: INCSTR, insfl
      integer (kind = 4) :: IWVFC, INSTR, IOWGT, ICORDIN, IOCON, IWVIN
      integer (kind = 4) :: INTIMS, INWGT, IMASSOUT, ITIMFL
      !meaning of the variables
      !------------------------
      
      !            TODO: examine, how ICFL works
      !icfl:       unit number definition of the console
      !LIN         unit of control file (control.r10)
      !LOUT        after 1st call of file.sub unit of output file (.ech)
      !            after 2nd call of file.sub LOUT becomes the general output unit (.out-file)
      !LITR        unit of iteration file (.itr)
      !IMESOUT     unit of Message file (MESS.ech)
      !IKALYPSOFM  unit number of the results ouput files
      !IFILE       model geometry (in *.2d-format)
      !NB          restart input file (binary or ASCII)
      !ibup        external boundary condition file
      !INCSTR      control structure data
      !insfl       input control stage flow relationship file
      !IWVFC       input surface stress data file
      !INSTR       surface traction from external grid
      !IOWGT       Mesh weighting output
      !ICORDIN     external grid data
      !IOCON       continuity line hydrograph file
      !IWVIN       input wave data file
      !INTIMS      on/off controlling of constrol structure time series
      !INWGT       external inpolation weighting data file
      !IMASSOUT    ???
      !ITIMFL      processing time data
      !other units
      !-----------
      !unit definitions in other blocks
      !--------------------------------
      !integer (kind = 4) :: unitSWAN (BLK_SWANF)
      !integer (kind = 4) :: IIQ, IOMET (BLK11MOD)
      
      !obsolet unit definitions
      !------------------------
      !integer (kind = 4) :: NLL, NIPT, NOPT, NB, IFIT, IFOT
      !integer (kind = 4) :: NAINPT, IRMAFM, JBEDOT, ISMSFM, ISMSFM1
      !integer (kind = 4) :: ISMSGN, ISMSFM2, IMESHOUT, IWAVOT, IBEDOT, INBNWGT
      !integer (kind = 4) :: INBNSTR, IOBNWGT
      
      !execution parameters; initialised in the file opening process
      !--------------------
      integer (kind = 4) :: iutub, KRESTF
      !Meaning of the variables
      !------------------------
      !iutub : switch (0 or 1) - 0 means that certain turbulence options can not be executed.
      !                          1 means all turbulence options can be executed, i.e. after correct restart or after to iterations with constant eddies.
      !TODO: Meaning of krestf
      !krestf: switch (0 or 1) - 0 means not restarted and ???
      !                          1 means restarted or ???; the value is edited in the rma10.sub, too. The meaning is not clear. It is used in surcof.
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
      !outwriting frequency and starting point parametrs
      !-------------------------------------------------
      INTEGER          :: iaccyc
      !meaning of the variables
      !------------------------
      !iaccyc: time step to start the calculation from; counting through the list of time steps to reach the iaccyc-th step

      
      !model dimension parameters
      !--------------------------
      integer (kind = 4) :: maxP, maxE, maxA, maxT, maxLT, ncl
      integer (kind = 4) :: np, ne, lp, le
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
      !ncl:    number of continuity lines
      !ne         maximum element number
      !le         minimum element number
      !np         maximum node number
      !lp         minimum node number
      !maxPolyA:  maximum number of polynomials to describe A(h) at any node
      !maxPolyQ:  maximum number of polynomials to describe Q(h) at any node
      !maxPolyB:  maximum number of polynomials to describe alpha(h)/ beta(h) at any node
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
      
      
      !general node geometry definition data
      !-------------------------------------
      real (kind = 8), allocatable, dimension (:,:)  :: cord
      real (kind = 8), allocatable, dimension (:)    :: ao, aorig
      integer (kind = 4), allocatable, dimension (:) :: ndep
      !general node calculation value data definition
      !----------------------------------------------
      real (kind = 8), allocatable, dimension (:,:)  :: vel, vdot
      real (kind = 8), allocatable, dimension (:,:)  :: vold, vdoto
      real (kind = 8), allocatable, dimension (:)    :: alfak
      real (kind = 8), allocatable, dimension (:)    :: den
      !meaning of the variables
      !------------------------
      !cord       x-, y- and ?-coordinates of a node
      !ao         bed elevation of a node
      !aorig      original value of the bed elevation of a node; necessary for bed evolution calculations
      !ndep       number of layers per nodes; that means depending nodes on original 2D-node
      !vel        The degree of freedom field; it's the key array
      !vdot       derivative of the degrees of freedom
      !vold       The degree of freedom field from the last time step; i.e. storage of old results
      !vdoto      derivative of the degrees of freedom from the last time step; i.e. storage of old results
      !for all counts:
      !           1 - x-velocity
      !           2 - y-velocity
      !           3 - water depth
      !           4 - salinity
      !           5 - temperature
      !           6 - suspended sediment
                  !TODO: what is vel(7,x)
      !           7 - ??? perhaps water column potential by element
      !alfak      Fixed flow direction angle at a certain node; Value 0.0 zero means, that flow direction not restricted
                  !TODO: What actually is the 3rd coordinate? Is it the same as ao
      !den        density at each node
      
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
      

      !continuity line definition data
      !-------------------------------
      integer (kind = 4) :: line (50, 350)
      integer (kind = 4) :: lmt (50)
      !meaning of the variables
      !------------------------
      !line    continuity line definition; stores all the nodes 
      !lmt     number of nodes within continuity line

      !dimension transition data
      !-------------------------
      integer (kind = 4), allocatable, dimension (:,:) :: TransLines
      integer (kind = 4), allocatable, dimension (:)   :: jpoint
      !meaning of the variables
      !------------------------
      !translines   data of transition lines; transitioning line, 1D-element, and node
      !jpoint       determines connections of nodes in 1D/2D and 2D/3D-vertical elements
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
      !input control parameters - scaling
      !----------------------------------
      real (kind = 8) :: xscale, yscale, zscale
      !meaning of the variables
      !------------------------
      !xscale: scaling of all coordinates in x-direction by this factor
      !yscale: scaling of all coordinates in y-direction by this factor
      !zscale: scaling of all bed-coordinates in z-direction by this factor
      !global constituents
      !-------------------
      real (kind = 8) :: roavg, tempi, grav
      !meaning of the variables
      !------------------------
      !roavg:  average density of the fluid
      !tempi:  input temperature (from input file; is used here to determine the average density)
      !grav:   gravitational constant
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
      
      !time parameter
      !--------------
      integer (kind = 4) :: iyrr
      real (kind = 8)    :: tett
      !meaning of the variables
      !------------------------
      !iyrr       year
      !tett       date in hours
      

C-
      INTEGER NDF,NMAT,NSZF,IWIND,MAXN,
     1        NITI,NITN,NCBC,ND1,MXSEDLAY
     2         ,IT,ICYC,NLP,NEM,NPM,NS
     3          ,IVS,NPRTF,IR1MAX,NPRTI
     4          ,ICK,NSCR,NCYC 
     5          ,NPSAV,NESAV,ISALBC,ISTRT,NCONV,NITA,IZB,IRSAV
     6          ,IPASS1,IPASS2,IPASS3,IOPTZD
     7          ,ibinrst,iyend,idye,nscrin,ISVS,NMETF
     9          ,idrpt,nrbd,iedsw,istch
     +          ,ioptim,IDNOPT,IHGNN,IZERS,ITRANSIT
     +          ,IPROJ,NODETR
     +          ,ITPAS,IOUTFREQ,IOUTRST
     +          ,NBSFRQ,IDIFSW
!removed: lnnam
!     +          ,IOV,IYKK,IDTM,TTEM,LNNAM,INOTR
     +          ,IOV,IYKK,IDTM,TTEM,INOTR
     +          ,INEWBED,IDEBUG,IOUTRWD
     +          ,ICNSV,IAVEL,ICPU,IREALLCT,MR1
     +          ,ISOLCT
CIPK AUG07  ADD ICPU to ISOLCT
cipk aug06  add input options    

!EFa, mar07     add MFWW for gloabl use (used in numbopt)
     +          ,MFWW

!-
!-

!nis,sep06: Adding switch for chosing approach of boundary condition transformation
      INTEGER          :: BSWIT
      INTEGER          :: npr !steady state counter for iterations
!-

!NiS,apr:       former variables of the common.cfg-file in the KALYPSO-2D version move to this module because they
!               are used here, if those informations about material and so on are used:
      INTEGER           :: irk
      CHARACTER(LEN=120):: rk_zeile(100)
!-

CIPK MAY06 ASS IMASSOUT     
CIPK MAR06 ADD IOUTRWD     
      REAL    
     1           OMEGA
     2          ,DELT,TMAX,ALTM,FACT,TET
     3          ,POWER,PWERIN,UMIN,VMIN
     6          ,UINP,VINP,TSTART
     7          ,UNOM,SALI,SEDI
     8          ,hrend,teth
     9          ,alpha,alphasn,drfact,prcnt,dmix
     +          ,tbfact,tbmin,TRANSIT
     +          ,ELEV1,TRELEV,TRFACT
     +          ,ZSTDEP,ATIM(10)
!EFa may07, dispersion-coefficient for turbulence modell
     +          ,p_bottom


      REAL      CINT(100),CPOW(100),AJ(100),BJ(100),CJ(100)
     +         ,GAMJ(100),QD(100),DJ(100)
      INTEGER   JLIN(50), !ITLVL(90),ITEQV(90)
!     +         ,ITEQS(90),IURVL(90),NCNV(7)
     +          NCNV(7)
!Nis,sep06: adding lineimat(50,350) for storage of material of line segments
     +         ,lineimat(50,350), lineelement (50, 350)

!-
      !EFa jun07, changed dimension for autoconverge
      !+         ,ITEQS(65),IURVL(65),ITLVL(65),ITEQV(65)
      INTEGER,ALLOCATABLE :: ITEQS(:),IURVL(:),ITLVL(:),ITEQV(:)
      !-
      REAL      STQ(50),STQA(50),STQE(50),STQC(50),STQT(50),ALN(50)
     +         ,CONV(7)
     
      INTEGER   NJT(100),NDUPJ(100),NDDNJ(100),NDFLJ(100)

      REAL      AVES(50),AVET(50),AVESD(50)
     +          ,ADDTMP(50,3),ADDSAL(50),ADDSED(50)
     +          ,ADDMAX(50)

      INTEGER   NADTYP(50),NOUTCC(50)


!     integer*2 IBN,NOPSS,NFIXHS
      INTEGER :: IBN, NOPSS, NFIXHS


!Nis,mar06      specialized declaration of ALFA, so that the input-reading with binary files don't cause an error
      REAL(KIND=4) :: ALFA
!-


      ALLOCATABLE CORDS(:,:),NBC(:,:),SPEC(:,:),ITAB(:),
     1              ALFA(:),
     3              NFIX(:),NLOC(:)
     4             ,NSURF(:),NBTN(:),IBN(:),NFIX1(:)
     5             ,FCTV(:),FCTS(:),NREF(:),NTHREE(:)
     6             ,VVEL(:),FC(:,:)
     7             ,XSLP(:),YSLP(:),NFIXK(:)
     8             ,NFIXP(:),ADIF(:)
     +             ,VOUTN(:)
     +             ,DIR(:)
     +             ,PRESS(:),NODC(:),NSTRT(:,:)
     +           ,NDROP(:,:),ICPON(:),NSPL(:)
     +           ,DROXIN(:),DRODXN(:),DROYIN(:),DRODYN(:)
     +           ,irbd(:),nfixsav(:)
     +           ,IESPC(:,:)
!NiS,jul06: For consistent data types declaration below
!     +           ,iactv(:,:),HEL(:),HOL(:),HDET(:)
     +           ,iactv(:,:),HOL(:),HDET(:)
!-
     +           ,HDOT(:)
     +           ,SIDN(:),XTLN(:)
     +           ,ICOLLAP(:),WSLL(:)
     +           ,V2OL(:,:),H2OL(:),NBCKP(:,:),NFIXSV1(:)
     +           ,WAT(:,:),NODWT(:,:)
     +           ,GAN(:),GAN0(:)
     +           ,DGN(:),IBNA(:),XNU(:),XNMANN(:),ISTLIN(:)
     +             ,VSCALE(:),DDHDX(:),DDHDY(:),DDAODX(:),DDAODY(:)
     1            ,DIVID(:),XVEL(:,:),LAB(:)
     +            ,EVISXZ(:),EVISYZ(:),DVISZ(:)
     +            ,UDST(:),VDST(:),UUDST(:),UVDST(:)
     +            ,VVDST(:),SDST(:),TDST(:),SEDST(:),IMID(:,:),IPASST(:)
     +            ,IOVLDN(:)
!EFa jul07, added istab for stage-flow boundaries (table)
     +            ,istab(:)
cipk sep06 add IOVLDN
cipk apr06 add IPASST
C-

!NiS,jul06: Variable HEL has to be real,kind=8 because of calling problems for amf
      REAL(KIND=8), ALLOCATABLE :: HEL(:)
!-

      REAL MANMIN,MANMAX,MANTAB

      ALLOCATABLE  AREA(:),IMATL(:)
     7             ,LNO(:),SIDF(:),SIDQ(:,:)
     +             ,DFCT(:)
     +             ,EDD1(:),EDD2(:),EDD3(:),nelord(:)
     +             ,DROXINS(:,:),DROYINS(:,:)
     +             ,ELAREA(:),neref(:),nedep(:),TVOLC(:)
     +             ,TVOL(:),ICOLLAPE(:),NRELSF(:)
     +      ,SIDFF(:),NOPSS(:,:),NFIXHS(:),EINX(:),EINY(:)
     +             ,IEDSW1(:),TBFACT1(:),TBMIN1(:)
     +             ,INOFLOW(:)
     +             ,NCTREF(:),NTMREF(:),NREORD(:),EEXXYY(:,:)
     +             ,IGTP(:),igtcl(:)
     +             ,ELMMIN(:),MANMIN(:),ELMMAX(:),MANMAX(:) 
     +             ,DRAGX(:),DRAGY(:),IMAN(:),HMAN(:,:)
     +             ,MANTAB(:,:,:)
    
      !NCTREF :: reference number of the weir type; especially control structures in 2D can consist of
      !          more than one element with identical material type; material type is referencing to
      !          weir data in tabular data file


C     LAYER VARIABLE
      ALLOCATABLE  THL(:),THLAY(:,:)

!NiS,apr06: for data exchange with kalypso-reading and writing modules
!      REAL(KIND=8), ALLOCATABLE :: fehler(:,:),
!-

!nis,nov06: Adding arrays
      !The scaling factors for vx, vy and h, that comes into the global matrix
      REAL, ALLOCATABLE, DIMENSION (:,:)   :: EqScale
!-
      !nis,dec06: Connection node of the coupling elements
      !INTEGER, ALLOCATABLE                 :: ConnectNode (:)
      !-

      !nis,jan07: The velocities at the transition have to be stored temporarily; allocation takes place at the calling of the qgen-subroutine with
      !           BSWIT == 3; the size is then exactly of the transition size; it is deallocated directly after everery usage
      REAL, ALLOCATABLE, DIMENSION (:)   :: TransitionVels
      REAL, ALLOCATABLE, DIMENSION (:)   :: TransSpec
      !-

      !testing variables
      REAL (KIND = 8), allocatable   :: matrix(:,:), vector(:)

      END MODULE

