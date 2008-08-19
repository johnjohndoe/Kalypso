C     Last change:  NIS  15 May 2008    8:11 pm
      MODULE BLK10MOD

CIPK AUG05      INCLUDE 'PARAM.COM'

      CHARACTER*8 ID
      CHARACTER*72 DLIN,TITLE
      CHARACTER*1000 HEADER
      CHARACTER*96 FNAM


C-
      INTEGER NP,NE,NB,NDF,NCN,NMAT,NSZF,IWIND,MAXN,IGF,NDP,LITR,
     1        MAXP,MAXE,NLL,NCL,NOPT,NITI,NITN,NCBC,ND1,MXSEDLAY
     2         ,IT,ICYC,NLP,NEM,NPM,NS,LE,LP
     3          ,IVS,NPRTF,IR1MAX,NPRTI
     4          ,ICFL,ICK,IFILE,IFIT,IFOT,NSCR,LIN,LOUT,NCYC,NAINPT 
     5          ,NPSAV,NESAV,ISALBC,ISTRT,NCONV,NIPT,NITA,IZB,IRSAV,IBUP
     6          ,IPASS1,IPASS2,IPASS3,IOPTZD,KRESTF
     7          ,ibinrst,iyend,idye,nscrin,IYRR,ISVS,NMETF
     9          ,idrpt,nrbd,iedsw,istch,IUTUB
     +          ,ioptim,IDNOPT,IHGNN,IZERS,ITRANSIT
     +          ,IRMAFM,IPROJ,NODETR,IOCON
     +          ,ISMSGN,ISMSFM,ISMSFM1,ITPAS,IBEDOT,IOUTFREQ,IOUTRST
     +          ,NBSFRQ,IWAVOT,ISMSFM2,IDIFSW
     +          ,INWGT,INSTR,INBNWGT,INBNSTR,IGEO,IOWGT,IOBNWGT,ICORDIN
     +          ,IOV,IYKK,IDTM,TTEM,LNNAM,IWVIN,IWVFC,INOTR,INCSTR
     +          ,INTIMS,INEWBED,IDEBUG,IMESHOUT,IOUTRWD,IMASSOUT
     +          ,ICNSV,IAVEL,ICPU,IREALLCT,MR1
     +          ,ITIMFL,ISOLCT
CIPK AUG07  ADD ICPU to ISOLCT
cipk aug06  add input options    
!NiS,mar06      add MAXA as dimensioner for arc-array, MaxT for number of 1D-2D-TRANSITION ELEMENTS
     +          ,MaxA, MaxT, MaxPolyA, MaxPolyQ, MaxPolyB
!-
!nis,feb07: Add FFFMS as counter for midside nodes of Flow1DFE elements
     +          ,FFFMS, AddMS, LPPoly
!-
!nis,nov06      add MaxLT as dimensioner for line-transitions
     +          ,MaxLT
!-

!NiS,apr06      add IKALYPSOFM and KALYPRST as unit specifier for output file and restart-in file in KALYPSO-2D-format in global use
     +          ,IKALYPSOFM, KALYPRST

!EFa, mar07     add MFWW for gloabl use (used in numbopt)
     +          ,MFWW

!-
!NiS,may06: Adding the character-variable ct. This is used for generation of output file names; the FNAM38 is deactivated again, because
!           the output file name will be generated automatically with the scheme of Kalyps-2D.
      CHARACTER(LEN=1) :: ct
      CHARACTER(LEN=32):: modellein, modellaus, modellrst
      INTEGER          :: iaccyc
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
     1           GRAV,ROAVG,OMEGA
     2          ,DELT,TMAX,ALTM,FACT,TET
     3          ,POWER,PWERIN,UMIN,VMIN
     4          ,ELEV
     6          ,UINP,VINP,TSTART
     7          ,UNOM,HMIN,HMNN,XSCALE,YSCALE,ZSCALE,TEMPI,SALI,SEDI
     8          ,hrend,teth
     9          ,alpha,alphasn,drfact,prcnt,dmix
     +          ,tbfact,tbmin,TRANSIT
     +          ,ELEV1,TRELEV,TRFACT
     +          ,ZSTDEP,ATIM(10)
!NiS,apr06: adding tett as global variable because it is used in different subroutines
     +          ,tett
!EFa may07, dispersion-coefficient for turbulence modell
     +          ,p_bottom


      REAL      CINT(100),CPOW(100),AJ(100),BJ(100),CJ(100)
     +         ,GAMJ(100),QD(100),DJ(100)
      INTEGER   LINE(50,350),LMT(50),JLIN(50), !ITLVL(90),ITEQV(90)
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


!      INTEGER*2 IMAT,NCORN,NCRN
!     1         ,NETYP,NDEP,IBN,NOPSS,NFIXHS
      INTEGER :: IMAT, NCORN, NCRN ,NETYP, NDEP, IBN, NOPSS, NFIXHS

cipk may02
      real*8 vel,ao,cord,AORIG

!Nis,mar06      specialized declaration of ALFA and wss, so that the input-reading with binary files don't cause an error
      REAL(KIND=4) :: ALFA,wss
!-

      ALLOCATABLE CORD(:,:),VEL(:,:),AO(:),AORIG(:)

      ALLOCATABLE CORDS(:,:),NBC(:,:),SPEC(:,:),ITAB(:),
     1              ALFA(:),VOLD(:,:),
     2              VDOT(:,:),
     3              NFIX(:),NLOC(:),VDOTO(:,:)
     4             ,NDEP(:),NSURF(:),NBTN(:),IBN(:),NFIX1(:)
     5             ,FCTV(:),FCTS(:),NREF(:),NTHREE(:)
     6             ,VVEL(:),FC(:,:)
     7             ,XSLP(:),YSLP(:),NFIXK(:)
     8             ,WIDTH(:),SS1(:),SS2(:),NFIXP(:),ADIF(:)
     +             ,ALFAK(:),VOUTN(:),WIDS(:)
     +             ,DIR(:)
     +             ,PRESS(:),DEN(:),NODC(:),NSTRT(:,:)
     +           ,NDROP(:,:),ICPON(:),NSPL(:)
     +           ,DROXIN(:),DRODXN(:),DROYIN(:),DRODYN(:)
     +           ,irbd(:),nfixsav(:)
     +           ,IESPC(:,:)
!NiS,jul06: For consistent data types declaration below
!     +           ,iactv(:,:),HEL(:),HOL(:),HDET(:)
     +           ,iactv(:,:),HOL(:),HDET(:)
!-
     +           ,HDOT(:)
     +           ,jpoint(:),SIDN(:),XTLN(:)
     +           ,widbs(:),wss(:),ICOLLAP(:),WSLL(:)
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

      ALLOCATABLE  NOP(:,:),IMAT(:),
     3              NFIXH(:),TH(:),AREA(:),NCORN(:),IMATL(:)
     7             ,NOPS(:,:),NCRN(:),LNO(:),SIDF(:),SIDQ(:,:)
     +             ,ZMANN(:),CHEZ(:),NETYP(:)
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
      !for the storage of line-couplings
      INTEGER, ALLOCATABLE                 :: TransLines (:,:)
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
      !-

      !EFa jul07, insfl for stage-flow boundaries
      INTEGER                        :: insfl!,iswtb
      !REAL (KIND = 8)                :: srfel,dfdh,ff
      !-


      END MODULE

