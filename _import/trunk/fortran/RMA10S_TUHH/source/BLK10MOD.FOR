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
     +          ,ZSTDEP

      REAL      CINT(100),CPOW(100),AJ(100),BJ(100),CJ(100)
     +         ,GAMJ(100),QD(100),DJ(100)
      INTEGER   LINE(50,350),LMT(50),JLIN(50),ITLVL(65),ITEQV(65)
     +         ,ITEQS(65),IURVL(65),NCNV(7)
      REAL      STQ(50),STQA(50),STQE(50),STQC(50),STQT(50),ALN(50)
     +         ,CONV(7)
     
      INTEGER   NJT(100),NDUPJ(100),NDDNJ(100),NDFLJ(100)

      REAL      AVES(50),AVET(50),AVESD(50)
     +          ,ADDTMP(50,3),ADDSAL(50),ADDSED(50)
     +          ,ADDMAX(50)

      INTEGER   NADTYP(50),NOUTCC(50)


      INTEGER*2 IMAT,NCORN,NCRN
     1         ,NETYP,NDEP,IBN,NOPSS,NFIXHS

cipk may02
      real*8 vel,ao,cord,AORIG

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
     +           ,iactv(:,:),HEL(:),HOL(:),HDET(:)
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
cipk apr06 add IPASST
C-



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
    
C     LAYER VARIABLE
      ALLOCATABLE  THL(:),THLAY(:,:)

      END MODULE

