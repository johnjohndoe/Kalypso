      MODULE BLKSEDMOD


      INTEGER IMTREF(100)

c-    DEPRAT = array of deposition rates
c-    BSHEAR = array of bed shears
c-    VS     = array of settling rates
c-    EDOT   = array of erosion rates
c-    UST    = array of u-star
c-    SERAT  = array of net settling rates
c-    RKS    = bed roughness height
c-    VK     = Von karman's constant
c-    WEGT   = weight of clay layer
c-    ISVL   = switch for settling velocity type
c-    ESRO   = array of previous bed shears


      REAL  RKS,VK,WEGT
      INTEGER ISVL

      ALLOCATABLE  DEPRAT(:),BSHEAR(:),VS(:),
     +             EDOT(:),UST(:),SERAT(:)
     +            ,ESRO(:),DEPRATO(:)
cipk sep06 add deprato

c-
c-    CRCON1 = constant in settling rate equation
c-    VSS1   = constant in settling rate equation
c-    CRCON2 = constant in settling rate equation
c-    EXP2   = constant in settling rate equation
c-    VSK    = constant in settling rate equation
c-    VSS2   = constant in settling rate equation
c-    NLAYT  = number of clay layers
c-    TAUCD  = critical shear stress
c-    GAW    = density of clay suspension 
c-    GAB    = bulk density of top clay layer
c-    TLAY   = thickness of clay layer
c-    GAC    = dry weight of clay in top layer
c-    GAD    = array of dry density of clay layers
c-    SS     = array of critical shear stresses of clay layers
c-    GB     = array of bulk density of clay layers
c-    ERC    = erosion rate constant
c-    UN     = kinematic viscosity of susp water 
c-    VSST   = settling velocity

      REAL CRCON1,VSS1,CRCON2,EXP2,VSK,VSS2,TAUCD,GAW
     1         ,GAB,VSST,ERC,UN,GAC
      INTEGER NLAYT,LSS

      ALLOCATABLE NLAYTND(:),GAWND(:),UNND(:),GABND(:),TTLAYND(:)
     +           ,GACND(:),WEGTND(:),ERCND(:),SSND(:,:),GBND(:,:)
     +           ,TLAYND(:,:),GADND(:,:),TAUCDND(:),EROST(:,:)

      REAL TAUCDM,GAWM,GABM,ERCM,UNM,GACM
      INTEGER NLAYTM

      ALLOCATABLE GBM(:),SSM(:),GADM(:),TLAYM(:),EROSTM(:)

      ALLOCATABLE  TLAY(:),GAD(:),SS(:), GB(:)


c-    NLAYO  =
c-    SSTO   =
c-    SMVAL  =
c-    GBO    =
c-    THICKO =
c-    NLAY   =
c-    BEDEL  =
c-    THICK  =
c-    SST    =
c-    TM     =
c-    TMD    =
C     BEDORIG=  original bed thickness
C     DEPINCR=  increment of depth over simulation

      ALLOCATABLE NLAYO(:),SSTO(:,:),SMVAL(:,:),
     +            GBO(:,:),THICKO(:,:),NLAY(:),BEDEL(:),
     +            THICK(:,:),SST(:,:),TM(:),TMD(:) 
     +           ,BEDORIG(:),DEPINCR(:),BEDELO(:)

CIPK MAY06 and sep06
      REAL*8 TMSED,TMSSED,TMSADD,TMASSLEFT,TMSDEP
      ALLOCATABLE TMSED(:),TMSSED(:),TMSADD(:),TMASSLEFT(:)
      ALLOCATABLE SSTOM(:),SMVALM(:),GBOM(:),THICKOM(:),TMSDEP(:)



C      COMMON/SED/DEPRAT(MNP),BSHEAR(MNP),VS(MNP),
C     1EDOT(MNP),UST(MNP),SERAT(MNP),CMAX,RKS,VK,BRKS,WEGT,ISVL
C     +,ESRO(MNP)

C      COMMON /PROPS/ CRCON1,VSS1,CRCON2,EXP2,VSK,VSS2,NLAYT,TAUCD,GAW
C     1         ,GAB,TLAY(MLAYER),GAC,GAD(MLAYER),SS(MLAYER),
C     +          GB(MLAYER),ERC,UN,BNKCD,BNHGAB,BNKERC,VSST,LSS

C      COMMON /INBED/ NLAYO(MNP),SSTO(MNP,MLAYER),SMVAL(MNP,MLAYER),
C     +   GBO(MNP,MLAYER),THICKO(MNP,MLAYER),NLAY(MNP),BEDEL(MNP),
C     +   THICK(MNP,MLAYER),SST(MNP,MLAYER),TM(MNP),TMD(MNP) 
C     +  ,BEDORIG(MNP),DEPINCR(MNP)

      END MODULE