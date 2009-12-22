C     Last change:  MD    5 Oct 2009    6:17 pm
      MODULE BLKSEDMOD

!MD:    FACT_SPEED_UP = Speed-up factor for bed evolution as a multipilier
!MD:                    for each DELT (timestep) and change of suspended and bed layers
      REAL(KIND = 8):: FACT_SPEED_UP


      INTEGER IMTREF(100)

c-    DEPRAT = array of deposition rates
c-    DEPRATO= array of old deposition rates
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


      REAL (KIND = 8)::  RKS,VK,WEGT
      INTEGER ISVL
      REAL (KIND = 8), ALLOCATABLE, DIMENSION (:) :: EROSTM

      REAL (KIND = 8):: DEPRAT,BSHEAR,VS,EDOT,
     +             UST,SERAT,ESRO,DEPRATO
      ALLOCATABLE  DEPRAT(:),BSHEAR(:),VS(:),EDOT(:),
     +             UST(:),SERAT(:),ESRO(:),DEPRATO(:)
cipk sep06 add deprato


! ------------------------------------------------------------
!MD: Variables for Deposition
c-
c-    CRCON1 & CRC_1 = constant in settling rate equation
c-    VSS1   & VS_1  = constant in settling rate equation
c-    CRCON2 & CRC_2 = constant in settling rate equation
c-    EXP2   & EX_2  = constant in settling rate equation
c-    VSK    & VS_K  = constant in settling rate equation
c-    VSS2   & VS_2  = constant in settling rate equation
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

      REAL (KIND = 8):: TAUCD,GAW,GAB,VSST,ERC,UN,GAC

      REAL (KIND = 8):: CRCON1,VSS1,CRCON2,EXP2,VSK,VSS2
      ALLOCATABLE CRCON1(:),VSS1(:),CRCON2(:),EXP2(:),VSK(:),VSS2(:)
                  ! Values per Node

      REAL (KIND = 8):: CRC_1,VS_1,CRC_2,EX_2,VS_K,VS_2
      ALLOCATABLE CRC_1(:),VS_1(:),CRC_2(:),EX_2(:),VS_K(:),VS_2(:)
                  ! Values per Roughness-Class


! ------------------------------------------------------------
!MD: Variables for Bed- and suspended Layers
      INTEGER NLAYT,LSS
      INTEGER NLAYTND
      REAL (KIND = 8):: GAWND,UNND,GABND,TTLAYND,GACND,WEGTND,
     +           ERCND,SSND,GBND,TLAYND,GADND,TAUCDND,EROST
      ALLOCATABLE NLAYTND(:),GAWND(:),UNND(:),GABND(:),TTLAYND(:)
     +           ,GACND(:),WEGTND(:),ERCND(:),SSND(:,:),GBND(:,:)
     +           ,TLAYND(:,:),GADND(:,:),TAUCDND(:),EROST(:,:)

      REAL (KIND = 8):: TAUCDM,GAWM,GABM,ERCM,UNM,GACM
      INTEGER NLAYTM

      REAL (KIND = 8):: GBM,SSM,GADM,TLAYM
      ALLOCATABLE GBM(:),SSM(:),GADM(:),TLAYM(:)

      REAL (KIND = 8):: TLAY,GAD,SS,GB
      ALLOCATABLE TLAY(:),GAD(:),SS(:), GB(:)


c-    NLAYO  = Number of Layers for Bed-Layer
c-    SSTO   = Critical Erosion-Stress for Bed-Layer
c-    SMVAL  = Erosion-Rate Factor for Bed-Layer
c-    GBO    = wet bulkdensity for Bed-Layer
c-    THICKOND = maximal Thickness of Layers for Bed-Layer
c-    THICKO = actual Thickness of Layers for Bed-Layer
c-    NLAY   = Number of Layers
c-    BEDEL  =
c-    THICK  = actual Thickness of suspended Layers
c-    SST    = Critical Erosion-Stress
c-    TM     = Mass in suspended Layers
c-    TMD    = Sediment Mass
C     BEDORIG=  original bed thickness
C     DEPINCR=  increment of depth over simulation

      INTEGER NLAYO, NLAY
      REAL (KIND = 8):: SSTO,SMVAL,GBO,THICKO,THICKOND,BEDEL,THICK,
     +            SST,TM,TMD,BEDORIG,DEPINCR,BEDELO
      ALLOCATABLE NLAYO(:),SSTO(:,:),SMVAL(:,:),
     +            GBO(:,:),THICKO(:,:),THICKOND(:,:),NLAY(:),BEDEL(:),
     +            THICK(:,:),SST(:,:),TM(:),TMD(:),
     +            BEDORIG(:),DEPINCR(:),BEDELO(:)

CIPK MAY06 and sep06
      REAL (KIND = 8):: TMSED,TMSSED,TMSADD,TMASSLEFT,TMSDEP
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