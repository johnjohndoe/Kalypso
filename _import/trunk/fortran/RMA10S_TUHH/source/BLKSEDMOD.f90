!     Last change:  MD    5 Oct 2009    6:17 pm
      MODULE BLKSEDMOD
!
!MD:    FACT_SPEED_UP = Speed-up factor for bed evolution as a multipilier
!MD:                    for each DELT (timestep) and change of suspended and bed layers
      REAL(KIND = 8):: FACT_SPEED_UP
!
!
      INTEGER IMTREF(100)
!
!-    DEPRAT = array of deposition rates
!-    DEPRATO= array of old deposition rates
!-    BSHEAR = array of bed shears
!-    VS     = array of settling rates
!-    EDOT   = array of erosion rates
!-    UST    = array of u-star
!-    SERAT  = array of net settling rates
!-    RKS    = bed roughness height
!-    VK     = Von karman's constant
!-    WEGT   = weight of clay layer
!-    ISVL   = switch for settling velocity type
!-    ESRO   = array of previous bed shears
!
!
      REAL (KIND = 8)::  RKS,VK,WEGT
      INTEGER ISVL
      REAL (KIND = 8), ALLOCATABLE, DIMENSION (:) :: EROSTM
!
      REAL (KIND = 8):: DEPRAT,BSHEAR,VS,EDOT,                          &
     &             UST,SERAT,ESRO,DEPRATO
      ALLOCATABLE  DEPRAT(:),BSHEAR(:),VS(:),EDOT(:),                   &
     &             UST(:),SERAT(:),ESRO(:),DEPRATO(:)
!ipk sep06 add deprato
!
!
! ------------------------------------------------------------
!MD: Variables for Deposition
!-
!-    CRCON1 & CRC_1 = constant in settling rate equation
!-    VSS1   & VS_1  = constant in settling rate equation
!-    CRCON2 & CRC_2 = constant in settling rate equation
!-    EXP2   & EX_2  = constant in settling rate equation
!-    VSK    & VS_K  = constant in settling rate equation
!-    VSS2   & VS_2  = constant in settling rate equation
!-    NLAYT  = number of clay layers
!-    TAUCD  = critical shear stress
!-    GAW    = density of clay suspension 
!-    GAB    = bulk density of top clay layer
!-    TLAY   = thickness of clay layer
!-    GAC    = dry weight of clay in top layer
!-    GAD    = array of dry density of clay layers
!-    SS     = array of critical shear stresses of clay layers
!-    GB     = array of bulk density of clay layers
!-    ERC    = erosion rate constant
!-    UN     = kinematic viscosity of susp water 
!-    VSST   = settling velocity
!
      REAL (KIND = 8):: TAUCD,GAW,GAB,VSST,ERC,UN,GAC
!
      REAL (KIND = 8):: CRCON1,VSS1,CRCON2,EXP2,VSK,VSS2
      ALLOCATABLE CRCON1(:),VSS1(:),CRCON2(:),EXP2(:),VSK(:),VSS2(:)
! Values per Node                  
!
      REAL (KIND = 8):: CRC_1,VS_1,CRC_2,EX_2,VS_K,VS_2
      ALLOCATABLE CRC_1(:),VS_1(:),CRC_2(:),EX_2(:),VS_K(:),VS_2(:)
! Values per Roughness-Class                  
!
!
! ------------------------------------------------------------
!MD: Variables for Bed- and suspended Layers
      INTEGER NLAYT,LSS
      INTEGER NLAYTND
      REAL (KIND = 8):: GAWND,UNND,GABND,TTLAYND,GACND,WEGTND,          &
     &           ERCND,SSND,GBND,TLAYND,GADND,TAUCDND,EROST
      ALLOCATABLE NLAYTND(:),GAWND(:),UNND(:),GABND(:),TTLAYND(:)       &
     &           ,GACND(:),WEGTND(:),ERCND(:),SSND(:,:),GBND(:,:)       &
     &           ,TLAYND(:,:),GADND(:,:),TAUCDND(:),EROST(:,:)
!
      REAL (KIND = 8):: TAUCDM,GAWM,GABM,ERCM,UNM,GACM
      INTEGER NLAYTM
!
      REAL (KIND = 8):: GBM,SSM,GADM,TLAYM
      ALLOCATABLE GBM(:),SSM(:),GADM(:),TLAYM(:)
!
      REAL (KIND = 8):: TLAY,GAD,SS,GB
      ALLOCATABLE TLAY(:),GAD(:),SS(:), GB(:)
!
!
!-    NLAYO  = Number of Layers for Bed-Layer
!-    SSTO   = Critical Erosion-Stress for Bed-Layer
!-    SMVAL  = Erosion-Rate Factor for Bed-Layer
!-    GBO    = wet bulkdensity for Bed-Layer
!-    THICKOND = maximal Thickness of Layers for Bed-Layer
!-    THICKO = actual Thickness of Layers for Bed-Layer
!-    NLAY   = Number of Layers
!-    BEDEL  =
!-    THICK  = actual Thickness of suspended Layers
!-    SST    = Critical Erosion-Stress
!-    TM     = Mass in suspended Layers
!-    TMD    = Sediment Mass
!     BEDORIG=  original bed thickness
!     DEPINCR=  increment of depth over simulation
!
      INTEGER NLAYO, NLAY
      REAL (KIND = 8):: SSTO,SMVAL,GBO,THICKO,THICKOND,BEDEL,THICK,     &
     &            SST,TM,TMD,BEDORIG,DEPINCR,BEDELO
      ALLOCATABLE NLAYO(:),SSTO(:,:),SMVAL(:,:),                        &
     &            GBO(:,:),THICKO(:,:),THICKOND(:,:),NLAY(:),BEDEL(:),  &
     &            THICK(:,:),SST(:,:),TM(:),TMD(:),                     &
     &            BEDORIG(:),DEPINCR(:),BEDELO(:)
!
!IPK MAY06 and sep06
      REAL (KIND = 8):: TMSED,TMSSED,TMSADD,TMASSLEFT,TMSDEP
      ALLOCATABLE TMSED(:),TMSSED(:),TMSADD(:),TMASSLEFT(:)
      ALLOCATABLE SSTOM(:),SMVALM(:),GBOM(:),THICKOM(:),TMSDEP(:)
!
!
!
!      COMMON/SED/DEPRAT(MNP),BSHEAR(MNP),VS(MNP),
!     1EDOT(MNP),UST(MNP),SERAT(MNP),CMAX,RKS,VK,BRKS,WEGT,ISVL
!     +,ESRO(MNP)
!
!      COMMON /PROPS/ CRCON1,VSS1,CRCON2,EXP2,VSK,VSS2,NLAYT,TAUCD,GAW
!     1         ,GAB,TLAY(MLAYER),GAC,GAD(MLAYER),SS(MLAYER),
!     +          GB(MLAYER),ERC,UN,BNKCD,BNHGAB,BNKERC,VSST,LSS
!
!      COMMON /INBED/ NLAYO(MNP),SSTO(MNP,MLAYER),SMVAL(MNP,MLAYER),
!     +   GBO(MNP,MLAYER),THICKO(MNP,MLAYER),NLAY(MNP),BEDEL(MNP),
!     +   THICK(MNP,MLAYER),SST(MNP,MLAYER),TM(MNP),TMD(MNP) 
!     +  ,BEDORIG(MNP),DEPINCR(MNP)
!
      END MODULE