!     Last change:  MD    9 Jun 2009   12:03 pm
!IPK  LAST UPDATE MAR 05 2005 ADD OPTION TO ALLOW NODAL DEFINITION OF PROPS
!IPK  LAST UPDATE DEC 22 2005 ADD RHOBS
      MODULE BLKSANMOD
!
!     BLKSAND
!
      CHARACTER*8 CLABL,CUNIT 
!
!-
      REAL(KIND = 8):: ALP1,ALP2,POSA,UWSA,SGSA,GSF,PPMMAX,CLDE,CLER,   &
     &                 RHOBS,RHOF,ACGR,AMANN,D35,D50,D90,GPMAX,VSAND,   &
     &                 FACTMORPH,RWMIN,RWFACT
      INTEGER LSAND,LBED,IGS,LGS,MTC,IFALL,ISMODE
      INTEGER DROPMAX, IEDROP(1:85)
!
      REAL(KIND = 8):: CRSLOP,TRSLOP,srate,gpbsav
!MD: Add definitions for Variables ....                 
      ALLOCATABLE CRSLOP(:),TRSLOP(:),srate(:),gpbsav(:)                &
     &            ,RCAE(:),RWAE(:),RCAN(:),RWAN(:),TRRAT(:)             &
     &            ,CAI(:)
!
!IPK MAR05
      INTEGER ISMODEND
      REAL(KIND = 8):: SGSAND,CLDEND,CLERND,AMANNND,D35ND               &
     &           ,D50ND,D90ND,GPMAXND,VSANDND,SDND
      ALLOCATABLE SGSAND(:),CLDEND(:),CLERND(:),AMANNND(:),D35ND(:)     &
     &           ,D50ND(:),D90ND(:),GPMAXND(:),VSANDND(:),ISMODEND(:)   &
     &           ,SDND(:,:)
!
!IPK JUN06
      REAL(KIND = 8):: TMSAND,TMSSAND,ELEVB
      ALLOCATABLE TMSAND(:),TMSSAND(:), ELEVB(:)
!
      INTEGER IDONE
      REAL(KIND = 8):: SD,GPB,TTHICK,ALPHA1,ALPHA2,DELBED,GP
      ALLOCATABLE SD(:),GPB(:,:),TTHICK(:),ALPHA1(:),ALPHA2(:),         &
     &            DELBED(:),GP(:),IDONE(:)
!
!
      ALLOCATABLE  WAVEHT(:),PEAKPRD(:),WAVEDR(:)                       &
     &            ,WAVEHT0(:),PEAKPRD0(:),WAVEDR0(:)                    &
     &            ,WAVEHT1(:),PEAKPRD1(:),WAVEDR1(:)                    &
     &            ,WVH0(:),WVA0(:),WVT0(:)                              &
     &            ,WVH1(:),WVA1(:),WVT1(:)
!
      INTEGER ELTON, IEDONE
      REAL(KIND = 8):: TRIBAREA,EXTLD,EXTLDEL
      ALLOCATABLE TRIBAREA(:),ELTON(:,:),deltab(:),nsrc(:)              &
     &           ,IEDONE(:),NKEY1(:),EXTLD(:),trslp(:),EXTLDEL(:)
!
!Friction Factor for each element
      REAL (KIND = 8), ALLOCATABLE, dimension (:):: FFACT_EL    
!Friction Factor for each node
      REAL (KIND = 8), ALLOCATABLE, dimension (:):: FFACT_KN    
! HN Geothechnical properties of sand      
! critical_slope    ::   Unsaturated critical slope of consolidated sand      
! crepose           ::   Saturated critical slope of consolidated sand       
! repose            ::   Angle of sand after avalanche.      
! EFFECTIVE_COHESION::   Effective cohesion due to consolidation and mositure content.      
! FRICTION_ANGLE    ::   Friction angle of sand      
! MATRIC_ANGLE      ::   Angle of matric suction (negative pore pressure)      
! ROOT              ::   Extra cohesion due to roots of vegetation      
! Refine            :: a switch to determine if a refined node adoptation for tensile failure is wished(1), not wished (0)      
      REAl           :: critical_slope,crepose,repose,EFFECTIVE_COHESION&
     &     ,FRICTION_ANGLE, MATRIC_ANGLE, ROOT
! HN Pore Pressure Distribution parameters.      
! EXPO3 to 1        :: These three vaiables define the pore pressure distribution function      
!                      in the river bank as follows: (EXPO3* h**3 + EXPO2 *h**2 + EXPO1 * h)      
!                      h is elevation over water table.                              
      REAL (kind = 8):: EXPO3,EXPO2,EXPO1
      INTEGER        :: refine
!     +              ,CRSLOP(MEL),TRSLOP(MNP),srate(mnp),gpbsav(mnp)        
!     +              ,RCAE(:),RWAE(:),RCAN(MNP),RWAN(MNP),TRRAT(MNP)
!     +              ,CAI(MNP)
!
!      REAL*8 ELEVB
!      ALLOCATABLE ELEVB(MNP)
!
!      COMMON/SANDG/ 
!     +             ,SD(MNP),VSAND,GPB(MNP,MSAND)
!     +             ,TTHICK(MNP),ALPHA1(MNP),ALPHA2(MNP),IDONE(MNP)
!     +             ,DELBED(MNP),GP(MSAND)
!
!
!      COMMON /WAVEDAT/ WAVEHT(MNP),PEAKPRD(MNP),WAVEDR(MNP)
!     +                ,WAVEHT0(MNP),PEAKPRD0(MNP),WAVEDR0(MNP)
!     +                ,WAVEHT1(MNP),PEAKPRD1(MNP),WAVEDR1(MNP)
!     +                ,WVH0(MNP),WVA0(MNP),WVT0(MNP)
!     +                ,WVH1(MNP),WVA1(MNP),WVT1(MNP)
!
!      INTEGER ELTON
!      COMMON /SLUMP/ TRIBAREA(MNP),ELTON(MNP,12),deltab(mnp),nsrc(mnp)
!     +              ,IEDONE(MEL),NKEY(MNP),EXTLD(MNP),trslp(mnp)   
!
      END MODULE