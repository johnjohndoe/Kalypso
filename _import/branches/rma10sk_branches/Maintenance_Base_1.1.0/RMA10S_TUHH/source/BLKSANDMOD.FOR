C     Last change:  MD   29 Jul 2008   11:28 am
CIPK  LAST UPDATE MAR 05 2005 ADD OPTION TO ALLOW NODAL DEFINITION OF PROPS
CIPK  LAST UPDATE DEC 22 2005 ADD RHOBS
      MODULE BLKSANMOD
c
c     BLKSAND
c
      CHARACTER*8 CLABL,CUNIT 

C-
      real ALP1,ALP2,POSA,UWSA,SGSA,GSF,PPMMAX,CLDE,CLER,RHOBS
     +             ,RHOF,ACGR,AMANN
     +             ,D35,D50,D90,GPMAX,VSAND,FACTMORPH,RWMIN,RWFACT
      INTEGER LSAND,LBED,IGS,LGS,MTC,IFALL,ISMODE
     +              ,IEDROP(9)

      ALLOCATABLE
     +               CRSLOP(:),TRSLOP(:),srate(:),gpbsav(:)        
     +              ,RCAE(:),RWAE(:),RCAN(:),RWAN(:),TRRAT(:)
     +              ,CAI(:)

CIPK MAR05

      ALLOCATABLE
     +            SGSAND(:),CLDEND(:),CLERND(:),AMANNND(:),D35ND(:)
     +           ,D50ND(:),D90ND(:),GPMAXND(:),VSANDND(:),ISMODEND(:)
     +           ,SDND(:,:)
      REAL*8 ELEVB
      ALLOCATABLE ELEVB(:)
CIPK JUN06
      REAL*8 TMSAND,TMSSAND
      ALLOCATABLE TMSAND(:),TMSSAND(:)

      ALLOCATABLE 
     +              SD(:),GPB(:,:)
     +             ,TTHICK(:),ALPHA1(:),ALPHA2(:),IDONE(:)
     +             ,DELBED(:),GP(:)


      ALLOCATABLE      WAVEHT(:),PEAKPRD(:),WAVEDR(:)
     +                ,WAVEHT0(:),PEAKPRD0(:),WAVEDR0(:)
     +                ,WAVEHT1(:),PEAKPRD1(:),WAVEDR1(:)
     +                ,WVH0(:),WVA0(:),WVT0(:)
     +                ,WVH1(:),WVA1(:),WVT1(:)

      INTEGER ELTON
      ALLOCATABLE    TRIBAREA(:),ELTON(:,:),deltab(:),nsrc(:)
     +              ,IEDONE(:),NKEY1(:),EXTLD(:),trslp(:),EXTLDEL(:)   


	REAL (KIND = 8), ALLOCATABLE, dimension (:):: FFACT_EL    !Friction Factor for each element
	REAL (KIND = 8), ALLOCATABLE, dimension (:):: FFACT_KN    !Friction Factor for each node


C     +              ,CRSLOP(MEL),TRSLOP(MNP),srate(mnp),gpbsav(mnp)        
C     +              ,RCAE(:),RWAE(:),RCAN(MNP),RWAN(MNP),TRRAT(MNP)
C     +              ,CAI(MNP)

C      REAL*8 ELEVB
C      ALLOCATABLE ELEVB(MNP)

C      COMMON/SANDG/ 
C     +             ,SD(MNP),VSAND,GPB(MNP,MSAND)
C     +             ,TTHICK(MNP),ALPHA1(MNP),ALPHA2(MNP),IDONE(MNP)
C     +             ,DELBED(MNP),GP(MSAND)


C      COMMON /WAVEDAT/ WAVEHT(MNP),PEAKPRD(MNP),WAVEDR(MNP)
C     +                ,WAVEHT0(MNP),PEAKPRD0(MNP),WAVEDR0(MNP)
C     +                ,WAVEHT1(MNP),PEAKPRD1(MNP),WAVEDR1(MNP)
C     +                ,WVH0(MNP),WVA0(MNP),WVT0(MNP)
C     +                ,WVH1(MNP),WVA1(MNP),WVT1(MNP)

C      INTEGER ELTON
C      COMMON /SLUMP/ TRIBAREA(MNP),ELTON(MNP,12),deltab(mnp),nsrc(mnp)
C     +              ,IEDONE(MEL),NKEY(MNP),EXTLD(MNP),trslp(mnp)   

      END MODULE