      MODULE BLK10 
!
! MLAYER = Maximum number of layers in sediment
      PARAMETER( MNP=26000,MEL=10000,MEQ=50000,MLAY= 17000,             &
     &     NLAYM=   1,MLAYER=5)
!
!     +     NCQOBS = 2500,
!     +     NHDS =5  ,NCHOBS = 3500, NELDS = 40  ,NDPTS = 2600)
!
      PARAMETER( MMAT = 20, MELSIZ=10000)
      REAL(kind = 8) :: R1,RKEEP
      INTEGER (kind = 2) :: IPOINT,LCS,LPS
      integer (kind = 4) :: irwept
!
      ALLOCATABLE   LCS(:),LPS(:),IPOINT(:)
      ALLOCATABLE   R1(:),NLSTEL(:),RKEEP(:),                           &
     &              EKEEP(:),RKEEPEQ(:),LCPOINT(:,:),EQQ(:,:)
!
      ALLOCATABLE   LHS(:),QS(:),LDEST(:),QR(:)
!
      ALLOCATABLE   RR(:),QS1(:),IRWEPT(:)
!
      ALLOCATABLE   EQ(:,:),LHED(:),QQ(:),PVKOL(:)
!
      INTEGER    LBMAX,LQ,IVRSID,MAXFIL,MFW
!
!WP Feb 2006
      INTEGER    NBS
!-
!
      ALLOCATABLE NLSTLL(:)
!
!
!
!
      END MODULE 