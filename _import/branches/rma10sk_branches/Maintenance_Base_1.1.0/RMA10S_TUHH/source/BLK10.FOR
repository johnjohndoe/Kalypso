      MODULE BLK10 

      INCLUDE 'PARAM.COM'
      REAL(kind = 8) :: R1,RKEEP
      INTEGER (kind = 2) :: IPOINT,LCS,LPS
      integer (kind = 4) :: irwept
      
      ALLOCATABLE   LCS(:),LPS(:),IPOINT(:)
      ALLOCATABLE   R1(:),NLSTEL(:),RKEEP(:),
     +              EKEEP(:),RKEEPEQ(:),LCPOINT(:,:),EQQ(:,:)

	ALLOCATABLE   LHS(:),QS(:),LDEST(:),QR(:)
	
	ALLOCATABLE   RR(:),QS1(:),IRWEPT(:)
            
      ALLOCATABLE   EQ(:,:),LHED(:),QQ(:),PVKOL(:)

      INTEGER    LBMAX,LQ,IVRSID,MAXFIL,MFW

cWP Feb 2006
      INTEGER    NBS
c-
      
      ALLOCATABLE NLSTLL(:)
      



      END MODULE 