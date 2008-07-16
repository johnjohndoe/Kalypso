      MODULE BLK10 

      REAL*8 R1,RKEEP
      INTEGER*2 IPOINT,LCS,LPS
      
      ALLOCATABLE   LCS(:),LPS(:),IPOINT(:)
      ALLOCATABLE   R1(:),NLSTEL(:),RKEEP(:),
     +              EKEEP(:),RKEEPEQ(:)

	ALLOCATABLE   LHS(:),QS(:),LDEST(:),QR(:)
            
      ALLOCATABLE   EQ(:,:),LHED(:),QQ(:),PVKOL(:)

      INTEGER    LBMAX,LQ,IVRSID,MAXFIL,MFW
      



      END MODULE 