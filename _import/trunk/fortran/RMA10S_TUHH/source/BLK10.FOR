C     Last change:  WP   23 Feb 2006   12:11 pm
      MODULE BLK10 

      REAL*8 R1,RKEEP
      INTEGER*2 IPOINT,LCS,LPS
      
      ALLOCATABLE   LCS(:),LPS(:),IPOINT(:)
      ALLOCATABLE   R1(:),NLSTEL(:),RKEEP(:),
     +              EKEEP(:),RKEEPEQ(:)

	ALLOCATABLE   LHS(:),QS(:),LDEST(:),QR(:)
            
      ALLOCATABLE   EQ(:,:),LHED(:),QQ(:),PVKOL(:)

      INTEGER    LBMAX,LQ,IVRSID,MAXFIL,MFW

cWP Feb 2006
      INTEGER    NBS
c-
      



      END MODULE 