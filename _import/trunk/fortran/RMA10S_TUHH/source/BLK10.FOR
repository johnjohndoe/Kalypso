C     Last change:  K    19 Dec 2006    7:36 pm
      MODULE BLK10 

      REAL*8 R1,RKEEP
      INTEGER*2 IPOINT,LCS,LPS
      
      ALLOCATABLE   LCS(:),LPS(:),IPOINT(:)
      ALLOCATABLE   R1(:),NLSTEL(:),RKEEP(:),
     +              EKEEP(:),RKEEPEQ(:)

	ALLOCATABLE   LHS(:),QS(:),LDEST(:),QR(:)
      !nis,dec06: QS' type must be defined to integer
      INTEGER :: QS
      !-
            
      ALLOCATABLE   EQ(:,:),LHED(:),QQ(:),PVKOL(:)

      INTEGER    LBMAX,LQ,IVRSID,MAXFIL,MFW

cWP Feb 2006
      INTEGER    NBS
c-
      



      END MODULE 