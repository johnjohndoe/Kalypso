      MODULE BLKSSTMOD
c
c     BLKSST
c

      ALLOCATABLE      STRESS(:,:),SPWP(:),AWL(:),SWH(:),WVDR(:)
     +                ,STR1(:,:),STR2(:,:)
     +                ,STR10(:),STR20(:)
     +                ,STR11(:),STR21(:)

C      COMMON /STRDAT/STRESS(MNP,2),SPWP(MNP),AWL(MNP),SWH(MNP),WVDR(MNP)
C     +                ,STR1(MNP,6),STR2(MNP,6)
C     +                ,STR10(MNP),STR20(MNP)
C     +                ,STR11(MNP),STR21(MNP)
      END MODULE