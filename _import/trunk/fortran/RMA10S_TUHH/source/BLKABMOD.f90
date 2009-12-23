      MODULE BLKABMOD
!
!IPK  LAST UPDATE AUG 09 2005 MAJOR REVISION TO ADD XOUTL,YOUTL
      REAL*8 XUSR,YUSR,XOUTL,YOUTL
      INTEGER NDWT(3)
      INTEGER NOPEL,IPOLY
      ALLOCATABLE                                                       &
     &     XUSR(:),YUSR(:),XOUTL(:),YOUTL(:)
      ALLOCATABLE                                                       &
     &     NOPEL(:,:),IPOLY(:)
!
!     +     XUSR(MNP),YUSR(MNP),XOUTL(MNP),YOUTL(MNP)
!
!     +     NOPEL(3,MNP*2),IPOLY(MNP)
      END MODULE