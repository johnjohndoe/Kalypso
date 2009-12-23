      MODULE BLKMETMOD
!
!
!     BLK4A
!
      INTEGER METFL
!
      ALLOCATABLE WDT1(:,:),WDT0(:,:),IYS(:)                            &
     &       ,METID(:,:),DA(:),TTM(:),AETMP(:)                          &
     &       ,BETMP(:),ISOL(:)                                          &
     &       ,DSOL(:),SOLIN0(:),SOLIN1(:),IDPT(:)
!
!IPK JUL01 ADD IDPT
!      COMMON /BLK4A/ WDT1(11,MMAT),WDT0(11,MMAT),IYS(MMAT)
!     +       ,METFL,METID(MMAT,9),DA(MMAT),TTM(MMAT),AETMP(MMAT)
!     +       ,BETMP(MMAT),ISOL(MMAT)
!     +       ,DSOL(MMAT),SOLIN0(MMAT),SOLIN1(MMAT),IDPT(MMAT)
!
!
      END MODULE
!
!
!
