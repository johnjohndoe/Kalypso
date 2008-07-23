      MODULE BLKSUBMOD
      ALLOCATABLE
     +             IWFLOW(:),WFLOW(:),WHGT(:),WLEN(:)
     +             ,wflolw(:),TRANSEL(:),ISUBM(:),ISUBMEL(:)
     +             ,NFCTP(:)

CLF NOV06 weir info for renumbering
      INTEGER, ALLOCATABLE :: REWEIR(:,:)


C      COMMON /SUBMRG/
C     +             IWFLOW(MNP),WFLOW(MNP),WHGT(MNP),WLEN(MNP)
C     +             ,wflolw(MNP),TRANSEL(MNP),ISUBM(MNP),ISUBMEL(MEL)
C     +             ,NFCTP(MEL)

      END MODULE
