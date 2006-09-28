C     Last change:  M    14 Jul 2006   12:54 pm
      MODULE BLKDRMOD
C-
      REAL  DSET,DSETD,SADEL,SADX

      INTEGER IDSWT,ISAD
      ALLOCATABLE    NDRY(:),NDRYO(:),IPT(:),DREC(:),
     +               IMATO(:),LISTEL(:)
     +               ,ADB(:),ADT(:),ADO(:),AKP(:)
     +               ,DAME(:),AME(:),ACOR(:),ADOKP(:),AOKP(:)

CIPK NOV97 LINE ABOVE ADDED

C      COMMON /BLKDR/ NDRY(MNP),NDRYO(MNP),IPT(MNP),DREC(MNP),
C     +               IMATO(MEL),LISTEL(MEL),DSET,DSETD,IDSWT
C     +               ,ADB(MNP),ADT(MNP),ADO(MNP),AKP(MNP)
C     +               ,DAME(MNP),AME(MNP),ACOR(MNP),ADOKP(MNP),AOKP(MNP)
C     +               ,SADEL,SADX,ISAD

      ENDMODULE