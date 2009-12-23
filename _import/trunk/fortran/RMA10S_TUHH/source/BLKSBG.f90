!IPK  NEW MODULE DEC 13 2006
      MODULE BLKSBG
      integer (kind = 4), allocatable :: IINTERP(:)
!meaning of the variables
!IINTERP        Switch for H-Boundary lines
!               0 - water level with constant value along CCL
!               1 - water level with linearily changing value along CCL
!               2 - same as 1 (so far)
!
!
      ALLOCATABLE NHYDQ(:),QDIRS(:),NHYDH(:),NHYDG(:)                   &
     &         ,NLAYH(:),NLAYQ(:),IQID(:)
!
!
!
      END MODULE
!
