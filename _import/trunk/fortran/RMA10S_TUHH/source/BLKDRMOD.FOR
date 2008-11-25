      MODULE BLKDRMOD
      
      !element definition data for drying/wetting purposes
      !---------------------------------------------------
      integer (kind = 4), allocatable, dimension (:) :: imato
      !meaning of the variables
      !------------------------
      !imato      old material type of an element from the last time step
      

      !primary execution control parameters
      !------------------------------------
      integer (kind = 4) :: idswt
!meaning of the variables
!-----------------------
!           TODO: How does the dry node modification testing really work?
!idswt      frequency of testing for dry node modification
!           0 - eliminate testing
!          1+ - frequency

      !nodal Marsh parameters
      !----------------------
      real (kind = 8), allocatable, dimension (:) :: adb, adt, ado, akp
!adb        bottom elevation of the transition range in operative Marsh algorithm
!adt        top elevation of the transition range in operative Marsh algorithm
!ado        slot bed elevation in operative Marsh algorithm
!akp        porosity for Marsh algorithm

      

      REAL  DSET,DSETD,SADEL,SADX

      INTEGER ISAD
      ALLOCATABLE    NDRY(:),NDRYO(:),IPT(:),DREC(:),
     +               LISTEL(:)
     +               ,DAME(:),AME(:),ACOR(:),ADOKP(:),AOKP(:)


      ENDMODULE