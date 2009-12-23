      SUBROUTINE MKSAND(T,D,VSET,SRCSNK,GRATE,NETT)
      USE BLKSANMOD
      USE BLK11MOD
!      Calculate source sink term for Suspended Solids
!
      DIMENSION ITINC(24)
      DATA ITINC/1,2,0,0,0,3,3,3,0,0,                                   &
     &           1,2,0,0,4,3,0,3,0,0,                                   &
     &           4,4,4,4/
!
      INTEGER (kind = 4) :: NETT
!NiS,jul06: For calling compatibility D must also be real, kind=8
      REAL(KIND=8) :: D
      REAL*8 T
!
!     USE NETYP TO DETERMINE OUR ELEMENT TYPE
!
      IETP=ITINC(NETT)
      IF(IETP == 0) RETURN
!
!     ALP1 AND ALP2 ARE IS DELIVERED TO MKSS AS A GAUSS POINT VALUES
!     IN COMMON
!
      IF(IETP == 4) THEN
        VSET = -ALP1
        SET=0.
        GRW=0.
      ELSEIF(IETP == 3) THEN
        SET = -ALP1/D
        VSET = 0.
        GRW=ALP2/D
      ELSEIF(IETP == 2) THEN
        IF(ALP1*T+ALP2 <= 0) THEN
          SET=0.
          VSET=-ALP1
        ELSE
          SET=-ALP1
          VSET = 0.
        ENDIF
        GRW=ALP2
      ELSE
        SET = 0.
        VSET = 0.
        GRW= 0.
      ENDIF        
!
      GRATE = GRATE  - SET
      SRCSNK = SRCSNK + GRW
!
      RETURN
      END
!
!
!**********************************************************************
!
      SUBROUTINE MKSSED(T,D,VSET,SRCSNK,GRATE,NETT)
!      Calculate source sink term for Suspended Solids
!
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLK11MOD
!
      DIMENSION ITINC(24)
      DATA ITINC/1,2,0,0,0,3,3,3,0,0,                                   &
     &           1,2,0,0,4,3,0,3,0,0,                                   &
     &           4,4,4,4/
!
      INTEGER (kind = 4) ::  NETT
!NiS,jul06: For calling compatibility D must also be real, kind=8
      REAL(KIND=8) :: D
      REAL*8 T
!
!     USE NETYP TO DETERMINE OUR ELEMENT TYPE
!
      IETP=ITINC(NETT)
!
!     VSET IS DELIVERED TO MKSS AS A GAUSS POINT VALUE
!     Contribution due to settling (positive rate removes mass)
      IF(IETP == 4) THEN
        VSET= ALP1
        SRC = 0.
        SET = 0.
      ELSEIF(IETP == 3) THEN
        VSET = 0.
        SRC = ALP2/D
        SET = ALP1/D
      ELSEIF(IETP == 2) THEN
!
!MD: NEU NEU      
        IF(ALP1*T+ALP2 <= 0) THEN
          SET=0.
          VSET=ALP1
        ELSE
          SET=ALP1
          VSET = 0.
        ENDIF
        SRC = ALP2
!MD: NEU NEU      
!MD:  VSET= ALP1      
!MD:  SRC = ALP2      
!MD:  SET = 0.      
      ELSE
        VSET = 0.
        SRC = 0.
!MD wrong!! GRW = 0.        
        SET = 0.
      ENDIF
!
      GRATE = GRATE - SET
      SRCSNK = SRCSNK + SRC
!MD: ALP1 = Deposition --> SRC --> SRCSNK (+)
!MD: ALP2 = Erosion --> SET --> GRATE (-)
!
      RETURN
      END
!**********************************************************************
!
