      SUBROUTINE MKSAND(T,D,VSET,SRCSNK,GRATE,NETT)
      USE BLKSANMOD
      USE BLK11MOD
C      Calculate source sink term for Suspended Solids

CIPK AUG05      INCLUDE 'PARAM.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05	INCLUDE 'BLKSAND.COM'
C-
      DIMENSION ITINC(24)
      DATA ITINC/1,2,0,0,0,3,3,3,0,0,
     +           1,2,0,0,4,3,0,3,0,0,
     +           4,4,4,4/

      INTEGER (kind = 4) :: NETT
!NiS,jul06: For calling compatibility D must also be real, kind=8
      REAL(KIND=8) :: D
      REAL*8 T
C
C     USE NETYP TO DETERMINE OUR ELEMENT TYPE
C
      IETP=ITINC(NETT)
      IF(IETP .EQ. 0) RETURN
C
C     ALP1 AND ALP2 ARE IS DELIVERED TO MKSS AS A GAUSS POINT VALUES
C     IN COMMON
C
      IF(IETP.EQ. 4) THEN
        VSET = -ALP1
        SET=0.
        GRW=0.
      ELSEIF(IETP .EQ. 3) THEN
        SET = -ALP1/D
        VSET = 0.
        GRW=ALP2/D
      ELSEIF(IETP .EQ. 2) THEN
        IF(ALP1*T+ALP2 .LE. 0) THEN
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
C
      GRATE = GRATE  - SET
      SRCSNK = SRCSNK + GRW

      RETURN
      END


C**********************************************************************

      SUBROUTINE MKSSED(T,D,VSET,SRCSNK,GRATE,NETT)
C      Calculate source sink term for Suspended Solids

      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLK11MOD
CIPK AUG05      INCLUDE 'PARAM.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05	INCLUDE 'BLKSED.COM'
CIPK AUG05	INCLUDE 'BLKSAND.COM'

      DIMENSION ITINC(24)
      DATA ITINC/1,2,0,0,0,3,3,3,0,0,
     +           1,2,0,0,4,3,0,3,0,0,
     +           4,4,4,4/

      INTEGER (kind = 4) ::  NETT
!NiS,jul06: For calling compatibility D must also be real, kind=8
      REAL(KIND=8) :: D
      REAL*8 T
C
C     USE NETYP TO DETERMINE OUR ELEMENT TYPE
C
      IETP=ITINC(NETT)
C
C     VSET IS DELIVERED TO MKSS AS A GAUSS POINT VALUE
C     Contribution due to settling (positive rate removes mass)
      IF(IETP .EQ. 4) THEN
        VSET= ALP1
        SRC = 0.
        SET = 0.
      ELSEIF(IETP .EQ. 3) THEN
        VSET = 0.
        SRC = ALP2/D
        SET = ALP1/D
      ELSEIF(IETP .EQ. 2) THEN

      !MD: NEU NEU
        IF(ALP1*T+ALP2 .LE. 0) THEN
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
C
      GRATE = GRATE - SET
      SRCSNK = SRCSNK + SRC
!MD: ALP1 = Deposition --> SRC --> SRCSNK (+)
!MD: ALP2 = Erosion --> SET --> GRATE (-)

      RETURN
      END
C**********************************************************************

