
      FUNCTION DRODS(SAL,I)
      !function type declaration
      real (kind = 8) :: drods
      !input parameter declaration
      REAL (kind = 8) :: SAL
      integer         :: i
      !local variable declaration
      real (kind = 8) :: gf
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
      DRODS=61932.8/(6511.7-1.906*SAL)**2*GF(I)
      RETURN
      END

!**********************************************************
C
      FUNCTION FDSED(SED,I)
      !function type declaration
      real (kind = 8) :: fdsed
      !input parameter declaration
      REAL (kind = 8) :: SED
      integer         :: i
      !local variable declaration
      real (kind = 8) :: gf
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
C**************** SEDIMENT FUNCTION
      FDSED= 1.940*(1.0+SED*1.0E-3)*GF(I)
C****************** 
      RETURN
      END

!**********************************************************

C
      FUNCTION FDN(TEM,I)
      !function type declaration
      real (kind = 8) :: fdn
      !input parameter declaration
      REAL (kind = 8) ::  TEM
      integer         :: i
      !local variable declaration
      real (kind = 8) :: gf
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
C**************** TEMPERATURE FUNCTION
      FDN=(1.939938+TEM*(5.588599E-5-1.108539E-5*TEM))*GF(I)
C****************
      RETURN
      END

!**********************************************************

C
      FUNCTION DRODTM(TEM,I)
      !function type declaration
      real (kind = 8) :: drodtm
      !input parameter declaration
      REAL (kind = 8) ::  TEM
      integer         :: i
      !local variable declaration
      real (kind = 8) :: gf
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
      DRODTM=(5.588599E-5-2.217078E-5*TEM)*GF(I)
      RETURN
      END

!**********************************************************

C
      FUNCTION FDEN(SAL,I)
      !function type declaration
      real (kind = 8) :: fden
      !input parameter declaration
      REAL (kind = 8) ::  SAL
      integer         :: i
      !local variable declaration
      real (kind = 8) :: gf
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
C**************** SALINITY FUNCTION
      FDEN=1.940*((4.906*SAL-11.7)/(6511.7-1.906*SAL)+1.0)*GF(I)
C****************
      RETURN
      END

!**********************************************************
C
      FUNCTION DRODSD(SED,I)
      !function type declaration
      real (kind = 8) :: drodsd
      !input parameter declaration
      REAL (kind = 8) ::  SED
      integer         :: i
      !local variable declaration
      real (kind = 8) :: gf
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
      DRODSD=1.940E-3*GF(I)
      RETURN
      END
