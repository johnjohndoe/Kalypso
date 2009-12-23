!     Last change:  MD   12 May 2009    4:25 pm
!
      FUNCTION DRODS(SAL,I)
!function type declaration      
      real (kind = 8) :: drods
!input parameter declaration      
      REAL (kind = 8) :: SAL
      integer         :: i
!local variable declaration      
      real (kind = 8) :: gf
!
      DIMENSION GF(2)
      DATA GF/1.0,516./
!-
      DRODS=61932.8/(6511.7 - 1.906*SAL)**2*GF(I)
      RETURN
      END
!
!**********************************************************
!
      FUNCTION FDSED(SED,I)
!function type declaration      
      real (kind = 8) :: fdsed
!input parameter declaration      
      REAL (kind = 8) :: SED
      integer         :: i
!local variable declaration      
      real (kind = 8) :: gf
!
      DIMENSION GF(2)
      DATA GF/1.0,516./
!-
!**************** SEDIMENT FUNCTION
!MD:  falsche Basisformel
!MD:      FDSED= 1.940*(1.0+SED*1.0E-3)*GF(I)
!MD: Korrektur fuer Dichte(sed):
      FDSED= (1.935*GF(I)) + ((SED*1.0E-3)*(1.0 - 1.935*GF(I)/2650.0))
!****************** 
      RETURN
      END
!
!**********************************************************
!
!
      FUNCTION FDN(TEM,I)
!function type declaration      
      real (kind = 8) :: fdn
!input parameter declaration      
      REAL (kind = 8) ::  TEM
      integer         :: i
!local variable declaration      
      real (kind = 8) :: gf
!
      DIMENSION GF(2)
      DATA GF/1.0,516./
!-
!**************** TEMPERATURE FUNCTION
      FDN=(1.939938+TEM*(5.588599E-5 - 1.108539E-5*TEM))*GF(I)
!****************
      RETURN
      END
!
!**********************************************************
!
!
      FUNCTION DRODTM(TEM,I)
!function type declaration      
      real (kind = 8) :: drodtm
!input parameter declaration      
      REAL (kind = 8) ::  TEM
      integer         :: i
!local variable declaration      
      real (kind = 8) :: gf
!
      DIMENSION GF(2)
      DATA GF/1.0,516./
!-
      DRODTM=(5.588599E-5 - 2.217078E-5*TEM)*GF(I)
      RETURN
      END
!
!**********************************************************
!
!
      FUNCTION FDEN(SAL,I)
!function type declaration      
      real (kind = 8) :: fden
!input parameter declaration      
      REAL (kind = 8) ::  SAL
      integer         :: i
!local variable declaration      
      real (kind = 8) :: gf
!
      DIMENSION GF(2)
      DATA GF/1.0,516./
!-
!**************** SALINITY FUNCTION
      FDEN=1.935*((4.906*SAL-11.7)/(6511.7 - 1.906*SAL)+1.0)*GF(I)
!****************
      RETURN
      END
!
!**********************************************************
!
      FUNCTION DRODSD(SED,I)
!function type declaration      
      real (kind = 8) :: drodsd
!input parameter declaration      
      REAL (kind = 8) ::  SED
      integer         :: i
!local variable declaration      
      real (kind = 8) :: gf
!
      DIMENSION GF(2)
      DATA GF/1.0,516./
!-
!MD   Ableitung von Dichte(sed)/ nach d(Sed):
!MD    --> Dichte(sed) = (1.940*(1.0+SED*1.0E-3)*GF(I)) *1.0E-3
!MD   wird zu:
!MD   !! falsche Basisformel
!MD       DRODSD=(1.935*1.0E-3)*GF(I)
!
!MD: Korrektur fuer Dichte(sed)/ nach d(Sed)      
!MD: Dichte(sed) = (1.940*GF(I)) + ((SED*1.0E-3)*(1.0 - 1.940*GF(I)/2650.0)      
      DRODSD= 1.0E-3 * (1.0 - 1.935*GF(I)/2650.0)
!MDMD: DRODSD= (1.940*(1.0+SED*1.0E-3)*GF(I)) *1.0E-3
      RETURN
      END
