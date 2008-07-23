
      FUNCTION DRODS(SAL,I)
      REAL*8 SAL
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
      DRODS=61932.8/(6511.7-1.906*SAL)**2*GF(I)
      RETURN
      END
C
      FUNCTION FDSED(SED,I)
      REAL*8 SED
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
C**************** SEDIMENT FUNCTION
      FDSED= 1.940*(1.0+SED*1.0E-3)*GF(I)
C****************** 
      RETURN
      END
C
      FUNCTION FDN(TEM,I)
      REAL*8 TEM
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
C**************** TEMPERATURE FUNCTION
      FDN=(1.939938+TEM*(5.588599E-5-1.108539E-5*TEM))*GF(I)
C****************
      RETURN
      END
C
      FUNCTION DRODTM(TEM,I)
      REAL*8 TEM
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
      DRODTM=(5.588599E-5-2.217078E-5*TEM)*GF(I)
      RETURN
      END
C
      FUNCTION FDEN(SAL,I)
      REAL*8 SAL
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
C**************** SALINITY FUNCTION
      FDEN=1.940*((4.906*SAL-11.7)/(6511.7-1.906*SAL)+1.0)*GF(I)
C****************
      RETURN
      END
C
      FUNCTION DRODSD(SED,I)
      REAL*8 SED
C
      DIMENSION GF(2)
      DATA GF/1.0,516./
C-
      DRODSD=1.940E-3*GF(I)
      RETURN
      END
