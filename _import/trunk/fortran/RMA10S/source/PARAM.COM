c
C MLAYER = Maximum number of layers in sediment
      PARAMETER( MNP=26000,MEL=10000,MEQ=50000,MLAY= 17000,
     1     NLAYM=   1, MMAT = 20,  
     +     NQLDS =20, NCQOBS = 2500,
     +     MLAYER=5,
     +     NHDS =5  ,NCHOBS = 3500, NELDS = 40  ,NDPTS = 2600)
