      MODULE BLK11MOD
      
      
      !parameter for time purposes
      !---------------------------
      integer (kind = 4) :: dayofy
      !meaning of the variables
      !-----------------------
      !dayofy     day of the year

      !primary execution control parameters
      !------------------------------------
      integer (kind = 4) :: icesw
      !meaning of the variables
      !-----------------------
      !icesw      ice usage switch
      !           0 - no usage of ice cover
      !           1 - use Ashton formulation of temperature slope calculation
      !           2 - use RMA10 formulation of temperature slope calculation
      
      !global meterology data
      !----------------------
      real (kind = 8) :: TMED
      !meaning of the variables
      !-----------------------
      !tmed       medium temperature to examine status of freezing/melting

      
      !general node calculation value data definition
      !----------------------------------------------
      real (kind = 8), allocatable, dimension (:) :: icethk, icethkol
      !meaning of the variables
      !-----------------------
      !icethk     ice thickness from current time step
      !icethkol   ice thickness from previous time step





      !unit definitions
      integer (kind = 4) :: IIQ, IOMET
      
      !execution parameters; perhaps initialised in the file opening process
      integer (kind = 4) :: MMET
      !meaning of the parameters
      !MMET (switch 0 or 1) - 0 means meteorology file but don't read from it or 
      !                         no meteorology file is present
      !                     - 1 means meteorology file is present and read from it!


      REAL LAT,LONG,SOLHR,TOFDAY,TRLCD,CHI,ELEVAT,STANM,STIME,EXTINC,
     +DAYLEN,VPAIR,EQTEMP,XKRAT,EXTING
      INTEGER NPM11,IJULD,METRIC,IMGEOM,METEQ,
     +IHTMET,IWNDMET
     

      REAL ort


     
      ALLOCATABLE       TSOLHR(:),HA(:),DRYBLB(:),WETBLB(:),
     +             CLOUD(:),WIND(:),AE(:),BE(:),SNOWMT(:)
     +   ,WDIRT(:),DAT(:),ATMPR(:),ORT(:,:)
                 

cipk SEP02
      REAL CAL1,CAL2,CAL3,CAL4,ROW,CHEAT,HTR,XLAT
     +,ROSN,ROIC,TICE,VTR
      
      ALLOCATABLE QICE(:),WNDSP(:),WNDDR(:)
     +,SNOWD(:),AIRTMP(:),SIGMA(:,:)




      END MODULE