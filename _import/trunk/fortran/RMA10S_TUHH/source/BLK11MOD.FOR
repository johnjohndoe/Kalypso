C     Last change:  K    25 Jan 2007    6:30 pm
      MODULE BLK11MOD

CIPK LAST UPDATE JAN 06 2003
CIPK  LAST UPDATE SEP 27 2002
CIPK  LAST UPDATE APRIL 26 1996
CIPK  LAST UPDATED AUGUST 30 1995
c
c     BLK11
c
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
      INTEGER DAYOFY,NPM11,IJULD,METRIC,IMGEOM,METEQ,
     +IHTMET,IWNDMET
     

      REAL ort


     
      ALLOCATABLE       TSOLHR(:),HA(:),DRYBLB(:),WETBLB(:),
     +             CLOUD(:),WIND(:),AE(:),BE(:),SNOWMT(:)
     +   ,WDIRT(:),DAT(:),ATMPR(:),ORT(:,:)
                 

cipk SEP02
      REAL ICETHK,ICETHKOL,CAL1,CAL2,CAL3,CAL4,ROW,CHEAT,TMED,HTR,XLAT
     +,ROSN,ROIC,TICE,VTR
      INTEGER ICESW
      
      ALLOCATABLE ICETHK(:),QICE(:),WNDSP(:),WNDDR(:)
     +,SNOWD(:),AIRTMP(:),ICETHKOL(:),SIGMA(:,:)



C      ALLOCATABLE       TSOLHR(MMAT1),HA(MMAT1),DRYBLB(MMAT1),WETBLB(MMAT1),
C     +             CLOUD(MMAT1),WIND(MMAT1),AE(MMAT1),BE(MMAT1),SNOWMT(MMAT1)
C     +   ,WDIRT(MMAT1),DAT(MMAT1),ATMPR(MMAT1)
      
C      ALLOCATABLE ICETHK(MAXP),QICE(MAXP),WNDSP(MAXP),WNDDR(MAXP)
C     +,SNOWD(MAXP),AIRTMP(MAXP),ICETHKOL(MAXP),SIGMA(MAXP,2)

      END MODULE