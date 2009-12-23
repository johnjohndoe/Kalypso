      module mod_input
      
      contains
CIPK  LAST UPDATE AUG 22 2007  ADD ICPU
CIPK  LAST UPDATE FEB 26 2007  REVISE TEST TO AVOID ACCIDENTALLY GOING TO COEFV
CIPK  LAST UPDATE AUGUST 30 2006 ADD CONSV AND AVEL OPTIONS
CIPK  LAST UPDATE APRIL 05 2006 MODIFY CALL TO GETINIT
CIPK  LAST UPDATE MARCH 25 2006 ADD TESTMODE
CIPK  LAST UPDATE MAR 22 2006 ADD OUTPUT FILE REWIND
CIPK  LAST UPDATE MAY 02 2003 add ELEMENT TYPE DROPOUT FOR EROSION/SETTLING
CIPK  LAST UPDATE MAR 18 2003 add diffusion switch ( default of  0 uses old formulations
CIPK  LAST UPDATE AUG 4  2002 REVISIT WIND STRESS COEFFICIENTS
CIPK  LAST UPDATE SEP 26 2004  ADD MAH AND MAT OPTION
CIPK  LAST UPDATE SEP 20 2004  ENSURE AO,AKP ETC VALUES AT ALL NODES
CIPK  LAST UPDATE SEP 6 2004   add error file
CIPK  LAST UPDATE MAY 03 2004 ALLOW FOR LOAD APPLIED ONLY AS MASS
CIPK  last update FEB 11 2004 add IOV option
CIPK  LAST UPDATE DEC 16 2003 ADD IEDSW DEPENDENCE
cipk  last update sep 19 2003 switch units for col.dat
cipk  LAST UPDATE MAR 14 2003 add FREQUCY FOR OUTPUT OF RESULTS FILES AND RESTART FILES
cipk  last update jan 14 2003 add extra character read to data line
CIPK  LAST UPDATE JAN 06 2003 ADD EQUILIBRIUM TEMP FORMULATION
CIPK  LAST UPDATE DEC 12 2002 ADD DEFINITIONS FOR 3DG FILE
CIPK  LAST UPDATE SEP 27 2002 ADD DATA FOR ICE CALCULATION
CIPK  LAST UPDATE DEC 24 2001 ADD DEFINITIONS FOR 3DG FILE
CIPK  LAST UPDATE OCT 17 2001 ADD FURTHER OPTION TO POWER STATION DATA
CIPK  LAST UPDATE  SEP 18 2001 ALLOW FOR MAX PERMISSIBLE TEMP INCREMENT
CIPK  LAST UPDATE JUL 09 2001 CHNAGE CALL TO FILE
CIPK  LAST UPDATE MAR 13 2001 ADD TIME STEP TRANSITION OPTION
CIPK  LAST UPDATE FEB 08 2001 MOVE CONTINUITY LINE READ TO GETGEO
CIPK  LAST UPDATE DEC 21 2000 ADD PROJECTION SWITCH
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
CRRR  LAST UPDATE mAR 20 2000 CORREC BUG IN COMPUTATION OF ELEMENT LOAD OPTION
cipk  last update Jan 19 2000 allow * label
CIPK  LAST UPDATE DEC 20 1999 IMPROVE ROUND-OFF TEST FOR MID-SIDES
cipk  last update Nov 12 1999 add data line for automatic 3-d to 2-d transition 
cipk  last update July 18 1999 revers print order for Smagorinsky params
CIPK  LAST UPDATE jAN 19 1999 FIX DIRCETIONS AT 2DV JUCNTIONS
CIPK  LAST UPDATE JAN 12 1999  ADDITIONS FOR SS VALUES AND FIX TO 'RO'
cipk  last update Nov 11 1998
cipk  last update Aug 27 1998
cipk  last update Mar 13 1998 set default additional friction
CIPK  LAST UPDATE FEB 12 1998 SAVE NETYP
CIPK  LAST UPDATE FEB 4 1998
cipk  last update Jan 21 1998 expand check lines
CIPK  LAST UPDATE NOV 26 1997 ADD GINPT
CIPK  LAST UPDATE NOV 20 1997 ADD ELEMENT COUNTING
CIPK  LAST UPDATED NOVEMBER 13 1997
CIPK  LAST UPDATE JAN22 1997 ADD SMAGORINSKY OPTION
CIPK  LAST UPDATE OCT 1 1996
cipk  last updated Apr 24 1996
CIPK  LAST UPDATED SEP 19 1995
      SUBROUTINE INPUT(IBIN, m_SimModel)
      
      !calls
      !-----
      !ginpt
      !ErrorMessageAndStop
      !getgeo
      !initsed
      !check
      !insand
      !sprop
      !getbc
      !anglen
      !getcon
      !threed
      
      use mod_getgeo
      use mod_getinit

      use mod_Model   
      USE IHILMOD
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKTSMOD
      USE BLK10
!NiS,mar06: Making Kalypso-2D specific arrays accessable
      USE ParaKalyps
!NiS,mar06: add the module Parammod because the occuring error while compiling is caused by a variable
!           that is defined within that module (NLAYMX)
      USE Parammod
      USE Para1DPoly
      use mod_Nodes
      use mod_ContiLines
     
      USE ASSIGNROFILES    ! the BANKPROFILES array is assigned by reading profile data.Including variable SIZ.
      USE make_profile
      USE types
      USE share_profile,only :BANKPROFILES , fenodes, BANKEVOLUTION ! the BANKPROFILES and fenodes array are defined here.
      
      use check_mod, only: check
!-
      SAVE
      type (simulationModel), pointer :: m_SimModel

      type (linkedNode), pointer :: tmpNode => null()
      type (Node), pointer :: node1D_first, node1D_last
      type (arc), pointer :: arc1D
      real (kind = 8) :: arcVec (1:2)
      type (contiLine), pointer :: tmp_contiLines (:)
      type (contiLine), pointer :: tmp_singleCCL, tmp_ccl
      integer (kind = 4) :: i
      integer (kind = 4) :: elt
      
      integer (kind = 4) :: iostatus
      real (kind = 8) :: schwarzConv


C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK APR96 ADD BLK11
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'

!NiS,mar06: new variable definitions
      INTEGER :: k,NL,kmax
      CHARACTER(LEN=5)::IDString
      character (len = 192) :: inputline
      INTEGER :: istat_temp
  
      INTEGER :: ProfileCounter = 0     !HN,May2009, PROFILE COUNTER FOR THE CASE THAT NONE OF THE CONTILENS HAS AN ASSOCIATED PROFILE.
      INTEGER :: ISTAT , ProfileID = -1
      LOGICAL :: middsidd =.FALSE., exists
  ! test
      type (linkedNode) , pointer     :: nextNode=> null()
      real(kind = 8)   :: distt         ! distance the to origin of profile.
  ! test
!-
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: HTP
      REAL(KIND=8) :: dirPointer   ! HN.07.07.2009 should be defined here, if it is not already defined elsewhere!
!-

CIPK JUL01
!NiS,jul06: Consistent data length for passing it as a parameter
!      CHARACTER*48 ANAME
      CHARACTER*96 ANAME
!-
C-
      ALLOCATABLE IFXSAL(:)
      DIMENSION SALBC(3),QDM(3)
C
      DIMENSION VDIM(4),IM(8),IMIDD(16)
cipk jan94 new line below
C      CHARACTER*80 DLIN
      
      DATA VOID/-1.E20/
      DATA  IHOE/100/
cipk      CHI = 3.8E-06
CIPK AUG02      CHI =13.60E-06
      CHI=0.0005*1.226
C-
C-..... INITALIZE ARRAYS NOT COVERED BY INPUT .....
C-
CAUG93IPK      MACC=0
CIPKFEB94
      ALLOCATE (IFXSAL(MAXP))
      IFXSAL=0
      DO I=1,3
        QDM(I)=0.
      ENDDO
      IBIN=LIN
CIPK APR96 ADD DEFINITIONS
      ibinrst=lin
      iyend=0
      idye=0
      hrend=0
      nscrin=18
      open (nscrin, file = 'scratch.fil', 
     +      status = 'REPLACE', form='formatted')
CIPK APR96 END ADDITIONS
      LE=MAXE
      LP=MAXP
      TET = 0.0
      GRAV=32.2
      IGF=1
      ICYC=0
      UMIN=1.
      VMIN=1.
      POWER=1.
      PWERIN=1.

      !nis,jun07: Add some initializations
      !for sediment (unit no. ???)
      lss = 0
      !for sand (unit no. ???)
      lsand = 0
      !for equilibrium bedload calculation
      lbed = 0
      !initial number of ocean boundary nodes
      nrbd = 0
      !default value for collapsing 3D to 2D (default switch off)
      ITRANSIT = 0
      !-
CIPK OCT96      EDD1=1.
CIPK OCT96      EDD2=0.
CIPK OCT96      EDD3=0.
C      do k=1,maxe
C        edd1(k)=1.0
C        edd2(k)=0.0
C        edd3(k)=0.0
C      enddo

C
C                                       READ AND PRINT TITLE AND CONTROL
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     READING THE PROFILE DATA IF IT IS ALREADY PRESENT     !
!     AND OPENED BY SUBROUTINE IN FILE.F90                  !
!     Restart data (Iprofile = 731)        HN Sept2009                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      BANKEVOLUTION =.FALSE. 
      
      IF (( IPROFIN == 73) .OR. ( IPROFIN == 731) ) THEN
       ! read profile data from unit 73 and assign BANKPROFILES array
       CALL READPROFILES (IPROFIN,ProfileCounter) 
      
       CLOSE (IPROFIN)
      
         BANKEVOLUTION =.TRUE. 
      
        OPEN( UNIT =276 , FILE = 'PROFILE_CONTI.TXT', STATUS ='REPLACE',
     +   ACTION ='WRITE')

       IF ( .NOT. ALLOCATED (FENODES) )THEN   
        ALLOCATE (FENODES(MAXP),STAT = ISTAT)     
      
        IF (ISTAT/=0) THEN
          WRITE (*,*) 'FAILURE BY ALLOCATION OF FENODES ARRAY IN 
     +                                    SUBROUTINE INPUT....'
        END IF
       
       END IF 
        ! initialize the fenodes array
        CALL MAKEFENODES ( FENODES, MAXP , 1)
       
      ENDIF 
    
      WRITE(LOUT,6000)
CIPK NOV97      READ(LIN,7000) ID,DLIN
CIPK AUG05      call ginpt(lin,id,dlin)
      IF(ID(1:2) /= 'TI') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(75,*) ' ERROR -- EXPECTED LINE TYPE TI'
        WRITE(*,*) ' ERROR -- EXPECTED LINE TYPE TI'
        STOP
      ENDIF
cipk aug02 expand to 80 char
      READ(DLIN,5005) TITLE(1:72)
      WRITE(LOUT,6005) TITLE
      write(*,*) 'read title'
CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr96 add line type C0
      IF(ID(1:2) /= 'C0') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6998) ID(1:2)
        WRITE(75,6998) ID(1:2)
 6998   FORMAT('UNABLE TO FIND LINE TYPE -C0- FOUND LINE TYPE ',A2)
        STOP 'LOOKING FOR C0'
      ENDIF 
c  the following variable turns:
c   0 - original RMA-10
c   1 - quadratic functions
c   2 - linear functions
      IOPTZD = 1
cipk dec03      WRITE(*,*) 'Enter option for Eddy coefficient method'
cipk dec03      WRITE(*,*) '  0 = original RMA-10'
cipk dec03      WRITE(*,*) '  1 = quadratic nodal Mellor-Yamada'
cipk dec03      WRITE(*,*) '  2 = linear nodal Mellor-Yamada'
cipk dec03      WRITE(*,*) '  3 = gaussian Mellor-Yamada'
cipk oct96 add iedswddd
CIPK DEC00 ADD IPROJ
      READ(DLIN,5061)IOPTZD,IDNOPT,IYRR,DAYOFY,TET,IEDSW,TBFACT,TBMIN
     +  ,IPROJ

      write(*,*) 'read c0'
CIPK FEB04 SAVE TIMES IN CASE IOV ACTIVE
      IYKK=IYRR
      IDTM=DAYOFY
      TTEM=TET
CIPK DEC03      IF(TBFACT == 0.) TBFACT=0.2
CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)
cipk end changes apr 96

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ID(1:2) /= 'C1') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6999) ID(1:2)
        WRITE(75,6999) ID(1:2)
 6999   FORMAT('UNABLE TO FIND LINE TYPE -C1- FOUND LINE TYPE ',A2)
        STOP 'LOOKING FOR C1'
      ENDIF 

cipk nov98 add surface friction option
      
cipk mar03 add diffusion switch ( default of  0 uses old formulations
      
CIPK MAR05
      READ(DLIN,5010) NDP,IGRV,IZB,IPASS1,IPASS2,IPASS3,IZERS,IDIFSW
     +  ,INOTR
      write(*,*) 'read c1'
      IF(IGRV == 0) GRAV=32.2
      IF(IGRV == 1) GRAV=9.81
      
cipk jul01  Store info on metric geometry
      if(grav < 10.) then
        imgeom=1
      else
        imgeom=0
      endif

cipk sep96 add iedsw
CIPK JUL99 reverse tbfact and tbmin order

cipk jan01 add max layer test
cWP Feb 2006 Change NLAYM to NLAYMX
      IF(NDP < -1*(NLAYMX+1)) THEN
c-
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,*) ' Allowable number of layers exceeded'
        WRITE(*,*) ' Increase MLAY in PARAM.COM'
        WRITE(*,*) ' Execution terminated'
        WRITE(75,*) ' Allowable number of layers exceeded'
        WRITE(75,*) ' Increase MLAY in PARAM.COM'
        WRITE(75,*) ' Execution terminated'
        STOP
      ENDIF

      WRITE(LOUT,6009) IOPTZD,IDNOPT,IEDSW,TBFACT,TBMIN,IYRR,DAYOFY,TET
     +,IPROJ
CIPK DEC00 ADD IPROJ

cipk mar03 add diffusion switch ( default of  0 uses old formulations
      
      WRITE(LOUT,6010) NDP,GRAV,IZB,IPASS1,IPASS2,IPASS3,IZERS,IDIFSW
     +  ,INOTR
      !REMOVE FOR RMA·Kalypso
      !nll is obsolete
      !nopt is obsolete
      !ifot is obsolete
      !ifit is obsolete
      !WRITE(LOUT,6011) NB,NLL, IFILE,NOPT,IFIT,IFOT
      WRITE(LOUT,6011) NB, IFILE
      
C
      IF( IFILE > 0 ) REWIND IFILE

!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove editing obsolete unit nopt
!nopt is obsolete
!-

CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ID(1:2) /= 'C2') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6997) ID(1:2)
        WRITE(75,6997) ID(1:2)
 6997   FORMAT('UNABLE TO FIND LINE TYPE -C2- FOUND LINE TYPE ',A2)
        STOP 'LOOKING FOR C2'
      ENDIF 
      READ(DLIN,'(5F8.0,I8,f8.2,I8)')
     + OMEGA, ELEV, XSCALE, YSCALE, ZSCALE, IDEBUG, p_bottom, Moment_off
      !nis,jun07: Set default values
      if (Moment_off == 0) then
        Moment_off = 15
      ELSEIF (Moment_off < 0) then
        Moment_off = 0
      endif
      write(*,*) 'read c2'
CIPK DEC99 SET UP INITIAL ELEV
      ELEV1=ELEV
CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ID(1:2) /= 'C3') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6996) ID(1:2)
        WRITE(75,6996) ID(1:2)
 6996   FORMAT('UNABLE TO FIND LINE TYPE -C3- FOUND LINE TYPE ',A2)
        STOP 'LOOKING FOR C3'
      ENDIF 
      READ(DLIN,5020) CMIN,CPR,UNOM,UDIR,HMNN,DSET,DSETD
      
      !nis,aug08: Prevent nan in Darcy-Weisbach calculations
      if (dset < 0.0d0) then
        dset = 0.0d0
        write(*,*)'WARNING - dset is < 0,0, it will be fixed 0,0'
      endif
      if (dsetd <= dset) then
        dsetd = dset + 0.0005
        write(*,*) 'WARNING - dsetd is smaller dset, it will be fixed'
        write(*,*) '          to a value 0.0005 higher then dset'
        write(*,*) '          Change prevents oszillating activation'
      endif
      
      
      write(*,*) 'read c3'
C      HMIN=VOID
C      IF(HMNN /= 0.) HMIN=HMNN
      IF(HMNN == 0.) THEN
        HMIN=VOID
      ELSE
        HMIN=HMNN
      ENDIF
      !We don't want any minimum value!
      !IF(UNOM == 0.) UNOM=0.25
      UDIR=UDIR/57.3
      IF( XSCALE == 0.0 ) XSCALE = 1.0
      IF( YSCALE == 0.0) YSCALE =1.0
      IF( ZSCALE == 0.0 ) ZSCALE = 1.0
      WRITE(LOUT,6020) 
     + OMEGA,ELEV,XSCALE,YSCALE,ZSCALE,CMIN,CPR,UNOM,UDIR,HMNN,IDEBUG
c
CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ID(1:2) /= 'C4') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6995) ID(1:2)
        WRITE(75,6995) ID(1:2)
 6995   FORMAT('UNABLE TO FIND LINE TYPE -C4- FOUND LINE TYPE ',A2)
        STOP 'LOOKING FOR C4'
      ENDIF 
cipk sep96 add to 3 lines below for ocean exchange percentantage and mixing
      !EFa Nov06, beient ließt Option für Beiwert ein

      READ(DLIN,5021) SALI,TEMPI,SEDI,UINP,VINP,prcnt,DMIX,beient
      write(*,*) 'read c4'
      !default values; what's that
      if (beient /= 1 .AND. beient /= 2 .AND. beient /= 3) then
        beient = 0
      end if
      !testoutput
      WRITE(*,*) 'Beient: ', beient, 'Moment_off: ', Moment_off
      !-

      WRITE(LOUT,6021) prcnt,DMIX,SALI,TEMPI,SEDI,UINP,VINP
 6021 FORMAT(5X,'PERCENT RETURNED AT OCEAN',T26,F10.1/
     +       5X,'SURFACE MIXING DEPTH',T26,F10.1
     +       //'   INITIAL VALUES'/8X,'  SALINITY   ',F8.2/
     +                             8X,'  TEMPERATURE',F8.2/
     +                             8X,'  SEDIMENT   ',F8.2/
     +                             8X,'  X-VEL      ',F8.2/
     +                             8X,'  Y-VEL      ',F8.2)
c
CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ID(1:2) /= 'C5') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,6994) ID(1:2)
        WRITE(75,6994) ID(1:2)
 6994   FORMAT('UNABLE TO FIND LINE TYPE -C5- FOUND LINE TYPE ',A2)
        STOP 'LOOKING FOR C5'
      ENDIF 
      READ(DLIN,5011) NITI,NITN,TSTART,NCYC,IPRT,NPRTF,IRSAV,NPRTI,IDSWT
      !check for maximum values
      if (NITI > 90) call ErrorMessageAndStop (1008, 0, 0.0d0, 0.0d0)
      if (ncyc > 0 .AND. nitn > 90)
     +  call ErrorMessageAndStop (1009, 0, 0.0d0, 0.0d0)

!NiS,may06: In the case of Kalypso-2D format input file for geometry, it is among other things predetermined, from which time step to start.
!           This timestep is saved in the global variable iaccyc. If the value is (iaccyc > 1), it means, that the beginning time step is not
!           the over all beginning. For that reason, no steady calculation can be started. This means, that the user has to be informed about
!           the occuring error, if  (iaccyc > 1) .AND. (NITI /= 0)
      !EFa jun07, necessary for autoconverge

      nitizero = niti
      nitnzero = nitn
      if (niti /= 0.) then
        nitazero = niti
      ELSE
        nitazero = nitn
      end if
      !-
      !nis,jan09: Use Error Message file
      IF (iaccyc > 1 .AND. NITI /= 0) 
     + call ErrorMessageAndStop (1604, 0, 0.0d0, 0.0d0)

      write(*,*) 'read c5'
C
      WRITE(LOUT,6025)NITI,NITN,TSTART,NCYC,IPRT,NPRTF,IRSAV,NPRTI,IDSWT
     +,DSET,DSETD
     
!parameter meaning
!nprti      frequency to print results to model output file at the end of time step
!nprt       frequency to print results to model output file at the end of iteration
!irsav      time step to start with writing of results

      if (nprtf == 0) nprtf = 1

CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk MAR03 add FREQUCY FOR OUTPUT OF RESULTS FILES AND RESTART FILES

CIPK AUG07  ADD ICPU
      ICPU = 0
      IF(ID(1:2) == 'C6') THEN
cipk mar06 allow for output file rewind      
CIPK AUG07  ADD ICPU
        READ(DLIN,'(4I8)') irMiniMaxiSav, nprtmetai, IOUTRWD,ICPU
        !irMiniMaxiSav frequency to print out min/max results
        !nprtmetai  frequency to print results to output file at the end of iteration
        !ioutrwd    frequency of rewinding output file to reduce the file size
        !icpu       number of processors; if value is greater than 0 it means that the MKL is used!
        
        if (irMiniMaxiSav == 0) irMiniMaxiSav = nitn
        
        

        call ginpt(lin,id,dlin)
          WRITE(LOUT,6024) IOUTRWD,ICPU
        IF(IOUTRWD == 0) IOUTRWD=NCYC+1
      ELSE
        IOUTRWD=NCYC+1
      ENDIF
      
      !nis,sep09: HACK: Deallocate some arrays, if MKL is used
      if (icpu /= 0) deallocate (LHS, QS)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !nis,jan08: New control line for additional output options
      !Set standard values
      WriteNodeBlock = 0
      testoutput = 0
      percentCheck = 0
      if (ID(1:2) == 'C7') then
        read (DLIN, '(3I8)') WriteNodeBlock, testoutput, percentCheck
        call ginpt (lin, id, dlin)
      end if

CIPK ADD ICNSV AND IAVEL AND MAKE ORDER OPTIONAL
      ICNSV=0
      IAVEL=0

CIPK MAR06 ADD TESTMODE

   18 CONTINUE
      IF(ID(1:4) == 'TEST') THEN
      
        READ(DLIN,'(I8,8F8.0)') ITSTMOD,TSTVAR
        call ginpt(lin,id,dlin)
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk dec99 add initial condition
      IF(ID(1:3) == 'INI') THEN
      
        READ(DLIN,'(F8.0)') ELEV1
        call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk MAY02 add sand data
      ELSEIF(ID(1:3) == 'SND') THEN
      
        LSAND=6 
        call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk APR05 add CLAY data
      ELSEIF(ID(1:3) == 'SED') THEN
        READ(DLIN,'(I8)') INEWBED
        LSS=6 
        call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk MAY02 add BEDLOAD OPTION data
      ELSEIF(ID(1:3) == 'BED') THEN
      
        LBED=1 
        call ginpt(lin,id,dlin)
CIPK SEP06
      ELSEIF(ID(1:5) == 'CONSV') THEN
      
        ICNSV=1 
        call ginpt(lin,id,dlin)
      ELSEIF(ID(1:4) == 'AVEL') THEN
      
        IAVEL=1 
        call ginpt(lin,id,dlin)
      ELSE
        GO TO 19
      ENDIF
      GO TO 18
   19 CONTINUE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk aug02 add BEDLOAD OPTION data
      ZSTDEP=0.
      IF(ID(1:3) == 'ZDP') THEN
      
        READ(DLIN,'(F8.0)') ZSTDEP
        WRITE(LOUT,6047) ZSTDEP
        call ginpt(lin,id,dlin)
      ENDIF


!MD:
!MD:   New Option to Speed-up Morphology for cohesive Sediment
!MD
      FACT_SPEED_UP=1.0D0
      IF(ID(1:6) == 'FACTSU') THEN
        READ(DLIN,'(F8.1)') FACT_SPEED_UP
      WRITE(LOUT,'(F8.1)') FACT_SPEED_UP
        call ginpt(lin,id,dlin)
      ENDIF
!

!
!  DJW Dec 2004.  Adding Option to Switch off Morphology
!
      FACTMORPH=1.
      IF(ID(1:3) == 'SMO') THEN
      
        READ(DLIN,'(F8.0)') FACTMORPH
      WRITE(LOUT,'(F8.0)') FACTMORPH
        call ginpt(lin,id,dlin)
      ENDIF
!
!  END DJW Dec 2004
!      
!
!  DJW Feb 2005.  Adding Option For Scaling Factors to apply to RW
!
C      FACTMORPH=1.
      IF(ID(1:3) == 'RUF') THEN
      
        READ(DLIN,'(2F8.0)') RWFACT, RWMIN
        WRITE(LOUT,'(2F8.0)') RWFACT, RWMIN
        call ginpt(lin,id,dlin)
      ENDIF
!
!  END DJW Dec 2004
!      



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk SEP02 add sand data
   20 CONTINUE
      IF(ID(1:3) == 'CRS') THEN
      
        READ(DLIN,'(I8,F8.0)') NN, CRSLOP(NN)
        call ginpt(lin,id,dlin)
        GO TO 20
      ENDIF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk may03 add cutout opton for settling/erosion for element types
   22 CONTINUE
      IF(ID(1:4) == 'DRP1') THEN
        READ(DLIN,5010) (IEDROP(N),N=1,9)
        call ginpt(lin,id,dlin)

        !MD: weniger als 9 Eintrage
        DO N = 1, 9
          IF (IEDROP(N) > 0) THEN
            N1=N
            DROPMAX = N1
          END IF
        Enddo

          GO TO 22
      ElSEIF(ID(1:4) == 'DRP2') THEN
        READ(DLIN,5010) (IEDROP(N),N2=(1+N1),(9+N1))

        !MD: weniger als 9 Eintrage
        DO N = (1+N1), (9+N1)
          IF (IEDROP(N) > 0) THEN
            N2=N1+N
            DROPMAX = N2
          END IF
        Enddo

        N1 = N2
        DROPMAX = N1

        IF (DROPMAX > 85) THEN
          WRITE(*,*) ' ERROR in INPUT: '
          WRITE(*,*) ' Maximal Counter for DropOut DRP=85 is reached!!'
          WRITE(75,*) ' ERROR in INPUT: '
          WRITE(75,*) ' Maximal Counter for DropOut DRP=85 is reached!!'
          STOP
        END IF

        call ginpt(lin,id,dlin)
          GO TO 22
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!MD Jun09 add sediment data

      IF(ID(1:6) == 'MAXSED') THEN
        READ(DLIN,'(F8.0)') SedHighPerm
        IF (SedHighPerm <= 0.0) THEN
          WRITE(*,*) ' ERROR in INPUT: '
          WRITE(*,*) ' Maximal Sediment-Concentration MAXSED <= ZERO!!'
          WRITE(75,*) ' ERROR in INPUT: '
          WRITE(75,*) ' Maximal Sediment-Concentration <= ZERO!!'
          STOP
        ElseIF (SedHighPerm < 1000.0) THEN
          WRITE(*,*) ' CAUTION: '
          WRITE(*,*) ' Maximal Sediment-Concentration MAXSED < 1000.0 '
          WRITE(75,*) ' CAUTION: '
          WRITE(75,*) ' Maximal Sediment-Concentration MAXSED < 1000.0 '
        END IF
        call ginpt(lin,id,dlin)
      ENDIF

      IF(ID(1:6) == 'MINSED') THEN
        READ(DLIN,'(F8.0)') SedLowPerm
         IF (SedLowPerm < 0.0) THEN
          WRITE(*,*) ' CAUTION: '
          WRITE(*,*) ' Minimal Sediment-Concentration < ZERO!!'
          WRITE(75,*) ' CAUTION: '
          WRITE(75,*) ' Minimal Sediment-Concentration < ZERO!!'
        ElseIF (SedLowPerm >= SedHighPerm) THEN
          WRITE(*,*) ' ERROR in INPUT: '
          WRITE(*,*) ' Concentration MAXSED <= MINSED --> STOP'
          WRITE(75,*) ' ERROR in INPUT: '
          WRITE(75,*) ' Concentration MAXSED <= MINSED --> STOP'
          STOP
        END IF
        call ginpt(lin,id,dlin)
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF(ID(1:2) == 'CV') THEN
cipk apr97 add to data read for equation dropout
        READ(DLIN,'(6F8.0,i8,2f8.0)', iostat = iostatus) 
     +   (CONV(J),J=1,6),idrpt,drfact, schwarzConv
     
      if (iostatus == 0 .AND. schwarzConv > 0) then
        m_SimModel.schwarzConv = schwarzConv
      endif
CIPK NOV97      READ(LIN,7000) ID,DLIN
      call ginpt(lin,id,dlin)
      ELSE
        DO J=1,6
          CONV(J)=0.
        ENDDO
      ENDIF
      WRITE(LOUT,6022) (CONV(J),J=1,6) 
 6022 FORMAT(// '  CONVERGENCE VALUES'/'     X-VELOCITY',1PE19.2/
     +                                 '     Y-VELOCITY',1PE19.2/
     +                                 '     DEPTH     ',1PE19.2/
     +                                 '     SALINITY  ',1PE19.2/
     +                                 '     TEMP      ',1PE19.2/
     +                                 '     SEDIMENT  ',1PE19.2)
cipk apr97
      if(idrpt == 1) then
        write(lout,6034) drfact
 6034   format(//'  EQUATION DROPOUT ACTIVATED.  FACTOR EQUALS'
     +            ,F8.4)
      endif
cipk apr97

!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!EFa jun07, autoconverge option
!
!      IF(ID(1:2) == 'AC') THEN
!        READ(dlin,'(4i8)') beiauto, linlog, nnnst, nnnunst
!        CALL GINPT(LIN,ID,DLIN)
!      ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------

CIPK  FEB04 add IOV option

      IF(ID(1:3) == 'IOV') THEN
        IOV=1
        CALL GINPT(LIN,ID,DLIN)
      ELSE
        IOV=0
      ENDIF
cipk FEB04 end addition

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!nis,jun07: Deactivated for the moment, has to be reactivated, when everything else is debugged
!cipk feb97 add line to select optimisation
!      IF(ID(1:3) == 'IOP') THEN
!        READ(DLIN,'(F8.2)') W2FACT
!        IF(W2FACT == 0.) THEN
!          W2FACT=4.
!        ENDIF
!CIPK NOV97      READ(LIN,7000) ID,DLIN
!        call ginpt(lin,id,dlin)
!        IOPTIM=0
!      ELSE
!        IOPTIM=2
!      ENDIF
!cipk feb97 end changes
!-

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK dec02 add input of data for ice on surface

      IF(ID(1:4) == 'ICE1') THEN
        READ(DLIN,'(8F8.0,i8)')ROW,CHEAT,TMED,HTR,XLAT,ROSN,ROIC,TICE,
     +        ICESW
        ICESW=ICESW+1
        IF(ROW == 0.) ROW=1000.
        IF(XLAT == 0.) XLAT=333.4
        IF(ROSN == 0.) ROSN=300.
        IF(ROIC == 0.) ROIC=917.
        WRITE(LOUT,6200) ROW,CHEAT,TMED,HTR,XLAT,ROSN,ROIC,TICE
        call ginpt(lin,id,dlin)
        IF(ID(1:4) == 'ICE2') THEN
          READ(DLIN,'(5F8.0)') CAL1,CAL2,CAL3,CAL4,VTR
          call ginpt(lin,id,dlin)
        ELSE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR ICE2 DATA LINE EXPECTED'
          WRITE(75,*) 'ERROR ICE2 DATA LINE EXPECTED'
          STOP
        ENDIF
        WRITE(LOUT,6201) CAL1,CAL2,CAL3,CAL4,VTR
      ELSE
        ICESW=0
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK NOV99 ADD DATA LINE FOR 3-D TO 2-D COLLAPSE
      
      IF(ID(1:3) == 'COL') THEN
        READ(DLIN,'(F8.2)') TRANSIT
        ITRANSIT=1
        WRITE(LOUT,6100)
        IF(TRANSIT == 0.) THEN
          WRITE(LOUT,6101)
        ELSE
          WRITE(LOUT,6102) TRANSIT
        ENDIF
        call ginpt(lin,id,dlin)
cipk revised unit
        OPEN(UNIT=78,FILE='COL.DAT',STATUS='UNKNOWN')

      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK MAR01 ADD DATA LINE FOR TIME STEP LENGTHENING
      
      IF(ID(1:3) == 'TST') THEN
        READ(DLIN,'(I8,2F8.0)') NODETR,TRELEV,TRFACT
        WRITE(LOUT,6103)
        WRITE(LOUT,6104) NODETR,TRELEV,TRFACT
        call ginpt(lin,id,dlin)
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK MAR01 ADD DATA LINE FOR RECYCLING FLOW
      
   24 CONTINUE
      IF(ID(1:3) == 'PWR') THEN
CIPK OCT01 ADD NADTYP OPTION TO INPUT AND OUTPUT
        READ(DLIN,'(2I8,2F8.0,8X,3F8.0,I8)') NINCC,NOUTCC(NINCC),
     +      ADDSAL(NINCC),ADDTMP(NINCC,1),ADDTMP(NINCC,3),ADDSED(NINCC)
     +     ,ADDMAX(NINCC),NADTYP(NINCC)
CIPK SEP01 ADD ADDMAX
        WRITE(LOUT,6105)
        WRITE(LOUT,6106) NINCC,NOUTCC(NINCC),
     +      ADDSAL(NINCC),ADDTMP(NINCC,1),ADDTMP(NINCC,3),ADDSED(NINCC)
     +     ,ADDMAX(NINCC),NADTYP(NINCC)
CIPK SEP01 ADD ADDMAX
        call ginpt(lin,id,dlin)
        GO TO 24
      ENDIF
cipk aug01 add Multi PWR line option

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JAN03 ADD DATA LINE FOR EQUILIBRIUM TEMPERATURE FORMULATION
      
      METEQ=0
      IF(ID(1:3) == 'EQT') THEN
        READ(DLIN,'(3F8.0)') EQTEMP,XKRAT,EXTING
        WRITE(LOUT,6107)
        WRITE(LOUT,6108) EQTEMP,XKRAT,EXTING
        METEQ=1
        call ginpt(lin,id,dlin)
      ENDIF
CIPK HAN03 END ADDITION

      WRITE(LOUT,6000)
      WRITE(LOUT,6005) TITLE
C-
C-.....READ ELEMENT CHARACTERISTICS.....
C-

!NiS,apr06: Add switch for calculation of tree roughness, this option can only be calculated in combination
!           with equivalent sand-roughnesses in DARCY-WEISBACH equation
      IF (ID(1:6) == 'VEGETA') THEN
        IVEGETATION = 1
        call ginpt(lin,id,dlin)
      ELSE
        IVEGETATION = 0
      END IF

      !use of energy elevation for application at weirs
      if (ID(1:6) == 'ENERGY') then
        call ginpt(lin,id,dlin)
      endif
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,sep06: Add switch for approach of boundary-condition-transformation
      IF (ID(1:6) == 'KAL_BC') THEN
        BSWIT = 2
        call ginpt(lin,id,dlin)
      ELSE
        BSWIT = 1
      END IF
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NMAT=0

      ElementCharacteristics: Do
        IF(ID(1:3) == 'ED1') THEN
          READ(DLIN,5030) J,(ORT(J,K),K=1,7)
          write(*,*) 'read ed1'

          IF(NMAT < J) NMAT=J
CIPK NOV97      READ(LIN,7000) ID,DLIN
          call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          IF(ID(1:3) == 'ED2') THEN
cipk nov97  add extra friction
CIPK NOV98 ADD SURFACE FICTION
            READ(DLIN,5031) (ORT(J,K),K=8,14)
            write(*,*) 'read ed2'
cipk mar98 
            if(ort(j,12) == 0.) ort(j,12)=1.
CIPK NOV97      READ(LIN,7000) ID,DLIN
            call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk dec03 add element dependence IEDSW
            IF(ID(1:3) == 'ED3') THEN
              READ(DLIN,5032) IT1,TT1,TT2
              call ginpt(lin,id,dlin)
              IF(IEDSW < 0) THEN
                IEDSW1(J)=IT1
              ENDIF
              IF(TT1 > 0.) THEN
                TBFACT1(J)=TT1
              ENDIF
              IF(TT2 > 0.) THEN
                TBMIN1(J)=TT2
              ENDIF
            ELSEIF(IEDSW < 0) THEN
cipk sep04
              CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(75,*) 'ERROR -- EXPECTED ED3 DATA LINE'
              WRITE(*,*) 'ERROR -- EXPECTED ED3 DATA LINE'
              STOP
            ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,apr06: add element type specifications for equivalent sand roughness and vegetation calculation
            IF (ID(1:3) == 'ED4') THEN
              !comment:
              !As IPK suggested, the Darcy coefficient is used, if the ED4-line is present. If not, then the other approach is applied
              !It must be only checked for vegetation, that Darcy approach is used.
              !read parameters for vegetation and roughness of Darcy approach
              READ (DLIN, 5033) (ORT (J, K), K = 15, 17)
              !reset Manning's N or Chezy coefficient
              ort (j, 5) = -1.0
              call ginpt(lin,id,dlin)

            ELSEIF (IVEGETATION == 1 .AND. ID(1:3) /= 'ED4') THEN
              CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(75,*) 'ERROR -- EXPECTED ED4 DATA LINE'
              WRITE(*,*) 'ERROR -- EXPECTED ED4 DATA LINE'
              STOP 'LOOKING FOR ED4 LINE'
            ENDIF
          ELSE
cipk sep04
            CLOSE(75)
            OPEN(75,FILE='ERROR.OUT')
            WRITE(75,*) 'ED2 LINE MISSING'
            STOP 'LOOKING FOR ED2 LINE'
          ENDIF
        else
          EXIT ElementCharacteristics
        ENDIF
      ENDDO ElementCharacteristics

      IF(NMAT == 0) THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(75,*) 'ERROR -- NO ELEMENT CHARACTERISTICS READ', j
        WRITE(*,*) 'ERROR -- NO ELEMENT CHARACTERISTICS READ', j
        STOP
      ENDIF
      WRITE(LOUT,6030)
      DO J=1,NMAT
        IF(ORT(J,1) /= 0.) THEN
cipk mar98 change limit to 12
cipk nov98 change limit to 13

!NiS,apr06: increased number of field entries, because of DARCY-WEISBACH-equation and tree roughness
!          WRITE(LOUT,6031) J,(ORT(J,K),K=1,14)
          WRITE(LOUT,6031) J,(ORT(J,K),K=1,17)
!-
        ENDIF
      ENDDO

CIPK DEC03 OUTPUT ED3 LINE
      IF(IEDSW == -1) THEN
        WRITE(LOUT,6043)
        DO  J=1,NMAT
          IF(ORT(J,1) /= 0) THEN
            WRITE(LOUT,6044) J,IEDSW1(J),TBFACT1(J),TBMIN1(J)
          ENDIF
        ENDDO 
      ELSE
        IT1=0
        DO J=1,NMAT
          IF(ORT(J,1) /= 0) THEN
            IF(TBFACT1(J) /= 0. .OR. TBMIN1(J) /= 0.) THEN
              IF(IT1 == 0) THEN
                     WRITE(LOUT,6045)
                IT1=1 
              ENDIF
              WRITE(LOUT,6046) J,TBFACT1(J),TBMIN1(J)
            ENDIF
          ENDIF
        ENDDO
      ENDIF


cipk dec03     Copy to all element types

      IF(IEDSW >= 0) THEN
        DO N=1,NMAT
          IEDSW1(N)=IEDSW
          IF(TBFACT1(N) == 0.) TBFACT1(N)=TBFACT
          IF(TBMIN1(N) == 0.) TBMIN1(N)=TBMIN
          IF(TBFACT1(N) == 0.) TBFACT1(N)=0.2
        ENDDO
      ENDIF
             

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
Cipk mar01  add data for variable Manning n and drag force
c
      N=0
CIPK SEP04
      NH=0
      NH1=0
   35 CONTINUE
      IF(id(1:3) == 'MAN') THEN
       READ(DLIN,'(I8,4F8.0)') J,ELMMIN(J),MANMIN(J),ELMMAX(J),MANMAX(J)
        IF(N == 0) THEN
          WRITE(LOUT,6032)
          N=1
        ENDIF
        WRITE(LOUT,6033) J,ELMMIN(J),MANMIN(J),ELMMAX(J),MANMAX(J)
        CALL GINPT (LIN,ID,DLIN)
        GO TO 35

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK SEP04  ADD MAH and MAT OPTIONS
      ELSEIF(id(1:3) == 'MAH') THEN
        READ(DLIN,'(2I8,4F8.0)') J,IMAN(J),(HMAN(J,K),K=1,4)
        IF(IMAN(J) == 1) THEN
          HMAN(J,1)=0.02
          HMAN(J,2)=2.0
          HMAN(J,3)=0.026
          HMAN(J,4)=0.08
        ELSEIF(IMAN(J) == 2) THEN
          HMAN(J,1)=0.04
          HMAN(J,2)=4.0
          HMAN(J,3)=0.040
          HMAN(J,4)=0.166667
        ELSEIF(IMAN(J) == 3) THEN
          HMAN(J,1)=0.04
          HMAN(J,2)=2.0
          HMAN(J,3)=0.040
          HMAN(J,4)=0.166667
        ENDIF
        IF(NH == 0) THEN
          WRITE(LOUT,6052)
          N=1
        ENDIF
        WRITE(LOUT,6053) J,IMAN(J),(HMAN(J,K),K=1,4)
        CALL GINPT (LIN,ID,DLIN)
        GO TO 35

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ELSEIF(id(1:3) == 'MAT') THEN
       READ(DLIN,'(I8,8F8.0)') J,(MANTAB(J,K,1),MANTAB(J,K,2),K=1,4)
        IF(NH1 == 0) THEN
          WRITE(LOUT,6054)
          N=1
        ENDIF
        WRITE(LOUT,6055) J,(MANTAB(J,K,1),MANTAB(J,K,2),K=1,4)
        CALL GINPT (LIN,ID,DLIN)
        GO TO 35

      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      N=0
   36 CONTINUE
      IF(id(1:3) == 'DRG') THEN
        READ(DLIN,'(I8,4F8.0)') J,DRAGX(J),DRAGY(J)
        IF(N == 0) THEN
          WRITE(LOUT,6041)
          N=1
        ENDIF
        WRITE(LOUT,6042) J,DRAGX(J),DRAGY(J)
        CALL GINPT (LIN,ID,DLIN)
        GO TO 36
      ENDIF

C
cipk feb01 reading of continuity lines moved to GETGEO
C-

!NiS,mar06: In the case of Kalypso-2D model input data, the continuity line block
!           is put back to this place; the lines are ignored if the geometry is in
!           another format. In former versions of RMA10S, the continuity lines were
!           read for every model-format at this point. (I guess)
!
!           The block starts with SCL and ends with ECL, so that the code recognizes
!           a continuity-line block

      !nis,jun07: line array was never initialized, so it is done here now
      do i = 1, 50
        do j = 1, 3535
          line (i,j) = 0
        end do
      end do
      !-

      if (ifile == 60) then
        !if continuity line block is present
        if (id == 'SCL') then
          !read number of continuity lines
          !EFa sep08, added CCLout for output of discharge and waterstage at all continuity lines
          !READ(DLIN,*) NCL
          READ(DLIN,'(i4,i12)') NCL,CCLout
          !-
          !allocate continuity line objects
          allocate (ccls(1:ncl))
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
!          kmax = NCL
!          !EFa jun07, necessary for autoconverge
!          ALLOCATE (speccc(kmax,8))
!          ALLOCATE (specccold(kmax,8))
!          ALLOCATE (specccfut(kmax,8))
!          do i = 1, kmax
!            do k=1,8
!              speccc(i,k) = 0.0
!              specccold(i,k) = 0.0
!              specccfut(i,k) = 0.0
!            end do
!          end do
!----------------------------------------------------------------
!AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE AUTOCONVERGE
!----------------------------------------------------------------
          !if there is at least one continuity line
          IF (NCL > 0) THEN
            !read on input file
            read (filecontrol.lin.unit, '(a)') inputline
            !READ(lin,'(A3,A5,A72)') ID, IDString ,DLIN
            all_CL: do k = 1, ncl
              nl = 1
              !throw error, if there's no continuity line block
              if (inputline(1:3) /= 'CC1') 
     #          call ErrorMessageAndStop (1404, k, 0.0d0, 0.0d0)

              !read first continuity line definition line
              read (inputline (4:8), *) i
              read (inputline (9:), '(9i8)') (line (i, j), j = 1, 9)

              !add continuity line ID
              tmp_singleCCL => ccls (i)
              call addID (tmp_singleCCL, i)

              !check for zero entry in first definition position
              if (line (i, 1) == 0)
     +          call ErrorMessageAndStop (1405, i, 0.0d0, 0.0d0)


              !Read in the following nodes (CC2 lines)
              endless: do
                read(filecontrol.lin.unit,'(a)') inputline
                if (inputline(1:3) /= 'CC2') exit endless
                nl = nl + 9
                read (inputline(9:),'(9i8)') (line (i, j), j = nl, nl+8)
              enddo endless

              !look for positive normal side definition
              if (inputLine(1:3) == 'CPN') then
                read (inputLine(4:), *) tmp_singleCCL.posNormal(1),
     +                                  tmp_singleCCL.posNormal(2)
                !read next line
                read (filecontrol.lin.unit, '(a)') inputLine
              endif
              
              !Settle continuity line to be an inner boundary
              if (inputLine(1:3) == 'ICL') then
                tmp_singleCCL.isInnerBoundary = .true.
                call addInnerBC (ccl = tmp_singleCCL)
                read (inputLine (4:), *) 
     +                            tmp_singleCCL.innerCondition.globalID,
     +                            tmp_singleCCL.innerCondition.BCtype
                !read next line
                read (filecontrol.lin.unit, '(a)') input    Line
              endif
              
              lmt_loop: do j = min (nl + 9, 3535), 1, -1
                if (line (i, j) /= 0) then
                  lmt (i) = j
                  exit lmt_loop
                endif
              enddo lmt_loop
              
              assignNodes: do j = 1, lmt(i)
                if (line (i, j) == 0) exit assignNodes
                tmpNode => newLinkedNode
     +            (line (i, j), cord (line (i, j), 1),
     +             cord (line (i, j), 2))
                call addNode (tmp_singleCCL, tmpNode)
              enddo assignNodes

            !HN JUNE2009,------------------------------------------------------------------
            ! BUILDING PROFILES OUT OF CONTILINES OR JOINING BOTH.             
            ! LOOKING FOR CORRESPONDING PROFILE ASSOCIATED TO THE CURRENT CONTILINE.

      PRFID:if (inputLine(1:3) == 'PRF') then
               ! IF BANKEVOLUTION HAS NOT BEEN ACTIVATED AT THE BEGINNIG, WHILE NO PROFILE DATA
               ! IS AVAILABLE THEN ACTIVATE IT HERE FOR THE CASE THAT CONTILINES ARE THEMSELVES
               ! BANK PROFILES.

               read (inputLine(4:), *)ProfileID
               ! tmp_singleCCL.ProifileID => ProfileIDD
               ! case of restart profiles
       IF731:  IF ( IPROFIN /= 731) THEN
     
               BANKEVOLUTION = .TRUE.
               tmp_singleCCL.HasProfile = .TRUE.
     
            ID0:IF ( ProfileID > 0 ) THEN
               
                 tmp_singleCCL.MorphoProfile =>BANKPROFILES(ProfileID)
                 BANKPROFILES(ProfileID).CL_NUMBER = i
               
                 WRITE (276,'(2(2x,i2),2x,I4,2x,L3)') tmp_singleCCL.ID,
     +                  tmp_singleCCL.MorphoProfile.cl_number, 
     +                  tmp_singleCCL.MorphoProfile.max_nodes,
     +                  tmp_singleCCL.HasProfile
                 WRITE (*,'(2(2x,i2),2x,I4,2x,L3)') tmp_singleCCL.ID,
     +                  tmp_singleCCL.MorphoProfile.cl_number, 
     +                  tmp_singleCCL.MorphoProfile.max_nodes,
     +                  tmp_singleCCL.HasProfile

               !    do j =1,lmt(i)
                   do j =1,BANKPROFILES(ProfileID).max_nodes
                    WRITE (276,1013)  
     +              tmp_singleCCL.MorphoProfile.prnode(j).fe_nodenumber,
     +              tmp_singleCCL.MorphoProfile.prnode(j).distance,
     +              tmp_singleCCL.MorphoProfile.prnode(j).elevation,
     +              tmp_singleCCL.MorphoProfile.prnode(j).attribute
 1013               format (1X,I6,3X, 2(F8.4,3X), A9) 
                    WRITE (*,1013)  
     +              tmp_singleCCL.MorphoProfile.prnode(j).fe_nodenumber,
     +              tmp_singleCCL.MorphoProfile.prnode(j).distance,
     +              tmp_singleCCL.MorphoProfile.prnode(j).elevation,
     +              tmp_singleCCL.MorphoProfile.prnode(j).attribute
      
                   enddo
      
                ELSEIF ( .NOT. ALLOCATED(BANKPROFILES) ) THEN    ID0 
                  ProfileCounter = ProfileCounter + 1             ! counting profiles when all PRF = 0,and 
                END IF   ID0                                      ! there is no profile data file.
      
               ENDIF   IF731
               
               read (filecontrol.lin.unit, '(a)') inputLine
      
            END IF PRFID
             
            !-------------------------------------------------------------------------
             ENDDO all_CL
 
          close (276)  !HN, May2009
 
          !The bankprofile allocation is here in the  case of ProfileId= 0 (for all CCL)and bankevolution = .TRUE.

       IF( (BANKEVOLUTION) .AND. ( .NOT. ALLOCATED(BANKPROFILES) ) )THEN
              ALLOCATE (BANKPROFILES ( ProfileCounter),stat=ISTAT )
              ProfileCounter = 1                 ! To be used later(after getcoords loop) as counter for profiles with ID=0
              IF (ISTAT/=0) THEN
                WRITE (*,*) 'FAILURE BY ALLOCATION OF PROFILE ARRAY IN 
     +                            SUBROUTINE INPUT....'
                stop
              END IF
           
            ENDIF         
    !      endif
            IF(inputLine(1:3)=='ECL') THEN
              write ( *,6901)
              write (75,6901)
              CALL GINPT(lin,ID,DLIN)
            ELSE
              CLOSE(75)
              OPEN(75,File='ERROR.dat')
              write ( *,6803)
              write (75,6803)
              stop
            ENDIF
          ELSE
            WRITE( *,6902)
            WRITE(75,6902)
            CALL GINPT(lin,ID,DLIN)
          ENDIF
        ELSE
          WRITE( *,6902)
          WRITE(75,6902)
        ENDIF

!NiS,mar06: For the case of non-Kalypso-2D-format but nevertheless a
!           continuity line block being entered, this should be told to
!           the user and jumped over.
      ElSE
        IF(ID == 'SCL') THEN
!NiS,mar06: Just an information message
          write ( *,6906)
          write (75,6906)
          DO WHILE (ID/='ECL')
            istat_temp = 0
            READ(lin,'(A3)',iostat = istat_temp)ID
            if (istat_temp /= 0) then
              close (75)
              OPEN(75,File='ERROR.dat')
              WRITE( *,6804)
              WRITE(75,6804)
              stop
            end if
          END DO
          CALL GINPT(lin,ID,DLIN)
        ENDIF
      ENDIF

!NiS,mar06 write control

      WRITE( *,6903)NCL
      WRITE(75,6903)NCL
      lines_schreiben: do i=1,NCL
        IF(LINE(NCL,1)==0) CYCLE lines_schreiben
        WRITE( *,6904)I,LINE(i,1),LINE(i,LMT(i))
        WRITE(75,6904)I,LINE(i,1),LINE(i,LMT(i))
      ENDDO lines_schreiben
      WRITE( *,6905)
      WRITE(75,6905)
!NiS,mar06:     format descriptors for information messages; error messages at
!               the end of this subroutine
 6901 FORMAT(5x,'--- End of continuity line input block! ---')
 6902 FORMAT(5x,'--- no continuity lines defined! ---')
 6903 FORMAT(/
     +       5x,'-----------------------------------',/
     +       5x,'Control-output:',/
     +       5x,'max ID-No. ',I3,/
     +       5x,'continuity lines.')
 6904 FORMAT(5x,'ID: ',I6,', 1.: ',I6,', last: ',I6)
 6905 FORMAT(5x,'-----------------------------------')
 6906 FORMAT(5x,'---------------------------------',/
     +       5x,'Continuity line definition in',/
     +       5x,'in control file only allowed for',/
     +       5x,'Kalypso-2D input format!',/
     +       5x,'continued after CL-Block!',/
     +       5x,'---------------------------------')
      WRITE(*,*)'Just informational: ', id(1:6)
!NiS,mar06: End of CONTINUITYLINEBLOCK-



CIPK SEP96 ADD OCEAN BOUNDARY NODE LIST

      N1=-8
  40  N1=N1+9
      N9=N1+8
      IF(ID(1:2) == 'OB') THEN
        READ(DLIN,'(9I8)') (IRBD(I),I=N1,N9)
        DO I=N1,N9
          IF(IRBD(I) == 0) THEN
            NRBD=I
            GO TO 45
          ENDIF
          NRBD=I
        ENDDO
CIPK NOV97      READ(LIN,7000) ID,DLIN
        call ginpt(lin,id,dlin)
        GO TO 40
      ELSE
        GO TO 47
      ENDIF
CIPK NOV7   45 READ(LIN,7000) ID,DLIN
   45 call ginpt(lin,id,dlin)
   47 CONTINUE
C
CIPK OCT96 END ADDITION
C-
C-.....READ GENERATED GEOMETRY DATA
C-
   
      CALL GETGEO (m_SimModel)
      
      !get coordinates of the continuity line nodes
      getCoords: do i = 1, ncl
        !get contiLine
        tmp_singleCCL => ccls(i)
        if (associated (tmp_singleCCL.firstNode)) then
          tmpNode => tmp_singleCCL.firstNode
          if (associated (tmpNode.next)) then
            assignCoords: do 
              call setCoords (tmpNode.thisNode,
     +          cord (tmpNode.thisNode.ID, 1:2),
     +          ao(tmpNode.thisNode.ID))
              if ( .NOT. (associated (tmpNode.next))) exit assignCoords
              tmpNode => tmpNode.next
            enddo assignCoords
            call setChordNormal (tmp_singleCCL)
            call calcSegments (tmp_singleCCL)
            call assignSegPosNormals (tmp_singleCCL)
          else
            elt = isNodeOfElement (tmpNode.thisNode.ID, 1)
            node1D_first => newNode (nop (elt,1), cord (nop (elt,1), 1),
     +                             cord (nop (elt, 1), 2))
            node1D_last => newNode (nop (elt,3), cord (nop (elt,3), 1),
     +                            cord (nop (elt,3), 2))
            arc1D => newArc (node1D_first, node1D_last)
            arcVec = arcVector (arc1D)
            !get direction pointer
            if (ccls(i).posNormal(1) == 0.0d0 .AND. 
     +          ccls(i).posNormal(2) == 0.0d0) then
              dirPointer = 1.0d0
            else
              dirPointer = projectionDirPointer
     +                     (arcVec, ccls(i).posNormal)
            endif 
            ccls(i).posNormal = arcVec
            call scaleToUnitVector (ccls(i).posNormal, dirPointer)
          endif
        endif
        
      enddo getCoords
 !-------------------------------------------------------------------------------     
      ! HN JUNE2009. HERE IS THE FIRST PLACE THAT COORDINATES HAVE BEEN ASSIAGNED TO CORNER NODES IN CONTILINES.
 !        IF ( ProfileID == 0 ) THEN
        IF (( bankevolution ) .AND. (IPROFIN/= 731) ) THEN

        inquire (file = 'PROFILE_CONTI.TXT', EXIST= exists)
    
        if (exists) then

         OPEN( UNIT =276 , FILE = 'PROFILE_CONTI.TXT', STATUS ='OLD',
     +   ACTION ='WRITE', POSITION = 'APPEND')
        else
        
        OPEN( UNIT =276 , FILE = 'PROFILE_CONTI.TXT', STATUS ='REPLACE',
     +   ACTION ='WRITE')
     
        end if

          DO n = 1, ncl
         
           IF( (ccls(n).HasProfile) .AND. 
     +          ( .NOT. associated( ccls(n).MorphoProfile)) ) THEN
             tmp_CCL => ccls(n)
             write (*,*) 'CCL ', n, 'lmt(n) ', lmt(n)
             write (*,*) ' entering into makeprofile subroutine...'
             CALL MAKEPROFILE (tmp_CCL,lmt(n),
     +                          BANKPROFILES( ProfileCounter ),.FALSE. )
   !          tmp_singleCCL.MorphoProfile =>BANKPROFILES(ProfileCounter)
             tmp_CCL.MorphoProfile =>BANKPROFILES(ProfileCounter)
             BANKPROFILES(ProfileCounter).CL_number = n
             ProfileCounter = ProfileCounter + 1
           END IF

           nextnode => CCLs(n).firstnode
           do j =1,lmt(n)
            if (ccls(n).HasProfile) then
             distt = sqrt( ( NextNode.ThisNode.cord (1) -           
     +                       CCLs(n).FirstNode.ThisNode.cord (1) ) **2 
     +                        +  ( NextNode.ThisNode.cord (2) -     
     +                        CCLs(n).FirstNode.ThisNode.cord (2) )**2 )

             write (276,5999) nextnode.thisnode.ID,
     +        NextNode.ThisNode.cord (1)
     +       ,NextNode.ThisNode.cord (2), distt, Nextnode.thisnode.ao
5999         Format (I5,2(2x,F12.4),2(2x,F7.4)) 
             nextnode =>nextnode.next
            end if 
           end do
             
          END DO
         CLOSE (276)
        END IF
         
 
!-----------------------------------------------------------------------------------
!NiS,apr06: In the case of ORT(J,5)==-1.0, the parameters are given to the arrays:
!           At this point the IMAT-array is not modified by additions for element
!           type pointer. These changes are made in threed.subroutine, which is
!           started below.
      !NiS,may06: testing
      !OPEN(UNIT=357, FILE='roughnesstest.txt')
      Materialassigning: DO J=1,MaxE
        if (IMAT(J) == 0 .OR. imat (j) == 89) CYCLE Materialassigning
        CNIKU(J)     = ORT(ABS(IMAT(J)),15)
        ABST(J)      = ORT(ABS(IMAT(J)),16)
        DURCHBAUM(J) = ORT(ABS(IMAT(J)),17)
      END DO Materialassigning
      !close (357,STATUS='keep')
!-

CIPK JUN02 GET LIST OF ACTIVE NODES INCLUDING NODES WITH ORT NON ZERO
      
      DO N=1,NP
        IBNA(N)=0
      ENDDO
      DO N=1,NE
        IF(IMAT(N) > 0 .AND. imat (n) /= 89) THEN
          IF(ORT(IMAT(N),1) /= 0.) THEN
            DO K=1,NCORN(N)
              IBNA(NOP(N,K))=IBNA(NOP(N,K))+1
            ENDDO
          ENDIF
        ENDIF
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN02   IBNA IS NOW ZERO FOR INACTIVE NODES
C-
C-..... Read reorder array if there is input
C-
      tofday=tet
      IF(ID(1:2) == 'RO') THEN
        READ(DLIN,5010) (NFIXH(J),J=1,9)
CIPK JAN99
        IF(NE <= 9) THEN
          call ginpt(lin,id,dlin)
          GO TO 71
        ENDIF
CIPK JAN99 END ADDITION  
        N1=1
   70   N1=N1+9
        N2=N1+8
        IF(N2 > NE) N2=NE
CIPK NOV97        READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(ID(1:3) == 'RO ') THEN
          READ(DLIN,5010) (NFIXH(J),J=N1,N2)
          IF(N2 < NE) GO TO 70
        ELSE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(75,*) 'ERROR -- READING REORDERING DATA RO1'
          WRITE(*,*) 'ERROR -- READING REORDERING DATA RO1'
          STOP
        ENDIF
CIPK NOV97        READ(LIN,'(A8,A72)') ID,DLIN
      call ginpt(lin,id,dlin)
      ENDIF

C-
C......FILL REORDERING ARAAY
C-
CIPK JAN99 ADD A LINE
   71 CONTINUE
      IF(NFIXH(1) == 0) NFIXH(1)=1
      DO N=2,NE
        IF(NFIXH(N) == 0) NFIXH(N)=NFIXH(N-1)+1
      ENDDO
C-
C...... Preliminary initialisation of VEL(3,  ) for BLINE
C-
CIPK NOV97 DEVELOP CONSISTENT DEPTH 
      DO J=1,NPM
CIPK DEC99 CHANGE TO ELEV1
        HEL(J)=ELEV1-ADO(J)
        CALL AMF(HEL(J),HTP,AKP(J),ADT(J),ADB(J),D1,D2,1)
          VEL(3,J)=HTP
        IF(VEL(3,J) < HMIN) VEL(3,J)=HMIN
        VOLD(3,J)=VEL(3,J)
        HOL(J)=HEL(J)
CIPK NOV97      VEL(3,J)=ELEV-AO(J)
CIPK NOV97      IF(VEL(3,J) < HMIN) VEL(3,J)=HMIN
CIPKNOV97       IF(HMNN < 0.) VEL(3,J)=-HMNN
      ENDDO
   80 CONTINUE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-
C-.....READ EXTERNAL SLOPE SPECS.....
C-
   81 CONTINUE
      IF(ID(1:2) == 'SL') THEN
        READ(DLIN,5029) N,ALFAK(N)
        ALFA(N)=ALFAK(N)
        WRITE(LOUT,6035) N,ALFAK(N)
CIPK NOV97        READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)
        GO TO 81
      ENDIF
C-

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C  Read fixed salinity nodes and values
C
      NFXSAL = 0
   82 CONTINUE
      IF (ID(1:2) == 'SA')  THEN
        READ(DLIN,'(3F8.0)') SALBC
CIPK NOV97        READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)
        IF(ID(1:3) == 'SA1') THEN
          WRITE(LOUT,6036) SALBC
          READ(DLIN,5010) (IFXSAL(K),K=1,9)
          WRITE(LOUT,5010) (IFXSAL(K),K=1,9)
CIPK NOV97          READ(LIN,'(A8,A72)') ID,DLIN
          call ginpt(lin,id,dlin)
          DO K = 1, 9
            N = IFXSAL(K)
            IF (N == 0 .OR. N > 9990)  GOTO 82
            NFIX(N) = (NFIX(N)/100)*100 + 11
            NFIX1(N)=1
            SPEC(N,4) = SALBC(1)
            SPEC(N,5) = SALBC(2)
            SPEC(N,6) = SALBC(3)
            NFXSAL = NFXSAL + 1
          ENDDO
        ELSE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR  EXPECTED LINE TYPE -SA1-'
          WRITE(75,*) 'ERROR  EXPECTED LINE TYPE -SA1-'
          STOP
        ENDIF
        N1=1
   83   N1=N1+9
        N9=N1+8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(ID(1:3) == 'SA1') THEN
          READ(DLIN,5010) (IFXSAL(K),K=N1,N9)
          WRITE(LOUT,5010) (IFXSAL(K),K=N1,N9)
CIPK NOV97          READ(LIN,7000) ID,DLIN
          call ginpt(lin,id,dlin)
          DO K= N1,N9
            N = IFXSAL(K) 
            IF (N == 0 .OR. N > 9990)  GOTO 82
            NFIX(N) = (NFIX(N)/100)*100 + 11
            NFIX1(N)=1
            SPEC(N,4) = SALBC(1)
            SPEC(N,5) = SALBC(2)
            SPEC(N,6) = SALBC(3)
            NFXSAL = NFXSAL + 1
          ENDDO
          GO TO 83
        ENDIF
        GO TO 82
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C  Read straight line mid-side nodes
C
      IF(ID(1:3) == 'ST1') THEN                               !NiS,mar06,comment:
        READ(DLIN,5010) (IMIDD(K),K=1,9)                        !
        WRITE(LOUT,6037) (IMIDD(K),K=1,9)                       !This part of the code does not do what the handbook says. Therefore see pages
CIPK NOV97        READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)
C
        DO  II=1,9
          N=IMIDD(II)
          IF(N > 0) NSTRT(N,2)=1
        ENDDO
        N1=1
  116   N1=N1+9
        N9=N1+8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF(ID(1:3) == 'ST1') THEN
          READ(DLIN,5010) (IMIDD(K),K=N1,N9)
          WRITE(LOUT,5010) (IMIDD(K),K=N1,N9)
CIPK NOV97      READ(LIN,7000) ID,DLIN
          call ginpt(lin,id,dlin)
          DO K= N1,N9
            N = IMIDD(K) 
            IF(N > 0) NSTRT(N,2)=1
          ENDDO
          GO TO 116
        ENDIF

      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C  Read Coefficients for salinity distribution at 2-d 3-d junctions
C
       DO  I=1,NP
         ICPON(I) = 1
       ENDDO
       CINT(1) = CMIN
       CPOW(1) = CPR 
C
  192 CONTINUE
      IF (ID(1:2) == 'CP')  THEN
        READ(DLIN,'(I8,2F8.0)') J,CINT(J),CPOW(J)
        WRITE(LOUT,6038) CINT(J),CPOW(J)
CIPK NOV97        READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C  Read list of nodes and associated coefficient number for salinity distr.
C
        IF(ID(1:3) == 'CP1') THEN
          READ(DLIN,5010) (IMIDD(K),K=1,9)
          WRITE(LOUT,5010) (IMIDD(K),K=1,9)                                     !NiS,mar06,comment:
CIPK NOV97          READ(LIN,'(A8,A72)') ID,DLIN                                !This part of the code does not do, what the handbook says. See
      call ginpt(lin,id,dlin)                                                   !therefore page 42 in the handbook release of September 2005
          DO K = 1, 9
            N = IMIDD(KK)
            IF(N > 0) ICPON(N)=J
          ENDDO
        ELSE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR  EXPECTED LINE TYPE -CP1-'
          WRITE(75,*) 'ERROR  EXPECTED LINE TYPE -CP1-'
          STOP
        ENDIF
        N1=1
  193   N1=N1+9
        N9=N1+8
        IF(ID(1:3) == 'CP1') THEN
          READ(DLIN,5010) (IMIDD(K),K=N1,N9)
          WRITE(LOUT,5010) (IMIDD(K),K=N1,N9)
CIPK NOV97          READ(LIN,7000) ID,DLIN
          call ginpt(lin,id,dlin)
          DO K= N1,N9
            N = IMIDD(K) 
            IF(N > 0) ICPON(N)=J
          ENDDO
          GO TO 193
        ENDIF
        GO TO 192
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      IF(ID(1:6) /= 'ENDGEO') THEN
cipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(*,*) 'ERROR AT END OF GEOMETRIC INPUT NO -ENDGEO- FOUND'
        WRITE(75,*) 'ERROR AT END OF GEOMETRIC INPUT NO -ENDGEO- FOUND'
        STOP
      ENDIF



      CALL INITSED(IBIN)


CIPK MAY01

      IF(LSAND /= 0 .OR. LBED /= 0) CALL INSAND

      IF(LSS /= 0) CALL SPROP



C
C...... Initialize CHECK
C-
      CALL CHECK
               
 
! HN June 2009
! IN THE CASE THAT THERE IS NO FILE FOR PROFILE DATA THEN MAKE PROFILES
! OUT OF CONTILINES DIRECTLY.
! ProfileID HAS BEEN INITIATED TO -1: IT EITHER GETS AN INTEGER NUMBER
! FROM CONTROLFILE REFERING TO THE PROFILE ID OR ZERO WHEN NO PROFILE DATA
! IS AVAILABLE AND CONTILINES SHOULD SERVE DIRECTLY AS PROFILES.  

! MIDSIDE NODES ARE CONSIDERED HERE IN CONSTRUCTING PROFILE NODES.
! THEY ARE INCLUDED IN CONTILINE AFTER CALLING SUBROUTINE CHECK.      
       if (middsidd) then
         if ((ProfileID == 0 ) .AND. ( IPROFIN/=731) ) then
          j = 0
         
          do n = 1, ncl
         
           if (ccls(n).HasProfile ) then
         
               tmp_singleCCL => ccls(n)
               j = j + 1
    
               write (*,*) ' entering into makeprofile subroutine...'
  
               CALL MAKEPROFILE (tmp_singleCCL,LMT(n),
     +                           BANKPROFILES( j ), .TRUE. )
    
               tmp_singleCCL.MorphoProfile =>BANKPROFILES(j)
 
           end if
          end do
         end if 
        
       end if 


!-  HN June 2009
     

      !TODO: This call is only for getting the water levels at Q-boundaries and transitions;
      !      The way of programming is not efficient!
      call getinit(ibin, 1, m_SimModel)
      
C-
C-.....INPUT BOUNDARY AND WIND DATA.....
C-
      write(*,*) 'going to getbc', ibin
      CALL GETBC(IBIN)

cipk jan99 set directions
       do n=1,ne
         if(imat(n) > 900) then
           n1=nop(n,1)
           if(ndep(n1) > 1) then

             !get midside node
             n2=nop(n,2)

             if(abs(alfa(n1)-alfa(n2)) > 1.570796 .AND. 
     +          abs(alfa(n1)-alfa(n2)) < 4.713388) then
                if(alfa(n1) > alfa(n2)) then
                  alfa(n2)=alfa(n2)+3.141592
                else
                  alfa(n2)=alfa(n2)-3.141592
                endif
             endif
           endif
         endif
       enddo

C
C...... Set up IBN for three dimensional element generation
C-
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove obsolete unit ifit
!ifit is obsolete
!      IF(IFIT > 0) GO TO 99
!-
      DO 86 I=1,NP
   86 IBN(I)=0
      DO 90 J=1,NE
        IF(IMAT(J) > 0) THEN
        IF(NETYP(J) == 17) GO TO 90
CIPK OCT98 CONVERT TO F90
          MTYP=IMAT(J)
          IF(MOD(MTYP,100) <= 90) THEN
            ILK=1
            NCN=NCRN(J)
            IF(NCN == 5) NCN=3
            DO 87 K=1,NCN
              N=NOP(J,K)
              IBN(N)=IBN(N)+1
   87       CONTINUE
          ELSE
C-
C...... SPIN ELEMENT CONNECTIONS TO ENSURE FIRST THREE NODES ARE ON
C       REAL BOUNDARY AND ARE SOME KIND OF SPECIFIED HEAD
C-
cipk sept 95 add definition for ncn
            NCN=NCRN(J)
            IF(NCN > 3) THEN
              DO 881 K=1,NCN,2
                L=NOP(J,K)
                IF(MOD(NFIX(L)/100,10) == 0) GO TO 881
                KL=MOD(K+2,NCN)
                KL=NOP(J,KL)
                IF(MOD(NFIX(KL)/100,10) == 0) GO TO 881
                KK=K-1
                GO TO 882
  881         CONTINUE
cipk sep04
              CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(*,6502) J,(NOP(J,K),K=1,NCN)
              WRITE(75,6502) J,(NOP(J,K),K=1,NCN)
 6502         FORMAT(' ***ERROR*** OUTSIDE BOUNDARY ELEMENT NO',I6,
     *        '   WITH CONNECTIONS ',8I5/'     HAD IMPROPER NFIX')
              STOP 'ERROR'
  882         DO 884 K=1,NCN
                IM(K)=NOP(J,K)
  884         CONTINUE
              DO 886 K=1,NCN
                KK=KK+1
                KL=MOD(KK,NCN)
                IF(KL == 0) KL=NCN
                NOP(J,K)=IM(KL)
                NOPS(J,K)=IM(KL)
  886         CONTINUE
              NBL=4
            ELSE
              NBL=2
              L=NOP(J,1)
              IF(MOD(NFIX(L)/100,10) /= 0) GO TO 887
              KL=NOP(J,3)
cbm july95 fix error reference        IF(MOD(NFIX(L)/100,10) /= 0) THEN
              IF(MOD(NFIX(KL)/100,10) /= 0) THEN
                NOP(J,3)=L
                NOPS(J,3)=L
                NOP(J,1)=KL
                NOPS(J,1)=KL
                GO TO 887
              ENDIF
cipk sep04
              CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(*,6502) J,(NOP(J,K),K=1,NCN)
              WRITE(75,6502) J,(NOP(J,K),K=1,NCN)
              STOP 'ERROR'
  887         CONTINUE
            ENDIF
            DO 89 K=NBL,NCN
              L=NOP(J,K)
              IBN(L)=1
   89       CONTINUE
          ENDIF
        ENDIF
   90 CONTINUE
C-
C...... reset IBN to NETYP=18
C-
      DO 91 N=1,NE
        IF(NETYP(N) == 18) THEN
          J=NOP(N,19)
CZZZ 
          if (j > 0)  IBN(J)=2
          J=NOP(N,3)
          if (j > 0)  IBN(J)=2
        ENDIF
   91 CONTINUE
      DO  98 I=1,NP
      IF(IBN(I) /= 1) GO TO 95
      IF(NFIX(I)/ 1000 == 11) GO TO 95
      IBN(I)=1
      IF(MOD(NFIX(I), 1000)/100 == 2) IBN(I)=2
      IF(NFIX(I)/ 1000 == 31) IBN(I)=3
      GO TO  98
   95 IBN(I)=0
   98 CONTINUE
C
C  Calculate 2-D Eddy Viscosity Coef
C  Initialize viscosity array
C
      DO 141  N=1,NE
         IF (IMAT(N) /= 0 .AND. imat(n) /= 89)  THEN
cipk dec00          IF (IMAT(N) >= 900) GO TO 141
            I = abs(IMAT(N))
            EEXXYY(1,N) = ORT(I,1)
            EEXXYY(2,N) = ORT(I,2)
            EEXXYY(3,N) = ORT(I,3)
            EEXXYY(4,N) = ORT(I,4)
            EEXXYY(5,N) = ORT(I,8)
            EEXXYY(6,N) = ORT(I,9)
         ELSE
            EEXXYY(1,N) = 0.
            EEXXYY(2,N) = 0.
            EEXXYY(3,N) = 0.
            EEXXYY(4,N) = 0.
            EEXXYY(5,N) = 0.
            EEXXYY(6,N) = 0.
         ENDIF
  141 CONTINUE
C
C....... Call ANGLEN to find major axis and scale velocity terms
C
      CALL ANGLEN

CZZZ
      CALL GETCON

C-
C-
C......FORM THREE DIMENSIONAL ELEMENTS FROM INPUT
C-
      CALL THREED !nis,comment
!In dependency of the switch NT, two general jobs are possible for file.subroutine
!NT = 1: First call (called from RMA10.program); the general files are read and opened
!NT = 2: Second call (called from input.subroutine); output file LOUT is opened

   99 CONTINUE

CIPK AUG00 SAVE GENERATED ORDER
      DO N=1,NE
        NELORD(N)=NFIXH(N)
      ENDDO

cipk OCT96 add logic to expand ocean boundaries to 3-d
      IF(NRBD > 0) THEN
        NRBDT=NRBD
        DO I=1,NRBD
          N=IRBD(I)
          IF(NDEP(N) > 1) THEN
            NL=NREF(N)+1
            NT=NL+NDEP(N)-2
            DO  M=NL,NT
              NRBDT=NRBDT+1
              IRBD(NRBDT)=M
            ENDDO
          ENDIF
        ENDDO
        NRBD=NRBDT
      ENDIF     
cipk OCT96 end changes
C-
C-
C-..... COMPUTE MID-SIDE VALUES.....
C-
      DO 102 J=1,NE
        IF(IMAT(J) == 0) GO TO 102
C     IF(IMAT(J) == 901) GO TO 102
C     IF(NCORN(J) == 3 .AND. IMAT(J) < 1000) NCORN(J)=NCRN(J)
        IF(NCORN(J) < 6 .AND. IMAT(J) < 901) NCORN(J)=NCRN(J)
        IF(IMAT(J) == 901) GO TO 102
        NCN=NCORN(J)
        IF(NCN < 6) THEN    
          IF(NCN == 5) NCN=3
          ILK=1
        ELSEIF(NCN == 6) THEN
          ILK=2
        ELSEIF(NCN == 15) THEN
          ILK=2
        ELSEIF(NCN == 10) THEN
          ILK=3
        ELSEIF(NCN == 13) THEN
          ILK=4
        ELSE
          ILK=1
        ENDIF

!nis,may07
!Add midside node for polynom approach
!The interpolation of missing data is also done in RDKalypso.subroutine. So this is skipped here for polynom approach data
        if (imat(j) == 89) then
        DO 101 K=1,NCN
          KL=IL(K,ILK)
CIPK SEP05
          N2=NOP(J,K)
          IF(KL == 0) THEN
            IF(N2 > 0) IMID(N2,1)=0
            GO TO 101
          ENDIF
          KH=IH(K,ILK)
          N1=NOP(J,KL)
          IF(N1 == 0) GO TO 101
          N3=NOP(J,KH)
          IMID(N2,1)=N1
          IMID(N2,2)=N3
          IF(WIDTH(N1) > 0. .AND. WIDTH(N3) > 0.) THEN
            WIDTH(N2)=0.5*(WIDTH(N1)+WIDTH(N3))
            SS1(N2)=0.5*(SS1(N1)+SS1(N3))
            SS2(N2)=0.5*(SS2(N1)+SS2(N3))
          ENDIF
          IF(CORD(N2,1) <= VOID) THEN
            CORD(N2,1)=0.5*(CORD(N1,1)+CORD(N3,1))
            CORD(N2,2)=0.5*(CORD(N1,2)+CORD(N3,2))
          ENDIF
  100     CORD(N2,3)=0.5*(CORD(N1,3)+CORD(N3,3))
          N1=NSURF(N1)
          N3=NSURF(N3)
          AO(N2)=0.5*(AO(N1)+AO(N3))
          ADO(N2)=0.5*(ADO(N1)+ADO(N3))
          ADT(N2)=0.5*(ADT(N1)+ADT(N3))
          ADB(N2)=0.5*(ADB(N1)+ADB(N3))
cipk aug98
            if(ndep(n1) > 0) then 
              akp(n2)=akp(n3)
              if(ndep(n3) == 0) akp(n1)=akp(n3)
            elseif(ndep(n3) > 0) then
              akp(n2)=akp(n1)
              if(ndep(n1) == 0) akp(n3)=akp(n1)
            else
            AKP(N2)=0.5*(AKP(N1)+AKP(N3))
            endif
cipk aug98 end changes
C
C    Define node connectors to N2 if surface node is defined straight
C
CIPK OCT98 CONVERT TO F90
          II=ABS(NSURF(N2))
          IF(NSTRT(II,2) /= 0) THEN
            NSTRT(N2,1)=KL
            NSTRT(N2,2)=KH
          ENDIF
  101   CONTINUE
      endif
  102 CONTINUE

CIPK SEP04  ENSURE VALUES AT ALL NODES
      DO N=1,NPM
          IF(NDEP(N) > 1) THEN
            N1=NREF(N)+1
            NV=NDEP(N)+N1-2
          DO M=N1,NV
          AO(M)=AO(N)
          ADO(M)=ADO(N)
          ADT(M)=ADT(N)
          ADB(M)=ADB(N)
          akp(M)=akp(n)
        ENDDO
        ENDIF
      ENDDO


c-
!REMOVE FOR RMA·KALYPSO
!nis,nov08: Remove writing to obsolete unit ifot
!ifot is obsolete
!-


C     HEADER  =   1000 character header with the label RMA103DG in loc 1-8

C      NP            =      I4      Number of nodes
C      NE            =      I4      Number of elements
C      NPM            =      I4      Number of surface nodes
C      NEM            =      I4      Number of surface elements
C      CORD      =      R8      Array of nodal cordinates                   (NP,3)
C      SPEC      =      R4      Array of specifie BC's                        (NP,3)
C      ALFA      =      R4      Array of boundary slopes                  (NP)
C      NFIX      =      I4      Array of boundary conditions                  (NP)
C      NFIX1      =      I4      Continuation array of BC's                  (NP)
C      AO            =      R8      Array of bed elevation                        (NP)
C      NSURF      =      I4      Array of surface node number above this node       (NP)
C      NDEP      =      I2      Array of number of nodes in vertical line below       (NPM)
C      NREF      =      I4      Array showing nodes below a fvien sirface node      (NPM)
C      NOP            =      I4      Array of nodes forming an element            (NE,20)
C      NCORN      =      I2      Array of number of nodes around an element      (NE)
C      IMAT      =      I2      Array of element characteristics number            (NE)
C      NETYP      =      I2      Array defining element type                  (NE)
C      TH            =      R4      Array of principal direction of element            (NE)
C      NFIXH      =      I4      Array containing element elimination order      (NE)
C      WIDTH      =      R4      Array of bed widths for 1-D nodes            (NP)

C       Note that the nodel number immediately below the surface node N is given by
C            ND       =      NREF(N)+1
C       and that the node at the bed is given by
C            NDB      =      NREF(N)+NDEP(N)-1

C       NETYP is an array that defines the type of each element
C           = 1   One dimensional  surface element (2d applications)
C           = 2   One dimensional  bottom  element (2d applications)
C           = 3
C           = 4   One dimensional  end     element (2d applications)
C           = 6   One dimensional          element (1d applications)
C           = 7   One dimensional junction element (1d applications)
C           = 8   One-two dimensional      transition element
C
C           = 11  Two dimensional  surface element (3d applications)
C           = 12  Two dimensional  bottom  element (3d applications)
C           = 13  Two dimensional  side    element (3d applications)
C           = 14  Two dimensional  end     element (3d applications)
C           = 15  Two dimensional          element (2d applications)
C           = 16  Two dimensional          element (2d applications)
C           = 17  Two dimensional junction element (2d applications)
C           = 18  Two-three dimensional    transition element
C
C           = 21   Three dimensional 20 point element
C           = 22   Three dimensional 15 point element
C           = 23   Three dimensional 13 point element
C           = 24   Three dimensional 10 point element

C     AN EXAMPLE OF A NETWORK IN CROSS-SECTION

C                  1-----------2-----------3-----------4-----------5
C                  |                       |                       |
C                  |                       |                       |
C                 11                      30                      41
C                  |                       |                       |
C                  |                       |                       |
C                 12----------20----------31----------38----------42
C                  |                       |                       |
C                  |                       |                       |
C                 13                      32                      43
C                  |                       |                       |
C                  |                       |                       |
C                 14----------21----------33----------39----------44
C                  |                       |                       |
C                  |                       |                       |
C                 15                      34                      45
C                  |                       |                       |
C                  |                       |                       |
C                 16----------22----------35----------40----------46

C     IN THIS EXAMPLE
C     NDEP(1)=7
C      NDEP(2)=4
C      NREF(1)=10
C      NREF(2)=19
C      NSURF(11)=1
C      NSURF(20)=2

C-
C-.....ASSIGN HIGHER ORDER TO CURVED ELEMENTS.....
C-
      DO 130 J = 1, NE
        IF( IMAT(J) == 0 .OR. IMAT(J) > 100 ) GO TO 130
        NCN = NCORN(J)
        IF(NCN == 20) NCN=8
        IF(NCN == 6 .OR. NCN == 8) THEN
          DO 125 K = 1, NCN, 2
            N1 = NOP(J,K)
            N2 = NOP(J,K+1)
            N3 = MOD(K+2,NCN)
            N3 = NOP(J,N3)
            XM = 0.5*( CORD(N1,1) + CORD(N3,1) )
            XD = SQRT((CORD(N1,1)-CORD(N3,1))**2+
     +                (CORD(N1,2)-CORD(N3,2))**2)
cipk dec99  revise test
          IF(XD > ABS(XM) ) XM=XD
            IF( ABS(XM-CORD(N2,1) ) < 0.005*ABS(XM) ) THEN
              YM = 0.5*( CORD(N1,2) + CORD(N3,2) )
              IF(XD > ABS(YM) ) YM=XD
              IF( ABS(YM-CORD(N2,2) ) < 0.005*ABS(YM) ) GO TO 125
            ENDIF

            IF(NCORN(J) < 20) THEN
              IMAT(J)=IMAT(J)+IHOE
              GO TO 130
            ENDIF
C-
C...... CHECK VERTICAL DIMENSIONS
C-
            DO 118 I=1,4
              N1=NOP(J,2*I-1)
              N2=NOP(J,2*I+11)
  118       VDIM(I)=CORD(N2,3)-CORD(N1,3)
            AVG=(VDIM(1)+VDIM(2)+VDIM(3)+VDIM(4))/4.
            DO 120 I=1,4
              IF(ABS(VDIM(I)-AVG) < 0.005) GO TO 120
              IMAT(J) = IMAT(J) + IHOE
              GO TO 130
  120       CONTINUE
  125     CONTINUE
        ENDIF
  130 CONTINUE
C
C..... GET VOLUMES FOR EACH ELEMENT
C
      DO  N=1,NE
        IF(IMAT(N) > 0) THEN

CIPK DEC03 ADD IEDSW DEPENDENCE

          NMATYP=(MOD(IMAT(N),1000))

          if (imat(n) /= 89) then
            IEDSW=IEDSW1(NMATYP)
            TBFACT=TBFACT1(NMATYP)
            TBMIN=TBMIN1(NMATYP)
          end if

          IF(IMAT(N) > 1000 .AND. IMAT(N) < 5000) THEN
            CALL SURCOF(N,0)
          ELSEIF(NETYP(N)/10 == 2) THEN
            CALL COEF3(N,0)
          ELSEIF(NETYP(N)/10 == 1) THEN
            IF(IMAT(N) < 900 .OR. 
     +    (IMAT(N) > 903 .AND. IMAT(N) < 1000) ) THEN
CIPK FEB07  ADD TO TEST
              CALL COEF2(N,0)
            ELSE
              CALL COEFV(N,0)
CIPKNOV97
              CALL COEFV(N,3)
            ENDIF

cipk dec00 allow for gate option

          ELSEIF((IMAT(N) < 900 .OR. 
!nis,may07
!Add midside node for polynom approach
      !nis,feb07: Allow for numbered FFF midsides and documentation, it wasn't documented before. This code is also questionable!
      !+    IGTP(N) /= 0) .AND. nop(n,2) /= -9999) THEN
      !+    IGTP(N) /= 0) .AND. nop(n,2) > -1000) THEN
     +    IGTP(N) /= 0) .AND. imat(n) /= 89) THEN
      !-
!add midside node for polynom approach
!-
            CALL COEF1(N,0)
CIPK NOV97
            CALL COEF1(N,3)
          !EFa Nov06, Aufruf der coef1dPoly-Subroutine für 1D-Teschke-Elemente
          ELSEIF((IMAT(N) < 900 .OR. 
!nis,may07
!add midside node for polynom approach
      !nis,feb07: Allow for numbered FFF midsides
      !+    IGTP(N) /= 0) .AND. nop(n,2) == -9999) THEN
      !+    IGTP(N) /= 0) .AND. nop(n,2) < -1000) THEN
     +    IGTP(N) /= 0) .AND. imat(n) == 89) THEN
      !-
!add midside node for polynom approach
!-

            CALL COEF1dPoly (N,0)
            CALL COEF1dPoly (N,3)
          ENDIF
        ENDIF
      ENDDO
C
Cipk nov97  Determine vertical lines for element numbers and number of
c           elements below each node
      NN=1
 1301 CONTINUE
      N=NFIXH(NN)
      IF(N <= NEM) THEN
cipk feb98 update to skip zero
        IF(N == 0) THEN
c
c...... This is a missing element
c
          NN=NN+1
          IF(NN > NE) GO TO 132
          GO TO 1301
        ENDIF
cipk feb98 end changes
C
C.....  At a surface layer  after checking for 
C       whether it is 3D
C


CIPK JAN99        IF(NETYP(N) == 16 .OR. NETYP(N) == 17 .OR. 
        IF(NETYP(N) == 16 .OR. 
     +    NETYP(N) == 6) THEN
CIPK JAN99     +       NETYP(N) == 18 .OR. NETYP(N) == 6) THEN
c
c...... This is 2-d. so there are no elements below
c
          NEREF(N)=0
          NEDEP(N)=0
          NN=NN+1
crrr mar2000          IF(NN > NE) GO TO 131
CRRR MAR2000   <---------- changed to this
          IF(NN > NE) GO TO 132
          GO TO 1301
        ELSE
c
c...... Now look below start counting
c
          NCTE=1
 1305     CONTINUE
          NN=NN+1
          IF(NN > NE) GO TO 131
          N1=NFIXH(NN)
          IF(N1 <= NEM) THEN
C
C       we have found another surface layer quit search
C
            NEDEP(N)=NCTE
            GO TO 1301
          ELSE
C
C       keep looking  check if it is a side element etc
C
            IF(IMAT(N1) < 900 .OR. IMAT(N1) > 5000) THEN
              IF(NCTE == 1) THEN
                NEREF(N)=N1-1
              ENDIF
              NCTE=NCTE+1
            ENDIF
            GO TO 1305
          ENDIF  
        ENDIF
      ENDIF
cipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,*) 'ERROR IN ELEMENT LAYER SEARCH'
      WRITE(75,*) 'ERROR IN ELEMENT LAYER SEARCH - ELEMENT ',N
      WRITE(75,*) 'SEQ=',NN,NEDEP(N),NEREF(N)
      STOP
  131 CONTINUE
      NEDEP(N)=NCTE
  132 CONTINUE
C
C..... Get volumes for all vertical columns
C
      DO N=1,NEM
CIPK FEB98 ADD TEST TO SKIP ZERO ELEMENT
        IF(NOP(N,1) > 0) THEN
          TVOLC(N)=TVOL(N)
          K=NEREF(N)+1
          IF(NEDEP(N) > 1) THEN
            L=K+NEDEP(N)-2
            DO M=K,L
              TVOLC(N)=TVOLC(N)+TVOL(M)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
CIPK NOV97 END CHANGES


      DO 133 N=1,NE
        IF(IMAT(N)/1000 /= 2) GO TO 133
        CALL BSLOP(N)
  133 CONTINUE
      DO 1331 N=1,NPM
        NBM=NREF(N)+NDEP(N)-1
        NLM=NREF(N)+1
        IF(NBM < 1) GO TO 1331
        DO 1333 M=1,3
          FC(N,M)=FC(NBM,M)
          DO 1332 J=NLM,NBM
 1332     FC(J,M)=FC(NBM,M)
 1333   CONTINUE
 1331 CONTINUE
      WRITE(LOUT,6195) NP,NE
C
CIPK NOV97 ADD CALL
      CALL ELFLOWS(IBIN)
C
C
C......READ SPECIAL CASE BOUNDARY CONDITIONS AND INSERT FUNCTIONAL SHAPE
C-
      CALL BCS(IBIN,CMIN,CPR)
C
      CALL QGENSPCL(1,0,0, 0.,0.,QDM)
C       
      CALL HGENSPCL(1,0,0, 0.,QDM)
C-
C
      IF( IPRT /= 1 ) GO TO 156
C-
C-....PRINT ELEMENT AND CORD DATA.....
C-
      WRITE(LOUT,6000)
      WRITE(LOUT,6005) TITLE
      WRITE(LOUT,6060)
      WRITE(LOUT,6061)
      INT=(NE-LE)+1
      INTT=INT+LE-1
      DO 155 J=LE,INTT
      WRITE(LOUT,6070)(N,(NOP(N,M),M=1,20),IMAT(N),NFIXH(N),AREA(N),
     1 TH(N), N=J,NE,INT)
  155 CONTINUE
  156 CONTINUE


CIPK AUG95 ADD CALL TO GET MET DATA
      CALL INMET(LOUT,NMETF,TET)

CIPK APR06
!testing, original place
      call getinit(ibin, 1, m_SimModel)
    !testing, original place-  
 !----------------------------------------------------------
 ! HN. JUNE 2009: GETINIT INITILIZES THE WSLL VALUES AND NOW FENODES CAN BE BUILT.
 ! IT IS NEEDED FOR BANK EVOLUTION MODELLING.
 
        IF (( PROFILEID == 0 ) .OR. ( IPROFIN == 73) .OR. 
     +                            ( IPROFIN == 731) )THEN
          
          IF ( .NOT. ALLOCATED (FENODES) ) THEN
          ALLOCATE ( FENODES (MAXP) )
          END IF 
                 
          CALL MAKEFENODES ( FENODES, MAXP , 2)
        END IF  
!-------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-
C-..... Read reorder array if there is input
C-
      IF(ID(1:2) == 'RT') THEN
        READ(DLIN,5010) (NFIXH(J),J=1,9)
        IF(NE <= 9) THEN
          call ginpt(lin,id,dlin)
          GO TO 3071
        ENDIF
        N1=1
 3070   N1=N1+9
        N2=N1+8
        IF(N2 > NE) N2=NE
        call ginpt(lin,id,dlin)
        IF(ID(1:3) == 'RT ') THEN
          READ(DLIN,5010) (NFIXH(J),J=N1,N2)
          IF(N2 < NE) GO TO 3070
        ELSE
cipk sep04
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(*,*) 'ERROR -- READING REORDERING DATA LINE RT'
          WRITE(75,*) 'ERROR -- READING REORDERING DATA LINE RT'
          STOP
        ENDIF
      call ginpt(lin,id,dlin)
      ENDIF
 3071 CONTINUE

      IF( IPRT /= 1 ) GO TO 500
      DO 480 J = 1, NP
      IF( MOD(J,45) /= 1 ) GO TO 470
      WRITE(LOUT,6000)
      WRITE(LOUT,6005) TITLE
      WRITE(LOUT,6080)
      WRITE(LOUT,6081)
  470 CONTINUE
      IF(J > NPM) GO TO 475
      WRITE(LOUT,6090) J,(CORD(J,K),K=1,3),AO(J),ALFA(J),NFIX(J),
     1  NFIX1(J),(SPEC(J,K),K=1,NDF),(VEL(K,J),K=1,NDF),NDEP(J)
      GO TO 480
  475 CONTINUE
      WRITE(LOUT,6090) J,(CORD(J,K),K=1,3),AO(J),ALFA(J),NFIX(J),
     1  NFIX1(J),(SPEC(J,K),K=1,NDF),(VEL(K,J),K=1,NDF)
  480 CONTINUE
  500 IF( IPRT == 2 ) CALL OUTPUT( 0 )
      OMEGA = 1.458E-4*SIN( OMEGA/57.3 )
C-
C-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
C-
*     CALL BFORM(0)
C-
cipk may06
      IF(LSS > 0) THEN
        CALL GETMAS
      ENDIF


CIPK JUL01
      CALL filehandling(2,ANAME)

      RETURN
C-
C-.....INPUT DATA CARD FORMATS.....
C-
 5005 FORMAT( A72 )
cipk jan94 new format
 5010 FORMAT( 9I8 )
CIPK AUG02
 5011 FORMAT( 2I8,F8.0,7I8)
 5020 FORMAT(9F8.0)
 5021 FORMAT(7F8.0,i8)
 5029 FORMAT(I8,F8.0)
 5030 FORMAT( I8, 8F8.0)
 5031 FORMAT(8X,8F8.0)
 5032 FORMAT(8X,I8,2F8.0)
!Nis,apr06: new format
 5033 FORMAT(8x,3f8.0)
!-
cipk apr96 add format
CIPK OCT96 UPDATE FORMAT
cipk jan97 update format
CIPK DEC00 UPDATE FORMAT
 5061 FORMAT(4I8,F8.0,I8,2f8.2,I8)
 6000 FORMAT( 1H1  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRA
     1M RMA-10S'/ 10X, 'THREE-DIMENSIONAL HYDRODYNAMICS  WITH SALINITY-T
     2EMPERATURE-SEDIMENT'/18X,'VERSION 3.5d MAy 2006')
 6005 FORMAT( 5X, A72 )
cipk apr96 add format and change start of 6010
CIPK OCT96 UPDATE FORMAT
 6009 FORMAT( // 2X, 'RUN CONTROL PARAMETERS' //
     +  5X, 'TURBULENCE CLOSURE',T26,I5/
     +  5X, 'MARSH INPUT SWITCH',T26,I5/
     +  5X, 'HORIZ. TURB. SWITCH',T26,I5/
     +  5X, 'TURBULENCE FACTOR',T23,F8.2/
     +  5X, 'TURBULENCE MINIMUM',T23,F8.2/
     +  5X, 'STARTING YEAR',T26,I5/
     +  5X, 'STARTING DAY',T26,I5/
     +  5X, 'STARTING HOUR',T23,F8.2/
     +  5X, 'PROJECTION SWITCH',T23,I8)
cipk dec00 add to format above

cipk mar03 add diffusion switch ( default of  0 uses old formulations
CIPK MAR05 ADD INOTR      
 6010 FORMAT( // 2X, 'RUN CONTROL PARAMETERS' //
     + 5X, 'READ LAYERS',T40,I5/
     + 5X,'GRAVITATIONAL ACCEL', T39,F6.2/
     + 5X,'ZERO BOTTOM VEL. SWITCH',T40,I5/
     + 5X, 'PASSIVE DENSITY SW1',T40,I5/
     + 5X, 'PASSIVE DENSITY SW2',T40,I5/
     + 5X, 'PASSIVE DENSITY SW3',T40,I5
     +/5X, 'ZERO SURFACE VEL. SWITCH',T40,I5/
     + 5X, 'HORIZ. DIFF. SWITCH',T40,I5/
     + 5X, 'APPLY NO TRANSFRM SWITCH',T40,I5)
CIPK NOV98 ADD FORMAT ABOVE
 6011 FORMAT(5X,'INPUT RESTART FILE', T26,I5/
     +5X,'OUTPUT RESTART FILE', T26, I5/
     +5X,'INPUT GEOMETRY FILE', T26, I5/5X,'FINAL RESULTS FILE', T26,I5/
     + 5X, 'INPUT 3-D FILE',T26,I5/ 5X, 'OUTPUT 3-D FILE',T26,I5)
 6020 FORMAT(  5X, 'AVG LAT(DEG)', F13.2 / 
     + 5X, 'INIT/TRANS WS ELEV',F7.2/
     1 5X, 'X-SCALE FACTOR', F11.2 /
     +  5X, 'Y-SCALE FACTOR', F11.2 /
     2  5X, 'Z-SCALE FACTOR', F11.2 /
     3  5X, 'BOUNDARY CMIN ', F11.2 /
     4  5X, 'BOUNDARY CPOWER',F10.2/
     3  5X, 'NOMINAL VELOC.', F11.2 / 5X, 'VELOCITY DIR.',F12.0/
     5  5X, 'MINIMUM DEPTH',F12.2/
     +  5X  'DEBUG SWITCH ',I12)
 6025 FORMAT( // 2X, 'ITERATIONAND PRINT CONTROLS' //
     1  5X, 'CYCLES S-S ITERATION',I9 / 5X, 'CYCLES DYN ITERATIONS',
     2 I8/5X,'STARTING TIME',F16.2/
     +5X, 'TOTAL TIME STEPS',I13/
     +5X, 'PRINT OPTION    ',I13 /
     +5X,'PRINT FREQUENCY-ITERATIONS',I3/5X,'PRINT FREQUENCY-STEPS',I8/
     +5X,'STEP TO START RESULTS SAVE',I3/
     +5X, 'WET/DRY FREQ.   ',I13/5X,'BINARY SAVE FREQUENCY',I8/
     +5X,'MIN DEPTH -DRYING    ',F8.2/
     +5X, 'MIN DEPTH -WETTING',F11.2)
CIPK AUG07 ADD ICPU
 6024 FORMAT(
     +5X, 'FREQUENCY FOR BINARY OUTPUT',I5/
     +5X, 'FREQUENCY FOR RESTART FILES',I5/
     +5X, 'FREQUENCY FOR RESTARTING OUTPUT FILE',I5/
     +5X, 'SOLUTION OPTION                     ',I5)
CIPK MAR06 ADD OUTPUT FILE REWIND     
cipk mar98 add to format for additional parameter
 6030 FORMAT( /// 5X, 'ELEMENT CHARACTERISTICS' //
     1'  NUM   X-X TURB   X-Y TURB   Y-X TURB   Y-Y TURB CHEZY-MANN   X-
     2Z TURB   Y-Z TURB     X DIFF     Y DIFF     Z DIFF  BANK-FRIC'
     3,'  MARSH FAC SURF MANN   MIN DIFF')
!NiS,apr06: increased number of field entries:
! 6031 FORMAT( I5,1P14E11.3)
 6031 FORMAT( I5,1P17E11.3)
!-
CIPK MAR01
 6032 FORMAT(/'VARIABLE MANNING COEFFICIENTS HAVE BEEN DEFINED'/
     +'   EL TYPE    ELEV MIN     MAN MIN    ELEV MAX     MAN MAX')
 6033 FORMAT(I10,4F12.4)
 6035 FORMAT('  NODE',I6,' FORCED TO SLOPE',F12.6)
 6036 FORMAT(/'   THE FOLLOWING NODES FORCED TO FIXED SAL/TEMP/SED WITH 
     +VALUES'/10X,3F12.3)
 6037 FORMAT(/'   THE FOLLOWING MID-SIDE NODES ARE CONSTRAINED TO A STRA
     +IGHT LINE INTERPOLATION'/9I8)
 6038 FORMAT(/'   THE FOLLOWING NODES USE BOUNDARY INTERFACE FUNCTION WI
     +TH VALUES'/10X,2F12.3)
 6040 FORMAT(///9X,'BOUNDARY CONDITION FUNCTIONAL DISTRIBUTION'//
     1'  EXTERNAL BOUNDARIES'/
     25X,'MINIMUM VELOCITY',T22, F9.4/5X,'POWER FUNCTION',T21,F10.4//
     3'  INTERNAL BOUNDARIES'/
     45X,'MINIMUM VELOCITY',T22, F9.4/5X,'POWER FUNCTION',T21,F10.4///
     510X,'PARAMETERS FOR VERTICAL EDDY COEFFICIENTS'//
     65X,'EDDY1',T21,F10.4/5X,'EDDY2',T21,F10.4/5X,'EDDY3',T21,F10.4///
     75X,'**G**',T21,F10.2)
 6041 FORMAT(/'ADDITIONAL DRAG COEFFICIENTS HAVE BEEN DEFINED'/
     +'   EL TYPE    DRAG  X     DRAG  Y')
 6042 FORMAT(I10,2G12.6)

 6043 FORMAT(/, ' ELEMENT-TYPE-DEPENDENT TURBULENCE PARAMETERS', /, 
     + '  ELEMENT TYPE  HORIZ-TURB-SWITCH  TURBULENCE FACTOR', 
     + '  TURBULENCE MINIMUM')
 6044 FORMAT(I14,I19,F19.3,F20.4)

 6045 FORMAT(/, ' OPTIONAL ELEMENT-TYPE-DEPENDENT TURBULENCE PARAMETERS'
     +/'  ELEMENT TYPE  TURBULENCE FACTOR  TURBULENCE MINIMUM')
 6046 FORMAT(I14,F19.3,F20.4)
CIPK AUG02
 6047 FORMAT(/  5X, 'ZERO SURFACE STRESS WHEN DEPTH LESS THAN',F10.3/)
CIPK SEP04
 6052 FORMAT(/'DEPTH VARIABLE MANNING COEFFICIENTS HAVE BEEN DEFINED'/
     +'   EL TYPE MAX NON-VEG N DEP VEG EFF   N FOR VEG      C-COEF ')
 6053 FORMAT(I10,I12,F12.4,F14.4,2F12.4)
 6054 FORMAT(/'DEPTH TABLE MANNING COEFFICIENTS HAVE BEEN DEFINED'/
     +'   EL TYPE   EL1    MAN1   EL2    MAN2   EL3    MAN3   EL4    MAN
     +4')
 6055 FORMAT(I10,4(F6.2,F8.4))
 6060 FORMAT(  /  10X, 'NODAL CONNECTIONS AND MATERIAL NUMBERS....' )
 6061 FORMAT( /       ' ELT     NODES(COUNTERCLOCKWISE)',75X,'TYPE    SE
     1Q         VOL   ANGLE')
 6070 FORMAT( I4,20I5,2I7,F12.1,F8.3 )
 6080 FORMAT( / 10X, '.....NODAL SPECIFICATIONS.....' )
 6081 FORMAT( /    ' NODE     X-LOC     Y-LOC  Z-LOC   BELEV   SLOPE   F
     +IX X-FLW Y-FLW  ELEV CONC1 CONC2 CONC3 INITU INITV INITH INITS INI
     +TT INITS NDEP')
 6090 FORMAT(  I5,2F10.0,F7.2,F8.1,F8.4,I5,I1,12F6.2,I5)
cipk dec99
 6100 FORMAT(//10X,'AUTOMATIC TRANSITION IN SHALLOW AREAS FROM 3-D TO 2-
     +D ACTIVATED')
 6101 FORMAT(10X,'TRANSITION DEPTH AUTOMATICALLY CALCULATED')
 6102 FORMAT(10X,'TRANSITION DEPTH SET TO',F8.2)
CIPK MAR01
 6103 FORMAT(//10X,'AUTOMATIC SCALING OF TIME STEP ACTIVATED')
 6104 FORMAT(10X,'REFERENCE NODE NUMBER',I10/
     +       10X,'TRANSITION ELEVATION ',F10.2/
     +       10X,'TIME STEP FACTOR     ',F10.2)
 6105 FORMAT(//10X,'AUTOMATIC POWER STATION INCREMENT ACTIVATED')
 6106 FORMAT(10X,'INFLOW CC LINE NUMBER     ',I10/
     +       10X,'OUTFLOW CC LINE NUMBER    ',I10/
     +       10X,'SAL CONCENTRATION FACTOR  ',F10.2/
     +       10X,'THERMAL LOADING (MW)      ',F10.2/
     +       10X,'THROTTLING FACTOR         ',F10.2/
     +       10X,'SED CONCENTRATION FACTOR  ',F10.2/
     +       10X,'MAX ALLOWED TEMP INCREMENT',F10.2/
     +       10X,'TERMAL INCREMENT TYPE     ',I10)
CIPK OCT01 LINE ABOVE ADDED

CIPK JAN03 ADD FORMATS 6107,6108
 6107 FORMAT(//10X,'EQUILIBRIUM TEMPERATURE FORMULATION ACTIVATED')
 6108 FORMAT( 10X,'EQUILIBRIUM TEMPERATURE  ',F10.2,' Deg C'/
     +        10X,'HEAT TRANSFER COEFFICIENT',F10.4,'kJ/m2/hr-Deg C'/
     +        10X,'EXTINCTION COEFFCIENT    ',F10.3,'1/m')

CIPK SEP01 ADD TO LINE ABOVE

 6115 FORMAT( /// 10X, 'NO CONTINUITY CHECKS REQUESTED.....' )
 6120 FORMAT(   //// 10X, 'CONTINUITY CHECKS TO BE MADE ALONG THE FOLLOW
     1ING LINES'  // 6X, 'LINE',7X,'NODES')
 6125 FORMAT( I10, 4X, 20I5 )
 6140 FORMAT(//,2X,'THIS PROGRAM FILE IS DIMENSIONED AS FOLLOWS',//
     .5X,'MAX NO. OF NODES',I12,/,5X,'MAX NO. OF ELEMENTS',I9,/,
     .5X,'MAX NO. OF EQUATIONS',I8,/,5X,'MAX FRONT WIDTH   ',I10,/,
     .5X,'MAX BUFFER SIZE    ',I9,/)
 6155 FORMAT(2I10,I1,6F10.2 )
c6190 FORMAT(/15X,'TIME IN VOLUME GENERATION',I6)
 6195 FORMAT(///15X,'MAX 3D NODE NUMBER    ',I9/
     1       15X,'MAX 3D ELEMENT NUMBER ',I9)
 6200 FORMAT(/'  ICE SIMULATION PARAMETERS'//
     + 5X,'DENSITY OF WATER (KG/M3)         ',F8.1/
     + 5X,'HEAT CAPACITY OF WATER (J/DEG C) ',F8.4/
     + 5X,'TEMP. OF ICE-WATER INTERFACE     ',F8.1/
     + 5X,'HEAT TRANSFER COEFFICIENT FOR UPPER SURFACE TO AIR',F8.4/
     + 5X,'LATENT HEAT OF FUSION (J/GM)     ',F8.1/
     + 5X,'DENSITY OF SNOW (KG/M3)          ',F8.1/
     + 5X,'DENSITY OF ICE  (KG/M3)          ',F8.1/
     + 5X,'TEMP OF ICE (DEG C)              ',F8.1)
 6201 FORMAT(
     + 5X,'CALIBRATION PARAMETER - 1        ',F8.4/
     + 5X,'CALIBRATION PARAMETER - 2        ',F8.4/
     + 5X,'CALIBRATION PARAMETER - 3        ',F8.4/
     + 5X,'CALIBRATION PARAMETER - 4        ',F8.4/
     + 5X,'TRANSITION VELOCITY FOR HEAT FLUX',F8.3/)

!NiS,mar06      new formats for Error handling messages
 6802        FORMAT(5x,'------------------------------',/
     +              5x,'Wrong Definition of Continuity',/
     +              5x,'line with ID-No.: ',I3,/
     +              5x,'Program will be stopped!',/
     +              5x,'------------------------------')
 6803        FORMAT(5x,'------------------------------------',/
     +              5x,'no end of continuity line block',/
     +              5x,'condition. The sign "ECL" is missing',/
     +              5x,'Program will be stopped!',/
     +              5x,'------------------------------------')
 6804        FORMAT(5x,'----------------------------------------',/
     +              5x,'Although no continuity line to read,',/
     +              5x,'the start of continuity block sign "SCL"',/
     +              5x,'exists. To continue without reading',/
     +              5x,'that block an end of continuity block',/
     +              5x,'sign is necessary.',/
     +              5x,'Program is stopped!',/
     +              5x,'----------------------------------------')
!-

!NiS,apr06: new format for error handling while proceeding on
!           element material definitions:
 6810        FORMAT(5x,'---------------------------------------------',/
     +              5x,'ERROR - Calculation only works if there',/
     +              5x,'are equivalent sand roughnesses and in',/
     +              5x,'in combination with vegetation roughness',/
     +              5x,'calculation: Delete the line "PASCHE" from',/
     +              5x,'control file or fill in the correct parameter',/
     +              5x,'configuration.'
     +              5x,'---------------------------------------------')
!-

      END subroutine
      
      end module      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk feb97 new subroutine to process input files
      SUBROUTINE GINPT(IIN,ID,DLIN)
      CHARACTER ID*8,DLIN*72
      integer (kind = 4) :: iin
      
  100 CONTINUE
CIPK JAN03
      READ(IIN,7000) ID,DLIN
      write(75,7000) id,dlin

 7000 FORMAT(A8,A72,a8)
CIPK JAN03 END CHANGES
      do i=1,8
        if(id(i:i) == char(9)) go to 200
      enddo
      do i=1,72
        if(dlin(i:i) == char(9)) go to 200
      enddo
      IF(ID(1:3) == 'com') GO TO 100
      IF(ID(1:3) == 'COM') GO TO 100
      IF(ID(1:3) == 'Com') GO TO 100
      IF(ID(1:8) == '        ') GO TO 100
cipk jan00 add * as a possible label
      if(id(1:1) == '*') go to 100
      
      RETURN
  200 continue
cipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      write(*,*) 'Error Tab character found in the following line'
      write(75,*) 'Error Tab character found in the following line'
      write(75,7000) id,dlin
      write(*,7000) id,dlin
      stop
      END subroutine

CIPK AUG03 EXPAND TO ADD 11TH ITEM
      SUBROUTINE GINPT10(IIN,ID,DLIN)
CIPK AUG03 EXPAND TO ADD 11TH ITEM
      CHARACTER ID*8,DLIN*88
  100 CONTINUE
      READ(IIN,7000) ID,DLIN
      write(75,7000) id,dlin
 7000 FORMAT(A8,A80)
      do i=1,8
        if(id(i:i) == char(9)) go to 200
      enddo
CIPK AUG03 EXPAND TO ADD 11TH ITEM
      do i=1,88
        if(dlin(i:i) == char(9)) go to 200
      enddo
      IF(ID(1:3) == 'com') GO TO 100
      IF(ID(1:3) == 'COM') GO TO 100
      IF(ID(1:3) == 'Com') GO TO 100
      IF(ID(1:8) == '        ') GO TO 100
cipk jan00 add * as a possible label
      if(id(1:1) == '*') go to 100
      RETURN
  200 continue
cipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      write(*,*) 'Error Tab character found in the following line'
      write(75,*) 'Error Tab character found in the following line'
      write(75,7000) id,dlin
      write(*,7000) id,dlin
      stop
      END subroutine