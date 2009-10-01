!******************************************************************************************
!  subroutine file.sub opens the proper input files and assigns constant unit numbers to 
!    the files. In advance it reads the control file. The subroutine is called twice.
!    During the first call:
!     - opening the main control file
!     - opening main output files, i.e. *.itr, *MESS.out
!     - opeing the echo file *.ech; the echo file gives back everything that was read in
!       the control file
!     - reading through the files data block of the control file
!     - opening the proper files corresponding to the control file lines; checking, whether
!       filenames are valid and stopping on error, after collection of all file errors
!
!    During second call:
!     - closing the echo file *.ech
!     - based on the output file name suffix, the output file *.out is opened; It's the
!       general information file; it gets connected to the former echo file unit number.
!
!******************************************************************************************
SUBROUTINE filehandling (NT, FNAM0)

use mod_fileType
use mod_fileHandler



USE BLK10MOD, only:   &
!unit definitions
&  fileControl, &
&                     lout, lin, litr, imesout, ifile, ikalypsofm, nb, ibup, &
&                     incstr, iocon, insfl, IREBED, &
&                     iwvin, iwvfc, instr, iowgt, inwgt, icordin, intims, imassout, itimfl, &
&                     modellaus, modellein, modellrst, fnam, &
! HN052009: PROFILE DATA
&                     IPROFIN, &    
!runtime parameters
&                     iutub, id, ct, iaccyc, krestf, atim, header, nscr
!meaning of unit definition variables
!------------------------------------
! - described below
!
!meaning of parameter variables
!------------------------------
!iutub:  switch (0 or 1) - 0 means that certain turbulence options can not be executed.
!                          1 means all turbulence options can be executed, i.e. after correct restart or after to iterations with constant eddies.
!krestf: switch (0 or 1) - 0 means not restarted and ???
!                          1 means restarted or ???; the value is edited in the rma10.sub, too. The meaning is not clear. It is used in surcof.
!        TODO: Real meaning of krestf, if used unequal 0
!ct:     first letter for the output files in *.2d-format
!iaccyc: time step cycle to start calculation from
!atim:   array, that stores processing times for 1D, 2D and so on; it is used for the time-output file
!id:     temporary character variable, into which the file identifier is entered when reading a line from a file
!        -TODO: Can probably be made local
!header: header that is used for the output into several files. 
!        - TODO: Probably obsolete for RMA·KALYPSO
!nscr:   ???

USE BLK11MOD, only:   &
!unit definitions
&                     iiq, iomet, &
!parameters
&                     mmet
use BLKSWANF, only:   unitSwan
use blk_ifu, only:    iqeunit, ihunit, iqunit, key, iwindin
!*******************  DJW 20/07/04
USE WBMMODS, only: wbm_initcons, wbm_initconsfile, wbm_scourlim, wbm_scourlimfile
!*******************
implicit none
SAVE

!local temporary file
type (file), pointer :: tempFile


!passed input variables
!----------------------
character (len = *), intent (in) ::  FNAM0
integer (kind = 4), intent (in) :: NT
!meaning of the variables
!------------------------
!fnam0: name of the input files
!nt:    status of the program progress; the call of the 
!       1 - 1st call (called from RMA10.program); the general files are read and opened
!       2 - 2nd call (called from input.subroutine); output file LOUT is opened


!parameters for error messages because of i/o errors
!---------------------------------------------------
integer (kind = 4) :: iermsg, ioerr
!meaning of the variables
!------------------------
!iermsg: global program variable for i/o status, describing whether any file has a problem;
!        if the variable becomes unequal zero, execution will be terminated
!ioerr:  used in each open and read statement to check for the i/o status. (iostat-variable)
!        It gives back the system error code, for what happened to the i/o trial. If everything
!        is fine, variable becomes 0

!counters
!--------
integer (kind = 4) :: n, j

!For reading in purposes
!-----------------------
CHARACTER (len = 96) :: FNAME, FNAMIN
!meaning of string variables
!---------------------
!FNAMIN = original line string read from control file to get file open informations
!FNAME  = trimmed file name trim(fnamin) to get real name of file for opening procedure

!for output purposes every filename gets its own variable
!--------------------------------------------------------
character (len = 96) :: FNAMMES
character (len = 96) :: FNAM1,  FNAM2,  FNAM3,  FNAM4,  FNAM6
character (len = 96) :: FNAM10, FNAM11, FNAM12, FNAM13, FNAM14, FNAM15, FNAM16, FNAM17, FNAM18 , FNAM19
character (len = 96) :: FNAM20, FNAM21, FNAM22, FNAM26, FNAM27
character (len = 96) :: FNAM30, FNAM32, FNAM33, FNAM34, FNAM36, FNAM38, FNAM39
character (len = 96) :: FNAM40, FNAM41
!meaning of file names
!---------------------
!FNAMMES= message file                     (OutputMESS.out)
!FNAM0  = main control                     (control.r10)
!FNAM1  = Iteration results output         (Output.itr)
!FNAM2  = geometry                         (model.2d)
!FNAM3  = restart result with geometry     (z.B. model.2d)
!FNAM4  = Bed restart with Layer-Thickness (z.B. model.bed)
!FNAM5  = obsolete
!FNAM6  = Echo file                        (Output.ech)
!FNAM7  = obsolete
!FNAM8  = obsolete
!FNAM9  = obsolete
!FNAM10 = obsolet; was never used, but probably it was ment to be for 3D input (ifit was the unit)
!FNAM11 = external boundary condition file (INPUT)
!FNAM12 = meteorological data              (INPUT)
!FNAM13 = element inflow hydrograph data   (INPUT)
!FNAM14 = tidalgraph data                  (INPUT)
!FNAM15 = hydrograph data                  (INPUT)
!FNAM16 = tidal harmonic data              (INPUT)
!FNAM17 = wind data                        (INPUT)
!FNAM18 = PROFILE DATA FOR BANK EROSION    (INPUT)   !HN052009
!FNAM19 = temperature data results         (OUTPUT)
!FNAM20 = CCL hydrograph data              (OUTPUT)
!FNAM21 = wave data                        (INPUT)
!FNAM22 = surface stress data              (INPUT)
!FNAM23 = obsolete
!FNAM24 = obsolete
!FNAM25 = obsolete
!FNAM26 = SWAN control file                (INPUT)
!FNAM27 = surface traction from external grid (INPUT)
!FNAM28 = obsolete
!FNAM29 = obsolete
!FNAM30 = Mesh weighting output            (OUTPUT)
!FNAM31 = obsolete
!FNAM32 = external grid data               (INPUT)
!FNAM33 = control structure data           (INPUT)
!FNAM34 = on/off controlling of constrol structure time series (INPUT)
!FNAM35 = obsolete
!FNAM36 = weighting factors for interpolation from external grid (INPUT)
!FNAM37 = obsolete
!FNAM38 = ???
!FNAM39 = processing time data              (OUTPUT)
!FNAM40 = stage flow table (h/q - relationship definition (INPUT)

!local variables
!---------------
CHARACTER (len = 10) :: datec, timec, zonec
integer (kind = 8), dimension (1:8) :: DTI
!meaning of the variables; those time variables are used for header outputs later;
!------------------------
!datec: system date as YYYYMMDD
!timec: system time as hhmmss.Milsec
!zonec: system time zone as +XXXX (e.g. GMT+1 is +0100)
!DTI  : whole system date and time as array
!       YYYY - MM - DD - XXXX - hh - mm - ss - milsec
!       XXXX: minutes difference to the UTC time (i.e. variables for time zone)


!Second call: Open the real output file and close the echo
!-----------
IF (NT == 2) THEN
  CLOSE (LOUT)
  FNAME = trim (FNAM) // '.out'
  OPEN (LOUT, FILE = FNAME, STATUS = 'UNKNOWN')
  RETURN
ENDIF

!First call
!----------
!Adding unit initializations for KALYPSO-format
!Kalypso-2D input file

!obsolet unit definitions
!NLL = 0    !restart output file (binary or ASCII)
!NIPT = 0   !result input file for external value overwriting (binary or ASCII)
!NOPT = 0   !result output file (binary or ASCII)
!IFIT = 0   !input 3D geometry file; was not even recommend in RMA10 applications (binary or ASCII)
!IFOT = 0   !output 3D geometry file for postprocessing purposes (binary or ASCII)
!NAINPT = 0 !output 2D geo
!IRMAFM = 0 !output binary results in RMA format
!JBEDOT = 0 !output bed data file
!ISMSFM = 0 !sms output data file
!ISMSFM1 = 0 !sms output data file
!ISMSGN = 0 !sms geometry data file
!ISMSFM2 = 0 !sms output data file
!IMESHOUT = 0 !output mesh file (RM1)
!IWAVOT = 0 !output wave data
!IBEDOT = 0 !ice or bed data output
!INBNWGT = 0 !external weighting (binary)
!INBNSTR = 0 !external stresses (binary)
!IOBNWGT = 0 !weighting results output (binary)

!parameter initializations for the program runtime
!global variables
!----------------
iutub = 0
krestf = 0
mmet = 0
!local variables
!---------------
IERMSG = 0
IOERR = 0

!active unit initialisations connected to variable file names
!--------------------------------------------------------
ikalypsofm = 0 !geometry and results output file (in *.2d-format)

!active unit initialisations connected to constant file names
!------------------------------------------------------------
!named
IMESOUT = 75   !unit of Message file (MESS.ech)                    (FNAMMES)
!numbered and always present
LIN = 2        !unit of control file (control.r10)                 (FNAM0)
LITR = 1234    !unit of iteration file (.itr)                      (FNAM1)
LOUT = 3       !after 1st call unit of output file (.ech)          (FNAM6)/
               !after 2nd call LOUT becomes the general output unit (.out-file)
!numbered and present, if the unit number becomes unequal zero during runtime!
IFILE = 0      !model geometry (in *.2d-format)                    (FNAM2)
NB = 0         !restart input file (ASCII)                         (FNAM3)
IREBED = 0     !Bed-restart input file (ASCII)                     (FNAM4)
ibup = 0       !external boundary condition file                   (FNAM11)
IIQ = 0        !input meteorological data file                     (FNAM12)
IQEUNIT = 0    !Q-graph for element inflow                         (FNAM13)
IHUNIT = 0     !tidalgraph                                         (FNAM14)
IQUNIT = 0     !Q-graph for continuity line                        (FNAM15)
KEY = 0        !tidal coefficient graph                            (FNAM16)
IWINDIN = 0    !wind data graph                                    (FNAM17)
IPROFIN = 0      ! INPUT UNIT FOR PROFILE DATA                     (FNAM18)
IOMET = 0      !output meteorological data file                    (FNAM19)
IOCON = 0      !continuity line hydrograph file                    (FNAM20)
IWVIN = 0      !input wave data file                               (FNAM21)
IWVFC = 0      !input surface stress data file                     (FNAM22)
unitSWAN = 0   !SWAN control file                                  (FNAM26)
INSTR = 0      !surface traction from external grid                (FNAM27)
IOWGT = 0      !Mesh weighting output                              (FNAM30)
ICORDIN = 0    !external grid data                                 (FNAM32)
INCSTR = 0     !control structure data                             (FNAM33)
INTIMS = 0     !on/off controlling of constrol structure time series(FNAM34)
INWGT = 0      !external inpolation weighting data file            (FNAM36)
IMASSOUT = 0   !???                                                (FNAM38)
ITIMFL = 0     !processing time data                               (FNAM39)
insfl = 0      !input control stage flow relationship file         (FNAM40)


fileControl.lin => newFile (2, fnam0, 'old')

!Open the main control file of the execution
!-------------------------------------------
call openFileObject (fileControl.lin)

WRITE(*,*) 'Inputstatus control-file: ', ioerr
!check for errors
IF(IOERR /= 0) THEN
  CALL iosmsg (IOERR, trim(fnam0), IERMSG)
  call ErrorMessageAndStop (1012, 0, 0.0d0, 0.0d0)
ENDIF



!Read input files block
!-------------------------------------------
FileRead: DO

  !read the next line from the control file
  READ (fileControl.lin.unit, '(A8, A96)') ID, FNAMIN
  !write out the read line from the control file to the console
  WRITE(*,*) ' id: ', ID, ' file name: ', FNAMIN
  !trim the read file name from control file, to leave outer blanks aside
  fname = adjustl (fnamin)
  fname = trim (fname)


  IF (ID == 'OUTFIL  ') THEN
    !assign to global name space for output (FNAM is defined in BLK10MOD)
    FNAM = FNAME
    !assign local file names
    FNAM1 = trim (FNAME) // '.itr'
    FNAM6 = trim (FNAME) // '.ech'
    FNAMMES = trim (FNAME) // 'MESS.OUT'
    !open the files
    OPEN (LITR, FILE = FNAM1, STATUS = 'UNKNOWN')
    OPEN (LOUT, FILE = FNAM6, STATUS = 'UNKNOWN')
    OPEN (IMESOUT, FILE = FNAMMES, STATUS = 'UNKNOWN')
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,may06: Changed concept for Kalypso-input files. If the user decides to run RMA10S with Kalypso-2D modell format, he has to
!           enter additional informations. These informations are concerning the suffix of result files, the first letter of result files
!           and the timestep, at which to start from. Generally this concept is the same as it is in Kalypso-2D. If the value of that is <= 1, then
!           no restart data will be read. The only exception of that is a restart file in another format. If there is a restart file in another
!           format, then it is considered in the case of starting time step == 1. The user has no option, for writing result file. If he is
!           working with Kalyso-2D geometry input, at least a Kalypso-2D format output and restartable file will be generated.

  !Input geometry (and optionally restart results file)
  ELSEIF(ID == 'INKALYPS') THEN
    !unit of the geometry file
    IFILE = 60
    call fileOpen (IFILE, trim(FNAME), 'OLD', 'FORMATTED', FNAM2, IERMSG)
    !initialise: The first time step is by default the first one in the list
    iaccyc = 1

    !Read additional informations; THEY HAVE TO BE THERE!!!
    READ (fileControl.lin.unit,'(A8,A96)') ID, FNAMIN

    !Error, if the additional control informations were not entered
    IF (ID(1:7) /= 'CONTROL') call ErrorMessageAndStop (1010, 0, 0.0d0, 0.0d0)

    !If additional CONTROL-line, read data
    READ (FNAMIN, *, iostat = ioerr) ct, iaccyc, modellaus
    IF (ioerr /= 0) call ErrorMessageAndStop (1010, 0, 0.0d0, 0.0d0)
    !unit number for the output files in Kalypso format; only set ir
    IKALYPSOFM = 77

    !Now get the Restart informations
    !There can be 2 cases for restarting:
    !  - iaccyc > 1: It means, that not the first, but a later time step should be used to start with the
    !                calculation. For this reason, a result from before must be present
    !  - RESTART   : The optional line RESTART is given, that implies not to use the initial guess, but the 
    !                result informations from the given model.
    !In both cases the geometry file (line INKALYPS) contains the result to restart with, too.

    !check for 'RESTART' in next line
    READ (fileControl.lin.unit,'(A8,A96)') ID, FNAMIN
    !handle restarting
    IF (ID(1:7) == 'RESTART' .or. iaccyc > 1) THEN
      !unit number of restart file is the same as input file
      NB     = ifile       
      !parameters for run time
      KRESTF = 1
      iutub  = 1
      !name of restart file is in Kalypso-2D case the same as geometry-file
      FNAM3  = FNAM2    
    endif
    !backspace file, if 'RESTART'-entry was not present, not to jump over any line
    if (.not. (ID(1:7) == 'RESTART')) backspace (fileControl.lin.unit)


!Input 'BEDREST' = Bed-RESTART (INPUT-Data)
  !-----------------------------------------
  ELSEIF(ID(1:7) == 'BEDREST') THEN
    !unit of the Bed-Restart file
    IREBED = 50
    !name of bed restart file = FNAM4
    call fileOpen (IREBED, trim(FNAME), 'OLD', 'FORMATTED', FNAM4, IERMSG)
    ! Restart bed need not be combined with Restart-Hydrodynamic File.
    ! Restart bed can also be loaded for starting from Lake-Solution as well
    !       as starting from a real Restart-Case


  !meteorological data time series (INPUT)
  !---------------------------------------
  ELSEIF(ID == 'METFIL  ') THEN
    !unit number
    IIQ = 30
    call fileOpen (IIQ, trim(FNAME), 'OLD', 'FORMATTED', FNAM12, IERMSG)
    !additional parameters to be set
    MMET=1

  !external boundary condition file (INPUT)
  !----------------------------------------
  ELSEIF (ID == 'BCFIL   ') THEN
    IBUP=61
    call fileOpen (ibup, trim (fname), 'OLD', 'FORMATTED', fnam11, iermsg)

  !element inflow hydrograph data (INPUT)
  !--------------------------------------
  ELSEIF (ID == 'INELTFL ') THEN
    !unit number
    IQEUNIT=14
    call fileOpen (IQEUNIT, trim(FNAME), 'OLD', 'FORMATTED', FNAM13, IERMSG)

  !tidalgraph data file (INPUT)
  !----------------------------
  ELSEIF (ID == 'INELEV  ') THEN
    !unit number
    IHUNIT=12
    call fileOpen (IHUNIT, trim(FNAME),'OLD','FORMATTED',FNAM14,IERMSG)

  !hydrograph data file (INPUT)
  !----------------------------
  ELSEIF(ID == 'INHYD   ') THEN
    !unit number
    IQUNIT=11
    call fileOpen (IQUNIT, trim(FNAME), 'OLD', 'FORMATTED', FNAM15, IERMSG)

  !tidal harmonic data file (INPUT)
  !--------------------------------
  ELSEIF(ID == 'INHARM  ') THEN
    !unit number
    key=13
    call fileOpen (key, trim(FNAME), 'OLD', 'FORMATTED', FNAM16, IERMSG)

  !continuity line hydrograph result data (output)
  !-----------------------------------------------
  ELSEIF(ID == 'OUTCON  ') THEN
    !unit number
    IOCON=21
    call fileOpen (IOCON, trim(FNAME), 'REPLACE', 'FORMATTED', FNAM20, IERMSG)

  !wave data (INPUT)
  !-----------------
  ELSEIF(ID == 'INWAVE  ') THEN
    IWVIN=101
    call fileOpen (IWVIN, trim(FNAME), 'OLD', 'UNFORMATTED', FNAM21, IERMSG)

  !surface stress data (INPUT) (unformatted file)
  !-----------------------------------------------
  ELSEIF(ID == 'INSSTR  ') THEN
    IWVFC=102
    call fileOpen (IWVFC, trim(FNAME), 'OLD', 'UNFORMATTED', FNAM22, IERMSG)

  !surface stress data (INPUT) (formatted file)
  !-----------------------------------------------
  ELSEIF(ID == 'INDFSSTR') THEN
    IWVFC=104
    call fileOpen (IWVFC, trim(FNAME), 'OLD', 'FORMATTED', FNAM22, IERMSG)

  !SWAN control file (INPUT)
  !-------------------------
  ELSEIF(ID == 'SWANFL  ') THEN
    unitSwan = 106
    call fileOpen (unitSwan, trim(FNAME), 'OLD', 'FORMATTED', FNAM26, IERMSG)

  !stage flow table (h/q - relationship definition (INPUT)
  !-------------------------------------------------------
  ELSEIF(ID == 'STFLFIL ') THEN
    INSFL=40
    call fileOpen (INSFL, trim(FNAME), 'OLD', 'FORMATTED', FNAM40, IERMSG)
    
  !volume - waterlevel relationship of storage elements definitino (input)
  !-----------------------------------------------------------------------
  ELSEIF (ID == 'VOLWLFIL') THEN
    fileControl.volWlFil => newFile (41, trim(fname), 'old')
    call openFileObject (fileControl.volWlFil)

  !external grid data (INPUT)
  !--------------------------
  ELSEIF(ID == 'INSRCORD') THEN
    ICORDIN=19
    call fileOpen (ICORDIN, trim(FNAME), 'OLD', 'FORMATTED', FNAM32, IERMSG)

  !weighting factors for interpolation from external grid (INPUT)
  !--------------------------------------------------------------
  ELSEIF(ID == 'INWGT   ') THEN
    INWGT=10
    call fileOpen (INWGT, trim(FNAME), 'OLD', 'FORMATTED', FNAM36, IERMSG)

  !surface traction from external grid (INPUT)
  !-------------------------------------------
  ELSEIF(ID == 'INSTRESS') THEN
    INSTR=15
    call fileOpen (INSTR, trim(FNAME), 'OLD', 'FORMATTED', FNAM27, IERMSG)

  !wind data file (INPUT)
  !----------------------
  ELSEIF(ID == 'AWINDIN ') THEN
    IWINDIN=70
    call fileOpen (IWINDIN, trim(FNAME), 'OLD', 'FORMATTED', FNAM17, IERMSG)

  !control structure data (INPUT)
  !------------------------------
  ELSEIF(ID(1:6) == 'INCSTR') THEN
    INCSTR=20
    call fileOpen (INCSTR, trim(FNAME), 'OLD', 'FORMATTED', FNAM33, IERMSG)
 
  !on/off controlling of constrol structure time series (INPUT)
  !------------------------------------------------------------
  ELSEIF(ID(1:6) == 'INTIMS') THEN
    INTIMS=22
    call fileOpen (INTIMS, trim(FNAME), 'OLD', 'FORMATTED', FNAM34, IERMSG)

   !HN052009: PROFILE DATA FILE FOR BANK EVOLUTION MODELLING.
  !-------------------------------------
  ELSEIF ( ID(1:7) == 'PROFILE' ) THEN
    ! UNIT NUMBER
    IPROFIN = 73
    call fileOpen (IPROFIN, trim(FNAME), 'OLD', 'FORMATTED', FNAM18, IERMSG)

 !HN082009: "RESTART" PROFILE DATA FILE FOR BANK EVOLUTION MODELLING.
 ! It must be available for restarting a morphological timestep with bank evolution.
  !---------------------------------------------------------------------------------
  ELSEIF ( ID(1:7) == 'PRFREST' ) THEN
    ! UNIT NUMBER
    IPROFIN = 731
    call fileOpen (IPROFIN, trim(FNAME), 'OLD', 'FORMATTED', FNAM18, IERMSG)
  !--------------------------------
  ELSEIF(ID == 'OUTMET  ') THEN
    IOMET=72
    call fileOpen (IOMET, trim(FNAME), 'REPLACE', 'FORMATTED', FNAM19, IERMSG)

  !Mesh weighting output            (OUTPUT)
  !-----------------------------------------
  ELSEIF(ID == 'OUTWGT  ') THEN
    IOWGT=16
    call fileOpen (IOWGT, trim(FNAME), 'REPLACE', 'FORMATTED', FNAM30, IERMSG)

!********************************************************
!
!     DJW 15/07/04, Adds Hook for reading in the initial conditions for Temp, Salt and Sed concentrations.
!        
!********************************************************
  ELSEIF(ID== 'INITCONS') THEN
    wbm_InitCons = .TRUE.
    wbm_InitConsFile = FNAME
!********************************************************        
!
!     DJW 15/02/05, Adds Hook for reading in the rock levels 
!        
!********************************************************
  ELSEIF(ID=='SCOURLIM') THEN
    wbm_ScourLim = .TRUE.
    wbm_ScourLimFile = FNAME
!********************************************************
!cipk MAY06

  !
  !-----------------------------------------------
  ELSEIF(ID == 'AMASSOUT') THEN
    IMASSOUT=24
    call fileOpen (IMASSOUT, trim(FNAME), 'OLD', 'FORMATTED', FNAM38, IERMSG)

  !processing time output file (OUTPUT)
  !------------------------------------
  ELSEIF(ID(1:7) == 'TIMFIL') THEN
    ITIMFL=25
    call fileOpen (ITIMFL, trim(FNAME), 'REPLACE', 'FORMATTED', FNAM39, IERMSG)
    CALL SECOND(ATIM(1))

  !not longer used line types for input definition in RMA-Kalypso
  !--------------------------------------------------------------
  ELSEIF(ID == 'INRST   ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'INBNRST ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTRST  ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTBNRST') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTRES  ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTBNRES') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'VELFIL  ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'VELBNFIL') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUT2GE  ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTBN2GE') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUT3GE  ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTBN3GE') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'IN3DGE  ') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'IN3DBNGE') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'OUTBNRMA') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'INBNSSTR') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'INBNWAVE') THEN
    call notSupportedLine (ID)
  ELSEIF(ID == 'INBNWGT ') THEN
    call notSupportedLine (ID)

  !end of files input block
  !------------------------
  ELSEIF(ID == 'ENDFIL  ') THEN
    exit FileRead
  ENDIF

enddo FileRead

!catch any problems during file opening process and stop on it
!problematc files were written into the echo file!
!-------------------------------------------------------------
if(iermsg == 1) call ErrorMessageAndStop (1013, 0, 0.0d0, 0.0d0)


!TODO: What shall happen with this?
!----------------------------------
!cipk dec01 add geometry file name to information
!c         first the directory, then the file name
!cipk JUL02 REMOVE CONDITION      IF(IRMAFM == 71) THEN
!cipk      START FORMING HEADER
DO J=11,1000
  HEADER(J:J)=' '
ENDDO
HEADER(1:10)='RMA10     '
CALL DATE_AND_TIME (DATEC,TIMEC,ZONEC,DTI)
HEADER(11:20)=DATEC
HEADER(21:30)=TIMEC
HEADER(31:40)=ZONEC
HEADER(251:298)=FNAM2(1:48)


!TODO: Is this the correct way?
!This can only happen, if there is no -OUTPUT- line; write default file names
IF (IMESOUT == 0) OPEN (75, FILE = 'MESSRM10.OUT')
IF (LOUT == 0) call ErrorMessageAndStop (1014, 0, 0.0d0, 0.0d0)

!global use of geometry file name and restart file name
!------------------------------------------------------
modellein = FNAM2
modellrst = FNAM3

!some control outputs
!--------------------
WRITE (LOUT, 6010) trim (FNAM0)
 6010 FORMAT (' RMA-10 INPUT FILE NAME:         ', A)
IF (MMET > 0) WRITE (LOUT, 6016) trim (FNAM12) !auxiliary parameter used
 6016 FORMAT (' INPUT MET FILE NAME:            ', A)
IF(IFILE > 0) WRITE(LOUT, 6012) trim (FNAM2)
 6012 FORMAT (' INPUT GEOMETRY FILE NAME:       ', A)
IF(NB > 0) WRITE(LOUT,6013) trim (FNAM3)
 6013 FORMAT (' INPUT RESTART FILE NAME:        ', A)
IF(IHUNIT > 0) WRITE(LOUT,6024) trim (FNAM14)
 6024 FORMAT (' INPUT TIDAL GRAPH FILE NAME:    ', A)
IF(IQUNIT > 0) WRITE(LOUT,6025) trim (FNAM15)
 6025 FORMAT (' INPUT HYDROGRAPH FILE NAME:     ', A)
IF(IQEUNIT > 0) WRITE(LOUT,6026) trim (FNAM13)
 6026 FORMAT (' INPUT ELEMENT INFLOW FILE NAME: ', A)
IF(KEY > 0) WRITE(LOUT,6027) trim (FNAM16)
 6027 FORMAT (' INPUT TIDAL HARMONIC FILE NAME: ', A)
IF(IWINDIN == 70) WRITE(LOUT,6029) trim (FNAM17)
 6029 FORMAT (' INPUT ASCII WIND FILE NAME:     ', A)
IF(IPROFIN == 73) WRITE(LOUT,6030) trim (FNAM18)
 6030 FORMAT (' INPUT PROFILE DATA FILE NAME:   ', A)
IF(IOMET == 72) WRITE(LOUT,6031) trim (FNAM19)
 6031 FORMAT (' OUTPUT MET FILE NAME:           ', A)
IF(IOCON == 21) WRITE(LOUT,6032) trim (FNAM20)
 6032 FORMAT (' OUTPUT CONTINUITY FILE NAME:    ', A)
IF(IWVIN == 101) WRITE(LOUT,6033) trim (FNAM21)
 6033 FORMAT (' INPUT WAVE DATA FILE NAME:      ', A)
IF(IWVFC == 102) WRITE(LOUT,6034) trim (FNAM22)
 6034 FORMAT (' INPUT SURFACE STRESS FILE NAME: ', A)
IF(INWGT == 10) WRITE(LOUT,6038) trim (FNAM36)
 6038 FORMAT (' INPUT ASCII WEIGHTING FILE NAME: ', A)
IF(INSTR == 15) WRITE(LOUT,6039) trim (FNAM27)
 6039 FORMAT (' INPUT STRESS DATA FILE NAME:    ', A)
IF(IOWGT /= 0) WRITE(LOUT,6050) trim (FNAM30)
 6050 FORMAT (' OUTPUT ASCII MESH WEIGHTING FILE NAME: ', A)
IF(ICORDIN /= 0) WRITE(LOUT,6044) trim (FNAM32)
 6044 FORMAT (' INPUT COORDINATE FILE NAME:     ', A)
IF(IWVFC == 104) WRITE(LOUT,6045) trim (FNAM22)
 6045 FORMAT (' INPUT WAVE DATA DIRECTORY FILE NAME: ', A)
IF(unitSwan == 106) WRITE(LOUT,6046) trim (FNAM26)
 6046 FORMAT (' INPUT SWAN CONTROL FILE NAME:   ', A)
IF(insfl == 39) WRITE(LOUT,6056) trim (FNAM39)
 6056 FORMAT (' INPUT STAGE-FLOW BOUNDARIES DATA FILE NAME: ', A)
IF(INCSTR == 20) WRITE(LOUT,6052) trim (FNAM33)
 6052 FORMAT (' INPUT CONTROL STRUCTURE DATA FILE NAME: ', A)
IF(INTIMS == 22) WRITE(LOUT,6053) FNAM34
 6053 FORMAT (' INPUT TIME SERIES DATA FILE NAME: ', A)
IF(IMASSOUT > 0) WRITE(LOUT,6055) trim (FNAM38)
 6055 FORMAT (' OUTPUT MASS BALANCE FILE NAME:  ', A)
!TODO: Here's something wrong
IF(ITIMFL == 25) WRITE(LOUT,6057) trim (FNAM39)
 6057 FORMAT (' TIMING OUTPUT DATA FILE NAME:   ', A)

!TODO: Meaning of NSCR; perhaps: (maximum) Number of SCRatch files?
NSCR = 9

RETURN

END
!***


!******************************************************************************************
!  subroutine to give out the information, that the file of the name >fileName< can't be
!    opened. The global error status >ioerr< (system value) is given to the subroutine
!    and the answer >ioStatus<, which displays the global status is given back. The global
!    status is zero, if everything is okay. It becomes 1, if there was any file causing
!    problems.
!******************************************************************************************
subroutine iosmsg (ioerr, fileName, ioStatus)
implicit none
!passed input variables
!----------------------
integer (kind = 4), intent (in)  :: ioerr
character (len = *), intent (in) :: fileName
!passed output variables
!-----------------------
integer (kind = 4), intent (out) :: ioStatus
!meaning of the variables
!------------------------
!ioerr    : is the global error code as an integer; it is a system dependet variable
!fileName : is the file name string of the file that couldn't be opened
!ioStatus : gives back the global program error code, that shows, whether any file caused
!           opening problems.
!           0 means everything okay
!           1 means that there was at least one file causing problems

iostatus = 1
write (*, *) "Can't open file", trim (fileName), ' error ', ioerr, ' reported'
end
!***


!******************************************************************************************
!  subroutine to write out information about obsolet line types. In RMA-Kalypso users can
!    only use a restricted amount of line types.
!******************************************************************************************
subroutine notSupportedLine (linetype)

!global variables
!----------------
use Blk10mod, only: LOUT

implicit none

!passed input variables 
!----------------------
character (len = 8), intent (in) :: linetype
!local variables
!---------------
character (len = 1000) :: output
!meaning of the variables
!------------------------
!linetype : is the line string of the no more supported linetype, the user wanted to use
!output   : is the line string that will be generated for output. Output is given to the
!         : global output file (*.out) and to the console.

!generate output string
output = 'The line type ' // trim (linetype) // ' is not supported in RMA-Kalypso.'
!write it to the output file and to the console
write (*, *) trim (output)
write (LOUT, *) trim (output)

end subroutine
!***


!******************************************************************************************
!  subroutine for opening purpose and error checks. Opens the file >fileName< with the unit
!    number assignment >unitNo<. It considers the passed status (>statusString<) and the 
!    passed form (>formString<). >localFileName< is just a copy of the name, but it saves
!    a lot of lines in file.sub. >globalErrorStatus< gives back the opening status. The
!    appearing value, when calling should be zero.
!******************************************************************************************
!***
