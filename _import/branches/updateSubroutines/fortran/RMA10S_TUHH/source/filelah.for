C     Last change:  WP    8 Jan 2008   11:11 am
cipk  last update SEP 05 2006 FIX AMASSOUT BUG
cipk  last update MAY 30 2006 add MASS OUTPUT OPTION
cipk  last update june 28 2005 add time series option
cipk  last update june 27 2005 add control structure option
cipk  last update sep 06 2004 revise filename writing CREATE ERROR FILE
CIPK  LAST UPDATE JUN 13 2003 ADD INPUT POINT STRESS FILE AND RM1 FILE
CIPK  LAST UPDATE AUG 21 2002 ADD SMS FILE CAPABILITIES
CIPK  LAST UPDATE AUG 01 2002 EXPAND TO 96 CHAR
CIPK  LAST UPDATE MAY 28 2002 ADD WAVE DATA AND SURFACE STRESS FILES
CIPK  LAST UPDATE DEC 14 2001 ADD GEO FILE AND DIRECTORY TO HEADER
CIPK  LAST UPDATE HUL 27 2001 ADD OUTPUT CONTINUITY LINE FILE
cipk  last update June 21 2001 add test to determine home directory and switch there
CIPK  LAST UPDATE DEC 21 2000 REMOVE INITIALISATION AND CREATE INITL ROUTINE
CIPK  LAST UPDATE SEP 30 1999 ADD TEST FOR BLANK INITIAL CHAR ANS EXPAND CHAR TO 48
CIPK  LAST UPDATE AUG 11 1998 ADD COMMAND LINE INTERPRETATION
cipk july 4 1997 add further pure binary options
cipk  last update April 28 1997
CIPK  LAST UPDATE jAN 22 1997
CIPK  AUG 95  SIGNIFICANT CHANGES TO FILE
      SUBROUTINE FILE(NT,FNAM0)
!NiS,Nov06: Not available in Fortran
!cintel      USE IFPORT
!CIPK COMPAQ      USE DFLIB
!-
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE parakalyps
      !*******************  DJW 20/07/04
      USE WBMMODS
      !*******************
      SAVE
C-
*...... Open files as requested by input
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
      INCLUDE 'SWANF.COM'
CIPK AUG98
      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,IWINDIN
C-
CIPK AUG02
CIPK SEP04 REMOVE FNAM ADDD FNAMMES
      CHARACTER*96 FNAMJ,fnamd,FNAMMES
      CHARACTER*96 FNAME,FNAM1,FNAM2,FNAM3,FNAM4,FNAM5,FNAM6,FNAM7
     +            ,FNAM8,FNAM9,FNAM10,FNAM0,FNAM11,FNAM12,FNAM13
     +            ,FNAM14,FNAM15,FNAM16,FNAM17,FNAMIN,fnam18,FNAM19
     +            ,FNAM20,FNAM24,FNAM25,FNAM23,FNAM26,FNAM27,FNAM28
     +            ,FNAM29,FNAM30,FNAM31,FNAM32,FNAM21,FNAM22,FNAM33
     +            ,FNAM34,FNAM35,FNAM36,FNAM37, FNAM38,
      !EFa jul07, added FNAM39 for input data file for stage-flow boundaries
     + FNAM39
      !-

!NiS,may06:
      INTEGER :: rst_error !iostat-variable
      CHARACTER (LEN = 20) :: INQUIRETEST
!-

CIPK JUN05 ADD FNAM33,FNAM34
CIPK jun0302 ADD FNAM26,FNAM27,FNAM28,FNAM29
CIPK AUG02 ADD FNAM24,FNAM25
CIPK JUL01 ADD FNAM20,
      CHARACTER*10 DATEC,TIMEC,ZONEC
      INTEGER  DTI(8)
cipk sep99 add to line above
CIPK AUG98 ADD COMMAND LINE
      INTEGER*2 N1,STATUS

CIPK DEC00      DATA VOID/-1.E20/
      LIN=2
      LOUT=3

!nis,comment
!In dependency of the switch NT, two general jobs are possible for file.subroutine
!NT = 1: First call (called from RMA10.program); the general files are read and opened
!NT = 2: Second call (called from input.subroutine); output file LOUT is opened

      !Second call
      IF (NT == 2) THEN
        CLOSE (LOUT)
        FNAME = FNAM (1:LNNAM) // '.out'
        OPEN (LOUT, FILE = FNAME, STATUS = 'UNKNOWN')
        RETURN

      !First call
      ELSE
      
        !Adding some missing unit number initializations
        !meteorological data
        MMET = 0
        !tidalgraph
        IHUNIT = 0
        !Q-graph for continuity line
        IQUNIT = 0
        !Q-graph for element inflow
        IQEUNIT = 0
        !tidal coefficient graph
        KEY = 0
        !wind data graph
        IWINDIN = 0
        !-

        !Adding unit initializations for KALYPSO-format
        !Kalypso-2D input file
        IKALYPSOFM = 0 

CIPK MAY02
C......IWVIN  = INPUT WAVE DATA FILE
        IWVIN=0
C......IWVFC  = INPUT SURFACE STRESS FILE
        IWVFC=0
C......JBEDOT = OUTPUT BED DATA FILE
        JBEDOT=0
        IFILE=0
        IFIT=0
        IFOT=0
        NB=0
        NLL=0
        NOPT=0
        NIPT=0
        IBUP=0
        NAINPT=0
cipk mar00
        IRMAFM=0
cipk jan97
        iutub=0
        insfl=0
cipk sep99
        IERMSG=0
        IOERR=0
CIPK MAR01
        IOMET=0
CIPK JUL01
        IOCON=0
cipk AUG02
        ISMSFM=0
        ISMSFM1=0
        ISMSGN=0
        ISWANR=0
        IMESHOUT=0
        DO N=1,96
          SWANEX(N:N)=' '
          SWANFL(N:N)=' '
        ENDDO
        ISMSFM2=0
        IWAVOT=0
CIPK OCT02
        IBEDOT=0

CIPK JUN03
        INWGT=0
        INBNWGT=0
        INSTR=0
        INBNSTR=0
        IOWGT=0
        IOBNWGT=0
        ICORDIN=0
CIPK SEP04
        IMESOUT=0
CIPK JUN05
        INCSTR=0
      INTIMS=0
CIPK MAY06
        IMASSOUT=0
C-
*...... Open input and output files
C-
CIPK JUL01  INPUT FILE NAME NOW IN MAIN PROGRAM

cipk oct99 add iostat test ,STATUS='OLD'
        WRITE(*,*) ioerr
        OPEN(LIN,FILE=FNAM0,IOSTAT=IOERR)
        WRITE(*,*) ioerr
        IF(IOERR .NE. 0) THEN
          CALL IOSMSG(IOERR)
CIPK SEP04 CREATE ERROR FILE
          CLOSE(75)
          OPEN(75,FILE='ERROR.OUT')
          WRITE(75,*) 'ERROR FOR FILE',FNAM0
          STOP
        ENDIF

cipk jun01  this is logic to determine the driectory of the initial r10 file
      do n=96,1,-1
        if(fnam0(n:n) .ne. ' ') then
          lnnam=n
            go to 198
        endif
      enddo
  198 lnnnam=0
        write(*,*) 'lnnnam',lnnnam
      do n=lnnam,1,-1
        if(fnam0(n:n) .eq. '\') then
            lnnnam=n-1
            go to 199
        endif
      enddo
  199 continue

        write(*,*) 'lnnnam',lnnnam
        write(*,'(a)') fnam0(1:lnnnam)

      if(lnnnam .gt. 0) then
        fnamd=fnam0(1:lnnnam)
      endif

  200 CONTINUE
cipk sep99
        READ(LIN,7000) ID,FNAMIN
 7000   FORMAT(A8,A96)
        WRITE(*,7001) ID,FNAMIN
 7001   FORMAT(' id = ',a8,'  File name = 'a96)

cipk sep99 add test for blank initial characters

        DO K=1,96
          IF(FNAMIN(K:K) .NE. ' ') THEN
            KS=K
            GO TO 201
          ENDIF
        ENDDO
        KS=1
  201   DO K=KS,96
          FNAME(K-KS+1:K-KS+1)=FNAMIN(K:K)
        ENDDO


!NiS,mar06,com: Start of test for file type; first 8 digits show type

        IF(ID .EQ. 'OUTFIL  ') THEN
          LOUT=3
          LNNAM=LENSTR(FNAME,0)
          FNAM=FNAME
          FNAM6=FNAM(1:LNNAM) // '.ech'
          OPEN(LOUT,FILE=FNAM6,STATUS='UNKNOWN')
          FNAM1=FNAM(1:LNNAM) // '.itr'
          !TODO:
          !Find a good unit number and not 1234
          !Chosen unit number for LITR
          LITR = 1234

          OPEN(LITR,FILE=FNAM1,STATUS='UNKNOWN')
CIPK SEP04
          FNAMMES=FNAM(1:LNNAM)// 'MESS.OUT'
          IMESOUT=75
          OPEN(IMESOUT,FILE=FNAMMES,STATUS='UNKNOWN')
          CALL ZVRS(0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'METFIL  ') THEN
          IIQ=30
          MMET=1
cipk oct99 add iostat test
          OPEN(IIQ,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM12=FNAME

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'BCFIL   ') THEN
          IBUP=61
cipk oct99 add iostat test
          OPEN(IBUP,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM11=FNAME

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,may06: Changed concept for Kalypso-input files. If the user decides to run RMA10S with Kalypso-2D modell format, he has to
!           enter additional informations. These informations are concerning the suffix of result files, the first letter of result files
!           and the timestep, at which to start from. Generally this concept is the same as it is in Kalypso-2D. If the value of that is <= 1, then
!           no restart data will be read. The only exception of that is a restart file in another format. If there is a restart file in another
!           format, then it is considered in the case of starting time step == 1. The user has no option, for writing result file. If he is
!           working with Kalyso-2D geometry input, at least a Kalypso-2D format output and restartable file will be generated.
        ELSEIF(ID .EQ. 'INKALYPS') THEN                                         !INPUT-Netzdatei im Kalypso-2D Format
          !identification of geometry file kind
          IFILE = 60
          IGEO  = 2
          !open the file
          OPEN(IFILE,FILE=FNAME, STATUS='OLD', IOSTAT=IOERR)
          !test for error on opening sequence
          IF(IOERR /= 0) THEN
            CALL IOSMsG(IOERR)
            IERMSF = 1
          ENDIF

          !make input file name globally accessable
          IF (LEN(TRIM(FNAME)) > 32) THEN
            CLOSE (75)
            OPEN(75,FILE='ERROR.OUT')
            WRITE(75,*) ' ERROR - MODEL NAME TO LONG; ONLY 32 DIGITS',
     +                  ' ALLOWED'
            WRITE(*,*)  ' ERROR - MODEL NAME TO LONG; ONLY 32 DIGITS',
     +                  ' ALLOWED'
            WRITE(75,*) '   EXECUTION TERMINATED'
            WRITE(*,*) '   EXECUTION TERMINATED'
            STOP
          ENDIF
          FNAM2=FNAME
          !Read additional informations; THEY HAVE TO BE THERE!!!
          READ(LIN,'(A8,A96)') ID,FNAMIN
          !Error, if the additional control informations were not entered
          IF (ID(1:7) /= 'CONTROL') THEN
            CLOSE(75)
            OPEN(75,FILE='ERROR.OUT')
            WRITE(75,*) ' ERROR - NO ADDITIONAL CONTROL DATA FOR INPUT'
            WRITE(*,*) ' ERROR - NO ADDITIONAL CONTROL DATA FOR INPUT'
            WRITE(75,*) '   EXECUTION TERMINATED'
            WRITE(*,*) '   EXECUTION TERMINATED'
            STOP
          ENDIF

          !If additional CONTROL-line, read data
          itefreq = 0 !Set default value
          READ (FNAMIN,*,iostat = ioerr) ct, iaccyc, modellaus, itefreq

          IF (ioerr == -1 .or. itefreq == 0) THEN
            WRITE(*   , 7002)
            WRITE(Lout, 7002)
 7002 FORMAT (1x, 'no results after iteration will be written')
            ioerr = 0

          ELSE
            WRITE (   *, 7003) itefreq
            WRITE (Lout, 7003) itefreq
 7003 format ('Output file will be written every', i2,
     +        'st/nd/rd/th iteration')
          ENDIF

          !TODO
          !What is this for?
          itefreq = itefreq

          !TODO
          !Is unit no. 77 good or bad
          IKALYPSOFM = 77

          !Restarting is also predetermined. It is dependent on the time step to start from. If
          !the timestep to start from is (iaccyc == 1), then no restart file is taken because it
          !is started from the first step. If the value (iaccyc > 1), then restarting is forced:
          IF (iaccyc > 1) THEN
            NB     = ifile       !unit number of restart file is the same as input file
            KRESTF = 1
            iutub  = 1
            FNAM3  = FNAM2    !name of restart file is in Kalypso-2D case the same as geometry-file
          ELSE
          !time step to start from is always, when used, the first one:
            iaccyc = 1
          !If the first step to calculate is the first time step or the steady state step, then
          !a restart file can be specified or the modell input file can be used as restart file.
            READ(LIN,'(A8,A96)') ID, FNAMIN
            RESTARTswitch = 0
            IF (ID(1:7) == 'RESTART') THEN
              IF (TRIM(FNAMIN) == FNAM2 .OR. TRIM(FNAMIN) == '' ) THEN
                NB = ifile
                FNAM3 = FNAM2
              ELSE
                NB = 62
                FNAM3 = TRIM(FNAMIN)
                open (nb, FILE=fnamin, STATUS='OLD',  iostat=IOERR)
                !test for error on opening sequence
                IF(IOERR /= 0) THEN
                  CALL IOSMsG(IOERR)
                  IERMSF = 1
                ENDIF
              ENDIF
              KRESTF = 1
              iutub  = 1
              RESTARTSwitch = 1
            ELSE
              BACKSPACE (LIN)
            ENDIF
          END IF
!-
!NiS,may06: Writing results file in Kalypso format, when input is not Kalypso-format
        ELSEIF(ID .EQ. 'OUTKALYP') THEN
          IF (IGEO .ne. 2) THEN
            IKALYPSOFM = 77
            READ (FNAMIN,*,iostat = ioerr) ct, modellaus, itefreq
            IF (ioerr.ne.0) THEN
              WRITE(*   , *) 'ERROR - DATA LINE OUTKALYP INCOMPLETE'
              WRITE(Lout, *) 'ERROR - DATA LINE OUTKALYP INCOMPLETE'
              ioerr = 0
            ENDIF
          ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'INGEO   ') THEN
          IFILE=60
CIPK JUN03
          IGEO=1
cipk oct99 add iostat test
          OPEN(IFILE,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED'
     +        ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM2=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK AUG02
        ELSEIF(ID .EQ. 'INSMSBIN') THEN
          ISMSGN=60
          OPEN(ISMSGN,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
          FNAM2=FNAME

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr97 add file type option
        ELSEIF(ID .EQ. 'INBNGEO ') THEN
          IFILE=60
CIPK JUN03
          IGEO=1

!msf version	OPEN(IFILE,FILE=FNAME,STATUS='OLD',FORM='BINARY'
!msf version	+        ,RECL=2 , IOSTAT=IOERR )


cdec version          OPEN(IFILE,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
cipk oct99 add iostat test

      OPEN(IFILE,FILE=FNAME,STATUS='OLD',ACCESS='TRANSPARENT'
     +        ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM2=FNAME
        ELSEIF(ID .EQ. 'INRST   ') THEN
!NiS,may06: The restart file can't be opened, if the input file is in Kalypso-2D format
!           and the variable iaccyc, that shows the beginning time step, is (iaccyc > 1)
          IF (IFIlE == 60 .and. IGEO == 2 .and. iaccyc > 1) then
            WRITE (*,*)' You have chosen to give a restart file. ',
     +        'If you want to restart from external file, then the ',
     +        'beginning time step must be less than 2. The restart ',
     +        'informations are read from geometry input file, this ',
     +        'command line is skipped.'
            CONTINUE
          ELSE
            NB=62
            KRESTF=1
cipk jan97
          iutub=1
cipk oct99 add iostat test
            OPEN(NB,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED'
     +          ,IOSTAT=IOERR)
            IF(IOERR .NE. 0) THEN
              CALL IOSMSG(IOERR)
              IERMSG=1
            ENDIF

            FNAM3=FNAME
          ENDIF
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'INBNRST ') THEN
          IF (IFIlE == 60 .and. IGEO == 2 .and. iaccyc > 1) then
            WRITE (*,*)' You have chosen to give a restart file. ',
     +        'If you want to restart from external file, then the ',
     +        'beginning time step must be less than 2. The restart ',
     +        'informations are read from geometry input file, this ',
     +        'command line is skipped.'
            CONTINUE
          ELSE
            NB=62
            KRESTF=1
cipk jan97
            iutub=1
!msf version          OPEN(NB,FILE=FNAME,STATUS='OLD',FORM='BINARY'
!msf version     +        ,IOSTAT=IOERR)
cipk dec version          OPEN(NB,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
cipk oct99 add iostat test
            OPEN(NB,FILE=FNAME,STATUS='OLD',ACCESS='TRANSPARENT'
     +          ,IOSTAT=IOERR)
            IF(IOERR .NE. 0) THEN
              CALL IOSMSG(IOERR)
              IERMSG=1
            ENDIF

            FNAM3=FNAME
          ENDIF
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'OUTRST  ') THEN
          NLL=63
          OPEN(NLL,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM4=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'OUTBNRST') THEN
          NLL=63
!msf version          OPEN(NLL,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
cipk dec version          OPEN(NLL,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(NLL,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM4=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'OUTRES  ') THEN
          NOPT=64
          OPEN(NOPT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM5=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr97 add file type option
        ELSEIF(ID .EQ. 'OUTBNRES') THEN
          NOPT=64
!msf version          OPEN(NOPT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
cipk dec version       OPEN(NOPT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(NOPT,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM5=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'VELFIL  ') THEN
          NIPT=65
          OPEN(NIPT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM7=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr97 add file type option
        ELSEIF(ID .eq. 'VELBNFIL') THEN
          NIPT=65
!msf version          OPEN(NIPT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
cipk dec version       OPEN(NIPT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(NIPT,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM7=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUT2GE  ') THEN
          NAINPT=68
          OPEN(NAINPT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM8=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr97
        ELSEIF(ID .eq. 'OUTBN2GE') THEN
          NAINPT=68
!msf version          OPEN(NAINPT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
cipk dec version       OPEN(NAINPT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(NAINPT,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM8=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUT3GE  ') THEN
          IFOT=67
          OPEN(IFOT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM9=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr97
        ELSEIF(ID .eq. 'OUTBN3GE') THEN
          IFOT=67
!msf version          OPEN(IFOT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
cipk dec version       OPEN(IFOT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(IFOT,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM9=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'IN3DGE  ') THEN
          IFIT=66
          OPEN(IFIT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM11=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cipk apr97
        ELSEIF(ID .eq. 'IN3DBNGE') THEN
          IFIT=66
!msf version          OPEN(IFIT,FILE=FNAME,STATUS='OLD',FORM='BINARY'
!msf version     +        ,IOSTAT=IOERR)
cipk dec version       OPEN(IFIT,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
cipk oct99 add iostat test
      OPEN(IFIT,FILE=FNAME,STATUS='OLD',ACCESS='TRANSPARENT'
     +        ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM11=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'INELTFL ') THEN
          IQEUNIT=14
cipk oct99 add iostat test
          OPEN(UNIT=14,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM13=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INELEV  ') THEN
          IHUNIT=12
cipk oct99 add iostat test
          OPEN(UNIT=12,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM14=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INHYD   ') THEN
          IQUNIT=11
cipk oct99 add iostat test
          OPEN(UNIT=11,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM15=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INHARM  ') THEN
          key=13
cipk oct99 add iostat test
          OPEN(UNIT=13,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM16=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK AUG98 ADD OPTIONS
        ELSEIF(ID .eq. 'BWINDIN ') THEN
          IWINDIN=69
cipk oct99 add iostat test
!msf version          OPEN(UNIT=69,FILE=FNAME,STATUS='OLD',FORM='BINARY',
!msf version     +    IOSTAT=IOERR)
cipk dec version       OPEN(UNIT=69,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
      OPEN(UNIT=69,FILE=FNAME,STATUS='OLD',ACCESS='TRANSPARENT')
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM17=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'AWINDIN ') THEN
          IWINDIN=70
cipk oct99 add iostat test
          OPEN(UNIT=70,FILE=FNAME,STATUS='OLD',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM17=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK AUG98 END ADDITIONS
CIPK MAR00 ADD RMA FORMAT OUTPUT
        ELSEIF(ID .eq. 'OUTBNRMA') THEN
          IRMAFM=71
!msf version          OPEN(IRMAFM,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
cipk dec version       OPEN(IRMAFM,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(IRMAFM,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM18=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUL02  MOVE HEADER FORMATION TO LATER
CIPK MAR00 END ADDITIONS
*-
        ELSEIF(ID .eq. 'OUTMET  ') THEN
          IOMET=72
cipk oct99 add iostat test
          OPEN(UNIT=72,FILE=FNAME,STATUS='UNKNOWN',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

cipk apr01 correct definition of fnam19
          FNAM19=FNAME

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Cipk jul01 add OUTCON option
        ELSEIF(ID .eq. 'OUTCON  ') THEN
          IOCON=21
          OPEN(UNIT=21,FILE=FNAME,STATUS='UNKNOWN',IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF

          FNAM20=FNAME

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK MAY02 ADD WAVE DATA FILE
        ELSEIF(ID .eq. 'INWAVE  ') THEN
          IWVIN=101
          OPEN(IWVIN,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM21=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INBNWAVE') THEN
          IWVIN=101
          OPEN(IWVIN,FILE=FNAME,STATUS='OLD',FORM='BINARY'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM21=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK MAY02 ADD SURFACE STRESS DATA FILE
        ELSEIF(ID .eq. 'INSSTR  ') THEN
          IWVFC=102
          OPEN(IWVFC,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM22=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INBNSSTR') THEN
          IWVFC=102
          OPEN(IWVFC,FILE=FNAME,STATUS='OLD',FORM='BINARY'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM22=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INDFSSTR') THEN
          IWVFC=104
          OPEN(IWVFC,FILE=FNAME,STATUS='OLD',FORM='FORMATTED'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM22=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUTBNBED') THEN
          IBEDOT=103
          OPEN(IBEDOT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM23=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK AUG02 ADD SMS FORMAT OUTPUT
        ELSEIF(ID .eq. 'OUTSMS  ') THEN
          ISMSFM=73
          OPEN(ISMSFM,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM24=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUTSMS1 ') THEN
          ISMSFM1=74
          OPEN(ISMSFM1,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM25=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'SWANFL  ') THEN
          SWANFL=FNAME
          ISWANR=1
          FNAM26=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUTBNICE') THEN
          IBEDOT=103
          OPEN(IBEDOT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM23=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUTSMS2 ') THEN
          ISMSFM2=81
          OPEN(ISMSFM2,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
          FNAM27=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'OUTBNWAV') THEN
          IWAVOT=82
          OPEN(IWAVOT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY'
     +         ,IOSTAT=IOERR)
          IF(IOERR .NE. 0) THEN
            CALL IOSMSG(IOERR)
            IERMSG=1
          ENDIF
          FNAM28=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!EFa jul07, ADD STAGE FLOW TABLE INPUT
        ELSEIF(ID .eq. 'STFLFIL ') THEN
          INSFL=39
          OPEN(INSFL,FILE=FNAME,STATUS='OLD',FORM='FORMATTED')
          FNAM39=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN03 ADD WEIGHTING ASCII  FILE
        ELSEIF(ID .eq. 'INWGT   ') THEN
          INWGT=10
          OPEN(INWGT,FILE=FNAME,STATUS='OLD',FORM='FORMATTED')
          FNAM36=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN03 ADD WEIGHTING BINARY FILE
        ELSEIF(ID .eq. 'INBNWGT ') THEN
          INBNWGT=10
!msf version          OPEN(INBNWGT,FILE=FNAME,STATUS='OLD',FORM='BINARY')
cipk dec version       OPEN(INBNWGT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(INBNWGT,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM37=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN03 ADD WEIGHTING ASCII STRESS FILE FOR WEIGHTING
        ELSEIF(ID .eq. 'INSTRESS') THEN
          INSTR=15
          OPEN(INSTR,FILE=FNAME,STATUS='OLD',FORM='FORMATTED')
          FNAM27=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .eq. 'INBNSTRS') THEN
          INBNSTR=15
!msf version          OPEN(INBNSTR,FILE=FNAME,STATUS='OLD',FORM='BINARY')
cipk dec version       OPEN(INBNSTR,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
      OPEN(INBNSTR,FILE=FNAME,STATUS='UNKNOWN',ACCESS='TRANSPARENT')
          FNAM29=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN03 ADD RM1 input file
        ELSEIF(ID(1:5) .eq. 'INRM1') THEN
          IGEO=0
          IFILE=60
          OPEN(IFILE,FILE=FNAME,STATUS='OLD',FORM='FORMATTED')
          FNAM2=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'OUTWGT  ') THEN
          IOWGT=16
          OPEN(16,FILE=FNAME,STATUS='UNKNOWN')
          FNAM30=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'OUTBNWGT') THEN
          IOBNWGT=17
!msf version          OPEN(17,FILE=FNAME,STATUS='OLD',FORM='BINARY')
cipk dec version          OPEN(17,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
      OPEN(17,FILE=FNAME,STATUS='OLD',ACCESS='TRANSPARENT')
          FNAM31=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF(ID .EQ. 'INSRCORD') THEN
          ICORDIN=19
          OPEN(19,FILE=FNAME,STATUS='OLD')
          FNAM32=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN05 ADD INCSTR input file
        ELSEIF(ID(1:6) .eq. 'INCSTR') THEN
          INCSTR=20
          OPEN(INCSTR,FILE=FNAME,STATUS='OLD',FORM='FORMATTED')
          FNAM33=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN05 ADD INTIMS input file
        ELSEIF(ID(1:6) .eq. 'INTIMS') THEN
          INTIMS=22
          OPEN(INTIMS,FILE=FNAME,STATUS='OLD',FORM='FORMATTED')
          FNAM34=FNAME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CIPK JUN05 ADD MESH OUTPUT file FROM TRIANGULATION
        ELSEIF(ID(1:7) .eq. 'MESHOUT') THEN
          IMESHOUT=23
          OPEN(IMESHOUT,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED')
          FNAM35=FNAME
!********************************************************
!
!     DJW 15/07/04, Adds Hook for reading in the initial conditions for Temp, Salt and Sed concentrations.
!        
!********************************************************
        ELSEIF(ID.eq. 'INITCONS') THEN
	    wbm_InitCons = .TRUE.
	    wbm_InitConsFile = FNAME
!********************************************************        
!
!     DJW 15/02/05, Adds Hook for reading in the rock levels 
!        
!********************************************************
        ELSEIF(ID.eq.'SCOURLIM') THEN
	    wbm_ScourLim = .TRUE.
	    wbm_ScourLimFile = FNAME
!********************************************************
CIPK MAY06
        ELSEIF(ID .EQ. 'AMASSOUT') THEN
          IMASSOUT=24
          OPEN(IMASSOUT,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED')
          FNAM38=FNAME
      ELSEIF(ID .EQ. 'ENDFIL  ') THEN
        GO TO 210
      ENDIF

      GO TO 200
  210 CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

cipk sep99
      if(iermsg .eq. 1) then
        write(*,*) 'Execution terminated after errors opening file(s)'
CIPK SEP04 CREATE ERROR FILE
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        write(75,*) 'Execution terminated after errors opening file(s)'
        stop
      endif


!NiS,mar06,comment: LAYOUTBLOCK for output file
cipk dec01 add geometry file name to information
c         first the directory, then the file name
CIPK JUL02 REMOVE CONDITION      IF(IRMAFM .EQ. 71) THEN
CIPK      START FORMING HEADER
      DO J=11,1000
        HEADER(J:J)=' '
      ENDDO
      HEADER(1:10)='RMA10     '
      CALL DATE_AND_TIME (DATEC,TIMEC,ZONEC,DTI)
      HEADER(11:20)=DATEC
      HEADER(21:30)=TIMEC
      HEADER(31:40)=ZONEC
      HEADER(201:248)=FNAMD(1:48)
      HEADER(251:298)=FNAM2(1:48)
CIPK JUL02ENDIF

cipk nov97 open message file
CIPK SEP04
      IF(IMESOUT .EQ. 0) THEN
        OPEN(75,FILE='MESSRM10.OUT')
      ENDIF

      IF(LOUT .EQ. 0) THEN
        WRITE(*,*) ' ERROR ---- NO OUTPUT FILE DEFINED'
        WRITE(*,*) '   EXECUTION TERMINATED'
CIPK SEP04 CREATE ERROR FILE
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        WRITE(75,*) ' ERROR ---- NO OUTPUT FILE DEFINED'
        WRITE(75,*) '   EXECUTION TERMINATED'
        STOP
      ENDIF

!NiS,may06: global use of geometry file name and restart file name
      modellein = FNAM2
      modellrst = FNAM3
!-

      WRITE(LOUT,6010) FNAM0
 6010 FORMAT(' RMA-10 INPUT FILE NAME  ',A96)
      IF(MMET .GT. 0) WRITE(LOUT,6016) FNAM12
 6016 FORMAT(' INPUT MET FILE NAME     ',A96)
      IF(IBUP .GT. 0) WRITE(LOUT,6011) FNAM11
 6011 FORMAT(' INPUT BC  FILE NAME     ',A96)
      IF(IFILE .GT. 0) WRITE(LOUT,6012) FNAM2
 6012 FORMAT(' INPUT GEOMETRY FILE NAME',A96)
      IF(NB .GT. 0) WRITE(LOUT,6013) FNAM3
 6013 FORMAT(' INPUT RESTART FILE NAME ',A96)
      IF(NLL .GT. 0) WRITE(LOUT,6014) FNAM4
 6014 FORMAT(' OUTPUT RESTART FILE NAME',A96)
      IF(NOPT .GT. 0) WRITE(LOUT,6015) FNAM5
 6015 FORMAT(' OUTPUT RESULTS FILE NAME',A96)
      IF(NIPT .GT. 0) WRITE(LOUT,6017) FNAM7
 6017 FORMAT(' INPUT VELOCITY FILE NAME',A96)
      IF(NAINPT .GT. 0) WRITE(LOUT,6018) FNAM8
 6018 FORMAT(' OUTPUT 2-D GEO FILE NAME',A96)
      IF(IFOT .GT. 0) WRITE(LOUT,6019) FNAM9
 6019 FORMAT(' OUTPUT 3-D GEO FILE NAME',A96)
C      IF(ISPRT .GT. 0) WRITE(LOUT,6020) FNAM10
C 6020 FORMAT(' SELECTED NODE FILE NAME ',A96)
      IF(IFIT .GT. 0) WRITE(LOUT,6021) FNAM11
 6021 FORMAT(' INPUT 3-D GEO FILE NAME ',A96)
      IF(IHUNIT .GT. 0) WRITE(LOUT,6024) FNAM14
 6024 FORMAT(' INPUT TIDAL GRAPH FILE NAME ',A96)
      IF(IQUNIT .GT. 0) WRITE(LOUT,6025) FNAM15
 6025 FORMAT(' INPUT HYDROGRAPH FILE NAME ',A96)
      IF(IQEUNIT .GT. 0) WRITE(LOUT,6026) FNAM13
 6026 FORMAT(' INPUT ELEMENT INFLOW FILE NAME ',A96)
      IF(KEY .GT. 0) WRITE(LOUT,6027) FNAM16
 6027 FORMAT(' INPUT TIDAL HARMONIC FILE NAME ',A96)
CIPK AUG98 ADD OUTPUT
      IF(IWINDIN .EQ. 69) WRITE(LOUT,6028) FNAM17
 6028 FORMAT(' INPUT BINARY WIND FILE NAME    ',A96)
      IF(IWINDIN .EQ. 70) WRITE(LOUT,6029) FNAM17
 6029 FORMAT(' INPUT ASCII WIND FILE NAME     ',A96)
CIPK MAR00
      IF(IRMAFM .EQ. 71) WRITE(LOUT,6030) FNAM18
 6030 FORMAT(' OUTPUT RMA FORMAT FILE NAME     ',A96)
      IF(IOMET .EQ. 72) WRITE(LOUT,6031) FNAM19
 6031 FORMAT(' OUTPUT MET FILE NAME     ',A96)
cipk jul01
      IF(IOCON .EQ. 21) WRITE(LOUT,6032) FNAM20
 6032 FORMAT(' OUTPUT CONTINUITY FILE NAME     ',A96)

cipk MAY02
      IF(IWVIN .EQ. 101) WRITE(LOUT,6033) FNAM21
 6033 FORMAT(' INPUT WAVE DATA FILE NAME     ',A96)
      IF(IWVFC .EQ. 102) WRITE(LOUT,6034) FNAM22
 6034 FORMAT(' INPUT SURFACE STRESS FILE NAME     ',A96)
      IF(IBEDOT .EQ. 103) WRITE(LOUT,6035) FNAM23
 6035 FORMAT(' OUTPUT BED DATA FILE NAME     ',A96)

CIPK AUG02
      IF(ISMSGN .EQ. 61) WRITE(LOUT,6042) FNAM2                 !NiS,mar06,comment: unit number should be 60 not 61
 6042 FORMAT(' INPUT SMS BIN GEOMETRY FILE NAME     ',A96)
C      IF(IBEDOT .EQ. 103) WRITE(LOUT,6043) FNAM23
C 6043 FORMAT(' OUTPUT ICE DATA FILE NAME     ',A96)
      IF(ISMSFM .EQ. 73) WRITE(LOUT,6036) FNAM24
 6036 FORMAT(' OUTPUT SMS RESULTS FILE NAME     ',A96)
      IF(ISMSFM1 .EQ. 74) WRITE(LOUT,6037) FNAM25
 6037 FORMAT(' OUTPUT SMS QUALITY RESULTS FILE NAME     ',A96)

CIPK JUN03
      IF(INWGT .EQ. 10) WRITE(LOUT,6038) FNAM36
 6038 FORMAT(' INPUT ASCII WEIGHTING FILE NAME     ',A96)
      IF(INSTR .EQ. 15) WRITE(LOUT,6039) FNAM27
 6039 FORMAT(' INPUT STRESS DATA FILE NAME     ',A96)
      IF(INBNWGT .EQ. 10) WRITE(LOUT,6040) FNAM37
 6040 FORMAT(' INPUT BINARY WEIGHTING FILE NAME     ',A96)
      IF(INBNSTR .EQ. 15) WRITE(LOUT,6049) FNAM29
 6049 FORMAT(' INPUT BINARY STRESS DATA FILE NAME     ',A96)
      IF(IOWGT .NE. 0) THEN
        WRITE(LOUT,6050) FNAM30
 6050   FORMAT(' OUTPUT ASCII MESH WEIGHTING FILE NAME  ',A96)
      ENDIF
      IF(IOBNWGT .NE. 0) THEN
        WRITE(LOUT,6051) FNAM31
 6051   FORMAT(' OUTPUT BINARY MESH WEIGHTING FILE NAME  ',A96)
      ENDIF
      IF(ICORDIN .NE. 0) THEN
        WRITE(LOUT,6044) FNAM32
 6044   FORMAT(' INPUT COORDINATE FILE NAME  ',A96)
      ENDIF

      IF(IWVFC .EQ. 104) WRITE(LOUT,6045) FNAM22
 6045 FORMAT(' INPUT WAVE DATA DIRECTORY FILE NAME     ',A96)
      IF(ISWANR .EQ. 1) WRITE(LOUT,6046) FNAM26
 6046 FORMAT(' INPUT SWAN CONTROL FILE NAME     ',A96)
      IF(ISMSFM2 .EQ. 81) WRITE(LOUT,6047) FNAM27
 6047 FORMAT(' OUTPUT SMS WAVE DATA FILE NAME     ',A96)
      IF(IWAVOT .EQ. 82) WRITE(LOUT,6048) FNAM28
CIPK JUN05
 6048 FORMAT(' OUTPUT RMA WAVE DATA FILE NAME     ',A96)
!EFa jul07
      IF(insfl .EQ. 39) WRITE(LOUT,6056) FNAM39
 6056 FORMAT(' INPUT STAGE-FLOW BOUNDARIES DATA FILE NAME     ',A96)
!-
      IF(INCSTR .EQ. 20) WRITE(LOUT,6052) FNAM33
 6052 FORMAT(' INPUT CONTROL STRUCTURE DATA FILE NAME     ',A96)
      IF(INTIMS .EQ. 22) WRITE(LOUT,6053) FNAM34
 6053 FORMAT(' INPUT TIME SERIES DATA FILE NAME     ',A96)
      IF(IMESHOUT .GT. 0) WRITE(LOUT,6054) FNAM35
 6054 FORMAT(' OUTPUT MESHED RM1 FORMAT FILE NAME   ',A96)
CIPK MAY06
CIPK SEP06 CHANGE IMASSOUT
      IF(IMASSOUT .GT. 0) WRITE(LOUT,6055) FNAM38
 6055 FORMAT(' OUTPUT MASS BALANCE FILE NAME   ',A96)
C-
        NSCR=9
C       IVS=8
C      OPEN(IVS,FILE='FBUF1',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
C-
      ENDIF
C-
      RETURN
      END
c
C************************************************************************

c
C************************************************************************

      function lenstr(str,iswt)
c
c  Find length of string (position of last non-blank character or period)
c
      character*(*)  str
      n = len(str)
      lenstr = n
      do 10  i=1,n
         idx = i
         if(iswt .eq. 0) then
           if (str(idx:idx) .eq.  '.')  then
              lenstr = idx-1
              return
           endif
         endif
         if(str(idx:idx) .eq. ' ') then
            lenstr = idx-1
            return
         endif
  10  continue
      return
      end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine iosmsg(ioerr)
        write(*,*) 'Cannot open file, error reported'
      return
      end
