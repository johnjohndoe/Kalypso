CIPK  LAST UPDATE SEP 6 2004 RENAME FILE ENTRY NAME  add error file
CIPK  LAST UPDATE JAN 25 2002 ADD 2000 AS AN OPTIONAL CENTURY
cipk  last update Apr 28 1996
C     Last change:  IPK  17 Jan 96   10:29 pm
cipk  last update Jan 17 1996
      subroutine GTIDE(TTIDE,IYRR,DAYOFY,TIME,TDTIM,QQAL)
      
      use blk_ifu
      SAVE
cipk APR96 add common

!NiS,jul06: Add IWINDIN for correct memory interpretation
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,IWINDIN
!-

C
C     Generate time series of tidal elevations at a station 
C     from harmonic constants for a given starting time and 
C     a given station.
C
cipk jan96 change variable name
      real RH(16),SP(16),EQQ(16),ZP(16),FCT(16),JUDAYM(12,2)
     +    ,QQAL(3),QDAT(3)
      character comment*80,fnamt*32,prefix*26
cipk jan96 changes to permit earlier reading of filename
cipk jan96      DATA KEY/0/
      DATA KEYSWT/0/
      DATA JUDAYM/1,32,60,91,121,152,182,213,244,274,305,335,
     +            1,32,61,92,122,153,183,214,245,275,306,336/
C
C
C  Read data, print out tide for 25 hours
      IF (KEY .EQ. 0) THEN
        WRITE(*,*) 'Filename for tidal coefficients not defined'
        WRITE(*,*) 'Enter filename for tidal coefficients'
cipk sep04
        READ(*,4800) FNAMT
 4800   format(a32)
        key=13
        OPEN(UNIT=13,FILE=FNAMT,STATUS='OLD')
      ENDIF
      IF(KEYSWT .EQ. 0) THEN
        KEYSWT=1
cipk APR96 end changes
C
C     Read comment line.
C     Read year, month, day, and (hour * 100) of starting time 
C     from input data file.  
C     NOTE:  Equilibrium arguments and node factor reciprocals
C            of harmonic equation are specific to the starting date.
C            Be sure harmonic constants in data file are correct
C            for a given station and starting date.
C
C     Also read length of desired time series (in hours * 100.), 
C     and time increment for output (in hours * 100.).
C
         write(*,*) 'Processing tidal harmonic file'
         read (key,'(A80)') comment
         write (*,'(/,A80,/)') comment
         read (key,'(2x)')
         read(key,'(3i3,i5,2f10.0)')  iyr, imo, ida, ihr,  tdum, utilt
         
CIPK JAN02 ADD 2000 AS OPTIONAL TEST
         IF(IYR+1900 .NE. IYRR  .AND.  IYR+2000 .NE. IYRR) THEN
cipk sep04
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
           WRITE(*,*) 'Harmonics defined for wrong starting year'
           WRITE(*,*) 'Execution terminated'
           STOP
         ENDIF
         NLP=1
         IF(MOD(IYR,4) .EQ. 0) NLP=2
         JDAY=JUDAYM(IMO,NLP)+IDA-1
         HSTART=(JDAY-1)*24.+IHR/100.
         HDAT=(DAYOFY-1)*24.+TIME-TDTIM
         TCORR=HDAT-HSTART
         read (key,'(i5)') nhm
         tmax = tdtim + 50.
         delt = 1.0
C
         t0  = (ida-1)*24. + ihr/100. + (JDAY-1)*24.
         write (75,'(
     .    '' Starting Year Month Day and Hour of time series:'',/,
     .    9x,I5,I6,I4,F9.2,/)') IYR,IMO,IDA,float(IHR)/100.
C
C     Read harmonic constants for 16 partial tidal constituents.
C
         read (key,'(2X)')
         do 10 J=1,nhm
C
            read (key,'(5F10.0)') RH(J),SP(J),EQQ(J),ZP(J),FCT(J)
C
   10    continue
         READ(KEY,'(3F10.0)') QDAT
C
C     Write input echo
C
         write (75,60) nhm
   60 format(/' Harmonic constants for',i5,' partial',
     .             ' tidal constituents:',/
     .             '    K       RH(K)       SP(K)       EQQ(K)',
     .             '       ZP(K)      FCT(K)'/)
         do 20 K = 1,nhm
            write (75,'(I5,5E12.5)') K,RH(K),SP(K),
     .                                EQQ(K),ZP(K),FCT(K)
   20    continue
C
C     TLAG is the delay time (hours) of the arrival of high water
C     compared to the station for which these constants were computed.
C     UTILT is the deviation of the mean tidal elevation from
C     sea level.
C
ccc         read (inp,'(2X)')
ccc         read (inp,'(2F10.0)') TLAG,UTILT
            tlag=0
c         write (*,'(/,'' Delay time of arrival of high water'',/,
c     .     ''      compared to ref. station (hours) :  TLAG = '',
c     .  E10.3)') TLAG
         write (75,'('' Constant tilt                     :'',
     .     '' UTILT = '',E10.3)') UTILT
C
C_____Compute harmonic time series for tidal elevation_________________  
C
         write (75,'(/,''       Day     TIME (hrs)   WS ELEV     '',/)')
         R = 57.29578
         time = tdtim 
         idx = 0
  100       tidesum = 0.
            time1 = time - TLAG +TCORR
            do 110 J=1,nhm
C   
               tidesum = tidesum + RH(J) / FCT(J) * 
     .                COS( SP(J) * time1 + ( EQQ(J) - ZP(J) ) / R ) 
C
  110       continue
C
            tide = tidesum + UTILT
C
C            timout = time + t0+TCORR
            TIMOUT = TIME1
            idayout = timout/24.+1
            hrout = mod(timout,24.)
C            tidout = 3.28*tide/100.
            TIDOUT=TIDE
            write (75,'(I10,F10.3,F15.5)') idayout,hrout,tidout
C
            time = time + DELT
            if (time .le. tmax) go to 100
C
C  Generate tidal b.c. for this timestep
      ENDIF
c      ELSE
C
      tidesum = 0.
      time1 = tdtim+TCORR
      do 120 J=1,nhm
C   
         tidesum = tidesum + RH(J) / FCT(J) * 
     +           COS( SP(J) * time1 + ( EQQ(J) - ZP(J) ) / R ) 
C
  120 continue
C
      tide = tidesum + UTILT
      TTIDE = TIDE
          write(*,*)  '  tide for time ',time1, ' = ', ttide
         DO K=1,3
           QQAL(K)=QDAT(K)
         ENDDO
      return
C
      end
C

