!IPK  LAST UPDATE SEP 6 2004 RENAME FILE ENTRY NAME  add error file
!IPK  LAST UPDATE JAN 25 2002 ADD 2000 AS AN OPTIONAL CENTURY
!ipk  last update Apr 28 1996
!     Last change:  IPK  17 Jan 96   10:29 pm
!ipk  last update Jan 17 1996
      subroutine GTIDE(TTIDE,IYRR,DAYOFY,TIME,TDTIM,QQAL)
!
      use blk_ifu
      SAVE
!ipk APR96 add common
!
!NiS,jul06: Add IWINDIN for correct memory interpretation
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY
!      COMMON /IFU/ IQEUNIT,IHUNIT,IQUNIT,KEY,IWINDIN
!-
!
!
!     Generate time series of tidal elevations at a station 
!     from harmonic constants for a given starting time and 
!     a given station.
!
!ipk jan96 change variable name
      real RH(16),SP(16),EQQ(16),ZP(16),FCT(16),JUDAYM(12,2)            &
     &    ,QQAL(3),QDAT(3)
      character comment*80,fnamt*32,prefix*26
!ipk jan96 changes to permit earlier reading of filename
!ipk jan96      DATA KEY/0/
      DATA KEYSWT/0/
      DATA JUDAYM/1,32,60,91,121,152,182,213,244,274,305,335,           &
     &            1,32,61,92,122,153,183,214,245,275,306,336/
!
!
!  Read data, print out tide for 25 hours
      IF (KEY == 0) THEN
        WRITE(*,*) 'Filename for tidal coefficients not defined'
        WRITE(*,*) 'Enter filename for tidal coefficients'
!ipk sep04
        READ(*,4800) FNAMT
 4800   format(a32)
        key=13
        OPEN(UNIT=13,FILE=FNAMT,STATUS='OLD')
      ENDIF
      IF(KEYSWT == 0) THEN
        KEYSWT=1
!ipk APR96 end changes
!
!     Read comment line.
!     Read year, month, day, and (hour * 100) of starting time 
!     from input data file.  
!     NOTE:  Equilibrium arguments and node factor reciprocals
!            of harmonic equation are specific to the starting date.
!            Be sure harmonic constants in data file are correct
!            for a given station and starting date.
!
!     Also read length of desired time series (in hours * 100.), 
!     and time increment for output (in hours * 100.).
!
         write(*,*) 'Processing tidal harmonic file'
         read (key,'(A80)') comment
         write (*,'(/,A80,/)') comment
         read (key,'(2x)')
         read(key,'(3i3,i5,2f10.0)')  iyr, imo, ida, ihr,  tdum, utilt
!
!IPK JAN02 ADD 2000 AS OPTIONAL TEST
         IF(IYR+1900 /= IYRR .AND. IYR+2000 /= IYRR) THEN
!ipk sep04
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
           WRITE(*,*) 'Harmonics defined for wrong starting year'
           WRITE(*,*) 'Execution terminated'
           STOP
         ENDIF
         NLP=1
         IF(MOD(IYR,4) == 0) NLP=2
         JDAY=JUDAYM(IMO,NLP)+IDA-1
         HSTART=(JDAY-1)*24.+IHR/100.
         HDAT=(DAYOFY-1)*24.+TIME-TDTIM
         TCORR=HDAT-HSTART
         read (key,'(i5)') nhm
         tmax = tdtim + 50.
         delt = 1.0
!
         t0  = (ida-1)*24. + ihr/100. + (JDAY-1)*24.
         write (75,'(                                                   &
     &    '' Starting Year Month Day and Hour of time series:'',/,      &
     &    9x,I5,I6,I4,F9.2,/)') IYR,IMO,IDA,float(IHR)/100.
!
!     Read harmonic constants for 16 partial tidal constituents.
!
         read (key,'(2X)')
         do 10 J=1,nhm
!
            read (key,'(5F10.0)') RH(J),SP(J),EQQ(J),ZP(J),FCT(J)
!
   10    continue
         READ(KEY,'(3F10.0)') QDAT
!
!     Write input echo
!
         write (75,60) nhm
   60 format(/' Harmonic constants for',i5,' partial',                  &
     &             ' tidal constituents:',/                             &
     &             '    K       RH(K)       SP(K)       EQQ(K)',        &
     &             '       ZP(K)      FCT(K)'/)
         do 20 K = 1,nhm
            write (75,'(I5,5E12.5)') K,RH(K),SP(K),                     &
     &                                EQQ(K),ZP(K),FCT(K)
   20    continue
!
!     TLAG is the delay time (hours) of the arrival of high water
!     compared to the station for which these constants were computed.
!     UTILT is the deviation of the mean tidal elevation from
!     sea level.
!
!cc         read (inp,'(2X)')
!cc         read (inp,'(2F10.0)') TLAG,UTILT
            tlag=0
!         write (*,'(/,'' Delay time of arrival of high water'',/,
!     .     ''      compared to ref. station (hours) :  TLAG = '',
!     .  E10.3)') TLAG
         write (75,'('' Constant tilt                     :'',          &
     &     '' UTILT = '',E10.3)') UTILT
!
!_____Compute harmonic time series for tidal elevation_________________  
!
         write (75,'(/,''       Day     TIME (hrs)   WS ELEV     '',/)')
         R = 57.29578
         time = tdtim 
         idx = 0
  100       tidesum = 0.
            time1 = time - TLAG +TCORR
            do 110 J=1,nhm
!   
               tidesum = tidesum + RH(J) / FCT(J) *                     &
     &                COS( SP(J) * time1 + ( EQQ(J) - ZP(J) ) / R ) 
!
  110       continue
!
            tide = tidesum + UTILT
!
!            timout = time + t0+TCORR
            TIMOUT = TIME1
            idayout = timout/24.+1
            hrout = mod(timout,24.)
!            tidout = 3.28*tide/100.
            TIDOUT=TIDE
            write (75,'(I10,F10.3,F15.5)') idayout,hrout,tidout
!
            time = time + DELT
            if (time <= tmax) go to 100
!
!  Generate tidal b.c. for this timestep
      ENDIF
!      ELSE
!
      tidesum = 0.
      time1 = tdtim+TCORR
      do 120 J=1,nhm
!   
         tidesum = tidesum + RH(J) / FCT(J) *                           &
     &           COS( SP(J) * time1 + ( EQQ(J) - ZP(J) ) / R ) 
!
  120 continue
!
      tide = tidesum + UTILT
      TTIDE = TIDE
          write(*,*)  '  tide for time ',time1, ' = ', ttide
         DO K=1,3
           QQAL(K)=QDAT(K)
         ENDDO
      return
!
      end
!
!
