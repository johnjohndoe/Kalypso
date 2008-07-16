CIPK  LAST UPDATE JUNE 25 2005 RESET LOWER LIMIT
      SUBROUTINE GETGEO1
      USE BLK10MOD

CIPK FEB05

cipk aug05      INCLUDE 'BLK10.COM'
      CHARACTER*6 HEADSH
      CHARACTER*8 ID8
      CHARACTER*1000 HEADR

      MAXP=0
      MAXE = 0
      NCLL=0
C-
C-.....READ EXTERNALLY GENERATED GEOMETRY FILE.....
C-

      IF(ISMSGN .NE. 0) THEN
        ILONG=1
CIPK FEB05
        CALL RDBIN(N,M,1)
	  REWIND IFILE
        GO TO 100
      ENDIF
      IF( IFILE .NE. 0 ) then

cipk jun03 add RM1 input capabilities
        IF(IGEO .EQ. 0) THEN
          CALL RDRM1(N,M,1)
          ILONG=1
	    REWIND IFILE
          GO TO 100
        ENDIF

cipk may02 Make CORD real*8 and NOP integer*4

        READ(IFILE,ERR=24) HEADSH
        IF(HEADSH .EQ. 'RMAGEN') THEN
          ILONG=1
          REWIND IFILE

          READ(IFILE) HEADR
          READ (IFILE,ERR=63) N,M
          REWIND IFILE

        ELSE
cipk feb01 lahey version        READ (IFILE,ERR=63)
          ILONG=0
            REWIND IFILE
          WRITE(75,*) 'READING SHORT FORMAT '
          READ (IFILE,ERR=63)

     1      N,M
        ENDIF
      ENDIF
   24 CONTINUE
   63 CONTINUE

  100 CONTINUE
	IF(N .GT. 500000  .OR.  M .GT. 500000) THEN
	  CLOSE(75)
	  OPEN(75,file='ERROR.OUT')
        WRITE(*,6300) N,M
        WRITE(75,6300) N,M
 6300   FORMAT(' SUSPICIOUSLY LARGE NODE NUMBER',I10
     +,'OR LARGE ELEMENT NUMBER',I10,' DETECTED'/
     +  '   EXECUTION TERMINATED')
      ENDIF
      MELL=M+1
CIPK JUN05 SET LOWER LIMIT
      IF(MELL .LT. 1000) MELL=1000
      MAXE=MELL
      MAXP=N+1
      RETURN
      END
