C     Last change:  K    15 Feb 2007    5:15 pm
CNis  LAST UPDATE MAR  XX 2006 Adding capabilities for Kalypso-2D-network-files
CIPK  LAST UPDATE JUNE 25 2005 RESET LOWER LIMIT
      SUBROUTINE GETGEO1
      USE BLK10MOD

CIPK FEB05

cipk aug05      INCLUDE 'BLK10.COM'
      CHARACTER*6 HEADSH
      CHARACTER*8 ID8
      CHARACTER*1000 HEADR

!NiS,mar06: adding declaration of hand-over-parameters between subroutines
      INTEGER :: n,m,a,LT
!-
!nis,feb07: Adding counter of midside nodes of Flow1DFE elements
      INTEGER :: FFFMS
!-
      MAXP=0
      MAXE = 0
!NiS,may06: Initialize MaxA and MaxT for Kalypso-2D format input file
      MaxA = 0
      MaxT = 0
!-

!nis,nov06: Initialize MaxLT, the dimensioner for the number of Line transitions
      MaxLT = 0
!-
!nis,feb07: Initialize MaxFFFMS, the dimensioner of the number of midside nodes of Flow1DFE elements
      MaxFFFMS = 0
!-

      NCLL=0
C-
C-.....READ EXTERNALLY GENERATED GEOMETRY FILE.....
C-

      IF(ISMSGN .NE. 0) THEN
        ILONG=1
CIPK FEB05
        !NiS,mar06,com: reading SMS-input-file node and element numbers
        CALL RDBIN(N,M,1)
	  REWIND IFILE
        GO TO 100
      ENDIF
      IF( IFILE .NE. 0 ) then

cipk jun03 add RM1 input capabilities
        !NiS,mar06,com: reading ASCII-RM1-formatted-input-file node and element numbers
        IF(IGEO .EQ. 0) THEN
          CALL RDRM1(N,M,1)
          ILONG=1
	    REWIND IFILE
          GO TO 100
        ENDIF

!NiS,mar06: adding the option of reading KALYPSO-2D input files for determination of allocatable array size
        if (iGEO == 2) then
          !NiS,mar06: Additional parameter 'A' in comparison to other read-subroutines; the parameter gives back the number of arcs
          !nis,feb07: Add the counter of midside nodes of Flow1DFE elements
          !call RDKALYPS(N,M,A,LT,1)
          call RDKALYPS(N,M,A,LT,FFFMS,1)
          !-
          REWIND IFILE
          WRITE(*,*)' back from rdkalypso'
          WRITE(*,*)' N,M,A,LT= ',N,', ',M,', ',A,', ', LT, ', ', FFFMS
          GO TO 100
        end if
!-

cipk may02 Make CORD real*8 and NOP integer*4

         !NiS,mar06,com: reading standard-binary-input-file node and element numbers
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
      MELL=M+1                                         	!the setting of maximum dimensions of the geometry data into the variables MaxE and MaxP
CIPK JUN05 SET LOWER LIMIT                             	!provides the oportunity to dimension the allocatable arrays for the geometry input.
      IF(MELL .LT. 1000) MELL=1000                     	!The way taken here is not very elegant, because firstly the variable MELL seems not be
      MAXE=MELL                                        	!used in another place and secondlyit can be replaced by just one statement concerning
      MAXP=N+1                                         	!MaxE, that is:
      							!MAXE=MAX(1000,M+1);
                                                        !the usage of MELL is no more necessary because it was
                                                        !oringinary the maximum value for the number of elements, set in a (old) module; as it is
                                                        !mentioned in the latest documentation so far; borders are now set dynamic, so MELL is no
                                                        !more necessary and should be replaced. If this value is still used somewhere there might
                                                        !occur some problems with the execution of the compiled program, therefore it will be left
                                                        !here.
                                                        !Nis,mar06,comment

!NiS,mar06: if the input file is in KALYPSO-2D format the size of allocatable arc array is not neglectable
        if (igeo==2) then
	  MAXA=A+1
          !nis,dec06: Adding 1D-2D-Transitionline-array counter
          MaxLT=LT
          !-
          !nis,feb07: Adding counter of midside nodes of Flow1DFE elements
          MaxFFFMS = FFFMS
          !-
        else
          MAXA=0
          !nis,dec06: Adding 1D-2D-Transitionline-array counter
          MaxLT=0
          !-
          !nis,feb07: Adding counter of midside nodes of Flow1DFE elements; dummy value zero
          MaxFFFMS = 0
          !-
        end if
!-

      RETURN
      END
