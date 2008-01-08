C     Last change:  WP   11 Dec 2007    5:14 pm
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
      INTEGER :: n,m,a,LT, PA, PQ, PB
!-
      MAXP = 0
      MAXE = 0
!NiS,may06: Initialize MaxA and MaxT for Kalypso-2D format input file
      MaxA = 0
      MaxT = 0
!-
!nis,nov07: introducing counter of polynomials per node
      MaxPolyA = 1
      MaxPolyQ = 1
      MaxPolyB = 1
!-

!nis,nov06: Initialize MaxLT, the dimensioner for the number of Line transitions
      MaxLT = 0
!-

      NCLL = 0
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
!nis,may07
!Add midside node for polynom approach
          !NiS,mar06: Additional parameter 'A' in comparison to other read-subroutines; the parameter gives back the number of arcs
          !nis,feb07: Add the counter of midside nodes of Flow1DFE elements
          call RDKALYPS (N, M, A, PA, PQ, PB, LT, 1)
          !call RDKALYPS(N,M,A,LT,FFFMS,AddMS,1)
          !-
          REWIND IFILE
          WRITE(*,*)' back from rdkalypso'
          WRITE(*,*)' N,M,A,PA,PQ,PB,LT= ',N,', ',M,', ',A,', ',
     +                              PA, ', ', PQ,', ', PB,', ', LT
          GO TO 100
        end if
!-
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
      !nis,sep07
      !ERROR - SUSPICIOUSLY LARGE NODE NUMBER OR ELEMENT NUMBER DETECTED
      IF(N .GT. 500000) then
        call ErrorMessageAndStop(1102, N, 0.0d0, 0.0d0)
      ELSEIF (M .GT. 500000) THEN
        call ErrorMessageAndStop(1201, M, 0.0d0, 0.0d0)
      endif
      !-

      !the setting of maximum dimensions of the geometry data into the variables MaxE and MaxP
      MELL = M + 1

!TOASK: Why lower limit
CIPK JUN05 SET LOWER LIMIT
      !deactivate lower limit
      !IF(MELL .LT. 1000) MELL = 1000
      MAXE = MELL
      MAXP = N + 1

      !nis,comment
      !the usage of MELL is no more necessary because it was
      !oringinary the maximum value for the number of elements, set in a (old) module; as it is
      !mentioned in the latest documentation so far; borders are now set dynamic, so MELL is no
      !more necessary and should be replaced. If this value is still used somewhere there might
      !occur some problems with the execution of the compiled program, therefore it will be left
      !here.


!if the input file is in KALYPSO-2D format the size of allocatable arc array is not neglectable
        if (igeo==2) then
          MAXA = A + 1
          !Adding 1D-2D-Transitionline-array counter
          MaxLT = LT
          MaxPolyA = PA
          MaxPolyQ = PQ
          MaxPolyB = PB
          !-
        else
          MAXA = 0
          !Adding 1D-2D-Transitionline-array counter
          MaxLT = 0
          MaxPolyA = 1
          MaxPolyQ = 1
          MaxPolyB = 1
          !-
        end if
!-

      RETURN
      END
