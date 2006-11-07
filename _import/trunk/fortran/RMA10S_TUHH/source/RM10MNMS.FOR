c           RMA-10
c           COPYRIGHT  
c           RESOURCE MODELLING ASSOCIATES PTY LTD 
c         9 DUMARESQ STREET 
c         GORDON 2072 
c         NSW AUSTRALIA 
c           JAN 1 2002


CIPK  LAST UPDATE APR 03 2001 CHANGE TO MOVE WSLL TO UPDATE
CIPK  LAST UPDATE JAN 12 2001 CHANGE AME TO AME1
cipk  last update Dec 21 2000 add projection option and gate structure
cipk  last update Aug 18 2000 test for two-d switch only for 3 iterations
cipk  last update Dec 17 1999 add test zero delta t
cipk  last update Nov 24 1999 add initializatin for DFCT
cipk  last update April 1 1999 add comments to output file area
cipk  last update Aug 7 1998    add to restart file for consistent restart
cipk  last update Mar 23 1998 refine elevation updating
CIPK  LAST UPDATE MAR 13 1998 
cipk  last update Jan 21 1998
CIPK  LAST UPDATE DEC 8 1997
C     Last change:  IPK  24 Nov 97    9:54 am
CIPK  LAST UPDATED NOVEMBER 13 1997
CIPK  LAST UPDATE APRIL 28 1997
CIPK  LAST UPDATE jAN 22 1997
CIPK  LAST UPDATED SEPT 26 1996
CIPK  LAST UPDATED MAY 1 1996
CIPK  LAST UPDATED SEPT 8 1995
      PROGRAM RMA10
C-
C........ MAIN PROGRAM FOR THREE-D HYDRODYNAMICS MODEL
C-                      (RMA-10)
C-
C-
C-      For update information see release notes
C-
c-
c                     REVISION 6.8H
c                         Apr 2001
c
c       This revision implements a modification to the heat budget routines so
c       the user may input dew point or wet bulb temeperature.  The model has 
c       been carefully tested ot ensure that it works with bot metric and English 
c       unit input of met data and and for metric and English units lengths.  NOTE
c       the model only operates in deg C.  The model has also been modified to
c       recognise the path of the input r11 file and switch directories appropriately.
C
c                     REVISION 6.8g
c                         Apr 2001
c
c       This revision implements a series of minor corrections and bug fixes
c       that arose from feedback from version 6.8f.  They influence 
c       applications that use element inflows, and COL option
C       A further correction has been applied to the equation dropouot option to
c       avoid accidental elimiation of depth equations that leads to instability.c
c
c                     REVISION 6.8f
c                         Apr 2001
c
c       A series of new options and improvements have been made:
c      (1)    An option to adjust time step based on a reference elevation.  
c             This option is accessed through the "TST" data line.
c      (2)    An option to automatically use as inflows at a continuity line, 
c             outflow salinity/temperatures/sediment concentrations from the
c             model modified to simulate passing through a power station.
c             This option is accessed through the "PWR" data line.
c             This option is limited to metric units only.
c      (3)    As part of the development of the "PWR" option the model now outputs
c             average salinity/temperature/sediment concentrations for continuity lines.
c      (4)    A new equation dropout option designed to reflect convergence history
c             when switching between components.  This option appears on the "CV" data line.
c             This option is experimental and should be used with caution.
c      (5)    A internal reordering processor is now available and may be used to speed solution.
c             This option appears on the "IOP" data line.  
c             This option is experimental and should be used with caution 
c             (please see note on the data line describing the "IOP" option).
c      (6)    A minor bug in the marshing option has been fixed

c                     REVISION 6.8e
c                         Mar 2001
c
c       Implements two additional options for energy losses through friction.
c       Manning coefficients may now vary with depth, see the "MAN" data line
c       in the instructions for details.  Additional drag may now be spread 
c       uniformly over the entire cross-section using "DRG" data lines.
c       Both these data inputs are option and operate for an element type number.
c
c                     REVISION 6.8d
c                         Feb 2001
c
c       Restructure tide gate routines to permit alternative testing
c
c
c                     REVISION 6.8c
c                         Jan 2001
c
c       Revise to add version 6.0 RMAGEN input compatibility
c       This option allows continuiyt lines to stored on geo file
c
c
c                     REVISION 6.8b
c                         Dec 2000
c
c       Add gate structure option.  Revise to ensure water surface 
c       elevation is saved in type RMA files.  Add time step
c       projection option.
c
C
c                     REVISION 6.8a
c                         Aug 2000
c
c       Clean up some minor bugs  
c
C
c                     REVISION 6.8
c                         Dec 1999
C
c       Add switch to allow transition to 2-d when depth below 
c       specified value
c 
c
c                     REVISION 6.7A
c                        Dec 1999
c
C       Clean up a few nuisance input data failures, fig bug in AZER for 
c       marsh elements
c
cc                     REVISION 6.7
c                       April 1999
C
c       Note that version number 6.6 was skipped
c       This version adds the off channel sloped conditions of 1-D elements
c       in RMA-2.  Input geo files are made compatible.
c 
c                     REVISION 6.5
c                       March 1998
c 
c       Add logic for direct solar input.
c       Fix initialisation error in routine marsh and formatting of
c       input
c
c
c                     REVISION 6.4
c                      January 1998
c 
c
c       Modifies the code to permit marsh porosity for 2-d elements
c
c
c
c                     REVISION 6.3(d)
c                     November 1997
c 
c       Modifies the treatment of internal sources for all element
c       types.  Note that as an unavoidable consequence the locations
c       of input for EFA and EFE data lines have been changed.  
c       Functionality of subroutine SBGEN has changed and two new 
c       routines ELFLOWS and LNFL:O have been added.
c
C                     REVISION 6.3(c) 
C                     June 1997
c
c       Extends this option to all binary files.
c
c                     REVISION 6.3(b)
C                     April 1997
c
C       Modifies the input structure to allow pure binary files 
C       for geometry and results files.
C
C                     REVISION 6.3(a)
C                     January 1997
C
C       Includes the Smagorinsky closure as an option for both 2-D 
c       and 3-D elements.  The model now allows equations to be 
c       dropped out of the solution when they pass a more rigid 
c       criteria than the standard convergence test.  The model 
c       accepts a factor times the original convergence tolerance.
C
C-                    REVISION 6.3
C                     NOVEMBER 1996
C
C       Contains a series of modifications designed to bring RMA-10 in
C       line with RMA-11 and RMA-2.  Files are now year, day, hour
C       stamped.  New options have been added to organize horizontal 
C       turbulence algorithms.

C-
C-                     REVISION 6.2
C-                      MAY 1996
C-
C-       This version revises the structure and from of input files
C-
C-
C-                     REVISION 6.1
C-                      FEB 1996
C-
C-       This version add a heat budget computation
C-
C-
C-                     REVISION 6.0
C-                      JULY 1995
C-
C-       This version has a totally revised input structure
C-
C-
C-                     REVISION 5.0
C-                      FEB 1994
C-
C-       This version has a number of changes aimed to make input of data
C-       both easier and consistent with RMA-2
C-
C-
C-                     REVISION 4.4
C-                      MAY 1993
C-
C-       This version has a series of changes to the coefficient routine
C-       to correct integration of the eddy turbulence and diffusion terms
C-       and to change roin to correctly compute surface density gradients.
C-
C-
C-                     REVISION 4.3
C-                      MAY 1992
C-
C-       This version contains has a number of changes:
C-       (1) A series of internal changes designed to better simulate
C-           horizontally stratified systems.
C-       (2) A modification to input stream defining layers so that as
C-           an alternative to defining layers by proportions, it is 
C-           possible to give absolute elevations.  These elevations must be
C-           entered in the transformed vertical coordinate system.
C-       (3) Implementation of alternate schemes for computation of the eddy
C-           coefficient and the diffusion coefficient.  The additional 
C-           options are based on the method proposed by Mellor and Yamada.
C-           The references for the methods are given in routines mellii and
C-           nellii.  The input stream has been altered to input a switch
C-           values from the keyboard or command file.
C-       (4) Options have been created to ensure that salinity, temperature
C-           and suspended sediment are treated as passive constituents that
C-           do not couple to the density terms.  When these options are used
C-           a conservative routing model appears that can be executed using
C-           velocities from a previous run.
C-       (5) An option has been created that permits the user to specify zero
C-           velocity at the bottom for three-dimensional nodes.
C-
C-
C-                    REVISION 4.2
C-                      MAY 1991
C-
C-      This version contains multiple constituents, options
C-      for using fixed values and linear basis functions for salinity,
C-      alternate format for 2-d geometric input.
C-
C-                    REVISION 4.1a
C-                      JULY 1989
C-

!NiS,may06: The program needs access to that module, because iaccyc must be accessable
      USE Blk10mod
!-

      LOGICAL PRESS_ANYKEY
      LOGICAL STARPROC
      CHARACTER*96 FNAM0,FNAMEX
      INTEGER*2 N1,STATUS
      n1=1

COMMENT OUT  "IF PRESS_ANYKEY.... "  AND   " N1=2 " STATEMENTS FOR DEBUGGING
      fnam0='RMA10s'
CCC      if (press_anykey(fnam0)) goto 99999
C        WRITE(*,'(A)') FNAM0
CCC        n1=2
cWP        CALL GETARG(N1,FNAM0,STATUS)
C        write(*,*) 'n1',n1,status
C        WRITE(*,'(A)') FNAM0

        WRITE(*,*) 'YOU ARE EXECUTING VERSION 3.5D OF RMA10S'
        WRITE(*,*)' COPYRIGHT  RESOURCE MODELLING ASSOCIATES PTY LTD'
        WRITE(*,*)' 9 DUMARESQ STREET'
        WRITE(*,*)' GORDON 2072'
        WRITE(*,*)' NSW AUSTRALIA' 
        WRITE(*,*)' MAR 1 2006'



c       write(*,*) 'THIS EXECUTABLE HAS SPECIAL CODING FOR EL DATA LINES'
cipk apr02
cWP        if(status .eq. -1  .or. fnam0(1:5) .eq. 'dummy') then
          WRITE(*,6000)
 6000     FORMAT(/' Enter RMA-10 input file name: ',/)
CIPK JUN05
cWP          call fileopen(fnam0)
          READ(*,*) FNAM0
CIPK JUN05 5000     FORMAT(A48)
cWP        endif
        WRITE(*,'(A)') FNAM0
CCC      FNAMEX='D:\Rmalf95\Rmagen\RMAGEN70\rmagen70\Release\rmagen70-3M.EX
CCC     +E'
CCC      if(starproc(FNAMEX ,'    ')) then
CCC         write(*,*) 'came out OK'
CCC	else
CCC	   write(*,*) 'came out bad'
CCC	   STOP
CCC	endif
C-
C...... Open files
C-
  !NiS,may06: The variable iaccyc shows the timestep to start from. For opening the files it is important,
  !           that the program acts on the assumption, that it will start with the first time step or steady
  !           state
      iaccyc = 1
  !-
      CALL FILE(1,FNAM0)
C-

        CALL GETGEO1
        
        CALL RMA10SUB

99999 continue

      STOP

      END
