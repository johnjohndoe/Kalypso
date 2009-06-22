C     Last change:  NIS  13 Jan 2008    7:44 pm
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE MAY 03 2004 ALLOW FOR LOAD APPLIED ONLY AS MASS
CIPK  LAST UPDATE JAN 14 2003 ADD MOMENTUM FOR INFLOWS
cipk  last update July 4 2001 fix bug to correct interpolation for 3-d case
cipk  last update  March 2001 allow for rainfall/evap on surface for 3-d
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
cipk  last update Feb 4 1998 Fix format
CIPK  LAST UPDATE DEC 16 1997 TEST FOR LIMITS
cipk  last update Dec 9 1997  initialize SIDF correctly
      SUBROUTINE ELFLOWS(IBIN)
      USE BLK10MOD
      USE BLK11MOD
      USE PARAMMOD
      USE ParaKalyps
      SAVE
C-
C...... Input special boundary conditions
C-
CIPKJAN94 V4.5
CIPK DEC97      PARAMETER (NQBS=20)
CIPKJAN94 V4.5 END CHANGE
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
cipk jan03  ADD AN EXTRA 8 CHARACTERS
      CHARACTER*8 DLINEXTRA
      COMMON /DLINF/ DLINEXTRA
      DIMENSION QQAL(3)
      
      integer :: ioerror
CIPKFEB94 V4.5
      ALLOCATABLE
     +          NEQ(:)
CIPK DEC97 CHANGE LIMITS
      DATA ISTDY/0/
C
CIPKFEB94 V4.5
      IF(ISTDY .EQ. 0) THEN
        ALLOCATE (NEQ(NELDS))
        NEQ=0
        ISTDY=1
      ENDIF
cipk dec97
      do n=1,ne
        sidf(n)=0.
      enddo
C-
C...... Input element inflows
C-
cipk APR96 major changes start here
      IEGEN=0
CIPK DEC97 CHANGE LIMITS
      DO N=1,NELDS
        IF(NEQ(N) .NE. 0) THEN
          IEGEN=IEGEN+1
          IF(IEGEN .EQ. 1) THEN
            WRITE(LOUT,6043)
          ENDIF
          J=NEQ(N)
          TIME=TET
cipk jan96  add logic for day of year
          IF(TIME .GT. 24.) THEN
            DAYNOW=DAYOFY+1
            TIME=TIME-24.
          ELSE
            DAYNOW=DAYOFY
          ENDIF
          IF(MOD(IYRR,4) .EQ. 0) THEN
            IYDAYS=366
          ELSE
            IYDAYS=365
          ENDIF
          IYRN=IYRR
          IF(DAYNOW .GT. IYDAYS) THEN
            IYRN=IYRR+1
            DAYNOW=DAYNOW-IYDAYS
          ENDIF
          CALL GETEQ(J,QF,QQAL,IYRN,DAYNOW,TIME,TETH,NEST,LN)
          IF(J .EQ. -9999) THEN
            WRITE(LOUT,6042) QF,QQAL,EINX(1),EINY(1)
            DO J=1,NE
CIPK LOGIC FOR LAYERS MOVED TO LNFLO
cipk mar01  go to LNFLO with NEST increased by 2 (for recognition)
CIPK JAN03  ADD MOMENTUM TERMS
              ELINX=EINX(J)
              ELINY=EINY(J)
              NEST_Copy = Nest + 2
              CALL LNFLO(J,QF,QQAL,NestCopy,LN,ELINX,ELINY)
cipk mar01              CALL LNFLO(J,QF,QQAL,NEST,LN)
CIPK NOV97 END CHANGES
            ENDDO
          ELSEIF(J .LT. 0) THEN
            DO JJ=1,NE
              IF(IMAT(JJ) .EQ. -J) THEN
CIPK LOGIC FOR LAYERS MOVED TO LNFLO
cipk mar01  go to LNFLO with NEST increased by 2 (for recognition)
CIPK JAN03  ADD MOMENTUM TERMS
                ELINX=EINX(JJ)
                ELINY=EINY(JJ)
                Nest_Copy = Nest + 2
                CALL LNFLO(JJ,QF,QQAL,NEST_Copy,LN,ELINX,ELINY)
cipk mar01              CALL LNFLO(JJ,QF,QQAL,NEST,LN)
CIPK NOV97 END CHANGES
              ENDIF
            ENDDO
          ELSE
CIPK LOGIC FOR LAYERS MOVED TO LNFLO
CIPK JAN03  ADD MOMENTUM TERMS
            IF(LN .GT. 1) THEN
	        JJJ=NEREF(J)+LN-1
              ELINX=EINX(JJJ)
              ELINY=EINY(JJJ)
	      ELSE
              ELINX=EINX(J)
              ELINY=EINY(J)
	      ENDIF
            CALL LNFLO(J,QF,QQAL,NEST,LN,ELINX,ELINY)
CIPK NOV97 END CHANGES
            WRITE(LOUT,6038) J,QF,QQAL,ELINX,ELINY
          ENDIF
        ENDIF
      ENDDO
  201 CONTINUE
      IF(ID(1:3) .EQ. 'EFA') THEN
        !counter of elements inflows
        IEGEN = IEGEN + 1
CIPK DEC97 REFINE TEST FOR TOO MANY ELEMENT FLOWS
        IF (IEGEN > NELDS)
           !ERROR - TOO MANY ELEMENT INFLOWS.
     +     CALL ErrorMessageAndStop (1007, 0, 0.0D0, 0.0D0)

CIPK DEC97 END ADDITION
        IF (IEGEN .EQ. 1) THEN
          WRITE (LOUT, 6029)
        ENDIF
        IF(NEQ (IEGEN) .EQ. 0) THEN
cipk jan03          READ(DLIN,5029) N,LN,NEST,SF,QQAL,LABL
          ioerror = 0
          READ (DLIN, 5029, iostat = ioerror)
     +         N, LN, NEST, SF, QQAL, LABL, ELINR, ELINT
          !read (dlinextra, 5030) ELINT
          ELINX = ELINR * COS (ELINT)
          ELINY = ELINR * SIN (ELINT)
          WRITE (LOUT, 6019) N, SF, QQAL, ELINX, ELINY
cipk nov97          READ(IBIN,7000) ID,DLIN
          call ginpt (ibin, id, dlin)
cipk APR96 save data to a scratch file
          if(isvs .eq. 1) then
            write (nscrin, 7000) id, dlin
          endif
C

          DO N=1,NE
CIPK LOGIC FOR LAYERS MOVED TO LNFLO
cipk mar01  go to LNFLO with NEST increased by 2 (for recognition)
CIPK JAN03            CALL LNFLO(N,SF,QQAL,NEST+2,LN)
            NEST_Copy = Nest + 2
            CALL LNFLO (N, SF, QQAL, NEST_Copy, LN, ELINX, ELINY)
CIPK NOV97 END CHANGES
          ENDDO
          IF(LABL .EQ. 1) THEN
            NEQ(IEGEN)=-9999
            WRITE(LOUT,6050)
 6050       FORMAT (/'   GLOBAL ELEMENT INFLOW HYDROGRAPH WILL BE INTERP
     +OLATED ')
          ENDIF
          GO TO 201
        ENDIF
      ENDIF
      IFPAS=0
  202 CONTINUE


      !individual element inflow
      IF(ID(1:3) .EQ. 'EFE') THEN
        IEGEN = IEGEN + 1

CIPK DEC97 REFINE TEST FOR TOO MANY ELEMENT FLOWS
        IF (IEGEN > NELDS)
           !ERROR - TOO MANY ELEMENT INFLOWS.
     +     call errorMessageAndStop (1007, 0, 0.0D0, 0.0D0)

CIPK DEC97 END ADDITION
        IFPAS = IFPAS + 1

        !write header of control output
        IF(IFPAS .EQ. 1) THEN
          WRITE (LOUT, 6041)
        ENDIF

        IF (NEQ (IEGEN) .EQ. 0) THEN

          !get the user's input data with new option for inflow angle if inflow has direction (for momentum equation)
          ioerror = 0
          READ (DLIN, 5029, iostat = ioerror)
     +          N, LN, NEST, SF, QQAL, LABL, ELINR, ELINT
          !meaning of input
          !n      element ID
          !ln     layer number to apply inflow (0 means all layers)
          !nest   0 -> value is specific load
          !       1 -> value is total load (has to be divided by the area of the element
          !qqal   concentrations (salinity, temperature, sediment)
          !labl   0 -> take given value
          !       1 -> read from time series file
          !elinr  inflow velocity
          !elint  angle of inflow velocity, measured in rad
     
          write(*,*) ioerror
          write(*,*) n, ln, nest, sf, qqal, labl, elinr, elint
          !read (dlinextra, 5030) ELINT

          !calculate x- and y- inflow
          ELINX = ELINR * COS (ELINT)
          ELINY = ELINR * SIN (ELINT)
CIPK MAY04
          IF (N < 0) THEN
            N = -N
            IF (LN > 1) THEN
              NNN = NEREF (N) + LN - 1
              INOFLOW (NNN) = 1
            ELSE
              INOFLOW (N) = 1
            ENDIF
          ELSE
            IF (LN > 1) THEN
              NNN = NEREF (N) + LN - 1
              INOFLOW (NNN) = 0
            ELSE
              INOFLOW (N) = 0
            ENDIF
          ENDIF

          !TODO:
          !This output is not correct; it's only displaying the direct input, but it can differ
          !between total inflow or specific inflow
          !control output, and read new line
          WRITE (LOUT, 6021) N, LN, SF, QQAL, ELINX, ELINY
          call ginpt(ibin,id,dlin)

cipk APR96 save data to a scratch file
          if (isvs == 1) then
            write (nscrin, 7000) id, dlin
          endif
C          WRITE(LOUT,6038) N,SF,ELINX,ELINY
          IF(LABL .EQ. 1) THEN
            NEQ (IEGEN) = N
            WRITE (LOUT, 6051)
 6051       FORMAT (/
     +'   INDIVIDUAL ELEMENT INFLOW HYDROGRAPH WILL BE INTERPOLATED ')
          ENDIF
CIPK LOGIC FOR LAYERS MOVED TO LNFLO
CIPK JAN03  ADD MOMENTUM TERMS
          if (imat (n) == 89 .and. NEST /= 0)
     +      SF = SF / (IntPolNo (n) + 1)

          CALL LNFLO (N, SF, QQAL, NEST, LN, ELINX, ELINY)

          !for interpolated elements
          if (IntPolNo (n) /= 0) then
            ForInterpolated: do i = 1, IntPolNo (n)
              if (IntPolElts (n, i) == 0) EXIT ForInterpolated
              WRITE (LOUT, 6021)
     +          IntPolElts (n, i), LN, SF, QQAL, ELINX, ELINY
              call LNFLO (IntPolElts (n, i),
     +          SF, QQAL, NEST, LN, ELINX, ELINY)
            end do ForInterpolated
          end if

CIPK JAN03          CALL LNFLO(N,SF,QQAL,NEST,LN)
CIPK NOV97 END CHANGES
          GO TO 202
        ENDIF
      ENDIF


CIPK NOV97 END CHANGES
CIPK JAN03
 5029 FORMAT( 3I8, 4F8.0, I8 ,3F8.0)
 5030 FORMAT(F8.0)
 5035 FORMAT( I8, 9F8.0 )
 6019 FORMAT(/' ALL ELEMENTS HAVE DISTRIBUTED INFLOW'/
     + /'               LAYER      INFLOW  INFLOW-QUAL'/
     +I20,1PE12.5,0P5F12.3)
CIPK JAN98 FIX FORMAT
 6020 FORMAT(/' THE FOLLOWING ELEMENTS HAVE DISTRIBUTED INFLOW'/
     + /'    ELT NO     LAYER      INFLOW  INFLOW-QUAL')
 6021 FORMAT(2I10,6F12.3)
 6033 FORMAT(/'   BOUNDARY CONDITION DEFINED AS FLOW ACROSS A LINE'//
     +'      LINE     LEVEL      FLOW DIRECTION        QUALITY')
 6034 FORMAT(2I10,5F10.1,I10)
 6035 FORMAT(/'   BOUNDARY CONDITION DEFINED AS ELEV ALONG A LINE'//
     +'      LINE     LEVEL           ELEV        QUALITY')
 6036 FORMAT(I10,8F15.1)
 6038 FORMAT(I11,E15.5,5F15.2)
 6042 FORMAT(' ALL EXCEPT',E15.5,5F15.2)
 6029 FORMAT(/'   GLOBAL ELEMENT INFLOW'//'           FLOW')
 6041 FORMAT(/'   INDIVIDUAL ELEMENT INFLOW'//'   ELEMENT',
     +'                  FLOW                 QUALITY                 ',
     +'  X-VEL       Y-VEL')
 6043 FORMAT(/'   INTERPOLATED ELEMENT INFLOW'//'    ELEMENT',
     +'                  FLOW                 QUALITY                 ',
     +'  X-VEL       Y-VEL')
 6040 FORMAT(/'   WIND SPEED =',F8.1,'  DIRECTION =',F8.0,' DEGREES'/)
 6045 FORMAT(/'   BOUNDARY CONDITION DEFINED AS STAGE FLOW ALONG A LINE'
     +//'      LINE             A1             A2             E0',
     +            '             C       DIRECTION        QUALITY')
 6048 FORMAT(/'   BOUNDARY CONDITION DEFINED AS SAL-TEMP-SED ON A LINE '
     +//'      LINE        SALINITY    TEMPERATURE       SEDIMENT')
 7000 FORMAT(A8,A72)
       RETURN
      END