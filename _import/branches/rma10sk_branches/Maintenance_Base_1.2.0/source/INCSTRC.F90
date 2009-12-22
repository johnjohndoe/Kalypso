!     Last change:  WP   12 Jun 2008    1:11 pm
!C     Last change:  WP   14 Feb 2008    3:47 pm
SUBROUTINE INCSTRC


USE CSVAR, only: nsets, nrowm, ncolm, nrowcs, ncolcs, hrw, hcl, flwcs
USE BLK10MOD, only: maxweir, incstr, contrStructures, nctref
use mod_ControlStructure

implicit none

!loop counter
integer (kind = 4) :: j, l

!i/o variables
character (len = 8) ::idc
character (len = 72) :: dlinc
character (len = 250) :: linetoeval
character (len = 3)   :: lineid
integer (kind = 4) :: iostatvar

!data sets for QCurve definition
type (controlStructure), pointer :: tmpCstrc
integer (kind = 4) :: weirCounter
integer (kind = 4) :: weirTypeID
real (kind = 8) :: Qinner, euw, eow

!data sets for tabular data definition
integer (kind = 4) :: nsetsCounter
integer (kind = 4) :: nlo, nhi, id1
integer (kind = 4) :: nrow, ncol

!Examine size of control structure data (both types: QCurves and tabular data)
!--------------------------------------
iostatvar = 0
!QCurve control structures
MaxWeir = 0
!tabular control structures
nsets=0
nrowm=0
ncolm=0

readWeirSize: do
  !read new line
  call ginpt (incstr,idc,dlinc)
  !check for type of control structure and count
  if (idc(1:2) == 'TI') then
    cycle readWeirSize
  elseif (idc(1:7) == 'ENDDATA') then
    exit readweirsize
  elseif (idc(1:3) == 'DLI') then
    maxWeir = maxWeir + 1
  elseif (idc(1:3) == 'IDC') then
    read (dlinc, '(3i8)') id1, nrow, ncol
    if (nrow > nrowm) nrowm = nrow
    if (ncol > ncolm) ncolm = ncol
    nsets = nsets + 1
  end if
end do readWeirSize

!rewind input file to really evaluate data
rewind incstr

!prepare control structure array for QCurves
if (maxweir > 0) allocate (ContrStructures (1: MaxWeir))

!prepare control structure arrays using tabular data
if (nrowm /= 0) then
  !NROWCS :: number data lines (number of downstream water levels)
  !NCOLCS :: number of data columns (number of upstream water levels)
  !HRW    :: interpolated water stage in line (downstream)
  !HCL    :: interpolated water stage in column (upstream)
  !FLWCS  :: 3D-discharge table array (weir number, line number, column number)
  allocate (nrowcs (nrowm), ncolcs (ncolm), hrw (nsets, nrowm), hcl (nsets, ncolm), flwcs (nsets, nrowm, ncolm))
endif

!read cstrc data
weirCounter = 0
nsetsCounter = 0

!Read data and store to proper arrays
readCstrcData: do
  read (INCSTR, '(a)', iostat = iostatvar) lineToEval

  !end of file reached
  if (iostatvar /= 0) EXIT readCstrcData

  if (lineToEval(1:7) == 'ENDDATA') then
    EXIT readCstrcData

  !file headline; nothing happens
  elseif (lineToEval (1:2) == 'TI') then
    cycle readCstrcData

  !new cstrc with QCurve definition data block
  elseif (lineToEval(1:3) == 'DLI') then
    read (lineToEval, *) lineID, weirTypeID
    !create new weir
    weirCounter = weirCounter + 1
    ContrStructures (weirCounter) = newCstrc (ID = weirCounter, TypeID = weirTypeID)
    tmpCstrc => ContrStructures (weirCounter)
    if (weirTypeID < 903 .or. weirTypeID > 990) call ErrorMessageAndStop(1208, weirTypeID, 0.0d0, 0.0d0)
    nctref(weirTypeID) = weirCounter

    readCstrc_QCurve: do 
      read (INCSTR, '(a)', iostat = iostatvar) lineToEval
      !end of data block
      if (lineToEval (1:7) == 'ENDBLOC') exit readCstrc_QCurve
      !Read data line
      read (lineToEval, *) lineID, Qinner, EUW, EOW
      !Add Q-Curves data set to cstrc
      if (.not. (associated (tmpCstrc.QCurves))) call addQCurveGroup (tmpCstrc)
      !Add Q-Curve to Q-Curves data set of cstrc
      call addQCurve (tmpCstrc, Qinner)
      !Add Triple of Q-Eow-Euw
      call addValueTriple (tmpCstrc, Qinner, EOW, EUW)
    enddo readCstrc_QCurve
  
  !new cstrc tabular data block
  elseif (lineToEval (1:2) == 'IDC') then
    backspace (incstr)
    call ginpt(incstr,idc,dlinc)
    nsetsCounter = nsetsCounter + 1

    READ (DLINC, '(3I8)') ID1, NROWCS(nsetsCounter), NCOLCS(nsetsCounter)
    nctref(id1)=nsetscounter

    CALL GINPT(INCSTR,IDC,DLINC)
    NLO=1

    rows: DO
      IF(IDC(1:3) .EQ. 'HRW') THEN
        NHI=NLO+8
        IF (NROWCS(nsetsCounter) .LT. NHI) THEN
          NHI = NROWCS(nsetsCounter)
        ENDIF
        READ(DLINC,5010) (HRW(nsetsCounter,J), J=NLO,NHI)
        CALL GINPT(INCSTR,IDC,DLINC)
        NLO=NHI+1
      ENDIF
      IF(NHI >= NROWCS(nsetsCounter)) EXIT rows
    ENDDO rows

    NLO=1

    cols: do
      IF(IDC(1:3) .EQ. 'HCL') THEN
        NHI=NLO+8
        IF(NCOLCS(nsetsCounter) .LT. NHI) THEN
          NHI=NCOLCS(nsetsCounter)
        ENDIF
        READ(DLINC,5010) (HCL(nsetsCounter,J), J=NLO,NHI)
        CALL GINPT(INCSTR,IDC,DLINC)
        NLO=NHI+1
      ENDIF
      IF(NHI >= NCOLCS(nsetsCounter)) EXIT cols
    ENDDO cols

    DO J=1,NROWCS(nsetsCounter)
      NLO=1
      flows: do
        IF(IDC(1:3) .EQ. 'FLW') THEN
          NHI=NLO+8
          IF(NCOLCS(nsetsCounter) .LT. NHI) THEN
            NHI=NCOLCS(nsetsCounter)
          ENDIF
          READ(DLINC,5010) (FLWCS(nsetsCounter,J,L), L=NLO,NHI)
          CALL GINPT(INCSTR,IDC,DLINC)
          NLO=NHI+1
        ENDIF
        IF(NHI >= NCOLCS(nsetsCounter)) EXIT flows
      ENDDO flows
    ENDDO
  end if
enddo readCstrcData

 5010 FORMAT(9F8.0)

END
