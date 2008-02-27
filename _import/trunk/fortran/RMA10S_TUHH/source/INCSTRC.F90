!     Last change:  WP   22 Feb 2008    1:05 pm
!C     Last change:  WP   14 Feb 2008    3:47 pm
SUBROUTINE INCSTRC


USE CSVAR
USE BLK10MOD
USE parakalyps

!implicit none
!definition block

!cipk aug05	INCLUDE 'BLK10.COM'
CHARACTER*8  IDC
CHARACTER*72 DLINC
INTEGER :: nlo, nhi, id1
INTEGER :: NMAXM, nrow, ncol


!local definitions for Energy usage with linear functions
INTEGER :: iostatvar, iostateval
integer :: i, j, k, l
CHARACTER (LEN = 250) :: lineToEval
CHARACTER (LEN = 3) :: lineID
INTEGER :: MaxWeir, MaxQ, TempWeir, TempQs
INTEGER :: countOuter, countInner
REAL (KIND = 8) :: EUW, EUW_old, EOW, EOW_old
REAL (KIND = 8) :: Qinner, Q_loop
!------



if (UseEnergyCstrc == 1) then

  NROWM=0
  NCOLM=0
  NMAXM=0
  NSETS=0
  CALL GINPT(INCSTR,IDC,DLINC)
  IF(IDC(1:2) .EQ. 'TI') THEN
    WRITE(*,*) 'Reading title on control structure',DLINC
  ENDIF

  findDataSize: do
    CALL GINPT(INCSTR,IDC,DLINC)
    IF(IDC(1:3) .EQ. 'IDC') THEN
      READ(DLINC,'(3I8)') ID1,NROW,NCOL
      IF(NROW .GT. NROWM) NROWM=NROW
      IF(NCOL .GT. NCOLM) NCOLM=NCOL
      NSETS=NSETS+1
    ELSEIF(IDC(1:7) .EQ. 'ENDDATA') THEN
      EXIT findDataSize
    ENDIF
  ENDDO findDataSize

  REWIND INCSTR

  !weir file shall also be present if no data is available
  if (nrowm == 0) return
  CALL GINPT(INCSTR,IDC,DLINC)
  ALLOCATE (NROWCS (NROWM), NCOLCS (NCOLM), HRW (NSETS, NROWM), HCL (NSETS, NCOLM), FLWCS (NSETS, NROWM, NCOLM))
  CALL GINPT(INCSTR,IDC,DLINC)
  DO K=1,NSETS
    READ (DLINC, '(3I8)') ID1, NROWCS(K), NCOLCS(K)
    NCTREF(ID1)=K

    CALL GINPT(INCSTR,IDC,DLINC)
    NLO=1

    rows: DO
      IF(IDC(1:3) .EQ. 'HRW') THEN
        NHI=NLO+8
        IF (NROWCS(K) .LT. NHI) THEN
          NHI = NROWCS(K)
        ENDIF
        READ(DLINC,5010) (HRW(K,J), J=NLO,NHI)
        CALL GINPT(INCSTR,IDC,DLINC)
        NLO=NHI+1
      ENDIF
      IF(NHI >= NROWCS(K)) EXIT rows
    ENDDO rows

    NLO=1

    cols: do
      IF(IDC(1:3) .EQ. 'HCL') THEN
        NHI=NLO+8
        IF(NCOLCS(K) .LT. NHI) THEN
          NHI=NCOLCS(K)
        ENDIF
        READ(DLINC,5010) (HCL(K,J), J=NLO,NHI)
        CALL GINPT(INCSTR,IDC,DLINC)
        NLO=NHI+1
      ENDIF
      IF(NHI >= NCOLCS(K)) EXIT cols
    ENDDO cols

    DO J=1,NROWCS(K)
      NLO=1
      flows: do
        IF(IDC(1:3) .EQ. 'FLW') THEN
          NHI=NLO+8
          IF(NCOLCS(K) .LT. NHI) THEN
            NHI=NCOLCS(K)
          ENDIF
          READ(DLINC,5010) (FLWCS(K,J,L), L=NLO,NHI)
          CALL GINPT(INCSTR,IDC,DLINC)
          NLO=NHI+1
        ENDIF
        IF(NHI >= NCOLCS(K)) EXIT flows
      ENDDO flows
    ENDDO
  ENDDO
ELSEIF (UseEnergyCstrc == 0) then

  iostatvar = 0
  MaxWeir = 0
  MaxQ = 0
  readWeirSize: do
    read (INCSTR, '(a)', iostat = iostatvar) lineToEval
    if (iostatvar /= 0) EXIT readWeirSize

    if (lineToEval(1:7) == 'ENDBLOC' .or. lineToEval (1: 2) == 'TI') then
      !to header lines per weir
      read (incstr, '(a)', iostat = iostatvar) lineToEval

      if (lineToEval (1: 7) == 'ENDDATA') EXIT readweirsize
      if (lineToEval (1: 3) == 'DLI') then
        read (lineToEval, *) lineID, TempWeir, TempQs
        MaxWeir = MAX (MaxWeir, TempWeir - 903)
        MaxQ = MAX (MaxQ, TempQs)
      end if
    end if
  end do readWeirSize
  REWIND INCSTR


  !allocation of the weir arrays
  ALLOCATE (cstrcCoefs (1: MaxWeir, 1: MaxQ, 1: MaxQ, 0:1))
  ALLOCATE (cstrcRange (1: MaxWeir, 1: MaxQ, 1: MaxQ))
  ALLOCATE (cstrcdisch (1: MaxWeir, 1: MaxQ))
  do i = 1, MaxWeir
    do j = 1, MaxQ
      do k = 1, MaxQ
        cstrcRange (i, j, k) = 0.0d0
        do l = 0, 1
          cstrcCoefs (i, j, k, l) = 0.0d0
        end do
      end do
    end do
  end do

  iostatvar  = 0
  iostateval = 0
  countOuter = 0
  countInner = 0
  Qinner     = 0.0d0
  Q_loop     = 0.0d0
  EUW        = 0.0d0
  EUW_old    = 0.0d0
  EOW        = 0.0d0
  EOW_old    = 0.0d0


  readWeirData: do
    read (INCSTR, '(a)', iostat = iostatvar) lineToEval
    if (iostatvar /= 0) EXIT readWeirData

    if (lineToEval(1:7) == 'ENDBLOC' .or. lineToEval(1:2) == 'TI') then
      countOuter = 0
      countInner = 0
      Qinner     = 0.0d0
      Q_loop     = 0.0d0
      EUW        = 0.0d0
      EUW_old    = 0.0d0
      EOW        = 0.0d0
      EOW_old    = 0.0d0

      read (INCSTR, '(a)', iostat = iostatvar) lineToEval
      if (lineToEval(1:7) == 'ENDDATA') EXIT readWeirData

      if (lineToEval(1:3) == 'DLI') then
        read (lineToEval, *) lineID, TempWeir, TempQs

        if (TempWeir < 903 .or. TempWeir > 990) call ErrorMessageAndStop(1208, TempWeir, 0.0d0, 0.0d0)
        NCTREF(TempWeir) = TempWeir - 903
        TempWeir = TempWeir - 903
      endif
    else

      read (lineToEval, *) lineID, Qinner, EUW, EOW

      if (Qinner > Q_loop) then
        Q_loop = Qinner
        countOuter = countOuter + 1
        cstrcdisch (TempWeir, countOuter) = Q_loop
        countInner = 1
      ELSEIF (Qinner < Q_loop) then
        !ERROR - data is not sorted
        call ErrorMessageAndStop (1207, TempWeir, 0.0d0, 0.0d0)
      end if

      if (countInner == 1) then
        cstrcRange (TempWeir, CountOuter, CountInner) = EUW
        call CoefsOfLinFunct (EUW, EUW - 1.0d0, EOW, EOW, cstrcCoefs (TempWeir, CountOuter, CountInner, 0:1))
      else
        cstrcRange (TempWeir, CountOuter, CountInner) = EUW
        call CoefsOfLinFunct (EUW, EUW_old, EOW, EOW_old, cstrcCoefs (TempWeir, CountOuter, CountInner, 0:1))
      end if

      countInner = countInner + 1
      EUW_old = EUW
      EOW_old = EOW

    end if
  end do readWeirData
end if


 5010 FORMAT(9F8.0)
RETURN
END

!subroutine for the weir parameters, which gives back the coefficients of a line
subroutine CoefsOfLinFunct (X1, X2, Y1, Y2, A)
  REAL (KIND = 8), INTENT (IN) :: X1, X2
  REAL (KIND = 8), INTENT (IN) :: Y1, Y2
  REAL (KIND = 8), INTENT (OUT) :: A (0:1)

  A(1) = (Y2 - Y1)/ (X2 - X1)
  A(0) = Y2 - A(1) * X2
end subroutine