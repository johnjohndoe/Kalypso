!Last change:  WP   12 Jun 2008    2:06 pm
!Last change:  WP    7 Feb 2008    3:42 pm
SUBROUTINE WTFORM(Q, NCTR, HOWIN, HUWIN, xcord, ycord)
USE CSVAR
USE parakalyps

implicit none


!dummys ins
REAL (KIND = 8), INTENT (IN) :: huwin, howin
real (kind = 8), intent (in) :: xcord, ycord
INTEGER, INTENT (IN)         :: NCTR
!dummy outs
REAL (KIND = 8), INTENT (OUT) :: Q

!general locals
INTEGER         :: nrhi, nchi
REAL (KIND = 8) :: how, huw
INTEGER         :: i, j, k
REAL (KIND = 8) :: Diff, dummy

!locals for water stage method
REAL (KIND = 8) :: Q11, Q12, Q21, Q22, QRH, QRL
REAL (KIND = 8) :: WTC, WTR

!locals for energy method with linear functions
REAL (KIND = 8), ALLOCATABLE :: valuesOnLine (:)
REAL (KIND = 8) :: qfact, huw_tmp
REAL (KIND = 8) :: CalcPolynomial, HUPP, HLOW
REAL (KIND = 8) :: A(0:1)
INTEGER         :: noOfOutQs
INTEGER         :: Pos, FindPolynom


!locals for energy method with IDW
REAL (KIND = 8) :: power, TotalQ, TotalWeight, Weight
REAL (KIND = 8) :: how_tab, huw_tab
LOGICAL :: FileExists
INTEGER :: searchRange
INTEGER :: lowerBoundUW, upperBoundUW, lowerBoundOW, upperBoundOW


how = howin
huw = huwin

!original method from RMA10S with usage of water stages at control structures
if (UseEnergyCstrc == 0) then


  FileExists = .false.
  inquire (FILE='bauwerkstest.txt', EXIST= FileExists)
  if (.not.FileExists) then
   OPEN (123321, file='bauwerkstest.txt')
   WRITE (123321, *) 0.0d0, (hcl (nctr, j), j=1, 160)
   do i = 1, 1000
     WRITE (123321, *) hrw (nctr, i), (flwcs(nctr, i, j), j=1, 160)
   end do
   close (123321)
  endif

  !find position in table

  FindRow: DO K=1, NROWCS (NCTR)
    NRHI = K
    IF(HOW < HRW (NCTR, K)) EXIT FindRow
    !nis,aug08: If no entry found, value range too small, stop!
    if (k == nrowcs (nctr)) then
      call errormessageandstop (4004, 0, xcord, ycord)
    endif  
      
  ENDDO FindRow

  FindColumn: DO K = 1, NCOLCS (NCTR)
    NCHI = K
    IF (HUW < HCL (NCTR, K)) EXIT FindColumn
    !nis,aug08: If no entry found, value range too small, stop!
    if (k == ncolcs (nctr)) then
      call errormessageandstop (4004, 0, xcord, ycord)
    endif  
    
  ENDDO FindColumn


  WTR = (HOW - HRW (NCTR, NRHI - 1))/ (HRW (NCTR, NRHI) - HRW (NCTR, NRHI - 1))
  WTC = (HUW - HCL (NCTR, NCHI - 1))/ (HCL (NCTR, NCHI) - HCL (NCTR, NCHI - 1))

  !nis,jan08: Calculate the single values and interpolate later
  !      |  HCL-1  HUW  HCL
  !-------------------------------
  !      |
  !HRW-1 |   Q11       Q12
  !      | (1)|   (3)   |(2)
  !HOW   |   QRL - Q - QRH
  !      | (1)|         |(2)
  !HRW   |   Q21       Q22
  !      |

  !get tabular values

  !The following values shouldn't be problematic, otherwise the calculation will not work
  Q11 = FLWCS (NCTR, NRHI - 1, NCHI - 1)
  Q22 = FLWCS (NCTR, NRHI, NCHI)

  Q21 = FLWCS (NCTR, NRHI, NCHI - 1)
  if (Q21 < -999.0) then
    Q = Q11 + WTR * (Q22 - Q11)
    return
  end if

  Q12 = FLWCS (NCTR, NRHI - 1, NCHI)
  if (Q12 < -999.0) Q12 = 0


  !interpolate between the rows
  QRL = (Q21 - Q11) * WTR + Q11
  QRH = (Q22 - Q12) * WTR + Q12

  !TODO: catch problems

  !interpolate in general
  Q = (QRH - QRL) * WTC + QRL


!energy slope with partly linear equations
!*****************************************
ELSEIF (UseEnergyCstrc == 1) then

  !very special case
  if (huw == how) then
    Q = 0.0
    return
  end if

  !if flow is turning around: introduce direction factor and switch water stages
  if (how < huw) then
    QFact = -1.0
    huw_tmp = huw
    huw = how
    how = huw_tmp
  else
    QFact = 1.0
  end if

  !get the number of entries in array
  noOfOutQs = UBOUND (cstrcRange, 3)
  ReduceForZeros: do i = noOfOutQs, 1, -1
    if (cstrcRange (nctr, 1, i) /= 0.0) exit ReduceForZeros
  enddo ReduceForZeros
  noOfOutQs = i
      
    

  !valuesOnLine
  !
  !  how
  !  |
  !  |    |    Q1  Q2
  !  |    |    /   /
  !  |        /   /
  !  | ------/   /
  !  | ---------/
  !  |
  !  |    |
  !  |    | (valuesOnLine)
  !  |    |
  !  |
  !  |-------------------->huw

  !valuesOnLine :: valuesOnLine are giving all the how values from the Q-curves corresponding to huw and how

  ALLOCATE (valuesOnLine (1: noOfOutQs))

  do i = 1, noOfOutQs
    Pos = findPolynom (cstrcRange (nctr, i, :), huw, noOfOutQs, xcord , ycord, 0)
    if (Pos > noOfOutQs) then
      findLast: do j = 1, noOfOutQs

        if (cstrcRange (nctr, i, j) < 0.00001) then
          Pos = j - 1
          EXIT findLast
        endif

        if (j == noOfOutQs) then
          Pos = j
          EXIT findLast
        end if

      end do FindLast
    end if
    !testing
    !WRITE(*,*) 'vor :', valuesOnLine (i)
    !WRITE(*,*) (cstrcCoefs (nctr, i, Pos, j), j= 0, 1)
    !testing-
    valuesOnLine (i) = CalcPolynomial (cstrcCoefs (nctr, i, Pos, 0:1), huw, 1)
    !testing
    !WRITE(*,*) Pos, huw
    !WRITE(*,*) 'nach:', valuesOnLine(i)
    !pause
    !testing-
  end do

  !now get it in vertical line
  Pos  = findPolynom (valuesOnLine, how, noOfOutQs, xcord, ycord, 0)

  !testing
  !WRITE(*,*) how, pos
  !do k = 1, noOfOutQs
  !  if (k >= 2) then
  !    if (valuesonline (k) <= valuesonline (k-1)) then
  !      WRITE(*,*) nctr, k
  !      WRITE(*,*) valuesOnLIne(k)
  !      WRITE(*,*) valuesOnLIne(k-1)
  !    endif
  !  endif
  !enddo
  !pause
  !testing-

  !testing
  !WRITE(*,*) huw, how, pos
  !pause
  !testing-

  if (pos == 1) then
    HUPP = valuesOnLine (pos)
    HLOW = HOW
    Q = cstrcdisch (nctr, pos) *  (HOW - huw) / (valuesOnLine (pos) - huw)

    !testing
    !WRITE(*,*) 'pos: ', pos, 'nctr: ', nctr
    !WRITE(*,*) 'HUP: ', HUPP, 'HLOW: ', hlow
    !WRITE(*,*) 'how: ', how
    !WRITE(*,*) 'huw: ', huw
    !WRITE(*,*) 'dis: ', cstrcdisch (nctr, pos)
    !WRITE(*,*) 'res: ', Q
    !pause
    !testing-


  !extrapolation at very high upstream water levels
  ELSEIF (pos > noOfOutQs) then
    HUPP = valuesOnLine (pos - 1)
    HLOW = valuesOnLine (pos - 2)
    A(1) = (cstrcdisch (nctr, pos - 1) - cstrcdisch (nctr, pos - 2)) / (valuesOnLine (pos - 1) - valuesOnLine (pos - 2))
    A(0) = cstrcdisch (nctr, pos - 1) - A(1) * valuesOnLine (pos - 1)
    Q = CalcPolynomial (A, how, 1)

  !normal interpolation between functions
  else
    HUPP = valuesOnLine (pos)
    HLOW = valuesOnLine (pos - 1)
    Q11 = cstrcdisch (nctr, pos - 1)
    Q22 = cstrcdisch (nctr, pos)
    Q = Q11 + (Q22 - Q11) / (HUPP - HLOW) * (HOW - HLOW)

    !testing
    !WRITE(*,*) 'Calculating in WTFORM'
    !WRITE(*,*) HUPP, HLOW
    !WRITE(*,*) Q22, Q11
    !WRITE(*,*) HOW, Q
    !WRITE(*,*) 'End of Calculation in WTFORM'
    !testing-

  end if

  !set direction of Q
  Q = Q * Qfact

  !testing
  !if (qfact < 0.0) then
  !  WRITE(*,*) nctr
  !  WRITE(*,*) huw, how
  !  WRITE(*,*) hlow, hupp
  !  WRITE(*,*) Q
  !  pause
  !end if
  !testing-

  !turn the values back!
  if (qfact < 0.0) then
    huw_tmp = huw
    huw = how
    how = huw_tmp
  end if
  deallocate (valuesOnLine)

!energy slope with IDW-method
elseif (UseEnergyCstrc == 2) then

  !if there is no energy slope (or even negative slope, then Q is known
  if (how <= huw) then
    Q = 0.0
    return
  end if

  !testing
  !WRITE(*,*) 'Wasserstaende'
  !WRITE(*,*) how, huw
  !testing-

  !power for simple IDW-method
  power = 1.0d0
  searchRange = 15

  !accumulated weighted discharge/ average value with IDW
  TotalQ = 0.0D0
  !accumulated weight
  TotalWeight = 0.0D0

  Weight = 0.0D0
  lowerBoundUW = MAX (NCHI - 1 - searchRange, 1)
  upperBoundUW = MIN (NCHI - 1 + searchRange, UBOUND (FLWCS, 3))
  lowerBoundOW = MAX (NRHI - searchRange, 1)
  upperBoundOW = MIN (NRHI + searchRange, UBOUND (flwcs, 2))

  !testing
  !write(*,*) 'part of the matrix to be written'
  !WRITE(*,*) lowerbounduw, lowerboundow
  !WRITE(*,*) upperbounduw, upperboundow
  !testing-

  throughHUW: do i = lowerBoundUW, upperBoundUW
    throughHOW: do j = lowerBoundOW, upperBoundOW
      HUW_tab = HCL (nctr, i)
      how_tab = HRW (nctr, j)


      !if (huw_tab < how_tab) then
      Q = flwcs (nctr, j, i)
      !else
      !  huw_tab = how_tab
      !  Q = 0
      !end if

      !null data value
      if (Q < -999.0) CYCLE throughHOW

      !testing
      !WRITE(*,*) huw_tab, how_tab
      !WRITE(*,*) Q
      !WRITE(*,*)
      !testing-

      Weight = 1/ (SQRT ((HUW - huw_tab)**2 + (HOW - how_tab)**2)) ** power
      TotalQ = TotalQ + Weight * Q
      TotalWeight = TotalWeight + Weight
    enddo throughHOW
  enddo throughHUW
  TotalQ = TotalQ / TotalWeight
  Q = TotalQ
end if


!testing
!WRITE(*,*) 'ende wtform', huw, how, Q
!pause
!testing-


!what is this?
Diff = ABS (HOW - HUW)
IF (Diff > 0.02) Then
  dummy = 0
End If

RETURN
END