!Last change:  WP    7 Feb 2008    4:13 pm
!Last change:  WP    7 Feb 2008    3:42 pm
SUBROUTINE WTFORM(Q, NCTR, HOW, HUW)
USE CSVAR
USE parakalyps

implicit none


!dummys ins
REAL (KIND = 8), INTENT (IN)  :: huw, how
INTEGER, INTENT (IN) :: NCTR
!dummy outs
REAL (KIND = 8), INTENT (OUT) :: Q

!general locals
INTEGER :: nrhi, nchi
INTEGER :: i, j, k
REAL (KIND = 8) :: Diff, dummy

!locals for water stage method
REAL (KIND = 8) :: Q11, Q12, Q21, Q22, QRH, QRL
REAL (KIND = 8) :: WTC, WTR
!locals for energy method
REAL (KIND = 8) :: power, TotalQ, TotalWeight, Weight
REAL (KIND = 8) :: how_tab, huw_tab
INTEGER :: searchRange
INTEGER :: lowerBoundUW, upperBoundUW, lowerBoundOW, upperBoundOW


!find position in table

FindRow: DO K=1,NROWCS(NCTR)
  NRHI=K
  IF(HOW < HRW(NCTR,K)) EXIT FindRow
ENDDO FindRow

FindColumn: DO K = 1, NCOLCS (NCTR)
  NCHI = K
  IF (HUW < HCL (NCTR, K)) EXIT FindColumn
ENDDO FindColumn

!original method from RMA10S with usage of water stages at control structures
if (UseEnergyCstrc == 0) then

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


elseif (UseEnergyCstrc == 1) then

  !if there is no energy slope (or even negative slope, then Q is known
  if (huw <= how) then
    Q = 0.0
    return
  end if

  !power for simple IDW-method
  power = 1.0d0
  searchRange = 5
  !accumulated weighted discharge/ average value with IDW
  TotalQ = 0.0D0
  !accumulated weight
  TotalWeight = 0.0D0
  Weight = 0.0D0
  lowerBoundUW = MAX (NCHI - 1 - searchRange, 1)
  upperBoundUW = MIN (NCHI - 1 + searchRange, UBOUND (FLWCS, 3))
  lowerBoundOW = MAX (NRHI - searchRange, 1)
  upperBoundOW = MIN (NRHI - searchRange, UBOUND (flwcs, 2))

  throughHUW: do i = lowerBoundUW, upperBoundUW
    throughHOW: do j = lowerBoundOW, upperBoundOW
      HUW_tab = HCL (nctr, i)
      how_tab = HRW (nctr, j)

      if (huw_tab < how_tab) then
        Q = flwcs (nctr, j, i)
      else
        huw_tab = how_tab
        Q = 0
      end if

      !null data value
      if (Q < -999.0) CYCLE throughHOW

      Weight = 1/ (SQRT ((HUW - huw_tab)**2 + (HOW - how_tab)**2))** power
      TotalQ = TotalQ + Weight * Q
      TotalWeight = TotalWeight + Weight
    enddo throughHOW
  enddo throughHUW
  TotalQ = TotalQ / TotalWeight
end if

!what is this?
Diff = ABS (HOW - HUW)
IF (Diff > 0.02) Then
  dummy = 0
End If

RETURN
END