!Last change:  WP   12 Jun 2008    2:06 pm
!Last change:  WP    7 Feb 2008    3:42 pm
SUBROUTINE WTFORM(Q, weirImat, HOWIN, HUWIN, xcord, ycord)
USE CSVAR
USE parakalyps
use blk10mod, only: nctref, maxweir

implicit none

real (kind = 8) :: absz, ord
real (kind = 8) :: testQ

integer (kind = 4), intent (in) :: weirImat

!dummys ins
REAL (KIND = 8), INTENT (IN) :: huwin, howin
real (kind = 8), intent (in) :: xcord, ycord
INTEGER :: NCTR
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
  nctr = NCTREF(weirImat)


  FileExists = .false.

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

RETURN
END