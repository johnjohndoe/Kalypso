!-------------------------------------------------------
!subroutine to sort the matrix-vector in ascending order
!-------------------------------------------------------
SUBROUTINE MYSORT (IX, Y, N)
!
!ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
!
!Sorts the N values stored in array X in ascending order

!dummy arguments
!---------------
integer (kind = 4) :: n
integer (kind = 4) :: ix(n)
real (kind = 8) :: y(N)

!local variables
!---------------
integer (kind = 4) :: i, j, incr
REAL (kind = 8) :: ytemp

!Calculate the increment
!-----------------------
incr = 1
SetIncrement: Do
  incr = 3 * INCR + 1
  IF (INCR > N) exit SetIncrement
enddo SetIncrement

!------------------
!Shell-Metzner sort
!------------------
VeryOuterSort: Do
  INCR = INCR / 3
  I = INCR + 1

  OuterSort: Do
    IF (I > N) exit OuterSort
    ITEMP = IX(I)
    YTEMP = Y(I)
    J = I
    InnerSort: Do
      IF (IX(J - INCR) < ITEMP) exit InnerSort
      IX(J) = IX(J - INCR)
      Y(J) = Y(J - INCR)
      J = J - INCR
      IF (J <= INCR) exit InnerSort
    enddo InnerSort
    IX(J) = ITEMP
    Y(J) = YTEMP
    I = I + 1
  enddo OuterSort
  IF (INCR <= 1) exit veryOuterSort
enddo veryOuterSort


END subroutine mysort
