SUBROUTINE OUTNRM (X1, Y1, X2, Y2, X3, Y3, S1, NTP)
!A routine designed to return the outward normal along a line

implicit none
save
real (kind = 8), intent (in) :: x1, y1, x2, y2, x3, y3
real (kind = 8), dimension (1:3):: sv
real (kind = 8), intent (out), dimension (1:3) :: s1
integer (kind = 4), intent (in) :: ntp
real (kind = 8) :: x, y, s
real (kind = 8) :: xn1, xn2, xn3
real (kind = 8) :: dx, dy
integer (kind = 4) :: j
data sv/ 0.02, 0.52, 0.98/

do j = 1, 3
  s = sv (j)
  !these are the quadratic approximation functions
  xn1 = 1.0 - 3.0 * s + 2.0 * s**2
  xn2 = 4.0 * s * (1.0 - s)
  xn3 = s * (2.0 * s - 1.0)
  !get the coordinates of the auxiliary points
  x = xn1 * x1 + xn2 * x2 + xn3 * x3
  y = xn1 * y1 + xn2 * y2 + xn3 * y3
  if (ntp == 1) then
    if (j == 1) then
      dy = x1 - x
      dx = y - y1
    elseif (j == 2) then
      dy = x2 - x
      dx = y - y2
    else
      dy = x - x3
      dx = y3 - y
    endif
  else
    if (j == 1) then
      dy = y1 - y
      dx = x1 - x
    elseif (j == 2) then
      dy = y2 - y
      dx = x2 - x
    else
      dy = y3 - y
      dx = x3 - x
    endif
  endif
  s1 (j) = atan2 (dy, dx)
enddo

return
end
