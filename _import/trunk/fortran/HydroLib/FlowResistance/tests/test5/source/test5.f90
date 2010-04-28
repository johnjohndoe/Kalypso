Program test5
real (kind = 8) :: cw_r
external GET_CWR
cw_r = GET_CWR (0.001, 2., 7., 0.1, 0.23)
WRITE(*,*) 'cw_r = ', cw_r
pause
end program test5

