Program test4
real (kind = 8) :: lambda_p
external wald
lambda_p = wald (2., 3., 0.2, 1.2,10.)
WRITE(*,*) 'lambda_p = ', lambda_p
pause
end program test4