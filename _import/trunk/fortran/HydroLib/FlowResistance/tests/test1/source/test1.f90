Program test1
implicit none
real (kind = 8) :: lambda_s
external cole
lambda_s = cole (2., 2., 0.25, 2000)
WRITE(*,*) 'lambda_s = ', lambda_s
pause
end program test1