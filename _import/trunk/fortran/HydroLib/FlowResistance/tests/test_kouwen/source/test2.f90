Program test2
implicit none
real (kind = 8) :: lambdakouwen
real (kind = 8) :: kouwen
external kouwen
lambdakouwen = kouwen (0.1, 2., 0.001, 2., 1)
WRITE(*,*) 'lambdakouwen = ', lambdakouwen
pause
end program test2