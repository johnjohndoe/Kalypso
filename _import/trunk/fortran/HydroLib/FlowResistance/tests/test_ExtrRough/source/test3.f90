Program test3
real (kind = 8) :: lambdaExtrRau
external AgPeFuentes
lambdaExtrRau = AgPeFuentes (6.8, 0.3, 0.3, 2.)
WRITE(*,*) 'lambdaExtrRau = ', lambdaExtrRau
pause
end program test3