!     Last change:  WP    4 May 2009    2:30 pm
subroutine second (ta)
implicit none
save
! this version is for microsoft fortran
integer :: itim
data itim /0/
real (kind = 8) , intent (out):: ta

integer :: clock1, rate
real (kind = 8) :: rrate, told, tadbl

!get benchmark (n) start time
call system_clock (clock1, rate)  
rrate = rate
tadbl = clock1/ rrate
      
if (itim == 0) then
  told = tadbl
  itim = 1
endif
ta = tadbl - told


return
end
