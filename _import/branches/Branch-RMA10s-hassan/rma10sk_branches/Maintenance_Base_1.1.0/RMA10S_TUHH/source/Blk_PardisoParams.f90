module PardisoParams
implicit none

integer (kind = 4) :: mfwsiz, mr1siz, nbuffsiz


!save values
integer :: nOfEntriesInQsOld, nzzold, nszfold
integer, allocatable :: perm(:)
!32 bit OS
integer (kind = 4) :: ipt (64)
!64 bit OS
!integer (kind = 8) :: ipt (64)

end module