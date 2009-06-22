!     Last change:  EF   26 Mar 2007   11:00 am
subroutine reweir2dKALYPSO (weir_no, reweir)
!LF nov06 this subroutine redefines the order of nodes around the weir elements
!         first node is read from 2D file, other nodes are rotated accordingly

USE BLK10MOD
!USE BLKSUBMOD

INTEGER, INTENT(IN) :: weir_no
INTEGER :: L, renum, i, refnode
INTEGER, DIMENSION(8) :: nop_temp
integer (kind = 4), intent (in) :: reweir (1:*)

!EFa Mar07, weirs in 1d-elements
if (nop(reweir(2),5) .eq.0) then

  if (nop(reweir(2),1) .ne. reweir(1)) then
    refnode=nop(reweir(2),1)
    nop(reweir(2),1)=nop(reweir(2),3)
    nop(reweir(2),3)=refnode
  end if

else
!-

 do L=1,8
!   write (*,*) nop(reweir(2),L)
   if (nop(reweir(2),L) .eq. reweir(1)) then
     refnode = L
     exit
   endif
 enddo

! write (*,*) reweir(2), reweir(1), refnode, nop(reweir(2),refnode)

 do i = 1, 8
!EFa Mar07
   if (refnode > i) then
     renum = 8 + MOD(i-refnode+1,8)
   else
     renum = MOD(i-refnode+1,8)
   endif
!   renum = MOD(i+refnode-1,8)
!-
   if (renum .eq. 0) renum = 8
!   write (*,*) renum
!   if (nop(reweir(2), i) .eq. 0) cycle
   nop_temp(renum) = nop(reweir(2), i)
!   write (*,*) nop(reweir(2), i)
 end do

 do i = 1, 8
   nop(reweir(2), i) = nop_temp(i)
 end do

endif

end subroutine
