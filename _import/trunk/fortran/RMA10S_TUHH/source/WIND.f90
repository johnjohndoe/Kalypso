SUBROUTINE WINDF(IBIN)
USE BLK10MOD, only: lout, id, dlin, isvs, nscrin, np
USE BLK11MOD, only: wndsp, wnddr, sigma, chi
use blk_ifu, only: iwindin

implicit none

integer (kind = 4) :: ibin
integer (kind = 4) :: nxx, j, n
integer (kind = 4) :: iyfl, dyofy, tfl
real (kind = 8) :: TW, TA

WRITE(LOUT,6050)
 6050 FORMAT('0   WIND STRESS INFORMATION:')

! 
!    Three options are available for input of wind data
!
!IPK AUG98  CHECK AND READ FILE FIRST
IF (IWINDIN > 0.and.iwindin.ne.71) THEN
  !read binary wind file
  IF (IWINDIN == 69) THEN
    READ(IWINDIN) NXX,IYFL,DYOFY,TFL,(WNDSP(J),WNDDR(J),J=1,NXX)
    !Read ASCII wind file
  ELSE
    !here it should be unit no. 70
    READ(IWINDIN,5400) NXX,IYFL,DYOFY,TFL
    READ(IWINDIN,5401) (WNDSP(J),WNDDR(J),J=1,NXX)
  ENDIF
 5400 FORMAT(2I7,F7.0,F14.3)
 5401 FORMAT(5(2F7.2))

  DO J=1,NXX
    SIGMA(J,1)=CHI*COS(WNDDR(J)/57.3)*WNDSP(J)**2
    SIGMA(J,2)=CHI*SIN(WNDDR(J)/57.3)*WNDSP(J)**2
  ENDDO
  WRITE(75,*) 'WIND',SIGMA(1,1),SIGMA(1,2),WNDSP(1)

!read global wind specification
ELSEIF(ID(1:3) .EQ. 'WVA') THEN
  if (iwindin==71) then
    READ(DLIN,'(I8,2F8.2,i8)') N,TW,TA,lablwind
  else
    READ(DLIN,'(I8,2F8.0)') N,TW,TA
  endif  
!ipk nov97        READ(IBIN,7000) ID,DLIN
  call ginpt(ibin,id,dlin)
!ipk APR96 save data to a scratch file
  if(isvs .eq. 1) write(nscrin,7000) id,dlin

  WRITE(LOUT,6046) TW,TA
  WRITE(*,6046) TW,TA
  WRITE(*,*) 'CHI',CHI
  DO N=1,NP
    SIGMA(N,1)=CHI*COS(TA/57.3)*TW**2
    SIGMA(N,2)=CHI*SIN(TA/57.3)*TW**2
  ENDDO
ENDIF

!Read particular nodal wind specification and overwrite global wind specification
readNodalWind: Do
  IF(.not.(ID(1:3) .EQ. 'WVN')) exit readNodalWind
    
  READ(DLIN,'(I8,2F8.0)') N,TW,TA

  call ginpt(ibin,id,dlin)

  if(isvs .eq. 1) write(nscrin,7000) id,dlin

  SIGMA(N,1)=CHI*COS(TA/57.3)*TW**2
  SIGMA(N,2)=CHI*SIN(TA/57.3)*TW**2
enddo readNodalWind

RETURN
 6046 FORMAT(/'   WIND SPEED =',F8.1,'  DIRECTION =',F8.0,' DEGREES'/)
 7000 FORMAT(A8,A72)

END subroutine
