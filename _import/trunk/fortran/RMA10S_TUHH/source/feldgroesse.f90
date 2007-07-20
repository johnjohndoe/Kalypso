!     Last change:  EF   17 Jul 2007   12:01 pm
! __________________________________________________________________________________________________________


!******************************************************************************************************
!******************************************************************************************************
!******************************************************************************************************
!
!   	This subroutine computes the dynamic dimensions of arrays which are necessary for autoconverge
!
!
!******************************************************************************************************
!******************************************************************************************************
!******************************************************************************************************




subroutine feldgroesse(p,nita_auto)

USE BLK10MOD
USE ParaKALYPS

! Data block

INTEGER,INTENT(IN) :: p,nita_auto
INTEGER :: i

if (p==1.0) then

  ALLOCATE (iteqs(nita_auto))
  ALLOCATE (iteqv(nita_auto))
  ALLOCATE (iurvl(nita_auto))
  ALLOCATE (itlvl(nita_auto))

  do i=1,nita_auto
    iteqs(i)=0.0
    iteqv(i)=0.0
    iurvl(i)=0.0
    itlvl(i)=0.0
  end do

ELSEIF (p==2.0) THEN

  DEALLOCATE (iteqs)
  DEALLOCATE (iteqv)
  DEALLOCATE (iurvl)
  DEALLOCATE (itlvl)

ELSEIF (p==3.0) then

  ALLOCATE (rss(nita_auto))
  do i=1,nita_auto
    rss(i)=0.0
  end do

ELSEIF (p==4.0) then

  DEALLOCATE (rss)

ELSEIF (p==5.0) then

  ALLOCATE (iteqs(nita_auto))
  ALLOCATE (iteqv(nita_auto))
  ALLOCATE (iurvl(nita_auto))
  ALLOCATE (itlvl(nita_auto))
  ALLOCATE (rss(nita_auto))

  do i=1,nita_auto
    iteqs(i)=0.0
    iteqv(i)=0.0
    iurvl(i)=0.0
    itlvl(i)=0.0
    rss(i)=0.
  end do

ELSEIF (p==6.0) THEN

  DEALLOCATE (iteqs)
  DEALLOCATE (iteqv)
  DEALLOCATE (iurvl)
  DEALLOCATE (itlvl)
  DEALLOCATE (rss)

end if


end subroutine
