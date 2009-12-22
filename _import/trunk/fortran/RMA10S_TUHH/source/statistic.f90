!     Last change:  EF    2 Jul 2007   12:00 pm
! __________________________________________________________________________________________________________


!******************************************************************************************************
!******************************************************************************************************
!******************************************************************************************************
!
!         This subroutine computes the sttatistical parameters, used in determination of
!       convergence curve behaviour, which reads as follow :
!       mean, Standard Devation(sdd), Regression coefficient (R), Coefficient in fitted
!       equation(A,m),and type of regression (linear) which is either linear or exponential
!
!
!******************************************************************************************************
!******************************************************************************************************
!******************************************************************************************************




subroutine statistic(dimn,var,R,nita_stat,extra)
! dimn     : dimension of the matrix _ input
! var      : variable storing the raw data _ input
! R        : regression correlation coefficient _output
! linear   : type of fitted curve either exponential or linear
! nita_stat: number of iteration steps ('CV')
! extra    : added number of iteration steps (defined in rma10.for)

! Data block

INTEGER,INTENT(IN) :: dimn,nita_stat,extra
REAL, DIMENSION(dimn),INTENT(IN):: var
REAL, INTENT (out):: R
INTEGER  :: linear

REAL,DIMENSION (dimn) :: y,xy,y2
INTEGER , DIMENSION (dimn) :: x, x2

REAL :: sigmay,sigmaxy,sigmay2,numerator,denominator
INTEGER :: i,sigmax,sigmax2,z,n

linear = 0
R = 0.

if (dimn == nita_stat) then

  do i = 1,dimn

      x(i) = i
      x2(i) = i*i

  end do

  DO i = 1,dimn

    if (var(i) <= 0.) then

      linear = 1.

    endif

  END do

   do i = 1,dimn

   !EFa jun07, deactivated LOG, because it is wrong and not working

     !if (linear == 1) then

      y(i) = var(i)
      xy(i) = x(i) * y(i)

    ! else

     ! y(i) = LOG (var(i))
     ! xy(i) = x(i) * y(i)

    ! end if

  !-

   end do

   sigmax = isigma(dimn,x)
   sigmay = sigma(dimn,y)
   sigmaxy = sigma(dimn,xy)
   sigmax2 = isigma(dimn,x2)

   ! Correlation coefficient

   do i=1,dimn

      y2(i)= y(i) * y(i)

   end do

   sigmay2 = sigma(dimn,y2)

   numerator = dimn*sigmaxy-sigmax*sigmay
   denominator = SQRT ((dimn*sigmax2-sigmax*sigmax)*(dimn*sigmay2-sigmay*sigmay))
   R = numerator/ denominator

else

  n = (dimn - nita_stat) / extra

  do z = dimn - extra+1, dimn

    i = z - nita_stat - (n - 1) * extra
    x(z) = i
    x2(z) = i * i

  end do

  do i = dimn - extra+1, dimn

    if (var(i) <= 0.) then

      linear = 1.

    end if

  end do

  do i = dimn - extra+1, dimn

    y(i) = var(i)
    xy(i) = x(i) * y(i)

  end do

   sigmax = isigmaa(dimn,x,extra)
   sigmay = sigmaa(dimn,y,extra)
   sigmaxy = sigmaa(dimn,xy,extra)
   sigmax2 = isigmaa(dimn,x2,extra)

   ! Correlation coefficient

  do i = dimn - extra+1, dimn

      y2(i)= y(i) * y(i)

   end do

   sigmay2 = sigmaa(dimn,y2,extra)

   numerator = extra*sigmaxy-sigmax*sigmay
   denominator = SQRT ((extra*sigmax2-sigmax*sigmax)*(extra*sigmay2-sigmay*sigmay))
   R = numerator/ denominator

end if

end subroutine

!-----------------------------------------------------------------------------------------------------

! This function calculates summation over dependent variable


REAL function sigma(sumindex,vari)

implicit none

INTEGER,INTENT (IN) :: sumindex
REAL,INTENT (IN) :: vari(sumindex)
REAL :: summ
INTEGER :: i

summ = 0.

  do i= 1,sumindex

    summ = summ + vari(i)

  end do

 sigma = summ

end function

!-----------------------------------------------------------------------------------------------------

! This function calculates summation over dependent variable


REAL function sigmaa(sumindex,vari,extraa)

implicit none

INTEGER,INTENT (IN) :: sumindex,extraa
REAL,INTENT (IN) :: vari(sumindex)
REAL :: summ
INTEGER :: i

summ = 0.

  do i= sumindex - extraa+1,sumindex

    summ = summ + vari(i)

  end do


 sigmaa = summ

end function
!-----------------------------------------------------------------------------------------------------

! This function calculates summation over independent variable (iteration or cycle)

INTEGER  function isigma(sumindex,vari)

implicit none

INTEGER,INTENT (IN) :: sumindex
INTEGER ,INTENT (IN) :: vari(sumindex)
INTEGER :: i,summ1

summ1 = 0.

  do i= 1,sumindex

    summ1 = summ1 + vari(i)

  end do

 isigma = summ1

end function

!-----------------------------------------------------------------------------------------------------

! This function calculates summation over independent variable (iteration or cycle)

INTEGER  function isigmaa(sumindex,vari,extraa)

implicit none

INTEGER,INTENT (IN) :: sumindex,extraa
INTEGER ,INTENT (IN) :: vari(sumindex)
INTEGER :: i,summ1

summ1 = 0.

  do i= sumindex - extraa+1,sumindex

    summ1 = summ1 + vari(i)

  end do

 isigmaa = summ1

end function
