!     Last change:  WP   15 Feb 2008   11:02 am
!polynom calculation

function calcPolynomial(a, x, endk)
implicit none
REAL (KIND = 8) :: calcPolynomial
REAL (KIND = 8) :: a(0:*)
REAL (KIND = 8) :: x
INTEGER         :: k, startk, endk

calcPolynomial = 0.0
startk = LBOUND (a, 1)
do k = startk, endk
  calcPolynomial = calcPolynomial + a(k) * x ** (k)
end do
return
end

!function calcPolynomial2(a, x)
!implicit none
!REAL (KIND = 8) :: calcPolynomial2
!REAL (KIND = 8) :: a (0:)
!REAL (KIND = 8) :: x
!INTEGER         :: k, startk, endk
!
!calcPolynomial2 = 0.0
!startk = LBOUND (a, 1)
!endk   = ubound (a, 1)
!!WRITE(*,*) 'in calcpolynomial2'
!!WRITE(*,*) startk, endk
!!WRITE(*,*) a(startk), a (endk)
!!WRITE(*,*) x
!do k = startk, endk
!  calcPolynomial2 = calcPolynomial2 + a(k) * x ** (k)
!  !WRITE(*,*) calcpolynomial2
!end do
!!pause
!return
!end

!Integration of a polynom

function calcPolynomialIntegral(a, x, endk)
implicit none
REAL (KIND = 8) :: calcPolynomialIntegral
REAL (KIND = 8) :: a(0:*)
REAL (KIND = 8) :: x
INTEGER         :: k, startk, endk

calcPolynomialIntegral = 0.0
startk = LBOUND (a, 1)
do k = startk, endk
  calcPolynomialIntegral = calcPolynomialIntegral + a(k) / (k+1) * x ** (k + 1)
end do
return
end

!first derivative of a polynom

function calcPolynomial1stDerivative(a, x, endk)
implicit none
REAL (KIND = 8) :: calcPolynomial1stDerivative
REAL (KIND = 8) :: a(0:*)
REAL (KIND = 8) :: x
INTEGER         :: k, startk, endk

calcPolynomial1stDerivative = 0.0
startk = LBOUND (a, 1)
do k = startk, endk
  calcPolynomial1stDerivative = calcPolynomial1stDerivative + (k) * a(k) * x ** (k - 1)
end do
return
end

!second derivative of a polynom

function calcPolynomial2ndDerivative(a, x, endk)
implicit none
REAL (KIND = 8) :: calcPolynomial2ndDerivative
REAL (KIND = 8) :: a(0:*)
REAL (KIND = 8) :: x
INTEGER         :: k, startk, endk

calcPolynomial2ndDerivative = 0.0
startk = LBOUND (a, 1)
do k = startk, endk
  calcPolynomial2ndDerivative = calcPolynomial2ndDerivative + (k) * (k-1) * a(k) * x ** (k - 2)
end do
return
end

