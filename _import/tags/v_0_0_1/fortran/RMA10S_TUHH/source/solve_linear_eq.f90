!     Last change:  SR    9 Aug 2004    6:19 pm
!----------------------------------------------------
! This code, solve_linear_eq.f90, solves linear equations based on
! the gauss algorithm and pivot search in the library 'Kalypso-2D'.
! Copyright (C) 2004  SEBASTIAN RATH & WOLF PLOEGER.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Wolf Ploeger:     phone: +49 40 42878 4305 mail: ploeger@tuhh.de
! Sebastian Rath:   phone: +49 40 42878 4180 mail: s.rath@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-2D'.
!
! Sebastian Rath, 09 August 2004
! Research Associate
!
!
subroutine gauss(N,A,B,X,SING,NDIM)
!
!
!----------------------------------------------------------
! Some additional information of relevance:
!----------------------------------------------------------
! This subroutine is taken from the book:
!
! "Mathematik fuer Ingenieure" by Ansorge/Oberle, p. 100-102
! Akademie Verlag GmbH, Berlin, 1994
!
! It has been translated to FORTRAN90.
!----------------------------------------------------------

implicit none

! Calling variables
INTEGER, INTENT(IN)             			:: N    ! Dimension of equation system
INTEGER, INTENT(IN)             			:: NDIM ! Dimension of A, B and X
LOGICAL, INTENT(OUT) 					:: SING ! true if singularity appears, otherwise false
REAL (KIND=8), DIMENSION(1:NDIM,1:NDIM), INTENT(INOUT) 	:: A    ! coefficient matrix
REAL (KIND=8), DIMENSION(1:NDIM), INTENT(IN)    	:: B    ! right side vector
REAL (KIND=8), DIMENSION(1:NDIM), INTENT(OUT)   	:: X    ! solution vector

! Local variables
REAL (KIND=8), PARAMETER :: epmach = 1.E-15
REAL (KIND=8)	:: amax
REAL (KIND=8)   :: aij, ajk
REAL (KIND=8)   :: tol
REAL (KIND=8)   :: ap, q
INTEGER         :: i, j, k, kp, xp = 0

! calculation of bound of TOL
amax = 0.E0
do i = 1, N
  x(i) = b(i)
  do j = 1, n
    aij = ABS(a(i,j))
    if (aij .gt. amax) amax = aij
  end do
end do

tol = amax * epmach

! triangular decomposition
triangular: do k = 1, n-1

  ! Choice of pivot
  ap = a(k,k)
  kp = k

  pivot: do j = k+1, n

    ajk = a(j,k)
    if (ABS(ajk) .gt. ABS(ap)) then
      ap = ajk
      kp = j
    end if

  end do pivot

  if (ABS(ap) .lt. tol) then
    SING = .true.
    RETURN
  end if

  ! Changing lines
  xp = x(kp)
  if (kp .ne. k) then
    x(kp) = x(k)
    x(k) = xp
    do j = 1, n
      q = a(k,j)
      a(k,j) = a(kp,j)
      a(kp,j) = q
    end do
  end if

  sub1: do i = k+1,n
    q= a(i,k)
    if (q .eq. 0.E0) CYCLE sub1
    q = q/ap
    a(i,k) = q
    x(i) = x(i) - q*xp

    sub2: do j = k+1,n
      a(i,j) = a(i,j) - q*a(k,j)
    end do sub2

  end do sub1

end do triangular

if (ABS(a(n,n)) .lt. tol) then
  SING = .true.
  RETURN
end if

! back substitution
back: do i = n, 1, -1

  if (i .lt. n) then
    do j = i+1,n
      x(i) = x(i) - a(i,j)*x(j)
    end do
  end if

  x(i) = x(i) / a(i,i)

end do back

SING = .false.
RETURN

end subroutine gauss


