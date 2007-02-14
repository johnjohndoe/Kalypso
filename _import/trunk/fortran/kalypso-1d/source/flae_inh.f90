!     Last change:  WP   30 May 2005    9:59 am
!--------------------------------------------------------------------------
! This code, flae_inh.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - flae_inh
!
! Copyright (C) 2004  ULF TESCHKE & WOLF PLOEGER.
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
! Ulf Teschke:      phone: +49 40 42878 3895 mail: teschke@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 18 August 2004
! Research Associate
!***********************************************************************


SUBROUTINE flae_inh (max, n, x, y, f)

! Diese routine berechnet den flaecheninhalt eines geschlossenen
! Gebietes. Der erste und der letzte Punkt sind verschieden.

DIMENSION x (max), y (max)

f = 0. 

!WP PRINT * , 'max   = ', max, ' n     = ', n

n01 = n + 1 
x(n01) = x(1)
y(n01) = y(1)

DO i = 1, n01 - 1
  !WP write (*,1000) i, x(i), y(i)
  f = f + (x (i + 1) - x (i) ) * (y (i + 1) + y (i) )
  !WP write (*,1001) i, f
END DO
f = f / 2.

!WP write (*,1002) f

!1000 format (1X, 'I: ', I3, ' X(I): ', F10.4, ' Y(I): ', F10.4)
!1001 format (1X, 'Flaeche nach ',I3,' Punkten: ', F10.4)
!1002 format (1X, 'Flaeche am Ende von FLAE_INH: ', F10.4/)

END SUBROUTINE flae_inh
