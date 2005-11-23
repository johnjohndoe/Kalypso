!     Last change:  WP   30 May 2005   12:02 pm
!--------------------------------------------------------------------------
! This code, t0f.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - t0f
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
!**********************************************************************
SUBROUTINE t0f (x, y, xs, ys, xmax, xmin, ymax, ymin, n, ivz)

DIMENSION x ( * ), y ( * )

IF (ivz.eq.1) then
  CALL minmaxm (n, x, j, xmin, 'min')
  CALL minmaxm (n, y, j, ymin, 'min')
  CALL minmaxm (n, x, j, xmax, 'max')
  CALL minmaxm (n, y, j, ymax, 'max')
  DO i = 1, n
    x (i) = (x (i) - xmin * ivz) / (xmax - xmin + 1.e-04)
    y (i) = (y (i) - ymin * ivz) / (ymax - ymin + 1.e-04)
  END DO
ENDIF
IF (ivz.eq. - 1) then
  DO i = 1, n
    x (i) = x (i) * (xmax - xmin) + xmin
    y (i) = y (i) * (ymax - ymin) + ymin
  END DO
  xs = xs * (xmax - xmin) + xmin
  ys = ys * (ymax - ymin) + ymin
ENDIF

END SUBROUTINE t0f                                         
