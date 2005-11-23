!     Last change:  WP   30 May 2005   10:16 am
!--------------------------------------------------------------------------
! This code, integrat.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - integrat
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


SUBROUTINE integrat (max, n, x, y, fs, j)

!***********************************************************************
!**                                                                     
!**   SUB INTEGRAT                                                      
!**                                                                     
!**   KOMMENTIERUNG: BEGINN AM 01.09.2000                               
!**                                                                     
!***********************************************************************

DIMENSION x (max), y (max)

IF (max.le.n) then

  write (*, 9000) n+1
  9000 format (/1X, 'Felddimensionierung in Subroutine INTEGRAT nicht ausreichend!', /, &
              & 1X, 'Bitte erhoehen Sie den Parameter MAX auf mindestens ', I5, /, &
              & 1X, '->PROGRAMMABBRUCH!')
  STOP

ENDIF

fs = 0.0
n01 = n + 1

x (n01) = x (1)
y (n01) = y (1)

DO i = 1, n01 - 1

  fh = x (i + 1) **2 + x (i + 1) * x (i) + x (i) **2
  fs = fs + (y (i + 1) - y (i) ) * fh

END DO

fs = j * fs / 6.

END SUBROUTINE integrat                                                                   
