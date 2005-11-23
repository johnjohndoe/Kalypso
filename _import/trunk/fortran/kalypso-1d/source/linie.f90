!     Last change:  WP   26 May 2005   10:26 am
!--------------------------------------------------------------------------
! This code, linie.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - linie
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

SUBROUTINE linie (hz, x1, h1, nknot, il, ir, npl, xl, hl)

!WP 01.02.2005
USE DIM_VARIABLEN

REAL x1 (maxkla), h1 (maxkla), xl (maxkla), hl (maxkla)
REAL zl (maxkla)

INTEGER n (maxkla)


xl (1) = x1 (il)
hl (1) = hz

i1 = 2
                                                                        
DO i = il, ir
  xl (i1) = x1 (i)
  hl (i1) = h1 (i)
  i1 = i1 + 1
END DO
                                                                        
xl (i1) = xl (i1 - 1)
hl (i1) = hz

npl = i1 
                                                                        
gen = 0.001
CALL pktfilt (npl, n, xl, hl, zl, gen)
                                                                        

END SUBROUTINE linie
