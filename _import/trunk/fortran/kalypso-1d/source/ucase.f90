!     Last change:  WP   30 May 2005    1:05 pm
!--------------------------------------------------------------------------
! This code, ucase.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - ucase
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



!***********************************************************************
SUBROUTINE ucase (string)
!
! geschrieben :                   05.08.88  e.pasche
! geaendert   :                   14.05.05  w.ploeger
!                                                                       
!-----------------------------------------------------------------------
! allgemeine beschreibung :
!                                                                       
! das programm ucase konvertiert von Kleinbuchstaben in Grossbuchstaben
!                                                                       
!-----------------------------------------------------------------------

CHARACTER ( * ) string

ia = ichar ('a')
iz = ichar ('z')
iadd = ichar ('A') - ichar ('a')

l = len (string)
DO n = 1, l
  k = ichar (string (n:n) )
  IF (k.lt.ia .or. k.gt.iz) CYCLE
  string (n:n) = char (k + iadd)
END DO

END SUBROUTINE ucase              
                                                                        
                                                                        

