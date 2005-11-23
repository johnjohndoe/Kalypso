!     Last change:  WP   26 May 2005   10:27 am
!--------------------------------------------------------------------------
! This code, lu0tr.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - lu0trf
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


SUBROUTINE lu0trf (lunit)

! Die Subroutine LU0TRF stutzt einen Datensatz ab Position des Pointers
! bis zum Ende.

! Calling variables
INTEGER, INTENT(IN) :: lunit

! Local variables
LOGICAL :: is_open
INTEGER :: ioerr

INQUIRE (unit = lunit, opened = is_open, iostat = ioerr)
if (ioerr /= 0) then
  write (*,1000) lunit
  1000 format (//1X, 'Fehler in LU0trf beim Versuch, die Datei mit der internen UNIT ', I5, /, &
               & 1X, 'zu untersuchen! Bitte beim Entwickler melden!', /, &
               & 1X, '-> PROGRAMMABBRUCH!')
  stop
end if

if (.not. is_open) then
  write (*, 1001) lunit
  1001 format (//1X, 'Fehler in LU0trf beim Versuch, die Datei mit der internen UNIT ', I5, /, &
               & 1X, 'zu untersuchen! Die Datei wurde noch nicht geoeffnet!', /, &
               & 1X, '-> PROGRAMMABBRUCH!')
  stop
end if

ENDFILE (UNIT=lunit)

BACKSPACE (UNIT=lunit)

END SUBROUTINE lu0trf                         
