!     Last change:  WP   10 Mar 2006    9:22 pm
!--------------------------------------------------------------------------
! This code, close_units.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - close_units
!
! Copyright (C) 2005  WOLF PLOEGER.
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
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 18 Juli 2005
! Research Associate
!***********************************************************************


!-----------------------------------------------------------------------
subroutine close_units()

implicit none

! Local variables
INTEGER :: istat
INTEGER :: lunit
LOGICAL :: offen
                                                                        
!write (UNIT=0, FMT=1000)
write (UNIT=*, FMT=1000)
1000 format (//1X, 'Dateien werden geschlossen...')

do lunit = 10, 200

  INQUIRE (UNIT = lunit, OPENED = offen)

  IF (offen) then
    close (UNIT=lunit, IOSTAT=istat)
    if (istat/=0) then
      !write (0,1001) lunit
      write (*,1001) lunit
      1001 format (1X, 'Problem beim Schliessen von Unit Nr. ', I3, ' -> wird uebersprungen!')
    else
      !write (0,1002) lunit
      write (*,1002) lunit
      1002 format (1X, 'Unit Nr. ', I3, ' wird geschlossen.')
    end if

  end if

end do

!write (UNIT=0, FMT=9000)
write (UNIT=*, FMT=9000)
9000 format (/1X, 'Alle offenen Dateien wurden geschlossen!')

end subroutine close_units
