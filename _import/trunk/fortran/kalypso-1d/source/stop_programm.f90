!     Last change:  WP   25 Jan 2006    5:19 pm
!--------------------------------------------------------------------------
! This code, stop_programm.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - stop_programm
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
subroutine stop_programm(fehlnr)

USE VERSION

implicit none

! Calling variables
INTEGER, INTENT(IN) :: fehlnr

! Local variables
CHARACTER(LEN=1) :: antwort
INTEGER :: istat
INTEGER :: selected

if (fehlnr > 0) then

  call Check_Dialog(fehlnr, selected)

  if (selected ==0) then
    call close_units()
    WRITE ( *, 1000) VERSIONNR
    stop
  else
    RETURN
  end if

else if (fehlnr == 0) then

  call close_units()
  WRITE ( *, 1000) VERSIONNR
  stop

else

  ! Fall FEHLNR kleiner 0, dann werden nur alle offenen Datein geschlossen
  call close_units()

end if

1000 FORMAT (//1X, 'Berechnung mit ', A29, ' wurde abgebrochen.',//     &
             & 1X, 'BITTE PRUEFEN SIE DIE FEHLERMELDUNGEN!!!')


end subroutine stop_programm
