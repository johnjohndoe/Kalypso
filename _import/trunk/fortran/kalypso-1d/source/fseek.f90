!     Last change:  WP   30 May 2005   10:42 am
!--------------------------------------------------------------------------
! This code, fseek.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Functions:
! - fseek
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

INTEGER function fseek (nf, offset, from)

USE DIM_VARIABLEN

! Calling variabales
INTEGER, INTENT(IN) :: nf
INTEGER, INTENT(IN) :: offset
INTEGER, INTENT(IN) :: from

! Local variables
CHARACTER (LEN=nch80) :: satz

! positionieren auf das Dateiende
IF (from.eq.2) then
  60 READ (nf, *, ERR = 100, END = 90) satz
  GOTO 60
ENDIF

! positionieren um "offset" von der aktueller Position
IF (from.eq.1) then
  DO i = 1, offset
    READ (nf, *, ERR = 100) satz
  END DO
ENDIF

! positionieren um "offset" von der Dateianfang
IF (from.eq.0) then
  REWIND (nf, ERR = 100)
  DO i = 1, offset
    READ (nf, *, ERR = 100) satz
  END DO
ENDIF

90 CONTINUE

fseek = 0
BACKSPACE nf
GOTO 110

100 CONTINUE

fseek = 1
PRINT * , 'Error beim positionieren der Datei'

110 CONTINUE

RETURN
END FUNCTION fseek                                         
