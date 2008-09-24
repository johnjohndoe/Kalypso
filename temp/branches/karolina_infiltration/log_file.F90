!     Last change:  SK   23 May 2006    5:40 pm
MODULE generic_LOG_FILE

!**************************************LICENSE**************************************************
!
! This code is part of the library 'Kalypso-NA'.
! KALYPSO-NA is a deterministic, non-linear, detailed Rainfall-Runoff-Model (RRM).
! The model permits a complete simulation of the land bound
! part of the global water balance as a reaction on observed precipitation events.
! Copyright (C) 2004  HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering (in co-operation with Bjoernsen Cunsulting Engineers)
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
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Dipl.-Ing. Jessica Hübsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Hübsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************

IMPLICIT NONE

INTERFACE LOG_FILE
! TODO: init-routine privat machen damit nur getLogFile nach aussen sichtbar ist
!MODULE PROCEDURE init
MODULE PROCEDURE getLogFile
END INTERFACE
PUBLIC
INTEGER, PRIVATE :: ndebug=-1
CHARACTER (LEN=9), PRIVATE :: fname = 'debug.log'
CONTAINS

SUBROUTINE init( no , path, LENGTH )
INTEGER, INTENT(IN) :: LENGTH
INTEGER, INTENT(INOUT) :: no
CHARACTER (LEN=LENGTH), INTENT(IN) :: path
!Local Vars
CHARACTER (LEN=LENGTH+LEN_TRIM(fname)) ::debugnam
INTEGER :: ju0gfu, idebug
   IF (ndebug == -1 ) THEN
      ! Erzeugen von debug.dat
      ! Diese Datei enthaelt alle im code enthaltenen detailierten Ausgaben
      !------------------------
      debugnam(1:LENGTH+LEN_TRIM(fname))=path//fname
      ndebug = ju0gfu()
      OPEN(ndebug,IOSTAT=idebug,FILE=debugnam,STATUS='REPLACE')
   END IF
   no=ndebug
END SUBROUTINE

SUBROUTINE getLogFile( no  )
INTEGER, INTENT(OUT) :: no
IF ( ndebug < 0 ) THEN
   CALL init( no,'',LEN_TRIM('') )
ELSE
  no = ndebug
ENDIF
END SUBROUTINE

END MODULE generic_LOG_FILE
