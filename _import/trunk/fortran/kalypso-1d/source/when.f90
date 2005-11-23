!     Last change:  WP    2 May 2005    5:43 pm
!--------------------------------------------------------------------------
! This code, when.f90, contains the following subroutines 
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - when
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
! ST 10 Mar. 2005


SUBROUTINE WHEN (MMTTJJ, HHMM, HHMMSS, HHMMSSHH)

!***********************************************************************
!**   SUBROUTINE WHEN                                                ***
!***********************************************************************
!***********************************************************************
!**                                                                     
!**   SUB ZUM ABFRAGEN VON DATUM UND UHRZEIT BEIM LAUFEN DES PROGRAMMES 
!**                                                                     
!**   HHMMSSHH: STUNDE; MINUTE; SEKUNDE; HUNDERTSTELSEKUNDE             
!**   HHMMSS  : STUNDE; MINUTE; SEKUNDE                                 
!**   MMTTJJ  : MONAT; TAG; JAHR                                        
!**   HHMM    : STUNDE; MINUTE                                          
!**                                                                     
!********************************************************************** 

implicit none

!Output-Variablen
CHARACTER(LEN=11), INTENT(OUT):: HHMMSSHH
CHARACTER(LEN=8), INTENT(OUT):: HHMMSS, MMTTJJ
CHARACTER(LEN=5), INTENT(OUT):: HHMM

!Lokale Variablen
CHARACTER(LEN=8) :: DATE
CHARACTER(LEN=10):: TIME
CHARACTER(LEN=5):: ZONE
INTEGER, DIMENSION(8):: VALUE


call DATE_AND_TIME(DATE, TIME, ZONE, VALUE)           	! In Fortran integrierte Subroutine
! DATE returns a string CCYYMMDD
        !mit CC = Jahrhundert (z.B. 20)
        !    YY = Jahr        (z.B. 05)
        !    MM = Monat
        !    DD = Tag

! TIME returns a string HHMMSS.SSS
        !mit HH = Stunde
        !    MM = Minuten
        !    SS = Sekunden
        !    .SSS = Millisekunden

! ZONE returns a string in the form +/- HHMM, where HHMM is the time difference between local time and
! Coordinate Universal time (UTC or GMT)

! Value ist ein Array mit 8 Positionen
        ! 1     Jahrhundert und Jahr (z.B. 2005)
        ! 2     Monat
        ! 3     Tag
        ! 4     Zeitzonendifferenz
        ! 5     Stunde
        ! 6     Minuten
        ! 7     Sekunden
        ! 8     Millisekunden                                                                         

! Lokalen Variablen in die Ausgabevariablen schreiben
HHMMSSHH(1:2)  = TIME(1:2)
HHMMSSHH(3:3)  = ':'
HHMMSSHH(4:5)  = TIME(3:4)
HHMMSSHH(6:6)  = ':'
HHMMSSHH(7:8)  = TIME(5:6)
HHMMSSHH(9:11) = TIME(7:9)

HHMMSS(1:2)  = TIME(1:2)
HHMMSS(3:3)  = ':'
HHMMSS(4:5)  = TIME(3:4)
HHMMSS(6:6)  = ':'
HHMMSS(7:8)  = TIME(5:6)

MMTTJJ(1:2)  = DATE(5:6)
MMTTJJ(3:3)  = '/'
MMTTJJ(4:5)  = DATE(7:8)
MMTTJJ(6:6)  = '/'
MMTTJJ(7:8)  = DATE(3:4)

HHMM(1:2)  = TIME(1:2)
HHMM(3:3)  = ':'
HHMM(4:5)  = TIME(3:4)     

END SUBROUTINE WHEN

