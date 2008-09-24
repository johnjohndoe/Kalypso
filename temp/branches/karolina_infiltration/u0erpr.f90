!     Last change:  JH   17 Jan 2007    8:21 am

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
! Dipl.-Ing. Jessica H�bsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica H�bsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************

      subroutine u0erpr(key,nrfehl,text,LEN_TEXT,NAME,LEN_NAME)

!**************************************�NDERUNGEN***********************************************
!     Date      Programmer      	Description of change
!     ====      ==========      	=====================

!***************************************BESCHREIBUNG********************************************
!
!  Die Subroutine gibt eine Fehlermeldung aus.
!

!***************************************EIN-/AUSGABE********************************************
!JH 31.01.2005
!     Eingabe:
!		key		Programmabbruchvariable (Bei key=2 wird das Programm weiter gef�hrt).
!		nrfehl          Fehlernummer (meist aus iostat)
!		text		Fehlertext
!               name            name der aufrufenden Routine (in welcher der Fehler aufgetreten ist)

!******************************************VARIABLEN********************************************
      INTEGER, INTENT(IN) :: LEN_TEXT,LEN_NAME
      INTEGER 		  :: key
      INTEGER 		  :: nrfehl
      INTEGER 		  :: ltext
      INTEGER 		  :: lname
      CHARACTER (LEN=LEN_TEXT)     :: text
      CHARACTER (LEN=LEN_NAME)     :: name

!******************************************ANWEISUNGEN******************************************
  
      if (nrfehl.EQ.0) return
      ltext= ju0nch(text)
      lname= ju0nch(name)
      write(*,1000) nrfehl,text(:ltext),name(:lname)


      if (key .eq. 2) return
!      call writeFooter()
      stop


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
1000  FORMAT(/ 1X, 'Fehler= ',i12 /&
             & 1X, a,'  (',a,')')

      end
