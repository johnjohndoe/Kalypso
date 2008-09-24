!     Last change:  JH   26 Jul 2006    2:46 pm

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


	subroutine inp_lzsim(fnam,ilenlz,iz,nanf)

!**************************************ÄNDERUNGEN***********************************************
!AG    Subroutine inp_lzsim
!AG
!AG    Änderung am 19.06.02 Alexander Goetze (AG) inp_lzsim_00a
!AG   -----------------------------------------------------------------------
!AG   Vorherige Version konnte keine Werte in die LZSIM-Dateien schreiben,
!AG   außerhalb des ersten Zyklus oder wenn in der Datei schon Werte standen
!AG   und die Datei fortgesetzt werden sollte.
!AG   Folgende Fehlermeldung tauchte dabei auf:
!AG   'A WRITE statement cannot be executed after an endfile record is read (unit=124).'
!AG   Dies führte dann zu einem Programmabbruch in dem Zyklus, in dem das Datum
!AG   für die Langzeitwerte in der Konfig-Datei lag.
!AG   Änderungen sind alle mit Kürzel (AG) und Änderungsdatum (190602)
!AG   gekennzeichnet.


!***************************************BESCHREIBUNG********************************************
!JH
!c	falls	iz=0	wenn vorhanden wird File geoeffnet; falls nicht vorh, nanf=0
!c		iz>1	File wird geoeffnet und bis ans Ende gelesen (um alte Daten nicht zu überschreiben?!?).
!c		iz=1	neuer File (evtl. bestehender File wird geoeffnet und dann geschlossen und
!c                      geloescht. Anschließend wird einer neuer File erzeugt.)
!c
!c		ntg	nummer teilgebiet
!c
!c
!***************************************EIN-/AUSGABE********************************************
!JH 01.09.2004
!
!       Eingabe:
!			fnam    Dateipfad
!                       ilenlz  Laenge des Dateipfades
!                       iz      Flag: Wie wird die Datei behandelt/ welcher Fall wird in dieser
!                                     Routine angesteuert.
!       Ausgabe:
!                       nanf    Kanalnummer der Datei fnam
!
!******************************************VARIABLEN********************************************
        USE generic_LOGGER

	integer		nanf
	character*80	fnam
	logical		lvor


!******************************************ANWEISUNGEN******************************************
	inquire(file=fnam,exist=lvor)   			! Existiert die Datei schon?
        nanf = ju0gfu()                 			! Kanalnummer

	if (iz.eq.1) then
	   if (lvor) then                                       ! Existiert die Datei bereits, dann
	      open  (nanf,err=455,file=fnam)    		! Oeffnen
	      close (nanf,err=455,status='delete')              ! Schließen und Löschen
	   endif

	   open(nanf,file=fnam,err=300,form='formatted')        ! Datei neu erzeugen (oeffnen)
	   goto 301
300	   call system('mkdir '//fnam(1:ilenlz))
	   open(nanf,file=fnam,err=456,form='formatted')

301	   continue
	   return
	end if


	if(.not.lvor .and. iz.eq.0) then
	   nanf=0
	   return
	endif

        ! iz > 1 oder lvor .and. iz.eq.0
	open (nanf,err=455,file=fnam,form='formatted',status='old')     ! Datei oeffnen
	rewind (nanf)                                                   ! Zum Dateianfang
	if(iz.eq.0) return

        ! iz > 1 Auslesen der Datei
10	read (nanf,'(6x)',end=11)
	read(nanf,*,end=11)
	goto 10
11	continue
!AG 19.06.02  Nach dem Lesen der Datei steht der Cursor hinter der End-Of-File-Marke.
!             Das führt dazu, dass in die geöffnete Datei nichts weiter geschrieben werden kann.
!             Deswegen wird hier eine BACKSPACE-Anweisung gesetzt.
        BACKSPACE nanf
	return

455	open (nanf,err=310,file=fnam,form='formatted',status='new')
	goto 311
310	call system('mkdir '//fnam(1:ilenlz))
	open (nanf,err=456,file=fnam,form='formatted',status='new')

311	continue
	return
 
456	write(*,1001) fnam
        call writeLogString(6,'Fehler beim Oeffnen einer Datei!','Error when opening a file!',&
             & fnam,'','','','','inp_lzsim')
	return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

1001    FORMAT (/ 1X, 'Fehler beim oeffnen von File',a)

	end
