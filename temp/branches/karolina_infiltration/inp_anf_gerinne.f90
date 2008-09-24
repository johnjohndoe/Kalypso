!     Last change:  JH    2 Jan 2007    5:16 pm

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

	subroutine inp_anf_gerinne(fnam,idatsa20,nanff,qg_lz,iok)

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!   24.09.2004  Jessica Huebsch (JH)    Anfangswerte können auch aus der Kurzzeitsimulation zu
!   bis                                 einer beliebigen Stunde geschrieben werden. Hierzu wurde
!   30.09.2004                          das Format des *.lzg Files geaendert, da hier zusaetzlich
!					zum Datum der Anfangswerte jetzt auch die Stunden
!					hinzugefügt wurden. Die Einleseroutine wurde daher dem
!					entsprechend umformuliert.

!   12.11.2004 Jessica Huebsch (JH)	Anfangswerte können sowohl im alten als auch im neuen
!					Format mit Stunden eingelesen werden.



!***************************************BESCHREIBUNG********************************************
!JH 23.09.2004
!c	Anfangswerte werden aus *.lzg-Dateien fuer Kalypso-Gerinneabfluss eingelesen.
!c
!c
!***************************************EIN-/AUSGABE********************************************
!JH 23.09.2004
!
!       Eingabe:
!			fnam    	Dateipfad der *.lzg-Datei
!                       idatsa20        Anfang der Simulation (8-stellig, z.B. 19851103, yyyymmdd)
!                       nanff           Kanalnummer der *.lzg-Datei
!       Ausgabe:
!                       qg_lz           Eingelesener Anfangswert für den Gesamtabfluss
!                       iok             Flag zu Abfrage, ob Anfangswert vorhanden ist
!                                               iok = 1         kein Anfangswert vorhanden
!                                               iok = 0         Anfangswert vorhanden
!
!******************************************VARIABLEN********************************************
        USE generic_LOGGER
        INCLUDE 'include\nad.cmn'

	character*80	fnam
	character*4	textken
	integer		idatsa20,nanff
	integer		iok
	real		qg_lz
        INTEGER 	istd

        CHARACTER*80	dummy

!******************************************ANWEISUNGEN******************************************

	iok=1
	if(nanff.eq.0) return                                           	! Kein File vorhanden (Kanalnummer = 0)
!JH 27.09.2004 Lesen an Schreiben angepasst!!!
!vorher:
!100	read (nanff,9900,err=510,end=500) idat,ianz,textken(1:3)        	! Lesen der Anfangswertdatei *.lzg
!	if ((idat-idatsa20).ge.0.and.textken(1:3).eq.'qgs') then        	! Wenn Startdatum erreicht und textkenn=qgs
100     READ (nanff,'(a)',err=510,end=500) dummy                        	! Lesen der Anfangswertdatei *.lzg
        IF (dummy(14:14) .EQ. 'h') THEN
            read (dummy,9900) idat,istd,ianz,textken(1:3)                       ! neues Format mit Stunden
        ELSE
            read (dummy,9901) idat,ianz,textken(1:3)                            ! altes Format
        END IF
	if (idat.EQ.idatsa20 .and. istd.EQ.nast .and. textken(1:3).EQ.'qgs') then        ! Wenn Startdatum erreicht und textkenn=qgs
	   read(nanff,'(4x,f9.3)',err=510) qg_lz                	! Einlesen der Anfangswerte
	   iok=0
	   return
	else                                                            	! Startdatum noch nicht erreicht
	   do 150 i=1,ianz                                              	! Einlesen der nicht verwendeten Werte, ianz ist Zeilenanzahl
	    read(nanff,*)
150	   continue
	end if
	goto 100

500	continue                                                        	! Anfangswert nicht gefunden
      	write (*,501) idatsa20
        call writeLogIntInt(6,'Keine Anfangswerte fuer Simulationsanfang vorhanden!',&
             & 'No initial values for simulation startdate found!','','',0,'idatsa20',idatsa20,'inp_anf_gerinne')

	return

510	continue                                                        	! Fehler in der Anfangswertdatei *.lzg
      	write (*,511) idatsa20,fnam
        call writeLogIntInt(6,'Fehler beim Einlesen der Anfangswerte!','Error when reading initial values!',&
             & fnam,'',0,'idatsa20',idatsa20,'inp_anf_gerinne')

	return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

501	FORMAT(/ 1X, 'Keine Anfangswerte fuer Startdatum ',i8,' gefunden.')
511	FORMAT(/ 1X, 'Fehler beim Einlesen der Anfangswerte!'/ &
     	        & 1X, 'Startdatum ', i8/ &
     	        & 1X, 'Filename: ', a)

!JH 27.09.2004 Lesen an Schreiben angepasst!!!
9900	FORMAT(i8,2x,i2,2x,i4,1x,a)
!JH altes Format (auch noch moeglich)
9901	FORMAT(i8,4x,i4,4x,a)
	end
