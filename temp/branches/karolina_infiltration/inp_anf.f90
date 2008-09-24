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

	subroutine inp_anf(fnam,idatsa20,nanff,xhschnee,xwschnee,xheig,qb_lz,      &
     			   m_bi0,m_bl0,iok)



!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!   01.08.2002  Alexander Goetze (AG)   Vorherige Version las keine Anfangswerte aus der LZSIM-
!					Datei ein, wenn ohne Schnee gerechnet wurde. In diesem
!					Fall erfolgt keine Eintragung des Absatzes mit 'snow' in
!					der LZSIM-Datei. Das Programm springt dadurch, dass es
!					keine Eintragung findet, aus der Subroutine heraus, ohne
!					die Werte fuer Bodenfeuchte und Grundwasserstand
!					einzulesen.
!					Folgende Fehlermeldung tauchte dabei auf:
!					keine anfangswerte fuer startdatum ', i8,' gefunden.

!   28.02.2003 Stephan Kraessig (SK),   Neue Version ist Jahr 2000 faehig und kann bei Angabe von
!   bis        Prof. Pasche (EP)        1000001 als Anfangsdatum in der *.konfig Datei für jeden
!   04.03.2003                          gerechneten Zeitschritt der Langzeitsimulation
!					Anfangswerte für die Kurzzeitsimulation erzeugen.

!   24.09.2004 Jessica Huebsch (JH)     Anfangswerte können auch aus der Kurzzeitsimulation zu
!   bis                                 einer beliebigen Stunde geschrieben werden. Hierzu wurde
!   30.09.2004                          das Format des *.lzs Files geaendert, da hier zusaetzlich
!					zum Datum der Anfangswerte jetzt auch die Stunden
!					hinzugefügt wurden. Die Einleseroutine wurde daher dem
!					entsprechend umformuliert.

!   12.11.2004 Jessica Huebsch (JH)	Anfangswerte können sowohl im alten als auch im neuen
!					Format mit Stunden eingelesen werden.


!***************************************BESCHREIBUNG********************************************
!JH 29.09.2004
!
!	Anfangswerte werden aus der Datei *.lzs eingelesen.
!                   	- Grundwasser
!                       - Boden
!                       - Schnee
!

!***************************************EIN-/AUSGABE********************************************
!JH 28.09.2004
!
!       Eingabe:
!                       fnam            Dateipfad der *.lzs-Datei
!			idatsa20        Anfang der Simulation (8-stellig, z.B. 19851103, yyyymmdd)
!                       nanff           Kanalnummer der Datei *.lzs
!                       nerr            Kanalnummer der output.err Datei
!
!       Ausgabe:
!                       xhschnee        Anfangs? Schnee
!                       xwschnee        Anfangs? Schnee
!                       xheig           Anfangs? Grundwasser
!                       qb_lz           Anfangs? Grundwasser
!                       m_bi0           Anfangs? Boden
!                       m_bl0           Anfangs? Boden
!                       iok             Flag zur Abfrage, ob Anfangswert vorhanden ist
!                                               iok = 1         kein Anfangswert vorhanden
!                                               iok = 0         Anfangswert vorhanden
!
!
!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      include 'include\param.cmn'
      INCLUDE 'include\nad.cmn'
      CHARACTER*80      fnam
      character*4	textken
      integer		idatsa20,nanff
      real		xheig,qb_lz
      real		m_bi0(2*idimnutz),m_bl0(2*idimnutz,maxlay)
      INTEGER	 	istd

      CHARACTER*80	dummy
                                                           
!******************************************ANWEISUNGEN******************************************
      iok=1
      if(nanff.eq.0) return                                             	! Kein File vorhanden


! Anfangswerte Grundwasser
!-------------------------
!AG 01.08.2002  In der naechsten read-Anweisung wurde die end-Marke von
!AG             500 auf 502 geaendert.

!JH 27.09.2004 Lesen an Schreiben angepasst!!! Das "alte" Format ist auch noch möglich.
!vorher:
!100  read (nanff,9900,err=510,end=502) idat,ianz,textken(1:4)
!     if ((idat-idatsa20).ge.0.and.textken(1:4).eq.'gwsp') then
100   READ (nanff,'(a)',err=510,end=502) dummy                                          ! Lesen der Anfangswertdatei *.lzs
      IF (dummy(14:14) .EQ. 'h') THEN
          read (dummy,9900) idat,istd,ianz,textken(1:4)                                 ! neues Format mit Stunden
      ELSE
          read (dummy,9901) idat,ianz,textken(1:4)                                      ! altes Format
      END IF
      if (idat.EQ.idatsa20 .and. istd.EQ.nast .and. textken(1:4).EQ.'gwsp') then        ! Wenn Startdatum erreicht und textkenn=gwsp
!AG 010802  Die naechste goto-Anweisung wurde von 500 auf 502 geaendert.
!JH	 if ((idat-idatsa20).gt.0) goto 502
	 read(nanff,'(4x,f9.2,f9.3)',err=510) xheig,qb_lz                            ! Einlesen der Anfangswerte (Grundwasser)
	 goto 101
      else                                                                     		! Startdatum noch nicht erreicht
	 do 150 i=1,ianz
	  read(nanff,*)                                                               ! Einlesen der nicht verwendeten Werte, ianz ist Zeilenanzahl
150	 continue
      endif
      goto 100

101   rewind (nanff)                                                                    ! Ende Anfangswerte Grundwasser, *.lzs zum Dateianfang zurück setzen


! Anfangswerte Schnee
!--------------------
!AG 010802  In der naechsten read-Anweisung wurde die end-Marke von
!AG         500 auf 503 geaendert.

!JH 27.09.2004 Lesen an Schreiben angepasst!!!
!vorher:
!102  read (nanff,9900,err=510,end=503) idat,ianz,textken(1:4)
!     if ((idat-idatsa20).ge.0.and.textken(1:4).eq.'snow') then
102   READ (nanff,'(a)',err=510,end=503) dummy                                          ! Lesen der Anfangswertdatei *.lzs
      IF (dummy(14:14) .EQ. 'h') THEN
          read (dummy,9900) idat,istd,ianz,textken(1:4)                                 ! neues Format mit Stunden
      ELSE
          READ (dummy,9901) idat,ianz,textken(1:4)                                     ! altes Format
      END IF
      if (idat.EQ.idatsa20 .and. istd.EQ.nast .and. textken(1:4).EQ.'snow') then        ! Wenn Startdatum erreicht und textkenn=snow
!AG 010802  Die naechste goto-Anweisung wurde von 500 auf 503 geaendert.
	 read(nanff,'(4x,2(f9.2))',err=510) xhschnee,xwschnee                        ! Einlesen der Anfangswerte (Schnee)
	 goto 103
      else                                                                              ! Startdatum noch nicht erreicht
	 do 151 i=1,ianz
	  read(nanff,*)                                                               ! Einlesen der nicht verwendeten Werte, ianz ist Zeilenanzahl
151	 continue
      endif
      goto 102

103   rewind (nanff)                                                                    ! Ende Anfangswerte Schnee, *.lzs zum Dateianfang zurück setzen


! Anfangswerte Boden
!-------------------
!AG 010802  In der naechsten read-Anweisung wurde die end-Marke von
!AG         500 auf 504 geaendert.

!JH 27.09.2004 Lesen an Schreiben angepasst!!!
!vorher:
!104  read (nanff,9900,err=510,end=504) idat,ianz,textken(1:4)
!     if ((idat-idatsa20).ge.0.and.textken(1:4).eq.'bodf') then
104   READ (nanff,'(a)',err=510,end=504) dummy                                          ! Lesen der Anfangswertdatei *.lzs
      IF (dummy(14:14) .EQ. 'h') THEN
          read (dummy,9900) idat,istd,ianz,textken(1:4)                                 ! neues Format mit Stunden
      ELSE
          READ (dummy,9901) idat,ianz,textken(1:4)                                     ! altes Format
      END IF
      if (idat.EQ.idatsa20 .and. istd.EQ.nast .and. textken(1:4).EQ.'bodf') then        ! Wenn Startdatum erreicht und textkenn=bodf
!AG 010802  Die naechste goto-Anweisung wurde von 500 auf 504 geaendert.
	 do 320 nn=1,ianz
	  read(nanff,'(i4,f7.2,10(f7.2))',err=510) i,m_bi0(nn),(m_bl0(nn,ilay),ilay=1,maxlay)   ! Einlesen der Anfangswerte (Boden)
320	 continue
	 iok=0
	 return
      else
	 do 152 i=1,ianz
	  read(nanff,*)
152	 continue
      endif
      goto 104

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

!AG 01.08.2002  Die naechsten 4 Zeilen sind eine Fehlerbehandlung die in dieser Version
!AG             nicht weiter verwendet wird und wurden deshalb auskommentiert.

!AG 500	continue
!AG      	write (*,501) idatsa20
!AG	return
!AG 501	format(/'keine anfangswerte fuer startdatum ', i8,' gefunden.')

!AG 010802  Die naechsten Marken 502 bis 507 sind die neuen Fehlerbehandlungen für diese
!AG         Version und wurden zusaetzlich eingefügt.
502   continue
      write (nerr,505) idatsa20
505   FORMAT(/ 1X, 'Keine Anfangswerte fuer Grundwasserspeicher bei Startdatum ',i8,' gefunden.')

      call writeLogIntInt(6,'Keine Anfangswerte fuer Grundwasserspeicher fuer das Simulationsstartdatum gefunden.', &
           & 'No initial values found for groundwater storage!','','',0,'idatsa20',idatsa20,'inp_anf' )

      GOTO 101

503   continue
      write (nerr,506) idatsa20
506   FORMAT(/ 1X, 'Keine Anfangswerte fuer Schnee bei Startdatum ',i8,' gefunden.')

      call writeLogIntInt(6, 'Keine Anfangswerte fuer Schnee fuer das Simulationsstartdatum gefunden.', &
           & 'No initial values found for snow.','','',0,'idatsa20',idatsa20,'inp_anf')
      GOTO 103

504   continue
      write (nerr,507) idatsa20
507   FORMAT(/ 1X, 'Keine Anfangswerte fuer Bodenfeuchte bei Startdatum ',i8,' gefunden.')

      call writeLogIntInt(6, 'Keine Anfangswerte fuer Bodenfeuchte fuer das Simulationsstartdatum gefunden.', &
           & 'No initial values found for soil moisture.','','',0,'idatsa20',idatsa20,'inp_anf')

      return

510   continue
      write (nerr,511) idatsa20,fnam

       call writeLogIntInt(6, 'Fehler beim Einlesen einer Datei (Anfangswerte)!', &
      & 'Error when reading initial values from the file.',fnam,'',0,'idatsa20',idatsa20,'inp_anf')

      return

511   FORMAT(/ 1X, 'Fehler beim Einlesen der Anfangswerte!'/  &
     	      & 1X, 'Startdatum ', i8/ &
              & 1X, 'Filename: ',a)


!JH 27.09.2004 Lesen an Schreiben angepasst!!!
9900  FORMAT(i8,2x,i2,4x,i4,1x,a)
!JH altes Format (auch noch möglich):
9901  format(i8,4x,i4,4x,a)

      end
