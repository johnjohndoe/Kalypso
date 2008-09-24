!     Last change:  JH    2 Jan 2007    5:25 pm

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

!SK
!SK   -----------------------------------------------------------------------
!SK   Änderung am 28.02.03 bis 04.03.03 Stephan Kraessig (SK), Prof. Pasche (EP)
!SK   -----------------------------------------------------------------------
!SK   Neue Version ist Jahr 2000 faehig und kann bei Angabe von 1000001 als
!SK   Anfangsdatum in der *.konfig Datei für jeden gerechneten Zeitschritt
!SK   der Langzeitsimulation Anfangswerte für die Kurzzeitsimulation erzeugen.
!SK   30.07.2003: Dies war ursprünglich die inp_nied.lz-Datei. Hier wird sie jetzt benutzt, um 
!SK   30.07.2003 Temperatur und Verdunstung in getrennten Dateien einlesen zu können.
!SK   30.07.2003 Verwendet wird das Format EX2. Das Format EX2 wurde von 3x auf 1x geaendert format(2(i2,1x),i4,1x,i2,1x,a).
!SK   30.07.2003 in der Kopfzeile muss das Wort EX2 stehen. Dann folgt das Format.
!SK   30.07.2003 in der Gebietsdatei wird nun zuerst Name der Temperatur-, dann der Verdunstungsdatei eingelesen (gleiche Zeile)
	subroutine inp_nied_tv(namfn,                      &
     		naja,namo,natg,                             &
     		neja,nemo,netg,                             &
     		p,idif,ierr)
!c
!c	program liest nieders. tageswerte (NASIM-BLOCK-format) ein
!c	formate (3. zeile:)
!c	NASIM...		(NASIM-BLOCK-format)
!c	DWD.....		DWD-Format:  JahrMoTg
!c	EXEL....        tt.mm.jjjj		wert	Exel-Ausgabeformat
!c	UVF....         jjmmttssmm	wert		UVF Format-Ausgabeformat
!c	EX2..			tt.mm.jjjj ss:mm  wert
!c		angabe anfangsdatum
!c			enddatun
!c		oder 	anzahl der werte
!c
!c	ausgabe:
!c		p	(idimt)	[mm] 	niederschlag tageswerte
!c		idif	int		anzahl n.werte (zeitschritte)
!c

        USE generic_LOGGER
      USE Units        
	include 'include\param.cmn'
        INTEGER maxchar
        parameter (maxchar=120)

	character*(maxchar)	namfn,text
	character*10	dummy
	character*20	dummy2

	integer naja,namo,natg,                             &
     		neja,nemo,netg,idif,                         &
     		itag1,imon1,ijah1,istu1,imin1
	integer ierr

	integer	inum,ianz,iform,iinp,i
	real	dtc


	real	pinp(400),p(idimt)


	if(naja.lt.100) then
		naja=naja+1900
	endif
	if(neja.lt.100) then
		neja=neja+1900
	endif

!c	if(idif.gt.0) then
!c		nend=idif
!c	else
!c		nend=ntdif(natg,namo,naja,netg,nemo,neja)+1
!c	endif
!c	idif=0


!cccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der niederschlagswerte aus datenfile
!c

	idif=0
	nend=ntdif(natg,namo,naja,netg,nemo,neja)+1
	nfil=ju0gfu()
	open (nfil,err=999,status='old',file=namfn)

6600	read(nfil,'(a)',end=998) text
	if (text(1:2).eq.'/*'.or.text(1:1).eq.'$') then
		goto 6600
	endif
	iform=1
	if(text(1:3).eq.'NAS'.or.text(1:4).eq.'/NAS' .or.		&
		text(1:3).eq.'nas'.or.text(1:4).eq.'/nas') iform=1
	if(text(1:3).eq.'DWD'.or.text(1:4).eq.'/DWD' .or.		&
		text(1:3).eq.'dwd'.or.text(1:4).eq.'/dwd') iform=2
	if(text(1:3).eq.'EXE'.or.text(1:4).eq.'/EXE' .or.		&
		text(1:3).eq.'exe'.or.text(1:4).eq.'/exe') iform=3
	if(text(1:3).eq.'NAG'.or.text(1:4).eq.'/NAG') iform=4
	if(text(1:3).eq.'UVF'.or.text(1:4).eq.'/UVF'  .or.          &
		text(1:3).eq.'uvf'.or.text(1:4).eq.'/uvf'            &
     		.or.text(1:2).eq.'*Z') iform=5
	if(text(1:3).eq.'EX2'.or.text(1:4).eq.'/EX2'.or.	&
		text(1:3).eq.'ex2'.or.text(1:4).eq.'/ex2') iform=6

	goto(3001,3002,3002,3002,3002,3002)iform

!cccc nasim Blockformat

3001	continue


	read(nfil,6100,end=998) inum,ijah1,imon1,itag1,istu1,imin1,ianz,dtc

        write (nerr,9901)inum,ijah1,imon1,itag1,istu1,imin1,ianz,dtc
	read(nfil,*)
!c SK/EP 28.02.03 Format für Niederschlaege Langzeit ist schon für das Jahrhundert
!c auf 4stellig geaendert worden.
6100	format (i5,2x,i4,4i2,i4,f4.0)
9901    FORMAT(i5,4x,i4,4i2,i4,f4.0)

	if(ijah1.lt.100) then
		ijah1=ijah1+1900
	endif
	nstart=ntdif(itag1,imon1,ijah1,natg,namo,naja)+1+idif

	idummy=abs(ianz/12)
	n=0
	if(idummy*12.lt.ianz) idummy=idummy+1
	do 20 i=1,idummy
		read(nfil,505) (pinp(m),m=n+1,n+12)
		n = i*12
20	continue


505	format(7x,12(f6.2))

	if(ianz.lt.nstart) goto 6600

        WRITE(nerr,9902)ianz,nstart,idummy



9902    FORMAT(3i10)
	m1=nstart-1
	ianz=min(nend,ianz-m1)

	do 197 m=1,ianz
			if(m+idif.gt.idimt) goto 997
			p(m+idif)=pinp(m+m1)
197	continue

	idif=idif+ianz
        WRITE(nerr,9903)ianz,nstart,idif,nend,m


9903    FORMAT(5i10)
	if(m.le.nend) then
		nend=nend-idif
		goto 6600
	endif
	goto 3100

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	DWD oder EXEL Format

3002	continue
	m=1
	if(iform.eq.2) then
		read(nfil,4002,end=3103,err=3102)                 &
     			ijah1,imon1,itag1,iinp
		p(m)=float(iinp) *.1
	elseif(iform.eq.3) then
		read(nfil,4003,end=3103,err=3102)                 &
     			itag1,imon1,ijah1,dummy(1:10)
		read(dummy(1:10),*)p(m)
	elseif(iform.eq.4) then
		read(nfil,4003,end=3103,err=3102)                 &
     			itag1,imon1,ijah1,dummy2
		read(dummy2(11:20),*)p(m)
	elseif(iform.eq.5) then
		text=''
		read(nfil,'(a2)')text(1:2)
		read(nfil,'(a2)')text(1:2)
		read(nfil,'(a2)')text(1:2)
		read(nfil,4005,end=3103,err=3102)                  &
     			ijah1,imon1,itag1,istu1,dummy2
		if(istu1.eq.7.or.istu1.eq.8) then
			call cdatum(itag1,imon1,ijah1,-1)
		endif
		read(dummy2(1:10),*)p(m)
	elseif(iform.eq.6) then
		read(nfil,4006,end=3103,err=3102)                  &
     			itag1,imon1,ijah1,istu1,dummy2
		if(istu1.eq.7.or.istu1.eq.8) then
			call cdatum(itag1,imon1,ijah1,-1)
		endif
		read(dummy2(1:15),*)p(m)
	endif

	nend=ntdif(itag1,imon1,ijah1,natg,namo,naja)
	if(nend.lt.0) then
		goto 3103
	elseif(nend.eq.1) then
		m=m-1
	elseif(nend.gt.1) then
		do 3300 i=1,nend-1
			read(nfil,'(a1)')text(1:1)
3300		continue

		if(iform.eq.2) then
			read(nfil,4002,end=3103,err=3102)          &
     				ijah1,imon1,itag1,iinp
			if(iinp.lt.-900)iinp = 0
			p(m)=float(iinp) *.1
		elseif(iform.eq.3) then
			read(nfil,4003,end=3103,err=3102)          &
     				itag1,imon1,ijah1,dummy(1:10)
			read(dummy(1:10),*)p(m)
		elseif(iform.eq.4) then
			read(nfil,4003,end=3103,err=3102)          &
     				itag1,imon1,ijah1,dummy2
			read(dummy2(11:20),*)p(m)
		elseif(iform.eq.5) then
			read(nfil,4005,end=3103,err=3102)          &
     				ijah1,imon1,itag1,istu1,dummy2
			if(istu1.eq.7.or.istu1.eq.8) then
				call cdatum(itag1,imon1,ijah1,-1)
			endif
			read(dummy2(1:10),*)p(m)
		elseif(iform.eq.6) then
			read(nfil,4006,end=3103,err=3102)          &
     				itag1,imon1,ijah1,istu1,dummy2
			if(istu1.eq.7.or.istu1.eq.8) then
				call cdatum(itag1,imon1,ijah1,-1)
			endif
			read(dummy2(1:15),*)p(m)
		endif
		nend=ntdif(itag1,imon1,ijah1,natg,namo,naja)
		if(nend.ne.0) goto 3101
	endif

	nend=ntdif(natg,namo,naja,netg,nemo,neja)+1-m

	do 3301 i=1,nend
		m=m+1
		if(iform.eq.2) then
			read(nfil,4002,end=3103,err=3102)         &
     				ijah1,imon1,itag1,iinp
			if(iinp.lt.-900)iinp = 0
			p(m)=float(iinp) *.1
		elseif(iform.eq.3) then
			read(nfil,4003,end=3103,err=3102)         &
     				itag1,imon1,ijah1,dummy(1:10)
			read(dummy(1:10),*)p(m)
		elseif(iform.eq.4) then
			read(nfil,4003,end=3103,err=3102)         &
     				itag1,imon1,ijah1,dummy2
			read(dummy2(11:20),*)p(m)
		elseif(iform.eq.5) then
			read(nfil,4005,end=3103,err=3102)          &
     				ijah1,imon1,itag1,istu1,dummy2
			if(istu1.eq.7.or.istu1.eq.8) then
				call cdatum(itag1,imon1,ijah1,-1)
			endif
			read(dummy2(1:10),*)p(m)
		elseif(iform.eq.6) then
			read(nfil,4006,end=3103,err=3102)          &
     				itag1,imon1,ijah1,istu1,dummy2
			if(istu1.eq.7.or.istu1.eq.8) then
				call cdatum(itag1,imon1,ijah1,-1)
			endif
			read(dummy2(1:15),*)p(m)
		endif
3301	continue

	nend=ntdif(itag1,imon1,ijah1,netg,nemo,neja)
	if(nend.ne.0) goto 3101
	idif=m
	goto 3100

!ccc DWD-Format
4002	format(1x,i4,2(i2),i5)

!ccc EXEL-Format      tt:mm:jjjj
4003	format(2(i2,1x),i4,a)

!ccc EXEL2-Format mit tt:mm:jjjj ss:mm
4006	format(2(i2,1x),i4,1x,i2,1x,a)

!ccc UVF-format   jjmmttssmm
4005	format(4(i2),2x,a)


3100	close(nfil)

return


999	write(nerr,909)namfn
	write(*,909)namfn

909	format(/'xxx datenfile mit niederschlagsdaten nicht gefunden!',/  &
     		'xxx filename: ',a)

        call writeLogString(7, 'Datenfile nicht vorhanden!', 'File not found!',namfn,'','','','','inp_nied_tv')

	ierr=1
	return

998	if(idif.le.0) then
   	   write(nerr,908)

           call writeLogString(7,'Keine Temperatur- und/oder Verdunstungsdaten innerhalb vorgegebenem Zeitintervall gefunden!',&
                & 'No temperature and/or evaporation data was not found within the time interval!',namfn,'','','','','inp_nied_tv')

	else
	   write(nerr,918)idif
           call writeLogString(7,'Simulationszeitraum nicht vollstaendig von Temperatur- und/oder Verdunstungsdaten abgedeckt!',&
                & 'Simulation period not totally covered by temperature and/or evaporation data.',namfn,'','','','','inp_nied_tv')

	endif
	ierr=1

	close(nfil)

	return

908	format(/'xxx keine niederschlag innerhalb vorgegebenem ',          &
     		'zeitintervall gefunden!',/'xxx filename: ',a)
918	format(/'xxx simulationszeitraum wird nicht vollstaendig von '/  &
     		'xxx niederschlags-datenfile abgedeckt!',/               &
     		'xxx anzahl uebernommener tageswerte: ',i6)

997	write(nerr,907)

        call writeLogIntInt(7, 'Mehr als max. moegliche Anzahl von Temperatur- und/oder Verdunstungsdaten ueberschritten!',&
              & 'Max. number of temperature- and/or evaporation reached!','','',0,'idimt',idimt,'inp_nied_tv' )


	ierr=1
	return
907	format(/'xxx mehr als idimt niederschlagsdaten innerhalb ',/    &
     		'xxx simulationszeitraum!'/                              &
     		'xxx erhoehe parameter idimt in inp_nied_c')

3101	write(nerr,3151)namfn(1:60)

        call writeLogString(7, 'Temperatur- und/oder Verdunstungswert fehlt im Eingabefile!', &
             & 'Temperature and/or evaporation value is missing!',namfn(1:60),'','','','','inp_nied_tv')

	ierr=1
	return
3151	format(/'xxx fehler beim einlesen der niederschlagsdaten ',/    &
     		'xxx niederschlagswert fehlt im eingabefile!',           &
     		'xxx filename eingabefile: ',a)

3102	write(nerr,3152)namfn(1:60)

        call writeLogString(7, 'Formatfehler in den Temperatur- und/oder Verdunstungseingabedateien!', &
             & 'Error reading the temperature and/ or evaporation data, format error in input file!',&
	     & namfn(1:60),'','','','','inp_nied_tv')


	ierr=1
	return
3152	format(/'xxx fehler beim einlesen der niederschlagsdaten ',/    &
     		'xxx formatfehler im eingabefile!',                     &
     		'xxx filename eingabefile: ',a)

3103	write(nerr,3153)namfn(1:60)

        call writeLogString(7,'Simulationszeitraum nicht vollstaendig von Temperatur- und/oder Verdunstungsdaten abgedeckt!',&
            & 'Simulation period not totally covered by temperature and/or evaporation data.',namfn(1:60),'','','','','inp_nied_tv')


	ierr=1
	return
3153	format(/'xxx fehler beim einlesen der niederschlagsdaten ',/      &
     		'xxx zeitraum der simulation wird nicht vollstaendig ',/   &
     		'xxx durch daten im eingabefile abgedeckt!',/              &
     		'xxx filename eingabefile: ',a)

	end
