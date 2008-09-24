!     Last change:  JH    2 Jan 2007    5:22 pm

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

	subroutine inp_epot(namet,isim,dt,                &
        	naja,namo,natg,nast,namn,                 &
      		neja,nemo,netg,nest,nemn,                &
                epot,temp,idif,ierr)

!c	program liest verd. tageswerte (NASIM-BLOCK-format) ein
!c		angabe anfangsdatum
!c			enddatun
!c		oder 	anzahl der werte
!c		isim=0  dt=24, 1 wert/tag wird im eingabefile erwartet
!c		isim=1  kurzz, tageswerte werden auf zeitintervall
!c	ausgabe:
!c		epot	(idimt)	[mm] 	potentielle verdunstung (tagesw.)
!c		temp	(idimt)	[grad] 	temperatur (tageswert)
!c		idif	int		anzahl werte (zeitschritte)

        USE generic_LOGGER
        USE Units        
	include 'include/param.cmn'

	character*80	namet

	integer naja,namo,natg,neja,nemo,netg,idif,itag1,imon1,ijah1
	integer	ianz

	real	pinp(400),epot(idimt),tinp(400),temp(idimt)
	real	vek1(idimt),vek2(idimt)


	if(idif.gt.0.and.isim.eq.0) then
		nend=idif
	else
		nend=ntdif(natg,namo,naja,netg,nemo,neja)+1
	endif
	if(isim.eq.1) nend=nend+2


!cccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der niederschlagswerte aus datenfile
!c

	idif=0
	nfil=ju0gfu()
	open (nfil,iostat=ierr,err=999,status='old',file=namet)

6600	read(nfil,*,end=998)
	read(nfil,6100,end=998) ijah1,imon1,itag1,ianz
	read(nfil,*)

	nstart=ntdif(itag1,imon1,ijah1,natg,namo,naja)+idif
	if(isim.eq.0) nstart=nstart+1
	if(isim.eq.1.and.nstart.le.0) goto 996

!ccccccccccccccccc
!c
!c	bei langzeit: anfangs und endtag werden beruecksichtigt,
!c		dh. 1.1 - 31.12 = 365 tage
!c	bei kurzzeit: endtag nur bis zur angegebenen stunde,
!c		d.h.  26. 9 - 29. 9 = 3 tage

		do 20 i=1,ianz
			read(nfil,506) pinp(i),tinp(i)
20		continue

   
506	format(9x,2(1x,f6.2))

	if(ianz.lt.nstart) goto 6600

	m1=nstart-1
	ianz=min(nend,ianz-m1)

	do 197 m=1,ianz
			if(m+idif.gt.idimt) goto 997
			epot(m+idif)=pinp(m+m1)
			temp(m+idif)=tinp(m+m1)
197	continue

	idif=idif+ianz
	if(m.le.nend) then
		nend=nend-ianz
		goto 6600
	endif

	close(nfil)
!c	write(*,'(10(f6.2))')(epot(i),i=1,idif)

	if(isim.eq.0) return

	do 250 m=1,idif
		vek1(m)=24.*(m-1)
250	continue

! Warnung: tdif wurde verwendet, aber nicht definiert
!          da Wert von tdif nicht nachvollziehbar, wird tdif herausgenommen
!       Pasche 04.05.01

	tstart=nast+namn/60.+12.                   !+tdif
!c	tend=(nend-1)*24.-nast-namn/60.+nest+nemn/60.-tstart
	tend=(idif-2)*24.+nest+nemn/60.-12
	call n_approx(dt,tstart,tend,idif,vek1,epot,0,idif2,vek2,2,ierr)

	xtime=24./dt
	do 251 m=1,idif2
		epot(m)=vek2(m)/xtime
251	continue
!c	write(*,'(10(f6.2))')(vek2(i),i=1,idif2)

	call n_approx(dt,tstart,tend,idif,vek1,temp,0,idif2,vek2,2,ierr)

	do 252 m=1,idif2
		temp(m)=vek2(m)
252	continue

!c	write(*,'(10(f6.2))')(temp(i),i=1,idif2)

	idif=idif2
	return



999	write(nerr,909)
909	format( ' datenfile mit niederschlagsdaten nicht gefunden!')

        call writeLogString(5,'Datei mit Verdunstungsdaten nicht gefunden!', &
        & 'File with evapotranspiration data has not been found!',namet,'','','','','inp_epot' )

	ierr=1
	return



998	if(idif.le.0) then
	  write(nerr,908)

          call writeLogString(5, 'Keine Verdunstungsdaten innerhalb des vorgegebenen Zeitintervalls gefunden!', &
       	       & 'No evapotranspiration data within given time interval has been found!',namet,'','','','','inp_epot')

	else
          write(nerr,918)idif

          call writeLogIntInt(6, 'Simulationszeitraum wird nicht vollstaendig von Verdunstungsdaten abgedeckt!', &
       	       & 'Simulation period is not completely covered by evapotranspiration data file!',namet,'',0,'idif',idif,'inp_epot')

	endif
	ierr=1
	close(nfil)
	return

6100	format (9x,3i2,4x,i4,4x,8x)

908	format('xxx keine niederschlag innerhalb vorgegebenem ',     &
      		'zeitintervall gefunden!',/'xxx filename: ',a)
918	format(' simulationszeitraum wird nicht vollstaendig von ',   &
      		' niederschlags datenfile abgedeckt!',/              &
      		' anzahl uebernommener tageswerte: ',i6)

997	write(nerr,907)

        call writeLogIntInt(6, 'Mehr als moegliche Anzahl Verdunstungsdaten innerhalb des Simulationszeitraum!', &
        & 'More than possible number of evapotranspiration data within simulation period','','',0,'idimt',idimt,'inp_epot')

	ierr=1
	return
907	format( 'xxx mehr als idimt niederschlagsdaten innerhalb ',    &
      		'xxx simulationszeitraum!'/                            &
      		'xxx erhoehe parameter idimt in inp_nied_c')

996	write(nerr,906)
906	format( 'xxx keine inputdaten fuer simulationszeitraum gegeben',/  &
      	'xxx ein zusaetzl. wert vor simul.beginn muss gegeben sein!'/      &
      		'xxx verdunstungs und temperaturdaten!')

        call writeLogString(5,'Keine Inputdaten fuer Simulationszeitraum vorhanden!',&
             & 'No input data for the simulation period has been given!',namet,'','','','','inp_epot')

	ierr=1
	return

	end
