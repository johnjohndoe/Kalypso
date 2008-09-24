!     Last change:  JH   10 Mar 2007    1:55 pm

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


	subroutine inp_nied_lz(namfn,dt,     &
               naja,namo,natg,nast,namn,     &
               neja,nemo,netg,nest,nemn,     &
               p,idif,ierr)

!**************************************ÄNDERUNGEN***********************************************

!SK   ------------------------------------------------------------------------------------------
!SK   Änderung am 28.02.03 bis 04.03.03 Stephan Kraessig (SK), Prof. Pasche (EP)
!SK   ------------------------------------------------------------------------------------------
!SK   Neue Version ist Jahr 2000 faehig und kann bei Angabe von 1000001 als
!SK   Anfangsdatum in der *.konfig Datei für jeden gerechneten Zeitschritt
!SK   der Langzeitsimulation Anfangswerte für die Kurzzeitsimulation erzeugen.

!SK   ------------------------------------------------------------------------------------------
!SK   Änderung am 25.07.2003 Stephan Kraessig (SK)
!SK   ------------------------------------------------------------------------------------------
!SK   input_nied_kz wurde eingefügt um Langzeitniederschlaege im Grap-Format einzulesen.

!JH   ------------------------------------------------------------------------------------------
!JH   Änderung am 22.06.2004 Jessica Hübsch (JH)
!JH   ------------------------------------------------------------------------------------------
!JH   Da die Übernahme der inp_nied_kz zu verschiedensten Problemen führte (Verschiebung der
!JH   Niederschlaege Ein-/Ausgabe, Approximation,...), wurde diese überarbeitet und inp_nied_kz
!JH   und inp_nied_kz_dat zu einer neuen Subroutine inp_nied_lz zusammengefügt.
!JH   Es wird jetzt lediglich das grap-Format der Niederschlagsdatei Langzeit akzeptiert.


!***************************************BESCHREIBUNG********************************************
!JH 22.06.2004
!     In der Subroutine wird das Einlesen der Niederschlagsdaten für die Langzeitsimulation
!     behandelt.
!
!     Hinweise: - Es ist lediglich das grap-Format (ehemals aus Kurzzeitniederschlaegen bekannt)
!                 bei der Niederschlagsangabe möglich.
!               - Es müssen Tageswerte Angegeben werden, eine Approximation auf das
!		  Simulationsintervall erfolgt nicht.
!               - Alle Werte werden um ein Zeitintervall zurück versetzt (DWD misst am Folgetag
!     	          für den abgelaufenen Tag).
!               - Der Aufruf dieser Routine aus den Subroutinen sp_geisel und zufgang ist noch
!                 nicht vollstaendig erfolgt!!!
!
!     Unterstütztes Format:
!               -Header
!                  text
!                  inum,ijah1,imon1,itag1,istu1,imin1,ianz,dtc,fl
!		    Format (i5,2x,i4,4i2,i4,f4.0,f8.0)
!		   grap
!               -Werte
!                  itag2,imon2,ijah2,istu2,imin2,isek2,p(ianz)
!                   Format (2(i2,x),(i4,x),3(i2,x),a20)
!
!     Beispieldatensatz:
!       Kommentartext
!       10     198001010000   0  0.      0.
!       grap
!	28.03.1997 00:00:00	7
!	28.03.1997 00:00:00	0
!	28.03.1997 00:00:00	1.3
!	28.03.1997 00:00:00	0.1


!***************************************EIN-/AUSGABE********************************************
!JH 22.06.2004
!     Eingabe:
!		namfn	Filename der Niederschlagsdatei
!		naja,namo,natg,nast,namn	Anfangsdatum, naja:xxxx
!		neja,nemo,netg,nest,nemn	Enddatum, naja:xxxx
!
!     Ausgabe:
!		p	Niederschlagswerte [mm]
!		idif	Anzahl der Werte

!******************************************VARIABLEN********************************************
        USE generic_LOGGER
        USE Units
        INCLUDE 'include\param.cmn'
        INTEGER maxchar
	parameter (idim2=5000)
	parameter (maxchar=120)

	character*(maxchar)	namfn
	character*80	text,textkenn
	character*20	text20
	integer naja,namo,natg,nast,namn,                &
               neja,nemo,netg,nest,nemn,ntage,                 &
               kzif,idif,                                       &
    	       itag1,imon1,ijah1,istu1,imin1,             &
    	       itag2,imon2,ijah2,istu2,imin2,isek2

	integer	ianz
	real	fl

	real	t(idim2),p(idim2)
        REAL    dt


        idif=0
	kzif=0
	ianz=0
	sum1=0.
        call u1null(t,idim2)

!******************************************ANWEISUNGEN******************************************
!
!SK 25.07.2003  Bei Tageswerten wird Tageswert von 0.00 Uhr an bestimmt.
!

!JH 22.06.2004  Ergaenzung: Die Werte nast,namn,nest,nemn müssen zu 0 gesetzt werden, falls sie
!               dies nicht sind, nicht wenn sie 7 sind.
!               vorher: if(nast.ne.7) then, if(nest.ne.7) then
!               Da die Routine nur für die Langzeitsimulation aufgerufen wird, ist eine weitere
!               IF-Schleife, die dies abfragt, nicht notwendig.


!	if(abs(dt-24.).lt.0.5) then
	   if(nast.ne.0) then
	      	nast=0
	      	namn=0
	   endif
	   if(nest.ne.0) then
                ntage = 1
		call cdatum(netg,nemo,neja,ntage)
		nest=0
		nemn=0
	   endif
!	endif



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der niederschlagswerte aus datenfile
!c

	nfil=ju0gfu()
	open (nfil,iostat=ierr,err=999,status='old',file=namfn)

6600	read(nfil,'(a)',end=6001) text
	if(text(1:1).eq.'$' .or. text(1:1).eq.'/'.or. text(1:1).eq.'x'.or.text(1:1).eq.'X') goto 6600
	read(nfil,6100,end=6001) ijah1,imon1,itag1,istu1,imin1,ianz,fl
	read(nfil,'(a)') textkenn

	imin1=0

! warnung: tstart mus noch berechnet werden
	nstart=ntdif(itag1,imon1,ijah1,natg,namo,naja)
	tstart=nstart*24.+istu1+imin1/60.-nast-namn/60.
!JH 22.06.2004 Die Tagesanzahl der einzulesenden Niederschlaege muss um 1 erhöht werden, da der
!              Simulationszeitraum sich bis 24 h und nicht bis 0 h erstreckt.
	nend=ntdif(natg,namo,naja,netg,nemo,neja) + 1
	tend=nend*24.-nast-namn/60.+nest+nemn/60.

	if(fl.gt.1.and.tstart.ge.fl) goto 6600

!JH  15.06.2004 Da ausschließlich das grap-Format unterstütz wird, erfolgt bei Eingabe der anderen 'alten'
!JH             möglichen Eingabeformate die Ausgabe einer Fehlermeldung!

	ikziff=2
	if(textkenn(1:4).eq.'grap'.or.textkenn(1:3).eq.'EX2'.or.textkenn(1:3).eq.'ex2') ikziff=1
	if(textkenn(1:4).eq.'na-1') ikziff=2
	if(textkenn(1:3).eq.'nas') ikziff=2
	if(textkenn(1:4).eq.'weih') ikziff=2
	if(textkenn(1:4).eq.'wei3') ikziff=2
	if(textkenn(1:1).eq.'*') ikziff=2
	if(textkenn(1:3).eq.'fam') ikziff=2
	if(textkenn(1:3).eq.'bce') ikziff=2

	goto (3001,3000)ikziff

3000	continue


        call writeLogString(6,'Keine gueltige Formatangabe in Niederschlagdatenfile! Vorgesehenes Format: grap', &
             & 'Invalid format specification in precipitation data file, intended format: grap!',namfn,'','','', '', 'inp_nied_lz' )

	write (nerr,421) namfn

	goto 6001

437		continue

         IF(t(ianz)+23.99.ge.tend) then
        call writeLogIntRealReal(6, 'Zeitraum mit Niederschlagsdaten kuerzer als Simulationszeitraum!', &
             & 'Period in precipitation data is shorter than simulation period','','',0,'t(ianz-1)',t(ianz-1),&
             & 'tend',tend,'inp_nied_lz')
         END if

		write(nerr,411)t(ianz-1),tend
!c		ianz=ianz+1
		t(ianz)=tend
		p(ianz)=0.0
		continue
		p(1)=0.0
	goto 6001

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	textkenn='grap'
!c	elseif(textkenn(1:4).eq.'grap') then

3001	continue
468	ianz=ianz+1
	if(ianz.ge.idim2) goto 997

	read(nfil,419,end=437) itag2,imon2,ijah2,istu2,imin2,isek2,text20
	read(text20,*) p(ianz)

	n2=NTDIF(natg,namo,naja,itag2,imon2,ijah2)

	t(ianz) = n2*24.0 - nast - namn/60.0                          &
     	   	  + istu2 + imin2/60.0 + isek2/3600.0

	if(kzif.eq.2 .or. kzif.eq.0 .or. kzif.eq.3) then
	   if(t(ianz).lt.0.0 .and. ianz.ge.1 )  then
	      t(ianz-1)=t(ianz)
	      ianz=ianz-1
	      goto 468
	   endif

	elseif(t(ianz).lt.t(ianz-1) .and.ianz.gt.1)then
           write(*,410)
	   write(*,406)itag2,imon2,ijah2,istu2,imin2,isek2

	endif

!JH 	Messausfall mit -777 in Dateien kennzeichnen.           .
	if(abs(p(ianz)+777.).le. 0.001) then
	   p(ianz)=0.
	endif

!JH 22.06.2004 Da nur ein Niederschlagswert mehr eingelesen werden muss, als tatsaechlich
!              benötigt wird, kann das Auslesen der Niederschlagswerte bei t(ianz) größer
!	       gleich tend enden.
!              vorher: t(ianz).gt.tend
	if(t(ianz).ge.tend) goto 469
	goto 468
469	continue
!ep 07.11.2003 Bei der Langzeitsimulation ist eine Verschiebung der Niederschlaege um einen
!              Tag zurück erforderlich, da der DWD die Tagesniederschlaege immer für den
!              Zeitraum von 7.30 Uhr eines Tages bis 7.30 Uhr des naechsten Tages misst. D.h.
!              der in der Grap-Datei am Tag X angegebene Niederschlag bezieht sich immer auf
!              den Tag davor.

        DO 602 m=1,ianz-1
           p(m)=P(m+1)
602     continue
!JH  22.06.2004 Ergaenzung zu EP 07.11.2003: Es ist nicht erforderlich, dass das Enddatum
!               aufgrund der Niederschlagsverschiebung um einen Tag zurück verschoben wird.
!       ntage=-1
!       call cdatum(netg,nemo,neja,ntage)

	goto 6001


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	Einlesen Niederschlagsdaten abgeschlossen


6001	continue
	if(ianz.gt.1.and.(tend-t(ianz)).gt.0.5) then

        call writeLogIntReal( 6, 'Niederschlagsdaten decken nicht den gesamten Simulationszeitraum ab!', &
             & 'Precipitation data is not available for the whole simulation period!','','',0,'tend-t(ianz)',&
             & tend-t(ianz), 'inp_nied_lz' )


           write(*,6051)tend-t(ianz)
	   write(nerr,6051)tend-t(ianz)
	endif

	close(nfil)


	if(ianz.eq.0) goto 998

	if(kzif.eq.2.or.kzif.eq.0.or.kzif.eq.3) then
           do 198 m1=2,ianz
	      if (t(m1).gt.0.) goto 199
198	   continue



          call writeLogString(7, 'Simulationszeitraum wird nicht von Niederschlagsdatenfile abgedeckt!',&
          & 'Simulation period is not covered in the precipitation data file',namfn,'','','','','inp_nied_lz')

           write(nerr,918)namfn
           call writeFooter()
	   stop
199	   continue
	endif

	if(m1.gt.2.) then
	   m1=m1-1
	   ianz=ianz-m1
	   do 196 m=1,ianz
	      t(m)=t(m+m1)
	      p(m)=p(m+m1)
196	   continue
	endif

	if(t(1).lt.0..and.kzif.ne.3) then
	   p(2)=t(2)/(t(2)-t(1))*p(2)
	   t(1)=0.
	endif

!JH 22.06.2004 Die Ausgabe der eingelesenen Niederschlaege wurde an das Ende der Datei verschoben,
!              da vorher auch die vor Beginn des Berechnungszeitraumes eingelesenen Werte bei dieser
!              Angabe mit berücksichtigt wurden. Dies führte zu unsinnigen Angaben der Anzahl
!              eingelesener Niederschlagswerte.
!              Da zunaechst ein Niederschlagswert mehr eingelesen werden musste (aufgrund der DWD
!              Verschiebung) wird die Anzahl um 1 reduziert.
        ianz = ianz - 1
	write(*,515)ianz


!JH 	Approximation bei Langzeitsimulation nicht nötig, da
!	Niederschlagsintervall = Simulationsintervall = 24h
!	kzif=1
!	tstart=0.
!	call n_approx(dt,tstart,tend,ianz_d,            &
!     		t,p_inp,0,idife,p_m,kzif)

	do 1001 i=1,ianz
		sum1=sum1+p(i)
1001	continue
	continue
	idif=ianz


	print *
	print*,'eingeles. Niederschlag: ', sum1,namfn

	write(nerr,9019)sum1,namfn



	return



!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

!SK/EP 28.02.03 Neues Format für Jahr 2000 Faehigkeit Jahr wird 4stellig angegeben (mit Jahrhundert)
6100	format (7x,i4,4i2,i4,4x,f8.0)

918	format('xxx simulationszeitraum wird nicht von niederschlags',        &
     		'datenfile abgedeckt!',/'xxx filename: ',a)

9019	format('xxx eingelesener Niederschlag: ',f8.1,' mm',/ &
		'xxx Datenfile:  ', a)


515	format(' Anzahl eingelesener Niederschlagswerte: ', i4)
6051	format(/'xxx warnung!',/                                      &
     	'xxx nicht fuer den ges. zeitraum der simulation sind',/        &
     	'xxx niederschlagsdaten im eingabefile vorhanden.',/            &
     	'xxx fehlender zeitraum (stunden): ',f8.2)

406	format (5(i2,1x),i2)
419	format (2(i2,1x),(i4,1x),3(i2,1x),a20)
410	format(/'xxx falscher zeitwert in eingabefile! datum: ')          
411	format(/,'xxx zeitraum mit niederschlagsdaten '/               &
     		'xxx kuerzer als simulationszeitraum!',/               &
     		'xxx letzter niederschlagswert (h): ',f8.1,/           &
     		'xxx ende simulationszeitraum (h):  ',f8.1)

421	format(/'xxx keine gueltige formatangabe in niederschlag-datenfile'/, &
     		'xxx vorgesehenes format: grap ( 3. zeile) '/,          &
     		'xxx filename datenfile: ',a)




999     write(nerr,909)namfn
        call writeLogString(7, 'Datenfile nicht gefunden!', 'File not found!',namfn,'','','','','inp_nied_lz')
 	write(*,909)namfn
	ierr=1
        call writeFooter()
	stop
!	return
909	format( 'xxx datenfile mit niederschlagsdaten nicht gefunden!',     &
     		'xxx filename: ',a)




997	write(nerr,907)
        call writeLogIntInt(7, 'Mehr als als max. moegliche Anzahl Niederschlagsdaten im Eingabeblock!',&
             & 'More than max. possible precipitation data in input block!','','',0,'idim2',idim2,'inp_nied_lz')

	write(*,907)
	ierr=1
        call writeFooter()
	stop
!	return
907	format( 'xxx Mehr als idim2 Niederschlagsdaten im Eingabeblock!',    &
     		'xxx Erhoehe Parameter idim2 in inp_nied_lz')


998 write(nerr,908)namfn
    call writeLogString(7, 'Keine Niederschlagsdaten innerhalb vorgegebenem Simulationsintervall gefunden!',&
            & 'No precipitation found within the simulation period',namfn,'','','','','inp_nied_lz')

	write(*,908)namfn
	ierr=1
	return
908	format('xxx keine niederschlag innerhalb vorgegebenem zeitintervall', &
     		'gefunden!',/'xxx filename: ',a)

	end
