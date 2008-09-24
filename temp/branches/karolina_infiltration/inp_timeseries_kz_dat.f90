!     Last change:  JH   15 Jun 2007    5:49 pm

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

	subroutine inp_timeseries_kz_dat(namfn,dt,kzif,naja,namo,natg,nast,namn,                  &
     		neja,nemo,netg,nest,nemn,tend,t,p,ianz)

!**************************************ÄNDERUNGEN***********************************************

!SK   ------------------------------------------------------------------------------------------
!SK   Änderung am 28.02.03 bis 04.03.03 Stephan Kraessig (SK), Prof. Pasche (EP)
!SK   ------------------------------------------------------------------------------------------
!SK   Neue Version ist Jahr 2000 faehig und kann bei Angabe von 1000001 als
!SK   Anfangsdatum in der *.konfig Datei für jeden gerechneten Zeitschritt
!SK   der Langzeitsimulation Anfangswerte für die Kurzzeitsimulation erzeugen.

!JH   ------------------------------------------------------------------------------------------
!JH   Änderung am 24.06.2004 Jessica Hübsch (JH)
!JH   ------------------------------------------------------------------------------------------
!JH   Da es eine eigenstaendige Subroutine zum Einlesen der Langzeitniederschlaege im grap-Format
!JH   gibt (inp_nied_lz, 23.06.2004), sind die Spezifikationen zum Langzeitniederschlag in
!JH   dieser Datei nicht mehr notwendig.
!JH   Da Ausschließlich das grap-Format verwendet werden soll, werden die anderen Formate
!JH   gelöscht, wobei beim Aufruf dieser Formate durch den Benutzer eine Fehlermeldung erscheint.

!***************************************BESCHREIBUNG********************************************
!JN 17.06.2007
!     In der Subroutine wird das Einlesen der Niederschlags- bzw. Pegeldaten für die Kurzzeitsimulation
!     behandelt.
!
!     Unterstuetztes Format:
!
!	  textkenn = 'grap'
!		format (2(i2,x),(i4,x),3(i2,x),a20)
!       werte	itag2,imon2,ijah2,istu2,imin2,isek2,p(ianz)
!
!     Beispieldatensatz:
!       Kommentartext
!       10     198001010000   0  0.      0.
!       grap
!	    28.03.1997 07:09:00	0
!	    28.03.1997 07:10:00	0.1
!	    28.03.1997 07:29:00	0
!	    28.03.1997 07:30:00	0
!***************************************EIN-/AUSGABE********************************************
!JH 24.06.2004
!     Eingabe:
!		namfn	Filename der Niederschlags- bzw. Pegeldatei
!		naja,namo,natg,nast,namn	Anfangsdatum, naja:xxxx
!		neja,nemo,netg,nest,nemn	Enddatum, naja:xxxx
!     Ausgabe:
!		t	Zeitintervall ti,ti+1 [h]
!		p      	Niederschlag zwischen ti und ti+1 [mm], bzw. Pegelwerte
!		idif   	Anzahl Zeitschritte

!******************************************VARIABLEN********************************************
     USE generic_LOGGER
      USE Units     
     IMPLICIT NONE
     include      'include/param.cmn'

     CHARACTER(LEN=120)		:: namfn
	 CHARACTER(LEN=80)	    :: textkenn
	 CHARACTER(LEN=20)	    :: text20

	 INTEGER                :: naja,namo,natg,nast,namn,  &
            		           neja,nemo,netg,nest,nemn,kzif, &
     		                   itag2,imon2,ijah2,istu2,imin2,isek2
	 INTEGER                :: ianz
	 REAL                   :: dt
	 REAL 	                :: t(idim_n),p(idim_n),tend
	 INTEGER                :: m1,m
	 INTEGER				:: nfil,ierr,nend,n2
	 INTEGER				:: ju0gfu,ntdif
	 

     CALL u1null(t,idim_n)
     CALL u1null(p,idim_n)
     m1 = 0
     m  = 0
!******************************************ANWEISUNGEN******************************************

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	Uebernahme der Niederschlagswerte aus Datenfile
!c

	 nfil=ju0gfu()
	 OPEN (nfil,iostat=ierr,err=999,status='old',file=namfn)

	 READ(nfil,'(a)',end=6001)
	 READ(nfil,'(a)',end=6001)
	 READ(nfil,'(a)') textkenn

	 nend = ntdif(natg,namo,naja,netg,nemo,neja)
	 tend = nend*24.-nast-namn/60.+nest+nemn/60.


	 if(.not.(textkenn(1:4).eq.'grap')) then
        call writeLogString(7,'Keine gueltiges Format im Datenfile. Moegliches Format ist grap',&
                & 'Invalid format specificacion im data file, intended format: grap',namfn,'','', &
                & '','','inp_nied_kz_dat' )
   	    write (nerr,421) namfn
	    goto 6001
	 else if(textkenn(1:4).eq.'grap') then

468		ianz=ianz+1
		if(ianz.ge.idim_n) goto 997

		read(nfil,419,end=437) itag2,imon2,ijah2,istu2,imin2,isek2,text20
		read(text20,*) p(ianz)

		n2=NTDIF(natg,namo,naja,itag2,imon2,ijah2)
		t(ianz) = n2*24.0 - nast - namn/60.0                          &
     			  + istu2 + imin2/60.0 + isek2/3600.0
        
  	    if((t(ianz) .lt. 0.0000001) .and. (ianz .gt. 1))  then
		    t(ianz-1)=t(ianz)
		    ianz=ianz-1
		    goto 468

		else if((t(ianz) .lt. t(ianz-1)) .and. (ianz .gt. 1))then
            call writeLogString(6,'Falsches Datum in Eingabefile - Datumsrücksprung!', &
                       & 'Wring date in file!',namfn,'','','','','inp_nied_kz_dat')
		endif

!JH Messausfall
		if(p(ianz) .eq. 999. .or. p(ianz).eq. 9999.) then
		  p(ianz)=0.
		  call writeLogString(6,'Messausfall in Zeitreihe vorhanden (Wert gleich 999 oder 9999)!', &
                       & 'failure in measured values of one time series (value equals 999 or 9999)',namfn,&
                       & '','','','','inp_nied_kz_dat')
		endif

		if(t(ianz).gt.tend) goto 6001
		goto 468
     end if
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	Einlesen Niederschlagsdaten abgeschlossen

437	 continue

     call writeLogIntRealReal(7,'Zeitraum mit Zeitreihendaten kuerzer als Simulationszeitraum!', &
              & 'Timeseries data period shorter than simulation period!',namfn,'',0,'Datenfilezeitschritte (t)',t(ianz-1),&
              & 'Simulationszeitschritte (tend)',tend,'inp_nied_kz_dat')

	 t(ianz)=tend
	 p(ianz)=0.0
	 p(1)=0.0
	 goto 6001

6001 continue
	 if((ianz .gt. 1 ).and. ((tend-t(ianz)) .gt. 0.5)) then
	
        call writeLogIntReal(6, 'Nicht fuer den gesamten Simulationszeitraum sind Daten im Eingabefile vorhanden.',&
                & 'For the total simulation period the data in the input file is not available',namfn,'',0,&
                & 'Fehlender Zeitraum:',tend-t(ianz),'inp_nied_kz_dat')
	    write(nerr,6051)tend-t(ianz)
	 endif
	 close(nfil)

	 write(nres,515)ianz

	 if(ianz.eq.0) goto 998

	 do 198 m1=2,ianz
	   if (t(m1).gt.0.) goto 199
198	 continue

     call writeLogString(7,'Simulationszeitraum wird nicht von Datenfile abgedeckt!',&
                & 'Simulation period is not covered in the data file',namfn,'','', &
                & '','','inp_nied_kz_dat')
     call writeFooter()
	 write(nerr,918)namfn
     stop
   
199	   continue

	 if(m1.gt.2.) then
	    m1=m1-1
	    ianz=ianz-m1
	   do 196 m=1,ianz
	      t(m)=t(m+m1)
	      p(m)=p(m+m1)
196	   continue
	 endif

	 if(t(1) .lt. 0.0000001 .and. kzif .ne. 2) then
	   p(2)=t(2)/(t(2)-t(1))*p(2)
	   t(1)=0.
	 endif

	return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

!SK/EP 28.02.03 Neues Format für Jahr 2000 Faehigkeit Jahr wird 4stellig angegeben (mit Jahrhundert)
!JN 14.06.07 Neues format ohne die ungenutzen Variablen dtc, fl ,ianz
419	format (2(i2,1x),(i4,1x),3(i2,1x),a20)

421	format(/' Keine gueltige Formatangabe in Datenfile.'/, &
     	  &	' Vorgesehenes Format: grap!'/,          &
     	  &	' Filename: ',a)
515	format(' Anzahl eingelesener Zeitreihenwerte: ', i4)

6051	format(/' Warnung!'/, & 
     	      & ' Nicht fuer den ges. Zeitraum der Simulation sind Daten im Eingabefile vorhanden.'/, &
     	      & ' Fehlender Zeitraum (Stunden): ',f8.2)

999	write(nerr,909)namfn
    call writeLogString(7,'Datenfile mit Zeitreiehendaten nicht gefunden!',&
                        & 'File with timeseries data is not found!',namfn,'','','','','inp_nied_kz_dat')
    call writeFooter()
	stop

909	format( /' Datenfile mit Zeitreiehendaten nicht gefunden!'/, &
     	   & ' Filename: ',a)

997	write(nerr,907)
    call writeLogIntInt(7,'Mehr als max. moegliche Zeitreihendaten im Eingabefile!',&
             & 'More than max. possible data in input block!','','',0, &
             & 'max. Anzahl Zeitreihendaten (idim_n)',idim_n,'inp_nied_kz_dat' )
    call writeFooter()
	stop
907	format( / ' Mehr als max. mögliche Zeitreihendaten im Eingabeblock!'/,&
     		& ' Erhoehe Parameter idim_n in param.cmn')

998 write(nerr,908)namfn
    write(*,908)namfn
    call writeLogString(7,'Keine Zeitreihendaten innerhalb vorgegebenem Zeitintervall gefunden!',&
             &'No timeseries data found within the given interval!',namfn,'','','','','inp_nied_kz_dat')
908	format(/ ' Keine Daten innerhalb vorgegebenem Zeitintervall gefunden!',/' Filename: ',a)
	ierr=1
	return

918	format(/ ' Simulationszeitraum wird nicht von Zeitreihendatenfile abgedeckt!',/' Filename: ',a)

	end
