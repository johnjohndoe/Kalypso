!     Last change:  JH   20 Apr 2007    3:27 pm

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

      subroutine zufgang(nzu,jdata,jdate,idif,dt,q,    	&
                         namn,nast,natg,namo,naja20,	&
	                 nemn,nest,netg,nemo,neja20,    &
                         pns)

!**************************************ÄNDERUNGEN***********************************************

!JH 22.07.2004 Variable satz auf 120 Zeichen erweitert, da zuvor (mit 80 Zeichen)
!              das Einlesen einer Zuflussganglinie bei KZ-Simulation nicht möglich war.
!              80 Zeichen reichen für die Kopfzeile der Zuflussgangliniendatei bei kz
!              nicht aus.

!***************************************BESCHREIBUNG********************************************
!c     aus einem file wird eine zuflusswelle eingelesen und zu den werten
!c           im feld q addiert
!c
!***************************************EIN-/AUSGABE********************************************
!c      eingabe:    nzu character*80 mit
!c                    izuf1 (1:2)   bestimmt zufluss oder entnahme
!c                     = 1        zufluss, werte in m^3/sec
!c                     = 2        zufluss, werte in 1000 m^3/tag, werte umrechnen
!c                     = 3        entnahme, werte in m^3/sec
!c                     = 4        entnahme, werte in 1000 m^3/tag, werte umrechnen
!c                    iknot  (3:10)     mit knotennummer stelle 3-8
!c                                     filename der ganglinie 10-80
!c                  idata      integer   anfangsdatum, z.b. 81010100
!c                  idate         "      enddatum,     z.b. 81123123
!c                                        max.differenz: idim
!c                  idif      integer   anzahl der zeitschritte
!c                  dt        real      laenge eines zeitschrittes
!                   pns       character Niederschlags-/Berechnungsart (s: synthetisch, n:natürlich)
!c
!c      ausgabe:    q(idim)  real     ganglinie wird addiert
!c                  idif      real     fuer dt<23. kann sich anzahl der
!c                                     zeitschritte erhoehen



!******************************************VARIABLEN********************************************
        USE generic_LOGGER
        USE Units
        implicit logical (a-z)
!c      parameter(idim=460)

      include 'include\param.cmn'



      INTEGER maxchar
      parameter (maxchar=120)
      CHARACTER*(maxchar)   fnam,satz
      character*80 nzu,text
      real     q(idim),qgin(idim),dt,dtf,fak

      integer  knotz,idif,jdata,jdate,nu,                    &
              ju0gfu,iblock,ntdif,                                &
              izeile,idatsa,idatse,idif1,idif2,            &
              i1,ijaa,imoa,itaa,ista,idata,idate,imoe,itae,ijae,     &
              ndifa,ndife,ijasa,imosa,itasa,knot,izuf1
     CHARACTER (LEN=1), INTENT(IN) :: pns
     INTEGER :: nzuf ! kanalnummer der datei der Zuflussganglinie
     INTEGER :: ianz,kzif
	integer	ijlz,imlz,itlz
	real	xdifstu

	real p(idimt)
	integer	naja20,namo,natg,nast,namn,neja20,nemo,netg,nest,nemn,ierr
	real	faktn
 
	ierr=0

!******************************************ANWEISUNGEN******************************************

      read(nzu,'(i2,i8,a)')izuf1,knotz,fnam
      write(nres,1001) fnam,knotz,izuf1

      fak=1.
      if(izuf1.eq.2.or.izuf1.eq.4) fak=10./864.
      if(izuf1.eq.3.or.izuf1.eq.4) fak=-fak

	if (izuf1.eq.5) then
		p=0.
                !TODO: synthetischer Niederschlag - check if this works (jessica)
		if (pns.eq.'s') then
                   nzuf=ju0gfu()
                   open(nzuf,iostat=ierr,err=999,status='old',file=fnam)
6600               read(nzuf,'(a)',end=6001) text
     	           if(text(1:1).eq.'$' .or. text(1:1).eq.'/'.or. text(1:1).eq.'x'.or.text(1:1).eq.'X') goto 6600
	           read(nzuf,6100,end=6001) ianz
 	           read(nzuf,'(a)') text
                   readqzuf: DO ianz = 1,idif
	                   read(nzuf,419,end=6001) text                   ! bei synthetischem ereignis ist der Zeitpunkt(Datum) nicht relevant und wird daher nicht gelesen. es wird davon ausgegangen, dass die Zuflussdatei entsprechend aufgebaut ist, dass der erste Wert und das Intervall "passt".
        	           read(text,*) q(ianz)
                   END DO readqzuf
                   return
6001               call writeLogString(7,'Fehler beim Einlesen der Zuflussganglinie!', &
                         &'Error when reading inflow hydrograph!',fnam,'','','','','zufgang')
	           call writeFooter()
		   stop

!                   call writeLogString(7,'Die Angabe einer Zuflussganglinie bei einer synthetischen Berechnung ist nicht möglich.',&
!                         &'Inflow hydrograph not possible with synthetic calculation',fnam,'','','','','zufgang')
		else if(dt.gt.23.5) then
			call inp_nied_lz(fnam,dt,      			&
                                         naja20,namo,natg,nast,namn, 	&
                                         neja20,nemo,netg,nest,nemn,    &
				         p,idif,ierr)
		else
			faktn=1.
			kzif = 2
			call inp_timeseries_kz(fnam,dt,faktn,naja20,namo,natg,nast,namn,               &
		               neja20,nemo,netg,nest,nemn,p,idif,kzif)
		endif

		if(ierr.ge.1) then
                   call writeLogString(7,'Fehler beim Einlesen der Zuflussganglinie!', &
                         &'Error when reading inflow hydrograph!',fnam,'','','','','zufgang' )

		write(nerr,9200) fnam
                call writeFooter()
		stop
		endif
		q=p
		close(nu)
		return
	endif
9200	format(' Fehler beim Einlesen der Zuflussganglinie. ',/	&
			' Datenfile: ', a)

!c      print*,' izuf1 = ',izuf1
!c      print*,' fak   = ',fak


      nu = ju0gfu()
      open(unit=nu,err=8001,file=fnam,status='old')

      iblock = 0

      idata = int(jdata/100)
      ijaa = int(jdata/10**6)
      imoa = int((jdata-ijaa*10**6)/10**4)
      itaa = int((jdata-ijaa*10**6-imoa*10**4)/100)
      ista = int((jdata-ijaa*10**6-imoa*10**4-itaa*100))

      ijae = int(jdate/10**6)
      imoe = int((jdate-ijae*10**6)/10**4)
      itae = int((jdate-ijae*10**6-imoe*10**4)/100)
      idate = int(jdate/100)

      izeile = 3
      read(nu,*)
      read(nu,*)
      read(nu,*)
      izeile = izeile + 1
      read(nu,'(a)',err=301) satz
      read(satz,'(27x,i8,i4,10x,i8,i4,5x,10x,f6.3)',err=301) idatsa,nast,idatse,nest,dtf

	ijlz=0
      if(abs(dt-dtf).gt.10e-5) then
		continue
		if(abs(dtf-24.).lt.10e-5)  then
			ijlz=idatsa/10000
      			imlz = int((idatsa-ijlz*10**4)/10**2)
      			itlz = int(idatsa-ijlz*10**4-imlz*10**2)
		else
			goto 8002
		endif
	endif

!ccccccccccc kurzzeitsimulation: nur ein durchlauf
!c
	if(ijlz.gt.0) goto 7001
      if(dt.lt.23.) then
           if(idata.eq.-990000 .and. idate.eq.0) then
               write(nres,'(/a/)')                                     &
       ' uebernahme von abflussw. bei synthetischen niederschlaegen.'
           elseif(idata.ne.idatsa.or.ista.ne.nast) then
               goto 8003
           endif
      else
!cccccccccc langzeitsimulation: richtiger zeitraum muss ausgewaehlt werden
!c
           ijasa = int(idatsa/10**4)
           imosa = int((idatsa-ijasa*10**4)/100)
           itasa = int((idatsa-ijasa*10**4-imosa*100))

!c           print*,' idata: ', idata
!c           print*,' idatsa: ',idatsa
           if(idata.lt.idatsa.or.idate.gt.idatse) goto 8004
      endif

7001	continue
      
      izeile = izeile + 1
      read(nu,*)
      idif1=0
100   izeile = izeile + 1
      iblock = iblock + 1
      idif2=idif1
      read(nu,'(/,i8,8x,i8,/)',end=300,err=301)  knot,idif1
      izeile = izeile + idif1/10
      read(nu,'(10(f8.3,1x))',err=303) (qgin(i1),i1=1,idif1)

      if(knot.eq.knotz) then

	if(ijlz.gt.0) then
		if(ijlz.ne.ijaa) then
			ijlz=ijlz+1
			itlz=0
			imlz=0
			goto 100
		else
			ndifa=ntdif(itlz,imlz,ijlz,itaa,imoa,ijaa)+1
			xdifstu=ista
			do 7010 i1=1,idim
				xdifstu=xdifstu+dt
				if(xdifstu.gt.24.)then
					xdifstu=xdifstu-24.
					ndifa=ndifa+1
					if(ndifa.gt.idif1) ndifa=idif1
				endif
				q(i1)=fak*qgin(ndifa)
7010			continue
			goto 8000
		endif
	endif

!ccccccccccc kurzzeitsimulation: nur ein durchlauf
!c
          if(dt.lt.23.) then
              do 2003 i1=1,idif1
                  q(i1)=fak*qgin(i1)
2003          continue
!ccccccccccccccc
!c  geaendert wegen feldprobleme, 5.9.1999, R.S.
!c
!c              if(idif1.gt.idif) idif=idif1
              goto 8000
          else
!cccccccccc langzeitsimulation: richtiger zeitraum muss ausgewaehlt werden
!c
          call cdatum(itasa,imosa,ijasa,idif2)
          ndifa=ntdif(itasa,imosa,ijasa,itaa,imoa,ijaa)
          ndife=ntdif(itasa,imosa,ijasa,itae,imoe,ijae)+1
!c          print*,'itasa,imosa,ijasa: ',itasa,imosa,ijasa
!c          print*,'itaa,imoa,ijaa:   ',itaa,imoa,ijaa
!c          print*,'itae,imoe,ijae:   ',itae,imoe,ijae
!c          print*,'ndifa: ',ndifa
!c          print*,'ndife: ',ndife
!c          print*,'idif1: ',idif1

          if (ndife.lt.0) goto 8000
          if(ndifa.gt.0) goto 100
          do 2004 i1=1,idif1-ndifa
             if(i1.gt.ndife) goto 8000
             q(i1)=fak*qgin(i1+ndifa)
2004      continue
          if((i1-1).ge.ndife) goto 8000
          endif
      endif

      goto 100

8000  close(nu)
      return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
999   call writeLogString(7,'Datenfile der Zuflussganglinie nicht gefunden!',&
           &'File not found!',fnam,'','','','','zufgang')
      call writeFooter()
      stop
1001  FORMAT(  1X,' Zuflussganglinie wird eingelesen aus File ',a,/           	   &
             & 1X,' Eingelesener Knoten: ',i5,/                                	   &
             & 1X,' Fall:                ',i5,/                                	   &
             & 1X,' (1+2 = Zufluss, 3+4 = Entnahme, 2+4 = Werte in 1000m^3/Tag)')

6100	format (19x,i4)

419	format (20x,a20)

8001  write(nerr,1801)fnam
      call writeLogString(7, 'File mit Zuflussganglinie konnte nicht geoeffnet werden!', &
      & 'File with the inflow hydrograph can not be opened!',fnam,'','','','','zufgang')

1801  format(' file mit zuflussganglinie konnte nicht geoeffnet werden!' &
           /' filename:',a)
      call writeFooter()
      stop



8002  write(nerr,1802)dt,dtf
      call writeLogIntRealReal( 7,'Zeitschritt der Simulation und der Zuflussganglinie sind unterschiedlich.',&
           & 'Interval length of Simulation and the inflow hydrograph are different.',&
           & '','',0,'Simulation (dt)',dt,'Ganglinie (dtf)',dtf,'zufgang')

1802  format(' intervallaengen sind unterschiedlich:'                    &
          /' intervallaenge der na-simulation:    ',f6.3                 &
          /' intervallaenge der zuflussganglinie: ',f6.3 )
      call writeFooter()
      stop

8003  write(nerr,1803)idata,idatsa,idate,idatse,ista,nast
1803  format('x anfangsdatum der zuflussganglinie und der na-simulation'/ &
       'x stimmen nicht ueberein!',/                                       &
       'x dies ist fuer eine kurzzeitsimulation notwendig!',/              &
       'x anfangsdatum na-simulation: ', i6/                               &
       'x anfangsdatum zufl.kennl.:   ', i6/                               &
       'x enddatum na-simulation:     ', i6/                             &
       'x enddatum zufl.kennl.:       ', i6/                             &
       'x jahreszyklus na-simulation: ', i6/                             &
       'x jahreszyklus zufl.kennl.:   ', i6)
      call writeLogIntInt(7,'Anfangsdatum der Zuflussganglinie und der Simulation stimmen nicht ueberein!',&
           &'Start date for inflow hydrograph and Simulation are different.','','',0,'idata',idata,'zufgang' )

      call writeFooter()
      stop

8004  write(nerr,1804)
1804  format(' zeitraum der na-simulation liegt ausserhalb des'/         &
       ' zeitraums der auflussganglinie!')
      call writeLogString(7,'Zeitraum der Simulation liegt ausserhalb des Zeitraums der Ganglinie!',&
      & 'RRM simulation period is outside of the hydrograph','','','','','','zufgang')

      call writeFooter()
      stop



300   write(nerr,1300) knotz
1300  format(' knoten ',i5,' wurde nicht im file der abflussganglinie ',  &
        ' gefunden'/,' oder'/,' fehler bei dem simulationszeitraum!')
      call writeLogIntInt(7,'Fuer einen Knoten liegt die Zuflussganglinie nicht vor oder Fehler im Simulationszeitraum.',&
           & 'A node is not in the discharge hydrograph file or mistake in simulation period.','','Knoten',knotz ,'',0,'zufgang')
      close(nu)
      call writeFooter()
      stop


301   write(nerr,1301) fnam,izeile,iblock
1301  format('fehler in ',a/,' zeile/block = ',i5,i5)
      call writeLogIntInt(7,'Fehler beim Lesen einer Datei.','Error while reading a file.',&
           & fnam,'',0,'Zeile (izeile)',izeile,'zufgang')

      call writeFooter()
      stop

303   write(nerr,1303) fnam,izeile-idif1/10,izeile,iblock
1303  format('fehler in ',a/,'zwischen zeile ',i4,' und zeile ',      &
            i4,' in block ',i4)
      call writeLogIntInt(7,'Fehler beim Lesen einer Datei.','Error while reading a file.',&
           & fnam,'',0,'Block (iblock)',iblock,'zufgang')

      call writeFooter()
      stop

      end



!c      function ntdif(tag1,monat1,jahr1,tag2,monat2,jahr2)

!c  *********************************************
!c  *                                           *
!c  *  ntdif rechnet die zeitdifferenz in tagen *
!c  *                                           *
!c  *********************************************

!c      integer tag1,monat1,jahr1,tag2,monat2,jahr2,ntdif,nt2000
!c      ntdif=nt2000(tag2,monat2,jahr2)-nt2000(tag1,monat1,jahr1)

!c      return
!c      end


!c      function nt2000(tag,monat,jah)

!c  ********************************************************
!c  *                                                      *
!c  *  nt2000 brechnet die zeitdifferenz in tagen zwischen *
!c  *  einem datum und dem  0.0.1900                       *
!c  *                                                      *
!c  ********************************************************

!c      integer tag,monat,jah,ntsm,nt2000,jahr
!c      dimension ntsm(12)
!c      data ntsm/0,31,59,90,120,151,181,212,243,273,304,334/

!c      jahr=jah
!c      nt2000=jahr*365+ntsm(monat)+tag
!c      nt2000=(jahr-1)/4+1+nt2000
!c      if (mod(jahr,4) .eq. 0 .and. monat .gt. 2) nt2000=nt2000+1

!c      return
!c      end
