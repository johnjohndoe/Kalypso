!     Last change:  JH   24 Apr 2007    1:35 pm

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
	subroutine inp_zft(ntg,namfzf,il,dt,ispk,      &
     		idif,fn,ianzn,pns)

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     ?		Wiebke Klauder (WK)	nur Umbenennung

!***************************************BESCHREIBUNG********************************************
! Einlesen der für das Teilgebiet angegebenen Zeitflaechenfunktion.
! Umrechnung der abflusswirksamen Flaechen auf die Zeitintervalle der Simulation.

!***************************************EIN-/AUSGABE********************************************
! Eingabe:
!               ntg             Nummer des Teilgebietes
!               namfzf          Pfad der Zeitflaechenfunktionsdatei
!               dt              Simulationszeitschritt
!               ispk            Gebietsbearbeitungsfall
!               idif            Anzahl der Simulationszeitschritte
!               pns             Niederschlagsart (synthetisch, natürlich)

! Ausgabe:
!               fn              normierte Zeitflaechen auf Simulationsschritt interpoliert
!               ianzn           Anzahl notwendiger Simulationszeitschritte
!                	       	zur Abarbeitung der ZFT

! nicht verwendet:
!               il              Laenge des Pfades der ZFT

!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      include 'include\param.cmn'

      real			fn(idim),p(idim),dt
      real			datc,datca,datuma,datume,dtzf

      integer			iart,ianzf,inum,il,ispk
      integer 			nzf,idif
      character*80		namfzf
      character*1 		pns

!******************************************ANWEISUNGEN******************************************
      inum = 0
      nzf = ju0gfu()



! Einlesen der *.zft
!--------------------
      open (nzf,iostat=ierr,err=9,file=namfzf,status='old')
222   read(nzf,*,end=9697,err=9696) inum                                ! TG Nummer
      iart=0
      read(nzf,*,err=9696) ianzf,dtzf,iart                              ! Zeitintervallanzahl, Intervall, Datenformat
      if(ianzf.gt. idim)then                                            ! Zu viel Intervalle in zft
         write(nerr,9002)inum,ianzf
         call writeLogIntInt(7,'Angabe der Zeitflaechenfunktion zu detailliert. Max. Anzahl an Intervallen ueberschritten.', &
              & 'Dimensioning of the time area function is too small. Max. number of intervals reached.',namfzf,'Teilgebiet',inum,&
              & 'Zeitintervalle',ianzf,'inp_zft')

9002     FORMAT (/ 1X,'Dimensionierung der Zeitflaechenfunktion zu klein'/			& !JH:gross?
                 & 1X,'Anzahl der angegebenen  Zeitintervalle fuer'/				&
                 & 1X,'Teilgebiet ',i5,': ',i5,'.'/						&
                 & 1X,'Korregieren Sie die Funktion oder erhoehen Sie den Parameter idim.')
         call writeFooter()
	 stop 1000
      endif

      if (iart.eq.2) then                                               ! Format 2
         do 2223 i=1,ianzf
          read(nzf,*) ptime,p(i)                                        ! Zeit, Flaeche [km²] oder normiert
2223     continue
!cccccccccc   angaben der flaechen in m**2 im projekt soest
      elseif(iart.eq.3) then                                            ! Format 3
         do 2224 i=1,ianzf
          read(nzf,*) p(i)                                              ! Flaeche [m²]
          p(i)=p(i) * .000001                                           ! Umrechnung Flaeche auf km²
2224     continue
      else                                                              ! Format ungleich 2 und 3
	 do 2225 i=1,ianzf
	  read(nzf,*) p(i)                                              ! Flaeche [km²] oder normiert
2225     continue
      endif

      if (inum.ne.ntg.and.inum.ne.999) goto 222                         ! Suche nach richtigem Teilgebiet
                                                                        ! 999 fuer Standardfunktion aller TG´s
      close (nzf)

!2.10.1990
! Mit dem "feltens-verfahren" (trala) erstellte Zeitflaechenfunktionen
! liefern bereits Werte in [km]  ==>  34er-Schleife ueberfluessig.

! evtl. Normierung der Flaeche und Fehlerbehandlung der Anzahl der ZFT-Zeitschritte
!----------------------------------------------------------------------------------
      sumzfl = 0.
      do 34 i=1,ianzf
       sumzfl = p(i)+sumzfl                                         	! Summe Teilflaechen
34    continue

      if(abs(sumzfl-1.).gt.0.01)then                                    ! Korrektur der Flaeche, falls nicht normiert
	 write(nerr,9401) sumzfl
     call writeLogIntReal( 6, 'Fehler in Zeitflaechenfunktion! Summe der Teilflaechen ungleich 1. Automatisierte Korrektur.', &
              & 'Error in time area function! The sum of the sub-areas is not equal 1. Automatic correction. ',namfzf,'Teilgebiet',&
              & ntg,'Summe',sumzfl, 'inp_zft' )

	 write(*,9401) sumzfl
	 fakzfl = 1./sumzfl
	 do 1034 i=1,ianzf
	  p(i) = p(i)*fakzfl
 1034	 continue
	 sumzfl=sumzfl*fakzfl
      endif
9401  format(/ 1X,'Fehler in Zeitflaechenfunktion!'/            &
             & 1X,'Summe der Teilflaechen ungleich 1.'/              &
             & 1X,'Berechnete Summe:   ',f12.4/            &
             & 1X,'Abweichung wird automatisch korregiert! ')

      ianzn = nint(dtzf*ianzf/dt + 0.5)                                 ! Anzahl notwendiger Simulationsschritte
      if(ianzn.gt.idim)then                                             ! zur Abarbeitung der ZFT
         write(nerr,1335) ntg,ianzn,idim
         call writeLogIntIntInt(6,&
               & 'Fliesszeiten in der Zeitflaechenfunktion ueberschreiten max. moeglichen Zeitraum der Simulation!',&
               & 'Travel time of the time area function exceeds maximum possible simulation time period.','','Teilgebiet',&
               & ntg,'ianzn',ianzn,'idim',idim,'inp_zft')


1335     format(/ 1X, 'Fliesszeiten in der Zeitflaechenfktn fuer Gebiet ',i5/ 		&
       		& 1X, 'ueberschreiten max. moeglichen Zeitraum der Simulation!'/  	&
       		& 1X, 'Fliesszeiten werden auf diesen Zeitraum beschraenkt!'/       	&
       		& 1X, 'Zeitschritte der Zeitflaechenfunktion: ',i4/                 	&
        	& 1X, 'max. moegliche Zeitschrittanzahl:      ',i4/                	&
       		& 1X, 'Gegebenenfalls groesseres Simulationsintervall dt waehlen.')
      endif
      ianzn = min0(ianzn,idim)                                          ! Begrenzung von ianzn durch idim (Kleinstwert)

!c   falls die anzahl der zeitschritte der n-a-simulation, bezogen auf
!c   den zeitraum der zeitflaechenfunktion, groeszer als die anzahl
!c   zeitschritte des simulationszeitraumes ist, musz das enddatum des
!c   simulationszeitraumes neu berechnet und idate zugewiesen werden.
!c   das urspruengliche enddatum wird in zwischengroeszen umgespeichert,
!c   da cdatum die variablen mit dem neuen datum ueberschreibt.
!c   fuer den fall einer kurzzeitsimulation, d.h. innerhalb eines jahres:

      if (pns.eq.'s'.or.dt.lt.23.5) then                                ! synth. Niederschlag oder Kurzzeitsim
	 if(idif.lt.ianzn)idif=ianzn
      endif
      if(ianzn.gt.idif) then
	 write(nerr,10000)ianzn,idif
      call writeLogIntIntInt(6,&
               & 'Fliesszeiten in der Zeitflaechenfunktion ueberschreiten Zeitraum der Simulation!',&
               & 'Travel time of the time area function exceeds simulation time period.',namfzf,'Teilgebiet',&
               & ntg,'ianzn',ianzn,'idif',idif,'inp_zft')

	 goto 123
10000	 format(/ 1X, '!!!!!!!!!!!!!!!!!!!!1!!!'/                        			&
                & 1X, 'Fehler in der Zeitflaechenfunktion !!!'/                          	&
                & 1X, 'Noch nicht beruecksichtigte Version.'/					&
                & 1X, 'Anzahl der benoetigten Simulationsschritte zur Abarbeitung der'/		&
                & 1X, 'Zeitflaechenfunktion (',i5,') > Anzahl Simulationsschritte (',i5,').')
      endif
!c      if (pns.ne.'s'.and.pns.ne.'s') then
!c       if (neja.eq.naja) then
!c          if (ianzn.gt.idif) then
!c               idifd = int(((ianzn-idif)*dt)+0.5)+1
!c               idift = (nest+idifd)/24
!c               call cdatum(netg,nemo,neja,idift)
!c               nest = idifd + nest - idift*24
!c345            idate = neja*10**6 + nemo*10**4 + netg*10**2 + nest
!c               idif = ntdif(natg,namo,naja,netg,nemo,neja) + 1
!c               idif = int(idif*24./dt - nast/dt - (24.-nest)/dt +0.5)
!c               if (idif.gt.idim)then
!c                  nest=nest-1
!c                 write(nres,'(a)')'warnung!!! rundungsfehler - '
!c                 write(nres,'(a)')'enddatum wird um eine stunde ',
!c     +                   'zurueckgesetzt'
!c                   goto 345
!c               endif
!c          end if
!c       end if
!c      end if

! Interpolation auf die Zeitschritte der NA-Simulation
!------------------------------------------------------

123   continue
      do 10 i=1,ianzn
       datc  = i*dt                                     ! Ende Zeitschritt Simulation
       datca = (i-1)*dt                                 ! Anfang Zeitschritt Simulation
       do 11 i2=1,ianzf
        datuma = (i2-1)*dtzf                            ! Anfang Zeitschritt ZFT
        datume = i2*dtzf                                ! Ende Zeitschritt ZFT

!c                    if (datc.gt.datuma.and.datc.le.datume) then
!c                           if (dt.gt.dtzf) then
!c					fn(i) = fn(i)+p(i2)*
!c     +                                   ((datc-datuma)/(datume-datuma))
!c                          else
!c					   fn(i) = fn(i)+p(i2)*dt/dtzf
!c                           end if
!ccccccccccccccccccc
!c ergaenzt, 18.5.1999, R.S.

        if (datume.ge.datc) then
           if (datuma.lt.datca) then
	      fn(i) = fn(i)+p(i2)*dt/dtzf
	   else
	      fn(i) = fn(i)+p(i2)*((datc-datuma)/dtzf)
	   endif
	   goto 10
        else if(datca.lt.datume.and.datca.gt.datuma) then
	   fn(i) = fn(i)+p(i2)*(datume-datca)/dtzf
        else if(datc.gt.datume.and.datca.le.datuma) then
           if (abs(fn(i)).le.1.e-04.and. i2.gt.1) then
	      fn(i) = p(i2-1)*(datuma-datca)/dtzf
           end if
	   fn(i) = fn(i)+p(i2)
        end if
11     continue
10    continue
      sumizl = 0.
      do 1035 i=1,ianzn
       sumizl=sumizl+fn(i)                                      ! Summe Flaeche von umgerechneter ZFT
1035  continue
!TODO: JH, warum dtzf<3???
      if(abs(sumizl-sumzfl)/sumzfl.gt.0.05.and.dtzf.lt.3.)then
	 write(nerr,9400)sumzfl,sumizl,dtzf,dt
	 write(*,9400)sumzfl,sumizl,dtzf,dt
     call writeLogIntRealReal(6, 'Fehler in den Teilflaechen der Zeitflaechenfunktion.','Error in area of the time area function.',&
           & namfzf,'',0,'sumzfl',sumzfl,'sumizl',sumizl,'inp_zft')

9400	 format(/ 1X, 'Fehler in den Teilflaechen der Zeitflaechenfunktion.'/	&
       		& 1X, 'Flaeche aus Zeitflaechenfunktion = ',f12.3/              &
       		& 1X, 'Flaeche nach Interpolation       = ',f12.3/              &
       		& 1X, 'Zeitschritt Zeitflaeche          = ',f10.3/              &
       		& 1X, 'Zeitschritt Rechnung             = ',f10.3/              &
       		& 1X, 'Fehler wird interpoliert! Kontrollieren sie die Zeitflaechenfunktion!')
	 sumizl = sumzfl/sumizl                                 ! Korrektur der Flaeche
	 do 1036 i=1,ianzn
          fn(i) = fn(i)*sumizl
1036	 continue
      endif

return
!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

9696  write(nerr,969)

969   FORMAT (/ 1X, 'Fehler beim Einlesen der Zeitflaechenfunktion')

      call writeLogString(7, 'Fehler beim Einlesen der Zeitflaechenfunktion.','Error reading the time area function.', &
           & namfzf,'','','','','inp_zft')
      call writeFooter()
      stop

9697  write(nerr,9900) ntg

9900  format(/ 1X,'Fuer das Teilgebiet ',i5,' existiert keine zeitflaechenfunktion.'/          		&
             & 1X,'Ueberpruefen sie die Zeitflaechenfunktion *.zft oder'/ 				&
             & 1X,'Kennziffer ispk in der Gebietsdatei zur Festlegung der Abflussverzoegerung.')

      call writeLogIntInt(7,'Fuer ein Teilgebiet existiert keine Zeitflaechenfunktion.', &
          & 'For a sub catchment does not exist any time area function',namfzf,'Teilgebiet',ntg,'',0,'inp_zft')
      

      close (nzf)
      call writeFooter()
      stop

9     write(nerr,9001) namfzf(1:il)
      call writeLogString( 7,'Datei konnte nicht geoeffnet werden. Ueberpruefen sie die Datei.', &
           & 'File could not be opened. Check the file.',namfzf(1:il) ,'','','','','inp_zft')

9001  format(/ 1X,'Datei mit Filename ',a/		&
             & 1X,'konnte nicht geoeffnet werden.'/    	&
             & 1X,'Ueberpruefen sie die Datei im directory ../inp.dat')

      call writeFooter()
      stop

      end
