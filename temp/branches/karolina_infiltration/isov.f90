!     Last change:  JH    9 Mar 2007    2:21 pm

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

      subroutine isov(dt,izsim,idif,ret4,ret2,ret3,ns1,ns2,       &
     		ns3,betaa,betab,                            &
     		fvsum,peff,qavs,suqdk,ntg)


!**************************************ÄNDERUNGEN***********************************************
!     Date      Programmer      	Description of change
!     ====      ==========      	=====================

!***************************************BESCHREIBUNG********************************************
! Diese Methode berechnet den Unit-Hydrograph eine Speicherkaskade. Es besteht die Möglichkeit 
! den Ordinaten des UH aus höchsten drei parallen Speicherkaskaden zu ermitteln. Die Summe der
! Aufteilungsfaktoren muss somit immer 1 ergeben. Das bedeutet betaa + betab <= 1 sein. Die Differenz
! betaa-betab ist der Aufteilungsfaktor der dritten Speicherkaskade.
!***************************************EIN-/AUSGABE********************************************
!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      implicit logical(a-z)
!c    include 'isov.doc'
!c    parameter (idim=460)
      include 'include\param.cmn'


!c    include 'include/isov.cmn'

      real	vd

      integer	izsim,izsim1,ituh,ikmax,iqkmax,idif,i,i2,ntg

      real	uk(idim),t(idim),qavs(*),peff(*)
      real	ukmin,ukmax,qkmax,qdk,suqdk,y,xi,dt,        &
          	h,suuk,suuhk,fnormk,fvsum,dqk,h1,h2,h3,     &
          	suqn,sumu,ret4,ret2,ret3,betaa,betab,       &
          	hydrograph,ns1,ns2,ns3

!c    logical 	iflag

!c    variableninitialisierung

!c    call under0(iflag)
      ukmax 	= 0.
      qkmax 	= 0.
      suqdk 	= 0.
      suqn  	= 0.
      ituh 	= 0
      ikmax 	= 0
      iqkmax	= 0
      suuk 	= 0.
      fnormk 	= 0.
      suuhk 	= 0.
      dqk 	= 0.
      call u1null(uk,idim)
      call u1null(t,idim)


!******************************************ANWEISUNGEN******************************************
!cRS      ituh = idif
! UH wird ungünstig veraendert, wenn sehr hohe Retentionskonstanten
! Hierdurch kann das Ergebnis von Isov sehr stark von der Dauer der Simu.
! lation (idif) abhaengig werden. Idif+ituh immer gleich idim. Das bedeutet,
! wenn idif klein ist,wird ituh groß und umgekehrt. Bei großer Retentions-
! konstante wird der UH nicht vollstaendig im Zeitraum von ituh liegen, d.h.
! der UH wird mit einem Korrekturfaktor multipliziert, so daß nach der
! Korrektur der UH auf den Wert 1.0 normiert ist. Wenn ituh kurz ausfaellt,
! aufgrund eines langen Simulationszeitraumes, dann wird die Korrektur
! größer ausfallen, als bei kleinem Simulationszeitraum. Hierdurch entsteht
! eine größere Stauchung des UH bei langem Simulationszeitraum. Dies für
! zu dem paradoxen Ergebnis, daß bei langem Simulationszeitraum die
! durch ISOV gefaltete Ganglinie schneller ablaeuft als bei kurzem
! Simulationszeitraum.
! Zur Vermeidung dieses Paradoxom wird ituh= idim gesetzt. In diesem Fall
! bleibt bei hohen Retentionskonstanten immer noch ein Einfluß der
! Simulationszeit bestehen, allerdings wird der Widerspruch vermieden.
! Die Änderung von RS wird rückgaengig gemacht.
! Erik Pasche 05.06.03


!ep   ituh = idim-idif
      ituh = idim

      izsim1 = idif
      ukmin = 1.0e-06

      write (nres,1009) ret4,ret2,ret3
      write(nres,1008) ns1,ns2,ns3
      write(nres,1007) betaa,betab
1009  format('ISOV: Retentionskonstanten:',3(f6.1,4x))
1008  format('ISOV: Speicheranzahl:      ',3(f6.1,4x))
1007  format('ISOV: Aufteilngsfaktor:    ',2(f6.2,4x))

!c    berechnung der uebertragungsfunktion
!c
!c    unit hydrograph aus charakteristischen systemwerten,
!c    ordinaten u(i) [1/dt h], i=dt [h]

      y = dt/5.
!cp 1010  if (ituh.gt.(idim-idif)) then
1010  if (ituh.gt.(idim)) then
         ituh = idim
      end if
      sumu = 0.
      do 1000 i=1,ituh
       uk(i) = 0.
       xi = float(i-1)*dt-dt/10.
       do 2000 i2=1,5
        xi = xi+y
        h1 = hydrograph(xi,ns1,ret4)
        h2 = hydrograph(xi,ns2,ret2)
        h3 = hydrograph(xi,ns3,ret3)
        h  = betaa*h1 + betab*h2 + (1.-betaa-betab)*h3
        uk(i) = uk(i)+h*y
2000   continue

!c   bestimmung des max. von u(i) fuer zeitschritt i

       sumu = sumu + uk(i)
       if (uk(i).gt.ukmax) then
          ukmax=uk(i)
       end if
!c     write(nres,'(f8.4,3x,f8.4)')uk(i),sumu

!c   abbruchkriterium fuer die berechnung der uebertragungsfunktion


!c     ukmin  = ukmin/100.

       if (uk(i).le.ukmin .and. sumu .gt. 0.8) goto 11

!c     zwischenausdruck uk - werte
!c     write(*,100) uk(i),i
!c100  format(1x,' wert uk(i) = ',f12.3,' bei i = ',i3)
!c     call u0tnoa('weiter mit return')
!c     read(*,'(a)')

1000  continue
      i = i-1

!ergaenzung aug.2000
      if (ituh.lt.1) then
         ituh = 1
      endif

!c   falls mit dem vorgegebenem ituh das minimum von uk noch
!c   nicht erreicht wurde, wird ituh verdoppelt und die
!c   1000der-schleife noch einmal durchlaufen

!cRS  if (ituh.lt.idim) then
!ep   if (ituh.lt.(idim-idif)) then

      if (ituh.lt.(idim)) then
         ituh = ituh * 2
         goto 1010
      end if

11    ituh = i

!ergaenzung aug.2000
      if (ituh.lt.1) then
	 ituh = 1
      endif

!c   aenderung (6.6.90):
!c   bei einer langzeitsimulation musz gegebenenfalls die retentions-
!c   konstante der versiegelten flaechen (retvs<1) erhoeht werden,
!c   damit die uh-ordinaten uhmin ueberschreiten

      if (ret4.lt.1.0.and.dt.gt.23.) then
         if (ituh.eq.1.and.uk(1).lt.1.e-06) then
            ret4 = ret4 + 0.1
            ret2 = 5. * ret4
            ituh = idif
            goto 1010
         end if
      end if

!c   normierung der unit-hydrograph - ordinaten

      do 3000 i=1,ituh
       suuk = suuk + uk(i)
3000  continue

      if(suuk.lt. 0.95)then
         write(nerr,'(a)')'ISOV: Warnung! UH nicht korrekt.'
         write(nerr,120) suuk
         call writeLogIntRealReal(6,'Unit Hydrograph wird korrigiert,da Summe UH-Ordinaten &lt; 1 (Grenzkriterium Summe&lt;0.95).',&
              & 'Unit hydrograph is not correct. Sum of UH-Ordinates &lt; 1.','','Teilgebiet',ntg,'Summe',suuk,&
              & 'Retentionskonstante',ret4,'isov')

120      format(1x,'ISOV: Summe der UH-Ordinaten = ',f8.6)

         write(nerr,110) uk(ituh),ituh

110      format(1x,'ISOV: Wert uh(ituh) = ',f12.4,' bei ituh = ',i4)
      endif
      if(abs(suuk).lt.1.e-05)then
         ituh = 1
         uk(1) = 1.0
         suuk = 1.0
      endif

      fnormk = 1.0 / suuk
      suuhk = 0.
      do 4000 i=1,ituh
       uk(i) = uk(i) * fnormk
       suuhk = suuhk + uk(i)
4000  continue

      write(nres,130) fnormk, suuhk,ituh
130   format(1x,'ISOV: Normierungsfaktor = ',f10.2,/                     &
             1x,'ISOV: Summe normierter UH-Ordinaten = ',f10.4/               &
             1x,'ISOV: Anz. Zeitschritte UH = ',i4)

!c    call u0tnoa('weiter mit return')
!c    read(*,'(a)')


!c    faltung der systemfunktion

      izsim1 = idif + ituh

      if (izsim1.gt.idim) then
	 izsim1 = idim
         write (nres,73)
      end if

      if (izsim1.gt.izsim) then
         izsim = izsim1
      end if

      do 7000 i=1,izsim1
       t(i) = float(i-1) * dt
       qdk = 0.
       do 8000 i2=1,i
        ! mm/h -> m³/s: A[km²] * 1e6(km²->m²) * peff[mm/h]* 1e-3(mm->m) * 1/3600(h->s)= m³/s
        qdk = qdk + uk(i-i2+1) * peff(i2)*fvsum / 3.6
8000   continue

!c     write(*,140) qdk,i
!c140  format(1x,' wert qdk = ',f20.3,' bei i = ',i2/)
!c     call u0tnoa('weiter mit return')
!c     read(*,'(a)')

       qavs(i) = qdk

!c     bestimmung des max. von qavs(i) bei zeitschritt i (i*dt [h bzw. d])

       if (qavs(i).gt.qkmax) then
          qkmax = qavs(i)
       end if

!c     summe des direktabflusses (fuelle in m^3)
       if(i.gt.1)then
	  suqdk = suqdk + (qavs(i)+qavs(i-1)) * dt * 3600./2.
	  suqn = suqn + (peff(i) + peff(i-1))*fvsum*dt*1.e+03/2.
       endif

!c     write(*,150) suqdk,i
!c150  format(1x,'summe qdk = ',f20.3,' [m^3] bei i = ',i2/)
!c     call u0tnoa('weiter mit return')
!c     read(*,'(a)')

7000  continue

!c   dauer der abfluszwelle dqk (echtzeit)

      write(nres,'(a)')'ISOV:  fuellenberechnung'
      write(nres,'(a,f12.3)')'ISOV: Abflusz =      ',suqdk
      write(nres,'(a,f12.3)')'ISOV: Niederschlag = ',suqn


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

!c ausdruck der charakteristischen systemwerte und der scheitelabfluszcharakteristika

!c      write(nres,160) spkk1,ikmax,ukmax,iqkmax,qkmax,suqdk
160   format (3x,'speicherkonstante  spkk1 = ',f8.3/                   &
      3x,'anlaufzeit des unit-hydrograph  ikmax = ',i3,' * dt '/        &
      3x,'maximum-ordinaten des uhk  ukmax = ',f12.3,' [1/dt]'/         &
      3x,'zeitpunkt wellenscheitel  iqkmax = ',i4,' * dt '/             &
      3x,'scheitelabflusz qkmax  = ',f12.3,' [m^3/s]'/                  &
      3x,'fuelle von qdk (suqdk)  = ',f12.3,' [m^3]'//)

73    format(' anzahl der zeitschritte in isov durch nachlauf zu',     &
      ' hoch geworden.'/' sie wird auf IDIM begrenzt !')

      return

3010  write (nerr,3011) betaa,betab

3011  format(' summe von ',f3.2,' und ',f3.2, ' darf nicht groeszer ', &
       'als 1 werden.'/'ueberpruefe ausgangswerte !')
      call writeFooter()
      stop

      end




!******************************************************************************************************
!*                                                                                                    *
!*                                    Funktion Hydrograph                                             *
!*                                                                                                    *
!******************************************************************************************************


      real function hydrograph (xi,nspk,ret)

      USE generic_LOGGER
      USE Units     
      real     ret,xi,hilf1,hilf2,fakul,quot,dum1,nspk
      integer  i,nspkm,ierr

      hydrograph = 0
      quot = 0.

      if (ret.gt.1.e-10) quot = -xi/ret
      fakul=1.0
      if(abs(nspk-int(nspk)).lt.0.00001) then
         if (nspk.gt.1.) then
            nspkm=int(nspk)-1
            do 101 i=1,nspkm
             fakul=fakul*i
101         continue
         endif
      else
         call gamma(nspk,fakul,ierr)
         if(ierr.gt.0) goto 9200
      endif

      if (nspk.eq.0 .or. ret.le.1.e-10) then
         hydrograph = 0
      else if (nspk.eq.1) then
         hydrograph = exp(quot)/ret
      else
         if(quot.gt.-43.75)then
            hilf1 = alog10(fakul)+alog10(ret)*alog10(nspk)
            if (hilf1.gt.36.) goto 3030
            hilf1 = (xi**(nspk-1.)/(fakul*ret**nspk))
            hilf2 = exp(quot)
            if(abs(hilf1).gt.1.e-37) then
               dum1 = alog10(hilf1) + alog10(hilf2)
            else
               dum1 = alog10(hilf2) -37.
            endif
            if(dum1.lt.-38)then
               hilf1=0
            else
               hilf1 = hilf1*hilf2
            endif
         else
            hilf1=0
         endif
         hydrograph = hilf1
      endif

      return

3030  write(nerr,3031)
      call writeLogIntReal(7,'Speicheranzahl und Retentionskonstante zu gross gewaehlt. Programm kann Wert nicht verarbeiten!', &
           &'Number of storage & retention constants selected are too large. Program cannot process value.',&
           &'','',0,'hilf1',hilf1,'isov' )

3031  format(' Speicheranzahl und Retentionskonstante zu gross ',       &
        'gewaehlt.'/' Programm kann Wert nicht verarbeiten!')
      call writeFooter()
      stop

9200  write(nerr,9201) nspk
      call writeLogIntRealReal(7,'Fehler bei Berechnung der Gammafunktion fuer die Systemfunktion im Linearspeicher.', &
      & 'Error calculating the gamma function for the system function in the linear reservoir.','','',0,'nspk',nspk,'fakul',&
      & fakul,'isov')

9201  format(' Fehler bei Berechnung der Gammafunktion fuer die ',      &
         'Systemfunktion im Linearspeicher.'/,                          &
         ' gewaehlte Speicheranzahl: ',f7.2)
      call writeFooter()
      stop

      end
