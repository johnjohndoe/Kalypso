!     Last change:  JH    9 Mar 2007    3:56 pm

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

	subroutine iso(ntg,dt,izsim,idif,izf,spk,f,peff,qa,dqa,suqd,   &
                          flaech,vsga,nquh)

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================

!***************************************BESCHREIBUNG********************************************

!***************************************EIN-/AUSGABE********************************************
!               izsim
!               idif
!               peff
!               qa
!               dqa
!               suqd
!               flaech
!               vsga
!               nquh

! Eingabe:
!               ntg             Nummer des Teilgebietes
!               dt              Simulationszeitschritt
!               izf	        Anzahl notwendiger Simulationszeitschritte
!                	       	zur Abarbeitung der ZFT
!               spk             Retentionskonstante
!               f               normierte Zeitflaechen auf Simulationsschritt interpoliert

! Ausgabe:
! nicht verwendet:

!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      implicit logical(a-z)
!c    include 'iso.doc'
!c    parameter (idim=460)
      include 'include\param.cmn'
!TODO: JH, implicit none!

      integer 		ntg,izsim,idif,izf,nquh,izsim1,ituh,i,i2
      real 		u(idim),t(idim),f(*),unull(idim),qa(*),peff(*)
      real 		dt,spk,dqa,suqd,flaech,vsga,umin,umax,qamax,qd,y,xi,h,suu,suuh,fnorm, &
                  	suqn,sumu
      real 		sumf

!c   variableninitialisierung
      sumf=0
      do 333 i=1,izf
       	sumf=sumf+f(i)
333   continue

      umax = 0.
      qamax = 0.
      suqd = 0.
      suqn = 0.
      qd = 0.
      ituh = 0
      suu = 0.
      fnorm = 0.
      suuh = 0.
      dqa = 0.
      call u1null(u,idim)
      call u1null(t,idim)
      call u1null(unull,idim)

      ituh = idif
      izsim1 = idif
      umin = 1.0e-06

!******************************************ANWEISUNGEN******************************************

!c   Berechnung der uebertragungsfunktion

      y = dt/10.
1010  if(ituh .gt. idim)then
         ituh = idim
      endif
      sumu = 0.
      do 1000 i=1,ituh
       u(i) = 0.
       xi = float(i-1)*dt-dt/20.
       do 2000 i2=1,10
        xi = xi+y
        h = -xi/spk
        if(h.gt.-43.75)then
           h= exp(h)
           if((alog10(1./spk)+alog10(h)).lt.-38.)then
              h = 0.
           else
              h = 1./spk * exp(-xi/spk)
           endif
        else
           h=0.
        endif
                
        u(i) = u(i)+h*y
2000   continue

!c   bestimmung des max. von u(i) fuer zeitschritt i

       if (u(i).gt.umax) then
          umax=u(i)
       end if

!c   abbruchkriterium fuer die berechnung der uebertragungsfunktion
       sumu = sumu + u(i)
       if (u(i).le.umin .and. sumu.gt. 0.99) goto 11

!c      zwischenausdruck u - werte
!c      write(*,100) u(i),i
!c100   format(1x,'wert u(i) = ',f12.3,' bei i = ',i2)
!c      call u0tnoa('weiter mit return')
!c      read(*,'(a)')

1000  continue
      i  = i-1

!c   falls mit dem vorgegebenem ituh das minimum von u noch
!c   nicht erreicht wurde, wird ituh verdoppelt und die
!c   1000der-schleife noch einmal durchlaufen

      if (ituh .lt. idim) then
         ituh = ituh * 2
         goto 1010
      endif

11    ituh = i

!ergaenzung aug.2000
      if (ituh.lt.1) then
	ituh = 1
      endif

      if(sumu.lt.0.95)then
         write(nerr,'(a)')'ISO: Warnung!  UH nicht korrekt'
         write(nerr,'(''ISO: Summe UH-Ordinaten = '',f12.4)')sumu
         write(nerr,110) u(ituh),ituh
         call writeLogIntRealReal(6,'Unit Hydrograph wird korrigiert,da Summe UH-Ordinaten &lt; 1 (Grenzkriterium Summe&lt;0.95).',&
              & 'Unit hydrograph is not correct. Sum of UH-Ordinates &lt; 1.','','Teilgebiet',ntg,'Summe',sumu,&
              & 'Retentionskonstante',spk,'iso')

110      format(/ 1X,'ISO: Endwert u(ituh) = ',f12.4,' bei ituh = ',i4)
      endif

!c   normierung der unit-hydrograph - ordinaten

      do 3000 i=1,ituh
       suu = suu + u(i)
3000  continue

      fnorm = 1.0 / suu

      do 4000 i=1,ituh
       u(i) = u(i) * fnorm
       suuh = suuh + u(i)
4000  continue
      write(nres,130) fnorm, suuh

130   format(/ 1x,'ISO: Normierungsfaktor   = ',f6.4,/                        &
      	     & 1x,'ISO: Summe normierter uh = ',f8.4)

!c      call u0tnoa('weiter mit return')
!c      read(*,'(a)')

!c      ausgabe des unit hydrograph auf den file quh
      if (nquh.gt.0) then
         write(nquh,7601)
         write(nquh,7602) spk
7602     format('  k: ',f8.2,'   n: 1')
7601     format(/' unit-hydrograph fuer oberflaechenabfluss':)
         write(nquh,'(10(f6.4,1x))') (u(i2),i2=1,ituh)
      endif


!c   faltung der systemfunktion in zwei schritten, zunaechst erfolgt
!c   ueberlagerung mit dem zeitflaechendiagramm anschlieszend mit dem
!c   effektivniederschlag

!c   um den nachlauf des letzten impulses ebenfalls zu erfassen, musz
!c   izsim1 um die anzahl zeitschritte der systemfunktion, bzw. auch
!c   der zeitflaechenfunktion erhoeht werden
!c   aus dimensionierungsgruenden wird izsim1 auf 460 zeitschritte
!c   begrenzt

      izsim1 = idif + ituh

      if (izsim1.gt.idim) then
	 izsim1 = idim
         write(nres,73) ntg
      end if

      do 5000 i=1,izsim1
       unull(i) = 0.

!c       erster faltungsvorgang

       do 6000 i2=1,i
        unull(i) = unull(i) + u(i-i2+1) * f(i2)
6000   continue
5000  continue

!c      ausgabe des unit hydrograph auf den file quh
      if (nquh.gt.0) then
         write(nquh,7603)
7603     format(/' unit-hydrograph nach faltung mit zeitflaechenfktn:')
         write(nquh,'(10(f6.4,1x))') (unull(i2),i2=1,izsim1)
      endif


!c     zweiter faltungsvorgang

      izsim1 = izsim1 + izf

      if (izsim1.gt.idim) then
	 izsim1 = idim
      end if

      if (izsim1.gt.izsim) then
         izsim = izsim1
      end if

      do 7000 i=1,izsim1
       t(i) = float(i-1) * dt
       qd = 0.
       do 8000 i2=1,i
        qd = qd + unull(i-i2+1) * peff(i2) * flaech / 3.6
8000   continue
!c           do 8000 i2=1,i
!c                qd = qd + unull(i-i2+1) * peff(i2) * vsga / 3.6
!c8000       continue

!c        write(*,140) qd,i
!c140     format(1x,'wert qd = ',f20.3,' bei i = ',i2/)
!c        call u0tnoa('weiter mit return')
!c        read(*,'(a)')


       qa(i) = qd


!c       bestimmung des max. von qa(i) bei zeitschritt i (i*dt [h bzw. d])

       if (qa(i).gt.qamax) then
          qamax = qa(i)
       end if

!c       summe des direktabflusses (fuelle in m^3)
       if(i.gt.1)then
	  suqd = suqd + (qa(i-1)+qa(i)) * dt * 3600./2.
	  suqn = suqn + (peff(i) + peff(i-1))*flaech*dt*1.e+03/2.
       endif

!c        write(*,150) suqd,i
!c150     format(1x,'summe qd = ',f20.3,' [m^3] bei i = ',i2/)
!c        call u0tnoa('weiter mit return')
!c        read(*,'(a)')

7000  continue

!c   dauer der abfluszwelle dqa (echtzeit)
      write(nres,'(a)') 'fuellenberechnung in iso' 
      write(nres,'(a,f12.3)') 'abflusz =      ',suqd
      write(nres,'(a,f12.3)') 'niederschlag = ',suqn
      write(nres,'(a,f12.3)') 'flaeche zft  = ',sumf
      write(nres,'(a,f12.3)') 'flaeche eing.= ',flaech

      dqa = t(izsim)


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
73    format(/ 1X, 'Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.'/	&
             & 1X, 'Sie wird auf 460 begrenzt !  gebiet : ',i5)

      return
      end

