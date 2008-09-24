!     Last change:  JH    2 Jan 2007    5:24 pm
	subroutine n_approx(dt,tstart1,tend,ianz1,vek1,vek2,idif,idife,vek3,kzif)

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

!c
!c	program  kont. nieders. (NASIM-XY-format) ein
!c		mit unterschiedlichen zeitschritten
!c	berechnet: niederschlaege zu konstanten zeitschritten
!c
!c	eingabe:	vek1	zeitvektor
!c			vek2	wertevektor
!c				(abs. Menge(mm) oder intensitaet(mm/h)
!c			ianz	anzahl der werte
!c			idif+1  erster wert fuer ausgabefeld
!c			dt	zeitschritt
!c
!c	ausgabe:	vek3	wertevektor, konst. zeitschritt
!c				(abs. Menge(mm) oder intensitaet(mm/h)
!c			idife    letzter wert fuer ausgabefeld
!c
!c
!c	kzif:	Kennziffer	=1:	werte in vek2 werden ueber
!c					zeitschritt dt aufaddiert
!c					(niederschlag)
!c				=2:	werte in vek2 werden ueber
!c					zeitschritt dt gemittelt
!c					(niederschlagsenergie)
!c				=3:	werte fuer vek2 wird fuer
!c					zeitpunkt dt bestimmt
!c					(= wert am ende d.zeitintervalls)
!c					(summe niederschlagsenergie)
        USE generic_LOGGER
      USE Units        
	include 'include\param.cmn'
!c	parameter (idim2=5000)


	real	dt
	integer idif,ianz

	real	steig,wert,xalt,tdif,talt,tdelta,xdelta,sumx
	real	vek3(*),vek1(*),vek2(*)

	logical	nied

	ianz=ianz1
	talt=0.
	if(kzif.eq.1) then
		nied=.true.
		xalt=0.
	elseif (kzif.eq.3) then
		nied=.true.
		xalt=vek2(1)
	else
		nied=.false.
		xalt=vek2(1)
		if(vek2(ianz).lt.tend) then
			ianz=ianz+1
			vek2(ianz)=tend
			vek2(ianz)=vek2(ianz-1)
		endif
	endif


	sumx=0.
	i2=idif+1
	tstart=tstart1
	

!cccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	interpolation der eingelesenen wert auf
!c	das aktuelle zeitintervall
!c

	do 333 m=1,ianz

	tdif=vek1(m)-tstart
!c	if(m.gt.1.and.nied) then
!c		wert=vek2(m)-vek2(m-1)
!c	else
!c		wert=vek2(m)
!c	endif
	wert=vek2(m)
	if(m.eq.1 .and. abs(wert).lt.0.000001 .and. kzif.eq.2)then
		wert=vek2(2)
	endif
334	continue
	if((tstart+dt).ge.tend.and.(tstart+tdif).ge.tend) then
		if(nied) then
			steig=wert/(vek1(m)-tstart-talt)
			xdelta=steig*(tend-tstart-talt)
			sumx=sumx+xdelta
			vek3(i2)=sumx
		else
			steig=(wert-xalt)/(tdif-talt)
			tdelta=(dt-talt)
			xdelta=xalt+steig*tdelta
!c			sumx=sumx+(xdelta+xalt)*.5
			sumx=sumx+tdelta*(xdelta+xalt)*.5
			vek3(i2)=sumx/dt
!c     tdelta noch ueberlegen
		endif
		goto 6002
	endif		

			
!c	eingelesener wert liegt ausserhalb aktuellen zeitintervalls
!c	naechster zeitschritt
		if (tdif.le.0.) then
			xalt=wert
			talt=tdif
			goto 333
		endif

!c	alter wert liegt ausserhalb und neuer wert liegt innerhalb
!c	des 1. zeitschrittes
		if(talt.lt.0) then
			if(nied) then
				steig=wert/(-talt+tdif)
				wert=wert-(-talt*steig)
			else
				steig=(wert-xalt)/(tdif-talt)
				xalt=xalt+steig*(-talt)
			endif
			talt=0.
		endif

		if (tdif.eq.dt) then
			if(nied) then
				sumx=sumx+wert
				vek3(i2)=sumx
			else
				sumx=sumx+(tdif-talt)*                &
     					(wert+xalt)*.5
				xalt=wert
				vek3(i2)=sumx/dt
			endif
			sumx=0.
			i2=i2+1
			if(i2.gt.idimt) goto 997
			talt=0.
			tstart=tstart+dt
			goto 333
		elseif (tdif.gt.dt) then
			if(nied) then
				steig=wert/(tdif-talt)
				tdelta=(dt-talt)
				xdelta=steig*tdelta
				sumx=sumx+xdelta
				wert=wert-xdelta
				vek3(i2)=sumx
			else
				steig=(wert-xalt)/(tdif-talt)
				tdelta=(dt-talt)
				xdelta=xalt+steig*tdelta
!c				sumx=sumx+(xdelta+xalt)*.5
				sumx=sumx+tdelta*(xdelta+xalt)*.5
!c     tdelta noch ueberlegen
				xalt=xdelta
				vek3(i2)=sumx/dt
			endif
			sumx=0.
			i2=i2+1
			if(i2.gt.idimt) goto 997
			talt=0.
			tdif=tdif-dt
			tstart=tstart+dt
			goto 334
		endif
		if(nied) then
	        	sumx=sumx+wert
		else
	        	sumx=sumx+(tdif-talt)*(wert+xalt)*.5
		endif
		talt=tdif
		xalt=wert

		goto 333

!c	neuer niederschlagswert wird ausgewertet!
333	continue

!c	neuer n-datenblock wird eingelesen!

!cccccccccccccccccccccccccccccccc
!c
!c	einlesen der n.daten abgeschlossen

	if(nied) then
		vek3(i2)=sumx
	else
		vek3(i2)=sumx/dt
	endif

6002	continue

	idife=i2

	return




997	write(nerr,909)
909	format( ' Mehr als idimt Zeitreiehendaten in Ausgabeblock!',   &
     		' erhoehe parameter idimt in param.cmn')
        call writeLogIntInt(6,'Mehr als max. moegliche Daten in Zeitreihe vorhanden!',&
             & 'More than possible data in time series.','','',0,'idimt',idimt,'n_approx')
	return

	end
