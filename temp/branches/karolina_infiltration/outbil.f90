!     Last change:  JH   26 Aug 2004    5:14 pm

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

!c      WK: nur Umbenennung!
      subroutine outbil(iz,iend,dt,xbil,xbil2)
!c
!c   erstellt ausgabe fuer gesammtbilanz des einzugsgebietes
!c
!c   eingabe:    iz    jahreszyklus
!c                xbil  summenwerte der bilanzgrossen
!c
!c
USE Units
      real xbil(21),xbil2(21)
      integer  it
	integer iend,iz
	real	dt

	real	xend,fx,fy

        it = iz
      write(nress,'(///a)')'***************************************************************'
      write(nress,102)
102   format(/,' gesamtbilanz des einzugsgebietes: ')
	if(it.ge.1)then
		fx=xbil(15)
		fy=1000.*dt/(24.*iend)
		write(nress,'(/a,i3,//)')' jahreszyklus:', it

		write(nress,9001) xbil(15)+xbil(12),xbil(12),xbil(15),   &
                       xbil(13),xbil(14),                                &
                       xbil(1)*fy,xbil(1)/fx,                            &
                       xbil(18)*fy,xbil(18)/fx,                          &
                       xbil(19)*fy,xbil(19)/fx,                          &
                       xbil(2)*fy,xbil(2)/fx,                            &
                       xbil(3)*fy,xbil(3)/fx,                            &
                       (xbil(2)+xbil(3))*fy,(xbil(2)+xbil(3))/fx,        &
                       (xbil(i)*fy,xbil(i)/fx,i=4,9)
		write(nress,9003) xbil(16)*fy,xbil(16)/fx,                &
                       (xbil(i)*fy,xbil(i)/fx,i=20,21),                  &
                       xbil(17)*fy,xbil(17)/fx,                          &
                       xbil(10)*dt/(24.*iend)

	else if (it.le.-1) then
		it=-it
		write(nress,9002)it
		do 1001	i=1,10
			xbil2(i)=xbil2(i)/it
1001		continue
		do 1002	i=16,21
			xbil2(i)=xbil2(i)/it
1002		continue
		fx=xbil(15)
		xend=iend/it
		fy=1000.*dt/(24.*xend)
		write(nress,9001) xbil(15)+xbil(12),xbil(12),xbil(15),   &
                       xbil(13),xbil(14),                                &
                       xbil2(1)*fy,xbil2(1)/fx,                          &
                       xbil2(18)*fy,xbil2(18)/fx,                        &
                       xbil2(19)*fy,xbil2(19)/fx,                        &
                       xbil2(2)*fy,xbil2(2)/fx,                          &
                       xbil2(3)*fy,xbil2(3)/fx,                          &
                       (xbil2(2)+xbil2(3))*fy,(xbil2(2)+xbil2(3))/fx,    &
                       (xbil2(i)*fy,xbil2(i)/fx,i=4,9)

		write(nress,9003) xbil2(16)*fy,xbil2(16)/fx,             &
                       (xbil2(i)*fy,xbil2(i)/fx,i=20,21),                &
                       xbil2(17)*fy,xbil2(17)/fx,                        &
                       xbil2(10)*dt/(24.*xend)

	else
		write (nress,9010)

	endif

      return

9001	format(/' gesamtflaeche:                   ',f10.3,/            &
       ' aufgeteilt in:',/                                               &
       ' versiegelte Flaeche:             ',f10.3,' qkm'/                &
       ' natuerliche Flaeche:             ',f10.3,' qkm'/               &
       ' davon:',/                                                       &
       ' mit aktivem Grundwasserspeicher: ',f10.3,' qkm'/                &
       ' mit Entwaesserung zum Tiefen-GWS:',f10.3,' qkm'/               &
       ' Bilanz natuerliche Flaechen:         '/                        &
       ' Niederschlag:                    ',f10.3,' qbm/d',f10.3,' mm'/ &
       ' pot. Grasreferenzverd.:          ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' pot. Verd. n. Nutzungskorr.:     ',f10.3,' qbm/d',f10.3,' mm'/ &
       ' Interzeptionsverdunstung:        ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Verdunstung aus Bodenspeicher:   ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Verdunstung gesamt:              ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Overlandflow:                    ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Interflow:                       ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Perkolation:                     ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Aenderung Bodenspeicher:         ',f10.3,' qbm/d',f10.3,' mm'/ &
       ' Zufluss TGWS:                    ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Zufluss GWS:                     ',f10.3,' qbm/d',f10.3,' mm')
9003	format(' Grundwasserspeicher      ',/                             &
       ' Gesamtabfluss Basisabfluss:      ',f10.3,' qbm/d',f10.3,' mm'/   &
       ' Kapilarer Aufstieg aus GWS:      ',f10.3,' qbm/d',f10.3,' mm'/   &
       ' Entnahme GWS:                    ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Tiefengrundwasserspeicher:                    ',/                &
       ' Verlust TGWS:                    ',f10.3,' qbm/d',f10.3,' mm'/  &
       ' Abfluss versiegelte Flaechen:    ',f10.3,' qbm/d')

9002	format(' Mittelwerte der Berechnung bei ',i3,                 &
     		' Simulationszyklen')

9010	format(' Kein vollstaendiger jahreszyklus durchlaufen ',/     &
     		' Mittelwertberechnung sinnlos')
      end
