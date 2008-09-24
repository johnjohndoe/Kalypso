!     Last change:  JH   26 Aug 2004    5:14 pm
      subroutine outbil_sp(iz,xbilsp)

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
!c   erstellt ausgabe fuer gesammtbilanz dr speicher
!c
!c   eingabe:     iz    jahreszyklus
!c                xbil  summenwerte der bilanzgrossen
!c
!c
      USE Units
      real xbilsp(20)


      write(nress,'(///a)')'*********************************************************'
      write(nress,102)
102   format(/,' gesamtbilanz der speicherberechnung: ')
	if(iz.eq.0) then
		write(nress,103)
103   format(/,' K E I N E  Gesamtbilanz: ',/                          &
     		' Bilanzierungszeitraum betraegt 0 Jahre.')

	 elseif (iz.le.-1) then
		iz=-iz
		write(nress,9002)iz
		do 1001	i=3,15
			xbilsp(i)=xbilsp(i)/iz
1001		continue

		write(nress,5000)xbilsp(1),xbilsp(2)
		write(nress,5001)xbilsp(3),xbilsp(4)
		if (abs(xbilsp(8)).gt.0.000001) write(nress,5005)      &
     				xbilsp(8)
		if(xbilsp(5).gt.0.0001) then
		   write(nress,4003)xbilsp(5),xbilsp(14),              &
     			xbilsp(6),xbilsp(15),xbilsp(7)
		   write(nress,5006)xbilsp(13)
		elseif(xbilsp(7).gt.0.00001) then
		   write(nress,5002)xbilsp(7)
		endif
		write(nress,5004)xbilsp(11)
		write(nress,5003)xbilsp(12)
		if (abs(xbilsp(9)).gt.0.0001) then
			write(nress,5007)xbilsp(9),xbilsp(8),         &
     				xbilsp(4),xbilsp(10)
		endif

	else
		write (nress,9010)

	endif

      return

5000	format(/' speicherinhalt: ',/                                &
               '    anfangsinhalt:               [hm]   ',f10.6,/     &
               '    endinhalt:                   [hm]   ',f10.6)
5001	format(/'    zufluss-fuelle:              [hm/a] ',f10.6,/    &
                '    abfluss-fuelle:              [hm/a] ',f10.6)      
5002	format(/'    entnahme-fuelle:             [hm/a] ',f10.6)
5003	format(/'    speicherinhaltsaenderung:    [hm/a] ',f10.6)
5004	format(/' davon abfluss-fuelle ueberlauf: [hm/a] ',f10.6)
5005	format( '    davon: ',/                                       &
     		'    zufluss-fuelle ueberleitung: [hm/a] ',f10.6)
5006	format(/' mittlere Seeflaeche:            [km2]  ',f10.6)

4003	format(/' Potentielle Seeverdunstung (V): [hm/a] ',f10.6,     &
                       '  [mm]',f4.0/                                 &
               ' Niederschlag               (N): [hm/a] ',f10.6,      &
                       '  [mm]',f4.0/                                &
               ' Netto-Bilanz             (V-N): [hm/a] ',f10.6)

9002	format(' Mittelwerte der Berechnung bei ',i3,                 &
     		' Simulationszyklen')

9010	format(' Kein vollstaendiger jahreszyklus durchlaufen ',/     &
               'Mittelwertberechnung sinnlos')

5007	format(/' summenbilanz Zulaufknoten: ',/                     &
               '     zufluss:                    [hm/a] ',f10.6,/     &
               '     zuleitung in see:           [hm/a] ',f10.6,/     &
               '     stuetzabfluss aus see:      [hm/a] ',f10.6,/     &
               '     gesamtabfluss:              [hm/a] ',f10.6)

      end
