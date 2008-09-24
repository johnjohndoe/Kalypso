!     Last change:  JH   26 Aug 2004    5:14 pm
	subroutine outbil_tg (ismax,m_bil,m_flaech,nr,iz,ispk)

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

!ccccccccccccccc
!c
!c	bildschirm-ausgabe zusammenfassung
    USE Units
	include 'include/param.cmn'
	include 'include.ntz'
!c	include 'include/netz.cmn'
!c	include 'include/anf.cmn'
	real	nr(jdim)

	real	m_bil(jdim,20),m_flaech(jdim,4)

!c	iz=izykl-1
	xz=float(iz)
	if(iz.eq.0) then
		write(nress,9351)iz
		goto 1111
	endif

	do 1100 is = 1,ismax
	if (iteil(is).gt.0) then
	do 1500 i=1,iteil(is)

		ntg = nteil(is,i)

		do 10 isv=1,jdim
			if(nr(isv).eq.ntg) goto 111
10		continue
		write (nress,10901) ntg
111		continue

		do 500 i1=1,20
			m_bil(isv,i1)=m_bil(isv,i1)/xz
500		continue

		write(nress,9251)ntg,iz

		sudif1 = m_bil(isv,1)-m_bil(isv,3)-m_bil(isv,5)

		write(nress,9151)m_bil(isv,1),m_bil(isv,3),          &
                       m_bil(isv,5),sudif1,m_bil(isv,2),             &
                       m_bil(isv,19),m_bil(isv,3)+m_bil(isv,4)

		sudif=m_bil(isv,5)+m_bil(isv,7)-m_bil(isv,4)-        &
                       m_bil(isv,6)-m_bil(isv,8)-m_bil(isv,9)
		write(nress,9152)m_bil(isv,5),m_bil(isv,6),          &
                       m_bil(isv,5)-m_bil(isv,6),m_bil(isv,7),       &
                       m_bil(isv,4),m_bil(isv,8),m_bil(isv,9),       &
                       sudif

	if (ispk.eq.7) then
		write(nress,9400) m_bil(isv,10),m_bil(isv,11),       &
                       m_bil(isv,12),m_bil(isv,13),                  &
                       m_bil(isv,10)-m_bil(isv,11)-m_bil(isv,12)-    &
                       m_bil(isv,13)

		ybil=m_bil(isv,11)+m_bil(isv,14)-m_bil(isv,15)-      &
                       m_bil(isv,16)-m_bil(isv,17)-m_bil(isv,18)     &
                       - m_bil(isv,20)

		write(nress,9401) m_bil(isv,11),m_bil(isv,14),        &
                       m_bil(isv,15),                                 &
                       m_bil(isv,16),m_bil(isv,17),m_bil(isv,18),     &
                       m_bil(isv,20),                                 &
                       ybil
	    else
		write(nress,9500) m_bil(isv,10),m_bil(isv,14),       &
                       m_bil(isv,11),                                &
                       m_bil(isv,12),m_bil(isv,13),                  &
                       m_bil(isv,10)+m_bil(isv,14)-                  &
                       m_bil(isv,11)-m_bil(isv,12)


		ybil=m_bil(isv,11)-m_bil(isv,15)-                    &
                       m_bil(isv,16)-m_bil(isv,17)-m_bil(isv,18)     &
                       - m_bil(isv,20)

		write(nress,9501) m_bil(isv,11),                     &
                       m_bil(isv,15),                                &
                       m_bil(isv,16),m_bil(isv,17),m_bil(isv,18),    &
                       m_bil(isv,20),                                &
                       ybil
	    endif

		write(nress,9402) m_flaech(isv,3),m_flaech(isv,4),   &
                       m_flaech(isv,1),m_flaech(isv,2)

1500	continue
	endif
1100	continue

1111	return


10901	format(' fehler bei bilanz. tg nicht gefunden.',i6)


9351	format(/'-----------------------------------------',/  &
               ' k e i n e    z u s a m m e n f a s s u n g  ',/     &
               ' bilanzierungszeitraum:        ',i2,' jahre'  )

9251	format(/'-----------------------------------------',/   &
               ' z u s a m m e n f a s s u n g  ',/                  &
               ' bilanz fuer teilgebiet:       ',i5,/               &
               ' bilanzierungszeitraum:        ',i2,' jahre'  )

9151	format(/' speicherbilanz interzeption: '/,                  &
               ' niederschlag:(nach schneesp.)', f10.1,' mm'/,        &
               ' interz.verd.:                ', f10.1,' mm'/,        &
               ' bestandsniederschlag:        ', f10.1,' mm'/,        &
               ' fehler(diff.interz.speicher):', f10.1,' mm'/,       &
               ' pot.grasref.verdunstung:     ', f10.1,' mm'/,        &
               ' pot.verdunst. (nutzungskor.):', f10.1,' mm'/,        &
               ' aktuelle verdunstung:        ', f10.1,' mm')

9152	format(/' speicherbilanz bodenspeicher:   ',/                &
               ' bestandsniederschlag:        ', f10.1,' mm'/,        &
               ' overlandflow:                ', f10.1,' mm'/,        &
               ' [infiltration:               ', f10.1,' mm]'/,       &
               ' kapilarer aufstieg:          ', f10.1,' mm'/,        &
               ' verdunstung (bodensp.):      ', f10.1,' mm'/,        &
               ' lateral. abfluss (interflow):', f10.1,' mm'/,        &
               ' perkolation:                 ', f10.1,' mm'/,        &
               ' fehler (dif. bodenspeicher): ', f10.1,' mm')
9400	format(/' aufteilung perkolation: ',/                        &
               ' perkolation bodenspeicher:     ',f10.1,' mm'/,      &
               ' abgabe grundw.speicher:        ',f10.1,' mm'/,       &
               '        tiefengrundw.leiter:    ',f10.1,' mm'/,       &
               '        verlust tiefengw.leiter:',f10.1,' mm'/,      &
               ' fehler:                        ',f10.1,' mm')

9500	format(/' aufteilung perkolation: ',/                       &
               ' perkolation bodenspeicher:     ',f10.1,' mm'/,      &
               ' zufluss grundw. oberh.l.TG:    ',f10.1,' mm'/,     &
               ' abgabe grundw.speicher:        ',f10.1,' mm'/,      &
               '        tiefengrundw.leiter:    ',f10.1,' mm'/,      &
               '        verlust tiefengw.leiter:',f10.1,' mm'/,     &
               ' fehler:                        ',f10.1,' mm')

9401	format(/' bilanz grundwasserspeicher: ',/                   &
               ' zufluss perkolation:           ',f10.1,' mm'/,      &
               '        grundw. oberh.l.TG:     ',f10.1,' mm'/,      &
               ' abgabe basisabfluss:           ',f10.1,' mm'/,      &
               '        grundw. unterh.l.TG:    ',f10.1,' mm'/,      &
               '        kapilarer aufstieg:     ',f10.1,' mm'/,      &
               '        entnahme GWS:          ',f10.1,' mm'/,       &
               ' diff. speicherinhalt:          ',f10.1,' mm'/,      &
               ' fehler:                        ',f10.1,' mm')

9501	format(/' bilanz grundwasserspeicher: ',/                   &
               ' zufluss (perk. und overh.l TG: ',f10.1,' mm'/,      &
               ' abgabe basisabfluss:           ',f10.1,' mm'/,      &
               '        grundw. unterh.l.TG:    ',f10.1,' mm'/,      &
               '        kapilarer aufstieg:     ',f10.1,' mm'/,      &
               '        entnahme GWS:          ',f10.1,' mm'/,       &
               ' diff. speicherinhalt:          ',f10.1,' mm'/,      &
               ' fehler:                        ',f10.1,' mm')

9402	format(/' flaechenanteile: ',/                              &
               '   natuerl. flaeche:            ',f10.3,' qkm'/,     &
               '   vers. flaeche:               ',f10.3,' qkm'/,    &
               '   grundwasserleiter:           ',f10.3,' qkm'/,     &
     		'   tiefengrundwasserleiter:     ',f10.3,' qkm')


	end
