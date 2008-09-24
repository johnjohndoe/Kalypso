!     Last change:  JH   17 Jan 2007    8:20 am

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

!SK
!SK   -----------------------------------------------------------------------
!SK   Änderung am 28.02.03 bis 04.03.03 Stephan Kraessig (SK), Prof. Pasche (EP)
!SK   -----------------------------------------------------------------------
!SK   Neue Version ist Jahr 2000 faehig und kann bei Angabe von 1000001 als
!SK   Anfangsdatum in der *.konfig Datei für jeden gerechneten Zeitschritt
!SK   der Langzeitsimulation Anfangswerte für die Kurzzeitsimulation erzeugen.

	subroutine inp_veg_b(ndg,ijaha,natg,namo,naja,pns,     &
      			xtime,xkc,xwt,xlai)

	integer	ndg,natg,namo,naja,ijaha
	real	xtime,xkc,xwt,xlai
	character*1	pns

10	read(ndg,426,end=100)itagl,imonl,ijahl,xkc,xwt,xlai
426	format (i2,1x,i2,1x,i2,1x,3(f8.2))

	if(pns == 's') then
		rewind (ndg)
		read(ndg,*)
		read(ndg,*)
		read(ndg,*)
		read(ndg,426,end=100)itagl,imonl,ijahl,xkc,xwt,xlai
		return
	endif
	if(itagl == 99) goto 100
	nleaf=ntdif(natg,namo,naja,itagl,imonl,ijaha+ijahl)

!c	xtime=nleaf*24.-nast+12.
	xtime=real(nleaf)

	return
!c SK/EP 28.02.03
!c Jahr 2000 Faehigkeit: ijaha kann zu null werden, da das Jahr 2000 mit 00 gerechnet wird.
!c Die Datei muß auch für diesen Fall komplett eingelesen werden.
!c 100	if(ijaha.ge.10) then

100	if(ijaha >= 00) then
		rewind (ndg)
		read(ndg,*)
		read(ndg,*)
		read(ndg,*)
		ijaha=ijaha+1
		goto 10
	endif
	write(*,101)

101	format('xxx fehler in datenfile vegetationsparam.'/,            &
      	' simulationszeitr. wird nicht durch nutzungswerte abgedeckt')
!c     *	' filename: ',a)
!        call writeFooter()
	stop 'stop in inp_veg_b'

	end
