!     Last change:  JH   17 Jan 2007    7:56 am

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

	subroutine inp_snow(filetree,ilen,snowtyp,					&
     			xwwo,xwwmax,xsnotem,xsnorad,xh0,slash)

!c
!c	uebernahme der schneeparameter aus datei snowtyp.dat in hydro.top
!c
        USE generic_LOGGER
        USE Units
	character*80	filetree
	character*80	text,namsnow
	character*20	snowtyp,texttyp	

	character*1		slash
	real			xwwo,xwwmax,xsnotem,xsnorad,xh0

	integer	nsn,ilen,ilent

!cccccccccccccccccccccccccccccccccccc

	nsn=ju0gfu()

!	call lcase(snowtyp)
!	dim	snowtyp
	
!		texttyp = LCase (snowtyp)
	ilent=lastchar(snowtyp)

	namsnow=filetree(1:ilen)//'hydro.top'//slash//'snowtyp.dat'

	open (nsn,iostat=ierr,err=995,status='old',file=namsnow)

333	read(nsn,'(a)',err=996,end=997) text 
	if(text(1:1).eq.'/') goto 333 
	texttyp=text(1:20)
!	call lcase (texttyp)
!	texttyp = LCase (texttyp)

	if(snowtyp(1:ilent).eq.texttyp( 1:ilent)) then			
		read(text(21:80),*,err=996)							&
     			xwwo,xwwmax,xsnotem,xsnorad,xh0
		goto 999
	endif
	goto 333


999	close (nsn)

	return


!ccccccccccccccccccccccccccccc
!c
!c	fehlermeldungen
!c

995	write(nerr,1995)namsnow
    call writeLogString(7,'Datenfile mit Schneeparametern nicht vorhanden!','File with snow parameters not found!',&
         & namsnow,'','','','','inp_snow')
        call writeFooter()
	stop
1995	format( 'xxx datenfile mit schneeparametern nicht gefunden!',/	&
     		'xxx filename: ',a)

996	write(nerr,1996)namsnow
    call writeLogString(7,'Fehler beim Einlesen der schneeparametern! Ueberpruefe Dateiformat.', &
         &'Error when reading the snow parameters! Check the file format.',&
         & namsnow,'','','','','inp_snow')
        call writeFooter()
	stop
1996	format( 'xxx fehler beim einlesen der schneeparametern!',/		&
     		'xxx ueberpruefe dateiformat und parameteranzahl. ',/		&
     		'xxx filename: ',a)

997	write(nerr,1997)snowtyp,namsnow
    call writeLogString(7,'Verwendeter Schneetyp ist in den Schneeparametern nicht spezifiziert.', &
         &'Usedsnow type has no paramater specification.',namsnow,'','','snowtyp',snowtyp,'inp_snow')

        call writeFooter()
	stop
1997	format( 'xxx in datenfile mit schneeparametern keine angaben',/	&
     		'xxx fuer schneetyp: ',a,/									&
     		'xxx filename:      ',a)


	end


