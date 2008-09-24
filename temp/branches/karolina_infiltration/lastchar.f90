!     Last change:  JH   26 Aug 2004    5:12 pm
      integer function lastchar(string)

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


!c Die Funktion LASTCHAR erhaelt als Funktionswert die Nummer des
!c letzten Characters (buchstabe, ziffer, sonderzeichen) eines Strings.

      character string*(*)
!	integer	lastchar

	lan = len(string)

	ilang=0
	do 100 i = 1,lan
		call chartyp(string(i:i),ikey)
		if(ikey .eq. 9) goto 101
		if(ikey .gt. 1) ilang = i
100	continue
101	continue

	lastchar=ilang
!c	yrint*,ilang

	return
	end


      subroutine chartyp(string,ikey)
      character*1 string
!c
!c	string:	blank:			ikey = 1
!c		buchstabe,klein:	ikey = 2
!c		buchstabe,gross:	ikey = 3
!c		zahl:			ikey = 4
!c		zeichen:		ikey = 5
!c		zeichenende:		ikey = 9
!c
	i1 = ichar(string)
	ikey=0
!c	print*,' i1:', i1
	if(i1 .eq.0) ikey = 9
	if(i1 .eq.32) ikey = 1
	if(i1 .ge.65 .and. i1.le. 90) ikey = 3
	if(i1 .ge.97 .and. i1.le. 122) ikey = 2
	if(i1 .ge.48 .and. i1.le. 57) ikey = 4
	if(i1 .ge.33 .and. i1.le. 47) ikey = 5
	if(i1 .ge.58 .and. i1.le. 64) ikey = 5
	if(i1 .ge.91 .and. i1.le. 96) ikey = 5
	if(i1 .ge.123 .and. i1.le.175) ikey = 5
	return
	end
