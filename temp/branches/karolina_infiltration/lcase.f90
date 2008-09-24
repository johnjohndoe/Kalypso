!     Last change:  JH   26 Aug 2004    5:12 pm

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
	subroutine lcase(char80)
!c
!c
!c
	character*10 char80

	imax=lastchar(char80)

	do 100 i=1,imax
		if(char80(i:i).eq.'A') char80(i:i)='a'
		if(char80(i:i).eq.'B') char80(i:i)='b'
		if(char80(i:i).eq.'C') char80(i:i)='c'
		if(char80(i:i).eq.'D') char80(i:i)='d'
		if(char80(i:i).eq.'E') char80(i:i)='e'
		if(char80(i:i).eq.'F') char80(i:i)='f'
		if(char80(i:i).eq.'G') char80(i:i)='g'
		if(char80(i:i).eq.'H') char80(i:i)='h'
		if(char80(i:i).eq.'I') char80(i:i)='i'
		if(char80(i:i).eq.'J') char80(i:i)='j'
		if(char80(i:i).eq.'K') char80(i:i)='k'
		if(char80(i:i).eq.'L') char80(i:i)='l'
		if(char80(i:i).eq.'M') char80(i:i)='m'
		if(char80(i:i).eq.'N') char80(i:i)='n'
		if(char80(i:i).eq.'O') char80(i:i)='o'
		if(char80(i:i).eq.'P') char80(i:i)='p'
		if(char80(i:i).eq.'Q') char80(i:i)='q'
		if(char80(i:i).eq.'R') char80(i:i)='r'
		if(char80(i:i).eq.'S') char80(i:i)='s'
		if(char80(i:i).eq.'T') char80(i:i)='t'
		if(char80(i:i).eq.'U') char80(i:i)='u'
		if(char80(i:i).eq.'V') char80(i:i)='v'
		if(char80(i:i).eq.'W') char80(i:i)='w'
		if(char80(i:i).eq.'X') char80(i:i)='x'
		if(char80(i:i).eq.'Y') char80(i:i)='y'
		if(char80(i:i).eq.'Z') char80(i:i)='z'
100	continue

	return

	end
