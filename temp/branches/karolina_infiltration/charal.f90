!     Last change:  JH   26 Aug 2004    1:19 pm
      subroutine charal (dummyinp,name,ilname)

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
!c     eingabe:  dummyinp, charakter
!c
!c     ausgabe:  bis zu drei strings aus dummyinp, getrennt durch blank
!c
!c            in  name(1), name(2), name(3)
!c                lange der strings in
!c                ilname(1), ilname(2), ilname(3)
!c
!c   z.b:  \projekte.bjo\90113\klima.dat\nwes.lz     \projekte.bjo\90113\klima.dat\nwes.lz
!c     ergibt
!c               nwes.lz    und nwerm.lz
!c
!c

       character*140 dummyinp
       character*120 name(3)

       integer ilname(3),i1,i,istrich

!c      print*,dummyinp

      do 3010 i=1,3
          ilname(i)=0
      name(i)='                                               '
3010  continue

      istrich=0
      i1= 1
      i = 0
3011     i=i+1
         if(i.ge.120.or.i1.gt.3) goto 3015
!c         istring=ichar(dummyinp(i:i))
!c         if(istring.lt.40.or. istring.gt.123) then
         if(dummyinp(i:i).eq.' ') then
               istrich = i
               goto 3011
         endif
3012     if(dummyinp(i:i).eq.'/') istrich=i
         if(dummyinp(i:i).eq.' ') then
            name(i1)=dummyinp(istrich+1:i-1)
            ilname(i1)=i-istrich-1
            i1=i1+1
	    istrich=i
            goto 3011
         endif
         i=i+1
         goto 3012

3015  return

      end
