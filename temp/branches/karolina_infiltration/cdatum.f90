!     Last change:  JH   26 Aug 2004    1:18 pm
!copt -64v -dbg -range -nl

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

      subroutine cdatum(tag,monat,jahr,ntage)
!c
!c   ****************************************************
!c   *                                                  *
!c   * das up cdatum aendert ein vorgegebenes datum     *
!c   * um ntage auf ein neues datum.                    *
!c   * dabei aendern sich die werte von tag,monat,jahr. *
!c   * der wert von ntage wird nicht geaendert.         *
!c   * der wert von ntage darf pos. oder neg. sein.     *
!c   *                                                  *
!c   *                           12.11.82               *
!c   *  fuer tag = 0 ,  rev 2 am 12.11.85  diesel       *
!c   ****************************************************
!c
!c
      integer    tag,monat,jahr,ntage,ns
      dimension  ntpm(12),ntpmn(12)
!c
      data       ntpm/31,28,31,30,31,30,31,31,30,31,30,31/
      data      ntpmn/-31,-31,-28,-31,-30,-31,-30,-31,-31,-30,-31,-30/
!c
      if (ntage .eq. 0) return
!c
      if(tag .ne. 0) goto 50
      tag = tag + 1
      if(ntage .gt. 0) ntage = ntage - 1

 50   ns=ntage
      if (ns .lt. 0) go to 200

100   ntpm(2)=28
      if (mod(jahr,4) .eq. 0) ntpm(2)=29
101   if (ns .lt. ntpm(monat)) go to 102
      ns=ns-ntpm(monat)
      monat=monat+1
      if (monat .lt. 13) go to 101
      monat=1
      jahr=jahr+1
      go to 100

102   if (ns .eq. 0) return

      ns=ns-1
      tag=tag+1
      if (tag .le. ntpm(monat)) goto 102
      tag=1
      monat=monat+1
      if (monat .lt. 13) goto 102
      monat=1
      jahr=jahr+1
      go to 102


!c     ns neg

200   ntpmn(3)=-28
      if (mod(jahr,4) .eq. 0) ntpmn(3)=-29

201   if (ns .gt. ntpmn(monat)) go to 202
      ns=ns-ntpmn(monat)
      monat=monat-1
      if (monat .gt. 0) go to 201
      monat=12
      jahr=jahr-1
      go to 200

202   if (ns .eq. 0) return

      ns=ns+1
      tag=tag-1
      if (tag .gt. 0) go to 202
      tag=-ntpmn(monat)
      monat=monat-1
      if (monat .gt. 0) go to 202
      monat=12
      jahr=jahr-1
      go to 202

      end
