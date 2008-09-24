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


      function ntdif(tag1,monat1,jahr1,tag2,monat2,jahr2)

!c *********************************************
!c *                                           *
!c *  ntdif rechnet die zeitdifferenz in tagen *
!c *                                           *
!c *                        kesselheim 2.9.82  *
!c *                                           *
!c *********************************************

      integer tag1,monat1,jahr1,tag2,monat2,jahr2,ntdif,nt1900
      ntdif=nt1900(tag2,monat2,jahr2)-nt1900(tag1,monat1,jahr1)
      return
      end





      function nt1900(tag,monat,jah)

!c ********************************************************
!c *                                                      *
!c *  nt1900 brechnet die zeitdifferenz in tagen zwischen *
!c *  einem datum und dem  0.0.1900                       *
!c *                                                      *
!c *                             kesselheim 2.9.82        *
!c *                                                      *
!c ********************************************************
!c
      integer tag,monat,jah,jahr,ntsm,nt1900
      dimension ntsm(12)
      data ntsm/0,31,59,90,120,151,181,212,243,273,304,334/
      jahr=jah
      if (jahr .gt. 1900) jahr=jahr-1900
      nt1900=jahr*365+ntsm(monat)+tag
      nt1900=(jahr-1)/4+1+nt1900
      if (mod(jahr,4) .eq. 0 .and. monat .gt. 2) nt1900=nt1900+1
      return
      end

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

