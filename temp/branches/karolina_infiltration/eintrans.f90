!     Last change:  JH   26 Aug 2004    1:19 pm
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


      subroutine eintrans(chein,wert,error)
!c
!c    ein eingelesener wert wird als realzahl umgewandelt
!c
!c    eingabe:  chein  character
!c    ausgabe:  wert   real
!c              error  errorflag
!c

      character*10 chein

      real  wert

      integer   error

      error=0
      iblank=0
      ipoint=0
      istart=0
!c      read(*,'(a)')chein

      i=0
998   i=i+1
      if(i.gt.10) then
         wert=0.
         return
      endif
      if(chein(i:i).eq.' ') goto 998
      istart=i

      do 1000 i=istart,10
        if(chein(i:i).eq.',') chein(i:i)='.'
        if(chein(i:i).ne.'1'.and.chein(i:i).ne.'2'.and.               &
           chein(i:i).ne.'3'.and.chein(i:i).ne.'4'.and.               &
           chein(i:i).ne.'5'.and.chein(i:i).ne.'6'.and.               &
           chein(i:i).ne.'7'.and.chein(i:i).ne.'8'.and.               &
           chein(i:i).ne.'9'.and.chein(i:i).ne.'0'.and.               &
           chein(i:i).ne.'.'.and.chein(i:i).ne.' '.and.               &
           chein(i:i).ne.'-') goto 5006
         if(chein(i:i).eq.'.')then
             if(ipoint.gt.0) goto 5000
             ipoint=i
         endif
         if(chein(i:i).eq.' ')then
            iblank=i
            goto 3000
         endif
1000  continue
      write(*,2001)
2001  format(' fehlerhafte eingabe: mehr als 9 stellen!')
      error=1
      return

5000  write(*,2002)
2002  format(' fehlerhafte eingabe: mehr als 1 komma!')
      error=1
      return

5006  write(*,2006)
2006  format(' fehlerhafte eingabe: keine ziffer!')
      error=1
      return

3000  if(ipoint.gt.0) then
         read(chein(istart:iblank),'(f20.9)')  wert
      else
         read(chein(istart:iblank),'(i10)')  nwert
         wert=real(nwert)
      endif

      return

      end

