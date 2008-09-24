!     Last change:  JH   28 Sep 2005    2:51 pm

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
! Dipl.-Ing. Jessica H�bsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica H�bsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************

      subroutine incept(t,dt,bianf,bimax,pr,ept,pri,evi,bi)

!**************************************�NDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     ? 	Wiebke Klauder (WK)     nur Umbenennung der Subroutone (f90)
!
!     28.09.05	Jessica H�bsch (JH)     �bernahme der F�llung des Interzeptionsspeichers aus dem
!		Andreas v. D�mming(AvD) letzten Zeitschritt (bzw. Anfangswert). Hierdurch
!					entfaellt die Variable bivor, da die �bernahme des
!					Ergebnisses durch die Variable bi in der aufrufenden
!                                       Subroutine boden f�r jedes Hydrotop gespeichert wird und
!					im naechsten Zeitschritt als Variable bianf als �bernahme-
!					bzw. Anfangswert in der Berechnung verwendet wird
!					(bi=bianf). (�nderungen wurden durch L�schen der
!					Variable(nverwendung) bivor und durch Initialisierung
!					bi=bianf durchgef�hrt).

!***************************************BESCHREIBUNG********************************************
!c     interceptions-speicher mit konstanter verdunstung

! Mit Hilfe der Subroutine wird der nach Abzug der Interzeption verbleibende Niederschlag
! abhaengig des Verhaeltnisses des Niederschlages zur potentiellen Verdunstung (=<>) berechnet.

!***************************************EIN-/AUSGABE********************************************
!c     t       lfd.nr fuer zeitschritt
!c     dt      zeitschrittweite (h)
!c     bianf   anfangsfuellung  (mm)
!c     bimax   max.fuellung     (mm)
!c     bi      aktuelle fuellung (mm)
!c     pr      niederschlag     (mm/h)
!c     pri     niederschlag abzueglich interzeption (mm/h)
!c     ept     potentielle verdunstung (mm/h)
!c     evi     aktuelle        "       (mm/h)

!******************************************VARIABLEN********************************************

!c     implicit none

      integer t
      real    dt,tmax,bi,bianf,bimax,pr,pri,ept,evi

!******************************************ANWEISUNGEN******************************************

! Initialisierung
! ---------------
      bi=bianf

! pr=ept                                                                ! Niederschlag = pot. Verdunstung
! ------                                                                  ! Es verdunstet "alles"
      if(abs(pr-ept).gt.1.e-03) goto 10
      bi=bi
      pri=0
      evi=ept
      return

! pr > ept		                                                ! Niederschlag > pot. Verdunstung
! --------
10    if(pr.lt.ept) goto 20
      evi=ept                                                             ! akt. Verdunstung ist pot. Verdunstung
      if(bimax.lt.bi) bi=bimax
      tmax=(bimax-bi)/(pr-ept)
      if(tmax.gt.dt) then
         bi=bi+(pr-ept)*dt
         pri=0
         return
      else
         bi=bimax
         pri=(pr-ept)*(dt-tmax)/dt
         return
      endif

! pr < ept                                                              ! Niederschlag < pot. Verdunstung
! --------
20    pri=0
      tmax=bi/(ept-pr)
      if(tmax.gt.dt) then
         bi=bi+(pr-ept)*dt
         evi=ept
         return
      else
         bi=0
         evi=pr-(pr-ept)*tmax/dt
         return
      endif


      end
