!     Last change:  SK   26 Feb 2007    3:49 pm

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
! Dipl.-Ing. Jessica H¸bsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica H¸bsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************


	subroutine bodf_n(kzif,dt,pr,epot,kap,  &
         	 wp,nfk,bmax,cin,cex,            &
     		 bof,inf,eva,perk,Imic,ilay,t,infl)

!**************************************ƒNDERUNGEN***********************************************


!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     ? 	Wiebke Klauder		nur Umbenennung
!  08.09.2004   Claudia Br¸ning         Fehlermeldung bei negativer Bodenfeuchte erweitert. Nach
!                                       der Fehlermeldung stop eingef¸gt, um die Berechnung
!                                       abzubrechen.


!***************************************BESCHREIBUNG********************************************
!c    Berechnung der Bodenfeuchte fuer t.zeitschritt
!c
!c    Bodenfeuchte bof=nutzbare Bodenfuechte [mm]
!c
!***************************************EIN-/AUSGABE********************************************
!c    Eingabe:
!c		kzif	=1 akt. evapotranspiration linear aus bodenfeuchte
!c		     	=2   "     "   nach wendling entsp. DVWK-empfehltung
!c		dt      zeitschrittweite  (h)
!c     		pr	niederschlag abzuegl.interzeption (mm)
!c     		epot	potentielle verdunstung (mm)
!c     		rfak	anteil akt. verdunstung (%)
!c     		kap	kapilare aufstiegsrate (mm)
!c
!c     		bmax	nutzbare max.bodenfeuchte  (mm)
!c     		nfk	nutzbare feldkapazitaet    (-)
!c     		cin	infiltrations-kapazitaet (mm/h)
!c     		cex	durchsickerungs-   "     (mm/h)
!c
!c     		bof	bodenfeuchte aus zeitschritt t-1 (mm)
!c
!c    Ausgabe:
!c
!c     		bof	bodenfeuchte  zeitschritt t (mm)
!c		inf     aktuelle infiltration          (mm)
!c		eva        "     verdunstung aus boden (mm)
!c		perk       "     durchsickerung        (mm)
!c
!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units
      IMPLICIT NONE
      integer	kzif,errcount,ilay,t
      real	dt,pr,epot,rfak,kap,xdif,check
      real	nfk,bmax,cin,cex,wp
      real	bof,inf,eva,perk,bofneu
      REAL::Imic,infl

      REAL, parameter :: abort=100
      errcount = 0

!******************************************ANWEISUNGEN******************************************

bofneu = 0.
                                                         
! Infiltration
!-------------
      inf = dt*cin*(1.-bof/bmax)

      if(inf.gt.pr) inf=pr
      if((bof+inf).gt.bmax) inf=bmax-bof

!write (nres,*)ilay
!write (nres,*)pr
!WRITE(nres,*)t
!write (nres,*)'bof_ini',bof
!write (nres,*)'inf',inf
!write (nres,*)'bmax',bmax

! Perkolation
!------------
      if(bof.lt.nfk) then
	 perk=0.
      else
	 perk= dt*cex*(bof/bmax - nfk/bmax)
	 if((bof-perk).lt.nfk) then
	    perk=bof-nfk
	 endif
      endif
!write (nres,*)'nfk',nfk
!write (nres,*)'perk',perk
! Transpiration
!--------------
      if(bof.le.0.) then
	 eva = 0.
      elseif(kzif.eq.1) then
	 if(bof.gt.nfk) then
	    eva=epot
	 else
	    eva=epot*bof/nfk
	 endif
      else
	 rfak=(1.-wp/(wp+bof))/(1.-wp/(wp+nfk))
	 eva = epot*rfak
      endif

!write (nres,*)'eva',eva
! Bilanzierung
!-------------
300   bofneu=bof+inf+kap-eva-perk

!write (nres,*)'bofneu_ini',bofneu
      check = bofneu - bmax
      if ( check > 0.001 ) then
         write(nerr,*) 'xxx fehler1 in bodf_n (bofneu > bmax). kor. von inf, nach ',abort,&
                       ' interationen abgebrochen -> differenz=',check



         IF (errcount > abort) THEN
            call writeLogIntRealReal(6,'Fehler in der Berechnung der Bodenfeuchte (bof_neu > bmax). Interationen abgebrochen.',&
                & 'Error in calculation of the soil moisture (bof_neu > bmax). Interationen terminated!','','',0,'Differenz',check,&
                & 'Iterationen',abort,'bodf_n')
            call writeFooter()
            stop !TODO: Fehlermanagment
         END IF
	 xdif=bofneu-bmax
	 bofneu=bmax
	 inf=inf-xdif
	 if(inf.lt.0.) then
	    xdif=-inf
	    inf=0.
	    eva=eva+xdif
	    if(eva.gt.epot) then
	       xdif=eva-epot
	       eva=epot
	       if (xdif.gt.0.) then
		  write(nerr,'(a)')  'xxx fehler3 in bodf_n. kor. von perk '
		  perk=perk+xdif
	       endif
	    endif
	 endif
         errcount = errcount + 1
	 goto 300
      endif

      if (bofneu.lt.0.) then
	 eva=eva+bofneu
	 bofneu=0.
! CB 08.09.2004 Fehlermeldung bei negativer Bodenfeuchte erweitert.
    	 write(nerr,1001)

         call writeLogString( 7,'Fehler in der Berechnung der Bodenfeuchte! Bodenfeuchte negativ!', &
              & 'Soil moisture negative!','','','','','','bodf_n')

! CB 08.09.2004 Fehler produziert durch Schleife (goto 300) sehr groﬂe Errordatei, welche den Rechner
! nahezu zum Absturz bringt. Daher wird nach der Fehlermeldung die Berechnung abgebrochen.
	 CLOSE(nerr)
            call writeFooter()
	 stop

	 if(eva.gt.epot) then
	    xdif=eva-epot
	    eva=epot
	    perk=perk-xdif
	    if(perk.lt.0.) then
 	       write(nerr,'(a)') 'xxx fehler4 in bodf_n. kor. von perk'

               call writeLogString(6,'Fehler in der Bodenfeuchteberechnung. Korrektur der Perkolation!',&
                   & 'Error in soil moisture calculation. correct it in percolation!','','','','','','bodf_n')

	    endif
	 endif
	 goto 300
      endif
	
      bof=bofneu
     !write (nres,*)'bof_final',bof
     infl=inf
      return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
1001  FORMAT (/ 1X, 'Fehler2 in bodf_n. kor. von eva!' /&
      	      & 1X, 'Bodenfeuchte negativ!'/&
              & 1X, 'Startwerte von aktuellem Modell erzeugt?')

      end
