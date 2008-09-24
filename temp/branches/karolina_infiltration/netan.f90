!     Last change:  JH    2 Jan 2007    5:19 pm

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

      subroutine netan(ismax,fnetz,ilen,iteilg,nstop)



!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     09.11.05	Jessica Huebsch (JH)	Strukturiert, Kommentiert und Aufgeraeumt.

!***************************************BESCHREIBUNG********************************************
!c    procedure  n e t w o r k  a n a l y s e r
!c
!c    liest netzdatei in die variablen des na-modells ein
!c    und analisiert datei auf fehler im netzaufbau

!***************************************EIN-/AUSGABE********************************************
!c    ismax = anzahl der straenge in der vernetzungsmatrix
!c    ikmax = anzahl der knoten in der vernetzungsmatrix
!c    itlmax = anzahl der teilgebiete
!c    fnetz = filename der netzdatei
!c    ilen = laenge des filenamens
!c    knoto, knotu : knotennummern am oberen bzw unteren strangende


!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      implicit logical (a-z)
      include 'include\param.cmn'

!TODO: JH, implicit herausnehmen


      character*80 		fnetz,text
      integer 			i,ia,ib,is,ismax,ju0gfu,infil,              &
              			ik,ierr,ilen,nzufanz,                                 &
              			itl,nstop,iteilg,is1,i1
      include 'include\str.cmn'
      include 'include\knt.cmn'
      include 'include\netz.cmn'

      nzufanz=0
      nstop=0
      is=0
      iteilg=0

!******************************************ANWEISUNGEN******************************************

!------------------------------------------------------------------
! E r f a s s u n g   d e r    V E R N E T Z U N G S M A T R I X  -
!------------------------------------------------------------------

      infil = ju0gfu()
      open (infil,iostat=ierr,err=9000,file=fnetz,status='old')

10    is = is+1

      if(is.gt.jdim)then                  				! Maximale Anzahl der Straenge überschritten.
         read(infil,'(i8)',err=9600)i
         if(i .eq. 99999 .or. i .eq.999999 .or. i .eq. 9999) goto 111
         call writelogIntIntInt(7,'Felddimensionierung max Stranganzahl erreicht. Erhoehe Parameter jdim!',&
              &'Max. number of strands exceeded.','','Letzter eingelesener Strang',istrng(is-1),'is',is,'jdim',jdim,'netan')
         WRITE(nerr,1010) istrng(is-1)
1010     FORMAT (/ 1X, 'Fehler in der Netzdatei!'/				&
		 & 1X, 'Felddimensionierung nicht ausreichend.'/		&
                 & 1X, '!!!Programmabbruch!!! Erhoehe Parameter jdim!'/	&
                 & 1X, 'Letzter eingelesener Strang: ',i5)
         nstop=1
         return
      endif

33    read(infil,'(a80)') text
      if(text(1:1).eq.'\') goto 33                              	! Kommentar in der Datei

      read(text(1:28),'(3(i8),i4)',err=9600) istrng(is),knoto(is),knotu(is),iteil(is)
      i1=istrng(is)

      if(istrng(is).eq.99999.or.istrng(is).eq.999999.or.istrng(is).eq.9999) then
	 goto 111                                                       ! Ende Vernetzungsmatrix
      endif

      if(iteil(is).gt.igwdim) then		                                                    	! Mehr als mögliche Anzahl TG´s am Strang
         call writelogIntInt(7,'Max. Anzahl Teilgebiete an einem Strang ueberschritten!',&
              &'Max. number of sub catchments at one strand exceeded!','','Strang', istrng(is),'igwdim',igwdim,'netan')
         write(nerr,9501) istrng(is),igwdim
         nstop=1
         return
      endif

      if (knotu(is).eq.0) then
      call writelogIntInt(7, &
           &'Anzahl der angegebene Teilgebietsnummern ist groesser als Parameter der einzulesenden Teilgebiete!',&
           &'Number of sub catchments bigger then read sub catchments!','','Strang',istrng(is-1),'iteil',iteil(is),'netan')
         write (nerr,12) istrng(is-1)
         nstop=1
         return
12       format(/ 1X,'Fehler in der Netzdatei!'/                                    &
                & 1X,'knotu = 0,'/' moeglicher Fehler:'/                            &
                & 1X,'Anzahl der angegebene Teilgebietsnummern ist groesser als'/   &
                & 1X,'Parameter der einzulesenden Teilgebiete iteil.'/              &
                & 1X,'Letzter eingelesener strang: ',i5)
      endif

      if(iteil(is).gt.0) then                                           ! Einlesen der angeschlossenen TG Nummern
         do 14 i=1,iteil(is)
          iteilg=iteilg+1
          read(infil,'(i8)',err=9600) nteil(is,i)
14       continue
      endif

      goto 10                                   			! Einlesen der naechsten Zeile (Strang)

111   continue                                                          ! Ende Vernetzungsmatrix

      ismax = is-1
      write (nres,3301)ismax
      write (nres,3302)iteilg
3301  format(/ 1X,'Anzahl der Straenge in der Netzdatei:    ',i3)
3302  format(1X,'Anzahl der Teilgebiete in der Netzdatei: ',i3)


!----------------------------------------------------
! E i n l e s e n   d e r    K N O T E N D A T E N  -
!----------------------------------------------------

      ik=0
20    ik = ik+1

      if(ik.gt.ikdim)then                                		! Maximale Anzahl der Knoten überschritten.
         read(infil,'(i5)',err=9610) i
         if (i .eq. 99999 .or. i .eq. 999999 .or. i .eq. 9999) goto 222
         call writelogIntInt(7,'Max. Anzahl Knoten ueberschritten!',&
              & 'Max. number of nodes exceeded!','','Knoten',knot(ikdim),'ikdim',ikdim,'netan')
         WRITE (nerr, 1011) knot(ikdim)
1011     FORMAT (/ 1X,'Fehler in der Netzdatei!'/				&
	         & 1X,'Felddimensionierung nicht ausreichend.'/			&
                 & 1X,'!!!Programmabbruch!!! Erhoehe Parameter ikdim!'/		&
                 & 1X,'Letzter eingelesener Knoten: ',i6)
         nstop =2
         return
      endif
                                                                     
      read(infil,'(6(i5))',err=9610) knot(ik),izug(ik),iabg(ik),iueb(ik),izuf(ik),ivzwg(ik)

! Einlesen von Knoten zu Knoten Beziehungen
      if (izug(ik).gt.0) then                                           ! konst. Zufluss
         read(infil,'(f10.3)',err=9610) qzug(ik)
      endif
      if (iabg(ik).gt.0) then                                           ! konst. Entnahme
	 read(infil,'(f10.3,i8)',err=9610) qabg(ik),ikzie(ik)
      endif
      if (iueb(ik).gt.0) then                                           ! Überlauf
	 read(infil,'(f10.3,i8)',err=9610) queb(ik),ikzue(ik)
      endif
      if (izuf(ik).gt.0) then                                           ! Zuflusskennlinie
         nzufanz=nzufanz+1
         if(nzufanz.gt.10) then
            call writeLogString( 7, 'Max. Anzahl an Knoten mit Zuflusswelle ueberschritten!', &
                 & 'Maximum number of nodes with an inflow hydrograph exceeded!','','','','nzufanz','10','netan' )
            write(nerr,3113)
3113        format(/ 1X,'An max. 10 Stellen im Netz kann ein Zufluss '/	&
                   & 1X,'durch eine vorgegebene Welle erfolgen!')
            nstop=1
            return
         endif
         write(nzuf(nzufanz)(1:2),'(i2)',err=9610) izuf(ik)
         read(infil,'(a)',err=9610) nzuf(nzufanz)(3:10)
         read(infil,'(a)',err=9610) nzuf(nzufanz)(11:80)
         izuf(ik)=nzufanz
      endif
      if (ivzwg(ik).gt.0) then                                          ! Verzweigung
	 read(infil,'(f10.3,i8)',err=9610) zproz(ik),ikz1(ik)
      endif

      if (knot(ik).eq. 0 )then
	 goto 9750
      endif
      if (knot(ik).eq.99999.or.knot(ik).eq.999999.or.knot(ik).eq.9999) then
	 goto 222
      endif

      goto 20                                                           ! Einlesen der naechsten Zeile (Knoten)
222   ikmax = ik-1                                                      ! Ende Einlesen Knotendaten

      close (infil)                                                     ! Netzdatei schließen



! Ueberpruefung auf doppelte Strangnummern
!------------------------------------------

      do 6333 ia=2,ismax
       do 6334 ib=1,ia-1
        if(ib.ne.ia.and.istrng(ib).eq.istrng(ia)) then
           call writeLogIntInt( 7, 'Fehler in der Netzstruktur! Strang existiert zweifach im Netz!', &
                & 'Error in the net structure! Duplicated strand in the net','', 'Strang',istrng(ia), '',0, 'netan' )
           write (nerr,6335) istrng(ia)
6335       format (/ 1X,'Fehler in der Netzstruktur!'/	             	&
                   & 1X,'Der strang ',i5,' existiert doppelt im Netz!'/    	&
                   & 1X,'Ueberpruefe Netzdatei!')
           nstop=1
        endif
6334   continue
6333  continue    


! Ueberpruefung auf doppelte Teilgebietsnummern
!-----------------------------------------------

      do 6801 is=2,ismax
       do 6802 i=1,iteil(is)
        do 6803 is1=1,is-1
         do 6804 i1=1,iteil(is1)
          if(nteil(is,i).eq.nteil(is1,i1)) then
             call writeLogIntInt( 7, 'Fehler in der Netzstruktur! Teilgebiet existiert doppelt im Netz!', &
                  & 'Error in the Net structure! Duplicated sub catchment in the net','', 'Teilgebiet',nteil(is,i), '',0, 'netan' )
             write (nerr,6835) nteil(is,i)
6835         format (/ 1X,'Fehler in der Netzstruktur!'/          		&
                     & 1X,'Das Gebiet ',i5,' existiert doppelt im Netz!'/   &
                     & 1X,'Ueberpruefe Netzdatei!')
             nstop=11
          endif
6804     continue
6803    continue
6802   continue
6801  continue    




! Ueberpruefung der Endknoten, ob Fortsetzung im Netz gegeben ist
!-----------------------------------------------------------------
! Beabsichtigte zusaetzliche Endknoten muessen Knotennummer
! zwischen 8000 und 9000 aufweisen


      ia = 0
1001  ia = ia+1
      if (ia.eq.ismax) then                                     ! Netz wurde vollstaendig analysiert
	 goto 2000
      endif

      if (knotu(ia).eq.knotu(ismax)) then                       ! knotu(ia) ist Endknoten des Netzes
	 goto 1001                                              ! und besitzt somit keine Fortfuehrung
      endif

      ib = ia
1002  ib = ib+1
      if (knoto(ib).eq. knotu(ia)) then                         ! Weiterführender Knoten gefunden
	 goto 1001                                              ! Naechster Endknoten kann analysiert werden
      endif

      if (ib.lt.ismax) then
	 goto 1002
      endif                                       			! Kein weiterführender Knoten gefunden
      call writeLogIntInt( 6, 'Fuer einen Endknoten wurde keine Anfangsknoten gefunden.', &
           & 'For one end node there has not been found the initial node.','', 'Knoten',knotu(ia), '',0, 'netan' )
      write(nerr,1101) knotu(ia)
      write(*,1101) knotu(ia)
1101  format (/ 1X,'Fuer den Endknoten ',i5,' wurde kein Anfangsknoten gefunden!')

      if (knotu(ia).gt.8000.and.knotu(ia).lt.9000) then
         call writeLogIntInt( 6,'Endknoten besitzt keine Fortfuehrung im Netz. Abschlagsknoten?',&
              &'End node does not have any continuity in the net','','Knoten',knotu(ia), '',0, 'netan' )
         write (nerr,1102) knotu(ia)
1102     format (/ 1X,'!!! Warnung !!!'/                                         		&
                 & 1X,'Der Endknoten ',i5,' besitzt keine Fortfuehrung im Netz!'/      	&
                 & 1X,'Er sollte Abschlaege in ausserhalb des Untersuchungsgebietes'/    	&
                 & 1X,'liegende Bereiche beschreiben.'/                             	&
       	         & 1X,'Sonst: Fehler in der Netzztruktur!')
      else
         call writeLogIntInt(7, 'Fehler in der Netzstruktur! Endknoten besitzt keine Fortfuehrung im Netz!', &
              & 'Error in the net structure! End node has no continuity in the net ','', 'Knoten',knotu(ia), '',0, 'netan' )
         write (nerr,1103) knotu(ia)
1103     format (/ 1X,'Fehler in der Netzstruktur!'/                         	&
                 & 1X,'Der Endknoten ',i5,' besitzt keine Fortfuehrung im Netz!'/   &
                 & 1X,'Ueberpruefen Sie die Netzdatei!')
         nstop=1
      endif

      goto 1001

2000  continue


      itl = 0
      itlmax = 0
      do 6401 is = 1,ismax
         do 6501 i = 1,iteil(is)
            itl = itl + 1
            itlmax = itl
            itlz(itl) = nteil(is,i)
 6501    continue
 6401 continue



! Ueberpruefung der Anfangsknoten, ob Netzanschluss gegeben ist
!---------------------------------------------------------------
! Beabsichtigte Anfangsknoten muessen Knotennummern zwischen 9000 und 9999 aufweisen

      if(ismax.eq.1) goto 6001                                  ! Unnoetig, falls nur ein Strang gegeben ist

      ia=0
      ib=0
3001  ia=ia+1
      if (ia.eq.ismax+1) then                                   ! Alle Straenge wurden ueberprueft
	 goto 3000
      endif

      if (knoto(ia).gt.9000 .and. knoto(ia).lt.10000) then      ! Es handelt sich um einen Anfangsknoten.
	 goto 3001                                              ! Naechster Strang kann geprueft werden.
      endif

!TODO: JH, wird dies noch geraucht???
      if (knoto(ia).gt.90000 .and. knoto(ia).lt.100000) then    ! Ergaenzung fuer Aabach-Projekt
	  goto 3001
      endif

      ib=ia
3002  ib=ib-1
      if (knoto(ia).eq.knotu(ib)) then                          ! Enknoten wurde fuer Anfangsknoten gefunden.
	 goto 3001                                              ! Kontrolliere naechsten Strang.
      endif

      if (ib.gt.1) then
	 goto 3002
      endif

! Indirekter Zufluss ueber Knotendaten moeglich
      do 3010 i=1,ikmax
       if (ikzie(i).eq.knoto(ia)) then
          write(nres,3101) knoto(ia),knot(i)
          goto 3001
       endif
3010  continue
      do 3011 i=1,ikmax
       if (ikzue(i).eq.knoto(ia)) then
          write(nres,3101) knoto(ia),knot(i)
          goto 3001
       endif
3011  continue
      do 3012 i=1,ikmax
       if (ikz1(i).eq.knoto(ia)) then
          write(nres,3101) knoto(ia),knot(i)
          goto 3001
       endif
3012  continue
3101  format (/ 1X,'!!! Warnung !!!'/                                             &
              & 1X,'Knoten ',i5,' erhaelt lediglich Zuschlaege von Knoten ',i5)


      call writeLogIntInt(7, 'Fehler in der Netzstruktur! Fuer den Anfangsknoten wurde kein Endknoten gefunden!', &
           & 'Error in the net structure! For the initial node there is no end node ','', 'Knoten',knoto(ia), '',0,&
           & 'netan' )
      write(nerr,3090)knoto(ia)
3090  format (/ 1X,'Fehler in der Netzstruktur!'/                         		&
              & 1X,'Fuer Anfangsknoten ',i5,' wurde kein Endknoten gefunden!'/    	&
              & 1X,'Das Netz ist nicht zusammenhaengend.'/                        	&
              & 1X,'Kontrolliere Netzdatei!')
      nstop=1
      goto 3001

3000  continue


! Ueberpruefung, ob Anfangsknoten eindeutig sind und nicht mehrmals autreten
!----------------------------------------------------------------------------

      do 5001 is=1,ismax
       if (knoto(is).lt.9000 .or. knoto(is).gt.9999) then
          do 5002 i=1,ismax
           if (i.eq.is) then
	      goto 5002
	   endif
           if (knoto(is).eq.knoto(i)) then
           call writeLogIntInt(7,'Knoten wird mehrmals als Anfangsknoten verwendet. Umgekehrte Baumstruktur beachten!', &
                & 'Node is used several times as initial node. Attend to inverse tree structure','', 'Knoten', knoto(is), &
                & '',0, 'netan' )
              write (nerr,5501) knoto(is)
              nstop=1
           endif
5002      continue
       endif
5001  continue

5501  format(/ 1X,'Fehler in der Netzdatei!'/                           		&
             & 1X,'Knoten ',i5,' wird mehrmals als Anfangsknoten benutzt.'/     	&
             & 1X,'Eindeutigkeit der Baumstruktur ist nicht gegeben.'/          	&
             & 1X,'Korrigiere Netzdatei!')



! Ueberpruefung, ob alle Knoten in der Netzdatei aufgenommen sind
!-----------------------------------------------------------------


6001  do 6100 ik=1,ismax
       i=0
6200   i=i+1
       if(i.gt.ikmax) then
       call writeLogIntInt(7,&
            &'Knoten wird in der Netzstruktur verwendet, aber nicht in den Knotendaten aufgefuehrt.', &
            &'Node is used in the net structure but is not specified in the node data','', 'Knoten',knoto(ik),'',0, 'netan' )
          write(nerr,6900)knoto(ik)
          nstop=1
       else
          if(knoto(ik).gt.9000.and.knoto(ik).lt.10000) then
	     goto 6100
	  endif
          if(knoto(ik).ne.knot(i)) then
	     goto 6200
	  endif
       endif
6100  continue
        do 6600 ik=1,ismax
          i=0
6700      i=i+1
          if(i.gt.ikmax) then
             call writeLogIntInt(7,&
                  &'Knoten wird in der Netzstruktur verwendet, aber nicht in den Knotendaten aufgefuehrt',&
                  &'Node is used in the net structure but is not specified in the node data','','Knoten',knotu(ik),'',0, 'netan' )
             write(nerr,6900)knotu(ik)
             nstop=1
          else
             if(knotu(ik).ne.knot(i)) goto 6700
          endif
6600   continue

6900   format(/ 1X,'Fehler in der Netzdatei!'/                         		&
              & 1X,'Knoten ',i5,' wird in der Netzstruktur verwendet,'/        	&
              & 1X,'aber nicht in den Knotendaten aufgefuehrt.'/                    &
              & 1X,'Ergaenzen Sie die Netzdatei!')


! Ueberpruefung der Abschlaege an Knoten auf sinnvolle Reihenfolge innerhalb der Netzbearbeitung
!------------------------------------------------------------------------------------------------

      do 4001 ik=1,ikmax                                                ! Alle Knotendaten werden ueberprueft

       if (iabg(ik).gt.0) then                                          ! Kontrolle von iabg (konst. Abgabe)
          if (ikzie(ik).gt.8000.and.ikzie(ik).lt.9000) then
	     goto 4002
	  endif
          i=0
4100      i=i+1
          if (i.gt.ismax) then
             call writeLogIntInt(7,&
                  &'Abschlagknoten angenommen. Knoten ist jedoch nicht im Netz angebunden! ', &
                  &'Reductionnode assumed. Node is not in the net structure.','', 'Knoten',knot(ik),'',0, 'netan')
            write (nerr,4501) knot(ik)
             nstop=1
             goto 4002
          endif
          if (knot(ik).eq.knoto(i)) then
	     goto 4110
	  endif
          goto 4100
4110      i=i+1
          if (ikzie(ik).eq.knoto(i)) then
	     goto 4002
	  endif
          if (i.lt.ismax) then
	     goto 4110
	  endif
          if (ikzie(ik).eq.knotu(ismax)) then
	     goto 4002
	  endif
          call writeLogIntInt( 6, 'Zielknoten der Knotenbeziehung kann nicht erreicht werden, da vorher berechnet!',&
               &'Relationship of one node is unreachable because it was calculated before.', '','Knoten', knot(ik),&
               &'ikzie',ikzie(ik),'netan' )
          write (nerr,4511) knot(ik),ikzie(ik)				! Fehlermeldung produziert nur eine Warnung.
          write (*,4511) knot(ik),ikzie(ik)                             ! Programm wird zunaechst fortgesetzt.
          nstop=4
       endif


4002   if (iueb(ik).gt.0) then                                        	! Kontrolle von ikzue (Ueberlauf)
          if (ikzue(ik).gt.8000.and.ikzue(ik).lt.9000) then
	     goto 4003
	  endif
          i=0
4200      i=i+1
          if (i.gt.ismax) then
             call writeLogIntInt(7,'Ueberlaufknoten angenommen, der Knoten ist jedoch nicht im Netz angebunden! ', &
                  &'Overflownode assumed, the node is not in the river system','', 'Knoten',knot(ik),'',0,'netan')
             write(nerr,4501) knot(ik)
             nstop=1
             goto 4003
          endif
          if (knot(ik).eq.knoto(i)) then
	     goto 4210
	  endif
          goto 4200
4210      i=i+1
          if (ikzue(ik).eq.knoto(i)) then
	     goto 4003
	  endif
          if (i.lt.ismax) then
	     goto 4210
	  endif
          if (ikzue(ik).eq.knotu(ismax)) then
	     goto 4003
	  endif
          call writeLogIntInt( 6, 'Zielknoten der Knotenbeziehung kann nicht erreicht werden, da vorher berechnet!',&
               &'Relationship of one node is unreachable because it was calculated before.', '','Knoten', knot(ik),&
               &'ikzue',ikzue(ik),'netan' )
          write (nerr,4511) knot(ik),ikzue(ik)
          write (*,4511) knot(ik),ikzue(ik)
          nstop=1
       endif


4003   if (ivzwg(ik).gt.0) then                                		! Kontrolle von ikz1 (Verzweigung)
          if (zproz(ik).lt.0.0 .or. zproz(ik).gt.1.0) then
             call writeLogIntReal(7,&
                  & 'Aufteilungsfaktor fuer den Knoten sollte einen Wert zwischen 0 und 1 aufweisen!',&
                  & 'Error in the net file! Branching rate for the node must be between 0 and 1','','Knoten',knot(ik),'zproz',&
                  & zproz(ik),'netan')
             write (nerr,4611) knot(ik)
             nstop=1
          endif
          if (ikz1(ik).gt.8000.and.ikz1(ik).lt.9000) then
	     goto 4004
	  endif
          i=0
4300      i=i+1
          if (i.gt.ismax) then
             call writeLogIntInt(7,'Verzweigungsknoten angenommen. Knoten ist jedoch nicht im Netz angebunden! ', &
                  &'Branchnode assumed. Node is not in the net structure.','', 'Knoten',knot(ik),'',0, 'netan' )
             write (nerr,4501) knot(ik)
             nstop=1
             goto 4004
          endif
          if (knot(ik).eq.knoto(i)) then
	     goto 4310
	  endif
          goto 4300
4310      i=i+1
          if (ikz1(ik).eq.knoto(i)) then
	     goto 4004
	  endif
          if (i.lt.ismax) then
	     goto 4310
	  endif
          if (ikz1(ik).eq.knotu(ismax)) then
	     goto 4004
	  endif
          call writeLogIntInt( 6, 'Zielknoten der Knotenbeziehung kann nicht erreicht werden, da vorher berechnet!',&
               &'Relationship of one node is unreachable because it was calculated before.', '','Knoten', knot(ik),&
               &'ikz1',ikz1(ik),'netan' )
          write (nerr,4511) knot(ik),ikz1(ik)
          write (*,4511) knot(ik),ikz1(ik)
          nstop=1
       endif

!ccc  kontrolle von izuf (zufluss vorgegebene funktion)
4004   if (izuf(ik).gt.0) then
           call writeLogIntInt( 5, 'Es wurde eine Zuflussganglinie angegeben fuer Knoten...',&
                &'There is a inflow hydrograph for node...','','Knoten', knot(ik),'',0,'netan' )
          write (nres,4401) knot(ik), nzuf(izuf(ik))
       endif

4001  continue
      return


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************



4401  format(/ 1X,'Fuer Knoten ',i5,' wurde eine Zuflussganglinie angegeben'/	&
             & 1X,'Knotennummer und Eingabefile:'/                               	&
       	     & 1X,a)

4511  format(/ 1X,'!!! Warnung !!! Moeglicher Fehler in der Netzdatei!'/   	&
             & 2X,'Beziehung von Knoten ',i5,' existiert auf Knoten ',i5/   &
             & 2X,'Dieser Knoten existiert nicht in ihrem Netz.'/          	&
             & 2X,'Ueberpruefe Abschlag am Knoten. '/                     	&
             & 1X,'Oder:'/           					&
             & 2X,'Der Zielknoten kann nicht mehr erreicht werden, '/       &
             & 2X,'da er im Netz vor dem betrachteten Knoten liegt.'/       &
             & 2X,'Beachte: Verbindung zu Zielknoten erfolgt erst,'/	&
             & 2X,'wenn oberer Knoten bearbeitet (berechnet) wurde.')

4501  format(/ 1X,'Fehler in der Netzdatei!'/                           	&
             & 1X,'Fuer Knoten ',i5,' wird ein'/				&
             & 1X,'Abschlag/ Ueberlauf/ Verzweigung angenommen, '/      	&
             & 1X,'der Knoten ist jedoch nicht im Netz angebunden!')

4611  format(/ 1X,'Fehler in der Netzdatei!'/                            	&
             & 1X,'Aufteilungsfaktor fuer Knoten ',i5,' sollte einen Wert '/&
             & 1X,'zwischen 0 und 1 aufweisen!'/ 				&
             & 1X,'Korrigiere zproz = ',f6.2)

9000  write(nerr,9001) fnetz(1:ilen)
      call writeLogString(7,'Netzdatei konnte nicht geoeffnet werden. Ueberpruefen Sie diese Datei in der directory ../input', &
           &'Net file can not be oppened. Examine this file in the directory ../input',fnetz(1:ilen),'','','','','netan')

9001  format(/ 1X,'Netzdatei mit Filenamen ',a,' konnte nicht geoeffnet werden.'/&
             & 1X,'Ueberpruefen Sie diese Datei in der directory ../input.')
      nstop=1
      return

9501  format(/ 1X,'Fehler in der Netzdatei!'/                                	&
             & 1X,'Felddimensionierung nicht ausreichend.'/                        	&
             & 1X,'An Strang ',i5,' wurden zu viele Teilgebiete angeschlossen!'/   	&
             & 1X,'Maximal moegliche Anzahl: igwdim= ',i3/                         	&
             & 1X,'Korrigieren Sie das Netz oder erhoehen Sie den Parameter igwdim.')

9600  write(nerr,9601) istrng(1:ilen)
      call writeLogIntInt( 7, 'Fehler beim Einlesen der Vernetzungsmatrix. Letzter eingelesener Strang:', &
           & 'Error when reading the networking matrix. Last read strand:','','Strang',istrng(is),'',0,'netan')

9601  format(/ 1X,'Fehler in der Netzdatei!' /                         		&
             & 1X,'Fehler beim Einlesen der Vernetzungsmatrix.'/                   	&
             & 1X,'Letzter eingelesener Strang: ',i5)
      nstop=1
      return

9610  write(nerr,9611) knot(ik-1)
      call writeLogIntInt( 7, 'Fehler beim Einlesen des Knotenblocks. Letzter eingelesener Knoten:', &
           &'Error when reading the node block. Last read node:','','Knoten',knot(ik-1),'',0,'netan')

9611  format(/ 1X,'Fehler in der Netzdatei!' /                         		&
             & 1X,'Fehler beim Einlesen des Knotenblocks.'/                  	&
             & 1X,'Letzter eingelesener Knoten: ',i5)
      nstop=1
      return

9750  write(nerr,9751) ik-1
      call writeLogIntInt( 7, 'Knotenblock sollte mit "99999" beendet werden. Anzahl der eingelesenen Knoten', &
           & 'Node block should finish with "99999". Number of read nodes:','','',0,'ik',ik-1,'netan')

9751  format(/ 1X,'Fehler in der Netzdatei!' /                         		&
             & 1X,'Knotenblock sollte mit "99999" beendet werden.'/          	&
             & 1X,'Anzahl der eingelesenen Knoten: ',i5)
      nstop=1
      goto 222
      end
