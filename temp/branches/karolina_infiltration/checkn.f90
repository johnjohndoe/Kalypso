!     Last change:  JH   17 Jan 2007    7:48 am

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
      subroutine checkn(pathn,ilend,nin,namfn,xjah,xwahl2,        &
     			slash)
!c
!c
!c ***************************************************************************
!c      das programm checkn prueft, ob fuer eines der teilgebiete ein synthe-
!c      tisches niederschlagsereignis definiert ist.
!c      wenn ja, werden der bemessungsniederschlag, die niederschlagsdauer
!c      und die art der niederschlagsverteilung abgefragt.
!c
!c
!c *********************************************
!c      parameter (igwdim=6,npmax=10,ndmax=20)
      USE generic_LOGGER
      USE Units      
      parameter (npmax=10,ndmax=20)
!JH 09.07.2004 Auskommentiert, da nicht weiter verwendet
!JH      include 'include\param.cmn'

!c      implicit logical(a-z)

	real	xwahl2,xjah


      integer iz,xver,maxchar
      parameter (maxchar=120)
      character*(maxchar)  namfn
      character*80  pathn,text
	character*1	slash

      integer nip1,ilend,                                                 &
           ipd(npmax),iwahl,iwahl2,ipver,ilen,id,ju0nch,ju0gfu,nin

      real pn(npmax),                                                  &
          pd(npmax,ndmax),psum(npmax,ndmax),                          &
          pjsol,pdsol,pvsol
      common /syndat/ pjsol,pdsol,pvsol,ipver
            iz = 0

     	if (nin.gt.0) read(nin,*) namfn
       	ilen = ju0nch(namfn)
   !    	namfn=pathn(1:ilend)//'klima.dat'//slash//                    &
   !    		namfn(1:ilen)
    	namfn=namfn(1:ilen)
                 nip1 = ju0gfu()
            if(ilen.gt.80)then

               call writeLogString(7,'Pfad hat mehr als 80 Zeichen.','Path name has more than 80 characters.',&
                    & '','','','namfn',namfn,'checkn')

		write(nerr,10001)namfn
                call writeFooter()
		stop
            endif  
10001	format( 'tree-name fuer file hat mehr als 80 zeichen.',/       &
     		' filename: ',a/                                       &
     		' programm-abbruch.' )

            open (nip1,file=namfn,err=6001)

 1021       iz = iz+1
 1019          read (nip1,'(a)',end=1022,err=6010)text
	    if(text(1:1).eq.'/'.or.text(1:1).eq.'#')  goto 1019 
            read (text,*)pn(iz),ipd(iz) 
		if(abs(pn(iz)+99.).le.0.001) then
			goto 1022
		endif
            if(ipd(iz).eq.0) goto 1022
		do 303 id=1,ipd(iz)
			read (nip1,*,err=6010)pd(iz,id),psum(iz,id)
303		continue

 
        continue
            goto 1021

 1022       iz=iz-1
	   write (nres,'(a)')                                         &
     		'teilgebiet mit synth. niederschlag selektiert'

!ccccccccc
!c  automatische eingabe beim programmaufruf moeglich
!c
!c          call u0tnoa('gib haeufigkeitszahl -->')


        if(nin.gt.0) read(nin,*,err=1011)xjah
	do 2000 iwahl=1,iz
		if(abs(pn(iwahl)-xjah).lt.0.0001) then
			write(nres,901) pn(iwahl)
			goto 2001
		endif
2000	continue
!	if(abs(pn(iwahl)-xjah).lt.0.0001) then
!			write(nres,901) pn(iwahl)
!			goto 2001
!                        endif
	goto 1011

901	format(' simulation mit synthetischen niederschlaegen.',/     &
     		' ausgewaehlte jaehrlichkeit: ',f8.3)
2001	continue

      pjsol = pn(iwahl)

        if(nin.gt.0) read(nin,*,err=1012)xwahl2
	do 2002 iwahl2=1,ipd(iwahl)
		if(abs(pd(iwahl,iwahl2)-xwahl2).lt.0.0001) then
			write(nres,902) pd(iwahl,iwahl2)
			goto 2003
		endif
2002	continue
!		if(abs(pd(iwahl,iwahl2)-xwahl2).lt.0.0001) then
!			write(nres,902) pd(iwahl,iwahl2)
!			goto 2003
!		endif
	goto 1012
902	format(' ausgewaehlte dauer:     ',f6.2)
 
2003	continue

           if(iwahl2 .lt. 1 .or. iwahl2 .gt. ipd(iwahl)) goto 1011

            pdsol = pd(iwahl,iwahl2)
            pvsol = psum(iwahl,iwahl2)


        if(nin.gt.0) read (nin,'(i1)',err=1031)ipver
	xver=ipver
	write(nres,903)ipver
903	format(' ausgewaehlte verteilung:     ',i6)

            if(ipver .lt.1 .or. ipver .gt. 4) goto 1031

            close (nip1)
!c            goto 9000

      return


6001  write (nerr,6002) namfn(1:ilen)
6002  format(' Datei ',a,' mit den Daten zum synthetischen Niederschlag konnte nicht geoeffnet werden.')

      call writeLogString(7, 'Synthetische Niederschlagsdatei konnte nicht geoeffnet werden.',&
           & 'Synthetic precipitation file can not be opened',namfn(1:ilen),'','','','','checkn')
      call writeFooter()
      stop


6010  write (nerr,6012) namfn(1:ilen),iz-1
6012  format(' Formatfehler beim Einlesen der Daten zum synthetischen Niederschlag.'/&
            &' Filename: ',a,/       &
            &' Anzahl der eingelesenen Datenbloecke: ',i3)

      call writeLogIntInt(7, 'Formatfehler beim Einlesen der synthetischen Niederschlaege. Anzahl eingelesener Datenbloecke:',&
           & 'Format error when reading synthetic precipitation. Number of read blocks:',namfn(1:ilen),'',0,'iz-1',iz-1 ,'checkn')
      call writeFooter()
      stop


1011  write (nerr,6013) namfn(1:ilen),xjah
6013  format(' Fehler beim Einlesen der Daten zum synthetischen Niederschlag.'/&
           & ' Filename: ',a,/         &
           & ' Eingelesene Jaehrlichkeit nicht im Datenfile vorhanden.'/ &
           & ' Eingelesene Jaehrlichkeit: ',f6.2)

      call writeLogIntReal(7, 'Fehler beim Einlesen synthetischen Niederschlaege. Jaehrlichkeit nicht vorhanden.', &
           & 'Format error when reading synthetic precipitation. Annuality not available.',namfn(1:ilen),'',0,'xjah',xjah,'checkn')
      call writeFooter()
      stop


1012  write (nerr,6014) namfn(1:ilen),xwahl2
6014  format(' Fehler beim Einlesen der Daten zum synthetischen Niederschlag.'/&
           & ' Filename: ',a,/         &
           & ' Eingelesene Niederschlagdauer nicht im Datenfile vorhanden.'/ &
           & ' Eingelesene Dauer: ',f6.2)

      call writeLogIntReal(7, 'Fehler beim Einlesen der synthetischen Niederschlaege. Dauerstufe nicht vorhanden.', &
           & 'Error when reading synthetic precipitation. Duration not available.',namfn(1:ilen),'',0,'xwahl2',xwahl2,'checkn')
      call writeFooter()
      stop


1031  write (nerr,6015) xver
6015  format(' Fehler bei Niederschlagsverteilung fuer synthetischen Niederschlag.',/&
            &' Sinnvolle Verteilungen: 1-4.',/                             &
            &' Eingelesene Verteilung: ',i3)


      call writeLogIntInt(7,'Niederschlagsverteilung fuer synthetischen Niederschlag nicht korrekt. Sinnvolle Verteilungen: 1-4',&
           & 'Error with precipitation distribution for synthetic precipitation. Acceptable distribution: 1-4',&
           & '','',0,'xver',xver ,'checkn')
      call writeFooter()
      stop

      end

