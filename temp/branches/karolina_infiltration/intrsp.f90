!     Last change:  JH   17 Jan 2007    8:00 am

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

      subroutine intrsp(istrng)


!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!    31.01.06   JH                      Formatänderung der Kalinin-Miljukov Parameter (nicht
!					abwärts kompatibel)!

!***************************************BESCHREIBUNG********************************************
!JH 31.01.2005
!     Einleseroutine der Gerinnedatei *.ger
!
!     Formatbeispiele eines KM-Stranges:
!
!     	Beispiel 1: Gerinne mit Vorlandüberströmung (Bordvolller Abfluss = 2.867666)
!	1000
!	1
!	2.489974    0.085388   20.807590    0.000000    0.000000 1.00
!     	2.679062    0.097459   19.804899    0.000000    0.000000 1.00
!     	2.867666    0.081755   19.768904    0.000000    0.000000 1.00
!     	3.056713    0.575013   17.151905    0.867132   29.999998 0.83
!     	3.245364    8.630986   14.676947    0.629331   29.999998 0.65

!     	Beispiel 2: Gerinne ohne Vorlandüberströmung (Bordvoller Abfluss wird nicht erreicht)
!     	1000
!     	1
!     	2.489974    0.085388   20.807590    0.000000    0.000000 1.00
!     	2.679062    0.097459   19.804899    0.000000    0.000000 1.00
!     	2.867666    0.081755   19.768904    0.000000    0.000000 1.00
!     	3.056713    0.575013   17.151905    0.000000    0.000000 1.00
!     	3.245364    8.630986   14.676947    0.000000    0.000000 1.00

!     Formatbeispiele eines virtuellen Stranges:
!     	1000
!     	0

!     Formatbeispiele eines Rückhaltebecken Stranges:
!     	1000
!     	2
!***************************************EIN-/AUSGABE********************************************
!JH 31.01.2005
!     Eingabe:
!		istrng		Strangnummer
!
!     Ausgabe:  keine direkten Ausgabeparameter vorhanden
!               Ausgabeparameter in ger.cmn (Gerinneparameter)

!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      implicit none
!     implicit logical(a-z)
      INTEGER,parameter     :: ndim	= 5
      include 'include\ger.cmn' 
      include 'include\filnm.cmn'


      CHARACTER (LEN=80)     :: namges
      CHARACTER (LEN=80)     :: text

      INTEGER 		:: istrng               ! Strangnummer des gesuchten Stranges
      INTEGER 		:: nger                 ! Kanalnummer der Gerinnedatei
      INTEGER 		:: i1,i2
      INTEGER 		:: inum
      INTEGER 		:: ju0gfu
      INTEGER 		:: ierr
      INTEGER 		:: ilen
      INTEGER 		:: ianz

      DO i1=1,ndim
       qrk(i1) = 0.
       c(i1) = 0.
       DO i2=1,2
        rk(i2,i1) = 0.
        rn(i2,i1) = 0.
       END DO
      END DO
      qfmax = 0.
      iart = 0
      inum = 0
      nger = 0
      ilen = 0
      ierr = 0

!******************************************ANWEISUNGEN******************************************

      namges(1:80)=pathn(1:ilend)
      namges(ilend+1:ilend+9)='inp.dat/'
      ilen = ilend + 8
      namges(ilen+1:ilen+ilenb)=namger
      ilen=ilen+ilenb

      namges(ilen+1:ilen+4)='.ger'
      ilen = ilen+4

      ! Öffnen der Gerinnedatei
      nger = ju0gfu()
      open (nger,iostat=ierr,err=999,file=namges)

      ianz=0
      strangschleife: DO

    ianz=ianz+1
11    text=''
      read(nger,'(a80)',end=9000,err=3001) text
      if(text(1:1).eq.'/'.or.text(1:1).eq.'x' .or. text(1:1).eq.'X') goto 11

      read(text,*,err=3001) inum
      if (inum.eq.0) goto 9100

      read(nger,*,err=3002) iart

      IF (iart.EQ.1) THEN
!JH,31.01.2006: Der Aufteilungsfaktor wird Abflussabhängig angegeben,
!               der bordvolle Abfluss wird vorerst immer als der 3. definiert.
!     read(nger,*,err=3003) qfmax,c
         DO i1=1,ndim
          read(nger,*,err=3004) qrk(i1),rk(1,i1),rn(1,i1),rk(2,i1),rn(2,i1),c(i1)
         END DO
         qfmax=qrk(3)
      END IF
      ! Abbruchkriterium: der gesuchte Strang wurde eingelesen
      IF (inum.EQ.istrng) EXIT strangschleife
      END DO strangschleife

      close (nger)
!      IF (iart.EQ.1) THEN
!         do i = 1,ndim
         !TODO: prüfen, ob und wozu diese Abfrage notwendig wäre (JH).
!          if (rk(2,i).lt.1.e-6) rk(2,i) = rk(1,i)
!         END DO
!      END IF
      return


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
9000  write(nerr,9900) istrng
9900  FORMAT(/ 1X, 'Strang ',i5,' wurde nicht in der Gerinnedatei gefunden.' 	/&
             & 1X, 'Ergaenzen Sie den Strang in der Gerinnedatei xx.ger oder '  /&
             & 1X, 'korrigieren Sie obere Knotennummer des Strangs in der'      /&
             & 1X, 'Netzdatei, wenn es sich um einen Anfangsstrang handeln soll'/&
             & 1X, '(Anfangsknoten>9000)')
      write(*,9300)
      call writeLogIntInt(7, 'Strang wurde nicht in der Gerinnedatei gefunden.','Strand was not found in the channel data.', &
           &'','Strang',istrng ,'',0,'intrsp')
      close (nger)
      call writeFooter()
      stop

9100  write(nerr,9101) ianz
      write(*,9300)
9101  format(' fehler in der gerinnedatei! (subroutine intrsp)'	/&
        ' ein string mit nummer 0 wurde eingelesen.' 		/&
        ' eingelesene stranganzahl: ',i4,			/&
        ' moeglicher fehler:'/' leerzeile am dateiende!')
      call writeLogIntInt(7,'Fehlerhafter Strang wurde eingelesen. Moeglicher Fehler: Leerzeile am Dateiende!',&
           &'Error while reading strand data. Possible error: blank line at the end file ',namges(1:ilen),'',0,&
           &'Eingelesene Stranganzahl (ianz)',ianz,'intrsp')
      call writeFooter()
      stop

999   WRITE(nerr,9991) namges(1:ilen)
9991  FORMAT(/ 1X, 'Gerinnedatei mit dem Namen '	/&
             & 1X, a 					/&
             & 1X, 'konnte nicht geoeffnet werden.')
      call writeLogString(7,'Gerinnedatei konnte nicht geoeffnet werden', 'Channel data can not be opened', namges(1:ilen), &
           & '','','','','intrsp')
      call writeFooter()
      STOP

3001  write(nerr,3101) inum
3101  FORMAT(/ 1X, 'Fehler beim einlesen der Gerinnedatei.'	/&
             & 1X, 'Fehler beim Lesen der Strangnummer.'	/&
             / 1X, 'Letzter korrekt eingelesener Gerinneblock: ',i5)
      write(*,9300)
      call writeLogIntInt(7,'Fehler beim Lesen der Strangdaten (Strangnummer). Letzter korrekt eingelesener Gerinneblock:', &
           & 'Error when reading the strand data (number). Last reading made correctly in channel block was:', namges(1:ilen),&
           & 'Strang',inum,'',0,'intrsp' )
      call writeFooter()
      stop

3002  write(nerr,3102) inum
3102  FORMAT(/ 1X, 'Fehler beim einlesen der Gerinnedatei.'	/&
             & 1X, 'Fehler beim Lesen der Gerinneart'		/&
             & 1X, 'Strangnummer: ',i5)
      write(*,9300)
      call writeLogIntInt(7,'Fehler beim Lesen der Gerinneart.','Error when reading the channel type.','','Strangnummer',&
           & inum,'',0,'intrsp')
      call writeFooter()
      stop
3004  write(nerr,3104) inum, i1
3104  FORMAT(/ 1X,' Fehler beim einlesen der Gerinnedatei.'		/&
             & 1X,' Fehler beim Lesen der KM-Parameter.'        	/&
             & 1X,' Strangnummer: ',i5,' im ',i2, 'Parametersatz')
      WRITE(*,9300)
      call writeLogIntInt(7,'Fehler beim Lesen der Kalinin-Miljukov Parameter.','Error in reading the Kalinin-Miljukov parameter.',&
           & '','Strang',inum,'Abflussdaten (i1)',i1,'intrsp')
      call writeFooter()
      STOP

9300  FORMAT(/ 1X, 'Fehler in der Gerinnedatei!' /&
             / 1X, 'Weitere Hinweise im Errorfile output.err')
      end
