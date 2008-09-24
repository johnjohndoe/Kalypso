!     Last change:  JH    9 Mar 2007   10:31 am

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
! Dipl.-Ing. Jessica Huebsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Huebsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************

	subroutine inp_hydro(namhyd,ntg,                   &
               flaech1,flaech2,flaech3,		               &
               anzelem,kenelem,anzbod,anznutz,                 &
               kennbod,kennutz,nutzprz,                        &
               m_perkm,m_f1gws,m_vers,                  &
               n_hydid,m_hydTyp,m_nhyd,nhydtyp)

!**************************************ÄNDERUNGEN***********************************************
!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!      ?        ?			Aenderungen umfassen nur Verbesserungen
!                         		bei der Fehlerausgabe.
!
!      ?        ?			uebernahme der bodenparameter aus datei boden_n.par
!
!    26.11.05   CK 	                Umstellung auf Fortran95, und formatfreies einlesen
!                                       aller Parameter einfuegen des Parameters m_hydTyp

!***************************************BESCHREIBUNG********************************************
!JH Die Subroutine steuert das Einlesen der Hydrotopdatei *.hyd.
USE generic_LOGGER
USE Units
USE generic_LOG_FILE
USE generic_NAConstants
IMPLICIT NONE

include	'include\param.cmn'
!******************************************VARIABLEN********************************************

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      EINGABE
!c      -------
!c      nhydtyp                         = hydrotoptyp (nur fuer ausgabe)
!c					  hier fuer Fehlerhandling
!c      nt                              = Nummers des zu berechnenden TG
!c      namhyd                          = Name der Hydrotopdatei
!c
INTEGER, INTENT(IN):: nhydtyp
INTEGER, INTENT(IN) ::  ntg
CHARACTER(LEN=80), INTENT(IN):: namhyd

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      AUSGABE
!c      -------
!c      kennutz                         = Kennung der Nutzung
!c      kennbod                         = Kennung des Bodens
!c      m_f1gws                         = Zuflussanteil Grundwasser
!c      m_perkm                         = max. Perkulation im Hydrotop
!c                                        bezogen auf letzte Schicht
!c      m_vers                          = Versiegelungsgrad des Hydrotops
!c      nutzprz                         = Flaechenverhaeltnis, nat.Hyd/nat.TG
!c      anznutz                         = Anzahl der eingelesenen Nutzungsarten
!c      anzbod                          = Anzahl der eingelesenen Bodenarten
!c      anzelem                         = Anzahl der Hydrotope im TG
!c      kennelem                        = kennelem(1,hyd-nummer)=bodenart-nr
!c                                        kennelem(2,hyd-nummer)=nutzung-nr
!c      m_hydTyp                        = Hydrotoptyp (0,1 oder 2) siehe gebiet.f90
!c      m_nhyd                          = natürliche Flaeche des Hydrotops
!c
CHARACTER(LEN=10), INTENT(OUT) :: kennutz(idimnutz2)
CHARACTER(LEN=10), INTENT(OUT) :: kennbod(idimnutz2)
REAL,INTENT(OUT):: m_f1gws(2*idimnutz)
REAL,INTENT(OUT):: m_perkm(2*idimnutz)
REAL,INTENT(OUT):: m_vers(2*idimnutz)
REAL, INTENT(OUT):: nutzprz(2*idimnutz),m_nhyd(2*idimnutz)
INTEGER, INTENT(OUT):: anznutz,anzbod,anzelem
INTEGER, INTENT(OUT) :: kenelem(2,2*idimnutz),m_hydTyp(2*idimnutz)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      LOKALE VARIABELN
!c      -----------------
!c 	n_hydid 			= Hydrotop id im Teilgebiet
!c	xflaechhyd                      = Summe der nat.Hyd-Flaeche
!c 	xfak		     		= Korrektur fuer Summe der Hydrotopflaechen
!c				     	  zur Teilgebietsgesamtflaeche
!c      istat                  		= Error status beim einlesen
!c      juOgfu/nbo,i1,13   		= Kanalnummer Hydrotopdatei,Loop-Zaehler,Loop-Zaehler
!c      text, text10                   	= eingelesene Zeile, teil der eingelesenen Zeile
!c      num,typanz                    	= eingelesene TG-Nummer, Anzahl der Hydrotpe im TG
!c	flaech1,flache2,flache3,flache4 = vers.Flaeche-TG, nat.Flaeche-TG, ges.Flaeche-TG, nat.Flaeche-Hyd
!c      nutz                            = eingelesener Nutzungsschluessel des Hydrotops
!c      boden                           = eingelesener Bodentyp des Hydrotops (boden.dat)
!c      DEBUG                           = ein/aus fuer Debug info der Routine
!c
INTEGER	:: n_hydid(2*idimnutz)
REAL	:: xflaechhyd,xfak
INTEGER	:: counter
INTEGER :: istat
INTEGER :: ju0gfu,nbo,i1, i3
CHARACTER(LEN=10) ::text10
CHARACTER(LEN=150):: text
INTEGER	:: num,typanz,ndebug
REAL	:: flaech1,flaech2,flaech3,flaech4
CHARACTER(LEN=10):: nutz(2*idimnutz)
CHARACTER(LEN=10):: boden(2*idimnutz)

!******************************************ANWEISUNGEN******************************************


1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: inp_hydro.f90                  ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (inp_hydro.f90)           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

ndebug = -1
 IF ( DEBUG ) THEN
     CALL getLogFile( ndebug )
     WRITE (ndebug,1)
 END IF

nbo=ju0gfu()
typanz=0
xflaechhyd=0.
m_hydTyp =0


OPEN (nbo,IOSTAT=istat,ERR=996,STATUS='old',FILE=namhyd)                           ! Oeffnen *.hyd
READ(nbo,'(a)',end=9901) text                                                     ! 1.Zeile - Formatauswahl
text10=text(1:10)
CALL lcase(text10)

read_file: DO
	     READ(nbo,'(a)',IOSTAT=istat) text
             IF( istat == -1 ) THEN
             	CLOSE(nbo)
     	     	RETURN
            ! 1.Zeile des Datenblocks oder Kommentar
      	     ELSE IF(text(1:1).eq.'/'.or.text(1:1).eq.'#') THEN
       		CYCLE read_file
             ELSE IF(typanz > 2*idimnutz) THEN
       		GOTO 9300
      	     END IF
	    ! liest die 1. Zeile des Datenblock
      	     READ(text(1:120),*,IOSTAT=istat)num,typanz,flaech1,flaech2,flaech3
      	     IF (num == ntg ) THEN
	        DO  i1=1,typanz
                    ! normales Format (verwendte von TUHH)
             	    READ(nbo, *, IOSTAT=istat) flaech4, nutz(i1), boden(i1), m_perkm(i1), m_f1gws(i1), &
                                              & n_hydid(i1),m_vers(i1),m_hydTyp(i1)
                    IF (istat/=0) THEN
                      WRITE (*,1000) num, n_hydid(i1)
                      1000 FORMAT (/1X, 'Fehler beim Einlesen der Hydrotopdatei:', /, &
                            & 1X, 'Gebiet:     ', I5, /, &
                            & 1X, 'HydrotopID: ', I5)
                    call writeLogIntInt( 7, 'Fehler beim Einlesen der Hydrotopdatei!','Error while reading hydrotop data file!',&
                         & namhyd,'Teilgebiet',num,'n_hydid(i1)',n_hydid(i1),'inp_hydro')
                    END IF

                    IF ( DEBUG ) THEN
                       WRITE (ndebug,*)  flaech4, nutz(i1), boden(i1), m_perkm(i1), m_f1gws(i1), &
                                              n_hydid(i1),m_vers(i1),m_hydTyp(i1)
                    END IF

                    nutzprz(i1)=flaech4/flaech2
     	            xflaechhyd=xflaechhyd+flaech4
             	    m_nhyd(i1) = flaech4
	     	    CALL lcase(nutz(i1))
 	            CALL moveblank(nutz(i1))

             	    CALL lcase(boden(i1))
   	     	    CALL moveblank(boden(i1))
	            IF (n_hydid(i1).gt.idimhyd.and.nhydtyp.gt.0) goto 6006
	        END DO ! ende des Hydrotpdatenblocks im Teilgebiet (ntg)

!------------------------------------------------------------------------------
! hydrotopflaeche wird angepasst, gesamtflaeche wird uebernommen nutzprz wird korrigiert
! Behandelt den Fall wenn nat. Flaeche Summe Hydrotope nicht nat. Flaeche TG entspricht
	       IF (ABS(xflaechhyd/flaech2-1.).gt. .00001) THEN
	  	  xfak=flaech2/xflaechhyd
		  DO i1=1,typanz
   	 	    nutzprz(i1)=nutzprz(i1)*xfak
		  END DO
		  WRITE(nerr,9800)flaech2,xflaechhyd
                   IF(ABS(xflaechhyd/flaech2-1.).gt. .01) THEN ! Nur Abweichungen > 1% werden angezeigt! Fehlermeldung ist sonst irreführend, wenn Anpassung nur sehr klein (Check: Warum sind die Flächen nicht gleich in Java - gleiche Berechnungsgrundlage Hydrotopgeometrien???)
                  call writeLogIntRealReal(6, &
                       &'Natuerliche Teilgebietsflaeche entspricht nicht der Summe der natuerlichen Hydrotopflaechen!',&
                       &'Natural sub catchments area is not equal to sum of natural hydrotop areas! ',&
                       & namhyd,'Teilgebiet',ntg,'flaech2',flaech2,'xflaechhyd',xflaechhyd,'inp_hydro')
                   ENDIF
	       ENDIF

	       anzelem=typanz

	       IF( anzelem < 1) THEN
               write(nerr,9036)namhyd,ntg

               call writeLogIntInt(7,'Fuer ein Teilgebiet sind keine Hydrotopinformationen vorhanden!', &
                    &'There is no hydrotop infomation for a sub catchment!',namhyd,'Teilgebiet',ntg,'',0,'inp_hydro')


               istat=1
               call writeFooter()
               STOP 'inp_hydro -> anzahl elemente'
               ENDIF
               WRITE(nres,'(/2a,i6,/)')'Subroutine INP_HYDRO:',            &
                              '      Anzahl Hydrotope im TG: ',anzelem
	       anzbod=0
	       anznutz=0
	       DO i1=1,anzelem
       	    	 DO i3=1,anzbod
	           IF(kennbod(i3) == boden(i1)) THEN
	       	      kenelem(1,i1)=i3
                      IF (DEBUG) THEN
	                  write (ndebug,*) 'i1=',i1,' i3=',i3,' (bod-loop)kennelem(1,',i1,')=', kenelem(1,i1)
                      END IF
	   	      goto 204
	       	   END IF
		 END DO
      		 anzbod=anzbod+1
	         IF (anzbod > idimnutz2) GOTO 9027
	         kennbod(anzbod)=boden(i1)
 	         kenelem(1,i1)=anzbod
                 IF (DEBUG) THEN
                    write (ndebug,*) '(elem-loop-1)kennbod(',anzbod,')=',boden(i1),' kenelem(1,',i1,')=',kenelem(1,i1)
                 END IF
	204      continue
       	         DO i3=1,anznutz
		   IF(kennutz(i3).eq.nutz(i1)) THEN
	   	      kenelem(2,i1)=i3
                      IF (DEBUG) THEN
                          write (ndebug,*) 'i1=',i1,' i3=',i3,' (nutz-loop)kennelem(2,',i1,')=', kenelem(2,i1)
                      END IF
	   	      goto 206
		   END IF
	      	 END DO
	      	 anznutz=anznutz+1
	      	 if(anznutz.gt.idimnutz2) GOTO 9028
	         kennutz(anznutz)=nutz(i1)
       		 kenelem(2,i1)=anznutz
                 IF (DEBUG) THEN
                     write (ndebug,*) '(elem-loop-2)kennutz(',anznutz,')=',nutz(i1),' kenelem(2,',i1,')=',kenelem(2,i1)
                 END IF
	206      continue
	       END DO !ende anzelem

	       close (nbo)

      	       RETURN
             ELSE
               DO counter=1,typanz
                  READ(nbo,'(a)',IOSTAT=istat) text
               	  IF ( istat /= 0 ) THEN
               	     CLOSE(nbo)
	             WRITE (*,1001) num, text
                     1001 FORMAT (/1X, 'Fehler beim Einlesen der Hydrotopdatei:', /, &
                            & 1X, 'letztes gelesenes Gebiet:     ', I5, /, &
                            & 1X, 'letzte gelesene Zeile: ', '(a)' )
                     WRITE (nerr,1001) num, text

                     call writeLogIntInt(7, 'Fehler beim Einlesen einer Datei!', 'Error while reading a file!',&
                          & namhyd,'Teilgebiet',num,'',0,'inp_hydro')

                     call writeFooter()
                     STOP
                  ENDIF
               END DO
	     END IF !ende (num=tg)

	   END DO read_file

IF ( DEBUG ) THEN
    WRITE (ndebug,2)
END IF

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

996   write(nerr,906)namhyd
906   format(/,'xxx Hydrotop-Datenfile nicht gefunden!',/        &
               'xxx Filename: ',a)

      call writeLogString(7, 'Hydrotop-Datenfile nicht vorhanden!','Hydrotop-file not found!',namhyd,&
           & '','','','','inp_hydro')

      call writeFooter()
      stop

9901  write(nerr,9902)ntg,namhyd
9902  format(/,'xxx Keine Daten zu Hydrotopen fuer Teilgebiet gefunden!' &
               ,/'xxx Teilgebiet:  ',i4,/                                 &
               'xxx Filename: ',a)

      call writeLogIntInt(7, 'Keine Daten zu Hydrotopen fuer ein Teilgebiet gefunden!',&
      & 'No hydrotop´s data was found for a sub catchment!','','Teilgebiet',ntg,'',0,'inp_hydro')

      call writeFooter()
      stop

!ccccccccccccccccc
9036  format(/,'xxx Keine Hydrotope in der Hydrotop-Datei ',a,        &
               'fuer folgendes Teilgebiet definiert: ',i4)

9027  write(nerr,9037) idimnutz2
9037  format(/,'xxx max. ',i2,'bodenarten pro teilgebiet ',         &
               'sind zugelassen.'/                                    &
               'xxx erhoehe parameter idimnutz2 in param.cmn! ')

      call writeLogIntInt(7,'Max. Anzahl zu verwendender Bodenarten pro Teilgebiet ueberschritten!',&
           & 'Maximum number of soil type per sub catchment reached!','','',0,'idimnutz2',idimnutz2,'inp_hydro')

      istat=1
      goto 9000
!ccccccccccccccccc
9028  write(nerr,9038) idimnutz2
9038  format(/,'xxx max. ',i2,'nutzungsarten pro teilgebiet ',      &
               'sind zugelassen.'/                                    &
               'xxx erhoehe parameter idimnutz2 in param.cmn! ')

      call writeLogIntInt(7,'Max. Anzahl Nutzungen pro Teilgebiet ueberschritten!',&
            & 'Maximum number of land use types per sub catchment reached!','','',0,'idimnutz2',idimnutz2,'inp_hydro')

      istat=1
      goto 9000
!ccccccccccccccccc
9300  write(nerr,9338) idimnutz
9338  format(/,'xxx Max. ',i2,' Hydrotope pro Teilgebiet sind zugelassen.'/   &
               'xxx Erhoehe ggf. Parameter idimnutz in param.cmn! ')

      call writeLogIntInt(7,'Max. Anzahl an Hydrotopen pro Teilgebiet ueberschritten!',&
           & 'Maximum number of hydrotops per sub catchment reached!','','',0,'idimnutz',idimnutz,'inp_hydro')

      istat=1
      goto 9000

!ccccccccccccccccc
6006  write(nerr,6016)idimhyd
6016  format(/,'xxx hyd-id uebersteigt vorgesehene anzahl!',/            &
               'xxx erhoehe parameter idimhyd in param.cmn!',/             &
               'xxx begrenzung der hydrotopanzahl fuer hydrot.ausgabe!',/   &
               'xxx falls hydot.ausgabe unterdrueckt --> kein abbruch!',/  &
               'xxx idimhyd = ',i8)

      call writeLogIntInt(7,'Hyd-id uebersteigt vorgesehene Anzahl! Falls hyd. Ausgabe unterdrueckt --> kein Abbruch!',&
           & 'hyd-id exceeds intended number! If hydrotop output suppressed --> Program does not abort!',&
           & '','',0,'idimhyd',idimhyd,'inp_hydro')

      istat=1
      goto 9000

9000  call writeFooter()
      stop

!ccccccccccccccccc
9800  format(/'Subroutine INP_HYDRO: natuerliche Teilgebietflaeche',    &
               ' und Summe der natuerlichen Flaechenanteile der',         &
               ' einzelnen Hydrotope stimmen nicht ueberein.'/            &
               ' Natuerliche Teilgebietsflaeche:         ',f12.1,/        &
               ' Summe der natuerlichen Flaechenanteile der Hydrotope:',   &
               f12.1,/ 'Hydrotopflaechen werden angepasst!')

      end
