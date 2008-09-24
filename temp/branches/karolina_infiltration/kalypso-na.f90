!     Last change:  CB    4 Sep 2008   11:31 am
      program kalypso_na

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


!-----------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------
!     KALYPSO-NA
!     Das Niederschlag-Abfluss-Modell Kalypso-NA ist ein Softwarepaket zur Simulation des
!     vollstaendigen, landgebundenen Teils der globalen Wasserbilanz. Hierbei werden die
!     Teilprozesse Schneespeicherung, Evapotranspiration, Bodenwasserspeicherung,
!     Grundwasserneubildung, Oberflaechenabfluss, Interflow, Grundwasserabfluss und
!     Wellentransport im Gerinne des Wasserkreislaufes berücksichtigt. Bei dem Modell handelt
!     es sich um ein konzeptionelles, deterministisches, nicht lineares, detailliert
!     hydrologisches Modell.
!
!     authors: - Arbeitsbereich Wasserbau, Technischen Universitaet Hamburg Harburg
!              - Björnsen Beratende Ingenieure GmbH
!-----------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------


!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     18.08.02 	Alexander Goetze (AG)   Neue Version erzeugt als Ausgabe-Dateien, Dateien mit
!     					der Endung .dat. Das neue Grafiktool benoetigt diese
!					Endung. In der alten Version wurden die verschiedenen
!					Ausgabedateien ueber die Endungen unterschieden. Sie
!					hatten alle das Anfangsdatum des Simulationszeitraumes
!					als Namen. Die alten Endungen werden in der neuen
!					Version als Name verwendet. Die neuen Endungen sind
!					jetzt alle einheitlich .dat. Lediglich die zweite
!					Bilanzdatei mit der Endung .txt behaelt ihre Endung
!					weiterhin. Ihr Name lautet jetzt bil.txt

!     16.08.02 	Stephan Kraessig (SK)   Neue Version initialisiert nanzanf fuer kzsimulation
!					Zeile 1345

!     28.02.03  Stephan Kraessig (SK)   Neue Version ist Jahr 2000 faehig und kann bei Angabe
!     04.03.03  Prof. Pasche (EP)       von 1000001 als Anfangsdatum in der *.konfig Datei für
!					jeden gerechneten Zeitschritt der Langzeitsimulation
!					Anfangswerte für die Kurzzeitsimulation erzeugen.

!     05.03.03  Stephan Kraessig (SK)	Der Inhalt des lzsim Ordners kann sehr groß werden.
!		Andreas v Dömming (AvD)	Daher kann der erzeugte lzsim Ordner ausgelagert und
!					in der *.konfig Datei am Ende der entsprechende Pfad
!					angegeben werden mit lzpath=. Liegt der lzsim Ordner
!					unter c:\daten\lzsim, ist z.B. lzpath=c:\daten
!					anzugeben.

!     28.10.03  Stephan Kraessig (SK)   call checkn auskommentiert, um sie in Routine input
!		Claudia Brüning (CB)    aufzurufen.
!					Ziel: Synthetische Niederschlaege sollen für jedes
!					Teilgebiet eingelesen werden.

!     19.07.04  Jessica Hübsch          Parameter idimanf (Anzahl der möglichen
!					Anfangsbedingungen) auf 100 erhöht. Betrifft param.cmn

!     24.09.04  Jessica Hübsch (JH)     Anfangswerte können auch aus der Kurzzeitsimulation
!     30.09.04                          zu einer beliebigen Stunde geschrieben werden. Hierzu
!					wurde das Format der *.konfig sowie der *.lzg und *.lzs
!					geaendert, da hier zusaetzlich zum Datum der Anfangswerte
!					jetzt auch die Stunden hinzugefügt wurden. Hinweis: die
!					Änderungen beziehen sich nur auf das Schreiben der
!					Anfangswerte zu bestimmten Daten und deckt für die
!					Kurzzeitsimulation nicht die Möglichkeit der
!					fortlaufenden Speicherung der Anfangswerte ab.

!    11.11.2004 Stefan Kraessig (SK) 	Für Ausführung mehrerer Berechnungen hintereinander
!		Jessica Huebsch (JH)    wird die Bezeichnung der Ausgabedateien wieder mit dem
!					Anfangsdatum versehen. Die Endung .dat bleibt erhalten,
!					die Änderungen von AG 18.08.02 werden teilweise
!					zurückgenommen. Außerdem werden für einige Dateien die
!					Kopfzeilen im Fall von synthetischen Niederschlaegen
!					angepasst.

!    26.11.2004 Jessica Huebsch (JH)	Ausgabe der UH´s fuer das Gerinne (siehe auch
!					gerinne.f90) als Ergebnisdatei hinzugefuegt. Sollte
!					evtl. als optionale Ausgabe in *.konfig aufgenommen
!                                       werden.

!    28.09.2005 Jessica Huebsch (JH)	Behebung eines Bugs in der Interzeptionsberechnung der
!		Andreas v Dömming(AvD)  Hydrotope. Es wird jetzt der Anfangsinhalt und der
!					Inhalt des vorherigen Zeitschrittes für jedes Hydrotop
!                                       richtig übernommen.


!******************************************VARIABLEN********************************************
      USE Units
      USE generic_LOG_FILE
      USE generic_NAConstants
      USE generic_LOGGER
      implicit logical(a-z)
      include 'include\param.cmn'




      include 'include.all'
      LOGICAL ::        lopen
      character*80 		datnam,datqgs,namnal,namfan,nameptal
      character*20 		dummy
      character*80 		tkenn,datnamhyd
!JH 13.10.04 char_idatsa in Common-Block nad.cmn uebernommen
!     character*8  		char_idatsa
      character*1  		iant
      character*1  		pns

      integer 			l3,nueb,nsv,nt,nquh,ngeruh
      integer 			npin,nqa,nqg,nqb,ngw,ngws,nh,nkap,nf
      integer 			nqif,nqgg,ju0gfu,ierr,ndebug
      integer 			iko,iku,ntg,izsim,i,i1,i2,i3,i4,i5,iz,izykl,izaehl,itemp,itag,imona,ijahr
      integer 			ntdif,nqvs,nmrqa,ikziel,ikzueb,idatum,iflag_lzsim
      integer 			knout(ikdim),iknmax,is,ik
      integer 			idif1,idif,iend,ilen,iken,itgmax
      integer 			ilenda,ju0nch,inlen,ipver
      integer 			ikzv1,ikt,nbof,nqtg1,nqtg2,nstop
      integer 			ninf,nvep,nprk,nhyd
      integer 			nin,nfall
      integer 			nanzanf1
      integer                   imic,imac,inft,nvgp
      INTEGER 			int_4
      character*80 		namntz,datfalln
      character*120 		datfall

      real    			qsum,qsum2,qzsum
      integer 			nsv_e,nsv_p,nsv_b,nsh

      real    			dt,pjsol,pdsol,qz(idimt),pvsol,xqm
      REAL    			qzuf(idimt)
      common /syndat/ pjsol,pdsol,pvsol,ipver


! anfangsinhalte = endwerte zyklus iz-1

      real    			nw_bianf(jdim,2*idimnutz)
      real    			nw_boanf(jdim,2*idimnutz,maxlay)
      real    			xbil(21),xbil2(21),xbilsp(20)

!cccccccccccccc
!
!	anfangswerte lzsim-datein
!
      real    			xanf(idimanf)
!     real*8  			x1
      integer 			nanf(idimanf),nanzanf,ianf,nanzanff

! JH 24.09.2004 Anfangsstunde für KZ-sim Anfangswerte
      INTEGER ::      		nanfstd(idimanf),ianfkz(idimanf)

!       5.3.2003 Kraessig/v.Doemming
!       es besteht die moeglichkeit das lzsim-verzeichnis ausserhalb des projektordners zu lagern
!       lzpath wird dann anstelle des projektpfades verwendet
!       lzpath wird in start/*.konfig in der letzten zeile gesetzt, Beispiel:
!       lzpath=c:\temp
      character*87 		lzpath
      CHARACTER*87 		lzpdum

!cccccccccccccc
!
!	statistik mittelwerte
!

      real    			qmnq(ikdim),qmhq(ikdim),qmax,qmin
      real    			qkillm(12)
      integer 			nmq,ntag
      real    			xmq(ikdim),xnnq(ikdim)
      real    			qmm(12),qmms(ikdim,12)
      real    			svmm(12),svmmw(12),svend
      integer 			anzm(12),anzdm(12),xsum


!ccccccccccccccccccccccccccccccc
!
!	anfang- und endzeitpunkt, 2000 jahr faehig
!
      integer 			naja2000,neja2000,naja20,neja20,idatsa20,idatse20
      integer 			idatselz
      INTEGER 			netg1,nemo1,neja1,nemo20,netg20
      INTEGER 			nemn,namn

!ccccccccccccc
!
!	ausgabe hydrotope
!
      real    			m_hyd(idimhyd,7),fakls,zahl
      integer 			idnr
      real    			m_bil(jdim,20),m_flaech(jdim,4)

      character*80		text,dathyd
!     integer 			ilhyd,nhydp,nhydm,maxhyd
      integer 			ilhyd,nhydtyp,ivor,ispk,idifbil,izbil
      character*1		komma
      character*34		fmt

!cccccccccccccccccccc
!
!    checkn:  synthet. niederschlag
!
      real			xwahl2,xjah
!     integer			ipver
      INTEGER 			maxchar
      parameter (maxchar=120)
      character*(maxchar)	namfn


      logical 			dosflag
      character*1 		slash


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
!
! anfangsabfluss gerinne ../lzsim/****.lzg
!
      character*80 		text0
      integer			lt,nanff,iok
      real			r,qg_lz


!JH 24.09.2004 Variablen Anfagswerte Kz-Sim
      INTEGER ::		sjahr,smona,stag,sdatum
      REAL    ::      		sstd
      INTEGER ::      		ilenlzsim
      INTEGER ::      		isstd
!  Aufteilungsfaktor Parallelspeicherkaskade
!SK/EP 28.02.03
!c beta wird schon im COMMONBLOCK DER SUBROUTINE ISOV deklariert.
!     REAL 			beta

!  Festlegung des Simulationszeitraumes und des Zeitschrittes
!  Über Eingabefile

!ccc
! ivor Vorlaufjahr fuer Mittelwertbildung Jahreswerte Langzeitsimulation
! kann z.B. auf 1 gesetzt werden.
INTEGER		:: ismax

      nerrlog = 0
      nerr = 0
      nres = 0

!******************************************ANWEISUNGEN******************************************
!JH Programmaufruf, Versionsnummer, Autoren

      WRITE (*, 600)
600   FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****      HAMBURG UNIVERSITY OF TECHNOLOGY       ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'**** Department of River and Coastal Engineering ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****            K A L Y P S O - N A              ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****           Rainfall-Runoff-Model             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     VERS.  2.1.0    STATUS: 24.06.2008      ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     Contact:        Dipl.-Ing J. Nordmeier  ****',/,10x,     &
      &'****                     nordmeier@tuhh.de       ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Hauptroutine: kalypso-na.f90           ****',/,10x,     &
      &'****     ZYKLUS:',I6,'                           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (kalypso-na.f90)          ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')


      ivor=1
      lzpath=''

! Erzeugen von debug.dat
! Diese Datei enthaelt alle im code enthaltenen detailierten Ausgaben
!------------------------
IF ( DEBUG) THEN
    CALL getLogFile( ndebug  )
    WRITE (ndebug,600)
    WRITE (ndebug,1)
END IF



! Erzeugen von error.log
!-------------------------

      datnam(1:5)='error'
      datnam(6:80)='.gml'
      nerrlog = ju0gfu()
      inquire (file=datnam,exist=lex)
      if(lex) then
    	 open(nerrlog,iostat=ierr,err=8,file=datnam)
	 close(nerrlog,err=8,status='delete')
      endif
      open(nerrlog,iostat=ierr,err=8,file=datnam)
      rewind(nerrlog)
      call writeHeader()

! Erzeugen von output.err
!-------------------------

      datnam(1:6)='output'
      datnam(7:80)='.err'
      nerr = ju0gfu()
      inquire (file=datnam,exist=lex)
      if(lex) then
    	 open(nerr,iostat=ierr,err=8,file=datnam)
	 close(nerr,err=8,status='delete')
      endif
      open(nerr,iostat=ierr,err=8,file=datnam)
      rewind(nerr)
      WRITE (nerr,600) !JH Kalypso About: Version,Author...

      dosflag=.true.
      if(dosflag) then
	 slash = char(92)
      else
	 slash = '\'
      endif

! Lesen von falstart.lst
!------------------------
      datnam(1:12)='falstart.lst'
      nfall=ju0gfu()
      open (nfall,iostat=ierr,err=668,file=datnam,status='old')
667   read(nfall,'(a)',err=669,end=669)datfall
      if (datfall(1:1).eq.'x'.or.datfall(1:1).eq.'X') goto 667
      if(datfall(1:1).eq.' ')goto 669

!  Aufteilungsfaktor für die Parallelspeicherkaskade beim
!  Isochronenverfahren für versiegelte Flaechen

      beta = 1.

!  Initialisierung
!     iflag_lzsim = 0
      namo = 0
      naja = 0
      natg = 0
      nast = 0
      nemo = 0
      neja = 0
      neja2000 = 0
      naja2000 = 0
      netg = 0
      nest = 0
      dt = 0.
      isim = 0
      pdsol = 0.
      pjsol = 0.
      ipver = 0
      ntag = 0
      do 3228 i1=1,ikdim
       xnnq(i1)=10000.
       do 3227 i2=1,12
        qmms(i1,i2)=0.
3227   continue
3228  continue
      do 3225 i1=1,jmaxcon2
       nresj(i1,1)=0
       nresj(i1,2)=0
3225  continue
      svend=0.


! Erzeugen von output.res
!-------------------------
      datnam(1:80)='output.res'
      nres = ju0gfu()
      inquire (file=datnam,exist=lex)
      if(lex) then
	 open(nres,iostat=ierr,err=8,file=datnam)
	 close(nres,err=8,status='delete')
      endif
      open(nres,iostat=ierr,err=8,file=datnam)
      rewind(nres)
      WRITE (nres,600) !JH Kalypso About: Version,Author...

      write (*,601) datfall
      write (nres,601) datfall
      write (nerr,601) datfall

601   FORMAT (/,1X, 'Bearbeitung Fall / Berechnungseinstellungen aus Steuerungsdatei:'/ &
                & 1X, '----------------------------------------------------------------'/ &
                & 1X, a)

! Variableninitialisierung
!--------------------------

!EP   call u1null(nr,jdim)
      call u1null(h01,jdim)
!     call u1null(bo1,jdim)
      call u1null(bianf1,jdim)
      call u1null(aint1,jdim)
!     call u1null(sbar_l,jdim)
      call u0null(jnr,10)
!     call u0null(sv1,10)
      call u0null(istrng,jdim)
      call u0null(ntgout,jdim)
      call u0null(knoto,jdim)
      call u0null(knotu,jdim)
      call u0null(knot,ikdim)
      call u0null(knout,ikdim)
      call u0null(iteil,jdim)
      call u0null(izug,ikdim)
      call u0null(iueb,ikdim)
      call u0null(izuf,ikdim)
      call u0null(iabg,ikdim)
      call u1null(qzug,ikdim)
      call u1null(qabg,ikdim)
      call u1null(queb,ikdim)
      call u0null(ikzie,ikdim)                                           
      call u0null(ikzue,ikdim)                                           
      call u0null(ikz1,ikdim)
      call u1null(xmq,ikdim)
 
      call u0null(anzm,12)
      call u1null(xbil,21)
      call u1null(xbil2,21)
      call u1null(xbilsp,20)
      call u1null(svmmw,12)
      call u1null(svmm,12)
!JH 24.09.1004 Initialisierung der Variable nanfstd (Uhrzeit zu der Startwerte in kz-sim geschrieben werden sollen)
      call u0null(nanfstd,idimanf)
                                          
      do 30 i1=1,ikdim
       do 40 i2=1,idimtgw
	qg(i1,i2) = 0.
40     continue
30    continue
      do 31 i1=1,jdim
       nw_boanf(i1,1,1)=0.
       do i2=1,maxlay
        bo1(i2,i1) = 0.
       end do
31    continue

      ismax = 0
      ikmax = 0
      iko = 0
      iku = 0
      ikziel = 0
      ikzueb = 0
      iz = 0
      izykl = 0
      npin = 0
      nqa = 0
      nqg = 0
      l3 = 0
      nqif = 0
      nqvs = 0
      nmrqa = 0
      nqb = 0
      ngw = 0
      nkap = 0
      ninf = 0
      nvep = 0
      nvet = 0
      nprk = 0
      nress = 0
      nres2 = 0
      nmq = 0
      nt = 0
      nquh = 0
      nqgg = 0
      ngws = 0
      nh = 0
      nueb = 0
      nbof=0
      nqtg1=0
      nqtg2=0
      nsv = 0
      nsh = 0
      nsv_e = 0
      nsv_p = 0
      nsv_b = 0
      imic=0
      imac=0
      inft=0
      nvgp=0

      iend = 0
      nf = 0

      idifbil=0
      izbil=0

      xwahl2 = 0

!  Initialdimensionierung der anzahl Teilgebiete
!                     und  "    "    Rueckhaltebecken

      jmax = 12


! Auswertung falstart.lst - Berechnungseinstellungen
!----------------------------------------------------

!c  Erweiterte Eingabe zur Vereinfachung der Ausgabekonfiguration
!c
!c	1: synthetische Niederschlaege
!c      2: natürlicher Niederschlag

      if(datfall(1:1).eq.'1'.or.datfall(1:1).eq.'2') then
	 i1=2
	 call nextblank(i1,i2,datfall)
	 read(datfall(i1:i2),'(a)')pathn                ! Projektpfad
         ilend = ju0nch(pathn)
	 if(pathn(ilend:ilend).ne.slash) then
	    pathn(ilend+1:ilend+1)=slash
	    ilend=ilend+1
	 endif

	 i1=i2
	 call nextblank(i1,i2,datfall)
	 read(datfall(i1:i2),'(a)')namger               ! Gerinnename
	 i1=i2
	 call nextblank(i1,i2,datfall)
	 read(datfall(i1:i2),'(a)')tkenn                ! Bearbeitungszustand


	 if(datfall(1:1).eq.'1') then                 	! synthetischer Niederschlag (datfall(1:1).eq.'1')
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),'(a)')namfn             ! Name synth. Niederschlagsdatei
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)xjah                  ! Wiederkehrintervall (1/a)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)xwahl2                ! Niederschlagsdauer (h)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)ipver                 ! Niederschlagsverteilung

!SK/CB 28.10.03 - synth. Niederschlag soll durch input fuer jedes TG eingelesen werden.
!	   call checkn(pathn,ilend,0,namfn,xjah,xwahl2,slash)
	    pns='s'


	 else                                         	! natürlicher Niederschlag (datfall(1:1).eq.'2')
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)naja                  ! Jahr Simulationsstart (yyyy oder yy)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)namo                  ! Monat Simulationsstart (MM)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)natg                  ! Tag Simulationsstart (dd)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)nast                  ! Stunde Simulationsstart (hh)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)neja                  ! Jahr Simulationsende (yyyy oder yy)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)nemo                  ! Monat Simulationsende (MM)
	    i1=i2
	    call nextblank(i1,i2,datfall)
	    read(datfall(i1:i2),*)netg                  ! Tag Simulationsende (dd)
	    i1=i2
	    call nextblank(i1,i2,datfall)
            read(datfall(i1:i2),*)nest                  ! Stunde Simulationsstartsende (hh)

	    if(naja.gt.100) then
	       naja2000=naja
	       i1=int(naja/100)
	       naja=naja-i1*100
!TODO: 	JH Jahr 200 Faehigkeit nicht gewaehrleistet, da benutzer mit yy=04 auch 2004
!       meinen kann, es aber als 1904 interpretiert wird!!!
	    else
	       naja2000=naja+1900
	    endif
	    if(neja.gt.100) then
	       neja2000=neja
	       i1=int(neja/100)
	       neja=neja-i1*100
	    else
	       neja2000=neja+1900
	    endif
	    pns='n'
	 endif

	 i1=i2
	 call nextblank(i1,i2,datfall)
	 read(datfall(i1:i2),'(a)')datfalln             ! Pfad Ausgabesteuerungsdatei (relativ vom Projektpfad)

	 if(datfalln(1:1).ne.slash)then
	    datfalln=pathn(1:ilend)//datfalln
	 endif

	 datfall=datfalln
      	 nin = ju0gfu()
	 open(nin,iostat=ierr,err=9600,file=datfall,status='old')         ! Ausgabesteuerungsdatei

	 goto 6789

      endif


!JH*****  Weitere Möglichkeit der Programmsteuerung (Angabe des Pfades zur *.konfig in der ***************
!         falstart.lst - alle anderen Angaben dann in der *.konfig), welche z.zT jedoch nicht           !*
!         verwendet wird                                                                                !*
      nin = ju0gfu()                                                                                    !*
      open(nin,iostat=ierr,err=9600,file=datfall,status='old')                ! *.konfig                !*
        												!*
                                                                                                        !*
                                                                                                        !*
!c hauptvorfluter des entwaesserungsgebietes -->                                                        !*
      read(nin,'(a)')namger                                                                             !*
!c gib name des projekt-directory -->                                                                   !*
      read(nin,'(a)')pathn                                                                              !*
      ilend = ju0nch(pathn)                                                                             !*
      if(pathn(ilend:ilend).ne.slash) then                                                              !*
	 pathn(ilend+1:ilend+1)=slash                                                              	!*
	 ilend=ilend+1                                                                                  !*
      endif                                                                                             !*
                                                                                                        !*
!c zustand --->                                                                                         !*
      read(nin,'(a)')tkenn                                                                              !*
                                                                                                        !*
!c kurzzeit / langzeit / synthetischer N. (noch nicht einbezogen)                                       !*
                                                                                                        !*
      pns='n'	                                                                                        !*
      read(nin,'(i4)') naja                                                                             !*
!cccccccccccccc                                                                                         !*
!c                                                                                                      !*
!c jahr 2000-problem                                                                                    !*
!c                                                                                                      !*
      if(naja.gt.100) then                                                                              !*
	 naja2000=naja                                                                                  !*
	 i1=int(naja/100)                                                                               !*
	 naja=naja-i1*100                                                                               !*
      elseif(naja.gt.0) then                                                                            !*
	 naja2000=naja+1900                                                                             !*
      endif                                                                                             !*
                                                                                                        !*
                                                                                                        !*
!ccccccc                                                                                                !*
!  synthetischer niederschlag                                                                           !*
!  dann: datenuebergabe:                                                                                !*
!c		filename datenfile synth.nieders. (in klima.dat)                                        !*
!c	                                                                                                !*
      if(naja.eq.-99) then                                                                              !*
	 call checkn(pathn,ilend,nin,namfn,xjah,xwahl2,slash)                                 !*
	 pns='s'                                                                                        !*
      else                                                                                              !*
	 read(nin,'(i4)') namo                                                                          !*
	 read(nin,'(i4)') natg                                                                          !*
	 read(nin,'(i4)') nast                                                                          !*
	 read(nin,'(i4)') neja                                                                          !*
	 if(neja.gt.100) then                                                                           !*
	    neja2000=neja                                                                     	     	!*
	    i1=int(neja/100)                                                          	             	!*
	    neja=neja-i1*100                                                  	                     	!*
	 else                                                         	                             	!*
	    neja2000=neja+1900                                	                                     	!*
	 endif                                        	                                             	!*
	 read(nin,'(i4)') nemo                	                                                     	!*
	 read(nin,'(i4)') netg        	                                                             	!*
	 read(nin,'(i4)') nest   	                                                                !*
      endif                                                                                             !*
!JH*******************************************************************************************************


6789  continue

      ileng= ju0nch(namger)           ! Zeichenlaenge Gerinnename (namger)
      iken = ju0nch(tkenn)            ! Zeichenlaenge Bearbeitungszustand (tkenn)
!JH   WRITE(*,'(/1X,a,a8,a,a8)') 'Gerinnename: ',namger,' Bearbeitungszustand: ',tkenn
      namger(ileng+1:80)='_'//tkenn(1:iken)
      ilenb = ileng + 1 + iken


!---------------------------------------------------------------------------------
! E R F A S S U N G      D E R      V E R N E T Z U N G S M A T R I X            -
!---------------------------------------------------------------------------------

      namntz=pathn(1:ilend)//'inp.dat'//slash//namger(1:ilenb)//'.ntz'
      WRITE(*,'(/1X,a,a)') 'Pfad der Netzdatei: ',namntz
      ilen=ju0nch(namntz)
      nstop=0


      call netan(ismax,namntz,ilen,itgmax,nstop)                     ! Einlesen der Netzdatei

      if(nstop.gt.0.and.nstop.lt.3) then                                        ! Programmabbruch bei Fehler in der Datei
      call writelogString(7,'Fehler in der Netzdatei!','Error in the Netfile!','', &
            & '','','','','kalypso-na')

	 write(nerr,10101)
10101	 FORMAT(/,1X, 'Fehler in der Netzdatei! ',  &
     	           1X, 'Weitere Hinweise im File output.err')
         call writeFooter()         
         stop
      endif

!-----------------------------------------------------------------------------------------------
!      E I N L E S E N     D E R     A U S G A B E S T E U E R U N G S D A T E I   (*.konfig)  -
!-----------------------------------------------------------------------------------------------

      read(nin,*) dt
      if (dt.gt.23.5) then
	 isim = 0                     ! Langzeitsimulation
         WRITE(nres,10111)
         WRITE(*,10111)
10111    FORMAT (/,1X,'L A N G Z E I T S I M U L A T I O N')
      else
	 isim = 1                     ! Kurzzeitsimulation
         WRITE(nres,10112)
         WRITE(*,10112)
10112    FORMAT (/,1X,'K U R Z Z E I T S I M U L A T I O N')
      endif

!  Ermittlung des Simulationszeitraumes
!--------------------------------------

      idatsa = naja*10**4 + namo*10**2 + natg
      idatse = neja*10**4 + nemo*10**2 + netg

      if (dt.gt.23.5) then
	 nast = 24
	 nest = 24
         int_4= 1
	 call cdatum (natg,namo,naja,int_4)
	 call cdatum (netg,nemo,neja,int_4)
	 idatselz = neja*10**4 + nemo*10**2 + netg
         int_4=-1
	 call cdatum (natg,namo,naja,int_4)
	 call cdatum (netg,nemo,neja,int_4)
      else
	 idatselz = idatse
      endif


      datnam=pathn
      datnam(ilend+1:ilend+4)='out.'
      ilenda=ilend+4

      datnam(ilenda:ilenda+ileng+1)='_'//namger(1:ileng)//'.'
      ilenda=ilenda+ileng+1

      datnam(ilenda+1:80)=tkenn(1:iken)//slash
      ilenda=ilenda+iken+1

      dathyd(1:)=datnam(1:ilenda)//'gwn_hyd.out'//slash
      char_idatsa='000000'

      if(idatsa.lt.1000) then
         write(char_idatsa(4:6),'(i3)')idatsa
      elseif (idatsa.lt.10000) then
	 write(dummy(3:6),'(i4)')idatsa
      elseif (idatsa.lt.100000) then
	 write(char_idatsa(2:6),'(i5)')idatsa
      else
	 write(char_idatsa,'(i6)')idatsa
      endif
      inlen=6

! synthetischer Niederschlag
!----------------------------
      if(pns.eq.'s') then
	 if(xjah.lt.0.6) then
	    i1=int(1./xjah)
	 else
	    i1=int(xjah)
	 endif
	 if(i1.lt.10) then
	    write(char_idatsa(1:1),'(i1)')i1
	    inlen=2
	 elseif(i1.lt.100) then
	    write(char_idatsa(1:2),'(i2)')i1
	    inlen=3
	 else
	    write(char_idatsa(1:3),'(i3)')i1
	    inlen=4
	 endif
	 write(char_idatsa(inlen:inlen),'(a)')'_'
	 inlen=inlen+1
	 if(xwahl2.lt..99) then
	    write(char_idatsa(inlen:inlen),'(a)')'-'
	    i1=int(xwahl2*100.)
	    inlen=inlen+1
	    write(char_idatsa(inlen:inlen+1),'(i2)')i1
	    inlen=inlen+1
	 elseif(abs(xwahl2-1.5).lt.0.0001) then
	    i1=int(xwahl2*10.)
	    write(char_idatsa(inlen:inlen+1),'(i2)')i1
	    inlen=inlen+1
	 elseif(abs(xwahl2-2.5).lt.0.0001) then
	    i1=int(xwahl2*10.)
	    write(char_idatsa(inlen:inlen+1),'(i2)')i1
	    inlen=inlen+1
	 elseif(xwahl2.lt.9.9)then
	    i1=int(xwahl2)
	    write(char_idatsa(inlen:inlen),'(i1)')i1
	 else
	    i1=int(xwahl2)
	    write(char_idatsa(inlen:inlen+1),'(i2)')i1
	    inlen=inlen+1
	 endif
      endif


!c	call u0ljst(dummy)
!c	inlen = ju0nch(dummy)

!AG 18.08.2002  Die folgenden beiden Zeilen werden auskommentiert, da in dieser Version
!               die Ausgabedateien nicht mehr nach dem Anfangsdatum benannt werden sollen.
!               Das neue Grafiktool braucht als Dateiendung .dat !
!

!SK/JH 11.11.2004  Für Ausführung mehrerer Berechnungen hintereinander wird die Bezeichnung der
!                  Ausgabedateien wieder mit dem Anfangsdatum versehen. Die Endung .dat bleibt
!		   erhalten, die Änderungen von AG werden teilweise zurückgenommen.
      datnam(ilenda+1:80)=char_idatsa(1:inlen)
      ilenda=ilenda+inlen


!-----------------------------------------------------------------------------------------------
!        O E F F N E N    D E R    G E W Ä H L T E N    E R G E B N I S D A T E I E N          -
!                                                                                              -
!          - Eingelesene Temperaturwerte                                                       -
!          - Eingelesene Niederschlaege                                                        -
!          - Berechnete Schneehoehe                                                            -
!          - Bodenfeuchte                                                                      -
!          - Bodenspeicher                                                                     -
!          - Grundwasserstand                                                                  -
!          - Gesamtabfluss Knoten                                                              -
!          - Gesamtabfluss Teilgebiet                                                          -
!          - Berechneter Oberflaechenabfluss natuerlicher Flaechen                             -
!          - Interflow                                                                         -
!          - Berechneter Oberflaechenabfluss versiegelter Flaechen                             -
!          - Basisabfluss                                                                      -
!          - Kluftgrundwasser 1                                                                -
!          - Kluftgrundwasser 2                                                                -
!          - Grundwasserabfluss                                                                -
!          - Grundwasserabfluss                                                                -
!          - kapillarer Aufstieg                                                               -
!          - Evapotranspiration                                                                -
!          - Ausgabe Hydrotope                                                                 -
!          - Abflussbilanz                                                                     -
!          - statistische Abfluesse                                                            -
!          - Speicherinhalt, Wasserspiegelhoehe, Verdunstung aus Talsperre,                    -
!            Niederschlag in Talsperre, Zehrung                                                -
!          - Speicherueberlauf 
!          - imic                                                                -
!          - imac
!          - inft
!          - nvgp
!-----------------------------------------------------------------------------------------------

! Temperatur tmp.dat
!-------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nt = ju0gfu()
	 datnam(ilenda+1:ilenda+8)='_tmp.dat'
	 open(nt,iostat=ierr,err=9,file=datnam)
	 close(nt,status='delete')
	 open(nt,iostat=ierr,err=9,file=datnam)
	 write(nt,79)
	 write(nt,*)
	 write(nt,9964) char_idatsa(1:6),nast,idatselz,nest,dt
9964     format(' simulationszeitraum:   von  ',a6,i4,' uhr   bis',i8,i4,   &
                ' uhr ',10x,f6.3,/,'')
79       format(/' t e m p e r a t u r   tagesmittel in grad c ')
      endif

! Niederschlag pre.dat
!---------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 npin = ju0gfu()
	 datnam(ilenda+1:ilenda+8)='_pre.dat'
	 open(npin,iostat=ierr,err=9009,file=datnam)
	 close(npin,status='delete')
	 open(npin,iostat=ierr,err=9009,file=datnam)
	 write(npin,98)
	 write(npin,*)
	 if(xwahl2.gt.1.e-04)then
	    write(npin,5001) xjah,xwahl2,dt,ipver
	 else
	    write(npin,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
5001     format(' synth. n.: haufigkeit: ',f8.3,                      &
                ' jahre,dauer: ',f5.2,' h , zeitschr.: ',f6.3,          &
              ' h , verteilung: ',i1,/,'')
98       format(/' n i e d e r s c h l a g   in mm/h ')
      endif
      
! Schneehoehe sch.dat
!--------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nh = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_sch.dat'
	 open(nh,iostat=ierr,err=8008,file=datnam)
	 close(nh,status='delete')
	 open(nh,iostat=ierr,err=8008,file=datnam)
	 write(nh,89)
	 write(nh,*)
	 write(nh,9964) char_idatsa(1:6),nast,idatselz,nest,dt
89       format(/' s c h n e e h o e h e    in mm ')
      endif

! Bodenfeuchte bof.dat
!---------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nbof  = ju0gfu()
	 datnam(ilenda+1:ilenda+8)='_bof.dat'
	 open(nbof,iostat=ierr,err=9099,file=datnam)
	 close(nbof,status='delete')
	 open(nbof,iostat=ierr,err=9099,file=datnam)
	 write(nbof,146)
	 write(nbof,*)	 
	 write(nbof,9964) char_idatsa(1:6),nast,idatselz,nest,dt
146      format(/'b o d e n f e u c h t e  in mm ')
      endif

! Bodenspeicher bsp.dat
!----------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 l3 = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_bsp.dat'
	 open(l3,iostat=ierr,err=9090,file=datnam)
	 close(l3,status='delete')
	 open(l3,iostat=ierr,err=9090,file=datnam)
	 write(l3,34)
	 write(l3,*)	 
	 write(l3,9964) char_idatsa(1:6),nast,idatselz,nest,dt
34       format(/' d a t e n   b o d e n s p e i c h e r ')
      endif

! Grundwasserstand gws.dat
!-------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 ngws = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_gws.dat'
	 open(ngws,iostat=ierr,err=8088,file=datnam)
	 close(ngws,status='delete')
	 open(ngws,iostat=ierr,err=8088,file=datnam)
	 write(ngws,88)
	 write(ngws,*)	 
	 write(ngws,9964) char_idatsa(1:6),nast,idatselz,nest,dt
88       format(/' g r u n d w a s s e r s t a n d  in m ')
      endif

! Gesamtabfluss Knoten qgs.dat
!-----------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqg  = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qgs.dat'
	 datqgs = datnam
INQUIRE (file=datnam, OPENED=lopen)
     IF( lopen ) THEN
        write(*, *) 'Datei ist bereits geoeffnet. Kann nicht zweimal geoeffnet werden' 
        stop
     ENDIF
     OPEN(nqg,file=datnam,STATUS='replace',IOSTAT=ierr,ACTION='write')
	 write(nqg,500)
	 write(nqg,*)
	 if(xwahl2.gt.1.e-04)then
	    write(nqg,5001) xjah,xwahl2,dt,ipver
	 else
	    write(nqg,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
500      format(/' G e s a m t a b f l u s s    in m**3/s  ')
	 close (nqg)
      endif


! Gesamtabfluss Teilgebiet qgg.dat
!---------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqgg  = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qgg.dat'
	 open(nqgg,iostat=ierr,err=9099,file=datnam)
	 close(nqgg,status='delete')
	 open(nqgg,iostat=ierr,err=9099,file=datnam)
	 write(nqgg,502)
	 write(nqgg,*)
	 if(xwahl2.gt.1.e-04)then
	    write(nqgg,5001) xjah,xwahl2,dt,ipver
	 else
	    write(nqgg,9964) char_idatsa(1:6),nast,idatse,nest,dt
	 endif
502      format(/' Gesamtabfluss  Teilgebiet ',                          &
                 '(Overl., Interfl., vers.Fl., Basisabf.) in m**3/s')
      endif

!Overlandflow qna.dat
!--------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqa  = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qna.dat'
	 open(nqa,iostat=ierr,err=9099,file=datnam)
	 close(nqa,status='delete')
	 open(nqa,iostat=ierr,err=9099,file=datnam)
	 write(nqa,100)
	 write(nqa,*)
	 if(xwahl2.gt.1.e-04)then
	    write(nqa,5001) xjah,xwahl2,dt,ipver
	 else
	    write(nqa,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
100      format(/' o v e r l a n d - f l o w  in m**3/s ')
      endif

!Interflow qif.dat
!-----------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqif = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qif.dat'
	 open(nqif,iostat=ierr,err=9909,file=datnam)
	 close(nqif,status='delete')
	 open(nqif,iostat=ierr,err=9909,file=datnam)
	 write(nqif,110)
	 write(nqif,*)
	 if(xwahl2.gt.1.e-04)then
	    write(nqif,5001) xjah,xwahl2,dt,ipver
	 else
	    write(nqif,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
110      format(/'i n t e r f l o w   in m**3/s ')
      endif

!Abfluss versiegelter Flaechen qvs.dat
!------------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'j') then
	 nqvs = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qvs.dat'
	 open(nqvs,iostat=ierr,err=9900,file=datnam)
	 close(nqvs,status='delete')
	 open(nqvs,iostat=ierr,err=9900,file=datnam)
	 write(nqvs,120)
	 write(nqvs,*)
	 if(xwahl2.gt.1.e-04)then
	    write(nqvs,5001) xjah,xwahl2,dt,ipver
	 else
	    write(nqvs,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
120      format(/'A b f l u s s  versiegelter Flaechen  in m**3/s ')
      endif

!Basisabfluss qbs.dat
!--------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqb = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qbs.dat'
	 open(nqb,iostat=ierr,err=99,file=datnam)
	 close(nqb,status='delete')
	 open(nqb,iostat=ierr,err=99,file=datnam)
	 write(nqb,130)
	 write(nqb,*)
 	 if(xwahl2.gt.1.e-04)then
	    write(nqb,5001) xjah,xwahl2,dt,ipver
	 else
	    write(nqb,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
130      format(/'B a s i s a b f l u s s    in m**3/s ')
      endif


!Abfluss Kluftgrundwasser gt1.dat
!--------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqtg1  = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qt1.dat'
	 open(nqtg1,iostat=ierr,err=9099,file=datnam)
	 close(nqtg1,status='delete')
	 open(nqtg1,iostat=ierr,err=9099,file=datnam)
	 write(nqtg1,145)
	 write(nqtg1,*)	 
	 write(nqtg1,9964) char_idatsa(1:6),nast,idatselz,nest,dt
145      format(/'A b f l u s s  Kluftgrundwasser  in m**3/s ')
      endif

!Abfluss Kluftgrundwasser gtg.dat
!--------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nqtg2  = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qtg.dat'
	 open(nqtg2,iostat=ierr,err=9099,file=datnam)
	 close(nqtg2,status='delete')
	 open(nqtg2,iostat=ierr,err=9099,file=datnam)
	 write(nqtg2,1145)
	 write(nqtg2,*)	 
	 write(nqtg2,9964) char_idatsa(1:6),nast,idatselz,nest,dt
1145     format(/'A b f l u s s  Kluftgrundwasser  in m**3/s ')
      endif

!Grundwasserabfluss qgw.dat
!--------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 ngw = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qgw.dat'
	 open(ngw,iostat=ierr,err=90,file=datnam)
	 close(ngw,status='delete')
	 open(ngw,iostat=ierr,err=90,file=datnam)
	 write(ngw,140)
	 write(ngw,*)
 	 if(xwahl2.gt.1.e-04)then
	    write(ngw,5001) xjah,xwahl2,dt,ipver
	 else
	    write(ngw,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
140      format(/'G r u n d w a s s e r a b f l u s s    in m**3/s ')
      endif

!TODO: JH, wird nicht berechnet, daher kann das hier wohl auch weg.
!Kapillarer Aufstieg kap.dat
!---------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nkap = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_kap.dat'
	 open(nkap,iostat=ierr,err=90,file=datnam)
	 close(nkap,status='delete')
	 open(nkap,iostat=ierr,err=90,file=datnam)
	 write(nkap,151)
	 write(nkap,*)
	 write(nkap,9964) char_idatsa(1:6),nast,idatselz,nest,dt
151      format(/' k a p i l l a r e r   A u f s t i e g    in mm')
      endif



!pot. Inflitrationsrate inf.dat  (noch nicht implementiert)
!----------------------------------------------------------
!     read (nin,'(a)') iant
      iant='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
	 ninf = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_inf.dat'
	 open(ninf,iostat=ierr,err=90,file=datnam)
	 close(ninf,status='delete')
	 open(ninf,iostat=ierr,err=90,file=datnam)
	 write(ninf,141)
	 write(ninf,*)	 
	 write(ninf,9964) char_idatsa(1:6),nast,idatselz,nest,dt
141      format(/' pot. i n f i l t r a t i o n s - r a t e ',     &
                 '(inf)   in mm/h ')
      endif



!Evaporation aus Bodenspeicher vep.dat   (noch nicht implementiert)
!------------------------------------------------------------------
!     read (*,'(a)') iant
      iant='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nvep = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_vep.dat'
	 open(nvep,iostat=ierr,err=90,file=datnam)
	 close(nvep,status='delete')
	 open(nvep,iostat=ierr,err=90,file=datnam)
	 write(nvep,142)
	 write(nvep,*)
	 write(nvep,9964) char_idatsa(1:6),nast,idatselz,nest,dt
142      format(/' E v a p o r a t i o n  aus Bodenspeicher   in mm ')
      endif


! Evapotranspiration vet.dat
!---------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nvet = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_vet.dat'
	 open(nvet,iostat=ierr,err=90,file=datnam)
	 close(nvet,status='delete')
	 open(nvet,iostat=ierr,err=90,file=datnam)
	 write(nvet,143)
	 write(nvet,*)	 
	 write(nvet,9964) char_idatsa(1:6),nast,idatselz,nest,dt
143      format(/'  E v a p o t r a n s p i r a t i o n ',      &
                 '   in mm ')
      endif


!Perkolation prk.dat    (noch nicht implementiert)
!-------------------------------------------------
!     read (*,'(a)') iant
      iant='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nprk = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_prk.dat'
	 open(nprk,iostat=ierr,err=90,file=datnam)
	 close(nprk,status='delete')
	 open(nprk,iostat=ierr,err=90,file=datnam)
	 write(nprk,147)
	 write(nprk,*)	 
	 write(nprk,9964) char_idatsa(1:6),nast,idatselz,nest,dt
147      format(/'  P e r k o l a t i o n     in mm ')

      endif





!Abfluss Mulden-Rigolen von versiegelten Flaechen qmr.dat
!------------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
 	 nmrqa = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_qmr.dat'
 	 open(nmrqa,IOSTAT=ierr,FILE=datnam,STATUS='REPLACE')
 	 write(nmrqa,149)
	 write(nmrqa,*) 	 
         write(nmrqa,9964) char_idatsa(1:6),nast,idatselz,nest,dt
149      format(/'A b f l u s s  Mulden-Rigolen von versiegelten Flaechen  in m**3/s ')
      endif


!TODO: JH, Hydrotopausgabe testen
!Hydrotope hyd.dat
!-----------------
!c	varianten:
!c	  nhydtyp=1:  ausgabe transp, gws, usw hydrotop und sim.jahr
!c	  nhydtyp=2:  ausgabe transp, gws, usw hydrotop und mittelwert
!c	  nhydtyp=3:  ausgabe grundwasserneubildung in mm/tageswert 
!c	  nhydtyp=4:  ausgabe grundwasserneubildung in mm/jahr und Monat 

      read (nin,'(a)') iant
      nhydtyp=0
      nhyd = 0
      i4=ichar(iant)
      if(i4.ge.49.and.i4.le.57)then
	 nhydtyp=i4-48
      elseif(iant.eq.'j'.or.iant.eq.'J') then
	 nhydtyp=2
      endif
      if(nhydtyp.eq.2) then
!AG 180802  Die folgende Zeile wird fuer das neue Grafiktool auf .dat geaendert.
	 datnamhyd=datnam(1:ilenda)//'_hyd.dat'
	 do 245 idnr=1,idimhyd
	  m_hyd(idnr,1)=0.
	  m_hyd(idnr,2)=0.
	  m_hyd(idnr,3)=0.
	  m_hyd(idnr,4)=0.
	  m_hyd(idnr,5)=0.
	  m_hyd(idnr,6)=0.
	  m_hyd(idnr,7)=0.
245	 continue
      endif


!Abflussbilanz bil.dat bzw. bil.txt
!----------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'j'.or.iant.eq.'2') then
	 nress = ju0gfu()
 	 datnam(ilenda+1:ilenda+8)='_bil.dat'
	 open(nress,iostat=ierr,err=90,file=datnam)
	 close(nress,status='delete')
	 open(nress,iostat=ierr,err=90,file=datnam)
	 write(nress,139)
         if(xwahl2.gt.1.e-04)then
	    write(nress,5001) xjah,xwahl2,dt,ipver
	 else
            write(nress,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
139      format(/'     A b f l u s s b i l a n z  ')
         do 246 i1=1,jdim
	  do 246 i2=1,20
	   m_bil(i1,i2)=0.
246	 continue
      endif
	  
      if(iant.eq.'1'.or.iant.eq.'2') then
	 nres2 = ju0gfu()
	 datnam(ilenda+1:ilenda+8)='_txt.dat'
	 open(nres2,iostat=ierr,err=90,file=datnam)
	 close(nres2,status='delete')
	 open(nres2,iostat=ierr,err=90,file=datnam)
         write(nres2,'(a)')'tgnrx,nied ,schnee,verdunst,abflob,abflbeiw,evaveg,evabod,Qover,Qint,Qvers,Perk'
      endif


!statistische Abfluesse nmq.dat
!------------------------------
      read (nin,'(a)') iant
      nmq=0
      if(iant.eq.'j'.and. dt.gt.23.5) then
	 nmq = ju0gfu()
	 datnam(ilenda+1:ilenda+8)='_nmq.dat'
	 open(nmq,iostat=ierr,err=90,file=datnam)
	 close(nmq,status='delete')
	 open(nmq,iostat=ierr,err=90,file=datnam)
	 write(nmq,138)
	 write(nmq,*)	 
         write(nmq,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 do 2303 ik=1,ikmax
	  qmnq(ik)=0.
	  qmhq(ik)=0.
2303	 continue
138      format(/'     s t a t i s t i s c h e   A b f l u e s s e ')
      endif


!Speicherinhalt spi.dat, Wasserspiegelhoehe sph.dat,
!Verdunstung aus Talsperre spv.dat,
!Niederschlag in Talsperre spn.dat, Zehrung spb.dat,
!---------------------------------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nsv = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_spi.dat'
	 open(nsv,iostat=ierr,err=8800,file=datnam)
	 close(nsv,status='delete')
	 open(nsv,iostat=ierr,err=8800,file=datnam)
	 write(nsv,144)
 	 write(nsv,*)
         if(xwahl2.gt.1.e-04)then
	    write(nsv,5001) xjah,xwahl2,dt,ipver
	 else
            write(nsv,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
!	 write(nsv,64) idatsa,nast,idatse,nest,dt
144      format(/' S p e i c h e r i n h a l t   in hm**3 ')
         nsh = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_sph.dat'
	 open(nsh,iostat=ierr,err=8800,file=datnam)
	 close(nsh,status='delete')
	 open(nsh,iostat=ierr,err=8800,file=datnam)
	 write(nsh,14401)
	 write(nsh,*)	 
         if(xwahl2.gt.1.e-04)then
	    write(nsh,5001) xjah,xwahl2,dt,ipver
	 else
            write(nsh,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
14401    format(/' W a s s e r s p i e g e l h o e h e   in muNN ')

	 nsv_e = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_spv.dat'
	 open(nsv_e,iostat=ierr,err=8800,file=datnam)
	 close(nsv_e,status='delete')
	 open(nsv_e,iostat=ierr,err=8800,file=datnam)
	 write(nsv_e,14402)
	 write(nsv_e,*)	 
         if(xwahl2.gt.1.e-04)then
	    write(nsv_e,5001) xjah,xwahl2,dt,ipver
	 else
            write(nsv_e,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
14402    format(/' Verdunstung aus Talsperre in qbm/tag ')

	 nsv_p = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_spn.dat'
	 open(nsv_p,iostat=ierr,err=8800,file=datnam)
	 close(nsv_p,status='delete')
	 open(nsv_p,iostat=ierr,err=8800,file=datnam)
	 write(nsv_p,14403)
	 write(nsv_p,*)	 
         if(xwahl2.gt.1.e-04)then
	    write(nsv_p,5001) xjah,xwahl2,dt,ipver
	 else
            write(nsv_p,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
14403    format(/' Niederschlag in Talsperre in qbm/tag ')


	 nsv_b = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_spb.dat'
	 open(nsv_b,iostat=ierr,err=8800,file=datnam)
	 close(nsv_b,status='delete')
	 open(nsv_b,iostat=ierr,err=8800,file=datnam)
	 write(nsv_b,14404)
	 write(nsv_b,*)	 
         if(xwahl2.gt.1.e-04)then
	    write(nsv_b,5001) xjah,xwahl2,dt,ipver
	 else
            write(nsv_b,9964) char_idatsa(1:6),nast,idatselz,nest,dt
	 endif
14404    format(/' Zehrung (Nieders.-Verd.)  qbm/tag ')
      endif


!Speicherueberlauf sub.dat
!-------------------------
      read (nin,'(a)') iant
      if(iant.eq.'j'.or.iant.eq.'J') then
	 nueb = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_sub.dat'
	 open(nueb,iostat=ierr,err=8080,file=datnam)
	 close(nueb,status='delete')
	 open(nueb,iostat=ierr,err=8080,file=datnam)
	 write(nueb,148)
	 write(nueb,*)	 
         if(xwahl2.gt.1.e-04)then
	    write(nueb,5001) xjah,xwahl2,dt,ipver
	 else
            write(nueb,9964) char_idatsa(1:6),nast,idatselz,nest,dt
         endif

148      format(/' S p e i c h e r u e b e r l a u f   in m**3/s ')
      endif


!Unit Hydrograph quh.dat (noch nicht implementiert)
!--------------------------------------------------
!c    read (*,'(a)') iant
      iant='n'
      if(iant.eq.'J'.or.iant.eq.'j') then
	 nquh = ju0gfu()
         datnam(ilenda+1:ilenda+8)='_quh.dat'
	 open(nquh,iostat=ierr,err=8800,file=datnam)
	 close(nquh,status='delete')
	 open(nquh,iostat=ierr,err=8800,file=datnam)
	 write(nquh,153)
	 write(nquh,*)	 
	 write(nquh,154) dt
153      format(/' U n i t - H y d r o g r a p h  in m**3/s ')
154      format(/' Laenge des Rechenzeitintervalls: ',f6.2)
      endif


!TODO: JH, evtl. in *.konfig als Auswahl hinzufuegen
!JH 26.11.2004 Ausgabe der UH´s fuer das Gerinne (siehe gerinne.f90) hinzugefuegt

!Unit Hydrograph Gerinne ngeruh.dat                             ! UH-Gerinne, wird immer erzeugt in Ergebnisdatei
!----------------------------------

      ngeruh = ju0gfu()
      datnam(ilenda+1:ilenda+11)='_uh_ger.dat'
      open(ngeruh,iostat=ierr,err=8800,file=datnam)
      close(ngeruh,status='delete')
      open(ngeruh,iostat=ierr,err=8800,file=datnam)
      write(ngeruh,155)
  	  write(ngeruh,*)      
155   format(/' U n i t - H y d r o g r a p h  Gerinne')
      write(ngeruh,9964) char_idatsa(1:6),nast,idatselz,nest,dt


 ! Imic_real imic.dat
!---------------------
!      read (nin,'(a)') iant
      iant ='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
	 imic  = ju0gfu()
	 datnam(ilenda+1:ilenda+12)='_imic.dat'
	 open(imic,iostat=ierr,err=9089,file=datnam)
	 close(imic,status='delete')
	 open(imic,iostat=ierr,err=9089,file=datnam)
	 write(imic,346)
	 write(imic,*)	 
	 write(imic,9964) char_idatsa(1:6),nast,idatselz,nest,dt
346      format(/'Infiltration in soil matrix  in mm ')
      endif

  ! Imac_real imac.dat
!----------------------
!      read (nin,'(a)') iant
      iant ='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
	 imac  = ju0gfu()
	 datnam(ilenda+1:ilenda+12)='_imac.dat'
	 open(imac,iostat=ierr,err=9079,file=datnam)
	 close(imac,status='delete')
	 open(imac,iostat=ierr,err=9079,file=datnam)
	 write(imac,347)
	 write(imac,*)	 
	 write(imac,9964) char_idatsa(1:6),nast,idatselz,nest,dt
347      format(/'Infiltration in macropores  in mm ')
      endif


! Infiltration in soil inft.dat
!-------------------------------
!      read (nin,'(a)') iant
      iant ='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
	 inft  = ju0gfu()
	 datnam(ilenda+1:ilenda+12)='_inft.dat'
	 open(inft,iostat=ierr,err=9069,file=datnam)
	 close(inft,status='delete')
	 open(inft,iostat=ierr,err=9069,file=datnam)
	 write(inft,348)
	 write(inft,*)	 
	 write(inft,9964) char_idatsa(1:6),nast,idatselz,nest,dt
348      format(/'Total Infiltration in soil  in mm ')
      endif

! Dual porosity  nvgp.dat
!---------------------
!     read (nin,'(a)') iant
      iant ='n'
      if(iant.eq.'j'.or.iant.eq.'J') then
   	 nvgp  = ju0gfu()
    	 datnam(ilenda+1:ilenda+11)='_vgp.dat'
    	 open(nvgp,iostat=ierr,err=890,file=datnam)
     	 close(nvgp,status='delete')
      	 open(nvgp,iostat=ierr,err=890,file=datnam)
        write(nvgp,*)'Soil moisture parameters based on dual porosity approach [mm] '
        write (nvgp,*)''
        write (nvgp,*)'Perc               Eva     Inf_micropores    Inf_macropores    Pore interaction    Total inf         ',&
                      'Inf_soilmoist             Inf_old         inf_prev'
        write (nvgp,*) '[mm]              [mm]        [mm]              [mm]                [mm]              [mm]          ',&
                      '    [mm]                    [mm]            [mm]'
      endif



      if (dt.gt.23.5) then                    ! Langzeitsimulation
	 nast = 0
	 nest = 0
      endif

! Einlesen der Knoten, fuer die die gewaehlten Ergebnisdateien erstellt werden sollen (aus *.konfig)
!---------------------------------------------------------------------------------------------------
      i1 = 0
25    continue
      i1 = i1 + 1
      read(nin,'(i8)',err = 25) knout(i1)
      if (knout(i1).ne.0.and.knout(i1).ne.9999.and.knout(i1).ne.99999  &
	  .and.knout(i1).ne.999999.and.knout(i1).ne.9999999  &
	  .and.knout(i1).ne.99999999) then
	 goto 25
      endif
      iknmax = i1-1

! Einlesen der Teilgebiete, fuer die die gewaehlten Ergebnisdateien erstellt werden sollen (aus *.konfig)
!--------------------------------------------------------------------------------------------------------
      i1 = 0
26    continue
      i1 = i1 + 1
      read(nin,'(i8)') ntgout(i1)
      if (ntgout(i1).ne.0.and.ntgout(i1).ne.9999.and.ntgout(i1).ne.99999) then
	 goto 26
      end if
      ingmax=i1-1


! Einlesen der Daten für die Anfangswerte geschrieben werden sollen (aus *.konfig)
!----------------------------------------------------------------------------------

      i1 = 0
27    continue
      i1 = i1 + 1
      dummy=' '

!JH 24.09.2004 neues Format der Anfangswerte mit Stunde eingefügt (yyyyMMdd  hh)
      read (nin, '(a12)',END=288) dummy

      if (dummy(1:1).ne.' '.and.dummy(1:4).ne.'9999') then
         if(i1.gt.idimanf) then                                                 ! Zu viele Werte angegeben
	    write(*,9401)idimanf

           call writelogIntInt(6,'Anzahl der Ereignisse fuer Anfangsbedingungen uebersteigt zulaessige Zahl!',&
           & 'Maximum number of initial conditions exceeded!','','',0,'idimanf',idimanf,'kalypso-na')

            WRITE(nerr,9401)idimanf

	    goto 288                                                            ! Ende der Einleseroutine Anfangswerte
	 endif
         if (LEN_TRIM(dummy(1:8)).lt.8) then		! Anfangswerte ohne Jahrhundert angeben
             call writelogString(7,'Anfangsdaten muessen mit Jahrhundert angegeben werden! Format: yyyymmdd  hh',&
             &'Initial data must be indicated with the format:  yyyymmdd  hh','','','','datfall',datfall,'kalypso-na')
    		WRITE(nerr,9402) datfall
            WRITE (*,9402) datfall
            CLOSE(nerr)
            call writeFooter()
            stop
         endif
         read (dummy(1:8),'(i8)') nanf(i1)                                    	! Datum des Anfangswertdatums
         if (LEN_TRIM(dummy(11:12)).eq.0) then                                	! Anfangswert ohne Stunde angeben
            WRITE (*,9400) nanf(i1), datfall
            call writelogIntInt(6,'Uhrzeit fuer Anfangswert wurde nicht angegeben! Es wird 0 Uhr angesetzt!',&
           & 'Time has been not specified! Time will be set to 0 h','','',0,'nanf',nanf(i1),'kalypso-na')

            WRITE (nerr,9400) nanf(i1), datfall
            nanfstd(i1) = 0                                                     ! Stunde wird zu 0 Uhr gesetzt
         else
            read (dummy(11:12),'(i2)') nanfstd(i1)                              ! Stunde des Anfangswertdatums
         endif
         GOTO 27
      END if

9400  FORMAT(/ 1X, 'Hinweis:'/&
	     & 1X, 'Uhrzeit fuer Anfangswert ',i8,' wurde nicht angegeben!' /  &
             & 1X, 'Datei: ', a80 /&
             & 1X, 'Es wird 0 Uhr angesetzt!')
9401  Format(/ 1X, 'Anzahl der Ereignisse fuer Anfangsbedingungen '/   &
     	     & 1X, 'uebersteigt zulaessige Zahl!',                 /   &
     	     & 1X, 'Maximal vorgesehene Anzahl: ', i5              /   &
     	     & 1X, 'Erhoehe Parameter idimanf in param.cmn!')
9402  FORMAT(/ 1X, 'Fehler in der Datei: ',a / &
             & 1X, 'Anfangsdaten muessen mit Jahrhundert angegeben werden!'/  &
             & 1X, 'Format: yyyymmdd  hh')

288   nanzanf1=i1-1

!EP 05.11.03  Initialisierung von nanzanf1 findet unter bestimmten Bedingungen
!             nicht statt. Damit Programm nicht spaeter abstürzt, wird nanzanf
!             zu null initialisiert.
      nanzanf = 0

!SK/AvD 05.04.2003
! Einlesen des lzsim-path
!-------------------------
! Dient der optionalen Angabe des Pfades zum lzsim-Ordner

      read(nin,'(a87)',end=2476) lzpdum     ! z.B. lzpath=c:\
      if(lzpdum(1:7).eq.'lzpath=') then
         lzpath(1:ju0nch(lzpdum)-7)=lzpdum(8:ju0nch(lzpdum))
         write(nres,'(//,1X,a,1X,a)') 'LZSIM-PATH:', lzpath
         write(*,'(//,1X,a,1X,a)') 'LZSIM-PATH:', lzpath
      endif
2476  close (nin)

!-----------------------------------------------------------------------------------------------
! E N D E   E I N L E S E N    D E R    A U S G A B E S T E U E R U N G S D A T E I  (*.konfig)-
!-----------------------------------------------------------------------------------------------



!  Bestimmung der  J A H R E S Z Y K L E N A N Z A H L
!-----------------------------------------------------
! Langzeit = Anzahl der Berechnungsjahre
! Kurzzeit = 1

      if (isim.eq.0) then
!	 izykl = neja - naja + 1
 	 izykl = neja2000 - naja2000 + 1
      else
	 izykl = 1
      endif


      naja20=naja2000

!c SK/EP 28.02.03
!c iflag_lzsim muß auch im zweiten/ folgenden Zyklus der Langzeitsimulation
!c mit 1000001 aus der *.konfig Datei gelesen werden. Key, damit zu jedem Tag der
!c Langzeitsimulation Anfangswerte geschrieben werden.
      iflag_lzsim=nanf(1)
      neja20=neja2000
!c    nemo20=nemo
!c    netg20=netg


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c                                                                                             c
!c                              B e g i n n    Z y k l e n s c h l e i f e                     c
!c                                                                                             c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do 1000 iz = 1,izykl

       namnal = ' '
       namfan = ' '
       nameptal = ' '
       izsim = 0

!TODO: JH, ist das hier richtig? warum jdim (Straenge)?
! Fuer jeden zyklus muss feld mit grundwasserzufuhr initialisiert werden
       do 344 i1=1,jdim
	do 343 i2=1,idim
	 qgwzu(i1,i2) = 0.
343     continue
344    continue

       call u1null(xbil,21)

!---------------------------------------------------------------
! Berechnung der Zyklenanfangs- und Enddaten                   -
! sowie der benötigten Berechnungszeitschritte und lzsim Daten -
!---------------------------------------------------------------

! synthetischer Niederschlag
!----------------------------
       if(xwahl2.gt.1.e-04)then
!c	  idif= int(xwahl2/dt+0.5)

!SK 23.10.04 neuer Simulationszeitraum für synth N
	  idif= 360
	  idata = naja*10**6 + namo*10**4 + natg*10**2 + nast
	  idate = neja*10**6 + nemo*10**4 + netg*10**2 + nest
	  idatsa = naja*10**4 + namo*10**2 + natg
	  idatse = neja*10**4 + nemo*10**2 + netg
	  idatsa20 = naja*10**4 + namo*10**2 + natg
	  idatse20 = neja*10**4 + nemo*10**2 + netg

! Kurzzeitsimulation
!--------------------
       elseif (isim.eq.1) then

!SK 16.08.02 nanzanf wird fuer kzsimulation initialisiert
          if (nanzanf1.eq.0) then
     	     nanzanf=0
          end if
!c SK 04.03.03
!c naja20 und neja20 für ntdif angesetzt, da jahr mit jahrhundert für differenz angesetzt.
          idif=ntdif(natg,namo,naja20,netg,nemo,neja20)+1
          if (idif.eq.1) then
	     idif = idif*24*int(1./dt + 0.1) - int(float(nast)/dt) - int(float(24-nest)/dt)
   	     goto 333
          endif
          idif = int(idif*24./dt + 0.1) -int(float(nast)/dt) - int(float(24-nest)/dt)
          if (idif.gt.idimt) then
           call writeLogIntIntInt(7,'Simulationzzeitraum laenger als maximal simulierbares Zeitintervall!',&
           & 'Simulation period longer than maximum allowed zeit intervall!','','',0,'idif',idif,'idimt',idimt,'kalypso-na')


   	     write(*,3333) idif, idimt
   	     write(nerr,3333) idif,idimt


3333         format(/ 1X, 'Simulationzzeitraum laenger als maximal simulierbares Zeitintervall !!!'/ &
                    & 1X, 'Waehle groesseres Zeitintervall dt oder kuerzeren Simulationszeitraum.'/ &
                    & 1X, 'Anzahl der Simulationsintervalle idif = ',i5/  &
                    & 1X, 'Moegliche Anzahl an Zeitschritten idimt = ',i5)
             call writeFooter()
             stop 'kalypso-na 445'
          endif

333       idata = naja*10**6 + namo*10**4 + natg*10**2 + nast
          idate = neja*10**6 + nemo*10**4 + netg*10**2 + nest
          idatsa = naja*10**4 + namo*10**2 + natg
          idatse = neja*10**4 + nemo*10**2 + netg
          idatsa20 = naja20*10**4 + namo*10**2 + natg
          idatse20 = neja20*10**4 + nemo*10**2 + netg
          netg1=netg
          nemo1=nemo
          neja1=neja

          write(nres,10001) ' Zyklus (iz):                  ', iz
          write(nres,10001) ' Anfangsdatum Zyklus (idatsa): ', idatsa
          write(nres,10001) ' Enddatum Zyklus (idatse):     ', idatse
          write(nres,10001) ' Zeitschritte (idif):          ', idif



!TODO: JH, prüfen, ob dies korrekt funktioniert.
!JH 14.10.04 zu welchem Zeitschritt ianfkz(i1) in der KZ-simulation sollen Anfangswerte geschrieben werden?

! ********************* Zeitschritte mit Ergebnisausgabe  L Z S I M
          IF (nanzanf1 .GT. 0) THEN
             sjahr = naja20
             smona = namo
             stag = natg
             sstd = REAL(nast)
             i1 = 1
             starttime : DO i=1,idimt
                          sstd = sstd + dt
                          IF (sstd .GE. 24.0) THEN
                             sstd = sstd - 24.0
                             call cdatum (stag,smona,sjahr,1)
                          END IF
                          sdatum = sjahr*10**4 + smona*10**2 + stag
                          isstd=INT(sstd)
                          IF (nanf(i1) .EQ. sdatum .and. nanfstd(i1) .EQ. isstd) THEN
                             ianfkz(i1) = i
                             i1 = i1 + 1
                             IF (i1 .GT. nanzanf1) EXIT starttime
                          END IF
                         END DO starttime
          END IF

! Langzeitsimulation
!--------------------
       else

! weitere Differenzierung in den jeweiligen Zyklus

! ********************* 1. Zyklus
	  if (iz.eq.1) then
             if(naja.eq.neja) then    ! Simulationszeitraum innerhalb eines Jahres
	        idif = ntdif(natg,namo,naja,netg,nemo,neja) + 1
		idif = int(idif*24./dt + 0.1)
		neja1=naja
		nemo1=nemo
		netg1=netg
		idata = naja*10**6 + namo*10**4 + natg*10**2 + nast
		idate = naja*10**6 + nemo*10**4 + netg*10**2 + nest
		idatsa = naja*10**4 + namo*10**2 + natg
		idatse = int(float(idate)/100.+0.1)
		idatsa20 = naja20*10**4 + namo*10**2 + natg
	        idatse20 = neja20*10**4 + nemo1*10**2 + netg1
	     else
		idif = ntdif(natg,namo,naja,31,12,naja) + 1
		idif = int(idif*24./dt + 0.1)
		neja1=naja
		nemo1=12
		netg1=31
                neja20=naja20
		nemo20=12
		netg20=31
                idata = naja*10**6 + namo*10**4 + natg*10**2 + nast
		idate = naja*10**6 + 123124
		idatsa = naja*10**4 + namo*10**2 + natg
		idatsa20 = naja20*10**4 + namo*10**2 + natg
		idatse = int(float(idate)/100.+0.1)
		idatse20 = neja20*10**4 + nemo1*10**2 + netg1
	     endif
             write(nres,10001) ' erster Zyklus (iz):           ', iz
             write(nres,10001) ' Anfangsdatum Zyklus (idatsa): ', idatsa
             write(nres,10001) ' Enddatum Zyklus (idatse):     ', idatse
             write(nres,10001) ' Zeitschritte (idif):          ', idif
10001	     format (a,i8)


! ********************* Nicht erster und nicht letzter Zyklus
 	  else if (iz.ne.izykl) then
	     natg=1
	     namo=1
	     naja=naja+1

!SK/EP 28.02.03 Jahr 2000 Faehigkeit
! Bei naja=99 im vorherigen Zyklus -> neuer Zyklus Jahr 2000: 100 statt 00,
! daher Abzug von 100, wenn naja ge 100   (naja: Anfangsjahr des Zyklus ohne Jahrhundert)
             if (naja.ge.100) THEN
                naja=naja-100
             end if
	     neja1=neja1+1

!SK/EP 28.02.03 Jahr 2000 Faehigkeit
! Bei neja=99 im vorherigen Zyklus -> neuer Zyklus Jahr 2000: 100 statt 00,
! daher Abzug von 100, wenn neja ge 100   (naja: Endjahr des Zyklus ohne Jahrhundert)
             if (neja.ge.100) THEN
                neja=neja-100
             end if

	     naja20=naja20+1
	     neja20=naja20
	     nemo20=12
	     netg20=31
             nemo1 = nemo20
             netg1 = netg20
             idif = ntdif(1,1,naja,31,12,naja) + 1
	     idif = int(idif*24./dt + 0.1)
	     idata = (naja)*10**6 + 010100
	     idate = (naja)*10**6 + 123124
	     idatsa = int(float(idata)/100.+0.1)
	     idatse = int(float(idate)/100.+0.1)
 	     idatsa20 = naja20*10**4 + namo*10**2 + natg
   	     idatse20 = neja20*10**4 + nemo1*10**2 + netg1
             write(nres,10001) ' Zyklus (iz):                  ', iz
             write(nres,10001) ' Anfangsdatum Zyklus (idatsa): ', idatsa
             write(nres,10001) ' Enddatum Zyklus (idatse):     ', idatse
             write(nres,10001) ' Zeitschritte (idif):          ', idif


! ********************* letzter Zyklus
          elseif (iz.eq.izykl) then
	     idif = ntdif(1,1,neja,netg,nemo,neja) + 1
	     idif = int(idif*24./dt + 0.1)
	     natg=1
	     namo=1
	     naja=naja+1
!	     neja=naja
	     nemo1=nemo
	     netg1=netg
	     neja1=neja
	     naja20=naja20+1
	     neja20=naja20
	     nemo20=nemo
	     netg20=netg
             idata = neja*10**6 + 010100
	     idate = neja*10**6 + nemo*10**4 + netg*10**2 + nest
	     idatsa = int(float(idata)/100.+0.1)
	     idatse = neja*10**4 + nemo*10**2 + netg
	     idatsa20 = naja20*10**4 + namo*10**2 + natg
	     idatse20 = neja20*10**4 + nemo1*10**2 + netg1
             write(nres,10001) ' letzer Zyklus (iz):           ', iz
             write(nres,10001) ' Anfangsdatum Zyklus (idatsa): ', idatsa
             write(nres,10001) ' Enddatum Zyklus (idatse):     ', idatse
             write(nres,10001) ' Zeitschritte (idif):          ', idif
          endif




! ********************* Zeitschritte mit Ergebnisausgabe  L Z S I M

	  i2 = 0
	  i3 = 0
!SK/EP 28.02.03
! in lzsim werden bei 1000001 als Anfangsdatum zu allen Zeitschritten der Langzeitsimulation
! Anfangswerte rausgeschrieben
! nanf(1) wird für jeden Zyklus gleich dem Anfangsdatum des Zykluses mit Jahrhundert gesetzt.
! nanzanf wird für diesen Fall zu null gesetzt, da kein Datum, sondern ein Key: 1000001 eingelesen wird.
! Weiteres Kriterien für diesen Fall: Anzahl der Anfangsdaten nanzanf1=1

          if(nanzanf1.EQ.1 .and.iflag_lzsim.EQ.1000001)then    		! Key angegeben (1000001)
             nanf(1) = idatsa20
             nanzanf=i3
	  elseif(iz.eq.1.and.nanzanf1.eq.0)then                         ! Keine Anfangswerte zu schreiben
	     nanzanf=-1
	  else                                                          ! Anfangswerte zu schreiben
	     do 32 i1 = 1,nanzanf1
	      if(nanf(i1).lt.idatsa20) goto 32                        	! Wert ist vor Anfang Zyklus
	      i2=i2+1
	      if(nanf(i1).le.idatse20) then                           	! Wert liegt im Zyklus
		 i3=i3+1
		 i4=nanf(i1)-(nanf(i1)/100)*100
		 i5=nanf(i1)/100-(nanf(i1)/10000)*100
		 xanf(i2)=ntdif(natg,namo,neja,i4,i5,neja)            	! Simulationsschritt in welchem
		 nanf(i2)=nanf(i1)                                    	! Anfangswert zu schreiben ist
	      else
		 nanf(i2)=nanf(i1)                                    	! Wert ist spaeter als Ende Zyklus
	      endif
32	     continue
	     nanzanf=i3
	     nanzanf1=i2
	  endif
       endif
! Ende Differenzierung nach Zyklen fuer Langzeitsimulation


!--------------------
! Zwischenausdruck  -
!--------------------

       write(nerr,10003)naja20,iz
       write(nres,10003)naja20,iz


10003  FORMAT(/,1X, '************************************************************************************'/&
               & 1X, '*  Neuer Jahreszyklus                                                              *'/ &
               & 1X, '*      Jahr:   ',i5,'                                                              *'/ &
               & 1X, '*      Zyklus: ',i5,'                                                              *'/ &
               & 1X, '************************************************************************************')


       idif1 = idif
       if(nmq .ne. 0) then            ! Ausdruck statistische Abflüsse
	  write(nmq,2332)iz
       endif
2332   FORMAT(/,1X, '************************************************************'/ &
               & 1X, 'Statistische Abflusskennwerte fuer Simulationszyklus ',i3)

!TODO: JH, Hydrotopausgabe testen
       if(nhydtyp.eq.1) then          ! Ausgabedatei Hydrotope
	  nhyd=ju0gfu()
!AG 18.08.02
! Die folgenden 3 Zeile werden fuer das neue Grafiktool auf .dat geaendert.
          datnam(ilenda+1:ilenda+4)='hyd_'
          write(datnam(ilenda+5:ilenda+6),'(i2)')naja
          datnam(ilenda+7:ilenda+10)='.dat'
	  open(nhyd,iostat=ierr,err=90,file=datnam)
	  close(nhyd,status='delete')
	  open(nhyd,iostat=ierr,err=90,file=datnam)
	  write(nhyd,1139) naja
       endif

       if(nhydtyp.eq.4.and.iz.gt.1) then
          print *, 'filename muss angepasst werden'
!c        stop
	  write(text(1:2),'(i2)')naja
	  dathyd='/fu1f/pgm/hydro/gwn_hyd.ou3/gwn_'//text(1:2)//'.hyd'
	  nhyd=ju0gfu()
	  open(nhyd,iostat=ierr,err=99099,file=dathyd)
	  close(nhyd,status='delete')
	  open(nhyd,iostat=ierr,err=99099,file=dathyd)
	  write(nhyd,10502)
	  write(nhyd,10064) idatsa,idatse,dt
10502     format(/' grundwasserneubildung in  mm  ',    &
     	          'hyd.id, gws/ jahr /monat')
10064     format(' simulationszeitraum:   von',i8,'           bis',i8,  &
                 '         ',10x,f6.3)
       endif



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c                                                                                             c
!c                              B e g i n n    S t r a n g s c h l e i f e                     c
!c                                                                                             c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       do 1100 is = 1,ismax          ! is = Strangzaehler

        call u1null(qaifl,idimt)
	call u1null(qavs,idimt)
	call u1null(qa,idimt)
	call u1null(qtgw,idimt)
	call u1null(qb,idimt)

        write(nres,10001) ' Auswertung des Stranges:     ',istrng(is)
	write(nerr,10001) ' Auswertung des Stranges:     ',istrng(is)
	write(*,10001)    ' Auswertung des Stranges:     ',istrng(is)

! Bestimmung der Knotennummer im Knotendatensatz
	do 1200 ik = 1,ikmax
	 if (knot(ik).eq.knoto(is)) iko = ik
	 if (knot(ik).eq.knotu(is)) iku = ik
1200    continue

! Zeitschritte
	if (izykl.gt.1) then
	   iend = idif                                  ! Berechnete Zeitschritte
	else
	   iend = idimt                                 ! maximal mögliche Zeitschritte
	end if

!------------------------------------------------------------------------------
! Abflussverzögerung im Gerinne                                               -
!------------------------------------------------------------------------------

! Anfangsstraenge (9000 < knoto(is) > 10000) koennen Uebersprungen werden
! es erfolgt keine Abflussverzoegerung
        if (knoto(is).ge.9000 .and. knoto(is).lt.10000) then         ! Anfangsstrang
	   write(nres,1461)
	   write(*,1461)
	   goto 1462
	endif
1461    FORMAT(/ 1X, 'Anfangsstrang! ' / &
               & 1X, 'Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000'/&
               & 1X, 'Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.')

	call u1null(qz,idimt)


! Umspeichern: Gesamtabfluss Oberknoten qg(iko,i) (= Gesamtabfluss Unterknoten vorheriger Strang)
!              entspricht Strangzufluss qz(i)
	do 1201 i=1,iend
	 qz(i) = qg(iko,i)
1201	continue


!-----------------------------------------------------------------------------------------------
! Anfangswerte *.lzg                                                                           -
!                                                                                              -
! Langzeit: Gerinneabfluss wird in *.lzg für vorgegebene Anfangszeitpunkte gespeichert.        -
!                                                                                              -
! Kurzzeit: Bei Kurzzeitsimulation wird Abfluss von *.lzg eingelesen und als unterer Grenzwert -
!           angesetzt (Schwellwertkonzept).                                                    -
!           Für vorgegebene Anfangszeitpunkte wird Gerinneabfluss in *.lzg gespeichert.        -
!-----------------------------------------------------------------------------------------------
	lt=0.
	r = float(istrng(is))
	call s0rc(text0,r,lt)

! Pfad zum *.lzg  File wird gesetzt

!SK/AvD  5.4.2003
        IF (isim.eq.1 .AND. ju0nch(lzpath).gt.0) then           ! Ausgelagerter lzsim-Ordner
           fnam=lzpath
           ilenlz = ju0nch(fnam)
           fnam(ilenlz+1:ilenlz+7)='\lzsim'//slash
	   ilenlz = ju0nch(fnam)
	else                                                    ! lzsim-Ordner im Projektverzeichnis
 	   fnam=pathn
 	   fnam(ilend+1:ilend+6)='lzsim'//slash
	   ilenlz = ilend+6
       	endif

        ilenlzsim = ilenlz - 1

	if(lt.gt.4.and.ileng.gt.3) then  !c  projekte aabach beruecksichtigt 5-stellige gebietsnummern
	   fnam(ilenlz+1:ilenlz+ileng-1)=namger(1:ileng-1)
	   ilenlz = ilenlz+ileng-1
	else
	   fnam(ilenlz+1:ilenlz+ileng)=namger(1:ileng)
	   ilenlz = ilenlz+ileng
	endif

	fnam(ilenlz+1:ilenlz+lt)=text0(1:lt)
	ilenlz = ilenlz+lt
	fnam(ilenlz+1:ilenlz+4) = '.lzg'
	ilenlz=ilenlz+4

	if(ilenlz.gt.80)then
         call writeLogString(7, 'Pfad hat mehr als 80 Zeichen!','File path is longer than 80 characters!',&
              &'','','','fnam',fnam,'kalypso-na')

	   write(nerr,1013)fnam
	   write(*,1013)fnam
1013       format (/ 1X, 'Pfad fuer File ',a,' hat mehr als 80 Zeichen.'/&
                   & 1X, 'Speichern sie das Projekt unter einem kuerzeren Pfad.'/&
                   & 1X, '!!! Programmabbruch !!!')
           call writeFooter()
	   stop
	endif

	qg_lz=0.

! Langzeitsimulation - Anfangswerte schreiben
!---------------------------------------------
! Gerinneabfluss wird in *.lzg für vorgegebene Anfangszeitpunkte gespeichert (geschrieben).
	if(isim.eq.0) THEN

!SK/EP 28.02.03
! Öffnen der lzsim auch für den Fall das Anfangswerte zu jedem Zeitschritt geschreiben werden.
           IF(nanzanf.gt.0.or.nanf(1).gt.0)then
	      if(iz.eq.1.and.nanzanf.ge.0) then
	         call inp_lzsim(fnam,ilenlz,1,nanff)                       ! vorhandene Datei löschen und neu anlegen
	      else
	         call inp_lzsim(fnam,ilenlz,2,nanff)                       ! Datei bis zum Ende lesen
	      endif
	      nanzanff=nanzanf
!	      if(nanzanff.eq.0.and. nanf(1).eq.0) goto 1041
	      ianf=1
              do 1040 i=1,idif
! SK/EP 28.02.03
! Datenausgabe Anfangswerte LZSIM
! Das Anfangsdatum des Zykluses 8 stellig wird in Jahr, Monat und Tag gesplittet, um die
! Funktion cdatum anzuwenden.
               IF(nanf(1).EQ.idatsa20.and. nanzanf.eq.0)then
                  ijahr=(nanf(1)/10**4)
                  imona=((nanf(1)-ijahr*10**4)/100)
                  itag =(nanf(1)-ijahr*10**4-imona*100)
! SK/EP 28.02.03
! Die do Variable i: do i=1, idif (Anzahl der Zeitschritte im Zyklus) muß neu initialisiert werden,
! da keine do-Variable übergeben werden darf. Die Funktion cdatum setzt idatum für jeden Zeitschritt
! neu, damit so in der lzsim alle Anfangswerte herausgeschrieben werden können.
                  itemp=i
                  call cdatum (itag,imona,ijahr,itemp)
                  idatum=ijahr*10**4+imona*10**2+itag
                  write(nanff,'(i8,2x,a2,a2,a4,a4)') idatum,'00',' h','   1',' qgs'
	          write(nanff,'(i4,f9.3)')1,qz(i)
	       elseif (nanzanff.gt.0.and.i.ge.xanf(ianf)) then
	          write(nanff,'(i8,2x,a2,a2,a4,a4)') nanf(ianf),'00',' h','   1',' qgs'
	          write(nanff,'(i4,f9.3)')1,qz(i)
	          ianf=ianf+1
	          if(ianf.gt.nanzanff) then
	     	     nanzanff=0
		     goto 1041
	          endif
	       endif
1040	      continue
1041	      continue
	      if(nanff.gt.0) close (nanff)
           endif

! Kurzzeitsimulation natürlicher Niederschlag
!---------------------------------------------

	elseif(pns.ne.'s') then
! Einlesen der Werte aus *.lzg
	   iok=1
	   call inp_lzsim(fnam,ilen+5,0,nanff)                               	! Öffnen der *.lzg-Datei, falls sie vorhanden ist
	   if(nanff.gt.0.and.pns.ne.'s') then                                	! Falls Kanalnummer nanff vergeben ist und nicht mit synthetischem Niederschlag gerechnet wird...
              call inp_anf_gerinne(fnam,idatsa20,nanff,qg_lz,iok)            	! Einlesen des Anfangswertes, falls vorhanden
	      close (nanff)
	   endif

!TODO: JH, Schwellwertkonzept prüfen
! Anwendung der Anfangswerte für die Kurzzeitsimulation (Schwellwertkonzept)
	   if(iok.eq.0.and.qg_lz.gt.0.) then                                 	! Falls Anfangswerte vorhanden und größer 0, dann
	      do 1202 i=1,iend                                               	! Schleife über mögliche Zeitschritte in der kz-Simulation
	       if(qg_lz.gt.qz(i)) then                                		! wenn Anfangswert > berechneter Knotenzufluss
		  qz(i) = qg_lz                                                 ! berechneter Knotenzufluss und Gesamtabfluss am vorherigen Strang (unterer Knoten)= Anfagswert
		  qg(iko,i) =qg_lz
	       else                                                             ! übersteigt berechneter Knotenzufluss den Anfagswert
		  qg_lz=0.                                                      ! wird für alle weiteren Zeitschritte der berechnete wert angesetzt.
		  goto 1203
	       endif
1202	      continue
1203	      continue
              WRITE (nres,9001) qg_lz
9001          FORMAT (/ 1X, 'Anfangswert aus LZSIM-Datei (*.lzg)'/&
                      & 1X, 'Uebernommener Schwellwert des Abflusses: ', f9.3)
	   endif

!TODO: JH, Testing
!JH 24.09.2004
! Schreiben von *.lzg-Dateien auch für ausgewaehlte Daten in der KZ-sim.

           if (nanzanf1.gt.0) then
              IF (nanff.eq.0) THEN                                              ! *.lzg ist nicht vorhanden
                 call inp_lzsim(fnam,ilenlzsim,1,nanff)                         ! neue Datei erzeugen und öffnen
              ELSE                                                              ! *.lzg ist bereits vorhanden
!                call inp_lzsim(fnam,ilen+5,2,nanff)                            ! alte Datei öffnen und bis ans Ende lesen
                 call inp_lzsim(fnam,ilen+5,1,nanff)                            ! alte Datei öffnen, schließen und neu erzeugen
              END IF
!JH hier xanf auch fuer KZ-sim einfuehren, um nicht kompliziert nach dem Datum "zu suchen"
              sjahr = naja20
              smona = namo
              stag = natg
              sstd = REAL(nast)
              i1 = 1
              writekzanf: do i = 1, idif
                           sstd = sstd + dt
                           if (sstd .ge. 24.0) then
                              sstd = sstd - 24.0
                              call cdatum (stag,smona,sjahr,1)
                           end if
                           sdatum = sjahr*10**4 + smona*10**2 + stag
!                          
                           isstd=INT(sstd)
                           if (nanf(i1) .eq. sdatum .and. nanfstd(i1) .eq. isstd) then
                              WRITE(nanff,'(i8,2x,i2,a2,a4,a4)') nanf(i1),nanfstd(i1),' h','    1',' qgs'
                              WRITE(nanff,'(a4,f9.3)') '1',qz(i)
                              i1 = i1+1
                              IF (i1.gt.nanzanf1) EXIT writekzanf
                           end if
                          end do writekzanf
              CLOSE(nanff)
           end if
        endif

!-----------------------------------------------------------------------------------------------
!     Korrektur der Inputwelle ins Gerinne um Zu- oder Abschlaege am Anfangsknoten             -
!     (Werte werden in der Netzdatei angegeben.)                                               -
!     Korrektur erfolgt fuer Kurzzeitsimulation bis iend (max. Zeitschritte)                   -
!     fur Langzeitsimulation lediglich fuer Anzahl der Zeitschritte pro jahr = idif            -
!-----------------------------------------------------------------------------------------------

! kontinuierliche Zugabe [m^3/s] (qzug)
!-------------------------------------
	if (izug(iko).gt.0) then
	   do 1300 i=1,iend
	    qz(i) = qz(i) + qzug(iko)
1300	   continue
	end if

! kontinuierliche Entnahme [m^3/s] (qabg) zu einem Zielknoten (ikzie)
!-------------------------------------------------------------------
	if (iabg(iko).gt.0) then
	   do 1310 ik = 1,ikmax
	    if(knot(ik) .eq. ikzie(iko)) then
	       ikziel = ik
	       goto 1315
	    endif
1310	   continue
1315	   do 1400 i=1,iend
	    if (qz(i).gt.qabg(iko)) then
	       qg(ikziel,i) = qabg(iko) + qg(ikziel,i)
	       qz(i) = qz(i) - qabg(iko)
	    else
	       qg(ikziel,i) = qg(ikziel,i) + qz(i)
	       qz(i) = 0
	    end if
1400	   continue

! Ueberlauf bei Ueberschreitung einer Ueberlaufhoehe [m^3/s] (queb) zu einem Zielknoten (ikzue)
!---------------------------------------------------------------------------------------------
	else
	   if (iueb(iko).gt.0) then
	      do 1410 ik = 1,ikmax
	       if(knot(ik) .eq. ikzue(iko)) then
		  ikzueb = ik
		  goto 1415
	       endif
1410	      continue
1415	      do 1440 i=1,iend
	       if (qz(i).gt.queb(iko)) then
		  qg(ikzueb,i) = qz(i) - queb(iko) + qg(ikzueb,i)
		  qz(i) = queb(iko)
	       end if
1440	      continue
	   end if
	end if

!Zu-/Abfluss ueber Ganglinie
!---------------------------
        if (izuf(iko).gt.0) then
              call zufgang(nzuf(izuf(iko)),idata,idate,idif,dt,qzuf,  &
                           namn,nast,natg,namo,naja20,		      &
	                   nemn,nest,netg1,nemo1,neja20,              &
       		           pns)
              qsum=0.
	      qsum2=0.
	      qzsum=0.
	      do 1451 i=1,iend
	       qsum=qz(i)+qsum
	       qzsum=qzuf(i)+qzsum
	       qz(i)=qz(i)+qzuf(i)
	       qsum2=qz(i)+qsum2
	       if(qz(i).lt.0.) qz(i)=0.
1451	      continue
	      write(nres,9403)qsum,qzsum,qsum2
	   endif
9403	   Format(/1X, ' Abflussfuelle im Gerinne vor Zugabe/Entnahme:  ',f10.3,/      &
                  & 1X, ' Abfluessfuelle Zugabe/Entnahme:                ',f10.3,/      &
                  & 1X, ' Abflussfuelle im Gerinne nach Zugabe/Entnahme: ',f10.3)

!Verzweigung. Prozentuale Angabe des aus dem Gerinne in die Verzeigung laufenden
!Anteils (zproz). Dieser Anteil wird dem Zielknoten (ikz1) zugeschlagen.
!-------------------------------------------------------------------------------
	   if (ivzwg(iko).gt.0) then
	      do 1450 ik=1,ikmax
	       if (knot(ik).eq.ikz1(iko)) then
		  ikzv1 = ik
		  goto 1455
	       end if
1450          continue
1455          do 1460 i=1,iend
	       qg(ikzv1,i) = qg(ikzv1,i) + qz(i) * zproz(iko)
	       qz(i) = qz(i) * (1.-zproz(iko))
1460          continue
	   end if


!-----------------------------------------------------------------------------------------------
!                               l i n e a r e  T r a n s l a t i o n                           -
!-----------------------------------------------------------------------------------------------

	   if(iz.eq.1.and.nquh.gt.0) then
	      do 1463 i=1,iknmax
	       if(knotu(is).eq.knout(i)) then
		  write(nquh,'(/'' Strang: '',i8/)') istrng(is)
	       endif
1463          continue
	   endif
	   if(izsim.lt.iend)izsim=iend
           call intrsp(istrng(is))                                    ! Einlesen der Gerinnedatei
	   call gerinne(iz,dt,iko,iku,izsim,idif,istrng(is),    &
                        nsv,nueb,nf,nquh,qz,nsv_e,nsv_p,nsv_b,nsh,      &
                        natg,namo,naja20,netg1,nemo1,neja20,            &
                        svmm,svend,ivor,xbilsp,                         &
                        slash,ianfkz,nanf,nanfstd,      &
                        nanzanf1,ngeruh)              				! JH 14.10.04 erweitert um ianfkz,nanf,nanfstd,nanzanf1


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c                                                                                             c
!c                           T e i l g e b i e t s b e h a n d l u n g                         c
!c                                                                                             c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



1462       if (iteil(is).eq.0) then
              write(nres,10001) ' Keine angeschlossenen Teilgebiete an Strang  ',istrng(is)
              write(*,10001) ' Keine angeschlossenen Teilgebiete an Strang  ',istrng(is)
           end if
           if (iteil(is).gt.0) then                     ! iteil = Anzahl der an den Strang (is) angeschlossenen Tg´s
	      do 1500 i=1,iteil(is)
	       ntg = nteil(is,i)
	       if(iz.eq.1) then
		  do 1464 i1=1,ingmax
		   if(ntg.eq.ntgout(i1)) then
		      write(nquh,'(/'' gebiet: '',i8/)')ntg
		   endif
1464              continue
	       endif

!cccccccccccccccccccc
!c
!c	ausgabedatei perkolation/hydrotop 

	       if(nhydtyp.eq.3) then
		  nhyd=ju0gfu()
		  write(text(1:2),'(i2)')naja
		  dathyd='/fu1f/pgm/hydro/gwn_hyd.ou3/'//          &
     		  text(1:2)//'_'
		  ilhyd=ju0nch(dathyd)+1
                  write(*,*)'dathyd=',dathyd
		  write(text(1:4),'(i4)')ntg
		  if(ntg.gt.999.5) then
		     dathyd(ilhyd:ilhyd+8)=text(1:4)//'.gwh'
		  elseif(ntg.gt.99.5) then
		     dathyd(ilhyd:ilhyd+8)=text(2:4)//'.gwh '
		  elseif(ntg.gt.9.5) then
		     dathyd(ilhyd:ilhyd+8)=text(3:4)//'.gwh  '
		  else
		     dathyd(ilhyd:ilhyd+8)=text(4:4)//'.gwh   '
		  endif
		  open(nhyd,iostat=ierr,err=99099,file=dathyd)
		  close(nhyd,status='delete')
		  open(nhyd,iostat=ierr,err=99099,file=dathyd)
		  write(nhyd,10503)
		  write(nhyd,10064) idatsa,idatse,dt
	       endif
10503          format(/' Grundwasserneubildung in  mm ')
!c10064        format(' simulationszeitraum:   von',i8,'           bis',i8,
!     +           '         ',10x,f6.3/)

!	       WRITE(*,'(//,a,i8,i10,i10,i8,i8)') ' Kontrollausgabe Gebiet: ',ntg, idata, idate, idatsa, idatse

!SK CB Dokumentation 28. 10. 2003: xjah und xwahl an Gebiet übergeben
	       call gebiet(dt,iz,ntg,npin,nqa,l3,          	 &
                           nbof,nqvs,nqif,nqb,ngw,               &
                           nh,ngws,nqtg1,nqtg2,nt,nkap,nmrqa,    &
                           idif,izsim,idif1,                     &
                           tkenn,namnal,namfan,nameptal,nquh,           &
                           nvep,nprk,nhyd,nhydtyp,    	 &
                           nanf,nanfstd,xanf,nanzanf,nanzanf1,                    & !JH nanfstd,nanzanf1 hinzugefügt
                           naja20,namo,natg,                     &
                           neja20,nemo1,netg1,idatsa20,          &
                           xbil,nw_bianf,nw_boanf,pns,   	 &
                           m_bil,m_flaech,m_hyd,slash,ispk,ivor, &
                           lzpath,xjah,xwahl2,nvgp,imic,imac,inft)

               if(nhydtyp.eq.3.and.nhyd.gt.0) close(nhyd)
!	       if(itime.gt.izsim)izsim=itime
	       ikt = 0
	       do 1550 ik=1,ikmax
		if (knot(ik).eq.izkn) ikt = ik
1550	       continue
	       if (ikt.gt.0) then
		  do 1600 i1 = 1,idimtgw
		   qg(ikt,i1) = qg(ikt,i1) + qtgw(i1)
1600		  continue
	       endif
               ! CK 03.03.06
               ! Hier wird der versiegelte Abfluss aus dem Teilgebiet behandelt.
               ! Wenn der versieglete Abfluss an einen bestimmten Knoten (in der Gebietsdatei als izkn_vers gesetzt)
               ! abgeschlagen werden soll wird dieser rausgesucht und mit qavs beaufschlagt. Ist kein Knoten angeben
               ! ( izkn_vers =0 ) wird es ganz normal an den Abflussknoten des aktuellen TG abgeschlagen.
	       ikt=0.
	       if(izkn_vers.gt.0.)then
		  do 1551 ik=1,ikmax
		   if (knot(ik).eq.izkn_vers) ikt = ik
1551		  continue
	       else
		  ikt=iku
	       endif
	       if (ikt.gt.0) then
		  do 1601 i1 = 1,izsim
		   qg(ikt,i1) = qg(ikt,i1) + qavs(i1)
1601		  continue
	       endif

!              if(nerr .ne. 0 .and. ntg .eq. 430) then
!		  do 3002 iner = 1,iknmax
!		   if (knot(iku).eq.knout(iner)) then
!		      write(nerr,'(/3(i8)/)') knot(iku),iz,iend
!		      write(nerr,'(10(f8.3,1x))') (qg(iku,i1),i1=1,iend)
!		      write(nerr,'(/3(i8)/)') knot(iku),iz,iend
!		      write(nerr,'(10(f8.3,1x))') (qa(i1),i1=1,iend)
!		      write(nerr,'(/3(i8)/)') knot(iku),iz,iend
!		      write(nerr,'(10(f8.3,1x))') (qaifl(i1),i1=1,iend)
!		      write(nerr,'(/3(i8)/)') knot(iku),iz,iend
!		      write(nerr,'(10(f8.3,1x))') (qb(i1),i1=1,iend)
!		   end if
!3002             continue
!               end if

		
		do 1606 i1 = 1,izsim
		 qg(iku,i1) = qg(iku,i1) + qa(i1) + qaifl(i1)+ qb(i1)
1606		continue

! ausgabe gesamtabfluss eines teilgebietes in den gerinneabschnitt
!-----------------------------------------------------------------
		if(nqgg.gt.0)then
		   do 1607 i4 = 1,ingmax
		    if (ntg.eq.ntgout(i4)) then
		       do 1608 i5=1,izsim
			qa(i5)=qa(i5)+qaifl(i5)+qavs(i5)+qb(i5)
1608		       continue
		       write(nqgg,'(/3(i8)/)') ntg,iz,idif
		       write(nqgg,'(10(f8.3,1x))')(qa(i5),i5=1,idif)
		    end if
1607		   continue
		endif



! ausgabe abfluss teilgebiet
!---------------------------

1500          continue          ! *****************************************************Ende Teilgebietsschleife
	   end if

1100      continue              ! *****************************************************Ende Strangschleife

	  if(nhyd.gt.0) close(nhyd)


!         Ergebnisfile  q g . d a t  erstellen
          iend = 0
!RS 7.4.2000 geaendert
!c	  if (isim.eq.0) then
!c	     iend = idif1
!c	  else
!c	     iend = izsim
!c	  end if

          iend = idif1
!         nress=2
          if(nress.gt.0) then
             call outbil(iz,iend,dt,xbil,xbil2)
!            call outbil(iz,iend,dt)
          endif

	  if (iend.gt.364.and. iz.gt.ivor) then
	     do 1010	i=1,10
	      xbil2(i)=xbil2(i)+xbil(i)
1010	     continue
	     do 1011	i=16,21
	      xbil2(i)=xbil2(i)+xbil(i)
1011	     continue
	     idifbil=idifbil+iend
	     izbil=izbil+1
	  endif

          if(nqg .ne. 0) then
	     nqg = ju0gfu()
	     open(nqg,access='append',iostat=ierr,err=9999,file=datqgs)
            do 2000 ik = 1,ikmax
	      do 2002 i = 1,iknmax
	       if (knot(ik).eq.knout(i)) then
		  write(nqg,'(/3(i8)/)') knot(ik),iz,iend
		  write(nqg,'(10(f8.3,1x))') (qg(ik,i1),i1=1,iend)
!c	          write(nqg,'(i4,f8.3)') (i1, qg(ik,i1),i1=1,iend)
	       end if
2002          continue
2000         continue
	     if (nqg.gt.0) close(nqg)
          endif


!  ausgabe grundwasserneubildung hydrotope 
!-----------------------------------------

!c	  if(nhydp.gt.0.and.iz.gt.1) then
!c	     do  16002 i2=1,13
!c	      do  16002 i1=1,maxhyd
!c	       if(m_perkout(i1,i2).le.0.1)m_perkout(i1,i2)=0.
!c16002	      continue
!c	      do  16001 i2=1,12
!c	       write(nhydm,'(3(i8))')i2,naja,maxhyd
!c	       write(nhydm,*) (m_perkout(i1,i2),i1=1,maxhyd)
!c16001	      continue
!c	      write(nhydp,'(2(i8))')naja,maxhyd
!c	      write(nhydp,*)(m_perkout(i1,13),i1=1,maxhyd)
!c	  endif


!  ausgabe statistische abflussdaten 
!-----------------------------------

	  if(nmq .ne. 0) then
	     do 2300 ik = 1,ikmax
	      do 2301 i = 1,iknmax
	       if (knot(ik).eq.knout(i)) then
		  qmin=100000.
		  qmax=0.
		  xsum=0.
		  do 2302 i1=1,iend
		   if(qg(ik,i1).lt.qmin) qmin=qg(ik,i1)
		   if(qg(ik,i1).gt.qmax) qmax=qg(ik,i1)
		   xsum=xsum+qg(ik,i1)
2302		  continue
		  write(nmq,2330) knot(ik),qmin
		  write(nmq,2331) qmax
                  xqm=xsum/float(iend)
		  if(iend.gt.0)write(nmq,2329) xqm
                  if(iz.eq.1) goto 2300
                  qmnq(ik)=qmnq(ik)+qmin
		  qmhq(ik)=qmhq(ik)+qmax
!  jahresmittelwerte
		  do 2305 i1=1,iend
		   xmq(ik)=xmq(ik)+qg(ik,i1)
		   if(xnnq(ik).gt.qg(ik,i1)) xnnq(ik)=qg(ik,i1)
2305		  continue

!   monatsmittelwerte

		  do 2304 i1=1,12
		   qkillm(i1)=100000.
		   qmm(i1)=0.
		   anzdm(i1)=0.
2304		  continue

	          do 7501 i1=1,iend
	           if(i1.le.31) then
		      qmm(1)=qmm(1)+qg(ik,i1)
		      anzdm(1)=anzdm(1)+1
		      if (qg(ik,i1).lt.qkillm(1)) qkillm(1)=qg(ik,i1)
	           elseif(i1.gt.31.and.i1.le.59) then
		      qmm(2)=qmm(2)+qg(ik,i1)
		      anzdm(2)=anzdm(2)+1
		      if (qg(ik,i1).lt.qkillm(2)) qkillm(2)=qg(ik,i1)
	           elseif(i1.gt.59.and.i1.le.90) then
		         qmm(3)=qmm(3)+qg(ik,i1)
	    	         anzdm(3)=anzdm(3)+1
		      if (qg(ik,i1).lt.qkillm(3)) qkillm(3)=qg(ik,i1)
	           elseif(i1.gt.90.and.i1.le.120) then
		      qmm(4)=qmm(4)+qg(ik,i1)
		      anzdm(4)=anzdm(4)+1
		      if (qg(ik,i1).lt.qkillm(4)) qkillm(4)=qg(ik,i1)
	           elseif(i1.gt.120.and.i1.le.151) then
		      qmm(5)=qmm(5)+qg(ik,i1)
		      anzdm(5)=anzdm(5)+1
		      if (qg(ik,i1).lt.qkillm(5)) qkillm(5)=qg(ik,i1)
	           elseif(i1.gt.151.and.i1.le.181) then
		      qmm(6)=qmm(6)+qg(ik,i1)
		      anzdm(6)=anzdm(6)+1
		      if (qg(ik,i1).lt.qkillm(6)) qkillm(6)=qg(ik,i1)
	           elseif(i1.gt.181.and.i1.le.212) then
		      qmm(7)=qmm(7)+qg(ik,i1)
		      anzdm(7)=anzdm(7)+1
		      if (qg(ik,i1).lt.qkillm(7)) qkillm(7)=qg(ik,i1)
	           elseif(i1.gt.212.and.i1.le.243) then
		      qmm(8)=qmm(8)+qg(ik,i1)
		      anzdm(8)=anzdm(8)+1
		      if (qg(ik,i1).lt.qkillm(8)) qkillm(8)=qg(ik,i1)
	           elseif(i1.gt.243.and.i1.le.273) then
		      qmm(9)=qmm(9)+qg(ik,i1)
		      anzdm(9)=anzdm(9)+1
		      if (qg(ik,i1).lt.qkillm(9)) qkillm(9)=qg(ik,i1)
	           elseif(i1.gt.273.and.i1.le.304) then
		      qmm(10)=qmm(10)+qg(ik,i1)
		      anzdm(10)=anzdm(10)+1
		      if (qg(ik,i1).lt.qkillm(10)) qkillm(10)=qg(ik,i1)
	           elseif(i1.gt.304.and.i1.le.334) then
		      qmm(11)=qmm(11)+qg(ik,i1)
		      anzdm(11)=anzdm(11)+1
		      if (qg(ik,i1).lt.qkillm(11)) qkillm(11)=qg(ik,i1)
	           elseif(i1.gt.334) then
		      qmm(12)=qmm(12)+qg(ik,i1)
		      anzdm(12)=anzdm(12)+1
		      if (qg(ik,i1).lt.qkillm(12)) qkillm(12)=qg(ik,i1)
	           endif
7501	          continue
		

	       end if
2301	      continue
2300	     continue
	  endif


2330	  format (' Tagesabfluss Knoten ',i5,'  minimum : ',f8.3,' qbm/s')
2331	  format ('                             maximum : ',f8.3,' qbm/s')
2329	  format ('          mittlerer Tagesabfluss     : ',f8.3,' qbm/s')



!     nachlauferfassung

	  do 1700 i1=1,ikmax
	   izaehl = 0
!	   do 1800 i2 = idif+1,idimtgw
!	    izaehl = izaehl + 1
!	    qg(i1,izaehl) = qg(i1,i2)
!1800      continue
	   do 1900 i2 = izaehl+1,idimtgw
	    qg(i1,i2) = 0.
1900       continue
1700      continue
1000    continue                  !****************************************************Ende Zyklusschleife


! ausgabe statistische abflussdaten
!----------------------------------

	if(nmq .ne. 0) then
	   write(nmq,2333)izykl-1
	   do 2340 ik = 1,ikmax
	    do 2341 i = 1,iknmax
	     if (knot(ik).eq.knout(i).and.izykl.gt.1) then
                zahl = qmnq(ik)/real(izykl-1)
		write(nmq,2334) knot(ik),zahl
		write(nmq,2335) zahl
		if(ntag.gt.0) then
                   zahl = xmq(ik)/REAL(ntag)
		   write(nmq,2350) zahl,ntag
		else
		   write(nmq,2350) xmq(ik),ntag
		endif
		write(nmq,2351) xnnq(ik)
		write(nmq,2352)
		do 7515 i1=1,12
		 if(anzm(i1).gt.0) then
                    zahl = qmms(ik,i1)/REAL(anzm(i1))
		    write(nmq,2353) i1,zahl
		 else
		    write(nmq,2353) i1,qmms(ik,i1)
		 endif
7515		continue
	     end if
2341	    continue
2340	   continue

!c umrechnung inhalt in wasserstand
!c geloescht ab version 3.2

	   close (nmq)
	endif
		
2333	format (/,'   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',/    &
     		'    statistische Abflusskennwerte ',/		        &
     		'	Mittelwerte ueber ',i3,' Simulationszyklen ',/   &
     		'	(ohne Anfangsjahr)')
2334	format (/'   xxxxxxxxxxxxxxxxxxxxxx',/                        &
      		' Mittelw. aller Jahres-Minima am Knoten',i5,      &
                '  mnQ : ',f8.3,' qbm/s')

2335	format (' Mittelw. aller Jahres-Maxima  mhQ : ',f8.3,' qbm/s')
2350	format (' Mittelw. aller Tageswerte     MQ  : ',f8.3,' qbm/s',   &
      		'    Anz. Tage:  ',i5)                                  
2351	format ('  absolut niedrigster Tageswert   NNQ : ',f8.3,' qbm/s')
2352	format (/,'  mittlere Monatswerte:')
2353	format (' Monat: ',i2,'     Mittelwert: ',f8.3,' qbm/s')



! hydrotopbezogene Ausgabe:
!--------------------------------
!c	m_hyd(id,1)+suevi(nn)
!c	m_hyd(id,2)=sueva(nn)
!c	m_hyd(id,3)=supover(nn)
!c	m_hyd(id,4)=supint(nn)
!c	m_hyd(id,5)=superk(nn)
!c	m_hyd(id,6)=supgws(nn)
!c	m_hyd(id,7)=suptgw(nn)

	if(nhydtyp.eq.2) then
	   nhyd=ju0gfu()
	   open(nhyd,iostat=ierr,err=90,file=datnamhyd)
	   close(nhyd,status='delete')
	   open(nhyd,iostat=ierr,err=90,file=datnamhyd)
	   iz=izykl-ivor
	   write(nhyd,1138) ivor,iz
	   write(nhyd,9605)
	   fakls=31.536
	   fmt(1:34)='(i5,a1,0x,5(f7.1,a1),f7.2,a1,f7.2)'
	   komma=','
	   do 9622 idnr=1,idimhyd
	    if(m_hyd(idnr,1).gt.0. .and. iz.gt.0) then
	       if(idnr.ge.1000.and.idnr.lt.10000)then
		  fmt(3:3)='4'
		  fmt(8:8)='1'
	       elseif(idnr.ge.100.and.idnr.lt.1000)then
		  fmt(3:3)='3'
		  fmt(8:8)='2'
	       elseif(idnr.ge.10.and.idnr.lt.100)then
		  fmt(3:3)='2'
		  fmt(8:8)='3'
	       elseif(idnr.ge.1.and.idnr.lt.10)then
		  fmt(3:3)='1'
		  fmt(8:8)='4'
	       endif
	       write (nhyd,fmt) idnr,komma,                            &
                                m_hyd(idnr,1)/iz,komma,                &
                                m_hyd(idnr,2)/iz,komma,                &
                                m_hyd(idnr,3)/iz,komma,                &
                                m_hyd(idnr,4)/iz,komma,                &
                                m_hyd(idnr,5)/iz,komma,                &
                                m_hyd(idnr,6)/(fakls*iz),komma,        &
                                m_hyd(idnr,7)/(fakls*iz)
	    endif
9622	   continue
	   if (nhyd.gt.0) close (nhyd)
	endif

1139	format(/'  e r g e b n i s a u s g a b e   h y d r o t o p e '/, &
               '  jahreszyklus: ', i4)
1138	format(/'  e r g e b n i s a u s g a b e   h y d r o t o p e '/,&
               '  mittelwert ueber auswertungszeitraum',                &
               '  (ohne ',i2,' anfangsjahre): ',/                       &
               '  anzahl der simulationsjahre:', i4)
9605	format( 'hydid,  interz,     eva,',                              &
               '   overl, interfl,    perk,',                            &
               '   qgws,    qtgw,')





! Ausgabe bilanz teilgebiete:
!----------------------------
	if(nress.gt.0) then
	   if (abs(dt-24.).lt.0.5) then
	      iz=izykl-ivor
	   else
	   iz=izykl
	   endif
	   call outbil_tg(ismax,m_bil,m_flaech,nr,iz,ispk)
	   izbil=-(izbil)
	   call outbil(izbil,idifbil,dt,xbil,xbil2)
	   if(xbilsp(4).gt.0.) then
	      izbil=-(izbil)
	      call outbil_sp(izbil,xbilsp)
	   endif
	endif

        if (npin.gt.0)close (npin)
        if (nqa.gt.0) close (nqa)
        if (nqg.gt.0) close (nqg)
        if (nqgg.gt.0) close (nqgg)
        if (l3.gt.0) close (l3)
        if (nqif.gt.0) close (nqif)
        if (nqvs.gt.0) close (nqvs)
        if (nqb.gt.0) close (nqb)
        if (ngw.gt.0) close (ngw)
        if (nt .gt.0) close (nt)
        if (nbof.gt.0) close (nbof)
        if (nqtg1.gt.0) close (nqtg1)
        if (nqtg2.gt.0) close (nqtg2)
        if (nh.gt.0) close (nh)
        if (ngws.gt.0) close (ngws)
        IF (nmrqa > 0 ) close (nmrqa)
        IF (imic .gt. 0 ) close (imic)
        IF (imac .gt. 0 ) close (imac)
        IF (inft .gt. 0 ) close (inft)
        IF (nvgp .gt. 0 ) close (nvgp)

        IF (DEBUG) THEN
           CLOSE(ndebug)
        END IF

        if (ngeruh .GT. 0) close (ngeruh)

        WRITE(*,9000)
        write(nres,9000)
        write(nerr,9000)
        call writeLogString(1,'Berechnung wurde ohne Fehler beendet!', 'Calculation finished without errors!','','','', &
             & '','','kalypso-na')

9000    FORMAT (/,1X, '------------------------------------------------------------------'/ &
                 & 1X, '          Berechnung wurde ohne Fehler beendet!                   '/ &
                 & 1X, '------------------------------------------------------------------')
        if (nerr.gt.0) close (nerr)
        if (nres.gt.0) close (nres)
        if (nress.gt.0) close (nress)
        call writeFooter()
        if (nerrlog.gt.0) CLOSE(nerrlog)
!af     if (nres2.gt.0) close (nres2)
        goto 667                ! Weitere Berechnung (Zustand), falls in falstart.lst angegeben

669	continue
	close(nfall)
        stop                    ! reales Ende (keine weiteren Angaben in falstart.lst)


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
99099   write(nerr,9019) 'hyd'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!',&
             & dathyd,'','','','','kalypso-na')

	write(nerr,89099)dathyd
        call writeLogString(7, 'Fehler beim oeffnen der ausgabedatei fuer hydrotopdaten', &
        & 'Error opening the output file for hydrotop data', dathyd, '', '', '', '', 'kalypso-na' )

89099	format(' fehler beim oeffnen der ausgabedatei fuer hydrotopdaten' &
      	,/' ausgabedatei:',a)
        call writeFooter()
        stop

9009    write(nerr,9019) 'npin'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )
        
        call writeFooter()
        stop

890    write(nerr,9019) 'nvgp'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )
        call writeFooter()
        stop

9089    write(nerr,9019) 'imic'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )
        call writeFooter()
        stop

9079    write(nerr,9019) 'imac'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )
        call writeFooter()
        stop

9069    write(nerr,9019) 'inft'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )
        call writeFooter()
        stop

9099    write(nerr,9019) 'nqa'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )
        call writeFooter()
        stop
9999    write(nerr,9019) 'nqg'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        write(*,9019)
        call writeFooter()
        stop
9090    write(nerr,9019) 'l3'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
9909    write(nerr,9019) 'nqif'
       call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
9900    write(nerr,9019) 'nqvs'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
99      write(nerr,9019) 'nqb'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
90      write(nerr,9019) 'ngw'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
9       write(nerr,9019) 'nt'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
8008    write(nerr,9019) 'nh'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
8088    write(nerr,9019) 'ngws'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
8800    write(nerr,9019) 'nsv'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop

8080    write(nerr,9019) 'nueb'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        call writeFooter()
        stop
9019    FORMAT(/,1X, 'Fehler beim Oeffnen der Ergebnisdatei ',a,'!'/ &
               & 1X, 'Kontrollieren Sie, ob Subdirectory out."systemzustand" eingerichte wurde!')

8       write(nerr,'(a)')' Fehlermeldungen ergaenzen!'
        call writeLogString(7,'Fehler beim Oeffnen einer Datei!','Error opening a file!', &
        & datnam, '', '', '', '', 'kalypso-na' )

        CLOSE(nres)
        call writeFooter()
	stop


9600	write(nerr,9601) datfall
        call writeLogString(7, 'File mit Ausgabesteuerung konnte nicht geoeffnet werden!', &
        & 'File with the control output could not be opened!',datfall,'','','','','kalypso-na')

        write(*,9601) datfall
        CLOSE(nerr)
        call writeFooter()
	stop
9601	FORMAT(/,1X, 'File mit Ausgabesteuerung konnte nicht geoeffnet werden.'/ &
               & 1X, 'Filename: ',a)

668	write(nerr,678) datnam
        call writeLogString(7, 'File mit Eingabesteuerung konnte nicht geoeffnet werden!', &
        & 'File with the control input could not be opened!',datnam(1:12),'','','','','kalypso-na')

        write(*,678) datnam
        CLOSE(nerr)
!       CLOSE(nres)

        call writeFooter()
	stop
678	FORMAT(/,1X, 'File mit Eingabesteuerung konnte nicht geoeffnet werden.'/  &
               & 1X, 'Filename: ',a)

        end

