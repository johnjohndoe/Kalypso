!     Last change:  KV   16 Sep 2008    3:42 pm

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

      subroutine gebiet(dt,iz,ntg,npin,nqa,l3,                &
               nbof,nqvs,nqif,nqb,ngw,                        &
               nh,ngws,nqtg1,nqtg2,nt,nkap,nmrqa,             &
               idif,izsim,idif1,                              &
               tkenn,namnal,namfal,nameptal,nquh,                    &
               nvep,nprk,nhyd,nhydtyp,                   &
               nanf,nanfstd,xanf,nanzanf,nanzanf1,            & 
               naja20,namo20,natg20,                          &
               neja20,nemo20,netg20,idatsa20,                 &
               xbil,nw_bianf,nw_boanf,pns,             	      &
               m_bil,m_flaech,m_hyd,slash,ispk1,ivor,lzpath,xjah,xwahl2,nvgp,imic,imac,inft)

!**************************************ÄNDERUNGEN***********************************************
!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!    07.05.02   WK			Subroutine boden_n in boden_alt geaendert. Berechnung
!					einfacher Bodenspeicher.´Umbenennung zweck aufraeumens!
!
!    28.02.03   SK,EP                   Neue Version ist Jahr 2000 fähig und kann bei Angabe von
!    bis                                1000001 als Anfangsdatum in der *.konfig Datei für jeden
!    04.03.03                           gerechneten Zeitschritt der Langzeitsimulation
!					Anfangswerte für die Kurzzeitsimulation erzeugen.

!    05.03.02   SK,AvD			Die Datei kann sehr groß werden, daher kann der erzeugte
!					lzsim Ordner ausgelagert und in der *.konfig  Datei der
!					entsprechende Pfad angegeben werden mit lzpath=. Liegt
!					der lzsim Ordner unter c:\daten\lzsim, ist
!					lzpath=c:\daten anzugeben.

!    05.08.03   SK,CB			Versiegelungsgrad und Gesamtfläche wird nur noch aus
!					Hydrotopdatei übernommen.
!    28.10.03:  SK,CB			xjah und xwahl2 als Real definiert, call input xjah und
!					ywahl2 übergeben

!    24.09.04
!    bis
!    30.09.04   JH			Anfangswerte können auch aus der Kurzzeitsimulation zu
!					einer beliebigen Stunde geschrieben werden. Hierzu
!					wurden beim Aufruf der snow und boden Routinen Variablen
!					hinzugefügt, da das Format der *.lzs Dateien geändert
!					wurde (zusätzlich zum Datum der Anfangswerte werden auch
!					die Stunden hinzugefügt). Das Öffnen der *.lzs Dateien zum
!					Schreiben der Anfangswerte für die Kurzzeitsimulation
!					wurde hinzugefügt.

!    21.10.04   SK			Faltung des Interflows mit ZFT (Subroutine iso) statt
!					vorher Parallelspeicher (Subroutine isov) innerhalb
!					Gebietsbearbeitungsfall ispk=7

!***************************************BESCHREIBUNG********************************************
!***************************************EIN-/AUSGABE********************************************
!******************************************VARIABLEN********************************************
USE generic_LOGGER
USE Units
USE generic_LOG_FILE
USE generic_NAConstants

IMPLICIT NONE
include 'include\param.cmn'
include 'include.geb'
include 'include\knt.cmn'
include 'include\q.cmn'


1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: gebiet.f90                     ****',/,10x,     &
      &'****     ZYKLUS:',I6,'                           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK                           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')



! 5.3.2003 Kraessig/v.Doemming
character*87 lzpath

integer  ntg,izsim,i,iz,npin,nqa,l3,nqif,nqvs,nqb,ngw,     &
         nh,ngws,nt,is,idif1,idif2,lt,        &
         idif,iend,i1,     &
         nqtg1,nqtg2,tanf,nbof,nboff,isv,nquh,nkap,      &
         nvep,nvepp,nprk,ndt,            &
         ianzzft,ianzzfp,izsimtgw,ngwss,ianz
integer  ilay,nb,nn,nqtgg,nqbb,nkapp,ngww,nmrqa

REAL     real_ns1,real_ns2,real_ns3,real_r2,real_r3,x1

real 	 peff(idim),pakt(idim),vs(idim),perk(idim),vsg,fvsum,        &
     	 h(idim), preff(idim), pintfl(idim), fn(idim), fnf(idim),     &
         fvf(idim), dt, tmeans(idim), pkorr(idim), dqa, dqaifl,       &
         gwsend,flaech,fnsum,r,ftgw,   &
         suqn,suqp,suqgw,suqi,suqa,suqv,suqt,suqo,suqvp,suinf,   &
         suqgg,suqab,suqzu,sutgw,sugws,ret4,ret2,betaa,betab,    &
         pkorra,ns2,flaechn,su1,su2,supeff,ws(idim),fperk(idim),      &
         qgwzufak,suqinf,suepot,supbest,suzugw

INTEGER  ilenlzsim, ju0nch,ivor,ispk1,tidimnutz,i2

real     uh1

character*80 tkenn,namnal,namfal,nameptal,namhydro,text0
character*1		komma
character*45	fmt

real	 vsghyd,psum1,psum2

!ccccccccccccccccccccccccccccccc
!c
!c	anfang- und endzeitpunkt, 2000 jahr faehig
!c
integer  naja20,namo20,natg20
integer	 neja20,nemo20,netg20,idatsa20

!ccccccccccc
!c variablen zeitpunkte zwischenergebnis

real	xanf(idimanf)
integer	nanf(idimanf),nanzanf,nanff,nanzanff
INTEGER :: nanfstd(idimanf),nanzanf1
real	qb_lz
!SK/CB Dokumentation 28.10.2003 Variablen xjah und xwahl2 definiert.
real 	pjsol,pdsol,pvsol,xjah,xwahl2
integer	ipver
!TODO: common block in eine Datei verschieben, die Variablen werden auch in
!      psynt.f90,kalypso-na.f90 und input.f90 definiert.
common /syndat/ pjsol,pdsol,pvsol,ipver

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	nutzung pro hydrotop
!c

real	nutzprz(2*idimnutz)
integer	anzelem,kenelem(2,2*idimnutz)
character*1	pns
real	flaech1,flaech2,flaech3
REAL :: m_nhyd(2*idimnutz)

!cccccccccccccccccccccccccc
!c
!c      Mulden-Rigolen-Paramter
!c
INTEGER :: mr_abflkno,ik
REAL :: mqab(idim),mr_ueberlauf(idim),flaechmr,mqavs(idim),	&
	mqa_ueberl(idim),sumqvs,sumueberl,hw_mr(idim)

!ccccc
!c bodendaten/Boden 

real	m_wep(maxlay,idimnutz2),m_nfk(maxlay,idimnutz2),       &
        m_bmax(maxlay,idimnutz2),m_tief(maxlay,idimnutz2),      &
        m_cin(maxlay,idimnutz2),m_cex(maxlay,idimnutz2),        &
        m_retlay(maxlay,idimnutz2),m_kf(maxlay,idimnutz2)

REAL :: m_alpha(maxlay,idimnutz2),m_n(maxlay,idimnutz2),m_l(maxlay,idimnutz2),		&
        m_vmacro(maxlay,idimnutz2)!,m_bf0(maxlay,idimnutz2),     &
REAL::  m_r(maxlay,idimnutz2)



REAL:: Imicc_real(idim)!,Imacc_real(idim)

INTEGER::vg,nvgp
INTEGER:: imic,imac,inft

character*10	kennbod(idimnutz2),kennutz(idimnutz2)
character*8	m_typ(maxlay,idimnutz2)
integer 	anzlay(idimnutz2),anznutz,anzbod,iok

!cccccccccccccccc
!c gws-aufteilungsfaktoren
real	m_f1gws(2*idimnutz),m_perkm(2*idimnutz),m_vers(2*idimnutz)

integer	n_hydid(2*idimnutz),nhyd,nhydtyp,m_hydtyp(2*idimnutz)

REAL :: perk1(idim),perk2(idim),eva1(idim),evi1(idim),sperk,sperk2,skap

!cccccccccccccccc
!c anfangsinhalte = endwerte zyklus iz-1

real	nw_bianf(jdim,2*idimnutz),nw_gws(jdim)
real	nw_boanf(jdim,2*idimnutz,maxlay)
real	m_bi0(2*idimnutz),m_bl0(2*idimnutz,maxlay)
real	xbil(21)

!cccccccccccccc
!c	ausgabe hydrotope
!c
real	m_hyd(idimhyd,7)
real	m_bil(jdim,20),m_flaech(jdim,4)

character*1 slash


data peff/idim*0/,vs/idim*0/,tmeans/idim*0/

INTEGER :: ndebug


!c der zeitschritt der n-a-simulation dt musz eine zahl derart sein,
!c dasz 24 vollstaendig durch dt teilbar ist!

ndebug = -1
IF ( DEBUG ) THEN
   CALL getLogFile( ndebug )
   WRITE (ndebug,1)
ENDIF

!c variableninitialisierung

qb_lz=0.
fvsum = 0.
fnsum = 0.
ftgw = 0.
gwsend = 0.
idat = 0
iend = 0
suqn = 0.
suqp = 0.
suqgw = 0.
suqi = 0.
suqinf = 0.
suqa = 0.
suqv = 0.
suqgg=0.
suqab=0.
suzugw=0.
sutgw=0.
sugws=0.
supbest=0.
mr_abflkno = 0
ik = 0
sumqvs = 0.
call u1null(h,idim)
call u1null(ws,idim)
call u1null(pkorr,idim)
call u1null(pintfl,idim)
call u1null(perk,idim)
call u1null(preff,idim)
call u1null(pakt,idim)
call u1null(fn,idim)
call u1null(fperk,idim)
call u1null(fnf,idim)
call u1null(fvf,idim)
call u1null(qaifl,idim)
call u1null(qavs,idim)
call u1null(qa,idim)
call u1null(qtgw,idimtgw)
call u1null(m_nhyd,(2*idimnutz) )
call u0null(m_hydtyp,(2*idimnutz) )
call u1null(mqavs,idim)
call u1null(mqab,idim)
call u1null(hw_mr,idim)
call u1null(mqa_ueberl,idim)

!******************************************ANWEISUNGEN******************************************

!c  oeffnen der scratch-datei, die zum erstellen der langzeitsim.datei
!c  mit verschiedenen teilgebietsdaten als initialisierungswerte fuer
!c  kurzzeitsimulationen benoetigt wird

write(*,102) ntg
write(nres,102) ntg
write(nerr,102) ntg

102   FORMAT (/,' Auswertung von Gebiet:                  ',i5)

DO isv=1,imax
   IF (nr(isv) == 0) THEN
	  imax = isv
	  GOTO 111
       ENDIF
       IF (nr(isv).eq.ntg) GOTO 111
ENDDO
      imax      = imax + 1
      isv      = imax
111   i         = isv
      nr(isv)     = ntg
      lt = 0
      r = float(ntg)
      call s0rc(text0,r,lt)

ilenlz = ju0nch(lzpath)
IF (isim == 1 .AND. ilenlz > 0) THEN
    fnam=lzpath
    ilenlz = ju0nch(fnam)
    fnam(ilenlz+1:ilenlz+7)='\lzsim'//slash
    ilenlz = ju0nch(fnam)
ELSE
    fnam=pathn
    fnam(ilend+1:ilend+6)='lzsim'//slash
    ilenlz = ilend+6
ENDIF

ilenlzsim = ilenlz-1

!c  projekte aabach beruecksichtigt 5-stellige gebietsnummern
IF ( lt > 4 .AND. ileng > 3) then
   fnam(ilenlz+1:ilenlz+ileng-1)=namger(1:ileng-1)
   ilenlz = ilenlz+ileng-1
ELSE
   fnam(ilenlz+1:ilenlz+ileng)=namger(1:ileng)
   ilenlz = ilenlz+ileng
ENDIF

fnam(ilenlz+1:ilenlz+lt)=text0(1:lt)
ilenlz = ilenlz+lt
fnam(ilenlz+1:ilenlz+4) = '.lzs'
ilenlz=ilenlz+4

IF ( ilenlz > 80 )then
   write(nerr,'(a,a,a)')'tree-name fuer den file',       &
      			fnam,'mehr als 80 zeichen.'
   write(nerr,'(a)')'programm-abbruch'

   call writeLogString(7,'Pfad fuer eine Datei hat mehr als 80 Zeichen!', 'Path for a file with more than 80 characters!', &
        & fnam,'','','','','gebiet')
   call writeFooter()
   stop
ENDIF

!ccccccccccccccccccc
!c	oeffnen des datenfile fuer  L Z S I M - werte

iok= 0
IF ( isim == 0 ) then                                         	! Langzeit
   IF ( iz == 1 .AND. nanzanf >= 0 ) then                       ! 1. Zyklus, Anfangswerte sollen geschrieben werden
      call inp_lzsim(fnam,ilenlzsim,1,nanff)                    ! *.lzg wird neu geoeffnet und überschrieben
   ELSE                                                         ! Zyklus > 1
      call inp_lzsim(fnam,ilenlzsim,2,nanff)                    ! *.lzg wird geoeffnet und bis zum Ende gelesen (zum weiter schreiben)
   ENDIF
ELSE                                                            ! Einlesen *.lzs in Kurzzeit
   iok=1
   call inp_lzsim(fnam,ilenlzsim,0,nanff)                       ! Öffnen der *.lzs-Datei, falls sie vorhanden ist
   if ( nanff > 0 .AND. pns /= 's' ) then
      call inp_anf(fnam,idatsa20,nanff,h01(isv),w01(isv), &     ! Einlesen der Anfangswerte *.lzs
                   nw_gws(isv),qb_lz,m_bi0,m_bl0,iok)
      close ( nanff )
   else
      write( nres,499 ) fnam
   endif
   IF ( nanzanf1 > 0 ) THEN                                     ! neue Anfangswerte sollen aus der KZ-Sim geschreiben werden.
       IF ( nanff == 0 ) THEN                                   ! *.lzs ist nicht vorhanden
          call inp_lzsim(fnam,ilenlzsim,1,nanff)                ! neue Datei erzeugen und öffnen
       ELSE                                                     ! *.lzs ist bereits vorhanden
          call inp_lzsim(fnam,ilenlzsim,1,nanff)                ! alte Datei öffnen, schließen und neu erzeugen
       END IF
   END IF
endif

499   FORMAT(/ 1X, 'Warnung!' /&
             & 1X, 'File ',a /&
             & 1X, 'mit Anfangsbedingungen fuer die Kurzzeitsimulation existiert nicht!'/      &
             & 1X, '(Schnee, Bodenfeuchte, Grundwasserstand, Interzeptionsfeuchte)'/       &
             & 1X, 'Vor der Kurzzeitsimulation sollte eine Langzeitsimulation durchgefuehrt werden.')

qgwzufak=0.
!cccccccccc
!c  einlesen aller daten der teilgebiete

call input(dt,iz,ntg,npin,peff,tmeans,vs,fn,	&
     	   vsg,ianzzft,flaech,idif,             &
     	   namnal,namfal,nameptal,              &
     	   tanf,qgwzufak,             &
           naja20,namo20,natg20,                &
           neja20,nemo20,netg20,                &
           namhydro,pns,slash,xjah,xwahl2)


if(vsg.lt.0.) then
   vsg=-vsg
endif

! ispk = 7
!----------
CALL inp_hydro(namhydro,ntg,                      &
                     flaech1,flaech2,flaech3,		               &
                     anzelem,kenelem,anzbod,anznutz,                 &
                     kennbod,kennutz,nutzprz,                        &
                     m_perkm,m_f1gws,m_vers,                  &
                     n_hydid,m_hydtyp,m_nhyd,nhydtyp)
!SK, CB Fläche und Versiegelungsgrad soll nur noch aus der Hydrotopdatei übernommen werden.
	 vsghyd=flaech1/flaech3
	 vsg=vsghyd

!SK,CB flaech wird in der Routine input in km² umgerechnet, daher hier auch erforderlich, weil flaech zu flaeche3 gesetzt wird.
	 flaech3=flaech3*1.e-6
	 flaech=flaech3

CALL inp_bod_b(isim,iz,pathn,ilend,                 	    &
               anzbod,kennbod,anzelem,kenelem,              &
               anzlay,m_wep,m_nfk,m_bmax,m_tief,            &
               m_cin,m_cex,m_kf,m_retlay,nw_boanf,m_typ,    &
               isv,fko,bmax,cinh,cind,cex,retlay,banf,      &
               slash,m_alpha,m_n,m_l,m_vmacro,m_r,vg)  !! ADDED BY KV

!-----------------------------------------------------------------------------------------------
!                                                                                              -
!                     	Uebernahme der Anfangsbedingungen                                      -
!                                                                                              -
!-----------------------------------------------------------------------------------------------

IF ( (dt > 23.5 .AND. iz==1 ) .OR. iok == 1) THEN    			 ! LZ-sim 1. Zyklus oder KZ-sim *.lzs unvollständig (siehe inp_anf)
									 ! anfangsbedingungen aus eingabefile *.geb
      WRITE (nres,9000)
9000  FORMAT (/ 1X, 'Anfangswerte: aus Eingabe-Dateien *.geb'/&
              & 1X,  'Parameter: aigw, bianf, banf'/&
              & 1X, 'Grund: 1. Zyklus Langzeitsimulation oder Anfangswertdatei *.lzg unvollstaendig.')
      nw_gws(isv)=aigw

! SK Anfangswerte für Bodenfeuchte werden in der inp_bod_b bestimmt,
! je Hydrotop und Bodenlayer
      DO nn=1,anzelem
        nb=kenelem(1,nn)
        nw_bianf(isv,nn)= bianf
      ENDDO
ELSE                                                              ! LZ-sim > 1. Zyklus oder KZ-sim Werte aus *.lzs
!c anfangsbedingungen als endwert letzter zyklus
!c	oder aus lzsim (*.lzs) Datei
      WRITE (nres,9001)
9001  FORMAT (/ 1X, 'Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus')
      h0=h01(isv)
      w0=w01(isv)
      aint  = aint1(isv)
      IF(ispk == 7.) THEN
	 IF (dt < 23.5.AND. iok == 0 )THEN
	       do nn=1,anzelem
	       	  nb=kenelem(1,nn)
		  nw_bianf(isv,nn)=m_bi0(nn)
		  do ilay=1,anzlay(nb)
		     nw_boanf(isv,nn,ilay)=m_bl0(nn,ilay)
                  ENDDO
               ENDDO
         ENDIF
      ELSE
	 IF(dt < 23.5) THEN
	    banf(1)= m_bl0(1,1)
	    bianf = m_bi0(1)
	 ELSE
	    banf(1)= nw_boanf(isv,1,1)
	    bianf = nw_bianf(isv,1)
	 ENDIF
	 aigw  = nw_gws(isv)
      ENDIF
ENDIF

write (nres,'(a,f7.3)') 'Anfangswerte   h01 =',h01(isv)
write (nres,'(a,f7.3)') '               w01 =',w01(isv)
write (nres,'(a,f7.3)') '               bo1 =',banf(1)
write (nres,'(a,f7.3)') '            bianf1 =',bianf
write (nres,'(a,f7.3)') '             aint1 =',aint
write (nres,'(a,f7.3)') '             aigw1 =',aigw

!c    TODO:  Hier müsste der neue time-loop eingefügt werden
nanzanff=nanzanf

!c    s c h n e e r e t e n t i o n
CALL snow(idif,dt,peff,pns,tmeans,h,ws,pkorr,idif2,isim,         &
         wwo,wwmax,snotem,snorad,h0,w0,              &
         nanf,nanfstd,xanf,nanzanff,nanzanf1,nanff,nres,idatsa20,naja20)

!c SK/EP 28.02.03
!c idatsa20 wird in boden übergeben, damit dort Anfangswerte zu jedem Zeitschritt der Langzeitsimulation
!c generiert werden können.
IF ( idif2 > idif ) THEN
   is=idif2-idif
   idif=idif2
   WRITE(nres,489) is
   489   format(/' schneedecke ist innerhalb des angegebenen zeitraums ',   &
                 'nicht vollstaendig geschmolzen!',/' simulationszeitraum wird' ,   &
                 ' um ',i5, ' zeitschritte verlaengert !')
ENDIF



!c    b o d e n w a s s e r h a u s h a l t

nboff=0
nvepp=0
nvett=0
ngwss=0
nqtgg=0
nqbb=0
nkapp=0
ngww=0
DO is=1,ingmax
   IF (ntg == ntgout(is) ) THEN
       nboff=nbof
       nvepp=nvep
       nvett=nvet
       ngwss=ngws
       nqbb=nqb
       nkapp=nkap
       ngww=ngw
       nqtgg=nqtg1
   ENDIF
ENDDO

is=idif

!c  nachlauferfassung von verdunstung aus boden und perkulation fuer
!c  kurzzeitsimulation auch ueber angegebenen simulaitonszeitraum hinaus

fvsum = flaech * vsg
fnsum = flaech * (1-vsg)
ftgw = flaech * klupor

!c  fuer vollstaendig versiegelte gebiete koennen bodenroutinen
!c  ueberstrungen werden
if( abs( fnsum + fvsum - flaech ) / flaech > 0.1 ) THEN
    write(nres,'(//a)') 'fehler in den teilflaechen'
    write(nres,'(//a,f10.2)') 'vers. flaeche  =',fvsum
    write(nres,'(a,f10.2)') 'natur. flaeche =',fnsum
    write(nres,'(a,f10.2,//)') 'gesamtflaeche  =',flaech
ENDIF

IF(vsg < 0.999) THEN

   nanzanff=nanzanf
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	bestimmung der retentionskonstante interflow/hydrotop-boden
!c
!c	fuer retlay<0:	wert retlay direkt uebernommen
!c		sonst:	faltung unit-hydrogaph interflow X retlay
!c
   DO nb=1,anzbod
      ndt=10
      DO ilay=1,anzlay(nb)
         IF(m_retlay(ilay,nb) > 1.) THEN
            m_retlay(ilay,nb)=1.
         ENDIF
      ENDDO
   ENDDO

   !c idatsa20 wird in boden übergeben, damit dort Anfangswerte zu jedem Zeitschritt der Langzeitsimulation
   !c generiert werden können.
         CALL boden(ntg,iz,dt,is,pns,pkorr,			&
                   vs,preff,pintfl,perk1,perk2,eva1,evi1, 	&
                   suqi,suqinf,                                 &
                   sperk,sperk2,skap,			    	&
                   xbil,anzelem,kenelem,nutzprz, 		&
              	   anzlay,m_wep,m_nfk,m_bmax,m_tief, 		&
                   m_cin,m_cex,m_retlay,m_typ, 			&
                   m_perkm,m_f1gws,m_vers,   		  	&
              	   n_hydid,nhyd,nhydtyp,                  	&
              	   isv,nw_bianf,nw_boanf,nw_gws, 	  	&
              	   anzbod,anznutz,kennbod,kennutz, 		&
              	   pathn,ilend,naja,namo,natg,l3,nboff,nvepp, &
              	   nanf,nanfstd,xanf,nanzanff,nanzanf1,nanff, &
              	   nkapp,fnsum,                                 &
              	   m_bil,m_flaech,m_hyd,ivor,slash,  	&
              	   idatsa20,naja20,				&
                   m_kf,m_alpha,m_n,m_l,m_vmacro, &
                   Imicc_real,m_r,vg,nvgp,imic,imac,inft,&
                   cind,cinh)


         nanzanff=nanzanf


         CALL calc_mrs(ntg,iz,dt,is,pns,pkorr,	             	    &
                      vs,mr_ueberlauf,pintfl,perk1,perk2,eva1,evi1, &
                      mqab,mr_abflkno,         		    	    &
                      suqi,suqinf,              		    &
                      xbil,anzelem,		                    &
                      anzlay,m_vers,0,            		    &
                      nhyd,nhydtyp,isv,nw_bianf,		    &
                      pathn,ilend,naja,namo,natg,                   &
                      l3,nanf,nanfstd,xanf,nanzanff,nanzanf1,nanff,  &
                      fvsum,fnsum,flaechmr,m_bil,m_hydtyp,ivor,slash,             &
                      idatsa20,naja20,m_nhyd,namger,LEN_TRIM(namger),hw_mr)

         nanzanff=nanzanf
   	 CALL gwsp(ntg,dt,is,isv,ivor,iz,perk1,perk2,anzelem,m_f1gws,      &
   		  nw_gws,klupor,tgwzu,nutzprz,pns,naja20,m_flaech,sperk,   &
                  sperk2,skap,idatsa20,nanfstd,nanf,nanzanff,nanzanf1,xanf, &
                  nanff,m_bil,xbil,fnsum,ngws,nqb,ngw,nqtgg)
         nanzanff=nanzanf
   IF(iz == 1) THEN
      m_flaech(isv,3)=fnsum
      m_flaech(isv,4)=fvsum
   ENDIF

   xbil(15)=xbil(15)+fnsum
   xbil(12)=xbil(12)+fvsum

   suqi=suqi*fnsum*1.e+03
   suinf=suqinf*fnsum*1.e+03

!cccccccccccccccccccccccccccccccccccc
!c  ausgabe boden von preff(overl) pintfl(interf) u. perk(perk) in mm
!c	gebiet setzt werte in mm/h voraus!
!c	faltung des oberflaechenabflusses mit retob --> qbm
!c	

   DO i1=1,is
      preff(i1)=preff(i1)/dt
      perk(i1)=perk(i1)/dt
      pintfl(i1)=pintfl(i1)/dt
   ENDDO
   suqa=0.

   CALL iso(ntg,dt,izsim,idif,ianzzft,retob,fn,preff,qa,dqa,suqa,        &
            fnsum,1.,nquh)

   WRITE(nres,'(/a)')'GEBIET: Abflusswelle Interflow:'
   IF (retint.gt.0.) THEN
	   ! Parameter für Parallelspeicher
       real_ns1 = 3.
       real_ns2 = 0.
       real_ns3 = 3.
       real_r2  = 0.
       real_r3  = 0.
       betaa    = 1.
       betab    = 0.

       ! Faltung mit ZFT für Interflow statt Parallelspeicher
       CALL iso(ntg,dt,izsim,idif,ianzzft,retint,fn,pintfl,qaifl,dqaifl, &
                suqi,fnsum,1.,nquh)
   ELSE
       DO i1=1,is
          qaifl(i1)=pintfl(i1)*fnsum/3.6
       ENDDO
   ENDIF
ENDIF ! wenn vsg < 0.999

!cccccccccccccccccccccccccccccccccccccccccccc
!c
!c
!c    parallele speicherkaskade oder linearspeicher fuer 
!c
!c    a b f l u s z   v e r s i e g e l t e r   f l a e c h en 

!c   10% verlust durch verdunstung von der oberflaeche
!C   nach neueren Untersuchungen von Frau Brünung ist der Verlust 20 %
IF (vsg > 0.0001) THEN
      pkorra=0.
      psum1=0.
      psum2=0.
      DO i=1,idif
         IF (vsg < 0.9999) then
	       IF (rintmx > 0 .AND. dt > 23.5) THEN
	           pakt(i)=(pkorr(i)-rintmx/dt)*.8
	           IF (pakt(i).lt.0.) THEN
                    pakt(i)=0.
               ENDIF
	       ELSEIF(tint > 0. ) THEN
	          psum1=psum1+pkorr(i)*dt
	           IF (psum1 > tint) THEN
	               IF (psum2.lt.tint) THEN
		              pakt(i)=(psum1-tint)/dt*0.8
		           ELSE
		              pakt(i)=pkorr(i)*0.8
		           ENDIF
	           ELSE
		          pakt(i)=0.
	           ENDIF
	           psum2=psum1
           ELSE ! Da an der TUHH rintmx und tint immer Null sind wird nur diese Zeile erreicht
                ! FRAGE: Was bedeuteten die Formeln wenn rintmx und tint nicht Null sind ??
                pakt(i)=pkorr(i)*0.8                  
	       ENDIF
         ELSE
	       pakt(i)=pkorr(i)-vs(i)
	       IF (pakt(i).lt.0.) THEN
                 pakt(i)=0.
           ENDIF
	       pkorra=pkorra+pakt(i)
         ENDIF
      ENDDO

      ! Abschlagsknoten der MR suchen
      IF ( mr_abflkno > 0 ) THEN
         kno_mr:DO ik=1,ikmax
                  IF( knot(ik) == mr_abflkno ) EXIT
         END DO kno_mr
      END IF
      ! Addition des MR-Abflusses zum versiegelten Abfluss
!      korr_mr:DO i=1, idif
!                IF( mr_abflkno == 0) THEN ! Zuschlag zum versiegelten Abfluss
!                   pakt(i)= mqab(i)+ mr_ueberlauf(i)
!                ELSE !Umrechnung in m**3/s und abschlag an knoten
!                   qg(ik,i1) = ( mqab(i)+ mr_ueberlauf(i) )*1e-9*3600
!                ENDIF
!      ENDDO korr_mr   
      write(nres,'(/,a)') 'GEBIET: Abfluszwelle versiegelte Flaechen'

      ! Parameter für Parallelspeicher
      real_ns1 = 3.
      real_ns2 = 0.
      real_ns3 = 3.
      real_r2  = 0.
      real_r3  = 0.
      betaa    = 1.
      betab    = 0.

      ! Abfluss von Versiegelten Flächen in das Kannalnetz
      call isov(dt,izsim,idif,retvs,real_r2,          &
                      real_r3,real_ns1,real_ns2,real_ns3,    &
                      betaa,betab,            &
                      fvsum-(flaechmr*1e-6),pakt,qavs,suqv,ntg)

      IF ( mr_abflkno > NO_MR_ELEMENT ) THEN
         ! Abfluss aus Mulden-Rigolen in das Kannalnetz
       call isov(dt,izsim,idif,retvs,real_r2,          &
                      real_r3,real_ns1,real_ns2,real_ns3,    &
                      betaa,betab,            &
                      flaechmr*1e-6,mqab,mqavs,sumqvs,ntg)
	  ENDIF
       ! Ueberlauf aus der Mulde in das Kannalnetz
       call isov(dt,izsim,idif,retvs,real_r2,          &
                      real_r3,real_ns1,real_ns2,real_ns3,    &
                      betaa,betab,            &
                      flaechmr*1e-6,mr_ueberlauf,mqa_ueberl,sumueberl,ntg)
	  

       DO i1=1,idif
!          qavs(i1)=qavs(i1) + mqa_ueberl(i1)
!          ! wenn kein Knoten spezifiziert ist wird der MR-Abfluss dem vers. Abfluss hinzugefügt
          IF ( mr_abflkno == 0 ) THEN
             qavs(i1) = qavs(i1) + mqavs(i1)
!          ! an der im MR-Datei spezifizierten Knoten im Netz leiten
          ELSE
             qg(ik,i1) = qg(ik,i1) + mqavs(i1)
          END IF
       ENDDO

      ! TODO: Was muss hier von den MR addiert werden
      xbil(10)=xbil(10)+suqv+sumueberl+sumqvs
ENDIF ! berechnung versiegelte Flächen


IF (isim.eq.0) THEN
    iend = idif         ! langzeitsimulation
ELSE
    iend = izsim 	! kurzzeitsimulation
ENDIF

IF (vsg < 0.9999 ) THEN
!c    linearspeicher fuer den kluft-grundwasserleiter

  IF ( ABS( retklu ) > 1.e-02 ) THEN
      write(nres,'(/,a)')  'GEBIET: Abfluszwelle Kluft-Grundwasserleiter'
      izsimtgw=izsim
      real_ns1 = 3.
      real_ns2 = 0.
      real_ns3 = 3.
      real_r2  = 0.
      real_r3  = 0.
      betaa    = 1.
      betab    = 0.

      CALL isov(dt,izsimtgw,iend,retklu,real_r2,    	&
                real_r3,real_ns1,real_ns2,real_ns3,    	&
                betaa,betab,ftgw,tgwzu,qtgw,suqt,	&
                ntg) !Änderung CB: 12.6.2008 hier fehlte Formalparameter ntg
  ENDIF
ENDIF

!c    abspeichern der endwerte schneehoehe, bodenfeuchte,
!c    interzeption, interflow und grundwasserstand, die
!c    als anfangswerte des naechsten zyklus fungieren
IF (isim == 0) THEN
    h01(isv)   = h(idif)
    w01(isv)   = ws(idif)
    aint1(isv) = pintfl(idif)
ENDIF

ispk1=ispk

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c    ergebnisdateien   q a . d a t
!c                      q i f . d a t
!c                      q v s . d a t
!c                      q t g . d a t
!c                      s c h n e e . d a t
!c                      t e m p . d a t
!c                      p r k . d a t

IF ( nqa > 0 )THEN
    DO is = 1,ingmax
       IF ( ntg == ntgout(is) ) THEN
          write(nqa,'(/3(i8)/)') ntg,iz,iend
          write(nqa,'(10(f8.3,1x))') ( qa(i),i=1,iend )
       ENDIF
    ENDDO
ENDIF

IF ( nqif > 0 ) THEN
    DO is = 1,ingmax
       IF ( ntg == ntgout(is) ) THEN
  	  write(nqif,'(/3(i8)/)') ntg,iz,iend
	  write(nqif,'(10(f8.3,1x))') ( qaifl(i),i=1,iend )
       endif
    ENDDO
ENDIF

!c     if(npin.gt.0) then
!c	 do 1015 is = 1,ingmax
!c	    if (ntg .eq. ntgout(is)) then
!c		write(npin,'(/(a)/)') 'niederschlag nach schneeschmelze'
!c		write(npin,'(/3(i8)/)') ntg,iz,iend
!c		write(npin,'(10(f6.2,1x))') (pkorr(i),i=1,iend)
!c	    endif
!c 1015    continue
!c     endif

IF ( nqvs > 0) THEN
    DO is = 1,ingmax
       if ( ntg == ntgout(is) ) THEN
	  WRITE(nqvs,'(/3(i8)/)') ntg,iz,iend
	  WRITE(nqvs,'(10(f8.3,1x))') ( qavs(i),i=1,iend )
       ENDIF
    ENDDO
ENDIF

IF ( nh > 0 ) THEN
    DO is = 1,ingmax
       IF ( ntg == ntgout(is) ) THEN
	  write(nh,'(/3(i8)/)') ntg,iz,idif1
	  write(nh,'(10(f6.0,1x))') ( h(i),i=1,idif1 )
       ENDIF
    ENDDO
ENDIF

IF ( nqtg2 > 0 ) THEN
    DO is = 1,ingmax
       if ( ntg == ntgout(is) ) THEN
	  WRITE (nqtg2,'(/3(i8)/)') ntg,iz,iend
	  WRITE (nqtg2,'(10(f8.3,1x))') ( qtgw(i),i=1,iend )
       ENDIF
    ENDDO
ENDIF

if( nt > 0 ) THEN
   DO is = 1,ingmax
      IF ( ntg == ntgout(is) ) THEN
	 WRITE(nt,'(/3(i8)/)') ntg,iz,idif1
	 WRITE(nt,'(10(f6.2,1x))') ( tmeans(i),i=1,idif1 )
      ENDIF
   ENDDO
ENDIF

IF( nprk > 0 ) THEN
   DO is = 1,ingmax
      IF ( ntg .eq. ntgout(is)) THEN
	 WRITE(nprk,'(/3(i8)/)') ntg,iz,idif1
	 WRITE(nprk,'(10(f8.3,1x))') ( perk(i)*dt,i=1,iend )
      ENDIF
   ENDDO
ENDIF

IF (nmrqa > 0)then
   DO is = 1,ingmax
      if (ntg == ntgout(is)) then
	 write(nmrqa,'(/3(i8)/)') ntg,iz,iend
	 write(nmrqa,'(10(f8.3,1x))') (mqab(i) ,i=1,iend)
     
      end if
   END DO
ENDIF

IF (DEBUG)then
   WRITE (ndebug,*)'----------- Muldenrigolen-Ueberlauf------------- m**3/s'
   DO is = 1,ingmax
      if (ntg == ntgout(is)) then
	 write(ndebug,'(/3(i8)/)') ntg,iz,iend
	 write(ndebug,'(10(f8.3,1x))') (mqa_ueberl(i) ,i=1,iend)
      end if
   END DO   
   WRITE (ndebug,*)'----------- Muldenrigolen-Ueberlauf------------- mm/h'
   DO is = 1,ingmax
      if (ntg == ntgout(is)) then
	 write(ndebug,'(/3(i8)/)') ntg,iz,iend
	 write(ndebug,'(10(f8.3,1x))') (mr_ueberlauf(i) ,i=1,iend)
      end if
   END DO   
   WRITE (ndebug,*)'----------- Muldenrigolen-Abfluss------------- mm/h'
   DO is = 1,ingmax
      if (ntg == ntgout(is)) then
     write(ndebug,'(/3(i8)/)') ntg,iz,iend
     write(ndebug,'(10(ES16.6,1x))') (mqab(i) ,i=1,iend)
      end if
   END DO
    WRITE (ndebug,*)'----------- Muldenrigolen-Wasserstand------------- mm/h'
    DO is = 1,ingmax
       if (ntg == ntgout(is)) then
           write(ndebug,'(/3(i8)/)') ntg,iz,iend
           write(ndebug,'(10(f8.3,1x))') (hw_mr(i) ,i=1,iend)
       end if
    END DO
ENDIF

IF ( nanff > 0 ) THEN
   close (nanff)
ENDIF

ianz = iend

IF (DEBUG) THEN
   WRITE (ndebug,2)
END IF
end
