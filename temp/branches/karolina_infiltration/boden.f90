!     Last change:  KV   16 Sep 2008    4:17 pm

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


      subroutine boden(ntg,iz,dt,idif,pns,peff,                     &
                       vs,pover1,pint1,perk1,perk2,eva1,evi1,	    &
                       supint1,supinf1,				    &
                       sperk,sperk2,skap,xbil,              &
                       anzelem,kenelem,nutzprz,                     &
                       anzlay,m_wep,m_nfk,m_bmax,m_tief,            &
                       m_cin,m_cex,m_retlay,m_typ,                  &
                       m_perkm,m_f1gws,m_vers,               &
                       n_hydid,nhyd,nhydtyp,                        &
                       isv,nw_bianf,nw_boanf,nw_gws,                &
                       anzbod,anznutz,kennbod,kennutz,              &
                       pathn,ilen,naja,namo,natg,                   &
                       l3,nbof,nvep,                          &
                       nanf,nanfstd,xanf,nanzanf,nanzanf1,nanff,       &
                       nkap,fnsum,                &
                       m_bil,m_flaech,m_hyd,ivor,slash,   &
	               idatsa20,naja20,					&
                       m_kf,m_alpha,m_n,m_l,m_vmacro,&
                       Imicc_real,m_r,vg,nvgp,immic,immac,inft,&
                       cind,cinh)

      USE generic_LOGGER
      USE generic_LOG_FILE
      USE generic_NAConstants
      USE Units
      IMPLICIT  NONE
      include      'include/param.cmn'
      include      'include.bod'
      include      'include/is.cmn'

1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: boden.f90                      ****',/,10x,     &
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


!**************************************�NDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     ?         WK                      keine inhaltliche Aenderung, nur der Name!

!     28.02.03  SK,EP                   Neue Version ist Jahr 2000 f�hig und kann bei Angabe von
!     bis                               1000001 als Anfangsdatum in der *.konfig Datei f�r jeden
!     04.03.03                          gerechneten Zeitschritt der Langzeitsimulation
!                                       Anfangswerte f�r die Kurzzeitsimulation erzeugen.

!     24.09.04  JH                      Anfangswerte k�nnen auch aus der Kurzzeitsimulation zu
!     bis                               einer beliebigen Stunde geschrieben werden. Hierzu wurde
!     30.09.04                          das Format des *.lzs Files ge�ndert, da hier zus�tzlich
!					zum Datum der Anfangswerte jetzt auch die Stunden
!					hinzugef�gt wurden. Hinweis: die �nderungen beziehen
!					sich nur auf das Schreiben der Anfangswerte zu bestimmten
! 					Daten und deckt f�r die Kurzzeitsimulation nicht die
!                                       M�glichkeit der fortlaufenden Speicherung der Anfangswerte
!					ab. Im Zuge dieser �nderung mussten die �bergabeparameter
!  					dieser Routine durch die Variablen nanfstd,nanzanf1,
!					naja20 erweitert werden.

!     21.10.04  SK			Formataenderung, damit BOf in GraphicTool darstellbar.

!     09.03.06  CK                      Alle unn�tigen Kommentare gel�scht sowie alte code zeilen.
!     bis                               Die Grundwasserberechnung in eine eigene subroutine verschoben,
!     15.03.06                          sowie den Kappilarenaufstieg gel�scht. Die alten F�lle f�r das
!                                       Gebietsdatei gel�scht es wird nur noch mit ispk == 7 gerechnet.
!                                       Umstellung auf "implicit none".
!
!
!    22.08.2006 KV                      Addition of subroutine infmm (infiltration macro-micro pores).

!***************************************BESCHREIBUNG********************************************

!***************************************EIN-/AUSGABE********************************************
!c
!c	eingabe	peff	[mm/h]	effektivniederschlag
!c		vs	[mm/h]	potentielle transpiration
!c              anzlayy                 Anzahl der Bodenschichten in einem bestimmten Hydrotop.
!c              perkl                   Perkolation in einer Schicht in einem bestimmten Hydrotop
!c              pover(nn)       [mm]    Oberfl�chenabfluss je Hydrotop und je Zeitschritt t im Zyklus noch
!c                                      nicht auf die nat. Fl�che des Hydrotpes bezogen.
!c              suevalay(nn,ilay)       Summe der Evaporation pro Hydrotop (nn) und je Bodenschicht (ilay).
!c
!c      Detailausgabe Hydrotop
!c              suphort(nn)             Summe des Oberfl�chenabflusses (noch nicht auf nat. Fl�che bezogen)
!c                                      je Hydrotop f�r jeden Zyklus.
!c		supstau(nn,ilay)        Die Summe des Stauwassers je Hydtrotop und Schicht f�r jeden Zyklus.
!c		supbod(nn,ilay)         Die Summe der Differenz des Stauwassers mit und ohne Korrekturwert
!c                                      aus dem Teilgebiet f�r jeden Zyklus
!c
!c
!c	eingabe	peff   		[mm/h]	effektivniederschlag
!c		vs		[mm/h]	potentielle transpiration
!c
!c	ausgabe	zeitreihen
!c		pover1(t) [r,r]	[mm]	overlandflow summiert �ber den Zeitschritt t bezogen auf nutzprz(nn)
!c		pint1(t)  [r,r]	[mm]	interflow                 -"-
!c		perk1(t)  [r,r]	[mm]	perkolation               -"-

!c	     summenwerte
!c		suevi		[mm]	verdunstung interzeption
!c		sueva		[mm]	verdunstung aus bodenspeicher
!c		supint		[mm]	summe interflow
!c		suboden		[mm]	differenz bodenspeicher
!c              supob(nn)       [mm]    Summe des Niederschlags im Zyklus pro Hydrotop
!c              supob1          [mm]    Summe des Niederschlags im Zyklus �ber das gesamte
!c                                      Teilgebiet bezogen die nat�rliche Fl�che ->
!c					supob1 = supob(nn)*nutzprz(nn)
!c
!c      Zeitschrittmanagement
!c              dt              [h]     Globaler Zeitschritt in Stunden
!c              dtn             [h]     interner Zeitschritt der Bodenroutine (Berechung Bodenfeuchte)

!******************************************VARIABLEN********************************************


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!CK Umstellung impicit none
! Groessen f�r Zeitschrittmanagement

REAL :: dt,dtmax,dtn
REAL :: itimevor
INTEGER :: t,it,itime,idif,ttemp
REAL :: fnsum

! Datum
INTEGER :: naja20,idatsa20,isstd,ijaha,idatum,itag,imona,ijahr,naja,namo,natg
INTEGER :: sjahr,smona,stag,sdatum

! Niederschlag
REAL :: prinp,prest,peff(idim),pri,vs(idim),supre

! Boden- und Nutzungsparameter
INTEGER :: ntg,ilay,nb,nv
INTEGER :: anzelem,kenelem(2,2*idimnutz),anzbod,anznutz,hydTyp
REAL :: xt,xtb,bimax,eptt,xcin,xcex,boderr,xv,xmax

! Dateikan�le und read/open Error-codes
INTEGER :: ju0gfu,l3,nkap,nvep,nbof,ierr

! Schleifenz�hler
INTEGER :: isv,iz,ianf,is,nast,nn,i1,i2,i

! Tempor�re Variablen
INTEGER :: lastchar,ndebug

! variablen mit  unbekannter Verwendung
REAL :: fakls = 31.536! irgend ein faktor wird f�r die Ausgabe der Hydrotop-Datei auf 31.536 gesetzt. Wieso ???
INTEGER :: idnr ! Variable die mit einer Hydrotop-ID gef�llt wird ( Ausgabe Hyd-Datei)
LOGICAL	:: lop ! zeigt an ob eine Hyd.Datei-Ausgabe erfolgen soll
INTEGER :: ivor

!ccccccccccccccccccccc
!c
!c 	Bodenhaushalt: Wassermengen im Bodenspeicher
!c

! eavapotransp. im TG pro Zeitschritt t,eavapotransp. pro Hyd. internen Zeischritt dtn, verdunst. auf TG pro Zeitschritt t, versdunst.
REAL, INTENT(OUT) ::	eva1(idim),evi1(idim)
REAL :: eva(2*idimnutz),evi(2*idimnutz)
! perkolation,interlfow,oberflaechenabfluss f�r jedes Hydrotop pro internen Zeischritt dtn
REAL ::	perk(2*idimnutz),pint(2*idimnutz),pover(2*idimnutz) 

! grundwasserneubildung,tiefengw-leiter,interflow, oberflaechenabfluss. aggergiert auf TG pro Zeitschritt t
REAL, INTENT(OUT) ::	pint1(idim),pover1(idim),perk1(idim),perk2(idim)
real	pstau(maxlay),pintlay(maxlay)

! Bodenhaushalt pro Layer
INTEGER :: anzlay(idimnutz2),anzlayy
REAL ::	suintlay(2*idimnutz,maxlay),evpotlay(maxlay),eval(maxlay),perkl(2*idimnutz),inf
REAL ::	b(maxlay,0:idim),bof(2*idimnutz,maxlay)

! Anfangswerte Bodenfeuchte und Interzeptionspeicher
REAL ::	bofstart(2*idimnutz,maxlay),bistart(2*idimnutz)
REAL ::	bi(2*idimnutz),bianf,biend

! Bodenfeuchteaenderung f�r jeden Layer
REAL ::	bdif(maxlay),bofsum!,bofant(maxlay)

! Bilanzgroessen
REAL ::	suevi(2*idimnutz),sueva(2*idimnutz),supob(2*idimnutz)
REAL ::	superk(2*idimnutz),supgws(2*idimnutz),suptgw(2*idimnutz)
REAL ::	supstau(2*idimnutz,maxlay),supbod(2*idimnutz,maxlay)
REAL ::	supsat(2*idimnutz,maxlay),suevalay(2*idimnutz,maxlay)
REAL ::	supover(2*idimnutz),supint(2*idimnutz)
REAL ::	suphort(2*idimnutz),superklay(2*idimnutz,maxlay)
REAL :: sperk,sperk2,suepot,suepotv,supob1,suinf,sudif,sudif1
REAL :: suevi1,sueva1,supint1,supinf1,suboden
REAL :: suinf1,supover1,superk1,suboden1,xbil(21)
real      skap
CHARACTER (LEN=1) :: pns,slash


!ccccccccccccccccccccccccccccccccccc
!c
!c	bodenparameter hydrotop
!c
REAL :: m_wep(maxlay,idimnutz2),m_nfk(maxlay,idimnutz2),           &
        m_bmax(maxlay,idimnutz2),m_tief(maxlay,idimnutz2),         &
        m_cin(maxlay,idimnutz2),m_cex(maxlay,idimnutz2),           &
        m_retlay(maxlay,idimnutz2),m_perkout(2*idimnutz,13)

REAL :: m_alpha(maxlay,idimnutz2),m_n(maxlay,idimnutz2),m_l(maxlay,idimnutz2),		&
        m_vmacro(maxlay,idimnutz2),m_bf0(maxlay,idimnutz2),     &      !m_bf0: not use??
        m_r(maxlay,idimnutz2),wt_c(maxlay,idimnutz2),m_kf(maxlay,idimnutz2)

!REAL :: Imac_real(maxlay)
REAL:: Imic_real(maxlay),infm(maxlay)!,Imac_mic(maxlay)


!REAL :: sumImac_real(2*idimnutz,maxlay)
REAL:: sumImic_real(2*idimnutz,maxlay)!,       &
        !sumImac_mic(2*idimnutz,maxlay)
REAL:: suminfmm(2*idimnutz,maxlay)

REAL :: sumImicc_real(2*idimnutz)!,sumImacc_real(2*idimnutz),            &
        !sumImacc_mic(2*idimnutz),
REAL:: suminffmm(2*idimnutz)

REAL,INTENT(OUT)::Imicc_real(idim)!,Imacc_real(idim)     !ok
REAL:: inffmm(idim)



INTEGER:: nvgp,immic,immac,inft
REAL:: sumhz(2*idimnutz2)
REAL:: sumtief

REAL::suImicc_real1,suinfmm1!,suImacc_real1,suImacc_mic1
REAL::suImicc_real(2*idimnutz),suinfmm(2*idimnutz)!,suImacc_real(2*idimnutz),suImacc_mic(2*idimnutz)
!REAL::nw_hz(jdim,2*idimnutz,maxlay,idim)
INTEGER::vg


REAL:: hz(2*idimnutz2,maxlay,idim)
!REAL:: Imacro_micro(2*idimnutz2,maxlay,idim)
REAL:: Imax(2*idimnutz2,maxlay,idim),Imic_pot(2*idimnutz2,maxlay,idim)
!REAL:: Imacc_reall(2*idimnutz2,maxlay,idim),Imac_reall(2*idimnutz2,maxlay,idim)
!REAL:: Imacro_mmicro(2*idimnutz2,maxlay,idim),Imacro_micromax(2*idimnutz2,maxlay,idim)
REAL:: Imicc_reall(2*idimnutz2,maxlay,idim),hz_plus(2*idimnutz2,maxlay,idim)

!REAL::Imac(2*idimnutz,maxlay),Imm(2*idimnutz,maxlay),Imacc(maxlay,0:idim)
REAL:: Imic(2*idimnutz,maxlay),infl(2*idimnutz,maxlay)
REAL::Imicc(maxlay,0:idim),infll(maxlay,0:idim)

REAL:: pre(maxlay),hoo(2*idimnutz2,maxlay,idim),hov(maxlay)
REAL:: sumhzz(2*idimnutz2,maxlay)

REAL:: suminff(2*idimnutz),suminfff(2*idimnutz,maxlay),inff(idim),suinff(2*idimnutz),suinff1

REAL:: sm_real(2*idimnutz2,maxlay,idim)
REAL:: kh(2*idimnutz2,maxlay,idim),kh1(maxndx,maxlay,2*idimnutz2)

REAL:: cind(maxlay),cinh(maxlay)

REAL :: perkout(idim,2*idimnutz)
REAL ::	nutzprz(2*idimnutz)
REAL :: xmax_h(15),xmin_h(15)
INTEGER :: n_hydid(2*idimnutz),nhyd,nhydtyp
CHARACTER (LEN=10) :: kennbod(idimnutz2),kennutz(idimnutz2)
CHARACTER (LEN=8)  :: m_typ(maxlay,idimnutz2)

!c anfangsinhalte = endwerte zyklus iz-1

REAL :: nw_bianf(jdim,2*idimnutz)
REAL :: nw_boanf(jdim,2*idimnutz,maxlay)
REAL :: nw_gws(jdim)

!ccccccccccccccccccccccccccccccccccc
!c
!c	parameter kapilarer aufstieg TODO: entfernen
!c

      real	qkapss(2*idimnutz)
      
!cccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	variablen vegetationsparameter
!c 	boden und nutzungsabhaengige durchwurzelungstiefe

REAL :: kc1(idimnutz2),kc2(idimnutz2),wt1(idimnutz2)
REAL :: wt2(idimnutz2),lai1(idimnutz2),lai2(idimnutz2)


REAL :: time1(idimnutz2),time2(idimnutz2)
REAL :: kc(idimnutz2),lai(idimnutz2)
REAL :: wtsum,wtlay(0:maxlay)
REAL :: wt_b(idimnutz2,idimnutz2),wt_bm(idimnutz2,2)
REAL :: time_b1(idimnutz2),time_b2(idimnutz2)
INTEGER :: ndg(idimnutz2),ilen,ilen1,aktlay
CHARACTER (LEN=80) :: pathn,namveg,text

!c gws-aufteilungsfaktoren
REAL :: m_f1gws(2*idimnutz),m_perkm(2*idimnutz),m_vers(2*idimnutz)

!ccccccccccccccc
!c
!c	anfangswerte lzsim-datein
!c
REAL :: xanf(idimanf),sstd
INTEGER :: nanf(idimanf),nanfstd(idimanf),nanzanf,nanff
INTEGER :: nanzanf1

!cccccccccccccc
!c
!c	ausgabe hydrotope/statistik
!c
REAL :: m_hyd(idimhyd,7),m_bil(jdim,20),m_flaech(jdim,4)

CHARACTER (LEN=1)  :: komma
CHARACTER (LEN=45) :: fmt

sstd = 0.
!******************************************ANWEISUNGEN******************************************
ndebug = -1
IF(DEBUG) THEN
   CALL getLogFile( ndebug )
   WRITE(ndebug,1) iz
ENDIF

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	interne zeitschleife, vorgabe dtmax,
!c	laufindex wird bestimmt
!c

ianf=1
dtmax=8.
IF ( dt > dtmax) THEN
   itimevor=( dt - 0.01 ) / dtmax + 1
ELSE
    itimevor=1
ENDIF

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	vorbereitung  d a t e n a u s g a b e
!c


lop = .FALSE.
IF( l3 > 0 ) THEN
  DO is = 1,ingmax
     IF ( ntg == ntgout(is) ) THEN
        lop = .TRUE.
     ENDIF
  ENDDO
ENDIF

!ccccccccccccccccccccccccc
!c
!c      INITIALISIEREN

nast = 0.
supre=0.
suepot=0.
suepotv=0.
skap=0.

DO nn=1,anzelem
   suevi(nn)=0.
   sueva(nn)=0.
   supob(nn)=0.
   supover(nn)=0.
   supint(nn)=0.
   superk(nn)=0.
   supgws(nn)=0.
   suptgw(nn)=0.
   qkapss(nn)=0.
   suphort(nn)=0.
   suImicc_real(nn)=0.
   !suImacc_real(nn)=0.
   !suImacc_mic(nn)=0.
   suinfmm(nn)=0.
   sumhz(nn)=0.

   suinff(nn)=0.

   DO ilay=1,maxlay
      suintlay(nn,ilay)=0.
      supbod(nn,ilay)=0.
      supstau(nn,ilay)=0.
      supsat(nn,ilay)=0.
      superklay(nn,ilay)=0.
       suevalay(nn,ilay)=0.

      sumImic_real(nn,ilay)=0.
      !sumImac_real(nn,ilay)=0.
      !sumImac_mic(nn,ilay)=0.
      suminfff(nn,ilay)=0.
      sumhzz(nn,ilay)=0.      !check if this goes here!!!
   ENDDO
   DO i1=1,13
      m_perkout(nn,i1)=0.
   ENDDO
ENDDO

DO ilay=1,maxlay
   perkl(ilay)=0.
ENDDO


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der  v e g e t a t i o n s  parameter
!c	 - vorbereitung -    oeffne datenfile und einlesen bis akt. wert
!c

do 6402 nv=1,anznutz
       namveg=''
       ndg(nv)=ju0gfu()
       ilen1=lastchar(kennutz(nv))
       namveg(1:ilen+ilen1+14)=pathn(1:ilen)//'hydro.top'//    &
                      slash//kennutz(nv)(1:ilen1)//'.nuz'
       open (ndg(nv),iostat=ierr,err=996,status='old',file=namveg)

       ijaha=0
       read(ndg(nv),'(a)',end=997) text
       read(ndg(nv),'(a)',end=997) text

       if(text(1:6) == 'ideali') then
	  ijaha=naja
       endif

       read(ndg(nv),'(a)') text

       call inp_veg_b(ndg(nv),ijaha,natg,namo,naja,pns,       &
                       time1(nv),kc1(nv),wt1(nv),lai1(nv))
       if(pns == 's') time1(nv)=0.
       if(time1(nv) > 0.) then
	  write(nerr,435)

435    format('xxx fehler in angabe bedeckungsgrad!',/                  &
   	      'xxx erster wert muss vor beginn d. simulationszeitraums '/,  &
              'xxx oder niederschlagswerten liegen.')

          call writeLogString(6, &
               &'Fehler in den Daten des idealisierten Jahresganges. 1. Wert ist vor Beginn des Simulationszeitraums anzugeben!', &
               & 'Error in ideal Year of natural land use. 1st value should be before simulation start.','','','','','','boden')

	  time2(nv)=time1(nv)
	  kc2(nv)=kc1(nv)
	  time1(nv)=0.
	  goto 433
       endif

432    call inp_veg_b(ndg(nv),ijaha,natg,namo,naja,pns,         &
                       time2(nv),kc2(nv),wt2(nv),lai2(nv))

       if(pns == 's')time2(nv)=100000.

       if(time2(nv) < 0.) then
	  time1(nv)=time2(nv)
	  kc1(nv)=kc2(nv)
	  wt1(nv)=wt2(nv)
	  lai1(nv)=lai2(nv)

	  goto 432
       endif
433    continue

       time_b2(nv)=-1.
       close(ndg(nv))
6402  continue

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme parameter k a p i l a r e r  a u f s t i e g
!c	vorbereitung anfangssummen
!c
!c      �nderung: gel�scht 9.3.06, CK
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der anfangswerte = endwerte zyklus iz-1
!c	interzeptionsspeicher u. bodenfeuchte / hydrotop


DO nn=1,anzelem
   bistart(nn) = nw_bianf(isv,nn)
   bi (nn)= nw_bianf(isv,nn)
   nb=kenelem(1,nn)
   DO ilay=1,anzlay(nb)
      bof(nn,ilay)=nw_boanf(isv,nn,ilay)
      bofstart(nn,ilay)=nw_boanf(isv,nn,ilay)
   ENDDO
ENDDO


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c            s c h l e i f e    z e i t s c h r i t t
!c            =========================================

time:DO t=1,idif
        evi1(t)=0.
	eva1(t)=0.
	pover1(t)=0.
	pint1(t)=0.
  	perk1(t)=0.
	perk2(t)=0.
	tgwzu(t)=0.
 	supre=supre+peff(t)*dt
	suepot=suepot+vs(t)*dt

    !write (nres,*)'t',t
    Imicc_real(t)=0.
    !Imacc_real(t)=0.
    inffmm(t)=0.
    inff(t)=0.

!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	vorbereitung
!!c
!c	uebernahme der zeitabhaengigen parameter/vegetation
!c		kc:  vegetationsabh. korrektur transpiration
!c		wt:  wurzeltiefe
!c		lai: leaf area index
!c                     ---------------------------

  nutz:DO nv=1,anznutz
          xt=real(t)*dt/24.
	  xtb=real(t)*dt/24.
	  if(xt > time2(nv) ) then
	    time1(nv)=time2(nv)
	    kc1(nv)=kc2(nv)
	    wt1(nv)=wt2(nv)
	    lai1(nv)=lai2(nv)
	    namveg=''
	    ndg(nv)=ju0gfu()
	    ilen1=lastchar(kennutz(nv))
	    namveg(1:ilen+ilen1+14)=pathn(1:ilen)//'hydro.top'//  &
                  slash//kennutz(nv)(1:ilen1)//'.nuz'
	    open (ndg(nv),iostat=ierr,err=996,status='old',file=namveg)

	    ijaha=0
	    read(ndg(nv),'(a)',end=997) text
	    read(ndg(nv),'(a)',end=997) text
	    if(text(1:6).eq.'ideali') then
	      ijaha=naja
	    endif
	    read(ndg(nv),'(a)') text

6400	    call inp_veg_b(ndg(nv),ijaha,natg,namo,naja,pns,     &
                          time2(nv),kc2(nv),wt2(nv),lai2(nv))
	    if(time2(nv).lt.xt) goto 6400
            close(ndg(nv))
	  endif

	  if(xtb > time_b2(nv) .AND. wt2(nv) < 0.) then
	    namveg=''
	    ilen1=lastchar(kennutz(nv))
	    namveg(1:ilen+ilen1+14)=pathn(1:ilen)//'hydro.top'//  &
     	           slash//kennutz(nv)(1:ilen1)//'.wtb'
	    ijaha=0
	    call inp_veg_b_wt(namveg,natg,namo,naja,              &
                              xtb,time_b1(nv),time_b2(nv),      &
                                wt_bm,anzbod,kennbod,pns)
	  endif

	  xt = (xt-time1(nv))/(time2(nv)-time1(nv))
	  kc(nv) = xt*(kc2(nv)-kc1(nv)) + kc1(nv)

	  if (wt2(nv) > 0) then
	     DO nb=1,anzbod
	        wt_b(nv,nb) = xt*(wt2(nv)-wt1(nv)) + wt1(nv)
             ENDDO
	  else
	     xtb = (xtb-time_b1(nv))/(time_b2(nv)-time_b1(nv))
	     if(xtb.lt.0. .or. xtb.gt.1.) then
	       write(*,908)xtb,time_b1(nv),time_b2(nv)
   	       write(nerr,908)xtb,time_b1(nv),time_b2(nv)

          908  format('xxx fehler bei zeitpunkten bodenabhaengiger ',          &
               		'durchwurzelungstiefe!',/                               &
               		'xxx zeitpunkte: ',(3(f9.4)))
               call writeLogIntReal(7, 'Fehler in den Daten des idealisierten Jahresganges (Durchwurzelungstiefe)!', &
                    & 'Error in ideal year of natural land use (roots depth)!','','',0,'xtb',xtb,'boden')
               call writeFooter()
   	       stop
	     endif

	     DO nb=1,anzbod
	        wt_b(nv,nb) = xtb*(wt_bm(nb,2)-wt_bm(nb,1)) + wt_bm(nb,1)
	     ENDDO
	  endif
	  lai(nv) = xt*(lai2(nv)-lai1(nv)) + lai1(nv)
       ENDDO nutz



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c            s c h l e i f e    h y d r o t o p e
!c            =====================================



   hyd:DO nn=1,anzelem

   !write (nres,*)'nn',nn
          nb=kenelem(1,nn)
	      nv=kenelem(2,nn)
	      anzlayy=anzlay(nb)
          bianf=bi(nn)
	      bimax=lai(nv)
	      eptt=vs(t)*kc(nv)
	      suepotv=suepotv+eptt*dt*nutzprz(nn)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c             berechnung der interzeption
!c             ---------------------------
!c	       achtung: werte in mm/h
!c
	      call incept(t,dt,bianf,bimax,peff(t),eptt,pri,evi(nn),biend)
	      bi(nn)=biend

	      supob(nn)=supob(nn)+pri*dt
	      eptt=(eptt-evi(nn))



	  ! festsetzung von itime  fuer interne zeitschleife bodenspeicher
	  ! so, dass pri < .1 * (bfmax-bfakt)

	      itime=itimevor
3011	  dtn=dt/itime
	      prinp=pri*dtn
	      if(prinp*10. > m_bmax(1,nb) - bof(nn,1) )then
	        itime=itime*2
	        if(itime < 50) goto 3011
	        dtn=dt/itime
	      endif


	  ! eplay(ilay): pot. evaporation fuer layer i [mm]
	  ! wt [m], m_tief[dm]
	  ! achtung: eptt[mm/h], evpotlay[mm], bezogen auf interne zeitschleife
	  ! mit zeitschrittlaenge dtn

	      call evapot(dtn,anzlayy,nb,nv,nn,eptt,m_tief, &
          	     m_bmax,bof,wt_b,wtlay,bofsum,wtsum,evpotlay)




!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c  kapilarer aufstieg: entfernt 9.03.06,CK
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	      pover(nn)=0.
	      eva(nn)=0.
	      pint(nn)=0.
	      perk(nn)=0.


      !sumImacc_real(nn)=0.
      sumImicc_real(nn)=0.
      !sumImacc_mic(nn)=0.
      suminffmm(nn)=0.
      !sumhz(nn)=0.

      suminff(nn)=0.

	  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	  !c
	  !c  beginn i n t e r n e  z e i t s c h l e i f e  bodenspeicher
	  !c
        !write (nres,*)'nn',nn

 int_time:DO it=1,itime

      !write (nres,*)'it_afurera',it
      sumtief=0.
             ! In diesem Loop wird f�r jede schicht die Infiltration,Perkolation,Evaporation
             ! und aktuelle Bodenfeuchte berechnet
       layer:DO ilay=1,anzlayy

               !write (nres,*)'ilay',ilay
	       if(ilay == 1) then
	         prinp=pri*dtn
	       else
	         prinp=perkl(ilay-1)
	       endif
               !write (nres,*)'pr',prinp
	       xcin=m_cin(ilay,nb)
	       xcex=m_cex(ilay,nb)

	       boderr=bof(nn,ilay)

               !write (nres,*)'peff_afuera',peff(t)
               !WRITE(nres,*)'prinp',prinp
               !stop
	       ! berechnung der bodenfeuchte
	       ! ---------------------------
	       ! achtung: berechnete werte in mm
	       ! kennz:1: NASIM ansatz, kennz=2: Ansatz nach Wendlin

               !write (nres,*)'sumhz_afuera',sumhz(nn)
               !write (nres,*)'sumhzz_afu',sumhzz(nn,ilay)
          ! write (*,*)vg

           if (vg .eq. 1) then
              !call bod_mic_mac(1,dtn,prinp,evpotlay(ilay),0.,m_wep(ilay,nb),m_nfk(ilay,nb),m_bmax(ilay,nb),            &
     	      !	                   bof(nn,ilay),eval(ilay),inf,perkl(ilay),m_bf0(ilay,nb),		&
              !                 m_alpha(ilay,nb),m_kf(ilay,nb),m_n(ilay,nb),m_l(ilay,nb),		&
              !                 m_vmacro,m_r(ilay,nb),t,sumhz,sumtief,m_tief,ilay,			&
              !                 Imic_real(ilay),Imac_mic(ilay),Imac_real(ilay),nb,peff(t),wt_b,nn,nv,anzelem,idif,&
              !                 Imacro_micro,Imacc_reall,Imac_reall,Imacro_mmicro,Imacro_micromax,hz,infm(ilay),Imicc_reall,&
              !                 pre,sumhzz,hoo,hz_plus,hov,Imax,Imic_pot,it,Imac(nn,ilay),Imm(nn,ilay),Imic(nn,ilay),&
              !                 xcin,sm_real,kh,infl(nn,ilay),xcex)

               call bod_mic_mac(1,dtn,t,it,prinp,ilay,evpotlay(ilay),m_wep(ilay,nb),m_nfk(ilay,nb),m_bmax(ilay,nb), &
     		                bof(nn,ilay),eval(ilay),inf,perkl(ilay),					    &
                                m_alpha(ilay,nb),m_kf(ilay,nb),m_n(ilay,nb),m_l(ilay,nb),			    &
                                m_tief,nb,nn,nv,itime,m_retlay(ilay,nb),   				            &
                                Imic_real(ilay),infm(ilay),Imicc_reall,Imic_pot,Imic(nn,ilay),			    &
                                kh1,infl(nn,ilay),xcex)
             else

             call bodf_n(1,dtn,prinp,evpotlay(ilay),0., 	         &
                         m_wep(ilay,nb),m_nfk(ilay,nb),m_bmax(ilay,nb),&
                     	 xcin,xcex,                                    &
                         bof(nn,ilay),inf,eval(ilay),perkl(ilay),      &
                         Imic(nn,ilay),ilay,t,infl(nn,ilay))
           end if

           !write (nres,*)'bof',bof(1,1)
           boderr=boderr-bof(nn,ilay)+(inf-eval(ilay)-perkl(ilay) )

	       if(abs(boderr).gt.0.1) then
	         write (nerr,10101) t,ilay,boderr

                 call writeLogIntIntReal(6,'Fehler bei der Berechnung der Bodenfeuchte!', &
                      & 'Error while calculation of the soil moisture.','','',0,'layer',ilay,'boderr',boderr,'boden')

		 10101 format(' fehlte in bodenroutine!'/,               &
                    	      ' zeitschritt: ',i4,/                      &
                    	      ' layer:       ',i4,/                      &
                    	      ' fehler:      ',i4)
	       endif

	       pstau(ilay)=0.
           if (vg .eq. 1) then
             if( (prinp-infm(ilay)) > 0.) then
	            pstau(ilay)=prinp-infm(ilay)
             end if
           else
	         if( (prinp-inf) > 0.) then
	            pstau(ilay)=prinp-inf
             endif
           endif
	       if( pstau(ilay).lt.0.) THEN
                 pstau(ilay)=0.
               ENDIF
	       eva(nn)=eva(nn)+eval(ilay)
	       suevalay(nn,ilay)=suevalay(nn,ilay)+eval(ilay)
	       if(ilay == anzlayy) then
	         if(m_perkm(nn).gt.0.) then
		   xmax=m_perkm(nn)*dtn*3.6*1.e6
	         else
		   xmax=100000.
	         endif
	         if(perkl(ilay) > xmax) then
                 ! Der Anteil der nicht in den GW-Leiter fliessen kann (weil maxperk > perklayer )
                 ! wird in einen Fiktiven Layer geschrieben -> immer in layer = anzlay +1
		 pstau(ilay+1)=perkl(ilay)-xmax
		 perkl(ilay)=xmax
		 if(pstau(ilay+1) < 0.) THEN
                   pstau(ilay+1)=0.
                 ENDIF
	         else
		 pstau(ilay+1)=0.
	         endif
	         perk(nn)=perk(nn)+perkl(ilay)

	       endif !ende if letzte Schicht

               ! Summiert die Perkolation f�r jeden Layer je Hydrotop �ber den internen Zeitschritt dtn auf
	       superklay(nn,ilay)=superklay(nn,ilay)+perkl(ilay)


           if (vg .eq. 1) then
!             Sum the Infiltration per porous medium in each layer in each hydrotop

               sumImic_real(nn,ilay)=sumImic_real(nn,ilay)+Imic_real(ilay)
               !sumImac_real(nn,ilay)=sumImac_real(nn,ilay)+Imac_real(ilay)
               !sumImac_mic(nn,ilay)=sumImac_mic(nn,ilay)+Imac_mic(ilay)
               suminfmm(nn,ilay)=suminfmm(nn,ilay)+infm(ilay)

!            Sum infiltration per hydrotop

               sumImicc_real(nn)=sumImicc_real(nn)+Imic_real(ilay)
               !sumImacc_real(nn)=sumImacc_real(nn)+Imac_real(ilay)
               !sumImacc_mic(nn)=sumImacc_mic(nn)+Imac_mic(ilay)
               suminffmm(nn)=suminffmm(nn)+infm(ilay)
           else
               sumImic_real(nn,ilay)=0.
               !sumImac_real(nn,ilay)=0.
               !sumImac_mic(nn,ilay)=0.
               suminfmm(nn,ilay)=0.

               sumImicc_real(nn)=0.
               !sumImacc_real(nn)=0.
               !sumImacc_mic(nn)=0.
               suminffmm(nn)=0.
           END if

               suminff(nn)=suminff(nn)+inf
               suminfff(nn,ilay)=suminfff(nn,ilay)+inf

	     ENDDO layer


	     suphort(nn)=suphort(nn)+pstau(1)

	 inv:DO ilay=anzlayy+1,2,-1
	        if( pstau(ilay) > 0.) then
	          pintlay(ilay)=pstau(ilay)* m_retlay(ilay-1,nb)
	          prest=pstau(ilay)-pintlay(ilay)
	          supstau(nn,ilay)=supstau(nn,ilay)+pstau(ilay)
	          supbod(nn,ilay)=supbod(nn,ilay)+prest
	          bof(nn,ilay-1)=bof(nn,ilay-1)+prest
	        else
	          pintlay(ilay)=0.
	        endif
	        if(bof(nn,ilay-1) > m_bmax(ilay-1,nb) )then
	          prest=bof(nn,ilay-1)-m_bmax(ilay-1,nb)
	          bof(nn,ilay-1)=m_bmax(ilay-1,nb)
	          supsat(nn,ilay-1)=supsat(nn,ilay-1)+prest
	          if(ilay.gt.2)then
		    bof(nn,ilay-2)=bof(nn,ilay-2)+prest
	          else
		    pstau(ilay-1)=pstau(ilay-1)+prest
	          endif
	        endif
	     ENDDO inv

	     DO ilay=2,anzlayy+1
	        pint(nn)=pint(nn)+pintlay(ilay)
	        suintlay(nn,ilay)=suintlay(nn,ilay)+pintlay(ilay)
 	     ENDDO
	     pover(nn)=pover(nn)+pstau(1)

          ENDDO int_time



	 !c ende interne zeitschleife fuer bodenspeicher
	 !c
	 !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       ENDDO hyd
!c
!c	e n d e   s c h l e i f e   h y d r o t o p e
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebergabe der ergebnisse / hydrotop als summe / TG

sum_tg:DO nn=1,anzelem
	  ! ausgabe perkolation/hydrotop, Summation f�r den laufenden Zeitschritt
	  ! rtr = Korrekturfaktor f. Aufteilung grundwasser/tiefengrundwasser
          ! aus gebietsdatei *.geb eingelesen!c

	  IF( nhydtyp == 3) THEN
            perkout(t,nn)=perk(nn)
          ENDIF
	  IF ( nhydtyp == 4 .AND. iz > 1 .AND. nhyd > 0) THEN
	     xv=1.-m_vers(nn)
	     IF ( xv < 0. ) THEN
                xv=0.
             ENDIF

	     IF ( xv > 1. ) THEN
                xv=1.
             ENDIF

	     m_perkout(nn,13)=m_perkout(nn,13)+perk(nn)
	     i2=int(t*12./365.25)+1
	     IF ( i2 > 12 ) THEN
                i2=12
             ENDIF
	     m_perkout(nn,i2)=m_perkout(nn,i2)+perk(nn)
	  ENDIF

	  evi1(t)=evi1(t)+nutzprz(nn)*evi(nn)
	  eva1(t)=eva1(t)+nutzprz(nn)*eva(nn)
	  pover1(t)=pover1(t)+nutzprz(nn)*pover(nn)
	  pint1(t)=pint1(t)+nutzprz(nn)*pint(nn)
!Sum at catchment level

      if (vg .eq. 1) then
          Imicc_real(t)=Imicc_real(t)+nutzprz(nn)*sumImicc_real(nn)
          !Imacc_real(t)=Imacc_real(t)+nutzprz(nn)*sumImacc_real(nn)

          inffmm(t)=inffmm(t)+nutzprz(nn)*suminffmm(nn)

          suImicc_real(nn)=suImicc_real(nn)+sumImicc_real(nn)
          !suImacc_real(nn)=suImacc_real(nn)+sumImacc_real(nn)
          !suImacc_mic(nn)=suImacc_mic(nn)+sumImacc_mic(nn)
          suinfmm(nn)=suinfmm(nn)+suminffmm(nn)
       else
          Imicc_real(t)=0.
          !Imacc_real(t)=0.
          inffmm(t)=0.

          suImicc_real(nn)=0.
          !suImacc_real(nn)=0.
          !suImacc_mic(nn)=0.
          suinfmm(nn)=0.
       END if

       inff(t)=inff(t)+nutzprz(nn)*suminff(nn)
       suinff(nn)=suinff(nn)+suminff(nn)

	  perk1(t)=perk1(t)+nutzprz(nn)*perk(nn)
	  perk2(t)=perk2(t)+nutzprz(nn)*perk(nn)*m_f1gws(nn)*(rtr)
	  tgwzu(t)=tgwzu(t)+nutzprz(nn)*perk(nn)*(1.-m_f1gws(nn)+m_f1gws(nn)*(1.-rtr))
	  suevi(nn)=suevi(nn)+evi(nn)*dt
	  sueva(nn)=sueva(nn)+eva(nn)
	  supover(nn)=supover(nn)+pover(nn)
	  supint(nn)=supint(nn)+pint(nn)
	  superk(nn)=superk(nn)+perk(nn)

       ENDDO sum_tg


       DO ilay=1,anzlayy
    	  b(ilay,t)=bof(1,ilay)
          Imicc(ilay,t)=Imic(1,ilay)
          !Imacc(ilay,t)=Imac(1,ilay)
          infll(ilay,t)=infl(1,ilay)
       ENDDO

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	g r u n d w a s s e r s p e i c h e r:
!c      �NDERUNG
!c	in eigene Routine geschoben 9.3.06, CK
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!c SK/EP 28.02.03
!c Datenausgabe Anfangswerte LZSIM
!c Das Anfangsdatum des Zykluses 8 stellig wird in Jahr, Monat und Tag gesplittet, um die
!c Funktion cdatum anzuwenden.
!JH 28.09.2004 Anfangswerte werden auch aus einer Kurzzeitsimulation gespeichert.
!              Hierzu musste das Format des *.lzs-Files ge�ndert werden, um die Stunden hinzuzuf�gen.
!              Abfrage (If-Schliefe) um lz-kz erweitert.
       IF ( isim == 0 ) THEN                                                      ! Langzeitsim.
          IF( nanf(1) == idatsa20 .AND. nanzanf == 0) THEN
            ijahr=(nanf(1)/10**4)
            imona=((nanf(1)-ijahr*10**4)/100)
            itag =(nanf(1)-ijahr*10**4-imona*100)

!c SK/EP 28.02.03
!c Die do Variable t: do t=1, idif (Anzahl der Zeitschritte im Zyklus) mu� neu initialisiert werden,
!c da keine do-Variable �bergeben werden darf. Die Funktion cdatum setzt idatum f�r jeden Zeitschritt
!c neu, damit so in der lzsim alle Anfangswerte herausgeschrieben werden k�nnen.
            ttemp=t
            call cdatum (itag,imona,ijahr,ttemp)                         ! 1 Tag = 1 Simulationsschritt t weiter
            idatum=ijahr*10**4+imona*10**2+itag
	    write(nanff,'(i8,2x,a2,a2,2x,i4,1x,a4)') idatum,'00',' h',anzelem,'bodf'

	    DO nn=1,anzelem
	       nb=kenelem(1,nn)
	       anzlayy=anzlay(nb)
	       write(nanff,'(i4,f7.2,10(f7.2))')nn,bi(nn),(bof(nn,ilay),ilay=1,anzlayy)
            ENDDO

          ELSEIF (nanzanf > 0 .AND. t >= xanf(ianf) ) then

	    write(nanff,'(i8,2x,a2,a2,2x,i4,1x,a4)') nanf(ianf),'00',' h',anzelem,'bodf'
	    DO nn=1,anzelem
	       nb=kenelem(1,nn)
	       anzlayy=anzlay(nb)
	       write(nanff,'(i4,f7.2,10(f7.2))')nn,bi(nn),(bof(nn,ilay),ilay=1,anzlayy)
            ENDDO
	    ianf=ianf+1
	    if(ianf.gt.nanzanf) nanzanf=0

          ENDIF
       ELSEIF(pns /= 's') THEN                                          ! Kurzzeitsimulation nat�rlicher Niederschlag
          IF (ianf <= nanzanf1) THEN
             IF (t == 1) THEN
                sjahr = naja20
                smona = namo
                stag = natg
                sstd = REAL(nast)
             END IF
             sstd = sstd + dt
             if (sstd >= 24.0) then
                sstd = sstd - 24.0
                call cdatum (stag,smona,sjahr,1)
             end if
             sdatum = sjahr*10**4 + smona*10**2 + stag
             isstd=INT(sstd)
             if (nanf(ianf) == sdatum .AND. nanfstd(ianf) == isstd) then
                write(nanff,'(i8,2x,i2,a2,2x,i4,1x,a4)') nanf(ianf),nanfstd(ianf),' h',anzelem,'bodf'
      	        DO nn=1,anzelem
      	           nb=kenelem(1,nn)
   	           anzlayy=anzlay(nb)
   	           write(nanff,'(i4,f7.2,10(f7.2))')nn,bi(nn),(bof(nn,ilay),ilay=1,anzlayy)
		ENDDO
                ianf = ianf + 1
                IF (ianf.gt.nanzanf1) THEN
                END IF
             end if
          END IF
       END IF

ENDDO time
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c		ende zeitschleife
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c  	fehler bodenspeicher:
!c	�NDERUNG
!c	in eigene Routine geschoben 9.3.06, CK
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	weitergabe abfluss grundwasser an unterhalb liegende TG
!c		(in qbm/s)
!c	�NDERUNG
!c	in eigene Routine geschoben 9.3.06, CK
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc!c

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	speichern der endwerte = anfangswerte zyklus iz+1
!c
DO nn=1,anzelem
   nb=kenelem(1,nn)
   nw_bianf(isv,nn) = bi (nn)
   DO ilay=1,anzlay(nb)
      nw_boanf(isv,nn,ilay) = bof(nn,ilay)
   ENDDO
ENDDO


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	bilanzierung und kontrolle der ergebnisse
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!ccccccccccc
!c  bilanzierung gesamtgebiet bodenspeicher

suevi1=0.
sueva1=0.
supint1=0.
supinf1=0.
supob1=0.
suinf1=0.
supover1=0.
superk1=0.
suboden1=0.

suImicc_real1=0.
!suImacc_real1=0.
!suImacc_mic1=0.
suinfmm1=0.
suinff1=0.

call u1null(bdif,maxlay)

elem:DO nn=1,anzelem
        nb=kenelem(1,nn)
        suboden=0.
        DO ilay=1,anzlay(nb)
	   bdif(ilay)=bofstart(nn,ilay)-bof(nn,ilay)
	   suboden=suboden+bdif(ilay)
	ENDDO
        suinf=bistart(nn)-bi(nn)
        suevi1=suevi1+suevi(nn)*nutzprz(nn)
        sueva1=sueva1+sueva(nn)*nutzprz(nn)
        supint1=supint1+supint(nn)*nutzprz(nn)
        supinf1=supinf1+(supob(nn)-supover(nn))*nutzprz(nn)
        supob1=supob1+supob(nn)*nutzprz(nn)
        suinf1=suinf1+suinf*nutzprz(nn)
        supover1=supover1+supover(nn)*nutzprz(nn)
        superk1=superk1+superk(nn)*nutzprz(nn)
        suboden1=suboden1+suboden*nutzprz(nn)

        suImicc_real1=suImicc_real1+nutzprz(nn)*suImicc_real(nn)
        !suImacc_real1=suImacc_real1+nutzprz(nn)*suImacc_real(nn)
        !suImacc_mic1=suImacc_mic1+nutzprz(nn)*suImacc_mic(nn)
        suinfmm1=suinfmm1+nutzprz(nn)*suinfmm(nn)

        suinff1=suinff1+nutzprz(nn)*suinff(nn)

	! bildschirm-ausgabe
        sudif1 = supre-suevi(nn)-supob(nn)+suinf
        sudif=supob(nn)+qkapss(nn)-sueva(nn)-supover(nn)-supint(nn)-superk(nn)+suboden


	! ausfuehrliche ausgabe    h y d r o t o p
	! (achtung bei grosser hydrotopanzahl)

        if (l3 > 0 .AND. lop) then
	   if (nn == 1) write(l3,9150)ntg
	      write(l3,9250)nn,kennbod(nb)(1:6),kennutz(nv)(1:6)
	      write(l3,9151)supre,suevi(nn),supob(nn),suinf,sudif1,suepot,suevi(nn)+sueva(nn)
	      write(l3,9152)supob(nn),supover(nn),supob(nn)-supover(nn),      &
 			    sueva(nn),supint(nn),superk(nn),suboden,sudif
	   if(.TRUE.) then
	     write(nres,'(/a/)')' kein kapilarer aufstieg vorgesehen!'
	   endif
	   write(l3,9159) 1,supob(nn),                                    &
                suphort(nn),supsat(nn,1),                               &
                supob(nn)-supover(nn),                                  &
                suevalay(nn,1),                           &
                superklay(nn,1),supsat(nn,2),                           &
                supbod(nn,2),bdif(1)
	   write(l3,9158)(ilay,superklay(nn,ilay-1),                      &
                suintlay(nn,ilay),supbod(nn,ilay),                      &
                superklay(nn,ilay-1)-suintlay(nn,ilay)-supbod(nn,ilay), &
                suevalay(nn,ilay),                     &
                superklay(nn,ilay),supsat(nn,ilay+1),                   &
                supbod(nn,ilay+1),bdif(ilay),                           &
                ilay = 2,anzlayy+1)

 	   write(l3,9153)
	   write(l3,9154)(ilay-1,ilay,suintlay(nn,ilay),ilay = 2,anzlayy+1)
	   write(l3,9155)supover(nn),suphort(nn),supsat(nn,1)
	   write(l3,9156)(ilay-1,ilay,supstau(nn,ilay),suintlay(nn,ilay),  &
                       supbod(nn,ilay),ilay = 2,anzlayy+1)
	   if(.TRUE.) then
	     write(l3,9420)
	     write(l3,9421) qkapss(nn)
	   else
	     write(l3,'(/a/)')' kein kapilarer aufstieg vorgesehen!'
	   endif

        endif
ENDDO elem

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c     a u s g a b e
!c
!c         bodenfeuchte         b o f . d a t (nur erstes Hydrotop)
!c         transpiration        v t r . d a t
!c         evaporation          v e p . d a t
!c         evapotranspiration   v e t . d a t
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

IF ( nbof > 0 ) then
   nb=kenelem(1,1)
   DO ilay=1,anzlay(1)
      write(nbof,'(/3(i8))')ntg,iz,idif
      write(nbof,'(a,i2,a,f6.1,a,f6.1,a)') 'bodenlayer: ',ilay,  &
                 ' nutz.porenvol.:',m_bmax(ilay,nb),'mm    nutz.feldk.:',m_nfk(ilay,nb),' mm'
      write(nbof,'(10(f8.3,1x))') ( b(ilay,i),i=1,idif )
      
   ENDDO
ENDIF

IF (nvep > 0 ) then
   write(nvep,'(/3(i8)/)') ntg,iz,idif
   write(nvep,'(10(f8.3,1x))') (eva1(i)*dt,i=1,idif)
ENDIF

IF ( nvet > 0 ) then
   write(nvet,'(/3(i8)/)') ntg,iz,idif
   write(nvet,'(10(f8.3,1x))') ((evi1(i)+eva1(i))*dt,i=1,idif)
endif

IF (nkap > 0 ) then
   write(nkap,'(/3(i8)/)') ntg,iz,idif
   write(nkap,'(a)')' perkolation im TG {mm}'
   write(nkap,'(10(f8.3,1x))') (perk2(i),i=1,idif)
ENDIF

!c	ausgabe perkolation/hydrotop
IF ( nhydtyp == 3 .AND. nhyd > 0 ) then
   DO nn=1,anzelem
      write(nhyd,'(/3(i8)/)') n_hydid(nn),iz,idif
      write(nhyd,'(10(f8.2,1x))') (perkout(t,nn),t=1,idif)
   ENDDO
ENDIF

IF ( nhydtyp == 4 .AND. nhyd > 0 ) then
   DO nn=1,anzelem
      write(nhyd,10010)n_hydid(nn),m_perkout(nn,13),(m_perkout(nn,i1),i1=1,12)
      10010  format(i8,',',13((f8.2),','))
   ENDDO
ENDIF

IF (immic .gt. 0) then
   nb=kenelem(1,1)
   DO ilay=1,anzlay(1)
      write(immic,'(/3(i8))')ntg,iz,idif
      write(immic,'(a,i2,a,f6.1,a,f6.1,a)') 'bodenlayer: ',ilay,  &
                 ' nutz.porenvol.:',m_bmax(ilay,nb),'mm    nutz.feldk.:',m_nfk(ilay,nb),' mm'
      write(immic,'(10(f8.3,1x))') ( Imicc(ilay,i),i=1,idif )
   ENDDO
ENDIF

!IF (immac .gt. 0) then
!   nb=kenelem(1,1)
!   DO ilay=1,anzlay(1)
!      write(immac,'(/3(i8))')ntg,iz,idif
!      write(immac,'(a,i2,a,f6.1,a,f6.1,a)') 'bodenlayer: ',ilay,  &
!                 ' nutz.porenvol.:',m_bmax(ilay,nb),'mm    nutz.feldk.:',m_nfk(ilay,nb),' mm'
!      write(immac,'(10(f8.3,1x))') ( Imacc(ilay,i),i=1,idif )
!   ENDDO
!ENDIF

IF (inft .gt. 0) then
   nb=kenelem(1,1)
   DO ilay=1,anzlay(1)
      write(inft,'(/3(i8))')ntg,iz,idif
      write(inft,'(a,i2,a,f6.1,a,f6.1,a)') 'bodenlayer: ',ilay,  &
                 ' nutz.porenvol.:',m_bmax(ilay,nb),'mm    nutz.feldk.:',m_nfk(ilay,nb),' mm'
      write(inft,'(10(f8.3,1x))') ( infll(ilay,i),i=1,idif )
   ENDDO
ENDIF


IF (nvgp .gt. 0) then
   write (nvgp,*)''
   write(nvgp,200) ntg,iz,idif
   write(nvgp,201)superk1,sueva1,suImicc_real1,0.,0.,& !0. correspond to suImacc_real1,suImacc_mic1,&
                  suinfmm1,suImicc_real1+0.,supob1-supover1,suinff1     !0. correspond to suImacc_mic1
end if

200 format (i8,2x,i8,2x,i8)

201 format (F11.4,2x,F11.4,3x,F11.4,8x,F11.4,8x,F11.4,8x,F11.4,8x,F11.4,8x,F11.4,8x,F11.4)

!c	bildschirm-ausgabe zusammenfassung

write(nres,9251)ntg,anzelem
sudif1 = supre-suevi1-supob1+suinf1
write(nres,9149)supre,suevi1,supob1,suinf1,sudif1,suepot,suepotv,  &
     		suevi1+sueva1
sudif=supob1-sueva1-supover1-supint1-superk1+suboden1
write(nres,9152)supob1,supover1,supob1-supover1,           &
     		sueva1,supint1,superk1,suboden1,sudif

IF ( nres2 > 0) then
   fmt(1:45)='(i5,a1,0x,4(f6.1,a1),f7.3,a1,5(f6.1,a1),f6.1)'
   komma=','
   IF ( ntg >= 1000 .AND. ntg < 10000 ) then
       fmt(3:3)='4'
       fmt(8:8)='1'
   ELSEIF ( ntg >= 100 .AND. ntg < 1000 )then
       fmt(3:3)='3'
       fmt(8:8)='2'
   ELSEIF ( ntg.ge.10.and.ntg.lt.100 ) then
	fmt(3:3)='2'
	fmt(8:8)='3'
   ELSEIF ( ntg >=1 .AND. ntg < 10) then
	fmt(3:3)='1'
	fmt(8:8)='4'
   ENDIF

   write (nres2,fmt) ntg,komma,supre,komma,-99.,   &
		     komma,(sueva1+suevi1),   	   &
		     komma,supover1+supint1,komma, &
                     (supover1+supint1)/supre,     &
		     komma,suevi1,komma,sueva1,    &
		     komma,supover1,komma,supint1, &
		     komma,-99.,komma,superk1
ENDIF

IF ( iz > ivor) then
    m_bil(isv,1)=m_bil(isv,1)+supre
    m_bil(isv,2)=m_bil(isv,2)+suepot
    m_bil(isv,19)=m_bil(isv,19)+suepotv
    m_bil(isv,3)=m_bil(isv,3)+suevi1
    m_bil(isv,4)=m_bil(isv,4)+sueva1
    m_bil(isv,5)=m_bil(isv,5)+supob1
    m_bil(isv,6)=m_bil(isv,6)+supover1
    ! Hier wird die bilanz so gelassen wie sie ist obwohl der Kap. Aufstieg
    ! nicht mehr enthalten ist. Deshalb wird nur null addiert
    m_bil(isv,7)=m_bil(isv,7)+ 0.
    m_bil(isv,8)=m_bil(isv,8)+supint1
    m_bil(isv,9)=m_bil(isv,9)+superk1
ENDIF

!c	ausgabe hydrotop-file

IF ( nhydtyp == 1 .AND. nhyd > 0) then
   call minmax(suevi,anzelem,xmin_h(1),xmax_h(1))
   call minmax(sueva,anzelem,xmin_h(2),xmax_h(2))
   call minmax(supover,anzelem,xmin_h(4),xmax_h(4))
   call minmax(supint,anzelem,xmin_h(5),xmax_h(5))
   call minmax(superk,anzelem,xmin_h(6),xmax_h(6))
   call minmax(qkapss,anzelem,xmin_h(7),xmax_h(7))
   call minmax(supgws,anzelem,xmin_h(9),xmax_h(9))
   call minmax(suptgw,anzelem,xmin_h(10),xmax_h(10))
   write(nhyd,9603) ntg
   write(nhyd,9600) (xmin_h(i),i=1,7),                         &
                    (xmin_h(i)/fakls,i=8,10),                  &
                    (xmax_h(i),i=1,7),(xmax_h(i)/fakls,i=8,10)

   write(nhyd,'(2(i8))')ntg,anzelem
ENDIF

IF ( nhydtyp > 0 .AND. nhydtyp <= 2 .AND. iz > ivor) then
   DO nn=1,anzelem
      idnr=n_hydid(nn)
      IF ( idnr > idimhyd ) then
	 write(nerr,9606) idnr

         call writeLogIntIntInt(7,'Anzahl der Hydrotope eines Teilgebietes uebersteigt max. moegliche Anzahl!',&
              & 'Amount of hydrotops exceed max. number in a sub catchment.','','Teilgebiet',ntg,'idnr',idnr,&
              & 'idimhyd',idimhyd,'boden')
         call writeFooter()
	 stop
      endif
      IF ( iz > ivor ) then
	 m_hyd(idnr,1)=m_hyd(idnr,1)+suevi(nn)
	 m_hyd(idnr,2)=m_hyd(idnr,2)+sueva(nn)
	 m_hyd(idnr,3)=m_hyd(idnr,3)+supover(nn)
	 m_hyd(idnr,4)=m_hyd(idnr,4)+supint(nn)
	 m_hyd(idnr,5)=m_hyd(idnr,5)+superk(nn)
	 m_hyd(idnr,6)=m_hyd(idnr,6)+supgws(nn)
	 m_hyd(idnr,7)=m_hyd(idnr,7)+suptgw(nn)
      ENDIF
      IF ( nhyd > 0 .AND. nhydtyp == 1 ) then
	 nb=kenelem(1,nn)
	 nv=kenelem(2,nn)
	 write (nhyd,9601) n_hydid(nn),supre,         &
                           suevi(nn),sueva(nn),suevi(nn)+sueva(nn),  &
                           supover(nn),supint(nn),superk(nn),        &
                           qkapss(nn),                               &
                           (supint(nn)+superk(nn))/fakls,            &
                           supgws(nn)/fakls,suptgw(nn)/fakls,        &
                           kennutz(nv)(1:6),kennbod(nb)(1:6)
      ENDIF
   ENDDO

ENDIF




!cccccccccccccccccccc
!c
!c	datenuebergabe gesamtbilanz

xbil(1)=xbil(1)+supre*fnsum
xbil(18)=xbil(18)+suepot*fnsum
xbil(19)=xbil(19)+suepotv*fnsum
xbil(2)=xbil(2)+suevi1*fnsum
xbil(3)=xbil(3)+sueva1*fnsum
xbil(4)=xbil(4)+supover1*fnsum
xbil(5)=xbil(5)+supint1*fnsum
xbil(6)=xbil(6)+superk1*fnsum
xbil(7)=xbil(7)+suboden1*fnsum

IF (DEBUG) THEN
   WRITE (ndebug,2)
END IF




RETURN

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
9606  format('xxx anzahl der hydrotope uebersteigt vorgesehene ',      &
               'felddimension!',/'xxx erhoehe parameter idimhyd . ',/    &
               'xxx hydrotopid: ',i8)
9603  format('-------------------------------------------',/         &
               ' ausgabe ergebnisse hydrotop ',/                       &
               ' teilgebiet: ',(i4),/,                                &
               ' ausgegebene werte: ',/                                &
               '   hydid    nied  interz     eva',                      &
               '    verd   overl interfl    perk   kap-A',              &
               ' int+perk   qgws    qtgw',/                            &
               '            [mm]    [mm]    [mm]',                     &
               '    [mm]    [mm]    [mm]    [mm]    [mm]',             &
               '  [l/s*qkm]    [l/s*qkm]')
9600  format('   range check: '/                                    &
               ' minimun-werte: ',7(f8.1),3(f8.2),/                   &
               ' maximum-werte: ',7(f8.1),3(f8.2))
9601  format(i8,8(f8.1),3(f8.2),(2(a6,2x)))


9149  format(/' speicherbilanz interzeption: '/,                  &
               ' niederschlag:(nach schneesp.)', f10.1,' mm'/,        &
               ' interz.verd.:                ', f10.1,' mm'/,        &
               ' bestandsniederschlag:        ', f10.1,' mm'/,        &
               ' differenz interz.speicher:   ', f10.1,' mm'/,        &
               ' fehler:                      ', f10.1,' mm'/,       &
               ' pot. verdunst. (grasref.):   ', f10.1,' mm'/,        &
               ' pot. verdunst.(akt. vegetat.):', f10.1,' mm'/,       &
               ' aktuelle verdunstung:        ', f10.1,' mm')
9151  format(/' speicherbilanz interzeption: '/,                 &
               ' niederschlag:(nach schneesp.)', f10.1,' mm'/,       &
               ' interz.verd.:                ', f10.1,' mm'/,       &
               ' bestandsniederschlag:        ', f10.1,' mm'/,       &
               ' differenz interz.speicher:   ', f10.1,' mm'/,       &
               ' fehler:                      ', f10.1,' mm'/,      &
               ' pot. verdunst.(akt. vegetat.):', f10.1,' mm'/,      &
               ' aktuelle verdunstung:        ', f10.1,' mm')
9152  format(/' speicherbilanz bodenspeicher:   ',/               &
               ' bestandsniederschlag:        ', f10.1,' mm'/,       &
               ' overlandflow:                ', f10.1,' mm'/,       &
               ' [infiltration:               ', f10.1,' mm]'/,      &
               ' verdunstung (bodensp.):      ', f10.1,' mm'/,       &
               ' lateral. abfluss (interflow):', f10.1,' mm'/,       &
               ' perkolation:                 ', f10.1,' mm'/,       &
               ' differenz bodenspeicher:     ', f10.1,' mm'/,       &
               ' fehler:                      ', f10.1,' mm')
9153  format(/'lateraler abfluss: ')
9154  format( ' oberhalb grenzschlicht l.',i2,' -',i2,             &
               ': ', f10.1,' mm')
9155  format(/' oberflaechenabfluss ges.:    ',f10.1,' mm'/        &
               ' nach horton (inf < pre) :    ', f10.1,' mm'/,       &
               ' aus saettigungsflaechen:     ', f10.1,' mm')
9156  format(/' stau an grenzschlicht layer ',i2,' -',i2,/         &
               ' gesamt:                     ', f10.1,' mm'/,        &
               ' lateraler abfluss:          ', f10.1,' mm'/,        &
               ' korrektur der bodenfeuchte: ', f10.1,' mm')
9157  format(' abfl.aus gesaettigtem boden, layer',i2,': ',        &
               f10.1,' mm')
9158  format(/' bilanz fuer bodenlayer: ',i2/,                     &
               '   zufluss von oben:            ',f10.1,' mm'/,      &
               '   lateraler abfluss:           ',f10.1,' mm'/,      &
               '   abgabe an layer oben:        ',f10.1,' mm'/,      &
               '   infiltration:                ',f10.1,' mm'/,      &
               '   transpiration:               ',f10.1,' mm'/,      &
               '   perkolation:                 ',f10.1,' mm'/,      &
               '   zug.v.unten aus nied.leitf.: ',f10.1,' mm'/,      &
               '   zug.v.unten bei saettigung:  ',f10.1,' mm'/,      &
               ' differenz bodenfeuchte:        ',f10.1,' mm')
9159  format(/' bilanz fuer bodenlayer: ',i2/,                     &
               '   niederschlag:                ',f10.1,' mm'/,      &
               '   lat. abfluss nach horton:    ',f10.1,' mm'/,      &
               '   lat. abfluss bei saettigung: ',f10.1,' mm'/,      &
               '   infiltration     :           ',f10.1,' mm'/,      &
               '   transpiration:               ',f10.1,' mm'/,      &
               '   perkolation:                 ',f10.1,' mm'/,      &
               '   zug.v.unten aus nied.leitf.: ',f10.1,' mm'/,      &
               '   zug.v.unten bei saettigung:  ',f10.1,' mm'/,      &
               ' differenz bodenfeuchte:        ',f10.1,' mm')
9150  format(/'-----------------------------------------',/      &
               ' bilanz bodenspeicher teilgebiet ',i5)
9251  format(/'-----------------------------------------',/      &
               ' z u s a m m e n f a s s u n g  ',/                  &
               ' der bilanz fuer teilgebiet:   ',i5,/                &
               ' anzahl hydrotope:   ',i5)
9250  format(/'-----------------------------------------',/      &
               ' bilanz hydrotop: ',i3,/                            &
               '           boden: ',a,/                              &
               '         nutzung: ',a)
9420  format(/' summenbilanz kapilarer aufstieg: ')
9421  format(/' kap.aufstieg aus gws(gesamt):', f10.1,' mm')
9422  format (' kap.aufstieg in bodens.',i2,':   ',f10.1,' mm',/   &
     		' zus. kap.aufstieg aus bodens.:   ',f10.1,' mm')

9400  format(/' aufteilung perkolation: ',//                       &
               ' perkolation bodenspeicher:     ',f10.1,' mm'//,     &
               ' abgabe grundw.speicher:        ',f10.1,' mm'/,      &
               '        tiefengrundw.leiter:    ',f10.1,' mm'//,     &
               '        verlust tiefengw.leiter:',f10.1,' mm'//,     &
               ' fehler:                        ',f10.1,' mm',/)

9401  format(/' bilanz grundwasserspeicher: ',//                   &
               ' zufluss perkolation:           ',f10.1,' mm'/,      &
               '        grundw. oberh.l.TG:     ',f10.1,' mm'/,      &
               ' abgabe basisabfluss:           ',f10.1,' mm'/,      &
               '        grundw. unterh.l.TG:    ',f10.1,' mm'/,      &
               '        kapilarer aufstieg:     ',f10.1,' mm'/,      &
               '        entnahme GWS:          ',f10.1,' mm'/,       &
               '     pot.entnahme GWS:          ',f10.1,' mm'/,      &
               ' speicherinhaltsaenderung:      ',f10.1,' mm'/,      &
               ' fehler:                        ',f10.1,' mm'/)

9402  format(/' flaechenanteile: ',//                              &
               '   grundwasserleiter:           ',f10.3,' qkm'/,     &
               '   tiefengrundwasserleiter:     ',f10.3,' qkm'/,     &
               '   gesamtflaeche:               ',f10.3,' qkm'//)


996   write(nerr,906)namveg

906   format('xxx datenfile mit vegetationsdaten nicht gefunden!',/ &
               'xxx filename: ',a)

      call writeLogString( 7, 'Datenfile mit Vegetationsdaten nicht vorhanden!','File with vegetation data not found!', &
           & namveg,'','','','','boden')
      call writeFooter()
      stop


997   write(nerr,907)namveg

907   format('xxx fehler beim einlesen der vegetationsdaten!',/       &
     		'xxx filename: ',a)

      call writeLogString( 7, 'Fehler beim Einlesen der Vegetationsdaten!', 'Error reading the vegetation data!', &
           & namveg,'','','','','boden')
      call writeFooter()
      stop

end
