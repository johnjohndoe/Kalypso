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


      subroutine calc_mrs(ntg,iz,dt,idif,pns,peff,	             	  &
                         vs,pover1,pint1,perk1,perk2,eva1,evi1,          &
                         mqabvs1,kno_ab,         			  &
                         supint1,supinf1,              			  &
                         xbil,anzelem,		                	  &
                         anzlay,m_vers,nmrqa,            		  &
                         nhyd,nhydtyp,isv,nw_bianf,			  &
                         pathn,ilen,naja,namo,natg,                   	  &
                         l3,nanf,nanfstd,xanf,nanzanf,nanzanf1,nanff,     &
                         fvsum,fnsum,mrfvsum,m_bil,m_hydtyp,ivor,slash,              &
                         idatsa20,naja20,m_nhyd,namger,LEN_GER,hw_mr)

!**************************************ÄNDERUNGEN***********************************************


!***************************************BESCHREIBUNG********************************************

!***************************************EIN-/AUSGABE********************************************
!c
!c              ntg                     Teilgebietsnummer welche die MRS enthält (aktuell berechnetes TG)
!c              iz                      aktueller Zykluszaehler
!c              dt              [h]     globaler Zeitschritt (aus *.konfig)
!c              idif                    Zeitschritte im aktuellen Zyklus
!c              pns                     Flag für synt. Niederschlag
!c              peff   		[mm/h]	effektivniederschlag
!c		vs		[mm/h]	potentielle transpiration

!c              anzlayy                 Anzahl der Bodenschichten in einem bestimmten Hydrotop.
!c              perkl                   Perkolation in einer Schicht in einem bestimmten Hydrotop
!c              pover(nn)       [mm]    Oberflaechenabfluss je Hydrotop und je Zeitschritt t im Zyklus noch
!c                                      nicht auf die nat. Flaeche des Hydrotpes bezogen.
!c              suevalay(nn,ilay)       Summe der Evaporation pro Hydrotop (nn) und je Bodenschicht (ilay).
!c              anzelem                 Anzahl der Hydrotope im aktuellen Teilgebiet
!c
!c      Detailausgabe Hydrotop
!c              suphort(nn)             Summe des Oberflaechenabflusses (noch nicht auf nat. Flaeche bezogen)
!c                                      je Hydrotop für jeden Zyklus.
!c		supstau(nn,ilay)        Die Summe des Stauwassers je Hydtrotop und Schicht für jeden Zyklus.
!c		supbod(nn,ilay)         Die Summe der Differenz des Stauwassers mit und ohne Korrekturwert
!c                                      aus dem Teilgebiet für jeden Zyklus
!c
!c
!c	eingabe m_wep           [mm]    Welkepunkt 	der Bodenschichten je Hydrotop
!c              m_nfk           [mm]    Feldkapazität             -"-
!c              m_bmax          [mm]    max. Bodenfeuchte         -"-
!c              m_tief          [mm]    Schichtdicke              -"-
!c              m_cin           [mm]    Infiltrationskapazität    -"-
!c              m_cex           [mm]    Perkolationskapazität     -"-
!c              m_kf            [mm]    Durchlässikgkeitswert (Darcy) -"-
!c              m_retlay        	Retensionkonstante        -"-
!c              anzlay                  Schichtenanzahl des Bodenprofils je Hydrotop
!c              m_vers                  Versiegelungsgrad aus der Hydrotopdatei
!c              nmrqa                   Kanalnummer für die Ergebnisdatei der Muldenrigole
!c              m_hydTyp                Entwässerungsart der versiegelten Flächen je Hydrotops (z.B. in MRS,Gründach)
!c
!c	ausgabe	zeitreihen
!c		pint1(t)  	[mm/h]	interflow summiert über den Zeitschritt t im Element
!c		perk1(t)  	[mm/h]	perkolation               -"-
!c              perk2(t)  	[mm/h]	perkolation               -"-                  (Grundwasserneubildung)
!c              qver_mr(t)      [mm/h]  Ueberlauf aus der Mulde ist dem versiegelten Abfluss zuzuschlagen
!c

!c	     summenwerte
!c		suevi		[mm]	verdunstung interzeption
!c		sueva		[mm]	verdunstung aus bodenspeicher
!c		supint		[mm]	summe interflow
!c		suboden		[mm]	differenz bodenspeicher
!c              supob(nn)       [mm]    Summe des Niederschlags im Zyklus pro Hydrotop
!c              supob1          [mm]    Summe des Niederschlags im Zyklus über das gesamte
!c                                      Teilgebiet bezogen die natürliche Flaeche ->
!c					supob1 = supob(nn)*nutzprz(nn)

!c           Mulden-Rigolen-System
!c              nutzmr(nn)      [m**2]  Versiegelte Fläche des an die MRS angeschlossene Fläche
!c              nutzmr1         [m**2]  gesamte angeschlossene versiegelte Fläche
!               nfakt_mr                Fläche Hydrotope die in MRS entwässeren / MRS Fläche  

!******************************************VARIABLEN********************************************
USE generic_LOGGER
USE Units
USE generic_LOG_FILE
USE generic_NAConstants
USE generic_ANF_CONS

IMPLICIT NONE

include      'include\param.cmn'
include      'include.bod'
include      'include\is.cmn'
include      'include\b4n.cmn'

logical	lop
1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: calc_mrs.f90                   ****',/,10x,     &
      &'****     ZYKLUS:',I6,'                           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (calc_mrs.f90)            ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

INTEGER	:: t,it,itime,idif,itag,imona,ijahr,ijaha,idatum,nast,ttemp,isstd
INTEGER :: idatsa20,naja20
INTEGER :: nn,nmrqa,nb,nv,ivor,l3,isv,ianf,itimevor,i2,i1,i,is,iz
INTEGER :: ilay,ntg,ju0gfu,lastchar
REAL    :: peff(idim),pri,vs(idim),fvsum,fnsum

INTEGER, INTENT(IN) :: LEN_GER
REAL, INTENT(INOUT) ::	eva1(idim),evi1(idim)
real	perk,pint,pover,evi,eva
REAL, INTENT(INOUT) :: perk1(idim),perk2(idim)
real    pint1(idim)
REAL, INTENT(OUT) :: pover1(idim)
REAL :: hw_mr(idim)
real	pstau(maxlay),pintlay(maxlay)
real	suintlay(maxlay)
real	evpotlay(maxlay),eval(maxlay)
real	perkl(2*idimnutz),inf
real	b(maxlay,0:idim),bof(2*idimnutz,maxlay)
real	bofstart(2*idimnutz,maxlay),bistart
real	bi

real	supre,biend
real	suevi,sueva,supob
real	superk,suphort                                                                                           
real	supstau(maxlay),supbod(maxlay)
real	supsat(maxlay),suevalay(maxlay)
real	bdif(maxlay),superklay(maxlay)
real	dtmax,dtn,dt
real	supover,supint
REAL    suepot,suepotv

character*1	pns,slash
real	bofsum,prinp,prest


!cccccccccccccccccccccccccccccccccc
!c
!c      Mulden-Rigolen
!c

REAL :: sumqabvs1,mrfvsum,hw!,pri_mrzu
REAL :: fk,bfmax,tieflay,qmr1,aktbof,kf
REAL :: kfmr,bmr,drd,drs,drks,mqabvs, &
	mqabvs1(idim),sumqabvs,mrperkm,mrgwszu,flaechmr,nfakt_mr,nprzvs,nprzn,supobmr,supobmr1


CHARACTER (LEN=120) :: nammr
CHARACTER (LEN=LEN_GER) :: namger
INTEGER, INTENT(OUT) :: kno_ab
INTEGER :: m_hydtyp(2*idimnutz),mrelem(2,2*idimnutz)
CHARACTER (LEN=10) :: mrbod(idimnutz2),mrnutz
REAL :: dummyArray(maxlay)

!ccccccccccccccccccccccccccccccccccc
!c
!c	bodenparameter Mulden-Rigole
!c
real	m_wep(maxlay,idimnutz2),m_nfk(maxlay,idimnutz2),          &
        m_bmax(maxlay,idimnutz2),m_tief(maxlay,idimnutz2),         &
        m_cin(maxlay,idimnutz2),m_cex(maxlay,idimnutz2),           &
        m_retlay(maxlay,idimnutz2),m_kf(maxlay,idimnutz2)
REAL testBOFmax,testBOF

!character(LEN=10) :: arraymrbod(idimnutz2)
character(LEN=8)  :: m_typ(maxlay,idimnutz2)


integer	anzlay(idimnutz2),anzlayy


integer	nhyd,nhydtyp
real	perkout(idim)
real	m_perkout(13)
integer	anzelem
real	xt,xtb


real	xcin,xcex,boderr,xmax


!c anfangsinhalte = endwerte zyklus iz-1

real	nw_bianf(jdim,2*idimnutz)
real	nw_boanf(jdim,2*idimnutz,maxlay)

!cccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	variablen vegetationsparameter

real	kc1(1),kc2(1),wt1(1)
real	wt2(1),lai1(1),lai2(1)
real	time1(1),time2(1)
real	kc(1),lai(1),eptt!,wt(1)

integer	ndg,ilen,ilen1
integer	naja,namo,natg

character(LEN=80) :: pathn,namveg,text



!cccccccccccccccccccccc
!c
!c boden und nutzungsabhaengige durchwurzelungstiefe
!c
real	wt_b(1,1),wt_bm(1,2)
real	time_b1(1),time_b2(1)
real	wtsum,wtlay(0:maxlay)

real	m_vers(2*idimnutz)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	bilanzgroessen

real	supint1,supinf1,suboden
real    supover1,superk1,suboden1
real	supob1,suinf,sudif1,sudif
real	xbil(21)

!ccccccccccccccc
!c
!c	anfangswerte lzsim-datein
!c
real	xanf(idimanf)
integer	nanf(idimanf),nanzanf,nanff
INTEGER :: nanfstd(idimanf)
INTEGER	:: sjahr,smona,stag,sdatum
REAL 	:: sstd
INTEGER :: nanzanf1


!cccccccccccccc
!c
!c	ausgabe hydrotope/statistik
!c
real	m_bil(jdim,20),m_nhyd(2*idimnutz)

INTEGER :: ndebug,ierr
LOGICAL :: has_mrs

!Änderung 11.7. 2008 CB : Formalparameter m_wcr,m_wcs,m_alpha,m_kff,m_n,m_l,m_vmacro,m_bf0,m_r,vg,m_xlpr zugefügt,
! da sie in inp_bod_b benötigt werden (sonst keine Verwendung in calc_mr.f90).


REAL :: m_alpha(maxlay,idimnutz2),m_n(maxlay,idimnutz2),m_l(maxlay,idimnutz2),m_vmacro(maxlay,idimnutz2),m_r(maxlay,idimnutz2)
INTEGER :: vg

!Änderung 2.6. 2008 KV, CB : Formalparameter imic, hinzugefügt, da sie in bodf_n benötigt werden.
REAL :: Imic, infl

!******************************************ANWEISUNGEN******************************************
ndebug = -1
IF(DEBUG) THEN
   CALL getLogFile( ndebug )
   WRITE(ndebug,1) iz
ENDIF

!cccccccccccccccccccc
!c
!c      INIT VARS
!c
nast = 0.
supre=0.
suepot=0.
suepotv=0.
suphort=0.
suevi=0.
sueva=0.
supob=0.
supobmr=0.
supover=0.
supint=0.
superk=0.
sumqabvs=0.
mrfvsum=0.
nammr = ''

DO ilay=1,maxlay
   suintlay(ilay)=0.
   supbod(ilay)=0.
   supstau(ilay)=0.
   supsat(ilay)=0.
   superklay(ilay)=0.
   suevalay(ilay)=0.
ENDDO

DO i1=1,13
      m_perkout(i1)=0.
ENDDO


DO ilay=1,maxlay
       perkl(ilay)=0.
       ! Initilaisiert des dummy Korrekturfaktor array
       dummyArray(ilay) = 1.0
ENDDO


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      Vorbereitung Mulden-Rigolen-Daten
!c      Berechnet den Anteil der versiegelten Flaeche der als
!c      Mulden-Rigolen-Zufluss gekennzeichneten Hydrotope
!c	Annahme: Nur ein Mulden-Rigolen-Element pro TG
!c



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      Einlesen der Parameter für die Mulden-Rigolen
!c
kfmr=0
bmr=0
drd=0
drs=0
drks=0
kno_ab=NO_MR_ELEMENT
mrperkm=0.
mrgwszu=0.
flaechmr=0.

ilen1=lastchar(namger)
nammr(1:ilen+ilen1+17)=pathn(1:ilen)//'inp.dat'//  &
      & slash//namger(1:ilen1)//'.mr'

call inp_mrs (ntg,nammr,LEN_TRIM(nammr),mrnutz,mrbod,mrelem,	&
	        kfmr,bmr,drd,drs,drks,kno_ab,mrperkm,mrgwszu,	&
                flaechmr)

has_mrs = .FALSE.
DO nn=1, anzelem
    IF( m_hydTyp(nn) == HYDTYP_MRS_KEY ) THEN
        ! berechnet die gesamte versiegelete Fläche die in dieses Element entwässert
        mrfvsum = mrfvsum + m_vers(nn) * m_nhyd(nn) / (1 - m_vers(nn) )! m² * 1e-6  ! km²
        has_mrs = .TRUE.
    END IF
END DO
! faktor zur erfassung des mr-bestandsniederschlags
nfakt_mr = mrfvsum/flaechmr

nprzvs = mrfvsum*1e-6/fvsum
nprzn = flaechmr*1e-6/fnsum

! Wenn es keine Mulden-Rigolen gibt, nicht rechnen!!
IF ( has_mrs .AND. flaechmr > 0 ) THEN
   WRITE (nres,1489) ntg
   1489  format(/' Rechne Mulden-Rigolen im Teilgebiet: ' ,i5/)
   IF(DEBUG) WRITE (ndebug,*)'Rechne TG-NR. ',ntg,'mit Mulden-Rigole!'
   
   CONTINUE
ELSE
   IF(DEBUG) WRITE (ndebug,*) 'Kein MRS für TG-NR. ',ntg,'gefunden!'
   RETURN
END IF

IF ( DEBUG ) THEN
    WRITE (ndebug,*) '********************************************'
    WRITE (ndebug,*) '*  EINGANGSDATEN:'
    WRITE (ndebug,*) '*  Mulden-Rigolen-Fläche:',flaechmr
    WRITE (ndebug,*) '*  Angeschlossene versiegelte Fläche:',mrfvsum
    WRITE (ndebug,*) '*  Niederschlagsfaktor:',nfakt_mr
    WRITE (ndebug,*) '*  Verhältnis versiegelte Flächen:',nprzvs
    WRITE (ndebug,*) '*  Verhältnis natürliche Fläche:',nprzn
    WRITE (ndebug,*) '********************************************'
ENDIF
! Einlesen der boden_art.dat und boden.dat Dateien
! Die MR werden nicht mit den Korrekturwerten aus der Gebietsdatei korrigiert.
! Die bereits bestehende Einlese-Routine verlangt aber Korrekturwerte, hier wird
! also nur ein dummy Array (initialisiert mit 1.0) übergeben.
CALL inp_bod_b (isim,iz,pathn,ilen,                    		&
               1,mrbod,1,mrelem,                        	&
               anzlay,m_wep,m_nfk,m_bmax,m_tief,                &
               m_cin,m_cex,m_kf,m_retlay,nw_boanf,m_typ,        &
               isv,dummyArray,dummyArray,dummyArray,dummyArray,	&
               dummyArray,dummyArray,dummyArray,slash,     &
               m_alpha,m_n,m_l,m_vmacro,m_r,vg)


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	vorbereitung  d a t e n a u s g a b e
!c

! l3 = kanalnummer der Ergebnisdatei *_bsp.dat
lop = .FALSE.
IF (l3 > 0) THEN
   DO is = 1,ingmax
      IF (ntg == ntgout(is) ) THEN
	 lop = .TRUE.
      ENDIF
   ENDDO
ENDIF
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	interne zeitschleife, vorgabe dtmax,
!c	laufindex wird bestimmt
!c

ianf=1
dtmax=8.
IF (dt > dtmax) THEN
   itimevor=INT((dt -.01)/dtmax + 1)
ELSE
   itimevor=1
ENDIF

!c Setzen der nutzungs- und boden paramter, sowie die Anzahl der Bodenschichten des MRS.
!c Da das MR-Element nur aus einem Bodenprofil und einer Nutzung besteht sind diese
!c Parameter immer gleich
nb=mrelem(1,1)
nv=mrelem(2,1)
anzlayy=anzlay(nb)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der  v e g e t a t i o n s  parameter
!c	 - vorbereitung -    oeffne datenfile und einlesen bis akt. wert
!c
ndg=ju0gfu()
namveg=''
ilen1=lastchar( mrnutz )
namveg(1:ilen+ilen1+14)=pathn(1:ilen)//'hydro.top'//    &
      slash//mrnutz(1:ilen1)//'.nuz'
open (ndg,iostat=ierr,err=996,status='old',file=namveg)
ijaha=0
read(ndg,'(a)',end=997) text
read(ndg,'(a)',end=997) text
if ( text(1:6) == 'ideali') then
   ijaha=naja
endif
read(ndg,'(a)') text
call inp_veg_b(ndg,ijaha,natg,namo,naja,pns,       &
              time1(1),kc1(1),wt1(1),lai1(1))
if ( pns == 's') time1(1)=0.

if ( time1(nv) > 0.) then
   write(nerr,435)
   call writeLogString(6, &
        &'Fehler in den Daten des idealisierten Jahresganges. 1. Wert ist vor Beginn des Simulationszeitraums anzugeben!', &
        & 'Error in ideal Year of natural land use. 1st value should be before simulation start.','','','','','','calc_mrs')

   time2(nv)=time1(1)
   kc2(1)=kc1(1)
   time1(1)=0.
   GOTO 433
endif

435   format('xxx fehler in angabe bedeckungsgrad!',/                  &
            'xxx erster wert muss vor beginn d. simulationszeitraums '/  &
               ,'xxx oder niederschlagswerten liegen.')
432   call inp_veg_b(ndg,ijaha,natg,namo,naja,pns,         &
                    time2(1),kc2(1),wt2(1),lai2(1))

if ( pns == 's') time2(1)=100000.

if ( time2(nv) < 0.) then
   time1(1)=time2(1)
   kc1(1)=kc2(1)
   wt1(nv)=wt2(1)
   lai1(1)=lai2(1)
   goto 432
endif
433   continue

time_b2(1)=-1.
close( ndg )

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebernahme der anfangswerte = endwerte zyklus iz-1
!c	interzeptionsspeicher u. bodenfeuchte / hydrotop
!c      TODO: Wie läuft das hier mit den Anfangswerten??
!c            Es ist nur ein element welches berechnet wird (MRS-Element)

! Anfangswert Interceptionspeicher aus *.geb Datei
nw_bianf(isv,1)= bianf
bistart = nw_bianf(isv,1)
bi = nw_bianf(isv,1)

DO ilay=1,anzlayy
   bof(1,ilay)=nw_boanf(isv,1,ilay)
   bofstart(1,ilay)=nw_boanf(isv,1,ilay)
ENDDO



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c            s c h l e i f e    z e i t s c h r i t t
!c            =========================================

time:DO t=1,idif
        if(t==32)then
             write(*,*)
        endif
        evi1(t)=0.
        eva1(t)=0.
        pover1(t)=0.
        pint1(t)=0.
        perk1(t)=0.
        perk2(t)=0.
        mqabvs1(t)=0.
!       pri_mrzu = 0.

        IF (DEBUG) THEN
           WRITE (ndebug,*) '************************** Timestep: ',t,'*******************************************'
        END IF

	! uebernahme der zeitabhaengigen parameter/vegetation
	! kc:  vegetationsabh. korrektur transpiration
	! wt:  wurzeltiefe
	! lai: leaf area index
        ndg = ju0gfu()
	    xt=real(t)*dt/24.
	    xtb=real(t)*dt/24.
	    if ( xt > time2(1) ) then
	       time1(1)=time2(1)
	       kc1(1)=kc2(1)
	       wt1(1)=wt2(1)
	       lai1(1)=lai2(1)
	       namveg=''
	       ilen1=lastchar(mrnutz)
	       namveg(1:ilen+ilen1+14)=pathn(1:ilen)//'hydro.top'//  &
                                   slash//mrnutz(1:ilen1)//'.nuz'
	       open (ndg,iostat=ierr,err=996,status='old',file=namveg)

	       ijaha=0
	       read(ndg,'(a)',end=997) text
	       read(ndg,'(a)',end=997) text
	       if(text(1:6).eq.'ideali') then
	          ijaha=naja
	       endif
	       read(ndg,'(a)') text

6400	   call inp_veg_b(ndg,ijaha,natg,namo,naja,pns,     &
                               time2(1),kc2(1),wt2(1),lai2(1))
	       if ( time2(1) < xt) goto 6400
           
           close( ndg )
	    endif

	    if(xtb > time_b2(1) .and. wt2(1) < 0.) then
	      namveg=''
	      ilen1=lastchar(mrnutz)
	      namveg(1:ilen+ilen1+14)=pathn(1:ilen)//'hydro.top'//  &
     		   	                  slash//mrnutz(1:ilen1)//'.wtb'
	      ijaha=0

	      call inp_veg_b_wt(namveg,natg,namo,naja,              &
                               xtb,time_b1(1),time_b2(1),      &
                               wt_bm,1,mrbod,pns)
	    endif

	    xt = (xt-time1(1))/(time2(1)-time1(1))
	    kc(nv) = xt*(kc2(1)-kc1(1)) + kc1(1)
	    if (wt2(1) > 0) then
	       wt_b(1,1) = xt*(wt2(1)-wt1(1)) + wt1(1)
	    else
	      xtb = (xtb-time_b1(1))/(time_b2(1)-time_b1(1))
	      if(xtb < 0. .or. xtb > 1.) then
	         write(*,908)xtb,time_b1(1),time_b2(1)
   	         write(nerr,908)xtb,time_b1(1),time_b2(1)

             call writeLogIntReal(7, 'Fehler in den Daten des idealisierten Jahresganges (Durchwurzelungstiefe)!', &
                    & 'Error in ideal year of natural land use (roots depth)!','','',0,'xtb',xtb,'calc_mrs')
             call writeFooter()
   	         stop 'boden   duchw.tiefe'
	      endif
	      wt_b(1,1) = xtb*(wt_bm(1,2)-wt_bm(1,1)) + wt_bm(1,1)
	    endif
	    lai(1) = xt*(lai2(1)-lai1(1)) + lai1(1)


908     format('xxx fehler bei zeitpunkten bodenabhaengiger ',          &
              'durchwurzelungstiefe!',/                               &
              'xxx zeitpunkte: ',(3(f9.4)))




!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c                     berechnung der interzeption
!c                     ---------------------------
!c	achtung: werte in mm/h
!c
        suepotv=suepotv+eptt*dt
        supre=supre+peff(t)*dt
	    bianf=bi
 	    bimax=lai(nv)
	    eptt=vs(t)*kc(nv)
        ! auf was bezieht sich die potentielle Verdunstung??
        ! verhältnis zwischen nat. MR-Fläche zu Reduzierter versiegelter Fläche
        suepot=suepot+vs(t)*dt

	    call incept(t,dt,bianf,bimax,peff(t),eptt,pri,evi,biend)
	    bi = biend
        !TODO: Summe anpassen (mqabgw,mqabvs)
	    supob=supob+pri*dt
	    supobmr=supobmr+pri*(1+nfakt_mr)*dt
	    eptt=( eptt - evi )

        if( DEBUG ) then
            write(ndebug,*) ' Bestands-N [mm/h]= ',pri,' für t = ',t,' supob= ',supob, '  eppt - evi=', eptt,'-',evi,'= ',&
                            eptt-evi
        endif

!cccccccccccccccccccccccccccccccccccccc
!c
!c	festsetzung von itime  fuer interne zeitschleife bodenspeicher
!c	so, dass pri < .1 * (bfmax-bfakt)
!c

	     itime=itimevor
3011	 dtn=dt/itime
	     prinp=pri*dtn*nfakt_mr
	     if (prinp*10. > m_bmax(1,nb) - bof(1,1) ) then
	       itime=itime*2
	       if ( itime < 50) goto 3011
	       dtn=dt/itime
	     endif
 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c  	eplay(ilay): pot. evaporation fuer alle layer [mm]
!c	wt [m], m_tief[dm]
!c  	achtung: eptt[mm/h], evpotlay[mm], bezogen auf interne
!c	zeitschleife mit zeitschrittlaenge dtn
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         call evapot(dtn,anzlayy,nb,nv,1,eptt,m_tief,m_bmax,bof,wt_b,wtlay,bofsum,	&
	  	               wtsum,evpotlay)
	     pover=0.
	     eva=0.
	     pint=0.
	     perk=0.
         mqabvs=0.

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c  beginn i n t e r n e  z e i t s c h l e i f e  bodenspeicher
!c
IF (DEBUG) THEN
    WRITE (ndebug,*)
    WRITE (ndebug,*) '+++++++++ interne-zeitschleife: max-zeitschritte=',itime,' dtn=',dtn,'++++++++++++++++++'
ENDIF
    i_time:DO it=1,itime

! In diesem Loop wird für jede schicht die Infiltration,Perkolation,Evaporation
! und aktuelle Bodenfeuchte berechnet
                 IF (DEBUG) THEN
                    WRITE (ndebug,*)
                    WRITE (ndebug,*) ' BERECHNUNG von inf, perk, eva und bof je Schicht'
                    WRITE (ndebug,*) ' Zeitschritt',it,' von ',itime
                 ENDIF
        layer:DO ilay=1,anzlayy
	             if ( ilay == 1 ) then
! CK 03.02.06  Zufluss zu einer Mulden-Rigole, alle nicht in das Kanalnetz angeschlossenen
! Hydrotope leiten deren Abfluss in die MR
! TODO: Hier könnte nochmal eine Verzögerung die bei einem Grabensystem entsteht
!       eingebaut werden. Muldenrückhalt!
                 ! TODO: muss hier wirklich der Faktor mrfvsum/flaechmr eingesetzt sein. Ich glaube schon da
                 !       der Niederschlag sich auf den Fläche der MR beziehen l/s = m³/s = (m/s * 1/m²)
                    prinp = (1 + nfakt_mr)*pri * dtn
	             else
	                prinp=perkl(ilay-1)
	             endif
	             xcin=m_cin(ilay,nb)
	             xcex=m_cex(ilay,nb)

	             boderr=bof(1,ilay)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c                     berechnung der bodenfeuchte
!c                     ---------------------------
!c	achtung: berechnete werte in mm
!c 	kennz:1: NASIM ansatz, kennz=2: Ansatz nach Wendlin
!Änderung 2.6. 2008 KV, CB: Parameter Imic,ilay,t,infl hinzugefügt, da sie in bodf_n verwendet werden.
!c
                 call bodf_n(1,dtn,prinp,evpotlay(ilay),0.,		     &
                      m_wep(ilay,nb),m_nfk(ilay,nb),m_bmax(ilay,nb),    &
                      xcin,xcex,bof(1,ilay),inf,eval(ilay),perkl(ilay),Imic,ilay,t,infl)
	          
                 boderr=boderr-bof(1,ilay)+(inf-eval(ilay)-perkl(ilay) )
                 if(DEBUG) then
                    write(ndebug,*) ' Schicht: ',ilay,'   prinp=',prinp,' bof= ',bof(1,ilay),' inf=',inf,' eval=',eval(ilay),&
                                    ' perkl=',perkl(ilay),' boderr=',boderr
                 endif

	             if ( abs(boderr) > 0.1) then
	                write (nerr,10101) t,ilay,boderr

                    call writeLogIntIntReal(6, 'Fehler bei der Berechnung der Bodenfeuchte!', &
                      & 'Error while calculation of the soil moisture.','','',0,'layer',ilay,'boderr',boderr,'calc_mrs')

		            10101	format(' fehler in muldenrigolen-routine!'/,                      &
                    	       ' zeitschritt: ',i4,/                             &
                               ' layer:       ',i4,/                             &
                               ' fehler:      ',i4)
	             endif
	             pstau(ilay)=0.
! Hier wird der Stauwasseranteil in jeder Schicht berechnet
	             if ( (prinp-inf) > 0. ) then
	                pstau(ilay)=prinp-inf
	             endif
	             if ( pstau(ilay) < 0. ) pstau(ilay)=0.
! Summiert die Evaporation für jedes Hydrotop
	             eva=eva+eval(ilay)
! Summiert die Evaporation für jedes Hydtrotop und je Schicht
	             suevalay(ilay)=suevalay(ilay)+eval(ilay)
!letzte Schicht wird gesondert behandelt ( GW-Neubildung über mrperkm )
	             if ( ilay == anzlayy ) then
	        	    if ( mrperkm > 0. ) then
		               xmax=mrperkm*dtn*3.6*1.e6
	                else
		               xmax=100000.
	                endif
	                if ( perkl(ilay) > xmax) then
! Der Anteil der nicht in den GW-Leiter fliessen kann (weil maxperk > perklayer )
! wird in einen Fiktiven Layer geschrieben -> immer in layer = anzlay +1
		               pstau(ilay+1)=perkl(ilay)-xmax
		               perkl(ilay)=xmax
		               if ( pstau(ilay + 1) < 0.) pstau(ilay + 1)=0.
	                else
		               pstau(ilay+1)=0.
	                endif
	                perk=perk+perkl(ilay)
	             endif !ende if letzte Schicht

! Summiert die Perkolation für jeden Layer je Hydrotop über den internen Zeitschritt dtn auf
	            superklay(ilay) = superklay(ilay) + perkl(ilay)
                 IF (DEBUG) THEN
		             WRITE (ndebug,*)' perk=',perk,' pover=', pover,' pstau=',pstau(ilay),' superklay=',superklay(ilay)
                 END IF

              ENDDO layer

              IF (DEBUG) THEN
                WRITE (ndebug,*)
                WRITE (ndebug,*) '++++++++++++  Berechnung des Oberflächenabflusses +++++++++++++++++++++'
                WRITE (ndebug,*)
              END IF

! Oberflaechenabfluss für Mulden-Rigole
	          suphort=suphort+pstau(1)
      inf_lay:DO ilay=anzlayy+1,2,-1
	             if ( pstau(ilay) > 0.) then
	      	        pintlay(ilay)=pstau(ilay)* m_retlay(ilay-1,nb)
	        	    prest=pstau(ilay)-pintlay(ilay)
	      	        supstau(ilay)=supstau(ilay)+pstau(ilay)
	      	        supbod(ilay)=supbod(ilay)+prest
	      	        bof(1,ilay-1)=bof(1,ilay-1)+prest
	             else
	                pintlay(ilay)=0.
	             endif
                 IF (DEBUG) THEN
                     WRITE (ndebug,*) 'Schicht: ',ilay, ' BOF= ', bof(1,ilay),' BFMAX= ',m_bmax(ilay,nb),' FK= ', &
                                       m_nfk(ilay,nb)+m_wep(ilay,nb) 
                 END IF

!Schicht 3 der MRS wird gesondert behandelt.
	             IF ( ilay == 3) THEN
                    qmr1 = 0
       		        fk = m_nfk(ilay,nb)+m_wep(ilay,nb)
       		        bfmax = m_bmax(ilay,nb)
      		        tieflay = m_tief(ilay,nb) * 100  ![mm]
		            kf = m_kf(ilay,nb)
                    testBOFmax=m_bmax(ilay+1,nb)-bof(1,ilay+1)
                    testBOF=bof(1,ilay)-fk
                 IF (DEBUG) THEN
                     WRITE (ndebug,*) ' Untere Schicht: ** BOF= ', bof(1,ilay+1),' BFMAX= ',m_bmax(ilay+1,nb) 
                     WRITE (ndebug,*) ' TEST BOF-FK =', testBOF, ' TEST BOFMAX-BOF =', testBOFmax
                 END IF
! berechnet den Mudldenabfluss, es kann nur etwas aus der MR rausfliessen wenn die aktuelle bodenfeuchte über fk liegt
! und die darunterliegende Schicht BFmax erreicht hat.
!                   IF ( bof(1,ilay) > fk-1e3 .AND. bof(1,ilay+1) >= m_bmax(ilay+1,nb)-1e3 ) THEN
                   IF ( testBOF > 0.001.AND.testBOFmax < 0.03 ) THEN
                      call qmr(dtn,drs,drks,drd,bmr,flaechmr,kf,fk,bof(1,ilay),bfmax,qmr1,1,tieflay,hw_mr)
                      aktbof = bof(1,ilay)-qmr1
                      IF ( aktbof < 0 ) then
                          aktbof = fk
                          mqabvs = mqabvs + bof(1,ilay) - fk
                      ELSE
                          mqabvs = mqabvs + qmr1
                      ENDIF
                   ELSE
                      qmr1 = 0
                      aktbof = bof(1,ilay)
                   END IF
!setzt die neue bodenfeuchte in dieser Schicht
                   bof(1,ilay) = aktbof
	             END IF
                 IF( DEBUG ) then
                    WRITE(ndebug,*) ' bof - bmax = ',bof(1,ilay-1),'-', m_bmax(ilay-1,nb),'=',bof(1,ilay-1) - m_bmax(ilay-1,nb), &
                                    ' qmr =',qmr1
                 ENDIF
	             if ( bof(1,ilay-1) > m_bmax(ilay-1,nb) ) then
	                prest=bof(1,ilay-1)-m_bmax(ilay-1,nb)
	                bof(1,ilay-1)=m_bmax(ilay-1,nb)
	                supsat(ilay-1)=supsat(ilay-1)+prest
	                if ( ilay > 2 )then
		               bof(1,ilay-2)=bof(1,ilay-2)+prest
	                else
		               pstau(ilay-1)=pstau(ilay-1)+prest
	                endif
                    IF( DEBUG ) THEN
                        WRITE(ndebug,*)' prest= bof - bmax =',prest,' pstau(ilay-1)= pstau(ilay-1) + prest=', pstau(ilay-1)
                    ENDIF
	             endif
!                 IF (DEBUG) THEN
!                    WRITE(ndebug,*) 'it=',it,' ** bof(1,',ilay,')= ',bof(1,ilay),' mqabvs=', &
!                                  mqabvs
!                 END IF
	         END DO inf_lay
! aufsummieren des Oberflaechenabflusses über alle interne Zeitschritte
             pover=pover+pstau(1)
! aufsummieren des Interflows für jeden Layer über alle interne Zeitschritte,
! ist i.d.R. null, da die Mulden-Rigolen keinen interflow zulassen sollten (boden.dat).
	         DO ilay=2,anzlayy+1
	  	       pint=pint+pintlay(ilay)
	   	       suintlay(ilay)=suintlay(ilay)+pintlay(ilay)
             ENDDO
           ENDDO i_time
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c ende interne zeitschleife fuer bodenspeicher
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
           hw_mr(t) = hw(tieflay,bfmax,fk,bof(1,3))

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	uebergabe der ergebnisse / hydrotop als summe / TG

!cccccccccccccccccccccccccccccc
!c
!c	ausgabe perkolation/hydrotop, Summation für den laufenden Zeitschritt
!c	rtr = Korrekturfaktor f. Aufteilung grundwasser/tiefengrundwasser
!c	       aus gebietsdatei *.geb eingelesen!c

	       if ( nhydtyp == 3 ) perkout(t)=perk

	       if ( nhydtyp == 4 .and. iz > 1 .and. nhyd > 0) then
	           m_perkout(13)=m_perkout(13)+perk
	           i2=int(t*12./365.25)+1
	           if ( i2 > 12) i2=12
	               m_perkout(i2)=m_perkout(i2)+perk
	       endif
	       evi1(t)=evi1(t)+evi*nprzn
	       eva1(t)=eva1(t)+eva*nprzn
	       pover1(t)=pover1(t)+pover*nprzvs
	       pint1(t)=pint1(t)+pint*nprzvs
	       perk1(t)=perk1(t)+perk*nprzvs
	       perk2(t)=perk2(t)+perk*mrgwszu*rtr*nprzvs
           mqabvs1(t)=mqabvs1(t)+mqabvs*nprzvs

           suevi=suevi+evi*dt
	       sueva=sueva+eva
           supover=supover+pover
	       supint=supint+pint
	       superk=superk+perk
           sumqabvs=sumqabvs+mqabvs

           IF (DEBUG) THEN
               WRITE (ndebug,*)
               WRITE (ndebug,4)ntg,t,evi1(t),eva1(t),pover1(t),pint1(t),perk1(t),perk2(t),mqabvs1(t)
               4    FORMAT (//' SUMME ÜBER TG-Nr. ',i4,' für MR im ZEITSCHRITT t = ',i4,/ &
        	         ' evi1=',f9.3,' eva1=',f9.3,' qver_mr=',f9.3,' pint1=',f9.3,/ &
                     ' perk1=',f9.3,' perk2=',f9.3,' mqabvs1=',f9.6)
               WRITE (ndebug,5)ntg,suevi,sueva,supover,supint,superk,sumqabvs
               5    FORMAT (//' BILANZ ÜBER TG-Nr. ',i4,' für MR',/ &
        	         ' suevi=',f9.3,' sueva=',f9.3,' supover(mr überlauf)=',f9.3,' supint=',f9.3,/ &
                     ' superk=',f9.3,' sumqabvs=',f9.3)
               WRITE (ndebug,*)
           END IF


           DO ilay=1,anzlayy
	           b(ilay,t)=bof(1,ilay)
           ENDDO

!cccccccccccccccccccc
!c SK/EP 28.02.03
!c Datenausgabe Anfangswerte LZSIM
!c Das Anfangsdatum des Zykluses 8 stellig wird in Jahr, Monat und Tag gesplittet, um die
!c Funktion cdatum anzuwenden.
!JH 28.09.2004 Anfangswerte werden auch aus einer Kurzzeitsimulation gespeichert.
!              Hierzu musste das Format des *.lzs-Files geaendert werden, um die Stunden hinzuzufügen.
!              Abfrage (If-Schliefe) um lz-kz erweitert.
           IF ( isim == 0 ) THEN                                                      ! Langzeitsim.
              IF ( nanf(1) == idatsa20 .and. nanzanf == 0 ) THEN
                 ijahr=(nanf(1)/10**4)
                 imona=((nanf(1)-ijahr*10**4)/100)
                 itag =(nanf(1)-ijahr*10**4-imona*100)

!c SK/EP 28.02.03
!c Die do Variable t: do t=1, idif (Anzahl der Zeitschritte im Zyklus) muß neu initialisiert werden,
!c da keine do-Variable übergeben werden darf. Die Funktion cdatum setzt idatum für jeden Zeitschritt
!c neu, damit so in der lzsim alle Anfangswerte herausgeschrieben werden können.
                 ttemp=t
                 call cdatum (itag,imona,ijahr,ttemp)                         ! 1 Tag = 1 Simulationsschritt t weiter
                 idatum=ijahr*10**4+imona*10**2+itag
	             write(nanff,'(i8,2x,a2,a2,2x,i4,1x,a4)') idatum,'00 ',ANF_TIME_H,1, ANF_MRS_BF
	             write(nanff,'(i4,f7.2,10(f7.2))')1,bi,(bof(1,ilay),ilay=1,anzlayy)
              ELSEIF ( nanzanf > 0.and. t >= xanf(ianf) ) THEN
	             write(nanff,'(i8,2x,a2,a2,2x,i4,1x,a4)') nanf(ianf),'00 ', ANF_TIME_H ,1, ANF_MRS_BF
	             write(nanff,'(i4,f7.2,10(f7.2))')1,bi,(bof(1,ilay),ilay=1,anzlayy)
	             ianf=ianf+1
	             if(ianf > nanzanf) nanzanf=0
              ENDIF
           ELSEIF (pns /='s') THEN                                          ! Kurzzeitsimulation natürlicher Niederschlag
              IF ( ianf <= nanzanf1 ) THEN
                 IF ( t == 1 ) THEN
                    sjahr = naja20
                    smona = namo
                    stag = natg
                    sstd = REAL(nast)
                 ENDIF
                 sstd = sstd + dt
                 IF (sstd >= 24.0) THEN
                    sstd = sstd - 24.0
                    call cdatum (stag,smona,sjahr,1)
                 ENDIF
                 sdatum = sjahr*10**4 + smona*10**2 + stag
                 isstd=INT(sstd)
                 IF (nanf(ianf) == sdatum .and. nanfstd(ianf) == isstd) THEN
                    write(nanff,'(i8,2x,i2,a2,2x,i4,1x,a4)') nanf(ianf),nanfstd(ianf),ANF_TIME_H,1,ANF_MRS_BF
                    write(nanff,'(i4,f7.2,10(f7.2))')1,bi,(bof(1,ilay),ilay=1,anzlayy)
                    ianf = ianf + 1
                    IF ( ianf > nanzanf1) THEN
                       CLOSE(nanff)
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF

ENDDO time
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c		ende zeitschleife
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	speichern der endwerte = anfangswerte zyklus iz+1
!c     TODO: Wie läuft das hier mit den anfangswerten ?

nw_bianf(isv,1) = bi
DO ilay=1,anzlay(nb)
   nw_boanf(isv,1,ilay) = bof(1,ilay)
ENDDO



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	bilanzierung und kontrolle der ergebnisse
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!ccccccccccc
!c  bilanzierung gesamtgebiet bodenspeicher

call u1null(bdif,maxlay)
supint1=0.
supinf1=0.
supob1=0.
supobmr1=0.
supover1=0.
superk1=0.
suboden1=0.
sumqabvs1=0.
DO ilay=1,anzlayy
	bdif(ilay)=bofstart(1,ilay)-bof(1,ilay)
	suboden=suboden+bdif(ilay)
ENDDO
write(ndebug,*) '+++ suboden= ',suboden

suinf=bistart-bi

supint1=supint*nprzvs
supinf1=(supob-supover)*nprzvs
!Änderung CB 14.07.2008: Faktor nprzvs bezieht sich auf versiegelte Flächen, für diese wird die Interzeption nicht berechnet.
!Für die Berechnung des Bestandsniederschlag der MR muss die gesamte Fläche der MR eingehen.
!supob1=supob*nprzvs
supob1=supob
supobmr1=supobmr*nprzvs
supover1=supover*nprzvs
superk1=superk*nprzvs
suboden1=suboden*nprzvs
sumqabvs1 =sumqabvs*nprzvs




!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c     a u s g a b e
!c
!c         bodenfeuchte         b o f . d a t (nur erstes Hydrotop)
!c         
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
IF(DEBUG) nmrqa = 999
if ( nmrqa > 0 ) then
   DO ilay=1,anzlayy
      write(ndebug,'(/3(i8))')ntg,iz,idif
      write(ndebug,'(a,i2,a,f6.1,a,f6.1,a)') 'bodenlayer: ',ilay,                              &
                  ' nutz.porenvol.:',m_bmax(ilay,nb),'mm    nutz.feldk.:',m_nfk(ilay,nb),' mm'
      write(ndebug,'(10(f8.3,1x))') (b(ilay,i),i=1,idif)
   ENDDO
endif
! TODO: wo schreibe ich die eva und die evi des MRS hin??


!cccccccccccccccccccccccccccccc
!c
!c	ausgabe perkolation/hydrotop
!c
if ( nhydtyp  == 3 .and. nhyd > 0 ) then
   write(nhyd,'(/2(i8)/)') iz,idif
   write(nhyd,'(10(f8.2,1x))') (perkout(t),t=1,idif)
endif
if ( nhydtyp == 4 .and. nhyd > 0) then
   write(nhyd,10010)m_perkout(13),(m_perkout(i1),i1=1,12)
   10010 format(13((f8.2),','))
endif

!ccccccccccccccc
!c
!c	bildschirm-ausgabe

!sudif1 = supre-suevi-supob+suinf
!sudif=supob-sueva-supover-supint-superk+suboden

!IF (DEBUG) THEN
!   WRITE (ndebug,*)'MRS-Element ** sumqabvs1=',sumqabvs1,' ** sudif=',sudif
!END IF

!ccccccccccccccc
!c
!c	bildschirm-ausgabe zusammenfassung

write(nres,9251)ntg
sudif1 = supre-suevi-supob1+suinf
write(nres,9149)supre,suevi,supob1,suinf,sudif1,suepot,suepotv,  &
     		suevi+sueva
sudif=supobmr1-supover1+sueva-supint1-superk1+suboden1-sumqabvs1
write(nres,9152)supobmr1,supover1,supobmr1-supover1,&
     		sueva,supint1,superk1,sumqabvs1,suboden1,sudif
IF (DEBUG) THEN
    write(ndebug,9251)ntg
    write(ndebug,9149)supre,suevi,supob1,suinf,sudif1,suepot,suepotv,suevi+sueva
    write(ndebug,9152)supobmr1,supover1,supobmr1-supover1,sueva,supint1,superk1,sumqabvs1,suboden1,sudif
ENDIF
!ccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      Bilanz im Teilgebiet
!c
IF ( iz > ivor) then
   ! Da der selbe Niederschlag/pot.Verd. wie für die natürlichen Flächen
   !m_bil(isv,1) = m_bil(isv,1) + supre
   !m_bil(isv,2) = m_bil(isv,2) + suepot
   m_bil(isv,3)=m_bil(isv,3)+suevi
   m_bil(isv,4)=m_bil(isv,4)+sueva
   m_bil(isv,5)=m_bil(isv,5)+supob1
   m_bil(isv,6)=m_bil(isv,6)+supover1
   m_bil(isv,7)=0.
   m_bil(isv,8)=m_bil(isv,8)+supint1
   m_bil(isv,9)=m_bil(isv,9)+superk1
!   m_bil(isv,21)= m_bil(isv,21) + sumqabvs1
ENDIF





!cccccccccccccccccccc
!c
!c	datenuebergabe gesamtbilanz

     ! xbil(1)=xbil(1)+supre*fnsum
     ! xbil(18)=xbil(18)+suepot*fnsum
     ! xbil(19)=xbil(19)+suepotv*fnsum
     ! xbil(2)=xbil(2)+suevi1*fnsum
     ! xbil(3)=xbil(3)+sueva1*fnsum
     ! xbil(4)=xbil(4)+supover1*fnsum
     ! xbil(5)=xbil(5)+supint1*fnsum
     ! xbil(6)=xbil(6)+superk1*fnsum
     ! xbil(7)=xbil(7)+suboden1*fnsum

      IF (DEBUG) THEN
          WRITE (ndebug,2)
      END IF

      return
!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

9149  format(/' speicherbilanz interzeption: '/,                    &
               ' niederschlag:(nach schneesp.)', f10.1,' mm'/,        &
               ' interz.verd.:                ', f10.1,' mm'/,        &
               ' bestandsniederschlag:        ', f10.1,' mm'/,        &
               ' differenz interz.speicher:   ', f10.1,' mm'/,        &
               ' fehler:                      ', f10.1,' mm'/,       &
               ' pot. verdunst. (grasref.):   ', f10.1,' mm'/,        &
               ' pot. verdunst.(akt. vegetat.):', f10.1,' mm'/,       &
               ' aktuelle verdunstung:        ', f10.1,' mm')
9152  format(/' speicherbilanz bodenspeicher:   ',//               &
               ' bestandsniederschlag:        ', f10.1,' mm'/,       &
               ' muldenueberlauf:                ', f10.1,' mm'/,       &
               ' [infiltration:               ', f10.1,' mm]'/,      &
               ' verdunstung (bodensp.):      ', f10.1,' mm'/,       &
               ' lateral. abfluss (interflow):', f10.1,' mm'/,       &
               ' perkolation:                 ', f10.1,' mm'/,       &
               ' rigolenabfluss:              ', f10.1,' mm'/,       &
               ' differenz bodenspeicher:     ', f10.1,' mm'/,       &
               ' fehler:                      ', f10.1,' mm'//)
9251  format(//'-----------------------------------------',//      &
               ' z u s a m m e n f a s s u n g  ',/                  &
               ' der bilanz das Mulden-Rigolen Element im teilgebiet:   ',i5,/)

996   write(nerr,906)namveg
906   format('xxx datenfile mit vegetationsdaten nicht gefunden!',/ &
               'xxx filename: ',a)

      call writeLogString( 7, 'Datenfile mit Vegetationsdaten nicht vorhanden!','File with vegetation data not found!', &
           & namveg,'','','','','calc_mrs')
      call writeFooter()
      stop

997   write(nerr,907)namveg
907   format('xxx fehler beim einlesen der vegetationsdaten!',/       &
     		'xxx filename: ',a)

       call writeLogString( 7, 'Fehler beim Einlesen der Vegetationsdaten!', 'Error reading the vegetation data!', &
           & namveg,'','','','','calc_mrs')
      call writeFooter()
      stop

end
