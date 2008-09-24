!     Last change:  JH   17 Jan 2007   12:17 pm

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
	  subroutine input (dt,iz,ntg,npin,peff,tmeans,vs,fn,   &
            vsg,ianzzft,flaech,idif,      &
			namnal,namfal,nameptal,       &
			tanf,qgwzufak,      &
			naja20,namo20,natg20,         &
               neja20,nemo20,netg20,      &
               namhydro,pns,slash,xjah,xwahl2)

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     ?		Wiebke Klauder (WK)	nur Umbenennung

!   28.02.03    Stephan Kraessig (SK)   Neue Version ist Jahr 2000 fähig und kann bei Angabe
!      -               &                von 1000001 als Anfangsdatum in der *.konfig Datei
!   04.03.03    Prof. Pasche (EP)       für jeden gerechneten Zeitschritt der Langzeitsimulation
!					Anfangswerte für die Kurzzeitsimulation erzeugen.
!   30.07.03 	Stephan Kraessig (SK)	Die Aufrufe der Subroutine zum Einlesen von Temperatur
!					und Verdunstung wurden von inp_nied.lz in inp_nied.tv
!					geändert.
!					Es ist immer noch möglich das ursprüngliche
!					BCE-Verdunstungsformat einzulesen.
!					Die Niederschlagsstation kann für jedes Teilgebiet
!					angezeigt werden.

!***************************************BESCHREIBUNG********************************************
!***************************************EIN-/AUSGABE********************************************
! Eingabe:
! Ausgabe:
!               fn              normierte Zeitflaechen auf Simaulationsschritt interpoliert
!               ianzzft         Anzahl notwendiger Simulationszeitschritte
!                	       	zur Abarbeitung der ZFT

! nicht verwendet:

!******************************************VARIABLEN********************************************
USE generic_LOGGER
USE Units
IMPLICIT NONE

include 'include\param.cmn'
include 'include.inp'

character*140 dummyinp
INTEGER maxchar
parameter (maxchar=120)
character*(maxchar) name(3),namfn,namft,namept
character*80 dummy,namfzf,namfal,nameptal
character*80 namnal,namezg,namhydro
character*10 cizs
character*1  kennz,pns

integer inum,nip,ntg,nfil,ianz,izs,izaehl,           &
        izmax,i,i2,i3,ntdif,iz,npin,           &
        ju0gfu,ierr,ipaja,ipamo,itgn,mon,jan,idatpa,        &
	izdif,ipatg,ntv,ipast,ianzzft,ilen,ju0nch,     &
	lfnr,ivb,ipot,itka,imka,ijka,iska,itke,imke,ijke,iske,             &
	ikom,idatak,idatek,istk,ip(12),itkz,                    &
	idif,najs,itr,tanf,il,tg1,mo1,ja1,st1,idat1,ilay,i1

integer anzlayy,kzif

integer ilname(3)

real    vsg,dt,abs,p(idim),p2(idimp),peff(idim),flaech,          &
     	datc,datca,datume,datuma,zdif,tmeans(idim),                 &
     	fn(idim),                                                        &
     	vs(idim),datce,faktn,v1,sump,          &
     	ftem,fver,ftema,fvera,qgwzufak,psum
REAL xjah,xwahl2
INTEGER namn,nemn

real	pint(idimt),vek1(idimt),xend,xstart
real	xsum1,xsum2
integer	idife,tend
!cccccccccccccccccccccccccccccccc
!c
!c	anfang- und endzeitpunkt, 2000 jahr faehig
!c
integer naja20,namo20,natg20
integer	neja20,nemo20,netg20

!cccccccccccccccccccccccccccccccccccc
!c
!c	nutzung pro hydrotop
!c
real 	pjsol,pdsol,pvsol
integer	ipver
common /syndat/ pjsol,pdsol,pvsol,ipver

!cccccccccccccccccccccccccccccccccccc
!c
!c	schneeparameter
!c
real		xwwo,xwwmax,xsnotem,xsnorad,xh0
character*20	snowtyp
character*1	slash



!c  initialisieren bzw. nullsetzen der felder und variablen

ianzzft = 0
inum = 0
ianz = 0
izs = 0
idatpa = 0
izdif = 0
zdif = 0.
nip = 0
nfil = 0
ntv = 0
ftem = 0.
fver = 0.
ftema = 0.
fvera = 0.
i = 0
call u0null(jatv,idim)
call u0null(motv,idim)
call u0null(itgtv,idim)
call u0null(idatum,idim)

!******************************************ANWEISUNGEN******************************************
      namezg(1:80)=pathn(1:ilend)
      namezg(ilend+1:ilend+8)='inp.dat/'
      ilen = ilend + 8
      namezg(ilen+1:ilen+ilenb)=namger
      ilen=ilen+ilenb

      namezg(ilen+1:ilen+4)='.geb'
      ilen = ilen+4

      if(ilen.gt.80)then
         write(nerr,'(3a)')'tree-name fuer den file',namezg,           &
                 'mehr als 80 zeichen.'
         call writeLogString(7,'Pfad fuer einen File hat mehr als 80 Zeichen.', 'Path for a file has more than 80 characters.', &
     	     & namezg,'','','','','input')

         write(nerr,'(a)')'programm-abbruch.'
         call writeFooter()
         stop   
      endif                   

!c  einlesen des g e b i e t s d a t e n s a t z e s


      inum = 0
      faktn = 1.0
      nip = ju0gfu()

      open (nip,iostat=ierr,err=9999,file=namezg,form='formatted',     &
            status='old')

111   ispk = 0
      fint = 1.
      ftra = 1.
      ftem = 0.
      fver = 1.

      read(nip,'(11x,i5,4x,i3)',end=9000) inum,ispk
      read(nip,*)
      read(nip,*) flaech

	read(nip,'(a)') dummyinp

      dummyinp(1:)=dummyinp(2:)
      call charal(dummyinp,name,ilname)
      faktn=1.
      if(pns.eq.'s'.or.pns.eq.'s'.or.ilname(2).eq.0) then
         namfn(1:maxchar)=pathn(1:ilend)//'klima.dat'//slash//name(1)(1:ilname(1))
      elseif (isim.eq.1) then
         namfn(1:maxchar)=pathn(1:ilend)//'klima.dat'//slash//name(2)(1:ilname(2))
          elseif (isim.eq.0) then
         namfn(1:maxchar)=pathn(1:ilend)//'klima.dat'//slash//name(1)(1:ilname(1))
      endif
      if(abs(ilname(3)).gt.0.00001) then
              cizs = name(3)
              call eintrans(cizs,faktn,ierr)
              if(ierr.gt.0)goto 9995
       endif

      read(nip,'(a)')dummyinp
      call charal(dummyinp,name,ilname)
      if (ilname(1).gt.0) then
      namft(1:maxchar)=pathn(1:ilend)//'klima.dat'//slash//name(1)(1:ilname(1))
      else
      namft='                                                     '
      endif
	
      if (ilname(2).gt.0.and.name(2)(1:10).eq.'nasimblock') then
	 namept(1:10)=name(2)(1:10)
      elseif (ilname(2).gt.0.and.name(2)(1:4).ne.'kein') then
      namept(1:maxchar)=pathn(1:ilend)//'klima.dat'//slash//name(2)(1:ilname(2))
      else
      namept=' '
      endif

      read(nip,'(a)')dummyinp
      call charal(dummyinp,name,ilname)
      if (ilname(1).gt.0) then
      namfzf(1:80)=pathn(1:ilend)//'inp.dat'//slash//name(1)(1:ilname(1))
      else
      namfzf='                                                     '
      endif
	if(ispk.eq.7.or.ispk.eq.8) then
		read(nip,'(a)')namhydro
		if(namhydro(1:4).eq.'kein') then
			ispk=9
		else
		namhydro(1:80)=pathn(1:ilend)//'inp.dat'//slash//namhydro
		endif
	endif
	read(nip,'(a)')dummyinp
!	i1=int(dummyinp(1:1))
	i1=ichar(dummyinp(1:1))
	if(i1.gt.65.and.i1.lt.122) then
		read(dummyinp(1:15),'(a)')snowtyp
		read(dummyinp(16:80),*)ftem,fver
		call inp_snow(pathn,ilend,snowtyp,                      &
      			xwwo,xwwmax,xsnotem,xsnorad,xh0,slash)
		wwo = xwwo
		wwmax = xwwmax
		snotem = xsnotem
		snorad = xsnorad
		h0 = xh0
	else
		read(dummyinp(1:60),140)                               &
      			 wwo,wwmax,snotem,snorad,h0,ftem,fver
	endif

      if (fver.eq.0.) fver=1.
	if(ispk.eq.7.or.ispk.eq.9) then
		read(nip,1501)vsg,anzlayy,bimax,bianf,izkn_vers,tint,rintmx
1501  format(f5.3,i5,2(5x,f5.1),i5,2(5x,f5.1))
		do 1505 i=1,anzlayy
			read(nip,*)                                     &
                       cinh(i),cind(i),cex(i),bmax(i),banf(i),fko(i),  &
                       retlay(i),evalay(i)
1505		continue
		do 1506 i=anzlayy+1,maxlay
			cinh(i)=cinh(anzlayy)
			cind(i)=cind(anzlayy)
			cex(i) =cex(anzlayy)
			bmax(i)=bmax(anzlayy)
			banf(i)=banf(anzlayy)
			fko(i) =fko(anzlayy)
			retlay(i)=retlay(anzlayy)
			evalay(i)=evalay(anzlayy)
1506		continue
	else
		anzlayy=1
		read(nip,150) vsg,cinh(1),cind(1),cex(1),                &
      			bmax(1),banf(1),fko(1),tint,rintmx,bimax,bianf
	endif
	if(ispk.eq.7.or.ispk.eq.9) then
		read(nip,*) f_eva,aint,aigw,fint,ftra
	else
		read(nip,160) f_eva,aint,aigw,fint,ftra
	endif
       if (fint.eq.0.) fint=1.
       if (ftra.eq.0.) ftra=1.

!c     read(nip,*) retvs,retob,retint,retbas,retgw,retklu 
       

      if (ispk.eq.1 .or. ispk.eq.4 .or. ispk.eq.6.or.                 &
      		ispk.eq.7.or. ispk.eq.9) then
!c    einlesen der retentionskonstanten  
!c    retention mit zeitflaechenfunktion
         read(nip,*) retvs,retob,retint,retbas,retgw,retklu 
         else if (ispk.eq.2) then 
!c    zwei parallelspeicher nach wackermann
         read(nip,*) retvs,ohf,vd,retbas,retgw,retklu 

         else if (ispk.eq.3) then
!c    3 parallele linearspeicher mit vorgegebenen retentionskonstanten
         read(nip,*) retvs,retob,retint,retint2,nspk1,nspk2           &
                  ,nspk3,beta1,beta2,retbas,retgw,retklu

         else if (ispk.eq.5) then
!c    scs verfahren, cn-wert = retvs, abflussverzoegerung:
!c         3 parallele linearspeicher mit vorgegebenen retentionskonstanten
         read(nip,*) retvs,ohf,retob,retint,retint2,nspk1,nspk2       &
                  ,nspk3,beta1,beta2

         else
!c    altes format (alter als 5.91) wird weiter eingelesen 
      read(nip,*) retvs,retob,retint,retbas,retgw,retklu,ohf,vd
         endif 

!c     write (nres,'(i4,x,4(f8.2,x)3(i5,x),2(f4.2,x))') ispk,      
!c    + retvs,retob,retint,retint2,nspk1,nspk2,nspk3,beta1,beta2
	if(ispk.eq.7) then
		retbas=1./retbas
		retgw=1./retgw
	endif
      read(nip,'(i4)') igwzu
       qgwzufak=0.
	if(igwzu.eq.9.and.(ispk.eq.7.or.ispk.eq.8.or.ispk.eq.9))then
		igwzu = 0
	endif
	if (igwzu.gt.0) then
	   if(igwzu.gt.igwdim)then
       write(nerr,'(a)')'dimensionierung gw-speicher nicht ausreichend.'
       call writeLogIntInt(7,'Max. Anzahl der Grundwasserrelationen eines Gebietes in Nachbargebiete ueberschritten.',&
            &'Max. nuber of groundwater relationships from a catchment to others reached!', &
            & '','Gebiet',inum,'igwdim',igwdim,'input')

       write(nerr,'(a)')'igwdim musz sein = ',igwzu
                call writeFooter()
		stop 1000
	   endif

	read(nip,'(a)')dummyinp
	read(dummyinp,*) (ngwzu(i),i=1,igwzu)
	read(nip,'(a)')dummyinp
	read(dummyinp,*) (gwwi(i),i=1,igwzu)
          if(ngwzu(igwzu).eq.-1) then
              qgwzufak = gwwi(igwzu)
              igwzu=igwzu-1
          endif
          if(ngwzu(igwzu).eq.-2) then
              qgwzufak = gwwi(igwzu)/24.
              igwzu=igwzu-1
          endif
      end if
	if(ispk.eq.7.or.ispk.eq.8.or.ispk.eq.9) then
!cp		read(nip,*) hgru,hgro,gwsent,pors,rtr,klupor,izkn
!cp		klupor=1.-klupor
      		read(nip,*) hgru,hgro,rtr,pors,gwsent,klupor,izkn
!                    pors= pors/100.
!cp		if(ispk.eq.7.or.ispk.eq.8.)aigw=aigw*pors
	else
      read(nip,'(4(f8.2),i4,f6.2,i8)') hgru,hgro,gwsent,pors,itr,      &
                                      klupor,izkn
         rtr = float(itr)/100.
	endif
      read(nip,*)

      if (inum.ne.ntg) goto 111
	if(ispk.eq.9) then
		ispk = izkn_vers
		izkn_vers = 0
	endif

      close (nip)

!c  die einzugsgebietsflaeche wurde in m**2 eingelesen, wird jedoch in 
!c  in der weiteren berechnung in km**2 benoetigt, die abschlagsrate ins
!c  tiefengrundwasser [%] wird in einen real-wert umgerechnet

	flaech = flaech/1.e+06


      if (pns.ne.'s'.and.faktn.lt.0.0001) faktn = 1.0

      write(nres,4687) namfn(1:60)
4687  format(/' filename der niederschlageswerte: ',a)

!c  gemaesz nw-norm (vgl. nasim) sind cinh, cind und cex in der einheit
!c  [1/h] angegeben. fuer die weitere berechnung werden sie in [mm/h]
!c  benoetige und sind mit bmax zu multiplizieren

!c	do 1506 i=1,anzlayy
!c		cinh(i) = cinh(i) * bmax(i)
!c		cind(i) = cind(i) * bmax(i)
!c		cex(i)  = cex(i)  * bmax(i)
!c1506	continue

!c    zuweisung bereits berechneter 'anfangswerte' bei mehreren zyklus-
!c    durchlaeufen oder bei kurzzeitsimulationen
!c    (pro zyklus-lauf gelten neue startbedingungen bei den
!c     berechnungen der schneehoehe, der bodenfeuchte, der
!c     interzeption und des interflow)
!c    imax wird im hauptprogramm (mit jdim=100) initialisiert

      if(pns .ne. 's'.and.pns.ne.'s') then
        if (iz.gt.1.or.(isim.eq.1.and.idatsa.ge.idat)) then
           do 1000 i=1,imax
                 if(nr(i).eq.ntg) goto 1111
1000       continue

1111       if (bo1(1,i) .ne. 0.)then
           h0    = h01(i)
           w0    = w01(i)
	   do 1112 ilay=1,anzlayy
           if (isim.eq.0) then
             banf(ilay)  = bo1(ilay,i)
           else if (isim.eq.1) then
             banf(ilay)  = bo1(ilay,i)*100./bmax(ilay)
           end if
1112	   continue
           bianf = bianf1(i)
           aint  = aint1(i)
           aigw  = aigw1(i)
           endif
        endif
       else

      write (nres,'(a)') 'anfangswerte   '                          
      write (nres,'(a,f7.3)')										&
         'schneehoehe                    h01 =',h0
      write (nres,'(a,f7.3)')                                       &
         'wassergeh. schnee              w01 =',w0
	do 1113 ilay=1,maxlay
      write (nres,'(a,f7.3)')                                       &
         'bodenfeuchts                   banf =',banf(ilay)
1113	continue
      write (nres,'(a,f7.3)')                                      &
         'inhalt interzeptionsspeicher   bianf1 =',bianf
      write (nres,'(a,f7.3)')                                      &
         'anfangsintensitaet interflow   aint1 =',aint
      write (nres,'(a,f7.3,//)') 'grundwasserstand aigw1 =',aigw
      endif


!c oeffnen und einlesen des datenfiles mit der
!c   z e i t f l a e c h e n f u n k t i o n

      if (ispk.ne.1.and.ispk.ne.0.and.ispk.ne.6.and.             &
     		ispk.ne.7.and.ispk.ne.8) goto 225

       il = ju0nch(namfzf)
       if(ispk.eq.0.and.il.lt.2) goto 225

	call inp_zft(ntg,namfzf,il,dt,ispk,         &
     			idif,fn,ianzzft,pns)
!c	if(ispk.eq.7) then
!c		namfzf(il-2:il)='zfp'
!c		call inp_zft(ntg,namfzf,il,dt,ispk,
!c    *			idif,fperk,ianzzfp,pns,nres)
!c	endif


!c  oeffnen des niederschlag-datenfiles und einlesen


225   continue 


!c niederschlag wird nur dann eingelesen, wenn niederschlagsstation
!c   fuer neues teilgebiet nicht mit alter station uebereinstimmt

          write(nres,'(a,f8.2)')' korrekturfaktor fuer niederschl.:',faktn

      izaehl = 0
      ipaja = 0
      ipamo = 0
      ipatg = 0
      ipast = 0
     call u1null(peff,idim)
   	call u1null(p,idim)
      namnal = namfn


!c  nicht benoetigte zeilen des datenfiles werden in der variable
!c  dummy "ueberlesen"
!c  das 333-statement umfaszt einen jahresdatenblock (die n-werte
!c  werden jahresweise in p ueberschrieben, wobei in p2 die zur n-a-
!c  simulation gehoerigen n-daten einschlieszlich vor- und nachlaufendem
!c  n-zeitschritt gespeichert werden

   if(isim .eq. 0) then


      if(pns.eq.'s'.or.pns.eq.'s')then
            write(nerr,'(a)')'xxx fehlerhafte definition der ',      &
            'niederschlagsdatei!!'
            call writeLogString(7,'Fehlerhafte Definition! Langzeitsimulation mit synth. Niederschlag nicht moeglich!',&
            &'Incorrect definition! Long-term simulation with synth. precipitation not possible!','','','','','','input')

            write(nerr,'(a)')'xxx langzeitsimulation mit synthet.'   &
            ,'niederschlag nicht moeglich.'
            call writeFooter()
            stop 1000
      endif

	call inp_nied_lz(namfn,dt,                                &
               naja20,namo20,natg20,nast,namn,                  &
               neja20,nemo20,netg20,nest,nemn,                   &
               pint,izaehl,ierr)
	
	if(idif.gt.izaehl.or.ierr.gt.0) then
	  write(nerr,6201) idif,izaehl
          call writeLogIntIntInt(7, 'Fehler beim Einlesen der Langzeitniederschlaege! Evtl. Simulationszeitraum nicht abgedeckt.',&
                & 'Error when reading the precipitation for long term!','','',0,'Simulationszeitschritte (idif)',idif, &
		& 'Niederschlagsdaten (izaehl)',izaehl,'input')
                call writeFooter()
		stop
	endif

6201	format ('xxx fehler beim einlesen der niederschlaege/langzeit',/ &
       'xxx anzahl zeitschritte in simulation: ', i5,/                  &
       'xxx anzahl tageswerte aus datenfile:   ', i5)

	do 3401 i=1,izaehl
		peff(i)=(pint(i)*faktn)/dt
3401	continue
	goto 6200

!ccccccccccccccc
!c
!c kurzzeitsimulation
!c
!c  aufschluesselung des datums zwecks feststellung der stundendifferenz
!c  zwischen dem anfangsdatum der n-werte und dem beginn der n-a-simulation
      else
   32      if(pns.eq.'s')then
  !SK Einlesen der synth. N je Teilgebiet
                call checkn(pathn,ilend,0,namfn,xjah,xwahl2,		&
    				slash)

                  call psynt(dt,idif,peff,faktn)
                  goto 6200
           endif
        kzif = 1
		call inp_timeseries_kz(namfn,dt,faktn,naja20,namo20,natg20,nast,namn,                  &
               neja20,nemo20,netg20,nest,nemn,peff,idife,kzif)
		goto 6200

	nfil = ju0gfu()
	open (nfil,iostat=ierr,err=999,file=namfn,form='formatted',    &
            status='old')

           itkz = 0
           
           ja1   = naja
           mo1   = namo
           tg1   = natg
           st1   = nast
           idat1 = idata

3334         read(nfil,'(5x,a8,i2)',end=9100) dummy,lfnr
           if(lfnr .ne. 1) goto 32
           if(dummy(1:8) .ne. ' 0 0   0') goto 32

           read(nfil,3325)dummy,lfnr,ivb,ipot,itka,imka,ijka,iska,itke,&
              imke,ijke,iske,ikom
 3325      FORMAT(a13,i2,5x,i5,i5,2i2,2x,2i2,4x,2i2,2x,2i2,4x,i5)
           do 31 i = 1,ikom
               read(nfil,'(a)')dummy
   31      continue
           idatak = ijka*10**6 + imka*10**4 + itka*10**2 + iska
!ccccccc
!c fuer agger
           if(ijke.eq.92.and.imke.eq.12.and.itke.eq.31) ijke=99
           idatek = ijke*10**6 + imke*10**4 + itke*10**2 + iske
           if(idatak .gt. idat1 .or. idatek .lt. idate) goto 32
           if(dt.lt.ivb/60.)then
               datc=ivb/60.
               write(nerr,'(a)')'xxx simulationsintervall dt =',dt,'h'
               write(nerr,'(a)')'xxx kleiner als zeitintervall =',datc,'h'
               write(nerr,'(a)')'xxx der niederschlagsdaten fuer die'
               write(nerr,'(a)')'xxx kurzzeitsimulation!'    
               write(nerr,'(/a)')'xxx waehle groeszeres simulations'   &
                    ,'intervall dt.'

               call writeLogIntRealReal(7,'Simulationsintervall ist kleiner als Zeitintervall der Niederschlagsdaten!',&
                    & 'Simulation interval smaller than time interval in precipitation data!', &
                    & '','',0,'Simulationsintervall (dt)',dt,'Datenzeitintervall (datc)',datc,'input')

               write(nerr,'(a)')'xxx programmabbruch.'
               call writeFooter()
               stop 'input 443'
           endif




38   	   read(nfil,'(4x,1x,2(i2),2x,2i2,4x,a1,12i5)',end=9102) itgn,mon,jan,istk,kennz,(ip(i),i=1,12)

           idatak = jan*10**6 + mon*10**4 + itgn*10**2 + istk

           if(kennz.eq.'e') goto 3334
           if(idatak .lt. idata) goto 38

           if(idatak.gt.0) goto 3337
9102       if (itkz.gt.10) then
                idatak=21*10**6
                goto 3337
           else 
               goto 9100
           endif

!cccccccccccccccccccccccccc
!c
!c  bei zeitsprung im einlesefile muess fehlender zeitraum mit 0. aufgefullt
!c                 werden

3337      if(idatak.gt.idat1) then
                do 3336 i= itkz+1,itkz+12
                      p2(i) = 0.
3336            continue
!c               yrint*,' aufgefuellt: ',tg1,mo1,ja1,st1,itkz
                itkz = itkz+12
                if((itkz-12).gt.idimp) then
                     write(nres,3366) idimp
                    izmax=itkz
                    idatpa=idata
                    goto 444
                endif

                st1=st1+1
                if(st1.eq.24) then
                    st1=0
                    call cdatum(tg1,mo1,ja1,1)
                endif
                idat1 = ja1*10**6 + mo1*10**4 + tg1*10**2 + st1
                if(idat1.gt.idate)then
                    izmax=itkz
                    idatpa=idata
                    goto 444
                endif
                goto 3337
           endif

                do 3339 i= 1,12
                      if(ip(i).lt.0) ip(i) = 0
                      p2(itkz+i) = float(ip(i))*10.**(ipot)*faktn
                      psum=psum+p2(itkz+1)
3339              continue
!c          yrint*,' eingelesen: ',itgn,mon,jan,istk,itkz,psum
                itkz = itkz+12
                if((itkz-12).gt.idimp) then
                     write(nres,3366) idimp
                    izmax=itkz
                    idatpa=idata
                    goto 444
                endif
                st1=st1+1
                if(st1.eq.24) then
                    st1=0
                    call cdatum(tg1,mo1,ja1,1)
                endif
                idat1 = ja1*10**6 + mo1*10**4 + tg1*10**2 + st1
                if(idat1.gt.idate)then
                    izmax=itkz
                    idatpa=idata
                    goto 444
                endif
                goto 38
      endif


444       if (iz.gt.1) then
                      najs = idata/10**6
                      namo = 01
                      natg = 01
                      nast = 0
          else
                 najs = naja
          end if
      if(idatpa.le.0)then
	write(nerr,10002)ntg,idata,idate,namfn,idatak
    call writeLogIntInt(7,'Fehler bei der Zuordnung der Niederschlaege!',&
                    & 'Error in precipitation data!',namfn,'Teilgebiet',ntg,'idatak',idatak,'input')

10002	format('xxx fehler bei d.zuordnung der niederschlaege',/        &
               'xxx im teilgebiet ',i5,/                                &
               'xxx gesuchtes niederschlagsereignis',/                  &
               i8,'  -  ',i8,/                                          &
               'xxx in der datei ',a,/' nicht gefunden.',/              &
               'xxx moegliche fehler: ereignis nicht definiert.',/      &
               'xxx                   chronologie der n-ereignisse',/   &
               'xxx                   nicht eingehalten.',/             &
               'xxx letztes in der datei selektiertes datum:',i8,/      &
               'xxx programmabbruch.')
         call writeFooter()
         stop 'input 444'
       endif 
          ipaja = int(idatpa/10**6)
          ipamo = int((idatpa-ipaja*1000000)/10000)
          ipatg = int((idatpa-ipaja*1000000-ipamo*10000)/100)
          ipast = int(idatpa-ipaja*1000000-ipamo*10000-ipatg*100)
          izdif = ntdif(ipatg,ipamo,ipaja,natg,namo,najs) * 24
          zdif  = float(izdif - ipast) + nast
!c  interpolation der n-werte auf den zeitschritt der n-a-simulation

      datc = 0
      datuma = 0
      datume = 0

!c         yrint*,'  izmax,idif: ', izmax,idif

               if (isim .eq. 0) then
                   v1=dt/izs
                   i=int(v1+.1)
                   if ((v1-i).ge.10.**(-3)) goto 9200 
               endif
               do 40 i=1,idif 
!c                   datca = i*dt + zdif 
!c                   datce = (i+1)*dt + zdif 
                    datca = (i-1)*dt + zdif 
                    datce = (i)*dt + zdif 
                    do 41 i2=1,izmax
                      if(isim .eq. 0) then
!ccccccc langzeit
                         datuma = float((i2-1)*izs)+10.**(-3)
                       if (datuma.ge.datca.and.datuma.lt.datce) then
                            peff(i)=peff(i)+p2(i2)/dt
                            elseif (datuma.gt.datce) then  
                               goto 40 
                         end if  
!ccccccc kurzzeit
                      else
                         datuma = float((i2-1)*ivb)/60.
                         datume = float((i2)*ivb)/60.
                       if (datca.ge.datuma.and.datca.lt.datume) then
                           peff(i)=(datume-datca)/(datume-datuma)*p2   &
                                    (i2)/dt
                            do 45 i3 = i2+1,izmax
                              datuma = float((i3-1)*ivb)/60.
                              datume = float((i3)*ivb)/60.
                              if(datume .le. datce)then
                                    peff(i) = peff(i) + p2(i3)/dt
                               elseif(datuma.lt. datce .and. datume .ge.datce) then
                                    peff(i) = peff(i) + (datce-datuma)/   &
                                     (datume-datuma)*p2(i3-1)/dt
                                    goto 40  
                              endif
   45                       continue
                         endif
                       end if

41                  continue
40             continue

      close (nfil)

!c uebertragen der peff-werte in die datei  p . d a t

6200	continue

      if(npin.gt.0) then
         do 1010 i2 = 1,ingmax
            if (ntg .eq. ntgout(i2)) then
               write(npin,'(/3(i8)/)') ntg,iz,idif
               write(npin,'(10(f6.2,1x))') (peff(i)*dt,i=1,idif)
            endif
 1010    continue
      endif
      sump=0
      do 1011 i2 = 1,idif
              sump=sump+peff(i2)
 1011    continue
      sump=sump*dt
      write(nres,1012) sump
 1012 format(/,' gesamtmenge an niederschlag (mm): ',f10.1)



!ccccccccccccccccccccccccccccccccccccccc
!c
!c
!c oeffnen und einlesen des datenfiles mit den t e m p e r a t u r -
!c und den v e r d u n s t u n g s - werten
!c interpolation auf den zeitschritt der n-a-simulation

   continue  

      if(pns .eq. 's')then
            do 5500 i=1,idif
			tmeans(i) = 24.
			vs(i)     = 0.
 5500       continue
            goto 777
      endif

	if(namept(1:1).ne.' ') then
	
  	    if(namft.ne.namfal.or.abs(ftem-ftema).gt.1e-4)then

		call inp_nied_tv(namft,                              &
                       naja20,namo20,natg20,                         &
                       neja20,nemo20,netg20,                         &
                       pint,izaehl,ierr)
				if(ierr.gt.0) then
  				   write(nerr,9202)namft
9202	                           format('xxx fehler beim einlesen von temperaturwerten. ',/	&
               			          'xxx datenfile: ', a)

                                   call writeLogString(7,'Fehler beim Einlesen von Temperaturdaten!', &
                                        & 'Error when reading temperature data!',namft,'','','','','input')
                                   call writeFooter()
				   stop
				endif

		if(dt.lt.23.5) then
			xstart=real(nast)
			tend=ntdif(natg20,namo20,naja20,				&
      				netg20,nemo20,neja20)                
			xend=real(tend*24+nest-nast)
			do 6441 i=1,izaehl
				vek1(i)=(i-1)*24.+12
6441			continue
			call n_approx(dt,xstart,xend,izaehl,					&
                               vek1,pint,0,idife,tmeans,2)  
			if(abs(ftem-1.).gt.0.001) then
			    do 6442 i=1,izaehl
				tmeans(i)=(tmeans(i)+ftem)
6442			    continue
			endif
		else
			if(idif.gt.izaehl.or.ierr.gt.0) then
			    write(nerr,6301) idif,izaehl
                            call writeLogIntIntInt(7,'Fehler beim Einlesen der Temperaturdaten Langzeit!',&
                            & 'Error when reading the temperature long-term!','','',0,'idif',idif,'izaehl',izaehl,'input')
                            call writeFooter()
                            stop
			endif
	
			do 6401 i=1,izaehl
			    tmeans(i)=(pint(i)+ftem)
6401			continue
		endif
		namfal=namft
		ftema=ftem
	    endif

  	    if(namept.ne.nameptal.or.abs(fver-fvera).gt.1e-4)then

	      
		call inp_nied_tv(namept,                          &
                       naja20,namo20,natg20,                      &
                       neja20,nemo20,netg20,                      &
                       pint,izaehl,ierr)
		if(dt.lt.23.5) then
			xstart=real(nast)
			tend=ntdif(natg20,namo20,naja20,			&
      				netg20,nemo20,neja20)             
			xend=real(tend*24+nest-nast)
			xsum1=0.
			xsum2=0.
			do 6445 i=1,izaehl
				vek1(i)=(i-1)*24.+12
				xsum2=xsum2+pint(i)
6445			continue
			call n_approx(dt,xstart,xend,izaehl,		&
      				vek1,pint,0,idife,vs,1)      
			do 6446 i=1,idife
				vs(i)=(vs(i)*fver)/dt
				xsum1=xsum1+vs(i)
6446			continue
			print*,' verdunst.',xsum2,xsum1*dt
		else
			if(idif.gt.izaehl.or.ierr.gt.0) then

			  write(nerr,6301) idif,izaehl
                          call writeLogIntIntInt(7, 'Fehler beim Einlesen der Temperaturdaten Langzeit!', &
                               & 'Error when reading the temperature data long-term','','',0,'idif',idif,'izaehl',izaehl,'input')
                          call writeFooter()
			  stop
			endif

			do 6402 i=1,izaehl
				vs(i)=(pint(i)*fver)/dt
6402			continue
		endif

		nameptal=namept
		fvera=fver
	   endif
	else


6301	format ('xxx fehler beim einlesen der temperatur/langzeit',/     &
       'xxx anzahl zeitschritte in symulation: ', i5,/                   &
       'xxx anzahl tageswerte aus datenfile:   ', i5)


!c	goto 6399

!cccccccccccc
!c alter einleseblock

      if(namft.eq.namfal .and. abs(ftem-ftema).lt.1e-4				&
         .and. abs(fver-fvera).lt.1e-4) goto 777

      namfal = namft
      ftema = ftem
      fvera = fver

	call u1null(p2,idim)
	call u1null(p,idim)

	ntv = ju0gfu()
      open (ntv,iostat=ierr,err=99,file=namft,status='old')

!c der zeitschritt izs des t-v-datenfiles sei 24 h:
      izs = 24
      izaehl = 0
      izmax = 0
      ianz = 0
      datc = 0
      datuma = 0
      datume = 0
!c SK 04.03.03
!c Abbruchkriterium für Kurzzeitsimulation: Werte werden eingelesen bis notwenndige
!c Anzahl an zeitschritten erreicht ist.
!c      if(isim.eq.1) datume=int((dt*460.)/(izs))+1
     if(isim.eq.1) datume=int((idif)/(izs))+1
555   read(ntv,'(a)',end=2002) dummy
!c SK/EP 28.02.03
!c Kopf der TV-Datei wird nur die Anzahl an Werten pro Jahresblock eingelesen.
      read(ntv,600) ianz
      read(ntv,'(a)') dummy
      do 50 i=1,ianz
	     read(ntv,606) jatv(i),motv(i),itgtv(i),vs(i),tmeans(i)
           idatum(i) = jatv(i)*10**4 + motv(i)*10**2 + itgtv(i)

           if ((izaehl.gt.datume).and.(isim.eq.1)) goto 666

           IF(izaehl.eq.0 .AND. idatum(i).ne.idatsa) then
!          Suchen der Zeile in der Klimadatei, deren Datum mit dem Anfangsdatum
!          des aktuellen Zyklus übereinstimmt (IDATSA)

                GOTO 50
           elseif ((idatum(i).gt.idatse).and.(isim.ne.1)) then
                         izaehl         = izaehl+1
				 p2(izaehl)     = vs(i)
				 p(izaehl) = tmeans(i)
                         goto 666
           else if(idatum(i).ge.idatsa) then
                         izaehl         = izaehl+1
				 p2(izaehl)     = vs(i)
				 p(izaehl) = tmeans(i)
           if (idif.eq.izaehl.AND.(isim.ne.1)) GOTO 666
!c SK 04.03.03
!c Für Kurzzeitsimulation werden in dieser Schleife die Werte für Temp/Verd eingelesen
           else IF (isim.eq.1 .and. izaehl.gt.0) then
                         izaehl         = izaehl+1
      				 p2(izaehl)     = vs(i)
      				 p(izaehl) = tmeans(i)
!c  SK/EP 28.02.03
!       Jahr 2000 Fähigkeit
!       Beim Übergang von 99 auf (20)00 erkennt das Programm nicht,
!       daß Jahr 2000 sondern 00. In diesem Fall wird idatum(i) nie größer idatse
!       Das Endkriterium wird aber auch erreicht, wenn bereits die notwendige Anzahl
!       Tageswerte an Klimadaten bereits gelesen wurde, dh. idif=izaehl.
!       Somit kann Lesevorgang in diesem Fall vorzeitig beendet werden.
      !  if (idif.eq.izaehl.AND.(isim.ne.1)) GOTO 666
      !   if ((idatum(ianz).ge.idatse).and.(isim.eq.1)) goto 666
      !   if ((izaehl.ge.ianz).and.(isim.eq.1)) goto 666
            END if
        
50     continue
       goto 555

666    izmax = izaehl
       call u1null(tmeans,idim)
	call u1null(vs,idim)

	if (int(dt) .ne. izs) then
           i3=idim
           if(isim.eq.0) i3=izmax
           do 60 i=1,i3
                datc  = i*dt + nast
                do 61 i2=2,izmax
                         datuma = float((i2-1)*izs) - izs
                         datume = float(i2*izs) -izs
                         if (datc.ge.datuma.and.datc.lt.datume) then
                            tmeans(i)=(((datc-datuma)/(datume-datuma)*(   &
                                      p(i2)-p(i2-1)))+                    &
                                      p(i2-1))+ftem
				    vs(i) = (p2(i2-1)/izs)*fver
                            goto 60
                         end if
61             continue
60         continue
      else
           do 70 i=1,izmax
			  vs(i)     = (p2(i)/izs)*fver
			  tmeans(i) = p(i)+ftem
70         continue
      end if
!c     write(nres,'(/'' korrekturwert der temperatur : '',f6.2/)')ftem
!c     write(nres,'('' faktor fuer verdunstungskorrektur: '',f6.2)')fver
!c     write(nres,'('' korrekturfaktor fuer interz.kap.-winter: '',
!c    + f6.2)') fint
!c     write(nres,'('' korrekturfaktor fuer f_eva-winter: '',f6.2)')ftra
      close (ntv)

	endif
!c6399  	continue

!c    tanf gibt anzahl der tage zum 1.1. an
      if (iz.gt.1) then
         tanf=0
      else
         tanf=ntdif(1,1,naja,natg,namo,naja)
      endif
777   return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
140   format(4(f6.2,3x),f6.0,2(3x,f6.2))
150   format(f5.3,3x,2(f7.5,1x),f8.5,1x,4(f5.0,3x),3(f5.1,3x))
160   format(f4.1,3x,f4.1,2x,f6.2,2x,f4.2,3x,f4.2)

600   format(20x,i3)
606   format(3(1x,i2),2(1x,f6.2))

2002  write(nerr,'(''idatum(ianz): '',i10)')idatum(i)
      call writeLogIntInt(5,'Der Simulationszeitraum wird nicht durch Verdunstungsdaten abgedeckt!',&
           & 'Simulation period is not covered by evaporation data!',namft,'',0,'',0,'input')

      write(nerr,'(''idatse: '',i10)')idatse


!    if ((idatum(ianz).ge.idatse).and.(isim.ne.1)) goto 666
  
      write(nerr,                                                    &
      '('' enddatum t-v-datenfiles < enddatum na-simulation!'')')
      call writeFooter()
      stop 'input 446'
9000  write(nerr,9656) ntg
9656  format(' fehler in der subroutine input ! gebiet',i5,          &
      ' wurde nicht gefunden.')
      call writeLogIntInt(7,'Das Gebiet wurde in dem Gebietsdatensatz nicht gefunden.','Catchment has been not found.','',&
      & 'Teilgebietnummer', ntg,'',0,'input' )

      close (nip)
      call writeFooter()
      stop 'input 447'

9100  write(nerr,9101)idatak,namfn
9101  format(/'fuer das simulationsintervall existieren keine ',     &
             'niederschlagsdaten!!'/' abbruch der berechnung.'       &
      /,'letzter eingelesener niederschlagswert: ',i10,              &
      /,'niederschlagseingabefile:               ',a )
      call writeLogString(7,'Fuer das simulationsintervall existieren keine Niederschlagsdaten!',&
      &'There is no precipitation data for the simulation period',namfn,'','','','','input')

      call writeFooter()
      stop 'input 448'

9200  write(nerr,9201)
      call writeLogString(7,'Ueberpruefe zeitintervall! Intervall ist kein Vielfaches des Niederschlagsintervalles!',&
      &'Check time interval! Intervall is no multiple of precipitation interval!',namfn,'','','','','input')

9201  format(/' ueberpruefe zeitintervall !!',/,' dt kein vielfac',   &
        'hes des niederschlagsintervall! abbruch der berechnung.')
      call writeFooter()
      stop

99    dummy ='fehler beim oeffnen von '//namft
       write (nerr,9001) namft(1:ilen),'  (famft)','klima.dat'
      call writeLogString(7,'Datei konnte nicht geoeffnet werden.','File could not be opened.',namft(1:ilen),'','','','','input')

	call u0erpr(0,ierr,dummy,LEN_TRIM(dummy),'input 98',              &
        	   LEN_TRIM('input 98'))
       call writeFooter()
       stop 'input 98'

999   dummy='fehler beim oeffnen von '//namfn
       write (nerr,9001) namfn(1:ilen),'  (namfn)','klima.dat'
     call writeLogString(7,'Datei konnte nicht geoeffnet werden','File could not be opened',namfn(1:ilen),'','','','','input')

	call u0erpr(0,ierr,dummy,LEN_TRIM(dummy),'input 97',              &
        	   LEN_TRIM('input 97'))
       call writeFooter()
       stop 'input 97'

9999  dummy ='fehler beim oeffnen von '//namezg
       write (nerr,9001) namezg(1:ilen),'  (namezg)','inp.dat'
     call writeLogString(7,'Datei konnte nicht geoeffnet werden','File could not be opened',namezg(1:ilen),'','','','','input')

	call u0erpr(0,ierr,dummy,LEN_TRIM(dummy),'input 30',              &
        	   LEN_TRIM('input 30'))
       call writeFooter()
       stop 'input 30'

9995  dummy ='fehler beim einlesen von faktn in gebiet '
       write (nerr,'(a,i5)')                                         &
        'fehler beim einlesen von faktn in gebiet ',inum
     call writeLogIntReal(7,'Fehler beim Einlesen der Gebietdaten (Faktor Niederschlag).',&
          &'Error while reading catchment data (correction factor precipitation)',&
          &'','Teilgebiet',inum,'faktn',faktn,'input')

	call u0erpr(0,ierr,dummy,LEN_TRIM(dummy),'input 95',              &
        	   LEN_TRIM('input 95'))
       call writeFooter()
       stop 'input 95'

9001  format(' datei mit filename ',a,a/,' konnte nicht ',             &
       'geoeffnet werden. '/' ueberpruefe datei im directory ../',a)

3366  format(' fuer maximal ', i5,' stunden koennen niederschlaege ',  &
      'eingelesen werden.'/,                                           &
     ' restlicher zeitraum bleibt unberuecksichtigt',                  &
     ' gegebenenfalls parameter idimp in input.for erhoehen.')
      end
