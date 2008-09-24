!     Last change:  JH   17 Jan 2007    8:12 am

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

      subroutine sp_geisel(izyk,iko,iku,dt,idif,izsim,issp,    &
      			qz,nsv,nueb,nsv_e,nsv_p,nsv_b,nsh,     &
      			natg1,namo1,naja1,netg1,nemo1,neja1,   &
      			nast1,namn1,nest1,nemn1,               &
                        svmm,svend,ivor,                &
                        slash)


!**************************************ÄNDERUNGEN***********************************************
!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!   22.05.2002  Claudia Brüning         vorherige Version berechnete unsinnige Werte

!   04.07.2002  Alexander Goetze!AG     Vorherige Version konnte keine Werte in die LZSIM-
!					Dateien schreiben. Es war zwar eine Schleife dafuer
!					vorhanden, allerdings nicht kompilierbar. Die Schleife
!					wurde wieder hereingenommen in die Routine und die
!					Syntaxen so abgeaendert, dass die Routine kompilierbar
!					wurde. In der vorigen Version wurden auch keine Daten in
!					die Ausgabedateien für den Speicherinhalt, sowie für den
!					Überlauf geschrieben. Dies wurde in der neuen Version
!					eingefügt.

!***************************************BESCHREIBUNG********************************************
!***************************************EIN-/AUSGABE********************************************
!c    speichersimulation fuer projekte agger;
!c    --------------------------------------------------------------------------
!c    Eingabe:
!c    --------------------------------------------------------------------------
!c    izyk:  anzahl der jahresschleifen bei langzeitsim.
!c    iko:   oberer knoten
!c    iku:   unterer knoten
!c    dt:    zeitschritt, in stunden, dt>23. = langzeitsim.
!c    idif:  anzahl zeitschritte, wird fuer kzs auf izsim erhoeht
!c    izsim: max. zeitschrittanzahl (z.b. 460)
!c    issp:  gerinnenummer des aktuellen speicherelement
!c    qz(i): zuflussganglinie
!c    nsv:   fileflag ausgabe speicherinhalt
!c    nueb:     "       "     ueberlauf

!c    common      isim  mit  0: langzeit-, 1: kurzzeitsimulation
!c                idatsa,natg,namo,naja: datum - beginn der aktuellen schleife
!c    --------------------------------------------------------------------------
!c    Weitere Variablen
!c    --------------------------------------------------------------------------
!c    sv(iz)  = speicherinhalt zum zeitpunkt iz
!c    ueb(iz) = speicherueberlauf zum zeitpunkt iz
!c    qz(iz)  = zufluss zum zeitpunkt iz
!c    qa(iz)  = abfluss zum zeitpunkt iz
!c    iz:  zeitschritte von 1..iend
!c    qent(i) = entnahme im kennlinienbereich i

!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      parameter (idim2=24,idim3=15)

      include 'include/param.cmn'
      include 'include.spi'
!c    include 'include/q.cmn'
!c    include 'include/rhb2.cmn'
!c    include 'include/knt.cmn'
!c    include 'include/is.cmn'
!c    include 'include/filnm.cmn'
      include 'include/nad.cmn'
      character*5 	kende
      character*16 	nams
      character*80 	ifehl,itext,namrhb
      integer 		insp,inum,lfs,issp,iz,nsv,nueb,ik,iknot,izsim,      &
              		ilen,i,irhb
!cb   220502   zusaetzliche Deklaration von sv_speicher,svdiff
      real fuqz,fuqa,fueb,sqz,sqa,sueb,sv_speicher,svdiff
      dimension qz(1:idim),qa(1:idim),sv(1:idim+1),ueb(idim),     &
          hv(idim2),vs(idim2),qd(idim2)

      real v(idim2,idim3),av(idim3),q1(idim2,idim3),              &
           q2(1:idim2,1:idim3),q4max(idim+1),qent(idim3)
      integer t(idim3),fall(idim2,idim3),jakt,ianf,iend,          &
           nasteig(idim+1),idifa,idife,iztime

      INTEGER maxchar
      parameter (maxchar=120)

        CHARACTER*(maxchar) namfn
      character*80 text0,fnamver
      integer  lt,jja,jmo,jtg,jdif,                          &
               nksim
      real     r,sent,x1
	real	qentx,qver,sumqverp
	real	svmm(12),svend
	integer	anzdm(12)
	character*1	slash
 
      logical  lvor
!AG 040702  Die Boolean-Variable 'lex' wurde zusaetzlich deklariert.
      LOGICAL lex

	integer		naja1,namo1,natg1,nast1,namn1
	integer		neja1,nemo1,netg1,nest1,nemn1
	integer		idif,ierr
	real		dt,epot(idimt),temp(idimt)
	real		epot_o(idimt),p_o(idimt)
	real		p(idimt)
	integer	nsv_e,nsv_p,nsv_b,nsh

       character*140 dummyinp
       character*120 name(3)

       integer ilname(3)

      data hv/idim2*0./,vs/idim2*0./,qd/idim2*0./,           &
            nams/'                '/

!c          initialisieren

      call u1null(q4max,idim+1)
      call u0null(nasteig,idim+1)
      call u1null(qa,idim)
      call u1null(sv,idim+1)
      call u1null(ueb,idim)

      svdiff = 0.
      sv_speicher = 0.
      qver = 0.
      qnied = 0.
!c all2 werte in 1 hqbm = 1000000 qbm


      dtfak = dt * 0.0036
      irhb = 0
      sent = 0.
	sumqverp = 0.
	sumnied = 0.
!c          einlesen speicherdaten

      namrhb(1:80)=pathn(1:ilend)
      namrhb(ilend+1:ilend+8)='inp.dat/'
      ilen = ilend + 8
      namrhb(ilen+1:ilen+ilenb)=namger
      ilen=ilen+ilenb

      namrhb(ilen+1:ilen+4)='.rht'
      ilen = ilen+4

      if(ilen.gt.80)then
         call writeLogIntInt(7,'Pfad hat mehr als 80 Zeichen!','Path has more than 80 characters!',namrhb,&
              & '',0,'ilen',ilen,'sp_geisel')

         write(nerr,'(3(/a))')'tree-name fuer den file',namrhb,   &
                'mehr als 80 zeichen.'

         write(nerr,'(a)')'programm-abbruch.' 
         call writeFooter()
         stop
      endif

      insp = ju0gfu()
      open (insp,iostat=ierr,err=9999,file=namrhb)

14    ikenn=0
      read(insp,'(8x,2(i8),i4,a)',end=9000) inum,iknot,ikenn,dummyinp
      read (insp,'(a80)',end=9000) itext

	call charal(dummyinp,name,ilname)

!ccccccccccccccccccccccccccccccccc
!ceinlesen der seeverdunstung

	if(ilname(1).gt.0) then
		fnamver(1:80)=pathn(1:ilend)//'klima.dat'//slash//  &
      			name(1)(1:ilname(1))
		namfn(1:maxchar)=pathn(1:ilend)//'klima.dat'//slash//    &
      			name(2)(1:ilname(2))
		call inp_epot(fnamver,isim,dt,                    &
      			naja1,namo1,natg1,nast1,namn1,            &
      			neja1,nemo1,netg1,nest1,nemn1,           &
      			epot,temp,idif,ierr)
		call inp_nied_lz(namfn,dt,                         &
      			naja1,namo1,natg1,nast1,namn1,                       &
      			neja1,nemo1,netg1,nest1,nemn1,                      &
      			p,idif,ierr)
	endif

      if(ikenn.eq.0) then
      read (insp,22,end=9000) lfs,nams,sv(1),vmax,vmin,jev

!c          fehlermeldungen

    if (vmax.gt.vmin.and.sv(1).ge.vmin.and.sv(1).le.vmax) goto 30
      ifehl = 'vo/vmax/vmin'

      write (nerr,31) ifehl
      call writeLogIntInt(7,'Fehler der Dateneingabe im Speicherstrang, Hinweis: vmin < Anfangsinhalt < vmax.',&
             & 'Incorrect input in storage channel. Hint: vmin < initial content < vmax.','','Strang',inum,&
             & '',0,'sp_geisel')

      write (nerr,332) inum,vmin,vmax,sv(1)
      call writeFooter()
      stop

34    ifehl = 'vo/vmax/vmin'


33    write (nerr,31) ifehl
       call writeLogIntInt(7,'Fehler der Dateneingabe im Speicherstrang, Hinweis: vmin, vmax entspricht nicht WQV Inhalten.',&
            & 'Incorrect input in storage channel. Hint: vmin, vmax not equal to data in WQV content.','','Strang',inum,&
            & '',0,'sp_geisel')

      write (nerr,331) inum,vmin,vmax,sv(1),jev,sv(jev)
      call writeFooter()
      stop

30    if (jev.lt.24) goto 32
      ifehl = 'jev'
      goto 33
32    do 40 i=1,jev
      read (insp,24,end=9000) hv(i),vs(i),qd(i)
40    continue
      if ((hv(jev)-0.00001).lt.0.) goto 9500
      if ((vs(jev)-0.00001).lt.0.) goto 9510
      if ((qd(jev)+0.00001).lt.0.) goto 9520
      if (qd(jev).eq.0.) then

         write(nres,9531)inum
         call writeLogIntInt(6, 'Speicherabfluss von 0.0 m³/s fuer vollen Speicherinhalt wurde angegeben.',&
              & 'Storage discharge of 0.0 m³/s was read for the total storage content.','','Speicher',inum,'',0,'sp_geisel')

         endif
      if (abs(vs(1)-vmin).gt.0.001) goto 34
      if(abs(vs(jev)-vmax).gt.0.001) goto 34
      read (insp,25,end=114) kende
      goto 14

      else 
          irhb=irhb+1
          if (irhb.gt.jmaxcon2) then
             call writeLogIntInt(7,'Max. Anzahl von Speicherstraengen im Modell ueberschritten.',&
                  & 'Max number of storage channels reached.','','',0,'jmaxcon',jmaxcon2,'sp_geisel')

                write(nerr,141)
                call writeFooter()
                stop
          endif
141   format ('xxx es wurden zu viele speicher im modell integriert.'  &
      /'xxx maximal moegliche speicheranzahl: 5'/                     &
      'xxx erhoehe parameter jmaxcon2 und felddim. in rhb.cmn'/        &
      'xxx in subdirectory include.')

          if(ikenn.gt.idim3) goto 9301
          do 300 i2=1,ikenn
              read (insp,22,end=9000,err=9001)                    &
                  t(i2),nams,av(i2),vmax,qent(i2),jev
              do 340 i1=1,jev
                 read (insp,301,end=9000,err=9002)                &
                      v(i1,i2),q1(i1,i2),q2(i1,i2),fall(i1,i2)
340            continue
300       continue
          read (insp,25,end=114,err=9003) kende
      endif

114   if(inum.ne.issp) goto 14

      close (insp)

!cccccccccccccccccccccccccccccc
!c
!c einlesen der speicherdaten abgeschlossen
!c
!c fuer kurzzeitsimulationen wird die zeitschrittanzahl auf die max 
!c      moegliche ausgedehnt
!c
!c     write(res,9393)itext,issp,natg,namo,naja,netg,nemo,neja

      if (isim.eq.1) then
              idif = izsim
      end if

      ilen=ju0nch(itext)

      if(nress.gt.0) then
            if(dt.gt.23.) then 
                 write(nress,9393)itext(1:ilen),issp,           &
          natg1,namo1,naja1,netg1,nemo1,neja1
             else
                 n1t = netg1
                 n1m = nemo1
                 n1j = neja1
                 n1diff = (idif*dt+nast1)/24.
                 n1s = idif*dt+nast1-n1diff*24
                 call cdatum (n1t,n1m,n1j,n1diff)
                 write(nress,9394)itext(1:ilen),issp,           &
                  natg1,namo1,naja1,nast1,n1t,n1m,n1j,n1s
             endif
      endif
9393  format(/' auswertung ',a/                              &
       '    (gerinnestrang ',i5,')'/                         &
       ' bilanz:     vom ',i2,'.',i2,'.',i2/                    &
                 '   bis ',i2,'.',i2,'.',i2)

9394  format(/' auswertung ',a/                                 &
       '    (gerinnestrang ',i5,')'/                           &
       ' bilanz:     vom ',i2,'.',i2,'.',i2,', ',i2,' uhr   bis '  &
                     ,i2,'.',i2,'.',i2,', ',i2,' uhr')


      tanf=ntdif(1,1,naja1,natg1,namo1,naja1)+1


!cccccccccccccccccccccccccc
!
!c    jahres. bzw. ereignisschleife
!c
      do 3000 iz=1,idif

!ccccccccccccccccccccccccccccc
!c
!c festlegung der startwerte fuer iz=1
!c
      if((iz).gt.1) goto 119

      if(isim.eq.0.and.izyk.eq.1) then
                sv(1)     = av(1)
                nasteig(1)= 1
                q4max(1)  = 0.
      elseif(isim.eq.0.and.izyk.gt.1) then
              do 117 i=1,jmaxcon2
                  if(nresj(i,1).eq.issp) then
                      nasteig(1) = nresj(i,2)  
                      q4max(1) = xresj(i,1)  
                      sv(1) = xresj(i,2) 
                   endif
117           continue
        WRITE(nres,'(a)') ' Uebernommene Werte aus vorherigem Jahreszyklus:'
        write(nres,116) sv(1),q4max(1),nasteig(1)

        elseif (isim.eq.1) then
       	nksim = ju0gfu()
	       lt = 0
	       r = float(issp)
	       call s0rc(text0,r,lt)
	       fnam=pathn
	       fnam(ilend+1:ilend+6)='lzsim/'                              
	       ilenlz = ilend+6

!c  projekte aabach beruecksichtigt 5-stellige gebietsnummern
              if(lt.gt.4.and.ileng.gt.3) then
	          fnam(ilenlz+1:ilenlz+ileng-1)=namger(1:ileng-1)
                 ilenlz = ilenlz+ileng-1
              else
                 fnam(ilenlz+1:ilenlz+ileng)=namger(1:ileng)
                 ilenlz = ilenlz+ileng
              endif
	       fnam(ilenlz+1:ilenlz+lt)=text0(1:lt)
	       ilenlz = ilenlz+lt
	       fnam(ilenlz+1:ilenlz+4) = '.lzt'
	       ilenlz=ilenlz+4
              if(ilenlz.gt.80)then
              call writeLogIntInt(7, 'Pfad hat mehr als 80 Zeichen!','Path has more than 80 characters!',fnam,&
                   & '', 0,'ilenlz',ilenlz,'sp_geisel' )

	          write(nerr,'(3(/a))')'tree-name fuer den file '        &
                 ,fnam,' hat mehr als 80 zeichen.'


	          write(nerr,'(a)')'programm-abbruch.'                                       
                  call writeFooter()
	          stop                                                             
              endif                                                               
 	       inquire(file=fnam,exist=lvor)
              if(lvor)then
                 open (nksim,file=fnam,err=459,form='unformatted',  &
                       status='old')
	          rewind (nksim)
	          read (nksim,err=458) idat
	          if (idatsa.ge.idat) then
                     jja = int(idat/10**4)
		       jmo = int((idat-jja*10**4)/10**2)
		       jtg = int(idat-jja*10**4-jmo*10**2)
		       jdif = ntdif(jtg,jmo,jja,natg1,namo1,naja)
                     jdif=jdif+1

!c    in zeile 2-7 des datenfiles befindet sich kommentartext

		       do 2110 is=1,jdif
	                 read (nksim,err=458,end=468)             &
                              sv(1),q4max(1),nasteig(1)
2110                 continue

!EP,SK 21.10.2004 Aenderung Ausgabe

	      WRITE(nres,'(a)') ' Uebernommene Werte aus Langzeitdatei: '
              write(nres,116) sv(1),q4max(1),nasteig(1)

                 else
	              write(nres,2130)
                 endif
                 close (nksim)
!c                write (nres,'(2a)') 'name langzeitdatei ',
!c    +                    fnam(1:ilenlz)
	      else
          	  write(nres,499) fnam
                sv(1)=av(1)
                write(nres,116) sv(1),q4max(1),nasteig(1)
	      end if
      endif

	      if(izyk.gt.1) sv(1)=svend



116   format(' werte aus anfangsbedingung: '/ &
             ' speicherinhalt: ',f12.8/                        &
                ' q4max:          ',f12.8 /                        &
                ' nasteig:             ', i2)

2130  format('warnung! lesen der anfangsbedingungen fehlerhaft.'/    &
      	   ' anfangsdatum der simulation ist frueher als'/           &
          ' anfangsdatum der abgespeicherten werte.')

499   format(' warnung!'/&
             ' file ',a/&
             ' mit anfangsbedingungen fuer die'/  &
             ' kurzzeitsimulation existiert nicht!'/&
             '( talsperreninhalt, fuellzustand)'/&
             ' vor der kurzzeitsimulation sollte langzeitsimulation durch'/  &
             ' gefuehrt werden.')


119   continue


!cccccccccccccccccccccccccccc
!c
!c  zuordnung der fuer zeitschritt relavanten kennlinie
!c
!c    zuordnung der aktuellen zeit des simulationszeitschritts iz
!c       auf tage des jahres zur festsetzung der speicherennlinie
!c       (tag beginnt um 8 uhr)
      iztime=tanf+abs((nast1-8+iz*dt)/24.)

      idife = 0
      idifa = 0
      iend  = 0
      do 3100 i=1,ikenn
         if(iztime.lt.t(i)) then
             iend=i
             goto 3101
         endif
3100  continue
	iend=ikenn+1
3101  ianf=iend-1
      if(iend.eq.0) then
         iend=1
         ianf=ikenn
         idife=365
      endif
      if(ianf.eq.0) then
          ianf=ikenn
          idifa=-365
      endif

!c     write(nres,'('' iztime, ianf,iend: '',3(i5))') iztime,ianf,iend

      jakt=jev

!c     print*,' sv(iz): ', sv(iz)

      do 3200 i=2,jev
          vgrenz = v(i,ianf)+(v(i,iend)-v(i,ianf))                       &
                 *(iztime-t(ianf)-idifa)/(t(iend)+idife-t(ianf)-idifa)
!c         print*,' i, vgrenz: ', i,vgrenz
          if (sv(iz).lt.vgrenz) then
              jakt = i-1
              goto 3201
          endif
3200  continue
3201  continue

!c     print*,' iz: ', iz,'    zeitraum: ianf,iend: ', ianf,iend 
!c     print*,'                       wasserst.: jakt: ',jakt
!cccccccccccccccccccccccccccc
!c
!c festlegung des abflusses fuer zeitschritt iz
!c     in abhaengigkiet von fall(jakt,ianf)

      if(jakt.ge.jev.and.fall(jakt,ianf).le.4) then
           qa(iz)=qz(iz)+(sv(iz)-v(jev,ianf))/dtfak
           ueb(iz)=0.
           if(qa(iz).gt.q1(jev,ianf)) then
                ueb(iz) = qa(iz)-q1(jev,ianf)
                qa(iz)  = q1(jev,ianf)
           endif
           if(fall(jev,ianf).eq.2) nasteig(iz+1)=1
           if(fall(jev,ianf).eq.3) q4max(iz+1)=qa(iz)
           sv(iz+1)=v(jev,ianf) 
           goto 3000
      elseif(fall(jakt,ianf).eq.1) then
           qa(iz)   = q1(jakt,ianf)
      elseif(fall(jakt,ianf).eq.2) then
           qa(iz)   = q1(jakt,ianf)
           nasteig(iz+1) = 1
      elseif(fall(jakt,ianf).eq.3) then
           if (nasteig(iz).eq.1) then
                qa(iz) = q2(jakt,ianf)
                nasteig(iz+1)=1
           else
                qa(iz) = q1(jakt,ianf)
           endif
      elseif(fall(jakt,ianf).eq.4) then
           qa(iz)=qz(iz)
           if(qa(iz).gt.q1(jakt,ianf)) qa(iz)=q1(jakt,ianf)
           if(qa(iz).lt.q4max(iz)) qa(iz)=q4max(iz)
           q4max(iz+1)=qa(iz)
      elseif(fall(jakt,ianf).eq.5) then

!ccccccccccccc
!c abfluss = zufluss - verdunstung + niederschlag 
!c
!c	umrechnung: 	qent: 	= korrekturfaktor,
!c			epot	= verd. [mm/tag] pro m**2
!c			q2[.]	= seeflaeche [km**2]
!c	----->		qent	= entnahme in m**3/s
!c
            if(jakt.gt.0)then
		qver=qent(ianf)*epot(iz)*q2(jakt,ianf)/(24.*3.6)
		qnied=p(iz)*q2(jakt,ianf)/(24.*3.6)
		epot_o(iz)=epot(iz)*q2(jakt,ianf)
		p_o(iz)=p(iz)*q2(jakt,ianf)
		if(qver .le. qnied)then
                    qa(iz) = qz(iz)
                else
		    x1=qver-qnied
		    qa(iz)=(qz(iz)-x1)*q1(jakt,ianf)
                endif
                if(qa(iz) .lt. 0.) qa(iz) = 0.
!cpa		if(qa(iz)+qg(iku,iz).lt.0.3) qa(iz)=0.3-qg(iku,iz)
            else
!c
!c          Speicher hat Grenz-Wasserstand erreicht
!c
		qver=qent(ianf)*epot(iz)*q2(1,ianf)/(24.*3.6)
		qnied=p(iz)*q2(1,ianf)/(24.*3.6)
		qa(iz)=0.0
		epot_o(iz)=epot(iz)*q2(1,ianf)
		p_o(iz)=p(iz)*q2(1,ianf)
		
           endif
!c	write(nsv ,'(i4,6(f9.3))')iz,qz(iz),qa(iz),epot(iz),
!c    *			qver,p(iz),qnied
		sumnied=sumnied+qnied
		sumqverp=sumqverp+qver

      elseif(fall(jakt,ianf).eq.6) then
!ccccccccccccc
!c abfluss vorgegeben, inp: niederschlag , verd. seeflaeche 
!c
!c	umrechnung: 	qent: 	= korrekturfaktor,
!c			epot	= verd. [mm/tag] pro m**2
!c			q2[.]	= seeflaeche [km**2]
!c	----->		qent	= entnahme in m**3/s
!c
		qver=qent(ianf)*epot(iz)*q2(jakt,ianf)/(24.*3.6)
		qnied=p(iz)*q2(jakt,ianf)/(24.*3.6)
		epot_o(iz)=epot(iz)*q2(jakt,ianf)
		p_o(iz)=p(iz)*q2(jakt,ianf)

		sv2=(sv(iz)+(qz(iz)*dtfak)-v(jakt,ianf))/          &
      			(v(jakt+1,ianf)-v(jakt,ianf))
		if(sv2.lt.0.)sv2=0.
		if(sv2.gt.1.)sv2=1.
		qa(iz)=(1.-sv2)*q1(jakt,ianf)+sv2*q1(jakt+1,ianf)
!c		qa(iz)=q1(jakt,ianf)
		if(qa(iz).lt.0.) qa(iz)=0.
		sv2 = sv(iz)+(qz(iz)-qa(iz))*dtfak
		if(sv2.lt.v(jakt,ianf)) then
			qa2=sv(iz)+(qnied-qver)*dtfak -v(jakt,ianf)
			qa2=qa2/dtfak
			dtf2=qa2/qa(iz)
       			if(dtf2.lt.0.) dtf2=0.
			if(dtf2.gt.1.) dtf2=1.
!cb    220502   zusaetzliche If-Bedingung IF(jakt.gt.1.) then
                        IF(jakt.gt.1.) then
			qa(iz)=dtf2*qa(iz)+(1.-dtf2)*q1(jakt-1,ianf)
!cb    220502   zusaetzliche else-Anweisung  qa(iz)=qa(iz)*dtf2
                        else
                        qa(iz)=qa(iz)*dtf2
		        endif
                endif
		sumqverp=sumqverp+qver
		sumnied=sumnied+qnied

      elseif(fall(jakt,ianf).eq.7) then
!ccccccccccccc
!c abfluss: vorgegeben abfluss - abfluss knoten unten
!c		, inp: niederschlag , verd. seeflaeche
!c
!c	umrechnung: 	qent: 	= korrekturfaktor,
!c			epot	= verd. [mm/tag] pro m**2
!c			q2[.]	= seeflaeche [km**2]
!c	----->		qent	= entnahme in m**3/s
!c
		qver=qent(ianf)*epot(iz)*q2(jakt,ianf)/(24.*3.6)
		qnied=p(iz)*q2(jakt,ianf)/(24.*3.6)
		epot_o(iz)=epot(iz)*q2(jakt,ianf)
		p_o(iz)=p(iz)*q2(jakt,ianf)

		sv2=(sv(iz)+(qz(iz)*dtfak)-v(jakt,ianf))/        &
      			(v(jakt+1,ianf)-v(jakt,ianf))
		if(sv2.lt.0.)sv2=0.
		if(sv2.gt.1.)sv2=1.
		qa(iz)=(1.-sv2)*q1(jakt,ianf)+sv2*q1(jakt+1,ianf)
		qa(iz)=qa(iz)-qg(iku,iz)
		if(qa(iz).lt.0.) qa(iz)=0.
		sumqverp=sumqverp+qver
		sumnied=sumnied+qnied
      else
           write (nerr,3901) fall(jakt,ianf),ianf,inum
          call writeLogIntIntInt(7,'Fehler bei der Angabe des Rechenfalls! Moegliche Faelle:1-4.', &
          & 'Error with the indication of the calculation case! Possible cases:1-4.','','Speicher',inum,&
          & 'fall(jakt,ianf)',fall(jakt,ianf),'ianf',ianf,'sp_geisel' )

           call writeFooter()
           stop
3901       format(/,' fehler bei der angabe des rechenfalls!',/    &
             ' moegliche faelle:   1 - 4 ',/                      &
              ' eingelesener fall: ',i4,/                         &
              ' kennlinie:         ',i4,/                         &
              ' speicherelement:   ',i4)

      endif


      ueb(iz)  = 0.
      sv(iz+1) = sv(iz)+(qz(iz)-qa(iz))*dtfak
!cccccc
!c
!c korrektur des speicherinhaltes um entnahmemenge
!c

	if(fall(jakt,ianf).lt.5) then
		qentx=qent(ianf)
	else
		qentx=qver-qnied
	endif 

      if(sv(iz+1).gt.0) then
               svdiff = qentx*dtfak
               sv_speicher = sv(iz+1)- svdiff
      endif
      if(sv_speicher.lt.0.) then
               sent = sent + (svdiff-sv_speicher)/dtfak
      else
               sent = sent + qentx
      endif

      if(sv(iz+1).lt.0.) then
              sv(iz+1) = 0.
              qa(iz) = sv(iz)/dtfak+qz(iz)
      endif
      if(sv(iz+1).gt.vmax) then
             ueb(iz)  = (sv(iz+1)-vmax)/dtfak
             sv(iz+1) = vmax
      endif

3000  continue

!c  ende der zeitschrittschleife

!cccccccccccccccccccccccccccccccccc
!c
!c werteausgabe und wertuebergabe

      sqa  = 0.
      sueb = 0.
      sqz = 0.
      do 16 i=1,idif
           sqz = sqz +qz(i)
           sqa = sqa + qa(i)
           qg(iku,i) = qa(i) + qg(iku,i)
           if (iknot.eq.0) then              
			qg(iku,i) = ueb(i) + qg(iku,i)
			sueb = sueb + ueb(i)
	   else
             do 17 ik=1,ikmax 
                 if (iknot.eq.knot(ik)) then
                   qg(ik,i) = qg(ik,i) + ueb(i)
                   sueb = sueb + ueb(i)
              end if
17            continue
           end if
16    continue

!cwerte in hqbm
      fuqz = sqz * dtfak
      fuqa = sqa * dtfak
      fueb = sueb * dtfak
      fuqs=sv(idif+1)-sv(1)

	write(*,'(a)')' Speicherinhalt: '
	write(*,'(/a,f10.6)')'    Anfangsinhalt [hm]: ',sv(1)
	write(*,'(/a,f10.6)')'    Endinhalt [hm]:     ',sv(idif+1)
	write(*,'(/a,f10.6)')'    Zufluss-Fuelle [hm]:     ',fuqz
	write(*,'(/a,f10.6)')'    Abfluss-Fuelle [hm]:     ', fuqa
	if(sumqverp.gt.0.0001) then
		write(*,4003)sumqverp*dtfak,sumnied*dtfak,sent*dtfak
	endif
	write(*,'(/a,f10.6)')' Speicherinhaltsaenderung:   [hm] ',fuqs
	xs=fuqz-fuqa-(sent)*dtfak-fueb-fuqs
	write(*,4004) xs
4004	format(/' fehler:                     [hm] ',f10.6)

      if(nress.gt.0) then
         write(nress,'(a)')' Speicherinhalt: '
         write(nress,'(/a,f10.6)')'    Anfangsinhalt [hm]: ',sv(1)
         write(nress,'(/a,f10.6)')'    Endinhalt [hm]:     ',sv(idif+1)
         write(nress,'(/a,f10.6)')'    Zufluss-Fuelle [hm]:     ',fuqz
         write(nress,'(/a,f10.6)')'    Abfluss-Fuelle [hm]:     ', fuqa
	if(sumqverp.gt.0.0001) then
		write(nress,4003)sumqverp*dtfak,sumnied*dtfak,sent*dtfak

	elseif(sent.gt.0.00001) then
		write(nress,'(a,f10.6)')'Entnahme-Fuelle [hm]:    ', sent*dtfak
	endif

4003	format(/' Potentielle Seeverdunstung (V): [hm] ',f10.6,/      &
     		' Niederschlag               (N): [hm] ',f10.6,/     &
     		' Netto-Bilanz             (N-V): [hm] ',f10.6)

         if (iknot.ne.0)THEN
          write(nres,'(a,f8.4)')'Abfluss-Fuelle  Ueberlauf [hm]:',fueb
         ENDIF
         write(nress,'(/a,f10.6)')'Speicherinhaltsaenderung: [hm]',fuqs
      endif
      write(nres,'(/a,f10.6)')'zufluss-fuelle [hm]:     ', fuqz
      write(nres,'(/a,f10.6)') 'abfluss-fuelle [hm]:     ',fuqa
      if(sent.gt.0.00001)then
        write(nres,'(/a,f10.6)')'entnahme-fuelle [hm]:    ', sent*dtfak
      endif
      if (iknot.ne.0)then
       write(nres,'(/a,f10.6)')' abfluss-fuelle [hm]  ueberlauf ', fueb
      endif

      write(nres,'(/a,f10.6)') ' speicherinhaltsaenderung [hm]',fuqs

!c   ergebnisdateien: fuellenlinie      s v . d a t
!c                    ueberlauf     q u e b . d a t

!AG 040702  Die folgenden 9 Zeilen waren in der vorigen Version auskommentiert.
!AG         Sie werden wieder in die Subroutine hineingenommen, damit Fuellen-
!AG         linie auch fuer die gesteuerten Regenrückhaltebecken dargestellt
!AG         werden.

     if(nsv.gt.0) then
	if(sv(i) .gt.100.) then
         write (nsv,'(/4(i8)/)') inum,izyk,idif,2
         write (nsv,'(10(f10.4))') (sv(i),i=1,idif)
	else
         write (nsv,'(/4(i8)/)') inum,izyk,idif
         write (nsv,'(10(f10.6))') (sv(i),i=1,idif)
	endif
     endif

      if(nueb.gt.0) then
          write (nueb,'(/3(i8)/)') inum,izyk,idif
          write (nueb,'(10(f8.3,1x))') (ueb(i),i=1,idif)
      endif


!cccccccccccccccccccccccccccc
!c
!c	statistik: monatsmittel

	do 2304 i1=1,12
		svmm(i1)=0.
		anzdm(i1)=0.
2304	continue

	svend=sv(idif+1)

	do 7501 i1=1,idif
	   if(i1.le.31) then
		svmm(1)=svmm(1)+sv(i1)
		anzdm(1)=anzdm(1)+1
	   elseif(i1.gt.31.and.i1.le.59) then
		svmm(2)=svmm(2)+sv(i1)
		anzdm(2)=anzdm(2)+1
	   elseif(i1.gt.59.and.i1.le.90) then
		svmm(3)=svmm(3)+sv(i1)
		anzdm(3)=anzdm(3)+1
	   elseif(i1.gt.90.and.i1.le.120) then
		svmm(4)=svmm(4)+sv(i1)
		anzdm(4)=anzdm(4)+1
	   elseif(i1.gt.120.and.i1.le.151) then
		svmm(5)=svmm(5)+sv(i1)
		anzdm(5)=anzdm(5)+1
	   elseif(i1.gt.151.and.i1.le.181) then
		svmm(6)=svmm(6)+sv(i1)
		anzdm(6)=anzdm(6)+1
	   elseif(i1.gt.181.and.i1.le.212) then
		svmm(7)=svmm(7)+sv(i1)
		anzdm(7)=anzdm(7)+1
	   elseif(i1.gt.212.and.i1.le.243) then
		svmm(8)=svmm(8)+sv(i1)
		anzdm(8)=anzdm(8)+1
	   elseif(i1.gt.243.and.i1.le.273) then
		svmm(9)=svmm(9)+sv(i1)
		anzdm(9)=anzdm(9)+1
	   elseif(i1.gt.273.and.i1.le.304) then
		svmm(10)=svmm(10)+sv(i1)
		anzdm(10)=anzdm(10)+1
	   elseif(i1.gt.304.and.i1.le.334) then
		svmm(11)=svmm(11)+sv(i1)
		anzdm(11)=anzdm(11)+1
	   elseif(i1.gt.334) then
		svmm(12)=svmm(12)+sv(i1)
		anzdm(12)=anzdm(12)+1
	   endif
7501	continue

	do 7502 i1=1,12
		if(anzdm(i1).gt.0)svmm(i1)=svmm(i1)/anzdm(i1)
7502	continue


!c  endwert fuer den naechsten zyklus abspeichern

      if(isim.eq.0) then
           do 118 i=1,jmaxcon2
           if(nresj(i,1).eq.0.or.nresj(i,1).eq.issp) then
                 nresj(i,1) = issp
                 nresj(i,2) = nasteig(idif+1)
                 xresj(i,1) = q4max(idif+1)
                 xresj(i,2) = sv(idif+1)
                 goto 18
           endif
           write(nres,'(/a,2(i5),f8.4,f12.8)')' endwerte: ',           &
              issp,nasteig(idif+1),q4max(idif+1),sv(idif+1)
	write(nres,'('' kontroll:  '',2(f12.4))')xresj(1,2),xresj(2,2)
118        continue
18         continue

!cneu bei ohne LSZIM

!AG 040702  Die folgende endif-Anweisung ist fuer diese Version der Subroutine
!AG         hinfaellig geworden und wird daher auskommentiert.
!AG	endif
!cccccccccccccccccccccccccccccccccc
!c
!c   speichern der werte einer langzeitsimulation als anfangswerte fuer
!c             kurzzeitsimulationen

!AG  040702  Die folgenden 24 Zeilen wurden wieder in die Subroutine hereingenommen,
!AG          damit fuer die Kurzzeitsimulation Anfangswerte des Speicherinhaltes
!AG          in der LZSIM-Datei mit der Endung '.lzt' abgespeichert werden.

            lt = 0
	      r = float(issp)
	      call s0rc(text0,r,lt)
	      fnam=pathn
	      fnam(ilend+1:ilend+6)='lzsim/'
	      ilenlz = ilend+6
!c     uebernahme von nur 3 buchstaben des gerinnenamens noetig fuer
!c      gebiete mit 5 stelliger gebietsnummer (aabach)
            if(lt.gt.4.and.ileng.gt.3) then
	          fnam(ilenlz+1:ilenlz+ileng-1)=namger(1:ileng-1)
                ilenlz = ilenlz+ileng-1
            else
                fnam(ilenlz+1:ilenlz+ileng)=namger(1:ileng)
                ilenlz = ilenlz+ileng
            endif

	      fnam(ilenlz+1:ilenlz+lt)=text0(1:lt)
	      ilenlz = ilenlz+lt
	      fnam(ilenlz+1:ilenlz+4) = '.lzt'
	      ilenlz=ilenlz+4

            if(ilenlz.gt.80)then
!AG  040702  Die folgende write-Anweisung wurde Fortran 90 kompatibel gemacht indem
!AG          der Zeilenübergang von '+' auf '&' geaendert wurde und in die obere Zeile
!AG          verschoben wurde.
	           write(nerr,'(3(/a))') ' tree-name fuer den file',   &
                       fnam,' hat mehr als 80 zeichen.'
                   call writeLogIntInt(7,'Pfad hat mehr als 80 Zeichen.','Tree-name has more than 80 characters.',fnam,&
                   & '',0,'ilenlz',ilenlz,'sp_geisel')

	           write(nerr,'(a)') 'programm-abbruch.'
                   call writeFooter()
	           stop
            endif

            nanf = ju0gfu()

!ccccccccccccccccccccccccc
!c
!c  bei neuer langzeitsimulationsdurchlauf muss alter lzt file geloescht
!c          und neuer file angelegt werden
!c
!ccccccccccccc
!c
!c zzsim wird uebergangen
!c
!AG 040702  Die naechsten 24 Zeilen wurden wieder in die Subroutine hineingenommen.
!AG         Sie legen die LZSIM-Datei mit der Endung .lzt als formatlose Datei an,
!AG         und schreiben Anfangs- und Enddatum, sowie den aktuellen Speicherinhalt,
!AG         die maximale Entlastung für Fall 4 und den Zustand für Fall 3 hinein.
!AG
!AG         Da sie unformatiert ist, ist sie mit einem Texteditor nicht lesbar!
!AG         Die für diese Zeilen benötigte logische Variable 'lex' musste zusaetzlich
!AG         deklariert werden.
!AG
!AG         Die Zeilenübergaenge waren wiederum in Fortran77-Syntax geschrieben. Dies
!AG         wurde auf Fortran90-Syntax abgeaendert.

            if(izyk.eq.1) then
  	         inquire (file=fnam,exist=lex)
	         if (lex) then
		      open  (nanf,file=fnam,FORM='unformatted')
		      close (nanf,status='delete')
	         end if
	         open (nanf,err=3456,file=fnam,FORM='unformatted')
	         goto 3457
3456            write(nerr,'(a)')' directory ../lzsim/ muss angelegt werden!'

                call writeLogString(7,'Ordner ../lzsim/ existiert nicht und muss angelegt werden!',&
                     & 'Directory../lzsim/ must be applied.','','','','','','sp_geisel')

                call writeFooter()
               stop

3457            rewind (nanf)
	         write (nanf) idatsa,idatse
	         close (nanf)
            end if


	      open (nanf,err=456,file=fnam,form='unformatted',status='old')

	      rewind (nanf)
	      read (nanf) idum1,idum2
	      print*,' datsa = ',idum1,' idatse = ',idum2

!AG 040702  Die folgenden 3 Zeilen wurden wieder auskommentiert, weil sie
!AG         zu einer Fehlermeldung führen:
!AG         'sp_agger 468'
!AG         Was diese Fehlermeldung zu bedeuten hat, steht in dieser Routine
!AG         bei Marke 468.

!AG	      close (nanf)
!AG
!AG	      nanf = ju0gfu()
!AG	      open (nanf,err=456,file=fnam,form='unformatted',status='old')


6302          read (nanf,end=6303,err=6304) x1
		goto 6302
6304           write(nerr,'('' Fehler beim Einlesen von File '',a)')   &
                  fnam
              call writeLogString(6, 'Fehler beim Einlesen der Daten aus einem File.','Error by reading data from a file.', &
                   & fnam,'','','','','sp_geisel')

!AG 040702  An dieser Stelle musste eine Backspace-Anweisung eingeführt werden,
!AG         da ansonsten nach dem Lesevorgang die Datei nicht weiter beschrieben
!AG         werden konnte.
!AG         Folgende Fehlermeldung trat dann auf:
!AG         'A WRITE statement cannot be executed after an endfile record is read (unit=124).'
6303         BACKSPACE nanf
	     do 464 i=1,idif
		   write (nanf) sv(i),q4max(i),nasteig(i)
464          continue
	      close (nanf)
	end if


!cals ausgangswerte fuer die kurzzeitsimulation werden in der lzsim-datei
!cgespeichert:
!c              h(i)            talsperreninhalt
!c              q4max(i)        max. entlastung fuer fall 4
!c              nasteig(i)      zustand fuer fall 3
!c

      return


!c----------------------------------------------------------------------------
!c           formate
!c----------------------------------------------------------------------------

22    format (i4,1x,a10,3f10.6,i4)
31    format (' fehlerabbruch speicher, fehlerhafte eingabe von ',a12)
24    format (4x,f8.2,8x,f9.6,6x,f8.3)
301    format (10x,f10.6,2(f10.3),8x,i2)
25    format (a5)

331   format(/' fehler beim einlesen der daten von speicher ',i5,/      &
        ' eingelesene werte:'/,                                         &
       '            vmin = ',f12.8,'      vmax  = ',f12.8,/            &
       ' kennlinie: v(1) = ',f12.8,'      v(',i2,') = ',f12.8)

332   format(/' fehler beim einlesen der daten von speicher ',i5,/     &
        ' eingelesene werte:'/,                                        &
        '            vmin = ',f12.8,'      vmax  = ',f12.8,/           &
        ' kennlinie: v(1) = ',f12.8)



!ccccc fehlermeldungen



9999  write(nerr,'(a)')'fehler beim oeffnen von insp in speicher'
      call writeLogString(5,'Fehler beim Oeffnen einer Datei!', 'Error when opening a file',namrhb,'','','','','sp_geisel')

9000  write(nerr,9900) issp

9900  format(' fehler in subroutine speicher '/                        &
      ' rhb - gerinneabschnitt ',i4,' wurde nicht gefunden ! ')
      
      call writeLogIntInt(7,'Fuer einen Speicherstrang wurden keine Speicherdaten gefunden!', &
      & 'For a storage channel there is no data!!','','Strang',issp,'',0,'sp_geisel')
      call writeFooter()
      stop

9500  write (nerr,9501) 'hv',jev,inum
      call writeLogIntReal(7,'Fehler in den Speicherdaten. Es wurde ein Wert <= 0 eingegeben.',&
           & 'Error in the storage data. A value <= 0 was read.','','Speicher',inum,'Wasserstand (hv)',hv(jev),'sp_geisel' )

      call writeFooter()
      stop

9510  write (nerr,9501) 'vs',jev,inum
      call writeLogIntReal(7,'Fehler in den Speicherdaten. Es wurde ein Wert <= 0 eingegeben.',&
           & 'Error in the storage data. A value <= 0 was read.','','Speicher',inum,'Volumen (vs)',vs(jev),'sp_geisel' )
      call writeFooter()
      stop

9520  write (nerr,9501) 'qd',jev,inum
      call writeLogIntReal(7,'Fehler in den Speicherdaten. Es wurde ein Wert <= 0 eingegeben.',&
           & 'Error in the storage data. A value <= 0 was read.','','Speicher',inum,'Abfluss (qd)',qd(jev),'sp_geisel' )
      call writeFooter()
      stop

9501  format('xxx fehler in der rhb-datei!'/                           &
        'xxx fuer ',a, '(',i2,') wurde ein wert <= 0 eingelesen.'/     &
        'xxx rhb-nummer: ',i5,/'xxx korrigiere rhb-datei!')
9531  format(' warnung!'/ ' speicherabfluss von 0. fuer vollen ',      &
        'speicherinhalt wurde eingelesen.'/                           &
        ' speicher ',i5,' kann nicht leerlaufen!' )


9301  write (nerr,9302) idim3
9302  format('xxx fehler in der rhb-datei!'/                          &
        'xxx mehr als ',i3,' kennlinien wurde eingelesen.'/           &
       'xxx aktuelles speicherelement ',i5,/                          &
      'xxx korregiere rhb-datei oder erhoehe p. idim3 in sp_agger !')
      call writeLogIntInt(7, 'Mehr als max. moegliche Anzahl von Speicherkennlinien wurde eingelesen!',&
           & 'More than max. possible wqv functions have been read!','','Speicher',inum,'max. Anzahl (idim3)',idim3,'sp_geisel')
      call writeFooter()
      stop

456   write(nerr,4561)fnam
4561  FORMAT(' fehler beim oeffnen von nanf in ',                    &
       ' sp_geisel '/' (anfangsbedingung kurzzeitsimulation)',/    &
       ' filename: ',a)
      call writeLogString(7,'Fehler beim Oeffnen einer Datei (Anfangsbedingung Kurzzeitsimulation)!', &
      &'Error when opening a file (initial condition short term simulation)!',fnam,'','','','','sp_geisel' )

      call writeFooter()
      stop

459   write(nerr,4591)
4591  Format('xxx fehler beim oeffnen von nksim in  ',                    &
       'g e b i e t '/'xxx (anfangsbedingung langzeitsimulation)')
      call writeLogString(7,'Fehler beim oeffnen einer Datei (Anfangsbedingung Langzeitsimulation)!', &
      &'Error when opening a file (initial condition long term simulation)!',fnam,'','','','','sp_geisel')

      call writeFooter()
      stop

458   write(nerr,10458)fnam
10458 format('xxx unerwartetes ende in der scratch-datei (nksim)',/       &
      	'xxx der anfangsbedingung einer kurzzeitsimulation gefunden. ',/  &
        'xxx wiederhole langzeitsimulation und ',/                        &
        'xxx ueberpruefe datei ',a)
      call writeLogString(7,'Formatfehler in einer Datei!','Formatting error in a file!', &
           & fnam,'','','','','sp_geisel')

      call writeFooter()
      stop

468   write(nerr,10468)fnam
10468 format(/'xxx unerwartetes ende in der scratch-datei *.lzt ',/        &
      	'xxx der anfangsbedingung fuer die kurzzeitsim. gefunden.',/     &
      	'xxx die langzeitsimulation wurde nicht bis zum zeitintervall'/    &
      	'xxx der kurzzeitsimulation durchgefuehrt!'/,                     &
      	'xxx wiederhole langzeitsimulation fuer laengeren zeitraum ',/    &
        'xxx oder ueberpruefe datei ',a)
      call writeLogString(7,'Formatfehler in einer Datei!','Formatting error in a file!', &
           & fnam,'','','','','sp_geisel')

      call writeFooter()
      stop

9001  write (nerr,9011) inum,i2
9011  format('xxx fehler beim einlesen der kopfzeile in rht-datei!'/     &
        'xxx letzter eingelesener speicher:  ',i3, /                     &
        'xxx eingelesende kennlinienanzahl:  ',i3)
      call writeLogIntInt(7,'Fehler beim Einlesen der Kopfzeile in der Speicherdatei!',&
           & 'Error when reading the headline in storage channel file!',namrhb,'Speicher',inum,'i2',i2,'sp_geisel' )

      call writeFooter()
      stop

9002  write (nerr,9012) inum,i2,i1
9012  format(                                                             &
        'xxx fehler beim einlesen d. speicherinhaltskurve in rht-datei!'/ &
        'xxx letzter eingelesener speicher:      ',i3, /                  &
        'xxx anzahl eingelesener kennlinien:     ',i3, /                  &
        'xxx anzahl eingelesene speicherfuellen: ',i3)
      call writeLogIntInt(7,'Fehler beim einlesen d. speicherinhaltskurve in der Speicherdatei!', &
      & 'Error when reading the storage content curve in storage channel file!','','Speicher',inum,'i2',i2,'sp_geisel' )

      call writeFooter()
      stop


9003  write (nerr,9013) inum,i2,i1,kende
      call writeLogIntInt(7,'Fehler beim Einlesen der Kopfzeile in der Speicherdatei!',&
           & 'Error when reading the headline in storage channel file!',namrhb,'Speicher',inum,'',0,'sp_geisel')
      call writeFooter()
      stop
9013  format('xxx fehler beim einlesen der kopfzeile in rht-datei!'/      &
        'xxx letzter eingelesener speicher:  ',i3, /                      &
        'xxx eingelesende kennlinienanzahl:  ',i3,/                       &
        'xxx anzahl eingelesene speicherfuellen: ',i3,/                   &
        'xxx eingelesener text: ', a)

	end



