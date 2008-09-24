!     Last change:  JH   17 Jan 2007    8:08 am

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

      subroutine speicher(iz,iku,dt,idif,izsim,issp,qz1,nsv,nueb,slash)

!**************************************ÄNDERUNGEN***********************************************
!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!    11.11.07   JN					Clear unused stuff
!

!***************************************BESCHREIBUNG********************************************
!     Rueckhaltebeckendurchlauf einer Abflusswelle
!     Speicherinhalt und Abfluss wird berechnet nach dem Puls-Verfahren

!***************************************EIN-/AUSGABE********************************************

!******************************************VARIABLEN********************************************

USE generic_LOGGER
USE Units	

parameter (idim2=24)

INCLUDE 'include\param.cmn'
INCLUDE 'include.spi'

CHARACTER (LEN=1)  :: slash
CHARACTER (LEN=16) :: nams
CHARACTER (LEN=80) :: namrhb
INTEGER 		   :: insp,inum,issp,iz,nsv,nueb,ik,iknot,iend,izsim,&
                      istart,iende,ilen,i,irhb
REAL(KIND=4)       :: fuqz,fuqz2,fuqa,fueb,sqz,sqz2,sqa,sueb,delta,qaa,&
                      qz1,qhilf
dimension qz(0:idim),qa(0:idim),sv(0:idim),ueb(idim),             &
          hv(idim2),vs(idim2),qd(idim2),qz1(idim)

data hv/idim2*0./,vs/idim2*0./,qd/idim2*0./,nams/'                '/

call u1null(qz,idim)
call u1null(qa,idim)
call u1null(sv,idim)
call u1null(ueb,idim)
qz(0) = 0
qa(0) = 0
sv(0) = 0

insp = 0 
sqz  = 0
sqz2 = 0
sqa  = 0
sueb = 0
fuqz = 0
fuqa = 0
fueb = 0

qaa = 0.
!******************************************ANWEISUNGEN******************************************
!   Zufluszwelle: Gesamtabfluss des oberen Knotens  

do i=1,idif
 qz(i)=qz1(i)
 sqz = sqz + qz(i)
enddo
do i=idif+1,izsim
 qz(i)=qz1(i)
 sqz2 = sqz2 + qz(i)
enddo

dtz = dt * 0.0036

! Zuflussfuelle in [hm]
fuqz = sqz * dtz
fuqz2 = sqz2 * dtz

! Einlesen Speicherdaten

namrhb(1:80)=pathn(1:ilend)
namrhb(ilend+1:ilend+8)='inp.dat'//slash
ilen = ilend + 8
namrhb(ilen+1:ilen+ilenb)=namger
ilen=ilen+ilenb
namrhb(ilen+1:ilen+4)='.rhb'
ilen = ilen+4

if(ilen.gt.80)then
   call writeLogIntInt(7, 'Pfad hat mehr als 80 Zeichen', 'Path has more than 80 characters', namrhb,&
                       & '',0,'ilen',ilen,'speicher')
   write(nerr,'(3a)')'tree-name fuer den file',namrhb,'mehr als 80 zeichen.'
   write(nerr,'(a)')'programm-abbruch.'
   call writeFooter()
   stop
endif


insp = ju0gfu()
open (insp,iostat=ierr,err=9999,file=namrhb)
irhb=0
14    irhb=irhb+1
if (irhb.gt.jmaxcon) then
    call writeLogIntInt(7,'Max. Anzahl von Speicherstraengen im Modell ueberschritten.',&
                        & 'Max number of storage channels reached.','','',0,'jmaxcon',jmaxcon ,'speicher' )
    write(nerr,141)
    call writeFooter()
    stop
endif

read(insp,'(8x,2(i8))',end=9000) inum,iknot
read(insp,'(80x)',end=9000)
read(insp,22,end=9000) nams,sv(1),vmax,vmin,jev

!c    fehlermeldungen

if (vmax .le. vmin .or. sv(1) .lt. vmin .or. sv(1) .gt. vmax) then
call writeLogIntInt(7,'Fehler der Dateneingabe im Speicherstrang, Hinweis: Es muss gelten vmin < Anfangsinhalt < vmax.',&
              & 'Incorrect input in storage channel. Hint: vmin < initial content < vmax.','','Strang',inum,&
              & '',0,'speicher')
write (nerr,31) 'vo/vmax/vmin'
write (nerr,332) inum,vmin,vmax,sv(1)
call writeFooter()
stop
end if

if (jev .ge. 24) then
write (nerr,31) 'jev'
call writeLogIntInt(7,'Fehler der Dateneingabe im Speicherstrang. Max. Anzahl der Elemente der Speicherkennlinie auf 23 begrenzt',&
              & 'Incorrect input in storage channel. Max. number of values 23.','','Strang',inum,&
              & '',0,'speicher')

write (nerr,*) 'Maximale Anzahl an Elementen dre Speicherkennlinie (23) ist überschritten.'
call writeFooter()
stop
end if
do i=1,jev
  read (insp,24,end=9000) hv(i),vs(i),qd(i)
end do
if ((hv(jev)-0.00001).lt.0.) goto 9500
if ((vs(jev)-0.00001).lt.0.) goto 9510
if ((qd(jev)+0.00001).lt.0.) goto 9520
if (qd(jev).eq.0.) then
   write(nres,9531)inum
   call writeLogIntInt(6, 'Speicherabfluss von 0.0 m³/s fuer vollen Speicherinhalt wurde angegeben.',&
         & 'Storage discharge of 0.0 m³/s was read for the total storage content.','','Speicher',inum,'',0,'speicher')
   write(nerr,9531)inum
endif
if (abs(vs(1)-vmin) .gt. 0.001 .or. abs(vs(jev)-vmax) .gt. 0.001) then
write (nerr,31) 'vo/vmax/vmin'
call writeLogIntInt(7,'Fehler der Dateneingabe im Speicherstrang, Hinweis: vmin, vmax entspricht nicht WQV Inhalten.',&
              & 'Incorrect input in storage channel. Hint: vmin, vmax not equal to data in WQV content.','','Strang',inum,&
              & '',0,'speicher')

write (nerr,331) inum,vmin,vmax,sv(1),jev,sv(jev)
call writeFooter()
stop
end if
read (insp,25)

if(inum.ne.issp) goto 14

close (insp)

!c  bei mehreren jahreszyklen ist der jeweilige endwert (speicherinhalt)
!c  des vorherigen zyklusses zu beruecksichtigen
!c  ansonsten muessen startwerte fuer das erste zeitintervall angegeben
!c  werden
if (iz.gt.1) then
            do 13 i=1,jmax
                 if (jnr(i).eq.issp) goto 333
13          continue

333         sv(0) = sv1(i,1)
    qz(0) = sv1(i,2)
    qa(0) = sv1(i,3)
    istart=1
else
   if(sv(1).le.vs(1)) then
      if(qz(1).gt.qd(1)) then
         qa(1) = qd(1)
      else
         qa(1) = qz(1)
      end if
   else if(sv(1).ge.vs(jev)) then
      if(qz(1).lt.qd(jev)) then
         qa(1) = qd(jev)
         if((qd(jev)*dtz).ge.vmax) then
            qa(1) = qd(1)
         endif
      else
         qa(1) = qz(1) 
      end if
   else
      do 120 i=2,jev
       if(vs(i).ge.sv(1)) then
          qa(1) = qd(i-1) + (qd(i) - qd(i-1)) *	(sv(1) - vs(i-1))/(vs(i) - vs(i-1))
          goto 130
       end if
120   continue
   end if 
130 istart=2
endif
         

iend = idif
qges = qa(istart-1)

!c-----------------------------------------------------------------------
!c berechnung von speicherinhalt und abfluss aus zufluss und altem speicherinhalt
!c  berechnung erfolgt nach dem puls-verfahren  

!c  qges bezeichnet den gesamtabfluss = ueberfall+grundabl.
!c----------------------------------------------------------------------- 
135   do 220 i=istart,iend 

 svr=(sv(i-1)-vmin)/dtz+qz(i) 
!c  speicherinhalt + zuflusz ist kleiner als min. grundablasz      
 if (svr.lt.qd(1)) then 
    sv(i)=vmin
    if (sv(i-1).lt.vmin) then
       qges=qz(i)
    else
       qges=(sv(i-1)-vmin)/dtz+qz(i) 
    endif
    goto 200 
 endif

!c berechnung erfolgt auf der basis der speicherkontinuitaetsgleichung: 
!c
!c     0.5*dtz [(qz(i-1)+qz(i)) - (qa(i-1)+qa(i))] = sv(i)-sv(i-1) 
!c
!c  bzw. aufgeloest nach dem unbekannten speicherinhalt und abfluss: 
!c
!c      0.5*dtz * (qz(i-1)+qz(i)) + sv(i-1) - 0.5*dtz * qa(i-1) =   
!c                       sv(i) + 0.5*dtz * qa(i)  
      
 spo=0.5*dtz*(qz(i-1)+qz(i))+sv(i-1)-0.5*dtz*qges 

!c  aus spo=vs(i)+0.5*qa(i)*dtz muss nun der jeweilige wert von
!c  sv(i) und qa(i) berechnet werden 

 if (spo.ge.(vs(jev)+0.5*dtz*qd(jev))) then
    sv(i)=vs(jev)
    qges=(spo-sv(i))*2/dtz
    qhilf=(sv(i-1)-sv(i))/dtz
    if(qhilf.lt.0.) qhilf=0.
    if (qges.gt.(qz(i)+qhilf)) qges=qz(i)
 else if(spo.le.(vs(1)+0.5*qd(1)*dtz)) then
    sv(i)=vs(1)
    qges=(spo-sv(i))*2/dtz
    if (qges.gt.qd(1)) then
       sv(i)=vs(1)+(qges-qd(1))*dtz
       qges=qd(1) 
       write(nres,2001)i,sv(i),qges
    endif
 else
    do 240 i1=2,jev
     if ((vs(i1)+0.5*dtz*qd(i1)).gt.spo) then
        delta=(spo-(vs(i1-1)+0.5*dtz*qd(i1-1)) ) /((vs(i1)-vs(i1-1))+0.5*dtz*(qd(i1)-qd(i1-1)))
        qges=delta*(qd(i1)-qd(i1-1))+qd(i1-1)
        sv(i)=delta*(vs(i1)-vs(i1-1))+vs(i1-1)
        goto 200 
     endif 
240 continue
      
 endif 

            
200 if (qges.lt.10**(-10)) qges=0.
   if (qges.gt.qd(jev)) ueb(i)=qges-qd(jev) 
   if (iknot.ne.0) then 
      qa(i) = qges-ueb(i)
   else    
      qa(i) = qges
   endif  

220    continue
!c-----------------------------------------------------------------------
      
if(isim .eq. 0) goto 131 

!c   keine verlaengerung des simulationszeitraums fuer langzeitsimulationen
if (iend.eq.izsim) goto 131
if (sv(iend).gt.vmin.or.qz(iend).gt.0.001) then
   istart = iend+1
   iend = iend+50
   iend = min(iend,izsim)
   goto 135
else if (iend.eq.izsim) then
   write(nres,'(a,i5)')'warnung!! speicherroutine fuer speicher ',issp 
   write(nres,'(a)')' bei berechnungsende vmin noch ','nicht erreicht !' 
   write(nres,'(a,i5)')' zeitschritt ',iend
   goto 131
end if

131 do i=1,iend
 sqa = sqa + qa(i)
 qg(iku,i) = qa(i) + qg(iku,i)
end do

if (iknot.ne.0) then              
   do ik=1,ikmax 
    if (iknot.eq.knot(ik)) then
       do i=1,iend
        qg(ik,i) = qg(ik,i) + ueb(i)
        sueb = sueb + ueb(i)
       end do
    end if
   end do
end if

fuqa = sqa * dtz
fueb = sueb * dtz

write(nres,'(/a,f10.6,a,i4)')' zufluss-fuelle [hm]      ', fuqz,' speicher ',inum
write(nres,'('' zusaetzl.zufl im nachl.ast [hm]'',f10.6)')   fuqz2
write(nres,'(/a,f10.6,a,i4)')' speicher ', fuqa,' abfluss-fuelle [hm]      ',inum

if (iknot.ne.0)  &
write(nres,'('' abfluss-fuelle [hm]'',f8.4,'' ueberlauf '')') fueb
fuqa=sv(iend)-sv(1)
write(nres,'(/'' speicherinhaltsaenderung [hm]'',f10.6)') fuqa

!c   endwert fuer den naechsten zyklus abspeichern
    
do 18 i=1,jmax
 if (jnr(i).eq.0) then
    jmax = i
    goto 777
 end if
 if (jnr(i).eq.issp) goto 777
18 continue

jmax = jmax + 1
i    = jmax

777 jnr(i) = issp
sv1(i,1) = sv(iend)
sv1(i,2) = qz(iend)
sv1(i,3) = qges
      
if (isim.eq.0) then
   iende = iend
else
   iende = izsim
end if

!c----------------------------------------------------------------------------

! korrektur von schwingungen bei leerem speicher
do i=istart,iende
 qan=qa(i)
 if (sv(i).lt.10**(-5)) qa(i)=(qaa+qa(i))/2
 qaa=qan
end do

!c    ergebnisdateien: fuellenlinie      s v . d a t
!c                     ueberlauf     q u e b . d a t

if(nsv.gt.0) then
   write (nsv,'(/3(i8)/)') inum,iz,iende
   write (nsv,'(10(f10.6))') (sv(i),i=1,iende)
endif

if(nueb.gt.0) then
   write (nueb,'(/3(i8)/)') inum,iz,iende
   write (nueb,'(10(f8.3,1x))') (ueb(i),i=1,iende)
endif

return


!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

141   format (' es wurden zu viele speicher im modell integriert.'      &
     /' maximal moegliche speicheranzahl: 15'/                          &
     ' erhoehe parameter jmaxcon und felddimensionierung in rhb.cmn'/   &
     ' in subdirectory include.')

2001    format('speicherroutine berechnet fuer zeitschritt',i4,      &
                ' nicht sinnvolle werte',/' speicherinhalt:',        &
                f10.5,/' abflusz:    ',f10.5)

22    format (5x,a10,3f10.6,i4)
31    format (' fehlerabbruch speicher, fehlerhafte eingabe von ',a12)
24    format (4x,f8.2,8x,f9.6,6x,f8.3)

25    format (5x)

331   format(/' fehler beim einlesen der daten von speicher ',i5,/      &
       ' eingelesene werte:'/,                                          &
       '            vmin = ',f12.8,'      vmax  = ',f12.8,/             &
       ' kennlinie: v(1) = ',f12.8,'      v(',i2,') = ',f12.8)

332   format(/' fehler beim einlesen der daten von speicher ',i5,/      &
       ' eingelesene werte:'/,                                          &
       '            vmin = ',f12.8,'      vmax  = ',f12.8,/             &
       ' kennlinie: v(1) = ',f12.8)

9999  write(nerr,'(a)')'fehler beim oeffnen von *.rhb in speicher'
      call writeLogString(7, 'Fehler beim Oeffnen der Speicherstrangdatei.','Error when opening storage channel file',namrhb,&
           & '','','','','speicher')

      write(*,'(a)')'fehler beim oeffnen von *.rhb in speicher'

9000  write(nerr,9900) issp
     call writeLogIntInt(7,'Fuer einen Speicherstrang sind keine Speicherdaten vorhanden!',&
          & 'storage channel data was not found!','','Strang',issp,'',0,'speicher')

9900  format(' fehler in subroutine speicher '/                         &
       ' rhb - gerinneabschnitt ',i4,' wurde nicht gefunden ! ')
      call writeFooter()
      stop

9500  write (nerr,9501) 'hv',jev,inum
      call writeLogIntReal(7,'Fehler in den Speicherdaten. Es wurde ein Wert <= 0 eingegeben.',&
           & 'Error in the storage data. A value <= 0 was read.','','Speicher',inum,'Wasserstand (hv)',hv(jev),'speicher' )

      call writeFooter()
      stop
9510  write (nerr,9501) 'vs',jev,inum
      call writeLogIntReal(7,'Fehler in den Speicherdaten. Es wurde ein Wert <= 0 eingegeben.',&
           & 'Error in the storage data. A value <= 0 was read.','','Speicher',inum,'Volumen (vs)',vs(jev),'speicher' )

      call writeFooter()
      stop
9520  write (nerr,9501) 'qd',jev,inum
      call writeLogIntReal(7,'Fehler in den Speicherdaten. Es wurde ein Wert <= 0 eingegeben.',&
           & 'Error in the storage data. A value <= 0 was read.','','Speicher',inum,'Abfluss (qd)',qd(jev),'speicher' )

      call writeFooter()
      stop
9501  format(' fehler in der rhb-datei!'/                              &
       ' fuer ',a, '(',i2,') wurde ein wert <= 0 eingelesen.'/         &
       ' rhb-nummer: ',i5,/' korrigiere rhb-datei!')
9531  format(' warnung!'/ ' speicherabfluss von 0. fuer vollen ',      &
       'speicherinhalt wurde eingelesen.'/                             &
       ' speicher ',i5,' kann nicht leerlaufen!' )

end

