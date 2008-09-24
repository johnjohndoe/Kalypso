!     Last change:  JH   17 Jan 2007    7:57 am

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

	subroutine inp_veg_b_wt(namveg,natg,namo,naja,       &
      		xtime,time1,time2,                            &
      		wt_bm,anzbod,kennbod,pns)
!ccccccc
!c	subroutine zum einlesen von bodenabhaengigen Wurzeltiefen
!c	fuer vorgegebene nutzung
!c
!c	gesucht werden werte wt_bm(i,1) und wt_bm(i,2) der Wurzweltiefen,
!c	 1<i<anzbof aus daten namveg fuer zeitpunkte
!c	fuer  time1 < xtime < time2,
!c

	USE generic_LOGGER
	include      'include\param.cmn'

	integer	nwt,natg,namo,naja,ijaha,ianz1,ianz
	real	xtime,time1,time2,time3,time4
	real	wt(idimnutz3,2),dmfak
	character*80	text,text1,text2,text3,namveg
	character*10	text10
	character*1	pns
	integer	anzbod

	character*10	kennbod(idimnutz2),kennwt(idimnutz3)

!cccccccccccccccccccccc
!c
!c boden und nutzungsabhaengige durchwurzelungstiefe
!c
	real	wt_bm(idimnutz2,2)

	ianz1=0
	ijaha=0

	nwt=ju0gfu()
	open (nwt,iostat=ierr,err=996,                       &
      			status='old',file=namveg)

	ijaha=0
	read(nwt,'(a)',end=997) text   
	read(nwt,'(a)',end=997) text   
	if(text(1:6).eq.'ideali') then
		ijaha=naja
	endif 
	dmfak=1.
	do 6000 i=1,50
		if(text(10+i:17+i).eq.'eingabe:') then
			if(text(19+i:20+i).eq.'dm')dmfak=.1
			if(text(19+i:20+i).eq.'zm')dmfak=.01
			goto 6001
		endif
6000	continue
6001	continue

9	read(nwt,'(a)',end=1000) text 
	if(text(1:1).eq.' ') goto 1000  
	read(text(20:),*)ianz
	if(ianz1+ianz.gt.idimnutz3) goto 995

	read(nwt,'(a)') text3

	read(nwt,426,end=997)itagl,imonl,ijahl,text1
	nleaf=ntdif(natg,namo,naja,itagl,imonl,ijaha+ijahl)
	time1=real(nleaf)
	nleaf=ntdif(natg,namo,naja,itagl,imonl,ijaha+ijahl+1)
	time3=real(nleaf)
10	read(nwt,426,end=997)itagl,imonl,ijahl,text2
426	format (i2,1x,i2,1x,i4,1x,a)

	if(pns.eq.'s') then
		read(nwt,426,end=997)itagl,imonl,ijahl,text
		return
	endif
	if(itagl.eq.99) return

	nleaf=ntdif(natg,namo,naja,itagl,imonl,ijaha+ijahl)
	time2=real(nleaf)
	nleaf=ntdif(natg,namo,naja,itagl,imonl,ijaha+ijahl+1)
	time4=real(nleaf)
	if ((time1.le.xtime.and.time2.gt.xtime) .or.                &
      		(time3.le.xtime.and.time4.gt.xtime))then
		read(text1,*,err=990)(wt(ianz1+i1,1),i1=1,ianz)
		read(text2,*,err=990)(wt(ianz1+i1,2),i1=1,ianz)
		i2=11
		do 15 i1=1,ianz
16			if(text3(i2:i2).eq.' ') then
				i2=i2+1
				if(i2.ge.80) goto 991
				goto 16
			endif
			i3=i2
17			continue
!c			if(text3(i3:i3).ne.' ') then
			if(ichar(text3(i3:i3)).ne.32) then
				i3=i3+1
				goto 17
			endif
			read(text3(i2:i3-1),'(a)')text10
			call lcase(text10)
			call moveblank(text10)
			kennwt(ianz1+i1)=text10
			i2=i3
15		continue
		ianz1=ianz1+ianz
11		continue
		read(nwt,426,end=997)itagl,imonl,ijahl,text
		if(itagl.eq.99) goto 9
		goto 11
	endif
	text1=text2
	time1=time2
	time3=time4
	goto 10

!ccccccccccc
!c
!c  ende dateneinlesen

1000	continue
	close (nwt)

	do 1001 i1=1,anzbod
	   do 1002 i2=1,ianz1
		if(kennwt(i2).eq.kennbod(i1)) then
			wt_bm(i1,1)=wt(i2,1)*dmfak
			wt_bm(i1,2)=wt(i2,2)*dmfak
			goto 1001
		endif
1002	   continue
	   goto 998
1001	continue

	if (time3.le.xtime.and.time4.gt.xtime) then
		time1=time3
		time2=time4
	endif


	return

!ccccccc
!c	fehlermeldungen

997	continue	
	write(*,101)
	call writeLogString(7,'Simulationszeitraum wird nicht durch Jahresgang abgedeckt.',&
	     & 'Simulation period is not covered by ideal land use period!','','','','','','inp_veg_b_wt')

101	format('xxx fehler in datenfile vegetationsparam.'/,               &
     	'xxx simulationszeitr. wird nicht durch nutzungswerte abgedeckt')
        call writeFooter()
	stop


996	continue	
	write(*,102)         
102	format('xxx fehler bei uebernahme durchwurzelungstiefe'/,       &
       'xxx datenfile mit boden/nutzungsabhaengigen werte nicht ',      &
       'gefunden',/                                                     &
       'xxx filename: ',a)

     call writeLogString(7,'Datenfile mit Nutzungsabhaengigen Werten nicht gefunden!',&
	     & 'File data with land use dependant values was not found!',namveg,'','','','','inp_veg_b_wt')

        call writeFooter()
	stop


998	continue	
	write(*,103)kennbod(i1)
103	format('xxx fehler bei uebernahme durchwurzelungstiefe'/,       &
       'xxx fuer bodentyp wurden keine angaben zur durchwurzelungs',     &
       'tiefe gefunden ',/                                              &
       'xxx bodentyp: ',(a))
	
        call writeLogString(7,'Fuer einen Bodentyp wurden keine Angaben zur Durchwurzelungstiefe gefunden!',&
      	     & 'For a soil type no information have been found for root penetration!','','','','kennbod(i1)',&
	     & kennbod(i1),'inp_veg_b_wt')

        call writeFooter()
	stop


990	continue	
	write(*,104)text1,text2,ianz
104	format('xxx fehler bei uebernahme durchwurzelungstiefe'/         &
       'xxx angaben durchwurzelungstiefen und boden stimmen in anzahl ',   &
       'nicht ueberein: ' ,/                                              &
       'xxx eingelesene zeile mit durchwurzelungstiefen: ' ,/             &
       'xxx ',(a),/                                                       &
       'xxx ',(a),/                                                       &
       'xxx anzahl unterschiedlicher boeden: ',(i4))
	
	call writeLogIntInt(7,'Anzahl angegebener Durchwurzelungstiefen und Boden stimmen nicht ueberein!', &
	     & 'Number of root depth and soil do not match!','','',0,'ianz',ianz,'inp_veg_b_wt')
	
        call writeFooter()
	stop

991	continue
	write(*,106)text3,ianz
106	format('xxx fehler bei uebernahme durchwurzelungstiefe'/,         &
       'xxx angaben zu bodenanzahl stimmen nicht ueberein ',/             &
       'xxx eingelesene zeile mit bodenkennung: ' ,/                      &
       'xxx ',(a),/                                                       &
       'xxx anzahl unterschiedlicher boeden: ',(i4))
	
	call writeLogIntInt(7,'Fehler bei Uebernahme der Durchwurzelungstiefe. Angaben zu Bodenanzahl stimmen nicht ueberein!',&
	     & 'Specification of number of soils do not match!','','',0,'ianz',ianz,'inp_veg_b_wt')

        call writeFooter()
	stop

995	continue
	write(*,105)ianz1+ianz
105	format('xxx fehler bei uebernahme durchwurzelungstiefe'/         &
       'xxx mehr als idimnutz3 unterschiedliche bodentypen in  ',          &
       'eingabefile ',/                                                   &
       'xxx anzahl: ',2(i8))
	
	 call writeLogIntInt(7,&
              &'Fehler bei der Uebernahme der Durchwurzelungstiefe. Mehr als max. moegliche Bodentypen angegeben!',&
	      & 'Max. number of different soil types reached!','','',0,'ianz1+ianz',ianz1+ianz,'inp_veg_b_wt')

        call writeFooter()
	stop

	end
