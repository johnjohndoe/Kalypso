!     Last change:  JH    9 Mar 2007    3:54 pm
subroutine gwsp(ntg,dt,idif,isv,ivor,iz,perk1,perk2,anzelem,m_f1gws,    	&
		       nw_gws,klupor,tgwzu,nutzprz,pns,naja20,m_flaech,sperk,   &
                       sperk2,skap,idatsa20,nanfstd,nanf,nanzanf,nanzanf1,xanf, &
                       nanff,m_bil,xbil,fnsum,ngws,nqb,ngw,nqtg)
USE generic_LOGGER
USE Units
IMPLICIT NONE

INCLUDE  'include\param.cmn'
INCLUDE  'include\gw.cmn'
INCLUDE  'include\gwl.cmn'
INCLUDE  'include\qgw.cmn'
INCLUDE  'include\str.cmn'
INCLUDE  'include\is.cmn'
INCLUDE  'include\nad.cmn'
INCLUDE  'include\tg.cmn'

REAL, INTENT(IN) :: perk1(2*idimnutz),perk2(2*idimnutz),nutzprz(2*idimnutz),	&
		    klupor,xanf(idimanf), &
                    skap,dt


REAL, INTENT(INOUT) :: m_f1gws(idim),tgwzu(idim),xbil(21),           &
		       sperk,sperk2,m_bil(jdim,20),m_flaech(jdim,4)

REAL, INTENT(OUT) :: nw_gws(jdim)

INTEGER, INTENT(IN) :: anzelem,idif,ntg,nanzanf1,nanff,ivor,iz,	&
		       ngws,nqb,ngw,nqtg,naja20,idatsa20,nanfstd(idimanf),      &
                       nanf(idimanf)

INTEGER, INTENT(INOUT) :: nanzanf

CHARACTER, INTENT(IN) :: pns

REAL :: sqb,sqent,sqentp,sqgw,stgw,sgwzu,sum1,sperk1,				&
        flaechgws,flaechtg,xerr,xerr2,gws1,gws2,qkaps1,qkaps2,qent1,qent2,      &
        fnsum,fakqbm,fakqbm_gw,qperk1,qperk2,qgwzu1,qgwzu2,qb1,qb2,qgwab1,      &
        qgwab2,qgw(idim),dh,hdif,hvol_0,hvol_idif,ybil,hgws(0:idim),xnutzsum


INTEGER :: nn,i,itl,t,i1,i2,i3,isv,isstd,ttemp,ijahr,imona,itag,sjahr,smona,    &
	   stag,sstd,sdatum,ianf,idatum

LOGICAL :: outres

! Inizialisierung
sperk=0.
sperk1=0.
sperk2=0.
sqb=0.
sqent=0.
sqentp=0.
sqgw=0.
stgw=0.
sgwzu=0.
sum1=0.
flaechgws=0.
flaechtg=0.
fakqbm=0.
fakqbm_gw=0.
itl = 0
!c setzen des Anfangswertindex
ianf = 1
sstd = 0
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	vorbereitung  g r u n d w a s s e r z u f l u s s
gwzu: DO i=1,itlmax
   IF(itlz(i) == ntg) THEN
      itl=i
      EXIT gwzu
   ENDIF
END DO gwzu

!c prüft ob das TG ausgegeben werden soll
outres = .FALSE.
tgres : DO i = 1,ingmax
   IF (ntg == ntgout(i)) THEN
       outres = .TRUE.
       EXIT tgres
   ENDIF
ENDDO tgres

! berechnet die Flaeche des Grundwasserleiters; muss dann wohl ausserhalb berechent werden
DO nn=1,anzelem
   flaechgws=flaechgws+nutzprz(nn)*m_f1gws(nn)*rtr
   flaechtg=flaechtg + nutzprz(nn)*(1.-m_f1gws(nn)+1.-rtr)
END DO

! Wieso diese Bedingung
IF(flaechgws < 0.000001) THEN
   WRITE(nerr,6901) rtr

   call writeLogIntReal(6, &
        &'Flaeche Grundwasserleiter= 0.! Es wird gesetzt: Aufteilungsfaktor GW * FlaecheTG (&gt; 0.1 * FlaecheTG).',&
        & 'Area of GW head =0.! Area will be automatically set as rtr*totalAreaSubCatchment (&gt; 0.1 * totalAreaSubCatchment)', &
        & '','Teilgebiet',ntg,'Aufteilungsfaktor GW',rtr,'gwsp')


   IF(rtr < 1) rtr=0.1
   DO nn=1,anzelem
      m_f1gws(nn)=rtr
   END DO
   rtr=0.
   flaechgws = 0.1*flaechtg
   flaechtg  = 0.9*flaechtg
ENDIF

! Fehlerprüfung: ist die aktive Grundwasserleiterflaeche gleich der TG-Flaeche
IF( ABS(flaechgws+flaechtg-1.) > 0.001 ) THEN
   WRITE(nerr,9931)flaechgws,flaechtg

   call writeLogIntRealReal(7,&
        &'Fehler bei Aufteilung der Hydrotopflaechen auf aktiven Grundwleiter und Tiefengrundwasserleiter!', &
        & 'Error allocating the hydrotop area on active groundwater layer and deep groundwater layer!','','Teilgebiet',ntg,&
        & 'flaechgws',flaechgws,'flaechtg',flaechtg,'gwsp')

   xnutzsum=0.
   DO nn=1,anzelem
      xnutzsum=xnutzsum+nutzprz(nn)
   ENDDO
   WRITE(nerr,9933)xnutzsum

   call writeLogIntReal(7,'Summe der Hydrotopflaeche entspricht nicht der Gesamtflaeche des Teilgebietes!', &
       & 'Hydrotops do not cover completely the total area of the subcatchment!','','Teilgebiet',ntg,'xnutzsum',xnutzsum,'gwsp')

   call writeFooter()
   STOP
ENDIF

flaechgws=flaechgws*fnsum
flaechtg=flaechtg*fnsum
fakqbm_gw=flaechgws/(3.6*dt)
fakqbm=fnsum/(3.6*dt)
hgws(0)=nw_gws(isv)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	g r u n d w a s s e r s p e i c h e r
!c
!c 	geaendert:
!c 	29.11.1999, alle groessen in mm bezogen auf die flaeche des c gws, daher perk1 anstelle von perk2
!c   	23.2.2000, grundwasserspeicher in mm bezogen auf flaeche flaechgws
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	    DO t=1,idif  ! Zeitschleife
                sperk1 = sperk1+perk2(t)
                qkaps2 = 0.
   		IF(t > 1) THEN
		   qperk1=qperk2
		   qgwzu1=qgwzu2
	           qent1=qent2
		   qb1=qb2
	           qgwab1=qgwab2
   		ELSE
      		   qperk1=perk2(t)
		   qgwzu1=0.
		   qb1=0.
		   qgwab1=0.
      		   IF(fakqbm_gw.lt.0.0000001) THEN
         	      qent1=0.
		   ELSE
         	      qent1=gwsent/fakqbm_gw
	              IF(qent1.gt.0.) THEN
         	         write(nres,10102)gwsent
     	    	         10102  format(' entnahme gws [qbm/s]:', f12.3)
         	      ENDIF
      		   ENDIF
	  	ENDIF
  		gws1=hgws(t-1)
		qperk2=perk2(t)
		IF(itl == 0)THEN
		   IF(t == 1) write(nres,'(a)') ' kein zufluss grundwasserleiter '
  		ELSE
     		   qgwzu2=qgwzu(itl,t)/fakqbm_gw
		   sum1=sum1+qgwzu(itl,t)
                   IF(t == idif) write(nres,*) sum1, ' -> zufluss grundwasserleiter in ',ntg
		ENDIF
                sperk2=sperk2+perk2(t)
       		sperk=sperk+perk1(t)
		sgwzu=sgwzu+qgwzu2
		!c  entnahme festlegen, qent [mm] pro zeitschritt
		qent2=gwsent/fakqbm_gw
		sqentp=sqentp+qent2

  		CALL gwsp_st(dt,t,qkaps1,qkaps2,                  &
       	       	             qperk1,qperk2,qgwzu1,qgwzu2,         &
		             qb1,qgwab1,qent1,gws1,               &
		             qb2,qgwab2,qent2,gws2)
                !WRITE (*,*) qb1,qb2,gws1,gws2,fakqbm_gw
		!ccccccccccccccccc
		!c
		!c  fehlerkorrektur
		!c  ohne qkap,qentqgwab
		!c

  		qkaps1=qkaps2
		sqb=sqb+qb2
		sqent=sqent+qent2
		sqgw=sqgw+qgwab2
		stgw=stgw+tgwzu(t)
		qb(t)=qb2*fakqbm_gw
		qgw(t)=qgwab2*fakqbm_gw
		hgws(t)=gws2

		!c pauschaler verlust in hoehe von 1-klupor im teifengrundwasserleiter
		!c tgwzu in mm bezogen auf die gesamtflaeche (nat) des TG
		!c transformation> tgwzu von mm nach mm/h fuer faltung mit isov
		!c	dort: multip. mit gesamtflaeche TG --> qbm
		tgwzu(t)=tgwzu(t)*klupor
		tgwzu(t)=tgwzu(t)/dt




		!c SK/EP 28.02.03
		!c Datenausgabe Anfangswerte LZSIM
		!c Das Anfangsdatum des Zykluses 8 stellig wird in Jahr, Monat und Tag gesplittet, um die
		!c Funktion cdatum anzuwenden.
		!JH 28.09.2004 Anfangswerte werden auch aus einer Kurzzeitsimulation gespeichert.
		!Hierzu musste das Format des *.lzs-Files geaendert werden, um die Stunden hinzuzufügen.
		!Abfrage (If-Schliefe) um lz-kz erweitert.
       		IF (isim == 0) THEN                                                      ! Langzeitsim.
	           IF(nanf(1) == idatsa20.and.nanzanf.eq.0) THEN
        	      ijahr=(nanf(1)/10**4)
	              imona=((nanf(1)-ijahr*10**4)/100)
        	      itag =(nanf(1)-ijahr*10**4-imona*100)

		!c SK/EP 28.02.03
		!c Die do Variable t: do t=1, idif (Anzahl der Zeitschritte im Zyklus) muß neu initialisiert werden,
		!c da keine do-Variable übergeben werden darf. Die Funktion cdatum setzt idatum für jeden Zeitschritt
		!c neu, damit so in der lzsim alle Anfangswerte herausgeschrieben werden können.
             	      ttemp=t
	              CALL cdatum (itag,imona,ijahr,ttemp)                         ! 1 Tag = 1 Simulationsschritt t weiter
        	      idatum=ijahr*10**4+imona*10**2+itag

     	     	      WRITE(nanff,'(i8,2x,a2,a2,2x,a4,1x,a4)') idatum,'00',' h','   1','gwsp'

		      WRITE(nanff,'(i4,f9.2,f9.3)')1,hgws(t),qb(t)

        	   ELSEIF (nanzanf > 0.AND.t >= xanf(ianf)) THEN

		      WRITE(nanff,'(i8,2x,a2,a2,2x,a4,1x,a4)') nanf(ianf),'00',' h','   1','gwsp'

		      WRITE(nanff,'(i4,f9.2,f9.3)')1,hgws(t),qb(t)

	     	      ianf=ianf+1
		      IF(ianf > nanzanf) nanzanf=0
        	   ENDIF
       		ELSEIF(pns /= 's') THEN                                          ! Kurzzeitsimulation natürlicher Niederschlag
          	   IF (ianf <= nanzanf1) THEN
        	      IF (t == 1) THEN
		         sjahr = naja20
                   	 smona = namo
		         stag = natg
                	 sstd = nast
		      END IF
	              sstd = sstd + INT(dt)
		      IF (sstd >= 24.0) THEN
                         sstd = sstd - 24
		         CALL cdatum (stag,smona,sjahr,1)
	              ENDIF
             	      sdatum = sjahr*10**4 + smona*10**2 + stag

		      isstd=INT(sstd)
             	      IF(nanf(ianf) == sdatum.AND.nanfstd(ianf) == isstd) THEN
	                 WRITE(nanff,'(i8,2x,i2,a2,2x,a4,1x,a4)')nanf(ianf),nanfstd(ianf),' h','   1','gwsp'
            		 WRITE(nanff,'(i4,f9.2,f9.3)')1,hgws(t),qb(t)
		         ianf = ianf + 1
                	 IF (ianf > nanzanf1) THEN
                   	    CLOSE(nanff)
	                 END IF
             	      ENDIF
		   ENDIF
	        ENDIF
	    ENDDO




!cccccccccccccc
!c
!c  fehler bodenspeicher
!c
dh  = hgws(0) - hgru
hdif= hgro-hgru

IF(hgws(0) <= hgru) THEN
   hvol_0 = hgws(0)
ELSEIF(hgru < hgws(0).and.hgws(0) < hgro) THEN
   hvol_0 = (hgws(0)-0.5*dh*dh/hdif)
ELSEIF(hgws(0) >= hgro) THEN
   hvol_0 = (hgru+hgro)*0.5
ENDIF

dh  = hgws(idif) - hgru

IF(hgws(idif) <= hgru) THEN
   hvol_idif = hgws(idif)
ELSEIF(hgru < hgws(idif).and.hgws(idif) < hgro) THEN
    hvol_idif = (hgws(idif)-0.5*dh*dh/hdif)
ELSEIF(hgws(idif) >= hgro) THEN
    hvol_idif = (hgru+hgro)*0.5
ENDIF

xerr2=(hvol_0-hvol_idif)*pors*1000.+sperk2+sgwzu-sqgw-sqb-sqent
xerr=(hgws(0)-hgws(idif))*pors*1000.+sperk2+sgwzu-sqgw-sqb-sqent

IF (nerr /= 0) THEN
   if (ABS(xerr2)>1.0) then
      write (nerr,'(a,f10.4)')' xerr gws-speicher2 : ', xerr
      write (nerr,'(a,f10.4)')' xerr2 gws-speicher : ', xerr2
      call writeLogIntRealReal(6, 'Bilanzierung in Grundwasserberechnung nicht korrekt (Grenzkriterium Bilanz > 1.0)!',&
              & 'Error on groundwater calculation!',&
              & '','Teilgebiet',ntg,'Bilanz',xerr2,'Grundwasserabstrom + Basisabfluss ',sqgw+sqb,'gwsp')
   end if
END IF

IF(ABS(xerr2) > 0.001.and.(sqgw+sqb) > xerr2) THEN
   xerr2=(sqgw+sqb+xerr2)/(sqgw+sqb)
   sqgw=0.
   sqb=0.
   DO i=1,idif
     qb(i)=qb(i)*xerr2
     qgw(i)=qgw(i)*xerr2
     sqgw=sqgw+qgw(i)
     sqb=sqb+qb(i)
   END DO
   sqgw=sqgw/fakqbm_gw
   sqb=sqb/fakqbm_gw
   xerr2=(hvol_0-hvol_idif)*1000.*pors+sperk2+sgwzu-sqgw-sqb-sqent
   IF (nerr /= 0) THEN
       if (ABS(xerr2)>0.001) then
          write (nerr,'(a,f10.4)')                      &
                ' xerr2 gws-speicher nach korrekt. : ', xerr2

          call writeLogIntReal(6, 'Fehler nach Korrektur im Grundwasserspeicher!','Error on groundwater calculation!',&
               &'','Teilgebiet',ntg,'Bilanz',xerr2,'gwsp')
       end if
   END IF
endif

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c	weitergabe abfluss grundwasser an unterhalb liegende TG
!c		(in qbm/s)
!c

DO 7000 i1=1,igwzu
  DO i2=1,itlmax
	IF(itlz(i2) == ngwzu(i1)) THEN
	   sum1=0.
	   DO i3=1,idif
	      qgwzu(i2,i3)=qgwzu(i2,i3)+qgw(i3)*gwwi(i1)
	      sum1=sum1+qgw(i3)*gwwi(i1)
              IF( i3 == 1 ) WRITE (nres,1) ntg,itlz(i2),gwwi(i1)
              1 FORMAT('TG:',i8,' -> TG:',i8,/&
              		'Prozent:',f4.2)
	   END DO
	   GOTO 7000
	ENDIF
  END DO
7000  continue

!c	speichern der endwerte = anfangswerte zyklus iz+1
nw_gws(isv)=hgws(idif)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c     a u s g a b e
!c     gws.dat
!c     qbs.dat
!c     qgw.dat
!c     tg1.dat
!c
IF( outres ) THEN
   IF(ngws.gt.0) THEN
      WRITE(ngws,'(/3(i8)/)') ntg,iz,idif
      WRITE(ngws,'(10(f8.3,1x))')(hgws(i),i=1,idif)
    ENDIF

   IF(nqb.gt.0) THEN
      WRITE(nqb,'(/3(i8)/)') ntg,iz,idif
      WRITE(nqb,'(10(f8.3,1x))') (qb(i),i=1,idif)
   ENDIF

   IF(ngw.gt.0) THEN
      WRITE(ngw,'(/3(i8)/)') ntg,iz,idif
      WRITE(ngw,'(10(f8.3,1x))') (qgw(i),i=1,idif)
   ENDIF

   IF(nqtg.gt.0) THEN
      WRITE(nqtg,'(/3(i8)/)') ntg,iz,idif
      WRITE(nqtg,'(10(f8.3,1x))') (tgwzu(i)*fakqbm,i=1,idif)
   ENDIF
ENDIF
!ccccccccccccccc
!c
!c	bildschirm-ausgabe zusammenfassung

write(nres,9400) sperk,sperk2,stgw*klupor,stgw*(1.-klupor),       &
 		sperk-sperk2-stgw

ybil=sperk2+sgwzu-sqb-sqgw-skap-sqent-(hgws(idif)-hgws(0))*1000.*pors

write(nres,9401) sperk2,sgwzu,sqb,sqgw,skap,sqent,sqentp,          &
		(hgws(idif)-hgws(0))*1000.*pors,ybil

write(nres,9402) flaechgws,flaechtg,fnsum

! Teilgebietsbilanz
if(iz.gt.ivor) then
   m_bil(isv,10)=m_bil(isv,10)+sperk
   m_bil(isv,11)=m_bil(isv,11)+sperk1
   m_bil(isv,12)=m_bil(isv,12)+stgw*klupor
   m_bil(isv,13)=m_bil(isv,13)+stgw*(1.-klupor)
   m_bil(isv,14)=m_bil(isv,14)+sgwzu
   m_bil(isv,15)=m_bil(isv,15)+sqb
   m_bil(isv,16)=m_bil(isv,16)+sqgw
   m_bil(isv,17)=m_bil(isv,17)+skap
   m_bil(isv,18)=m_bil(isv,18)+sqent
   m_bil(isv,20)=m_bil(isv,20)+(hgws(idif)-hgws(0))*1000.*pors
else
    m_flaech(isv,1)=flaechgws
    m_flaech(isv,2)=flaechtg
endif

! Gesamtbilanz
xbil(9)=xbil(9)+sperk2*flaechgws
xbil(8)=xbil(8)+stgw*fnsum
xbil(16)=xbil(16)+sqb*flaechgws
xbil(17)=xbil(17)+stgw*fnsum*(1.-klupor)
xbil(20)=xbil(20)+skap*flaechgws
xbil(21)=xbil(21)+sqent*flaechgws

xbil(13)=xbil(13)+flaechgws
xbil(14)=xbil(14)+flaechtg

return
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c              FORMATE
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

9400  format(/' aufteilung perkolation: ',/                         &
               ' perkolation bodenspeicher:     ',f10.1,' mm'/,     &
               ' abgabe grundw.speicher:        ',f10.1,' mm'/,      &
               '        tiefengrundw.leiter:    ',f10.1,' mm'/,     &
               '        verlust tiefengw.leiter:',f10.1,' mm'/,     &
               ' fehler:                        ',f10.1,' mm')

9401  format(/' bilanz grundwasserspeicher: ',/                     &
               ' zufluss perkolation:           ',f10.1,' mm'/,      &
               '        grundw. oberh.l.TG:     ',f10.1,' mm'/,      &
               ' abgabe basisabfluss:           ',f10.1,' mm'/,      &
               '        grundw. unterh.l.TG:    ',f10.1,' mm'/,      &
               '        kapilarer aufstieg:     ',f10.1,' mm'/,      &
               '        entnahme GWS:          ',f10.1,' mm'/,       &
               '     pot.entnahme GWS:          ',f10.1,' mm'/,      &
               ' speicherinhaltsaenderung:      ',f10.1,' mm'/,      &
               ' fehler:                        ',f10.1,' mm')

9402  format(/' flaechenanteile: ',/                                &
               '   grundwasserleiter:           ',f10.3,' qkm'/,     &
               '   tiefengrundwasserleiter:     ',f10.3,' qkm'/,     &
               '   gesamtflaeche:               ',f10.3,' qkm')

6901  format(/' Subroutine Boden: flaeche grundwasserleiter = 0. !',/ &
       ' flaeche wird automatisch als "rtr*gesamtflaeche" gesetzt,'/  &
       ' mindestens aber "0.1 * gesamtflaeche".',/                    &
       ' rtr =    ', f6.3)

9931  format(/'xxx fehler bei aufteilung der hydrotopflaechen auf ',/     &
               'xxx aktiven grundw.leiter und tiefengrundwasser.leiter'/  &
               'xxx anteil aktiven grundw.leiter: ',f10.4,/               &
               'xxx anteil tiefengrundw.leiter: ',f10.4)

9933  format(/'xxx hydrotope ueberdecken nicht vollstaendig die ',   &
               'gesamtflaeche des TG?'/                              &
               ' hydrotopanteil: ',f10.5)

end
