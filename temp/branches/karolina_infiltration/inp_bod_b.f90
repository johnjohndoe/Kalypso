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

	subroutine inp_bod_b(isim,iz,namproj,ilen,                    &
               anzbod,kennbod,anzelem,kenelem,                        &
               anzlay,m_wep,m_nfk,m_bmax,m_tief,                      &
               m_cin,m_cex,m_kf,m_retlay,nw_boanf,m_typ,                   &
               isv,fko,bmax,cinh,cind,cex,retlay,banf,slash,	&
               m_alpha,m_n,m_l,m_vmacro,m_r,vg)

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!  29.10.2002   FN			Wenn Bodenarten nicht richtig aus der Hydrotop-datei
!					eingelesen werden, kommt es jetzt direkt zu einer Fehler
!					meldung. Vorher wurde die do-Schleife (Zeile 109 bis 185)
!					nicht durchlaufen und es kam erst im Anschluß zu der
!					Fehlermeldung (Zeile 243), welche nicht eindeutig auf
!					die Fehler in der Hydrotop-Datei hinwies.
!					Zeilen hinzugefügt: Zeile 64 bis Zeile 72

!  03.11.2003   EP         		Fehler in der Berechnung des CIN und CEX-Wertes
!					festgestellt. Beide Parameter wurden bislang in der
!					Einheit [mm/dm/h] gefuehrt. In der SUBROUTINE BODF_N
!					werden diese beiden Parameter, die dort cin und cex
!					heißen in der Einheit [mm/h] erwartet.
!                       		Der Fehler liegt darin, dass xbfm (max. Bodenfeuchte)
!					nur in der Einheit mm je dm Schichtdicke definiert ist.
!                       		Diese Groesse muss daher noch mit der tatsaechlichen
!					Schichtdicke multipliziert werden.
!                       		Da diese Aenderung weitreichende Auswirkung auf das
!					Berechnungsergebnis hat, gibt es Inkonsistenzen für alte
!					geeichte Modelle. Daher wird zunaechst an der alten, wenn
!					auch fehlerhaften Definition von xcin und xcex
!					festgehalten.
!                       		Bei neuen Modellanwendungen hingegen ist die neue
!					Definition zu verwenden.

!  21.10.2004   SK,EP 			Es wird für die folgenden Modellaufstellung jetzt auf die
!					richtige Definition von cin und cex umgestellt. Sollten
!					alte Modelle gerechnet werden, so muss diese Änderung
!					dann rückgaengig gemacht werden!


! 22.08.2006    KV                      Addition of van Genuchten parameter alpha, n (from bod_art)

! 09.07.2008    KV                      Add new file for boden_vg.dat to separate the layer data from
!                                       macropore data


!***************************************BESCHREIBUNG********************************************

!***************************************EIN-/AUSGABE********************************************

!******************************************VARIABLEN********************************************
USE generic_LOGGER
USE Units
USE generic_LOG_FILE
USE generic_NAConstants
!c	uebernahme der bodenparameter aus datei boden_n.par
!c
IMPLICIT NONE

include	'include\param.cmn'

1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: inp_bod_b.f90                  ****',/,10x,     &
      &'****     ZYKLUS:',I6,'                           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (inp_bod_b.f90)           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')



INTEGER:: isv,iz,isim

REAL:: 		   m_wep(maxlay,idimnutz2),m_nfk(maxlay,idimnutz2),     &
               	   m_bmax(maxlay,idimnutz2),m_cin(maxlay,idimnutz2),	&
                   m_cex(maxlay,idimnutz2),m_retlay(maxlay,idimnutz2),  &
                   m_tief(maxlay,idimnutz2),m_kf(maxlay,idimnutz2)


REAL :: m_bf0(maxlay,idimnutz2)

CHARACTER(LEN=10):: kennbod(idimnutz2)
CHARACTER(LEN=8):: m_typ(maxlay,idimnutz2)
CHARACTER(LEN=10):: char3

INTEGER, INTENT(OUT) :: anzlay(idimnutz2)


INTEGER :: anzlayy

REAL:: fko(maxlay),bmax(maxlay),cinh(maxlay),                  &
       cind(maxlay),cex(maxlay),retlay(maxlay),                &
       banf(maxlay)

REAL:: nw_boanf(jdim,2*idimnutz,maxlay)

INTEGER:: ilen,anzbod
INTEGER:: nbo,nb,ilenb,ilent,ndebug

INTEGER:: anzelem, kenelem(2,2*idimnutz)
!SK 21.10.2003 Änderung der Dimension von 50 auf idimnutz
REAL:: typwp(idimnutz2),typfk(idimnutz2),typbfm(idimnutz2),    &
       typkf(idimnutz2),typbf0(idimnutz2),typalpha(idimnutz2), &
       typn(idimnutz2)

REAL :: alpha(idimnutz2),n(idimnutz2),l(idimnutz2),vmacro(idimnutz2),r(idimnutz2)
CHARACTER(LEN=80):: namvgp,nambodvgp,nambod_dummy
INTEGER:: bvg
REAL:: m_alpha(maxlay,idimnutz2),       &
       m_n(maxlay,idimnutz2),m_l(maxlay,idimnutz2),m_vmacro(maxlay,idimnutz2),m_r(maxlay,idimnutz2)
real :: xalpha,xn,xl,xvmacro,xr
LOGICAL::lexist,llexist
INTEGER::vg

INTEGER:: typanz

CHARACTER(LEN=8):: typchar(idimnutz2)
CHARACTER(LEN=8):: typkap(idimnutz2),ctypkap

CHARACTER(LEN=120):: textin
CHARACTER(LEN=80):: namproj,nambod,nambtyp,text
CHARACTER(LEN=14):: text14
CHARACTER(LEN=10):: text6
CHARACTER(LEN=1):: slash


INTEGER :: ju0gfu,ierr,ianz,lastchar,itg,ilay,nn,ityp,ilt,iosta
REAL :: xret,xtief,xwp,xfk,xbfm,xkf,xbf0,xcex,xcind,xcinh,xlpr,xbfmn,xcin


!******************************************ANWEISUNGEN******************************************


ndebug = -1
IF (DEBUG) THEN
    CALL getLogFile( ndebug )
    WRITE (ndebug,1)iz
END IF

!cccccccccccccccccccccccccccccccccccc
!FN     ------------------------------------------------------------
!FN     Änderung 29.10.2002

        do 101 nb=1, idimnutz2
        m_bmax(1,nb)=0.0
101     continue

!call u1null( anzlay,idimnutz2)

!FN     Ende Änderung 29.10.2002
!FN     ------------------------------------------------------------

	nbo=ju0gfu()
	typanz=0

	nambod=namproj(1:ilen)//'hydro.top'//slash//'boden.dat'
        nambodvgp=namproj(1:ilen)//'hydro.top'//slash//'boden_vg.dat'
	nambtyp=namproj(1:ilen)//'hydro.top'//slash//'bod_art.dat'
        namvgp=namproj(1:ilen)//'hydro.top'//slash//'bod_vg.dat'


 	!09.07.2008. KV. Proof if boden_vg file (with macropores parameters) exists.
        !Only used one file either boden.dat or boden_vg.dat.

        inquire (FILE=nambodvgp,EXIST=llexist)
        if (llexist) then
            nambod_dummy=nambodvgp
        else
            nambod_dummy=nambod
        END if

	open (nbo,iostat=ierr,err=996,status='old',file=nambod_dummy)
	read(nbo,'(a)',err=996,end=6501) text
6500	read(nbo,'(a,i2)',IOSTAT=iosta,end=6501)text14(1:14)
	IF (iosta/=0) THEN
               	      WRITE (*,1000) text14
               	      1000 FORMAT (/1X, 'Fehler beim Einlesen der Bodendatei:', /, &
                            & 1X, 'letzte gelesene zeichen    ',a14  )
        END IF
	if(text14(1:1).eq.' ') goto 6501
	if(text14(1:1).eq.'/'.or.text14(1:1).eq.'#') goto 6500
	read(text14(11:14),*) ianz
	text6=' '
	text6(1:10)=text14(1:10)
	call lcase(text6)
	ilent=lastchar(text6)
	if(ilent > 10 )ilent=10
	do 301 nb=1,anzbod
		ilenb=lastchar(kennbod(nb))
		if(text6(1:ilent) == kennbod(nb)(1:ilenb)) THEN
                   goto 302
                ENDIF
301	continue

	nb=0

302	continue

	itg=1
	do 303 ilay=1,ianz

		textin(1:120)=''
		read(nbo,'(a)')textin(1:120)
                char3=' '
		char3(1:8)=textin(1:8)


		if(textin(31:50).eq.'                    ')then

                      if (llexist) then
			read(textin(9:),*,err=9991)xtief,xret,xvmacro,xr
                      else
			read(textin(9:),*,err=9991)xtief,xret
                      end if

		      if(nb.eq.0) goto 402
		      if(typanz.le.0) call inp_bod_typ(typanz,nambtyp,typchar,typkap,typwp,typfk,&
                               typbfm,typkf,typbf0)


                      inquire (FILE=namvgp,EXIST=lexist)
                      if (lexist) then
                         call inp_vgp_parm(typanz,namvgp,typchar,alpha,n,l,ierr)
                         vg=1
                      else
                         vg=0
		      END if

 		      do 401 ityp=1,typanz

			ilt=lastchar(char3)
			if(char3(1:ilt).eq.typchar(ityp)(1:ilt))then
			   ctypkap=typkap(ityp)
			   xwp=typwp(ityp)
                           xfk=typfk(ityp)
                           xbfm=typbfm(ityp)
			   xkf=typkf(ityp)
			   xbf0=typbf0(ityp)

                           if(vg .eq. 1) then
                              xalpha=alpha(ityp)
       	       		      xn=n(ityp)
                              xl=l(ityp)
                           else
                              xalpha=0.
                              xn=0.
                              xl=0.
                           end if

			   goto 402
			endif

401	              continue

		      goto 9992
402		      continue

		else
			write(*,10402)
10402	format('xxx eingabemoeglichkeit nicht aktuell.'/            &
               'xxx korregiere inp_bod_b.')
			read(textin(7:),*,err=9991)xtief,xret,xwp,   &
                               xfk,xbfm,xcinh,                       &
                               xcind,xcex,xbf0

                        call writeFooter()
			stop
		endif


		if(nb.gt.0) then
			anzlay(nb)=ianz
			if(bmax(ilay).gt.0.01) then
				itg=ilay
			endif
                        xlpr = xtief*(xbfm-xfk)
			xbfmn = xtief*(xbfm-xwp)
                        xcin=xkf*xbfm/xbfmn/24.*xtief
                        xcex=xkf*xbfm/xlpr/24.*xtief
			m_tief(ilay,nb)=xtief
			m_wep(ilay,nb)=xtief*xwp
                      	m_kf(ilay,nb) = xkf
                        m_nfk(ilay,nb)=xtief*(xfk-xwp)*fko(itg)

                       	m_bmax(ilay,nb)=xbfmn*bmax(itg)

                        m_alpha(ilay,nb)=xalpha
                        m_n(ilay,nb)=xn
                        m_l(ilay,nb)=xl

 !09.07.2008 KV. If boden_vg.dat exists, xvmacro & xr are read, otherwise they are set to zero.

                        if (llexist) then
                            m_vmacro(ilay,nb)=xvmacro
                            m_r(ilay,nb)=xr
                        else
                            m_vmacro(ilay,nb)=0.
                            m_r(ilay,nb)=0.
       		        END if

                         !write (nres,*)'m_vmacro',m_vmacro(ilay,nb)
                         !write (nres,*)'m_r',m_r(ilay,nb)


                        if (m_vmacro(ilay,nb) .gt. 0.) then
                            if(isim.eq.1) then
                               m_bmax(ilay,nb)=m_bmax(ilay,nb)*cinh(itg)
			   																      else
			       m_bmax(ilay,nb)=m_bmax(ilay,nb)*cind(itg)
			    endif
                        end if
!-------------------------------------------------------------------------------------------

			if(isim.eq.1) then
				m_cin(ilay,nb)=xcin*cinh(itg)             
			else
				m_cin(ilay,nb)=xcin*cind(itg)
			endif
			m_cex(ilay,nb)=xcex*cex(itg)
			m_retlay(ilay,nb)=xret*retlay(itg)

!ep 03.11.03   Korrektur der Anfangsbodenfeuchte mit bmax (Korrekturfaktor
!              für maximale Bodenfeuchte. Hierdurch ist sichergestellt,
!              dass Anfangsbodenfeuchte < max. Bodenfeuchte ist.
!              Nur relevant, wenn keine LZSIM-Dateien bei Kurzzeitsimulation
!              existieren oder synthetische Niederschlaege verarbeitet werden.

			m_bf0(ilay,nb)=xbf0*xbfmn*banf(itg)*bmax(itg)
			m_typ(ilay,nb)=ctypkap


		endif



303	continue
	goto 6500


!c   gemaesz nw-norm (vgl. nasim) sind cinh, cind und cex in der einheit
!c   [1/h] angegeben. fuer die weitere berechnung werden sie in [mm/h]
!c   benoetige und sind mit bmax zu multiplizieren



6501	close (nbo)

	do 312 nb=1,anzbod
		if(m_bmax(1,nb).le.0.001) goto 997
312	continue

!cccccccccccccccccccccccccccccc
!c
!c   langzeit>
!c	anfangswerte aus eingabedatei nur startwerte fuer iz=1
!c	sonst:uebernahme der endwerte des letzten zeitzyklus iz-1 

	if ( (isim == 1.and. nw_boanf(isv,1,1) <= 0.).or.                  &
           ( isim == 0 .and. iz == 1) ) then
	   do nn=1,anzelem
	      nb=kenelem(1,nn)
              anzlayy=anzlay(nb)
	      do ilay=1,anzlayy
		 nw_boanf(isv,nn,ilay)=m_bf0(ilay,nb)
              END do
           END do
	endif
!cccccccccccccccccccccccccccccc
!c
!c   kurzzeit>
!c	uebernahme der startwerte aus LZSIM-datei

	continue

IF (DEBUG) THEN
    WRITE (ndebug,2)
END IF


	return


!ccccccccccccccccccccccccccccc
!c
!c	fehlermeldungen
!c

996	write(nerr,906)nambod_dummy

906	format( 'xxx datenfile mit bodendaten KALYPSO-NA nicht gefunden!',/   &
               'xxx filename: ',a)

        call writeLogString(7,'Datenfile nicht vorhanden!','File not found!',nambod_dummy,'','','','','inp_bod_b')

        call writeFooter()
	stop

997	write(nerr,907)kennbod(nb),nambod_dummy

907	format( 'xxx in datenfile mit bodendaten KALYPSO-NA keine Angaben',/   &
               'xxx fuer bodenart: ',a,/                                 &
               'xxx filename:      ',a)

        call writeLogString(7,'Parameter fuer eine verwendetes Bodenprofil nicht gesetzt oder fehlerhaft!',&
             &'Data for used soil profil not found or with error!',nambod_dummy,'','','kennbod(nb)',kennbod(nb),'inp_bod_b')

        call writeFooter()
	stop

9991	write(nerr,9091)nambod_dummy,text6,char3

9091	format( 'xxx fehler in eingabeformat bodentyp/hydrotop',/      &
               'xxx datenfile:      ',a,/                              &
               'xxx bodenart:       ',a,/                              &
               'xxx bodentyp:       ',a)

        call writeLogString(7, 'Parametereingabe fuer eine verwendetes Bodenprofil fehlerhaft!', &
             &'Error in input format soil profil',nambod_dummy,'','','bodentyp',char3,'inp_bod_b')

        call writeFooter()
	stop

9992	write(nerr,9092)nambod,char3
        call writeLogString(7,'Parameter fuer eine verwendetes Bodenprofil nicht gesetzt!',&
             &'Data for used soil profil not found!',nambod_dummy,'','','bodentyp',char3,'inp_bod_b')

        call writeFooter()
	stop
9092	format( 'xxx keine parameter zum bodentyp gefunden',/          &
               'xxx datenfile:      ',a,/                              &
               'xxx bodentyp:      ',a)

	end


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c
	subroutine inp_bod_typ(typanz,nambtyp,                     &
                               typchar,typkap,typwp,typfk,         &
                               typbfm,typkf,typbf0)

        USE generic_LOGGER
	include	'include\param.cmn'

	real	typwp(idimnutz2),typfk(idimnutz2),typbfm(idimnutz2),                       &
               typkf(idimnutz2),typbf0(idimnutz2)

	character*8 typchar(idimnutz2),typkap(idimnutz2)
	character*80 nambtyp,text

	integer	typanz,nbt,ityp

 	nbt=ju0gfu()

       	open (nbt,iostat=ierr,err=996,status='old',file=nambtyp)

	read(nbt,*,end=6501)
	read(nbt,*,end=6501)
	read(nbt,*,end=6501)


	ityp=0  
6500	ityp=ityp+1
!SK 21.10.2003 Änderung der maximal möglichen Bodenarten ityp von fixem Wert auf Variable
!SK idimnutz2, die in der Param.cmn-Datei definiert ist
	if(ityp.gt.idimnutz2) goto 9906
	read(nbt,*,end=6501)typchar(ityp)(1:8),typkap(ityp),           &
                       typwp(ityp),typfk(ityp),typbfm(ityp),           &
                       typkf(ityp),typbf0(ityp)


	goto 6500
6501	close (nbt)

	typanz=ityp-1


	return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

996	write(*,906)nambtyp

906	format( 'xxx datenfile mit daten bodentyp nicht gefunden!',/      &
               'xxx filename: ',a)

        call writeLogString(7, 'Datenfile nicht gefunden!','File not found!',nambtyp,'','','','','inp_bod_typ')

        call writeFooter()
	stop

9906	write(*,9006)nambtyp

9006	format('xxx anzahl bodentyp ueberschreitet felddimensionierung!', &
               /'xxx erhoehe parameter idimnutx in param.cmn!',/          &
               'xxx filename:           ',a,/                             &
               'xxx erlaubte typanzahl: ',i6)

        call writeLogIntInt (7,'Anzahl der max. moeglichen Bodenprofile ueberschritten!',&
              & 'Amount of soil type exceeds max. number!',nambtyp,'',0,'idimnutz2',idimnutz2,'inp_bod_typ')

        call writeFooter()
	stop

	end
