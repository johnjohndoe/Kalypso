!     Last change:  JH    9 Mar 2007    3:53 pm

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



      subroutine gerinne(iz,dt,iko,iku,izsim,idif,issp,             &
                        nsv,nueb,nf,nquh,qz,nsv_e,nsv_p,nsv_b,nsh,    &
                       natg1,namo1,naja20,netg1,nemo1,neja20,         &
                       svmm,svend,ivor,xbilsp,                        &
                       slash,ianfkz,nanf,nanfstd,     &
                       nanzanf1,ngeruh)               					! JH 14.10.04 erweitert um ianfkz,nanf,nanfstd,nanzanf1,ngeruh

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!     ?		Wiebke Klauder (WK)	nur Umbenennung

!  26.11.04     Jessica Huebsch (JH)	Ausgabe der verschiedenen UH´s fuer das Gerinne beim
!					ersten Zyklus (directory out..., File ..._uh_ger.dat).

! 01.02.06      Jessica Huebsch (JH)    Anpassung der KM-Berechnung mit abflussabhängigem
!                                       Aufteilungsfaktor c.

!***************************************BESCHREIBUNG********************************************
!c    translationsmodell fuer das gerinne und den ueberschwemmungs-
!c    bereich (flood-routing mittels paralleler schwellenwertkaskade)
!c
!***************************************EIN-/AUSGABE********************************************
!c    qg (im commonblock) ist sowohl input- als auch output-variable:
!c                              input   abflusz am oberen knoten iko
!c                             output      "    am unteren   "   iku

!******************************************VARIABLEN********************************************
      USE generic_LOGGER
      USE Units      
      implicit logical(a-z)

	  include 'include\param.cmn'
      
      INTEGER, parameter 	:: kmdim 	= 5	! Anzahl angegebener Abflüsse mit zugehörigen KM-Parametern in *.ger (Abflussfälle)

! JH 08.10.2004


      include 'include\nad.cmn'
!     include 'include\anfdat.cmn'
! JH Ende


      include 'include.spi'
      include 'include.ger'


      INTEGER		:: izsim,ituh,ituhm,imax(2),iqmax,iqamax(2),iqzmax,idif,issp
      INTEGER		:: iqzfmx,iqzvmx,i,i1,iko,iku,izsim1(2),ilauf,i2,i3,iz
      INTEGER		:: nsv,nueb,i5,iuni,nf,nquh
      REAL	:: u(idim,5),t(idim),umin,umax(2),y,xi,dt,qag(idim)
      REAL	:: suu(2),suuh(2),fnorm(2),sqz,sqzf,gamf(kmdim),qaend
      REAL:: qz(idim),qzu(2,idim)
      REAL 	:: sqzv,qzmax,qzfmax,qzvmax,dtz
      REAL 	:: fuqz,fuqzf,fuqzv,tdtz,sq,sqa(2),sqg,qmax,qamax(2)
      REAL 	:: tdta(2),fuq,fuqa(2),qa(idim),qau(2,idim)
      REAL 	:: y1,y2,h5,sumu
      REAL	:: h,h1,h2,h3,dum1
      INTEGER		:: natg1,namo1,naja20,netg1,nemo1,neja20
      REAL 	:: svmm(12),svend
      INTEGER		:: nsv_e,nsv_p,nsv_b,nsh
      INTEGER		:: istart
      REAL 	:: qauf
      CHARACTER(LEN=1)	:: slash
      REAL 	:: alpha
      REAL	:: xbilsp(20)
      INTEGER		:: ivor,ier,nemn,namn

!JH 14.10.04 Anfangswerte
      INTEGER		:: nanzanf1,ianfkz(nanzanf1),nanf(nanzanf1),nanfstd(nanzanf1),i7,i8,ngeruh
      REAL	:: qauanf(2,nanzanf1,idim),qzuanf(2,nanzanf1,idim),qaanf(nanzanf1,idim),qaganf(nanzanf1,idim)
      REAL 	:: qaufanf
      CHARACTER(LEN=1)	:: char_i1, char_i5
!      INTEGER		:: nqaanf
!      CHARACTER(LEN=80) :: nameqaanf,nameqaganf,nameqa
!      INTEGER		:: nqaganf
!      INTEGER		:: nqa
      LOGICAL 		:: extrapolation_up


!******************************************ANWEISUNGEN******************************************

!  Verzweigungsanweisung in Abhaengigkeit von iart (Strangart)
      if (iart.EQ.1) THEN

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                                                              c
!              Abflussverzögerung mit Kalinin-Miljukov (iart = 1, realer Gerinnestrang)        c
!                                                                                              c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      write(nres,'(/a)')' Abflussverzoegerung mit Kalinin-Miljukov Parametern'
!c  variableninitialisierung

      do 1000 i=1,2
       umax(i) 	 = 0.
       suu(i) 	 = 0.
       suuh(i) 	 = 0.
       fnorm(i)  = 0.
       sqa(i) 	 = 0.
       qamax(i)  = 0.
       iqamax(i) = 0.
       tdta(i) 	 = 0.
       fuqa(i) 	 = 0.
1000  continue
      fuq 	= 0.
      ituh 	= 0
      ituhm 	= 0.
      sqz 	= 0.
      sqzf 	= 0.
      sqzv 	= 0.
      iqmax 	= 0
      iqzfmx 	= 0
      iqzvmx 	= 0
      iqzmax 	= 0
      qzmax 	= 0.
      qzfmax 	= 0.
      qzvmax 	= 0.
      dtz 	= 0.
      fuqz 	= 0.
      tdtz 	= 0.
      fuqzf 	= 0.
      fuqzv 	= 0.
      sq 	= 0.
      sqg 	= 0.
      qmax 	= 0.
      h 	= 0.
      qaend 	= 0.
      alpha     = 0.

      extrapolation_up = .FALSE.
      nurflu = .false.

      call u1null(t,idim)
      call u1null(qa,idim)
      call u1null(qag,idim)

      do i=1,idim
       qau(1,i)	= 0.
       qzu(1,i)	= 0.
       qau(2,i)	= 0.
       qzu(2,i)	= 0.
       do i2 = 1,5
        u(i,i2) = 0.
       END do
      END do



!c    zeitschritt dtz: die fuelle wird in [hm**3] angegeben
!c                     dtz = dt [h] * 3600 [s] / 10**6

      dtz = dt * 3.6 / 1000.

!-----------------------------------------------------------------------------------------------
! Aufteilung der Zuflusswelle auf Gerinne und Vorland (?abzueglich Basisabfluss?)              -
!-----------------------------------------------------------------------------------------------
      do i=1,izsim

! Aufteilung in Gerinne- und Vorlandabfluss
!------------------------------------------
! JH: 04.01.2007
! qfmax (bordvoller Abfluss ist nicht erforderlich als 3ter Abfluss - Abflussaufteilung nach alpha - Aufteilungsfaktor)
!       IF (qz(i).LE.qfmax) THEN
!          alpha = 1.
!       ELSE IF (qz(i).GT.qfmax.AND.qz(i).LE.qrk(4)) THEN
!          alpha = c(3)+((c(4)-c(3))*(qz(i)-qrk(3))/(qrk(4)-qrk(3)))
       IF (qz(i).LE.qrk(1)) THEN
          alpha = 1.
       !Warnung ist am Ende der Zeitschleife, da hier nicht extrapoliert werden kann (km eigentlich nicht gültig)
       ELSE IF (qz(i).GT.qrk(1).AND.qz(i).LE.qrk(2)) THEN
          alpha = c(1)+((c(2)-c(1))*(qz(i)-qrk(1))/(qrk(2)-qrk(1)))
       ELSE IF (qz(i).GT.qrk(2).AND.qz(i).LE.qrk(3)) THEN
          alpha = c(2)+((c(3)-c(2))*(qz(i)-qrk(2))/(qrk(3)-qrk(2)))
       ELSE IF (qz(i).GT.qrk(3).AND.qz(i).LE.qrk(4)) THEN
          alpha = c(3)+((c(4)-c(3))*(qz(i)-qrk(3))/(qrk(4)-qrk(3)))
       ELSE IF (qz(i).GT.qrk(4).AND.qz(i).LE.qrk(5)) THEN
          alpha = c(4)+((c(5)-c(4))*(qz(i)-qrk(4))/(qrk(5)-qrk(4)))
       ELSE
          alpha = c(5)
          extrapolation_up=.TRUE.
       !Warnung ist am Ende der Zeitschleife, da hier nicht extrapoliert werden kann (km eigentlich nicht gültig)

       END IF
! JH: calc like old version, why minus qfmax?
!         qzu(2,i) = (qz(i) - qfmax) * (1-alpha)                          ! Abflussanteil auf dem Vorland
!         qzu(1,i) = qz(i) -qzu(2,i)                              ! Abflussanteil im Fluss
        qzu(1,i) = qz(i) * alpha                              ! Abflussanteil im Fluss
        qzu(2,i) = qz(i) * (1-alpha)                          ! Abflussanteil auf dem Vorland


! Bestimmung der Scheitelabfluesse sowie korrespondierender Zeitschritte
!-----------------------------------------------------------------------
     if(qz(i).gt.qzmax) then
          iqzmax = i                                    ! Zeitschritt des Scheitelabflusses Gesamt
          qzmax = qz(i)                                 ! Scheitelabfluss Gesamt
       end if
       if(qzu(1,i).gt.qzfmax) then
          iqzfmx = i                                    ! Zeitschritt des Scheitelabflusses Flussschlauch
          qzfmax = qzu(1,i)                             ! Scheitelabfluss Flussschlauch
       end if
       if(qzu(2,i).gt.qzvmax) then
          iqzvmx = i                                    ! Zeitschritt des Scheitelabflusses Vorland
          qzvmax = qzu(2,i)                             ! Scheitelabfluss Vorland
       end if




! Abflusssummen über alle Zeitschritte
!-------------------------------------
       sqz  = sqz  + qz(i)                              ! Summe des Gesamtabflusses
       sqzf = sqzf + qzu(1,i)                           ! Summe des Abflussanteils im Fluss
       sqzv = sqzv + qzu(2,i)                           ! Summe des Abflussanteils auf dem Vorland

      END do
      if (extrapolation_up) then
      	call writeLogIntReal(6,'Der Abfluss ueberschreitet den angegebenen max. Abfluss der Kalinin-Miljukov Parameter!',&
         	 & 'The discharge exceeds the max. discharge of the Kalinin-Miljukov parameters','','Strang',&
          	 & issp,'qmax',qrk(5),'gerinne')

        WRITE(nerr,9063)
      end if


! Bestimmung, ob Vorlandberechnung erforderlich ist
!--------------------------------------------------
       if(qzvmax.eq.0.)  nurflu = .true.

!JH 14.10.04 Für Anfagswerte qzuanf ab Anfangswert zu 0.0 setzen
      IF (isim .EQ. 1) THEN                             ! Kurzzeit
         IF (nanzanf1 .GT. 0) THEN                  	! Es sollen Anfangswerte geschrieben werden
              startwerte: DO i7 = 1,nanzanf1
                           setzeqzu: do i = 1,ianfkz(i7)
                                      qzuanf(1,i7,i) = qzu(1,i)
                                      qzuanf(2,i7,i) = qzu(2,i)
                                     end do setzeqzu
                           aufnull: do i8 = ianfkz(i7)+1,izsim        	! qzuanf für die Zuflüsse nach Anfangswert 0 setzen, damit diese keine Auswirkung auf den Anfangsabfluss haben
                                    qzuanf(1,i7,i8)= 0.0
                                    qzuanf(2,i7,i8)= 0.0
                                    end do aufnull
                          END DO startwerte
         END IF
      END IF
!JH Ende

! Trockenperiode (kein Zufluss-kein KM- zurück zum Hauptprogramm)
!----------------------------------------------------------------
!c    Falls kein Zufluss erfolgte (ausgedehnte Trockenperiode), kann der
!c    Null-Array des Gesamtabflusses direkt als (Null - )Ausflusswelle
!c    zuzueglich des eventuell bereits bestehenden Abflusses am unteren
!c    Knoten umgespeichert werden und mit der Berechnung im Hauptprogramm
!c    fortgefahren werden.

      if (iqzmax.eq.0) then
         return
      end if


! Scheitel einschliesslich Basisabfluss, Anstiegszeit, Umrechnung in Tage, Zuflussfuelle [hm**3]
!-----------------------------------------------------------------------------------------------
      tdtz = float(iqzmax) * dt                         ! Anstiegszeit [h]
      tdtz = tdtz/24.                                   ! Anstiegszeit [d]
      fuqz = sqz * dtz                                  ! Fülle Gesamtabfluss [hm**3]
      fuqzf = sqzf * dtz                                ! Fülle Flussschlauch [hm**3]
      fuqzv = sqzv * dtz                                ! Fülle Vorland [hm**3]


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c                                                                                             c
!c          B e r e c h n u n g   d e r   U e b e r t r a g u n g s f u n k t i o n            c
!c                                                                                             c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!    Unit Hydrograph aus charakteristischen Systemwerten, Ordinaten u(i) [1/dt h], i=dt [h]
!    Einmalig falls nur Gerinne-Translation, doppelt falls auch Zufluss im Ueberschwemmungsbereich


      if (nurflu) then
         ilauf = 1                                      ! Abfluss nur im Flussschlauch
      else
         ilauf = 2                                      ! Abfluss auch auf Vorland
      end if


!-----------------------------------------------------------------------------------------------
!                 S c h l e i f e    F L U S S S C H L A U C H - V O R L A N D                 -
!-----------------------------------------------------------------------------------------------
      gerinnevorland: do i1=1,ilauf

       ituh = izsim
       umin = 1.0e-06                                           ! Abbruchkriterium Minimalwert UH


       gammafunktion: DO i5=1,kmdim                            	! Abflussfälle (1-5)
!JH Ausgabe quh.dat noch nicht implementiert (*.konfig)
        if (nquh.gt.0) then
           write(nquh,7602)i5,rk(i1,i5),rn(i1,i5)               ! Ausgabe der K-M-Parameter in der Datei quh.dat (U n i t - H y d r o g r a p h  in m**3/s), falls vorhanden
        endif
7602    FORMAT(/ 1X,' Fall: ' ,i2,'  k: ',f8.4,'   n: ',f6.2)

!    Fehlerabfang fuer zu kleine, eingelesene Speicheranzahlen und deren Retentionskonstanten,
!    die in der Berechnung der Impulsantwort die Rechnerkapazitaet (e+30) ueberschreiten wuerden.
        IF (rk(i1,i5).GE.1.e-10) THEN
           call gamma(rn(i1,i5),gamf(i5),ier)              	! Berechnung der Gamma-Funktion gamf für das Argument rn
        ELSE IF(rk(i1,i5).GT.0.0) then                              ! Bei rk=0.0 sind keine Werte vorhanden (z.B. Vorland nicht ueberstroemt)
           call writeLogIntReal(5,'Die Speicherkonstante der KM-Parameter unterschreitet den Minimalwert (1.0E-10)!',&
         	 & 'Retentionconstants falls below the minimum value (1.0E-10)!','','Strang',&
          	 & issp,'Retentionskonstante k',rk(i1,i5),'gerinne')
        END IF							! Gammafunktion entspricht Fakultät des Argumentes Gammaf(x+1)=x!
       END DO gammafunktion

!JH 15.01.07 der 3te Abfluss ist nicht mehr Bordvoll, daher kann auch bei den Abflüssen 1-3 das Vorland betroffen sein.

!---------------------------------------------------------------------------------------------------------------
!     S c h l e i f e    A b f l u s s f a l l    ( a u s   * . g e r,   1 - 5 ) ,  B e r e c h n u n g   U H  -
!---------------------------------------------------------------------------------------------------------------

       unithydrograph: do i5 = 1,kmdim
       ! Wenn das Vorland nicht überströmt wird (r=0.0;k=0.0), kann/muss der UH nicht berechnet werden.
       if (rn(i1,i5).GT.0.0.AND.rk(i1,i5).GT.0.0) then
	umax(i1) = 0
	sumu = 0.
        y1 = rk(i1,i5)
        y2 = dt/10.
        y = amin1(y1,y2)                                	! intrinsic function amin1 liefert kleinsten Wert der angegebenen Argumente (vom Typ Real)
	h5 = dt/y + 0.5
	iuni = int(h5)
        y = dt/iuni
30      if(ituh.gt. idim)then
           ituh=idim
        endif
        sumu = 0.

        zeitschritte: do i2=1,ituh
         u(i2,i5) = 0.
         xi = float(i2-1)*dt-dt/(2*iuni)
         do i3=1,iuni
          xi = xi+y
	  h1 = xi/rk(i1,i5)
	  h3 = alog10(h1)                               ! Intrinsic Function alog10 liefert den natürlichen Logarithmus des Argumentes zurück
	  dum1 = (rn(i1,i5)-1.)*(h3)
	  if(dum1.le.-38.)then                          !--
	     h = 0.                                     !  |
	  elseif(dum1.gt.38.)then                       !  | h wird auf den darstellbaren Bereich einer
	     h = 1.e+37                                 !  | REAL Variablen begrenzt:
	  else                                          !  |       +- 1.70141*10**38
	     h = (xi/rk(i1,i5))**(rn(i1,i5)-1.)         !  |
	  endif                                         !--
	  if(h.lt.1.18e-38)then
	     h=1.1e-37
	  endif
          h1 = -xi/rk(i1,i5)
	  if (h1.gt.-43.75) then
	     h3 = exp(h1)
             h2 = rk(i1,i5)*gamf(i5)
             dum1 = alog10(h)-alog10(h2)+alog10(h3)     !--  h wird auf den darstellbaren Bereich einer REAL
	     if(dum1.lt.-38.)then                       !  | Variablen begrenzt:      +- 1.70141*10**38
		h = 0.                                  !--
             else
		h  = h*h3/h2
             endif
          else
             h = 0.
          endif
          if (h.le.1.e-12 .and. i3 .gt. 5) goto 5001
          u(i2,i5) = u(i2,i5)+h*y
         END DO

!c   bestimmung des max. von u(i2) fuer zeitschritt i2

5001   	 if (u(i2,i5).gt.umax(i1)) then
            imax(i1)=i2
            umax(i1)=u(i2,i5)
         end if

! Überprüfen des Abbruchkriteriums fuer die Berechnung der Uebertragungsfunktion
!-------------------------------------------------------------------------------
	 sumu = sumu + u(i2,i5)
	 if (u(i2,i5) .le. umin .and. sumu .gt. 0.8) goto 40
        END DO zeitschritte

! Abbruchkriterium noch nicht erreicht
!-------------------------------------
!       erneute Berechnung der Übertragungsfunktion mit erweitertem ituh (4000er Schleife)

        if (u(ituh,i5) .gt. umin .and. ituh .lt. idim) then
           ituh = min(ituh * 2,ituh+20)
           goto 30
        endif

! Abbruchkriterium erreicht
!--------------------------
40      ituh = i2-1
        ituhm = max0(ituh,ituhm)
        if (ituhm.gt.idim) then
	   WRITE(nerr, '(a)') 'gerinne: Anz. UH-Ordinaten > IDIM'

           call writeLogIntIntInt(6, 'Anz. UH-Ordinaten ist groesser als maximale Anzahl Zeitschritte und wird hierauf begrenzt!',&
             &'Amount of UH-ordinates bigger max. number of time steps!','','Strang',issp,'Max. Anzahl',idim,&
             &'Anz. UH-Ordinaten',ituhm,'gerinne')
           ituhm = idim
        END if

!c      write(*,102) u(ituh),ituh
!c102   format(1x,'wert u(ituh) = ',f12.4,' bei ituh = ',i3)

! Normierung der Unit-Hydrograph-Ordinaten
!-----------------------------------------
        suu(i1) = 0.
        do 6000 i2=1,ituhm
         suu(i1) = suu(i1) + u(i2,i5)                                           ! Summe UH (bezogen auf Fluss/Vorland (i1) und diesen Abflussabschnitt (i5))
6000    continue

        write(nerr,9103) suu(i1)
9103    FORMAT(1X, 'Summe der UH-Ordinaten = ',f9.6)

        fnorm(i1) = 1.0 / suu(i1)                                               ! Normierungsfaktor 1/Summe

        suuh(i1) = 0.
        do 7000 i2=1,ituhm
         u(i2,i5) = u(i2,i5) * fnorm(i1)                                        ! Normierung des UH
         suuh(i1) = suuh(i1) + u(i2,i5)                                         ! Summe normierter UH
	 if (i2.lt.ituh.and.u(i2,i5).le.umin .and. suuh(i1) .gt. 0.8) then      ! Abbruchkriterium
	    ituh=i2
	    goto 41
	 endif
7000    continue
41      continue

	if(abs(fnorm(i1)-1.0).gt. 0.2)then

           call writeLogIntRealRealRealReal(5,'Unit Hydrograph wird normiert (Grenzkriterium Summe&lt;0.83).',&
                &'Standardization factor and sum of standardized UH','','Strang',issp,&
                &'Speicheranzahl',rn(i1,i5),'Retentionskonstante',rk(i1,i5),'Normierungsfaktor',fnorm(i1),&
                &'Summe UH-Ordinaten vor Normierung',suu(i1),'gerinne' )

           write(nerr,104) fnorm(i1), suuh(i1)
104        FORMAT(1X, 'Normierungsfaktor = ',f8.4,' Summe normierter UH = ',f8.4)
	endif

! Ausgabe des Unit-Hydrograh in File quh (noch nicht implementiert in *.konfig)
!------------------------------------------------------------------------------
        if (nquh.gt.0) then
           if (i1.eq.1) then
              write(nquh,7601)i5,' fuer Gerinneabfluss'
           else
              write(nquh,7601)i5,' fuer Vorlandabfluss'
           endif
7601       format(/1X, 'Unit-Hydrograph fuer: Zustand ',i1,a)
           write(nquh,'(10(f6.4,1x))') (u(i2,i5),i2=1,ituh)
        endif

! JH 13.10.04
! Ausgabe der verschiedenen UH´s fuer das Gerinne (directory out..., File ...uh_ger.dat)
!------------------------------------------------
        IF (iz .EQ. 1) THEN
            if (i1 .eq. 1) then
                char_i1='f'
            else
                char_i1='v'
            end if
            IF (i5 .eq. 1) THEN
                char_i5 ='1'
            else if (i5 .eq. 2) then
                char_i5 ='2'
            else if (i5 .eq. 3) then
                char_i5 ='3'
            else if (i5 .eq. 4) then
                char_i5 ='4'
            else if (i5 .eq. 5) then
                char_i5 ='5'
            END IF
            write(ngeruh,'(/i6,a1,a1,2(i8)/)') issp,char_i1,char_i5,iz,ituh
            write(ngeruh,'(10(f8.3,1x))') (u(i2,i5),i2=1,ituh)
        END IF
        end if
       END DO unithydrograph	              		! Ende Schleife Abflussfall mit Berechnung UH´s

!-----------------------------------------------------------------------------------------------
! P a r a l l e l k a s k a d e n m o d e l l   f u e r   G e r i n n e   u n d   Vo r l a n d -
!                                                                                              -
!            A u s f l u s s w e l l e   n a c h  K a l i n i n - M i l j u k o v              -
!-----------------------------------------------------------------------------------------------

!c     Ausflussaufteilung auf Gerinne und Vorland und Addition
!c     Aus Dimensionierungsgruenden wird izsim1(i1) auf idim zeitschritte begrenzt.

       izsim1(i1) = izsim + ituhm

       if (izsim1(i1).gt.idim) then
          izsim1(i1) = idim
          write(nres,73) idim
       end if

       do 8000  i2=1,izsim1(i1)                                         ! Zeitschleife
!c      zeitzuordnung
!c      t(i2) = float(i2-1) * dt
        qa(i2) = 0.
!JH Anfangswerte initialisierung
        IF (isim .EQ. 1) THEN                                           ! Kurzzeit
           if (nanzanf1 .GT. 0) then
              do i7 = 1, nanzanf1
              qaanf(i7,i2)  = 0.
              qauanf(i1,i7,i2) = 0.
              end do
           end if
        END IF
!JH Ende
        qau(i1,i2) = 0.
	qauf=1.
! Ermittlung der Gewichtung der UH in Abhaengigkeit des Zuflusses und der 5 Abflussfaelle
!-----------------------------------------------------------------------------------------
!	if(i1.eq.1) then                                                ! Flussschlauch  und Vorland können von 1-5 besetzt sein!
           if (qz(i2).lt.qrk(1)) then                                   ! qrk(i): Abfluss aus *.ger
              i5 = 1
	      qauf = 1.
           else if (qz(i2).ge.qrk(1).and.qz(i2).lt.qrk(2)) then
              i5 = 1
	      qauf = (qrk(2)-qz(i2))/(qrk(2)-qrk(1))
           else if (qz(i2).ge.qrk(2).and.qz(i2).lt.qrk(3)) then
              i5 = 2
	      qauf = (qrk(3)-qz(i2))/(qrk(3)-qrk(2))
           else if (qz(i2).ge.qrk(3).and.qz(i2).lt.qrk(4)) then
              i5 = 3
	      qauf = (qrk(4)-qz(i2))/(qrk(4)-qrk(3))
           else if (qz(i2).ge.qrk(4).and.qz(i2).lt.qrk(5)) then
              i5 = 4
   	      qauf = (qrk(5)-qz(i2))/(qrk(5)-qrk(4))
           else
              i5 = 5
	      qauf = 1.
	   endif

!	else                                                            ! Vorland
!           if (qz(i2).lt.qrk(4)) then
!              i5 = 4
!	      qauf = 1.
!           else if (qz(i2).ge.qrk(4).and.qz(i2).lt.qrk(5)) then
!              i5 = 4
!  	      qauf = (qrk(5)-qz(i2))/(qrk(5)-qrk(4))
!           else
!              i5 = 5
!	      qauf = 1.
!	   endif
!	endif


	istart=max(1,i2-ituhm)




! JH 14.10.04
        qaufanf = qauf
!JH

! Berechnung des Anteils aus dem "unteren" UH unter Beruecksichtigung des Wichtungsfaktors qau
	do 9000  i3=istart,i2
	 qau(i1,i2) = qau(i1,i2) + u(i2-i3+1,i5) * qzu(i1,i3) * qauf
9000	continue

! Berechnung des Anteils aus dem "oberen" UH
	qauf = 1.-qauf

	if(abs(qauf) .gt. 0.0001) then
	   do 9001  i3=istart,i2
	    qau(i1,i2) = qau(i1,i2) + u(i2-i3+1,i5+1) * qzu(i1,i3) * qauf
9001	   continue
	endif

!JH 14.10.04 Anfangswerte Abfluss erzeugen
        IF (isim .EQ. 1) THEN                           ! Kurzzeit
           if (nanzanf1 .GT. 0) then                  	! Es sollen Anfangswerte geschrieben werden
              vstart: do i7 = 1,nanzanf1
                       erste: do i3 = istart,i2
                               qauanf(i1,i7,i2) = qauanf(i1,i7,i2) + u(i2-i3+1,i5) * qzuanf(i1,i7,i3) * qaufanf
                              end do erste
                       qaufanf = 1. - qaufanf
                       if (ABS(qaufanf) .GT. 0.0001) then
                          zweite: do i3 = istart,i2
     	                           qauanf(i1,i7,i2) = qauanf(i1,i7,i2) + u(i2-i3+1,i5+1) * qzuanf(i1,i7,i3) * qaufanf
                                  end do zweite
                       end if
                       qaanf(i7,i2) = qauanf(i1,i7,i2)
                      end do vstart
           end if
        END IF
!JH Ende Anfangswerte Abfluss


        qa(i2) = qau(i1,i2)
        sq = sq + qa(i2)
        fuq = sq*dtz
        sqa(i1) = sqa(i1) + qau(i1,i2)
        fuqa(i1) = sqa(i1)*dtz


        sqg = sqg + qa(i2)


!c      Ausflussmaxima Gerinne, Vorland

        if(qau(i1,i2).gt.qamax(i1)) then
           iqamax(i1) = i2
           qamax(i1) = qau(i1,i2)
        end if

!c      Umspeichern

        qg(iku,i2) = qa(i2) + qg(iku,i2)                        ! Es kann an einem Vereinigungsknoten (Zulauf aus mehreren Flüssen) schon ein Abfluss qg vorhanden sein, daher Addition von qg.
        qag(i2) = qag(i2) + qa(i2)

!JH 04.01.2005 Umspeichern Anfangswerte
        IF (isim .EQ. 1) THEN                           ! Kurzzeit
           if (nanzanf1 .GT. 0) then                  	! Es sollen Anfangswerte geschrieben werden
               umsp: do i7 = 1,nanzanf1
                      qaganf(i7,i2) = qaganf(i7,i2) + qaanf(i7,i2)
                     END do umsp
           end if
        END IF
!Ende Umspeichern Anfangswerte

!c      Definition Endpunkt der Ausflusswelle falls diese der Zuflusswelle entspricht
!c      (Vergleich der Einzelkomponenten Gerinne- und Vorlandabfluss)

!c	write(*,'(3(f9.3))')qzu(i1,i2),qa(i2),qau(i1,i2)

        if (i1.eq.1) then
           if(fuqa(i1).ge.fuqzf)then
	   goto 50
	   endif
!c      else if (i1.eq.2) then
!c         if(fuqa(i1-1).ge.fuqzf.and.fuqa(i1).ge.fuqzv)then
!c	   goto 50
!c	endif
        end if

8000   continue                                         ! Ende Zeitschleife



!c   Falls der do-loop vollstaendig durchlief, hat i2 den Wert von
!c   izsim(1)+1, qaend soll jedoch den Endwert erhalten.

       i2 = izsim1(i1) - 1

!c   In qaend wird kumuliert ueber 'ilauf', falls Vorland- und
!c   Flussschlauchabfluss muessen beide Komponenten erfasst sein.

50     qaend = qaend + qa(i2)

!c	do 8001 i3=1,i2
!c	write(*,'(5(f9.3))')qzu(1,i3),qau(1,i3),qzu(2,i3),qau(2,i3),
!c     *			qau(1,i3)+qau(2,i3)
!c8001	continue

!c   Zeitpunkt Scheitel der Ausflusswelle, Umrechnung auf Tage

       tdta(i1) = float(iqamax(i1))*dt
       tdta(i1) = tdta(i1)/24.

      END DO gerinnevorland				 ! Ende Schleife Flusschlauch - Vorland

      DO i=1,ilauf
       if (izsim1(i).gt.izsim) izsim = izsim1(i)
      END DO

!c    Gesamtausflussmaximum

      DO i=1,izsim
       if (qag(i).gt.qmax) then
          iqmax = i
          qmax = qag(i)
       end if
      END DO


! Ergebnisausdruck
!-----------------

! JH 12.10.04 Ausgabe Test Abfluss
!      IF (isim .EQ. 1) THEN                     ! Kurzzeit
!      nqaanf = ju0gfu()
!      nameqaanf = fnam
!      nameqaanf(ilenlz-3:ilenlz+6) = '_qaanf.dat'
!      OPEN (UNIT=nqaanf,FILE=nameqaanf,STATUS='new', ACTION = 'write', IOSTAT=ierr)
!      write(nqaanf,507)
!507   format(/' G e s a m t z u f l u s s qaanf  S t r a n g in m**3/s  '/)
!      aus_qaanf: do i7 = 1,nanzanf1
!                  write(nqaanf,'(/i7,i1,2(i8)/)') issp,i7,iz,izsim
!                  write(nqaanf,'(10(f8.3,1x))') (qaganf(i7,i),i=ianfkz(i7),izsim)
!                 end do aus_qaanf
!      CLOSE (nqaanf)
!      END IF

!      IF (isim .EQ. 1) THEN                     ! Kurzzeit
!      nqaganf = ju0gfu()
!      nameqaganf = fnam
!      nameqaganf(ilenlz-3:ilenlz+7) = '_qaganf.dat'
!      OPEN (UNIT=nqaganf,FILE=nameqaganf,STATUS='new', ACTION = 'write', IOSTAT=ierr)
!      write(nqaganf,509)
!509   format(/' G e s a m t z u f l u s s qaganf  S t r a n g in m**3/s  '/)
!      aus_qaganf: do i7 = 1,nanzanf1
!                  write(nqaganf,'(/i7,i1,2(i8)/)') issp,i7,iz,ituhm
!                  write(nqaganf,'(10(f8.3,1x))') (qaganf(1,i),i=ianfkz(1),ianfkz(1)+ituhm)
  !               end do aus_qaganf
 !     CLOSE (nqaganf)
!      END IF

!      nqa = ju0gfu()
!      nameqa = fnam
!      nameqa(ilenlz-3:ilenlz+3) = '_qa.dat'
!      OPEN (UNIT=nqa,FILE=nameqa,STATUS='new', ACTION = 'write', IOSTAT=ierr)
!      write(nqa,508)
!508   format(/' G e s a m t z u f l u s s qa  S t r a n g in m**3/s  '/)
!      write(nqa,'(/3(i8)/)') issp,iz,izsim
!       write(nqa,'(10(f8.3,1x))') (qag(i),i=1,izsim)
!      CLOSE (nqa)
! JH Ende


      write(nres,100) dt,tdtz
      write(nres,110) iqzmax,qzmax,iqzfmx,qzfmax,iqzvmx,qzvmax,fuqz,fuqzf,fuqzv
      write(nres,120) ituh,imax(ilauf),umax(ilauf),suu(ilauf),suuh(ilauf)
      write(nres,140) tdta(ilauf)
      if (ilauf.eq.1) then
         write(nres,150) qaend,iqmax,qmax,fuq,fuqa(ilauf),iqamax(ilauf),qamax(ilauf)
      else if(ilauf.eq.2) then
         write(nres,160) qaend,iqmax,qmax,fuq,fuqa(ilauf-1),fuqa(ilauf), &
                         iqamax(ilauf-1),qamax(ilauf-1),iqamax(ilauf),qamax(ilauf)
      end if
      write(nres,170) sqz,sq

!c       write(nf,'('' s t r a n g  '',i4/)') issp
!c       write(nf,100) dt,tdtz
!c       write(nf,110) iqzmax,qzmax,iqzfmx,qzfmax,iqzvmx,qzvmax,fuqz,fuqzf,fuqzv
!c       write(nf,140) tdta(ilauf)
!c       if (ilauf.eq.1) then
!c          write(nf,150) qaend,iqmax,qmax,fuq,fuqa(ilauf),iqamax(ilauf),qamax(ilauf)
!c       else if(ilauf.eq.2) then
!c          write(nf,160) qaend,iqmax,qmax,fuq,fuqa(ilauf-1),fuqa(ilauf), &
!c                        iqamax(ilauf-1),qamax(ilauf-1),iqamax(ilauf),qamax(ilauf)
!c       end if
!c       write(nf,170) sqz,sq


100   FORMAT(/ 1X, 'dt [h]: ',f6.2,5x,' tmax [d]: ',f8.2)
110   FORMAT(/ 1X, 'Kennwerte Zuflusswelle'/1x,22('-')/                		&
             & 1X, 'Scheitel-Zufluss               iqzmax = ',i6,'   qzmax = ',     	&
                   1x,f10.3,' [m**3/s]'/                                             	&
             & 1X, 'Scheitel-Zufluss im Gerinne    iqzfmx = ',i6,'  qzfmax = ',     	&
                   1x,f10.3,' [m**3/s]'/                                             	&
             & 1X, 'Scheitel-Zufluss im Vorland    iqzvmx = ',i6,'  qzvmax = ',     	&
                   1x,f10.3,' [m**3/s]'/                                            	&
             & 1X, 'Fuelle-Zufluss                   fuqz = ',f8.4,' [hm**3]'/       	&
             & 1X, 'Fuelle-zufluss im Gerinne       fuqzf = ',f8.4,' [hm**3]'/       	&
             & 1X, 'Fuelle-zufluss im Vorland       fuqzv = ',f8.4,' [hm**3]')
120   FORMAT(/ 1X, 'Kennwerte der Systemfunktionen'/1x,31('-')/            		&
             & 1X, 'Anzahl der Zeitschritte  ituh = ',i6/                         	&
             & 1X, 'Maximum u                imax = ',i6,'     umax = ',f8.6/     	&
             & 1X, 'Summe u                   suu = ',f8.6,'   suuh = ',f8.6)
140   FORMAT(/ 1X, 'tmax [d]: ',f8.2)
150   FORMAT(/ 1X, 'Kennwerte Ausflusswelle'/1x,23('-')/                  		&
             & 1X, 'Endordinate Ausfluss           qaend = ',f7.3/                  	&
             & 1X, 'Scheitel-Ausfluss              iqmax = ',i6,'    qmax = ',1x,f10.3,' [m**3/s]'/&
             & 1X, 'Fuelle-Ausfluss                  fuq = ',f8.4,' [hm**3]'/      	&
             & 1X, 'Fuelle-Ausfluss im Gerinne      fuqa = ',f8.4,' [hm**3]'/     	&
             & 1X, 'Scheitel-Ausfluss im Gerinne  iqamax = ',i6,'   qamax = ',1x,f10.3,' [m**3/s]')
160   FORMAT(/ 1X, 'Kennwerte Ausflusswelle'/1x,23('-')/                 		&
             / 1X, 'Endordinate Ausfluss           qaend = ',f7.3/                  	&
             / 1X, 'Scheitel-Ausfluss              iqmax = ',i6,'    qmax = ',1x,f10.3,' [m**3/s]'/&
             / 1X, 'Fuelle-Ausfluss                  fuq = ',f8.4,' [hm**3]'/       	&
             / 1X, 'Fuelle-Ausfluss im Gerinne      fuqa = ',f8.4,' [hm**3]'/       	&
             / 1X, 'Fuelle-Ausfluss im vorland      fuqa = ',f8.4,' [hm**3]'/      	&
             / 1X, 'Scheitel-Ausfluss im Gerinne  iqamax = ',i6,'   qamax = ',1x,f10.3,' [m**3/s]'/&
             / 1X, 'Scheitel-Ausfluss im Vorland  iqamax = ',i6,'   qamax = ',1x,f10.3,' [m**3/s]')
170   FORMAT(/ 1X, 'Gesamtzuflusswelle (ohne Basisabfluss)  sqz = ',f12.3/		&
             & 1X, 'Gesamtabflusswelle    "       "         sq  = ',f12.3)

73    FORMAT(/ 1X, 'gerinne: Anzahl der Zeitschritte in Gerinne durch Nachlauf zu hoch geworden.'/&
             & 1X, 'Sie wird auf ',i5,' begrenzt!')
      return
      END IF



!-----------------------------------------------------------------------------------------------
!              keine Abflussverzögerung (iart = 0, fiktiver Gerinnestrang)                     -
!-----------------------------------------------------------------------------------------------

      if (iart.eq.0) then
         write(nres,'(/a)')' Fiktiver Strang! Es erfolgt keine Abflussverzoegerung!'
         do i=1,izsim
          qg(iku,i) = qz(i) + qg(iku,i)
         END do
      return
      END if


!-----------------------------------------------------------------------------------------------
!              Abfluss durch Sopeicherkennlinie (iart = 2, Speicherstrang)                     -
!-----------------------------------------------------------------------------------------------


      IF (iart.eq.2) then
         write(nres,'(/'' Aufruf der Speicherroutine ''/)')
         call speicher(iz,iku,dt,idif,izsim,issp,qz,nsv,nueb,slash)


!-----------------------------------------------------------------------------------------------
!              Abfluss durch Talsperre (iart = 3, Talsperre)                                   -
!-----------------------------------------------------------------------------------------------

      else if (iart.eq.3) then
         write(nres,'(/'' Aufruf der Speicherroutine zur Talsperrensteuerung ''/)')
	 call sp_geisel(iz,iko,iku,dt,idif,izsim,issp,                 &
                       qz,nsv,nueb,nsv_e,nsv_p,nsv_b,nsh,              &
                       natg1,namo1,naja20,netg1,nemo1,neja20,          &
                       nast,namn,nest,nemn,                            &
                       svmm,svend,ivor,slash)


! iart ist nicht 0-3, keine Zuordnung möglich.
      else


      call writeLogIntInt(7,'Der Strang konnte nicht den moeglichen Typen (natuerlich,fiktiv, Speicher) zugeordnet werden!', &
           & 'The channel could not be identified. Possible types are natural, virtual or reservoir!','','Strang',issp, & 
           & 'Strangart',iart,'gerinne' )

         write(nerr,8788)
         call writeFooter()
         stop 'gerinne 440'
      end if

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

8788  FORMAT(/ 1X, 'Fehler in der Subroutine gerinne : '/ &
	     & 1X, 'Die Kennzeichnung des Gerinneabschnitts konnte weder'/ &
             & 1X, 'als natuerlich noch als fiktiv oder als Speicher identifiziert werden.'/      &
             & 1X, 'Bitte ueberpruefen sie die Eingabegroesse iart (Strangart) in *.ger!')
9063  FORMAT(/ 1X, 'Warnung in der Subroutine gerinne : '/ &
	     & 1X, 'Der Gerinneabfluss uebersteigt den maximal angegebenen Abfluss'/ &
             & 1X, 'der Kalinin-Miljukov Wertepaare. Berechnen Sie ggf. die Parameter fuer'/      &
             & 1X, 'hoehere Abflussereignisse.'/      &
             & 1X, 'Es wird mit den Parametern des groessten angegebenen Wertes gerechnet.')

      end

