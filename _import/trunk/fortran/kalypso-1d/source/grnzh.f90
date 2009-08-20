!     Last change:  MD    8 Jul 2009    3:41 pm
!--------------------------------------------------------------------------
! This code, grnzh.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - grnzh
!
! Copyright (C) 2004  ULF TESCHKE & WOLF PLOEGER.
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
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Wolf Ploeger:     phone: +49 40 42878 4305 mail: ploeger@tuhh.de
! Ulf Teschke:      phone: +49 40 42878 3895 mail: teschke@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 18 August 2004
! Research Associate
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
SUBROUTINE grnzh (q, indmax, hgrenz, xi, hi, s, Q_Abfrage)
!
! Beschreibung
! ------------
! Unterprogramm zu SUB NORMBER und WSPANF. Hier wird die Grenztiefe
! iterativ bestimmt. Dazu wird für verschiedene Wasserstände
! die Froudzahl bestimmt.
!
!**
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE PARAMETER               
!**   ---------------------------------------------------               
!**                                                                     
!**   df      --      Differenz =  df = (frouda-froudb)/(hra-hrb)       
!**   dif     --      Differenz = froud-1. oder dif=(1.-froud)/df       
!**   diff    --      Differenz                                         
!**   dx      --      Schrittweite
!**   dx1     --      Schrittweite                                      
!**   falt    --      Gesamtfläche (alt)                                
!**   fges    --      Gesamtfläche                                      
!**   froud   --      Froud-Zahl                                        
!**   frouda  --      Froud-Zahl                                        
!**   froudb  --      Froud-Zahl                                        
!**   froudm  --      Froud-Zahl                                        
!**   froudn  --      Froud-Zahl                                        
!**   froudopt--      optimale Froud-Zahl                               
!**   GRNZ_IT --      max. Anzahl der Grenztiefeniteration              
!**   GRNZ_SCH--      Maxwert fuer Schleifenzaehler der Grenztiefenit.  
!**   hgrenz  --      Grenztiefe                                        
!**   hmax    --      maximale Wasserspiegelhöhe                        
!**   hmin    --      minimale Wasserspiegelhöhe                        
!**   hra     --      Wasserspiegelhöhe                                 
!**   hrb     --      Wasserspiegelhöhe                                 
!**   hrm     --      Wasserspiegelhöhe                                 
!**   hrn     --      Wasserspiegelhöhe                                 
!**   hrn     --      Wasserspiegelhöhe                                 
!**   hrn     --      Wasserspiegelhöhe                                 
!**   i       --      ZAEHLPARAMETER DER SCHLEIFE [-]                   
!**   idruck  --      Charakterisierung des Druckabflusses              
!**   izz     --
!**   izzz    --      Laufparameter SCHLEIFE 7002                       
!**   nstat   --      Anzahl der Stationen
!**   nsch    --      Schleifenzaehler fuer Grenztiefenteration         
!**   q       --      Abfluß                                            
!**   wsp     --      Wasserspiegelhöhe                                 
!**                                                                     
!**                                                                     
!**   AUFGERUFENE SUBROUTINEN                                           
!**   -----------------------                                           
!**   - eb2ks(iprof,hv,rg,rg1,q,q1,itere1,nstat,hr,nknot)
!**   - eb2kst(indmax,hr,hv,rg,q)                                       
!**   - erfroud(br,f,qt,u,iprof,indfl,indmax,froud)                     
!**   - uf(hr,hi,xi,s,indmax,nfli,nfre)                                 
!**                                                                     
!***********************************************************************

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS
USE MOD_INI

! Calling variables
REAL :: q                       ! Abfluss
INTEGER :: indmax               ! Anzahl Rauhigkeitszonen im Profil
REAL :: hgrenz                  ! Ermittelte Grenztiefe?
REAL :: xi (maxkla)             ! Horizontaldistanz der Profilpunkte [?]
REAL :: hi (maxkla)             ! Hoehendifferenzen zwischen den Profilpunkten [?]
REAL :: s (maxkla)              ! Distanz der Profilpunkte [?]


! Local variables
REAL :: rg1 = 0.0               !EP   05.02.2002  Initialisierung des  Reibungsgefälles rg1
INTEGER :: i, j


! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /ROHR/ ---------------------------------------------------------
INTEGER         :: idruck
COMMON / rohr / idruck
! -----------------------------------------------------------------------------

CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     !Abfrage fuer Ende der Inneren Q-Schleife


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------


!**   SCHREIBEN IN KONTROLLFILE                                         
write (UNIT_OUT_LOG, 2000)
2000 format (/1X, 'Grenztiefenberechnung:', /, &
            & 1X, '----------------------', /)
                                                                        
! Vorbelegung FUER ITERATION
! (Niedrigster Wasserspiegel ist also 20 cm über dem niedrigsten Punkt des Profils)
diff = 0.2
                                                                        
!**   Schleifenzaehler fuer Grenztiefenteration                         
grenztiefe: do

  nsch = 0
  !HB   setzen von itmax
  itmax_grnzh = GRNZ_IT
  !HB   setzen von ischlmax
  ischlmax = GRNZ_SCH
  ischl = 0
  izz = 0
  hrn = 0.0
  froudn = 0.
  froudm = 1000.

  ! Anfangswasserspiegel fuer Grenzwertiteration auf Sohlhoehe
  ! hmin = minimale Gelaendehoehe des Profiles, diff = 0,2 m (siehe oben)
  hr = hmin + diff
                                                                        
  froudopt = 1000.
                                                                        
  ! Anfangsschrittweite fuer die Veraenderung des Wasserspiegels
  dx = 0.1
                                                                        

  !** SCHREIBEN IN KONTROLLFILE, WERTE WERDEN IN ZEILE 700 GESCHRIEBEN
  write (UNIT_OUT_LOG, 2001) 'I', 'HR', 'FROUDZAHL', 'DELTA H', 'ABSCHNITT'
  2001 format (1X, A3, A12, A12, A12, A12)
                                                                        

  23 CONTINUE


  ! ------------------------------------------------------------------
  ! BEGINN ITERATIONSSCHLEIFE BIS LABEL 100
  iteration: DO i = 1, itmax_grnzh
                                                                        
                                                                        
    ! ------------------------------------------------------------------
    ! Berechnung der wasserstandsabhaengigen Geometriegroessen
    !
    !     hrb    r4          input       wasserstand in nn+m
    !     xi     r4(1...*)   input       horizontaldistanz der profilpunkte
    !     hi     r4(1...*)   input       hoehendifferenzen zw. d. profilpkte
    !     s      r4(1...*)   input       distanz der profilpunkte
    !     indmax i4          output      anzahl rauhigkeitszonen im profil
    !     nfli   i4          output      nr. der rauhigkeitszone an der
    !                                    linken vorland/
    !                                    fluszschlauch-trennflaeche
    !     nfre   i4          output      nr. der rauhigkeitszone an der
    !                                    rechten vorland/
    !                                    Flusschlauch-trennflaeche
    ! Beachte: Es wird unterstellt, dass ein Rauhigkeitswechsel zwischen
    ! den Trennflaechen stattfindet.
    !
    ! Bei Sonderprofilen liegt nur E I N E Rauhigkeitszone vor.
    ! Hier ist NFLI = NFRE = 1.

    ! falt = FLAECHE DER VORHERIGEN BERECHNUNG, START 0.0
    ! WIRD MIT fges VERGLICHEN
    falt = 0.
                                                                        
                                                                        
    110 CONTINUE
                                                                        
    !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 257, hr = ', hr

    CALL uf (hr, hi, xi, s, indmax, nfli, nfre)

    !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 260, fges = ', fges

    !**   Ueberpruefen der gesamt durchstroemten flaeche:
    !**   wenn fges < 0  --> wsp unterhalb tiefster sohlenkoordinate
    !**   DANN WIRD hr um dx ERHOEHT UND DIE GROESSEN WERDEN ERNEUT
    !**   BERECHNET --> LABEL 110
    !**   fges WIRD NEU BERECHNET, STEHT IM COMMONBLOCK/GES/
                                                                        
    IF (fges.lt.1.e-02) then
      hr = hr + dx
      GOTO 110
    ELSEIF (q / fges.gt.20.) then
      IF (abs (falt - fges) .gt.1.e-06) then
        hr = hr + dx
        falt = fges
        GOTO 110
      ENDIF
    ENDIF                                     
                                                                        
    !**   Berechnung und ueberpruefen der Froud-Zahl:
    !**   Berechnung des wirksamen Geschwindigkeitsverlustes (hv)
    !**   und Reibungsverlustes des Profils (rg) output ---> qt !!!!!
                                                                        
                                                                        
    ! -----------------    ROHRSTOEMUNG  --------------------------
    IF (idruck.eq.1) then

      !** keine Grenztiefe ermittelbar, Druckrohr
      WRITE (UNIT_OUT_LOG, 2100)
      2100 format (1X, 'Keine Grenztiefe bis Rohscheitel!', /, &
                 & 1X, '-> Setze Grenztiefe = hmax !')

      hr = hmax + 0.0001

      CALL uf (hr, hi, xi, s, indmax, nfli, nfre)

      !**  hgrenz = hmax + (q/f(1))*(q/f(1))/2./9.81
      hgrenz = hmax
      !UT  SUBROUTINE ENDE
      GOTO 9999

    ENDIF 
    ! ---------------------- ENDE ROHRSTROEMUNG ------------------
                                                                        

    i8000 = 0
                                                                        
    !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 310, vor call eb2ks.'

    if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
      nstat = 1
      itere1 = i
      !** BERECHNUNG NACH DARCY
      CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hr, nknot, Q_Abfrage)
    ELSE
      !** BERECHNUNG NACH STRICKLER
      CALL eb2kst (indmax, hr, hv, rg, q)
    ENDIF

    CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froud)

    !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 325, nach call erfroud. FROUD = ', froud


    IF (abs (froud-1.) .lt.1.e-3) then
                                                                        
      !**         SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, 2002) i, hr, froud
      2002 format (/1X, 'Grenztiefe nach ', I3, ' Iterationen berechnet.', /, &
                  & 1X, 'HR = ', F10.3, '   bei FROUDZAHL = ', F10.4)
      GOTO 200
                                                                        
    ELSE
                                                                        
      ! ueberpruefen der tendenz mit newton
      hra = hr + dx
      hrb = hr - dx

      !write (UNIT_OUT_LOG,*) 'HR=', hr, '  HRA=', hra, '  HRB=',hrb

      IF (hrb.le.hmin) hrb = hmin + 0.01
                                                                        
      102 CONTINUE
                                                                        
      CALL uf (hra, hi, xi, s, indmax, nfli, nfre)
                                                                        
      IF (fges.lt.1.e-2) then
        hra = hra + dx
        GOTO 102
      ENDIF
                                                                        
      !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 353, vor call eb2ks.'
      !**  BERECHNUNG NACH STRICKLER eb2kst ODER DARCY eb2ks
      if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
        nstat = 1
        itere1 = i
                                                                        
        !**  BERECHNUNG NACH DARCY
        CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hra, nknot, Q_Abfrage)

      ELSE

        !**  BERECHNUNG NACH STRICKLER
        CALL eb2kst (indmax, hra, hv, rg, q)

      ENDIF

      CALL erfroud (br, f, qt, u, iprof, indfl, indmax, frouda)
                                                                        
      !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 371, nach call erfroud. FROUDA = ', frouda

      103 CONTINUE
                                                                        

      CALL uf (hrb, hi, xi, s, indmax, nfli, nfre)
                                                                        
      IF (fges.lt.1.e-2) then
        hrb = hrb + dx / 10.
        GOTO 103
      ENDIF
                                                                        
      !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 383, vor call eb2ks.'
      if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
        nstat = 1
        itere1 = i
                                                                        
        !** BERECHNUNG NACH DARCY
        CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hrb, nknot, Q_Abfrage)

      ELSE

        !** BERECHNUNG NACH STRICKLER
        CALL eb2kst (indmax, hrb, hv, rg, q)

      ENDIF 

      CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froudb)

      !write (UNIT_OUT_LOG,*) 'In GRNZH. Zeile 400, nach call erfroud. FROUDB = ', froudb

      df = (frouda - froudb) / (hra - hrb)

      IF (abs (df) .gt. 0.0001) dif = (1. - froud) / df
                                                                        
      !**        SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, 2003) i, hr, froud, dif, indfl
      2003 format (1X, I3, 3F12.4, I12)

      hra = hr
      hr = hr + dif
      IF ( (hr - hmin) .le.0.001) hr = (hra + hmin) / 2.
      IF (abs (dif) .lt.0.001) then
        !**  if (abs(hr-hra).lt.0.001) then
        hgrenz = hra
        i8000 = 1
        GOTO 123
      ENDIF
                                                                        
    ENDIF
                                                                        
    IF (abs (froud-1.) .lt.abs (froudopt - 1.) ) then
                                                                        
      ! Abspeichern der guenstigsten Werte
                                                                        
      froudopt = froud
      wsp = hra

    ENDIF
                                                                        
    !***************************************************
    !**   Ergaenzung vom 30.10.92 J. Csocsan
                                                                        
    123 CONTINUE

    IF (froud.lt.1.0) then
                                                                        
      IF (froud.gt.froudn) then
        froudn = froud
        hrn = hra
      ENDIF
                                                                        
    ELSE
                                                                        
      IF (froud.lt.froudm) then
        froudm = froud
        hrm = hra
      ENDIF
                                                                        
    ENDIF
                                                                        
    !**   i8000 = 1, WENN if (abs(dif).lt.0.001), UT 19.01.00
    IF (i8000.eq.1) goto 8000
                                                                        
    !**   Ergaenzung vom 30.10.92 Ende
    !**************************************************

  END DO iteration
                                                                        
  !UT   ACHTUNG: froudn IST REAL!!!
  !UT   alt if (froudn.eq.0.0) then
                                                                        
  IF (froudn .lt. 1.e-6 .and. froudn .gt. - 1.e-6) then
    ischl = ischl + 1
                                                                        
    IF (ischl.le.ischlmax) then
                                                                        
      !** SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, 2004)
      2004 format (1X, 'Maximale Anzahl der Iterationsschritte ueberschritten!', /, &
                 & 1X, '-> Grenztiefe noch nicht gefunden, daher noch ein Suchlauf!')
                                                                        
      GOTO 23
                                                                        
    ENDIF

    IF (abs (froud-1.) .lt.0.06) then
      i8000 = 1
      !ccc  D.Bley 12.1.98 geaendert
      hgrenz = hra
      GOTO 8000
    ENDIF

    !**    SCHREIBEN IN KONTROLLFILE
    write (UNIT_OUT_LOG, 2005)
    2005 format (1X, 'Keine Grenztiefe ermittelbar. Rechne mit dem optimalsten Wert weiter!')

    hgrenz = wsp
    froud = froudopt

    !**    SCHREIBEN IN KONTROLLFILE
    write (UNIT_OUT_LOG, 2006) hgrenz, froud
    2006 format (1X, 'HR    = ', F12.4, /, &
               & 1X, 'FROUD = ', F12.4)
  ELSE

    hr = hrn 
    GOTO 6000

  ENDIF 

  200 CONTINUE

  IF (hr.le.hmin) then 
    write (*,2101)
    2101 format (1X, 'In SUB GRNZH. Wasserstand ist kleiner als niedrigster Gelaendepunkt!')
    call stop_programm(0)
  ENDIF

  hgrenz = hr 
  RETURN
                                                                        
  8000 CONTINUE

  IF (hr.le.hmin) then
    write (*,2101)
    call stop_programm(0)
  ENDIF
                                                                        


  IF (abs (froud-1.) .gt. 0.05) then

    !**   SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, 2102) i, hgrenz, froud
    2102 format (/1X, 'Abbruch der Iteration in Schritt ', I3, ' mit DIF > 0.05.', /, &
                & 1X, 'HR    = ', F12.4, /, &
                & 1X, 'FROUD = ', F12.4)

  ELSE
                                                                        
    !**         SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, 2103) i, hgrenz, froud
    2103 format (/1X, 'Abbruch der Iteration in Schritt ', I3, ' mit DIF < 0.05.', /, &
                & 1X, 'HR    = ', F12.4, /, &
                & 1X, 'FROUD = ', F12.4)

    RETURN 

  ENDIF
                                                                        
  ! ------------------------------------------------------------------
  ! Ergaenzung vom 30.10.92 J. Csocsan

  6000 CONTINUE

  dx1 = 0.01
  iterfr = 0
  ieb = 0
  ianz = ifix ( (hrn - hrm) * 100.) + 1
  ikennfr = 0
                                                                        
  ! keine konvergenz --> andere iteration:
                                                                        
  !**   SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, 2007) 'I', 'HR', 'FROUDZAHL', 'DELTA H'
  2007 format (/1X, 'Keine Konvergenz --> Es wird eine andere Iteration versucht!', //, &
              & 1X, A3, A12, A12, A12, A12)
                                                                        
  hr = hrn
                                                                        
  6040 CONTINUE

  hr = hr - dx1
  iterfr = iterfr + 1
  ieb = ieb + 1

  CALL uf (hr, hi, xi, s, indmax, nfli, nfre)

  if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
    nstat = 1
    itere1 = ieb

    !**       BERECHNUNG NACH DARCY
    CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hr, nknot, Q_Abfrage)

  ELSE

    !**       BERECHNUNG NACH STRICKLER
    CALL eb2kst (indmax, hr, hv, rg, q)

  ENDIF

  CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froud)

  dif = froud-1.
                                                                        
  !**   SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, 2008) ieb, hr, froud, dif
  2008 format (1X, I3, 3F12.4)
                                                                        

  IF (abs (froud-1.) .lt.1.e-3) then

    write (UNIT_OUT_LOG, '(''grenztiefe im '',i3,''ten schritt b&
     &erechnet:''           ,/,''hr = '',f12.4,1x,''bei froud '',f12.4)'&
     &) ieb, hr, froud

    hgrenz = hr
    RETURN

  ENDIF

  IF ( (froud-1.) * (froudn - 1.) .lt.0.) then
                                                                        
    IF (abs (froud-1.) .lt.0.05) then

      WRITE (UNIT_OUT_LOG, '(''abbruch der iteration in schritt '',i4,       &
       &     '' mit'',/,''froud= '',f12.4,''bei hr = '',f12.4)') ieb, froud, hr
      WRITE (UNIT_OUT_LOG, '(''abbruch der iteration : dif < 0.05'')')
      WRITE (UNIT_OUT_LOG, '(''--> aus grenztiefe mit hr = '',f12.4,/,      &
       &              ''                  froud = '',f12.4,/)') hr, froud

      hgrenz = hr 
      RETURN
                                                                        
    ELSEIF (ikennfr.gt.0) then
                                                                        
      !**         SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(''abbruch der iteration dif > 0.05 !!'',    &
       &''in schritt '',i4,'' mit'',/,''froud= '',f12.4,                  &
       &'' bei hr = '',f12.4,'' dif='',f12.4)') ieb, froud, hr, dif

                                                                        
      IF (abs (dif) .gt.abs (froudopt - 1.) ) then
        hgrenz = wsp
        froud = froudopt
      ELSE
        hgrenz = hr
      ENDIF
                                                                        
      !**         SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(''keine grenztiefe ermittelbar !!!'')')
      WRITE (UNIT_OUT_LOG, '(''--> weiter mit dem optimalsten '',    &
       &  ''wert:'',/,'' hr = '',f12.4,/,''froud = '', f12.4,/)') hgrenz, froud

      RETURN 
                                                                        
    ELSE
                                                                        
      ikennfr = ikennfr + 1
                                                                        
      IF (ieb.eq.1) then
                                                                        
        hra = hr + 0.01
                                                                        
        IF (abs (dif) .lt.abs (froudopt - 1.) ) then
          froudopt = froud
          wsp = hr
        ENDIF
                                                                        
      ENDIF
                                                                        
      ianz = ifix ( (hra - hr) * 1000.) + 1
      dx1 = 0.001
                                                                        
      IF (ieb.eq.1) then
        hr = hrn
      ELSE
        hr = hra
      ENDIF
                                                                        
      iterfr = 0
      GOTO 6040
                                                                        
    ENDIF
                                                                        
  ENDIF
                                                                        
  hra = hr
                                                                        
  IF (abs (froud-1.) .lt.abs (froudopt - 1.) ) then
                                                                        
    ! abspeichern der guenstigsten werte
    froudopt = froud
    wsp = hra

  ENDIF
                                                                        
  6060 CONTINUE

  IF (iterfr.lt.ianz) goto 6040
                                                                        
  !**    SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(''keine grenztiefe gefunden !!! '',/,        &
   &       ''es wird mit der bis dahin ermittelten '',                &
   &        ''guenstigsten werten weitergerechnet'',/,                &
   &        ''wasserspiegel = '',f8.3,/,                              &
   &        ''froud         = '',f7.4)') wsp, froudopt

  hgrenz = wsp
                                                                        
  RETURN
                                                                        
  !**   Ergaenzung vom 30.10.92 Ende
  !**   ------------------------------------------------------------------

                                                                        

  7000 CONTINUE
                                                                        
  !     keine Konvergenz --> andere Iteration:
                                                                        
  !**   SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(/,'' keine konvergenz --> andere iteration'')')
  WRITE (UNIT_OUT_LOG, '('' i    hr    froud    dif'')')

                                                                        
  !  1. schritt : finden des Einschlussintervalls:
  hr = hmin + 0.01
  izzz = 0
                                                                        
  einschlussintervall: DO i = 1, 100
                                                                        
    IF (idruck.ne.0) then
      izzz = izzz + 1
      IF (izzz.lt.2) hr = hmax + 0.0001
    ENDIF
                                                                        
    104   CONTINUE

    CALL uf (hr, hi, xi, s, indmax, nfli, nfre) 
                                                                        
    IF (fges.lt.1.e-2) then
      hr = hr + 0.01
      GOTO 104
    ENDIF
                                                                        
                                                                        
    if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
      nstat = 1
      itere1 = i

      !**              BERECHNUNG NACH STRICKLER
      CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hrb, nknot, Q_Abfrage)

    ELSE

      !**              BERECHNUNG NACH STRICKLER
      CALL eb2kst (indmax, hr, hv, rg, q)

    ENDIF

    CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froud)
                                                                        

    IF (idruck.eq.1) then

      !              keine grenztiefe ermittelbar, druckrohr                  
      !**            SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '('' keine grenztiefe bis rohrscheitel!! '', &
       &           /,''-> Setze Grenztiefe = hmax !!'')')

      hr = hmax + 0.0001

      CALL uf (hr, hi, xi, s, indmax, nfli, nfre)

      !              hgrenz = hmax + (q/f(1))*(q/f(1))/2./9.81
      hgrenz = hmax
                                                                        
      !**            LABEL 9999 --> SUBENDE
      GOTO 9999
                                                                        
    !**         ENDIF (idruck.eq.1)
    ENDIF
                                                                        
    !**        SCHREIBEN IN KONTROLLFILE
    dif = froud-1.
    WRITE (UNIT_OUT_LOG, '(i3,3(f12.4))') i, hr, froud, dif

                                                                        
    !**        i = ZAEHLPARAMETER DO-SCHLEIFE 7002
    IF (i.gt.1) then
                                                                        
      IF ( ( (froud-1.) * (frouda - 1) .lt.0.) ) then
                                                                        
        !              einschlussintervall gefunden
        !**            SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '('' einschlussintervall gefunden '')')
        WRITE (UNIT_OUT_LOG, '('' --> anfang mit hr = '',f15.3)') hra

        hr = hra
                                                                        
        EXIT grenztiefe
                                                                        
      !**          ENDIF (((froud-1.)*(frouda-1).lt.0.))
      ENDIF
                                                                        
    !**        ENDIF (i.gt.1)
    ENDIF
                                                                        
    hra = hr
    hr = hr + diff
                                                                        
  END DO einschlussintervall
                                                                        
  !          keine konvergenz in grenztiefe
  !**      print *,'kein schnittpunkt in grenztiefe '
                                                                        
  !**   VERAENDERUNG VON hr=hmin+diff, ca. Zeile 170
  !**   HALBIERUNG FUER DEN NAECHSTEN DURCHLAUF
  diff = diff / 2.

!**   ca. Zeile 150, GANZ OBEN
end do grenztiefe

hr = hmin + 0.1
dx = 0.05
                                                                        
! -----------------------------------------
! BEGINN DO-SCHLEIFE 7001 BIS ca. ZEILE 820
DO i = 1, 100
                                                                        
  hr = hr + dx
  falt = 0.
                                                                        
  710 CONTINUE

  CALL uf (hr, hi, xi, s, indmax, nfli, nfre) 
                                                                        
  IF (fges.lt.1.e-02) then
    hr = hr + dx
    GOTO 710
  ELSEIF (q / fges.gt.20.) then
    IF (abs (falt - fges) .gt.1.e-06) then
      hr = hr + dx
      falt = fges
      GOTO 710
    ENDIF
  ENDIF
                                                                        
  if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
    nstat = 1
    itere1 = i

    !**            BERECHNUNG NACH DARCY
    CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hrb, nknot, Q_Abfrage)

  ELSE

    !**              BERECHNUNG NACH STRICKLER
    CALL eb2kst (indmax, hr, hv, rg, q)
                                                                        
  ENDIF

  CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froud)
                                                                        
  WRITE (UNIT_OUT_LOG, '(i3,3(f12.4))') i, hr, froud, dif

                                                                        
  IF (i.gt.1) then
                                                                        
    IF ( ( (froud-1.) * (frouda - 1.) ) .lt.0.) then
      !              Schnittpunkt gefunden
      IF (abs (frouda - 1.) .lt.abs (froud-1.) ) then
        hgrenz = hra
        froud = frouda
      ELSE
        hgrenz = hr
      ENDIF
                                                                        
      !**            SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, '(''grenztiefe im '',i3,               &
       &                       ''ten schritt berechnet:''                 &
       &                 ,/,''hr = '',f6.2,1x,''bei froud '',f6.4)') i, hr, froud
                                                                        
      !**            LABEL 9999 --> SUBENDE
      GOTO 9999
                                                                        
    !**          ENDIF (abs(frouda-1.).lt.abs(froud-1.))
    ENDIF
                                                                        
  !**        ENDIF (i.gt.1)
  ENDIF
                                                                        
  frouda = froud
  hra = hr
                                                                        
END DO
!**   ENDE DO-SCHLEIFE 7001                                             
!**   ---------------------                                             
                                                                        
hgrenz = hr
                                                                        
froud = frouda
                                                                        

9999 RETURN
                                                                        
                                                                        
END SUBROUTINE grnzh
