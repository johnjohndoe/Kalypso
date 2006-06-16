!     Last change:  WP   13 Jun 2006   10:00 am
!--------------------------------------------------------------------------
! This code, Abfluss.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - abfluss
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
!**********************************************************************


SUBROUTINE abfluss (ad, aue, apg, wl, rhy, raub, breite, he1, hr3,&
                  & qd, qw, qges, aue3, wl3, iart)


!JK   BESCHREIBUNG: BERECHNUNG DES GESAMTABFLUSSES AN EINEM             
!JK                 BRUECKENPROFIL; DRUCKABFLUSS + UEBERFALLABFLUSS     
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**                                                                     
!**   PARAMETER DER SUB Abfluss                                         
!**   -------------------------                                         
!**   ad      --      durchströmte Fläche beim Druckabfluß              
!**   apg     --      angeströmte Pfeilerfläche                         
!**   aue     --      durchströmte Überfallfläche                       
!**   aue3    --      durchströmte Überfallfläche                       
!**   beiw    --      Verlustbeiwert an der Brücke                      
!**   breite  --      Brückenbreite in Fließrichtung                    
!**   c0      --      Überfallbeiwert für Brücke                        
!**   dif     --      Differenz                                         
!**   fx1     --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   fx2     --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   fxanf   --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   h0      --      Wasserspiegelhöhe                                 
!**   he1     --      Energiehöhe an Brückenprofil 1                    
!**   hmit    --      mittlere Fließtiefe                               
!**   hr3     --      Wasserspiegelhöhe an Brückenprofil 3              
!**   iart    --      Art des Überfalls an der Brücke                   
!**   lam     --      Widerstandsbeiwert für die Iteration
!**   lam_neu --      Widerstandsbeiwert                                
!**   qd      --      Druckabfluß an der Brücke
!**   qges    --      Gesamtabfluß an der Brücke                        
!**   qw      --      Wehrabfluß an der Brücke                          
!**   qw1     --      Wehrabfluß an der Brücke                          
!**   raub    --      Rauheit der Brücke                                
!**   re      --      Einlaufverlustbeiwert an der Brücke               
!**   rey     --      Reynoldszahl                                      
!**   rhy     --      hydraulischer Radius                              
!**   rpf     --      Pfeilerverlustbeiwert an der Brücke               
!**   rr      --      Reibungsverlustbeiwert                            
!**   tau00   --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   taumax  --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   tauu    --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   tauu0   --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   vd      --      mittlere Geschwindigkeit des Druckabflusses       
!**   wl      --      Brückenbreite senkrecht zur Fließrichtung         
!**   wl3     --      Brückenbreite senkrecht zur Fließrichtung         
!**   x1      --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   x2      --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   xanf    --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**   yk      --      Parameter zur Überprüfung der Grenzbedingungen    
!**                   für die Überfallart                               
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!JK   KEINE                                                             
!**                                                                     
!***********************************************************************

!WP 01.02.2005
USE KONSTANTEN
USE IO_UNITS
USE MOD_INI

implicit none

! Uebergabe Variablen
REAL, INTENT(IN)  	:: ad, aue, apg, wl, rhy, raub, breite
REAL, INTENT(IN)  	:: he1, hr3, aue3, wl3
REAL, INTENT(OUT) 	:: qd, qw, qges
INTEGER, INTENT(OUT)    :: iart

! Lokale Variablen
REAL :: beiw, c0, dif, fx1, fx2, fxanf, h0, hmit, lam, lam_neu, qw1, f1
REAL :: re, rey, rpf, rr, tau00, taumax, tauu, tauu0, vd, x1, x2, xanf, yk

INTEGER :: itermax, iter, itx, itxmax
! ST 24.03.2005
REAL :: GET_LAMBDA, f


!**   ------------------------------------------------------------------
!**   BERECHNUNGEN
!**   ------------------------------------------------------------------

write (UNIT_OUT_LOG, 1000) ad, aue, wl, apg, aue3, wl3, rhy, raub, breite, he1, hr3
1000 format (/1X, 'SUB ABFLUSS, uebergebene Variablen:', /, &
           &  1X, '-----------------------------------', /, &
           &  1X, 'AD  = ', F12.5, '  AUE  = ', F12.5, '  WL     = ', F12.5, /, &
           &  1X, 'APG = ', F12.5, '  AUE3 = ', F12.5, '  WL3    = ', F12.5, /, &
           &  1X, 'RHY = ', F12.5, '  RAUB = ', F12.5, '  BREITE = ', F12.5, /, &
           &  1X, 'HE1 = ', F12.5, '  HR3  = ', F12.5)


qw   = 0.
qd   = 0.
qges = 0.

iart = 0
!                          /* vollkommener ueberfall
!                             iart=1 (--> ueberstroemen)
!JK   ***************************************************************
!JK   BERECHNUNG DRUCKABFLUSS AN BRUECKE:
!JK   ***************************************************************

h0 = he1 - hr3    ! Wasserspiegelhoehe

re = 0.25 	! Einlaufverlust scharfk. an der Bruecke

IF (apg .gt. 0.) then
  rpf = 0.05
ELSE
  rpf = 0.
ENDIF

!JK   WENN BERECHNUNG NACH DARCY-WEISBACH
if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then

  !WP 02.05.2005, auch LAM wird initialisiert, um es weiter unten schon
  !WP in erster Schleife zur Relaxierung verwenden zu koennen.
  lam_neu = 0.001
  lam = 0.001

  dif = 1000.

  itermax = 20
  iter = 1

  it_lambda_bruecke: DO

    if (ABS(dif) < 0.001) EXIT it_lambda_bruecke

    !WP 02.05.2005, Relaxierung mit dem vorherigen Wert, fuehrt zu besserer Konvergenz!
    lam = (lam_neu + lam) / 2

    rr = breite * lam_neu / 4. / rhy

    beiw = re + rpf + rr
    vd = sqrt (2. * g * h0 / beiw)
    rey = vd * rhy * 4 / nue

    IF (lam .lt. 1.e-04) then
      EXIT it_lambda_bruecke
    ELSE
      !----------------------------
      ! ST 24.03.2005
      !write (*,*) 'In ABFLUSS. Vor GET_LAMBDA. vd = ', vd, '  Rey = ', rey
      !WP 24.06.2005
      !WP Folgende Formel ist laut Theorie-Anleitung zum Pasche Kern richtig
      !WP für den Widerstandsbeiwert an Brücken.
      lam_neu = (1. / ( - 2.03 * alog10 (4.4 / rey / (lam**0.5)  &
                 & + raub / (4. * rhy * 3.71) ) ) ) **2

      !lam_neu = GET_LAMBDA (vd, rhy, raub, 1.0)
    ENDIF

    dif = abs (lam - lam_neu)
    iter = iter + 1

    IF (iter.gt.itermax) then

      WRITE (UNIT_OUT_LOG, 15) iter, dif
      15 FORMAT (/1X, 'Konvergenzfehler in schleife 20 in SUB abfluss!',/, &
                & 1X, 'Nach ',I3,' Iterationen betraegt der fehler noch: ', F10.4)

      EXIT it_lambda_bruecke

    ENDIF

  END DO it_lambda_bruecke


  qd = ad * vd


!JK   WENN BERECHNUNG NACH GMS
ELSE

  ! Rauhigkeitsverl. Durchlass
  rr = 19.6 / (raub * raub) * breite / (rhy** (4. / 3.) )

  beiw = re+rpf + rr
  ! /* 1. --> rueckstaueffekt

  qd = ad * sqrt (2. * g * h0 / beiw)

ENDIF





!JK   ABFRAGE NACH UEBERFALLFLAECHE
IF (aue.gt.0.) then

  !JK   ******************************************************************
  !JK   BERECHNUNG UEBERFALLABFLUSS AN BRUECKE (Wehrabfluss):
  !JK   ******************************************************************

  !  1. Annahme: vollkommener ueberfall:  qw=c0*lw*h0**3/2 (c0-beiwert)

  h0 = aue / wl   ! Wasserspiegelhoehe

  c0 = 1.8 * (h0 / breite) **0.0544

  IF (c0.gt.1.705) c0 = 1.705

  qw = c0 * wl * (h0**3. / 2.)

  ! Ueberpruefen der grenzbedingung :
  ! vollkommener ueberfall <-> ueberstroemen
  IF (wl3.gt.0.) then

    hmit = aue3 / wl3

    tau00 = 2.141 / (c0** (2. / 3.) )

    tauu0 = 3.286 - 1.905 * tau00

    qw1 = qw / wl

    yk = ( (qw1 * qw1) / g) ** (1. / 3.)

    tauu = hmit / yk

    !JK     WAR SCHON DEAKTIVIERT:25.04.2000, JK
    !**       if(lein.eq.3)then
    !**       write(UNIT_OUT_LOG,'(''tau00='',f8.3,'' tauu0='',f8.3,'' tauu='',
    !**     *     f8.3,'' tau0u=''f8.3,'' hmit='',f8.3,'' yk'',f8.3)')
    !**     *            tau00,tauu0,tauu,tau0u,hmit,yk
    !**       endif

    itx = 1

    itxmax = 50

    xanf = 2. * tau00 / 3.

    fxanf = xanf**3. - tau00 * xanf**2. + 0.5

    IF (abs (fxanf) .le.0.001) then
      x2 = xanf
      GOTO 45
    ENDIF

    fx2 = 1000.

    it_wehrueberfall_bruecke: DO

      if (ABS(fx2) < 0.001) EXIT it_wehrueberfall_bruecke

      IF (itx.eq.1) then

        x1 = xanf + 0.1
        fx1 = x1**3. - tau00 * x1**2. + 0.5

        IF (abs (fx1) .le.0.001) then
          x2 = x1
          EXIT it_wehrueberfall_bruecke
        ENDIF

      ELSE

        x1 = x2
        fx1 = fx2

      ENDIF

      f1 = 3. * x1**2. - 2. * tau00 * x1

      IF (abs (f1) .lt.0.0001) f1 = 0.0001

      x2 = x1 - fx1 / f1

      fx2 = x2**3. - tau00 * x2**2. + 0.5

      !JK         WAR SCHON DEAKTIVIERT: 25.04.2000, JK
      !**         if(lein.eq.3)then
      !**            write(UNIT_OUT_LOG,'(''i='',i3,'' x1='',f8.3,'' fx1='',f8.3,
      !**     1            '' f1='',f8.3, '' x2='',f8.3,'' fx2='',f8.3)')
      !**     2                  itx,x1,fx1,f1,x2,fx2
      !**         endif

      itx = itx + 1


      IF (itx.gt.itxmax) then

        !UT  ACHTUNG f2 ALS FEHLER AN DIESER STELLE NICHT BEKANNT
        !UT  ITX = ANZAHL DER ITERATIONEN, ES MUESSTE fx2 HEISSEN
        ! alt    write (UNIT_OUT_LOG,16) itx,f2, bis 18.08.2000
        WRITE (UNIT_OUT_LOG, 16) itx, fx2
        16 FORMAT (/1X, 'Konvergenzfehler in schleife 40 in SUB abfluss!',/, &
                  & 1X, 'Nach ',I3,' Iterationen betraegt der fehler noch: ', F10.4)

        GOTO 999

      ENDIF

    END do it_wehrueberfall_bruecke

    45 CONTINUE

    taumax = x2

    IF (tauu.gt.taumax) then

    ! 2. Annahme: unvollkommener Ueberfall --> breites wehr -->
    ! gewellter Abfluss --> weiter mit normber

      iart = 1

    ENDIF

  !JK     ENDIF ZU (wl3.gt.0.)
  ENDIF

!JK   ENDIF ZU (aue.gt.0.)
ENDIF


!JK   ******************************************************************
!JK   BERECHNUNG GESAMTABFLUSS qges
!JK   ******************************************************************


999 CONTINUE

qges = qd + qw


write (UNIT_OUT_LOG, 1001) qd, qw, qges, iart
1001 format (/1X, 'SUB ABFLUSS, berechnete Variablen:', /, &
           &  1X, '-----------------------------------', /, &
           &  1X, 'QD   = ', F12.5, '  QW   = ', F12.5, '  QGES   = ', F12.5, /, &
           &  1X, 'IART = ', I2, //)


END SUBROUTINE abfluss

