!     Last change:  WP    1 Aug 2006    4:42 pm
!--------------------------------------------------------------------------
! This code, w_ber.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - w_ber
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

SUBROUTINE w_ber (he, qw, np, nz, idr1, nblatt)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE W_BER                                                  
!**                                                                     
!**   BESCHREIBUNG: dieses Programm berechnet den Wehrueberfall in einem
!**                 Wehrprofil bei der Wasserspiegellinienberechnung    
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   kopf      (nblatt,nz,jw5,jw7,idr1)
!**   wspow     Berechnet den Wasserspiegel im Oberwasser aus der Energi
!**   g_wehr    Berechnet die Wehrgeometrie                             
!**   beiwert   Berechnet den Ueberfallbeiwert des Wehres               
!**                                                                     
!***********************************************************************
!                                                                       
!     geschrieben J. Csocsan  August '93                                
!                                                                       
!     dokumentiert und ueberarbeitet Ole Holm / Marco Wichers November 2
!                                                                       
!***********************************************************************
!**                                                                     
!**   IN DIESER ROUTINE VERWENDETE VARIABLEN                            
!**   --------------------------------------                            
!**   a_uw(maxw)          -- durchstroemte Flaeche im Unterwasser       
!**   auew(maxw)          -- Ueberfallflaeche am Wehr                   
!**   beiw(maxw)          -- Abflussbeiwert                             
!**   br(maxkla)                                                        
!**   brg(maxger,maxkla)                                                
!**   cq(maxw)            -- Ueberfallbeiwert mue                       
!**   f(maxkla)                                                         
!**   fgesp(maxger)       -- insgesamt durchflossene Flaeche            
!**   fp(maxger,maxkla)                                                 
!**   froudp(maxger)      -- Froud-Zahl                                 
!**   h_uw(maxw)          -- Wasserspiegelhoehe im Unterwasser          
!**   hbors(maxger)       -- Borda'sche Verlusthoehe                    
!**   hein(maxger)        -- Einlaufverlusthöhe                         
!**   hen(maxger)         -- Energiehoehe                               
!**   hgrw(maxw)          -- Grenztiefe                                 
!**   hokw(maxkla)        -- Hoehe der Wehroberkante im Profil          
!**   hort(maxger)        -- Auslaufverlusthoehe                        
!**   hrs(maxger)         -- Reibungsverlusthoehe                       
!**   hs(maxger)          -- Gesamtverlusthoehe                         
!**   htrw(maxw)          -- z-Koordinate der Trennlinie Wehr (wird nie 
!**   huew(maxw)          -- Ueberfallhöhe am Wehr (Wsp.-Höhe im Wehrsch
!**   hvs(maxger)         -- Geschwindigkeitsverlusthoehe               
!**   hwmin(maxw)         -- minimale Wehrhoehe                         
!**   hwmin_uw(maxw)      -- minimale Wasserspiegelhoehe im Unterwasser 
!**   ianfw(maxw)         -- Punktnummer des Wehranfangs im Profil      
!**   idr1                                                              
!**   iendw(maxw)         -- Punktnummer des Wehrendes im Profil        
!**   igrenz(maxger)                                                    
!**   iwehr               -- Profilparamter (Wehr vorhanden/nicht vorhan
!**   iwmin                                                             
!**   lw(maxw)            -- Breite des Wehrs quer zur Fliessrichtung   
!**   nokw                -- Anzahl der Punkte der Wehroberkante im Prof
!**   nwfd                -- Anzahl der Wehrfelder                      
!**   q_w(maxw)           -- Abfluss ueber das Wehr                     
!**   qs(maxger)          -- Durchfluss                                 
!**   qt(maxkla)          -- Teilabfluss                                
!**   qtp(maxger,maxkla)                                                
!**   ra(maxkla)                                                        
!**   ra1(maxkla)                                                       
!**   rb(maxkla)                                                        
!**   rk(maxkla)                                                        
!**   rkp(maxger,maxkla)                                                
!**   rkw(maxw)           -- Radius des rundkronigen Wehres             
!**   ts1(maxkla)                                                       
!**   ts2(maxkla)                                                       
!**   u(maxkla)           -- benetzter Teilumfang                       
!**   up(maxger,maxkla)                                                 
!**   v(maxkla)           -- Teilgeschwindigkeit                        
!**   vmp(maxger)         -- mittlere Geschwindigkeit                   
!**   vp(maxger,maxkla)                                                 
!**   wart - Wehrart                                                    
!**   wl(maxw)            -- Laenge des Wehres in Fliessrichtung        
!**   wl_uw(maxw)         -- Wehrbreite in Unterwasser                  
!**   wsp(maxger)         -- Wasserspiegel                              
!**   xokw(maxkla)        -- x-Koordinate der Wehroberkante             
!**   xtrw(maxw)          -- x-Koodinate der Trennlinie Wehr            
!***********************************************************************
                                                                        
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS
USE MOD_INI

! COMMON-Block /ERG/ ----------------------------------------------------------
REAL 		:: wsp (maxger), hen (maxger), qs (maxger), fgesp (maxger)
REAL 		:: froudp (maxger), hvs (maxger), hrs (maxger), hs (maxger)
REAL 		:: fp (maxger, maxkla), up (maxger, maxkla), vp (maxger, maxkla)
REAL 		:: qtp (maxger, maxkla), rkp (maxger, maxkla), fbwp (maxger, maxkla)
REAL 		:: brp (maxger, maxkla)
REAL		:: vmp (maxger), hbors (maxger), hein (maxger), hort (maxger), brg (maxger)
INTEGER 	:: igrenz (maxger)
COMMON / erg / wsp, hen, qs, fgesp, froudp, hvs, hrs, hs, fp, up, &
             & vp, qtp, rkp, fbwp, brp, vmp, hbors, hein, hort, igrenz, brg
! -----------------------------------------------------------------------------


! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------


! COMMON-Block /P3/ -----------------------------------------------------------
INTEGER 	:: isohl, iming
REAL            :: hming
COMMON / p3 / isohl, hming, iming
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /WEHR/ ---------------------------------------------------------
REAL 		:: xokw (maxkla), hokw (maxkla)
INTEGER 	:: nokw, iwmin
REAL            :: hokwmin
CHARACTER(LEN=1):: iwehr
REAL 		:: xtrw (maxw), htrw (maxw)
INTEGER         :: nwfd
INTEGER 	:: iendw (maxw), ianfw (maxw)
COMMON / wehr / xokw, hokw, nokw, iwmin, hokwmin, iwehr, xtrw, htrw, nwfd, iendw, ianfw
! -----------------------------------------------------------------------------


! COMMON-Block /WEHR2/ --------------------------------------------------------
REAL 		:: beiw (maxw), rkw (maxw), lw (maxw)
CHARACTER(LEN=2):: wart                         ! = 'bk', 'rk', 'bw', 'sk'
COMMON / wehr2 / beiw, rkw, lw, wart
! -----------------------------------------------------------------------------



REAL :: hgrw (maxw), q_w (maxw) 
                                                                        

! ------------------------------------------------------------------
! Uebergabegroessen der Geometrieberechnung:
! ------------------------------------------------------------------
REAL :: auew (maxw), huew (maxw), wl (maxw), hwmin (maxw) 
REAL :: a_uw (maxw), h_uw (maxw), wl_uw (maxw), hwmin_uw (maxw) 
                                                                        
REAL cq (maxw) 

CHARACTER(LEN=1) :: idr1 
                                                                        
!HW ------------------------------------------------------------------
!HW Kennziffer der Ueberfallart:                                      
!HW iueart = 0     - vollkommener Ueberfall                           
!HW iueart = 1     - unvollkommener Ueberfall                         
!HW iueart = 2     - Ueberstroemen                                    
!HW  ------------------------------------------------------------------
!HW Ueberfallart der einzelnen Felder vektoriell zusammengefasst      

INTEGER :: iueartfd (maxw) 
CHARACTER(LEN=12) :: uearttxt 

! ******************************************************************
! Programmanfang                                   
! ******************************************************************
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        
      WRITE (UNIT_OUT_LOG, '(/,''Programmstart Subroutine w_ber'')')
      WRITE (UNIT_OUT_LOG, '(/,''=============================='')')

                                                                        
!HW    Ausgabe der Eingabewerte zur Wehrberechnung in Ergebnisfile      
!HW   ------------------------------------------------------------------
      WRITE (UNIT_OUT_TAB, '(/,5x,''Eingabedaten der Wehrberechnung'')') 
      WRITE (UNIT_OUT_TAB, '(5x,''-------------------------------'')')
      IF (wart.eq.'rk') then
        WRITE (UNIT_OUT_TAB, '(5x,''Wehrtyp: rundkroniges Wehr'')') 
        WRITE (UNIT_OUT_TAB, '(5x,''Feld-Nr.'',5x,''Radius'')')
        DO 100 j = 1, nwfd
          WRITE (UNIT_OUT_TAB, '(5x,i2,6x,3f10.2)') j, rkw (j) 
  100   END DO
      ELSEIF (wart.eq.'bk') then 
        WRITE (UNIT_OUT_TAB, '(5x,''Wehrtyp: breitkroniges Wehr'')') 
        WRITE (UNIT_OUT_TAB, '(5x,''Feld-Nr.'',5x,''Breite'')')
        DO 110 j = 1, nwfd
          WRITE (UNIT_OUT_TAB, '(5x,i2,6x,3f10.2)') j, lw (j) 
  110   END DO
      ELSEIF (wart.eq.'bw') then 
        WRITE (UNIT_OUT_TAB, '(5x,''Wehr mit vorgegebenem Ueberfallbeiwert'')') 
        WRITE (UNIT_OUT_TAB, '(5x,''Feld-Nr.'',5x,''mue'')')
        DO 120 j = 1, nwfd
          WRITE (UNIT_OUT_TAB, '(5x,i2,6x,3f10.3)') j, beiw (j) 
  120   END DO
      ELSEIF (wart.eq.'sk') then 
        WRITE (UNIT_OUT_TAB, '(5x,''Wehrtyp: scharfkantiges Wehr'')') 
      ELSE
        WRITE (UNIT_OUT_TAB, '(/,5x,''Wehrtyp unbestimmt'')') 
        WRITE (UNIT_OUT_TAB, '(5x,''Fehler in Eingabe!'')')
      ENDIF
!HW   ------------------------------------------------------------------

dx = 0.1
itmax2000 = 100
g2 = sqrt (g * 2.)
huw = 0.0
wh = 0.0

!WP 01.02.2005 Hier unnoetig, da weiter unten neu mit itmax = 50
!itmax = 500

hr1 = wsp (np - 1)
hen1 = hen (np - 1)
v_uw = SQRT (2. * g * (hen1 - hr1) )
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        
WRITE (UNIT_OUT_LOG, '(''Wasserspiegel aus Profil n-1: hr1  = '',f8.4)') hr1
WRITE (UNIT_OUT_LOG, '(''Energiehoehe  aus Profil n-1: hen1 = '',f8.4)') hen1

psieins = 0.0
psiorts = 0.0

!      Genauigkeiten der Berechnung:                                    
                                                                        
eps_q = 0.0001
!     ------------------------------------------------------------------
!      Bestimmung der Wassertiefe ueber Wehroberkannte im Unterwasser   
!     ------------------------------------------------------------------
                                                                        
!HW    Beginn der if-Abfrage hr1 > hokwmin                              
!HW    Schreiben in Kontrollfile                                        
WRITE (UNIT_OUT_LOG, '(''Minimale Hoehe Wehr-OK: hokwmin = '',f8.4)') hokwmin

IF (hr1.gt.hokwmin) then

  !JK        SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(/,''Unterwasser'')')
  WRITE (UNIT_OUT_LOG, '(''hr1='',f8.4)') hr1

  !HW    Aufruf der Subroutine g_wehr
  CALL g_wehr (hr1, ifehl, a_uw, h_uw, wl_uw, hwmin_uw)

ENDIF
                                                                        
!      -----------------------------------------------------------------
!      Iterieren he - Energiehoehe:                                     
!      -----------------------------------------------------------------
!HW    SCHREIBEN IN KONTROLLFILE                                        
WRITE (UNIT_OUT_LOG, '(/,''Beginn der Energiehoehen-Iteration he'')')

it1000 = 0

!WP 01.02.2005 Wird bereits in Modul DIM_VARIABLES definiert (=99),
!WP deshalb hier nicht!
!itmax = 50

it1000max = 50
dif_e = 10.

absmax1 = 1.e+06
                                                                        
DO 1000 WHILE(dif_e.gt.0.0001)
   absmax = 1.e+06
                                                                        
   !HW    Beginn der do-Schleife dif_e > 0.001 (Energiehoehen-Iteration)
   it1000 = it1000 + 1

   IF (it1000.gt.it1000max) then
                                                                        
     !JK           SCHREIBEN IN KONTROLLFILE
     WRITE (UNIT_OUT_LOG, '('' Konvergenz in der Schleife 1000 nicht gefunden !'')')

     !JK              WAR SCHON DEAKTIVIERT, 02.05.00, JK
     !**              print*,'keine Konvergenz in der Wehrberechnung!'
     !**              print*,'Schleife1000 -> STOP '
     !**              stop 'programende'
                                                                        
   ENDIF
                                                                        
      IF (it1000.eq.1) then 
        IF ( (hen1 - hokwmin) .gt.0.1) then 
          he = hen1 
        ELSE 
          he = hokwmin + 0.1 
        ENDIF 
                                                                        
      ELSEIF (it1000.le.it1000max) then 
                                                                        
      ! Energiehoehe = Mittelwert(hen-wehr <-> hen-wsp)
                                                                        
        he = (he+he_q) / 2. 
                                                                        
        IF (he.lt.hen1) he = hen1 
                                                                        
      ELSE 
                                                                        
        he = he_opt 
                                                                        
        GOTO 4001 
                                                                        
                                                                        
      ENDIF 
                                                                        

      1010 CONTINUE

      !HW    SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(''Neue Energiehoehe he = '',f8.4)') he

      !HW    Aufruf der Subroutine wspow
      CALL wspow (he, strbr, qw, q1, hrow, hv, rg, indmax, hvst, hrst,  &
      psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz, idr1)
                                                                        
      !HW    SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,''Werte aus wspow'')')
      WRITE (UNIT_OUT_LOG, '(''he = '',f8.4)') he
      WRITE (UNIT_OUT_LOG, '(''qw = '',f8.4)') qw
      WRITE (UNIT_OUT_LOG, '(''q1 = '',f8.4)') q1

                                                                        
      !JK        BERECHNUNG NACH DARCY-WEISBACH
      if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
        q_wehr = qt (2) 
        v_ow = v (2) 
      !JK        BERECHNUNG NACH GMS
      ELSE 
        q_wehr = qt (indfl) 
        v_ow = v (indfl) 
      ENDIF 
                                                                        
      !HW    SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach DW/GMS'')')
      WRITE (UNIT_OUT_LOG, '(''q_wehr = '',f8.4)') q_wehr
      WRITE (UNIT_OUT_LOG, '(''v_ow   = '',f8.4)') v_ow

                                                                        
      h_v = v_ow**2. / (2. * g) 
                                                                        
                                                                        
      dif_ok = he-h_v 
      dif_hv = h_v / 10. 
                                                                        
      !HW    SCHREIBEN IN KONTROLLFILE

      WRITE (UNIT_OUT_LOG, '(''h_v    = '',f8.4)') h_v 
      WRITE (UNIT_OUT_LOG, '(''dif_ok = '',f8.4)') dif_ok
      WRITE (UNIT_OUT_LOG, '(''dif_hv = '',f8.4)') dif_hv

      IF ( (dif_ok - hokwmin) .le.dif_hv) then 
                                                                        
        he = he+1.15 * h_v 
                                                                        
        GOTO 1010 
                                                                        
      ENDIF 
                                                                        
      !JK        SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,''it1000='',i4,'' he='',f8.4,   &
        &  '' q_wehr='',f8.4,'' h_v='',f8.4)') it1000, he, q_wehr, h_v
                                                                        

      !HW    SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,''Ende der Energiehoehen-Iteration'')')

      !     ------------------------------------------------------------------
      !      Iterieren q_wehr - Wehrueberfall:
      !     ------------------------------------------------------------------
      !HW    SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,''Beginn Abfluss-Iteration - Wehrüberfall'')')

      dif_q = 10. 
                                                                        
      it2000 = 0 
                                                                        
      !HW    Beginn do-Schleife abs(dif_q).gt.0.0001 (Abfluss-Iteration)
      DO 2000 WHILE(abs (dif_q) .gt.0.0001) 
                                                                        

                                                                        
        !HW    SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(''dif_q = '',f8.4)') dif_q

                                                                        
        it2000 = it2000 + 1
                                                                        
        IF (it2000.gt.itmax) then

          PRINT * , 'keine Konvergenz in der Wehrberechnung!'
          PRINT * , 'Schleife2000 -> STOP '
          STOP 'programende'
        ENDIF
                                                                        
        IF (it2000.eq.1) he_q = he
                                                                        
        !JK            SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(/,''---------------------------------'')')
        WRITE (UNIT_OUT_LOG, '(/,'' it2000='',i4,'' he_q='',f8.4, '' nwfd='',i3)') it2000, he_q, nwfd

                                                                        
                                                                        
        !HW    Aufruf der Subroutine g_wehr
                                                                        
        CALL g_wehr (he_q, ifehl, auew, huew, wl, hwmin)
                                                                        
        q_ue = 0.0
                                                                        
        !HW    Summation des Abflusses für alle Wehrfelder
        DO 2010 j = 1, nwfd
                                                                        
          !HW    SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '(/,''Berechnung Ueberfallmenge fuer alle Wehrfelder'')')
          WRITE (UNIT_OUT_LOG, '(''Wsp. im Wehrscheitel: huew = '',f8.4)') huew (j)

                                                                        
          IF (huew (j) .lt.0.0001) then
                                                                        
            q_w (j) = 0.0
            hgrw (j) = 0.0
                                                                        
          ELSE
                                                                        
            !HW    vollkommener Ueberfall
            !HW    SCHREIBEN IN KONTROLLFILE

            WRITE (UNIT_OUT_LOG, '(/,''Vollkommener Ueberfall'')') 

            iueart = 0
                                                                        
            IF (wart.eq.'sk') then
              wh = hwmin (j) - hming
                                                                        
              !HW    SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '(/,''scharfkantiges Wehr'')')

            ENDIF
                                                                        
            !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
            CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
            !HW    SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
            WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
            WRITE (UNIT_OUT_LOG, '(''--------------------------'')')
            WRITE (UNIT_OUT_LOG, '(''g2   = '',f8.4)') g2
            WRITE (UNIT_OUT_LOG, '(''cq   = '',f8.4)') cq (j)
            WRITE (UNIT_OUT_LOG, '(''huew = '',f8.4)') huew (j)
            WRITE (UNIT_OUT_LOG, '(''hv   = '',f8.4)') hv

            !HW    Berechnung des Wehrueberfalls nach DU BUAT
                                                                        
            q_b = (2. / 3.) * g2 * cq (j) * (huew (j) + h_v) **1.5
                                                                        
            !HW    SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(/,''Breite des Wehres wl:'')')
            WRITE (UNIT_OUT_LOG, '(''wl = '',f8.4)') wl (j)

                                                                        
            q_w (j) = q_b * wl (j)
            hgrw (j) = (q_b**2. / g) ** (1. / 3.)

          !HW            endif zu huew(j) .lt. 0.0001
          ENDIF
                                                                        
          q_ue = q_ue+q_w (j)
                                                                        
          !HW    SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '(''Ergebnis der Summation bis hier:q_ue='',f8.4)') q_ue

                                                                        
        2010 END DO
                                                                        
        !HW    SCHREIBEN IN KONTROLLFILE

        WRITE (UNIT_OUT_LOG, '(/,''Endergebnisse fuer alle Felder:'')') 
        WRITE (UNIT_OUT_LOG, '(''q_ue   = '',f8.4)') q_ue
        WRITE (UNIT_OUT_LOG, '(''i      = '',10i8)')  (i, i = 1, nwfd)
        WRITE (UNIT_OUT_LOG, '(''q_w(i) = '',10f8.4)') (q_w (i) , i = 1, nwfd)
        WRITE (UNIT_OUT_LOG, '(''cq(i)  = '',10f8.4)')  (cq (i) , i = 1, nwfd)
        WRITE (UNIT_OUT_LOG, '(''hgrw(i)= '',10f8.4)') (hgrw (i) , i = 1, nwfd)

                                                                        
        IF (hr1.le.hokwmin) then
                                                                        
          IF (abs (q_ue-q_wehr) .le.eps_q) then
            dif_q = 0.0
            !JK      ABSPEICHERN GUENSTIGER WERTE
                                                                        
            !HW    SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(/,''Abspeichern guenstiger Werte'')')
            GOTO 2400

          ELSE
                                                                        
            !HW    SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(/,''Schaetzen neuer Energiehoehe'')')

            !JK        SCHAETZEN NEUER ENERGIEHOEHE
            GOTO 2100

          ENDIF 
                                                                        
        !JK            ELSE ZU (hr1.le.hokwmin)
        ELSE
                                                                        
          q_ue = 0.0

          DO 2020 j = 1, nwfd
                                                                        
            IF (h_uw (j) .gt.0.0005) then
                                                                        
              !HW    Berechnung der dimensionslosen Hoehen
                                                                        
              !HW    SCHREIBEN IN KONTROLLFILE

              WRITE (UNIT_OUT_LOG, '(/,''Berechnung der dimensionlosen Hoehen'')')

              tauu = h_uw (j) / hgrw (j)
              taoo = huew (j) / hgrw (j)

              !MD    taoo = (huew (j) + h_v ) / hgrw (j)
              !MD    taoo = huew (j) / hgrw (j)   <- MUSS ENERGIEHÖHE DES OBERWASSERS SEIN


              !JK                    SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '(/,''j='',i3,'' tauu='',f8.4, &
                 & '' taoo='',f8.4)') j, tauu, taoo

              !HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONIGE WEHR MITTELS DER
              !HW    IMPULSBILANZ GEMAESS BWK/MERKBLATT 1, Seite 55, Abb. 3.34
                                                                        
              IF (wart.eq.'bk') then
                                                                        
                !HW    SCHREIBEN IN KONTROLLFILE
                WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
                WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')

                !MD   FEHLER IN DER FORMEL --> KORRIGIERT                                                                   
                ! taugrenz = SQRT ( (1 + hwmin (j) / hgrw (j) ) **2 + 2 + 1  / g * (v_uw**2 / hgrw (j) ) **2) &             
                !             &  - v_uw**2 / (g * hgrw (j)) - hwmin (j) / hgrw (j)                                          
                                                                                                                            
                taugrenz = SQRT ( (1 + (hwmin (j) / hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) * g)) **2)    &               
                              &  - (v_uw**2 / (g * hgrw (j))) - (hwmin (j) / hgrw (j))                                      

                !HW    SCHREIBEN IN KONTROLLFILE
                WRITE (UNIT_OUT_LOG, '(/,''Grenzwert für überströmen nach Imp.bilanz'')')
                WRITE (UNIT_OUT_LOG, '(''taugrenz = '',f8.4)') taugrenz

                !HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz
                                                                        
              IF (tauu.gt.taugrenz) then 
                                                                        
!                              Ueberstroemen                            
                iueart = 2 
!JK                            ZU UEBERSTROEMEN                         
                GOTO 2040 
              ELSE 
!JK                            ZU VOLLKOMMENER UEBERFALL                
                GOTO 2030 
              ENDIF 
                                                                        
            ENDIF 
                                                                        
!HW   ----------------------ENDE BREITKRONIGES WEHR---------------------
                                                                        
!JK                    ZU UEBERSTROEMEN                                 
            IF (tauu.ge.2.or.taoo.ge.2) goto 2040 
                                                                        
!                          Grenze Ueberstroemen - Ueberfallen:          
                                                                        
            tauo = 3.286 - 1.905 * taoo 
                                                                        
              !HW                        SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '(''tauo='',f8.4)') tauo

                                                                        
            IF (tauo.ge.tauu.and.tauo.le.1.0) then 
!                             vollkommener Ueberfall                    
              GOTO 2030 
            ELSEIF (tauo.ge.2.) then 
                                                                        
      write (UNIT_OUT_LOG, '(''Widerspruch !!!!: '',              &
     &               ''tauu < 2.0 <=> tauoo > 2.0'',                    &
     &               /,''Annahme Ueberstroemen !'')')
                                                                        
!JK                           ZU UEBERSTROEMEN                          
              GOTO 2040 
                                                                        
            ENDIF 
                                                                        
                                                                        
!                          Unvollkommener Ueberfall                     
!                          ------------------------                     
                                                                        
            !HW    SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(/,''Unvollkommener Ueberfall'')')

                                                                        
            iueart = 1 
                                                                        
            !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
            CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)

            !HW    SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w (Wehrueberfall)'')')

            q_w (j) = q_w (j) * cm 
                                                                        
            !JK                        SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4,'' cm='',f8.4)') q_w (j), cm

                                                                        
            GOTO 2030 
                                                                        
!                          Ueberstroemen                                
!                          -------------                                
 2040       iueart = 2 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        
            WRITE (UNIT_OUT_LOG, '(/,''Ueberstroemen'')')
            WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w nach Knapp S.304'')')

            q_w (j) = a_uw (j) * g2 * (huew (j) - h_uw (j) ) **0.5
                                                                        
!JK                        SCHREIBEN IN KONTROLLFILE                    

                                                                        
      WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4,                                   &
     &        '' Ueberstroemen'')')  (q_w (j) )

                                                                        
!JK                    ENDIF ZU (tauu.ge.2.or.taoo.ge.2)                
          ENDIF 
                                                                        
 2030     q_ue = q_ue+q_w (j) 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

            WRITE (UNIT_OUT_LOG, '(''Summation q_ue bis hier: q_ue ='',f8.4)') q_ue

                                                                        
 2020   END DO 
                                                                        
!JK                SCHREIBEN IN KONTROLLFILE                            
        WRITE (UNIT_OUT_LOG, '(/,''q_ue-m ='',f8.4)') q_ue

                                                                        
!JK            ENDIF ZU (hr1.le.hokwmin)                                
      ENDIF 
                                                                        
      IF (abs (q_ue-q_wehr) .le.eps_q) then 
        dif_q = 0.0 
        GOTO 2400 
      ENDIF 
                                                                        
!              Schaetzen neue Energiehoehe fuer Wehrabfluss             
                                                                        
 2100 hea_q = he_q + dx 
      heb_q = he_q - dx 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

      WRITE (UNIT_OUT_LOG, '(/,''--------------------------------------------'')')
      WRITE (UNIT_OUT_LOG, '(''Schaetzen neuer Energiehoehen und'')')
      WRITE (UNIT_OUT_LOG, '(''neue Berechnung mit diesen Werten'')')
      WRITE (UNIT_OUT_LOG, '(''--------------------------------------------'')')

                                                                        
      IF (heb_q.le.hen1) heb_q = hen1 
      IF (heb_q.le.hokwmin) heb_q = (he_q + hokwmin) / 2. 
                                                                        
!       ................................................................
!        ---> hea_q                                                     
!       ................................................................
                                                                        
      CALL g_wehr (hea_q, ifehl, auew, huew, wl, hwmin)
                                                                        
      q_uea = 0.0 
                                                                        
!HW    Beginn der Abfluss-Iteration fuer den geschaetzten unteren Wert  
                                                                        
      DO 2110 j = 1, nwfd 
                                                                        
        IF (huew (j) .lt.0.0001) then 
                                                                        
          q_w (j) = 0.0 
                                                                        
        ELSE 
                                                                        
          iueart = 0 
                                                                        
!JK                    SCHARFKANTIGES WEHR                              
          IF (wart.eq.'sk') wh = hwmin (j) - hming 
                                                                        
          !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
          CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

          WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
          WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
          WRITE (UNIT_OUT_LOG, '(''g2    = '',f8.4)') g2
          WRITE (UNIT_OUT_LOG, '(''cq    = '',f8.4)') cq (j)
          WRITE (UNIT_OUT_LOG, '(''huew  = '',f8.4)') huew (j)
          WRITE (UNIT_OUT_LOG, '(''q_uea ='',f8.4)') q_uea
          WRITE (UNIT_OUT_LOG, '(''hv    = '',f8.4)') hv

                                                                        
          q_b = (2. / 3.) * g2 * cq (j) * (huew (j) + h_v) **1.5
                                                                        
          q_w (j) = q_b * wl (j) 
          hgrw (j) = (q_b**2. / g) ** (1. / 3.) 
                                                                        
!HW                 endif zu huew(j) .lt. 0.0001                        
        ENDIF 
                                                                        
        q_uea = q_uea + q_w (j) 
                                                                        
 2110 END DO 
                                                                        
      IF (hr1.lt.hokwmin) then 
                                                                        
        IF (abs (q_uea - q_wehr) .le.eps_q) goto 2200 
                                                                        
      ELSE 
                                                                        
        q_uea = 0.0 
                                                                        
        DO 2120 j = 1, nwfd 
                                                                        
          IF (h_uw (j) .gt.0.0005) then 
                                                                        
            tauu = h_uw (j) / hgrw (j) 
                                                                        
            taoo = huew (j) / hgrw (j) 
                                                                        
!HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONOIGE WEHR MITTELS DE
!HW    IMPULSBILANZ GEMÄSS BWK/MERKBLATT 1, Seite 55, Abb. 3.34         
                                                                        
            IF (wart.eq.'bk') then 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        
              WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
              WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')

              !MD   FEHLER IN DER FORMEL --> KORRIGIERT                                                       
              ! taugrenz = SQRT ( (1 + hwmin (j) / hgrw (j) ) **2 + 2 + 1  / g * (v_uw**2 / hgrw (j) ) **2) & 
              !             &  - v_uw**2 / (g * hgrw (j)) - hwmin (j) / hgrw (j)                              
                                                                                                              
              taugrenz = SQRT ( (1 + (hwmin (j) / hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) * g)) **2)    &   
                             &  - (v_uw**2 / (g * hgrw (j))) - (hwmin (j) / hgrw (j))                         


!HW    SCHREIBEN IN KONTROLLFILE                                        

              WRITE (UNIT_OUT_LOG, '(/,''Grenzwert für überströmen nach Imp.bilanz'')')
              WRITE (UNIT_OUT_LOG, '(''taugrenz = '',f8.4)') taugrenz

                                                                        
!HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz     
                                                                        
              IF (tauu.gt.taugrenz) then 
                                                                        
!                              Ueberstroemen                            
                iueart = 2 
!JK                            ZU UEBERSTROEMEN                         
                GOTO 2140 
              ELSE 
!JK                            ZU VOLLKOMMENER UEBERFALL                
                GOTO 2130 
              ENDIF 
                                                                        
            ENDIF 
                                                                        
!HW   ----------------------ENDE BREITKRONIGES WEHR---------------------
                                                                        
                                                                        
            IF (tauu.ge.2.or.taoo.ge.2) goto 2140 
                                                                        
!                          Grenze Ueberstroemen - Ueberfallen:          
                                                                        
            tauo = 3.286 - 1.905 * taoo 
                                                                        
            IF (tauo.ge.tauu.and.tauo.le.1.0) then 
!                             vollkommener Ueberfall                    
              GOTO 2130 
            ELSEIF (tauo.ge.2.) then 
                                                                        
!JK                             SCHREIBEN IN KONTROLLFILE               
      write (UNIT_OUT_LOG, '(''Widerspruch !!!!: '',              &
     &                 ''tauu < 2.0 <=> tauoo > 2.0'',                  &
     &                 /,''Annahme Ueberstroemen   !'')')
                                                                        
              GOTO 2140 
                                                                        
            ENDIF 
                                                                        
!                             Unvollkommener Ueberfall                  
!                             ------------------------                  
            iueart = 1 
                                                                        
            !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
            CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
            q_w (j) = q_w (j) * cm 
                                                                        
            GOTO 2130 
                                                                        
!                             Ueberstroemen                             
!                             -------------                             
 2140       iueart = 2 
                                                                        
            q_w (j) = a_uw (j) * g2 * (huew (j) - h_uw (j) ) **0.5 
                                                                        
          ENDIF 
                                                                        
 2130     q_uea = q_uea + q_w (j) 
                                                                        
 2120   END DO 
                                                                        
!JK            ENDIF ZU (hr1.lt.hokwmin)                                
      ENDIF 
                                                                        
!       ................................................................
!        ---> heb_q                                                     
!       ................................................................
                                                                        
 2200 CALL g_wehr (heb_q, ifehl, auew, huew, wl, hwmin)
                                                                        
      q_ueb = 0.0 
                                                                        
!HW    Beginn der Abfluss-Iteration fuer den oberen geschätzten Wert    
                                                                        
      DO 2210 j = 1, nwfd 
                                                                        
        IF (huew (j) .lt.0.0001) then 
                                                                        
          q_w (j) = 0.0 
                                                                        
        ELSE 
                                                                        
          iueart = 0 
                                                                        
!JK                    SCHARFKANTIGES WEHR                              
          IF (wart.eq.'sk') wh = hwmin (j) - hming 
                                                                        
          !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
          CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

          WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
          WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
          WRITE (UNIT_OUT_LOG, '(''g2   = '',f8.4)') g2
          WRITE (UNIT_OUT_LOG, '(''cq   = '',f8.4)') cq (j)
          WRITE (UNIT_OUT_LOG, '(''huew = '',f8.4)') huew (j)
          WRITE (UNIT_OUT_LOG, '(''hv   = '',f8.4)') hv

                                                                        
          q_b = 2. / 3. * g2 * cq (j) * (huew (j) + h_v) **1.5
                                                                        
          q_w (j) = q_b * wl (j) 
          hgrw (j) = (q_b**2. / g) ** (1. / 3.) 
                                                                        
!HW                endif zu huew(j) .lt. 0.0001                         
        ENDIF 
                                                                        
        q_ueb = q_ueb + q_w (j) 
                                                                        
 2210 END DO 
                                                                        
      IF (hr1.lt.hokwmin) then 
                                                                        
        IF (abs (q_ueb - q_wehr) .le.eps_q) goto 2300 
                                                                        
      ELSE 
                                                                        
        q_ueb = 0.0 
                                                                        
                                                                        
        DO 2220 j = 1, nwfd 
                                                                        
          IF (h_uw (j) .gt.0.0005) then 
                                                                        
            tauu = h_uw (j) / hgrw (j) 
                                                                        
            taoo = huew (j) / hgrw (j) 
                                                                        
                                                                        
!HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONOIGE WEHR MITTELS DE
!HW    IMPULSBILANZ GEMÄSS BWK/MERKBLATT 1, Seite 55, Abb. 3.34         
                                                                        
            IF (wart.eq.'bk') then 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        
              WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
              WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')


              !MD   FEHLER IN DER FORMEL --> KORRIGIERT                                                          
              ! taugrenz = SQRT ( (1 + hwmin (j) / hgrw (j) ) **2 + 2 + 1  / g * (v_uw**2 / hgrw (j) ) **2) &    
              !             &  - v_uw**2 / (g * hgrw (j)) - hwmin (j) / hgrw (j)                                 
                                                                                                                 
              taugrenz = SQRT ( (1 + (hwmin (j) / hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) * g)) **2)    &      
                            &  - (v_uw**2 / (g * hgrw (j))) - (hwmin (j) / hgrw (j))                             


!HW    SCHREIBEN IN KONTROLLFILE                                        

              WRITE (UNIT_OUT_LOG, '(/,''Grenzwert fuer Ueberstroemen nach Imp.bilanz'')')
              WRITE (UNIT_OUT_LOG, '(''taugrenz = '',f8.4)') taugrenz

                                                                        
!HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz     
                                                                        
              IF (tauu.gt.taugrenz) then 
                                                                        
!                              Ueberstroemen                            
                iueart = 2 
!JK                            ZU UEBERSTROEMEN                         
                GOTO 2240 
              ELSE 
!JK                            ZU VOLLKOMMENER UEBERFALL                
                GOTO 2230 
              ENDIF 
                                                                        
            ENDIF 
                                                                        
!HW   ----------------------ENDE BREITKRONIGES WEHR---------------------
                                                                        
                                                                        
            IF (tauu.ge.2.or.taoo.ge.2) goto 2240 
                                                                        
!                          Grenze Ueberstroemen - Ueberfallen:          
                                                                        
            tauo = 3.286 - 1.905 * taoo 
                                                                        
            IF (tauo.ge.tauu.and.tauo.le.1.0) then 
!                              vollkommener Ueberfall                   
              GOTO 2230 
            ELSEIF (tauo.ge.2.) then 
                                                                        
      write (UNIT_OUT_LOG, '(''Widerspruch !!!!: '',              &
     &                     ''tauu < 2.0 <=> tauoo > 2.0'',              &
     &                     /,''Annahme Ueberstroemen  !'')')
                                                                        
              GOTO 2240 
            ENDIF 
                                                                        
                                                                        
!                              Unvollkommener Ueberfall                 
!                              ------------------------                 
            iueart = 1 
                                                                        
            !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
            CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
            q_w (j) = q_w (j) * cm 
                                                                        
            GOTO 2230 
                                                                        
                                                                        
!                             Ueberstroemen                             
!                             -------------                             
 2240       iueart = 2 
                                                                        
            q_w (j) = a_uw (j) * g2 * (huew (j) - h_uw (j) ) **0.5 
          ENDIF 
                                                                        
 2230     q_ueb = q_ueb + q_w (j) 
                                                                        
 2220   END DO 
!JK            ENDIF ZU (hr1.lt.hokwmin)                                
      ENDIF 
                                                                        
!       ................................................................
                                                                        
!HW    Ende der Abfluss-Iteration                                       
                                                                        
 2300 dif_q = 0.001 
      df = (q_uea - q_ueb) / (hea_q - heb_q) 
                                                                        
!HW            Schreiben in Kontrollfile                                
      WRITE (UNIT_OUT_LOG, '(/,''q_uea  ='',f8.4)') q_uea
      WRITE (UNIT_OUT_LOG, '(''q_ueb  ='',f8.4)') q_ueb
      WRITE (UNIT_OUT_LOG, '(''hea_q  ='',f8.4)') hea_q
      WRITE (UNIT_OUT_LOG, '(''heb_q  ='',f8.4)') heb_q
      WRITE (UNIT_OUT_LOG, '(''df     ='',f8.4)') df
      WRITE (UNIT_OUT_LOG, '(''q_wehr ='',f8.4)') q_wehr
      WRITE (UNIT_OUT_LOG, '(''q_ue   ='',f8.4)') q_ue

                                                                        
      IF (abs (df) .gt.0.0001) dif_q = (q_wehr - q_ue) / df 
                                                                        
      hra = he_q 
                                                                        
 2400 he_q = he_q + dif_q 
                                                                        
!HW            Schreiben in Kontrollfile                                

        WRITE (UNIT_OUT_LOG, '(/,''Ende eines Iterationsschrittes '')') 
        WRITE (UNIT_OUT_LOG, '(''Festlegen neuer Eingangswerte '')')
        WRITE (UNIT_OUT_LOG, '(''alt: he_q ='',f8.4)') he_q
      WRITE (UNIT_OUT_LOG, '(''dif_q     ='',f8.4)') dif_q
        WRITE (UNIT_OUT_LOG, '(''neu: he_q = he_q + dif_q ='',f8.4)') he_q

                                                                        
      IF (hen1.ge.hokwmin) then 
                                                                        
        IF ( (he_q - hen1) .lt.0.001) he_q = (hra + hen1) / 2. 
                                                                        
      ELSEIF ( (he_q - hokwmin) .lt.0.001) then 
                                                                        
        he_q = (hra + hokwmin) / 2. 
                                                                        
      ENDIF 
                                                                        
!HW            Abspeichern des guenstigsten Ergebnisses                 
                                                                        
      IF (abs (q_wehr - q_ue) .lt.absmax) then 
        absmax = abs (q_wehr - q_ue) 
        heopt = he_q 
        q_wopt = q_ue 
      ENDIF 
                                                                        
 2000 CONTINUE 
                                                                        
!JK        ENDE DO WHILE-SCHLEIFE (abs(dif_q).gt.0.0001)                
!      '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                                                                        
!          Kontrolle der Energiehoehe                                   
                                                                        
 3000 dif_e = abs (he_q - he) 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

        WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Energiehoehe'')') 
      WRITE (UNIT_OUT_LOG, '(''he_q  = '',f8.4)') he_q
      WRITE (UNIT_OUT_LOG, '(''he    = '',f8.4)') he
        WRITE (UNIT_OUT_LOG, '(''dif_e = he_q - he = '',f8.4,/)') dif_e

                                                                        
      IF (abs (dif_e) .lt.absmax1) then 
        absmax1 = abs (dif_e) 
        he_opt = he 
      ENDIF 
                                                                        
 1000 CONTINUE 
                                                                        
!      -----------------------------------------------------------------
!      -----------------------------------------------------------------
!      Ergebnisrechnung                                                 
!      -----------------------------------------------------------------
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        
 4001 CONTINUE


      WRITE (UNIT_OUT_LOG, '(/,''Ergebnis der Wehrberechnung:'')')

                                                                        
      CALL g_wehr (he_q, ifehl, auew, huew, wl, hwmin)
                                                                        
      q_ue = 0.0 
                                                                        
      DO 4010 j = 1, nwfd 
                                                                        
        IF (huew (j) .lt.0.0001) then 
                                                                        
          q_w (j) = 0.0 
          hgrw (j) = 0.0 
                                                                        
        ELSE 
                                                                        
          iueart = 0 
                                                                        
          IF (wart.eq.'sk') wh = hwmin (j) - hming 
                                                                        
          !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
          CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

          WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
          WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
          WRITE (UNIT_OUT_LOG, '(''g2   = '',f8.4)') g2
          WRITE (UNIT_OUT_LOG, '(''cq   = '',f8.4)') cq (j)
          WRITE (UNIT_OUT_LOG, '(''huew = '',f8.4)') huew (j)
          WRITE (UNIT_OUT_LOG, '(''hv   = '',f8.4)') hv

          q_b = 2. / 3. * g2 * cq (j) * (huew (j) + h_v) **1.5
                                                                        
          q_w (j) = q_b * wl (j) 
          hgrw (j) = (q_b**2. / g) ** (1. / 3.) 
                                                                        
!HW        endif zu huew(j) .lt. 0.0001                                 
        ENDIF 
                                                                        
        q_ue = q_ue+q_w (j) 
                                                                        
 4010 END DO 
                                                                        
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

      WRITE (UNIT_OUT_LOG, '(/,''q_ue   ='',f8.4)') q_ue 
      WRITE (UNIT_OUT_LOG, '(''i      ='',10i8)')  (i, i = 1, nwfd)
      WRITE (UNIT_OUT_LOG, '(''q_w(i) ='',10f8.4)') (q_w (i) , i = 1, nwfd)
      WRITE (UNIT_OUT_LOG, '(''cq(i)  ='',10f8.4)')  (cq (i) , i = 1, nwfd)
      WRITE (UNIT_OUT_LOG, '(''hgrw(i)='',10f8.4)') (hgrw (i) , i = 1, nwfd)

                                                                        
      IF (hr1.le.hokwmin) then 
                                                                        
        GOTO 4000 
                                                                        
      ELSE 
                                                                        
        q_ue = 0.0 
                                                                        
        DO 4020 j = 1, nwfd 
                                                                        
          IF (h_uw (j) .gt.0.0005) then 
                                                                        
            tauu = h_uw (j) / hgrw (j) 
                                                                        
            taoo = huew (j) / hgrw (j) 
                                                                        
!JK                 SCHREIBEN IN KONTROLLFILE                           

      WRITE (UNIT_OUT_LOG, '(/,''j='',i3,'' tauu='',f8.4,                        &
     &          '' taoo='',f8.4)') j, tauu, taoo

                                                                        
!HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONOIGE WEHR MITTELS DE
!HW    IMPULSBILANZ GEMÄSS BWK/MERKBLATT 1, Seite 55, Abb. 3.34         
                                                                        
            IF (wart.eq.'bk') then 
                                                                        
!HW    SCHREIBEN IN KONTROLLFILE                                        

              WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
              WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')

                                                                        
              !MD   FEHLER IN DER FORMEL --> KORRIGIERT
              !taugrenz = SQRT ( (1 + hwmin (j) / hgrw (j) ) **2 + 2 + 1  / g * (v_uw**2 / hgrw (j) ) **2) &
              !             &  - v_uw**2 / (g * hgrw (j)) - hwmin (j) / hgrw (j)

              taugrenz = SQRT ( (1 + (hwmin (j) / hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) * g)) **2)    &
                          &  - (v_uw**2 / (g * hgrw (j))) - (hwmin (j) / hgrw (j))


!HW    SCHREIBEN IN KONTROLLFILE                                        

      WRITE (UNIT_OUT_LOG, '(/,''Grenzwert fuer Ueberstroemen nach Imp.bilanz'')')
                WRITE (UNIT_OUT_LOG, '(''taugrenz = '',f8.4)') taugrenz 

                                                                        
!HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz     
                                                                        
              IF (tauu.gt.taugrenz) then 
                                                                        
!                              Ueberstroemen                            
                iueart = 2 
!JK                            ZU UEBERSTROEMEN                         
                GOTO 4040 
              ELSE 
!JK                            ZU VOLLKOMMENER UEBERFALL                
                GOTO 4030 
              ENDIF 
                                                                        
            ENDIF 
                                                                        
!HW   ----------------------ENDE BREITKRONIGES WEHR---------------------
                                                                        
            IF (tauu.ge.2.or.taoo.ge.2) goto 4040 
                                                                        
!                   Grenze Ueberstroemen - Ueberfallen:                 
                                                                        
            tauo = 3.286 - 1.905 * taoo 
                                                                        
!JK                 SCHREIBEN IN KONTROLLFILE                           
            write (UNIT_OUT_LOG, '(''tauo='',f8.4)') tauo

            IF (tauo.ge.tauu.and.tauo.le.1.0) then 
!                        vollkommener Ueberfall                         
              GOTO 4030 
            ELSEIF (tauo.ge.2.) then 
                                                                        
!JK                      SCHREIBEN IN KONTROLLFILE                      
      write (UNIT_OUT_LOG, '(''Widerspruch !!!!: '',              &
     &                       ''tauu < 2.0 <=> tauoo > 2.0'',            &
     &                        /,''Annahme Ueberstroemen  !'')')
                                                                        
              GOTO 4040 
            ENDIF 
                                                                        
                                                                        
!                   Unvollkommener Ueberfall                            
!                   ------------------------                            
            iueart = 1 
                                                                        
            !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
            CALL beiwert (huew (j), j, wh, h_v, h_uw (j), iueart, cq (j), cm)
                                                                        
            q_w (j) = q_w (j) * cm 
                                                                        
!JK                 SCHREIBEN IN KONTROLLFILE                           

              WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4,'' cm='',f8.4)') q_w (j), cm

                                                                        
            GOTO 4030 
                                                                        
                                                                        
!                   Ueberstroemen                                       
!                   -------------                                       
 4040       iueart = 2 
                                                                        
            q_w (j) = a_uw (j) * g2 * (huew (j) - h_uw (j) ) **0.5 
                                                                        
!JK                 SCHREIBEN IN KONTROLLFILE                           

      WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4, '' Ueberstroemen'')')  (q_w (j) )

                                                                        
!JK            ENDIF ZU (h_uw(j).gt.0.0005)                             
          ENDIF 
                                                                        
 4030     q_ue = q_ue+q_w (j) 
                                                                        
!HW        Abspeichern der Ueberfallart der einzelnen Wehrfelder        
          iueartfd (j) = iueart 
                                                                        
 4020   END DO 
                                                                        
!JK        SCHREIBEN IN KONTROLLFILE                                    

          WRITE (UNIT_OUT_LOG, '(/,''q_ue-m='',f8.4)') q_ue 

                                                                        
!JK    ENDIF ZU (hr1.le.hokwmin)                                        
      ENDIF 
                                                                        
!HW    -----------------------------------------------------------------
!HW                    E R G E B N I S A U S G A B E                    
!HW    -----------------------------------------------------------------
                                                                        
!JK    SCHREIBEN IN KONTROLLFILE                                        
 4000 CONTINUE


      WRITE (UNIT_OUT_LOG, '(/,''Ergebnis der Wehrberechnung'',/,                &
     & ''he='',f8.4,'' q_wehr='',f8.4)') he_q, q_ue

                                                                        
!HW    Ausgabe des Ueberfallbeiwertes und der Ueberfallart in Ergebnisfi
      WRITE (UNIT_OUT_TAB, '(/,5x,''Ergebnisse der Wehrberechnung'')') 
      WRITE (UNIT_OUT_TAB, '(5x,''-----------------------------'')')
      WRITE (UNIT_OUT_TAB, '(5x,''Feld-Nr.'',4x,''mue'',10x,                     &
     &''q-wehr'',5x,''h-ue'',8x,''A-ue'',6x,''Ü-Art'')')
      DO 5000 j = 1, nwfd 
                                                                        
!HW    Abfragen der Ueberfallart fuer das Wehrfeld                      
        IF (iueartfd (j) .eq.0) then 
          uearttxt = 'vollkommen' 
        ELSEIF (iueartfd (j) .eq.1) then 
          uearttxt = 'unvollkommen' 
        ELSE 
          uearttxt = 'Überströmen' 
        ENDIF 
                                                                        
      WRITE (UNIT_OUT_TAB, '(5x,i2,10x,f6.4,5x,f7.3,5x,f6.3,5x,f7.3,5x,a)        &
     &') j, cq (j) , q_w (j) , huew (j) , auew (j) , uearttxt
                                                                        
 5000 END DO 
                                                                        
!HW    Ausgabe der Ergebnisse in Ergebnisfile                           
      WRITE (UNIT_OUT_TAB, '(/,5x,''Summation aller Felder'')') 
      WRITE (UNIT_OUT_TAB, '(5x,''he-wehr'',5x,''q-wehr'',/,1x,f10.3,2x,f10.3)') &
      he_q, q_ue
                                                                        
      nz = nz + 6 
                                                                        
 4200 FORMAT  (t8,i4,3f10.3) 
                                                                        
      nz = nz + nwfd 
                                                                        
      IF (nz.gt.50) then 
        nblatt = nblatt + 1 
        CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
      ENDIF
                                                                        
      RETURN 
                                                                        
!HW    Ende der Subroutine W_BER !                                      
                                                                        
      END SUBROUTINE w_ber                          
