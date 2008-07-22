!     Last change:  MD    9 Jul 2008   11:14 am
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

SUBROUTINE w_ber (he, qw, np, nz, idr1, nblatt, Q_Abfrage)

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
!**   wspow     Berechnet den Wasserspiegel im Oberwasser aus der Energiehoehe
!**   g_wehr    Berechnet die Wehrgeometrie und die Ueberfallhoehe am Wehr!!!
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
!**   dx                  -- Delta fuer die Veraenderung der angesetzten Energiehoehe im Oberwasser
!**                          aus den Energieghohenpaar he_q und hea_q wird je ein Wehrabfluss berechnet
!**                          Aus diesen zwei Punkten wird die Steigung der Geraden berechnet und ueber Q-Wehr
!**                          die im OW erfoderliche Energiehoehe berechnet.
!**   f(maxkla)                                                         
!**   fgesp(maxger)       -- insgesamt durchflossene Flaeche            
!**   fp(maxger,maxkla)                                                 
!**   froudp(maxger)      -- Froud-Zahl
!**   g2                  -- Hilfsvaraibale = sqrt (g *2)
!**   h_uw(maxw)          -- Wasserspiegelhoehe im Unterwasser          
!**   hbors(maxger)       -- Borda'sche Verlusthoehe                    
!**   hein(maxger)        -- Einlaufverlusthöhe                         
!**   hen(maxger)         -- Energiehoehe
!**   hen1                -- Energiehoehe im Unterwasser (Input erste Iteration)
!**   he                  -- Energiehoehe im Oberwasser während der Iteration
!**   he_opt              -- Energiehoehe im Oberwasser nach Abschluss der Iteration (ergebnis!)
!**   he_q                -- Energiehoehe im Oberwasser während der Wehrberechnung
!**   hgrw(maxw)          -- Grenztiefe
!**   hming               -- inimalew GOK im Wehrprofil
!**   hrow                -- Wassertiefe im Oberwasser aus wspow und wspanf
!**   h_ow                -- angenommene Wassertiefe im Oberwasser während der Wehrberechnung
!**   hokw(maxkla)        -- Hoehe der Wehroberkante im Profil          
!**   hort(maxger)        -- Auslaufverlusthoehe                        
!**   hrs(maxger)         -- Reibungsverlusthoehe
!     hra                 -- alte Energiehoehe im Oberwasser nach Abschluss der Iteration
!**   h_uw 	              -- Wasserspiegel im Unterwasser
!**   hs(maxger)          -- Gesamtverlusthoehe                         
!**   htrw(maxw)          -- z-Koordinate der Trennlinie Wehr (wird nie 
!**   huew(maxw)          -- Ueberfallhöhe am Wehr (Wsp.-Höhe im Wehrscheitel)
!**   hvs(maxger)         -- Geschwindigkeitsverlusthoehe               
!**   hwmin(maxw)         -- minimale Wehrhoehe                         
!**   hwmin_uw(maxw)      -- minimale Wasserspiegelhoehe im Unterwasser 
!**   ianfw(maxw)         -- Punktnummer des Wehranfangs im Profil      
!**   idr1                                                              
!**   iendw(maxw)         -- Punktnummer des Wehrendes im Profil        
!**   igrenz(maxger)                                                    
!**   iwehr               -- Profilparamter (Wehr vorhanden/nicht vorhan
!**   iwmin
!**   iueart              -- Art des Überfalls
!                            iueart = 0 :: vollkommener Ueberfall
!                            iueart = 1 :: unvollkommener Ueberfall
!                            iueart = 2 :: Ueberstroemen
!**   wb(maxw)            -- Breite des Wehrs quer zur Fliessrichtung
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
!**   wb_uw(maxw)         -- Wehrbreite in Unterwasser
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

! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
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
REAL 		:: beiw (maxw), rkw (maxw), wl (maxw), wb (maxw)
CHARACTER(LEN=2):: wart                         ! = 'bk', 'rk', 'bw', 'sk'
COMMON / wehr2 / beiw, rkw, wl, wart, wb
! -----------------------------------------------------------------------------

REAL :: v_uew(maxw), huew_neu(maxw)       ! Fließgeschwindigkeit und Wasserstand im Wehr
REAL :: h_ow                              ! angenommene Wassertiefe im Oberwasser während der Wehrberechnung
REAL :: hgrw (maxw), q_w (maxw)           ! Grenztiefe am Wehr und Wehrabfluss
REAL :: v_uw, h_uw, h_uw_w                ! Fließgeschwindigkeit und Wasserstand im Unterwasser
REAL :: f_uw                              ! gesamte Durchströmte Fläche im UW qm
REAL :: lu_ow                             ! benetzter Umfang im Oberwasser
REAL :: f_ow                              ! durchstroemte flaeche im Fluss Oberwasser
! ------------------------------------------------------------------
! Uebergabegroessen der Geometrieberechnung:
! ------------------------------------------------------------------
REAL :: auew (maxw), huew (maxw), hwmin (maxw)
REAL :: wb_uw (maxw), hwmin_uw (maxw), q_ueber(maxw)
REAL :: cq (maxw)            ! Ueberfallbeiwert
REAL :: tauu_grenz           ! Grenzwert zwischen vollkommen u unvollkommem Ueberfall
REAL :: TermA, TermB, TermC  ! Hilfsterme fuer die Impulsbilanz
REAL :: delta                ! Differenz zwischen UW-sohlpunkt und Wehr-Gok-Sohlpunkt
INTEGER :: ow                ! zaehler für den wiederholten aufruf von wspow
REAL :: h_v_opt              ! Optimale Geschwindigkeitshoehe zur optimalen Energiehoehe
REAL :: q_wehr_opt
REAL :: q_wopt
REAL :: v_uw_konti           ! Fliessgeschwindigkeit im UW angepasst fuer die Kontinuitaet am Wehr
                             ! erforderlich für die Impulsbilanz (ueberstroemen)
REAL :: K_verlust            ! Verlustbeiwert für Anstroemung des Wehres bei Ueberströmen

!HW Ueberfallart der einzelnen Felder vektoriell zusammengefasst
CHARACTER(LEN=1) :: idr1
INTEGER :: iueart (maxw)        ! Art des Uberfalls: vollk., unvollk. oder ueberstroemen
CHARACTER(LEN=12) :: uearttxt   ! Bezeichnung des Uberfalls: vollk., unvollk. oder ueberstroemen
CHARACTER(LEN=6)  :: ABFRAGE    ! Kennung zur Abfluss-iteration Fall 0 oder Fall A
CHARACTER(LEN=11) :: Q_Abfrage  ! Abfrage fuer Ende der Inneren Q-Schleife aus wspber

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
          WRITE (UNIT_OUT_TAB, '(5x,i2,6x,3f10.2)') j, wl (j)
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

dx = 0.01
itmax2000 = 100
g2 = sqrt (g * 2.)
!MD  h_uw = 0.0    !MD  nochmal Unterwasser
wh = 0.0           !MD  Wehrhoehe in [m] = minimale Wehrhoehe - minimalew GOK im Wehrprofil
he_opt = 0.0
!MD  hr1 = wsp (np - 1)
!MD  v_uw = SQRT (2. * g * (hen1 - hr1) )
h_uw = wsp (np - 1)                     !MD  Wasserspiegel im Unterwasser
hen1 = hen (np - 1)                     !MD  Energiehoehe im Unterwasser
v_uw = SQRT (2. * g * (hen1 - h_uw))    !MD  Fließgeschwindigkeit im Unterwasser
f_uw = fgesp (np - 1)                   !MD  gesamte Durchströmte Fläche im UW

IF (BERECHNUNGSMODUS=='REIB_KONST' .and. Q_Abfrage=='WE_SCHLEIFE') then
  h_uw = wsp (np - 1)                    !MD  Wasserspiegel im Unterwasser
  v_uw = qw / f_uw                       !MD  Fließgeschwindigkeit im Unterwasser
  hen1 = h_uw + ((v_uw**2.)/(2. * g))    !MD  Energiehoehe im Unterwasser

  !MD Kontrolle der Grenztiefe im Unterwasser
  ! -----------------------------------------------------
  IF ((h_uw - sohlp(np-1)) .lt. (2./3.* (hen1 - sohlp(np-1))) ) THEN
    write (UNIT_OUT_LOG,' ('' KEINE Wehrberechnung an Station km = '',f7.3)') stat(np)
    WRITE (UNIT_OUT_LOG, '(''   fuer Innerer Abfluss qw  ='',f8.4)') qw
    WRITE (UNIT_OUT_LOG, '(''   da Grenztiefe im UW erreicht!! '')')
    Q_Abfrage = 'GR_SCHLEIFE'
    RETURN
  ELSEIF (v_uw .lt. ( SQRT(0.0002 *2.*g)) ) THEN
    write (UNIT_OUT_LOG,' ('' KEINE Wehrberechnung an Station km = '',f7.3)') stat(np)
    WRITE (UNIT_OUT_LOG, '(''   fuer Innerer Abfluss qw  ='',f8.4)') qw
    WRITE (UNIT_OUT_LOG, '(''   da Fliessgeschwindigkeit im UW kleiner 0,063!! '')')
    Q_Abfrage = 'GR_SCHLEIFE'
    RETURN
  END IF
Endif

!HW    SCHREIBEN IN KONTROLLFILE                                        
WRITE (UNIT_OUT_LOG, '(''Wasserspiegel am Profil n-1: h_uw = '',f8.4)') h_uw
WRITE (UNIT_OUT_LOG, '(''Energiehoehe  am Profil n-1: hen1 = '',f8.4)') hen1
WRITE (UNIT_OUT_LOG, '(''Fliessgesch.  am Profil n-1: v_uw = '',f8.4)') v_uw
WRITE (UNIT_OUT_LOG, '(''Durch.Fläche  am Profil n-1: f_uw = '',f8.4)') f_uw

psieins = 0.0
psiorts = 0.0

!MD  Zulaessige Differenz zwischen OW-Zufluss ausm Fluss und Wehrabfluss
eps_q = 0.0001

!HW   Beginn der if-Abfrage h_uw > hokwmin
WRITE (UNIT_OUT_LOG, '(''Minimale Hoehe Wehr-OK: hokwmin = '',f8.4)') hokwmin

IF (h_uw.gt.hokwmin) then  ! Wasserspiegel im Unterwasser > minimale Wehrhöhe

  !JK        SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(/,''Unterwasser'')')
  WRITE (UNIT_OUT_LOG, '(''h_uw='',f8.4)') h_uw

  !HW  Aufruf der Subroutine g_wehr
  !MD  CALL g_wehr (hr1, ifehl, a_uw, h_uw, wl_uw, hwmin_uw)
  CALL g_wehr (h_uw, ifehl1, auew, huew, wb, hwmin)
ENDIF



                                                                        
!-----------------------------------------------------------------------------
!      Iterieren he - Energiehoehe im Oberwasser
!-----------------------------------------------------------------------------
!HW    SCHREIBEN IN KONTROLLFILE                                        
WRITE (UNIT_OUT_LOG, '(/,''Beginn der Energiehoehen-Iteration he'')')

it1000 = 0
it1000max = 50
dif_e = 10.
absmax1 = 1.e+06

!HW  Beginn der do-Schleife dif_e > 0.001 (Energiehoehen-Iteration)
DO 1000 WHILE(dif_e.gt.0.0001)
  absmax = 0.0005       !MD Startwert fuer die Differenz zwischen OW-Zufluss ausm Fluss und Wehrabfluss
  it1000 = it1000 + 1

  IF (it1000.gt.it1000max) then
    !JK   SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '('' Konvergenz in der Schleife 1000 nicht gefunden !'')')
    WRITE (UNIT_OUT_LOG, '('' Keine Konvergenz in Wehrberechnung bei Energiehoehen-Iteration!'')')
    WRITE (UNIT_OUT_LOG, '('' Energiedifferenz zwischen UW und OW minimal:'')')
    WRITE (UNIT_OUT_LOG, '('' --> Energiehoehe UW = Energiehoehe OW!'')')

    WRITE (*, '('' Keine Konvergenz am Wehr bei Energiehoehen-Iteration!'')')
    WRITE (*, '('' Energiedifferenz zwischen UW und OW minimal:'')')
    WRITE (*, '('' --> Energiehoehe UW = Energiehoehe OW!'')')

    !JK   WAR SCHON DEAKTIVIERT, 02.05.00, JK
    !**   print*,'keine Konvergenz in der Wehrberechnung!'
    !**   print*,'Schleife1000 -> STOP '
    !**   stop 'programende'
  ENDIF

  !MD  fuer die erste Iteration
  IF (it1000.eq.1) then
    !MD  Energiehoehe UW - Wehrhöhe > 0.1 m
    delta = sohlp (np) - sohlp (np - 1)
    IF ( (hen1 - hokwmin) .gt.0.1) then
      !MD  NEU:: he = hen1 + Sohlanstieg
      if (delta .gt. 0.1) then
         he = hen1 + delta
      end if
      he = hen1 + 0.1   !MD  he ist zu Beginn = Energiehoehe im UW + 0.1
    ELSE  !MD  Energiehoehe UW - Wehrhöhe kleiner gleich 0.1 m
      he = hokwmin + 0.1
    ENDIF
  !MD  fuer die zweite bis letzte Iteration
  ELSEIF (it1000 .LE. 30) then
    !MD  Energiehoehe = Mittelwert(he alt & he_q neu)
    !MD  he = (he+he_q) / 2.
    he = he_q

    !MD  Achtung diese Annahme fuer he_q = hen1
    !MD  muss sich von der Annahme hea_q unterscheiden, sonst ENDLOS-SCHLEIFE
    IF (he.lt.hen1) he = hen1
  ELSEIF (it1000.gt.30 .and. it1000.le.it1000max) then
    !MD  Energiehoehe = Mittelwert(he alt & he_q neu)
    he = (he +hea_q+he_q)/ 3.
    !MD neu he = (he+he_q) / 2.
    IF (he.lt.hen1) he = hen1

  ELSE !MD  nach der letzte Iteration
    if (he_opt == 0.0) then
      he = he_q
      he_opt = 0.0  !MD neu
      GOTO 4001
    else
      he = he_opt  !MD  Ubergabe der fertigen Energiehoehe
      GOTO 4001    !MD  weiter zur Ergebnisberechnung
    endif
  ENDIF

  ow = 0
  1010 CONTINUE

  !HW    SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(''Neue Energiehoehe he = '',f8.4)') he

  !MD   Start der Oberwasseriteration: auf Basis einer ggbn Energiehoehe
  !HW   Aufruf der Subroutine wspow ------------------------------------------------
  CALL wspow (he, strbr, qw, q1, hrow, hv, rg, indmax, hvst, hrst,  &
              psieins, psiorts, nprof, hgrenz, ikenn, nblatt, nz, idr1, Q_Abfrage)
  !MD   Sammeln der neuen Groeßen aus dem OW: Abfluss q, v, he und h_ow (=WSP)

  !HW    SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(/,''Werte aus wspow'')')
  WRITE (UNIT_OUT_LOG, '(''he = '',f8.4)') he
  WRITE (UNIT_OUT_LOG, '(''qw = '',f8.4)') qw
  WRITE (UNIT_OUT_LOG, '(''q1 = '',f8.4)') q1

  If (hrow.eq.he .and. ow.le.2) then
   he = hv + hrow
   ! he = hrow + (0.1 * delta)
   ow = ow + 1
   WRITE (UNIT_OUT_LOG, '(/,''Neue Iteration in wspow'')')
   WRITE (UNIT_OUT_LOG, '(''ow = '',I1)') ow
   GOTO 1010
  End if


  !JK  BERECHNUNG NACH DARCY-WEISBACH
  if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
    q_wehr = qt (2)     !MD  uebers Wehr geht nur Q aus dem Fluss
    v_ow = v (2)        !MD  am Wehr wirkt nur v aus dem Fluss
    h_ow = hrow         !NEU MD Wasserstand im Oberwasser
    lu_ow = u (2)       !NEU MD benetzter Umfang im Oberwasser
    f_ow = f(2)         !NEU MD durchstroemte flaeche im Fluss Oberwasser
  !JK  BERECHNUNG NACH GMS
  ELSE
    q_wehr = qt (indfl)  !MD  uebers Wehr geht nur Q aus dem Fluss
    v_ow = v (indfl)     !MD  am Wehr wirkt nur v aus dem Fluss
    h_ow = hrow          !NEU MD Wasserstand im Oberwasser
    lu_ow = u (indfl)    !NEU MD benetzter Umfang im Oberwasser
    f_ow = f(indfl)         !NEU MD durchstroemte flaeche im Fluss Oberwasser
  ENDIF


  !HW    SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach DW/GMS'')')
    WRITE (UNIT_OUT_LOG, '(''q_wehr = '',f8.4)') q_wehr
    WRITE (UNIT_OUT_LOG, '(''v_ow   = '',f8.4)') v_ow
    WRITE (UNIT_OUT_LOG, '(''lu_ow  = '',f8.4)') lu_ow
    WRITE (UNIT_OUT_LOG, '(''f_ow   = '',f8.4)') f_ow
  ENDIF

  ! h_v = (v_ow**2.) / (2. * g)
  h_v = hv                      !MD  Geschwindigkeits-Energiehoehe im OW
  dif_ok = he - h_v             !MD  = h_ow = Energiehoehe OW ohne Fliess-v = WSP-OW
  dif_hv = h_v / 10.

  If (hrow.eq.he) then      !MD  Korrektur der falschen Energiehoehe he im OW infolge
    he = h_v + h_ow         !    hrow =!= henow aus der Berechnung in wspow und ff
    dif_ok = h_ow
    dif_hv = h_v / 10.
  Endif

  he_q = he                 !MD  Neue Belegung der Energiehoehe am Wehr

  !HW    SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '(''h_v    = '',f8.4)') h_v
    WRITE (UNIT_OUT_LOG, '(''dif_ok = '',f8.4)') dif_ok
    WRITE (UNIT_OUT_LOG, '(''dif_hv = '',f8.4)') dif_hv
  ENDIF

  IF ( (dif_ok - hokwmin) .le. dif_hv) then   !MD  Ueberfallhoehe kleiner gleich h_v / 10
    he = he + 1.15 * h_v        !MD   Dann Energiehoehe um ca. 15% Erhoehen
    GOTO 1010                   !MD    --> Zurueck:: Neue Iteration fuer das Oberwasser
  ENDIF

  !JK   SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '(/,''it1000='',i4,'' he='',f8.4,   &
      &  '' q_wehr='',f8.4,'' h_v='',f8.4)') it1000, he, q_wehr, h_v
    WRITE (UNIT_OUT_LOG, '(/,''Ende der Energiehoehen-Iteration'')')
  ENDIF

  WRITE (UNIT_OUT_LOG, '(/,''Beginn Abfluss-Iteration - Wehrüberfall'')')

  !-----------------------------------------------------------------------------
  !      Iterieren he - Energiehoehe im Oberwasser
  !-----------------------------------------------------------------------------




  !------------------------------------------------------------------
  !      Iterieren q_wehr - Wehrueberfall:
  !------------------------------------------------------------------
  !WP 01.02.2005 Wird bereits in Modul DIM_VARIABLES definiert (=99),
  !WP itmax = 50
  dif_q = 10.
  it2000 = 0

  !HW   Beginn der Abfluss-Iteration
  DO 2000 WHILE(abs (dif_q) .gt.0.0001)

    !HW    SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(''dif_q = '',f8.4)') dif_q
    endif

    it2000 = it2000 + 1

    IF (BERECHNUNGSMODUS=='REIB_KONST' .and. Q_Abfrage=='WE_SCHLEIFE') then
      IF (it2000.GT.itmax) THEN
        write (UNIT_OUT_LOG,' ('' WARNUNG: !!!!!!!!!!!!!!!!!!!!!'')')
        WRITE (UNIT_OUT_LOG, '('' -------------------------------------'')')
        write (UNIT_OUT_LOG,' ('' KEINE Konvergenz in Wehrberechnung: an Station km = '',f7.3)') stat(np)
        WRITE (UNIT_OUT_LOG, '(''   fuer Innerer Abfluss qw  ='',f8.4)') qw
        WRITE (UNIT_OUT_LOG, '(''   Abbruch in Schleife 2000 bei Abfluss-Iteration!! '')')
        WRITE (UNIT_OUT_LOG, '('' --------------------------------------'')')
        WRITE (UNIT_OUT_LOG, '('' Weiter mit naechstem Inneren Abfluss qw !! '')')

        write (*,' ('' WARNUNG: !!!!!!!!!!!!!!!!!!!!!'')')
        WRITE (*, '('' -------------------------------------'')')
        write (*,' ('' KEINE Konvergenz am Wehr = '',f7.3)') stat(np)
        WRITE (*, '(''   fuer Innerer Abfluss qw ='',f8.4)') qw
        WRITE (*, '(''   Abbruch bei Abfluss-Iteration!! '')')
        WRITE (*, '('' -------------------------------------'')')
        WRITE (*, '('' Weiter mit naechstem Inneren Abfluss qw+1 !! '')')

        Q_Abfrage = 'GR_SCHLEIFE'
        RETURN
      Endif
    END IF

    IF (it2000.gt.itmax) then   ! itmax = 99 in DIM_VARIABLES
      WRITE (*, '('' Keine Konvergenz in der Wehrberechnung: '')')
      WRITE (*, '(''   Abbruch in Schleife 2000 bei Abfluss-Iteration!'')')
      STOP 'programende'
    ENDIF

    !MD fuer die erste Iteration: Energiehoehe im OW  he_q = he = Energiehoehe im UW
    IF (it2000.eq.1) he_q = he

    !JK  SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(/,''---------------------------------'')')
      WRITE (UNIT_OUT_LOG, '(/,'' it2000='',i4,'' he_q='',f8.4, '' nwfd='',i3)') it2000, he_q, nwfd
    ENDIF

    h_ow = he_q - h_v
    !HW   Aufruf der Subroutine g_wehr
    !MD   CALL g_wehr (he_q, ifehl, auew, huew, wl, hwmin)
    !NEU MD  Eingang mit Wasserspiegelhoehe statt Energiehoehe
    !NEU MD    --> sonst wird Wasserstand im Wehr huew falsch berechnet
    CALL g_wehr (h_ow, ifehl1, auew, huew, wb, hwmin)


    q_ue = 0.0             ! berechneter Gesamt-Abfluss am Wehr = 0
    DO 2010 j = 1, nwfd    ! Schleife ueber  alle Wehrfelder

      !HW    SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,''Berechnung Ueberfallmenge fuer alle Wehrfelder'')')
      WRITE (UNIT_OUT_LOG, '(''Wsp. im Wehrscheitel: huew = '',f8.4)') huew (j)

      IF (huew (j) .lt.0.0001) then  ! fuer Ueberfallhoehe kleiner 0.0001
        q_w (j) = 0.0
        hgrw (j) = 0.0

      ! ------------------------------------------------------------------------
      ! fuer Ueberfallhoehe groeßer gleich 0.0001 --> vollkommener Ueberfall
      ! ------------------------------------------------------------------------
      ELSE
        WRITE (UNIT_OUT_LOG, '(/,''Vollkommener Ueberfall'')')
        !MD  Ueberfallart = 0 :: vollkommener Ueberfall
        iueart(j) = 0

        IF (wart.eq.'sk') then       ! wenn scharfkantiges Wehr
          wh = hwmin (j) - hming     ! minimale Wehrhoehe - minimalew GOK im Wehrprofil
          WRITE (UNIT_OUT_LOG, '(/,''scharfkantiges Wehr'')')
        ENDIF

        h_uw_w = h_uw - hokwmin
        !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
        CALL beiwert (huew (j), j, wh, h_v, h_uw_w, iueart(j), cq (j), cm)

        !HW    SCHREIBEN IN KONTROLLFILE
        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
          WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
          WRITE (UNIT_OUT_LOG, '(''--------------------------'')')
          WRITE (UNIT_OUT_LOG, '(''g2   = '',f8.4)') g2
          WRITE (UNIT_OUT_LOG, '(''cq   = '',f8.4)') cq (j)
          WRITE (UNIT_OUT_LOG, '(''huew = '',f8.4)') huew (j)
          WRITE (UNIT_OUT_LOG, '(''hv   = '',f8.4)') hv
          WRITE (UNIT_OUT_LOG, '(/,''Breite des Wehres wb:'')')
          WRITE (UNIT_OUT_LOG, '(''wb = '',f8.4)') wb (j)
        ENDIF

        !HW  Berechnung des spezifischen Wehrabflusses q nach DU BUAT
        q_b = (2./3.) * g2 * cq (j) * ((huew (j) + h_v) **1.5)
        !MD   vollstaendiger Wehrabfluss Q = b * q_b
        q_w (j) = q_b * wb (j)
        hgrw (j) = ((q_b**2.) / g) ** (1. / 3.)   ! Grenztiefe
      ENDIF   ! Ende zum vollkommenen Ueberfall

      q_ue = q_ue+q_w (j)   ! Wehr-Abflussaddition uber alle Felder

      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''Summation des Wehrabflusses bis hier:q_ue='',f8.4)') q_ue
      ENDIF
    2010 END DO  ! ------------------------------------------------------------------------
    

    !HW    SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(/,''Endergebnisse fuer alle Felder:'')')
      WRITE (UNIT_OUT_LOG, '(''q_ue   = '',f8.4)') q_ue
      WRITE (UNIT_OUT_LOG, '(''i      = '',10i8)')  (i, i = 1, nwfd)
      WRITE (UNIT_OUT_LOG, '(''q_w(i) = '',10f8.4)') (q_w (i) , i = 1, nwfd)
      WRITE (UNIT_OUT_LOG, '(''cq(i)  = '',10f8.4)')  (cq (i) , i = 1, nwfd)
      WRITE (UNIT_OUT_LOG, '(''hgrw(i)= '',10f8.4)') (hgrw (i) , i = 1, nwfd)
    ENDIF

    !MD IF (h_uw.le.hokwmin) then   ! Unterwasserstand =< minimale Wehrhoehe
    !MD   IF (abs (q_ue-q_wehr) .le.eps_q) then  ! Abfluss uebers Wehr - Zufluss aus OW =< 0.0001
    !       GOTO 2400
    !MD   ... siehe unten
    !MD   ENDIF
    !MD ELSE  !MD   Unterwasserstand > minimale Wehrhoehe

    ! Wehr-Gesamt-Abfluss = 0
    q_ue = 0.0
    ABFRAGE = 'FALL 0'
    ! ----------------------------------------------------------------------------------------------
    ! ----------------------------------------------------------------------------------------------
    DO 2020 j = 1, nwfd   ! Ueber alle Wehrfelder
      IF (h_uw.gt.0.0005) then

        !HW    Berechnung der dimensionslosen Hoehen
        WRITE (UNIT_OUT_LOG, '(/,''Berechnung der dimensionlosen Hoehen'')')

        tauu = (h_uw  - hokwmin) / hgrw (j)  !MD   Wasserstand DES Unterwassers - Wehrhoehe
        taoo = (huew (j) + h_v) / hgrw (j)   !MD   ENERGIEHÖHE DES OBERWASSERS an Wehrkrone
        tauu_grenz = 2.0 - (0.5 / 0.38679)
        tauo = 1.725 - (0.525 * tauu)        !MD  Grenzgerade f(tauu) vollk - unvollk
        ! tauo = 3.286 - (1.905 * taoo)

        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''j='',i3,'' tauu='',f8.4,'' taoo='',f8.4)') j, tauu, taoo
        ENDIF

        !------------------------------------------------------------------------------------------
        !HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONIGE WEHR MITTELS DER
        !HW    IMPULSBILANZ GEMAESS BWK/MERKBLATT 1, Seite 55, Abb. 3.34
        !------------------------------------------------------------------------------------------
        IF (wart.eq.'bk') then
          !HW    SCHREIBEN IN KONTROLLFILE
          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
            WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')
          ENDIF
          !MD   FEHLER IN DER FORMEL --> KORRIGIERT
          ! taugrenz = SQRT ( (1 + hwmin (j) / hgrw (j) ) **2 + 2 + 1  / g * (v_uw**2 / hgrw (j) ) **2) &
          !             &  - v_uw**2 / (g * hgrw (j)) - hwmin (j) / hgrw (j)
          taugrenz = SQRT ( (1 +(hokwmin/ hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) *g)) **2)    &
                        &  - (v_uw**2 / (g * hgrw (j))) - (hokwmin/ hgrw (j))

          if ((h_uw-sohlp(np-1)).le.hgrw(j)) then
            iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
            GOTO 2030       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
          elseif (h_uw.le.hokwmin) then
            iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
            GOTO 2030       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
          else
            iueart(j) = 2   !    Ueberstroemen
            GOTO 2040       !JK  Weiter ZU UEBERSTROEMEN
          end if
       !   v_uw_konti = q_wehr/f_uw
       !   taugrenz = SQRT ( (1 +(hokwmin/ hgrw (j)))**2 + 2 + (v_uw_konti**2 / (hgrw (j) *g)) **2)  &
       !                 &  - (v_uw_konti**2 / (g * hgrw (j))) - (hokwmin/ hgrw (j))

    !      !HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz
    !      IF (tauu .gt. taugrenz) then
    !        iueart(j) = 2   !    Ueberstroemen
    !        GOTO 2040       !JK  Weiter ZU UEBERSTROEMEN
    !      ELSE
    !        iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
    !        GOTO 2030       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
    !      ENDIF

        ENDIF
        !HW ---------------------------------  ENDE BREITKRONIGES WEHR   ---------------------

        ! ZU UEBERSTROEMEN
        !-----------------------------------------------------------
        IF (tauu.ge.2 .and. taoo.ge.2) then
          iueart(j) = 2   !   Ueberstroemen
          goto 2040
        endif

        ! Grenze zwischen vollkommen und unvollkommenen Ueberfallen:
        !-----------------------------------------------------------
        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(''tauo='',f8.4)') tauo
          WRITE (UNIT_OUT_LOG, '(''tauu_grenz='',f8.4)') tauu_grenz
        ENDIF

        !MD  IF (tauo.ge.tauu .and. tauo.le.1.0) then
        IF (tauu.le.0.0) then  ! Wenn UW < Wehr-ok
          iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
          GOTO 2030
        ! ELSEIF (tauu.lt.2.0 .and. tauo.lt.tauu_grenz .and. tauu.le.tauo) then
        ELSEIF (tauo.lt.tauu_grenz .and. tauu.lt.tauu_grenz) then
          iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
          GOTO 2030

        ! ELSEIF (tauu.lt.2.0 .and. tauu.gt.tauo .and. taoo.lt.tauu) then
        ELSEIF (tauu.lt.2.0 .and. taoo.lt.2) then
          iueart(j) = 1   !    UNVOLLKOMMENER UEBERFALL

          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(/,''Unvollkommener Ueberfall'')')
          ENDIF

          !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
          CALL beiwert (huew (j), j, wh, h_v, h_uw_w, iueart(j), cq (j), cm)
          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w (Wehrueberfall)'')')
          ENDIF
       !   q_b = (2. / 3.) * g2 * cq (j) * ((huew (j) + h_v - h_uw_w) **1.5)
       !   q_w (j) = q_b * wl (j) * cm
          q_w (j) = q_w(j) * cm
          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4,'' cm='',f8.4)') q_w (j), cm
          ENDIF

          GOTO 2030

        ELSEIF (tauu.ge.2.) then
          iueart(j) = 2   !    Ueberstroemen
          GOTO 2040    !JK  Weiter ZU UEBERSTROEMEN
        ELSE
          iueart(j) = 2   !    Ueberstroemen
          GOTO 2040    !JK  Weiter ZU UEBERSTROEMEN
        ENDIF


        ! --------------------------------------------------------------------------
        !  SCHLEIFE 2040::  FUERS Ueberstroemen
        ! -----------------------------------------------------------------------------
        2040   iueart(j) = 2

        !HW    SCHREIBEN IN KONTROLLFILE
        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''Ueberstroemen'')')
          WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w nach Knapp S.304'')')
        ENDIF
        !MD    q_w (j) = auew (j) * g2 * (huew (j) - h_uw (j) ) **0.5
        !MD    Formel nach Knapp S 304: verbessert!
        if (auew(j).ge. f_ow) then
          K_verlust = ((lu_ow ** 2.) * (wb(j) * (hokwmin - sohlp(np))) *1.5) / &
                   &   ((auew(j) + wb(j) * (hokwmin - sohlp(np))) **2.)
        elseif (auew(j).lt. f_ow) then
          K_verlust = ((lu_ow ** 2.) * (f_ow - auew(j)) *1.5) / (f_ow **2.)
        end if

        if ((h_uw - hokwmin).gt.0.0) then
          q_w (j) = huew(j) * wb(j) * g2* ((huew(j) + h_v - (h_uw - hokwmin)) **0.5)
        endif

        IF (K_verlust.gt.0.0 .and. (huew(j) +h_v).gt.(h_uw -hokwmin)) then
          q_ueber(j) = huew(j) * wb(j) * g2 * (((huew(j) + h_v - (h_uw - hokwmin)) /(1.+ K_verlust))  **0.5)
          q_w(j) =  q_ueber(j)
        ELSEIF (K_verlust.gt.0.0 .and. (huew(j) +h_v).le.(h_uw -hokwmin)) then
          q_ueber(j) = huew(j) * wb(j) * g2 * (((huew(j) + h_v) /(1.+ K_verlust))  **0.5)
          q_w(j) =  q_ueber(j)
        endif

 !     if (he_q.eq.hen1 .and. q_w(j).ge.q_wehr) then
 !        ! Korrektur der Kontigleichung uber v_uw --> v_uw_konti
 !        ! Impulsbilanz mit Wassertiefe nicht WSP sondern mit BEZUG auf Sohle UW
 !        v_uw_konti = q_wehr/f_uw
 !        TermA = f_uw * ((v_uw_konti**2.) + (g* (h_uw-sohlp(np-1)) /2.))
 !        TermB = (g/2.)*(hokwmin-sohlp(np-1)+huew(j)) * (auew(j) + ((auew(j)/huew(j)) *(hokwmin-sohlp(np-1))))
 !        TermC = auew(j)
 !
 !        if (TermA .gt. TermB) then
 !          v_uew(j) = SQRT ((TermA-TermB)/ TermC)
 !        else
 !          v_uew(j) = v_ow
 !        end if
 !        q_w (j) = huew(j) * wb(j) * v_uew(j)
 !
 !         IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
 !            WRITE (UNIT_OUT_LOG, '(''huew(j)='',f8.4, '' aus Impulsbilanz'')')  (huew(j))
 !            WRITE (UNIT_OUT_LOG, '(''v_uew(j)='',f8.4, '' aus Impulsbilanz'')')  (v_uew(j))
 !         ENDIF
 !      end if

        WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4, '' Ueberstroemen'')')  (q_w (j) )

        ! --------------------------------------------------------------------------
        !  SCHLEIFE 2030::  FUERS Ueberfall vollk und unvollk
        ! -----------------------------------------------------------------------------
        2030  IF (ABFRAGE == 'FALL 0') THEN
          q_ue = q_ue+q_w (j)
          WRITE (UNIT_OUT_LOG, '(''Summation q_ue bis hier: q_ue ='',f8.4)') q_ue
        ELSEIF (ABFRAGE == 'FALL A') THEN
          q_uea = q_uea + q_w (j)
        ELSEIF (ABFRAGE == 'FALL 1') THEN
          q_ue = q_ue + q_w (j)
        END IF
      ENDIF
    2020 END DO

    ! ----------------------------------------------------------------------------------------------
    ! ----------------------------------------------------------------------------------------------
    ! JK   SCHREIBEN IN KONTROLLFILE
    ! WRITE (UNIT_OUT_LOG, '(/,''q_ue-m ='',f8.4)') q_ue

    ! IF (h_uw.le.hokwmin) then   ! Unterwasserstand =< minimale Wehrhoehe
    IF (abs (q_ue-q_wehr) .le.eps_q) then  ! Abfluss uebers Wehr - Zufluss aus OW =< 0.0001
      dif_q = 0.0
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Abspeichern guenstiger Werte'')')
      ENDIF
      GOTO 2400               ! --> Weiter zur Auswertung
    ELSE                      ! Abfluss uebers Wehr - Zufluss aus OW > 0.0001
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Schaetzen neuer Energiehoehe'')')
      ENDIF
      !JK    Weiter mit SCHAETZEN NEUER ENERGIEHOEHE
      GOTO 2100               !  --> Weiter mit neuer Energiehoehe hea_q fuer Wehrabfluss
    ENDIF




    !---------------------------------------------------------------------------
    !  Schleife 2100:   Schaetzen neue Energiehoehe fuer Wehrabfluss
    !---------------------------------------------------------------------------
    2100 if (q_ue .le. q_wehr) then
      hea_q = (he_q + dx)      ! Energiehoehe "FALL A" = Energiehoehe "Fall 0" + 0.01
    ELSEIF (q_ue .gt. q_wehr) then
      hea_q = (he_q - dx)      ! Energiehoehe "FALL A" = Energiehoehe "Fall 0" - 0.01
    endif

    !HW  SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(/,''--------------------------------------------'')')
      WRITE (UNIT_OUT_LOG, '(''Schaetzen neuer Energiehoehen und'')')
      WRITE (UNIT_OUT_LOG, '(''neue Berechnung mit diesen Werten'')')
      WRITE (UNIT_OUT_LOG, '(''--------------------------------------------'')')
    ENDIF

    IF (hea_q .le. hen1) then      ! neue Energiehoehe =< Energiehoehe UW
      hea_q = hen1 + dx
    endif
    IF (hea_q .le. hokwmin) then     ! neue Energiehoehe =< minimale Wehrhoehe
      hea_q = (he_q + hokwmin) / 2.  ! Mittelwert aus Energiehohe und minimale Wehrhoehe
    endif



    !--------------------------------------------------------------------------
    !   Berechnung mit hea_q
    !--------------------------------------------------------------------------
    h_ow = hea_q - h_v
    !MD  neue geschaetzte Wassertiefe fuer den Fall A
    !MD  CALL g_wehr (hea_q, ifehl, auew, huew, wl, hwmin)
    CALL g_wehr (h_ow, ifehl, auew, huew, wb, hwmin)
    q_uea = 0.0

    !HW  Beginn der Abfluss-Iteration fuer den geschaetzten unteren Wert
    !MD  Diese Schleife entspricht der Schleife 2010 --------------------------
    DO 2110 j = 1, nwfd

      IF (huew (j) .lt.0.0001) then  ! wenn Uberfallhoehe < 0.0001
        q_w (j) = 0.0                ! keine Abfluss im wehr

      ! ------------------------------------------------------------------------
      ! fuer Ueberfallhoehe groeßer gleich 0.0001 --> vollkommener Ueberfall
      ! ------------------------------------------------------------------------
      ELSE
        iueart(j) = 0  !  VOLLKOMMENER UEBERFALL

        !JK   SCHARFKANTIGES WEHR
        IF (wart.eq.'sk') then     ! wenn scharfkantiges Wehr
          wh = hwmin (j) - hming   ! minimale Wehrhoehe - minimalew GOK im Wehrprofil
          WRITE (UNIT_OUT_LOG, '(/,''scharfkantiges Wehr'')')
        ENDIF

        !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
        CALL beiwert (huew (j), j, wh, h_v, h_uw_w, iueart(j), cq (j), cm)

        !HW  SCHREIBEN IN KONTROLLFILE
        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
          WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
          WRITE (UNIT_OUT_LOG, '(''g2    = '',f8.4)') g2
          WRITE (UNIT_OUT_LOG, '(''cq    = '',f8.4)') cq (j)
          WRITE (UNIT_OUT_LOG, '(''huew  = '',f8.4)') huew (j)
          WRITE (UNIT_OUT_LOG, '(''hv    = '',f8.4)') hv
        ENDIF

        !HW  Berechnung des spezifischen Wehrabflusses q nach DU BUAT
        q_b = (2./3.) * g2 * cq (j) * ((huew (j) + h_v) **1.5)
        q_w (j) = q_b * wb (j)
        hgrw (j) = ((q_b**2.) / g) ** (1. / 3.)   ! Grenztiefe
      ENDIF   ! Ende zum vollkommenen Ueberfall

      q_uea = q_uea + q_w (j)

      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''Summation des Wehrabflusses bis hier:q_uea='',f8.4)') q_uea
      ENDIF
    2110 END DO

  
    !MD  IF (h_uw.lt.hokwmin) then    ! Unterwasserstand =< minimale Wehrhoehe
    !MD  ... siehe unten
    !MD    goto 2400                  ! --> Weiter zur Auswertung
    !MD  ELSE
    q_uea = 0.0
    ABFRAGE = 'FALL A'
    ! GOTO 2020
    ! ----------------------------------------------------------------------------------------------
    DO 2021 j = 1, nwfd   ! Ueber alle Wehrfelder
      IF (h_uw.gt.0.0005) then

        tauu = (h_uw  - hokwmin) / hgrw (j)   !MD   Wasserstand DES Unterwassers - Wehrhoehe
        taoo = (huew (j) + h_v) / hgrw (j)    !MD   ENERGIEHÖHE DES OBERWASSERS an Wehrkrone
        tauo = 1.725 - (0.525 * tauu)         !MD  Grenzgerade f(tauu) vollk - unvollk
        ! tauo = 3.286 - (1.905 * taoo)       !MD  Grenzgerade f(taoo) vollk - unvollk

        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''Berechnung der dimensionlosen Hoehen'')')
          WRITE (UNIT_OUT_LOG, '(/,''j='',i3,'' tauu='',f8.4,'' taoo='',f8.4)') j, tauu, taoo
        ENDIF
        !------------------------------------------------------------------------------------------
        !HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONIGE WEHR MITTELS DER
        !------------------------------------------------------------------------------------------
        IF (wart.eq.'bk') then
          !MD  Impuls
          taugrenz = SQRT ( (1 +(hokwmin/ hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) *g)) **2)    &
                        &  - (v_uw**2 / (g * hgrw (j))) - (hokwmin/ hgrw (j))

          if ((h_uw-sohlp(np-1)).le.hgrw(j)) then
            iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
            GOTO 2031       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
          elseif (h_uw.le.hokwmin) then
            iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
            GOTO 2031       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
          else
            iueart(j) = 2   !    Ueberstroemen
            GOTO 2041       !JK  Weiter ZU UEBERSTROEMEN
          end if

          !HW    SCHREIBEN IN KONTROLLFILE
          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
            WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')
            WRITE (UNIT_OUT_LOG, '(/,''Grenzwert für überströmen nach Imp.bilanz'')')
            WRITE (UNIT_OUT_LOG, '(''taugrenz = '',f8.4)') taugrenz
          ENDIF

     !     !HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz
     !     IF (tauu .gt. taugrenz) then
     !       iueart(j) = 2   !    Ueberstroemen
     !       GOTO 2041       !JK  Weiter ZU UEBERSTROEMEN
     !
     !     ELSE
     !       iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
     !       GOTO 2031       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
     !     ENDIF
        ENDIF   !HW ----------------------  ENDE BREITKRONIGES WEHR   ---------------------

        ! ZU UEBERSTROEMEN
        !-----------------------------------------------------------
        IF (tauu.ge.2 .and. taoo.ge.2) then
          iueart(j) = 2   !   Ueberstroemen
          goto 2041
        endif

        ! Grenze zwischen vollkommen und unvollkommenen Ueberfallen:
        !-----------------------------------------------------------
        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(''tauo='',f8.4)') tauo
          WRITE (UNIT_OUT_LOG, '(''tauu_grenz='',f8.4)') tauu_grenz
        ENDIF

        !MD  IF (tauo.ge.tauu .and. tauo.le.1.0) then
        IF (tauu.le.0.0) then  ! Wenn UW < Wehr-ok
          iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
          GOTO 2031
        ! ELSEIF (tauu.lt.2.0 .and. tauo.lt.tauu_grenz .and. tauu.le.tauo) then
        ELSEIF (tauo.lt.tauu_grenz .and. tauu.lt.tauu_grenz) then
          iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
          GOTO 2031

        ! ELSEIF (tauu.lt.2.0 .and. tauu.gt.tauo .and. taoo.lt.tauu) then
        ELSEIF (tauu.lt.2.0 .and. taoo.lt.2) then
          iueart(j) = 1   !    UNVOLLKOMMENER UEBERFALL

          WRITE (UNIT_OUT_LOG, '(/,''Unvollkommener Ueberfall'')')
          !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
          CALL beiwert (huew (j), j, wh, h_v, h_uw_w, iueart(j), cq (j), cm)

       !  q_b = (2. / 3.) * g2 * cq (j) * ((huew (j) + h_v - h_uw_w) **1.5 )
       !  q_w (j) = q_b * wl (j) * cm
          q_w (j) = q_w (j) * cm

          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w (Wehrueberfall)'')')
            WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4,'' cm='',f8.4)') q_w (j), cm
          ENDIF

          GOTO 2031

        ELSEIF (tauu.ge.2.) then
          write (UNIT_OUT_LOG, '(''Widerspruch !!!!: '', ''tauu < 2.0 <=> tauo > 2.0'',   &
          &               /,''Annahme Ueberstroemen !'')')
          iueart(j) = 2   !    Ueberstroemen
          GOTO 2041    !JK  Weiter ZU UEBERSTROEMEN
        ELSE
          iueart(j) = 2   !    Ueberstroemen
          GOTO 2041    !JK  Weiter ZU UEBERSTROEMEN
        ENDIF

        ! --------------------------------------------------------------------------
        !  SCHLEIFE 2040::  FUERS Ueberstroemen
        ! -----------------------------------------------------------------------------
        2041   iueart(j) = 2

        !HW    SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(/,''Ueberstroemen'')')
        WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w nach Knapp S.304'')')
        !MD    q_w (j) = auew (j) * g2 * (huew (j) - h_uw (j) ) **0.5
        !MD    Formel nach Knapp S 304: verbessert!
        if (auew(j).ge. f_ow) then
          K_verlust = ((lu_ow ** 2.) * (wb(j) * (hokwmin - sohlp(np))) *1.5) / &
                   &   ((auew(j) + wb(j) * (hokwmin - sohlp(np))) **2.)
        elseif (auew(j).lt. f_ow) then
          K_verlust = ((lu_ow ** 2.) * (f_ow - auew(j)) *1.5) / (f_ow **2.)
        end if

        if ((h_uw - hokwmin).gt.0.0) then
          q_w(j) = huew(j) * wb(j) * g2* ((huew(j) + h_v - (h_uw - hokwmin)) **0.5)
        endif

        IF (K_verlust.gt.0.0 .and. (huew(j) +h_v).gt.(h_uw -hokwmin)) then
          q_ueber(j) = huew(j) * wb(j) * g2 * (((huew(j) + h_v - (h_uw - hokwmin)) /(1. + K_verlust))  **0.5)
          q_w(j) =  q_ueber(j)
        ELSEIF (K_verlust.gt.0.0 .and. (huew(j) +h_v).le.(h_uw -hokwmin)) then
          q_ueber(j) = huew(j) * wb(j) * g2 * (((huew(j) + h_v) /(1.+ K_verlust))  **0.5)
          q_w(j) =  q_ueber(j)
        endif

  !    if (he_q.eq.hen1 .and. q_w(j).ge.q_wehr) then
  !       ! Korrektur der Kontigleichung uber v_uw --> v_uw_konti
  !       ! Impulsbilanz mit Wassertiefe nicht WSP sondern mit BEZUG auf Sohle UW
  !       v_uw_konti = q_wehr/f_uw
  !       TermA = f_uw * ((v_uw_konti**2.) + (g* (h_uw-sohlp(np-1)) /2.))
  !       TermB = (g/2.)*(hokwmin-sohlp(np-1)+huew(j)) * (auew(j) + ((auew(j)/huew(j)) *(hokwmin-sohlp(np-1))))
  !       TermC = auew(j)
  !
  !       if (TermA .gt. TermB) then
  !         v_uew(j) = SQRT ((TermA-TermB)/ TermC)
  !       else
  !         v_uew(j) = v_ow
  !       end if
  !       q_w (j) = huew(j) * wb(j) * v_uew(j)
  !
  !        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
  !          WRITE (UNIT_OUT_LOG, '(''huew(j)='',f8.4, '' aus Impulsbilanz'')')  (huew(j))
  !          WRITE (UNIT_OUT_LOG, '(''v_uew(j)='',f8.4, '' aus Impulsbilanz'')')  (v_uew(j))
  !        ENDIF
  !     end if

        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4, '' Ueberstroemen'')')  (q_w (j) )
        ENDIF
        ! --------------------------------------------------------------------------
        !  SCHLEIFE 2030::  FUERS Ueberfall vollk und unvollk
        ! -----------------------------------------------------------------------------
        2031  IF (ABFRAGE == 'FALL 0') THEN
          q_ue = q_ue+q_w (j)
          IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(''Summation q_ue bis hier: q_ue ='',f8.4)') q_ue
          ENDIF
        ELSEIF (ABFRAGE == 'FALL A') THEN
          q_uea = q_uea + q_w (j)
        ELSEIF (ABFRAGE == 'FALL 1') THEN
          q_ue = q_ue + q_w (j)
        END IF
      ENDIF

    2021 END DO
    ! ----------------------------------------------------------------------------------------------

    IF (abs (q_uea - q_wehr) .le.eps_q) then
       !MD  goto 2200               !Weiter zur Berechnung mit heb_q: ENTFERNT
       he_q = hea_q
       !MD  falsch rum  q_uea = q_ue
       q_ue = q_uea
       dif_q = 0.0
       goto 2400                  ! --> Weiter zur Auswertung
    Endif

  


    ! --------------------------------------------------------------------
    !    Auswertung der Abfluss-Iteration
    ! --------------------------------------------------------------------
    2300 dif_q = 0.001
    if (hea_q .ne. he_q) then
      df = (q_uea - q_ue) / (hea_q - he_q)      !ACHTUNG:: Nur gueltig fuer den gleichen Abflusszustand!!!
    else
    !  df = 0.0       ! so wird dif_q = 0.001 behalten
    end if

    !MD  Steigung der Geraden aus dq / dhe
    !MD  dhe = 0.2
    !HW  Schreiben in Kontrollfile
    IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(/,''q_uea  ='',f8.4)') q_uea
      WRITE (UNIT_OUT_LOG, '(''hea_q  ='',f8.4)') hea_q
      WRITE (UNIT_OUT_LOG, '(''df     ='',f8.4)') df
      WRITE (UNIT_OUT_LOG, '(''q_wehr ='',f8.4)') q_wehr
      WRITE (UNIT_OUT_LOG, '(''q_ue   ='',f8.4)') q_ue
    ENDIF

    ! Wenn Geraden Steigung kleiner 0.0001: dif_q = 0.001
    IF (abs (df) .gt.0.0001) dif_q = (q_wehr - q_ue) / df
  
    !MD   behalten der alten Energiehoehe
    IF (abs(q_uea - q_wehr) .le. abs(q_ue - q_wehr)) then
      hra = hea_q
    Elseif (abs(q_uea - q_wehr) .gt. abs(q_ue - q_wehr)) then
      hra = he_q
    endif

    2400  he_q = he_q + dif_q    !MD  neue Energiehoehe


    !HW    Schreiben in Kontrollfile
    IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(/,''Ende eines Iterationsschrittes '')')
      WRITE (UNIT_OUT_LOG, '(''Festlegen neuer Eingangswerte '')')
      WRITE (UNIT_OUT_LOG, '(''alt: he_q ='',f8.4)') hra
      WRITE (UNIT_OUT_LOG, '(''dif_q     ='',f8.4)') dif_q
      WRITE (UNIT_OUT_LOG, '(''neu: he_q = he_q + dif_q ='',f8.4)') he_q
    ENDIF

    ! Energiehoehe > minimale Wehroberkante
    !MD   IF (hen1.ge.hokwmin) then
    !MD   macht keinen Sinn: wenn Energiehoehe im UW kleiner minimale Wehrkante, dann ...
    !MD   hier irrelevant fuer die neue Energiehoehe im OW

    IF ( (he_q - hen1) .lt.0.0001) then  ! Energiehhoehe UW - Energiehohe OW < 0.001
      he_q =  (hra + he_q)/ 2.
    !  IF (he_q .le. hen1) then      ! neue Energiehoehe =< Energiehoehe UW
    !    he_q = (hen1 + he_q)/2.
    !  endif
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Korrektur1 der neuen Energiehoehe'')')
        WRITE (UNIT_OUT_LOG, '('' mit he_q = (he_q,alt + he,UW) / 2 '')')
        WRITE (UNIT_OUT_LOG, '(''he_q  = '',f8.4)') he_q
      ENDIF

    ELSEIF ( (he_q - hokwmin) .lt.0.001) then    ! Energiehhoehe UW - minimale Wehrkante < 0.001
      he_q = (hra + hokwmin) / 2.
    !  IF (he_q .le. hen1) then      ! neue Energiehoehe =< Energiehoehe UW
    !    he_q = (hen1 + he_q)/2.
    !  endif
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Korrektur2 der neuen Energiehoehe '')')
        WRITE (UNIT_OUT_LOG, '('' mit he_q = (he_q,alt + hokwmin) / 2 '')')
        WRITE (UNIT_OUT_LOG, '(''he_q  = '',f8.4)') he_q
      ENDIF
    ENDIF


    !---------------------------------------------------
    !HW   Abspeichern des guenstigsten Ergebnisses
    !---------------------------------------------------

    ! Kontrolle der Abfluesse
    ! --------------------------------

    IF (abs (q_wehr - q_ue) .lt.absmax) then
      absmax = abs (q_wehr - q_ue)
      q_wopt = q_ue
    ELSEIF (abs (q_wehr - q_uea) .lt.absmax) then
      absmax = abs (q_wehr - q_uea)
      q_wopt = q_uea
    ENDIF

    2000 CONTINUE

    ! Kontrolle der Energiehoehe
    ! --------------------------------
    dif_e = abs (he_q - he)

    IF (dif_e .lt.absmax1) then
      absmax1 = dif_e
      he_opt = he
      h_v_opt = h_v
      q_wehr_opt = q_wehr

      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Energiehoehe'')')
        WRITE (UNIT_OUT_LOG, '(''he_q  = '',f8.4)') he_q
        WRITE (UNIT_OUT_LOG, '(''he    = '',f8.4)') he
        WRITE (UNIT_OUT_LOG, '(''dif_e = he_q - he = '',f8.4,/)') dif_e
        WRITE (UNIT_OUT_LOG, '(''he_opt= he = '',f8.4,/)') he_opt
      ENDIF

    ENDIF

  1000 CONTINUE  ! Zurueck zur While Schleife, wenn bedingung mit dif_e erfüllt ENDE
  ! --------------------------------------------------------------------------------
  ! --------------------------------------------------------------------------------
  !!! Ende While-schleife
  ! --------------------------------------------------------------------------------



  ! ------------------------------------------------------------------------
  ! Ergebnisrechnung    :: nach Abschluss der Iteration fuer die Energiehöhe
  ! ------------------------------------------------------------------------
  4001 CONTINUE

  !MD  Fuer keine Konvergenz in der Engergiehoehen Iteration
  !-----------------------------------

  !MD IF (it1000.gt.it1000max .and. he_opt.ne.0.0) then
  IF (it1000.gt.it1000max .and. he_opt.eq.0.0) then
    he_q = he
   !MD  h_v = h_v_opt    ! wurde nie belegt
   !MD  q_wehr = q_wehr_opt
  endif


  WRITE (UNIT_OUT_LOG, '(/,''Ergebnis der Wehrberechnung:'')')
  h_ow = he_q - h_v
  ! CALL g_wehr (he_q, ifehl, auew, huew, wl, hwmin)
  CALL g_wehr (h_ow, ifehl, auew, huew, wb, hwmin)
  q_ue = 0.0

  DO 4010 j = 1, nwfd

    IF (huew (j) .lt.0.0001) then
      q_w (j) = 0.0
      hgrw (j) = 0.0
    ELSE
      iueart(j) = 0
      IF (wart.eq.'sk') wh = hwmin (j) - hming

      !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
      CALL beiwert (huew (j), j, wh, h_v, h_uw_w, iueart(j), cq (j), cm)

      !HW SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Berechnung nach Du Buat'')')
        WRITE (UNIT_OUT_LOG, '(''Ausgabe der Eingangswerte:'')')
        WRITE (UNIT_OUT_LOG, '(''g2   = '',f8.4)') g2
        WRITE (UNIT_OUT_LOG, '(''cq   = '',f8.4)') cq (j)
        WRITE (UNIT_OUT_LOG, '(''huew = '',f8.4)') huew (j)
        WRITE (UNIT_OUT_LOG, '(''hv   = '',f8.4)') hv
      ENDIF

      q_b = (2./3.) * g2 * cq (j) * ((huew (j) + h_v) **1.5)
      q_w (j) = q_b * wb (j)
      hgrw (j) = ((q_b**2.) / g) ** (1. / 3.)   ! Grenztiefe
    ENDIF

    q_ue = q_ue+q_w (j)

  4010 END DO


  !HW  SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '(/,''q_ue   ='',f8.4)') q_ue
    WRITE (UNIT_OUT_LOG, '(''i      ='',10i8)')  (i, i = 1, nwfd)
    WRITE (UNIT_OUT_LOG, '(''q_w(i) ='',10f8.4)') (q_w (i) , i = 1, nwfd)
    WRITE (UNIT_OUT_LOG, '(''cq(i)  ='',10f8.4)')  (cq (i) , i = 1, nwfd)
    WRITE (UNIT_OUT_LOG, '(''hgrw(i)='',10f8.4)') (hgrw (i) , i = 1, nwfd)
  ENDIF

  !MD  IF (h_uw.le.hokwmin) then
  !MD    GOTO 4000
  !MD  ELSE
  q_ue = 0.0
  ABFRAGE = 'FALL 1'
  ! ----------------------------------------------------------------------------------------------
  DO j = 1, nwfd   ! Ueber alle Wehrfelder
    IF (h_uw .gt.0.0005) then

      tauu = (h_uw - hokwmin) / hgrw (j)      !MD   Wasserstand DES Unterwassers - Wehrhoehe
      taoo = (huew (j) + h_v ) / hgrw (j)     !MD   ENERGIEHÖHE DES OBERWASSERS an Wehrkrone
      tauo = 1.725 - (0.525 * tauu)
    ! tauo = 3.286 - (1.905 * taoo)

      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Berechnung der dimensionlosen Hoehen'')')
        WRITE (UNIT_OUT_LOG, '(/,''j='',i3,'' tauu='',f8.4,'' taoo='',f8.4)') j, tauu, taoo
      ENDIF
      !------------------------------------------------------------------------------------------
      !HW    KONTROLLE DER UEBERFALLART FUER DAS BREITKRONIGE WEHR MITTELS DER
      !HW    IMPULSBILANZ GEMAESS BWK/MERKBLATT 1, Seite 55, Abb. 3.34
      !------------------------------------------------------------------------------------------
      IF (wart.eq.'bk') then
        !HW    SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(/,''Breitkroniges Wehr'')')
        WRITE (UNIT_OUT_LOG, '(/,''Kontrolle der Ueberfallart'')')

        !MD  Impuls aus BEZUG Sohle UW
        taugrenz = SQRT ( (1 +(hokwmin/ hgrw (j))) **2 + 2 + (v_uw**2 / (hgrw (j) *g)) **2)    &
                        &  - (v_uw**2 / (g * hgrw (j))) - (hokwmin/ hgrw (j))

        if ((h_uw-sohlp(np-1)).le.hgrw(j)) then
          iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
          GOTO 2033       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
        elseif (h_uw.le.hokwmin) then
          iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
          GOTO 2033       !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
        else
          iueart(j) = 2   !    Ueberstroemen
          GOTO 2043       !JK  Weiter ZU UEBERSTROEMEN
        end if

  !      !HW    Kontrolle der Abflussart bei breitkronigem Wehr mit taugrenz
  !      IF (tauu .gt. taugrenz) then
  !        iueart(j) = 2   !    Ueberstroemen
  !        GOTO 2043    !JK  Weiter ZU UEBERSTROEMEN
  !
  !      ELSE
  !        iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
  !        GOTO 2033    !JK  Weiter ZUM VOLLKOMMENER UEBERFALL
  !      ENDIF

      ENDIF
      !HW ---------------------------------  ENDE BREITKRONIGES WEHR   ---------------------

      ! ZU UEBERSTROEMEN
      !-----------------------------------------------------------
      IF (tauu.ge.2 .and. taoo.ge.2) then
        iueart(j) = 2   !   Ueberstroemen
        goto 2043
      endif

      ! Grenze zwischen vollkommen und unvollkommenen Ueberfallen:
      !-----------------------------------------------------------
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''tauo='',f8.4)') tauo
        WRITE (UNIT_OUT_LOG, '(''tauu_grenz='',f8.4)') tauu_grenz
      ENDIF

      !MD  IF (tauo.ge.tauu .and. tauo.le.1.0) then
      IF (tauu.le.0.0) then  ! Wenn UW < Wehr-ok
        iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
        GOTO 2033

      ! ELSEIF (tauu.lt.2.0 .and. tauo.lt.tauu_grenz .and. tauu.le.tauo) then
      ELSEIF (tauo.lt.tauu_grenz .and. tauu.lt.tauu_grenz) then
        iueart(j) = 0   !    VOLLKOMMENER UEBERFALL
        GOTO 2033

      ! ELSEIF (tauo.ge.tauu_grenz .and. tauu.lt.2.0) then
      ELSEIF (tauu.lt.2.0 .and. taoo.lt.2) then
        iueart(j) = 1   !    UNVOLLKOMMENER UEBERFALL

        WRITE (UNIT_OUT_LOG, '(/,''Unvollkommener Ueberfall'')')
        !MD CALL beiwert (huew (j), j, wh, h_uw (j), iueart, cq (j), cm)
        CALL beiwert (huew (j), j, wh, h_v, h_uw_w, iueart(j), cq (j), cm)

     !   q_b = (2. / 3.) * g2 * cq (j) * ((huew (j) + h_v - h_uw_w) **1.5)
     !   q_w (j) = q_b * wl (j) * cm
        q_w (j) = q_w (j) * cm

        IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w (Wehrueberfall)'')')
          WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4,'' cm='',f8.4)') q_w (j), cm
        ENDIF

        GOTO 2033

      ELSEIF (tauu.ge.2.) then
        write (UNIT_OUT_LOG, '(''Widerspruch !!!!: '', ''tauu < 2.0 <=> tauo > 2.0'',   &
        &               /,''Annahme Ueberstroemen !'')')
        iueart(j) = 2   !    Ueberstroemen
        GOTO 2043    !JK  Weiter ZU UEBERSTROEMEN
      ELSE
        iueart(j) = 2   !    Ueberstroemen
        GOTO 2043    !JK  Weiter ZU UEBERSTROEMEN
      ENDIF

      ! --------------------------------------------------------------------------
      !  SCHLEIFE 2040::  FUERS Ueberstroemen
      ! -----------------------------------------------------------------------------
      2043    iueart(j) = 2
      !HW    SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/,''Ueberstroemen'')')
        WRITE (UNIT_OUT_LOG, '(/,''Berechnung q_w nach Knapp S.304'')')
      ENDIF
      !MD    q_w (j) = auew (j) * g2 * (huew (j) - h_uw (j) ) **0.5
      !MD    Formel nach Knapp S 304: verbessert!
      if (auew(j).ge. f_ow) then
        K_verlust = ((lu_ow ** 2.) * (wb(j) * (hokwmin - sohlp(np))) *1.5) / &
                 &   ((auew(j) + wb(j) * (hokwmin - sohlp(np))) **2.)
      elseif (auew(j).lt. f_ow) then
        K_verlust = ((lu_ow ** 2.) * (f_ow - auew(j)) *1.5) / (f_ow **2.)
      end if

      if ((h_uw - hokwmin).gt.0.0) then
        q_w(j) = huew(j) * wb(j) * g2* ((huew(j) + h_v - (h_uw - hokwmin)) **0.5)
      endif

      IF (K_verlust.gt.0.0 .and. (huew(j) +h_v).gt.(h_uw -hokwmin)) then
        q_ueber(j) = huew(j) * wb(j) * g2 * (((huew(j) + h_v - (h_uw - hokwmin)) /(1. + K_verlust))  **0.5)
        q_w(j) =  q_ueber(j)
      ELSEIF (K_verlust.gt.0.0 .and. (huew(j) +h_v).le.(h_uw -hokwmin)) then
        q_ueber(j) = huew(j) * wb(j) * g2 * (((huew(j) + h_v) /(1.+ K_verlust))  **0.5)
        q_w(j) =  q_ueber(j)
      endif

  !     if (he_q.eq.hen1 .and. q_w(j).ge.q_wehr) then
  !        ! Korrektur der Kontigleichung uber v_uw --> v_uw_konti
  !        ! Impulsbilanz mit Wassertiefe nicht WSP sondern mit BEZUG auf Sohle UW
  !        v_uw_konti = q_wehr/f_uw
  !        TermA = f_uw * ((v_uw_konti**2.) + (g* (h_uw-sohlp(np-1)) /2.))
  !        TermB = (g/2.)*(hokwmin-sohlp(np-1)+huew(j)) * (auew(j) + ((auew(j)/huew(j)) *(hokwmin-sohlp(np-1))))
  !        TermC = auew(j)
  !
  !        if (TermA .gt. TermB) then
  !          v_uew(j) = SQRT ((TermA-TermB)/ TermC)
  !        else
  !          v_uew(j) = v_ow
  !        end if
  !        q_w (j) = huew(j) * wb(j) * v_uew(j)
  !
  !      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
  !        WRITE (UNIT_OUT_LOG, '(''huew(j)='',f8.4, '' aus Impulsbilanz'')')  (huew(j))
  !        WRITE (UNIT_OUT_LOG, '(''v_uew(j)='',f8.4, '' aus Impulsbilanz'')')  (v_uew(j))
  !      ENDIF
  !    end if

      IF (Q_Abfrage.ne.'WE_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''q_w(j)='',f8.4, '' Ueberstroemen'')')  (q_w (j) )
      ENDIF
      ! --------------------------------------------------------------------------
      !  SCHLEIFE 2030::  FUERS Ueberfall vollk und unvollk
      ! -----------------------------------------------------------------------------
      2033  IF (ABFRAGE == 'FALL 0') THEN
        q_ue = q_ue+q_w (j)
        WRITE (UNIT_OUT_LOG, '(''Summation q_ue bis hier: q_ue ='',f8.4)') q_ue
      ELSEIF (ABFRAGE == 'FALL A') THEN
        q_uea = q_uea + q_w (j)
      ELSEIF (ABFRAGE == 'FALL 1') THEN
        q_ue = q_ue + q_w (j)
      END IF
    ENDIF
  END DO
 ! -------------------------------------------------------------------------------------
 ! -------------------------------------------------------------------------------------





!-----------------------------------------------------------------
!                 E R G E B N I S A U S G A B E
!-----------------------------------------------------------------

!JK    SCHREIBEN IN KONTROLLFILE
4000 CONTINUE


WRITE (UNIT_OUT_LOG, '(/,''Ergebnis der Wehrberechnung'',/,     &
     & ''he_q='',f8.4,'' q_wehr='',f8.4)') he_q, q_ue

!HW  Ausgabe des Ueberfallbeiwertes und der Ueberfallart in Ergebnisfi
WRITE (UNIT_OUT_TAB, '(/,5x,''Ergebnisse der Wehrberechnung'')')
WRITE (UNIT_OUT_TAB, '(5x,''-----------------------------'')')
WRITE (UNIT_OUT_TAB, '(5x,''Feld-Nr.'',4x,''mue'',10x,           &
      &''q-wehr'',5x,''h-ue'',8x,''A-ue'',6x,''Ü-Art'')')

DO 5000 j = 1, nwfd
  !HW  Abfragen der Ueberfallart fuer das Wehrfeld
  IF (iueart(j) .eq.0) then
    uearttxt = 'vollkommen'
  ELSEIF (iueart (j) .eq.1) then
    uearttxt = 'unvollkommen'
  ELSE
    uearttxt = 'Überstroemen'
  ENDIF
  WRITE (UNIT_OUT_TAB, '(5x,i2,10x,f6.4,5x,f7.3,5x,f6.3,5x,f7.3,5x,a)        &
   &') j, cq (j) , q_w (j) , huew (j) , auew (j) , uearttxt

  !MD  NEUER BERECHNUNGSMODUS
  IF (BERECHNUNGSMODUS == 'REIB_KONST' .and. Q_Abfrage == 'WE_SCHLEIFE') then
    WRITE (UNIT_OUT_WEHR, '(4x,f7.3,1x,f10.3,1x,f8.3,1x,f10.3,2x,f6.4,2x,f7.3,2x,f6.3,2x,f7.3, &
       & 3x,a,1x,f10.3,1x,f10.3,1x,f8.3,1x,f10.3,1x,f8.3)')  &
       & qw, h_ow, v_ow, he, cq (j), q_w (j), huew (j), auew (j), uearttxt, he_q, h_uw, v_uw, hen1, K_verlust

    WRITE (UNIT_OUT_QWEHR, '(1x,F10.4,4F10.3,3x,a)')  &
       & stat (np), qw, q_w (j), h_ow, h_uw, uearttxt

    WRITE (UNIT_OUT_HEWEHR, '(1x,F10.4,4F10.3,3x,a)')  &
       & stat (np), qw, q_w (j), he, hen1, uearttxt
  endif

5000 END DO


!HW  Ausgabe der Ergebnisse in Ergebnisfile
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

IF (it1000.gt.it1000max) then
  he = he_q
endif
!MD  Ruecksetzen der falschen Energiehoehe he im OW infolge


RETURN

END SUBROUTINE w_ber     !HW    Ende der Subroutine W_BER !




  
