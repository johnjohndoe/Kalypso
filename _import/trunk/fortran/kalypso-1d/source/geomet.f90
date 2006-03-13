!     Last change:  WP   10 Mar 2006   10:11 pm
!--------------------------------------------------------------------------
! This code, geomet.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - geomet
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
!***********************************************************************



SUBROUTINE geomet (hr, x1, h1, hdif, hsohl, nknot, il, ir, q, wl, rhy, ifehl2)
!**
!JK   BESCHREIBUNG: BERECHNUNG GEOMETRIE AN BRUECKEN - FLAECHEN         
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!     - input: x1,h1 --> bezugslinie im profil
!              hr    --> ermittelter wsp im profil                      
!              cd    --> drag-koeffizient                               
!              il,ir --> punktnr. linkes und rechtes widerlager         
!              q     --> durchfluss                                     
!              iart  --> art der impulsgleichung (1,2,3)                
!                                                                       
!     - output:    m --> ermittelter impuls                             
!                                                                       
!**
!**   AUFGERUFENE ROUTINEN
!**   --------------------                                              
!**   schnpkt (xa,ya,xe,ye,x3,y3,x4,y4,sx,sy,inside,ikenn)              
!**   linie                                                             
!JK   schwpkt                                                           
!JK   prf                                                               
!JK   alg1                                                              
!JK   reih_inv                                                          
!
!     wird benutzt in bruecke
!                     intdat                                            
!                     impuls                                            
!                     wspber                                            
!***********************************************************************

                                                                        
!**   ------------------------------------------------------------------
!**   VEREINBARUNGSTEIL                                                 
!**   ------------------------------------------------------------------
                                                                        
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

REAL x1 (maxkla), h1 (maxkla)

REAL x1n (maxkla), h1n (maxkla)
REAL xl (maxkla), hl (maxkla), zl (maxkla)
REAL xl1 (maxkla), hl1 (maxkla)

REAL as (max2), hs (max2)
REAL hps (max2), ap (max2)
REAL apls (max2), aues (max2)
REAL hpls (max2), hues (max2)
REAL xukn (maxkla), hukn (maxkla)
REAL m

COMMON / flaechen / ad, aue, apl, apg, hss, hue, hpl, hp  

! **************************************************************
! common-block fuer die brueckenberechnung
CHARACTER(1) ibridge
REAL xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)

COMMON / brueck / iwl, iwr, nuk, nok, xuk, huk, xok, hok, hukmax, hokmax, hsuw, raub, breite, xk, ibridge

COMMON / brueck2 / hokmin                                           

!      **************************************************************
COMMON / pfeilerstau / alpha, bnetto
!     ***************************************************************   
!     common-bloecke boolsche operationen --> algebra                   
! ================================================                      
COMMON / angabe / xko (mpts, 2), hko (mpts, 2), na1, na2
! ================================================                      
COMMON / xr0yr0 / xr0 (mpts, max2), hr0 (mpts, max2), mr0 (max2), kr0
COMMON / xr1yr1 / xr1 (mpts, max2), hr1 (mpts, max2), mr1 (max2), kr1
COMMON / xr2yr2 / xr2 (mpts, max2), hr2 (mpts, max2), mr2 (max2), kr2
! ================================================                      
                                                                        
COMMON / iprint / id
CHARACTER(2) id
                                                                        
!WP 10.05.2004                                                          
!WP Common Block angepasst                                              
INTEGER :: isohl, iming
REAL :: hming
COMMON / p3 / isohl, hming, iming  
!WP 10.05.2004                                                          

INTEGER :: ifehl1


!WP -------------------------------------------------------
!WP Kontrollausgaben
!WP -------------------------------------------------------


write (UNIT_OUT_LOG, 5000) hr, hdif, hsohl, nknot, il, ir, q
5000 format (//1X, 'Start der Subroutine GEOMET'              , /, &
             & 1X, '---------------------------'              , /, &
             & 1X, 'HR (ermittelter WSP im Profil):   ', F15.5, /, &
             & 1X, 'HDIF (?):                         ', F15.5, /, &
             & 1X, 'HSOHL (Hoehe der Sohle?):         ', F15.5, /, &
             & 1X, 'NKNOT (Anzahl der Knoten?):       ', I15  , /, &
             & 1X, 'IL (Punktnr. linkes Widerlager):  ', I15  , /, &
             & 1X, 'IR (Punktnr. rechtes Widerlager): ', I15  , /, &
             & 1X, 'Q (Abfluss gesamt):               ', F15.5, /)


!**   ------------------------------------------------------------------
!**   BERECHNUNGEN                                                      
!**   ------------------------------------------------------------------
                                                                        
                                                                        
!    ueberpruefen ob bedingungen  eingehalten sind:                     
ifehl2 = 0

IF (hukmax .gt. hokmin) then
  PRINT * , 'die bedingungen fuer die berechnung der '
  PRINT * , 'bruecken mit impuls sind nicht gegeben, '
  PRINT * , 'da die max. hoehe der brueckenuk groesser ist'
  PRINT * , 'als das mininmum der bruecken-ok zwischen den'
  PRINT * , 'beiden widerlagern (--> gemischter abfluss !)'
  PRINT *
  PRINT * , '--> empfehlung: neustart des programms, '
PRINT * , '    berechnung der bruecke nach hec'
ENDIF

id = ' '
igraf = 0       	!  /* --> grafik alg
igra = 0		!  /* --> grafik schwerpunkt
iflli = 0
iflre = 0
                                                                        
!     variablen x1,h1 --> x1n,h1n                                       
!     (damit die urspruenglichen profilwerte nicht ueberschrieben       
!     werden)                                                           
                                                                        
DO i = 1, nknot
  x1n (i) = x1 (i)
  h1n (i) = h1 (i)
END DO
                                                                        
nknotn = nknot
                                                                        
IF ( (hr - hsohl) .le. 0.0001) then
  PRINT * , 'wasserspiegel unterhalb tiefster sohle in impuls'
  STOP
ENDIF
                                                                        
gen2 = 0.00001
a = 0.
apg = 0.
hss = 0.
hp = 0.
aue = 0.
hue = 0.
apl = 0.
hpl = 0.

aue1 = 0.
aue2 = 0.
aue3 = 0.

hue1 = 0.
hue2 = 0.
hue3 = 0.

wl1 = 0.
wl2 = 0.
wl3 = 0.
wl = 0.

rhy = 0.
phyz = 0.

xmitte = x1 (isohl)
isohln = isohl
il = iwl		!  /* linkes widerlager
ir = iwr 		!  /* rechtes widerlager
nukn = nuk
isl = 0 		!  /* schnittpunkt wsp-gelaende links
isr = 0           	!  /* schnittpunkt wsp-gelaende rechts
                                                                        
                                                                        

!JK --------------------------------------------------------------
!JK suchen 1ter schnittpunkt mit hs=hr und sx > x1n(iwl) !!
!JK bestimmen der schnittpunkte wsp mit gelaende:
!JK --------------------------------------------------------------
                                                                        
! gerade (STRECKE) wsp:
                                                                        
xa = x1n (1)
ya = hr
xe = x1n (nknotn)
ye = hr

ikenn = 1 ! /* --> schnittpkt. zwischen 2 strecken gesucht

!JK       STRECKE DURCH LINKES WIDERLAGER (BRUECKE)
x3 = x1n (il)
y3 = h1n (il) + 100.
x4 = x1n (il)
y4 = h1n (il)
CALL schnpkt (xa, ya, xe, ye, x3, y3, x4, y4, sx, sy, inside, ikenn)

IF (inside.ne.0) then
!JK  Schnittpunkt gefunden zwischen LINKE widerlager-wand und ws
!    --> erster schnittpunkt gelaende-wsp (il->1) suchen
!JK      START SCHNITTPUNKTSUCHE GELAENDE-WSP LINKS
! ******************************************************************

  DO 210 i = il - 1, 1, - 1

    !JKV   IN PASCHE-VERSION ZUSAETZLICHE ABFRAGE
    IF (i.gt.0 .AND. i.lt.maxkla) then
    !JKV   ENDE

      x3 = x1n (i)
      y3 = h1n (i)

      x4 = x1n (i + 1)
      y4 = h1n (i + 1)

      CALL schnpkt (xa, ya, xe, ye, x3, y3, x4, y4, sx, sy, inside, ikenn)

      IF (inside.ne.0) then
        ! schnittpunkt gefunden

        isg = isg + 1

        ! einsortieren schnittpunkt in gelaende, wenn nicht
        ! identisch mit anfangs- oder endpunkt

        dpx = x3 - sx
        dpy = y3 - sy
        dp1 = sqrt (dpx * dpx + dpy * dpy)
        dpx = x4 - sx
        dpy = y4 - sy
        dp2 = sqrt (dpx * dpx + dpy * dpy)

        IF (dp1.le.gen2) then
          ! schnittpunkt mit anfangspunkt der strecke p3/p4 identisch
          isl = i

        ELSEIF (dp2.le.gen2) then
          ! schnittpunkt mit endpunkt der strecke p3/p4 identisch
          isl = i + 1

        ELSE
          isl = i + 1

          IF (x1n (isohln) .ge.sx) isohln = isohln + 1

          DO 211 ii = nknotn, isl, - 1
            !JKV ZUSÄTZLICHE ABFRAGE AUS PASCHE-VERSION
            IF (ii.gt.0.AND.ii.lt.maxkla) then
              !JKV ENDE
              x1n (ii + 1) = x1n (ii)
              h1n (ii + 1) = h1n (ii)
              !JKV ZUSAETZLICHE ABFRAGE AUS PASCHE-VERSION
            ENDIF
            !JKV ENDE
          211 END DO

          nknotn = nknotn + 1
          x1n (isl) = sx
          h1n (isl) = sy

          IF (x1n (ir) .ge.x1n (isl) ) ir = ir + 1
          IF (x1n (il) .ge.x1n (isl) ) il = il + 1
        ENDIF

        !JK SCHNITTPUNKT GEFUNDEN
        !JK ZU SUCHEN 2. SCHNITTPUNKT --> WSP-GELAENDE RECHTS
        GOTO 120
        !JKV ZUSAETZLICHE ABFRAGE AUS PASCHE-VERSION
      ENDIF
      !JKV ENDE
    ENDIF

  210 END DO

!JK  ENDE SCHNITTPUNKTSUCHE
! *****************************************************************

!JK  ELSE ZU SCHNITTPUNKT ZWISCHEN WIDERLAGER (LINKS)
!JK  - WSP GEFUNDEN
ELSE

  ! schnittpunkt gelaende-wsp im bereich zwischen den beiden wl
  ! --> verschieben der wl-punkte
  !JK  wl = WIDERLAGER
  iflli = 1

  !JK ------------------------------------------------------------
  !JK START SCHNITTPUNKTSUCHE GELAENDE-WSP (LINKS)
  !JK ZWISCHEN WIDERLAGERN

  DO 110 i = isohln - 1, il, - 1
    !JKV ZUSAETZLICHE ABFRAGE AUS PASCHE-VERSION
    IF (i.gt.0.AND.i.lt.maxkla) then
      !JKV ENDE
      x3 = x1n (i)
      y3 = h1n (i)

      x4 = x1n (i + 1)
      y4 = h1n (i + 1)

      CALL schnpkt (xa, ya, xe, ye, x3, y3, x4, y4, sx, sy, inside, ikenn)

      IF (inside.ne.0) then
        ! Schnittpunkt gefunden
        isg = isg + 1

        ! Einsortieren schnittpunkt in gelaende, wenn nicht
        ! identisch mit anfangs- oder endpunkt

        dpx = x3 - sx
        dpy = y3 - sy
        dp1 = sqrt (dpx * dpx + dpy * dpy)
        dpx = x4 - sx
        dpy = y4 - sy
        dp2 = sqrt (dpx * dpx + dpy * dpy)

        IF (dp1.le.gen2) then
          ! Schnittpunkt mit anfangspunkt der strecke p3/p4  identisch
          il = i

        ELSEIF (dp2.le.gen2) then
          ! Schnittpunkt mit endpunkt der strecke p3/p4 identisch
          il = i + 1

        ELSE

          IF (x1n (isohln) .ge.sx) isohln = isohln + 1

          DO 111 ii = nknotn, i + 1, - 1
            !JKV  ABFRAGE AUS PASCHE-VERSION
            IF (ii.gt.0.AND.ii.lt.maxkla) then
              !JKV  ENDE
              x1n (ii + 1) = x1n (ii)
              h1n (ii + 1) = h1n (ii)
              !JKV  ABFRAGE AUS PASCHE-VERSION
            ENDIF
            !JKV  ENDE

          111 END DO

          nknotn = nknotn + 1
          il = i + 1
          ir = ir + 1
          x1n (il) = sx
          h1n (il) = sy
        ENDIF

        !JK  SCHNITTPUNKT GEFUNDEN
        !JK  ZU SUCHEN 2. SCHNITTPUNKT --> WSP-GELAENDE RECHTS
        GOTO 120
        !JKV  ABFRAGE AUS PASCHE-VERSION
      ENDIF
      !JKV   ENDE
    ENDIF
    !JK  ENDE SCHNITTPUNKTSUCHE GELAENDE-WSP (LINKS)
    !JK  ZWISCHEN WIDERLAGERN
    !JK  ------------------------------------------------
  110 END DO

  !JK       ENDIF ZU SCHNITTPUNKT ZWISCHEN WIDERLAGER (LINKS)
  !JK       - WSP GEFUNDEN
ENDIF                                                                     
                                                                        


120 CONTINUE
                                                                        

!  Suchen 2ter schnittpunkt mit hs=hr und sx<x1n(iwr) !!
!  bestimmen der schnittpunkte wsp mit gelaende:
!  gerade wsp:

!JK  STRECKE DURCH RECHTES WIDERLAGER (BRUECKE)
x3 = x1n (ir)
y3 = h1n (ir) + 100.
x4 = x1n (ir)
y4 = h1n (ir)

CALL schnpkt (xa, ya, xe, ye, x3, y3, x4, y4, sx, sy, inside, ikenn)

IF (inside.ne.0) then
  !JK  Schnittpunkt gefunden zw. widerlager-wand (RECHTS) und wsp
  !    --> 1ter schnittpunkt gelaende-wsp(ir-->nknot) suchen
  !JK  START SCHNITTPUNKTSUCHE GELAENDE-WSP RECHTS
  !JK ********************************************************

  DO 230 i = ir, nknotn - 1
    !JKV  ABFRAGE AUS PASCHE-VERSION
    IF (i.gt.0.AND.i.lt.maxkla) then
    !JKV  ENDE
      x3 = x1n (i)
      y3 = h1n (i)

      x4 = x1n (i + 1)
      y4 = h1n (i + 1)

      CALL schnpkt (xa, ya, xe, ye, x3, y3, x4, y4, sx, sy, inside, ikenn)

      IF (inside.ne.0) then
        ! Schnittpunkt gefunden
        ! Einsortieren schnittpunkt in gelaende, wenn nicht
        ! identisch mit anfangs- oder endpunkt

        dpx = x3 - sx
        dpy = y3 - sy
        dp1 = sqrt (dpx * dpx + dpy * dpy)
        dpx = x4 - sx
        dpy = y4 - sy
        dp2 = sqrt (dpx * dpx + dpy * dpy)

        IF (dp1.le.gen2) then
          ! Schnittpunkt mit anfangspunkt der strecke p3/p4 identisch
          isr = i

        ELSEIF (dp2.le.gen2) then
          ! Schnittpunkt mit endpunkt der strecke p3/p4 identisch
          isr = i + 1

        ELSE
          ! Einsortieren schnittpunkt in gelaendelinie
          isr = i + 1

          IF (x1n (isohln) .ge.sx) isohln = isohln + 1

          DO 231 ii = nknotn, isr, - 1
            !JKV   ABFRAGE AUS PASCHE-VERSION
            IF (ii.gt.0.AND.ii.lt.maxkla) then
            !JKV   ENDE
              x1n (ii + 1) = x1n (ii)
              h1n (ii + 1) = h1n (ii)
            !JKV  ABFRAGE AUS PASCHE-VERSION
            ENDIF
            !JKV  ENDE
          231 END DO

          nknotn = nknotn + 1
          x1n (isr) = sx
          h1n (isr) = sy

        ENDIF

        !JK  2. SCHNITTPUNKT GEFUNDEN
        !JK  ZU FLAECHEN- UND SCHWERPUNKTBESTIMMUNG
        GOTO 140
      ENDIF
      !JKV  ABFRAGE AUS PASCHE-VERSION
    ENDIF
    !JKV  ENDE

  230 END DO

  !JK  ENDE SCHNITTPUNKTSUCHE GELAENDE-WSP RECHTS
  ! ********************************************************
  !JK ELSE ZU SCHNITTPUNKT ZWISCHEN WIDERLAGER (RECHTS)
  !JK - WSP GEFUNDEN
ELSE

  ! Schnittpunkt gelaende-wsp im bereich zwischen den beiden wl
  ! --> verschieben rechtes wl
  iflre = 1

  !JK ------------------------------------------------------------
  !JK START SCHNITTPUNKTSUCHE GELAENDE-WSP (RECHTS)
  !JK ZWISCHEN WIDERLAGERN

  DO 130 i = isohln, ir - 1
    !JKV  ABFRAGE AUS PASCHE-VERSION
    IF (i.gt.0.AND.i.lt.maxkla) then
      !JKV  ENDE
      x3 = x1n (i)
      y3 = h1n (i)
      x4 = x1n (i + 1)
      y4 = h1n (i + 1)

      CALL schnpkt (xa, ya, xe, ye, x3, y3, x4, y4, sx, sy, inside, ikenn)

      IF (inside.ne.0) then
        ! Schnittpunkt gefunden
        isg = isg + 1

        ! Einsortieren schnittpunkt in gelaende, wenn nicht
        ! identisch mit anfangs- oder endpunkt

        dpx = x3 - sx
        dpy = y3 - sy
        dp1 = sqrt (dpx * dpx + dpy * dpy)
        dpx = x4 - sx
        dpy = y4 - sy
        dp2 = sqrt (dpx * dpx + dpy * dpy)

        IF (dp1.le.gen2) then
          ! Schnittpunkt mit anfangspunkt der strecke p3/p4 identisch
          ir = i

        ELSEIF (dp2.le.gen2) then
          ! Schnittpunkt mit endpunkt der strecke p3/p4 identisch
          ir = i + 1

        ELSE
          ! Einsortieren schnittpunkt in gelaendelinie

          IF (x1n (isohln) .ge.sx) isohln = isohln + 1
          DO ii = nknotn, i + 1, - 1
            !JKV  ABFRAGE AUS PASCHE-VERSION
            IF (ii.gt.0.AND.ii.lt.maxkla) then
              !JKV  ENDE
              x1n (ii + 1) = x1n (ii)
              h1n (ii + 1) = h1n (ii)
              !JKV  ABFRAGE AUS PASCHE-VERSION
            ENDIF
            !JKV  ENDE
          END DO

          nknotn = nknotn + 1
          ir = i + 1
          x1n (ir) = sx
          h1n (ir) = sy

        ENDIF

        !JK  2. SCHNITTPUNKT GEFUNDEN
        !JK  ZU FLAECHEN- UND SCHWERPUNKTBESTIMMUNG
        GOTO 140

      ENDIF
      !JKV  ABFRAGE AUS PASCHE-VERSION
    ENDIF
    !JKV  ENDE

    !JK  ENDE SCHNITTPUNKTSUCHE GELAENDE-WSP (RECHTS)
    !JK  ZWISCHEN WIDERLAGERN
    !JK  ------------------------------------------------------------
  130 END DO

!JK  ENDIF ZU SCHNITTPUNKT ZWISCHEN WIDERLAGER (RECHTS)
!JK  - WSP GEFUNDEN
ENDIF                                                                 
                                                                        
                                                                        

140 CONTINUE
                                                                        

IF (isl.eq.0) isl = 1
IF (isr.eq.0) isr = nknotn
                                                                        
igew = 0
!JK    UNSINNIGE ABFRAGE --> TRITT NIE EIN, DA igew=0 GESETZT----       
IF (igew.eq.1) then
  PRINT *
  PRINT *
  PRINT * , '**************************************************'
  PRINT * , '* ermittelt in impuls :                          *'
  PRINT * , '*                                                *'
  PRINT * , '* hr       = ', hr
  PRINT * , '* nknotn   = ', nknotn
  PRINT * , '* x1n(isl) = ', x1n (isl)
  PRINT * , '* x1n(isr) = ', x1n (isr)
  PRINT * , '* x1n(il)  = ', x1n (il)
  PRINT * , '* x1n(ir)  = ', x1n (ir)
  PRINT *
  PRINT * , '* hsohl    = ', hsohl
  PRINT * , '* hukmax   = ', hukmax
  PRINT * , '* hokmin   = ', hokmin
  PRINT * , '* xmitte   = ', xmitte
  PRINT * , '**************************************************'
ENDIF                                                             
!JK    ENDE UNSINN-----------------------------------------------       
                                                                        



! *************************************************************************
! Flaechen- und Schwerpunktbestimmung
! *************************************************************************
                                                                        
ia = 1
ie = nok


! Oberkante auswaehlen fuer bereiche 1,2,3
Oberkante1: DO i1 = 1, nok
  IF (xok (i1) .gt.x1n (il) ) then
    ia = i1
    EXIT Oberkante1
  ENDIF
END DO Oberkante1

Oberkante2: DO i1 = ia, nok
  IF (xok (i1) .gt.x1n (ir) ) then
    ie = i1 - 1
    EXIT Oberkante2
  ENDIF
END DO Oberkante2                   


! *************************************************************************
! - linie 1  gelaende-wsp, bzw. bei druckabfluss gelaende-hukmax
! *************************************************************************
                                                                        
!JK   WENN DRUCKABFLUSS                                                 
IF (hr.ge.hukmax) then
  hber = hukmax
!JK   WENN KEIN DRUCKABFLUSS                                            
ELSE
  hber = hr
ENDIF
                                                                        

CALL linie (hber, x1n, h1n, nknotn, il, ir, npl1, xl, hl) 

!JK    WAR SCHON DEAKTIVIERT, 27.04.00, JK                              
! call pktfilt(npl1,n,xl,hl,zl,gen2)
                                                                        
! Linie schliessen (1ter pkt.=letzter pkt.):
npl1 = npl1 + 1
xl (npl1) = xl (1)
hl (npl1) = hl (1)

DO i = 1, npl1
  xko (i, 1) = xl (i)
  hko (i, 1) = hl (i)
END DO               
                                                                        
                                                                        
! - verbauter querschnitt (widerlager,pfeiler...)

! Ueberschreiben der punkte, die vor dem linken widerlager bzw. nach
! dem rechten wl liegen.
                                                                        
DO i = 1, nukn
  xukn (i) = xuk (i)
  hukn (i) = huk (i)
END DO

Ueberschreiben: DO i = 1, nukn

  IF ( (xukn (i) - x1 (iwl) ) .lt. - 1.e-4) then

    DO i2 = 1, nukn - 1
      xukn (i2) = xukn (i2 + 1)
      hukn (i2) = hukn (i2 + 1)
    END DO

    nukn = nukn - 1
    CYCLE Ueberschreiben
  ENDIF

  IF ( (xukn (i) - x1 (iwr) ) .gt.1.e-4) then
    nukn = i - 1
    EXIT Ueberschreiben
  ENDIF

END DO Ueberschreiben                             
                                                                        

!  ****************************************
!  - linie 2    gelaende-brueckenunterkante
!  ****************************************
                                                                        
IF (nukn.lt.2) then
  PRINT * , 'anzahl der punkte nukn<2 , nukn=', nukn
  PRINT * , 'stop in impuls'
  STOP
ENDIF

npl2 = 0

DO i = iwl, iwr
  npl2 = npl2 + 1
  xko (npl2, 2) = x1 (i)
  hko (npl2, 2) = h1 (i) - 5.
END DO

DO i = nukn, 1, - 1
  npl2 = npl2 + 1
  xko (npl2, 2) = xukn (i)
  hko (npl2, 2) = hukn (i)
END DO

npl2 = npl2 + 1
xko (npl2, 2) = xko (1, 2)
hko (npl2, 2) = hko (1, 2)

! Zusammensetzen der fuer die berechnung benoetigten
! Bezugslinien
! Fuer die genauigkeit ausgetestet und verworfen sind schon:
! gen=0.1, gen=0.001
gen = 0.0001

! ueberpruefen der flaecheninhalte der beiden linien:
IF (igra.ne.0) print * , 'gelaende-wsp'
CALL schwpkt (maxkla, npl1, xl, hl, xx, hh, ah, igra, gen)

IF (ah.lt. - 0.01) then
  write (*,1000) ah
  1000 format (1X, 'Flaeche AH ist negativ! (', F10.4,')',/,&
             & 1X, '-> Programmabbruch!' )
  STOP
ENDIF


IF (npl2.gt.maxkla) then
  WRITE ( * , 1001) npl2, maxkla
  1001 format (1X, 'Anzahl der Punkte n= ', I5, /,&
             & 1X, 'ist groesser als zugelassen: MAXKLA= ', I5, /,&
             & 1X, '-> Programmabbruch!' )
  STOP
ENDIF



DO i = 1, npl2
  xl (i) = xko (i, 2)
  hl (i) = hko (i, 2)
END DO

IF (igra.ne.0) print * , 'gelaende- buk (-5!)'
CALL schwpkt (maxkla, npl2, xl, hl, xx, hh, ah, igra, gen)


IF (ah.gt.0.01) then

  genau = 0.001
  na1 = npl1
  na2 = npl2
  igew = 0

  !JK  NOCH IMMER UNSINNIGE ABFRAGE??---------------------------
  IF (igew.eq.1) then
    PRINT * , '**************************************'
    WRITE ( * , '(i5,2f10.3)') (ii, xko (ii, 1) , hko (ii, 1) , ii = 1, na1)
    PRINT * , '**************************************'
    WRITE ( * , '(i5,2f10.3)') (ii, xko (ii, 2) , hko (ii, 2) , ii = 1, na2)
    PRINT * , '**************************************'
  ENDIF
  !JK ENDE UNSINN??--------------------------------------------

  igew = 0
  kr1 = 0
  kr2 = 0
  kr0 = 0

  CALL prf (ik1, ik2)

  !JK  SCHREIBEN IN KONTROLLFILE
  IF (ik1.eq.1) write (UNIT_OUT_LOG, 1005) 1
  IF (ik2.eq.1) write (UNIT_OUT_LOG, 1005) 2

  1005 format (1X, 'Punkte der Linie Nr. ', I2,' abgefiltert!')

  CALL alg1 (igraf, ifehl)

  IF (ifehl.eq.1) then
    write (*,1006)
    1006 format (1X, 'Polygonzuege im algebra nicht fehlerfrei abgearbeitet!' )

    ifehl1 = 1
    ifehl2 = 1

  !JK SCHREIBEN IN KONTROLLFILE
  !WP ELSEIF (ifehl1.eq.3.and.lein.eq.3) then
  ELSE

    WRITE (UNIT_OUT_LOG, 1007)
    1007 format (1X, 'Linie 1 Gelaende-WSP, bzw. bei Druckabfluss Gelaende-HUKMAX:' )

    write (UNIT_OUT_LOG, 1008) (ii, xko (ii, 1) , hko (ii, 1) , ii = 1, na1)
    !WP WRITE (UNIT_OUT_LOG, '(i5,2f10.3)') (ii, xko (ii, 1) , hko (ii, 1) , ii = 1, na1)
    1008 format (1X, I5, 2F10.3)

    WRITE (UNIT_OUT_LOG, 1009)
    1009 format (1X, 'Linie 2 Gelaende-Brueckenunterkante:' )

    WRITE (UNIT_OUT_LOG, 1008) (ii, xko (ii, 2) , hko (ii, 2) , ii = 1, na2)
    ! WP WRITE (UNIT_OUT_LOG, '(i5,2f10.3)') (ii, xko (ii, 2) , hko (ii, 2) , ii = 1, na2)

  ENDIF

  IF (kr0.eq.0) then
    write (*,1010)
    1010 format (1X, 'KR0 = 0 --> kein Durchflussquerschnitt!')

    ifehl2 = 1
    !JK  ZU FEHLER-, DATENUEBERGABE
    GOTO 9999
  ENDIF                                                                                





  ! ************************************************************
  ! --> 1. schnittmenge linie 1 mit 2 --> durchflussquerschnitt
  ! ************************************************************
                                                                        
  ! koordinaten xr0(mpts,max),yr0(mpts,max)
  ! anzahl der polygonzuege --> kr0
  ! anzahl der pkte je polygonzug mr0(1...kr0)
                                                                        
  polygonzuege: DO i = 1, kr0

    i_print = 0
    !JK UNSINNIG ABFRAGE------------------------------------
    IF (i_print.eq.1) then
      WRITE (UNIT_OUT_LOG, 1020) i, i
      1020 format (1X, 'XR0(', I3,') YR0(', I3,')' )

      WRITE (UNIT_OUT_LOG, 1021) (xr0 (ii, i) , hr0 (ii, i) , ii = 1, mr0 (i) )
      !WP WRITE (UNIT_OUT_LOG, '(2f8.3)') (xr0 (ii, i) , hr0 (ii, i) , ii = 1, mr0 (i) )
      1021 format (1X, 2F8.3)

    ENDIF
    !JK ENDE UNSINN-----------------------------------------

    DO ii = 1, mr0 (i)
      xl (ii) = xr0 (ii, i)
      hl (ii) = hr0 (ii, i)
    END DO

    npl = mr0 (i)

    IF (npl.lt.3) CYCLE polygonzuege

    ! Hydraulischer radius

    phy = 0.

    DO ii = 1, npl - 1

      rd1 = xl (ii + 1) - xl (ii)
      rd2 = hl (ii + 1) - hl (ii)
      phy = phy + sqrt (rd1 * rd1 + rd2 * rd2)

      phyz = phyz + sqrt (rd1 * rd1 + rd2 * rd2)

      !JK  WAR SCHON DEAKTIVIERT,27.04.00,JK
      !**  rhy=rhy+sqrt(rd1*rd1+rd2*rd2)
    END DO

    IF (igra.ne.0) WRITE (*,*) ' Durchflussquerschnitt:'

    !JK  KONTROLLE FLAECHENINHALT
    CALL schwpkt (maxkla, npl, xl, hl, xss, hs (i), as (i), igra, gen)

    IF (as (i) .lt. - 0.01) then
      write (*, 1022)
      1022 format (1X, 'Flaeche AS(I) = ', F10.4, ' < 0 !',/, &
                 & 1X, 'Stop in Impuls!')
      STOP
    ENDIF

    rhy = rhy + as (i) / phy

    a = a + as (i)
    hss = hss + hs (i) * as (i)

    IF (i_print.eq.1) then
      WRITE (UNIT_OUT_LOG, '(''hs '',f10.4,'' as '',f10.4)') hs (i) , as ( i)
    ENDIF

    i_print = 0

  END DO polygonzuege



!JK   ELSE ZU (ah.gt.0.01)
ELSE

  a = 0.0

!JK   ENDIF ZU (ah.gt.0.01)
ENDIF                                                                           




IF (a.gt.0.) then
  hss = hss / a
ELSE
  PRINT * , 'durchflossener querschnitt = 0!'
  hss = 0.
  ifehl2 = 1
  !JK  ZU FEHLER-, DATENUEBERGABE
  GOTO 9999
ENDIF


hss = hr - hss                                                 

!     ************************************************************      
!     --> 2. linie 1 differenz linie 2  --> pfeiler und widerlager      
!     ************************************************************      
!     koordinaten xr1(mpts,max),yr1(mpts,max)                           
!     anzahl der polygonzuege --> kr1                                   
!     anzahl der pkte je polygonzug mr1(1...kr1)                        
                                                                        
      IF (kr1.eq.0) then 
        apg = 0. 
        hp = 0. 
                                                                        
      ELSE 
        DO 73 i = 1, kr1 
          DO ii = 1, mr1 (i)
            xl (ii) = xr1 (ii, i) 
            hl (ii) = hr1 (ii, i) 
          END DO
                                                                        
          npl = mr1 (i) 

          IF (npl.lt.3) goto 73 

          IF (igra.ne.0) then 
            PRINT * , 'pfeiler- und widerlagerquerschnitt : ' 
          ENDIF 
                                                                        
          CALL schwpkt (maxkla, npl, xl, hl, xss, hps (i), ap (i), igra, gen)
                                                                        
          IF (ap (i) .lt. - 0.01) then 
            PRINT * , 'fehler: flaeche ap(i) < 0!' 
            PRINT * , 'stop in impuls' 
            STOP 
          ENDIF 
                                                                        
          apg = apg + ap (i) 
          hp = hp + hps (i) * ap (i) 
   73   END DO 
                                                                        
                                                                        
        IF (apg.gt.0.) then 
          hp = hp / apg 
        ELSE 
          hp = 0.0 
        ENDIF 
                                                                        
        hp = hr - hp 
      ENDIF 
                                                                        
                                                                        



      !----------------------------------------------------------------------------
      ! BEREICH Nr. 1: von isl bis il
      ! -----------------------------
      ! Linie 1 --> gelaende(isl-il)-hr
      ! Linie 2 --> hok(i),hmax(ges)
      ! Schnittmenge --> aue1
      !----------------------------------------------------------------------------

      write (UNIT_OUT_LOG, 5001) isl, il, x1n(isl), x1n(il)
      5001 format (1X, 'Bereich Nr. 1 von ISL = ', I3, ', bis IL = ', I3, /, &
                 & 1X, 'X(ISL) = ', F15.5, '  X(IL) = ', F15.5, /, &
                 & 1X, '-> Schnittmenge AUE1')

      !JK  START IF-SCHLEIFE BEREICH 1*************************************
      IF (isl.lt.il .and. iflli.eq.0 .and. (x1n (il) - x1n (isl) ) .gt.0.005) then
                                                                        
        ! Zusammensetzen der linien (gegen den Uhrzeigersinn)

        ! Bereich 1, Linie 1:
        ! -------------------
        CALL linie (hr, x1n, h1n, nknotn, isl, il, npl1, xl, hl)
                                                                        
        ! Linie schliessen
        npl1 = npl1 + 1 
        xl (npl1) = xl (1) 
        hl (npl1) = hl (1) 
                                                                        
        DO i = 1, npl1
          xko (i, 1) = xl (i) 
          hko (i, 1) = hl (i) 
        END DO
                                                                        
                                                                        
        ! Ueberpruefen des Flaecheninhalts:
        IF (igra.ne.0) print * , 'bereich 1: isl-il, gelaende-hr' 
                                                                        
        CALL schwpkt (maxkla, npl1, xl, hl, xx, hh, ah, igra, gen) 
        IF (ah.lt. - 0.01) then 
          PRINT * , 'negative flaeche ah ' 
          PRINT * , '--> stop in impuls' 
          STOP 
        ELSEIF (ah.le.0.01) then 
          !JK  ZU BEREICH 2: von ir bis isr
          GOTO 2023 
        ENDIF 

        write (UNIT_OUT_LOG, 5002) ah
        5002 format (1X, 'Flaeche im Bereich 1, Linie 1:  AH = ', F15.5)

        ! Bereich 1, Linie 2:
        ! -------------------
        ia1 = 1
                                                                        
        do_iii1: DO iii = 1, nok

          IF (xok (iii) .gt.x1n (isl) ) then 
            ia1 = iii 
            EXIT do_iii1
          ENDIF 

        END DO do_iii1
                                                                        
        hx = - 100000.
                                                                        
        do_iii2: DO iii = 1, ia1

          IF (hok (iii) .gt. hx) hx = hok (iii)

        END DO do_iii2
                                                                        

        hx = hx + 10. 

        CALL linie (hx, xok, hok, nok, ia1 - 1, ia, npl2, xl, hl) 
                                                                        
        ! Linie schliessen
        npl2 = npl2 + 1 
        xl (npl2) = xl (1) 
        hl (npl2) = hl (1) 
                                                                        
        DO i = 1, npl2
          xko (i, 2) = xl (i) 
          hko (i, 2) = hl (i) 
        END DO
                                                                        
        ! Ueberpruefen des flaecheninhalts:
        IF (igra.ne.0) print * , 'bereich 1: isl-il, hok-hmax' 
                                                                        
        CALL schwpkt (maxkla, npl2, xl, hl, xx, hh, ah, igra, gen) 
                                                                        
        IF (ah.lt. - 0.01) then 
          PRINT * , 'negative flaeche ah ' 
          PRINT * , '--> stop in impuls' 
          STOP 
        ELSEIF (ah.le.0.01) then 
          !JK  ZU BEREICH 2: von ir bis isr
          GOTO 2023 
        ENDIF 

        write (UNIT_OUT_LOG, 5003) ah
        5003 format (1X, 'Flaeche im Bereich 1, Linie 2:  AH = ', F15.5)


        !JK  ------ENDE LINIE 2------
                                                                        
        na1 = npl1 
        na2 = npl2 
                                                                        
        kr1 = 0 
        kr2 = 0 
        kr0 = 0 
                                                                        
        CALL prf (ik1, ik2) 
                                                                        
        !JK   SCHREIBEN IN KONTROLLFILE
        IF (ik1.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 1 abfiltriert'')')
        IF (ik2.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 2 abfiltriert'')')

        CALL alg1 (igraf, ifehl) 
                                                                        
        IF (ifehl.eq.1) then 
          !JK  SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '(''polygonzuege im algebra nicht fehlerfrei abgearbeitet'')')

          ifehl1 = 1 
          ifehl2 = 1 

        !JK         SCHREIBEN IN KONTROLLFILE                                   
        !WP ELSEIF (ifehl1.eq.3 .and. lein.eq.3) then
        ELSE
          !  linie 1 --> gelaende(isl-il)-hr
          !  linie 2 --> hok(i),hmax(ges)
          WRITE (UNIT_OUT_LOG, '('' linie 1  gelaende(isl,il)-wsp :'')')
          WRITE (UNIT_OUT_LOG, '(i5,2f15.5)') (ii, xko (ii, 1) , hko (ii, 1) , ii = 1, na1)
          WRITE (UNIT_OUT_LOG, '('' linie 2  hok(i),hmax (--> aue) '')')
          WRITE (UNIT_OUT_LOG, '(i5,2f15.5)') (ii, xko (ii, 2) , hko (ii, 2) , ii = 1, na2)
        ENDIF
                                                                        


        !JK  START IF-SCHLEIFE INNERHALB BEREICH 1-----------------------
        IF (kr0.eq.0) then 

          aue1 = 0. 
          hue1 = 0. 

        ELSE 
                                                                        
          !  linie umspeichern in xl,yl --> in schwerpunkt
                                                                        
          DO 379 i = 1, kr0 
                                                                        
            DO ii = 1, mr0 (i)
              xl (ii) = xr0 (ii, i) 
              hl (ii) = hr0 (ii, i) 
            END DO
                                                                        
            npl = mr0 (i) 

            IF (npl.lt.3) goto 379 

            IF (igra.ne.0) print * , 'ueberstroemte flaeche :' 
                                                                        
            CALL schwpkt (maxkla, npl, xl, hl, xss, hs (i), as (i), igra, gen)
                                                                        
            !  ermittlung der wehrlaenge
            xmi0 = + 10000. 
            xma0 = - 10000. 
                                                                        
            DO ii = 1, npl
              IF (xl (ii) .lt. xmi0) xmi0 = xl (ii)
              IF (xl (ii) .gt. xma0) xma0 = xl (ii)
            END DO
                                                                        
            IF (as (i) .lt. - 0.01) then 
              PRINT * , 'flaeche as(i) < 0 ! ' 
              PRINT * , 'stop in impuls !' 
              STOP 
            ENDIF 
                                                                        
            aue1 = aue1 + as (i) 
            hue1 = hue1 + hs (i) * as (i) 
                                                                        
            !JK             WAR SCHON DEAKTIVIERT,27.04.00,JK
            !**             wl1=xma0-xmi0

            wl1 = wl1 + xma0 - xmi0 

            IF (wl1.lt.0.) wl1 = 0. 
                                                                        
  379     END DO 
                                                                        

        !JK   ENDIF ZU (kr0.eq.0)
        ENDIF 
        !JK   ENDE IF-SCHLEIFE INNERHALB BEREICH 1------------------------
                                                                        

        IF (aue1.gt.0.01) hue1 = hue1 / aue1 
          !JK   SCHREIBEN IN KONTROLLFILE

      !JK   ELSE ZU BEREICH 1
      ELSE

        aue1 = 0.
        hue1 = 0.
        wl1 = 0.

      !JK     ENDIF ZU BEREICH 1
      ENDIF
      !JK     ENDE IF-SCHLEIFE BEREICH 1**************************************
                                                                        


      !      gleiches fuer bereich 2 !!!!!!!
      !      bereich 2 : von ir bis isr
      !JK    --------------------------
      !                 linie 1 --> gelaende(ir-isr)-hr
      !                 linie 2 --> hok(i),hmax(ges)
      !                 schnittmenge --> aue2

      !JK     START IF-SCHLEIFE BEREICH 2*************************************

      !JKV    ABFRAGE AUS PASCHE-VERSION                                       
      ist = 0 
                                                                        
 2023 IF (ist.eq.0.OR.ir.eq.0) then 

        aue2 = 0. 
        hue2 = 0. 
        wl2 = 0. 

      !JKV    ENDE UND IF IN ELSEIF VERAENDERT                                
      ELSEIF (isr.gt.ir.and.iflre.eq.0.and. (x1n (isr) - x1n (ir) ) .gt.0.005) then
                                                                        
        ! Zusammensetzen der linien (gegenuhrzeigersinn)
        ! Linie 1:

        CALL linie (hr, x1n, h1n, nknotn, ir, isr, npl1, xl, hl) 
                                                                        
        ! Linie schliessen
        npl1 = npl1 + 1 
        xl (npl1) = xl (1) 
        hl (npl1) = hl (1) 
                                                                        
        ! Ueberpruefen des flaecheninhalts:
        IF (igra.ne.0) print * , 'bereich 2: isr-ir, gelaende-hr' 
                                                                        
        CALL schwpkt (maxkla, npl1, xl, hl, xx, hh, ah, igra, gen) 
                                                                        
        IF (ah.lt. - 0.01) then 
          PRINT * , 'negative flaeche ah ' 
          PRINT * , '--> stop in impuls' 
          STOP 
        ELSEIF (ah.le.0.01) then 
          !JK     ZU BEREICH 3: mitte, von il bis ir
          GOTO 3023 
        ENDIF 
                                                                        
        DO i = 1, npl1
          xko (i, 1) = xl (i) 
          hko (i, 1) = hl (i) 
        END DO
                                                                        
                                                                        
        ! Linie 2:
        ! --------
        ie1 = nok
                                                                        
        do_iii_linie2: DO iii = ie, nok

          IF (xok (iii) .gt.x1n (isr) ) then 
            ie1 = iii 
            EXIT do_iii_linie2
          ENDIF 

        END DO do_iii_linie2
                                                                        
        hx = - 100000.
                                                                        
        DO iii = ie, ie1
          IF (hok (iii) .gt.hx) hx = hok (iii) 
        END DO
                                                                        
        hx = hx + 10.

        CALL linie (hx, xok, hok, nok, ie, ie1, npl2, xl, hl) 
                                                                        
        ! Linie schliessen
        npl2 = npl2 + 1 
        xl (npl2) = xl (1) 
        hl (npl2) = hl (1) 
                                                                        
        DO i = 1, npl2
          xko (i, 2) = xl (i) 
          hko (i, 2) = hl (i) 
        END DO
                                                                        
        ! Ueberpruefen des flaecheninhalts:
        IF (igra.ne.0) print * , 'bereich 2: isr-ir, hok-hmax' 
                                                                        
        CALL schwpkt (maxkla, npl2, xl, hl, xx, hh, ah, igra, gen) 
                                                                        
        IF (ah.lt. - 0.01) then 
          PRINT * , 'negative flaeche ah ' 
          PRINT * , '--> stop in impuls' 
          STOP 
        ELSEIF (ah.le.0.01) then 
          !JK             ZU BEREICH 3: mitte, von il bis ir
          GOTO 3023 
        ENDIF 
        !JK         ----ENDE LINIE 2-----
                                                                        
        na1 = npl1 
        na2 = npl2 
                                                                        
        kr1 = 0 
        kr2 = 0 
        kr0 = 0 
                                                                        
        CALL prf (ik1, ik2) 
                                                                        
        !JK     SCHREIBEN IN KONTROLLFILE
        IF (ik1.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 1 abfiltriert'')')
        IF (ik2.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 2 abfiltriert'')')

        CALL alg1 (igraf, ifehl) 
                                                                        
        IF (ifehl.eq.1) then 
          !JK    SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '(''polygonzuege im algebra nicht fehlerfrei abgearbeitet'')')

          ifehl1 = 1 
          ifehl2 = 1 
        !JK    SCHREIBEN IN KONTROLLFILE
        ELSEIF (ifehl1.eq.3) then
          WRITE (UNIT_OUT_LOG, '('' linie 1  gelaende(ir,isr)-wsp :'')')
          WRITE (UNIT_OUT_LOG, '(i5,2f10.3)') (ii, xko (ii, 1) , hko (ii, 1) , ii = 1, na1)
          WRITE (UNIT_OUT_LOG, '('' linie 2  hok(i),hmax (--> aue) '')')
          WRITE (UNIT_OUT_LOG, '(i5,2f10.3)') (ii, xko (ii, 2) , hko (ii, 2) , ii = 1, na2)
        ENDIF
                                                                        

        !JK    START IF-SCHLEIFE INNERHALB BEREICH 2-----------------------
        IF (kr0.eq.0) then 

          aue2 = 0. 
          hue2 = 0. 

        ELSE 
                                                                        
          ! Linie umspeichern in xl,yl --> in schwerpunkt
                                                                        
          DO 479 i = 1, kr0 
                                                                        
            DO ii = 1, mr0 (i)
              xl (ii) = xr0 (ii, i) 
              hl (ii) = hr0 (ii, i) 
            END DO
            npl = mr0 (i) 

            IF (npl.lt.3) goto 479 

            ! Ermittlung der wehrlaenge
            xmi0 = + 10000.
            xma0 = - 10000. 
                                                                        
            DO ii = 1, npl
              IF (xl (ii) .lt.xmi0) xmi0 = xl (ii) 
              IF (xl (ii) .gt.xma0) xma0 = xl (ii) 
            END DO
                                                                        
            IF (igra.ne.0) print * , 'bereich 2: aue' 
            CALL schwpkt (maxkla, npl, xl, hl, xss, hs (i), as (i),     &
            igra, gen)                                                  
                                                                        
            IF (as (i) .lt. - 0.01) then 
              PRINT * , 'flaeche as(i) < 0 ! ' 
              PRINT * , 'stop in impuls !' 
              STOP 
            ENDIF 
                                                                        
            !JK                WAR SCHON DEAKTIVIERT,27.04.00,JK
            !**                wl2=xma0-xmi0
            wl2 = wl2 + xma0 - xmi0

            IF (wl2.lt.0.) wl2 = 0. 
                                                                        
            aue2 = aue2 + as (i) 
            hue2 = hue2 + hs (i) * as (i) 
                                                                        
  479     END DO 


        ENDIF 
        !JK  ENDE IF-SCHLEIFE INNERHALB BEREICH 2------------------------
                                                                        



        IF (aue2.gt.0.01) hue2 = hue2 / aue2 
          !JK  SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '('' wl3 = '',f8.3,'' aue3 = '',f8.3, '' hue3 = '',f8.3)') &
            & wl2, aue2, hue2

      !JK     ELSE ZU START BEREICH 2                                         
      ELSE 

        aue2 = 0. 
        hue2 = 0. 
        wl2 = 0. 

      !JK     ENDIF ZU START BEREICH 2
      ENDIF 
      !JK     ENDE IF-SCHLEIFE BEREICH 2**************************************
                                                                        
                                                                        


      !           bereich 3 -- mitte il,ir
      !JK         ------------------------
      !           --> apl,aue3
      !           linie1 : hok(i)(1-nknot) -- hsohl
      !           linie2 : hukmax-hr (il,ir)

      !JK     START IF-SCHLEIFE BEREICH 3************************************* 

 3023 IF (hr.gt.hukmax) then 
                                                                        
        ! Linie 1:
        ! --------
                                                                        
        CALL linie (hsohl, xok, hok, nok, ia - 1, ie+1, npl1, xl, hl) 
        CALL reih_inv (maxkla, npl1, xl, hl) 
                                                                        
        ! Linie schliessen
        npl1 = npl1 + 1 
        xl (npl1) = xl (1) 
        hl (npl1) = hl (1) 
                                                                        
        DO i = 1, npl1
          xko (i, 1) = xl (i) 
          hko (i, 1) = hl (i) 
        END DO
                                                                        
        ! Ueberpruefen des flaecheninhalts:
        IF (igra.ne.0) print * , 'bereich 3: il-ir, hok-hsohl' 
                                                                        
        CALL schwpkt (maxkla, npl1, xl, hl, xx, hh, ah, igra, gen) 
                                                                        
        IF (ah.lt. - 0.01) then 
          PRINT * , 'negative flaeche ah ' 
          PRINT * , '--> stop in impuls' 
          STOP 
        ELSEIF (ah.le.0.01) then 
          !JK    ZU ZUSAMMENSETZEN DER 3 BEREICHE
          GOTO 4023 
        ENDIF 
                                                                        
        ! Linie2:
        ! -------
                                                                        
        xko (1, 2) = x1n (il) 
        hko (1, 2) = hukmax 
        xko (2, 2) = x1n (ir) 
        hko (2, 2) = hukmax 
        xko (3, 2) = x1n (ir) 
        hko (3, 2) = hr 
        xko (4, 2) = x1n (il) 
        hko (4, 2) = hr 
        xko (5, 2) = x1n (il) 
        hko (5, 2) = hukmax 
        npl2 = 5 
                                                                        
        DO ii = 1, npl2
          xl (ii) = xko (ii, 2) 
          hl (ii) = hko (ii, 2) 
        END DO
                                                                        
        ! Ueberpruefen des Flaecheninhalts:
        IF (igra.ne.0) print * , 'bereich 3: il-ir, hukmax-hr' 
        CALL schwpkt (maxkla, npl2, xl, hl, xx, hh, ah, igra, gen) 
                                                                        
        IF (ah.lt. - 0.01) then 
          PRINT * , 'negative flaeche ah ' 
          PRINT * , '--> stop in impuls' 
          STOP 
        ELSEIF (ah.le.0.01) then 
          !JK  ZU ZUSAMMENSETZEN DER 3 BEREICHE
          GOTO 4023 
        ENDIF 
        !JK         -----ENDE LINIE 2------
                                                                        
        na1 = npl1 
        na2 = npl2 
                                                                        
        kr1 = 0 
        kr2 = 0 
        kr0 = 0 
                                                                        
        CALL prf (ik1, ik2) 
                                                                        
        !JK   SCHREIBEN IN KONTROLLFILE
        IF (ik1.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 1 abfiltriert'')')
        IF (ik2.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 2 abfiltriert'')')

        CALL alg1 (igraf, ifehl) 
                                                                        
        IF (ifehl.eq.1) then 

          !JK   SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '(''polygonzuege im algebra nicht fehlerfrei abgearbeitet'')')

          ifehl1 = 1 
          ifehl2 = 1 
                                                                        
        !JK   SCHREIBEN IN KONTROLLFILE
        ELSEIF (ifehl1.eq.3) then
          WRITE (UNIT_OUT_LOG, '('' linie 1  hok(il,ir)-hsohl:'')')
          WRITE (UNIT_OUT_LOG, '(i5,2f10.3)') (ii, xko (ii, 1) , hko (ii, 1) , ii = 1, na1)
          WRITE (UNIT_OUT_LOG, '('' linie 2  hukmax-wsp (--> aue-mitte,apl) '')')
          WRITE (UNIT_OUT_LOG, '(i5,2f10.3)') (ii, xko (ii, 2) , hko (ii, 2) , ii = 1, na2)
        ENDIF
                                                                        


        !JK     START IF-SCHLEIFE INNERHALB BEREICH 3-----------------------
        IF (kr0.eq.0) then 
          apl = 0. 
          hpl = 0. 
        ELSE 
                                                                        
          ! Linie umspeichern in xl,yl --> in schwerpunkt
                                                                        
          DO 579 i = 1, kr0 

            DO ii = 1, mr0 (i)
              xl (ii) = xr0 (ii, i) 
              hl (ii) = hr0 (ii, i) 
            END DO
                                                                        
            npl = mr0 (i) 

            IF (npl.lt.3) goto 579 

            !JK   UEBERPRUEFEN FLAECHENINHALT
            IF (igra.ne.0) print * , 'angestroemte platte:' 
            CALL schwpkt (maxkla, npl, xl, hl, xss, hs (i), as (i),     &
            igra, gen)                                                  
                                                                        
            IF (as (i) .lt. - 0.01) then 
              PRINT * , 'flaeche as(i) < 0 ! ' 
              PRINT * , 'stop in impuls !' 
              STOP 
            ENDIF 
                                                                        
            apl = apl + as (i) 
            hpl = hpl + hs (i) * as (i) 
  579     END DO 
                                                                        
          IF (apl.gt.0.01) hpl = hpl / apl 
                                                                        
          DO 679 i = 1, kr2 

            DO ii = 1, mr2 (i)
              xl (ii) = xr2 (ii, i) 
              hl (ii) = hr2 (ii, i) 
            END DO
                                                                        
            npl = mr2 (i) 

            IF (npl.lt.3) goto 679 

            ! Ermittlung der Wehrlaenge
                                                                        
            xmi0 = + 10000. 
            xma0 = - 10000. 
                                                                        
            DO ii = 1, npl
              IF (xl (ii) .lt.xmi0) xmi0 = xl (ii) 
              IF (xl (ii) .gt.xma0) xma0 = xl (ii) 
            END DO
                                                                        
            !JK    UEBERRUEFEN FLAECHENINHALT
            IF (igra.ne.0) print  * , 'ueberstroemte flaeche fluszschl.:'

            CALL schwpkt (maxkla, npl, xl, hl, xss, hs (i), as (i), igra, gen)
                                                                        
            IF (as (i) .lt. - 0.01) then 
              PRINT * , 'flaeche as(i) < 0 ! ' 
              PRINT * , 'stop in impuls !' 
              STOP 
            ENDIF 
                                                                        
            wl3 = xma0 - xmi0 
                                                                        
            IF (wl3.lt.0.) wl3 = 0. 
                                                                        
            aue3 = aue3 + as (i) 
            hue3 = hue3 + hs (i) * as (i) 
                                                                        
  679     END DO 

        !JK   ENDIF ZU (kr0.eq.0)
        ENDIF 
        !JK   ENDE IF-SCHLEIFE INNERHALB BEREICH 3------------------------
                                                                        
                                                                        
        IF (aue3.gt.0.01) then 
                                                                        
          hue3 = hue3 / aue3 
                                                                        
          WRITE (UNIT_OUT_LOG, '('' wl2 = '',f8.3,'' aue2 = '',f8.3, '' hue2 = '',f8.3)') &
             & wl3, aue3, hue3

        ELSE 

          aue3 = 0. 
          hue3 = 0. 
          wl3 = 0. 

        ENDIF 

      !JK  ENDIF ZU (hr.gt.hukmax)
      ENDIF
      !JK  ENDE IF-SCHLEIFE BEREICHE 3*************************************
                                                                        

      !JK   **********************************************                    
      !     gesamtflaechen:
      !JK   **********************************************
                                                                        
 4023 wl = wl1 + wl2 + wl3 
      aue = aue1 + aue2 + aue3 
      hue = hue1 * aue1 + hue2 * aue2 + hue3 * aue3 
                                                                        
      IF (aue.gt.0.01) hue = hue / aue 
                                                                        
      hue = hr - hue 
      hpl = hr - hpl 
                                                                        
      ! ************************************************
      ! durchstroemte breite
      ! ************************************************
                                                                        
      bbrutto = a / (hr - hsohl) 
      bnetto = apg / (hr - hsohl) 
      bnetto = bbrutto - bnetto 
      ad = a 
                                                                        
      !    berechnung alpha fuer pfeilerstau nach yarnell
      !    alpha=av/abrutto
      !    av=bv*tv          mit tv- mittlere fliesstiefe im verbauten bereich
      !    abrutto=bbrutto * t  mit t - mittlere fliesstiefe im unverbauten b.
      !    annahme: tv=t --> alpha = b(verbaut)/bbrutto
      !                      (nicht unbedingt richtig !!, siehe beschreibung)  

      IF (abs (bbrutto) .gt.1.e-06) then 
        alpha = (bbrutto - bnetto) / bbrutto 
      ELSE 
        alpha = 0. 
      ENDIF 
                                                                        
 9999 IF (x1n (isohln) .eq.x1 (isohl) .and.h1n (isohln) .eq.h1 (isohl) ) then
        CONTINUE 
      ELSE 
        PRINT * , 'fehler bei der einordnung der schnittpunkte in proflinie'
        PRINT * , 'pn(isohln) /= p(isohl) !!!!' 
        PRINT * , 'neu: isohln=', isohln 
        PRINT * , '     xn    =', x1n (isohln)
        PRINT * , '     hn    =', h1n (isohln)
        PRINT * , 'alt: isohl =', isohl 
        PRINT * , '     x     =', x1 (isohl)
        PRINT * , '     h     =', h1 (isohl)
      ENDIF 
                                                                        

      write (UNIT_OUT_LOG, 2100) wl, wl1, wl2, wl3, aue, aue1, aue2, aue3, hue, hue1, hue2, hue3, rhy
      2100 format (1X, 'Flaechen, in GEOMET berechnet    ', /, &
                 & 1X, '-------------------------------- ', /, &
                 & 1X, 'WL (Wehrlaenge gesamt):          ', F15.5, /, &
                 & 1X, 'WL1:                             ', F15.5, /, &
                 & 1X, 'WL2:                             ', F15.5, /, &
                 & 1X, 'WL3:                             ', F15.5, /, &
                 & 1X, 'AUE (Ueberfallflaeche gesamt):   ', F15.5, /, &
                 & 1X, 'AUE1:                            ', F15.5, /, &
                 & 1X, 'AUE2:                            ', F15.5, /, &
                 & 1X, 'AUE3:                            ', F15.5, /, &
                 & 1X, 'HUE (Ueberstroemflaeche gesamt): ', F15.5, /, &
                 & 1X, 'HUE1:                            ', F15.5, /, &
                 & 1X, 'HUE2:                            ', F15.5, /, &
                 & 1X, 'HUE3:                            ', F15.5, /, &
                 & 1X, 'RHY (Hydraulischer Radius):      ', F15.5, /, &
                 & 1X, '-------------------------------- ',        /, &
                 & 1X, 'Ende GEOMET                      ',        //)

      RETURN 
                                                                        


END SUBROUTINE geomet
