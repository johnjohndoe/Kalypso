!     Last change:  WP    1 Aug 2006   11:15 am
!--------------------------------------------------------------------------
! This code, speicher.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - speicher
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


!***********************************************************************
SUBROUTINE speicher (l, hr, hv, hvst, hrst, q, station, indmax, ikenn)
!
!   SUBROUTINE SPEICHER ZU KALYPSO-1D
!
!   Funktion: Speicherung der Ergebnisse des Profils  Nr. l
!
!**
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   boli    --      linke Böschungshöhe                               
!**   bolip   --      linke Böschungshöhe                               
!**   bore    --      rechte Böschungshöhe
!**   borep   --      rechte Böschungshöhe                              
!**   brg     --      Spiegelbreite                                     
!**   brges   --      Spiegelbreite                                     
!**   f       --      Teilfläche                                        
!**   fges    --      gesamte durchströmte Fläche                       
!**   fgesp   --      gesamte durchströmte Fläche                       
!**   fp      --      Teilfläche                                        
!**   froud   --      Froud-Zahl                                        
!**   hborda  --      Einengungsverlust                                 
!**   hbors   --      Einengungsverlust                                 
!**   hbv     --      Wasserspiegelhöhe bei Bordvoll                    
!**   hein    --      Einlaufverlust                                    
!**   heins   --      Einlaufverlust                                    
!**   hen     --      Energiehöhe                                       
!**   hmin    --      niedrigste Sohlhöhe                               
!**   hming   --      niedrigste Geländehöhe                            
!**   hmingp  --      niedrigste Geländehöhe                            
!**   hort    --      Auslaufverlust                                    
!**   horts   --      Auslaufverlust                                    
!**   hrs     --      Reibungsverlusthöhe
!**   hs      --      Gesamtverlust
!**   hv      --      Geschwindigkeitsverlusthöhe                       
!**   hvs     --      Geschwindigkeitsverlusthöhe                       
!**   hwsp    --      Wasserspiegelhöhe
!**   isstat  --      Stationsgefälle
!**   qbv     --      Bordvollabfluß
!**   qs      --      Abfluß                                            
!**   qt      --      Teilabfluß                                        
!**   qtp     --      Teilabfluß                                        
!**   rk      --      Rauheit                                           
!**   rkp     --      Rauheit                                           
!**   sohlp   --      niedrigste Sohlhöhe                               
!**   stat    --      Station                                           
!**   str     --      Strecke zwischen zwei Stationen                   
!**   tm      --      Verhältnis Fläche zu Breite                       
!**   u       --      benetzter Teilumfang                              
!**   up      --      benetzter Teilumfang                              
!**   v       --      Teilgeschwindigkeit                               
!**   vm      --      mittlere Geschwindigkeit                          
!**   vmp     --      mittlere Geschwindigkeit                          
!**   vp      --      Teilgeschwindigkeit                               
!**   wsp     --      Wasserspiegelhöhe                                 
!**                                                                     
!***********************************************************************


!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE MOD_INI

implicit none

! Calling variables
INTEGER, INTENT(IN) 	:: l		! = NPROF, Nummer des aktuellen Profils
REAL, INTENT(IN) 	:: hr           ! Wasserspiegelhoehe
REAL, INTENT(IN)        :: hv           ! Geschwindigkeitsverlusthoehe
REAL, INTENT(IN)        :: hvst         ! Geschwindigkeitsverlusthoehe
REAL, INTENT(IN)        :: hrst         ! Reibungsverlusthoehe
REAL, INTENT(IN)        :: q            ! Gesamtabfluss
REAL, INTENT(IN)        :: station      ! Station (km)
INTEGER, INTENT(IN)     :: indmax       ! Anzahl der Rahigkeitsabschnitte
INTEGER, INTENT(IN)     :: ikenn	! Kennung fuer die Art der Berechnung


! COMMON-Block /BV/ ----------------------------------------------------------------
CHARACTER(LEN=1):: idr1, idr2
INTEGER         :: nprof, isch
REAL            :: qvar
COMMON / bv / idr1, idr2, nprof, qvar, isch
! ----------------------------------------------------------------------------------


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


! COMMON-Block /ERG_KM/ -------------------------------------------------------
INTEGER 	:: ind (maxger), indf (maxger)
REAL 		:: brp1 (maxger, maxkla), qtp1 (maxger, maxkla), fp1 (maxger, maxkla)
COMMON / erg_km / ind, indf, brp1, qtp1, fp1
! -----------------------------------------------------------------------------


! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /K_M/ ----------------------------------------------------------
CHARACTER(LEN=1) :: km          ! Kalinin - Miljukov - Parameter ( = 'j' oder = 'n')
INTEGER 	 :: i_km        ! Zaehler fuer stationaer-gleichfoermige Berechnung
COMMON / k_m / km, i_km
! Common-bloecke fuer Kalinin - Miljukov - Parameter, aufgerufen in:
! - qbordv
! - wspber
! - speicher
! - wsp
! -----------------------------------------------------------------------------


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! COMMON-Block /P3/ -----------------------------------------------------------
INTEGER 	:: isohl, iming
REAL            :: hming
COMMON / p3 / isohl, hming, iming
! -----------------------------------------------------------------------------


! COMMON-Block /PROF/ ---------------------------------------------------------
REAL 		:: hwsp (100, maxger)
COMMON / prof / hwsp
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /QERG/ ---------------------------------------------------------
INTEGER :: ikennung (maxger, maxger)
COMMON / qerg / ikennung
! -----------------------------------------------------------------------------


! COMMON-Block /VORT/ ---------------------------------------------------------
REAL 		:: hborda, heins, horts
COMMON / vort / hborda, heins, horts
! -----------------------------------------------------------------------------


! Local variables
INTEGER 	:: i
REAL 		:: vm, tm, froud, str


! Wasserspiegel wsp(m+nn):
wsp (l) = hr

! Durchfluss q(m**3/s):
qs (l) = q

! Insgesamt durchflossene Flaeche (m**2)
fgesp (l) = fges

! Gesamt durchstroemte Breite
brg (l) = brges

! Mittleres v:
vm = q / fges
vmp (l) = vm

! Froudzahl
tm = fges / brges
froud = (vm * vm) / (tm * g)
froudp (l) = froud

! Geschwindigkeitsverlust:
hvs (l) = hvst

! Reibungsverlust:
hrs (l) = hrst

! Borda'scher Verlust:
hbors (l) = hborda

! Einlaufverlust:
hein (l) = heins

! Auslaufverlust:
hort (l) = horts

! Gesamtverlust:
hs (l) = hvst + hrst + hborda + horts + heins

! Energiehoehe (m+nn):
! hen(l) = hr+(vm*vm/2./9.81)
hen (l) = hr + hv

! mittlere Rauhigkeit: akges
k_kp (l) = akges

! Hinweis auf grenztiefe:
igrenz (l) = ikenn

! Abspeicherung der werte fuer die durch unterschiedliche
! Rauhigkeiten oder durch trennflaechen gekennzeichneten
! Teilflaechen:

DO i = 1, indmax

  fp (l, i) = f (i)
  up (l, i) = u (i)
  vp (l, i) = v (i)
  qtp (l, i) = qt (i)

  !write (*,*) 'In SPEICHER. RK(i) = ', rk(i)

  rkp (l, i)  = rk (i)
  fbwp (l, i) = formbeiwert(i)
  brp (l, i)  = br(i)

  IF (km.eq.'j') then
    brp1 (l, i) = br (i)
    fp1 (l, i) = f (i)
    qtp1 (l, i) = qt (i)
  ENDIF

END DO

! Speicherung fuer Laengsprofile
! -------------------------------------------------------------------

! immer Speicherung : stat,wsp,q,boli,bore,hmin

stat (l) = station	  ! Stationierung fuer Profil l
bolip (l) = boli          ! linke boeschungskante
borep (l) = bore          ! rechte boeschungskante
sohlp (l) = hmin          ! niedrigster Sohlpunkt
hmingp (l) = hming        ! niedrigster Gelaendepunkt

IF (l.gt.1) then
  str = stat (l) - stat (l - 1)
  IF (str.gt.0.0011) then
    isstat (l - 1) = (sohlp (l) - sohlp (l - 1) ) / (str * 1000.)
  ELSE
    isstat (l - 1) = 0
  ENDIF
ENDIF

! Soll-wasserspiegelhoehe bei der bordvoll-berechnung
hbv (l) = min (boli, bore)
! Fuer K-M Parameter
IF (km.eq.'j') then
  ind (l) = indmax
  indf (l) = indfl
ENDIF



IF (BERECHNUNGSMODUS == 'BF_NON_UNI') then

  ! Speicherung fuer bordvoll
  ! -------------------------------------------------------------------

  hwsp (isch, l) = hr
  ! qbv(isch,l)=q
  ikennung (isch, l) = ikenn

  !  ikenn=0 -->     stationaer ungleichfoermig
  !  ikenn=1 -->     grenztiefe
  !  ikenn=2 -->     grenztiefe wegen konvergenzproblemen
  !  ikenn=3 -->     stationaer gleichfoermig
  !  ikenn=4 -->     stationaer gleichfoermig,schiessend in Rohrst
  !                  Grenztiefe bis Vollfuellung nicht erreicht.
  !                  Daher Fortsetzung mit Wsp=Rohrscheitelhoehe
  !  ikenn=5 -->     wie 4., jedoch ohne Konvergenz in stationaer
  !  ikenn>10-->     keine Konvergenz in Grenztiefenberechnung
  !  ikenn>100  -->  keine Konvergenz in Newton-Berechnung
  !  ikenn>1000 -->  keine Konvergenz in Pegasus-Berechnung
  !  ikenn>10000 --> keine Konvergenz in Brueckenberechnung

ENDIF                                                                         

END SUBROUTINE speicher
