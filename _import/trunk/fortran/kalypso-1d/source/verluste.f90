!     Last change:  WP    2 Jun 2006   11:26 pm
!--------------------------------------------------------------------------
! This code, verluste.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - verluste
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

SUBROUTINE verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,   &
         & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, itere1)

!***********************************************************************
!**                                                                     
!**   Subroutine Verluste                                               
!**                                                                     
!**   geschrieben: P. Koch, Maerz 1990                                  
!**                                                                     
!**   Programmbeschreibung:                                             
!**                                                                     
!**   Diese Subroutine ermittelt die Verluste an einem Profil.          
!**                                                                     
!**
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN
!**   ---------------------------------------------------               
!**                                                                     
!**   betta   --      Ungleichförmigkeitsbeiwert                        
!**   betta1  --      Ungleichförmigkeitsbeiwert                        
!**   dx      --      Schrittweite
!**   f       --      durchströmte Fläche des Teilabschnittes           
!**   falt    --      gesamte durchströmte Fläche (alt)                 
!**   fges    --      gesamte durchströmte Fläche                       
!**   fges1   --      gesamte durchströmte Fläche                       
!**   hborda  --      Einengungsverluste                                
!**   heins   --      Einlaufverluste                                   
!**   horts   --      Auslaufverluste                                   
!**   hr      --      Wasserspiegelhöhe                                 
!**   hrst    --      Reibungsverlust                                   
!**   ht      --      Wasserspiegelhöhe                                 
!**   hv      --      Geschwindigkeitsverlust                           
!**   hv1     --      Geschwindigkeitsverlust                           
!**   hvst    --      Geschwindigkeitsverlust                           
!**   ifehlg  --      Fehlervariable                                    
!**   ifg     --      Art der Widerstandsbeiwertberechnung nach         
!**                   Darcy-Weisbach oder Gauckler-Manning-Strickler    
!**   isstat  --      Sohlgefälle                                       
!**   nprof   --      Anzahl der Punkte in einem Profil
!**   nstat   --      Anzahl der Stationen                              
!**   psiein  --      Verlustbeiwert                                    
!**   psiort  --      Verlustbeiwert                                    
!**   q       --      Abfluß                                            
!**   q1      --      Abfluß                                            
!**   qt      --      Teilabfluß                                        
!**   rg      --      Reibungsverlust                                   
!**   rg1     --      Reibungsverlust
!**   rgm     --      Reibungsverlust                                   
!**   rk      --      Rauheit des Teilabschnitte                        
!**   sohle   --      Sohlgefälle                                       
!**   u       --      benetzter Umfang des Teilabschnittes              
!**   v       --      mittlere Fließgeschwindigkeit des Teilabschnittes 
!**   vges    --      mittlere Fließgeschwindigkeit                     
!**   vm      --      mittlere Fließgeschwindigkeit                     
!**   vo      --      Fließgeschwindigkeit im Oberwasser                
!**   vu      --      Fließgeschwindigkeit im Unterwasser               
!**                                                                     
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   qks (iprof,sohle,q,itere1,hr,hv,nknot)
!**   qkst (indmax,hr,hv,vm,q,sohle,ife)                                
!**   uf (hr,hi,xi,s,indmax,nfli,nfre)                                  
!JK   eb2ks                                                             
!JK   eb2kst                                                            
!**                                                                     
!***********************************************************************
                                                                        
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGEN
! ------------------------------------------------------------------

USE DIM_VARIABLEN
USE KONSTANTEN
USE MOD_INI

! Calling variables -----------------------------------------------------------
REAL, INTENT(IN) 	:: str          ! Abstand (Strecke) zwischen zwei Profilen
REAL, INTENT(INOUT) 	:: q            ! Abfluss, wird uebergeben an eb2ks oder beechnet in qks!
REAL, INTENT(IN)	:: q1           ! Abfluss
INTEGER, INTENT(IN) 	:: nprof	! Entspricht NPROF, Anzahl bzw. aktive Nummer des Profil,
                                        ! wird von WSBBER ueber NORMBER durchgereicht.
REAL, INTENT(INOUT) 	:: hr           ! Wasserspiegelhoehe
REAL, INTENT(INOUT) 	:: hv           ! Geschwindigkeitsverlusthoehe, wird veraendert/berechnet
REAL, INTENT(INOUT) 	:: rg           ! Reibungsverlusthoehe, wird in eb2ks bestimmt.
REAL, INTENT(INOUT) 	:: hvst         ! Geschwindigkeitsverlust
REAL, INTENT(INOUT) 	:: hrst         ! Geschwindigkeitsverlust

INTEGER, INTENT(INOUT) 	:: indmax       ! Anzahl der Rauhigkeitszonen in einem Profil
REAL, INTENT(INOUT) 	:: psiein       ! Verlustbeiwert
REAL, INTENT(INOUT) 	:: psiort       ! Örtlicher Verlust
REAL, INTENT(INOUT) 	:: xi (maxkla)  ! Abstand der einzelnen Profilpunkte (werden in VERLUSTE -> UF verwendet)
REAL, INTENT(INOUT) 	:: hi (maxkla)  ! Höhen der einzelnen Profilpunkte (werden in VERLUSTE -> UF verwendet)
REAL, INTENT(INOUT) 	:: s (maxkla)   ! Distanz der Profilpunkte (werden in VERLUSTE -> UF verwendet)
INTEGER, INTENT(IN) 	:: istat        ! Flag zur Kennzeichnung der Strömung ( = 1 gleichförmig, = 0 ungleichförmig)
REAL, INTENT(INOUT) 	:: froud        ! Froudzahl, wird in VERLUSTE -> ERFROUD bestimmt.
INTEGER, INTENT(OUT) 	:: ifehlg       ! Fehlerkennung, normalerweise = 0
INTEGER 		:: itere1       ! Iterationsvariable


! COMMON-Block /ALT/ ---------------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
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


! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
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


! COMMON-Block /VORT/ ---------------------------------------------------------
REAL 		:: hborda, heins, horts
COMMON / vort / hborda, heins, horts
! -----------------------------------------------------------------------------


! Local variables -------------------------------------------------------------
REAL :: GET_BORDA
REAL :: betta

! istat=0      --> stationaer ungleichfoermig
! istat=1      --> stationaer gleichfoermig
                                                                        
                                                                        
! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
heins = 0.
horts = 0.
hborda = 0.
dx = 0.05
falt = 0.
                                                                        

101 CONTINUE


! ------------------------------------------------------------------
! BERECHNUNG DER GEOMETRIE
CALL uf (hr, hi, xi, s, indmax, nfli, nfre)
! ------------------------------------------------------------------
                                                                        


IF (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

  ! -----------------------------------------------------------------
  ! STATIONAER UNGLEICHFOERMIGER ABFLUSS (SPIEGELLINIENBERECHNUNG)
  ! -----------------------------------------------------------------
  IF (fges .lt. 1.e-02) then

    hr = hr + dx
    falt = fges
    GOTO 101

  ELSEIF ( (q/fges) .gt. 20) then

    IF (abs (falt - fges) .gt. 1.e-06) then
      hr = hr + dx
      falt = fges
      ifehlg = 0
      GOTO 101
    ELSE
      ifehlg = 1
    ENDIF

  ENDIF                                     
                                                                        
                                                                        
  ! ------------------------------------------------------------------
  ! Berechnung des wirksamen Geschwindikeitsverlustes (hv) und Rei-
  ! bungsverlustes des Profils (rg)
  ! ------------------------------------------------------------------
                                                                        
  !JK  BERECHNUNG NACH DARCY-WEISBACH
  IF (FLIESSGESETZ/='MANNING_STR') then
    nstat = nprof
    CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hr, nknot)
  !JK  BERECHNUNG NACH GMS
  ELSE
    ! vorbelegung
    hv = 0.
    !                       /* output
    rg = 0.
    !                       /* output
    CALL eb2kst (indmax, hr, hv, rg, q)
  ENDIF

  ! Geschwindigkeitsverlust:
  IF (nprof.eq.1.or.istat.eq.1) then
    hvst = 0.
  ELSE
    hvst = hv1 - hv

    IF (hv1.lt.hv) then

      ! beruecksichtigung aufweitungsverlust
      ! betta(dvwk)=2./3.
      ! betta(bjoernsen)=0.5
      ! betta(DFG)=2./(1.+Ai/Ai-1)

      IF (fges.lt. (fges1 - 2. * str / 7.) ) then
        ! Aufweitung > 1:7 --> Borda'scher Stossdruck
        ht = MIN (hr, hmax) - hmin
        vo = sqrt (hv * 2 * 9.81)
        vu = sqrt (hv1 * 2 * 9.81)
        hborda = GET_BORDA (vo, vu, ht)

      ELSE

        if (VERZOEGERUNGSVERLUST == 'DVWK') then
          betta = 2. / 3.
        else if (VERZOEGERUNGSVERLUST == 'BJOE') then
          betta = 0.5
        else if (VERZOEGERUNGSVERLUST == 'DFG ') then
          betta = 2. / (1. + fges1 / fges)
        end if                                          

        hvst = betta * hvst

        !IF (betta.lt.1.e-04) then
        !  betta1 = 2. / (1. + fges1 / fges)
        !  hvst = betta1 * hvst
        !ELSE
        !  hvst = betta * hvst
        !ENDIF

      ENDIF

    ENDIF

  ENDIF

  ! -------------------------------------------------------------
  ! Reibungsverlust hrst (rg - reibungsgefaelle)                              
  ! -------------------------------------------------------------

  ifehlg = 0

  IF (fges.le.1.e-02.and.rg.le.1.e-06.and.hborda.gt.1.e+05) then
    ifehlg = 1
    rgm = 10000

  ELSEIF (nprof.eq.1.or.istat.eq.1) then

    IF (rg.gt.1.e-06) then
      rgm = q / rg * q / rg
    ELSE
      rgm = 0.001
    ENDIF

  ELSEIF (REIBUNGSVERLUST == 'GEOMET') THEN

    IF (rg1.gt.1.e-06.and.rg.gt.0) then
      wurzrg = (q + q1) / (rg1 + rg)
      rgm = wurzrg**2
    ELSEIF (rg1.gt.1.e-06) then
      wurzrg = q1 / rg1
      rgm = wurzrg**2
    ELSEIF (rg.gt.1.e-06) then
      wurzrg = q / rg
      rgm = wurzrg**2
    ELSE
      wurzrg = (q + q1) / 0.001
      rgm = wurzrg**2
    ENDIF

  ELSE

    IF (rg1.gt.1.e-06.and.rg.gt.0) then
      rgm = 0.5 * ( (q / rg) **2 + (q1 / rg1) **2)
    ELSEIF (rg1.gt.1.e-06) then
      rgm = (q1 / rg1) **2
    ELSEIF (rg.gt.1.e-06) then
      rgm = (q / rg) **2
    ELSE
      rgm = q**2 / 0.001
    ENDIF

  ENDIF

  hrst = rgm * str                                                    
                                                                        
  ! --------------------------------------------------------------
  ! Ermittlung lokaler verluste
  ! --------------------------------------------------------------
  horts = psiort * hv

  IF (nprof.ne.1.and.istat.ne.1) then
    vu = sqrt (hv1 * 2 * 9.81)
    vo = sqrt (hv * 2 * 9.81)
    heins = psiein * (vu - vo) * (vu - vo) / (2 * 9.81)
  ENDIF                                                  

  ! --------------------------------------------------------------
  ! Ermittlung der froudzahl:
  ! --------------------------------------------------------------
  IF (idruck.ne.1) then

    CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froud)

  ELSE 

    froud = 999.

  ENDIF
                                                                        

ELSE

  ! -----------------------------------------------------------------
  ! STATIONAER GLEICHFOERMIGER ABFLUSS (BORDVOLL)
  ! -----------------------------------------------------------------

  sohle = isstat (nprof)

  IF (sohle.ge.1.e-05) then

    IF (FLIESSGESETZ/='MANNING_STR') then

      CALL qks (iprof, sohle, q, itere1, hr, hv, nknot)

      IF (fges.gt.1.e-05) then
        vm = q / fges
      ELSE
        vm = 0.
      ENDIF

    ELSE

      CALL qkst (indmax, hr, hv, vm, q, sohle, ife)
      vm = vges

    ENDIF

  ELSE

    DO i1 = 1, indmax

      qt (i1) = 0.
      v (i1) = 0.
      f (i1) = 0.
      rk (i1) = 0.
      u (i1) = 0.

    END DO

    q = 0.
    vm = 0.

  ENDIF

  hvst = vm

ENDIF

END SUBROUTINE verluste                                         
