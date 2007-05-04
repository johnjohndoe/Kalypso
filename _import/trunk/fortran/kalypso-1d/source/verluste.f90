!     Last change:  MD   10 Apr 2007    6:37 pm
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
!**   f_alt   --      gesamte durchströmte Fläche (alt)
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

implicit none

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
INTEGER :: i
INTEGER :: ife
INTEGER :: nfre, nfli
INTEGER :: nstat                ! Anzahl der Stationen

REAL :: GET_BORDA               ! Funktion zur Bestimmung des Aufweitungsverlustes nach Borda-Carnot
REAL :: betta                   ! Ungleichfoermigkeitsbeiwert
REAL :: dx
REAL :: f_alt
REAL :: ht
REAL :: vo, vu, vm
REAL :: rgm
REAL :: wurzrg
REAL :: sohle
REAL :: hv_ow           ! Verlusthoehe im Oberwasser (aktuell zu berechnendes Profil)
REAL :: hv_uw           ! Verlusthoehe im Unterwasser (altes Profil)
REAL :: fges_ow         ! Gesamter Fließquerschnitt im Oberwasser (aktuell zu berechnendes Profil)
REAL :: fges_uw         ! Gesamter Fließquerschnitt im Unterwasser (altes Profil)
REAL :: bges_ow         ! Gesamte Fließbreite im Oberwasser (aktuell zu berechnendes Profil)
REAL :: bges_uw         ! Gesamte Fließbreite im Unterwasser (altes Profil)

REAL :: alpha_RAD
REAL :: alpha_GRAD

REAL :: zeta            ! Verlustbeiwert bei seitlicher Verzeihung (Aufweitung oder Einschnuerung)

! istat=0      --> stationaer ungleichfoermig
! istat=1      --> stationaer gleichfoermig
                                                                        
                                                                        
!write (*,*) 'In VERLUSTE. Start Subroutine.'
!do i = 1, 3
!  write (*,*) 'rk(',i,') = ', rk(i), '  f(',i,') = ', f(i)
!end do

! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
heins = 0.
horts = 0.
hborda = 0.
dx = 0.05
f_alt = 0.
                                                                        

101 CONTINUE


! ------------------------------------------------------------------
! BERECHNUNG DER GEOMETRIE
CALL uf (hr, hi, xi, s, indmax, nfli, nfre)
! ------------------------------------------------------------------
                                                                        
!write (*,*) 'In VERLUSTE. Nach call UF. hr = ', hr
!do i = 1, 3
!  write (*,*) 'rk(',i,') = ', rk(i), '  f(',i,') = ', f(i)
!end do


IF (BERECHNUNGSMODUS /= 'BF_UNIFORM') then

  ! -----------------------------------------------------------------
  ! STATIONAER UNGLEICHFOERMIGER ABFLUSS (SPIEGELLINIENBERECHNUNG)
  ! -----------------------------------------------------------------

  !write (*,*) 'In VERLUSTE. fges = ', fges, '  q = ', q

  IF (fges .lt. 1.e-02) then

    hr = hr + dx
    f_alt = fges
    GOTO 101

  ELSEIF ( (q/fges) .gt. 20) then

    IF (abs (f_alt - fges) .gt. 1.e-06) then
      hr = hr + dx
      f_alt = fges
      ifehlg = 0
      GOTO 101
    ELSE
      ifehlg = 1
    ENDIF

  ENDIF                                     
                                                                        
                                                                        
  ! ------------------------------------------------------------------
  ! Berechnung des wirksamen Reibungsverlustes des Profils (rg)
  ! ------------------------------------------------------------------
                                                                        
  !JK  BERECHNUNG NACH DARCY-WEISBACH
  IF (FLIESSGESETZ/='MANNING_STR') then
    nstat = nprof
    CALL eb2ks (iprof, hv, rg, rg1, q, q1, itere1, nstat, hr, nknot)

  !JK  BERECHNUNG NACH GMS
  ELSE
    ! vorbelegung
    hv = 0.
    rg = 0.

    CALL eb2kst (indmax, hr, hv, rg, q)
  ENDIF

  !write (*,*) 'In VERLUSTE. Nach eb2ks.'
  !do i = 1, 3
  !  write (*,*) 'rk(',i,') = ', rk(i)
  !end do



  ! -------------------------------------------------------------------
  ! Berechnung des wirksamen Geschwindikeitsverlustes (hvm) des Profils
  ! -------------------------------------------------------------------
  IF (nprof.eq.1.or.istat.eq.1) then
    hvst = 0.

  ELSE

    hv_uw = hv1
    hv_ow = hv

    fges_uw = fges1
    fges_ow = fges

    bges_uw = fges_uw / 1.0   ! Annahme einer virtuellen Wassertiefe von 1,0 m, kann vielleicht mal um genauen Wert ergänzt werden?
    bges_ow = fges_ow / 1.0   ! Annahme einer virtuellen Wassertiefe von 1,0 m, kann vielleicht mal um genauen Wert ergänzt werden?

    !write (*,1005) hv_uw, hv_ow
    1005 format (1X, 'Geschwindigkeitshoehe Unterwasser (alt) = ', F10.6, /, &
               & 1X, 'Geschwindigkeitshoehe Oberwasser (neu)  = ', F10.6)

    if (VERZOEGERUNGSVERLUST == 'DFG ' .or. VERZOEGERUNGSVERLUST == 'BWK ') then
      ! Neue Berechnungsmethode vom Dez. 2006 (siehe BWK Merkblatt 1, 1999, S. 39f)
      if (hv_uw < hv_ow) then

        ! Bestimmung des Aufweitungswinkels
        alpha_RAD = ATAN( (bges_uw-bges_ow) / (2*str) )
        alpha_GRAD = alpha_RAD * 180.0 / pi

        !write (*,1006) alpha_GRAD
        1006 format (1X, 'Aufweitung nach Oberwasser mit einem Winkel von ALPHA = ', F10.7)

        if (alpha_GRAD < 1.0) then
          !write (*,*) 'Aufweitung ist kleiner als 1 grad.'
          zeta = 0.0
          !write (*, 1009) zeta

        else if (alpha_GRAD >=1.0 .and. alpha_GRAD < 8.0) then
          !write (*,*) 'Aufweitung ist zwischen 1,0 und 8,0 grad.'
          zeta = (alpha_GRAD - 1) * 0.1 / (8.0 - 1.0)
          !write (*, 1009) zeta

        else if (alpha_GRAD >=8.0 .and. alpha_GRAD < 12.5) then
          !write (*,*) 'Aufweitung ist zwischen 8,0 und 12,5 grad.'
          zeta = ( (alpha_GRAD - 8) * 0.1 / (12.5 - 8.0) ) + 0.1
          !write (*, 1009) zeta

        else if (alpha_GRAD >=8.0 .and. alpha_GRAD < 12.5) then
          !write (*,*) 'Aufweitung ist zwischen 8,0 und 12,5 grad.'
          zeta = ( (alpha_GRAD - 8) * 0.1 / (12.5 - 8.0) ) + 0.1
          !write (*, 1009) zeta

        else if (alpha_GRAD >=12.5 .and. alpha_GRAD < 18.5) then
          !write (*,*) 'Aufweitung ist zwischen 12,5 und 18,5 grad.'
          zeta = ( (alpha_GRAD - 12.5) * 0.3 / (18.5 - 12.5) ) + 0.2
          !write (*, 1009) zeta

        else if (alpha_GRAD >=18.5 .and. alpha_GRAD < 45.0) then
          !write (*,*) 'Aufweitung ist zwischen 12,5 und 18,5 grad.'
          zeta = ( (alpha_GRAD - 18.5) * 0.25 / (45.0 - 18.5) ) + 0.5
          !write (*, 1009) zeta

        else if (alpha_GRAD >= 45.0) then
          !write (*,*) 'Aufweitung ist groesser als 45,0 grad.'
          zeta = 0.75
          !write (*, 1009) zeta
        end if
        1009 format (1X, 'Verlustbeiwert bei seitlicher Verziehung. ZETA = ', F10.7 )

        hvst = zeta * ( hv_ow - hv_uw)

        !write (*, 1010) hvst
        1010 format (1X, 'Verlusthoehe bei deutlicher Profilaufweitung. HVST = ', F10.7, /)

      else
        ! Bestimmung des Einschnuerungswinkels
        alpha_RAD = ATAN( (bges_ow - bges_uw) / (2*str) )
        alpha_GRAD = alpha_RAD * 180.0 / pi

        !write (*,1006) alpha_GRAD
        1011 format (1X, 'Einschnuerung nach Oberwasser mit einem Winkel von ALPHA = ', F10.7)

        if (alpha_GRAD < 8.0) then
          !write (*,*) 'Einschnuerung ist kleiner als 8 grad.'
          zeta = 0.0
          !write (*, 1012) zeta

        else if (alpha_GRAD >=8.0 .and. alpha_GRAD < 12.5) then
          !write (*,*) 'Einschnuerung ist zwischen 8,0 und 12,5 grad.'
          zeta = (alpha_GRAD - 8) * 0.1 / (12.5 - 8.0)
          !write (*, 1012) zeta

        else if (alpha_GRAD >=12.5 .and. alpha_GRAD < 18.5) then
          !write (*,*) 'Einschnuerung ist zwischen 12,5 und 18,5 grad.'
          zeta = ( (alpha_GRAD - 12.5) * 0.1 / (18.5 - 12.5) ) + 0.1
          !write (*, 1012) zeta

        else if (alpha_GRAD >=18.5 .and. alpha_GRAD < 45.0) then
          !write (*,*) 'Einschnuerung ist zwischen 12,5 und 18,5 grad.'
          zeta = ( (alpha_GRAD - 18.5) * 0.25 / (45.0 - 18.5) ) + 0.2
          !write (*, 1012) zeta

        else if (alpha_GRAD >= 45.0) then
          !write (*,*) 'Einschnuerung ist groesser als 45,0 grad.'
          zeta = 0.30
          !write (*, 1012) zeta
        end if
        1012 format (1X, 'Verlustbeiwert bei seitlicher Verziehung. ZETA = ', F10.7 )

        hvst = zeta * ( hv_uw - hv_ow )

        !write (*, 1013) hvst
        1013 format (1X, 'Verlusthoehe bei deutlicher Profileinschnuerung. HVST = ', F10.7, /)


      end if

    ELSEIF (VERZOEGERUNGSVERLUST == 'BJOE') then
    !MD   fuer Berechnungsvarainte mit konstantem Rebinungsgefaelle:
    !MD   werden KEINE oertlichen Verluste beruecksichtigt
      hvst = hv1 - hv

      IF (hv1.lt.hv) then

        ! beruecksichtigung aufweitungsverlust
        ! betta(dvwk)=2./3.
        ! betta(bjoernsen)=0.5
        ! betta(DFG)=2./(1.+Ai/Ai-1)

        IF (fges.lt. (fges1 - 2. * str / 7.) ) then
          ! Aufweitung > 1:7 --> Borda'scher Stossdruck
          ht = MIN (hr, hmax) - hmin
          vo = sqrt (hv * 2 * g)
          vu = sqrt (hv1 * 2 * g)
          hborda = GET_BORDA (vo, vu, ht)

        ELSE

          if (VERZOEGERUNGSVERLUST == 'DVWK') then
            betta = 2. / 3.
          else if (VERZOEGERUNGSVERLUST == 'BJOE') then
            betta = 0.5
          else
            betta = 0.5
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
                                                                        
  ! ---------------------------------------------------------------------
  ! Ermittlung lokaler verluste: NICHT fuer Reibungsgefaelle = konstant
  ! ---------------------------------------------------------------------
  IF (VERZOEGERUNGSVERLUST /= 'NON ') then
    horts = psiort * hv
    IF (nprof.ne.1.and.istat.ne.1) then
      vu = sqrt (hv1 * 2 * 9.81)
      vo = sqrt (hv * 2 * 9.81)
      heins = psiein * (vu - vo) * (vu - vo) / (2 * 9.81)
    ENDIF

  else  ! MD  fuer Reibungsgefaelle = konstant
    horts = 0.0
    IF (nprof.ne.1.and.istat.ne.1) then
      vu = sqrt (hv1 * 2 * 9.81)
      vo = sqrt (hv * 2 * 9.81)
      heins = (vu - vo) * (vu - vo) / (2 * 9.81)
    ENDIF
  end if



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

    DO i = 1, indmax

      qt (i) = 0.
      v (i) = 0.
      f (i) = 0.
      rk (i) = 0.
      u (i) = 0.

    END DO

    q = 0.
    vm = 0.

  ENDIF

  hvst = vm

ENDIF

END SUBROUTINE verluste                                         
