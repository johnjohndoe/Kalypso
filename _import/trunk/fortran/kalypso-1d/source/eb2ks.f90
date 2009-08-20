!     Last change:  MD    8 Jul 2009    7:54 pm
!--------------------------------------------------------------------------
! This code, ebksn.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - eb2ks
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



SUBROUTINE eb2ks (iprof, hv, rg, rg_alt, q, q_alt, itere1, nstat, hr, nknot, Q_Abfrage)

!***********************************************************************
!**                                                                     
!**   Subroutine eb2ks                                                  
!**                                                                     
!JK   BESCHREIBUNG: BERECHNUNG DER RAUHIGKEITSHOEHE/ENERGIEHOEHE        
!**                                                                     
!**   DIREKT ÜBERGEBENE VARIABLEN                                       
!**   ---------------------------                                       
!**   hr      -                                                         
!**   hv      - Verlusthoehe infolge Reibung
!**   iprof   -                                                         
!**   itere1  - Zahl der Iterationen                                    
!**   nstat
!**   q       -                                                         
!**   q_alt   -                                                         
!**   rg                                                                
!**   rg_alt                                                            
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**                                                                     
!**   a_hg    --      durchströmte Fläche des Flußschlauches            
!**   a_ks    --      durchströmte Fläche eines Teilabschnittes im      
!**                   Vorland                                           
!**   a_li    --      Einflußfläche des linken Absturzes                
!**   a_re    --      Einflußfläche des rechten Absturzes               
!**   ak_mi   --      abgeminderte Rauheit eines Teilabschnittes im     
!**                   Vorland                                           
!**   akges   --      Gesamtrauheit                                     
!**   aks_li  --      Rauheit des linken Absturzes                      
!**   aks_re  --      Rauheit des rechten Absturzes                     
!**   alges   --      Gesamtwiderstandsbeiwert                          
!**   alsum   --      Summe aus den Teilwiderstandsbeiwerte             
!**                   multipliziert mit dem jeweiligen Teilumfang       
!**   alpha   --      BOESCHUNGSWINKEL                                  
!**   b_hg    --      Breite des Flußschlauches                         
!**   bf(1)   --      mitwirkende Flußschlauchbreite für die linke      
!**                   Trennfläche                                       
!**   bf(2)   --      mitwirkende Flußschlauchbreite für die rechte     
!**                   Trennfläche                                       
!**   dhy1    --      hydraulischer Durchmesser                         
!**   difi    --      Fehler nach Iteration, =abs (isener-isenen)/isener
!**   f(1)    --      durchströmte Fläche des linken Vorlandes          
!**   f(2)    --      durchströmte Fläche des Flußschlauches            
!**   f(3)    --      durchströmte Fläche des rechten Vorlandes         
!**   factor1 --      Faktor zur Berechnung der Gesamtrauheit           
!**   factor2 --      Faktor zur Berechnung der Gesamtrauheit           
!**   fges    --      gesamte durchströmte Fläche                       
!**   h_ks    --      Wasserspiegelhöhe eines Teilabschnittes im        
!**                   Vorland                                           
!**   h_li    --      Wasserspiegelhöhe an einem Absturz links          
!**   h_re    --      Wasserspiegelhöhe an einem Absturz rechts         
!**   hr      --      Wasserspiegelhöhe                                 
!**   hv      --      Geschwindigkeitsverlusthöhe
!**   hvor    --      mittlere Wasserspiegelhöhe im Vorland             
!**   if_l    --      Fehlervariable für die Subroutine lindy
!**   if_pa   --      Fehlervariable für die Subroutine pasche          
!**   ii      --      LAUFPARAMETER ANZAHL KNOTEN                       
!**   ischl   --      linker Punkt des linken Vorlandes                 
!**   ischr   --      rechter Punkt des rechten Vorlandes               
!**   isenen  --      Energiegefälle                                    
!**   isener  --      Energiegefälle                                    
!**   isenk   --      Funktion des Energiegefälles                      
!**   itrli   --      Punkt der linken Trennfläche                      
!**   itrre   --      Punkt der rechten Trennfläche                     
!**   k_ks    --      Rauheit eines Teilabschnittes im Vorland          
!**   l_hg    --      Widerstandsbeiwert des Flußschlauches             
!**   l_ks    --      Widerstandsbeiwert eines Teilabschnittes im       
!**                   Vorland                                           
!**   maxkla  --      Anzahl der Rauheitsabschnitte
!**   nknot   --      Anzahl der Punkte in einem Profil                 
!**   nstat   --      Anzahl der Stationen                              
!**   phion   --      Zähler des Energiestrombeiwertes                  
!**   phiun   --      Nenner des Energiestrombeiwertes                  
!**   psi     --      Summe von pst von linkes, rechtes Vorland und     
!**                   Flußschlauch                                      
!**   psi_alt --      Summe von pst von linkes, rechtes Vorland und     
!**                   Flußschlauch (alt)                                
!**   pst     --      Geometrieparameter zur Berechnung des             
!**                   Reibungsverlustes                                 
!**   q       --      Abfluß                                            
!**   q_alt   --      Abfluß (alt)                                      
!**   q_hg    --      Abfluß im Flußschlauch                            
!**   q_ks    --      Abfluß eines Teilabschnittes im Vorland           
!**   qgs     --      Gesamtabfluß                                      
!**   qt(1)   --      Teilabfluß linkes Vorland                         
!**   qt(2)   --      Teilabfluß Flußschlauch                           
!**   qt(3)   --      Teilabfluß rechtes Vorland                        
!**   qvor1   --      zusätzlicher Abfluß im linken Vorland             
!**   qvor2   --      zusätzlicher Abfluß im rechten Vorland            
!**   r_hg    --      hydraulischer Radius des Flußschlauches           
!**   ra(2)   --      hydraulischer Radius des Flußschlauches           
!**   re      --      Reynoldszahl                                      
!**   rg      --      Reibungsverlusthöhe                               
!**   rg_alt  --      Reibungsverlusthöhe (alt)                         
!**   rk(1)   --      Widerstandsbeiwert linkes Vorland
!**   rk(2)   --      Widerstandsbeiwert Flußschlauch                   
!**   rk(3)   --      Widerstandsbeiwert rechtes Vorland                
!**   u(1)    --      benetzter Umfang des linken Vorlandes             
!**   u(2)    --      benetzter Umfang des Flußschlauches               
!**   u(3)    --      benetzter Umfang des rechten Vorlandes            
!**   u_ks    --      benetzter Umfang eines Teilabschnittes im Vorland 
!**   uges    --      gesamter benetzter Umfang                         
!**   v(1)    --      mittlere Geschwindigkeit des linken Vorlandes     
!**   v(2)    --      mittlere Geschwindigkeit des Flußschlauches       
!**   v(3)    --      mittlere Geschwindigkeit des rechten Vorlandes    
!**   v_hg    --      mittlere Geschwindigkeit im Flußschlauch          
!**   v_ks    --      mittlere Geschwindigkeit eines Teilabschnittes im 
!**                   Vorland                                           
!**   vges    --      gesamte mittlere Geschwindigkeit                  
!**   vlam    --      Verhältnis der Widerstandsbeiwerte links und recht
!**                                                                     
                                                                        
!HB   ***************************************************************** 
!HB   26.11.2001 H.Broeker                                              
!HB   --------------------                                              
!HB   a_vor_li --     durchstroemte Flaeche, Vorland links              
!HB   a_vor_re --     durchstroemte Flaeche, Vorland rechts             
!HB   alpha_E  --     Energiestrombeiwert je Berechnungsschritt         
!HB   alpha_EW --     Energiestrombeiwert auf Profilnummer bezogen      
!HB   alpha_I  --     Boussinesq-Beiwert je Berechnungsschritt          
!HB   alpha_IW --     Boussinesq-Beiwert auf Profilnummer bezogen       
!HB   phi_o_li --     Zaehler der Summe Vorland links (Energiestrombeiwe
!HB   phi_u_li --     Nenner der Summe Vorland links (Energiestrombeiwer
!HB   phi_o_re --     Zaehler der Summe Vorland rechts (Energiestrombeiw
!HB   phi_u_re --     Nenner der Summe Vorland rechts (Energiestrombeiwe
!HB   phi_o_fl --     Zaehler des Flusschlauches (Energiestrombeiwert)  
!HB   phi_u_fl --     Nenner des Flusschlauches (Energiestrombeiwert)   
!HB   phion    --     Summe des Zähler beim Energiestrombeiwertes       
!HB   phiun    --     Summe des Nenner beim Energiestrombeiwertes und   
!HB                   Boussinesq-Beiwertes                              
!HB   pn_alpha --     Profilnummer                                      
!HB   psi_o_li --     Zaehler der Summe Vorland links (Boussinesq-Beiwer
!HB   psi_o_re --     Zaehler der Summe Vorland rechts (Boussinesq-Beiwe
!HB   psi_o_fl --     Zaehler der Summe Flusschlauch (Boussinesq-Beiwert
!HB   psion    --     Summe des Zaehler beim Boussinesq-Beiwertes       
!HB   st_alpha --     Stationskilometer                                 
!HB   gesamt_a --     gesamte durchstroemte Flaeche                     
!HB   ***************************************************************** 
                                                                        

                                                                        
! ------------------------------------------------------------------
! VEREINBARUNGSTEIL                                                 
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS
USE MOD_INI

! Calling variables
CHARACTER(LEN=1), INTENT(IN)	:: iprof        ! Art des Profils
REAL, INTENT(OUT)               :: hv           ! Geschwindigkeitsverlusthoehe
REAL, INTENT(INOUT)             :: rg           ! Reibungsverlusthoehe
REAL, INTENT(IN)                :: rg_alt       ! Reibungsverlusthoehe (alt)
REAL, INTENT(IN)                :: q            ! Abfluss
REAL, INTENT(IN)                :: q_alt        ! Abfluss (alt)
INTEGER, INTENT(IN)             :: itere1       ! Zahl der Iterationen
INTEGER, INTENT(IN)             :: nstat        ! Anzahl der Stationen/Nummer der aktuellen Station
REAL, INTENT(IN)                :: hr		! Wasserspiegelhöhe
INTEGER, INTENT(IN) 		  	:: nknot 	! Anzahl der Profilpunkte


! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: bete1 (idim2), bete12 (idim2)
COMMON / datlin1 / bete1, bete12
! ----------------------------------------------------------------------------------


! COMMON-Block /FEXI/ --------------------------------------------------------------
! DK, 21/06/01 - The user-defined value of MEI (for Kouwen procedure)
REAL 		:: mei (maxkla)
COMMON / flexi / mei
! ----------------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /NR_ALPHA/ -----------------------------------------------------
! Variablen zur Uebergabe der Profilnummern und Stationskilometer
! fuer die Energiestrom- und Boussinesq-beiwertberechnung
INTEGER 	:: pn_alpha
REAL 		:: st_alpha, gesamt_a (maxkla), alpha_EW (maxkla), alpha_IW (maxkla)
COMMON / nr_alpha / pn_alpha, st_alpha, gesamt_a, alpha_EW, alpha_IW
! -----------------------------------------------------------------------------


! COMMON-Block /PA_COM/ -------------------------------------------------------
REAL 		:: a_ks (maxkla), b_ks (maxkla), r_ks (maxkla), u_ks (maxkla)
REAL            :: k_ks (maxkla), h_ks (maxkla), v_ks (maxkla), l_ks (maxkla)
REAL            :: q_ks (maxkla), b_hg, a_hg, q_hg, v_hg, r_hg, l_hg, fr_hg
INTEGER         :: ischl, ischr
COMMON / pa_com / a_ks, b_ks, r_ks, u_ks, k_ks, h_ks, v_ks, l_ks, &
       & q_ks, b_hg, a_hg, u_hg, q_hg, v_hg, r_hg, l_hg, fr_hg, ischl, ischr
! -----------------------------------------------------------------------------


! COMMON-Block /PRI/ ----------------------------------------------------------
! DK, 07/06/01 - Parameters for printing warnings and comments!
INTEGER         :: ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
COMMON / pri / ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /ROUGH/ --------------------------------------------------------
! DK, 21/06/01 - Parameters of bottom roughness type and grass type!
CHARACTER(LEN=4) :: tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
COMMON / rough / tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
! -----------------------------------------------------------------------------


! Local variables
INTEGER :: if_l (maxkla)      ! if_l = Fehlervariable für die Subroutine lindy
REAL    :: isener, isenen, isenk
REAL 	:: hvor (maxkla), cwr (maxkla), alp (maxkla)
REAL 	:: also (maxkla), anl (maxkla), anb (maxkla)
REAL  	:: vnvv (maxkla), cwn (maxkla)
REAL 	:: bf (2), vt_l (2), anl_l (2), anb_l (2), om_l (2), ct_l (2)
REAL 	:: bm_l (2), alt_l (2), h_t (2), v1_hg, vt_n (2)
REAL 	:: cm                           !JK   ZUSATZ FUER ERWEITERUNG VON JANA KIEKBUSCH, 23.06.00

!HB   26.11.2001 - H.Broeker
!HB   Parameter fuer Energiestrombeiwert
REAL 	:: phi_o_li, phi_u_li, phi_o_re, phi_u_re, phi_o_fl, phi_u_fl
REAL 	:: psi_o_li, psi_o_re, psi_o_fl, psion
REAL 	:: a_vor_li, a_vor_re, alpha_E, alpha_I
CHARACTER(LEN=11), INTENT(IN) :: Q_Abfrage     !Abfrage fuer Ende der Inneren Q-Schleife

                                                                        
!WP 10.05.2004                                                          
ibed = 0
!WP 10.05.2004                                                          


!write (*,*) '     In EB2KS. Anfang Subroutine. f(1) = ', f(1), ' f(3) = ', f(3), ' hr = ', hr

! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
CALL maeand (cm)

! Gleichsetzen der uebergebenen Groessen mit den common-werten
it2max = 4

!UT   STARTPARAMETER SCHLEIFE 15, ANZAHL DER ITERATIONEN
itere2 = 0


IF (itere1.le.1 .and. nstat.gt.1 .and. BERECHNUNGSMODUS/='REIB_KONST') then   !MD neu**
  isenen = q_alt * q_alt / rg_alt / rg_alt
ELSE IF (itere1 .le. 1 .and. nstat .le. 1) then
  isenen = 0.001
ELSE IF (nstat .eq. 1) then
  isenen = q * q / rg / rg
ELSEIF (nstat.gt.1 .and. BERECHNUNGSMODUS=='REIB_KONST'.and. Q_Abfrage=='IN_SCHLEIFE') then
  !MD  Nur unter 'REIB_KONST' nutzen, wenn nicht in einer Inneren Abflussschleife an Bruecke oder Wehr
  isenen = q * q / rg / rg
ELSEIF (itere1.gt.1) then
  IF (REIBUNGSVERLUST == 'GEOMET') THEN
    isenen = (q + q_alt) * (q + q_alt) / (rg + rg_alt) / (rg + rg_alt)
  ELSE  ! REIBUNGSVERLUST=='TRAPEZ'
    isenen = q * q / rg / rg
  ENDIF
ENDIF

!MD Vermeidung von Unendlichgroßem Gefaelle oder negativem Gefaelle
IF (isenen .gt. 3.0) then
  isenen = 2.999
ElseIF (isenen .le. 0.1E-4) then
  isenen = 0.001
ENDIF


!UT   STARTPARAMETER FUER DIE DIFFERENZ difi = abs(isener-isenen)/isener
difi = 1000.

!UT   difi = abs(isener-isenen)/isener ----------------------------------
DO 15 WHILE(difi .gt. 0.01)
! -----------------------------------------------------------------------

  !***********************************************************************
  !     DK, 07/06/01 - Setting values of parameters for
  !                    printing warnings and comments!
  icwl = 0
  icwm = 0
  icwr = 0
  ikol = 0
  ikom = 0
  ikor = 0
  ifum = 0
  !***********************************************************************

  isener = isenen

  !UT   ZAEHLEN DES ITERATIONSPARAMETERS FUER SCHLEIFE 15
  itere2 = itere2 + 1

  !UT   SETZEN DER TEILABFLUSSWERTE, 1=LINKES VORL.,2=FLUSSCHL. 3=RECHTES,
  qt (1) = 0.
  qt (2) = 0.
  qt (3) = 0.

  !UT   SETZEN WIDERSTANDSBEIWERTE, 1=LINKES VORL.,2=FLUSSCHL. 3=RECHTES,
  rk (1) = 0.
  rk (2) = 0.
  rk (3) = 0.

  !UT   SETZEN DER WERTE ZU NULL
  DO ii = 1, nknot
    v_ks (ii) = 0.0
    l_ks (ii) = 0.0
    q_ks (ii) = 0.0
    hvor (ii) = 0.0
  END do


  !***********************************************************************
  !     DK 15/05/01
  !      PRINT *,'Sub EB2KS - general data!'
  !      PRINT *,'nknot=',nknot
  !      print *,'ischl=',ischl,' ischr=',ischr
  !      print *,'itrli=',itrli,' itrre=',itrre
  !      PRINT *
  !***********************************************************************

  !***********************************************************************
  !      DK 22/05/01 - no need for this variable any longer (see below)!
  !      alsum = 0.
  !***********************************************************************


  !***********************************************************************
  !     DK 17/05/01 - setting values of f and u for flow active subsect.
  !     actf - wetted area of flow active subsections
  !     actu - wetted perimeter of flow active subsections
  actf = f (1)
  actu = u (1)
  !***********************************************************************


  !JK   SCHLEIFE FUER BERECHNUNG LINKES VORLAND --------------------------
  linkes_vorland: DO ii = ischl, itrli - 1

    hvor(ii) = hr - (h_ks(ii) + h_ks (ii+1) ) / 2.
    !MD: passt auch fur negative WSP

    IF (b_ks(ii) .gt. 1.e-01) then
      alpha = atan (abs (h_ks(ii) - h_ks(ii+1)) / b_ks (ii) )
    ELSE
      alpha = 0.
    ENDIF

    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KS - left floodplain!'
    !         PRINT *,'Calculation for subsection',ii,'!'
    !         PRINT *,'hr=',hr
    !         IF(ii.gt.1) PRINT *,'h_ks(ii-1)=',h_ks(ii-1)
    !         PRINT *,'h_ks(ii)=',h_ks(ii)
    !         print *,'h_ks(ii+1)=',h_ks(ii+1),' h_ks(ii+2)=',h_ks(ii+2)
    !         print *,'hvor(ii)=',hvor(ii)
    !         print *,'b_ks(ii)=',b_ks(ii),' alpha=',alpha
    !         PRINT *
    !***********************************************************************


    IF (ii .gt. ischl) then

      IF (hr .gt. h_ks(ii-1) ) then
        h_li = h_ks (ii-1) - h_ks (ii)
      ELSE
        h_li = hr - h_ks (ii)
        !DK 10/05/01          h_ks(ii-1) changed to h_ks(ii)
      ENDIF

      a_li = a_ks (ii-1)
      aks_li = k_ks (ii-1)

    ELSE

      h_li = 0.
      a_li = 0.
      aks_li = 0.

    ENDIF

    IF (ii .lt. (ischr-1) ) then

      IF (hr .gt. h_ks (ii+2) ) then
        h_re = h_ks (ii+2) - h_ks (ii+1)
      ELSE
        h_re = hr - h_ks (ii+1)
      ENDIF

      a_re = a_ks (ii+1)
      aks_re = k_ks (ii+1)
      !DK 10/05/01       ! these two lines are moved from the line **

    ELSE

      h_re = 0.
      a_re = 0.
      aks_re = 0.

    ENDIF

    !DK      Activated 22/05/01 for KOUWEN:
    ak_mi = k_ks (ii)


    !UT      FEHLERMELDUNG WENN WSPHOEHE hvor IM VORLAND NULL, ABER EINE
    !UT      DURCHSTROEMTE FLAECHE a_ks VORLIEGT
    IF (hvor(ii) .lt. 1.e-06 .and. a_ks(ii) .gt. 1.e-05) then
      PRINT * , 'Fehler! Fliesstiefe Vorland inkorrekt '
      PRINT * , 'Voraussichtlich Geometriefehler.'
      PRINT * , 'Beim Entwickler melden.'
      STOP 'SUB EB2KS, da Fehler nicht behebbar.'
    ENDIF


    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KSN_00 - left floodplain!'
    !         PRINT *,'h_li=',h_li,' a_li=',a_li,' aks_li=',aks_li
    !         PRINT *,'h_re=',h_re,' a_re=',a_re,' aks_re=',aks_re
    !         print *,'h_ks(ii+1)=',h_ks(ii+1),' hvor(ii)=',hvor(ii)
    !         print *,'k_ks(ii)=',k_ks(ii),' ak_mi=',ak_mi
    !         PRINT *,'It is going to Sub LINDY!'
    !         PRINT *
    !***********************************************************************

    ibed = 1

    !***********************************************************************
    !        DK 29/05/01
    !         PRINT *,'Sub EB2KS - left floodplain!'
    !         PRINT *,'Values before LINDY:'
    !         print *,'ibed=',ibed
    !         print *,'icwl=',icwl,' icwm=',icwm,' icwr=',icwr
    !         PRINT *
    !***********************************************************************

    !UT      AUFRUF VON LINDY, BERECHNUNG VON
    !UT      LAMBDA IM VORLAND AUS BEWUCHS UND SOHLRAUHEIT

    !WP 02.02.2006
    !write (*,*) 'In EB2KS, kurz vor LINDY Linkes Vorland. '
    !write (*,*) 'v_ks(ii) = ', v_ks(ii)
    !write (*,*) 'l_ks(ii) = ', l_ks(ii)
    !write (*,*) 'ax(ii)   = ', ax(ii)
    !write (*,*) 'ay(ii)   = ', ay(ii)
    !write (*,*) 'dp(ii)   = ', dp(ii)
    !write (*,*) 'hvor(ii) = ', hvor(ii)
    !write (*,*) 'mei(ii)  = ', mei(ii)
    !write (*,*) 'isener   = ', isener
    !write (*,*) 'u_ks(ii) = ', u_ks(ii)
    !write (*,*) 'a_ks(ii) = ', a_ks(ii)
    !write (*,*) 'ak_mi    = ', ak_mi
    !write (*,*) 'a_li     = ', a_li
    !write (*,*) 'a_re     = ', a_re
    !write (*,*) 'h_li     = ', h_li
    !write (*,*) 'h_re     = ', h_re
    !write (*,*) 'aks_li   = ', aks_li
    !write (*,*) 'aks_re   = ', aks_re
    !write (*,*) 'alpha    = ', alpha
    !write (*,*) 'cwr(ii)  = ', cwr(ii)
    !write (*,*) 'alp(ii)  = ', alp(ii)
    !write (*,*) 'also(ii) = ', also(ii)
    !write (*,*) 'anl(ii)  = ', anl(ii)
    !write (*,*) 'anb(ii)  = ', anb(ii)
    !write (*,*) 'vnvv(ii) = ', vnvv(ii)
    !write (*,*) 'cwn(ii)  = ', cwn(ii)
    !write (*,*) 'if_l(ii) = ', if_l(ii)
    !write (*,*) 'k_ks(ii) = ', k_ks(ii)
    !write (*,*) 'formb(1) = ', formbeiwert(1)

    CALL lindy (v_ks (ii), l_ks (ii), ax (ii), ay (ii), dp (ii),    &
     & hvor (ii), mei (ii), isener, u_ks (ii), a_ks (ii), ak_mi, a_li, &
     & a_re, h_li, h_re, aks_li, aks_re, alpha, cwr (ii), &
     & alp (ii), also (ii), anl (ii), anb (ii), vnvv (ii), cwn (ii),   &
     & if_l (ii), formbeiwert(1) )


    !WP 02.02.2006
    !write (*,*) 'In EB2KS, kurz nach LINDY Linkes Vorland. '
    !write (*,*) 'v_ks(ii) = ', v_ks(ii)
    !write (*,*) 'l_ks(ii) = ', l_ks(ii)
    !write (*,*) 'ax(ii)   = ', ax(ii)
    !write (*,*) 'ay(ii)   = ', ay(ii)
    !write (*,*) 'dp(ii)   = ', dp(ii)
    !write (*,*) 'hvor(ii) = ', hvor(ii)
    !write (*,*) 'mei(ii)  = ', mei(ii)
    !write (*,*) 'isener   = ', isener
    !write (*,*) 'u_ks(ii) = ', u_ks(ii)
    !write (*,*) 'a_ks(ii) = ', a_ks(ii)
    !write (*,*) 'ak_mi    = ', ak_mi
    !write (*,*) 'a_li     = ', a_li
    !write (*,*) 'a_re     = ', a_re
    !write (*,*) 'h_li     = ', h_li
    !write (*,*) 'h_re     = ', h_re
    !write (*,*) 'aks_li   = ', aks_li
    !write (*,*) 'aks_re   = ', aks_re
    !write (*,*) 'alpha    = ', alpha
    !write (*,*) 'cwr(ii)  = ', cwr(ii)
    !write (*,*) 'alp(ii)  = ', alp(ii)
    !write (*,*) 'also(ii) = ', also(ii)
    !write (*,*) 'anl(ii)  = ', anl(ii)
    !write (*,*) 'anb(ii)  = ', anb(ii)
    !write (*,*) 'vnvv(ii) = ', vnvv(ii)
    !write (*,*) 'cwn(ii)  = ', cwn(ii)
    !write (*,*) 'if_l(ii) = ', if_l(ii)
    !write (*,*) 'k_ks(ii) = ', k_ks(ii)
    !write (*,*) 'formb(1) = ', formbeiwert(1)


    !***********************************************************************
    !JK   PROGRAMMERWEITERUNG, 23. JUNI 2000, JANA KIEKBUSCH
    !JK   ------------------------------------------------------------------
    !      if (maean_ber.eq.'bwk') then

    l_ks (ii) = cm * l_ks (ii)
    v_ks (ii) = v_ks (ii) / SQRT (cm)

    !MD Abfangen von Extremwerten
    IF (l_ks(ii).le.1.e-06) THEN
      l_ks(ii) = 0.0
    END IF
    IF (v_ks(ii).le.1.e-05) THEN
      v_ks(ii) = 0.0
    END IF
    !          q_ks(ii)=v_ks(ii)*a_ks(ii)


    !***********************************************************************
    !        DK 29/05/01
    !         PRINT *,'Sub EB2KS - left floodplain!'
    !         PRINT *,'Values after correction:'
    !         print *,'l_ks(ii)=',l_ks(ii),' v_ks(ii)=',v_ks(ii)
    !         PRINT *
    !***********************************************************************

    !***********************************************************************
    !         DK 22/05/01 - no more calculation using wetted
    !                       perimeter as the weighting factor
    !          alsum  = alsum + l_m(ii)*u_ks(ii)
    !***********************************************************************

    !JK   ENDE PROGRAMMERWEITERUNG
    !***********************************************************************
    !      else
    !UT       BERECHNUNG DES ABFLUSSES AUS GESCHW. UND FLAECHE
    !          q_ks(ii)=v_ks(ii)*a_ks(ii)
    !UT       BERECHNUNG DER NENNERSUMME VON LAMDAges LINKES VORLAND
    !UT       NACH EINSTEIN, NENNER FORMEL (7) BWK, S.16


    !***********************************************************************
    !         DK 22/05/01 - no more calculation using wetted
    !                       perimeter as the weighting factor
    !          alsum  = alsum + l_ks(ii)*u_ks(ii)
    !***********************************************************************

    !      endif

    !UT       BERECHNUNG DES GESAMTABFLUSSES IM LINKEN VORLAND
    !UT       ALS SUMME DER TEILABFLUESSE i
    q_ks (ii) = v_ks (ii) * a_ks (ii)
    qt (1) = qt (1) + q_ks (ii)


    !***********************************************************************
    !         DK 17/05/01 - excluding flow inactive subsections
    IF (l_ks (ii) .EQ. 0.) then
      actf = actf - a_ks (ii)
      actu = actu - u_ks (ii)
    ENDIF
    !***********************************************************************


    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KSN_00 - left floodplain!'
    !         print *,'q_ks(ii)=',q_ks(ii),' qt(1)=',qt(1)
    !         PRINT *,'Calculation for the subsection finished!'
    !         PRINT *,'It is going to the next one!'
    !         PRINT *
    !***********************************************************************


  !JK   SCHLEIFENENDE FUER BERECHNUNG LINKES VORLAND ---------------------
  END DO linkes_vorland



  !***********************************************************************
  !     DK 22/05/01 - renaming !!!
  !      if (maean_ber.eq.'bwk') then
  !          l_ks(itrli-1)=l_m(itrli-1)
  !          v_ks(itrli-1)=v_m(itrli-1)
  !      endif
  !***********************************************************************

  !write (*,*) '        In EB2KS. Linkes Vorland. F(1) = ', f(1)

  !UT   FALLS FLAECHE RECHTES VORLAND GROESSER NULL
  !UT   rk = WIDERSTANDSBEIWERT, v = GESCHWINDIGKEIT, qt = ABFLUSS
  IF (f(1) .gt. 1.e-06 .and. qt(1) .gt. 1.e-06) then

    IF (actu .gt. 0 .and. actf .gt. 0) then
      ! rk(1)=alsum/actu
      v(1) = qt (1) / actf

      !***********************************************************************
      !DK   22/05/01 - calculation l=(v_i/v)^2*r_hy/r_hy,i*l_i
      rk (1) = (v_ks (itrli - 1) / v (1) ) **2 * (actf / actu)  &
       & * (u_ks (itrli - 1) / a_ks (itrli - 1) ) * l_ks (itrli - 1)
      !***********************************************************************

    ENDIF

  ELSE

    rk (1) = 0.
    v (1) = 0.

  ENDIF

  !write (*,*) '        In EB2KS. Linkes Vorland. RK(1) = ', rk(1)

  !***********************************************************************
  !DK   Change!!! This includes also cases where only one subsection of
  !DK   floodplain is flooded, but with very small flow depth (so that the
  !DK   subsection is considered not flow active) !!!
  !***********************************************************************


  !***********************************************************************
  !     DK 22/05/01
  !      PRINT *,'Sub EB2KS - finished calc. for left floodplain!'
  !      do 4040 ii=ischl,itrli-1
  !         PRINT *,'ii=',ii
  !         PRINT *,'hvor(ii)=',hvor(ii)
  !         print *,'a_ks(ii)=',a_ks(ii),' u_ks(ii)=',u_ks(ii)
  !         PRINT *,'l_ks(ii)=',l_ks(ii)
  !4040     print *,'v_ks(ii)=',v_ks(ii),' q_ks(ii)=',q_ks(ii)
  !      PRINT *
  !      print *,'v_ks(itrli-1)=',v_ks(itrli-1)
  !      print *,'a_ks(itrli-1)=',a_ks(itrli-1)
  !      print *,'u_ks(itrli-1)=',u_ks(itrli-1)
  !      PRINT *,'l_ks(itrli-1)=',l_ks(itrli-1)
  !      PRINT *
  !      PRINT *,'hr=',hr
  !      print *,'f(1)=',f(1),' u(1)=',u(1)
  !      print *,'actf=',actf,' actu=',actu
  !      print *,'rk(1)=',rk(1),' v(1)=',v(1)
  !      print *,'qt(1)=',qt(1)
  !      PRINT *
  !***********************************************************************

  !***********************************************************************
  !      DK 22/05/01 - no need for this variable any longer (see below)!
  !      alsum = 0.
  !***********************************************************************


  !***********************************************************************
  !     DK 17/05/01 - setting values of f and u for flow active subsect.
  actf = f (3)
  actu = u (3)
  !***********************************************************************


  !JK   SCHLEIFE FUER BERECHNUNG RECHTES VORLAND -------------------------
  rechtes_vorland: DO ii = itrre, ischr - 1

    hvor(ii) = hr - (h_ks(ii) + h_ks(ii+1)) / 2.
    !MD: passt auch fur negative WSP

    IF (b_ks(ii) .gt. 1.e-01) then
      alpha = atan (abs (h_ks (ii+1) - h_ks (ii) ) / b_ks (ii) )
    ELSE
      alpha = 0.
    ENDIF


    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'Calculation for subsection',ii,'!'
    !         PRINT *,'ii=',ii
    !         PRINT *,'hr=',hr
    !         PRINT *,'h_ks(ii-1)=',h_ks(ii-1),' h_ks(ii)=',h_ks(ii)
    !         print *,'h_ks(ii+1)=',h_ks(ii+1)
    !         IF(ii.lt.nknot-1) print *,'h_ks(ii+2)=',h_ks(ii+2)
    !         print *,'hvor(ii)=',hvor(ii)
    !         print *,'b_ks(ii)=',b_ks(ii),' alpha=',alpha
    !         PRINT *
    !***********************************************************************


    IF (ii .gt. 1) then
      IF (hr .gt. h_ks(ii-1) ) then
        h_li = h_ks (ii-1) - h_ks (ii)
      ELSE
        h_li = hr - h_ks (ii)
        !DK 10/05/01  h_ks(ii-1) changed to h_ks(ii)
      ENDIF

      a_li = a_ks (ii-1)
      aks_li = k_ks (ii-1)

    ELSE
      h_li = 0.
      a_li = 0.
      aks_li = 0.
    ENDIF

    IF (ii .lt. (nknot- 1) ) then
      IF (hr .gt. h_ks(ii+2) ) then
        h_re = h_ks (ii+2) - h_ks (ii+1)
      ELSE
        h_re = hr - h_ks (ii+1)
      ENDIF

      a_re = a_ks (ii+1)
      aks_re = k_ks (ii+1)
    !DK 10/05/01       ! these two lines are moved from the line ***
    ELSE
      h_re = 0.
      a_re = 0.
      aks_re = 0.
    ENDIF

    !DK 10/05/01       ! this is the line ***
    !DK       Deactivated 22/05/01 for KOUWEN:
    !         if (0.60*hvor(ii).lt.k_ks(ii)) then
    !             ak_mi=0.60*hvor(ii)
    !         else
    !             ak_mi=k_ks(ii)
    !         endif
    !DK      Activated 22/05/01 for KOUWEN:

    ak_mi = k_ks (ii)

    IF (hvor(ii) .lt. 1.e-06 .and. a_ks(ii) .gt. 1.e-05) then
      PRINT * , 'Fehler! Fliesstiefe Vorland inkorrekt '
      PRINT * , 'Voraussichtlich Geometriefehler.'
      PRINT * , 'Beim Entwickler melden.'
      STOP 'SUB EB2KS, da Fehler nicht behebbar.'
    ENDIF


    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'h_li=',h_li,' a_li=',a_li,' aks_li=',aks_li
    !         PRINT *,'h_re=',h_re,' a_re=',a_re,' aks_re=',aks_re
    !         print *,'h_ks(ii+1)=',h_ks(ii+1),' hvor(ii)=',hvor(ii)
    !         print *,'k_ks(ii)=',k_ks(ii),' ak_mi=',ak_mi
    !         PRINT *,'It is going to Sub LINDY!'
    !         PRINT *
    !***********************************************************************

    ibed = 3

    !***********************************************************************
    !        DK 29/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'Value before LINDY:'
    !         print *,'ibed=',ibed
    !         print *,'icwl=',icwl,' icwm=',icwm,' icwr=',icwr
    !         PRINT *
    !***********************************************************************

    CALL lindy (v_ks (ii), l_ks (ii), ax (ii), ay (ii), dp (ii),    &
     & hvor (ii), mei (ii), isener, u_ks (ii), a_ks (ii), ak_mi, a_li, &
     & a_re, h_li, h_re, aks_li, aks_re, alpha, cwr (ii), &
     & alp (ii), also (ii), anl (ii), anb (ii), vnvv (ii), cwn (ii),   &
     & if_l (ii), formbeiwert(3) )


    !***********************************************************************
    !        DK 29/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'Value after LINDY:'
    !         print *,'ibed=',ibed
    !         print *,'icwl=',icwl,' icwm=',icwm,' icwr=',icwr
    !         PRINT *
    !***********************************************************************

    !***********************************************************************
    !        DK 29/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'Values before correction:'
    !         print *,'l_ks(ii)=',l_ks(ii),' v_ks(ii)=',v_ks(ii)
    !         PRINT *
    !***********************************************************************


    !***********************************************************************
    !JK   PROGRAMMERWEITERUNG, 23. JUNI 2000, JANA KIEKBUSCH
    !JK   ------------------------------------------------------------------
    !      if (maean_ber.eq.'bwk') then

    l_ks (ii) = cm * l_ks (ii)
    v_ks (ii) = v_ks (ii) / SQRT (cm)

    !MD Abfangen von Extremwerten
    IF (l_ks(ii).le.1.e-06) THEN
      l_ks(ii) = 0.0
    END IF
    IF (v_ks(ii).le.1.e-05) THEN
      v_ks(ii) = 0.0
    END IF

    !          q_ks(ii)=v_ks(ii)*a_ks(ii)

    !***********************************************************************
    !        DK 29/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'Values after correction:'
    !         print *,'l_ks(ii)=',l_ks(ii),' v_ks(ii)=',v_ks(ii)
    !         PRINT *
    !***********************************************************************


    !***********************************************************************
    !         DK 22/05/01 - no more calculation using wetted
    !                       perimeter as the weighting factor
    !          alsum  = alsum + l_m(ii)*u_ks(ii)
    !***********************************************************************

    !      else
    !          q_ks(ii)=v_ks(ii)*a_ks(ii)

    !***********************************************************************
    !         DK 22/05/01 - no more calculation using wetted
    !                       perimeter as the weighting factor
    !          alsum  = alsum + l_ks(ii)*u_ks(ii)
    !***********************************************************************

    !      end if
    !JK   ENDE PROGRAMMERWEITERUNG
    !***********************************************************************

    q_ks (ii) = v_ks (ii) * a_ks (ii)
    qt (3) = qt (3) + q_ks (ii)


    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         print *,'q_ks(ii)=',q_ks(ii),' qt(3)=',qt(3)
    !         PRINT *
    !***********************************************************************

    !***********************************************************************
    !        DK 17/05/01 - excluding flow inactive subsections
    IF (l_ks (ii) .EQ.0.) then
      actf = actf - a_ks (ii)
      actu = actu - u_ks (ii)
    ENDIF
    !***********************************************************************

    !***********************************************************************
    !        DK 15/05/01
    !         PRINT *,'Sub EB2KS - right floodplain!'
    !         PRINT *,'Calculation for the subsection finished!'
    !         PRINT *,'It is going to the next one!'
    !         PRINT *
    !***********************************************************************

  !JK   SCHLEIFENENDE FUER BERECHNUNG RECHTES VORLAND --------------------
  END DO rechtes_vorland



  !***********************************************************************
  !     DK 22/05/01 - renaming !!!
  !      if (maean_ber.eq.'bwk') then
  !          l_ks(itrre)=l_m(itrre)
  !          v_ks(itrre)=v_m(itrre)
  !      endif
  !***********************************************************************

  !UT   FALLS FLAECHE RECHTES VORLAND GROESSER NULL
  !UT   rk = WIDERSTANDSBEIWERT, v = GESCHWINDIGKEIT, qt = ABFLUSS
  IF (f(3).gt.1.e-06 .and. qt(3).gt.1.e-06) then

    IF (actu.gt.0.and.actf.gt.0) then
      ! rk(3)=alsum/actu
      v(3) = qt(3) / actf

      !***********************************************************************
      !DK   22/05/01 - calculation l=(v_i/v)^2*r_hy/r_hy,i*l_i
      rk (3) = (v_ks (itrre) / v (3) ) **2 * (actf / actu) *  &
       & (u_ks (itrre) / a_ks (itrre) ) * l_ks (itrre)
      !***********************************************************************

    ENDIF
  ELSE
    rk (3) = 0.
    v (3) = 0.
  ENDIF

  !***********************************************************************
  !DK   Change!!! This includes also cases where only one subsection of
  !DK   floodplain is flooded, but with very small flow depth (so that the
  !DK   subsection is considered not flow active) !!!
  !***********************************************************************

  !***********************************************************************
  !     DK 22/05/01
  !      PRINT *,'Sub EB2KS - finished calc. for right floodplain!'
  !      do 6060 ii=itrre,ischr-1
  !         PRINT *,'ii=',ii
  !         PRINT *,'hvor(ii)=',hvor(ii)
  !         print *,'a_ks(ii)=',a_ks(ii),' u_ks(ii)=',u_ks(ii)
  !         PRINT *,'l_ks(ii)=',l_ks(ii)
  !6060     print *,'v_ks(ii)=',v_ks(ii),' q_ks(ii)=',q_ks(ii)
  !      PRINT *
  !      print *,'v_ks(itrre)=',v_ks(itrre)
  !      print *,'a_ks(itrre)=',a_ks(itrre)
  !      print *,'u_ks(itrre)=',u_ks(itrre)
  !      PRINT *,'l_ks(itrre)=',l_ks(itrre)
  !      PRINT *
  !      PRINT *,'hr=',hr
  !      print *,'f(3)=',f(3),' u(3)=',u(3)
  !      print *,'actf=',actf,' actu=',actu
  !      print *,'rk(3)=',rk(3),' v(3)=',v(3)
  !      print *,'qt(3)=',qt(3)
  !      PRINT *
  !***********************************************************************

  !JK   ------------------------------------------------------------------
  !JK   BERECHNUNG bf(1) UND bf(2)
  !JK   ------------------------------------------------------------------
  !UT   bf = MITWIRKENDE FLUSSCHLAUCHBREITE 1= LINKE TRENNFL., 2 = RECHTS

  !WP  70 CONTINUE  Never used

  IF (itere1 .le. 1 .and. itere2 .le. 1) then

    IF (itrli .le. ischl .and. itrre .ge. ischr) then
      bf (1) = b_hg / 2.
      bf (2) = b_hg / 2.
    ELSE IF (itrli .le. ischl) then
      bf (1) = 1.e-06
      bf (2) = b_hg
    ELSE IF (itrre .ge. ischr) then
      bf (1) = b_hg
      bf (2) = 1.e-06
    ELSE

      ! --------------------------------------
      ! if(abs(l_ks(itrli-1)) .le. 1.e-04)then
      !     if(abs(l_ks(itrre)).le.1.e-04)then
      !           vlam = 1.
      !      else
      !           vlam = 1.e+06
      !      endif
      ! elseif(l_ks(itrre) .le.1.e-04)then
      !           vlam = 0
      ! else
      !      vlam = l_ks(itrre)/l_ks(itrli-1)
      ! endif
      ! ---------------------------------------

      IF (k_ks (itrli - 1) .le. 1.e-06) then
        PRINT * , 'Fehler in Rauheitsdefinition Vorland.'
        PRINT * , 'Ueberpruefe Rauheit auf linkem Vorland.'
        STOP 'Programmabbruch in SUB PASCHE'
      ENDIF

      vlam = k_ks (itrre) / k_ks (itrli - 1)
      bf (1) = b_hg / (1. + vlam)
      bf (2) = b_hg - bf (1)
    ENDIF

  ELSE IF (abs (bf (1) ) .le.1.e-04) then

    IF (itrli.ne.1.or.ischl.lt.itrli) then
      IF (abs (l_ks (itrli - 1) ) .le.1.e-04) then
        IF (abs (l_ks (itrre) ) .le.1.e-04) then
          vlam = 1.
        ELSE
          vlam = 1.e+06
        ENDIF
      ELSE
        vlam = l_ks (itrre) / l_ks (itrli - 1)
      ENDIF
      bf (1) = b_hg / (1. + vlam)
      bf (2) = b_hg - bf (1)
    ENDIF

  ELSE IF (abs (bf (2) ) .le.1.e-04) then

    IF (itrre.ne.nknot.or.ischr.gt.itrre) then
      IF (abs (l_ks (itrli - 1) ) .le.1.e-04) then
        IF (abs (l_ks (itrre) ) .le.1.e-04) then
          vlam = 1.
        ELSE
          vlam = 0.
        ENDIF
      ELSE
        vlam = l_ks (itrre) / l_ks (itrli - 1)
      ENDIF
      bf (1) = b_hg / (1. + vlam)
      bf (2) = b_hg - bf (1)
    ENDIF

  ENDIF

  !EP     05.02.2002   Initialisierung des Fehlerflags wieder eingeführt
  if_pa = 0

  !JK   ------------------------------------------------------------------
  !JK   BERECHNUNG TEILABFLUESSE NACH PASCHE
  !JK   ------------------------------------------------------------------


  ibed = 2

  !write (*,*) 'In EB2ks. vor Aufruf von PASCHE. v_hg  = ', v_hg

  CALL pasche (nknot, iprof, hr, bf, itere2, br, qvor1, qvor2,      &
   & isener, vt_l, anl_l, anb_l, om_l, ct_l, bm_l, alt_l, &
   & h_t, vt_n, v1_hg, if_pa, formbeiwert)

  !write (UNIT_OUT_LOG,*) 'In EBKSN. Nach Aufruf von PASCHE. Linke Trennflaeche:'
  !write (UNIT_OUT_LOG, 2000) vt_l(1), anl_l(1), anb_l(1), om_l(1), ct_l(1), &
  !                         & bm_l(1), alt_l(1), h_t(1), vt_n(1), v1_hg
    
  !2000 format (1X, 'Trennfl. Geschw. vt_l = ', F10.4, /, &
  !           & 1X, 'Nachlauflaenge anl_l  = ', F10.4, /, &
  !           & 1X, 'Nachlaufbreite anb_l  = ', F10.4, /, &
  !           & 1X, 'Bewuchsparam. om_l    = ', F10.4, /, &
  !           & 1X, 'Hilfsparam. ct_l      = ', F10.4, /, &
  !           & 1X, 'Mitwirk. Vorland B.   = ', F10.4, /, &
  !           & 1X, 'Lambda Trennflaeche   = ', F10.4, /, &
  !           & 1X, 'Hoehe Trennfl.        = ', F10.4, /, &
  !           & 1X, 'Trennfl. Geschw. vt_n = ', F10.4, /, &
  !           & 1X, 'Mittl. Geschw. FlussS = ', F10.4)
  
  ! write (UNIT_OUT_LOG,*) 'In EBKSN. Nach Aufruf von PASCHE. alt_l(2)  = ', alt_l(2)

  !**********************************************************************
  !JK  PROGRAMMERWEITERUNG, 23. JUNI 2000, JANA KIEKBUSCH
  !JK  ------------------------------------------------------------------
  !      if (maean_ber.eq.'bwk') then
  l_hg = cm * l_hg
  v_hg = v_hg / SQRT (cm)
  !          q_hg  = v_hgm*a_hg
  !          rk(2) = l_hgm
  !          v(2)  = v_hgm
  !      else
  !          q_hg  = v_hg*a_hg
  !          v(2)  = v_hg
  !          rk(2) = l_hg
  !      end if
  !JK  ENDE PROGRAMMERWEITERUNG
  !**********************************************************************


  !write (*,*) 'In EBKSN. nach Aufruf von PASCHE. v_hg  = ', v_hg

  q_hg = v_hg * a_hg

  v (2) = v_hg
  rk (2) = l_hg


  qt (1) = qt (1) + qvor1
  qt (3) = qt (3) + qvor2
  qt (2) = q_hg

  !write (*,*) 'In EBKSN. QT(2) = ', qt(2)

  ra (2) = r_hg

  psi = 0.

  !JK   BERECHNUNG psi FUER LINKES VORLAND

  DO ii = ischl, itrli - 1

    IF (l_ks (ii) .gt. 1.e-6) then
      !JK  ERWEITERUNG FUER MAEANDRIERUNG
      !    if (maean_ber.eq.'bwk') then
      !      pst = a_ks(ii)*((r_ks(ii)/l_m(ii))**0.5)
      !JK  ENDE ERWEITERUNG
      !    else
      !UT  OHNE ERWEITERUNG
      pst = a_ks (ii) * ( (r_ks (ii) / l_ks (ii) ) **0.5)
      !    endif
      !UT  MIT UND OHNE ERWEITERUNG
      psi = psi + pst
    ENDIF
  END DO

  !JK   BERECHNUNG psi FUER RECHTES VORLAND
  DO ii = itrre, ischr - 1
    IF (l_ks (ii) .gt. 1.e-6) then
      !JK  ERWEITERUNG FUER MAEANDRIERUNG
      !    if (maean_ber.eq.'bwk') then
      !      pst=a_ks(ii)*((r_ks(ii)/l_m(ii))**0.5)
      !JK  ENDE ERWEITERUNG
      !    else
      !UT  OHNE ERWEITERUNG
      pst = a_ks (ii) * ( (r_ks (ii) / l_ks (ii) ) **0.5)
      !    endif
      !UT  MIT UND OHNE ERWEITERUNG
      psi = psi + pst
    ENDIF
  END DO

  !JK   BERECHNUNG psi FUER FLUSSSCHLAUCH
  !JK        ERWEITERUNG FUER MAEANDRIERUNG
  !WP 170 CONTINUE ! Never Used
  !      if (maean_ber.eq.'bwk') then
  !               psi=psi+a_hg*((r_hg/l_hgm)**0.5)
  !JK        ENDE ERWEITERUNG
  !           else
  psi = psi + a_hg * ( (r_hg / l_hg) **0.5)
  !           endif

  !JK   BERECHNUNG RAUHIGKEITSHOEHE rg
  rg = psi * sqrt (8. * g)

  IF (rg_alt .le. 1.e-04) then
    psi = 1. / (8. * g * psi * psi)
  ELSE
    psi_alt = rg_alt / sqrt (8. * g)
    ! psi=1./(8.*g*(psi+psi_alt)*(psi+psi_alt))*2.
    psi = 1. / (8. * g * psi * psi)
  ENDIF

  isenen = q * q * psi

  !JK   BERECHNUNG GESAMTABFLUSS qgs
  qgs = qt (1) + qt (2) + qt (3)

  IF (itere2 .gt. 20) then
    !**   SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, 9006) itere2, difi
    9006 FORMAT (1X, 'Warnung! Keine Konvergenz bei der Berechnung des Gefaelles!', /, &
               & 1X, 'Nach ',I3,' Iterationen betraegt der Fehler :', F10.4)
    GOTO 181
  ENDIF

  difi = abs (isener - isenen) / isener

!WP Ende DO 15-Schleife--------------------------------------------------------
15 CONTINUE
!WP ---------------------------------------------------------------------------



!JK   FEHLERWARNUNG LINKES VORLAND
181 CONTINUE

DO i = ischl, itrli - 1
  IF (if_l (i) .ne.0) then
    !**   SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, 9001) if_l (i) , i
    9001 format (1X, 'Warnung! w- ',I8,' am Profilpunkt',i3, /, &
               & 1X, 'bei der Vorlandberechnung links.')
  ENDIF
END DO

!JK   FEHLERWARNUNG RECHTES VORLAND
DO i = itrre, ischr - 1
  IF (if_l (i) .ne.0) then
    !**      SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, 9000) if_l(i), i
    9000 format (1X, 'Warnung! w- ', I8, 'am Profilpunkt' ,I3, /, &
               & 1X, 'bei der Vorlandberechnung rechts.')
  ENDIF
END DO

!JK   FEHLERWARNUNG SOHLBERECHNUNG
IF (if_pa .ne. 0) then

  !**      SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(''Warnung! w- '',i8,'' bei der Sohlberechnung'')') if_pa

ENDIF

uges = u (1) + u (2) + u (3)

IF (uges .gt. 0) then

  rhges = fges / uges
  vges = q / fges
  alges = 8 * g * rhges * isenen / vges / vges

  !IF (i_typ_flg.eq.'pasche') then
  IF (FLIESSGESETZ == 'DW_M_FORMBW') THEN

    factor1 = - 1. / 2.03 / sqrt (alges)
    re = vges * 4. * rhges / nue
    factor2 = 4.4 / sqrt (alges) / re

  ELSE

    factor1 = - 1. / 2. / sqrt (alges)
    isenk = isenen * (1. + isenen**2.) **0.5
    dhy1 = 4. * rhges
    factor2 = nue * 2.51 / (dhy1 * (2. * g * dhy1 * isenk) **0.5)

  ENDIF

  akges = 3.71 * 4. * rhges * (10.** (factor1) - factor2)

ENDIF

!UT   SETZEN VON ZAEHLER(on) UND NENNER(un) ENERGIESTROMBEIWERT
!UT   VOR DER BERECHNUNGSSCHLEIFE
phion = 0.0
phiun = 0.0

!HB   *****************************************************************
!HB   26.11.2001 H.Broeker
!HB   --------------------
!HB   Erweiterte Programmierung des Energiestrom- und Boussinesq-
!HB   Beiwertes befindet sich unterhalb dieser Deaktivierung
!HB   *****************************************************************
!HB___Beginn Deaktivierung
!HB
!JK   BER. ENERGIESTROMBEIWERT LINKES VORLAND, FORMEL 14, BWK, S.19
!210   do 250 ii=ischl,itrli-1
!           if (l_ks(ii).gt.1.e-6) then
!           phion=phion+a_ks(ii)*((r_ks(ii)/l_ks(ii))**1.5)
!           phiun=phiun+a_ks(ii)*((r_ks(ii)/l_ks(ii))**0.5)
!           endif
!250   continue
!JK   BER. ENERGIESTROMBEIWERT RECHTES VORLAND, FORMEL 14, BWK, S.19
!      do 251 ii=itrre,ischr-1
!           if (l_ks(ii).gt.1.e-6) then
!           phion=phion+a_ks(ii)*((r_ks(ii)/l_ks(ii))**1.5)
!           phiun=phiun+a_ks(ii)*((r_ks(ii)/l_ks(ii))**0.5)
!           endif
!251   continue
!JK   BERECHNUNG ENERGIESTROMBEIWERT GESAMT
!UT   DURCH ADDITION DER ANTEILE DES FLUSSCHLAUCHES
!260   phion=phion+a_hg*((r_hg/l_hg)**1.5)
!      phiun=phiun+a_hg*((r_hg/l_hg)**0.5)
!HB
!HB___Ende Deaktivierung
!HB   *****************************************************************

!HB   Setzen der Summen vor Berechnungsbeginn auf Null

phi_o_li = 0
psi_o_li = 0
phi_u_li = 0
phi_o_re = 0
psi_o_re = 0
phi_u_re = 0
phi_o_fl = 0
psi_o_fl = 0
phi_u_fl = 0
a_vor_li = 0
a_vor_re = 0
gesamt_a(pn_alpha) = 0
alpha_E = 0
alpha_I = 0
psion = 0
phiun = 0
phion = 0

!HB   Berechnung der einzelnen Summen des linken Vorlandes des Energie-
!HB   strombeiwert (ohne Flaeche A) für Zaehler und Nenner
!HB   nach FORMEL 14, BWK1/99, S.19 (phi_o_li und phi_u_li)
!HB   sowie Berechnung der einzelnen Summen des linken Vorlandes des
!HB   Boussinesq-Beiwertes (ohne Flaeche A) fuer Zaehler und Nenner
!HB   nach FORMEL 15, BWK1/99, S.19 (psi_o_li)

!WP 210 CONTINUE ! Never Used
ii = 0
! PRINT *,'Linkes Vorland'
! PRINT *,'ischl:',ischl
! PRINT *,'itrli-1:',itrli-1
DO ii = ischl, itrli - 1
  IF (l_ks(ii).gt.1.e-3 .and. u_ks(ii).gt.1.e-3 .and. a_ks(ii).gt.1.e-3) then
    r_ks(ii) = a_ks(ii) /u_ks(ii)
    !HB  Zaehler Energiestrombeiwert
    phi_o_li = phi_o_li + (a_ks(ii) * ((r_ks(ii) / l_ks(ii)) ** 1.5))
    !HB  Nenner Energiestrombeiwert und Boussinesq-Beiwert
    phi_u_li = phi_u_li + (a_ks(ii) * ((r_ks(ii) / l_ks(ii)) ** 0.5))
    !HB  Zaehler Boussinesq-Beiwert
    psi_o_li = psi_o_li + (a_ks(ii) * (r_ks(ii) / l_ks(ii) ))

    ! PRINT *,'ii:',ii
    ! PRINT *,'a_ks(ii):',a_ks(ii)
    ! PRINT *,'r_ks(ii):',r_ks(ii)
    ! PRINT *,'l_ks(ii):',l_ks(ii)
    ! PRINT *,'r_ks(ii)/l_ks(ii):',r_ks(ii)/l_ks(ii)
    ! PRINT *,'phi_o_li:',phi_o_li
    ! PRINT *,'phi_u_li:',phi_u_li
    !HB  Uebergabe der durchstroemten Flaechen um Summe der
    !HB  Einzelflaechen zu erhalten (fuer Beiwertberechnung
    a_vor_li = a_vor_li + a_ks(ii)
  ENDIF
END DO


!HB   Berechnung der einzelnen Summen des rechten Vorlandes des Energie-
!HB   strombeiwert (ohne Flaeche A) fuer Zaehler und Nenner
! PRINT*,'Rechtes Vorland'
! PRINT*,'itrre:',itrre
! PRINT*,'ischr - 1:',ischr - 1
ii = 0
DO ii = itrre, ischr - 1
  IF (l_ks (ii) .gt.1.e-3 .and. u_ks(ii).gt.1.e-3 .and. a_ks(ii).gt.1.e-3) then
    r_ks(ii) = a_ks(ii) /u_ks(ii)
    !HB  Zaehler Energiestrombeiwert
    phi_o_re = phi_o_re+ (a_ks(ii) * ((r_ks(ii) / l_ks(ii)) ** 1.5))
    !HB  Nenner Energiestrombeiwert und Boussinesq-Beiwert
    phi_u_re = phi_u_re+ (a_ks(ii) * ((r_ks(ii) / l_ks(ii)) ** 0.5))
    !HB  Zaehler Boussinesq-Beiwert
    psi_o_re = psi_o_re+ (a_ks(ii) * (r_ks(ii) / l_ks(ii)))

    ! PRINT*,'ii:',ii
    ! PRINT*,'a_ks(ii):',a_ks(ii)
    ! PRINT*,'r_ks(ii):',r_ks(ii)
    ! PRINT*,'l_ks(ii):',l_ks(ii)
    ! PRINT*,'r_ks(ii)/l_ks(ii):',r_ks(ii)/l_ks(ii)
    ! PRINT*,'phi_o_re:',phi_o_re
    ! PRINT*,'phi_u_re:',phi_u_re
    !HB  Uebergabe der durchstroemten Flaechen um Summe der
    !HB  Einzelflaechen zu erhalten (fuer Beiwertberechnung)
    a_vor_re = a_vor_re + a_ks(ii)
  ENDIF
END DO

!HB   Berechnung Zaehler und Nenner des Energiestrombeiwert fuer
!HB   Flusschl. nach FORMEL 14, BWK1/99, S.19 (phi_o_fl und phi_u_fl)
!HB   sowie Berechnung Zaehler und Nenner des Boussinesq-Beiwertes fuer
!HB   Flusschlauch nach FORMEL 15, BWK1/99, S.19 (psi_o_fl)
!WP 260 CONTINUE Never Used

r_hg = ABS(a_hg) /u_hg

phi_o_fl = ABS(a_hg) * ((r_hg / l_hg) **1.5)
phi_u_fl = ABS(a_hg) * ((r_hg / l_hg) **0.5)
psi_o_fl = ABS(a_hg) * (r_hg / l_hg)
! PRINT*,'Flusschlauch'
! PRINT*,'Flaeche a_hg:',a_hg
! PRINT*,'r_hg:',r_hg
! PRINT*,'l_hg:',l_hg
! PRINT*,'r_hg/l_hg:',r_hg/l_hg
! PRINT*,'phi_o_fl:',phi_o_fl
! PRINT*,'phi_u_fl:',phi_u_fl

!HB   Berechnung Summe des Zaehlers fuer:
!HB   (1.) weitere Berechnung von Verlusthoehe
!HB   (2.) Energiestrombeiwert
phion = phi_o_li + phi_o_re + phi_o_fl

!HB   Berechnung Summe des Nenners fuer:
!HB   (1.) weitere Berechnung von Verlusthoehe
!HB   (2.) Energiestrombeiwert
!HB   (3.) Boussinesq-Beiwert
phiun = phi_u_li + phi_u_re + phi_u_fl
psion = psi_o_li + psi_o_re + psi_o_fl

! PRINT*,'Zaehler und Nenner'
! PRINT*,'phion:',phion
! PRINT*,'phiun:',phiun
! PRINT*,'psion:',psion

!HB   Gesamte durchstroemte Flaeche
gesamt_a(pn_alpha) = a_vor_li + a_vor_re + ABS(a_hg)
! PRINT*,'Gesamtflaeche',gesamt_a

!HB   -----------------------------------------------------------------
!HB   Berechnung des Energiestrombeiwertes und des Boussinesq-Beiwertes
!HB   -----------------------------------------------------------------

!HB   Energiestrombeiwert, BWK1/99, Formel 14, Seite 19
!HB   Coriolis-Beiwert in Abhaengigkeit von der Profilnummer
alpha_E = (gesamt_a(pn_alpha) **2.D0) * phion / (phiun**3.D0)
alpha_EW(pn_alpha) = alpha_E

!HB   Impulsstrombeiwert in Abhaengigkeit von der Profilnummer
!HB   Boussinesq-Beiwert, BWK1/99, Formel 15, Seite 19
alpha_I = gesamt_a(pn_alpha) * psion / (phiun**2.D0)
alpha_IW(pn_alpha) = alpha_I

! PRINT*,'Impulsstrombeiwert:',alpha_I
! PRINT*,'Energiestrombeiwert:',alpha_E
! PRINT*,'Beiwert in abh. von Profilnummer:',alpha_EW(pn_alpha
! PRINT*,'pn_alpha:',pn_alpha
! PRINT*,'st_alpha:',st_alpha

!HB   Ende der Erweiterung
!HB   ****************************************************************

!JK   BERECHNUNG VERLUSTHOEHE ANLEHNUNG FORMEL 6, S.15 BWK?, UT
!UT   => hv = (phion/(phiun**3)) *q*q / (9.81*2.)     ???
!HB   Der Verlusthoehe wird die Energiebilanz zugrundegelegt,
!HB   so dass der Nenner in dritter Potenz vorliegt
   hv = phion / phiun**3 * q * q / g / 2.

!MD  NEU: NEU  04.07.2007
!MD  Durch diese Veraenderung wird weniger haeufig die Grenztiefe angesetzt
!MD  BERECHNUNG VERLUSTHOEHE ANLEHNUNG FORMEL 9, S.17 BWK
!MD   => hv = (1/(phiun**2)) *q*q / (9.81*8.)
!MD    hv = (q*q)/(phiun**2.D0) / g / 8.0D0

!MD  hv = (vges**2.) / g / 2.0D0
!WRITE (UNIT_OUT_TAB, '('' hv(eb2ks)  ='',f8.4)') hv

iergeb = 0


!UT   WIE KANN AN DIESER STELLE iergeb UNGLEICH NULL SEIN?, UT, 28.08.00
IF (iergeb.ne.0) then

  DO i1 = ischl, ischr
    IF (i1.lt.itrli.or.i1.ge.itrre) then
      !**    SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, '(9f8.4)') cwr (i1) , alp (i1), &
       & also (i1) , l_ks (i1) , v_ks (i1) , anl (i1) , anb (i1) , &
       & vnvv (i1) , cwn (i1)
    ENDIF
  END DO

  DO i1 = 1, 2
    !**   SCHREIBEN IN KONTROLLFILE
    IF (i1.eq.1) then
      WRITE (UNIT_OUT_LOG, '(12f8.4)') vt_l (i1) , anl_l (i1) , anb_l (&
       & i1) , om_l (i1) , ct_l (i1) , bm_l (i1) , bf (i1) , alt_l &
       & (i1) , h_t (i1) , l_hg, v1_hg, vt_n (i1)
    ELSE
      WRITE (UNIT_OUT_LOG, '(12f8.4)') vt_l (i1) , anl_l (i1) , anb_l (&
       & i1) , om_l (i1) , ct_l (i1) , bm_l (i1) , bf (i1) , alt_l &
       & (i1) , h_t (i1) , l_hg, v_hg, vt_n (i1)
    ENDIF
  END DO

!**   ENDIF (iergeb.ne.0)
ENDIF

END SUBROUTINE eb2ks                                                                              


