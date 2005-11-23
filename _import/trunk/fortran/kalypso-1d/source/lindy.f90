!     Last change:  WP   11 Jul 2005    5:41 pm
!--------------------------------------------------------------------------
! This code, lindy.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - lindy
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

SUBROUTINE lindy (vxmvor, lamvog, ax, az, dp, h_mi, meil, is, &
     & u_mi, a_mi, rau_mi, a_li, a_re, h_li, h_re, rau_li, rau_re, alpha, &
     & iuerr, lein, cwr, lamv, aln_mi, axn1, b05, vv2, cwn, ifehl, ks_mi, fbw)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE LINDY                                                  
!**                                                                     
!**   ALLGEMEINE BESCHREIBUNG:                                          
!**   ------------------------                                          
!**   DIESES PROGRAMM BERECHNET DEN WIDERSTANDSBEIWERT IM VORLAND, DER  
!**   SICH ZUSAMMENSETZT AUS DEM WIDERSTAND AUS BEWUCHS UND DEM WIDER-  
!**   STAND AUS DER SOHLRAUHEIT.                                        
!**                                                                     
!**   LITERATUR: BWK-MERKBLATT, Teil 1, SEPTEMBER 1999                  
!**
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**   alpha        - NEIGUNGSWINKEL SOHLE, BOESCHUNG QUER ZUR FLIESS-   
!**                  RICHTUNG                                           
!**   a_mi/li/re   - FLAECHE DES TEILABSCHNITTES DES VORLANDES [m]      
!**   ax,az,dp     - BEWUCHSPARAMETER                                   
!**   b05          -                                                    
!**   dp           - BEWUCHSPARAMETER                                   
!**   h_mi/li/re   - WASSERSPIEGELHOEHE IM VORLAND [M]                  
!**   ifehl                                                             
!**   is           - GEFAELLE [-]                                       
!**   ks_mi        - RAUHEIT DES TEILABSCHNITTES DES VORLANDES [m]      
!**   lamvog       - WIDERSTANDSBEIWERT (LAMBDA) DES TEILABSCHNITTES    
!**                  DES VORLANDES [-]                                  
!**   lamv         - WIDERSTANDSBEIWERT DURCH BEWUCHS [m]               
!**   rau_mi/li/re - KORRIGIERTE RAUHEIT DES TEILABSCHNITTES DES        
!**                  VORLANDES [m]                                      
!**   u_mi         - BENETZTER UMFANG DES TEILABSCHNITTES DES           
!**                  VORLANDES [m]                                      
!**   vxmvor       - MITTLERE GESCHWINDIGKEIT IM VORLAND [m/s]          
!**                                                                     
!**   DIREKTE AUSGABE                                                   
!**   ---------------                                                   
!**   LAMDA-VORLAND UND GESCHWINDIGKEIT VORLAND                         
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**   a_mi    - durchströmte Fläche eines Teilabschnittes des Vorlandes 
!**   ak_li   - Rauheit des linken Absturzes                            
!**   ak_re   - Rauheit des rechten Absturzes                           
!**   ak1_li  - abgeminderte Rauheit des linken Absturzes               
!**   ak1_mi  - abgeminderte Rauheit des mittleren Abschnittes          
!**   ak1_re  - abgeminderte Rauheit des rechten Absturzes              
!**   al_li   - Widerstandsbeiwert des linken Absturzes                 
!**   al_mi   - Widerstandsbeiwert des mittleren Abschnittes            
!**   al_re   - Widerstandsbeiwert des rechten Absturzes                
!**   aln_li  - Widerstandsbeiwert des linken Absturzes                 
!**   aln_mi  - Widerstandsbeiwert des Teilabschnittes                  
!**             des Vorlandes aus der Rauheit                           
!**   aln_re  - Widerstandsbeiwert des rechten Absturzes                
!**   alpha   - Böschungswinkel                                         
!**   ax      - Bewuchsparameter                                        
!**   axj1    - Nachlauflänge                                           
!**   axn1    - Nachlauflänge                                           
!**   ay      - Bewuchsparameter                                        
!**   az      - Parameter des Verbauverhältnisses                       
!**   b05     - Nachlaufbreite [m]                                      
!**   cc      - Verbauverhältnis                                        
!**   cwn     - Formwiderstandsbeiwert eines Zylinders in einer Gruppe  
!**   cwr     - Formwiderstandsbeiwert eines Zylinders in einer Gruppe  
!**   cwun    - Formwiderstandsbeiwert eines einzelnen Zylinders        
!**   dcw     - Schwerewelle                                            
!**   delhyd  - Grenzbedingung                                          
!**   dellam  - Grenzbedingung                                          
!**   delrev  - Grenzbedingung                                          
!**   dh_li   - hydraulischer Durchmesser des linken Absturzes          
!**   dh_mi   - hydraulischer Durchmesser des mittleren Abschnittes     
!**   dh_re   - hydraulischer Durchmesser des rechten Absturzes         
!**   dhn_li  - hydraulischer Durchmesser des linken Absturzes          
!**   dhn_mi  - hydraulischer Durchmesser des mittleren Abschnittes des 
!**             Vorlandes                                               
!**   dhn_re  - hydraulischer Durchmesser des rechten Absturzes         
!**   dhyd    - hydraul. Durchmesser des Teilabschnittes des Vorlandes  
!**   dhyg    - hydraul. Durchmesser des Teilabschnittes des Vorlandes  
!**   dp      - Bewuchsparameter                                        
!**   fr1     - Froud-Zahl                                              
!**   fr2     - Froud-Zahl                                              
!**   fr2min  - minimale Froud-Zahl                                     
!**   g       - Erdbeschleunigung                                       
!**   h_li    - Wasserspiegelhöhe am linken Absturz                     
!**   h_mi    - mittlere Wsp-höhe des Teilabschnittes des Vorlandes     
!**   h_re    - Wasserspiegelhöhe am rechten Absturz                    
!**   il_fr30 - = 1                                                     
!**   ifehl   -                                                         
!**   is      - Energiegefälle                                          
!**   itcwr   - ANZAHL DER ITERATIONEN                                  
!**   itcwm   - 40. Maximum                                             
!**   iter    - ITERATIONSCHRITTE FUER FROUDZAHL fr1 und fr2            
!**   iter272 -                                                         
!**   iterm   - Parameter = 20                                          
!**   lamv    - Widerstandsbeiwert des Teilabschnittes des              
!**             Vorlandes aus Bewuchs                                   
!**   lamvog  - Widerstandsbeiwert des Teilabschnittes des Vorlandes    
!**   nue     - Viskosität                                              
!**   rau_mi  - Rauheit des mittleren Abschnittes                       
!**   re_li   - Reynoldszahl des linken Absturzes                       
!**   re_mi   - Reynoldszahl des Teilabschnittes des Vorlandes          
!**   re_mi   - Reynoldszahl des mittleren Abschnittes                  
!**   re_re   - Reynoldszahl des rechten Absturzes                      
!**   ren_li  - Reynoldszahl des linken Absturzes                       
!**   ren_mi  - Reynoldszahl des mittleren Abschnittes                  
!**   ren_re  - Reynoldszahl des rechten Absturzes                      
!**   revor   - Reynoldszahl des Vorlandes                              
!**   u_mi    - benetzter Umfang des Teilabschnittes des Vorlandes      
!**   uk_li   - benetzter Umfang des linken Absturzes                   
!**   uk_re   - benetzter Umfang des rechten Absturzes                  
!**   vv2     - relative Anströmgeschwindigkeit                         
!**   vxmvor  - mittlere Fließgeschwindigkeit des Vorlandes             
!**   ya      - Wasserstandsverhältnis                                  
!**   yn      - Wasserstandsverhältnis                                  
!**   ystern  - Wasserstandsverhältnis                                  
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**     - korrigi       (wenn gewuenscht)                               
!**     - fuent_l       (wenn gewuenscht)                               
!**     - cwsub(a,b,c)  a = geschwindigkeit                             
!**                     b = stabdurchmesser                             
!**                     c = cw_unendlich (Ergebniss)                    
!**                                                                     
!***********************************************************************
                                                                        
                                                                        
! ------------------------------------------------------------------
! geschrieben :                            16.11.1984 b.schwamborn
! geaendert   :                            28.03.1985 juergen kuck
!                                          17.03.1986 l.kirschbauer
! erweitert   :                            16.06.2000 jana kiekbusch
! kommentiert :                            24.08.2000 ulf teschke
! angepasst   :                            20.04.2005 Wolf Ploeger
! ------------------------------------------------------------------
                                                                        
                                                                        
! ------------------------------------------------------------------
! Vereinbarungsteil
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE KONSTANTEN

! Calling variables
REAL, INTENT(OUT) :: vxmvor
REAL, INTENT(OUT) :: lamvog
REAL, INTENT(IN)  :: fbw                ! Formbeiwert fuer LAMBDA-Berechnung

INTEGER, parameter :: iterm = 20

REAL :: GET_CW_UN
REAL :: GET_A_NL
REAL :: GET_LAMBDA, rhy
                                                                        
REAL :: lamv, is
                                                                        
!**   ZUSATZ FUER ERWEITERUNG VON JANA KIEKBUSCH, 16.06.00 -------------
!      CHARACTER*3 ber_art                                              
REAL ks_mi
                                                                        
!**   ZUSATZ FUER KORREKTUR VON JANA KIEKBUSCH, 23.06.00 ---------------
!      CHARACTER*1 korrekt                                              
!JK   ENDE ZUSATZ-------------------------------------------------------
                                                                        
!***********************************************************************
! DK, 21/06/01 - Stiffness parameter in Kouwen procedure!
REAL meil
!***********************************************************************
                                                                        
!***********************************************************************
! DK, 21/06/01 - Parameters of bottom roughness type and grass type!
CHARACTER(4) tbr1, tbr2, tbr3, tgr1, tgr2, tgr3, tbedr, tgrar
COMMON / rough / tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
!***********************************************************************
                                                                        
!***********************************************************************
! DK, 07/06/01 - Parameters for printing warnings and comments!
COMMON / pri / ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
!***********************************************************************
                                                                        

! -----------------------------------------------------------------------
! Initialisieren
! -----------------------------------------------------------------------
lamvog = 0.0
vxmvor = 0.0


!WP 07.05.2004
IF (ibed.eq.1) then
  ! linkes Vorland
  tbedr = tbr1
  tgrar = tgr1
ELSEIF (ibed.eq.3) then
  ! rechtes Vorland
  tbedr = tbr3
  tgrar = tgr3
ELSE
  ! Flussmitte
  tbedr = tbr2
  tgrar = tgr2
ENDif
!WP 07.05.2004


!***********************************************************************
!     DK, 15/05/01
!      PRINT *,'Sub LINDY!'
!      print *, 'tbr1: ', tbr1, '  tbr2: ', tbr2, '  tbr3: ', tbr3
!      print *,'Data introduced into LINDY:'
!      print *,'v=',vxmvor,' lambda=',lamvog
!      print *,'ax=',ax,' az=',az,' dp=',dp
!      print *,'h_mi=',h_mi,' a_mi=',a_mi,' u_mi=',u_mi,' rau_mi=',rau_m
!      print *,'h_li=',h_li,' a_li=',a_li,' rau_li=',rau_li
!      print *,'h_re=',h_re,' a_re=',a_re,' rau_re=',rau_re
!      PRINT *,'cwr=',cwr,' lamv=',lamv,' aln_mi=',aln_mi
!      PRINT *,'Beginning calculation in LINDY!'
!      PRINT *
!***********************************************************************

!***********************************************************************
!     DK, 31/05/01
!      PRINT *,'Entered Sub LINDY!'
!      PRINT *,'ibed=',ibed,' tbedr=', tbedr
!      PRINT *,'icwl=',icwl,' ikol=',ikol
!      PRINT *,'icwm=',icwm,' ikom=',ikom,' ifum=',ifum
!      PRINT *,'icwr=',icwr,' ikor=',ikor
!      PRINT *,'Beginning calculation in LINDY!'
!      PRINT *
!***********************************************************************


! ------------------------------------------------------------------
! Programmanfang
! ------------------------------------------------------------------

!UT   Abfrage ob Flaeche a_mi Teilabschnitt Vorland groesser Null
!UT   dann setzen von lamda und Fliessgeschw. null im Vorland
!UT   sonst hydraul. Durchmesser Vorland berechnen
IF (abs (a_mi) .le.1.e-02) then
  vxmvor = 0.0
  lamvog = 0.0
  RETURN
ENDIF

!***********************************************************************
!DK   16/05/01 - IMPORTANT NOTE !!!
!DK   In case of flat floodplain and small water depth, a_mi can be < 0.
!DK   although u_mi is considerably greater than zero! In this case,
!DK   a significant part of floodplain is considered frictionless (lamvo
!DK   and lambda for the floodplain is not calculated properly (it is sm
!DK   than it should be) !
!DK   Example: for h_mi=0.03 m and slope of the subsection 1:20,
!DK   a_mi=0.009 (<0.01) and u_mi=0.6 m !!!
!DK   For that reason, the total wetted area and perimeter in Sub EBKSN
!DK   recalculated by subtracting a_mi and u_mi of flow inactive subsect
!***********************************************************************

dhyd = 4. * a_mi / u_mi

!UT   aln_mi  - Widerstandsbeiwert des Teilabschnittes
!UT             des Vorlandes aus der Rauheit
aln_mi = 0.09

!***********************************************************************
!     DK, 09/05/01 - calculate aln_mi instead of setting the value to 0.
!      ak1_mi=rau_mi
!      if(rau_mi.gt.0.60*dhyd/4.) ak1_mi=0.60*dhyd/4.
!      aln_mi=(1./(-2.03*alog10(ak1_mi/dhyd/3.71)))**2
!***********************************************************************

!UT   falls Bewuchs vorliegt Berechnung Widerstandsbeiwert Vorland
!UT   dp    - Bewuchsparameter
!UT   alpha - Boeschungswinkel
!UT   lamv  - aus Bewuchs
!UT   lamvog- komplett
IF (abs (dp - 0.0) .gt.1.e-04) then
  lamv = 4. * cwn * h_mi * dp / ax / az * cos (alpha)
ELSE
  lamv = 0.
ENDIF

!UT   Berechnung von Lamda_Vorland, Formel 11, BWK, S.18
lamvog = aln_mi + lamv
!UT   Berechnung Geschw. Vorland, Formle 6, BWK, S.15
vxmvor = (2. * g * is * dhyd / lamvog) **0.5
!                                /* anfangswert schleife 4


!***********************************************************************
!     DK, 16/05/01 - added:
re_mi = vxmvor * dhyd / nue
!***********************************************************************


!***********************************************************************
!     DK, 15/05/01
!      PRINT *,'Sub LINDY!'
!      PRINT *,'aln_mi=',aln_mi,' lamv=',lamv,' lamvog=',lamvog
!      print *,'dhyd=',dhyd,' vxmvor=',vxmvor,' re_mi=',re_mi
!      PRINT *
!***********************************************************************


!     schaetzen des rechnerischen formwiderstandsbeiwertes cwr eines
!     kreiszylinders unter mehreren.(anfangswert) :
!                                 /* anfangswert
cwn = 1.
cwr = 0.

!**   axn1 - Nachlauflänge, ax - Bewuchsparameter
axn1 = 2. * ax

! Iteration bis zum endkriterium cwn=cwr:
! ---------------------------------------

!UT   ANZAHL DER ITERATIONEN BESTIMMUNG cwr
itcwr = 0
itcwm = 40




! MAIN DO 3000-Loop-------------------------------------------------------------
DO 3000 WHILE(abs (cwr - cwn) .gt.1.e-02)

  il3000 = 0
  il_fr30 = 0

  !UT   Berechne Anzahl der Iterationsschritte
  itcwr = itcwr + 1


  !***********************************************************************
  !     DK, 15/05/01
  !      PRINT *,'Sub LINDY - entered the cycle for cwr!'
  !      PRINT *,'itcwr=',itcwr
  !      PRINT *
  !***********************************************************************


  !UT   FALLS KEIN BEWUCHSPARAMETER ax VORLIEGT, ALLES NULL
  IF (abs (ax - 0.0) .le.1.e-04) then

    cwr = 1.0
    lamv = 0.0

    !UT   u_mi - benetzter Umfang des Teilabschnittes des Vorlandes
    !UT   FALLS u_mi VORLIEGT RECHNUNG, SONST ALLES NULL UND ENDE DER SUB
    IF (u_mi.ge.1.e-4) then
      !UT  FORMEL 6, S.15, BWK
      vxmvor = (2. * g * dhyd * is / lamvog) **0.5
      re_mi = vxmvor * dhyd / nue
    ELSE
      vxmvor = 0.0
      lamvog = 0.0
      !  dhyd   = 0.0
      !  re_mi  = 0.0
      !***********************************************************************
      !DK   These two values are not needed by EB2KSN and hence deactivated!
      !***********************************************************************
      RETURN
    ENDIF

  !UT   ELSE ZU (abs(ax-0.0).le.1.e-04)
  ELSE

    !UT      MITTELUNG NEUER UND ALTER CW-WERT
    cwr = (cwn + cwr) / 2.
    !UT      BWK, Formel 27, S.28, WARUM az STATT ay ????, 24.08.2000
    lamv = 4. * cwr * h_mi * dp / ax / az * cos (alpha)

    !UT MIT GESCHW. UND BEWUCHSPARAMETER WIRD DER CW-WERT BERECHNET
    !UT IN ANLEHNUNG FORMEL 30, BWK, S.30
    cwun = GET_CW_UN (vxmvor, dp, nue)

    !WP Iterative Berechnung der Nachlauflaenge axn1
    !WP Formel 29a, S.30, BWK Merkblatt 2000
    axj1 = GET_A_NL (cwun, dp, is, vxmvor, axn1, g)
    axn1 = axj1

    ! BERECHNUNG NACHLAUFBREITE NACH FORMEL 29b, BWK, S.30
    b05  = GET_A_NB (cwun, dp, axn1)

    ! BERECHNUNG REL. ANSTROEMGESCHW. NACH FORMEL 29, BWK, S.30
    vv2  = GET_REL_V_AN (axn1, ax, b05, az)


    ! Iteration bis zum endkriterium fr1=fr2 :
    ! ----------------------------------------
    ystern = 1.0 - 1.e-06
    yn = ystern
    iter = 0
    ya = 0.9

    !UT      FORMEL BWK, S.30, MITTE
    fr1 = vxmvor / (g * h_mi) **.5
    !UT      FORMEL 31A, BWK, S.30,
    fr2 = (ystern * (ystern**2 - 1.) / (2. * (ystern - az / (az - dp) ) ) ) **.5

    2500 CONTINUE
    !                                               /* schleife 3


    IF (fr1.gt.fr2) then

      !UT          ZAEHLEN DER ITERATIONSSCHRITTE FUER FROUDZAHL
      iter = iter + 1
      fr2 = (ystern * (ystern**2 - 1.) / (2. * (ystern - az / (az - dp) ) ) ) **.5
      !UT          BERECHNE VERBAUVERHAELTNIS
      cc = az / (az - dp)

      IF (abs (fr2) .lt.1.e-06) then
        il_fr30 = 1

        !**      -----------------------------------
        !JK      WAR SCHON DEAKTIVIERT, 01.05.00, JK
        !**      if(lein.eq.3)
        !**   *  write(iuerr,'(''Warnung. Iteration in Schleife 3 fehlerhaft.'',
        !**   *                ''Froud-2 = 0'','' Fortsetzung mit FR2=FR1'')')
        !**      endif
        !**      -----------------------------------

        fr2 = fr1

      ENDIF

      !UT         WAS IST DD???, 25.08.2000
      !UT         BWK-BERICHT 2000, FORMEL (4.2.23) geteilt durch fr2
      dd = - 0.5 / fr2 * ( (3. * ystern**2 - 1.) * 2. * (ystern - &
            & cc) - (ystern**3 - ystern) * 2) / (4. * (ystern - cc) **2)


      !JK         KONVERGENZ ERREICHT --> VERLASSEN DER SCHLEIFE
      IF (abs ( (fr1 - fr2) / fr2) .lt.1.e-3) goto 2600

      !UT         BERECHNUNG VON DDi, WELCHE IM PROGRAMM NICHT MEHR BENOETIGT
      !UT         WERDEN??? -> TERME FORMEL (4.2.23), ABER dd1 oben NEGATIV?
      IF (abs (dd) .lt.1.e-05) then
        dd1 = 0.5 / fr2
        dd2 = (3. * ystern**2 - 1.) * 2 * (ystern - cc)
        dd3 = (ystern**3 - ystern) * 2
        dd4 = 4 * (ystern - cc) **2

        !JK         WAR SCHON DEAKTIVIERT, 01.05.00, JK
        !**         print *,'Warnung dd in LINDY = 0 , setzte ystern neu.'

        ystern = ystern - 0.01
        yn = yn - 0.01

      ELSE

        yn = yn - (fr1 - fr2) / dd
        IF (yn.ge.1.) yn = (ya + 1.) / 2.
        IF (yn.le.0.5) yn = (ya + 1.) / 2.
        ya = ystern
        ystern = yn

      ENDIF


      IF (iter.gt.iterm) then

        ystern = 0.999999
        yn = ystern
        fr2 = 1.e+06
        difmin = 1.e+06

        DO 2550 WHILE(abs (fr1 - fr2) / fr2.gt.1.e-02)

          fr2a = fr2
          fr2 = (ystern * (ystern**2 - 1.) / (2. * (ystern - az / (az - dp) ) ) ) **.5
          diff = abs (fr1 - fr2)

          IF (difmin.gt.diff) then
            difmin = diff
            fr2min = fr2
            yn = ystern
          ENDIF

          IF (fr2 - fr2a.ge.0) then
            ystern = ystern - 0.001

            IF (ystern.lt.0) then
              ystern = yn
              fr2 = fr2min
              !JK  VERLASSEN DER SCHLEIFE
              GOTO 2600
            ENDIF
          ELSE
            ystern = yn
            fr2 = fr2min
            !JK    VERLASSEN DER SCHLEIFE
            GOTO 2600
          ENDIF

        2550 CONTINUE

        !JK  VERLASSEN DER SCHLEIFE
        GOTO 2600

      ENDIF

      GOTO 2500

    !JK      ELSE ZU (fr1.gt.fr2)
    ELSE

      fr2 = fr1
      ystern = 1.0

    !JK      ENDIF ZU (fr1.gt.fr2)
    ENDIF

    !                                                 /* schleife 3
    2600 CONTINUE

    dcw = 2. / fr2**2 * (1. - ystern)
    yst = ystern

    !JK   ---KORREKTUR-----------------------------------------------------
    !JK
    !JK      BEI DER BERECHNUNG VON cwn NACH (28) BWK, S.29
    !JK      FEHLT DER FAKTOR cwun!

    !JK      NEU UND NACH BWK RICHTIG:
    cwn = 1.3124 * cwun * vv2 + dcw

    !JK      ALT UND NACH BWK FALSCH:
    ! cwn = 1.3124 * vv2 + dcw
    !JK
    !JK   ---ENDE DER KORREKTUR--------------------------------------------

    !JK       WAR SCHON DEAKTIVIERT, 01.05.00, JK
    ! dhyd=4.*h_mi

    !UT      BERECHNUNG REYNOLDSZAHL DES VORLANDES revor
    revor = vxmvor * dhyd / nue

  !JK   ENDIF ZU (abs(ax-0.0).le.1.e-04)
  ENDIF                                                                                 

  2650 CONTINUE
                                                                        
                                                                        
  !***********************************************************************
  !     DK, 15/05/01
  !      PRINT *,'Sub LINDY - cwn calculated!'
  !      PRINT *,'cwn=',cwn
  !      PRINT *,'aln_li=',aln_li,' aln_mi=',aln_mi,' aln_re=',aln_re
  !      PRINT *
  !***********************************************************************
                                                                        
                                                                        
  !UT   RAUHEIT UND BENETZTER UMFANFG RECHTER UND LINKER ABSTURZ
  ak_re = 0.0
  uk_re = 0.0
  ak_li = 0.0
  uk_li = 0.0


  !***********************************************************************
  !     DK, 15/05/01 - set values to avoid those for the previous subsecti
  al_li = 0.0
  al_re = 0.0
  !      al_mi = 0.0
  al_mi = aln_mi
  dh_mi = dhyd
  dh_li = 0.0
  dh_re = 0.0
  ren_mi = re_mi
  ren_li = 0.0
  ren_re = 0.0
  !***********************************************************************


  !UT   WIDERSTANDSBEIWERT UND HYDRAUL. DURCHMESSER R. U. L. ABSTURZ
  aln_li = 0.03
  aln_re = 0.03
  aln_mi = 0.03
  !***********************************************************************
  !DK    No need to change this value! Therefore, it was deactivated!
  !***********************************************************************
  dhn_li = 0.0
  dhn_re = 0.0


  !UT   BEDINGUNG FUER DURCHSTROEMTE FLAECHE a-li? UND
  !UT   WASSERSPIEGELHOEHE LINKER ABSTURZ h_li (HIER FEHLT EIN PUNKT 0.0!)
  IF ( (abs (a_li) .le.0.01) .and. (h_li.gt.0) ) then
    IF (2 * a_li / h_li**2.le.0.05) then
      ak_li = rau_li
      uk_li = h_li
      !***********************************************************************
      !DK          Term -h_mi taken out! This line was: uk_li = h_li-h_mi
      !***********************************************************************
    ENDIF
    !***********************************************************************
    !DK   The inner "if" means slopes greater than 20:1 (1:0.05)! Now,
    !DK   it simply does not consider cases of very flat subsections!
    !***********************************************************************
  ENDIF

  IF ( (abs (a_re) .le.0.01) .and. (h_re.gt.0) ) then
    IF (2 * a_li / h_li**2.le.0.05) then
      ak_re = rau_re
      uk_re = h_re
      !***********************************************************************
      !DK          Term -h_mi taken out! This line was: uk_re = h_re-h_mi
      !***********************************************************************
    ENDIF
    !***********************************************************************
    !DK   The inner "if" means slopes greater than 20:1 (1:0.05)! Now,
    !DK   it simply does not consider cases of very flat subsections!
    !***********************************************************************
  ENDIF

  !UT   BERECHNUNG HYDRAUL. DURCHMESSER dhyg DES TEILABSCHNITTES VORLANDES
  dhyg = a_mi / (u_mi + uk_li + uk_re) * 4.
  !UT   BERECHNUNG HYDRAUL. DURCHMESSER
  dhn_mi = dhyg
  IF (uk_li.gt.1.e-03) dhn_li = dhyg
  IF (uk_re.gt.1.e-03) dhn_re = dhyg
  !***********************************************************************
  !DK   These two lines have been added in order to define hydraulic diame
  !DK   in these cases!
  !***********************************************************************


  !***********************************************************************
  !     DK, 15/05/01
  !      PRINT *,'Sub LINDY - dhyg calculated!'
  !      PRINT *,'uk_li=',uk_li,' u_mi=',u_mi,' uk_re=',uk_re
  !      PRINT *,'a_mi=',a_mi,' dhyg=',dhyg
  !      PRINT *,'dhn_li=',dhn_li,' dhn_mi=',dhn_mi,' dhn_re=',dhn_re
  !      PRINT *
  !***********************************************************************


  !UT   SETZEN DER RANDBEDINGUNGEN FUER SCHLEIFE 2710
  delhyd = 1000.
  iter27 = 0
  it27ma = 30

  DO 2710 WHILE(delhyd.gt.1.e-3)

    il2710 = 0

    iter27 = iter27 + 1


    !***********************************************************************
    !     DK, 15/05/01
    !      PRINT *,'Sub LINDY - entered the cycle for dhyd!'
    !      PRINT *,'iter27=',iter27
    !      PRINT *
    !***********************************************************************

    !***********************************************************************
    !     DK, 15/05/01
    !      PRINT *,'Sub LINDY - before calculation of dhy!'
    !      PRINT *,'dh_li=',dh_li,' dh_mi=',dh_mi,' dh_re=',dh_re
    !      PRINT *,'ren_li=',ren_li,' ren_mi=',ren_mi,' ren_re=',ren_re
    !      PRINT *
    !***********************************************************************


    !UT   MITTELUNG DER ITERIERTEN WERTE
    IF (iter27.gt.1) then
      dh_mi = (dhn_mi + dh_mi) / 2.
      dh_li = (dhn_li + dh_li) / 2.
      dh_re = (dhn_re+dh_re) / 2.
    ELSE
      dh_mi = dhn_mi
      dh_li = dhn_li
      dh_re = dhn_re
    ENDIF

    !UT       BERECHNUNG DER NEUEN REYNOLDSZAHLEN
    ren_mi = vxmvor * dh_mi / nue
    ren_li = vxmvor * dh_li / nue
    ren_re = vxmvor * dh_re / nue


    !***********************************************************************
    !     DK, 15/05/01
    !      PRINT *,'Sub LINDY - after calculation of dhy!'
    !      PRINT *,'dh_li=',dh_li,' dh_mi=',dh_mi,' dh_re=',dh_re
    !      PRINT *,'ren_li=',ren_li,' ren_mi=',ren_mi,' ren_re=',ren_re
    !      PRINT *
    !***********************************************************************


    !UT       GRENZBEDINGUNG REYNOLDSZAHL VORLAND FUER SCHLEIFE 2720
    delrev = 1000.
    iter272 = 0
    it272ma = 10

    DO 2720 WHILE(delrev.gt.1.e-2)

      il2720 = 0

      iter272 = iter272 + 1

      !***********************************************************************
      !     DK, 15/05/01
      !      PRINT *,'Sub LINDY - entered the cycle for Re!'
      !      PRINT *,'iter272=',iter272
      !      PRINT *
      !***********************************************************************

      !***********************************************************************
      !     DK, 15/05/01
      !      PRINT *,'Sub LINDY, the cycle for Re, before setting re_* values!
      !      PRINT *,'re_li=',re_li,' re_mi=',re_mi,' re_re=',re_re
      !      PRINT *,'ren_li=',ren_li,' ren_mi=',ren_mi,' ren_re=',ren_re
      !      PRINT *
      !***********************************************************************


      re_mi = ren_mi
      re_li = ren_li
      re_re = ren_re

      dellam = 1000.
      it4max = 20

      !**************************************************************
      !**   KORREKTUR, 23. JUNI 2000, JANA KIEKBUSCH
      !**   ----------------------------------------------------------
      !**
      !**   DAS GRENZKRITERIUM FUER DEN ks-WERT BEZIEHT SICH AUF DEN
      !**   HYDRAULISCHEN RADIUS, NICHT AUF DEN HYDRAULISCHEN DURCH-
      !**   MESSER. DIE SUBROUTINE 'KORRIGI' KORRIGIERT DAS GRENZ-
      !**   KRITERIUM DURCH DIE UMRECHNUNG VON HYDRAULISCHEN DURCH-
      !**   MESSER AUF HYDRAULISCHEN RADIUS (FAKTOR 4).
      !**
      !**   KORREKTUR ANWENDEN:
      !**     korrekt = 'j' --> GRENZKRITERIUM KORRIGIEREN
      !**
      !**     korrekt = 'n' --> GRENZKRITERIUM BEIBEHALTEN
      !      korrekt='n'
      !      if (korrekt.eq.'j') then
      !          call korrigi(rau_mi,rau_li,rau_re,ak_li,ak_re,dh_mi,dh_li,
      !     +    dh_re,ak1_mi,ak1_li,ak1_re)
      !          GOTO 333
      !      endif
      !**
      !**   ENDE PROGRAMMERWEITERUNG VOM 23.06.2000
      !***************************************************************

      !***********************************************************************
      !     DK, 21/05/01 - deactivated in order to apply Kouwen's procedure!
      !              if (rau_mi.gt.0.60*dh_mi/4.) then         ! dh_mi/4. inst
      !                  ak1_mi = 0.60*dh_mi/4.                ! dh_mi/4. inst
      !              else
      !                  ak1_mi = rau_mi
      !              endif
      !
      !              if (ak_li.gt.0.60*dh_li/4.) then          ! dh_li/4. inst
      !                  ak1_li = 0.60*dh_li/4.                ! dh_li/4. inst
      !              else
      !                  ak1_li = rau_li
      !              endif
      !
      !              if (ak_re.gt.0.60*dh_re/4.) then          ! dh_re/4. inst
      !                  ak1_re = 0.60*dh_re/4.                ! dh_re/4. inst
      !              else
      !                  ak1_re = rau_re
      !              endif
      !***********************************************************************
      !DK   Hydraulic radius introduced instead of hydraulic diameter!!!
      !***********************************************************************

      !***********************************************************************
      !     DK, 15/05/01
      !      PRINT *,'Sub LINDY - still in the cycle for Re!'
      !      PRINT *,'dh_li=',dh_li,' dh_mi=',dh_mi,' dh_re=',dh_re
      !      PRINT *,'rh_li=',dh_li/4.,' rh_mi=',dh_mi/4.,' rh_re=',dh_re/4.
      !      PRINT *,'ak1_li=',ak1_li,' ak1_mi=',ak1_mi,' ak1_re=',ak1_re
      !      PRINT *
      !***********************************************************************

      !UT   EINSPRUNGLABEL WENN NACH KIEKBUSCH GERECHNET WURDE
      !333   continue

      iter4 = 0

      DO 2730 WHILE(dellam.gt.1.e-03)

        il2730 = 0

        iter4 = iter4 + 1


        !***********************************************************************
        !     DK, 15/05/01
        !      PRINT *,'Sub LINDY - entered the cycle for lambda!'
        !      PRINT *,'iter4=',iter4
        !      PRINT *
        !***********************************************************************

        !***********************************************************************
        !     DK, 15/05/01
        !      PRINT *,'Sub LINDY, cycle for lambda, before setting al_* values!
        !      PRINT *,'al_li=',al_li,' al_mi=',al_mi,' al_re=',al_re
        !      PRINT *,'aln_li=',aln_li,' aln_mi=',aln_mi,' aln_re=',aln_re
        !      PRINT *
        !***********************************************************************


        !UT               NEUSETZEN DER WIDERSTANDSBEIWERTE DER ABSTUERZE
        al_mi = aln_mi
        al_li = aln_li
        al_re = aln_re

        !**********************************************************************
        !**   PROGRAMMERWEITERUNG, 16. JUNI 2000, JANA KIEKBUSCH
        !**   -----------------------------------------------------------------
        !**
        !**   BERECHNUNGSART BESTIMMEN:
        !**     ber_art = 'bwk' --> WIDERSTANDSGESETZ NACH BWK
        !**                         MIT RAUHEITSUNTERSCHEIDUNG
        !**
        !**     ber_art = 'alt' --> WIDERSTANDSGESETZ NACH PASCHE
        !      ber_art='bwk'
        !      if (ber_art.eq.'bwk') then
        !          call fuent_l(rau_mi,rau_li,rau_re,dh_mi,dh_li,dh_re,
        !     +re_mi,re_li,re_re,aln_mi,aln_li,aln_re,al_mi,al_li,al_re,
        !     +grenzkrit_mi,grenzkrit_li,grenzkrit_re,dm_mi,dm_li,dm_re,ks_mi)
        !          GOTO 888
        !      endif
        !**
        !**   ENDE PROGRAMMERWEITERUNG
        !***********************************************************************

        !           aln_mi=(1./(-2.03*alog10(ak1_mi/dh_mi/3.71)))**2
        !JK         WAR SCHON DEAKTIVIERT, 01.05.00, JK


        !***********************************************************************
        !     DK, 15/05/01
        !      PRINT *,'Sub LINDY, cycle for lambda, before calc. aln_* values!'
        !      PRINT *,'al_li=',al_li,' al_mi=',al_mi,' al_re=',al_re
        !      PRINT *
        !***********************************************************************

        !           aln_mi=(1./(-2.03*alog10(5.8/(re_mi*sqrt(al_mi))+ak1_mi/
        !     &             dh_mi/3.71)))**2
        !
        !                  if (ak_li .gt.0 .AND. dh_li.gt.0) then
        !                      aln_li=(1./(-2.03*alog10(5.8/(re_li*al_li**0.5)
        !     &               +ak1_li/dh_li/3.71)))**2
        !                  else
        !                      aln_li=0.0
        !                  endif
        !
        !                  if (ak_re.gt.0.AND.dh_re.gt.0) then
        !                      aln_re=(1./(-2.03*alog10(5.8/(re_re*al_re**0.5)
        !     &               +ak1_re/dh_re/3.71)))**2            ! ren_re chang
        !                  else
        !                      aln_re=0.0
        !                  endif

        !***********************************************************************
        !     DK, 21/05/01 - Calculation of the friction factor using
        !     Colebrook-White formula for sediment bed or Kouwen procedure for g

        !**   BERECHNUNG GRENZKRITERIUM NACH BATHURST, BWK, S.24
        grenzkrit_mi = dh_mi / 4. / rau_mi

        !**   ABFRAGE, OB SENKRECHTER ABSTURZ LINKS DEFINIERT
        IF (rau_li.gt.0.AND.dh_li.gt.0) then
          grenzkrit_li = dh_li / 4. / rau_li
        ENDIF

        !**   ABFRAGE, OB SENKRECHTER ABSTURZ RECHTS DEFINIERT
        IF (rau_re.gt.0.AND.dh_re.gt.0) then
          grenzkrit_re = dh_re / 4. / rau_re
        ENDIF

        !***********************************************************************
        !     DK, 21/05/01
        !      PRINT *,'grenzkrit_li=',grenzkrit_li,' grenzkrit_mi=',grenzkrit_m
        !      PRINT *,'grenzkrit_re=',grenzkrit_re
        !      PRINT *
        !***********************************************************************

        IF (tbedr.eq.'h-g ') then

          !***********************************************************************
          !     DK, 21/05/01
          !      PRINT *,'Sub LINDY - It is entering Sub KOUWEN with these data:'
          !      PRINT *,'is=',is,' rau_mi=',rau_mi,' tgrar=',tgrar,' dh_mi=',dh_m
          !      PRINT *,'meil=',meil
          !      PRINT *
          !***********************************************************************

          !**                   CALCULATION AFTER KOUWEN
          CALL kouwen (meil, is, rau_mi, tgrar, dh_mi, aln_mi)

          !***********************************************************************
          !     DK, 21/05/01
          !      PRINT *,'Sub LINDY - Passed Sub KOUWEN!'
          !      PRINT *,'is=',is,' rau_mi=',rau_mi,' dh_mi=',dh_mi
          !      PRINT *,'aln_mi=',aln_mi
          !      PRINT *
          !***********************************************************************

          IF (rau_li.gt.0.AND.dh_li.gt.0) then
            CALL kouwen (meil, is, rau_li, tgrar, dh_li, aln_li)
          ELSE
            aln_li = 0.0
          ENDIF

          IF (rau_re.gt.0.AND.dh_re.gt.0) then
            CALL kouwen (meil, is, rau_re, tgrar, dh_re, aln_re)
          ELSE
            aln_re = 0.0
          ENDIF

        !**   END OF CALCULATION AFTER KOUWEN
        ELSEIF (tbedr.eq.'k-s ') then

          !**   CALCULATION AFTER COLEBROOK-WHITE
          ak1_mi = rau_mi

          IF (grenzkrit_mi.lt.1.66667) ak1_mi = 0.60 * dh_mi / 4.

          rhy = dh_mi/4
          aln_mi = GET_LAMBDA (vxmvor, rhy, ak1_mi, fbw)
          !aln_mi = (1. / ( - 2.03 * alog10 (2.51 / (re_mi * al_mi**0.5) &
          !+ ak1_mi / dh_mi / 3.71) ) ) **2

          IF (rau_li.gt.0.AND.dh_li.gt.0) then
            ak1_li = rau_li

            IF (grenzkrit_li.lt.1.66667) ak1_li = 0.6 * dh_li / 4.

            rhy = dh_li/4
            aln_li = GET_LAMBDA (vxmvor, rhy, ak1_li, fbw)
            !aln_li = (1. / ( - 2.03 * alog10 (2.51 / (re_li * al_li**   &
            !0.5) + ak1_li / dh_li / 3.71) ) ) **2
          ELSE
            aln_li = 0.0
          ENDIF

          IF (rau_re.gt.0.AND.dh_re.gt.0) then
            ak1_re = rau_re

            IF (grenzkrit_re.lt.1.66667) ak1_re = 0.6 * dh_re / 4.

            rhy = dh_re/4
            aln_re = GET_LAMBDA (vxmvor, rhy, ak1_re, fbw)
            !aln_re = (1. / ( - 2.03 * alog10 (2.51 / (re_re * al_re**   &
            !0.5) + ak1_re / dh_re / 3.71) ) ) **2
          ELSE
            aln_re = 0.0
          ENDIF

          !*******************************************************************
          ! Setting codes for printing warnings!
          IF (grenzkrit_mi.lt.1.66667) then
            ! Left floodplain (ibed=1)
            IF (ibed.eq.1) icwl = 1
            ! Right floodplain (ibed=3)
            IF (ibed.eq.3) icwr = 1
          ENDIF
          !***********************************************************************        
                                                                        
          !***********************************************************************
          !     DK, 21/05/01
          !      PRINT *,'Sub LINDY - Colebrook-White formula applied!'
          !      PRINT *,'re_mi=',re_mi,' al_mi=',al_mi
          !      print *,'rau_mi=',rau_mi,' ak1_mi=',ak1_mi,' dh_mi=',dh_mi
          !      PRINT *,'aln_mi=',aln_mi
          !      PRINT *
          !      PRINT *,'re_li=',re_li,' al_li=',al_li
          !      print *,'rau_li=',rau_li,' ak1_li=',ak1_li,' dh_li=',dh_li
          !      PRINT *,'aln_li=',aln_li
          !      PRINT *
          !      PRINT *,'re_re=',re_re,' al_re=',al_re
          !      print *,'rau_re=',rau_re,' ak1_re=',ak1_re,' dh_re=',dh_re
          !      PRINT *,'aln_re=',aln_re
          !      PRINT *
          !***********************************************************************

          !**                   END OF CALCULATION AFTER COLEBROOK-WHITE           
                                                                        
        ELSEIF (tbedr.eq.'d-50') then 
                                                                        
          PRINT * , 'Sub LINDY!' 
          PRINT * , 'Fuentes proc. can not be applied in floodplain!' 
          PRINT * , 'Change type of bottom roughness in PRF file!' 
          PRINT * 
                                                                        
        ELSE 
                                                                        
          PRINT * , 'Sub LINDY!' 
          PRINT * , 'Name of bottom roughness type given improperly!' 
          PRINT * , 'Modify the name in PRF file!' 
          PRINT * 
                                                                        
        ENDIF 
                                                                        
        !     DK, 21/05/01 - End of calculation of the friction factor using
        !     Colebrook-White formula for sediment bed or Kouwen procedure for g
        !***********************************************************************
                                                                        
        !***********************************************************************
        !     DK, 21/05/01 - Calculation of the friction factor organized anothe
        !     Therefore, the following lines were deactivated!

        !**               DIE BERECHNUNG FUER LINKS UND RECHTS ERFOLGT NUR BEI
        !**               SENKR. ABSTUERZEN IM VORLAND. SONST SIND DIE WERTE 0!

        !**               ABFRAGE, OB SENKRECHTER ABSTURZ RECHTS DEFINIERT
        !                  if (rau_li.gt.0.AND.dh_li.gt.0) then
        !**                   ABFRAGE DES GRENZKRITERIUMS FUER RAUHEITSFORM
        !                      if (grenzkrit_li.le.4.) then
        !**                       CALCULATION AFTER KOUWEN
        !                          call kouwen(grenzkrit_li,is,rau_li,grass,
        !     +                                dh_li,aln_li)
        !                      else
        !**                       FORMEL 16 NACH BWK-MERKBLATT, S.20
        !                          aln_li=(1./(-2.03*alog10(2.51/
        !     &                    (re_li*al_li**0.5)+rau_li/dh_li/3.71)))**2
        !                      endif
        !                  else
        !                      aln_li=0.0
        !                  endif

        !**               ABFRAGE, OB SENKRECHTER ABSTURZ RECHTS DEFINIERT
        !                  if (rau_re.gt.0.AND.dh_re.gt.0) then
        !**                   ABFRAGE DES GRENZKRITERIUMS FUER RAUHEITSFORM
        !                      if (grenzkrit_re.le.4.) then
        !**                       CALCULATION AFTER KOUWEN
        !                          call kouwen(grenzkrit_re,is,rau_re,grass,
        !     +                                dh_re,aln_re)
        !                      else
        !**                       FORMEL 16 NACH BWK-MERKBLATT, S.20
        !                          aln_re=(1./(-2.03*alog10(2.51/
        !     &                    (re_re*al_re**0.5)+rau_re/dh_re/3.71)))**2
        !                      endif
        !                  else
        !                      aln_re=0.0
        !                  endif
        !***********************************************************************    

        !GOTO 888
                                                                        
        !UT   EINSPRUNGLABEL BEI BERECHNUNG NACH KIEKBUSCH
        ! 888   CONTINUE
                                                                        

        !UT     BERECHNUNG ABWEICHUNG DER NEUEN/ALTEN LAMBDA WERTE    
        dellam = abs (aln_mi - al_mi) + abs (aln_li - al_li) + abs (aln_re-al_re)
                                                                        
        !UT               ABFRAGE OB MAXIMALE ITERATIONSZAHL UEBERSCHRITTEN
        !UT               --> AUSSTIEG 2731
        IF (iter4.gt.it4max) then 

          il2730 = 10 
                                                                        
          !**           -----------------------------------
          !JK           WAR SCHON DEAKTIVIERT, 01.05.00, JK
          !**           if(lein.eq.3)then
          !**            write(iuerr,
          !**     &             '(''Warnung. Keine Konvergenz in Lambda-Schleife''
          !**     &                 '' der SUB LINDY.'')')
          !**            write(iuerr,'(''(Der Fehler betraegt nach '',i4,
          !**     &               '' Iterationen DELLAM = '',F10.6)')iter4,dellam
          !**           endif
          !**           -----------------------------------                        
                                                                        
          GOTO 2731 
        ENDIF 
                                                                        
      ! Ende der DO
      2730 CONTINUE
                                                                        
      2731 CONTINUE
                                                                        
      !UT           BERECHNUNG LAMBDAgesamt FUER VORLAND
      lamvog = ( (aln_mi + lamv) * u_mi + aln_li * uk_li + aln_re *   &
                 & uk_re) / (u_mi + uk_li + uk_re)
                                                           ! al_re changed

      !UT           STROEMUNGSGESCHWINDIGKEIT IM VORLAND
      vxmvor = (2. * g * is * dhyg / lamvog) **0.5

      !UT           BERECHNE NEUE REYZAHLEN, MITTE, LINKS, RECHTS
      ren_mi = vxmvor * dh_mi / nue
      ren_li = vxmvor * dh_li / nue
      ren_re = vxmvor * dh_re / nue


      !***********************************************************************
      !     DK, 15/05/01
      !      PRINT *,'Sub LINDY, exited the cycle for lambda!'
      !      PRINT *,'al_li=',al_li,' aln_li=',aln_li
      !      PRINT *,'al_mi=',al_mi,' aln_mi=',aln_mi
      !      PRINT *,'al_re=',al_re,' aln_re=',aln_re
      !      print *,'lamvog=',lamvog
      !      print *,'vxmvor=',vxmvor
      !      PRINT *,'ren_li=',ren_li,' ren_mi=',ren_mi,' ren_re=',ren_re
      !      PRINT *
      !***********************************************************************

      !UT           ABWEICHNUNG DER NEU BERECHNETEN REYZAHLEN IN DER SUMME
      delrev = abs (re_mi - ren_mi) + abs (re_li - ren_li) + abs (re_re-ren_re)

      !UT           ABFRAGE OB MAXIMAL ITERATIONSZAHL UEBERSCHRITTEN
      !UT           --> AUSSTIEG UND FEHLERMARKER il2720 BELEGEN
      IF (iter272.gt.it272ma) then

        il2720 = 100

        !**           -----------------------------------
        !JK           WAR SCHON DEAKTIVIERT, 01.05.00, JK
        !**           if(lein .eq.3)then
        !**            write(iuerr,
        !**     &           '(''Warnung. Keine Konvergenz in Reynolds-Schleife''
        !**     &                 '' der SUB LINDY.'')')
        !**            write(iuerr,'(''(Der Fehler betraegt nach '',i4,
        !**     &             '' Iterationen DELREV = '',F10.6)')iter272,delrev
        !**           endif
        !**           -----------------------------------

        GOTO 2721
      ENDIF                                                                          
                                                                        
    2720 CONTINUE

    2721   CONTINUE
                                                                        
    !UT       BERECHNUNG HYDRAULISCHER DURCHMESSER MITTE MIT
    !UT       lamv DURCH BEWUCHS
                                                         ! al_mi changed
    dhn_mi = (aln_mi + lamv) / lamvog * dhyg
                                                                        
    !         Gemaess Pasche-Verfahren (DVWK-Heft 220) ist bei der Rauheits-
    !         ueberlagerung im Vorland die bewuchsbedingte Rauheit zur Be-
    !         rechnung des hydrl. Durchmessers nicht mit anzusetzen.
                                                                        
                                                         ! al_li changed
    dhn_li = aln_li / lamvog * dhyg
                                                         ! al_re changed
    dhn_re = aln_re / lamvog * dhyg
                                                                        
                                                                        
    !***********************************************************************
    !     DK, 15/05/01
    !      PRINT *,'Sub LINDY, exited the cycle for Re!'
    !      PRINT *,'al_li=',al_li,' aln_li=',aln_li
    !      PRINT *,'al_mi=',al_mi,' aln_mi=',aln_mi
    !      PRINT *,'al_re=',al_re,' aln_re=',aln_re
    !      print *,'lamvog=',lamvog
    !      print *,'dhyg=',dhyg
    !      PRINT *,'dhn_li=',dhn_li,' dhn_mi=',dhn_mi,' dhn_re=',dhn_re
    !      PRINT *
    !***********************************************************************


    !UT       BERECHNUNG DER NOCH VORLIEGENDEN ABWEICHUNGEN IM HYDR. DURCHM. 
    delhyd = abs (dh_mi - dhn_mi) + abs (dh_re-dhn_re) + abs (dh_li - dhn_li)
                                                                        
    IF (iter27.gt.it27ma) then

      il2710 = 1000
                                                                        
      !**           -----------------------------------
      !JK           WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !**           if(lein .eq. 3)then
      !**            write(iuerr,
      !**     &             '(''Warnung. Keine Konvergenz in DHYDR-Schleife''
      !**     &                 '' der SUB LINDY.'')')
      !**            write(iuerr,'(''(Der Fehler betraegt nach '',i4,
      !**     &               '' Iterationen delhyd = '',F10.6)')iter27,delhyd
      !**           endif
      !**           -----------------------------------                        
                                                                        
      GOTO 3000
    ENDIF
                                                                        

  ! Ende DO 2710-Schleife ----------------------------------------------------------------
  2710 CONTINUE
                                                                        
                                                                        
                                                                        
  !***********************************************************************
  !     DK, 15/05/01
  !      PRINT *,'Sub LINDY, exited the cycle for dhyd!'
  !      PRINT *,'dhn_li=',dhn_li,' dhn_mi=',dhn_mi,' dhn_re=',dhn_re
  !      PRINT *
  !***********************************************************************
                                                                        
                                                                        
  !UT       ABFRAGE OB MAX. ITERATION ERREICHT, ITCWM = 40
  IF (itcwr.gt.itcwm) then

    il3000 = 10000
                                                                        
    !UT           BERECHNUNG DER NOCH VORLIEGENDEN DIFFERENZ
    !UT           WIRD IM MOMENT NICHT BENOETIGT, 25.08.2000
    delcwr = abs (cwr - cwn)
                                                                        
    !**           -----------------------------------
    !JK           WAR SCHON DEAKTIVIERT, 01.05.00, JK
    !**           if(lein .eq. 3)then
    !**            write(iuerr,
    !**     &             '(''Warnung. Keine Konvergenz in CWR-Schleife''
    !**     &                 '' der SUB LINDY.'')')
    !**            write(iuerr,'(''(Der Fehler betraegt nach '',i4,
    !**     &               '' Iterationen DELcwr = '',F10.6)')itcwr,delcwr
    !**           endif
    !**           -----------------------------------                        
                                                                        
    cwr = (cwn + cwr) / 2.
    cwn = cwr
                                                                        
  ENDIF
                                                                        
!UT   SCHLEIFE ZUR BERECHNUNG VON CWR, MAXIMAL 40 ITERATIONEN -----------------------
3000 CONTINUE
                                                                        
                                                                        
!***********************************************************************
!     DK, 15/05/01                                                      
!      PRINT *,'Sub LINDY, exited the cycle for cwr!'                   
!      PRINT *,'cwn=',cwn                                               
!      print *,'v=',vxmvor,' lambda=',lamvog                            
!      PRINT *,'It is returning to EBKSN!'                              
!      PRINT *                                                          
!***********************************************************************
                                                                        
                                                                        
!UT   BERECHNUNG DER FEHLERBELEGUNG                                     
ifehl = il_fr30 + il2730 + il2720 + il2710 + il3000
                                                                        
                                                                        
!***********************************************************************
!     DK, 31/05/01                                                      
!      PRINT *,'Sub LINDY - exited the cycle for cwr!'                  
!      PRINT *,'icwl=',icwl,' ikol=',ikol                               
!      PRINT *,'icwm=',icwm,' ikom=',ikom,' ifum=',ifum                 
!      PRINT *,'icwr=',icwr,' ikor=',ikor                               
!      PRINT *,'It is returning to EBKSN!'                              
!      PRINT *                                                          
!***********************************************************************
                                                                        
END SUBROUTINE lindy
                                                                        


