!     Last change:  MD    8 Jul 2009    4:37 pm
!--------------------------------------------------------------------------
! This code, pasche.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - pasche
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
!-----------------------------------------------------------------------------


SUBROUTINE pasche (nknot, iprof, hr, bf, itere2, br, qvor1, qvor2, &
    & isener, v_tr, anl_l, anb_l, om_l, ct_l, bmwvor, l_tr, &
    & u_tr, vt_n, v1_hg, if_pa, formbeiwert)

!-----------------------------------------------------------------------------
!                                                                     
! SUBROUTINE PASCHE                                                 
!
! BESCHREIBUNG: BERECHNUNG TEILABFLUSS DER VORLAENDER NACH PASCHE   
!               BERECHNUNG TRENNFLAECHENGESCHWINDIGKEIT             
!               BERECHNUNG WIDERSTANDSBEIWERT DER TRENNFLAECHEN     
!
!-----------------------------------------------------------------------------

USE DIM_VARIABLEN
USE KONSTANTEN
USE BEWUCHS
USE IO_UNITS

! Calling variables
INTEGER, INTENT(IN) 		  	    :: nknot 	! Anzahl der Profilpunkte
CHARACTER(LEN=1), INTENT(IN) 	  	:: iprof        ! Art des Profils
REAL, INTENT(IN)                  	:: hr		! Wasserspiegelhöhe
REAL, DIMENSION(1:2), INTENT(OUT) 	:: bf           ! Mitwirkende Bewuchsbreite (1=links, 2=rechts)
INTEGER, INTENT(IN)               	:: itere2       ! WP bleibt unklar ????
REAL, DIMENSION(maxkla), INTENT(IN)	:: br           ! Spiegelbreite der drei Abschnitte
                                                        ! (1 = links, 2 = mitte, 3 = rechts)
REAL, INTENT(OUT)                   :: qvor1        ! Abfluss Vorland links
REAL, INTENT(OUT)                   :: qvor2        ! Abfluss Vorland rechts
REAL, INTENT(IN)                    :: isener       ! Energieliniengefälle
REAL, DIMENSION(1:2), INTENT(OUT)   :: v_tr         ! Trennflächengeschwindigkeit (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: anl_l        ! Nachlauflänge  (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: anb_l        ! Nachlaufbreite (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: om_l         ! Bewuchsparameter (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: ct_l         ! HILFSWERT FORMEL (42), BWK S.36, (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: bmwvor       ! Mitwirkende Bewuchsbreite auf dem Vorland (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: l_tr         ! Widerstandsbeiwert der Trennfläche (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: u_tr         ! Benetzter Umfang der Trennfläche (1 = links, 2 = rechts)
REAL, DIMENSION(1:2), INTENT(OUT)   :: vt_n         ! Trennflächengeschwindigkeit (1 = links, 2 = rechts)
REAL, INTENT(OUT)                   :: v1_hg        ! Mittlere Fließgeschwindigkeit im Flußschlauch
INTEGER, INTENT(OUT)                :: if_pa        ! Fehlervariable für Subroutine Pasche
REAL, DIMENSION(maxkla), INTENT(IN) :: formbeiwert  ! Formbeiwert der drei Abschnitte fuer die LAMBDA-Berechnung

! Local variables
REAL :: GET_CW_UN
REAL :: GET_A_NL
CHARACTER(LEN=2) :: ifact
INTEGER, DIMENSION(1:2) :: i10
INTEGER :: isch, ifall
REAL, DIMENSION(1:2) :: dqvor, b_vor, rhytr, a_kt

!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**   a_ks    --      durchströmte Fläche eines Teilabschnittes         
!**   a_kt    --      Einflußfläche der Trennfläche?                    
!**   a1      --      Faktor zur Berechnung von ct                      
!**   alpha   --      Exponent zur omega-Berechnung                     
!**   anb_l   --      Nachlaufbreite                                    
!**   anl_l   --      Nachlauflänge                                     
!**   ax      --      Bewuchsparameter                                  
!**   axj2    --      Nachlauflänge                                     
!**   axn2    --      Nachlauflänge                                     
!**   ay      --      Bewuchsparameter                                  
!**   b_hg    --      Breite des Flußschlauches                         
!**   b_vor   --      Breite des Vorlandes                              
!**   b_vor(1)--      Breite des linken Vorlandes                       
!**   b_vor(2)--      Breite des rechten Vorlandes                      
!**   b05f    --      Nachlaufbreite                                    
!**   b1      --      Faktor zur Berechnung von ct                      
!**   beta    --      Exponent zur omega-Berechnung                     
!**   bf(1)   --      mitwirkende Breite des Flußschlauches links       
!**   bf(2)   --      mitwirkende Breite des Flußschlauches rechts      
!**   bfn1    --      mitwirkende Breite bei Abbruch                    
!**   bmwvor  --      mitwirkende Breite des Vorlandes                  
!**   br(1)   --      Breite des linken Vorlandes                       
!**   br(3)   --      Breite des rechten Vorlandes                      
!**   c       --      Exponent                                          
!**   c4      --      Exponent                                          
!**   ct      --      Parameter zu Berechnung der Trennflächen-         
!**                   geschwindigkeit                                   
!**   ct_l    --      Parameter zu Berechnung der Trennflächen-         
!**                   geschwindigkeit
!**   ctfak   --      Verhältnis des benetzten Umfanges und des
!**                   Widerstandsbeiwertes der Trennflächen             
!**   cwun    --      Formwiderstandsbeiwert eines einzelnen Zylinders  
!**   dp      --      Bewuchsparameter                                  
!**   dqvor   --      zusätzlicher Abfluß auf dem Vorland               
!**   fakt    --      Faktor zur Berechnung der mitwirkenden Breite des 
!**                   Vorlandes                                         
!**   g       --      Erdbeschleunigung                                 
!**   h_tr(1) --      Höhe der Trennfläche links                        
!**   h_tr(2) --      Höhe der Trennfläche rechts                       
!**   hr      --      Wasserspiegelhöhe                                 
!**   htrli   --      Höhe der Trennfläche links                        
!**   htrre   --      Höhe der Trennfläche rechts                       
!**   i10(2)  --      Fehlerzaehler Schleife 10                         
!**   ianf_hg --      Punktnummer des Anfangs des Flußschlauches        
!**   iend_hg --      Punktnummer des Ende des Flußschlauches           
!**   if_pa   --      Fehlervariable für Subroutine Pasche              
!**   ifact   --      Trennflächentyp                                   
!**   ifall   --      Fallunterscheidung der Gerinneart                 
!**   iprof   --      Art des Profils                                   
!**   ischl   --      Punktnummer des linken Profilpunktes              
!**   ischr   --      Punktnummer des rechten Profilpunktes             
!**   isener  --      Energiegefälle                                    
!**   itr_typ_li      --      Trennflächentyp links                     
!**   itr_typ_re      --      Trennflächentyp rechts                    
!**   itrli   --      Punktnummer der linken Trennfläche                
!**   itrre   --      Punktnummer der rechten Trennfläche               
!**   l_hg    --      Widerstandsbeiwert des Flußschauches
!**   l_ks    --      Widerstandsbeiwert eines Teilabschnittes
!**   l_tr    --      Widerstandsbeiwert der Trennfläche                
!**   l_tr(1) --      Widerstandsbeiwert der linken Trennfläche         
!**   l_tr(2) --      Widerstandsbeiwert der rechten Trennfläche        
!**   num     --      ITERAT-ZAEHLER SCHLEIFE 1000, TRENNFLAECHENGESCHW.
!**   om_l    --      Bewuchsparameter, FELDEGROESSE RECHTS/LINKS       
!**   omega   --      Bewuchsparameter                                  
!**   q_ks    --      Abfluß eines Teilabschnittes                      
!**   qvor1   --      zusätzlicher Abfluß des linken Vorlandes          
!**   qvor2   --      zusätzlicher Abfluß des rechten Vorlandes         
!**   r_hg    --      hydraulischer Radius des Flußschlauches           
!**   r_ks    --      hydraulischer Radius eines Teilabschnittes        
!**   rhytr   --      hydraulischer Radius der Trennflächen             
!**   u_tr    --      benetzter Umfang der Trennfläche                  
!**   v_hg    --      mittlere Fließgeschwindigkeit im Flußschlauch     
!**   v_ks    --      Geschwindigkeit eines Teilabschnittes             
!**   v_tr(1) --      Trennflächengeschwindigkeit links                 
!**   v_tr(2) --      Trennflächengeschwindigkeit rechts                
!**   v1_hg   --      mittlere Fließgeschwindigkeit im Flußschlauch     
!**   vbmwv   --      Verhältnis der Widerstandsbeiwerte der Trennfläche
!**   vffa    --      mittlere Fließgeschwindigkeit im Flußschlauch     
!**   vlabw   --      Summe vlam und vbmwv                              
!**   vlam    --      Verhältnis der Widerstandsbeiwerte der Trennfläche
!**   vstt    --      Schubspannungsgeschwindigkeit in den Trennflächen 
!**   vxn     --      Trennflächengeschwindigkeit                       
!**                                                                     
!HB   ***************************************************************** 
!HB   30.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB   trenn_li --   benetzter Umfang der Trennflaeche links (=u_tr(1))  
!HB   trenn_re --   benetzter Umfang der Trennflaeche rechts (=u_tr(2)) 
!HB                 (benetzter Umfang einer Trennflaeche entspricht     
!HB                  benetzte Hoehe der Trennflaeche)                   
!HB   ***************************************************************** 
!**                                                                     
!**
!**   HINWEISE                                                          
!**   --------                                                          
!**                                                                     
!**                                                                     
!**   Folgende Fallunterscheidungen in Sub Pasche                       
!**   -------------------------------------------                       
!**                                                                     
!**   1. Fall: gegliedertes Gerinne mit beidseitigem Vorlandbewuchs     
!**                                                                     
!**               |--|--|--|------------------|--|--|--|--|             
!**               -----------                 ------------              
!**                          \               /                          
!**                           \             /                           
!**                            -------------                            
!**                                                                     
!**   2. Fall: gegliedertes Gerinne mit einseitigem Vorlandbewuchs      
!**                                                                     
!**               |--|--|--|------------------------------              
!**               -----------                 ------------              
!**                          \               /                          
!**                           \             /                           
!**                            -------------                            
!**   3. Fall: gegliedertes Gerinne ohne Vorlandbewuchs                 
!**                                                                     
!**               ----------------------------------------              
!**               -----------                 ------------              
!**                          \               /                          
!**                           \             /                           
!**                            -------------                            
!**                                                                     
!**  4. Fall: gegliedertes Gerinne mit beidseitigem Boeschungsbewuchs   
!**                                                                     
!**               |--|--|--|--|--|-------|--|-|--|--|--|--|             
!**               ----------- |  |       |  | ------------              
!**                          \|  |       |  |/                          
!**                           \  |       |  /                           
!**                            \ |       | /                            
!**                             \|       |/                             
!**                              ---------                              
!**                                                                     
!**   5. Fall: gegliedertes Gerinne mit einseitigem Boeschungsbewuchs   
!**                                                                     
!**               |--|--|--|--|--|------------|--|--|--|--|             
!**               ----------- |  |            ------------              
!**                          \|  |           /
!**                           \  |          /                           
!**                            \ |         /                            
!**                             \|        /                             
!**                              ---------                              
!**                                                                     
!**   6. Fall: Wasserspiegel unter Bordvoll                             
!**                                                                     
!**               -----------                 ------------              
!**                          \---------------/                          
!**                           \             /                           
!**                            -------------                            
!**                                                                     
!**   7. Fall: Wasserspiegel einseitig Bordvoll, einseitig Vorlandbew.  
!**                                                                     
!**                        \------------------|--|--|--|--|             
!**                         \                 ------------              
!**                          \               /                          
!**                           \             /                           
!**                            \           /                            
!**                             \         /                             
!**                              ---------                              
!**                                                                     
!**   8. Fall: Wasserspiegel einseitig Bordvoll, Vorland ohne Bewuchs   
!**                                                                     
!**                        \------------------------------              
!**                         \                 ------------              
!**                          \               /                          
!**                           \             /                           
!**                            \           /                            
!**                             \         /                             
!**                              ---------                              
!**                                                                     
!***********************************************************************



! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
! ----------------------------------------------------------------------------------


! COMMON-Block /H_TRENN/ -----------------------------------------------------------
! Die Hoehen der beiden Trennflaechen werden an SUB ALPHA_DRU
! uebergeben.
REAL 		:: trenn_li, trenn_re
COMMON / h_trenn / trenn_li, trenn_re
! ----------------------------------------------------------------------------------


! COMMON-Block /PA_COM/ -------------------------------------------------------
REAL 		:: a_ks (maxkla), b_ks (maxkla), r_ks (maxkla), u_ks (maxkla)
REAL            :: k_ks (maxkla), h_ks (maxkla), v_ks (maxkla), l_ks (maxkla)
REAL            :: q_ks (maxkla), b_hg, a_hg, q_hg, v_hg, r_hg, l_hg, fr_hg
INTEGER         :: ischl, ischr
COMMON / pa_com / a_ks, b_ks, r_ks, u_ks, k_ks, h_ks, v_ks, l_ks, &
       & q_ks, b_hg, a_hg, u_hg, q_hg, v_hg, r_hg, l_hg, fr_hg, ischl, ischr
! -----------------------------------------------------------------------------



! -----------------------------------------------------------
! Initialisieren
! -----------------------------------------------------------
trenn_li = 0
trenn_re = 0

bf(1) = 0.0
bf(2) = 0.0
                                                                        
!ep 05.02.2002 Initialisieren des Fehlerflags
ip1000 = 0
ip2000 = 0

!JK   ------------------------------------------------------------------
!JK   BERECHNUNGEN                                                      
!JK   ------------------------------------------------------------------

!write (*,*) 'In PASCHE. ITERE2 = ', itere2

!UT   SETZE STARTWERT FUER ITERATIONSBERECHNUNG
iter1 = 0

!UT   BERECHNEN KONSTANTE c
c = - 1. / 0.7

!UT   UMBENNUNG BREITE DES VORLANDES, br WURDE DIREKT UEBERGEBEN
!UT   1 = links, 2 = mitte, 3 = rechts
b_vor (1) = br (1)
b_vor (2) = br (3)


!JK   FUER NORMALPROFIL
IF (iprof.eq.' ') then

  !UT      BERECHNUNG BENETZTER UMFANG DER TRENNFLAECHE FUER NORMALPROFIL
  !UT      1 = links, 2 = rechts
  u_tr (1) = hr - htrli
  u_tr (1) = MAX (0., u_tr(1))
  u_tr (2) = hr - htrre
  u_tr (2) = MAX (0., u_tr(2))

ELSE

  !UT      KEIN NORMALPROFIL => BEN. UMFANG TRENNFL. = 0
  u_tr (1) = 0.0
  u_tr (2) = 0.0

ENDIF


!HB   *****************************************************************
!HB   30.11.2001 - H.Broeker
!HB   ----------------------
!HB   Hoehe der Trennflaechen wird neuer Variable zugewiesen. Diese
!HB   werden durch den Commonblock/h_trenn/ an SUB ALPHA_DRU uebergeben.
trenn_li = u_tr (1)
trenn_re = u_tr (2)
!HB   *****************************************************************

!UT   itere2 WIRD DIREKT UEBERGEBEN!
IF (itere2 .le. 1) then
  v_tr (1) = 0.0
  v_tr (2) = 0.0
  l_tr (1) = 0.0
  l_tr (2) = 0.0

  IF (itrli .gt. 1 .and. u_tr(1) .gt. 1.e-02) then
    v_tr (1) = 3.0 * v_ks (itrli - 1)
    !WRITE (*, * ) 'In PASCHE: v_ks_li=', v_ks (itrli - 1)
  ENDIF

  IF (itrre .lt. nknot .and. u_tr(2) .gt. 1.e-02) then
    v_tr (2) = 3.0 * v_ks (itrre)
    !WRITE (*, * ) 'In PASCHE: v_ks_re=', v_ks (itrre)
  ENDIF

ENDIF

!UT   STARTWERT FUER SCHLEIFE 1 VERH. WIDERSTANDSBEIWERTE DER TRENNFL.
vlam = 1000.0
!UT   STARTWERT FUER SCHLEIFE 1 VERHAELTNIS DER MITWIRK. BREITEN
vbmwv = 0.0


!write (*,*) 'In PASCHE. Vor DO 1. V_TR(1) = ', v_tr(1)

!JK   START ITERATIONSSCHLEIFE (BIS ENDE SUBROUTINE)
!UT   VERHAELTNIS LAMBDAvorlaender UND MITWIRKENDE BREITEN IM VORLAND
!JK   --------------------------------------------------------------------------
!UT   epsi=1.e-03*50 = 0.05 = 5%
DO 1 WHILE(abs (vlam - vbmwv) .gt. (epsi * 50.) )

  !UT      SETZE FEHLERWERT SCHLEIFE 1???
  ip1 = 0

  !UT      ZAEHLEN DER ITERATIONEN, SCHLEIFE 1
  iter1 = iter1 + 1

  !UT      SETZEN STARTWERTE SCHLEIFE 5, MITTL. FLIESSGESCHW. FLUSSCHLAUCH
  vffa = 1000.0
  v_hg = 0.0

  !UT      STARTWERT ANZAHL ITERATIONEN SCHLEIFE 5
  iter5 = 0

    DO 5 WHILE(abs (vffa - v_hg) .gt.1.e-02)

      !UT  FEHLERWERT SCHLEIFE 5???
      ip5 = 0

      !UT  ZAHL DER ITERATIONEN SCHLEIFE 5
      iter5 = iter5 + 1

      !write (*,*) 'In PASCHE. DO 5 Schleife Nr. ', iter5, '  V_TR = ', v_tr(1), v_tr(2)

      !JK  FUER BEIDE VORLAENDER, 1 = LINKS, 2 = RECHTS
      DO 10 i1 = 1, 2

        !UT  FEHLERKENNWERT SCHLEIFE 10
        !UT  i10(1 = LINKS, i10(2) = RECHTs
        i10 (i1) = 0

        !UT  BETRACHTUNG LINKES VORLAND
        IF (i1 .eq. 1) then
          iii = itrli
          ik = itrli - 1
          ifact = itr_typ_li
          isch = ischl

          !****  D.Bley 12.1.98 eingefuegt:
          IF (itrli .eq. 1) then
            ikli = 1
          ELSE
            ikli = itrli - 1
          ENDIF

        !UT  BETRACHTUNG RECHTES VORLAND
        ELSE
          iii = itrre-1
          ik = itrre
          ifact = itr_typ_re
          isch = ischr

        !UT  ENDIF ZU (i1.eq.1)
        ENDIF

        !UT  SETZE STARTWERT SCHLEIFENZAEHLER FUER
        !UT  SCHLEIFE 1000, TRENNFLAECHENGESCHW.
        num = 0

        !UT  MITWIRKENDE BREITE FUER BEIDE VORLAENDER NULL SETZEN
        bmwvor (i1) = 0.0


        !JK   FALLUNTERSCHEIDUNG
        !JK   ------------------
        ! Boeschung mit und ohne Bewuchs, ifact = TRENNFLAECHENTYP

        !UT  SUB lcase konvertiert von Klein- in Grossbuchstaben
        CALL lcase (ifact)

        IF (ifact (1:1) .eq.'b') then
          ! Boeschung mit Bewuchs, Fall 5+6
          !JK MUSS DAS NICHT FALL 4+5 SEIN??? => JA!!! UT, 31.08.00

          !UT  SETZE FAKTOR ZUR BERECHNUNG DES VORLANDES
          fakt = 1.7

        ELSE

          !UT  SETZE FAKTOR ZUR BERECHNUNG DES VORLANDES
          !  Vorland mit Bewuchs, Fall 1+2+7
          fakt = 1.0

        ENDIF

        200 continue

        !JK  WENN TRAPEZ-,KREIS-,MAUL-,EIPROFIL
        IF (iprof (1:1) .ne.' ') then

          !UT               LAMBDA TRENNFLAECHE LINKS NULL
          l_tr (1) = 0.0

          !UT               TRENNFLAECHENGESCHW. LINKS = 0
          v_tr (1) = 0.0

          !UT               LAMBDA TRENNFLAECHE RECHTS NULL
          l_tr (2) = 0.0

          !UT               TRENNFLAECHENGESCHW. RECHTS = 0
          v_tr (2) = 0.0

          !UT               FALLUNTERSCHEIDUNG GERINNEART
          ifall = 1

          !UT               PUNKTNUMMER ANFANG FLUSSCHLAUCH
          ianf_hg = max (itrli, ischl)
          !UT               PUNKTNUMMER ENDE FLUSSCHLAUCH
          iend_hg = min (itrre, ischr)

          CALL sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr,  &
             & l_tr, isener, l_hg, v_hg, ifall, ianf_hg, iend_hg, itere2, i1,&
             & if_so, formbeiwert(2) )

          !UT  ABFLUSS LINKES(1) UND RECHTES(2) VORLAND
          qvor1 = 0.0
          qvor2 = 0.0

          !UT  DA FALL TRAPEZ-,KREIS-,MAUL-,EIPROFIL ENDE SUB
          RETURN

        !UT            ELSEIF ZU if (iprof(1:1).ne.' ') then
        ELSE IF (isch .ge. itrli .and. isch .le. itrre) then
          ! Keine Trennflaechen, da Wsp unter Bordvoll
          ! Fall 6-8

          ! D.Bley 12.1.98 eingefuegt
          u_tr (i1) = 0.0

          l_tr (i1) = 0.0
          v_tr (i1) = 0.0
          ifall = 1

          IF (itere2 .le. 1 .and. i1 .eq. 1 .and. iter5 .le. 1) l_tr (2) = l_tr (i1)

          ianf_hg = max (itrli, ischl)
          iend_hg = min (itrre, ischr)

          CALL sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr,  &
             & l_tr, isener, l_hg, v_hg, ifall, ianf_hg, iend_hg, itere2, i1,&
             & if_so, formbeiwert(2) )


        !JK  beide Vorlaender ohne Bewuchs, FALL 3
        ELSE IF (abs (dp (ikli) ) .le. 1.e-04 .AND. abs(dp(itrre)) &
             & .le. 1.e-04 .AND. itrli .gt. ischl .AND. itrre .lt. ischr) then

          ifall = 22

          !write (*,*) 'In PASCHE. Beide Vorlaender ohne Bewuchs! IFALL = ', ifall

          !UT  PUNKTNUMMER ANFANG FLUSSCHLAUCH
          ianf_hg = max (itrli, ischl)
          !UT  PUNKTNUMMER ENDE FLUSSCHLAUCH
          iend_hg = min (itrre, ischr)


          CALL sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr,  &
             & l_tr, isener, l_hg, v_hg, ifall, ianf_hg, iend_hg, itere2, i1,&
             & if_so, formbeiwert(2) )


        !JK            Vorland ohne Bewuchs, FALL 2
        ELSE IF (abs (dp(ik) ) .le. 1.e-04) then

          ifall = 2

          !write (*,*) 'In PASCHE. Vorland ohne Bewuchs! IFALL = ', ifall

          !UT               IM FALL LINKES VORLAND
          IF (itere2 .le. 1 .and. i1 .eq. 1 .and. iter5 .le. 1) then
            l_tr (2) = l_tr (i1)
          ENDIF

          !UT               PUNKTNUMMER ANFANG FLUSSCHLAUCH
          ianf_hg = max (itrli, ischl)
          !UT               PUNKTNUMMER ENDE FLUSSCHLAUCH
          iend_hg = min (itrre, ischr)

          CALL sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr,  &
                     & l_tr, isener, l_hg, v_hg, ifall, ianf_hg, iend_hg, itere2, i1,&
                     & if_so, formbeiwert(2) )

        ELSE IF (v_tr(i1) .le. 1.e-03 .and. v_ks(ik) .le. 1.e-06) then

          ! Dieser Fall tritt nur auf, wenn der Vorlandbereich so gering
          ! eingestaut ist, dass die Fliessgeschwindigkeiten dort fast null
          ! sind.
          ! In diesem Fall ist es nicht sinnvoll den Einfluss der Trennflaech
          ! zu beruecksichtigen, so dass das Profil wie ein kompaktes Profil
          ! ohne Trennflaechen gerechnet wird.
          ! Daher direkter Sprung in die Subroutine SOHLEF.

          ifall = 3

          !write (*,*) 'In PASCHE. Geringes v auf Vorland! IFALL = ', ifall

          IF (itere2 .le. 1 .and. i1 .eq. 1 .and. iter5 .le. 1) then
            l_tr (2) = l_tr (i1)
          ENDIF

          ianf_hg = max (itrli, ischl)
          iend_hg = min (itrre, ischr)

          CALL sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr,  &
             & l_tr, isener, l_hg, v_hg, ifall, ianf_hg, iend_hg, itere2, i1,&
             & if_so, formbeiwert(2) )

        !UT   ELSE ZU (iprof(1:1).ne.' ')
        ELSE

          ifall = 4

          !write (*,*) 'In PASCHE. Beide Vorlaender mit Bewuchs! IFALL = ', ifall
          !write (*,*) 'In Pasche. Trennfl.geschw. V_TR(i1) = ', v_tr(i1)

          !UT  TRENNFLAECHENGESCHWINDIGKEIT
          vxn = 0.0


          !JK  ITERATIVE BESTIMMUNG DER TRENNFL.-GESCHWINDIGKEIT,
          !JK  SCHLEIFE 1000 AN TRENNFLAECHE v_tr(i1) LINKS/RECHTS
          DO 1000 WHILE(abs (v_tr(i1) - vxn) .gt. 0.015)

            !UT  SETZE STARTWERT FEHLERKENNWERT SCHLEIFE 1000
            ip1000 = 0

            !UT  MITTELUNG ALTE UND NEUE TRENNFL.-GESCHW.
            !WP  Erweiterung um staerkere Unterrelaxation 02.02.2006
            if (num < 10) then
              v_tr (i1) = (v_tr (i1) + vxn) / 2.
            else
              v_tr (i1) = (2*v_tr (i1) + vxn) / 3.
            end if

            !WP  BERECHNUNG CW-WERT
            cwun = GET_CW_UN (v_tr (i1), dp (ik), nue)
            !write (*,*) 'In Pasche. IFALL = 4, NUM = ', num, '  CWUN = ', cwun

            !WP Initialisierung fuer die Nachlauflaenge
            IF (itere2.eq.1.and.iter1.le.1.and.iter5.le.1.and.num.le.1) then
              !WP Erste Schaetzung der Nachlauflaenge
              axj2 = cwun * dp (ik) * (0.033**c)
            ENDIF
            axn2 = axj2 * 0.90

            ! BERECHNE NACHLAUFLAENGE, BWK, S.36, FORMEL (38)
            axn2 = GET_A_NL (cwun, dp(ik), isener, v_tr (i1), axn2, g)
            anl_l (i1) = axn2

            ! BERECHNE NACHLAUFBREITE, BWK, S.36, FORMEL (39)
            b05f 	= GET_A_NB (cwun, dp(ik), axn2)
            anb_l (i1) 	= b05f

            ! BERECHNUNG BEWUCHSPARAMETER, BWK, S.37 FORMEL (37)
            ! BEWUCHSPARMETER, 1=links, 2=rechts
            omega 	= GET_OMEGA (axn2, ax(ik), b05f, ay(ik))
            om_l (i1) 	= omega

            ! BERECHNUNG ct, FORMEL 42, BWK, S.36
            ct 		= GET_CT (omega)
            ct_l (i1) 	= ct

            !UT  SCHUBSPANNUNG IN TRENNFLAECHE (rho = 1000)
            taut = rho * (v_tr (i1) / ct) **2

            !UT  BERECHNUNG SCHUBSPANNUNGSGESCH. IN DEN TRENNFLAECHEN
            !UT  MIT OBIGER ZEILE = FORMEL 43, BWK, S.36
            vstt = (taut / rho) **0.5

            !UT  BER. DER MITWIR. VORLANDBREITE, FORMEL 41, BWK, S.36
            !UT  DURCH AUFRUF SUB MITVOR, DORT NUR DIESE FORMEL
            bmwvor(i1) = GET_MITVOR(ct, dp(ik), ax(ik), u_tr(i1), l_ks(ik))
            
            !write (UNIT_OUT_LOG, 7000) i1, ik, bmwvor(i1), ct, dp(ik), ax(ik), u_tr(i1), l_ks(ik)
            !7000 format (1X, 'In PASCHE. Nach GET_MITVOR. i1 = ', I5, '  ik = ', I5, /, &
            !           & 1X, 'BMWVOR(i1) = ', F10.4, /, &
            !           & 1X, 'ct         = ', F10.4, /, &
            !           & 1X, 'dp(ik)     = ', F10.4, /, &
            !           & 1X, 'ax(ik)     = ', F10.4, /, &
            !           & 1X, 'u_tr(i1)   = ', F10.4, /, &
            !           & 1X, 'l_ks(ik)   = ', F10.4)

            !UT  WENN MITWIRK. BREITE DES VORL. > BREITE VORLAND
            !UT  DANN MITWIRK. BREITE DES VORL. = BREITE VORLAND
            IF (bmwvor(i1) .gt. b_vor(i1)) bmwvor(i1) = b_vor(i1)

            !UT  ZAEHLEN ITERATIONEN SCHLEIFE 1000 GRENZFL.-GESCHW
            num = num + 1

            !UT  MITWIRKENDE BREITE FLUSSCHLAUCH bf
            !UT  FALLS NULL, SETZE AUF HALBE FLUSSBREITE
            IF (abs (bf (i1) ) .lt. 1.e-04) then

              bf (1) = b_hg / 2.
              bf (2) = b_hg / 2.

            ENDIF

            !UT  BERECHNUNG LAMBDA, LINKES (1), RECHTES (2) VORLAND
            !UT  BWK, S.35, FORMEL 36
            !ST  Widerstandsbeiwerte der Trennflächen
            l_tr (i1) = (1. / ( -2.03 * log10(omega * c3*   (fakt*bmwvor(i1)/bf(i1)) **c4) ) ) **2

            !WP Begrenzung des Trennflaechenwiderstandes
            IF (l_tr (i1) .gt. 2.D0) THEN
              l_tr (i1) = 2.D0
            END IF

            IF (itere2.eq.1.and.iter1.le.1.and.iter5.le.1.and.i1.eq.1) then
              l_tr (2) = l_tr (i1)
            ENDIF

            !WP ik ist Knoten links von der linken Trennflaeche oder Knoten der rechten Trennflaeche
            !IF (ik.gt.0) then
            !  IF (l_ks (ik) .gt. 1.e-04) then
            !    ctfak = u_tr (i1) / l_ks (ik)
            !  ELSE
            !    !UT  SCHREIBEN IN KONTROLLFILE
            !    write (UNIT_OUT_LOG, '(''Berechnung von ctfak nicht moeglich. l_ks(ik) < 1.e-04!'' )')
            !    write (UNIT_OUT_LOG, 8902) ik, l_ks(ik), i1, u_tr(i1), l_tr(i1)
            !    8902 format (1X, 'IK=', i3, ' l_ks(ik) = ', F10.6, ' I1 = ', I3, ' u_tr(i1) = ', F10.5, ' l_tr(i1) = ', F10.5)
            !  ENDIF
            !ELSE
            !  !UT     SCHREIBEN IN KONTROLLFILE
            !  write (UNIT_OUT_LOG, '(''Berechnung von ctfak nicht moeglich. ik <= 0!'')')
            !ENDIF

            ianf_hg = max (itrli, ischl)
            iend_hg = min (itrre, ischr)

            !UT AUFRUF SOHLF, BER. LAMBDA, UND GESCHW. FLUSSS. i
            CALL sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr,  &
               & l_tr, isener, l_hg, v_hg, ifall, ianf_hg, iend_hg, itere2, i1,&
               & if_so, formbeiwert(2) )

            !write (*,*) 'In Pasche. nach SOHLEF, IFALL = 4, NUM = ', num
            !WRITE (*,*) 'V_HG = ', v_hg, '  l_tr(i1) = ', l_tr(i1)

            !UT  SCHUBSPA.GESCHW.[m/s], BWK, S.36, FORMEL (44)
            vstt = ( (l_tr (i1) / 8.) * v_hg**2) **0.5

            !UT                 TRENNFLAECHEN GESCHW. [m/s], FORMEL (43)
            vxn = ct * vstt
            !write (*,*) 'CT = ', ct, '   VSTT = ', vstt
            !write (UNIT_OUT_LOG,*) ' In PASCHE. vxn = ', vxn

            vt_n (i1) = vxn

            !UT                 SCHLEIFE 1000, MEHR ALS 10mal => ENDE
            !UT                 TRENNFLAECHENGESCHWINDIGKEIT
            IF (num .gt. 20) then

              !UT                    SETZEN FEHLERKENNZAHL SCHLEIFE 1000
              ip1000 = 1000

              write(UNIT_OUT_LOG,9002) num, v_tr(i1), vxn
              9002 format(/1X, 'Keine konvergenz in schleife V_TR (sub pasche)!', /, &
                         & 1X, 'Nach ',I3,' Iterationen betraegt die ', /, &
                         & 1X, 'Differenz V_TR_alt = ',F10.4, ' V_TR_neu = ',F10.4)
              write(UNIT_OUT_LOG,8901) iter1, i1, v_tr(i1), axn2, b05f, omega, &
                              & ct, bmwvor(i1), bf(1), bf(2), l_tr(i1)
              8901 FORMAT (/1X, 'ITER1 = ', I7, /, &
                          & 1X, 'I1    = ', I7, /, &
                          & 1X, 'V_TR  = ', F7.4, /, &
                          & 1X, 'AXN2  = ', F7.4, /, &
                          & 1X, 'B05F  = ', F7.4, /, &
                          & 1X, 'OMEGA = ', F7.4, /, &
                          & 1X, 'CT    = ', F7.4, /, &
                          & 1X, 'BMW   = ', F7.4, /, &
                          & 1X, 'BF(1) = ', F7.4, /, &
                          & 1X, 'BF(1) = ', F7.4, /, &
                          & 1X, 'LAMT  = ', F7.4 )

              !JK   VERLASSEN DER 1000-SCHLEIFE,
              !JK   DA KEINE KONVERGENZ NACH 10 DURCHLAEUFEN
              GOTO 3100

            !UT                 ENDIF ZU if (num.gt.10)
            ENDIF                                                                          
                                                                        

          !WP Ende der DO 1000-Schleife
          1000 CONTINUE

          !JK               ENDE BESTIMMUNG DER FLIESSGESCHWINDIGKEI
          !JK               AN TRENNFLAECHE v_tr(i1)
                                                                        
        !JK  ENDIF ZU if(iprof(1:1) .ne. ' ')
        ENDIF 
                                                                        
        3100 CONTINUE

        !     erweiterung um ausgabe einer fiktiven rauheitshoehe k-t
        !     nach formel (7) des dvwk-fortbildungsheftes 13

        IF (bmwvor (i1) .gt. 1.e-04) then 
          rhytr (i1) = l_tr (i1) / l_hg * r_hg 
          a_kt(i1) = 0.854 * rhytr(i1) * omega * (1.7 * bmwvor(i1) / bf(i1) ) **c4
          dqvor(i1) = 0.25 * u_tr(i1) * bmwvor(i1) * ABS(v_tr(i1) - v_ks(ik) )
          !WP 12.11.2007
          !WP Der Lambda-Wert des Vorlandes kann nur von den Bewuchsparametern,
          !WP der Sohlrauheit und dem Energieliniengefälle abhängen, nicht von der Fließgeschwindigkeit!
          !WP Da das Gefälle und die Wassertiefe vorgegeben wird, ist eine Anpassung
          !WP des Lambda-Wertes auf dem ersten Vorlandabschnitt nicht sinnvoll und
          !WP führt auch zu unsinnigen Ergebnissen und schlechter Konvergenz!!!!
          !l_ks(ik) = 8. * g * r_ks(ik) * isener / ( ( (dqvor(i1) + q_ks(ik)) /a_ks(ik)) **2.D0)
        ELSE 
          a_kt (i1) = 0. 
          dqvor (i1) = 0. 
        ENDIF 
                                                                        

        !UT            ALLE DREI GROESSEN = MITTL. FLIESSGESCHW. IM FLUSSS.
        IF (i1.eq.1) vffa = v_hg 
        IF (i1.eq.1) v1_hg = vffa 
                                                                        
        !UT            BERECHNUNG EINER FEHLERKENNZAHL
        i10 (i1) = ip2000 + ip1000 


      !WP Ende der DO 10-Schleife-------------------------------------------------
      10 END DO
      !WP ------------------------------------------------------------------------

      !write (*,*) 'In PASCHE. Ende DO 10 Schleife. V_HG = ', v_hg


      IF (iter5 .gt. 20) then 
                                                                        
        !UT            SETZE FEHLERKENNZAHL
        ip5 = 10000 
                                                                        
        write(UNIT_OUT_LOG,9003) iter5, vffa, v_hg
        9003 format(/1X, 'Keine konvergenz in schleife 5 (sub pasche)!', /, &
                   & 1X, 'Nach ',I3,' Iterationen betraegt die ', /, &
                   & 1X, 'Differenz V_HG_alt = ',F10.4, ' V_HG_neu = ',F10.4)

        !JK  ZU VERLASSEN DER 5-SCHLEIFE,
        !JK  DA KEINE KONVERGENZ NACH 20 DURCHLAEUFEN
        GOTO 3110 
                                                                        
      !UT         ENDIF ZU (iter5.gt.20)
      ENDIF 
                                                                        
      ! write(UNIT_OUT_LOG,'(3i2,11f8.4)')itere2,iter1,iter5,hr,vffa,v_hg, &
      !   & bf(1),bf(2),l_tr(1),l_tr(2),l_hg,l_ks(itrli),bmwvor(1), &
      !   & bmwvor(2)
                                                                        

    !WP Ende der DO 5-Schleife----------------------------------------------------
    5 CONTINUE
    !WP --------------------------------------------------------------------------
                                                                        

  !UT      EINSPRUNGLABEL NICHTKONVERGENZ 5er SCHLEIFE
  3110 CONTINUE
                                                                        
  !write (*,*) 'In PASCHE. Ende DO 5 Schleife. V_HG    = ', v_hg
  !write (*,*) 'In PASCHE. Ende DO 5 Schleife. V_TR(1) = ', v_tr(1)

  !UT  BERECHNETE TRENNFLAECHENRAUHEIT BEIDE SEITEN = NULL
  !JK  => KEINE TRENNFLAECHEN
  IF (l_tr (1) .le. 1.e-06 .and. l_tr (2) .le. 1.e-06) then

    !UT MITWIRKENDE BREITE = HALBE FLUSSSCHLAUCHBREITE
    !UT FORMEL 44b, BWK S.37,
    bf (1) = b_hg / 2.0
    bf (2) = b_hg / 2.0

    !UT VERHAELTNIS WIDERSTANDSBEIWERTE
    vlam = 0.0
    !UT VERHAELTNIS MITWIRKENDE BREITE
    vbmwv = 0.0

  !JK  NUR KEINE TRENNFLAECHE LINKS
  ELSE IF (l_tr (1) .le. 0.0) then

    !UT  MITWIRKENDE BREITE LINKS NULL
    bf (1) = 0.0
    !UT  MITWIRKENDE BREITE RECHTS = FLUSSSCHLAUCHBREITE
    bf (2) = b_hg
    !UT  VERHAELTNIS WIDERSTANDSBEIWERTE
    vlam = 0.0
    !UT  VERHAELTNIS MITWIRKENDE BREITE
    vbmwv = 0.0

  !JK  NUR KEINE TRENNFLAECHE RECHTS
  ELSE IF (l_tr (2) .le. 0.0) then

    !UT  MITWIRKENDE BREITE LINKS = FLUSSSCHLAUCHBREITE
    bf (1) = b_hg
    !UT  MITWIRKENDE BREITE RECHTS NULL
    bf (2) = 0.0
    !UT  VERHAELTNIS WIDERSTANDSBEIWERTE
    vlam = 0.0
    !UT  VERHAELTNIS MITWIRKENDE BREITE
    vbmwv = 0.0

  !JK  ZWEI TRENNFLAECHEN
  ELSE

    !UT  VERHAELTNIS MITWIRKENDE BREITE
    vbmwv = bf (2) / bf (1)

    !UT  VERHAELTNIS WIDERSTANDSBEIWERTE
    vlam = l_tr (2) / l_tr (1)

    !UT  MITTELWERT VERHAELTNIS WIDERSTANDSBEIWERTE
    !UT  UND VERHAELTNIS MITWIRKENDE BREITE
    vlabw = 0.5 * (vlam + vbmwv)

    !UT  ERRECHNE DARAUS MITWIRKENDE BREITE LINKS
    !UT  WELCHE FORMEL???, 31.08.2000
    bf (1) = b_hg / (1. + vlabw)

    !UT  DARAUS RES. MITWIRKENDE BREITE RECHTS
    bf (2) = b_hg - bf (1)

    !UT  SCHLEIFE 1 KONVERGIERT NICHT
    IF (iter1 .gt. 10) then

      !UT  MITWIRKENDE BREITE BEI NICHTKONVERGENZ
      bfn1 = b_hg / (vlabw + 1.)

      !UT  SETZE FEHLERKENNZAHL SCHLEIFE 1, DA iter1>10
      ip1 = 100000

      !JK  VERLASSEN DER 1-SCHLEIFE,
      !JK  DA KEINE KONVERGENZ NACH 10 DURCHLAEUFEN
      GOTO 3200

    !UT  ENDIF ZU if (iter1.gt.10)
    ENDIF

  !UT ENDIF ZU if (l_tr(1).le.1.e-06.and.l_tr(2).le.1.e-06)
  ENDIF                                                                         
                                                                        

!WP ENDE HAUPTSCHLEIFE----------------------------------------------------------
1 CONTINUE
!WP ----------------------------------------------------------------------------
                                                                        

!UT   ==> FALLS SCHLEIFE 1 KONVERGIERT, GEHT ES HIER WEITER
                                                                        
!UT   ==> EINSPRUNGLABEL FALLS SCHLEIFE NICHT KONVERGIERT               
3200 CONTINUE
                                                                        


!UT   ERMITTLUNG DER ABFLUESSE VORLANF RECHTS (2) UND LINKS (1)         
!UT   BEIDE PARAMETER WERDEN DIREKT ZURÜCK UEBERGEBEN                   
qvor1 = MAX (0., dqvor (1) )
qvor2 = MAX (0., dqvor (2) )
                                                                        
!JK   FEHLERZAHLBERECHNUNG, Ggf. AUSGABE DER FEHLERZAHLEN               
if_pa = ip5 + ip1 + i10 (1) + 2 * i10 (2)
                                                                        
!UT      HIER WAR MAL DIE AUSGABE DER FEHLERZAHLEN AKTIVIERT, 29.08.00 U
!        write(10,'(f10.2)')if_pa                                       
!        close(10)                                                      

END SUBROUTINE pasche
