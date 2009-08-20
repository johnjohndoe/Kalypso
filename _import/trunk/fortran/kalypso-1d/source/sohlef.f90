!     Last change:  MD    8 Jul 2009    4:50 pm
!--------------------------------------------------------------------------
! This code, sohlef.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - sohlef
!
! Copyright (C) 2004-2007  ULF TESCHKE & WOLF PLOEGER.
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

SUBROUTINE sohlef (u_hg, r_hg, a_ks, l_ks, u_ks, k_ks, r_ks, u_tr, &
                 & l_tr, isener, l_hg, v_hg, ifall, itrli, itrre, &
                 & itere2, i1, if_so, fbw)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE SOHLEF                                                 
!**                                                                     
!JK   ALLGEMEINE BESCHREIBUNG:                                          
!**   ------------------------                                          
!**   BERECHNUNG WIDERSTANDSBEIWERT ln_ks UND HYDRAULISCHER RADIUS r_ks 
!JK   FUER JEDE RAUHIGKEITSZONE IM FLUSSSCHLAUCH                        
!**                                                                     
!**
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**   dellam  --      Grenzbedingung                                    
!**   delrh   --      Grenzbedingung                                    
!**   dhy1    --      hydraulischer Durchmesser                         
!**   dm      --                                                        
!**   factor  --      Faktor zur Berechnung des Widerstandsbeiwertes
!**   ln_ks   --      Widerstandsbeiwert eines Teilabschnittes
!**   r_ks    --      hydraulischer Radius eines Teilabschnittes
!**   resof   --      Reynoldszahl                                      
!**   rhynn   --      hydraulischer Radius eines Teilabschnittes        
!**   sumdd   --      Grenzbedingung                                    
!**   sumdel  --      Grenzbedingung
!**
!**                                                                     
!JK   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   - fuent_s (wenn gewuenscht)                                       
!**                                                                     
!***********************************************************************



! ------------------------------------------------------------------
! Vereinbarungsteil
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE BEWUCHS
USE IO_UNITS
USE MOD_INI

! Calling Variables
REAL, INTENT (IN) :: u_hg               ! benetzter Unfang des Flussschlauchs
REAL, INTENT (IN) :: r_hg               ! hydraulischer Radius des Flusschlauchs
REAL, INTENT (IN) :: isener             ! Energieliniengefälle
REAL, INTENT (IN) :: a_ks (maxkla)         ! Fließquerschnitt einer Teilfläche (wird nicht verwendet)
REAL, INTENT (IN) :: u_ks (maxkla)          ! benetzter Unfang einer Teilfläche
REAL, INTENT (IN) :: k_ks (maxkla)         	! ks-Wert einer Teilfläche
REAL, DIMENSION(1:2), INTENT (IN) :: u_tr 	! benetzter Umfang der Trennflächen
INTEGER, INTENT(IN) :: ifall            	! Fallunterscheidung der Gerinneart
INTEGER, INTENT(IN) :: itrli            	! Punktnummer der linken Trennfläche
INTEGER, INTENT(IN) :: itrre            	! Punktnummer der rechten Trennfläche
INTEGER, INTENT(IN) :: itere2           	! ???
INTEGER, INTENT(IN) :: i1               	! Fallunterschiedung Bewuchsseite
REAL, INTENT(IN) :: fbw            		! Formbeiwert fuer den Flussschlauch

! Output variables
REAL, INTENT (OUT) :: l_hg              	! Widerstandsbeiwert im Flussschlauch
REAL, INTENT (OUT) :: v_hg              	! mittlere Fließgeschwindigkeit im Flussschlauch
REAL, INTENT (OUT) :: l_ks (maxkla)        	! Widerstandsbeiwert einer Teilfläche
REAL, INTENT (OUT) :: r_ks (maxkla)        	! hydraulischer Radius einer Teilfläche
REAL, DIMENSION(1:2), INTENT (INOUT) :: l_tr  	! Widerstandsbeiwert der Trennfläche
INTEGER, INTENT(OUT) :: if_so           	! Fehlerkennzahl

! Local variables
REAL :: GET_LAMBDA                      ! Aufruf REAL function für lambda
REAL :: dhynn                           ! Fließdurchmesser einer Teilfläche
REAL :: ln_ks (maxkla)                  ! Widerstandsbeiwert einer Teilfläche
REAL :: rhynn (maxkla)                  ! hydraulischer Radius einer Teilfläche
REAL :: dellam (maxkla)                 ! Grenzbedingung für DO-Schleife lambda
REAL :: delrh (maxkla)                  ! Grenzbedingung für DO-Schleife rhy
REAL :: dm (maxkla)                     ! mittlerer Durchmesser der Rauheitselemente
REAL,PARAMETER :: accuracy = 1.0E-5     ! Genauigkeitsschranke fuer REAL Variablen

! COMMON-Block /AUSGABELAMBDA/ -----------------------------------------------------
REAL, DIMENSION(maxkla) :: lambda_teilflaeche
CHARACTER(LEN=nch80) 	:: lambdai
COMMON / ausgabelambda / lambda_teilflaeche, lambdai
! ----------------------------------------------------------------------------------


! COMMON-Block /FLEXI/ -------------------------------------------------------------
! DK, 21/06/01 - The user-defined value of MEI (for Kouwen procedure)
REAL 		:: mei (maxkla)
COMMON / flexi / mei
! ----------------------------------------------------------------------------------


! COMMON-Block /PRI/ ----------------------------------------------------------
! DK, 07/06/01 - Parameters for printing warnings and comments!
INTEGER         :: ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
COMMON / pri / ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
! -----------------------------------------------------------------------------


! COMMON-Block /ROUGH/ --------------------------------------------------------
! DK, 21/06/01 - Parameters of bottom roughness type and grass type!
CHARACTER(LEN=4) :: tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
COMMON / rough / tbr1, tbr2, tbr3, tgr1, tgr2, tgr3
! -----------------------------------------------------------------------------



! ----------------------------------------------------------------------
! Initialisierung
! ----------------------------------------------------------------------

DO i = itrli, itrre-1
  !WP 11.11.2005
  !WP Für die Ausgabe der Widerstandsbeiwerte jedes einzelnen Abschnittes
  !WP im Flussschlauch in DRUCKTAB, lambd_teilflaeche wird zunaechst = 0 gesetzt
  lambda_teilflaeche(i) = 0.0
  !WP 11.11.2005
END DO


v_hg = 0.0
l_hg = 0.0

!WP 29.04.2005
!WP WICHTIG: Auch wenn die Trennflaechenrauheit l_tr in dieser
!WP Subroutine berechnet wird, darf sie doch nicht am Anfang
!WP zu Null initialisiert werden! Es kann sonst zu unvorhergesehenen
!WP Programm-Abbruechen kommen!
!WP Offenbar werden fuer die Berechnung die alten Werte teils noch
!WP mit uebernommen
!l_tr(1) = 0.0
!l_tr(2) = 0.0

if_so = 0

ibed = 2

                                                                        
! ------------------------------------------------------------------
! BERECHNUNGEN                                                      
! ------------------------------------------------------------------


!UT   ZUWEISUNG VON KONSTANTEN AUF DIE FELDERWERTE                      
!UT   LAMBDA(i) UND HYDRAUL. RADIUS
!ST   STARTWERTE FÜR DIE ITERATIONEN
DO i = itrli, itrre-1
   ln_ks(i) = 0.05
   rhynn(i) = r_hg
END DO
                                                                        
!UT   STARTBEDINGUNGEN SCHLEIFE 3000
sumdd = 1000.
iter3 = 0
                                                                        
!JK   START BERECHNUNGSSCHLEIFE 3000 BIS ENDE SUBROUTINE (MAXIMAL 20 Durchläufe)
!JK   ---------------------------------------------------------------
!ST   Berechnung des hydraulischen Radius für jeden Teilquerschnitt
DO 3000 WHILE(abs (sumdd) .gt. 0.005)

   !UT     SETZEN DES FEHLERKENNWERTES
   is3000 = 0
                                                                        
   !UT     HOCHZAEHLEN DER ITERATIONSSCHRITTE SCHLEIFE 3000
   iter3 = iter3 + 1

   !UT     SETZE ABWEICHNUG NULL, SCHLEIFE 3000
   sumdd = 0.
                                                                        
   !UT     STARTBEDINGUNGEN SCHLEIFE 2000
   iter2 = 0
   sumdel = 1000.
                                                                        
   !JK       BERECHNUNGSSCHLEIFE FUER WIDERSTANDSBEIWERT
   !JK       ----------------------------------------------------
   !ST  Lambda für jeden Teilabschnitt und gesamt = Sohle
   DO 2000 WHILE(abs (sumdel) .gt. 0.001)

      !UT       SETZEN DES FEHLERKENNWERTES SCHLEIFE 2000
      is2000 = 0

      !UT       HOCHZAEHLEN DER ITERATIONSSCHRITTE SCHLEIFE 2000
      iter2 = iter2 + 1
      sumdel = 0.
      l_hg = 0.
                                                                        
                                                                        
      !******************************************************************
      !      DK, 31/05/01
      !      PRINT *,'Sub SOHLEF - entered the cycle for lambda!'
      !      PRINT *,'iter2=',iter2
      !      PRINT *
      !******************************************************************
                                                                        
                                                                        
     !UT       FELDERZUWEISUNG DER WERTE
     DO i = itrli, itrre-1
        l_ks(i) = ln_ks(i)
        l_hg    = l_hg + l_ks(i) * u_ks(i)
     END DO
                                                                        
     !ST  Abfrage: Art der Gerinnegliederung und Zuweisung der Trennflächenrauheiten
     !JK       WENN GEGLIEDERTES GERINNE OHNE VORLANDBEWUCHS
     IF (ifall .eq. 22) then

        l_tr (1) = l_ks (itrli)
        l_tr (2) = l_ks (itrre-1)
                                                                        
     !JK       WENN GEGLIEDERTES GERINNE MIT EINSEITIGEM BEWUCHS
     ELSE IF (ifall .eq. 2) then

        IF (i1 .eq. 1) then
           l_tr (i1) = l_ks (itrli)
        ELSE
           l_tr (i1) = l_ks (itrre-1)
        ENDIF

     ENDIF
                                                                        
     !UT       ABFRAGE OB UEBERHAUPT EIN BENETZTER UMFANG VORLIEGT
     IF ( (u_hg + u_tr(1) + u_tr(2) ) .le. accuracy) then
        !WP 10.11.2004, Erweiterung der Fehlermeldung um Punkt b)
        write (*,2001)
        2001 format (/1X, 'Es ist ein Fehler aufgetreten: ', /, &
                  & 1X, '------------------------------ ', /, &
                  & 1X, 'Die Querschnittsflaeche des Flussschlauches wird zu', /, &
                  & 1X, 'NULL berechnet, obwohl Wasserspiegel > HMIN liegt !', /, &
                  & 1X, 'Moegliche Fehler:',/,&
                  & 1X, 'a) Trennflaechen falsch gesetzt ?', /, &
                  & 1X, 'b) Abfluss zu gering (Q < 0.001) ?', //, &
                  & 1X, 'Programm wird beendet!')
        call stop_programm(11)
     ENDIF

     !ST Summe über alle Teilflächen lambda * benetzter Umfang (Superpositionsprinzip)
     !write (*,*) 'In SOHLEF. Zeile 296. L_HG = ', l_hg
     !WP l_hg = l_hg + l_tr (1) * u_tr (1) + l_tr (2) * u_tr (2)

     !          l_hg = l_hg/(u_hg+u_tr(1)+u_tr(2))
     !***********************************************************************
     !     DK, 31/05/01
     !     It happens that l_tr=0 and u_tr>0! In that case, u_tr should be ex
     !     from calculation of l_hg! Therefore, the following lines are intro
     !     instead of the line above:
     ku1 = 1
     ku2 = 1
     IF (ABS(l_tr(1)) < accuracy) then
       ku1 = 0
     END IF  
     IF (ABS(l_tr(2)) < accuracy) then
       ku2 = 0
     END IF  

     l_hg = l_hg + l_tr(1)*u_tr(1)*ku1 + l_tr(2)*u_tr(2)*ku2

     !ST lambda_gesamt = (Summe lambda_i * lu_i) / lu_gesamt = lambda_So

     l_hg = l_hg / (u_hg + ku1*u_tr(1) + ku2*u_tr(2) )

                                                                       
     !UT       SCHREIBEN IN KONTROLLDATEI, FALLS LAMBDA NULL -> Programmabbruch
     IF (l_hg < accuracy) then

        !write (UNIT_OUT_LOG, 1000) i, itere2, iter3, iter2, l_hg, v_hg, ln_ks(i), rhynn(i)
        write (UNIT_OUT_LOG, 1001)

        !write (0, 1000) i, itere2, iter3, iter2, l_hg, v_hg, ln_ks(i), rhynn(i)
        !write (*, 1000) i, itere2, iter3, iter2, l_hg, v_hg, ln_ks(i), rhynn(i)
        write (0, 1001)
        write (*, 1001)

        1001 format (/1X, 'Fehler bei der Berechnung des Widerstandsbeiwertes!', /, &
                    & 1X, 'Lambda Fluss < 1.0E-6.', //, &
                    & 1X, 'Hinweis: Wenn dieser Fehler bei der Berechnung einer', /, &
                    & 1X, 'Bruecke auftreten sollte, bitte Folgendes beachten:', /, &
                    & 1X, ' - Auf den Vorlaendern neben einer Bruecke darf es keine Senken geben!', /, &
                    & 1X, ' - Falls dies der Fall ist muessen alle tiefer liegenden Knoten', /, &
                    & 1X, '   zunindest auf die gleiche Hoehe wie der Anschlusspunkt der', /, &
                    & 1X, '   Brueckenoberkante an das Gelaende angehoben werden!')

        !1000 format (/1X, 'Fehler bei der Berechnung des Widerstandsbeiwertes!', /, &
        !            & 1X, 'SUB SOHLEF, Zeile 380.', /, &
        !            & 1X, 'Lambda Fluss < 1.0E-6.', /, &
        !            & 1X, 'Abschnitt i = ', I3, /, &
        !            & 1X, 'ITERE2 = ', I3, /, &
        !            & 1X, 'ITER3 =  ', I3, /, &
        !            & 1X, 'ITER2 =  ', I3, /, &
        !            & 1X, 'L_HG =   ', ES14.5, '  (LAMBDA Hauptgerinne)', /, &
        !            & 1X, 'V_HG =   ', F15.10, '  (Fliessgeschw. Hauptger.)', /, &
        !            & 1X, 'LN_KS =  ', F15.10, '  (LAMBDA Teilabschnitt i)', /, &
        !            & 1X, 'RHYNN =  ', F15.10, '  (hdr. Radius Teilabschnitt i)' )
        call stop_programm(0)

     ENDIF
                                                                        
                                                                        
     !UT       BERECHNE MITTLERE FLIESSGESCHWINDIGKEIT IM FLUSSCHLAUCH v_hg
     !UT       z.B. UMGESTELLTE FORMEL 6, BWK, S.15
     v_hg = (8 * g * r_hg * isener / l_hg) **0.5
     
     !write (UNIT_OUT_LOG,1009) isener, r_hg, l_hg, v_hg
     !1009 format(1X, 'Gefaelle I             = ', F10.6, /, &
     !          & 1X, 'hydr. Radius r_hg      = ', F10.5, /, &
     !          & 1X, 'lambda ges l_hg        = ', F10.5, /, &
     !          & 1X, 'mittl. Fl.Geschw. v_hg = ', F10.5)

     !write (UNIT_OUT_LOG,1010) 'Iteration3', 'Iteration2', 'Abschnitt', 'R_HY', 'LAMBDA'
     !1010 format (/1X, A12, A12, A12, A12, A12)

     all_points_in_main_ch: DO i = itrli, itrre-1
                                                                        
        IF (ln_ks (i) .le. accuracy) then
           write (UNIT_OUT_LOG, '(3A5,4A10)') 'IT2', 'IT3', 'IT2', 'l_ges', 'v_m', 'ln_ks' , 'rhy'
           write (UNIT_OUT_LOG, '(3I5,4F10.6)') itere2, iter3, iter2, l_hg, v_hg, ln_ks (i) , rhynn (i)
           PRINT * , 'Fehler bei ca. Zeile 215 in SUB SOHLEF. Beim Entwickler melden'
           PRINT * , 'ln_ks(i).le.1.e-05, ln_ks(i) = ', ln_ks (i)
           PRINT * , 'Fehler bei ca. Zeile 215 in SUB SOHLEF. Beim Entwickler melden'
           call stop_programm(0)
        ENDIF
                                                                        
        IF (k_ks (i) .le. accuracy) then
           WRITE ( * , '(''Fehler in der Rauhigkeitsdefinition.'')')
           WRITE ( * , '(''Rauhigkeit ist gleich Null an Station'',i3)') i
           WRITE ( * , '(''nicht uebergehbarer Fehler in SUB SOHLEF'')')
           call stop_programm(0)
        ENDIF
                                                                        
        
        IF (tbr2 .eq. 'd-50') then

          ! ------------------------------------------------------------
          ! CALCULATION AFTER FUENTES
          ! ------------------------------------------------------------
          CALL fuent_s (rhynn (i), k_ks (i), dm (i), ln_ks (i) )

        ELSE IF (tbr2 .eq. 'h-g ') then

          ! ------------------------------------------------------------
          ! CALCULATION AFTER KOUWEN
          ! ------------------------------------------------------------
          dhynn = 4 * rhynn (i)

          CALL kouwen (mei (i), isener, k_ks (i), tgr2, dhynn, ln_ks (i) )

        ELSE IF (tbr2 .eq. 'k-s ') then

          !------------------------------------------------------------------
          ! CALCULATION AFTER COLEBROOK-WHITE with form factor
          !------------------------------------------------------------------

          !WP 09.11.2007 deaktiviert, weil ab Version 2.0.5
          !IF (grenzkrit(i) .lt. 1.66667) ks_iter = 0.60 * rhynn (i)

          !---------------------------------
          !ST 23.03.2005
          !Reynoldszahl wird in GET_LAMBDA berechnet, deaktiviert
          !WP resof (i) = v_hg * 4 * rhynn (i) / nue
          ln_ks(i)  = GET_LAMBDA(v_hg, rhynn(i), k_ks(i), fbw)
          !---------------------------------

          !WP 09.11.2007 folgende Zeile rausgenommen ab Version 2.0.5.1, weil nun 
          !WP auch extreme Rauheiten regrechnet werden koennen.
          !IF (grenzkrit (i) .lt.1.66667) icwm = 1

        ELSE

          PRINT * , 'Sub SOHLEF!'
          PRINT * , 'Name of bottom roughness type given improperly!'
          PRINT * , 'Modify the name in PRF file!'
          call stop_programm(0)

        ENDIF
                                                                        
        !write (UNIT_OUT_LOG, 1011) iter3, iter2, i, rhynn(i), ln_ks(i)
        !1011 format (1X, I12, I12, I12, F12.4, F12.7)
        
        !UT           BERECHNUNG DER GESAMTABWEICHUNG WIDERSTANDSBEIWERT
        dellam (i) = abs (ln_ks (i) - l_ks (i) )
        sumdel = sumdel + dellam (i)

     END do all_points_in_main_ch
                                                                        
     !UT           ABBRUCHKRITERIUM WENN ITERATION UEBER 20
     !WP IF (iter2 .gt. 20) then
     IF (iter2 .gt. 50) then

         is2000 = 1

         GOTO 1850

     ENDIF
                                                                        
   !JK       ENDE BERECHNUNG ln_ks(i) -------------------------------------
   2000 CONTINUE
                                                                        
                                                                        
   !JK     BERECHNUNG HYDRAULISCHER RADIUS r_ks(i)
   !JK     ---------------------------------------
                                                                        
   1850 CONTINUE

   DO i = itrli, itrre-1
     !JK         WENN 1. SCHLEIFENDURCHLAUF
     IF (iter3 .gt. 1) then
       r_ks (i) = (rhynn (i) + r_ks (i) ) / 2.
     !JK         WENN MEHRMALIGE SCHLEIFENDURCHLAEUFE
     ELSE
       r_ks (i) = rhynn (i)
     ENDIF
   END DO
                                                                        
   DO i = itrli, itrre-1
      rhynn (i) = ln_ks (i) / l_hg * r_hg
      delrh (i) = abs (rhynn (i) - r_ks (i) )
      !UT         BERECHNUNG SUMME ALLER ABWEICHUNGEN delrh(i)
      sumdd = sumdd+delrh (i)
   END DO
                                                                        
                                                                        
   !UT     ABBRUCHKRITERIUM SCHLEIFE 3000
   IF (iter3 .gt. 20) then

      !UT         BELEGEN DES FEHLERKENNWERTES
      is3000 = 10

      !UT         SUB ENDE - MUSS NICHT DIE FEHLERBERECHNUNG MIT REIN???,25080
      !UT         ALSO 3 ZEILEN VORHER
      GOTO 2050
                                                                        
   ENDIF
                                                                        
!JK   ENDE BERECHNUNGSSCHLEIFE 3000 ------------------------------------
3000 CONTINUE
                                                                        
!UT   BERECHNUNG FEHLERKENNZAHL DER SOHLE if_so                         
if_so = is2000 + is3000
                                                                        
DO i = itrli, itrre-1
  !WP 11.11.2005
  !WP Für die Ausgabe der Widerstandsbeiwerte jedes einzelnen Abschnittes
  !WP im Flussschlauch in DRUCKTAB
  lambda_teilflaeche(i) = ln_ks(i)
  !WP 11.11.2005
END DO

2050 RETURN
                                                                        
                                                                        
!UT   ENDE SUB SOHLF                                                    
END SUBROUTINE sohlef
                                                                        

