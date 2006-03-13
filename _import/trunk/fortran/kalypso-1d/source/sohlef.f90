!     Last change:  WP   12 Mar 2006    3:29 pm
!--------------------------------------------------------------------------
! This code, sohlef.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - sohlef
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
!**   i_typ_flg       --      Art der Widerstandsbeiwertberechnung
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

!---------------------
! ST 23.03.2005
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
REAL, DIMENSION(1:2), INTENT (OUT) :: l_tr  	! Widerstandsbeiwert der Trennfläche
INTEGER, INTENT(OUT) :: if_so           	! Fehlerkennzahl

! Local variables
REAL :: GET_LAMBDA                      ! Aufruf REAL function für lambda
REAL :: isenk                           ! Funktion des Energieliniengefälle
REAL :: dhynn                           ! Fließdurchmesser einer Teilfläche
REAL :: ks_iter                         ! abgeminderter ks-Wert einer Teilfläche
REAL :: ln_ks (maxkla)                  ! Widerstandsbeiwert einer Teilfläche
REAL :: rhynn (maxkla)                  ! hydraulischer Radius einer Teilfläche
REAL :: resof (maxkla)                  ! Reynoldszahl
REAL :: dellam (maxkla)                 ! Grenzbedingung für DO-Schleife lambda
REAL :: delrh (maxkla)                  ! Grenzbedingung für DO-Schleife rhy
REAL :: grenzkrit (maxkla)              ! GRENZKRITERIUM NACH BATHURST
REAL :: dm (maxkla)                     ! mittlerer Durchmesser der Rauheitselemente


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


! COMMON-Block /FLG_TYP/ -----------------------------------------------------------
! Unterscheidung offenes Gerinne (=pasche) oder Rohrströmung (= colebr)
CHARACTER(LEN=6):: i_typ_flg
COMMON / flg_typ / i_typ_flg
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

                                                                        
!**   ------------------------------------------------------------------
!**   BERECHNUNGEN                                                      
!**   ------------------------------------------------------------------


!UT   ZUWEISUNG VON KONSTANTEN AUF DIE FELDERWERTE                      
!UT   LAMBDA(i) UND HYDRAUL. RADIUS
!ST   STARTWERTE FÜR DIE ITERATIONEN
DO i = itrli, itrre-1
   ln_ks (i) = 0.0001
   rhynn (i) = r_hg
   !write (*,*) 'In SOHLEF. Zeile 194.  u_ks(',i,') = ', u_ks(i)
END DO

                                                                        
!***********************************************************************
!     DK, 31/05/01                                                      
!      PRINT *,'Entered Sub SOHLEF!'                                    
!      do 1001 i=itrli,itrre-1                                          
!          PRINT *,'i=',i,' ln_ks(i)=',ln_ks(i),' rhynn(i)=',rhynn(i)   
!1001  continue                                                         
!      PRINT *                                                          
!***********************************************************************
                                                                        
!***********************************************************************
!     DK, 31/05/01                                                      
!      PRINT *,'Beginning of Sub SOHLEF!'                               
!      PRINT *,'ibed=',ibed                                             
!      PRINT *,'icwl=',icwl,' ikol=',ikol                               
!      PRINT *,'icwm=',icwm,' ikom=',ikom,' ifum=',ifum                 
!      PRINT *,'icwr=',icwr,' ikor=',ikor                               
!      PRINT *                                                          
!***********************************************************************
                                                                        
                                                                        
!JK    WAR SCHON DEAKTIVIERT,30.04.00, JK                               
!      endif                                                            
                                                                        
!UT   STARTBEDINGUNGEN SCHLEIFE 3000
sumdd = 1000.
iter3 = 0
                                                                        
!JK   START BERECHNUNGSSCHLEIFE 3000 BIS ENDE SUBROUTINE (MAXIMAL 20 Durchläufe)
!JK   ---------------------------------------------------------------
!ST   Berechnung des hydraulischen Radius für jeden Teilquerschnitt
DO 3000 WHILE(abs (sumdd) .gt.0.005)

   !UT     SETZEN DES FEHLERKENNWERTES
   is3000 = 0
                                                                        
   !UT     HOCHZAEHLEN DER ITERATIONSSCHRITTE SCHLEIFE 3000
   iter3 = iter3 + 1

   !UT     SETZE ABWEICHNUG NULL, SCHLEIFE 3000
   sumdd = 0.
                                                                        
   !UT     STARTBEDINGUNGEN SCHLEIFE 2000
   iter2 = 0
   sumdel = 1000.
                                                                        
                                                                        
   !*********************************************************************
   !      DK, 31/05/01
   !      PRINT *,'Sub SOHLEF - entered the cycle for r_hy!'
   !      PRINT *,'iter3=',iter3
   !      PRINT *
   !*********************************************************************
                                                                        
                                                                        
   !JK       BERECHNUNGSSCHLEIFE FUER WIDERSTANDSBEIWERT
   !JK       ----------------------------------------------------
   !ST  Lambda für jeden Teilabschnitt und gesamt = Sohle
   DO 2000 WHILE(abs (sumdel) .gt.0.001)

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
        l_ks (i) = ln_ks (i)
        l_hg = l_ks (i) * u_ks (i) + l_hg
     END DO
                                                                        
                                                                        
     !********************************************************************
     !     DK, 31/05/01
     !     PRINT *,'Sub SOHLEF - Contribution of the main channel!'
     !     do i=itrli,itrre-1
     !       PRINT *,'i=',i
     !       print *,'l_ks(i)=',l_ks(i),' u_ks(i)=',u_ks(i),' l_hg=',l_hg
     !     end do
     !     PRINT *
     !     PRINT *,'l_hg=',l_hg
     !     PRINT *
     !*******************************************************************
                                                                        
     !ST  Abfrage: Art der Gerinnegliederung und Zuweisung der Trennflächenrauheiten
     !JK       WENN GEGLIEDERTES GERINNE OHNE VORLANDBEWUCHS
     IF (ifall.eq.22) then
        l_tr (1) = l_ks (itrli)

        !***********************************************************************
        !  DK, 12/06/01 - IMPORTANT: l_ks(itrre) changed to l_ks(itrre-1)!!!
        !  In previous case, it takes lambda value of the first subsection in
        !  right floodplain, which is not correct and is more expressed when
        !  lambda in that subsection is high (calculated using Kouwen procedu
        !  Now, it takes lambda value for the main channel!
        l_tr (2) = l_ks (itrre-1)
        !***********************************************************************
                                                                        
     !JK       WENN GEGLIEDERTES GERINNE MIT EINSEITIGEM BEWUCHS
     ELSEIF (ifall.eq.2) then

        IF (i1.eq.1) then
           l_tr (i1) = l_ks (itrli)
        ELSE
           l_tr (i1) = l_ks (itrre-1)
        ENDIF

     ENDIF
                                                                        
     !UT       ABFRAGE OB UEBERHAUPT EIN BENETZTER UMFANG VORLIEGT
     IF ( (u_hg + u_tr (1) + u_tr (2) ) .le.1.e-06) then
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
     !write (*,*) 'In SOHLEF. Zeile 334. L_HG = ', l_hg
     l_hg = l_hg + l_tr (1) * u_tr (1) + l_tr (2) * u_tr (2)

     !          l_hg = l_hg/(u_hg+u_tr(1)+u_tr(2))
     !***********************************************************************
     !     DK, 31/05/01
     !     It happens that l_tr=0 and u_tr>0! In that case, u_tr should be ex
     !     from calculation of l_hg! Therefore, the following lines are intro
     !     instead of the line above:
     ku1 = 1
     ku2 = 1
     IF (l_tr (1) .eq.0.) ku1 = 0
     IF (l_tr (2) .eq.0.) ku2 = 0

     !ST lambda_gesamt = (Summe lambda_i * lu_i) / lu_gesamt = lambda_So
     !write (*,*) 'In SOHLEF. Zeile 347. L_HG = ', l_hg

     l_hg = l_hg / (u_hg + ku1 * u_tr (1) + ku2 * u_tr (2) )
     !write (*,*) 'In SOHLEF. Zeile 350. L_HG = ', l_hg

     !*********************************************************************
                                                                        
                                                                        
     !*********************************************************************
     !     DK, 31/05/01
     !      PRINT *,'Sub SOHLEF - Contribution of imaginary walls!'
     !      PRINT *,'ifall=',ifall
     !      if (ifall.eq.22) then
     !          PRINT *,'l_ks(itrli)=',l_ks(itrli)
     !          print *,'l_ks(itrre-1)=',l_ks(itrre-1)
     !      end if
     !      if (ifall.eq.2) then
     !          PRINT *,'i1=',i1
     !          if (i1.eq.1) then
     !             PRINT *,'l_ks(itrli)=',l_ks(itrli)
     !          else
     !              PRINT *,'l_ks(itrre-1)=',l_ks(itrre-1)
     !          end if
     !      end if
     !      PRINT *
     !      print *,'l_tr(1)=',l_tr(1),' u_tr(1)=',u_tr(1)
     !      print *,'l_tr(2)=',l_tr(2),' u_tr(2)=',u_tr(2)
     !      PRINT *,'l_hg=',l_hg
     !      PRINT *
     !*********************************************************************
                                                                        
                                                                        
     !UT       SCHREIBEN IN KONTROLLDATEI, FALLS LAMBDA NULL -> Programmabbruch
     IF (l_hg.lt.1.e-06) then

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

     !write (*,*) 'In SOHLEF. V_HG = ', v_hg

     !***********************************************************************
     !     DK, 31/05/01
     !      PRINT *,'Sub SOHLEF!'
     !      PRINT *,'r_hg=',r_hg,'isener=',isener,'v_hg=',v_hg
     !      PRINT *
     !***********************************************************************

     !UT       SCHLEIFE 30, SCHREIBEN IN KONTROLLFILE
     !UT       - FALLS EIN LAMBDA(i)= Widerstandsbeiwert NULL IST
     !UT       - FALLS EIN RAUHEITSWERT(i) NULL IST
     !ST    -> Programmabbruch
     DO 30 i = itrli, itrre-1
                                                                        
        IF (ln_ks (i) .le.1.e-05) then
           write (UNIT_OUT_LOG, '(3i2,4f8.4)') itere2, iter3, iter2, l_hg, v_hg, ln_ks (i) , rhynn (i)
           PRINT * , 'Fehler bei ca. Zeile 215 in SUB SOHLEF. Beim Entwickler melden'
           PRINT * , 'ln_ks(i).le.1.e-05, ln_ks(i) = ', ln_ks (i)
           PRINT * , 'Fehler bei ca. Zeile 215 in SUB SOHLEF. Beim Entwickler melden'
           call stop_programm(0)
        ENDIF
                                                                        
        IF (k_ks (i) .le.1.e-06) then
           WRITE ( * , '(''Fehler in der Rauhigkeitsdefinition.'')')
           WRITE ( * , '(''Rauhigkeit ist gleich Null an Station'',i3)') i
           WRITE ( * , '(''nicht uebergehbarer Fehler in SUB SOHLEF'')')
           call stop_programm(0)
        ENDIF
                                                                        
                                                                        
        !*******************************************************************
        !             DK 31/05/01 - moved below !!!
        !              if (k_ks(i).gt.0.60*rhynn(i)) then
        !                  ks_iter = 0.60*rhynn(i)
        !              else
        !                  ks_iter = k_ks(i)
        !              endif
        !*******************************************************************
                                                                        
                                                                        
        !JK           WAR SCHON DEAKTIVIERT, 30.4.00, JK
        !             ln_ks(i)=(1./(-2.03*alog10(ks_iter/(4.*rhynn(i)*3.71))))**
                                                                        
        !JK           WENN BERECHNUNG NACH PASCHE
        !ST    OFFENES GERINNE
        IF (i_typ_flg.eq.'pasche') then

           !***********************************************************************
           !             DK 31/05/01 - deactivated and "call fuent_s" moved below!

           !**           *****************************************************
           !**           PROGRAMMERWEITERUNG, 16. JUNI 2000, JANA KIEKBUSCH
           !**           -----------------------------------------------------
           !**
           !**           BERECHNUNGSART BESTIMMEN:
           !**           ber_art_sohle = 'bwk' --> WIDERSTANDSGESETZ NACH BWK
           !**                                     MIT RAUHEITSUNTERSCHEIDUNG
           !**
           !**           sonst                 --> WIDERSTANDSGESETZ NACH PASCHE

           !UT                     LAEUFT NICHT MIT DIESER ERWEITERUNG AM 19.10.200
           !UT                     BEI BERECHNUNG VON PROJEKT VORFLUT
           !UT 191000              ber_art_sohle='bwk'

           !                 if (ber_art_sohle.eq.'bwk') then

           !                   call fuent_s(v_hg,l_ks(i),rhynn(i),k_ks(i),resof(i),
           !     +             grenzkrit(i),dm(i),ln_ks(i))

           !                   GOTO 222

           !                 else
           !                   continue
           !                 end if
           !**
           !**           ENDE PROGRAMMERWEITERUNG
           !**           ******************************************************

           !             DK 31/05/01 - deactivated and "call fuent_s" moved below!
           !***********************************************************************




           !***********************************************************************
           !     DK, 21/05/01 - CALCULATION OF THE FRICTION FACTOR using
           !     Colebrook-White formula, Kouwen or Fuentes procedure
           !
           !

           !**               BERECHNUNG GRENZKRITERIUM NACH BATHURST, BWK, S.24
           grenzkrit (i) = rhynn (i) / k_ks (i)

           !***********************************************************************
           !     DK, 31/05/01
           !      PRINT *,'Sub SOHLEF!'
           !      PRINT *,'i=',i
           !      print *,'rhynn(i)=',rhynn(i),' k_ks(i)=',k_ks(i)
           !      print *,'grenzkrit(i)=',grenzkrit(i)
           !      PRINT *
           !***********************************************************************

           IF (tbr2.eq.'d-50') then

              !**                   CALCULATION AFTER FUENTES
              CALL fuent_s (rhynn (i), k_ks (i), dm (i), ln_ks (i) )

           ELSEIF (tbr2.eq.'h-g ') then

              !**             CALCULATION AFTER KOUWEN

              !    PRINT *,'Sub SOHLEF!'
              !    PRINT *,'Application of Kouwen procedure in main channel
              !    PRINT *,'has not yet been checked!'
              !    PRINT *

              dhynn = 4 * rhynn (i)

              !***********************************************************************
              !     DK, 21/05/01
              !      PRINT *,'Sub SOHLEF - It is entering Sub KOUWEN with these data:'
              !      PRINT *,'ibed=',ibed,' isener=',isener,' k_ks(i)=',k_ks(i)
              !      print *,'tgr2=',tgr2,' dhynn=',dhynn,' mei(i)=',mei(i)
              !      PRINT *
              !***********************************************************************

              CALL kouwen (mei (i), isener, k_ks (i), tgr2, dhynn, ln_ks (i) )

              !***********************************************************************
              !     DK, 21/05/01
              !      PRINT *,'Sub SOHLEF - Passed Sub KOUWEN!'
              !      PRINT *,'isener=',isener,' k_ks(i)=',k_ks(i),' dhynn=',dhynn
              !      PRINT *,'ln_ks(i)=',ln_ks(i)
              !      PRINT *
              !***********************************************************************

              !**                   END OF CALCULATION AFTER KOUWEN

           ELSEIF (tbr2.eq.'k-s ') then

              !**                  CALCULATION AFTER COLEBROOK-WHITE
              ks_iter = k_ks (i)

              IF (grenzkrit (i) .lt.1.66667) ks_iter = 0.60 * rhynn (i)

              !---------------------------------
              !ST 23.03.2005
              !Reynoldszahl wird in GET_LAMBDA berechnet, deaktiviert
              resof (i) = v_hg * 4 * rhynn (i) / nue
              ln_ks(i)  = GET_LAMBDA(v_hg, rhynn(i), ks_iter, fbw)
              !---------------------------------

              !**                   FORMEL NACH BWK-MERKBLATT, S.20, Formel 16 mit
              !UT                   4*3.71=14.84, ABER OHNE FORMFAKTOR
              !                      ln_ks(i) = (1./(-2.03*alog10(2.51/resof(i)
              !     +                  /(l_ks(i)**0.5)+k_ks(i)/(4.*rhynn(i)*3.71))))**

              !---------------------------------
              !ST  23.03.2005 deaktiviert
              !ln_ks (i) = (1. / ( - 2.03 * alog10 (2.51 / resof (i)       &
              !&  / (l_ks (i) **0.5) + ks_iter / (4. * rhynn (i) * 3.71) ) ) ) ** 2
              !---------------------------------

              !***********************************************************************
              !     DK, 21/05/01
              !      PRINT *,'Sub SOHLEF - Colebrook-White formula applied!'
              !      PRINT *,'v_hg=',v_hg,' rhynn(i)=',rhynn(i),'resof(i)=',resof(i)
              !      PRINT *,'l_ks(i)=',l_ks(i),' ks_ite=',ks_iter,' dhynn=',4*rhynn(i
              !      PRINT *,'ln_ks(i)=',ln_ks(i)
              !      PRINT *
              !***********************************************************************

              !***********************************************************************
              !                     Setting code for printing warning!
              !                     Main channel
              IF (grenzkrit (i) .lt.1.66667) icwm = 1
              !***********************************************************************

              !**                   END OF CALCULATION AFTER COLEBROOK-WHITE

           ELSE

              PRINT * , 'Sub SOHLEF!'
              PRINT * , 'Name of bottom roughness type given improperly!'
              PRINT * , 'Modify the name in PRF file!'
              PRINT *

           ENDIF

           !
           !
           !     DK, 21/05/01 - CALCULATION OF THE FRICTION FACTOR using
           !     Colebrook-White formula, Kouwen or Fuentes procedure
           !***********************************************************************

           !***********************************************************************
           !      DK, 31/05/01
           !      PRINT *,'Sub SOHLEF!'
           !      PRINT *,'ln_ks(i)=',ln_ks(i)
           !      PRINT *
           !***********************************************************************



        !JK           BERECHNUNG NACH COLEBROOK von if (i_typ_flg.eq.'pasche')
        ELSE

           ! i_typ_flg /= 'pasche'
           ! ---------------------

           !write (*,*) 'In SOHLEF, bevor LAMBDA-Berechnung!'

           !UT              BERECHNUNG EINER ENERGIEFUNKTION?
           isenk = isener * (1. + isener**2.) **0.5

           !UT              BERECHNUNG Dhyd AUS Rhyd
           dhy1 = 4 * rhynn (i)
           !UT              BERECHNUNG HILFSFAKTOR
           factor = nue * 2.51 / (dhy1 * (2. * g * dhy1 * isenk) **0.5)

           !DK   16/05/01 - "nue" put for 1.31e-06 and "g" put for 9.81!

           !UT              BERECHNUNG REYNOLDSZAHL resof
           resof (i) = v_hg * 4 * rhynn (i) / nue

           !DK   16/05/01 - "nue" put for 1.31e-06!

           !UT              BERECHNUNG LAMBDA


           ln_ks(i) = (1. / (-2.0 * alog10(factor+k_ks(i) / (4. * rhynn(i)*3.71) ) ) ) ** 2

           !ln_ks(i)  = GET_LAMBDA(v_hg, rhynn(i), ks_iter, fbw)


           !                 ln_ks(i)=(1./(-2.03*alog10(2.51/resof(i)/
           !     1             (l_ks(i)**0.5)+k_ks(i)/(2.*rhynn(i)*3.71))))**2

           !UT           ENDIF VON if (i_typ_flg.eq.'pasche')
        ENDIF
                                                                        
                                                                        
        !UT       EINSPRUNGLABEL AENDERUNG KIEKBUSCH
        !222           continue
                                                                        
        !UT           BERECHNUNG DER GESAMTABWEICHUNG WIDERSTANDSBEIWERT
        dellam (i) = abs (ln_ks (i) - l_ks (i) )
        sumdel = sumdel + dellam (i)


     30 END do

                                                                        
                                                                        
     !UT           ABBRUCHKRITERIUM WENN ITERATION UEBER 20
     IF (iter2.gt.20) then

         is2000 = 1

         GOTO 1850

     ENDIF
                                                                        
   !JK       ENDE BERECHNUNG ln_ks(i) -------------------------------------
   2000 CONTINUE
                                                                        
                                                                        
   !***********************************************************************
   !     DK, 31/05/01
   !      PRINT *,'Sub SOHLEF - exited the cycle for lambda!'
   !      PRINT *,'iter2=',iter2
   !      PRINT *
   !***********************************************************************
                                                                        
                                                                        
   !JK     BERECHNUNG HYDRAULISCHER RADIUS r_ks(i)
   !JK     ---------------------------------------
                                                                        
   1850 CONTINUE

   DO 60 i = itrli, itrre-1
      !JK         WENN 1. SCHLEIFENDURCHLAUF
      IF (iter3.gt.1) then
         r_ks (i) = (rhynn (i) + r_ks (i) ) / 2.
      !JK         WENN MEHRMALIGE SCHLEIFENDURCHLAEUFE
      ELSE
         r_ks (i) = rhynn (i)
      ENDIF
   60 END DO
                                                                        
                                                                        
   !***********************************************************************
   !     DK, 31/05/01
   !      PRINT *,'Sub SOHLEF!'
   !      do 61 i=itrli,itrre-1
   !          PRINT *,'i=',i,' r_ks(i)=',r_ks(i)
   !61    continue
   !      PRINT *
   !***********************************************************************
                                                                        
   DO i = itrli, itrre-1
      rhynn (i) = ln_ks (i) / l_hg * r_hg
      delrh (i) = abs (rhynn (i) - r_ks (i) )
      !UT         BERECHNUNG SUMME ALLER ABWEICHUNGEN delrh(i)
      sumdd = sumdd+delrh (i)
   END DO
                                                                        
                                                                        
   !***********************************************************************
   !     DK, 31/05/01
   !      PRINT *,'Sub SOHLEF!'
   !      PRINT *,'l_hg=',l_hg,' r_hg=',r_hg
   !      do 51 i=itrli,itrre-1
   !          PRINT *,'i=',i
   !          print *,'l_ks(i)=',l_ks(i),' ln_ks(i)=',ln_ks(i)
   !          print *,'rhynn(i)=',rhynn(i),' r_ks(i)=',r_ks(i)
   !51    continue
   !      PRINT *
   !***********************************************************************
                                                                        
                                                                        
                                                                        
   !UT     ABBRUCHKRITERIUM SCHLEIFE 3000
   IF (iter3.gt.20) then

      !UT         BELEGEN DES FEHLERKENNWERTES
      is3000 = 10
                                                                        
                                                                        
      !JK         WAR SCHON DEAKTIVIERT, 30.4.00, JK
      !**     *      write(UNIT_OUT_LOG,9002)iter3,sumdd
      !**9002        format(/'konvergenzfehler in schleife 3000 [up sohlef]'/,
      !**     1             'nach ',i2,' iterationen betraegt der fehler noch'
      !**     2              f10.4)

      !UT         SUB ENDE - MUSS NICHT DIE FEHLERBERECHNUNG MIT REIN???,25080
      !UT         ALSO 3 ZEILEN VORHER
      GOTO 2050
                                                                        
   ENDIF
                                                                        
                                                                        
!JK   ENDE BERECHNUNGSSCHLEIFE 3000 ------------------------------------
3000 CONTINUE
                                                                        
                                                                        
!***********************************************************************
!     DK, 31/05/01                                                      
!      PRINT *,'Sub SOHLEF - exited the cycle for r_hy!'                
!      PRINT *,'iter3=',iter3                                           
!      PRINT *,'icwl=',icwl,' ikol=',ikol                               
!      PRINT *,'icwm=',icwm,' ikom=',ikom,' ifum=',ifum                 
!      PRINT *,'icwr=',icwr,' ikor=',ikor                               
!      PRINT *,'It is returning to PASCHE!'                             
!      PRINT *                                                          
!***********************************************************************
                                                                        
                                                                        
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
                                                                        

