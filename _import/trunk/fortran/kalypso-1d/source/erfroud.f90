!     Last change:  WP   26 May 2005   10:11 am
!--------------------------------------------------------------------------
! This code, erfro.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - erfroud
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


SUBROUTINE erfroud (br, f, Q_teil, lu, iprof, indfl, indmax, froud)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE ERFROUD                                                
!**                                                                     
!**   BESCHREIBUNG: BERECHNUNG DER FROUD-ZAHL
!**                                                                     
!**                                                                     
!**   DIREKT UEBERGEBENE PARAMETER                                      
!**   ----------------------------                                      
!**   br       BREITE
!**   f        FLAECHE
!**   Q_teil   TEILABFLUSS
!**   lu       BENETZTER UMFANG
!**   iprof    PROFILNUMMER ?                                           
!**   indfl    ZAHL DER RAUHIGKEITSZONEN HIER????                       
!**   indmax - Anzahl Rauhigkeitszonen im Profil [-]                    
!**   froud  - FROUDZAHL                                                
!**                                                                     
!**   WEITERE PARAMETER                                                 
!**   -----------------                                                 
!**   froudi(maxkla) -                                                  
!**   ifg            - 1=Darcy, 0=Strickler, bei bordvoll,              
!**                    aus Commonblock/p4/                              
!**   ii             - ZAEHLPARAMETER                                   
!**   indfl          - siehe oben                                       
!**   tm             - max. Rhydr im Profil?, ERGEBNIS IN common/dd/    
!**   tmi(maxkla)    - Rhydr im Profilabschnitt?                        
!**   vm             - max. Geschw. im Profil?, ERGEBNIS IN common/dd/  
!**   vmi(maxkla)    - v im Profilabschnitt?                            
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**                                                                     
!**   br      --      Wasserspiegelbreite                               
!**   f       --      durchströmte Fläche                               
!**   froud   --      Froud-Zahl                                        
!**   froudi  --      Teilfroud-Zahl                                    
!**   ifg     --      Art der Widerstandsbeiwertberechnung nach         
!**                   Darcy-Weisbach oder Gauckler-Manning-Strickler    
!**   indfl   --      Abschnitte nach Darcy-Weisbach                    
!**   indmax  --      Rauheitsabschnitte                                
!**   iprof   --      Art des Profils                                   
!**   Q_teil      --      Teilabfluß
!**   tm      --      mittlere Tiefe                                    
!**   tmi     --      mittlere Tiefe eines Rauheitsabschnittes          
!**   lu       --      benetzter Umfang
!**   vm      --      mittlere Fließgeschwindigkeit                     
!**   vmi     --      mittlere Geschwindigkeit eines Rauheitsabschnittes
!**                                                                     
!**
!***********************************************************************
                                                                        
USE DIM_VARIABLEN
USE KONSTANTEN

!Calling variables
CHARACTER(LEN=1), INTENT(IN) :: iprof
                                                                        
REAL, INTENT(IN) :: br (maxkla), f (maxkla), Q_teil (maxkla)
REAL, INTENT(IN) :: lu (maxkla)
                                                                        
!lokal variables
REAL :: tmi (maxkla), froudi (maxkla)
REAL :: vmi (maxkla)

COMMON / dd / tm, vm
COMMON / p4 / ifg, betta
                                                                        
!ep     Erweiterung auf Grenztiefenberechnung für Rohre mit Teilabfluß g
!       Schneider, Bautabellen, S. 13.29                                
COMMON / ro_fr / term1, term2, winkel, durchm_fr

!ST deaktiviert: der Winkel wird in dem COMMON ro_fr schon in [rad] übergeben und in [°] wird er nicht benötigt
!**   Winkel-Umrechnungsfaktor:                                         
!rad = 180.0 / pi
                                                                        
!ep   05.02.2002                                                        
!     Initialisierung des Feldes tmi(maxkla)                            
                                                                        
DO ii = 1, maxkla
   tmi (ii) = 0.
END DO
                                                                        
                                                                        
!**   ------------------------------------------------------------------
!**   BERECHNUNGEN                                                      
!**   ------------------------------------------------------------------
                                                                        
!     Ermittlung der Froudzahl:                                         
                                                                        
froud = 0.
                                                                        
!**   HAUPTSCHLEIFE, indmax - Anzahl Rauhigkeitszonen im Profil [-]
Hauptschleife: DO ii = 1, indmax
                                                                        
    !**   BERECHNUNG der mittleren Fließtiefe tmi=Flaeche/Breite, WENN br > 0
    !JK   WENN NORMALPROFIL
    IF (iprof.eq.' ') then
       IF (br (ii) .gt.0.01) then
          tmi (ii) = f (ii) / br (ii)
       ENDIF
    !JK   WENN TRAPEZ-,KREIS-,MAUL-,EIPROFIL
    ELSEIF (iprof.eq.'k') then
      IF (br (ii) .gt.0.01) then
         termq = (term1 - term2) / SIN (0.5 * winkel)
         v_gr = g * durchm_fr * termq / 8.
         tmi (ii) = v_gr / g
         IF (v_gr.gt.1.e-04) then
            v_gr = SQRT (v_gr)
         ELSE
            WRITE ( *, 9001) winkel, term1, term2, v_gr, durchm_fr, tmi (ii), f (ii), br (ii)
            9001 FORMAT      ('Fehler in Froud-Zahlberechnung Rohr',8f10.4)
            STOP 'erfroud 9001'
         ENDIF
      ELSE
         tmi (ii) = 0.
      ENDIF
    ELSE
      IF (br (ii) .gt.0.01) then
         tmi (ii) = f (ii) / lu (ii)
      ELSE
         tmi (ii) = 0.
      ENDIF
    ENDIF
                                                                        
    !**   BERECHNUNG DER GESCHWINDIGKEIT vmi=Q/A, FALLS f null, vmi = null
    IF (f (ii) .gt.1.e-6) then
       vmi (ii) = Q_teil (ii) / f (ii)
    ELSE
       vmi (ii) = 0.
    ENDIF
                                                                        
    !**   BERECHNUNG VON FROUD NACH v/Wurzel(Rhydr*g)
    IF (tmi (ii) .gt.0.001) then
       froudi (ii) = vmi (ii) * vmi (ii) / (tmi (ii) * g)
       froudi (ii) = sqrt (froudi (ii) )
    ELSE
       froudi (ii) = 0.
    ENDIF
                                                                        
    !UT   froud WURDE ZU PROGRAMMBEGINN = null gesetzt
    IF (froudi (ii) .gt.froud) then
       froud = froudi (ii)
       indfl = ii
    ENDIF
END DO Hauptschleife

                                                                        
                                                                        
!**   ifg=1, bei bordvoll nach Darcy, dann froud=froudi(2)???           
IF (iprof.eq.' '.and.ifg.eq.1) then
!JK      FLUSSSCHLAUCH => indfl = 2?                                    
    indfl = 2
    froud = froudi (indfl)
ENDIF
                                                                        
                                                                        
vm = vmi (indfl)
tm = tmi (indfl)
                                                                        
!**    ------------- AM 19.01.00 BEREITS DEAKTIVIERT, UT                
!      tm=fges/brges                                                    
!      vm=q/fges                                                        
!      froud=(vm*vm)/(tm*9.81)                                          
!      froud=froudi(indfl)                                              
!**    -------------------                                              
                                                                        
END SUBROUTINE erfroud
