!     Last change:  WP   30 May 2005   10:09 am
!--------------------------------------------------------------------------
! This code, fuente_s.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - fuent_s
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
!     DK, 31/05/01 - Sub FUENT_S was modified to have the same structure
!         of the program as in Sub LINDY (Colebrook-White formula taken 
!         Accordingly, list of arguments of the subroutine shortened!   
SUBROUTINE fuent_s (rhynn, k_ks, dm, ln_ks)

!**   JANA KIEKBUSCH                                                    
!**   SUB FUENT_S, START BEARBEITUNG AM 16. Juni 2000                   

!***********************************************************************
!**                                                                     
!**   SUBROUTINE FUENT_S                                                
!**                                                                     
!**   ALLGEMEINE BESCHREIBUNG:                                          
!**   ------------------------                                          
!**   DIESES PROGRAMM BERUECKSICHTIGT BEI DER                           
!**   BERECHNUNG DES WIDERSTANDSBEIWERTES DIE RAUHEIT DES FLUSSCHLAUCHS.
!**   DANACH WIRD BEI HOHER RAUHEIT NACH AGUIRRE-PE/FUENTES GERECHNET.  
!**   BEI NIEDRIGER RAUHEIT WIRD NACH BWK-MERKBLATT GERECHNET.          
!**                                                                     
!**   LITERATUR: - BWK-MERKBLATT                                        
!**              - BATHURST: FLOW RESISTANCE ESTIMATION IN MOUTAIN RIVER
!**                          JOURNAL OF HYDRAULIC ENGINEERING,          
!**                          VOL. 111, NO.4, 1985, 625-643              
!**              - AGUIRRE-PE/FUENTES:                                  
!**                          RESISTANCE TO FLOW IN STEEP ROUGH STREAMS  
!**                          JOURNAL OF HYDRAULIC ENGINEERING,          
!**                          VOL. 116, NO.11, 1990, 1374-1387           
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**     v_hg     = MITTLERE GESCHWINDIGKEIT IM FLUSSCHLAUCH [m/s]       
!**     l_ks(i)  = WIDERSTANDSBEIWERT (LAMBDA) DES TEILABSCHNITTES      
!**                DES FLUSSCHLAUCHES [-]                               
!**     k_ks(i)  = RAUHEIT DES TEILABSCHNITTES DES FLUSSCHLAUCHES [m]   
!**     rhynn(i) = HYDRAULISCHER RADIUS TEILABSCHNITTES                 
!**                DES FLUSSCHLAUCHES [m]                               
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**     bettaw       - WAKE-PARAMETER AUS AGUIRRE-PE/FUENTES [-]        
!**     alphat       - TEXTUR-PARAMETER AUS AGUIRRE-PE/FUENTES [-]      
!**     dm           - MITTLERER DURCHMESSER DER RAUHEITSELEMENTE [m]   
!**     grenzkrit    - GRENZKRITERIUM NACH BATHURST FUER TEILABSCHNITT  
!**                    DES FLUSSCHLAUCHES                               
!**     resof        - REYNOLDSZAHL DES TEILABSCHNITTES DES FLUSSCHLAUCH
!**                    [-]                                              
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**     - keine                                                         
!**                                                                     
!***********************************************************************

!**   ------------------------------------------------------------------
!**   VEREINBARUNGSTEIL                                                 
!**   ------------------------------------------------------------------
REAL dm, alphat, bettaw
REAL rhynn, k_ks, ln_ks
                                                                        
!**   ------------------------------------------------------------------
!**   BERECHNUNGEN                                                      
!**   ------------------------------------------------------------------
                                                                        
!**   SETZEN DER VARIABLEN FUER BERECHNUNG NACH AGUIRRE-PE/FUENTES      
bettaw = 0.3
alphat = 6.8
                                                                        
!**   MITTLERER KORNDURCHMESSER (FAKTOR KOENNTE ggf. EINGEFUEGT WERDEN) 
dm = k_ks
                                                                        
!***  BERECHNUNG REYNOLDSZAHL, BWK FORMEL 16a, S.20                     
!      resof = v_hg*4*rhynn/nue                                         
!DK   21/05/01 - "nue" put for 1.31e-06!                                
                                                                        
!***  ABFRAGE DES GRENZKRITERIUMS FUER RAUHEITSFORM NACH BATHURST       
!      if (grenzkrit.le.4.) then                                        
                                                                        
!**        FORMEL NACH AGUIRRE-PE/FUENTES                               
ln_ks = (1. / (0.88 * bettaw * dm / rhynn + 2.03 * alog10 (11.1 * &
           & rhynn / (alphat * dm) ) ) ) **2
                                                                        
!      else                                                             
                                                                        
!**        FORMEL NACH BWK-MERKBLATT, S.20, Formel 16 mit 4*3.71=14.84  
!UT        ABER OHNE FORMFAKTOR                                         
!           ln_ks = (1./(-2.03*alog10(2.51/resof/(l_ks**0.5)+           
!     +     k_ks/(4.*rhynn*3.71))))**2                                  
                                                                        
!      endif                                                            
                                                                        
!***********************************************************************
!     Setting code for printing warning!                                
!     Main channel                                                      
IF (rhynn / k_ks.ge.20) ifum = 1
!***********************************************************************

!UT   ENDE subroutine fuent_s                                           
end subroutine fuent_s

!     DK, 31/05/01 - Sub FUENT_S was modified to have the same structure
!         of the program as in Sub LINDY (Colebrook-White formula taken 
!         Accordingly, list of arguments of the subroutine shortened!   
!***********************************************************************


