!     Last change:  WP   30 May 2005    9:43 am
!--------------------------------------------------------------------------
! This code, alpha_dru.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - alpha_dru
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


!********************************************************************** 
SUBROUTINE alpha_dru (stat, wsp, hen, up, qs)
!HB
!HB   26.11.2001 - H.Broeker                                            
!HB   ----------------------                                            
!HB                                                                     
!HB   Beschreibung                                                      
!HB   ------------                                                      
!HB   Diese Subroutine bereitet Daten zur Ausgabe des Energiestrom- und 
!HB   Boussinesq-Beiwertes vor. Die Werte beziehen sich jeweils auf ein 
!HB   Gesamtprofil.                                                     
!HB   Die Daten werden in Datei Beiwerte.AUS geschrieben.               
!HB                                                                     
!HB   direkt uebergebene Parameter                                      
!HB   ----------------------------                                      
!HB   hen      -- Gesamtenergiehoehe                                    
!HB   qs       -- Abfluss Teilquerschnitt                               
!HB   stat     -- Stationskilometer                                     
!HB   up       -- benetzter Umfang Teilquerschnitt                      
!HB   wsp      -- Wasserspiegelhoehe                                    
!HB                                                                     
!HB   weitere Parameter          (k.B.= keine Bedeutung in dieser SUB)  
!HB   -----------------                                                 
!HB   alpha_IW -- Impulsbeiwert bzw. Boussinesq-Beiwert                 
!HB   alpha_EW -- Energiestrombeiwert                                   
!HB   alph_aus -- Projektpfad der Ausgabedatei Beiwerte.AUS (k.B.)      
!HB   g        -- Gravitationskonstante                                 
!HB   gefaelle -- Sohlgefaelle fuer Anfangsprofil (positiver Wert)      
!HB   gesamt_a -- Gesamte durchstroemte Flaeche                         
!HB   geschw_g -- Gesamtgeschwindigkeit (V=Q/A)                         
!HB   g_sohl   -- manuell eingegebenes Sohlgefaelle fuer Anfangsprofil  
!HB   i        -- Zaehler der Profilnummern                             
!HB   i_zaehl  -- Zaehler der Teilquerschnitte (Vorland,Fluss,Vorland)  
!HB   lambda_g -- Lambda des Gesamtquerschnittes                        
!HB   maxkger  -- Parameter fuer Laenge einer Zeichenkette              
!HB   maxkla   -- Parameter fuer Laenge einer Zeichenkette              
!HB   nr_alph  -- Interne Dateinummer fuer Beiwerte.AUS                 
!HB   pn_alpha -- Profilnummer                                          
!HB   st_alpha -- Stationskilometer                                     
!HB   trenn_li -- benetzter Umfang der Trennflaeche links (=u_tr(1))    
!HB   trenn_re -- benetzter Umfang der Trennflaeche rechts (=u_tr(2))   
!HB               (benetzter Umfang einer Trennflaeche entspricht       
!HB                benetzte Hoehe der Trennflaeche)                     
!HB   umfang_g -- benetzter Gesamtumfang inklusiv ev. Trennfaechen      
!HB                                                                     
!HB   ***************************************************************** 

USE DIM_VARIABLEN
USE KONSTANTEN

! COMMON-Block /ALPH_PF/ -----------------------------------------------------------
INTEGER 		:: nr_alph
CHARACTER(LEN=nch80)    :: alph_aus     ! Projektpfad fuer Beiwerte.AUS
COMMON / alph_pf / alph_aus, nr_alph
! ----------------------------------------------------------------------------------



!HB   Variablen zur Uebergabe des Energiestrombeiwertes und des         
!HB   Boussineq-Beiwertes                                               
INTEGER pn_alpha
REAL st_alpha, alpha_EW (maxkla), alpha_IW (maxkla), gesamt_a (maxkla)
COMMON / nr_alpha / pn_alpha, st_alpha, gesamt_a, alpha_EW, alpha_IW
                                                                        
!HB   Uebergabe Sohlgefaelle aus SUB NORMBER                            
REAL g_sohl
COMMON / gef_sohl / g_sohl
                                                                        




INTEGER i, i_zaehl
REAL gefaelle, umfang_g (maxkla), geschw_g (maxkla), lambda_g (maxkla)
REAL stat (maxger), wsp (maxger), hen (maxger), up (maxger, maxkla)
REAL qs (maxger)
                                                                        
!HB   30.11.2001 - H.Broeker                                            
!HB   Die Hoehen der beiden Trennflaechen werden aus SUB PASCHE         
!HB   uebergeben.                                                       
REAL trenn_li, trenn_re
COMMON / h_trenn / trenn_li, trenn_re
!            PRINT*,'trenn_li, trenn_re',trenn_li,trenn_re              
                                                                        
                                                                        
!HB   ***************************************************************** 
!HB   Berechnung                                                        
!HB   ***************************************************************** 
      i = pn_alpha 
!HB   Das Gefaelle soll als positiver Wert in die Berechnung eingehen.  
!HB   Bei stationaer gleichfoermigem Abfluss, der bei diesen speziellen 
!HB   Berechnugsdurchlaeufen vorausgesetzt wird, entspricht das         
!HB   Sohlgefaelle dem Energieliniengefaelle. Es kann deshalb bei der   
!HB   u.a. Berechnung fuer ein gesamt_Lambda angesetzt werden.          
      gefaelle = SQRT ( (g_sohl**2) ) 
!          PRINT*,'gefaelle:',gefaelle                                  
!          PRINT*,'i:',i                                                
!HB   benetzter Gesamtumfang des durchstroemtem Bereiches               
      umfang_g (i) = 0 
                                                                        
!HB   Berechnung des benetzten Gesamtumfangs je Profil                  
!HB   d.h. (Vorland_links+Fluss+Vorland_rechts; noch ohne Trennflaechen)
      DO 100i_zaehl = 1, 3 
      umfang_g (i) = umfang_g (i) + up (i, i_zaehl) 
!          PRINT*,'umfang:',umfang_g(i)                                 
  100 CONTINUE 
                                                                        
!HB   30.11.2001 - H.Broeker                                            
!HB   Zum benetzten Gesamtumfang werden die benetzten Umfaenge bzw.     
!HB   Hoehen der Trennflaechen addiert                                  
      umfang_g (i) = umfang_g (i) + trenn_li + trenn_re 
                                                                        
!HB   Berechnung der Gesamtgeschwindigkeit: V=Q/A                       
      geschw_g (i) = qs (i) / gesamt_a (i) 
                                                                        
!HB   Berechnung des lambda-Widerstandsbeiwertes fuer das               
!HB   Gesamt-Profil                                                     
!HB   Berechnung:  lambda_gesamt=(I*A**3*8*g)/(l_gesamtumfang*Q**2)     
      lambda_g (i) = (gefaelle * gesamt_a (i) **3 * 8 * g) / (umfang_g (&
      i) * qs (i) **2)                                                  
                                                                        
!HB   ***************************************************************** 
!HB   Ausdruck der Werte                                                
!HB   ***************************************************************** 
                                                                        
!HB   Schreiben der Werte in die Datei Beiwerte.AUS                     
      WRITE (nr_alph, 500) stat (i), wsp (i), hen (i), lambda_g (i),    &
      gesamt_a (i), umfang_g (i), geschw_g (i), qs (i), alpha_EW (i),   &
      alpha_IW (i), gefaelle                                            
!HB   Formatbeschreibung der Ausgabezeile                               
  500 FORMAT(/,f10.3,t12,f14.6,t27,f14.6,t45,f12.6,t58,f13.6,           &
     &t72,f14.6,t87,f9.6,t97,f11.6,t109,f12.8,t122,f12.8,t139,f7.5)     
!      print*,'stat(i),wsp(i),hen(i),lambda_g(i),gesamt_a(i),'          
!      print*,'umfang_g(i),geschw_g(i),qs(i),alpha_EW(i),alpha_IW(i)'   
!      print*,stat(i),wsp(i),hen(i),lambda_g(i),gesamt_a(i)             
!      print*,umfang_g(i),geschw_g(i),qs(i),alpha_EW(i),alpha_IW(i)     
                                                                        
!HB   ***************************************************************** 
                                                                        
!HB   ENDE SUB ALPHA_DRU                                                
      END SUBROUTINE alpha_dru                      
