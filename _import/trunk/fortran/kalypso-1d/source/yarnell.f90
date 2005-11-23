!     Last change:  WP   30 May 2005    1:41 pm
!--------------------------------------------------------------------------
! This code, yarnell.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - yarnell
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

SUBROUTINE yarnell (hpf, fges, q, xk) 

!***********************************************************************
!**                                                                     
!**   SUBROUTINE YARNELL                                                
!**                                                                     
!JK   BESCHREIBUNG: BERECHNUNG DES PFEILERSTAUS AN BRUECKEN             
!**                                                                     
!UT   THEORIE NACH WSPWIN-ANLEITUNG                                     
!UT   HINWEISE AUCH IM BWK-MERKBLATT                                    
!**                                                                     
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**   fges    --      Gesamtfläche (EINGABE)                            
!**   hpf     --      Pfeilerstauhöhe --> AUSGABE                       
!UT   q       --      ABFLUSS (EINGABE)                                 
!**   xk      --      Pfeilerformbeiwert (EINGABE)                      
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   alpha   --      Verbauverhältnis, AUS COMMONBLOCK PFEILERSTAU     
!**   bnetto  --      Nettobreite, AUS COMMONBLOCK PFEILERSTAU          
!**   fak     --      Faktor zur Pfeilerstauberechnung                  
!**   fges    --      Gesamtfläche                                      
!**   hpf     --      Pfeilerstauhöhe                                   
!**   hz      --      Bezugshöhe (idealisiert rechteckquerschnitt)      
!UT   iykenn  --      KENNWERT ZUR FALLUNTERSCHEIDUNG IN DER SUB        
!**   qges    --      Gesamtabfluß                                      
!**   vm3     --      mittlere Fließgeschwindigkeit von Brückenprofil 3 
!**   xk      --      Pfeilerformbeiwert                                
!**                                                                     
!**                                                                     
!JK   AUFGERUFENEN SUBROUTINEN                                          
!**   ------------------------                                          
!**   - KEINE                                                           
!**                                                                     
!***********************************************************************
                                                                        
                                                                        
                                                                        
      COMMON / pfeilerstau / alpha, bnetto 
      COMMON / ausgabeart / lein, jw8 
!     xk - pfeilerformbeiwert                                           
!     hz - bezugshoehe (idealisiert rechteckquerschnitt)                
                                                                        
      iykenn = 0 
                                                                        
!UT   BERECHNUNG DER BEZUGSHOEHE                                        
      hz = fges / bnetto 
                                                                        
!UT   MITTLERE GESCHW. VON BRUECKENPROFIL 3                             
      vm3 = q / fges 
                                                                        
!UT   TABELLE 3.44, BWK, S. 66                                          
      IF (alpha.gt.0.23) then 
                                                                        
!UT     SCHREIBEN IN KONTROLLDATEI                                      
        IF (lein.eq.3) then 
          WRITE (jw8, '(''warnung !!!!'')') 
          WRITE (jw8, '(''verbauverhaeltnis alpha = '',f8.4)') alpha 
          WRITE (jw8, '(''- fuer rechteckgerinne alpha < 0.23'')') 
          WRITE (jw8, '(''- fuer trapezgerinne'')') 
      WRITE (jw8, '(''  boeschungsneigung  1:2     < 0.34'')') 
      WRITE (jw8, '(''                     1:3     < 0.44'')') 
        ENDIF 
                                                                        
      ENDIF 
                                                                        
!UT   LIEGT alpha ueber 0.44 GILT TABELLE 3.44 nicht MEHR               
!UT   iykenn WIRD 1 GESETZT UND AN SUB-ENDE DANN STAUHOEWHE AUF NULL    
      IF (alpha.gt.44) iykenn = 1 
                                                                        
!JK         WAR SCHON DEAKTIVIERT, 30.04.00, JK                         
!**         print *                                                     
!**         print *                                                     
!**         print *,'warnung !!!!'                                      
!**         print *                                                     
!**         print *,'verbauverhaeltnis alpha = ',alpha                  
!**         print *,'- fuer rechteckgerinne alpha < 0.23'               
!**         print *,'- fuer trapezgerinne'                              
!**         print *,'  boeschungsneigung  1:2     < 0.34'               
!**         print *,'                     1:3     < 0.44'               
!**         print *,'ueberpruefen sie ggf. die verhaeltnisse !!'        
!**         print *,'(vgl beschreibung)'                                
!**         endif                                                       
                                                                        
!UT      BERECHNUNG NACH PFEILERSTAUFORMEL 58,S.35 IN PROGRAMMANLEITUNG 
!UT      KLAMMERTERM                                                    
      alpha = alpha + 15.0 * alpha**4 
                                                                        
!UT      GESCHWINDIGKEITSTERM                                           
      hpf = vm3 * vm3 / 2. / 9.81 
                                                                        
!UT      FAKTOR 10-TERM                                                 
      fak = hpf * 10. / hz 
                                                                        
      fak = xk + fak - 0.6 
                                                                        
!JK         WAR SCHON DEAKTIVIERT, 30.04.00, JK                         
!**         hpf=hpf*alpha                                               
                                                                        
      fak = fak * alpha 
                                                                        
!UT   BERECHNUNG PFEILERSTAU, FORMEL 58,S.35 IN PROGRAMMANLEITUNG       
      hpf = hpf * 2.0 * xk * fak 
                                                                        
                                                                        
!UT      SCHREIBEN IN KONTROLLDATEI                                     
      IF (lein.eq.3) then 
        WRITE (jw8, '(''Pfeilerstau hpf='',f8.3)') hpf 
      ENDIF 
                                                                        
      IF (iykenn.eq.1) hpf = 0.0 
!**                                                                     
                                                                        
 9999 RETURN 
                                                                        
!UT   ENDE SUB YARNELL                                                  
      END SUBROUTINE yarnell                        
