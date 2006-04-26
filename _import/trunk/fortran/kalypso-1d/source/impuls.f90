!     Last change:  WP   26 Apr 2006    1:57 pm
!--------------------------------------------------------------------------
! This code, impuls.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - impuls
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

SUBROUTINE impuls (hr, cd, x1, h1, hdif, hsohl, nknot, il, ir, q, iart, m, a, ifehl2)

!***********************************************************************
!**                                                                     
!JK   SUBROUTINE IMPULS                                                 
!**                                                                     
!JK   BESCHREIBUNG: IMPULSBERECHNUNG FUER 3 UNTERSCHIEDLICHE PROFILE    
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**                                                                     
!**   a       --      durchströmte Flläche                              
!**   ab      --      Gesamtfläche                                      
!**   ad      --      durchströmte Fläche des Druckabflusses            
!**   alges   --      Gesamtwiderstandsbeiwert                          
!**   an      --      durchströmte Fläche                               
!**   apg     --      angeströmte Fläche der Brückenpfeiler             
!**   apl     --      angeströmte Fläche der Brückenplatte              
!**   aue     --      Überfallfläche                                    
!**   breite  --      Brückenbreite in Fließrichtung                    
!**   cb      --      Widerstandsbeiwert der Brückenplatte oder der     
!**                   Pfeiler                                           
!**   cd      --      Widerstandsbeiwert der Brückenplatte oder der     
!**                   Pfeiler                                           
!**   fr      --      Reibungskraft                                     
!**   g       --      Erdbeschleunigung                                 
!**   hp      --      Schwerpunkt der angeströmten Pfeilerfläche        
!**   hpl     --      Schwerpunkt der angeströmten Plattenfläche        
!**   hr      --      Wasserspiegelhöhe                                 
!**   hss     --      Schwerpunkt der durchströmten Fläche des          
!**                   Druckabflusses                                    
!**   hue     --      Schwerpunkt der Überfallfläche                    
!**   hukmax  --      maximale Höhe der Brückenunterkante               
!**   iart    --      Art der Impulsberechnung (Profil 1,2,3)           
!**   ifehl2  --      Fehlervariable                                    
!**   m       --      Impuls                                            
!**   q       --      Abfluß                                            
!**   tau     --      Faktor zur Berechnung der Reibungskraft           
!**   uges    --      geamter benetzter Umfang                          
!**   vges    --      mittlere Fließgeschwindigkeit                     
!**                                                                     
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!JK   geomet                                                            
!***********************************************************************
                                                                        
! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS

                                                                        
      REAL x1 (maxkla), h1 (maxkla) 
                                                                        
      REAL x1n (maxkla), h1n (maxkla) 
      REAL xl (maxkla), hl (maxkla), zl (maxkla) 
      REAL xl1 (maxkla), hl1 (maxkla) 
                                                                        
      REAL as (max2), hs (max2)
      REAL hps (max2), ap (max2)
      REAL apls (max2), aues (max2)
      REAL hpls (max2), hues (max2)
      REAL xukn (maxkla), hukn (maxkla) 
      REAL m 
                                                                        
      COMMON / flaechen / ad, aue, apl, apg, hss, hue, hpl, hp 


! COMMON-Block /BRUECK/ ------------------------------------------------------------
INTEGER 	:: iwl, iwr, nuk, nok
INTEGER 	:: iokl, iokr           ! Gelaende Oberkante Grenze
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk, hokmin
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, iokl, iokr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, hokmin, ibridge
! ----------------------------------------------------------------------------------



!     ******************************************************************
!     uebergabe-parameter:                                              
!                                                                       
!     - input: x1,h1 --> bezugslinie im profil                          
!              hr    --> ermittelter wsp im profil                      
!              cd    --> drag-koeffizient                               
!              il,ir --> punktnr. linkes und rechtes widerlager         
!              q     --> durchfluss                                     
!              iart  --> art der impulsgleichung (1,2,3)                
!                                                                       
!     - output:    m --> ermittelter impuls                             
!                                                                       
!      **************************************************************   
      COMMON / pfeilerstau / alpha, bnetto 
!     ***************************************************************   
!     common-bloecke boolsche operationen --> algebra                   
! ================================================                      
      COMMON / angabe / xko (mpts, 2), hko (mpts, 2), na1, na2 
! ================================================                      
COMMON / xr0yr0 / xr0 (mpts, max2), hr0 (mpts, max2), mr0 (max2), kr0
COMMON / xr1yr1 / xr1 (mpts, max2), hr1 (mpts, max2), mr1 (max2), kr1
COMMON / xr2yr2 / xr2 (mpts, max2), hr2 (mpts, max2), mr2 (max2), kr2
! ================================================                      
                                                                        
      COMMON / iprint / id 
      CHARACTER(2) id 
                                                                        
      !WP 10.05.2004
      !WP Common-Block angepasst
      INTEGER ::isohl, iming 
      REAL ::hming 
      COMMON / p3 / isohl, hming, iming 
      !WP 10.05.2004
                                                                        
                                                                        
                                                                        
      COMMON / ges / fges, brges, uges, akges, vges, rhges, alges 
      !==================================================================
                                                                        

      ! ------------------------------------------------------------------
      ! BERECHNUNGEN
      ! ------------------------------------------------------------------
                                                                        
      iterfmax = 50 
      iterf = 0 
                                                                        
                                                                        
      1100 CONTINUE

      CALL geomet (hr, x1, h1, hdif, hsohl, nknot, il, ir, q, wl, rhy, ifehl2)

      IF (ifehl2.ne.0) then 
        hr = hr + 0.01 
        iterf = iterf + 1 
                                                                        
        IF (iterf.gt.iterfmax) then 
          PRINT * , 'Fehler in UP geometrie' 
          PRINT * , 'Auch bei Korrektur des Wasserspiegels nicht' 
          PRINT * , 'behebbar. Daher Programmabbruch.' 
                                                                        
          !JK         SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '(''Fehler in UP geometrie'',/,  &
           &       ''Auch bei Korrektur des Wasserspiegels nicht'',/  &
           &       ''behebbar. Daher Programmabbruch.'')')
          STOP 'Programmabbruch impuls 1'

        ELSE 

          !JK         GEOMET NEU EINLESEN                                         
          GOTO 1100 

        ENDIF 

      ENDIF 
                                                                        
      !     *************************************************************
      !     impulsberechnung als f(iart)
      !     *************************************************************

      IF (iart.eq.1) then 
                                                                        
        !        - impulsberechnung profil '1'
                                                                        
        cb = cd 
        ab = apg + apl + aue+ad 
                                                                        
        !JK     WAR SCHON DEAKTIVIERT,27.04.00,JK
        !       m = ad*hss + aue*hue- apg*hp -apl*hpl +
        !     +     q*q/(g*ab*ab) * (ab-cd/2.*apg-cb/2.*apl)
                                                                        
        m = ad * hss + aue * hue+q * q / (g * ab * ab) * (ab - cd / 2. * apg - cb / 2. * apl)

      ELSEIF (iart.eq.2) then 
                                                                        
        !        - impulsberechnung profil '2'
                                                                        
        !           schwerpkt. ages     --> hss
        !           schwerpkt. apfeiler --> hp
                                                                        
        an = ad+aue 
                                                                        
        m = ad * hss + aue * hue+q * q / (g * an) 
                                                                        
                                                                        
      ELSEIF (iart.eq.3) then 
                                                                        
        !        - (iart=3) impulsberechung profil '3'
                                                                        
        !           profil "3"
                                                                        
        ab = aue+ad+apg + apl 
                                                                        
        IF (hr.lt.hukmax) then 

          tau = vges * vges * alges / 8 
          fr = uges * breite * tau / g

        ELSE 

          fr = 0. 

        ENDIF 
                                                                        
        !JK    WAR SCHON DEAKTIVIERT,27.04.00,JK
        !      m = ad*hss + aue*hue - apg*hp - apl*hpl + q*q/(g*ab) + fr
                                                                        
        m = ad * hss + aue * hue+q * q / (g * ab) + fr 
                                                                        
      ENDIF 
                                                                        
      ! gesamt durchstroemte flaeche a=a+aue
      ! print *,'***************************************************'
      ! print *,'*  ergebnisse aus impuls:                         *'
      ! print *,'*                                                 *'
      ! print *,'*  - hr = ',hr
      ! print *,'*  - durchstroemte flaeche : a   = ',a
      ! print *,'*                            aue = ',aue
      ! print *,'*  - angestroemte flaeche :  apl = ',apl
      ! print *,'*  -                         apg = ',apg
      ! print *,'***************************************************'
      ! print *                                                       
                                                                        
      a = ad+aue 
                                                                        
      9999 RETURN
                                                                        
      END SUBROUTINE impuls                         
