!     Last change:  WP   13 Jul 2005    1:32 pm
!--------------------------------------------------------------------------
! This code, pegasus.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - pegasus
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
      SUBROUTINE pegasus (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,&
      psiein, psiort, jw5, ikenn, froud, xi, hi, s, ifehl, istat)       
!     Last change:  WP   28 Jul 2004   10:50 am
                                                                        
!**   von CD F:WSP/WSP_NEU/GROSS/pegasus.for                            
                                                                        
!     ------------------------------------------------------------------
                                                                        
!     geschrieben: p.koch    maerz'90                                   
!     ******************************************************************
!     programmbeschreibung:                                             
!                                                                       
!     dieses programm ermittelt den wasserspiegel an einem profil.      
!JK   WENN BEI newton KEINE KONVERGENZ ERREICHBAR                       
!                                                                       
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   a_m     --                                                        
!**   f1      --      Variablen der Einschlußintervallberechnung        
!**   f2      --      Variablen der Einschlußintervallberechnung        
!**   fak     --      Grenzbedingung                                    
!**   h1x     --      Variablen der Einschlußintervallberechnung        
!**   h2x     --      Variablen der Einschlußintervallberechnung        
!**   hborda  --      Einengungsverlust                                 
!**   heins   --      Einlaufverluste                                   
!**   horts   --      örtliche Verluste                                 
!**   hr      --      Wasserspiegelhöhe                                 
!**   hrneu   --      Summe aus Wasserspiegel- und Verlusthöhen         
!**   hrst    --      Reibungsverlust                                   
!**   hvst    --      Geschwindigkeitsverlust                           
!ep   05.02.2002                                                        
!     istat   --      Flag zur Kennzeichnung der Strömung               
!                     = 1        gleichförmig                           
!                     = 0        ungleichförmig                         
!                     muß in dieser Subroutine aus übergeordnetem       
!                     Programm übernommen werden.                       
!                                                                       
!**   ifehl   --      Fehlervariable                                    
!**   jw8     --      Name des Kontrollfiles                            
!**   lein    --      Art des Ergebnisausdruckes                        
!**   ws1     --      Wasserspiegelhöhe                                 
!**                                                                     
!**                                                                     
!**                                                                     
!JK   AUFGERUFENE SUBROUTINEN:                                          
!JK     verluste                                                        
!     ******************************************************************
!                                                                       
!JK   ------------------------------------------------------------------
!JK   VEREINBARUNGSTEIL                                                 
!JK   ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN

!     itmax - max. anzahl der iterationsschritte                        
!     err   - genauigkeitsschranke fuer wsp-ermittlung bei iteration    
                                                                        
REAL xi (maxkla), hi (maxkla), s (maxkla)
                                                                        
                                                                        
!     ------------------------------------------------------------------
!     benutzte common-bloecke: (von _____ uebergabe (-->) nach ______)  
                                                                        
!     common /p0/ -   wspber --> normber                                
                                                                        
!     ******************************************************************
!                                                                       
!     common-block fuer die gewaessergeometrie                          
!                                                                       
!     beachte parameteranweisung maxkla                                 
!                                                                       
!     common-block /p2/ wird von folgenden subroutinen aufgerufen       
!                                                                       
!           intdat                                                      
!           uf                                                          
!           bovog                                                       
!           normber                                                     
!                                                                       
      CHARACTER(1) iprof 
      REAL x1 (maxkla), h1 (maxkla), rau (maxkla), durchm, hd, sohlg,   &
      steig, boli, bore, hmin, hmax                                     
      INTEGER ianf, iend 
                                                                        
      COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig,&
      boli, bore, hmin, hmax, ianf, iend, hrbv                          

! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------

!     ******************************************************************
      COMMON / ausgabeart / lein, jw8 
!     lein =1 --> einfacher ergebnisausdruck                            
!     lein =2 --> erweiterter ergebnisausdruck                          
!     lein =3 --> kontrollfile                                          
!     ******************************************************************


! COMMON-Block /ERG/ ----------------------------------------------------------
REAL 		:: wsp (maxger), hen (maxger), qs (maxger), fgesp (maxger)
REAL 		:: froudp (maxger), hvs (maxger), hrs (maxger), hs (maxger)
REAL 		:: fp (maxger, maxkla), up (maxger, maxkla), vp (maxger, maxkla)
REAL 		:: qtp (maxger, maxkla), rkp (maxger, maxkla), fbwp (maxger, maxkla)
REAL 		:: brp (maxger, maxkla)
REAL		:: vmp (maxger), hbors (maxger), hein (maxger), hort (maxger), brg (maxger)
INTEGER 	:: igrenz (maxger)
COMMON / erg / wsp, hen, qs, fgesp, froudp, hvs, hrs, hs, fp, up, &
             & vp, qtp, rkp, fbwp, brp, vmp, hbors, hein, hort, igrenz, brg
! -----------------------------------------------------------------------------



!     common-block fuer die uebergabe von zusatzverlusten               
!                                                                       
!     common-block enthalten in                                         
!                speicher                                               
!                normber                                                
!                wspanf                                                 
                                                                        
      REAL hborda, heins, horts 
                                                                        
      COMMON / vort / hborda, heins, horts 
!     ******************************************************************
                                                                        
!     common /p4/ -   wspber --> normber                                
!     common /ges/-   uf     --> normber                                
!     ------------------------------------------------------------------
      COMMON / p4 / ifg, betta 
      COMMON / ges / fges, brges, uges, akges, vges, rhges, alges 
!     ******************************************************************
      REAL bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger) 
      REAL hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger) 
                                                                        
      COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, &
      k_kp                                                              
                                                                        
!     wird aufgerufen in : speicher                                     
!                          laprof                                       
!                          lapro1                                       
!                          bordvoll                                     
!                                                                       
!      *****************************************************************
      COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1 
!     wird benoetigt in stationaer und wspanf (definition in normber)   
      COMMON / peg / h1x, h2x, f1, f2 
      COMMON / rohr / idruck 
      INTEGER a_m 
      COMMON / w_a / a_m 
!                                                                       
                                                                        
!JK   ------------------------------------------------------------------
!JK   BERECHNUNGEN                                                      
!JK   ------------------------------------------------------------------
                                                                        
      ifehl = 0 
!     ******************************************************************
!     iterationsschleife                                                
!     ******************************************************************
                                                                        
!**                                                                     
!JK   WENN GENAUE BERECHNUNG MIT EINSCHLUSSINTERVALL                    
      IF (a_m.eq.2) then 
                                                                        
        fak = (h2x - h1x) / (f2 - f1) 
        hr = (h1x - fak * f1) / (1 - fak) 
                                                                        
!JK       SCHREIBEN IN KONTROLLFILE                                     
        IF (lein.eq.3) then 
          WRITE (jw8, '('' in pegasus : '')') 
      WRITE (jw8, '('' gesuchte nullstelle zwischen '',f15.3,'' und '', f15.3,/,&
                  & '' --> weiter mit hr = '',f15.3)') h1x, h2x, hr
          WRITE (jw8, '('' f1 = '',f15.3,'' f2 = '',f15.3)') f1, f2 
      WRITE (jw8, '(//,'' iteration mittels pegasus-verfahren : '',//,  &
     &              '' hr   hrneu   h1x   f1   h2x   f2 '')')           
        ENDIF 
                                                                        
      ENDIF 
                                                                        
      DO 100 jsch = 1, 50 
!**                                                                     
!JK       WENN GENAUE BERECHNUNG MIT EINSCHLUSSINTERVALL                
        IF (a_m.eq.2) then 
          jen = jsch + 1 
        ELSE 
          jen = jsch 
        ENDIF 
!**                                                                     
        hborda = 0. 
                                                                        
        CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
        psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg, jen)      
                                                                        
        hrneu = ws1 + hrst + hvst + hborda + heins + horts 
                                                                        
        IF (abs (hrneu - hr) .lt.err) then 
          hr = hrneu 
!JK           ZU STROEMENDER BEREICH + DATENUEBERGABE                   
          GOTO 9999 
        ELSE 
!           ************************************************************
!           neuen wasserspiegel schaetzen :                             
!           **********************************************************  
!JK           SCHREIBEN IN KONTROLLFILE                                 
          IF (lein.eq.3) then 
            WRITE (jw8, '(6f15.3)') hr, hrneu, h1x, f1, h2x, f2 
          ENDIF 
                                                                        
!             festlegen neues einschlussintervall:                      
          IF ( ( (hrneu - hr) * (f1 - h1x) ) .lt.0.0025) then 
            h2x = hr 
            f2 = hrneu 
!                h1x=h1x, f1=f1                                         
          ELSEIF ( ( (hrneu - hr) * (f1 - h1x) ) .gt.0.0025) then 
            h1x = hr 
            f1 = hrneu 
!                h2x=h2x, f2=f2                                         
          ELSE 
!                h1x muss die nullstelle sein !!!                       
            IF (abs (f1 - h1x) .gt.0.05) then 
              PRINT * , 'fehler in der logik !!' 
              PRINT * , 'stop in pegasus!!!' 
            ELSE 
              hr = h1x 
!JK                 SCHREIBEN IN KONTROLLFILE                           
              IF (lein.eq.3) then 
      WRITE (jw8, '('' wsp gefunden : hr= '',f15.3,                     &
     &       '' hrneu = '',f15.3)') hr, f1                              
              ENDIF 
!                                                                       
              CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst,     &
              indmax, psiein, psiort, jw5, hi, xi, s, istat, froud,     &
              ifehlg, jen)                                              
            ENDIF 
                                                                        
          ENDIF 
!**                                                                     
!JK                        ZU FEHLERMELDEUNG: KEINE KONVERGENZ          
          IF (f1.eq.f2) goto 103 
!**                                                                     
          fak = (h2x - h1x) / (f2 - f1) 
!**                                                                     
!JK                           ZU FEHLERMELDEUNG: KEINE KONVERGENZ       
          IF (fak.eq.1.) goto 103 
!**                                                                     
          hr = (h1x - fak * f1) / (1 - fak) 
                                                                        
        ENDIF 
  100 END DO 
!JK   ENDE ITERATIONSSCHLEIFE----------------------------------------   
                                                                        
  103 ifehl = 1 
                                                                        
!JK   FEHLERMELDUNG + DATENUEBERGABE                                    
!JK   ------------------------------                                    
                                                                        
 9999 CONTINUE 
      IF (ifehl.eq.0) then 
!       stroemender bereich                                             
        CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
        psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg, jsch)     
!JK      SCHREIBEN IN KONTROLLFILE                                      
        IF (lein.eq.3) then 
      WRITE (jw8, '('' wsp in pegasus zu '',f15.3,'' ermittelt'',       &
     &        /,'' froud = '',f15.3,'' < 1. ! '',/)') hr,froud
        ENDIF 
      ELSE 
!JK      SCHREIBEN IN KONTROLLFILE                                      
        IF (lein.eq.3) then 
          WRITE (jw8, '('' keine konvergenz in pegasus '')') 
!**     +           ,/,'' -> weiter mit stationaer gleichfoermig '')')  
        ENDIF 
      ENDIF 
                                                                        
      RETURN 
                                                                        
      END SUBROUTINE pegasus                        
