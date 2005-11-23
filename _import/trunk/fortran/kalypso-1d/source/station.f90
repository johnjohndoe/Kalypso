!     Last change:  WP   25 Aug 2005    6:15 pm
!--------------------------------------------------------------------------
! This code, station.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - station
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

SUBROUTINE station (sgef, nprof, hgrenz, q, hr, hv, rg, indmax,   &
         & hvst, hrst, psiein, psiort, jw5, hi, xi, s, ikenn, froud, str,    &
         & ifehl, nblatt, nz, idr1)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE STATION ZU WSPWIN                                      
!**                                                                     
!**                                                                     
!**   Bestimmung eines stationaer gleichfoermigen wasserspiegels bei    
!**   angenommenem sohlgefaelle sgef                                    
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   bordvoll --      Art der Bordvollberechnung                       
!**   del     --      Grenzkriterium                                    
!**   dela    --      Grenzkriterium                                    
!**   dfh     --      Höhendifferenz                                    
!**   dfh     --      Höhendifferenz                                    
!**   dx      --      Schrittweite                                      
!**   froud   --      Froud-Zahl                                        
!**   hborda  --      Einengungsverlust                                 
!**   hdif1   --      Höhendifferenz                                    
!**   hdif2   --      Höhendifferenz                                    
!**   heins   --      Einlaufverlust                                    
!**   hgrenz  --      Grenztiefe                                        
!**   hmax    --      maximale Höhe                                     
!**   hmin    --      minimale Höhe                                     
!**   horts   --      Auslaufverlust                                    
!**   hr      --      Wasserspiegelhöhe                                 
!**   hra     --      Wasserspiegelhöhe                                 
!**   hraa    --      Wasserspiegelhöhe                                 
!**   hrb     --      Wasserspiegelhöhe                                 
!**   hrst    --      Reibungsverlust                                   
!**   hrsta   --      Reibungsverlust                                   
!**   hrstb   --      Reibungsverlust                                   
!**   hukmax  --      maximale Höhe der Brückenunterkante               
!**   hvst    --      Geschwindigkeitsverlust                           
!**   ibridge --      Abfrage Brückenprofil                             
!**   idruck  --      Charakterisierung des Druckabflusses              
!**   ifehl   --      Fehlervariable                                    
!**   iprof   --      Art des Profiles                                  
!**   istat   --      Stationsnummer                                    
!**   jdruck  --      Charakterisierung des Druckabflusses              
!**   jw5     --      Name des Ergebnisfiles                            
!**   jw8     --      Name des Kontrollfiles                            
!**   lein    --      Art der Ergebnisausgabe                           
!**   nblatt  --      Anzahl der Blätter des Ergebnisfiles              
!**   nprof   --      Anzahl der Punkte in einem Profil                 
!**   nz      --      Anzahl der Zeilen des Ergebnisfiles               
!**   q       --      Abfluß                                            
!**   q1      --      Abfluß                                            
!**   rdruck  --      Charakterisierung des Druckabflusses              
!**   salt    --      Verhältnis Reibungsverlust / Stationsabstand      
!**   sgef    --      Sohlgefälle                                       
!**   sist    --      Verhältnis Reibungsverlust / Stationsabstand      
!**   sista   --      Verhältnis Reibungsverlust / Stationsabstand      
!**   sistb   --      Verhältnis Reibungsverlust / Stationsabstand      
!**   str     --      Stationsabstand                                   
!**   ws1     --      Wasserspiegelhöhe                                 
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!JK   abskst                                                            
!**   verluste(str,q,q1,nprof,hr,hv,rg,hvst,hrst,indmax,                
!**            psiein,psiort,jw5,hi,xi,s,istat,froud,ifehlg,itere1)     
!JK   erfroud                                                           
!JK   kopf                                                              
!**                                                                     
!***********************************************************************

USE DIM_VARIABLEN


!EP   Die Variable idr1 wird aufgerufen, obwohl sie nicht vorher deklari
!EP   Der Common-Block BV definiert diese Variable. Er kann aber nicht ü
!EP   werden, da die Variable nprof, die im Common-Block steht, auch in 
!EP   Parameterliste dieser SUBR steht. Daher wird die Variable IDR1 ebf
!EP   diese Parameterliste übergeben und hier nur noch als Character def
!EP                                                                     
CHARACTER(LEN=1) :: idr1
!EP   Ende der Ergänzung
REAL :: xi (maxkla), hi (maxkla), s (maxkla)


! COMMON-Block /ALT/ ----------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
! -----------------------------------------------------------------------------


! COMMON-Block /AUSGABEART/ ---------------------------------------------------
! lein=1    --> einfacher ergebnisausdruck
! lein=2    --> erweiterter ergebnisausdruck
! lein=3    --> erstellung kontrollfile
! jw8       --> NAME KONTROLLFILE
INTEGER 	:: lein
INTEGER 	:: jw8
COMMON / ausgabeart / lein, jw8
! -----------------------------------------------------------------------------


! COMMON-Block /BRUECK/ -------------------------------------------------------
! fuer BRUECKENBERECHNUNG, AUFRUF IN INTDAT, IMPULS, WSPBER
INTEGER 	:: iwl, iwr, nuk, nok
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, ibridge
! -----------------------------------------------------------------------------


! COMMON-Block /DD/ -----------------------------------------------------------
REAL            :: tm, vm
COMMON / dd / tm, vm
! -----------------------------------------------------------------------------


! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------


! COMMON-Block /GEF_SOHL/ -----------------------------------------------------
REAL 		:: g_sohl
COMMON / gef_sohl / g_sohl
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /P1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: ereignis, fnam1, fluss
CHARACTER(LEN=1) :: bordvoll
COMMON / p1 / ereignis, fnam1, bordvoll, fluss
! -----------------------------------------------------------------------------


! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! COMMON-Block /P4/ -----------------------------------------------------------
INTEGER         :: ifg
REAL            :: betta
COMMON / p4 / ifg, betta
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /ROHR/ ---------------------------------------------------------
INTEGER         :: idruck
COMMON / rohr / idruck
! -----------------------------------------------------------------------------


! COMMON-Block /VORT/ ---------------------------------------------------------
REAL 		:: hborda, heins, horts
COMMON / vort / hborda, heins, horts
! -----------------------------------------------------------------------------



INTEGER :: rdruck

! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
!                                                                       
!       Vorbelegung:                                                    
!       -max. anzahl der iterationsschritte:                            
g_sohl = sgef
                                                                        
izz = 0
ifehl = 0
istat = 1
rdruck = idruck
                                                                        
!     Umspeichern der idruck-Variablen, da sie durch Aufruf von verluste
!     ueberschrieben wird. An dieser Stelle zeigt idruck an, ob im Rohr 
!     Grenztiefe auftritt(idruck=0) oder bei Erreichen des Rohrscheitels
!     die Froud-Zahl nocht > 0 ist (idruck=1).                          
!     Fuer den Fall, dasz der stationaer-gleichfoermige Abfluss bei     
!     schiessendem Abfluss auftritt, wird die Variable rdruck           
!     ueberprueft.                                                      
!     Wenn rdruck= 0   dann mit Grenztiefe weiter                       
!     Wenn rdruck=1    dann mit Rohrscheitelhoehe weiter                
!                                                                       
                                                                        
!                               /* kennung fuer stationaer gleichfoermig
      ikenn = 3 
                                                                        
      hborda = 0. 
!                               /* nur fuer ungleichfoermig             
      heins = 0. 
      horts = 0. 
                                                                        
      IF (nprof.eq.1) then 
        q1 = 0. 
      ELSE 
        q1 = q 
      ENDIF 
!**                                                                     
      it155 = 0 
      it155m = 30 
!**                                                                     
!JK   SCHREIBEN IN KONTROLLFILE                                         
      IF (lein.eq.3) then 
                                                                        
        IF (nprof.ne.1) then 
      WRITE (jw8, '(''ermittlung des stationaer gleichfoermigen '',     &
     &         ''wasserspiegels in subroutine stationaer'')')           
        ELSE 
      WRITE (jw8, '(''--> wsp stationaer gleichfoermig in wspanf'')') 
          WRITE (jw8, '('' hmin ='',f12.3)') hmin 
        ENDIF 
                                                                        
      ENDIF 
!                                                                       
                                                                        
!     ******************************************************************
!     berechnung von basisgeometriegroessen : sehnen, abstand zwischen  
!                                             den profilpunkten (hori-  
!                                             zontal und veritkal)      
!     ******************************************************************
!     input:  nknot,x1,h1
!     output: xi,hi,s                                                   

      CALL abskst (nknot, x1, xi, h1, hi, s, maxkla)

!     berechnung der tiefsten sohle und der boeschungskanten (s. profil)
!     gebraucht werden - boli(i),bore(i),sohl(i)                        

                                                                        
      IF (bordvoll.ne.'g') then 
                                                                        
                                                                        
!      *****************************************************************
!      der anfangswasserspiegel wird unter annahme stationaer           
!      gleichfoermigen abflusses berechnet                              
!      *****************************************************************
                                                                        
!      anfangsiterationswert (grenztiefe+delta, damit nicht im labilen  
!                            bereich der koch'schen parabel)            
        IF (nprof.eq.1) then 
          nz = nz + 3 
          IF (nz.gt.50) then 
            nblatt = nblatt + 1 
            CALL kopf (nblatt, nz, jw5, ifg, jw7, idr1) 
          ENDIF 
                                                                        
      WRITE (jw5, '(/,t10,''der Anfangswasserspiegel wird unter Annahme'&
     &',       /t10,'' stationaer gleichfoermigen Abflusses berechnet. '&
     &',           /,t10,''Sohlgefaelle = '',f7.5)') sgef               
        ENDIF 
                                                                        
        dx = 0.02 
!     anfangswasserspiegel:                                             
        IF (nprof.gt.1) then 
          hr = ws1 - str * sgef 
        ELSE 
          hr = hgrenz + 0.2 
        ENDIF 
                                                                        
        IF (iprof.ne.' ') then 
          hr = (hmin + hmax) / 2.0 
        ENDIF 
                                                                        
        IF (hr.le.hmin) hr = hmin + 0.2 
                                                                        
!     ******************************************************************
!     iterationsschleife                                                
!     ******************************************************************
                                                                        
!JK   SCHREIBEN IN KONTROLLFILE                                         
        IF (lein.eq.3) then 
      WRITE (jw8, '(''iterations-       gefaelle          wsp      froud&
     &'',''       dif     KENNZAHL'',/,''nr           soll      ist'')')
        ENDIF 
                                                                        
        del = 1000. 
        hrb = hr - dx 
        itere1 = 1 
        falt = 0. 
                                                                        
  152   CALL verluste (str, q, q1, nprof, hrb, hv, rg, hvstb, hrstb,    &
        indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg,   &
        itere1)                                                         
                                                                        
                                                                        
        sistb = hrstb / str 
        itere1 = 2 
        hra = hrb + 2. * dx 
                                                                        
  151   CALL verluste (str, q, q1, nprof, hra, hv, rg, hvsta, hrsta,    &
        indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg,   &
        itere1)                                                         
                                                                        
        sista = hrsta / str 
        hraa = - 10000. 
                                                                        
        DO 150 i = 1, itmax 
                                                                        
          del = hrsta / str + sgef 
          dfh = abs (hraa - hrb) 
                                                                        
!HB       ************************************************************* 
!HB       26.11.2001 Pasche                                             
!HB       Genauigkeitsveraenderung durchgefuehrt                        
!HB       alter Wert: 0.00005                                           
!HB       Auf diese Weise treten bei stationaer gleichfoermiger         
!HB       Berechnung keine nicht vorhandenen Geschwindigkeitsverluste   
!HB       auf.                                                          
!HB       Bisher Berechnung des Anfangswasserspiegels ungenauer als     
!HB       Berechnung nach Arbeitsgleichung fuer uebrige Profile.        
!HB       erzielte Genauigkeit nun: 10**-3 (mm-Bereich)                 
!HB       ************************************************************* 
          IF (abs (del) .lt.0.0000001) then 
                                                                        
!             wsp gefunden -->                                          
                                                                        
!JK              WAR SCHON DEAKTIVIERT, 02.05.00, JK                    
!**              if(lein.eq.3) then                                     
!**              write(jw8,'(''wsp durch iteration ermittelt zu '',f7.2,
!**     +              )')                                              
!**              endif                                                  
                                                                        
!             kennung, ob bei bruecken druckabfluss oder nicht :        
            IF (ibridge.eq.'b'.and.hr.ge.hukmax) then 
              jdruck = 1 
            ELSE 
              jdruck = 0 
            ENDIF 
                                                                        
!             ueberpruefen, ob schiessen oder stroemen                  
            IF (froud.ge.1..and.rdruck.ne.0) then 
              ikenn = 4 
              hr = hgrenz 
              itere1 = i 
                                                                        
!                 ermitteln der verlustwerte                            
              CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
              indmax, psiein, psiort, jw5, hi, xi, s, istat, froud,     &
              ifehlg, itere1)                                           
            ELSEIF (froud.ge.1.and.jdruck.eq.0) then 
              ikenn = 1 
              hr = hgrenz 
!**                                                                     
!JK              SCHREIBEN IN KONTROLLFILE                              
              IF (lein.eq.3) then 
                WRITE (jw8, '( ''froud ='',f12.4,'' > 1. !'')') froud 
!**                  write(jw8,'(/,'' grenztiefe aus stationaer '')')   
                WRITE (jw8, '(/,'' d.h. steiles gefaelle yn<ygr !!'')') 
      WRITE (jw8, '(/,'' ->  weiter mit grenztiefe hr= '',              &
     &              f12.4)') hr                                         
              ENDIF 
!**                                                                     
              froud = 1. 
              itere1 = itere1 + 1 
                                                                        
!JK              SCHREIBEN IN KONTROLLFILE                              
              IF (lein.eq.3) write (jw8, '(''--> hgrenz ='',f7.3)') hr 
                                                                        
!                ermitteln der verlustwerte                             
              CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
              indmax, psiein, psiort, jw5, hi, xi, s, istat, froud,     &
              ifehlg, itere1)                                           
                                                                        
            ELSE 
              ikenn = 3 
              hr = hra 
              sist = sista 
            ENDIF 
                                                                        
!JK           SCHREIBEN IN KONTROLLFILE                                 
            IF (lein.eq.3) then 
!                 write(jw8,'(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,      
!     +             f12.7)') i,sgef,sist,hr,froud,del                   
!HB           SCHREIBEN IN KONTROLLFILE              *** KENNZAHL 10 ***
      WRITE (jw8, '(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,                &
     &    f12.7,'' 10'')') i, sgef, sist, hr, froud, del                
            ENDIF 
!JK           WSP GEFUNDEN                                              
            GOTO 9999 
!JK       ELSEIF ZU (abs(del).lt.0.00005)                               
                                                                        
!HB       ************************************************************* 
!HB       26.11.2001 Pasche                                             
!HB       Genauigkeitsveraenderung durchgefuehrt                        
!HB       alter Wert: 0.01                                              
!HB       ************************************************************* 
          ELSEIF (abs (dfh) .le.0.0001) then 
!             Iteration hat in Bezug auf Wasserspiegel konvergiert      
            hr = hra 
                                                                        
!JK           SCHREIBEN IN KONTROLLFILE                                 
            IF (lein.eq.3) then 
              WRITE (jw8, '(''Konvergenz im Wasserspiegel erreicht '')') 
!              write(jw8,'(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,         
!     +             f12.7)') i,sgef,sist,hr,froud,del                   
!HB           SCHREIBEN IN KONTROLLFILE              *** KENNZAHL 20 ***
      WRITE (jw8, '(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,                &
     &       f12.7,'' 20'')') i, sgef, sist, hr, froud, del             
                                                                        
            ENDIF 
                                                                        
            IF (ibridge.eq.'b'.and.hr.ge.hukmax) then 
              jdruck = 1 
            ELSE 
              jdruck = 0 
            ENDIF 
                                                                        
!             ueberpruefen, ob schiessen oder stroemen                  
            IF (froud.ge.1..and.rdruck.ne.0) then 
              ikenn = 4 
              hr = hgrenz 
                                                                        
!                 ermitteln der verlustwerte                            
              CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
              indmax, psiein, psiort, jw5, hi, xi, s, istat, froud,     &
              ifehlg, itere1)                                           
            ELSEIF (froud.ge.1.and.jdruck.eq.0) then 
                                                                        
              ikenn = 1 
              hr = hgrenz 
!**                                                                     
!JK               SCHREIBEN IN KONTROLLFILE                             
              IF (lein.eq.3) then 
                WRITE (jw8, '( ''froud ='',f12.4,'' > 1. !'')') froud 
!**                  write(jw8,'(/,'' grenztiefe aus stationaer '')')   
                WRITE (jw8, '(/,'' d.h. steiles gefaelle yn<ygr !!'')') 
      WRITE (jw8, '(/,'' ->  weiter mit grenztiefe hr= '',              &
     &              f12.4)') hr                                         
              ENDIF 
!**                                                                     
              froud = 1. 
                                                                        
!JK               SCHREIBEN IN KONTROLLFILE                             
              IF (lein.eq.3) write (jw8, '(''--> hgrenz ='',f7.3)') hr 
                                                                        
!                 ermitteln der verlustwerte                            
              CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst, &
              indmax, psiein, psiort, jw5, hi, xi, s, istat, froud,     &
              ifehlg, itere1)                                           
                                                                        
            ELSE 
              ikenn = 3 
              hr = hra 
              sist = sista 
            ENDIF 
                                                                        
!JK           SCHREIBEN IN KONTROLLFILE                                 
            IF (lein.eq.3) then 
!                 write(jw8,'(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,      
!     +             f12.7)') i,sgef,sist,hr,froud,del                   
!HB           SCHREIBEN IN KONTROLLFILE              *** KENNZAHL 30 ***
      WRITE (jw8, '(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,                &
     &    f12.7,'' 30'')') i, sgef, sist, hr, froud, del                
            ENDIF 
                                                                        
!JK           WSP GEFUNDEN                                              
            GOTO 9999 
                                                                        
!JK       ELSE ZU (abs(del).lt.0.00005)                                 
          ELSE 
                                                                        
!             schaetzen neuer wasserspiegel                             
                                                                        
            hdif2 = sistb - abs (sgef) 
  155       hdif1 = sista - abs (sgef) 
!**                                                                     
            it155 = it155 + 1 
!**                                                                     
            IF (hdif1 * hdif2.lt.0) then 
              hraa = hra 
              dfh = abs (hraa - hrb) 
                                                                        
!HB            Moeglichkeit Genauigkeitseinstellung                     
              IF (abs (dfh) .le.0.01) then 
!                 Iteration hat in Bezug auf Wasserspiegel konvergiert  
                IF (ibridge.eq.'b'.and.hr.ge.hukmax) then 
                  jdruck = 1 
                ELSE 
                  jdruck = 0 
                ENDIF 
                                                                        
!                    ueberpruefen, ob schiessen oder stroemen           
                IF (froud.ge.1..and.rdruck.ne.0) then 
                  ikenn = 4 
                  hr = hgrenz 
!                        ermitteln der verlustwerte                     
                  CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst,   &
                  hrst, indmax, psiein, psiort, jw5, hi, xi, s, istat,  &
                  froud, ifehlg, itere1)                                
                                                                        
                ELSEIF (froud.ge.1.and.jdruck.eq.0) then 
                  ikenn = 1 
                  hr = hgrenz 
!**                                                                     
!JK                      SCHREIBEN IN KONTROLLFILE                      
                  IF (lein.eq.3) then 
                    WRITE (jw8, '( ''froud ='',f12.4,'' > 1. !'')')     &
                    froud                                               
!**                  write(jw8,'(/,'' grenztiefe aus stationaer '')')   
      WRITE (jw8, '(/,'' d.h. steiles gefaelle yn<ygr !!'')') 
      WRITE (jw8, '(/,'' ->  weiter mit grenztiefe hr= '',              &
     &              f12.4)') hr                                         
                  ENDIF 
!**                                                                     
                  froud = 1. 
                  IF (lein.eq.3) write (jw8, '(''--> hgrenz ='',f7.3)') &
                  hr                                                    
                                                                        
!                        ermitteln der verlustwerte                     
                  CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst,   &
                  hrst, indmax, psiein, psiort, jw5, hi, xi, s, istat,  &
                  froud, ifehlg, itere1)                                
                                                                        
                ELSE 
                  ikenn = 3 
                  hr = hra 
                  sist = sista 
                ENDIF 
                                                                        
!JK                  SCHREIBEN IN KONTROLLFILE                          
                IF (lein.eq.3) then 
!                     write(jw8,'(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,  
!     +                     f12.7)') i,sgef,sist,hr,froud,del           
!HB           SCHREIBEN IN KONTROLLFILE              *** KENNZAHL 40 ***
      WRITE (jw8, '(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,                &
     &        f12.7,'' 40'')') i, sgef, sist, hr, froud, del            
                ENDIF 
                                                                        
!JK                  WSP GEFUNDEN                                       
                GOTO 9999 
!JK               ENDIF ZU (abs(dfh) .le. 0.01)                         
              ENDIF 
                                                                        
              hra = hra + 0.1 * (hrb - hra) 
              CALL verluste (str, q, q1, nprof, hra, hv, rg, hvsta,     &
              hrsta, indmax, psiein, psiort, jw5, hi, xi, s, istat,     &
              froud, ifehlg, itere1)                                    
              sista = hrsta / str 
                                                                        
!JK               SCHREIBEN IN KONTROLLFILE                             
              IF (lein.eq.3) then 
!                  write(jw8,'(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,     
!     +                   f12.7)') i,sgef,sista,hra,froud,del           
!HB           SCHREIBEN IN KONTROLLFILE              *** KENNZAHL 50 ***
      WRITE (jw8, '(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,                &
     &         f12.7,'' 50'')') i, sgef, sista, hra, froud, del         
              ENDIF 
!**                                                                     
              IF (it155.le.it155m) then 
!JK                  SCHAETZEN NEUEN WSP                                
                GOTO 155 
              ELSE 
!JK                  BEENDEN DER SCHLEIFE 150                           
                GOTO 153 
              ENDIF 
!**                                                                     
!JK           ELSE ZU (hdif1*hdif2.lt.0)                                
            ELSE 
                                                                        
              IF (abs (hra - hrb) .lt.1.e-05) then 
!JK                  SCHREIBEN IN KONTROLLFILE                          
                WRITE (jw8, '(''Keine Konvergenz in Stationaer'')') 
                GOTO 8000 
!                     hra=hra+0.1                                       
!                     goto 151                                          
              ENDIF 
                                                                        
              df = (hrsta - hrstb) / (hra - hrb) 
              df = df / str 
              hrb = hra 
              hrstb = hrsta 
              sistb = sista 
                                                                        
              IF (abs (df) .le.0.00001) then 
                                                                        
!                 geradengleichung tangente + schnitt mit sollwert m:   
                izz = izz + 1 
                IF (izz.gt.2) then 
!                      andere iteration                                 
                  ifehl = 1 
                  GOTO 8000 
                ENDIF 
                dif = 0.1 
              ELSE 
                dif = (abs (sgef) - sista) / df 
              ENDIF 
                                                                        
              hra = hra + dif 
                                                                        
              IF (hra.le.hmin) then 
                hra = hmin + 0.10 
              ENDIF 
                                                                        
              IF (hrb.le. (hmin + 0.02) ) hrb = hr 
              CALL verluste (str, q, q1, nprof, hra, hv, rg, hvst,      &
              hrsta, indmax, psiein, psiort, jw5, hi, xi, s, istat,     &
              froud, ifehlg, itere1)                                    
              sista = hrsta / str 
                                                                        
!JK                  SCHREIBEN IN KONTROLLFILE                          
              IF (lein.eq.3) then 
!                        write(jw8,'(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x
!     +                    f12.7)') i,sgef,sista,hra,froud,del          
!HB           SCHREIBEN IN KONTROLLFILE              *** KENNZAHL 60 ***
      WRITE (jw8, '(i3,f12.7,1x,f12.7,1x,f12.5,f12.7,1x,                &
     &    f12.7,'' 60'')') i, sgef, sista, hra, froud, del              
              ENDIF 
                                                                        
!JK         ENDIF ZU (hdif1*hdif2.lt.0)                                 
            ENDIF 
!**     ENDIF ZU if(abs(del).lt.0.00005)                                
          ENDIF 
!     iterationsschleifenende                                           
                                                                        
  150   END DO 
                                                                        
!JK   SCHREIBEN IN KONTROLLFILE                                         
  153   IF (lein.eq.3) then 
      WRITE (jw8, '(''maximale anzahl der iterationsschritte &
     &              ueberschritten in stationaer --> keine konvergenz &
     &              erreicht weiter mit grenztiefe'',/)')
        ENDIF 
                                                                        
        IF (rdruck.eq.0) then 
          hr = hgrenz 
          froud = 1. 
          ikenn = 2 
        ELSE 
!JK       SCHREIBEN IN KONTROLLFILE                                     
          IF (lein.eq.3) then 
      WRITE (jw8, '(''keine konvergenz in stationaer'',                 &
     &  ''gleichfoermig -> Rechne mit Grenztiefe weiter.'')')           
          ENDIF 
                                                                        
          hr = hgrenz 
          ikenn = 5 
        ENDIF 
                                                                        
 9999   CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,       &
        indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg,   &
        itere1)                                                         
                                                                        
        IF (nprof.eq.1) then 
          hvst = 0. 
!                                /* 1.profil                            
          hrst = 0. 
          hborda = 0. 
        ENDIF 
                                                                        
        IF (ikenn.ne.1.and.ikenn.ne.2) then 
!         d.h. wenn keine grenztiefe errechnet                          
          hvst = 0. 
          hborda = 0. 
        ENDIF 
                                                                        
!JK   SCHREIBEN IN KONTROLLFILE                                         
        IF (lein.eq.3) then 
      WRITE (jw8, '(/,'' --> aus stationaer weiter mit hr ='',f7.3,     &
     &              '' froud = '',f7.3,'' hrst = '',f10.4, '' hvst= '', &
     &              f10.4,//)') hr, froud, hrst, hvst                   
        ENDIF 
                                                                        
        CALL erfroud (br, f, qt, u, iprof, indfl, indmax, froud) 
                                                                        
!JK   SCHREIBEN IN KONTROLLFILE                                         
        IF (lein.eq.3) then 
      WRITE (jw8, '(/,''brges = '',f10.3,'' fges = '',f10.3,'' tm = ''  &
     &  ,f10.3,'' vm = '',f10.3,/)') brges, fges, tm, vm                
          WRITE (jw8, '(''froud = '',f7.4,/)') froud 
      WRITE (jw8, '(''   i     f(i)      u(i)     br(i)   ra(i)   q(i)''&
     &  )')                                                             
                                                                        
          DO 289 i = 1, 3 
            WRITE (jw8, '(i4,5(f10.3))') i, f (i) , u (i) , br (i) , ra &
            (i) , qt (i)                                                
  289     END DO 
        ENDIF 
                                                                        
!JK   DATENUEBERGABE                                                    
        RETURN 
                                                                        
!JK   ITERATION MIT EINSCHLUSSINTERVALL                                 
 8000   hr = hmin + 0.05 
        dx = 0.2 
        ifehl = 0 
                                                                        
!JK   SCHREIBEN IN KONTROLLFILE                                         
        IF (lein.eq.3) then 
      WRITE (jw8, '('' keine konvergenz --> suchen einschlussintervall''&
     &,            /,'' i       sollgef.   istgef.      hr'')')         
        ENDIF 
                                                                        
        DO 8001 jsch = 1, 100 
                                                                        
          hborda = 0. 
 8002     CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,     &
          indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg, &
          itere1)                                                       
                                                                        
          hvst = 0. 
!                                 /* gleichfoermig                      
          dela = del 
          del = hrst / str + sgef 
          sist = hrst / str 
                                                                        
!JK        SCHREIBEN IN KONTROLLFILE                                    
          IF (lein.eq.3) then 
            WRITE (jw8, '(i4,4f15.3)') jsch, sgef, sist, hr 
          ENDIF 
                                                                        
          IF (jsch.gt.1) then 
            IF ( (dela * del) .le.1.e-7) then 
!              einschlussintervall gefunden                             
              hr = hra 
                                                                        
!JK                 SCHREIBEN IN KONTROLLFILE                           
              IF (lein.eq.3) then 
      WRITE (jw8, '('' einschlussintervall gefunden --> '',             &
     &   '' weiter mit hr = '',f15.3)')                                 
              ENDIF 
                                                                        
              h1x = hra 
              h2x = hr 
              f1 = salt 
              f2 = sist 
!JK                  ERMITTLUNG GEFAELLE                                
              GOTO 9000 
            ENDIF 
          ENDIF 
                                                                        
          hra = hr 
          hr = hr + dx 
          salt = sist 
 8001   END DO 
                                                                        
        ifehl = 1 
                                                                        
!JK   SCHREIBEN IN KONTROLLFILE                                         
        IF (lein.eq.3) then 
      WRITE (jw8, '('' kein einschlussintervall fuer stationaer '',     &
     &              '' gleichfoermig gefunden'')')                      
        ENDIF 
                                                                        
        IF (iprof.ne.' ') then 
                                                                        
!JK      SCHREIBEN IN KONTROLLFILE                                      
          IF (lein.eq.3) then 
      WRITE (jw8, '(''keine konvergenz in stationaer'',                 &
     &   ''gleichfoermig -> Rechne mit Grenztiefe weiter.'')')          
          ENDIF 
                                                                        
          hr = hgrenz 
                                                                        
          IF (rdruck.eq.0) then 
            ikenn = 2 
          ELSE 
            ikenn = 5 
          ENDIF 
!JK      NEUER VERSUCH MIT EINSCHLUSSINTERVALL                          
          GOTO 9999 
        ELSE 
          STOP ' in stationaer, da keine Konvergenz.' 
        ENDIF 
                                                                        
 9000   CONTINUE 
                                                                        
        jend = int (dx / 0.01) + 1 
!                                 /* 0.01 zu erzielende genauigkeit     
!JK   SCHREIBEN IN KONTROLLFILE                                         
        IF (lein.eq.3) then 
      WRITE (jw8, '(''mit 1cm schritten ermittlung des gefaelles'')') 
        ENDIF 
                                                                        
        DO 9001 jsch = 1, jend 
                                                                        
          hborda = 0. 
                                                                        
 9002     CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,     &
          indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg, &
          itere1)                                                       
                                                                        
          hvst = 0. 
!                                  /* gleichfoermig                     
          dela = del 
          del = hrst / str + sgef 
          sist = hrst / str 
                                                                        
!JK        SCHREIBEN IN KONTROLLFILE                                    
          IF (lein.eq.3) then 
            WRITE (jw8, '(i4,4f15.3)') jsch, sgef, sist, hr 
          ENDIF 
                                                                        
          IF (jsch.gt.1) then 
            IF ( (dela * del) .le.1.e-7) then 
!                  einschlussintervall gefunden                         
              IF (dela.lt.del) then 
                hr = hra 
                CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst,     &
                hrst, indmax, psiein, psiort, jw5, hi, xi, s, istat,    &
                froud, ifehlg, itere1)                                  
                GOTO 9999 
              ELSE 
                GOTO 9999 
              ENDIF 
            ENDIF 
          ENDIF 
                                                                        
          hra = hr 
          hr = hr + 0.01 
 9001   END DO 
                                                                        
        PRINT * , ' Logischer Fehler im Programm !!!' 
        PRINT * , ' Beim Entwickler melden !!!' 
        STOP 'stationaer nach 9001' 
                                                                        
!JK   ELSE ZU (bordvoll .ne. 'g')                                       
      ELSE 
                                                                        
        CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,       &
        indmax, psiein, psiort, jw5, hi, xi, s, istat, froud, ifehlg,   &
        itere1)                                                         
!JK   ENDIF ZU (bordvoll .ne. 'g')                                      
      ENDIF 
                                                                        
      RETURN 
                                                                        
      END SUBROUTINE station                        
                                                                        



