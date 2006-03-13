!     Last change:  WP   13 Mar 2006   12:57 pm
!--------------------------------------------------------------------------
! This code, iterat.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - iterat
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

SUBROUTINE iterat (hr, hdif, hsohl, iart, cd, a, ai, x1, h1,      &
                 & nknot, il, ir, q, m, mi, staso)

!***********************************************************************
!**                                                                     
!JK   SUBROUTINE ITERAT                                                 
!**                                                                     
!JK   Diese Subroutine berechnet die Wasserspiegelhoehe mit Hilfe       
!JK   der Impulsgleichung                                               
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   a       --      durchströmte Fläche                               
!**   ai      --      durchströmte Teilfläche                           
!**   delta   --      Grenzbedingung                                    
!**   difa    --      Differenz                                         
!**   difn    --      Differenz                                         
!**   dx      --      Schrittweite                                      
!**   hdif    --      Höhendifferenz                                    
!**   hr      --      Wasserspiegelhöhe                                 
!**   hra     --      Wasserspiegelhöhe                                 
!**   hrb     --      Wasserspiegelhöhe                                 
!**   hropt   --      optimale Wasserspiegelhöhe                        
!**   hsohl   --      Sohlhöhe                                          
!**   ifehl   --      Fehlervariable                                    
!**   ischnitt--      Anzahl der Schnittpunkte                          
!**   m       --      Impuls
!**   ma      --      Impuls                                            
!**   mb      --      Impuls                                            
!**   mi      --      Impuls                                            
!**   q       --      Abfluß                                            
!**   vm      --      mittlere Geschwindigkeit                          
!**   vmi     --      mittlere Teilgeschwindigkeit                      
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!JK    impuls                                                           
!***********************************************************************
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

CHARACTER(2) id
COMMON / iprint / id
COMMON / flaechen / ad, aue, apl, apg, hss, hue, hpl, hpg

REAL m, mi, ma, mb
REAL ms (100)
REAL x1 (maxkla), h1 (maxkla)
                                                                        

      ! ------------------------------------------------------------------
      ! BERECHNUNGEN
      ! ------------------------------------------------------------------
                                                                        
      !JK   SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(//,''Beginn der Iteration in subroutine iteration : '')')

      !     anfangswert:
      delta = 0.01 
      difopt = 10000. 
      dx = 0.05 
      difa = 10000. 
                                                                        
      IF (hr.le.hsohl) then 
        hr = 4. * dx + hsohl 
      ENDIF 
                                                                        
      ischnitt = 0 
      vm = q / a 

   11 CONTINUE 
                                                                        
      !JK   SCHREIBEN IN KONTROLLFILE

      WRITE (UNIT_OUT_LOG, '(''q= '',f10.3,'' iart = '',i2,'' m= '',f10.3,/)') q, iart, m
      WRITE (UNIT_OUT_LOG, '(''i   hr     mi     ad     aue     apl     apg     hss     hue     hpl     hpg'')')

                                                                        

      !JK   ITERATIONSSCHLEIFE
      !JK   ------------------
      DO 10 i = 1, itmax 
                                                                        
        !           wiederherstellen der urspruenglichen profilwerte:
        CALL impuls (hr, cd, x1, h1, hdif, hsohl, nknot, il, ir, q, iart, mi, ai, ifehl)
                                                                        
        IF (ifehl.ne.0) then
          !               kein durchflussquerschnitt --> hr zu klein
          hr = hr + abs (hr - hsohl) 
        ENDIF 

        !           ueberpruefen der bedingung mi = m !!!                       

        !JK         SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(i4,10f8.3,a2)') i, hr, mi, ad, aue, apl, apg, hss, hue, hpl, hpg, id

        IF (hr.le.hsohl) then 
          PRINT * , 'stop in iteration' 
          PRINT * , 'hr < hsohl'
        ENDIF 
                                                                        
        difn = abs (mi - m) 
                                                                        
        IF (difn.lt.difopt) then 
          hropt = hr 
          difopt = difn 
        ENDIF 
                                                                        
        difa = difn 
                                                                        
        IF (abs (mi - m) .lt.delta) then 
                                                                        
          !               2 schnittpunkte sind moeglich:
          !               --> ueberpruefung der fliessgeschwindigkeit
          !                   (kontinuitaetsbedingung q=v*a)
                                                                        
          vmi = q / ai 
                                                                        
          IF ( (a - ai) .le.delta) then 
            IF ( (vmi - vm) .le.delta) then 
              ! schnittpunkt gefunden
              !JK DATENUEBERGABE
              GOTO 20 

            ELSE 
              ! neue iteration mit anderem anfangswert :
              ! aendern der iterationsrichtung:
                                                                        
              hr = hsohl + hdif + 10. 
              !JK  ZUM SCHLEIFENANFANG
              GOTO 11 
            ENDIF 

          ELSE 

            IF (vmi.gt.vm) then 
              ! schnittpunkt gefunden
              !JK   DATENUEBERGABE
              GOTO 20 

            ELSE 

              ! neue iteration mit anderem anfangswert :
              ! aendern der iterationsrichtung
              hr = hdif + hsohl + 10. 
              !JK     ZUM SCHLEIFENANFANG
              GOTO 11 

            ENDIF 

          ENDIF 
                                                                        
        !JK         ELSE ZU (abs(mi-m).lt.delta)
        ELSE 
                                                                        
          ! ansonsten : neuer schaetzwert nach newton'scher iteration
          ! ableitung:
          hra = hr + dx 
          hrb = hr - dx 
                                                                        
          IF (hrb.le.hsohl) hrb = hr 
                                                                        
          CALL impuls (hra, cd, x1, h1, hdif, hsohl, nknot, il, ir, q, iart, ma, aa, ifehl)
                                                                        
          CALL impuls (hrb, cd, x1, h1, hdif, hsohl, nknot, il, ir, q, iart, mb, ab, ifehl)
                                                                        
          IF (ifehl.ne.0) then 
            !  kein durchflussquerschnitt --> hrb nicht beruecksichtigen
            df = (ma - mi) / (hra - hr) 
          ELSE 
            df = (ma - mb) / (hra - hrb) 
          ENDIF 
                                                                        
          !  geradengleichung der tangente und schnitt mit sollwert m:
                                                                        
          IF (abs (df) .le.0.001) then 
            dif = 0.1 
          ELSE 
            dif = (m - mi) / df 
          ENDIF 
                                                                        
          IF (abs (dif) .le.0.005) then 
            WRITE (UNIT_OUT_LOG, '('' abbruch der iteration --> dif = '', f15.3)') dif
            !JK    ZU ENDE PROGRAMM
            GOTO 8000 
          ENDIF 
                                                                        
          hra = hr 
          hr = hr + (m - mi) / df 
          IF ( (hr - hsohl) .le.0.0001) hr = (hra + hsohl) / 2. 
                                                                        
          hr = (hra + hr) / 2. 
          IF (abs (dif) .lt.0.005) then 
            IF (abs (m - mi) .lt.0.1.or.abs (hr - hra) .lt.0.005) then 
              !JK                    ZU ENDE PROGRAMM
              GOTO 8000 
            ENDIF 
          ENDIF 

        !JK         ENDIF ZU (abs(mi-m).lt.delta)                               
        ENDIF 
                                                                        
   10 END DO 
!JK   ENDE ITERATIONSSCHLEIFE----------------------------------------   
                                                                        
                                                                        
      !JK       WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !**       print *,'maximale anzahl der iterationsschritte'
      !**       print *,'ueberschritten !!! -->  weiter mit optimalstem wert'
                                                                        
                                                                        
      hr = hropt 

      CALL impuls (hr, cd, x1, h1, hdif, hsohl, nknot, il, ir, q, iart, mi, ai, ifehl)

      difn = abs (mi - m) 
                                                                        
                                                                        
      !JK       WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !**       print *,' m = ',m,' mi= ',mi, ' hr= ',hr
      !**20       print *
      !**    print *,'anzahl der iterationschritte : ',i
      !*****
      !**      print *,'hr = ',hr,' ermittelt!!'
      !**      print *,'***************************************************'
      !**      print *,'*  ergebnisse :                                   *'
      !**      print *,'*                                                 *'
      !**      print *,'*  - hr = ',hr
      !**      print *,'*  - durchstroemte flaeche : a   = ',a,' (aue+a)'
      !**      print *,'*                            aue = ',aue
      !**      print *,'*  - angestroemte flaeche :  apl = ',apl
      !**      print *,'*  -                         apg = ',apg
      !**      print *,'***************************************************'
      !       call graf(i,ms,m)                                                
                                                                        
   20 RETURN 
                                                                        
 8000 CONTINUE
                                                                        
      !JK      WAR SCHON DEAKTIVIERT, 01.05.00, JK
      !**8000  print *,'abbruch der iteration : dif < 0.005  '
                                                                        
      hr = hropt 
                                                                        
      CALL impuls (hr, cd, x1, h1, hdif, hsohl, nknot, il, ir, q, iart, mi, ai, ifehl)
                                                                        
      RETURN

      END SUBROUTINE iterat                         
