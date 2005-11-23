!     Last change:  WP   30 May 2005    1:40 pm
!--------------------------------------------------------------------------
! This code, wspow.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - wspow
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

SUBROUTINE wspow (henow, strbr, q, q1, hrow, hv, rg, indmax, hvst,&
     & hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt, nz,    &
     & idr1)

!***********************************************************************
!**                                                                     
!**   Subroutine wspow                                                  
!**                                                                     
!JK   BESCHREIBUNG: BERECHNUNG DES WASSERSPIEGELS IM OBERWASSER         
!JK                 AUS DER ENERGIEHOEHE                                
!**                                                                     
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   df      --      Differenz                                         
!**   dif     --      Differenz                                         
!**   dx      --      Schrittweite                                      
!**   he1     --      Energiehöhe                                       
!**   hea     --      Energiehöhe                                       
!**   henow   --      Energiehöhe im Oberwasser                         
!**   hranf   --      Anfangswasserspiegelhöhe                          
!**   hrow    --      Wasserspiegelhöhe im Oberwasser                   
!**   hrowa   --      Wasserspiegelhöhe im Oberwasser                   
!**   hukmax  --      maximale Höhe der Unterkante                      
!**   hv      --      Geschwindigkeitsverlust                           
!**   iwehr   --      Abfrage nach Wehrprofil                           
!**   jw8     --      Name des Kontrollfiles                            
!**   lein    --      Art des Ergebnisausdruckes                        
!**   wsanf   --      Anfangswasserspiegelhöhe                          
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   wspanf(hrow,strbr,q,q1,hr,hv,rg,indmax,hvst,hrst,                 
!**          psieins,psiorts,jw5,nprof,hgrenz,ikenn,nblatt,nz)          
!**                                                                     
!***********************************************************************
                                                                        
! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

CHARACTER(1) idr1
                                                                        
! commonblock fuer die Brueckenberechnung
CHARACTER(1) ibridge
REAL xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
COMMON / brueck / iwl, iwr, nuk, nok, xuk, huk, xok, hok, hukmax, &
 & hokmax, hsuw, raub, breite, xk, ibridge
                                                                        
! commonblock fuer die Wehrberechnung

INTEGER iwmin, nokw, nwfd

CHARACTER(1) iwehr
CHARACTER(2) wart

REAL xokw (maxkla), hokw (maxkla)
REAL xtrw (maxw), htrw (maxw)

INTEGER ianfw (maxw), iendw (maxw)

COMMON / wehr / xokw, hokw, nokw, iwmin, hokwmin, iwehr, xtrw,    &
 & htrw, nwfd, iendw, ianfw

COMMON / ausgabeart / lein, jw8

INTEGER itmax_ow

! ------------------------------------------------------------------
! PROGRAMMBEGINN
! ------------------------------------------------------------------
                                                                        
IF (iwehr.eq.'w') hukmax = 0.0
                                                                        
dx = 0.10
                                                                        
itmax_ow = 20
                                                                        
hranf = henow - .15

hrow = hranf
                                                                        
!JK    SCHREIBEN IN KONTROLLFILE                                        
IF (lein.eq.3) then
      WRITE (jw8, '(''bestimmung des wasserspiegels im oberwasser'',    &
     &     '' aus der energiehoehe im oberwasser'',/,''henow= '',       &
     &     f8.4)') henow                                                
ENDIF
                                                                        
      !JK    START ITERATIONSSCHLEIFE ZUR BERECHNUNG WSP IM OBERWASSER
      !JK    ---------------------------------------------------------
      DO 10 i = 1, itmax_ow
                                                                        
        !JK      SCHREIBEN IN KONTROLLFILE
        IF (lein.eq.3) write (jw8, '(''it= '',i2)') i 
                                                                        
                                                                        
        CALL wspanf (hrow, strbr, q, q1, hr, hv, rg, indmax, hvst, hrst,&
        psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt, nz, idr1)  
                                                                        
        he1 = hrow + hv 
        dif = henow - he1 
                                                                        
        !JK      SCHREIBEN IN KONTROLLFILE
        IF (lein.eq.3) then 
          WRITE (jw8, '(''he1= '',f8.4,'' dif= '',f8.4)') he1, dif 
        ENDIF 
                                                                        
        IF (abs (dif) .lt.1.e-04) then 

          IF (hrow.gt.hukmax) then 
            !JK            SCHREIBEN IN KONTROLLFILE
            IF (lein.eq.3) then 
              WRITE (jw8, '(''abbruch der iteration im schritt'',i3,            &
              &'' wasserspiegel oberwasser:'',f8.4,/,''energielinie '',          &
              &''im oberwasser:'',f8.4)') i, hrow, he1
            ENDIF 
                                                                        
            GOTO 9999 

          ELSE 

            !JK            SCHREIBEN IN KONTROLLFILE                                
            IF (lein.eq.3) then 
              WRITE (jw8, '(''wasserspiegel oberwasser unter hukmax'',          &
               &     '' rechne hrow = henow weiter'')')
            ENDIF 
                                                                        
            wsanf = henow 
                                                                        
            CALL wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, &
            hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt,  &
            nz, idr1)                                                   

            RETURN 

          ENDIF 

        ENDIF 


        hrowa = hrow - dx 
                                                                        
        CALL wspanf (hrowa, strbr, q, q1, hra, hva, rg, indmax, hvst,   &
        hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt, nz,  &
        idr1)                                                           
                                                                        
                                                                        
        hea = hrowa + hva 
        df = (hea - he1) / (hrowa - hrow) 
                                                                        
        IF (df.lt.1.e-04) then 

          !JK         SCHREIBEN IN KONTROLLFILE
          IF (lein.eq.3) then 
            WRITE (jw8, '(''keine loesung fuer oberwasser hrow = henow!'')')
          ENDIF 
                                                                        
          hrow = henow 
                                                                        
          CALL wspanf (hrow, strbr, q, q1, hr, hv, rg, indmax, hvst,    &
          hrst, psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt, nz,&
          idr1)                                                         

          RETURN 
                                                                        
        ENDIF 
                                                                        
        hrow = hrow + (henow - he1) / df 
                                                                        
        !JK      SCHREIBEN IN KONTROLLFILE
        IF (lein.eq.3) then 
          WRITE (jw8, 9810) hea, df, hrow 
          9810 FORMAT   ('hea= ',f8.4,' df= ',f8.4,' hrow= ',f8.4)
        ENDIF 
                                                                        
                                                                        
   10 END DO 
                                                                        
      !JK    SCHREIBEN IN KONTROLLFILE
      IF (lein.eq.3) then 
        WRITE (jw8, '(''keine konvergenz fuer oberwasser hrow = henow '')')
      ENDIF 
                                                                        
      wsanf = henow 
      hrow = henow 
                                                                        
      CALL wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, hrst, &
      psieins, psiorts, jw5, nprof, hgrenz, ikenn, nblatt, nz, idr1)    
                                                                        
 9999 RETURN 
                                                                        
      END SUBROUTINE wspow                          
