!     Last change:  WP   12 Mar 2006    2:33 pm
!--------------------------------------------------------------------------
! This code, eb2kst.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - eb2kst
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

SUBROUTINE eb2kst (indmax, hr, hv, rg, q) 

!***********************************************************************
!**                                                                     
!**   SUBROUTINE EB2KST                                                 
!**                                                                     
!**   geschrieben :     24.08.1988  e.pasche                            
!**   geaendert   :     23.02.1989  v.thum (pc-version)                 
!**                     01.06.1989  v.thum (common/sdruck/ aufgenommen  
!**                                         aufrufparameter geaendert)  
!**                                                                     
!**   Allgemeine Beschreibung:                                          
!**                                                                     
!**   Das Programm eb2kst berechnet eine flaechengemittelte             
!**   Rauheitshoehe rg und eine Energiehoehe hv nach dem                
!**   Naudascher-Verfahren                                              
!**                                                                     
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   - KEINE                                                           
!***********************************************************************

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
                                                                        

! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


REAL alg (maxkla)
                                                                        
                                                                        
! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        

      hv = 0. 
      rg = 0. 
                                                                        
!JK   indmax = MAX. ANZAHL RAUHIGKEITSABSCHNITTE                        
      DO 400 i = 1, indmax 
        IF (ra (i) .le.1.e-04.or.br (i) .le.0.0001) then 
!            falls hydr. radius oder spiegelbreite = 0                  
          ts1 (i) = 0. 
          ts2 (i) = 0. 
          ra (i) = 0. 
          rb (i) = 0. 
                                                                        
        ELSE 
          rb (i) = f (i) / br (i) 
          ra1 (i) = ra (i) ** (2. / 3.) 
          ts1 (i) = rk (i) * ra1 (i) 
          ts2 (i) = f (i) * ts1 (i) 
                                                                        
        ENDIF 
!**                                                                     
        IF (rk (i) .le.1.e-04.or.ra (i) .le.1.e-04) then 
          alg (i) = 0. 
        ELSE 
          alg (i) = 8 * 9.81 / rk (i) **2 / ra (i) ** (1. / 3.) 
          alg (i) = alg (i) * u (i) 
        ENDIF 
!**                                                                     
                                                                        
  400 END DO 
                                                                        
      c1 = 0. 
      als1 = 0. 
      DO 401 i = 1, indmax 
        c1 = c1 + ts2 (i) 
!**                                                                     
        als1 = als1 + alg (i) 
!**                                                                     
  401 END DO 
                                                                        
                                                                        
!     Zwischenspeichern fuer mittlere Reibungsgefaelle                  
!     ----------------------------------------------                    
                                                                        
                                                                        
      IF (c1.lt.1.e-06) then 
        !    abbruch, weil profilwerte unsinnig  (c1=0)
        WRITE (UNIT_OUT_LOG, '(''abbruch, weil profilwerte unsinnig in eb2kst'',   &
         &        /,''--> return'')')

        RETURN 
                                                                        
      ELSE 
                                                                        
        rg = c1 
        c1 = q / c1 
        !**          rg  = c1
                                                                        
        IF (als1.lt.1.e-04) then 
          alges = 0. 
        ELSE 
          alges = uges / als1 
          alges = 1 / alges 
        ENDIF 

      ENDIF 
                                                                        
      IF (uges.gt.0) then 
                                                                        
        vges = q / fges 
        rges = fges / uges 
                                                                        
                                                                        
      ENDIF 
                                                                        
                                                                        
      c2 = 0. 
      DO 402 i = 1, indmax 
                                                                        
        v (i) = ts1 (i) * c1 
        qt (i) = f (i) * v (i) 
        c2 = f (i) * (v (i) **3) + c2 
  402 END DO 
                                                                        
      IF (abs (q) .le.1.e-06) then 
        PRINT * , 'kein Abfluss definiert. Ueberpruefe QWERT-Datei.' 
        STOP 'Programmabbruch in eb2kst' 
      ENDIF 
                                                                        
      hv = c2 / (2. * 9.81 * q) 
                                                                        
!     ------------------------------------------------------------------
!     ruecksprung, wenn kein konrollausdruck verlangt (kontrl=0)        
!     ------------------------------------------------------------------
      kontrl = 0 
      IF (kontrl.eq.0) return 
                                                                        
!     ------------------------------------------------------------------
!     wenn kontrollausdruck verlangt (kontrl=1)                         
!     ------------------------------------------------------------------
                                                                        
!  403 write(nfi,65) lfnr, nsch                                         
!      write(nfi,66)                                                    
                                                                        
      DO 40 i = 1, indmax 
        IF (rk (i) .le.0.00001) goto 40 
!        write(*,67) i,rk(i),f(i),u(i),ra(i),br(i),ts1(i),              
!    ^   ts2(i),v(i),qt(i)                                              
   40 END DO 
!      write(nfi,68)                                                    
!      write(nfi,69) hr,q,c1,rg,c2,hv                                   
                                                                        
      RETURN 
                                                                        
                                                                        
!**   ------------------------------------------------------------------
!**   Formate                                                           
!**   ------------------------------------------------------------------
                                                                        
   65 FORMAT(1h ,/,'berechnungsparameter profil',i5,2x,'schritt',i5,/) 
   66 FORMAT(1h ,1x,'fÅr jeden bereich',/,                              &
     & '   nr     rk        f         u         ra        br    ',      &
     & '   ts1       ts2        v         q',/,102('-'))                
   67 FORMAT(1x,i5,7f10.3,2f10.5) 
   68 FORMAT(1h ,/,'fÅr das profil:',/,                                 &
     &'    hr        qges       c1          rg         c2',             &
     &'            hv',/,70('-'))                                       
   69 FORMAT(1x,2(f10.3,1x),2(f10.6,1x),2(f12.6,1x)) 
                                                                        
      END SUBROUTINE eb2kst                         
