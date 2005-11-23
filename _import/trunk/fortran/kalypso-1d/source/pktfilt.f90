!     Last change:  WP   30 May 2005   10:54 am
!--------------------------------------------------------------------------
! This code, pktfilt.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - pktfilt
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

SUBROUTINE pktfilt (n, np, x, y, z, gen) 

!***********************************************************************
!**                                                                     
!**   SUBROUTINE PKTFILT                                                
!**                                                                     
!JK   BESCHREIBUNG                                                      
!**                                                                     
!     Diese Routine filtert Punkte aus einem 4-dim Feld (x,y,z-Koord. un
!     Punktnummer) in abhaengigkeit von der Genauigkeit gen.            
!**                                                                     
!**                                                                     
!     Entwickelt : 1991     Dr. A. Leibo                                
                                                                        
!     Aenderungen: 15.08.91 Stuermer                                    
!                           Aenderung der uebergeben Feldgroesse        
!                           geaendert von max(=Uebergabeparameter) in * 
!**                                                                     
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**                                                                     
!     Name   Typ     Uebergabe    Erklaerung                            
                                                                        
!     gen    real    input        Genauigkeit                           
                                                                        
!     n      int     input/output Anzahl der Punkte                     
!     x,y,z  real(*) input/output Koordinaten der Punkte                
!     np     int (*) input/output Punktnummer                           
!**                                                                     
!***********************************************************************
                                                                        
                                                                        
                                                                        
      DIMENSION x ( * ), y ( * ), z ( * ), np ( * ) 
                                                                        
                                                                        
      k = 0 
                                                                        
      DO 1 i = 1, n - 1 
                                                                        
        l = 0 
                                                                        
        DO 2 j = i + 1, n 
                                                                        
!UT   PRUEFUNG OB DAS QUADRAT DER DIFFERENZ GROESSER ALS DAS            
!UT   QUADRAT DER VORGEGEBEN GENAUIGKEIT gen IST                        
          IF ( (x (i) - x (j) ) **2 + (y (i) - y (j) ) **2.gt.gen * gen)&
          goto 2                                                        
                                                                        
          l = 1 
                                                                        
!      print *,'punkte',i,' und',j,' wiederholen sich'                  
                                                                        
    2   END DO 
                                                                        
        IF (l.eq.0) then 
          k = k + 1 
          x (k) = x (i) 
          y (k) = y (i) 
          z (k) = z (i) 
          np (k) = np (i) 
        ENDIF 
                                                                        
    1 END DO 
                                                                        
!UT   n = Anzahl der geprueften Punkte                                  
                                                                        
      k = k + 1 
      x (k) = x (n) 
      y (k) = y (n) 
      z (k) = z (n) 
      np (k) = np (n) 
      n = k 
                                                                        
      RETURN 
                                                                        
!**   ENDE SUB pktfilt                                                  
      END SUBROUTINE pktfilt                        
