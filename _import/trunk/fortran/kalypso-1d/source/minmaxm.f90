!     Last change:  WP   30 May 2005   10:51 am
!--------------------------------------------------------------------------
! This code, minmaxm.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - minmaxm
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

SUBROUTINE minmaxm (n, x, j, xm, funktion) 

                                                                        
!     ******************************************************************
!     Programmbeschreibung und Variablendefinition                      
!     ******************************************************************
                                                                        
!     Diese Routine findet Maximal-,Minimal- oder Mittelwert des        
!     Vektors x                                                         
                                                                        
!     Variablendefinition:                                              
                                                                        
!     Name     Typ     Uebergabe   Erklaerung                           
                                                                        
!     funktion char*3  input       Suchkriterium (min,max od. mid)      
!     n        int     input       Laenge des Vektors x                 
!     x        real(*) input       Koordinaten des Vektors x            
                                                                        
!     j        int     output      Nummer des Extrempunktes             
!     xm       real    output      xm=Minimum,    wenn funktion='min'   
!                                  xm=Maximum,    wenn funktion='max'   
!                                  xm=Mittelwert, wenn funktion='mid'   
                                                                        
!     ******************************************************************
!     Entwicklung und Aenderungen                                       
!     ******************************************************************
                                                                        
!     Entwickelt : 1991     Dr. A. Leibo                                
!     Aenderungen: 15.08.91 Stuermer                                    
!                           Aenderung der uebergebenen Feldgroesse      
!                           geaendert von max(=Uebergabeparameter) in * 
                                                                        
!     ******************************************************************
!     Programmbeginn                                                    
!     ******************************************************************
                                                                        
DIMENSION x ( * )
CHARACTER(LEN=3), INTENT(IN) :: funktion

      j = 0 
      xm = x (1) 
      IF (funktion.eq.'max') then 
        DO 1 i = 2, n 
          IF (xm.ge.x (i) ) goto 1 
          xm = x (i) 
          j = i 
    1   END DO 
      ENDIF 
                                                                        
      IF (funktion.eq.'min') then 
        DO 2 i = 2, n 
          IF (xm.le.x (i) ) goto 2 
          xm = x (i) 
          j = i 
    2   END DO 
      ENDIF 
                                                                        
      IF (funktion.eq.'mid') then 
        xm = float (0) 
        DO 3 i = 1, n 
          xm = xm + x (i) 
    3   END DO 
        xm = xm / float (n) 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE minmaxm                        
