!     Last change:  WP    1 Feb 2005    2:52 pm
!--------------------------------------------------------------------------
! This code, prf.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - prf
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
SUBROUTINE prf (ik1, ik2)

!**   von CD F:WSP/WSP_NEU/GROSS/prf.for                                
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

COMMON / angabe / xko (mpts, 2), hko (mpts, 2), na1, na2
                                                                        
                                                                        
!     diese routine prueft mittels dem cosinussatz, ob punkte der       
!     linie ueberfluessig sind (ausser letzter pkt., der gleich dem     
!     ersten pkt ist)                                                   
      ik1 = 0 
      ik2 = 0 
      DO 50 ii = 1, 2 
        IF (ii.eq.1) then 
          npl = na1 
        ELSE 
          npl = na2 
        ENDIF 
  400   DO 100 i = 2, npl - 2 
!        untersuchen punkt i zwischen i-1 und i+1:                      
          c2 = (abs (xko (i, ii) - xko (i - 1, ii) ) ) **2. + (abs (hko &
          (i, ii) - hko (i - 1, ii) ) ) **2.                            
          c = sqrt (c2) 
          b2 = (abs (xko (i + 1, ii) - xko (i, ii) ) ) **2. + (abs (hko &
          (i + 1, ii) - hko (i, ii) ) ) **2.                            
          b = sqrt (b2) 
          a2 = (abs (xko (i + 1, ii) - xko (i - 1, ii) ) ) **2. +       &
          (abs (hko (i + 1, ii) - hko (i - 1, ii) ) ) **2.              
          a = sqrt (a2) 
!        23.01.98 J.Sz. (Dr. E. Pasche)                                 
!        2 Zeilen geaendert                                             
          IF (abs (c) .le.1e-04) goto 200 
          IF (abs (b) .le.1e-04) goto 100 
          cosalpha = c2 + b2 - a2 / (2. * b * c) 
          df = abs (cosalpha) - 1. 
          IF (abs (df) .le.0.01) goto 200 
  100   END DO 
        IF (ii.eq.1) then 
          IF (na1.ne.npl) ik1 = 1 
          na1 = npl 
          GOTO 50 
        ELSE 
          IF (na2.ne.npl) ik2 = 1 
          na2 = npl 
          GOTO 9999 
        ENDIF 
  200   DO 300 i2 = i, npl - 1 
          xko (i2, ii) = xko (i2 + 1, ii) 
          hko (i2, ii) = hko (i2 + 1, ii) 
  300   END DO 
        npl = npl - 1 
        GOTO 400 
                                                                        
   50 END DO 
 9999 RETURN 
      END SUBROUTINE prf                            
