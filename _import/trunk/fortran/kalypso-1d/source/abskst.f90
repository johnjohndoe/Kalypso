!     Last change:  MD    8 Jul 2009    3:35 pm
!--------------------------------------------------------------------------
! This code, abskst.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - abskst
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



!***********************************************************************
SUBROUTINE abskst (nknot, x1, xi, h1, hi, s, DIM_ARRAY)
!
! geschrieben :                   24.08.1988  e. pasche
! geaendert   :                   25.08.2005  w. ploeger
!                                                                       
!---------------------------------------------------------------------  
! Allgemeine Beschreibung :
!
! Das Programm abskst berechnet den benetzten Umfang und die Breite
! der einzelnen Abschnitte im Profil fuer d. Spiegellinienberechnung
! mit Strickler-Beiwerten.
!
! *
!  *     Profil
!   *                    a
!   *                 |-----***************
!   *                 |    *
!   ********         b|  * s
!           *         | *
!            *        |*
!             *********
!
!
!
! INPUT            x1, h1, nknot
!
! BERECHNUNG VON   a=xi, b=hi, s
!
!-----------------------------------------------------------------------
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**   a       --      Betrag des Abstandes zweier nebeneinanderliegender
!**                   Punkte in x-Richtung
!**   b       --      Betrag des Abstandes zweier nebeneinanderliegender
!**                   Punkte in z-Richtung
!**   h1      --      z-Koordinate eines Punktes                        
!**   hi      --      Abstand zweier nebeneinanderliegender Punkte in z-Richtung
!**   nknot   --      Anzahl der Punkte in einem profil                 
!**   s       --      Abstand zweier nebeneinanderliegender Punkte      
!**   x1      --      x-Koordinate eines Punktes                        
!**   xi      --      Abstand zweier nebeneinanderliegender Punkte in x-Richtung

!-----------------------------------------------------------------------
!     Vereinbarungsteil                                                 
!-----------------------------------------------------------------------

implicit none

! Calling variables
INTEGER, INTENT(IN) :: nknot
INTEGER, INTENT(IN) :: DIM_ARRAY

REAL, DIMENSION(DIM_ARRAY), INTENT(IN)  :: x1, h1
REAL, DIMENSION(DIM_ARRAY), INTENT(OUT) :: xi, hi, s

! Local variables
REAL :: a, b
INTEGER :: n

!-----------------------------------------------------------------------
!     RECHNUNGEN                                                        
!-----------------------------------------------------------------------
                                                                        
DO n = 1, nknot - 1
  a = x1 (n+1) - x1(n)
  xi (n) = abs (a)
  b = h1(n+1) - h1(n)
  hi (n) = abs (b)
  s (n) = sqrt (a * a + b * b)
END DO

END SUBROUTINE abskst           
