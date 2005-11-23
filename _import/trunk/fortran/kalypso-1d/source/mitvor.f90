!     Last change:  WP   30 May 2005   10:51 am
!--------------------------------------------------------------------------
! This code, mitvor.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - mitvor
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
!**********************************************************************
SUBROUTINE mitvor (ct, dp, ax, bmv, hvor, lamvog)

!*********************************************************************  
!**                                                                     
!**   geschrieben :                               20.11.84 b.schwamborn 
!**   geaendert   :                               28.3.1985 juergen kuck
!**                                                                     
!**                                                                     
!**   Subroutine 'mitvor' berechnet die mitwirkende vorlandbreite       
!**   AUFRUF IN SUB PASCHE                                              
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   ax      --      Bewuchsparameter                                  
!**   bmv     --      mitwirkende Breite des Vorlandes                  
!**   ct      --      Faktor zur Berechnung der Trennflächengeschwindigk
!**   dp      --      Bewuchsparameter                                  
!**   hvor    --      mittlere Wasserspiegelhöhe des Vorlandes          
!**   lamvog  --      Widerstandsbeiwert des Vorlandes                  
!**                                                                     
!**   Aufgerufene Unterprogramme :                                      
!**                                                                     
!**   keine                                                             
!**                                                                     
!***********************************************************************

!**   ------------------------------------------------------------------
!**   Vereinbarungsteil                                                 
!**   ------------------------------------------------------------------
REAL, INTENT(IN)  :: ct, dp, ax, hvor, lamvog
REAL, INTENT(OUT) :: bmv

!**   ------------------------------------------------------------------
!**   Programmanfang                                                    
!**   ------------------------------------------------------------------
                                                                        
IF (dp / ax .gt. 0.5) then
  bmv = dp
else
  ! naeherungsweise bestimmung von bmv als anfangswert nach
  ! dissertation pasche diagramm 5.39 bzw.angenaeherter parabel :
  !UT   BWK, S.36, FORMEL 41
  bmv = hvor / (lamvog * (0.068 * exp (0.564 * ct) - 0.0558) )
END if

END SUBROUTINE mitvor
