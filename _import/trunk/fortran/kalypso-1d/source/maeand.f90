!     Last change:  WP    7 Jul 2005   10:43 am
!--------------------------------------------------------------------------
! This code, maeand.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - maeand
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
SUBROUTINE maeand (cm)
!**
!UT   GESCHRIEBEN: JANA KIEKBUSCH 23.06.2000                            
!UT   KOMMENTIERT: ULF TESCHKE    28.08.2000                            
!**                                                                     
!**   BESCHREIBUNG: BERUECKSICHTIGT DEN MAEANDRIERUNGSVERLUST DURCH     
!**   ------------- KORREKTURFAKTOR c_m FUER WIDERSTANDSBEIWERT (LAMBDA)
!**                                                                     
!**   LIT: BWK-MERKBLATT                                                
!**        BWK-BERICHT                                                  
!**                                                                     
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**   sm      - SINUOSITAET [-]                                         
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**   cm      - KORREKTURPARAMETER FUER MAEANDRIERUNG [-]               
!**   ger_art - ART DES GERINNES                                        
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   - keine                                                           
!**                                                                     
!***********************************************************************
                                                                        
! Calling variables
REAL, INTENT(OUT) :: cm

! COMMON-Block /MAEANDER/ ------------------------------------------
REAL 			:: sm
CHARACTER(LEN=10) 	:: ger_art
COMMON / maeander / sm, ger_art
! ------------------------------------------------------------------
                                                                        


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
!UT   SETZEN VON cm GEMAESS TABELLE 3.20 BWK-MERKBLATT, S.38
!UT   FUER KOMPAKTES GERINNE
IF (ger_art.eq.'kompakt') then

  IF (sm.le.1.0) then
    cm = 1.0
  ELSEIF (sm.le.1.05) then
    cm = 6.4 * sm - 5.4
  ELSEIF (sm.le.1.5) then
    cm = 0.822 * sm + 0.457
  ELSEIF (sm.gt.1.5) then
    cm = 1.69
  ENDIF

!UT   SETZEN VON cm GEMAESS TABELLE 3.20 BWK-MERKBLATT, S.38
!UT   FUER GEGLIEDERTES GERINNE
ELSEIF (ger_art.eq.'gegliedert') then

  IF (sm.le.1.0) then
    cm = 1.0
  ELSEIF (sm.le.1.05) then
    cm = 1.0 + 16.0 * (sm - 1.0)
  ELSEIF (sm.le.1.5) then
    cm = 1.0 + 2.055 * (sm - 0.66)
  ELSEIF (sm.gt.1.5) then
    cm = 2.725
  ENDIF

ELSE
  cm = 1.0

ENDIF

END SUBROUTINE maeand                                            
