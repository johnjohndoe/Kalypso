!     Last change:  WP   30 May 2005   10:44 am
!--------------------------------------------------------------------------
! This code, kouwen.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - kouwen
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
 SUBROUTINE kouwen (meik, isk, hgk, grass, dhk, alk)

!********************************************************************** 
!**   DEJAN KOMATINA, 7 Jun 2001                                        
!**   Calculation of friction factor due to flexible vegetation (grass)
!********************************************************************** 

!WP 01.02.2005
USE KONSTANTEN

CHARACTER(4) grass
REAL meik, isk, hgk, dhk, alk
REAL kgk, taub, vsk, vskcr, aa, bb

!     Parameters for printing warnings and comments!                    
      COMMON / pri / ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum 
                                                                        
!      PRINT *,'Entered Sub KOUWEN!'                                    
!      PRINT *,'meik=',meik                                             
!      PRINT *,'ibed=',ibed,' isk=',isk,' hgk=',hgk                     
!      print *,'grass=',grass,' dhk=',dhk,' alk=',alk                   
!      PRINT *                                                          
                                                                        
      IF (grass.EQ.'user') then 
        IF (meik.eq.0.0) then 
          PRINT * , 'Sub KOUWEN!' 
          PRINT * , 'The user-defined value of MEI equal to 0!' 
          PRINT * , 'Modify the value in PRF file!' 
          PRINT * 
        ENDIF 
        GOTO 321 
      ENDIF 
      IF (grass.EQ.'dead') then 
        meik = 25.4 * hgk**2.26 
      ELSEIF (grass.EQ.'veg ') then 
        meik = 319 * hgk**3.3 
      ELSE 
        PRINT * , 'Sub KOUWEN!' 
        PRINT * , 'Name of grass type given improperly!' 
        PRINT * , 'Modify the name in PRF file!' 
        PRINT * 
      ENDIF 
                                                                        
  321 taub = rho * g * (dhk / 4.) * isk 
      vsk = (taub / rho) **0.5 
      vskcr = 0.23 * meik**0.106 
      IF (vskcr.GT. (0.028 + 6.33 * meik) ) vskcr = 0.028 + 6.33 * meik 
      IF (vsk / vskcr.lt.1.) then 
        aa = 0.15 
        bb = 1.85 
      ELSEIF (vsk / vskcr.ge.1.and.vsk / vskcr.lt.1.5) then 
        aa = 0.20 
        bb = 2.70 
      ELSEIF (vsk / vskcr.ge.1.5.and.vsk / vskcr.lt.2.5) then 
        aa = 0.28 
        bb = 3.08 
      ELSEIF (vsk / vskcr.ge.2.5) then 
        aa = 0.29 
        bb = 3.50 
      ENDIF 
                                                                        
      kgk = 0.14 * hgk * ( (meik / taub) **0.25 / hgk) **1.59 
                                                                        
!      PRINT *,'Calculated kg=',kgk                                     
                                                                        
!     Physical limit: grass height affected by the flow can be either   
!     less than or equall to grass height not affected by the flow !!!  
      IF (kgk.gt.hgk) kgk = hgk 
                                                                        
!     If hydraulic radius is less than the grass height affected by the 
!     flow, then exclude the second term from resistance equation !!!   
      IF ( (dhk / 4.) .lt.kgk) bb = 0. 
                                                                        
      alk = (aa + bb * log10 ( (dhk / 4.) / kgk) ) ** ( - 2)
                                                                        
!      PRINT *,'meik=',meik,' taub=',taub,' kgk=',kgk                   
!      print *,'vsk=',vsk,' vskcr=',vskcr,' vsk/vskcr=',vsk/vskcr       
!      print *,'aa=',aa,' bb=',bb                                       
!      PRINT *,'alk=',alk                                               
!      PRINT *,'Exiting Sub KOUWEN!'                                    
!      PRINT *                                                          
                                                                        
!     Setting codes for printing warnings!                              
!     Left floodplain (ibed=1)                                          
      IF (ibed.eq.1) then 
        IF (meik.gt.200) ikol = 1 
!          if (kgk.gt.hgk) ikol=2                                       
        IF ( (dhk / 4.) .lt.kgk) ikol = 2 
      ENDIF 
!     Main channel (ibed=2)                                             
      IF (ibed.eq.2) then 
        IF (meik.gt.200) ikom = 1 
!          if (kgk.gt.hgk) ikom=2                                       
        IF ( (dhk / 4.) .lt.kgk) ikom = 2 
      ENDIF 
!     Right floodplain (ibed=3)                                         
      IF (ibed.eq.3) then 
        IF (meik.gt.200) ikor = 1 
!          if (kgk.gt.hgk) ikor=2                                       
        IF ( (dhk / 4.) .lt.kgk) ikor = 2 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE kouwen                         
