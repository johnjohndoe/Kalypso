!     Last change:  JH    4 Oct 2004    3:34 pm

!**************************************LICENSE**************************************************
!
! This code is part of the library 'Kalypso-NA'.
! KALYPSO-NA is a deterministic, non-linear, detailed Rainfall-Runoff-Model (RRM).
! The model permits a complete simulation of the land bound
! part of the global water balance as a reaction on observed precipitation events.
! Copyright (C) 2004  HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering (in co-operation with Bjoernsen Cunsulting Engineers)
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
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Dipl.-Ing. Jessica Hübsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Hübsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************

      subroutine gamma(xx,gx,ier)

!**************************************ÄNDERUNGEN***********************************************

!     Date      Programmer      	Description of change
!     ====      ==========      	=====================

!***************************************BESCHREIBUNG********************************************
!c
!c     up gamma  gamma-funktion, werte fuer reelle a > 0
!c***********************************************************************
!c                                                                       gmmm  50
!c        purpose                                                        gmmm  60
!c           computes the gamma function for a given argument            gmmm  70
!c                                                                       gmmm  80
!c        usage                                                          gmmm  90
!c           call gamma(xx,gx,ier)                                       gmmm 100
!c                                                                       gmmm 110
!c        description of parameters                                      gmmm 120
!c           xx -the argument for the gamma function                     gmmm 130
!c           gx -the resultant gamma function value                      gmmm 140
!c           ier-resultant error code where                              gmmm 150
!c               ier=0  no error                                         gmmm 160
!c               ier=1  xx is within .000001 of being a negative integer gmmm 170
!c               ier=2  xx gt 57, overflow, gx set to 1.0e75             gmmm 180
!c                                                                       gmmm 190
!c        remarks                                                        gmmm 200
!c           none                                                        gmmm 210
!c                                                                       gmmm 220
!c        subroutines and function subprograms required                  gmmm 230
!c           none                                                        gmmm 240
!c                                                                       gmmm 250
!c        method                                                         gmmm 260
!c           the recursion relation and polynomial approximation         gmmm 270
!c           by c.hastings,jr., 'approximations for digital computers',  gmmm 280
!c           princeton university press, 1955                            gmmm 290
!c                                                                       gmmm 300
!c     ..................................................................gmmm 310
!c                                                                       gmmm 320

!***************************************EIN-/AUSGABE********************************************
!******************************************VARIABLEN********************************************
!******************************************ANWEISUNGEN******************************************

      if(xx-57.)6,6,4                                                   
4     ier=2
      gx=1.e25                                                          
      return                                                            
6     x=xx
      err=1.0e-6                                                        
      ier=0                                                             
      gx=1.0                                                            
      if(x-2.0)50,50,15                                                 
10    if(x-2.0)110,110,15
15    x=x-1.0
      gx=gx*x                                                           
      go to 10                                                          
50    if(x-1.0)60,120,110

!c                                                                       
!c        see if x is near negative integer or zero                      
!c

60    if(x-err)62,62,80
62    y=float(int(x))-x
      if(abs(y)-err)130,130,64                                          
64    if(1.0-y-err)130,130,70
!c                                                                       
!c        x not near a negative integer or zero                          
!c
70    if(x-1.0)80,80,110
80    gx=gx/x
      x=x+1.0                                                           
      go to 70                                                          
110   y=x-1.0
      gy=1.0+y*(-0.5771017+y*(+0.9858540+y*(-0.8764218+y*(+0.8328212+    &
         y*(-0.5684729+y*(+0.2548205+y*(-0.05149930)))))))                 
      gx=gx*gy                                                          
120   return
130   ier=1
      return

!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************
      end
