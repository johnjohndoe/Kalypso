!     Last change:  WP   11 Mar 2006    8:29 pm
!--------------------------------------------------------------------------
! This code, beiwert.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - beiwert
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


SUBROUTINE beiwert (huew, ii, wh, huw, iueart, cq, cm) 

!***********************************************************************
!**                                                                     
!**   SUBROUTINE BEIWERT                                                
!**                                                                     
!**   Diese Subroutine berechnet den Ueberfallbeiwert fuer die          
!**   Wehrberechnung                                                    
!**                                                                     
!**   geschrieben:  09.93  J. Csocsan                                   
!**   dokumentiert: 11.01  Ole Holm / Marco Wichers                     
!**                                                                     
!***********************************************************************
!**
!**   DIREKT UEBERGEBENE VARIABLEN                                      
!**   ----------------------------                                      
!**   cm                                                                
!**   cq                                                                
!**   huew                                                              
!**   huw                                                               
!**   ii                                                                
!**   iueart                                                            
!**   wh                                                                
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   beta    --      Parameter zur Berechnung des Überfallbeiwertes    
!**                   für rundkronige Wehre nach Knapp                  
!**   Wehre                                                             
!**   cm      --      Abminderungsfaktor für unvollkommenen Überfall    
!**   cq      --      Überfallbeiwert                                   
!**   d1      --      Verhältnis von Überfallhöhe zu Wehrhöhe           
!**   d2      --      Verhältnis von Wehrhöhe zu Überfallhöhe           
!**   df      --      Differenz                                         
!**   dh      --      Verhältnis der Wasserspiegelhöhe im Unterwasser   
!**                   zum Oberwasser                                    
!**   dif     --      Differenz                                         
!**   g2      --      Wurzel aus 2*Erdbeschleunigung                    
!**   how     --      Wasserspiegelhöhe im Oberwasser                   
!**   huew    --      Überfallhöhe                                      
!**   huw     --      Wasserspiegelhöhe im Unterwasser                  
!**   iueart  --      Art des Überfalls am Wehr                         
!**   lw      --      Länge des Wehres in Fließrichtung
!**   phi     --      Parameter zur Berechnung des Überfallbeiwertes    
!**                   für rundkronige Wehre nach Knapp                  
!**   Wehre                                                             
!**   phi1    --      Funktion von phi                                  
!**   phi2    --      Funktion von phi                                  
!**   r01     --      Radius der Wehrkrone in Iteration                 
!**   r011    --      Radius der Wehrkrone in Iteration                 
!**   rkr     --      Radius der Wehrkrone                              
!**   wart    --      Art des Wehres                                    
!**   wh      --      Wehrhöhe                                          
!**   y0      --      Überfallhöhe im Wehrscheitel                      
!**   y01     --      Überfallhöhe im Wehrscheitel                      
!**   y011    --      Überfallhöhe im Wehrscheitel                      
!**                                                                     
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   - KEINE                                                           
!***********************************************************************
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

CHARACTER(LEN=2) :: wart

REAL :: beiw (maxw), rkw (maxw), lw (maxw)

COMMON / wehr2 / beiw, rkw, lw, wart


!**   ------------------------------------------------------------------
!**   BERECHNUNGEN                                                      
!**   ------------------------------------------------------------------
                                                                        
!HW   Vollkommener Überfall

cm = 1.

!HW      Regelwehr mit Beiwert
IF (wart.eq.'bw') then

  cq = beiw (ii)

!HW      scharfkantiges Wehr
ELSEIF (wart.eq.'sk') then

  d1 = huew / wh
  d2 = 1. / d1

  !HW  Überfallbeiwert nach empirischer Formel von Rehbock
  !HW  für kleine relative Überströmungshöhen d1
  IF (d1.le.6.) then
    cq = 0.61 + 0.08 * d1

  !HW  Überfallbeiwert für kleine relative Wehrhöhen d2 -> scharfkan
  ELSEIF (d2.le.0.06) then
    cq = 1.061 * (1 + d2) **1.5

  ELSE
    !   Mittelwert
    cq = 1.15
  ENDIF

!HW      rundkroniges Wehr
ELSEIF (wart.eq.'rk') then

  rkr = rkw (ii)

  !HW   Überfallhoehe im Wehrscheitel
  y0 = 0.7 * huew
  dif = 100.

  it10max = 1000
  it10 = 0

  !JK  ---------------------------------------------------
  !JK  ITERATION ZUR BESTIMMUNG VON PHI, BETA
  !HW  nach Knapp, S. 228 ff, allg. Ueberfallgleichung

  DO 10 WHILE(dif .gt. 0.0001)

    WRITE (UNIT_OUT_LOG, '(''dif='',f8.4)') dif
    it10 = it10 + 1

    IF (it10.gt.it10max) then
      !                keine y0 gefunden
      y0 = .7 * huew

      !JK              SCHREIBEN IN KONTROLILE
      write (UNIT_OUT_LOG, '(''Keine y0-Tiefe gefunden y0=.7*HO ='',f8.4,/)') y0

      GOTO 20
    ENDIF

    !HW          Geschwindigkeitsverhaeltnis
    phi = rkr / (rkr + y0)

    !HW          Hilfsfunktionen
    phi1 = alog (1. / phi)
    phi2 = phi * phi

    !HW          Bedingungsgleichung für den Druckbeiwert beta
    beta = (1./phi2) * (1.+phi-(1. + 2.*phi) * phi1) / (1.+phi+(2.+phi) * phi1)

    y01 = huew * (1. - phi2) / (1. - phi2 * beta)

    r01 = huew * (phi * (1. + phi) / (1. - phi2 * beta) )

    !                 Berechnen y011 aus y01

    phi = rkr / (rkr + y01)

    phi1 = alog (1. / phi)
    phi2 = phi * phi

    beta = (1./phi2) * (1.+phi-(1. + 2.*phi) * phi1) / (1.+phi+(2.+phi) * phi1)

    y011 = huew * (1. - phi2) / (1. - phi2 * beta)

    r011 = huew * (phi * (1. + phi) / (1. - phi2 * beta) )

    df = (y01 - y011) / (r01 - r011)

    IF (abs (df) .lt.0.0001) then
      y0 = y01
      GOTO 20
    ENDIF

    dif = df * (rkr - r01)

    y0 = y0 + dif

  !JK         ENDE ITERATION ------------------------------------
  10 CONTINUE

  20 CONTINUE

  !HW         Überfallbeiwert für rundkronige Wehre nach Knapp, S. 232
  cq = (3./2.)*phi*( (1.+phi) * (1.-beta)**0.5) / (1. - beta*phi2) ** (3./2.) * phi1

ELSE

  !HW      breitkroniges Wehr
  !HW          Empirische Formel nach Knapp, S. 297
  cq = 1.8 * (huew / lw (ii) ) **0.0544

  !HW          Grenzwert für den Überfallbeiwert eines breitkronigen Wehre
  !HW          cq,grenz=1.7048, nach Knapp S. 295
  IF (cq.gt.1.705) cq = 1.705

  g2 = sqrt (2. * 9.81)

  cq = cq / ( (2. / 3.) * g2)

ENDIF

!JK      WENN UNVOLLKOMMENER UEBERFALL
IF (iueart.eq.1) then

  !           Berechnen des Minderungsfaktors
  !HW         für den Abfluss ueber das Wehr

  how = huew

  dh = huw / how

  !JK         FUER SCHARFKANTIGES WEHR
  !HW         Abminderungsfaktoren nach Press/Schroeder
  !HW         Annaeherung der nichtlinearen Funktion mittels Polygonzug
  IF (wart.eq.'sk') then

    IF (dh.le.0.5) then

      cm = 1. - 0.44 * dh

    ELSEIF (dh.gt.0.5.and.dh.le.0.8) then

      cm = 1.115 - 0.67 * dh

    ELSEIF (dh.gt.0.8.and.dh.le.0.95) then

      cm = 1.54 - 1.20 * dh

    ELSEIF (dh.gt.0.95.and.dh.le.1.0) then

      cm = 4.20 - 4.0 * dh

    ELSE

      !JK                SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, '(''Widerspruch!  dh=huw/how='',f8.4,'' >1.0 '')') dh
      WRITE (*, *) 'Widerspruch in der Beiwertberechnung, deshalb STOP!'
      STOP

    ENDIF

  !           untere Kurve rundkronig fuer die anderen Faelle
  !HW         Abminderungsfaktoren nach Press/Schroeder
  !HW         Annaeherung der nichtlinearen Funktion mittels Polygonzug
  ELSE

    IF (dh.le.0.25) then

      cm = 1.0

    ELSEIF (dh.gt.0.25.and.dh.le.0.78) then

      cm = 1.05 - 0.19 * dh

    ELSEIF (dh.gt.0.78.and.dh.le.0.90) then

      cm = 2.20 - 1.67 * dh

    ELSEIF (dh.gt.0.90.and.dh.le.0.95) then

      cm = 4.30 - 4.0 * dh

    ELSEIF (dh.gt.0.95.and.dh.le.1.00) then

      cm = 5.20 - 5.0 * dh

    ELSE

      !JK          SCHREIBEN IN KONTROLLFILE
      write (UNIT_OUT_LOG, '(''Widerspruch! dh=huw/how='',f8.4,'' >1.0 '')') dh
      write (*,*) 'Widerspruch in der Beiwertberechnung', 'deshalb STOP!!'
      STOP

    ENDIF

  !JK         ENDIF ZU (wart.eq.'sk')
  ENDIF

!JK      ENDIF ZU (iueart.eq.1)
ENDIF

END SUBROUTINE beiwert                                                                   
