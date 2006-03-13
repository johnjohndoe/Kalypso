!     Last change:  WP   10 Mar 2006   10:38 pm
!--------------------------------------------------------------------------
! This code, gwehr.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - g_wehr
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

SUBROUTINE g_wehr (hr, ifehl1, auew, huew, wl, hwmin)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE G_WEHR ZU WSPWIN                                       
!**                                                                     
!JK   BESCHREIBUNG: BERECHNUNG WEHRGEOMETRIE (WEHROBERKANTE, WASSER-    
!JK                 SPIEGEL, WEHRLAENGE)                                
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**   ah      --      Polygonfläche                                     
!**   at      --      Polygonfläche                                     
!**   auew    --      Überfallfläche am Wehr                            
!**   gen     --      Fehlergenauigkeit                                 
!**   hko     --      Schnittpunktkoordinate in x-Richtung Oberkante    
!**   hl      --      Schnittpunktkoordinate in z-Richtung links        
!**   hmin    --      minimale Profilhöhe                               
!**   hokwmin --      minimale Höhe der Wehroberkante im Profil         
!**   hr      --      Wasserspiegelhöhe                                 
!**   hr2     --      z-Koordinate                                      
!**   hwmin   --      minimale Wehrhöhe                                 
!**   ianfw   --      Punktnummer des Anfangspunktes des Wehres         
!**   iendw   --      Punktnummer des Endpunktes des Wehres             
!**   ifehl1  --      Fehlervariable                                    
!**   ifehl2  --      Fehlervariable                                    
!**   kr0     --      Anzahl der Polygonpunkte                          
!**   kr1     --      Anzahl der Polygonzüge                            
!**   kr2     --      Anzahl der Polygonpunkte                          
!**   mr2     --      Anzahl der Wehrpunkte
!**   n_anf   --      Punktnummer des Anfangspunktes des Wehres         
!**   n_end   --      Punktnummer des Endpunktes des Wehres             
!**   na1     --      Schnittpunkt 1                                    
!**   na2     --      Schnittpunkt 2                                    
!**   npl1    --      Schnittpunkt 1                                    
!**   npl2    --      Schnittpunkt 2                                    
!**   nwf     --      Anzahl der Polygonpunkte                          
!**   nwfd    --      Anzahl der Wehrfelder                             
!**   nwp     --      Anzahl der Wehrpunkte                             
!**   wl      --      Länge des Wehres in Fließrichtung                 
!**   xko     --      Schnittpunktkoordinate in z-Richtung Oberkante    
!**   xl      --      Schnittpunktkoordinate in x-Richtung links        
!**   xmax    --      maximale x-Koordinate                             
!**   xmin    --      minimale x-Koordinate                             
!**   xokw    --      x-Koordinate der Wehroberkante                    
!**   xr2     --      x-Koordinate                                      
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!**   linie (hmin,xokw,hokw,nokw,ianfw(j),iendw(j),npl1,xl,hl)          
!**   schwpkt (maxkla,npw,xl,hl,xss,ht,at,igra,gen)                     
!**                                                                     
!***********************************************************************
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
                                                                        
USE DIM_VARIABLEN
USE IO_UNITS

INTEGER iwmin, nokw, nwfd

CHARACTER(1) iwehr
CHARACTER(2) wart

REAL xokw (maxkla), hokw (maxkla)
REAL xtrw (maxw), htrw (maxw)

INTEGER ianfw (maxw), iendw (maxw)

COMMON / wehr / xokw, hokw, nokw, iwmin, hokwmin, iwehr, xtrw,    &
              & htrw, nwfd, iendw, ianfw

REAL xl (maxkla), hl (maxkla)

!     common-bloecke boolsche operationen --> alg1
!     ================================================
COMMON / angabe / xko (mpts, 2), hko (mpts, 2), na1, na2
!     ================================================
COMMON / xr0yr0 / xr0 (mpts, max2), hr0 (mpts, max2), mr0 (max2), kr0
COMMON / xr1yr1 / xr1 (mpts, max2), hr1 (mpts, max2), mr1 (max2), kr1
COMMON / xr2yr2 / xr2 (mpts, max2), hr2 (mpts, max2), mr2 (max2), kr2
!     ========================================================


! common-block Uebergabegroessen der Geometrieberechnung:

REAL auew (maxw), huew (maxw), wl (maxw), hwmin (maxw)                  

! ------------------------------------------------------------------
! PROGRAMMBEGINN
! ------------------------------------------------------------------
                                                                        
!**   SCHREIBEN IN KONTROLLFILE                                         
write (UNIT_OUT_LOG, '(//,'' hr='',f8.4)') hr

ifehl1 = 0

igraf = 0

gen = 0.0001

hmin = hokwmin - 5.

!**   SCHREIBEN IN KONTROLLFILE
 write (UNIT_OUT_LOG, '(''   j  huew    auew     wl     hwmin  '')')

!JK   START DO-SCHLEIFE-------------------------------------------------
!JK   FUER ALLE WEHRFELDER?? (nwfd)
DO 1000 j = 1, nwfd

  auew (j) = 0.0
  huew (j) = 0.0
  wl (j) = 0.0
  hwmin (j) = 10000.

  !     ******************************************************************
  !       1. Linie      Wehroberkante
  !     ******************************************************************

  !**        ifehl2 = 0

  CALL linie (hmin, xokw, hokw, nokw, ianfw (j), iendw (j), npl1, xl, hl)

  npl1 = npl1 + 1

  hl (npl1) = hl (1)
  xl (npl1) = xl (1)

  DO j1 = 1, npl1

    xko (j1, 1) = xl (j1)
    hko (j1, 1) = hl (j1)

  END DO

  na1 = npl1


  !     ******************************************************************
  !       2. Linie      Wasserspiegel
  !     ******************************************************************


  npl2 = 5

  n_anf = ianfw (j)
  n_end = iendw (j)


  xko (1, 2) = xokw (n_anf)
  hko (1, 2) = hr


  xko (2, 2) = xokw (n_end)
  hko (2, 2) = hr


  xko (3, 2) = xokw (n_end)
  hko (3, 2) = hmin


  xko (4, 2) = xokw (n_anf)
  hko (4, 2) = hmin

  xko (5, 2) = xokw (n_anf)
  hko (5, 2) = hr


  DO j1 = 1, npl1

    hl (j1) = hko (j1, 1)
    xl (j1) = xko (j1, 1)

  END DO

  CALL schwpkt (maxkla, npl1, xl, hl, xx, hh, ah, igra, gen)

  IF (ah.lt. - 0.001) then
    PRINT * , 'Negative Flaeche der Linie1 in der Wehrberechnung!'
    STOP
  ENDIF



  DO j1 = 1, npl2

    hl (j1) = hko (j1, 2)
    xl (j1) = xko (j1, 2)

  END DO

  CALL schwpkt (maxkla, npl2, xl, hl, xx, hh, ah, igra, gen)

  IF (ah.lt. - 0.001) then
    PRINT * , 'Negative Flaeche der Linie2 in der Wehrberechnung!'
    STOP
  ENDIF

  na1 = npl1
  na2 = npl2


  kr1 = 0
  kr2 = 0
  kr0 = 0

  CALL prf (ik1, ik2)
  IF (ik1.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 1 abfiltriert'')')
  IF (ik2.eq.1) write (UNIT_OUT_LOG, '(''pkte linie 2 abfiltriert'')')

  CALL alg1 (igraf, ifehl)

  IF (ifehl.eq.1) then
    !JK       SCHREIBEN IN KONTROLLFILE
    write (UNIT_OUT_LOG, '(''Polygonzuege im algebra nicht fehlerfrei abgearbeitet '')')
  ENDIF

  !JK                   ZU DATENUEBERGABE
  IF (kr2.eq.0) goto 1000

  nwf = kr2

  DO 40 i = 1, nwf


    npw = mr2 (i)

    DO i1 = i, npw

      xl (i1) = xr2 (i1, i)
      hl (i1) = hr2 (i1, i)

    END DO


    IF (npw.lt.3) goto 40

    CALL schwpkt (maxkla, npw, xl, hl, xss, ht, at, igra, gen)

    IF (at.lt. - 0.01) then
      PRINT * , 'flaeche at < 0 ! '
      PRINT * , 'stop in wehr_ber !'
      STOP
    ENDIF

    auew (j) = auew (j) + at
    !****************************************************
    !  Wehrlaenge:
    !****************************************************

    xmin = 10000.
    xmax = - 10000.

    DO i2 = 1, npw

      IF (xl (i2) .lt.xmin) xmin = xl (i2)
      IF (xl (i2) .gt.xmax) xmax = xl (i2)
      IF (hl (i2) .lt.hwmin (j) ) hwmin (j) = hl (i2)

    END DO

    wl (j) = wl (j) + xmax - xmin

  40 END DO

  IF (wl (j) .gt.0.0001) then
    huew (j) = auew (j) / wl (j)
  ELSE
    huew (j) = 0.0
  ENDIF

  write (UNIT_OUT_LOG, '(i4,4f8.4)') j, huew (j) , auew (j), wl (j) , hwmin (j)

  !JK   ENDE DO-SCHLEIFE ------------------------------------------
1000 END DO

END SUBROUTINE g_wehr                                                                         
