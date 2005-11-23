!     Last change:  WP   25 Aug 2005    3:34 pm
!--------------------------------------------------------------------------
! This code, SaWSP.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - schnpkt
! - schwpkt
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
! ------------------------------------------------------------------------


! ------------------------------------------------------------------------
SUBROUTINE schnpkt (x1, y1, x2, y2, x3, y3, x4, y4, sx, sy, inside, ikenn)
!
! Programmbeschreibung:
! ---------------------
! Das Programm hat je nach Uebergabe von IKENN folgende Moeglich-
! keiten zur Ermittlung von Schnittpunkten zwischen zwei Linien,
! die durch die Punkte (x1,y1) und (x2,y2), bzw. (x3,y3) und
! (x4,y4) definiert sind.
!                                                                       
! ikenn=1:  Schnittpunkt zwischen zwei Strecken (d.h. in der Laenge
!           begrenzten Geradenstuecken)
! ikenn=2:  Schnittpunkt zwischen einer Strecke und einer Gerade
! ikenn=3:  Schnittpunkt zwischen zwei Geraden

implicit none

! Calling variables
REAL, INTENT(IN)  	:: x1, y1, x2, y2, x3, y3, x4, y4
REAL, INTENT(OUT) 	:: sx, sy
INTEGER, INTENT(OUT) 	:: inside
INTEGER, INTENT(IN)  	:: ikenn

! Local variables
REAL :: aoben, boben, unten, ak, bk


! Berechnungen
inside = 0

aoben = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
boben = (y3 - y1) * (x4 - x3) - (x3 - x1) * (y4 - y3)
unten = (x4 - x3) * (y2 - y1) - (y4 - y3) * (x2 - x1)

IF (abs (unten) .le.1.e-03) then
  RETURN
ENDIF

ak = aoben / unten
bk = boben / unten

IF (ikenn.eq.1) then

  IF (ak.ge.0. .and. bk.ge.0. .and. ak.le.1. .and. bk.le.1.) then
    inside = 1
    sx = (x3 + ak * (x4 - x3) + x1 + bk * (x2 - x1) ) / 2.
    sy = (y3 + ak * (y4 - y3) + y1 + bk * (y2 - y1) ) / 2.
  ENDIF
  RETURN
ENDIF

IF (ikenn.eq.2) then
  IF (bk.ge.0. .and. bk.le.1.) then
    inside = 1
    sx = (x3 + ak * (x4 - x3) + x1 + bk * (x2 - x1) ) / 2.
    sy = (y3 + ak * (y4 - y3) + y1 + bk * (y2 - y1) ) / 2.
  ENDIF
  RETURN
ENDIF

IF (ikenn.eq.3) then
  inside = 1
  sx = (x3 + ak * (x4 - x3) + x1 + bk * (x2 - x1) ) / 2.
  sy = (y3 + ak * (y4 - y3) + y1 + bk * (y2 - y1) ) / 2.
  RETURN
ENDIF                                                        

END SUBROUTINE schnpkt
                                                                        



!***********************************************************************
SUBROUTINE schwpkt (max, n, xu, yu, xs, ys, f, igra, gen)
                                                                        
! das programm findet den schwerpunkt eines Flaechenbereiches
                                                                        
! max 		: input, maximallaenge von vektoren x,y
! n   		: input, anzahl der grenzpunkte
! xu,yu 	: input, koordinaten der grenzpunkte
! xs,ys 	: output, schwerpunktkoordinaten
! f     	: output, flaecheninhalt des gebietes
! igra=1 	: mit grafik
! igra=0 	: ohne grafik
                                                                        
DIMENSION x (200), y (200)
                                                                        
DIMENSION xu (max), yu (max)
                                                                        

DO i = 1, n
  x (i) = xu (i)
  y (i) = yu (i)
END DO
                                                                        
ivz = + 1
                                                                        
CALL t0f (x, y, xs, ys, xmax, xmin, ymax, ymin, n, ivz)

iflag = 0 
IF ( (x(1) - x(n) )**2 + (y(1) - y(n) )**2 .le. gen) then
  n1 = n - 1
ELSE
  n1 = n
ENDIF

CALL flae_inh (max, n1, x, y, f) 
                                                                        
IF (abs (f) .lt.gen) then
  ! Flaeche ist null.
  xs = 0.
  ys = 0.
  RETURN
ENDIF

IF (f.lt.0.) then
  iflag = 1
  CALL reih_inv (max, n1, x, y)
  f = - f
ENDIF
                                                                        
CALL integrat (max, n1, x, y, fx, - 1)
CALL integrat (max, n1, y, x, fy, + 1)

xs = fx / f
ys = fy / f

CALL t0f (x, y, xs, ys, xmax, xmin, ymax, ymin, n1, - 1)
! if(igra.eq.1) call grafik(max,n1,x,y,xs,ys)

IF (iflag.eq.1) call reih_inv (max, n1, x, y)

f = f * (xmax - xmin) * (ymax - ymin) 

END SUBROUTINE schwpkt
                                                                        
                                                                        

