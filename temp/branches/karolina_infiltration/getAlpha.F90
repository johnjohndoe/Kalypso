!     Last change:  JH   17 Jan 2007    8:21 am

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
subroutine getAlpha(area,d,alpha,maxN,tol)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      Diese Funktion berechnet den Zentriwinkel eines teilgefüllten
!c      Rohres mit der durchströmten Flaeche A nach dem Newton-Verfahren.
!c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c      Name                Beschreibung
!c      ----                ------------
!c      area                Flaeche des durchströmten Rohrquerschnittes
!c      d                   Rohrdurchmesser
!c      alpha               berechneter Zentriwinkel (Ausgabe)
!c      tol                 Abruchkriterium für Newtron-Verfahren
!c      maxN	            max. Anzahl der Interationen
!c      n                   Defaultwert von maxN

REAL, INTENT(IN) :: tol,area
INTEGER, INTENT(IN) :: maxN
REAL :: p0,p,a,alpha
INTEGER :: i,n
REAL, PARAMETER :: pi=3.141592654
n=0
alpha = pi
p0 = alpha
a = area * 8 / d**2

IF (maxN == 0 ) n = 50

i = 1
DO
   p =  p0 - ( (SIN(p0)-p0 + a)/(COS(p0)-1) )
   !WRITE (*,*) 'alpha: ', alpha,' p = ',p,' A =',a
   IF ( i > n ) THEN
       alpha = 0
       WRITE (*,1000) alpha,i
       1000 FORMAT (/1X, 'Fehler bei der Berechnung des', &
       			  & 1X, 'Wasserstandes in der MR-Draenage:', / &
                          & 1X, 'alpha[rad]:     ', F5.2, /, &
                          & 1X, 'Iteration: ', I5, 5X,' PROGRAMM HALT')
!       call writeFooter()
       stop
   END IF
   IF (ABS(p-p0) < tol) THEN
        alpha = p
       RETURN
   END IF
   i = i + 1
   p0 = p
END DO

end
