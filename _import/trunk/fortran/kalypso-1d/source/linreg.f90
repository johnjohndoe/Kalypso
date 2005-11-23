!     Last change:  WP   25 May 2005   11:33 am
!--------------------------------------------------------------------------
! This code, linreg.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - linreg
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


SUBROUTINE linreg (zln, wr, nt, bbyx, zlnqur, wquer, xwsp, ywsp)
!
! LINREG fuehrt eine lineare regression durch
!                                                                       
! ------------------------------------------------------------------

REAL(8) sxsum, sysum, sxysum, sxy1, sxye, zlnsum, wsum
DIMENSION zln ( * ), wr ( * )


zlnsum = 0.
wsum = 0.d0
itmin = 1
itmax = nt

!     normierung der zln und wr-werte

!     finden des maximalen wertes

zlmax = 0.00
DO i = 1, nt
  IF (zln (i) .gt.zlmax) zlmax = zln (i)
  IF (wr (i) .lt.zlmax) zlmax = wr (i)
END DO

!     normierung ueber zlmax

DO i = 1, nt
  zln (i) = zln (i) / zlmax
  wr (i) = wr (i) / zlmax
END DO

zlnsum = 0.d0
sxsum = 0.d0
sysum = 0.d0
sxysum = 0.d0

15 CONTINUE

DO i = itmin, itmax
  zlnsum = zlnsum + dble (zln (i) )
  wsum = wsum + dble (wr (i) )
END DO

n = itmax - itmin + 1
tn = float (n)
zlnqur = zlnsum / dble (tn)
wquer = wsum / dble (tn)

DO i = itmin, itmax
  sx1 = (zln (i) - zlnqur) * (zln (i) - zlnqur)
  sxsum = dble (sx1) + sxsum
  sy1 = (wr (i) - wquer) * (wr (i) - wquer)
  sysum = dble (sy1) + sysum
  sxy1 = dble (zln (i) ) * dble (wr (i) )
  sxysum = sxy1 + sxysum
END DO

sxe = sngl (dsqrt (1.d0 / (dble (tn) - 1.d0) * sxsum) )
sye = sngl (dsqrt (1. / (dble (tn) - 1.d0) * sysum) )
sxye = 1.d0 / (dble (tn) - 1.d0) * (sxysum - dble (tn) * dble (zlnqur) * dble (wquer) )

DO i = 1, nt
  zln (i) = zln (i) * zlmax
  wr (i) = wr (i) * zlmax
END DO

regk = sxye / (sxe * sye)
bbyx = sxye / (sxe * sxe)
wquer = wquer * zlmax
zlnqur = zlnqur * zlmax
IF (abs (regk) .gt.0.10) return

IF (ywsp.eq.0) THEN
  ywsp = wquer
  xwsp = zlnqur
  sa = 0.0
  tnn = tn
END IF

DO 40 i = itmin, itmax
  dy = wr (i) - ywsp
  dx = zln (i) - xwsp
  IF (dx.ne.0) goto 43
  tnn = tnn - 1
  GOTO 40
  43 CONTINUE
  a = dy / dx
  sa = sa + a
40 END DO

bbyx = sa / tnn
zlnqur = xwsp
wquer = ywsp

END SUBROUTINE linreg                                                                    
