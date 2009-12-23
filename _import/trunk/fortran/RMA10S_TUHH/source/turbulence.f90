!     Last change:  MD   20 May 2009    4:27 pm
!----------------------------------------------------------------------------
! This code, turbulence.f90, computes the turbulence parameter nu_T for all
! finite elements depending on chosen turbulence model as specified by
! input-variable ITURB in the library 'Kalypso-2D'.
! Copyright (C) 2004  KAJ LIPPERT & WOLF PLOEGER.
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
! Sebastian Rath:   phone: +49 40 42878 4180 mail: s.rath@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-2D'.
!
! Sebastian Rath, 09 August 2004
! Research Associate
!
!
SUBROUTINE turbulence(nn, iturb, mineddy, &
                & eexxyy1, eexxyy2, eexxyy3, eexxyy4,    &
                    & epsxx,   epsxz,   epszx,   epszz,           &
                    & roavg,p_bottom,p_prandtl,ffact,vecq,h, &
                    & drdx, drdz, dsdx, dsdz, gscal)
!
!
!---------------------------------------------------------------------
! Some additional information of relevance:
!---------------------------------------------------------------------
!
! Calculates turbulence parameter nu_T for all elements
! depending on input-variable ITURB
!
! ITURB = 10   : Constant eddy viscosity
! ITURB = 11   : Bottom-generated turbulence model
! ITURB = 12   : PRANTDL-mixing length
! ITURB = 13   : PRANDTL-mixing length with SMAGORINSKY model
! ITURB = 14   : Combination of 13 and 11
! ITURB = 15   : Combination of 12 and 11
! ITURB = 16   : Combination of 11 and 12 and 13
!
!---------------------------------------------------------------------

implicit none

! Calling variables
INTEGER, INTENT(IN) ::                nn, iturb
REAL, INTENT(IN)    ::                     mineddy
REAL, INTENT(IN)    ::                     eexxyy1,eexxyy2,eexxyy3,eexxyy4
REAL, INTENT(IN)    ::                     roavg, p_bottom, p_prandtl, ffact, vecq
REAL (KIND = 8), INTENT(IN) ::          h
REAL, INTENT(IN) ::                     drdx, drdz, dsdx, dsdz
REAL (kind = 8), INTENT(IN) ::          gscal
REAL, INTENT(OUT) ::                    epsxx, epsxz, epszx, epszz

! Local variables
REAL ::                                 col, cot, layerwidth, cs= 0.0
REAL ::                                 mined = 0.0
REAL ::                                 mixl = 0.0
REAL ::                                 BOTTOM_GENERATED, PRANDTL_MIXING
REAL ::                                 temp_prandtl, temp_smagorinsky, mean_value


if (iturb == 10) then

  epsxx = eexxyy1 / roavg
  epsxz = eexxyy2 / roavg
  epszx = eexxyy3 / roavg
  epszz = eexxyy4 / roavg

else if (iturb == 11) then

  col = p_bottom
  cot = p_bottom
! col - dispersion coefficient in flow direction (longitudinal);  
! cot - dispersion coefficient in cross-flow dir. (transversal).  
  epsxx  =  BOTTOM_GENERATED(col, ffact, vecq, h)
  epszx  =  epsxx
  epsxz  =  BOTTOM_GENERATED(cot, ffact, vecq, h)
  epszz  =  epsxz
  


ELSE IF(iturb == 12 .OR. iturb == 13) then

  IF(iturb == 12) then
    layerwidth = p_prandtl
    mixl = 0.090 * layerwidth
  END  if
  IF(iturb == 13) then
    cs   = p_prandtl
    mixl = gscal * cs
  END if

! cs - parameter of the Smagorinsky model,  
!      having one of the following values:  
! -----------------------------------  
!           Author         |    cs  
! -----------------------------------  
!        Smagorinsky       |  0.197  
!       Lilly (1965)       |  0.230  
!       Lilly (1967)       |  0.170  
!     Deardorff (1970)     |  0.100  
!     Deardorff (1971)     |  0.130  
!   Mansour et al. (1979)  |  0.210  
!  Manson & Callen (1986)  |  0.200  
!  Piomelli et al. (1987)  |  0.065  
! -----------------------------------       

  epsxx  = PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl)
  epszx  = epsxx
  epsxz  = epsxx
  epszz  = epsxx
  


ELSE IF(iturb == 14) then

  col = p_bottom
  cot = p_bottom
  cs   = p_prandtl
  mixl = gscal * cs
  epsxx  = (BOTTOM_GENERATED(col, ffact, vecq, h) + PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl))
  epszx  = epsxx
  epsxz  = (BOTTOM_GENERATED(cot, ffact, vecq, h) + PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl))
  epszz  = epsxx

ELSE IF(iturb == 15) then

  col = p_bottom
  cot = p_bottom
  layerwidth = p_prandtl
  mixl = 0.090 * layerwidth
  epsxx  = (BOTTOM_GENERATED(col, ffact, vecq, h) + PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl))
  epszx  = epsxx
  epsxz  = (BOTTOM_GENERATED(cot, ffact, vecq, h) + PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl))
  epszz  = epsxz


ELSE IF(iturb == 16) then

  col = p_bottom
  cot = p_bottom

! Prandtl-mixing length  
  layerwidth = 23.0
  mixl = 0.090 * layerwidth
  temp_prandtl     = PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl)

  mixl = gscal * 0.197
  temp_smagorinsky = PRANDTL_MIXING(drdx, dsdz, drdz, dsdx, mixl)

  mean_value = (temp_prandtl + temp_smagorinsky) / 2

  epsxx  = (BOTTOM_GENERATED(col, ffact, vecq, h) + mean_value)
  epszx  = epsxx
  epsxz  = (BOTTOM_GENERATED(cot, ffact, vecq, h) + mean_value)
  epszz  = epsxx

else

  write (*,*) ' ITURB > 16 not supported!'
  stop

end if


mined=mineddy

!******************************************************************
!     DK, 26.02.02.
!      if (nn == intel) then
!         WRITE (iout,*) 'BEFORE COMPARISON TO MINEDDY:'
!         write (iout,*) '  eexxyy = ',eexxyy(1,nn)
!         write (iout,*) '  epsxx  = ',epsxx
!         write (iout,*) '  mined  = ',mined
!         write (iout,*)
!      end if
!     ******************************************************************

if ((epsxx < mined) .AND. (epszx < mined) .AND. (epsxz < mined) .AND. (epszz < mined)) then
  epsxx  = mined
  epszx  = mined
  epsxz  = mined
  epszz  = mined

!EFa may07, testing
!else
!  WRITE(*,*)'wirklich berechnet :',epsxx
end if
!-

END subroutine turbulence







!WP-----------------------------------------------------------------------------------------------------------------------
real function bottom_generated(c, ffact, vecq, h)

implicit none

REAL, INTENT(IN) ::      ffact, vecq, c
REAL (KIND = 8), INTENT(IN) :: h

BOTTOM_GENERATED = c * sqrt(ffact) * vecq * h

end function bottom_generated




!WP-----------------------------------------------------------------------------------------------------------------------
REAL function prandtl_mixing(drdx, dsdz, drdz, dsdx, mixl)

implicit none

REAL, INTENT(IN) ::     drdx, drdz, dsdx, dsdz, mixl
REAL ::                 brackets = 0.0


brackets = (2.*(drdx**2))+(2.*(dsdz**2))+((drdz+dsdx)**2)
PRANDTL_MIXING = (mixl**2)*SQRT(brackets)

end function prandtl_mixing



