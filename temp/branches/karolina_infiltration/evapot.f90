!     Last change:  CK   20 Apr 2006    7:35 am
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
! Dipl.-Ing. Jessica Huebsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Huebsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************
subroutine evapot(dtn,anzlay,nb,nv,nn,eptt,m_tief,m_bmax,bof,wt_b,wtlay,bofsum,    &
		   & wtsum,evpotlay)

IMPLICIT NONE
include      'include\param.cmn'

INTEGER, INTENT(IN) :: anzlay,nv,nb,nn
REAL, INTENT(OUT) :: bofsum,wtlay(0:maxlay),evpotlay(maxlay)
REAL, INTENT(IN) :: eptt,dtn
REAL, INTENT(IN) :: m_tief(maxlay,idimnutz2),bof(2*idimnutz,maxlay),m_bmax(maxlay,idimnutz2), &
                    wt_b(idimnutz2,idimnutz2)
REAL :: xbodt,wtsum,bofant(maxlay),xs
INTEGER :: ilay,aktlay
xbodt=0.
wtsum=0.
bofsum=0.
xs=0.


DO ilay=1,anzlay
   xbodt=xbodt+m_tief(ilay,nb)*0.1
   wtlay(ilay)=m_tief(ilay,nb)*0.1
   bofant(ilay)=bof(nn,ilay)/m_bmax(ilay,nb)
   IF ((xbodt+0.001) > wt_b(nv,nb)) THEN
      wtlay(ilay)=wt_b(nv,nb)-wtsum
      wtsum=wtsum+wtlay(ilay)
      bofsum=bofsum+bofant(ilay)
      bofant(ilay)=bof(nn,ilay)/m_bmax(ilay,nb)
      goto 2997
   END IF
   wtsum=wtsum+wtlay(ilay)
   bofsum=bofsum+bofant(ilay)
END DO
ilay=ilay-1

2997	continue

aktlay=ilay
bofsum=bofsum/aktlay
xs=0.
DO ilay=1,anzlay
   IF (ilay <= aktlay) THEN
      evpotlay(ilay) = eptt*dtn*wtlay(ilay)/wtsum*bofant(ilay)/bofsum
      xs=xs+evpotlay(ilay)
   ELSE
      evpotlay(ilay)=0.
   END IF
END DO

IF (ABS(xs-eptt*dtn) > 0.00001) THEN
   bofsum=eptt*dtn/xs
   xs=0.
   DO ilay=1,anzlay
      evpotlay(ilay)=evpotlay(ilay)*bofsum
      xs=xs+evpotlay(ilay)
   END DO
END IF

IF (ABS(xs-eptt*dtn) > 0.00001) THEN
   PRINT*,' fehler transpiration xs,eptt*dtn',xs,eptt*dtn
END IF

END
