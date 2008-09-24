!     Last change:  SK   16 Apr 2007   11:35 am

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
   subroutine vg_cond(kcita,wcs,wcr,alpha,kff,n,h1,h,l,bof,bmax,wp,nn,ilay,t,kh)



  !**************************************ÄNDERUNGEN***********************************************


!     Date      Programmer      	Description of change
!     ====      ==========      	=====================
!  23.08.2005   Karolina-Villagra       Van Genuchten parameters:n,alpha. Estimation of unsaturated
!                                       hydraulic conductivity


!***************************************EIN-/AUSGABE********************************************
!c    Eingabe:
!c              kff     saturated hydraulic conductivity (mm/d)
!               n       van Genuchten parameter (-)
!		alpha 	van Genuchten parameter (1/cm)
!               l       pore size distribution parameter  (-)
!               h       soil suction    (cm)
!               wcr     residual soil moisture(mm)
!               wcs     soil moisture at saturation (mm)
!

!c    Ausgabe:
!c
!c     		K(h)    unsaturated hydraulic conductivity     (mm/d)
!
!c
!******************************************VARIABLEN********************************************

      USE generic_LOGGER
      USE Units      
      IMPLICIT NONE
      include	'include\param.cmn'
      include      'include\is.cmn'

      real	n,alpha,kff,wcs,wcr,l,bof,bmax,wp
      integer:: nn,ilay,t
      !local

      REAL::Se,m,kr

      REAL:: h1,h(2*idimnutz2,maxlay,idim)
      REAL:: kh1,kh(2*idimnutz2,maxlay,idim)
      REAL,INTENT(OUT):: kcita

     kcita=0.
     kh1=0.
     kh(nn,ilay,t)=0.
      !h=ABS(h)
  !Calculation of Kh1

      if (ilay .eq. 1) then
       kh1=kff
      else
       kh1=kh(nn,ilay-1,t)
      end if
 !Calculation of kh as function of h

 !     if (h(nn,ilay,t) .gt. 0.) then
 !     	a=alpha*h(nn,ilay,t)
 !     	b=(1.-(a**(n-1.))*(1.+(a**n))**((1.-n)/n))**2.
 !     	c= (1.+(a**n))**(((n-1.)/n)*l)
 !     	kh(nn,ilay,t)=kff*(b/c)
 !     else
 !     	kh(nn,ilay,t)=kff
 !     end if


 !    kcita=(kh1*kh(nn,ilay,t))**(0.5)


 !Calculation of kh as a function of soil moisture


 if (bof .gt. wcr) then
     Se=(bof-wcr)/(wcs-wcr)
 else
     Se=(bof)/(wcs-wcr)
 end if

 m=1.-(1./n)

 kr=(Se**l)*(1.-(1.-(Se**(1./m)))**m)**2.

 if (bof .lt. wcr) then

  call writeLogIntRealReal( 7,'Fehler in der Berechnung ! Berechnung Negativ!', &
              & 'Error calculating the soil tension, Negative denominator ','','',0,'bof',bof,&
              &'residual water content',wcr,'infmm')
   call writeFooter()
   CLOSE(nerr)
   stop


 else

  kh(nn,ilay,t)=kff*kr

  end if

  kcita=(kh1*kh(nn,ilay,t))**(0.5)

 !WRITE(nres,*)'kr',kr
 !WRITE(nres,*)'n',n
 !WRITE(nres,*)'m',m
 !WRITE(nres,*)'Se',Se
 !WRITE(nres,*)kh
    end
