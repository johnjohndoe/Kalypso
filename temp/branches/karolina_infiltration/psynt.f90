!     Last change:  CK   11 Mar 2006    8:09 am

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

!c      WK: nur Umbenennung!
      subroutine psynt(dt,idif,peff,faktn)
!c
!c
!c ***************************************************************************
!c      das programm psynt berechnet eine niederschlagsreihe aus einem synthe-
!c      tischen niederschlagsereignis
!c
!c     gegebenenfalls koennen n.werte um konstanten faktor veraendert werden
!c
!c ***************************************************************************
        implicit logical(a-z)
!c	parameter(igwdim=6,ndmax=20,nvmax=4)
       !parameter(ndmax=20,nvmax=4)
       INTEGER, PARAMETER :: nvmax=4
      include 'include\param.cmn'


      integer ipver,nst,l,l1,n,ipkk,imax1,idif

	integer	ntrans

      real psum,x(idim),y(idim),peff(*),dsum,dtp,sup,supk,     &
          pjsol,pdsol,tproz(nvmax,nvmax),      &
          pproz(nvmax,nvmax),pkk,xmax,dsupk,dt,pp,faktn,       &
               pvsol


      common /syndat/ pjsol,pdsol,pvsol,ipver

      data tproz/0,30,50,100,                                       &
                0,16.67,33.34,100,                                  &
                0,30.00,50.00,100,                                  &
                0,66.67,83.33,100/
      data pproz/0,30,50,100,                                      &
                0,50,70,100,                                       &
                0,20,70,100,                                       &
                0,30,50,100/

	psum=pvsol
!ccccccccc
!c
!cc ntrans=verschiebefaktor fuer uebernahme niederschlag,
!c   anzahl zeitschritte, positiv
!c	ntrans=66
      ntrans=0
imax1= 0
      call u1null(x,idim)
      call u1null(y,idim)

!ccccccccccc
!c
!c korrektur des niederschlags um konst. faktor
!c
            if(faktn.lt.-0.01) then
		ntrans=int(-faktn/dt)
		faktn=1.
	    endif
            if(faktn.lt.0.99.or.faktn.gt.1.01) then 
                 print*,' faktor zur korrektur des niederschlags: ', faktn
                 print*,' eingelesene n.menge: ', psum
                      psum = psum * faktn
                 print*,' korregierte n.menge: ', psum
            endif

!c
      nst=int(pdsol/dt +0.5)
      dsum =nst*dt
      dtp=100.0/float(nst)
!c
      l=1
      l1=2
!c
      sup = 0.
      supk =0.
      do 500 n=1,nst
      x(n)=dtp*float(n)
      if (x(n) .lt. tproz(l1,ipver)) go to 501
      if (l1 .eq. nvmax) go to 501
      l=l1
      l1=l+1
501   y(n)=((pproz(l1,ipver)-pproz(l,ipver))*(x(n)-tproz(l,ipver))/   &
            (tproz(l1,ipver)-tproz(l,ipver))                          &
             +pproz(l,ipver))*psum*0.01
      x(n)=x(n)*dsum*0.01
!c
      if(n.le.1)then
            peff(n+ntrans)=y(n)/dt
            pp = y(n)
      else
            pp = y(n)-y(n-1)
            peff(n+ntrans)=pp/dt
      endif
      sup = sup+pp
      ipkk=(pp*1000.)+0.5
      pkk=(float(ipkk))/1000.
      supk=supk+pkk
 500  continue
!c
      if(abs(sup-psum).gt.1.e-04) then
            xmax=0.0
            do 600 n=1,nst
                 ipkk=(peff(n+ntrans)*dt*1000.)+0.5
                 peff(n+ntrans)=(float(ipkk))/1000./dt
                 if(xmax .lt. peff(n+ntrans)) then
                      imax1=n+ntrans
                      xmax=peff(n+ntrans)
                 endif
 600         continue
             dsupk=psum-supk
             peff(imax1)=peff(imax1)+dsupk/dt
      endif
      idif=max(idif,nst+ntrans)
      idif=min(idif,idim)
      return
      end

