!     Last change:  JH   26 Jul 2006    2:45 pm

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
      subroutine gwsp_st(dt,itime,qkap1,qkap2,                   &
               perk1,perk2,qgwzu1,qgwzu2,                         &
               qb1,qgwab1,qent1,gws1,                             &
               qb2,qgwab2,qent2,gws2)
!c
!c	subroutine zum grundwasserspeicher
!c			(zeitschrittreduziert)
!c
!c	eingabe	dt	real	zeitschrittlaenge
!c		itime	int	zeitschritt
!c		flaech	real	flaeche teilgebiet
!c
!c		perk1	[mm]	perkolation bodensp,t=t-1
!c		perk2	[mm]	perkolation bodensp,t=t
!c		qkap1	[mm]	kapilarer aufstieg in bodensp,t=t-1
!c		qkap2	[mm]	kapilarer aufstieg in bodensp,t=t
!c		qgwzu1	[mm]	grundw.zufl.aus tg oberhalb, t=t-1
!c		qgwzu2	[mm]	grundw.zufl.aus tg oberhalb, t=t
!c		qent1	[mm]	grundw.entnahme t=t-1
!c		qent2	[mm]	grundw.entnahme t=t
!c		qb1	[mm]	basisabfluss,   t=t-1
!c		qgwab1	[mm]	grundw.abfluss zu tg unterhalb, t=t-1
!c		gws1	[m]	grundw.stand, t=t-1
!c	ausgabe (berechnet)
!c		gws2	[m]	grundw.stand, t=t
!c		qb2	[mm]	basisabfluss	t=t
!c		qent2	[mm]	grundw.entnahme t=t
!c				(korrektur, falls speicher leer)
!c		qgwab2	[mm]	grundwasserabfluss,	t=1
!c
!c		nres	int	flag fuer kontrollausgabe
!c
!c	common	aigwm	real	aktueller inhalt grundw.speicher
!c		qbm	real	basisabfluss
!c		qgwm	real	grundwasserabflus
!c
!c	common/gwls
!c	integer	igwzu, ngwzu(igwdim)
!c	real	retbas,retgw,gwwi(igwdim),aigw,hgru,hgro,pors

!cccccccccccccccccccccccccccc
!c
!c	fehler mit pors, noch korregieren
!c
        USE generic_LOGGER
        USE Units
	implicit logical(a-z)
	include 'include\param.cmn'
	include 'include\gwl.cmn'

	integer	itime,iter

	real 	dgws,dh,dt,                                 &
               dgwsn,hdif,qkap1,qkap2,                          &
               qgwzu1,qgwzu2,xdif,gws1,gws2,perk1,perk2,   &
               qb1,qgwab1,qent1,qb2,qgwab2,qent2,c_mm

        real    d_vol, hvol_0, hvol_max,hvol_neu,gwsneu



!c   variableninitialisierung

	iter = 0
	dgws  = 0.
	dh    = 0.
	dgwsn = 0.
	hdif  = hgro - hgru
        c_mm = 1000.
!ccccccccccccccccc
!c
!c	zufuhr in grundwasserspeicher muss noch gesetzt werden1

!cccccccccccccccccccccccccccccccccc
!c
!c  gesonderte berechnung fuer ersten seitschritt
!c  nicht genutzt, da t0=1
!c
	if(itime.eq.0) then
		gws2=gws1
		if (gws2 .lt.0) gws2 = 0.
		dh  = gws2 - hgru
		if(gws2.le.hgru) then
			qb2 = 0.
			qgwab2 =retgw*c_mm * gws2 * pors * dt
		else if(hgru.lt.gws2.and.gws2.lt.hgro) then
			qb2 = retbas *c_mm* dh*0.5*dh/ hdif * pors * dt
			qgwab2 = retgw*c_mm *(gws2-0.5*dh*dh/hdif)*pors*dt
		else if(gws2.ge.hgro) then
			qb2 = retbas*c_mm * (0.5*hdif) * pors * dt
			qgwab2 = retgw*c_mm * (hgru+hgro)*pors *0.5 *  dt
		end if
		goto 2999
	endif


	if (gws1 .lt.0) gws1 = 0.

	if(gws1.le.hgru) then
		hvol_0 = gws1
	else if(hgru.lt.gws1.and.gws1.lt.hgro) then
                dh = gws1-hgru
                hvol_0 = (gws1-0.5*dh*dh/hdif)
	else if(gws1.ge.hgro) then
		hvol_0 = (hgru+hgro)*0.5
	end if

        hvol_max = 0.5*(hgru+hgro)


!c  schaetzwert fuer die aenderung des grundwasserstandes in [mm]

	if (perk2.le.0..and.qgwzu2.le.0.) then
		dgws = -1
	else
		dgws = 10 
	endif

!c  verlust in richtung tiefliegender grundwasserleiter
!c  qtief in [mm]

!c	tgwzu2 = rtr *(qgwzu2+perk2)

3030	gws2 = gws1 + dgws/1000.
	if (gws2 .lt.0) gws2 = 0.
        if (gws2.gt.hgro) then
                  gws2 = hgro
                  dgws = (hgro-gws1)*c_mm
        endif




!c  zwischenfaktoren

	dh  = gws2 - hgru

!c  lage des grundwasserstandes im verhaeltnis zu den
!c  gerinnehoehen  ( 3 faelle )


          if(gws2.lt.0.01) then
                     qb2 = 0.
                     qgwab2 = 0.
          		qent2=0.
           else if(gws2.lt.hgru) then
                     qb2 = 0.
                     qgwab2 = retgw * gws2 * c_mm * pors * dt
          else if(gws2.lt.hgro) then
                     qb2 = retbas*dh*c_mm*0.5*dh/hdif*pors*dt
                     qgwab2 = retgw *c_mm*(gws2-0.5*dh*dh/hdif)*pors*dt
          else
!c                     qb2 = retbas*c_mm*0.5*hdif*pors*dt
                     qgwab2 = retgw *c_mm*0.5*(hgru+hgro)*pors*dt
                     IF(itime.eq.1)then
                        qb2 =0.5*( perk1 +perk2 +qgwzu1 + qgwzu2 -qkap1-qkap2     &
                           -qent1-qent2-2.*qgwab2)
                     else
                     qb2 = perk1 +perk2 +qgwzu1 + qgwzu2 -qb1 -qkap1-qkap2     &
                           -qent1-qent2-qgwab1-qgwab2
                     endif
                     d_vol = hvol_max-hvol_0
          end if

!ep 30.10.03
        if (qb2.lt. 0) qb2=0
        if (qgwab2.lt. 0)qgwab2 = 0



        if (qb2.lt. 0.) then
                       WRITE(nres,160) qb2
                       qb2 = 0.
                       dgws = -10.
                       GOTO 3030
                     end if

	if(itime.eq.1) then
			qb1=qb2
			qgwab1=qgwab2
	endif

!cc
!c geaendert 19.11.99, R.S
!c	   dgwsn = 0.5 * (1./pors) *
!cpa	   dgwsn = 0.5 *
!c Die zu- und abfließenden Wasserströme des Grundwasserleiters werden bilanziert. Die Differenz wird in ein Volumen
!c umgerechnet. Da die Zu- und Abflüsse als spez. Größe mm/km² Einzugsgebietsflaeche ausgedrückt sind. Wird auch die Differenz
! als spez. Volumen berechnet. Um den Abgleich mit dem tatsaechlichen Grundwasserstand vornehmen zu können, muß die
! Differenz noch um durch die Porositaet dividiert werden, da dieses Differenzvolumen eine Nettogröße darstellt, in der
! die Bodenmatrix noch nicht enthalten ist.

        IF((gws2-hgro).LT.-0.00001)then
	   d_vol = 0.5 * (1./pors) *                                  &
                       ((perk1+perk2)+(qgwzu1+qgwzu2)            &
                        -(qb1+qb2)-(qkap1+qkap2)                 &
                        -(qent1+qent2)                           &
                        -(qgwab1+qgwab2))
           d_vol = d_vol / 1000.
        endif

!c Umrechnung von mm in m



           hvol_neu = hvol_0+d_vol

           IF(hvol_neu.lt.hgru)then
                gwsneu=hvol_neu
           ELSEIF(hvol_neu.lt.hvol_max)then
                gwsneu = -SQRT(hgru*(2.*hvol_neu-hgru)+hgro*(hgro-2*hvol_neu))
                gwsneu = gwsneu+hgro
            else
                gwsneu = hgro
                d_vol = hvol_max-hvol_0
           endif
           dgwsn = (gwsneu-gws1)*c_mm

         if (iter.ge.100) then

             write(nres,101)iter,itime,dgwsn,dgws

             goto 2999
         endif

!c aenderung, 21. 2.94, fehler: division durch null
         if(dgws.eq.0.) then
             xdif=abs(dgwsn-dgws)
         else
             xdif=abs((dgwsn-dgws)/dgws)
         endif

         if(dgwsn.eq.0.and.abs(dgws).lt.10.e-4) then
             goto 2999
         elseif(xdif.gt.0.0001) then
                              iter = iter+1 
                              dgws = dgwsn
                              goto 3030 
         end if

2999   continue  

!c	print*,'iter: ', iter, dgws
     	return



101   format(/' warnung!!!'/,' konvergenzkriterium im grundwasser',   &
          ' speicher noch nicht erreicht.'/,                &
          'Iterationsanzahl= ',i4,' zeitschritt:',i4/       &
          ' deltagws1:',f15.12,'  deltagws2:',f15.12)

160   FORMAT('qb2 = ',f9.4)
   
            end


