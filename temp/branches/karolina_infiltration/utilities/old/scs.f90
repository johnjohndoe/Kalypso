!     Last change:  SK   24 Jul 2006    8:59 pm

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

      subroutine scs(p,ipdim,cn,dt,pmmax,nres,nerr)
!c
!c     berechnung der abflussbeiwerte nach scs-verfahren (modifiziert)
!c
!c     uebernommen aus scs.f77 - darmstadt
!c     stand: 8.9.1988  zior
!c     geaendert:   vers.  1.3    25.8.1989   zior    version koblenz
!c
!c     eingabe:  p(i)  i=1,ipdim    ausgangsniederschlag zum zeitintervall i
!c               ipdim integer      max. anzahl an zeitschritten
!c               dt    real         zeitintervall
!c               cn    real         cn-wert
!c               pmmax real         max. muldenverlust
!c               nres  integer      flag fuer file output.res
!c               nerr  integer      flag fuer file output.err
!c     ausgabe:  p(i)  i=1,ipdim    effektivniederschlag nach scs-verfahren

      USE generic_LOGGER
      include 'include\param.cmn'

      real p(idim)

!c     *********************************************************************


       if (dt.gt.23.) goto 6003

!c        write(nres,'(12(f6.2))')(p(i),i=1,ipdim)
!c        write(nres,'(a,i4)')'ipdim = ',ipdim

        hn=0
       pmak = 0.
       pmmax = pmmax/dt
       do 602 i=1,ipdim
          if(pmak.lt.pmmax)then
            pmak=pmak+p(i)
            p(i)=0
            if(pmak.gt.pmmax) then
                p(i) = pmak-pmmax
!c                pmak = pmmax
            endif
          endif
          hn=hn+p(i)
602    continue

!c   p gibt niederschlagsintensitaet an!
!c   absolute niederschlagsmenge = niederschlagsintensitaet * zeitdauer
        hn=hn*dt
        pmmax=pmmax*dt

        write(nres,2603) pmmax, hn
2603    format (' vorgegebener muldenverlust:            ',f6.2,' mm',    &
              /,' gesamtniederschlag noch muldenverlust: ',f6.2,' mm')

!c        write(nres,'(12(f6.2))')(p(i),i=1,ipdim)


       if( cn.lt.0.) then
          write(nres,'(///,a)') '     abflussbeiwert-verfahren'
          psi = - cn
          hne = hn*psi
      write (nres,613) psi, hn, hne
613   format (///,10x,                                                 &
     '     eingelesener abflussbeiwert: ',f6.2,/,10x,                  &
     '     -----------------------------------------------',/,10x,     &
     '     gesamtniederschlagshoehe     hn  = ',f7.3,' mm',/,10x,      &
     '     effektive niederschlagshoehe hne = ',f7.3,' mm',/,//)
          goto 3999
       endif

	 write(nres,'(///,a)') '     scs-verfahren'

!c      grenzwert fuer psi

       psinul = (0.43 * (cn/100.)**4.) -0.03
       if(psinul.lt.0.) psinul = 0.

!c      punkt hnk ab welchem oberhalb die scs-kurve gilt
!c      startwert fuer hnk =
       hnk = (200./cn -2.) * 25.3
       do 10 i = 1,200
          hnk = hnk + 0.5
          hne = (hnk/25.3 - 200/cn + 2)**2./(hnk/25.3                 &
                + 800/cn -8) * 25.3
       if(hne.ge.10.) goto 20
 10    continue

!c      berechnung der steigung
 20    psik = hne/hnk
       hnu = hnk - 0.5
       hno = hnk + 0.5
       hneu = (hnu/25.3 - 200/cn + 2)**2. / (hnu/25.3 + 800/cn -8) * 25.3
       hneo = (hno/25.3 - 200/cn + 2)**2. / (hno/25.3 + 800/cn -8) * 25.3
       psiu = hneu/hnu
       psio = hneo/hno
       psikstr = (psio-psiu)/(hno-hnu)

!c      koeffizienten der ausgleichskurve

       b = psikstr/(psik-psinul) * hnk
       a = (psik-psinul)/hnk**b


!c      berechnung der effektiven niederschlagshoehe

       if(hn.gt.hnk) then

       hne = (hn/25.3 - 200/cn + 2)**2. / (hn/25.3 + 800/cn -8) * 25.3
       psi = hne/hn

       else 

       psi = psinul + a*hn**b
       hne = psi * hn

       end if


      write (nres,603) cn, hnk, hn, hne, psi
603   format (///,10x,                                                    &
     '     eingelesener cn wert: ',f4.1,'    (hnk = ',f7.3,' mm)',/,10x,  &
     '     -----------------------------------------------',/,10x,        &
     '     gesamtniederschlagshoehe     hn  = ',f7.3,' mm',/,10x,         &
     '     effektive niederschlagshoehe hne = ',f7.3,' mm',/,10x,         &
     '     abflussbeiwert               psi = ',f7.3,'  -',//)

3999  xsum=0
      do 4001 i=1,ipdim
          p(i)=p(i)*psi
          xsum=xsum+p(i)
4001  continue
      if(abs(hne-xsum*dt).gt.0.001) then
            write(nerr,4010) hne,xsum

            call writeLogRealReal_two_parameters(7, &
            & 'theoretischer und tatsaechlicher effektivniederschlag nach scs-verfahren stimmen nicht ueberein!',&
            & 'Theoretical and actual effective precipitation do not agree in accordance with SCS-method!', '', &
            & '', 0, 'hne', hne, 'xsum', xsum, 'scs' )

            stop 'scs,1'
4010  format(' theoretischer und tatsaechlicher effektivniederschlag '    &
     /,'nach scs-verfahren stimmen nicht ueberein!'//' programmfehler!'   &
     /, ' theoretischer wert: ',f8.4,/' tatsaechlicher wert: ', f8.4)

      endif
      return

6003  write(nerr,6004)

    call writeLogString(7,'SCS-verfahren nur sinnvoll fuer kurzzeitsimulation','SCS-method is only used for short term simulation',&
    & '', '', '', '', '', 'scs' )

      write(nres,6004)
      stop 'scs,2'
6004  format(' scs-verfahren nur sinnvoll fuer kurzzeitsimulation'/,     &
         ' abbruch der berechnung!')
9999  stop
      end

