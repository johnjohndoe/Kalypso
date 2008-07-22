!     Last change:  MD    9 Jul 2008   11:04 am
!--------------------------------------------------------------------------
! This code, rohre.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - Rohre
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



!-----------------------------------------------------------------------
SUBROUTINE Rohre (sgef, nprof, q, hr, indmax, hvst, hrst, froud, str, Q_Abfrage)

!
! Beschreibung
! ------------
! Bestimmung des Abflusses durch ein Rohr/Durchlass bei angenommenem
! Sohlgefaelle sgef und Vorgabe eines Wasserspiegels
!
!
! IN DIESER SUBROUTINE VERWENDETE VARIABLEN
! -----------------------------------------
!
! delthr  --      Schrittweite fuer wasserstandsgesteuerte Berechnung
! froud   --      Froud-Zahl
! hmax    --      maximale Höhe (Rohroberkante)
! hmin    --      minimale Höhe (Rohrunterkante)
! hr      --      Wasserspiegelhöhe
! hrst    --      Reibungsverlust
! hvst    --      Geschwindigkeitsverlust
! iprof   --      Art des Profiles
! maxqr   --      Durchfluss bei Vollfuellung des Rohres/Durchlasses
! nprof   --      Anzahl der Punkte in einem Profil
! q       --      Abfluß
! q1      --      Abfluß
! qr      --      Rohrabfluss
! sgef    --      Sohlgefälle
! str     --      Stationsabstand
!
!
! AUFGERUFENE ROUTINEN
! --------------------
!
! verluste(str,q,q1,nprof,hr,hv,rg,hvst,hrst,indmax,
!          psiein,psiort,jw5,hi,xi,s,istat,froud,ifehlg,itere1)
!
!*******************************************************************

USE MOD_ERG
USE DIM_VARIABLEN
USE IO_UNITS
USE MOD_INI


REAL, INTENT(IN) :: sgef		! Vorgegebenes Reibungsgefaelle
INTEGER, INTENT(IN) :: nprof	        ! Profilnummer
INTEGER, INTENT(IN) :: indmax	        ! Anzahl der Rauheitsabschnitte
REAL, INTENT(OUT) :: froud		! Froudzahl
REAL, INTENT(IN) :: str			! Abstand (Strecke) zwischen zwei Profilen


REAL :: xi (maxkla), hi (maxkla), s (maxkla)

! COMMON-Block /ALT/ ----------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
! -----------------------------------------------------------------------------

! COMMON-Block /DD/ -----------------------------------------------------------
REAL            :: tm, vm
COMMON / dd / tm, vm
! -----------------------------------------------------------------------------

! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------

! COMMON-Block /GEF_SOHL/ -----------------------------------------------------
REAL 		:: g_sohl
COMMON / gef_sohl / g_sohl
! -----------------------------------------------------------------------------

! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------

! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------

! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /ROHR/ ---------------------------------------------------------
INTEGER         :: idruck
COMMON / rohr / idruck
! -----------------------------------------------------------------------------


! COMMON-Block /VORT/ ---------------------------------------------------------
REAL 		:: hborda, heins, horts
COMMON / vort / hborda, heins, horts
! -----------------------------------------------------------------------------

!EN++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! COMMON-Block /PA_COM/ -------------------------------------------------------
REAL 		:: a_ks (maxkla), b_ks (maxkla), r_ks (maxkla), u_ks (maxkla)
REAL            :: k_ks (maxkla), h_ks (maxkla), v_ks (maxkla), l_ks (maxkla)
REAL            :: q_ks (maxkla), b_hg, u_hg, a_hg, q_hg, v_hg, r_hg, l_hg, fr_hg
INTEGER         :: ischl, ischr
COMMON / pa_com / a_ks, b_ks, r_ks, u_ks, k_ks, h_ks, v_ks, l_ks, &
       & q_ks, b_hg, a_hg, u_hg, q_hg, v_hg, r_hg, l_hg, fr_hg, ischl, ischr
! -----------------------------------------------------------------------------

INTEGER :: rdruck

! EN lokale Variablen
REAL :: maxqr(1:maxger)
INTEGER :: n
REAL :: delthr(1:maxger)
REAL :: erstd(1:maxger)
REAL :: qr
CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     ! Abfrage fuer Ende der Inneren Q-Schleife


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
! Vorbelegung:

g_sohl = sgef


WRITE (UNIT_OUT_LOG, 333)
     333 format (//1X, '*************************************************', /, &
                 & 1X, 'Sonderprofil Rohr/Durchlass ', /, &
                 & 1X, '*************************************************', /)

WRITE (UNIT_OUT_LOG, '(''Teilweise Entkopplung von der aeusseren Abflussschleife!'' ,/,  &
                & ''Es werden lediglich Stuetzstellen bis zur vollstaendigen Fuellung benoetigt.'' ,/,  &
                & ''Entsprechend der vorgegebenen Schrittzahl der aeusseren Abflussschleife'' ,/,  &
                & ''erfolgt die Rohrberechnung wasserstandsgesteuert.'', /)')


! EN Folgende Schleife wird nur einmal je Profil durchlaufen
IF (erstd(nprof) .ne. 1) THEN

  ! EN Zur besseren Uebersicht wird das Q bei Vollfuellung angegeben
  hr=hmax

  CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,    &
     & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, itere1, Q_Abfrage)

  erstd(nprof) = 1

  maxqr(nprof) = SQRT (ABS(sgef)*8*9.81*fges**3/(uges*l_hg))

  ! EN Anpassung an den Schrittzaehler der globalen Abflussschleife in qbordv
  n = anz_q

  ! EN Berechnung der Schrittweite
  ! delthr=(hmax-hmin)/REAL(n)
  delthr(nprof) = (hmax-hmin)/REAL(n)

END IF


WRITE (UNIT_OUT_LOG, '(''Rohrdurchfluss bei Vollfuellung = '',f12.3, /)') maxqr(nprof)
WRITE (UNIT_OUT_LOG, '(''Schrittweite = '',f12.3, /)') delthr(nprof)


!EN Ermittlung des Wasserstandes
!EN dieser ist an den Schrittzaehler der aesseren Abflussschleife in QBORDV gekoppelt
hr = hmin + (delthr(nprof) * REAL(nr_q))


WRITE (UNIT_OUT_LOG, '(''       wsp        abfluss      flaeche      froud '')')


CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,    &
   & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, itere1, Q_Abfrage)


qr = sqrt( ABS(sgef)*8*9.81*fges**3/(uges*l_hg))


! EN Umspeichern auf die globalen Variable
q = qr


CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,    &
   & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, itere1, Q_Abfrage)


!EN   SCHREIBEN IN KONTROLLFILE
WRITE (UNIT_OUT_LOG, '(f12.5,1x,f12.5,1x,f12.5,1x,f12.5)') hr, qr, fges, froud

WRITE (UNIT_OUT_LOG, '(/,'' --> aus rohre weiter mit hr ='',f7.3,     &
   &              '' froud = '',f7.3,'' hrst = '',f10.4, '' hvst= '', &
   &              f10.4,//)') hr, froud, hrst, hvst


WRITE (UNIT_OUT_LOG, '(/,''brges = '',f10.3,'' fges = '',f10.3,  &
 &  '' vm = '',f10.3,/)') brges, fges, vm


! EN Eigentlich werden nur für i=2 (Flussschlauch) Werte .ne. 0 ausgegeben.
! EN Zu Kontrollzwecken wird die folgene Schleife jedoch beibehalten.
WRITE (UNIT_OUT_LOG, '(''   i     f(i)      u(i)     br(i)   ra(i)   q(i)'')')

DO i = 1, 3
  WRITE (UNIT_OUT_LOG, '(i4,5(f10.3))') i, f(i) , u(i) , br(i) , ra(i) , qt(i)
END DO


! EN Abspeichern des Abflusses für das Aktuelle Rohrprofil
out_PROF(nprof,nr_q)%qges=qr


END SUBROUTINE rohre

