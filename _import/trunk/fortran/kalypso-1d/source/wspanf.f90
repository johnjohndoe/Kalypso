!     Last change:  MD   10 Jul 2007    1:14 pm
!--------------------------------------------------------------------------
! This code, wspanf.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - wspanf
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



!--------------------------------------------------------------------------------
SUBROUTINE wspanf (wsanf, strbr, q, q1, hr, hv, rg, indmax, hvst, &
         & hrst, psiein, psiort, nprof, hgrenz, ikenn, nblatt, nz, idr1)
!
! BESCHREIBUNG
! ------------
! Bestimmung des Ausgangswasserspiegel ALS GRENZTIEFE
! bzw. unter Annahme STATIONAER-GLEICHFOERMIGEN ABFLUSSES mit einem
! festen Gefaelle
!
!
! IN DIESER SUBROUTINE VERWENDETE VARIABLEN
! -----------------------------------------
!
! boli(i)
! bore(i)
! froud   --      Froud-Zahl
! hborda  --      Einengungsverlust
! ifehl   --      Fehlervariable
! sohl(i) --
! str     --      Strecke zwischen den Profilen
!
!
! AUFGERUFENE ROUTINEN
! --------------------
! - grnzh (q,indmax,hgrenz,xi,hi,s,igrnz)
! - kopf(nblatt,nz,jw5,jw7,idr1)
! - abskst
! - station
! - verluste
!
!--------------------------------------------------------------------------------
                                                                        

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

implicit none

! Calling variables
REAL :: wsanf                   ! Anfangswasserspiegelhöhe
REAL, INTENT(IN) :: strbr       ! Strecke zwischen den Profilen
REAL :: q                       ! Abfluss
REAL :: q1                      ! Abfluss
REAL :: hr                      ! Wasserspiegelhoehe
REAL :: hv                      ! Verlusthoehe gesamt
REAL :: rg                      ! Reibungsverlusthoehe
INTEGER :: indmax               ! Anzahl der Rauheitsabschnitte
REAL :: hvst                    ! Verlusthoehe
REAL :: hrst                    ! Verlusthoehe
REAL :: psiein                  ! Verlustbeiwert
REAL :: psiort                  ! Verlustbeiwert
INTEGER :: nprof                ! Anzahl der Punkte in einem Profil
REAL :: hgrenz                  ! Grenztiefe
INTEGER :: ikenn
INTEGER :: nblatt               ! Anzahl der Blaetter des Ergebnisfiles
INTEGER :: nz                   ! Anzahl der Zeilen des Ergebnisfiles

!EP   Die Variable idr1 wird über SUBR KOPF referenziert aber nie vorher
!EP   deklariert. IDR1 wird in SUBR QBORDV über den Common-Blcok BV initialisiert
!EP   Dieser Common-Block ist hier aber nicht übernehmbar, da die Variable
!EP   bereits in der Parameterliste der SUBR übergeben wird. Daher muß IDR1
!EP   falls über die Parameterliste übergeben werden. Entsprechend ist
!EP   diese Version erweitert worden. Gleichzeitig wird sie als Charactacter definiert
CHARACTER(LEN=1) :: idr1



! COMMON-Block /ALT/ ---------------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
! ----------------------------------------------------------------------------------



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


! COMMON-Block /VORT/ ---------------------------------------------------------
REAL 		:: hborda, heins, horts
COMMON / vort / hborda, heins, horts
! -----------------------------------------------------------------------------


! Local variables
INTEGER :: igrnz, ifehl, itere1, ifehlg, istat
REAL :: str
REAL :: froud

REAL :: xi (maxkla), hi (maxkla), s (maxkla)


! ------------------------------------------------------------------
! VORBELEGUNG
! ------------------------------------------------------------------

hborda = 0.
heins = 0.
horts = 0.

! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------

!write (*,*) 'In WSPANF. wsanf = ', wsanf

! Berechnung von Basisgeometriegroessen:
! Sehnen, Abstand zwischen den profilpunkten (horizontal und veritkal)
CALL abskst (nknot, x1, xi, h1, hi, s, maxkla)
! input:  nknot,x1,h1
! output: xi,hi,s
                                                                        
! Berechnung der tiefsten sohle und der boeschungskanten (s. profil).
! Gebraucht werden: boli(i),bore(i),sohl(i)
                                                                        
                                                                        
! ------------------------------------------------------------------
! ERMITTLUNG DER GRENZTIEFE
! ------------------------------------------------------------------

! Initialisierung der Variable igrnz.
! Diese bleibt undefiniert, wenn SUBR grnzh nicht aufgerufen wurde
! Dieses ist dann der Fall, wenn nprof.ne.1 .or. strbr.eq.0)
igrnz = 0
                                                                        
IF (nprof.eq.1.or.strbr.ne.0.) then
                                                                        
  IF (nprof.eq.1) then

    write (UNIT_OUT_LOG, 1000) hmin
    1000 format (/1X, 'Aufruf von GRNZH in WSPANF mit   hmin = ', F8.3)

  ENDIF
                                                                        
  CALL grnzh (q, indmax, hgrenz, xi, hi, s)

ENDIF

100 CONTINUE
                                                                        

IF (wsanf.lt.0.) then
!MD  Ermittlung Anfangs-WSP aus stationaer gleichfoermigen Gefaelle = wsanf
!MD  bzw. aus dem ggbnen Reibungsgefa  elle fuer das gesamte Gewaesser
                                                                        
  !UT   SETZE EINUNGSVERLUST NULL
  hborda = 0.
                                                                        
  !UT    Erstes profil, DANN str = 100
  IF (nprof.eq.1) then
    str = 100.
  ELSEIF (nprof.ne.1) then   !MD neu fuer Reibungsgefaelle
    str = strbr
  Endif

  CALL station (wsanf, nprof, hgrenz, q, hr, hv, rg, indmax, hvst, &
      & hrst, psiein, psiort, hi, xi, s, ikenn, froud, str, ifehl, &
      & nblatt, nz, idr1)

  !UT       FALLS DAS AUS station ERHALTENE ifehl ne.0, DANN IN FILE
  IF (ifehl.ne.0) then

    WRITE (UNIT_OUT_LOG, 1001)
    1001 format (1X, 'Keine Konvergenz in stationaer --> Ermittlung Anfangswsp als Grenztiefe!')

    wsanf = 0.
                                                                        
    !JK      Weiter zur BERECHNUNG mit GRENZTIEFE
    GOTO 100
                                                                        
  ENDIF
                                                                        
ELSEIF (wsanf.eq.0.) then
  ! Ermittlung des Wasserspiegels als Grenztiefe
  ikenn = 1
  hr = hgrenz
                                                                        
  IF (strbr.eq.0.) then

    nz = nz + 2
                                                                        
    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF
                                                                        
    WRITE (UNIT_OUT_TAB, 1002)
    1002 format (/1X, 'Als Ausgangswasserspiegel wird Grenztiefe angesetzt!')

    str = 100.
    !                                /* 1tes profil (ideeller abstand)
    q1 = 0.
    !                                /* 1tes profil
    itere1 = 1

  ELSE

    str = strbr

  ENDIF
                                                                        

  CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,       &
     & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, itere1)

ELSE 
  ! (wsanf.gt.0.)
  ! wsp aus qwert.dat, bzw. vorgegeben (z.b. bei bruecken)
  ! --> wenn wsp aus bruecke, dann rau(i)=raub !!!!!

  ikenn = 0

  !UT       WASSERSPIEGELHOEHE = ANFANGSWASSERSPIEGELHOEHE
  hr = wsanf
                                                                        
  IF (nprof.eq.1) then

    nz = nz + 2
                                                                        
    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF
                                                                        
    WRITE (UNIT_OUT_TAB, 1003) hr
    1003 format (/1X, 'Als Anfangswasserspiegel wird WSP = ', F8.3, ' angesetzt.' )

    str = 0. 
    q1 = 0.

  ENDIF 
                                                                        
  itere1 = 1
                                                                        
  CALL verluste (str, q, q1, nprof, hr, hv, rg, hvst, hrst,       &
     & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, itere1)

  IF (froud.ge.1.) ikenn = 1
                                                                        
ENDIF
                                                                        
!UT   WENN ERSTES PROFIL, STRECKLE = null                               
IF (nprof.eq.1) str = 0.
                                                                        
WRITE (UNIT_OUT_LOG, 1004) hr, froud, hrst, hvst
1004 format (/1X, '--> In WSPANF berechnete Werte:', /, &
            & 1X, '    HR    = ', F8.3, /, &
            & 1X, '    FROUD = ', F8.3, /, &
            & 1X, '    HRST  = ', F8.3, /, &
            & 1X, '    HVST  = ', F8.3, /)

ikenn = ikenn + igrnz

END SUBROUTINE wspanf
