!     Last change:  MD    8 Jul 2009    8:32 pm
!--------------------------------------------------------------------------
! This code, newton.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - newton
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
SUBROUTINE newton (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax, &
     & psiein, psiort, froud, xi, hi, s, ifehl, istat, Q_Abfrage)

! geschrieben: p. Koch      Maerz 1990
! geaendert  : w. Ploeger   Mai 2005
!
! Programmbeschreibung:
! ---------------------
! Dieses Programm ermittelt den Wasserspiegel an einem Profil
! Ueber das Newton-Verfahren.
!
!
! AUFGERUFENE SUBROUTINEN:
! ------------------------
! - verluste
!
!-----------------------------------------------------------------------

                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS
USE MOD_INI

implicit none

! Calling variables
REAL, INTENT(IN) :: str                 ! Abstand (Strecke) zwischen zwei Profilen
REAL, INTENT(INOUT) :: q                   ! Abfluss
REAL, INTENT(IN) :: q1                  ! Abfluss
INTEGER, INTENT(IN) :: i                ! Entspricht NPROF, Anzahl bzw. aktive Nummer des Profil,
                                        ! wird von WSBBER ueber NORMBER durchgereicht.
REAL, INTENT(INOUT) :: hr               ! Wasserspiegelhöhe
REAL, INTENT(INOUT) :: hv               ! Geschwindigkeitsverlusthöhe
REAL, INTENT(INOUT) :: rg               ! Reibungsverlusthöhe
REAL, INTENT(INOUT) :: hvst             ! Geschwindigkeitsverlust
REAL, INTENT(INOUT) :: hrst             ! Geschwindigkeitsverlust

INTEGER, INTENT(INOUT) :: indmax	! Anzahl der Rauhigkeitszonen in einem Profil
REAL, INTENT(INOUT) :: psiein		! Verlustbeiwert
REAL, INTENT(INOUT) :: psiort		! Örtlicher Verlust
REAL, INTENT(INOUT) :: froud            ! Froudzahl, wird in VERLUSTE -> ERFROUD bestimmt.
REAL, INTENT(INOUT) :: xi (maxkla)	! Abstand der einzelnen Profilpunkte (werden in VERLUSTE -> UF verwendet)
REAL, INTENT(INOUT) :: hi (maxkla)      ! Höhen der einzelnen Profilpunkte (werden in VERLUSTE -> UF verwendet)
REAL, INTENT(INOUT) :: s (maxkla)       ! Distanz der Profilpunkte (werden in VERLUSTE -> UF verwendet)
INTEGER, INTENT(OUT) :: ifehl           ! Fehlerkennung, normalerweise = 0
INTEGER, INTENT(IN) :: istat            ! Flag zur Kennzeichnung der Strömung ( = 1 gleichförmig, = 0 ungleichförmig)


! COMMON-Block /ALT/ ---------------------------------------------------------------
REAL            :: ws1, rg1, vmp1, fges1, hv1
INTEGER         :: ikenn1
COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
! ----------------------------------------------------------------------------------


! COMMON-Block /ERG/ ----------------------------------------------------------
REAL 		:: wsp (maxger), hen (maxger), qs (maxger), fgesp (maxger)
REAL 		:: froudp (maxger), hvs (maxger), hrs (maxger), hs (maxger)
REAL 		:: fp (maxger, maxkla), up (maxger, maxkla), vp (maxger, maxkla)
REAL 		:: qtp (maxger, maxkla), rkp (maxger, maxkla), fbwp (maxger, maxkla)
REAL 		:: brp (maxger, maxkla)
REAL		:: vmp (maxger), hbors (maxger), hein (maxger), hort (maxger), brg (maxger)
INTEGER 	:: igrenz (maxger)
COMMON / erg / wsp, hen, qs, fgesp, froudp, hvs, hrs, hs, fp, up, &
             & vp, qtp, rkp, fbwp, brp, vmp, hbors, hein, hort, igrenz, brg
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
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


! Local variables
INTEGER, PARAMETER  :: itmax_newton = 25        ! Max. Anzahl der iterationsschritte
CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     ! Abfrage fuer Ende der Inneren Q-Schleife

INTEGER :: jsch, jen            ! Schleifenzaehler
INTEGER :: ifehlg               ! Fehlervariable
REAL :: hrneu, hra, hrb         ! Wasserspiegelhöhe
REAL :: delt                    ! Grenzbedingung
REAL :: dx                      ! Schrittweite, in der der Wasserspiegel veraendert wird
REAL :: fa, fb                  ! Summe Wasserspiegel- und Verlusthöhen
REAL :: df, df1, diff, dif      ! Differenzen
REAL :: hrsta, hrstb   		! Reibungsverlust
REAL :: hvsta, hvstb		! Geschwindigkeitsverlust



! ------------------------------------------------------------------
! VORBELEGUNGEN
! ------------------------------------------------------------------


ifehl = 0
dx = 0.05
!WP
hrsta = 0.0
hvsta = 0.0
hrstb = 0.0
hvstb = 0.0


!WP 19.05.2005
write (UNIT_OUT_LOG, 1000) str, q, q1, i, hr, hv, rg, hvst, hrst, indmax
1000 format (/1X, 'In NEWTON:', /, &
            & 1X, '----------', /, &
            & 1X, 'STR =    ', F10.4, /, &
            & 1X, 'Q =      ', F10.4, /, &
            & 1X, 'Q1 =     ', F10.4, /, &
            & 1X, 'I =      ', I10, /, &
            & 1X, 'HR =     ', F10.4, /, &
            & 1X, 'HV =     ', F10.4, /, &
            & 1X, 'RG =     ', F10.4, /, &
            & 1X, 'HVST =   ', F10.4, /, &
            & 1X, 'HRST =   ', F10.4, /, &
            & 1X, 'INDMAX = ', I10)

WRITE (UNIT_OUT_LOG, 1001) 'I', 'HR', 'HRNEU'
1001 format (/1X, 'Iteration mittels Newton-Verfahren:', //, &
            & 1X,  A2, A15,         A15            , /, &
            & 1X, '--------------------------------')                    


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------




! ******************************************************************
! Iterationsschleife
! ******************************************************************

DO 100 jsch = 1, itmax_newton

  hborda = 0.

  !JK       BERECHNUNG WSP MIT EINSCHLUSSINTERVALL
  if (ITERATIONSART=='EXACT ') then
    jen = jsch + 1
  else
    jen = jsch
  end if

  ! ******************************************************************
  ! berechnung der wasserstands-abhaengigen geometriegroessen:
  ! - flaeche, umfang, breite jeweils fuer die einzelnen teilquer-
  ! schnitte
  ! ******************************************************************

  ifehlg = 0

  CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
          & psiein, psiort, hi, xi, s, istat, froud, ifehlg, jen, Q_Abfrage)

  hrneu = ws1 + hrst + hvst + hborda + heins + horts

  write (UNIT_OUT_LOG, 2001)  q, i, hrneu, ws1, rg, hrst, hvst, hborda, heins, indmax
  2001 format (/1X, 'In NEWTON nach VERLUST:', /, &
            & 1X, '----------', /, &
            & 1X, 'Q =      ', F10.4, /, &
            & 1X, 'I =      ', I10, /, &
            & 1X, 'HRNEU =  ', F10.4, /, &
            & 1X, 'WS1 =    ', F10.4, /, &
            & 1X, 'RG =     ', F10.4, /, &
            & 1X, 'HRST =   ', F10.4, /, &
            & 1X, 'HVST =   ', F10.4, /, &
            & 1X, 'Hborda = ', F10.4, /, &
            & 1X, 'Heins =  ', F10.4, /, &
            & 1X, 'INDMAX = ', I10)


  ! **************************************************************
  ! konvergenzueberpruefung
  ! **************************************************************


  WRITE (UNIT_OUT_LOG, 1002) jsch, hr, hrneu
  1002 format (1X, I2, F15.5, F15.5)

  IF (abs (hrneu - hr) .lt. err) then
    hr = hrneu
    !JK           ZU DATENUEBERGABE
    GOTO 9999

  ELSE

    ! **********************************************************
    ! neuen wasserspiegel schaetzen :
    ! **********************************************************

    IF (idruck.eq.1) then

      IF (jsch.eq.1) then
        IF (hr.gt.hmax .and. hrneu.lt.hmax) then
          hra = hmax
          diff = hmax - hmin
          delt = MIN (diff / 2. , 0.15)
          hrb = hmin + delt

        ELSEIF (hr.gt.hmax .and. hrneu.ge.hmax) then
          hr = hrneu
          !JK                    WEITER MIT PEGASUS
          GOTO 100
        ENDIF
      ELSE
        IF (hr.gt.hmax .and. hrneu.gt.hmax) then
          hr = hrneu
          !JK                    WEITER MIT PEGASUS
          GOTO 100
        ELSE
          hra = hr + dx
          hrb = hr - dx
        ENDIF
      ENDIF
    ELSE
      hra = hr + dx
      hrb = hr - dx
    ENDIF


    IF (hrb.le.hmin) hrb = hr


    !WP Aufruf fuer Parameterset A
    CALL verluste (str, q, q1, i, hra, hv, rg, hvsta, hrsta,      &
     & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, jen, Q_Abfrage)

    fa = ws1 + hrsta + hvsta + hborda + heins + horts

    !WP Aufruf fuer Parameterset B
    CALL verluste (str, q, q1, i, hrb, hv, rg, hvstb, hrstb,      &
     & indmax, psiein, psiort, hi, xi, s, istat, froud, ifehlg, jen, Q_Abfrage)

    fb = ws1 + hrstb + hvstb + hborda + heins + horts


    df = (fa - fb) / (hra - hrb)

    IF (abs (df) .le. 0.0001) then

      hr = hrneu
      !JK  WEITER MIT PEGASUS
      GOTO 100

    ELSEIF (abs (df - 1.00) .lt. 0.0001) then

      ifehl = 1
      !JK  WEITER MIT PEGASUS
      GOTO 9999

    ELSE

      df1 = 1. - df

      ! geradengleichung tangente + schnitt mit sollwert m:
      dif = (hrneu - hr) / df1
      hra = hr
      hr = hr + dif

      IF ( (hr - hmin) .le. 0.001) then
        ifehl = 1
        !JK   WEITER MIT PEGASUS
        GOTO 9999

      ELSEIF (abs (dif) .lt. 0.001) then

        IF (abs (hra - hrneu) .lt. 0.01) then

          WRITE (UNIT_OUT_LOG, 9000) dif
          9000 format (1X, 'Achtung! Abbruch der Iteration bei DIF = ', F15.5)

          hr = hra
          !JK   DATENUEBERGABE
          GOTO 9999

        ENDIF

      ENDIF

    ENDIF

  ENDIF

100 END DO


ifehl = 1                                                                        
                                                                        
9999 CONTINUE



!WP Nur noch Konrollausgaben
IF (ifehl.eq.0) then

  ! stroemender bereich
  CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
   & psiein, psiort, hi, xi, s, istat, froud, ifehlg, jsch, Q_Abfrage)

  !JK  SCHREIBEN IN KONTROLLFILE
  IF (idruck.eq.0) then

    WRITE (UNIT_OUT_LOG, 1003) hr,froud
    1003 format (/1X, 'Wasserspiegel wurde in NEWTON ermittelt zu   HR = ', F15.5, '.', /, &
                & 1X, '                          ( Froudzahl ist FROUD = ', F15.5, ')')
  ELSE

    WRITE (UNIT_OUT_LOG, 1004) hr, hmax
    1004 format (/1X, 'Wasserspiegel wurde in NEWTON ermittelt zu   HR = ', F15.5, '.', /, &
                & 1X, '                                         ( HMAX = ', F15.5, ')')
  ENDIF

ELSE

  !JK     SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, 9001)
  9001 format (1X, 'Achtung! Keine Konvergenz in NEWTON -> Weiter mit PEGASUS!')

ENDIF

END SUBROUTINE newton
