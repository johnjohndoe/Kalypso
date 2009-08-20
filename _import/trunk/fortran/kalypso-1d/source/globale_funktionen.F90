!     Last change:  MD    8 Jul 2009    4:11 pm
!--------------------------------------------------------------------------
! This code, globale_funktionen.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Functions:
! - GET_LAMBDA
! - GET_CW_UN
! - GET_A_NL
! - GET_A_NB
! - GET_OMEGA
! - GET_CT
! - GET_REL_V_AN
! - GET_BORDA
! - GET_MITVOR
!
! Copyright (C) 2005-2006  WOLF PLOEGER.
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
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Deptartment of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-1D'.
!
! Wolf Ploeger, 07 Juli 2005
! Research Associate
!---------------------------------------------------------------------------------------



!---------------------------------------------------------------------------------------
REAL function GET_LAMBDA (v, rhy, ks, f)
!
! geschrieben :                        April 2005 Wolf Ploeger
! geaendert :                          Oktober 2007 Wolf Ploeger
!
! Allgemeine Beschreibung:
! ------------------------
! Funktion zur Bestimmung des Widerstandsbeiwertes der Sohle
! nach Colebrook-White.
! NEU: Erweiterung Berechnung von extremen Rauheiten
! nach Aguirre-Pe und Fuentes
!
! LITERATUR: BWK-MERKBLATT 1, 1999, S.20 ff
!
! Variablendefinition:
! f      =   profilartabhängiger Formbeiwert (nach BWK Merkblatt 1, 1999)
! ks     =   aequivalente Sandrauheit
! lalt   =   Widerstandsbeiwert der Sohle (Iteration)
! lambda =   Widerstandsbeiwert der Sohle
! re     =   Reynoldszahl
! rhy    =   hydraulischer Radius
! v      =   mittlere Fließgeschwindigkeit
! ------------------------------------------------------------------------

USE KONSTANTEN
USE MOD_INI
USE IO_UNITS
USE EXTREME_ROUGHNESS

implicit none

! Calling variables
REAL, INTENT(IN) 	:: v, rhy, f, ks

! Local variables
REAL :: Re, lalt, Dm, ks_local, rhy_local
REAL :: lambda_CW, lambda_AF, lambda_final
REAL :: f_CW, f_AF
REAL :: lower_limit, upper_limit
REAL :: grenzkrit
INTEGER :: i

! Initialisieren --------------------------------------------------------------------
lalt = 1000.0   ! Vorheriger Lambda-Wert (fuer Iteration)
i = 0           ! Zaehler fuer Iterationsschleife
ks_local = ks   ! Umspeichern von ks, da eventuell geaendert bei geringen Wassertiefen


IF (.not. USE_EXTREM_ROUGH) THEN
  grenzkrit = rhy / ks
  IF (grenzkrit .lt. 1.66667) ks_local = 0.60 * rhy
END IF

! Berechnung ------------------------------------------------------------------------
Re = v * 4 * rhy / nue

!WP Es kann passieren, dass v = 0 an diese Funktion uebergeben wird.
!WP Dann darf nicht mit Re = 50 gerechnet werden, sondern es sollte
!WP mit der Formel ohne laminaren Einfluss (Re > 2320) gerechnet werden.
if (Re < 50. .and. Re >= 1.) then
  Re = 50.0
else if (Re < 1.0) then
  !WP Re < 1 tritt nie auf! Deshalb ist hier wahrscheinlich, dass v = 0.0
  !WP uebergeben wurde.
  Re = 50000.0
end if

IF (Re < 10000.0) THEN           !WP Vorher stand hier Re < 2320

   !MD:CHECK !MD:CHECK !MD:CHECK
   IF (ks_local .le. 0.0 ) then
     write (*,8000)
     write (UNIT_OUT_LOG,8000)
     8000 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                   & 1X, 'URSACHE: ks-Wert <= 0.0 vorhanden.')
     Stop
   ElseIF (rhy .le. 0.0 ) then
     write (*,8001)
     write (UNIT_OUT_LOG,8001)
     8001 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                   & 1X, 'URSACHE: rhy <= 0.0 vorhanden --> WSP negativ??.')
     Stop
   ElseIF (f .le. 0.0 ) then
     write (*,8002)
     write (UNIT_OUT_LOG,8002)
     8002 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                   & 1X, 'URSACHE: formbeiwert <= 0.0 vorhanden.')
     Stop
   ENDIF
   !MD:CHECK !MD:CHECK !MD:CHECK


   ! Startwert Lambda
   lambda_CW = ( - 2.03 * log10 (ks_local / (14.84 * rhy * f) ) ) ** 2.0
   IF (lambda_CW .eq. 0.0) lambda_CW = 0.0001
   lambda_CW = 1.0 / lambda_CW

   iteration_lambda: do

  	IF ( ABS((lambda_CW/lalt)-1.0) .LE. 0.001 ) EXIT iteration_lambda

  	IF ( i >= 50 ) then
          ! no convergence after 50 iterations
          lambda_CW = 0.10

          write (*,9000)
          write (UNIT_OUT_LOG,9000)
          9000 format (1X, 'Achtung: Keine Konvergenz bei der Berechnung von LAMBDA!', /, &
                     & 1X, 'Es wird LAMBDA = 0.10 angenommen.')

          EXIT iteration_lambda
  	ENDIF

   	i = i + 1
	lalt = lambda_CW

        !MD:CHECK !MD:CHECK !MD:CHECK

         IF (ks_local .le. 0.0 ) then
           write (*,8006)
           write (UNIT_OUT_LOG,8006)
           8006 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                         & 1X, 'URSACHE: ks-Wert <= 0.0 vorhanden.')
           Stop
         ElseIF (rhy .le. 0.0 ) then
           write (*,8007)
           write (UNIT_OUT_LOG,8007)
           8007 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                         & 1X, 'URSACHE: rhy <= 0.0 vorhanden --> WSP negativ??.')
           Stop
         ElseIF (f .le. 0.0) then
           write (*,8008)
           write (UNIT_OUT_LOG,8008)
           8008 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                         & 1X, 'URSACHE: formbeiwert <= 0.0 vorhanden.')
           Stop
         ElseIF (lalt .le. 0.0) then
           write (*,8009)
           write (UNIT_OUT_LOG,8009)
           8009 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                         & 1X, 'URSACHE: lalt <= 0.0 vorhanden.')
           Stop
         ElseIF (Re .le. 0.0) then
           write (*,8010)
           write (UNIT_OUT_LOG,8010)
           8010 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                         & 1X, 'URSACHE: Re <= 0.0 vorhanden.')
           Stop
         ENDIF
         !MD:CHECK !MD:CHECK !MD:CHECK


   	! formular by COLEBROOK/WHITE
   	lambda_CW = ( - 2.03 * log10 (2.51 / (f * Re * SQRT(lalt) ) + ks_local / (14.84 * rhy * f) ) ) ** 2.0

   	IF (lambda_CW .eq. 0.0) lambda_CW = 0.0001

   	lambda_CW = 1.0 / lambda_CW

   end do iteration_lambda

   !write (*,*) 'In GET_LAMBDA. Iterationen: ', i, '  LAMBDA = ', lambda, '  RE = ', re

ELSE
  
  ! Turbulente Strömung

  !MD:CHECK !MD:CHECK !MD:CHECK
   IF (ks_local .le. 0.0 ) then
     write (*,8003)
     write (UNIT_OUT_LOG,8003)
     8003 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                   & 1X, 'URSACHE: ks-Wert <= 0.0 vorhanden.')
     Stop
   ElseIF (rhy .le. 0.0 ) then
     write (*,8004)
     write (UNIT_OUT_LOG,8004)
     8004 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                   & 1X, 'URSACHE: rhy <= 0.0 vorhanden --> WSP negativ??.')
     Stop
   ElseIF (f .le. 0.0 ) then
     write (*,8005)
     write (UNIT_OUT_LOG,8005)
     8005 format (1X, 'Achtung: Keine Berechnung von LAMBDA moeglich!', /, &
                   & 1X, 'URSACHE: formbeiwert <= 0.0 vorhanden.')
     Stop
   ENDIF

  ! formular by COLEBROOK/WHITE
  lambda_CW = ( - 2.03 * log10 (ks_local / (14.84 * rhy * f) ) ) ** 2.0

  IF (lambda_CW .eq. 0.0) lambda_CW = 0.0001

  lambda_CW = 1.0 / lambda_CW

END IF


IF (USE_EXTREM_ROUGH) THEN

  upper_limit = 3*ks_local
  lower_limit = 1*ks_local
  Dm = ks_local / 3.0

  ! In very shallow water (h < 0.05 m) the formula after AGUIRRE/FUENTES
  ! sometimes tends to produce very low lambda-values. This must
  ! be avioded and is done by limiting the hydraulic radius.
  if (rhy < 0.05) then
    rhy_local = 0.05
  else
    rhy_local = rhy
  end if     

  ! Calculation of LAMBDA if using AGUIRRE/FUENTES
  lambda_AF = ( (0.88*beta_w*Dm)/rhy_local + 2.03 * log10 ( (11.1*rhy_local)/(alpha_t*Dm) ) ) ** 2.0

  IF (lambda_AF <= 0.0001) lambda_AF = 0.0001
  lambda_AF = 1.0 / lambda_AF

  ! Do transition between Colebrook and Aguirre
  if (rhy_local > upper_limit) then
    f_CW = 1.0
    f_AF = 0.0
  else if (rhy_local < lower_limit) then
    f_CW = 0.0
    f_AF = 1.0
  else
    f_CW = (rhy_local-lower_limit)/(upper_limit-lower_limit)  
    f_AF = 1.0 - f_CW
  end if

  lambda_final = f_CW*lambda_CW + f_AF*lambda_AF

ELSE

  lambda_final = lambda_CW

END IF

GET_LAMBDA = lambda_final

end function GET_LAMBDA





!---------------------------------------------------------------------------------------
REAL function GET_CW_UN (vrep, d_p, nue)
!
! geschrieben :                        16.11.1984 B. Schwamborn
! geaendert   :                        28.03.1985 Juergen Kuck
!                                      28.04.2005 Wolf Ploeger
!
! Allgemeine Beschreibung:
! ------------------------
! 'GET_CW_UN' berechnet den cw-wert eines zylindrischen Stabelements
! in Abhaengigkeit von der Anstroemgeschwindigkeit, dem Stabdurchmes-
! ser dp und der kinematischen Zaehigkeit nue des Fluids.
!
! LITERATUR: BWK-MERKBLATT 1, 1999, S.30
!---------------------------------------------------------------------------------------

implicit none
                                                                        
! Calling variables
REAL, INTENT(IN) :: vrep        ! Anstroemgeschwindigkeit
REAL, INTENT(IN) :: d_p         ! Durchmesser des zylindrischen Elements
REAL, INTENT(IN) :: nue         ! Kinematische Viskositaet

! Lokal variables
REAL 		:: re_p
REAL            :: cwun

re_p = vrep * d_p / nue
                                                                        
IF (re_p < 800.0 .and. re_p > 0.01) then
   cwun = 3.07 * re_p ** ( - .168)
ELSEIF (re_p < 8000.0 .and. re_p >= 800.0) then
   cwun = 1.0
ELSEIF (re_p < 10000.0 .and. re_p >= 8000.0) then
   cwun = 1.0 + 0.2 * (re_p - 8000.0) / 2000.0
ELSEIF (re_p > 10000.0) then
   cwun = 1.2
ELSE
   cwun = 1.0
ENDIF

GET_CW_UN = cwun

END FUNCTION GET_CW_UN




!---------------------------------------------------------------------------------------
REAL function GET_A_NL(c_wr_un, d_p, I_R, v, anf, g)
!
! geschrieben :                        28.04.2005 Wolf Ploeger
!
! Allgemeine Beschreibung:
! ------------------------
! 'GET_A_NL' berechnet die Nachlauflaenge bei der Umstroemung
! von zylindrischen Elementen.
!
! LITERATUR: BWK-MERKBLATT 1, 1999, S.30
!---------------------------------------------------------------------------------------

USE IO_UNITS

implicit none
! calling variables
REAL, INTENT(IN) :: c_wr_un    ! Widerstandsbeiwert
REAL, INTENT(IN) :: d_p        ! Durchmesser des Bewuchselementes
REAL, INTENT(IN) :: I_R        ! Stationaeres Gefaelle
REAL, INTENT(IN) :: v          ! Massgebende Fliessgeschwindigkeit
REAL, INTENT(IN) :: anf        ! Anfangswert für Iteration ( = 2 * ax )
REAL, INTENT(IN) :: g          ! Erdbeschleunigung

! Local variables
INTEGER :: i
REAL :: a_NL
REAL :: a_NL2

! Testen Gueltigkeit
if (v < 0.001) then
  !write (*,*) 'In GET_A_NL. v = ', v
  GET_A_NL = anf
  RETURN
end if

! Initialisierung
a_NL  = anf
a_NL2 = 128.9 * c_wr_un * d_p * (1. + (g * a_NL * I_R)/((v**2)/2))**(-2.14)


i = 0                           	! Zähler der Iteration von a_NL

iteration_a_NL: do			! Iteration bis Konvergenzkriterium für a_NL erfüllt ist

  if ( ABS(1-(a_NL/a_NL2)) < 0.001 ) exit iteration_a_NL
  i = i + 1               		! Inkrementieren des Schleifenzählers
  a_NL  = a_NL2
  a_NL2 = (a_NL + 128.9 * c_wr_un * d_p * (1 + (g * a_NL * I_R)/((v**2)/2))**(-2.14)) / 2

  if (i > 100) then       		! Keine Konvergenz nach 100 Iterationen
    GET_A_NL = anf                      ! Der Wert 0.0 zeigt dem aufrufenden Programm

    write (UNIT_OUT_LOG,1000) GET_A_NL
    1000 format (1X, 'Achtung: Keine Konvergenz bei der Berechnung der Nachlauflaenge!', /, &
               & 1X, 'Es wird angenommen: A_NL = ', F10.4)

    GOTO 100                            ! an, dass die Iteration NICHT erfolgreich war!
  END if

  !write (*,*) 'Iteration Nr. ', i, '  A_NL = ', a_NL2

end do iteration_a_NL			! Iteration von a_NL erfolgreich!

GET_A_NL = a_NL                         ! Zuweisung des endgültigen Wertes

100 continue

END function GET_A_NL




!---------------------------------------------------------------------------------------
REAL function GET_A_NB (c_wr_un, d_p, a_NL)
!
! geschrieben :                        28.04.2005 Wolf Ploeger
!
! Allgemeine Beschreibung:
! ------------------------
! 'GET_A_NB' berechnet die Nachlaufbreite bei der Umstroemung
! von zylindrischen Elementen.
!
! LITERATUR: BWK-MERKBLATT 1, 1999, S.30
!
!---------------------------------------------------------------------------------------

implicit none

! calling variables
REAL, INTENT(IN) :: c_wr_un    ! Widerstandsbeiwert
REAL, INTENT(IN) :: d_p        ! Durchmesser des Bewuchselementes
REAL, INTENT(IN) :: a_NL       ! Nachlauflänge

GET_A_NB = 0.24 * (a_NL ** 0.59) * (c_wr_un * d_p) ** 0.41

END function GET_A_NB




!---------------------------------------------------------------------------------------
REAL function GET_OMEGA (a_NL, ax, a_NB, ay)
!
! geschrieben :                        28.04.2005 Wolf Ploeger
!
! Allgemeine Beschreibung:
! ------------------------
! 'GET_OMEGA' berechnet den Bewuchsparameter, mit dem der Einfluss
! des Bewuchses auf die Trennflächenrauheit beschrieben wird.
!
! LITERATUR: BWK-MERKBLATT 1, 1999, S.36
!
!---------------------------------------------------------------------------------------

implicit none

! calling variables
REAL, INTENT(IN) :: a_NL       ! Nachlauflänge
REAL, INTENT(IN) :: ax         ! Bewuchsabstand x
REAL, INTENT(IN) :: a_NB       ! Nachlaufbreite
REAL, INTENT(IN) :: ay         ! Bewuchsabstand y

! local variables
REAL :: temp

temp = ( 0.07 * (a_NL/ax) ) ** 3.3 + (a_NB/ay) ** 0.95

if (temp > 3.0) then
  GET_OMEGA = 3.0
else
  GET_OMEGA = temp
end if

END function GET_OMEGA




!---------------------------------------------------------------------------------------
REAL function GET_CT (omega)

implicit none

! calling variables
REAL, INTENT(IN) :: omega       ! Bewuchsparameter

GET_CT = - 3.27 * LOG10(omega) + 2.85

END function GET_CT



!---------------------------------------------------------------------------------------
REAL function GET_REL_V_AN (a_NL, ax, a_NB, ay)

implicit none

! calling variables
REAL, INTENT(IN) :: a_NL       ! Nachlauflänge
REAL, INTENT(IN) :: ax         ! Bewuchsabstand x
REAL, INTENT(IN) :: a_NB       ! Nachlaufbreite
REAL, INTENT(IN) :: ay         ! Bewuchsabstand y

! local variables
REAL :: temp

GET_REL_V_AN = ( 1.151 * (a_NL/ax) ) ** (-0.483) + 0.5 * (a_NB/ay) ** 1.1

END function GET_REL_V_AN



!---------------------------------------------------------------------------------------
REAL function GET_BORDA (vo, vu, wsp)

! Berechnung des Fliessverlustes infolge ploetzlicher Einengung
! mit Hilfe der borda'schen Stossdruckformel
! Quelle: Roessert, Hydraulik im Wasserbau, 1978, s. 66

implicit none

REAL, INTENT(IN) :: vo, vu, wsp

GET_BORDA = (vo - vu) * (vo - vu) / 2. / 9.81 - sqrt ( ( (vo * vu -   &
      &  vu * vu) / 9.81) **2 + wsp * wsp) + wsp

END FUNCTION GET_BORDA



!---------------------------------------------------------------------------------------
REAL function GET_FORM(Flaeche, Breite)

USE KONSTANTEN

implicit none

REAL, INTENT(IN) :: Flaeche
REAL, INTENT(IN) :: Breite

REAL :: dyn_hoehe

if (Flaeche > 0.0 .and. Breite > 0.0) then
  dyn_hoehe = Flaeche / Breite
  get_form = 0.9 - 0.38 * (EXP(-5*dyn_hoehe/Breite))
else
  get_form = 1.0
end if

end function GET_FORM



!---------------------------------------------------------------------------------------
REAL FUNCTION GET_MITVOR (ct, dp, ax, hvor, lamvog)
!
! Beschreibung:
! -------------
! Berechnung der mitwirkenden Vorlandbreite nach PASCHE,
! siehe BWK Merkblatt 1, 1999, S. 36

! Calling variables
REAL, INTENT(IN)  :: ct         ! Faktor zur Berechnung der Trennflaechengeschw.
REAL, INTENT(IN)  :: dp         ! Bewuchsparameter
REAL, INTENT(IN)  :: ax         ! Bewuchsparameter
REAL, INTENT(IN)  :: hvor       ! mittlere Wasserspiegelhoehe des Vorlandes
REAL, INTENT(IN)  :: lamvog     ! Widerstandsbeiwert des Vorlandes

! Local variables
REAL :: bmv

IF (dp / ax .gt. 0.5) then
  bmv = dp
else
  ! naeherungsweise bestimmung von bmv als anfangswert nach
  ! dissertation pasche diagramm 5.39 bzw.angenaeherter parabel :
  bmv = hvor / (lamvog * (0.068 * exp (0.564 * ct) - 0.0558) )
END if

GET_MITVOR = bmv

END FUNCTION GET_MITVOR


