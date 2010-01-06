!     Last change:  MD    6 Jan 2010   12:42 pm
!--------------------------------------------------------------------------
! This code, lapro.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - lapro1
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



!---------------------------------------------------------------------------
SUBROUTINE lapro1 (unit5, pfad2, nprof, NAME_OUT_LAENGS, NAME_OUT_QLAENGS)
!MD SUBROUTINE lapro1 (unit5, pfad2, nprof, mark, NAME_OUT_LAENGS)
!
! geschrieben:    01.08.1989  p. koch
! geaendert:      10.05.2005  w. ploeger (um variable file_laengs erweitert)
!
!                                                                       
! Programmbeschreibung:
! ---------------------
! dieses programm bereitet die daten auf zur erstellung von plotge-
! rechten laengsschnittdateien. die dateien selbst werden dann in
! der subroutine zeila erstellt.
!
! Insbesondere wird die Reihenfolge der Profile vertauscht.
!                                                                       
! laengschnitte bei der bordvoll-berechnung:
! mark=1  - erstellung eines .wsp-files (nur zu kontroll-zwecken)
! mark=2  - erstellung eines .qbv-files
!                                                                       
! laengsschnitt bei der normalen wasserspiegellagenberechnung:
! mark=3  - erstellung eines .wsl-files
! --------------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE AUSGABE_LAENGS
USE MOD_INI

implicit none

CHARACTER(LEN=nch80), INTENT(IN) :: unit5
CHARACTER(LEN=nch80), INTENT(IN) :: pfad2
INTEGER, INTENT(IN) :: nprof
! INTEGER, INTENT(IN) :: mark
CHARACTER(LEN=nch80), INTENT(IN) :: NAME_OUT_LAENGS         ! Variable für Laengschnitt.txt
CHARACTER(LEN=nch80), INTENT(IN) :: NAME_OUT_QLAENGS        ! Variable für Q_Langschnitt.txt

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


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 	:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 	:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /PRO1/ ---------------------------------------------------------
REAL 	:: qbord(maxger), hbord (maxger), vbord (maxger)
INTEGER :: jkenn (maxger)
COMMON / pro1 / qbord, hbord, jkenn, vbord
!     Vorbelegung:
!     ------------
!     jkenn - kennung dafuer, ob der bordvoll wert ermittelt worden ist 
!             oder nicht                                                
!             jkenn=0 - bordvoller abfluss noch nicht ermittelt         
!             jkenn=1 - bordvoller abfluss ermittelt                    
! -----------------------------------------------------------------------------


! COMMON-Block /PROFILNUMMER/ -------------------------------------------------
CHARACTER(LEN=10) :: num (maxger)
COMMON / profilnummer / num
! -----------------------------------------------------------------------------


! Local variables -------------------------------------------------------------
INTEGER :: i, m


! Berechnungen ----------------------------------------------------------------

DO i = 1, nprof

  m = nprof + 1 - i
  xss (i) = - stat (m) * 1000.
  num5 (i) = num (m)
  boli1 (i) = bolip (m)
  bore1 (i) = borep (m)
  sohl1 (i) = sohlp (m)
  wsp1 (i) = wsp (m)
  hen1 (i) = hen (m)
  hwsp1 (i) = hbord (m)
  qbv1 (i) = qbord (m)
  vp_li (i) = vp(m,1)
  vp_fl (i) = vp(m,2)
  vp_re (i) = vp(m,3)
  fp_li (i) = fp(m,1)
  fp_fl (i) = fp(m,2)
  fp_re (i) = fp(m,3)
  rk_li (i) = rkp(m,1)
  rk_fl (i) = rkp(m,2)
  rk_re (i) = rkp(m,3)
  br_li (i) = brp(m,1)
  br_fl (i) = brp(m,2)
  br_re (i) = brp(m,3)

  IF (BERECHNUNGSMODUS == 'WATERLEVEL') then
    vbv1 (i) = vmp (m)
  ELSE
    vbv1 (i) = vbord (m)
  ENDIF

  gef1 (i) = isstat (m)
  jkenn1 (i) = jkenn (m)
  hming1 (i) = hmingp (m)
  k_ks1 (i) = k_kp (m)

  !write (*,1001) i, gef1(i)
  !1001 format (1X, 'Profil ', I3, ' Gefaelle = ', F12.9)

END DO


!ST 29.03.2005---------------------------------------------------
!Erweitert um Variable für Pfadnamen von Laengsschnit.txt
!MD CALL zeila (unit5, nprof, pfad2, mark, NAME_OUT_LAENGS)
CALL zeila (unit5, nprof, pfad2, NAME_OUT_LAENGS, NAME_OUT_QLAENGS)

END SUBROUTINE lapro1                                                
