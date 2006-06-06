!     Last change:  WP    2 Jun 2006   10:42 pm
!--------------------------------------------------------------------------
! This code, kopf.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - kopf
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

SUBROUTINE kopf (nblat, nz, iw1, jw7, idr1)

USE ZEIT
USE VERSION
USE MOD_INI

INTEGER, INTENT(IN)  :: nblat           ! Blattnummer
INTEGER, INTENT(OUT) :: nz              ! Anzahl der Zeilen auf Blatt, wird auf 1 zurueckgesetzt.
INTEGER, INTENT(IN)  :: iw1             ! UNIT fuer die Ausgabe
INTEGER, INTENT(IN)  :: jw7             ! UNIT fuer die Ausgabe
CHARACTER(LEN=1), INTENT(IN) :: idr1    ! Genauigkeitsparameter
                                                                        

!WP 02.05.2005
!WP Falls KOPF die UNIT-Nummer jw7 = 0 uebergeben wird, bedeutet das
!WP die Ausgabe in der DOS-Box, die von WSPWIN erzeugt wird.
!WP Es soll dort aber nicht mehr die Urzeit stehen, sondern nur noch die
!WP Profilnummern, ausgegeben direkt in WSPBER.
if (jw7 /= 0) then

  WRITE (jw7, * ) ''
  WRITE (jw7, * ) '    ', MMTTJJ, ' ', HHMMSSHH, ' Uhr   ', VERSIONNR
  WRITE (jw7, * ) ''

end if

                                                                        
! Widerstandsberechnung nach DARCY-WEISBACH
if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then

  WRITE (iw1, 61) nblat

  IF (idr1.eq.'j') then
    WRITE (jw7, 61) nblat
  ENDIF

! Widerstandsberechnung nach MANNING-STRICKLER
ELSE

  WRITE (iw1, 60) nblat

  IF (idr1.eq.'j') then
    WRITE (jw7, 60) nblat
  ENDIF

ENDIF

nz = 1
RETURN

60 FORMAT(''/, &
     & 66X, 'Blatt', i5,//, &
     &  1x, '  Stat-km', 1x, '  Wsp-m+NN', 1x, '  hen-m+NN', 1x, 'IND', &
     &  1x, '   k-st', 1x, ' formb', 1X, '   Fl-qm', 1x, '  Umfg-m', 1x, ' v-m/s', 1x, ' Q-cbm/s',//, &
     & T78, '   Q-ges', 1X, '   hv-m', 1X, '   hr-m',1X, '  h-bor', 1X, '  h-ein', 1X, '  h-ort', 1X,'     h-m'/)

61 FORMAT(''/, &
     & 66X, 'Blatt', i5,//, &
     &  1x, '  Stat-km', 1x, '  Wsp-m+NN', 1x, '  hen-m+NN', 1x, 'IND', &
     &  1x, ' Lambda', 1x, ' formb', 1X, '   Fl-qm', 1x, '  Umfg-m', 1x, ' v-m/s', 1x, ' Q-cbm/s',//, &
     & T78, '   Q-ges', 1X, '   hv-m', 1X, '   hr-m',1X, '  h-bor', 1X, '  h-ein', 1X, '  h-ort', 1X,'     h-m'/)

END SUBROUTINE kopf
