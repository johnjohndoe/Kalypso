!     Last change:  MD    4 May 2007   11:08 am
!--------------------------------------------------------------------------
! This code, read_kalypso_config_file.f90, contains the following
! subroutines and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - read_kalypso_config_file
! - get_data_line
! - GET_VALUE_REAL
! - GET_VALUE_INTEGER
!
! Copyright (C) 2006  WOLF PLOEGER.
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
! Wolf Ploeger, 18 August 2004
! Research Associate
!--------------------------------------------------------------------------



!--------------------------------------------------------------------------
subroutine read_config_file(filename_config,len_filename)

USE MOD_INI
USE IO_UNITS
USE DIM_VARIABLEN

implicit none

! Calling variables
INTEGER, INTENT(IN) :: len_filename
CHARACTER(LEN=len_filename), INTENT(IN) :: filename_config

! Local variables
INTEGER :: istat, pos_eq, len_line, ju0gfu
CHARACTER(LEN=nch80) :: line


UNIT_EIN_INI = ju0gfu()

open (UNIT=UNIT_EIN_INI, FILE=filename_config, STATUS='OLD', ACTION='READ',IOSTAT=istat)
if (istat/=0) then
  write (*,*) 'Error opening ', filename_config
  stop
end if

read_ini: do

  read (UNIT=UNIT_EIN_INI,FMT=1000, IOSTAT=istat) line
  1000 format (A120)
  if (istat/=0) then
    exit read_ini
  end if

  call get_data_line(line)

end do read_ini

close (UNIT_EIN_INI)

end subroutine read_config_file



! ------------------------------------------------------------
subroutine get_data_line(line)

USE DIM_VARIABLEN
USE MOD_INI

implicit none

! Calling parameters
CHARACTER(LEN=nch80), INTENT(IN) :: line

! Local parameters
INTEGER :: i, istat, pos_eq, len_line
CHARACTER(LEN=nch80) :: part1, part2

! Called Function
REAL 	:: GET_VALUE_REAL
INTEGER :: GET_VALUE_INTEGER

! Check for comment line
if (line(1:1) == '#') return
if (line(1:1) == '!') return

pos_eq = INDEX(line,'=')
if (pos_eq == 0) return

len_line = LEN_TRIM(line)

part1 = line(1:pos_eq-1)
part2 = line(pos_eq+1:len_line)

!write (*,1000) part1, part2
!1000 format (/1X, '-----------------------------------------', /, &
!            & 1X, 'PART1 = ', A, /, &
!            & 1X, 'PART2 = ', A)

if (INDEX(part1,'PROJEKTPFAD')/=0) then
  PROJEKTPFAD = ADJUSTL(part2)

else if (INDEX(part1,'STRANGDATEI')/=0) then
  STRANGDATEI = ADJUSTL(part2)

else if (INDEX(part1,'BERECHNUNGSMODUS')/=0) then

  if (INDEX(part2,'WATERLEVEL')/=0) then
    BERECHNUNGSMODUS = 'WATERLEVEL'
  else if (INDEX(part2,'BF_UNIFORM')/=0) then
    BERECHNUNGSMODUS = 'BF_UNIFORM'
  else if (INDEX(part2,'BF_NON_UNIFORM')/=0) then
    BERECHNUNGSMODUS = 'BF_NON_UNI'
  !MD  neue Berechnungsvarainte mit konstanten Reibungsgefaelle
  else if (INDEX(part2,'REIB_KONST')/=0) then
    BERECHNUNGSMODUS = 'REIB_KONST'
  else
    write (*,*) 'Keine Zuweisung fuer BERECHNUNGSMODUS. Abbruch.'
    stop
  end if

else if (INDEX(part1,'FLIESSGESETZ')/=0) then

  if (INDEX(part2,'DARCY_WEISBACH_MIT_FORM')/=0) then
    FLIESSGESETZ = 'DW_M_FORMBW'
  else if (INDEX(part2,'DARCY_WEISBACH_OHNE_FORM')/=0) then
    FLIESSGESETZ = 'DW_O_FORMBW'
  else if (INDEX(part2,'MANNING_STRICKLER')/=0) then
    FLIESSGESETZ = 'MANNING_STR'
  else
    write (*,*) 'Keine Zuweisung fuer FLIESSGESETZ. Abbruch.'
    stop
  end if

else if (INDEX(part1,'ANFANGSSTATION')/=0) then
  ANFANGSSTATION = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'ENDSTATION')/=0) then
  ENDSTATION = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'ART_RANDBEDINGUNG')/=0) then

  if (INDEX(part2,'CRITICAL_WATER_DEPTH')/=0) then
    ART_RANDBEDINGUNG = 'CRITICAL_WATER_DEPTH'
  else if (INDEX(part2,'UNIFORM_BOTTOM_SLOPE')/=0) then
    ART_RANDBEDINGUNG = 'UNIFORM_BOTTOM_SLOPE'
  else if (INDEX(part2,'WATERLEVEL')/=0) then
    ART_RANDBEDINGUNG = 'WATERLEVEL          '
  else
    write (*,*) 'Keine Zuweisung fuer ART_RANDBEDINGUNG. Abbruch.'
    stop
  end if

else if (INDEX(part1,'ANFANGSWASSERSPIEGEL')/=0) then
  ANFANGSWASSERSPIEGEL = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'GEFAELLE')/=0) then
  GEFAELLE = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'VERZOEGERUNGSVERLUST')/=0) then

  if (INDEX(part2,'DVWK')/=0) then
    VERZOEGERUNGSVERLUST = 'DVWK'
  else if (INDEX(part2,'BJOERNSEN')/=0) then
    VERZOEGERUNGSVERLUST = 'BJOE'
  else if (INDEX(part2,'DFG')/=0) then
    VERZOEGERUNGSVERLUST = 'DFG '
  else if (INDEX(part2,'BWK')/=0) then
    VERZOEGERUNGSVERLUST = 'BWK '
  !MD  neue Berechnungsvarainte mit konstanten Reibungsgefaelle
  else if (INDEX(part2,'NON')/=0) then
    VERZOEGERUNGSVERLUST = 'NON '
  else
    write (*,*) 'Keine Zuweisung fuer VERZOEGERUNGSVERLUST. Abbruch.'
    stop
  end if

else if (INDEX(part1,'ITERATIONSART')/=0) then

  if (INDEX(part2,'SIMPLE')/=0) then
    ITERATIONSART = 'SIMPLE'
  else if (INDEX(part2,'EXACT')/=0) then
    ITERATIONSART = 'EXACT '
  else
    write (*,*) 'Keine Zuweisung fuer ITERATIONSART. Abbruch.'
    stop
  end if

else if (INDEX(part1,'REIBUNGSVERLUST')/=0) then

  if (INDEX(part2,'TRAPEZ_FORMULA')/=0) then
    REIBUNGSVERLUST = 'TRAPEZ'
  else if (INDEX(part2,'GEOMETRIC_FORMULA')/=0) then
    REIBUNGSVERLUST = 'GEOMET'
  else
    write (*,*) 'Keine Zuweisung fuer REIBUNGSVERLUST. Abbruch.'
    stop
  end if

else if (INDEX(part1,'MIT_BRUECKEN')/=0) then

  if (INDEX(part2,'false')/=0) then
    MIT_BRUECKEN = .false.
  else if (INDEX(part2,'true')/=0) then
    MIT_BRUECKEN = .true.
  else
    write (*,*) 'Keine Zuweisung fuer MIT_BRUECKEN. Abbruch.'
    stop
  end if

else if (INDEX(part1,'MIT_WEHREN')/=0) then
  if (INDEX(part2,'false')/=0) then
    MIT_WEHREN = .false.
  else if (INDEX(part2,'true')/=0) then
    MIT_WEHREN = .true.
  else
    write (*,*) 'Keine Zuweisung fuer MIT_WEHREN. Abbruch.'
    stop
  end if

else if (INDEX(part1,'ABFLUSSEREIGNIS')/=0) then
  ABFLUSSEREIGNIS = ADJUSTL(part2)

else if (INDEX(part1,'EINZELVERLUSTE')/=0) then
  EINZELVERLUSTE = ADJUSTL(part2)

else if (INDEX(part1,'MIN_Q')/=0) then
  MIN_Q = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'MAX_Q')/=0) then
  MAX_Q = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'DELTA_Q')/=0) then
  DELTA_Q = GET_VALUE_REAL(ADJUSTL(part2), LEN_TRIM(part2))

else if (INDEX(part1,'DURCHFLUSS_EINHEIT')/=0) then

  if (INDEX(part2,'QM_S')/=0) then
    DURCHFLUSS_EINHEIT = 'M'
  else if (INDEX(part2,'L_S')/=0) then
    DURCHFLUSS_EINHEIT = 'L'
  else
    DURCHFLUSS_EINHEIT = 'M'
  end if

else if (INDEX(part1,'USE_EXTREM_ROUGH')/=0) then
  if (INDEX(part2,'false')/=0) then
    USE_EXTREM_ROUGH = .false.
  else if (INDEX(part2,'true')/=0) then
    USE_EXTREM_ROUGH = .true.
  else
    USE_EXTREM_ROUGH = .false.
  end if

else if (INDEX(part1,'CALC_KM_INTERN')/=0) then
  if (INDEX(part2,'false')/=0) then
    CALC_KM_INTERN = .false.
  else if (INDEX(part2,'true')/=0) then
    CALC_KM_INTERN = .true.
  else
    CALC_KM_INTERN = .true.
  end if

end if

end subroutine get_data_line


! ------------------------------------------------------------
REAL function GET_VALUE_REAL(part2, length)

implicit none

! Calling variables
INTEGER, INTENT(IN) :: length
CHARACTER(LEN=length), INTENT(IN) :: part2

! Local variables
REAL :: temp
INTEGER :: ju0gfu, UNIT_SCRATCH

UNIT_SCRATCH = ju0gfu()

OPEN(UNIT_SCRATCH, STATUS='SCRATCH')
write (UNIT_SCRATCH, *) part2
REWIND(UNIT_SCRATCH)
READ(UNIT_SCRATCH,*) temp
close (UNIT_SCRATCH)

GET_VALUE_REAL = temp

end function GET_VALUE_REAL



! ------------------------------------------------------------
INTEGER function GET_VALUE_INTEGER(part2, length)

implicit none

! Calling variables
INTEGER, INTENT(IN) :: length
CHARACTER(LEN=length), INTENT(IN) :: part2

! Local variables
INTEGER :: temp, ju0gfu, UNIT_SCRATCH

UNIT_SCRATCH = ju0gfu()

OPEN(UNIT_SCRATCH, STATUS='SCRATCH')
write (UNIT_SCRATCH, *) part2
REWIND(UNIT_SCRATCH)
READ(UNIT_SCRATCH,*) temp
close (UNIT_SCRATCH)

GET_VALUE_INTEGER = temp

end function GET_VALUE_INTEGER



