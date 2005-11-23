!     Last change:  WP   30 May 2005   10:40 am
!--------------------------------------------------------------------------
! This code, ju0ch.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - ju0chr
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

SUBROUTINE ju0chr (string, real, ireal, char, ichara, int, iint, ifehl)                                                            
!
! Die subroutine ju0chr konvertiert einen character-string in
! real-werte,integer-werte und character-strings.
! dabei wird unterstellt, dasz die in dem character-string
! enthaltenen groeszen durch blanks oder * getrennt sind.
!
! aufgerufen in: subroutine intdat
!
! ------------------------------------------------------------------

USE DIM_VARIABLEN

! Calling variables
CHARACTER(LEN=nch80), INTENT(IN) :: string
REAL :: real(merg)
INTEGER :: ireal
CHARACTER(LEN=nch80) :: char ( * )
INTEGER :: ichara
INTEGER :: int ( * )
INTEGER :: iint
INTEGER, INTENT(OUT) :: ifehl

! Local variables
CHARACTER(LEN=4) :: ibuf
                                                                        


      !ilen = ju0nch (string)
      ilen = LEN_TRIM (string)
      IF (ilen.eq.0) then 
        ifehl = 1 
        GOTO 9999 
        !        string ist leer
      ENDIF 
      !      ianz = 0
      iint = 0 
      ireal = 0 
      ichara = 0 
      ifehl = 0 
      DO 1020 i2 = 1, ilen 
        IF (string (i2:i2) .ne.' ') then 
          GOTO 1025 
        ENDIF 
 1020 END DO 
                                                                        
 1025 iu = ichar ('0') 
      io = ichar ('9') 
      im = ichar ('.') 
      im2 = ichar ('-') 
      im3 = ichar ('+') 
      im4 = ichar ('e') 
      im5 = ichar ('e') 
      ianf = i2 
      DO 1030 i5 = ianf, ilen 
        IF (string (i5:i5) .eq.' ') then 
          ianf = ianf + 1 
          IF (ianf.gt.ilen) goto 9999 
          GOTO 1030 
        ENDIF 
        DO 1035 i3 = ianf, ilen 
          IF (string (i3:i3) .eq.' '.or.i3.eq.ilen) then 
            IF (string (i3:i3) .eq.' ') iend = i3 - 1 
            IF (i3.eq.ilen) iend = ilen 
            ibuf = 'inte' 
            DO 1040 i4 = ianf, iend 
              IF (string (i4:i4) .eq.' ') goto 1040 
              ik = ichar (string (i4:i4) ) 
              IF (ik.le.io.or.ik.eq.im4.or.ik.eq.im5) then 
                IF (ik.ge.iu.and.ik.ne.im4.and.ik.ne.im5) then 
                  GOTO 1040 
                ELSEIF (ik.eq.im.or.ik.eq.im2.or.ik.eq.im3) then 
                  ibuf = 'real' 
                  GOTO 1040 
                ELSEIF (ik.eq.im4.or.ik.eq.im5) then 
                  IF (i4.eq.iend) then 
                    ibuf = 'char' 
                    GOTO 200 
                  ELSE 
                    GOTO 1040 
                  ENDIF 
                ELSE 
                  ibuf = 'char' 
                  GOTO 200 
                ENDIF 
              ELSE 
                ibuf = 'char' 
                GOTO 200 
              ENDIF 
 1040       END DO 
  200       IF (ibuf.eq.'char') then 
              ichara = ichara + 1 
              char (ichara) = string (ianf:i3) 
              IF (ianf.gt.ilen) goto 9999 
                                                                        
            ELSE 
 1055         IF (string (ianf:ianf) .eq.'0'.and.string (ianf + 1:ianf +&
              1) .ne.' ') then                                          
                ianf = ianf + 1 
                IF (ianf.gt.i3) goto 1035 
                GOTO 1055 
              ENDIF 
              IF (ianf.gt.i3) goto 1035 
              IF (ibuf.eq.'inte') then 
                iint = iint + 1 
                READ (string (ianf:i3) , '(i10)') int (iint) 
              ELSE 
                ireal = ireal + 1 
                CALL ucase (string (ianf:i3) ) 
                READ (string (ianf:i3) , '(f15.7)') real (ireal) 
              ENDIF 
            ENDIF 
            ianf = i3 + 1 
            IF (ianf.gt.ilen) goto 9999 
 5555       IF (string (ianf:ianf) .eq.' ') then 
              ianf = ianf + 1 
              IF (ianf.gt.ilen) goto 9999 
              GOTO 5555 
            ENDIF 
          ENDIF 
 1035   END DO 
 1030 END DO 
                                                                        
 9999 RETURN 
      END SUBROUTINE ju0chr                         
