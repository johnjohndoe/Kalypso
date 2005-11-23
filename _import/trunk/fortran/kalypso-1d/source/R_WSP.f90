!     Last change:  WP   30 May 2005   11:17 am
!--------------------------------------------------------------------------
! This code, R_WSP.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - reih_inv
! - rsymbole
! - rdatplt
! - rkanal
! - readentl
! - readbauw
! - readbem
! - rtrapez
! - rkreis
! - readei
! - readmaul
! - rlinetxt
! - rhwerte
! - rstexte
! - readlcr
! - readcomment
! - rstring
! - querfeld
! - profil
! - eckwerte
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
SUBROUTINE reih_inv (max, n, x, y)

! ******************************************************************
! Programmbeginn
! ******************************************************************

DIMENSION x (max), y (max)

DO k = 1, n / 2
  h = x (k)
  x (k) = x (n - k + 1)
  x (n - k + 1) = h
  h = y (k)
  y (k) = y (n - k + 1)
  y (n - k + 1) = h
END DO

END SUBROUTINE reih_inv



! ===========================================================
SUBROUTINE rsymbole (j, n, nfil, nnp)

!WP 01.02.2005
USE DIM_VARIABLEN

! COMMON-Block /DATH/ --------------------------------------------------------------
INTEGER :: lityp (idim2), nzeiyl (idim2), nzeist (idim2)
INTEGER :: nzeiho (idim2), nzeisf (idim2), nzeiyk (idim2)
INTEGER :: ntextl (idim2), ntextg (idim2), nsymb (idim2)
COMMON / dath / lityp, nzeiyl, nzeist, nzeiho, nzeisf, nzeiyk, ntextl, ntextg, nsymb
! ----------------------------------------------------------------------------------
                                                                        
! ===========================================================
IF (nsymb (j) .eq.1) call rdatplt (j, n, nfil)
IF (nsymb (j) .eq.2) call rkanal (j, n, nfil)
IF (nsymb (j) .eq.3) call readentl (j, n, nfil)
IF (nsymb (j) .eq.4) call readbauw (j, n, nfil)
IF (nsymb (j) .eq.5) call readbem (j, n, nfil)
IF (nsymb (j) .eq.6) call rtrapez (j, n, nfil)
IF (nsymb (j) .eq.7) call rkreis (j, n, nfil)
IF (nsymb (j) .eq.8) call readei (j, n, nfil)
IF (nsymb (j) .eq.9) call readmaul (j, n, nfil)
IF (nsymb (j) .eq.10) call rlinetxt (j, n, nfil)
IF (nsymb (j) .eq.11) call rhwerte (j, n, nfil)
IF (nsymb (j) .eq.12) call rstexte (j, n, nfil)
IF (nsymb (j) .eq.13) call readlcr (j, n, nfil)
IF (nsymb (j) .eq.17) call readcomment (j, n, nfil, nnp)

RETURN
END SUBROUTINE rsymbole                                 
! ===========================================================




! ===========================================================
SUBROUTINE rdatplt (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

! COMMON-Block /DAT1/ --------------------------------------------------------------
CHARACTER(LEN=nch80) 	:: satz (idim, idim2)
INTEGER 		:: imax (idim)
COMMON / dat1 / satz, imax
! ----------------------------------------------------------------------------------

l = 0

10 CONTINUE

l = l + 1 
READ (nfil, '(a)') satz (l, j)

!CALL u0ljst (satz (l, j) )
satz (l,j) = ADJUSTL ( satz(l, j) )

CALL ucase (satz (l, j) )
IF (satz (l, j) (:3) .eq.'XXX') then
  imax (j) = l - 1
  RETURN
ENDIF
GOTO 10
                                                                        
END SUBROUTINE rdatplt
                                                                        



! ===========================================================
SUBROUTINE rkanal (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

INTEGER bezok (idim2), bezuk (idim2), ok, uk, anzok, anzuk, anzkanal
COMMON / kan1 / bezok, bezuk, ok, uk, anzok, anzuk, anzkanal
                                                                        
CHARACTER(128) string
CHARACTER(30) words (50)
                                                                        
      ilauf = 0 
      n = n + 1 
      jj = 0 
                                                                        
      READ (nfil, * ) ok, anzok, uk, anzuk, anzkanal 
      IF (anzkanal.eq.0) return 
                                                                        
   10 n = n + 1 
                                                                        
      READ (nfil, '(a)') string 
      CALL rstring (string, iword, words) 
      ilauf = ilauf + iword 
      IF (ilauf.gt.anzkanal) goto 50 
      DO 20 i = 1, iword 
        jj = jj + 1 
        READ (words (i) , '(i30)') bezok (jj) 
   20 END DO 
      IF (jj.lt.anzkanal) goto 10 
                                                                        
      ilauf = 0 
      jj = 0 
                                                                        
   30 n = n + 1 
      READ (nfil, '(a)') string 
      CALL rstring (string, iword, words) 
      ilauf = ilauf + iword 
      IF (ilauf.gt.anzkanal) goto 50 
      DO 40 i = 1, iword 
        jj = jj + 1 
        READ (words (i) , '(i30)') bezuk (jj) 
   40 END DO 
      IF (jj.lt.anzkanal) goto 30 
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
      END SUBROUTINE rkanal                         
                                                                        
!     ===========================================================       


! ===========================================================
SUBROUTINE readentl (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

INTEGER dl, anzl, is (idim2), ipunkt (idim2), anzdl 
COMMON / ent1 / dl, anzl, is, ipunkt, anzdl

CHARACTER(128) string
CHARACTER(30) words (50)
                                                                        
      ilauf = 0 
      jj = 0 
                                                                        
      n = n + 1 
      READ (nfil, * ) dl, anzl 
                                                                        
   10 n = n + 1 
      READ (nfil, '(a)') string 
      CALL rstring (string, iword, words) 
      ilauf = ilauf + iword 
      IF (ilauf.gt.2 * anzl) goto 30 
      DO 20 i = 1, iword, 2 
        jj = jj + 1 
        READ (words (i) , '(i30)') is (jj) 
        READ (words (i + 1) , '(i30)') ipunkt (jj) 
   20 END DO 
      IF (jj.lt.anzl) goto 10 
                                                                        
      RETURN 
                                                                        
   30 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
      END SUBROUTINE readentl                       
                                                                        
!     ===========================================================       


! ===========================================================
SUBROUTINE readbauw (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

INTEGER ibau, anzibau, anzbau, punktbau (idim)
REAL hbau (idim), bbau (idim), lbau (idim)
COMMON / bau1 / ibau, anzibau, anzbau, punktbau, hbau, bbau, lbau
                                                                        
                                                                        
      n = n + 1 
      READ (nfil, *, err = 40) ibau, anzbau 
      DO 10 i = 1, anzbau 
        n = n + 1 
        READ (nfil, *, err = 40) punktbau (i), hbau (i), bbau (i),      &
        lbau (i)                                                        
   10 END DO 
      RETURN 
                                                                        
   40 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
      END SUBROUTINE readbauw                       
                                                                        
! ===========================================================



! ===========================================================
SUBROUTINE readbem (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

! COMMON-Block /BEM11/ -- FUER BEMASSUNG -------------------------------------------
CHARACTER(LEN=nch80) 	:: textbem (idim, 4)
INTEGER 		:: ibem, anzbem, p1bem (idim), p2bem (idim)
REAL 			:: hbem (idim)
COMMON / bem11 / ibem, anzbem, p1bem, p2bem, textbem, hbem
! ----------------------------------------------------------------------------------

      n = n + 1 
      READ (nfil, *, err = 50) ibem, anzbem 
      DO 10 i = 1, anzbem 
        n = n + 1 
        READ (nfil, *, err = 50) p1bem (i), p2bem (i), hbem (i) 
        READ (nfil, fmt = 40, err = 50) textbem (i, 1) 
        READ (nfil, fmt = 40, err = 50) textbem (i, 2) 
        READ (nfil, fmt = 40, err = 50) textbem (i, 3) 
        READ (nfil, fmt = 40, err = 50) textbem (i, 4) 
        n = n + 4 
   10 END DO 
      RETURN 
                                                                        
   40 FORMAT(a) 
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE readbem
                                                                        


! ===========================================================
SUBROUTINE rtrapez (j, n, nfil)

!WP 01.02.2005
USE DIM_VARIABLEN


! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), nprof, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, nprof, np
! ----------------------------------------------------------------------------------



                                                                        
      REAL dtra, htra, stra, sgtra, xtra, ytra 
      COMMON / tra1 / dtra, htra, stra, sgtra, xtra, ytra 
                                                                        
                                                                        
                                                                        
      n = n + 1 
      READ (nfil, *, err = 50) dtra, htra, stra, sgtra, x (1, j),       &
      y (1, j)                                                          
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE rtrapez
!     ===========================================================


!-----------------------------------------------------------------------
SUBROUTINE rkreis (j, n, nfil)

! Letzte Aenderung WP 11.01.2005

! Hier werden die Werte aus dem Block KREIS eingelesen
! und den globalen Variablen DKREIS, SGKREIS, X(i,j) und Y(i,j)
! zugewiesen.

!WP 01.02.2005
USE DIM_VARIABLEN

! Calling variables
INTEGER, INTENT(IN) :: j, nfil
INTEGER, INTENT(INOUT) :: n

! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), nprof, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, nprof, np
! ----------------------------------------------------------------------------------


REAL :: dkreis, sgkreis, xkreis, ykreis
COMMON / kre1 / dkreis, sgkreis, xkreis, ykreis

! Local varibales
INTEGER :: istat

n = n + 1
READ (nfil, *, IOSTAT=istat) dkreis, sgkreis, x (1, j), y (1, j)
if (istat /= 0 ) then
  write (*,50) j, n
  stop
  50 format (/1X, 'Fehler in Datensatz = ', I3, ', Zeile = ', I3, '!', /, &
            & 1X, 'Programm wird beendet!')
end if

END SUBROUTINE rkreis
!-----------------------------------------------------------------------



! ===========================================================
SUBROUTINE readei (j, n, nfil)

!WP 01.02.2005
USE DIM_VARIABLEN
                                                                        

! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), nprof, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, nprof, np
! ----------------------------------------------------------------------------------
                                                                        
                                                                        
      REAL dei, hei, sgei, xei, yei 
      COMMON / eip1 / dei, hei, sgei, xei, yei 
                                                                        
                                                                        
      n = n + 1 
      READ (nfil, *, err = 50) dei, hei, sgei, x (1, j), y (1, j) 
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE readei
! ===========================================================


! ===========================================================
SUBROUTINE readmaul (j, n, nfil)

!WP 01.02.2005
USE DIM_VARIABLEN

REAL dmaul, hmaul, sgmaul, xmaul, ymaul
COMMON / mau1 / dmaul, hmaul, sgmaul, xmaul, ymaul
                                                                        
! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), nprof, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, nprof, np
! ----------------------------------------------------------------------------------

                                                                        
      n = n + 1 
      READ (nfil, *, err = 50) dmaul, hmaul, sgmaul, x (1, j), y (1, j) 
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE readmaul
! ===========================================================


! ===========================================================
SUBROUTINE rlinetxt (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
                                                                        
CHARACTER(30) lintex (10, idim)
INTEGER ilintex (10), ianztext (10), imaxtl, ipt (10, idim), ltfla
COMMON / lin1 / ilintex, ianztext, lintex, ipt, imaxtl, ltflag
                                                                        
!     ==================================================================
                                                                        
      CHARACTER(128) string, tstring 
      CHARACTER(30) words (50) 
                                                                        
      n = n + 1 
      imaxtl = imaxtl + 1 
      READ (nfil, *, err = 50) ilintex (imaxtl), ianztext (imaxtl) 
                                                                        
      DO 10 i = 1, ianztext (imaxtl) 
        n = n + 1 
        READ (nfil, '(a)') string 
        tstring = string 
        CALL rstring (string, iword, words) 
        IF (iword.lt.2) goto 50 
        READ (words (1) , '(i30)') ipt (imaxtl, i) 

        !CALL u0ljst (tstring)
        tstring = ADJUSTL (tstring)

        !ilen = ju0nch (tstring)
        ilen = LEN_TRIM (tstring)
        ii = index (tstring, ' ') 
        string = tstring (ii:ilen) 

        !CALL u0ljst (string)
        string = ADJUSTL (string)

        lintex (imaxtl, i) = string 
   10 END DO 
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE rlinetxt
! ===========================================================


! ===========================================================
SUBROUTINE rhwerte (j, n, nfil)

!WP 01.02.2005
USE DIM_VARIABLEN
                                                                        
INTEGER ihw (10), ianzhw (10), ishw (idim, 10), imaxhw, hwflag
COMMON / hwe1 / ihw, ianzhw, ishw, imaxhw, hwflag

CHARACTER(128) string 
CHARACTER(30) words (50)
                                                                        
      jj = 0 
      ilauf = 0 
      n = n + 1 
      imaxhw = imaxhw + 1 
                                                                        
      READ (nfil, *, err = 50) ihw (imaxhw), ianzhw (imaxhw) 
                                                                        
   10 n = n + 1 
      READ (nfil, '(a)') string 
      CALL rstring (string, iword, words) 
      ilauf = ilauf + iword 
      IF (ilauf.gt.ianzhw (imaxhw) ) goto 50 
      DO 20 i = 1, iword 
        jj = jj + 1 
        READ (words (i) , '(i30)') ishw (jj, imaxhw) 
   20 END DO 
      IF (jj.lt.ianzhw (imaxhw) ) goto 10 
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE rhwerte
! ===========================================================


! ===========================================================
SUBROUTINE rstexte (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
                                                                        
CHARACTER(30) stx (10, 100, 10)
INTEGER istx (10), ianzstx (10), ijustx (100, 10), xbezstx (100, 10)
INTEGER itx (100, 10), iarttx (100, 10), ijusart (100, 10), imaxstx
INTEGER ihightx (100, 10), linflag, dlinflag, elinflag, stflag
REAL ybezstx (10)

      COMMON / son1 / istx, ianzstx, ijustx, xbezstx, itx, iarttx,      &
      ijusart, ihightx, stx, ybezstx, imaxstx, linflag, elinflag,       &
      dlinflag, stflag                                                  
                                                                        
!     ==================================================================
                                                                        
      n = n + 1 
      imaxstx = imaxstx + 1 
      READ (nfil, *, err = 50) istx (imaxstx), ianzstx (imaxstx),       &
      ybezstx (imaxstx)                                                 
                                                                        
      DO 10 i = 1, ianzstx (imaxstx) 
        n = n + 1 
        READ (nfil, *, err = 50) itx (i, imaxstx), iarttx (i, imaxstx), &
        ijusart (i, imaxstx), ijustx (i, imaxstx), xbezstx (i, imaxstx),&
        ihightx (i, imaxstx)                                            
        DO 20 jj = 1, itx (i, imaxstx) 
          n = n + 1 
          READ (nfil, '(a)', err = 50) stx (jj, i, imaxstx) 
   20   END DO 
   10 END DO 
                                                                        
      RETURN 
                                                                        
   50 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE rstexte
! ===========================================================



! ===========================================================
SUBROUTINE readlcr (j, n, nfil)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN

CHARACTER(30) lcrtext (idim)
INTEGER ilcr
REAL xlcr1 (idim), xlcr2 (idim), ylcr1 (idim), ylcr2 (idim)
COMMON / lcr1 / ilcr, xlcr1, xlcr2, ylcr1, ylcr2, lcrtext
                                                                        
                                                                        
      n = n + 1 
      READ (nfil, *, err = 40) ilcr 
                                                                        
      DO 10 i = 1, ilcr 
        READ (nfil, *, err = 40) xlcr1 (i), xlcr2 (i), ylcr1 (i),       &
        ylcr2 (i)                                                       
        READ (nfil, '(a)', err = 40) lcrtext (i) 
   10 END DO 
      RETURN 
                                                                        
   40 PRINT * , ' ' 
      PRINT * , 'Fehler in Datensatz = ', j, ' Zeile = ', n, '!!' 
      PRINT * , ' ' 
      STOP 
END SUBROUTINE readlcr
                                                                        
                                                                        

!-----------------------------------------------------------------------
SUBROUTINE readcomment (j, n, nfil, nnp)
                                                                        
! Letzte Aenderung WP 19.10.2004

! READCOMMENT liest den Kommentarblock in der Profildatei.
! Dabei kann es zu Problemen kommen, wenn deutsche Sonderzeichen
! vorkommen (z.B. Brücke). Diese werden durch eine IOSTAT-Abfrage
! in der READ-Anweisung abgefangen.

implicit none

! Calling variables
INTEGER, INTENT(IN) :: j	! Nummer des Datensatzes
INTEGER, INTENT(INOUT) :: n	! Zeile des Datensatzes
INTEGER, INTENT(IN) :: nfil	! Unit der zu lesenden Datei
INTEGER, INTENT(IN) :: nnp      ! Anzahl der zu lesenden Zeilen

! Local variables
CHARACTER(LEN=80) :: text       ! Text in Zeile wird hier nur temporaer gespeichert
INTEGER :: i                    ! Zaehlvariable fuer Schleife
INTEGER :: istat
                                                                        

!write (*,*) 'READCOMMENT:'
!write (*,*) 'Nummer des Datensatzes j: ', j
!write (*,*) 'Zeile des Datensatzes n: ', n
!write (*,*) 'Anzahl der zu lesenden Zeilen nnp: ', nnp


DO i = 1, nnp

  n = n + 1

  !WP 19.10.2004: Verwendung von IOSTAT in READ-Anweisung statt ERR!
  READ (nfil, *, IOSTAT=istat) text
  if (istat /= 0) then
    write (*,1000)
  end if
  !WP
  !write (*,*) text

end do

1000 format (/1X, 'Problem beim Lesen der Kommentarzeile(n)!', /, &
            & 1X, 'Wurden eventl. deutsche Sonderzeichen eingegeben?', /, &
            & 1X, '-> Programm wird fortgesetzt!', /)

end subroutine readcomment
!-------------------------------------------------------------------------------------
                                                                        


!-------------------------------------------------------------------------------------
SUBROUTINE rstring (string, iword, words)
                                                                        
! Last change Wolf Ploeger, 11.01.2005

! Die Subroutine RSTRING teilt die Variable STRING
! in einzelne Character-Strings auf. Hierbei werden
! Leerzeichen als Trenner verwendet. Die Anzahl der
! erzeugten strings wird in iword abgelegt.
! Z.B. string   = "0  0  0  0"
!  ->  iwords   = 4
!      words(1) = "0"
!      words(2) = "0"
!      words(3) = "0"
!      words(4) = "0"

! Calling variables
CHARACTER(*), INTENT(INOUT) :: string
INTEGER, INTENT(OUT) :: iword
CHARACTER(LEN=30), INTENT(OUT) :: words (50)

! Local variables
INTEGER :: ilen, i
                                                                        
iword = 0

DO i = 1, 30
  words (i) = ' '
END DO
                                                                        
10 CONTINUE

!CALL u0ljst (string)    ! Loescht alle linksbuendigen Leerzeichen von string
string = ADJUSTL (string)

!ilen = ju0nch (string)   ! Ersetzen der Funktion JU0NCH durch LEN_TRIM! WP 11.01.05
ilen = LEN_TRIM(string)

IF (ilen.eq.0) return
                                                                        
!ianf = 1                  ! Keine Funktion

DO 20 i = 1, ilen 
                                                                        
  IF (string (i:i) .eq.' ') then
    iword = iword+1
    words (iword) = string (1:i - 1)
    string = string (i:ilen)
    GOTO 10
  ENDIF
  IF (i.eq.ilen) then
    iword = iword+1
    words (iword) = string (1:ilen)
  ENDIF

20 END DO
                                                                        
END SUBROUTINE rstring
!-------------------------------------------------------------------------------------



! ===========================================================
SUBROUTINE querfeld (iword, words)
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN                                                                        

! Calling variables
INTEGER, INTENT(IN)           :: iword
CHARACTER(LEN=30), INTENT(IN) :: words (50)


! COMMON-Block /HEADER/ ------------------------------------------------------------
! Fuer die Headerinformation
CHARACTER(LEN=nch80) 	:: text0
CHARACTER(LEN=36)    	:: text1, text11, text2, text22, text23, text3, text32
CHARACTER(LEN=36)    	:: text33, text7
CHARACTER(LEN=16)    	:: text4
CHARACTER(LEN=20)    	:: text5
CHARACTER(LEN=12)    	:: text6, text66
INTEGER 		:: nsteu1, nsteu2, nsteu3, nsteu4, nsteu5, nsteu6, nquer (3)
CHARACTER(LEN=8)     	:: textq (3, 2)
REAL 			:: yo, gx, gy, bzgh, xmass, ymass
INTEGER 		:: ntei, itei (idim2), isatz
INTEGER 		:: ilegende, leerflag, luftflag
COMMON / header / text0, text1, text11, text2, text22, text23,    &
       & text3, text32, text33, text7, text4, text5, text6, text66, nsteu1,&
       & nsteu2, nsteu3, nsteu4, nsteu5, nsteu6, nquer, textq, yo, gx, gy, &
       & bzgh, xmass, ymass, ntei, itei, isatz, ilegende, leerflag,        &
       & luftflag
! ----------------------------------------------------------------------------------

                                                                        

                                                                        
      DO 100 i1 = 1, 3 
        textq (i1, 1) = ' ' 
        textq (i1, 2) = ' ' 
!        textq(i1,3) = ' '                                              
  100 END DO 
      text66 = words (1) 
      READ (words (2) , '(i30)') nsteu3 
      READ (words (3) , '(i30)') nsteu5 
      READ (words (4) , '(i30)') nsteu6 
      IF (nsteu6.ne.0) then 
        READ (words (5) , '(i30)') nquer (1) 
        READ (words (6) , '(i30)') nquer (2) 
        READ (words (7) , '(i30)') nquer (3) 
        ii = 8 
        DO 10 i = 1, nsteu6 
                                                                        
          xreal = REAL (nquer (i) ) 
          pr = amod (xreal, 10.0) 
          IF (pr.gt.0) then 
            textq (i, 1) = words (ii) 
            ii = ii + 1 
          ENDIF 
          IF (pr.gt.1) then 
            textq (i, 2) = words (ii) 
            ii = ii + 1 
          ENDIF 
   10   END DO 
        nquer (1) = nint (nquer (1) / 10.0) 
        nquer (2) = nint (nquer (2) / 10.0) 
        nquer (3) = nint (nquer (3) / 10.0) 
      ENDIF 
      RETURN 
END SUBROUTINE querfeld
! ===========================================================
                                                                        

!-----------------------------------------------------------------------
SUBROUTINE profil (iword, words) 

! Letzte Aenderung WP 11.01.2005

! Die Werte aus iwords und words werden der Anzahl der Blöcke
! NPROF und der Anzahl der Zeilen in den Blöcken NP(i)
! zugewiesen.

! Folgende Werte muessen zugewiesen werden, weil NPROF und NP(idim2)
! Teil eines COMMON-Blocks sind!
!WP 01.02.2005
USE DIM_VARIABLEN

! Calling variables
INTEGER, INTENT(IN) :: iword
CHARACTER(LEN=30), INTENT(IN) :: words (50)


! COMMON-Block /DATLIN1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: bete1 (idim2), bete12 (idim2)
COMMON / datlin1 / bete1, bete12
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), nprof, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, nprof, np
! ----------------------------------------------------------------------------------


READ (words (1) , '(i30)') nprof

DO i = 2, iword
  READ (words (i) , '(i30)') np (i - 1)
END DO

END SUBROUTINE profil
!-----------------------------------------------------------------------




!-----------------------------------------------------------------------
SUBROUTINE eckwerte (iword, words)

! Letzte Aenderung WP 18.05.2005

! Diese Subroutine weist den Variablen YO, GX, BZGH, XMASS, YMASS, GY und NTEI
! die in WORDS(i) eingelesenen Werte zu. Diese beschreiben die Darstellung des
! Profils im WSPWin Plotter. So gibt z.B. XMASS und YMASS den Massstab des Profil
! in der Zeichnung an.
! Fuer die eigentliche Berechnung sind die Werte irrelevant!
! Im Normalfall enthalten diese Werte alle 0.000 bzw. 0.

!WP 01.02.2005
USE DIM_VARIABLEN

! Calling variables
INTEGER, INTENT(IN)           :: iword
CHARACTER(LEN=30), INTENT(IN) :: words (50)



! COMMON-Block /HEADER/ ------------------------------------------------------------
! Fuer die Headerinformation
CHARACTER(LEN=nch80) 	:: text0
CHARACTER(LEN=36)    	:: text1, text11, text2, text22, text23, text3, text32
CHARACTER(LEN=36)    	:: text33, text7
CHARACTER(LEN=16)    	:: text4
CHARACTER(LEN=20)    	:: text5
CHARACTER(LEN=12)    	:: text6, text66
INTEGER 		:: nsteu1, nsteu2, nsteu3, nsteu4, nsteu5, nsteu6, nquer (3)
CHARACTER(LEN=8)     	:: textq (3, 2)
REAL 			:: yo, gx, gy, bzgh, xmass, ymass
INTEGER 		:: ntei, itei (idim2), isatz
INTEGER 		:: ilegende, leerflag, luftflag
COMMON / header / text0, text1, text11, text2, text22, text23,    &
       & text3, text32, text33, text7, text4, text5, text6, text66, nsteu1,&
       & nsteu2, nsteu3, nsteu4, nsteu5, nsteu6, nquer, textq, yo, gx, gy, &
       & bzgh, xmass, ymass, ntei, itei, isatz, ilegende, leerflag,        &
       & luftflag
! ----------------------------------------------------------------------------------



READ (words (1) , '(f30.2)', err = 9110) yo
READ (words (2) , '(f30.2)', err = 9110) gx
READ (words (3) , '(f30.2)', err = 9110) bzgh
READ (words (4) , '(f30.2)', err = 9110) xmass
READ (words (5) , '(f30.2)', err = 9110) ymass
READ (words (6) , '(f30.2)', err = 9110) gy
READ (words (7) , '(i30)', err = 9110) ntei

DO i = 1, ntei
  READ (words (i + 7) , '(i30)') itei (i)
END DO
RETURN                                                              
                                                                        
9110 PRINT * , 'Formatfehler im Datenkopf-Satz, Zeile 15!!'
     STOP 'Programmabbruch in SUBROUTINE PROEIN_PRO (ECKWERTE)'
                                                                        
END SUBROUTINE eckwerte
!-----------------------------------------------------------------------

     
