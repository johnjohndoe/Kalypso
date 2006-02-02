!     Last change:  WP    2 Feb 2006   11:38 am
!--------------------------------------------------------------------------
! This code, proe_pro.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - proe_pro
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





! -----------------------------------------------------------------------------------------
SUBROUTINE proe_pro (nfil, textkm, filename)
!
! Einlesen der Daten aus den Profildateien. Die Werte werden den
! globalen Variablen zugewiesen
!
! -----------------------------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN

INTEGER, INTENT(IN)  		:: nfil		! FORTRAN-Unit der Profildatei (aus WSPBER)
CHARACTER(LEN=36), INTENT(OUT)	:: textkm       !
CHARACTER(LEN=nch80), INTENT(IN):: filename     ! Name der Profildatei


! COMMON-Block /DAT1/   FUER DATPLT ------------------------------------------------
CHARACTER(LEN=nch80) 	:: satz (idim, idim2)
INTEGER 		:: imax (idim)
COMMON / dat1 / satz, imax
! ----------------------------------------------------------------------------------


! COMMON-Block /DATH/ --------------------------------------------------------------
INTEGER 		:: lityp (idim2), nzeiyl (idim2), nzeist (idim2)
INTEGER 		:: nzeiho (idim2), nzeisf (idim2), nzeiyk (idim2)
INTEGER 		:: ntextl (idim2), ntextg (idim2), nsymb (idim2)
COMMON / dath / lityp, nzeiyl, nzeist, nzeiho, nzeisf, nzeiyk, ntextl, ntextg, nsymb
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN1/ -----------------------------------------------------------
CHARACTER(LEN=nch80) :: bete1 (idim2), bete12 (idim2)
COMMON / datlin1 / bete1, bete12
! ----------------------------------------------------------------------------------


! COMMON-Block /DATLIN2/ -----------------------------------------------------------
REAL 		:: x (idim, ipro), y (idim, ipro)
INTEGER 	:: noprxw (idim, ipro), nopryw (idim, ipro), nprof, np (ipro)
COMMON / datlin2 / x, y, noprxw, nopryw, nprof, np
! ----------------------------------------------------------------------------------


! COMMON-Block /ENT1/ -- FUER ROHRENT- LUEFTUNG, -LEERUNG --------------------------
INTEGER 	:: dl                   ! Bezugslinie fuer das Symbol
INTEGER  	:: anzl                 ! Anzahl der Symbole
INTEGER  	:: is (idim2)           ! Steuerung des Symbols
INTEGER  	:: ipunkt (idim2)       ! Bezugspunkte fuer das Symbol
INTEGER  	:: anzdl
COMMON / ent1 / dl, anzl, is, ipunkt, anzdl
! ----------------------------------------------------------------------------------


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


! COMMON-Block /KAN1/ -- FUER KANALDARSTELLUNG -------------------------------------
INTEGER 	:: bezok (idim2), bezuk (idim2), ok, uk, anzok, anzuk, anzkanal
COMMON / kan1 / bezok, bezuk, ok, uk, anzok, anzuk, anzkanal
! ----------------------------------------------------------------------------------


! COMMON-Block /BAU1/ -- FUER BAUWERKE ---------------------------------------------
INTEGER 	:: ibau, anzibau, anzbau, punktbau (idim)
REAL 		:: hbau (idim), bbau (idim), lbau (idim)
COMMON / bau1 / ibau, anzibau, anzbau, punktbau, hbau, bbau, lbau
! ----------------------------------------------------------------------------------

                                                                        
! COMMON-Block /BEM11/ -- FUER BEMASSUNG -------------------------------------------
CHARACTER(LEN=nch80) 	:: textbem (idim, 4)
INTEGER 		:: ibem, anzbem, p1bem (idim), p2bem (idim)
REAL 			:: hbem (idim)
COMMON / bem11 / ibem, anzbem, p1bem, p2bem, textbem, hbem
! ----------------------------------------------------------------------------------



REAL :: dtra, htra, stra, sgtra, xtra, ytra
COMMON / tra1 / dtra, htra, stra, sgtra, xtra, ytra

! ================================================================
! ====== COMMONBLOCK 'tra1' FUER TRAPEZPROFIL ====================
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       dtra       Laenge der unteren Trapezseite
!       4.2       htra       Hoehe des Profils                          
!       4.3       stra       Steigungswinkel                            
!       4.4       sgtra      Sohlgefaelle                               
!       4.5       xtra       Stationswert des Profils                   
!       4.6       ytra       Hoehenwert des Profils                     
! ================================================================
! ================================================================
                                                                        
REAL :: dkreis, sgkreis, xkreis, ykreis
COMMON / kre1 / dkreis, sgkreis, xkreis, ykreis
                                                                        
! ================================================================
! ====== COMMONBLOCK 'kre1' FUER KREISPROFIL =====================
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       dkreis     Kreisdurchmesser                           
!       4.2       sgkreis    Sohlgefaelle                               
!       4.3       xkreis     Stationswert des Profils                   
!       4.4       ykreis     Hoehenwert des Profils                     
! ================================================================
! ================================================================
                                                                        
REAL :: dei, hei, sgei, xei, yei
COMMON / eip1 / dei, hei, sgei, xei, yei
                                                                        
! ================================================================
! ====== COMMONBLOCK 'eip1' FUER EIPROFIL ========================
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       dei        groesster Durchmesser                      
!       4.2       hei        Gesamthoehe                                
!       4.3       sgei       Sohlgefaelle                               
!       4.5       xei        Stationswert des Profils                   
!       4.6       yei        Hoehenwert des Profils                     
! ================================================================
! ================================================================
                                                                        
REAL :: dmaul, hmaul, sgmaul, xmaul, ymaul
COMMON / mau1 / dmaul, hmaul, sgmaul, xmaul, ymaul
                                                                        
! ================================================================
! ====== COMMONBLOCK 'mau1' FUER MAULPROFIL ======================
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       dmaul      groesster Durchmesser                      
!       4.2       hmaul      Gesamthoehe                                
!       4.3       sgmaul     Sohlgefaelle                               
!       4.5       xmaul      Stationswert des Profils                   
!       4.6       ymaul      Hoehenwert des Profils                     
! ================================================================
! ================================================================
                                                                        
CHARACTER(LEN=30) :: lintex (10, idim)
INTEGER :: ilintex (10), ianztext (10), imaxtl, ipt (10, idim), ltfla
COMMON / lin1 / ilintex, ianztext, lintex, ipt, imaxtl, ltflag
                                                                        
! ================================================================
! ====== COMMONBLOCK 'lin1' FUER LINIENTEXT ======================
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       ilintex    Bezugslinie fuer den Text                  
!       4.2       ianztext   Anzahl der Texte                           
!       5.1       ipt        Bezugspunkt fuer den Text                  
!       5.2       lintex     Text                                       
! ================================================================
! ================================================================
                                                                        
INTEGER :: ihw (10), ianzhw (10), ishw (idim, 10), imaxhw, hwflag
COMMON / hwe1 / ihw, ianzhw, ishw, imaxhw, hwflag
                                                                        
! ================================================================
! ====== COMMONBLOCK 'hwe1' FUER HOEHENWERTE AUF PROFILLINIE =====
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       ihw        Bezugslinie fuer den Hoehenwert            
!       4.2       ianzhw     Anzahl der Hoehenwerte                     
!       5.1       ishw       Steuerung des Symbols                      
! ================================================================
! ================================================================
                                                                        
CHARACTER(LEN=30) :: stx (10, 100, 10)
INTEGER :: istx (10), ianzstx (10), ijustx (100, 10), xbezstx (100, 10)
INTEGER :: itx (100, 10), iarttx (100, 10), ijusart (100, 10), imaxstx
INTEGER :: ihightx (100, 10), linflag, dlinflag, elinflag, stflag
REAL :: ybezstx (10)
COMMON / son1 / istx, ianzstx, ijustx, xbezstx, itx, iarttx,         &
       & ijusart, ihightx, stx, ybezstx, imaxstx, linflag, elinflag, &
       & dlinflag, stflag
                                                                        
! ================================================================
! ====== COMMONBLOCK 'hwe1' FUER SONDERTEXTE =====================
! ================================================================
!       Zeilennr  Variable   Bedeutung                                  
!       4.1       istx       Bezugslinie fuer den Sondertext            
!       4.2       ianzstx    Anzahl der Sondertexte                     
!       4.3       ybezstx    Darstellunghoehe                           
!       5.1       ixt        Anzahl der Textzeilen                      
!       5.2       iarttx     Steuerung Textrahmens                      
!       5.3       ijusart    Steuerung Rahmenjustierung                 
!       5.4       ijustx     Steuerung Textjustierung                   
!       5.5       xbezstx    Bezugspunkt fuer den Sondertext            
!       5.6       ihightx    Steuerung Textgroesse                      
!       6 ...     stx        Sondertext                                 
! ================================================================
! ================================================================
                                                                        

! COMMON-BLOCK /LCR1/ --------------------------------------------------------
! Fuer Bereichsmarierung (superlinie)
CHARACTER(LEN=30) :: lcrtext (idim)
INTEGER :: ilcr
REAL :: xlcr1 (idim), xlcr2 (idim), ylcr1 (idim), ylcr2 (idim)
COMMON / lcr1 / ilcr, xlcr1, xlcr2, ylcr1, ylcr2, lcrtext
!       Zeilennr  Variable   Bedeutung
!       4         ilcr       Anzahl der Symbole                         
!       5.1       xlcr1      xwert linke untere Ecke                    
!       5.2       ylcr1      ywert linke untere Ecke                    
!       5.3       xlcr2      xwert rechte obere Ecke                    
!       5.4       ylcr2      ywert rechte obere Ecke                    
!       6         lcrtext    Text                                       
! -----------------------------------------------------------------------------


CHARACTER(LEN=128) :: string
CHARACTER(LEN=30) :: words (50)
INTEGER :: test
LOGICAL :: flag, ex

CHARACTER(LEN=36) :: text321
COMMON / txt32 / text321

                                                                        
! ==================================================================
! Belegung
! ==================================================================
                                                                        
flag = .true.
test = 2
n = 0
isatz = 0
                                                                        

! ==================================================================
! einlesedaten (neuer bce-stempel)
! ==================================================================

! ==================================================================
! 1. Lese Stempeldaten
! ==================================================================
!      if (flag) then                                                   
                                                                        
test = 1
n = 0
isatz = 0
                                                                        
READ (nfil, '(a)') text0
READ (nfil, '(a)') text1
READ (nfil, '(a)') text11
READ (nfil, '(a)') text2
READ (nfil, '(a)') text22
READ (nfil, '(a)') text23
READ (nfil, '(a)') text3
READ (nfil, '(a)') text32
READ (nfil, '(a)') text33
READ (nfil, '(a)') text4
READ (nfil, '(a)') text5
n = n + 12

READ (nfil, '(a)') string
CALL rstring (string, iword, words)
!c      if(iword.lt.4) goto 3000
!c      call querfeld (iword,words)

n = n + 1
READ (nfil, '(a)') text7

n = n + 1
READ (nfil, '(a)') string             ! Anzahl Blocke, Anzahl Werte pro Block
                                      ! z.B. " 6 23  2  1  2  0  3"

CALL rstring (string, iword, words)   ! String wird aufgeteilt in einzelne Stücke
IF (iword.lt.2) goto 3000

CALL profil (iword, words)            ! In PROFIL werden die Werte aus RSTRING den Variablen
                                      ! NPROF (Anz. Bloecke) und NP(i) (Anz. Zeilen pro Block)

n = n + 1
READ (nfil, '(a)') string
CALL rstring (string, iword, words)
IF (iword.lt.7) goto 3000

CALL eckwerte (iword, words)          ! Nur fuer die Darstellung im WSPWin Plotter (Massstab usw.)



! ==================================================================
! 2. Lese Datensaetze
! ==================================================================

nn = 0

DO 75 j = 1, nprof                    ! NPROF: Anzahl der Bloecke

  isatz = isatz + 1
  READ (nfil, '(a)') bete1 (j)        ! Bezeichnungstext 1
  READ (nfil, '(a)') bete12 (j)       ! bezeichnungstext 2

  !WP 11.01.2005
  !write (*,*) 'BETE1(j):  ', bete1(j)
  !write (*,*) 'BETE12(j): ', bete12(j)
  !WP ende

  READ (nfil, '(a)') string           ! Zwischenzeile mit "0 0 0 0" usw.
  CALL rstring (string, iword, words)

  n = 3

  !WP
  !write (*,*) 'IWORD: ', iword
  !WP

  IF (iword .ne. 9) goto 3000         ! Falls in dem String NICHT neun Zahlen stehen
                                      ! liegt Sonderblock vor, daher Sprung zu 3000
  BACKSPACE (nfil)

  ! Lesen der Zeile mit den ganzen Nullen!
  ! Offenbar haben diese Nullen doch eine Bedeutung! fuer das Programm PROFBER?
  ! Allerdings nicht fuer Kalypso-1D!
  READ (nfil, * ) lityp (j), nzeiyl (j), nzeist (j), nzeiho (j),  &
                & nzeisf (j), nzeiyk (j), ntextl (j), ntextg (j), nsymb (j)

  nn = nn + 1
  nnp = np (nn)
  ilauf = 0
  jj = 0

  !WP
  !write (*,*) ' NN:      ', nn
  !write (*,*) ' NP(nn) : ', np(nn)
  !write (*,*) 'NSYMB(j) :', nsymb(j)
  !WP

  IF (nsymb (j) .ne. 0) then

    ! WP 18.04.2005
    ! Falls es sich NICHT um einen Kommentar (nsymb(j) = 17) handelt, ist es wohl ein
    ! Sonderprofil. Aus irgendeinem Grund muss dann der Bezeichnungstext BETE12
    ! nocheinmal eingelsen werden.
    if (nsymb(j) .ne. 17) then
      READ (nfil, '(a)') bete12 (j)
      backspace (nfil)
    end if

    CALL rsymbole (j, n, nfil, nnp) ! In Abhaengigkeit von NSYMB(j) werden
                                    ! die verschiedenen Einleseroutinen
                                    ! aufgerufen, z.B.: NSYMB(j) = 7 -> Kreisprofil

  ELSE                                                                  
                                                                        
    2010 CONTINUE

    n = n + 1 
    READ (nfil, '(a)') string
    CALL rstring (string, iword, words)

    ilauf = ilauf + iword
    !WP
    !write (*,*) ' STRING:   ', string
    !write (*,*) ' ILAUF:    ', ilauf
    !write (*,*) ' 2*NP(nn): ', (2*np(nn))
    !write (*,*) ' IWORD:    ', iword
    !WP ende
    IF (ilauf .gt. (2 * np(nn) ) .and. iword.ne.1 ) goto 3000
                                                                        
    DO i = 1, iword, 2
      jj = jj + 1
      READ (words (i) , '(i30)') noprxw (jj, j)
      READ (words (i + 1) , '(f30.2)') x (jj, j)
    END DO
                                                                        
    IF (jj .lt. np(nn) ) goto 2010

    ilauf = 0
    jj = 0
                                                                        
    2030 CONTINUE

    n = n + 1
                                                                        
    READ (nfil, '(a)') string
    CALL rstring (string, iword, words)

    ilauf = ilauf + iword
    !WP
    !write (*,*) ' STRING:   ', string
    !write (*,*) ' ILAUF:    ', ilauf
    !write (*,*) ' 2*NP(nn): ', (2*np(nn))
    !write (*,*) ' IWORD:    ', iword
    !WP ende
    IF (ilauf .gt. (2 * np (nn)) .and. iword .ne. 1) goto 3000

    DO 2040 i = 1, iword, 2

      jj = jj + 1
      READ (words (i) , '(i30)') nopryw (jj, j)
      READ (words (i + 1) , '(f30.2)', err = 2035) y (jj, j)
      GOTO 2040

      2035 CONTINUE

    2040 END DO
                                                                        
    IF (jj.lt.np (nn) ) goto 2030

  END IF
                                                                        
75 END DO
                                                                        
                                                                        
flag = .false.
                                                                        
3000 CONTINUE
                                                                        

IF (flag) then

  ilen = LEN_TRIM (filename)
  WRITE ( *, 3500) filename (:ilen), isatz, n
  3500 FORMAT (/1X, 'Fehler in der Datei ',  (a) , /, &
              & 1X, 'Datensatznr.: ', I3, ',  Zeile :', I3, ' !')
  STOP

ENDIF 
                                                                        
!       close(nfil)                                                     
textkm = text33
text321 = text32

END SUBROUTINE proe_pro
! =========================================================

