!     Last change:  MD    6 Jan 2010    2:15 pm
!--------------------------------------------------------------------------
! This code, drucktab.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - drucktab
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
!--------------------------------------------------------------------------



!--------------------------------------------------------------------------
SUBROUTINE drucktab (i, indmax, nz, jw5, nblatt, station, jw7, idr1, Q_Abfrage, nr_q_out)
!
! Programmbeschreibung
! --------------------
! In dieser Subroutine werden die Ergebnisfiles .tab und .wsl
! geoeffnet und die Ergebnisse in diesen Dateien ausgegeben.
!
! Die .tab-Datei dient als Tabellarische Darstellung der Ergebnisse.
! Die .wsl-Datei dient als Grundlage zur Erzeugung des Wasserspiegel-
! laengsschnittes mit dem Programm prolae.med (veraltet)
                                                                        

!HB   ***************************************************************** 
!HB   26.11.2001 - H.Broeker      (k.B.= keine Bedeutung in dieser SUB) 
!HB   ----------------------                                            
!HB   Aufruf der SUB ALPHA_DRU mit anschliessender Ausgabe in Datei     
!HB   Beiwerte.AUS (Ausgabe von Energiestrom- und Boussinesq-Beiwert)   
!HB                                                                     
!HB   alph_aus -- Projektpfad der Ausgabedatei Beiwerte.AUS (k.B.)      
!HB   alpha_ja -  Steuerparameter, ob Beiwerte.AUS erstellt werden soll 
!HB   *****************************************************************
                                                                        

!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS
USE MOD_ERG

! Calling variables
INTEGER, INTENT(IN) 	:: i        		! NPROF, also Nummer des aktuellen Profils
INTEGER, INTENT(IN) 	:: indmax   		! Anzahl der Rauhigkeitsabschnitte (LV=1, FS=2, RV=3)
INTEGER, INTENT(INOUT) 	:: nz       		! Anzahl der Zeilen im Ergebnisfile
INTEGER, INTENT(IN) 	:: jw5      		! UNIT der Ausgabedatei (Ergebnisfile)
INTEGER, INTENT(IN) 	:: nblatt   		! Anzahl der Seiten des Ergebnisausdrucks
REAL, INTENT(IN)    	:: station (maxger)    	! Stationierung in km
INTEGER, INTENT(IN) 	:: jw7              	! UNIT einer weiteren Ausgabedatei ??
CHARACTER(LEN=1)    	:: idr1             	! = 'j' falls eine Wasserstandsabflussbeziehung
                                        	! ausgegeben werden soll. Gesetzt in wsp.f90 oder QaWSP.f90

! COMMON-Block /ALPH_PF/ -----------------------------------------------------------
INTEGER 		:: nr_alph
CHARACTER(LEN=nch80)    :: alph_aus     ! Projektpfad fuer Beiwerte.AUS
COMMON / alph_pf / alph_aus, nr_alph
! ----------------------------------------------------------------------------------

!HB   Boussineq-Beiwertes/ ---------------------------------------------------------
INTEGER :: pn_alpha
REAL :: st_alpha, alpha_EW (maxkla), alpha_IW (maxkla), gesamt_a (maxkla)
COMMON / nr_alpha / pn_alpha, st_alpha, gesamt_a, alpha_EW, alpha_IW
! ----------------------------------------------------------------------------------

! COMMON-Block /AUSGABELAMBDA/ -----------------------------------------------------
REAL, DIMENSION(maxkla) :: lambda_teilflaeche
CHARACTER(LEN=nch80) 	:: lambdai
COMMON / ausgabelambda / lambda_teilflaeche, lambdai
! ----------------------------------------------------------------------------------


! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
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


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /OB_ALPHA/ -----------------------------------------------------
!HB   Uebergabe eines Steuerparameters, ob die Datei Beiwerte.AUS
!HB   erzeugt und in sie geschrieben werden soll.                       
!HB   Der Parameter alpha_ja wird in WSPBER gesetzt.                    
INTEGER 	:: alpha_ja
COMMON / ob_alpha / alpha_ja
! -----------------------------------------------------------------------------


! COMMON-Block /GEF_SOHL/ -----------------------------------------------------
!HB   Uebergabe Sohlgefaelle aus SUB NORMBER
REAL             :: g_sohl
COMMON / gef_sohl / g_sohl
! -----------------------------------------------------------------------------

! COMMON-Block /P2/ -----------------------------------------------------------
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! Local variables
CHARACTER(LEN=72) 	:: textg
INTEGER                 :: j
REAL                    :: v
CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     !Abfrage fuer Ende der Inneren Q-Schleife
INTEGER, INTENT(IN)            :: nr_q_out
                                                                        
! *****************************************************************
! BERECHNUNG
! *****************************************************************
                                                                        
! bemerkungen : es werden in speicher die qtp(i,j)...(i,j)
! gespeichert. meiner meinung werden diese werte nicht mehr
! verwendet. da schrittweise ausgedruckt wird (fuer den .tab-
! file) brauchen nur die im .wsl-file zu speichernden werte be
! halten werden.
                                                                        

!WP
!write (*,*) 'Jetzt in Drucktab mit INDMAX = ', indmax
!write (*,*) 'Teilabfluss QTP(i,1)         = ', qtp(i,1)
!write (*,*) 'Teilabfluss QTP(i,2)         = ', qtp(i,2)
!write (*,*) 'Teilabfluss QTP(i,3)         = ', qtp(i,3)

!WP 11.03.2006
! Speichern der ausgegebenen Werte in dem globalen Ergebnis-Modul

IF (Q_Abfrage.eq.'IN_SCHLEIFE') then

  do j = 1, 3
    out_IND(i,nr_q,j)%lambda = rkp(i,j)         ! Lambda der Teilabschnitte
    out_IND(i,nr_q,j)%formb  = fbwp(i,j)        ! Formbeiwert der Teilabschnitte
    out_IND(i,nr_q,j)%A      = fp(i,j)		! Teilquerschnittsflaeche
    out_IND(i,nr_q,j)%lu     = up(i,j)          ! benetzter Umfang
    out_IND(i,nr_q,j)%v      = vp(i,j)          ! mittlere Flieﬂgeschwindigkeit
    out_IND(i,nr_q,j)%Q      = qtp(i,j)      	! Teilabfluesse
    out_IND(i,nr_q,j)%B      = brp(i,j)      	! Wasserspiegelbreite der Teilabschnitte
  end do

  out_PROF(i,nr_q)%stat   = station(i)          	! Station in [km]
  out_PROF(i,nr_q)%wsp    = wsp(i)              	! Wasserspiegelhoehe [mNN]
  out_PROF(i,nr_q)%hen    = hen(i)              	! Energielinienhoehe [mNN]
  out_PROF(i,nr_q)%sohle  = sohlp(i)            	! tiefster Sohlpunkt [mNN]
  out_PROF(i,nr_q)%boeli  = bolip(i)            	! Bordvollhoehe links [mNN]
  out_PROF(i,nr_q)%boere  = borep(i)            	! Bordvollhoehe rechts [mNN]
  out_PROF(i,nr_q)%hbv    = MIN(bolip(i), borep(i)) 	! minimale Bordvollhoehe [mNN]
  out_PROF(i,nr_q)%vm     = vmp(i)              	! mittlere Fliessgeschwindigkeit [m/s]
  out_PROF(i,nr_q)%tau = (rkp(i,2)/8)*rho*vp(i,2)**2    ! Sohlschubspannung im Flussschlauch [N/m2]
  out_PROF(i,nr_q)%alphaIW  = Alpha_IW(i)               ! Impulsstrombeiwert aus eb2ks [-]
  out_PROF(i,nr_q)%alphaEW  = Alpha_EW(i)               ! Energiestrombeiwert aus eb2ks [-]
  out_PROF(i,nr_q)%gefaelle = ABS(g_sohl)          ! Reibungsgefaelle [-]

ELSEIF (Q_Abfrage.eq.'BR_SCHLEIFE') then
  do j = 1, 3
    out_Qin_IND(i,nr_q_out,nr_q,j)%lambda = rkp(i,j)            ! Lambda der Teilabschnitte
    out_Qin_IND(i,nr_q_out,nr_q,j)%formb  = fbwp(i,j)           ! Formbeiwert der Teilabschnitte
    out_Qin_IND(i,nr_q_out,nr_q,j)%A      = fp(i,j)	        ! Teilquerschnittsflaeche
    out_Qin_IND(i,nr_q_out,nr_q,j)%lu     = up(i,j)             ! benetzter Umfang
    out_Qin_IND(i,nr_q_out,nr_q,j)%v      = vp(i,j)             ! mittlere Flieﬂgeschwindigkeit
    out_Qin_IND(i,nr_q_out,nr_q,j)%Q      = qtp(i,j)            ! Teilabfluesse
    out_Qin_IND(i,nr_q_out,nr_q,j)%B      = brp(i,j)            ! Wasserspiegelbreite der Teilabschnitte
  end do

  out_Qin_PROF(i,nr_q_out,nr_q)%stat   = station(i)          	! Station in [km]
  out_Qin_PROF(i,nr_q_out,nr_q)%wsp    = wsp(i)              	! Wasserspiegelhoehe [mNN]
  out_Qin_PROF(i,nr_q_out,nr_q)%hen    = hen(i)              	! Energielinienhoehe [mNN]
  out_Qin_PROF(i,nr_q_out,nr_q)%sohle  = sohlp(i)            	! tiefster Sohlpunkt [mNN]
  out_Qin_PROF(i,nr_q_out,nr_q)%boeli  = bolip(i)            	! Bordvollhoehe links [mNN]
  out_Qin_PROF(i,nr_q_out,nr_q)%boere  = borep(i)                       ! Bordvollhoehe rechts [mNN]
  out_Qin_PROF(i,nr_q_out,nr_q)%hbv    = MIN(bolip(i), borep(i))        ! minimale Bordvollhoehe [mNN]
  out_Qin_PROF(i,nr_q_out,nr_q)%vm     = vmp(i)              	        ! mittlere Fliessgeschwindigkeit [m/s]
  out_Qin_PROF(i,nr_q_out,nr_q)%tau    = (rkp(i,2)/8)*rho*vp(i,2)**2    ! Sohlschubspannung im Flussschlauch [N/m2]
  out_Qin_PROF(i,nr_q_out,nr_q)%alphaIW  = Alpha_IW(i)                  ! Impulsstrombeiwert aus eb2ks [-]
  out_Qin_PROF(i,nr_q_out,nr_q)%alphaEW  = Alpha_EW(i)                  ! Energiestrombeiwert aus eb2ks [-]
  out_Qin_PROF(i,nr_q_out,nr_q)%gefaelle = ABS(g_sohl)             ! Reibungsgefaelle [-]

END if




ifbr = 0
ifpgs = 0
ifnew = 0
ifgrnz = 0
                                                                        
IF (igrenz (i) .ge.10000) then
  ifbr = 1
  igrenz (i) = igrenz (i) - 10000
ENDIF
IF (igrenz (i) .ge.1000) then
  ifpgs = 1
  igrenz (i) = igrenz (i) - 1000
ENDIF
IF (igrenz (i) .ge.100) then
  ifnew = 1
  igrenz (i) = igrenz (i) - 100
ENDIF
IF (igrenz (i) .ge.10) then
  ifgrnz = 1
  igrenz (i) = igrenz (i) - 10
ENDIF

IF (igrenz (i) .eq.1) then
  textg = 'Stroemung schiessend. Weiter mit Grenztiefe'
ELSEIF (igrenz (i) .eq.2) then
  textg = 'Stroemung schiessend. Weiter mit Grenztiefe (*)'
ELSEIF (igrenz (i) .eq.4) then
  textg = 'Rohrstroemung schiessend. Weiter mit Vollfuellung'
ELSEIF (igrenz (i) .eq.5) then
  textg = 'Rohrstroemung schiessend. Weiter mit Vollfuellung (*)'
ELSE
  textg = ' '
ENDIF


ilen = LEN_TRIM (textg)
IF (ifgrnz.ne.0) then
  textg (ilen + 2:) = ' g'
  ilen = ilen + 2
ENDIF
IF (ifnew.ne.0) then
  textg (ilen + 2:) = ' n'
  ilen = ilen + 2
ENDIF
IF (ifpgs.ne.0) then
  textg (ilen + 2:) = ' p'
  ilen = ilen + 2
ENDIF
IF (ifbr.ne.0) then
  textg (ilen + 2:) = ' b'
  ilen = ilen + 2
ENDIF


! 1.) Ausgabe der Zeile mit den Verlusthoehen
! -------------------------------------------
WRITE (jw5, 12) textg, qs (i), hvs (i), hrs (i), hbors (i), hein (i), hort (i), hs (i)


! 1b.) Ausgabe der Zeile mit den Verlusten in Datei mit WQ-Beziehung
! ------------------------------------------------------------------
IF (idr1.eq.'j') then
  WRITE (jw7, 12) textg, qs (i), hvs (i), hrs (i), hbors (i), hein (i), hort (i), hs (i)
ENDIF

12 format (/1X, A, T78, F8.3, 1X, F7.3, 1X, F7.3, 1X, F7.3, 1X, F7.3, 1X, F7.3, 1X, F8.4)
!                       Qges      hv-m      hr-m      h-bor     h-ein    h-ort      h-m


nz = nz + 2


! Verhaeltnisse am profil i:
j = 1

! Falls der Rauhigkeitsabschnitt j keinen Abfluss aufweist, weiter mit dem Naechsten.
IF (qtp (i, j) .lt. 1.e-6) j = j + 1

! 2.) Ausgabe der ersten Zeile mit den Profilparametern (wsp, lambda, v, A, usw.)
! -------------------------------------------------------------------------------

WRITE (jw5, 10) station(i), wsp(i), hen(i), j, rkp(i,j), fbwp(i,j), fp(i,j), up(i,j), vp(i,j), qtp(i,j)

!HB   *****************************************************************
!HB        26.11.2001 - H.Broeker
!HB        ----------------------
!HB        Die IF_Schleife wird durchlaufen, wenn die Datei Beiwerte.AUS
!HB        erzeugt werden soll und in sie geschrieben werden soll.
IF (alpha_ja.eq.1) then
  !HB          Uebergabe der benoetigten Werte an SUB ALPHA_DRU
  !HB          mit anschliessender Ausgabe in Datei Beiwerte.AUS
  !HB          (Zusatz-AusgabeDatei von Impuls- und Energiestrombeiwert)
  CALL alpha_dru (station, wsp, hen, up, qs)
  !HB          Ausdruck der ueblichen Ausgabewerte in die Datei
  !HB          Beiwerte.AUS. Das Format entspricht der bekannten
  !HB          Tabellenform.
  WRITE (UNIT_OUT_ALPHA, 10) station(i), wsp(i), hen(i), j, rkp(i,j), fbwp(i,j), fp (i,j), up(i,j), vp(i,j), qtp(i,j)
ENDIF
!HB   *****************************************************************



! 2b.) Ausgabe der Zeile mit den Profilparametern in Datei mit WQ-Beziehung
! -------------------------------------------------------------------------
IF (idr1.eq.'j') then
  WRITE (jw7, 10) station(i), wsp(i), hen(i), j, rkp(i,j), fbwp(i,j), fp(i,j), up(i,j), vp(i,j), qtp(i,j)
ENDIF

! Format auf 3 Stellen genau (normale Einstellung)
!10 FORMAT (/,f11.4,f10.3,f11.3,i4,f9.2,f9.2,f11.3,f9.3,f8.3)
10 format (/1X, F9.4, 1X, F10.3, 1X, F10.3, 1X, I3, 1X, F7.4, 1X, F6.3, 1X, F8.3, 1X, F8.3, 1X, F6.3, 1X, F8.3)
!               stat      wsp        hen        IND     lambd     formb     Fl        Umfg      v         Q


nz = nz + 2





! 3.) Falls es weitere Rauhigkeitsabschnitte gibt, dann weitere Ausgabe, aber ohne STAT und WSP
! ---------------------------------------------------------------------------------------------
IF (j .ne. indmax) then

  Ausgabe: DO j1 = j + 1, indmax

    IF (qtp (i, j1) .lt.1.e-6) CYCLE Ausgabe

    WRITE (jw5, 11) j1, rkp(i,j1), fbwp(i,j1), fp(i,j1), up(i,j1), vp(i,j1), qtp(i,j1)

    !HB   ****************************************************************
    !HB     26.11.2001 - H.Broeker
    !HB     ----------------------
    !HB     Die IF_Schleife wird durchlaufen, wenn die Datei
    !HB     Beiwerte.AUS erzeugt werden soll und in sie
    !HB     geschrieben werden soll.
    IF (alpha_ja.eq.1) then
      !HB     Ausdruck der ueblichen Ausgabewerte in die Datei
      !HB     Beiwerte.AUS. Das Format entspricht der bekannten
      !HB     Tabellenform
      WRITE (UNIT_OUT_ALPHA, 11) j1, rkp(i,j1), fbwp(i,j1), fp(i,j1), up(i,j1), vp(i,j1), qtp(i,j1)
    ENDIF
    !HB   ***************************************************************           

    ! 3b.) Ausgabe der Zeile mit den Profilparametern in Datei mit WQ-Beziehung
    ! -------------------------------------------------------------------------
    IF (idr1.eq.'j') then
      WRITE (jw7, 11) j1, rkp(i,j1), fbwp(i,j1), fp(i,j1), up(i,j1), vp(i,j1), qtp(i,j1)
    ENDIF

    nz = nz + 1

  END DO Ausgabe

  ! Format auf 3 Stellen genau (normale Einstellung)
  !11 FORMAT (32x,i4,f9.2,f9.2,f11.3,f9.3,f8.3)
  11 format (1X, T33, 1X, I3, 1X, F7.4, 1X, F6.3, 1X, F8.3, 1X, F8.3, 1X, F6.3, 1X, F8.3)
  !                       IND     lambd     formb     Fl        Umfg      v         Q
  ! Format auf 6 Stellen genau
  !15 FORMAT (t42,i2,t45,f12.6,t58,f13.6,t72,f14.6,t87,f9.6,t97,f11.6)

ENDIF


!WP -------------------------------------------------------------------------------------
!WP 11.11.2005
!WP Ausgabe der einzelnen Widerstandsbeiwerte in LAMBDA_I.TXT. Die Fliessgeschwindigkeit
!WP bleibt gemittelt. Auch der Widerstandsbeiwert auf dem Vorland is konstant ueber
!WP das gesamte Vorland, unabhaengig von eventuellen ks-Wert Aenderungen.
DO j = 1, nknot

  if (j < itrli) then
    ! Linkes Vorland
    if ( lambda_teilflaeche(j) > 0.0 ) then
      v = vp(i,1)
    else
      v = 0.0
    end if

  else if ( j >= itrli .and. j < itrre) then
    ! Flussschlauch
    if ( lambda_teilflaeche(j) > 0.0 ) then
      v = vp(i,2)
    else
      v = 0.0
    end if

  else if ( j >= itrre) then
    ! Rechtes Vorland
    if ( lambda_teilflaeche(j) > 0.0 ) then
      v = vp(i,3)
    else
      v = 0.0
    end if

  end if

  write (UNIT_OUT_LAMBDA_I,9000) station(i), j, x1(j), h1(j), lambda_teilflaeche(j), v   !rhynn (j), ln_ks(j)

END DO
9000 format (1X, F10.4, i7, F10.3, F10.3, F12.7, F10.3)
!WP -------------------------------------------------------------------------------------


END SUBROUTINE drucktab                                                                        
                                                                        

