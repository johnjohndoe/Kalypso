!     Last change:  WP   10 Mar 2006    8:38 pm
!--------------------------------------------------------------------------
! This code, probv.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - probv1
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
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
SUBROUTINE probv1 (nstat, qmax, hrb, lif, &
                 & kif, lv, kv, qges, bges, wspgef, cc, sgef_m)
!
! Beschreibung
! ------------
! Bestimmung der Kalinin-Miljukov-Parameter am Profil
!
! - aufgerufen von SUB BOVOG1
!
!
! DIREKT UEBERGEBENE PARAMETER
! ----------------------------
! bges     - GESAMTE FLUSSBREITE?
! cc       - ???
! hrb      - Fliesstiefe [m NN]
! kif      - k_i Fluss
! kv       - kiv?
! lif      - REZIPROKE CHAR. LAENGE? (FELD) [1/m?]
! lv       - liv?
! nstat    - ANZAHL DER STATIONEN
! qges     - GESAMTABFLUSS
! qmax     - q-voll [m**3/s]
! qvar     - NICHT VERWENDET?
! sgef_m   - MITTLERES SOHLGEFAELLE?
! st_o     - NICHT VERWENDET?
! st_u     - NICHT VERWENDET?
! wspgef   - WSP-GEFAELLE = MITTLERES SOHLGEFAELLE?
!
! WEITERE VARIABLEN
! -----------------
! b        - MITTLERE BREITE VON F ODER V - RECHENHILFSGROESSE
! bb       - FLUSSBREITE
! bvor     - VORLANDBREITE
! gefaelle - WSP-GEFAELLE, WIRD ALS WSPGEF DIREKT UEBERGEBEN
! ianz     - istep
! ikenn0   - ischbv(nstat)-3
! ikenn1   - ikenn0-1
! ischbv   - Integerfeld(maxger) aus COMMON/UEBER_KM/ ???
!            ??? aus SUB BORDV ???
! istep    - = 6 zu Beginn,
! kiv      - k_i VORLAND [h]
! kif      - k_i FLUSS [h]
! lif      - l_i FLUSS
! liv      - l_i VORLAND
! qm       - MITTELWERT VON q ZWISCHEN ZWEI ABSCHNITTEN IM FLUSS
!            ODER IM VORLAND - RECHENHILFSGROESSENFELD
! qq       - q im FLUSS DER EINZELNEN ABSCHNITTE
! rkf      - mittlere KM-Parameter im Profil, k-Fluss [h]
! rkv      - mittlere KM-Parameter im Profil, k-vorl. [h]
! rnf      - (1./sl), mittlere KM-Parameter im Profil, n-Fluss [1/m]
! rnfk     - mittlere KM-Parameter im Profil, n-Fluss [1/km]
! rnv      - (1./sl), mittlere KM-Parameter im Profil, n-vorl. [1/m]
! rnvk     - mittlere KM-Parameter im Profil, n-vorl. [1/km]
! sl       - MITTLERE LAENGE EINES CHARAKTERISTISCHEN ABSCHNITTS
! suki     - SUMME DER REZIPROKEN kif BZW. kiv
! suli     - SUMME DER REZIPROKEN lif BZW. liv
!
! AUFGERUFENE ROUTINEN
! --------------------
! - KEINE
!
! FUER WEITERE VERSIONEN
! ----------------------
! - AM 04.02.2000, SIND VERSCHIEDENE IF-SCHLEIFEN BEI REAL GROESSEN
!   MIT .eq. ABGEFRAGT, AENDERN AUF .le.!
!-----------------------------------------------------------------------
                                                                        

!------------------------------------------------------------------
! VEREINBARUNGEN
!------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS

! Calling variables
INTEGER, INTENT(IN) 	:: nstat    	! Anzahl der Stationen
REAL, INTENT(IN)        :: qmax         ! Maximaler Abfluss (Bordvoll?)
REAL, INTENT(IN)        :: hrb          ! Fliesstiefe [m NN]
REAL       		:: lif(6)       ! l_i Fluss
REAL       		:: kif(6)       ! k_i Fluss [h]
REAL       		:: lv(6)        ! l_i Vorland
REAL       		:: kv(6)        ! k_i Vorland [h]
REAL                    :: qges(6)      ! Gesamtabfluss ?
REAL                    :: bges(6)      ! Gesamte Breite ?
REAL                    :: wspgef(6)    ! WSP Gefaelle ?
REAL                    :: cc
REAL, INTENT(IN)        :: sgef_m       ! Mittleres Gefaelle ?


! COMMON-Block /PROF/ ---------------------------------------------------------
REAL 		:: hwsp (100, maxger)
COMMON / prof / hwsp
! -----------------------------------------------------------------------------


! COMMON-Block /UEBER_KM/ -----------------------------------------------------
REAL 	:: qq (100, maxger), bb (100, maxger), fb (100, maxger)
REAL 	:: vfl (100, maxger), qvor (100, maxger), bvor (100, maxger)
REAL 	:: fvor (100, maxger)
INTEGER :: ischbv (maxger)
COMMON / ueber_km / qq, bb, fb, vfl, qvor, bvor, fvor, ischbv
! -----------------------------------------------------------------------------


! Local variables
REAL 	:: cr (6), q (6), b (6), dq (6), db (6), dh (6), qm (6), liv (6), kiv (6)

                                                                        
! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
!**   SETZEN DER STARTWERTE
istep = 6
suki = 0.
suli = 0.
lif (1) = 0.
liv (1) = 0.
kiv (1) = 0.
kif (1) = 0.

ianz = istep

!**   nstat wird mit 4 uebergeben? --> ikenn0=1?. UT, 04.02.2000
ikenn0 = ischbv (nstat) - 3
ikenn1 = ikenn0 - 1
gefaelle = 0.


! BEMERKUNGEN ZUR KM-PARAMETERBERECHNUNG:
! ---------------------------------------
! DAS PROFIL WIRD IN 6 ABSCHNITTE AUFGETEILT (istep)
! DIE GROESSEN SIND PROFILNR. UND UND ABSCHNITTSNR. ABHAENGIG
! BERECHNUNG VON
! dq = Differenz von qq zwischen zwei Abschnitten im PROFIL
! dh = HOEHENDIFFERENZ zwischen zwei Abschnitten im PROFIL
! wspgef(Abschnitt) = mittlerem Gefaelle

! ------------------------------------------------------------------
! BERECHNUNG DER PARAMETER FUER DAS VORLAND
! ------------------------------------------------------------------

DO 2000 i1 = 2, istep
  ikenn1 = ikenn1 + 1
  ikenn2 = ikenn1 + 1
  dq (i1) = qq (ikenn2, nstat) - qq (ikenn1, nstat)
  IF (dq (i1) .le.1.e-04) then
    ianz = ianz - 1
    GOTO 2000
  ENDIF
  qm (i1) = (qq (ikenn2, nstat) + qq (ikenn1, nstat) ) / 2.
  dh (i1) = hwsp (ikenn2, nstat) - hwsp (ikenn1, nstat)

  !**     17.11.98 Csocsan
  wspgef (i1) = sgef_m




  db (i1) = bb (ikenn2, nstat) - bb (ikenn1, nstat)

  b (i1) = (bb (ikenn2, nstat) + bb (ikenn1, nstat) ) / 2.

  !**      FALLS NENNER NICHT NULL, WIRD DIE MITTLERE LAENGE EINES CHAR.
  !**      ABSCHNITTS BERECHNET, MANIAK, p. 347, 4. Auflage
  !**      L=Q*H_delta/(J*Q_delta) => reziproker Wert (warum?)
  IF (qm (i1) .ne.0.0.AND.wspgef (i1) .ne.0.0.AND.dq (i1)         &
       & .ne.0.0.AND.dh (i1) .ne.0.0) then
    lif (i1) = 1. / (qm (i1) * dh (i1) / (wspgef (i1) * dq (i1) ) )

    !**          BERECHNUNG DER SUMME DER reziproken lif (besser -gt.0.001)!

    IF (lif (i1) .ne.0.0) then
      suli = suli + 1. / lif (i1)
    ENDIF
  ENDIF

2000 END DO


!**   BERECHNUNG MITTLERE LAENGE sl EINES CHARAKTERISTISCHEN ABSCHNITTS?
IF (ianz.ne.1) then
  sl = suli / (ianz - 1)

  IF (sl.ne.0.0) rnf = (1. / sl)

  !**     17.11.98: gefaelle = gefaelle/(ianz-1)
  gefaelle = sgef_m

ENDIF


!**   BERECHNUNG DER KONSTANTE kif FUER DIE ABSCHNITTE
DO 3000 i1 = 2, istep
  IF (dq (i1) .le.1.e-04) goto 3000
  IF (dq (i1) .ne.0.0.AND.lif (i1) .ne.0.0) then
    !**        k=(b*l*dh)/dq, Maniak, 4. Auflage, p. 345
    !**        UMRECHNUNG VON SEKUNDEN AUF h?
    kif (i1) = b (i1) * dh (i1) / dq (i1) / 3600. / lif (i1)
  ELSE
    kif (i1) = 0
  ENDIF
  !**     BERECHNUNG DER SUMME suki VON kif FUER EIN PROFIL
  suki = suki + kif (i1)
3000 END DO

!**   BERECHNUNG EINER MITTLEREN KONSTANTE rkf
IF (ianz.ne.1) then
  rkf = suki / (ianz - 1)
ENDIF

!**   SETZE DIE HILFSGROESSEN ZUR BESTIMMUNG DER MITTLEREN WERTE
!**   WIEDER AUF NULL
suki = 0.
suli = 0.


! ------------------------------------------------------------------
! BERECHNUNG DER PARAMETER FUER DAS VORLAND
! ------------------------------------------------------------------

ianzv = 6
rkv = 0.
rnv = 0.
cc = 1.

DO i1 = 1, 6
  liv (i1) = 0.
  kiv (i1) = 0.
END DO

ikenn1 = ikenn0 - 1

DO 2100 i1 = 2, 6
  ikenn1 = ikenn1 + 1
  ikenn2 = ikenn1 + 1

  dq (i1) = qvor (ikenn2, nstat) - qvor (ikenn1, nstat)

  IF (dq (i1) .le.1.e-04) then
    ianzv = ianzv - 1
    GOTO 2100
  ENDIF

  qm (i1) = (qvor (ikenn1, nstat) + qvor (ikenn2, nstat) ) / 2.
  dh (i1) = hwsp (ikenn2, nstat) - hwsp (ikenn1, nstat)
  db (i1) = bvor (ikenn2, nstat) - bvor (ikenn1, nstat)
  b (i1) = (bvor (ikenn1, nstat) + bvor (ikenn2, nstat) ) / 2.
  liv (i1) = 1. / (qm (i1) * dh (i1) / (wspgef (i1) * dq (i1) ) )
  qz = qq (ikenn2, nstat) + qvor (ikenn2, nstat) - qmax

  IF (qz.le. + 1.e-06) then
    cr (i1) = 1.
  ELSE
    cr (i1) = (qz - qvor (ikenn2, nstat) ) / qz
  ENDIF

  suli = suli + 1. / liv (i1)

2100 END DO

IF (ianzv.ne.1) then
  sl = suli / (ianzv - 1)
  IF (sl.ne.0.0) rnv = (1. / sl)
ENDIF

DO 3100 i1 = 2, 6
  IF (dq (i1) .le.1.e-04) goto 3100
  IF (dq (i1) .ne.0.0.AND.liv (i1) .ne.0.0) then
    kiv (i1) = b (i1) * dh (i1) / dq (i1) / liv (i1) / 3600.
  ELSE
    kiv (i1) = 0
  ENDIF
  suki = suki + kiv (i1)

3100 END DO

IF (ianzv.ne.1) then
  rkv = suki / (ianzv - 1)
ENDIF

csum = 0.
imin = 6 - ianzv + 1

DO i1 = imin, 6
  csum = cr (i1) + csum
END DO

IF ( (ianzv - 1) .gt.1.e-06) cc = csum / (ianzv - 1)


!**   Schreiben der KOPFZEILEN IN ErgebnissDATEI UNIT_OUT_LOG_KM UND DES GEFAELLES

!     write(UNIT_OUT_LOG_KM,9001)statso
WRITE (UNIT_OUT_LOG_KM, 9002) gefaelle
WRITE (UNIT_OUT_LOG_KM, 9003)
WRITE (UNIT_OUT_LOG_KM, 9004)
WRITE (UNIT_OUT_LOG_KM, 9005)

lv (1) = 0.
kv (1) = 0.

ikenn1 = ikenn0 - 1


! -----------------------------------------------------------------
! SUMMATION VON FLUSS UND VORLAND
! -----------------------------------------------------------------

DO 5000 i1 = 2, istep
  ikenn1 = ikenn1 + 1
  ikenn2 = ikenn1 + 1

  sli = lif (i1) * 1000.
  ski = kif (i1)

  IF (hwsp (ikenn2, nstat) .le.hrb) then

    WRITE (UNIT_OUT_LOG_KM, 9008) hwsp (ikenn2, nstat), fb (ikenn2, nstat),  &
    bb (ikenn2, nstat), bb (ikenn2, nstat), qq (ikenn2, nstat),   &
    sli, ski

    lv (i1) = 0.
    kv (i1) = 0.
    qges (i1) = qq (ikenn2, nstat)
    bges (i1) = bb (ikenn2, nstat)

  ELSE

    bges (i1) = bb (ikenn2, nstat) + bvor (ikenn2, nstat)

    IF (liv (i1) .gt.0) then
      sliv = liv (i1) * 1000.
      skiv = kiv (i1)
      lv (i1) = liv (i1)
      kv (i1) = kiv (i1)
      qges (i1) = qq (ikenn2, nstat) + qvor (ikenn2, nstat)
    ELSE
      sliv = 0.0
      skiv = 0.0
      lv (i1) = 0.0
      kv (i1) = 0.0
      qges (i1) = qq (ikenn2, nstat)
    ENDIF

    WRITE (UNIT_OUT_LOG_KM, 9006) hwsp (ikenn2, nstat), fb (ikenn2, nstat),  &
    fvor (ikenn2, nstat), bb (ikenn2, nstat), bvor (ikenn2, nstat)&
    , bges (i1), qq (ikenn2, nstat), qvor (ikenn2, nstat),        &
    sli, ski, sliv, skiv
  ENDIF

5000 END DO

!**   UMRECHNUNG DER KONSTANTE n VON 1/m in 1/km
rnfk = rnf * 1000.
rnvk = rnv * 1000.

!**   SCHREIBEN DER KONSTANTEN FUER FLUSS UND VORLAND
!**   SOWIE VON qmax UND DER FLIESSTIEFE IN DATEI UNIT_OUT_LOG_KM
WRITE (UNIT_OUT_LOG_KM, 9007) hrb, qmax, rkf, rnfk, rkv, rnvk


                                                                        
! -----------------------------------------------------------------
! Formate
! -----------------------------------------------------------------
                                                                        
 9001 FORMAT('',/t2,'kalinin-miljukov-parameter fuer profil-km',f10.4) 
 9002 FORMAT(/,t2,'wasserspiegelgefaelle : ',f6.4,' [-]') 
 9003 FORMAT(//,t2,'fliess',t14,'flaeche',t35,'breite',t54,'abfluss',   &
     &       t67,'fluss-speicher',t82,'vorland-speicher')               
 9004 FORMAT(t2,'tiefe',t11,'fluss',t18,'vorland',t27,'fluss',t34,      &
     &           'vorland',t43,'gesamt',t52,'fluss',t59,'vorland',t68,  &
     &           'anzahl',t75,'konst.',t84,'anzahl',t93,'konst.')       
 9005 FORMAT(t3,'[nn+m]',t11,'[m**2]',t19,'[m**2]',t29,'[m]',t37,'[m]', &
     &       t45,'[m]',t51,'[m**3]',t59,'[m**3]',t68,'[1/km]',t77,'[h]',&
     &       t87,'[1/km]',t94,'[h]')                                    
 9006 FORMAT(12f8.3) 
 9007 FORMAT(//,t2,'parameter bordvoller abflusz:  fliesstiefe = nn+  ',&
     &       f8.3,' m          q-voll = ',f8.3,' m**3/s',               &
     &       //t2,'mittlere kalinin-miljukov-parameter im profil: ',t50,&
     &             'k-fluss ='                                          &
     &            ,f8.3,' h',/t50,'n-fluss =',f8.3,' 1/km',/            &
     &            ,t50,'k-vorl. =',f8.3,' h',/t50,'n-vorl. =',f8.3,     &
     &             ' 1/km')                                             
 9008 FORMAT(2f8.3,t25,f8.3,t41,2f8.3,t65,2f8.3) 
                                                                        
END SUBROUTINE probv1
