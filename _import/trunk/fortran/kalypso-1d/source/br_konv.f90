!     Last change:  MD    7 Feb 2008    2:01 pm
!--------------------------------------------------------------------------
! This code, br_konv.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - br_konv
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
SUBROUTINE br_konv (staso, str1, q, q1, nprof, hr, hv, rg, hvst,  &
     & hrst, indmax, psieins, psiorts, nblatt, &
     & nz, idr1, hgrenz, ikenn, ifroud, iwehr, Q_Abfrage, nr_q_out)
!
! BESCHREIBUNG: KONVENTIONELLE BRUECKENBERECHNUNG
!
!
! AUFGERUFENE ROUTINEN
! --------------------
! linie (hokmax,x1,h1,nknot,iwl,iwr,npl,xl,hl)
! normber (strbr,q,q1,nprof,hr,hv,rg,hvst,hrst,indmax,
!         psieins,psiorts,jw5,hgrenz,ikenn,froud,nblatt,nz)
! intdat (staso,ifehl)
! linier (hokmax,x1,h1,rau,nknot,iwl,iwr,npl,xl,hl,raul)
! speicher (nprof,hr,hv,hvst,hrst,q,stat(nprof),indmax,ikenn)
! kopf (nblatt,nz,jw5,jw7,idr1)
! drucktab (nprof,indmax,nz,jw5,nblatt,stat,jw7,idr1)
! yarnell (hpf,f,q,xk)
!-----------------------------------------------------------------------


! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN
USE IO_UNITS
USE MOD_INI

implicit none

REAL, INTENT(IN)     	:: staso        ! Station [km]
REAL                    :: str1         ! Abstand zum naechsten Profil [m]
REAL                    :: q            ! Abfluss
REAL                    :: q1           ! Abfluss
INTEGER                 :: nprof        ! Nummer des Profils
REAL                    :: hr
REAL                    :: hv
REAL                    :: rg
REAL                    :: hvst
REAL                    :: hrst
INTEGER                 :: indmax       ! Anzahl der Rauheitsabschnitte
REAL                    :: psieins
REAL                    :: psiorts
INTEGER                 :: nblatt
INTEGER                 :: nz
CHARACTER(LEN=1) 	:: idr1
REAL                    :: hgrenz
INTEGER                 :: ikenn
INTEGER                 :: ifroud
INTEGER                 :: iwehr

! COMMON-Block /BRUECK/ ------------------------------------------------------------
INTEGER 	:: iwl, iwr, nuk, nok
INTEGER 	:: iokl, iokr           ! Gelaende Oberkante Grenze
REAL 		:: xuk (maxkla), huk (maxkla), xok (maxkla), hok (maxkla)
REAL    	:: hukmax, hokmax, hsuw, raub, breite, xk, hokmin
CHARACTER(LEN=1):: ibridge
COMMON / brueck / iwl, iwr, iokl, iokr, nuk, nok, xuk, huk, xok, hok, hukmax, &
       & hokmax, hsuw, raub, breite, xk, hokmin, ibridge
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
! x1     r4(1...maxkla)   x-wert der profilpunkte
! h1     r4(1...maxkla)   hoehen-wert der profilpunkte
! rau    r4(1...maxkla)   rauhigkeitsbeiwert (k-s,k-st) profilpunkte
! nknot  i4               anzahl profilpunkte
! iprof  a1               kennung zur bezeichnung des profiltyps
!                         iprof = ' ' : normalprofil
!                                 't' : trapezprofil
!                                 'k' : kreisprofil
!                                 'e' : eiprofil
!                                 'm' : maulprofil
! durchm r4               fuer iprof = ' ' : nicht belegt
!                                      't' : breite der sohle
!                                      'k' : durchmesser
!                                      'e' : max. breite
!                                      'm' : max. breite
! hd     r4               fuer iprof = ' ' : nicht belegt
!                                      't' : vertikales lichtmasz
!                                      'k' : nicht belegt
!                                      'e' : vertikales lichtmasz
!                                      'm' : vertikales lichtmasz
! steig  r4               steigung beider profilseiten
!                         nur fuer iprof = 't' belegt
! sohlg  r4               sohlgefaelle
!                         bei normalprofil z.zt. nicht belegt.
! boli   r4               gelaendehoehe an linker vorland/
!                         fluszschlauchtrennflaeche
! bore   r4               gelaendehoehe an rechter vorland/
!                         fluszschlauchtrennflaeche
! hmin   r4               minimale gelaendehoehe des profils
! hmax   r4               maximale gelaendehoehe des profils
! ianf   i4               punkt-nr. der linken vorland
!                         fluszschlauchtrennflaeche
! iend   i4               punkt-nr. der rechten vorland
!                         fluszschlauchtrennflaeche
REAL 		 :: x1 (maxkla), h1 (maxkla), rau (maxkla)
CHARACTER(LEN=1) :: iprof
REAL 		 :: durchm, hd, sohlg, steig, boli, bore, hmin, hmax, hrbv
INTEGER 	 :: nknot, ianf, iend
COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig, &
            & boli, bore, hmin, hmax, ianf, iend, hrbv
! -----------------------------------------------------------------------------


! COMMON-Block /P3/ -----------------------------------------------------------
INTEGER 	:: isohl, iming
REAL            :: hming
COMMON / p3 / isohl, hming, iming
! -----------------------------------------------------------------------------


! COMMON-Block /FLAECHEN/ -----------------------------------------------------
REAL            :: ad, aue, apl, apg
REAL            :: hss, hue, hpl, hpg
COMMON / flaechen / ad, aue, apl, apg, hss, hue, hpl, hpg
! -----------------------------------------------------------------------------


! COMMON-Block /PFEILERSTAU/ --------------------------------------------------
REAL            :: alpha
REAL            :: bnetto
COMMON / pfeilerstau / alpha, bnetto
! -----------------------------------------------------------------------------


! Local variables
REAL :: xi (maxkla), hi (maxkla), s (maxkla)
REAL :: x1_orig (maxkla), h1_orig (maxkla), s_orig (maxkla)
REAL :: x1_bruecke(maxkla), h1_bruecke(maxkla), s_bruecke(maxkla)

REAL :: raul (maxkla)
REAL :: xl (maxkla), hl (maxkla)
!**   hilfsparameter fuer das Vorland
REAL :: x11 (maxkla), h11 (maxkla), ax1 (maxkla), ay1 (maxkla), dp1 (maxkla)


INTEGER, PARAMETER :: itmax_brkon = 20

INTEGER :: i, i1, i2, ifehl, ifehl2, isch
INTEGER :: itrlio, itrreo
INTEGER :: il, ir                       ! Punktnummer linkes und rechtes Widerlager
INTEGER :: npl
INTEGER :: ibiter, itbmax               ! Iterationsparameter
INTEGER :: iartb, iart, iartt
INTEGER :: ifkonv                       ! Konvergenzbeschreiber

REAL :: m, m1, m2, m3
REAL :: a, a1, a2, a3

REAL :: qdiff, qdif1, qdif2
REAL :: hea, heb, heaa
REAL :: he1, he3
REAL :: hr1, hr2, hr3, hr1n
REAL :: wl, wl3
REAL :: hmass                           ! Massgebende Hoehe
REAL :: cd                              ! Widerstandsbeiwert Brückenplatte
REAL :: dx
REAL :: d1
REAL :: hminn, hsohl, hdif
REAL :: aue3
REAL :: froud
REAL :: rhy
REAL :: vm
REAL :: tm
REAL :: qgesb, qgesa, qw, qd
REAL :: df, dfq, dfh
REAL :: hpf, f                          ! Hoehenverlust, Fliesstiefe bei Pfeilerstau
REAL :: strbr
CHARACTER(LEN=11), INTENT(IN)  :: Q_Abfrage     !Abfrage fuer Ende der Inneren Q-Schleife
INTEGER, INTENT(IN)            :: nr_q_out


IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  write (UNIT_OUT_LOG, 20) staso, str1, q, q1, nprof, hr, rg, hvst, hrst
  20 format (//1X, 'Subroutine BR_KONV', /, &
           & 1X, '------------------------------------------------', //, &
           & 5X, 'Uebergebene Parameter:', /, &
           & 5X, '----------------------', /, &
           & 5X, 'STASO (Station)                      = ', F12.5, /, &
           & 5X, 'STR1 (Abstand zum vorh. Profil)      = ', F12.5, /, &
           & 5X, 'Q (Abfluss)                          = ', F12.5, /, &
           & 5X, 'Q1 (Abfluss?)                        = ', F12.5, /, &
           & 5X, 'NPROF (Nummer des Profils)           = ', I12,   /, &
           & 5X, 'HR (Wasserstand im vorh. Profil)     = ', F12.5, /, &
           & 5X, 'RG (?)                               = ', F12.5, /, &
           & 5X, 'HVST (Verlusthoehe?)                 = ', F12.5, /, &
           & 5X, 'HRST (Reibungsverlust aus STATION)   = ', F12.5, /)
ENDIF
  !do i = 1, nknot
  !  write (UNIT_OUT_LOG,*) i, x1(i), h1(i)
  !end do



! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------


do i = 1, maxkla
  x1_orig(i) = x1(i)
  h1_orig(i) = h1(i)
  s_orig(i)  = s(i)
end do

ikenn = 0
ifkonv = 0
iartt = 0
iwehr = 0

hr2 = 0.
cd = 2.0



!----------------------------------------------------------
!WP Setzen der Hoehe der Sohle im Unterwasser auf HMIN ????
hsuw = hmin
!WP Hier eventuell Problem!
!----------------------------------------------------------


!     2 moeglichkeiten :
!     - 1. profilwerte fuer 2 und 3 um (hmin-hsuw)/2., bzw. (hmin-hsuw)
!          erniedrigen, d.h. veraenderung profilwerte
!     - 2. differenz zwischen sohle und uk --> hdif
!          - je nachdem, ob hr>hmin+hdif --> druckabfluss
!            d.h. theoretische veraenderung der uk,ok
!     ******************************************************************
!     berechnung der wassertiefe im uw-profil (profil 3)
!     - profil ungestoert durch pfeiler
!     - beruecksichtigung der einengung durch widerlager
!
!     *********************************
!     vorbelegungen:
!     *********************************

dx = 0.10

!     ***************************************************************
!     zusammensetzen der profillinie (mit '*' gekennzeichnete linie)
!     ***************************************************************
!
!     *************....................****************
!     ............*.......... .........*..............
!               . *         . .       **
!                 ****      . .     *
!                     *     . .    *
!                     *************

CALL linie (hokmax, x1, h1, nknot, iwl, iwr, npl, xl, hl)

!     *************************************************************
!     berechnung wsp im ungestoerten, eingeengten profil '3'
!     *************************************************************

!     common-block fuer die gewaessergeometrie uberschreiben ((
!     x1,h1,rau,boli,bore .... festlegen
!**
!**    zaelher fuer iterationsschritte bei brueckenberechnung
!**    konventionelle methode

isch = 0

ifroud = 0


!JK    SCHREIBEN IN KONTROLLFILE
IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  WRITE (UNIT_OUT_LOG, 28) iokl, iokr
ENDIF
  28 format (/1X, 'IOKL (Nummer des Punktes der Oberkante links):  ', I3, /, &
          & 1X, 'IOKR (Nummer des Punktes der Oberkante rechts): ', I3)

i1 = iokl
i2 = iokl + npl

!JK   WENN BERECHNUNG NACH DARCY-WEISBACH
if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
  DO i = i1, i2
    ax (i) = 0.0
    ay (i) = 0.0
    dp (i) = 0.0
  END DO
ENDIF

IF (iokl.gt.1) then
  DO i = 1, iokl
    raul (i) = rau (i)
    IF (i.eq.iokl) raul (i) = raub
  END DO
ELSE
  raul (1) = raub
ENDIF

IF (iokr.lt.nknot) then

  DO i = iokr, nknot

    x11 (i) = x1 (i)
    h11 (i) = h1 (i)

    !JK  WENN BERECHNUNG NACH DARCY-WEISBACH
    if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
      ax1 (i) = ax (i)
      ay1 (i) = ay (i)
      dp1 (i) = dp (i)
    ENDIF

  END DO

  DO i = iokr, nknot

    i2 = i2 + 1
    x1 (i2) = x11 (i)
    h1 (i2) = h11 (i)
    raul (i2) = rau (i)

    !JK   WENN BERECHNUNG NACH DARCY-WEISBACH
    if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
      ax (i2) = ax1 (i)
      ay (i2) = ay1 (i)
      dp (i2) = dp1 (i)
    ENDIF

  END DO

ELSE

  i2 = i2 + 1
  x1 (i2) = x1 (nknot)
  h1 (i2) = h1 (nknot)
  raul (i2) = rau (nknot)

ENDIF

DO i = 1, npl

  i1 = i1 + 1
  x1 (i1) = xl (i)
  h1 (i1) = hl (i)
  IF (i.eq.npl) then
    raul (i1) = raub
  ELSEIF (raub.lt.1.0) then
    raul (i1) = rau (ianf + 1)
  ELSE
    raul (i1) = rau (ianf)
  ENDIF

END DO


IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  WRITE (UNIT_OUT_LOG, 29) npl, i2, nknot
ENDIF
  29 format (/1X, 'NPL (Anzahl Punkte auf Brueckenlinie?):       ', I3,/,&
            & 1X, 'I2 (Gesamte Anzahl der Punkte inkl. Bruecke): ', I3,/,&
            & 1X, 'NKNOT (Anzahl Profilpunkte ohne Bruecke):     ', I3)

nknot = i2

!  erniedrigen der profilwerte profil '1' um (hmin-hsuw)
!  --> profilwerte profil '3'

d1 = hmin - hsuw
hminn = 1000.

DO i = 1, nknot

  h1 (i) = h1 (i) - d1
  rau (i) = raul (i)
  IF (h1 (i) .lt.hminn) then
    isohl = i
    hminn = h1 (i)
  ENDIF

  !JK   SCHREIBEN IN KONTROLLFILE
  !JK   WENN BERECHNUNG NACH DARCY-WEISBACH
  IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
    if (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
      WRITE (UNIT_OUT_LOG, 30) i, x1 (i) , h1 (i) , rau (i) , ax (i) , ay (i) , dp (i)
    ELSE
      WRITE (UNIT_OUT_LOG, 31) i, x1 (i) , h1 (i) , rau (i)
    ENDIF
  ENDIF

END DO

30 format (1X, 'i=', I3,'  x1=', F10.3, '  h1=', F10.3, '  rau=', F8.3, &
            &  'ax=', F6.2, '  ay=', F6.2, '  dp=', F6.2)
31 format (1X, 'i=', I3,'  x1=', F10.3, '  h1=', F10.3, '  rau=', F8.3)

hsohl = hmin - d1
hmax = hmax - d1

ianf = 2
iend = nknot - 1

boli = h1 (ianf)
bore = h1 (iend)

hokmax = hokmax - d1
hukmax = hukmax - d1

hrbv = hokmax
hming = hming - d1

hmin = hmin - d1

itrlio = itrli
itrreo = itrre

itrli = iokl + 1
itrre = iokl + npl

!**      itrli = 1
!**      itrre = nknot

htrli = h1 (itrli)
htrre = h1 (itrre)

!JK   SCHREIBEN IN KONTROLLFILE
IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  WRITE (UNIT_OUT_LOG, 32) itrli, itrre
ENDIF
  32 format (/1X, 'ITRLI (Trennflaeche links):  ', I3,/,&
            & 1X, 'ITRRE (Trennflaeche rechts): ', I3)

!write (UNIT_OUT_LOG,*) 'Zeile 423 in brkon, Starte NORMBER!'


!WP Die urspruengliche Profilgeometrie wird wiederhergestellt.
!do i = 1, maxkla
!  x1_bruecke(i) = x1(i)
!  h1_bruecke(i) = h1(i)
!  s_bruecke(i)  = s(i)
!  x1(i) = x1_orig(i)
!  h1(i) = h1_orig(i)
!  s(i)  = s_orig(i)
!end do

CALL normber (str1, q, q1, nprof, hr, hv, rg, hvst, hrst, indmax, &
            & psieins, psiorts, hgrenz, ikenn, froud, nblatt, nz)

!WP Die urspruengliche Profilgeometrie wird wiederhergestellt.
!do i = 1, maxkla
!  x1(i) = x1_bruecke(i)
!  h1(i) = h1_bruecke(i)
!  s(i) = s_bruecke(i)
!end do



!write (UNIT_OUT_LOG,*) 'Zeile 429 in brkon, Ende NORMBER!'

CALL speicher (nprof, hr, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)

WRITE (UNIT_OUT_TAB, '(/,t10,'' Profil "3":'')')
nz = nz + 2
IF (nz.gt.50) then
  nblatt = nblatt + 1
  CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
ENDIF

CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat, &
         & UNIT_OUT_PRO, idr1, Q_Abfrage, nr_q_out)


IF (nz.gt.50) then
  nblatt = nblatt + 1
  CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
ENDIF


he3 = hen (nprof)

hr3 = hr

!**      print *
!**      print *,'wsp in profil 3 '
!**      print *,'- sohlhoehe : ',hsohl
!**      print *,'- wasserspiegel : ', hr


! Wiederherstellen der urspruenglichen profilwerte:
nprof = nprof + 1
stat (nprof) = staso

!write (*,*) ' In BR_KONV. Vor call INTDAT'

CALL intdat (staso, ifehl)

!***********************************************************************
!**
!**    test nur fuer impulsberechnung
!**    ==> Q-W Schleife bleibt weg
!**
!**       goto 200
!**
!    - kontrolle, ob schiessende stroemung in profil 3
!     (fuer freispiegelabfluss)

!     froud aus normber

!** schiessen      if (froud.ge.1.and.hr.lt.hukmax) then

!**       if (froud .ge. 1.) goto 101

!         - weiter mit grenztiefe im ow --> profil '1'
!**          print *,'froud = ',froud
!          print *,'--> weiter mit grenztiefe im ow (profil 1))'

!          call abskst(nknot,x1,xi,h1,hi,s)
!          print *,'aufruf von grnzh in bruecke '
!          call grnzh (q,indmax,hgrenz,xi,hi,s,ifgrnz)
!          hr1=hgrenz+d1
!          hsohl=hsohl+d1
!          print *,'sohlhoehe profil 1: ',hsohl
!          print *,'wsp = grenztiefe : ',hr1
!          froud=1.
!          ikenn=1
!          print *,'in bruecke mit stat = ',staso
!          print *,'ow-profil --> ',(staso-str1/1000.)
!          stat(nprof)=staso

!** schiessen         nz = nz+4
!** schiessen         if (nz.gt.50)  then
!** schiessen            nblatt=nblatt+1
!** schiessen            call kopf(nblatt,nz,jw5,jw7,idr1)
!** schiessen          endif


!** schiessen         write(jw5,15)
!** schiessen15  format(/,t10,'schiessende stroemung im uw-profil/pr."3"
!** schiessen    +       ',/,t10,'--> weiter mit Impulsberechung  ')


!** schiessen          goto 200
!** schiessen      endif
!
!      stroemen:


!**      if (hr3.ge.hukmax) then

!     brueckenberechnung konventionelle methode:

!        geometrie im uw-profil '3':

!**
!***********************************************************************


101 continue

!JK     SCHREIBEN IN KONTROLLFILE
WRITE (UNIT_OUT_LOG, '(/,''anfangsgeometrie im uw-profil'',/)')

!WP
!write (UNIT_OUT_LOG,*) 'Zeile 533 in brkon, Starte GEOMET!'


CALL geomet (hr3, x1, h1, hdif, hsohl, nknot, il, ir, q, wl3, rhy, ifehl2)
!WP
!write (UNIT_OUT_LOG,*) 'Zeile 538 in brkon, Ende GEOMET!'


aue3 = aue

! energiehoehe im uw --> he3

vm = q / fges
!**  he3=hr3+vm*vm/2./9.81
!  schaetzung wsp in profil 1: hr1=he1

IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  write (UNIT_OUT_LOG,33) aue3, vm, q, fges
  33 format (1X, 'AUE3 = ', F12.5, '  VM = ', F12.5, ' q = ', F12.5, ' FGES = ', F12.5)
ENDIF


! 1.te Schaetzung: he1=he3
! ------------------------
! Energiehoehe am Profil 1 (Brucke oberstrom) ist gleich Energiehoehe
! am Profil 3 (Bruecke unterstrom).
he1 = he3

! Bestimmen der massgebenden hoehe 'hmass', die im
! Iterationszyklus nicht unterschritten werden darf :

IF (hr3 .gt. hukmax) then
  hmass = hr3
ELSE
  hmass = hukmax
ENDIF

hea = he3 + dx
heb = he3

IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  WRITE (UNIT_OUT_LOG, '(//,'' q = '',f6.2,'' hukmax = '',f6.2)') q, hukmax
  write (UNIT_OUT_LOG, 34) he1, he3, hr3, hea, heb
  34 format (1X, 'HE1 (Energiehoehe am Profil 1):     ', F12.5, /, &
           & 1X, 'HE3 (Energiehoehe am Profil 3):     ', F12.5, /, &
           & 1X, 'HR3 (=hr, Wasserspiegel am Profil): ', F12.5, /, &
           & 1X, 'HEA (Energiehoehe am Profil A):     ', F12.5, /, &
           & 1X, 'HEB (Energiehoehe am Profil B):     ', F12.5 )
ENDIF

ibiter = 0
itbmax = 30



1015 CONTINUE

CALL geomet (heb, x1, h1, hdif, hsohl, nknot, il, ir, q, wl, rhy, ifehl2)

IF (ifehl2.ne.0) then

  ibiter = ibiter + 1
  !JK  SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(''Fehler bei der Abarbeitung von Geometrie!'')')
  WRITE (UNIT_OUT_LOG, '(''stop in bruecke_konv'')')
  WRITE (UNIT_OUT_LOG, '(''Korrektur der Hoehe heb um -dx/10.'')')

  heb = heb - dx / 10.
  IF (ibiter.gt.itbmax) then
    !JK  SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '(''auch nach wiederholter Korrektur.'')')
    WRITE (UNIT_OUT_LOG, '(''Daher Programmabbruch'')')
    STOP 'br_konv 2'
  ENDIF
  !JK  NOCHMALIGER DURCHLAUF NACH KORREKTUR DER HOEHE
  GOTO 1015

ENDIF


! qges aus summe(qd,qw):
isch = isch + 1


!MD  Hier ist die Uebergabe des Wasserspiegels hr3 an die Abflussberechnung
!MD    nicht Uebergabe der Energiehoehe he3 erforderlich!!!!!!!!!!!
!MD  -------------------------------------------------------------------
!MD  CALL abfluss (ad, aue, apg, wl, rhy, raub, breite, heb, he3, qd,  &
!MD            & qw, qgesb, aue3, wl3, iartb)

CALL abfluss (ad, aue, apg, wl, rhy, raub, breite, heb, hr3, qd,  &
            & qw, qgesb, aue3, wl3, iartb)

!JK      SCHREIBEN IN KONTROLLFILE
IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  WRITE (UNIT_OUT_LOG, '(/'' i   ad    aue    wl    he1     qd      qw      qges'')')
  WRITE (UNIT_OUT_LOG, '(i2,1x,7(f8.3,1x))') isch, ad, aue, wl, heb, qd, qw, qgesb
ENDIF

ibiter = 0



1016 CONTINUE

CALL geomet (hea, x1, h1, hdif, hsohl, nknot, il, ir, q, wl, rhy, ifehl2)

IF (ifehl2.ne.0) then

  ibiter = ibiter + 1
  !JK  SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '(''Fehler bei der Abarbeitung von Geometrie !'')')
  WRITE (UNIT_OUT_LOG, '(''stop in bruecke_konv'')')
  WRITE (UNIT_OUT_LOG, '(''Korrektur der Hoehe hea um dx/10.'')')

  hea = hea + dx / 10.

  IF (ibiter.gt.itbmax) then
    !JK   SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '(''auch nach wiederholter Korrektur.'')')
    WRITE (UNIT_OUT_LOG, '(''Daher Programmabbruch'')')
    STOP 'br_konv 3'
  ENDIF
  !JK   NOCHMALIGER DURCHLAUF NACH KORREKTUR DER HOEHE
  GOTO 1016

ENDIF

! qges aus summe(qd,qw):
isch = isch + 1

CALL abfluss (ad, aue, apg, wl, rhy, raub, breite, hea, he3, qd,  &
            & qw, qgesa, aue3, wl3, iartt)

!JK  SCHREIBEN IN KONTROLLFILE
IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
  WRITE (UNIT_OUT_LOG, '(/'' i   ad    aue    wl    he1     qd      qw      qges'')')
  WRITE (UNIT_OUT_LOG, '(i2,1x,7(f8.3,1x))') isch, ad, aue, wl, hea, qd, qw, qgesa
ENDIF

heaa = - 1000.





!JK  ---------------------------------------------------------------------------------
!JK  ITERATIONSSCHLEIFE ZUR BERECHNUNG DER ENERGIEHOEHE
!JK  ---------------------------------------------------------------------------------

DO 100 i = 1, itmax_brkon

  !write (*,*) 'In BR_KONV. Iterationsschleife 100. i = ', i
  !write (*,*) 'In BR_KONV. iartt = ', iartt, ' iartb = ', iartb, ' aue = ', aue
  !write (*,*) 'In BR_KONV. dfq = ', dfq, ' dfh = ', dfh

  dfq = abs (q - qgesa)
  dfh = abs (heaa - heb)

  IF ( dfq .le. (q/100.) ) then

    !write (*,*) 'In BR_KONV. dfq < q/100!'

    ! Energiehoehe gefunden --> weiter mit hr1=he
    hr1 = hea

    IF (hr1.le.hukmax) then
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''energiehoehe tiefer hukmax --> weiter mit impuls'')')
      ENDIF

      !JK  ZUR IMPULSBERECHNUNG
      GOTO 200

    ENDIF

    IF (iartt.eq.0) then

      !JK   SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '('' --> weiter mit hr1 = '',f7.3)') hr1
        WRITE (UNIT_OUT_LOG, '('' (vollkommener ueberfall) '',/)')
      ENDIF

      qdiff = q / qgesa
      qw = qw * qdiff
      qd = qd * qdiff

      IF (iartb.eq.0) then

        !JK  UEBERFALLFLAECHE => DRUCKABFLUSS+WEHRABFLUSS
        IF (aue.gt.1.e-04) then

          nz = nz + 6

          IF (nz.gt.50) then
            nblatt = nblatt + 1
            CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          ENDIF

          WRITE (UNIT_OUT_TAB, 40) qw, qd, q
          40 format (//10X, 'Druck- und Wehrabfluss', /, &
                     & 10X, '----------------------', /, &
                     & 10X, 'Q-Wehr   = ', F10.3, /, &
                     & 10X, 'Q-Druck  = ', F10.3, /, &
                     & 10X, 'Q-Gesamt = ', F10.3)

        !JK  KEINE UEBERFALLFLAECHE => DRUCKABFLUSS
        ELSE
          nz = nz + 6

          IF (nz.gt.50) then
            nblatt = nblatt + 1
            CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          ENDIF

          WRITE (UNIT_OUT_TAB, 41) qd, q
          41 format (//10X, 'Druckabfluss', /, &
                     & 10X, '------------', /, &
                     & 10X, 'Q-Druck  = ', F10.3, /, &
                     & 10X, 'Q-Gesamt ) ', F10.3)

        ENDIF

      ENDIF

      iwehr = 1

      !write (*,*) 'In BR_KONV. Ende (iartt == 0)'
      !JK  ZU VOLLKOMMENEM UEBERFALL
      GOTO 9999

    !JK  ELSE ZU (iartt.eq.0)
    ELSE

      !JK  SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''unvollkommener ueberfall'',/)')
        WRITE (UNIT_OUT_LOG, '(''--> weiter mit impuls'',/)')
      ENDIF

      !JK  ZU IMPULSBERECHNUNG
      GOTO 200

    !JK  ENDIF ZU (iartt.eq.0)
    ENDIF


  !JK  ELSEIF ZU (dfq.le.q/100.)
  ELSEIF (abs (dfh) .le.0.01) then

    ! Iteration hat in Bezug auf Wasserspiegel konvergiert.
    hr1 = heb

    IF (hr1 .le. hukmax) then

      !JK   SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''energiehoehe tiefer hukmax --> weiter mit impuls'')')
      ENDIF
      !JK  ZU IMPULSBERECHNUNG
      GOTO 200

    ENDIF

    IF (iartt.eq.0) then

      !JK    SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        write (UNIT_OUT_LOG, 42) hr1
        42 format (1X, 'Konvergenz im Wasserspiegel erreicht!', /, &
                 & 1X, '--> weiter mit hr1 = ',f12.5, /, &
                 & 1X, '(vollkommener Ueberfall)',/)
      ENDIF

      qdiff = q / qgesa
      qw = qw * qdiff
      qd = qd * qdiff

      IF (iartb .eq. 0) then

        !JK  UEBERFALLFLAECHE => DRUCKABFLUSS+WEHRABFLUSS
        IF (aue.gt.1.e-04) then
          nz = nz + 6

          IF (nz.gt.50) then
            nblatt = nblatt + 1
            CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          ENDIF

          WRITE (UNIT_OUT_TAB, 43) qw, qd, q
          43 format (//10X, 'Druck- und Wehrabfluss', /, &
                     & 10X, '----------------------', /, &
                     & 10X, 'Q-Wehr   = ', F10.3, /, &
                     & 10X, 'Q-Druck  = ', F10.3, /, &
                     & 10X, 'Q-Gesamt = ', F10.3)

        !JK  KEINE UEBERFALLFLAECHE => DRUCKABFLUSS
        ELSE

          nz = nz + 6

          IF (nz .gt. 50) then
            nblatt = nblatt + 1
            CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          ENDIF

          WRITE (UNIT_OUT_TAB, 44) qd, q
          44 format (//10X, 'Druckabfluss', /, &
                     & 10X, '------------', /, &
                     & 10X, 'Q-Druck  = ', F10.3, /, &
                     & 10X, 'Q-Gesamt ) ', F10.3)

        ENDIF

      ENDIF

      iwehr = 1

      !JK  ZU VOLLKOMMENER UEBERFALL
      GOTO 9999

    !JK  ELSE ZU (iartt.eq.0)
    ELSE

      !JK   SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(''unvollkommener ueberfall'',/)')
        WRITE (UNIT_OUT_LOG, '(''--> weiter mit impuls'',/)')
      ENDIF

      !JK   ZU IMPULSBERECHNUNG
      GOTO 200

    !JK  ENDIF ZU (iartt.eq.0)
    ENDIF


  !JK  ELSEIF ZU (dfq.le.q/100.)
  ELSEIF (i.eq.1 .and. qgesa.gt.q) then

    ! keine konvergenz moeglich, da die q-anteile mit
    ! steigendem wsp steigen --> he1 zwischen hukmax und
    ! he3 ==> weiter mit impuls

    !JK  SCHREIBEN IN KONTROLLFILE
    write (UNIT_OUT_LOG, 45) hukmax, he3
    45 format (1X, 'Keine Konvergenz moeglich, da die q-anteile ', /, &
             &     'mit steigendem WSP steigen!', /, &
             & 1X, '-> HE1 muss zwischen HUKMAX und HE3 liegen!', /, &
             & 1X, '   (HUKMAX = ', F12.5, '  HE3 = ', F12.5, ')', /, &
             & 1X, '-> Weiter mit Impulsberechnung.')

    !JK  ZU IMPULSBERECHNUNG
    GOTO 200

  !JK   ELSE ZU (dfq.le.q/100.)
  ELSE

    ! Schaetzung neue energiehoehe:
    qdif2 = qgesb - q

    110 CONTINUE

    qdif1 = qgesa - q

    !JK  WENN NULLSTELLE
    IF ( (qdif1.gt.0.and.qdif2.lt.0) .or. (qdif1.lt.0.and.qdif2.gt.0) ) then

      heaa = hea
      dfh = abs (heaa - heb)

      IF (abs (dfh) .le.0.01) then
        ! Iteration hat in Bezug auf Wasserspiegel konvergiert
        hr1 = heb

        IF (hr1 .le. hukmax) then

          !JK   SCHREIBEN IN KONTROLLFILE
          IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(''energiehoehe tiefer hukmax -->'', &
                 & ''weiter mit impuls'')')
          ENDIF
          !JK   ZU IMPULSBERECHNUNG
          GOTO 200

        ENDIF

        IF (iartt.eq.0) then

          !JK   SCHREIBEN IN KONTROLLFILE
          IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(''Konvergenz im Wasserspiegel erreicht.'')')
            WRITE (UNIT_OUT_LOG, '('' --> weiter mit hr1 = '',f7.3)') hr1
            WRITE (UNIT_OUT_LOG, '('' (vollkommener ueberfall) '',/)')
          ENDIF

          qdiff = q / qgesa
          qw = qw * qdiff
          qd = qd * qdiff

          IF (iartb.eq.0) then

            !JK  UEBERFALLFLAECHE => DRUCKABFLUSS + WEHRABFLUSS
            IF (aue.gt.1.e-04) then
              nz = nz + 6

              IF (nz.gt.50) then
                nblatt = nblatt + 1
                CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
              ENDIF

              WRITE (UNIT_OUT_TAB, '(//t10,'' Druckabfluss '',//  &
                &             t10,'' Q-Druck= '',f8.2,// &
                &                ,''Q-gesamt = '',f8.2)') qd, q

              WRITE (UNIT_OUT_TAB, '(//t10,'' Druck- und Wehrabfluss '', / &
                        &     t10,'' Q-Wehr = '',f8.2,'' Q-Druck= '',f8.2,// &
                        &         ,''Q-gesamt = '',f8.2)') qw, qd, q

            !JK   KEINE UEBERFALLFLAECHE => DRUCKABLUSS
            ELSE
              nz = nz + 6

              IF (nz.gt.50) then
                nblatt = nblatt + 1
                CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
              ENDIF

              WRITE (UNIT_OUT_TAB, '(//t10,'' Druckabfluss '',//   &
                     &        t10,'' Q-Druck= '',f8.2,//  &
                     &        ,''Q-gesamt = '',f8.2)') qd, q
            ENDIF

          ENDIF

          iwehr = 1

          !JK  ZU VOLLKOMMENEN UEBERFALL
          GOTO 9999

        !JK   ELSE ZU (iartt.eq.0)
        ELSE

          !JK  SCHREIBEN IN KONTROLLFILE
          IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '(''unvollkommener ueberfall'',/)')
            WRITE (UNIT_OUT_LOG, '(''--> weiter mit impuls'',/)')
          ENDIF

          !JK   ZU IMPULSBERECHNUNG
          GOTO 200

        !JK   ENDIF ZU (iartt.eq.0)
        ENDIF

      !JK  ENDIF ZU (abs(dfh) .le. 0.01)
      ENDIF

      hea = hea + 0.1 * (heb - hea)

      CALL geomet (hea, x1, h1, hdif, hsohl, nknot, il, ir, q, wl, rhy, ifehl2)

      isch = isch + 1

      CALL abfluss (ad, aue, apg, wl, rhy, raub, breite, hea, he3, &
             & qd, qw, qgesa, aue3, wl3, iartt)

      !JK   SCHREIBEN IN KONTROLLFILE
      IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
        WRITE (UNIT_OUT_LOG, '(/'' i   ad    aue    wl    he1     qd      qw      qges'')')
        WRITE (UNIT_OUT_LOG, '(i2,1x,7(f8.3,1x))') isch, ad, aue, wl, hea, qd, qw, qgesa
      ENDIF

      !JK  ZU NULLSTELLE
      GOTO 110

    !JK   ELSE ZU NULLSTELLE
    ELSE

      df = (qgesa - qgesb) / (hea - heb)
      heb = hea
      iartb = iartt
      qgesb = qgesa

      IF (abs (df) .lt.1.e-04) then

        !JK  SCHREIBEN IN KONTROLLFILE
        IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(''df--> 0! --> he1=he1+0.1'')')
        ENDIF
        hea = hea + 0.1

      ELSE

        ! Geradengleichung der tangente und schnitt mit Sollwert q:
        hea = hea + (q - qgesa) / df

        IF (heaa.gt.0) then
          IF (heaa.lt.hea) hea = heaa
        ENDIF

      ENDIF

    !JK  ENDIF ZU NULLSTELLE
    ENDIF

    IF (hea.lt.hmass) hea = hmass

    CALL geomet (hea, x1, h1, hdif, hsohl, nknot, il, ir, q, wl, rhy, ifehl2)

    isch = isch + 1
    CALL abfluss (ad, aue, apg, wl, rhy, raub, breite, hea, he3,  &
     & qd, qw, qgesa, aue3, wl3, iartt)

    !JK  SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(/'' i   ad    aue    wl    he1     qd      qw      qges'')')
      WRITE (UNIT_OUT_LOG, '(i2,1x,7(f8.3,1x))') isch, ad, aue, wl, hea, qd, qw, qgesa
    ENDIF
  ENDIF

  IF (abs (hea - hmass) .le.1.e-04.and.abs (heb - hmass) .le.1.e-04) then
    ! Keine konvergenz moeglich, da  he1 unter hukmax liegt
    ! he3 ==> weiter mit impuls

    !JK    SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '(''keine konvergenz moeglich, da  '',        &
      &  ''he1 unter hukmax liegt '',/,                       &
      &  '' ==> he1 muss zwischen hukmax und he3 liegen : '', &
      &  /,''hukmax = '',f6.2,'' he3 = '',f6.2,/,             &
      &  '' ==> weiter mit impuls '')') hukmax, hea

    !JK  ZU IMPULSBERECHNUNG
    GOTO 200

  !JK   ENDIF ZU (dfq.le.q/100.)
  ENDIF

!JK   ENDE ITERATION ---------------------------------------------                      
100 END DO
                                                                        


ifkonv = 10000

!JK   SCHREIBEN IN KONTROLLFILE
WRITE (UNIT_OUT_LOG, '(''maximale anzahl der iterationsschritte ueberschritten ==> weiter mit impuls !'')')



! Brueckenberechnung mit impulsgleichung:

200 CONTINUE

!write (*,*) 'In BR_KONV. Impulsberechnung am Profil 3'

! ************************************************
! Impulsberechnung profil '3'
! ************************************************
iart = 3
iartt = 0

! /* kennung fuer die art der Impulsgleichung

! print *,'impulsberechnung profil 3'

IF ( (hr3 - hsohl) .le.0.0001) then

  hsohl = hmin

  !JK        SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '(''wasserspiegel unterhalb tiefster'')')
    WRITE (UNIT_OUT_LOG, '(''sohlenkoordinate in bruecke (profil 3 - uw) -->'')')
    WRITE (UNIT_OUT_LOG, '(''weiter mit grenztiefe im ow-profil 1'')')
  ENDIF


  CALL abskst (nknot, x1, xi, h1, hi, s, maxkla)
  !write (*,*) 'In BR_KONV. Aufruf von GRNZH.'
  CALL grnzh (q, indmax, hgrenz, xi, hi, s)

  CALL abskst (nknot, x1, xi, h1, hi, s, maxkla)

  ikenn = 1
  froud = 1.
  hr1 = hgrenz

  ! print *,'sohlhoehe profil 1: ',hsohl
  ! print *,'wsp = grenztiefe : ',hr1

  nz = nz + 5

  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  WRITE (UNIT_OUT_TAB, 11) (staso - str1 / 1000.), hr1
  11 FORMAT (/,t10,'wsp unterhalb tiefstem Sohlpkt. uw (profil "3") &
          &  ',t10,'--> weiter mit Grenztiefe im ow    (profil "1") &
          &  ',/,f10.3,f11.3)

  !JK   ZU VOLLKOMMENER UEBERFALL
  GOTO 9999

!JK    ELSE ZU ((hr3-hsohl).le.0.0001)
ELSE

  !** hr3=hr
  CALL impuls (hr3, cd, x1, h1, hdif, hmin, nknot, il, ir, q, &
   & iart, m3, a3, ifehl)

  IF (ifehl.ne.0) then
    PRINT * , 'kein durchflussquerschnitt --> stop in bruecke!'
    STOP
  ENDIF

  nz = nz + 8
  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  WRITE (UNIT_OUT_TAB, '(/,t10,''Profil "3": hr  m  a'',/,t10,3f10.3)') &
   & hr3, m3, a3
  WRITE (UNIT_OUT_TAB, '(/,t10,''ad  aue apl  apg '',/,t10,4f10.3)') &
   & ad, aue, apl, apg

  !JK   SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '(/,t10,''profil "3": hr  m  a'',/,t10,3f10.3)') hr3, m3, a3
    WRITE (UNIT_OUT_LOG, '(/,t10,''ad  aue apl  apg '',/,t10,4f10.3)') ad, aue, apl, apg
  ENDIF

  !JK        WENN DRUCKABFLUSS
  IF (hr3.ge.hukmax) then

    nz = nz + 2
    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF

    WRITE (UNIT_OUT_TAB, 23)
    IF (idr1.eq.'j') write (UNIT_OUT_PRO, 23)

    WRITE (UNIT_OUT_LOG, 23)
    23 FORMAT (/,10x,'Druckabfluss')

    !JK   ZU IMPULSBERECHNUNG PROFIL '1'
    GOTO 8000

  ENDIF


  ! ************************************************
  ! impulsberechnung profil '2'
  ! *************************************************
  ! impulsberechnung in profil '2' --> kontrolle der
  ! fliesszustaende im brueckenbereich

  ! mittels impuls m3 auf profil '3' -->  berechnung der fliess-
  ! tiefe in profil '2' mittels impulsgleichung:
  !               m3 = m2 !!

  ! Iterativer prozess:
  hdif = hokmax - hsuw


  ! print *,'aufruf der subroutine iteration in bruecke'

  ! Ansetzen wsp in profil '3' als anfangs-wsp in profil '2'
  ! (1.te schaetzung)

  !**  hr=hr3

  hr = hokmax + 0.5

  CALL iterat (hr, hdif, hmin, 2, cd, a3, a2, x1, h1, nknot, il,  &
   & ir, q, m3, m2, staso)


  hr2 = hr
  nz = nz + 8
  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  WRITE (UNIT_OUT_TAB, '(/,t10,''Profil "2": hr  m  a'',/,t10,3f10.3)') &
   & hr2, m2, a2
  WRITE (UNIT_OUT_TAB, '(/,t10,''ad  aue apl  apg '',/,t10,4f10.3)') &
   & ad, aue, apl, apg
  !**           print *,'profil 2:'
  !**           print *,'impuls am profil 3 m3=',m3
  !**           print *,'impuls am profil 2 m2=',m2
  !**           print *,'                  hr2=',hr2

  !JK   WENN DRUCKABFLUSS
  IF (hr2.ge.hukmax) then

    nz = nz + 2
    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF

    WRITE (UNIT_OUT_TAB, 24)
    IF (idr1.eq.'j') write (UNIT_OUT_PRO, 24)
    24 FORMAT     (/,10x,'Druckabfluss')

  ENDIF

  ! ************************************************
  ! impulsberechnung profil '1'
  ! *************************************************

  ! mittels impuls m3 auf profil '3' -->  berechnung der fliess-
  ! tiefe in profil '1' mittels impulsgleichung:
  ! m3 = m1 !!

  !  profil '1' --> ow-profil !!

  8000 continue

  hdif = hokmax - hsohl

  IF (hr2.ne.0.) then
    hr = hokmax + 0.5
    a = a2
    m = m2
  ELSE
    hr = hokmax + 0.5
    a = a3
    m = m3
  ENDIF

  !JK  SCHREIBEN IN KONTROLLFILE
  WRITE (UNIT_OUT_LOG, '('' in iteration :'')')

  CALL iterat (hr, hdif, hsohl, 1, cd, a, a1, x1, h1, nknot, il,  &
   & ir, q, m, m1, staso)

  hr1 = hr
  nz = nz + 8

  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
    WRITE (UNIT_OUT_TAB, '(/,t10,''Profil "1": hr  m  a'',/,t10,3f10.3)') &
     & hr1, m1, a1
    WRITE (UNIT_OUT_TAB, '(/,t10,''ad  aue apl  apg '',/,t10,4f10.3)') &
     & ad, aue, apl, apg
    !JK  SCHREIBEN IN KONTROLLFILE
    WRITE (UNIT_OUT_LOG, '(/,t10,''profil "1": hr  m  a'',/,t10,3f10.3)') hr1, m1, a1
    WRITE (UNIT_OUT_LOG, '(/,t10,''ad  aue apl  apg '',/,t10,4f10.3)') ad, aue, apl, apg
  ENDIF

  IF (hr1.ge.hukmax.or.hr2.ge.hukmax.or.hr3.ge.hukmax) then

    !write (*,*) 'In BR_KONV. Typ = Druckabfluss'

    !  druckabfluss

    !JK   SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '(''druckabfluss !!'',/,''ermittelt hr1 aus impuls: '',f10.2)') hr1
      WRITE (UNIT_OUT_LOG, 17)
    ENDIF

    nz = nz + 2
    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF

    WRITE (UNIT_OUT_TAB, 17)
    17 FORMAT (/,t10,'Druckabfluss',/, &
              &  t10, '--> wsp im ow (profil "1") ermittelt aus Impulsbilanz')
    IF (nz.gt.50) then
      nblatt = nblatt + 1
      CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
    ENDIF
    nz = nz + 2

  !JK      ELSE ZU DRUCKABFLUSS
  ELSE

    !write (*,*) 'In BR_KONV. Typ = kein Druckabfluss'

    ! Kontrolle, ob schiessende stroemung in '2'

    !JK  SCHREIBEN IN KONTROLLFILE
    IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
      WRITE (UNIT_OUT_LOG, '('' profil 1: '',/,''impuls m1: '',f7.3,/,   &
         &  '' gesamt durchstroemte flaeche (aue+ad) '',f7.3)') m1, a1
    ENDIF

    tm = (ad+aue) / bnetto
    vm = q / (ad+aue)
    froud = vm * vm / (tm * 9.81)
    froud = sqrt (froud)

    IF (froud.ge.1.) then
      ifroud = 1

      ! Schiessende stroemung in profil '2'
      ! --> hr1 aus impulsgleichung 1 bleibt wasserstand

      WRITE (UNIT_OUT_TAB, 12)
      12 FORMAT (/,10X, 'schiessende Stroemung in Profil "2"', /, &
                 & 10X, '--> wsp im ow (profil "1") ermittelt aus Impulsbilanz')

      IF (nz.gt.50) then
        nblatt = nblatt + 1
        CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
      ENDIF

      nz = nz + 2
      CONTINUE

    !JK        ELSE ZU (froud.ge.1.)
    ELSE

      ! Berechung der fliesstiefe mit hilfe der yarnell-equation:

      f = aue+ad

      CALL yarnell (hpf, f, q, xk)

      hr1n = hr3 + hpf

      ! how=hr1n-hmin
      ! delpf=100.
      ! if(how.gt.0.0001)delpf=hpf/how
      ! if (hpf .gt. 0.001.and.delpf.lt.0.50) then

      IF (hpf.gt.0.001) then

        !JK   SCHREIBEN IN KONTROLLFILE
        IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
          WRITE (UNIT_OUT_LOG, '(/,''berechnung der fliesstiefe mit yarnell:'' &
            &      /,'' => pfeilerstau aus yarnell = '',f7.3)') hpf
          WRITE (UNIT_OUT_LOG, '('' --> wsp profil 1 = max (hr3+hpf,hr1): '',/ &
            &        ''        = max ('',2(f7.3,1x),'')'')') hr1n, hr1
        ENDIF

        !**  if (hr1n.ge.hr1) then
        IF (hr1n.ge.hr1.AND.hr1n.le.hukmax) then

          !JK  SCHREIBEN IN KONTROLLFILE
          IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
            WRITE (UNIT_OUT_LOG, '('' --> wsp profil 1 = max (hr3+hpf, hr1): '',/, &
                          & '' = max ('',2(f7.3,1x),'')'')') hr1n, hr1
          ENDIF

          hr1 = hr1n
          write (UNIT_OUT_LOG, 14) hpf
          nz = nz + 4

          IF (nz.gt.50) then
            nblatt = nblatt + 1
            CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          ENDIF

          WRITE (UNIT_OUT_TAB, 14) hpf
          14 FORMAT (/10X, 'WSP im ow (Profil "1") ermittelt aus der YARNELL-Gleichung',/, &
                    & 10X, '--> Pfeilerstau aus YARNELL: ', F7.3)
        ELSE

          !JK  SCHREIBEN IN KONTROLLFILE
          write (UNIT_OUT_LOG, 13)

          nz = nz + 2

          IF (nz.gt.50) then
            nblatt = nblatt + 1
            CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
          ENDIF

          WRITE (UNIT_OUT_TAB, 13)

        ENDIF

      !JK  ELSE ZU (hpf.gt.0.001)
      ELSE

        !JK  SCHREIBEN IN KONTROLLFILE
        write (UNIT_OUT_LOG, 13)

        nz = nz + 2

        IF (nz.gt.50) then
          nblatt = nblatt + 1
          CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
        ENDIF

        WRITE (UNIT_OUT_TAB, 13)
        13 FORMAT (/,t10,'wsp im ow (Profil "1") ermittelt aus Impulsbilanz')

      !JK   ENDIF ZU (hpf.gt.0.001)
      ENDIF

    !JK    ENDIF ZU (froud.ge.1.)
    ENDIF

  !JK   ENDIF ZU DRUCKABFLUSS
  ENDIF

!JK    ENDIF ZU ((hr3-hsohl).le.0.0001)
ENDIF



9999 CONTINUE

!write (*,*) 'In BR_KONV. Sprungmarke 9999. IARTT = ', iartt


IF (iartt.eq.2) then
  ! *****************************************************************
  ! Unvollkommener ueberfall
  ! *****************************************************************

  !JK    SCHREIBEN IN KONTROLLFILE
  IF (Q_Abfrage.ne.'BR_SCHLEIFE') then
    WRITE (UNIT_OUT_LOG, '('' unvollkommener ueberfall --> it. normber.'')')
  ENDIF

  nz = nz + 1
  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  WRITE (UNIT_OUT_TAB, '(t10,''unvollkommener ueberfall '')')

  ! Common-block fuer die gewaessergeometrie uberschreiben ((
  ! x1,h1,rau,boli,bore .... festlegen

  hmin = 10000.
  nknot = nok

  DO i = 1, nok
    h1 (i) = hok (i)
    IF (hmin.gt.h1 (i) ) then
      hmin = h1 (i)
      isohl = i
    ENDIF
    x1 (i) = xok (i)
    rau (i) = raub
  END DO

  hsohl = hmin
  ianf = 1

  iend = nknot
  boli = h1 (ianf)
  bore = h1 (iend)
  hrbv = max (boli, bore)
  hming = hmin
  iming = isohl

  strbr = breite / 2.

  CALL normber (strbr, q, q1, nprof, hr, hv, rg, hvst, hrst,      &
   & indmax, psieins, psiorts, hgrenz, ikenn, froud, nblatt, nz)

  stat (nprof) = stat (nprof) - strbr / 1000.

  ikenn = ikenn + ifkonv

  CALL speicher (nprof, hr, hv, hvst, hrst, q, stat (nprof), indmax, ikenn)

  ! ****************************************************************
  ! Ausdrucken der ergebisse im .tab-file
  ! ****************************************************************

  nz = nz + 2

  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  WRITE (UNIT_OUT_TAB, '(/,t10,'' profil "2":'')')
  CALL drucktab (nprof, indmax, nz, UNIT_OUT_TAB, nblatt, stat,  &
              & UNIT_OUT_PRO, idr1, Q_Abfrage, nr_q_out)


  IF (nz.gt.50) then
    nblatt = nblatt + 1
    CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
  ENDIF

  ! Wiederherstellen der urspruenglichen profilwerte:

  nprof = nprof + 1
  stat (nprof) = stat (nprof - 1) + strbr / 1000.
  CALL intdat (staso, ifehl)

  CALL linier (hokmax, x1, h1, rau, nknot, iwl, iwr, npl, xl, hl, raul)

  h1 (1) = hokmax
  ! x1(1) bleibt
  rau (1) = raub
  h1 (npl + 2) = hokmax
  x1 (npl + 2) = x1 (nknot)
  rau (npl + 2) = raub

  nknot = npl + 2

  DO i1 = 1, npl
    x1 (i1 + 1) = xl (i1)
    h1 (i1 + 1) = hl (i1)
    rau (i1 + 1) = raul (i1)
  END DO

  rau (npl + 1) = raub
  hsohl = hmin
  hmax = hokmax

  ianf = 2

  iend = nknot - 1
  boli = h1 (ianf)
  bore = h1 (iend)
  hrbv = hokmax
  hming = hsohl

  CALL normber (strbr, q, q1, nprof, hr, hv, rg, hvst, hrst,   &
   &  indmax, psieins, psiorts, hgrenz, ikenn, froud, nblatt, nz)


  ikenn = ikenn + ifkonv

!JK    ELSE ZU (iartt.eq.2)
ELSE

  !write (*,*) 'In BR_KONV. Ende Berechnung Brueckenprofil.'

  ! **************************************************************
  ! Vollkommener ueberfall oder impulsberechnung
  ! **************************************************************

  hr = hr1

ENDIF                                                                            

END SUBROUTINE br_konv
