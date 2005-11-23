!     Last change:  WP   26 Aug 2005   10:31 am
!--------------------------------------------------------------------------
! This code, bovog.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - bovog1
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
SUBROUTINE bovog1 (iw11, iw12, nbv, qvar, rqmax, rqmin)
!
! IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN
! ---------------------------------------------------
! Br_konv
! Cwsub
! cwun    --      Formwiderstandsbeiwert eines einzeln stehenden
!                 Kreiszylinders
! dp      --      Bewuchsparameter
! rep     --      Reynoldszahl für Bewuchs
! vrep    --      mittlere Geschwindigkeit in der Bewuchszone
!
!
! AUFGERUFENE ROUTINEN
! --------------------
! linreg (xsp,hsohle,lz,byx,xq,yq,xww,yww)
! probv1(l,iw12,st_o,st_u,q_fl_bv,hbord(l),qvar,lif,
!        kif,liv,kiv,qges,bges,wspgef,c(l),slhngg)
!-----------------------------------------------------------------------

!------------------------------------------------------------------
! VEREINBARUNGSTEIL
!------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN

! Calling variables
INTEGER, INTENT(IN) :: iw11
INTEGER, INTENT(IN) :: iw12
INTEGER, INTENT(IN) :: nbv
REAL, INTENT(IN) :: qvar
REAL, INTENT(IN) :: rqmax
REAL, INTENT(IN) :: rqmin


! COMMON-Block /LAENGS/ -------------------------------------------------------
REAL 		:: bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL 		:: hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)
COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp
! -----------------------------------------------------------------------------


! COMMON-Block /PRO1/ ---------------------------------------------------------
REAL 	:: qbord(maxger), hbord (maxger), vbord (maxger)
INTEGER :: jkenn (maxger)
COMMON / pro1 / qbord, hbord, jkenn, vbord
!     Vorbelegung:
!     ------------
!     jkenn - kennung dafuer, ob der bordvoll wert ermittelt worden ist oder nicht
!     jkenn=0 - bordvoller abfluss noch nicht ermittelt
!     jkenn=1 - bordvoller abfluss ermittelt
! -----------------------------------------------------------------------------


! COMMON-Block /PROF/ ---------------------------------------------------------
REAL 		:: hwsp (100, maxger)
COMMON / prof / hwsp
! -----------------------------------------------------------------------------


! COMMON-Block /TEILDAT/ ------------------------------------------------------
REAL 		:: anftg (merg)
INTEGER 	:: iw13, numitg (merg), ikg
COMMON / teildat / iw13, anftg, numitg, ikg
! -----------------------------------------------------------------------------


! COMMON-Block /TEILGEBIET/ ---------------------------------------------------
INTEGER         :: ikitg, itg
COMMON / teilgebiet / ikitg, itg
! -----------------------------------------------------------------------------


! COMMON-Block /UEBER_KM/ -----------------------------------------------------
REAL 	:: qq (100, maxger), bb (100, maxger), fb (100, maxger)
REAL 	:: vfl (100, maxger), qvor (100, maxger), bvor (100, maxger)
REAL 	:: fvor (100, maxger)
INTEGER :: ischbv (maxger)
COMMON / ueber_km / qq, bb, fb, vfl, qvor, bvor, fvor, ischbv
! -----------------------------------------------------------------------------


! Local variables
CHARACTER(LEN=nch80) :: dummy

REAL 	:: hsohle (maxger), xsp (maxger)

REAL 	:: kf (maxger, 6), nf (maxger, 6), kv (maxger, 6), nv (maxger, 6)
REAL 	:: c (maxger), qg (maxger, 6), difsta (maxger), qbv (maxger)
REAL   	:: sta (maxger), dif1 (maxger), lif (6), kif (6), liv (6), kiv (6)
REAL  	:: qges (6), rkf (6), rkv (6), rnf (6), rnv (6), qqg (6), bges (6)
REAL 	:: bgg (maxger), wspgef (6), wspg (maxger, 6), hwg (maxger)
REAL  	:: bg (maxger, 6), hr (maxger)

INTEGER ianf (merg), iend (merg), ikenntg (merg)


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
iteil = 0
ianftg = 0

IF (ikitg.eq.0) then
  ianf (1) = 1
  iend (1) = nbv
  numitg (1) = itg
  ianftg = 1
  iendtg = 1

ELSEIF (stat (nbv) .le.anftg (1) ) then
  WRITE (iw12, '(''kein Teilgebiet im Abschnitt!!'')')
  WRITE (iw12, '(''K-M Parameter wird nicht bestimmt!'')')
  GOTO 2000
ELSEIF (stat (1) .ge.anftg (ikg) ) then

  ianf (ikg) = 1
  iend (ikg) = nbv
  ianftg = ikg
  iendtg = ikg

ELSEIF (ikg.eq.1) then
  i_b = 0
  DO j3 = 1, nbv - 1
    IF (anftg (1) .le.stat (j3) .and.i_b.eq.0) then
      ianf (1) = j3
      iend (1) = nbv
      ianftg = 1
      iendtg = 1
      i_b = 1
    ENDIF
  END DO

  IF (ianftg.eq.0) then
    WRITE (iw12, '(''kein Teilgebiet im Abschnitt!'')')
    WRITE (iw12, '(''K-M Parameter wird nicht bestimmt'')')
    GOTO 2000
  ENDIF

ELSE
  it1 = 0
  DO 320 j4 = 1, ikg
    DO 325 j3 = 2, nbv
      !**   19.11.98 Csocsan
      IF (j3.eq.2.and.anftg (j4) .le.stat (j3 - 1) ) then
        it1 = it1 + 1
        ianf (j4) = j3 - 1
        IF (it1.ge.1) ianftg = j4
        GOTO 320
      !****   ende
      ELSEIF (anftg (j4) .gt.stat (j3 - 1) .and.anftg (j4) .le.stat (j3) ) then
        it1 = it1 + 1
        ianf (j4) = j3
        IF (it1.eq.1) ianftg = j4
        GOTO 320
      ENDIF
    325 END DO
  320 END DO

  !**  19.11.98 Csocsan
  IF (ianftg.gt.1) then
    IF (stat (1) .ge.anftg (ianftg - 1) ) then
      ianftg = ianftg - 1
      ianf (ianftg) = 1
    ENDIF
  ENDIF
  !*** ende

  it1 = 0
  DO 330 j4 = ikg, 2, - 1
    DO 335 j3 = nbv, 2, - 1
      !**  19.11.98 Csocsan
      IF (j3.eq.nbv.and.anftg (j4) .gt.stat (j3) ) then
        it1 = it1 + 1
        iend (j4 - 1) = j3
        IF (it1.ge.1) then
          iendtg = j4 - 1
          iend (j4) = nbv
        ENDIF
        GOTO 330
      !**    ende
      ELSEIF (anftg (j4) .le.stat (j3) .and.anftg (j4) .gt.stat (j3 - 1) ) then
        it1 = it1 + 1
        iend (j4 - 1) = j3 - 1
        IF (it1.eq.1) then
          iendtg = j4
          iend (j4) = nbv
        ENDIF
        GOTO 330
      ENDIF
    335 END DO

330 END DO

ENDIF

iteil = iendtg - ianftg + 1
it1 = 0

DO jj = ianftg, iendtg
  ikenntg (jj) = 0
  idiff = iend (jj) - ianf (jj)

  IF (idiff.lt.1) then
    it1 = it1 + 1
    ikenntg (jj) = 1
  ENDIF
END DO

IF (it1.eq.iteil) then
  WRITE (iw12, '("nur 1 Profil in Teilgebieten!")')
  WRITE (iw12, '("K-M Parameter wird nicht bestimmt!")')
  GOTO 2000
ENDIF


DO 500 jj = ianftg, iendtg
  nteil = numitg (jj)

WRITE (iw12, '('''',/////////////////////t12,                    &
   &     ''auswertung fuer gerinneabschnitt '',i5)') nteil

  IF (ikenntg (jj) .eq.1) then
    WRITE (iw12, '("nur 1 Profil im Teilgebiet!")')
    WRITE (iw12, '("K-M Parameter wird nicht bestimmt!")')

    GOTO 500
  ENDIF

  ianfp = ianf (jj)
  iendp = iend (jj)

  !**   17.11.98 Csocsan
  n_prof = iendp - ianfp + 1
  lz = 0
  DO ll = ianfp, iendp
    lz = lz + 1
    xsp (lz) = stat (ll) * 1000.0
    hsohle (lz) = sohlp (ll)
  END DO
  xww = 0.0
  yww = 0.0

  CALL linreg (xsp, hsohle, lz, byx, xq, yq, xww, yww)

  slhngg = abs (byx)
  !***   ende

  DO 1000 l = ianfp, iendp

    IF (l.eq.ianfp) then
      st_o = stat (l + 1) - stat (l)
      st_u = 0.0
    ELSEIF (l.eq.iendp) then
      st_o = 0.0
      st_u = stat (l) - stat (l - 1)
    ELSE
      st_o = stat (l + 1) - stat (l)
      st_u = stat (l) - stat (l - 1)
    ENDIF

    st_o = st_o * 1000.
    st_u = st_u * 1000.

    ier = 0

    DO i2 = 1, 6
      kf (l, i2) = 0.
      nf (l, i2) = 0.
      kv (l, i2) = 0.
      nv (l, i2) = 0.
      qg (l, i2) = 0.
      bg (l, i2) = 0.
      wspg (l, i2) = 0.
    END DO

    WRITE (iw12, 9001) stat (l)

    IF (qbord (l) .gt.0.) then

      qmaxo = qbord (l) + 2 * qvar
      qminu = qbord (l) - 3 * qvar
      ikenn = ischbv (l)
      IF (qmaxo.gt.rqmax.or.qminu.lt.rqmin) then

        WRITE (iw12, '("K-M Parameter ist nicht bestimmbar!")')
        GOTO 1000

      ELSE


        IF (l.eq.ianfp) then
          ikenn0 = ikenn - 3
          hr (ianfp) = 0.
          DO il = 2, 6
            ikenn0 = ikenn0 + 1
            hr (il) = hwsp (ikenn0, l)
          END DO
        ENDIF
        !**   16.11.98 Csocsan
        q_fl_bv = qq (ikenn, l)

        304 CONTINUE

        CALL probv1 (l, iw12, q_fl_bv, hbord (l),     &
         & lif, kif, liv, kiv, qges, bges, wspgef, c (l),      &
         & slhngg)
      ENDIF

    ELSE
      WRITE (iw12, '("kein Bordvollabfluss!!")')
      WRITE (iw12, '("K-M Parameter ist nicht bestimmbar!")')
      GOTO 1000
    ENDIF

    DO i3 = 1, 6
      nf (l, i3) = lif (i3)
      kf (l, i3) = kif (i3)
      nv (l, i3) = liv (i3)
      kv (l, i3) = kiv (i3)
      qg (l, i3) = qges (i3)
      bg (l, i3) = bges (i3)
      wspg (l, i3) = wspgef (i3)
    END DO

  1000 END DO



  dif1 (ianfp) = stat (ianfp)

  DO i = ianfp + 1, iendp
    dif1 (i) = (stat (i - 1) + stat (i) ) / 2.
    difsta (i - 1) = abs (dif1 (i - 1) - dif1 (i) )
  END DO

  difsta (iendp) = abs (stat (iendp) - dif1 (iendp) )

  sumlen = stat (iendp) - stat (ianfp)

  sumlen = sumlen * 1000.


  DO i1 = 2, 6
    sukf = 0.
    sukv = 0.
    sunf = 0.
    sunv = 0.
    suqb = 0.
    suqbv = 0.
    suc = 0.
    sudif = 0.
    suqqg = 0.
    suvel = 0
    subbg = 0.
    suwsg = 0.
    DO i = ianfp, iendp
      sukf = kf (i, i1) * difsta (i) + sukf
      sukv = kv (i, i1) * difsta (i) + sukv
      sunf = difsta (i) * 1000. * nf (i, i1) + sunf
      sunv = difsta (i) * 1000. * nv (i, i1) + sunv
      suc = difsta (i) * c (i) + suc
      sudif = difsta (i) + sudif
      !**   17.11.98 Csocsan  suqbv= difsta(i)*qbord(i)+ suqbv
      ikenn = ischbv (i)

      suqbv = difsta (i) * qq (ikenn, i) + suqbv
      suqqg = difsta (i) * qg (i, i1) + suqqg
      !**   17.11.98 Csocsan  suvel= difsta(i)*vbord(i) + suvel
      suvel = difsta (i) * vfl (ikenn, i) + suvel
      subbg = difsta (i) * bg (i, i1) + subbg
      IF (i.eq.2) then
        suhso = difsta (i) * hwsp (i1, 1) + suhso
      ELSE
        suhdi = difsta (i) * (hwsp (i1, i) - hwsp (i1 - 1, 1) ) + suhdi
      ENDIF
      !**   17.11.98  suwsg= difsta(i)*wspg(i,i1)*2. + suwsg
    END DO

    rkf (i1) = sukf / sudif
    rkv (i1) = sukv / sudif
    rnf (i1) = sunf
    rnv (i1) = sunv
    bgg (i1) = subbg / sudif

    IF (i1.eq.2) then
      hsol = suhso / sudif
      hwg (i1) = hsol
    ELSE
      hdif = suhdi / sudif
      hwg (i1) = hwg (i1 - 1) + hdif
    ENDIF

    !** 17.11.98 Csocsan   hwg(i1) = hr(i1) + suwsg/sudif*sumlen
    !
    !     korrektur der werte, fall rnf und rnv groeszer 30

    IF (rnf (i1) .gt.30.) then
      div = sunf / 30.
      rkf (i1) = rkf (i1) * div
      rnf (i1) = rnf (i1) / div
    ENDIF

    IF (rnv (i1) .gt.30.) then
      div = sunv / 30.
      rkv (i1) = rkv (i1) * div
      rnv (i1) = rnv (i1) / div
    ENDIF

    qmax = suqbv / sudif
    ckf = suc / sudif
    vmit = suvel / sudif

    qqg (i1) = suqqg / sudif
  END DO

  iart = 1
  WRITE (iw11, * ) nteil
  WRITE (iw11, * ) iart
  WRITE (iw11, * ) qmax, ckf, vmit, sumlen, slhngg

  WRITE (iw12, 9304) nteil, qmax, vmit, sumlen, slhngg

  DO i1 = 2, 6

    WRITE (iw11, '(7f12.6)') qqg (i1), rkf(i1), rnf(i1), rkv(i1), rnv(i1), bgg(i1), hwg(i1)
    WRITE (iw12, 9305)       qqg (i1), rnf(i1), rkf(i1), rnv(i1), rkv(i1), bgg(i1), hwg(i1)

  END DO

500 END DO                                                                                
                                                                        
2000 RETURN
                                                                        
                                                                        
! ------------------------------------------------------------------
! Formate
! ------------------------------------------------------------------
                                                                        
   50 FORMAT(20i4) 
   51 FORMAT(2f7.3,f8.2,3f4.3,1x,a3) 
   52 FORMAT(f7.3,f8.2,f6.3,4i2) 
   53 FORMAT(5f4.1) 
   60 FORMAT(1h0,i5,2x,'stationen,',2x,'kontrl,lein,laus',3i5,//, &
     &              2x,'max.iterationsschritte',i5, /, &
     &              2x,'rauhigkeitsklassen',i5, /, &
     &              2x,'max.wiederholung der max.iterationsschritte',i5,/,&
     &              2x,'zeiger fuer k-st-werteinlesung=',i5,/,&
     &              2x,'zeiger fuer q-wert= ',i5,/, &
     &              2X,'zeiger fuer fliessgesetz= ',i5//)

   61 FORMAT(1h0,1x,'anfangszuweisungen',/,1x,'stat',f10.3,3x,'wsp=',f10&
     &.3,3x,'m+nn',3x,'q=',f10.2,2x,'cbm/s'/1x,'err',f7.3,2x,'m',2x,'fgr&
     &err=',f7.3,'m',2x,'delth=',f7.3,'m'/,'gerinnekennung = ',a3/)     
   62 FORMAT (1h0,'eingabedaten fuer profil',i5,'  bei stat.',f10.3//) 
   63 FORMAT(1h0,1x,'rauhigkeit der klasse in m /sec:',/ 5(i5,f8.2)/)
   65 FORMAT(1h0,1x,'schiessende stroemung bei profil',i5,2x,'stat',f10.&
     &3,2x,'schritt',i5,2x,/1x,'wsp=',f10.3,2x,'q=',f10.2,2x,'fl=',f10.3&
     &,2x,'v=',f10.3,2x,'brt=',f10.3,2x,'vsi=',f10.3,2x,'rb=',f10.3,2x,'&
     &bereich',i5,//)                                                   
   66 FORMAT(// 1X, '******   berechnung laueft *****',//, &
     &          1X, 'auswertung des profils an station ',f10.4,' km')
   67 FORMAT(// 1X, 'auswertung des profils an station ',f10.4,' km')
   69 FORMAT(1h0,1x,'ueberschreitung der iterationsschritte bei profil',&
     &i5,2x,'stat',f10.3,2x,'letzter berechneter schritt',i5,/1x,'wsp=',&
     &f10.3,2x,'q=',f10.2,2x,'err1=',f10.3//)                           
  660 FORMAT(1h0,'hr,hr2,str,hrst,hvst,rgm,err1') 
  661 FORMAT(1h0,3f10.4,4f10.7) 
  662 FORMAT(1h0,1x,'bei profil',i5,2x,'stat',f10.3,2x,'schritt',i5,2x,'&
     &wsp=',f10.3,2x,'unterhalb tiefster sohlenkoordinate'//)           
  663 FORMAT(1h1,' bei der berechnung von hgrenz wurde die maximalle ite&
     &rationsschrittzahl ueberschritten')                               
  664 FORMAT(1h0,'schiessende stroemung - es wird mit hgrenz weitergerechnet'//)
  665 FORMAT(1h0,'keine schiessende stroemung - das programm wird unterbrochen'//)
  666 FORMAT(1h0,'normale schrittanzahl zum',i4,'. mal ueberschritten'//&
     &)                                                                 
    1 FORMAT(1x,4f8.3) 
 9001 FORMAT('',/t2,'kalinin-miljukov-parameter fuer profil-km ',f10.4) 
                                                                        
 9304 FORMAT (//,'',///////////t12,                                    &
     &    'mittlere kalinin-miljukov-parameter im gerinneabschnitt '    &
     &   ,i6,///t30,'bordvoller abfluss:            ',f15.2, ' qbm/sec' &
     &        ,/t30,'mittlere geschwindigkeit:      ',f15.2,' m/sec',   &
     &        /t30,'laenge des gerinneabschnittes: ',f15.2,' m',        &
     & /t30,'mittleres gefaelle:            ',f16.3,' m/m',///t30,      &
     &     'abfluss   fluss-speicher   vorlandspeicher   ',             &
     &/t30,                                                             &
     &'             n       k         n      k       breite  hoehe'     &
     &,/t28,                                                            &
     &'-------------------------------------------------------------'   &
     &,/t30,                                                            &
     &'[qbm/s]     [-]     [h]       [-]    [h]       [m]   [m u.nn]'   &
     &,/t28,                                                            &
     &'=============================================================')  
                                                                        
 9305 FORMAT(t28,f8.3,2x,2f8.3,2x,2f8.3,2x,2f8.3) 
                                                                        
 9990 FORMAT(2(1x,f6.3),1x,f6.2,1x,2(f7.2),2(1x,f4.2)) 
 9901 FORMAT(1x,i3,1x,f7.2,1x,2(f5.2,1x),f7.2,i2) 
 9902 FORMAT(2(1x,f7.2)) 
 9903 FORMAT((3f7.2)) 
 9904 FORMAT('     bwc','     bwp','      ss','   elchu',               &
     &'   elchd','      xk','    cmom',/,7f8.2)                         
 9905 FORMAT(' nrd','   barea','    xkor','    cofq',                   &
     &'   rdlen',' irf',/,i4,4f8.2,i4)                                  
 9906 FORMAT('   eltrd','    ellc',/,2(f8.2)) 
 9907 FORMAT(2('    rdel','    bruk','    rdst')) 
 9908 FORMAT(6(f8.2)) 
 9909 FORMAT(l2,20a4) 
 9910 FORMAT('brueckenprofil') 
                                                                        
                                                                        
      END SUBROUTINE bovog1                         
