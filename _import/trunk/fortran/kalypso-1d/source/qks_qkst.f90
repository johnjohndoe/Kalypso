!     Last change:  WP   11 Jul 2005    5:48 pm
!--------------------------------------------------------------------------
! This code, qks_qkst.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - qks
! - qkst
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

SUBROUTINE qks (iprof, isener, qgs, itere1, hr, hv, nknot, iuerr)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE QKS                                                    
!**                                                                     
!JK   BESCHREIBUNG: BERECHNUNG DER TEILABFLUESSE (VORLAENDER,FLUSS-     
!JK                 SCHLAUCH) UND GESAMTABFLUSS NACH DARCY-WEISBACH     
!**                                                                     
!**   IN DIESER SUBROUTINE VERWENDETE VARIABLEN                         
!**   -----------------------------------------                         
!**                                                                     
!**                                                                     
!**   a_hg    --      durchströmte Fläche des Flußschlauches            
!**   a_ks    --      durchströmte Fläche eines Teilabschnittes         
!**   a_li    --      durchströmte Fläche des linken Absturzes          
!**   a_re    --      durchströmte Fläche des rechten Absturzes         
!**   ak_mi   --      abgeminderte Rauheit des mittleren Abschnittes    
!**   akges   --      Gesamtrauheit                                     
!**   aks_li  --      Rauheit des linken Abstuzes                       
!**   aks_re  --      Rauheit des rechten Abstuzes                      
!**   alges   --      Gesamtwiderstandsbeiwert                          
!**   alsum   --      Summe der Teilwiderstandsbeiwerte multipliziert   
!**                   mit dem jeweiligen Teilumfang                     
!**   b_hg    --      Breite des Flußschlauches                         
!**   b_ks    --      Breite eines Teilabschnittes                      
!**   bf(1)   --      mitwirkende Breite des Flußschlauches links       
!**   bf(2)   --      mitwirkende Breite des Flußschlauches rechts      
!**   dhy1    --      hydraulischer Durchmesser                         
!**   f(1)    --      durchströmte Fläche des linken Vorlandes          
!**   f(2)    --      durchströmte Fläche des Flußschlauches            
!**   f(3)    --      durchströmte Fläche des rechten Vorlandes         
!**   factor1 --      Faktor zu Berechnung der Gesamtrauheit            
!**   factor2 --      Faktor zu Berechnung der Gesamtrauheit            
!**   fges    --      gesamte durchströmte Fläche                       
!**   h_ks    --      Wasserspiegelhöhe eines Teilabschnittes           
!**   h_li    --      Wasserspiegelhöhe am linken Absturz               
!**   h_re    --      Wasserspiegelhöhe am rechten Absturz              
!**   hr      --      Wasserspiegelhöhe                                 
!**   hv      --      Geschwindigkeitsverlust                           
!**   hvor    --      mittlere Fließtiefe im Vorland                    
!**   i_typ_flg --    Art der Widerstandsbeiwertberechnung              
!**   if_l    --      Fehlervariable der Subroutine Lindy               
!**   if_pa   --      Fehlervariable der Subroutine Pasche              
!**   ischl   --      linker Profilpunkt                                
!**   isener  --      Energiegefälle                                    
!**   isenk   --      Funktion des Energiegefälles                      
!**   itrli   --      Profilpunkt der linken Trennfläche                
!**   k_ks    --      Rauheit eines Teilabschnittes                     
!**   l_hg    --      Widerstandsbeiwert des Flußschlauches             
!**   l_ks    --      Widerstandsbeiwert eines Teilabschnittes          
!**   nknot   --      Anzahl der Punkte im Profil                       
!**   phion   --      Zähler des Energiestrombeiwertes                  
!**   phiun   --      Nenner des Energiestrombeiwertes                  
!**   q_hg    --      Abfluß des Flußschlauches                         
!**   q_ks    --      Abfluß eines Teilabschnittes                      
!**   qgs     --      Gesamtabfluß                                      
!**   qt(1)   --      Teilabfluß des linken Vorlandes                   
!**   qt(2)   --      Teilabfluß des Flußschlauches                     
!**   qt(3)   --      Teilabfluß des rechten Vorlandes                  
!**   r_hg    --      hydraulischer Radius des Flußschlauches           
!**   re      --      Reynoldszahl                                      
!**   rhges   --      gesamter hydraulischer Radius                     
!**   rk(1)   --      Widerstandsbeiwert des linken Vorlandes           
!**   rk(2)   --      Widerstandsbeiwert des Flußschlauches             
!**   rk(3)   --      Widerstandsbeiwert des rechen Vorlandes           
!**   u(1)    --      benetzter Umfang des linken Vorlandes             
!**   u(2)    --      benetzter Umfang des Flußschauches                
!**   u(3)    --      benetzter Umfang des rechten Vorlandes            
!**   u_ks    --      benetzter Umfang eines Teilabschnittes im Vorland 
!**   uges    --      gesamter benetzter Umfang                         
!**   v(1)    --      mittlere Fließgeschwindigkeit im linken Vorland   
!**   v(2)    --      mittlere Fließgeschwindigkeit im Flußschlauch     
!**   v(3)    --      mittlere Fließgeschwindigkeit im rechten Vorland  
!**   v_hg    --      mittlere Flißgeschwindigkeit des Flußschlauches   
!**   v_ks    --      mittlere Fließgeschwindigkeit eines Teilabschnitte
!**   vges    --      mittlere Flißgeschwindigkeit                      
!**   vlam    --      Verhältnis der Widerstandsbeiwerte der Trennfläche
!**                                                                     
!**                                                                     
!**                                                                     
!**   AUFGERUFENE ROUTINEN                                              
!**   --------------------                                              
!JK   lindy                                                             
!JK   pasche                                                            
!**                                                                     
!***********************************************************************
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN

! ------------------------------------------------------------------
! VEREINBARUNGSTEIL
! ------------------------------------------------------------------
CHARACTER(LEN=1), INTENT(IN) 	:: iprof
REAL                		:: isener
REAL, INTENT(INOUT)             :: qgs
INTEGER, INTENT(IN)             :: itere1
REAL, INTENT(INOUT)             :: hr
REAL, INTENT(INOUT)             :: hv
INTEGER, INTENT(IN)             :: nknot
INTEGER, INTENT(IN)             :: iuerr


! COMMON-Block /AUSGABEART/ --------------------------------------------------------
! lein=1    --> einfacher ergebnisausdruck
! lein=2    --> erweiterter ergebnisausdruck
! lein=3    --> erstellung kontrollfile
! jw8       --> NAME KONTROLLFILE
INTEGER 	:: lein
INTEGER 	:: jw8
COMMON / ausgabeart / lein, jw8
! ----------------------------------------------------------------------------------


! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
! ----------------------------------------------------------------------------------


! COMMON-Block /FLG_TYP/ -----------------------------------------------------------
CHARACTER(LEN=6):: i_typ_flg
COMMON / flg_typ / i_typ_flg
! ----------------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /PA_COM/ -------------------------------------------------------
REAL 		:: a_ks (maxkla), b_ks (maxkla), r_ks (maxkla), u_ks (maxkla)
REAL            :: k_ks (maxkla), h_ks (maxkla), v_ks (maxkla), l_ks (maxkla)
REAL            :: q_ks (maxkla), b_hg, a_hg, q_hg, v_hg, r_hg, l_hg, fr_hg
INTEGER         :: ischl, ischr
COMMON / pa_com / a_ks, b_ks, r_ks, u_ks, k_ks, h_ks, v_ks, l_ks, &
       & q_ks, b_hg, a_hg, u_hg, q_hg, v_hg, r_hg, l_hg, fr_hg, ischl, ischr
! -----------------------------------------------------------------------------


! COMMON-Block /PRI/ ----------------------------------------------------------
! DK, 07/06/01 - Parameters for printing warnings and comments!
INTEGER         :: ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
COMMON / pri / ibed, icwl, icwm, icwr, ikol, ikom, ikor, ifum
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /REIB/ ---------------------------------------------------------
! Berechnungsart des Reibungsverlustes
INTEGER 	:: rg_vst
COMMON / reib / rg_vst
! -----------------------------------------------------------------------------


! Local variables
INTEGER :: if_l (maxkla)        	! if_l = Fehlervariable für die Subroutine lindy

REAL    :: isenk
REAL 	:: hvor (maxkla), cwr (maxkla), alp (maxkla)
REAL 	:: also (maxkla), anl (maxkla), anb (maxkla)
REAL  	:: vnvv (maxkla), cwn (maxkla)
REAL 	:: bf (2), vt_l (2), anl_l (2), anb_l (2), om_l (2), ct_l (2)
REAL 	:: bm_l (2), alt_l (2), h_t (2), v1_hg, vt_n (2)

                                                                        
!HB***************************************                              
!HB   11.12.02 H.BROEKER                                                
!HB                                                                     
!HB   Def. des fehlenden Param. mei(ii) beim Call-Befehl von Lindy      
!HB   s.u.                                                              
REAL 	:: mei (maxkla)
!HB   Ergänzung Ende                                                    
!HB***************************************                              

! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
!      froud(a,b,c)=c/a/sqrt(9.81*a/b)                                  
!                                                                       
!     gleichsetzen der uebergebenen groessen mit den common-werten      
!                                                                       
                                                                        
      ibed = 0 
      icwl = 0 
      icwm = 0 
      icwr = 0 
      ikol = 0 
      ikom = 0 
      ikor = 0 
      ifum = 0 
                                                                        
      itere2 = 1 
                                                                        
                                                                        
      qt (1) = 0. 
      qt (2) = 0. 
      qt (3) = 0. 
      rk (1) = 0. 
      rk (2) = 0. 
      rk (3) = 0. 
                                                                        
      alsum = 0. 
      DO 30 ii = 1, nknot 
        v_ks (ii) = 0.0 
        l_ks (ii) = 0.0 
        q_ks (ii) = 0.0 
   30 hvor (ii) = 0.0 
                                                                        
!JK   BERECHNUNG TEILABFLUSS LINKES VORLAND                             
!JK   -------------------------------------                             
                                                                        
      DO 40 ii = ischl, itrli - 1 
!JK       VORBELEGUNG                                                   
        hvor (ii) = hr - (h_ks (ii) + h_ks (ii + 1) ) / 2. 
        IF (b_ks (ii) .gt.1.e-01) then 
          alpha = atan (abs (h_ks (ii) - h_ks (ii + 1) ) / b_ks (ii) ) 
        ELSE 
          alpha = 0. 
        ENDIF 
!JK      BESTIMMUNG FLIESSTIEFE LINKS                                   
        IF (ii.gt.1) then 
          IF (hr.gt.h_ks (ii - 1) ) then 
            h_li = h_ks (ii - 1) - h_ks (ii) 
          ELSE 
            h_li = hr - h_ks (ii - 1) 
          ENDIF 
          a_li = a_ks (ii - 1) 
          aks_li = k_ks (ii - 1) 
        ELSE 
          h_li = 0. 
          a_li = 0. 
          aks_li = 0. 
        ENDIF 
!JK      BESTIMMUNG FLIESSTIEFE RECHTS                                  
        IF (ii.lt.nknot - 1) then 
          IF (hr.ge.h_ks (ii + 2) ) then 
            h_re = h_ks (ii + 2) - h_ks (ii + 1) 
          ELSE 
            h_re = hr - h_ks (ii + 1) 
          ENDIF 
        ELSE 
          h_re = 0. 
          a_re = 0. 
          aks_re = 0. 
        ENDIF 
        a_re = a_ks (ii + 1) 
        aks_re = k_ks (ii + 1) 
                                                                        
        IF (0.60 * hvor (ii) .lt.k_ks (ii) ) then 
          ak_mi = 0.60 * hvor (ii) 
        ELSE 
          ak_mi = k_ks (ii) 
        ENDIF 
                                                                        
        IF (hvor (ii) .lt.1.e-06.and.a_ks (ii) .gt.1.e-05) then 
      PRINT * , 'Fehler.Fliesztiefe Vorland  inkorrekt ' 
          PRINT * , 'Voraussichtlich Geometriefehler.' 
          PRINT * , 'Beim Entwickler melden.' 
          STOP 'SUB EB2KS, da Fehler nicht behebbar.' 
        ENDIF 
!JK      BERECHNUNG TEILABFLUSS                                         
!HB***************************************                              
!HB   11.12.02 H.BROEKER                                                
!HB                                                                     
!HB   im Aufruf von Lindy fehlt mei(ii), alter deaktivierte Befehl:     
!HB         call lindy(v_ks(ii),l_ks(ii),ax(ii),ay(ii),                 
!HB     1                   dp(ii),hvor(ii),isener,                     
!HB     2                 u_ks(ii),a_ks(ii),ak_mi,a_li,a_re,h_li,h_re,  
!HB     3                 aks_li,aks_re,alpha,iuerr,lein,cwr(ii),alp(ii)
!HB     4      also(ii),anl(ii),anb(ii),vnvv(ii),cwn(ii),if_l(ii),k_ks(i
!HB   neuer Aufruf:                                                     
        CALL lindy (v_ks (ii), l_ks (ii), ax (ii), ay (ii), dp (ii),    &
        hvor (ii), mei (ii), isener, u_ks (ii), a_ks (ii), ak_mi, a_li, &
        a_re, h_li, h_re, aks_li, aks_re, alpha, iuerr, lein, cwr (ii), &
        alp (ii), also (ii), anl (ii), anb (ii), vnvv (ii), cwn (ii),   &
        if_l (ii), k_ks (ii), formbeiwert(1) )
!HB   Ende des neuen Aufrufs                                            
!HB***************************************                              
                                                                        
        q_ks (ii) = v_ks (ii) * a_ks (ii) 
        qt (1) = qt (1) + q_ks (ii) 
        alsum = alsum + l_ks (ii) * u_ks (ii) 
   40 END DO 
                                                                        
!JK   BERECHNUNG BENETZTER UMFANG?, GESCHWINDIGKEIT (LINKES VORLAND)    
      IF (f (1) .gt.1.e-06) then 
        rk (1) = alsum / u (1) 
        v (1) = qt (1) / f (1) 
      ELSE 
        rk (1) = 0. 
        v (1) = 0. 
      ENDIF 
                                                                        
      alsum = 0. 
                                                                        
!JK   BERECHNUNG TEILABFLUSS RECHTES VORLAND                            
!JK   -------------------------------------                             
                                                                        
      DO 60 ii = itrre, ischr - 1 
!JK       VORBELEGUNG                                                   
        hvor (ii) = hr - (h_ks (ii) + h_ks (ii + 1) ) / 2. 
        IF (b_ks (ii) .gt.1.e-01) then 
          alpha = atan (abs (h_ks (ii + 1) - h_ks (ii) ) / b_ks (ii) ) 
        ELSE 
          alpha = 0. 
        ENDIF 
!JK      BESTIMMUNG FLIESSTIEFE LINKS                                   
        IF (ii.gt.1) then 
          IF (hr.gt.h_ks (ii - 1) ) then 
            h_li = h_ks (ii - 1) - h_ks (ii) 
          ELSE 
            h_li = hr - h_ks (ii - 1) 
          ENDIF 
          a_li = a_ks (ii - 1) 
          aks_li = k_ks (ii - 1) 
        ELSE 
          h_li = 0. 
          a_li = 0. 
          aks_li = 0. 
        ENDIF 
!JK      BESTIMMUNG FLIESSTIEFE RECHTS                                  
        IF (ii.lt.nknot - 1) then 
          IF (hr.ge.h_ks (ii + 2) ) then 
            h_re = h_ks (ii + 2) - h_ks (ii + 1) 
          ELSE 
            h_re = hr - h_ks (ii + 1) 
          ENDIF 
        ELSE 
          h_re = 0. 
          a_re = 0. 
          aks_re = 0. 
        ENDIF 
        a_re = a_ks (ii + 1) 
        aks_re = k_ks (ii + 1) 
                                                                        
        IF (0.60 * hvor (ii) .lt.k_ks (ii) ) then 
          ak_mi = 0.60 * hvor (ii) 
        ELSE 
          ak_mi = k_ks (ii) 
        ENDIF 
                                                                        
        IF (hvor (ii) .lt.1.e-06.and.a_ks (ii) .gt.1.e-05) then 
      PRINT * , 'Fehler.Fliesztiefe Vorland  inkorrekt ' 
          PRINT * , 'Voraussichtlich Geometriefehler.' 
          PRINT * , 'Beim Entwickler melden.' 
          STOP 'SUB EB2KS, da Fehler nicht behebbar.' 
        ENDIF 
!JK      BERECHNUNG TEILABFLUSS                                         
!HB***************************************                              
!HB   08.01.03 H.BROEKER                                                
!HB                                                                     
!HB   im Aufruf von Lindy fehlt mei(ii), alter deaktivierte Befehl:     
!HB         call lindy(v_ks(ii),l_ks(ii),ax(ii),ay(ii),                 
!HB     1                   dp(ii),hvor(ii),isener,                     
!HB     2                 u_ks(ii),a_ks(ii),ak_mi,a_li,a_re,h_li,h_re,  
!HB     3                aks_li,aks_re,alpha,iuerr,lein,cwr(ii),alp(ii),
!HB     4      also(ii),anl(ii),anb(ii),vnvv(ii),cwn(ii),if_l(ii),k_ks(i
!HB   neuer Aufruf:                                                     
        CALL lindy (v_ks (ii), l_ks (ii), ax (ii), ay (ii), dp (ii),    &
        hvor (ii), mei (ii), isener, u_ks (ii), a_ks (ii), ak_mi, a_li, &
        a_re, h_li, h_re, aks_li, aks_re, alpha, iuerr, lein, cwr (ii), &
        alp (ii), also (ii), anl (ii), anb (ii), vnvv (ii), cwn (ii),   &
        if_l (ii), k_ks (ii), formbeiwert(3) )
!HB   Ende des neuen Aufrufs                                            
!HB***************************************                              
                                                                        
                                                                        
        q_ks (ii) = v_ks (ii) * a_ks (ii) 
        qt (3) = qt (3) + q_ks (ii) 
        alsum = alsum + l_ks (ii) * u_ks (ii) 
   60 END DO 
                                                                        
!JK   BERECHNUNG BENETZTER UMFANG?, GESCHWINDIGKEIT (RECHTES VORLAND)   
      IF (f (3) .gt.1.e-06) then 
        rk (3) = alsum / u (3) 
        v (3) = qt (3) / f (3) 
      ELSE 
        rk (3) = 0. 
        v (3) = 0. 
      ENDIF 
                                                                        
                                                                        
!JK   BERECHNUNG TEIL-WSP-BREITEN?                                      
!JK   ----------------------------                                      
                                                                        
   70 IF (itere1.le.1.and.itere2.le.1) then 
!JK       WENN KEIN VORLAND                                             
        IF (itrli.eq.1.and.itrre.eq.nknot) then 
          bf (1) = b_hg / 2. 
          bf (2) = b_hg / 2. 
!JK       WENN VORLAND NUR RECHTS                                       
        ELSEIF (itrli.eq.1) then 
          bf (1) = 1.e-06 
          bf (2) = b_hg 
!JK       WENN VORLAND NUR LINKS                                        
        ELSEIF (itrre.eq.nknot) then 
          bf (1) = b_hg 
          bf (2) = 1.e-06 
!JK       WENN VORLAND BEIDSEITIG                                       
        ELSE 
          IF (abs (l_ks (itrli - 1) ) .le.1.e-04) then 
            IF (abs (l_ks (itrre) ) .le.1.e-04) then 
              vlam = 1. 
            ELSE 
              vlam = 0. 
            ENDIF 
          ELSE 
            vlam = l_ks (itrre) / l_ks (itrli - 1) 
          ENDIF 
          bf (1) = b_hg / (1. + vlam) 
          bf (2) = b_hg - bf (1) 
        ENDIF 
!JK   WENN bf(LINKES VORLAND) ZU KLEIN                                  
      ELSEIF (abs (bf (1) ) .le.1.e-04) then 
        IF (itrli.ne.1.or.ischl.lt.itrli) then 
          IF (abs (l_ks (itrli - 1) ) .le.1.e-04) then 
            IF (abs (l_ks (itrre) ) .le.1.e-04) then 
              vlam = 1. 
            ELSE 
              vlam = 0. 
            ENDIF 
          ELSE 
            vlam = l_ks (itrre) / l_ks (itrli - 1) 
          ENDIF 
          bf (1) = b_hg / (1. + vlam) 
          bf (2) = b_hg - bf (1) 
        ENDIF 
!JK    WENN bf(RECHTES VORLAND) ZU KLEIN                                
      ELSEIF (abs (bf (2) ) .le.1.e-04) then 
        IF (itrre.ne.nknot.or.ischr.gt.itrre) then 
          IF (abs (l_ks (itrli - 1) ) .le.1.e-04) then 
            IF (abs (l_ks (itrre) ) .le.1.e-04) then 
              vlam = 1. 
            ELSE 
              vlam = 0. 
            ENDIF 
          ELSE 
            vlam = l_ks (itrre) / l_ks (itrli - 1) 
          ENDIF 
          bf (1) = b_hg / (1. + vlam) 
          bf (2) = b_hg - bf (1) 
        ENDIF 
      ENDIF 
                                                                        
!JK   BERECHNUNG FLUSSSCHLAUCH NACH PASCHE                              
      CALL pasche (nknot, iprof, hr, bf, itere2, br, qvor1, qvor2,      &
      isener, iuerr, lein, vt_l, anl_l, anb_l, om_l, ct_l, bm_l, alt_l, &
      h_t, vt_n, v1_hg, if_pa, formbeiwert)
                                                                        
!JK   GESAMTABFLUSS LINKES VORLAND                                      
      qt (1) = qt (1) + qvor1 
!JK   GESAMTABFLUSS RECHTES VORLAND                                     
      qt (3) = qt (3) + qvor2 
!JK   GESAMTABFLUSS FLUSSSCHLAUCH                                       
      q_hg = v_hg * a_hg 
      qt (2) = q_hg 
!JK   GESCHWINDIGKEIT FLUSSSCHLAUCH                                     
      v (2) = v_hg 
!JK   HYDRAULISCHER RADIUS FLUSSSCHLAUCH                                
      ra (2) = r_hg 
!JK   BENETZTER UMFANG? FLUSSSCHLAUCH                                   
      rk (2) = l_hg 
                                                                        
!JK   GESAMTABFLUSS                                                     
      qgs = qt (1) + qt (2) + qt (3) 
                                                                        
      DO 160 i = ischl, itrli - 1
!JK   FEHLERMELDUNG FUER LINKES VORLAND                                 
        IF (if_l (i) .ne.0) then 
          IF (lein.eq.3) then 
      WRITE (iuerr, '(''Warnung! w- '',i8,'' am Profilpunkt'',i3,       &
     &          '' bei der Vorlandberechnung'')') if_l (i) , i          
          ENDIF 
        ENDIF 
  160 END DO 
                                                                        
      DO 161 i = itrre, ischr - 1 
!JK   FEHLERMELDUNG FUER RECHTES VORLAND                                
        IF (if_l (i) .ne.0) then 
          IF (lein.eq.3) then 
      WRITE (iuerr, '(''Warnung! w- '',i8,'' am Profilpunkt'',i3,       &
     &           '' bei der Vorlandberechnung'')') if_l (i) , i         
          ENDIF 
        ENDIF 
  161 END DO 
                                                                        
      IF (if_pa.ne.0) then 
!JK   FEHLERMELDUNG FUER FLUSSSCHLAUCH                                  
        IF (lein.eq.3) then 
      WRITE (iuerr, '(''Warnung! w- '',i8,'' bei der'',                 &
     &           '' Sohlberechnung'')') if_pa                           
        ENDIF 
      ENDIF 
!**                                                                     
                                                                        
!JK   BERECHNUNG VON:                                                   
!JK   ---------------                                                   
!JK      - GESAMTUMFANG                                                 
!JK      - HYDRAULISCHER RADIUS (GESAMT)                                
!JK      - GESAMTGESCHWINDIGKEIT                                        
!JK      - HYDRAULISCHER WIDERSTAND (akges)??                           
      uges = u (1) + u (2) + u (3) 
      IF (uges.gt.0) then 
        rhges = fges / uges 
        vges = qgs / fges 
        alges = 8 * 9.81 * rhges * isener / vges / vges 
!JK   WENN WIDERSTANDSGESETZ NACH PASCHE                                
        IF (i_typ_flg.eq.'pasche') then 
          factor1 = - 1. / 2.03 / alges**0.5 
          re = vges * 4. * rhges / 1.e-06 
          factor2 = 4.4 / alges**0.5 / re 
!JK   WENN WIDERSTANDSGESETZ NACH COLEBROOK                             
        ELSE 
          factor1 = - 1. / 2. / sqrt (alges) 
          isenk = isener * (1. + isener**2.) **0.5 
          dhy1 = 4. * rhges 
          factor2 = 1.31e-06 * 2.51 / (dhy1 * (2. * 9.81 * dhy1 * isenk)&
          **0.5)                                                        
        ENDIF 
        akges = 3.71 * 4. * rhges * (10** (factor1) - factor2) 
      ENDIF 
                                                                        
!JK   BERECHNUNG DES WIRKSAMEN GESCHWINDIGKEITSVERLUSTES                
!JK   --------------------------------------------------                
      phion = 0.0 
      phiun = 0.0 
                                                                        
                                                                        
  210 DO 250 ii = ischl, itrli - 1 
!JK   FUER LINKES VORLAND                                               
        IF (l_ks (ii) .gt.1.e-6) then 
          phion = phion + a_ks (ii) * ( (r_ks (ii) / l_ks (ii) ) **1.5) 
          phiun = phiun + a_ks (ii) * ( (r_ks (ii) / l_ks (ii) ) **0.5) 
        ENDIF 
  250 END DO 
                                                                        
      DO 251 ii = itrre, ischl - 1 
!JK   DAZU RECHTES VORLAND                                              
        IF (l_ks (ii) .gt.1.e-6) then 
          phion = phion + a_ks (ii) * ( (r_ks (ii) / l_ks (ii) ) **1.5) 
          phiun = phiun + a_ks (ii) * ( (r_ks (ii) / l_ks (ii) ) **0.5) 
        ENDIF 
  251 END DO 
                                                                        
!JK   DAZU FLUSSSCHLAUCH                                                
  260 phion = phion + a_hg * ( (r_hg / l_hg) **1.5) 
      phiun = phiun + a_hg * ( (r_hg / l_hg) **0.5) 
                                                                        
!JK   WIRKSAMEN GESCHWINDIGKEITSVERLUST                                 
      hv = phion / phiun**3 * qgs * qgs / 9.81 / 2. 
                                                                        
                                                                        
!JK   UNSINNIGE ABFRAGE----------------------------------------------   
      idruck = 0 
      IF (idruck.eq.1) then 
  181   DO 6100 i1 = 1, 2 
          IF (i1.eq.1) then 
            WRITE (iuerr, '(12f8.4)') vt_l (i1) , anl_l (i1) , anb_l (  &
            i1) , om_l (i1) , ct_l (i1) , bm_l (i1) , bf (i1) , alt_l ( &
            i1) , h_t (i1) , l_hg, v1_hg, vt_n (i1)                     
          ELSE 
            WRITE (iuerr, '(12f8.4)') vt_l (i1) , anl_l (i1) , anb_l (  &
            i1) , om_l (i1) , ct_l (i1) , bm_l (i1) , bf (i1) , alt_l ( &
            i1) , h_t (i1) , l_hg, v_hg, vt_n (i1)                      
          ENDIF 
 6100   END DO 
      ENDIF 
!JK   ENDE UNSINNIG ABFRAGE------------------------------------------   
                                                                        
      RETURN 
                                                                        
      END SUBROUTINE qks
                                                                        
                                                                        

!-----------------------------------------------------------------------
SUBROUTINE qkst (indmax, hr, hv, vm, qges, sohle, ife)
!-----------------------------------------------------------------------
! geschrieben :                   24.08.1988  e.pasche
! geaendert   :
!                                                                       
!-----------------------------------------------------------------------
! allgemeine beschreibung :
!                                                                       
! das programm qkst berechnet bei vorgegebenen spiegelliniengefaelle
! den abflusz in den rauhigkeitszonen und den gesamtabflusz
!
!
!
! IN DIESER SUBROUTINE VERWENDETE VARIABLEN
! -----------------------------------------
!
! akges   --      Gesamtrauheit
! br      --      Breite des Abschnittes
! c2      --      Faktor zur Berechnung des Geschwindigkeitsverluste
! f       --      Teilfläche
! fges    --      gesamte durchströmte Fläche
! hv      --      Geschwindigkeitsverlust
! indmax  --      Anzahl der Rauheitsabschnitte
! qges    --      Gesamtabfluß
! qt      --      Teilabfluß
! ra      --      hydraulischer Radius des Abschnittes
! ra1     --      Funktion von ra
! rb      --      Verhältnis aus Fläche und Breite
! rges    --      gesamter hydraulischer Radius
! rk      --      Rauheit des Abschnittes
! sohle   --      Sohlgefälle
! ts1     --      Funktion von ra
! ts2     --      Funktion von ra
! uges    --      gesamter benetzter Umfang
! v       --      Teilgeschwindigkeit
! vges    --      mittlere Fließgeschwindigkeit
! vm      --      mittlere Fließgeschwindigkeit
!
!-----------------------------------------------------------------------
! aufgerufene routinen : KEINE
!-----------------------------------------------------------------------

!WP 01.02.2005
USE DIM_VARIABLEN

! COMMON-Block /AUSGABEART/ --------------------------------------------------------
! lein=1    --> einfacher ergebnisausdruck
! lein=2    --> erweiterter ergebnisausdruck
! lein=3    --> erstellung kontrollfile
! jw8       --> NAME KONTROLLFILE
INTEGER 	:: lein
INTEGER 	:: jw8
COMMON / ausgabeart / lein, jw8
! ----------------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------
                                                                        
ife = 0

DO i = 1, indmax
  !JK FUER ALLE RAUHIGKEITSZONEN
  IF (ra (i) .le.1.e-04.or.br (i) .le.0.0001) then
    ! falls hydr. radius oder spiegelbreite = 0
    ts1 (i) = 0.
    ts2 (i) = 0.
    ra (i) = 0.
    rb (i) = 0.
  ELSE
    rb (i) = f (i) / br (i)
    ra1 (i) = ra (i) ** (2. / 3.)
    ts1 (i) = rk (i) * ra1 (i)
    ts2 (i) = f (i) * ts1 (i)
  ENDIF
END DO

!JK   BERECHNUNG GESAMTABFLUSS AUS TEILABFLUESSEN DER RAUHIGKEITSZONEN
!JK   ----------------------------------------------------------------
qges = 0.
DO i1 = 1, indmax
  qt (i1)  = sqrt (sohle) * ts2 (i1)
  v (i1)   = sqrt (sohle) * ts1 (i1)
  qges     = qt (i1) + qges
END DO

IF (qges.lt.1.e-04) then
  !JK  FEHLERMELDUNG + ABBRUCH
  write (*,*) 'Wasserspiegel unterhalb Sohlenkoordinate!'
  ife = 1
  RETURN
ENDIF

!JK   BERECHNUNG MITTLERE GESCHWINDIGKEIT
!JK   -----------------------------------
vm = (qges) / fges

!JK   BERECHNUNG WIRKSAMER GESCHWINDIGKEITSVERLUST
!JK   --------------------------------------------
c2 = 0.

DO i = 1, indmax
  IF (f (i) .gt.1.e-03) then
    c2 = f (i) * ( (qt (i) / f (i) ) **3) + c2
  ENDIF
END DO

IF (abs (qges) .le.1.e-06) then
  write (*,*) 'Kein oder zu geringer Abfluss definiert. Ueberpruefe QWERT-Datei.'
  write (*,*) '-> Programmabbruch in QKST!'
  STOP
ENDIF

hv = c2 / (2. * 9.81 * qges)

!JK   BRECHNUNG VON:
!JK   --------------
!JK      - MITTLERE GESCHWINDIGKEIT
!JK      - HYDRAULISCHER RADIUS (GESAMT)
!JK      - HYDRAULISCHER WIDERSTAND (akges)??
IF (uges.gt.0.and.sohle.gt.0) then
  vges = qges / fges
  rges = fges / uges
  akges = vges / (rges** (2. / 3.) * sohle**0.5)
ENDIF                                                                      

END SUBROUTINE qkst                           
