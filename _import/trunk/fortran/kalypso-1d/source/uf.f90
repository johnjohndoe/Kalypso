!     Last change:  MD    8 Jul 2009    4:10 pm
!--------------------------------------------------------------------------
! This code, U_WSP.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - uf
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



!***********************************************************************
!**                                                                     
SUBROUTINE uf (hr, hi, xi, s, indmax, nfli, nfre)
!**
!**   geschrieben :                   24.08.1988  e.pasche              
!**   geaendert   :                   21.03.1990  p.koch                
!**                                   27.03.1990  e.pasche              
!**                                                                     
!**   Allgemeine Beschreibung :                                         
!**                                                                     
!**   Das Programm uf berechnet die geometrischen Groessen Flaeche,     
!**   benetzter Umfang und Breite fuer die einzelnen Teilquerschnitte   
!**   (Anzahl indmax)                                                   
!**                                                                     
!**   Uebergebene Parameter                                             
!**   ---------------------                                             
!**   hrb    r4          input       Wasserstand in nn+m                
!**   xi     r4(1...*)   input       Horizontaldistanz der Profilpunkte 
!**   hi     r4(1...*)   input       Hoehendifferenzen zw. den P-Punkten
!**   s      r4(1...*)   input       Distanz der Profilpunkte           
!**   indmax i4          output      Anzahl Rauhigkeitszonen im profil  
!**   nfli   i4          output      nr. der Rauhigkeitszone an der     
!**                                  li. Vorland/f-schlauch-trennflaeche
!**   nfre   i4          output      nr. der rauhigkeitszone an der     
!**                                  re. Vorland/f-schlauch-trennflaeche
!**                                                                     
!**   Beachte: Es wird unterstellt, dass ein Rauhigkeitswechsel zwischen
!**   den Trennflaechen stattfindet. Bei Sonderprofilen liegt nur       
!**   eine Rauhigkeitszone vor. Hier ist nfli=nfre=1.                   
!**                                                                     
!**   DIE BERECHNETEN PARAMETER fges, brges, uges WERDEN IM COMMON-     
!**   BLOCK/GES/ UEBERGEBEN                                             
!**                                                                     
!**   PARAMETER                                                         
!**   ---------                                                         
!**   a        - FLAECHE?                                               
!**   indmax   - Anzahl der verschiedenen kst- bzw. ks-werte            
!**              --> Anzahl der verschiedenen Teilbereiche              
!**   u        - FELD UMFANG                                            
!**   uges     - SUMME DER FELDGROESSEN u                               
!**   br       - FELD BREITE                                            
!**   brges    - SUMME DER FELDGROESSEN br                              
!**   f        - FELD Flaeche                                           
!**   fges     - SUMME DER FELDGROESSEN f                               
!**                                                                     
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**                                                                     
!**   a_hg    --      durchströmte Fläche des Flußschlauches            
!**   a_ks    --      durchströmte Fläche eines Teilabschnittes         
!**   akges   --      Gesamtrauheit                                     
!**   b_hg    --      Breite des Flußschlauches                         
!**   b_ks    --      Spiegelbreite eines Teilabschnittes               
!**   br      --      Spiegelbreite eines Teilabschnittes               
!**   br(1)   --      Spiegelbreite des linken Vorlandes                
!**   br(2)   --      Spiegelbreite des Flußschlauches                  
!**   br(3)   --      Spiegelbreite des rechten Vorlandes               
!**   brges   --      gesamte Spiegelbreite                             
!**   durchm  --      Durchmesser                                       
!**   f       --      durchströmte Teilfläche                           
!**   f(1)    --      durchströmte Fläche des linken Vorlandes          
!**   f(2)    --      durchströmte Fläche des Flußschlauches            
!**   f(3)    --      durchströmte Fläche des rechten Vorlandes         
!**   fges    --      gesamte durchströmte Fläche                       
!**   h_ks    --      Wasserspiegelhöhe eines Teilabschnittes           
!**   hm      --      mittlere Wasserspiegelhöhe                        
!**   hmax    --      maximale Wasserspiegelhöhe                        
!**   hr      --      Wasserspiegelhöhe                                 
!**   ht      --      Wasserspiegelhöhe                                 
!**   ianf    --      Anfangsprofilpunkt                                
!**   idruck  --      Charakterisierung des Druckabflusses              
!**   idruck  --      Charakterisierung des Druckabflusses              
!**   iend    --      Endprofilpunkt                                    
!**   indfl   --      Gerinneabschnitt
!**   indmax  --      Anzahl der Rauheitsabschnitte                     
!**   inside  --      Abfrage Schnittpunkt gefunden                     
!**   iprof   --      Art des Profils                                   
!**   ischl   --      linker Profilpunkt                                
!**   ischr   --      rechter Profilpunkt                               
!**   istart  --      Anfangsprofilpunkt                                
!**   istop   --      Endprofilpunkt                                    
!**   itrli   --      Punktnummer linke Trennfläche                     
!**   itrre   --      Punktnummer rechte Trennfläche                    
!**   k_ks    --      Rauheit eines Teilabschnittes                     
!**   maxkla  --      Anzahl Teilabschnitte                             
!**   nfli    --      Punktnummer linke Trennfläche                     
!**   nfre    --      Punktnummer rechte Trennfläche                    
!**   nknot   --      Anzahl der Punkte in einem Profil                 
!**   ns      --      Anzahl Schnittpunkt                               
!**   nsl     --      linker Schnittpunkt                               
!**   nsr     --      rechter Schnittpunkt                              
!**   r_hg    --      hydraulischer Radius des Flußschlauches           
!**   r_ks    --      hydraulischer Radius eines Teilabschnittes        
!**   ra      --      hydraulischer Radius eines Teilabschnittes        
!**   ra(1)   --      hydraulischer Radius des linken Vorlandes         
!**   ra(2)   --      hydraulischer Radius des Flußschlauches           
!**   ra(3)   --      hydraulischer Radius des rechten Vorlandes        
!**   rau     --      Rauheit eines Teilabschnittes                     
!**   rau     --      Rauheit eines Teilabschnittes                     
!**   raum    --      mittlere Rauheit                                  
!**   rad     --      Umrechnung in Grad
!**   steig   --      Steigung im Trapezprofil                          
!**   u       --      benetzter Teilumfang                              
!**   u(1)    --      benetzter Umfang des linken Vorlandes             
!**   u(2)    --      benetzter Umfang des Flußschlauches               
!**   u(3)    --      benetzter Umfang des rechten Vorlandes            
!**   u_hg    --      benetzter Umfang des Flußschlauches               
!**   u_ks    --      benetzter Umfang eines Teilabschnittes            
!**   u_tl1   --      benetzter Umfang der linken Trennfläche           
!**   u_tr1   --      benetzter Umfang der rechten Trennfläche          
!**   uges    --      gesamter benetzter Umfang                         
!**                                                                     
!**                                                                     
!**                                                                     
!**   Aufgerufene Routinen                                              
!**   --------------------                                              
!**   schnpkt(xra,y11,xmitte,y22,x1(i),h1(i),x1(i-1),                   
!**           h1(i-1),sx,sy,inside,ikenn)                               
!**                                                                     
!**                                                                     
!***********************************************************************

! ------------------------------------------------------------------
! Vereinbarungsteil
! ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE MOD_INI

! Calling variables
REAL, INTENT(INOUT) 	:: hr		! Wasserspiegelhoehe [m +NN]
DIMENSION xi ( * )                      ! r4(1...*) input  Horizontaldistanz der Profilpunkte
DIMENSION hi ( * )                      ! r4(1...*) input  Hoehendifferenzen zw. den P-Punkten
DIMENSION s ( * )                       ! r4(1...*) input  Distanz der Profilpunkte
INTEGER, INTENT(OUT)	:: indmax       ! Anzahl Rauhigkeitszonen im Profil
INTEGER, INTENT(OUT)    :: nfli         ! Nr. der Rauhigkeitszone an der li. Vorland/f-schlauch-trennflaeche
INTEGER, INTENT(OUT)    :: nfre         ! Nr. der Rauhigkeitszone an der re. Vorland/f-schlauch-trennflaeche

! Local variables
INTEGER, PARAMETER 		:: max_felder = 100
REAL, DIMENSION(max_felder) 	:: psim, omegm, betam, psie, omege, betae


! COMMON-Block /DARCY/ -------------------------------------------------------------
REAL 		:: ax (maxkla), ay (maxkla), dp (maxkla), htrre, htrli
INTEGER 	:: itrre, itrli
CHARACTER(LEN=2):: itr_typ_re, itr_typ_li
COMMON / darcy / ax, ay, dp, htrre, htrli, itrre, itrli, itr_typ_re, itr_typ_li
! ----------------------------------------------------------------------------------


! COMMON-Block /FROUD/ --------------------------------------------------------
INTEGER         :: indfl
REAL            :: froudi (maxkla)
COMMON / froud / indfl, froudi
! -----------------------------------------------------------------------------


! COMMON-Block /GES/ ----------------------------------------------------------
REAL 		:: fges, brges, uges, akges, vges, rhges, alges
COMMON / ges / fges, brges, uges, akges, vges, rhges, alges
! -----------------------------------------------------------------------------


! COMMON-Block /P2/ -----------------------------------------------------------
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


! COMMON-Block /PA_COM/ -------------------------------------------------------
REAL 		:: a_ks (maxkla), b_ks (maxkla), r_ks (maxkla), u_ks (maxkla)
REAL            :: k_ks (maxkla), h_ks (maxkla), v_ks (maxkla), l_ks (maxkla)
REAL            :: q_ks (maxkla), b_hg, a_hg, q_hg, v_hg, r_hg, l_hg, fr_hg
INTEGER         :: ischl, ischr
COMMON / pa_com / a_ks, b_ks, r_ks, u_ks, k_ks, h_ks, v_ks, l_ks, &
       & q_ks, b_hg, a_hg, u_hg, q_hg, v_hg, r_hg, l_hg, fr_hg, ischl, ischr
! -----------------------------------------------------------------------------


! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


! COMMON-Block /RO_FR/ --------------------------------------------------------
REAL            :: term1, term2, winkel, durchm_fr
COMMON / ro_fr / term1, term2, winkel, durchm_fr
! -----------------------------------------------------------------------------


! COMMON-Block /ROHR/ ---------------------------------------------------------
INTEGER         :: idruck
COMMON / rohr / idruck
! -----------------------------------------------------------------------------



! ------------------------------------------------------------------
! DATENFELDER
! ------------------------------------------------------------------
DATA omegm / 0.3268, 0.4625, 0.5668, 0.6548, 0.7326, 0.8030,      &
0.8679, 0.9284, 0.9854, 1.0394, 1.1018, 1.1429, 1.1792, 1.2121,   &
1.2425, 1.2711, 1.2981, 1.3240, 1.3488, 1.3728, 1.3962, 1.4189,   &
1.4411, 1.4628, 1.4842, 1.5053, 1.5261, 1.5466, 1.5670, 1.5873,   &
1.6074, 1.6274, 1.6475, 1.6541, 1.6741, 1.6941, 1.7142, 1.7342,   &
1.7543, 1.7743, 1.7945, 1.8146, 1.8348, 1.8550, 1.8753, 1.8957,   &
1.9161, 1.9365, 1.9571, 1.9777, 1.9984, 2.0192, 2.0401, 2.0610,   &
2.0821, 2.1033, 2.1247, 2.1461, 2.1677, 2.1895, 2.2114, 2.2334,   &
2.2557, 2.2781, 2.3007, 2.3236, 2.3466, 2.3699, 2.3935, 2.4173,   &
2.4414, 2.4658, 2.4905, 2.5155, 2.5410, 2.5668, 2.5930, 2.6197,   &
2.6469, 2.6746, 2.7029, 2.7319, 2.7615, 2.7919, 2.8231, 2.8552,   &
2.8883, 2.9227, 2.9583, 2.9954, 3.0343, 3.0753, 3.1187, 3.1652,   &
3.2155, 3.2710, 3.3337, 3.4078, 3.5039, 3.7352 /
DATA psim / 0.0022, 0.0061, 0.0113, 0.0173, 0.0242, 0.0318,       &
0.0400, 0.0488, 0.0582, 0.0681, 0.0784, 0.0892, 0.1003, 0.1117,   &
0.1233, 0.1351, 0.1472, 0.1594, 0.1717, 0.1842, 0.1968, 0.2096,   &
0.2226, 0.2353, 0.2484, 0.2614, 0.2746, 0.2878, 0.3010, 0.3143,   &
0.3276, 0.3409, 0.3542, 0.3676, 0.3809, 0.3942, 0.4075, 0.4209,   &
0.4341, 0.4474, 0.4607, 0.4739, 0.4871, 0.5003, 0.5134, 0.5266,   &
0.5396, 0.5527, 0.5656, 0.5786, 0.5915, 0.6043, 0.6171, 0.6298,   &
0.6424, 0.6550, 0.6675, 0.6799, 0.6923, 0.7045, 0.7167, 0.7288,   &
0.7408, 0.7527, 0.7644, 0.7761, 0.7877, 0.7991, 0.8105, 0.8217,   &
0.8327, 0.8437, 0.8545, 0.8651, 0.8756, 0.8859, 0.8961, 0.9061,   &
0.9159, 0.9255, 0.9349, 0.9441, 0.9531, 0.9619, 0.9705, 0.9788,   &
0.9868, 0.9946, 1.0021, 1.0092, 1.0161, 1.0226, 1.0288, 1.0345,   &
1.0398, 1.0446, 1.0489, 1.0525, 1.0553, 1.0568 /
DATA betam / 0.3260, 0.4601, 0.5625, 0.6483, 0.7234, 0.7909,      &
0.8527, 0.9098, 0.9631, 1.0132, 1.0580, 1.0940, 1.1242, 1.1503,   &
1.1733, 1.1936, 1.2119, 1.2282, 1.2430, 1.2563, 1.2683, 1.2790,   &
1.2886, 1.2972, 1.3047, 1.3113, 1.3170, 1.3218, 1.3258, 1.3289,   &
1.3312, 1.3326, 1.3333, 1.3333, 1.3329, 1.3323, 1.3313, 1.3301,   &
1.3285, 1.3266, 1.3245, 1.3220, 1.3192, 1.3162, 1.3128, 1.3090,   &
1.3050, 1.3007, 1.2960, 1.2910, 1.2857, 1.2800, 1.2740, 1.2676,   &
1.2610, 1.2539, 1.2465, 1.2387, 1.2306, 1.2220, 1.2131, 1.2038,   &
1.1940, 1.1839, 1.1733, 1.1623, 1.1508, 1.1389, 1.1265, 1.1136,   &
1.1001, 1.0862, 1.0716, 1.0565, 1.0408, 1.0245, 1.0075, 0.9898,   &
0.9714, 0.9522, 0.9322, 0.9113, 0.8894, 0.8666, 0.8426, 0.8175,   &
0.7910, 0.7632, 0.7337, 0.7024, 0.6690, 0.6333, 0.5948, 0.5528,   &
0.5066, 0.4549, 0.3955, 0.3241, 0.2301, 0.0001 /
DATA omege / 0.1161, 0.1650, 0.2031, 0.2358, 0.2651, 0.2921,      &
0.3174, 0.3422, 0.3668, 0.3912, 0.4154, 0.4393, 0.4631, 0.4868,   &
0.5102, 0.5335, 0.5566, 0.5796, 0.6024, 0.6251, 0.6476, 0.6700,   &
0.6923, 0.7145, 0.7365, 0.7585, 0.7803, 0.8021, 0.8237, 0.8453,   &
0.8667, 0.8881, 0.9093, 0.9306, 0.9517, 0.9727, 0.9937, 1.0146,   &
1.0355, 1.0562, 1.0770, 1.0976, 1.1182, 1.1388, 1.1593, 1.1798,   &
1.2002, 1.2206, 1.2409, 1.2612, 1.2815, 1.3017, 1.3219, 1.3421,   &
1.3622, 1.3824, 1.4025, 1.4225, 1.4426, 1.4627, 1.4827, 1.5027,   &
1.5227, 1.5428, 1.5628, 1.5828, 1.6028, 1.6228, 1.6428, 1.6629,   &
1.6830, 1.7032, 1.7235, 1.7440, 1.7646, 1.7853, 1.8062, 1.8274,   &
1.8488, 1.8704, 1.8924, 1.9148, 1.9375, 1.9607, 1.9843, 2.0086,   &
2.0335, 2.0591, 2.0856, 2.1130, 2.1416, 2.1716, 2.2033, 2.2370,   &
2.2734, 2.3133, 2.3583, 2.4112, 2.4796, 2.6433 /
DATA psie / 0.0008, 0.0021, 0.0039, 0.0059, 0.0082, 0.0107,       &
0.0133, 0.0161, 0.0190, 0.0221, 0.0253, 0.0287, 0.0322, 0.0358,   &
0.0395, 0.0433, 0.0473, 0.0514, 0.0556, 0.0599, 0.0643, 0.0688,   &
0.0734, 0.0781, 0.0829, 0.0878, 0.0928, 0.0979, 0.1030, 0.1083,   &
0.1136, 0.1190, 0.1244, 0.1300, 0.1356, 0.1412, 0.1470, 0.1528,   &
0.1586, 0.1645, 0.1705, 0.1765, 0.1826, 0.1887, 0.1949, 0.2011,   &
0.2074, 0.2137, 0.2200, 0.2264, 0.2328, 0.2392, 0.2457, 0.2522,   &
0.2587, 0.2652, 0.2718, 0.2784, 0.2850, 0.2916, 0.2982, 0.3048,   &
0.3155, 0.3182, 0.3248, 0.3315, 0.3381, 0.3448, 0.3515, 0.3581,   &
0.3647, 0.3713, 0.3779, 0.3844, 0.3909, 0.3973, 0.4037, 0.4100,   &
0.4162, 0.4224, 0.4284, 0.4344, 0.4403, 0.4460, 0.4517, 0.4572,   &
0.4625, 0.4677, 0.4728, 0.4776, 0.4823, 0.4867, 0.4909, 0.4949,   &
0.4986, 0.5019, 0.5049, 0.5074, 0.5094, 0.5105 /
DATA betae / 0.1137, 0.1583, 0.1908, 0.2166, 0.2380, 0.2561,      &
0.2716, 0.2863, 0.3006, 0.3146, 0.3281, 0.3414, 0.3543, 0.3668,   &
0.3790, 0.3910, 0.4025, 0.4138, 0.4248, 0.4355, 0.4459, 0.4561,   &
0.4659, 0.4755, 0.4848, 0.4938, 0.5026, 0.5111, 0.5194, 0.5274,   &
0.5351, 0.5426, 0.5499, 0.5569, 0.5637, 0.5703, 0.5766, 0.5827,   &
0.5886, 0.5942, 0.5997, 0.6049, 0.6098, 0.6146, 0.6192, 0.6235,   &
0.6276, 0.6315, 0.6352, 0.6387, 0.6420, 0.6450, 0.6479, 0.6506,   &
0.6530, 0.6553, 0.6573, 0.6591, 0.6608, 0.6622, 0.6635, 0.6645,   &
0.6653, 0.6660, 0.6664, 0.6666, 0.6666, 0.6661, 0.6650, 0.6633,   &
0.6610, 0.6581, 0.6545, 0.6503, 0.6455, 0.6400, 0.6338, 0.6270,   &
0.6194, 0.6610, 0.6019, 0.5919, 0.5811, 0.5694, 0.5568, 0.5431,   &
0.5283, 0.5122, 0.4949, 0.4761, 0.4556, 0.4333, 0.4087, 0.3816,   &
0.3512, 0.3166, 0.2764, 0.2274, 0.1621, 0.0001 /                    
                                                                        
                                                                        
! ------------------------------------------------------------------
! BERECHNUNGEN
! ------------------------------------------------------------------

idruck = 0
indmax = 1
indfl = 1
nfre = 0
nfli = 0
nsr = 0
nsl = 0
ischl = 1
ischr = nknot
                                                                        
!**   Winkel-Umrechnungsfaktor:                                         
rad = 180.0 / 3.1415926
brges = 0.
                                                                        
!**   u=UMFANG, br=BREITE, f=Flaeche                                    
!**   SETZE FELDER FUER ALLE PROFILABSCHNITTE ZU NULL                   
DO i = 1, maxkla
   u (i) = 0.
   f (i) = 0.
   br (i) = 0.
END DO
                                                                        
!MD: IF (hr.le.1.e-04) hr = hmax
!MD: Korrektur fur negative wsp
IF ( abs(hr-hmin) .le.1.e-04) hr = hmax


!**   ABFRAGE WELCHE PROFILART VORLIEGT, ZUNAECHST NORMALPROFIL         
IF (iprof (1:1) .eq.' ') then
                                                                        
  ! ---------------------------------------------------------------
  ! Errechnen der Schnittpunkte wsp mit Gelaende
  ! ---------------------------------------------------------------

  ! ISOHL ist Nummer des tiefsten Punktes im Profil
  ! X1(isohl) ist x-Koordinate von tiefstem Punkt
  xla = - 1000.
  xmitte = x1 (isohl)
  xra = 1000.

  !write (*,*) 'In UF. ISOHL = ', isohl, '  xmitte = ', xmitte


  ns = 0	! Anzahl der Schnittpunkte gelaende-wsp je seite
  ikenn = 1   	! Schnittpunkt zwischen 2 strecken gesucht, Kennung fuer SUB schnpkt


  ! ----------------------------------------------------------------
  ! Bestimmung der Schnittpunkte des Wasserspiegels mit dem Gelaende
  ! L I N K S  von dem tiefsten Punkt!
  ! ----------------------------------------------------------------
  schnittpunkte_links: DO i = 1, isohl - 1

    !write (*,*) 'In UF. Punkt ', i

    y11 = hr
    y22 = hr

    CALL schnpkt (xla, y11, xmitte, y22, x1 (i), h1 (i), x1 (i + 1), h1 (i + 1), sx, sy, inside, ikenn)

    IF (inside.ne.0) then
       ! schnittpunkt gefunden
       ischl = i
       !write (*,*) 'IN UF. Schnittpunkt sx = ', sx, '  sy = ', sy, '  ISCHL = ', ischl
       ns = ns + 1
    ENDIF
                                                                        
    !write (*,*) 'In UF. xmitte = ', xmitte, '  x1(i+1) = ', x1(i+1)

    IF ( (xmitte - x1(i+1) ) .lt. 1.e-3) then
      ! rechts der mitte
      IF (ns.eq.0) ischl = 1
      EXIT schnittpunkte_links
    ENDIF

  END DO schnittpunkte_links
                                                                        

  IF (ns.gt.1) then
    nsl = ns
  ENDIF
                                                                        
  ns = 0
                                                                        
  ! ----------------------------------------------------------------
  ! Bestimmung der Schnittpunkte des Wasserspiegels mit dem Gelaende
  ! R E C H T S  von dem tiefsten Punkt!
  ! ----------------------------------------------------------------
  DO i = nknot, isohl + 1, - 1
    y11 = hr
    y22 = hr
                                                                        
    CALL schnpkt (xra, y11, xmitte, y22, x1 (i), h1 (i), x1 (i-1), h1 (i-1), sx, sy, inside, ikenn)

    IF (inside.ne.0) then
      ! schnittpunkt gefunden
      ischr = i
      ns = ns + 1
    ENDIF
                                                                        
    IF (xmitte-x1 (i - 1) .gt.1.e-3) then
    ! naechste strecke laege links der mitte
    IF (ns.eq.0) ischr = nknot
      GOTO 2400
    ENDIF
                                                                        
  END DO
                                                                        
  2400 CONTINUE

  IF (ns.gt.1) nsr = ns
                                                                        

  IF (FLIESSGESETZ == 'MANNING_STR') then
    ! Ermitteln mittlere Rauhigkeiten im Flusschlauch
    ssfak = 0.
    ss = 0.
    istart = MAX (ianf, ischl)
    istop = MIN (iend, ischr)
                                                                        
    DO n = istart, istop - 1
      sfak = s (n) / (rau (n) **1.5)
      ssfak = ssfak + sfak
      ss = ss + s (n)
    END DO
                                                                        
    raum = ss / ssfak
    raum = raum** (2. / 3.)
                                                                        
    ! zuordnen der mittleren rauhigkeit:
    DO n = istart, istop - 1
      rau (n) = raum
    END DO
                                                                        
  ELSE

    nfli = itrli 
    nfre = itrre
                                                                        
  ENDIF
                                                                        
                                                                        
  ! Schleife ueber alle Punkte im Profil zwischen den durchstroemten
  ! Bereichen

  !write (*,*) ' In UF. ischl = ', ischl, '   ischr = ',ischr

  DO 210 n = ischl, ischr - 1

    x11 = 0
    f1 = 0
    s1 = 0
    h2 = 0
    hn = h1(n)
    hn2 = 0
    k_ks (n) = rau (n)
                                                                        
    !write (*,*) ' In UF. n = ', n, '   s(n) = ', s(n)

    IF (n.lt.ischl.or.n.gt. (ischr - 1) ) goto 205
    IF (h1(n) .ge.hr .and. h1(n+1) .ge.hr) goto 205
    IF (h1(n) .lt.hr .and. h1(n+1) .lt.hr) goto 201
    IF (h1(n) .ge.hr .and. h1(n+1) .lt.hr) goto 202
    IF (h1(n) .lt.hr .and. h1(n+1) .ge.hr) continue
                                                                        
    h11 = hr - h1(n)
    hn2 = h11
    !write (*,*) ' In UF. hn2 = ', hn2, '  hr = ', hr, '  h1(n) = ', h1(n), '  h1(n+1) = ', h1(n+1)

    GOTO 203

    202 CONTINUE

    h11 = hr - h1(n+1)
    hn = hr

    203 CONTINUE

    h2 = 0.
    fakt = h11/ hi(n)
    !MD: Keine Korrektur fur negative wsp, da hi absolut und h11 >0

    x11 = fakt * xi(n)
    s1 = fakt * s(n)
    f1 = h11 * x11/ 2.
    !MD: Keine Korrektur fur negative wsp

    GOTO 205

    201 CONTINUE

    s1 = s (n) 
    x11 = xi (n)
    h11 = hr - h1(n)
    h2 = hr - h1(n+1)
    hm = (h11 + h2) / 2.

    f1 = xi (n) * hm
    !MD: keine Korrektur fur negative wsp
                                                                       
    205 CONTINUE


    !JK           BERECHNUNG NACH MANNING-STRICKLER
    IF (FLIESSGESETZ == 'MANNING_STR') then

      IF (n.gt.1) then
                                                                        
        IF (n.eq.ianf) then
          indmax = indmax + 1
          indfl = indmax
        ELSEIF (n.eq.iend) then
          indmax = indmax + 1
        ELSEIF (abs (rau(n) - rau(n-1) ) .gt.1.e-06) then
          indmax = indmax + 1
        ENDIF

      ENDIF
                                                                        
      u (indmax) = u (indmax) + s1
      f (indmax) = f (indmax) + f1
      rk (indmax) = rau (n)

      IF (u (indmax) .gt.1.e-04) then
        ra (indmax) = f(indmax) / u(indmax)
      ELSE
        ra (indmax) = 0.
      ENDIF

      br(indmax) = br(indmax) + x11

      IF (n.eq.ianf) nfli = indmax
      IF (n.eq.iend) nfre = indmax - 1


    !JK  BERECHNUNG NACH DARCY-WEISBACH
    ! ----------------------------------
    ELSE

      indmax = 3
      indfl = 2

      a_ks (n) = f1
      b_ks (n) = x11
      u_ks (n) = s1
      h_ks (n) = hn

      !write (*,*) ' In UF. u_ks(',n,') = ', u_ks(n)


      IF (f1.gt.1.e-04) then
        r_ks (n) = a_ks (n) / u_ks (n)
      ELSE
        r_ks (n) = 0.
      ENDIF

      IF (abs (hn2) .gt.1.e-06) then
        h_ks (n+1) = hn + hn2
        !MD: = hr + hr -h1(n)
      ENDIF

      !JK              LINKES VORLAND
      IF (n.lt.itrli) then
        f (1)  = f (1) + a_ks (n)
        u (1)  = u (1) + u_ks (n)
        br (1) = br (1) + b_ks (n)

        IF (u (1) .gt.0) then
          ra (1) = f (1) / u (1)
        ELSE
          ra (1) = 0.0
        ENDIF

      !JK              RECHTES VORLAND
      ELSEIF (n.ge.itrre) then
        f (3) = f (3) + a_ks (n)
        u (3) = u (3) + u_ks (n)
        br (3) = br (3) + b_ks (n)

        IF (u (3) .gt.0) then
          ra (3) = f (3) / u (3)
        ELSE
          ra (3) = 0.0
        ENDIF

      !JK              FLUSSSCHLAUCH
      ELSE
        f (2) = f (2) + a_ks (n)
        u (2) = u (2) + u_ks (n)
        br (2) = br (2) + b_ks (n)
        a_hg = f (2)
        u_hg = u (2)
        b_hg = br (2)

        u_tl1 = hr - htrli
        u_tl1 = MAX (0., u_tl1)
        u_tr1 = hr - htrre
        u_tr1 = MAX (0., u_tr1)

        IF (u_hg.lt.1.e-03) then
          r_hg = 0.
        ELSE
          r_hg = a_hg / (u_hg + u_tl1 + u_tr1)
        ENDIF

        IF (u (2) .gt.0) then
          ra (2) = f (2) / u (2)
        ELSE
          ra (2) = 0.0
        ENDIF

      ENDIF

      IF (abs (hn2) .gt. 1.e-06) then

        !write (*,*) ' In UF. hn2 = ', hn2

        DO i1 = n + 2, nknot
          a_ks (i1) = 0.
          b_ks (i1) = 0.
          u_ks (i1) = 0.
          h_ks (i1) = 0.
          r_ks (i1) = 0.
          k_ks (i1) = rau (i1)
        END DO

        GOTO 211

      ENDIF

    ENDIF                                                  
                                                                        
  210 END DO
                                                                        
  !JK       BERECHNUNG NACH DARCY-WEISBACH
  IF (FLIESSGESETZ /= 'MANNING_STR') then

    IF (nknot.gt.0) then
      k_ks (nknot) = rau (nknot)
    ENDIF

  ENDIF

  211 CONTINUE

  IF (nfre.eq.0) then
    nfre = indmax
  ENDIF


!**   ENDE NORMALPROFIL, BEGINN TRAPEZPROFIL iprof=t
ELSEIF (iprof.eq.'t') then                              
                                                                        
  IF (hr.gt.hmax) then
    ht = hmax - h1 (1)
    idruck = 1
  ELSE
    ht = hr - h1 (1)
  ENDIF

  !**       FORMEL SCHNEIDER 12. AUFL., p. 13.20, m=steig, b=durchm
  f (2) = durchm * ht + steig * ht * ht
  !**       FORMEL SCHNEIDER 12. AUFL., p. 13.20, m=steig, b=durchm
  u (2) = durchm + 2 * ht * sqrt (steig * steig + 1)

  IF (abs (ht - hd) .le.1.e-02) u (2) = u (2) + durchm + 2 * steig * hd

  br (2) = durchm + 2 * ht * steig

  IF (u (2) .gt.1.e-04) then
    ra (2) = f (2) / u (2)
  ELSE
    ra (2) = 0.0
  ENDIF

  nfli = 1
  nfre = 1

  IF (rau (1) .gt.0.) indmax = 2

  rk (2) = rau (1)
  akges = rk (2)
  indfl = 1

  IF (FLIESSGESETZ /= 'MANNING_STR') then

    a_ks (1) = f (2)
    u_ks (1) = u (2)
    b_ks (1) = br (2)
    k_ks (1) = rau (1)

    IF (a_ks (1) .gt.1.e-04) then
      r_ks (1) = a_ks (1) / u_ks (1)
    ELSE
      r_ks (1) = 0.
    ENDIF

    u_hg = u (2)
    a_hg = f (2)
    b_hg = br (2)
    r_hg = ra (2)

    indmax = 2

  ENDIF

!**   ENDE TRAPEZPROFIL, BEGINN KREISPROFIL, iprof=k
ELSEIF (iprof.eq.'k') then

  !ep     04.02.2002 Ergänzung für Grenztiefenberechnung in Rohren
  durchm_fr = durchm

  IF (hr.ge.hmax) then
    idruck = 1
    ht = hmax - h1 (1)
  ELSE
    ht = hr - h1 (1)
  ENDIF

  term = 1. - ht / (durchm / 2.)
  term = MIN (1.0, term)
  term = MAX ( - 1.0, term)

  alf = 2 * acos (term) * rad

  f (2) = durchm * durchm / 8.
  term1 = pi / 180. * alf
  winkel = alf / rad

  term2 = sin (winkel)
  f (2) = f (2) * (term1 - term2)
  u (2) = 2 * pi * durchm / 2 * alf / 360.
  !br (2) = u (2)
  ! EN Korrekte Berechnung der Wasserspiegelbreite
  br (2) = durchm * SIN (winkel/2)

  IF (u (2) .gt.1.e-04) then
    ira = int (200. * f (2) / u (2) + 0.5)
    ra (2) = float (ira) / 200.
  ELSE
    ra (2) = 0.0
  ENDIF

  nfli = 1
  nfre = 1

  IF (rau (1) .gt.0.) indmax = 2
  rk (2) = rau (1)

  akges = rk (2)

  indfl = 1

  IF (FLIESSGESETZ /= 'MANNING_STR') then
    a_ks (1) = f (2)
    u_ks (1) = u (2)
    b_ks (1) = br (2)
    k_ks (1) = rau (1)

    IF (a_ks (1) .gt.1.e-04) then
      ira = int (200. * a_ks (1) / u_ks (1) + 0.5)
      r_ks (1) = float (ira) / 200.
    ELSE
      r_ks (1) = 0.
    ENDIF

    u_hg = u (2)
    a_hg = f (2)
    b_hg = br (2)
    r_hg = ra (2)
    indmax = 2

  ENDIF

!**   ENDE KREISPROFIL, BEGINN MAULPROFIL, iprof=m
ELSEIF (iprof.eq.'m') then

  IF (hr.gt.hmax) then
    idruck = 1
    ht = hmax - h1 (1)
  ELSE
    ht = hr - h1 (1)
  ENDIF

  nue1 = int (ht / hd * 100.)
  nue1 = MAX (nue1, 1)
  f (2) = psim (nue1) * hd * hd
  u (2) = omegm (nue1) * hd
  !br (2) = u (2)
  ! EN Korrekte Berechnung der Wasserspiegelbreite
  br (2) =2*(SQRT(1-(ht-hd/2)**2/(hd/2)**2)*durchm/2)


  IF (u (2) .gt.1.e-04) then
    ra (2) = f (2) / u (2)
  ELSE
    ra (2) = 0.0
  ENDIF

  nfli = 1
  nfre = 1

  IF (rau (1) .gt.0.) indmax = 2

  rk (2) = rau (1)
  akges = rk (2)
  indfl = 1

  IF (FLIESSGESETZ /= 'MANNING_STR') then
    a_ks (1) = f (2)
    u_ks (1) = u (2)
    b_ks (1) = br (2)
    k_ks (1) = rau (1)

    IF (a_ks (1) .gt.1.e-04) then
      r_ks (1) = a_ks (1) / u_ks (1)
    ELSE
      r_ks (1) = 0.
    ENDIF

    u_hg = u (2)
    a_hg = f (2)
    b_hg = br (2)
    r_hg = ra (2)
    indmax = 2
  ENDIF

!**   ENDE MAULPROFIL, BEGINN EIPROFIL, iprof=e
ELSEIF (iprof.eq.'e') then

  IF (hr.gt.hmax) then
    idruck = 1
    ht = hmax - h1 (1)
  ELSE
    ht = hr - h1 (1)
  ENDIF

  nue1 = int (ht / hd * 100.)
  nue1 = MAX (nue1, 1)
  f (2) = psie (nue1) * hd * hd
  u (2) = omege (nue1) * hd
  !br (2) = u (2)
  ! EN Korrekte Berechnung der Wasserspiegelbreite
  br (2) =2*(SQRT(1-(ht-hd/2)**2/(hd/2)**2)*durchm/2)


  IF (u (2) .gt.1.e-04) then
    ra (2) = f (2) / u (2)
  ELSE
    ra (2) = 0.0
  ENDIF

  nfli = 1
  nfre = 1

  IF (rau (1) .gt.0.) indmax = 2

  rk (2) = rau (1)
  akges = rk (2)
  indfl = 1

  IF (FLIESSGESETZ /= 'MANNING_STR') then

    a_ks (1) = f (2)
    u_ks (1) = u (2)
    b_ks (1) = br (2)
    k_ks (1) = rau (1)

    IF (a_ks (1) .gt.1.e-04) then
      r_ks (1) = a_ks (1) / u_ks (1)
    ELSE
      r_ks (1) = 0.
    ENDIF

    u_hg = u (2)
    a_hg = f (2)
    b_hg = br (2)
    r_hg = ra (2)
    indmax = 2

  ENDIF


ENDIF                                                                     
                                                                        
! ------------------------------------------------------------------
! Berechnung Parameter Gesamtprofil
! ------------------------------------------------------------------
                                                                        
3000 CONTINUE

do i = 1, indmax
  if (FLIESSGESETZ == 'DW_M_FORMBW' .and. iprof(1:1) == ' ') then
    formbeiwert(i) = GET_FORM( f(i), br(i) )
  else
    formbeiwert(i) = 1.0
  end if
end do

! WP 11.07.2005
!write (*, 1001) br(1), br(2), br(3)
!1001 format (/1X, 'Breite linkes Vorland:  ', F10.3, /, &
!            & 1X, 'Breite Flussschlauch:   ', F10.3, /, &
!            & 1X, 'Breite rechtes Vorland: ', F10.3)
!write (*, 1002) f(1), f(2), f(3)
!1002 format (/1X, 'Fliessflaeche linkes Vorland:  ', F10.3, /, &
!            & 1X, 'Fliessflaeche Flussschlauch:   ', F10.3, /, &
!            & 1X, 'Fliessflaeche rechtes Vorland: ', F10.3)
!write (*, 1003) formbeiwert(1), formbeiwert(2), formbeiwert(3)
!1003 format (/1X, 'Formbeiwert linkes Vorland:  ', F10.3, /, &
!            & 1X, 'Formbeiwert Flussschlauch:   ', F10.3, /, &
!            & 1X, 'Formbeiwert rechtes Vorland: ', F10.3)


fges 	= 0.
brges 	= 0.
uges 	= 0.
                                                                        
! indmax = Anzahl Rauhigkeitszonen im Profil

DO i = 1, indmax

  IF (FLIESSGESETZ == 'DW_M_FORMBW' .or. FLIESSGESETZ == 'DW_O_FORMBW') then
    fges = fges + f (i)
    brges = brges + br (i)
    uges = uges + u (i)
  ELSEIF (rk (i) .gt.0.001) then
    fges = fges + f (i)
    brges = brges + br (i)
    uges = uges + u (i)
  ENDIF

END DO

!**   indmax = Anzahl Rauhigkeitszonen im Profil
IF (indmax.eq.0) then
  write (*,1000)
  1000 format (/1X, 'Fehler! Keine Rauhigkeiten definiert!', /, &
              & 1X, '-> Programmabruch!')
  STOP
ENDIF                                                       
                                                                        
END SUBROUTINE uf
