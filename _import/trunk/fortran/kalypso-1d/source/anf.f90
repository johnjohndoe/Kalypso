!     Last change:  WP    2 Jun 2006   11:00 pm
!--------------------------------------------------------------------------
! This code, anf.f90, contains the following subroutines
! and functions of the hydrodynamic modell for
! 1D steady state calculations: KALYPSO-1D
!
! Subroutines:
! - anf
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
SUBROUTINE anf (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,    &
              & psiein, psiort, ikenn, froud, xi, hi, s, ifehl, &
              & nblatt, nz, istat)

!***********************************************************************
!**                                                                     
!**   SUBROUTINE ANF                                                    
!**                                                                     
!**   geschrieben: P. Koch, Maerz 1990                                  
!**                                                                     
!**   Programmbeschreibung:                                             
!**                                                                     
!**   Dieses Programm ermittelt den Wasserspiegel an einem Profil       
!JK   mittels Diagramm.                                                 
!**                                                                     
!**   VARIABLEN                                                         
!**   ---------                                                         
!**   ik    - Anzahl gefundenen Schnittpunkte                           
!**   itmax - max. anzahl der iterationsschritte                        
!**   err   - Genauigkeitsschranke fuer wsp-ermittlung bei Iteration    
!**                                                                     
!**   IN DIESER SUBROUTINE WEITERHIN VERWENDETE VARIABLEN               
!**   ---------------------------------------------------               
!**                                                                     
!**   a(1-4)  --      Charakterisierung der gefundenen Schnittpunkte    
!**   dff     --      Differenz                                         
!**   dffmin  --      minimale Differenz                                
!**   dx      --      Schrittweite                                      
!**   f1      --      Schnittpunktkoordinaten                           
!**   f2      --      Schnittpunktkoordinaten                           
!**   froud   --      Froud-Zahl                                        
!**   frouda  --      Froud-Zahl                                        
!**   h1x     --      Schnittpunktkoordinaten                           
!**   h2x     --      Schnittpunktkoordinaten                           
!**   hborda  --      Verlusthöhe durch Profileinengung                 
!**   hmin    --      minimale Wasserspiegelhöhe                        
!**   hr      --      Wasserspiegelhöhe                                 
!**   hra     --      Wasserspiegelhöhe                                 
!**   hrneu   --      Wasserspiegelhöhe                                 
!**   hrneua  --      Wasserspiegelhöhe                                 
!**   hv      --      Geschwindigkeitsverlusthöhe                       
!**   idruck  --      Charakterisierung des Druckabflusses              
!**   ifehl   --      Parameter für Fehlermeldungen                     
!**   iprof   --      Art des Profils                                   
!**   ischn   --      Anzahl der Schnittpunkte                          
!**   nblatt  --      Anzahl der Blätter im Ergebnisfile
!**   nz      --      Anzahl der Zeilen im Ergebnisfile                 
!**  ws1     --      Wasserspiegelhöhe                                  
!**                                                                     
!**                                                                     
!**   AUFGERUFENEN ROUTINEN                                             
!**   ---------------------                                             
!**   kopf(nblatt,nz,jw5,jw7,idr1)
!**   verluste(str,q,q1,i,hr,hv,rg,hvst,hrst,indmax,                    
!**            psiein,psiort,jw5,hi,xi,s,istat,froud,ifehlg,jsch)       
!**                                                                     
!***********************************************************************
                                                                        
                                                                        
!**   ------------------------------------------------------------------
!**   VEREINBARUNGSTEIL                                                 
!**   ------------------------------------------------------------------
                                                                        
!WP 01.02.2005
USE DIM_VARIABLEN
USE KONSTANTEN
USE IO_UNITS

CHARACTER(LEN=1) :: iprof
CHARACTER(LEN=1) :: a (10)

!WP 28.07.2004
CHARACTER(LEN=1) :: idr1
!WP 28.07.2004

INTEGER :: ianf, iend

REAL xi (maxkla), hi (maxkla), s (maxkla)

REAL x1 (maxkla), h1 (maxkla), rau (maxkla), durchm, hd, sohlg, steig, boli, bore, hmin, hmax

! COMMON-Block /PROF_HR/ ------------------------------------------------------
REAL 		:: f (maxkla), u (maxkla), br (maxkla), ra (maxkla), rb (maxkla)
REAL 		:: v (maxkla), qt (maxkla), ts1 (maxkla), ts2 (maxkla)
REAL 		:: rk (maxkla), ra1 (maxkla), formbeiwert(maxkla)
COMMON / profhr / f, u, br, ra, rb, v, qt, ts1, ts2, rk, ra1, formbeiwert
! -----------------------------------------------------------------------------


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




REAL hborda, heins, horts
REAL bolip (maxger), borep (maxger), sohlp (maxger), stat (maxger)
REAL hbv (maxger), isstat (maxger), hmingp (maxger), k_kp (maxger)

COMMON / p2 / x1, h1, rau, nknot, iprof, durchm, hd, sohlg, steig,&
boli, bore, hmin, hmax, ianf, iend, hrbv

COMMON / vort / hborda, heins, horts

COMMON / ges / fges, brges, uges, akges, vges, rhges, alges

COMMON / laengs / bolip, borep, sohlp, stat, hbv, isstat, hmingp, k_kp

COMMON / alt / ws1, rg1, vmp1, fges1, hv1, ikenn1
!     wird benoetigt in stationaer und wspanf (definition in normber)
COMMON / peg / h1x, h2x, f1, f2
COMMON / rohr / idruck                                                 
                                                                        
                                                                        
!     werte vom vorhergehenden profil umspeichern                       
                                                                        
                                                                        
!**   ------------------------------------------------------------------
!**   BERECHNUNGEN                                                      
!**   ------------------------------------------------------------------
                                                                        
      a (1) = ' ' 
      a (2) = ' ' 
      a (3) = ' ' 
      a (4) = ' ' 
                                                                        
      !**   SCHREIBEN IN KONTROLLFILE
      IF (iprof.ne.' ') then
        WRITE (UNIT_OUT_LOG, '('' Art des Profils : iprof = '',a1)') iprof
      ENDIF
      WRITE (UNIT_OUT_LOG, '('' Bestimmen Einschlussintervall in anf.f90 : '' &
       &            ,/,''   i   hr        hrneu      froud'',             &
       &            ''     hvst      hrst'',/)')

      !**   BEREITS DEAKTIVIERT, 20.01.00 -------------------------
      !     ermitteln hrkrit: d.h. keine beeinflussung vom uw mehr,
      !                       wenn hrneu --> hrkrit
      !      hrkrit=ws1+(q+q1)/rg1*str+hv1+heins+horts
      !      if (lein.eq.3) then
      !      write(UNIT_OUT_LOG,'('' grenzwert hrneu --> hrkrit = '',f15.3)') hrkrit
      !      endif
      !**   ---------------------------------------------------------------          
                                                                        
      ik = 0 
      ifehl = 0 
      hr = hmin + 0.005 
      dffmin = 1.e-04 
      dff = 1.0 
      jmin = 1 
                                                                        
      101 CONTINUE

      dx = 0.2 
      ischn = 0 


      1000 CONTINUE 

      jzaehl = 0 
                                                                        
      !**   ------------------------------------------------------------------
      !**   Iterationsschleife
                                                                        
      jverl = 0 
      jmax = 100 
                                                                        
      DO 100 jsch = jmin, jmax 
                                                                        
        hborda = 0. 
                                                                        
  102   CALL verluste (str, q, q1, i, hr, hv, rg, hvst, hrst, indmax,   &
        psiein, psiort, hi, xi, s, istat, froud, ifehlg, jsch)
                                                                        
        hrneu = ws1 + hrst + hvst + hborda + heins + horts 
                                                                        
!         if (hv.lt.0.or.hv.gt.ws1.or.hv.gt.10.0) then                  
                                                                        
        IF (hv.gt.ws1.or.hv.gt.10.0) then 
          hr = hr + 0.01 
          jverl = jverl + 1 
          IF (jsch.eq.jmax) then 
                                                                        
!JK               WENN 100 DURCHLAEUFE ERREICHT                         
!JK               --> ZUM ANFANG DER ITERATIONSSCHLEIFE                 
            GOTO 1000 
                                                                        
          ELSE 
                                                                        
!JK               NEUE VERLUSTBERECHNUNG                                
            IF (jverl.lt.jmax) goto 102 
                                                                        
!JK               WENN 100 DURCHLAEUFE ERREICHT                         
!JK               --> ZUM ANFANG DER ITERATIONSSCHLEIFE                 
            GOTO 1000 
                                                                        
          ENDIF 
        ENDIF 
                                                                        
        !**      SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(i4,5f10.3)') jsch, hr, hrneu, froud, hvst, hrst

        IF (jsch.gt.1) then 
                                                                        
          dff = abs (hrneu - hrneua) 
                                                                        
          IF ( (hrneua - hra) * (hrneu - hr) .le.0.) then 
            !JK            NULLDURCHGANG
            !              abspeichern des einschlussintervalls
            h1x = hra 
            h2x = hr 
            f1 = hrneua 
            f2 = hrneu 
                                                                        
            IF (hrneu.le.hmin) then 
              ik = ik + 1 
              a (ik) = 'u' 
                                                                        
              !**              SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '(1x,i1,''ter schnittpunkt'')') ik

              jmin = 1 
                                                                        
              !JK              BESTIMMUNG DES NAECHSTEN SCHNITTPUNKTES
              !JK              (SCHLEIFENANFANG)
              GOTO 101 
                                                                        
            ELSEIF (froud.ge.1.) then 
                                                                        
              IF (idruck.ne.0) then 
                                                                        
                !**                    SCHREIBEN IN KONTROLLFILE
                WRITE (UNIT_OUT_LOG, '('' froud>1. und druckabfluss -->'',  &
                 &       /,'' Froudzahl nicht mehr massgebend '')')

                ik = ik + 1 
                a (ik) = 'm' 
                hr = h2x 
                jmin = 1 
                                                                        
                !JK                 BESTIMMUNG DES NAECHSTEN SCHNITTPUNKTES
                !JK                 (SCHLEIFENANFANG)
                GOTO 101 
                                                                        
              ENDIF 
                                                                        
              !**                  SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '('' froud>1 !! --> '',              &
               &        '' Einschlussintervall komplett '',             &
               &        ''im schiessenden Bereich'',/,                  &
               &        '' suchen naechster Schnittpunkt'')')
              WRITE (UNIT_OUT_LOG, '(''froud '',f15.3,'' h2x = '',f15.3)') froud, h2x

              hr = h2x 
              ik = ik + 1 
              !JK                  SCHIESSENDER ABFLUSS
              a (ik) = 's' 
                                                                        
              !**                  SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '(1x,i1,''ter schnittpunkt'')') ik

              jmin = 1 
                                                                        
              !JK                  BESTIMMUNG DES NAECHSTEN SCHNITTPUNKTES
              !JK                  (SCHLEIFENANFANG)
              GOTO 101 
                                                                        
            ELSEIF (frouda.lt.1.) then 
                                                                        
              !**                  SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '('' einschlussintervall komplett '',   &
               &            ''im stroemenden bereich '',/,                        &
               &            '' h1x = '',f15.3,'' frouda = '',f15.3)') h1x, frouda

              ik = ik + 1 

              !JK                  STROEMENDER ABFLUSS                                
              a (ik) = 'm' 
                                                                        
              !**                  SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '(1x,i1,''ter schnittpunkt'')') ik

              hr = h1x 
                                                                        
              !JK                  DATENUEBERGABE
              GOTO 9999 
                                                                        
            ELSE 
                                                                        
              !**                  SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '( ''Mischintervall (h1x<hgrenz,h2x>hgrenz)'',/ &
               &            '' --> Intervall verkleinern'')')

              dx = dx / 2. 
              hr = h1x + dx 
                                                                        
              IF (dx.lt.0.01) then 
                                                                        
                !**                    SCHREIBEN IN KONTROLLFILE
                WRITE (UNIT_OUT_LOG, '(''Keine Konvergenz beim Misch-'',/ &
                 &             ''intervall. Schnittpunt im '',/           &
                 &          ''schiessenden Bereich angenommen.'')')

                ik = ik + 1 
                !JK                    SCHIESSENDER ABFLUSS
                a (ik) = 's' 
                hr = h2x 
                jmin = 1 
                                                                        
                !JK                    BESTIMMUNG DES NAECHSTEN SCHNITTPUNKTES
                !JK                    (SCHLEIFENANFANG)
                GOTO 101 
                                                                        
              ENDIF 
                                                                        
              !JK                 BESTIMMUNG DES NAECHSTEN SCHNITTPUNKTES
              !JK                 (SCHLEIFENANFANG)
              GOTO 100 

            ENDIF 
                                                                        
          !JK        ENDIF ZU ((hrneua-hra)*(hrneu-hr).le.0.)
          ENDIF 

        !JK       ENDIF ZU (jsch.gt.1)
        ENDIF 
                                                                        
        !JK       WENN GRENZKRITERIUM ERREICHT
        IF (abs (dff) .le.dffmin) then 
                                                                        
          IF (hr.gt.ws1) then 
                                                                        
            IF (idruck.eq.1) then 
              IF (hr.lt.hrneu) then 
                hrneua = hrneu 
                hra = hr 
                frouda = froud 
                jmin = 2 
                hr = hr + dx 
                GOTO 101 
              ENDIF 
            ENDIF 
                                                                        
            IF (hr.lt.hrneu) then 
              hrneua = hrneu 
              hra = hr 
              frouda = froud 
              jmin = 2 
              hr = hr + dx 
              GOTO 101 
            ENDIF 
                                                                        
            !**             ik = Anzahl Schnittpunkte, wo wird ischn oberhalb
            !**             gesetzt??? UT, nur in folgenden Zeilen?
            !JK             ISCHN WIRD DIREKT VOR SCHLEIFENANFANG = 0 GESETZT
            IF (ik.eq.0.and.ischn.eq.0) then 
                                                                        
              !**                SCHREIBEN IN KONTROLLFILE
              WRITE (UNIT_OUT_LOG, '('' keine Schnittpunkt in anf --> '', &
               &   '' Verringere Intervall dx auf 0.05'',/,                       &
               &   '' Nochmaliger Suchlauf'')')

              dx = 0.05 
              ifehl = 0 
              hr = hmin + 0.005 
              ischn = 1 
                                                                        
              !JK              ZUM SCHLEIFENANFANG,
              !JK              MIT KLEINEREM dx NOCHMALIGER DURCHLAUF
              GOTO 1000 
                                                                        
            ENDIF 
                                                                        
            !**             SCHREIBEN IN KONTROLLFILE
            WRITE (UNIT_OUT_LOG, '(''keine aenderung des wsp mehr -->'', &
             &  '' beeinflussung vom uw '',/,                            &
             &  '' --> kein zusaetzlicher schnittpunkt mehr '')')

            !JK          DATENUEBERGABE
            GOTO 9999 
                                                                        
          !JK          ENDIF ZU (hr.gt.ws1)
          ENDIF 
        !jk       ENDIF ZU (abs(dff).le.dffmin)
        ENDIF 
                                                                        
        hra = hr 
        hrneua = hrneu 
        frouda = froud 
        hr = hr + dx 
                                                                        
      !**   ENDE DER SCHLEIFE
      100 END DO
                                                                        
      IF (idruck.eq.1) then 
        IF (hr.lt.hrneu) then 
          hrneua = hrneu 
          hra = hr 
          frouda = froud 
          jmin = 2 
          hr = hr + dx 
          !JK                  NOCHMALIGER SCHLEIFENDURCHLAUF
          GOTO 101 
        ENDIF 
      ENDIF 
                                                                        
      !JK   WENN KEIN SCHNITTPUNKT GEFUNDEN
      IF (ik.eq.0) then 

        !JK     WENN KEIN SUCHDURCHLAUF MIT KLEINEREM INTERVALL
        !JK     DURCHGEFUEHRT WURDE
        IF (ischn.eq.0) then 
                                                                        
          !**       SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '('' keine schnittpunkt in anf --> '', &
           &           '' Verringere Intervall dx auf 0.05'',/,       &
           &           '' Nochmaliger Suchlauf'')')

          dx = 0.05 
          ik = 0 
          ifehl = 0 
          hr = hmin + 0.005 
          ischn = 1 
                                                                        
          !JK        NOCHMALIGER SUCHDURCHLAUF MIT KLEINEREM INTERVALL
          GOTO 1000 
                                                                        
        ELSE 
                                                                        
          !**     SCHREIBEN IN KONTROLLFILE
          WRITE (UNIT_OUT_LOG, '('' keine schnittpunkt in anf --> '', &
           &             '' keine konvergenz moeglich'',/,            &
           &             '' --> profile einschalten'')')

        ENDIF 
                                                                        
        !**   SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(''Keinen Schnittpunkt im stroemenden Bereich gef.'')')

        ifehl = 1 
                                                                        
        !     /* nur schnittpunkt im schiessenden bereich
                                                                        
      ENDIF 
                                                                        

 9999 CONTINUE

      !**   SCHREIBEN IN KONTROLLFILE
      WRITE (UNIT_OUT_LOG, '(/,'' insgesamt '',i1,'' schnittpunkte '',3(1x,a1),/)&
       &') ik,  (a (i1) , i1 = 1, 3)

      IF (ik.eq.0) then 
        nz = nz + 2 
        IF (nz.gt.50) then 
          nblatt = nblatt + 1 
          CALL kopf (nblatt, nz, UNIT_OUT_TAB, UNIT_OUT_PRO, idr1)
        ENDIF

        !**      SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(/,''Kein Einfluss vom Unterwasser.   '')')
        WRITE (UNIT_OUT_LOG, '(/,'' --> Fall 1: Profile einschalten '')')

        ifehl = 2 
                                                                        
      ELSEIF (ik.eq.1.and.a (1) .eq.'u') then 
                                                                        
        !**     SCHREIBEN IN KONTROLLFILE
        WRITE (UNIT_OUT_LOG, '(''Nur ein Schnittpunkt, und der im Schiessenden.'', &
         &              /,''Diskontinuitaet --> Es muss ein Fliesswechsel'',&
         &            '' stattgefunden haben (vgl. seus/uslu)'')')

        ik = 0 
        ifehl = 2 
                                                                        
      ENDIF 
                                                                        
      !**   SCHREIBEN IN KONTROLLFILE
      IF (ifehl.ne.2) then
        WRITE (UNIT_OUT_LOG, '(''in anf --> h1x = '',f15.3,'' h2x = '',f15.3,/, &
         &              ''           f1  = '',f15.3,'' f2  = '',f15.3,/,    &
         &              ''          hmin = '',f15.3)') h1x, h2x, f1, f2, hmin
      ENDIF 
                                                                        
      hr = h1x 
                                                                        
      RETURN 
                                                                        
      !UT   ENDE SUB anf
      END SUBROUTINE anf                            
