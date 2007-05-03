C     Last change:  K    17 Apr 2007    9:53 am
      MODULE PARAFlow1dFE


      REAL(KIND=8),ALLOCATABLE       :: apoly(:,:)                   !EFa Nov06 Fl�chenpolynom
      REAL(KIND=8),allocatable       :: hhmin(:),hhmax(:)            !EFa Nov06 min/max h-Werte f�r Polynomg�ltigkeit
      REAL(KIND=8),ALLOCATABLE       :: qpoly(:,:)                   !EFa Nov06 Q(h)-Kurve
      REAL(KIND=8),ALLOCATABLE       :: qgef(:)                      !EFa Nov06 Reibungsgef�lle f�r welches Q(h)-Kurve bestimmt wurde
      REAL(KIND=8),ALLOCATABLE       :: hbordv(:)                    !EFa Nov06 h_bordvoll
      REAL(KIND=8),ALLOCATABLE       :: alphah(:)                    !EFa Nov06 H�he des �bergangsbereiches alpha
      REAL(KIND=8),ALLOCATABLE       :: alphad(:,:)                  !EFa Nov06 alpha-�bergang bis h
      REAL(KIND=8),ALLOCATABLE       :: alphapk(:,:)                 !EFa Nov06 alpha-Polynomkoeffizienten
      REAL(KIND=8),ALLOCATABLE       :: betah(:)                     !EFa Nov06 H�he des �bergangsbereiches beta
      REAL(KIND=8),ALLOCATABLE       :: betad(:,:)                   !EFa Nov06 beta-�bergang bis h
      REAL(KIND=8),ALLOCATABLE       :: betapk(:,:)                  !EFa Nov06 beta-Polynomkoeffizienten
      REAL(KIND=8),allocatable       :: ah(:)                        !EFa Nov06 A(h) an den Eckknoten
      REAL(KIND=8),ALLOCATABLE       :: Intah(:)                     !Integration von A over h
      REAL(KIND=8),allocatable       :: qh(:)                        !EFa Nov06 Q(h) an den Eckknoten
      REAL(KIND=8),allocatable       :: dahdh(:)                     !EFa Nov06 Ableitung A(h) nach dh
      REAL(KIND=8),allocatable       :: dqhdh(:)                     !EFa Nov06 Ableitung Q(h) nach dh
      REAL(KIND=8),allocatable       :: d2ahdh(:)                    !EFa Nov06 2. Ableitung A(h) nach dh
      REAL(KIND=8),allocatable       :: d2qhdh(:)                    !EFa Nov06 2. Ableitung Q(h) nach dh
      REAL(KIND=8),ALLOCATABLE       :: kmx(:)                       !EFa Nov06 Kilometrierung f�r 1D-Teschke-Elemente
      REAL(KIND=8),ALLOCATABLE       :: sfnod(:)                     !EFa Nov06 Reibungsgef�lle am Knoten
      REAL(KIND=8),ALLOCATABLE       :: sfwicht(:)                   !EFa Nov06 Wichtungsfaktor f�r Reibungsgef�lle zur Korrektur
      REAL(KIND=8),ALLOCATABLE       :: dsfnoddh(:)                  !EFa Nov06 Ableitung sfnod nach dh
      REAL(KIND=8),ALLOCATABLE       :: dsfnoddq(:)                  !EFa Nov06 Ableitung sfnod nach dq
      REAL(KIND=8)                   :: qrandb                       !EFa Dec06 Durchfluss (RB) f�r alle Subroutinen deklarieren in sbgen
      INTEGER                        :: beient                       !EFa Nov06 Schalter f�r Beiwert (2: beta, 1: alpha, 0. ignorieren)
      REAL(KIND=8),allocatable       :: hdif(:)                      !EFa Nov06 H�he des �bergangsbereiches entsprechend der Beiwerte
      REAL(KIND=8),ALLOCATABLE       :: pdif(:,:)                    !EFa Nov06 Beiwert-Polynomkoeffizienten-�bergang
      REAL(KIND=8),ALLOCATABLE       :: pbei(:,:)                    !EFa Nov06 Beiwert-Polynomkoeffizienten
      REAL(KIND=8),ALLOCATABLE       :: bei(:)                       !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dbeidh(:)                    !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2beidh(:)                   !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dbeizdh(:)                   !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2beizdh(:)                  !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dbeiodh(:)                   !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2beiodh(:)                  !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: bnode(:)                     !EFa Nov06 fiktive Breite an den Knoten
      REAL(KIND=8),ALLOCATABLE       :: hhint(:,:)                   !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dhhintdx(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dhintdt(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: qqint(:,:)                   !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: qqintdx(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dqintdt(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: areaint(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dareaintdh(:,:)              !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2areaintdh(:,:)             !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: Intareaint(:,:)
      REAL(KIND=8),ALLOCATABLE       :: daintdx(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2aintdx(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2aidhdx(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: daintdt(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: qschint(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dqsintdh(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2qsidh(:,:)                 !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dqsintdx(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2qsidhdx(:,:)               !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: s0schint(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: sfwicint(:,:)                !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: sfint(:,:)                   !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dsfintdh1(:,:)               !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dhht(:,:)                    !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: beiint(:,:)                  !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dbeiintdh(:,:)               !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2beiintdh(:,:)              !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: dbeiintdx(:,:)               !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: d2beiintdhdx(:,:)            !EFa Nov06
      REAL(KIND=8),ALLOCATABLE       :: bint(:,:)                    !EFa Nov06 fiktive Breite an den Gausspunkten
      REAL(KIND=8),ALLOCATABLE       :: qqt(:)                       !EFa Nov06 speichern des Durchflusses f�r den n�chsten Zeitschritt
      REAL(KIND=8),ALLOCATABLE       :: dqqt(:,:)                    !EFa Nov06 Zeitableitung des Durchflusses
      REAL(KIND=8),ALLOCATABLE       :: dqdtaltzs(:,:)               !EFa Nov06 speichern der Zeitableitung des Durchflusses f�r den n�chsten Zeitschritt
      REAL(KIND=8)                   :: urfc                         !EFa Nov06 Relaxationsfaktor (????) muss schon vor update initialisiert werden
      REAL(KIND=8),ALLOCATABLE       :: froude(:)                    !EFa Nov06 Froudezahl an den Knoten
      REAL(KIND=8),ALLOCATABLE       :: froudeint(:,:)               !EFa Nov06 Froudezahl an den Gausspunken
      REAL(KIND=8),ALLOCATABLE       :: sbot(:)                      !EFa Nov06 Sohlgef�lle
      REAL(KIND=8),ALLOCATABLE       :: vflowint(:,:)                !EFa Dec06, Flie�geschwindigkeit
      REAL(KIND=8),ALLOCATABLE       :: dvintdx(:,:)                 !EFa Dec06, Ableitung der Flie�geschwindigkeit nach dx
      REAL(KIND=8),ALLOCATABLE       :: teschke(:)                   !EFa Dec06, gibt anhand der Fl�chenpolynomdaten an, ob mit 1d-Teschke-Elementen gerechnet wird
      !REAL,ALLOCATABLE       :: qhalt(:,:)                   !EFa Dec06, Durchfluss des vorherigen Zeitschrittes
      REAL(KIND=8),ALLOCATABLE       :: qhalt(:)                     !EFa Dec06, Durchfluss des vorherigen Zeitschrittes
      REAL(KIND=8),ALLOCATABLE       :: dhdtaltzs(:,:)               !EFa Dec06, speichern der Zeitableitung der H�he f�r den n�chsten Zeitschritt
      REAL(KIND=8),ALLOCATABLE       :: hhalt(:,:)                   !EFa Dec06, H�he des vorherigen Zeitschrittes
      REAL(KIND=8),ALLOCATABLE       :: hht(:)                       !EFa Dec06
      REAL(KIND=8),allocatable       :: kennung(:)                   !EFa Jan07, wird in coef1dFE mit 1 belegt; Kennung f�r Konten eines Teschke-Elementes
      REAL(KIND=8),ALLOCATABLE       :: dvvt(:,:)                    !EFa Apr07, Zeitableitung der Flie�geschwindigkeit
      REAL(KIND=8),ALLOCATABLE       :: dvdtaltzs(:,:)               !EFa Apr07, speichern der Zeitableitung der Flie�geschwindigkeit f�r den n�chsten Zeitschritt
      REAL(KIND=8),ALLOCATABLE       :: vvt(:)                       !EFa Apr07, speichern der Flie�geschwindigkeit f�r den n�chsten Zeitschritt
      REAL(KIND=8),ALLOCATABLE       :: dvintdt(:,:)                 !EFa Apr07, Zeitableitung der Flie�geschwindigkeit im Element
      !nis,feb07,testing
      REAL (KIND = 8), allocatable   :: matrix(:,:), vector(:)
      !-

      END MODULE 