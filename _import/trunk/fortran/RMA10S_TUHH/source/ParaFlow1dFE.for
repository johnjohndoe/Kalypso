C     Last change:  ST   22 Jan 2007   10:50 am
      MODULE PARAFlow1dFE


      real,ALLOCATABLE       :: apoly(:,:)                   !EFa Nov06 Fl�chenpolynom
      REAL,allocatable       :: hhmin(:),hhmax(:)            !EFa Nov06 min/max h-Werte f�r Polynomg�ltigkeit
      REAL,ALLOCATABLE       :: qpoly(:,:)                   !EFa Nov06 Q(h)-Kurve
      REAL,ALLOCATABLE       :: qgef(:)                      !EFa Nov06 Reibungsgef�lle f�r welches Q(h)-Kurve bestimmt wurde
      REAL,ALLOCATABLE       :: hbordv(:)                    !EFa Nov06 h_bordvoll
      REAL,ALLOCATABLE       :: alphah(:)                    !EFa Nov06 H�he des �bergangsbereiches alpha
      REAL,ALLOCATABLE       :: alphad(:,:)                  !EFa Nov06 alpha-�bergang bis h
      REAL,ALLOCATABLE       :: alphapk(:,:)                 !EFa Nov06 alpha-Polynomkoeffizienten
      REAL,ALLOCATABLE       :: betah(:)                     !EFa Nov06 H�he des �bergangsbereiches beta
      REAL,ALLOCATABLE       :: betad(:,:)                   !EFa Nov06 beta-�bergang bis h
      REAL,ALLOCATABLE       :: betapk(:,:)                  !EFa Nov06 beta-Polynomkoeffizienten
      REAL,allocatable       :: ah(:)                        !EFa Nov06 A(h) an den Eckknoten
      REAL,allocatable       :: qh(:)                        !EFa Nov06 Q(h) an den Eckknoten
      REAL,allocatable       :: dahdh(:)                     !EFa Nov06 Ableitung A(h) nach dh
      REAL,allocatable       :: dqhdh(:)                     !EFa Nov06 Ableitung Q(h) nach dh
      REAL,allocatable       :: d2ahdh(:)                    !EFa Nov06 2. Ableitung A(h) nach dh
      REAL,allocatable       :: d2qhdh(:)                    !EFa Nov06 2. Ableitung Q(h) nach dh
      REAL,ALLOCATABLE       :: kmx(:)                       !EFa Nov06 Kilometrierung f�r 1D-Teschke-Elemente
      REAL,ALLOCATABLE       :: sfnod(:)                     !EFa Nov06 Reibungsgef�lle am Knoten
      REAL,ALLOCATABLE       :: sfwicht(:)                   !EFa Nov06 Wichtungsfaktor f�r Reibungsgef�lle zur Korrektur
      REAL,ALLOCATABLE       :: dsfnoddh(:)                  !EFa Nov06 Ableitung sfnod nach dh
      REAL,ALLOCATABLE       :: dsfnoddq(:)                  !EFa Nov06 Ableitung sfnod nach dq
      REAL                   :: qrandb                       !EFa Dec06 Durchfluss (RB) f�r alle Subroutinen deklarieren in sbgen
      INTEGER                :: beient                       !EFa Nov06 Schalter f�r Beiwert (2: beta, 1: alpha, 0. ignorieren)
      REAL,allocatable       :: hdif(:)                      !EFa Nov06 H�he des �bergangsbereiches entsprechend der Beiwerte
      REAL,ALLOCATABLE       :: pdif(:,:)                    !EFa Nov06 Beiwert-Polynomkoeffizienten-�bergang
      REAL,ALLOCATABLE       :: pbei(:,:)                    !EFa Nov06 Beiwert-Polynomkoeffizienten
      REAL,ALLOCATABLE       :: bei(:)                       !EFa Nov06
      REAL,ALLOCATABLE       :: dbeidh(:)                    !EFa Nov06
      REAL,ALLOCATABLE       :: d2beidh(:)                   !EFa Nov06
      REAL,ALLOCATABLE       :: dbeizdh(:)                   !EFa Nov06
      REAL,ALLOCATABLE       :: d2beizdh(:)                  !EFa Nov06
      REAL,ALLOCATABLE       :: dbeiodh(:)                   !EFa Nov06
      REAL,ALLOCATABLE       :: d2beiodh(:)                  !EFa Nov06
      REAL,ALLOCATABLE       :: bnode(:)                     !EFa Nov06 fiktive Breite an den Knoten
      REAL,ALLOCATABLE       :: hhint(:,:)                   !EFa Nov06
      REAL,ALLOCATABLE       :: dhhintdx(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: dhintdt(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: qqint(:,:)                   !EFa Nov06
      REAL,ALLOCATABLE       :: qqintdx(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: dqintdt(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: areaint(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: dareaintdh(:,:)              !EFa Nov06
      REAL,ALLOCATABLE       :: d2areaintdh(:,:)             !EFa Nov06
      REAL,ALLOCATABLE       :: daintdx(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: d2aintdx(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: d2aidhdx(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: daintdt(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: qschint(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: dqsintdh(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: d2qsidh(:,:)                 !EFa Nov06
      REAL,ALLOCATABLE       :: dqsintdx(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: d2qsidhdx(:,:)               !EFa Nov06
      REAL,ALLOCATABLE       :: s0schint(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: sfwicint(:,:)                !EFa Nov06
      REAL,ALLOCATABLE       :: sfint(:,:)                   !EFa Nov06
      REAL,ALLOCATABLE       :: dsfintdh1(:,:)               !EFa Nov06
      REAL,ALLOCATABLE       :: dhht(:,:)                    !EFa Nov06
      REAL,ALLOCATABLE       :: beiint(:,:)                  !EFa Nov06
      REAL,ALLOCATABLE       :: dbeiintdh(:,:)               !EFa Nov06
      REAL,ALLOCATABLE       :: d2beiintdh(:,:)              !EFa Nov06
      REAL,ALLOCATABLE       :: dbeiintdx(:,:)               !EFa Nov06
      REAL,ALLOCATABLE       :: d2beiintdhdx(:,:)            !EFa Nov06
      REAL,ALLOCATABLE       :: bint(:,:)                    !EFa Nov06 fiktive Breite an den Gausspunkten
      REAL,ALLOCATABLE       :: qqt(:)                       !EFa Nov06 speichern des Durchflusses f�r den n�chsten Zeitschritt
      REAL,ALLOCATABLE       :: dqqt(:,:)                    !EFa Nov06 Zeitableitung des Durchflusses
      REAL,ALLOCATABLE       :: dqdtaltzs(:,:)               !EFa Nov06 speichern der Zeitableitung des Durchflusses f�r den n�chsten Zeitschritt
      REAL                   :: urfc                         !EFa Nov06 Relaxationsfaktor (????) muss schon vor update initialisiert werden
      REAL,ALLOCATABLE       :: froude(:)                    !EFa Nov06 Froudezahl an den Knoten
      REAL,ALLOCATABLE       :: froudeint(:,:)               !EFa Nov06 Froudezahl an den Gausspunken
      REAL,ALLOCATABLE       :: sbot(:)                      !EFa Nov06 Sohlgef�lle
      REAL,ALLOCATABLE       :: vflowint(:,:)                !EFa Dec06, Flie�geschwindigkeit
      REAL,ALLOCATABLE       :: dvintdx(:,:)                 !EFa Dec06, Ableitung der Flie�geschwindigkeit nach dx
      REAL,ALLOCATABLE       :: teschke(:)                   !EFa Dec06, gibt anhand der Fl�chenpolynomdaten an, ob mit 1d-Teschke-Elementen gerechnet wird
      REAL,ALLOCATABLE       :: qhalt(:,:)                   !EFa Dec06, Durchfluss des vorherigen Zeitschrittes
      REAL,ALLOCATABLE       :: dhdtaltzs(:,:)               !EFa Dec06, speichern der Zeitableitung der H�he f�r den n�chsten Zeitschritt
      REAL,ALLOCATABLE       :: hhalt(:,:)                   !EFa Dec06, H�he des vorherigen Zeitschrittes
      REAL,ALLOCATABLE       :: hht(:)                       !EFa Dec06
      REAL,allocatable       :: kennung(:)                   !EFa Jan07, wird in coef1dFE mit 1 belegt; Kennung f�r Konten eines Teschke-Elementes

      END MODULE 