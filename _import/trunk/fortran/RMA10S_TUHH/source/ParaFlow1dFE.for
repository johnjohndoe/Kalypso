C     Last change:  ST   22 Jan 2007   10:50 am
      MODULE PARAFlow1dFE


      real,ALLOCATABLE       :: apoly(:,:)                   !EFa Nov06 Flächenpolynom
      REAL,allocatable       :: hhmin(:),hhmax(:)            !EFa Nov06 min/max h-Werte für Polynomgültigkeit
      REAL,ALLOCATABLE       :: qpoly(:,:)                   !EFa Nov06 Q(h)-Kurve
      REAL,ALLOCATABLE       :: qgef(:)                      !EFa Nov06 Reibungsgefälle für welches Q(h)-Kurve bestimmt wurde
      REAL,ALLOCATABLE       :: hbordv(:)                    !EFa Nov06 h_bordvoll
      REAL,ALLOCATABLE       :: alphah(:)                    !EFa Nov06 Höhe des Übergangsbereiches alpha
      REAL,ALLOCATABLE       :: alphad(:,:)                  !EFa Nov06 alpha-Übergang bis h
      REAL,ALLOCATABLE       :: alphapk(:,:)                 !EFa Nov06 alpha-Polynomkoeffizienten
      REAL,ALLOCATABLE       :: betah(:)                     !EFa Nov06 Höhe des Übergangsbereiches beta
      REAL,ALLOCATABLE       :: betad(:,:)                   !EFa Nov06 beta-Übergang bis h
      REAL,ALLOCATABLE       :: betapk(:,:)                  !EFa Nov06 beta-Polynomkoeffizienten
      REAL,allocatable       :: ah(:)                        !EFa Nov06 A(h) an den Eckknoten
      REAL,allocatable       :: qh(:)                        !EFa Nov06 Q(h) an den Eckknoten
      REAL,allocatable       :: dahdh(:)                     !EFa Nov06 Ableitung A(h) nach dh
      REAL,allocatable       :: dqhdh(:)                     !EFa Nov06 Ableitung Q(h) nach dh
      REAL,allocatable       :: d2ahdh(:)                    !EFa Nov06 2. Ableitung A(h) nach dh
      REAL,allocatable       :: d2qhdh(:)                    !EFa Nov06 2. Ableitung Q(h) nach dh
      REAL,ALLOCATABLE       :: kmx(:)                       !EFa Nov06 Kilometrierung für 1D-Teschke-Elemente
      REAL,ALLOCATABLE       :: sfnod(:)                     !EFa Nov06 Reibungsgefälle am Knoten
      REAL,ALLOCATABLE       :: sfwicht(:)                   !EFa Nov06 Wichtungsfaktor für Reibungsgefälle zur Korrektur
      REAL,ALLOCATABLE       :: dsfnoddh(:)                  !EFa Nov06 Ableitung sfnod nach dh
      REAL,ALLOCATABLE       :: dsfnoddq(:)                  !EFa Nov06 Ableitung sfnod nach dq
      REAL                   :: qrandb                       !EFa Dec06 Durchfluss (RB) für alle Subroutinen deklarieren in sbgen
      INTEGER                :: beient                       !EFa Nov06 Schalter für Beiwert (2: beta, 1: alpha, 0. ignorieren)
      REAL,allocatable       :: hdif(:)                      !EFa Nov06 Höhe des Übergangsbereiches entsprechend der Beiwerte
      REAL,ALLOCATABLE       :: pdif(:,:)                    !EFa Nov06 Beiwert-Polynomkoeffizienten-Übergang
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
      REAL,ALLOCATABLE       :: qqt(:)                       !EFa Nov06 speichern des Durchflusses für den nächsten Zeitschritt
      REAL,ALLOCATABLE       :: dqqt(:,:)                    !EFa Nov06 Zeitableitung des Durchflusses
      REAL,ALLOCATABLE       :: dqdtaltzs(:,:)               !EFa Nov06 speichern der Zeitableitung des Durchflusses für den nächsten Zeitschritt
      REAL                   :: urfc                         !EFa Nov06 Relaxationsfaktor (????) muss schon vor update initialisiert werden
      REAL,ALLOCATABLE       :: froude(:)                    !EFa Nov06 Froudezahl an den Knoten
      REAL,ALLOCATABLE       :: froudeint(:,:)               !EFa Nov06 Froudezahl an den Gausspunken
      REAL,ALLOCATABLE       :: sbot(:)                      !EFa Nov06 Sohlgefälle
      REAL,ALLOCATABLE       :: vflowint(:,:)                !EFa Dec06, Fließgeschwindigkeit
      REAL,ALLOCATABLE       :: dvintdx(:,:)                 !EFa Dec06, Ableitung der Fließgeschwindigkeit nach dx
      REAL,ALLOCATABLE       :: teschke(:)                   !EFa Dec06, gibt anhand der Flächenpolynomdaten an, ob mit 1d-Teschke-Elementen gerechnet wird
      REAL,ALLOCATABLE       :: qhalt(:,:)                   !EFa Dec06, Durchfluss des vorherigen Zeitschrittes
      REAL,ALLOCATABLE       :: dhdtaltzs(:,:)               !EFa Dec06, speichern der Zeitableitung der Höhe für den nächsten Zeitschritt
      REAL,ALLOCATABLE       :: hhalt(:,:)                   !EFa Dec06, Höhe des vorherigen Zeitschrittes
      REAL,ALLOCATABLE       :: hht(:)                       !EFa Dec06
      REAL,allocatable       :: kennung(:)                   !EFa Jan07, wird in coef1dFE mit 1 belegt; Kennung für Konten eines Teschke-Elementes

      END MODULE 