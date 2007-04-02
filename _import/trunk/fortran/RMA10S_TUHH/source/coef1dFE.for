C     Last change:  K     2 Apr 2007    8:14 am
cipk  last update may 23 2006 fix error incorrect reference to NR, should be MAT
cipk  last update mar 07 2006 fix undefined for ice parameters
CIPK  LAST UPDATE SEP 26 2004  ADD MAH AND MAT OPTION
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE MAY 03 2004 ALLOW FOR LOAD APPLIED ONLY AS MASS
CIPK  LAST UPDATE AUG 06 2003 ADD TEST TO REMOVE STRESSES WHEN DRY
cipk  last update jan 13 2002 add momentum to element sources
CIPK  LAST UPDATE OCT  4 2002 ADD ICE FORMULATION
CIPK  LAST UPDATE AUG 28 2002 SET WIND STRESS AND WAVE STRESS TO ZERO BELOW A THRESHOLD DEPTH
CIPK  LAST UPDATE MAY 28 2002 ADD SURFACE STRESS INPUT
cipk  last update mar 26 2001 set sidf zero for dry nodes
CIPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
CIPK  LAST UPDATE DEC 21 2000 Setup for gate structure
cipk  last update Nov 12 1999 allow for collapsing 3-d to 2-d
CIPK  LAST UPDATE APRIL 27 1999 Fix to use mat instead of nr for material type test
cipk  last update Jan 6 1999 initialize AKE correctly
cipk  last update Nov 12 add surface friction
cipk  last update Aug 6 1998 complete division by xht for transport eqn
CIPK  LAST UPDATED NOVEMBER 13 1997
CIPK  LAST UPDATED MAY 1 1996
CIPK LAST UPDATED SEP 7 1995
      SUBROUTINE COEF1dFE(NN,NTX)
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSSTMOD
      USE BLKSANMOD
!NiS,apr06: adding block for DARCY-WEISBACH friction
      USE PARAKalyps
      USE PARAFlow1dFE
!-
      SAVE

!nis,feb07: Some definitions for internal use concerning the equation upsetting
      REAL (KIND=8) :: aint1(1:4), aint2(1:4)
      real (KIND=8) :: daintdh1(1:4), daintdh2(1:4)
      REAL (KIND=8) :: d2aintdh1(1:4), d2aintdh2(1:4)
      REAL (KIND=8) :: qschint1(1:4), qschint2(1:4)
      REAL (KIND=8) :: dqsintdh1(1:4), dqsintdh2(1:4)
      real (KIND=8) :: d2qsidh1(1:4), d2qsidh2(1:4)
      REAL (KIND=8) :: thnn
!-
!nis,feb07: testingvariables
      REAL (KIND=8) :: deltax, deltvx, deltay, deltvy, deltaalpha
      real (KIND=8) :: dirfact

!nis,feb07: local resulting velocities
      REAL(KIND=8) :: vel_res(2)
!-
C
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
      INCLUDE 'BLKE.COM'
      INCLUDE 'BLKH.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSST.COM'
C-
C-
      COMMON F(80),
     1 XN(3),DNX(3),DNY(3),XM(2),DMX(2),DMY(2),XL(3),YL(3),
     4 VX(3),VY(3),VDX(3),VDY(3),QFACT(3),QQFACT(3),ST(3),SDT(3)
     5,UBFC(3),XO(3),DOX(3),Q1(3)
cipk dec00 add Q1
C-
      DIMENSION NCON(20)
CIPK oct02
      COMMON /ICE2/ GSICE,GSQLW,QWLI(8),THKI(8)

C-
cipk sep95 add real*8
      REAL*8 SALT
      DATA FCOEF/14.57/
C
cipk jan99 initialize AKE
!      AKE=1.0

      IF (GRAV .LT. 32.)  THEN
        grav = GRAV
      ELSE
        grav = GRAV/2.208
      ENDIF
C-
C-.....ASSIGN PROPER COEFS.....
C-
C  Test for width > 0



!*******************************************
!nis,mar07,com: Get the nodes of the element
      N1 = NOP(NN,1)
      N3 = NOP(NN,3)
      IF (MOD(NFIX(N1)/100,10) .EQ. 2) THEN
        vel(3,n1) = spec(n1,3)
      ELSEIF (MOD(NFIX(N3)/100,10) .EQ. 2) THEN
        vel(3,n3) = spec(n3,3)
      end if

!****************************************************************
!nis,mar07,com: Check, whether all necessary parameters are given
      !EFa Nov06, Kontrolle der Eingabe Flächenpolynome und Schlüsselkurve
      if (apoly(n1,0).ne.0.or.apoly(n1,1).ne.0.or.apoly(n1,2).ne.0.or.
     +    apoly(n1,3).ne.0.or.apoly(n1,4).ne.0.or.apoly(n1,5).ne.0.or.
     +    apoly(n1,6).ne.0.or.apoly(n1,7).ne.0.or.apoly(n1,8).ne.0.or.
     +    apoly(n1,9).ne.0.or.apoly(n1,10).ne.0.or.apoly(n1,11)
     +    .ne.0.or.apoly(n1,12).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Flächenpolynom fehlt für Knoten ', N1
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Flächenpolynom fehlt für Knoten ', N1
         STOP  '  Flächenpolynom fehlt '
      end if

      if (apoly(n3,0).ne.0.or.apoly(n3,1).ne.0.or.apoly(n3,2).ne.0.or.
     +    apoly(n3,3).ne.0.or.apoly(n3,4).ne.0.or.apoly(n3,5).ne.0.or.
     +    apoly(n3,6).ne.0.or.apoly(n3,7).ne.0.or.apoly(n3,8).ne.0.or.
     +    apoly(n3,9).ne.0.or.apoly(n3,10).ne.0.or.apoly(n3,11)
     +    .ne.0.or.apoly(n3,12).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Flächenpolynom fehlt für Knoten ', N1
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Flächenpolynom fehlt für Knoten ', N1
         STOP  '  Flächenpolynom fehlt '
      end if
C
      if (qpoly(n1,0).ne.0.or.qpoly(n1,1).ne.0.or.qpoly(n1,2).ne.0.or.
     +    qpoly(n1,3).ne.0.or.qpoly(n1,4).ne.0.or.qpoly(n1,5).ne.0.or.
     +    qpoly(n1,6).ne.0.or.qpoly(n1,7).ne.0.or.qpoly(n1,8).ne.0.or.
     +    qpoly(n1,9).ne.0.or.qpoly(n1,8).ne.0.or.qpoly(n1,11)
     +    .ne.0.or.qpoly(n1,12).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Schlüsselkurve fehlt für Knoten ', N3
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Schlüsselkurve fehlt für Knoten ', N3
         STOP  '  Schlüsselkurve fehlt '
      end if
      if (qpoly(n3,0).ne.0.or.qpoly(n3,1).ne.0.or.qpoly(n3,2).ne.0.or.
     +    qpoly(n3,3).ne.0.or.qpoly(n3,4).ne.0.or.qpoly(n3,5).ne.0.or.
     +    qpoly(n3,6).ne.0.or.qpoly(n3,7).ne.0.or.qpoly(n3,8).ne.0.or.
     +    qpoly(n3,9).ne.0.or.qpoly(n3,10).ne.0.or.qpoly(n3,11)
     +    .ne.0.or.qpoly(n3,12).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Schlüsselkurve fehlt für Knoten ', N1
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Schlüsselkurve fehlt für Knoten ', N1
         STOP  '  Schlüsselkurve fehlt '
      end if

!****************************************************************
!nis,mar07,com: Calculate 'area' of element and coordinate things
      TEL=AREA(NN)
      AREA(NN)=0.
cipk nov97
!      TVOL(NN)=0.

      NCN=NCORN(NN)
      DO 63 N=1,NCN
        NCON(N)=NOP(NN,N)
        !kennung(ncon(n))=1
   63 CONTINUE

      IF(NCN .EQ. 5  .AND.  IMAT(NN) .LT. 900) NCN=3
cipk nov97
!      ncnx=2
c
c     Initialize AME and DAME
c
!      IF (IDNOPT.LT.0) THEN
!         DO M = 1, NCNX
!           MC = 2 * M - 1
!           N = NOP(NN,MC)
!           HS = VEL(3,N)
!           ISWT = 0
!           !nis,feb07,testing
!           WRITE(*,*) 'Gelange zum AMF-Aufruf in coef1dFE'
!           pause
!           !-
!           CALL AMF(DUM1,HS,AKP(N),ADT(N),ADB(N),AME(M),DAME(M),ISWT)
!         END DO
!      ENDIF

!***************************
!nis,mar07,com: Initializing
C-
C- INITIALIZE MATRICES AND VARIABLES
C-
      NEF=NCN*NDF
      DO I=1,NEF
        F(I) = 0.0
        DO J=1,NEF
          ESTIFM(I,J) = 0.0
        enddo
      enddo

cipk oct98 update to f90
!      IMMT=IMAT(NN)
!      MR=MOD(IMMT,1000)
cipk dec00 allow gate type elements
!      IF(MR .GT. 900  .AND. IGTP(NN) .EQ. 0) GO TO 2000
!
!      IF(MR .LT. 900) THEN
!
!        MR=MOD(IMMT,100)
!
!      ENDIF
!
!      MAT=MR
cipk dec00 skip out for active gate closure
!      IF(IGTP(NN) .NE. 0) THEN
!        IF(IGTCL(NN) .EQ. 1) THEN
!
!          NGT=IMAT(NN)-900
!          IF(NTX .EQ. 1) THEN
!            AREA(NN)=TEL
!            RETURN
!	    ENDIF
!	  ENDIF
!      ENDIF

      IF(NTX .EQ. 0) THEN
        N1=NCON(1)
        N2=NCON(3)
        !TH(NN) = ATAN2 (CORD(N2,2)-CORD(N1,2), CORD(N2,1) - CORD(N1,1))
        th(nn) = ATAN ((cord(n2,2)-cord(n1,2))/ (cord(n2,1)-cord(n1,1)))
        thnn   = ATAN ((cord(n2,2)-cord(n1,2))/ (cord(n2,1)-cord(n1,1)))
      ENDIF

      CXX=COS(TH(NN))
      SAA=SIN(TH(NN))
      NGP=4
!      NCNX=2
C-
C-.....COMPUTE LOCAL CORDS.....
C-
      n1=ncon(1)
      n3=ncon(3)
      h1=vel(3,n1)
      h3=vel(3,n3)
      NR=NCON(1)

      DO m = 1, 2
      k=2*m-1
      N=NCON(K)
      ANGDIF=TH(NN)-ALFA(N)

      IF(ABS(ANGDIF) .GT. 1.5708  .AND.  ABS(ANGDIF) .LT. 4.71779) THEN
        QFACT(K)=-1.0
        QQFACT(K)=-1.0
      ELSE
        QFACT(K)=1.0
        QQFACT(K)=1.0
      ENDIF

        DX = CORD(N,1) - CORD(NR,1)
        DY = CORD(N,2) - CORD(NR,2)

        XL(K) =  DX*CXX + DY*SAA
        YL(K) = -DX*SAA + DY*CXX

        !nis,feb07: Just using placeholder for resulting velocity
        vel_res(m) = vel(1,n)*COS(alfa(n)) + vel(2,n)*SIN(alfa(n))
        !WRITE(*,*) xl(3), dx, dy, cxx, saa, th(nn), alfa(n)
        !pause
      enddo

      !update for length, if kilometres are given
      if (kmx(n1) /= -1.0 .and. kmx(n3) /= -1.0) then
        xl(3) = xl(k) / ABS(xl(k)) * ABS((kmx(n3)-kmx(n1))*1000)
        yl(3) = 0.0
      end if

      xl(2) = xl(3)/2
      yl(2) = yl(3)/2

      if (vel_res(2) * xl(3) < 0.0) then
        dirfact = -1.0
      else
        dirfact = 1.0
      end if
      !WRITE(*,*) 'dirfact', dirfact, vel_res(2), xl(3), vel_res(2)*xl(3)
      !pause


CIPK JAN03
      EINA=EINX(NN)*CX+EINY(NN)*SA

C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
      TFR=TEL/ABS(XL(3))


CIPK MAY04 RESET ELEMENT INFLOW
      !EFa Nov06, wird eigentlich nicht genutzt
      IF(INOFLOW(NN) .EQ. 0) THEN
        SIDFQ=SIDF(NN)
      ELSE
        SIDFQ=0.
      ENDIF
      SIDFQQ=SIDF(NN)


      !nodes and waterdepths
      n1=ncon(1)
      n3=ncon(3)
      h1=vel(3,n1)
      h3=vel(3,n3)

      !test for valid water depth range
      if (h1.lt.hhmin(n1).and.ntx.eq.1)then
        WRITE(*,*)'Wasserstand an Knoten',n1,'kleiner als Hmin'
      ELSEIF (h1.gt.hhmax(n1).and.ntx.eq.1) then
        WRITE(*,*)'Wasserstand an Knoten',n1,'groesser als Hmax'
      ELSEIF (h3.lt.hhmin(n3).and.ntx.eq.1) then
        WRITE(*,*)'Wasserstand an Knoten',n3,'kleiner als Hmin'
      ELSEIF (h3.gt.hhmax(n3).and.ntx.eq.1) then
        WRITE(*,*)'Wasserstand an Knoten',n3,'groesser als Hmax'
      end if

      !EFa Nov06, Berechnung der Fläche, des Durchflusses, des Reibungsgefälles und der zugehörigen Ableitungen
      hdif(n1) = 0.0
      hdif(n3) = 0.0

      !bedslope
      !sbot(nn) = ABS(ao(n3)-ao(n1))/xl(3)
      !sbot(nn) = (ao(n1)-ao(n3))/xl(3)
      sbot(nn) = (ao(n1) - ao(n3))/
     +           (  (cord(n3,1) - cord(n1,1))*COS(thnn)
     +            + (cord(n3,2) - cord(n1,2))*SIN(thnn))
      !testing
      !WRITE(*,*) (cord(n3,1) - cord(n1,1))*COS(thnn)
      !+            + (cord(n3,2) - cord(n1,2))*SIN(thnn), sbot(nn)
      !pause
      !-

      do k=0,12
        pbei(n1,k) = 0.0
        pbei(n3,k) = 0.0
      end do
      do j=0,3
        pdif(n1,j) = 0.0
        pdif(n3,j) = 0.0
      end do

      !init A(h)
      ah(n1) = 0.0
      ah(n3) = 0.0
      !init QSch(h)
      qh(n1) = 0.0
      qh(n3) = 0.0

      do k=0, 12
        !A(h)
        ah(n1) = ah(n1) + apoly(n1,k) * vel(3,n1)**(k)
        ah(n3) = ah(n3) + apoly(n3,k) * vel(3,n3)**(k)
        !QSch(h)
        qh(n1) = qh(n1) + qpoly(n1,k) * vel(3,n1)**(k)
        qh(n3) = qh(n3) + qpoly(n3,k) * vel(3,n3)**(k)
      end do

      !nis,mar07: Fixing velocity for Q-boundary conditions
      !DO m = 1, 2
      !  k=2*m-1
      !  N=NCON(K)
      !  if (nfix(n)/1000 >= 13) then
      !    vel_res(m) = spec(n,1) / ah(n)
      !    vel(1,n) = vel_res(m) * COS(alfa(n))
      !    vel(2,n) = vel_res(m) * SIN(alfa(n))
      !  end if
      !enddo
      !-


      !Q
      qhalt(n1) = vel_res(1) * ah(n1)
      qhalt(n3) = vel_res(2) * ah(n3)

      !Sf
      sfnod(n1) = sfwicht(n1) * vel_res(1) * ah(n1)
     +          * ABS(vel_res(1) * ah(n1)) / (qh(n1)**2) * qgef(n1)
      sfnod(n3) = sfwicht(n3) * vel_res(2) * ah(n3)
     +          * ABS(vel_res(2) * ah(n3)) / (qh(n3)**2) * qgef(n3)

      if (ntx.eq.1) then
      !EFa Nov06, Testen ob schießen vorliegt
      froude(n1)=qhalt(n1)/(ah(n1)*sqrt(grav*h1))
      froude(n3)=qhalt(n3)/(ah(n3)*sqrt(grav*h3))
      if (froude(n1).gt.1) then
        WRITE(*,*)'Schiessen an Knoten ',n1
      ELSEIF (froude(n3).gt.1) then
        WRITE(*,*)'Schiessen an Knoten ',n3
      end if

      !nis,feb07,com: unsteady
      if (icyc.gt.0) then
        if (maxn.eq.1) then
          dhdtaltzs(nn,1)=dhht(nn,1)
          dhdtaltzs(nn,2)=dhht(nn,2)
          hhalt(nn,1)=vel(3,n1)
          hhalt(nn,2)=vel(3,n3)
          hht(n1)=vel(3,n1)
          hht(n3)=vel(3,n1)
        end if
        dhht(nn,1)=1.6*(vel(3,n1)-hht(n1))/delt+(1-1.6)*
     +           dhdtaltzs(nn,1)
        dhht(nn,2)=1.6*(vel(3,n3)-hht(n3))/delt+(1-1.6)*
     +           dhdtaltzs(nn,2)
      end if

      !nis,feb07,com: unsteady
      if (icyc.gt.0) then
        if (maxn.eq.1) then
          dqdtaltzs(nn,1)=dqqt(nn,1)
          dqdtaltzs(nn,2)=dqqt(nn,2)
          qqt(n1)=qh(n1)
          qqt(n3)=qh(n3)
        end if
        dqqt(nn,1)=1.6*(qhalt(n1)-qqt(n1))/delt+(1-1.6)*
     +             dqdtaltzs(nn,1)
        dqqt(nn,2)=1.6*(qhalt(n3)-qqt(n3))/delt+(1-1.6)*
     +             dqdtaltzs(nn,2)
      end if

      !EFa Nov06, Energiestrombeiwert
      if (beient.eq.1) then       
        do k=0,12
          pbei(n1,k) = alphapk(n1,k)
          pbei(n3,k) = alphapk(n3,k)
        end do
        do j=0,3
          pdif(n1,k) = alphad(n1,k)
          pdif(n3,k) = alphad(n3,k)
        end do
        hdif(n1) = alphah(n1)
        hdif(n3) = alphah(n3)
      endif

      !EFa Nov06, Impulsstrombeiwert
      IF (beient.eq.2) then       
        do k=0,12
          pbei(n1,k)=betapk(n1,k)
          pbei(n3,k)=betapk(n3,k)
        end do
        do j=0,3
          pdif(n1,k)=betad(n1,k)
          pdif(n3,k)=betad(n3,k)
        end do
        hdif(n1)=betah(n1)
        hdif(n3)=betah(n3)
      end if

      !init dA(h)/dh
      dahdh(n1) = 0.0
      dahdh(n3) = 0.0
      !init dQSch(h)/dh
      dqhdh(n1) = 0.0
      dqhdh(n3) = 0.0

      do k=1,12
        !dA(h)/dh
        dahdh(n1) = dahdh(n1) + (k) * apoly(n1,k) * h1**(k-1)
        dahdh(n3) = dahdh(n3) + (k) * apoly(n3,k) * h3**(k-1)
        !dQSch(h)/dh
        dqhdh(n1) = dqhdh(n1) + (k) * qpoly(n1,k) * h1**(k-1)
        dqhdh(n3) = dqhdh(n3) + (k) * qpoly(n3,k) * h3**(k-1)
      end do

      !init d2A(h)/dh2
      d2ahdh(n1) = 0.0
      d2ahdh(n3) = 0.0
      !init d2QSch(h)/dh2
      d2qhdh(n1) = 0.0
      d2qhdh(n3) = 0.0
      do k=2,12
        !d2A(h)/dh2
        d2ahdh(n1) = d2ahdh(n1) + (k-1) * (k) * apoly(n1,k)*h1**(k-2)
        d2ahdh(n3) = d2ahdh(n3) + (k-1) * (k) * apoly(n3,k)*h3**(k-2)
        !d2QSch(h)/dh2
        d2qhdh(n1) = d2qhdh(n1) + (k-1) * (k) * qpoly(n1,k)*h1**(k-2)
        d2qhdh(n3) = d2qhdh(n3) + (k-1) * (k) * qpoly(n3,k)*h3**(k-2)
      end do

      !dSf/dh
      !dsfnoddh(n1) = -2.0 * (sfnod(n1) / qh(n1)) * dqhdh(n1)
      !dsfnoddh(n3) = -2.0 * (sfnod(n3) / qh(n3)) * dqhdh(n3)
      dsfnoddh(n1) = qgef(n1) * vel_res(1)**2 * (qh(n1)**2 * 2 * ah(n1)
     +               * dahdh(n1) - ah(n1)**2 * 2 * qh(n1) * dqhdh(n1))
     +               / qh(n1)**4
      dsfnoddh(n3) = qgef(n3) * vel_res(2)**2 * (qh(n3)**2 * 2 * ah(n3)
     +               * dahdh(n3) - ah(n3)**2 * 2 * qh(n1) * dqhdh(n3))
     +               / qh(n1)**4

      !dSf/dQ
      !dsfnoddq(n1) = 2.0 * sfnod(n1) / qhalt(n1)
      !dsfnoddq(n3) = 2.0 * sfnod(n3) / qhalt(n3)


      !EFa Nov06, Bereich für Wasserspiegel Bordvoll (nur Hauptgerinne)
      if (h1.le.hbordv(n1)) then
        bei(n1)      = 1.0
        dbeidh(n1)   = 0.0
        d2beidh(n1)  = 0.0
        pbei(n1,1)   = 1.0
        do k = 0, 12
          pbei(n1,k) = 0.0
        end do
      end if

      if (h3.le.hbordv(n3)) then
        bei(n3)      = 1.0
        dbeidh(n3)   = 0.0
        d2beidh(n3)  = 0.0
        pbei(n3,1)   = 1.0
        do k = 0, 12
          pbei(n3,k) = 0.0
        end do
      end if

      !EFa Nov06, Bereich für Höhen zwischen dem unteren Grenzwert als Wasserspiegel Bordvoll
      !           und dem oberen Grenzwert des Übergangspolynoms
      if (h1.gt.hbordv(n1).and.h1.lt.hdif(n1)) then
        bei(n1)      = 0.0
        do k = 0, 3
          bei(n1)    = bei(n1) + pdif(n1,k) * h1**(k)
          pbei(n1,k) = pdif(n1,k)
        end do
        do k = 4, 12
          pbei(n1,k) = 0.0
        end do
        !1st derivative
        dbeizdh(n1)  = 0.0
        do k = 1, 3
          dbeizdh(n1) = dbeizdh(n1) + (k) * pdif(n1,k) * h1**(k-1)
        end do
        !2nd derivative
        d2beizdh(n1)=0
        do k = 2, 3
          d2beizdh(n1) = d2beizdh(n1) + (k-1)*(k) * pdif(n1,k)*h1**(k-2)
        end do
        dbeidh(n1)  = dbeizdh(n1)
        d2beidh(n1) = d2beizdh(n1)
      end if

      if (h3.gt.hbordv(n3).and.h3.lt.hdif(n3)) then
        bei(n3)=0
        do k = 0, 3
          bei(n3)    = bei(n3) + pdif(n3,k) * h3**(k)
          pbei(n3,k) = pdif(n3,k)
        end do
        do k = 4, 12
          pbei(n3,k)=0
        end do
        !1st derivative
        dbeizdh(n3)=0
        do k = 1, 3
          dbeizdh(n3)  = dbeizdh(n3) + (k) * pdif(n3,k) * h3**(k-1)
        end do
        !2nd derivative
        d2beizdh(n3)=0
        do k = 2, 3
          d2beizdh(n3) = d2beizdh(n3) + (k-1)*(k) * pdif(n3,k)*h3**(k-2)
        end do
        dbeidh(n3)=dbeizdh(n3)
        d2beidh(n3)=d2beizdh(n3)
      end if

      !EFa Nov06, Bereich für Höhen über dem oberen Grenzwert des Übergangspolynoms
      if (h1.ge.hdif(n1)) then
        bei(n1)=0
        do k = 0, 12
          bei(n1) = bei(n1) + pbei(n1,k) * h1**(k)
        end do
        !1st derivative
        dbeiodh(n1)=0
        do k = 1, 12
          dbeiodh(n1) = dbeiodh(n1) + (k) * pbei(n1,k) * h1**(k-1)
        end do
        !2nd derivative
        d2beiodh(n1)=0
        do k = 2, 12
          d2beiodh(n1) = d2beiodh(n1) + (k-1)*(k) * pbei(n1,k)*h1**(k-2)
        end do
        dbeidh(n1)=dbeiodh(n1)
        d2beidh(n1)=dbeiodh(n1)
      end if

      if (h3.ge.hdif(n3)) then
        bei(n3)=0
        do k = 0, 12
          bei(n3) = bei(n3) + pbei(n3,k) * h3**(k)
        end do
        !1st derivative
        dbeiodh(n3)=0
        do k = 1, 12
          dbeiodh(n3) = dbeiodh(n3) + (k) * pbei(n3,k) * h3**(k-1)
        end do
        !2nd derivative
        d2beiodh(n3)=0
        do k = 2, 12
          d2beiodh(n3) = d2beiodh(n3) + (k-1)*(k) * pbei(n3,k)*h3**(k-2)
        end do
        dbeidh(n3)=dbeiodh(n3)
        d2beidh(n3)=dbeiodh(n3)
      end if

      !EFa Nov06, wenn ohne Beiwerte gerechnet werden soll oder die Eingabe von beient fehlerhaft ist
      if ((beient .eq. 0) .OR. (beient.ne.2).AND.(beient.ne.1)) then
        bei(n1)      = 1.0
        dbeidh(n1)   = 0.0
        d2beidh(n1)  = 0.0
        pbei(n1,1)   = 1.0
        do k = 1, 12
          pbei(n1,k) = 0.0
        end do
        pdif(n1,1)   = 1.0
        do k = 1, 3
          pdif(n1,k) = 0.0
        end do
        bei(n3)      = 1.0
        dbeidh(n3)   = 0.0
        d2beidh(n3)  = 0.0
        pbei(n1,3)   = 1.0
        do k = 1, 12
          pbei(n3,k) = 0.0
        end do
        pdif(n3,1)   = 1.0
        do k = 1, 3
          pdif(n3,k) = 0.0
        end do
      endif

      !EFa Nov06, fiktive Breite
      bnode(n1) = ah(n1)/ h1
      bnode(n3) = ah(n3)/ h3

      ENDIF !ntx=1

      !nis,feb07,com: Start of loop over Gauss-nodes
      Gaussloop: DO I = 1, NGP

      TEMP=(DNAL(2,I)*XL(2)+DNAL(3,I)*XL(3))

C-
C......DEFINE SHAPE FUNCTIONS
C-
      XN(1)=(1.-AFACT(I))*(1.-2.*AFACT(I))
      XN(2)=(1.-AFACT(I))*4.*AFACT(I)
      XN(3)=(2.*AFACT(I)-1.)*AFACT(I)
      DNX(1)=(4.*AFACT(I)-3.)/TEMP
      DNX(2)=(4.-8.*AFACT(I))/TEMP
      DNX(3)=(4.*AFACT(I)-1.)/TEMP

      IF(NTX .EQ. 0) THEN
        DYDX=YL(2)*DNX(2)+YL(3)*DNX(3)
        ALF=ATAN(DYDX)
        CSALF=COS(ALF)
        TEMP=TEMP/CSALF
      ELSE
        TEMP=TEMP*TFR
      ENDIF
      IF(NTX .NE. 0) THEN
        DO 240 J=1,3
          DNX(J)=DNX(J)/TFR
  240   CONTINUE
      ENDIF
      XM(1)  = 1.0 - AFACT(I)
      XM(2)  = AFACT(I)
      DMX(1) = -1.0 / TEMP
      DMX(2) = 1. / TEMP

      IF(NTX .NE. 0) THEN
        H=VEL(3,N1)*XM(1)+VEL(3,N2)*XM(2)
      ELSE
        H=1.0
      ENDIF
      AMW=ABS(TEMP)*HFACT(I)/2.
      AREA(NN)=AREA(NN)+AMW
      !nis,mar07,testing
      !WRITE(*,*) 'GP: ', I
      !WRITE(*,*) xm(1), xm(2), dmx(1), dmx(2)
      !WRITE(*,*) afact(i)
      !WRITE(*,*) dydx, alf, csalf
      !WRITE(*,*) 'area(nn), amw, hfact(i), temp'
      !WRITE(*,*) area(nn), amw, hfact(i), temp
      !if (NTX == 1) then
      !  pause
      !endif
      !-


      IF(NTX .EQ. 0) CYCLE gaussloop
cipk nov97
      IF (NTX .EQ. 3) GO TO 276


      !EFa Nov06, hoehelem(nn)
      hhint(nn,i)    = h1    * xm(1)      + h3    * xm(2)
      dhhintdx(nn,i) = h1    * dmx(1)     + h3    * dmx(2)
      dhintdt(nn,i)  = xm(1) * dhht(nn,1) + xm(2) * dhht(nn,2)

      !EFa Dec06, Fließgeschwindigkeit
      vflowint(nn,i) = xm(1) * vel_res(1) + xm(2) * vel_res(2)
      dvintdx(nn,i)  = dmx(1) * vel_res(1) + dmx(2) * vel_res(2)

      !EFa Nov06, areaelem(nn)

      !nis,feb07: Formulation of derivatives over h within the element were wrong
      !areaint(nn,i)     = ah(n1) * xm(1) + ah(n3) * xm(2)
      areaint(nn,i) = 0.0
      aint1(i)      = 0.0
      aint2(i)      = 0.0
      do j = 0, 12
        aint1(i) = aint1(i) + apoly(n1, j) * hhint(nn,i)**(j)
        aint2(i) = aint2(i) + apoly(n3, j) * hhint(nn,i)**(j)
      enddo
      areaint(nn,i) = xm(1) * aint1(i) + xm(2) * aint2(i)

      !dareaintdh(nn,i)  = dahdh(n1) * xm(1) + dahdh(n3) * xm(2)
      !Equation 8.28, page 65
      dareaintdh(nn,i) = 0.0
      daintdh1(i) = 0.0
      daintdh2(i) = 0.0
      do j = 1, 12
       daintdh1(i) = daintdh1(i)
     +                + (j) * apoly(n1,j) * hhint(nn,i)**(j-1)
       daintdh2(i) = daintdh2(i)
     +                + (j) * apoly(n3,j) * hhint(nn,i)**(j-1)
      end do
        dareaintdh(nn,i) =
     +                  xm(1) * daintdh1(i) +  xm(2) * daintdh2(i)

      !d2areaintdh(nn,i) = d2ahdh(n1) * xm(1) + d2ahdh(n3) * xm(2)
      d2areaintdh(nn,i) = 0.0
      d2aintdh1(i) = 0.0
      d2aintdh2(i) = 0.0
      do j = 2, 12
        d2aintdh1(i) = d2aintdh1(i)
     +         + (j-1) * (j) * apoly(n1, j) * hhint(nn,i)**(j-2)
        d2aintdh2(i) = d2aintdh2(i)
     +         + (j-1) * (j) * apoly(n3, j) * hhint(nn,i)**(j-2)
      end do
        d2areaintdh(nn,i) =
     +     xm(1) * d2aintdh1(i) +  xm(2) * d2aintdh2(i)
      !-

      !nis,feb07: Also the other formulations are different
      daintdx(nn,i) = dmx(1) * aint1(i)
     +              + xm(1) * daintdh1(i) * dhhintdx(nn,i)
     +              + dmx(2) * aint2(i)
     +              + xm(2) * daintdh2(i) * dhhintdx(nn,i)
      !daintdx(nn,i)     = dmx(1) * ah(n1) + xm(1) * dahdh(n1)
      !+                  * dhhintdx(nn,i) + dmx(2) * ah(n3)
      !+                  + xm(2) * dahdh(n3) * dhhintdx(nn,i)

!      daintdx(nn,i)     = dmx(1) * (ah(n1) + h1 * ( xm(1) * dahdh(n1)
!     +                  + xm(2) * dahdh(n3)))
!     +                  + dmx(2) * (ah(n3) + h3 * ( xm(1) * dahdh(n1)
!     +                  + xm(2) * dahdh(n3)))

      d2aintdx(nn,i) = dhhintdx(nn,i) *
     +                 (2.0 * dmx(1) * daintdh1(i)
     +               +  xm(1) * d2aintdh1(i) * dhhintdx(nn,i)
     +               +  2.0 * dmx(2) * daintdh2(i)
     +               +  xm(2) * d2aintdh2(i) * dhhintdx(nn,i))
      !d2aintdx(nn,i)    = dhhintdx(nn,i) * (2*dmx(1) * dahdh(n1)
      !+                  + xm(1) * d2ahdh(n1) * dhhintdx(nn,i)
      !+                  + 2 * dmx(2) * dahdh(n3)
      !+                  + xm(2) * d2ahdh(n3) * dhhintdx(nn,i))

      d2aidhdx(nn,i) = dmx(1) *  daintdh1(i)
     +               +  xm(1) * d2aintdh1(i) * dhhintdx(nn,i)
     +               + dmx(2) *  daintdh2(i)
     +               +  xm(2) * d2aintdh2(i) * dhhintdx(nn,i)
      !d2aidhdx(nn,i)    = dmx(1) * dahdh(n1) + xm(1) * d2ahdh(n1)
      !+                  * dhhintdx(nn,i) + dmx(2) * dahdh(n3)
      !+                  + xm(2)*d2ahdh(n3) * dhhintdx(nn,i)

      daintdt(nn,i)     = (xm(1) * dahdh(n1) + xm(2) * dahdh(n3))
     +                  * dhintdt(nn,i)

      !EFa Nov06, flowelem(nn)
      qqint(nn,i) = vflowint(nn,i) * areaint(nn,i)
!      qqintdx(nn,i) = qhalt(n1)  * dmx(1) + qhalt(n3)  * dmx(2)
      dqintdt(nn,i) = dqqt(nn,1) * xm(1)  + dqqt(nn,2) * xm(2)

      !EFa Nov06, Testen ob Schießen im Element vorliegt
      froudeint(nn,i)=qqint(nn,i)/(areaint(nn,i)*sqrt(grav*hhint(nn,i)))
      if (froudeint(nn,i).gt.1) then
        WRITE(*,*)'Schiessen in Element', nn
      end if

      !EFa Nov06, schkelem(nn)
      !nis,feb07: Formulation of derivatives over h within the element were wrong
      !qschint(nn,i)   = xm(1) * qh(n1) + xm(2) * qh(n3)
      qschint(nn,i) = 0.0
      qschint1(i) = 0.0
      qschint2(i) = 0.0
      do j = 0, 12
        qschint1(i) = qschint1(i)
     +              + qpoly(n1,j) * hhint(nn,i)**(j)
        qschint2(i) = qschint2(i)
     +              + qpoly(n3,j) * hhint(nn,i)**(j)
      end do
      qschint(nn,i) = xm(1) * qschint1(i) + xm(2) * qschint2(i)

      !dqsintdh(nn,i)  = dqhdh(n1) * xm(1) + dqhdh(n3) * xm(2)
      dqsintdh(nn,i) = 0.0
      dqsintdh1(i)   = 0.0
      dqsintdh2(i)   = 0.0
      do j = 1, 12
        dqsintdh1(i) = dqsintdh1(i)
     +               + (j) * qpoly(n1,j) * hhint(nn,i)**(j-1)
        dqsintdh2(i) = dqsintdh2(i)
     +               + (j) * qpoly(n3,j) * hhint(nn,i)**(j-1)
      end do
      dqsintdh(nn,i) = xm(1) * dqsintdh1(i) + xm(2) * dqsintdh2(i)

      !d2qsidh(nn,i)   = d2qhdh(n1) * xm(1) + d2qhdh(n3) * xm(2)
      d2qsidh(nn,i) = 0.0
      d2qsidh1(i) = 0.0
      d2qsidh2(i) = 0.0
      do j = 2, 12
      d2qsidh1(i) = d2qsidh1(i)
     +            + (j-1) * (j) * qpoly(n1,j) * hhint(nn,i)**(j-2)
      d2qsidh2(i) = d2qsidh2(i)
     +            + (j-1) * (j) * qpoly(n3,j) * hhint(nn,i)**(j-2)
      end do
      d2qsidh(nn,i) = xm(1) * d2qsidh1(i) + xm(2) * d2qsidh2(i)

      !dqsintdx(nn,i)  =
      !+      dmx(1) * qh(n1) + xm(1) * dqhdh(n1) * dhhintdx(nn,i)
      !+    + dmx(2) * qh(n3) + xm(2) * dqhdh(n3) * dhhintdx(nn,i)
      dqsintdx(nn,i) = dmx(1) * qschint1(i)
     +               + xm(1) * dqsintdh1(i) * dhhintdx(nn,i)
     +               + dmx(2) * qschint2(i)
     +               + xm(2) * dqsintdh2(i) * dhhintdx(nn,i)

      !d2qsidhdx(nn,i) =
      !+      dmx(1) * dqhdh(n1) + xm(1)*d2qhdh(n1) * dhhintdx(nn,i)
      !+    + dmx(2) * dqhdh(n3) + xm(2)*d2qhdh(n3) * dhhintdx(nn,i)
      d2qsidhdx(nn,i) = dmx(1) * dqsintdh1(i)
     +                + xm(1) * d2qsidh1(i) * dhhintdx(nn,i)
     +                + dmx(2) * dqsintdh2(i)
     +                + xm(2) * d2qsidh2(i) * dhhintdx(nn,i)

      !EFa Nov06, fricelem(nn)
      s0schint(nn,i)=qgef(n1)*xm(1)+qgef(n3)*xm(2)
      sfwicint(nn,i)=sfwicht(n1)*xm(1)+sfwicht(n3)*xm(2)

      !nis,feb07: Formulation of the friction slope is not correct here
      !sfint(nn,i)=sfnod(n1)*xm(1)+sfnod(n3)*xm(2)
!      sfint(nn,i) = sfwicint(nn,i) * s0schint(nn,i) * qqint(nn,i)
!     +            * ABS(qqint(nn,i)) / qschint(nn,i)**2
      sfint(nn,i) = sfwicint(nn,i) * s0schint(nn,i) * vflowint(nn,i)
     +            * ABS(vflowint(nn,i)) * areaint(nn,i)**2
     +            / qschint(nn,i)**2
      !-

!      dsfintdh1(nn,i) = -2.0
!     +               * (sfint(nn,i) / qschint(nn,i)**2) * dqsintdh(nn,i)
      dsfintdh1(nn,i) = s0schint(nn,i) * vflowint(nn,i)**2
     +               * (QSchint(nn,i)**2 * 2 * areaint(nn,i)
     +                  * dareaintdh(nn,i) - areaint(nn,i)**2 * 2
     +                  * Qschint(nn,i) * dqsintdh(nn,i))
     +               / Qschint(nn,i)**4

      !EFa Nov06, beiwelem(nn)
      beiint(nn,i)=xm(1)*bei(n1)+xm(2)*bei(n3)
      !WRITE(*,*) bei(n1), bei(n3), beiint(nn,i), beient
      dbeiintdh(nn,i)=xm(1)*dbeiodh(n1)+xm(2)*dbeiodh(n3)
      d2beiintdh(nn,i)=xm(1)*d2beiodh(n1)+xm(2)*d2beiodh(n3)
      dbeiintdx(nn,i)=dmx(1)*bei(n1)+xm(1)*dbeiodh(n1)*dhhintdx(nn,i)+
     +                dmx(2)*bei(n3)+xm(2)*dbeiodh(n3)*dhhintdx(nn,i)
      d2beiintdhdx(nn,i)=dmx(1)*dbeiodh(n1)+xm(1)*d2beiodh(n1)*
     +                   dhhintdx(nn,i)+dmx(2)*dbeiodh(n3)+xm(2)*
     +                   d2beiodh(n3)*dhhintdx(nn,i)

      !EFa Nov06, fiktive Breite
      bint(nn,i) = areaint(nn,i) / hhint(nn,i)
      !-

      !****************
      !set up equations
      !****************

      do l = 1, 2
      !EFa Mar07, residual vector changed for steady state solution
      ia = 1 + (2 * ndf) * (l-1)

      f(ia)=f(ia)
           !Wichtungsfunktion
     +     -xm(l)*hfact(i)*ABS(xl(3))/2*
!     +     -xm(l)*hfact(i)*xl(3)/2*
           !Term a
     +     (dqintdt(nn,i)
           !Term b
     +     + vflowint(nn,i)**2 * areaint(nn,i) * dbeiintdx(nn,i)
           !Term c
     +     + beiint(nn,i) * vflowint(nn,i)**2 * daintdx(nn,i)
           !Term d
     +     + 2 * beiint(nn,i) * vflowint(nn,i) * areaint(nn,i)
     +       * dvintdx(nn,i)
           !Term e
     +     + grav * areaint(nn,i) * dhhintdx(nn,i)
           !Term f
     +     + grav * areaint(nn,i) * sfint(nn,i)
           !Term g
     +     - grav * areaint(nn,i) * sbot(nn))

      !nis,feb07,testing
      if (nn < -3) then
        WRITE(*,*) 'Impulsintegral', f(3), f(11)
       WRITE(*,*) 'Einzelterme Gauß: ', i
        WRITE(*,*) 'Term A: ', dqintdt(nn,i)
        WRITE(*,*) 'Term B: ',
     +   + vflowint(nn,i)**2 * areaint(nn,i) * dbeiintdx(nn,i)
     +    * xm(l)*hfact(i)*ABS(xl(3))/2
        WRITE(*,*) 'Term C: ',
     +   + beiint(nn,i) * vflowint(nn,i)**2 * daintdx(nn,i)
     +    * xm(l)*hfact(i)*ABS(xl(3))/2
        WRITE(*,*) 'Term D: ',
     +   + 2 * beiint(nn,i) * vflowint(nn,i) * areaint(nn,i)
     +     * dvintdx(nn,i)
     +    * xm(l)*hfact(i)*ABS(xl(3))/2
        WRITE(*,*) 'Term E: ',
     +   + grav * areaint(nn,i) * dhhintdx(nn,i)
     +    * xm(l)*hfact(i)*ABS(xl(3))/2
       WRITE(*,*) 'Term F: ',
     +   + grav * areaint(nn,i) * sfint(nn,i)
     +    * xm(l)*hfact(i)*ABS(xl(3))/2
        WRITE(*,*) 'Term G: ',
     +   - grav * areaint(nn,i) * sbot(nn)
     +    * xm(l)*hfact(i)*ABS(xl(3))/2
      !pause
      endif
      !-
      enddo

      !EFa Nov06, Kontinuitätsgleichung
      !EFa Mar07, residuen vector changed for steady state solution
      !if (icyc.gt.0) then
      do l = 1, 2

      ia = 3 + (2 * ndf) * (l - 1)

      f(ia)=f(ia)
            !Wichtungsfunktion
     +      - xm(l)*hfact(i)*ABS(xl(3))/2*
!     +      - xm(l)*hfact(i)*xl(3)/2*
            !Term A
     +      (daintdt(nn,i)
            !Term B
     +       + areaint(nn,i) * dvintdx(nn,i)
            !Term C
     +       + vflowint(nn,i) * daintdx(nn,i))

      enddo
      !-

      !EFa Nov06, Sprung an den Beginn der Gauss-Schleife für junction-Elemente
      !IF(MOD(immt,100).gt.90) GO TO 500

      !EFa Nov06, derivative: MOMENTUM over DEPTH
      !nis,mar07: Reducing the error possibilities by using a loop
      do l = 1, 2
        do c = 1, 2

        ia = 1 + (2 * ndf) * (l - 1)
        ib = 3 + (2 * ndf) * (c - 1)

      estifm(ia,ib) = estifm(ia,ib)
                  !Wichtungsfunktion
     +            + xm(l)*hfact(i)*ABS(xl(3))/2*
!     +            + xm(l)*hfact(i)*xl(3)/2*
                  !Term B
!     +              (xm(c) * vflowint(nn,i)**2 *
!     +               (areaint(nn,i)*d2beiintdhdx(nn,i)
!     +                + dareaintdh(nn,i) * dbeiintdx(nn,i))
     +              (vflowint(nn,i)**2 *
     +               (dareaintdh(nn,i) * dbeiintdh(nn,i)*xm(c)
     +                + areaint(nn,i) *
     +                  (d2beiintdhdx(nn,i) * xm(c)
     +                   + dbeiintdh(nn,i) * dmx(c)))
                  !Term C
!     +            + xm(c) * vflowint(nn,i)**2 *
!     +              (dbeiintdh(nn,i) * daintdx(nn,i)
!     +               + beiint(nn,i) * d2aidhdx(nn,i))
     +            + vflowint(nn,i)**2 *
     +              (dbeiintdh(nn,i) * daintdx(nn,i) * xm(c)
     +               + beiint(nn,i) *
     +                 (d2aidhdx(nn,i) * xm(c)
     +                  + dareaintdh(nn,i) * dmx(c)))
                  !Term D
     +            + xm(c) * 2 * vflowint(nn,i) * dvintdx(nn,i) *
     +              (beiint(nn,i) * dareaintdh(nn,i)
     +               + areaint(nn,i) * dbeiintdh(nn,i))
                  !Term E
     +            + grav*
     +              (xm(c) * dareaintdh(nn,i) * dhhintdx(nn,i)
     +               + areaint(nn,i) * dmx(c))
                  !Term F
!     +            + grav * xm(c) *
!    +              (areaint(nn,i)
!     +               * (s0schint(nn,i) * vflowint(nn,i)
!     +                  * ABS(vflowint(nn,i))
!     +                  / Qschint(nn,i)**4)
!     +               * (QSchint(nn,i)**2 * 2 * areaint(nn,i)
!     +                  * dareaintdh(nn,i) - areaint(nn,i)**2 * 2
!     +                  * Qschint(nn,i) * dqsintdh(nn,i))
!     +               + sfint(nn,i) * dareaintdh(nn,i))
     +            + grav * xm(c) *
     +              (areaint(nn,i) * dsfintdh1(nn,i)
     +               + sfint(nn,i) * dareaintdh(nn,i))
                  !Term G
     +            - grav * xm(c) * sbot(nn) * dareaintdh(nn,i))

      if (nn < -3) then
      WRITE(*,*) '*****************************'
      WRITE(*,*) '*****************************'
      WRITE(*,*) 'dF/dh'
      WRITE(*,*) estifm(ia,ib), ia, ib, i
      WRITE(*,*) xm(l) * hfact(i)*ABS(xl(3))/2,
     +           xm(l), hfact(i), ABS(xl(3))/2, i
!      WRITE(*,*) xm(l) * hfact(i)*xl(3)/2,
!     +           xm(l), hfact(i), xl(3)/2, i
      WRITE(*,*) '*****************************'
      !Term B
      WRITE(*,*) 'Term B', i,
!     +            + xm(c) * vflowint(nn,i)**2 *
!     +               (areaint(nn,i)*d2beiintdhdx(nn,i)
!     +                + dareaintdh(nn,i) * dbeiintdx(nn,i))
     +              vflowint(nn,i)**2 *
     +               (dareaintdh(nn,i) * dbeiintdh(nn,i)*xm(c)
     +                + areaint(nn,i) *
     +                  (d2beiintdhdx(nn,i) * xm(c)
     +                   + dbeiintdh(nn,i) * dmx(c)))
      !Term C
      WRITE(*,*) 'Term C', i,
!     +            + xm(c) * 2 * vflowint(nn,i)**2 *
!     +              (dbeiintdh(nn,i) * daintdx(nn,i)
!     +               + beiint(nn,i) * d2aidhdx(nn,i))
     +            + vflowint(nn,i)**2 *
     +              (dbeiintdh(nn,i) * daintdx(nn,i) * xm(c)
     +               + beiint(nn,i) *
     +                 (d2aidhdx(nn,i) * xm(c)
     +                  + dareaintdh(nn,i) * dmx(c)))
      !Term D
      WRITE(*,*) 'Term D', i,
     +            + xm(c) * 2 * vflowint(nn,i) * dvintdx(nn,i) *
     +              (beiint(nn,i) * dareaintdh(nn,i)
     +               + areaint(nn,i) * dbeiintdh(nn,i))
      !Term E
      WRITE(*,*) 'Term E', i,
     +            + grav*
     +              (xm(c) * dareaintdh(nn,i) * dhhintdx(nn,i)
     +               + areaint(nn,i) * dmx(c))
      !Term F
      WRITE(*,*) 'Term F', i,
     +            + grav * xm(c) *
     +              (areaint(nn,i) * (s0schint(nn,i) * vflowint(nn,i)**2
     +               * (QSchint(nn,i)**2 * 2 * areaint(nn,i)
     +                  * dareaintdh(nn,i) - areaint(nn,i)**2 * 2
     +                  * Qschint(nn,i) * dqsintdh(nn,i))
     +               / Qschint(nn,i)**4)
     +               + sfint(nn,i) * dareaintdh(nn,i))
      !Term G
      WRITE(*,*) 'Term G', i,
     +            - grav * xm(c) * sbot(nn) * dareaintdh(nn,i)
      endif


        end do
      end do
      !-



      !EFa Nov06, derivative: MOMENTUM over VELOCITY
      do l = 1, 2
        do c = 1, 2

      ia = 1 + (2 * ndf) * (l - 1)
      ib = 1 + (2 * ndf) * (c - 1)                                       !df1/dv1

      estifm(ia,ib) = estifm(ia,ib)
            !Wichtungsfunktion
     +      + xm(l)*hfact(i)*ABS(xl(3))/2*
!     +      + xm(l)*hfact(i)*xl(3)/2*
            !Term B
     +      (xm(c) * 2 * areaint(nn,i)* vflowint(nn,i) * dbeiintdx(nn,i)
            !Term C
     +       + xm(c) * 2 * vflowint(nn,i) * beiint(nn,i) * daintdx(nn,i)
            !Term D
     +      + 2 * beiint(nn,i) * areaint(nn,i)
     +        * (xm(c) * dvintdx(nn,i) + vflowint(nn,i) * dmx(c))
            !Term F
     +      + xm(c) * 2 * grav * areaint(nn,i) * sfint(nn,i)
     +        / vflowint(nn,i))
      !add time term for unsteady case
      if (icyc.gt.0) then
        estifm(ia,ib)=estifm(ia,ib)+xm(l)*hfact(i)*ABS(xl(3))/2*        !Wichtungsfunktion
!        estifm(ia,ib)=estifm(ia,ib)+xm(l)*hfact(i)*xl(3)/2*
     +                (xm(c)*areaint(nn,i)*1.6/delt)                     !Term a
      end if

      if (nn < -3) then
      WRITE(*,*) '*****************************'
      WRITE(*,*) '*****************************'
      WRITE(*,*) 'dF/ dv'
      WRITE(*,*) estifm(ia,ib), ia, ib, i
      WRITE(*,*) xm(l) * hfact(i)*ABS(xl(3))/2, xm(l), hfact(i), i
!      WRITE(*,*) xm(l) * hfact(i)*xl(3)/2, xm(l), hfact(i), i
      WRITE(*,*) '*****************************'
      !Term B
      WRITE(*,*) 'Term B', i, xm(c) * 2 * areaint(nn,i) *
     +           vflowint(nn,i) * dbeiintdx(nn,i)
      !Term C
      WRITE(*,*) 'Term C', i, xm(c) * 2 * vflowint(nn,i) *
     +           beiint(nn,i) * daintdx(nn,i)
      !Term D
      WRITE(*,*) 'Term D', i, + 2 * beiint(nn,i) * areaint(nn,i)
     +           * (xm(c) * dvintdx(nn,i) + vflowint(nn,i) * dmx(c))
      !Term F
      WRITE(*,*) 'Term F', i,
     +           xm(c) * 2 * grav * areaint(nn,i) * sfint(nn,i)
     +           / vflowint(nn,i)
      endif

      !-

        end do
      end do
      !-


      !EFa Nov06, derivative: CONTINUITY over DEPTH
      do l = 1, 2
        do c = 1, 2
          ia = 3 + (2 * ndf) * (l - 1)
          ib = 3 + (2 * ndf) * (c - 1)

          estifm(ia,ib) = estifm(ia,ib)
                        !Wichtungsfunktion
     +                  + xm(l) * hfact(i) * ABS(xl(3))/2*
!     +                  + xm(l) * hfact(i) * xl(3)/2*
                        !Term C
     +                  ( xm(c) * dareaintdh(nn,i) * dvintdx(nn,i)
                        !Term B
!     +                  + xm(c) * vflowint(nn,i) * d2aidhdx(nn,i))
     +                  + vflowint(nn,i) *
     +                    (xm(c) * d2aidhdx(nn,i)
     +                     + dareaintdh(nn,i) * dmx(c)))
          !unsteady term
          if (icyc > 0) then
            estifm(ia,ib) = estifm(ia,ib)
                        !Wichtungsfunktion
     +                  + xm(l) * hfact(i) * ABS(xl(3))/2*
!     +                  + xm(l) * hfact(i) * xl(3)/2*
                        !Term A
     +                  xm(c) *
     +                  (d2areaintdh(nn,i) * dhintdt(nn,i)
     +                   + dareaintdh(nn,i) * 1.6/delt)
          end if
        end do
      end do
      !-

      !EFa Nov06, derivative: CONTINUITY over VELOCITY
      !nis,mar07: Reducing the error possibilities by using a loop
      do l = 1, 2
        do c = 1, 2
          ia = 3 + (2 * ndf) * (l - 1)
          ib = 1 + (2 * ndf) * (c - 1)
          !dg1/dv1
          estifm(ia,ib) = estifm(ia,ib)
             !Wichtungsfunktion
     +       + xm(l) * hfact(i) * ABS(xl(3))/2*
!     +       + xm(l) * hfact(i) * xl(3)/2*
             !Term B
     +       ( areaint(nn,i) * dmx(c)
             !Term C
     +       + xm(c) * daintdx(nn,i))
        end do
      end do
      !-

  276 CONTINUE

!      TVOL(NN)=TVOL(NN)+AMW

      ENDDO gaussloop

      IF(NTX .EQ. 0) RETURN
      IF(NTX .EQ. 3) RETURN


      hhalt(nn,1)=vel(3,n1)
      hhalt(nn,2)=vel(3,n3)

!      IF(MOD(IMMT,100) .GT. 90) GO TO 1305

C...... Compute boundary forces
C-

      !EFa Nov06, Anpassung an 1D-Teschke-Elemente
      DO L=1,NCN,2
      N1=NCON(L)

      IF(MOD(NFIX(N1)/100,10) .EQ. 2) THEN

!       NA = (L-1) * NDF + 1
!
!       ASoll = 0.0
!       do k = 0,12
!         ASoll = ASoll + apoly(n1,k) * spec(n1,3)**(k)
!       end do
!
!       !EFa Mar07, Boundary condition
!       ppl = grav * ASoll !* qfact(L)
!       IF(L .EQ. 1) PPL = -PPL
!
!       Term = 0.0
!       Wert = 0.0
!       do k = 1, 12
!         kreal = k
!         Term = Term + (kreal) / (kreal + 1.0)
!     +          * apoly(n1,k) * vel(3,n1)**(k+1)
!         WRITE(*,*) Term, kreal/(kreal+1)*apoly(n1,k)*vel(3,n1)**(k+1)
!       end do
!       zist = Term/ vel(3,n1)
!
!       dTermdh = 0.0
!       do k = 1, 12
!         dTermdh = dTermdh + k * apoly(n1,k) * vel(3,n1)**(k)
!       end do
!       WRITE(*,*) 'RBTest:', ah(n1), zist, asoll, dTermdh
!
!       F(NA) = F(NA) - PPL * (SPEC(N1,3) - 2*zist)
!       ESTIFM(NA,NA+2) = ESTIFM(NA,NA+2) - PPL *
!     + 2 * ((dtermdh * vel(3,n1) - Term) / vel(3,n1)**2)
!       !nis,testing
!       WRITE(*,*) -2*PPL * ((dtermdh * vel(3,n1) - Term) / vel(3,n1)**2)
!       WRITE(*,*) PPL * (SPEC(N1,3) - zist)
!       WRITE(*,*) vel(3,n1), Term, dtermdh, zist, asoll, spec(n1, 3)
!       WRITE(*,'(13(f5.2,1x))') (apoly(n1,i), i=0, 12)
!       !-

        NA=(L-1)*NDF+1
        do iii=1, nef
          estifm(na,iii) = 0.0
        enddo

        estifm(na, na + 2) = 1.0
        f(na)             = 0.0

      ENDIF
      ENDDO

      !EFa Mar07, changed estifm and residuenvector for steady state solution
      !EFa Dec06, keine Berechnung der RB Q für stationären Fall, deaktiviert nach Test
      !if (icyc.le.0) then
      !  GO TO 1305
      !endif
      !-

      !nis,mar07: Boundary conditions - DISCHARGE Q
      QBCAssign: DO N=1, NCN, 2 !no midside nodes
        M=NCON(N)

        !if no BC for Q then cycle
        IF(NFIX(M)/1000.LT.13) CYCLE QBCAssign

        !line of degree of freedom (velocity)
        IRW = (N-1)*NDF + 1
        !line of degree of freedom (depth)
        IRH = IRW + 2

        !cosinus and sinus of nodal direction angle
        CX=COS(ALFA(M))
        SA=SIN(ALFA(M))

        !actual velocity
        VT=VEL(1,M)*CX+VEL(2,M)*SA

        !fictional width at node
        AWIDT = bnode(m)
        !all other entrees are zero
        DO J=1,NEF
          ESTIFM(IRW,J)=0.
        ENDDO

        !install new boundary condition values
        ESTIFM(IRW,IRW) =  ah(m)
        ESTIFM(IRW,IRH) =  dahdh(m) * vt
        F(IRW)          = (SPEC(M,1) - VT * ah(m))
     +                    * xl(3)/ ABS(xl(3)) * dirfact
        !ESTIFM(IRW,IRW) = AREA(NN) * VEL(3,M) * AWIDT
        !ESTIFM(IRW,IRH) = AREA(NN) * VT * AWIDT
        !F(IRW)          = AREA(NN) * (SPEC(M,1) - AWIDT * VT * VEL(3,M))
        !ESTIFM(IRW,IRW) = VEL(3,M) * AWIDT
        !ESTIFM(IRW,IRH) = VT * AWIDT
        !F(IRW)          = (SPEC(M,1) - AWIDT * VT * VEL(3,M))
        !+                    * xl(3)/ abs(xl(3)) * dirfact

!        WRITE(*,*) 'RB: ',spec(m,1), vt, ah(m), vt * ah(m)
!        WRITE(*,*) vel(1,m), vel(2,m), cx, sa, vel(3,m)
        !pause
        !nis,mar07: Fixing velocity as Q-boundary condition
        !do iii = 1, nef
        !  ESTIFM (IRW, iii) = 0.0
        !ENDDO
        !F(IRW) = 0.0
        !
        !ESTIFM (IRW, IRH) = 1.0
        !-

      ENDDO QBCAssign
      !-

 1305 CONTINUE
 1320 CONTINUE
      DO 1050 I=1,NCN,2
      J=NCON(I)
      IA=NDF*(I-1)
      DO 1050 K=1,NDF
      IA=IA+1
      JA=NBC(J,K)
      IF(JA.EQ.0) GO TO 1050
      R1(JA)=R1(JA)+F(IA)

 1050 CONTINUE

      !nis,feb07,testing
      if (nn < 3) then
        n3 = ncon(3)
        n1 = ncon(1)
        WRITE(*,*) '*************************'
        WRITE(*,*) 'Knotendaten, Element: ', nn
        WRITE(*,*) '*************************'
        WRITE(*,'(a18,2(6x,i4))')
     +                        '         Knoten: ',  nop(nn,1), nop(nn,3)
        WRITE(*,'(a18, 1x,f13.8)')   '   Sohlgefaelle: ', sbot(nn)
        WRITE(*,'(a18,2(1x,f13.8))') '         Laenge: ',   xl(1), xl(3)
        WRITE(*,*) area(nn)
        WRITE(*,'(a18,2(1x,f13.8))') '   Wassertiefen: ', h1, h3
        WRITE(*,'(a18,2(1x,f13.8))') '     Schl-kurve: ', qh(n1), qh(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '   akt. Abfluss: ',
     +                                              qhalt(n1), qhalt(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '   Fliessgeschw: ',
     +                                            vel_res(1), vel_res(2)
        WRITE(*,'(a18,2(1x,f13.8))') '  Fliessflaeche: ', ah(n1), ah(n3)
        !WRITE(*,*) 'xl = ', xl(3)
        WRITE(*,'(a18,2(1x,f13.8))') '   Reibgefaelle: ',
     +                                              sfnod(n1), sfnod(n3)

        !WRITE(*,*) sfnod(n1), sbot(nn), qrandb, qh(n1), qgef(n1)
        WRITE(*,'(a18,2(1x,f13.8))') '          dahdh: ',
     +                                              dahdh(n1), dahdh(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '        d2ahdh2: ',
     +                                            d2ahdh(n1), d2ahdh(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '       dsfnoddh: ',
     +                                        dsfnoddh(n1), dsfnoddh(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '        dQSchdh: ',
     +                                              dqhdh(n1), dqhdh(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '      d2QSchdh2: ',
     +                                            d2qhdh(n1), d2qhdh(n3)
        WRITE(*,'(a18,2(1x,f13.8))') '      dSfnodedQ: ',
     +                                        dsfnoddq(n3), dsfnoddq(n3)
        WRITE(*,*)                  '******************'
        WRITE(*,'(a18,4(1x,i4))'  ) '     Gausspunkte: ', (i,i = 1, 4)
        WRITE(*,*)                  '******************'
        WRITE(*,'(a18,4(1x,f13.8))') '           hhint: ',
     +                                           (hhint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '        dhhintdx: ',
     +                                        (dhhintdx(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '           qqint: ',
     +                                           (qqint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '         qqintdx: ',
     +                                         (qqintdx(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '         areaint: ',
     +                                         (areaint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '      dareaintdh: ',
     +                                      (dareaintdh(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '     d2areaintdh: ',
     +                                     (d2areaintdh(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '         daintdx: ',
     +                                         (daintdx(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '        d2aintdx: ',
     +                                        (d2aintdx(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '      d2aintdhdx: ',
     +                                        (d2aidhdx(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '         qschint: ',
     +                                         (qschint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '        dqsintdh: ',
     +                                        (dqsintdh(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '         d2qsidh: ',
     +                                         (d2qsidh(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '        dqsintdx: ',
     +                                        (dqsintdx(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '        s0schint: ',
     +                                        (s0schint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '           sfint: ',
     +                                           (sfint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '       dsfintdh1: ',
     +                                       (dsfintdh1(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '        vflowint: ',
     +                                        (vflowint(nn,i), i = 1, 4)
        WRITE(*,'(a18,4(1x,f13.8))') '         dvintdx: ',
     +                                         (dvintdx(nn,i), i = 1, 4)
      !end if
      !pause
      ENDIF
      !-
      !nis,feb07,testing: Writing whole matrix (just steady case for 1D element)
      matrix (2*nn-1, 2*nn-1) = matrix (2*nn-1, 2*nn-1) + estifm(1,1)
      matrix (2*nn-1, 2*nn)   = matrix (2*nn-1, 2*nn)   + estifm(1,3)
      matrix (2*nn-1, 2*nn+1) = matrix (2*nn-1, 2*nn+1) + estifm(1,9)
      matrix (2*nn-1, 2*nn+2) = matrix (2*nn-1, 2*nn+2) + estifm(1,11)
      matrix (2*nn  , 2*nn-1) = matrix (2*nn  , 2*nn-1) + estifm(3,1)
      matrix (2*nn  , 2*nn)   = matrix (2*nn  , 2*nn)   + estifm(3,3)
      matrix (2*nn  , 2*nn+1) = matrix (2*nn  , 2*nn+1) + estifm(3,9)
      matrix (2*nn  , 2*nn+2) = matrix (2*nn  , 2*nn+2) + estifm(3,11)
      matrix (2*nn+1, 2*nn-1) = matrix (2*nn+1, 2*nn-1) + estifm(9,1)
      matrix (2*nn+1, 2*nn)   = matrix (2*nn+1, 2*nn)   + estifm(9,3)
      matrix (2*nn+1, 2*nn+1) = matrix (2*nn+1, 2*nn+1) + estifm(9,9)
      matrix (2*nn+1, 2*nn+2) = matrix (2*nn+1, 2*nn+2) + estifm(9,11)
      matrix (2*nn+2, 2*nn-1) = matrix (2*nn+2, 2*nn-1) + estifm(11,1)
      matrix (2*nn+2, 2*nn)   = matrix (2*nn+2, 2*nn)   + estifm(11,3)
      matrix (2*nn+2, 2*nn+1) = matrix (2*nn+2, 2*nn+1) + estifm(11,9)
      matrix (2*nn+2, 2*nn+2) = matrix (2*nn+2, 2*nn+2) + estifm(11,11)

      vector (2*nn-1) = vector (2*nn-1) + f(1)
      vector (2*nn)   = vector (2*nn)   + f(3)
      vector (2*nn+1) = vector (2*nn+1) + f(9)
      vector (2*nn+2) = vector (2*nn+2) + f(11)
      !-

      !nis,mar07,testing
      if (nn < 100) then
      write (*,*) 'Element: ', nn
      WRITE(*,9898) estifm(1,1), estifm(1,3),
     + estifm(1,9),estifm(1,11), f(1)
      WRITE(*,9898) estifm(3,1), estifm(3,3),
     + estifm(3,9),estifm(3,11), f(3)
      WRITE(*,9898) estifm(9,1), estifm(9,3),
     + estifm(9,9),estifm(9,11), f(9)
      WRITE(*,9898) estifm(11,1), estifm(11,3),
     + estifm(11,9), estifm(11,11), f(11)

 9898 format (10(1x,f14.8))
      pause
      end if
      !-

      RETURN
*-
*...... Special case for junction element
*-
 2000 CONTINUE
cipk dec00
C-
C...... Special cases for control structures or junction sources
C-
      IF(IMAT(NN) .GT. 903) THEN
        CALL CSTRC(NN)
        GO TO 1320
      ENDIF

*-
*...... Special case for junction element
*-

      NCN=NCORN(NN)
c     WRITE(*,*) NN,NCN
      F(1)=0.
      N1=NCON(1)
      XHT=1.0
      DO 2010 KK=1,NCN,2
        N1=NCON(KK)
        IF(N1 .EQ. 0) GO TO 2010
        NA=(KK-1)*NDF+1
        ESTIFM(1,NA)=DIR(N1)*ah(n1)*xht
        CX=COS(ALFA(N1))
        SA=SIN(ALFA(N1))
        R=VEL(1,N1)*CX+VEL(2,N1)*SA
        ESTIFM(1,NA+2)=DIR(N1)*ah(n1)/vel(3,n1)*r*xht
        F(1)=F(1)-ESTIFM(1,NA)*R
 2010 CONTINUE
      NRX=NCON(1)
      DO 2020 KK=3,NCN
        N1=NCON(KK)
        IF(N1 .EQ. 0) GO TO 2020
        NA=(KK-1)*NDF+1
        ESTIFM(NA,3)=XHT
        ESTIFM(NA,NA+2)=-XHT
        IF (IDNOPT .LT. 0) THEN           
          HD1 = VEL(3,N1)
          CALL AMF(HS1,HD1,AKP(N1),ADT(N1),ADB(N1),AML,DUM2,0)
          WSEL1 = ADO(N1)+HS1
          HDX = VEL(3,NRX)
          CALL AMF(HSX,HDX,AKP(NRX),ADT(NRX),ADB(NRX),AML,DUM2,0)
          WSELX = ADO(NRX)+HSX
          ELSE
            WSEL1=AO(N1)+VEL(3,N1)
            WSELX=AO(NRX)+VEL(3,NRX)
        ENDIF
        F(NA)=XHT*(WSEL1-WSELX)
 2020 CONTINUE
      GO TO 1320
      END
