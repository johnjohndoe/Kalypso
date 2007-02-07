C     Last change:  ST   22 Jan 2007   10:50 am
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

!NiS,jul06: There's a problem with the data types while calling amf. In other subroutines amf is called by
!           passing the value directly as vel(3,n) (real kind=8). In this subroutine the vel(3,n) value is
!           stored in a local copy that is implicitly real, kind=4. All the temporary values are now declared
!           also as real, kind=8.
      REAL(KIND=8) :: HS, HD, HD1, HDX, dum1, HS1, HSX
!-

C
!NiS,apr06: adding variables for friction calculation with DARCY-WEISBACH
      REAL :: lambda
!-
!NiS,jul06: declaring waterdepth for proper parameter-passing
      REAL (KIND=8) :: h

!-
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
      AKE=1.0

      IF (GRAV .LT. 32.)  THEN
        grav = GRAV
      ELSE
        grav = GRAV/2.208
      ENDIF
C-
C-.....ASSIGN PROPER COEFS.....
C-
C  Test for width > 0
      N1 = NOP(NN,1)
      N3 = NOP(NN,3)
      !EFa Nov06, Kontrolle der Eingabe Flächenpolynome und Schlüsselkurve
      if (apoly(n1,1).ne.0.or.apoly(n1,2).ne.0.or.apoly(n1,3).ne.0.or.
     +    apoly(n1,4).ne.0.or.apoly(n1,5).ne.0.or.apoly(n1,6).ne.0.or.
     +    apoly(n1,7).ne.0.or.apoly(n1,8).ne.0.or.apoly(n1,9).ne.0.or.
     +    apoly(n1,10).ne.0.or.apoly(n1,11).ne.0.or.apoly(n1,12)
     +    .ne.0.or.apoly(n1,13).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Flächenpolynom fehlt für Knoten ', N1
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Flächenpolynom fehlt für Knoten ', N1
         STOP  '  Flächenpolynom fehlt '
      end if

      if (apoly(n3,1).ne.0.or.apoly(n3,2).ne.0.or.apoly(n3,3).ne.0.or.
     +    apoly(n3,4).ne.0.or.apoly(n3,5).ne.0.or.apoly(n3,6).ne.0.or.
     +    apoly(n3,7).ne.0.or.apoly(n3,8).ne.0.or.apoly(n3,9).ne.0.or.
     +    apoly(n3,10).ne.0.or.apoly(n3,11).ne.0.or.apoly(n3,12)
     +    .ne.0.or.apoly(n3,13).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Flächenpolynom fehlt für Knoten ', N1
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Flächenpolynom fehlt für Knoten ', N1
         STOP  '  Flächenpolynom fehlt '
      end if
C
      if (qpoly(n1,1).ne.0.or.qpoly(n1,2).ne.0.or.qpoly(n1,3).ne.0.or.
     +    qpoly(n1,4).ne.0.or.qpoly(n1,5).ne.0.or.qpoly(n1,6).ne.0.or.
     +    qpoly(n1,7).ne.0.or.qpoly(n1,8).ne.0.or.qpoly(n1,9).ne.0.or.
     +    qpoly(n1,10).ne.0.or.qpoly(n1,11).ne.0.or.qpoly(n1,12)
     +    .ne.0.or.qpoly(n1,13).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Schlüsselkurve fehlt für Knoten ', N3
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Schlüsselkurve fehlt für Knoten ', N3
         STOP  '  Schlüsselkurve fehlt '
      end if
      if (qpoly(n3,1).ne.0.or.qpoly(n3,2).ne.0.or.qpoly(n3,3).ne.0.or.
     +    qpoly(n3,4).ne.0.or.qpoly(n3,5).ne.0.or.qpoly(n3,6).ne.0.or.
     +    qpoly(n3,7).ne.0.or.qpoly(n3,8).ne.0.or.qpoly(n3,9).ne.0.or.
     +    qpoly(n3,10).ne.0.or.qpoly(n3,11).ne.0.or.qpoly(n3,12)
     +    .ne.0.or.apoly(n3,13).ne.0 ) then
         !nichts
      else
         WRITE(*,*) ' Schlüsselkurve fehlt für Knoten ', N1
         CLOSE(75)
         OPEN(75,FILE='ERROR.OUT')
         WRITE(75,*) ' Schlüsselkurve fehlt für Knoten ', N1
         STOP  '  Schlüsselkurve fehlt '
      end if

      TEL=AREA(NN)
      AREA(NN)=0.
cipk nov97
      TVOL(NN)=0.

      NCN=NCORN(NN)
      DO 63 N=1,NCN
        NCON(N)=NOP(NN,N)
        kennung(ncon(n))=1
   63 CONTINUE

      IF(NCN .EQ. 5  .AND.  IMAT(NN) .LT. 900) NCN=3
cipk nov97
      ncnx=2
c
c     Initialize AME and DAME
c
      IF (IDNOPT.LT.0) THEN
         DO M = 1, NCNX
           MC = 2 * M - 1
           N = NOP(NN,MC)
           HS = VEL(3,N)
           ISWT = 0
           CALL AMF(DUM1,HS,AKP(N),ADT(N),ADB(N),AME(M),DAME(M),ISWT)
         END DO
      ENDIF

C-
C- INITIALIZE MATRICES AND VARIABLES
C-
      NEF=NCN*NDF
      DO 77 I=1,NEF
      F(I) = 0.0
      DO 77 J=1,NEF
   77 ESTIFM(I,J) = 0.0
cipk oct98 update to f90
      IMMT=IMAT(NN)
      MR=MOD(IMMT,1000)

cipk dec00 allow gate type elements

      IF(MR .GT. 900  .AND. IGTP(NN) .EQ. 0) GO TO 2000

      IF(MR .LT. 900) THEN
       
        MR=MOD(IMMT,100)

      ENDIF

      MAT=MR

cipk dec00 skip out for active gate closure

      IF(IGTP(NN) .NE. 0) THEN
        IF(IGTCL(NN) .EQ. 1) THEN

          NGT=IMAT(NN)-900
          IF(NTX .EQ. 1) THEN
            AREA(NN)=TEL
            RETURN
	    ENDIF
	  ENDIF
      ENDIF

C
      ROAVG=1.935
      IF (GRAV .LT. 32.)  ROAVG = 516. * 1.935
C
      IF(NTX .EQ. 0) THEN
        N1=NCON(1)
        N2=NCON(3)
        TH(NN)=ATAN2(CORD(N2,2)-CORD(N1,2),CORD(N2,1)-CORD(N1,1))
      ENDIF

      CXX=COS(TH(NN))
      SAA=SIN(TH(NN))
      NGP=4
      NCNX=2
C-
C-.....COMPUTE LOCAL CORDS.....
C-
      n1=ncon(1)
      n3=ncon(3)
      h1=vel(3,n1)
      h3=vel(3,n3)
      NR=NCON(1)
      DO 100 m = 1, 2
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
        DX=CORD(N,1)-CORD(NR,1)
        DY=CORD(N,2)-CORD(NR,2)
        if (kmx(n1).EQ.-1.or.kmx(n3).EQ.-1) then
          XL(K)=DX*CXX+DY*SAA
          YL(K)=-DX*SAA+DY*CXX
        else
          xl(1)=0
          yl(1)=0
          xl(3)=ABS(kmx(n1)-kmx(n3))
          yl(3)=0
        end if
        vel(1,n1)=sqrt(vel(1,n1)**2+vel(2,n1)**2)
  100 CONTINUE
      xl(2)=xl(3)/2
      yl(2)=yl(3)/2
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


      n1=ncon(1)
      n3=ncon(3)
      h1=vel(3,n1)
      h3=vel(3,n3)
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
      ah(n1)=0
      ah(n3)=0
      qh(n1)=0
      qh(n3)=0
      hdif(n1)=0
      hdif(n3)=0
      sbot(nn)=ABS(ao(n3)-ao(n1))/xl(3)
      do k=1,13
        pbei(n1,k)=0
        pbei(n3,k)=0
      end do
      do j=1,4
        pdif(n1,j)=0
        pdif(n3,j)=0
      end do
      do k=1,13
        ah(n1)=ah(n1)+apoly(n1,k)*h1**(k-1)
        ah(n3)=ah(n3)+apoly(n3,k)*h3**(k-1)
        qh(n1)=qh(n1)+qpoly(n1,k)*h1**(k-1)
        qh(n3)=qh(n3)+qpoly(n3,k)*h3**(k-1)
      end do
      if (icyc.le.0.or.maxn.eq.1) then
        qhalt(nn,1)=qh(n1)
        qhalt(nn,2)=qh(n3)
      else
        qhalt(nn,1)=vel(1,n1)*ah(n1)
        qhalt(nn,2)=vel(1,n3)*ah(n3)
      end if
      if (icyc.le.0) then
        sfnod(n1)=sfwicht(n1)*qrandb*ABS(qrandb)/(qh(n1)**2)*
     +            qgef(n1)
        sfnod(n3)=sfwicht(n3)*qrandb*ABS(qrandb)/(qh(n3)**2)*
     +            qgef(n3)
      else
        sfnod(n1)=sfwicht(n1)*qhalt(nn,1)*ABS(qhalt(nn,1))/(qh(n1)**2)*
     +            qgef(n1)
        sfnod(n3)=sfwicht(n3)*qhalt(nn,2)*ABS(qhalt(nn,2))/(qh(n3)**2)*
     +            qgef(n3)
      endif
      if (icyc.le.0.OR.(icyc.gt.0.and.maxn.eq.1)) then
        vel(1,n1)=qh(n1)/ah(n1)
        vel(1,n3)=qh(n3)/ah(n3)
      end if


      if (ntx.eq.1) then
      !EFa Nov06, Testen ob schießen vorliegt
      froude(n1)=qhalt(nn,1)/(ah(n1)*sqrt(grav*h1))
      froude(n3)=qhalt(nn,2)/(ah(n3)*sqrt(grav*h3))
      if (froude(n1).gt.1) then
        WRITE(*,*)'Schiessen an Knoten ',n1
      ELSEIF (froude(n3).gt.1) then
        WRITE(*,*)'Schiessen an Knoten ',n3
      end if

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

      if (icyc.gt.0) then
        if (maxn.eq.1) then
          dqdtaltzs(nn,1)=dqqt(nn,1)
          dqdtaltzs(nn,2)=dqqt(nn,2)
          qqt(n1)=qh(n1)
          qqt(n3)=qh(n3)
        end if
        dqqt(nn,1)=1.6*(qhalt(nn,1)-qqt(n1))/delt+(1-1.6)*
     +             dqdtaltzs(nn,1)
        dqqt(nn,2)=1.6*(qhalt(nn,2)-qqt(n3))/delt+(1-1.6)*
     +             dqdtaltzs(nn,2)
      end if

      !EFa Nov06, Energiestrombeiwert
      if (beient.eq.1) then       
        do k=1,13
          pbei(n1,k)=alphapk(n1,k)
          pbei(n3,k)=alphapk(n3,k)
        end do
        do j=1,4
          pdif(n1,k)=alphad(n1,k)
          pdif(n3,k)=alphad(n3,k)
        end do
        hdif(n1)=alphah(n1)
        hdif(n3)=alphah(n3)
      endif

      !EFa Nov06, Impulsstrombeiwert
      IF (beient.eq.2) then       
        do k=1,13
          pbei(n1,k)=betapk(n1,k)
          pbei(n3,k)=betapk(n3,k)
        end do
        do j=1,4
          pdif(n1,k)=betad(n1,k)
          pdif(n3,k)=betad(n3,k)
        end do
        hdif(n1)=betah(n1)
        hdif(n3)=betah(n3)
      end if
      dahdh(n1)=0
      dahdh(n3)=0
      dqhdh(n1)=0
      dqhdh(n3)=0
      do k=2,13
        dahdh(n1)=dahdh(n1)+(k-1)*apoly(n1,k)*h1**(k-2)
        dahdh(n3)=dahdh(n3)+(k-1)*apoly(n3,k)*h1**(k-2)
        dqhdh(n1)=dqhdh(n1)+(k-1)*qpoly(n1,k)*h1**(k-2)
        dqhdh(n3)=dqhdh(n3)+(k-1)*qpoly(n3,k)*h1**(k-2)
        dsfnoddh(n1)=-2*(sfnod(n1)/qh(n1))*dqhdh(n1)
        dsfnoddh(n3)=-2*(sfnod(n3)/qh(n3))*dqhdh(n3)
      end do
      if (icyc.le.0) then
        dsfnoddq(n1)=2*sfnod(n1)/qrandb
        dsfnoddq(n3)=2*sfnod(n3)/qrandb
      else
        dsfnoddq(n1)=2*sfnod(n1)/qhalt(nn,1)
        dsfnoddq(n3)=2*sfnod(n3)/qhalt(nn,2)
      endif
      d2ahdh(n1)=0
      d2ahdh(n3)=0
      d2qhdh(n1)=0
      d2qhdh(n3)=0
      do k=3,13
        d2ahdh(n1)=d2ahdh(n1)+(n-1)*(k-2)*apoly(n1,k)*h1**(k-3)
        d2ahdh(n3)=d2ahdh(n3)+(n-1)*(k-2)*apoly(n3,k)*h1**(k-3)
        d2qhdh(n1)=d2qhdh(n1)+(n-1)*(k-2)*apoly(n1,k)*h1**(k-3)
        d2qhdh(n3)=d2qhdh(n3)+(n-1)*(k-2)*apoly(n3,k)*h1**(k-3)
      end do

      !EFa Nov06, Bereich für Wasserspiegel Bordvoll (nur Hauptgerinne)
      if (h1.le.hbordv(n1)) then
        bei(n1)=1
        dbeidh(n1)=0
        d2beidh(n1)=0
        pbei(n1,1)=1
        do k=2,13
          pbei(n1,k)=0
        end do
      end if

      if (h3.le.hbordv(n3)) then
        bei(n3)=1
        dbeidh(n3)=0
        d2beidh(n3)=0
        pbei(n3,1)=1
        do k=2,13
          pbei(n3,k)=0
        end do
      end if

      !EFa Nov06, Bereich für Höhen zwischen dem unteren Grenzwert als Wasserspiegel Bordvoll
      !           und dem oberen Grenzwert des Übergangspolynoms
      if (h1.gt.hbordv(n1).and.h1.lt.hdif(n1)) then
        bei(n1)=0
        do k=1,4
          bei(n1)=bei(n1)+pdif(n1,k)*h1**(k-1)
          pbei(n1,k)=pdif(n1,k)
        end do
        do k=5,13
          pbei(n1,k)=0
        end do
        dbeizdh(n1)=0
        do k=2,4
          dbeizdh(n1)=dbeizdh(n1)+(k-1)*pdif(n1,k)*h1**(k-2)          !1. Ableitung
        end do
        d2beizdh(n1)=0
        do k=3,4
          d2beizdh(n1)=d2beizdh(n1)+(k-1)*(k-2)*pdif(n1,k)*h1**(k-3)  !2. Ableitung
        end do
        dbeidh(n1)=dbeizdh(n1)
        d2beidh(n1)=d2beizdh(n1)
      end if

      if (h3.gt.hbordv(n3).and.h3.lt.hdif(n3)) then
        bei(n3)=0
        do k=1,4
          bei(n3)=bei(n3)+pdif(n3,k)*h3**(k-1)
          pbei(n3,k)=pdif(n3,k)
        end do
        do k=5,13
          pbei(n3,k)=0
        end do
        dbeizdh(n3)=0
        do k=2,4
          dbeizdh(n3)=dbeizdh(n3)+(k-1)*pdif(n3,k)*h3**(k-2)          !1. Ableitung
        end do
        d2beizdh(n3)=0
        do k=3,4
          d2beizdh(n3)=d2beizdh(n3)+(k-1)*(k-2)*pdif(n3,k)*h3**(k-3)  !2. Ableitung
        end do
        dbeidh(n3)=dbeizdh(n3)
        d2beidh(n3)=d2beizdh(n3)
      end if

      !EFa Nov06, Bereich für Höhen über dem oberen Grenzwert des Übergangspolynoms
      if (h1.ge.hdif(n1)) then
        bei(n1)=0
        do k=1,13
          bei(n1)=bei(n1)+pbei(n1,k)*h1**(k-1)
        end do
        dbeiodh(n1)=0
        do k=2,13
          dbeiodh(n1)=dbeiodh(n1)+(k-1)*pbei(n1,k)*h1**(k-2)          !1. Ableitung
        end do
        d2beiodh(n1)=0
        do k=3,13
          d2beiodh(n1)=d2beiodh(n1)+(k-1)*(k-2)*pbei(n1,k)*h1**(k-3)  !2. Ableitung
        end do
        dbeidh(n1)=dbeiodh(n1)
        d2beidh(n1)=dbeiodh(n1)
      end if

      if (h3.ge.hdif(n3)) then
        bei(n3)=0
        do k=1,13
          bei(n3)=bei(n3)+pbei(n3,k)*h3**(k-1)
        end do
        dbeiodh(n3)=0
        do k=2,13
          dbeiodh(n3)=dbeiodh(n3)+(n-1)*pbei(n3,k)*h3**(k-2)          !1. Ableitung
        end do
        d2beiodh(n3)=0
        do k=3,13
          d2beiodh(n3)=d2beiodh(n3)+(k-1)*(k-2)*pbei(n3,k)*h3**(k-3)  !2. Ableitung
        end do
        dbeidh(n3)=dbeiodh(n3)
        d2beidh(n3)=dbeiodh(n3)
      end if

      !EFa Nov06, wenn ohne Beiwerte gerechnet werden soll oder die Eingabe von beient fehlerhaft ist
      if ((beient.eq.0).OR.(beient.ne.2).AND.(beient.ne.1))then
        bei(n1)=1
        dbeidh(n1)=0
        d2beidh(n1)=0
        pbei(n1,1)=1
        do k=2,13
          pbei(n1,k)=0
        end do
        pdif(n1,1)=1
        do k=2,4
          pdif(n1,k)=0
        end do
        bei(n3)=1
        dbeidh(n3)=0
        d2beidh(n3)=0
        pbei(n1,3)=1
        do k=2,13
          pbei(n3,k)=0
        end do
        pdif(n3,1)=1
        do k=2,4
          pdif(n3,k)=0
        end do
      endif

      !EFa Nov06, fiktive Breite
      bnode(n1)=ah(n1)/h1
      bnode(n3)=ah(n3)/h3

      ENDIF !ntx=1

      
      DO 500 I = 1, NGP
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
      XM(1)=1.-AFACT(I)
      XM(2)=AFACT(I)
      DMX(1)=-1./TEMP
      DMX(2)=1./TEMP

      IF(NTX .NE. 0) THEN
        H=VEL(3,N1)*XM(1)+VEL(3,N2)*XM(2)
      ELSE
        H=1.0
      ENDIF
      AMW=ABS(TEMP)*HFACT(I)/2.
      AREA(NN)=AREA(NN)+AMW
      IF(NTX .EQ. 0) GO TO 500
cipk nov97
      IF(NTX .EQ. 3) GO TO 276


      !EFa Nov06, hoehelem(nn)
      hhint(nn,i)=h1*xm(1)+h3*xm(2)
      dhhintdx(nn,i)=h1*dmx(1)+h3*dmx(2)
      dhintdt(nn,i)=xm(1)*dhht(nn,1)+xm(2)*dhht(nn,2)

      !EFa Nov06, flowelem(nn)
      qqint(nn,i)=qhalt(nn,1)*xm(1)+qhalt(nn,2)*xm(2)
      qqintdx(nn,i)=qhalt(nn,1)*dmx(1)+qhalt(nn,2)*dmx(2)
      dqintdt(nn,i)=dqqt(nn,1)*xm(1)+dqqt(nn,2)*xm(2)

      !EFa Nov06, areaelem(nn)
      areaint(nn,i)=ah(n1)*xm(1)+ah(n3)*xm(2)
      dareaintdh(nn,i)=dahdh(n1)*xm(1)+dahdh(n3)*xm(2)
      d2areaintdh(nn,i)=d2ahdh(n1)*xm(1)+d2ahdh(n3)*xm(2)
      daintdx(nn,i)=dmx(1)*ah(n1)+xm(1)*dahdh(n1)*dhhintdx(nn,i)+
     +              dmx(2)*ah(n3)+xm(2)*dahdh(n3)*dhhintdx(nn,i)
      d2aintdx(nn,i)=dhhintdx(nn,i)*(2*dmx(1)*dahdh(n1)+xm(1)*
     +               d2ahdh(n1)*dhhintdx(nn,i)+2*dmx(2)*dahdh(n3)+
     +               xm(2)*d2ahdh(n3)*dhhintdx(nn,i))
      d2aidhdx(nn,i)=dmx(1)*dahdh(n1)+xm(1)*d2ahdh(n1)*dhhintdx(nn,i)+
     +               dmx(2)*dahdh(n3)+xm(2)*d2ahdh(n3)*dhhintdx(nn,i)
      daintdt(nn,i)=(xm(1)*dahdh(n1)+xm(2)*dahdh(n3))*dhintdt(nn,i)

      !EFa Dec06, Fließgeschwindigkeit
      vflowint(nn,i)=qqint(nn,i)/areaint(nn,i)
      dvintdx(nn,i)=dmx(1)*qhalt(nn,1)/ah(n1)+dmx(2)*qhalt(nn,2)/ah(n3)

      !EFa Nov06, Testen ob Schießen im Element vorliegt
      froudeint(nn,i)=qqint(nn,i)/(areaint(nn,i)*sqrt(grav*hhint(nn,i)))
      if (froudeint(nn,i).gt.1) then
        WRITE(*,*)'Schiessen in Element', nn
      end if

      !EFa Nov06, schkelem(nn)
      qschint(nn,i)=xm(1)*qh(n1)+xm(2)*qh(n3)
      dqsintdh(nn,i)=dqhdh(n1)*xm(1)+dqhdh(n3)*xm(2)
      d2qsidh(nn,i)=d2qhdh(n1)*xm(1)+d2qhdh(n3)*xm(2)
      dqsintdx(nn,i)=dmx(1)*qh(n1)+xm(1)*dqhdh(n1)*dhhintdx(nn,i)+
     +               dmx(2)*qh(n3)+xm(2)*dqhdh(n3)*dhhintdx(nn,i)
      d2qsidhdx(nn,i)=dmx(1)*dqhdh(n1)+xm(1)*d2qhdh(n1)*dhhintdx(nn,i)+
     +                dmx(2)*dqhdh(n3)+xm(2)*d2qhdh(n3)*dhhintdx(nn,i)

      !EFa Nov06, fricelem(nn)
      s0schint(nn,i)=qgef(n1)*xm(1)+qgef(n3)*xm(2)
      sfwicint(nn,i)=sfwicht(n1)*xm(1)+sfwicht(n3)*xm(2)
      sfint(nn,i)=sfnod(n1)*xm(1)+sfnod(n3)*xm(2)
      dsfintdh1(nn,i)=-2*(sfint(nn,i)/qschint(nn,i))*dqsintdh(nn,i)

      !EFa Nov06, beiwelem(nn)
      beiint(nn,i)=xm(1)*bei(n1)+xm(2)*bei(n3)
      dbeiintdh(nn,i)=xm(1)*dbeiodh(n1)+xm(2)*dbeiodh(n3)
      d2beiintdh(nn,i)=xm(1)*d2beiodh(n1)+xm(2)*d2beiodh(n3)
      dbeiintdx(nn,i)=dmx(1)*bei(n1)+xm(1)*dbeiodh(n1)*dhhintdx(nn,i)+
     +                dmx(2)*bei(n3)+xm(2)*dbeiodh(n3)*dhhintdx(nn,i)
      d2beiintdhdx(nn,i)=dmx(1)*dbeiodh(n1)+xm(1)*d2beiodh(n1)*
     +                   dhhintdx(nn,i)+dmx(2)*dbeiodh(n3)+xm(2)*
     +                   d2beiodh(n3)*dhhintdx(nn,i)

      !EFa Nov06, fiktive Breite
      bint(nn,i)=areaint(nn,i)/hhint(nn,i)


      !EFa Nov06, Impulsgleichung                                                               
      if (icyc.le.0) then
        ia=3                                                              !f1
      else
        ia=1
      end if
      f(ia)=f(ia)-xm(1)*hfact(i)*xl(3)/2*                                 !Wi(x)
     +     (dqintdt(nn,i)+                                                !Term a
     +     qqint(nn,i)**2/areaint(nn,i)*dbeiintdx(nn,i)+                  !Term b
     +     2*beiint(nn,i)*vflowint(nn,i)*qqintdx(nn,i)-                   !Term c
     +     beiint(nn,i)*vflowint(nn,i)**2*daintdx(nn,i)+                  !Term d
     +     grav*areaint(nn,i)*dhhintdx(nn,i)+                             !Term e
     +     grav*areaint(nn,i)*sfint(nn,i)-                                !Term f
     +     grav*areaint(nn,i)*sbot(nn))                                   !Term g

      if (icyc.le.0) then
        ia=3+2*ndf                                                        !f3
      else
        ia=1+2*ndf
      end if
      f(ia)=f(ia)-xm(2)*hfact(i)*xl(3)/2*                                 !Wi(x)
     +     (dqintdt(nn,i)+                                                !Term a
     +     qqint(nn,i)**2/areaint(nn,i)*dbeiintdx(nn,i)+                  !Term b
     +     2*beiint(nn,i)*vflowint(nn,i)*qqintdx(nn,i)-                   !Term c
     +     beiint(nn,i)*vflowint(nn,i)**2*daintdx(nn,i)+                  !Term d
     +     grav*areaint(nn,i)*dhhintdx(nn,i)+                             !Term e
     +     grav*areaint(nn,i)*sfint(nn,i)-                                !Term f
     +     grav*areaint(nn,i)*sbot(nn))                                   !Term g

      !EFa Nov06, Kontinuitätsgleichung
      if (icyc.gt.0) then
      ia=3                                                                !g1
      f(ia)=f(ia)-xm(1)*hfact(i)*xl(3)/2*                                 !EFa Dec06, Wi(x)
     +      (daintdt(nn,i)+qqintdx(nn,i))                                 !EFa DEc06, Term a, Term b

      ia=3+2*ndf                                                          !g3
      f(ia)=f(ia)-xm(2)*hfact(i)*xl(3)/2*                                 !EFa Dec06, Wi(x)
     +      (daintdt(nn,i)+qqintdx(nn,i))                                 !EFa DEc06, Term a, Term b
      endif

      !EFa Nov06, Sprung an den Beginn der Gauss-Schleife für junction-Elemente
      IF(MOD(immt,100).gt.90) GO TO 500

      !EFa Nov06, Ableitung der Impulsgleichung nach der Fließtiefe
      if (icyc.le.0) then
        ia=3                                                              !df1/dh1
        ib=3
      else
        ia=1
        ib=3
      end if
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(1)*qqint(nn,i)**2/areaint(nn,i)**2*               !Term b
     +              (areaint(nn,i)*d2beiintdhdx(nn,i)-dbeiintdx(nn,i)*
     +              daintdx(nn,i))+
     +              xm(1)*2*qqint(nn,i)/areaint(nn,i)**2*qqintdx(nn,i)*   !Term c
     +              (areaint(nn,i)*dbeiintdh(nn,i)-beiint(nn,i)*
     +              dareaintdh(nn,i))-
     +              xm(1)*(qqint(nn,i)**2/areaint(nn,i)**2)*              !Term d
     +              (dbeiintdh(nn,i)*daintdx(nn,i)+beiint(nn,i)*
     +              d2aidhdx(nn,i)-2*beiint(nn,i)/areaint(nn,i)*
     +              dareaintdh(nn,i)*daintdx(nn,i))+
     +              grav*(xm(1)*dareaintdh(nn,i)*dhhintdx(nn,i)+          !Term e
     +              areaint(nn,i)*dmx(1))+
     +              grav*xm(1)*(sfint(nn,i)*dareaintdh(nn,i)+             !Term f
     +              areaint(nn,i)*dsfintdh1(nn,i))-
     +              grav*xm(1)*sbot(nn)*dareaintdh(nn,i))                 !Term g

      if (icyc.le.0) then                                                 
        ia=3                                                              !df1/dh3
        ib=3+2*ndf
      else
        ia=1
        ib=3+2*ndf
      end if
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(2)*qqint(nn,i)**2/areaint(nn,i)**2*               !Term b
     +              (areaint(nn,i)*d2beiintdhdx(nn,i)-dbeiintdx(nn,i)*
     +              daintdx(nn,i))+
     +              xm(2)*2*qqint(nn,i)/areaint(nn,i)**2*qqintdx(nn,i)*   !Term c
     +              (areaint(nn,i)*dbeiintdh(nn,i)-beiint(nn,i)*
     +              dareaintdh(nn,i))-
     +              xm(2)*(qqint(nn,i)**2/areaint(nn,i)**2)*              !Term d
     +              (dbeiintdh(nn,i)*daintdx(nn,i)+beiint(nn,i)*
     +              d2aidhdx(nn,i)-2*beiint(nn,i)/areaint(nn,i)*
     +              dareaintdh(nn,i)*daintdx(nn,i))+
     +              grav*(xm(2)*dareaintdh(nn,i)*dhhintdx(nn,i)+          !Term e
     +              areaint(nn,i)*dmx(2))+
     +              grav*xm(2)*(sfint(nn,i)*dareaintdh(nn,i)+             !Term f
     +              areaint(nn,i)*dsfintdh1(nn,i))-
     +              grav*xm(2)*sbot(nn)*dareaintdh(nn,i))                 !Term g

      if (icyc.le.0) then
        ia=3+2*ndf                                                        !df3/dh1
        ib=3
      else
        ia=1+2*ndf
        ib=3
      end if
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(1)*qqint(nn,i)**2/areaint(nn,i)**2*               !Term b
     +              (areaint(nn,i)*d2beiintdhdx(nn,i)-dbeiintdx(nn,i)*
     +              daintdx(nn,i))+
     +              xm(1)*2*qqint(nn,i)/areaint(nn,i)**2*qqintdx(nn,i)*   !Term c
     +              (areaint(nn,i)*dbeiintdh(nn,i)-beiint(nn,i)*
     +              dareaintdh(nn,i))-
     +              xm(1)*(qqint(nn,i)**2/areaint(nn,i)**2)*              !Term d
     +              (dbeiintdh(nn,i)*daintdx(nn,i)+beiint(nn,i)*
     +              d2aidhdx(nn,i)-2*beiint(nn,i)/areaint(nn,i)*
     +              dareaintdh(nn,i)*daintdx(nn,i))+
     +              grav*(xm(1)*dareaintdh(nn,i)*dhhintdx(nn,i)+          !Term e
     +              areaint(nn,i)*dmx(1))+
     +              grav*xm(1)*(sfint(nn,i)*dareaintdh(nn,i)+             !Term f
     +              areaint(nn,i)*dsfintdh1(nn,i))-
     +              grav*xm(1)*sbot(nn)*dareaintdh(nn,i))                 !Term g

      if (icyc.le.0) then
        ia=3+2*ndf                                                        !df3/dh3
        ib=3+2*ndf
      else
        ia=1+2*ndf
        ib=3+2*ndf
      end if
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(2)*qqint(nn,i)**2/areaint(nn,i)**2*               !Term b
     +              (areaint(nn,i)*d2beiintdhdx(nn,i)-dbeiintdx(nn,i)*
     +              daintdx(nn,i))+
     +              xm(2)*2*qqint(nn,i)/areaint(nn,i)**2*qqintdx(nn,i)*   !Term c
     +              (areaint(nn,i)*dbeiintdh(nn,i)-beiint(nn,i)*
     +              dareaintdh(nn,i))-
     +              xm(2)*(qqint(nn,i)**2/areaint(nn,i)**2)*              !Term d
     +              (dbeiintdh(nn,i)*daintdx(nn,i)+beiint(nn,i)*
     +              d2aidhdx(nn,i)-2*beiint(nn,i)/areaint(nn,i)*
     +              dareaintdh(nn,i)*daintdx(nn,i))+
     +              grav*(xm(2)*dareaintdh(nn,i)*dhhintdx(nn,i)+          !Term e
     +              areaint(nn,i)*dmx(2))+
     +              grav*xm(2)*(sfint(nn,i)*dareaintdh(nn,i)+             !Term f
     +              areaint(nn,i)*dsfintdh1(nn,i))-
     +              grav*xm(2)*sbot(nn)*dareaintdh(nn,i))                 !Term g

      !Ableitung der Impulsgleichung nach der Fließgeschwindigkeit
      if (icyc.gt.0) then
      ia=1
      ib=1                                                                !df1/dv1
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(1)*areaint(nn,i)*1.6/delt+                        !Term a
     +              xm(1)*2*areaint(nn,i)*qqint(nn,i)*dbeiintdx(nn,i)+    !Term b
     +              2*beiint(nn,i)*(xm(1)*(areaint(nn,i)*dvintdx(nn,i)+   !Term c
     +              vflowint(nn,i)*daintdx(nn,i))+vflowint(nn,i)*
     +              (areaint(nn,i)*dmx(1)+xm(1)*daintdx(nn,i)))-
     +              xm(1)*2*beiint(nn,i)*vflowint(nn,i)*daintdx(nn,i)+    !Term d
     +              xm(1)*2*grav*areaint(nn,i)**2/qqint(nn,i)*            !Term f
     +              sfint(nn,i))
      ia=1
      ib=1+ndf*2                                                          !df1/dv3
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(2)*areaint(nn,i)*1.6/delt+                        !Term a
     +              xm(2)*2*areaint(nn,i)*qqint(nn,i)*dbeiintdx(nn,i)+    !Term b
     +              2*beiint(nn,i)*(xm(2)*(areaint(nn,i)*dvintdx(nn,i)+   !Term c
     +              vflowint(nn,i)*daintdx(nn,i))+vflowint(nn,i)*
     +              (areaint(nn,i)*dmx(2)+xm(2)*daintdx(nn,i)))-
     +              xm(2)*2*beiint(nn,i)*vflowint(nn,i)*daintdx(nn,i)+    !Term d
     +              xm(2)*2*grav*areaint(nn,i)**2/qqint(nn,i)*            !Term f
     +              sfint(nn,i))

      ia=1+2*ndf
      ib=1                                                                !df3/dv1
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(1)*areaint(nn,i)*1.6/delt+                        !Term a
     +              xm(1)*2*areaint(nn,i)*qqint(nn,i)*dbeiintdx(nn,i)+    !Term b
     +              2*beiint(nn,i)*(xm(1)*(areaint(nn,i)*dvintdx(nn,i)+   !Term c
     +              vflowint(nn,i)*daintdx(nn,i))+vflowint(nn,i)*
     +              (areaint(nn,i)*dmx(1)+xm(1)*daintdx(nn,i)))-
     +              xm(1)*2*beiint(nn,i)*vflowint(nn,i)*daintdx(nn,i)+    !Term d
     +              xm(1)*2*grav*areaint(nn,i)**2/qqint(nn,i)*            !Term f
     +              sfint(nn,i))

      ia=1+2*ndf
      ib=1+2*ndf                                                          !df3/dv3
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (xm(2)*areaint(nn,i)*1.6/delt+                        !Term a
     +              xm(2)*2*areaint(nn,i)*qqint(nn,i)*dbeiintdx(nn,i)+    !Term b
     +              2*beiint(nn,i)*(xm(2)*(areaint(nn,i)*dvintdx(nn,i)+   !Term c
     +              vflowint(nn,i)*daintdx(nn,i))+vflowint(nn,i)*
     +              (areaint(nn,i)*dmx(2)+xm(2)*daintdx(nn,i)))-
     +              xm(2)*2*beiint(nn,i)*vflowint(nn,i)*daintdx(nn,i)+    !Term d
     +              xm(2)*2*grav*areaint(nn,i)**2/qqint(nn,i)*            !Term f
     +              sfint(nn,i))

      !EFa Nov06, Ableitung de Kontinuitätsgleichung nach der Fließtiefe
      ia=3
      ib=3                                                                !dg1/dh1
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              xm(1)*(d2areaintdh(nn,i)*dhintdt(nn,i)+               !Term a
     +              dareaintdh(nn,i)*1.6/delt)
      ia=3
      ib=3+2*ndf                                                          !dg1/dh3
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              xm(2)*(d2areaintdh(nn,i)*dhintdt(nn,i)+               !Term a
     +              dareaintdh(nn,i)*1.6/delt)

      ia=3+2*ndf
      ib=3                                                                !dg3/dh1
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              xm(1)*(d2areaintdh(nn,i)*dhintdt(nn,i)+               !Term a
     +              dareaintdh(nn,i)*1.6/delt)

      ia=3+2*ndf
      ib=3+2*ndf                                                          !dg3/dh3
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              xm(2)*(d2areaintdh(nn,i)*dhintdt(nn,i)+               !Term a
     +              dareaintdh(nn,i)*1.6/delt)

      !EFa Nov06, Ableitung der Kontinuitätsgleichung nach der Fließgeschwindigkeit
      ia=3
      ib=1                                                                !dg1/dv1
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (areaint(nn,i)*dmx(1)+xm(1)*daintdx(nn,i))            !Term b

      ia=3
      ib=1+2*ndf                                                          !dg1/dv3
      estifm(ia,ib)=estifm(ia,ib)+xm(1)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (areaint(nn,i)*dmx(2)+xm(2)*daintdx(nn,i))            !Term b

      ia=3+2*ndf
      ib=1                                                                !dg3/dv1
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (areaint(nn,i)*dmx(1)+xm(1)*daintdx(nn,i))            !Term b

      ia=3+2*ndf
      ib=1+2*ndf                                                          !dg3/dv3
      estifm(ia,ib)=estifm(ia,ib)+xm(2)*hfact(i)*xl(3)/2*                 !Wichtungsfunktion
     +              (areaint(nn,i)*dmx(2)+xm(2)*daintdx(nn,i))            !Term b
      endif

  276 CONTINUE

      TVOL(NN)=TVOL(NN)+AMW

  500 CONTINUE

      IF(NTX .EQ. 0) RETURN
      IF(NTX .EQ. 3) RETURN

      hhalt(nn,1)=vel(3,n1)
      hhalt(nn,2)=vel(3,n3)

      IF(MOD(IMMT,100) .GT. 90) GO TO 1305

C...... Compute boundary forces
C-
      !EFa Nov06, Anpassung an 1D-Teschke-Elemente
      DO 650 L=1,NCN,2
      N1=NCON(L)
      IF(MOD(NFIX(N1)/100,10) .EQ. 2) THEN
        XHT=ELEV-AO(N1)
        NA=(L-1)*NDF+1
        RHO=DEN(N1)

        ah(n1)=0
        qh(n1)=0
        do k=1,13
          ah(n1)=ah(n1)+apoly(n1,k)*spec(n1,3)**(k-1)
          qh(n1)=qh(n1)+qpoly(n1,k)*spec(n1,3)**(k-1)
        end do
        if (l.eq.1) then
          mm=1
        ELSEIF (l.eq.3)then
          mm=2
        end if
        vel(3,n1)=spec(n1,3)
        if (icyc.le.0.or.maxn.eq.1) then
          vel(1,n1)=qh(n1)/ah(n1)
          vel(2,n1)=0
        end if
        if (icyc.le.0) then
          do kk=1,nef
            estifm(na+2,kk)=0
            f(na+2)=0
          end do
        else
          do kk=1,nef
            estifm(na,kk)=0
            f(na)=0
          END do
        end if

        if (icyc.le.0) then
          estifm(na+2,na+2)=1
          f(na+2)=0
        else
          estifm(na,na+2)=1
          f(na)=0
        end if

      ELSEIF((IBN(N1) .EQ. 1  .OR.  IBN(N1) .GE. 3)) THEN
        IF(NREF(N1) .EQ. 0) THEN
          NA=(L-1)*NDF+1
          DO 6667 KK=1,NEF
            if (icyc.le.0) then
              estifm(na,kk)=0
            else
             ESTIFM(NA+2,KK)=0.
           endif
 6667     CONTINUE
          if (icyc.le.0) then
            f(na)=0
          else
            F(NA+2)=0.
          endif
        ENDIF
      ENDIF
  650 CONTINUE


      !EFa Dec06, keine Berechnung der RB Q für stationären Fall, deaktiviert nach Test
      if (icyc.le.0) then
        GO TO 1305
      endif

C-
C......Insert boundary flows
C-
      !EFa Nov06, Anpassung an 1D-Teschke-Elemente
      DO 1300 N=1,NCN,2
      M=NCON(N)
C-
C...... Test for and then retrieve stage flow constants
C-
      IF(NFIX(M)/1000.LT.13) GO TO 1300
      IRW=(N-1)*NDF+1
      IRH=IRW+2
      CX=COS(ALFA(M))
      SA=SIN(ALFA(M))
      VT=VEL(1,M)*CX+VEL(2,M)*SA
      vel(1,m)=spec(m,1)/ah(m)
      DO 1200 J=1,NEF
 1200 ESTIFM(IRh,J)=0.
      estifm(irh,irw)=1
 1300 CONTINUE
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
