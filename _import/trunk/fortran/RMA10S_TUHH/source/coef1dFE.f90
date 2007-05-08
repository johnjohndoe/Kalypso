!Last change:  K     8 May 2007    2:23 pm

!****************************************************************
!1D subroutine for calculation of elements, whose corner nodes are described with
!polynoms. This approach was documented in Teschke's phd-work and implemented first
!in the Fortran code of Flow1DFE. This approach was rewritten with usage of v and h
!as the independent variables instead of having Q and h
!
!The implementation was done by N. Schrage and E. Falke in 2006/2007 at the
!Institute of river and coastal engineering at the Technical University of Hamburg-Harburg
!
!It was done with friendly support of Prof. King, who is the developer of the program suite of RMA
!
!
!Hamburg April, 18th 2007
!****************************************************************


SUBROUTINE COEF1dFE(NN,NTX)
USE BLK10
USE BLK10MOD
USE BLK11MOD
USE BLKDRMOD
USE BLKSSTMOD
USE BLKSANMOD
USE PARAKalyps
USE PARAFlow1dFE
SAVE

!nis,feb07: Some definitions for internal use concerning the equation upsetting
REAL (KIND=8) :: aint1(1:4),     aint2(1:4)
REAL (KIND=8) :: inta1(1:4),     inta2(1:4)
real (KIND=8) :: daintdh1(1:4),  daintdh2(1:4)
REAL (KIND=8) :: d2aintdh1(1:4), d2aintdh2(1:4)
REAL (KIND=8) :: qschint1(1:4),  qschint2(1:4)
REAL (KIND=8) :: dqsintdh1(1:4), dqsintdh2(1:4)
real (KIND=8) :: d2qsidh1(1:4),  d2qsidh2(1:4)
REAL (KIND=8) :: thnn
REAL (KIND=8) :: IntahRand
REAL (KIND=8) :: sbot
REAL (KIND=8) :: zs, dzsdh, fzs, dfzsdh


REAL(KIND=8) :: froudeint(1:4)               
REAL(KIND=8) :: vflowint(1:4)                
REAL(KIND=8) :: dvintdx(1:4)                 
REAL(KIND=8) :: dvintdt(1:4)                 
REAL(KIND=8) :: hhint(1:4)                   
REAL(KIND=8) :: dhhintdx(1:4)                
REAL(KIND=8) :: dhintdt(1:4)                 
REAL(KIND=8) :: qqint(1:4)                   
REAL(KIND=8) :: dqintdt(1:4)                 
REAL(KIND=8) :: areaint(1:4)                 
REAL(KIND=8) :: dareaintdh(1:4)              
REAL(KIND=8) :: d2areaintdh(1:4)             
REAL(KIND=8) :: Intareaint(1:4)
REAL(KIND=8) :: daintdx(1:4)
REAL(KIND=8) :: d2aintdx(1:4)                
REAL(KIND=8) :: d2aidhdx(1:4)                
REAL(KIND=8) :: daintdt(1:4)                 
REAL(KIND=8) :: qschint(1:4)                 
REAL(KIND=8) :: dqsintdh(1:4)                
REAL(KIND=8) :: d2qsidh(1:4)                 
REAL(KIND=8) :: dqsintdx(1:4)                
REAL(KIND=8) :: d2qsidhdx(1:4)               
REAL(KIND=8) :: s0schint(1:4)                
REAL(KIND=8) :: sfwicint(1:4)                
REAL(KIND=8) :: sfint(1:4)                   
REAL(KIND=8) :: dsfintdh1(1:4)               
REAL(KIND=8) :: beiint(1:4)                  
REAL(KIND=8) :: dbeiintdh(1:4)               
REAL(KIND=8) :: d2beiintdh(1:4)              
REAL(KIND=8) :: dbeiintdx(1:4)               
REAL(KIND=8) :: d2beiintdhdx(1:4)            

REAL(KIND=8) :: dhdtaltzs(1:2)
REAL(KIND=8) :: hhalt(1:2)
REAL(KIND=8) :: dqdtaltzs(1:2)
REAL(KIND=8) :: dvdtaltzs(1:2)

REAL(KIND=8) :: pdif(1:2,0:3)
REAL(KIND=8) :: pbei(1:2,1:12)
REAL(KIND=8) :: Intah(1:2)
REAL(KIND=8) :: d2ahdh(1:2)
REAL(KIND=8) :: dqhdh(1:2)
REAL(KIND=8) :: d2qhdh(1:2)
REAL(KIND=8) :: sfnod(1:2)
REAL(KIND=8) :: sfwicht(1:2)
REAL(KIND=8) :: hdif(1:2)
REAL(KIND=8) :: bei(1:2)
REAL(KIND=8) :: froude(1:2)
REAL(KIND=8) :: dbeidh(1:2)
REAL(KIND=8) :: d2beidh(1:2)
REAL(KIND=8) :: dbeizdh(1:2)
REAL(KIND=8) :: d2beizdh(1:2)
REAL(KIND=8) :: dbeiodh(1:2)
REAL(KIND=8) :: d2beiodh(1:2)

REAL(KIND=8) :: FRN, FRNX
REAL(KIND=8) :: FEEAN, FEEBN, FEECN
REAL(KIND=8) :: FRNC

REAL(KIND=8) :: rho

!nis,feb07: testingvariables
REAL (KIND=8) :: deltax, deltvx, deltay, deltvy, deltaalpha
real (KIND=8) :: dirfact

!nis,may07: Sidflowterm
REAL (KIND=8) :: sidft

!local resulting velocities
REAL(KIND=8) :: vel_res(2)

INTEGER :: PolyTest
!estifm block-definition
INCLUDE 'BLKE.COM'
!weighting function etc. block-definition
!INCLUDE 'BLKH.COM'
COMMON/BLKC/ ATEMP(7,3),WAIT(7),AFACT(4),HFACT(4),SLOAD(2), AX(3,3),DNAL(3,4),XNAL(3,4)

COMMON F(80), XN(3),DNX(3),DNY(3),XM(2),DMX(2),DMY(2),XL(3),YL(3), &
& VX(3),VY(3),VDX(3),VDY(3),QFACT(3),QQFACT(3),ST(3),SDT(3),UBFC(3),XO(3),DOX(3),Q1(3)

DIMENSION NCON(20)
!CIPK oct02
COMMON /ICE2/ GSICE,GSQLW,QWLI(8),THKI(8)

!initializing
rho = 1000.0
sbot          = 0.0
sidft         = 0.0
init1: DO j = 1, 4
  hhint(j)       = 0.0
  dhhintdx(j)    = 0.0
  dhintdt(j)     = 0.0
  qqint(j)       = 0.0
  areaint(j)     = 0.0
  Intareaint(j)  = 0.0
  dareaintdh(j)  = 0.0
  d2areaintdh(j) = 0.0
  daintdx(j)     = 0.0
  d2aintdx(j)    = 0.0
  d2aidhdx(j)    = 0.0
  daintdt(j)     = 0.0
  qschint(j)     = 0.0
  dqsintdh(j)    = 0.0
  d2qsidh(j)     = 0.0
  dqsintdx(j)    = 0.0
  d2qsidhdx(j)   = 0.0
  s0schint(j)    = 0.0
  sfwicint(j)    = 0.0
  sfint(j)       = 0.0
  dsfintdh1(j)   = 0.0
  beiint(j)      = 0.0
  dbeiintdh(j)   = 0.0
  d2beiintdh(j)  = 0.0
  dbeiintdx(j)   = 0.0
  d2beiintdhdx(j)= 0.0
  froudeint(j)   = 0.0
  dqintdt(j)     = 0.0
  vflowint(j)    = 0.0
  dvintdx(j)     = 0.0
  dvintdt(j)     = 0.0
ENDDO init1
init2: do j = 1, 2
  dhdtaltzs(j) = 0.0
  hhalt(j)     = 0.0
  dqdtaltzs(j) = 0.0
  dvdtaltzs(j) = 0.0
end do init2
init3: DO i = 1, 2
  sfwicht(i)    = 1.0
  Intah(i)      = 0.0
  hdif(i)       = 0.0
  dqhdh(i)      = 0.0
  d2ahdh(i)     = 0.0
  d2qhdh(i)     = 0.0
  sfnod(i)      = 0.0
  bei(i)        = 0.0
  dbeidh(i)     = 0.0
  d2beidh(i)    = 0.0
  dbeizdh(i)    = 0.0
  d2beizdh(i)   = 0.0
  dbeiodh(i)    = 0.0
  d2beiodh(i)   = 0.0
  froude(i)     = 0.0
ENDDO init3



!initialice gravitation factor for unit system
IF (GRAV .LT. 32.)  THEN
  grav = GRAV
ELSE
  grav = GRAV/2.208
ENDIF

!C-
!C-.....ASSIGN PROPER COEFS.....
!C-

do i = 1, 3, 2
  n1 = nop(nn,i)
  !fix waterdepth for h-boundaries
  IF (MOD(NFIX(N1)/100,10) .EQ. 2) THEN
    vel(3,n1) = spec(n1,3)
  end if
  
  !Check, whether all necessary parameters are given
  !A-Polynom test
  PolyTest = 3
  ACheck: do j = 0, 12
    if (apoly(n1,j) /= 0.0) then
      PolyTest = PolyTest - 1
      exit ACheck
    endif
  ENDDO ACheck
  !QSch-Polynom test
  QCheck: do j = 0, 12
    if (qpoly(n1,j) /= 0.0) then
      PolyTest = PolyTest - 2
      exit Qcheck
    endif
  ENDDO QCheck
  !ERROR - messages
  if (PolyTest == 1) then
    CLOSE(75)
    OPEN(75,FILE='ERROR.OUT')
    WRITE(75,*) 'ERROR - area polynom is missing for node ', N1
    STOP  'STOP - area polynom is missing - see ERROR.DAT'
  ELSEIF (PolyTest == 2) then
    CLOSE(75)
    OPEN(75,FILE='ERROR.OUT')
    WRITE(75,*) 'ERROR - discharge polynom is missing for node ', N1
    STOP  'STOP - discharge polynom is missing - see ERROR.DAT '
  ELSEIF (PolyTest == 3) then
    CLOSE(75)
    OPEN(75,FILE='ERROR.OUT')
    WRITE(75,*) 'ERROR - area and discharge polynoms are missing for node ', N1
    STOP  'STOP - area and discharge polynoms are missing - see ERROR.DAT '
  endif
enddo

!Get the nodes of the element
N1 = NOP(NN,1)
N3 = NOP(NN,3)

!nis,may07: special testouput
if (nn == 211) then
  do i = 1, 3
    WRITE(*,'(a9,2(1x,i4),3(1x, f9.3))') 'Element: ', nn, nop(nn,i), (vel(j, nop(nn,i)), j=1, 3)
  end do
endif
!nis,may07: special testouput-


!****************************************************************
!nis,mar07,com: Calculate 'area' of element and coordinate things
TEL=AREA(NN)
AREA(NN)=0.

!cipk nov97
!      TVOL(NN)=0.

!getting local copy of corner node numbers
NCN = NCORN(NN)
DO N=1,NCN
  NCON(N)=NOP(NN,N)
enddo

!for 1D-2D-connection elements
IF(NCN .EQ. 5  .AND.  IMAT(NN) .LT. 900) NCN=3

!Calculation of local number of element degree of freedom and initalizing the residual vector and Jacobi-Matrix arrays
NEF=NCN*NDF
DO I=1,NEF
  F(I) = 0.0
  DO J=1,NEF
    ESTIFM(I,J) = 0.0
  enddo
enddo

!process on element direction
IF(NTX .EQ. 0) THEN
  N1=NCON(1)
  N2=NCON(3)
  !TH(NN) = ATAN2 (CORD(N2,2)-CORD(N1,2), CORD(N2,1) - CORD(N1,1))
  th(nn) = ATAN ((cord(n2,2)-cord(n1,2))/ (cord(n2,1)-cord(n1,1)))
  thnn   = ATAN ((cord(n2,2)-cord(n1,2))/ (cord(n2,1)-cord(n1,1)))
ENDIF

!getting angles
CXX=COS(TH(NN))
SAA=SIN(TH(NN))
!Set number of Gauss-points
NGP=4

!      NCNX=2
!C-
!C-.....COMPUTE LOCAL CORDS.....
!C-
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


  !mean velocity at node m
  vel_res(m) = vel(1,n)*COS(alfa(n)) + vel(2,n)*SIN(alfa(n))
  !WRITE(*,*) vel(1,n), vel(2,n)
  !WRITE(*,*) dx, dy
  !WRITE(*,*) vel_res(m)
  !pause
enddo

!turn elemt degrees of freedom into correct initial direction
!if (maxn == 1 .and. ntx == 1) then
!WRITE(*,*) 'v1x:', vel(1,n1), xl(3)
!WRITE(*,*) 'v1y:', vel(2,n1)
!WRITE(*,*) 'v2x:', vel(1,n3)
!WRITE(*,*) 'v2y:', vel(2,n3)
!  vel (1,n1) = ABS(vel (1,n1)) * xl(3) / sqrt( (xl(3))**2)
!  vel (1,n3) = ABS(vel (1,n3)) * xl(3) / sqrt( (xl(3))**2)
!  vel (2,n1) = ABS(vel (2,n1)) * yl(3) / sqrt( (yl(3))**2)
!  vel (2,n3) = ABS(vel (2,n3)) * yl(3) / sqrt( (yl(3))**2)
!WRITE(*,*) 'v1x:', vel(1,n1)
!WRITE(*,*) 'v1y:', vel(2,n1)
!WRITE(*,*) 'v2x:', vel(1,n3)
!WRITE(*,*) 'v2y:', vel(2,n3)
!
!  pause
!endif
!
!
!if (ntx == 1) then
!WRITE(*,*) 'El: ', nn
!WRITE(*,*) 'dx: ', cord(n1,1) - cord(n1,3)
!WRITE(*,*) 'dy: ', cord(n3,1) - cord(n3,3)
!WRITE(*,*) 'xl: ', xl(3)
!WRITE(*,*) 'yl: ', yl(3)
!WRITE(*,*) 'a1: ', alfa(n1)
!WRITE(*,*) 'a2: ', alfa(n3)
!WRITE(*,*) 'v1x:', vel(1,n1)
!WRITE(*,*) 'v1y:', vel(2,n1)
!WRITE(*,*) 'v2x:', vel(1,n3)
!WRITE(*,*) 'v2y:', vel(2,n3)
!WRITE(*,*) 'g1: ', atan(vel(2,n1)/vel(1,n1))
!WRITE(*,*) 'g2: ', ATAN(vel(2,n3)/vel(1,n3))
!pause
!endif

!updating length, if kilometres are given
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

!CIPK JAN03
EINA=EINX(NN)*CX+EINY(NN)*SA

!C-
!C-.....COMPUTE ELEMENT EQUATIONS.....
!C-
TFR=TEL/ABS(XL(3))


!CIPK MAY04 RESET ELEMENT INFLOW
IF(INOFLOW(NN) .EQ. 0) THEN
  SIDFQ=SIDF(NN)
ELSE
  SIDFQ=0.
ENDIF
!SIDFQQ=SIDF(NN)
!EFa,Apr07
sidft = sidfq
!-

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
hdif(1) = 0.0
hdif(2) = 0.0

!bedslope
if (kmx(n1) == 0.0 .and. kmx(n3) == 0.0) then
  sbot = (ao(n1) - ao(n3)) / (  (cord(n3,1) - cord(n1,1))*COS(thnn) + (cord(n3,2) - cord(n1,2))*SIN(thnn))
else
  sbot = (ao(n1) - ao(n3)) / xl(3)
endif

!flow coefficient calculation (e.g. Boussinesq-coefficient)
do k=0,12
  pbei(1,k) = 0.0
  pbei(2,k) = 0.0
end do
do j=0,3
  pdif(1,j) = 0.0
  pdif(2,j) = 0.0
end do

!init A(h)
ah(n1) = 0.0
ah(n3) = 0.0
!init QSch(h)
qh(n1) = 0.0
qh(n3) = 0.0
!init Intah(h)
Intah(1) = 0.0
Intah(2) = 0.0

do k=0, 12
  !A(h)
  ah(n1) = ah(n1) + apoly(n1,k) * vel(3,n1)**(k)
  ah(n3) = ah(n3) + apoly(n3,k) * vel(3,n3)**(k)
  !Int[(A(h)dh]
  Intah(1) = Intah(1) + apoly(n1,k) / (k+1) * vel(3,n1)**(k+1)
  Intah(2) = Intah(2) + apoly(n3,k) / (k+1) * vel(3,n3)**(k+1)
  !QSch(h)
  qh(n1) = qh(n1) + qpoly(n1,k) * vel(3,n1)**(k)
  qh(n3) = qh(n3) + qpoly(n3,k) * vel(3,n3)**(k)
end do

!Sf
sfnod(1) = sfwicht(1) * vel_res(1) * ah(n1) * ABS(vel_res(1) * ah(n1)) / (qh(n1)**2) * qgef(n1)
sfnod(2) = sfwicht(2) * vel_res(2) * ah(n3) * ABS(vel_res(2) * ah(n3)) / (qh(n3)**2) * qgef(n3)

if (ntx.eq.1) then

  !EFa Nov06, Testen ob schießen vorliegt
  froude(1) = vel_res(1) / sqrt(grav*h1)
  froude(2) = vel_res(2) / sqrt(grav*h3)
  if (froude(1).gt.1) then
    WRITE(*,*)'Schiessen an Knoten ',n1
  ELSEIF (froude(2).gt.1) then
    WRITE(*,*)'Schiessen an Knoten ',n3
  end if
  
  !nis,feb07,com: unsteady
  if (icyc.gt.0) then
!    if (maxn.eq.1) then
!      dhdtaltzs(1) = dhht(n1)
!      dhdtaltzs(2) = dhht(n3)
!      hht(n1) = vel(3,n1)
!      hht(n3) = vel(3,n3)
!    end if
    dhht(n1) = vdot(3,n1)
    dhht(n3) = vdot(3,n3)
!    dhht(n1) = 1.8 * (vel(3,n1) - hht(n1)) / delt + (1.0 - 1.8) * dhdtaltzs(1)
!    dhht(n3) = 1.8 * (vel(3,n3) - hht(n3)) / delt + (1.0 - 1.8) * dhdtaltzs(2)
!  end if


  !EFa Apr07, unsteady
!  if (icyc.gt.0) then
!    if (maxn.eq.1) then
!      dvdtaltzs(1) = dvvt(n1)
!      dvdtaltzs(2) = dvvt(n3)
!      vvt(n1) = vel_res(1)
!      vvt(n3) = vel_res(2)
!    end if
    dvvt(n1) = vdot(1,n1)*COS(alfa(n1)) + vdot(2,n1)*SIN(alfa(n1))
    dvvt(n3) = vdot(1,n3)*COS(alfa(n3)) + vdot(2,n3)*SIN(alfa(n3))
!    dvvt(n1) = 1.8 * (vel_res(1) - vvt(n1)) / delt + (1.0 - 1.8) * dvdtaltzs(1)
!    dvvt(n3) = 1.8 * (vel_res(2) - vvt(n3)) / delt + (1.0 - 1.8) * dvdtaltzs(2)
  end if
    !if (maxn == 1) then
    !WRITE(*,*) 'Element ', nn
    !WRITE(*,*) dvvt(n1), vdot(1, n1)
    !WRITE(*,*) dvvt(n3), vdot(1, n3)
    !WRITE(*,*) vvt(n1), SQRT(vold(1, n1)**2 + vold(2,n1)**2)
    !WRITE(*,*) vvt(n3), SQRT(vold(1, n3)**2 + vold(2,n3)**2)
    !pause
    !endif
  
  !EFa Nov06, Energiestrombeiwert
  if (beient.eq.1) then       
    do k=0,12
      pbei(1,k) = alphapk(n1,k)
      pbei(2,k) = alphapk(n3,k)
    end do
    do j=0,3
      pdif(1,k) = alphad(n1,k)
      pdif(2,k) = alphad(n3,k)
    end do
    hdif(1) = alphah(n1)
    hdif(2) = alphah(n3)
  endif
  
  !EFa Nov06, Impulsstrombeiwert
  IF (beient.eq.2) then       
    do k=0,12
      pbei(1,k)=betapk(n1,k)
      pbei(2,k)=betapk(n3,k)
    end do
    do j=0,3
      pdif(1,k)=betad(n1,k)
      pdif(2,k)=betad(n3,k)
    end do
    hdif(1)=betah(n1)
    hdif(2)=betah(n3)
  end if
  
  !init dA(h)/dh
  dahdh(n1) = 0.0
  dahdh(n3) = 0.0
  !init dQSch(h)/dh
  dqhdh(1) = 0.0
  dqhdh(2) = 0.0

  do k=1,12
    !dA(h)/dh
    dahdh(n1) = dahdh(n1) + (k) * apoly(n1,k) * h1**(k-1)
    dahdh(n3) = dahdh(n3) + (k) * apoly(n3,k) * h3**(k-1)
    !dQSch(h)/dh
    dqhdh(1) = dqhdh(1) + (k) * qpoly(n1,k) * h1**(k-1)
    dqhdh(2) = dqhdh(2) + (k) * qpoly(n3,k) * h3**(k-1)
  end do
  
  !init d2A(h)/dh2
  d2ahdh(1) = 0.0
  d2ahdh(2) = 0.0
  !init d2QSch(h)/dh2
  d2qhdh(1) = 0.0
  d2qhdh(2) = 0.0
  do k=2,12
    !d2A(h)/dh2
    d2ahdh(1) = d2ahdh(1) + (k-1) * (k) * apoly(n1,k) * h1**(k-2)
    d2ahdh(2) = d2ahdh(2) + (k-1) * (k) * apoly(n3,k) * h3**(k-2)
    !d2QSch(h)/dh2
    d2qhdh(1) = d2qhdh(1) + (k-1) * (k) * qpoly(n1,k) * h1**(k-2)
    d2qhdh(2) = d2qhdh(2) + (k-1) * (k) * qpoly(n3,k) * h3**(k-2)
  end do
  
  !EFa Nov06, Bereich für Wasserspiegel Bordvoll (nur Hauptgerinne)
  if (h1.le.hbordv(n1)) then
    bei(1)      = 1.0
    dbeidh(1)   = 0.0
    d2beidh(1)  = 0.0
    pbei(1,1)   = 1.0
    do k = 0, 12
      pbei(1,k) = 0.0
    end do
  end if
  
  if (h3.le.hbordv(n3)) then
    bei(2)      = 1.0
    dbeidh(2)   = 0.0
    d2beidh(2)  = 0.0
    pbei(2,1)   = 1.0
    do k = 0, 12
      pbei(2,k) = 0.0
    end do
  end if
  
  !EFa Nov06, Bereich für Höhen zwischen dem unteren Grenzwert als Wasserspiegel Bordvoll
  !           und dem oberen Grenzwert des Übergangspolynoms
  if (h1.gt.hbordv(n1).and.h1.lt.hdif(1)) then
    bei(1)      = 0.0
    do k = 0, 3
      bei(1)    = bei(1) + pdif(1,k) * h1**(k)
      pbei(1,k) = pdif(1,k)
    end do
    do k = 4, 12
      pbei(1,k) = 0.0
    end do
    !1st derivative
    dbeizdh(1)  = 0.0
    do k = 1, 3
      dbeizdh(1) = dbeizdh(1) + (k) * pdif(1,k) * h1**(k-1)
    end do
    !2nd derivative
    d2beizdh(1)=0
    do k = 2, 3
      d2beizdh(1) = d2beizdh(1) + (k-1)*(k) * pdif(1,k) * h1**(k-2)
    end do
    dbeidh(1)  = dbeizdh(1)
    d2beidh(1) = d2beizdh(1)
  end if
  
  if (h3.gt.hbordv(n3).and.h3.lt.hdif(2)) then
    bei(2)=0
    do k = 0, 3
      bei(2)    = bei(2) + pdif(2,k) * h3**(k)
      pbei(2,k) = pdif(2,k)
    end do
    do k = 4, 12
      pbei(2,k)=0
    end do
    !1st derivative
    dbeizdh(2)=0
    do k = 1, 3
      dbeizdh(2)  = dbeizdh(2) + (k) * pdif(2,k) * h3**(k-1)
    end do
    !2nd derivative
    d2beizdh(2)=0
    do k = 2, 3
      d2beizdh(2) = d2beizdh(2) + (k-1)*(k) * pdif(2,k) * h3**(k-2)
    end do
    dbeidh(2)  = dbeizdh(2)
    d2beidh(2) = d2beizdh(2)
  end if
  
  !EFa Nov06, Bereich für Höhen über dem oberen Grenzwert des Übergangspolynoms
  if (h1.ge.hdif(1)) then
    bei(1)=0
    do k = 0, 12
      bei(1) = bei(1) + pbei(1,k) * h1**(k)
    end do
    !1st derivative
    dbeiodh(1)=0
    do k = 1, 12
      dbeiodh(1) = dbeiodh(1) + (k) * pbei(1,k) * h1**(k-1)
    end do
    !2nd derivative
    d2beiodh(1)=0
    do k = 2, 12
      d2beiodh(1) = d2beiodh(1) + (k-1)*(k) * pbei(1,k) * h1**(k-2)
    end do
    dbeidh(1)=dbeiodh(1)
    d2beidh(1)=dbeiodh(1)
  end if
  
  if (h3.ge.hdif(2)) then
    bei(2)=0
    do k = 0, 12
      bei(2) = bei(2) + pbei(2,k) * h3**(k)
    end do
    !1st derivative
    dbeiodh(2)=0
    do k = 1, 12
      dbeiodh(2) = dbeiodh(2) + (k) * pbei(2,k) * h3**(k-1)
    end do
    !2nd derivative
    d2beiodh(2)=0
    do k = 2, 12
      d2beiodh(2) = d2beiodh(2) + (k-1)*(k) * pbei(2,k) * h3**(k-2)
    end do
    dbeidh(2)=dbeiodh(2)
    d2beidh(2)=dbeiodh(2)
  end if
  
  !EFa Nov06, wenn ohne Beiwerte gerechnet werden soll oder die Eingabe von beient fehlerhaft ist
  if ((beient .eq. 0) .OR. (beient.ne.2).AND.(beient.ne.1)) then
    bei(1)      = 1.0
    dbeidh(1)   = 0.0
    d2beidh(1)  = 0.0
    pbei(1,1)   = 1.0
    do k = 1, 12
      pbei(1,k) = 0.0
    end do
    pdif(1,1)   = 1.0
    do k = 1, 3
      pdif(1,k) = 0.0
    end do
    bei(2)      = 1.0
    dbeidh(2)   = 0.0
    d2beidh(2)  = 0.0
    pbei(1,3)   = 1.0
    do k = 1, 12
      pbei(2,k) = 0.0
    end do
    pdif(2,1)   = 1.0
    do k = 1, 3
      pdif(2,k) = 0.0
    end do
  endif

ENDIF !ntx=1

!********************************************************************************************************************************************
!GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP
!********************************************************************************************************************************************
Gaussloop: DO I = 1, NGP

  TEMP=(DNAL(2,I)*XL(2)+DNAL(3,I)*XL(3))

!C-
!C......DEFINE SHAPE FUNCTIONS
!C-
  !quadratic shape-functions
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
    DO J=1,3
      DNX(J)=DNX(J)/TFR
    enddo
  ENDIF
  !linear shape-functions
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

  ams = amw !* rho
  !WRITE(*,*) ams
  !WRITE(*,*) ABS(xl(3)) * hfact(i) * 0.5
  !pause


  IF(NTX .EQ. 0) CYCLE gaussloop
!cipk nov97
  IF (NTX .EQ. 3) GO TO 276


  !primary variables h and v
  !h at GP
  hhint(i)    = h1     * xm(1)      + h3     * xm(2)
  !dh/dx at GP
  dhhintdx(i) = h1     * dmx(1)     + h3     * dmx(2)
  !dh/dt at GP
  dhintdt(i)  = xm(1)  * dhht(n1)    + xm(2)  * dhht(n3)

  !v at GP
  vflowint(i) = xm(1)  * vel_res(1) + xm(2)  * vel_res(2)
  !dv/dx at GP
  dvintdx(i)  = dmx(1) * vel_res(1) + dmx(2) * vel_res(2)
  !dv/dt at GP
  dvintdt(i)  = xm(1)  * dvvt(n1)    + xm(2)  * dvvt(n3)


  !secondary variables: areaelem(nn) means area and its derivatives

  !A at GP
  areaint(i) = 0.0
  aint1(i)   = 0.0
  aint2(i)   = 0.0
  do j = 0, 12
    aint1(i) = aint1(i) + apoly(n1, j) * hhint(i)**(j)
    aint2(i) = aint2(i) + apoly(n3, j) * hhint(i)**(j)
  enddo
  areaint(i) = xm(1) * aint1(i) + xm(2) * aint2(i)
  !Integration of A over h at GP
  Intareaint(i) = 0.0
  inta1(i)      = 0.0
  inta2(i)      = 0.0
  do j = 0, 12
    inta1(i) = inta1(i) + apoly(n1,j)/(j+1) * hhint(i)**(j+1)
    inta2(i) = inta2(i) + apoly(n3,j)/(j+1) * hhint(i)**(j+1)
  end do
  Intareaint(i) = xm(1) * inta1(i) + xm(2) * inta2(i)
  !dA/dt at GP
  !Equation 8.28, page 65
  dareaintdh(i) = 0.0
  daintdh1(i)   = 0.0
  daintdh2(i)   = 0.0
  do j = 1, 12
   daintdh1(i) = daintdh1(i) + (j) * apoly(n1,j) * hhint(i)**(j-1)
   daintdh2(i) = daintdh2(i) + (j) * apoly(n3,j) * hhint(i)**(j-1)
  end do
    dareaintdh(i) = xm(1) * daintdh1(i) +  xm(2) * daintdh2(i)

  !d2A/dh2 at GP
  d2areaintdh(i) = 0.0
  d2aintdh1(i)   = 0.0
  d2aintdh2(i)   = 0.0
  do j = 2, 12
    d2aintdh1(i) = d2aintdh1(i) + (j-1) * (j) * apoly(n1, j) * hhint(i)**(j-2)
    d2aintdh2(i) = d2aintdh2(i) + (j-1) * (j) * apoly(n3, j) * hhint(i)**(j-2)
  end do
    d2areaintdh(i) = xm(1) * d2aintdh1(i) +  xm(2) * d2aintdh2(i)

  !dA/dx at GP
  daintdx(i)  = dmx(1) * (aint1(i) + h1 * dareaintdh(i)) + dmx(2) * (aint2(i) + h3 * dareaintdh(i))
  !d2A/dx2 at GP
  d2aintdx(i) = dhhintdx(i) * (2.0 * dmx(1) * daintdh1(i) +  xm(1) * d2aintdh1(i) * dhhintdx(i)  &
  &                           +2.0 * dmx(2) * daintdh2(i) +  xm(2) * d2aintdh2(i) * dhhintdx(i))
  !d2A/dhdx at GP
  d2aidhdx(i) = dmx(1) *  daintdh1(i) +  xm(1) * d2aintdh1(i) * dhhintdx(i)  &
  &            +dmx(2) *  daintdh2(i) +  xm(2) * d2aintdh2(i) * dhhintdx(i)
  !dA/dt at GP
  daintdt(i)  = (xm(1) * daintdh1(i) + xm(2) * daintdh2(i)) * dhintdt(i)

  !Q at GP
  qqint(i)    = vflowint(i) * areaint(i)

  !supercritical or subcritical flow
  froudeint(i)=qqint(i)/(areaint(i)*sqrt(grav*hhint(i)))
  if (froudeint(i).gt.1) then
    WRITE(*,*)'Schiessen in Element', nn
  end if

  !secondary variables: schkelem(nn) means reference-discharge and its derivatives

  !QSch at GP
  qschint(i)    = 0.0
  qschint1(i)   = 0.0
  qschint2(i)   = 0.0
  do j = 0, 12
    qschint1(i) = qschint1(i) + qpoly(n1,j) * hhint(i)**(j)
    qschint2(i) = qschint2(i) + qpoly(n3,j) * hhint(i)**(j)
  end do
  qschint(i)    = xm(1) * qschint1(i) + xm(2) * qschint2(i)
  !dQSch/dh at GP
  dqsintdh(i)    = 0.0
  dqsintdh1(i)   = 0.0
  dqsintdh2(i)   = 0.0
  do j = 1, 12
    dqsintdh1(i) = dqsintdh1(i) + (j) * qpoly(n1,j) * hhint(i)**(j-1)
    dqsintdh2(i) = dqsintdh2(i) + (j) * qpoly(n3,j) * hhint(i)**(j-1)
  end do
  dqsintdh(i)    = xm(1) * dqsintdh1(i) + xm(2) * dqsintdh2(i)
  !d2QSch/dh2 at GP
  d2qsidh(i)   = 0.0
  d2qsidh1(i)  = 0.0
  d2qsidh2(i)  = 0.0
  do j = 2, 12
  d2qsidh1(i)  = d2qsidh1(i) + (j-1) * (j) * qpoly(n1,j) * hhint(i)**(j-2)
  d2qsidh2(i)  = d2qsidh2(i) + (j-1) * (j) * qpoly(n3,j) * hhint(i)**(j-2)
  end do
  d2qsidh(i)   = xm(1) * d2qsidh1(i) + xm(2) * d2qsidh2(i)
  !dQSch/dx at GP
  dqsintdx(i)  = dmx(1) * (qschint1(i) + h1 * dqsintdh(i)) + dmx(2) * (qschint2(i) + h3 * dqsintdh(i))
  !d2QSch/dhdx at GP
  d2qsidhdx(i) = dmx(1) * dqsintdh1(i) + xm(1) * d2qsidh1(i) * dhhintdx(i) &
  &             +dmx(2) * dqsintdh2(i) + xm(2) * d2qsidh2(i) * dhhintdx(i)

  !secondary variables: fricelem(nn) means friction-slope and its derivatives

  !Sf,0 at GP
  s0schint(i) = qgef(n1) * xm(1) + qgef(n3) * xm(2)
  !weighting factor
  sfwicint(i) = sfwicht(1) * xm(1) + sfwicht(2) * xm(2)
  !Sf at GP
  sfint(i) = sfwicint(i) * s0schint(i) * vflowint(i) * ABS(vflowint(i)) * areaint(i)**2 / qschint(i)**2
  !dSf/dh at GP
  dsfintdh1(i) = s0schint(i) * vflowint(i) * ABS(vflowint(i))                       &
  &                *(QSchint(i)**2 * 2 * areaint(i) * dareaintdh(i) - areaint(i)**2 &
  &                * 2 * Qschint(i) * dqsintdh(i)) / Qschint(i)**4

  !The following has to be resetted (April 2007)!!!
  !secondary variables: beiwelem(nn) means flow-coefficient (e.g. Boussinesq) and its derivatives

  !beta at GP
  beiint(i) = xm(1) * bei(1) + xm(2) * bei(2)
  !dbeta/dh at GP
  dbeiintdh(i) = xm(1) * dbeiodh(1) + xm(2) * dbeiodh(2)
  !d2beta/dh2 at GP
  d2beiintdh(i) = xm(1) * d2beiodh(1) + xm(2) * d2beiodh(2)
  !dbeta/dx at GP
  dbeiintdx(i) = dmx(1) * bei(1) + xm(1) * dbeiodh(1) * dhhintdx(i) &
  &             +dmx(2) * bei(2) + xm(2) * dbeiodh(2) * dhhintdx(i)
  !d2beta/dx2
  d2beiintdhdx(i) = dmx(1) * dbeiodh(1) + xm(1) * d2beiodh(1) * dhhintdx(i) &
  &                +dmx(2) * dbeiodh(2) + xm(2) * d2beiodh(2) * dhhintdx(i)

  !*****************************************************************************************************************************************
  !RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS
  !  EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS
  !*****************************************************************************************************************************************

  !*****************************************************************************************************************************************
  !momentum equation   momentum equation   momentum equation   momentum equation   momentum equation   momentum equation   momentum equation
  !*****************************************************************************************************************************************

  FRN  = 0.0
  FRNX = 0.0

  FRN =                                                          &
         !Term b
    &    + vflowint(i)**2 * areaint(i) * dbeiintdx(i)            &
         !Term c
    &    + beiint(i) * vflowint(i)**2 * daintdx(i)               &
         !Term d
    &    + 2 * beiint(i) * vflowint(i) * areaint(i) * dvintdx(i) &
         !Term e
!nis,may07: test for integration by parts
    &    + grav * areaint(i) * dhhintdx(i)                       &
      !Term E: hydrostatic Term
!2  &  - grav * dhhintdx(i) * (dareaintdh(i) / 2.0  - areaint(i)) &
!-
         !Term f
    &    + grav * areaint(i) * sfint(i)                          &
         !Term g
    &    - grav * areaint(i) * sbot                              &
         !Term H
    &    + sidft * vflowint(i)

!nis,may07: Test for integration by parts
!2  FRNX =                                                               &
!2  !Term E
!2  &  - grav * areaint(i) * hhint(i) / 2.0
!-

  if (icyc > 0) then
  FRN = FRN                                                      &
         !Term a
    &    + vflowint(i)*daintdt(i) + areaint(i)*dvintdt(i)
  end if

  do l = 1, 2
    !equation number
    ia = 1 + (2 * ndf) * (l-1)
    !equation
    !f(ia) = f(ia) - hfact(i) * ABS(xl(3)) / 2.0 *    &
    f(ia) = f(ia) - ams *                 &
      &     (xm(l) * FRN + dmx(l) * FRNX)
  enddo

  !**********************************************************************
  !testoutput  testoutput  testoutput  testoutput  testoutput  testoutput
  !**********************************************************************
  if (nn < 0) then
    WRITE(*,*) 'Term A: ', (vflowint(i)*daintdt(i) + areaint(i)*dvintdt(i))
    WRITE(*,*) 'Term B: ', + vflowint(i)**2 * areaint(i) * dbeiintdx(i)
    WRITE(*,*) 'Term C: ', + beiint(i) * vflowint(i)**2 * daintdx(i)
    WRITE(*,*) 'Term D: ', + 2 * beiint(i) * vflowint(i) * areaint(i) * dvintdx(i)
!Version without natural boundary condition
    WRITE(*,*) 'Term E: ',  + grav * areaint(i) * dhhintdx(i)
!Version WITH natural boundary condition
!2    WRITE(*,*) 'Term E-1: ', - grav * dhhintdx(i) * (dareaintdh(i) / 2.0  - areaint(i))
!2    WRITE(*,*) 'Term E-2: ', - grav * areaint(i) * hhint(i) / 2.0
!-
    WRITE(*,*) 'Term F: ',  + grav * areaint(i) * sfint(i)
    WRITE(*,*) 'Term G: ', - grav * areaint(i) * sbot
    WRITE(*,*) 'Term H: ', + sidft * vflowint(i)
    do l = 1, 2
      ia = 1 + (2 * ndf) * (l-1)
      WRITE(*,*) 'Impulsintegral', f(ia)
      WRITE(*,*) 'Wichtungsterm: ', xm(l) * ams
      WRITE(*,*) 'Einzelterme Gauß: ', i
    enddo
    pause
  endif

  !*********************************************************************************************************************************
  !continuity equation   continuity equation   continuity equation   continuity equation   continuity equation   continuity equation
  !*********************************************************************************************************************************
  FRNC = 0.0

  FRNC =                                 &
          !Term B
    &     + areaint(i) * dvintdx(i)      &
          !Term C
    &     + vflowint(i) * daintdx(i)     &
          !Term D
    &     - sidft

  if (icyc > 0) then
    FRNC = FRNC                          &
          !Term A
    &     + daintdt(i)

  end if

  do l = 1, 2
    !equation number
    ia = 3 + (2 * ndf) * (l - 1)
    !equation
    !f(ia) = f(ia) - xm(l) * hfact(i) * ABS(xl(3)) / 2.0 * FRNC
    f(ia) = f(ia) - xm(l) * ams * FRNC
  enddo

  !*********************************************************************************************************************************
  !DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES
  !      DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES
  !*********************************************************************************************************************************

  !EFa Nov06, Sprung an den Beginn der Gauss-Schleife für junction-Elemente
  !IF(MOD(immt,100).gt.90) GO TO 500

  !*********************************************************************************************************************************
  !MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH
  !*********************************************************************************************************************************
  FEEAN = 0.0
  FEEBN = 0.0
  FEECN = 0.0

  FEEAN =                                                                                          &
          !Term B
    &   + vflowint(i)**2 * (dareaintdh(i) * dbeiintdh(i) + areaint(i) * d2beiintdhdx(i))           &
          !Term C
    &   + vflowint(i)**2 * (dbeiintdh(i) * daintdx(i) + beiint(i) * d2aidhdx(i))                   &
          !Term D
    &   + 2.0 * vflowint(i) * dvintdx(i) * (beiint(i) * dareaintdh(i) + areaint(i) * dbeiintdh(i)) &
!nis,may07: Test for integration by parts
          !Term E
    &   + grav * dareaintdh(i) * dhhintdx(i)                                                       &
!2      & - 0.5 * grav * dhhintdx(i) * d2areaintdh(i) * hhint(i)                                   &
!-
          !Term F
    &   + grav * (areaint(i) * dsfintdh1(i) + sfint(i) * dareaintdh(i))                            &
          !Term G
    &   - grav * sbot * dareaintdh(i)
  if (icyc > 0) then
    FEEAN = FEEAN                    &
          !Term A1
    &   + dareaintdh(i) * dvintdt(i) &
          !Term A2
    &   + dareaintdh(i) * dhintdt(i)
  ENDIF

  FEEBN =                                                     &
          !Term B
    &   + vflowint(i)**2 * areaint(i) * dbeiintdh(i)          &
          !Term C
    &   + vflowint(i)**2 * beiint(i) * dareaintdh(i)          &
!nis,may07: Test for integration by parts
          !Term E
    &   + grav * areaint(i)
!2      & - grav * (hhint(i) * dareaintdh(i) - areaint(i))
!-

!nis,may07: Test for integration by parts
!2  FEECN =                                                &
!2      !Term E
!2      & - 0.5 * grav * (hhint(i) * dareaintdh(i) + areaint(i))
!-

  do l = 1, 2
    !line no.
    ia = 1 + (2 * ndf) * (l - 1)
    do c = 1, 2

      !column no.
      ib = 3 + (2 * ndf) * (c - 1)

      estifm(ia,ib) = estifm(ia,ib)                &
         !weighting function
      !&  + xm(l) * hfact(i) * ABS(xl(3)) / 2.0 *   &
      &  + xm(l) * ams *                           &
      &  (xm(c) * FEEAN + dmx(c) * FEEBN)          &
      !&  + dmx(l) * hfact(i) * ABS(xl(3)) / 2.0 *  &
      &  + dmx(l) * ams *                          &
      &  xn(c) * FEECN
    enddo
  enddo

  !**********************************************************************
  !testoutput  testoutput  testoutput  testoutput  testoutput  testoutput
  !**********************************************************************
  do l = 1, 2
    if (nn <- 10) then
      WRITE(*,*) '*****************************'
      WRITE(*,*) 'dF/dh'
      WRITE(*,*) estifm(ia,ib), ia, ib, i
      WRITE(*,*) xm(l) * ams , xm(l), ams, i
      WRITE(*,*) '*****************************'
    endif
  end do
  if (nn < 0) then
    !Direktterme FEEAN * xm(c)
    !Term B
    WRITE(*,*) 'Term B: ', + vflowint(i)**2 * (dareaintdh(i) * dbeiintdh(i) + areaint(i) * d2beiintdhdx(i)) * xm(c)
    !Term C
    WRITE(*,*) 'Term C: ', + vflowint(i)**2 * (dbeiintdh(i) * daintdx(i) + beiint(i) * d2aidhdx(i)) * xm(c)
    !Term D
    WRITE(*,*) 'Term D: ', + 2.0 * vflowint(i) * dvintdx(i) * (beiint(i) * dareaintdh(i) + areaint(i) * dbeiintdh(i)) * xm(c)
    !Term E
    WRITE(*,*) 'Term E: ', + grav * dareaintdh(i) * dhhintdx(i) * xm(c)
    !Term F
    WRITE(*,*) 'Term F: ', + grav * (areaint(i) * dsfintdh1(i) + sfint(i) * dareaintdh(i)) * xm(c)
    !Term G
    WRITE(*,*) 'Term G: ', - grav * sbot * dareaintdh(i) * xm(c)
    !Ableitungsterme FEEBN * dmx(c)
    !Term B
    WRITE(*,*) 'Term B: ', + vflowint(i)**2 * areaint(i) * dbeiintdh(i) * dmx(c)
    !Term C
    WRITE(*,*) 'Term C: ', + vflowint(i)**2 * beiint(i) * dareaintdh(i) * dmx(c)
    !Term E
    WRITE(*,*) 'Term E: ', + grav * areaint(i) * dmx(c)
  endif



  !**************************************************************************************************************************
  !MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY
  !**************************************************************************************************************************
  FEEAN = 0.0
  FEEBN = 0.0

  FEEAN =                                                   &
          !Term B
    &   + 2.0 * areaint(i)* vflowint(i) * dbeiintdx(i)      &
          !Term C
    &   + 2.0 * vflowint(i) * beiint(i) * daintdx(i)        &
          !Term D
    &   + 2.0 * beiint(i) * areaint(i) * dvintdx(i)         &
          !Term F
    &   + 2.0 * grav * areaint(i) * sfint(i) / vflowint(i)  &
          !Term H
    &   + sidft
  if (icyc > 0) then
    FEEAN = FEEAN    &
            !Term A
    &   + daintdt(i)
  end if

  FEEBN =                                                   &
            !Term D
    &   + 2.0 * beiint(i) * areaint(i) * vflowint(i)

  do l = 1, 2
    !line no.
    ia = 1 + (2 * ndf) * (l - 1)
    do c = 1, 2
      !column no.
      ib = 1 + (2 * ndf) * (c - 1)
      !equation
      estifm(ia,ib) = estifm(ia,ib)                 &
            !Wichtungsfunktion
      !&     + xm(l) * hfact(i) * ABS(xl(3)) / 2.0 * &
      &     + xm(l) * ams *                         &
            !Term B
      &     (xm(c) * FEEAN + dmx(c) * FEEBN)
    enddo
  enddo

  !**********************************************************************
  !testoutput  testoutput  testoutput  testoutput  testoutput  testoutput
  !**********************************************************************
  if (nn == -10) then
    do l = 1, 2
        WRITE(*,*) '*****************************'
        WRITE(*,*) 'dF/ dv'
        WRITE(*,*) estifm(ia,ib), ia, ib, i
        WRITE(*,*) xm(l) * ams , xm(l), ams, i
        WRITE(*,*) '*****************************'
    end do
  endif

  !*********************************************************************************************************************
  !CONTINUITY over DEPTH   CONTINUITY over DEPTH   CONTINUITY over DEPTH   CONTINUITY over DEPTH   CONTINUITY over DEPTH
  !*********************************************************************************************************************
  EA = 0.0
  EB = 0.0

  EA =                                &
         !Term C
    &  + dareaintdh(i) * dvintdx(i)   &
         !Term B
    &  + vflowint(i) * d2aidhdx(i)
  if (icyc > 0) then
    EA = EA                           &
    &  + dareaintdh(i) * dhintdt(i)

  end if
  EB =                                &
       !Term B
    &  + vflowint(i) * dareaintdh(i)

  do l = 1, 2
    !line no.
    ia = 3 + (2 * ndf) * (l - 1)
    do c = 1, 2
      !column no.
      ib = 3 + (2 * ndf) * (c - 1)

      estifm(ia,ib) = estifm(ia,ib)                    &
            !weighting function
      !&     + xm(l) * hfact(i) * ABS(xl(3))/2 *        &
      &     + xm(l) * ams *                            &
      &     ( xm(c) * EA + dmx(c) * EB)
    end do
  end do

  !************************************************************************************************************************************
  !CONTINUITY over VELOCITY   CONTINUITY over VELOCITY   CONTINUITY over VELOCITY   CONTINUITY over VELOCITY   CONTINUITY over VELOCITY
  !************************************************************************************************************************************
  EA = 0.0
  EB = 0.0

  EA =               &
       !Term C
    & + daintdx(i)
  EB =               &
         !Term B
    & + areaint(i)

  do l = 1, 2
    !line no.
    ia = 3 + (2 * ndf) * (l - 1)
    do c = 1, 2
      !column no.
      ib = 1 + (2 * ndf) * (c - 1)

      estifm(ia,ib) = estifm(ia,ib)             &
            !weighting function
      !&     + xm(l) * hfact(i) * ABS(xl(3))/2 * &
      &     + xm(l) * ams *                     &
      &     (xm(c) * EA + dmx(c) * EB)
    end do
  end do

!*****************************************************************************************************************************************
!END OF EQUATION FORMATION   END OF EQUATION FORMATION   END OF EQUATION FORMATION   END OF EQUATION FORMATION   END OF EQUATION FORMATION
!*****************************************************************************************************************************************


 276  CONTINUE

  !TVOL(NN)=TVOL(NN)+AMW

ENDDO gaussloop

IF(NTX .EQ. 0) RETURN
IF(NTX .EQ. 3) RETURN

!IF(MOD(IMMT,100) .GT. 90) GO TO 1305

!Boundary Conditions - Waterdepth H
HBCAssign: DO L=1, NCN, 2
  N1=NCON(L)

  IF(MOD(NFIX(N1)/100,10) .EQ. 2) THEN

    NA = (L-1) * NDF + 1
    do iii=1, nef
      estifm (na, iii) = 0.0
    enddo

    estifm(na, na + 2) = 1.0
    f(na)              = 0.0
!2    NA = (L-1) * ndf + 1
!2
!2    ah(n1) = 0.0
!2    do k = 0, 12
!2      ah(n1) = ah(n1) + apoly(n1,k) * spec(n1,3)**(k)
!2    end do
!2    ppl   = grav * ah(n1) * qfact(l) * rho
!2    if (l == 1) ppl = -ppl
!2    f(na) = f(na) - ppl * (spec (n1,3) - vel (3,n1) / 2.)
!2!    estifm (na, na+2) = estifm (na, na+2) - ppl / 2.
!2    estifm (na, na+2) = estifm (na, na+2) - ppl / 2.
!2
!2    WRITE(*,*) - ppl * (spec (n1,3) - vel (3,n1) / 2.)
!2    WRITE(*,*) - ppl/ 2.0
!2    WRITE(*,*) vel(3,n1), spec(n1,3)
!2    pause

  ELSEIF (IBN(N1) .GE. 3) THEN
    IF (NREF(N1) .EQ. 0) THEN
      NA = (L-1) * NDF + 1
      DO KK=1,NEF
        ESTIFM (NA,KK) = 0.
      END do
      F(NA) = 0.
    ENDIF
  ENDIF
ENDDO HBCAssign

!Boundary conditions - DISCHARGE Q
QBCAssign: DO N=1, NCN, 2
  M=NCON(N)

  !if no BC for Q then cycle
  IF(NFIX(M)/1000.LT.13) CYCLE QBCAssign

  !line of degree of freedom (velocity)
  IRW = (N-1) * NDF + 3
  !line of degree of freedom (depth)
  IRH = IRW + 2

  !cosinus and sinus of nodal direction angle
  CX=COS(ALFA(M))
  SA=SIN(ALFA(M))

  !actual velocity
  VT=VEL(1,M)*CX+VEL(2,M)*SA

  !all other entrees are zero
  DO J=1,NEF
    ESTIFM(IRW,J) = 0.
  ENDDO

  !install new boundary condition values
  ESTIFM(IRW,IRW-2) = ah(m) * xl(3)/ ABS(xl(3)) * dirfact !* area(nn)
  ESTIFM(IRW,IRH-2) = dahdh(m) * vt * xl(3)/ ABS(xl(3)) * dirfact !* area(nn)
  F(IRW)          = (SPEC(M,1) - VT * ah(m)) * xl(3)/ ABS(xl(3)) * dirfact !* area(nn)
ENDDO QBCAssign

!Correction for Coupling
!CouplingCorrection: do l = 1, ncn, 2
!  m = ncon(l)
!
!  if (l==1) p = 1
!  if (l==3) p = 2
!
!  do i = 1, MaxLT
!    !WRITE(*,*) 'Kopplungstest: ', (translines(i,j), j=1, 3), maxlt
!    !pause
!    if (m == TransLines(i,3)) then
!      fzs = 0.0
!      dfzsdh = 0.0
!      zs = 0.0
!      dzsdh = 0.0
!      do j = 0, 12
!        fzs    = fzs    + apoly(m, j) * j / (j+1) * vel(3,m)**(j+1)
!        dfzsdh = dfzsdh + apoly(m, j) * j * vel(3,m)**(j)
!      ENDDO
!      zs = fzs / ah(m)
!      dzsdh = (ah(m) * dfzsdh - dahdh(m) * fzs) / ah(m)**2
!
!      WRITE(*,*) '     fzs: ', fzs
!      WRITE(*,*) '  dfzsdh: ', dfzsdh
!      WRITE(*,*) '      zs: ', zs
!      WRITE(*,*) '   dzsdh: ', dzsdh
!      WRITE(*,*)    'ah(m): ', ah(m)
!      WRITE(*,*) 'dahdh(m): ', dahdh(m)
!      pause
!
!      XHT = ELEV - AO(N1)
!      NA  = (L - 1) * NDF + 1
!      RHO = DEN(N1)
!      PPL = 1.0
!
!      WRITE(*,*) 'Kontrolle'
!      WRITE(*,*) 'm: ',m, 'ah(m): ', ah(m), 'vel(3,m): ',  vel(3,m), 'zs: ',zs, 'dzsdh: ',  dzsdh,'xht: ', xht
!      WRITE(*,*) 'rho: ', rho, 'grav: ',  grav, 'Term: ', rho*grav*zs*ah(m), 'inotr: ', inotr
!      WRITE(*,*) f(na)
!
!      IF(L .EQ. 1) PPL = -PPL
!       if (inotr == 1) then
!          F(NA)           = F(NA) - PPL * RHO * grav * zs * ah(m)
!          ESTIFM(NA,NA+2) = ESTIFM(NA,NA+2) + PPL * RHO * grav * (dzsdh * ah(m) + dahdh(m))
!        else
!          f(na)           = f(na) - PPL * RHO * grav* zs * ah(m) * XHT
!          ESTIFM(NA,NA+2) = ESTIFM(NA,NA+2) + PPL * RHO * grav * (dzsdh * ah(m) + dahdh(m)) * xht
!        endif
!      WRITE(*,*) ppl, grav, rho, zs, ah(m)
!      WRITE(*,*) f(na)
!      pause
!    end if
!  end do
!end do CouplingCorrection
!-

 1305 CONTINUE
 1320 CONTINUE
outer: DO I=1,NCN,2
  J=NCON(I)
  IA=NDF*(I-1)
  inner: DO K=1,NDF
    IA=IA+1
    JA=NBC(J,K)
    IF(JA.EQ.0) CYCLE inner
    R1(JA)=R1(JA)+F(IA)
  ENDDO inner
enddo outer

!control output
if (nn > -10) then
  n3 = ncon(3)
  n1 = ncon(1)
  WRITE(*,*) '*************************'
  WRITE(*,*) 'Knotendaten, Element: ', nn
  WRITE(*,*) '*************************'
  WRITE(*,'(a18,2(6x,i4))')    '          Knoten: ', nop(nn,1), nop(nn,3)
  WRITE(*,'(a18, 1x,f13.8)')   '    Sohlgefaelle: ', sbot
  WRITE(*,'(a18,2(1x,f13.8))') '          Laenge: ', xl(1), xl(3)
  WRITE(*,*) area(nn)
  WRITE(*,'(a18,2(1x,f13.8))') '    Wassertiefen: ', h1, h3
  WRITE(*,'(a18,2(1x,f13.8))') '      Schl-kurve: ', qh(n1), qh(n3)
  WRITE(*,'(a18,2(1x,f13.8))') '    akt. Abfluss: ', vel_res(2)*ah(1), vel_res(2)*ah(2)
  WRITE(*,'(a18,2(1x,f13.8))') '    Fliessgeschw: ', vel_res(1), vel_res(2)
  WRITE(*,'(a18,2(1x,f13.8))') '   Fliessflaeche: ', ah(1), ah(2)
  WRITE(*,'(a18,2(1x,f13.8))') '    Reibgefaelle: ', sfnod(1), sfnod(2)
  WRITE(*,'(a18,2(1x,f13.8))') '    Wichtung    : ', sfwicht(1), sfwicht(2)

  WRITE(*,'(a18,2(1x,f13.8))') '           dahdh: ', dahdh(n1), dahdh(n3)
  WRITE(*,'(a18,2(1x,f13.8))') '         d2ahdh2: ', d2ahdh(1), d2ahdh(2)
  WRITE(*,'(a18,2(1x,f13.8))') '         dQSchdh: ', dqhdh(1), dqhdh(2)
  WRITE(*,'(a18,2(1x,f13.8))') '       d2QSchdh2: ', d2qhdh(1), d2qhdh(2)
  WRITE(*,*)                   '******************'
  WRITE(*,'(a18,4(1x,i4))'  )  '     Gausspunkte: ', (i,i = 1, 4)
  WRITE(*,*)                   '******************'
  WRITE(*,'(a18,4(1x,f13.8))') '           hhint: ', (hhint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        dhhintdx: ', (dhhintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '           qqint: ', (qqint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         areaint: ', (areaint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '      dareaintdh: ', (dareaintdh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '     d2areaintdh: ', (d2areaintdh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         daintdx: ', (daintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        d2aintdx: ', (d2aintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '      d2aintdhdx: ', (d2aidhdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         qschint: ', (qschint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        dqsintdh: ', (dqsintdh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         d2qsidh: ', (d2qsidh(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        dqsintdx: ', (dqsintdx(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        s0schint: ', (s0schint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '           sfint: ', (sfint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '       dsfintdh1: ', (dsfintdh1(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '        vflowint: ', (vflowint(i), i = 1, 4)
  WRITE(*,'(a18,4(1x,f13.8))') '         dvintdx: ', (dvintdx(i), i = 1, 4)
ENDIF

!estifm-testoutput
if (nn > 0) then
  WRITE(9919,*) 'Element ', nn, 'coef1Pol'
  WRITE(9919,'(6x,12(1x,i10))') ( nbc (nop(nn,1), j), j=1, 4), 0, 0, 0, 0, ( nbc (nop(nn,3), j), j=1, 4)
  do i = 1,12
    if (MOD(i,4) == 1 .or. MOD(i,4) == 2) then
      if (nop(nn, 1+(i-MOD(i,4))/ 4) < 0) then
        WRITE(9919,'(i6,13(1x,f10.2))') 0, (estifm(i,j), j=1, 12), f(i)
      else
        WRITE(9919,'(i6,13(1x,f10.2))') nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4)), (estifm(i,j), j=1, 12), f(i)
      end if
    elseif (MOD(i,4) == 3 ) then
      if (nop(nn, 1+(i-MOD(i,4))/ 4) < 0) then
        WRITE(9919,'(i6,13(1x,f10.2))') 0, (estifm(i,j), j=1, 12), f(i)
      else
        WRITE(9919,'(i6,13(1x,f10.2))') nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4)), (estifm(i,j), j=1, 12), f(i)
      end if
    ELSE
      if (nop(nn, i/4 ) < 0) then
        WRITE(9919,'(i6,13(1x,f10.2))') 0, (estifm(i,j), j=1, 12), f(i)
      else
        WRITE(9919,'(i6,13(1x,f10.2))') nbc( nop(nn, i/4 ), 4), (estifm(i,j), j=1, 12), f(i)
      end if
    endif
  end do
  WRITE(9919,*)
  WRITE(9919,*)
endif
!-

RETURN

!*-
!*...... Special case for junction element
!*-
 2000 CONTINUE
!cipk dec00
!C-
!C...... Special cases for control structures or junction sources
!C-
IF(IMAT(NN) .GT. 903) THEN
  !init A(h)
  ah(n1) = 0.0
  ah(n3) = 0.0
  !init Q(h)
  qh(n1) = 0.0
  qh(n3) = 0.0
  do k=0, 12
    !A(h)
    ah(n1) = ah(n1) + apoly(n1,k) * vel(3,n1)**(k)
    ah(n3) = ah(n3) + apoly(n3,k) * vel(3,n3)**(k)
  end do
  !init dA(h)/dh
  dahdh(n1) = 0.0
  dahdh(n3) = 0.0
  do k=1,12
    !dA(h)/dh
    dahdh(n1) = dahdh(n1) + (k) * apoly(n1,k) * h1**(k-1)
    dahdh(n3) = dahdh(n3) + (k) * apoly(n3,k) * h3**(k-1)
  end do

  CALL CSTRC(NN)
  GO TO 1320
ENDIF

!*-
!*...... Special case for junction element
!*-

NCN=NCORN(NN)
!c     WRITE(*,*) NN,NCN
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
