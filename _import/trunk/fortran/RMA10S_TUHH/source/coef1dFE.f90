!Last change:  K    22 Jun 2007    5:21 pm

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
REAL (KIND=8) :: IntahRand
REAL (KIND=8) :: sbot

INTEGER :: i, j, k
!new variables
!BC-values
REAL (KIND=8) :: zsBC, dzsdhBC, fzsBC, dfzsdhBC

REAL (KIND=8) :: Fint1(1:4), Fint2(1:4)
REAL (KIND=8) :: dFintdh1(1:4), dFintdh2(1:4)
REAL (KIND=8) :: d2Fintdh1(1:4), d2Fintdh2(1:4)
REAL (KIND=8) :: dFintdx1(1:4), dFintdx2(1:4)
REAL (KIND=8) :: d2Fintdxdh1(1:4), d2Fintdxdh2(1:4)

REAL (KIND=8) :: daintdx1(1:4), daintdx2(1:4)
REAL (KIND=8) :: d2aintdxdh1(1:4), d2aintdxdh2(1:4)

REAL (KIND=8) :: yps(1:4)
REAL (KIND=8) :: dypsdh(1:4)
REAL (KIND=8) :: d2ypsdh(1:4)
REAL (KIND=8) :: dypsdx(1:4)
REAL (KIND=8) :: d2ypsdhdx(1:4)

REAL (KIND=8) :: zsint(1:4)
REAL (KIND=8) :: dzsintdh(1:4)
REAL (KIND=8) :: d2zsintdh(1:4)
REAL (KIND=8) :: d2sintdx(1:4)
REAL (KIND=8) :: d2zsintdhdx(1:4)
!end of new variables

REAL(KIND=8) :: froudeint(1:4)
REAL(KIND=8) :: vflowint(1:4)                
REAL(KIND=8) :: dvintdx(1:4)                 
REAL(KIND=8) :: dvintdt(1:4)                 
REAL(KIND=8) :: hhint(1:4)                   
REAL(KIND=8) :: dhhintdx(1:4)                
REAL(KIND=8) :: dhintdt(1:4)                 
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
REAL(KIND=8) :: pbei(1:2,0:12)
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

!nis,may07: Sidflowterm
REAL (KIND=8) :: sidft

!local resulting velocities
REAL(KIND=8) :: vel_res(3)

INTEGER :: PolyTest

INTEGER :: optin, testoutput, byparts
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
  !new variables
  Fint1(j)       = 0.0
  Fint2(j)       = 0.0
  dFintdh1(j)    = 0.0
  dFintdh2(j)    = 0.0
  d2Fintdh1(j)   = 0.0
  d2Fintdh2(j)   = 0.0
  dFintdx1(j)    = 0.0
  dFintdx2(j)    = 0.0
  d2Fintdxdh1(j) = 0.0
  d2Fintdxdh2(j) = 0.0

  daintdx1(j)    = 0.0
  daintdx2(j)    = 0.0
  d2aintdxdh1(j) = 0.0
  d2aintdxdh2(j) = 0.0

  yps(j)        = 0.0
  dypsdh(j)     = 0.0
  d2ypsdh(j)    = 0.0
  dypsdx(j)     = 0.0
  d2ypsdhdx(j)  = 0.0

  zsint(j)      = 0.0
  dzsintdh(j)   = 0.0
  d2zsintdh(j)  = 0.0
  d2sintdx(j)   = 0.0
  d2zsintdhdx(j)= 0.0

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

!Use no Differentiation by parts
byparts = 1
!byparts = 1: No differentiation by parts
!byparts = 2: Apply Differentiation by parts
!byparts = 3: new trial for natural boundary condition; it doesn't work yet

testoutput = 0
!testoutput = 0: output switched off
!testoutput = 1: output of partly values of equations and output of matrces
!testoutput = 2: output of variables at the end
!testoutput = 3: output of beiwert-variable

optin = 1
!optin-differences only occur in the convective terms
!optin = 1: Calculation with flow coefficient in equations (standard formulation)
!optin = 2: Calculation without flow coefficient


do i = 1, 3, 2
  n1 = nop(nn,i)

  !fix waterdepth for h-boundaries
  if (byparts == 1) then
    IF (MOD(NFIX(N1)/100,10) .EQ. 2) THEN
      vel(3,n1) = spec(n1,3)
    end if
  endif

  
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

!for 1D-2D-transition elements
IF(NCN .EQ. 5  .AND.  IMAT(NN) .LT. 900) NCN=3

!residual vector and Jacobi-Matrix (derivatives) arrays
NEF=NCN*NDF
DO I=1,NEF
  F(I) = 0.0
  DO J=1,NEF
    ESTIFM(I,J) = 0.0
  enddo
enddo

!process on element direction
IF(NTX .EQ. 0) THEN
  TH(NN) = ATAN2 (CORD(N3,2)-CORD(N1,2), CORD(N3,1) - CORD(N1,1))
ENDIF

!getting angles
CXX = COS (TH(NN))
SAA = SIN (TH(NN))
!Set number of Gauss-points
NGP=4
NCNX=2

!C-
!C-.....COMPUTE LOCAL CORDS.....
!C-

!Get water depths at corner nodes
h1=vel(3,n1)
h3=vel(3,n3)

!get reference node
NR = NCON(1)
DO k = 1, ncn
  N = NCON(K)
  !Calculate angular difference between the principal element direction th and the direction at the node alfa
  ANGDIF = TH(NN) - ALFA(N)

  !Set a direction factor in dependency of the defintion direction at a node and of an element
  IF(ABS(ANGDIF) .GT. 1.5708  .AND.  ABS(ANGDIF) .LT. 4.71779) THEN
    QFACT(K) = -1.0
    QQFACT(K)= -1.0
  ELSE
    QFACT(K) = 1.0
    QQFACT(K)= 1.0
  ENDIF

  !Calculate x- and y- distance from node to reference node. With linear approach, an element might be curved
  DX = CORD(N,1) - CORD(NR,1)
  DY = CORD(N,2) - CORD(NR,2)

  !Calculate the direct distance between the nodes and the perpendicular distance from the node to the element chord
  XL(K) =  DX*CXX + DY*SAA
  YL(K) = -DX*SAA + DY*CXX

  !Calculate the mean velocity at node k (local number)
  vel_res(k) = vel(1,n)*COS(alfa(n)) + vel(2,n)*SIN(alfa(n))

!updating length, if kilometres are given
if (kmx(n1) /= -1.0 .and. kmx(n3) /= -1.0) then
  !Scaling the element-length
  xl(3) = xl(k) / ABS(xl(k)) * ABS((kmx(n3)-kmx(n1))*1000)
  !Questionable: What happens to curved elements? Might it be useful to introduce them for curved elements
  yl(3) = 0.0
end if
enddo

!Questionable: Why is the length always averaged. It is also done like this in coef1nt's code
xl(2) = xl(3)/2
yl(2) = yl(3)/2

!Questionable: What is EINA respectively EINX and EiNY standing for?
!CIPK JAN03
EINA=EINX(NN)*CX+EINY(NN)*SA

!C-
!C-.....COMPUTE ELEMENT EQUATIONS.....
!C-
!Questionable: What is TFR standing for?
TFR=TEL/ABS(XL(3))

!CIPK MAY04 RESET ELEMENT INFLOW
IF(INOFLOW(NN) .EQ. 0) THEN
  SIDFQ=SIDF(NN)
ELSE
  SIDFQ=0.
ENDIF

!Questionable: Why is there such a confusing name handling with the inflow term?
!SIDFQQ=SIDF(NN)
sidft = sidfq

!test for valid water depth range
if (h1 < hhmin(n1) .and. ntx == 1)then
  WRITE (*,*) 'waterdepth at node', n1, 'less than Hmin'
ELSEIF (h1 > hhmax(n1) .and. ntx == 1) then
  WRITE (*,*) 'waterdepth at node', n1, 'greater than Hmax'
ELSEIF (h3 < hhmin(n3) .and. ntx == 1) then
  WRITE (*,*) 'waterdepth at node', n3, 'less than Hmin'
ELSEIF (h3 > hhmax(n3) .and. ntx == 1) then
  WRITE (*,*) 'waterdepth at node', n3, 'greater than Hmax'
end if

!Questionable: Shouldn't be the form of the other coefs be used, i.e. the derivative form of the bed coordinates
!bedslope
if (kmx(n1) == 0.0 .and. kmx(n3) == 0.0) then
  sbot = (ao(n1) - ao(n3)) / (  (cord(n3,1) - cord(n1,1))*COS(th(nn)) + (cord(n3,2) - cord(n1,2))*SIN(th(nn)))
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
!Questionable: Why is this used in a global variable form
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
sfnod(2) = sfwicht(2) * vel_res(3) * ah(n3) * ABS(vel_res(3) * ah(n3)) / (qh(n3)**2) * qgef(n3)

!Calculation case
if (ntx == 1) then

  !Check for critical/ subcritical discharge
  froude(1) = vel_res(1) / sqrt (grav * h1)
  froude(2) = vel_res(3) / sqrt (grav * h3)
  if (froude(1).gt.1) then
    WRITE(*,*)'Supercritical flow at node ',n1
  ELSEIF (froude(2).gt.1) then
    WRITE(*,*)'Supercritical flow at node ',n3
  end if
  
  !unsteady case
  if (icyc.gt.0) then
    !time derivatives of the water depth
    dhht(n1) = vdot(3,n1)
    dhht(n3) = vdot(3,n3)
    !time derivatives of the velocity
    dvvt(n1) = vdot(1,n1) * COS(alfa(n1)) + vdot(2,n1) * SIN(alfa(n1))
    dvvt(n3) = vdot(1,n3) * COS(alfa(n3)) + vdot(2,n3) * SIN(alfa(n3))
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
  
  !flow coefficient calculation
  AssignFlowCoef: do i = 1, 2

    !corner node number
    j = (i - 1) * 2 + 1
    !corner node
    n = ncon(j)
    !corresponding water depth
    h = vel(3, n)

    !no flow coefficient
    if (beient == 0) then
      bei(i)     = 1.0
      dbeidh(i)  = 0.0
      d2beidh(i) = 0.0
      CYCLE AssignFlowCoef
    !energy flow coefficient
    ELSEIF(beient == 1) THEN
      hdif(i) = alphah(n)
    !momentum flow coefficient
    ELSEIF (beient == 2) THEN
      hdif(i) = betah(n)
    ELSE
      WRITE(lout,*) 'ERROR - wrong parameter beient'
      WRITE(lout,*) 'Check in input file, whether beient has a legal value (0, 1 or 2)'
      WRITE(lout,*) 'program goes on without flow parameter (default option beient = 0)'
      WRITE(*,*) 'ERROR - wrong parameter beient'
      WRITE(*,*) 'Check in input file, whether beient has a legal value (0, 1 or 2)'
      WRITE(*,*) 'program goes on without flow parameter (default option beient = 0)'
      !Reset beient
      beient = 0
      !Calculate, as if beient was 0
      bei(i)     = 1.0
      dbeidh(i)  = 0.0
      d2beidh(i) = 0.0
      CYCLE AssignFlowCoef
    ENDIF

    !range for water depth less than bandful (only main channel)
    if (h <= hbordv(n)) then

      pbei(i,1)   = 1.0
      do k = 0, 12
        pbei(i,k) = 0.0
      end do
    !range between bankful and lower border of full polynom application range for flow coefficient (apply transient polynom)
    elseif (h > hbordv(n) .and. h < hdif(i)) then

      if (beient == 1) then
        do k = 0, 3
          pbei(i,k) = alphad(n, k)
        end do
      ELSEIF (beient == 2) then
        do k = 0, 3
          pbei(i,k) = betad(n, k)
        end do
      ENDIF
      do k = 4, 12
        pbei(i,k) = 0.0
      end do

    !full polynom application after reaching the lower border water depth
    elseif (h >= hdif(i)) then
      !momentum flow coefficient
      IF (beient == 1) THEN
        DO   k = 0, 12
          pbei(1,k) = alphapk(n1,k)
        ENDDO
      ELSEIF (beient  == 2) THEN
        DO k = 0, 12
          pbei(i,k) = betapk(n, k)
        ENDDO
      ENDIF
    ENDIF

    !bei
    bei(i)=0
    do k = 0, 12
      bei(i) = bei(i) + pbei(i,k) * h**(k)
    end do
    !dbei/dh
    dbeidh(i)=0
    do k = 1, 12
      dbeidh(i) = dbeidh(i) + (k) * pbei(i,k) * h**(k-1)
    end do
    !d2bei/dh2
    d2beidh(i)=0
    do k = 2, 12
      d2beidh(i) = d2beidh(i) + (k-1)*(k) * pbei(i,k) * h**(k-2)
    enddo
  ENDDO AssignFlowCoef

ENDIF !ntx=1

!nis,jun07: If there was no restart, don't apply flow factor for stabilization reasons. The number of iterations for this is set to default
!           moment_off = 15, but may be changed by the user in C2-line
if (maxn < moment_off .and. nb == 0 .and. icyc <=1) then
  do i = 1, 2
    bei(i)     = 0.0
    dbeidh(i)  = 0.0
    d2beidh(i) = 0.0
  end do
end if

if (testoutput == 3) then
  do i = 1, 2
    WRITE(*,*) 'Beiwerte - Element: ', nn, ' Knoten: ', i
    WRITE(*,*) '    bei(i): ', bei(i)
    WRITE(*,*) ' dbeidh(i): ', dbeidh(i)
    WRITE(*,*) 'd2beidh(i): ', d2beidh(i)
  end do
end if

!********************************************************************************************************************************************
!GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP   GAUSS LOOP
!********************************************************************************************************************************************
Gaussloop: DO I = 1, NGP

  !Questionable: What exactly is DNAL and what exactly is TEMP
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
    !Questionable: Why do we not use just one local variable for the parameters at the Gauss-nodes. They are just used once
    H=VEL(3,N1)*XM(1)+VEL(3,N3)*XM(2)
  ELSE
    H=1.0
  ENDIF

  AMW=ABS(TEMP)*HFACT(I)/2.
  AREA(NN)=AREA(NN)+AMW

  ams = amw * rho
  amu = amw


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

  do j = 1, 3
    !v at GP
    vflowint(i) = vflowint(i) + xn(j)  * vel_res(j) * qfact(j)
    !dv/dx at GP
    dvintdx(i)  = dvintdx(i)  + dnx(j) * vel_res(j) * qfact(j)
    !dv/dt at GP
    dvintdt(i)  = dvintdt(i)  + xn(j)  * dvvt(nop(nn,j)) * qfact(j)
  end do


  !secondary variables: area and its derivatives
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
  d2aintdx(i) = dhhintdx(i) * (2.0 * dmx(1) * daintdh1(i) +  xm(1) * d2aintdh1(i) * dhhintdx(i) &
  &                           +2.0 * dmx(2) * daintdh2(i) +  xm(2) * d2aintdh2(i) * dhhintdx(i))
  !d2A/dhdx at GP
  d2aidhdx(i) = dmx(1) *  daintdh1(i) +  xm(1) * d2aintdh1(i) * dhhintdx(i) &
  &            +dmx(2) *  daintdh2(i) +  xm(2) * d2aintdh2(i) * dhhintdx(i)
  !dA/dt at GP
  daintdt(i)  = (xm(1) * daintdh1(i) + xm(2) * daintdh2(i)) * dhintdt(i)

  !sec

  !supercritical or subcritical flow
  froudeint(i) = vflowint(i) / sqrt(grav * hhint(i))
  if (froudeint(i) > 1.0) then
    WRITE(*,*) 'Critical discharge in element', nn
  end if

  !secondary variables: reference-discharge and its derivatives
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

  !secondary variables: friction-slope and its derivatives

  !Sf,0 at GP
  s0schint(i) = qgef(n1) * xm(1) + qgef(n3) * xm(2)
  !weighting factor
  sfwicint(i) = sfwicht(1) * xm(1) + sfwicht(2) * xm(2)
  !Sf at GP
  sfint(i) = sfwicint(i) * s0schint(i) * vflowint(i) * ABS(vflowint(i)) * areaint(i)**2 / qschint(i)**2
  !dSf/dh at GP
  dsfintdh1(i) = s0schint(i) * vflowint(i) * ABS(vflowint(i))                      &
  &              * (QSchint(i)**2 * 2 * areaint(i) * dareaintdh(i) - areaint(i)**2 &
  &              * 2 * Qschint(i) * dqsintdh(i)) / Qschint(i)**4

  !The following has to be resetted (April 2007)!!!
  !secondary variables: flow-coefficient (e.g. Boussinesq) and its derivatives
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

  !mass center calculations
  if (byparts == 3) then
    !dAintdx
    daintdx1(i) = daintdh1(i) * dhhintdx(i)
    daintdx2(i) = daintdh2(i) * dhhintdx(i)
    !d2aintdxdh
    d2aintdxdh1(i) = d2aintdh1(i) * dhhintdx(i)
    d2aintdxdh2(i) = d2aintdh2(i) * dhhintdx(i)
    !Fint
    Fint1(i) = 0.0
    Fint2(i) = 0.0
    do j = 0, 12
      Fint1(i) = Fint1(i) + (j / (j+1.)) * apoly(n1,j) * hhint(i)**(j+1)
      Fint2(i) = Fint2(i) + (j / (j+1.)) * apoly(n3,j) * hhint(i)**(j+1)
    enddo
    !dFintdh
    dFintdh1(i) = 0.0
    dFintdh2(i) = 0.0
    do j = 0, 12
      dFintdh1(i) = dFintdh1(i) + (j) * apoly(n1,j) * hhint(i)**j
      dFintdh2(i) = dFintdh2(i) + (j) * apoly(n3,j) * hhint(i)**j
    end do
    !d2Fintdh
    d2Fintdh1(i) = 0.0
    d2Fintdh2(i) = 0.0
    do j = 0, 12
      d2Fintdh1(i) = d2Fintdh1(i) + (j**2) * apoly(n1,j) * hhint(i)**(j-1)
      d2Fintdh2(i) = d2Fintdh2(i) + (j**2) * apoly(n3,j) * hhint(i)**(j-1)
    end do
    !dFintdx
    dFintdx1(i) = 0.0
    dFintdx2(i) = 0.0
    dFintdx1(i) = dFintdh1(i) * dhhintdx(i)
    dFintdx2(i) = dFintdh2(i) * dhhintdx(i)
    !d2Fintdxdh1
    d2Fintdxdh1(i) = d2Fintdh1(i) * dhhintdx(i)
    d2Fintdxdh2(i) = d2Fintdh2(i) * dhhintdx(i)
    if (testoutput == 3) then
      WRITE(*,*)
      WRITE(*,*) 'F1: ', fint1(i)
      WRITE(*,*) 'F2: ', fint2(i)
      WRITE(*,*) 'dF1/dh: ', dFintdh1(i)
      WRITE(*,*) 'dF2/dh: ', dFintdh2(i)
      WRITE(*,*) 'd2F1/dh2: ', d2Fintdh1(i)
      WRITE(*,*) 'd2F2/dh2: ', d2fintdh2(i)
      WRITE(*,*) 'd2F1/dhdx: ', d2Fintdxdh1(i)
      WRITE(*,*) 'd2F2/dhdx: ', d2Fintdxdh2(i)
      WRITE(*,*)
    end if

    !zS-Calculations
    !zS
    zsint(i) = xm(1) * Fint1(i) / aint1(i) &
    &        + xm(2) * Fint2(i) / aint2(i)

    !dzS/dh
    dzsintdh(i) = xm(1) * (aint1(i) * dFintdh1(i) - fint1(i) * daintdh1(i)) / aint1(i)**2 &
    &           + xm(2) * (aint2(i) * dFintdh2(i) - fint2(i) * daintdh2(i)) / aint2(i)**2

    !d2zS/dh2
    d2zsintdh(i) = xm(1) * (aint1(i) * d2Fintdh1(i) - fint1(i) * d2aintdh1(i)                                            &
    &                       - 2. * dFintdh1(i) * daintdh1(i) + 2. * fint1(i) / aint1(i) * daintdh1(i)**2) / aint1(i)**2  &
    &            + xm(2) * (aint2(i) * d2Fintdh2(i) - fint2(i) * d2aintdh2(i)                                            &
    &                       - 2. * dFintdh2(i) * daintdh2(i) + 2. * fint2(i) / aint2(i) * daintdh2(i)**2) / aint2(i)**2

    !d2zS/(dhdx)
    d2zsintdhdx(i) = xm(1) * (- daintdx1(i) * dFintdh1(i) + aint1(1) * d2Fintdxdh1(i) - dFintdx1(i) * daintdh1(i)                &
    &                         - fint1(i) * d2aintdxdh1(i) + 2. * daintdx1(i) * fint1(i) / aint1(i) * daintdh1(i)) / aint1(i)**2  &
    &              + xm(2) * (- daintdx2(i) * dFintdh2(i) + aint2(1) * d2Fintdxdh2(i) - dFintdx2(i) * daintdh2(i)                &
    &                         - fint2(i) * d2aintdxdh2(i) + 2. * daintdx2(i) * fint2(i) / aint2(i) * daintdh2(i)) / aint2(i)**2

    !ypsilon
    yps(i)  = xm(1) * (1. - 1. / hhint(i) * fint1(i) / aint1(i)) &
    &       + xm(2) * (1. - 1. / hhint(i) * fint2(i) / aint2(i))

    !dypsilon/dh
    dypsdh(i)  = - xm(1) * (  hhint(i) * aint1(i) * dFintdh1(i) - fint1(i) * (aint1(i) &
    &                       + hhint(i) * daintdh1(i))) / (hhint(i) * aint1(i))**2      &
    &            - xm(2) * (  hhint(i) * aint2(i) * dFintdh2(i) - fint2(i) * (aint2(i) &
    &                       + hhint(i) * daintdh2(i))) / (hhint(i) * aint2(i))**2

    !d2ypsilon/dh2
    d2ypsdh(i) = - xm(1) * (- 2. * hhint(i) * daintdh1(i) * dFintdh1(i) + 2. * fint1(i) * daintdh1(i) - aint1(i) * dFintdh1(i) &
    &                       + 2. * fint1(i) * aint1(i) / hhint(i) + hhint(i) * aint1(i) * d2Fintdh1(i)                         &
    &                       - hhint(i) * fint1(i) * d2aintdh1(i) + 2. * fint1(i) * hhint(i) / aint1(i) * (daintdh1(i))**2)     &
    &                    / (hhint(i) * aint1(i) )**2                                                                           &
    &            - xm(2) * (- 2. * hhint(i) * daintdh2(i) * dFintdh2(i) + 2. * fint2(i) * daintdh2(i) - aint2(i) * dFintdh2(i) &
    &                       + 2. * fint2(i) * aint2(i) / hhint(i) + hhint(i) * aint2(i) * d2Fintdh2(i)                         &
    &                       - hhint(i) * fint2(i) * d2aintdh2(i) + 2. * fint2(i) * hhint(i) / aint2(i) * (daintdh2(i))**2)     &
    &                    / (hhint(i) * aint2(i) )**2
    !dypsilon/dx
    dypsdx(i) = dmx(1) * (1. - fint1(i) / hhint(i) / aint1(i))                                                &
    &         + xm(1) * (dFintdh1(i) - fint1(i) / hhint(i) - fint1(i) / aint1(i) * daintdh1(i)) * dhhintdx(i) &
    &         + dmx(2) * (1. - fint2(i) / hhint(i) / aint2(i))                                                &
    &         + xm(1) * (dFintdh2(i) - fint2(i) / hhint(i) - fint2(i) / aint2(i) * daintdh2(i)) * dhhintdx(i)

    !d2ypsilon/(dhdx)
    d2ypsdhdx(i) = - xm(1) * (dhhintdx(i) * (2. * aint1(i) / hhint(i) * fint1(i) - aint1(i) * dFintdh1(i) - daintdh1(i) * fint1(i))&
    &                       + daintdx1(i) * (fint1(i) - hhint(i) * dFintdh1(i) - 2. * hhint(i) / aint1(i) * daintdh1(i) * fint1(i))&
    &                       + dFintdx1(i) * (hhint(i) * daintdh1(i) - aint1(i))                                                    &
    &                       + hhint(i) * aint1(i) * d2Fintdxdh1(i) + hhint(i) * fint1(i) * d2aintdxdh1(i))                         &
    &                    / (hhint(i) * aint1(i) )**2                                                                               &
    &              - dmx(1) * (hhint(i) * aint1(i) * dFintdh1(i) - fint1(i) * aint1(i) + fint1(i) * hhint(i) * daintdh1(i))        &
    &                     / (hhint(i) * aint1(i))**2                                                                               &
    &              - xm(2) * (dhhintdx(i) * (2. * aint2(i) / hhint(i) * fint2(i) - aint2(i) * dFintdh2(i) - daintdh2(i) * fint2(i))&
    &                       + daintdx2(i) * (fint2(i) - hhint(i) * dFintdh2(i) - 2. * hhint(i) / aint2(i) * daintdh2(i) * fint2(i))&
    &                       + dFintdx2(i) * (hhint(i) * daintdh2(i) - aint2(i))                                                    &
    &                       + hhint(i) * aint2(i) * d2Fintdxdh2(i) + hhint(i) * fint2(i) * d2aintdxdh2(i))                         &
    &                    / (hhint(i) * aint2(i) )**2                                                                               &
    &              - dmx(2) * (hhint(i) * aint2(i) * dFintdh2(i) - fint2(i) * aint2(i) + fint2(i) * hhint(i) * daintdh2(i))        &
    &                     / (hhint(i) * aint2(i))**2

    if (testoutput == 3 .or. testoutput == 1) then
      WRITE(*,*) 'Element: ', nn, 'GP: ', i
      WRITE(*,*) 'h: ', hhint(i)
      WRITE(*,*) 'zS: ', zsint(i)
      WRITE(*,*) 'dzsintdh: ', dzsintdh(i)
      WRITE(*,*) 'd2zsintdh: ', d2zsintdh(i)
      WRITE(*,*) 'd2zsintdhdx: ', d2zsintdhdx(i)
      WRITE(*,*) 'ypsilon: ', yps(i)
      WRITE(*,*) 'dypsdh: ', dypsdh(i)
      WRITE(*,*) 'd2ypsdh: ', d2ypsdh(i)
      WRITE(*,*) 'dypsdx: ', dypsdx(i)
      WRITE(*,*) 'd2ypsdhdx: ', d2ypsdhdx(i)
      if (i == 4) pause
    end if
  end if

 !*****************************************************************************************************************************************
 !RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS
 !  EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS   RESIDUAL EQUATIONS
 !*****************************************************************************************************************************************

 !*****************************************************************************************************************************************
 !momentum equation   momentum equation   momentum equation   momentum equation   momentum equation   momentum equation   momentum equation
 !*****************************************************************************************************************************************

  FRN  = 0.0
  FRNX = 0.0

  !Terms B - D: Convective terms
  IF     (optin == 1) THEN
    FRN =                                                          &
           !Term B: Convective term
      &    + vflowint(i)**2 * areaint(i) * dbeiintdx(i)            &
           !Term C: Convective term
      &    + beiint(i) * vflowint(i)**2 * daintdx(i)               &
           !Term D: Convective term
      &    + 2. * beiint(i) * vflowint(i) * areaint(i) * dvintdx(i)

  ELSEIF (optin == 2) THEN
    FRN =                                                          &
           !Term B - D: Convective term
       &   + vflowint(i) * areaint(i) * dvintdx(i)
  ENDIF

  !Term E: Hydrostatic term
  if     (Byparts == 1) then
    !Do not apply Differentiation by parts
    FRN = FRN                                                      &
      &    + grav * areaint(i) * dhhintdx(i)
  ELSEIF (Byparts == 2) then
    !Differentiation by parts with half a depth as natural boundary term
    FRN = FRN                                                      &
      &    - grav * dhhintdx(i) * (dareaintdh(i) * hhint(i) - areaint(i)) / 2.0
  ELSEIF (Byparts == 3) then
    !Differentiation using mass center of area
    FRN = FRN                                                      &
      &    - grav * (daintdx(i) * zsint(i))                        &
      &    + grav * areaint(i) * (hhint(i) * dypsdx(i) + yps(i) * dhhintdx(i) )
  end if

    FRN = FRN                                                      &
           !Term F: Friction term
      &    + grav * areaint(i) * sfint(i)                          &
           !Term G: bottom slope term
      &    - grav * areaint(i) * sbot                              &
           !Term H: sideflow term
      &    + sidft * vflowint(i)

  !Term E: Hydrostatic term
  IF     (byparts == 2) THEN
    !Differentiation by parts
    FRNX =                                                         &
      &  - grav * areaint(i) * hhint(i) / 2.0

  ELSEIF (byparts == 3) then
    !Differentiation using mass center of area
    FRNX = FRNX                                                    &
      &  - grav * areaint(i) * zsint(i)
  ENDIF

  !Term A: Unsteady terms
  IF (icyc > 0) THEN
    IF (optin == 1) THEN
      FRN = FRN                                                    &
        !Term A1: Unsteady term
        &    + areaint(i) * dvintdt(i)                             &
        !Term A2: Unsteady term
        &    + vflowint(i) * daintdt(i)

    ELSEIF (optin == 2) THEN
      FRN = FRN                                                    &
        !Term A
        &    + areaint(i) * dvintdt(i)
    ENDIF
  ENDIF

  !Assemble equation values
  do l = 1, 3
    !equation number
    ia = 1 + ndf * (l-1)
    !equation
    f(ia) = f(ia) - AMS * (xn(l) * FRN + dnx(l) * FRNX) * qfact(l)
  enddo

  !**********************************************************************
  !testoutput  testoutput  testoutput  testoutput  testoutput  testoutput
  !**********************************************************************
  if (testoutput == 1) then
    WRITE(*,*) 'Momentum equation, element: ', nn, 'GaussPt.: ', i
    WRITE(*,*) 'FRN'
    if (icyc > 0) then
      if (optin == 1) then
        WRITE(*,*) 'Term A1: ', + areaint(i) * dvintdt(i)
        WRITE(*,*) 'Term A2: ', + vflowint(i) * daintdt(i)
      ELSEIF (optin == 2) then
        WRITE(*,*) ' Term A: ', + areaint(i) * dvintdt(i)
      end if
    end if

    if (optin == 1) then
      WRITE(*,*) ' Term B: ', + vflowint(i)**2 * areaint(i) * dbeiintdx(i)
      WRITE(*,*) ' Term C: ', + beiint(i) * vflowint(i)**2 * daintdx(i)
      WRITE(*,*) ' Term D: ', + 2 * beiint(i) * vflowint(i) * areaint(i) * dvintdx(i)
    ELSEIF (optin == 2) then
      WRITE(*,*) ' Term D: ', + vflowint(i) * areaint(i) * dvintdx(i)
    endif

    if     (byparts == 1) then
      WRITE(*,*) ' Term E: ',  + grav * areaint(i) * dhhintdx(i)
    ELSEIF (byparts == 2) then
      WRITE(*,*) 'Term E1: ', - grav * dhhintdx(i) * (dareaintdh(i) * hhint(i)   - areaint(i))/ 2.0
    ELSEIF (byparts == 3) then
      WRITE(*,*) 'Term E1: ', - grav * (daintdx(i) * zsint(i)) &
                            & + grav * areaint(i) * (hhint(i) * dypsdx(i) + yps(i) * dhhintdx(i) )
    end if

    WRITE(*,*) ' Term F: ', + grav * areaint(i) * sfint(i)
    WRITE(*,*) ' Term G: ', - grav * areaint(i) * sbot
    WRITE(*,*) ' Term H: ', + sidft * vflowint(i)

    if (byparts == 2) then
      WRITE(*,*) 'FRNX'
      WRITE(*,*) 'Term E2: ', - grav * areaint(i) * hhint(i) / 2.0
    ELSEIF (byparts == 3) then
      WRITE(*,*) 'FRNX'
      WRITE(*,*) 'Term E2: ', - grav * areaint(i) * zsint(i)
    end if

    WRITE(*,*) 'Einzelterme: ', FRN, FRNX
    do l = 1, 3
      WRITE(*,*) 'weighting: ', l,  xn(l), dnx(l)
      WRITE(*,*) 'assembled value without weighting: ', (xn(l) * FRN + dnx(l) * FRNX)
      WRITE(*,*) 'assembled value    with weighting: ', ams * (xn(l) * FRN + dnx(l) * FRNX)
    end do

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

  !unsteady
  if (icyc > 0) then
    FRNC = FRNC                          &
          !Term A
    &     + daintdt(i)
  end if

  !Assemble equation values
  do l = 1, 2
    !equation number
    ia = 3 + (2 * ndf) * (l - 1)
    !equation
    f(ia) = f(ia) - xm(l) * amu * FRNC
  enddo
  !*********************************************************************************************************************************
  !DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES
  !      DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES   DERIVATIVES
  !*********************************************************************************************************************************

  !Questionable: What is about this here?
  !EFa Nov06, Sprung an den Beginn der Gauss-Schleife für junction-Elemente
  !IF(MOD(immt,100).gt.90) GO TO 500

  !*********************************************************************************************************************************
  !MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH
  !*********************************************************************************************************************************
  FEEAN = 0.0
  FEEBN = 0.0
  FEECN = 0.0

  !FEEAN FEEAN FEEAN
  !*****************
  !Terms B - D: Convective terms
  IF     (optin == 1) THEN
    FEEAN =                                                                                          &
            !Term B: Convective term
      &   + vflowint(i)**2 * (dareaintdh(i) * dbeiintdh(i) + areaint(i) * d2beiintdhdx(i))           &
            !Term C: Convective term
      &   + vflowint(i)**2 * (dbeiintdh(i) * daintdx(i) + beiint(i) * d2aidhdx(i))                   &
            !Term D: Convective term
      &   + 2.0 * vflowint(i) * dvintdx(i) * (beiint(i) * dareaintdh(i) + areaint(i) * dbeiintdh(i)) 

  ELSEIF (optin == 2) THEN
    FEEAN =                                                                                          &
            !Term B-D: Convective terms
      &   + vflowint(i) * dvintdx(i) * dareaintdh(i)
  ENDIF

  !Term E: Hydrostatic term
  IF     (byparts == 1) THEN
    !Do not apply differentiation by parts
    FEEAN = FEEAN                                                                                    &
            !Term E
      &   + grav * dareaintdh(i) * dhhintdx(i)
  ELSEIF (byparts == 2) then
    !Differentiation by parts with half a depth as natural boundary term
    FEEAN = FEEAN                                                                                    &
      &   - 0.5  * grav * hhint(i) * d2areaintdh(i) * dhhintdx(i)
  ELSEIF (byparts == 3) then
    FEEAN = FEEAN                                                                                    &
      &   + grav *                                                                                   &
      &     (hhint(i) * dypsdx(i) * dareaintdh(i) + areaint(i) * dypsdx(i) + areaint(i) * hhint(i) * d2ypsdhdx(i) &
      &      + yps(i) * dhhintdx(i) * dareaintdh(i) + areaint(i) * dhhintdx(i) * dypsdh(i)                        &
      &      - dzsintdh(i) * daintdx(i) - zsint(i) * d2areaintdh(i))
  ENDIF

    FEEAN = FEEAN                                                                                    &
            !Term F: Friction term
      &   + grav * (areaint(i) * dsfintdh1(i) + sfint(i) * dareaintdh(i))                            &
            !Term G: bottom slope term
      &   - grav * sbot * dareaintdh(i)


  !Term A: unsteady term
  IF (icyc > 0) THEN
    IF (optin == 1) THEN
      FEEAN = FEEAN                    &
           !Term A1
      &   + dareaintdh(i) * dvintdt(i) &
           !Term A2
      &   + dareaintdh(i) * dhintdt(i)

    ELSEIF (optin == 2) then
      FEEAN = FEEAN                    &
           !Term A1
      &   + dareaintdh(i) * dvintdt(i)
    ENDIF
  ENDIF

  !FEEBN FEEBN FEEBN
  !*****************
  !Terms B - D: Convective terms
  IF (optin == 1) THEN
    FEEBN =                                                   &
            !Term B: Convective term
      &   + vflowint(i)**2 * areaint(i) * dbeiintdh(i)        &
            !Term C: Convective term
      &   + vflowint(i)**2 * beiint(i) * dareaintdh(i)
  ENDIF

  !Term E: Hydrostatic term
  IF (byparts == 1) THEN
    !Do not apply differentiation by parts
    FEEBN = FEEBN                                          &
      &   + grav * areaint(i)
  ELSEIF (byparts == 2) THEN
    !Differentiation by parts with half a depth as natural boundary term
    FEEBN = FEEBN                                          &
      & - grav * 0.5 * (hhint(i) * dareaintdh(i) - areaint(i))
  ELSEIF (byparts == 3) then
    FEEBN = FEEBN                                          &
      & + grav * (areaint(i) * yps(i) + areaint(i) * hhint(i) * dypsdh(i) - zsint(i) * dareaintdh(i) )
  ENDIF

  !FEECN FEECN FEECN
  !*****************
  !Term E: Hydrostatic term
  if (byparts == 2) then
    FEECN = FEECN                                          &
      & - 0.5 * grav * (hhint(i) * dareaintdh(i) + areaint(i))
  ELSEIF (byparts == 3) then
    FEECN = FEECN                                          &
      & - grav * (zsint(i) * dareaintdh(i) + areaint(i) * dzsintdh(i) )
  end if

  !Assemble equation values
  do l = 1, 3
    !line no.
    ia = 1 + ndf * (l - 1)
    do c = 1, 2
      !column no.
      ib = 3 + (2 * ndf) * (c - 1)
      !Add derivatives
      estifm(ia,ib) = estifm(ia,ib) + ams * qfact(l) * (xn(l) * (xm(c)*FEEAN+dmx(c)*FEEBN) + dnx(l) * (xm(c)*FEECN) )
    enddo
  enddo

  !**********************************************************************
  !testoutput  testoutput  testoutput  testoutput  testoutput  testoutput
  !**********************************************************************
  if (testoutput == 1) then
    WRITE(*,*) 'Momentum over depth'
    WRITE(*,*) 'Direktterme FEEAN'

    if (icyc > 0) then
      if     (optin == 1) then
        WRITE(*,*) 'Term A1: ', + dareaintdh(i) * dvintdt(i)
        WRITE(*,*) 'Term A2: ', + dareaintdh(i) * dhintdt(i)
      ELSEIF (optin == 2) then
        WRITE(*,*) 'Term A1: ', + dareaintdh(i) * dvintdt(i)
      end if
    end if

    IF     (optin == 1) then
      WRITE(*,*) ' Term B: ', + vflowint(i)**2 * (dareaintdh(i) * dbeiintdh(i) + areaint(i) * d2beiintdhdx(i))
      WRITE(*,*) ' Term C: ', + vflowint(i)**2 * (dbeiintdh(i) * daintdx(i) + beiint(i) * d2aidhdx(i))
      WRITE(*,*) ' Term D: ', + 2.0 * vflowint(i) * dvintdx(i) * (beiint(i) * dareaintdh(i) + areaint(i) * dbeiintdh(i))
    ELSEIF (optin == 2) then
      WRITE(*,*) ' Term D', + vflowint(i) * dvintdx(i) * dareaintdh(i)
    ENDIF

    if     (byparts == 1) then
      WRITE(*,*) 'Term E1: ', + grav * dareaintdh(i) * dhhintdx(i)
    ELSEIF (byparts == 2) then
      WRITE(*,*) 'Term E1: ', - 0.5  * grav * hhint(i) * d2areaintdh(i) * dhhintdx(i)
    ELSEIF (byparts == 3) then
      WRITE(*,*) 'Term E1: ', + grav *                                                                            &
      &     (hhint(i) * dypsdx(i) * dareaintdh(i) + areaint(i) * dypsdx(i) + areaint(i) * hhint(i) * d2ypsdhdx(i) &
      &      + yps(i) * dhhintdx(i) * dareaintdh(i) + areaint(i) * dhhintdx(i) * dypsdh(i)                        &
      &      - dzsintdh(i) * daintdx(i) - zsint(i) * d2areaintdh(i))
    end if

    WRITE(*,*) ' Term F: ', + grav * (areaint(i) * dsfintdh1(i) + sfint(i) * dareaintdh(i))
    WRITE(*,*) ' Term G: ', - grav * sbot * dareaintdh(i)
    WRITE(*,*) 'Ableitungsterme FEEBN'
    if (byparts == 1) then
      WRITE(*,*) 'Term E2: ', + grav * areaint(i)
    ELSEIF (byparts == 2) then
      WRITE(*,*) 'Term E2: ', - grav * 0.5 * (hhint(i) * dareaintdh(i) - areaint(i))
    ELSEIF (byparts == 3) then
      WRITE(*,*) 'Term E2: ', + grav * (areaint(i) * yps(i) + areaint(i) * hhint(i) * dypsdh(i) - zsint(i) * dareaintdh(i) )
    end if
    if (byparts == 2) then
      WRITE(*,*) 'Ableitungsterme FEECN'
      WRITE(*,*) 'Term E3: ', - 0.5 * grav * (hhint(i) * dareaintdh(i) + areaint(i))
    ELSEIF (byparts == 3) then
      WRITE(*,*) 'Ableitungsterme FEECN'
      WRITE(*,*) 'Term E3: ', - grav * (zsint(i) * dareaintdh(i) + areaint(i) * dzsintdh(i))
    end if
    pause
  endif


  !**************************************************************************************************************************
  !MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY
  !**************************************************************************************************************************
  FEEAN = 0.0
  FEEBN = 0.0

  !FEEAN FEEAN FEEAN
  !*****************
  !Terms B-D: Convective terms
  IF (optin == 1) THEN
    FEEAN =                                                   &
            !Term B: Convective term
      &   + 2.0 * areaint(i)* vflowint(i) * dbeiintdx(i)      &
            !Term C: Convective term
      &   + 2.0 * vflowint(i) * beiint(i) * daintdx(i)        &
            !Term D: Convective term
      &   + 2.0 * beiint(i) * areaint(i) * dvintdx(i)
  ELSEIF (optin == 2) THEN
    FEEAN =                                                                    &
            !Term B-D: Convective terms
      &   + areaint(i) * dvintdx(i)
  ENDIF

  FEEAN = FEEAN                                               &
            !Term F: Friction term
      &   + 2.0 * grav * sfint(i) * areaint(i) / vflowint(i)  &
            !Term H: Sideflow term
      &   + sidft

  !Term A: unsteady term
  if (icyc > 0) then
    if (optin == 1) then
      FEEAN = FEEAN    &
            !Term A2
      &   + daintdt(i)
    endif
  end if

  !FEEBN FEEBN FEEBN
  !*****************
  !Terms B-D: Convective terms
  if (optin == 1) then
    FEEBN =                                                   &
            !Term D: Convective term
      &   + 2.0 * beiint(i) * areaint(i) * vflowint(i)
  ELSEIF (optin == 2) then
    FEEBN =                                                   &
            !Term B-D: Convective term
      &   + vflowint(i) * areaint(i)
  endif

  !Assemble equation values
  do l = 1, 3
    !line no.
    ia = 1 + ndf * (l - 1)
    do c = 1, 3
      !column no.
      ib = 1 + ndf * (c - 1)
      !equation
      estifm(ia,ib) = estifm(ia,ib) + ams * qfact(l) * xn(l) * (xn(c)*FEEAN + dnx(c)*FEEBN) * qqfact(c)
    enddo
  enddo

  !**********************************************************************
  !testoutput  testoutput  testoutput  testoutput  testoutput  testoutput
  !**********************************************************************
  if (testoutput == 1) then
    WRITE(*,*) 'Momentum over velocity'
    WRITE(*,*) 'Direktterme FEEAN'
    if (icyc > 0) then
      if (optin == 1) then
        WRITE(*,*) 'Term A1: ', + daintdt(i)
      end if
    end if

    IF     (optin == 1) then
      WRITE(*,*) ' Term B: ', + 2.0 * areaint(i)* vflowint(i) * dbeiintdx(i)
      WRITE(*,*) ' Term C: ', + 2.0 * vflowint(i) * beiint(i) * daintdx(i)
      WRITE(*,*) ' Term D: ', + 2.0 * beiint(i) * areaint(i) * dvintdx(i)
    ELSEIF (optin == 2) then
      WRITE(*,*) ' Term D: ', + areaint(i) * dvintdx(i)
    ENDIF

    WRITE(*,*) ' Term F: ', + 2.0 * grav * sfint(i) * areaint(i) / vflowint(i)
    WRITE(*,*) ' Term H: ', + sidft
    WRITE(*,*) 'Ableitungsterme FEEBN'
    IF     (optin == 1) then
      WRITE(*,*) ' Term D: ', + 2.0 * beiint(i) * areaint(i) * vflowint(i)
    ELSEIF (optin == 2) then
      WRITE(*,*) ' Term D: ', + vflowint(i) * areaint(i)
    ENDIF
    pause
  end if

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
      estifm(ia,ib) = estifm(ia,ib) + xm(l) * amw * ( xm(c) * EA + dmx(c) * EB)
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
    do c = 1, 3
      !column no.
      ib = 1 + ndf * (c - 1)
      estifm(ia,ib) = estifm(ia,ib) + xm(l) * amw * (xn(c) * EA + dnx(c) * EB) * qfact(c)
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

!Boundary Conditions (Forces) - Waterdepth H
HBCAssign: DO L=1, NCN, 2
  N1=NCON(L)

  IF(MOD(NFIX(N1)/100,10) .EQ. 2) THEN

    !Do not apply differentiation by parts
    if (byparts == 1) then
      NA = (L-1) * NDF + 1
      do iii=1, nef
        estifm (na, iii) = 0.0
      enddo

      estifm(na, na + 2) = 1.0
      f(na)              = 0.0
    !Differentiation by parts with half a depth as natural boundary term
    ELSEIF (byparts == 2) then
      NA = (L-1) * ndf + 1

      ASoll = 0.0
      do k = 0, 12
        ASoll = ASoll + apoly(n1,k) * spec(n1,3)**(k)
      end do
      ppl   = grav * ASoll * rho* qfact(l)
      if (l == 1) ppl = -ppl

      if (testoutput == 4) then
        WRITE(*,*) 'Rand am Knoten: ', n1
        WRITE(*,*) 'ppl: ', ppl
        WRITE(*,*) 'Sollflaeche: ', ASoll, 'Istflaeche: ???'
        WRITE(*,*) 'Sollwert: ', spec(n1,3)/ 2., 'Istwert: ', vel(3,n1)/2.
        WRITE(*,*) 'Ableitung dzsist/dh: ', 0.5
      end if

      f(na) = f(na) - ppl * (spec (n1,3) - vel (3,n1) / 2.)
      estifm (na, na+2) = estifm (na, na+2) - ppl / 2.
    ELSEIF (byparts == 3) then
      NA = (L-1) * ndf + 1

      ASoll = 0.0
      do k = 0, 12
        ASoll = ASoll + apoly(n1,k) * spec(n1,3)**k
      end do
      ppl = grav * ASoll * rho * qfact(l)

      if (l == 1) ppl = -ppl

      !Calc zs of both depth
      FBCSoll   = 0.0
      FBCIst    = 0.0
      dFBCistdh = 0.0
      do j = 0, 12
        FBCSoll = FBCSoll + (j / (j+1.)) * apoly(n1,j) * spec(n1,3)**(j+1)
        FBCIst  = FBCIst  + (j / (j+1.)) * apoly(n1,j) *  vel(3,n1)**(j+1)
        dFBCIstdh  = dFBCIstdh  + (j) * apoly(n1,j) *  vel(3,n1)**(j)
      enddo
      zssoll = FBCSoll / ASoll
      zsist  = FBCIst  / ah(n1)
      dzsistdh  = (ah(n1) * dFBCIstdh - FBCIst * dahdh(n1)) / ah(n1)**2

      if (testoutput == 4) then
        WRITE(*,*) 'Rand am Knoten: ', n1
        WRITE(*,*) 'Einzelterme: ', ah(n1), dfbcistdh, FBCIst, dahdh(n1), vel(3,n1)
        WRITE(*,*) 'ppl: ', ppl
        WRITE(*,*) 'Sollflaeche: ', ASoll, 'Istflaeche', ah(n1)
        WRITE(*,*) 'Sollzs: ', zssoll, 'Istzs: ', zsist
        WRITE(*,*) 'Ableitung dzsist/dh: ', dzsistdh
        !pause
      end if

      if (l == 1) ppl = - ppl
      f(na) = f(na) - ppl * (2. * zssoll - zsist)
      estifm (na, na + 2) = estifm (na, na + 2) - ppl * dzsistdh
    ENDIF

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
  IRW = (N-1) * NDF + 1
  !line of degree of freedom (depth)
  IRH = IRW + 2

  !cosinus and sinus of nodal direction angle
  CX = COS(ALFA(M))
  SA = SIN(ALFA(M))

  !actual velocity
  VT = VEL(1,M) * CX + VEL(2,M) * SA

  !all other entrees are zero
  DO J=1,NEF
    ESTIFM(IRW,J) = 0.
  ENDDO

  !install new boundary condition values
  !ah(m) is cross sectional area; area(nn) is "area" of element that means length
  ESTIFM(IRW,IRW) = ah(m) * area(nn)
  ESTIFM(IRW,IRH) = dahdh(m) * vt * area(nn)
  F(IRW)          = (SPEC(M,1) - VT * ah(m)) * area(nn)
ENDDO QBCAssign

!Correction for Coupling
CouplingCorrection: do l = 1, ncn, 2
  if (byparts == 2 .or. byparts == 3) EXIT couplingcorrection
  if (byparts == 1) EXIT couplingcorrection

  m = ncon(l)

  if (l==1) p = 1
  if (l==3) p = 2

  do i = 1, MaxLT
    !WRITE(*,*) 'Kopplungstest: ', (translines(i,j), j=1, 3), maxlt
    !pause
    if (m == TransLines(i,3)) then
      fzsBC    = 0.0
      dfzsdhBC = 0.0
      zsBC     = 0.0
      dzsdhBC  = 0.0
      do j = 0, 12
        fzsBC    = fzsBC    + apoly(m, j) * j / (j+1) * vel(3,m)**(j+1)
        dfzsdhBC = dfzsdhBC + apoly(m, j) * j * vel(3,m)**(j)
      ENDDO
      zsBC    = fzsBC / ah(m)
      dzsdhBC = (ah(m) * dfzsdhBC - dahdh(m) * fzsBC) / ah(m)**2

      WRITE(*,*) '   depth: ', vel(3,m)
      WRITE(*,*) '     fzs: ', fzsBC
      WRITE(*,*) '  dfzsdh: ', dfzsdhBC
      WRITE(*,*) '      zs: ', zsBC
      WRITE(*,*) '   dzsdh: ', dzsdhBC
      WRITE(*,*) '   ah(m): ', ah(m)
      WRITE(*,*) 'dahdh(m): ', dahdh(m)
      !pause

      XHT = ELEV - AO(N1)
      NA  = (L - 1) * NDF + 1
      RHO = DEN(N1)
      PPL = RHO * grav

      WRITE(*,*) 'Kontrolle'
      WRITE(*,*) 'Knoten m: ', m
      WRITE(*,*) ' Tiefe t: ', vel(3,m)
      WRITE(*,*) 'Fläche A: ', ah(m)
      WRITE(*,*) 'Schwerp.: ', zsBC
      WRITE(*,*) '   dzsdh: ', dzsdhBC
      WRITE(*,*) '     xht: ', xht, inotr
      WRITE(*,*) '  Dichte: ', rho
      WRITE(*,*) '    grav: ', grav
      WRITE(*,*) '    Term: ', rho*grav*zsBC*ah(m)
      WRITE(*,*) 'on Korr.: ', f(na)

      IF(L .EQ. 1) PPL = -PPL
       if (inotr == 1) then
          F(NA)           = F(NA) - PPL * zsBC * ah(m)
          ESTIFM(NA,NA+2) = ESTIFM(NA,NA+2) + PPL * (dzsdhBC * ah(m) + zsBC * dahdh(m))
        else
          f(na)           = f(na) + PPL * zsBC * ah(m) * XHT
          ESTIFM(NA,NA+2) = ESTIFM(NA,NA+2) - PPL * (dzsdhBC * ah(m) + zsBC * dahdh(m)) * xht
        endif
      !WRITE(*,*) ppl, grav, rho, zsBC, ah(m)
      WRITE(*,*) ' + Korr.: ', f(na)
      WRITE(*,*) 'Korrektur ', - PPL * zsBC * ah(m)
      !pause
    end if
  end do
end do CouplingCorrection
!-

 1305 CONTINUE
 1320 CONTINUE
outer: DO I=1,NCN
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
if (testoutput == 2) then
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
  WRITE(*,'(a18,2(1x,f13.8))') '    akt. Abfluss: ', vel_res(1)*ah(1), vel_res(3)*ah(2)
  WRITE(*,'(a18,2(1x,f13.8))') '    Fliessgeschw: ', vel_res(1), vel_res(3)
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
  pause
ENDIF

!estifm-testoutput
if (testoutput > -1) then
  WRITE(9919,*) 'Element ', nn, 'coef1Pol'
        WRITE(9919, 1233) ( nbc (nop(nn,1), j), j=1,  3, 2), nbc (nop(nn,2), 1), ( nbc (nop(nn,3), j), j=1,  3, 2)
  writematrix: do i = 1, 11, 2
    if (i == 7) CYCLE writematrix

    if (MOD(i,4) == 1 .or. MOD(i,4) == 2) then
      WRITE(9919, 1234) nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4)), (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
    elseif (MOD(i,4) == 3 ) then
      WRITE(9919, 1234) nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4)), (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
    ELSE
      WRITE(9919, 1234) nbc( nop(nn, i/4 ), 4), (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
    endif
  end do writematrix
  WRITE(9919,*)
  WRITE(9919,*)
 1233 format (6x, 5(1x, i10))
 1234 format (i6, 6(1x, f10.2))
endif

!end of subroutine
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

END subroutine
