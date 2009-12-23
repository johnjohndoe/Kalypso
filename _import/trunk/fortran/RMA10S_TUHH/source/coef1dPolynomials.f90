!Last change:  WP   22 Jul 2008    4:59 pm

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

SUBROUTINE COEF1dPoly (NN,NTX)
USE COEF1MOD
USE BLKHMOD
USE ICE2MOD
USE BLK10
use BLKECOM
USE BLK10MOD
USE BLK11MOD
USE BLKDRMOD
USE BLKSSTMOD
USE BLKSANMOD
USE PARAKalyps
USE Para1DPoly
SAVE

!nis,aug07: for refactoring purposes
REAL (KIND = 8) :: hs1, hd1, hsx, hdx, hs, hd, h
!nis,jan08: Weighting for interpolated profiles
REAL (KIND = 8) :: LocalWeight

!nis,feb07: Some definitions for internal use concerning the equation upsetting
REAL (KIND = 8) :: aint1(1:4),     aint2(1:4)
REAL (KIND = 8) :: inta1(1:4),     inta2(1:4)
real (KIND = 8) :: daintdh1(1:4),  daintdh2(1:4)
REAL (KIND = 8) :: d2aintdh1(1:4), d2aintdh2(1:4)
REAL (KIND = 8) :: qschint1(1:4),  qschint2(1:4)
REAL (KIND = 8) :: dqsintdh1(1:4), dqsintdh2(1:4)
real (KIND = 8) :: d2qsidh1(1:4),  d2qsidh2(1:4)
REAL (KIND = 8) :: sbot

REAL (KIND = 8) :: CalcPolynomial, CalcPolynomialIntegral, CalcPolynomial1stDerivative, CalcPolynomial2ndDerivative
REAL (KIND = 8) :: speclocal

INTEGER :: i, j, k, Pos, TrID, TrNo
INTEGER :: PPA(1:2), findpolynom, PP(1:2)
integer :: RefNodeN1, RefNodeN3

!new variables
!BC-values
REAL (KIND = 8) :: zsBC, dzsdhBC, fzsBC, dfzsdhBC

REAL (KIND = 8) :: Fint1(1:4), Fint2(1:4)
REAL (KIND = 8) :: dFintdh1(1:4), dFintdh2(1:4)
REAL (KIND = 8) :: d2Fintdh1(1:4), d2Fintdh2(1:4)
REAL (KIND = 8) :: dFintdx1(1:4), dFintdx2(1:4)
REAL (KIND = 8) :: d2Fintdxdh1(1:4), d2Fintdxdh2(1:4)

REAL (KIND = 8) :: daintdx1(1:4), daintdx2(1:4)
REAL (KIND = 8) :: d2aintdxdh1(1:4), d2aintdxdh2(1:4)

REAL (KIND = 8) :: yps(1:4)
REAL (KIND = 8) :: dypsdh(1:4)
REAL (KIND = 8) :: d2ypsdh(1:4)
REAL (KIND = 8) :: dypsdx(1:4)
REAL (KIND = 8) :: d2ypsdhdx(1:4)

REAL (KIND = 8) :: zsint(1:4)
REAL (KIND = 8) :: dzsintdh(1:4)
REAL (KIND = 8) :: d2zsintdh(1:4)
REAL (KIND = 8) :: d2sintdx(1:4)
REAL (KIND = 8) :: d2zsintdhdx(1:4)
!end of new variables

REAL (KIND = 8) :: vflowint(1:4)
REAL (KIND = 8) :: dvintdx(1:4)
REAL (KIND = 8) :: dvintdt(1:4)
REAL (KIND = 8) :: hhint(1:4)
REAL (KIND = 8) :: dhhintdx(1:4)
REAL (KIND = 8) :: dhintdt(1:4)
REAL (KIND = 8) :: areaint(1:4)
REAL (KIND = 8) :: dareaintdh(1:4)
REAL (KIND = 8) :: d2areaintdh(1:4)
REAL (KIND = 8) :: Intareaint(1:4)
REAL (KIND = 8) :: daintdx(1:4)
REAL (KIND = 8) :: d2aintdx(1:4)
REAL (KIND = 8) :: d2aidhdx(1:4)
REAL (KIND = 8) :: daintdt(1:4)
REAL (KIND = 8) :: qschint(1:4)
REAL (KIND = 8) :: dqsintdh(1:4)
REAL (KIND = 8) :: d2qsidh(1:4)
REAL (KIND = 8) :: dqsintdx(1:4)
REAL (KIND = 8) :: d2qsidhdx(1:4)

REAL (KIND = 8) :: s0schint(1:4)
REAL (KIND = 8) :: sfint(1:4)
REAL (KIND = 8) :: dsfintdh1(1:4)
REAL (KIND = 8) :: beiint(1:4)
REAL (KIND = 8) :: beiint1(1:4)
REAL (KIND = 8) :: beiint2(1:4)

!flow coeficient GP
REAL (KIND = 8) :: dbeiintdh(1:4)
REAL (KIND = 8) :: dbeiintdh1(1:4)
REAL (KIND = 8) :: dbeiintdh2(1:4)
REAL (KIND = 8) :: d2beiintdh(1:4)
REAL (KIND = 8) :: d2beiintdh1(1:4)
REAL (KIND = 8) :: d2beiintdh2(1:4)
REAL (KIND = 8) :: dbeiintdx(1:4)
REAL (KIND = 8) :: d2beiintdhdx(1:4)

!time dependent data
REAL (KIND = 8) :: dhdtaltzs(1:2)
REAL (KIND = 8) :: hhalt(1:2)
REAL (KIND = 8) :: dqdtaltzs(1:2)
REAL (KIND = 8) :: dvdtaltzs(1:2)

REAL (KIND = 8) :: FRN, FRNX
REAL (KIND = 8) :: FEEAN, FEEBN, FEECN
REAL (KIND = 8) :: FRNC

REAL (KIND = 8) :: rho

!nis,feb07: testingvariables
REAL (KIND = 8) :: deltax, deltvx, deltay, deltvy, deltaalpha

!nis,may07: Sidflowterm
REAL (KIND = 8) :: sidft

!local resulting velocities
REAL (KIND = 8) :: vel_res(1:3)
real (kind = 8) :: dvel_res_dt (1:3)

INTEGER :: PolyTest
integer (kind = 4) :: ps, ps_id

INTEGER :: byparts

!initializing
PP(1) = 1
PP(2) = 1

do i = 1, 3
  vel_res (i) = 0.0d0
  dvel_res_dt (i) = 0.0d0
enddo

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
  sfint(j)       = 0.0
  dsfintdh1(j)   = 0.0
  beiint(j)      = 0.0
  beiint1(j)     = 0.0
  beiint2(j)     = 0.0
  dbeiintdh(j)   = 0.0
  dbeiintdh1(j)  = 0.0
  dbeiintdh2(j)  = 0.0
  d2beiintdh(j)  = 0.0
  d2beiintdh1(j) = 0.0
  d2beiintdh2(j) = 0.0
  dbeiintdx(j)   = 0.0
  d2beiintdhdx(j)= 0.0
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

!initialize gravitation factor for unit system
IF (GRAV < 32.)  THEN
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


!testoutput-Variable is now defined in control file, line C7
!testoutput = 0: output switched off
!testoutput = 1: output of partly values of equations and output of matrces
!testoutput = 2: output of variables at the end
!testoutput = 3: output of beiwert-variable

do i = 1, 3, 2
  n1 = nop(nn,i)

!fix waterdepth for h-boundaries  
  if (byparts == 1) then
    IF (MOD (NFIX (N1) / 100, 10) == 2) THEN
      vel (3, n1) = spec (n1, 3)
    end if
  endif

!Check, whether all necessary parameters are given  
!A-Polynom test  
  PolyTest = 3
  ACheck: do j = 0, 4
    if (apoly (1, n1, j) /= 0.0) then
      PolyTest = PolyTest - 1
      exit ACheck
    endif
  ENDDO ACheck
!QSch-Polynom test  
  QCheck: do j = 0, 4
    if (qpoly(1, n1, j) /= 0.0) then
      PolyTest = PolyTest - 2
      exit Qcheck
    endif
  ENDDO QCheck

!ERROR - messages for missing polynom data  
  if (PolyTest == 1 .AND. ( .NOT. IntPolProf(n1))) then
    call ErrorMessageAndStop (1104, N1, cord (N1, 1), cord (N1, 2))
  ELSEIF (PolyTest == 2 .AND. ( .NOT. IntPolProf (n1))) then
    call ErrorMessageAndStop (1105, N1, cord (N1, 1), cord (N1, 2))
  ELSEIF (PolyTest == 3 .AND. ( .NOT. IntPolProf (n1))) then
    call ErrorMessageAndStop (1106, N1, cord (N1, 1), cord (N1, 2))
  endif
enddo

!Get the nodes of the element
N1 = NOP (NN, 1)
N3 = NOP (NN, 3)

!****************************************************************
!nis,mar07,com: Calculate 'area' of element and coordinate things
TEL = AREA (NN)
AREA (NN) = 0.

!cipk nov97
!      TVOL(NN)=0.

!getting local copy of corner node numbers
NCN = NCORN (NN)
DO N = 1, NCN
  NCON (N) = NOP (NN, N)
enddo

!for 1D-2D-transition elements
IF (NCN == 5 .AND. IMAT (NN) < 900) NCN = 3

!residual vector and Jacobi-Matrix (derivatives) arrays
NEF = NCN * NDF
DO I = 1, NEF
  F (I) = 0.0
  DO J = 1, NEF
    ESTIFM (I, J) = 0.0
  enddo
enddo

!process on element direction
IF (NTX == 0) THEN
  TH (NN) = ATAN2 (CORD (N3, 2) - CORD (N1, 2), CORD (N3, 1) - CORD (N1, 1))
ENDIF

!getting angles
CXX = COS (TH (NN))
SAA = SIN (TH (NN))

!Set number of Gauss-points
NGP = 4
NCNX = 2

!C-
!C-.....COMPUTE LOCAL CORDS.....
!C-

!local copy of water depths at corner nodes
h1 = vel (3, n1)
h3 = vel (3, n3)

!get reference node
NR = NCON (1)
DO k = 1, ncn
  N = NCON (K)
!Calculate angular difference between the principal element direction th and the direction at the node alfa  
  ANGDIF = TH (NN) - ALFA (N)

!Set a direction factor in dependency of the defintion direction at a node and of an element  
  IF (ABS (ANGDIF) > 1.5708 .AND. ABS (ANGDIF) < 4.71779) THEN
    QFACT (K)  = -1.0
    QQFACT (K) = -1.0
  ELSE
    QFACT (K)  = 1.0
    QQFACT (K) = 1.0
  ENDIF

!Calculate x- and y- distance from node to reference node. With linear approach, an element might be curved  
  DX = CORD (N, 1) - CORD (NR, 1)
  DY = CORD (N, 2) - CORD (NR, 2)

!Calculate the direct distance between the nodes and the perpendicular distance from the node to the element chord  
  XL (K) =  DX * CXX + DY * SAA
  YL (K) = -DX * SAA + DY * CXX

!Calculate the main velocity at node k (local number)  
  vel_res (k) = vel (1, n) * COS (alfa (n)) + vel (2, n) * SIN (alfa (n))
  if (icyc > 0) dvel_res_dt (k) = vdot (1, n) * COS (alfa (n)) + vdot (2, n) * SIN (alfa (n))

!updating length, if kilometres are given  
  if (kmx(n) /= -1.0 .AND. kmx(nr) /= -1.0 .AND. nr /= n) then
!Scaling the element-length    
!write(*,*) nn, k    
    xl(3) = xl(k) / ABS (xl(k)) * ABS ((kmx(n3) - kmx(n1)) * 1000)
!Questionable: What happens to curved elements? Might it be useful to introduce them for curved elements    
    yl(3) = 0.0
  end if
enddo

!Questionable: Why is the length always averaged. It is also done like this in coef1nt's code
xl(2) = xl(3)/2
yl(2) = yl(3)/2

!Questionable: What is EINA respectively EINX and EiNY standing for?
!CIPK JAN03
EINA = EINX (NN) * CX + EINY (NN) * SA

!C-
!C-.....COMPUTE ELEMENT EQUATIONS.....
!C-
!Questionable: What is TFR standing for?
TFR=TEL/ABS(XL(3))

!CIPK MAY04 RESET ELEMENT INFLOW
IF (INOFLOW (NN) == 0) THEN
  SIDFQ = SIDF (NN)
ELSE
  SIDFQ = 0.
ENDIF

!Questionable: Why is there such a confusing name handling with the inflow term?
!SIDFQQ=SIDF(NN)
sidft = sidfq

!Question: Shouldn't be the form of the other coefs be used, i.e. the derivative form of the bed coordinates
!bedslope
if (kmx(n1) == 0.0 .AND. kmx(n3) == 0.0) then
  sbot = (ao(n1) - ao(n3)) / (  (cord(n3,1) - cord(n1,1))*COS(th(nn)) + (cord(n3,2) - cord(n1,2))*SIN(th(nn)))
else
  sbot = (ao(n1) - ao(n3)) / xl(3)
endif

!Calculation case
if (ntx == 1) then


  do i = 1, 2

!corner node number, corner node and waterdepth    
    j = i * 2 - 1
    n = ncon(j)
    h = vel(3, n)

!test for valid water depth range    
    if (h < hhmin(n) .AND. ntx == 1 .AND. ( .NOT. IntPolProf (n))) then
      WRITE (*,*) 'WARNING - waterdepth', vel(3, n), ' at node', n, '(kmx: ', kmx (n), ') less than Hmin', hhmin(n)
    ELSEIF (h > hhmax (n) .AND. ntx == 1 .AND. ( .NOT. IntPolProf (n))) then
      WRITE (*,*) 'WARNING - waterdepth', vel(3, n), ' at node', n, '(kmx: ', kmx (n), ') greater than Hmax', hhmax(n)
    end if
    
    if (IntPolProf (n)) then
      if (h < max (hhmin (NeighProf(n, 1)), hhmin(NeighProf(n, 2)))) then
        WRITE (*,*) 'WARNING - waterdepth ', vel(3, n), ' at INTERPOLATED node', n, '(kmx: ', kmx (n), ') less than Hmin of one of the original neighbouring nodes!'
      elseif (h > min (hhmax (NeighProf (n, 1)), hhmax (NeighProf (n, 2)))) then
        WRITE (*,*) 'WARNING - waterdepth ', vel(3, n), ' at INTERPOLATED node', n, '(kmx: ', kmx (n), ') greater than Hmax of one of the original neighbouring nodes!'
      endif
    endif
    

!look for the position of the polynomial    
!TODO: This should be replaced by a binary search    
    if ( .NOT. IntPolProf(n)) then
      PPA(1) = findPolynom (polyRangeA (n, :), vel(3, n), PolySplitsA (n), cord(n,1), cord (n,2), n)
      PPA(2) = PPA(1)
    else
      PPA(1) = findPolynom (polyRangeA (NeighProf (n, 1), :), vel (3, n), PolySplitsA (NeighProf (n, 1)), cord (NeighProf(n,1), 1), cord (NeighProf(n,1), 2), n)
      PPA(2) = findPolynom (polyRangeA (NeighProf (n, 2), :), vel (3, n), PolySplitsA (NeighProf (n, 2)), cord (NeighProf(n,2), 1), cord (NeighProf(n,2), 2), n)
    end if


    if ( .NOT. IntPolProf (n)) then
!A(h)      
      ah(n)    = CalcPolynomial (apoly (PPA (1), n, 0:4), h, ubound (apoly, 3))
    else
!A(h)      
      ah(n)    =   (1.0 - kmWeight (n)) * CalcPolynomial (apoly (PPA (1), NeighProf (n, 1), 0: 4), h, ubound (apoly, 3)) &
               & +        kmWeight (n)  * CalcPolynomial (apoly (PPA (2), NeighProf (n, 2), 0: 4), h, ubound (apoly, 3))
    endif

!Check for critical/ subcritical discharge    
    if (vel_res(j) / sqrt (grav * h) > 1) WRITE (*,*) 'Supercritical flow at node ', n, kmx(n)

    dahdh(n) = 0.0D0

    do j = 1, 2
!get the node to calculate values from for node n      
      if (IntPolProf(n)) then
        nod = NeighProf (n, j)
      else
        nod = n
      end if

!get the weighting for the calculated value influencing node n      
      if (j == 1) then
        if (IntPolProf (n)) then
          LocalWeight = (1.0 - kmWeight (n))
        else
          LocalWeight = 1.0D0
        end if
      ELSEIF (j == 2) then
        if (IntPolProf (n)) then
          LocalWeight = kmWeight (n)
        else
          LocalWeight = 0.0D0
        end if
      end if

!dA(h)/dh      
      dahdh(n)  = dahdh (n) + LocalWeight * calcPolynomial1stDerivative (apoly (PPA (j), nod, 0:4), vel(3, n), ubound (apoly, 3))

    end do
  enddo
!ntx=1
ENDIF 

!calculate additional energy losses regarding the continuous widening or contraction of the element
!slope_l = CalcSlope_l (vel (1:2, n1), vel (1:2, n3), ah (n1), ah (n3), dahdh (n1), dahdh (n3), cord (n1, 1:2), cord (n3, 1:2), grav)

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

  IF(NTX == 0) THEN
    DYDX=YL(2)*DNX(2)+YL(3)*DNX(3)
    ALF=ATAN(DYDX)
    CSALF=COS(ALF)
    TEMP=TEMP/CSALF
  ELSE
    TEMP=TEMP*TFR
  ENDIF
  IF(NTX /= 0) THEN
    DO J=1,3
      DNX(J)=DNX(J)/TFR
    enddo
  ENDIF

!linear shape-functions  
  XM(1)  = 1.0 - AFACT(I)
  XM(2)  = AFACT(I)
  DMX(1) = -1.0 / TEMP
  DMX(2) = 1. / TEMP

  IF(NTX /= 0) THEN
!Questionable: Why do we not use just one local variable for the parameters at the Gauss-nodes. They are just used once    
    H = VEL (3, N1) * XM (1) + VEL (3, N3) * XM (2)
  ELSE
    H=1.0
  ENDIF

  AMW=ABS(TEMP)*HFACT(I)/2.
  AREA(NN)=AREA(NN)+AMW

  ams = amw * rho
  amu = amw


  IF (NTX == 0) CYCLE gaussloop
!cipk nov97

  TVOL(NN) = TVOL (NN) + AMW

  IF (NTX /= 3) then

!primary variables h and v  
!h at GP  
  hhint(i)    = h1     * xm(1)      + h3     * xm(2)
!dh/dx at GP  
  dhhintdx(i) = h1     * dmx(1)     + h3     * dmx(2)
!dh/dt at GP  
  dhintdt (i) = xm(1) * vdot (3, n1) + xm (2) * vdot (3, n3)

  do j = 1, 3
!v at GP    
    vflowint(i) = vflowint(i) + xn(j)  * vel_res(j) * qfact(j)
!dv/dx at GP    
    dvintdx(i)  = dvintdx(i)  + dnx(j) * vel_res(j) * qfact(j)
!dv/dt at GP    
    dvintdt(i)  = dvintdt(i)  + xn(j)  * dvel_res_dt(j) * qfact(j)
  end do

  do k = 1, 2

!get nodes to calculate values from, might be interpolated    
    if (IntPolProf (n1)) then
      RefNodeN1 = NeighProf (n1, k)
      if (k == 1) then
        WeightN1 = 1.0D0 - kmWeight (n1)
      else
        WeightN1 = kmWeight (n1)
      endif
    else
      RefNodeN1 = n1
      if (k == 1) then
        WeightN1 = 1.0D0
      else
        WeightN1 = 0.0D0
      end if
    end if

    if (IntPolProf (n3)) then
      RefNodeN3 = NeighProf (n3, k)
      if (k == 1) then
        WeightN3 = 1.0 - kmWeight (n3)
      else
        WeightN3 = kmWeight (n3)
      end if
    else
      RefNodeN3 = n3
      if (k == 1) then
        WeightN3 = 1.0D0
      else
        WeightN3 = 0.0D0
      end if
    end if

!polynomial position for a(h)-calculations    
    PP(1) = findPolynom (polyRangeA (RefNodeN1, :), hhint(i), PolySplitsA (RefNodeN1), cord (RefNodeN1, 1), cord (RefNodeN1, 2), RefNodeN1)
    PP(2) = findPolynom (polyRangeA (RefNodeN3, :), hhint(i), PolySplitsA (RefNodeN3), cord (RefNodeN3, 1), cord (RefNodeN3, 1), RefNodeN3)

!secondary variables: area and its derivatives    
!A at GP    
    if (k == 1) then
      aint1(i)   = 0.0D0
      aint2(i)   = 0.0D0
    end if
    aint1(i) = aint1(i) + WeightN1 * CalcPolynomial (apoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (apoly, 3))
    aint2(i) = aint2(i) + WeightN3 * CalcPolynomial (apoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (apoly, 3))
    if (k == 2) areaint(i) = xm(1) * aint1(i) + xm(2) * aint2(i)

!Integration of A over h at GP    
    if (k == 1) then
      inta1(i) = 0.0D0
      inta2(i) = 0.0D0
    end if
    inta1(i) = inta1(i) + WeightN1 * CalcPolynomialIntegral (apoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (apoly, 3))
    inta2(i) = inta2(i) + WeightN3 * CalcPolynomialIntegral (apoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (apoly, 3))
    if (k == 2) Intareaint(i) = xm(1) * inta1(i) + xm(2) * inta2(i)

!dA/dt at GP    
!Equation 8.28, page 65    
    if (k == 1) then
      daintdh1(i) = 0.0D0
      daintdh2(i) = 0.0D0
    end if
    daintdh1(i)   = daintdh1(i) + WeightN1 * calcPolynomial1stDerivative (apoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (apoly, 3))
    daintdh2(i)   = daintdh2(i) + WeightN3 * calcPolynomial1stDerivative (apoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (apoly, 3))
    if (k == 2) dareaintdh(i) = xm(1) * daintdh1(i) +  xm(2) * daintdh2(i)

!d2A/dh2 at GP    
    if (k == 1) then
      d2aintdh1(i) = 0.0D0
      d2aintdh2(i) = 0.0D0
    end if
    d2aintdh1(i)   = d2aintdh1(i) + WeightN1 * calcPolynomial2ndDerivative (apoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (apoly, 3))
    d2aintdh2(i)   = d2aintdh2(i) + WeightN3 * calcPolynomial2ndDerivative (apoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (apoly, 3))
    if (k == 2) d2areaintdh(i) = xm(1) * d2aintdh1(i) +  xm(2) * d2aintdh2(i)

    if (k == 2) then
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
    end if

!sec    

!supercritical or subcritical flow    
    if (vflowint(i) / sqrt(grav * hhint(i)) > 1.0) then
      WRITE(*,*) 'WARNING - Critical discharge in element', nn
    end if

!polynomial position for a(h)-calculations    
    PP(1) = findPolynom (polyRangeQ (RefNodeN1, :), hhint(i), PolySplitsQ (RefNodeN1), cord (RefNodeN1, 1), cord (RefNodeN1, 2), RefNodeN1)
    PP(2) = findPolynom (polyRangeQ (RefNodeN3, :), hhint(i), PolySplitsQ (RefNodeN3), cord (RefNodeN3, 1), cord (RefNodeN3, 3), RefNodeN3)
!secondary variables: reference-discharge and its derivatives    
!QSch at GP    
    if (k == 1) then
      qschint1(i) = 0.0D0
      qschint2(i) = 0.0D0
    end if
    qschint1(i) = qschint1(i) + WeightN1 * CalcPolynomial (qpoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (qpoly, 3))
    qschint2(i) = qschint2(i) + WeightN3 * CalcPolynomial (qpoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (qpoly, 3))
    if (k == 2) qschint(i) = xm(1) * qschint1(i) + xm(2) * qschint2(i)

!dQSch/dh at GP    
    if (k == 1) then
      dqsintdh1(i) = 0.0D0
      dqsintdh2(i) = 0.0D0
    end if
    dqsintdh1(i) = dqsintdh1(i) + WeightN1 * CalcPolynomial1stDerivative (qpoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (qpoly, 3))
    dqsintdh2(i) = dqsintdh2(i) + WeightN3 * CalcPolynomial1stDerivative (qpoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (qpoly, 3))
    if (k == 2) dqsintdh(i) = xm(1) * dqsintdh1(i) + xm(2) * dqsintdh2(i)

!d2QSch/dh2 at GP    
    if (k == 1) then
     d2qsidh1(i) = 0.0D0
     d2qsidh2(i) = 0.0D0
    end if
    d2qsidh1(i) = d2qsidh1(i) + WeightN1 * CalcPolynomial2ndDerivative (qpoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (qpoly, 3))
    d2qsidh2(i) = d2qsidh2(i) + WeightN3 * CalcPolynomial2ndDerivative (qpoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (qpoly, 3))
    if (k == 2) d2qsidh(i) = xm(1) * d2qsidh1(i) + xm(2) * d2qsidh2(i)

    if (k == 2) then
!dQSch/dx at GP      
      dqsintdx(i)  = dmx(1) * (qschint1(i) + h1 * dqsintdh(i)) + dmx(2) * (qschint2(i) + h3 * dqsintdh(i))
!d2QSch/dhdx at GP      
      d2qsidhdx(i) = dmx(1) * dqsintdh1(i) + xm(1) * d2qsidh1(i) * dhhintdx(i) &
      &             +dmx(2) * dqsintdh2(i) + xm(2) * d2qsidh2(i) * dhhintdx(i)

!secondary variables: friction-slope and its derivatives      

!Sf,0 at GP      
      s0schint(i) = qgef(n1) * xm(1) + qgef(n3) * xm(2)
!Sf at GP      
      sfint(i) = s0schint(i) * vflowint(i) * ABS (vflowint(i)) * areaint(i)**2 / qschint(i)**2

!dSf/dh at GP      
      dsfintdh1(i) = s0schint(i) * vflowint(i) * ABS(vflowint(i))                      &
      &              * (QSchint(i)**2 * 2 * areaint(i) * dareaintdh(i) - areaint(i)**2 &
      &              * 2 * Qschint(i) * dqsintdh(i)) / Qschint(i)**4
    ENDIF

!now do the flow coeficient    
!**************************    

!flow coeficient not considered    
    IF (beient == 0) THEN
      beiint (i)       = 1.0
      dbeiintdh  (i)   = 0.0
      d2beiintdh (i)   = 0.0
      dbeiintdx (i)    = 0.0
      d2beiintdhdx (i) = 0.0

!use momentum coeficient    
    ELSEIF (beient == 1) THEN
!polynomial position for a(h)-calculations      
      PP(1) = findPolynom (polyRangeB (RefNodeN1, :), hhint(i), PolySplitsB (RefNodeN1), cord (RefNodeN1, 1), cord (RefNodeN1, 2), RefNodeN1)
      PP(2) = findPolynom (polyRangeB (RefNodeN3, :), hhint(i), PolySplitsB (RefNodeN3), cord (RefNodeN3, 1), cord (RefNodeN3, 2), RefNodeN3)

!beta at GP      
      IF (k == 1) THEN
        beiint1(i) = 0.0D0
        beiint2(i) = 0.0D0
      ENDIF
      beiint1(i) = beiint1(i) + WeightN1 * CalcPolynomial (Alphapoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (Alphapoly, 3))
      beiint2(i) = beiint2(i) + WeightN3 * CalcPolynomial (Alphapoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (Alphapoly, 3))
      IF (k == 2) beiint(i) = xm(1) * beiint1(i) + xm(2) * beiint2(i)

!dbeta/dh at GP      
      IF (k == 1) THEN
        dbeiintdh1(i) = 0.0D0
        dbeiintdh2(i) = 0.0D0
      ENDIF
      dbeiintdh1(i) = dbeiintdh1(i) + WeightN1 * CalcPolynomial1stDerivative (Alphapoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (Alphapoly, 3))
      dbeiintdh2(i) = dbeiintdh2(i) + WeightN3 * CalcPolynomial1stDerivative (Alphapoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (Alphapoly, 3))
      if (k == 2) dbeiintdh(i) = xm(1) * dbeiintdh1(i) + xm(2) * dbeiintdh2(i)

!d2beta/dh2 at GP      
      IF (k == 1) THEN
       d2beiintdh1(i) = 0.0D0
       d2beiintdh2(i) = 0.0D0
      ENDIF
      d2beiintdh1(i) = d2beiintdh1(i) + WeightN1 * CalcPolynomial2ndDerivative (Alphapoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (Alphapoly, 3))
      d2beiintdh2(i) = d2beiintdh2(i) + WeightN3 * CalcPolynomial2ndDerivative (Alphapoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (Alphapoly, 3))
      IF (k == 2) d2beiintdh(i) = xm(1) * d2beiintdh1(i) + xm(2) * d2beiintdh2(i)

!derivatives over dx      
      IF (k == 2) THEN
!dQSch/dx at GP        
        dbeiintdx(i)  = dmx(1) * (beiint1(i) + h1 * dbeiintdh(i)) + dmx(2) * (beiint2(i) + h3 * dbeiintdh(i))
!d2QSch/dhdx at GP        
        d2beiintdhdx(i) = dmx(1) * dbeiintdh1(i) + xm(1) * d2beiintdh1(i) * dhhintdx(i) &
        &               + dmx(2) * dbeiintdh2(i) + xm(2) * d2beiintdh2(i) * dhhintdx(i)
      ENDIF


!use energy coeficient    
    ELSEIF (beient == 2) THEN
!polynomial position for a(h)-calculations      
      PP(1) = findPolynom (polyRangeB (RefNodeN1, :), hhint(i), PolySplitsB (RefNodeN1), cord (RefNodeN1, 1), cord (RefNodeN1, 2), RefNodeN1)
      PP(2) = findPolynom (polyRangeB (RefNodeN3, :), hhint(i), PolySplitsB (RefNodeN3), cord (RefNodeN3, 1), cord (RefNodeN3, 2), RefNodeN3)

!beta at GP      
      IF (k == 1) THEN
        beiint1(i) = 0.0D0
        beiint2(i) = 0.0D0
      ENDIF
      beiint1(i) = beiint1(i) + WeightN1 * CalcPolynomial (Betapoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (Betapoly, 3))
      beiint2(i) = beiint2(i) + WeightN3 * CalcPolynomial (Betapoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (Betapoly, 3))
      IF (k == 2) beiint(i) = xm(1) * beiint1(i) + xm(2) * beiint2(i)

!dbeta/dh at GP      
      IF (k == 1) THEN
        dbeiintdh1(i) = 0.0D0
        dbeiintdh2(i) = 0.0D0
      ENDIF
      dbeiintdh1(i) = dbeiintdh1(i) + WeightN1 * CalcPolynomial1stDerivative (Betapoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (Betapoly, 3))
      dbeiintdh2(i) = dbeiintdh2(i) + WeightN3 * CalcPolynomial1stDerivative (Betapoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (Betapoly, 3))
      IF (k == 2) dbeiintdh(i) = xm(1) * dbeiintdh1(i) + xm(2) * dbeiintdh2(i)

!d2beta/dh2 at GP      
      IF (k == 1) THEN
       d2beiintdh1(i) = 0.0D0
       d2beiintdh2(i) = 0.0D0
      ENDIF
      d2beiintdh1(i) = d2beiintdh1(i) + WeightN1 * CalcPolynomial2ndDerivative (Betapoly (PP(1), RefNodeN1, 0:4), hhint(i), ubound (Betapoly, 3))
      d2beiintdh2(i) = d2beiintdh2(i) + WeightN3 * CalcPolynomial2ndDerivative (Betapoly (PP(2), RefNodeN3, 0:4), hhint(i), ubound (Betapoly, 3))
      IF (k == 2) d2beiintdh(i) = xm(1) * d2beiintdh1(i) + xm(2) * d2beiintdh2(i)

!derivatives over dx      
      IF (k == 2) THEN
!dQSch/dx at GP        
        dbeiintdx(i)  = dmx(1) * (beiint1(i) + h1 * dbeiintdh(i)) + dmx(2) * (beiint2(i) + h3 * dbeiintdh(i))
!d2QSch/dhdx at GP        
        d2beiintdhdx(i) = dmx(1) * dbeiintdh1(i) + xm(1) * d2beiintdh1(i) * dhhintdx(i) &
        &               + dmx(2) * dbeiintdh2(i) + xm(2) * d2beiintdh2(i) * dhhintdx(i)
      ENDIF


!switch of convective terms    
    ELSEIF (beient == 3) THEN
      beiint (i)       = 0.0
      dbeiintdh  (i)   = 0.0
      d2beiintdh (i)   = 0.0
      dbeiintdx (i)    = 0.0
      d2beiintdhdx (i) = 0.0

!wrong input leads to usage of beient == 0    
    ELSE
      beiint (i)       = 1.0
      dbeiintdh  (i)   = 0.0
      d2beiintdh (i)   = 0.0
      dbeiintdx (i)    = 0.0
      d2beiintdhdx (i) = 0.0
    ENDIF

!mass center calculations    
!    if (byparts == 3) then
!      !polynomial position for a(h)-calculations
!      PP(1) = findPolynom (polyRangeA (n1, :), hhint(i), PolySplitsA (n1))
!      PP(2) = findPolynom (polyRangeA (n3, :), hhint(i), PolySplitsA (n3))
!
!      !dAintdx
!      daintdx1(i) = daintdh1(i) * dhhintdx(i)
!      daintdx2(i) = daintdh2(i) * dhhintdx(i)
!      !d2aintdxdh
!      d2aintdxdh1(i) = d2aintdh1(i) * dhhintdx(i)
!      d2aintdxdh2(i) = d2aintdh2(i) * dhhintdx(i)
!      !Fint
!      Fint1(i) = 0.0
!      Fint2(i) = 0.0
!      do j = 0, 4
!        Fint1(i) = Fint1(i) + (j / (j+1.)) * apoly (PP(1), n1, j) * hhint(i)**(j+1)
!        Fint2(i) = Fint2(i) + (j / (j+1.)) * apoly (PP(2), n3, j) * hhint(i)**(j+1)
!      enddo
!      !dFintdh
!      dFintdh1(i) = 0.0
!      dFintdh2(i) = 0.0
!      do j = 0, 4
!        dFintdh1(i) = dFintdh1(i) + (j) * apoly (PP(1), n1, j) * hhint(i)**j
!        dFintdh2(i) = dFintdh2(i) + (j) * apoly (PP(2), n3, j) * hhint(i)**j
!      end do
!      !d2Fintdh
!      d2Fintdh1(i) = 0.0
!      d2Fintdh2(i) = 0.0
!      do j = 0, 4
!        d2Fintdh1(i) = d2Fintdh1(i) + (j**2) * apoly (PP(1), n1,j) * hhint(i)**(j-1)
!        d2Fintdh2(i) = d2Fintdh2(i) + (j**2) * apoly (PP(2), n3,j) * hhint(i)**(j-1)
!      end do
!      !dFintdx
!      dFintdx1(i) = 0.0
!      dFintdx2(i) = 0.0
!      dFintdx1(i) = dFintdh1(i) * dhhintdx(i)
!      dFintdx2(i) = dFintdh2(i) * dhhintdx(i)
!      !d2Fintdxdh1
!      d2Fintdxdh1(i) = d2Fintdh1(i) * dhhintdx(i)
!      d2Fintdxdh2(i) = d2Fintdh2(i) * dhhintdx(i)
!      if (testoutput == 3) then
!        WRITE(*,*)
!        WRITE(*,*) 'F1: ', fint1(i)
!        WRITE(*,*) 'F2: ', fint2(i)
!        WRITE(*,*) 'dF1/dh: ', dFintdh1(i)
!        WRITE(*,*) 'dF2/dh: ', dFintdh2(i)
!        WRITE(*,*) 'd2F1/dh2: ', d2Fintdh1(i)
!        WRITE(*,*) 'd2F2/dh2: ', d2fintdh2(i)
!        WRITE(*,*) 'd2F1/dhdx: ', d2Fintdxdh1(i)
!        WRITE(*,*) 'd2F2/dhdx: ', d2Fintdxdh2(i)
!        WRITE(*,*)
!      end if
!
!      !zS-Calculations
!      !zS
!      zsint(i) = xm(1) * Fint1(i) / aint1(i) &
!      &        + xm(2) * Fint2(i) / aint2(i)
!
!      !dzS/dh
!      dzsintdh(i) = xm(1) * (aint1(i) * dFintdh1(i) - fint1(i) * daintdh1(i)) / aint1(i)**2 &
!      &           + xm(2) * (aint2(i) * dFintdh2(i) - fint2(i) * daintdh2(i)) / aint2(i)**2
!
!      !d2zS/dh2
!      d2zsintdh(i) = xm(1) * (aint1(i) * d2Fintdh1(i) - fint1(i) * d2aintdh1(i)                                            &
!      &                       - 2. * dFintdh1(i) * daintdh1(i) + 2. * fint1(i) / aint1(i) * daintdh1(i)**2) / aint1(i)**2  &
!      &            + xm(2) * (aint2(i) * d2Fintdh2(i) - fint2(i) * d2aintdh2(i)                                            &
!      &                       - 2. * dFintdh2(i) * daintdh2(i) + 2. * fint2(i) / aint2(i) * daintdh2(i)**2) / aint2(i)**2
!
!      !d2zS/(dhdx)
!      d2zsintdhdx(i) = xm(1) * (- daintdx1(i) * dFintdh1(i) + aint1(1) * d2Fintdxdh1(i) - dFintdx1(i) * daintdh1(i)                &
!      &                         - fint1(i) * d2aintdxdh1(i) + 2. * daintdx1(i) * fint1(i) / aint1(i) * daintdh1(i)) / aint1(i)**2  &
!      &              + xm(2) * (- daintdx2(i) * dFintdh2(i) + aint2(1) * d2Fintdxdh2(i) - dFintdx2(i) * daintdh2(i)                &
!      &                         - fint2(i) * d2aintdxdh2(i) + 2. * daintdx2(i) * fint2(i) / aint2(i) * daintdh2(i)) / aint2(i)**2
!
!      !ypsilon
!      yps(i)  = xm(1) * (1. - 1. / hhint(i) * fint1(i) / aint1(i)) &
!      &       + xm(2) * (1. - 1. / hhint(i) * fint2(i) / aint2(i))
!
!      !dypsilon/dh
!      dypsdh(i)  = - xm(1) * (  hhint(i) * aint1(i) * dFintdh1(i) - fint1(i) * (aint1(i) &
!      &                       + hhint(i) * daintdh1(i))) / (hhint(i) * aint1(i))**2      &
!      &            - xm(2) * (  hhint(i) * aint2(i) * dFintdh2(i) - fint2(i) * (aint2(i) &
!      &                       + hhint(i) * daintdh2(i))) / (hhint(i) * aint2(i))**2
!
!      !d2ypsilon/dh2
!      d2ypsdh(i) = - xm(1) * (- 2. * hhint(i) * daintdh1(i) * dFintdh1(i) + 2. * fint1(i) * daintdh1(i) - aint1(i) * dFintdh1(i) &
!      &                       + 2. * fint1(i) * aint1(i) / hhint(i) + hhint(i) * aint1(i) * d2Fintdh1(i)                         &
!      &                       - hhint(i) * fint1(i) * d2aintdh1(i) + 2. * fint1(i) * hhint(i) / aint1(i) * (daintdh1(i))**2)     &
!      &                    / (hhint(i) * aint1(i) )**2                                                                           &
!      &            - xm(2) * (- 2. * hhint(i) * daintdh2(i) * dFintdh2(i) + 2. * fint2(i) * daintdh2(i) - aint2(i) * dFintdh2(i) &
!      &                       + 2. * fint2(i) * aint2(i) / hhint(i) + hhint(i) * aint2(i) * d2Fintdh2(i)                         &
!      &                       - hhint(i) * fint2(i) * d2aintdh2(i) + 2. * fint2(i) * hhint(i) / aint2(i) * (daintdh2(i))**2)     &
!      &                    / (hhint(i) * aint2(i) )**2
!      !dypsilon/dx
!      dypsdx(i) = dmx(1) * (1. - fint1(i) / hhint(i) / aint1(i))                                                &
!      &         + xm(1) * (dFintdh1(i) - fint1(i) / hhint(i) - fint1(i) / aint1(i) * daintdh1(i)) * dhhintdx(i) &
!      &         + dmx(2) * (1. - fint2(i) / hhint(i) / aint2(i))                                                &
!      &         + xm(1) * (dFintdh2(i) - fint2(i) / hhint(i) - fint2(i) / aint2(i) * daintdh2(i)) * dhhintdx(i)
!
!      !d2ypsilon/(dhdx)
!      d2ypsdhdx(i) = - xm(1) * (dhhintdx(i) * (2. * aint1(i) / hhint(i) * fint1(i) - aint1(i) * dFintdh1(i) - daintdh1(i) * fint1(i))&
!      &                       + daintdx1(i) * (fint1(i) - hhint(i) * dFintdh1(i) - 2. * hhint(i) / aint1(i) * daintdh1(i) * fint1(i))&
!      &                       + dFintdx1(i) * (hhint(i) * daintdh1(i) - aint1(i))                                                    &
!      &                       + hhint(i) * aint1(i) * d2Fintdxdh1(i) + hhint(i) * fint1(i) * d2aintdxdh1(i))                         &
!      &                    / (hhint(i) * aint1(i) )**2                                                                               &
!      &              - dmx(1) * (hhint(i) * aint1(i) * dFintdh1(i) - fint1(i) * aint1(i) + fint1(i) * hhint(i) * daintdh1(i))        &
!      &                     / (hhint(i) * aint1(i))**2                                                                               &
!      &              - xm(2) * (dhhintdx(i) * (2. * aint2(i) / hhint(i) * fint2(i) - aint2(i) * dFintdh2(i) - daintdh2(i) * fint2(i))&
!      &                       + daintdx2(i) * (fint2(i) - hhint(i) * dFintdh2(i) - 2. * hhint(i) / aint2(i) * daintdh2(i) * fint2(i))&
!      &                       + dFintdx2(i) * (hhint(i) * daintdh2(i) - aint2(i))                                                    &
!      &                       + hhint(i) * aint2(i) * d2Fintdxdh2(i) + hhint(i) * fint2(i) * d2aintdxdh2(i))                         &
!      &                    / (hhint(i) * aint2(i) )**2                                                                               &
!      &              - dmx(2) * (hhint(i) * aint2(i) * dFintdh2(i) - fint2(i) * aint2(i) + fint2(i) * hhint(i) * daintdh2(i))        &
!      &                     / (hhint(i) * aint2(i))**2
!
!      if (testoutput == 3 .OR. testoutput == 1) then
!        WRITE(*,*) 'Element: ', nn, 'GP: ', i
!        WRITE(*,*) 'h: ', hhint(i)
!        WRITE(*,*) 'zS: ', zsint(i)
!        WRITE(*,*) 'dzsintdh: ', dzsintdh(i)
!        WRITE(*,*) 'd2zsintdh: ', d2zsintdh(i)
!        WRITE(*,*) 'd2zsintdhdx: ', d2zsintdhdx(i)
!        WRITE(*,*) 'ypsilon: ', yps(i)
!        WRITE(*,*) 'dypsdh: ', dypsdh(i)
!        WRITE(*,*) 'd2ypsdh: ', d2ypsdh(i)
!        WRITE(*,*) 'dypsdx: ', dypsdx(i)
!        WRITE(*,*) 'd2ypsdhdx: ', d2ypsdhdx(i)
!        if (i == 4) pause
!      end if
!    end if
  end do

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
  FRN =                                                          &
!Term B: Convective term         
    &    + vflowint(i)**2 * areaint(i) * dbeiintdx(i)            &
!Term C: Convective term         
    &    + beiint(i) * vflowint(i)**2 * daintdx(i)               &
!Term D: Convective term         
    &    + 2. * beiint(i) * vflowint(i) * areaint(i) * dvintdx(i)


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
      &    - sidft * vflowint(i)

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
    FRN = FRN                                                    &
!Term A2: Unsteady term      
      &    + areaint(i) * dvintdt(i)                             &
!Term A1: Unsteady term      
      &    + vflowint(i) * daintdt(i)

  ENDIF

!Assemble equation values  
  do l = 1, 3
!equation number    
    ia = 1 + ndf * (l-1)
!equation    
    f(ia) = f(ia) - AMS * (xn(l) * FRN + dnx(l) * FRNX) * qfact(l)
  enddo

!testoutput  
!  if (testoutput == 1) &
!  &  call Mom (nn, i, icyc, byparts, areaint(i), daintdt(i), daintdx(i), dareaintdh(i), vflowint(i), dvintdt(i), &
!            & dvintdx(i), hhint(i), dhhintdx(i), beiint(i), dbeiintdx(i), zsint(i), yps(i), dypsdx(i), sfint(i), sbot, &
!            & xn, dnx, frn, frnx, ams, grav, sidft)


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
!IF(MOD(immt,100) > 90) GO TO 500  

!*********************************************************************************************************************************  
!MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH   MOMENTUM over DEPTH  
!*********************************************************************************************************************************  
  FEEAN = 0.0
  FEEBN = 0.0
  FEECN = 0.0

!FEEAN FEEAN FEEAN  
!*****************  
!Terms B - D: Convective terms  
  FEEAN =                                                                                          &
!Term B: Convective term          
    &   + vflowint(i)**2 * (dareaintdh(i) * dbeiintdx(i) + areaint(i) * d2beiintdhdx(i))           &
!Term C: Convective term          
    &   + vflowint(i)**2 * (dbeiintdh(i) * daintdx(i) + beiint(i) * d2aidhdx(i))                   &
!Term D: Convective term          
    &   + 2.0 * vflowint(i) * dvintdx(i) * (beiint(i) * dareaintdh(i) + areaint(i) * dbeiintdh(i))

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
    FEEAN = FEEAN                    &
!Term A1          
    &   + vflowint(i) * (d2areaintdh(i) * dhintdt(i) + dareaintdh(i) * altm) &
!Term A2          
    &   + dareaintdh(i) * dvintdt(i)

  ENDIF

!FEEBN FEEBN FEEBN  
!*****************  
!Terms B - D: Convective terms  
  FEEBN =                                                   &
!Term B: Convective term          
    &   + vflowint(i)**2 * areaint(i) * dbeiintdh(i)        &
!Term C: Convective term          
    &   + vflowint(i)**2 * beiint(i) * dareaintdh(i)

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

!testoutput  
!  if (testoutput == 1) &
!  & call MomOvDep (vflowint(i), dvintdt(i), dvintdx(i), hhint(i), dhintdt(i), dhhintdx(i), beiint(i), dbeiintdh(i), &
!  & d2beiintdhdx(i), areaint(i), daintdx(i), dareaintdh(i), d2areaintdh(i), d2aidhdx(i), sfint(i), dsfintdh1(i), yps(i), &
!  & dypsdx(i), dypsdh(i), d2ypsdhdx(i), zsint(i), dzsintdh(i), grav, sbot, icyc, byparts)


!**************************************************************************************************************************  
!MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY   MOMENTUM over VELOCITY  
!**************************************************************************************************************************  
  FEEAN = 0.0
  FEEBN = 0.0

!FEEAN FEEAN FEEAN  
!*****************  
!Terms B-D: Convective terms  
  FEEAN =                                                   &
!Term B: Convective term          
    &   + 2.0 * areaint(i)* vflowint(i) * dbeiintdx(i)      &
!Term C: Convective term          
    &   + 2.0 * vflowint(i) * beiint(i) * daintdx(i)        &
!Term D: Convective term          
    &   + 2.0 * beiint(i) * areaint(i) * dvintdx(i)

  FEEAN = FEEAN                                               &
!Term F: Friction term            
      &   + 2.0 * grav * sfint(i) * areaint(i) / vflowint(i)  &
!Term H: Sideflow term            
      &   - sidft

!Term A: unsteady term  
  if (icyc > 0) then
    FEEAN = FEEAN    &
!Term A1          
    &   + daintdt(i) &

!nis,jun08: This should be activated, but it doesn't work fine then!
!         !Term A2
    &   + areaint(i) * altm
!-

  end if

!FEEBN FEEBN FEEBN  
!*****************  
!Terms B-D: Convective terms  
  FEEBN =                                                   &
!Term D: Convective term          
    &   + 2.0 * beiint(i) * areaint(i) * vflowint(i)

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

!testoutput  
!  if (testoutput == 1) call MomOvVel (daintdt(i), areaint(i), vflowint(i), dbeiintdx(i), beiint(i), &
!                            &              daintdx(i), dvintdx(i), grav, sfint(i), icyc, sidft)


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
    &  + d2areaintdh(i) * dhintdt(i) + dareaintdh(i) * altm

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
!Term B       
    & + daintdx(i)
  EB =               &
!Term C       
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

  endif

ENDDO gaussloop

IF(NTX == 0) RETURN
IF(NTX == 3) RETURN

!IF(MOD(IMMT,100) > 90) GO TO 1305

!Boundary Conditions (Forces) - Waterdepth H
HBCAssign: DO L=1, NCN, 2
  N1=NCON(L)

  IF(MOD(NFIX(N1)/100,10) == 2) THEN


    speclocal = spec(n1, 3)
!TODO: This should be replaced by a binary search    
    PPA(1) = findPolynom (polyRangeA (n1, :), speclocal, PolySplitsA (n1), cord (N1, 1), cord (N1, 2), N1)

!1. Implementation; currently in use!!!    
!Do not apply differentiation by parts    
    if (byparts == 1) then
      NA = (L-1) * NDF + 1
      do iii=1, nef
        estifm (na, iii) = 0.0
      enddo

      estifm(na, na + 2) = 1.0
      f(na)              = 0.0

!2. Implementation; not working properly    
!Differentiation by parts with half a depth in natural boundary term    
!result is the application of a not physically senseful term    
    ELSEIF (byparts == 2) then
      NA = (L-1) * ndf + 1

!get required cross sectional flow area      
      ASoll = CalcPolynomial (apoly (PPA(1), n1, 0:4), speclocal, ubound (apoly, 3))

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

!3. Implementation; not working yet    
!Differentiation by parts with the real mass center point of the boundary cross section    
!result would be a physically senseful boundary term    
    ELSEIF (byparts == 3) then
      NA = (L-1) * ndf + 1

!find required cross sectional flow area      
      ASoll = CalcPolynomial (apoly (PPA(1), n1, 0:4), speclocal, ubound (apoly, 3))

      ppl = grav * ASoll * rho * qfact(l)

      if (l == 1) ppl = -ppl

!Calc zs of both depth      
      FBCSoll   = 0.0
      FBCIst    = 0.0
      dFBCistdh = 0.0
      do j = 0, 4
        FBCSoll = FBCSoll + (j / (j+1.)) * apoly (PPA(1), n1, j) * spec(n1,3)**(j+1)
        FBCIst  = FBCIst  + (j / (j+1.)) * apoly (PPA(1), n1, j) *  vel(3,n1)**(j+1)
        dFBCIstdh  = dFBCIstdh  + (j) * apoly (PPA(1), n1, j) *  vel(3,n1)**(j)
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

  ELSEIF (IBN(N1) >= 3) THEN
    IF (NREF(N1) == 0) THEN
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

  M = NCON (N)

!if no BC for Q or HQ then cycle  
  IF (NFIX (M)/ 1000 < 13) CYCLE QBCAssign

!stage-flow boundaries with e-formula  
  IF (ISTLIN (M) /= 0) THEN
    J   = ISTLIN (M)
    AC1 = STQ (J)
    AC2 = STQA (J)
    E0  = STQE (J)
    CP  = STQC (J)
!no stage flow boundaries or at least no formula to be applied; it still can be tabular data as stage flow boundary  
  ELSE
    AC2 = 0.
  ENDIF

!line of degree of freedom (velocity)  
  IRW = (N-1) * NDF + 1
!line of degree of freedom (depth)  
  IRH = IRW + 2

!cosinus and sinus of nodal direction angle  
  CX = COS (ALFA (M))
  SA = SIN (ALFA (M))

!current velocity  
  VT = VEL (1, M) * CX + VEL(2,M) * SA

!all other entrees are zero  
  DO J = 1, NEF
    ESTIFM (IRW, J) = 0.
  ENDDO

!install new boundary condition values  
!ah(m) is cross sectional area; area(nn) is "area" of element that means length  
  ESTIFM (IRW, IRW) = ah (m) * area (nn)
  ESTIFM (IRW, IRH) = dahdh (m) * vt * area (nn)
  F (IRW)           = (SPEC (M, 1) - VT * ah (m)) * area (nn)

!stage flow boundaries with e-formula  
  IF (AC2 /= 0.) THEN
!calculate water suface elevation    
    WSEL = VEL (3, M) +AO (M)

!form additional boundary condition terms regarding    
    ESTIFM (IRW, IRH) = ESTIFM (IRW, IRH) - AREA (NN) * (AC2 * CP * (WSEL - E0)**(CP - 1.0))
    F (IRW) = F (IRW) + AREA (NN) * (AC2 * (WSEL - E0)**CP)
!stage flow boundaries with tabular data  
  ELSEIF (istab (m) > 0.) then
!calculate water surface elevation    
    WSEL = VEL (3, M) + AO (M)

!direction factor (inflow/ outflow)    
    if (spec (m, 1) < 0.) then
      adir = -1.
    else
      adir = 1.
    end if

!get discharge and derivative of discharge in dependency of the water depth    
    call stfltab (m, WSEL, dfdh, ff, 1)

!form equations    
    estifm (irw, irh) = estifm (irw, irh) - area (nn) * dfdh * adir
    f (irw) = f (irw) + area (nn) * (ff * adir - spec (m, 1))
  ENDIF
ENDDO QBCAssign


!Correction for Coupling
TransitionCorrection: do l = 1, ncn, 2
  if (byparts == 2 .OR. byparts == 3) EXIT  TransitionCorrection
!if (byparts == 1) EXIT couplingcorrection  

!get node number  
  M = NCON(l)

!check for Transition membership  
  if (TransitionMember (M)) then

!Find line number and exit, if found    
    throughLines: do i = 1, MaxLT
      if (TransLines(i, 3) == M) then
        LiNo = TransLines(i, 2)
        TrID = i
        EXIT throughLines
      end if
    enddo throughLines

!this is the former momentum equation of the coupling node    
    IRW = (l-1) * NDF + 1
    IRH = IRW + 2

!get flow direction    
    CX = COS(alfa(m))
    SA = SIN(alfa(m))

!get velocity    
    VT = vel(1,m) * CX + vel(2,m) * sa

!get cross product with continuity line, because of mathematical discharge-direction    
!components of line    
    DXtot = (cord(line(LiNo,lmt(LiNo)),1) - cord(line(LiNo,1),1))
    DYtot = (cord(line(LiNo,lmt(LiNo)),2) - cord(line(LiNo,1),2))

!crossproduct    
    Qcrossproduct = vel(1,m) * DYtot - vel(2,m) * DXtot
!find correct sign of discharge    
    areacorrection = Qcrossproduct * VT / ABS( Qcrossproduct * VT)

!reset the Jacobian    
    do j = 1, nef
      ESTIFM (irw, j) = 0.0
    end do

!2D -> 1D (H-Q): TransLines (i, 4) = 1    
!2D <- 1D (Q-H): TransLines (i, 4) = 2    
!2D <> 1D (H-H): TransLines (i, 4) = 3    


    FindTransition: do i = 1, MaxLT
      if (TransLines (i, 1) == nn) EXIT FindTransition
    end do FindTransition

!2D-1D:    
    if (TransLines (i, 4) == 1) then
!set residual entry for 1D-node - 2D-line identity      
      f (irw)           = areacorrection * ah(m) * VT - q2D (TrID)
!set derivative over velocity      
      estifm (irw, irw) = - ah(m) * areacorrection
!set derivative over depth      
      estifm (irw, irh) = - VT * dahdh(m) * areacorrection
!1D-2D:    
    ELSEIF (TransLines (i, 4) == 2 .OR. TransLines (i, 4) == 3) then
!set residual entry for 1D-node - water stage restriction      
      WRITE(*,*) spec(m, 3), VEL (3, m), ao(m), VEL (3, m) + ao(m)
      f (irw)           = VEL (3, m) - (spec(m, 3) - ao (m))
      estifm (irw, irw) = 0.0
      estifm (irw, irh) = - 1.0
    end if
  end if
end do TransitionCorrection
!-

 1305 CONTINUE
 1320 CONTINUE
outer: DO I=1,NCN
  J=NCON(I)
  IA=NDF*(I-1)
  inner: DO K=1,NDF
    IA=IA+1
    JA=NBC(J,K)
    IF(JA == 0) CYCLE inner
    R1(JA)=R1(JA)+F(IA)
  ENDDO inner
enddo outer

!control output
!if (testoutput == 2) &
!  & call ElementResult (nn, nop (nn, 1), nop (nn, 3), h1, h3, sbot, xl, area(nn), vel_res, &
!                       &  ah(n1), ah(n3), dahdh(n1), dahdh(n3), hhint, dhhintdx, areaint, &
!                       &  dareaintdh, d2areaintdh, daintdx, d2aintdx, d2aidhdx, qschint, dqsintdh, d2qsidh, dqsintdx, s0schin, &
!                       &  sfint, sdfintdh1, vflowint, dvintdx)


!estifm-testoutput
!if (testoutput > -1) then
!
!  !testing
!  !do i = 1, 3, 2
!  !  if (TransitionMember (nop (nn, i)) ) then
!  !    WRITE(*,*) '1D-Knoten: ', nop(nn, i)
!  !    WRITE(*,*) '***********'
!  !    IA = NDF * (i - 1) + 3
!  !    WRITE(*,*) 'Gleichung: ', NBC (nop (nn, i), 3)
!  !    WRITE(*,*) 'Residuum : ', f (ia), 'Gesamt: ', r1(NBC (nop(nn,i), 3))
!  !  end if
!  !end do
!  !testing-
!
!  if (Transitionelement (nn)) then
!    WRITE(9919,*) 'Element ', nn, 'coef1Pol'
!    WRITE(9919, 1233) ( nbc (nop(nn,1), j), j=1,  3, 2), nbc (nop(nn,2), 1), ( nbc (nop(nn,3), j), j=1,  3, 2)
!
!    writematrix: do i = 1, 11, 2
!      if (i == 7) CYCLE writematrix
!
!      if (MOD(i,4) == 1 .OR. MOD(i,4) == 2) then
!        IA = nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4))
!        if (f (i) /= r1(ia)) then
!          WRITE(9919, 1235) ia, (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i), r1(ia)
!        else
!          WRITE(9919, 1234) ia, (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
!        endif
!      elseif (MOD(i,4) == 3 ) then
!        ia = nbc( nop(nn, 1+(i-MOD(i,4))/ 4), mod(i,4))
!        if (f (i) /= r1 (ia)) then
!          WRITE(9919, 1235) ia, (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i), r1(ia)
!        else
!          WRITE(9919, 1234) ia, (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
!        end if
!      ELSE
!        ia = nbc( nop(nn, i/4 ), 4)
!        if (f (i) /= r1 (ia)) then
!          WRITE(9919, 1235) ia, (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i), r1(ia)
!        else
!          WRITE(9919, 1234) ia, (estifm(i,j), j=1,  5, 2), (estifm(i,j), j=9, 11, 2), f(i)
!        end if
!      endif
!    end do writematrix
!    WRITE(9919,*)
!    WRITE(9919,*)
!  end if
!
! 1233 format (6x, 5(1x, i10))
! 1234 format (i6, 6(1x, f10.2))
! 1235 format (i6, 7(1x, f10.2))
!endif
!-

!end of normal subroutine run
RETURN

!*-
!*...... Special case for junction element
!*-


 2000 CONTINUE

!TODO
!nis,nov07: This code should be for calling the control structure subroutine, but it can never reach this place, because imat can only be 89 in this
!           subroutine
IF(IMAT(NN) > 903) THEN

!Find position in polynom range definitions  
  PP(1) = FindPolynom (PolyrangeA (n1, :), vel(3, n1), PolySplitsA (n1), cord (N1, 1), cord (N1, 2), N1)
  PP(2) = FindPolynom (PolyrangeA (n3, :), vel(3, N3), PolySplitsA (N3), cord (N3, 1), cord (N3, 2), N3)

!A(h)  
  ah(n1) = CalcPolynomial (apoly (PP(1), n1, 0:4), vel(3, n1), ubound (apoly, 3))
  ah(n3) = CalcPolynomial (apoly (PP(2), n3, 0:4), vel(3, n3), ubound (apoly, 3))

!dA(h)/dh  
  dahdh(n1) = calcPolynomial1stDerivative (apoly (PP(1), n1, 0:4), vel(3, n1), ubound (apoly, 3))
  dahdh(n3) = calcPolynomial1stDerivative (apoly (PP(2), n3, 0:4), vel(3, n3), ubound (apoly, 3))

  CALL CSTRC(NN)
  GO TO 1320
ENDIF

END subroutine


