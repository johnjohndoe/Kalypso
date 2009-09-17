SUBROUTINE COEF1DJunction (NN,NTX)


USE BLK10, only: r1
USE BLK10MOD, only: maxn, iteqv, nops, ncn, ncorn, nop, imat, idnopt, vel, ndf, alfa, &
& width, dir, ss1, ss2, cord, ao, grav, nbc
USE BLKDRMOD, only: akp, adt, adb, ame, dame
USE Para1DPoly, only: polyrangea, polysplitsa, ah, apoly, dahdh
USE blkecom, only: ncon, f, estifm


implicit none


INTEGER :: NTX, NN, ncnx, mc, iswt, nef
INTEGER :: N1, MR, KK, NA, IMMT, NRX
INTEGER :: i, j, k, n, m
INTEGER :: IA, JA
INTEGER :: PolyPos, findPolynom

REAL (KIND = 8) :: SA, CX, R
real (kind = 4) :: dum2, aml
REAL (KIND = 8) :: WSEL1, WSELX
REAL (KIND = 8) :: calcpolynomial, calcPolynomial1stDerivative

REAL(KIND=8) :: HS, HD, HD1, HDX, DUM1, HS1, HSX

REAL (KIND=8) :: h

!local variables
real (kind = 8) :: rx, thn, ry, th1, acx, wsx, acy, wsy
integer :: nry


IF(ITEQV(MAXN) .EQ. 5) THEN
  DO N=1,8
    NCON(N)=NOPS(NN,N)
    IF(NCON(N) .NE. 0) NCN=N
  ENDDO
ELSE
  NCN=NCORN(NN)
  DO N=1,NCN
    NCON(N)=NOP(NN,N)
  enddo
ENDIF

!cipk nov97
ncnx=2

!c
!c     Initialize AME and DAME
!c
IF (IDNOPT.LT.0) THEN
  DO M = 1, NCNX
    MC = 2 * M - 1
    N = NOP(NN,MC)
    HS = VEL(3,N)
    ISWT = 0
    CALL AMF(DUM1,HS,AKP(N),ADT(N),ADB(N),AME(M),DAME(M),ISWT)
  END DO
ENDIF

!
! INITIALIZE MATRICES AND VARIABLES
!
NEF = NCN * NDF
DO I=1,NEF
  F(I) = 0.0
  DO J=1,NEF
    ESTIFM(I,J) = 0.0
  ENDDO
ENDDO


!cipk oct98 update to f90
IMMT=IMAT(NN)
!C
!Cipknov99     revise to test for collapse from 2-d vertical
!C
MR=MOD(IMMT,1000)


!1st condition is continuity at the junction; time dependency is neglected here!
checknodes: DO KK = 1, NCN

  !get current node
  N1 = NCON (KK)
  !cycle for zero-nodes
  IF (N1 == 0) CYCLE checknodes
  !get local equation number of current node
  NA = (KK - 1) * NDF + 1
  !calculate global angle relations
  CX = COS (ALFA(N1))
  SA = SIN (ALFA(N1))
  !calculate resulting 1D-velocity of current node
  R  = VEL(1, N1) * CX + VEL(2, N1) * SA

  !using geometry-approach (means trapezoidal channel)
  IF (width(n1) /= 0.0) THEN
    !derivative over velocity
    ESTIFM(1, NA) = DIR(N1) * (WIDTH(N1) + (SS1(N1) + SS2(N1)) / 2. * VEL(3, N1)) * VEL(3,N1)
    !derivative over depth
    ESTIFM(1, NA+2) = DIR(N1) * (WIDTH(N1) + (SS1(N1) + SS2(N1)) * VEL(3,N1)) * R

  !using polynom approach
  ELSE
    !get position in polynomial for current node with present water stage
    PolyPos = findPolynom (PolyRangeA (n1, :), vel(3, n1), PolySplitsA (n1), cord(n1, 1), cord (n1, 2), n1)
    !calculate the cross sectional area
    ah (n1) = calcPolynomial (apoly (PolyPos, n1, 0:12), vel (3, n1), ubound (apoly, 3))
    !install derivative of discharge over velocity at current node into local equation 1
    ESTIFM(1, NA) = DIR(N1) * ah(n1)
    !calculate the derivative of the cross sectional area of current node over depth
    dahdh (n1) = calcPolynomial1stDerivative (apoly (PolyPos, n1, 0:12), vel(3, n1), ubound (apoly, 3))
    !install derivative of discharge over depth at current node into local equation 1
    ESTIFM(1, NA+2) = DIR(N1) * dahdh (n1) * R

  ENDIF
  !install direction dependent discharge of current node into residual error vector; the sum of the in-/outflows must be zero!
  F(1) = F(1) - ESTIFM(1, NA) * R

ENDDO checknodes


if (imat (NN) == 901) then

  !1st node (definition by accident)
  NRX = NCON (1)
  !run through rest of nodes of junction
  checkNodes901: DO KK = 2, NCN
    !get node number
    N1 = NCON (KK)
    !check for zero-node number
    IF (N1 == 0) CYCLE checkNodes901
    !get local equation number
    NA = (KK - 1) * NDF + 1
    !instal derivatives with respect to (WRT) water depth of reference node and
    ESTIFM (NA, 3) = 1.0
    ESTIFM (NA, NA + 2) = - 1.0

!FOR MARSH ALGORITHM, NOT OPERATIVE AT THE MOMENT
  !CIPK NOV97        F(NA)=XHT*((VEL(3,N1)-VEL(3,NRX))+(AO(N1)-AO(NRX)))
  !  IF (IDNOPT .LT. 0) THEN
  !    HD1 = VEL(3,N1)
  !    CALL AMF(HS1,HD1,AKP(N1),ADT(N1),ADB(N1),AML,DUM2,0)
  !    WSEL1 = ADO(N1)+HS1
  !    HDX = VEL(3,NRX)
  !    CALL AMF(HSX,HDX,AKP(NRX),ADT(NRX),ADB(NRX),AML,DUM2,0)
  !    WSELX = ADO(NRX)+HSX
  !  ELSE
  !    WSEL1=AO(N1)+VEL(3,N1)
  !    WSELX=AO(NRX)+VEL(3,NRX)
  !  ENDIF
!FOR MARSH ALGORITHM, NOT OPERATIVE AT THE MOMENT

    !for polynomial approach no marsh-option for the moment!
    WSEL1 = AO (N1) + VEL (3, N1)
    WSELX = AO (NRX) + VEL (3, NRX)


    F (NA) = (WSEL1 - WSELX)
  ENDDO checkNodes901


ELSEIF (imat (nn) == 902) then

  NRX = nop (nn, 1)
  rx = vel (1, nrx) * COS (ALFA(nrx)) + vel (2, nrx) * SIN (ALFA(nrx))
  thn = vel (3, nrx) + AO(nrx) + rx**2 / (2. * grav)
  checknodes902: do KK = 2, ncn
    N1 = nop (nn, kk)
    if (n1 == 0) CYCLE checknodes902
    ry = vel (1, n1) * COS (ALFA(n)) + vel (2, n1) * SIN (ALFA (n1))
    th1 = vel (3, n1) + AO(n1) + ry**2 / (2. * grav)
    na = (kk - 1) * NDF + 1
    ESTIFM (na, 1) = rx/ grav
    ESTIFM (na, 3) = 1.0
    ESTIFM (na, na) = -ry/ grav
    ESTIFM (na, na + 2) = -1.0
    f (na) = th1 - thn
  end do checknodes902

ELSEIF (imat (nn) == 903) then
  nrx = nop (nn, 1)
  nry = nop (nn, 2)

  PolyPos = findPolynom (PolyRangeA (nrx, :), vel(3, nrx), PolySplitsA (nrx), cord (nrx, 1), cord (nrx, 2), nrx)
  acx = calcPolynomial (apoly (PolyPos, nrx, 0:12), vel (3, nrx), ubound (apoly, 3))
  wsx = calcPolynomial1stDerivative (apoly (PolyPos, nrx, 0:12), vel(3, nrx), ubound (apoly, 3))

  PolyPos = findPolynom (PolyRangeA (nry, :), vel(3, nry), PolySplitsA (nry), cord (nry, 1), cord (nry, 2), nry)
  acy = calcPolynomial (apoly (PolyPos, nry, 0:12), vel (3, nry), ubound (apoly, 3))
  wsy = calcPolynomial1stDerivative (apoly (PolyPos, nry, 0:12), vel(3, nry), ubound (apoly, 3))

  rx = vel (1, nrx) * COS (ALFA (nrx)) + vel (2, nrx) * SIN (ALFA (nrx))
  ry = vel (1, nry) * COS (ALFA (nry)) + vel (2, nry) * SIN (ALFA (nry))
  f(4) = - acx * rx**2 + acy * ry**2 - grav * acx * vel (3, nrx) + grav * acy * vel (3, nry)
  ESTIFM (4, 1) = acx * rx * 2.
  ESTIFM (4, 4) = - acy * ry * 2.
  ESTIFM (4, 3) = wsx * rx**2 + grav * (acx + vel (3, nrx) * wsx)
  ESTIFM (4, 6) = - wsy * ry**2 - grav * (acy + vel (3, nry) * wsy)
  checknodes903: do kk = 3, ncn
    n1 = nop (nn, kk)
    if (n1 == 0) CYCLE checknodes903
    na = (kk - 1) * NDF + 1
    ESTIFM (na, 3) = 0.5
    ESTIFM (na, 6) = 0.5
    ESTIFM (na, na + 2) = -1.0
    f (na) = vel (3, n1) - (vel (3, nrx) + vel (3, nry))/ 2. + AO(n1) - (AO(nrx) + ao (nry))/2.
  end do checknodes903

end if


!-------

!nis,Oct,com: Install element residual values into global vector. NCN.eq.3 for 1D-elements and 1D-2D-elements.
FindNodesToInstall: DO I = 1, NCN

  !get degree of freedom IA from node J to install equations to
  J = NCON (I)
  IA = NDF * (I - 1)

  !for every nodal degree of freedom
  InstallResiduals: DO K = 1, NDF

    !find equation number JA by local number IA
    IA = IA + 1
    JA = NBC (J, K)

    !Jump over deactivated nodal degree of freedom
    IF (JA == 0) CYCLE InstallResiduals

    !Install element residuum F(IA) into global residuum R1(JA)
    R1(JA)=R1(JA)+F(IA)

  ENDDO InstallResiduals
ENDDO FindNodesToInstall

RETURN

END
