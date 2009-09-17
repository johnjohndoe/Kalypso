module getgeo1_mod

contains

!******************************************************************************************
!  subroutine getgeo1.sub reads the model geometry file and examines the size of the mesh
!    to be able to prepare the arrays in the adapted size during real mesh reading and 
!    generation process
!******************************************************************************************
subroutine GETGEO1

use mod_RDKalypso_routines
use mod_Model

use BLK10MOD, only: &
&   maxp, maxe, maxa, maxlt, maxps, maxSE, &
&   maxpolya, maxpolyb, maxpolyq, &
&   ifile
!meaning of the used variables
!-----------------------------
!maxp: maximum number of points (surface points; includes midsides)
!maxe: maximum number of elements
!maxa: maximum number of arcs (auxiliary mean to generate mesh)
!maxlt: maximum number of 1D/2D transition elements for 1D/2D line-2-element transitions
!maxPolyA: maximum number of polynomials to describe A(h) at any node
!maxPolyQ: maximum number of polynomials to describe Q(h) at any node
!maxPolyB: maximum number of polynomials to describe alpha(h)/ beta(h) at any node
!ifile: unit number of the geometry file
use parakalyps, only: &
&   maxintpolelts, nodestointpol, statelsz, statnosz
!meaning of the used variables
!-----------------------------
!maxIntPolElts: number of elements that will be added, when interpolating nodes into 1D elements
!NodesToIntPol: summed number of nodes that will be added to the node list, when interpolating
!TODO: statElSz and statNoSz should be substituted and removed completely
!------------------------------------------------------------------------
!statElSz: real number of elements, to examine until which element to check for potentially nodes interpolation
!statNoSz: real number of nodes to start numbering of interpolated profiles later on

implicit none

!NiS,mar06: adding declaration of hand-over-parameters between subroutines
integer :: n, m, a, LT, PA, PQ, PB, ps, mse
type (simulationModel), pointer :: dummySimulationModel
character (len = 1000) :: dummyModelName

!initialisations of the mesh size parameters
!-------------------------------------------
maxP = 0
maxE = 0
maxA = 0
maxLT = 0
maxps = 0
maxPolyA = 1
maxPolyQ = 1
maxPolyB = 1
maxIntPolElts = 0
NodesToIntPol = 0
statElSz = 0
statNoSz = 0

!local variable initialisations
!------------------------------
n = 0
m = 0
a = 0
lt = 0
pa = 0
pq = 0
pb = 0
ps = 0
mse = 0

dummyModelName = 'dummyModelName'
dummySimulationModel => newSimulationModel(dummyModelName)

!call the model reading subroutine to examine the size of the geometry
call RDKALYPS (N, M, A, PA, PQ, PB, LT, ps, mse, 1, dummySimulationModel)

!bring model input file to the beginning for next read
rewind IFILE

!some informational output
write(*,*)' back from rdkalypso'
write(*,*)' N,M,A,PA,PQ,PB,LT,PS = ',N,', ',M,', ',A,', ',PA, ', ', PQ,', ', PB,', ', LT, ', ', ps, ', ', mse


!ERROR - SUSPICIOUSLY LARGE NODE NUMBER OR ELEMENT NUMBER DETECTED
if (N > 500000) then
  call ErrorMessageAndStop(1102, N, 0.0d0, 0.0d0)
elseif (M > 500000) then
  call ErrorMessageAndStop(1201, M, 0.0d0, 0.0d0)
endif

!maximum number of elements without the later on interpolated elements
maxE = M + 1 
!maximum number of nodes without the later on interpolated (corner and midside) nodes
maxP = N + 1 
!maximum number of arcs to set up original mesh
maxA = A + 1
!for interpolation of profiles between 1D-profiles, remember the original size
StatElSz = MAXE - 1
StatNoSz = MAXP - 1
!add the interpolated elements
MAXE = MAXE + NodesToIntPol
!add the interpolated corners and midsides
MAXP = MAXP + NodesToIntPol + NodesToIntPol
!1D/2D line transition counter
MaxLT = LT
!pipe surface connections
maxps = ps
!storage elements
maxSE = mse
!maximum number of polynomial splitting at any node
MaxPolyA = PA
MaxPolyQ = PQ
MaxPolyB = PB

return
end subroutine
!***
end module