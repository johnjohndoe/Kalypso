subroutine petsc_solver (noOfEq, Apetsc, bpetsc, x)
  USE petsc
  USE petscvec
  USE petscmat
  USE petscksp
  USE petscpc
  USE petscsnes
  use PardisoParams, only: mfwsiz

implicit none
!
!  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
! Include  files
!  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
!
!    The  following  include  statements  are  generally  used  in  SNES  Fortran
!    programs:
! petsc.h -  base  PETSc  routines
! petscvec.h        -  vectors
! petscmat.h        -  matrices
! petscksp.h        -  Krylov  subspace  methods
! petscpc.h -  preconditioners
! petscsnes.h      -  SNES  interface
!
#include "finclude/petscdef.h"
#include "finclude/petscvecdef.h"
#include "finclude/petscmatdef.h"
#include "finclude/petsckspdef.h"
#include "finclude/petscpcdef.h"
#include "finclude/petscsnesdef.h"

!arguments
!---------------
integer (kind = 4), intent (inout) :: noOfEq
!integer (kind = 4) :: irow(*), icol(*)
!real (kind = 8) :: a(*), b(*), x(*)
real (kind = 8), intent (inout) :: x(*)
real (kind = 8), allocatable :: x_buf(:)

!local variables
!---------------

!PETSC variables
PetscErrorCode ierr
PetscMPIInt     rank,size
Vec         xpetsc,xlocalpetsc
Vec, intent (inout) :: bpetsc
PetscScalar, pointer :: xx_v(:)
PetscInt, pointer, save :: nnz(:)
PetscInt noOfEqLocal, eqStart, eqEnd
VecScatter vscat
PetscTruth checkequal
IS ix, ixlocal
Mat, intent (inout) ::      Apetsc
KSP ::      ksp

!dimension of the equation system
integer (kind = 8) :: nonZeros

!execution and control of the routine
integer (kind = 4), save :: nzzold = 0, nszfold = 0
integer (kind = 8) :: nsort, maxnnz
integer (kind = 4), allocatable :: jalocal(:)

!counters
integer (kind = 4) :: i, k, kk

  call VecCreateSeq(PETSC_COMM_SELF,noOfEq,xlocalpetsc,ierr)
  call VecDuplicate(bpetsc,xpetsc,ierr)
  call VecGetOwnershipRange(xpetsc,eqStart,eqEnd,ierr)
  call VecGetLocalSize(xpetsc,noOfEqLocal,ierr)

  call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)
  call KSPSetOperators(ksp,Apetsc,Apetsc,DIFFERENT_NONZERO_PATTERN,ierr)
  call KSPSetFromOptions(ksp, ierr)

! initialize result vector to all zeros
call VecSet(xpetsc,0,ierr)
call KSPSolve(ksp,bpetsc,xpetsc,ierr)


if ( ierr .ne. 0 ) then
  call ErrorMessageAndStop (4203, ierr, 0.0d0, 0.0d0)
endif

call VecScatterCreateToAll(xpetsc,vscat,xlocalpetsc,ierr)
call VecScatterBegin(vscat,xpetsc,xlocalpetsc,INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vscat,xpetsc,xlocalpetsc,INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterDestroy(vscat,ierr)

call VecGetArrayF90(xlocalpetsc,xx_v,ierr)
do i=1,noOfEq
  x(i) = xx_v(i)
enddo
call VecRestoreArrayF90(xlocalpetsc,xx_v,ierr)

call VecDestroy(xpetsc,ierr)
call VecDestroy(xlocalpetsc,ierr)
call KSPDestroy(ksp,ierr)

end subroutine petsc_solver
