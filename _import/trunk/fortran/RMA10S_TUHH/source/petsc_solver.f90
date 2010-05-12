subroutine petsc_solver (nszf, Apetsc, bpetsc, x)
USE petsc
USE petscvec
USE petscmat
USE petscksp
USE petscpc
USE petscsnes
USE PardisoParams, only: mfwsiz

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
integer (kind = 4), intent (inout)  :: nszf
Vec, intent (inout)                 :: bpetsc
Mat, intent (inout)                 :: Apetsc
real (kind = 8), intent (inout)     :: x(*)

!local variables
!---------------
integer (kind = 4) :: i
integer (kind = 4), save :: nszfold

!PETSC variables
PetscErrorCode       :: ierr
Vec                  :: xpetsc, xlocalpetsc
PetscScalar, pointer :: xx_v(:)
VecScatter           :: vscat
KSP, save            :: ksp

data nszfold/0/

call VecCreateSeq(PETSC_COMM_SELF,nszf,xlocalpetsc,ierr)
call VecDuplicate(bpetsc,xpetsc,ierr)
! initialize result vector to all zeros
call VecZeroEntries(xpetsc,ierr)

if (nszfold /= nszf) then
    write(*,*) 'Creating new solver context'

    if(nszfold /= 0) then
		call KSPDestroy(ksp,ierr)
	endif

	call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)
	call KSPSetFromOptions(ksp, ierr)
endif

call KSPSetOperators(ksp,Apetsc,Apetsc,SAME_NONZERO_PATTERN,ierr)
call KSPSetUp(ksp,ierr)
call KSPSolve(ksp,bpetsc,xpetsc,ierr)

if ( ierr .ne. 0 ) then
  call ErrorMessageAndStop (4203, ierr, 0.0d0, 0.0d0)
end if

call VecScatterCreateToAll(xpetsc,vscat,xlocalpetsc,ierr)
call VecScatterBegin(vscat,xpetsc,xlocalpetsc,INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vscat,xpetsc,xlocalpetsc,INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterDestroy(vscat,ierr)

call VecGetArrayF90(xlocalpetsc,xx_v,ierr)
do i=1,nszf
  x(i) = xx_v(i)
end do
call VecRestoreArrayF90(xlocalpetsc,xx_v,ierr)

nszfold = nszf

call VecDestroy(xpetsc,ierr)
call VecDestroy(xlocalpetsc,ierr)

end subroutine petsc_solver
