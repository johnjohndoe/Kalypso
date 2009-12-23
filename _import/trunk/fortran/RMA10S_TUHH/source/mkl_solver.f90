subroutine mkl_solver (NPROCS, noOfEq, icol, irow, a, b, x)

implicit none

!dummy arguments
!---------------
integer (kind = 4), intent (inout) :: nprocs
integer (kind = 4), intent (inout) :: noOfEq
!values of ia and ja for the unsorted matrix a
integer (kind = 4) :: irow(*), icol(*)
real (kind = 8) :: a(*), b(*), x(*)

!local variables
!---------------
!general parameters passed to PARDISO, they will not change
integer (kind = 4) :: maxfct = 1, mnum = 1, mtype = 11, nrhs = 1, msglvl = 0
!dimension of the equation system
integer (kind = 8) :: nonZeros
!Parameters passed to PARDISO
!- - - - - - - - - - - - - - -
!integer (kind = 4), save :: ipt (64)!32 bit OS
!32 or 64 bit OS
integer (kind = 8), save :: ipt (64)
integer (kind = 4), allocatable :: ia(:), ja(:)
integer (kind = 4) :: ierror = 0
!a, b and x are matrix, right-side vector and result vector
real (kind = 8), allocatable :: da(:), db(:), dx(:)
integer (kind = 4) :: iphase
integer (kind = 4), save :: iparm(64)
integer (kind = 4), allocatable, save :: perm (:)
!parameter to use as dummy argument when calling PARDISO for RELEASING MEMORY (Phase -2) or ANALYSIS (Phase 1)
real (kind = 8) :: ddum
!execution and control of the routine
!- - - - - - - - - - - - - - - - - - -
integer (kind = 4), save :: executionSwitch = 0
integer (kind = 4), save :: nzzold, nszfold
!sorting
integer (kind = 4) :: nsort
!counters
integer (kind = 4) :: i, k, kk
!Time Monitoring
real (kind = 8) :: sutim1, sutim2, sutim3

integer omp_get_max_threads
external omp_get_max_threads


!meaning of the variables
!------------------------
!ipt(64)      memory address pointer, which is initialized with zero; DON'T CHANGE!!!
!maxfct       no. of factorization matrices to be generated
!mnum         factorization matrix (1... maxfct) that should be used
!mtype        real unsymmetric
!nrhs         number of right hand sides for which the system needs to be solved
!msglvl       indicate print of statistical information (msglvl = message level)
!             1 - print out statistical informations
!             0 - Don't print anything
!nonZeros     number of non-zero entries in the Matrx that has to be solved
!ipt (64)     memory pointer for PARDISO; needs to be initialized with 0 only at the beginning
!ia, ja, da   matrix (ia and ja are position pointers for the line-matrix (see documentation))
!db, dx       right-hand-side vector; solution vector
!             da - non-zeros of Matrix A
!             ia - Index in da, where a new row definition starts
!             ja - Column index within each row to localize each particular entry of da
!             db - right hand sight vector (Residual vector)
!             dx - solution vector
!iphase       solution phase 
!iparm        setting parameters for the solution process (APPLY DEFAULT VALUES!)
!             iparm(3) has to be given by the software; it determines the threads (processors) that can be used
!             by PARDISO
!perm         permutation matrix; will only be used, if iparm(5) == 1; with default values perm is not in use!
!executionSwitch indicates the situation in which pardiso shall be executed
!             0 - It is the very first execution time, so all parameters that need to be settled only once
!                 will be initialized
!             /= 0 - No general initializations need to be done any more; all further settings are done for 
!                    following particular purpose
!             ---
!             1 - Equation system needs to be factorized and solved; releasing of memory in advance is not
!                 needed - this option shall only be used, if pardiso was not executed before, so no memory
!                 allocation was done before. Thus executionSwitch is only == 1, if pardiso is called the
!                 very first time after general initializations.
!             2 - Equation system needs to be factorized and solved and memory needs to be released before
!             3 - Equation system needs just to be solved with the settings from previous run without new
!                 factorization or releasing of memory
!nsort        range to sort within ja array

!get number of non-Zero entries from matrix A  
nonZeros = icol (noOfEq + 1)

!allocate local copies of matrix
allocate (ia (noOfEq + 1), dx(noOfEq), db(noOfEq))
allocate (ja (nonZeros), da(nonZeros))

!at first call the parameters need to be settled
if (executionSwitch == 0)  then

!input parameters for PARDISO  
!----------------------------  
!Initialize parameter array to use default values of PARDISO  
  iparm = 0
  
!  !individual parameters; only for very advanced users of PARDISO
!  !--------------------------------------------------------------
!        !default iparm (1) = 0: default behviour or not
!        iparm(1) = 1 !no solver default
!        !default iparm (2) = 2: Reordering algorithm
!        iparm(2) = 2 !fill-in reordering from METIS
!default iparm (3) = no default: Reordering method        
! numbers of processors
        iparm(3) = omp_get_max_threads() 
!        !default iparm (4) = 0: Preconditioned CGS
!        iparm(4) = 0 ! no iterative-direct algorithm
!        !default iparm (5) = 0: Permutation
!        iparm(5) = 0 ! no user fill-in reducing permutation
!        !default iparm (6) = 0: write solution on x
!        iparm(6) = 0 ! =0 solution on the first n compoments of x
!          !default iparm (7) = no default; output data
!          iparm(7) = 0 ! output: number of performed iterative refinement steps
!        !default iparm (8) = 0: Iterative refinement steps
!        iparm(8) = 0 ! numbers of iterative refinement steps
!        !default iparm (9) = 0; reserved for future usage
!        iparm(9) = 0 ! not in use, but must be set!
!        !default iparm (10) = 13 for mtype = 11: pivoting pertubation
!        iparm(10) = 13 ! perturbe the pivot elements with 1E-13
!        !default iparm (11) = 1 for mtype = 11: scaling vectors
!        iparm(11) = 1 ! use nonsymmetric permutation and scaling MPS
!        !default iparm (12) = 0; reserved for future usage
!        iparm(12) = 0 ! not in use, but must be set!
!        !default iparm (13) = 1 for mtype = 11: improved accuracy using (non-)symmetric weighted matchings
!        iparm(13) = 1 ! not in use
!        !default iparm (14) no default; output data
!          iparm(14) = 0 ! Output: number of perturbed pivots
!          !default iparm (15) no default; output data
!          iparm(15) = 0 ! Output: peak memory symbolid factorization
!          !default iparm (16) no default; output data
!          iparm(16) = 0 ! Output: permanent memory symbolic factorization
!          !default iparm (17) no default; output data
!          iparm(17) = 0 ! Output: memory numerical factorization and solution
!          !default iparm (18) no default; output data
!          iparm(18) = -1 ! Output: number of nonzeros in the factor LU
!        !default iparm (19) = 0: Report on MFlops (10**6)
!        iparm(19) = 0 ! Output: Mflops for LU factorization
!        !default iparm (20) = no default; output data
!          iparm(20) = 0 ! Output: CG/CGS diagnostics
!          !default iparm (21) = 0: pivoting for symmetric indefinite matrices
!        iparm (21) = 0
!        !default iparm (22) = no default, output data
!          iparm (22) = 0 ! Output number of positive eigenvalues
!          !default iparm (23) = no default, output data
!          iparm (23) = 0 ! Output number of negative eigenvalues
!        !default iparm (27) = 0: No matrix checking
!          iparm (27) = 0
!        !default iparm (28) = 0: Precision of values
!        iparm (28) = 0 ! Double precision
!          
!          !iparm (24) through iparm (26) and iparm (29) through iparm (64) are reserved for further settings

!Allocate space for permutation matrix  
  if (iparm(5) == 1) then
    if ( .NOT. (allocated (perm))) allocate (perm (noOfEq))
  else
!dummy allocation for proper compilation; perm will not be used in PARDISO, if iparm(5) /= 1    
    if ( .NOT. (allocated (perm))) allocate (perm (1))
  endif
  
    perm = 0
  
!execution and equation system shape  
!-----------------------------------  
!indicate that initializations are done; factorization and solv  
  executionSwitch = 1
!Following parameters are just giving the shape of the equation system; they are used to compare  
!the system between to executions. If the model does not change in between, the system can be  
!directly solved, otherwise memory has to be released and factorizations have to be done again  
!initialize the parameters here:  
  nzzold = 0
  nszfold = 0
endif
      
!copy non-Zeros of Matrix A and their row position within Matrix to local array
do i = 1, nonZeros
!copy matrix-vector to local da-array  
  da (i) = a (i)
!copy the initial non zero-entry of the row i in the global matrix; see MKL (PARDISO) documenation for details  
  ja (i) = irow (i)
enddo
      
!copy first column position entry ia to local array
!copy right hand side vector db
!initialize solution vector dx
do i=1,noOfEq
!ia holds the column of the non-zero entry in the matrix row; see documentation of MKL (PARDISO) for details  
  ia(i) = icol(i)
!copy right side vector to local db-array  
  db(i) = b(i)
!dx will become the result vector  
  dx(i) = 0.
enddo
!Overgive the theoretical start of the equation after the last one, to make the system no, where Matrix ends
ia (noOfEq + 1) =  nonZeros

!Need to sort ja so that for the row, the column indices are in increasing order; with the sorting of ja, da 
!  also has to be changed in parallel
do i = 1, noOfEq
!Error, because equation i is not properly defined  
  if (ia(i) == 0) call ErrorMessageAndStop (4204, 0, 0.0d0, 0.0d0)
  
!Get the entry indices of equation i within the Matrix A and sort them in ascending order with respect to  
!  their occurance in ja-index; da has to be changed simultanously  
  nsort = ia(i+1) - ia(i)
  call mysort (ja (ia (i)), da (ia (i)), nsort)
enddo         
      
!**********************************************************************
!Phase 1: 'Analysis and symbolic factorization' of the equations system;
!                 +++ optionally release memory before +++
!**********************************************************************

!check for the change of the matrix; if the size of the matrix did not change pardiso can be
!solved with the same 'Analysis symbolic factorization' settings from the previous execution
if (nzzold /= nonZeros .OR. nszfold /= noOfEq) then
!memory needs to be released  
  if (nzzold /= 0) executionSwitch = 2
endif

!Write informations concerning number of equations
write(*,*) '      No. of equations: Prev. run - ', nszfold, 'Curr. run - ', noOfEq
write(*,*) ' No. of non-zeros in A: Prev. run - ',  nzzold, 'Curr. run - ', nonZeros
write(*,*) ' '
      
!Time Monitoring; Record start time
call second(sutim1)

!Factorize and optionally release memory before
! executionSwitch == 1: Just factorize, because it's the first call
! executionSwitch == 2: Release memory and factorize afterwards
if (executionSwitch < 3 ) then

!Release PARDISO used memory  
!---------------------------  
  if (executionSwitch == 2) then
!indicate releasing phase    
    iphase = -1
!write about status    
    write(*,*) '*****************'
    write(*,*) 'Releasing memory'
    write(*,*) '*****************'

!call PARDISO for releasing memory    
    call pardiso (ipt, maxfct, mnum, mtype, iphase, noOfEq, da, ia, ja, perm, nrhs, iparm, msglvl, ddum, ddum, ierror)
    
    
!Reset permutation matrix    
    if (iparm(5) == 1) then
      if (allocated (perm)) deallocate (perm)
      allocate (perm (noOfEq))
      perm = 0
    endif
  endif

!Analysis and symbolic factorization  
!-----------------------------------  
!indicate ANALYSIS phase  
!-----------------------  
  iphase = 11
!initialize memory pointer array  
  ipt = 0
!write about status  
  write(*,*) '********************'
  write(*,*) 'reordering of matrix'
  write(*,*) '********************'
  
!call PARDISO for ANALYSIS and SYMBOLIC FACTORIZATION  
  call pardiso(ipt, maxfct, mnum, mtype, iphase, noOfEq, da, ia, ja, perm, nrhs, iparm, msglvl, ddum, ddum, ierror )
     
!if Error occured: Error message and stop  
  if (ierror /= 0) then
    call ErrorMessageAndStop (4202, ierror, 0.0d0, 0.0d0)
  endif

!Indicate that calculation can be done  
  executionSwitch = 3
endif


!Time Monitoring
!---------------
call second(sutim2)
write(75,'(a,f8.2)') 'Time to Analyse and factorize symbolically (Ordering): ', sutim2-sutim1


!****************************************************************
!Phase 2 and 3: numerical factorization and solving of the matrix
!****************************************************************

!Indicate phase
iphase = 23
      
write(*,*) '***********************************'
write(*,*) 'factorization and solving of matrix'
write(*,*) '***********************************'
call pardiso( ipt, maxfct, mnum, mtype, iphase, noOfEq, da, ia, ja, perm, nrhs, iparm, msglvl, db, dx, ierror )

if ( ierror /= 0 ) then
  call ErrorMessageAndStop (4203, ierror, 0.0d0, 0.0d0)
endif


!Time Monitoring
call second(sutim3)
write(75,'(a,f8.2)') ' Time to factorize and solve: ', sutim3-sutim2
      
do i=1,noOfEq
  x(i) = dx(i)
enddo
      
!Remember values from this run for next round
nzzold = nonZeros
nszfold = noOfEq
      
!deallocate locals
deallocate (ia, dx)
deallocate (ja, da, db)

end subroutine mkl_solver



!-------------------------------------------------------
!subroutine to sort the matrix-vector in ascending order
!-------------------------------------------------------
SUBROUTINE MYSORT (IX, Y, N)
!
!ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
!
!Sorts the N values stored in array X in ascending order

!dummy arguments
!---------------
integer (kind = 4) :: n
integer (kind = 4) :: ix(n)
real (kind = 8) :: y(N)

!local variables
!---------------
integer (kind = 4) :: i, j, incr
REAL (kind = 8) :: ytemp

!Calculate the increment
!-----------------------
incr = 1
SetIncrement: Do
  incr = 3 * INCR + 1
  IF (INCR > N) exit SetIncrement
enddo SetIncrement

!------------------
!Shell-Metzner sort
!------------------
VeryOuterSort: Do
  INCR = INCR / 3
  I = INCR + 1
  
  OuterSort: Do
    IF (I > N) exit OuterSort
    ITEMP = IX(I)
    YTEMP = Y(I)
    J = I
    InnerSort: Do
      IF (IX(J - INCR) < ITEMP) exit InnerSort
      IX(J) = IX(J - INCR)
      Y(J) = Y(J - INCR)
      J = J - INCR
      IF (J <= INCR) exit InnerSort
    enddo InnerSort
    IX(J) = ITEMP
    Y(J) = YTEMP
    I = I + 1
  enddo OuterSort
  IF (INCR <= 1) exit veryOuterSort
enddo veryOuterSort 


END subroutine mysort
