

      subroutine mkl_solver(  NPROCS, NSZF,
     &             icol, irow, a, b, x, IERROR, nOfEntriesInQs )

      include 'PARAM.COM'
c      save ipt, perm
c      include 'mkl_pardiso.f77'
      
      integer :: nOfEntriesInQs, nOfEntriesInQsOld
      !32 bit OS
      !---------
      integer(kind = 4) :: ipt(64)
      !64 bit OS
      !---------
      !integer*8 ipt(64)
      
      
      
      !data for pardiso
      !----------------

      !-----
      !input
      !-----
      !ipt(64) :: memory address pointer, which is initialized with zero
      !maxfct  :: maximum number of factors with identical nonzewo sparsity stucture; usually 1; trials necessary
      !mnum    :: matrix to be factorized; it must be 1 <= mnum <= maxfct
      !mtype   :: type of the matrix to be solved; in our case it is a real unsymmetric matrix; mtype = 11
      !phase   :: phase of the solving process; first run needs to be the Analysis (phase = 11); second run needs to be the numerical fatorization, solving and linear refinement (phase = 23)
      !n       :: number of equations in the matrix (n = nszf)
      !a, ia and ib describe the matrix storage format for non-symmetric matrices to be put into pardiso; the documentation of MKL has the detailed informations
       ! - - - - - - - - - - - - - - - - - - - 
       !a      :: vector of all non-zero entries in the matrix; row by row
       !ja     :: stores the number of the occurance of a certain a-vector entry in the line
       !ia     :: stores the index for array a, which is always the first entry in a new row; the last additional entry must be the number of entries of a plus one (lenght(a)+1)
       !     a and ja must have equal size
       !     ia is large as the number of rows+1
      !perm    :: user fill-in permuation; switched off, if parm(5) = 1; here parm(5) = 0
      !nrhs    :: right hand sides to be solved; here always 1
      !iparm   :: 64 parameters to be passed for different purposes
        !(1) :: 1 - user defined settings
        !(2) :: 0 - minimum degree algorithm
        !       2 - nested dissection algorithm from METIS
        !(3) :: number of processors; must be the same as MKL_NUM_THREADS (environment variable)
        !(4) :: 0 - factorization computation as required by phase
        !       other values for experienced users
        !(5) :: 0 - for no user permuation
        !       other values for experienced users; regarding some other reordering algorithms
        !(6) :: 0 - write results to vector x, don't change vector b
        !       1 - write results to vector b, x is used as dummy anyway
        !(8) :: maximum number of refinement steps for iterative procedure
        !(9) :: future use reservation; has to have the value 0
        !(10):: perturbation accuracy exponent for pivoting (1e-iparm(10))
        !(11):: 1 - default value for unsymmetric matrices; for permutation
        !       0 - default value for symmetric matrices; for permutation
        !(12):: reserved for future use; must be zero
        !(13):: 
        !... to be continued
       !msglvl :: message level 1 - print out statistical informations
       !                       0 - don't print anything
       !b      :: right hand side vector and output vector if iparm(6) = 0
       !------
       !output
       !------
       !pt     :: internal address pointers; DO NOT CHANGE after initialization
       !iparm  :: 64 parameters to be passed for different purposes 
       ! (... to be continued
       !x      :: output vector, if iparm(6)=1
       !error  :: error (see documentatio for details!)


      !general parameters passed to PARDISO
      integer (kind = 4) :: iparm(64)
      
      !values of ia and ja for the unsorted matrix a
      integer (kind = 4) :: irow(*), icol(*)
      
      !ia and ja are position pointers for the line-matrix (see documentation below)
      integer (kind = 4) :: ia(MR1SIZ+1), ja(NBUFFSIZ)
      integer (kind = 4) :: perm(MR1SIZ)
      integer (kind = 4) :: ierror
      
      !a, b and x are matrix, right-side vector and result vector
      real (kind = 8) :: a(*), b(*), x(*)
      real (kind = 8) :: da(NBUFFSIZ), db(MR1SIZ), dx(MR1SIZ)
      !
      real*8 dum
      real*8 ddum
      
      common /blk_mkl/  ipt, perm, nzzold, nszfold, nOfEntriesInQsOld
      
      data ifirst /0/
      
      if ( ifirst .eq. 0 )  then
         iparm = 0
        iparm(1) = 1 ! no solver default
        iparm(2) = 2 ! fill-in reordering from METIS
c            iparm(2) = 0 ! fill-in reordering from METIS
        iparm(3) = 1 ! numbers of processors
        iparm(4) = 0 ! no iterative-direct algorithm
        iparm(5) = 0 ! no user fill-in reducing permutation
        iparm(6) = 0 ! =0 solution on the first n compoments of x
        iparm(7) = 0 ! not in use
c        iparm(8) = 9 ! numbers of iterative refinement steps
        iparm(8) = 9 ! numbers of iterative refinement steps
        iparm(9) = 0 ! not in use
        iparm(10) = 13 ! perturbe the pivot elements with 1E-13
        
        iparm(11) = 1 ! use nonsymmetric permutation and scaling MPS
        iparm(12) = 0 ! not in use
        iparm(13) = 1 ! not in use
        iparm(14) = 0 ! Output: number of perturbed pivots
        iparm(15) = 0 ! not in use
        iparm(16) = 0 ! not in use
        iparm(17) = 0 ! not in use
        iparm(18) = -1 ! Output: number of nonzeros in the factor LU
        iparm(19) = -1 ! Output: Mflops for LU factorization
        iparm(20) = 0 ! Output: Numbers of CG Iterations
        error = 0 ! initialize error flag
        msglvl = 1 ! print statistical information
        mtype = 11 ! real unsymmetric
        
c    iparm = 0 --> use defaults       
        iparm = 0
         
        iparm(3) = NPROCS ! numbers of processors
      endif
      
      !first run of mkl_solver.sub; some values need to be initialized
      if ( ifirst .eq. 0 )  then
         nzzold = 0
         nszfold = 0
         nOfEntriesInQsOld = 0
         ifirst = 1
      endif
      nzz = icol(nszf+1) 
      do i=1,nzz
         da(i) = 0.
         ja(i) = 0
      enddo
     

      do i=1,nzz
        !copy matrix-vector to local da-array
        da(i) = a(i)
        !copy the initial non zero-entry of the row i in the global matrix; see MKL (PARDISO) documenation for details
        ja(i) = irow(i)
      enddo
      
      do i=1,nszf
         ia(i) = 0
         db(i) = 0.
         dx(i) = 0.
      enddo
      
      do i=1,nszf
         !ia holds the column of the non-zero entry in the matrix row; see documentation of MKL (PARDISO) for details
         ia(i) = icol(i)
         !copy right side vector to local db-array
         db(i) = b(i)
         !dx will become the result vector
         dx(i) = 0.
      enddo
      
      
C  Need to sort ja so that for the row, the column indices are in increasing value
      ia(nszf+1) =  nzz
      do i=1,nszf
         if ( ia(i) .eq. 0 )  then
            write(75,*) ' ia(i) eq 0, i=', i
            write(75,*) ' nszf = ', nszf
            write(75,*) ' nzz  = ', nzz
            WRITE(75,'(2i10)') (kk,ia(kk),kk=1,nszf)
            stop 
         endif
         nsort = ia(i+1) - ia(i)
         call mysort( ja(ia(i)), da(ia(i)), nsort)
      enddo         
      
      
      !set number of equations to be solved in pardiso
      n = nszf
      
      !parameters for PARDISO; see documentation above or for MKL (PARDISO)
      nrhs = 1
      maxfct = 1
      mnum = 1
      mtype = 11
      ierror = 0
      iphase = 11
      
      if (nzzold /= nzz .or. nszfold /= nszf) then
        if (nzzold /= 0) ifirst = 2
      endif

      !Informations concerning number of equations
      write(*,*) ' nszf, nszfold = ', nszf, nszfold
      write(*,*) ' nzz, nzzold   = ', nzz, nzzold
      write(*,*) ' InQs, InQsOld = ', nOfEntriesInQs, nOfEntriesInQsOld
      
c      ifirst=1

      call second(sutim1)
       
      if ( ifirst < 3 ) then
      
          
          !release all allocated memory
          if (ifirst == 2) then
            iphase = -1
            call pardiso( ipt, maxfct, mnum, mtype, iphase, n, 
     &          da, ia, ja, perm, nrhs, iparm, msglvl,
c     &          db, dx, ierror )
     &          ddum, ddum, ierror )
          endif 

          ipt = 0
          iphase = 11
          call pardiso( ipt, maxfct, mnum, mtype, iphase, n, 
     &          da, ia, ja, perm, nrhs, iparm, msglvl,
c     &          db, dx, ierror )
     &          ddum, ddum, ierror )
     
          if ( ierror .ne. 0 ) then
            write(75,*) 'Error phase 11 ', ierror
            stop
          endif
        
          
C        endif  
        ifirst = 3
c        do i=1,n
c            x(i) = dx(i)
c        enddo
       
c        return

      endif
      call second(sutim2)

      write(75,'(a,f8.2)') ' Time in ordering ', sutim2-sutim1
     
            
      iphase = 23
      ierror = 0
      
      call pardiso( ipt, maxfct, mnum, mtype, iphase, n, 
     &          da, ia, ja, perm, nrhs, iparm, msglvl,
     &          db, dx, ierror )

      if ( ierror .ne. 0 ) then
        write(75,*) 'Error phase 33 ', ierror
        stop
      endif

      call second(sutim3)

      write(75,'(a,f8.2)') ' Time to solve  = ', sutim3-sutim2
      
c      write(210,'(2i10)') (i,iparm(i),i=1,23)
      
        do i=1,n
            x(i) = dx(i)
        enddo
      
      nzzold = nzz
      nszfold = nszf
      nOfEntriesInQsOld = nOfEntriesInQs
      
      end



!-------------------------------------------------------
!subroutine to sort the matrix-vector in ascending order
!-------------------------------------------------------
      SUBROUTINE MYSORT (IX, Y, N)
C
C        ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
C
C        Sorts the N values stored in array X in ascending order
C
      INTEGER :: N
      integer (kind = 4) :: ix(n)
      real (kind = 8) :: y(N)
C
      INTEGER I, J, INCR
      REAL*8 YTEMP
C
      INCR = 1
C
C        Loop : calculate the increment
C
   10 INCR = 3 * INCR + 1
      IF (INCR .LE. N) GOTO 10

C
C        Loop : Shell-Metzner sort
C
   20 INCR = INCR / 3
      I = INCR + 1
   30 IF (I .GT. N) GOTO 60
      ITEMP = IX(I)
      YTEMP = Y(I)
      J = I
   40 IF (IX(J - INCR) .LT. ITEMP) GOTO 50
      IX(J) = IX(J - INCR)
      Y(J) = Y(J - INCR)
      J = J - INCR
      IF (J .GT. INCR) GOTO 40
   50 CONTINUE
      IX(J) = ITEMP
      Y(J) = YTEMP
      I = I + 1
      GOTO 30
   60 IF (INCR .GT. 1) GOTO 20
C
      RETURN
      END
