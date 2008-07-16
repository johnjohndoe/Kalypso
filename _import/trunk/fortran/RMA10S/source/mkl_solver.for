c void superlu_s_(int *order, int *ordering,
c	      int *threads, int *N, int *column, int *row, float *value, 
c	      float *x, int *info)
      
      
      
      subroutine mkl_solver(  NPROCS, NSZF,
     &             icol, irow, a, b, x, IERROR )

      include 'PARAM.COM'
c      save ipt, perm
c      include 'mkl_pardiso.f77'
      
      integer*4 ipt(64)
C      integer*8 ipt(64)
      integer*4 iparm(64)
      integer*4 irow(*), icol(*)
      
      integer*4 ia(MR1SIZ+1), ja(NBUFFSIZ)
      integer*4 perm(MR1SIZ)
      
      real*4 a(*), b(*), x(*)
      real*8 da(NBUFFSIZ), db(MR1SIZ), dx(MR1SIZ)
      real*4 dum
      real*8 ddum
      
      common /blk_mkl/  ipt, perm, nzzold, nszfold
      
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
C        iparm(5) = 1         
      endif
C        do n=1,nszf
C          perm(n)=n
C        enddo
      
      if ( ifirst .eq. 0 )  then
               
         ipt = 0
         
         nzzold = 0
         nszfold = 0
         
c         perm =  0
      
         ifirst = 1
      endif

      
      nzz = icol(nszf+1) 
      do i=1,nzz
         da(i) = 0.
         ja(i) = 0
      enddo
     
      do i=1,nzz
        da(i) = a(i)
        ja(i) = irow(i)
      enddo
      
      do i=1,nszf
         ia(i) = 0
         db(i) = 0.
         dx(i) = 0.
      enddo
      
      do i=1,nszf
         ia(i) = icol(i)
         db(i) = b(i)
         dx(i) = 0.
      enddo


c        do j=1,nzz
c          write(195,*) j,da(j),ja(j)
c        enddo
c        do j=1,nszf
c          write(196,*) j,ia(j)
c        enddo
c        if(ifirst .lt. 2) stop
        
      
      
c      do n=1,nszf
c	  write(299,'(2i10)') n,ia(n)
c	enddo
      
      
      
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
      
      
      n = nszf
      
      nrhs = 1
      
      maxfct = 1
      mnum = 1
      mtype = 11
      ierror = 0
      
      iphase = 11
      
      if ( nzzold .ne. nzz .or.
     &    nszfold .ne. nszf )  then
        ifirst = 1
            write(*,*) ' nszf, nszfold = ', nszf, nszfold
            write(*,*) ' nzz, nzzold   = ', nzz, nzzold
       
      else
c        iparm(4) = 51
            write(*,*) ' nszf, nszfold = ', nszf, nszfold
            write(*,*) ' nzz, nzzold   = ', nzz, nzzold

      endif
      
c      ifirst=1

      call second(sutim1)
       
      if ( ifirst .lt. 2 ) then
      
c         do i=1,mr1
c             perm(i) =  0
c         enddo
C        if(iparm(5) .eq. 0) then

          call pardiso( ipt, maxfct, mnum, mtype, iphase, n, 
     &          da, ia, ja, perm, nrhs, iparm, msglvl,
c     &          db, dx, ierror )
     &          ddum, ddum, ierror )
     
          if ( ierror .ne. 0 ) then
            write(75,*) 'Error phase 11 ', ierror
            stop
          endif
        
          
C        endif  
        ifirst = 2
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
      
      end
C
C        General purpose subroutines
C
      SUBROUTINE MYSORT (IX, Y, N)
C
C        ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
C
C        Sorts the N values stored in array X in ascending order
C
      INTEGER N
      integer ix(n)
      real*8 y(N)
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



C
C        General purpose subroutines
C
      SUBROUTINE ISORT (IX, N)
C
C        ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
C
C        Sorts the N values stored in array X in ascending order
C
      INTEGER N
      integer ix(n)
C
      INTEGER I, J, INCR
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
      J = I
   40 IF (IX(J - INCR) .LT. ITEMP) GOTO 50
      IX(J) = IX(J - INCR)
      J = J - INCR
      IF (J .GT. INCR) GOTO 40
   50 CONTINUE
      IX(J) = ITEMP
      I = I + 1
      GOTO 30
   60 IF (INCR .GT. 1) GOTO 20
C
      RETURN
      END