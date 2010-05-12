SUBROUTINE FRONT_PETSC(nrx)

!ipk AUG07
USE BLK10, only: r1, nlstel, rkeep, ekeep, rkeepeq, lq, mfw
USE BLK10MOD, only: maxn, iteqv, idifsw, mr1, ireallct, nszf, ndf, nesav, nfixh, ne, imat, &
  & icollape, ncn, nops, nop, netyp, nbc, nreord, ncorn, np, nbckp, nd1, nscr, maxe, tvol, iedsw, iedsw1, &
  & tbfact, tbfact1, tbmin, tbmin1, inotr, iutub, igtp, iactv, icpu
USE BLK11MOD, only: ort
USE BLKECOM, only: ncon, estifm, f
USE PardisoParams, only: mfwsiz
USE petsc
USE petscvec
USE petscmat

implicit none

#include "finclude/petscdef.h"
#include "finclude/petscvecdef.h"
#include "finclude/petscmatdef.h"
#include "finclude/petsckspdef.h"
#include "finclude/petscpcdef.h"
#include "finclude/petscsnesdef.h"

PetscErrorCode  :: ierr
PetscMPIInt     :: rank,size
PetscInt        :: noOfEqLocal, eqStart, eqEnd
Vec, save       :: bpetsc
Mat, save       :: Apetsc
integer (kind = 4), save :: nszfold
integer (kind = 4), allocatable :: nnz(:)
PetscInt :: estifmrow(80)

integer (kind = 8) :: mxl, mfrw
PARAMETER (MXL=680)
      
integer :: NK(120), LLDONE(80)

!local variables      
integer (kind = 2), save :: itime
real (kind = 8) :: sec, islp, sinc
integer (kind = 4) :: nszfo, maxl, i, k, m, nn, n, local, global, innerk, innerl, node, estifmsize
integer (kind = 4) :: l, j, icteq, kc
integer (kind = 4) :: ll, jj, icur
integer (kind = 4) :: nell, startel, endel, rangeel
integer (kind = 4) :: nm
integer (kind = 4) :: nmatyp, nrx, ntip, nelm, nod, irwz, ii, ia
integer (kind = 4) :: ja, leltm
real (kind = 8) :: sutim1, sutim2

! superLU
REAL(kind = 8), allocatable ::  R1T(:)
INTEGER (kind = 8), allocatable :: ichgl(:)


data itime/0/
data nszfold/0/

MFRW = mfwsiz

!some comments
!NLSTEL(J)  :: NLSTEL (J) is last element, that is necessary to fill the global equation
!              of degree of freedom J

!NBUFFSIZ   :: size of the memory, that can be used from RMA10S (default definition in param.com to 20000000; user definition in control file!)
!MFWSIZ     :: seems to be the maximum band width of the matrix (default definition in param.com to 2000)
!MR1SIZ     :: seems to be the maximum number of free degrees
      
!
!allocate locals      
allocate (r1t(mr1), ichgl(mr1))
R1T = 0.d0
ICHGL = 0

CALL SECOND(SEC)

!ENe Was missing compared to front.for
      
IF(ITEQV(MAXN) == 2 .OR. ITEQV(MAXN) == 8 .OR. ITEQV(MAXN) == 9) THEN
  IF(IDIFSW == 0) THEN
    ISLP=0
  ELSE
    ISLP=1
  ENDIF
ELSE
  ISLP=0
ENDIF

     IF(IREALLCT > 0) THEN
       IF(IREALLCT == 2) THEN
    DEALLOCATE (R1,NLSTEL,rkeep,ekeep,rkeepeq)
  ENDIF
  ALLOCATE (R1(MR1),NLSTEL(0:MR1),rkeep(0:MR1),ekeep(MR1), rkeepeq(MR1))
ENDIF

IF( .NOT. ALLOCATED(NLSTEL)) ALLOCATE(NLSTEL(0:MR1))
if(maxn == 1 .AND. nrx == 1) nszfo=nszf
maxl = 0
if(nrx == 1) then
  NDF=4
else
  ndf=1
endif
!-
!...... Find last appearance of each node
!-

NLSTEL=0
K=NESAV+1

DO NN=1,NESAV
  K=K-1
  N=NFIXH(K)
  IF(N > 0 .AND. n <= NE) THEN
	  IF(IMAT(N) > 0) THEN
		  !ipk NOV99
		  IF(ICOLLAPE(N) /= 1 .OR. IMAT(N)/1000 == 1) THEN
			  !ipk jan06
			  IF(imat (n) > 0) THEN
			    ncn=20
			    IF(ITEQV(MAXN) == 5) THEN
			      DO I=1,8
			        NCON(I)=NOPS(N,I)
			        IF(NCON(I) > 0) NCN=I
			      ENDDO
			    ELSE
			      DO I=1,NCN
			        NCON(I)=NOP(N,I)
			      ENDDO
			    ENDIF
			    DO M=1,NCN
			      L=NCON(M)
			      !ipk JAN99 SKIP OUT FOR 2DV
			      if(l > 0) THEN
			        DO I=1,NDF
			          J=NBC(L,I)
			          IF(J > 0) THEN
			            IF(NLSTEL(J) == 0) THEN
			              NLSTEL(J)=N
			            ENDIF
			          ENDIF
			        ENDDO
			      ENDIF
			    ENDDO
			  ENDIF
		  ENDIF
	  ENDIF
  ENDIF
ENDDO
   
icteq = 0
do nn = 1, nesav
  !run through elements due to classical reordering order
    n = nfixh (nn)
    ncn = ncorn (n)
  IF (NOP (N, 3) == 0) NCN = 2
  do kc = 1, 80
    nk (kc) = 0
  !ipk feb07 add initialization of LLDONE
    LLDONE (KC) = 0
  enddo

  kc = 0
  !ipk jun07        DO J=1,NCN
  DO J = 1, 20
    I = NOP (N, J)
    IF (I == 0) THEN
      KC = KC + NDF
    ELSE
      oldDOFs: DO L = 1, NDF
  !old global number of DOF
        LL = NBC (I, L)
  !count global ordinal number
        KC = KC + 1
  !ipk feb07 add LLDONE
  !lldone shows, whether old number of DOF is already processed
        LLDONE (KC) = LL
        NK (KC) = LL

        IF (LL /= 0) THEN
  !Check, whether this element is the last occurance of the degree of freedom
          IF (NLSTEL (LL) == N) NK (KC) = -LL

          if(nk(kc) < 0) then
            DO JJ=1,KC-1
  !ipk feb07 add LLDONE test
              IF(LLDONE(JJ) == ABS(LL)) GO TO 250
            ENDDO
              icteq=icteq+1
              ichgl(ll)=icteq
  250             CONTINUE                  
            endif
        ENDIF
      ENDDO oldDOFs
    ENDIF
  enddo
ENDDO
      
!Reset the number of the global equation; reordering is done in solver      
!---------------------------------------------------------------------      
      do n=1,np
            do m=1,ndf
            k=nbc(n,m)
            if(k /= 0) then
              nbc(n,m)=ichgl(k)
              nbckp(n,m)=ichgl(k)
            endif
          enddo
        enddo

      DO J=1,NSZF
        NLSTEL(J)=0
      ENDDO
      K=NESAV+1
      DO NN=1,NESAV
        K=K-1
        N=NFIXH(K)
        IF(N > 0. .AND. N <= NE) THEN
          IF(IMAT(N) > 0) THEN
!ipk NOV99
            IF(ICOLLAPE(N) /= 1 .OR. IMAT(N)/1000 == 1) THEN

!ipk jan06
              IF(imat(n) > 0) THEN
                ncn=20
                IF(ITEQV(MAXN) == 5) THEN
                  DO I=1,8
                    NCON(I)=NOPS(N,I)
                    IF(NCON(I) > 0) NCN=I
                  ENDDO
                ELSE
                  DO I=1,NCN
                    NCON(I)=NOP(N,I)
                  ENDDO
                ENDIF
                DO M=1,NCN
                  L=NCON(M)
!ipk JAN99 SKIP OUT FOR 2DV
                  if(l > 0) THEN
                    DO I=1,NDF
                      J=NBC(L,I)
                      IF(J > 0) THEN
                        IF(NLSTEL(J) == 0) THEN
                          NLSTEL(J)=N
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDIF
        ENDIF
   14   CONTINUE
      ENDDO

      ICUR=nszf+1
      ND1=NSCR
      LQ=0
!-
!...... Initialize
!-
      DO N=1, maxe
        TVOL(N)=0.
      ENDDO

!C-
!C......ESTABLISH DENSITIES AND PRESSURES
!C-
      IF(NRX == 1) THEN
      CALL PRESR
      ENDIF


    CALL SECOND(SINC)

!check for the change of the matrix; if the size of the matrix did not change pardiso can be
!solved with the same 'Analysis symbolic factorization' settings from the previous execution
if (nszfold /= nszf) then
    write(*,*) 'Matrix structure has changed'

    if(nszfold /= 0) then
	    call VecDestroy(bpetsc,ierr)
	    call MatDestroy(Apetsc,ierr)
	    if(allocated(nnz)) deallocate(nnz)
    endif

    ! create new matrices
    call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,nszf,bpetsc,ierr)
    call VecSetOption(bpetsc,VEC_IGNORE_NEGATIVE_INDICES,PETSC_TRUE,ierr)
    call VecGetOwnershipRange(bpetsc,eqStart,eqEnd,ierr)
    call VecGetLocalSize(bpetsc,noOfEqLocal,ierr)
    allocate(nnz(noOfEqLocal))
    do i=1,noOfEqLocal
        nnz(i) = 80
    enddo
    call MatCreateMPIAIJ(PETSC_COMM_WORLD,noOfEqLocal,noOfEqLocal,nszf,nszf,0,nnz,0,nnz,Apetsc,ierr)
    call MatSetOption(Apetsc,MAT_ROW_ORIENTED,PETSC_FALSE,ierr)
else
    call MatZeroEntries(Apetsc, ierr)
    call VecZeroEntries(bpetsc, ierr)
endif

    call MPI_Comm_Rank(PETSC_COMM_WORLD,rank,ierr)
    call MPI_Comm_Size(PETSC_COMM_WORLD,size,ierr)

!-----------------------------      
!START CYCLE OVER ALL ELEMENTS      
!-----------------------------            
    rangeel = int(NE/size)
    startel = rank*rangeel+1
    if(rank == size - 1) then
        endel = ne
    else
        endel = startel+rangeel-1
    end if

   write(*,*) 'Process ', rank, ' is assembling matrix for elements ', startel, ' to ', endel

   nellLoop: DO NELL=startel,endel
   !nellLoop: DO NELL=rank+1,NE,size
      N=NFIXH(NELL)
      
      IF(IMAT(N) <= 0) cycle nellLoop
      NM=mod(IMAT(N),1000)
      IF(NM < 901) THEN
        IF(ORT(NM,1) == 0. .AND. nm /= 89) cycle nellLoop
      ENDIF

      NCN = NCORN(N)
      IF (NCN == 5 .AND. IMAT(N) < 901) NCN=3

!ipk DEC03 ADD IEDSW DEPENDENCE

          if (imat(n) /= 89) then
            NMATYP=(MOD(IMAT(N),1000))
            IEDSW=IEDSW1(NMATYP)
            TBFACT=TBFACT1(NMATYP)
            TBMIN=TBMIN1(NMATYP)
          endif

          IF(ITEQV(MAXN) /= 5) THEN
          
            IF(IMAT(N) > 1000 .AND. IMAT(N) < 5000) THEN
            
              IF(NRX == 2) cycle nellLoop

!ipk NOV99     Either process surface integrals or collapse to 2-d
    
              IF(ICOLLAPE(N) == 0) THEN
                     CALL SURCOF(N,NRX)
              ELSEIF(IMAT(N) < 2000) THEN
                IF(NETYP(N)/10 < 1) THEN
!ipk MAR05
                  IF(INOTR == 0) THEN
                    CALL COEF1(N,NRX)
                  ELSE
                    CALL COEF1NT(N,NRX)
                  ENDIF
!ipk MAR07
!C     Modify tests to allows for IDIFSW

                ELSEIF(IUTUB == 1 .AND. iedsw == 2 .AND. ISLP == 0) THEN

                  IF(INOTR == 0) THEN
                    CALL COEF2D(N,NRX)
                  ELSE
                    CALL COEF2DNT(N,NRX)
                  ENDIF
                ELSEIF(ISLP == 1 .AND. IUTUB == 1 .AND. IDIFSW == 2) THEN
!ipk MAR05
                  IF(INOTR == 0) THEN
                    CALL COEF2D(N,NRX)
                  ELSE
                    CALL COEF2DNT(N,NRX)
                  ENDIF
                ELSE
                  IF(INOTR == 0) THEN
                    CALL COEF2(N,NRX)
                  ELSE
                    CALL COEF2NT(N,NRX)
                  ENDIF
                ENDIF
              ELSE
                cycle nellLoop
              ENDIF

            ELSE
              IF(NETYP(N)/10 == 2) THEN

!ipk nov99 skip if collapsing to 2-d
                IF(ICOLLAPE(N) == 1 .AND. NRX /= 2) cycle nellLoop

!C     Process   threed element
!ipk jan06
                ntip=abs(mod(imat(n),100))
                IF(ORT(ntip,1) == 0) cycle nellLoop
!ipk jan97
!ipk MAR07
                if(iutub == 1 .AND. IEDSW == 2) then
                  CALL COEF3D(N,NRX)
                else
                  call coef3(n,nrx)
                endif
!ipk jan97 end changes
                ELSE
                IF(NETYP(N)/10 == 1) THEN

!C     Process   twod elements

                  IF(IMAT(N) < 900 .OR. NCORN(N) > 5) THEN

!C     Process   horizontal 2d
                    IF(NRX == 2) cycle nellLoop
!ipk jan06
                    ntip=abs(mod(imat(n),100))
                    IF(ORT(ntip,1) == 0) cycle nellLoop
!ipk jan97
!ipk MAR07
                    if(iutub == 1 .AND. IEDSW == 2 .AND. ISLP == 0) THEN
                      IF(INOTR == 0) THEN
                        CALL COEF2D(N,NRX)
                        ELSE
                        CALL COEF2DNT(N,NRX)
                      ENDIF
                    ELSEIF(ISLP == 1 .AND. IUTUB == 1 .AND. IDIFSW == 2) THEN
                      IF(INOTR == 0) THEN
                        CALL COEF2D(N,NRX)
                      ELSE
                        CALL COEF2DNT(N,NRX)
                      ENDIF
                    else
                      IF(INOTR == 0) THEN
                        CALL COEF2(N,NRX)
                        ELSE
                        CALL COEF2NT(N,NRX)
                      ENDIF
                    endif
!ipk jan97 end changes
                    ELSE
!ipk JAN99
                    IF(IMAT(N) > 900 .AND. IMAT(N) < 1000) cycle nellLoop

!C      Process vertical 2d
!ipk nov99 skip if collapsing to 2-d
                    IF(ICOLLAPE(N) == 1 .AND. NRX /= 2) cycle nellLoop
                          CALL COEFV(N,NRX)
                    ENDIF
                ELSE

!C      Process one-d elements
              IF ((imat(n) >= 901 .AND. imat(n) <= 903) .AND. IGTP(n) == 0) then
                CALL Coef1DJunction (N, NRX)

!material type 89 is used for polynom approach              
              ELSEIF (imat(n) /= 89) THEN
                IF(INOTR == 0) THEN
                  CALL COEF1(N,NRX)
                ELSE
                  CALL COEF1NT(N,NRX)
                ENDIF
!use polynom approach              
              ELSEIF (imat(n) == 89) THEN
                CALL COEF1dPoly(N,NRX)
              ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE
!C-
!C...... The calls below are for the case of vertical averaging
!C-
   19       IF(N <= NE) GO TO 20
            NELM=NELM+1
            N=NFIXH(NELM)
            GO TO 19
   20       CONTINUE
            IF(NOPS(N,6) > 0) THEN
              IF(INOTR == 0) THEN
                CALL COEF2(N,NRX)
              ELSE
                CALL COEF2NT(N,NRX)
              ENDIF
            ELSE
              IF(INOTR == 0) THEN
                CALL COEF1(N,NRX)
              ELSE
                CALL COEF1NT(N,NRX)
              ENDIF
            ENDIF
          ENDIF
!ipk jan99

         IF(NETYP(N) == 15 .OR. NETYP(N) == 16) THEN
               IF(NOP(N,14) /= 0) NCN=14
               IF(NOP(N,18) /= 0) NCN=18
         ENDIF

!-----------------------------------------------------------      
!Bring the element residuals into the distributed global residual vector
!Add the estifm to the distributed global matrix
!-----------------------------------------------------------      
!F(IA)  :: local residual (local degree of freedom) in line IA of element matrix      
!RR(JA) :: global residual (degree of freedom) in line JA of the global sparse matrix
        estifmrow = -1
        DO I = 1, NCN
          J = NOP (N, I)
    !non-zero node
          IF (J > 0) THEN
    !one step before first degree of freedom at local node is calculated
            IA = NDF * (I - 1)
    !k runs through the degrees of freedom of the certain local node
            DO K = 1, NDF
    !get the local equation number of degree k at local node I
              IA = IA + 1
    !get the global equation number of global degree k at global node J
              JA = NBC (J, K)
    !If JA is an active degree; means JA /= 0, fill in local residual in global one!
              IF (JA /= 0) THEN
                estifmrow(ia) = ja - 1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        call VecSetValues(bpetsc,80,estifmrow,F,ADD_VALUES,ierr)
        call MatSetValues(Apetsc,80,estifmrow,80,estifmrow,estifm,ADD_VALUES,ierr)
    ENDDO nellLoop

    call MatAssemblyBegin(Apetsc,MAT_FINAL_ASSEMBLY,ierr)
    call MatAssemblyEnd(Apetsc,MAT_FINAL_ASSEMBLY,ierr)
    call VecAssemblyBegin(bpetsc,ierr)
    call VecAssemblyEnd(bpetsc,ierr)

!C
!C ***** CALL THE SOLVER
!C
      call second(sutim1)
      
      write(*,*) 'Time to assemble matrix: ', sutim1-sinc

      !NSZF   :: number of equations
      CALL petsc_solver(NSZF, Apetsc, bpetsc, R1T)

      call second(sutim2)

      write(*,*) 'Time to solve matrix: ', sutim2-sutim1

      DO I=1,NSZF
        R1(I)=R1T(I)
        rkeep(i)=r1t(i)
      ENDDO

      IF(NRX == 1) NDF=6

      nszfold = nszf

    !deallocate local arrays
    deallocate (r1t, ichgl)
END SUBROUTINE
