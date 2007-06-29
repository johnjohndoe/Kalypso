C     Last change:  K    22 Jun 2007    7:47 am
CIPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES 
CIPK  LAST UPDATE MAR 25 2005
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE FEB 10 2004 IMPROVE DIAGNOSTIC
CIPK  LAST UPDATE DEC 16 2003 ADD IEDSW DEPENDENCE
CIPK  LAST UPDATE MAR 18 2003 add diffusion switch ( default of  0 uses old formulations
cipk  last update Mar 22 2001 revise to keep solution and NBC for equation dropout
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
cipk  last update Nov 18 1999 add capability to collapse elements to 2-D
cipk  last update Feb 4 1998 reduce output
cipk  last update Jan 21 97  add option for Smagorinksy in 2-D
      SUBROUTINE FRONT(NRX)
      USE BLK10
      USE BLK10MOD
      USE BLKSANMOD
      !nis,feb07,testing
      USE ParaKalyps
      USE paraflow1dfe
      !-
      SAVE
C
C
C     FRONTAL ELIMINATION ROUTINE USING FULL PIVOTING
C
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK4.COM'
CIPK 
CIPK AUG05      INCLUDE 'BLKSAND.COM'
C      INCLUDE 'RKEP.COM'
C-
      COMMON/BLKE/ ESTIFM(80,80)
C-
CAUG93IPK  COMMON LDEST(80),NK(80)
cWP2006      COMMON NK(80), see NUMBOPT1.FOR!
      COMMON NK(120)

CIPK AUG05       DIMENSION QR(MFW),NCON(20)
      DIMENSION NCON(20)
C-
CIPK AUG05      COMMON/BIGONE/ EQ(MFW,MFW),LHED(MFW),QQ(MFW),PVKOL(MFW)
C-
c     LBMAX=MBUF

!nis,nov06: Local container for Scaling factors at stiffness matrix assembly
      REAL (KIND=4), DIMENSION (80) :: fac
      !local copy of the actual nbn of the element
      INTEGER                       :: temp_nbn
      INTEGER                       :: node, degree
!-

!nis,nov06: Initializing fac
      do init=1,80
        fac(init) = 1.0
      end do
!-

      NMAX = MFW
      CALL SECOND(ASEC)
      ISHRK=0
      TSURC=0
      TCOEFS=0
      TCOEF2=0
      TCOEF1=0

CIPK MAR0 setup switch that says salinity is active

      IF(ITEQV(MAXN) .EQ. 2  .OR.  ITEQV(MAXN) .EQ. 8
     +                       .OR.  ITEQV(MAXN) .EQ. 9) THEN
	  IF(IDIFSW .EQ. 0) THEN
	    ISLP=0
	  ELSE
          ISLP=1
	  ENDIF
      ELSE
        ISLP=0
      ENDIF

      WRITE(ICFL,6198) ASEC
 6198 FORMAT(/10X,' TIME STARTING FRONT =',F10.3)
 6199 FORMAT( /10X,'DELTA T =',F10.3, '  TOTAL T =',F10.3)
      WRITE(*,*)
     +'ELEMENTS-PROCESSED EQNS-PROCESSED CURRENT-FRONT MAX-FRONT'
     +,' PRESENT-BUFFER'
      NEC=0
      IRTC=0
      LQ=0
      LCMAX=0
C
C     PREFRONT
C
C
C...... Find last appeareance of each node moved from LOAD3
C
      !nis,jun07: Initializing it from the beginning of the array
      !DO J=1,NSZF
      DO J = 0, NSZF
      !-
        NLSTEL(J)=0
      ENDDO
      K=NESAV+1
      DO NN=1,NESAV
        K=K-1
        N=NFIXH(K)
        IF(N .LE. NE  .AND.  N .GT. 0) THEN
          IF(ICOLLAPE(N) .EQ. 1  .AND.  IMAT(N)/1000 .NE. 1) GO TO 480

          IF(IMAT(N) .GT. 0) THEN
!NiS,may06:testing
!            WRITE(*,*) 'here i am'
!-
            ncn=20
            IF(ITEQV(MAXN) .EQ. 5) THEN
              DO  I=1,8
                NCON(I)=NOPS(N,I)
                IF(NCON(I) .GT. 0) NCN=I
              ENDDO
            ELSE
              DO I=1,NCN
              NCON(I)=NOP(N,I)
            ENDDO
          ENDIF
          MRC=N
cipk oct98 update to f90
          NTYP=NETYP(N)

          !NiS,may06,com: for junction elements (7 (1D) or 17 (2D))
          IF(MOD(NTYP,10) .EQ. 7) THEN
CIPK JAN99 SKIP OUT FOR 2DV
            if(ntyp .NE. 17) THEN
C-
C...... Search to see if any entry in NLSTEL
C-
              DO M=1,NCN
                L=NCON(M)
C SKIP OUT FOR ZERO
                if(l .GT. 0) THEN
                  DO I=1,7
                    J=NBC(L,I)
                    IF(J .GT. 0) THEN
                      IF(NLSTEL(J) .GT. 0) THEN
                        IF(NREORD(NLSTEL(J)) .GT. NREORD(MRC)) THEN
                          MRC=NLSTEL(J)
                        ENDIF
                        NLSTEL(J)=0
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDIF
  421     CONTINUE
          DO M=1,NCN
            L=NCON(M)
CIPK JAN99 SKIP OUT FOR 2DV
            if(l .GT. 0) THEN
CIPK MAY02 SWITCH NDF TO 7
              DO I=1,7
                J=NBC(L,I)
                IF(J .NE. 0) THEN
                  IF(NLSTEL(J) .EQ. 0) THEN
                    NLSTEL(J)=MRC
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
       ENDIF
  480 CONTINUE
      ENDDO

      NELL=0
      NELM=0
c      DO N=1,NP
c        WRITE(75,*) 'nbc',nrx,N,(NBC(N,M),M=1,4)
c      ENDDO
c      do n=1,nszf
c        write(75,*) 'nlst',n,nlstel(n),netyp(n)
c      enddo

!NiS,may06: reactivate test
!      DO N=1,NP
!          if (n.eq.12816 .or. n.eq.12790 .or. n.eq.12791 .or.n.eq.3286)
!     +                 WRITE(*,*) 'nbc',nrx,N,(NBC(N,M),M=1,4)
!        DO m=1,4
!          !WRITE(*,*) 'nbc',nrx,N,(NBC(N,M),M=1,4)
!          if (nbc(n,m).lt.0) WRITE(*,*) 'nbc',nrx,N,NBC(N,M)
!        ENDDO
!      ENDDO
!-

!NiS,may06:activate for testing and change unit from 75 to *
!      do n=1,nszf
!        write(75,*) 'nlst',n,nlstel(n),netyp(n)
!        write(*,*) 'nlst',n,nlstel(n),netyp(n)
!      enddo
!-

C
C     ASSEMBLY
C
      DO 15 N=1,NSZF
      IPOINT(N)=0
CC      rkeep(n)=0.
CC      ekeep(j)=0.
   15 R1(N)=0.
C-
C......ESTABLISH DENSITIES AND PRESSURES
C-
      IF(NRX .EQ. 1) THEN
	CALL PRESR
	NDF=4
      ENDIF
      LCOL=0
   18 NELL=NELL+1
      NELM=NELM+1
      IF(NELL.GT.NE) GO TO 380
      N=NFIXH(NELM)

c      write(75,*) 'front',n,nelm,netyp(n),imat(n)

!NiS,may06:testing
!      write(*,*) 'front',n,nelm,netyp(n),imat(n),ncn,ndf
!-

      IF (N .EQ. 0)  GO TO 380
      IF(IMAT(N) .LT. 1) GO TO 18
      CALL SECOND(SINC)

CIPK DEC03 ADD IEDSW DEPENDENCE

      NMATYP=(MOD(IMAT(N),1000))
	IEDSW=IEDSW1(NMATYP)
	TBFACT=TBFACT1(NMATYP)
	TBMIN=TBMIN1(NMATYP)

      IF(ITEQV(MAXN) .NE. 5) THEN
          
	IF(IMAT(N) .GT. 1000  .AND.  IMAT(N) .LT. 5000) THEN
	  IF(NRX .EQ. 2) GO TO 18

CIPK NOV99     Either process surface integrals or collapse to 2-d

          IF(ICOLLAPE(N) .EQ. 0) THEN
     	    CALL SURCOF(N,NRX)
          ELSEIF(IMAT(N) .LT. 2000) THEN
            IF(NETYP(N)/10 .LT. 1) THEN
CIPK MAR05
!nis,may07
!Add midside node for polynom approach
!              !EFa Nov06, Fallunterscheidung für 1D-Teschke-Elemente (notwendig?)
!              !nis,feb07: Allow for numbered FFF midsides
!              !IF(nop(n,2).NE.-9999)then
!              IF(nop(n,2) > -1000)then
!              !-
!              !-
!Add midside node for polynom approach
!-

              if ( imat(n) /= 89 ) then
                IF(INOTR .EQ. 0) THEN
                  CALL COEF1(N,NRX)
                ELSE
                  CALL COEF1NT(N,NRX)
	        endif

!nis,may07
!Add midside node for polynom approach
!              !nis,feb07: Allow for numbered FFF midside nodes
!              !ELSEif(nop(n,2).EQ.-9999)then
!              !ELSEif(nop(n,2) < -1000)then
!              !-
!Add midside node for polynom approach
!-

              ELSEif( imat(n) == 89 ) then
                call coef1dFE(n,nrx)
              endif

C     Modify tests to allows for IDIFSW

            ELSEIF(IUTUB .EQ. 1  .and.  iedsw .eq. 2  .AND. ISLP .EQ. 0)
     +	      THEN
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF2D(N,NRX)
	      ELSE
                CALL COEF2DNT(N,NRX)
	      ENDIF
            ELSEIF(ISLP .EQ. 1  .AND.  IUTUB .EQ. 1 .AND. IDIFSW .EQ. 2)
     +        THEN
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF2D(N,NRX)
	      ELSE
                CALL COEF2DNT(N,NRX)
	      ENDIF
      	    ELSE
CIPK MAR05
              IF(INOTR .EQ. 0) THEN
                CALL COEF2(N,NRX)
	      ELSE
	        CALL COEF2NT(N,NRX)
	      ENDIF
            ENDIF
          ELSE
            GO TO 18
          ENDIF
	  CALL SECOND(SOUC)
	  TSURC=SOUC-SINC+TSURC
	ELSE
!NiS,may06: testing
!          WRITE(*,*) NETYP(N)/10, 'NETYP(N)/10', N
!-
	  IF(NETYP(N)/10 .EQ. 2) THEN

cipk nov99 skip if collapsing to 2-d

            IF(ICOLLAPE(N) .EQ. 1 .AND. NRX .NE. 2) GO TO 18

C     Process   threed element
cipk jan97
            if(iutub .eq. 1  .AND.  IEDSW .EQ. 2) then
              CALL COEF3D(N,NRX)
            else
              call coef3(n,nrx)
            endif
cipk jan97 end changes
	    CALL SECOND(SOUC)
	    TCOEFS=TCOEFS+SOUC-SINC
	  ELSE
	    IF(NETYP(N)/10 .EQ. 1) THEN

C     Process   twod elements

CIPK JUN05
	      IF(IMAT(N)  .LT. 900  .OR.  NCORN(N) .GT. 5) THEN

C     Process   horizontal 2d

		IF(NRX .EQ. 2) GO TO 18
cipk jan97
                if(iutub .eq. 1  .AND.  IEDSW .EQ. 2  .AND. ISLP .EQ. 0)
     +            then
                  if(nell .eq. 1)  then
                    write(75,*) 'going to smag'
                  endif
CIPK MAR05
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF2D(N,NRX)
	          ELSE
	            CALL COEF2DNT(N,NRX)
	          ENDIF
                ELSEIF(ISLP .EQ. 1  .AND.  IUTUB .EQ. 1 .AND.
     +            IDIFSW .EQ. 2) THEN
CIPK MAR05
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF2D(N,NRX)
	          ELSE
	            CALL COEF2DNT(N,NRX)
	          ENDIF
                else
CIPK MAR05
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF2(N,NRX)
	          ELSE
	            CALL COEF2NT(N,NRX)
	          ENDIF
                endif
cipk jan97 end changes
	      ELSE
CIPK JAN99
                IF(IMAT(N) .GT. 900  .AND.  IMAT(N) .LT. 1000) GO TO 18

C      Process vertical 2d


cipk nov99 skip if collapsing to 2-d

                IF(ICOLLAPE(N) .EQ. 1  .AND.  NRX .NE. 2) GO TO 18

		CALL COEFV(N,NRX)
	      ENDIF
	      CALL SECOND(SOUC)
	      TCOEF2=SOUC-SINC+TCOEF2
            ELSE

C      Process one-d elements

	      IF(NRX .EQ. 2) GO TO 18
CIPK MAR05
!nis,may07
!Add midside node for polynom approach
!              IF (nop(n,2) > -1000) THEN
              IF (imat(n) /= 89) THEN
                IF(INOTR .EQ. 0) THEN
                  CALL COEF1(N,NRX)
                ELSE
                  CALL COEF1NT(N,NRX)
                ENDIF
!              !EFa Nov06, Fallunterscheidung für 1D-Teschke-Elemente
!                !nis,feb07: Allow for numbered FFF midsides
!                !if (nop(n,2).eq.-9999) then
!              ELSEIF (nop(n,2) < -1000) THEN
              ELSEIF (imat(n) == 89) THEN
                CALL COEF1dFE(N,NRX)
              ENDIF
!Add midside node for polynom approach
!-
              !NiS,may06: testing
              !WRITE(*,*)'element: ', N, ', NCN: ', NCN, ' nach'
              !-
	      CALL SECOND(SOUC)
	      TCOEF1=SOUC-SINC+TCOEF1
	    ENDIF
	  ENDIF
	ENDIF
      ELSE
C-
C...... The calls below are for the case of vertical averaging
C-
   19   IF(N .LE. NE) GO TO 20
	NELM=NELM+1
	N=NFIXH(NELM)
	GO TO 19
   20   CONTINUE
	IF(NOPS(N,6) .GT. 0) THEN
CIPK MAR05
          IF(INOTR .EQ. 0) THEN
            CALL COEF2(N,NRX)
          ELSE
            CALL COEF2NT(N,NRX)
          ENDIF
	  CALL SECOND(SOUC)
	  TCOEF2=TCOEF2+SOUC-SINC
	ELSE
CIPK MAR05
          IF(INOTR .EQ. 0) THEN
            CALL COEF1(N,NRX)
          ELSE
            CALL COEF1NT(N,NRX)
          ENDIF
	  CALL SECOND(SOUC)
	  TCOEF1=TCOEF1+SOUC-SINC
	ENDIF
      ENDIF
cipk jan99

      IF(NETYP(N) .EQ. 15  .OR.  NETYP(N) .EQ. 16) THEN
        IF(NOP(N,14) .NE. 0) NCN=14
        IF(NOP(N,18) .NE. 0) NCN=18
       
      ENDIF

!NiS,may06: testing
!      WRITE(*,*)'element: ', N, ', NCN: ', NCN,ndf
!-
      NBN = NCN*NDF

      DO 21 LK=1,NBN
	LDEST(LK)=0 !NiS,may06: position of special degree of freedom (lk) in equation solution window
	NK(LK)=0 !???
   21 CONTINUE
      KC=0
      DO 23 J=1,NCN
	IF(ITEQV(MAXN) .EQ. 5) THEN
	  I=NOPS(N,J)
	ELSE
	  I=NOP(N,J) !NiS,may06: i becomes node number
	ENDIF
	DO 22 L=1,NDF !NiS,may06: for every degree of freedom
	  KC=KC+1     !NiS,may06: count loop
CIPK JAN99
          if(i .eq. 0) go to 22 !NiS,may06: loop cycle, if node is zero

!nis,may07
!Add midside node for polynom approach
!          !nis,feb07: negative nodes are midside nodes of Flow1DFE elements, so cycle that loop
!          if (i < -1000) GO TO 22
!          !-
!Add midside node for polynom approach
!-

!Nis,mun06:testing
!          if(i.eq.12816 .or. i.eq.12790 .or. i.eq.12791)
!           WRITE(*,*) 'in front:', i, nbc(i,1), n
!           pause
!-

	  LL=NBC(I,L) !NiS,may06: LL becomes global equation number of the degree of freedom L at node I
	  NK(KC)=LL   !NiS,may06: NK saves the equation number of node-degree of freedom; KC runs from 1 to ncn*ndf
	  IF(LL .NE. 0) THEN
            !nis,feb07,testing
            !WRITE(*,*) nk(kc), kc, ll, i, l, MaxFFFMS
            !pause
            !-
	    IF(NLSTEL(LL) .EQ. N) NK(KC)=-LL !NiS,may06: if the current element is the last one of the equation (NLSTEL), then make
                                             !           NK(KC) negative as pointer, that degree of freedom can be taken out
	  ENDIF
   22   CONTINUE
   23 CONTINUE
C-
C...... Set up heading vectors
C-
      LFZ=1
      DO 52 LK=1,NBN !NiS,may06: do for every node-degree-of-freedom; nbn=ndf*ncn
	NODE=NK(LK)  !NiS,may06: get equation number of node-degree-of-freedom
	IF(NODE.EQ.0) GO TO 52 !NiS,may06: if equation number is deactivated, switch
	LM=IABS(NODE) !NiS,may06: get absolute number
	LL=IPOINT(LM) !NiS,may06: IPOINT is always zero at the first call
	IF(LL .NE. 0) THEN
	  LDEST(LK)=LL
	  IF(NODE .LT. 0) LHED(LL)=NODE
	ELSE
C
C     Look for vacant slot
C
	  DO 35 L=LFZ,LCOL
	    IF(LHED(L) .EQ. 0) THEN
	      IPOINT(LM)=L
	      LHED(L)=NODE
	      LDEST(LK)=L
	      LFZZ=L
	      ISHRK=ISHRK-1
	      GO TO 40
	    ENDIF
   35     CONTINUE
	  LCOL=LCOL+1
	  LDEST(LK)=LCOL
	  LHED(LCOL)=NODE
	  IPOINT(LM)=LCOL
	  LFZZ=LCOL
	  DO 38 L=1,LCOL
	    EQ(L,LCOL)=0.
	    EQ(LCOL,L)=0.
   38     CONTINUE
   40     LFZ=LFZZ
	ENDIF
   52 CONTINUE
CIPK FEB04
      !nis,dec06: if LHED(L).eq.0, the loop should be cycled because of assignment problems
      DO L=1,LCOL
      !columnassigning: DO L=1,LCOL
	  LEQ=ABS(LHED(L))
          !nis,dec06: see above
          !if (LEQ.eq.0) CYCLE columnassigning
          !-
	  IF(NLSTEL(LEQ) .EQ. N) LHED(L)=-ABS(LHED(L))
      ENDDO !columnassigning

      IF(LCOL .GT. LCMAX) LCMAX=LCOL
      IF(MOD(NELL,1000) .EQ. 0) THEN
        WRITE(*,'(I18,I15,I14,I10,I15)') NELL,NEC,LCOL,LCMAX,LQ
      ENDIF
      IF(LCOL.LE.NMAX) GO TO 54
      NERROR=2
CIPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,417)NERROR
      WRITE(75,417)NERROR
      WRITE(*,6008)   NFIXH(NELL)
      WRITE(75,6008)   NFIXH(NELL)
 6008 FORMAT( // 10X, '..STOP AT ELEMENT', I10 )
      WRITE(75,9820) (LHED(L),L=1,LCOL)
      WRITE(75,9822) (EQ(L,L),L=1,LCOL)
CIPK FEB04
 9823   FORMAT('  OFFENDING EQN      NODE   DEG OF FR')
 9824   FORMAT(I15,I10,I12)
        WRITE(75,9823)
        DO L=1,LCOL
	    NEQN=ABS(LHED(L))
	    IF(NEQN .GT. 0) THEN
   	      DO N=1,NP
              DO M=1,3
                IF(NBC(N,M) .EQ. NEQN) THEN
                  WRITE(75,9824) NEQN,N,M
	            GO TO 53
                ENDIF 
	        ENDDO
	      ENDDO
	    ENDIF
   53     CONTINUE
	  ENDDO
      WRITE(75,9821) (N,(NBC(N,M),M=1,4),N=1,NP)
      STOP
   54 CONTINUE

        !nis,nov06: introduce a scaling factor, that is in general 1.0, but changes at connections. It may be also used for other
        !           node to node relationships
        do node = 1,ncn
          !for every degree of freedom, because NBN = NDF * NCN
          do degree = 1,ndf
            !get line in Matrix
            temp_nbn = node * degree
            !get the factor; the factor is .ne. 1.0, if the node is     in a line-Connection
            !                the factor is .eq. 1.0, if the node is not in a line-Connection
            !WRITE(*,*) nop(n,node), node, n, degree, temp_nbn
            if (degree == 1 .or. degree == 2) then
              fac(temp_nbn) = EqScale(nop(n,node), degree)
            ELSEIF (degree == 3) then
              fac(temp_nbn) = 1.0
            end if
            !testing
            !if (fac(temp_nbn).ne.1.0) then
            !  WRITE(*,*) 'Factor wird angewendet:'
            !  WRITE(*,*) fac(temp_nbn), nop(n,node), n, degree
            !  WRITE(*,*) 'Faktor, Knoten, Element, freiheitsgrad'
            !end if
            !-
          enddo
        end do
        !-

      !NiS,jun06,comment: Assembly of global matrix within the solution window
      !nis,jun06,com: for every nodal degree of freedom (row of element matrix)
      DO 57 L=1,NBN
        !nis,jun06,com: if equation is present
        IF(NK(L) .NE. 0) THEN
          !nis,jun06,com: take the solution window slot
          LL=LDEST(L)
          !nis,jun06,com: then take again every nodal degree of freedom (column of element matrix)
          DO 56 K=1,NBN
	  IF(NK(K) .NE. 0) THEN
	    KK=LDEST(K)
!nis,nov06: Insert scaling factor for e.g. line-Transitions. The factor scales the coefficients in the column of the local matrix. The factor
!           gives a relation between the local nodal degree of freedom value and the referenced value. It is calculated from the solution of
!           the initial guess/solution of the equation system.
!
!           SWITCH EXPLANATION
!           If there is a 1D-2D-coupling, then coming from the initial guess, the relation between
!           the 1D-coupling point and each of the coupled 2D-points is known and can be expressed as a factor. For the iteration step it is assumed,
!           that this relation is the end-relation. The result of the line-scaling is, that the variable in all local equation systems, that are
!           connected to one coupling point can be collapsed to one point with two degrees of freedom. The local equations at the coupling can then
!           all come together.
!           EQ(LL,KK)=EQ(LL,KK)+ESTIFM(K,L)
            !nis,nov06,testing:
            !WRITE(*,*) 'Fac(L)=', fac(l)
            !-
            !nis,feb07,testing:
            !if (fac(l) /= 1.0) then
            !  WRITE(*,*) 'Spalte', LL, 'Zeile', KK
            !  WRITE(*,*)  'Wert: ', EQ(ll,KK), 'Scaling: ', Fac(L)
            !endif
            !-
            !IF(n == 1910) then

            EQ(LL,KK) = EQ(LL,KK) + ESTIFM(K,L) * Fac(L)
!-
	  ENDIF
   56     CONTINUE
	ENDIF
   57 CONTINUE
      !nis,feb07,testing (stop for eq-output)
      !pause
      !-
C
C     FIND OUT WHICH MATRIX ELEMENTS ARE FULLY SUMMED
C
   60 LPIVCO=0
      PIVOT=0.
      DO 64 L=1,LCOL
	IF(LHED(L) .GT. -1) GO TO 64
C     WRITE(*,*) 'NELL,LHED,EQ',NELL,LHED(L),EQ(L,L)
	PIVA=EQ(L,L)
	IF(ABS(PIVA) .LT. ABS(PIVOT)) GO TO 64
	PIVOT=PIVA
	LPIVCO=L
   64 CONTINUE
      IF(LPIVCO.EQ.0) GO TO 18
C     WRITE(*,*) 'LPIVCO,PIVOT',LPIVCO,PIVOT
      IF( ABS(PIVOT) .LT. 1.0E-8 .AND. NELL .LE. NE ) GO TO 18
C
C     NORMALISE PIVOTAL ROW
C
      LCO=IABS(LHED(LPIVCO))
C     IF(ABS(PIVOT).LT.1E-08)WRITE(ICFL,476)
CTEMP     pivtin=1.0/pivot
      DO 80 L=1,LCOL
	QQ(L)=EQ(L,LPIVCO)/PIVOT
CTEMP        QQ(L)=EQ(L,LPIVCO)*pivtin
	QR(L)=EQ(LPIVCO,L)
   80 CONTINUE
      QR(LPIVCO)=0.0
      QQ(LPIVCO)=0.0
      RHS=R1(LCO)/PIVOT
CTEMP      RHS=R1(LCO)*pivtin
      R1(LCO)=RHS
      PVKOL(LPIVCO)=PIVOT
C
C     ELIMINATE THEN DELETE PIVOTAL ROW AND COLUMN
C
      DO 100 K=1,LCOL
	IF(QR(K) .EQ. 0.) GO TO 100
	KRW=IABS(LHED(K))
	R1(KRW)=R1(KRW)-QR(K)*RHS
	DO 90 L=1,LCOL
	  EQ(L,K)=EQ(L,K)-QR(K)*QQ(L)
   90   CONTINUE
  100 CONTINUE  
C
C     WRITE PIVOTAL EQUATION ON DISC
C
      NEC=NEC+1
      LCS(NEC)=LCOL
      LPS(NEC)=LPIVCO
      DO 105 L=1,LCOL
	LQ=LQ+1
	LHS(LQ)=LHED(L)
	QS(LQ)=QQ(L)
  105 CONTINUE
      IF(LQ .LT. LBMAX-NMAX) GO TO 108
      CALL XWRT(ND1,-1,NRR)
      IRTC=IRTC+1
      LQ=0
 108  CONTINUE
      DO 109 L=1,LCOL
	EQ(L,LPIVCO)=0.
	EQ(LPIVCO,L)=0.
  109 CONTINUE
      LHED(LPIVCO)=0
C
C     REARRANGE HEADING VECTORS
C
C      IF(ISHRK .GT. LCOL/10+4) THEN
       IF(ISHRK .GE. 40) THEN
C
CAUG93  Changes start here
c       KM=0
c       DO 120 K=1,LCOL
c         IF(LHED(K) .NE. 0) THEN
c           KM=KM+1
c           LM=0
c           DO 110 L=1,LCOL
c             IF(LHED(L) .NE. 0) THEN
c               LM=LM+1
c               EQ(LM,KM)=EQ(L,K)
c             ENDIF
c 110       CONTINUE
c         ENDIF
c 120   CONTINUE
c       KM=0
c       DO 125 K=1,LCOL
c         IF(LHED(K) .NE. 0) THEN
c           KM=KM+1
c           LHED(KM)=LHED(K)
c           IPOINT(ABS(LHED(K)))=KM
c         ENDIF
c 125   CONTINUE
c       ISHRK=0
c       LCOL=KM
	KM=0
	DO 110 K=1,LCOL
	  IF(LHED(K) .NE. 0) THEN
	    KM=KM+1
	    LHED(KM)=LHED(K)
	    IPOINT(ABS(LHED(K)))=KM
	    LDEST(KM)=K
	  ENDIF
  110   CONTINUE
	LCOL=KM
C
	DO 120 K=1,LCOL
	    KM=LDEST(K)
	    DO 115 L=1,LCOL
		LM=LDEST(L)
		EQ(L,K)=EQ(LM,KM)
  115       CONTINUE
  120   CONTINUE
	ISHRK=0
C
CAUG93 changes end here
      ELSE
	ISHRK=ISHRK+1
      ENDIF
C
C     DETERMINE WHETHER TO ASSEMBLE,ELIMINATE,OR BACKSUBSTITUTE
C
      IF( LCOL .GT. 0) GO TO 60
      IF( NELL .LT. NE) GO TO 18
  380 CONTINUE
      IF(LCOL .GT. 0) THEN
	DO 400 L=1,LCOL
	  IF(LHED(L) .NE. 0) GO TO 405
  400   CONTINUE
	GO TO 420
  405   CONTINUE
CIPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')

	WRITE(75,479) LCOL,ISHRK
	WRITE(*,479) LCOL,ISHRK
  479   FORMAT( '  UNSATISFIED ELIMINATION ERROR STOP'/
     1  '   LCOL =',I5,' ISHRK =',I5)
	WRITE(75,9820) (LHED(L),L=1,LCOL)
	WRITE(75,9822) (EQ(L,L),L=1,LCOL)
 9820   FORMAT('  CONTENTS OF LHED ARE'/(5I8))
	WRITE(75,9821) (N,(NBC(N,M),M=1,4),N=1,NP)
 9821   FORMAT('  NBC ARRAY IS '/(5I8))
 9822   FORMAT('  EQ IS'/(1P5E12.4))
	CALL ZVRS(1)
	STOP 'Unsatisfied elimination error'
      ENDIF
  420 CONTINUE
      TTOTA=TSURC+TCOEF2+TCOEFS+TCOEF1
      WRITE(*,7804) TCOEF1,TSURC,TCOEF2,TCOEFS,TTOTA,LCMAX,IRTC,LQ
      WRITE(75,7804) TCOEF1,TSURC,TCOEF2,TCOEFS,TTOTA,LCMAX,IRTC,LQ
 7804 FORMAT(20X,'TIME IN 1-DIM   ELEMENTS',F10.3/
     +       20X,'TIME IN SURFACE ELEMENTS',F10.3/
     1       20X,'TIME IN 2-DIM   ELEMENTS',F10.3/
     2       20X,'TIME IN 3-DIM   ELEMENTS',F10.3/
     3       20X,'TOTAL TIME IN   ELEMENTS',F10.3/
     4       20X,'MAXIMUM FRONT WIDTH     ',I10/
     5       20X,'BUFFER BLOCKS WRITTEN   ',I10/
     6       20x,'FINAL LQ SIZE           ',I10)
      CALL SECOND(TA)
      ADT=TA-ASEC
      WRITE(75,6199) ADT,TA
      WRITE(*,6199) ADT,TA
C      WRITE(LOUT,7805) ADT,TTOTA,LCMAX,IRTC
C 7805 FORMAT(20X,'TIME IN FORWARD SECTION OF FRONT',F10.3/
C     3       20X,'TOTAL TIME IN   ELEMENTS        ',F10.3/
C     4       20X,'MAXIMUM FRONT WIDTH             ',I10/
C     5       20X,'BUFFER BLOCKS WRITTEN           ',I10)
C
C     BACK SUBSTITUTION
C
      NEC=NSZF+1
      DO 600 IV=1,NSZF
      NEC=NEC-1
      LCOL=LCS(NEC)
      LPIVCO=LPS(NEC)
      LQ=LQ-LCOL
      IF(LQ .GT. -1) GO TO 450
      CALL XRED(ND1,IRTC,NRR)
C     CALL RED(ND1,-1)
      LQ=LQ-LCOL
  450 DO 460 L=1,LCOL
      LQ=LQ+1
      LHED(L)=IABS(LHS(LQ))
  460 QQ(L)=QS(LQ)
      LQ=LQ-LCOL
      LCO=LHED(LPIVCO)
      GASH=0.
      QQ(LPIVCO)=0.
      DO 580 L=1,LCOL
      ITMM=LHED(L)
      IF(ITMM .NE. 0) THEN
	GASH=GASH-QQ(L)*R1(ITMM)
      ENDIF
  580 CONTINUE
      !nis,feb07,testing
      !if (lco == 2) then
      !  WRITE(*,*) 'Gelange zur Veraenderung von r1'
      !  WRITE(*,*) 'Zeile 2: ', r1(2)
      !endif
      !-
      R1(LCO)=R1(LCO)+GASH
      !nis,feb07,testing
      !if (lco == 2) then
      !  WRITE(*,*) 'Gelange zur Veraenderung von r1'
      !  WRITE(*,*) 'Zeile 2: ', r1(2)
      !  pause
      !endif
      !-
  600 CONTINUE
      CALL SECOND(TAA)
      ADT=TAA-TA
      WRITE(ICFL,6199) ADT,TAA

cipk mar01  Save results and NBC for equation dropout
      IF(IDRPT .GT. 0  .AND. NRX .EQ. 1) THEN
        do j=1,nszf
          RKEEP(J)=R1(J)
        enddo
	  DO J=1,NP
  	    DO K=1,NDF
	      NBCKP(J,K)=NBC(J,K)
	    ENDDO
	  ENDDO
	ENDIF

C      do n=1,np
C	do m=1,ndf
C	  if(nbc(n,m) .gt. 0) then
C          write(73,*) n,m,nbc(n,m),rkeepeq(nbc(n,m))
C        endif
C	enddo
C	enddo

  417 FORMAT(/' NERROR =',I5//
     1 'MFW IS NOT LARGE ENOUGH TO PERMIT ASSEMBLY OF THE NEXT EL'
     2,'EMENT'/'  INCREASE MFW IN PARAM.COM OR LOOK FOR ERROR IN'
     3,' ELEMENT ELIMINATION ORDER')
  476 FORMAT(' WARNING-MATRIX SINGULAR OR ILL CONDITIONED')
      IF(NRX .EQ. 1) NDF=6
      RETURN
      END
