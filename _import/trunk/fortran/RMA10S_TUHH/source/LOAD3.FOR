cipk  last update feb 26 2006 corect logic for 1-d structures and limit reordering to 1
cipk  last update dec 13 2006 seup for type QI (experimental)
cipk  last update nov 28 2006 allow for 1-d control structures
CIPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
CIPK  LAST UPDATE SEP 6 2004   add error file
CIPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
cipk  last update Nov 12 1999 add dropout of nodes for 3-d to 2-d
CIPK  LAST UPDATE JAN 12 1999 FIX 2DV JUNCTION LOGIC
cipk  last update Dec 5 1998 fix error at transitions
C     Last change:  EF    3 May 2007    9:53 am
cipk  last update Feb 4 1998 correct zero subscript test and for HCN
CIPK  LAST UPDATE JAN 22 1998
cipk  last update Dec 16 1997
cipk  last update April 28 1997 add equation dropout
cipk  last update Oct 31 1996 save nfixh when using optim
cipk  last update Sep 4 1996  change logic for type 2 spec's of constits
      SUBROUTINE LOAD
      USE BLK10MOD
      USE BLK11MOD
      USE BLK10
      USE BLKSUBMOD
      USE BLKSANMOD
CIPK DEC06      
      USE PARAMMOD
      USE BLKSBG
      SAVE
!NiS,may06:testing for non defined variables
!      implicit none
!-
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      include 'BLK4.COM'
CIPK JUN05
CIPK AUG05      INCLUDE 'BLKSUB.COM'
C
C......DIMENSION STATEMENTS
C-
      DIMENSION IM(20,3),ISHF(20,2),NCON(20),Q1C(3)
      DATA IM/1,0,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,
     1        1,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,
     2        1,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0/
      DATA ISHF/0,0,0,3,3,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
     + 0,0,0,3,3,2,1,1,0,0,10,9,0,0,0,15,15,14,13,13/
      DATA IENT/0/,PI2/1.5708/
      DATA CNMIN,PCERIN/1.0,1.0/ITIMTH/1/
      Dist(n1,n2)=sqrt((cord(n1,1)-cord(n2,1))**2 + 
     +                 (cord(n1,2)-cord(n2,2))**2)

!nis,nov06: Defintion of local variables for 1D-2D-line-transition handling
      INTEGER :: LiNo, LiLe, CoNode
!-

C-
C       NTHREE(N) = 1 FOR THREE DIMENSIONAL NODES
C       NTHREE(N) = 0 FOR TWO DIMENSIONAL HORIZONTAL NODES
C-
C...... Set direction for 1D - 2D junction node plus other initialise.
C-
      IF(IENT .EQ. 0) THEN
        CALL GETCON
        !NiS,may06: copy of element and point number
        NPSAV=NP
        NESAV=NE

        !NiS,may06: do for every element
        DO 140 N=1,NE
          NN=NFIXH(N)
cipk oct96 save original order
C          nelord(n)=nfixh(n)
CZZZ
            if (nn .eq. 0)  goto 140
          !NiS,may06: other way round, to every equation processing number an element number is stored
          NREORD(NN)=N
  140   CONTINUE
        IENT=IENT+1
      ENDIF

      !nis,dec06: EXTLDEL is an array for all elements. Initialization moved to initl.for
      !EXTLDEL=0
      !-

      NP=NPSAV
      NE=NESAV

      !NiS,may06: do for every node

!NiS,may06: test
!      OPEN(9999,'testfile.txt')
!-
      do n=1,np

!NiS,may06: test
!      testing variable iactvbc
!      IF(iactvbc(n).gt.0 .and. maxn.eq.1)
!    +   WRITE(9999,*) 'iactvbc(',N,') =', iactvbc(N)
!      testing node assignment at surface
!      IF(N.ne.nsurf(n)) WRITE(9999,*) 'NSURF(',N,') = ', NSURF(N)
!      testing the nodes below
!      if (nref(n).ne.0) WRITE(9999,*) 'NREF(',N,') = ', NREF(N)
!-

          IF(MOD(NFIX(N),100)/10 .EQ. 2) THEN
            ibc=iactvbc(n)
	      if(ibc .eq. 0) then
              NFD=NFIX(N)/100
              NFIX(N)=NFD*100+MOD(NFIX(N),10)
            endif
	    endif
          IF(MOD(NFIX(N),10) .EQ. 2) THEN
            ibc=iactvbc(n)
	      if(ibc .eq. 0) then
              NFD=NFIX(N)/10
              NFIX(N)=NFD*10
            endif
	    endif
          IF(NFIX1(N) .EQ. 2) THEN
            ibc=iactvbc(n)
	      if(ibc .eq. 0) then
              NFIX1(N)=0
            endif
	    endif
      enddo

!NiS,may06: test
!      CLOSE(9999)
!-

c      write(75,*) 'load-76 nfix(93),nfix(634)',nfix(93),nfix(634),
c     +    maxn,iteqv(maxn)

C-
C-.....COPY HEAD SPECS AND FIX BOUNDARY CONDITIONS.....
C-
      !NiS,may06: set degree of freedom number
      NDF=6
      !NiS,may06: NA.eq.1, ALWAYS!
      NA = 10**(6-NDF)
      !NiS,may06: store temporary array for boundary conditions of every node
      DO 195 J = 1, NP
        !NiS,may06: bring NFIX and NFIX1 together into one array
        NLOC(J)=NFIX(J)*10+NFIX1(J)
        !NiS,may06: if salinity part-value equals 2, then subtract 200
        IF( MOD( NLOC(J)/100,10 ) .EQ. 2 ) NLOC(J) = NLOC(J) - 200
        !NiS,may06: NA.eq.1; ALWAYS!
        NLOC(J) = NLOC(J) / NA

  195 CONTINUE
C-
C-    SET UP NBC ARRAY
C-
      DO 197 N=1,NP
        NTHREE(N)=1 !NiS,may06: at the beginning every element is 3D
        NBC(N,7)=-1 !NiS,may06: NBC(N,M), M=1..7 are evaluated by -1
        DO 197 M=1,NDF
  197     NBC(N,M)=-1
      !NiS,may06: end of do-loop 197

!NiS,may06: testing
!      OPEN(9998, 'testfile.txt')
!-

      !NiS,may06: for every element
      DO 199 J=1,NE

CIPK DEC00 REVISE TO ALLOW FOR GATE STRUCTURE

        NM=IMAT(J)
        !NiS,may06: IMAT .lt. 1 means deactivated element; jump over that
        IF(NM .LT. 1) GO TO 199
CIPK JUN05
        !NiS,may06: 3D- or special elements; jump over that
        IF(NM .GT. 900  .AND.  NM .LT. 5001  .AND.  IGTP(J) .EQ. 0
     +    .AND. NFCTP(J) .EQ. 0)
     +    GO TO 199
CIPK OCT98 CONVERT TO F90
        !NiS,may06: every element that is in the range of 1-899 and 1001+
        IF(NM .LT. 900  .OR. NM .GT. 1000) THEN
          L=MOD(NM,100) !NiS,may06: for every normal element
        ELSE
          L=NM !NiS,may06: for special elements
        ENDIF
        !NiS,may06: if no turbulent exchange coefficient, jump over element
        IF(ORT(L,1) .EQ. 0.) GO TO 199
        ILK=1

        !NiS,may06: ITEQV.eq.5 means forced reduction to plan view
        IF(ITEQV(MAXN) .EQ. 5) THEN
          IF(J .GT. NEM) GO TO 1991
          DO 1950 K=1,8
            NCON(K)=NOPS(J,K)
            IF(NCON(K) .GT. 0) NCN=K
 1950     CONTINUE
        !NiS,may06: every other case instead of forced reduction to plan view
        ELSE
          NCN=NCORN(J)
          DO 1951 K=1,NCN
            !NiS,may06: get local corner nodes of processed element
            NCON(K)=NOP(J,K)
 1951     CONTINUE
        ENDIF
        !NiS,may06: special cases for 3D-elements
        IF(NCN .EQ. 15) ILK=2
        IF(NCN .EQ. 13) ILK=3
        IF(NCN .EQ. 10) ILK=2

        !NiS,may06: process every node of element
        !nis,feb07: Rename loop for f90
        !do 198 k = 1, NCN
        NBCrelation: DO K=1,NCN
        !-
          !NiS,may06: local copy of processed node number
          N=NCON(K)
          !nis,feb07: Cycle, if nod is negative. It's only possible for midside nodes, which shouldn't be processed here
          if (n < 0) CYCLE NBCrelation
          !-
C-
C........ Setup for special case inflow quality conditions
C-

          !NiS,may06: no velocities, so no direction is applied
          IF (VEL(2,N) .EQ. 0. .AND. VEL(1,N) .EQ. 0) THEN
            VDIR = 0.
          !NiS,may06: superposition of component velocities for direction of flow
          ELSE
            VDIR=ATAN2(VEL(2,N),VEL(1,N))
          ENDIF
CIPK OCT98 CONVERT TO F90
          N1=ABS(NSURF(N))
          DIF=VDIR-VOUTN(N1)
          IF(DIF .GT. 2.*PI2) DIF=DIF-4.*PI2
          IF(DIF .LT. -2.*PI2) DIF=DIF+4.*PI2
CIPK OCT98 CONVERT TO F90
          NFTYP=NFIX(N)
          IF(MOD(NFTYP,100)/10 .EQ. 2) THEN
            IRMD=MOD(NFTYP,10)*10+NFIX1(N)
cipk sep96            IF(ABS(DIF) .LT. PI2) THEN
c             NLOC(N)=10*(NLOC(N)/10)
              nloc(n)=100+1000*(nloc(n)/1000)+IRMD
CIPK JAN98
              vel(4,n)=spec(n,4)
cipk sep96            ELSE
cipk sep96              nloc(n)=100+1000*(nloc(n)/1000)+IRMD
cipk sep96              VEL(4,N)=SPEC(N,4)
cipk sep96            ENDIF
          ENDIF
          IF(MOD(NFTYP,10) .EQ. 2) THEN
cipksep96            IF(ABS(DIF) .LT. PI2) THEN
c             NLOC(N)=10*(NLOC(N)/10)
              nloc(n)=10+100*(nloc(n)/100)+NFIX1(N)
CIPK JAN98
              vel(5,n)=spec(n,5)
cipk sep96            ELSE
cipk sep96              nloc(n)=10+100*(nloc(n)/100)+NFIX1(N)
cipk sep96              VEL(5,N)=SPEC(N,5)
cipk sep96            ENDIF
          ENDIF
          IF(NFIX1(N) .EQ. 2) THEN
cipk sep96            IF(ABS(DIF) .LT. PI2) THEN
c             NLOC(N)=10*(NLOC(N)/10)
              nloc(n)=1+10*(nloc(n)/10)
CIPK JAN98
              vel(6,n)=spec(n,6)
cipk sep96            ELSE
cipk sep96              nloc(n)=1+10*(nloc(n)/10)
cipk sep96              VEL(6,N)=SPEC(N,6)
cipk sep96            ENDIF
          ENDIF
          !NiS,may06: store pointer for every node, that is not 3D
          IF(NCN .LT. 9  .AND.  IMAT(J) .LT. 1000) NTHREE(N)=0
          NDS=2
          !NiS,may06: NDF is always greater than 3, that means all the concentrations
          IF(NDF .GT. 3) THEN
            DO 1970 KK=4,NDF
              NBC(N,KK)=0
 1970       CONTINUE
          ENDIF
          IF(IM(K,ILK) .NE. 0) THEN
            NBC(N,3)=-NSURF(N)-1
            IF(NDF .GT. 3) THEN
              DO 1971 KK=4,NDF
                NBC(N,KK)=0
 1971         CONTINUE
            ENDIF
            IF(NSURF(N) .EQ. N) NDS=3
          ENDIF
          DO 1976 I=1,NDS
 1976     NBC(N,I)=0
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
CIPK JUN02
          NBC(N,7)=0
!nis,feb07: rename loop to f90
!  198   CONTINUE
         ENDDO NBCrelation
!-
  199 CONTINUE

      !NiS,may06:testing
      !CLOSE(9998)
      !-

 1991 CONTINUE
C
C                                       FORM DEGREE OF FREEDOM ARRAY
C
      DO 220 N=1,NP
        K=10**NDF
cipk apr97 change loop to 210 - add code for element dropout
        DO 210 M=1,NDF
          K=K/10
          NBC(N,M)=NBC(N,M)+1
          IF(NBC(N,M) .NE. 1) GO TO 200
          IF(NLOC(N)/K .NE. 1) GO TO 200
          NBC(N,M)=0
  200     NLOC(N)=MOD(NLOC(N),K)
cipk feb97 add code for element dropout
c       if(iactv(n,m) .eq. 0) then
c         write (*,*) ' debug load: n = ',n,' m = ',m
c         write (*,*) ' debug load: iactv = ',iactv(n,m)
c       endif
        if(iactv(n,m) .eq. 0) nbc(n,m)=0
  210   continue
cipk apr97 end change
C
C.....  Set up for case of straight line salinity
C
        IF(NSTRT(N,1) .NE. 0) NBC(N,4)=0
  	  IF(ITEQV(MAXN) .EQ. 10) NBC(N,7)=NBC(N,7)+1
C
  220 CONTINUE

CIPK DEC00       test for gate structure

      if(maxn .lt. 2) then
        DO N=1,NE
	    igtcl(n)=0
          IF(IGTP(N) .NE. 0) THEN
            NGT=IMAT(N)-900
            IF(NDUPJ(NGT) .GT. 0  .AND. NDDNJ(NGT) .GT. 0) THEN
              IF(WSLL(NDUPJ(NGT)) .LT.  WSLL(NDDNJ(NGT))   .AND.
     +          WSLL(NDUPJ(NGT)) .GT. BJ(NGT)) THEN
	          CALL SETGT(N)
   	          igtcl(n)=1
              ELSE
                NSTRT(NOP(N,2),1)=0
              ENDIF
			write(75,*) 'gate setting', n,igtcl(n)          
	      ELSEIF(NDFLJ(NGT) .GT. 0) THEN
	        IF(NDUPJ(NGT) .EQ. 0  .AND. NDDNJ(NGT) .EQ. 0) THEN
                ACR=(2.*WIDTH(NDFLJ(NGT))+VEL(3,NDFLJ(NGT))*
     +		  (SS1(NDFLJ(NGT))+SS2(NDFLJ(NGT))))/2.*VEL(3,NDFLJ(NGT))
CC               QFL=ACR*SQRT(VEL(1,NDFLJ(NGT))**2+VEL(2,NDFLJ(NGT))**2)
                QFL=(VEL(1,NDFLJ(NGT))*COS(ALFA(NDFLJ(NGT)))+
     +               VEL(2,NDFLJ(NGT))*SIN(ALFA(NDFLJ(NGT))))*ACR
	          write(75,*) 'gate flow',qfl,aj(ngt)
	          IF(AJ(NGT) .GT. 0.) THEN
			    IF(QFL .LT. AJ(NGT)) THEN
	              CALL SETGT(N)
                    igtcl(n)=1
                  ELSE
                    NSTRT(NOP(N,2),1)=0
                  ENDIF
	          ELSE
			    IF(QFL .GT. AJ(NGT)) THEN
	              CALL SETGT(N)
                    igtcl(n)=1
                  ELSE
                    NSTRT(NOP(N,2),1)=0
                  ENDIF
	          ENDIF
			  write(75,*) 'gate setting', n,igtcl(n)          
			ELSEIF(NDUPJ(NGT) .GT. 0) THEN
                ACR=(2.*WIDTH(NDFLJ(NGT))+VEL(3,NDFLJ(NGT))*
     +		  (SS1(NDFLJ(NGT))+SS2(NDFLJ(NGT))))/2.*VEL(3,NDFLJ(NGT))
CC               QFL=ACR*SQRT(VEL(1,NDFLJ(NGT))**2+VEL(2,NDFLJ(NGT))**2)
                QFL=(VEL(1,NDFLJ(NGT))*COS(ALFA(NDFLJ(NGT)))+
     +               VEL(2,NDFLJ(NGT))*SIN(ALFA(NDFLJ(NGT))))*ACR

	          write(75,*) 'gate head flow',wsll(ndupj(ngt)),bj(ngt),
     +		  qfl,aj(ngt)
	          write(75,*) 'vels',VEL(1,NDFLJ(NGT)),Vold(1,NDFLJ(NGT))
	          IF(WSLL(NDUPJ(NGT)) .LT. BJ(NGT)) THEN
	            CALL SETGT(N)
                  igtcl(n)=1
	          ELSEIF(AJ(NGT) .GT. 0) THEN
                  IF(QFL .GT. AJ(NGT)) THEN
  	              CALL SETGT(N)
                    igtcl(n)=1
	            ELSE
                    NSTRT(NOP(N,2),1)=0
	            ENDIF
                ELSE
                  IF(QFL .LT. AJ(NGT)) THEN
  	              CALL SETGT(N)
                    igtcl(n)=1
	            ELSE
                    NSTRT(NOP(N,2),1)=0
	            ENDIF
                ENDIF
			  write(75,*) 'gate setting', n,igtcl(n)          
	        ENDIF
            ENDIF
          ENDIF
        ENDDO
      else
	  DO N=1,NE
          IF(IGTP(N) .NE. 0) THEN
            if(igtcl(n) .eq. 1) then
  	        call setgt(n)
	      else
              NSTRT(NOP(N,2),1)=0
	      endif
	    endif
	  ENDDO
	endif
C-
C...... Define overriding boundary conditions based on input ITEQV
C-
cipk jun05   0=   vel+ dep + sal	  
cipk jun05   1=   vel+ dep	  
cipk jun05   2=   sal	  
cipk jun05   3=   vels+sal	  
cipk jun05   4=   vels	  
cipk jun05   5=   2d approx	  
cipk jun05   6=   vels+dep+temp	  
cipk jun05   7=   vels+dep+sed	  
cipk jun05   8=   temp	  
cipk jun05   9=   sed	  
cipk jun05  11=   vels+temp	  
cipk jun05  12=   vels+sed	  

cipk may02 initialize seventh option
C      do n=1,np
C	  nbc(n,7)=0
C	enddo
cipk jun05
      inovel=0
      if(iteqv(maxn) .eq. 2) inovel=1
      if(iteqv(maxn) .eq. 8)  inovel=2
      if(iteqv(maxn) .eq. 9) inovel=3
      IF(ITEQV(MAXN) .EQ. 0) THEN
cipk jun05      vel+ dep + sal	  
        DO 2205 N=1,NP
          NBC(N,5)=0
          NBC(N,6)=0
 2205   CONTINUE
      ELSEIF(ITEQV(MAXN) .EQ. 1) THEN
cipk jun05      vel+ dep	  
        DO 221 N=1,NP
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
  221   CONTINUE
      ELSEIF(ITEQV(MAXN) .EQ. 2) THEN
cipk jun05      sal	  
        DO 222 N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,5)=0
          NBC(N,6)=0
  222   CONTINUE
      ELSEIF(ITEQV(MAXN) .EQ. 3) THEN
cipk jun05      vels+sal	  
        DO 223 N=1,NP
          NBC(N,3)=0
          NBC(N,5)=0
          NBC(N,6)=0
  223   CONTINUE
      ELSEIF(ITEQV(MAXN) .EQ. 4) THEN
cipk jun05      vels	  
        DO 224 N=1,NP
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
  224   CONTINUE
      ELSEIF(ITEQV(MAXN) .EQ. 5) THEN
cipk jun05      2d approx	  
        NP=NPM
        NE=NEM
      ELSEIF(ITEQV(MAXN) .EQ. 6) THEN
cipk jun05      vels+dep+temp	  
        DO 225 N=1,NP
          NBC(N,4)=0
          NBC(N,6)=0
  225   CONTINUE      
      ELSEIF(ITEQV(MAXN) .EQ. 7) THEN
cipk jun05      vels+dep+sed	  
        DO 226 N=1,NP
          NBC(N,4)=0
          NBC(N,5)=0
  226   CONTINUE   
      ELSEIF(ITEQV(MAXN) .EQ. 8) THEN
cipk jun05      temp	  
        DO 227 N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,6)=0
  227   CONTINUE
      ELSEIF(ITEQV(MAXN) .EQ. 9) THEN
cipk jun05      sed	  
        DO 228 N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
  228   CONTINUE
cipk may02 revise for new 23 option ob BN line
      ELSEIF(ITEQV(MAXN) .EQ. 10) THEN
        DO N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
	    NBC(N,6)=0
C	    NBC(N,7)=1
        ENDDO
CC        NBC(1,7)=0

CIPK MAY02      ELSEIF(ITEQV(MAXN) .EQ. 10) THEN
      ELSEIF(ITEQV(MAXN) .EQ. 11) THEN
cipk jun05      vels+temp	  
        DO 2281 N=1,NP
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,6)=0
 2281   CONTINUE
CIPK MAY02      ELSEIF(ITEQV(MAXN) .EQ. 11) THEN
      ELSEIF(ITEQV(MAXN) .EQ. 12) THEN
cipk jun05      vels+sed	  
        DO 2282 N=1,NP
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
 2282   CONTINUE
      ENDIF


      do n=1,npm
	  if(icesw .gt. 0) then
	    if(icethk(n) .gt. 0  .or. 
     +     (vel(5,n) .lt. TMED .and. ITPAS .GT. 0)) then
	      if(ndep(n) .gt. 1) then
	        nbc(n,5)=0
              spec(n,5)=0.
	        vel(5,n)=TMED
              VDOT(5,n)=ALTM*(TMED-VOLD(5,n))-(ALPHA-1.)*VDOTO(5,n)
	        vdoto(5,n)=0.
	      endif
	    endif
	  endif
	enddo



C-
C...... Install special values for one-two dimension intersections
C-
      !EFa Nov06, Modifikation für 1D-Teschke-Elemente
      do nn=1,ne
      !nis,feb07: Allow for numbered FFF midsides
        if ((ncorn(nn).eq.5.or.ncorn(nn).eq.3).and.
      !+       nop(nn,2).EQ.-9999) then
     +       nop(nn,2) < -1000) then
      !-

          nbc(nop(nn,1),2)=0 !EFa Dec06
          nbc(nop(nn,3),2)=0 !EFa Dec06
          !EFa Mar07, estifm changed in coef1dfe, continuity equation is activated for steady state solution
          !if (icyc.le.0) then
          !  nbc(nop(nn,1),1)=0
          !  nbc(nop(nn,3),1)=0
          !end if
          !nis,feb07: nbc of negative node numbers is not defined, so the program overrides other space. In this case overrides the velocities and depths of several nodes
          !do i=1,4
          !  nbc(nop(nn,2),i)=0
          !end do
          !-
        end if
      end do


      DO 260 NN=1,NE
        IF(IMAT(NN) .EQ. 0) GO TO 260
cipk oct98 update to f90
        NTYP=NETYP(NN)
        !NiS,may06: NTYP.eq. 18 means 2D-3D-transition element???
        IF(NTYP .EQ. 18) THEN
          DO 230 ITM=1,3
            NNL=NOP(NN,ITM+4)
            NDX=NOP(NN,12-ITM)
            ND2=NOP(NN,15-ITM)
            IF(ITM .EQ. 1) ND3=NOP(NN,20)
            IF(ITM .EQ. 3) ND3=NOP(NN,19)
cipk may02 switch ndf to 7
            DO 229 J=1,7
              NBC(NDX,J)=-NNL
              NBC(ND2,J)=-NNL
              IF(ITM .NE. 2) NBC(ND3,J)=-NNL
  229       CONTINUE
  230     CONTINUE
        !NiS,may06: NCORN.eq.5 and IMAT.lt.901 means 1D-2D-transition element
        ELSEIF(NCORN(NN) .EQ. 5  .AND. IMAT(NN) .LT. 901) THEN
          NLN=NOP(NN,3)
          DO 235 K=4,5
            IF(NOP(NN,K) .EQ. 0) GO TO 240
            IF(NLN .GT. NOP(NN,K)) NLN=NOP(NN,K)
  235     CONTINUE
  240     CONTINUE
          DO 250 K=3,5
            IF(NLN .EQ. NOP(NN,K)) GO TO 250
            ND=NOP(NN,K)
            IF(ND .EQ. 0) GO TO 250
            NBC(ND,1)= -NLN
            NBC(ND,2)= -NLN
            NBC(ND,3)=-NLN
            NBC(ND,4)=-NLN
            NBC(ND,5)=-NLN
            NBC(ND,6)=-NLN
  250     CONTINUE
  
C
CIPK JUN05     test for totally submerged control structure
C              set equations to by-pass
C
        !NiS,may06: IMAT.gt.903 and IMAT.lt.990 means control structure transition
        ELSEIF(IMAT(NN) .GT. 903   .AND.  IMAT(NN) .LT. 990) THEN

cipk JUN05 first analyse 2-d structures
CIPK NOV06 MOVE UP
          IF(NCORN(NN) .EQ. 8) THEN
            SIDL1=DIST(NOP(NN,1),NOP(NN,3))
            SIDL2=DIST(NOP(NN,7),NOP(NN,5))
            NMID1=NOP(NN,2)
            NMID2=NOP(NN,6)
            IF(ELTON(NMID1,1) .NE. 0) THEN
              NAD1=ELTON(NMID1,1)
            ELSE
              NAD1=ELTON(NMID1,2)
            ENDIF            
            IF(ELTON(NMID2,1) .NE. 0) THEN
              NAD2=ELTON(NMID2,1)
            ELSE
              NAD2=ELTON(NMID2,2)
            ENDIF            
CIPK NOV06          IF(NCORN(NN) .EQ. 8) THEN

            DO K=1,3
              N1=NOP(NN,K)
              N2=NOP(NN,8-K)
              WV1=SQRT(VEL(1,N1)**2+VEL(2,N1)**2)
              WV2=SQRT(VEL(1,N2)**2+VEL(2,N2)**2)
              ITP=1
              IF(N1 .GT. 0) THEN
                IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
                  IWTYP=N1
                  widem=sqrt((cord(n2,2)-cord(n1,2))**2+
     +            (cord(n2,1)-cord(n1,1))**2)
CIPK FEB07
                  HL1=VEL(3,N1)
                  HL2=VEL(3,N2)
                  CALL WFORM(Q1,HL1,WSLL(N1),WV1,HL2
     +           ,WSLL(N2),WV2,WHGT(N1),WLEN(N1),ITP,IWTYP,widem)
                ELSE
	            CALL WTFORM(Q1,NCTREF(IMAT(NN)),WSLL(N1),WSLL(N2))
	          ENDIF

                IF(NTMREF(IMAT(NN)) .NE. 0) THEN
                  CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET
     +             ,QFACT)
CIPK DEC05 ADD QFACT
                  Q1=Q1*QFACT
	            IF(ISWTOF .EQ. 1) THEN
	              Q1=0.0
   	            ENDIF
	          ENDIF

                Q1C(K)=Q1

CIPK DEC05                if(inovel .gt. 0) then 
CIPK DEC05                  IF(ABS(Q1) .GT. 0) then
CIPK DEC05                    NBC(N2,INOVEL+3)=-N1
CIPK DEC05                  ENDIF
CIPK DEC05                endif

c       for submerged elements  set active equations

                IF(ISUBMEL(nn) .EQ. 1) THEN
                  N2=NOP(NN,8-K)
CIPK  TEST
                   IF((IBN(N1) .EQ. 20  .OR. IBN(N1) .EQ.  11)  .AND.
     +                 ISUBM(N1) .EQ. 1) THEN
                    if(inovel .eq. 0) then
                      NBC(N1,1)=1
                      NBC(N1,2)=1
                      NBC(N2,1)=1
                      NBC(N2,2)=1
	              endif
	              if(inovel .gt. 0) then 
                      IF(ABS(Q1) .GT. 0) then
	                  NBC(N2,INOVEL+3)=-N1
	                ENDIF
	              endif
                  ELSE
                    if(inovel .eq. 0) then
                      NBC(N1,1)=1
                      NBC(N2,1)=1
                    endif
                    NBC(N1,2)=0
                    NBC(N2,2)=0
                  ENDIF
                ENDIF
              ENDIF
            ENDDO

         QFLUX=SIDL1*(Q1C(1)+Q1C(2)*4.+Q1C(3))/6.
         IF(QFLUX .GT. 0) THEN
         FLUX=QFLUX*(VEL(3+INOVEL,NOP(NN,2))-VEL(3+INOVEL,NOP(NN,6)))
         ELSE
         FLUX=QFLUX*(VEL(3+INOVEL,NOP(NN,6))-VEL(3+INOVEL,NOP(NN,2)))
         ENDIF
         IF(QFLUX .GT. 0.) THEN
           EXTLDEL(NAD2)=EXTLDEL(NAD2)+FLUX/AREA(NAD1)
         ELSE
           EXTLDEL(NAD1)=EXTLDEL(NAD1)-FLUX/AREA(NAD1)
         ENDIF

C         WRITE(203,'(3I6,6F15.6)') MAXN,NN,3+INOVEL,
C     +    QFLUX,FLUX,EXTLDEL(NAD1) 
C     +   ,EXTLDEL(NAD2),VEL(3+INOVEL,NOP(NN,2)),VEL(3+INOVEL,NOP(NN,6)) 
c       now do middle elements

            N1=NOP(NN,4)
            N2=NOP(NN,8)
            NBC(N1,INOVEL+3)=-NOP(NN,3)
            NBC(N2,INOVEL+3)=-NOP(NN,1)
            IF(ISUBMEL(nn) .EQ. 1) THEN
              IF(IBN(NOP(NN,4)) .EQ. 20
     +           .AND.  ISUBM(NOP(NN,4)) .EQ. 1) THEN
                if(inovel .eq. 0) then
                  NBC(NOP(NN,4),1)=1
                  NBC(NOP(NN,4),2)=1
	          endif
              ELSE
                if(inovel .eq. 0) then
                  NBC(NOP(NN,4),1)=1
	          endif
                NBC(NOP(NN,4),2)=0
              ENDIF
            ELSE
              if(inovel .eq. 0) then
                NBC(NOP(NN,4),1)=1
              endif
            ENDIF          
            IF(ISUBMEL(nn) .EQ. 1) THEN
              IF(IBN(NOP(NN,8)) .EQ. 20
     +           .AND.  ISUBM(NOP(NN,8)) .EQ. 1) THEN
                if(inovel .eq. 0) then
                  NBC(NOP(NN,8),1)=1
                  NBC(NOP(NN,8),2)=1
                endif
              ELSE
                if(inovel .eq. 0) then
                  NBC(NOP(NN,8),1)=1
                  NBC(NOP(NN,8),2)=0
	          endif
              ENDIF
            ELSE
              if(inovel .eq. 0) then
                NBC(NOP(NN,8),1)=1
	        endif
            ENDIF
          ELSE

c       now do 1-d structures

      !NiS,may06,com: initialize equations for 1D-elements;
      !NiS,may06,com: get corner nodes
            N1=NOP(NN,1)
            N2=NOP(NN,3)
            NAD1=ELTON(N1,1)
            NAD2=ELTON(N2,1)
            IF(N1 .GT. 0) THEN

              IF(NCTREF(IMAT(NN)) .EQ. 0) THEN
                IWTYP=N1
CIPK FEB07
                  HL1=VEL(3,N1)
                  HL2=VEL(3,N2)
                CALL WFORM(Q1,HL1,WSLL(N1),WV1,HL2
     +           ,WSLL(N2),WV2,WHGT(N1),WLEN(N1),ITP,IWTYP,widem)
              ELSE
	          CALL WTFORM(Q1,NCTREF(IMAT(NN)),WSLL(N1),WSLL(N2))
	        ENDIF
              IF(NTMREF(IMAT(NN)) .NE. 0) THEN
                CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET
     +             ,QFACT)
CIPK DEC05 ADD QFACT
                Q1=Q1*QFACT
                IF(ISWTOF .EQ. 1) THEN
                  Q1=0.0
   	          ENDIF
	        ENDIF
              Q1=Q1*(WIDTH(N1)*VEL(3,N1)
     +           +(SS1(N1)+SS2(N1))*VEL(3,N1)*VEL(3,N1)/2.)

              IF(ISUBMEL(nn) .EQ. 1) THEN
                N2=NOP(NN,3)
                NBC(N1,1)=1
                NBC(N1,2)=0
                NBC(N2,1)=1
                NBC(N2,2)=0
              ENDIF
            ENDIF
            !NiS,may06,com: get midside node
            N1=NOP(NN,2)
            !EFa Nov06, die folgende nbc-Zuweisung nicht für 1D-Teschke-Elemente
            !nis,feb07: Allow for numbered FFF midsides
            !if (n1.ne.-9999) then
            if (n1 > -1000) THEN
            !-
            IF(ISUBMEL(nn) .EQ. 1) THEN
                NBC(N1,1)=1
                NBC(N1,2)=0
            ELSE
              NBC(N1,1)=1
            ENDIF          
            end if

            IF(INOVEL .GT. 0.) THEN
             N3=NOP(NN,2)
             NBC(N3,INOVEL+3)=0
             IF(Q1 .GT. 0) THEN
              FLUX=Q1*(VEL(3+INOVEL,NOP(NN,1))-VEL(3+INOVEL,NOP(NN,3)))
             ELSE
              FLUX=Q1*(VEL(3+INOVEL,NOP(NN,3))-VEL(3+INOVEL,NOP(NN,1)))
             ENDIF
             IF(Q1 .GT. 0.) THEN
              EXTLDEL(NAD2)=EXTLDEL(NAD2)+FLUX/AREA(NAD2)
             ELSE
              EXTLDEL(NAD1)=EXTLDEL(NAD1)-FLUX/AREA(NAD1)
             ENDIF
            ENDIF

          ENDIF
CIPK JUN05 FINISHED CHANGES
  
  
        !NiS,may06,com: NTYP.eq.17 or NTYP.eq.27 means 1D- or 2D-laterally averaged junction element
        ELSEIF(MOD(NTYP,10) .EQ. 7) THEN
C          WRITE(*,*) 'NN,NCORN(NN),NOP(NN,1)',NN,NCORN(NN),NOP(NN,1)
          NLN=NOP(NN,1)
CIPK JAN99          DO 251 K=2,NCORN(NN)
CIPK JAN99
          IF(NTYP .EQ. 17) THEN
            NCN=2
          ELSE
            NCN=NCORN(NN)
          ENDIF
          DO 251 K=2,NCN

            ND=NOP(NN,K)
            IF(ND .EQ. 0) GO TO 251
            NBC(ND,4)=-NLN
            NBC(ND,5)=-NLN
            NBC(ND,6)=-NLN
  251     CONTINUE
C          WRITE(*,*) 'ND,NLN,NBC(ND,4)',ND,NLN,NBC(ND,4)
        ENDIF
cipk oct98 update to f90
      IMMT=IMAT(NN)
      !NiS,may06,com: (Last two digits of IMAT).gt.90 means ctrl.strc.elts.
      IF(MOD(IMMT,100) .GT. 90) THEN
        IK=1
        NCN=NCORN(NN)
        IF(NCN .GT. 8) IK=2
        DO 255 M=1,NCN
          IF(ISHF(M,IK) .GT. 0) THEN
            I=ISHF(M,IK)
            I=NOP(NN,I)
            J=NOP(NN,M)
            DO 254 K=1,3
              NBC(J,K)=-I
  254       CONTINUE
          ENDIF
  255   CONTINUE
      ENDIF

      !nis,jan07: Add control for 1D-2D-line-transitions
      if (maxLT.ne.0) then
        !test for all possible transitions
        EquationAdjustment: do i=1,MaxLT

          !look, whether element is part of a transition, if not test next transition
          if (NN.ne.TransLines(i,1)) CYCLE EquationAdjustment

          !Get the Line Number and it's length as well as the connecting node number
          LiNo   = TransLines(i,2)
          LiLe   = LMT(LiNo)
          CoNode = TransLines(i,3)

          !initializing NLN; connectivity starts with first node of transitioning line
          NLN = Line(LiNo,1)

          !Get the smallest node number to reference equations to; 1st was already processed in lines before
          DO K=2,LiLe
            IF(NLN .GT. Line(LiNo,K)) NLN = Line(LiNo,K)
          ENDDO

          !testing, whether connecting node is part of line; transmember(i).eq.2 means seperate node
          if (transmember(i) .eq. 2) then
            !check, whether the number is less than referenced node number for coupling of DOFs
            if (NLN.gt.CoNode) NLN = CoNode
          end if

          !assign references to all nodes of the connection line
          equationassigning: DO K=1, LiLe

            !the same node must not be processed on
            IF(NLN .EQ. LINE(LiNo,k)) CYCLE equationassigning

            !Stop, if there is gap in line
            if (Line(LiNo,k) .eq. 0) then
              WRITE(*,*) 'ERROR - Definition gap in transition line.'
              WRITE(*,*) 'the ', k, 'th slot has a zero entry'
              WRITE(*,*) 'Have a look at your line definition'
              WRITE(*,*) 'Program can not be executed'
              WRITE(*,*) 'STOP'
              STOP
            endif

            !get copy of node number
            nd = line(LiNo,k)
            !assign equation references
            DO eqdof=1,6
              NBC(ND,eqdof)= -NLN
            ENDDO

          ENDDO equationassigning

          !Overgive to 1D-node, if this is a seperate transition node and if it is not the reference node
          if (TransMember(i).eq.2 .and. NLN.ne.CoNode) then
            DO eqdof=1,6
              NBC(CoNode,eqdof)= -NLN
            ENDDO
          end if

          !if element was found, jump out of Loop; Reason: This loop was only done for the 1D-transitioning element nn.
          EXIT EquationAdjustment
        ENDDO EquationAdjustment
      end if
      !-

  260 CONTINUE


C
C    Insert velocity distribution for line types with negative pointer 
C
CIPK JAN98 EXPAND TEST            8 AND 9
      IF(ITEQV(MAXN) .NE. 2  .AND.  ITEQV(MAXN) .NE. 8  
     +                       .AND.  ITEQV(MAXN) .NE. 9) THEN
      DO 263 K=1,IHGNN
        IF(JLIN(K) .GT. 0) THEN
          JL=JLIN(K)
          MAX=LMT(JL)
          MIDC=(MAX-1)/2
          IF(MOD(MIDC,2) .EQ. 0) MIDC=MIDC+1
          MID=LINE(JL,MIDC)
          DO 262 J=1,MAX
            NA=LINE(JL,J)
CIPK OCT98 CONVERT TO F90
            NFTYP=NFIX(NA)
            NRD=MOD(NFTYP,100)
            IF(NA .NE. MID) THEN
              NBC(NA,1)=-MID
              NBC(NA,2)=0
              NFIX(NA)=NRD
cipk feb98
              if(ndep(na) .gt. 0) then
                n=nref(na)+1
                l=nref(na)+ndep(na)-1
                do kk=n,l
                  nbc(kk,1)=-mid
                  nbc(kk,2)=0
                  nfix(kk)=nrd
                  vscale(kk)=vscale(na)
                enddo
              endif
cipk feb98
            ELSE
              NBC(MID,1)=1
              NBC(MID,2)=0
cipk feb98
              if(ndep(mid) .gt. 0) then
                n=nref(mid)+1
                l=nref(mid)+ndep(mid)-1
                do kk=n,l
                  nbc(kk,1)=-mid
                  nbc(kk,2)=0
                  nfix(kk)=nrd
                  vscale(kk)=vscale(mid)
                enddo
              endif
cipk feb98
            ENDIF
            IF(J .GT. 1  .AND.  J .LT. MAX) THEN
              NL=LINE(JL,J-1)
              NQ=LINE(JL,J+1)
              DX=CORD(NQ,1)-CORD(NL,1)
              DY=CORD(NQ,2)-CORD(NL,2)
              IF(DY .LT. 0.) THEN
                DX=-DX
                DY=-DY
              ENDIF
              ALFA(NA)=ATAN2(-DX,DY)
              IF (J .GT. 2) THEN
                IF(ALFA(NA)-ALFA(NL) .GT. 1.5708) THEN
                  ALFA(NA)=ALFA(NA)-3.14159
                ELSEIF(ALFA(NA)-ALFA(NL) .LT. -1.5708) THEN
                  ALFA(NA)=ALFA(NA)+3.14159
                ENDIF
              ENDIF
cipk feb98
              if(ndep(na) .gt. 0) then
                n=nref(na)+1
                l=nref(na)+ndep(na)-1
                do kk=n,l
                  alfa(kk)=alfa(na)
                enddo
              endif
cipk feb98
            ENDIF
  262     CONTINUE
          IF(MIDC .GT. 1) THEN
            MIDN=LINE(JL,MIDC-1)
CIPK OCT98 CONVERT TO F90
            NFTYP=NFIX(MIDN)
            NRD=MOD(NFTYP,100)
            NFIX(MIDN)=200+NRD
          ENDIF
          MIDP=LINE(JL,MIDC+1)
CIPK OCT98 CONVERT TO F90
          NFTYP=NFIX(MIDP)
          NRD=MOD(NFTYP,100)
          NFIX(MIDP)=200+NRD
C
C  Correct angles of the two sides
C
          J1=LINE(JL,1)
          J2=LINE(JL,2)
          IF(ALFA(J1)-ALFA(J2) .GT. 1.5708) THEN
            ALFA(J1)=ALFA(J1)-3.14159
          ELSEIF(ALFA(J1)-ALFA(J2) .LT. -1.5708) THEN
            ALFA(J1)=ALFA(J1)+3.14159
          ENDIF
          J1=LINE(JL,MAX)
          J2=LINE(JL,MAX-1)
          IF(ALFA(J1)-ALFA(J2) .GT. 1.5708) THEN
            ALFA(J1)=ALFA(J1)-3.14159
          ELSEIF(ALFA(J1)-ALFA(J2) .LT. -1.5708) THEN
            ALFA(J1)=ALFA(J1)+3.14159
          ENDIF
cipk feb98
          if(ndep(j1) .gt. 0) then
            n=nref(j1)+1
            l=nref(j1)+ndep(j1)-1
            do kk=n,l
              alfa(kk)=alfa(j1)
            enddo
          endif
          if(ndep(j2) .gt. 0) then
            n=nref(j2)+1
            l=nref(j2)+ndep(j2)-1
            do kk=n,l
              alfa(kk)=alfa(j2)
            enddo
          endif
cipk feb98
        ENDIF
  263 CONTINUE
CIPK JAN98
      ENDIF


C
C...... Prepare for elements that interface 2D to 3D
C
      IF(ITEQV(MAXN) .NE. 5) THEN
        DO 270 N=1,NP
          IF(NTHREE(N) .EQ. 1) GO TO 270
C
C...... This node has a 2-d definition
C
          !NiS,may06: if NREF(N) was zero, N is a 2D-node
          M=NREF(N)+1
          IF(M .EQ. 1) GO TO 270
C
C...... Now we know this node is also 3D
C
          IF(M .GT. NP) GO TO 270
          MT=M+NDEP(N)-2
C
C...... Wipe out all the lower equations
C
          DO 265 L=M,MT
            NBC(L,1)=0
            NBC(L,2)=0
            IF(NDF .GT. 3) THEN
              DO 264 KK=4,NDF
                NBC(L,KK)=0
  264         CONTINUE
            ENDIF
  265     CONTINUE
  270   CONTINUE
      ENDIF

CIPK JAN99 ADD FOR JUNCTIONS
C-
C...... Use junction to establish boundary values
C-
C-
      DO 285 N=1,NE
        IF(IMAT(N) .GT. 900  .AND.  IMAT(N) .LT. 1000) THEN
          N1=NOP(N,1)
          IF(NDEP(N1) .GT. 1  .OR.  N .GT. NEM) THEN
            N2=NOP(N,2)
            !EFa Nov06, die folgenden nbc-Zuweisungen nicht für1D-Teschke-Elemente
            !nis,feb07: Allow for numbered FFF midsides
            !if (n2.NE.-9999) then
            if (n2 > -1000) then
            !-
cipk may02 switch  to 7
            DO 282 K=1,7
cipk may02            DO 282 K=1,6
              NBC(N2,K)= -N1
  282       CONTINUE
            endif
            N3=NOP(N,3)
cuuu
              nbc(n3,4) = -n1
              NBC(N3,5) =-N1
              NBC(N3,6) =-N1
cipk may02
              NBC(N3,7) =-N1
 
            SPEC(N3,3)=VEL(3,N1) + AO(N1)-AO(N3)
C
C
            !EFa Nov06, folgende Berechnungen nicht für 1D-Teschke-ELemente
            !nis,feb07: Allow for numbered FFF midsides
            !if (n2.NE.-9999) then
            if (n2 > -1000) then
            !-
            WZ = WIDTH(N3) + (SS1(N3)+SS2(N3))*(CORD(N3,3)-AO(N3))
     &                    * (VEL(3,N3)/(ELEV-AO(N3)) ) 
C
            SIDN(N1) = 2.*WZ/(XTLN(N1)+XTLN(N2))
            SIDN(N2)=SIDN(N1)
            endif
            CXX=COS(ALFA(N3))
            SAA=SIN(ALFA(N3))
            VTB=-(VEL(1,N3)*CXX+VEL(2,N3)*SAA)*DIR(N3)
            write(76,*) 'load3',n1,n2,sidn(n1),vtb
cyyy            IF(VTB*SIDN(N1) .LT. 0.) THEN
cuuu            IF ( (VTB*SIDN(N1) .LT. 0.  .or.  iteqv(maxn) .ne. 2) ) THEN
cu              NBC(N3,4)=0  
cu              spec(n3,4)=vel(4,n1)
cu              vel(4,n3)=spec(n3,4)
cu            ENDIF
          ENDIF
        ENDIF
  285 CONTINUE


CIPK JAN99 END FOR JUNCTIONS

C-
C......INITIALIZE FACTOR FOR VERTICAL DISTRIBUTION
C-
      SCFC=6./(UMIN+1.+4.*(UMIN+(1.-UMIN)*0.5**PWERIN))
      DO 320 N=1,NPSAV
      FCTV(N)=1.
      FCTS(N)=1.
      UDST(N)=1.0
      UUDST(N)=1.0
      VVDST(N)=1.0
      VDST(N)=1.0
      SDST(N)=1.0
      TDST(N)=1.0
      SEDST(N)=1.0
  320 CONTINUE
C-
C...... Prepare for specified distribution of horizontal velocities
C-
      IF(ITEQV(MAXN) .NE. 5) THEN
        DO 350 N=1,NP
          IF(NTHREE(N) .EQ. 1) GO TO 330
C
C...... This node has a 2-d definition
C
          M=NREF(N)+1
          IF(M .EQ. 1) GO TO 330
C
C...... Now we know this node is also 3D
C
          IF(M .GT. NP) GO TO 330
C
C...... Set interface factor
C
          CNMIN = CINT(ICPON(N))
          PCERIN = CPOW(ICPON(N)) 
          SCFCC=6./(CNMIN+1.+4.*(CNMIN+(1.-CNMIN)*0.5**PCERIN))
          ZL=(CORD(N,3)-AO(N))/(ELEV-AO(N))
          FCTV(N)=SCFC*(UMIN+(1.-UMIN)*ZL**PWERIN)
          FCTS(N)=SCFCC*(CNMIN+(1.-CNMIN)*ZL**PCERIN)
          UDST(N)=FCTV(N)
          VDST(N)=FCTV(N)
          sdst(n) = fcts(n) 
          tdst(n) = fcts(n) 
          sedst(n) = fcts(n) 
          MT=M+NDEP(N)-2
          DO 325 L=M,MT
            NBC(L,1)=-N
            NBC(L,2)=-N
            IF(NDF .GT. 3) THEN
              NBC(L,4)=-N
              NBC(L,5)=-N
              NBC(L,6)=-N
cipk may02
              NBC(L,7)=-N
            ENDIF
            ZL=(CORD(L,3)-AO(L))/(ELEV-AO(L))
            IF(ZL .LT. -0.01) THEN
cipk sep04
              CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(75,*) 'Error in vertical coordinates at',L,ZL
              WRITE(*,*) 'Error in vertical coordinates at',L,ZL
              STOP
            ENDIF
            IF(ZL .GT. 0.) THEN
              FCTV(L)=SCFC*(UMIN+(1.-UMIN)*ZL**PWERIN)
              FCTS(L)=SCFCC*(CNMIN+(1.-CNMIN)*ZL**PCERIN)
            ELSE
              FCTV(L)=SCFC*UMIN
              FCTS(L)=SCFCC*CNMIN
            ENDIF
            UDST(L)=FCTV(L)
            VDST(L)=FCTV(L)
            IF(IENT .EQ. 1) THEN
              VEL(1,L)=VEL(1,N)*FCTV(L)/FCTV(N)
              VEL(2,L)=VEL(2,N)*FCTV(L)/FCTV(N)
              VEL(4,L)=VEL(4,N)*FCTS(L)/FCTS(N)
              VEL(5,L)=VEL(5,N)*FCTS(L)/FCTS(N)
              VEL(6,L)=VEL(6,N)*FCTS(L)/FCTS(N)
              VOLD(1,L)=VOLD(1,N)*FCTV(L)/FCTV(N)
              VOLD(2,L)=VOLD(2,N)*FCTV(L)/FCTV(N)
              VOLD(4,L)=VOLD(4,N)*FCTS(L)/FCTS(N)
              VOLD(5,L)=VOLD(5,N)*FCTS(L)/FCTS(N)
              VOLD(6,L)=VOLD(6,N)*FCTS(L)/FCTS(N)
              VDOT(1,L)=VDOT(1,N)*FCTV(L)/FCTV(N)
              VDOT(2,L)=VDOT(2,N)*FCTV(L)/FCTV(N)
              VDOT(4,L)=VDOT(4,N)*FCTS(L)/FCTS(N)
cipk dec98 reverse 5 and 6 to corect bug
              VDOT(5,L)=VDOT(5,N)*FCTS(L)/FCTS(N)
              VDOT(6,L)=VDOT(6,N)*FCTS(L)/FCTS(N)
              VDOTO(1,L)=VDOTO(1,N)*FCTV(L)/FCTV(N)
              VDOTO(2,L)=VDOTO(2,N)*FCTV(L)/FCTV(N)
              VDOTO(4,L)=VDOTO(4,N)*FCTS(L)/FCTS(N)
              VDOTO(5,L)=VDOTO(5,N)*FCTS(L)/FCTS(N)
              VDOTO(6,L)=VDOTO(6,N)*FCTS(L)/FCTS(N)
            ENDIF
  325     CONTINUE
  330     CONTINUE
          M=NSURF(N)
          IF(M .LT. 1) THEN
            NBC(N,3)=0
          ELSEIF(NBC(N,3) .EQ. -99) THEN
            NBC(N,3)=-M
          ENDIF
  350   CONTINUE
      ELSE
        CALL DISFCT
      ENDIF
C
C...... Dropout equations if possible
C
c     IF(MAXN .GT. 1   .AND.  ITEQV(MAXN) .EQ. ITEQV(MAXN-1)) THEN
c       DO 353 NN=1,NP
c         DO 352 M=1,NDF
c           IF(NDROP(NN,M) .EQ. 1) NBC(NN,M)=0
c 352     CONTINUE
c 353   CONTINUE
c     ENDIF
C
C...... Insert zero bottom velocity if requested
C
      IF(IZB .EQ. 1) THEN
        DO 297 J=1,NPM
          K=NREF(J)+1
          IF(K .EQ. 1) GO TO 297
          L=NREF(J)+NDEP(J)-1
          IF(L .LT. K) GO TO 297
          IF(NBC(L,1) .LT. 0) GO TO 297
          NBC(L,1)=0
          NBC(L,2)=0
  297   CONTINUE
      ENDIF
CIPK NOV98
C
C...... Insert zero surface velocity if requested
C
      IF(IZERS .EQ. 1) THEN
        DO J=1,NPM
          NBC(J,1)=0
          NBC(J,2)=0
        ENDDO
      ENDIF
CIPK NOV99
C
C...... Insert switching for equation removal when depth below TRANSIT       
C
      IF(ITRANSIT .EQ. 1) THEN
        DO J=1,NPM
          IF(ICOLLAP(J) .NE. 0) THEN
            K=NREF(J)+1
            L=NREF(J)+NDEP(J)-1
            DO M=K,L
              DO I=1,NDF
                if(i .ne. 3)  NBC(M,I)=-j
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C
CIPK NOV99 End addition
C
CIPK MAY02 Eliminate mid-sides
      IF(ICK .EQ. 7) THEN
	  do n=1,ne
	    ncn=ncorn(n)
	    if(imat(n) .lt. 1000  .or. imat(n) .gt. 5000) then
	      ilk=1
            IF(NCN .EQ. 15) ILK=2
            IF(NCN .EQ. 13) ILK=3
            IF(NCN .EQ. 10) ILK=2
            do k=1,ncn
	        if(im(k,ilk) .eq. 0) then
                NBC(NOP(N,K),7)=0
	        endif
	      enddo
	    endif
	  enddo
	ENDIF

!NiS,jun06,comment: At this point the equation numbers will be assigned to the NBC-array
CIPK DEC06  ALLOW FOR EQUAL FLOW AND DEPTH BC'S
    
      DO N=1,NQLDS
        IF(IQID(N) .EQ. 1) THEN
          J=NHYDQ(N)
          NMID=(LMT(J)+1)/2
          NODMID=LINE(J,NMID)
          DO K=1,LMT(J)
            IF(LINE(J,K) .NE. NODMID) THEN
              NBC(LINE(J,K),1)=-NODMID
                VEL(1,LINE(J,K))=VEL(1,NODMID)
                VEL(2,LINE(J,K))=VEL(2,NODMID)
                VOLD(1,LINE(J,K))=VOLD(1,NODMID)
                VOLD(2,LINE(J,K))=VOLD(2,NODMID)
              IF(MOD(K,2) .EQ. 1) THEN
                NBC(LINE(J,K),3)=-NODMID
                VEL(3,LINE(J,K))=VEL(3,NODMID)
                VOLD(3,LINE(J,K))=VOLD(3,NODMID)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C...... Rearrange array
C

      NSZF=0
      N = 0
      DO 300 NN = 1, NP
        N = N + 1
CIPK MAY02 SWITCH NDF TO 7
        DO 299 M = 1, 7
cycw aug96
cc          if(iactv(n,m) .eq. 0) nbc(n,m)=0  not active at present

          IF( NBC(N,M) .NE. 1 ) GO TO 299
          NSZF=NSZF+1
          NBC(N,M)=NSZF
  299   CONTINUE
  300 CONTINUE
C-
C...... Reduce from upto 6 column to the 4th column
C
      IF(ICK .GT. 4) THEN 
        DO 302 N=1,NP
          NBC(N,4)=NBC(N,ICK)
          NBC(N,ICK)=0
  302   CONTINUE
      ENDIF
C
C...... Force equation numbers for multi-node intersection
C-
      DO 3111 KKK=1,2
      DO 310 N=1,NP
CIPK MAY02 SWITCH NDF TO 7
        DO 305 M=1,7
          IF(NBC(N,M) .LT. 0) THEN
            NRF=-NBC(N,M)
              itmp = nbc(nrf,m)
              if (nrf .gt. n  .and. itmp .lt. 0)  goto 305
            NBC(N,M)=NBC(NRF,M)
          ENDIF
  305   CONTINUE
  310 CONTINUE
 3111     continue
ccycw added for full 3D optimisation 19/06/96 currently testing
cipk feb07 reduce test to make it entered only once
      if(ioptim .LT. 1) then
C         write(76,*) 'load ivrsid1',ivrsid
         call optim
         ioptim=1+IOPTIM
C         write(76,*) 'load ivrsid2',ivrsid
         WRITE(76,6020) (NFIXH(N),N=1,NE)
 6020    FORMAT('RT      ',9I8)
         DO N=1,NE
            NN=NFIXH(N)
            if (nn .ne. 0)  then
               NREORD(NN)=N
            endif
         enddo
      endif
cipk oct96  go to calculate DRODXIN
      call presr
      do n=1,ne
        nn=nelord(n)
cipk feb98
        if(nn .gt. 0) then
          if(ncorn(nn) .gt. 8) call roin(nn)
        endif
cipk feb98 end change
      enddo
cipk oct96 end changes

CIPK AUG05 NLSTEL MOVED TO FRONT

      IF(ITIMTH .EQ. 1) THEN
	  MR1=NSZF+1
	  IR1MAX=MR1
       ALLOCATE (IPOINT(MR1),NLSTEL(0:MR1),R1(0:MR1),LCS(MR1),
     + LPS(MR1),rkeep(0:mR1),ekeep(mR1),rkeepeq(mR1))
	  ITIMTH=2
cipk jun05
      ELSE
        IF(MR1 .LT. NSZF) THEN	
          DEALLOCATE  (IPOINT,R1,NLSTEL,RKEEP,LCS,LPS,EKEEP,RKEEPEQ)
	    MR1=NSZF+1
	    IR1MAX=MR1
          ALLOCATE (IPOINT(MR1),NLSTEL(0:MR1),R1(0:MR1),LCS(MR1),
     +    LPS(MR1),rkeep(0:mR1),ekeep(mR1),rkeepeq(mR1))
	  ENDIF
	ENDIF

C      DO N=1,NP
C        WRITE(75,'(8I5)') N,(NBC(N,M),M=1,7)
C      ENDDO
      WRITE(*,6007) NSZF
      WRITE(75,6007) NSZF
 6007 FORMAT( // 20X, '.....TOTAL NUMBER OF ACTIVE SYSTEM EQUATIONS =',
     1  I6 )
      IF( NSZF .LE. IR1MAX ) RETURN
cipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,6008) NSZF,IR1MAX
      WRITE(75,6008) NSZF,IR1MAX
 6008 FORMAT( / 10X, 'TOO MANY EQUATIONS..STOP CALLED..'/
     1 10X,'EQUATIONS =',I6,' ..MAX ALLOWED..',I6)
      STOP
      END
