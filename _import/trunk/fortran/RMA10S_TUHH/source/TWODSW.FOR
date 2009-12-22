C     Last change:  WP   22 Jul 2008    5:05 pm
cipk  last update sep 19 2003 switch units for col.dat
CIPK  LAST UPDATE JAN 15 2002 ALLOW FOR FLOWS INTO COLLAPSING ELEMENTS
C     New routine November 12 1999

      SUBROUTINE TWODSW
      USE BLKHMOD
      USE BLK10MOD
      USE BLKDRMOD
      !USE blkecom
     
C
C     Routine to detect threed elements below threshold for wetting/drying
C
C-
C     Work through all the surface nodes      
C-
      DO N=1,NPM
C
C     Test for 3-D
C
        IF(NDEP(N) .GT. 1) THEN

C
C     Establish threshold if it is not defined
C
        IF(TRANSIT .EQ. 0) THEN
          TRANSITT= ADT(N)-(ADB(N)+ADT(N))/2.*(1.-AKP(N))
C          if(n .eq. 1) write(75,*) ' transit=',transitt
        ELSE
          TRANSITT=TRANSIT
        ENDIF
C
C     Is depth below the threshold
C
          IF(VEL(3,N) .LT. TRANSITT) THEN
C
C     Test whether collapse already active, if so skip by
C
            IF(ICOLLAP(N) .EQ. 1) GO TO 300
C
C     We have a candidate to set collapse switch and integrate the velocities
C
            CALL AVEPARAM(N,1)
          ELSE
            K=NREF(N)
            L=NREF(N)+NDEP(N)-1
            DO M=K,L
              IF(M .EQ. K) THEN
                MM=N
              ELSE
                MM=M
              ENDIF
              ICOLLAP(MM)=0
            ENDDO
          ENDIF
        ENDIF
  300   CONTINUE
      ENDDO
C
C     Now set up elements to be collapsed 
C
      DO N=1,NE
CIPK JAN02  INITIALIZE SIDFF
        SIDFF(N)=0.
	  IF(SIDF(N) .NE. 0.) THEN
	    WRITE(75,*) 'N,SIDF(N),TVOL(N)',N,SIDF(N),TVOL(N)
	  ENDIF
C
C     Intialize   ICOLLAPE=1 says this element is connected 
C     to node with depth below TRANSIT
C
        ICOLLAPE(N)=0
        IF(IMAT(N) .GT. 0) THEN
          NCN=NCORN(N)
C
C     Loop on the nodes looking for  all depths below TRANSIT
C
CIPK APR01 ADD BETTER TEST FOR SIDE ELEMENTS
          ICNT=0
          IF(NETYP(N) .EQ. 13  .OR.  NETYP(N) .EQ. 14) THEN
	      IF(ICOLLAPE(NRELSF(N)) .EQ. 1) THEN
	        ICOLLAPE(N)=1
	      ENDIF
	    ELSE
            DO M=1,NCN
              NN=ABS(NSURF(NOP(N,M)))
              IF(ICOLLAP(NN) .EQ. 1) THEN
                ICNT=ICNT+1
              ENDIF
            ENDDO 
crrr MAY00          IF(ICNT .GT. 0) THEN        
c if already depth averaged, skip
            IF (ICNT .GT. 0      .and. 
     &         (netyp(n) .ne. 15 .and. netyp(n) .ne. 16  .and.
     &          netyp(n) .ne. 17 .and. netyp(n) .ne. 6   .and.
     &          netyp(n) .ne. 7  .and. netyp(n) .ne. 8) ) THEN

              ICOLLAPE(N)=1

cipk jan02  Look for side flows to translate to surface element

              IF(IMAT(N)/1000 .EQ. 1) THEN
	          NNS=NRELSF(N)
                K=NEREF(NNS)+1
                L=NEREF(NNS)+NEDEP(NNS)-1
	          IF(SIDF(NNS) .NE. 0.) THEN
                  SIDFF(N)=SIDFF(N)+SIDF(NNS)*TVOL(NNS)
                  SIDQ(N,1)=SIDQ(NNS,1)
                  SIDQ(N,2)=SIDQ(NNS,2)
                  SIDQ(N,3)=SIDQ(NNS,3)
	          ENDIF
                IF(NEDEP(NNS) .GT. 1) THEN
                  DO M=K,L
	              IF(SIDF(M) .NE. 0.) THEN
                      SIDFF(N)=SIDFF(N)+SIDF(M)*TVOL(M)
                      SIDQ(M,1)=SIDQ(M,1)
                      SIDQ(M,2)=SIDQ(M,2)
                      SIDQ(M,3)=SIDQ(M,3)
	              ENDIF
                  ENDDO
                ENDIF
                SIDFF(N)=SIDFF(N)/TVOLC(NNS)
	          IF(SIDFF(N) .NE. 0.) THEN
			    WRITE(75,*) 'N,SIDFF',N,NNS,SIDFF(N),TVOLC(NNS)
	          ENDIF
	        ENDIF
              IF(IMAT(N) .LT. 1000  .OR.  IMAT(N) .GT. 5000) THEN
                DO M=1,NCN
              
                  IF(ICOLLAP(NOP(N,M)) .EQ. 0) THEN
                    IF(NOP(N,M) .LE. NPM) THEN
                      NN=NOP(N,M)
                      CALL AVEPARAM(NN,2)
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
	    ENDIF
        ENDIF

      ENDDO
cipk sep 03 revise write unit
      write(78,*) 'Collapsed surface nodes for time and iteration',tet,
     +         maxn
      do n=1,npm
        if(icollap(n) .gt. 0) then
cipk sep 03 revise write unit
          write(78,*) n      
        endif
      enddo
cipk sep 03 revise write unit
      write(78,*) 'Collapsed elements are'
      do n=1,ne
        if(icollape(n) .gt. 0) then
cipk sep 03 revise write unit
          write(78,*) n
        endif
      enddo
      RETURN
      END

      SUBROUTINE AVEPARAM(N,ICSW)
      USE BLK10MOD

      real*8 vintx,vinty,pints,pintt,pintu,DEPSUM,
     +       voldx,voldy,polds,poldt,poldu,vdotx,vdoty,pdots,pdott,pdott,
     +	   pdotu,vdotox,vdotoy,pdotos,pdotot,pdotou,dep1
      DEPSUM=0.
      VINTX=0.
      VINTY=0.
      PINTS=0.
      PINTT=0.
      PINTU=0.
      VOLDX=0.
      VOLDY=0.
      POLDS=0.
      POLDT=0.
      POLDU=0.
      VDOTX=0.
      VDOTY=0.
      PDOTS=0.
      PDOTT=0.
      PDOTU=0.
      VDOTOX=0.
      VDOTOY=0.
      PDOTOS=0.
      PDOTOT=0.
      PDOTOU=0.
      K=NREF(N)
      L=NREF(N)+NDEP(N)-2
c      write(78,*) n,nsurf(n),k,l,depscl,depsclo
      IF(NSURF(N) .LT. 0) THEN
        LL=1
      ELSE
        LL=2
        L=L-1
      ENDIF
      DO M=K,L,LL
        IF(M .EQ. K) THEN
          MM=N
        ELSE
          MM=M
        ENDIF
        M1=M+1
        M2=M+2
C
C     Check for mod-side nodes
C
        IF(NSURF(N) .LT. 0) THEN
C
C     We have a mid-side
C
          DEP1=(CORD(MM,3)-CORD(M1,3))
          DEPSUM=DEPSUM+DEP1
          VINTX=VINTX+(VEL(1,MM)+VEL(1,M1))/2.*DEP1
          VINTY=VINTY+(VEL(2,MM)+VEL(2,M1))/2.*DEP1
          PINTS=PINTS+(VEL(4,MM)+VEL(4,M1))/2.*DEP1
          PINTT=PINTT+(VEL(5,MM)+VEL(5,M1))/2.*DEP1
          PINTU=PINTU+(VEL(6,MM)+VEL(6,M1))/2.*DEP1
          VOLDX=VOLDX+(VOLD(1,MM)+VOLD(1,M1))/2.*DEP1
          VOLDY=VOLDY+(VOLD(2,MM)+VOLD(2,M1))/2.*DEP1
          POLDS=POLDS+(VOLD(4,MM)+VOLD(4,M1))/2.*DEP1
          POLDT=POLDT+(VOLD(5,MM)+VOLD(5,M1))/2.*DEP1
          POLDU=POLDU+(VOLD(6,MM)+VOLD(6,M1))/2.*DEP1
          VDOTX=VDOTX+(VDOT(1,MM)+VDOT(1,M1))/2.*DEP1
          VDOTY=VDOTY+(VDOT(2,MM)+VDOT(2,M1))/2.*DEP1
          PDOTS=PDOTS+(VDOT(4,MM)+VDOT(4,M1))/2.*DEP1
          PDOTT=PDOTT+(VDOT(5,MM)+VDOT(5,M1))/2.*DEP1
          PDOTU=PDOTU+(VDOT(6,MM)+VDOT(6,M1))/2.*DEP1
          VDOTOX=VDOTOX+(VDOTO(1,MM)+VDOTO(1,M1))/2.*DEP1
          VDOTOY=VDOTOY+(VDOTO(2,MM)+VDOTO(2,M1))/2.*DEP1
          PDOTOS=PDOTOS+(VDOTO(4,MM)+VDOTO(4,M1))/2.*DEP1
          PDOTOT=PDOTOT+(VDOTO(5,MM)+VDOTO(5,M1))/2.*DEP1
          PDOTOU=PDOTOU+(VDOTO(6,MM)+VDOTO(6,M1))/2.*DEP1
        ELSE
C
C     We have a corner
C
          DEP1=(CORD(MM,3)-CORD(M2,3))
          DEPSUM=DEPSUM+DEP1
c          write(78,*) 'aveparm',mm,m1,m2,vel(4,mm),vel(4,m1),vel(4,m2),
c     +		vel(3,mm),vel(3,m2),dep1
          VINTX=VINTX+(VEL(1,MM)+4.*VEL(1,M1)+VEL(1,M2))/6.*DEP1
          VINTY=VINTY+(VEL(2,MM)+4.*VEL(2,M1)+VEL(2,M2))/6.*DEP1
          PINTS=PINTS+(VEL(4,MM)+4.*VEL(4,M1)+VEL(4,M2))/6.*DEP1
          PINTT=PINTT+(VEL(5,MM)+4.*VEL(5,M1)+VEL(5,M2))/6.*DEP1
          PINTU=PINTU+(VEL(6,MM)+4.*VEL(6,M1)+VEL(6,M2))/6.*DEP1
          VOLDX=VOLDX+
     +        (VOLD(1,MM)+4.*VOLD(1,M1)+VOLD(1,M2))/6.*DEP1
          VOLDY=VOLDY+
     +        (VOLD(2,MM)+4.*VOLD(2,M1)+VOLD(2,M2))/6.*DEP1
          POLDS=POLDS+
     +        (VOLD(4,MM)+4.*VOLD(4,M1)+VOLD(4,M2))/6.*DEP1
          POLDT=POLDT+
     +        (VOLD(5,MM)+4.*VOLD(5,M1)+VOLD(5,M2))/6.*DEP1
          POLDU=POLDU+
     +        (VOLD(6,MM)+4.*VOLD(6,M1)+VOLD(6,M2))/6.*DEP1
          VDOTX=VDOTX+
     +        (VDOT(1,MM)+4.*VDOT(1,M1)+VDOT(1,M2))/6.*DEP1
          VDOTY=VDOTY+
     +        (VDOT(2,MM)+4.*VDOT(2,M1)+VDOT(2,M2))/6.*DEP1
          PDOTS=PDOTS+
     +        (VDOT(4,MM)+4.*VDOT(4,M1)+VDOT(4,M2))/6.*DEP1
          PDOTT=PDOTT+
     +        (VDOT(5,MM)+4.*VDOT(5,M1)+VDOT(5,M2))/6.*DEP1
          PDOTU=PDOTU+
     +        (VDOT(6,MM)+4.*VDOT(6,M1)+VDOT(6,M2))/6.*DEP1
          VDOTOX=VDOTOX+
     +        (VDOTO(1,MM)+4.*VDOTO(1,M1)+VDOTO(1,M2))/6.*DEP1
          VDOTOY=VDOTOY+
     +        (VDOTO(2,MM)+4.*VDOTO(2,M1)+VDOTO(2,M2))/6.*DEP1
          PDOTOS=PDOTOS+
     +        (VDOTO(4,MM)+4.*VDOTO(4,M1)+VDOTO(4,M2))/6.*DEP1
          PDOTOT=PDOTOT+
     +        (VDOTO(5,MM)+4.*VDOTO(5,M1)+VDOTO(5,M2))/6.*DEP1
          PDOTOU=PDOTOU+
     +        (VDOTO(6,MM)+4.*VDOTO(6,M1)+VDOTO(6,M2))/6.*DEP1
        ENDIF
      ENDDO
      K=NREF(N)
      L=NREF(N)+NDEP(N)-1
      DO M=K,L
        IF(M .EQ. K) THEN
          MM=N
        ELSE
          MM=M
        ENDIF
        VEL(1,MM)=VINTX/DEPSUM
        VEL(2,MM)=VINTY/DEPSUM
        VEL(4,MM)=PINTS/DEPSUM
        VEL(5,MM)=PINTT/DEPSUM
        VEL(6,MM)=PINTU/DEPSUM
        VOLD(1,MM)=VOLDX/DEPSUM
        VOLD(2,MM)=VOLDY/DEPSUM
        VOLD(4,MM)=POLDS/DEPSUM
        VOLD(5,MM)=POLDT/DEPSUM
        VOLD(6,MM)=POLDU/DEPSUM
        VDOT(1,MM)=VDOTX/DEPSUM
        VDOT(2,MM)=VDOTY/DEPSUM
        VDOT(4,MM)=PDOTS/DEPSUM
        VDOT(5,MM)=PDOTT/DEPSUM
        VDOT(6,MM)=PDOTU/DEPSUM
        VDOTO(1,MM)=VDOTOX/DEPSUM
        VDOTO(2,MM)=VDOTOY/DEPSUM
        VDOTO(4,MM)=PDOTOS/DEPSUM
        VDOTO(5,MM)=PDOTOT/DEPSUM
        VDOTO(6,MM)=PDOTOU/DEPSUM
        ICOLLAP(MM)=ICSW
c          write(78,*) 'aveparm-2',mm,vel(4,mm),vel(3,mm)

      ENDDO

      RETURN
      END