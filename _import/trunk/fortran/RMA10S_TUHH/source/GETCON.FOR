C     Last change:  WP   25 Oct 2007    9:00 am
      SUBROUTINE GETCON
      USE BLK10MOD
      USE BLKSANMOD

C     Routine to get elements connected to nodes and allocate areas

      DO N=1,NE
        IF (IMAT(N) .LT. 900  .OR. IMAT(N) .GT. 999) then
          NCN=NCORN(N)
	    DO M=1,NCN
            DO I=1,12
            IF(ELTON(NOP(N,M),I) .EQ. N) THEN
	        GO TO 250
	      ENDIF
		    IF(ELTON(NOP(N,M),I) .EQ. 0) THEN
	          ELTON(NOP(N,M),I) = N
	          GO TO 250
	        ENDIF
	      ENDDO
  	      WRITE(75,*) 
     +      'STOP!  TOO MANY ELEMENT CONNECTED TO NODE',NOP(N,M)
            WRITE(75,*) (ELTON(NOP(N,M),I),I=1,12)
            WRITE(*,*)
     +      'STOP!  TOO MANY ELEMENT CONNECTED TO NODE',NOP(N,M)
	      STOP
  250       CONTINUE
          ENDDO
	ENDIF
      ENDDO

C     Next assign areas based on straight side assumption

      DO N=1,NP
        TRIBAREA(N)=0.
	  DO I=1,12
	    M=ELTON(N,I)
	    IF(M .GT. 0) THEN
	      IF(NCORN(M) .EQ. 8) THEN
	        FACT=4.
	      ELSE 
              FACT=3.
	      ENDIF
	      TRIBAREA(N)=TRIBAREA(N)+AREA(M)/FACT
	    ENDIF
	  ENDDO
	ENDDO

C     Finally work up average manning 'n' at nodes

      DO N=1,NP
        DIVIDE=0.
        DO J=1,12
	    neadj=elton(n,j)
          if(neadj .ne. 0) then
              xnmann(n)=xnmann(n)+zmann(neadj)
              divide=divide+1.
          endif
        enddo
        IF(DIVIDE .GT. 0.) THEN
              xnmann(n)=xnmann(n)/divide
        ENDIF
      enddo


	RETURN
	END