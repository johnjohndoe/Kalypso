CIPK  LAST UPDATE APRIL 23 2001 ALLOW VERTICAL LINES TO STAY IN
cipk  last update Mar 21 2001 move logic into DRPOUT
      SUBROUTINE DRPOUT
      USE BLK10
      USE BLK10MOD
C-
cipk aug05      INCLUDE 'BLK10.COM'
      ALLOCATABLE IFAIL(:,:)
	DIMENSION IACCES(9),ITRAN(9)
      DATA NLOOP/3/
	DATA IACCES/0,0,0,0,0,0,0,0,0/
	DATA ITRAN/1,2,0,0,0,0,0,3,4/
	DATA ITIMEH/0/

cipk mar01 extensive changes to allow for second IDRPT option

c     initialize IACTV 

      IF(ITIMEH .EQ. 0) THEN
        ALLOCATE (IFAIL(MAXP,4))
        IFAIL=0
        ITIMEH=1
      ENDIF
      DO J=1,NP
        DO K=1,NDF
          IACTV(J,K)=0
        ENDDO
      ENDDO

c  Determine option used  set IACTV and return

      IF(IDRPT .EQ. 1) THEN
CIPK AUG05
	  IF(MAXN .EQ. 1) THEN
          DO J=1,NP
            DO K=1,NDF
              IACTV(J,K)=10
            ENDDO
          ENDDO
	    RETURN
        ELSE
          IF(ITEQV(MAXN) .NE. ITEQV(MAXN-1)) THEN
            DO J=1,NP
              DO K=1,NDF
                IACTV(J,K)=10
              ENDDO
            ENDDO
	      RETURN
	    ENDIF
C-
C.....Make list of still active equations
C-

          DO K=1,NDF
            CONVT=CONV(K)*DRFACT
            DO J=1,NP
              IF(NBCKP(J,K) .NE. 0) THEN
                IF(ABS(RKEEP(NBCKP(J,K))) .GT. CONVT) IACTV(J,K)=10
              ENDIF
            ENDDO
          ENDDO

	  ENDIF
	ELSE

c     for first iteration skip out

	  IF(MAXN .EQ. 1) THEN
	    DO J=1,9
	      IACCES(J)=0
	    ENDDO
          
	    IACCES(ITEQV(MAXN))=1
          DO J=1,NP
	      DO K=1,4
	        IFAIL(J,K)=0
	      ENDDO
            DO K=1,NDF
              IACTV(J,K)=10
            ENDDO
          ENDDO
	    RETURN
	  ENDIF

c     check to see if we have changed iteration type

	  IF(ITEQV(MAXN) .NE. ITEQV(MAXN-1)) THEN

c     now check if this is first access for this iteration type

	    IF(IACCES(ITEQV(MAXN)) .EQ. 0) THEN
            DO J=1,NP
              DO K=1,NDF
                IACTV(J,K)=10
              ENDDO
            ENDDO
	      IACCES(ITEQV(MAXN))=1
	      RETURN
	    ELSE

C-
C.....Make list of still active equations for same type
C-

            IATP=ITRAN(ITEQV(MAXN-1))
            DO K=1,NDF
              CONVT=CONV(K)*DRFACT
              DO J=1,NP
                IF(NBCKP(J,K) .NE. 0) THEN
                  IF(ABS(RKEEP(NBCKP(J,K))) .GT. CONVT) THEN
	              IFAIL(J,IATP)=1
	            ENDIF
                ENDIF
              ENDDO
            ENDDO



c     check to see which nodes have failed to converge recently

		  DO M=1,4
              DO J=1,NP

  	          IF(IFAIL(J,M) .EQ. 1) THEN
	            DO K=1,NDF
	              IACTV(J,K)=10
	            ENDDO
                ENDIF

	        ENDDO
            ENDDO

c     reset ifail for current type

            IATP=ITRAN(ITEQV(MAXN))
            DO J=1,NP
	        IFAIL(J,IATP)=0
	      ENDDO
	    ENDIF
        ELSE

C-
C.....Make list of still active equations for same type
C-

          IATP=ITRAN(ITEQV(MAXN))
          DO K=1,NDF
            CONVT=CONV(K)*DRFACT
            DO J=1,NP
              IF(NBCKP(J,K) .NE. 0) THEN
                IF(ABS(RKEEP(NBCKP(J,K))) .GT. CONVT) THEN
			    IACTV(J,K)=10
	            IFAIL(J,IATP)=1
	          ENDIF
              ENDIF
            ENDDO
          ENDDO

	  ENDIF

	ENDIF


      DO LOOP=1,NLOOP
        DO N=1,NE
          IF(IMAT(N) .GT. 0) THEN
            NCN=NCORN(N)
            DO M=1,NCN
              DO K=1,NDF
                IF(IACTV(NOP(N,M),K) .EQ. 10) THEN
                  DO MM=1,NCN
                    DO KK=1,NDF
                      IF(IACTV(NOP(N,MM),KK) .EQ. 0) THEN
                        IACTV(NOP(N,MM),KK)=1
                      ENDIF
                    ENDDO
                  ENDDO
                  GO TO 300
                ENDIF
              ENDDO
            ENDDO
          ENDIF
  300     CONTINUE
        ENDDO
        DO J=1,NP
          DO K=1,NDF
            IF(IACTV(J,K) .EQ. 1) IACTV(J,K)=10
          ENDDO
        ENDDO
      ENDDO
      ISUM=0
cipk apr01   Change to force nodes that are in vertical line to be active when doing velocities
      DO J=1,NPM
     	  IF(NDEP(J) .GT. 1) THEN
          K=NREF(J)
CIPK APR01  SKIP OUT FOR MISSING NODES
	    IF(K .EQ. 0) GO TO 350
          L=NREF(J)+NDEP(J)-1
	    DO M=K,L
            IF(M .EQ. K) THEN
	        MM=J
	      ELSE
	        MM=M
	      ENDIF
            IF(ITEQV(MAXN) .EQ. 1  .OR.  ITEQV(MAXN) .EQ. 0  .OR.
     +         ITEQV(MAXN) .EQ. 6  .OR.  ITEQV(MAXN) .EQ. 7) THEN
              IF(IACTV(J,3) .EQ. 10  .OR. IACTV(L,3) .EQ. 10) THEN
	          IACTV(MM,3)=10
	        ENDIF
            ENDIF
            IF(IACTV(MM,1) .GT. 0) ISUM=ISUM+1
	    ENDDO
	  ELSE
	    MM=J
          IF(IACTV(J,1) .GT. 0) ISUM=ISUM+1
	  ENDIF

cipk sep94        DO K=1,NDF
cipk sep94        ENDDO
  350 CONTINUE

      ENDDO
      WRITE(75,*) 'NEW NUMBER OF ACTIVE NODES =',ISUM
      WRITE(*,*) 'NEW NUMBER OF ACTIVE NODES =',ISUM
      RETURN
      END
