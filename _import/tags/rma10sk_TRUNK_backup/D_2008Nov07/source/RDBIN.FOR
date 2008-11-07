cipk  last update sep 7 2005 add initial call
      SUBROUTINE RDBIN(N,M,K)
      USE BLK10MOD

cipk aug05      INCLUDE 'BLK10.COM'

C     Read GFGEN banners

      READ(ISMSGN) MFLG,IREC,N,M
cipk sep05
      if(K .EQ. 1) RETURN
      READ(ISMSGN) IWRT1,(IDUM,I=1,IWRT1)
      READ(ISMSGN) IWRT2,IWRT3,(IDUM,I=1,IWRT2),(FDUM,I=1,IWRT3)
      READ(ISMSGN) IWRT4,(IDUM,I=1,IWRT4)

	READ(ISMSGN) N,M,((CORDS(J,K),K=1,2),ALFA(J),WSS(J),J=1,N),
     +                 ((NOP(J,K),K=1,8),IMATL(J),TH(J),NFIXH(J),J=1,M)
      READ(ISMSGN) (WIDTH(J),SS1(J),SS2(J),WIDS(J),J=1,N)

	DO J=1,N
	  AO(J)=WSS(J)
	  WSS(J)=0.
	  DO K=1,2
	    CORD(J,K)=CORDS(J,K)
	  ENDDO
	ENDDO

	DO J=1,M
	  IMAT(J)=IMATL(J)
	ENDDO
      RETURN
      END
