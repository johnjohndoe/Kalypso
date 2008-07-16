CIPK  LAST UPDATE JAN 13 2003 ADD MOMENTUM
      SUBROUTINE LNFLO(N,SF,QQAL,NEST,LN,ELINX,ELINY)
      USE BLK10MOD
      USE BLK11MOD
      SAVE
C-
C...... Input layered inlfows
C-
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
      DIMENSION QQAL(3)
C
C
C     Allocate to layers for 3-d
C                               
C      WRITE(75,*) 'LNFLO',N,SF,NEST,LN,TVOLC(N),TVOL(N)
cipk mar01   test for global loading and modify
      if(nest .lt. 2) go to 100
      if(nest .eq. 3  .or. ln .ne. 0) then
	  nest=nest-2
	  go to 100
      endif
	nest=nest-2
      if(imat(n) .lt. 1000) then
	  if(ncorn(n) .lt. 10) then
	    go to 100
	  endif
	endif
	if(imat(n)/1000 .eq. 1) then
        SIDF(N)=SIDF(N)+SF
        SIDQ(N,1)=QQAL(1)
        SIDQ(N,2)=QQAL(2)
        SIDQ(N,3)=QQAL(3)
CIPK JAN03  ADD MOMENTUM
	  EINX(N)=ELINX
	  EINY(N)=ELINY
      endif
      RETURN
	
  100 continue
      IF(NEST .NE. 0) THEN
        IF(LN .EQ. 0) THEN
          SF=SF/TVOLC(N)
        ELSEIF(LN .EQ. 1) THEN
          SF=SF/TVOL(N)
        ELSE
          M=NEREF(N)+LN-1
          SF=SF/TVOL(M)
        ENDIF
      ENDIF    
      IF(LN .EQ. 0) THEN
        K=NEREF(N)+1
        L=NEREF(N)+NEDEP(N)-1
        SIDF(N)=SIDF(N)+SF
        SIDQ(N,1)=QQAL(1)
        SIDQ(N,2)=QQAL(2)
        SIDQ(N,3)=QQAL(3)
CIPK JAN03  ADD MOMENTUM
	  EINX(N)=ELINX
	  EINY(N)=ELINY
c        WRITE(75,*) 'LNFLO-1',N,SIDF(N),SIDQ(N,1),SIDQ(N,2)
        IF(NEDEP(N) .GT. 1) THEN
          DO M=K,L
            SIDF(M)=SIDF(M)+SF
            SIDQ(M,1)=QQAL(1)
            SIDQ(M,2)=QQAL(2)
            SIDQ(M,3)=QQAL(3)
CIPK JAN03  ADD MOMENTUM
	      EINX(M)=ELINX
	      EINY(M)=ELINY
c        WRITE(75,*) 'LNFLO-2',M,SIDF(M),SIDQ(M,1),SIDQ(M,2)
          ENDDO
        ENDIF
      ELSEIF(LN .EQ. 1) THEN
        SIDF(N)=SIDF(N)+SF
        SIDQ(N,1)=QQAL(1)
        SIDQ(N,2)=QQAL(2)
        SIDQ(N,3)=QQAL(3)
CIPK JAN03  ADD MOMENTUM
	  EINX(N)=ELINX
	  EINY(N)=ELINY
c        WRITE(75,*) 'LNFLO-3',N,SIDF(N),SIDQ(N,1),SIDQ(N,2)
      ELSE
        M=NEREF(N)+LN-1
        SIDF(M)=SIDF(M)+SF
        SIDQ(M,1)=QQAL(1)
        SIDQ(M,2)=QQAL(2)
        SIDQ(M,3)=QQAL(3)
CIPK JAN03  ADD MOMENTUM
	  EINX(M)=ELINX
	  EINY(M)=ELINY
c        WRITE(75,*) 'LNFLO-4',M,SIDF(M),SIDQ(M,1),SIDQ(M,2)
      ENDIF
      RETURN
      END
