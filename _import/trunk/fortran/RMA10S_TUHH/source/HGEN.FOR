C     Last change:  WP   29 Aug 2007   11:37 am
cipk  last update feb 09 2001 allow for zero length cc lines
CIPK  LAST UPDATE AUG 1 2000 ADD SLOPING ELEVATION SET
CIPK  LAST UPDATE DEC 16 1997 
      SUBROUTINE HGEN(IBIN,J,HREQ,HREQ2,ISWTH,QQAL)
      USE BLK10MOD
      !nis,feb07,testing
      use paraflow1dfe
      !-
CIPK DEC97 calling sequence changed
      SAVE
C-
C...... Generate specified head boundary conditions
C-
cipk aug05      INCLUDE 'BLK10.COM'
      DIMENSION QQAL(3)
CIPK AUG00
      DISTS(N1,N2)=SQRT((CORD(N2,1)-CORD(N1,1))**2
     +                 +(CORD(N2,2)-CORD(N1,2))**2)
      !EFa jun07, changed dimension for autoconverge
      !,WTS(65)
      REAL,ALLOCATABLE :: wts(:)

      ALLOCATE (wts(nita))
      !-

CIPK DEC97 ADD TO DIMENSION
C-
C...... Insert values into SPEC  and  NFIX arrays
C-
cipk dec97
      jj=abs(j)
cipk dec97 change j to jj
      MAX=LMT(JJ)
CIPK FEB01
      IF(MAX .EQ. 0) THEN
        WRITE(75,*) 'ATTEMPT TO SET ELEVATION FOR NON-EXISTENT LINE',JJ
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET ELEVATION FOR NON-EXISTENT LINE',JJ
        WRITE(*,*) 'EXECUTION TERMINATED'
      ENDIF
CIPK AUG00
      IF(MAX .GT. 1) THEN
        NST=LINE(JJ,1)
        NFN=LINE(JJ,MAX)
        CONLEN=DISTS(NST,NFN)
      ENDIF
      DO 300 K=1,MAX
cipk dec97 change j to jj
        NA=LINE(JJ,K)
CIPK AUG00
        IF(ISWTH .EQ. 0  .OR.  MAX .EQ. 1) THEN
          SPEC(NA,3)=HREQ
        ELSE
          FACT=DISTS(NA,NST)/CONLEN
          SPEC(NA,3)=HREQ+FACT*(HREQ2-HREQ)
        ENDIF
        IF(MOD(NFIX(NA),1000) .LT. 200) THEN
          NFIX(NA)=NFIX(NA)+200
        ENDIF
CIPK AUG00  END CHANGES
        IFIX=NFIX(NA)/100
	  NFIX(NA)=IFIX*100
	  NFIX1(NA)=0
        IF(QQAL(1) .GE. 0.) THEN
          SPEC(NA,4)=QQAL(1)
          IF(MOD(NFIX(NA),100)/10 .EQ. 0) NFIX(NA)=NFIX(NA)+10
	  ELSEIF(QQAL(1) .GT. -9000.) THEN
          SPEC(NA,4)=QQAL(1)
	    NFIX(NA)=NFIX(NA)+20
        ENDIF
        IF(QQAL(2) .GE. 0.) THEN
          SPEC(NA,5)=QQAL(2)
          IF(MOD(NFIX(NA),10) .EQ. 0) NFIX(NA)=NFIX(NA)+1
	  ELSEIF(QQAL(2) .GT. -9000.) THEN
          SPEC(NA,5)=QQAL(2)
	    NFIX(NA)=NFIX(NA)+2
        ENDIF
        IF(QQAL(3) .GE. 0.) THEN
          SPEC(NA,6)=QQAL(3)
          IF(NFIX1(NA) .EQ. 0) NFIX1(NA)=1
	  ELSEIF(QQAL(3) .GT. -9000.) THEN
          SPEC(NA,6)=QQAL(3)
	    NFIX1(NA)=NFIX1(NA)+2
        ENDIF
        IF(ICYC .GT. 0) CALL BFORM(NA)
  300 CONTINUE

      !nis,feb07,testing
      !WRITE(*,*) 'Setze h-Randbedingung'
      !WRITE(*,*) 'Knoten: ', na
      !WRITE(*,*) spec(na,3)
      !WRITE(*,*) nfix(na), nfix1(na)
      !pause
      !-
c
cipk dec97 add logic for HCN
c

      N1=1
      N9=9
  305 CONTINUE
      IF(ID(1:3) .EQ. 'HCN') THEN
        IF(MAX .LT. N9) N9=MAX
        READ(DLIN,5010) (WTS(I),I=N1,N9)
 5010   FORMAT(9F8.0)
        call ginpt(ibin,id,dlin)
cipk jan96 save data to a scratch file
        if(isvs .eq. 1) then
          write(nscrin,7000) id,dlin
        endif
 7000   FORMAT(A8,A72)
        NDATLN=NDATLN+1
        IF (MAX .GT. N9) THEN
          N1=N1+9
          N9=N9+9
          GO TO 305
        ENDIF
        J=-JJ
        DO 400 K=1,MAX
          NN=LINE(JJ,K)
          VSCALE(NN)=WTS(K)
          IF(VSCALE(NN) .EQ. 1.0) VSCALE(NN)=0.0
  400   CONTINUE         
      ENDIF
cipk dec97 end changes
      !EFa jun07
      deallocate (wts)
      !-
      RETURN
      END
