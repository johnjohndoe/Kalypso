C     Last change:  MD   20 Aug 2008   10:11 am
cipk  last update feb 09 2001 allow for zero length cc lines
CIPK  LAST UPDATE AUG 1 2000 ADD SLOPING ELEVATION SET
CIPK  LAST UPDATE DEC 16 1997 
      SUBROUTINE HGEN(IBIN,J,HREQ,HREQ2,ISWTH,QQAL)
      USE BLK10MOD
CIPK DEC97 calling sequence changed
      SAVE
C-
C...... Generate specified head boundary conditions
C-
cipk aug05      INCLUDE 'BLK10.COM'
      DIMENSION QQAL(3)
CIPK AUG00
      !function definition to calculate the distance between two nodes N1 and N2
      DISTS(N1,N2)=SQRT((CORD(N2,1)-CORD(N1,1))**2
     +                 +(CORD(N2,2)-CORD(N1,2))**2)

      !EFa jun07, changed dimension for autoconverge
      !,WTS(65)
      REAL,ALLOCATABLE :: wts(:)
      ALLOCATE (wts(nita))

CIPK DEC97 ADD TO DIMENSION
C-
C...... Insert values into SPEC  and  NFIX arrays
C-
cipk dec97
      !number and length of continuity line
      jj = abs (j)
      MAX = LMT (JJ)

      !check for continuity line of zero-length
      IF(MAX .EQ. 0) THEN
        WRITE(75,*) 'ATTEMPT TO SET ELEVATION FOR NON-EXISTENT LINE',JJ
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET ELEVATION FOR NON-EXISTENT LINE',JJ
        WRITE(*,*) 'EXECUTION TERMINATED'
      ENDIF

      !2D boundary: Get first and last definition node and calculate direct distance in between
      IF (MAX > 1) THEN
        NST = LINE (JJ, 1)
        NFN = LINE (JJ, MAX)
        CONLEN = DISTS (NST,NFN)
      ENDIF

      !run through all nodes 
      DO 300 K = 1, MAX
        !get current node
        NA = LINE (JJ, K)
        
        !for the application of direct values without involing the interpolation of values between
        !first node and final node or for 1D applications directly apply the boundary conditions
        IF (ISWTH == 0 .OR. MAX == 1) THEN
          SPEC (NA, 3) = HREQ
        !interpolate water stage boundary conditions for nodes in boundary lines with a varying water
        !level distribution
        elseif (iswth == 1 .or. iswth == 2) then
          FACT = DISTS (NA, NST)/ CONLEN
          SPEC (NA, 3) = HREQ + FACT * (HREQ2 - HREQ)
        !do nothing for volume-waterlevel boundary conditions
        elseif (iswth == 3) then
          
        endif

        !Add boundary condition switch for water level BCs
        IF (MOD (NFIX (NA), 1000) < 200) THEN
          NFIX (NA) = NFIX (NA) + 200
        ENDIF

        NFIX1 (NA) = 0
        !constituent boundary conditions
        !salinity
        IF(QQAL(1) >= 0.) THEN
          SPEC (NA, 4) = QQAL (1)
          IF (MOD (NFIX (NA), 100)/ 10 == 0) NFIX (NA) = NFIX (NA) + 10
        ELSEIF (QQAL (1) > -9000.) THEN
          SPEC (NA, 4) = QQAL (1)
          NFIX (NA) = NFIX (NA) + 20
        ENDIF
        !temperature
        IF (QQAL (2) >= 0.) THEN
          SPEC (NA, 5) = QQAL (2)
          IF (MOD (NFIX (NA), 10) == 0) NFIX (NA) = NFIX (NA) + 1
        ELSEIF (QQAL (2) > -9000.) THEN
          SPEC (NA, 5) = QQAL (2)
          NFIX (NA) = NFIX (NA) + 2
        ENDIF
        !concentrations
        IF (QQAL (3) >= 0.) THEN
          SPEC (NA, 6) = QQAL (3)
          IF (NFIX1 (NA) == 0) NFIX1 (NA) = 1
        ELSEIF (QQAL (3) > -9000.) THEN
          SPEC (NA, 6) = QQAL (3)
          NFIX1 (NA) = NFIX1 (NA) + 2
        ENDIF
        !form boundary conditions for node na, means fix the direction slopes
        IF (ICYC > 0) CALL BFORM(NA)
  300 CONTINUE

c
cipk dec97 add logic for HCN
c

      N1=1
      N9=9
  305 CONTINUE
      !This is for the distribution of the velocities at water level boundary conditions
      IF (ID (1:3) == 'HCN') THEN
        IF (MAX < N9) N9 = MAX
        READ (DLIN, 5010) (WTS (I), I = N1, N9)
 5010   FORMAT (9F8.0)
        call ginpt(ibin,id,dlin)
cipk jan96 save data to a scratch file
        if(isvs .eq. 1) then
          write(nscrin,7000) id,dlin
        endif
 7000   FORMAT(A8,A72)
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
