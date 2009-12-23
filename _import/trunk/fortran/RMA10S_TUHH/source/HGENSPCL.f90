!ipk  last update feb 09 2001 allow for zero length cc lines
      SUBROUTINE HGENSPCL(IKEY,ILAYR,J,HREQ,QQAL)
      USE BLK10MOD
      SAVE
!-
!...... Generate specified head boundary conditions
!-
      DIMENSION QQAL(3)
!-
      ALLOCATABLE IHSPCL(:), HREQQ(:)
      DATA   IFIRST /  0 /
!
      DATA VOID/-1.E20/
!
      IF (IFIRST == 0)  THEN
         ALLOCATE (IHSPCL(MAXP), HREQQ(MAXP))
         IHSPCL = 0
          HREQQ = 0.0
         IFIRST = 1
      ENDIF
!
      IF (IKEY == 1)  GO TO 600
!
!
!-
!...... Insert values into SPEC  and  NFIX arrays
!
      MAX = LMT(J)
!IPK FEB01
      IF(MAX == 0) THEN
        WRITE(75,*) 'ATTEMPT TO SET ELEVATION FOR NON-EXISTENT LINE',J
        WRITE(75,*) 'EXECUTION TERMINATED'
        WRITE(*,*) 'ATTEMPT TO SET ELEVATION FOR NON-EXISTENT LINE',J
        WRITE(*,*) 'EXECUTION TERMINATED'
      ENDIF
!
      DO 300 K=1,MAX
         NA = LINE(J,K)
         SPEC(NA,3) = HREQ
         hreqq(na) = hreq
         IF (MOD(NFIX(NA),1000) < 200) THEN
            NFIX(NA) = NFIX(NA) + 200
         ENDIF
         IF (QQAL(1) >= 0.) THEN
            SPEC(NA,4) = QQAL(1)
            IF (MOD(NFIX(NA),100)/10 == 0) NFIX(NA) = NFIX(NA) + 10
         ENDIF
         IF (QQAL(2) >= 0.) THEN
            SPEC(NA,5) = QQAL(2)
            IF (MOD(NFIX(NA),10) == 0)  NFIX(NA) = NFIX(NA) + 1
         ENDIF
         IF (QQAL(3) >= 0.) THEN
            SPEC(NA,6) = QQAL(3)
            IF (NFIX1(NA) == 0)  NFIX1(NA) = 1
         ENDIF
!
!  2-dv element
!
         IF (MAX == 1)  THEN
            NDP = NDEP(NA)
            N1 = NREF(NA) + 1
            NV = NDEP(NA) + N1 - 2
            IF (N1 > 1)  THEN
               IHSPCL(NA) = 0
               DO I=N1,NV
                  IHSPCL(I) = 0
               ENDDO
!
               LDX = NREF(NA) + 1 + 2*(ILAYR-1) 
!
               IHSPCL(LDX) = 1 
               IHSPCL(NA) = -1
            ENDIF
!
         ELSE
!
!  3-d element
!
            DO KK=1,3
               ND = LINE(J,K-1+KK)
               NDP = NDEP(ND)
               N1 = NREF(ND) + 1
               NV = NDEP(ND) + N1 - 2
               IF (N1 > 1)  THEN
                  IHSPCL(ND) = 0
                  DO I=N1,NV
                     IHSPCL(I) = 0
                  ENDDO
!
                  IF (MOD(KK,2) /= 0)   THEN
                     LDX = NREF(ND) + 1 + 2*(ILAYR-1) 
                     IHSPCL(LDX) = 1 
                  ENDIF 
                  IHSPCL(ND) = -2
               ENDIF
            ENDDO
!
         ENDIF
!
         IF ( ICYC > 0) CALL BFORM(NA)
!
  300 CONTINUE
!
      RETURN
!
!
  600 CONTINUE
!
!  Assign flow to proper layer
!
      DO 610 N=1,NPM
         IF (IHSPCL(N) == -1 .OR. IHSPCL(N) == -2)  THEN
!
            NDP = NDEP(N)
            N1 = NREF(N) + 1
            NV = NDEP(NA) + N1 - 2
!
            IF (N1 > 1)  THEN
!
               NFIX(N) = 11200
               SPEC(N,1) = 0.0
               SPEC(N,2) = 0.0
               NSPL(N) = 1
               SPEC(N,3) = HREQQ(N) 
!
               DO 630 K=N1,NV
                  SPEC(K,3) = HREQQ(N) 
                  NSPL(K) = 1
                  IF (IHSPCL(K) == 1)  THEN
                     NFIX(K) = 1200
                  ELSE
                     SPEC(K,1) = 0.0
                     SPEC(K,2) = 0.0
                     NFIX(K) = 11200
!
                     IHSPCL(K) = 11
                  ENDIF
  630          CONTINUE
!
            ENDIF
!
         ENDIF
!
  610 CONTINUE
!
!
      END
