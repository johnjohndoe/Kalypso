!     Last change:  MD   20 Aug 2008    9:51 am
!     LAST UPDATE JAN 13 2008 modernize structure of subroutine and add for interpolated profiles in polynomial approach
!IPK  LAST UPDATE JAN 13 2003 ADD MOMENTUM
      SUBROUTINE LNFLO (N, SF_in, QQAL, NEST, LN, ELINX, ELINY)
!
      USE BLK10MOD,                                                     &
     &  only: sidf, sidq, einx, einy, imat, ncorn, tvol, tvolc,         &
     &        nedep, neref
      SAVE
!
!passed ones      
      INTEGER, intent (IN) :: N
      INTEGER, intent (INOUT) :: NEST
      INTEGER, INTENT (IN) :: LN
      REAL (kind = 8), INTENT (IN) :: SF_in
      real (kind = 8), INTENT (IN) :: QQAL(3)
      REAL (kind = 8), INTENT (IN) :: elinx, eliny
!
!locals      
      INTEGER :: K, L, M
      REAL (KIND = 8) :: SF
!
!
!nis,jan08: make a local copy
      SF = SF_in
!-
!...... Input layered inlfows
!-
!     Allocate to layers for 3-d
!                               
!      WRITE(75,*) 'LNFLO',N,SF,NEST,LN,TVOLC(N),TVOL(N)
!ipk mar01   test for global loading and modify
!if (nest < 2) go to 100      
!normally nest is 0 or 1, while calling a value is added to the input of the user for showing special cases      
      if (nest >= 2) then
!
        nest = nest - 2
!
!only if neither for elements marked in a special way nor for a special layer        
        if ( .NOT. ((nest + 2) == 3 .OR. ln /= 0)) then
!
!and only for 3D-applications          
          if ( .NOT. (imat (n) < 1000 .AND. ncorn (n) < 10)) THEN
!
!
            if (imat (n) / 1000 == 1) then
              SIDF (N) = SIDF(N) + SF
              SIDQ (N, 1) = QQAL (1)
              SIDQ (N, 2) = QQAL (2)
              SIDQ (N, 3) = QQAL (3)
!IPK JAN03  ADD MOMENTUM
              EINX (N) = ELINX
              EINY (N) = ELINY
            endif
!
            RETURN
          endif
        endif
      endif
!
!Scale total load to length/ area/ volume of element      
      IF (NEST /= 0) THEN
        IF (LN == 0) THEN
          SF = SF / TVOLC (N)
        ELSEIF (LN == 1) THEN
          SF = SF / TVOL (N)
        ELSE
          M = NEREF (N) + LN - 1
          SF = SF / TVOL (M)
        ENDIF
      ENDIF    
!
!apply to all layers      
      IF (LN == 0) THEN
        K = NEREF (N) + 1
        L = NEREF (N) + NEDEP (N) - 1
        SIDF (N) = SIDF (N) + SF
        SIDQ (N, 1) = QQAL (1)
        SIDQ (N, 2) = QQAL (2)
        SIDQ (N, 3) = QQAL (3)
!IPK JAN03  ADD MOMENTUM
        EINX (N) = ELINX
        EINY (N) = ELINY
!        WRITE(75,*) 'LNFLO-1',N,SIDF(N),SIDQ(N,1),SIDQ(N,2)
!
!in the case of 3D-elements; nedep shows the surface reference node in 3D applications        
        IF (NEDEP (N) > 1) THEN
          DO M = K, L
            SIDF (M) = SIDF (M) + SF
            SIDQ (M, 1) = QQAL (1)
            SIDQ (M, 2) = QQAL (2)
            SIDQ (M, 3) = QQAL (3)
!IPK JAN03  ADD MOMENTUM
            EINX (M) = ELINX
            EINY (M) = ELINY
!        WRITE(75,*) 'LNFLO-2',M,SIDF(M),SIDQ(M,1),SIDQ(M,2)
          ENDDO
        ENDIF
!
!apply only to the surface layer      
      ELSEIF (LN == 1) THEN
        SIDF (N) = SIDF(N)+SF
        SIDQ (N, 1) = QQAL (1)
        SIDQ (N, 2) = QQAL (2)
        SIDQ (N, 3) = QQAL (3)
!IPK JAN03  ADD MOMENTUM
        EINX (N) = ELINX
        EINY (N) = ELINY
!        WRITE(75,*) 'LNFLO-3',N,SIDF(N),SIDQ(N,1),SIDQ(N,2)
!
!apply to any other layer      
      ELSE
        M = NEREF (N) + LN - 1
        SIDF (M) = SIDF (M) + SF
        SIDQ (M, 1) = QQAL (1)
        SIDQ (M, 2) = QQAL (2)
        SIDQ (M, 3) = QQAL (3)
!IPK JAN03  ADD MOMENTUM
        EINX (M) = ELINX
        EINY (M) = ELINY
!        WRITE(75,*) 'LNFLO-4',M,SIDF(M),SIDQ(M,1),SIDQ(M,2)
      ENDIF
      RETURN
      END
