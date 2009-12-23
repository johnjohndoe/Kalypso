!     Last change:  WP   29 Apr 2008    3:45 pm
!
      SUBROUTINE ARAA (NN)
      USE BLKHMOD
      USE BLK10MOD
      SAVE
!
!-
      DIMENSION NCON (8), XL (3), YL (3), DNX (3)
!-
!-.....ASSIGN PROPER COEFS.....
!-
!
!AREA (NN) : plan view area of an element NN, i.e. Length in 1D-case and area in 2D-case      
!
!initialize the area      
      AREA (NN) = 0.
      IF (ITEQV (MAXN) == 5) THEN
        DO N = 1, 8
          NCON (N) = NOPS (NN, N)
          IF (NCON (N) /= 0) NCN = N
        ENDDO
      ELSE
        NCN = NCORN (NN)
        DO N = 1, NCN
          NCON (N) = NOP (NN, N)
        ENDDO
      ENDIF
      IF (NCN == 5 .AND. IMAT (NN) /= 901) NCN = 3
!
!-
!- INITIALIZE MATRICES AND VARIABLES
!-
      CXX = COS (TH (NN))
      SAA = SIN (TH (NN))
      NGP = 4
!
!-
!-.....COMPUTE LOCAL CORDS.....
!-
      NR = NCON (1)
      DO K = 1, NCN
        N      = NCON (K)
        DX     = CORD (N, 1) - CORD (NR, 1)
        DY     = CORD (N, 2) - CORD (NR, 2)
        XL (K) = DX * CXX + DY * SAA
        YL (K) = - DX * SAA + DY * CXX
      ENDDO
!
!-
!-.....COMPUTE ELEMENT EQUATIONS.....
!-
      DO I = 1, NGP
        TEMP      = (DNAL (2, I) * XL (2) + DNAL (3, I) * XL (3))
        DNX (2)   = (4. - 8. * AFACT (I))/ TEMP
        DNX (3)   = (4. * AFACT (I) - 1.) / TEMP
        DYDX      = YL (2) * DNX (2) + YL (3) * DNX (3)
        ALF       = ATAN (DYDX)
        CSALF     = COS (ALF)
        TEMP      = TEMP / CSALF
        AMW       = ABS (TEMP) * HFACT (I) / 2.
        AREA (NN) = AREA (NN) + AMW
!-
!...... END GAUSS DO LOOP
!-
      ENDDO
!
      RETURN
      END
