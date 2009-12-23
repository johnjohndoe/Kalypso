      FUNCTION TF(A)
      implicit none
!Declarations      
      real (kind = 8) :: TF
      real (kind = 4) :: A
      real (kind = 8) :: S, C
!
      S = SIN (A)
      C = COS (A)
!
      IF(C /= 0.) THEN
        TF = S/ C
      ELSEIF(S < 0.) THEN
        TF = -1.E20
      ELSE
        TF = 1.E20
      ENDIF
!
      RETURN
      END
