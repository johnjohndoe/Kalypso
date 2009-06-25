
      FUNCTION TF(A)
      
      real (kind = 4) :: A
      
      S=SIN(A)
      C=COS(A)
      IF(C .NE. 0.) THEN
        TF=S/C
      ELSEIF(S .LT. 0.) THEN
        TF=-1.E20
      ELSE
        TF=1.E20
      ENDIF
      RETURN
      END
