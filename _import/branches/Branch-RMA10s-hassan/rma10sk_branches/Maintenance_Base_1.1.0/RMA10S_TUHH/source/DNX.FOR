
      FUNCTION DNX (IT, K, X, Y)
      SAVE
C-
      !Declarations
      !Function type defintion
      real (kind = 8) :: dnx
      
      !variables
      REAL(kind = 8) ::X, Y
      integer :: it, k
     



C......FUNCTION TO DETERMINE X-DERIVATIVE OF SHAPE FUNCTION
C-
      IF(IT.EQ.1) GO TO 500
C-
C......TRIANGULAR ELEMENT
C-
      GO TO (110,120,130,140,150,160),K
  110 DNX=-3.D0 +4.D0*X+4.D0*Y
      RETURN
  120 DNX=4.D0-8.D0*X-4.D0*Y
      RETURN
  130 DNX=4.D0*X-1.D0
      RETURN
  140 DNX=4.D0*Y
      RETURN
  150 DNX=0.D0
      RETURN
  160 DNX=-4.D0*Y
      RETURN
C-
C......QUADRILATERAL ELEMENT
C-
  500 CONTINUE
      GO TO (510,520,530,540,550,560,570,580),K
  510 DNX=-(1.D0-Y)*(-2.D0*X-Y)/4.D0
      RETURN
  520 DNX=-X*(1.D0-Y)
      RETURN
  530 DNX=(1.D0-Y)*(2.D0*X-Y)/4.D0
      RETURN
  540 DNX=(1.D0-Y*Y)/2.D0
      RETURN
  550 DNX=(1.D0+Y)*(2.D0*X+Y)/4.D0
      RETURN
  560 DNX=-X*(1.D0+Y)
      RETURN
  570 DNX=-(1.D0+Y)*(-2.D0*X+Y)/4.D0
      RETURN
  580 DNX=-(1.D0-Y*Y)/2.D0
      RETURN
      END
