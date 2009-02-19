
      REAL*8 FUNCTION XN2(IT,K,X,Y)
      SAVE
C-
      !definitions
      real (kind = 8) :: x, y
      integer (kind = 4) :: k
      integer (kind = 4) :: it
      !it     type of element
      !       = 1 => quadrilateral element
      !      /= 1 => triangular element
      !k      ordinal number of the node in the master element to get the function value for
      !x, y   xi and eta for as local coordinates to get the function values for
      
C......FUNCTION TO DEFINE SHAPE FUNCTION VALUES
C-
      IF(IT.EQ.1) GO TO 500
C-
C......TRIANGULAR ELEMENT
C-
      GO  TO ( 110,120,130,140,150,160),K
  110 XN2=(1.D0-2.D0*X-2.D0*Y)*(1.D0-X-Y)
      GOTO 600
  120 XN2=4.D0*X*(1.D0-X-Y)
      GOTO 600
  130 XN2=(2.D0*X-1.D0)*X
      GOTO 600
  140 XN2=4.D0*X*Y
      GOTO 600
  150 XN2=(2.D0*Y-1.D0)*Y
      GOTO 600
  160 XN2=4.D0*Y*(1.D0-X-Y)
      GOTO 600
C-
C......QUADRILATERAL ELEMENT
C-
  500 CONTINUE
      GO TO (510,520,530,540,550,560,570,580),K
  510 XN2=(1.D0-X)*(1.D0-Y)*(-X-Y-1.D0)/4.D0
      GOTO 600
  520 XN2=(1.D0-X*X)*(1.D0-Y)/2.D0
      GOTO 600
  530 XN2=(1.D0+X)*(1.D0-Y)*(X-Y-1.D0)/4.D0
      GOTO 600
  540 XN2=(1.D0+X)*(1.D0-Y*Y)/2.D0
      GOTO 600
  550 XN2=(1.D0+X)*(1.D0+Y)*(X+Y-1.D0)/4.D0
      GOTO 600
  560 XN2=(1.D0-X*X)*(1.D0+Y)/2.D0
      GOTO 600
  570 XN2=(1.D0-X)*(1.D0+Y)*(-X+Y-1.D0)/4.D0
      GOTO 600
  580 XN2=(1.D0-X)*(1.D0-Y*Y)/2.D0
600   RETURN
      END
