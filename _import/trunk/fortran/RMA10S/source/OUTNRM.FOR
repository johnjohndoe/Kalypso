CIPK  LAST UPDATE AUGUST 1 2002 ADD REAL*8
      SUBROUTINE OUTNRM(X1,Y1,X2,Y2,X3,Y3,S1,NTP)
      SAVE
      REAL*8 X1,Y1,X2,Y2,X3,Y3
C-
C...... A routine designed to return the outward normal along a line
C-
      DIMENSION SV(3),S1(3)
      DATA SV/0.02,0.52,0.98/
      IF(NTP .EQ. 1) THEN
        DO 100 J=1,3
        S = SV(J)
        XN1 = 1.0-3.0*S+2.0*S**2
        XN2 = 4.0*S*(1.0-S)
        XN3 = S*(2.0*S-1.0)
        X= XN1*X1 + XN2*X2 + XN3*X3
        Y = XN1*Y1 + XN2*Y2 + XN3*Y3
        IF(J .EQ. 1) THEN
          DY=X1-X
          DX=Y-Y1
        ELSEIF(J .EQ. 2) THEN
          DY=X2-X
          DX=Y-Y2
        ELSE
          DY=X-X3
          DX=Y3-Y
        ENDIF
        S1(J)=ATAN2(DY,DX)
  100   CONTINUE
      ELSE
        DO 200 J=1,3
        S = SV(J)
        XN1 = 1.0-3.0*S+2.0*S**2
        XN2 = 4.0*S*(1.0-S)
        XN3 = S*(2.0*S-1.0)
        X= XN1*X1 + XN2*X2 + XN3*X3
        Y = XN1*Y1 + XN2*Y2 + XN3*Y3
        IF(J .EQ. 1) THEN
          DY=Y1-Y
          DX=X1-X
        ELSEIF(J .EQ. 2) THEN
          DY=Y2-Y
          DX=X2-X
        ELSE
          DY=Y3-Y
          DX=X3-X
        ENDIF
        S1(J)=ATAN2(DY,DX)
  200   CONTINUE
      ENDIF
      RETURN
      END
