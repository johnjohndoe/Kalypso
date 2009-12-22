
      SUBROUTINE SB(NCN,NGP)
      USE SACMOD
      USE WTPRMOD
      SAVE
C-
      DIMENSION SI(64,3),SL(64,3)
      REAL SJ(20,64,4),SM(15,64,4)
      DATA SI/-2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,
     1        -2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,
     1        -2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,
     1        -2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,-2.,-1.,1.,2.,
     2 4*-2.,4*-1.,4*1.,4*2.,4*-2.,4*-1.,4*1.,4*2.,
     2 4*-2.,4*-1.,4*1.,4*2.,4*-2.,4*-1.,4*1.,4*2.,
     3 16*2.,16*1.,16*-1.,16*-2./
      DATA ISJ/0/
      DATA ISM/0/
      IF(NCN .EQ. 15) GO TO 200
C-
C......PROCESS CUBE
C-
      IF(ISJ.GT.0) GO TO 100
      ISJ=1
      DO 80 I=1,NGP
      IF(ABS(SI(I,1)).EQ.1.) X=SI(I,1)*0.3399810436
      IF(ABS(SI(I,2)).EQ.1.) Y=SI(I,2)*0.3399810436
      IF(ABS(SI(I,3)).EQ.1.) Z=SI(I,3)*0.3399810436
      IF(ABS(SI(I,1)).EQ.2.) X=SI(I,1)*0.8611363116/2.
      IF(ABS(SI(I,2)).EQ.2.) Y=SI(I,2)*0.8611363116/2.
      IF(ABS(SI(I,3)).EQ.2.) Z=SI(I,3)*0.8611363116/2.
      DO 70 K=1,NCN
      DO 70 L=1,4
      SBS=XN3(3,K,X,Y,Z,L)
      SJ(K,I,L)=SBS
   70 CONTINUE
   80 CONTINUE
  100 DO 120 I=1,NGP
      DO 120 K=1,NCN
      XNX(K,I)=SJ(K,I,1)
      DA(K,I)=SJ(K,I,2)
      DB(K,I)=SJ(K,I,3)
      DC(K,I)=SJ(K,I,4)
  120 CONTINUE
      RETURN
C-
C......PROCESS TRIANGULAR PRISM
C-
  200 IF( ISM .EQ. 0 ) GO TO 250
  210 DO 220 I=1,NGP
      DO 220 K=1,NCN
      XNX(K,I)=SM(K,I,1)
      DA(K,I)=SM(K,I,2)
      DB(K,I)=SM(K,I,3)
      DC(K,I)=SM(K,I,4)
  220 CONTINUE
      RETURN
  250 CONTINUE
      ISM=1
      CALL GPT(SL,WAITPH)
      DO 280 L=1,4
      DO 275 I=1,NGP
      DO 270 K=1,NCN
      SM(K,I,L) = XN3(2,K,SL(I,1),SL(I,2),SL(I,3),L)
  270 CONTINUE
  275 CONTINUE
  280 CONTINUE
      GO TO 210
      END
