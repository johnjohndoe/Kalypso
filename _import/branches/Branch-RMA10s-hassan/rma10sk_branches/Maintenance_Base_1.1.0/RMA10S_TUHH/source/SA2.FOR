
      SUBROUTINE SA2(NCN)
      SAVE
*     FUNCTION SA2(K,I,L,NCN)
C
C     FUNCTION TO OBTAIN NODAL POINT VALUES
C
C     K IS SHAPE FUNCTION NUMBER
C     I IS NODAL POINT NUMBER
C     L IS SHAPE FUNCTION DESIRED
C                TYPE 1 = FUNCTION
C                TYPE 2 = X DERIVATIVE
C                TYPE 3 = Y DERIVATIVE
C     NCN IS NUMBER OF CORNER NODES
C
      INCLUDE 'BLKS.COM'
C
      REAL*8 DNX,DNY,SI,SL
      DIMENSION SI(6,2),SJ(6,6,3)
      DIMENSION SL(8,2),SM(8,8,3)
      DATA SI /0.,0.5,1.0,0.5,0.,0.,0.,0.,0.,0.5,1.0,0.5/
      DATA SL/
     A -1.,0.,1.0,1.0,1.0,0.,-1.,-1.,-1.,-1.,-1.,0.,1.,1.,1.,0./
      DATA IP1/0/,IP2/0/
      IF( NCN .EQ. 8 ) GO TO 200
      IF(IP1 .EQ. 0) THEN
        IP1=1
        DO 150 K=1,NCN
          DO 150 I=1,NCN
            SJ(K,I,2)=DNX(2,K,SI(I,1),SI(I,2))
            SJ(K,I,3)=DNY(2,K,SI(I,1),SI(I,2))
  150   CONTINUE
      ENDIF
      DO 180 K=1,NCN
        DO 180 I=1,NCN
          DA(K,I)=SJ(K,I,2)
          DB(K,I)=SJ(K,I,3)
  180 CONTINUE
      GO TO 400
  200 CONTINUE
      IF(IP2 .EQ. 0) THEN
        IP2=1
        DO 250 K=1,NCN
          DO 250 I=1,NCN
            SM(K,I,2)=DNX(1,K,SL(I,1),SL(I,2))
            SM(K,I,3)=DNY(1,K,SL(I,1),SL(I,2))
  250   CONTINUE
      ENDIF
      DO 280 K=1,NCN
        DO 280 I=1,NCN
          DA(K,I)=SM(K,I,2)
          DB(K,I)=SM(K,I,3)
  280 CONTINUE
C
C              CREATE LINEAR FUNCTIONS
C
  400 CONTINUE
      DO 500 I=1,NCN
        J=0
        DO 450 K=1,NCN,2
          J=J+1
          NA=K-1
          IF(K .EQ. 1) NA=NCN
          XMX(J,I)=XNX(K,I)+(XNX(NA,I)+XNX(K+1,I))*0.5
          CA(J,I)=DA(K,I)+(DA(NA,I)+DA(K+1,I))*0.5
          CB(J,I)=DB(K,I)+(DB(NA,I)+DB(K+1,I))*0.5
  450   CONTINUE
  500 CONTINUE
      RETURN
      END
