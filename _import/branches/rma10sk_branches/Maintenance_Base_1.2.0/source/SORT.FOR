      SUBROUTINE SORTAO
!*********************************** .....SORT.....                     
!-                                                                      
!......SORT IS A SIMPLE SHELL SORT ROUTINE                              
!-                                                                      
!     SHELL SORT                                                        
      USE BLK10MOD
      USE BLKSANMOD
cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05	INCLUDE 'BLKSAND.COM'
!                                                                       
      IF(NP.LT.2) RETURN 
      DO 90 J=1,NP
        NKEY1(J)=J 
   90 END DO 
      ID1 = NP
  100 ID1 = ID1 / 2 
  110 IB = 1 
  120 GO TO 200 
  130 IB = IB + 1 
      IF( IB .LE. ID1 ) GO TO 200 
      IF( ID1 .GT. 1 ) GO TO 100 
      RETURN 
  200 I = IB 
  210 K = I + ID1 
  220 IF( AO(NKEY1(I)) .LE. AO(NKEY1(K)) ) GO TO 250 
      NKT = NKEY1(K) 
      NKEY1(K) = NKEY1(I) 
      J = I 
  230 K = J - ID1 
      IF( K .LT. 1 ) GO TO 240 
      IF( AO(NKT) .GT. AO(NKEY1(K)) ) GO TO 240 
      NKEY1(J) = NKEY1(K) 
      J = K 
      GO TO 230 
  240 NKEY1(J) = NKT 
  250 I = I + ID1 
      IF( I + ID1 .LE. NP ) GO TO 210 
      GO TO 130 
      END                                           
