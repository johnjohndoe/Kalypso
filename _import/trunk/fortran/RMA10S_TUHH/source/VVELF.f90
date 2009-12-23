CIPK  LAST UPDATE JAN 14 2002 ALLOW FOR DIVID =0 CASE
      SUBROUTINE VVELF
      USE BLK10
      USE BLK10MOD
      SAVE
C-
C...... SUBROUTINE FOR VERTICAL VELOCITY GENERATION FROM R1
C-
      DO 400 N=1,NPM
      K=NREF(N)+1
      IF(K == 1) GO TO 400
      L=K+NDEP(N)-2

CIPK JAN02  CARCH CASE OF UNDEFINED DIVID

      IF(DIVID(L) > 0) THEN
        VVEL(L)=-(VEL(1,L )*FC(L,1)/FC(L,3)+VEL(2,L )*FC(L,2)/FC(L,3))
        VVEL(N)=(VEL(1,N)*DDHDX(L)+VEL(2,N)*DDHDY(L))/DIVID(L)+VDOT(3,N)
      ELSE
        VVEL(L)=0.
        VVEL(N)=0.
      ENDIF
      IF(L < K) GO TO 400
      DO 350 M=K,L
      NN=NBC(M,1)


cipk sep98      IF(NN == 0) GO TO 350
      IF(NN /= 0) THEN
CC        ZPO=(CORD(M,3)-AO(M))/(ELEV-AO(M))
CC        VVEL(M)=R1(NN)+VVEL(L)+ZPO*(VVEL(N) -VVEL(L))
CC      ELSEIF(M /= L) THEN
        ZPO=(CORD(M,3)-AO(M))/(ELEV-AO(M))
cipk jan99        
C
C  Adjust for changing width with depth
        IF(SS1(N)+SS2(N) > 0.) THEN
          BTOP = WIDTH(N) + (SS1(N) + SS2(N)) * VEL(3,N)
          BZ = WIDTH(N) + (SS1(N) + SS2(N)) * VEL(3,N) * ZPO
C
          BZ = BTOP/BZ
          ZPO = ZPO * BZ
        ENDIF
C
        VVEL(M)=R1(NN)+VVEL(L)+ZPO*(VVEL(N) -VVEL(L))
      ENDIF
  350 CONTINUE
  400 CONTINUE
      RETURN
      END
