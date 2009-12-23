      SUBROUTINE SA(NCN,NGP)
      USE SACMOD
      SAVE
!
!     Subroutine to obtain 5th order gauss point values
!
!     K is shape function number
!     I is gauss point number
!     L is shape function desired
!                type 1 = function
!                type 2 = x derivative
!                type 3 = y derivative
!                type 4 = z derivative
!     NCN is number of corner nodes, this determines element type.
!                20 = cube
!                15 = triangular prism
!                13 = rectangular pyramid
!                10 = tetrahedron
!
      REAL SJ(20,27,4),SM(15,21,4),SK(10,21,4),SS(13,27,4)
!-
      DIMENSION SI(27,3),SL(21,3),SN(7,3),SMULT(3),ST(9,2),ILOKUP(13)   &
     &  ,SI2(8,3)
!-
      DATA SI /-1.,0.,1.,-1.,0.,1.,-1.,0.,1.,-1.,0.,1.,-1.,0.,1.,       &
     &         -1.,0.,1.,-1.,0.,1.,-1.,0.,1.,-1.,0.,1.,                 &
     &         -1.,-1.,-1.,0.,0.,0.,1.,1.,1.,-1.,-1.,-1.,0.,0.,0.,      &
     &         1.,1.,1.,-1.,-1.,-1.,0.,0.,0.,1.,1.,1.,                  &
     &         1.,1.,1.,1.,1.,1.,1.,1.,1.,0.,0.,0.,0.,0.,0.,            &
     &         0.,0.,0.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1.,-1./
!-
      DATA SI2/  1.,-1.,-1., 1., 1.,-1.,-1., 1.,                        &
     &           1., 1.,-1.,-1., 1., 1.,-1.,-1.,                        &
     &          -1.,-1.,-1.,-1., 1., 1., 1., 1./
!-
      DATA SL/                                                          &
     & 0.333333332,0.470142063,0.059715872,0.470142063,0.101286507,     &
     & 0.797426984,0.101286507,0.333333332,0.470142063,0.059715872,     &
     & 0.470142063,0.101286507,0.797426984,0.101286507,0.333333332,     &
     &0.470142063,0.059715872,0.470142063,0.101286507,0.797426984,      &
     & 0.1012865078,                                                    &
     &0.0,0.410426192,0.0,-0.410426192,-0.696140483,0.0,0.696140483,    &
     &0.0,0.410426192,0.0,-0.410426192,-0.696140483,0.0,0.696140483,    &
     & 0.0,0.410426192,0.0,-0.410426192,-0.696140483,0.0,0.696140483,   &
     & 7*-0.774596669,7*0.0,7*0.774596669 /
!-
      DATA SN/                                                          &
     & 0.333333333,0.470142064,0.470142064,0.0597158717,0.1012865073    &
     &,0.797426985,0.1012865073,0.333333333,0.0597148717,0.470142064    &
     &,0.470142064,0.1012865073,0.1012865073,0.797426985,0.333333333    &
     &,0.470142064,0.0597158717,0.470142064,0.797426985,0.1012865073    &
     &,0.1012865073/
!-
      DATA SMULT/.2949977901,0.6529962340,0.9270059758/,SFCT/0.77459666/
      DATA ST/-1.,2*1.,-1.,0.,1.,0.,-1.,0.,2*-1.,2*1.,-1.,0.,1.,0.,0./
      DATA ILOKUP/5,6,1,2,3,4,9,7,14,15,10,11,13/
      DATA ITIMJ,ITIMM,ITIMK,ITIMS/4*0/
      IF( NCN == 20 ) GO TO 50
      IF( NCN == 15 ) GO TO 200
      IF( NCN == 10 ) GO TO 300
      GO TO 400
!-
!......PROCESS CUBE
!-
   50 CONTINUE
      IF(ITIMJ == 0) THEN
        DO 150 I=1,NGP
          IF(NGP == 8) THEN
            X=SI2(I,1)/SQRT(3.)
            Y=SI2(I,2)/SQRT(3.)
            Z=SI2(I,3)/SQRT(3.)
          ELSE
            X=SI(I,1)*SQRT(0.6)
            Y=SI(I,2)*SQRT(0.6)
            Z=SI(I,3)*SQRT(0.6)
          ENDIF
          DO 140 K=1,NCN
            DO 130 L=1,4
              SJ(K,I,L)=XN3(3,K,X,Y,Z,L)
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE
        ITIMJ=1
      ENDIF
      DO 70 I=1,NGP
        DO 70 K=1,NCN
          XNX(K,I)=SJ(K,I,1)
          DA(K,I)=SJ(K,I,2)
          DB(K,I)=SJ(K,I,3)
          DC(K,I)=SJ(K,I,4)
   70 CONTINUE
      RETURN
!-
!......PROCESS TRIANGULAR PRISM
!-
  200 IF(ITIMM == 0) THEN
        DO 280 L=1,4
          DO 275 I=1,NGP
            DO 270 K=1,NCN
              SM(K,I,L) = XN3(2,K,SL(I,1),SL(I,2),SL(I,3),L)
  270       CONTINUE
  275     CONTINUE
  280   CONTINUE
        ITIMM=1
      ENDIF
      DO 220 I=1,NGP
        DO 220 K=1,NCN
          XNX(K,I)=SM(K,I,1)
          DA(K,I)=SM(K,I,2)
          DB(K,I)=SM(K,I,3)
          DC(K,I)=SM(K,I,4)
  220 CONTINUE
      RETURN
!
!......PROCESS GAUSS POINTS FOR TETRAHEDRA
!
  300 IF(ITIMK == 0) THEN
        DO 380 I=1,NGP
          M=(I+6)/7
          J=I-(M-1)*7
          X=SN(J,1)*SMULT(M)
          Y=SN(J,2)*SMULT(M)
          Z=SN(J,3)*SMULT(M)
          DO 375 L=1,4
            DO 370 K=1,NCN
              SK(K,I,L)=XN3(1,K,X,Y,Z,L)
  370       CONTINUE
  375     CONTINUE
  380   CONTINUE
        ITIMK=1
      ENDIF
      DO 320 I=1,NGP
        DO 320 K=1,NCN
          XNX(K,I)=SK(K,I,1)
          DA(K,I)=SK(K,I,2)
          DB(K,I)=SK(K,I,3)
          DC(K,I)=SK(K,I,4)
  320 CONTINUE
      RETURN
!-
!......PROCESS RECTANGULAR PYRAMID
!-
  400 IF(ITIMS == 0) THEN
        DO 480 I=1,NGP
          M=(I+8)/9
          J=I-(M-1)*9
          X=1.-SMULT(M)
          Y=ST(J,1)*SMULT(M)*SFCT
          Z=ST(J,2)*SMULT(M)*SFCT
          DO 475 K=1,NCN
            KL=ILOKUP(K)
            DO 470 L=1,4
              SS(K,I,L)=XN3(4,KL,X,Y,Z,L)
  470       CONTINUE
  475     CONTINUE
  480   CONTINUE
        ITIMS=1
      ENDIF
      DO 420 I=1,NGP
        DO 420 K=1,NCN
          XNX(K,I)=SS(K,I,1)
          DA(K,I)=SS(K,I,2)
          DB(K,I)=SS(K,I,3)
  420 DC(K,I)=SS(K,I,4)
      RETURN
      END
