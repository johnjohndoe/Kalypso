!
      SUBROUTINE SAN(NCN)
      USE SACMOD
      SAVE
!
!     Subroutine to obtain nodal basis function values
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
      REAL SJN(20,20,4),SMN(15,15,4),SKN(10,10,4),SSN(13,33,4)
!-
      DIMENSION SI(20,3),SL(15,3),SN(10,3),SMULT(3),ST(13,3),ILOKUP(13) &
     &  ,SI2(8,3)
!-
      DATA SI /-1., 0., 1., 1., 1., 0.,-1.,-1.,-1., 1.,1.,-1.,          &
     &         -1., 0., 1., 1. ,1., 0.,-1.,-1.,                         &
     &         -1.,-1.,-1., 0., 1., 1., 1., 0.,-1.,-1.,1., 1.,          &
     &         -1.,-1.,-1. ,0., 1., 1., 1., 0.,                         &
     &         -1.,-1.,-1.,-1.,-1.,-1.,-1.,-1., 0., 0.,0., 0.,          &
     &          1., 1., 1., 1., 1., 1., 1. ,1./
!-
      DATA SI2/  1.,-1.,-1., 1., 1.,-1.,-1., 1.,                        &
     &           1., 1.,-1.,-1., 1., 1.,-1.,-1.,                        &
     &          -1.,-1.,-1.,-1., 1., 1., 1., 1./
!-
      DATA SL/                                                          &
     & 0.0,0.5,1.0,0.5,0.0,0.0,0.0,1.0,0.0,0.0,0.5,1.0,0.5,0.0,0.0,     &
     & -1.,-.5,0.0, .5, 1.,0.0,-1.,0.0, 1.,-1.,-.5,0.0, .5, 1.,0.0,     &
     & 6*-1.,3*0.0,6*1./
!-
      DATA SN/ 0.,0.5,1.0,0.5,0.0,0.0,0.0,0.5,0.0,0.0,                  &
     &         0.,0.0,0.0,0.5,1.0,0.5,0.0,0.0,0.5,0.0,                  &
     &         0.,0.0,0.0,0.0,0.0,0.0,0.5,0.5,0.5,1.0/
!-
      DATA SMULT/.2949977901,0.6529962340,0.9270059758/,SFCT/0.77459666/
      DATA ST/ 0., 0., 0., 0.5,1., 0.5, 0., 0., 0.0, 0.0, 0.0, 0.5,0.5, &
     &         1., 0.,-1.,-0.5,0., 0.5, 1.,-1., 1.0, 0.0,-1.0,-0.5,0.5, &
     &        -1.,-1.,-1.,-0.5,0.,-0.5, 0., 0., 1.0, 1.0, 1.0, 0.5,0.5/
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
        DO 150 I=1,NCN
          X=SI(I,1)
          Y=SI(I,2)
          Z=SI(I,3)
          DO 140 K=1,NCN
            DO 130 L=1,4
              SJN(K,I,L)=XN3(3,K,X,Y,Z,L)
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE
        ITIMJ=1
      ENDIF
      DO 70 I=1,NCN
        DO 70 K=1,NCN
          XNX(K,I)=SJN(K,I,1)
          DA(K,I)=SJN(K,I,2)
          DB(K,I)=SJN(K,I,3)
          DC(K,I)=SJN(K,I,4)
   70 CONTINUE
      RETURN
!-
!......PROCESS TRIANGULAR PRISM
!-
  200 IF(ITIMM == 0) THEN
        DO 280 L=1,4
          DO 275 I=1,NCN
            DO 270 K=1,NCN
              SMN(K,I,L) = XN3(2,K,SL(I,1),SL(I,2),SL(I,3),L)
  270       CONTINUE
  275     CONTINUE
  280   CONTINUE
        ITIMM=1
      ENDIF
      DO 220 I=1,NCN
        DO 220 K=1,NCN
          XNX(K,I)=SMN(K,I,1)
          DA(K,I)=SMN(K,I,2)
          DB(K,I)=SMN(K,I,3)
          DC(K,I)=SMN(K,I,4)
  220 CONTINUE
      RETURN
!
!......PROCESS GAUSS POINTS FOR TETRAHEDRA
!
  300 IF(ITIMK == 0) THEN
        DO 380 I=1,NCN
          DO 375 L=1,4
            DO 370 K=1,NCN
              SKN(K,I,L)=XN3(1,K,SN(I,1),SN(I,2),SN(I,3),L)
  370       CONTINUE
  375     CONTINUE
  380   CONTINUE
        ITIMK=1
      ENDIF
      DO 320 I=1,NCN
        DO 320 K=1,NCN
          XNX(K,I)=SKN(K,I,1)
          DA(K,I)=SKN(K,I,2)
          DB(K,I)=SKN(K,I,3)
          DC(K,I)=SKN(K,I,4)
  320 CONTINUE
      RETURN
!-
!......PROCESS RECTANGULAR PYRAMID
!-
  400 IF(ITIMS == 0) THEN
        DO 480 I=1,NCN
          DO 475 K=1,NCN
            KL=ILOKUP(K)
            DO 470 L=1,4
              SSN(K,I,L)=XN3(4,KL,ST(I,1),ST(I,2),ST(I,3),L)
  470       CONTINUE
  475     CONTINUE
  480   CONTINUE
        ITIMS=1
      ENDIF
      DO 420 I=1,NCN
        DO 420 K=1,NCN
          XNX(K,I)=SSN(K,I,1)
          DA(K,I)=SSN(K,I,2)
          DB(K,I)=SSN(K,I,3)
  420 DC(K,I)=SSN(K,I,4)
      RETURN
      END
