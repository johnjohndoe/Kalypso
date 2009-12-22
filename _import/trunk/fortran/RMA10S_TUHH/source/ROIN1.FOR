cipk  last update Oct 31 1996 Revised version to precompute values    

      SUBROUTINE ROIN(NN)
      USE IHILMOD
      USE SACMOD
      USE BLK10MOD
      SAVE
C-
      REAL J11,J12,J13,J21,J22,J23,J31,J32,J33
C-
      REAL XM(20),DMX(20),DMY(20),DMZ(20)
     2      ,XN(20),DNX(20),DNY(20),DNZ(20)
     3      ,XL(20),YL(20),ZL(20)
C-
      DIMENSION
     1        IPNT(4,6),ITOP(4,6)
     +        ,INSEQ(8,4)
      DATA NCNO/0/

      DATA IPNT/ 1, 3, 5, 7,5,14,0,0, 1, 3, 5,0,1,3,0,0,4*0,1,0,0,0/
      DATA ITOP/13,15,17,19,3,12,0,0,10,12,14,0,9,11,0,0,4*0,10,0,0,0/
cipk oct96
      DATA INSEQ/1,3,5,7,13,15,17,19,
     +           1,3,5,10,12,14,2*0,
     +           1,3,5,10,4*0,
     +           1,3,5,9,11,3*0/
C-
      DATA IPASS/0/
C-
C-.....ASSIGN PROPER COEFS.....
C-
      NCN=NCORN(NN)
      CX=COS(TH(NN))
      SAA=SIN(TH(NN))
      IF(NCN .EQ. 20) THEN
        ILK=1
        ITYP=1
      ELSEIF(NCN .EQ. 15) THEN
        ILK=2
        IF(NSURF(NOP(NN,1)) .NE. NSURF(NOP(NN,10))) THEN
          ITYP=2
        ELSE
          ITYP=3
        ENDIF
      ELSEIF(NCN .EQ. 13) THEN
        ILK=4
        ITYP=4
      ELSE
        ILK=3
        ITYP=6
      ENDIF
C-
C-.....COPY PROPER WEIGHTING FUNCTIONS.....
C-
c      IF(NCNO .EQ. NCN) GO TO 95
      CALL SAN(NCN)
C-
C-.....COMPUTE LOCAL CORDS.....
C-
   95 NR=NOP(NN,1)
      XL(1)=0.
      YL(1)=0.
      ZL(1)=0.
      DO 100 K = 2, NCN
      N=NOP(NN,K)
      DX=CORD(N,1)-CORD(NR,1)
      DY=CORD(N,2)-CORD(NR,2)
cipk nov96      XL(K)=DX*CX+DY*SAA
cipk nov 96      YL(K)=-DX*SAA+DY*CX
      XL(K)=DX
      YL(K)=DY
      ZL(K)=CORD(N,3)-CORD(NR,3)
  100 CONTINUE
C-
C- COMPUTE INTERMEDIATE COEFS. AND ELEMENT TRANSPORT MATRIX
C-
      DO 200 II=1,4
        I=IPNT(II,ITYP)
        IF(I .EQ. 0) GO TO 200
c      write(75,*) 'da',(da(k,i),k=1,20)
c      write(75,*) 'db',(db(k,i),k=1,20)
c      write(75,*) 'dc',(dc(k,i),k=1,20)
C-
C-.....FORM THE JACOBIAN.....
C-
      J11 = 0.0
      J12 = 0.0
      J13=0.
      J21 = 0.0
      J22 = 0.0
      J23=0.
      J31=0.
      J32=0.
      J33=0.
      DO 130 K = 2, NCN
      J11 = J11 + DA(K,I) * XL(K)
      J12 = J12 + DA(K,I) * YL(K)
      J13=J13+DA(K,I)*ZL(K)
      J21 = J21 + DB(K,I) * XL(K)
      J22 = J22 + DB(K,I) * YL(K)
      J23=J23+DB(K,I)*ZL(K)
      J31=J31+DC(K,I)*XL(K)
      J32=J32+DC(K,I)*YL(K)
      J33=J33+DC(K,I)*ZL(K)
  130 CONTINUE
c      write(75,*) j11,j12,j13,j21,j22,j23,j31,j32,j33
      A11= J22*J33-J23*J32
      A12=-J12*J33+J13*J32
      A13=J12*J23-J13*J22
      A21=-J21*J33+J23*J31
      A22=J11*J33-J13*J31
      A23=-J11*J23+J13*J21
      A31=J21*J32-J22*J31
      A32=-J11*J32+J12*J31
      A33=J11*J22-J12*J21
c      write(75,*) nn,a11,a12,a13,a21,a22,a23,a31,a32,a33
      DETJ=J11*A11+J12*A21+J13*A31
c      write(75,*) nn,i,detj
      DO 135 J = 1, NCN
      XN(J) = XNX(J,I)
      DNX(J)=(A11*DA(J,I)+A12*DB(J,I)+A13*DC(J,I))/DETJ
      DNY(J)=(A21*DA(J,I)+A22*DB(J,I)+A23*DC(J,I))/DETJ
      DNZ(J)=(A31*DA(J,I)+A32*DB(J,I)+A33*DC(J,I))/DETJ
  135 CONTINUE
C-
C-     REPEAT FOR LINEAR FUNCTION
C-
      DO 145 J=1,NCN
        XM(J)=0.
        DMX(J)=0.
        DMY(J)=0.
        DMZ(J)=0.
  145 CONTINUE
C-
      DO 160 J=1,NCN
        IF(IL(J,ILK) .NE. 0) THEN
          ML=IL(J,ILK)
          MH=IH(J,ILK)
          XM(ML)=XM(ML)+XN(J)/2.
          XM(MH)=XM(MH)+XN(J)/2.
          DMX(ML)=DMX(ML)+DNX(J)/2.
          DMX(MH)=DMX(MH)+DNX(J)/2.
          DMY(ML)=DMY(ML)+DNY(J)/2.
          DMY(MH)=DMY(MH)+DNY(J)/2.
          DMZ(ML)=DMZ(ML)+DNZ(J)/2.
          DMZ(MH)=DMZ(MH)+DNZ(J)/2.
        ELSE
          XM(J)=XM(J)+XN(J)
          DMX(J)=DMX(J)+DNX(J)
          DMY(J)=DMY(J)+DNY(J)
          DMZ(J)=DMZ(J)+DNZ(J)
        ENDIF
  160 CONTINUE
        K=NOP(NN,I)
        DRODXN(K)=0.0
        DRODYN(K)=0.0
        DO 180 M=1,NCN
ccc        IF(IL(J,ILK) .EQ. 0) THEN
          IF(IL(M,ILK) .EQ. 0) THEN
            MR=NOP(NN,M)
            DRODXN(K)=DRODXN(K)+DMX(M)*DEN(MR)
            DRODYN(K)=DRODYN(K)+DMY(M)*DEN(MR)
          ENDIF
c            DRODXN(K)=DRODXN(K)+DNX(M)*DEN(MR)
c            DRODYN(K)=DRODYN(K)+DNY(M)*DEN(MR)
  180   CONTINUE
c        write(76,*) 'drodxn',NN,k,drodxn(k),drodyn(k)
  200 CONTINUE
C
C
C
C  Calculate DRODXN, DRODYN for Surface nodes
C
      IF (NN .LE. NEM)  THEN
C
        DO 1200 II=1,4
          I=ITOP(II,ITYP)
          IF(I .EQ. 0) GO TO 1200
c      write(75,*) 'da',(da(k,i),k=1,20)
c      write(75,*) 'db',(db(k,i),k=1,20)
c      write(75,*) 'dc',(dc(k,i),k=1,20)
C-
C-.....FORM THE JACOBIAN.....
C-
        J11 = 0.0
        J12 = 0.0
        J13=0.
        J21 = 0.0
        J22 = 0.0
        J23=0.
        J31=0.
        J32=0.
        J33=0.
        DO 1130 K = 2, NCN
          J11 = J11 + DA(K,I) * XL(K)
          J12 = J12 + DA(K,I) * YL(K)
          J13=J13+DA(K,I)*ZL(K)
          J21 = J21 + DB(K,I) * XL(K)
          J22 = J22 + DB(K,I) * YL(K)
          J23=J23+DB(K,I)*ZL(K)
          J31=J31+DC(K,I)*XL(K)
          J32=J32+DC(K,I)*YL(K)
          J33=J33+DC(K,I)*ZL(K)
 1130   CONTINUE
c      write(75,*) j11,j12,j13,j21,j22,j23,j31,j32,j33
        A11= J22*J33-J23*J32
        A12=-J12*J33+J13*J32
        A13=J12*J23-J13*J22
        A21=-J21*J33+J23*J31
        A22=J11*J33-J13*J31
        A23=-J11*J23+J13*J21
        A31=J21*J32-J22*J31
        A32=-J11*J32+J12*J31
        A33=J11*J22-J12*J21
c      write(75,*) nn,a11,a12,a13,a21,a22,a23,a31,a32,a33
        DETJ=J11*A11+J12*A21+J13*A31
c      write(75,*) nn,i,detj
        DO 1135 J = 1, NCN
          XN(J) = XNX(J,I)
          DNX(J)=(A11*DA(J,I)+A12*DB(J,I)+A13*DC(J,I))/DETJ
          DNY(J)=(A21*DA(J,I)+A22*DB(J,I)+A23*DC(J,I))/DETJ
          DNZ(J)=(A31*DA(J,I)+A32*DB(J,I)+A33*DC(J,I))/DETJ
 1135   CONTINUE
C-
C-     REPEAT FOR LINEAR FUNCTION
C-
        DO 1145 J=1,NCN
          XM(J)=0.
          DMX(J)=0.
          DMY(J)=0.
          DMZ(J)=0.
 1145   CONTINUE
C-
        DO 1160 J=1,NCN
          IF(IL(J,ILK) .NE. 0) THEN
            ML=IL(J,ILK)
            MH=IH(J,ILK)
            XM(ML)=XM(ML)+XN(J)/2.
            XM(MH)=XM(MH)+XN(J)/2.
            DMX(ML)=DMX(ML)+DNX(J)/2.
            DMX(MH)=DMX(MH)+DNX(J)/2.
            DMY(ML)=DMY(ML)+DNY(J)/2.
            DMY(MH)=DMY(MH)+DNY(J)/2.
            DMZ(ML)=DMZ(ML)+DNZ(J)/2.
            DMZ(MH)=DMZ(MH)+DNZ(J)/2.
          ELSE
            XM(J)=XM(J)+XN(J)
            DMX(J)=DMX(J)+DNX(J)
            DMY(J)=DMY(J)+DNY(J)
            DMZ(J)=DMZ(J)+DNZ(J)
          ENDIF
 1160   CONTINUE
        K=NOP(NN,I)
        DRODXN(K)=0.0
        DRODYN(K)=0.0
        DO 1180 M=1,NCN
ccc        IF(IL(J,ILK) .EQ. 0) THEN
          IF(IL(M,ILK) .EQ. 0) THEN
            MR=NOP(NN,M)
            DRODXN(K)=DRODXN(K)+DMX(M)*DEN(MR)
            DRODYN(K)=DRODYN(K)+DMY(M)*DEN(MR)
          ENDIF
c            DRODXN(K)=DRODXN(K)+DNX(M)*DEN(MR)
c            DRODYN(K)=DRODYN(K)+DNY(M)*DEN(MR)
 1180   CONTINUE
c        write(76,*) 'drodxn',NN,k,drodxn(k),drodyn(k)
C
 1200   CONTINUE
C
      ENDIF
C
C
C ..... INTEGRATE VERTICALLY TO FORM NODAL INT OF DRODX
C
      DO 300 II=1,4
        I=IPNT(II,ITYP)
        IF(I .EQ. 0) GO TO 300
        ITP=ITOP(II,ITYP)
        NTOP=NOP(NN,ITP)
        IF(NTOP .LE. NPM) THEN
          DROXIN(NTOP)=0.
          DROYIN(NTOP)=0.
        ENDIF
C
C     INTEGRATE FOR NODAL VALUE
C
        SCL= VEL(3,NOP(NN,I))/(ELEV-AO(NOP(NN,I)))*(ZL(ITP)-ZL(I))
        DROXIN(NOP(NN,I))=DROXIN(NTOP)+
     +   (DRODXN(NOP(NN,I))+DRODXN(NTOP))/2.*SCL
        DROYIN(NOP(NN,I))=DROYIN(NTOP)+
     +   (DRODYN(NOP(NN,I))+DRODYN(NTOP))/2.*SCL
c        WRITE(75,*) 'droxin',NN,NOP(NN,I),DROXIN(NOP(NN,I)),
c     +             DROYIN(NOP(NN,I))
  300 CONTINUE
      NCNO=NCN
cipk nov96 add loop to store corner node values
      DO II=1,8
        JJ=INSEQ(II,ILK)
        IF(JJ .GT. 0) THEN
          DROXINS(NN,II)=DROXIN(NOP(NN,JJ))
          DROYINS(NN,II)=DROYIN(NOP(NN,JJ))
        ENDIF
      ENDDO
cipk nov96 end changes
      RETURN
      END
