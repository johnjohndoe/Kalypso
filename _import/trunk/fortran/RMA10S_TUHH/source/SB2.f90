!
      SUBROUTINE SB2(NCN,NGP)
      USE BLKSMOD
      SAVE
!
!     Subroutine to obtain 5th or 7th order gauss point values
!
!     K is shape function number
!     I is gauss point number
!     L is shape function desired
!                type 1 = function
!                type 2 = x derivative
!                type 3 = y derivative
!     NCN is number of corner nodes, this determines element type.
!                 6 = triangle
!                 8 = rectangle
!
!
!     NODE AND GAUSS POINT LOCATIONS DIAGRAMMED BELOW.
!     FOR 5TH ORDER CASE
!     FOR A QUAD, -1. < (ETA,PSI) < 1.
!     FOR A TRIANGLE, 0. < (ETA,PSI) < 1., AND ETA+PSI < 1.
!
!                      PSI                          5
!                      /                                                               /
!                     /                                                              /
!            7 - - - 6 - - - 5              PSI /   7
!           /               /                  /                                       /
!          /   1   2   3   /                  6  3     2  4     /
!         /               /                  /      1                            ETA
!        8   4   5   6   4 ---ETA           /
!       /               /                  /   5    4    6
!      /   7   8   9   /                  /
!     /               /                  1 - - - - -2- - - - - 3
!    1 - - - 2 - - - 3
!
!      QUADRALATERAL                            TRIANGLE
!
!
!input parameters      
!----------------      
      integer (kind = 4), intent (in) :: ncn
!
!local parameters      
!----------------      
      REAL*8 XN2,DNX,DNY
      REAL*8 ETAGPT,PSIGPT,ETAGPR,PSIGPR,ETGPTH,PSGPTH,ETGPRH,PSGPRH    &
     &      ,ETGPRL,PSGPRL
      REAL*8                                                            &
     &   SJ,   SK,SL,SM,   SN,    X,    Y
!
      DIMENSION ETAGPT(7),PSIGPT(7),ETAGPR(9),PSIGPR(9)                 &
     &         ,ETGPRL(4),PSGPRL(4)
      DIMENSION ETGPTH(16),PSGPTH(16),ETGPRH(16),PSGPRH(16)
      DIMENSION SJ(8,9,3),SK(8,16,3),SL(8,4,3)
      DIMENSION SM(6,7,3),SN(6,16,3)
      DATA ICNT7,ICNR9,ICNT16,ICNR16,ICNR4/5*0/
!
!     ---------------------
!     GAUSS POINT LOCATIONS
!     ---------------------
!
      DATA ETAGPT /                                                     &
     &   0.33333333333333D0,   0.05971587178978D0,   0.47014206410510D0,&
     &   0.47014206410510D0,   0.79742698535307D0,   0.10128650732346D0,&
     &   0.10128650732346D0/
!
      DATA PSIGPT /                                                     &
     &   0.33333333333333D0,   0.47014206410510D0,   0.05971587178978D0,&
     &   0.47014206410510D0,   0.10128650732346D0,   0.79742698535307D0,&
     &   0.10128650732346D0/
!
      DATA ETAGPR /                                                     &
     &  -0.77459666924146D0,   0.00000000000000D0,   0.77459666924146D0,&
     &  -0.77459666924146D0,   0.00000000000000D0,   0.77459666924146D0,&
     &  -0.77459666924146D0,   0.00000000000000D0,   0.77459666924146D0/
!
      DATA PSIGPR /                                                     &
     &   0.77459666924146D0,   0.77459666924146D0,   0.77459666924146D0,&
     &   0.00000000000000D0,   0.00000000000000D0,   0.00000000000000D0,&
     &  -0.77459666924146D0,  -0.77459666924146D0,  -0.77459666924146D0/
!
      DATA ETGPTH /                                                     &
     &0.00970378512695D0, 0.13005607921683D0, 0.04612207990645D0,       &
     &0.09363778443732D0, 0.02891208422438D0, 0.38749748340665D0,       &
     &0.13741910413455D0, 0.27899046349647D0, 0.05021012321138D0,       &
     &0.67294686315042D0, 0.23864865973143D0, 0.48450832663038D0,       &
     &0.06546699455501D0, 0.87742880933037D0, 0.31116455224431D0,       &
     &0.63173125164105D0  /
!
      DATA PSGPTH /                                                     &
     &0.13005607921683D0, 0.00970378512695D0, 0.09363778443732D0,       &
     &0.04612207990645D0, 0.38749748340665D0, 0.02891208422438D0,       &
     &0.27899046349647D0, 0.13741910413455D0, 0.67294686315042D0,       &
     &0.05021012321138D0, 0.48450832663038D0, 0.23864865973143D0,       &
     &0.87742880933037D0, 0.06546699455501D0, 0.63173125164105D0,       &
     &0.31116455224431D0  /
!
      DATA ETGPRH /                                                     &
     &-0.8611363116D0    ,-0.3399810436D0    ,0.3399810436D0,           &
     & 0.8611363116D0    ,                                              &
     &-0.8611363116D0    ,-0.3399810436D0    ,0.3399810436D0,           &
     & 0.8611363116D0    ,                                              &
     &-0.8611363116D0    ,-0.3399810436D0    ,0.3399810436D0,           &
     & 0.8611363116D0    ,                                              &
     &-0.8611363116D0    ,-0.3399810436D0    ,0.3399810436D0,           &
     & 0.8611363116D0    /
!
      DATA PSGPRH /                                                     &
     &4*-0.8611363116D0    ,4*-0.3399810436D0    ,                      &
     & 4*0.3399810436D0    , 4*0.8611363116D0    /
!-
      DATA ETGPRL /                                                     &
     & -0.57735026918626D0,  0.57735026918626D0,                        &
     & -0.57735026918626D0,  0.57735026918626D0/
!-
      DATA PSGPRL /                                                     &
     & -0.57735026918626D0, -0.57735026918626D0,                        &
     &  0.57735026918626D0,  0.57735026918626D0/
!-
      IF( NCN == 6 ) GO TO 250
!-
!......PROCESS RECTANGLE
!-
      IF(NGP == 16) GO TO 160
      IF(NGP == 4) GO TO 200
      IF(ICNR9 == 0) THEN
        DO 120 I=1,NGP
          X=ETAGPR(I)
          Y=PSIGPR(I)
          DO 110 K=1,NCN
            SJ(K,I,1)=XN2(1,K,X,Y)
            SJ(K,I,2)=DNX(1,K,X,Y)
            SJ(K,I,3)=DNY(1,K,X,Y)
  110     CONTINUE
  120   CONTINUE
        ICNR9=1
      ENDIF
      DO 140 I=1,NGP
        DO 130 K=1,NCN
          XNX(K,I)=SJ(K,I,1)
          DA(K,I)=SJ(K,I,2)
          DB(K,I)=SJ(K,I,3)
  130   CONTINUE
  140 CONTINUE
      GO TO 400
  160 IF(ICNR16 == 0) THEN
        DO 170 I=1,NGP
          X=ETGPRH(I)
          Y=PSGPRH(I)
          DO 165 K=1,NCN
            SK(K,I,1)=XN2(1,K,X,Y)
            SK(K,I,2)=DNX(1,K,X,Y)
            SK(K,I,3)=DNY(1,K,X,Y)
  165     CONTINUE
  170   CONTINUE
        ICNR16=1
      ENDIF
      DO 180 I=1,NGP
        DO 175 K=1,NCN
          XNX(K,I)=SK(K,I,1)
          DA(K,I)=SK(K,I,2)
          DB(K,I)=SK(K,I,3)
  175   CONTINUE
  180 CONTINUE
      GO TO 400
!-
!...... LOWER ORDER POINTS
!-
  200 IF(ICNR4 == 0) THEN
        DO 210 I=1,NGP
          X=ETGPRL(I)
          Y=PSGPRL(I)
          DO 205 K=1,NCN
            SL(K,I,1)=XN2(1,K,X,Y)
            SL(K,I,2)=DNX(1,K,X,Y)
            SL(K,I,3)=DNY(1,K,X,Y)
  205     CONTINUE
  210   CONTINUE
        ICNR4=1
      ENDIF
      DO 220 I=1,NGP
        DO 215 K=1,NCN
          XNX(K,I)=SL(K,I,1)
          DA(K,I)=SL(K,I,2)
          DB(K,I)=SL(K,I,3)
  215   CONTINUE
  220 CONTINUE
      GO TO 400
!-
!......PROCESS TRIANGLE
!-
  250 CONTINUE
      IF(NGP == 16) GO TO 300
      IF(ICNT7 == 0) THEN
        DO 275 I=1,NGP
          X=ETAGPT(I)
          Y=PSIGPT(I)
          DO 270 K=1,NCN
            SM(K,I,1) = XN2(2,K,X,Y)
            SM(K,I,2) = DNX(2,K,X,Y)
            SM(K,I,3) = DNY(2,K,X,Y)
  270     CONTINUE
  275   CONTINUE
        ICNT7=1
      ENDIF
      DO 290 I=1,NGP
        DO 285 K=1,NCN
          XNX(K,I)=SM(K,I,1)
          DA(K,I)=SM(K,I,2)
          DB(K,I)=SM(K,I,3)
  285   CONTINUE
  290 CONTINUE
      GO TO 400
  300 IF(ICNT16 == 0) THEN
        DO 325 I=1,NGP
          X=ETGPTH(I)
          Y=PSGPTH(I)
          DO 310 K=1,NCN
            SN(K,I,1) = XN2(2,K,X,Y)
            SN(K,I,2) = DNX(2,K,X,Y)
            SN(K,I,3) = DNY(2,K,X,Y)
  310     CONTINUE
  325   CONTINUE
        ICNT16=1
      ENDIF
      DO 350 I=1,NGP
        DO 340 K=1,NCN
          XNX(K,I)=SN(K,I,1)
          DA(K,I)=SN(K,I,2)
          DB(K,I)=SN(K,I,3)
  340   CONTINUE
  350 CONTINUE
!
!              CREATE LINEAR FUNCTIONS
!
  400 CONTINUE
      DO 500 I=1,NGP
        J=0
        DO 450 K=1,NCN,2
          J=J+1
          NA=K-1
          IF(K == 1) NA=NCN
          XMX(J,I)=XNX(K,I)+(XNX(NA,I)+XNX(K+1,I))*0.5
          CA(J,I)=DA(K,I)+(DA(NA,I)+DA(K+1,I))*0.5
          CB(J,I)=DB(K,I)+(DB(NA,I)+DB(K+1,I))*0.5
  450   CONTINUE
  500 CONTINUE
      RETURN
      END
