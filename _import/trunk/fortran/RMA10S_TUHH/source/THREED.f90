!IPK  LAST UPDATE SEP 6 2004  add error file
!ipk  last update August 1 2001 correct bug that occurs when IMAT=0 in elements
!IPK  LAST UPDATE APR 21 2001  ADD NRELSF FOR SIDE ELEMENTS
!ipk  last update Nov 8 1999 reset values for eddy coefficients for potential collapse
!ipk  last update Jan 3 1999
!     Last change:  K    23 May 2007    4:59 pm
!IPK  LATEST UPDATE NOV 17 1997
      SUBROUTINE THREED
      USE BLK10MOD
      USE BLKDRMOD
      USE PARAMMOD
      SAVE
!     NETYP is an array the defines the type of each element
!           = 1   One dimensional  surface element (2d applications)
!           = 2   One dimensional  bottom  element (2d applications)
!           = 3
!           = 4   One dimensional  end     element (2d applications)
!           = 6   One dimensional          element (1d applications)
!           = 7   One dimensional junction element (1d applications)
!           = 8   One-two dimensional      transition element
!
!           = 11  Two dimensional  surface element (3d applications)
!           = 12  Two dimensional  bottom  element (3d applications)
!           = 13  Two dimensional  side    element (3d applications)
!           = 14  Two dimensional  end     element (3d applications)
!           = 15  Two dimensional          element (2d applications)
!           = 16  Two dimensional          element (2d applications)
!           = 17  Two dimensional junction element (2d applications)
!           = 18  Two-three dimensional    transition element
!
!           = 21   Three dimensional 20 point element
!           = 22   Three dimensional 15 point element
!           = 23   Three dimensional 13 point element
!           = 24   Three dimensional 10 point element
!-
!......TEMPORARY COMMON
!-
      INTEGER IS(8),ITT(8)
      ALLOCATABLE NSID(:),NSEQ(:,:)
!-
!......BUILT IN FUNCTION TO DETERMINE NODE NUMBER ADDED
!-
      NRF (N, J) = NREF (N) + IS (J) - ITT (J)
!-
!......INITIALIZE ARRAYS
!-
      ALLOCATE (NSID (MAXE), NSEQ (8, NLAYMX))
      IERR = 0
      VOID = -1.E20
      Init: DO N=1,MAXP
        NREF (N) = 0
      ENDDO Init
!
!nis,jan07,testing
!      WRITE(*,*) 'In threed.for'
!      do i = 1, ne
!        WRITE(*,*) netyp(i)
!      end do
!      pause
!   HINT: IF THERE IS AN ERROR IN 2D-APPLICATIONS WITH THE THREED-SUBROUTINE, THEN THE NETWORK IS NOT PROPERLY DEFINED; YOU MIGHT HAVE INITIALIZED
!         ELEMENTS, THAT ARE NOT REALLY PART OF THE NETWORK, SO NOT OCCURING IN THE AR-LINES, BUT IN THE FE-LINES
!   HINT-END
!-
!
!-
!......SETUP NODAL COORDINATES
!-
      NCP=NP
      DO 300 M=1,NE
      LAB(M)=0
      NTHREE(M)=NFIXH(M)
      IF(IMAT(M) < 1) GO TO 300
      IF(NETYP(M) == 17) GO TO 300
      NCN=8
      IF(NOP(M,7) == 0) NCN=6
      IF(NOP(M,6) == 0) NCN=3
      IF(NOP(M,4) == 0) NCN=3
      DO 295 J=1,NCN
      N=NOP(M,J)
      IF(NREF(N) > 0) GO TO 295
      IF(MOD(J,2) /= 0) GO TO 245
!-
!......WORK ON MID-SIDE NODES
!-
      N1=NOP(M,J-1)
      MAA=NCN
      IF(NCN == 3) MAA=4
      N3=MOD(J+1,MAA)
      N3=NOP(M,N3)
      CORD(N,3)=ELEV
      NDEP(N)=NDEP(N1)
      IF(NDEP(N3) > NDEP(N)) NDEP(N)=NDEP(N3)
      IF(NDEP(N1) == 1 ) NDEP(N)=1
      IF(NDEP(N3) == 1 ) NDEP(N)=1
!     NREF(N)=NCP
      NV=NDEP(N)-1
      IF(NV == 0) GO TO 290
      NREF(N)=NCP
      DO 240 K=1,NV
      L=K+NCP
!ipk sep00      IF(L > MAXP) GO TO 2500
      if(l > maxp) then
        ierr=2
        go to 240
      endif
      NFIX(L)=NFIX(N)
      SPEC(L,1)=SPEC(N,1)
      SPEC(L,2)=SPEC(N,2)
      SPEC(L,4)=SPEC(N,4)
      SPEC(L,5)=SPEC(N,5)
      SPEC(L,6)=SPEC(N,6)
      NSURF(L)=N
      ALFA(L)=ALFA(N)
      IF(CORD(N,1) == VOID) GO TO 240
      CORD(L,1)=CORD(N,1)
      CORD(L,2)=CORD(N,2)
      WIDTH(L)=WIDTH(N)
!
!ipk jan99
      ss1(l) = ss1(n)
      ss2(l) = ss2(n)
!
!
  240 CONTINUE
      NCP=NCP+NV
      GO TO 295
  245 CONTINUE
!-
!......PROCESS CORNER NODES
!-
      CORD(N,3)=ELEV
!     NREF(N)=NCP
      NSURF(N)=N
      NV=NDEP(N)-1
      IF(NV == 0 ) GO TO 290
      NCPR=NCP+1
      NREF(N)=NCP
      FRAC=ELEV
      SUM=0.
      DO 246 K=1,NV
      SUM=SUM+THLAY(N,K)
  246 CONTINUE
      SUM=SUM/(ELEV-AO(N))
      DO 250 K=1,NV
      FRAC=FRAC-THLAY(N,K)/SUM
      IF(K == NV) FRAC=AO(N)
      NCP=NCP+2
!ipk sep00
      if(ncp > maxp) then
        ierr=2
        go to 250
      endif
      CORD(NCP,1)=CORD(N,1)
      CORD(NCP,2)=CORD(N,2)
      CORD(NCP,3)=FRAC
      CORD(NCP-1,1)=CORD(N,1)
      CORD(NCP-1,2)=CORD(N,2)
      WIDTH(NCP)=WIDTH(N)
!
!ipk jan99
      ss1(ncp) = ss1(n)
      ss2(ncp) = ss2(n)
!
      AO(NCP)=AO(N)
      NSURF(NCP)=N
      NSURF(NCP-1)=N
      NFIX(NCP)=NFIX(N)
      NFIX(NCP-1)=NFIX(N)
      SPEC(NCP-1,1)=SPEC(N,1)
      SPEC(NCP-1,2)=SPEC(N,2)
      SPEC(NCP-1,4)=SPEC(N,4)
      SPEC(NCP-1,5)=SPEC(N,5)
      SPEC(NCP-1,6)=SPEC(N,6)
      ALFA(NCP)=ALFA(N)
      ALFA(NCP-1)=ALFA(N)
!ipk nov97
      ADO(NCP)=ADO(N)
        AKP(NCP)=AKP(N)
        ADT(NCP)=ADT(N)
        ADB(NCP)=ADB(N)
      ADO(NCP-1)=ADO(N)
        AKP(NCP-1)=AKP(N)
        ADT(NCP-1)=ADT(N)
        ADB(NCP-1)=ADB(N)
!IPK NOV97 END ADDITIONS
      DO 248 NDA=1,NDF
  248 SPEC(NCP,NDA)=SPEC(N,NDA)
  250 CONTINUE
!ipk sep00
      if(ierr /= 2) then
        DO 265 K=NCPR,NCP,2
          NBTN(K)=NCP
  265   CONTINUE
      endif
!
!     NBTN IS BOTTOM NODE NUMBER
!     NSURF ID SURFACE NODE NUMBER
!
      NBTN(N)=NCP
      GO TO 295
  290 CONTINUE
!
!     LAB INDICATES TWO-DIMENSIONAL ELEMENT
!
  295 CONTINUE
  300 CONTINUE
      if(ierr == 2) then
!ipk sep04
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
        write(*,6410) ncp,maxp
        write(75,6410) ncp,maxp
 6410   format('ERROR STOP  -  3-D EXPANSION LEADS TO EXCESSIVE NUMBER' &
     & ' OF NODES'/'NODES GENERATED =',I8,'  NODES ALLOWED ='I8)
        stop
      endif
!
      DO 320 N=1,NP
        IF(NSURF(N) > 0) THEN
          NDEP(N)=NDEP(N)*2-1
        ELSE
          NSURF(N)=-N
        ENDIF
  320 CONTINUE
!
!     NDEP IS NOW NUMBER OF NODES DEEP FROM EACH SURFACE POINT
!
      NP=NCP
!-
!......AT THIS POINT ALL CORNER COORDINATES HAVE BEEN DEFINED
!-
!......NOW SET UP ELEMENT CONNECTIONS FOR ALL LOCATIONS
!-
!-
!......FIRST PREPARE BOTTOM AND TOP ELEMENTS
!-
      K=NE
      DO 350 N=1,NE
      IF(IMAT(N) < 1) GO TO 350
!      IF(NETYP(N) == 17) GO TO 350
      NCN=8
      IF(NOP(N,7) == 0) NCN=6
      IF(NOP(N,6) == 0) NCN=5
! rae 10/4/96  set NCN for junction elements of 4 nodes
!
      if(nop(n,5) == 0) ncn = 4
      IF(NOP(N,4) == 0) NCN=3
      NCORN(N)=NCN
      IF(NCN == 5) NCN=3
      DO 330 M=1,NCN
      I=NOP(N,M)
      IF(NDEP(I) == 1) GO TO 335
  330 CONTINUE
      GO TO 340
  335 LAB(N)=1
      GO TO 350
  340 IF(NETYP(N) == 17) GO TO 350
      NLOC(N)=K+1
!-
!......NOW DEFINE BOTTOM ELEMENTS
!-
      K=K+1
      IF(K > MAXE) GO TO 2500
      IMAT(K)=2000+IMAT(N)
      IF(NCN == 3) THEN
        NETYP(K)=2
      ELSE
        NETYP(K)=12
      ENDIF
      NCORN(K)=NCN
      DO 345 M=1,NCN
      I=NOP(N,M)
      NOP(K,M)=NREF(I)+NDEP(I)-1
  345 CONTINUE
!-
!......NOW DEFINE TOP ELEMEMTS
!-
!-    IF(IWIND == 0) GO TO 350
      K=K+1
      IF(K > MAXE) GO TO 2500
      IMAT(K)=1000+IMAT(N)
      IF(NCN == 3) THEN
        NETYP(K)=1
      ELSE
        NETYP(K)=11
      ENDIF
      NCORN(K)=NCN
!ipk jan02  This list gives the surface element for top 3d layer
      NRELSF(K)=N
      TH(K)=TH(N)
      DO 347 M=1,NCN
  347 NOP(K,M)=NOP(N,M)
!
!IPK NOV99 ADD FOR TRANSITION 2-D CASE
!
      DO M=1,6
        EEXXYY(M,K)=EEXXYY(M,N)
      ENDDO
!
  350 CONTINUE
!-
!......NOW DEVELOP SIDE ELEMENTS
!-
      NTB=K
      IHH=0
!
!     NTB IS NUMBER OF TOP LAYER ELEMENTS PLUS NUMBER OF BOTTOM ELEMENTS
!     INITIALIZE IHH WHICH IS COUNTER FOR REORDERING
!
      DO 500 NN=1,NE
      N=NTHREE(NN)
      NSID(N)=K
!ipk aug01
      IF(IMAT(N) < 1) then
        ihh=ihh+1
        nfixh(ihh)=n
        GO TO 500
      endif
!      IF(NETYP(N) == 17) GO TO 500
      IF(LAB(N) /= 1 ) GO TO 355
      IHH=IHH+1
      NFIXH(IHH)=N
      GO TO 500
  355 CONTINUE
      IF(NETYP(N) == 17) THEN
        IHH=IHH+1
        NFIXH(IHH)=N
        J2=NOP(N,1)
        N2DV=NDEP(J2)
        DO 356 KT=1,N2DV
          IF(KT /= 1) THEN
            IHH=IHH+1
            NFIXH(IHH)=0
          ENDIF
  356   CONTINUE
        GO TO 500
      ENDIF
!     NSID(N)=K
      NCN=NCORN(N)
      IF(NCORN(N) < 6) GO TO 430
!-
!..... Prepare to define NFIXH
!-
      N3DV=0
      DO 420 M=2,NCN,2
        NCT=0
!WP Feb 2006, Change NLAYM to NLAYMX
        DO 360 L=1,NLAYMX
          NSEQ(M,L)=0
  360   CONTINUE
!-
!......Form highest side rectangle and develop number of 3-D elements
!-
      J2=NOP(N,M)
      NCTM=NDEP(J2)-1
      IF(NCTM > N3DV) N3DV=NCTM
      IF(IBN(J2) == 0) GO TO 420
      J1=NOP(N,M-1)
      J3=MOD(M+1,NCN)
      J3=NOP(N,J3)
!ipk oct98 update to f90
      IMMT=IMAT(N)
      IF(MOD(IMMT,100) > 90) GO TO 420
!      IF(ALFA(J3) == ALFA(J1) .AND. IBN(J2) /= 2) GO TO 420
      K=K+1
      IF( K > MAXE) GO TO 2500
      IF(IBN(J2) == 2) THEN
        IMAT(K)=IMAT(N)+4000
        NETYP(K)=14
!IPK APR01
        NRELSF(K)=N
      ELSE
        IMAT(K)=3000+IMAT(N)
        NETYP(K)=13
!IPK APR01
        NRELSF(K)=N
      ENDIF
      NCORN(K)=8
      N1=NREF(J1)
      N2=NREF(J2)
      N3=NREF(J3)
      NOP(K,1)=J1
      NOP(K,2)=N1+1
      NOP(K,3)=N1+2
      NOP(K,4)=N2+1
      NOP(K,5)=N3+2
      NOP(K,6)=N3+1
      NOP(K,7)=J3
      NOP(K,8)=J2
      IS(1)=NDEP(J1)
      IS(2)=NDEP(J2)
      IS(3)=NDEP(J3)
      ITT(1)=IS(1)-2
      ITT(2)=IS(2)-1
      ITT(3)=IS(3)-2
!-
!...... NSEQ contains side element number
!-
      NCT=NCT+1
      NSEQ(M,NCT)=K
  375 IF(ITT(1) == 1) GO TO 380
      IF(ITT(3) == 1) GO TO 400
!-
!......Form other side rectangles
!-
      K=K+1
      IF(K > MAXE) GO TO 2500
      IF(IBN(J2) == 2) THEN
        IMAT(K)=IMAT(N)+4000
        NETYP(K)=14
!IPK APR01
        NRELSF(K)=N
      ELSE
        IMAT(K)=3000+IMAT(N)
        NETYP(K)=13
!IPK APR01
        NRELSF(K)=N
      ENDIF
      NCORN(K)=8
      NOP(K,1)=N1+IS(1)-ITT(1)
      NOP(K,2)=NOP(K,1)+1
      NOP(K,3)=NOP(K,1)+2
      NOP(K,8)=N2+IS(2)-ITT(2)
      NOP(K,4)=NOP(K,8)+1
      NOP(K,7)=N3+IS(3)-ITT(3)
      NOP(K,6)=NOP(K,7)+1
      NOP(K,5)=NOP(K,7)+2
      ITT(1)=ITT(1)-2
      ITT(2)=ITT(2)-1
      ITT(3)=ITT(3)-2
!-
!...... NSEQ contains side element number
!-
      NCT=NCT+1
      NSEQ(M,NCT)=K
      GO TO 375
!-
!......FORM SIDE TRIANGLE
!-
  380 CONTINUE
      IF(ITT(3) == 1) GO TO 420
      K=K+1
      IF(K > MAXE) GO TO 2500
      IF(IBN(J2) == 2) THEN
        IMAT(K)=IMAT(N)+4000
        NETYP(K)=14
!IPK APR01
        NRELSF(K)=N
      ELSE
        IMAT(K)=3000+IMAT(N)
        NETYP(K)=13
!IPK APR01
        NRELSF(K)=N
      ENDIF
      NCORN(K)=6
      NOP(K,1)=N1+IS(1)-ITT(1)
      NOP(K,6)=N2+IS(2)-ITT(2)
      NOP(K,2)=NOP(K,6)+1
      NOP(K,5)=N3+IS(3)-ITT(3)
      NOP(K,4)=NOP(K,5)+1
      NOP(K,3)=NOP(K,5)+2
      ITT(2)=ITT(2)-1
      ITT(3)=ITT(3)-2
!-
!...... NSEQ contains side element number
!-
      NCT=NCT+1
      NSEQ(M,NCT)=K
      GO TO 375
!-
!......FORM OTHER SHAPE TRIANGLE
!-
  400 CONTINUE
      K=K+1
      IF(K > MAXE) GO TO 2500
      IF(IBN(J2) == 2) THEN
        IMAT(K)=IMAT(N)+4000
        NETYP(K)=14
!IPK APR01
        NRELSF(K)=N
      ELSE
        IMAT(K)=3000+IMAT(N)
        NETYP(K)=13
!IPK APR01
        NRELSF(K)=N
      ENDIF
      NCORN(K)=6
      NOP(K,1)=N1+IS(1)-ITT(1)
      NOP(K,2)=NOP(K,1)+1
      NOP(K,3)=NOP(K,1)+2
      NOP(K,6)=N2+IS(2)-ITT(2)
      NOP(K,4)=NOP(K,6)+1
      NOP(K,5)=N3+IS(3)-1
      ITT(1)=ITT(1)-2
      ITT(2)=ITT(2)-1
!-
!...... NSEQ contains side element number
!-
      NCT=NCT+1
      NSEQ(M,NCT)=K
      GO TO 375
  420 CONTINUE
!-
!...... Now define NFIXH for this element
!-
      IHH=IHH+1
      NFIXH(IHH)=N
      IHH=IHH+1
      NFIXH(IHH)=NLOC(N)+1
      DO 428 KT=1,N3DV
!-
!...... After the first pass leave a gap for the 3-D element
!-
        IF(KT /= 1) THEN
          IHH=IHH+1
          NFIXH(IHH)=0
        ENDIF
        DO 426 M=2,NCN,2
          IF(NSEQ(M,KT) > 0) THEN
            IHH=IHH+1
            NFIXH(IHH)=NSEQ(M,KT)
          ENDIF
  426   CONTINUE
  428 CONTINUE
      IHH=IHH+1
      NFIXH(IHH)=NLOC(N)
      GO TO 500
!-
!...... Form elements for 2-D vertical line elements at end
!-
!...... Prepare for NFIXH definition
!-
  430 DO 460 KT=1,3,2
        NCT=0
!WP Feb 2006, Change NLAYM to NLAYMX
        DO 433 L=1,NLAYMX
          NSEQ(KT,L)=0
  433   CONTINUE
        J1=NOP(N,KT)
!ipk oct98 update to f90
        IMMT=IMAT(N)
        IF(MOD(IMMT,100) > 90) GO TO 460
        IF(IBN(J1) /= 2) GO TO 460
        N1=NREF(J1)
        IS(1)=NDEP(J1)
        ITT(1)=IS(1)
  440   K=K+1
        IF(K > MAXE) GO TO 2500
        NCT=NCT+1
        NSEQ(KT,NCT)=K
        NCORN(K)=3
        IMAT(K)=IMAT(N)+4000
        NETYP(K)=4
        IF(IS(1) == ITT(1)) THEN
          NOP(K,3)=J1
        ELSE
          NOP(K,3)=N1+IS(1)-ITT(1)
        ENDIF
        NOP(K,2)=N1+IS(1)-ITT(1)+1
        NOP(K,1)=NOP(K,2)+1
        IF(KT == 3) THEN
          NOP(K,1)=NOP(K,3)
          NOP(K,3)=NOP(K,2)+1
        ENDIF
!
!       store source element number in NOP(19)
!
        NOP(K,19)=N
        ITT(1)=ITT(1)-2
        IF(ITT(1) > 1) GO TO 440
  460 CONTINUE
!-
!...... Find the number of 2-D vertical elements at this element
!-
      J2=NOP(N,2)
      N2DV=NDEP(J2)-1
!-
!...... Now define NFIXH for this element
!-
      IHH=IHH+1
      NFIXH(IHH)=N
      IHH=IHH+1
      NFIXH(IHH)=NLOC(N)+1
      DO 475 KT=1,N2DV
!-
!...... After the first pass leave a gap for the 3-D element
!-
        IF(KT /= 1) THEN
          IHH=IHH+1
          NFIXH(IHH)=0
        ENDIF
        DO 470 M=1,3,2
          IF(NSEQ(M,KT) > 0) THEN
            IHH=IHH+1
            NFIXH(IHH)=NSEQ(M,KT)
          ENDIF
  470   CONTINUE
  475 CONTINUE
      IHH=IHH+1
      NFIXH(IHH)=NLOC(N)
  500 CONTINUE
!-
!......RESET TO FORM SURFACE ELEMENTS
!-
      NS=K-NTB
!
!     NS IS NUMBER OF SIDE ELEMENTS
!
      DO 600 N=1,NE
      IF(IMAT(N) < 1) GO TO 600
      IF(NETYP(N) == 17) GO TO 600
      MSHIFT=12
      IF(NCORN(N) == 6) MSHIFT=9
      IF(LAB(N) == 1 ) MSHIFT=0
      IF(NCORN(N) < 6) MSHIFT=0
      DO 599 M=1,8
      MS=M+MSHIFT
      NOP(N,MS)=NOP(N,M)
  599 CONTINUE
  600 CONTINUE
!IPK OCT98      NSC=0
      IHH=1
      NEE=NTB+NS
      NET=NEE+1
!IPK OCT98      NTHR=K-NEE
!
!     NEE IS LAST OF GENERATED ELEMENTS
!     NET IS NUMBER FOR NEXT NEW ELEMENT
!     NES WILL BE NUMBER OF PREVIOUSLY GENERATED ELEMENT
!
      DO 2200 NN=1,NE
      N=NTHREE(NN)
!IPK OCT98      NXT=NEE
      NNX=NTHREE(NN+1)
      IF(NN < NE) NXT=NSID(NNX)
      NES=NET-1
      NET=N
      NCN=NCORN(N)
      IF(NETYP(N) == 17) GO TO 2090
!-
!......TEST FOR QUADRILATERAL
!-
      IF(IMAT(N) < 1) GO TO 2150
      IF(LAB(N) == 1 ) GO TO 2150
      IF(NCN < 6) GO TO 1800
      IF(NCN == 8) GO TO 1600
!-
!......WORK ON TRIANGLE
!-
      DO 1325 M=1,6
      K=NOP(N,M)
      ITT(M)=NDEP(K)
      IF(MOD(M,2) == 1) ITT(M)=ITT(M)-1
      IS(M)=ITT(M)
 1325 CONTINUE
!-
!......DETERMINE TYPE OF TRIANGLE BASED ELEMENT
!-
!......CHECK IF ALL ELEMENTS ARE GENERATED
!-
 1330 CONTINUE
      DO 1331 M=1,5,2
      IF(ITT(M) < 0) ITT(M)=0
 1331 CONTINUE
      IF(ITT(1)+ITT(3)+ITT(5) == 0) GO TO 2200
      IF(NET == N) GO TO 1334
      IF(NET > MAXE) GO TO 2500
!-
!...... Find a gap in NFIXH and fill it
!-
 1332 IF(NFIXH(IHH) == 0) GO TO 1333
      IHH=IHH+1
      GO TO 1332
 1333 CONTINUE
      NFIXH(IHH)=NET
      TH(NET)=TH(N)
!
      DO 1336 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 1336 CONTINUE
!
 1334 CONTINUE
!-
!......CHECK FOR TRIANGULAR PRISM
!-
      IF(ITT(1)*ITT(3)*ITT(5) > 0) GO TO 1500
!-
!......NOW WORK OTHER ELEMENTS
!-
!......FIRST TEST IF FIRST TIME THROUGH WHICH IS AN ERROR
!-
      IF(NET /= N) GO TO 1335
!ipk sep04
      if(ierr /= 1) then     
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
      endif
      IERR=1
      WRITE(75,6090) N,(IS(K),K=1,6)
      WRITE(*,6090) N,(IS(K),K=1,6)
 6090 FORMAT(/5X,'SPECIFICATION ERROR FOR ELEMENT',I5/                  &
     &'  LAYERS AT CORNERS ARE AS FOLLOWS'/8I5)
      GO TO 2200
 1335 CONTINUE
!-
!......DETERMINE NUMBER OF SINGLE POINT CORNERS
!-
      DO 1380 J=1,5,2
      IF(ITT(J) == 0) GO TO 1380
        nwlp=NOP(N,J+9)
      N1=NRF(nwlp,J)
        nwlp=NOP(N,J+10)
      NM1=NRF(nwlp,J+1)
      JP=J+2
      IF(JP > 5) GO TO 1410
      DO 1375 I=JP,5,2
      IF(ITT(I) == 0) GO TO 1375
        nwlp=NOP(N,I+9)
      N2=NRF(nwlp,I)
      GO TO 1385
 1375 CONTINUE
      GO TO 1410
 1380 CONTINUE
!-
!......PROCESS 13 POINT ELEMENT
!-
 1385 CONTINUE
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
        nwlp=NOP(N,I+10)
      NM2=NRF(nwlp,I+1)
      NA=18-I-J
      IF(NA /= 12) GO TO 1386
      N3=N1
      N1=N2
      N2=N3
      N3=NM1
      NM1=NM2
      NM2=N3
 1386 CONTINUE
      NCORN(NET)=13
      NETYP(NET)=23
      NOP(NET,1)=N1+2
      NOP(NET,3)=N2+2
      NOP(NET,9)=N1
      NOP(NET,11)=N2
        nwlp=NOP(N,NA)
      NOP(NET,5)=NRF(nwlp,NA-9)
        nwlp=NOP(N,NA+1)
      NM3=NRF(nwlp,NA-8)
      NOP(NET,10)=NM1
      NOP(NET,12)=NM2
      NOP(NET,13)=NM3
      NOP(NET,7)=N1+1
      NOP(NET,8)=N2+1
      NOP(NET,6)=NM3+1
      NOP(NET,2)=NM1+1
      NOP(NET,4)=NM2+1
      DO 1390 I=1,6
      IF(I == NA) GO TO 1390
      ITT(I)=ITT(I)-1
      IF(MOD(I,2) == 1) ITT(I)=ITT(I)-1
 1390 CONTINUE
      IF(NET == N) NET=NES
      NET=NET+1
      IF(NET > MAXE) GO TO 2500
      GO TO 1330
!-
!......PROCESS 10 POINT ELEMENT
!-
 1410 CONTINUE
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      NCORN(NET)=10
      NETYP(NET)=24
      NOP(NET,1)=N1+2
      NOP(NET,10)=N1
      NOP(NET,8)=NM1
      NA=MOD(J+2,6)+9
        nwlp=NOP(N,NA)
      NOP(NET,3)=NRF(nwlp,NA-9)
        nwlp=NOP(N,NA+1)
      NOP(NET,4)=NRF(nwlp,NA-8)
      NA=MOD(J+4,6)+9
        nwlp=NOP(N,NA)
      NOP(NET,5)=NRF(nwlp,NA-9)
        nwlp=NOP(N,NA+1)
      NM2=NRF(nwlp,NA-8)
      NOP(NET,9)=NM2
      NOP(NET,7)=N1+1
      NOP(NET,6)=NM2+1
      NOP(NET,2)=NM1+1
      ITT(J)=ITT(J)-2
      ITT(J+1)=ITT(J+1)-1
      ITT(NA-8)=ITT(NA-8)-1
      IF(NET == N) NET=NES
      NET=NET+1
      IF(NET > MAXE) GO TO 2500
      GO TO 1330
!-
!......PROCESS 15 POINT ELEMENT
!-
 1500 CONTINUE
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      NCORN(NET)=15
      NETYP(NET)=22
        nwlp=NOP(N,10)
      N1=NRF(nwlp,1)
        nwlp=NOP(N,12)
      N2=NRF(nwlp,3)
        nwlp=NOP(N,14)
      N3=NRF(nwlp,5)
        nwlp=NOP(N,11)
      NM1=NRF(nwlp,2)
        nwlp=NOP(N,13)
      NM2=NRF(nwlp,4)
        nwlp=NOP(N,15)
      NM3=NRF(nwlp,6)
      IF(NET == N) GO TO 1550
      NOP(NET,10)=N1
      NOP(NET,12)=N2
      NOP(NET,14)=N3
      NOP(NET,11)=NM1
      NOP(NET,13)=NM2
      NOP(NET,15)=NM3
 1550 NOP(NET,1)=N1+2
      NOP(NET,3)=N2+2
      NOP(NET,5)=N3+2
      NOP(NET,2)=NM1+1
      NOP(NET,4)=NM2+1
      NOP(NET,6)=NM3+1
      NOP(NET,7)=N1+1
      NOP(NET,8)=N2+1
      NOP(NET,9)=N3+1
      IF(NET == N) NET=NES
      NET=NET+1
      IF(NET > MAXE) GO TO 2500
      DO 1560 I=1,6
      ITT(I)=ITT(I)-1
      IF(MOD(I,2) == 1) ITT(I)=ITT(I)-1
 1560 CONTINUE
      GO TO 1330
 1600 CONTINUE
!-
!......SET UP QUADRILATERAL ELEMENTS
!-
      DO 1610 M=1,8
      K=NOP(N,M)
      ITT(M)=NDEP(K)
      IF(MOD(M,2) == 1) ITT(M)=ITT(M)-1
      IS(M)=ITT(M)
 1610 CONTINUE
 1615 CONTINUE
      IF(NET == N) GO TO 1620
!-
!...... Find a gap in NFIXH and fill it
!-
 1617 IF(NFIXH(IHH) == 0) GO TO 1618
      IHH=IHH+1
      GO TO 1617
 1618 CONTINUE
      NFIXH(IHH)=NET
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      TH(NET)=TH(N)
!
      DO 1619 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 1619 CONTINUE
!
 1620 NCORN(NET)=20
      NETYP(NET)=21
        nwlp=NOP(N,13)
      N1=NRF(nwlp,1)
        nwlp=NOP(N,15)
      N2=NRF(nwlp,3)
        nwlp=NOP(N,17)
      N3=NRF(nwlp,5)
        nwlp=NOP(N,19)
      N4=NRF(nwlp,7)
        nwlp=NOP(N,14)
      NM1=NRF(nwlp,2)
        nwlp=NOP(N,16)
      NM2=NRF(nwlp,4)
        nwlp=NOP(N,18)
      NM3=NRF(nwlp,6)
        nwlp=NOP(N,20)
      NM4=NRF(nwlp,8)
      IF(NET == N) GO TO 1625
      NOP(NET,13)=N1
      NOP(NET,14)=NM1
      NOP(NET,15)=N2
      NOP(NET,16)=NM2
      NOP(NET,17)=N3
      NOP(NET,18)=NM3
      NOP(NET,19)=N4
      NOP(NET,20)=NM4
 1625 CONTINUE
      NOP(NET,9)=N1+1
      NOP(NET,10)=N2+1
      NOP(NET,11)=N3+1
      NOP(NET,12)=N4+1
      NOP(NET,1)=N1+2
      NOP(NET,2)=NM1+1
      NOP(NET,3)=N2+2
      NOP(NET,4)=NM2+1
      NOP(NET,5)=N3+2
      NOP(NET,6)=NM3+1
      NOP(NET,7)=N4+2
      NOP(NET,8)=NM4+1
      IF(NET == N) NET=NES
      GO TO 1710
 1650 CONTINUE
!-
!...... Find a gap in NFIXH and fill it
!-
 1652 IF(NFIXH(IHH) == 0) GO TO 1654
      IHH=IHH+1
      GO TO 1652
 1654 CONTINUE
      NFIXH(IHH)=NET
      TH(NET)=TH(N)
!
      DO 1656 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 1656 CONTINUE
!
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      DO 1660 I=1,7,2
      IF(ITT(I) < 0) ITT(I)=ITT(I)+2
      IF(ITT(I+1) == 0) ITT(I+1)=ITT(I+1)+1
 1660 CONTINUE
      NCORN(NET)=15
      NETYP(NET)=22
        nwlp=NOP(N,ILOC+12)
      N1=NRF(nwlp,ILOC)
        nwlp=NOP(N,ILOC+13)
      NM1=NRF(nwlp,ILOC+1)
      ILOC=MOD(ILOC+2,8)
        nwlp=NOP(N,ILOC+12)
      N2=NRF(nwlp,ILOC)
        nwlp=NOP(N,ILOC+13)
      NM2=NRF(nwlp,ILOC+1)
      ILOC=MOD(ILOC+2,8)
        nwlp=NOP(N,ILOC+12)
      N3=NRF(nwlp,ILOC)
        nwlp=NOP(N,ILOC+13)
      NM3=NRF(nwlp,ILOC+1)
      ILOC=MOD(ILOC+2,8)
        nwlp=NOP(N,ILOC+12)
      N4=NRF(nwlp,ILOC)
        nwlp=NOP(N,ILOC+13)
      NM4=NRF(nwlp,ILOC+1)
      NOP(NET, 1)=N1
      NOP(NET, 2)=NM1
      NOP(NET, 3)=N2
      NOP(NET, 4)=N2+1
      NOP(NET, 5)=N2+2
      NOP(NET, 6)=NM1+1
      NOP(NET, 7)=NM4
      NOP(NET, 8)=NM2
      NOP(NET, 9)=NM2+1
      NOP(NET,10)=N4
      NOP(NET,11)=NM3
      NOP(NET,12)=N3
      NOP(NET,13)=N3+1
      NOP(NET,14)=N3+2
      NOP(NET,15)=NM3+1
 1710 CONTINUE
      IF(NET == N) NET=NES
      NET=NET+1
      IF(NET > MAXE) GO TO 2500
      DO 1720 I=1,8
      ITT(I)=ITT(I)-1
      IF(MOD(I,2) == 1) ITT(I)=ITT(I)-1
 1720 CONTINUE
      ISETS=0
      ISETZ=0
      DO 1730 I=2,8,2
      IF(ITT(I-1) > 1) ISETS=ISETS+2
      IF(ITT(I) > 1) GO TO 1730
      ISETZ=ISETZ+1
      ILOC=MOD(I+1,8)
 1730 CONTINUE
      IF(ISETZ == 0 .AND. ISETS == 8) GO TO 1615
      IF(ISETZ == 1 .AND. ISETS == 4) GO TO 1650
      IF(ISETZ == 4) GO TO 2200
!ipk sep04
      if(ierr /= 1) then     
        CLOSE(75)
        OPEN(75,FILE='ERROR.OUT')
      endif
      WRITE(75,6090) N,(IS(K),K=1,8)
      WRITE(*,6090) N,(IS(K),K=1,8)
      IERR=1
      GO TO 2200
 1800 CONTINUE
      M=2
!-
!......FORM TOP SIDE RECTANGLE
!-
      J2=NOP(N,M)
      IF(IBN(J2) == 0) GO TO 2080
      J1=NOP(N,M-1)
      J3=NOP(N,M+1)
      IF( NET > MAXE) GO TO 2500
      NCORN(NET)=8
      IF(NETYP(N) == 18) THEN
        NETYP(NET)=18
      ELSE
        NETYP(NET)=15
      ENDIF
      IMAT(N)=5000+IMAT(N)
      ELAREA(NET)=ELAREA(N)
      TH(NET)=TH(N)
!
      DO 1801 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 1801 CONTINUE
!
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      J4=NOP(N,4)
      J5=NOP(N,5)
      J6=NOP(N,19)
      N1=NREF(J1)
      N2=NREF(J2)
      N3=NREF(J3)
      NOP(NET,1)=J1
      NOP(NET,2)=N1+1
      NOP(NET,3)=N1+2
!ipk jan99
      IF(JPOINT(J1) /= 0) THEN
        JJ4=ABS(JPOINT(J1))
        NN4=NREF(JJ4)
        NOP(NET,12)=JJ4
        NOP(NET,13)=NREF(JJ4)+1
        NOP(NET,14)=NREF(JJ4)+2
      ENDIF
!
      NOP(NET,4)=N2+1
      NOP(NET,5)=N3+2
      NOP(NET,6)=N3+1
      NOP(NET,7)=J3
!ipk jan99
!
      IF(JPOINT(J3) /= 0) THEN
        JJ4=ABS(JPOINT(J3))
        NN4=NREF(JJ4)
        NOP(NET,18)=JJ4
        NOP(NET,17)=NREF(JJ4)+1
        NOP(NET,16)=NREF(JJ4)+2
      ENDIF
!
      NOP(NET,8)=J2
      IS(1)=NDEP(J1)
      IS(2)=NDEP(J2)
      IS(3)=NDEP(J3)
      ITT(1)=IS(1)-2
      ITT(2)=IS(2)-1
      ITT(3)=IS(3)-2
      IF(NETYP(NET) == 18) THEN
        N4=NREF(J4)
        N5=NREF(J5)
        N6=NREF(J6)
        NOP(NET,9)=J4
        NOP(NET,10)=N4+1
        NOP(NET,11)=N4+2
        NOP(NET,12)=J5
        NOP(NET,13)=N5+1
        NOP(NET,14)=N5+2
        NOP(NET,19)=J6
        NOP(NET,20)=N6+1
        IS(4)=NDEP(J4)
        IS(5)=NDEP(J5)
        IS(6)=NDEP(J6)
        ITT(4)=IS(4)-2
        ITT(5)=IS(5)-2
        ITT(6)=IS(6)-1
      ENDIF
      NET=NES
 1975 NET=NET+1
      IF(NET > MAXE) GO TO 2500
      IF(ITT(1) < 2) GO TO 1980
      IF(ITT(3) < 2) GO TO 2000
!-
!......FORM OTHER SIDE RECTANGLE
!-
      NCORN(NET)=8
      IF(NETYP(N) == 18) THEN
        NETYP(NET)=18
      ELSE
        NETYP(NET)=15
      ENDIF
!-
!...... Find a gap in NFIXH and fill it
!-
 1977 IF(NFIXH(IHH) == 0) GO TO 1978
      IHH=IHH+1
      GO TO 1977
 1978 CONTINUE
      NFIXH(IHH)=NET
      TH(NET)=TH(N)
!
      DO 1979 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 1979 CONTINUE
!
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      NOP(NET,1)=N1+IS(1)-ITT(1)
      NOP(NET,2)=NOP(NET,1)+1
      NOP(NET,3)=NOP(NET,1)+2
!ipk jan99
!
      IF(JPOINT(J1) /= 0) THEN
        NOP(NET,12)=NN4+IS(1)-ITT(1)
        NOP(NET,13)=NOP(NET,12)+1
        NOP(NET,14)=NOP(NET,12)+2
      ENDIF
!
      NOP(NET,8)=N2+IS(2)-ITT(2)
      NOP(NET,4)=NOP(NET,8)+1
      NOP(NET,7)=N3+IS(3)-ITT(3)
      NOP(NET,6)=NOP(NET,7)+1
      NOP(NET,5)=NOP(NET,7)+2
!ipk jan99
!
      IF(JPOINT(J3) /= 0) THEN
        NOP(NET,18)=NN4+IS(3)-ITT(3)
        NOP(NET,17)=NOP(NET,18)+1
        NOP(NET,16)=NOP(NET,18)+2
      ENDIF
!
      IF(NETYP(NET) == 18) THEN
!        J4=NOP(N,4)
!        J5=NOP(N,5)
!        J6=NOP(N,19)
!        N4=NREF(J4)
!        N5=NREF(J5)
!        N6=NREF(J6)
        NOP(NET,9)=N4+IS(4)-ITT(4)
        NOP(NET,10)=NOP(NET,9)+1
        NOP(NET,11)=NOP(NET,9)+2
        NOP(NET,12)=N5+IS(5)-ITT(5)
        NOP(NET,13)=NOP(NET,12)+1
        NOP(NET,14)=NOP(NET,12)+2
        NOP(NET,19)=N6+IS(6)-ITT(6)
        NOP(NET,20)=NOP(NET,19)+1
        ITT(4)=ITT(4)-2
        ITT(5)=ITT(5)-2
        ITT(6)=ITT(6)-1
      ENDIF
      ITT(1)=ITT(1)-2
      ITT(2)=ITT(2)-1
      ITT(3)=ITT(3)-2
      GO TO 1975
!-
!......FORM SIDE TRIANGLE
!-
 1980 CONTINUE
      IF(ITT(3) < 2) GO TO 2080
      NCORN(NET)=6
      NETYP(NET)=15
!-
!...... Find a gap in NFIXH and fill it
!-
 1985 IF(NFIXH(IHH) == 0) GO TO 1987
      IHH=IHH+1
      GO TO 1985
 1987 CONTINUE
      NFIXH(IHH)=NET
      TH(NET)=TH(N)
!
      DO 1986 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 1986 CONTINUE
!
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
      NOP(NET,1)=N1+IS(1)-ITT(1)
      NOP(NET,6)=N2+IS(2)-ITT(2)
      NOP(NET,2)=NOP(NET,6)+1
      NOP(NET,5)=N3+IS(3)-ITT(3)
      NOP(NET,4)=NOP(NET,5)+1
      NOP(NET,3)=NOP(NET,5)+2
!ipk jan99
!
      IF(JPOINT(J3) /= 0) THEN
        NOP(NET,18)=NN4+IS(3)-ITT(3)
        NOP(NET,17)=NOP(NET,18)+1
        NOP(NET,16)=NOP(NET,18)+2
      ENDIF
!
      ITT(2)=ITT(2)-1
      ITT(3)=ITT(3)-2
      GO TO 1975
!-
!......FORM OTHER SHAPE TRIANGLE
!-
 2000 CONTINUE
      K=K+1
      NCORN(NET)=6
      NETYP(NET)=15
!-
!...... Find a gap in NFIXH and fill it
!-
 2020 IF(NFIXH(IHH) == 0) GO TO 2025
      IHH=IHH+1
      GO TO 2020
 2025 CONTINUE
      NFIXH(IHH)=NET
      TH(NET)=TH(N)
      IMAT(NET)=IMAT(N)
      ELAREA(NET)=ELAREA(N)
!
      DO 2026 II=1,6
         EEXXYY(II,NET) = EEXXYY(II,N)
 2026 CONTINUE
!
      NOP(NET,1)=N1+IS(1)-ITT(1)
      NOP(NET,2)=NOP(NET,1)+1
      NOP(NET,3)=NOP(NET,1)+2
!ipk jan99
!
      IF(JPOINT(J1) /= 0) THEN
        NOP(NET,12)=NN4+IS(1)-ITT(1)
        NOP(NET,13)=NOP(NET,12)+1
        NOP(NET,14)=NOP(NET,12)+2
      ENDIF
!
      NOP(NET,6)=N2+IS(2)-ITT(2)
      NOP(NET,4)=NOP(NET,6)+1
      NOP(NET,5)=N3+IS(3)-1
      ITT(1)=ITT(1)-2
      ITT(2)=ITT(2)-1
      GO TO 1975
 2080 CONTINUE
      GO TO 2200
!-
!...... Form element for junction
!-
 2090 CONTINUE
      NED=NOP(N,1)
      NED=NDEP(NED)
      NET=NES+1
      IF(NED > 2) THEN
        NIND=NCORN(N)
        DO 2130 K=2,NED
          IF(NET > MAXE) GO TO 2500
!-
!...... Find a gap in NFIXH and fill it
!-
 2100 IF(NFIXH(IHH) == 0) GO TO 2110
      IHH=IHH+1
      GO TO 2100
 2110 CONTINUE
          NFIXH(IHH)=NET
          NCORN(NET)=NCORN(N)
          NIND=0
          DO 2120 L=1,NCORN(N)
            N1=NOP(N,L)
            N2=NREF(N1)+K-1
            NIND=NIND+1
            NOP(NET,NIND)=N2
 2120     CONTINUE
          IMAT(NET)=IMAT(N)
          NCORN(NET)=NCORN(N)
          NETYP(NET)=17
          NET=NET+1
 2130   CONTINUE
        NETYP(N)=17
        GO TO 2200
      ELSE
        NETYP(N)=7
      ENDIF
 2150 NET=NES+1
      IF(NETYP(N) == 18) NETYP(N)=8
 2200 CONTINUE
      NE=NET-1
      IF(IERR == 1) STOP
      RETURN
 2500 WRITE(*,6400) NET,MAXE,N
      WRITE(75,6400) NET,MAXE,N
 6400 format('ERROR STOP  -  3-D EXPANSION LEADS TO EXCESSIVE NUMBER'   &
     &' OF ELEMENTS'/'ELEMENTS GENERATED =',I8,'  ELEMENTS ALLOWED ='I8/&
     &'2-D ELEMENT BEING PROCESSED =',I8)
      STOP
      END
