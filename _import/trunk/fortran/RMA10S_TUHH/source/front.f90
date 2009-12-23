!     Last change:  MD   20 May 2009    6:16 pm
!IPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
!IPK  LAST UPDATE MAR 25 2005
!IPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
!IPK  LAST UPDATE FEB 10 2004 IMPROVE DIAGNOSTIC
!IPK  LAST UPDATE DEC 16 2003 ADD IEDSW DEPENDENCE
!IPK  LAST UPDATE MAR 18 2003 add diffusion switch ( default of  0 uses old formulations
!ipk  last update Mar 22 2001 revise to keep solution and NBC for equation dropout
!IPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
!ipk  last update Nov 18 1999 add capability to collapse elements to 2-D
!ipk  last update Feb 4 1998 reduce output
!ipk  last update Jan 21 97  add option for Smagorinksy in 2-D
      SUBROUTINE FRONT(NRX)
      USE BLK10
      USE BLK10MOD
      USE BLKSANMOD
      USE BLKECOM
!nis,feb07,testing      
      USE ParaKalyps
      USE Para1DPoly
!-      
      SAVE
!
!
!     FRONTAL ELIMINATION ROUTINE USING FULL PIVOTING
!
      INTEGER NK(120)
!
!IPK AUG05       DIMENSION QR(MFW),NCON(20)
!IPK AUG07      DIMENSION NCON(20)
!-
!IPK AUG05      COMMON/BIGONE/ EQ(MFW,MFW),LHED(MFW),QQ(MFW),PVKOL(MFW)
!-
!     LBMAX=MBUF
!
!local copy of the actual nbn of the element      
      INTEGER                       :: temp_nbn
      INTEGER                       :: nod, degree
!
      NMAX = MFW
      CALL SECOND(ASEC)
      ISHRK=0
      TSURC=0
      TCOEFS=0
      TCOEF2=0
      TCOEF1=0
!
!IPK MAR0 setup switch that says salinity is active
!
      IF(ITEQV(MAXN) == 2 .OR. ITEQV(MAXN) == 8                         &
     &                      .OR. ITEQV(MAXN) == 9) THEN
        IF(IDIFSW == 0) THEN
          ISLP=0
        ELSE
          ISLP=1
        ENDIF
      ELSE
        ISLP=0
      ENDIF
!
      WRITE(ICFL,6198) ASEC
 6198 FORMAT(/10X,' TIME STARTING FRONT =',F10.3)
 6199 FORMAT( /10X,'DELTA T =',F10.3, '  TOTAL T =',F10.3)
      WRITE(*,*)                                                        &
     &'ELEMENTS-PROCESSED EQNS-PROCESSED CURRENT-FRONT MAX-FRONT'       &
     &,' PRESENT-BUFFER'
      NEC=0
      IRTC=0
      LQ=0
      LCMAX=0
!
!     PREFRONT
!
!
!...... Find last appeareance of each node moved from LOAD3
!
!nis,jun07: Initializing it from the beginning of the array      
!DO J=1,NSZF      
      DO J = 0, NSZF
        NLSTEL (J) = 0
      ENDDO
!
      K = NESAV + 1
!
      AssignNLSTEL: DO NN = 1, NESAV
!run through all elements, using the reordering number and starting with the highest. Purpose is to get the element, that is solved        
!as the last of all the elements, a degree of freedom is connected to.        
        K = K - 1
        N = NFIXH (K)
        IF (N > 0 .AND. n <= NE) THEN
          IF (ICOLLAPE (N) == 1 .AND. IMAT (N)/1000 /= 1)               &
     &      CYCLE AssignNLSTEL
!
          IF (IMAT (N) > 0) THEN
            ncn = 20
            IF (ITEQV (MAXN) == 5) THEN
              DO I = 1, 8
                NCON (I) = NOPS (N, I)
                IF (NCON (I) > 0) NCN = I
              ENDDO
            ELSE
              DO I = 1, NCN
                NCON (I) = NOP (N, I)
              ENDDO
            ENDIF
            MRC = N
!
!
            DO M = 1, NCN
              L = NCON (M)
!IPK JAN99 SKIP OUT FOR 2DV
              if (l > 0) THEN
                DO I = 1, 7
                  J = NBC (L, I)
                  IF (J /= 0) THEN
                    IF (NLSTEL (J) == 0) THEN
                      NLSTEL (J) = MRC
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO AssignNLSTEL
!NLSTEL (J) : Stores the nfixh-number of the element with the highest nfixh-number, where the degree of freedom J is connected to.      
!
      NELL=0
      NELM=0
!
!     ASSEMBLY
!
!Initializations      
!---------------      
      init: DO N = 1, NSZF
        IPOINT (N) = 0
        R1 (N) = 0.
      end do init
!-
!......ESTABLISH DENSITIES AND PRESSURES
!-
      IF (NRX == 1) THEN
        CALL PRESR
        NDF = 4
      ENDIF
      LCOL = 0
!
!
!Starting the assembly of the equations      
!--------------------------------------      
!
   18 NELL=NELL+1
      NELM=NELM+1
!
!jump out, if maximum element number is reached      
!exit
      IF (NELL > NE) GO TO 380    
!get element ID to process      
      N = NFIXH(NELM)
!special cases to cycle or to exit      
!exit
      IF (N == 0)  GO TO 380      
!cycle
      IF(IMAT(N) < 1) GO TO 18 
!monitor time consumption      
      CALL SECOND(SINC)
!
!IPK DEC03 ADD IEDSW DEPENDENCE
!get material dependent behaviour; due to turbulence model      
      if (imat (n) /= 89) then
        NMATYP = (MOD (IMAT (N), 1000))
        IEDSW = IEDSW1 (NMATYP)
        TBFACT = TBFACT1 (NMATYP)
        TBMIN = TBMIN1 (NMATYP)
      endif
!
!
!---------------------------
!Calling proper coef routine
!---------------------------
!collapse from 3D to 2D      
      IF(ITEQV(MAXN) /= 5) THEN
        IF(IMAT(N) > 1000 .AND. IMAT(N) < 5000) THEN
          IF(NRX == 2) GO TO 18
!IPK NOV99     Either process surface integrals or collapse to 2-d
        IF(ICOLLAPE(N) == 0) THEN
          CALL SURCOF(N,NRX)
        ELSEIF(IMAT(N) < 2000) THEN
          IF(NETYP(N)/10 < 1) THEN
!IPK MAR05
            IF(INOTR == 0) THEN
              CALL COEF1(N,NRX)
            ELSE
              CALL COEF1NT(N,NRX)
            endif
!     Modify tests to allows for IDIFSW
          ELSEIF(IUTUB == 1 .AND. iedsw == 2 .AND. ISLP == 0) THEN
!IPK MAR05
            IF(INOTR == 0) THEN
              CALL COEF2D(N,NRX)
            ELSE
              if(nell <= 2) then
                write(*,*) ' entering COEF2DNT'
              endif
              CALL COEF2DNT(N,NRX)
            ENDIF
!
!MD: Should only be used for Kings-Turbulence enclosure 2 (SMAG)          
!MD:  combined with Dispersion enclosure 2. Reason for this change          
!MD:  to avoid a switch between different COEF-Routines for one model          
          ELSEIF(ISLP == 1 .AND. IUTUB == 1 .AND. IDIFSW == 2           &
     &             .AND. IEDSW == 2) THEN
!IPK MAR05
            IF(INOTR == 0) THEN
              CALL COEF2D(N,NRX)
            ELSE
              if(nell <= 2) then
                write(*,*) ' entering COEF2DNT'
              endif
              CALL COEF2DNT(N,NRX)
            ENDIF
              ELSE
!IPK MAR05
            IF(INOTR == 0) THEN
              CALL COEF2(N,NRX)
            ELSE
              if(nell <= 2)  then
                write(*,*) ' entering COEF25NT'
              endif
              CALL COEF2NT(N,NRX)
            ENDIF
          ENDIF
        ELSE
            GO TO 18
          ENDIF
          CALL SECOND(SOUC)
          TSURC=SOUC-SINC+TSURC
!
        ELSE
          IF(NETYP(N)/10 == 2) THEN
!
!ipk nov99 skip if collapsing to 2-d
!
            IF(ICOLLAPE(N) == 1 .AND. NRX /= 2) GO TO 18
!
!     Process   threed element
!ipk jan97
            if(iutub == 1 .AND. IEDSW == 2) then
              CALL COEF3D(N,NRX)
            else
              call coef3(n,nrx)
            endif
!ipk jan97 end changes
            CALL SECOND(SOUC)
            TCOEFS=TCOEFS+SOUC-SINC
          ELSE
            IF(NETYP(N)/10 == 1) THEN
!
!     Process   twod elements
!
!IPK JUN05
              IF(IMAT(N) < 900 .OR. NCORN(N) > 5) THEN
!
!     Process   horizontal 2d
!
                IF(NRX == 2) GO TO 18
!ipk jan97
                if(iutub == 1 .AND. IEDSW == 2 .AND. ISLP == 0)         &
     &            then
                  if(nell == 1) then
                    write(75,*) 'going to smag'
                  endif
!IPK MAR05
                  IF(INOTR == 0) THEN
                    CALL COEF2D(N,NRX)
                  ELSE
                    if(nell <= 2) then
                      write(*,*) ' entering COEF2DNT'
                    endif
                    CALL COEF2DNT(N,NRX)
!
                  ENDIF
!
!MD: Should only be used for Kings-Turbulence enclosure 2 (SMAG)                
!MD:  combined with Dispersion enclosure 2. Reason for this change                
!MD:  to avoid a switch between different COEF-Routines for one model                
                ELSEIF(ISLP == 1 .AND. IUTUB == 1 .AND.                 &
     &                 IDIFSW == 2. .AND. IEDSW == 2) THEN
!IPK MAR05
                  IF(INOTR == 0) THEN
                    CALL COEF2D(N,NRX)
                  ELSE
                    if(nell <= 2) then
                      write(*,*) ' entering COEF2DNT'
                    endif
                    CALL COEF2DNT(N,NRX)
                  ENDIF
                else
!IPK MAR05
                  IF(INOTR == 0) THEN
                    CALL COEF2(N,NRX)
                  ELSE
                    if(nell <= 2)then
                      write(*,*) ' entering COEF25NT'
                    endif
                    CALL COEF2NT(N,NRX)
                  ENDIF
                endif
!ipk jan97 end changes
              ELSE
!IPK JAN99
                IF(IMAT(N) > 900 .AND. IMAT(N) < 1000) GO TO 18
!
!      Process vertical 2d
!
!ipk nov99 skip if collapsing to 2-d
                IF(ICOLLAPE(N) == 1 .AND. NRX /= 2) GO TO 18
!
                CALL COEFV(N,NRX)
              ENDIF
              CALL SECOND(SOUC)
              TCOEF2=SOUC-SINC+TCOEF2
            ELSE
!
!      Process one-d elements
!
              IF(NRX == 2) GO TO 18
!IPK MAR05
!nis,may07
!
              IF ((imat(n) >= 901 .AND. imat(n) <= 903)                 &
     &            .AND. IGTP(n) == 0) then
                CALL Coef1DJunction (N, NRX)
!
!material type 89 is used for polynom approach              
              ELSEIF (imat(n) /= 89) THEN
                IF(INOTR == 0) THEN
                  CALL COEF1(N,NRX)
                ELSE
                  CALL COEF1NT(N,NRX)
                ENDIF
!use polynom approach              
              ELSEIF (imat(n) == 89) THEN
                CALL COEF1dPoly(N,NRX)
              ENDIF
!
!
              CALL SECOND(SOUC)
              TCOEF1=SOUC-SINC+TCOEF1
            ENDIF
          ENDIF
        ENDIF
      ELSE
!-
!...... The calls below are for the case of vertical averaging
!-
   19   IF(N <= NE) GO TO 20
        NELM=NELM+1
        N=NFIXH(NELM)
        GO TO 19
   20   CONTINUE
        IF(NOPS(N,6) > 0) THEN
!IPK MAR05
          IF(INOTR == 0) THEN
            CALL COEF2(N,NRX)
          ELSE
            if(nell <= 2)  then
              write(*,*) ' entering COEF25NT at goto_20 '
            endif
            CALL COEF2NT(N,NRX)
          ENDIF
          CALL SECOND(SOUC)
          TCOEF2=TCOEF2+SOUC-SINC
        ELSE
!IPK MAR05
          IF(INOTR == 0) THEN
            CALL COEF1(N,NRX)
          ELSE
            CALL COEF1NT(N,NRX)
          ENDIF
          CALL SECOND(SOUC)
          TCOEF1=TCOEF1+SOUC-SINC
        ENDIF
      ENDIF
!ipk jan99
!-----------------------------------
!End of calling proper coef routines
!-----------------------------------
!
      IF(NETYP(N) == 15 .OR. NETYP(N) == 16) THEN
        IF(NOP(N,14) /= 0) NCN=14
        IF(NOP(N,18) /= 0) NCN=18
      ENDIF
!
      NBN = NCN*NDF
      DO 21 LK=1,NBN
!NiS,may06: position of special degree of freedom (lk) in equation solution window
        LDEST(LK)=0 
!???
        NK(LK)=0 
   21 CONTINUE
!
!
      KC=0
      DO J=1,NCN
        IF(ITEQV(MAXN) == 5) THEN
          I=NOPS(N,J)
        ELSE
!NiS,may06: i becomes node number
          I=NOP(N,J) 
        ENDIF
!NiS,may06: for every degree of freedom
        inner: DO L=1,NDF 
!NiS,may06: count loop
          KC=KC+1     
!IPK JAN99
!NiS,may06: loop cycle, if node is zero
          if(i == 0) cycle inner 
!
!NiS,may06: LL becomes global equation number of the degree of freedom L at node I
          LL=NBC(I,L) 
!NiS,may06: NK saves the equation number of node-degree of freedom; KC runs from 1 to ncn*ndf
          NK(KC)=LL   
          IF(LL /= 0) THEN
!If the current element is the last one of the equation (NLSTEL), then make
            IF(NLSTEL(LL) == N) NK(KC)=-LL 
!           NK(KC) negative as pointer, that degree of freedom can be taken out                                             
          ENDIF
         end do inner
       end do
!
!
!-
!...... Set up heading vectors
!-
      LFZ=1
!NiS,may06: do for every node-degree-of-freedom; nbn=ndf*ncn
      DO 52 LK=1,NBN 
!NiS,may06: get equation number of node-degree-of-freedom
        nod=NK(LK)  
!NiS,may06: if equation number is deactivated, switch
        IF(nod == 0) GO TO 52 
!NiS,may06: get absolute number
        LM=IABS(nod) 
!NiS,may06: IPOINT is always zero at the first call
        LL=IPOINT(LM) 
        IF(LL /= 0) THEN
          LDEST(LK)=LL
          IF(nod < 0) LHED(LL)=nod
        ELSE
!
!     Look for vacant slot
!
          DO 35 L=LFZ,LCOL
            IF(LHED(L) == 0) THEN
              IPOINT(LM)=L
              LHED(L)=nod
              LDEST(LK)=L
              LFZZ=L
              ISHRK=ISHRK-1
              GO TO 40
            ENDIF
   35     CONTINUE
          LCOL=LCOL+1
          LDEST(LK)=LCOL
          LHED(LCOL)=nod
          IPOINT(LM)=LCOL
          LFZZ=LCOL
          DO 38 L=1,LCOL
            EQ(L,LCOL)=0.
            EQ(LCOL,L)=0.
   38     CONTINUE
   40     LFZ=LFZZ
        ENDIF
   52 CONTINUE
!
!
!IPK FEB04
!nis,dec06: if LHED(L) == 0, the loop should be cycled because of assignment problems      
      DO L=1,LCOL
!columnassigning: DO L=1,LCOL      
          LEQ=ABS(LHED(L))
!nis,dec06: see above          
!if (LEQ == 0) CYCLE columnassigning          
!-          
          IF(NLSTEL(LEQ) == N) LHED(L)=-ABS(LHED(L))
!columnassigning
      ENDDO 
!
      IF(LCOL > LCMAX) LCMAX=LCOL
!
!Write out console output during processing of elements (every 1000 elements)      
!----------------------------------------------------------------------------      
      IF (MOD (NELL, 1000) == 0) THEN
        WRITE(*,'(I18,I15,I14,I10,I15)') NELL, NEC, LCOL, LCMAX, LQ
      ENDIF
!
!
      IF(LCOL <= NMAX) GO TO 54
      NERROR=2
!IPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,417)NERROR
      WRITE(75,417)NERROR
      WRITE(*,6008)   NFIXH(NELL)
      WRITE(75,6008)   NFIXH(NELL)
 6008 FORMAT( // 10X, '..STOP AT ELEMENT', I10 )
      WRITE(75,9820) (LHED(L),L=1,LCOL)
      WRITE(75,9822) (EQ(L,L),L=1,LCOL)
!IPK FEB04
 9823   FORMAT('  OFFENDING EQN      NODE   DEG OF FR')
 9824   FORMAT(I15,I10,I12)
        WRITE(75,9823)
        DO L=1,LCOL
          NEQN=ABS(LHED(L))
          IF(NEQN > 0) THEN
            DO N=1,NP
              DO M=1,3
                IF(NBC(N,M) == NEQN) THEN
                  WRITE(75,9824) NEQN,N,M
                  GO TO 53
                ENDIF 
              ENDDO
            ENDDO
          ENDIF
   53     CONTINUE
        ENDDO
      WRITE(75,9821) (N,(NBC(N,M),M=1,4),N=1,NP)
      STOP
   54 CONTINUE
!
!NiS,jun06,com: Assembly of global matrix within the solution window      
!nis,jun06,com: for every nodal degree of freedom (row of element matrix)      
      DO 57 L=1,NBN
!nis,jun06,com: if equation is present        
        IF(NK(L) /= 0) THEN
!nis,jun06,com: take the solution window slot          
          LL=LDEST(L)
!nis,jun06,com: then take again every nodal degree of freedom (column of element matrix)          
          DO 56 K=1,NBN
            IF(NK(K) /= 0) THEN
            KK=LDEST(K)
            EQ(LL,KK)=EQ(LL,KK)+ESTIFM(K,L)
            ENDIF
   56     CONTINUE
        ENDIF
   57 CONTINUE
!nis,feb07,testing (stop for eq-output)      
!pause      
!-      
!
!     FIND OUT WHICH MATRIX ELEMENTS ARE FULLY SUMMED
!
   60 LPIVCO=0
      PIVOT=0.
      DO 64 L=1,LCOL
        IF(LHED(L) > -1) GO TO 64
!     WRITE(*,*) 'NELL,LHED,EQ',NELL,LHED(L),EQ(L,L)
        PIVA=EQ(L,L)
        IF(ABS(PIVA) < ABS(PIVOT)) GO TO 64
        PIVOT=PIVA
        LPIVCO=L
   64 CONTINUE
      IF(LPIVCO == 0) GO TO 18
!     WRITE(*,*) 'LPIVCO,PIVOT',LPIVCO,PIVOT
      IF( ABS(PIVOT) < 1.0E-8 .AND. NELL <= NE ) GO TO 18
!
!     NORMALISE PIVOTAL ROW
!
      LCO=IABS(LHED(LPIVCO))
!     IF(ABS(PIVOT) < 1E-08)WRITE(ICFL,476)
!TEMP     pivtin=1.0/pivot
      DO 80 L=1,LCOL
        QQ(L)=EQ(L,LPIVCO)/PIVOT
!TEMP        QQ(L)=EQ(L,LPIVCO)*pivtin
        QR(L)=EQ(LPIVCO,L)
   80 CONTINUE
      QR(LPIVCO)=0.0
      QQ(LPIVCO)=0.0
      RHS=R1(LCO)/PIVOT
!TEMP      RHS=R1(LCO)*pivtin
      R1(LCO)=RHS
      PVKOL(LPIVCO)=PIVOT
!
!     ELIMINATE THEN DELETE PIVOTAL ROW AND COLUMN
!
      DO 100 K=1,LCOL
        IF(QR(K) == 0.) GO TO 100
        KRW=IABS(LHED(K))
        R1(KRW)=R1(KRW)-QR(K)*RHS
        DO 90 L=1,LCOL
          EQ(L,K)=EQ(L,K)-QR(K)*QQ(L)
   90   CONTINUE
  100 CONTINUE  
!
!     WRITE PIVOTAL EQUATION ON DISC
!
      NEC=NEC+1
      LCS(NEC)=LCOL
      LPS(NEC)=LPIVCO
      DO 105 L=1,LCOL
        LQ=LQ+1
        LHS(LQ)=LHED(L)
        QS(LQ)=QQ(L)
  105 CONTINUE
      IF(LQ < LBMAX-NMAX) GO TO 108
      CALL XWRT(ND1,-1,NRR)
      IRTC=IRTC+1
      LQ=0
 108  CONTINUE
      DO 109 L=1,LCOL
        EQ(L,LPIVCO)=0.
        EQ(LPIVCO,L)=0.
  109 CONTINUE
      LHED(LPIVCO)=0
!
!     REARRANGE HEADING VECTORS
!
!      IF(ISHRK > LCOL/10+4) THEN
       IF(ISHRK >= 40) THEN
!
!AUG93  Changes start here
!       KM=0
!       DO 120 K=1,LCOL
!         IF(LHED(K) /= 0) THEN
!           KM=KM+1
!           LM=0
!           DO 110 L=1,LCOL
!             IF(LHED(L) /= 0) THEN
!               LM=LM+1
!               EQ(LM,KM)=EQ(L,K)
!             ENDIF
! 110       CONTINUE
!         ENDIF
! 120   CONTINUE
!       KM=0
!       DO 125 K=1,LCOL
!         IF(LHED(K) /= 0) THEN
!           KM=KM+1
!           LHED(KM)=LHED(K)
!           IPOINT(ABS(LHED(K)))=KM
!         ENDIF
! 125   CONTINUE
!       ISHRK=0
!       LCOL=KM
        KM=0
        DO 110 K=1,LCOL
          IF(LHED(K) /= 0) THEN
            KM=KM+1
            LHED(KM)=LHED(K)
            IPOINT(ABS(LHED(K)))=KM
            LDEST(KM)=K
          ENDIF
  110   CONTINUE
        LCOL=KM
!
        DO 120 K=1,LCOL
            KM=LDEST(K)
            DO 115 L=1,LCOL
              LM=LDEST(L)
              EQ(L,K)=EQ(LM,KM)
  115       CONTINUE
  120   CONTINUE
        ISHRK=0
!
!AUG93 changes end here
      ELSE
        ISHRK=ISHRK+1
      ENDIF
!
!     DETERMINE WHETHER TO ASSEMBLE,ELIMINATE,OR BACKSUBSTITUTE
!
      IF( LCOL > 0) GO TO 60
      IF( NELL < NE) GO TO 18
  380 CONTINUE
      IF(LCOL > 0) THEN
        DO 400 L=1,LCOL
          IF(LHED(L) /= 0) GO TO 405
  400   CONTINUE
        GO TO 420
  405   CONTINUE
!IPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
!
        WRITE(75,479) LCOL,ISHRK
        WRITE(*,479) LCOL,ISHRK
  479   FORMAT( '  UNSATISFIED ELIMINATION ERROR STOP'/                 &
     &  '   LCOL =',I5,' ISHRK =',I5)
        WRITE(75,9820) (LHED(L),L=1,LCOL)
        WRITE(75,9822) (EQ(L,L),L=1,LCOL)
 9820   FORMAT('  CONTENTS OF LHED ARE'/(5I8))
        WRITE(75,9821) (N,(NBC(N,M),M=1,4),N=1,NP)
 9821   FORMAT('  NBC ARRAY IS '/(5I8))
 9822   FORMAT('  EQ IS'/(1P5E12.4))
        CALL ZVRS(1)
        STOP 'Unsatisfied elimination error'
      ENDIF
  420 CONTINUE
      TTOTA=TSURC+TCOEF2+TCOEFS+TCOEF1
      WRITE(*,7804) TCOEF1,TSURC,TCOEF2,TCOEFS,TTOTA,LCMAX,IRTC,LQ
      WRITE(75,7804) TCOEF1,TSURC,TCOEF2,TCOEFS,TTOTA,LCMAX,IRTC,LQ
 7804 FORMAT(20X,'TIME IN 1-DIM   ELEMENTS',F10.3/                      &
     &       20X,'TIME IN SURFACE ELEMENTS',F10.3/                      &
     &       20X,'TIME IN 2-DIM   ELEMENTS',F10.3/                      &
     &       20X,'TIME IN 3-DIM   ELEMENTS',F10.3/                      &
     &       20X,'TOTAL TIME IN   ELEMENTS',F10.3/                      &
     &       20X,'MAXIMUM FRONT WIDTH     ',I10/                        &
     &       20X,'BUFFER BLOCKS WRITTEN   ',I10/                        &
     &       20x,'FINAL LQ SIZE           ',I10)
      CALL SECOND(TA)
      ADT=TA-ASEC
      WRITE(75,6199) ADT,TA
      WRITE(*,6199) ADT,TA
!      WRITE(LOUT,7805) ADT,TTOTA,LCMAX,IRTC
! 7805 FORMAT(20X,'TIME IN FORWARD SECTION OF FRONT',F10.3/
!     3       20X,'TOTAL TIME IN   ELEMENTS        ',F10.3/
!     4       20X,'MAXIMUM FRONT WIDTH             ',I10/
!     5       20X,'BUFFER BLOCKS WRITTEN           ',I10)
!
!     BACK SUBSTITUTION
!
      NEC=NSZF+1
!
      DO 600 IV=1,NSZF
        NEC=NEC-1
        LCOL=LCS(NEC)
        LPIVCO=LPS(NEC)
        LQ=LQ-LCOL
        IF(LQ > -1) GO TO 450
        CALL XRED(ND1,IRTC,NRR)
!       CALL RED(ND1,-1)
        LQ=LQ-LCOL
  450 DO 460 L=1,LCOL
      LQ=LQ+1
      LHED(L)=IABS(LHS(LQ))
  460 QQ(L)=QS(LQ)
      LQ=LQ-LCOL
      LCO=LHED(LPIVCO)
      GASH=0.
      QQ(LPIVCO)=0.
      DO 580 L=1,LCOL
      ITMM=LHED(L)
      IF(ITMM /= 0) THEN
        GASH=GASH-QQ(L)*R1(ITMM)
      ENDIF
  580 CONTINUE
      R1(LCO)=R1(LCO)+GASH
  600 CONTINUE
      CALL SECOND(TAA)
      ADT=TAA-TA
      WRITE(ICFL,6199) ADT,TAA
!
!ipk mar01  Save results and NBC for equation dropout
      IF(IDRPT > 0 .AND. NRX == 1) THEN
        do j=1,nszf
          RKEEP(J)=R1(J)
        enddo
          DO J=1,NP
              DO K=1,NDF
              NBCKP(J,K)=NBC(J,K)
            ENDDO
          ENDDO
        ENDIF
!
!      do n=1,np
!        do m=1,ndf
!          if(nbc(n,m) > 0) then
!            write(73,*) n,m,nbc(n,m),rkeepeq(nbc(n,m))
!          endif
!        enddo
!      enddo
!
  417 FORMAT(/' NERROR =',I5//                                          &
     & 'MFW IS NOT LARGE ENOUGH TO PERMIT ASSEMBLY OF THE NEXT EL'      &
     &,'EMENT'/'  INCREASE MFW IN PARAM.COM OR LOOK FOR ERROR IN'       &
     &,' ELEMENT ELIMINATION ORDER')
  476 FORMAT(' WARNING-MATRIX SINGULAR OR ILL CONDITIONED')
      IF(NRX == 1) NDF=6
      RETURN
      END
