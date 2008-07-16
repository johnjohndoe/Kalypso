C     Last change:  IPK  21 Sep 2000    1:51 pm
cipk  last update June 26 1997
CIPK  LAST UPDATE MAY 9 1996
cipk  last update nov 10 1995
      SUBROUTINE FRONT_PARDISO(nrx)
C      
C
CIPK AUG07
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKSANMOD
      USE BLKECOM
      
      SAVE
      PARAMETER (MFRW=MFWSIZ, MXL=680)

CIPK FEB07      ALLOCATABLE eqq(:,:),lcpoint(:,:)
CIPK FEB07      ALLOCATABLE ELSTIF(:,:,:), ELF(:,:)
CIPK FEB07      ALLOCATABLE R(:),QS1(:)
CIPK FEB07  	ALLOCATABLE IRWEPT(:)

C-
cipk feb07 add LLDONE 
CIPK AUG07
      COMMON/FNT/ NK(120),LLDONE(80)
C
CIPK FEB07      REAL*4 BIGEQ,PILU,R,PIRU,BIGEQS
      REAL*4 BIGEQ,PILU,PIRU,BIGEQS
c      COMMON /NEWF/ BIGEQ(NBS),R(MR1)
CIPK FEB07      COMMON /NEWF/ R(MR1SIZ),QS1(NBUFFSIZ)
     
cipk feb07	COMMON /NEWI/ IRWEPT(NBUFFSIZ+MR1SIZ+1),LCPOINT(MXL,MFWSIZ),
cipk feb07     &                   eqq(MXL,MFWSIZ) 

CIPK FEB07	COMMON /NEWI/ IRWEPT(NBUFFSIZ+MR1SIZ+1)

      INTEGER*4 LROWENT(MR1SIZ),LRPOINT(MR1SIZ)
	
CIPK FEB07      COMMON /BLK_ELSTIF/ ELSTIF(30,30,MELSIZ), ELF(30,MELSIZ)

C superLU
      REAL*4  R1T(MR1SIZ)
	INTEGER*4 ichgl(mr1SIZ)
	data itime/0/
C
      IF(ITIME .EQ. 0) THEN
        ALLOCATE (LCPOINT(MXL,MFWSIZ),eqq(MXL,MFWSIZ))
ccc        ALLOCATE (ELSTIF(80,80,MELSIZ), ELF(80,MELSIZ))
        ALLOCATE (RR(MR1SIZ),QS1(NBUFFSIZ))
	  ALLOCATE (IRWEPT(NBUFFSIZ+MR1SIZ+1))
        ITIME=1
c        LCPOINT=0
c        EQQ=0.
c        ELSTIF=0.
c        ELF=0. 
      ENDIF

      LCPOINT=0
      EQQ=0.
      ELSTIF=0.
      ELF=0. 
      RR=0.
      QS1=0.
      IRWEPT=0

      CALL SECOND(SEC)

     	IF(IREALLCT .GT. 0) THEN 
     	  IF(IREALLCT .EQ. 2) THEN
          DEALLOCATE (R1,NLSTEL,rkeep,ekeep,rkeepeq)
        ENDIF
        ALLOCATE   
     +  (R1(MR1),NLSTEL(0:MR1),rkeep(0:MR1),ekeep(MR1)
     +   ,rkeepeq(MR1))
      ENDIF

      IF(.NOT. ALLOCATED(NLSTEL)) ALLOCATE(NLSTEL(0:MR1))
      if(maxn .eq. 1  .and. nrx .eq. 1) nszfo=nszf
      maxl = 0
	if(nrx .eq. 1) then
	  NDF=4
	else
	  ndf=1
	endif
*-
*...... Find last appearance of each node
*-

      NLSTEL=0
      ICHGL=0
           
      K=NESAV+1
      DO NN=1,NESAV
        K=K-1
        N=NFIXH(K)
        IF(N .GT. 0.  .AND.  N .LE. NE) THEN
          IF(IMAT(N) .GT. 0) THEN
CIPK NOV99
            IF(ICOLLAPE(N) .NE. 1  .OR.  IMAT(N)/1000 .EQ. 1) THEN

cipk jan06
              Mtip=mod(imat(n),100)
              IF(ORT(MTIP,1) .NE. 0.) THEN

cipk jan99      NCN=NCORN(N)
                ncn=20
                IF(ITEQV(MAXN) .EQ. 5) THEN
                  DO I=1,8
                    NCON(I)=NOPS(N,I)
                    IF(NCON(I) .GT. 0) NCN=I
                  ENDDO
                ELSE
                  DO I=1,NCN
                    NCON(I)=NOP(N,I)
                  ENDDO
                ENDIF
                MRC=N
cipk oct98 update to f90
                NTYP=NETYP(N)
                IF(MOD(NTYP,10) .EQ. 7) THEN
CIPK JAN99 SKIP OUT FOR 2DV
                  if(ntyp .NE. 17) THEN
C-
C...... Search to see if any entry in NLSTEL
C-
                    DO M=1,NCN
                      L=NCON(M)
CIPK JAN99 SKIP OUT FOR ZERO
                      if(l .GT. 0) THEN
                        DO I=1,NDF
                          J=NBC(L,I)
                          IF(J .GT. 0) THEN
                            IF(NLSTEL(J) .GT. 0) THEN
                              IF(NREORD(NLSTEL(J)) .GT. NREORD(MRC))
     +                          MRC=NLSTEL(J)
                              NLSTEL(J)=0
                            ENDIF
                          ENDIF
                        ENDDO                  
                      ENDIF
                    ENDDO
                  ELSE
                    GO TO 10
                  ENDIF
                ENDIF
                DO M=1,NCN
                  L=NCON(M)
CIPK JAN99 SKIP OUT FOR 2DV
                  if(l .GT. 0) THEN
                    DO I=1,NDF
                      J=NBC(L,I)
                      IF(J .GT. 0) THEN
                        IF(NLSTEL(J) .EQ. 0) THEN
                          NLSTEL(J)=MRC
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDIF
        ENDIF
   10   CONTINUE
      ENDDO
   12 CONTINUE
   
      icteq=0
      do nn=1,nesav
	  n=nfixh(nn)
	  ncn=ncorn(n)
c	  if(ncn .eq. 5) ncn=3
	  
        IF(NOP(N,3) .EQ. 0) NCN=2
            	  

        do kc=1,80
	    nk(kc)=0
cipk feb07 add initialization of LLDONE 
          LLDONE(KC)=0
	  enddo
        kc=0
cipk jun07        DO J=1,NCN
        DO J=1,20
          I=NOP(N,J)
          IF(I .EQ. 0) THEN
            KC=KC+NDF
          ELSE
            DO L=1,NDF
              LL=NBC(I,L)
              KC=KC+1
cipk feb07 add LLDONE 
              LLDONE(KC)=LL
              
              NK(KC)=LL
              IF(LL .NE. 0) THEN
                IF(NLSTEL(LL) .EQ. N) NK(KC)=-LL
                if(nk(kc) .lt. 0) then
                  DO JJ=1,KC-1
cipk feb07 add LLDONE test
                    IF(LLDONE(JJ) .EQ. ABS(LL)) GO TO 250
                  ENDDO
	            icteq=icteq+1
	            ichgl(ll)=icteq
  250             CONTINUE	            
	          endif
              ENDIF
            ENDDO
          ENDIF
	  enddo
      ENDDO
c      DO N=1,NSZF
c      WRITE(198,'(2I15)') N,ICHGL(N)
c      ENDDO
      
      do n=1,np
  	  do m=1,ndf
	    k=nbc(n,m)
	    if(k .ne. 0) then
	      nbc(n,m)=ichgl(k)
	      nbckp(n,m)=ichgl(k)
	    endif
	  enddo
	enddo

      DO J=1,NSZF
        NLSTEL(J)=0
        LROWENT(J)=0
	  LRPOINT(J)=0
	ENDDO
      K=NESAV+1
      DO NN=1,NESAV
        K=K-1
        N=NFIXH(K)
        IF(N .GT. 0.  .AND.  N .LE. NE) THEN
          IF(IMAT(N) .GT. 0) THEN
CIPK NOV99
            IF(ICOLLAPE(N) .NE. 1  .OR.  IMAT(N)/1000 .EQ. 1) THEN

cipk jan06
              Mtip=mod(imat(n),100)
              IF(ORT(MTIP,1) .NE. 0.) THEN

cipk jan99      NCN=NCORN(N)
                ncn=20
                IF(ITEQV(MAXN) .EQ. 5) THEN
                  DO I=1,8
                    NCON(I)=NOPS(N,I)
                    IF(NCON(I) .GT. 0) NCN=I
                  ENDDO
                ELSE
                  DO I=1,NCN
                    NCON(I)=NOP(N,I)
                  ENDDO
                ENDIF
                MRC=N
cipk oct98 update to f90
                NTYP=NETYP(N)
                IF(MOD(NTYP,10) .EQ. 7) THEN
CIPK JAN99 SKIP OUT FOR 2DV
                  if(ntyp .NE. 17) THEN
C-
C...... Search to see if any entry in NLSTEL
C-
                    DO M=1,NCN
                      L=NCON(M)
CIPK JAN99 SKIP OUT FOR ZERO
                      if(l .GT. 0) THEN
                        DO I=1,NDF
                          J=NBC(L,I)
                          IF(J .GT. 0) THEN
                            IF(NLSTEL(J) .GT. 0) THEN
                              IF(NREORD(NLSTEL(J)) .GT. NREORD(MRC))
     +                          MRC=NLSTEL(J)
                              NLSTEL(J)=0
                            ENDIF
                          ENDIF
                        ENDDO                  
                      ENDIF
                    ENDDO
                  ELSE
                    GO TO 14
                  ENDIF
                ENDIF
                DO M=1,NCN
                  L=NCON(M)
CIPK JAN99 SKIP OUT FOR 2DV
                  if(l .GT. 0) THEN
                    DO I=1,NDF
                      J=NBC(L,I)
                      IF(J .GT. 0) THEN
                        IF(NLSTEL(J) .EQ. 0) THEN
                          NLSTEL(J)=MRC
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDIF
        ENDIF
   14   CONTINUE
      ENDDO
C      do k=1,nszf
C        write(194,*) k,nlstel(j)
C      enddo
      ADTA=0.
      SELT=0.
 6199 FORMAT( /10X,'DELTA T =',F10.1,'  TOTAL T =',F10.1)
      WRITE(*,*)
     +'  ELEMENTS-PROCESSED   CURRENT-FRONT  MAX-FRONT'

      ICUR=nszf+1
      ND1=NSCR
      NEC=0
      IRTC=0
      LQ=0
      NRR=0
C     LBMAX=NBS
      LCMAX=0
      NMAX = MFW
      ISHRK=0
C-
C...... Initialize
C-
      NELL=0
      DO N=1,NSZF
        RR(N)=0.
      ENDDO
      DO N=1,NMAT
        TVOL(N)=0.
      ENDDO

      LCOL=0
      LROW=0
      LCOLM=MFW
      LROWM=0
c      DO I=1,LCOLM
c        DO J=1,lcolm
c          EQQ(J,I)=0.
c	    LCPOINT(J,I)=0
c        ENDDO
c      ENDDO
      eqq = 0.
      lcpoint = 0

      LELIM=0

      
C-
C......ESTABLISH DENSITIES AND PRESSURES
C-
      IF(NRX .EQ. 1) THEN
	CALL PRESR
      ENDIF


      CALL SECOND(SINC)
  
   18 NELL=NELL+1
      IF(NELL.GT.NE) GO TO 380
      N=NFIXH(NELL)
C
      IF(IMAT(N).LE.0) GO TO 18
      NM=IMAT(N)
      IF(NM .LT. 901) THEN
        IF(ORT(NM,1) .EQ. 0.) GO TO 18
      ENDIF

      NCN = NCORN(N)
      IF (NCN .EQ. 5  .AND.  IMAT(N) .LT. 901) NCN=3
ccc      do n=1,ne
ccc        IF ( IMAT(N) .gt. 0 ) then
ccc          NM = IMAT(N)

CIPK DEC03 ADD IEDSW DEPENDENCE

          NMATYP=(MOD(IMAT(N),1000))
	    IEDSW=IEDSW1(NMATYP)
	    TBFACT=TBFACT1(NMATYP)
	    TBMIN=TBMIN1(NMATYP)

          IF(ITEQV(MAXN) .NE. 5) THEN
          
	      IF(IMAT(N) .GT. 1000  .AND.  IMAT(N) .LT. 5000) THEN
	      
ccc              IF(NRX .EQ. 2) GO TO 17
              IF(NRX .EQ. 2) GO TO 18

CIPK NOV99     Either process surface integrals or collapse to 2-d
    
              IF(ICOLLAPE(N) .EQ. 0) THEN
     	          CALL SURCOF(N,NRX)
              ELSEIF(IMAT(N) .LT. 2000) THEN
                IF(NETYP(N)/10 .LT. 1) THEN
CIPK MAR05
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF1(N,NRX)
                  ELSE
                    CALL COEF1NT(N,NRX)
	            ENDIF
CIPK SEP05  ADD ERROR TEST
	            IF(JREDERR .EQ. 1) RETURN  
CIPK MAR07
C     Modify tests to allows for IDIFSW

                ELSEIF(IUTUB .EQ. 1  .and.  iedsw .eq. 2
     +            .AND. ISLP .EQ. 0) THEN

                  IF(INOTR .EQ. 0) THEN
                    CALL COEF2D(N,NRX)
	            ELSE
                    CALL COEF2DNT(N,NRX)
	            ENDIF
CIPK SEP05  ADD ERROR TEST
	            IF(JREDERR .EQ. 1) RETURN  
                ELSEIF(ISLP .EQ. 1  .AND.  IUTUB .EQ. 1
     +           .AND. IDIFSW .EQ. 2) THEN
CIPK MAR05
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF2D(N,NRX)
	            ELSE
                    CALL COEF2DNT(N,NRX)
	            ENDIF
                ELSE
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF2(N,NRX)
	            ELSE
                    CALL COEF2NT(N,NRX)
	            ENDIF
CIPK SEP05  ADD ERROR TEST
	            IF(JREDERR .EQ. 1) RETURN  
                ENDIF
              ELSE
ccc                GO TO 17
                GO TO 18
              ENDIF

	      ELSE
	        IF(NETYP(N)/10 .EQ. 2) THEN

cipk nov99 skip if collapsing to 2-d

ccc                IF(ICOLLAPE(N) .EQ. 1 .AND. NRX .NE. 2) GO TO 17
                IF(ICOLLAPE(N) .EQ. 1 .AND. NRX .NE. 2) GO TO 18

C     Process   threed element
cipk jan06
                ntip=abs(mod(imat(n),100))
ccc                IF(ORT(ntip,1) .EQ. 0) GO TO 17
                IF(ORT(ntip,1) .EQ. 0) GO TO 18
cipk jan97
CIPK MAR07
                if(iutub .eq. 1  .AND.  IEDSW .EQ. 2) then
                  CALL COEF3D(N,NRX)
                else
                  call coef3(n,nrx)
                endif
cipk jan97 end changes
  	        ELSE
	          IF(NETYP(N)/10 .EQ. 1) THEN

C     Process   twod elements

	            IF(IMAT(N)  .LT. 900  .OR. NCORN(N) .GT. 5) THEN

C     Process   horizontal 2d

ccc                    IF(NRX .EQ. 2) GO TO 17
                    IF(NRX .EQ. 2) GO TO 18
cipk jan06
                    ntip=abs(mod(imat(n),100))
ccc                    IF(ORT(ntip,1) .EQ. 0) GO TO 17
                    IF(ORT(ntip,1) .EQ. 0) GO TO 18
cipk jan97
CIPK MAR07
                    if(iutub .eq. 1  .AND.  IEDSW .EQ. 2
     +                .AND. ISLP .EQ. 0) THEN
                      if(nell .eq. 1)  then
                        write(75,*) 'going to smag'
                      endif
                      IF(INOTR .EQ. 0) THEN
                        CALL COEF2D(N,NRX)
  	                ELSE
	                  CALL COEF2DNT(N,NRX)
	                ENDIF
CIPK SEP05  ADD ERROR TEST
  	                IF(JREDERR .EQ. 1) RETURN  
                    ELSEIF(ISLP .EQ. 1  .AND.  IUTUB .EQ. 1 .AND.
     +                     IDIFSW .EQ. 2) THEN
                      IF(INOTR .EQ. 0) THEN
                        CALL COEF2D(N,NRX)
	                ELSE
	                  CALL COEF2DNT(N,NRX)
	                ENDIF
                    else
                      IF(INOTR .EQ. 0) THEN
                        CALL COEF2(N,NRX)
  	                ELSE
	                  CALL COEF2NT(N,NRX)
	                ENDIF
CIPK SEP05  ADD ERROR TEST
   	                IF(JREDERR .EQ. 1) RETURN  
                    endif
cipk jan97 end changes
        	      ELSE
CIPK JAN99
ccc                    IF(IMAT(N) .GT. 900  .AND. IMAT(N) .LT. 1000)GOTO 17
                    IF(IMAT(N) .GT. 900  .AND. IMAT(N) .LT. 1000)GOTO 18

C      Process vertical 2d


cipk nov99 skip if collapsing to 2-d
  
ccc                    IF(ICOLLAPE(N) .EQ. 1  .AND.  NRX .NE. 2) GO TO 17
                    IF(ICOLLAPE(N) .EQ. 1  .AND.  NRX .NE. 2) GO TO 18

  	  	          CALL COEFV(N,NRX)
  	            ENDIF
                ELSE

C      Process one-d elements

                  IF(NRX .EQ. 2) GO TO 18
                  IF(INOTR .EQ. 0) THEN
                    CALL COEF1(N,NRX)
                  ELSE
                    CALL COEF1NT(N,NRX)
                  ENDIF
CIPK SEP05  ADD ERROR TEST
                  IF(JREDERR .EQ. 1) RETURN  
	          ENDIF
	        ENDIF
	      ENDIF
          ELSE
C-
C...... The calls below are for the case of vertical averaging
C-
   19       IF(N .LE. NE) GO TO 20
	      NELM=NELM+1
	      N=NFIXH(NELM)
	      GO TO 19
   20       CONTINUE
	      IF(NOPS(N,6) .GT. 0) THEN
              IF(INOTR .EQ. 0) THEN
                CALL COEF2(N,NRX)
              ELSE
                CALL COEF2NT(N,NRX)
              ENDIF
CIPK SEP05  ADD ERROR TEST
              IF(JREDERR .EQ. 1) RETURN  
	      ELSE
              IF(INOTR .EQ. 0) THEN
                CALL COEF1(N,NRX)
              ELSE
                CALL COEF1NT(N,NRX)
              ENDIF
CIPK SEP05  ADD ERROR TEST
              IF(JREDERR .EQ. 1) RETURN  
	      ENDIF
          ENDIF
cipk jan99

          IF(NETYP(N) .EQ. 15  .OR.  NETYP(N) .EQ. 16) THEN
            IF(NOP(N,14) .NE. 0) NCN=14
            IF(NOP(N,18) .NE. 0) NCN=18  
          ENDIF

cipk jan01
      do i=1,ncn
        nod=nop(n,i)
        if(nod .ne. 0) then
          do j=1,ndf
            if(iactv(nod,j) .eq. 0  .and. nbc(nod,j) .ne. 0) then
              irwz=(i-1)*ndf+j
              do ii=1,80
                estifm(ii,irwz)=0.
                estifm(irwz,ii)=0.
              enddo
              estifm(irwz,irwz)=1.0
              f(irwz)=0.0
            endif
          enddo
        endif
      enddo
      DO I=1,NCN
        J=NOP(N,I)
	  IF(J .GT. 0) THEN
          IA=NDF*(I-1)
          DO K=1,NDF
            IA=IA+1
            JA=NBC(J,K)
            IF(JA .NE. 0) THEN
            
              if ( ja .lt. 0 )  then
                dum = 0
              endif
              RR(JA)=RR(JA)+F(IA)
  	      ENDIF
          ENDDO
        ENDIF
	ENDDO
      NBN = NCN*NDF
      DO LK=1,NBN
c        LDEST(LK)=0
        NK(LK)=0
      ENDDO
      KC=0

C     SETUP DESTINATION LOCATION

      DO J=1,NCN
        I=NOP(N,J)
        IF(I .EQ. 0) THEN
          KC=KC+NDF
        ELSE
          DO L=1,NDF
            LL=NBC(I,L)
            KC=KC+1
            NK(KC)=LL
            IF(LL .NE. 0) THEN
              IF(NLSTEL(LL) .EQ. N) NK(KC)=-LL
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C     SET UP HEADING VECTORS IF NEEDED
C
    
      LFZ=1
      DO LK=1,NBN
       LROWT=ABS(NK(LK))
	 if(lrowt .ne. 0) then
c
c     lrowt is the equation number for this degree of freedom
c          look in pointer matrix for this row if it has been used
c          before
        lrow= lrpoint(lrowt)
	  if(lrow .eq. 0) then
c
c     create an entry for a blank row
c
          do ll=1,lrowm
	      if(lrowent(ll) .eq. 0) then
	        lrpoint(lrowt)=ll
	        lrow=ll
	        go to 50
	      endif
	    enddo
          lrow=lrowm+1
	    lrowm=lrowm+1
          lrowent(lrow)=0
          IF(LROWM .GT. MFRW) THEN
            NERROR=2
            WRITE(*,*) '  MFRW not large enough'
            WRITE(75,417)NERROR
            WRITE(*,6008)   NFIXH(NELL)
            WRITE(75,6008)   NFIXH(NELL)
 6008       FORMAT( // 10X, '..STOP AT ELEMENT', I10 )
C            WRITE(75,9820) (LHED(L),L=1,LCOL)
            WRITE(75,9821) (N,(NBC(N,M),M=1,3),N=1,NP)
            CALL ZVRS(1)
            STOP
          ENDIF
          lrpoint(lrowt)=lrow
	  endif
   50   continue
        LL=LROW


        DO KK=1,NBN

C     now work along the columns hoping storing values
 
c         IF (NODEC .EQ. 0) GO TO 62
          IF (NK(KK) .EQ. 0) GO TO 62
          NODEC=ABS(NK(KK))
          
          DO L=1,lrowent(lrow)+1
             IF(LCPOINT(L,LL) .EQ. NODEC) THEN

C     we have a match - store the entry

crow-wise              EQ(LL,L)=EQ(LL,L)+ESTIFM(LK,KK)
c              EQ(LL,L)=EQ(LL,L)+ESTIFM(KK,LK)
                EQQ(L,LL)=EQQ(L,LL)+ESTIFM(LK,KK)
                GO TO 62
             ELSEIF(LCPOINT(L,LL) .EQ. 0) THEN

C     no match so add to last column and store

                LCPOINT(L,LL)=NODEC
crow-wise              EQ(LL,L)=EQ(LL,L)+ESTIFM(LK,KK)
c              EQ(LL,L)=EQ(LL,L)+ESTIFM(KK,LK)
                EQQ(L,LL)=EQQ(L,LL)+ESTIFM(LK,KK)
	          if( L .gt. lrowent(lrow) ) lrowent(lrow)=L
                GO TO 62
             ENDIF
          ENDDO
            
   62     CONTINUE
            if ( l .gt. maxl ) maxl = l
   
        ENDDO
        
	 ENDIF
      ENDDO

C     SET LHED NEGATIVE FOR ENTRIES THAT ARE FULLY SUMMED

      IF(MOD(NELL,1000) .EQ. 0)
     +  WRITE(*,'(I13,2I10)') NELL,LROWM,LELIM
      IF(MOD(NELL,1000) .EQ. 0)
     +  WRITE(75,'(I13,2I10)') NELL,LROWM,LELIM


   60 CONTINUE
C
C     DETERMINE WHETHER TO COMPACT OR ADD A NEW ELEMENT
C
      LELTM=0
      DO LL=1,LROWM
	  L=LL+LELIM
	  lrow=lrpoint(l)
	  IF(NLSTEL(L) .EQ. N) THEN
          IRWEPT(L)=ICUR-NSZF
          DO J=1,LROWENT(LROW)
            IF(LCPOINT(J,LROW) .NE. 0) THEN
c              IF(EQQ(J,LROW) .NE. 0.) THEN
                ICUR=ICUR+1
                qs1(ICUR-NSZF-1)=EQQ(J,LROW)
	          EQQ(J,LROW)=0.
                IRWEPT(ICUR)=LCPOINT(J,LROW)
	          LCPOINT(J,LROW)=0
c 	        ELSE
c	          LCPOINT(J,LROW)=0
c              ENDIF
            ELSE
              GO TO 70
            ENDIF
          END DO
          LROWENT(LROW)=0


   70     CONTINUE
          LELTM=LELTM+1
	  ELSE
	    GO TO 100
        ENDIF

      ENDDO
  100 CONTINUE
	LELIM=LELIM+LELTM
      GO TO 18
  380 CONTINUE

C     ADD LAST VALUE TO IRWEPT

      IRWEPT(NSZF+1)=ICUR-NSZF
C
C ***** OBTAIN INITIAL GUESS (ALL ZEROS)
C

      DO I = 1, NSZF
         R1(I) = 0.0D0
      END DO

C
C ***** CALL THE SOLVER
C
      DO I=1,NSZF
        R1T(I)=RR(I)
      ENDDO

      write(75,*) 'matrix size = ',icur
      write(*,*) 'matrix size = ',icur

            write(*,*) ' maxl = ', maxl
      
      call second(sutim1)
      
C  Set number of processors      
CIPK FEB07      NPROCS = ICPU
      NPROCS = ICPU

                   
      CALL mkl_solver(NPROCS,NSZF,
     &          IRWEPT(1),IRWEPT(NSZF+2),qs1(1), RR(1), R1T(1),IERR)

c      CALL SUPERLU(ORDER,ORDERING(1),NPROCS,NSZF,
c     &             ICOL(MPUC),IROW(MPUR),SEQ,R1T(1),IERR)

c      write(*,'(a,f8.3)') ' Setup time = ', sutim1-sec
c      write(75,'(a,f8.3)') ' Setup time = ', sutim1-sec

cc      CALL SUPERLU_S(IORDER, IORDERING(1),NPROCS,NSZF,
cc     &             IRWEPT(1),IRWEPT(NSZF+2),BIGEQ(1), R1T(1),IERR)
c     &             IRWEPT(NSZF+2),IRWEPT(1),BIGEQ(1), R1T(1),IERR)

c      do n=1,nszf
c	  write(75,'(2i10)') n,iordering(n)
c	enddo

      if(ierr .ne. 0) then
        write(75,*) 'error stop in mkl',ierr
        write(*,*) 'error stop in mkl',ierr
        stop
      endif        

      call second(sutim2)
      
      WRITE(75,'(a,f8.2)') ' Time in setup  = ', SINC-SEC
      WRITE(75,'(a,f8.2)') ' Time in coefs  = ', sutim1-SINC
      write(75,'(a,f8.2)') ' Time in solve  = ', sutim2-sutim1
      write(75,'(a,f8.2)') ' Total time     = ', sutim2-sec
C      WRITE(*,*) ' Number threads = ', ithreads

      DO I=1,NSZF
        R1(I)=R1T(I)
        rkeep(i)=r1t(i)
      ENDDO

      IF(NRX .EQ. 1) NDF=6


  479   FORMAT( '  UNSATISFIED ELIMINATION ERROR STOP'/
     1  '   LCOL =',I5)
 9820   FORMAT('  CONTENTS OF LHED ARE'/(5I8))
 9821   FORMAT('  NBC ARRAY IS '/(4I8))
 9822   FORMAT('  EQ IS'/(1P5E12.4))
C      CALL SECOND(SEC2)
C      ADTA=SEC2-SEC1
C	SECTOT=SEC2-SEC
C      WRITE(*,6199) ADTA,SEC2
C      WRITE(75,6040) ADTA,SELT,SECTOT
C      WRITE(*,6040) ADTA,SELT,SECTOT
 6040 FORMAT('  FRONT SETUP TIME =',F6.2,'  TIME IN COEFS =',F6.2/
     +       '  TOTAL TIME IN FRONT =',F6.2)

C      CALL SECOND(SEC3)
C      ADTA=SEC3-SEC2
C      WRITE(*,6199) ADTA,SEC3
  417 FORMAT(/' NERROR =',I5//
     1 'MFW IS NOT LARGE ENOUGH TO PERMIT ASSEMBLY OF THE NEXT EL'
     2,'EMENT'/'  INCREASE MFW IN PARAM.COM OR LOOK FOR ERROR IN'
     3,' ELEMENT ELIMINATION ORDER')
      RETURN
  476 FORMAT(' WARNING-MATRIX SINGULAR OR ILL CONDITIONED')
      END
