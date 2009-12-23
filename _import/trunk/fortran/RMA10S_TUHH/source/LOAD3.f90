!ipk  last update feb 26 2006 corect logic for 1-d structures and limit reordering to 1
!ipk  last update dec 13 2006 seup for type QI (experimental)
!ipk  last update nov 28 2006 allow for 1-d control structures
!IPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
!IPK  LAST UPDATE SEP 6 2004   add error file
!IPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
!ipk  last update Nov 12 1999 add dropout of nodes for 3-d to 2-d
!IPK  LAST UPDATE JAN 12 1999 FIX 2DV JUNCTION LOGIC
!ipk  last update Dec 5 1998 fix error at transitions
!     Last change:  MD   20 Aug 2008   11:55 am
!ipk  last update Feb 4 1998 correct zero subscript test and for HCN
!IPK  LAST UPDATE JAN 22 1998
!ipk  last update Dec 16 1997
!ipk  last update April 28 1997 add equation dropout
!ipk  last update Oct 31 1996 save nfixh when using optim
!ipk  last update Sep 4 1996  change logic for type 2 spec's of constits
      SUBROUTINE LOAD
      USE BLK10MOD
      USE BLK11MOD
      USE BLK10
      USE BLKSUBMOD
      USE BLKSANMOD
      use BLKDRMOD
!IPK DEC06      
      USE PARAMMOD
      USE BLKSBG
      USE parakalyps
      SAVE
!NiS,may06:testing for non defined variables
!      implicit none
!
!nis,feb08: for line transitions
      LOGICAL :: NodeIsDry (1:3535)
      integer :: dea_stat
      integer :: nln
!
!......DIMENSION STATEMENTS
!-
      DIMENSION IM(20,3),ISHF(20,2),NCON(20),Q1C(3)
      DATA IM/1,0,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,                  &
     &        1,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,                  &
     &        1,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0/
      DATA ISHF/0,0,0,3,3,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,                &
     & 0,0,0,3,3,2,1,1,0,0,10,9,0,0,0,15,15,14,13,13/
      DATA IENT/0/,PI2/1.5708/
      DATA CNMIN,PCERIN/1.0,1.0/ITIMTH/1/
      Dist(n1,n2)=sqrt((cord(n1,1)-cord(n2,1))**2 +                     &
     &                 (cord(n1,2)-cord(n2,2))**2)
!
!nis,nov06: Defintion of local variables for 1D-2D-line-transition handling
      INTEGER :: LiNo, LiLe, CoNode
!-
!NiS,jul06: Declaring waterlevel H for proper call of wform-subroutine
      REAL(KIND=8) :: HL1, HL2
!-
!
!-
!       NTHREE(N) = 1 FOR THREE DIMENSIONAL NODES
!       NTHREE(N) = 0 FOR TWO DIMENSIONAL HORIZONTAL NODES
!-
!...... Set direction for 1D - 2D junction node plus other initialise.
!-
      IF(IENT == 0) THEN
        CALL GETCON
!nis,com: copy of element and point number        
        NPSAV=NP
        NESAV=NE
!
!nis,com: do for every element        
        DO 140 N=1,NE
          NN=NFIXH(N)
!ipk oct96 save original order
!          nelord(n)=nfixh(n)
!ZZZ
            if (nn == 0)  goto 140
!nis,com: other way round, to every equation processing number an element number is stored          
          NREORD(NN)=N
  140   CONTINUE
        IENT=IENT+1
      ENDIF
!
!nis,dec06: EXTLDEL is an array for all elements. Initialization moved to initl.for      
!EXTLDEL=0      
!-      
!
      NP=NPSAV
      NE=NESAV
!
!NiS,may06: do for every node      
      do n=1,np
        IF(MOD(NFIX(N),100)/10 == 2) THEN
          ibc=iactvbc(n)
          if(ibc == 0) then
            NFD=NFIX(N)/100
            NFIX(N)=NFD*100+MOD(NFIX(N),10)
          endif
        endif
        IF(MOD(NFIX(N),10) == 2) THEN
          ibc=iactvbc(n)
          if(ibc == 0) then
            NFD=NFIX(N)/10
            NFIX(N)=NFD*10
          endif
        endif
        IF(NFIX1(N) == 2) THEN
          ibc=iactvbc(n)
          if(ibc == 0) then
            NFIX1(N)=0
          endif
        endif
      enddo
!
!      write(75,*) 'load-76 nfix(93),nfix(634)',nfix(93),nfix(634),
!     +    maxn,iteqv(maxn)
!
!-
!-.....COPY HEAD SPECS AND FIX BOUNDARY CONDITIONS.....
!-
!NiS,may06: set degree of freedom number      
      NDF=6
!NiS,may06: NA == 1, ALWAYS!      
      NA = 10**(6-NDF)
!NiS,may06: store temporary array for boundary conditions of every node      
      DO 195 J = 1, NP
!NiS,may06: bring NFIX and NFIX1 together into one array        
        NLOC(J)=NFIX(J)*10+NFIX1(J)
!NiS,may06: if salinity part-value equals 2, then subtract 200        
        IF( MOD( NLOC(J)/100,10 ) == 2 ) NLOC(J) = NLOC(J) - 200
!NiS,may06: NA == 1; ALWAYS!        
        NLOC(J) = NLOC(J) / NA
!
  195 CONTINUE
!-
!-    SET UP NBC ARRAY
!-
      DO 197 N=1,NP
!NiS,may06: at the beginning every element is 3D
        NTHREE(N)=1 
!NiS,may06: NBC(N,M), M=1..7 are evaluated by -1
        NBC(N,7)=-1 
        DO 197 M=1,NDF
  197     NBC(N,M)=-1
!NiS,may06: end of do-loop 197      
!
!NiS,may06: for every element      
      DO 199 J=1,NE
!
!IPK DEC00 REVISE TO ALLOW FOR GATE STRUCTURE
!
        NM=IMAT(J)
!NiS,may06: IMAT < 1 means deactivated element; jump over that        
        IF(NM < 1) GO TO 199
!IPK JUN05
!NiS,may06: 3D- or special elements; jump over that        
        IF(NM > 900 .AND. NM < 5001 .AND. IGTP(J) == 0                  &
     &   .AND. NFCTP(J) == 0) then
          GO TO 199
        endif
!IPK OCT98 CONVERT TO F90
!NiS,may06: every element that is in the range of 1-899 and 1001+        
        IF(NM < 900 .OR. NM > 1000) THEN
!
!nis,may07: Might be error with the mod-function, because this function would reduce any material type greater than 100 to a number less than 100
!           Increasing to mod(nm,1000)
!           probably it's for the reason of curved elements
!NiS,may06: for every normal element
          L=MOD(NM,100) 
!          L=MOD(NM,1000) !NiS,may06: for every normal element
!-
        ELSE
!NiS,may06: for special elements
          L=NM 
        ENDIF
!nis,may07
!Allow for polynom approach with turbulent exchange coefficient equal to zero
!NiS,may06: if no turbulent exchange coefficient, jump over element        
!        IF(ORT(L,1) == 0.) GO TO 199
        if (l /= 89) then
          IF(ORT(L,1) == 0.) GO TO 199
        end if
!Allow for polynom approach with turbulent exchange coefficient equal to zero
!-
        ILK=1
!
!NiS,may06: ITEQV == 5 means forced reduction to plan view        
        IF(ITEQV(MAXN) == 5) THEN
          IF(J > NEM) GO TO 1991
          DO 1950 K=1,8
            NCON(K)=NOPS(J,K)
            IF(NCON(K) > 0) NCN=K
 1950     CONTINUE
!NiS,may06: every other case instead of forced reduction to plan view        
        ELSE
          NCN=NCORN(J)
          DO 1951 K=1,NCN
!NiS,may06: get local corner nodes of processed element            
            NCON(K)=NOP(J,K)
 1951     CONTINUE
        ENDIF
!NiS,may06: special cases for 3D-elements        
        IF(NCN == 15) ILK=2
        IF(NCN == 13) ILK=3
        IF(NCN == 10) ILK=2
!
!NiS,may06: process every node of element        
!nis,feb07: Rename loop for f90        
!do 198 k = 1, NCN        
        NBCrelation: DO K=1,NCN
!-        
!NiS,may06: local copy of processed node number          
          N=NCON(K)
!
!-
!........ Setup for special case inflow quality conditions
!-
!
!NiS,may06: no velocities, so no direction is applied          
          IF (VEL(2,N) == 0. .AND. VEL(1,N) == 0) THEN
            VDIR = 0.
!NiS,may06: superposition of component velocities for direction of flow          
          ELSE
            VDIR=ATAN2(VEL(2,N),VEL(1,N))
          ENDIF
!IPK OCT98 CONVERT TO F90
          N1=ABS(NSURF(N))
          DIF=VDIR-VOUTN(N1)
          IF(DIF > 2.*PI2) DIF=DIF-4.*PI2
          IF(DIF < -2.*PI2) DIF=DIF+4.*PI2
!IPK OCT98 CONVERT TO F90
          NFTYP=NFIX(N)
          IF(MOD(NFTYP,100)/10 == 2) THEN
            IRMD=MOD(NFTYP,10)*10+NFIX1(N)
!ipk sep96            IF(ABS(DIF) < PI2) THEN
!             NLOC(N)=10*(NLOC(N)/10)
              nloc(n)=100+1000*(nloc(n)/1000)+IRMD
!IPK JAN98
              vel(4,n)=spec(n,4)
!ipk sep96            ELSE
!ipk sep96              nloc(n)=100+1000*(nloc(n)/1000)+IRMD
!ipk sep96              VEL(4,N)=SPEC(N,4)
!ipk sep96            ENDIF
          ENDIF
          IF(MOD(NFTYP,10) == 2) THEN
!ipksep96            IF(ABS(DIF) < PI2) THEN
!             NLOC(N)=10*(NLOC(N)/10)
              nloc(n)=10+100*(nloc(n)/100)+NFIX1(N)
!IPK JAN98
              vel(5,n)=spec(n,5)
!ipk sep96            ELSE
!ipk sep96              nloc(n)=10+100*(nloc(n)/100)+NFIX1(N)
!ipk sep96              VEL(5,N)=SPEC(N,5)
!ipk sep96            ENDIF
          ENDIF
          IF(NFIX1(N) == 2) THEN
!ipk sep96            IF(ABS(DIF) < PI2) THEN
!             NLOC(N)=10*(NLOC(N)/10)
              nloc(n)=1+10*(nloc(n)/10)
!IPK JAN98
              vel(6,n)=spec(n,6)
!ipk sep96            ELSE
!ipk sep96              nloc(n)=1+10*(nloc(n)/10)
!ipk sep96              VEL(6,N)=SPEC(N,6)
!ipk sep96            ENDIF
          ENDIF
!NiS,may06: store pointer for every node, that is not 3D          
          IF(NCN < 9 .AND. IMAT(J) < 1000) NTHREE(N)=0
          NDS=2
!NiS,may06: NDF is always greater than 3, that means all the concentrations          
          IF(NDF > 3) THEN
            DO 1970 KK=4,NDF
              NBC(N,KK)=0
 1970       CONTINUE
          ENDIF
          IF(IM(K,ILK) /= 0) THEN
            NBC(N,3)=-NSURF(N)-1
            IF(NDF > 3) THEN
              DO 1971 KK=4,NDF
                NBC(N,KK)=0
 1971         CONTINUE
            ENDIF
!nis,aug08: for continuity equation
            IF(abs(NSURF(N)) == N) NDS=3
          ENDIF
          DO 1976 I=1,NDS
 1976     NBC(N,I)=0
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
!IPK JUN02
          NBC(N,7)=0
!nis,feb07: rename loop to f90
!  198   CONTINUE
         ENDDO NBCrelation
!-
  199 CONTINUE
!
 1991 CONTINUE
!
!                                       FORM DEGREE OF FREEDOM ARRAY
!
      DO 220 N=1,NP
        K=10**NDF
!ipk apr97 change loop to 210 - add code for element dropout
        DO 210 M=1,NDF
          K=K/10
          NBC(N,M)=NBC(N,M)+1
          IF(NBC(N,M) /= 1) GO TO 200
          IF(NLOC(N)/K /= 1) GO TO 200
          NBC(N,M)=0
  200     NLOC(N)=MOD(NLOC(N),K)
!ipk feb97 add code for element dropout
!       if(iactv(n,m) == 0) then
!         write (*,*) ' debug load: n = ',n,' m = ',m
!         write (*,*) ' debug load: iactv = ',iactv(n,m)
!       endif
        if(iactv(n,m) == 0) nbc(n,m)=0
  210   continue
!ipk apr97 end change
!
!.....  Set up for case of straight line salinity
!
        IF(NSTRT(N,1) /= 0) NBC(N,4)=0
        IF(ITEQV(MAXN) == 10) NBC(N,7)=NBC(N,7)+1
!EFa oct09, hfd        
        IF(spec(n,8) /= 0.) NBC(N,2)= 0   
!-        
!
  220 CONTINUE
!
!IPK DEC00       test for gate structure
!
      if(maxn < 2) then
        DO N=1,NE
          igtcl(n)=0
          IF(IGTP(N) /= 0) THEN
            NGT=IMAT(N)-900
            IF(NDUPJ(NGT) > 0 .AND. NDDNJ(NGT) > 0) THEN
              IF(WSLL(NDUPJ(NGT)) < WSLL(NDDNJ(NGT))  .AND.             &
     &          WSLL(NDUPJ(NGT)) > BJ(NGT)) THEN
                CALL SETGT(N)
                igtcl(n)=1
              ELSE
                NSTRT(NOP(N,2),1)=0
              ENDIF
              write(75,*) 'gate setting', n,igtcl(n)
            ELSEIF(NDFLJ(NGT) > 0) THEN
              IF(NDUPJ(NGT) == 0 .AND. NDDNJ(NGT) == 0) THEN
                ACR=(2.*WIDTH(NDFLJ(NGT))+VEL(3,NDFLJ(NGT))*            &
     &           (SS1(NDFLJ(NGT))+SS2(NDFLJ(NGT))))/2.*VEL(3,NDFLJ(NGT))
!C               QFL=ACR*SQRT(VEL(1,NDFLJ(NGT))**2+VEL(2,NDFLJ(NGT))**2)
                QFL=(VEL(1,NDFLJ(NGT))*COS(ALFA(NDFLJ(NGT)))+           &
     &               VEL(2,NDFLJ(NGT))*SIN(ALFA(NDFLJ(NGT))))*ACR
                write(75,*) 'gate flow',qfl,aj(ngt)
                IF(AJ(NGT) > 0.) THEN
                  IF(QFL < AJ(NGT)) THEN
                    CALL SETGT(N)
                    igtcl(n)=1
                  ELSE
                    NSTRT(NOP(N,2),1)=0
                  ENDIF
                ELSE
                  IF(QFL > AJ(NGT)) THEN
                    CALL SETGT(N)
                    igtcl(n)=1
                  ELSE
                    NSTRT(NOP(N,2),1)=0
                  ENDIF
                ENDIF
                write(75,*) 'gate setting', n,igtcl(n)
              ELSEIF(NDUPJ(NGT) > 0) THEN
                ACR=(2.*WIDTH(NDFLJ(NGT))+VEL(3,NDFLJ(NGT))*            &
     &           (SS1(NDFLJ(NGT))+SS2(NDFLJ(NGT))))/2.*VEL(3,NDFLJ(NGT))
!C               QFL=ACR*SQRT(VEL(1,NDFLJ(NGT))**2+VEL(2,NDFLJ(NGT))**2)
                QFL=(VEL(1,NDFLJ(NGT))*COS(ALFA(NDFLJ(NGT)))+           &
     &               VEL(2,NDFLJ(NGT))*SIN(ALFA(NDFLJ(NGT))))*ACR
!
                write(75,*) 'gate head flow',wsll(ndupj(ngt)),bj(ngt),  &
     &              qfl,aj(ngt)
                write(75,*) 'vels',VEL(1,NDFLJ(NGT)),Vold(1,NDFLJ(NGT))
                IF(WSLL(NDUPJ(NGT)) < BJ(NGT)) THEN
                  CALL SETGT(N)
                  igtcl(n)=1
                ELSEIF(AJ(NGT) > 0) THEN
                  IF(QFL > AJ(NGT)) THEN
                    CALL SETGT(N)
                    igtcl(n)=1
                  ELSE
                    NSTRT(NOP(N,2),1)=0
                  ENDIF
                ELSE
                  IF(QFL < AJ(NGT)) THEN
                    CALL SETGT(N)
                    igtcl(n)=1
                  ELSE
                    NSTRT(NOP(N,2),1)=0
                  ENDIF
                ENDIF
                write(75,*) 'gate setting', n,igtcl(n)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      else
        DO N=1,NE
          IF(IGTP(N) /= 0) THEN
            if(igtcl(n) == 1) then
              call setgt(n)
            else
              NSTRT(NOP(N,2),1)=0
            endif
          endif
        ENDDO
      endif
!-
!...... Define overriding boundary conditions based on input ITEQV
!-
!ipk jun05   0=   vel+ dep + sal        
!ipk jun05   1=   vel+ dep        
!ipk jun05   2=   sal        
!ipk jun05   3=   vels+sal        
!ipk jun05   4=   vels        
!ipk jun05   5=   2d approx        
!ipk jun05   6=   vels+dep+temp        
!ipk jun05   7=   vels+dep+sed        
!ipk jun05   8=   temp        
!ipk jun05   9=   sed        
!ipk jun05  11=   vels+temp        
!ipk jun05  12=   vels+sed        
!
!ipk may02 initialize seventh option
!      do n=1,np
!        nbc(n,7)=0
!        enddo
!ipk jun05
      inovel=0
      if(iteqv(maxn) == 2) inovel=1
      if(iteqv(maxn) == 8)  inovel=2
      if(iteqv(maxn) == 9) inovel=3
      IF(ITEQV(MAXN) == 0) THEN
!ipk jun05      vel+ dep + sal        
        DO 2205 N=1,NP
          NBC(N,5)=0
          NBC(N,6)=0
 2205   CONTINUE
      ELSEIF(ITEQV(MAXN) == 1) THEN
!ipk jun05      vel+ dep        
        DO 221 N=1,NP
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
  221   CONTINUE
      ELSEIF(ITEQV(MAXN) == 2) THEN
!ipk jun05      sal        
        DO 222 N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,5)=0
          NBC(N,6)=0
  222   CONTINUE
      ELSEIF(ITEQV(MAXN) == 3) THEN
!ipk jun05      vels+sal        
        DO 223 N=1,NP
          NBC(N,3)=0
          NBC(N,5)=0
          NBC(N,6)=0
  223   CONTINUE
      ELSEIF(ITEQV(MAXN) == 4) THEN
!ipk jun05      vels        
        DO 224 N=1,NP
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
  224   CONTINUE
      ELSEIF(ITEQV(MAXN) == 5) THEN
!ipk jun05      2d approx        
        NP=NPM
        NE=NEM
      ELSEIF(ITEQV(MAXN) == 6) THEN
!ipk jun05      vels+dep+temp        
        DO 225 N=1,NP
          NBC(N,4)=0
          NBC(N,6)=0
  225   CONTINUE      
      ELSEIF(ITEQV(MAXN) == 7) THEN
!ipk jun05      vels+dep+sed        
        DO 226 N=1,NP
          NBC(N,4)=0
          NBC(N,5)=0
  226   CONTINUE   
      ELSEIF(ITEQV(MAXN) == 8) THEN
!ipk jun05      temp        
        DO 227 N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,6)=0
  227   CONTINUE
      ELSEIF(ITEQV(MAXN) == 9) THEN
!ipk jun05      sed        
        DO 228 N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
  228   CONTINUE
!ipk may02 revise for new 23 option ob BN line
      ELSEIF(ITEQV(MAXN) == 10) THEN
        DO N=1,NP
          NBC(N,1)=0
          NBC(N,2)=0
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
          NBC(N,6)=0
!          NBC(N,7)=1
        ENDDO
!C        NBC(1,7)=0
!
!IPK MAY02      ELSEIF(ITEQV(MAXN) == 10) THEN
      ELSEIF(ITEQV(MAXN) == 11) THEN
!ipk jun05      vels+temp
        DO 2281 N=1,NP
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,6)=0
 2281   CONTINUE
!IPK MAY02      ELSEIF(ITEQV(MAXN) == 11) THEN
      ELSEIF(ITEQV(MAXN) == 12) THEN
!ipk jun05      vels+sed
        DO 2282 N=1,NP
          NBC(N,3)=0
          NBC(N,4)=0
          NBC(N,5)=0
 2282   CONTINUE
      ENDIF
!
!
      do n=1,npm
        if(icesw > 0) then
          if(icethk(n) > 0 .OR.                                         &
     &     (vel(5,n) < TMED .AND. ITPAS > 0)) then
            if(ndep(n) > 1) then
              nbc(n,5)=0
              spec(n,5)=0.
              vel(5,n)=TMED
              VDOT(5,n)=ALTM*(TMED-VOLD(5,n))-(ALPHA-1.)*VDOTO(5,n)
              vdoto(5,n)=0.
            endif
          endif
        endif
      enddo
!
!
!
!-
!...... Install special values for one-two dimension intersections
!-
!
!nis,may07      
!Add midside node to Polynom approach      
!EFa Nov06, Modifikation für 1D-Teschke-Elemente      
      do nn=1,ne
        if ((ncorn(nn) == 5 .OR. ncorn(nn) == 3) .AND.                  &
     &       imat(nn) == 89) then
          nbc(nop(nn,1),2)=0
          nbc(nop(nn,3),2)=0
        end if
      end do
!
!
      DO 260 NN=1,NE
        IF(IMAT(NN) == 0) GO TO 260
!ipk oct98 update to f90
        NTYP=NETYP(NN)
!NiS,may06: NTYP == 18 means 2D-3D-transition element???        
        IF(NTYP == 18) THEN
          DO 230 ITM=1,3
            NNL=NOP(NN,ITM+4)
            NDX=NOP(NN,12-ITM)
            ND2=NOP(NN,15-ITM)
            IF(ITM == 1) ND3=NOP(NN,20)
            IF(ITM == 3) ND3=NOP(NN,19)
!ipk may02 switch ndf to 7
            DO 229 J=1,7
              NBC(NDX,J)=-NNL
              NBC(ND2,J)=-NNL
              IF(ITM /= 2) NBC(ND3,J)=-NNL
  229       CONTINUE
  230     CONTINUE
!NiS,may06: NCORN == 5 and IMAT < 901 means 1D-2D-transition element        
        ELSEIF(NCORN(NN) == 5 .AND. IMAT(NN) < 901) THEN
          NLN=NOP(NN,3)
          DO 235 K=4,5
            IF(NOP(NN,K) == 0) GO TO 240
            IF(NLN > NOP(NN,K)) NLN=NOP(NN,K)
  235     CONTINUE
  240     CONTINUE
          DO 250 K=3,5
            IF(NLN == NOP(NN,K)) GO TO 250
            ND=NOP(NN,K)
            IF(ND == 0) GO TO 250
            NBC(ND,1)= -NLN
            NBC(ND,2)= -NLN
            NBC(ND,3)=-NLN
            NBC(ND,4)=-NLN
            NBC(ND,5)=-NLN
            NBC(ND,6)=-NLN
  250     CONTINUE
!
!
!IPK JUN05     test for totally submerged control structure
!              set equations to by-pass
!
!NiS,may06,com: IMAT > 903 and IMAT < 990 means control structure        
        ELSEIF(IMAT(NN) > 903  .AND. IMAT(NN) < 990) THEN
!
!ipk JUN05 first analyse 2-d structures
!IPK NOV06 MOVE UP
          IF(NCORN(NN) == 8) THEN
            SIDL1=DIST(NOP(NN,1),NOP(NN,3))
            SIDL2=DIST(NOP(NN,7),NOP(NN,5))
            NMID1=NOP(NN,2)
            NMID2=NOP(NN,6)
            IF(ELTON(NMID1,1) /= 0) THEN
              NAD1=ELTON(NMID1,1)
            ELSE
              NAD1=ELTON(NMID1,2)
            ENDIF            
            IF(ELTON(NMID2,1) /= 0) THEN
              NAD2=ELTON(NMID2,1)
            ELSE
              NAD2=ELTON(NMID2,2)
            ENDIF            
!IPK NOV06          IF(NCORN(NN) == 8) THEN
!
            DO K=1,3
              N1=NOP(NN,K)
              N2=NOP(NN,8-K)
              WV1=SQRT(VEL(1,N1)**2+VEL(2,N1)**2)
              WV2=SQRT(VEL(1,N2)**2+VEL(2,N2)**2)
              ITP=1
              IF(N1 > 0) THEN
                nm = imat (nn) - 900
                IF(njt (nm) == 12) THEN
                  widem=sqrt((cord(n2,2)-cord(n1,2))**2+                &
     &            (cord(n2,1)-cord(n1,1))**2)
!IPK FEB07
                  HL1=VEL(3,N1)
                  HL2=VEL(3,N2)
                  CALL WFORM(Q1,HL1,WSLL(N1),WV1,HL2                    &
     &           ,WSLL(N2),WV2,WHGT(N1),WLEN(N1),ITP,widem)
                ELSEif (njt (nm) == 11) then
                  CALL WTFORM (Q1, imat(nn),                            &
     &                         WSLL (N1), WSLL (N2),                    &
     &                         mcord(nn,1),mcord(nn,2))
                elseif (njt (nm) == 10) then
                  Q1 = cstrcQ_fromQCurves (imat (nn),                   &
     &                   WSLL (N1) + wv1**2/(2.0*grav),                 &
     &                   WSLL (N2) + wv2**2/(2.0*grav))
                ENDIF
!
                IF(NTMREF(IMAT(NN)) /= 0) THEN
                  CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET   &
     &             ,QFACT)
!IPK DEC05 ADD QFACT
                  Q1=Q1*QFACT
                  IF(ISWTOF == 1) THEN
                    Q1=0.0
                  ENDIF
                ENDIF
!
                Q1C(K)=Q1
!
!IPK DEC05                if(inovel > 0) then 
!IPK DEC05                  IF(ABS(Q1) > 0) then
!IPK DEC05                    NBC(N2,INOVEL+3)=-N1
!IPK DEC05                  ENDIF
!IPK DEC05                endif
!
!       for submerged elements  set active equations
!
                IF(ISUBMEL(nn) == 1) THEN
                  N2=NOP(NN,8-K)
!IPK  TEST
                   IF((IBN(N1) == 20 .OR. IBN(N1) == 11) .AND.          &
     &                 ISUBM(N1) == 1) THEN
                    if(inovel == 0) then
                      NBC(N1,1)=1
                      NBC(N1,2)=1
                      NBC(N2,1)=1
                      NBC(N2,2)=1
                    endif
                    if(inovel > 0) then
                      IF(ABS(Q1) > 0) then
                        NBC(N2,INOVEL+3)=-N1
                      ENDIF
                    endif
                  ELSE
                    if(inovel == 0) then
                      NBC(N1,1)=1
                      NBC(N2,1)=1
                    endif
                    NBC(N1,2)=0
                    NBC(N2,2)=0
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
!
         QFLUX=SIDL1*(Q1C(1)+Q1C(2)*4.+Q1C(3))/6.
         IF(QFLUX > 0) THEN
         FLUX=QFLUX*(VEL(3+INOVEL,NOP(NN,2))-VEL(3+INOVEL,NOP(NN,6)))
         ELSE
         FLUX=QFLUX*(VEL(3+INOVEL,NOP(NN,6))-VEL(3+INOVEL,NOP(NN,2)))
         ENDIF
         IF(QFLUX > 0.) THEN
           EXTLDEL(NAD2)=EXTLDEL(NAD2)+FLUX/AREA(NAD1)
         ELSE
           EXTLDEL(NAD1)=EXTLDEL(NAD1)-FLUX/AREA(NAD1)
         ENDIF
!
!         WRITE(203,'(3I6,6F15.6)') MAXN,NN,3+INOVEL,
!     +    QFLUX,FLUX,EXTLDEL(NAD1) 
!     +   ,EXTLDEL(NAD2),VEL(3+INOVEL,NOP(NN,2)),VEL(3+INOVEL,NOP(NN,6)) 
!       now do middle elements
!
            N1=NOP(NN,4)
            N2=NOP(NN,8)
            NBC(N1,INOVEL+3)=-NOP(NN,3)
            NBC(N2,INOVEL+3)=-NOP(NN,1)
            IF(ISUBMEL(nn) == 1) THEN
              IF(IBN(NOP(NN,4)) == 20                                   &
     &          .AND. ISUBM(NOP(NN,4)) == 1) THEN
                if(inovel == 0) then
                  NBC(NOP(NN,4),1)=1
                  NBC(NOP(NN,4),2)=1
                endif
              ELSE
                if(inovel == 0) then
                  NBC(NOP(NN,4),1)=1
                endif
                NBC(NOP(NN,4),2)=0
              ENDIF
            ELSE
              if(inovel == 0) then
                NBC(NOP(NN,4),1)=1
              endif
            ENDIF          
            IF(ISUBMEL(nn) == 1) THEN
              IF(IBN(NOP(NN,8)) == 20                                   &
     &          .AND. ISUBM(NOP(NN,8)) == 1) THEN
                if(inovel == 0) then
                  NBC(NOP(NN,8),1)=1
                  NBC(NOP(NN,8),2)=1
                endif
              ELSE
                if(inovel == 0) then
                  NBC(NOP(NN,8),1)=1
                  NBC(NOP(NN,8),2)=0
                endif
              ENDIF
            ELSE
              if(inovel == 0) then
                NBC(NOP(NN,8),1)=1
              endif
            ENDIF
!
!
!if (ncn /= 8)
          ELSE 
!       now do 1-d structures
!
!NiS,may06,com: initialize equations for 1D-control structure elements      
!NiS,may06,com: get corner nodes      
            N1=NOP(NN,1)
            N2=NOP(NN,3)
            NAD1=ELTON(N1,1)
            NAD2=ELTON(N2,1)
!
!nis,feb08: Why do we need the water stage equations?            
!nbc(n1, 3) = 0            
!nbc(n2, 3) = 0            
!-            
!
            IF(N1 > 0) THEN
              nm = imat (nn) - 900
              IF(njt (nm) == 12) THEN
!IPK FEB07
                  HL1=VEL(3,N1)
                  HL2=VEL(3,N2)
                CALL WFORM(Q1,HL1,WSLL(N1),WV1,HL2                      &
     &           ,WSLL(N2),WV2,WHGT(N1),WLEN(N1),ITP,widem)
              ELSEif (njt (nm) == 11) then
                CALL WTFORM(Q1,NCTREF(IMAT(NN)),WSLL(N1),WSLL(N2),      &
     &                      mcord (nn, 1), mcord(nn, 2))
              elseif (njt (nm) == 10) then
                Q1 = cstrcQ_fromQCurves (imat (nn),                     &
     &                 WSLL (N1) + wv1**2/(2.0*grav),                   &
     &                 WSLL (N2) + wv2**2/(2.0*grav))
              ENDIF
!
!
              IF(NTMREF(IMAT(NN)) /= 0) THEN
                CALL SWITON(NTMREF(IMAT(NN)),ISWTOF,IYRR,DAYOFY,TET     &
     &             ,QFACT)
!IPK DEC05 ADD QFACT
                Q1=Q1*QFACT
                IF(ISWTOF == 1) THEN
                  Q1=0.0
                ENDIF
              ENDIF
              Q1=Q1*(WIDTH(N1)*VEL(3,N1)                                &
     &           +(SS1(N1)+SS2(N1))*VEL(3,N1)*VEL(3,N1)/2.)
!
              IF(ISUBMEL(nn) == 1) THEN
                N2=NOP(NN,3)
                NBC(N1,1)=1
                NBC(N1,2)=0
                NBC(N2,1)=1
                NBC(N2,2)=0
              ENDIF
            ENDIF
!NiS,may06,com: get midside node            
            N1=NOP(NN,2)
!
!for polynom approach            
            if (imat(nn) /= 89) then
              IF(ISUBMEL(nn) == 1) THEN
                NBC(N1,1)=1
                NBC(N1,2)=0
              ELSE
!nis,feb08: Activate midside node equation                
!NBC(N1,1)=1                
                NBC(N1,1)=0
!-                
              ENDIF
            end if
!
            IF(INOVEL > 0.) THEN
             N3=NOP(NN,2)
             NBC(N3,INOVEL+3)=0
             IF(Q1 > 0) THEN
              FLUX=Q1*(VEL(3+INOVEL,NOP(NN,1))-VEL(3+INOVEL,NOP(NN,3)))
             ELSE
              FLUX=Q1*(VEL(3+INOVEL,NOP(NN,3))-VEL(3+INOVEL,NOP(NN,1)))
             ENDIF
             IF(Q1 > 0.) THEN
              EXTLDEL(NAD2)=EXTLDEL(NAD2)+FLUX/AREA(NAD2)
             ELSE
              EXTLDEL(NAD1)=EXTLDEL(NAD1)-FLUX/AREA(NAD1)
             ENDIF
            ENDIF
!
          ENDIF
!IPK JUN05 FINISHED CHANGES
!
!
!NiS,may06,com: NTYP == 17 or NTYP == 27 means 1D- or 2D-laterally averaged junction element        
        ELSEIF(MOD(NTYP,10) == 7) THEN
!          WRITE(*,*) 'NN,NCORN(NN),NOP(NN,1)',NN,NCORN(NN),NOP(NN,1)
          NLN=NOP(NN,1)
!IPK JAN99          DO 251 K=2,NCORN(NN)
!IPK JAN99
          IF(NTYP == 17) THEN
            NCN=2
          ELSE
            NCN=NCORN(NN)
          ENDIF
          DO 251 K=2,NCN
!
            ND=NOP(NN,K)
            IF(ND == 0) GO TO 251
            NBC(ND,4)=-NLN
            NBC(ND,5)=-NLN
            NBC(ND,6)=-NLN
  251     CONTINUE
!          WRITE(*,*) 'ND,NLN,NBC(ND,4)',ND,NLN,NBC(ND,4)
        ENDIF
!ipk oct98 update to f90
      IMMT=IMAT(NN)
!NiS,may06,com: (Last two digits of IMAT) > 90 means ctrl.strc.elts.      
      IF(MOD(IMMT,100) > 90) THEN
        IK=1
        NCN=NCORN(NN)
        IF(NCN > 8) IK=2
        DO 255 M=1,NCN
          IF(ISHF(M,IK) > 0) THEN
            I=ISHF(M,IK)
            I=NOP(NN,I)
            J=NOP(NN,M)
            DO 254 K=1,3
              NBC(J,K)=-I
  254       CONTINUE
          ENDIF
  255   CONTINUE
      ENDIF
!
!
!nis,jan07: Add control for 1D-2D-line-transitions      
      do k = 1, 350
        NodeIsDry (k) = .false.
      end do
!
      if (maxLT /= 0) then
!
!test for all possible transitions        
        EquationAdjustment: do i = 1, MaxLT
!
!look, whether element is part of a transition, if not test next transition          
          if (NN /= TransLines(i,1)) CYCLE EquationAdjustment
!
!Get the Line Number and it's length as well as the connecting node number          
          LiNo   = TransLines (i, 2)
          LiLe   = LMT (LiNo)
          CoNode = TransLines (i, 3)
!
          do k = 1, LiLe
            nd = line (LiNo, k)
!
!first assumption: Node is not dry            
            NodeIsDry (k) = .false.
!
!TODO: Possibly rewetted nodes are not considered correctly            
!node is dry, if depth is below deactivating depth            
            if (vel (3, nd) <= dset) then
              NodeIsDry(k) = .true.
!
!node is dry, if it is a midside node and the element is deactivated            
            ELSEIF (MOD (k, 2) == 0) then
              if (imat (lineelement (i, k)) < 0) then
                NodeIsDry (k) = .true.
              endif
!
!node is dry, if it is a corner node and the two neighbouring (at the boundary the related one) elements are deactivated            
            elseif (mod (k, 2) /= 0) then
              if (k == 1) then
                if (imat (lineelement (i, k+1)) < 0)                    &
     &            NodeIsDry (k) = .true.
              elseif (k == LiLe) then
                if (imat (lineelement (i, k-1)) < 0)                    &
     &            NodeIsDry (k) = .true.
              else
                if (imat (lineelement (i, k-1)) < 0                     &
     &             .AND. imat(lineelement(i,k+1)) < 0)                  &
     &            NodeIsDry (k) = .true.
              endif
            end if
          end do
!
!initializing NLN; connectivity starts with first node of transitioning line          
          NLN = 999999999
!Get the smallest node number to reference equations to; 1st was already processed in lines before          
          DO K = 1, LiLe
            IF (NLN > Line(LiNo,K) .AND. ( .NOT. NodeIsDry(K)))         &
     &        NLN = Line(LiNo,K)
          ENDDO
!
!check for 1d-node          
!if (NLN > CoNode) NLN = CoNode          
!
!assign references to all nodes of the connection line          
          equationassigning: DO K=1, LiLe
!
!get copy of node number            
            nd = line (LiNo, k)
!
!deactivating nodes that are dry            
            if (NodeIsDry (k)) then
              do eqdof = 1, 6
                nbc(nd,eqdof) = 0
              end do
            else
!Initialize the momentum equations. The velocities are a result of the distribution calculation. This is done for every node              
!
!nis,jul07: activate the velocity component in main direction              
              NBC(ND,1) = 1
              NBC(ND,2) = 0
!
!gap in line definition leads to error              
              IF (nd == 0 .OR. NLN == 0) THEN
                WRITE(*,*) 'ERROR - Definition gap in transition line.'
                WRITE(*,*) 'the ', k, 'th slot has a zero entry'
                WRITE(*,*) 'Have a look at your line definition'
                WRITE(*,*) 'Program can not be executed'
                WRITE(*,*) 'STOP'
                STOP
!the same node must not be processed on              
              ELSEIF (NLN == nd) THEN
                nbc (nd, 3) = 1
                nbc (nd, 4) = 0
                nbc (nd, 5) = 0
                nbc (nd, 6) = 0
                CYCLE equationassigning
!
!this counts for corner nodes, where the continuity equation is active              
              ELSEIF (NLN /= nd .AND. MOD (k, 2) /= 0 ) THEN
!assign equation references                
                DO eqdof = 3, 6
                  NBC (ND, eqdof) = -NLN
                ENDDO
!
!this counts for midside nodes, where the continuity equation is not active              
              ELSEIF (NLN /= nd .AND. MOD (k, 2) == 0 ) then
!assign equation references                
                DO eqdof = 3, 6
                  NBC (ND, eqdof) = 0
                ENDDO
!
              ENDIF
            endif
          ENDDO equationassigning
!
!Process 1D-node, because this can't be member of the line          
          nbc(CoNode,1) = 1
          nbc(CoNode,2) = 0
          nbc(CoNode,3) = 1
          DO eqdof = 4,6
            NBC(CoNode,eqdof) = 0
          ENDDO
!
          EXIT EquationAdjustment
        ENDDO EquationAdjustment
      end if
!
  260 CONTINUE
!
!
!
!    Insert velocity distribution for line types with negative pointer 
!
!IPK JAN98 EXPAND TEST            8 AND 9
      IF(ITEQV(MAXN) /= 2 .AND. ITEQV(MAXN) /= 8                        &
     &                      .AND. ITEQV(MAXN) /= 9) THEN
      DO 263 K=1,IHGNN
        IF(JLIN(K) > 0) THEN
          JL=JLIN(K)
          MAX=LMT(JL)
          MIDC=(MAX-1)/2
          IF(MOD(MIDC,2) == 0) MIDC=MIDC+1
          MID=LINE(JL,MIDC)
          DO 262 J=1,MAX
            NA=LINE(JL,J)
!IPK OCT98 CONVERT TO F90
            NFTYP=NFIX(NA)
            NRD=MOD(NFTYP,100)
            IF(NA /= MID) THEN
              NBC(NA,1)=-MID
              NBC(NA,2)=0
              NFIX(NA)=NRD
!ipk feb98
              if(ndep(na) > 0) then
                n=nref(na)+1
                l=nref(na)+ndep(na)-1
                do kk=n,l
                  nbc(kk,1)=-mid
                  nbc(kk,2)=0
                  nfix(kk)=nrd
                  vscale(kk)=vscale(na)
                enddo
              endif
!ipk feb98
            ELSE
              NBC(MID,1)=1
              NBC(MID,2)=0
!ipk feb98
              if(ndep(mid) > 0) then
                n=nref(mid)+1
                l=nref(mid)+ndep(mid)-1
                do kk=n,l
                  nbc(kk,1)=-mid
                  nbc(kk,2)=0
                  nfix(kk)=nrd
                  vscale(kk)=vscale(mid)
                enddo
              endif
!ipk feb98
            ENDIF
            IF(J > 1 .AND. J < MAX) THEN
              NL=LINE(JL,J-1)
              NQ=LINE(JL,J+1)
              DX=CORD(NQ,1)-CORD(NL,1)
              DY=CORD(NQ,2)-CORD(NL,2)
              IF(DY < 0.) THEN
                DX=-DX
                DY=-DY
              ENDIF
              ALFA(NA)=ATAN2(-DX,DY)
              IF (J > 2) THEN
                IF(ALFA(NA)-ALFA(NL) > 1.5708) THEN
                  ALFA(NA)=ALFA(NA)-3.14159
                ELSEIF(ALFA(NA)-ALFA(NL) < -1.5708) THEN
                  ALFA(NA)=ALFA(NA)+3.14159
                ENDIF
              ENDIF
!ipk feb98
              if(ndep(na) > 0) then
                n=nref(na)+1
                l=nref(na)+ndep(na)-1
                do kk=n,l
                  alfa(kk)=alfa(na)
                enddo
              endif
!ipk feb98
            ENDIF
  262     CONTINUE
          IF(MIDC > 1) THEN
            MIDN=LINE(JL,MIDC-1)
!IPK OCT98 CONVERT TO F90
            NFTYP=NFIX(MIDN)
            NRD=MOD(NFTYP,100)
            NFIX(MIDN)=200+NRD
          ENDIF
          MIDP=LINE(JL,MIDC+1)
!IPK OCT98 CONVERT TO F90
          NFTYP=NFIX(MIDP)
          NRD=MOD(NFTYP,100)
          NFIX(MIDP)=200+NRD
!
!  Correct angles of the two sides
!
          J1=LINE(JL,1)
          J2=LINE(JL,2)
          IF(ALFA(J1)-ALFA(J2) > 1.5708) THEN
            ALFA(J1)=ALFA(J1)-3.14159
          ELSEIF(ALFA(J1)-ALFA(J2) < -1.5708) THEN
            ALFA(J1)=ALFA(J1)+3.14159
          ENDIF
          J1=LINE(JL,MAX)
          J2=LINE(JL,MAX-1)
          IF(ALFA(J1)-ALFA(J2) > 1.5708) THEN
            ALFA(J1)=ALFA(J1)-3.14159
          ELSEIF(ALFA(J1)-ALFA(J2) < -1.5708) THEN
            ALFA(J1)=ALFA(J1)+3.14159
          ENDIF
!ipk feb98
          if(ndep(j1) > 0) then
            n=nref(j1)+1
            l=nref(j1)+ndep(j1)-1
            do kk=n,l
              alfa(kk)=alfa(j1)
            enddo
          endif
          if(ndep(j2) > 0) then
            n=nref(j2)+1
            l=nref(j2)+ndep(j2)-1
            do kk=n,l
              alfa(kk)=alfa(j2)
            enddo
          endif
!ipk feb98
        ENDIF
  263 CONTINUE
!IPK JAN98
      ENDIF
!
!
!
!...... Prepare for elements that interface 2D to 3D
!
      IF(ITEQV(MAXN) /= 5) THEN
        DO 270 N=1,NP
          IF(NTHREE(N) == 1) GO TO 270
!
!...... This node has a 2-d definition
!
!NiS,may06: if NREF(N) was zero, N is a 2D-node          
          M=NREF(N)+1
          IF(M == 1) GO TO 270
!
!...... Now we know this node is also 3D
!
          IF(M > NP) GO TO 270
          MT=M+NDEP(N)-2
!
!...... Wipe out all the lower equations
!
          DO 265 L=M,MT
            NBC(L,1)=0
            NBC(L,2)=0
            IF(NDF > 3) THEN
              DO 264 KK=4,NDF
                NBC(L,KK)=0
  264         CONTINUE
            ENDIF
  265     CONTINUE
  270   CONTINUE
      ENDIF
!
!IPK JAN99 ADD FOR JUNCTIONS
!-
!...... Use junction to establish boundary values
!-
!-
      DO 285 N=1,NE
        IF(IMAT(N) > 900 .AND. IMAT(N) < 1000) THEN
          N1=NOP(N,1)
          IF(NDEP(N1) > 1 .OR. N > NEM) THEN
            N2=NOP(N,2)
!for polynom approach            
            if (imat(n) /= 89) then
!ipk may02 switch  to 7
            DO 282 K=1,7
!ipk may02            DO 282 K=1,6
              NBC(N2,K)= -N1
  282       CONTINUE
            endif
            N3=NOP(N,3)
!uuu
              nbc(n3,4) = -n1
              NBC(N3,5) =-N1
              NBC(N3,6) =-N1
!ipk may02
              NBC(N3,7) =-N1
!
            SPEC(N3,3)=VEL(3,N1) + AO(N1)-AO(N3)
!
!
            if (imat(n) /= 89) then
            WZ = WIDTH(N3) + (SS1(N3)+SS2(N3))*(CORD(N3,3)-AO(N3))      &
     &                    * (VEL(3,N3)/(ELEV-AO(N3)) ) 
!
            SIDN(N1) = 2.*WZ/(XTLN(N1)+XTLN(N2))
            SIDN(N2)=SIDN(N1)
            endif
            CXX=COS(ALFA(N3))
            SAA=SIN(ALFA(N3))
            VTB=-(VEL(1,N3)*CXX+VEL(2,N3)*SAA)*DIR(N3)
            write(76,*) 'load3',n1,n2,sidn(n1),vtb
!yyy            IF(VTB*SIDN(N1) < 0.) THEN
!uuu            IF ( (VTB*SIDN(N1) < 0. .OR. iteqv(maxn) /= 2) ) THEN
!u              NBC(N3,4)=0  
!u              spec(n3,4)=vel(4,n1)
!u              vel(4,n3)=spec(n3,4)
!u            ENDIF
          ENDIF
        ENDIF
  285 CONTINUE
!
!
!IPK JAN99 END FOR JUNCTIONS
!
!-
!......INITIALIZE FACTOR FOR VERTICAL DISTRIBUTION
!-
      SCFC=6./(UMIN+1.+4.*(UMIN+(1.-UMIN)*0.5**PWERIN))
      DO 320 N=1,NPSAV
      FCTV(N)=1.
      FCTS(N)=1.
      UDST(N)=1.0
      UUDST(N)=1.0
      VVDST(N)=1.0
      VDST(N)=1.0
      SDST(N)=1.0
      TDST(N)=1.0
      SEDST(N)=1.0
  320 CONTINUE
!-
!...... Prepare for specified distribution of horizontal velocities
!-
      IF(ITEQV(MAXN) /= 5) THEN
        DO 350 N=1,NP
          IF(NTHREE(N) == 1) GO TO 330
!
!...... This node has a 2-d definition
!
          M=NREF(N)+1
          IF(M == 1) GO TO 330
!
!...... Now we know this node is also 3D
!
          IF(M > NP) GO TO 330
!
!...... Set interface factor
!
          CNMIN = CINT(ICPON(N))
          PCERIN = CPOW(ICPON(N)) 
          SCFCC=6./(CNMIN+1.+4.*(CNMIN+(1.-CNMIN)*0.5**PCERIN))
          ZL=(CORD(N,3)-AO(N))/(ELEV-AO(N))
          FCTV(N)=SCFC*(UMIN+(1.-UMIN)*ZL**PWERIN)
          FCTS(N)=SCFCC*(CNMIN+(1.-CNMIN)*ZL**PCERIN)
          UDST(N)=FCTV(N)
          VDST(N)=FCTV(N)
          sdst(n) = fcts(n) 
          tdst(n) = fcts(n) 
          sedst(n) = fcts(n) 
          MT=M+NDEP(N)-2
          DO 325 L=M,MT
            NBC(L,1)=-N
            NBC(L,2)=-N
            IF(NDF > 3) THEN
              NBC(L,4)=-N
              NBC(L,5)=-N
              NBC(L,6)=-N
!ipk may02
              NBC(L,7)=-N
            ENDIF
            ZL=(CORD(L,3)-AO(L))/(ELEV-AO(L))
            IF(ZL < -0.01) THEN
!ipk sep04
              CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(75,*) 'Error in vertical coordinates at',L,ZL
              WRITE(*,*) 'Error in vertical coordinates at',L,ZL
              STOP
            ENDIF
            IF(ZL > 0.) THEN
              FCTV(L)=SCFC*(UMIN+(1.-UMIN)*ZL**PWERIN)
              FCTS(L)=SCFCC*(CNMIN+(1.-CNMIN)*ZL**PCERIN)
            ELSE
              FCTV(L)=SCFC*UMIN
              FCTS(L)=SCFCC*CNMIN
            ENDIF
            UDST(L)=FCTV(L)
            VDST(L)=FCTV(L)
            IF(IENT == 1) THEN
              VEL(1,L)=VEL(1,N)*FCTV(L)/FCTV(N)
              VEL(2,L)=VEL(2,N)*FCTV(L)/FCTV(N)
              VEL(4,L)=VEL(4,N)*FCTS(L)/FCTS(N)
              VEL(5,L)=VEL(5,N)*FCTS(L)/FCTS(N)
              VEL(6,L)=VEL(6,N)*FCTS(L)/FCTS(N)
!
              VOLD(1,L)=VOLD(1,N)*FCTV(L)/FCTV(N)
              VOLD(2,L)=VOLD(2,N)*FCTV(L)/FCTV(N)
              VOLD(4,L)=VOLD(4,N)*FCTS(L)/FCTS(N)
              VOLD(5,L)=VOLD(5,N)*FCTS(L)/FCTS(N)
              VOLD(6,L)=VOLD(6,N)*FCTS(L)/FCTS(N)
!
              VDOT(1,L)=VDOT(1,N)*FCTV(L)/FCTV(N)
              VDOT(2,L)=VDOT(2,N)*FCTV(L)/FCTV(N)
              VDOT(4,L)=VDOT(4,N)*FCTS(L)/FCTS(N)
!ipk dec98 reverse 5 and 6 to corect bug
              VDOT(5,L)=VDOT(5,N)*FCTS(L)/FCTS(N)
              VDOT(6,L)=VDOT(6,N)*FCTS(L)/FCTS(N)
!
              VDOTO(1,L)=VDOTO(1,N)*FCTV(L)/FCTV(N)
              VDOTO(2,L)=VDOTO(2,N)*FCTV(L)/FCTV(N)
              VDOTO(4,L)=VDOTO(4,N)*FCTS(L)/FCTS(N)
              VDOTO(5,L)=VDOTO(5,N)*FCTS(L)/FCTS(N)
              VDOTO(6,L)=VDOTO(6,N)*FCTS(L)/FCTS(N)
            ENDIF
  325     CONTINUE
  330     CONTINUE
          M=abs(NSURF(N))
          IF(M < 1) THEN
            NBC(N,3)=0
          ELSEIF(NBC(N,3) == -99) THEN
            NBC(N,3)=-M
          ENDIF
  350   CONTINUE
      ELSE
        CALL DISFCT
      ENDIF
!
!...... Dropout equations if possible
!
!     IF(MAXN > 1  .AND. ITEQV(MAXN) == ITEQV(MAXN-1)) THEN
!       DO 353 NN=1,NP
!         DO 352 M=1,NDF
!           IF(NDROP(NN,M) == 1) NBC(NN,M)=0
! 352     CONTINUE
! 353   CONTINUE
!     ENDIF
!
!...... Insert zero bottom velocity if requested
!
      IF(IZB == 1) THEN
        DO 297 J=1,NPM
          K=NREF(J)+1
          IF(K == 1) GO TO 297
          L=NREF(J)+NDEP(J)-1
          IF(L < K) GO TO 297
          IF(NBC(L,1) < 0) GO TO 297
          NBC(L,1)=0
          NBC(L,2)=0
  297   CONTINUE
      ENDIF
!IPK NOV98
!
!...... Insert zero surface velocity if requested
!
      IF(IZERS == 1) THEN
        DO J=1,NPM
          NBC(J,1)=0
          NBC(J,2)=0
        ENDDO
      ENDIF
!IPK NOV99
!
!...... Insert switching for equation removal when depth below TRANSIT       
!
      IF(ITRANSIT == 1) THEN
        DO J=1,NPM
          IF(ICOLLAP(J) /= 0) THEN
            K=NREF(J)+1
            L=NREF(J)+NDEP(J)-1
            DO M=K,L
              DO I=1,NDF
                if(i /= 3)  NBC(M,I)=-j
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!IPK NOV99 End addition
!
!IPK MAY02 Eliminate mid-sides
      IF(ICK == 7) THEN
        do n=1,ne
          ncn=ncorn(n)
          if(imat(n) < 1000 .OR. imat(n) > 5000) then
            ilk=1
            IF(NCN == 15) ILK=2
            IF(NCN == 13) ILK=3
            IF(NCN == 10) ILK=2
            do k=1,ncn
              if(im(k,ilk) == 0) then
                NBC(NOP(N,K),7)=0
              endif
            enddo
          endif
        enddo
      ENDIF
!
!NiS,jun06,comment: At this point the equation numbers will be assigned to the NBC-array
!IPK DEC06  ALLOW FOR EQUAL FLOW AND DEPTH BC'S
!
      DO N=1,NQLDS
        IF(IQID(N) == 1) THEN
          J=NHYDQ(N)
          NMID=(LMT(J)+1)/2
          NODMID=LINE(J,NMID)
          DO K=1,LMT(J)
            IF(LINE(J,K) /= NODMID) THEN
              NBC(LINE(J,K),1)=-NODMID
                VEL(1,LINE(J,K))=VEL(1,NODMID)
                VEL(2,LINE(J,K))=VEL(2,NODMID)
                VOLD(1,LINE(J,K))=VOLD(1,NODMID)
                VOLD(2,LINE(J,K))=VOLD(2,NODMID)
              IF(MOD(K,2) == 1) THEN
                NBC(LINE(J,K),3)=-NODMID
                VEL(3,LINE(J,K))=VEL(3,NODMID)
                VOLD(3,LINE(J,K))=VOLD(3,NODMID)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!
!...... Rearrange array
!
!
      NSZF=0
      N = 0
!
      DO 300 NN = 1, NP
        N = N + 1
!
!**********************************************************************************************************************************
!EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS
!**********************************************************************************************************************************
!nis,may07,comment: This unconsiderable loop assigns the global equation numbers to the nodal degrees of freedom. There's only a number applied,
!                   if the nbc-values of the nodal degree of freedom is equal 1
!**********************************************************************************************************************************
!EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS   EQUATION NUMBERS
!**********************************************************************************************************************************
!IPK MAY02 SWITCH NDF TO 7
        DO 299 M = 1, 7
!ycw aug96
!c          if(iactv(n,m) == 0) nbc(n,m)=0  not active at present
          IF( NBC(N,M) /= 1 ) GO TO 299
          NSZF=NSZF+1
          NBC(N,M)=NSZF
  299   CONTINUE
  300 CONTINUE
!
!-
!...... Reduce from upto 6 column to the 4th column
!
      IF(ICK > 4) THEN 
        DO 302 N=1,NP
          NBC(N,4)=NBC(N,ICK)
          NBC(N,ICK)=0
  302   CONTINUE
      ENDIF
!
!...... Force equation numbers for multi-node intersection
!-
      DO 3111 KKK=1,2
      DO 310 N=1,NP
!IPK MAY02 SWITCH NDF TO 7
        DO 305 M=1,7
          IF(NBC(N,M) < 0) THEN
!IF(NBC(N,M) < -1) THEN          
!
!
            NRF=-NBC(N,M)
            itmp = nbc(nrf,m)
            if (nrf > n .AND. itmp < 0)  goto 305
!
            NBC(N,M)=NBC(NRF,M)
          ENDIF
  305   CONTINUE
  310 CONTINUE
 3111     continue
!
!nis,jun07: Deactivated for the moment, has to be reactivated, when everything else is debugged
!ccycw added for full 3D optimisation 19/06/96 currently testing
!cipk feb07 reduce test to make it entered only once
!      if(ioptim < 1) then
!C         write(76,*) 'load ivrsid1',ivrsid
!         call optim
!         ioptim=1+IOPTIM
!C         write(76,*) 'load ivrsid2',ivrsid
!         WRITE(76,6020) (NFIXH(N),N=1,NE)
! 6020    FORMAT('RT      ',9I8)
!         DO N=1,NE
!            NN=NFIXH(N)
!            if (nn /= 0)  then
!               NREORD(NN)=N
!            endif
!         enddo
!      endif
!-
!
!ipk oct96  go to calculate DRODXIN
      call presr
      do n=1,ne
        nn=nelord(n)
!ipk feb98
        if(nn > 0) then
          if(ncorn(nn) > 8) call roin(nn)
        endif
!ipk feb98 end change
      enddo
!ipk oct96 end changes
!
!IPK AUG05 NLSTEL MOVED TO FRONT
!
      IF(ITIMTH == 1) THEN
       MR1=NSZF+1
       IR1MAX=MR1
!
       ALLOCATE (IPOINT(MR1),NLSTEL(0:MR1),R1(0:MR1),LCS(MR1),          &
     & LPS(MR1),rkeep(0:mR1),ekeep(mR1),rkeepeq(mR1))
       ITIMTH=2
!ipk jun05
      ELSE
        IF(MR1 < NSZF) THEN      
          DEALLOCATE  (IPOINT,R1,NLSTEL,RKEEP,LCS,LPS,EKEEP,RKEEPEQ)
          MR1=NSZF+1
          IR1MAX=MR1
          ALLOCATE (IPOINT(MR1),NLSTEL(0:MR1),R1(0:MR1),LCS(MR1),       &
     &    LPS(MR1),rkeep(0:mR1),ekeep(mR1),rkeepeq(mR1))
        ENDIF
      ENDIF
!
!initialize right-hand-side vector      
      r1 = 0.0d0
      rkeep = 0.0d0
      ekeep = 0.0d0
      rkeepeq = 0.0d0
!
!
!      DO N=1,NP
!        WRITE(75,'(8I5)') N,(NBC(N,M),M=1,7)
!      ENDDO
      WRITE(*,6007) NSZF
      WRITE(75,6007) NSZF
 6007 FORMAT( // 20X, '.....TOTAL NUMBER OF ACTIVE SYSTEM EQUATIONS =', &
     &  I6 )
      IF( NSZF <= IR1MAX ) RETURN
!ipk sep04
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')
      WRITE(*,6008) NSZF,IR1MAX
      WRITE(75,6008) NSZF,IR1MAX
 6008 FORMAT( / 10X, 'TOO MANY EQUATIONS..STOP CALLED..'/               &
     & 10X,'EQUATIONS =',I6,' ..MAX ALLOWED..',I6)
      STOP
      END
