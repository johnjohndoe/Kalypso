CIPK  LAST UPDATE AUG 30 2006 ADD QIN FOR CONSV AND AVEL LOADING FOR CLAY OPTION
CNiS  LAST UPDATE APR XX 2006 Adding flow equation of Darcy-Weisbach
CIPK  LAST UPDATE DEC 22 2005 MAKE INITIAL EXTL CALCILATION ONLY FOR ICK=6
CIPK  LAST UPDATE SEP 29 2005 MAKE ALP1 AND ALP2 INTERPOLATION LINEAR
cipk  last update june 27 2005 add control structure option
CIPK  LAST UPDATE SEP 26 2004  ADD MAH AND MAT OPTION
CIPK  LAST UPDATE MAY 03 2004 ALLOW FOR LOAD APPLIED ONLY AS MASS
CIPK  LAST UPDATE AUG 06 2003 ADD TEST TO REMOVE STRESSES WHEN DRY
cipk  LAST UPDATE jun 29 2003 add STRESS component
cipk  last update jan 13 2002 add momentum to element sources
CIPK  LAST UPDATE SEP 30 2002 ADD ICE FORMULATION
cipk  last update may 03 2003 reduce grate and srcsnk to zero when IEDROP active
CIPK  LAST UPDATE MAR 18 2003 add diffusion switch ( default of  0 uses old formulations
CIPK  LAST UPDATE SEP  4 2002  ADD LOGIC FOR WAVE SENSITIVE FRICTION
CIPK  LAST UPDATE AUG 28 2002 SET WIND STRESS AND WAVE STRESS TO ZERO BELOW A THRESHOLD DEPTH
cipk  last update aug 14 2002 set wind and wave stress to zero for dry areas
CIPK  LAST UPDATE MAY 28 2002 ADD SURFACE STRESS INPUT
CIPK  LAST UPDATE JAN 15 2002 ADD SIDFF FOR COLLAPSING CASE
CIPK  LAST UPDATE APR 20 2001 MOVE INITIALIZATION OF VOLS TO LATER ON 
CIPK  LAST UPDATE APR 02 2001 FIX AZER FOR BOUNDARIES
CIPK  LAST UPDATE MAR 26 2001 REPLACE SIDF(NN) WITH SIDFT  
CIPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
cipk  last update Jan 6 1999 fix azer calculation
cipk  last update Nov 12 1999 allow for collapsing 3-d to 2-d fix bug for salinity
cipk  last update Mar 17 1999 fix bug in bank friction
cipk  last update Nov 12 add surface friction
cipk  last update Aug 6 1998 complete division by xht for transport eqn
cipk  last update Jan 21 1998
cipk  last update Dec 16 1997
C     Last change:  WP   29 Aug 2007    3:02 pm
CIPK  LAST UPDATED NOVEMBER 13 1997
cipk  last update Jan 22 1997
cipk  last update Oct 1 1996 add new formulations for EXX and EYY
Cipk  last update Aug 26 1996 correct bug in element loads
CIPK  LAST UPDATED SEP 7 1995
      SUBROUTINE COEF2NT(NN,NTX)
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
      USE BLKSSTMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE PARAKalyps
      SAVE

      REAL (KIND = 8) :: HS, HM, DUM1, hm1

C

!nis,jun07: Changes for matrix output
      INTEGER :: dca
      INTEGER :: nbct (1:32,1:2)
      CHARACTER (LEN =  1) :: sort(1:32)
      CHARACTER (LEN = 16) :: FMT1
      CHARACTER (LEN = 34) :: FMT2
!-

cycw aug94 add double precision salt
      REAL*8 SALT
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK SEP02
CIPK AUG05      INCLUDE 'BLK11.COM'
      INCLUDE 'BLKH.COM'
      INCLUDE 'BLKE.COM'
      INCLUDE 'BLKS.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
CIPK AUG05      INCLUDE 'BLKSST.COM'
cipk jun05
CIPK AUG05      INCLUDE 'BLKSUB.COM'
c      INCLUDE 'RKEP.COM'

      REAL*8 WAITX,WAITT,WAITR,WAITTH,WAITRH
      REAL*8 DHDX,DHDZ,DAODX,DAODZ,H,AZER,XHT,GHC,F,FRN,FRNX,FRNZ
      REAL*8 TEMP,HP,HP1,DERR

      COMMON /WATP/ WAITT(7),WAITR(9),WAITTH(16),WAITRH(16)
C-
cipk apr05
      common /epor/ efpor

CIPK JUN03
C	COMMON /STR/
C     +  STRESS(MNP,2),STR11(MNP),STR21(MNP),STR10(MNP),STR20(MNP)

      COMMON F(80),
     1 XN(8),DNX(8),DNY(8),XM(4),DMX(4),DMY(4),XL(8),YL(8)
     2,XO(8),DOX(8),DOY(8),
     3  WAITX(16),DL(2,2)
     4 ,VXX(8),VY(8),VDX(8),VDY(8),ST(8),SDT(8),UBFC(8),VBFC(8)
     5,efpornn(4)
cipk apr05 add line above
C-
      DIMENSION NCON(20),FTF(2),PROJL(8)
CIPK SEP96 ADD PROJL
C-
CIPK sep02
      COMMON /ICE2/ GSICE,GSQLW,QWLI(8),THKI(8)

      REAL J11,J12,J21,J22
C-
      DATA FCOEF/14.47/,THRESH/1.0E-3/,PI/3.14159/
C
CIPK MAR05
cWP      data  pi/3.14159/

      IF (GRAV .LT. 32.)  THEN
        FCOEF = GRAV
        CVF2=3.28
        CVFCT=516./3.28
      ELSE
        FCOEF = GRAV/2.208
        CVF2=1.0
        CVFCT=1.0
      ENDIF
      ROAVG=1.935

CIPK MAR03  REPLACE TH(NN) WITH THNN

      THNN=TH(NN)
	 
      IF (GRAV .LT. 32.)  ROAVG = 516. * 1.935
C
C-
C-.....ASSIGN PROPER COEFS.....
C-
cipk jan98      AREA(NN)=0.
CIPKNOV97 ADD TVOL
CIPK APR 01 INITIALISATION MOVED FURTHER ON 
C      TVOL(NN)=0.

      IF(ITEQV(MAXN) .EQ. 5) THEN
        DO 61 N=1,8
          NCON(N)=NOPS(NN,N)
          IF(NCON(N) .NE. 0) NCN=N
   61   CONTINUE
      ELSE
        NCN=NCORN(NN)
        DO 63 N=1,NCN
          NCON(N)=NOP(NN,N)
   63   CONTINUE
      ENDIF
c
cipk sep96 move up thislogic
c
CIPK AUG06 ADD LOGIC TO AVE DEPRAT ETC
      IF(LSS .GT. 0  .AND.  IAVEL .EQ. 1) THEN
        IF(NCN .EQ. 6) THEN
      edotm=-(edot(NOP(NN,1))+edot(NOP(NN,3))+edot(NOP(NN,5)))/6.+        
     +  (edot(NOP(NN,2))+edot(NOP(NN,4))+edot(NOP(NN,6)))/2.    
      seratm=-(serat(NOP(NN,1))+serat(NOP(NN,3))+serat(NOP(NN,5)))/6.+        
     +  (serat(NOP(NN,2))+serat(NOP(NN,4))+serat(NOP(NN,6)))/2.   
      depratm=
     +-(deprat(NOP(NN,1))+deprat(NOP(NN,3))+deprat(NOP(NN,5)))/6.+        
     +  (deprat(NOP(NN,2))+deprat(NOP(NN,4))+deprat(NOP(NN,6)))/2.  
        elseif(ncn .eq. 8)then
      edotm=-(edot(NOP(NN,1))+edot(NOP(NN,3))+edot(NOP(NN,5))+
     +            edot(NOP(NN,7)))/12.+ 
     +  (edot(NOP(NN,2))+edot(NOP(NN,4))+edot(NOP(NN,6))+
     +            edot(NOP(NN,8)))/3.  
      seratm=-(serat(NOP(NN,1))+serat(NOP(NN,3))+serat(NOP(NN,5))+
     +            serat(NOP(NN,7)))/12.+ 
     +  (serat(NOP(NN,2))+serat(NOP(NN,4))+serat(NOP(NN,6))+
     +            serat(NOP(NN,8)))/3.  
      depratm=-(deprat(NOP(NN,1))+deprat(NOP(NN,3))+deprat(NOP(NN,5))+
     +            deprat(NOP(NN,7)))/12.+ 
     +  (deprat(NOP(NN,2))+deprat(NOP(NN,4))+deprat(NOP(NN,6))+
     +            deprat(NOP(NN,8)))/3.  

        endif
      ENDIF

CIPK JUN05 MOVE LOOP
      NEF=NCN*NDF
      DO  I=1,NEF
        F(I) = 0.0
        DO  J=1,NEF
          ESTIFM(I,J) = 0.0
        ENDDO
      ENDDO

cipk jun05
      inovel=0
      if(iteqv(maxn) .eq. 2) inovel=1
      if(iteqv(maxn) .eq. 8) inovel=2
      if(iteqv(maxn) .eq. 9) inovel=3

cipk oct98 update to f90
      IMMT=IMAT(NN)

cipk nov99 revise to allow for collapsing 3-d to 2-d
      if(immt .gt. 1000) immt=immt-1000

CIPK JUN05
c   
c     Test for and determine whether conrol structure now operates as an
c     ordinary element.  If one node is above transition then treat as
c     a  normal lement
      if(ntx .eq. 1) then
        if(imat(nn) .gt. 900) then
          if(inovel .gt. 0) return
          if(isubmel(nn) .eq. 0) go to 2000
        endif
      endif

      NR = MOD(IMMT,100)
cipkjun05
      if(immt .gt. 900) nr=immt
      FFACT=0.
cipk nov98 adjust for top friction
      IF(ORT(NR,5) .GT. 1.  .or.  ort(nr,13) .gt. 1.) then
        FFACT = GRAV/(CHEZ(NN)+ort(nr,13))**2
      endif

CIPK MAR0 setup switch that says salinity is active
      IF(ITEQV(MAXN) .EQ. 2  .OR.  ITEQV(MAXN) .EQ. 8
     +                       .OR.  ITEQV(MAXN) .EQ. 9) THEN
        ISLP=1
      ELSE
        ISLP=0
      ENDIF
c
cipk sep96 new logic for control of horiz eddy and diffusion

cipk mar03 add logic for IDIFSW choose based on active component
C
      IF(ISLP .EQ. 0  .OR. IDIFSW .EQ. 0) THEN
        IF(IEDSW .EQ. 0  .OR.  IEDSW .EQ. 1) THEN
          CX=COS(THNN)
          SA=SIN(THNN)
        ELSEIF((IEDSW .EQ. 3  .AND. ITEQV(MAXN) .EQ. 2)
     +    .OR.  IEDSW .EQ. 4  .or.  iedsw .eq. 2)  THEN
cipk jan97 add =2 option     +  .OR.  IEDSW .EQ. 4)  THEN
cipk aug96 redefine princ. direction when modeling salinity only
c       Find a direction based on velocities at the corner nodes
          DIRX=0.
          DIRY=0.
          DO K=1,NCN,2
c     Normalize velocity vector length
            VNORM=SQRT(VEL(1,NCON(K))**2 + VEL(2,NCON(K))**2)
            IF(VNORM .GT. 0.) THEN
              DIRX=DIRX+VEL(1,NCON(K))/VNORM
              DIRY=DIRY+VEL(2,NCON(K))/VNORM
            ENDIF
          ENDDO
          IF(DIRX .NE. 0.  .AND.  DIRY .NE. 0.) THEN
            CX=DIRX/SQRT(DIRX**2+DIRY**2)
            SA=DIRY/SQRT(DIRX**2+DIRY**2)
cipk jan97          IF(IEDSW .EQ. 4)
            IF(IEDSW .EQ. 2  .OR.  IEDSW .EQ. 4)
     +        THNN=ATAN2(SA,CX)
          ELSE
            CX=COS(THNN)
            SA=SIN(THNN)
          ENDIF
        ELSE
          CX=COS(THNN)
          SA=SIN(THNN)
        ENDIF
      ELSE
        IF(IDIFSW .EQ. 9  .OR. IDIFSW .EQ. 1) THEN
          CX=COS(THNN)
          SA=SIN(THNN)
        ELSEIF(IDIFSW .GT. 1  .AND. IDIFSW .LT. 6) THEN

c       Find a direction based on velocities at the corner nodes
          DIRX=0.
          DIRY=0.
          DO K=1,NCN,2
c     Normalize velocity vector length
            VNORM=SQRT(VEL(1,NCON(K))**2 + VEL(2,NCON(K))**2)
            IF(VNORM .GT. 0.) THEN
              DIRX=DIRX+VEL(1,NCON(K))/VNORM
              DIRY=DIRY+VEL(2,NCON(K))/VNORM
            ENDIF
          ENDDO
          IF(DIRX .NE. 0.  .AND.  DIRY .NE. 0.) THEN
            CX=DIRX/SQRT(DIRX**2+DIRY**2)
            SA=DIRY/SQRT(DIRX**2+DIRY**2)
            IF(IDIFSW .EQ. 4  .OR. IDIFSW .EQ. 5)
     +      THNN=ATAN2(SA,CX)
          ELSE
            CX=COS(THNN)
            SA=SIN(THNN)
          ENDIF
        ENDIF
      ENDIF

cipk mar03 end changes
cipk sep96 move up this computation
C-
C-.....COMPUTE LOCAL CORDS.....
C-
      MR=NCON(1)
      DO K = 1, NCN
        N=NCON(K)
        DX=CORD(N,1)-CORD(MR,1)
        DY=CORD(N,2)-CORD(MR,2)
        XL(K)=DX*CX+DY*SA
        YL(K)=-DX*SA+DY*CX
        !EFa may07, necessary for subroutine turbulence
        dside=SQRT(dx*dx+dy*dy)
        IF(k.eq.3) dsid(1)=dside
        IF(k.eq.5) dsid(3)=dside
        IF(k.eq.7) dsid(5)=dside
        !-
      ENDDO
      !EFa may07, necessary for subroutine turbulence
      do k=5,ncn,2
        n=nop(nn,k)
        mr=nop(nn,k-2)
        dx=cord(n,1)-cord(mr,1)
        dy=cord(n,2)-cord(mr,2)
        dside=SQRT(dx*dx+dy*dy)
        IF(k.eq.5) dsid(2)=dside
        IF(k.eq.7) dsid(4)=dside
      end do
      !area computation for triangular elements
      semp=(dsid(1)+dsid(2)+dsid(3))/2
      artr1=SQRT(semp*(semp-dsid(1))*(semp-dsid(2))*(semp-dsid(3)))
      gsc1=SQRT(4*artr1/SQRT(3.))
      gscal=gsc1
      !area computation for the complementary triangle within a
      !quadrilateral element (for quadrilateral elements only)
      if (ncn.gt.6) then
        semp=(dsid(3)+dsid(4)+dsid(5))/2
        artr2=SQRT(semp*(semp-dsid(3))*(semp-dsid(4))*(semp-dsid(5)))
        gsc2=SQRT(4*artr2/SQRT(3.))
        gscal=(gsc1+gsc2)/2.
      end if
      !-
CIPK JAN03 add momentum
      EINA=EINX(NN)*CX+EINY(NN)*SA
      EINB=-EINX(NN)*SA+EINY(NN)*SA
      NCNX=NCN/2
      IF(NTX .EQ. 0) GO TO 72
cipk nov97
c
c     Initialize AME and DAME
c
      IF (IDNOPT.LT.0) THEN
         DO M = 1, NCNX
           MC = 2 * M - 1
           N = NOP(NN,MC)
           HS = VEL(3,N)
           ISWT = 0
           CALL AMF(DUM1,HS,AKP(N),ADT(N),ADB(N),AME(M),DAME(M),ISWT)
cipk apr05
           efpornn(m)=efpor
         END DO
      ENDIF
cipk nov97 end update
C
cipk sep96 end move
CIPK AUG96      CX=COS(TH(NN))
CIPK AUG96      SA=SIN(TH(NN))
CIPK SEP96 END UPDATES
C
CIPK SEP96      ROAVG=1.935
CIPK SEP96      IF (GRAV .LT. 32.)  ROAVG = 516. * 1.935
C
CIPK SEP96 ADD TESTS FOR IEDSW
      IF(IEDSW .NE. 2  .AND.  IEDSW .NE. 4) THEN
        EPSX = EEXXYY(1,NN)/ROAVG
        EPSXZ = EEXXYY(2,NN)/ROAVG
        EPSZX = EEXXYY(3,NN)/ROAVG
        EPSZ = EEXXYY(4,NN)/ROAVG
CIPK JAN97      ELSEIF(IEDSW .EQ. 4) THEN
      ELSEIF(IEDSW .EQ. 2  .OR.  IEDSW .EQ. 4) THEN
c get element lengths in the projected direction
        PROJL(1)=0.
        DO N=3,NCN,2
          PROJL(N)=XL(N)*CX + YL(N)*SA
        ENDDO
        XLMAX=0.
        XLMIN=0.
        DO N=3,NCN,2
          IF(PROJL(N) .GT. XLMAX) XLMAX=PROJL(N)
          IF(PROJL(N) .LT. XLMIN) XLMIN=PROJL(N)
        ENDDO
        EPSX=ABS(ORT(NR,1)*(XLMAX-XLMIN))*CVFCT/ROAVG
        EPSXZ=EPSX*ABS(ORT(NR,2))
        EPSZX=EPSXZ
        EPSZ=EPSXZ
      ENDIF
cipk sep96 rewrite logic for DIFX and DIFY
CIPK MAR03  add logic for IDIFSW

      IF(ISLP .EQ. 0  .OR.  IDIFSW .EQ. 0) THEN
        IF(IEDSW .LT. 2) THEN
          DIFX = EEXXYY(5,NN)
          DIFY = EEXXYY(6,NN)
        ELSEIF(IEDSW .EQ. 3) THEN
          DIFX = EEXXYY(5,NN)
          DIFY = DIFX*ABS(ORT(NR,9))
CIPK JAN97      ELSEIF(IEDSW .EQ. 4) THEN
        ELSEIF(IEDSW .EQ. 2  .OR.  IEDSW .EQ. 4) THEN
          DIFX = ABS(ORT(NR,8)*(XLMAX-XLMIN))/CVF2
          DIFY = DIFX*ABS(ORT(NR,9))
        ENDIF
	ELSE
	  IF(IDIFSW .EQ. 9  .OR.  IDIFSW .LT. 3) THEN
          DIFX = EEXXYY(5,NN)
          DIFY = EEXXYY(6,NN)
        ELSEIF(IDIFSW .EQ. 3) THEN
          DIFX = EEXXYY(5,NN)
          DIFY = DIFX*ABS(ORT(NR,9))
        ELSEIF(IDIFSW .EQ. 4) THEN
          DIFX = ABS(ORT(NR,8)*(XLMAX-XLMIN))/CVF2
          DIFY = DIFX*ABS(ORT(NR,9))
        ENDIF
      ENDIF
cipk mar03 end changes
   72 CONTINUE
      NGP=7
CIPK JUN05      NEF=NCN*NDF
C-
C- INITIALIZE MATRICES AND VARIABLES
C-
CIPK SEP02  add logic to make ice cover functions linear
      DO i=1,ncn
        IF(ICESW .GT. 0) THEN
          IF(MOD(I,2) .EQ. 0  .AND.  I .LT. NCN) THEN
            THKI(I)=(ICETHK(NOP(NN,I-1))+ICETHK(NOP(NN,I+1)))/2.
            QWLI(I)=(QICE(NOP(NN,I-1))+QICE(NOP(NN,I+1)))/2.
          ELSEIF(MOD(I,2) .EQ. 0  .AND.  I .EQ. NCN) THEN
            THKI(I)=(ICETHK(NOP(NN,I-1))+ICETHK(NOP(NN,1)))/2.
            QWLI(I)=(QICE(NOP(NN,I-1))+QICE(NOP(NN,1)))/2.
          ELSE
            THKI(I)=ICETHK(NOP(NN,I))
            QWLI(I)=QICE(NOP(NN,I))
          ENDIF
        ELSE
          THKI(I)=0.
          QWLI(I)=0.
        ENDIF
	ENDDO
CIPK NOV97 MODERNIZE LOOP
cipk jun05      DO  I=1,NEF
cipk jun05        F(I) = 0.0
cipk jun05        DO  J=1,NEF
cipk jun05          ESTIFM(I,J) = 0.0
cipk jun05        ENDDO
cipk jun05      ENDDO
C-
C...... Check for element dropout
C-
      IF(NTX .EQ. 0) GO TO 79
      DO 78 I=1,NCN
        MM=NCON(I)
        DO 77 K=1,NDF
          IF(NBC(MM,K) .GT. 0) GO TO 79
   77   CONTINUE
   78 CONTINUE
      RETURN
   79 CONTINUE
C-
C-.....COPY PROPER WEIGHTING FUNCTIONS.....
C-
cipk jan98
      AREA(NN)=0.
CIPK APR01
	TVOL(NN)=0.
cipk nov99 revise for collapsing from 3-d
CIPK XXXX      IF(IMMT .LT. 100 ) THEN
cIPKMAR05  MAKE THEM ALL 16
CIPKMAR05      IF(IMMT .LT. 1000 ) THEN
cIPKMAR05        IF( NCN .LT. 8 ) THEN
cIPKMAR05          NGP = 7
cIPKMAR05          DO 80 M = 1, NGP
cIPKMAR05            WAITX(M) = WAITT(M)
cIPKMAR05   80     CONTINUE
cIPKMAR05        ELSE
cIPKMAR05          NGP = 9
cIPKMAR05          DO 90 M = 1, NGP
cIPKMAR05            WAITX(M) = WAITR(M)
cIPKMAR05   90     CONTINUE
cIPKMAR05        ENDIF
cIPKMAR05      ELSE
        NGP = 16
        IF( NCN .LT. 8 ) THEN
          DO 92 M = 1, NGP
            WAITX(M)=WAITTH(M)
   92     CONTINUE
        ELSE
          DO 94 M = 1, NGP
            WAITX(M) = WAITRH(M)
   94     CONTINUE
        ENDIF
C      ENDIF
C-
C-.....COPY SHAPE FUNCTIONS
C-
      CALL SB2(NCN,NGP)

CIPK MAY04 RESET ELEMENT INFLOW

      IF(INOFLOW(NN) .EQ. 0) THEN
        SIDFQ=SIDF(NN)
      ELSE
        SIDFQ=0.
	ENDIF
	SIDFQQ=SIDF(NN)

C-
C-.....COMPUTE ELEMENT EQUATIONS.....
C-
      DO 500 I = 1, NGP
C-
C-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
C-
      J11 = 0.0
      J12 = 0.0
      J21 = 0.0
      J22 = 0.0
CIPK NOV97 MODERNIZE LOOP
      DO  K = 2, NCN
        J11 = J11 + DA(K,I) * XL(K)
        J12 = J12 + DA(K,I) * YL(K)
        J21 = J21 + DB(K,I) * XL(K)
        J22 = J22 + DB(K,I) * YL(K)
      ENDDO
CIPK NOV97  130 CONTINUE
      DETJ = J11 * J22 - J12 * J21
CIPK NOV97 MODERNIZE LOOP
      DO J = 1, NCN
        XN(J) = XNX(J,I)
        DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / DETJ
        DNY(J) = ( J11 * DB(J,I) - J21 * DA(J,I) ) / DETJ
        XO(J)=XN(J)
        DOX(J)=DNX(J)
        DOY(J)=DNY(J)
      ENDDO
CIPK NOV97  135 CONTINUE
      AMW = WAITX(I) * DETJ
      AREA(NN)=AREA(NN)+AMW
      IF(AMW.LE. 0.) WRITE(LOUT,9802) NN,I
 9802 FORMAT(' AMW IS ZERO OR NEGATIVE FOR ELEMENT',I5,'GAUSS NO',I5)
cipk nov97      IF(NTX .EQ. 0) GO TO 500
C-
C-     REPEAT FOR LINEAR FUNCTION
C-
      JJ=0
      DO 145 J=1,NCN,2
      JJ=JJ+1
      XM(JJ)=XMX(JJ,I)
      DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/DETJ
      DMY(JJ)=(J11*CB(JJ,I)-J21*CA(JJ,I))/DETJ
  145 CONTINUE
C-
cipk nov97
      IF(NTX .EQ. 0) GO TO 273
      DO 155 J=2,NCN,2
        MR=NCON(J)
        IF(NSTRT(MR,1) .NE. 0) THEN
          XO(J-1)=XO(J-1)+XO(J)/2.
          DOX(J-1)=DOX(J-1)+DOX(J)/2.
          DOY(J-1)=DOY(J-1)+DOY(J)/2.
          IF(J .LT. NCN) THEN
            JP=J+1
          ELSE
            JP=1
          ENDIF
          XO(JP)=XO(JP)+XO(J)/2.
          DOX(JP)=DOX(JP)+DOX(J)/2.
          DOY(JP)=DOY(JP)+DOY(J)/2.
        ENDIF
  155 CONTINUE
C-
C...... Set momentum correction factors
C-
      AKX=0.
      AKY=0.
C-
C...... Set bottom friction coefficient
C-
      UBF=0.
      VBF=0.
C-
C.....COMPUTE R, S, H AND THEIR DERIVATIVES.....
C-
      R = 0.0
      S = 0.0
      DRDX = 0.0
      DRDZ = 0.0
      DSDX = 0.0
      DSDZ = 0.0
      BETA1 = 0.0
      BETA2 = 0.0
      SALT=0.0
      DSALDT= 0.0
      DSALDX=0.0
      DSALDY=0.0
CIPK SEP02 ADD WAVE DATA INTERPOLATION
      TP=0.0
	HSV=0.0
      WDIR=0.0
C-
C......ESTABLISH VELOCITIES
C-
      DO 250 M=1,NCN
      MR=NCON(M)
      VXX(M)=VEL(1,MR)/UDST(MR)
      VY(M)=VEL(2,MR)/VDST(MR)
      ST(M)=VEL(ICK,MR)/SDST(MR)
      VDX(M)=VDOT(1,MR)/UDST(MR)
      VDY(M)=VDOT(2,MR)/VDST(MR)
      SDT(M)=VDOT(ICK,MR)/SDST(MR)
      IF(ITEQV(MAXN) .EQ. 5  .AND.  NDEP(MR) .GT. 1) THEN
        NBOT=NREF(MR)+NDEP(MR)-1
        UBFC(M)=UDST(NBOT)
        VBFC(M)=VDST(NBOT)
      ELSE
        UBFC(M)=1.0
        VBFC(M)=1.0
      ENDIF
  250 CONTINUE
      DO 270 M=1,NCN
      MR=NCON(M)
CIPK SEP02 INTERPOLATE WAVE DATA
      TP=TP+XN(M)*PEAKPRD(MR)
	HSV=HSV+XN(M)*WAVEHT(MR)
	WDIR=WDIR+XN(M)*WAVEDR(MR)

      AKX=AKX+(UUDST(MR)*CX+VVDST(MR)*SA)*XN(M)
      AKY=AKY+(-UUDST(MR)*SA+VVDST(MR)*CX)*XN(M)
      UBF=UBF+(UBFC(M)*VXX(M)*CX+VBFC(M)*VY(M)*SA)*XN(M)
      VBF=VBF+(-UBFC(M)*VXX(M)*SA+VBFC(M)*VY(M)*CX)*XN(M)
      R=R+XN(M)*(VXX(M)*CX+VY(M)*SA)
      S=S+XN(M)*(-VXX(M)*SA+VY(M)*CX)
      DRDX=DRDX+DNX(M)*(VXX(M)*CX+VY(M)*SA)
      DRDZ=DRDZ+DNY(M)*(VXX(M)*CX+VY(M)*SA)
      DSDX=DSDX+DNX(M)*(-VXX(M)*SA+VY(M)*CX)
      DSDZ=DSDZ+DNY(M)*(-VXX(M)*SA+VY(M)*CX)
      IF(NSTRT(MR,1) .EQ. 0) THEN
        SALT=SALT+XO(M)*ST(M)
        DSALDT=DSALDT+XO(M)*SDT(M)
        DSALDX=DSALDX+DOX(M)*ST(M)
        DSALDY=DSALDY+DOY(M)*ST(M)
      ENDIF
      IF(ICYC.LT.1) GO TO 270
      BETA1=BETA1+XN(M)*(VDX(M)*CX+VDY(M)*SA)
      BETA2=BETA2+XN(M)*(-VDX(M)*SA+VDY(M)*CX)
  270 CONTINUE
cipk nov97
  273 continue
      H = 0.0
      DHDX = 0.0
      DHDZ = 0.0
      AZER=0.0
cipk mar01 use abed
      abed=0
      DAODX = 0.0
      DAODZ = 0.0
      SIGMAX = 0.0
      SIGMAZ = 0.0
      BETA3 = 0.0
      RHO=0.0
      DRODX=0.0
      DRODZ=0.0
CIPK NOV97 MODERNIZE LOOP AND ADD LOGIC FOR MARSH FRICTION
      BRANG=0.0
      AKAPMG=0.0
cipk sep02 add ice parameters
      GSICE=0.
      GSQLW=0.

CIPK JUN02
      GAIN=0.
      WSELL=0.
CIPK SEP02
      EXTL=0.
      DO M=1,NCNX
        MC = 2*M - 1
        MR=NCON(MC)
        H = H + XM(M)*VEL(3,MR)
cipk jun02
	  GAIN=GAIN+XM(M)*GAN(MR)
	  WSELL=WSELL+WSLL(MR)*XM(M)
CIPK SEP02
CIPK DEC05
        IF(ICK .EQ. 6) THEN
          EXTL=EXTL+XM(M)*EXTLD(MR)
        ENDIF
cipk jan00 correct location for azer
C        AZER=AZER+XM(M)*AO(MR)
        BETA3=BETA3+XM(M)*VDOT(3,MR)
        DHDX = DHDX + DMX(M)*VEL(3,MR)
        DHDZ = DHDZ + DMY(M)*VEL(3,MR)
CIPK NOV97
        IF (IDNOPT.GE.0) THEN

CIPK JAN00 ADD AZER HERE
          AZER=AZER+XM(M)*AO(MR)
cipk mar01 add abed
          Abed=Abed+XM(M)*AO(MR)

          DAODX = DAODX + DMX(M)*AO(MR)
          DAODZ = DAODZ + DMY(M)*AO(MR)
        ELSE

CIPK JAN00 ADD AZER HERE
          AZER  = AZER  + XM(M)*(AME(M)+ADO(MR))
cipk mar01 add abed
          Abed=Abed+XM(M)*AO(MR)

          DAODX = DAODX + DMX(M)*(AME(M)+ADO(MR))
          DAODZ = DAODZ + DMY(M)*(AME(M)+ADO(MR))
        ENDIF
        BRANG=BRANG+XM(M)*ADB(MR)
        AKAPMG=AKAPMG+XM(M)*AKP(MR)
        RHO=RHO+XM(M)*DEN(MR)
        DRODX=DRODX+DMX(M)*DEN(MR)
        DRODZ=DRODZ+DMY(M)*DEN(MR)
CIPK MAY02  ADD STRESS TERM

cipk jun03 add STRESS component

        SIGMAX=SIGMAX+XM(M)*((SIGMA(MR,1)+stress(mr,1))*CX
     +                        +(SIGMA(MR,2)+stress(mr,2))*SA)
        SIGMAZ=SIGMAZ+XM(M)*(-(SIGMA(MR,1)+stress(mr,1))*SA
     +                        +(SIGMA(MR,2)+stress(mr,2))*CX)

CIPK SEP02 GET GAUSS POINT ICE VALUES
        GSICE=GSICE+XM(M)*THKI(MC)
        GSQLW=GSQLW+XM(M)*QWLI(MC)
      ENDDO

cipk this is temporary addition for sand transport test
c      if(h .gt. 0.5) h=0.5
c      r=r/5.
c      s=s/5.

CIPK NOV97  275 CONTINUE

CIPK DEC05
      EXTL=EXTL+EXTLDEL(NN)

CIPK AUG03 ADD TEST TO REMOVE STRESSES WHEN DRY
      IF(H+AZER .LT. ABED) THEN
	  SIGMAX=0.
	  SIGMAZ=0.
      ENDIF

cipk jun05      RHO=1.0

      ROAVG=RHO

CIPK MAR05      XHT=ELEV-AZER
      AMU=AMW
      AMT=AMW*RHO
CIPK  MAR05     AMS=AMU*RHO
      AMS=AMW*RHO
cipk nov97
      TVOL(NN)=TVOL(NN)+AMW
      if(ntx .eq. 0) go to 500
C-
C...... Correct momentum factors
C-
      AKX=AKX*2.-1.0
      AKY=AKY*2.-1.0
CYYY                                   momentum factors disabled
      akx = 1.
      aky = 1.
C

      IF(ABS(R) .GT. THRESH) THEN
        UBF=UBF/R
      ELSE
        UBF=1.0
      ENDIF
      IF(ABS(S) .GT. THRESH) THEN
        VBF=VBF/S
      ELSE
        VBF=1.0
      ENDIF
      IF(ICK .EQ. 4) THEN
        DRDS=DRODS(SALT,IGF)
CIPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ELSEIF(ICK .EQ. 5) THEN
        DRDS=DRODTM(SALT,IGF)
CIPK AUG95 GET RATES
        DELTT=DELT
        HS=H
        CALL MKTEMP(SALT,HS,0.,SRCSNK,GRATE,DELTT,NR,NETYP(NN))
      ELSE
C
C     Set up sand transport variables
C
        IF(LSAND .GT. 0) THEN
          ALP1=0.0
          ALP2=0.0
          DO M=1,NCN
            MR=NOP(NN,M)
            ALP1=ALP1+ALPHA1(MR)*XN(M)
            ALP2=ALP2+ALPHA2(MR)*XN(M)
          ENDDO
        ENDIF
C
C     Set up cohesive transport variables
C
        IF(LSS .GT. 0) THEN
          ALP1=0.0
          ALP2=0.0
CIPK SEP05 MAKE INTERPOLATION LINEAR
CIPK SEP05          DO M=1,NCN
CIPK SEP05            MR=NOP(NN,M)
CIPK SEP05            ALP1=ALP1+DEPRAT(MR)*XN(M)
CIPK SEP05            ALP2=ALP2+(EDOT(MR)+SERAT(MR))*XN(M)

CIPK AUG06 ADD AVERAGE TEST
          IF(IAVEL .EQ. 0) THEN
            DO M=1,NCNX
              MC = 2*M - 1
              MR=NCON(MC)
              ALP1 = ALP1 + XM(M)*DEPRAT(MR)
             ALP2 = ALP2 +(EDOT(MR)+SERAT(MR))*XM(M)            
            END DO
          ELSE
            alp1=depratm
            alp2=edotm+seratm
          ENDIF
        ENDIF

        GRATE=0.0
	  srcsnk=0.
        HS=H
        IF(LSAND .GT. 0) THEN
	    CALL MKSAND(SALT,HS,VSET,SRCSNK,GRATE,NETYP(NN))
	  ENDIF

        IF(LSS .GT. 0) THEN
	    CALL MKSSED(SALT,HS,VSET,SRCSNK,GRATE,NETYP(NN))
	  ENDIF


        DRDS=DRODSD(SALT,IGF)
      ENDIF

C      IF(NN .EQ. 1407) THEN
C	  AZZZ=0.
C	ENDIF

CIPK AUG02 TEST FOR SHALLOW OR NEGATIVE DEPTH TO SET STRESS TO ZERO.
      IF(WSELL-ABED .LT. ZSTDEP) THEN
	  SIGMAX=0.
	  SIGMAZ=0.
	ENDIF
      IF(WSELL .LT. ABED) THEN
CIPK AUG06
        IF(LSS .EQ. 0) THEN
          grate=0.
          srcsnk=0.
        ENDIF
cipk aug02  make wind stress zero over dry areas
  	  sigmax=0.
	  sigmaz=0.
	ENDIF

cipk may03  reduce grate and srcsnk to zero when IEDROP active
c
      do ned=1,9

        IF(IMMT .EQ. iedrop(ned)) THEN
          grate=0.
    	    srcsnk=0.
	  ENDIF
	enddo

      DO M=1,NCN
        MR=NCON(M)
	  if(WSLL(mr) -ao(mr) .lt. zstdep) then
	    sigmax=0.
	    sigmaz=0.
CIPK AUG06
          IF(LSS .EQ. 0) THEN
            grate=0.
            srcsnk=0.
          ENDIF
cipk may03  reduce nodal rates to zero
	    alpha1(mr)=0.
	    alpha2(mr)=0.
	  endif

        do ned=1,9
cipk may03  reduce nodal rates to zero when IEDROP active

          IF(IMMT .EQ. iedrop(ned)) THEN
  	      alpha1(mr)=0.
	      alpha2(mr)=0.
	    ENDIF
	  enddo
	enddo

      DRODX=DRDS*DSALDX
      DRODZ=DRDS*DSALDY

CIPK SEP02 ADD AN ICE THICKNESS TEST FOR WIND STRESS

      IF(GSICE .LE. 0.001) THEN
        SIGMAX = SIGMAX/RHO
        SIGMAZ = SIGMAZ/RHO
      ELSE
	  SIGMAX=0.0
	  SIGMAZ=0.
      ENDIF
      GHC = GRAV*H
      VECQ = SQRT((R*UBF)**2+(S*VBF)**2)
      IF(H .LE. 0.0) H=0.001

!NiS,apr06: adding possibility of FrictionFactor calculation with
!           COLEBROOK-WHITE to apply DARCY-WEISBACH equation: Therefore,
!           the if-clause has also to be changed because surface friction
!           is deactivated!
!-
cipk nov98 adjust for surface friction
  !NiS,apr06: changing test:
  !    IF(ORT(NR,5) .GT. 0.  .OR.  ORT(NR,13) .GT. 0.) THEN
      IF(ORT(NR,5) .GT. 0.  .OR.  (ORT(NR,13) .GT. 0. .and.
     +   ORT(NR,5) /= -1.0)) THEN
  !-
CIPK SEP02
	  EFMAN=0.
        IF(ORT(NR,5) .LT. 1.0  .AND.  ORT(NR,13) .LT. 1.0) then
CIPK MAR01  ADD POTENTIAL FOR VARIABLE MANNING N
          IF(MANMIN(NR) .GT. 0.) THEN
	      IF(H+AZER .LT. ELMMIN(NR) ) THEN 
              FFACT=(MANMIN(NR))**2*FCOEF/(H**0.333)
CIPK SEP02
	        EFMAN=MANMIN(NR)
          ELSEIF(H+AZER .GT. ELMMAX(NR) ) THEN 
              FFACT=(MANMAX(NR))**2*FCOEF/(H**0.333)
CIPK SEP02
	        EFMAN=MANMAX(NR)
          ELSE
	        FSCL=(H+AZER-ELMMIN(NR))/(ELMMAX(NR)-ELMMIN(NR))
              FFACT=(MANMIN(NR)+FSCL*(MANMAX(NR)-MANMIN(NR)))**2
     +     	       *FCOEF/(H**0.333)
CIPK SEP02
	        EFMAN=MANMIN(NR)+FSCL*(MANMAX(NR)-MANMIN(NR))
	  ENDIF
CIPK SEP04  ADD MAH AND MAT OPTION
        ELSEIF(HMAN(NR,2) .GT. 0  .OR. HMAN(NR,3) .GT. 0.) THEN
	      TEMAN=0.
            IF(HMAN(NR,2) .GT. 0) THEN 
	        TEMAN=HMAN(NR,3)*EXP(-H/HMAN(NR,2))
	      ENDIF
	      TEMAN=TEMAN+HMAN(NR,1)/H**HMAN(NR,4)
            FFACT=TEMAN**2*FCOEF/(H**0.333)
          ELSEIF(MANTAB(NR,1,2) .GT. 0.) THEN
	      DO K=1,4
	        IF(H .LT. MANTAB(NR,K,1)) THEN
	          IF(K .EQ. 1) THEN
	            TEMAN=MANTAB(NR,1,2)
	          ELSE
	            FACT=(H-MANTAB(NR,K-1,1))/
     +                  (MANTAB(NR,K,1)-MANTAB(NR,K-1,1))
	            TEMAN=MANTAB(NR,K-1,2)
     +            +FACT*(MANTAB(NR,K,2)-MANTAB(NR,K-1,2))
	          ENDIF
	          GO TO 280
	        ENDIF
	      ENDDO
	      TEMAN=MANTAB(NR,4,2)
  280       CONTINUE
            FFACT=TEMAN**2*FCOEF/(H**0.333)

cipk mar05
            DFFDH=-FFACT/(H*3.0)
          ELSE
!**************************************************************
!
!   DJW 31/10/05 : Friction Factor Modification to adjust for Roughness Calcs
!
!**************************************************************
!
!            FFACT=(ORT(NR,5)+ORT(NR,13))**2*FCOEF/(H**0.333)
	      FFACT=(ZMANN(NN)+ORT(NR,13))**2*FCOEF/(H**0.333)
!
!**************************************************************
!
!        End DJW Changes
!
!**************************************************************

cipk mar05
            DFFDH=-FFACT/(H*3.0)
	    endif
cipk mar05
        ELSE
          DFFDH=0.
        ENDIF

      !NiS,apr06: adding RESISTANCE LAW form COLEBROOK-WHITE for DARCY-WEISBACH-equation:
      ELSEIF (ORT(NR,5) < 0.0) THEN

        !calculate lambda
        !nis,aug07: Introducing correction factor for roughness parameters, if Darcy-Weisbach is used
        call darcy(lambdaTot(nn), vecq, h,
     +             cniku(nn)     * correctionKS(nn),
     +             abst(nn)      * correctionAxAy(nn),
     +             durchbaum(nn) * correctionDp(nn),
     +             nn, morph, gl_bedform, mel, c_wr(nn), 2,
                   !store values for output
     +             lambdaKS(nn),
     +             lambdaP(nn),
     +             lambdaDunes(nn))


        !calculation of friction factor for roughness term in differential equation
        FFACT = lambdaTot(nn)/8.0

        !NiS,apr06: As parallel to the other parts from above, without knowledge about meaning, might be derivative, not clear
        DFFDH = 0.
      !-

      ENDIF


CIPK MAR03 APPLY ELDER EQUATION IF SELECTED AND ADD MINIMUM TEST

      IF(ISLP .EQ. 1  .AND. IDIFSW .EQ. 5) THEN
        IF(NN .EQ. 225) THEN
	    AAAA=0
	  ENDIF
        SHEARVEL=VECQ*SQRT(FFACT)
	  DIFX=SHEARVEL*H*ABS(ORT(NR,8))
	  DIFY=DIFX*ABS(ORT(NR,9))
	ENDIF

      if(difx .lt. ort(nr,14)) then
	  if(difx .gt. 0.) then
           dify=ort(nr,14)*dify/difx
        else
	     dify=ort(nr,14)
	  endif
	  difx=ort(nr,14)
      endif

	  peclet=vecq*abs((xl(3)+xl(5)))/2/difx
!	  
!        IF(NN .EQ. 15  .AND.  I .EQ. 5) THEN
!c        if(i .eq. 1  .and.  idebug .eq. 2) then
!	    WRite(129,'(3i5,8g15.6)') nn,i,MAXN,shearvel,h,difx,dify,thnn
!     +            ,peclet,h*difx*dsaldx,h*dify*dsaldy
!	  ENDIF

CIPK SEP02  ADD LOGIC FOR WAVE SENSITIVE FRICTION

      IF (TP .GT. 001.  .AND.  HSV .GT. 0.001) THEN

        Y=4.02*H/TP**2
        POL=1.+Y*(.666+Y*(.355+Y*(.161+Y*(.0632+Y*(.0218+.00654*Y)))))
        WAVENR=SQRT(Y**2+Y/POL)/H
        RLS=2.*PI/WAVENR
        ARG=WAVENR*H
        IF (ARG.GT.50.) THEN
          UBW=0.
        ELSE
          ABW=HSV/(2.*SINH(ARG))
          UBW  = 2.*PI/TP*ABW
        ENDIF
	  CORWDIR=WDIR-THNN
	  IF(S .EQ. 0.  .AND.  R .EQ. 0.) THEN
	    CURRDIR=0.
	  ELSE
	    CURRDIR=ATAN2(S,R)
        ENDIF
	  IF(ABS(CURRDIR-CORWDIR) .LT. PI/4.) THEN
	    GAM=1.1
	  ELSEIF(ABS(CURRDIR-CORWDIR) .GT. 1.75*PI) THEN
	    IF(ABS(ABS(CURRDIR-CORWDIR)-2.*PI) .LT. PI/4.) THEN
	      GAM=1.1
	    ELSE
	      GAM=0.75
	    ENDIF
	  ELSE
	    GAM=0.75
	  ENDIF
	  if(vecq .gt. 0.00001) then
	    IF(GAM*UBW/VECQ .GT. 2.30) THEN
	      FENH=10.
	    ELSE
	      FENH=EXP(GAM*UBW/VECQ)
	    ENDIF
	  else
	    fenh=10.
	  endif
	  IF(FENH .GT. 10.) FENH=10.
        IF(EFMAN .GT. 0.) THEN
	    EFCHEZ=H**0.166667/EFMAN
	  ELSE
	    EFCHEZ=CHEZ(NN)
	  ENDIF
	  FCT=12.*H/10**(EFCHEZ/18.)
        FACTO=FENH*FCT
	  EFCHEZA=18.*LOG10(12.*H/FACTO)
        FRICCR=EFCHEZ/EFCHEZA
	  FFACT=FFACT*FRICCR
	ENDIF
CIPK SEP02 END ADDITION

CIPK NOV97
cipk MAR05
C      IF(H .LT. AKAPMG*BRANG) THEN
CC	    frsc=ort(ntyp,7)**2-1.
C        FRSC=ort(nr,12)**2-1.
C        FMULT=FRSC*(AKAPMG*BRANG-H)/(AKAPMG*BRANG)+1.0
C	    dfmdh=-frsc/(akapmg*brang)
C      ELSE
C        FMULT=1.0
C       dfmdh=0.0
C      ENDIF
C        dffact=ffact*dfmdh+fmult*dffdh
C      FFACT=FFACT*FMULT
cipk nov97 end changes
        if(h .lt. akapmg*brang) then
            frsc=ort(nr,12)**2-1.
            xcd=(akapmg*brang-h)/(brang*AKAPMG)*pi
            fmult=frsc/2.*(1.-cos(xcd))+1.0
            dfmdh=-frsc/2.*sin(xcd)*pi/(akapmg*brang)
cipk	      endif
        else
          fmult=1.0
          dfmdh=0.0
        endif
        dffact=ffact*dfmdh+fmult*dffdh
c        ffact=ffact/sqrt(efporg)
        ffact=ffact*fmult
CIPK MAR01 ADD DRAG AND REORGANIZE      TFRIC = 0.0
      IF( VECQ .GT. 1.0E-6 ) THEN
	  TFRIC = FFACT / VECQ
	  TDRAGX = GRAV*DRAGX(NR)/VECQ
	  TDRAGY = GRAV*DRAGY(NR)/VECQ
      ELSE
	  TFRIC = 0.0
	  TDRAGX = 0.0
	  TDRAGY = 0.0
	ENDIF

cipk jun05
      IF(NR .GT. 90  .and.  nr .lt. 100) GO TO 291
CIPK AUG06
      IF(ICNSV .EQ. 1) THEN
          QIN=BETA3/H+(DRDX+DSDZ)+(R*DHDX+S*DHDZ)/H
      ENDIF

      !EFa may07, new subroutine for turbulence modell
      if (iedsw.ge.10) then
        call turbulence(nn,iedsw,tbmin,eexxyy(1,nn),eexxyy(2,nn),
     +       eexxyy(3,nn),eexxyy(4,nn),epsx,epsxz,epszx,epsz,roavg,
     +       p_bottom,tbfact,ffact,vecq,h,drdx,drdz,dsdx,dsdz,gscal)
      end if
      !-

CIPK MAR01 CLEANUP LOGIC
	IF(ICYC .LE. 0) THEN
        FRN = 0.0
        FSN = 0.0
CIPK MAR01      IF( ICYC .LE. 0 ) GO TO 279
	ELSE
        FRN=H*BETA1
        FSN=H*BETA2
	ENDIF
CIPK MAR01 SET SIDF=0 FOR DRY CASE
	IF(H+AZER .GT. ABED) THEN
CIPK JAN02 ADD SIDFF 
CIPK JAN04 ADD SIDFQQ 
	  SIDFT=SIDFQ+SIDFF(NN)
	  SIDFQQ=SIDFQQ+SIDFF(NN)
        ELSE
	  SIDFT=SIDFF(NN)
	  SIDFQQ=SIDFF(NN)
	ENDIF

C.....EVALUATE THE BASIC EQUATIONS WITH PRESENT VALUES.....

C
C.....MOMENTUM TERMS.....
C
  279 FRN = FRN + H*(AKX*R*DRDX + S*DRDZ) + R*SIDFT - SIDFQ*eina
      FSN = FSN + H*(AKY*S*DSDZ + R*DSDX) + S*SIDFT - SIDFQ*einb
C
C.....VISCOUS TERMS.....
C
      FRN=FRN+EPSX*DRDX*DHDX+EPSXZ*DRDZ*DHDZ
      FRNX=EPSX*H*DRDX
      FRNZ=EPSXZ*H*DRDZ
      FSN=FSN+EPSZ*DSDZ*DHDZ+EPSZX*DSDX*DHDX
      FSNX=EPSZX*H*DSDX
      FSNZ=EPSZ*H*DSDZ
C
C.....SURFACE AND BOTTOM SLOPE (PRESSURE) TERMS.....
C
      FRN=FRN+GHC*DAODX
      FSN=FSN+GHC*DAODZ
      FRNX=FRNX-H*GHC/2.
      FSNZ=FSNZ-H*GHC/2.
C
C.....BOTTOM FRICTION TERMS.....
C
      FRN = FRN + FFACT*VECQ*R*UBF
      FSN = FSN + FFACT*VECQ*S*VBF

CIPK MAR01   ADD DRAG TERM

      FRN = FRN + GRAV*VECQ*R*UBF*DRAGX(NR)*H      
      FSN = FSN + GRAV*VECQ*S*VBF*DRAGY(NR)*H
C
C.....CORIOLIS TERMS.....
C
      FRN = FRN - OMEGA*S*H
      FSN = FSN + OMEGA*R*H
C-
C-..... WIND TERMS
C-
      FRN=FRN-SIGMAX
      FSN=FSN-SIGMAZ
C
C.....MOTION EQUATIONS.....
C
      DO 285 M = 1, NCN
      IA = 1 + NDF*(M-1)
      F(IA) = F(IA) - AMS*(XN(M)*FRN + DNX(M)*FRNX + DNY(M)*FRNZ)
      IA = IA + 1
      F(IA) = F(IA) - AMS*(XN(M)*FSN + DNX(M)*FSNX + DNY(M)*FSNZ)
  285 CONTINUE
C
C.....CONTINUITY EQUATION.....
C
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT  
      FRN=H*(DRDX+DSDZ)+R*DHDX+S*DHDZ-SIDFT
      IF(ICYC.GT.0) FRN=FRN+BETA3
      DO 290 M=1,NCNX
      IA = 3 + 2*NDF*(M-1)
      F(IA) = F(IA) - AMW*XM(M)*FRN
  290 CONTINUE
C-
C......THE SALINITY EQUATION
C-
  291 CONTINUE

cipk jun02
      IF(ICK .EQ. 7) THEN

C     equilibrium method

        FRNX=AMU*DIFX*DSALDX*H
        FRNY=AMU*DIFY*DSALDY*H
        FRN=AMU*H*(R*DSALDX+S*DSALDY)
     1   -AMU*SIDFT*(SIDQ(NN,ICK-4)-SALT)
     +   -AMU*H*GAIN
        IF( ICYC .GT. 0) FRN=FRN+AMU*DSALDT*H
        IA=-4
        DO M=1,NCNX
          IA=IA+8
          F(IA)=F(IA)-(XM(M)*FRN+DMX(M)*FRNX+DMY(M)*FRNY)
        enddo

	ELSE
        FRNX=AMU*DIFX*DSALDX*H
        FRNY=AMU*DIFY*DSALDY*H
        FRN=AMU*H*(R*DSALDX+S*DSALDY)
     1   -AMU*(SIDFQQ*(SIDQ(NN,ICK-3)-SALT)+EXTL)
     +   -AMU*H*(SRCSNK+(GRATE-QIN)*SALT)
CIPK AUG06  ADD QIN IN ABOVE     
CIPK SEP02 ADD EXTL FROM SLUMP SOURCE
CIPK NOV97 ADJUST LINE ABOVE FOR SALINITY LOADING
CIPK AUG95    ADD LINE ABOVE FOR RATE TERMS
        IF( ICYC .GT. 0) FRN=FRN+AMU*DSALDT*H
        IA=0
        DO 295 M=1,NCN
          IA=IA+4
          IF(NSTRT(NCON(M),1) .EQ. 0) THEN
cipk aug98
            F(IA)=F(IA)-(XO(M)*FRN+DOX(M)*FRNX+DOY(M)*FRNY)
          ENDIF
  295   CONTINUE
      ENDIF

cipk jun05
      IF(NR .GT. 90  .and.  nr  .lt. 100) GO TO 380
C
C.....FORM THE X MOTION EQUATIONS.....
C
C.....FLOW TERMS.....
C
C
C.....INERTIAL COMPONENTS.....
C
C
C  N*N 
      T1=AMS*(AKX*H*DRDX+(TFRIC+TDRAGX*H)*UBF*(2.*(R*UBF)**2+(S*VBF)**2)
     +       +SIDFT)

C  N*DNX
      !EFa may07, added dhdx*epsx and dhdz*epsxz
      !T2=AMS*AKX*H*R
      T2=AMS*(AKX*H*R+epsx*dhdx)

C  N*DNY
!      T3=AMS*H*S
      T3=AMS*(H*S+epsxz*dhdz)
      !-

C  N*N ON V
      T4=AMS*(H*(DRDZ-OMEGA)+(TFRIC+TDRAGX*H)*UBF*R*S*UBF*VBF)

C  DNX*DNX ON V
      T5=AMS*EPSX*H

C  DNY*DNY ON V
      T6=AMS*EPSXZ*H
      IB=1-NDF
      DO 310 N=1,NCN
      IB=IB+NDF
      FEEAN=XN(N)*T1+DNX(N)*T2+DNY(N)*T3
      FEEDN=XN(N)*T4
      FEEBN=T5*DNX(N)
      FEECN=T6*DNY(N)
C-
C-.....FORM THE TIME TERMS.....
C-
      IF( ICYC .EQ. 0 ) GO TO 304
      FEEAN=FEEAN+AMS*XN(N)*H*ALTM
  304 CONTINUE
      IA=1-NDF
      DO 305 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN
     1  + DNY(M)*FEECN
      ESTIFM(IA,IB+1) = ESTIFM(IA,IB+1) + XN(M)*FEEDN
  305 CONTINUE
  310 CONTINUE
C
C.....FORM THE HEAD TERMS.....
C
C  N*M
      T1=(AKX*R*DRDX+S*(DRDZ-OMEGA)+GRAV*DAODX+
     +    GRAV*VECQ*R*UBF*DRAGX(NR))*AMS
     +     + ams*dffact*vecq*r
C  N*MX  
      T2=AMS*DRDX*EPSX
C  N*MY  or  NY*M
      T3=AMS*DRDZ*EPSXZ
C  NX*M
      T4=AMS*(EPSX*DRDX-GHC)
      IB=3-2*NDF
      DO 325 N=1,NCNX
      IB=IB+2*NDF
CMAY93      FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*T3
CIPK NOV97      FEEAN=XM(N)*T1
      IF (IDNOPT.GE.0) THEN
         FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*T3
      ELSE
         FEEAN=XM(N)*T1+DMX(N)*(T2+AMS*GRAV*H*DAME(N))+DMY(N)*T3
      ENDIF
CIPK NOV97
      FEEBN=XM(N)*T4
      FEECN=XM(N)*T3
C-
C-.....FORM THE TIME TERMS.....
C-
      IF( ICYC .LE. 0 ) GO TO 317
      FEEAN=FEEAN+AMS*XM(N)*BETA1
  317 CONTINUE
      IA=1-NDF
      DO 320 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN
     1  + DNY(M)*FEECN
  320 CONTINUE
  325 CONTINUE
C-
C......FORM THE SALINITY TERMS
C-
      TAA=AMU*H**2/2.*DRDS*GRAV
      TAB=AMU*DRDS*H*(R*DRDX+S*DRDZ+GRAV*DAODX)
      IF(ICYC .GT. 0) TAB=TAB+AMU*DRDS*H*BETA1
      IB=4-NDF
      DO 330 N=1,NCN
      IB=IB+NDF
      IF(NSTRT(NCON(N),1) .EQ. 0) THEN
        FEEAN=-XO(N)*TAA
        FEEBN=XO(N)*TAB
      ENDIF
      IA=1-NDF
      DO 329 M=1,NCN
      IA=IA+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+DNX(M)*FEEAN+XN(M)*FEEBN
  329 CONTINUE
  330 CONTINUE
C
C.....FORM THE Y MOTION EQUATIONS.....
C
C.....FLOW TERMS.....
C
CIPK MAR01      T1=AMS*(AKY*H*DSDZ+TFRIC*VBF*(2.*(S*VBF)**2+(R*UBF)**2)+SIDF(NN))
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT  
      T1=AMS*(AKY*H*DSDZ+(TFRIC+TDRAGY*H)*VBF*(2.*(S*VBF)**2+(R*UBF)**2)
     +       +SIDFT)
      !EFa may07, added dhdx * epszx and dhdz * epsz
      !T2=AMS*AKY*H*R
      !T3=AMS*H*S
      T2=AMS*(AKY*H*R+dhdx*epszx)
      T3=AMS*(H*S+dhdz*epsz)
      !-
CIPK MAR01 ADD DRAG TERMS      T4=AMS*(H*(DSDX+OMEGA)+TFRIC*VBF*R*S*UBF*VBF)
      T4=AMS*(H*(DSDX+OMEGA)+(TFRIC+TDRAGY*H)*VBF*R*S*UBF*VBF)
      T5=AMS*EPSZX*H
      T6=AMS*EPSZ*H
      IB=1-NDF
      DO 340 N=1,NCN
      IB=IB+NDF
C
C.....INERTIAL COMPONENTS.....
C
      FEEAN=XN(N)*T1+DNX(N)*T2+DNY(N)*T3
      FEEDN=XN(N)*T4
      FEEBN=DNX(N)*T5
      FEECN=DNY(N)*T6
C-
C-.....FORM THE TIME TERMS.....
C-
      IF( ICYC .LE. 0 ) GO TO 334
      FEEAN=FEEAN+AMS*XN(N)*ALTM*H
  334 CONTINUE
      IA=2-NDF
      DO 335 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEDN
      ESTIFM(IA,IB+1) = ESTIFM(IA,IB+1) + XN(M)*FEEAN + DNX(M)*FEEBN
     1  + DNY(M)*FEECN
  335 CONTINUE
  340 CONTINUE
C
C.....HEAD TERMS.....
C
      T1=AMS*(AKY*S*DSDZ+R*(OMEGA+DSDX)+GRAV*DAODZ +
     +    GRAV*VECQ*S*VBF*DRAGY(NR))
     +     + ams*dffact*vecq*s

      T2=AMS*EPSZX*DSDX
      T3=AMS*EPSZ*DSDZ
      T4=AMS*EPSZ*DSDZ-GHC*AMS
      IB=3-2*NDF
      DO 355 N=1,NCNX
      IB=IB+2*NDF
C-
C-.....INERTIAL COMPONENTS.....
C-
CMAY93      FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*T3
CIPK NOV97      FEEAN=XM(N)*T1
      IF (IDNOPT.GE.0) THEN
        FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*T3
      ELSE
        FEEAN=XM(N)*T1+DMX(N)*T2+DMY(N)*(T3+AMS*GRAV*H*DAME(N))
      ENDIF
CIPK NOV97
      FEEBN=XM(N)*T2
      FEECN=XM(N)*T4
C-
C-.....FORM THE TIME TERMS.....
C-
      IF( ICYC .LE. 0 ) GO TO 347
      FEEAN=FEEAN+AMS*XM(N)*BETA2
  347 CONTINUE
      IA=2-NDF
      DO 350 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN
     1  + DNY(M)*FEECN
  350 CONTINUE
  355 CONTINUE
C-
C......FORM THE SALINITY TERMS
C-
      TAB=AMU*DRDS*H*(R*DSDX+S*DSDZ+GRAV*DAODZ)
      IF(ICYC .GT. 0) TAB=TAB+AMU*DRDS*H*BETA2
      IB=4-NDF
      DO 359 N=1,NCN
      IB=IB+NDF
      IF(NSTRT(NCON(N),1) .EQ. 0) THEN
        FEEAN=-XO(N)*TAA
        FEEBN=XO(N)*TAB
      ENDIF
      IA=2-NDF
      DO 358 M=1,NCN
      IA=IA+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+DNY(M)*FEEAN+XN(M)*FEEBN
  358 CONTINUE
  359 CONTINUE
C
C.....FORM THE CONTINUITY EQUATIONS.....
C
      TA=AMW*H
      TX=AMW*DHDX
      TZ=AMW*DHDZ
      TB=AMW*(DRDX+DSDZ)
      TC=AMW*R
      TD=AMW*S
      IF(ICYC .NE. 0) TB=TB+ALTM*AMW
      IA=3-2*NDF
      DO 365 M=1,NCNX
      IA=IA+2*NDF
      IB=1-NDF
      EA=XM(M)*TA
      EB=XM(M)*TX
      EC=XM(M)*TZ
      DO 360 N = 1, NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+EA*DNX(N)+EB*XN(N)
      ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+EA*DNY(N)+EC*XN(N)
  360 CONTINUE
      EA=XM(M)*TB
      EB=XM(M)*TC
      EC=XM(M)*TD
      IB=3-2*NDF
      DO 363 N=1,NCNX
      IB=IB+2*NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*EA+DMX(N)*EB+DMY(N)*EC
  363 CONTINUE
  365 CONTINUE
C-
C......FORM THE SALINITY EQUATION
C-
C......VELOCITY AND HEAD TERMS
C-
  380 CONTINUE
      T1=AMU*H*DSALDX
      T2=AMU*H*DSALDY
      T3=AMU*DIFX*DSALDX
      T4=AMU*DIFY*DSALDY
      T5=AMU*(R*DSALDX+S*DSALDY)
 
      IF(ICYC .GT. 0) T5=T5+AMU*DSALDT
      IA=4-NDF
      DO 400 M=1,NCN
      IA=IA+NDF
      IF(NSTRT(NCON(M),1) .EQ. 0) THEN
cipk aug98
        FEEAN=XO(M)*T1
        FEEBN=XO(M)*T2
CMAY93        FEECN=XO(M)*T3
CMAY93        FEEDN=XO(M)*T4
cipk aug98
        FEEEN=(XO(M)*T5 + DOX(M)*T3+DOY(M)*T4)
      ENDIF
      IB=1-NDF
      DO 385 N=1,NCN
      IB=IB+NDF
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEAN
      ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+XN(N)*FEEBN
  385 CONTINUE
      IB=3-2*NDF
      DO 390 N=1,NCNX
      IB=IB+2*NDF
c      ESTIFM(IA,IB)=ESTIFM(IA,IB)+DMX(N)*FEECN+DMY(N)*FEEDN+XM(N)*FEEEN
      ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*FEEEN
  390 CONTINUE
  400 CONTINUE
C-
C......FORM SALINITY TERMS
C-
cipk jun02
      IF(ICK .EQ. 7) THEN

C     equilibrium method

        T1=-AMU*H
        IA=-4
        DO  M=1,NCNX
          IA=IA+8
          FEEAN=XM(M)*T1
          IB=-4
          DO N=1,NCNX
            IB=IB+8
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+FEEAN*XM(N)
	    ENDDO
	  ENDDO

      ELSE

CIPK AUG95 REPLACE LINE FOR RATE TERM
cipk aug96 fix term for loadings
cipk aug96      T1=0.
cipk aug96      IF(ICYC .GT. 0) T1=AMU*ALTM*H
CIPK NOV97 REWRITE FOR NEW UNITS OF SIDF      T1=-AMU*H*(SIDF(NN)+GRATE)
C IPK MAR01 REPLACE SIDF(NN) WITH SIDFT  TEN WITH SIFFQQ MAY04
        T1=-AMU*((GRATE-QIN)*H-SIDFQQ)
CIPK ADD QIN TO THE ABOVE      
C      T1=0.
        IF(ICYC .GT. 0) T1= T1 + AMU*ALTM*H
        T2=AMU*DIFX*H
        T3=AMU*DIFY*H
        T5=AMU*R*H
        T6=AMU*S*H
        IF(NN .EQ. 22 .AND.  ICYC .GT. 0) THEN
	AAA=0
	ENDIF
        IA=0
        DO 420 M=1,NCN
          IA=IA+4
          IF(NSTRT(NCON(M),1) .EQ. 0) THEN
cipk aug98
            FEEAN=XO(M)*T1

cipk nov99 add brackets for div by xht in next two lines

            FEEBN=(DOX(M)*T2+XO(M)*T5)
            FEECN=(DOY(M)*T3+XO(M)*T6)
          ENDIF
          IB=0
          DO 410 N=1,NCN
            IB=IB+4
            IF(NSTRT(NCON(N),1) .EQ. 0) THEN
              ESTIFM(IA,IB)=ESTIFM(IA,IB)
     +             +FEEAN*XO(N)+FEEBN*DOX(N)+FEECN*DOY(N)
            ENDIF
  410     CONTINUE
  420   CONTINUE
      ENDIF
C-
C......END GAUSS DO LOOP
C-
  500 CONTINUE
      IF(NTX .EQ. 0) RETURN

cipk jun05
      IF(NR .GT. 90  .and.  nr .lt. 100) GO TO 660
C       COMPUTE BOUNDARY FORCES
      DO 650 L=1,NCN,2
        N2=NCON(L+1)
CIPK JUN05        IF(IBN(N2) .NE. 1) GO TO 650
        IF(IBN(N2) .NE. 1  .AND.  IBN(N2) .NE. 10
     +    .AND.  IBN(N2) .NE. 11  .AND.  IBN(N2) .NE. 21) GO TO 650
        N1=NCON(L)
        NA=MOD(L+2,NCN)
        N3=NCON(NA)
        NC1=(L-1)*NDF+3
        NC2=(NA-1)*NDF+3
        H1=VEL(3,N1)
        H3=VEL(3,N3)
        DL(1,2)=(CORD(N2,1)-CORD(N1,1))*CX+(CORD(N2,2)-CORD(N1,2))*SA
        DL(1,1)=-(CORD(N2,1)-CORD(N1,1))*SA+(CORD(N2,2)-CORD(N1,2))*CX
        DL(2,2)=(CORD(N3,1)-CORD(N1,1))*CX+(CORD(N3,2)-CORD(N1,2))*SA
        DL(2,1)=-(CORD(N3,1)-CORD(N1,1))*SA+(CORD(N3,2)-CORD(N1,2))*CX
        IF(DL(2,2) .LT. 0.) THEN
          FTF(1)=1.0
        ELSE
          FTF(1)=-1.0
        ENDIF
        IF(DL(2,1) .LT. 0.) THEN
          FTF(2)=1.0
        ELSE
          FTF(2)=-1.0
        ENDIF
        IF(MOD(NFIX(N2)/100,10) .EQ. 2) THEN
          IHD=1
        ELSE
          IHD=0
        ENDIF
        DO 600 M=1,2
          DO 580 N=1,4
            RHO=DEN(N1)+AFACT(N)*(DEN(N3)-DEN(N1))

cipk jun05 testing            RHO=1.

            H=H1+AFACT(N)*(H3-H1)
            AZER=AO(N1)+AFACT(N)*(AO(N3)-AO(N1))
CIPK APR01 FIX BUG            AZER=AO(N1)+AFACT(N)*(AO(N3)-AO(N1))
            IF(IDNOPT .LT. 0) THEN
              AZER  = AME((L+1)/2)+ADO(N1)  + 
     +		AFACT(N)*(AME((NA+1)/2)+ADO(N3)-AME((L+1)/2)-ADO(N1))
            ELSE
              AZER=AO(N1)+AFACT(N)*(AO(N3)-AO(N1))
	      ENDIF
            XHT=ELEV-AZER
CMAY93            U=
CMAY93     +      XNAL(1,N)*VEL(1,N1)+XNAL(2,N)*VEL(1,N2)+XNAL(3,N)*VEL(1,N3)
CMAY93            V=
CMAY93     +      XNAL(1,N)*VEL(2,N1)+XNAL(2,N)*VEL(2,N2)+XNAL(3,N)*VEL(2,N3)
            UU=
     +      XNAL(1,N)*VEL(1,N1)/UDST(N1)+XNAL(2,N)*VEL(1,N2)/UDST(N2)
     +     +XNAL(3,N)*VEL(1,N3)/UDST(N3)
            VV=
     +      XNAL(1,N)*VEL(2,N1)/VDST(N1)+XNAL(2,N)*VEL(2,N2)/VDST(N2)
     +     +XNAL(3,N)*VEL(2,N3)/VDST(N3)
            U= UU*CX+VV*SA
            V=-UU*SA+VV*CX
CMAY93 ENDCHANGE
            VECQ=SQRT(U**2+V**2)
            IF(ORT(NR,11) .GT. 1.) THEN
              FFACT=1./ORT(NR,11)**2*H
            ELSE
              FFACT=ORT(NR,11)**2*FCOEF*H**0.6667/GRAV
cipk mar99 fix bug for bank friction (double count on GRAV)
          
            ENDIF
            TEMP=(DNAL(2,N)*DL(1,M)+DNAL(3,N)*DL(2,M))*GRAV/2.*RHO
            HP=TEMP*HFACT(N)*H**2/2.
            HP1=TEMP   *H*HFACT(N)
            DERR=SLOAD(M)*HP1*(AFACT(N)*(SPEC(N3,3)-VEL(3,N3))
     +          +(1.-AFACT(N))*(SPEC(N1,3)-VEL(3,N1)))
            IF(M .EQ. 2) THEN
              TFRIC=TEMP*FFACT*FTF(1)*HFACT(N)
              FFACT=TFRIC*U*VECQ
              IF(VECQ .GT. 0.001) THEN
                FDU=(2.*U**2+V**2)/VECQ*TFRIC
                FDV=U**2/VECQ*TFRIC
              ELSE
                FDU=0.
                FDV=0.
              ENDIF
            ELSE
CMAY93              TFRIC=TEMP*FFACT*FTF(2)
              TFRIC=TEMP*FFACT*FTF(2)*HFACT(N)
              FFACT=TFRIC*V*VECQ
              IF(VECQ .GT. 0.001) THEN
                FDU=V**2/VECQ*TFRIC
                FDV=(2.*V**2+U**2)/VECQ*TFRIC
              ELSE
                FDU=0.
                FDV=0.
              ENDIF
            ENDIF
            DO 575 K=1,3
              MA=MOD((L+K-2)*NDF+M,NEF)
              MA1=MA+3-2*M
              F(MA)=F(MA)+HP*XNAL(K,N)*SLOAD(M)
              IF(IHD .EQ. 0) THEN
                F(MA1)=F(MA1)+XNAL(K,N)*FFACT
                ESTIFM(MA,NC1)=ESTIFM(MA,NC1)
     +                         -SLOAD(M)*(1.-AFACT(N))*XNAL(K,N)*HP1
                ESTIFM(MA,NC2)=ESTIFM(MA,NC2)
     +                         -SLOAD(M)*AFACT(N)*XNAL(K,N)*HP1
                ESTIFM(MA1,NC1)=ESTIFM(MA1,NC1)
     +                         -XNAL(K,N)*FFACT/H*(1.-AFACT(N))
                ESTIFM(MA1,NC2)=ESTIFM(MA1,NC2)
     +                         -XNAL(K,N)*FFACT/H*AFACT(N)
                ESTIFM(MA1,NC1-2)=ESTIFM(MA1,NC1-2)
     +                         -FDU*XNAL(K,N)*XNAL(1,N)
                ESTIFM(MA1,NC1-1)=ESTIFM(MA1,NC1-1)
     +                         -FDV*XNAL(K,N)*XNAL(1,N)
                ESTIFM(MA1,NC1+NDF-2)=ESTIFM(MA1,NC1+NDF-2)
     +                         -FDU*XNAL(K,N)*XNAL(2,N)
                ESTIFM(MA1,NC1+NDF-1)=ESTIFM(MA1,NC1+NDF-1)
     +                         -FDV*XNAL(K,N)*XNAL(2,N)
                ESTIFM(MA1,NC2-2)=ESTIFM(MA1,NC2-2)
     +                         -FDU*XNAL(K,N)*XNAL(3,N)
                ESTIFM(MA1,NC2-1)=ESTIFM(MA1,NC2-1)
     +                         -FDV*XNAL(K,N)*XNAL(3,N)
              ELSE
                F(MA)=F(MA)+DERR*XNAL(K,N)
              ENDIF
  575       CONTINUE
  580     CONTINUE
  600   CONTINUE
  650 CONTINUE
  660 CONTINUE
C-
C- APPLY TRANSFORMATIONS TO STIFFNESS AND FORCE MATRICES FOR SLOPING B. C.
C-
      DO 1000 N=1,NCN
        N1=NCON(N)
        AFA=ALFA(N1)-THNN-ADIF(N1)
        IF(AFA) 820,1000,820
  820   CX=COS(AFA)
        SA=SIN(AFA)
        IB=NDF*(N-1)+1
        DO 900 M=1,NCN
          DO 900 MM=1,NDF
            IA=NDF*(M-1)+MM
            TEMP=ESTIFM(IA,IB)*CX + ESTIFM(IA,IB+1)*SA
            ESTIFM(IA,IB+1)=-ESTIFM(IA,IB)*SA + ESTIFM(IA,IB+1)*CX
            ESTIFM(IA,IB)=TEMP
  900   CONTINUE
        DO 990 M=1,NCN
          DO 990 MM=1,NDF
            IA=NDF*(M-1)+MM
            TEMP=ESTIFM(IB,IA)*CX + ESTIFM(IB+1,IA)*SA
            ESTIFM(IB+1,IA)=-ESTIFM(IB,IA)*SA + ESTIFM(IB+1,IA)*CX
            ESTIFM(IB,IA)=TEMP
  990   CONTINUE
        TEMP=CX*F(IB) + SA*F(IB+1)
        F(IB+1)=-F(IB)*SA + F(IB+1)*CX
        F(IB)=TEMP
 1000 CONTINUE

cipk jun05
C
C      test for control structure
C
      IF(IMAT(NN) .LT. 904  .or.  imat(nn) .gt. 1000) THEN

        DO L=1,NCN
          N2=NOP(NN,L)
          IF(IBN(N2) .GE. 10  .AND.  ISUBM(N2) .EQ. 0) THEN
            NA=(L-1)*NDF+1
            DO  KK=1,NEF
              ESTIFM(NA,KK)=0.
            ENDDO
            F(NA)=0.
          ENDIF
        ENDDO
      ENDIF

C
cipk dec97 apply sclae factors for elevtaion bc's if necessary
C-
C...... Apply scale factors to velocities for special boundaries
C-
      DO 1030 N=1,NCN
cipk nov95 remove iabs
        M=NOP(NN,N)
        IF(VSCALE(M) .NE. 0.) THEN
          NEQ=NDF*NCN
          IA=NDF*(N-1)+1
          DO 1025 I=1,NEQ
            ESTIFM(I,IA)=ESTIFM(I,IA)*VSCALE(M)
 1025     CONTINUE
        ENDIF
 1030 CONTINUE
cipk dec97 end changes
C-
C...... For 1D - 2D junctions adjust equation for direction
C-
      DO 1050 N=1,NCN,2
        M=NCON(N)
        IF(ADIF(M) .NE. 0.) THEN
          NEQ=NDF*NCN
          IA=NDF*(N-1)+1
          DO 1040 I=1,NEQ
            ESTIFM(I,IA)=ESTIFM(I,IA)+ESTIFM(I,IA+1)*SIN(ADIF(M))
     1                   /COS(ADIF(M))
 1040     CONTINUE
        ENDIF
 1050 CONTINUE
      IF(NR .GT. 90) GO TO 1310
C-
C......INSERT EXPERIMENTAL UPSTREAM BOUNDARY FLOWS
C-
      DO 1300 N=1,NCN
      M=NCON(N)
C-
C...... Test for and then retrieve stage flow constants
C-
      IF(ISTLIN(M) .NE. 0) THEN
        J=ISTLIN(M)
        AC1=STQ(J)
        AC2=STQA(J)
        E0=STQE(J)
        CP=STQC(J)
        ASC=ALN(J)
      ELSE
        AC2=0.
      ENDIF

      !nis,jul07: Write 1D-2D-line-Transition values to equation system
      if (TransitionMember(M)) then
        !get momentum equation of 2D-node at transition
        IRW = NDF * (N - 1) + 1
        IRH = NDF * (N - 1) + 3
        !calculate absolute velocity
        VX  = VEL (1,M) * COS (ALFA (M)) + VEL (2,M) * SIN (ALFA (M))
        !reset Jacobian
        do j = 1, nef
          estifm(irw, j) = 0.0
        enddo
        !form specific discharge values like inner boundary condition
        F (IRW)           = spec(M, 1) - vx * vel(3, M)
        ESTIFM (IRW, IRW) = vel(3, M)  - dspecdv(M)
        ESTIFM (IRW, IRH) = vx         - dspecdh(M)
      end if
      !-

      NFX=NFIX(M)/1000
      IF(NFX .LT. 13) GO TO 1300
      IRW=NDF*(N-1)+1
      IF(NFX .EQ. 13) IRW=IRW+1
      IRH=NDF*(N-1)+3
      VX=VEL(1,M)*COS(ALFA(M))+VEL(2,M)*SIN(ALFA(M))
      DO 1200 J=1,NEF
 1200 ESTIFM(IRW,J)=0.

      IF(MOD(N,2) .EQ. 0) GO TO 1250
      !nis,com: AC2.eq.0, if there was no h-Q-relationship, but just a water level BC
      IF(AC2 .EQ. 0.) THEN
        ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)
        ESTIFM(IRW,IRH)=AREA(NN)*VX
        F(IRW)=AREA(NN)*(SPEC(M,1)-VX*VEL(3,M))
        !EFa aug07, stage-flow boundaries (table)
        if (istab(m).gt.0.) then
          af = vel(3,m) / asc
          if (spec(m,1).lt.0.) then
            adir = -1.
          else
            adir = 1.
          end if
          srfel = hel(m) + ao(m)
          call stfltab(m,srfel,dfdh,ff,1)
          f(irw) = area(nn) * (af * adir * ff - vx * vel(3,m))
          estifm(irw,irw) = area(nn) * vel(3,m)
          estifm(irw,irh) = area(nn) * (vx - af * adir * dfdh)
        end if
        !-
      ELSE
        AF=VEL(3,M)/ASC
CIPK NOV97    F(IRW)=AREA(NN)*(AF*(AC1+AC2*(VEL(3,M)+AO(M)-E0)**CP)-VX
CIPK NOV97     1         *VEL(3,M))
        IF (IDNOPT.GE.0) THEN
          WSEL=VEL(3,M)+AO(M)
        ELSE
          HM = VEL(3,M)
          CALL AMF(HS,HM,AKP(M),ADT(M),ADB(M),AMEL,DUM2,0)
          WSEL = HS+ADO(M)
        ENDIF
        F(IRW)=AREA(NN)*(AF*(AC1+AC2*(WSEL-E0)**CP)-VX
     1            *VEL(3,M))
CIPK NOV97
        ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)
CIPK NOV97        ESTIFM(IRW,IRH)=AREA(NN)*(VX-AF*AC2*CP*(VEL(3,M)+AO(M)-E0)**
CIPK NOV97     1                  (CP-1.0))
        ESTIFM(IRW,IRH)=AREA(NN)*(VX-AF*AC2*CP*(WSEL-E0)
     1                     **(CP-1.0))
CIPK NOV97
      ENDIF
      GO TO 1300
 1250 N1=NCON(N-1)
      N2=MOD(N+1,NCN)
      N3=NCON(N2)
      HM=(VEL(3,N1)+VEL(3,N3))/2.
      IRI=(N2-1)*NDF+3
      IF(AC2 .EQ. 0.) THEN
        ESTIFM(IRW,IRW)=AREA(NN)*HM
        ESTIFM(IRW,IRH-NDF)=AREA(NN)*VX/2.
        ESTIFM(IRW,IRI)=AREA(NN)*VX/2.
        F(IRW)=AREA(NN)*(SPEC(M,1)-VX*HM)
        !EFa aug07, stage-flow boundaries (table)
        if (istab(m).gt.0.) then
          af = vel(3,m) / asc
          if (spec(m,1).lt.0.) then
            adir = -1.
          else
            adir = 1.
          end if
          hm1 = (hel(n1) + hel(n3)) / 2.
          srfel = hm1 + ao(m)
          call stfltab(m,srfel,dfdh,ff,1)
          f(irw) = area(nn) * (af * adir * ff - vx * hm)
          estifm(irw,irw) = area(nn) * hm
          estifm(irw,irh-ndf) = area(nn) / 2. * (vx - af * adir * dfdh)
          estifm(irw,iri) = estifm(irw,irh-ndf)
        end if
        !-
      ELSE
        AF=HM/ASC
CIPK NOV97        F(IRW)=AREA(NN)*(AF*(AC1+AC2*(HM+AO(M)-E0)**CP)-VX*HM)
        IF (IDNOPT.GE.0) THEN
          AOL=AO(M)
        ELSE
          CALL AMF(HS,HM,AKP(M),ADT(M),ADB(M),AMEL,DUM2,0)
          AOL = ADO(M) +HS
        ENDIF
        F(IRW)=AREA(NN)*(AF*(AC1+AC2*(HM+AOL-E0)**CP)-VX*HM)
CIPK NOV97
        ESTIFM(IRW,IRW)=AREA(NN)*HM
CIPK NOV97        ESTIFM(IRW,IRH-NDF)=AREA(NN)/2.*(VX-AF*AC2*CP*(HM+AO(M)-E0)**
CIPK NOV97     1                    (CP-1.0))
        ESTIFM(IRW,IRH-NDF)=AREA(NN)/2.*(VX-AF*AC2*CP*
     1                         (HM+AOL-E0)**(CP-1.0))
CIPK NOV97
        ESTIFM(IRW,IRI)=ESTIFM(IRW,IRH-NDF)
      ENDIF
 1300 CONTINUE
 1310 CONTINUE
 
CIPK JUN05
 1320 CONTINUE

      IF(IDNOPT .LT. 0) THEN
        DO N=1,NEF
          DO M=1,NCNX
            MM=(M-1)*NDF*2+3
            ESTIFM(N,MM)=ESTIFM(N,MM)*EFPORNN(M)
          ENDDO
        ENDDO
      ENDIF

      !nis,com: writing elt's Jacobian into global matrix
      DO 1450 I=1,NCN
        J=NCON(I)
        IA=NDF*(I-1)
        DO 1400 K=1,NDF
          IA=IA+1
          JA=NBC(J,K)
          IF(JA.GT.0) THEN
            R1(JA)=R1(JA)+F(IA)

C            rkeepeq(ja)=rkeepeq(ja)+f(ia)
          ENDIF
 1400   CONTINUE
 1450 CONTINUE


      !nis,com: Writing elt's Jacobian in file
      if (nn == 92 .or. nn == 93 .or. nn == 95) then
        !active degreecount
        dca = 0
        !active positions
        do i = 1, 32
          nbct(i,1) = 0
          nbct(i,2) = 0
          sort(i) = 'N'
        end do

        do i = 1, ncn
          do j = 1, 4
            if (nbc(nop(nn,i), j) /= 0) then
              dca = dca + 1
              if (j <=2) then
                sort(dca) = 'I'
              ELSEIF (j == 3) then
                sort(dca) = 'C'
              else
                sort(dca) = 'S'
              endif
              nbct (dca,1) = i
              nbct (dca,2) = j
            endif
          end do
        end do

        WRITE(FMT1, '(a5,i2.2,a9)') '(21x,', dca, '(1x,i10))'
        write(FMT2, '(a14,i2.2,a18)')
     +    '(a1,i1,a2,i10,', dca+1, '(1x,f10.2),1x,i10)'

        WRITE(9919,*) 'Element ', nn, 'coef2nt, ',dca,'active equations'
        WRITE(9919, FMT1)
     +    ( nbc (nop(nn, nbct(j,1)), nbct(j,2)), j=1, dca)
        DO i = 1, dca
          k = (nbct(i,1) - 1) * 4 + nbct(i,2)
          WRITE(9919, FMT2)
     +     sort(i), nbct(i,1), ': ',
     +     nbc( nop(nn, nbct(i,1)), nbct(i,2)),
!     +     f(nbc( nop(nn, nbct(i,1)), nbct(i,2))),
     +     f(k),
     +     (estifm(k, (nbct(j,1) - 1) * 4 + nbct(j,2)), j=1, dca),
     +     nbc( nop(nn, nbct(i,1)), nbct(i,2))
        ENDDO
        WRITE(9919,*)
        WRITE(9919,*)
      endif
      !-

!      WRITE(196,*) 'ELEMENT',NN,' F'
!      WRITE(196,1996) (F(I),I=1,32)
!      WRITE(196,*) 'ESTIFM'
!      WRITE(196,1996) (ESTIFM(I,I),I=1,32)
! 1996 FORMAT(1P4E15.6)      

      RETURN
      
CIPK JUN05
 2000 CONTINUE
      IF(IMAT(NN) .EQ. 990) RETURN
C-
C...... Special cases for control structures or junction sources
C-
      IF(IMAT(NN) .GT. 903) THEN
        CALL CSTRC2D(NN)
        GO TO 1320
      ENDIF
      RETURN
      END
