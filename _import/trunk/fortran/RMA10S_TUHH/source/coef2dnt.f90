!IPK  LAST UPDATE AUG 22 2007 UPDATE TO BLKECOM
!IPK  LAST UPDATE SEP 05 2006 ADD QIN FOR CONSV AND AVEL LOADING FOR CLAY OPTION
!NiS  LAST UPDATE APR XX 2006 Adding flow equation of Darcy-Weisbach
!IPK  LAST UPDATE DEC 22 2005 MAKE INITIAL EXTL CALCILATION ONLY FOR ICK=6
!IPK  LAST UPDATE SEP 29 2005 MAKE ALP1 AND ALP2 INTERPOLATION LINEAR
!ipk  last update june 27 2005 add control structure option
!IPK  LAST UPDATE SEP 26 2004  ADD MAH AND MAT OPTION
!IPK  LAST UPDATE AUG 06 2003 ADD TEST TO REMOVE STRESSES WHEN DRY
!ipk  LAST UPDATE jun 29 2003 add STRESS component
!ipk  last update jan 13 2002 add momentum to element sources
!IPK  LAST UPDATE SEP 30 2002 ADD ICE FORMULATION
!ipk  last update may 03 2003 reduce grate and srcsnk to zero when IEDROP active
!IPK  LAST UPDATE MAR 20 2003 ADD MINIMUM TEST
!IPK  LAST UPDATE SEP  4 2002  ADD LOGIC FOR WAVE SENSITIVE FRICTION
!IPK  LAST UPDATE AUG 28 2002 SET WIND STRESS AND WAVE STRESS TO ZERO BELOW A THRESHOLD DEPTH
!ipk  last update aug 14 2002 set wind and wave stress to zero for dry areas
!IPK  LAST UPDATE MAY 28 2002 ADD SURFACE STRESS INPUT
!IPK  LAST UPFDTE fEB 22 2002 REDCUE PECLET TEST
!IPK  LAST UPDATE JAN 15 2002 ADD SIDFF FOR COLLAPSING CASE
!IPK  LAST UPDATE APR 20 2001 MOVE INITIALIZATION OF VOLS TO LATER ON 
!IPK  LAST UPDATE APR 02 2001 FIX AZER FOR BOUNDARIES
!IPK  LAST UPDATE MAR 26 2001 REPLACE SIDF(NN) WITH SIDFT  
!IPK  LAST UPDATE MAR 02 2001 ADD VARIABLE MANNING N
!ipk  last update Jan 6 2000 fix azer bug for wetting and drying
!ipk  last update Nov 12 1999 allow for collapsing 3-d to 2-d
!ipk  last update Mar 17 1999 fix bug in bank friction
!ipk  last update Nov 12 add surface friction
!ipk  last update Aug 6 1998 complete division by xht for transport eqn
!ipk  last update Jan 21 1998
!ipk  last update Dec 16 1997
!     Last change:  MD    9 Jun 2009    2:18 pm
!IPK  LAST UPDATED NOVEMBER 13 1997
!ipk  New routine for Smagorinsky closure Jan 1997
      SUBROUTINE COEF2DNT(NN,NTX)
      USE COEF2MOD
      USE BLKHMOD
      USE BLKSMOD
      USE EPORMOD
      USE WATPMOD
      USE ICE2MOD
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
      USE BLKSSTMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKECOM
!NiS,apr06: adding block for DARCY-WEISBACH friction
      USE PARAKalyps
!-
      SAVE
!
!NiS,jul06: There's a problem with the data types while calling amf. In other subroutines amf is called by
!           passing the value directly as vel(3,n) (real kind=8). In this subroutine the vel(3,n) value is
!           stored in a local copy that is implicitly real, kind=4. All the temporary values are now declared
!           also as real, kind=8.
      REAL(KIND=8) :: HS, HM, DUM1
!-
!
!EFa aug07, stage-flow-boundaries
      REAL(KIND=8) :: hm1
!-
      integer :: TransLine, transtype
!
!
!
!ycw aug94 add double precision salt
      REAL*8 SALT
!
      REAL (kind = 8) :: DHDX,DHDZ,DAODX,DAODZ,H,AZER
      REAL (kind = 8) :: GHC,FRN,FRNX,FRNZ
!
      REAL (kind = 8) :: TEMP,HP,HP1,DERR
!
      real (kind = 8) :: lambda_shore, lambdaKS_shore, lambdaDunes_shore
      real (kind = 8) :: lambdaP_shore 
      real (kind = 8) :: lambda, lamKS, lamP, lamDunes
!
!IPK JUN03
!      COMMON /STR/
!     +  STRESS(MNP,2),STR11(MNP),STR21(MNP),STR10(MNP),STR20(MNP)
!
!ipk apr05 add line above
!-
!IPK AUG07
      DIMENSION FTF(2),PROJL(8)
!IPK SEP96 ADD PROJL
!
      REAL J11,J12,J21,J22
!-
      DATA FCOEF/14.47/,THRESH/1.0E-3/,PI/3.14159/
!
!
!---------------
!Execution block
!---------------
!
!define some constants due to unit system      
      IF (GRAV < 32.)  THEN
        FCOEF = GRAV
      ELSE
        FCOEF = GRAV/2.208
      ENDIF
!
!determine average density within element      
      ROAVG=1.935
      IF (GRAV < 32.)  ROAVG = 516. * 1.935
!
!IPK MAR03  REPLACE TH(NN) WITH THNN
!direction of element      
      THNN=TH(NN)
!
!
!-
!-.....ASSIGN PROPER COEFS.....
!-
!IPKNOV97 ADD TVOL
!IPK APR 01 INITIALISATION MOVED FURTHER ON 
!      TVOL(NN)=0.
!
!Find number of corner nodes of current element      
      IF(ITEQV(MAXN) == 5) THEN
        DO 61 N=1,8
          NCON(N)=NOPS(NN,N)
          IF(NCON(N) /= 0) NCN=N
   61   CONTINUE
      ELSE
        NCN=NCORN(NN)
        DO 63 N=1,NCN
          NCON(N)=NOP(NN,N)
   63   CONTINUE
      ENDIF
!
!IPK AUG06 ADD LOGIC TO AVE DEPRAT ETC
!MD:  only for LSS > 0: Cohesive SEDIMENT
!MD:  ......................................
      IF(LSS > 0 .AND. IAVEL == 1) THEN
        IF(NCN == 6) THEN
        edotm=-(edot(NOP(NN,1))+edot(NOP(NN,3))+edot(NOP(NN,5)))/6.+    &
     &  (edot(NOP(NN,2))+edot(NOP(NN,4))+edot(NOP(NN,6)))/2.
        seratm=-(serat(NOP(NN,1))+serat(NOP(NN,3))+serat(NOP(NN,5)))/6.+&
     &  (serat(NOP(NN,2))+serat(NOP(NN,4))+serat(NOP(NN,6)))/2.
        depratm=                                                        &
     &  -(deprat(NOP(NN,1))+deprat(NOP(NN,3))+deprat(NOP(NN,5)))/6.+    &
     &  (deprat(NOP(NN,2))+deprat(NOP(NN,4))+deprat(NOP(NN,6)))/2.
        elseif(ncn == 8)then
        edotm=-(edot(NOP(NN,1))+edot(NOP(NN,3))+edot(NOP(NN,5))+        &
     &            edot(NOP(NN,7)))/12.+                                 &
     &   (edot(NOP(NN,2))+edot(NOP(NN,4))+edot(NOP(NN,6))+              &
     &            edot(NOP(NN,8)))/3.  
        seratm=-(serat(NOP(NN,1))+serat(NOP(NN,3))+serat(NOP(NN,5))+    &
     &            serat(NOP(NN,7)))/12.+                                &
     &   (serat(NOP(NN,2))+serat(NOP(NN,4))+serat(NOP(NN,6))+           &
     &            serat(NOP(NN,8)))/3.  
        depratm=-(deprat(NOP(NN,1))+deprat(NOP(NN,3))+deprat(NOP(NN,5))+&
     &            deprat(NOP(NN,7)))/12.+                               &
     &  (deprat(NOP(NN,2))+deprat(NOP(NN,4))+deprat(NOP(NN,6))+         &
     &            deprat(NOP(NN,8)))/3.
        endif
      ENDIF
!MD:  END only for LSS > 0: Cohesive SEDIMENT
!
!IPK JUN05 MOVE LOOP
!get number of element equations (dependent variables)      
      NEF=NCN*NDF
!initialize residual vector f and Jacobian matrix estifm      
      DO  I=1,NEF
        F(I) = 0.0
        DO  J=1,NEF
          ESTIFM(I,J) = 0.0
        ENDDO
      ENDDO
!
!ipk jun05
      inovel=0
      if(iteqv(maxn) == 2) inovel=1
      if(iteqv(maxn) == 8) inovel=2
      if(iteqv(maxn) == 9) inovel=3
!
!ipk oct98 update to f90
!get local copy of material type of current element      
      IMMT=IMAT(NN)
!
!ipk nov99 revise to allow for collapsing 3-d to 2-d
!evaluate material type due to 3D applications      
      if(immt > 1000) immt=immt-1000
!
!IPK JUN05
!   
!     Test for and determine whether conrol structure now operates as an
!     ordinary element.  If one node is above transition then treat as
!     a  normal element
!
!MD  only for NTX=1: Real calculation
!MD  (NTX = 0: data Reading and preparing restart)
!Check for submerged control structure element, if so treat it as normal element      
      if(ntx == 1) then
        if(imat(nn) > 900) then
          if(inovel > 0) return
          if(isubmel(nn) == 0) go to 2000
        endif
      endif
!
!Find material type definition value (last two digits show type dedinition      
      NR = MOD(IMMT,100)
!ipkjun05
!Leave material types with value > 900 as they are > control structures      
      if(immt > 900) nr=immt
!
!initialize friction factor      
      FFACT=0.
!ipk nov98 adjust for top friction
!surface or bottom friction coefficient, if Chezy is used      
      IF(ORT(NR,5) > 1. .OR. ort(nr,13) > 1.) then
        FFACT = GRAV/(CHEZ(NN)+ort(nr,13))**2
      endif
!
!
!ipk  logic for direction of horiz eddy and diffusion
!
      DIRX=0.
      DIRY=0.
      DO K=1,NCN,2
!       Normalize velocity vector length
        VNORM=SQRT(VEL(1,NCON(K))**2 + VEL(2,NCON(K))**2)
        IF(VNORM > 0.) THEN
          DIRX=DIRX+VEL(1,NCON(K))/VNORM
          DIRY=DIRY+VEL(2,NCON(K))/VNORM
        ENDIF
      ENDDO
      IF(DIRX /= 0. .AND. DIRY /= 0.) THEN
        CX=DIRX/SQRT(DIRX**2+DIRY**2)
        SA=DIRY/SQRT(DIRX**2+DIRY**2)
        THNN=ATAN2(SA,CX)
      ELSE
        CX=COS(THNN)
        SA=SIN(THNN)
      ENDIF
!
!ipk mar03 end changes
!ipk sep96 move up this computation
!-
!-.....COMPUTE LOCAL CORDS.....
!-
      CX=COS(THNN)
      SA=SIN(THNN)
      MR=NCON(1)
      DO K = 1, NCN
        N=NCON(K)
        DX=CORD(N,1)-CORD(MR,1)
        DY=CORD(N,2)-CORD(MR,2)
        XL(K)=DX*CX+DY*SA
        YL(K)=-DX*SA+DY*CX
      ENDDO
!IPK JAN03 add momentum
      EINA=EINX(NN)*CX+EINY(NN)*SA
      EINB=-EINX(NN)*SA+EINY(NN)*SA
      NCNX=NCN/2
      IF(NTX == 0) GO TO 72
!ipk nov97
!MD  only for NTX=1: Real calculation
!MD  (NTX = 0: data Reading and preparing restart)
!................................................
!
!     Initialize AME and DAME
!
      IF (IDNOPT < 0) THEN
         DO M = 1, NCNX
           MC = 2 * M - 1
           N = NOP(NN,MC)
           HS = VEL(3,N)
           ISWT = 0
           CALL AMF(DUM1,HS,AKP(N),ADT(N),ADB(N),AME(M),DAME(M),ISWT)
!ipk apr05
           efpornn(m)=efpor
         END DO
      ENDIF
!ipk nov97 end update
!
!
   72 CONTINUE
      NGP=7
!IPK JUN05      NEF=NCN*NDF
!-
!- INITIALIZE MATRICES AND VARIABLES
!-
!IPK SEP02  add logic to make ice cover functions linear
      DO i=1,ncn
        IF(ICESW > 0) THEN
          IF(MOD(I,2) == 0 .AND. I < NCN) THEN
            THKI(I)=(ICETHK(NOP(NN,I-1))+ICETHK(NOP(NN,I+1)))/2.
            QWLI(I)=(QICE(NOP(NN,I-1))+QICE(NOP(NN,I+1)))/2.
          ELSEIF(MOD(I,2) == 0 .AND. I == NCN) THEN
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
!IPK NOV97 MODERNIZE LOOP
!ipk jun05      DO  I=1,NEF
!ipk jun05        F(I) = 0.0
!ipk jun05        DO  J=1,NEF
!ipk jun05          ESTIFM(I,J) = 0.0
!ipk jun05        ENDDO
!ipk jun05      ENDDO
!-
!...... Check for element dropout
!-
!
!MD  only for NTX=0: data Reading and preparing restart
      IF(NTX == 0) GO TO 79
      DO 78 I=1,NCN
        MM=NCON(I)
        DO 77 K=1,NDF
          IF(NBC(MM,K) > 0) GO TO 79
   77   CONTINUE
   78 CONTINUE
      RETURN
   79 CONTINUE
!-
!-.....COPY PROPER WEIGHTING FUNCTIONS.....
!-
!ipk jan98 move to here because of dropout problem
      AREAE=AREA(NN)
      AREA(NN)=0.
!IPK APR01
      TVOL(NN)=0.
!
!
!ipk nov99 revise for collapsing from 3-d
!ipk mar05      IF(IMMT < 100 ) THEN
!ipk mar05        IF( NCN < 8 ) THEN
!ipk mar05          NGP = 7
!ipk mar05          DO 80 M = 1, NGP
!ipk mar05            WAITX(M) = WAITT(M)
!ipk mar05   80     CONTINUE
!ipk mar05        ELSE
!ipk mar05          NGP = 9
!ipk mar05          DO 90 M = 1, NGP
!ipk mar05            WAITX(M) = WAITR(M)
!ipk mar05   90     CONTINUE
!ipk mar05        ENDIF
!ipk mar05      ELSE
        NGP = 16
        IF( NCN < 8 ) THEN
          DO 92 M = 1, NGP
            WAITX(M)=WAITTH(M)
   92     CONTINUE
        ELSE
          DO 94 M = 1, NGP
            WAITX(M) = WAITRH(M)
   94     CONTINUE
        ENDIF
!      ENDIF
!-
!-.....COPY SHAPE FUNCTIONS
!-
      CALL SB2(NCN,NGP)
!
!IPK MAY04 RESET ELEMENT INFLOW
!
      IF(INOFLOW(NN) == 0) THEN
        SIDFQ=SIDF(NN)
      ELSE
        SIDFQ=0.
      ENDIF
      SIDFQQ=SIDF(NN)
!
!
!--------------------------------
!GAUSS LOOP GAUSS LOOP GAUSS LOOP
!--------------------------------
!-
!-.....COMPUTE ELEMENT EQUATIONS.....
!-
      DO 500 I = 1, NGP
!-
!-..... FORM THE JACOBIAN FOR QUADRATIC FUNCTIONS.....
!-
      J11 = 0.0
      J12 = 0.0
      J21 = 0.0
      J22 = 0.0
!IPK NOV97 MODERNIZE LOOP
      DO  K = 2, NCN
        J11 = J11 + DA(K,I) * XL(K)
        J12 = J12 + DA(K,I) * YL(K)
        J21 = J21 + DB(K,I) * XL(K)
        J22 = J22 + DB(K,I) * YL(K)
      ENDDO
!IPK NOV97  130 CONTINUE
      DETJ = J11 * J22 - J12 * J21
!IPK NOV97 MODERNIZE LOOP
      DO J = 1, NCN
        XN(J) = XNX(J,I)
        DNX(J) = ( J22 * DA(J,I) - J12 * DB(J,I) ) / DETJ
        DNY(J) = ( J11 * DB(J,I) - J21 * DA(J,I) ) / DETJ
        XO(J)=XN(J)
        DOX(J)=DNX(J)
        DOY(J)=DNY(J)
      ENDDO
!IPK NOV97  135 CONTINUE
      AMW = WAITX(I) * DETJ
      AREA(NN)=AREA(NN)+AMW
      IF(AMW <= 0.) WRITE(LOUT,9802) NN,I
!
 9802 FORMAT(' AMW IS ZERO OR NEGATIVE FOR ELEMENT',I5,'GAUSS NO',I5)
      IF(NTX == 0) GO TO 500
!-
!-     REPEAT FOR LINEAR FUNCTION
!-
      JJ=0
      DO 145 J=1,NCN,2
        JJ=JJ+1
        XM(JJ)=XMX(JJ,I)
        DMX(JJ)=(J22*CA(JJ,I)-J12*CB(JJ,I))/DETJ
        DMY(JJ)=(J11*CB(JJ,I)-J21*CA(JJ,I))/DETJ
  145 CONTINUE
!-
      DO 155 J=2,NCN,2
        MR=NCON(J)
        IF(NSTRT(MR,1) /= 0) THEN
          XO(J-1)=XO(J-1)+XO(J)/2.
          DOX(J-1)=DOX(J-1)+DOX(J)/2.
          DOY(J-1)=DOY(J-1)+DOY(J)/2.
          IF(J < NCN) THEN
            JP=J+1
          ELSE
            JP=1
          ENDIF
          XO(JP)=XO(JP)+XO(J)/2.
          DOX(JP)=DOX(JP)+DOX(J)/2.
          DOY(JP)=DOY(JP)+DOY(J)/2.
        ENDIF
  155 CONTINUE
!-
!...... Set momentum correction factors
!-
      AKX=0.
      AKY=0.
!-
!...... Set bottom friction coefficient
!-
      UBF=0.
      VBF=0.
!-
!.....COMPUTE R, S, H AND THEIR DERIVATIVES.....
!-
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
!IPK SEP02 ADD WAVE DATA INTERPOLATION
      TP=0.0
      HSV=0.0
      WDIR=0.0
!-
!......ESTABLISH VELOCITIES
!-
      EstabVelos: DO M=1,NCN
        MR=NCON(M)
        VXX(M)=VEL(1,MR)/UDST(MR)
        VY(M)=VEL(2,MR)/VDST(MR)
        ST(M)=VEL(ICK,MR)/SDST(MR)
        VDX(M)=VDOT(1,MR)/UDST(MR)
        VDY(M)=VDOT(2,MR)/VDST(MR)
        SDT(M)=VDOT(ICK,MR)/SDST(MR)
!
!MD    !MD: testoutput into output.out
!MD        IF (M == 1 .OR. M == 2) THEN
!MD          WRITE (75, *) 'VDOT(6,MR):', VDOT(ICK,MR), 'VEL(6,MR):'
!MD       + ,VEL(ICK,MR)
!MD        END IF
!MD    !MD: testoutput into output.out
!
        IF(ITEQV(MAXN) == 5 .AND. NDEP(MR) > 1) THEN
          NBOT=NREF(MR)+NDEP(MR)-1
          UBFC(M)=UDST(NBOT)
          VBFC(M)=VDST(NBOT)
        ELSE
          UBFC(M)=1.0
          VBFC(M)=1.0
        ENDIF
!
!
      enddo EstabVelos
!
!
!
!
      DO 270 M=1,NCN
        MR=NCON(M)
!IPK SEP02 INTERPOLATE WAVE DATA
        TP=TP+XN(M)*PEAKPRD(MR)
        HSV=HSV+XN(M)*WAVEHT(MR)
        WDIR=WDIR+XN(M)*WAVEDR(MR)
!
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
        IF(NSTRT(MR,1) == 0) THEN
          SALT=SALT+XO(M)*ST(M)
          DSALDT=DSALDT+XO(M)*SDT(M)
          DSALDX=DSALDX+DOX(M)*ST(M)
          DSALDY=DSALDY+DOY(M)*ST(M)
        ENDIF
        IF(ICYC < 1) GO TO 270
        BETA1=BETA1+XN(M)*(VDX(M)*CX+VDY(M)*SA)
        BETA2=BETA2+XN(M)*(-VDX(M)*SA+VDY(M)*CX)
  270 CONTINUE
!MD  END only for NTX=1: Real calculation
!MD  (NTX = 0: data Reading and preparing restart)
!................................................
!
!
      H = 0.0
      DHDX = 0.0
      DHDZ = 0.0
      AZER=0.0
!ipk mar01 use abed
      abed=0
      DAODX = 0.0
      DAODZ = 0.0
      SIGMAX = 0.0
      SIGMAZ = 0.0
      BETA3 = 0.0
      RHO=0.0
      DRODX=0.0
      DRODZ=0.0
!IPK NOV97 MODERNIZE LOOP AND ADD LOGIC FOR MARSH FRICTION
      BRANG=0.0
      AKAPMG=0.0
!ipk sep02 add ice parameters
      GSICE=0.
      GSQLW=0.
!IPK JUN02
      GAIN=0.
      WSELL=0.
!IPK SEP02
      EXTL=0.
!
!
!MD   Mit linearer Wichtungsfunktion!
      DO M=1,NCNX
        MC = 2*M - 1
        MR=NCON(MC)
        H = H + XM(M)*VEL(3,MR)
!ipk jun02
!MD    GAN oder GAN0 ist die Erosionsrate [kg/m³/s]
!MD    Berechnung in BEDSUR (bzw. BEDLBED)
        GAIN=GAIN+XM(M)*GAN(MR)
        WSELL=WSELL+WSLL(MR)*XM(M)
!
!IPK SEP02
!IPK DEC05
!MD   ONLY for sediment
!MD   EXTLD kommt aus SLUMP = Boeschungsbruch
!nis,jun07: ICK is not assigned, whenn ntx == 0 (beginning of program), therefore jump        
        if (ntx /= 0) then
          IF(ICK == 6) THEN
            EXTL=EXTL+XM(M)*EXTLD(MR)
          ENDIF
        endif
!
        BETA3=BETA3+XM(M)*VDOT(3,MR)
        DHDX = DHDX + DMX(M)*VEL(3,MR)
        DHDZ = DHDZ + DMY(M)*VEL(3,MR)
!IPK NOV97
        IF (IDNOPT >= 0) THEN
!
!IPK JAN00 ADD AZER HERE
          AZER=AZER+XM(M)*AO(MR)
!ipk mar01 add abed
          Abed=Abed+XM(M)*AO(MR)
!
          DAODX = DAODX + DMX(M)*AO(MR)
          DAODZ = DAODZ + DMY(M)*AO(MR)
        ELSE
!
!IPK JAN00 ADD AZER HERE
          AZER  = AZER  + XM(M)*(AME(M)+ADO(MR))
!ipk mar01 add abed
          Abed = Abed+XM(M)*AO(MR)
!
          DAODX = DAODX + DMX(M)*(AME(M)+ADO(MR))
          DAODZ = DAODZ + DMY(M)*(AME(M)+ADO(MR))
        ENDIF
        BRANG=BRANG+XM(M)*ADB(MR)
        AKAPMG=AKAPMG+XM(M)*AKP(MR)
        RHO=RHO+XM(M)*DEN(MR)
        DRODX=DRODX+DMX(M)*DEN(MR)
        DRODZ=DRODZ+DMY(M)*DEN(MR)
!IPK MAY02  ADD STRESS TERM
!
!ipk jun03 add STRESS component
!
        SIGMAX=SIGMAX+XM(M)*((SIGMA(MR,1)+stress(mr,1))*CX              &
     &                        +(SIGMA(MR,2)+stress(mr,2))*SA)
        SIGMAZ=SIGMAZ+XM(M)*(-(SIGMA(MR,1)+stress(mr,1))*SA             &
     &                        +(SIGMA(MR,2)+stress(mr,2))*CX)
!
!IPK SEP02 GET GAUSS POINT ICE VALUES
        GSICE=GSICE+XM(M)*THKI(MC)
        GSQLW=GSQLW+XM(M)*QWLI(MC)
      ENDDO
!IPK DEC05
!MD   EXTLDEL = FLUX/AREA; Wert kommt aus LOAD
!--------------------------------------------
      EXTL=EXTL+EXTLDEL(NN)
!
!
!IPK AUG03 ADD TEST TO REMOVE STRESSES WHEN DRY
      IF(H+AZER < ABED) THEN
        SIGMAX=0.
        SIGMAZ=0.
      ENDIF
!
!MD: Equation for prandtl mixing lenght:
      DSQ=SQRT((DRDX**2.)+(DSDZ**2.)+0.5*((DRDZ+DSDX)**2.))
!
!MD:  TBFACT defined by user = 0.20
!MD:  AREAE = Area of Element
      AMH=TBFACT*AREAE*DSQ
      if(tbmin > 0. .AND. amh < tbmin/2.) then
        amh=tbmin/2.
!ipk jan98
        dsq=amh/(tbfact*areae)
        C1=0.
        C2=0.
        C3=0.
        C4=0.
      elseif(dsq < 0.05/areae) then
        dsq=.05/areae
        amh=tbfact*areae*dsq
        C1=0.
        C2=0.
        C3=0.
        C4=0.
      else
        C1=TBFACT*AREAE*DRDX/DSQ
        C2=TBFACT*AREAE*0.5*(DRDZ+DSDX)/DSQ
        C3=C2
        C4=TBFACT*AREAE*DSDZ/DSQ
      endif
      EPSX=2.*AMH
      EPSXZ=AMH
      EPSZX=AMH
      EPSZ=2.*AMH
!
!      if(dsq*tbfact*sqrt(areae) < 1.) then
!        write(75,*) 'err',nn,tbfact,areae,dsq,tbmin,amh
!      endif
!IPK FEB02      if(ABS(R) > 25.*tbfact*sqrt(areae)*dsq) then
!IPK FEB02        xkpr=ABS(R)/(25.*tbfact*sqrt(areae)*dsq)
      if(ABS(R) > 5.*tbfact*sqrt(areae)*dsq) then
        xkpr=ABS(R)/(5.*tbfact*sqrt(areae)*dsq)
      else
        xkpr=1.
      endif
!
      DIFX=AMH*xkpr
!ipk mar03      DIFY=AMH*xkpr
      DIFY=AMH*xkpr*ort(nr,9)
!
!IPK EXP      IF(IOVLDE == 1) THEN
!IPK EXP        DIFX=SQRT(AREAE)
!IPK EXP        DIFY=SQRT(AREAE)
!IPK EXP      ENDIF
!        IF(I == 1) WRITE(75,*) NN,I,EPSX,AMH
!
!ipk jun05      RHO=1.0
      ROAVG=RHO
!ipk mar05      AMU=AMW*XHT
      AMU=AMW
      AMT=AMW*RHO
!ipk mar05      AMS=AMU*RHO
      AMS=AMW*RHO
!-
!...... Correct momentum factors
!-
      AKX=AKX*2.-1.0
      AKY=AKY*2.-1.0
!YYY                                   momentum factors disabled
      akx = 1.
      aky = 1.
!
!
      IF(ABS(R) > THRESH) THEN
        UBF=UBF/R
      ELSE
        UBF=1.0
      ENDIF
      IF(ABS(S) > THRESH) THEN
        VBF=VBF/S
      ELSE
        VBF=1.0
      ENDIF
      IF(ICK == 4) THEN
        DRDS=DRODS(SALT,IGF)
!IPK AUG95 DEFINE RATES
        GRATE=0.
        SRCSNK=0.
      ELSEIF(ICK == 5) THEN
        DRDS=DRODTM(SALT,IGF)
!IPK AUG95 GET RATES
        DELTT=DELT
        HS=H
        CALL MKTEMP(SALT,HS,0.,SRCSNK,GRATE,DELTT,NR,NETYP(NN))
!MD (ICK=6)      
      ELSE
!
!     Set up sand transport variables (ICK=6)
!
        IF(LSAND > 0) THEN
          ALP1=0.0
          ALP2=0.0
          DO M=1,NCN
            MR=NOP(NN,M)
            ALP1=ALP1+ALPHA1(MR)*XN(M)
            ALP2=ALP2+ALPHA2(MR)*XN(M)
          ENDDO
        ENDIF
!
!     Set up cohesive transport variables
!
        IF(LSS > 0) THEN
          ALP1=0.0
          ALP2=0.0
!IPK SEP05 MAKE INTERPOLATION LINEAR
!IPK SEP05          DO M=1,NCN
!IPK SEP05            MR=NOP(NN,M)
!IPK SEP05            ALP1=ALP1+DEPRAT(MR)*XN(M)
!IPK SEP05            ALP2=ALP2+(EDOT(MR)+SERAT(MR))*XN(M)
!
!IPK AUG06 ADD AVERAGE TEST
          IF(IAVEL == 0) THEN
            DO M=1,NCNX
              MC = 2*M - 1
              MR=NCON(MC)
              ALP1 = ALP1 + XM(M)*DEPRAT(MR)
              ALP2 = ALP2 +(EDOT(MR)+SERAT(MR))*XM(M)
            END DO
!
          ELSE
            alp1=depratm
            alp2=edotm+seratm
          ENDIF
        ENDIF
!
        GRATE=0.0
        srcsnk=0.
        HS=H
        IF(LSAND > 0) THEN
          CALL MKSAND(SALT,HS,VSET,SRCSNK,GRATE,NETYP(NN))
        ENDIF
!
        IF(LSS > 0) THEN
          CALL MKSSED(SALT,HS,VSET,SRCSNK,GRATE,NETYP(NN))
        ENDIF
!
        DRDS=DRODSD(SALT,IGF)
      ENDIF
!IPK AUG02 TEST FOR SHALLOW OR NEGATIVE DEPTH TO SET STRESS TO ZERO.
      IF(WSELL-ABED < ZSTDEP) THEN
        SIGMAX=0.
        SIGMAZ=0.
      ENDIF
      IF(WSELL < ABED) THEN
!IPK AUG06
!MD: changed 09-06-2009        
        IF(LSS > 0 .OR. LSAND > 0) THEN
          grate=0.
          srcsnk=0.
        ENDIF
!ipk aug02  make wind stress zero over dry areas
        sigmax=0.
        sigmaz=0.
      ENDIF
!
!ipk may03  reduce grate and srcsnk to zero when IEDROP active
!
!MD:  do ned=1,9 : New: more than 9 Mat-Types
      do ned=1,DROPMAX
        IF(ABS(IMMT) == iedrop(ned)) THEN
          grate=0.
          srcsnk=0.
        ENDIF
      enddo
!
      DO M=1,NCN
        MR=NCON(M)
        if(WSLL(mr) -ao(mr) < zstdep) then
          sigmax=0.
          sigmaz=0.
!IPK AUG06
!MD: changed 09-06-2009          
          IF(LSS > 0 .OR. LSAND > 0) THEN
            grate=0.
            srcsnk=0.
          ENDIF
!ipk may03  reduce nodal rates to zero
          alpha1(mr)=0.
          alpha2(mr)=0.
        endif
      enddo
!
      DRODX=DRDS*DSALDX
      DRODZ=DRDS*DSALDY
!
!IPK SEP02 ADD AN ICE THICKNESS TEST FOR WIND STRESS
!
      IF(GSICE <= 0.001) THEN
        SIGMAX = SIGMAX/RHO
        SIGMAZ = SIGMAZ/RHO
      ELSE
        SIGMAX=0.0
        SIGMAZ=0.
      ENDIF
      GHC = GRAV*H
      VECQ = SQRT((R*UBF)**2+(S*VBF)**2)
      IF(H <= 0.0) H=0.001
!
!NiS,apr06: adding possibility of FrictionFactor calculation with
!           COLEBROOK-WHITE to apply DARCY-WEISBACH equation: Therefore,
!           the if-clause has also to be changed because surface friction
!           is deactivated!
!-
!ipk nov98 adjust for surface friction
!NiS,apr06: changing test:  
!    IF(ORT(NR,5) > 0. .OR. ORT(NR,13) > 0.) THEN  
      IF(ORT(NR,5) > 0. .OR. (ORT(NR,13) > 0. .AND.                     &
     &   ORT(NR,5) /= -1.0)) THEN
!-  
!IPK SEP02
        EFMAN=0.
        IF(ORT(NR,5) < 1.0 .AND. ORT(NR,13) < 1.0) then
!IPK MAR01  ADD POTENTIAL FOR VARIABLE MANNING N
          IF(MANMIN(NR) > 0.) THEN
            IF(H+AZER < ELMMIN(NR) ) THEN 
              FFACT=(MANMIN(NR))**2*FCOEF/(H**0.333)
!IPK SEP02
              EFMAN=MANMIN(NR)
          ELSEIF(H+AZER > ELMMAX(NR) ) THEN 
              FFACT=(MANMAX(NR))**2*FCOEF/(H**0.333)
!IPK SEP02
              EFMAN=MANMAX(NR)
          ELSE
              FSCL=(H+AZER-ELMMIN(NR))/(ELMMAX(NR)-ELMMIN(NR))
              FFACT=(MANMIN(NR)+FSCL*(MANMAX(NR)-MANMIN(NR)))**2        &
     &                  *FCOEF/(H**0.333)
!IPK SEP02
              EFMAN=MANMIN(NR)+FSCL*(MANMAX(NR)-MANMIN(NR))
          ENDIF
!IPK SEP04  ADD MAH AND MAT OPTION
        ELSEIF(HMAN(NR,2) > 0 .OR. HMAN(NR,3) > 0.) THEN
            TEMAN=0.
            IF(HMAN(NR,2) > 0) THEN 
              TEMAN=HMAN(NR,3)*EXP(-H/HMAN(NR,2))
            ENDIF
            TEMAN=TEMAN+HMAN(NR,1)/H**HMAN(NR,4)
            FFACT=TEMAN**2*FCOEF/(H**0.333)
          ELSEIF(MANTAB(NR,1,2) > 0.) THEN
            DO K=1,4
              IF(H < MANTAB(NR,K,1)) THEN
                IF(K == 1) THEN
                  TEMAN=MANTAB(NR,1,2)
                ELSE
                  FACT=(H-MANTAB(NR,K-1,1))/                            &
     &                  (MANTAB(NR,K,1)-MANTAB(NR,K-1,1))
                  TEMAN=MANTAB(NR,K-1,2)                                &
     &            +FACT*(MANTAB(NR,K,2)-MANTAB(NR,K-1,2))
                ENDIF
                GO TO 280
              ENDIF
            ENDDO
            TEMAN=MANTAB(NR,4,2)
  280       CONTINUE
            FFACT=TEMAN**2*FCOEF/(H**0.333)
!ipk mar05
            DFFDH=-FFACT/(H*3.0)
          ELSE
!**************************************************************
!
!   DJW 31/10/05 : Friction Factor Modification to adjust for Roughness Calcs
!
!**************************************************************
!
!           FFACT=(ORT(NR,5)+ORT(NR,13))**2*FCOEF/(H**0.333)
          FFACT=(ZMANN(NN)+ORT(NR,13))**2*FCOEF/(H**0.333)
!
!**************************************************************
!
!        End DJW Changes
!
!**************************************************************
!ipk mar05
            DFFDH=-FFACT/(H*3.0)
        endif
!ipk mar05
        ELSE
          DFFDH=0.
        ENDIF
!
!NiS,apr06: adding RESISTANCE LAW form COLEBROOK-WHITE for DARCY-WEISBACH-equation:      
      ELSEIF (ORT(NR,5) < 0.0) THEN
!
!calculate lambda        
!nis,aug07: Introducing correction factor for roughness parameters, if Darcy-Weisbach is used        
!
        call darcy(lambda, vecq, h,                                     &
     &             cniku(nn)      * correctionKS(nn),                   &
     &             abst(nn)       * correctionAxAy(nn),                 &
     &             durchbaum(nn)  * correctionDp(nn),                   &
     &             nn, morph, gl_bedform, MaxE, c_wr(nn), 2,            &
!store values for output                   
     &             lamKS,                                               &
     &             lamP,                                                &
     &             lamDunes, dset)
!
!
!at first Gauss node the lambdas are initialized        
        if (i == 1) then
          lambdaTot (nn) = 0.0d0
          lambdaKS (nn) = 0.0d0
          lambdaP (nn) = 0.0d0
          lambdaDunes (nn) = 0.0d0
        endif
        lambdaTot (nn) = lambdaTot (nn) + waitx(i) * lambda/ 4.0d0
        lambdaKS (nn) = lambdaKS (nn) + waitx(i) * lamKS/ 4.0d0
        lambdaP (nn) = lambdaP (nn) + waitx(i) * lamP/ 4.0d0
        lambdaDunes (nn) = lambdaDunes (nn) + waitx(i) * lamDunes/ 4.0d0
!
!calculation of friction factor for roughness term in differential equation        
        FFACT = lambda/8.0
!
!TODO:        
!Is here a derivative of friciton over h necessary?        
        DFFDH = 0.
!
      ENDIF
!
!IPK MAR03 ADD MINIMUM TEST
!        IF((NN == 1284 .OR. NN == 479) .AND. I == 5) THEN
!          WRite(129,'(3i5,4g15.6)') nn,i,MAXN,difx,dify
!        ENDIF
!
      if(difx < ort(nr,14)) then
        if(difx > 0.) then
          dify=ort(nr,14)*dify/difx
        else
          dify=ort(nr,14)
        endif
        difx=ort(nr,14)
      endif
!
!MD: testoutput into output.out
!MD      IF (NN == 1 .OR. NN == 2) THEN
!MD        WRITE (75, *) 'DIFX:', DIFX, 'DIFY:', DIFY
!MD      END IF
!
!IPK SEP02  ADD LOGIC FOR WAVE SENSITIVE FRICTION
!
      IF (TP > 0. .AND. HSV > 0.) THEN
!
        Y=4.02*H/TP**2
        POL=1.+Y*(.666+Y*(.355+Y*(.161+Y*(.0632+Y*(.0218+.00654*Y)))))
        WAVENR=SQRT(Y**2+Y/POL)/H
        RLS=2.*PI/WAVENR
        ARG=WAVENR*H
        IF (ARG > 50.) THEN
          UBW=0.
        ELSE
          ABW=HSV/(2.*SINH(ARG))
          UBW  = 2.*PI/TP*ABW
        ENDIF
        CORWDIR=WDIR-THNN
        IF(S == 0. .AND. R == 0.) THEN
          CURRDIR=0.
        ELSE
          CURRDIR=ATAN2(S,R)
        ENDIF
        IF(ABS(CURRDIR-CORWDIR) < PI/4.) THEN
          GAM=1.1
        ELSEIF(ABS(CURRDIR-CORWDIR) > 1.75*PI) THEN
          IF(ABS(ABS(CURRDIR-CORWDIR)-2.*PI) < PI/4.) THEN
            GAM=1.1
          ELSE
            GAM=0.75
          ENDIF
        ELSE
          GAM=0.75
        ENDIF
        if(vecq > 0.00001) then
          IF(GAM*UBW/VECQ > 2.30) THEN
            FENH=10.
          ELSE
            FENH=EXP(GAM*UBW/VECQ)
          ENDIF
        else
          fenh=10.
        endif
        IF(FENH > 10.) FENH=10.
        IF(EFMAN > 0.) THEN
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
!IPK SEP02 END ADDITION
!
!
!nis,com,may08:
!Scale the friction factor, if it is in the Marsh-slot. The value reaches FMULT = 1,0, if the water depth directly correspondes with the
!bottom border of the transition range (ADB)
!It is linearily increased down to ADO, where it reaches the friction correction factor given by the user (range 5.0 to 20.0)
!
!
!IPK NOV97
!nis,jan09: That's the OLD way; why not used anymore?
!      IF(H < AKAPMG*BRANG) THEN
!        FRSC=ort(nr,12)**2-1.
!        FMULT=FRSC*(AKAPMG*BRANG-H)/(AKAPMG*BRANG)+1.0
!        dfmdh=-frsc/(akapmg*brang)
!      ELSE
!        FMULT=1.0
!        dfmdh=0.0
!      ENDIF
!      dffact=ffact*dfmdh+fmult*dffdh
!      FFACT=FFACT*FMULT
!
!
!ipk nov97 end changes
!nis,jan09: That's the NEW way; what are the trigonometric functions for?
!
      if(h < akapmg*brang) then
        frsc=ort(nr,12)**2-1.
        xcd=(akapmg*brang-h)/(brang*AKAPMG)*pi
        fmult=frsc/2.*(1.-cos(xcd))+1.0
        dfmdh=-frsc/2.*sin(xcd)*pi/(akapmg*brang)
      else
        fmult=1.0
        dfmdh=0.0
      endif
      dffact=FFACT*dfmdh+fmult*dffdh
      FFACT=FFACT*fmult
!
!IPK MAR01 ADD DRAG AND REORGANIZE      TFRIC = 0.0
      IF( VECQ > 1.0E-6 ) THEN
        TFRIC = FFACT / VECQ
        TDRAGX = GRAV*DRAGX(NR)/VECQ
        TDRAGY = GRAV*DRAGY(NR)/VECQ
      ELSE
        TFRIC = 0.0
        TDRAGX = 0.0
        TDRAGY = 0.0
      ENDIF
!
!ipk jun05
      IF(NR > 90 .AND. nr < 100) GO TO 291
!IPK AUG06 ADD QIN 
      QIN=0.
      IF(ICNSV == 1) THEN
        QIN=BETA3/H+(DRDX+DSDZ)+(R*DHDX+S*DHDZ)/H
!testoutput into output.out        
        WRITE (75,*) 'QIN mit BETA3 =',QIN
!-        
      ENDIF
!
!
!-------------------------------------------------
!.....Set up TERMS for the MOMENTUM EQUATIONS.....
!-------------------------------------------------
!      
!       --                                                                                  
!       |      /   du        du        du     g                                    \      
! FRN = |rho * |h*---- + hu*---- + hv*---- + ---*u*|V| + u*qs - Omega*v*h - GammaWX|      
!       |      \   dt        dx        dy    C*C                                   /      
!       --                                                                                  
!               ------   -------   -------   ---------   ----   ---------   -------      
!                  A        B         C           D        E        F          G      
!      
!                                                                                  --      
!                      da   du             da   du             da          h*h  da  |      
!           - epsXX*h*----*---- - epsXY*h*----*---- + rho*g*h*---- + rho*g*---*---- |      
!                      dx   dx             dy   dy             dx           2   dx  |      
!                                                                                  --      
!             -----------------   -----------------   ------------   --------------      
!                      H                   I                J               K      
!      
!                                                                         --      
!                            du                   du    dv              h   |               
!           + DNX *2epsXX*h*---- - DNY *epsXY*h*(---- + ----)- DNX *g*h*--  |               
!                            dx                   dy    dx              2   |              
!                                                                         --                                   
!             -----------------   --------------------------   -----------       
!                      L                        M                    N         
!  
!
!.....INITIALIZE.....
!
      FRN = 0.0
      FSN = 0.0
!
!
!.....LOCAL ACCELARATION..... (only in time transient calculations)
!
      IF (ICYC > 0) THEN
        FRN = H * BETA1
        FSN = H * BETA2
!     ---------        
!         A        
      ENDIF
!
!IPK MAR01 SET SIDF=0 FOR DRY CASE
      IF (H + AZER > ABED) THEN
        SIDFT = SIDFQ + SIDFF (NN)
        SIDFQQ = SIDFQQ + SIDFF (NN)
      ELSE
        SIDFT = SIDFF (NN)
        SIDFQQ = SIDFF (NN)
      ENDIF
!
!.....EVALUATE THE BASIC EQUATIONS WITH PRESENT VALUES.....
!
!
!.....CONVECTIVE TERMS.....
!
      FRN = FRN + H * (AKX * R * DRDX + S * DRDZ) + R*SIDFT - SIDFQ*eina
      FSN = FSN + H * (AKY * S * DSDZ + R * DSDX) + S*SIDFT - SIDFQ*einb
!           -------------------------------   --------------------      
!                          C + B                        E      
!
!.....VISCOUS TERMS.....
!
      FRNX=EPSX*H*DRDX
      FRNZ=EPSXZ*H*(DRDZ+DSDX)
      FSNX=EPSZX*H*(DRDZ+DSDX)
      FSNZ=EPSZ*H*DSDZ
!           -----------------
!                L ,   M
!
!
!.....SURFACE AND BOTTOM SLOPE (PRESSURE) TERMS.....
!
!
!-----------------------------------------
!Difference regarding sigma-transformation
!-----------------------------------------
      FRN = FRN + GHC * DAODX
      FSN = FSN + GHC * DAODZ
!           -----------      
!                J      
!------------------------------------------------
!End of difference regarding sigma-transformation
!------------------------------------------------
!
      FRNX=FRNX-H*GHC/2.
      FSNZ=FSNZ-H*GHC/2.
!           -------    
!             N    
!
!
!.....BOTTOM FRICTION TERMS.....
!
      FRN = FRN + FFACT * VECQ * R * UBF
      FSN = FSN + FFACT * VECQ * S * VBF
!           ----------------------      
!                      D      
!
!IPK MAR01   ADD DRAG TERM
!MD: Wenn in CONTROL keine 'DRG' Zeile, dann = 0
!MD: Einlesen von DRG in INPUT
      FRN = FRN + GRAV*VECQ*R*UBF*DRAGX(NR)*H      
      FSN = FSN + GRAV*VECQ*S*VBF*DRAGY(NR)*H
!
!.....CORIOLIS TERMS.....
!
      FRN = FRN - OMEGA * S * H
      FSN = FSN + OMEGA * R * H
!           -------------      
!                  F      
!-
!-..... WIND TERMS
!-
      FRN = FRN - SIGMAX
      FSN = FSN - SIGMAZ
!           ------      
!              G      
!
!.....MOTION EQUATIONS.....
!
!MD   Allocating the residual vector F(IA) with IA=1 and IA=2
!MD   with F(IA=1)=VX and F(IA=2)=VY
      MomentumEquations: Do M = 1, NCN
        IA = 1 + NDF*(M-1)
        F(IA) = F(IA) - AMS*(XN(M)*FRN + DNX(M)*FRNX + DNY(M)*FRNZ)
        IA = IA + 1
        F(IA) = F(IA) - AMS*(XN(M)*FSN + DNX(M)*FSNX + DNY(M)*FSNZ)
      enddo MomentumEquations
!--------------------------------------------------
!.....Set up TERMS for the CONTINUITY EQUATION.....
!--------------------------------------------------
!      
!       --                                            --      
!       |   / du     dv \      dh       dh     dh      |      
! FRN = |h* |---- + ----| + u*---- + v*---- + ---- - qs|      
!       |   \ dx     dy /      dx       dy     dt      |      
!       --                                            --      
!        ----------------   ------   ------   ----   ---      
!              A              B         C       D     E      
! IPK MAR01 REPLACE SIDF(NN) WITH SIDFT  
      FRN = H * (DRDX + DSDZ) + R * DHDX + S * DHDZ - SIDFT
!     -----------------   --------   --------   -----      
!              A              B          C        E      
!Term D is optional; i.e. only in unsteady cases      
      IF (ICYC > 0) FRN = FRN + BETA3
!
!.....CONTINUITY EQUATION.....
!
!       
! --                      --      
! |      n                 |      
! |     ---   T            |      
! | A *  >  (M  * M * FRN) |      
! |     ---   i    i       |      
! |     i=1                |      
! --                      --       
!       
!for all corner nodes (linear approximation)      
      ContinuityEquations: DO M=1,NCNX
        IA = 3 + 2*NDF*(M-1)
        F(IA) = F(IA) - AMW * XM (M) * FRN
      enddo ContinuityEquations
!--------------------------------------------------
!.....Set up TERMS for the SALINITY EQUATION.....
!--------------------------------------------------
  291 CONTINUE
!
!ipk jun02
      IF(ICK == 7) THEN
!MD:  ICK=7 is (till now 05.08.2008) never used
!MD:  option is preparing the residual vector F(IA=7) for
!     equilibrium method with linear functions
        FRNX=AMU*DIFX*DSALDX*H
        FRNY=AMU*DIFY*DSALDY*H
        FRN=AMU*H*(R*DSALDX+S*DSALDY)                                   &
     &   -AMU*SIDFT*(SIDQ(NN,ICK-4)-SALT)                               &
     &   -AMU*H*GAIN
        IF( ICYC > 0) FRN=FRN+AMU*DSALDT*H
        IA=-4
        DO M=1,NCNX
          IA=IA+8
!-----------------------------------------
!Difference regarding sigma-transformation
!-----------------------------------------
          F(IA)=F(IA)-(XM(M)*FRN+DMX(M)*FRNX+DMY(M)*FRNY)
!------------------------------------------------
!End of difference regarding sigma-transformation
!------------------------------------------------
        enddo
!
!MD:  preparing equations and coeffiecents for water consituents
!MD:   like salinity, sediment and temperatur
      ELSE
        FRNX=AMU*DIFX*DSALDX*H
        FRNY=AMU*DIFY*DSALDY*H
        FRN=AMU*H*(R*DSALDX+S*DSALDY)                                   &
     &   -AMU*(SIDFQQ*(SIDQ(NN,ICK-3)-SALT)+EXTL)                       &
     &   -AMU*H*(SRCSNK+(GRATE-QIN)*SALT)
!IPK AUG06 ADD QIN ABOVE
!IPK SEP02 ADD EXTL FROM SLUMP SOURCE
!IPK NOV97 ADJUST LINE ABOVE FOR SALINITY LOADING
!IPK AUG95    ADD LINE ABOVE FOR RATE TERMS
        IF(ICYC > 0) FRN=FRN+AMU*DSALDT*H
!-
!......THE SALINITY EQUATION
!-
        IA=0
        DO 295 M=1,NCN
          IA=IA+4
!MD: every 4.th entry into residual vector F(IA)          
!
          IF(NSTRT(NCON(M),1) == 0) THEN
!MD with qudratic W-Functions (not linear)
!-----------------------------------------
!Difference regarding sigma-transformation
!-----------------------------------------
            F(IA)=F(IA)-(XO(M)*FRN+DOX(M)*FRNX+DOY(M)*FRNY)
!------------------------------------------------
!End of difference regarding sigma-transformation
!------------------------------------------------
          ENDIF
  295   CONTINUE
      ENDIF
!
!MD!MD Start testoutput to output.out
!MD      IF (NN == 1 ) THEN
!MD        WRITE (75,*) 'Parameter aus COEF25nt'
!MD        WRITE (75,*) ' NN, SALT, DSALDX, DSALDY, DSALDT'
!MD        WRITE (75,*)  NN, SALT, DSALDX, DSALDY, DSALDT
!MD       ! WRITE (75,*) 'Parameter aus COEF25nt'
!MD       ! WRITE (75,*) ' SALT, DSALDX, DSALDY, DSALDT, S, R, H,
!MD       !+SIDFQQ, SIDQ(NN,ICK-3), EXTL, SRCSNK, GRATE, QIN '
!MD       END IF
!MD  Ende testoutput into output.out
!
!.....................................................
!
!
!
!ipk jun05
      IF(NR > 90 .AND. nr < 100) GO TO 380
!--------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in X-DIRECTION wrt VELOCITY.....
!--------------------------------------------------------------------------------------
!
!.....FORM THE X MOTION EQUATIONS.....
!
!.....FLOW TERMS.....
!
!
!.....INERTIAL COMPONENTS.....
!
!  N*N DU
      T1=AMS*(AKX*H*DRDX+(TFRIC+TDRAGX*H)*UBF*(2.*(R*UBF)**2+(S*VBF)**2)&
     &     +SIDFT)
!
! N*NX DU
      T2=AMS*AKX*H*R
!
! N*NY DU
      T3=AMS*H*S
!
! N*N  DV
!
      T4=AMS*(H*(DRDZ-OMEGA)+(TFRIC+TDRAGX*H)*UBF*R*S*UBF*VBF)
! NX*NX DU
      T5=AMS*H*(EPSX+C1*2.*DRDX)
! NX*NY DU
      T5A=AMS*H*C2*2.*DRDX
! NY*NY DU
      T6=AMS*H*(EPSXZ+C2*(DRDZ+DSDX))
! NY*NX DU
      T6A=AMS*H*C1*(DRDZ+DSDX)
! NY*NX DV
      T7=AMS*H*(EPSXZ+C3*(DRDZ+DSDX))
! NY*NY DV
      T7A=AMS*H*C4*(DRDZ+DSDX)
! N*NX  DV
      T8=0.
!
! N*NY  DV
      T8A=0.
!
! NX*NX DV
      T9= AMS*2.*H*DRDX*C3
! NX*NY DV
      T9A = AMS*2.*H*DRDX*C4
      IB=1-NDF
      DO 310 N=1,NCN
      IB=IB+NDF
      FEEAN=XN(N)*T1+DNX(N)*T2+DNY(N)*T3
      FEEBN=T5*DNX(N)+T5A*DNY(N)
      FEECN=T6*DNY(N)+T6A*DNX(N)
      FEEDN=XN(N)*T4+DNX(N)*T8+DNY(N)*T8A
      FEEEN=T7*DNX(N)+T7A*DNY(N)
      FEEFN=T9*DNX(N)+T9A*DNY(N)
!-
!-.....FORM THE TIME TERMS.....
!-
      IF( ICYC == 0 ) GO TO 304
      FEEAN=FEEAN+AMS*XN(N)*H*ALTM
  304 CONTINUE
      IA=1-NDF
      DO 305 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN        &
     &  + DNY(M)*FEECN
      ESTIFM(IA,IB+1) = ESTIFM(IA,IB+1) + XN(M)*FEEDN + DNY(M)*FEEEN    &
     &  + DNX(M)*FEEFN
  305 CONTINUE
  310 CONTINUE
!-----------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in X-DIRECTION wrt WATER DEPTH.....
!-----------------------------------------------------------------------------------------
!
!.....FORM THE HEAD TERMS.....
!
!  N*M DH
!
      T1=(AKX*R*DRDX+S*(DRDZ-OMEGA)+GRAV*DAODX+                         &
     &    GRAV*VECQ*R*UBF*DRAGX(NR))*AMS
!
!  NY*M DH
      T3=AMS*(DRDZ+DSDX)*EPSXZ
!
!  NX*M DH
      T4=AMS*(EPSX*DRDX-GHC)
      IB=3-2*NDF
      DO 325 N=1,NCNX
      IB=IB+2*NDF
!IPK NOV97      FEEAN=XM(N)*T1
      IF (IDNOPT >= 0) THEN
         FEEAN=XM(N)*T1
      ELSE
         FEEAN=XM(N)*T1+DMX(N)*AMS*GRAV*H*DAME(N)
      ENDIF
!IPK NOV97
        FEEBN=XM(N)*T4
        FEECN=XM(N)*T3
!-
!-.....FORM THE TIME TERMS.....
!-
        IF( ICYC <= 0 ) GO TO 317
        FEEAN=FEEAN+AMS*XM(N)*BETA1
!
  317   CONTINUE
        IA=1-NDF
        DO 320 M = 1, NCN
          IA=IA+NDF
          ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN    &
     &                  + DNY(M)*FEECN
  320   CONTINUE
  325 CONTINUE
!--------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in X-DIRECTION wrt SALINITY.....
!--------------------------------------------------------------------------------------
!-
!......FORM THE SALINITY TERMS
!-
      TAA=AMU*H**2./2.*DRDS*GRAV
!-----------------------------------------
!Difference regarding sigma-transformation
!-----------------------------------------
!MDMD:TAA=TAA - AMU*DRDS*H*(GRAV*DAODX)
      TAB=AMU*DRDS*H*(R*DRDX+S*DRDZ+GRAV*DAODX)
      IF(ICYC > 0) TAB=TAB+AMU*DRDS*H*BETA1
      IB=4-NDF
      DO 330 N=1,NCN
        IB=IB+NDF
        IF(NSTRT(NCON(N),1) == 0) THEN
          FEEAN=-XO(N)*TAA
          FEEBN=XO(N)*TAB
        ENDIF
        IA=1-NDF
        DO 329 M=1,NCN
          IA=IA+NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+DNX(M)*FEEAN+XN(M)*FEEBN
  329   CONTINUE
  330 CONTINUE
!--------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in Y-DIRECTION wrt VELOCITY.....
!--------------------------------------------------------------------------------------
!
!.....FORM THE Y MOTION EQUATIONS.....
!
!.....FLOW TERMS.....
!
! N*N DV
      T1=AMS*(AKY*H*DSDZ+(TFRIC+TDRAGY*H)*VBF*(2.*(S*VBF)**2+(R*UBF)**2)&
     &     +SIDFT)
! N*NX DV
      T2=AMS*AKY*H*R
!
! N*NY DV
      T3=AMS*H*S
!
! N*N  DU
      T4=AMS*(H*(DSDX+OMEGA)+(TFRIC+TDRAGY*H)*VBF*R*S*UBF*VBF)
!
! NX*NX DV
      T5=AMS*H*(EPSZX +C3*(DRDZ+DSDX))
!
! NX*NY DV
      T5A=AMS*H*C4*(DRDZ+DSDX)
!
! NY*NY DV
      T6=AMS*H*(EPSZ+C4*2.*DSDZ)
!
! NY*NX DV
      T6A=AMS*H*C3*2.*DSDZ
!
! N*NX DU
      T7=0.
!
! N*NY DU
      T7A=0.0
!
! NX*NX DU
      T8=AMS*H*C1*(DRDZ+DSDX)
!
! NX*NY DU
      T8A=AMS*H*(EPSZX+C2*(DRDZ+DSDX))
!
! NY*NX DU
      T9=AMS*H*2.*DSDZ*C1
!
! NY*NY DU
      T9A=AMS*H*2.*DSDZ*C2
!
      IB=1-NDF
      DO 340 N=1,NCN
        IB=IB+NDF
!
!.....INERTIAL COMPONENTS.....
!
      FEEAN=XN(N)*T1+DNX(N)*T2+DNY(N)*T3
      FEEDN=XN(N)*T4+DNX(N)*T7+DNY(N)*T7A
      FEEBN=DNX(N)*T5+DNY(N)*T5A
      FEECN=DNX(N)*T6A+DNY(N)*T6
      FEEEN=DNX(N)*T8+DNY(N)*T8A
      FEEFN=DNX(N)*T9+DNY(N)*T9A
!-
!-.....FORM THE TIME TERMS.....
!-
      IF( ICYC <= 0 ) GO TO 334
      FEEAN=FEEAN+AMS*XN(N)*ALTM*H
  334 CONTINUE
      IA=2-NDF
      DO 335 M = 1, NCN
      IA=IA+NDF
      ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEDN + DNX(M)*FEEEN        &
     &  + DNY(M)*FEEFN
      ESTIFM(IA,IB+1) = ESTIFM(IA,IB+1) + XN(M)*FEEAN + DNX(M)*FEEBN    &
     &  + DNY(M)*FEECN
  335 CONTINUE
  340 CONTINUE
!-----------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in Y-DIRECTION wrt WATER DEPTH.....
!-----------------------------------------------------------------------------------------
!
!.....HEAD TERMS.....
!
!  N*M DH
      T1=AMS*(AKY*S*DSDZ+R*(OMEGA+DSDX)+GRAV*DAODZ+                     &
     &    GRAV*VECQ*S*VBF*DRAGY(NR))
!
!  NX*M DH
      T2=AMS*EPSZX*(DRDZ+DSDX)
!
!  NY*M DH
      T4=AMS*(EPSZ*DSDZ-GHC)
!
      IB=3-2*NDF
      DO 355 N=1,NCNX
!IB=3,11        
        IB=IB+2*NDF
!-
!-.....INERTIAL COMPONENTS.....
!-
      IF (IDNOPT >= 0) THEN
        FEEAN=XM(N)*T1
      ELSE
        FEEAN=XM(N)*T1+DMY(N)*AMS*GRAV*H*DAME(N)
      ENDIF
!IPK NOV97
      FEEBN=XM(N)*T2
      FEECN=XM(N)*T4
!-
!-.....FORM THE TIME TERMS.....
!-
        IF( ICYC <= 0 ) GO TO 347
        FEEAN=FEEAN+AMS*XM(N)*BETA2
  347   CONTINUE
!IA=-2        
        IA=2-NDF
        DO 350 M = 1, NCN
!IA=2, 4          
          IA=IA+NDF
          ESTIFM(IA,IB) = ESTIFM(IA,IB) + XN(M)*FEEAN + DNX(M)*FEEBN    &
     &                  + DNY(M)*FEECN
  350   CONTINUE
  355 CONTINUE
!--------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in Y-DIRECTION wrt SALINITY.....
!--------------------------------------------------------------------------------------
!-
!......FORM THE SALINITY TERMS
!-
      TAB=AMU*DRDS*H*(R*DSDX+S*DSDZ+GRAV*DAODZ)
      IF(ICYC > 0) TAB=TAB+AMU*DRDS*H*BETA2
!IB=0      
      IB=4-NDF
      DO 359 N=1,NCN
!IB=4        
        IB=IB+NDF
        IF(NSTRT(NCON(N),1) == 0) THEN
          FEEAN=-XO(N)*TAA
          FEEBN=XO(N)*TAB
        ENDIF
!IA=-2        
        IA=2-NDF
        DO 358 M=1,NCN
!IA=2          
          IA=IA+NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+DNY(M)*FEEAN+XN(M)*FEEBN
  358   CONTINUE
  359 CONTINUE
!--------------------------------------------------------------------------------------
!.....Set up the derivatives of the MOMENTUM EQUATIONS in X-DIRECTION wrt SALINITY.....
!--------------------------------------------------------------------------------------
!
!.....FORM THE CONTINUITY EQUATIONS.....
!
      TA=AMW*H
      TX=AMW*DHDX
      TZ=AMW*DHDZ
      TB=AMW*(DRDX+DSDZ)
      TC=AMW*R
      TD=AMW*S
      IF(ICYC /= 0) TB=TB+ALTM*AMW
      IA=3-2*NDF
      DO 365 M=1,NCNX
        IA=IA+2*NDF
!
!derivative dfc/dvx und dfc/dvy        
!NB=-3        
        IB=1-NDF
        EA=XM(M)*TA
        EB=XM(M)*TX
        EC=XM(M)*TZ
        DO 360 N = 1, NCN
!IB=1          
          IB=IB+NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+EA*DNX(N)+EB*XN(N)
          ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+EA*DNY(N)+EC*XN(N)
  360   CONTINUE
!
!derivative dfc/dh        
        EA=XM(M)*TB
        EB=XM(M)*TC
        EC=XM(M)*TD
        IB=3-2*NDF
        DO 363 N=1,NCNX
         IB=IB+2*NDF
         ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*EA+DMX(N)*EB+DMY(N)*EC
  363   CONTINUE
  365 CONTINUE
!.....................................................
!
!-
!......FORM THE SALINITY EQUATION
!-
!......VELOCITY AND HEAD TERMS
!-
  380 CONTINUE
!MD:dfs/du      
      T1=AMU*H*DSALDX
!MD:dfs/dv      
      T2=AMU*H*DSALDY
      T3=AMU*DIFX*DSALDX
      T4=AMU*DIFY*DSALDY
      T5=AMU*(R*DSALDX+S*DSALDY)
      IF(ICYC > 0) then
        T5=T5+AMU*DSALDT
      ENDIF
!IA=0      
      IA=4-NDF
      DO 400 M=1,NCN
!IA=4        
        IA=IA+NDF
        IF(NSTRT(NCON(M),1) == 0) THEN
!ipk aug98
          FEEAN=XO(M)*T1
          FEEBN=XO(M)*T2
          FEEEN=(XO(M)*T5 + DOX(M)*T3+DOY(M)*T4)
        ENDIF
!IB=-3        
        IB=1-NDF
        DO 385 N=1,NCN
!IB=1, 5, 7, ...          
          IB=IB+NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+XN(N)*FEEAN
          ESTIFM(IA,IB+1)=ESTIFM(IA,IB+1)+XN(N)*FEEBN
  385   CONTINUE
!IB=-5        
        IB=3-2*NDF
        DO 390 N=1,NCNX
!IB=3,11,..          
          IB=IB+2*NDF
          ESTIFM(IA,IB)=ESTIFM(IA,IB)+XM(N)*FEEEN
  390   CONTINUE
  400 CONTINUE
!.....................................................
!
!-
!......FORM SALINITY TERMS
!-
!ipk jun02
      IF(ICK == 7) THEN
!MD:  ICK=7 is (till now 05.08.2008) never used
!MD:  option is preparing the residual vector F(IA=8) for
!     equilibrium method with linear functions
!
        T1=-AMU*H
        IA=-4
        DO  M=1,NCNX
          IA=IA+8
!-----------------------------------------
!Difference regarding sigma-transformation
!-----------------------------------------
          FEEAN=XM(M)*T1
!------------------------------------------------
!End of difference regarding sigma-transformation
!------------------------------------------------
          IB=-4
          DO N=1,NCNX
            IB=IB+8
            ESTIFM(IA,IB)=ESTIFM(IA,IB)+FEEAN*XM(N)
          ENDDO
        ENDDO
!
!MD:  preparing equations and coeffiecents for water consituents
!MD:   like salinity, sediment and temperatur
      ELSE
!IPK NOV97 REWRITE FOR NEW UNITS OF SIDF      T1=-AMU*H*(SIDF(NN)+GRATE)
! IPK MAR01 REPLACE SIDF(NN) WITH SIDFT
!IPK MAY04 USE SIDFQQ
        T1=-AMU*((GRATE-QIN)*H - SIDFQQ)
!IPK AUG06 ADD QIN TO THE ABOVE      
        IF(ICYC > 0) T1= T1 + AMU*ALTM*H
        T2=AMU*DIFX*H
        T3=AMU*DIFY*H
        T5=AMU*R*H
        T6=AMU*S*H
        IA=0
        DO 420 M=1,NCN
          IA=IA+4
          IF(NSTRT(NCON(M),1) == 0) THEN
!ipk aug98
!-----------------------------------------
!Difference regarding sigma-transformation
!-----------------------------------------
            FEEAN=XO(M)*T1
            FEEBN=(DOX(M)*T2+XO(M)*T5)
            FEECN=(DOY(M)*T3+XO(M)*T6)
!------------------------------------------------
!End of difference regarding sigma-transformation
!------------------------------------------------
          ENDIF
          IB=0
          DO 410 N=1,NCN
            IB=IB+4
            IF(NSTRT(NCON(N),1) == 0) THEN
!MDMD: new: Korrektur nach S. A-7
!MDMD         ESTIFM(IA,IB)=ESTIFM(IA,IB) +FEEAN*XO(N)+FEEBN*DOX(N)+FEECN*DOY(N)
              ESTIFM(IA,IB)=ESTIFM(IA,IB)                               &
     &                     +FEEAN*XO(N)+FEEBN*DOX(N)+FEECN*DOY(N)
            ENDIF
  410     CONTINUE
  420   CONTINUE
      ENDIF
!-
!......END GAUSS DO LOOP
!-
  500 CONTINUE
      IF(NTX == 0) RETURN
!
!ipk jun05
      IF(NR > 90 .AND. nr < 100) GO TO 660
!       COMPUTE BOUNDARY FORCES
!
!2D -> 1D (h-Q): TransLines (i, 4) = 1      
!2D <- 1D (Q-h): TransLines (i, 4) = 2      
!2D <> 1D (h-h): TransLines (i, 4) = 3      
!
! with i == no. of connected Transition Line      
      TransLine = TransLinePart (nn)
!
      if (TransLine == 0) then
        transtype = 0
      else
        transtype = TransLines (TransLine, 4)
      endif
!
!Following part of the source code settles the boundary hydrostatic forces,      
!  either to active h-Boundary conditions or to shoreline boundaries, as well as      
!  the shoreline friction values, if there was a fricition coefficient given.      
      BoundaryForces: DO L=1,NCN,2
!midside node of the current arc        
        N2=NCON(L+1)
!
!Check the current arc for being a boundary arc        
!IPK JUN05        IF(IBN(N2) /= 1) GO TO 650
        IF (IBN (N2) /= 1 .AND. IBN (N2) /= 10 .AND.                    &
     &      IBN (N2) /= 11 .AND. IBN (N2) /= 21 .AND.                   &
!nis,feb08: for transition, has to be checked further, se below            
     &      IBN (n2) /= 2)                                              &
     &    CYCLE BoundaryForces
!
!Force only TransitionMember-nodes with transtype == 1 and transtype == 3 to be        
        if (IBN (N2) == 2) then
!all ibn == 2 - nodes are potentially transition nodes; consider only the TransitionMember - nodes          
          if ( .NOT. TransitionMember (N2)) CYCLE BoundaryForces
!
!from the TransitionMember - nodes only types 1 and 3 are considerable          
          if (( .NOT. transtype == 1) .AND. ( .NOT. transtype == 3))    &
     &      CYCLE BoundaryForces
!
        end if
!
!The three nodes of the current boundary arc        
        N1=NCON(L)
        NA=MOD(L+2,NCN)
        N3=NCON(NA)
!The equation numbers of the corner nodes of the current boundary arc        
        NC1=(L-1)*NDF+3
        NC2=(NA-1)*NDF+3
!water depth of the current corner nodes         
        H1=VEL(3,N1)
        H3=VEL(3,N3)
!The length of the current arc        
        DL(1,2)=(CORD(N2,1)-CORD(N1,1))*CX+(CORD(N2,2)-CORD(N1,2))*SA
        DL(1,1)=-(CORD(N2,1)-CORD(N1,1))*SA+(CORD(N2,2)-CORD(N1,2))*CX
        DL(2,2)=(CORD(N3,1)-CORD(N1,1))*CX+(CORD(N3,2)-CORD(N1,2))*SA
        DL(2,1)=-(CORD(N3,1)-CORD(N1,1))*SA+(CORD(N3,2)-CORD(N1,2))*CX
!Finding a direction factor (1.0 or -1.0)        
        IF(DL(2,2) < 0.) THEN
          FTF(1)=1.0
        ELSE
          FTF(1)=-1.0
        ENDIF
        IF(DL(2,1) < 0.) THEN
          FTF(2)=1.0
        ELSE
          FTF(2)=-1.0
        ENDIF
        IF(MOD(NFIX(N2)/100,10) == 2) THEN
          IHD=1
!Consider transition nodes (type 3) as active boundaries        
        ELSEIF (TransitionMember (n2)) THEN
          IHD = 1
!In all other cases they are passive boundaries, so that they will not get a        
!  user/transition specified boundary condition force        
        ELSE
          IHD=0
        ENDIF
!
!Run through momentum equations of a node and integrate the boundary forces        
!  over the length of the arc using a linear interpolation integrated with GAUSS        
        DO 600 M=1,2
!Run through the GAUSS nodes on a linear line (boundary arc)          
          DO 580 N=1,4
!get the density at the GAUSS point            
            RHO=DEN(N1)+AFACT(N)*(DEN(N3)-DEN(N1))
!get the water depth at the GAUSS point            
            H=H1+AFACT(N)*(H3-H1)
!Calculate the bottom elevation, considering the Marsh slot if the option            
!  is operative, otherwise the bottom elevation is ao.            
            IF(IDNOPT < 0) THEN
              AZER = AME((L+1)/2)+ADO(N1)  +                            &
     &             AFACT(N)*(AME((NA+1)/2)+ADO(N3)-AME((L+1)/2)-ADO(N1))
            ELSE
              AZER=AO(N1)+AFACT(N)*(AO(N3)-AO(N1))
            ENDIF
!
!Calculate the velocities using a distribution function if specified            
!  purpose is the calculation of the shoreline friction            
            UU=                                                         &
     &      XNAL(1,N)*VEL(1,N1)/UDST(N1)+XNAL(2,N)*VEL(1,N2)/UDST(N2)   &
     &     +XNAL(3,N)*VEL(1,N3)/UDST(N3)
            VV=                                                         &
     &      XNAL(1,N)*VEL(2,N1)/VDST(N1)+XNAL(2,N)*VEL(2,N2)/VDST(N2)   &
     &     +XNAL(3,N)*VEL(2,N3)/VDST(N3)
!Turn the velocities onto direction fixes            
            U= UU*CX+VV*SA
            V=-UU*SA+VV*CX
!
!.....Compute shore friction.....
!
!MAY93 ENDCHANGE
!get total velocity            
            VECQ=SQRT(U**2+V**2)
!Chezy            
            IF (ORT (NR, 11) > 1.) THEN
              FFACT = 1./ ORT (NR, 11)**2 * H
!Manning's N            
            ELSEif (ort(nr,11) > 0.0d0 .AND. ort(nr,11) < 1.0d0 ) then
              FFACT = ORT (NR, 11)**2 * FCOEF * H**(2.0d0/3.0d0) / GRAV
!ipk mar99 fix bug for bank friction (double count on GRAV)
!Darcy-Weisbach            
            elseif (ort(nr,11) < 0.0d0 .AND. abs(vecq) > 0.001d0) then
              lambda_shore = 0.0
              lambdaKS_shore = 0.0d0
              lambdaP_shore = 0.0d0
              lambdaDunes_shore = 0.0d0
              cwr_temp = 0.0d0
              call darcy (lambda_shore, vecq, h,                        &
     &          abs(ort(nr, 11)),                                       &
     &          0.0d0,                                                  &
     &          0.0d0,                                                  &
     &          nn, morph, gl_bedform, MaxE, cwr_temp, 2,               &
!store values for output                
     &          lambdaKS_shore,                                         &
     &          lambdaP_shore,                                          &
     &          lambdaDunes_shore, dset)
!
              FFACT = lambda_shore/ 8.0d0/ grav * h
!no shoreline friction            
            else
              FFACT = 0.0d0
            ENDIF
!
!... compute hydrostatic forces ...
!            
!                                        1             
!temp = (w1 * b1 + w2 * b2) * rho * g * ---            
!                                        2            
!w1, w2 = weighting factors            
!b1, b2 = arc segment widths            
!            
            TEMP=(DNAL(2,N)*DL(1,M)+DNAL(3,N)*DL(2,M))*GRAV/2.*RHO
!                                      1     2    1            
!hp = (w1 * b1 + w2 * b2) * rho * g * --- * h  * --- * f1            
!                                      2          2            
!f1 = linear function for interpolation.            
!            
!TODO            
!This is strange, because the factor 1/4 should be only 1/2. Only reason            
!might be the extension of the unit width for the weighting functions with             
!fator 2.            
            HP=TEMP*HFACT(N)*H**2/2.
!                                       1            
!hp1 = (w1 * b1 + w2 * b2) * rho * g * --- * h * f1            
!                                       2            
            HP1=TEMP   *H*HFACT(N)
!                                        1            
!DERR = (w1 * b1 + w2 * b2) * rho * g * --- * h * f1 * (a1 * (hsoll1 - hist1) + (1 - a1) * (hsoll2-hist2)            
!                                        2            
            DERR=SLOAD(M)*HP1*(AFACT(N)*(SPEC(N3,3)-VEL(3,N3))          &
     &          +(1.-AFACT(N))*(SPEC(N1,3)-VEL(3,N1)))
!
!get the friciton             
            IF(M == 2) THEN
              TFRIC=TEMP*FFACT*FTF(1)*HFACT(N)
              FFACT=TFRIC*U*VECQ
              IF(VECQ > 0.001) THEN
                FDU=(2.*U**2+V**2)/VECQ*TFRIC
                FDV=U**2/VECQ*TFRIC
              ELSE
                FDU=0.
                FDV=0.
              ENDIF
            ELSE
              TFRIC=TEMP*FFACT*FTF(2)*HFACT(N)
              FFACT=TFRIC*V*VECQ
              IF(VECQ > 0.001) THEN
                FDU=V**2/VECQ*TFRIC
                FDV=(2.*V**2+U**2)/VECQ*TFRIC
              ELSE
                FDU=0.
                FDV=0.
              ENDIF
            ENDIF
!Modify the estifm and f vectors/matrices            
!  for the influence of all 3 nodes of the current boundary arc            
            DO 575 K=1,3
!get the equaiton number                            
              MA=MOD((L+K-2)*NDF+M,NEF)
              MA1=MA+3-2*M
!Modify the equation               
              F(MA)=F(MA)+HP*XNAL(K,N)*SLOAD(M)
!
!Modify the equations for passive boundaries              
              IF(IHD == 0) THEN
!Hydrostatic side pressure                
                ESTIFM(MA,NC1)=ESTIFM(MA,NC1)                           &
     &                         -SLOAD(M)*(1.-AFACT(N))*XNAL(K,N)*HP1
!
                ESTIFM(MA,NC2)=ESTIFM(MA,NC2)                           &
     &                         -SLOAD(M)*AFACT(N)*XNAL(K,N)*HP1
!
!shoreline friction (FDU or FDV)                
                F(MA1)=F(MA1)+XNAL(K,N)*FFACT
!
                ESTIFM(MA1,NC1)=ESTIFM(MA1,NC1)                         &
     &                         -XNAL(K,N)*FFACT/H*(1.-AFACT(N))
!
                ESTIFM(MA1,NC2)=ESTIFM(MA1,NC2)                         &
     &                         -XNAL(K,N)*FFACT/H*AFACT(N)
!
                ESTIFM(MA1,NC1-2)=ESTIFM(MA1,NC1-2)                     &
     &                         -FDU*XNAL(K,N)*XNAL(1,N)
!
                ESTIFM(MA1,NC1-1)=ESTIFM(MA1,NC1-1)                     &
     &                         -FDV*XNAL(K,N)*XNAL(1,N)
!
                ESTIFM(MA1,NC1+NDF-2)=ESTIFM(MA1,NC1+NDF-2)             &
     &                         -FDU*XNAL(K,N)*XNAL(2,N)
!
                ESTIFM(MA1,NC1+NDF-1)=ESTIFM(MA1,NC1+NDF-1)             &
     &                         -FDV*XNAL(K,N)*XNAL(2,N)
!
                ESTIFM(MA1,NC2-2)=ESTIFM(MA1,NC2-2)                     &
     &                         -FDU*XNAL(K,N)*XNAL(3,N)
!
                ESTIFM(MA1,NC2-1)=ESTIFM(MA1,NC2-1)                     &
     &                         -FDV*XNAL(K,N)*XNAL(3,N)
!Modify the equaitons for active boundaries or transitions              
              ELSE
                F(MA)=F(MA)+DERR*XNAL(K,N)
              ENDIF
  575       CONTINUE
  580     CONTINUE
  600   CONTINUE
      ENDDO BoundaryForces
  660 CONTINUE
!-
!- APPLY TRANSFORMATIONS TO STIFFNESS AND FORCE MATRICES FOR SLOPING B. C.
!-
      DO 1000 N=1,NCN
        N1=NCON(N)
        AFA=ALFA(N1)-THNN -ADIF(N1)
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
!
!      test for control structure
!
      IF(IMAT(NN) < 904 .OR. imat(nn) > 1000) THEN
!
        DO L=1,NCN
          N2=NOP(NN,L)
          IF(IBN(N2) >= 10 .AND. ISUBM(N2) == 0) THEN
            NA=(L-1)*NDF+1
            DO  KK=1,NEF
              ESTIFM(NA,KK)=0.
            ENDDO
            F(NA)=0.
          ENDIF
        ENDDO
      ENDIF
!
!
!ipk dec97 apply scale factors for elevtaion bc's if necessary
!-
!...... Apply scale factors to velocities for special boundaries
!-
      DO 1030 N=1,NCN
!ipk nov95 remove iabs
        M=NOP(NN,N)
        IF(VSCALE(M) /= 0.) THEN
          NEQ=NDF*NCN
          IA=NDF*(N-1)+1
          DO 1025 I=1,NEQ
            ESTIFM(I,IA)=ESTIFM(I,IA)*VSCALE(M)
 1025     CONTINUE
        ENDIF
 1030 CONTINUE
!ipk dec97 end changes
!-
!...... For 1D - 2D junctions adjust equation for direction
!-
!run through corner nodes      
      DO 1050 N=1,NCN,2
!get node number        
        M=NCON(N)
!check for adif being not equal to zero        
        IF(ADIF(M) /= 0.) THEN
          NEQ=NDF*NCN
          IA=NDF*(N-1)+1
!
          DO 1040 I=1,NEQ
!Project the equations onto the fixed direction            
            ESTIFM(I,IA)=ESTIFM(I,IA)+ESTIFM(I,IA+1)*SIN(ADIF(M))       &
     &                   /COS(ADIF(M))
 1040     CONTINUE
!
        ENDIF
 1050 CONTINUE
!
!
      IF(NR <= 90) then
!-
!......INSERT EXPERIMENTAL UPSTREAM BOUNDARY FLOWS
!-
      throughnodes: DO N=1,NCN
!current node number at position n in current element nn        
        M = NCON (N)
!
!-
!...... Test for and then retrieve stage flow constants
!-
        IF (ISTLIN (M) /= 0) THEN
!line number to apply h-Q-relationship as BC          
          J = ISTLIN (M)
!coefficients/ parameters of h-Q-relationship equation          
          AC1 = STQ (J)
          AC2 = STQA (J)
          E0  = STQE (J)
          CP  = STQC (J)
!cross sectional area depending on actual water depth, generated in AGEN.sub          
          ASC = ALN (J)
        ELSE
!if there was no h-Q-relationship in form of a formula, then fix AC2 to 0.0, because this is used as a checker          
          AC2 = 0.
        ENDIF
!
!nis,jul07: Write 1D-2D-line-Transition values to equation system      
!if (TransitionMember(M)) then      
!      
!  !get momentum equation of 2D-node at transition      
!  IRW = NDF * (N - 1) + 1      
!  IRH = NDF * (N - 1) + 3      
!      
!  !calculate absolute velocity      
!  VX  = VEL (1,M) * COS (ALFA (M)) + VEL (2,M) * SIN (ALFA (M))      
!      
!  !reset Jacobian      
!  do j = 1, nef      
!    estifm(irw, j) = 0.0      
!  enddo      
!      
!  !form specific discharge values like inner boundary condition      
!  F (IRW)           = spec(M, 1) - vx * vel(3, M)      
!  ESTIFM (IRW, IRW) = vel(3, M)  - dspecdv(M)      
!  ESTIFM (IRW, IRH) = vx         - dspecdh(M)      
!end if      
!-      
!
      NFX=NFIX(M)/1000
      IF(NFX < 13) cycle throughnodes
      IRW=NDF*(N-1)+1
      IF(NFX == 13) IRW=IRW+1
      IRH=NDF*(N-1)+3
      VX=VEL(1,M)*COS(ALFA(M))+VEL(2,M)*SIN(ALFA(M))
      DO 1200 J=1,NEF
 1200 ESTIFM(IRW,J)=0.
!
      IF(MOD(N,2) == 0) GO TO 1250
!nis,com: AC2 == 0, if there was no h-Q-relationship, but just a water level BC      
      IF(AC2 == 0.) THEN
        ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)
        ESTIFM(IRW,IRH)=AREA(NN)*VX
        F(IRW)=AREA(NN)*(SPEC(M,1)-VX*VEL(3,M))
!EFa aug07, stage-flow boundaries (table)        
        if (istab(m) > 0.) then
          af = vel(3,m) / asc
          if (spec(m,1) < 0.) then
            adir = -1.
          else
            adir = 1.
          end if
!
!calculate surface elevation          
          if (idnopt < 0) then
            srfel = hel(m) + ado(m)
          else
            srfel = vel (3, m) + ao(m)
          end if
!
          call stfltab(m,srfel,dfdh,ff,1)
          f(irw) = area(nn) * (af * adir * ff - vx * vel(3,m))
          estifm(irw,irw) = area(nn) * vel(3,m)
          estifm(irw,irh) = area(nn) * (vx - af * adir * dfdh)
        end if
!-        
      ELSE
        AF=VEL(3,M)/ASC
!IPK NOV97    F(IRW)=AREA(NN)*(AF*(AC1+AC2*(VEL(3,M)+AO(M)-E0)**CP)-VX
!IPK NOV97     1         *VEL(3,M))
        IF (IDNOPT >= 0) THEN
          WSEL=VEL(3,M)+AO(M)
        ELSE
          HM = VEL(3,M)
          CALL AMF(HS,HM,AKP(M),ADT(M),ADB(M),AMEL,DUM2,0)
          WSEL = HS+ADO(M)
        ENDIF
        F(IRW)=AREA(NN)*(AF*(AC1+AC2*(WSEL-E0)**CP)-VX                  &
     &            *VEL(3,M))
!IPK NOV97
        ESTIFM(IRW,IRW)=AREA(NN)*VEL(3,M)
!IPK NOV97        ESTIFM(IRW,IRH)=AREA(NN)*(VX-AF*AC2*CP*(VEL(3,M)+AO(M)-E0)**
!IPK NOV97     1                  (CP-1.0))
        ESTIFM(IRW,IRH)=AREA(NN)*(VX-AF*AC2*CP*(WSEL-E0)                &
     &                     **(CP-1.0))
!IPK NOV97
      ENDIF
      cycle throughnodes
 1250 N1=NCON(N-1)
      N2=MOD(N+1,NCN)
      N3=NCON(N2)
      HM=(VEL(3,N1)+VEL(3,N3))/2.
      IRI=(N2-1)*NDF+3
      IF(AC2 == 0.) THEN
        ESTIFM(IRW,IRW)=AREA(NN)*HM
        ESTIFM(IRW,IRH-NDF)=AREA(NN)*VX/2.
        ESTIFM(IRW,IRI)=AREA(NN)*VX/2.
        F(IRW)=AREA(NN)*(SPEC(M,1)-VX*HM)
!EFa aug07, stage-flow boundaries (table)        
        if (istab(m) > 0.) then
          af = vel(3,m) / asc
          if (spec(m,1) < 0.) then
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
!IPK NOV97        F(IRW)=AREA(NN)*(AF*(AC1+AC2*(HM+AO(M)-E0)**CP)-VX*HM)
        IF (IDNOPT >= 0) THEN
          AOL=AO(M)
        ELSE
          CALL AMF(HS,HM,AKP(M),ADT(M),ADB(M),AMEL,DUM2,0)
          AOL = ADO(M) +HS
        ENDIF
        F(IRW)=AREA(NN)*(AF*(AC1+AC2*(HM+AOL-E0)**CP)-VX*HM)
!IPK NOV97
        ESTIFM(IRW,IRW)=AREA(NN)*HM
!IPK NOV97        ESTIFM(IRW,IRH-NDF)=AREA(NN)/2.*(VX-AF*AC2*CP*(HM+AO(M)-E0)**
!IPK NOV97     1                    (CP-1.0))
        ESTIFM(IRW,IRH-NDF)=AREA(NN)/2.*(VX-AF*AC2*CP*                  &
     &                         (HM+AOL-E0)**(CP-1.0))
!IPK NOV97
        ESTIFM(IRW,IRI)=ESTIFM(IRW,IRH-NDF)
        ENDIF
      ENDDO throughnodes
!
      ENDIF
!
!IPK JUN05
 1320 CONTINUE
!
      IF(IDNOPT < 0) THEN
        DO N=1,NEF
          DO M=1,NCNX
            MM=(M-1)*NDF*2+3
            ESTIFM(N,MM)=ESTIFM(N,MM)*EFPORNN(M)
          ENDDO
        ENDDO
      ENDIF
!
!Write local residual vector into global matrix      
      DO 1450 I=1,NCN
        J=NCON(I)
        IA=NDF*(I-1)
        DO 1400 K=1,NDF
          IA=IA+1
          JA=NBC(J,K)
          IF(JA > 0) THEN
            R1(JA)=R1(JA)+F(IA)
          ENDIF
 1400   CONTINUE
 1450 CONTINUE
!
!
!write matrix into file as control output
!      if (TransitionElement (nn))
!     +  call Write2DMatrix(nbc, nop, estifm, f, maxp, maxe, nn, ncn)
!-
!
      RETURN
!
!IPK JUN05
 2000 CONTINUE
      IF(IMAT(NN) == 990) RETURN
!-
!...... Special cases for control structures or junction sources
!-
      IF(IMAT(NN) > 903) THEN
        CALL CSTRC2D(NN)
        GO TO 1320
      ENDIF
      RETURN
      END
