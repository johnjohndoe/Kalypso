CIPK  LAST UPDATE SEP 05 2006 ADD DEPRATO AND TO TMD
CIPK  LAST UPDATE APR 05 2006 ADD IPASST ALLOCATION
CIPK  LAST UPDATE MAR 22 2006 FIX NCQOBS BUG
cipk  last update mar 07 2006 add call to ginpt for limits option
CIPK  LAST UPDATE MAR 05 2006 ADD ALLOCATION TO USE NODAL PROPS
CIPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
CIPK  LAST UPDATE SEP 04 2002 ADD WAVE DATA INITIALIZATION
CIPK  LAST UPDATE MAY 28 2002 ADD STRESS INITIALIZATION
CIPK  LAST UPDATE SEP 30 2002 ADD ICETHK,ICETHKOL
CIPK  LAST UPDATE JAN 22 2002  ADD SIDFF TO INITIALIZATION
CIPK  LAST UPDATE MAR 18 2001   ADD FOR POWER STATION RECYCLING
CIPK  LAST UPDATE MAR 13 2001 ADD TIME STEP TRANSITION OPTION
CIPK  LAST UPDATE mARCH 2 2001 ADD MANNING 'N' FUNCTIONS
      SUBROUTINE INITL
      
      USE BLK10
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
      USE BLKSSTMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKMETMOD
      USE BLKCDMOD
      USE BLKABMOD
      USE PARAMMOD
!NiS,apr06: add module for Kalypso-specific calculations
      USE PARAKalyps
      !EFa Nov06, neues Modul für Teschke-1D-Elemente
      USE PARA1DPoly
      use PardisoParams
!-


C	INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05	INCLUDE 'BLKDR.COM'
CIPK AUG05	INCLUDE 'BLKSST.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK JUN05
CIPK AUG05      INCLUDE 'BLKSUB.COM'

      DATA VOID/-1.E20/
      
      integer (kind = 4) :: mfww

c     Initialisation of values

      !NBS = 5000000
      NBS = 20000000
      MFW=1000
      NBSS=NBS
      LBMAX=NBSS
      MFWW=MFW
      !Band width of the system
      MFWSIZ    =5000
      !size of the right-hand-side vector, i.e. the number of active equations
      MR1SIZ   = 500000
      !number of entries in the Jacobian Matrix, i.e. all non-zero derivatives of the Jacobian
      NBUFFSIZ = 50000000


      MNPP=MAXP
      NLAYMX=1
      MXSEDLAY=10
      MSAND=1
      MMAT1=20
      NQLDS =20
      NCQOBS = 2500
      NHDS =5
      NCHOBS = 3500
      NELDS = 250
      NDPTS = 2600
  200 CONTINUE
      call ginpt(Lin,id,dlin)
      IF(ID(1:8) .EQ. 'ENDLIMIT') THEN
cipk mar06  add call for ENDLIMIT case
      call ginpt(Lin,id,dlin)
        GO TO 250
      ELSEIF(ID(1:2) .EQ. 'TI') THEN
        GO TO 250
      ELSEIF(ID(1:7) .EQ. 'MAXNODE') THEN
        READ(DLIN,'(I8)') MAXP
      ELSEIF(ID(1:6) .EQ. 'MAXELT') THEN
        READ(DLIN,'(I8)') MAXE
      ELSEIF(ID(1:8) .EQ. 'MAXFRONT') THEN
        READ(DLIN, *) MFWW
      ELSEIF(ID(1:8) .EQ. 'MAXLAYER') THEN
        READ(DLIN,'(I8)') NLAYMX
      ELSEIF(ID(1:8) .EQ. 'MXSEDLAY') THEN
        READ(DLIN,'(I8)') MXSEDLAY
      ELSEIF(ID(1:8) .EQ. 'BUFFSIZ ') THEN
        READ(DLIN,'(I8)') NBSS
        LBMAX=NBSS
        nbuffsiz = lbmax
      ELSEIF(ID(1:8) .EQ. 'BUFFSIZL') THEN
        READ(DLIN,'(I16)') NBSS
        LBMAX=NBSS
        nbuffsiz = lbmax
      ELSEIF(ID(1:8) .EQ. 'MAXQPTS ') THEN
CIPK MAR06
        READ(DLIN,'(I8)') NCQOBS
      ELSEIF(ID(1:8) .EQ. 'MAXQINPT') THEN
        READ(DLIN,'(I8)') NQLDS
      ELSEIF(ID(1:8) .EQ. 'MAXHPTS ') THEN
        READ(DLIN,'(I8)') NCHOBS
      ELSEIF(ID(1:8) .EQ. 'MAXHINPT') THEN
        READ(DLIN,'(I8)') NHDS
      ELSEIF(ID(1:8) .EQ. 'MAXELPTS') THEN
        READ(DLIN,'(I8)') NDPTS
      ELSEIF(ID(1:8) .EQ. 'MAXEINPT') THEN
        READ(DLIN,'(I8)') NELDS
C      ELSEIF(ID(1:8) .EQ. 'MAXPBUFR') THEN
C        READ(DLIN,'(I8)') MPB
C      ELSEIF(ID(1:8) .EQ. 'MAXSTEPS') THEN
C        READ(DLIN,'(I8)') MAXSTP
      ENDIF
      GO TO 200
  250 CONTINUE
      WRITE(LOUT,6012) MAXP
      WRITE(LOUT,6013) MAXE
      WRITE(LOUT,6000) MFWW
      WRITE(LOUT,6001) NBSS
      WRITE(LOUT,6014) NLAYMX
      WRITE(LOUT,6015) MXSEDLAY
CIPKMAR06
      WRITE(LOUT,6004) NCQOBS
      WRITE(LOUT,6005) NQLDS
      WRITE(LOUT,6006) NCHOBS
      WRITE(LOUT,6007) NHDS
      WRITE(LOUT,6008) NDPTS
      WRITE(LOUT,6009) NELDS
C      WRITE(LOUT,6010) MPB
C      WRITE(LOUT,6011) MAXSTP
 6000 FORMAT(' MAXIMUM FRONT WIDTH SET TO                    ',I8)
 6001 FORMAT(' MAXIMUM BUFFER SIZE SET TO            ',I16)
 6004 FORMAT(' MAXIMUM POINTS PER INFLOW HYDROGRAPH SET TO   ',I8)
 6005 FORMAT(' MAXIMUM INFLOW HYDROGRAPHS SET TO             ',I8)
 6006 FORMAT(' MAXIMUM POINTS PER INFLOW TIDALGRAPH SET TO   ',I8)
 6007 FORMAT(' MAXIMUM INFLOW TIDALGRAPHS SET TO             ',I8)
 6008 FORMAT(' MAXIMUM POINTS PER ELEMENT INFLOW GRAPH SET TO',I8)
 6009 FORMAT(' MAXIMUM INFLOW ELEMENT GRAPHS SET TO          ',I8)
C 6010 FORMAT(' MAXIMUM PRINT BUFFER SET TO                   ',I8)
C 6011 FORMAT(' MAXIMUM TIME STEPS SET TO                     ',I8)
 6012 FORMAT(/' MAXIMUM NUMBER OF NODES SET TO                ',I8)
 6013 FORMAT(' MAXIMUM NUMBER OF ELEMENTS SET TO             ',I8)
 6014 FORMAT(' MAXIMUM NUMBER OF LAYERS   SET TO             ',I8)
 6015 FORMAT(' MAXIMUM NUMBER OF SED LAYERS  SET TO          ',I8)
      MFW=MFWW
      mfwsiz = mfww
      NBS=NBSS

      ALLOCATE (EQ(MFWW,MFWW),LHED(MFWW),QQ(MFWW),PVKOL(MFWW)
     + ,LDEST(MFWW),QR(MFWW))


      ALLOCATE (LHS(NBS),QS(NBS))

      ALLOCATE (CORD(MAXP,3),VEL(7,MAXP),AO(MAXP),AORIG(MAXP))
      
      ALLOCATE (CORDS(MAXP,3),NBC(MAXP,7),SPEC(MAXP,7),ITAB(MAXP),
     1              ALFA(MAXP),VOLD(7,MAXP),
     2              VDOT(7,MAXP),VDOTO(7,MAXP),
     3              NFIX(MAXP),NLOC(MAXP),NDEP(MAXP),
     4             NSURF(MAXP),NBTN(MAXP),IBN(MAXP),NFIX1(MAXP)
     5             ,FCTV(MAXP),FCTS(MAXP),NREF(MAXP),NTHREE(MAXP)
     6             ,VVEL(MAXP),FC(MAXP,3)
!nis,may07
!Add midside node for polynom approach
!!nis,feb07: Changing the sizes of the arrays; purpose: direction setting for Flow1DFE elements
     7             ,ADIF(MAXP),XSLP(MAXP),YSLP(MAXP),NFIXK(MAXP)
!!     7             ,ADIF(MAXP), XSLP(minNoNu:MAXP)
!!     +             ,YSLP(minNoNu:MAXP),NFIXK(minNoNu:MAXP)
!!-
     8             ,WIDTH(MAXP),SS1(MAXP),SS2(MAXP),NFIXP(MAXP)
!!nis,feb07: Changing the sizes of the arrays; purpose: direction setting for Flow1DFE elements
     +             ,ALFAK(MAXP),VOUTN(MAXP),WIDS(MAXP)
!!     +             ,ALFAK(MAXP),VOUTN(minNoNu:MAXP),WIDS(MAXP)
!!-
!Add midside node for polynom approach
!-
     +             ,DIR(MAXP)
     +             ,PRESS(MAXP),DEN(MAXP),NODC(MAXP),NSTRT(MAXP,2)
     +           ,NDROP(MAXP,4),ICPON(MAXP),NSPL(MAXP)
     +           ,DROXIN(MAXP),DRODXN(MAXP),DROYIN(MAXP),DRODYN(MAXP)
     +           ,irbd(MAXP),nfixsav(MAXP)
     +           ,IESPC(7,MAXP)
     +           ,iactv(MAXP,7),HEL(MAXP),HOL(MAXP),HDET(MAXP)
     +           ,HDOT(MAXP)
     +           ,jpoint(MAXP),SIDN(MAXP),XTLN(MAXP)
     +           ,widbs(MAXP),wss(MAXP),ICOLLAP(MAXP),WSLL(MAXP)
     +           ,V2OL(7,MAXP),H2OL(MAXP),NBCKP(MAXP,7),NFIXSV1(MAXP)
     +           ,WAT(MAXP,3),NODWT(MAXP,3)
     +           ,GAN(MAXP),GAN0(MAXP),DGN(MAXP),IBNA(MAXP)
     +           ,XNU(MAXP),XNMANN(MAXP),ISTLIN(MAXP)
     +           ,VSCALE(MAXP),DDHDX(MAXP),DDHDY(MAXP),DDAODX(MAXP)
     1           ,DDAODY(MAXP),DIVID(MAXP),XVEL(10,MAXP),LAB(MAXP)
     +           ,EVISXZ(MAXP),EVISYZ(MAXP),DVISZ(MAXP)
     +           ,UDST(MAXP),VDST(MAXP),UUDST(MAXP),UVDST(MAXP)
     +           ,VVDST(MAXP),SDST(MAXP),TDST(MAXP),SEDST(MAXP)
     +           ,IMID(MAXP,2),IPASST(MAXP),IOVLDN(MAXP))
CIPK sep06 ADD IOVLDN (currently not used)
CIPK APR06

      ALLOCATE   (NDRY(MAXP),NDRYO(MAXP),IPT(MAXP),DREC(MAXP),
     +               IMATO(MAXE),LISTEL(MAXE)
     +               ,ADB(MAXP),ADT(MAXP),ADO(MAXP),AKP(MAXP)
     +               ,DAME(MAXP),AME(MAXP),ACOR(MAXP),ADOKP(MAXP)
     +               ,AOKP(MAXP))

      ALLOCATE   (IWFLOW(MAXP),WFLOW(MAXP),WHGT(MAXP),WLEN(MAXP)
     +             ,wflolw(MAXP),TRANSEL(MAXP),ISUBM(MAXP),ISUBMEL(MAXE)
     +             ,NFCTP(MAXE))

      ALLOCATE   (STRESS(MAXP,2),SPWP(MAXP),AWL(MAXP),SWH(MAXP)
     +                ,WVDR(MAXP),STR1(MAXP,6),STR2(MAXP,6)
     +                ,STR10(MAXP),STR20(MAXP)
     +                ,STR11(MAXP),STR21(MAXP))

!NiS,apr06: increase the number of element characteristics array ORT(:,:)
!      ALLOCATE   (NOP(MAXE,20),IMAT(MAXE),ORT(MAXE,14),NFIXH(MAXE)
!      ALLOCATE    (NOP(MAXE,20),IMAT(MAXE),ORT(MAXE,17),NFIXH(MAXE)
      ALLOCATE    (NOP (MAXE, 20), IMAT (MAXE), NFIXH (MAXE)
!-
     3             ,TH(MAXE),AREA(MAXE),NCORN(MAXE),IMATL(MAXE)
     7             ,NOPS(MAXE,8),NCRN(MAXE),LNO(MAXE),SIDF(MAXE)
     +             ,SIDQ(MAXE,3),ZMANN(MAXE),CHEZ(MAXE),NETYP(MAXE)
     +             ,DFCT(MAXE)
     +             ,EDD1(MAXE),EDD2(MAXE),EDD3(MAXE),nelord(MAXE)
     +             ,DROXINS(MAXE,8),DROYINS(MAXE,8)
     +             ,ELAREA(MAXE),neref(MAXE),nedep(MAXE),TVOLC(MAXE)
     +             ,TVOL(MAXE),ICOLLAPE(MAXE),NRELSF(MAXE),SIDFF(MAXE)
     +             ,NOPSS(MAXE,20),NFIXHS(MAXE),EINX(MAXE),EINY(MAXE)
!     +             ,IEDSW1(MAXE),TBFACT1(MAXE),TBMIN1(MAXE)
!     +             ,INOFLOW(MAXE),NCTREF(MAXE),NTMREF(MAXE)
     +             ,INOFLOW(MAXE)
     +             ,NREORD(MAXE),EEXXYY(6,MAXE)
     +             ,IGTP(MAXE),igtcl(MAXE)
     +             ,ELMMIN(MAXE),MANMIN(MAXE),ELMMAX(MAXE),MANMAX(MAXE) 
     +             ,DRAGX(MAXE),DRAGY(MAXE),IMAN(MAXE),HMAN(MAXE,4)
     +             ,MANTAB(MAXE,4,2))

      !nis,jan08: All the following variables are referring to the number of material types (NMAT), which is not known here
      ALLOCATE (NCTREF (1: 1000), IEDSW1 (1: 1000), ort (1: 1000, 17))
      ALLOCATE (TBFACT1 (1: 1000), TBMIN1 (1: 1000), NTMREF (1: 1000))



C     LAYER VARIABLE
      ALLOCATE  (THL(MNPP),THLAY(MNPP,NLAYMX))

      ALLOCATE  (DEPRAT(MAXP),BSHEAR(MAXP),VS(MAXP),
     +           EDOT(MAXP),UST(MAXP),SERAT(MAXP),ESRO(MAXP)
     +          ,NLAYO(MAXP),NLAY(MAXP),BEDEL(MAXP),TM(MAXP),TMD(0:MAXP) 
     +          ,BEDORIG(MAXP),DEPINCR(MAXP),BEDELO(MAXP),DEPRATO(MAXP))
cipk sep06  add  DEPRAT0 and to TMD


      ALLOCATE      (CRSLOP(MAXE),TRSLOP(MAXP),srate(MAXP),gpbsav(MAXP)   
     +              ,RCAE(MAXE),RWAE(MAXE),RCAN(MAXP),RWAN(MAXP)
     +              ,TRRAT(MAXP),CAI(MAXP))

      ALLOCATE    (ELEVB(MAXP))

      ALLOCATE  (SD(MAXP),GPB(MAXP,MSAND)
     +             ,TTHICK(MAXP),ALPHA1(MAXP),ALPHA2(MAXP),IDONE(MAXP)
     +             ,DELBED(MAXP),GP(MSAND))

CIPK MAR06
      ALLOCATE
     +           (SGSAND(MAXP),CLDEND(MAXP),CLERND(MAXP),AMANNND(MAXP)
     
     +           ,D35ND(MAXP),D50ND(MAXP),D90ND(MAXP),GPMAXND(MAXP)
     +           ,VSANDND(MAXP),ISMODEND(MAXP)
     +           ,SDND(1,MAXP))

      ALLOCATE   (WAVEHT(MAXP),PEAKPRD(MAXP),WAVEDR(MAXP)
     +                ,WAVEHT0(MAXP),PEAKPRD0(MAXP),WAVEDR0(MAXP)
     +                ,WAVEHT1(MAXP),PEAKPRD1(MAXP),WAVEDR1(MAXP)
     +                ,WVH0(MAXP),WVA0(MAXP),WVT0(MAXP)
     +                ,WVH1(MAXP),WVA1(MAXP),WVT1(MAXP))

      ALLOCATE  (TRIBAREA(MAXP),ELTON(MAXP,12),deltab(MAXP),nsrc(MAXP)
     +              ,IEDONE(MAXE),NKEY1(MAXP),EXTLD(MAXP),trslp(MAXP)
     +              ,EXTLDEL(MAXE))

      ALLOCATE  (WDT1(11,MMAT1),WDT0(11,MMAT1),IYS(MMAT1)
     +       ,METID(MMAT1,9),DA(MMAT1),TTM(MMAT1),AETMP(MMAT1)
     +       ,BETMP(MMAT1),ISOL(MMAT1)
     +       ,DSOL(MMAT1),SOLIN0(MMAT1),SOLIN1(MMAT1),IDPT(MMAT1))

      ALLOCATE  (XCEN(MAXP*2),YCEN(MAXP*2),RADS(MAXP*2), 
     +           NKEY(MAXP*2),IGAP(MAXP*2))

      ALLOCATE   (XUSR(MAXP),YUSR(MAXP),XOUTL(MAXP),YOUTL(MAXP)
     +           ,NOPEL(3,MAXP*2),IPOLY(MAXP))

      ALLOCATE   (TSOLHR(MMAT1),HA(MMAT1),DRYBLB(MMAT1),WETBLB(MMAT1),
     +        CLOUD(MMAT1),WIND(MMAT1),AE(MMAT1),BE(MMAT1),SNOWMT(MMAT1)
     +   ,WDIRT(MMAT1),DAT(MMAT1),ATMPR(MMAT1))
      
      ALLOCATE (ICETHK(MAXP),QICE(MAXP),WNDSP(MAXP),WNDDR(MAXP)
     +,SNOWD(MAXP),AIRTMP(MAXP),ICETHKOL(MAXP),SIGMA(MAXP,2))

CIPK MAY06
      ALLOCATE (TMSAND(0:MAXP),TMSSAND(0:MAXP))
      
       
      TMSAND=0
      TMSSAND=0

CIPK AUG05 ZERO ARRAYS
      ITAB=0
      CRSLOP=0.
      TRSLOP=0.
      srate=0.
      gpbsav=0.   
      RCAE=0.
      RWAE=0.
      RCAN=0.
      RWAN=0.
      TRRAT=0.
      CAI=0.

      ELEVB=0.
      
      SD=0.
      GPB=0.
      TTHICK=0.
      ALPHA1=0.
      ALPHA2=0.
      IDONE=0
      DELBED=0.
      GP=0.

CIPK MAR06
      SGSAND=0.
      CLDEND=0.
      CLERND=0.
      AMANNND=0.
      D35ND=0.
      D50ND=0.
      D90ND=0.
      GPMAXND=0.
      VSANDND=0.
      ISMODEND=0
      SDND=0.


      WAVEHT=0.
      PEAKPRD=0.
      WAVEDR=0.
      WAVEHT0=0.
      PEAKPRD0=0.
      WAVEDR0=0.
      WAVEHT1=0.
      PEAKPRD1=0.
      WAVEDR1=0.
      WVH0=0.
      WVA0=0.
      WVT0=0.
      WVH1=0.
      WVA1=0.
      WVT1=0.

      TRIBAREA=0.
      ELTON=0.
      deltab=0.
      nsrc=0
      IEDONE=0
      NKEY1=0
      EXTLD=0.
      trslp=0.
      
      WDT1=0.
      WDT0=0.
      IYS=0
      METID=0.
      DA=0.
      TTM=0.
      AETMP=0.
      BETMP=0.
      ISOL=0
      DSOL=0.
      SOLIN0=0.
      SOLIN1=0.
      IDPT=0
      XCEN=0.
      YCEN=0.
      RADS=0.
      NKEY=0
      IGAP=0
      XUSR=0.
      YUSR=0.
      XOUTL=0.
      YOUTL=0.
      NOPEL=0
      IPOLY=0
      TSOLHR=0.
      HA=0.
      DRYBLB=0.
      WETBLB=0.
      CLOUD=0.
      WIND=0.
      AE=0.
      BE=0.
      SNOWMT=0.
      WDIRT=0.
      DAT=0.
      ATMPR=0.
      ICETHK=0
      QICE=0.
      WNDSP=0.
      WNDDR=0.
      SNOWD=0.
      AIRTMP=0.
      ICETHKOL=0
      SIGMA=0.
CIPK MAR01
      NODETR=0

      DO J=1,MNPP
        THL(J)=0.
        DO K=1,NLAYMX
          THLAY(J,K)=1.0
        ENDDO
      ENDDO
      DO J = 1, MAXP
        IBN(J)=0
        IBNA(J)=0
        NFIX1(J)=0
        FCTS(J)=0.
        NREF(J)=0
        NTHREE(J)=0
        XSLP(J)=0.
        YSLP(J)=0.
        NFIXK(J)=0
        VOUTN(J)=0.
        ICPON(J)=0
        DROXIN(J)=0
        DRODXN(J)=0
        DROYIN(J)=0
        DRODYN(J)=0
        IRBD(J)=0
        NFIXSAV(J)=0
        HEL(J)=0
        HOL(J)=0
        JPOINT(J)=0
        SIDN(J)=0.0
        XTLN(J)=0.
        WIDBS(J)=0.
        WSS(J)=0.
        ICOLLAP(J)=0
        WSLL(J)=0.
        H2OL(J)=0.
        NFIXSV1(J)=0
        GAN(J)=0.
        GAN0(J)=0.
        DGN(J)=0.
        IBNA(J)=0
        XNU(J)=0.1
        XNMANN(J)=0.
        ISTLIN(J)=0
        VSCALE(J)=0.
        DDHDX(J)=0.
        DDHDY(J)=0.
        DDAODX(J)=0.
        DDAODY(J)=0.
        DIVID(J)=0.
        DO K=1,10
          XVEL(10,J)=0.
        ENDDO
        LAB(J)=0
        EVISXZ(J)=0.
        EVISYZ(J)=0.
        DVISZ(J)=0.
        UDST(J)=0.
        VDST(J)=0.
        UUDST(J)=0.
        UVDST(J)=0.
        VVDST(J)=0.
        SDST(J)=0.
        TDST(J)=0.
        SEDST(J)=0.
        DO K=1,3
          WAT(J,K)=0.
          NODWT(J,K)=0
        ENDDO
        IPT(J)=0
        DREC(J)=0.
        ADB(J)=0.
        ADT(J)=0.
        ADO(J)=0.
        AKP(J)=0.
        DAME(J)=0.
        AME(J)=0.
        ACOR(J)=0.
        ADOKP(J)=0.
        AOKP(J)=0.
        IWFLOW(J)=0
        WFLOW(J)=0.
        WLEN(J)=0.
        wflolw(J)=0
        ISUBM(J)=0
        SPWP(J)=0.
        AWL(J)=0.
        SWH(J)=0.
        WVDR(J)=0.
        DO K=1,6
          STR1(J,K)=0.
          STR2(J,K)=0.
        ENDDO
cipk sep06
        DEPRATO(J)=0
        DEPRAT(J)=0.
        BSHEAR(J)=0.
        VS(J)=0.
        EDOT(J)=0.
        UST(J)=0.
        SERAT(J)=0.
        ESRO(J)=0.
        NLAYO(J)=0
        NLAY(J)=0
        BEDEL(J)=0.
        TM(J)=0.
        TMD(J)=0.
        BEDORIG(J)=0.
        DEPINCR(J)=0.
CIPK JUN05
        !nis,com: transition elevation and submerging water stage initialized to unrealistic low values
        WHGT(J)=-10000.
        TRANSEL(J)=-10000.

        NSPL(J)=0
        NSTRT(J,1)=0
        NSTRT(J,2)=0
        NDRY(J)=1
        NDRYO(J)=0
        WIDTH(J)=0.
        WIDS(J)=0.0
        SS1(J)=0.
        SS2(J)=0.
        NFIXP(J)=0
        DIR(J)=0.
        PRESS(J)=0.
        DEN(J)=0.
        NODC(J)=0
        ADIF(J)=0.
        FC(J,1)=0.
        FC(J,2)=0.
        FC(J,3)=0.
        NSURF(J)=0
        NBTN(J)=0.
        VVEL(J)=0.0
        FCTV(J)=0.0
        CORD(J,1) = VOID
        CORD(J,2) = VOID
        CORD(J,3) = 0.0
        CORDS(J,1) = VOID
        CORDS(J,2) = VOID
        CORDS(J,3) = 0.0
        AO(J) = 0.0
        AORIG(J)=0.0
        ALFA(J) = 0.0
        ALFAK(J) = 0.0
        SIGMA(J,1) = 0.0
        SIGMA(J,2) = 0.0
        NFIX(J) = 0
        NLOC(J)=0
        NDEP(J)=0
CIPK SEP02
        ICETHK(J)=0.
        ICETHKOL(J)=0.
CIPK MAY02
        STRESS(J,1)=0.
        STRESS(J,2)=0.
        STR10(J)=0.
        STR11(J)=0.
        STR20(J)=0.
        STR21(J)=0.
CIPK SEP02
        WAVEHT(J)=0.
        WAVEDR(J)=0.
        PEAKPRD(J)=0.
        TTHICK(J)=0.
        DELBED(J)=0.
        ELEVB(J)=0.
        DO K = 1, 4
          NDROP(J,K)=0
        ENDDO
        DO K = 1, 7
          IESPC(K,J)=0
          IACTV(J,K)=0
          SPEC(J,K) = 0.0
          NBC(J,K) = 0
          VEL(K,J)=0.0
          VDOT(K,J) = 0.0
          VDOTO(K,J) = 0.0
          VOLD(K,J) = 0.0
          V2OL(K,J)=0.0
          NBCKP(J,K)=0
        ENDDO
CIPK NOV97
        HDET(J)=0.
        HDOT(J)=0.
      ENDDO

      DO J=1,MAXE
        DO K=1,20
          NOP(J,K)=0
          NOPSS(J,K)=0
        ENDDO
        IMAT(J)=0
!nis,jan08: ort variable is connected to number of material types
!!NiS,apr06: increased number of variables:
!!        DO K=1,14
!        DO K=1,17
!!-
!          ORT(J,K)=0.
!        ENDDO
!        LISTEL(J)=0
        NFIXH(J)=0
        TH(J)=0.
        AREA(J)=0.
        NCORN(J)=0
        IMATL(J)=0
        IMATO(J)=0
        NCRN(J)=0
        LNO(J)=0
        SIDF(J)=0.
        DO K=1,3
          SIDQ(J,K)=0.
        ENDDO
        ZMANN(J)=0.
        CHEZ(J)=0.
        NETYP(J)=0
        DFCT(J)=0.
        nelord(J)=0
        DO K=1,8
          DROXINS(J,K)=0.
          DROYINS(J,K)=0.
        ENDDO
        ELAREA(J)=0.
        neref(J)=0
        nedep(J)=0
        TVOLC(J)=0.
        TVOL(J)=0.
        ICOLLAPE(J)=0
        NRELSF(J)=0
        SIDFF(J)=0.
        NFIXHS(J)=0
        EINX(J)=0.
        EINY(J)=0.
!        IEDSW1(J)=0
!        TBFACT1(J)=0.
!        TBMIN1(J)=0.
        INOFLOW(J)=0
!        NCTREF(J)=0
!        NTMREF(J)=0
        NREORD(J)=0
        ISUBMEL(J)=0
        NFCTP(J)=0
        DO K=1,6
          EEXXYY(K,J)=0.
        ENDDO
        IGTP(J)=0
        igtcl(J)=0
        ELMMIN(J)=0.
        MANMIN(J)=0
        ELMMAX(J)=0.
        MANMAX(J)=0 
        DRAGX(J)=0.
        DRAGY(J)=0.
        IMAN(J)=0
        DO K=1,4
          HMAN(J,K)=0.
          DO L=1,2
            MANTAB(J,K,L)=0
          ENDDO
        ENDDO
        EDD1(J)=1.0
        EDD2(J)=0.0
        EDD3(J)=0.
        ZMANN(J)=0.
        CHEZ(J)=0.
        TH(J)=0.0
        AREA(J)=0.
        NFIXH(J)=0
        NCORN(J)=0
        IMAT(J)=0

cipk nov99
        DFCT(J)=0.0
CIPK MAR01
        ELMMIN(J)=0.0
        MANMIN(J)=0.0
        ELMMAX(J)=0.0
        MANMAX(J)=0.0
        DRAGX(J)=0.0
        DRAGY(J)=0.
cipk jan02
        SIDFF(J)=0.0

      ENDDO

      !nis,jan08: nctref is for the material types, especially for the weir structures
      !nis,jan08: ort-variable is according to the material types
      do j = 1, 1000
        NCTREF (J) = 0
        NTMREF (J) = 0
        IEDSW1 (J) = 0
        TBFACT1 (J) = 0.
        TBMIN1 (J) = 0.
        DO K=1,17
          ORT(J,K)=0.
        ENDDO
      end do

      DO I=1,MAXP
        ISTLIN(I)=0.
      ENDDO
      DO I=1,20
        STQA(I)=0.
        STQC(I)=0.
        STQ(I)=0.
        STQE(I)=0.
      ENDDO

      DO I=1,MAXE
        SIDF(I)=0.
        IGTP(I)=0
CIPK JUN05
        NFCTP(I)=0
        DO J=1,8
          NOPS(I,J)=0
        ENDDO
      ENDDO

      DO J=1,100
        NDUPJ(J)=0
        !nis,oct07: Needs to be initialized, if it should stand some tests
        ndflj(j) = 0
        !-
        NDDNJ(J)=0
        cj(j)=1.0
      ENDDO

CIPK MAR01
      DO J=1,50
        NOUTCC(J)=0
        ADDSAL(J)=-9999.
        ADDTMP(J,1)=-9999.
        ADDSED(J)=-9999.
      ENDDO

!NiS,apr06: allocating arrays for roughness calculation in DARCY-WEISBACH-equation
      ALLOCATE (CNIKU(MaxE), DURCHBAUM(MaxE), ABST(MaxE))
      ALLOCATE (C_WR(MaxE))
      ALLOCATE (mh(MaxE), mvx(MaxE), mvy(MaxE), mvxvy(MaxE))
      DO j=1, MaxE
        CNIKU(j)     = 0.0
        DURCHBAUM(j) = 0.0
        ABST(j)      = 0.0
        c_wr(j)      = 1.0
      ENDDO
!-
!NiS,apr06: allocating arrays for neighbourhood relations
      ALLOCATE(nconnect(1:MaxP),neighb(1:MaxP,0:3535),mcord(1:MaxE,1:2))
!-

!NiS,jul06: allocating the (for the moment) dummy gl_bedform array to pass variables correctly to the subroutine 
!           formrauheit
      allocate (gl_bedform(1:MaxE,1:4))
      do i = 1, MaxE
        do j = 1,4
          gl_bedform(i,j) = 0
        enddo
      enddo
!-

!nis,jun07: Add initializaton of membership of nodes in a transition
      ALLOCATE (TransitionMember (1: MaxP))
      ALLOCATE (TransitionElement (1: MaxE), TransLinePart (1: MaxE))
      ALLOCATE (dspecdh(1:MaxP), dspecdv(1:MaxP))
      do i = 1, MaxP
        TransitionMember(i) = .false.
        dspecdh (i) = 0.0
        dspecdv (i) = 0.0
      end do
      do i = 1, MaxE
        TransitionElement (i) = .false.
        TransLinePart (i) = 0
      end do
!-

!nis,nov06: allocating Transition lines for 1D 2D line transition;
!           1. 1D element number of the coupling
!           2. continuity line number of the transtion
!           3. 1D coupling node [nop(TransLines(i,1),3) = TransLines(i,3)]
      ALLOCATE (TransLines (MaxLT,1:4))
      DO i = 1, MaxLT
        DO j = 1,4
          TransLines (i,j) = 0
        ENDDO
      ENDDO
      
      allocate (PipeSurfConn(1:maxps))
      do i = 1, maxps
        PipeSurfConn(i)%SurfElt = 0
        PipeSurfConn(i)%pipeElt = 0
        PipeSurfConn(i)%PipeFlow = 0.0d0
        PipeSurfConn(i)%SurfFlow = 0.0d0
        PipeSurfConn(i)%DflowWRTv_upper = 0.0d0
        PipeSurfConn(i)%DflowWRTh_upper = 0.0d0
        PipeSurfConn(i)%DflowWRTv_lower = 0.0d0
        PipeSurfConn(i)%DflowWRTh_lower = 0.0d0
      enddo
      allocate (ConnectedElt (1:MaxE))
      do i = 1, maxe
        ConnectedElt(i) = 0
      enddo
      
      allocate (StorageElts (1: maxSE))
      

!nis,nov06: allocating the 1D-2D-Transition-line-Factor array and initialization of that (at the beginning no scaling)
!           in the coefs subroutines this factor is generally applied!
!           If later this approach is expanded also to calculate the concentrations, this EqScale array was initialized
!           for all degrees of freedom
      ALLOCATE (EqScale (1:MaxP,1:7))
      !for all nodes (in general only nodes at line-transition, but differing use is also possible)
      do i = 1, MaxP
        !for all degrees of freedom
        do j = 1, 7
          EqScale (i,j) = 1.0
        end do
      end do
!-

!nis,dec06: Initializing problematic array here
      do i = 1,MaxE
        EXTLDEL(i) = 0
      end do
!-

      !EFa Nov06, allocating für Teschke-1D-Elemente
      !polynom coefficients
      ALLOCATE (apoly      (1: MaxPolyA, 1: maxp, 0:12))
      ALLOCATE (qpoly      (1: MaxPolyQ, 1: maxp, 0:12))
      ALLOCATE (alphapoly  (1: MaxPolyB, 1: maxp, 0:12))
      ALLOCATE (betapoly   (1: MaxPolyB, 1: maxp, 0:12))
      WRITE(*,*) maxpolya, maxpolyq, maxpolyb
      ALLOCATE (polyrangeA (1: maxp, 1: MaxPolyA))
      ALLOCATE (polyrangeQ (1: maxp, 1: MaxPolyQ))
      ALLOCATE (polyrangeB (1: maxp, 1: MaxPolyB))
      ALLOCATE (polysplitsA (1: maxp))
      ALLOCATE (polysplitsQ (1: maxp))
      ALLOCATE (polysplitsB (1: maxp))
      !intersecting water depth for flow coefficient (e.g. Boussinesq)
      ALLOCATE (hbordv(maxp))
      !validity range for polynoms
      ALLOCATE (hhmin(maxp))
      ALLOCATE (hhmax(maxp))
      !reference friction slope
      ALLOCATE (qgef(maxp))
      !flow kilometer of node
      ALLOCATE (kmx(1:maxp))

      !cross sectional area
      ALLOCATE (ah(1:maxp))
      ALLOCATE (dahdh(1:maxp))


      DO i = 1, MaxP
        !polynom coefficients
        do p = 1, MaxPolyA
          polyrangeA (i, p) = 0.0
          DO j = 0, 12
            apoly (p, i, j) = 0.0
          enddo
        enddo
        do p = 1, MaxPolyQ
          polyrangeQ (i, p) = 0.0
          DO j = 0, 12
            qpoly (p, i, j) = 0.0
          enddo
        enddo
        do p = 1, MaxPolyB
          polyrangeB (i, p) = 0.0
          DO j = 0, 12
            alphapoly (p, i, j) = 0.0
            betapoly  (p, i, j) = 0.0
          enddo
        enddo
      enddo

      do i = 1, MaxP
        polySplitsA (i) = 0
        polySplitsQ (i) = 0
        polySplitsB (i) = 0
        !validity range for polynoms
        hhmin(i)      = 0.0
        hhmax(i)      = 10.0e3
        !flow kilometer of node
        kmx(i)        = -1.0
        !reference friction slope
        qgef(i)       = 0.0
        !intersecting water depth for flow coefficient (e.g. Boussinesq)
        hbordv(i)     = 0.0
        !cross sectional areas, derivative of areas and discharge
        ah(i)         = 0.0
        dahdh(i)      = 0.0
      ENDDO

      !nis,jun07: Proforma initialization
      !roughness class
      irk = 0
      !-
      !nis,jun07: iedrop is used as a source/sink for sediment or something. It needs to be initialized; there might be up to 9 scrs/snks
      !MD:  do i = 1, 9 : New: more than 9 Mat-Types >> now limited to 85
      DROPMAX = 0
      do i = 1, 85
        iedrop (i) = 0
      end do
      do i = 1, 50
        lmt(i) = 0
        q2D(i) = 0.0
        dq2ddh(i) = 0.0
      end do

!MD: Maximal and Minimal Sediment-Concentration
!MD: can be changed by user by Maxsed and MinSed in CONTROL
      SedHighPerm = 100000.0
      SedLowPerm = 0.0

      !nis,jun07: maxfil is not zero, if there was a scratch file, otherwise it should be zero, ALWAYS
      MAXFIL = 0
      !-
      !nis,jul07: following values are causing problems somewhere, therefore initialize them
      TETH = 0.0

      iwndmet = 0
      ihtmet = 0
      !-


      !nis,aug07: Introducing correction factors for elements roughness, only used for Darcy-approach
      ALLOCATE (correctionKS(1: maxe), correctionAxAy(1: maxe),
     +          correctionDp(1: maxe))
      do i = 1, maxe
        correctionAxAy(i) = 1.0
        correctionDp(i)   = 1.0
        correctionKS(i)   = 1.0
      end do
      !-

      !nis,aug07: Introducing flow resistance storage for every element
      ALLOCATE (lambdaKS(1:maxe), lambdaP(1:maxe), lambdaDunes(1:maxe),
     +          lambdaTot(1:maxe))
      do i = 1, maxe
        lambdaKS(i) = 0.0
        lambdaP(i) = 0.0
        lambdaDunes(i)  = 0.0
        lambdaTot(i) = 0.0
      end do
      !-

      !MD Introducing friction factor storage for every node
      ALLOCATE (FFACT_KN(1:maxp),FFACT_EL(1:maxe))
      do i = 1, MaxP
        FFACT_KN(i) = 0.0
      end do
      do i = 1, maxe
        FFACT_EL(i) = 0.0
      end do
      !-

      !nis,aug07: number of 12 elements maximum at a node is because of reducing the inside element angle to
      !           20 degrees in average (360°/12)
      !TODO: make second entry dynamical
      ALLOCATE (IsNodeOfElement(1:maxp, 0:12))
      do i = 1, maxp
        do j = 0, 12
          !0: counter
          !1-12: element numbers
          IsNodeOfElement(i, j) = 0
        end do
      end do

      !initializations for lines
      do i = 1, 50
        do j = 1, 3535
          lineimat(i, j) = 0
          lineelement (i, j) = 0
          LineCorrectionKS (i, j)   = 1.0
          LineCorrectionAxAy (i, j) = 1.0
          LineCorrectionDp (i, j)   = 1.0
        end do
      end do
      !-

      !EFa jun07, necessary for autoconverge
      beiauto = 0.0
      nnnunst = 0.0
      nnnst = 0.0
      linlog = 0.0
      hhh = 0.0
      hhh2 = 0.0
      qqq = 0.0
      qqqdir = 0.0
      exterr = 0.0
      !EFa aug07
      autoindex = 0.0
      !-
      ALLOCATE(temp_vel(7,maxp))
      ALLOCATE(temp_vdot(7,maxp))
      ALLOCATE(temp_vdoto(7,maxp))
      do i = 1,maxp
        do k = 1,7
          temp_vel(k,i) = 0
          temp_vdot(k,i) = 0
          temp_vdoto(k,i) = 0
        end do
      end do
      !-

      ALLOCATE (minvel (1:3, 1:maxp), maxvel (1:3, 1:maxp))
      ALLOCATE (rausv(1:4, MaxP))
      allocate (minrausv (1:MaxP), maxrausv (1:MaxP))
      do i = 1, maxp
        minrausv (i) = 100000.0d0
        maxrausv (i) = 0.0d0
        do j = 1, 3
          maxvel (j, i) = 0.0d0
          minvel (j, i) = 100000.0d0
        end do
        do j = 1, 4
          rausv (j, i) = 0.0d0
        end do
      end do

      !EFa jul07, added istab for stage-flow boundaries (table)
      ALLOCATE(istab(maxp))
      do i = 1,maxp
        istab(i) = 0.
      end do
      !-

      !EFa aug07, necessary for autoconverge
      ALLOCATE(temp_speccc(3))
      do i = 1,3
        temp_speccc(i) = 0.0
      end do
      !-

      ALLOCATE (IntPolNo (1: MaxE), NeighProf (1: MaxP, 1: 2))
      ALLOCATE (IntPolProf (1: MaxP), kmWeight (1: MaxP))
      allocate (IsPolynomNode (1: MaxP))
      ALLOCATE (IntPolElts (1: MaxE, 1: maxIntPolElts))
      do i = 1, MaxE
        IntPolNo (i) = 0
        do j = 1, maxIntPolElts
          IntPolElts (i, j) = 0
        end do
      enddo
      do i = 1, MaxP
        IntPolProf (i) = .FALSE.
        IsPolynomNode (i) = .FALSE.
        NeighProf (i, 1) = 0
        NeighProf (i, 2) = 0
        kmWeight (i) = 1.0D0
      end do

      RETURN
      END
