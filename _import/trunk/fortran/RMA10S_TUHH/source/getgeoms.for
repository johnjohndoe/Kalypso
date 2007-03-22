C     Last change:  K    15 Feb 2007    6:48 pm
cipk  last update june 27 2005 allow for control structures
CIPK  LAST UPDATE SEP 06 2004 CREATE ERROR FILE
CIPK  LAST UPDATE AUG 4 2003  ADD BLK11
CIPK  LAST UPDATE JUN 20 2003 ALLOW RM1 FORMAT INPUT
CIPK  LAST UPDATE AUG 21 2002 ALLOW FOR READ OF SMS FORMAT
cipk  last update add special coding for R. Watanabe to EL format
CIPK  LAST UPDATE JUL 25 2001 FIX BUG FOR 1 LAYER LD3 CASE
CIPK  LAST UPDATE FEB 9 2001 MOVE READING  OF CC1 LINES AND ADD CCLINE INPUT
CIPK  LAST UPDATE SEP 4 2000 REVISED OUTPUT OF ERROR MESSAGES
CIPK  LAST UPDATE AUG 10 2000 FIX LD3 INDIVIDUAL OPTION
CIPK  LAST UPDATE mAR 28 2000 CORRECT BUG IN TRANSITION FROM 2-D TO 3-D
cipk  lsat update Feb 25 2000 alter temp use of den to wss remove END= in reads
cipk  last update Dec 8 1999 Allow fo VD line to be skipped for 2-d only
cipk  last update Apr 28 1999 add more tests in read of geo files
cipk  last update Feb 2 1999 add test for max layers
CIPK  LAST UPDATE JAN 25 1999 REFINE TESTING WHEN LARGE NUMBER OF LAYERS INPUT
CIPK  LAST UPDATE JAN 19 1999 ADD MARSH PARAMETERS FOR 2DV TRANSITIONS REVISE
C                   JUNCTION PROPERTIES
cipk  last update Jan 3 1999 add for 2dv junctions
cipk  last update Aug 27 1998 fix marsh option
cipk  last update Aug 22 1997 fix problem with alfak
CIPK  LAST UPDATE OCT 1 1996
      SUBROUTINE GETGEO
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSUBMOD
      USE PARAMMOD
      SAVE
!NiS,jul06: Consistent data types for passing parameters
      INTEGER :: n, m, a, lt, fffms
!-
C-
cipk aug05	INCLUDE 'BLK10.COM'
CIPK AUG03 ADD
CIPK AUG05	INCLUDE 'BLK11.COM'
CIPK NOV97 REACTIVATE BLKDR
CIPK AUG05	INCLUDE 'BLKDR.COM'
CIPK JUN05
CIPK AUG05	INCLUDE 'BLKSUB.COM'

cipk apr99 add line below
      character*8 id8

!nis,jan07: Line Transition needs a temporary node storage place
      INTEGER :: noptemp
!-

CIPK MAY02 ADD CHARACTER VARIABLES
      CHARACTER*6 HEADSH
      CHARACTER*1000 HEADING

      DATA VOID/-1.E20/
      DATA A1,A2,A3/1.939938,5.588599E-5,-1.108539E-5/

C-
C-    Set limits for testing
C-

cWP Feb 2006, not used in this subroutine
cWP      MMM1=MNP
cWP      MMM2=MEL

cipk feb01
      NCLL=0

CNiS,mar06: show values of geometry recognition
        WRITE(*,*)' IFILE=  ', IFILE    
        WRITE(*,*)' IGEO=   ', IGEO
        WRITE(*,*)' ISMSGN= ', ISMSGN
CNis,mar06: control output for maximum values
        WRITE(*,*)' MaxP=   ', MAXP
        WRITE(*,*)' MaxE=   ', MAXE


CIPK AUG02
      IF(ISMSGN .GT. 0) THEN
CIPK SEP05 ALLOW FOR FIRST CALL
        CALL RDBIN(N,M,0)
      ENDIF 
C-
cipk may02

!NiS,mar06,com: End of first reading-block

C-    Read header from 2-d geometry file to test for size and type
C-

!NiS,mar06,com: start of second reading block: RMA-files binary or ASCII

      IF( IFILE .NE. 0 )  THEN
CIPK JUN03
       !NiS,mar06,com: binary-file (IGEO.eq.1)
       IF(IGEO .EQ. 1) THEN       
         READ(IFILE) HEADSH
         !NiS,mar06,com: Setting long/short-binary file format (ILONG=1 means long; ILONG=0 means short)
         IF(HEADSH .EQ. 'RMAGEN') THEN
           ILONG=1
           REWIND IFILE
           READ(IFILE) HEADING

          write(*,*) 'In GETGEO. ILONG = ',ILONG, ' HEADING = ',HEADING

         ELSE
           ILONG=0
           REWIND IFILE
         ENDIF
         READ (IFILE) N,M
         IERR=0

!NiS,mar06,com: Stop in between of reading-block 2

         IF(N .GT. MAXP) THEN
CIPK SEP04 CREATE ERROR FILE
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
           WRITE(*,*) ' Number of nodes exceeds dimension limit'
           WRITE(*,*) ' Number of nodes on file = ',N
           WRITE(75,*) ' Number of nodes exceeds dimension limit'
           WRITE(75,*) ' Number of nodes on file = ',N
           IERR=1
         ENDIF
         IF(M .GT. MAXE) THEN
CIPK SEP04 CREATE ERROR FILE
           IF(IERR .EQ. 0) THEN
	       CLOSE(75)
             OPEN(75,FILE='ERROR.OUT')
	     ENDIF
           WRITE(*,*) ' Number of elements exceeds dimension limit'
           WRITE(*,*) ' Number of elements on file = ',M
           WRITE(75,*) ' Number of elements exceeds dimension limit'
           WRITE(75,*) ' Number of elements on file = ',M
           IERR=1
         ENDIF

!NiS,mar06,com: Ende ABFRAGEBLOCK

!NiS,mar06,com: Control output of net size, that was recognized within the first reading

         write(75,*) 'initial read', n,m
         IF(IERR  .EQ. 1) STOP
         REWIND IFILE

!NiS,mar06,com: reading block for RMA-binary file (geo-format)

C-
C     Now read 2-d geometry file
C-
cipk feb00 and change to wss     READ (IFILE,end=46,ERR=46)
CIPK MAY02
         IF(ILONG .EQ. 1) READ(IFILE) HEADING
         read(ifile) n,m
         write(75,*) 'nodes =',n, 'elements =',m
         write(*,*) 'nodes =',n, 'elements =',m
         if(n .gt. maxp) THEN
CIPK SEP04 CREATE ERROR FILE
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
 	     WRITE(75,*) 'too many nodes'
           write(75,*) 'nodes =',n, 'elements =',m
 	     stop 'too many nodes'
	   ENDIF
         if(m .gt. maxe) THEN
CIPK SEP04 CREATE ERROR FILE
           CLOSE(75)
           OPEN(75,FILE='ERROR.OUT')
 	     WRITE(75,*) 'too many elements'
           write(75,*) 'nodes =',n, 'elements =',m
	     stop 'too many elements'
	   ENDIF
         rewind (ifile)

!NiS,mar06,com: reading block for binary geometry data, that is node and element data

CIPK MAY02  MAJOR CHANGE TO ALLOW FOR REAL*8 CORD ETC


cWP Feb 2006 Error while opening binary file with Lahey FORTRAN, likely
cWP Feb 2006 because of wrong internal record length of binary file.
cWP Feb 2006 Now detailled check for READ statement!
!NiS,mar06:     problem solved; error source was the compiling option
!               in the automake.fig file, which builts every real
!               variables to real(kind=8) variables. For proper reading
!               kind=4 variables are needed. After deleting that option
!               out of the automake.fig file, the program worked.

         IF(ILONG .EQ. 1) THEN
           READ(IFILE) HEADING

           WRITE(*,1010)
 1010      FORMAT(/1X, 'Now start reading binary GEO-FILE...')

!NiS,may06: Lahey version
!           READ (IFILE,ERR=21, END=21)
           READ (IFILE,ERR=21)
     1     n,m,((cord(j,k),K=1,2), ALFA(j),wss(j),J=1,N),
     2     ((NOP(J,K),K=1,8),IMAT(J),TH(J),NFIXH(J),J=1,M)
     +    ,(WIDTH(J),SS1(J),SS2(J),WIDS(J),J=1,N)

           WRITE(*,1011)
 1011      FORMAT(/1X, '...finished first part!')

           write(75,*) 'REAL*8 format',n,m
         ELSE
!NiS,may06: Lahey version
!           READ (IFILE,ERR=21, END=21)
           READ (IFILE,ERR=21)
!-           
     1     N,M,((CORDS(J,K),K=1,2), ALFA(J),wss(J),J=1,N),
     2     ((NOPSS(J,K),K=1,8),IMAT(J),TH(J),NFIXHS(J),J=1,M)
     +    ,(WIDTH(J),SS1(J),SS2(J),WIDS(J),J=1,N)
           write(75,*) 'long format',n,m
           DO K=1,N
             DO J=1,3
               CORD(K,J)=CORDS(K,J)
             ENDDO
           ENDDO
           DO K=1,M
             DO J=1,8
               NOP(K,J)=NOPSS(K,J)
             ENDDO
             NFIXH(K)=NFIXHS(K)
           ENDDO
         ENDIF

cipk feb00 copy wss to ao  because ao is real*8
         DO J=1,N
           AO(J)=WSS(j)
           WSS(J)=0.
         ENDDO

cipk apr99 add more flexibility to reading files, allow for sloping overbank

CIPK JUL00 NEED TO REMOVE END= FOR LAHEY

!NiS,may06: Lahey version
!          read(ifile,err=24, end=24) id8
          read(ifile,err=24) id8
!-          

!NiS,mar06	Hinzugefügt!, wird denn richtig weiter gelesen?
          WRITE(*,*)' ID8:  *>',id8,'<* Ende'

          if(id8(1:6) .eq. 'part-2') then
            write(75,*) 'reading part 2'
            iwdbs=1
            read(ifile,err=262) (widbs(j),wss(j),j=1,n)
          else
            iwdbs=0
          endif
!NiS,may06: Lahey version
!          read(ifile,err=24,end=24) id8
          read(ifile,err=24) id8
!-          
          if(id8(1:6) .eq. 'part-3') then
            write(75,*) 'reading part 3'
            read(ifile,err=262) ncll,((line(j,k),k=1,350),j=1,ncll)
            do j=1,ncll
              do k=1,350
                if(line(j,k) .eq. 0) then
                  lmt(j)=k-1
                  go to 26
                endif
              enddo
   26         continue
            enddo
          else
            ncll=0
          endif
          GO TO 24
C-
C     If there is an error try short format
C-
   21     CONTINUE
          write(75,*) 'short format',n,m
          REWIND IFILE
          READ (IFILE,ERR=262)
     1     N,M,((CORDS(J,K),K=1,2), ALFA(J),wss(J),J=1,N),
     2    ((NOPSS(J,K),K=1,8),IMAT(J),TH(J),NFIXHS(J),J=1,M)
cipk feb00 copy wss to ao
           DO K=1,N
             AO(K)=wss(K)
             wss(K)=0.
             DO J=1,3
               CORD(K,J)=CORDS(K,J)
             ENDDO
           ENDDO
           DO K=1,M
             DO J=1,8
               NOP(K,J)=NOPSS(K,J)
             ENDDO
             NFIXH(K)=NFIXHS(K)
           ENDDO
C-
   24     CONTINUE

C-
C     Now look for layer data
C-
c         READ(IFILE,END=48,ERR=48) 
c     +          N,MAXVL,(NDEP(N),(THLAY(N,K),K=1,MAXVL),N=1,N)
c         NDP=3
c         WRITE(LOUT,*) 'LAYER DATA READ FROM FILE'
c         WRITE(LOUT,*) 'MAXIMUM NUMBER OF LAYERS =',MAXVL
c         GO TO 49
c   48    WRITE(LOUT,*) 'NO LAYER DATA ON GEOMETRIC FILE'
c   49    CONTINUE
CIPK JUN03
!NiS,mar06: Change because of additional option in if-block
!       ELSE
       ELSEIF (IGEO == 0) THEN
         CALL RDRM1(N,M,0)
         NCLL=NCL

!NiS,mar06: Write control output in RM1 format, so it is readable again with for example RMAGEN
!-CONTROL OUTPUT FILE IN RM1 FORMAT------------------------------------------------------------
      OPEN(5555,'testRm1.rm1')
      do i = 1, maxe
        if (nop(i,1) /= 0) then
          WRITE(5555,*)i,(nop(i,j),j=1,8),imat(i),imato(i),nfixh(i)
        end if
      ENDDO
      do i = 1, maxp
        write (5555,*)i,(cord(i,j),j=1,2), ao(i)
      end do
      do i = 1, ncl
        write (5555,*) (line(i,j),j = 1, lmt(i))
      end do
      CLOSE(5555, STATUS='keep')
!-


!NiS,mar06: new option in if-block to enable the program to read 2D-geometry in Kalypso-2D-Format
       ELSEIF (IGEO ==2) then
         !nis,feb07: Allow for numbered FFF midsides
         !call rdkalyps(n,m,a,lt,0)
         call rdkalyps(n,m,a,lt,fffms,0)
         !-
  !NiS,apr06: adding this transoformation like it is called after RDRM1 (see above)
         NCLL = NCL
  !-

!NiS,mar06: Write control output in RM1 format, so it is readable again with for example RMAGEN
!-CONTROL OUTPUT FILE IN RM1 FORMAT------------------------------------------------------------
         OPEN(5555,'testKalypso.rm1')
         do i = 1, maxe
           if (nop(i,1) /= 0) then
             istat = 0
             WRITE(5555,FMT=2001,IOSTAT=istat)i,(nop(i,j),j=1,8),imat(i)
     +        ,0.0,nfixh(i)
             if (istat /= 0) then
               write (*,*) 'Fehler beim Schreiben'
             end if
 2001        FORMAT(10I5,F10.3,I5)
           end if
         ENDDO
         WRITE(5555,'(i5)')9999
         WRITE(*,*)'justinfo: ',maxe
         do i = 1, (maxp-1)
           write (5555,2002)i,(cord(i,j),j=1,2), ao(i),0,0.0
 2002      format (I10, 2(F16.6,'    '),F10.3,
     +  '                                                            ',
     +  I10,F10.4)
         end do
         WRITE(5555,'(i5)')9999
         CLOSE(5555, STATUS='keep')
!-

       ENDIF
      ENDIF
!-

!NiS,mar06,com: End of geometry reading block

CIPK FEB01 BEGIN NEW LOCATION FOR CC LINE READ

C-
C-.....INPUT/OUTPUT LINES FOR CONTINUITY CHECKS.....
C-
CIPK AUG02
   25 CONTINUE
      NCL=NCLL
      NCLM=NCLL
   28 CONTINUE
!NiS,may06: The geometry file must not be closed because of RESTART OPTION
cipk feb03  close geometry file
!      IF(ifile .gt. 0) close (ifile)
!-
      IF(ID(1:3) .EQ. 'CC1') THEN
        READ(ID(5:8),'(I4)') NCLT
        IF(NCLT .NE. 0) THEN
          NCL=NCLT
        ELSE
          NCL=NCLM+1
        ENDIF
        IF(NCL .GT. NCLM) NCLM=NCL
        READ(DLIN,5012) (LINE(NCL,K),K=1,9)
 5012 FORMAT( 9I8 )
        DO K = 1, 9
          IF( LINE(NCL,K) .EQ. 0 ) GO TO 35
          LMT(NCL) = K
        ENDDO
        N1=1
   31   N1=N1+9
        N2=N1+8
        call ginpt(lin,id,dlin)
        IF(ID(1:3) .EQ. 'CC2') THEN
          READ(DLIN,5012) (LINE(NCL,K),K=N1,N2)
          DO K= N1,N2
            IF(LINE(NCL,K) .EQ. 0) GO TO 35
            LMT(NCL)=K
          ENDDO
          GO TO 31
        ENDIF
        GO TO 28
   35   CONTINUE
        call ginpt(lin,id,dlin)
        GO TO 28
      ENDIF
      NCL=NCLM
      IF(NCL .GT. 0) THEN
        WRITE(LOUT,6120)
        DO 45 J = 1, NCL
          NA = LMT(J)
          IF(NA .GT. 0) THEN
            WRITE(LOUT,6125)J,(LINE(J,K),K=1,NA)
          ENDIF
   45   CONTINUE
      ELSE
        WRITE(LOUT,6115)
      ENDIF

!NiS,mar06,com: reading block for 3D-geometry

CIPK FEB01 END NEW LOCATION FOR CC LINE READ

C-
C     Read 3d geometry file
C-
      IF(IFIT .GT. 0) THEN
        READ(IFIT) N,M,NPM,NES,((CORD(J,K),SPEC(J,K),K=1,3),ALFA(J),
     2NFIX(J),NFIX1(J),AO(J),NSURF(J),J=1,N),(NDEP(J),NREF(J),J=1,NPM),
     3               ((NOP(J,K),K=1,20),NCORN(J),IMAT(J),IMAT(J),TH(J),
     4               NFIXH(J),J=1,M),(WIDTH(J),J=1,N)
C-
C     Then update from input file
C-
   50   CONTINUE
        IF(ID(1:4) .EQ. 'EL3D') THEN
          READ(DLIN,5041) J,(NOP(J,K),K=1,20),IMAT(J),TH(J)
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
          call ginpt(lin,id,dlin)
          GO TO 50
        ENDIF
C-
C...... Define NOPS the one or two-dimensional elements
C-
        DO 63 N=1,NES
          MM=0
          NCN=NCORN(N)
          DO 62 M=1,NCN
            IF(NOP(N,M) .LE. NPM) THEN
              MM=MM+1
              NOPS(N,MM)=NOP(N,M)
            ENDIF
   62     CONTINUE
          NCRN(N)=MM
   63   CONTINUE
      ELSE
C-
C-.....Read element data from input file
C-
   65   CONTINUE
        IF(ID(1:2) .EQ. 'EL') THEN
          READ(DLIN,5040) J,(NOP(J,K),K=1,8),IMAT(J),TH(J)
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
          call ginpt(lin,id,dlin)
          GO TO 65
        ENDIF
        DO I=1,MAXE
          IF(IMAT(I) .GT. 0) THEN
            DO J=1,8
              NOPS(I,J)=NOP(I,J)
            ENDDO
C
C    USE ELEMENT TYPES TO ASSIGN MANNINGS N IF VALUE LESS THAN  -1.0-
C-
!NiS,apr06: skip assignement, if the value (ORT(I,5) == -1):

cipk oct98 update to f90
            IMMT=IMAT(I)
            J=MOD(IMMT,100)
            IF(NOP(I,6) .EQ. 0) J=IMAT(I)
            IF(ORT(J,5) .GT. 1.) THEN
              CHEZ(I)=ORT(J,5)
  !NiS,apr06: skipping, if ORT(J,5)==-1:
  !          ELSE
            ELSEIF(ORT(J,5) /= -1) THEN
  !In the case of ORT(J,5)=-1 there is no assignement
              ZMANN(I)=ORT(J,5)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C-
C-.....Find max node and element numbers.....
C-
      NP = 0
      DO 75 J = 1, MAXE
        IF( IMAT(J) .GT. 0 ) THEN
          NE = J
          IF( J .LT. LE ) LE = J
          DO 70 K = 1, 20
            NCORN(J)=K-1
            IF( NOP(J,K) .EQ. 0) GO TO 72
            IF( NOP(J,K) .GT. NP ) NP = NOP(J,K)
            IF( NOP(J,K) .LT. LP ) LP = NOP(J,K)
   70     CONTINUE
          NCORN(J)=20
   72     CONTINUE
!NiS,may06: for Kalypso output increase MaxT by one for every 1D-2D-TRANSITION ELEMENT (NCORN = 5); MaxT was initialized in getgeo1.subroutine
          IF (ncorn(j) .eq. 5) MaxT = MaxT + 1
!-
          IF(IFIT .EQ. 0) NCRN(J)=NCORN(J)
C-
C....... Establish preliminary element types
C-

cWP Jan 2006, writing informations
!        write (*, 999) J, NCORN(J), IMAT(J)
!  999   format (1X, 'ELEM ', I8, ' NCORN(J) = ', I4, '  IMAT(J) = ', I4)

          IF(NCORN(J) .GT. 5) THEN
            NETYP(J)=16
          ELSEIF(IMAT(J) .GT. 900) THEN
            NETYP(J)=17
cipk jan99
C
C        Insert pointers to inform about junction elements
C

cWP Jan 2006, writing informations
        write (*, 1000)
 1000   format (/1X, 'In GETGEO. Line 391.')
          KK=NOP(J,1)
          KL=NOP(J,3)  
        write (*, 1001) KK, KL
 1001   format (1X, 'KK = ', I10, '  KL = ', I10)
          JPOINT(KK)=KL
          JPOINT(KL)=KK
          KK=NOP(J,2)
          JPOINT(KK)=KL
C

          ELSEIF(NCORN(J) .GT. 3) THEN
            NETYP(J)=18
          ELSE
            NETYP(J)=6
          ENDIF
        ENDIF
   75 CONTINUE
C-
C     For the case of 2d geometry input
C     Set NPM the number of surface nodes,
C     and NES the number of 2d elements in the surface
C-
      IF(IFIT .EQ. 0) THEN
        NPM=NP
        NEM=NE
      ENDIF
      NES=NE
      WRITE(ICFL,6210) NE,LE,NP,LP
C-
C-.....Read cord data from input file .....
C-
   80 CONTINUE
      IF(ID(1:2) .EQ. 'ND') THEN
        READ(DLIN,5035) J,(CORD(J,K),K=1,2),AO(J)
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)
        GO TO 80
      ENDIF
C-
C...... Preliminary initialisation of VEL(3,  ) for BLINE
C-
      DO 79 J=1,NPM
        VEL(3,J)=ELEV-AO(J)
        IF(VEL(3,J) .LT. HMIN) VEL(3,J)=HMIN
        IF(HMNN .LT. 0.) VEL(3,J)=-HMNN
   79 CONTINUE
C-
C...... Enter width values for one-dim nodes
C-
   85 CONTINUE
CIPK JUN05
      IF(ID(1:3) .EQ. 'WD ') THEN
cipk jun03 replace WAT with WATT for dimensional consistency
        READ(DLIN,5044) J,WATT,SL,SR,WS
        WIDTH(J)=WATT
        WIDS(J)=WS
        SS1(J)=SL
        SS2(J)=SR
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
        call ginpt(lin,id,dlin)
        GO TO 85
      ENDIF


CIPK JUN05  ENTER DATA FOR WEIRS
   86 CONTINUE
      IF(ID(1:3) .EQ. 'WDT') THEN
        READ(DLIN,5044) J,TWHT,TWLN,TRAEL
        IF(J .GT. 0) THEN
          WHGT(J)=TWHT
          WLEN(J)=TWLN
          TRANSEL(J)=TRAEL
        ELSE
CIPK SEP04
          CLOSE(75)
          OPEN(75,file='ERROR.OUT')
          WRITE(*,*) 'Error in weir data'
          WRITE(75,*) 'Error in weir data'
          STOP
        ENDIF
        WRITE(LOUT,6150) J,WHGT(J),WLEN(J),TRANSEL(J)
        WRITE(75,6150) J,WHGT(J),WLEN(J),TRANSEL(J)
 6150   FORMAT(/'WEIR ELEVATION, SECTION LENGTH AND TRANSEL FOR NODE',I6
     +,' =',3F12.3)
        call ginpt(lin,id,dlin)
        NDATLN=NDATLN+1
        GO TO 86
      ENDIF

C-
C     Skip processing of 2d geometry when 3d file readin
C-
      IF(IFIT .EQ. 0) THEN
C-
C     Process mid sides
C- 
        DO  J=1,NE
          IF(IMAT(J) . GT. 0  .AND.  IMAT(J) .LT. 901) THEN
            NCN=NCORN(J)
C-
C     Test for 1d - 2d transition
C-
            IF(NCN .EQ. 5) THEN
              N1=NOP(J,4)
              N2=NOP(J,3)
              N3=NOP(J,5)
              AO(N2)=0.5*(AO(N1)+AO(N3))
              IF(CORD(N2,1) .LE. VOID) THEN
                CORD(N2,1)=0.5*(CORD(N1,1)+CORD(N3,1))
                CORD(N2,2)=0.5*(CORD(N1,2)+CORD(N3,2))
                WIDTH(N2)=SQRT((CORD(N1,1)-CORD(N3,1))**2+
     +                     (CORD(N1,2)-CORD(N3,2))**2)
              ENDIF
              NCN=3
            ENDIF
C-
C     Now fill midside coordinates, widths etc.
C-
!NiS,may06: For net-file in Kalypso-2D-format, the midside filling is done in RDKALYPS.subroutine, skip then
            IF(IGEO .NE. 2) THEN

              DO K=2,NCN,2
                N1=NOP(J,K-1)
                N2=NOP(J,K)
                N3=MOD(K+1,NCN)
                IF(N3 .EQ. 0) N3=NCN
                N3=NOP(J,N3)
                AO(N2)=0.5*(AO(N1)+AO(N3))
                IF(WIDTH(N1) .GT. 0.  .AND.  WIDTH(N3) .GT. 0.) THEN
                  WIDTH(N2)=0.5*(WIDTH(N1)+WIDTH(N3))
                  WIDS(N2)=0.5*(WIDS(N1)+WIDS(N3))
                  SS1(N2)=0.5*(SS1(N1)+SS2(N3))
                  SS2(N2)=0.5*(SS1(N1)+SS2(N3))
                ENDIF
                IF(CORD(N2,1) .LE. VOID) THEN
                  CORD(N2,1)=0.5*(CORD(N1,1)+CORD(N3,1))
                  CORD(N2,2)=0.5*(CORD(N1,2)+CORD(N3,2))
                ENDIF
              ENDDO
            ENDIF
!-

          ENDIF
        ENDDO
C-
C     Scale coordinates
C-
        DO 112 J = 1, NP
          CORD(J,1) = CORD(J,1)*XSCALE
          CORD(J,2) = CORD(J,2)*YSCALE
          AO(J) = AO(J)*ZSCALE
CIPK JAN95 ADD A LINE
cipk mar98 remove          WIDTH(J) = WIDTH(J) * XSCALE
  112   CONTINUE
        ROAVG=A1+A2*TEMPI+A3*TEMPI**2
        IF(GRAV .LT. 32.) THEN
CIPK AUG02          CHI=CHI*239.87 
          ROAVG=ROAVG*516.
          IGF=2
        ELSE
	    CHI=CHI/239.87
        ENDIF
C
C..... Go to Marsh routine for possible input
C
        CALL MARSH
C-
C......Read number of layers at each node for 3d cases
C-
        IF(NDP .NE. 0) THEN
          IF(ID(1:2) .EQ. 'VD') THEN
CIPK SEP96 CHANGE TO READ EDD1 SEPARATELY
            READ(DLIN,5021) VMIN,POWER,UMIN,PWERIN
            WRITE(LOUT,6040) VMIN,POWER,UMIN,PWERIN
            WRITE(*,*) 'FOUND VMIN'
            NDATLN=NDATLN+1
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
            call ginpt(lin,id,dlin)
cipk sep96 add variable distribution of edd1's etc
            WRITE(LOUT,6041)
 113        IF(ID(1:2) .EQ. 'TD') THEN
              READ(DLIN,'(I8,3F8.0)') K,EDD1(K),EDD2(K),EDD3(K)
              WRITE(LOUT,6042) K,EDD1(K),EDD2(K),EDD3(K)
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
              call ginpt(lin,id,dlin)
              GO TO 113
            ENDIF
 6040 FORMAT(///9X,'BOUNDARY CONDITION FUNCTIONAL DISTRIBUTION'//
     1'  EXTERNAL BOUNDARIES'/
     25X,'MINIMUM VELOCITY',T22, F9.4/5X,'POWER FUNCTION',T21,F10.4//
     3'  INTERNAL BOUNDARIES'/
     45X,'MINIMUM VELOCITY',T22, F9.4/5X,'POWER FUNCTION',T21,F10.4//)
 6041       FORMAT(
     +      10X,'PARAMETERS FOR VERTICAL EDDY COEFFICIENTS'//
     +      5X,'MAT NO   EDDY1            EDDY2            EDDY3'//)
 6042 format(i10,3f12.4)
          ELSE
C            WRITE(*,*) 'LINE TYPE -VD- NOT FOUND'
C            WRITE(*,*) 'EXECUTION STOPPED'
C            STOP
             VMIN=1.
             POWER=1.
             UMIN=1.
             PWERIN=1.
             WRITE(LOUT,*) 'DEFAULT VD VALUES USED'
             WRITE(*,*) 'DEFAULT VD VALUES USED'
          ENDIF
          IF(NDP .LT. 0) THEN
C-
C      NDP .lt. -1 says use this value globally
C-
            IF(NDP .LT. -1) THEN
              DO 814 J=1,NP
                NDEP(J) = -NDP-1
  814         CONTINUE

            ELSE
C-
C      NDP = -1 says read values by node with weighting factors
C-
  815         CONTINUE
              WRITE(*,'(1X,A3)') ID
              IF(ID(1:3) .EQ. 'LD2') THEN
                READ(DLIN,'(2I8)') J,NTS
cipk feb99
cWP Feb 2006, Change NLAYM to NLAYMX
                if(nts .gt. nlaymx) then
CIPK SEP04 CREATE ERROR FILE
  	            CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')

cWP Feb 2006, Change NLAYM to NLAYMX	  
                 WRITE(75,*) 'Too many layers Increase NLAYMX in PARAM.'
     +            ,'COM'
                 WRITE(*,*) 'Too many layers Increase NLAYMX in PARAM.C'
     +            ,'OM'
                  stop 'Too many layers defined'
                endif

                N2=NTS
                IF(N2 .GT. 7) N2=7
                READ(DLIN,'(2I8,7F8.0)') J,NTS,(THL(N),N=1,N2)
                IF(NTS .GT. 7) THEN
cipk jan99                  N1=-2
                  N1=-1
 8151             N1=N1+9
                  N2=N1+8
CIPK JAN99
                  IF(N2 .GT. NTS) N2=NTS

cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
                  call ginpt(lin,id,dlin)
                  IF(ID(1:4) .EQ. 'LD2A') THEN 
                    READ(DLIN,'(9F8.0)') (THL(N),N=N1,N2)
                    IF(NTS .GT. N2) GO TO 8151
                  ENDIF
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
                  call ginpt(lin,id,dlin)
                ELSE
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
                  call ginpt(lin,id,dlin)
                ENDIF
C-
C      J > MAXP skips out
C-
                IF(J .GT. MAXP) GO TO 819
C-
C      Test for limit violation on MLAY parameter
C-

cWP Feb 2006, Change MLAY to NLAYMX
                IF (J .GT. NLAYMX)  THEN
CIPK SEP04 CREATE ERROR FILE
  	            CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                  WRITE(*,*)  ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
                  WRITE(75,*) ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
                  STOP
                ENDIF
CIPK JAN99 ALLOW FOR J=0
                IF(J .EQ. 0) THEN
                  DO J=1,NP

                    DO  N=1,NTS
                      THLAY(J,N)=THL(N)
                      IF(THL(N) .LE. 0.) THLAY(J,N)=1.0
                    ENDDO
                    NDEP(J)=NTS
                  ENDDO
                  GO TO 815
                ENDIF

                DO  N=1,NTS
                  THLAY(J,N)=THL(N)
                  IF(THL(N) .LE. 0.) THLAY(J,N)=1.0
                ENDDO
                NDEP(J)=NTS
                GO TO 815
              ENDIF
              GO TO 819
            ENDIF
C-
C      NDP = 1  Read a complete block of values for all 2d nodes
C-
          ELSEIF(NDP .EQ. 1) THEN
            IF(ID(1:3) .EQ. 'LD1') THEN
              READ(DLIN,5015) (NDEP(J),J=1,9)
              N1=1
  816         N1=N1+9
              N2=N1+8
              IF(N2 .GT. NP) N2=NP
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
              call ginpt(lin,id,dlin)
              IF(ID(1:4) .EQ. 'LD1') THEN
                READ(DLIN,5015) (NDEP(J),J=N1,N2)
                GO TO 816
              ENDIF
            ELSE
CIPK SEP04 CREATE ERROR FILE
  	        CLOSE(75)
              OPEN(75,FILE='ERROR.OUT')
              WRITE(*,*) 'ERROR -- NO LD1 LINES IN DATA SET'
              WRITE(75,*) 'ERROR -- NO LD1 LINES IN DATA SET'
              STOP
            ENDIF
C-
C      NDP = 2  Read layer data with elevations set
C-
          ELSEIF(NDP .EQ. 2) THEN
  817       IF(ID(1:3) .EQ. 'LD3') THEN
              READ(DLIN,'(2I8)') J,NTS
cipk feb99
cWP Feb 2006, Change NLAYM to NLAYMX
                if(nts .gt. nlaymx) then
CIPK SEP04 CREATE ERROR FILE
  	            CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                 WRITE(75,*) 'Too many layers Increase NLAYMX in PARAM.'
     +            ,'COM'
                 WRITE(*,*) 'Too many layers Increase NLAYMX in PARAM.C'
     +            ,'OM'

                  stop 'Too many layers defined'
                endif

              N2=NTS
              IF(N2 .GT. 7) N2=7
              READ(DLIN,'(2I8,7F8.0)') J,NTS,(THL(N),N=1,N2)
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
              call ginpt(lin,id,dlin)
              IF(NTS .GT. 7) THEN
CIPK JAN99                N1=-2
                N1=-1
  818           N1=N1+9
                N2=N1+8
CIPK JAN99
                IF(N2 .GT. NTS) N2=NTS
                IF(ID(1:4) .EQ. 'LD3A') THEN 
                  READ(DLIN,'(9F8.0)') (THL(N),N=N1,N2)
cipk nov97          READ(LIN,'(A8,A72)') ID,DLIN
                  call ginpt(lin,id,dlin)
                  IF(NTS .GT. N2) GO TO 818
                ENDIF
              ENDIF
              IF(J .EQ. 0) WRITE(LOUT,6007) (THL(N),N=1,NTS)
C
C    Permit reading of a single list of depths
C    when the node number is read in as zero.  A 15% margin is
C    applied at the bottom element to avoid small elements.
C
              IF(J .EQ. 0) THEN
                DO J=1,NPM
C-
C    Test for NTS =0
C
                  IF(NTS .GT. 0) THEN
                    DO N=1,NTS
                      IF(N .EQ. 1) THEN
                        IF(N .EQ. NTS) THEN
                          THLAY(J,1)=1.0
                          NDEP(J)=1
                          GO TO 8171
                        ENDIF
                        TKLAY=ELEV-THL(N)
                        IF(ELEV-TKLAY*1.15 .LT. AO(J)) THEN
                          THLAY(J,1)=1.0
                          NDEP(J)=1
                          GO TO 8171
                        ELSE
                          THLAY(J,N)= (ELEV-THL(N))/(ELEV-AO(J))
                        ENDIF
                      ELSEIF(N .EQ. NTS) THEN
                        TKLAY= THL(N-1)-AO(J)
                        THLAY(J,N)=TKLAY/(ELEV-AO(J))
                        NDEP(J)=N
                        GO TO 8171
                      ELSE
                        TKLAY=THL(N-1)-THL(N)
                        IF(THL(N-1)-TKLAY*1.15 .LT. AO(J)) THEN
                          TKLAY= THL(N-1)-AO(J)
                          THLAY(J,N)=TKLAY/(ELEV-AO(J))
                          NDEP(J)=N
                          GO TO 8171
                        ELSE
                          THLAY(J,N)= (THL(N-1)-THL(N))/(ELEV-AO(J))
                        ENDIF
                      ENDIF
                    ENDDO
                  ELSE
                    NDEP(J)=0
                  ENDIF
 8171             CONTINUE
                ENDDO
                GO TO 817
C
C     Process individual nodal values
C
              ELSEIF(J .LE. MAXP) THEN

cWP Feb 2006, Change MLAY to NLAYMX
                IF (J .GT. NLAYMX)  THEN
CIPK SEP04 CREATE ERROR FILE
  	            CLOSE(75)
                  OPEN(75,FILE='ERROR.OUT')
                  WRITE(*,*)  ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
                  WRITE(75,*) ' ERROR  ',J, '  EXCEEDS  MLAY = ', NLAYMX
                  STOP
                ENDIF
                IF(J .GT. 0) THEN
                  DO  N=1,NTS
                    IF(N .EQ. 1) THEN
                      IF(N .EQ. NTS) THEN
                        THLAY(J,1)=1.0
                        NDEP(J)=1
CIPK JUL01
                          GO TO 817
                      ELSE
                        TKLAY=ELEV-THL(N)
                        IF(ELEV-TKLAY*1.15 .LT. AO(J)) THEN
                          THLAY(J,1)=1.0
                          NDEP(J)=1
CIPK JUL01
                          GO TO 817
                        ELSE
                          THLAY(J,N)= (ELEV-THL(N))/(ELEV-AO(J))
                        ENDIF
                      ENDIF
                    ELSEIF(N .EQ. NTS) THEN
                      TKLAY= THL(N-1)-AO(J)
                      THLAY(J,N)=TKLAY/(ELEV-AO(J))
                      NDEP(J)=N
CIPK JUL01
                          GO TO 817
                    ELSE
                      TKLAY=THL(N-1)-THL(N)
                      IF(THL(N-1)-TKLAY*1.15 .LT. AO(J)) THEN
                        TKLAY= THL(N-1)-AO(J)
                        THLAY(J,N)=TKLAY/(ELEV-AO(J))
                        NDEP(J)=N
CIPK AUG00
                        GO TO 817
                      ELSE
                        THLAY(J,N)= (THL(N-1)-THL(N))/(ELEV-AO(J))
                      ENDIF
                    ENDIF
                  ENDDO
                ELSE
                  THLAY(J,1)=1.0
                ENDIF
                NDEP(J)=NTS
                GO TO 817
              ENDIF
            ENDIF
          ENDIF
        ENDIF

CIPK DEC99 ADD TEST TO SKIP PAST UNUSED 'VD' LINE
        IF(ID(1:2) .EQ. 'VD') THEN
          call ginpt(lin,id,dlin)
        ENDIF

C-
C     Save layer data on the geometric file
C-
        IF(NAINPT .GT. 0) THEN
          DO N=1,NP
            DEN(N)=AO(N)
          ENDDO
          WRITE(NAINPT)
     1      NP,NE,((CORD(J,K),K=1,2), ALFA(J),den(J),J=1,NP),
     2      ((NOP(J,K),K=1,8),IMAT(J),TH(J),NFIXH(J),J=1,NE)
     +      ,(WIDTH(J),SS1(J),SS2(J),WIDS(J),J=1,NP)

          write(nainpt) id8
          write(nainpt) (widbs(j),wss(j),j=1,np)

          IF(NDP .GT. 0) THEN
            MAXVL=0
            DO J=1,NP
              IF(MAXVL .LT. NDEP(J)) MAXVL=NDEP(J)
            ENDDO
            WRITE(NAINPT) NP,MAXVL,
     +                    (NDEP(J),(THLAY(J,K),K=1,MAXVL),J=1,NP)
          ENDIF
        ENDIF
  819   CONTINUE

C-
C       Print the NDEP values
C-
        IF(NDP .NE. 0) THEN
          WRITE(LOUT,6877) (NDEP(J),J=1,NP)
        ENDIF
C-
C     Reset NDEP AND DEN
C-
        DO 82 J=1,NP
CIPK NOV97 ADD LOGIC TO RESET MARSH PARAMETERS FOR 3-D
cipk dec9 remove AKP logic for 3-d
cipk dec99          IF(NDEP(J) .GT. 0) THEN
cipk dec99            AKP(J)=1.0
cipk dec99            ADO(J)=AO(J)
cipk aug98
cipk dec99            ADT(J)=0.
cipk dec99            ADB(J)=0.
CIPK AUG98            ADT(J)=AO(J)
CIPK AUG98            ADB(J)=AO(J)
cipk dec99          ENDIF
cipk dec99 end changes
CIPK NOV97 END CHANGES
          DEN(J)=0.
          NDEP(J)=NDEP(J)+1
   82   CONTINUE
C-
C....... Create new node for junctions of 2 and 3D systems (vertically)
C-
        DO 83 J=1,NE
          IF(NETYP(J) .EQ. 18) THEN
            K=NOP(J,3)
            IF(NDEP(K) .GT. 1) THEN
C-
C...... nop(j,19) is used to store the common node
C-
              NOP(J,19)=K
              NP=NP+1
              NOP(J,3)=NP
              NDEP(NP)=NDEP(K)
              WRITE(LOUT,6878) NP
              CORD(NP,1)=CORD(K,1)
              CORD(NP,2)=CORD(K,2)
              AO(NP)=AO(K)
              WIDTH(NP)=WIDTH(K)
              NOPS(J,3)=NP

CIPK JAN99
CIPK JUL00 REDEFINE PROPS OF ADDED POINTS
              AKP(NP)=AKP(K)
              ADO(NP)=ADO(K)
              ADT(NP)=ADT(K)
              ADB(NP)=ADB(K)
CIPK MAR00 ADD COMPUTATION OF LAYER INFO FOR NEW NODE
              DO L=1,NDEP(K)
                THLAY(NP,L)=THLAY(K,L)
              ENDDO

              NPM=NP
            ENDIF
          ENDIF
   83   CONTINUE
      ENDIF
C-
C...... Set direction for 1D - 2D junction node
C-
      DO 195 N=1,NE
cipk jan99
C
C....... Establish speical conditions for junctions when 2dv
C
        IF(IMAT(N) .GT. 900) THEN
          N1=NOP(N,1)
          IF(NDEP(N1) .GT. 1) THEN
            N1=NOP(N,1)
            N2=NOP(N,2)

C.......  Force widths and slopes equal for main stem nodes
            width(n1)=(width(n1)+width(n2))/2.
            width(n2)=width(n1)
            ss1(n1)=(ss1(n1)+ss1(n2))/2.
            ss1(n2)=ss1(n1)
            ss2(n1)=(ss2(n1)+ss2(n2))/2.
            ss2(n2)=ss2(n1)

            NCN=NCRN(N)
            DO 224 K=3,NCN
              N3=NOP(N,K)
              NFIX(N3)=01200
              JPOINT(N3)=-ABS(JPOINT(N3))
  224       CONTINUE
          ENDIF

        !nis,nov06,com: This is for 1D-2D-transition-elements

        ELSEIF (NCRN(N) .EQ. 5  .AND.  IMAT(N) .LT. 901) THEN
          !nis,com: get the two corner nodes of the coupling (2D-corners of connected 2D-element)
          N1=NOP(N,4)
          N2=NOP(N,5)
          !nis,com: calculate the x- and y-distances
          DX=CORD(N1,1)-CORD(N2,1)
          DY=CORD(N1,2)-CORD(N2,2)
          !nis,com: calculate the chord length between the two transition corner nodes
          WIDTT=SQRT(DX**2+DY**2)
cipk aug97 add fix for alfak
          ANG=ATAN2(DX,-DY)
          !nis,com: Bring vector-direction into 1. or 4. quadrant of Cartesian coordinate system
          IF(ANG .GT. 1.5707963) ANG=ANG-3.1415926
          IF(ANG .LT. -1.5707963) ANG=ANG+3.1415926
cipk aug97 end of change
          !nis,com: Get the transition node
          N3=NOP(N,3)
C-
C....... Set midside at halfway point
C-
          !nis,com: Set the midside node onto the middle of the transition-chord
          CORD(N3,1)=(CORD(N1,1)+CORD(N2,1))/2.
          CORD(N3,2)=(CORD(N1,2)+CORD(N2,2))/2.

          !nis,nov06,com: It has to be a rectangular channel at the coupling with no side-slopes!!!
          IF(SS1(N3) .NE. 0.  .OR.  SS2(N3) .NE. 0.) THEN
            WRITE(*,*) ' **ERROR**  SIDE SLOPES AT NODE',N3,' NON-ZERO'
            WRITE(*,*) '   VALUES FORCED TO ZERO'
            !nis,nov06,com: Set side slopes to zero
            SS1(N3)=0.0
            SS2(N3)=0.0
          ENDIF
          !nis,com: Resetting the width of the transition with the chord length between the transition-corner-nodes
          WIDTO=WIDTH(N3)
          WIDTH(N3)=WIDTT
cipk aug97 2nd part of change for alfak
           IF(ANG .NE. 0.) THEN
            !nis,nov06,com: Overgive the angle to the coupling node
            ALFAK(N3)=ANG
          ELSE
            !nis,nov06,com: If the angle is by accident zero, reset the direction to an increment unequal to zero, to show, that the direction is
            !               fixed! It wouldn't be handled as fixed direction, if it was zero.
            ALFAK(N3)=0.0001
          ENDIF
cipk aug97 end changes
!NiS,may06: adding alfak to output
          WRITE(*,*) ' SETTING ALFAK, WIDTH, OLD WIDTH',N3, alfak(n3)
     +                , WIDTH(N3), WIDTO
!-
!nis,nov06: Adding fixes for direction of 1D-2D-line-transitions:
        !nis,nov06: MaxLT shows the number of line-transitions within network
        ELSEIF (MaxLT.ne.0) then

          Transitiontest: do i=1,MaxLT
            !n is the actual element number in loop
            if (n.eq.TransLines(i,1)) then

              !look, whether length is non-zero
              if (lmt(TransLines(i,2)).le.0) then
               WRITE(*,*) 'Transition line has zero-length.'
               WRITE(*,*) 'Redefine it with non-zero-length!'
               WRITE(*,*) 'The problemline is: ', TransLines(i,2)
               WRITE(*,*) 'Program stopped for redefinition of line.'
               STOP       'Redefine line!'
              !look, whether length is not only one element
              elseif (lmt(TransLines(i,2)).eq.1) then
               WRITE(*,*) 'Transition consist only of one element.'
               WRITE(*,*) 'For this sort of transition use'
               WRITE(*,*) 'element-to-element-transition!'
               WRITE(*,*) 'Program stopped for transition-redefinition.'
               STOP       'Redefine transition!'
              end if

              !start- and ending-node of coupling
              N1 = Line ( TransLines(i,2) , 1)
              N2 = Line ( TransLines(i,2) , LMT (TransLines(i,2)) )

              !x- and y-distances
              DX = cord(n1,1) - cord(n2,1)
              DY = cord(n1,2) - cord(n2,2)
              !nis,jan07,testing
              WRITE(*,*) DX, cord(n1,1), cord(n2,1)
              WRITE(*,*) DY, cord(n1,2), cord(n2,2)
              !-


              !chord-length
              translength = sqrt(DX*DX + DY*DY)

              !velocity-direction of the nodes in between
              alfak_temp = atan2(Dx,-DY)

              !correction to bring it into quadrants 1 or 4
              if (alfak_temp .gt. 1.5707963) then
                alfak_temp = alfak_temp-3.1415926
              elseif (alfak_temp .LT. -1.5707963) then
                alfak_temp = alfak_temp+3.1415926
              endif
              !assign the value to the nodes of the line-coupling, the midside nodes will be filled in check.subroutine
              assigning: do j = 2, lmt(Translines(i,2))-1
                alfak(Line(Translines(i,2),j)) = alfak_temp
                !nis,jan07: Finding out what the global flow-angle does
                !testingversion: alfak(Line(Translines(i,2),j)) = alfak_temp - 3.1415926
                !-
              ENDDO assigning

              !nis,jan07,testing
              WRITE(*,*) alfak_temp, line(Translines(i,2),2)
              !pause
              !-

              !jump out of loop over all transitions
              EXIT Transitiontest
            endif
            !Assigning the directions to the CORNER-nodes of the coupling. Problem: This is in getgeo. The line-construct still consists only of
            !corner nodes of connected 2D-elements. In the subroutine check it is widened to the midside node. So the assignment changes to that
            !subroutine: CHECK.subroutine
          enddo Transitiontest
!-
        ENDIF
  195 CONTINUE

CIPK AUG03
      NPM11=NPM

	DO N=1,NP
        AORIG(N)=AO(N)
	ENDDO

      RETURN

  262 continue
CIPK SEP04 CREATE ERROR FILE
      CLOSE(75)
      OPEN(75,FILE='ERROR.OUT')

      WRITE(*,*) ' Error reading binary geometry file'
      WRITE(*,*) ' Execution terminated'
      WRITE(75,*) ' Error reading binary geometry file'
      WRITE(75,*) ' Execution terminated'
      STOP

 5015 FORMAT( 9I8)
 5016 FORMAT( 2I8,7F8.0)
 5021 FORMAT(9F8.0)
 5035 FORMAT( I8, 3E8.0)
cipk aug01 special coding for R Watanabe is 10I6
 5040 FORMAT(10I6,F8.0)
cc 5040 FORMAT(10I4,F8.0)
 5041 FORMAT(22I3,F6.0)
 5044     FORMAT(I8,4F8.0)
 6007       FORMAT(' THE FOLLOWING LAYER ELEVATIONS HAVE BEEN SPECIFIED'
     +      /(8F9.2))
 6115 FORMAT( /// 10X, 'NO CONTINUITY CHECKS REQUESTED.....' )
 6120 FORMAT(   //// 10X, 'CONTINUITY CHECKS TO BE MADE ALONG THE FOLLOW
     1ING LINES'  // 6X, 'LINE',10X,'NODES')
 6125 FORMAT( I10, 4X, 20I5 )
 6210 FORMAT( // 10X, '2D ..NETWORK INPUT COMPLETE..' //
     1  15X, 'MAX ELEMENT NUM  ', I14 / 15X, 'MIN ELEMENT NUM  ',I14 /
     2  15X, 'MAX NODE NUM  ', I17 / 15X, 'MIN NODE NUM  ' ,I17 )
 6877 FORMAT(///10X,'    INPUT VALUES OF NDEP BY NODE'/
     1          (10I5))
 6878       FORMAT(' NODE',I5,' ADDED AT 2D TO 3D JUNCTION')
      END
