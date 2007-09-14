C     Last change:  WP    3 Sep 2007    4:17 pm
cipk  last update sep 05 2006 add depostion/erosion rates to wave file
CNis  LAST UPDATE NOV XX 2006 Changes for usage of TUHH capabilities
CIPK  LAST UPDATE MAR 22 2006 ADD OUTPUT FILE REWIND and KINVIS initialization
CIPK  LAST UPDATE MAR 07 2006 CORRECT TO ADD SAND OUTPUT
cipk  LAST UPDATE aug 09 2005 correct to get BSHEAR OUTPUT
cipk  last update june 28 2005 add time series option
CIPK  LAST UPDATE JUNE 27 2005 ALLOW FOR CONTROL STRUCTURES
cipk  last update sep 06 2004 revise filename writing
CIPK  LAST UPDATE AUG 31 2004 REVISE SS PRINTING TEST
CIPK  LAST UPDATE JUN 30 2003 ADD GENT CALL
cipk  LAST UPDATE MAR 14 2003 add FREQUCY FOR OUTPUT OF RESULTS FILES AND RESTART FILES
CIPK  LAST UPDATE JAN 22 2002 SET SIDFF = 0
CIPK  NEW ROUTINE jULY 9 2001
      SUBROUTINE RMA10SUB
      !nis,feb07,testing purposes
      USE blk10
      !-
      USE BLK10MOD
      USE BLK11MOD
      USE BLKDRMOD
      USE BLKSEDMOD
      USE BLKSANMOD
      USE BLKSSTMOD
      USE WBMMODS ! djw 02/11/04
!NiS,apr06: add module for new Kalypso-2D-specific calculations
      USE PARAKalyps
!-
      !nis,feb07,testing: Writing whole matrix
      USE ParaFlow1DFE
      !-

cipk aug05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLKDR.COM'
CIPK AUG05      INCLUDE 'BLK4.COM'
CIPK AUG05      INCLUDE 'BLK11.COM'
CIPK AUG05      INCLUDE 'BLKSAND.COM'
CIPK AUG05      INCLUDE 'BLKSED.COM'
      ALLOCATABLE VSING(:,:)
cipk mar95 add a column      COMMON /SINPRE/ VSING(6,MNP)

cipk aug98 add character statement
      CHARACTER*48 FRST
      CHARACTER*6  INUM
      DIMENSION CURRENT(5),TARGT(5),IREC(40),FREC(40)
      CHARACTER*4 IPACKB(1200),IPACKT(77)
      !nis,feb07,testing: Writing matrix
      CHARACTER (LEN = 25) :: matrixname, aendname
      CHARACTER (LEN = 30) :: matrixformat
      !-
!NiS,jul06: Consistent data types for passing parameters
      REAL(KIND=8) :: VTM, HTP, VH, H, HS
!-

!nis,jan07: iostat variable for test writing purposes
      INTEGER :: teststat
!-

      DATA (IREC(I),I=1,40) / 40*0 /
      DATA (FREC(I),I=1,40) / 40*0. /
! djw 02/11/04  - Hook for WBM Routines 
      LOGICAL WBM
! djw 02/11/04  - Hook for WBM Routines

!EFa jun07, necessary for autoconverge
      extranita=50.
      temp_iteqv = 0.
      temp_iteqs = 0.
      temp_itlvl = 0.
      temp_iurvl = 0.
      temp_iurvl1 = 0.
      temp_maxnn = 0.
!-
      CALL SECOND(TA)       
      ALLOCATE (VSING(8,MAXP))
C-
c  the following variable turns:
c   0 - original RMA-10
c   1 - quadratic functions
c   2 - linear functions

C      IOPTZD = 1
CIPK DEC00 separate initialisation

      CALL INITL

C      MAXP = MNP
C      MAXE = MEL
c      IR1MAX=MEQ
      MAXN=1
      NDF=6

!nis,jun07: Moving this subroutine to a point before initl is called (so from rma10.mainroutine, nbs has no value, what leads to errors while calling zvrs from file.subroutine)
C      CALL FILE(1)


CIPK JUN05  Input control structure data
      IF(INCSTR .EQ. 20) CALL INCSTRC

CIPK JUN05  Input time series data
      IF(INTIMS .EQ. 22) CALL INTIMES

      ND1=NSCR
c      ICFL=1
c     ICFL=0
      ICFL=6
      NDFF=9
      NDG=5
      IDRYC=0
CIPK AUG95 MAKE ALPHA 1.8
      ALPHA=1.8
CIPK JUN05      ALPHA=2.0
cipk feb01 add thetcn
      thetcn=1./alpha
C-
C......INPUT GEOMETRY ETC
C-
      CALL INPUT(IBIN)

      !EFa jun07, output for autoconverge
      if (beiauto.ne.0.) then

      call autoconverge(-1.0)
  
      end if
      !-

CIPK MAR00  ADD FORMATION AND OUTPUT OF HEADER

      IF(IRMAFM .GT. 0) THEN
        WRITE(HEADER(41:60),'(2I10)') NP,NE
        HEADER(101:172)=TITLE   
        WRITE(IRMAFM) HEADER
      ENDIF

CIPK JUN02 ADD BED DATA OUTFILE HEADER

      IF(IBEDOT .GT. 0) THEN
        HEADER(1:5)='RMA11'
        WRITE(HEADER(41:60),'(2I10)') NP,NE
        HEADER(101:172)=TITLE   
        WRITE(IBEDOT) HEADER
      ENDIF

CIPK JAN03 ADD WAVE DATA OUTFILE HEADER

      IF(IWAVOT .GT. 0) THEN
        HEADER(1:5)='RMA11'
        WRITE(HEADER(41:60),'(2I10)') NP,NE
        HEADER(101:172)=TITLE   
        WRITE(IWAVOT) HEADER
      ENDIF

CIPK JUN02    ADD OUTPUT OF HEADER FOR SMS FORMAT
      
      IF(ISMSFM .GT. 0) THEN

        IREC(1) = 435
        MFLG = 120
        WRITE (ISMSFM) MFLG, IREC(1), NP, NEM
      ENDIF
CIPK AUG02
      IF(ISMSFM1 .GT. 0) THEN
        IREC(1) = 431     
        MFLG = 140
        WRITE (ISMSFM1) MFLG, IREC(1), NP, NEM
      ENDIF

CIPK JAN03
      IF(ISMSFM2 .GT. 0) THEN
        IREC(1) = 435     
        MFLG = 120
        WRITE (ISMSFM2) MFLG, IREC(1), NP, NEM
      ENDIF

      IF(ISMSFM .GT. 0  .OR.  ISMSFM1 .GT. 0  .OR.  ISMSFM2 .GT. 0) THEN
        IWRT1 = 1200
        DO I=11,1200
          IPACKB(I)='    '
        ENDDO
          IPACKB(1)='RMA '
        IPACKB(2)='IMPL'
        IPACKB(3)='EMEN'
        IPACKB(4)='TATI'
        IPACKB(5)='ON O'
        IPACKB(6)='F SM'
        IPACKB(7)='S OU'
        IPACKB(8)='TPUT'
        IPACKB(9)=' FOR'
        IPACKB(10)='MAT '

        IF(ISMSFM .GT. 0) THEN
          WRITE (ISMSFM) IWRT1, (IPACKB(I),I= 1,IWRT1)
        ENDIF
        IF(ISMSFM1 .GT. 0) THEN
          WRITE (ISMSFM1) IWRT1, (IPACKB(I),I= 1,IWRT1)
        ENDIF
CIPK JAN03
        IF(ISMSFM2 .GT. 0) THEN
          WRITE (ISMSFM2) IWRT1, (IPACKB(I),I= 1,IWRT1)
        ENDIF
        IWRT2 = 40
        IWRT3 = 40
        IF(ISMSFM .GT. 0) THEN
          WRITE (ISMSFM) IWRT2, IWRT3,
     *              (IREC(I),I=1, IWRT2), (FREC(I),I=1,IWRT3)
        ENDIF
        IF(ISMSFM1 .GT. 0) THEN
          WRITE (ISMSFM1) IWRT2, IWRT3,
     *              (IREC(I),I=1, IWRT2), (FREC(I),I=1,IWRT3)
        ENDIF
CIPK JAN03
        IF(ISMSFM2 .GT. 0) THEN
          WRITE (ISMSFM2) IWRT2, IWRT3,
     *              (IREC(I),I=1, IWRT2), (FREC(I),I=1,IWRT3)
        ENDIF
        DO I=1,77
          IPACKT(I)='    '
          IF(I .LT. 73) THEN
            IPACKT(I)(1:1)=TITLE(I:I)
          ENDIF
        ENDDO
        IWRT4 = 77
        IF(ISMSFM .GT. 0) THEN
          WRITE (ISMSFM) IWRT4, (IPACKT(I),I= 1,IWRT4)
        ENDIF
        IF(ISMSFM1 .GT. 0) THEN
          WRITE (ISMSFM1) IWRT4, (IPACKT(I),I= 1,IWRT4)
        ENDIF
CIPK JAN03
        IF(ISMSFM2 .GT. 0) THEN
          WRITE (ISMSFM2) IWRT4, (IPACKT(I),I= 1,IWRT4)
        ENDIF
      ENDIF

CIPK JUN03 SETUP STRESS WEIGHTING 

      IF(ICORDIN .NE. 0) CALL GENT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,apr06: Initializing c_wr-values out of file or to default values 1.3. input-cwr-file
!           must have the same name as geometry-file with suffix .cwr
!
!           This option is only activated, if VEGETA is entered in input file at proper place
!           Else set to (c_wr(i) = 1.0)
      IF (IVEGETATION /= 0) THEN
        WRITE(*,*)'going to cwr_init'
        CALL cwr_init
      ELSE
        DO i = 1,MaxE
          c_wr(i) = 1.0
        ENDDO
      ENDIF
!-

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-
C.......  Establish flow directions for one dimensional elements
C-
      CALL FLDIR
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 NiS,apr06: Start of STEADY STATE CALCULATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-
C......TEST FOR STEADY STATE
C-
      IF(NITI .EQ. 0) GO TO 350
      NITA=NITI
C-
C......PROCESS STEADY VALUES
C-
      !EFa jul07, necessary for autoconverge
      if (beiauto.ne.0.) then

        call autoconverge(1.)

      end if
      !-
      MAXN=0

200   MAXN=MAXN+1

      !EFa jun07, autoconverge
      if (beiauto.ne.0.) then
      
        call autoconverge(2.)

      end if
      !-

C     REWIND IVS

!NiS,sep06: For usage of boundary condition in the way of Kalypso-2D, a special counter for the number of
!           iterations in steady case is applied
      npr = maxn
!-

!nis,jun07: Moving distribution calculation to this place because of dry node handling
      if (MaxLT /= 0) call TransVelDistribution
!-

C-
C......  Process dry nodes
C-
        IF(IDSWT .NE. 0) THEN
          IF(IDRYC .EQ. 0) THEN
            WRITE(*,*) 'ENTERING REWET'
            CALL REWET
            WRITE(*,*) 'ENTERING DRY'
            CALL DEL
            IDRYC=IDSWT
          ENDIF
        ENDIF

cipk apr97  Add dropout
cipk mar01 rearrange dropout logic to allow for idrpt = 2
c          move logic into DRPOUT

      IF(IDRPT .EQ. 0) THEN

c     set IACTV to be active for all

        DO J=1,NP
          DO K=1,NDF
            IACTV(J,K)=10
          ENDDO
        ENDDO
      ELSE
        CALL DRPOUT
      ENDIF
cipk mar01 end change

cipk apr97 end change
      WRITE (*,*) 'ENTERING BLINE'
      CALL BLINE(MAXN)

      ICK=ITEQS(MAXN)+4

cipk nov99 add optional call for transitioning

CIPK JAN02 SET SIDFF TO ZERO
      DO N=1,NE
        SIDFF(N)=0.
      ENDDO

cipk aug00 experimental
      IF(ITRANSIT .EQ. 1  .and. maxn .lt. 4) CALL TWODSW

      WRITE (*,*) 'ENTERING LOAD.subroutine'
      CALL LOAD

C-
C......  Compute areas of continuity lines for stage flow input
C-
      CALL AGEN
c      DO 250 N=1,NE
c       DFCT(N)=1.
c  250 CONTINUE
      IF (ITEQV(MAXN) .NE. 5  .AND.  IOPTZD .GT. 0
     +    .AND. IOPTZD .LT. 3) CALL MELLII


      !nis,feb07,testing: Write whole matrix
      if (maxn > -1) then
        write (matrixname, '(a6,i3.3,a4)') 'matrix',maxn,'.txt'
        teststat = 0
        open (9919, matrixname, iostat = teststat)
        if (teststat /= 0) STOP 'ERROR - while opening matrix file'
      endif
      !-

      CALL FRONT(1)

      !close testfile
      close (9919, status = 'keep')
      !-

CIPK JAN97
      IF(MAXN .GE. 2) THEN
        IUTUB=1
      ENDIF
CIPK JAN97 END CHANGES

      IDRYC=IDRYC-1

      WRITE(aendname,'(a4,i3.3,a4)') 'aend', maxn, '.txt'
      open (12345, aendname)
      CALL UPDATE
      close (12345, status = 'keep')

      !EFa jul07, necessary for autoconverge
      if (exterr.eq.1.0) then

        call autoconverge(3.)

        GOTO 200

      end if
      !-

C      CALL CHECK
C     REWIND IVS
      IF(ITEQV(MAXN) .NE. 5  .AND.  ITEQV(MAXN) .NE. 2
     +  .AND.  ITEQV(MAXN) .LT. 8) CALL VRTVEL
CIPK AUG04 REVISE TEST

      IF(NPRTI .EQ. 0) THEN
        IPRTF=NITA
      ELSE
        IPRTF=NPRTI
      ENDIF
CIPK AUG04      IPRTF=IABS(NPRTF)

      IF(MOD(MAXN,IPRTF) .EQ. 0  .OR.  MAXN .EQ. NITA
     +             .OR. NCONV .EQ. 1) THEN
         CALL OUTPUT(2)

         CALL CHECK
CIPK MAR00
CIPK DEC00      ELSE
CIPK DEC00
        ENDIF

        !EFa jul07, necessary for autoconverge
        !if (beiauto.ne.0.) then
        !  if (temp_nan.eq.1.0) then
        !    maxn = 0.
        !    GOTO 200
        !  end if
        !end if
        !-

cipk apr01 This is now done in UPDATE
cipk apr01        DO  J = 1, NPM
cipk apr01          IF (IDNOPT.NE.0) THEN
cipk apr01            HS = VEL(3,J)
cipk apr01            ISWT = 0
cipk jan01  change AME to AME1
cipk apr01            CALL AMF(H,HS,AKP(J),ADT(J),ADB(J),AME1,D2,ISWT)
cipk apr01            WSLL(J) = H + ADO(J)
cipk apr01          ELSE
cipk apr01            WSLL(J) = VEL(3,J) + AO(J)
cipk apr01          ENDIF
cipk apr01          K=NREF(J)+1
cipk apr01          IF(K .NE. 1) THEN
cipk apr01            L=NREF(J)+NDEP(J)-1
cipk apr01            IF(L .GE. K) THEN
cipk apr01              DO M=K,L
cipk apr01                WSLL(M)=WSLL(J)
cipk apr01              ENDDO
cipk apr01            ENDIF
cipk apr01          ENDIF
cipk apr01        ENDDO
      
CIPK DEC00      ENDIF
CIPK MAY96 RESTORE TETT AS HOURS IN YEAR
      TETT=(DAYOFY-1)*24.+TET
      IF(NLL .GT. 0) THEN
        REWIND NLL
C      WRITE(NLL) TET,NP,NDFF,((VEL(K,J),VDOT(K,J),K=1,NDF),VVEL(J),J=1
CIPK MAY96 ADD YEAR TO FILE
        WRITE(NLL) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +            ,VVEL(J),J=1,NP)
     +     ,(hel(j),hdet(j),j=1,np)
     +     ,(DELBED(J),ELEVB(J),TTHICK(J),J=1,NP)
CIPK SEP02 ADD WRITE STATEMENT FOR ICE THICKNESS ON RESTART FILE
        IF(ICESW .GT. 0) THEN
          WRITE(NLL) (ICETHK(J),J=1,NPM)
        ENDIF
CIPK SEP02 ADD RESTART DATA FOR BED
cipk aug98 add line above for consistent restart
      ENDIF
CIPK NOV97      IF(NCONV .EQ. 1) GO TO 350
      IF(NCONV .EQ. 2) GO TO 350
  300 CONTINUE
      KRESTF=1
!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE STEADY STATE ITERATION, IF NOT CONVERGED
      IF (itefreq.ne.0) THEN
        IF (mod(maxn,itefreq).eq.0) THEN
          IF (IKALYPSOFM /= 0) THEN
            WRITE(*,*)' Entering write_Kalypso',
     +                ' steady state, after Iteration = ',maxn
            CALL write_KALYPSO
            WRITE(*,*)'back from write_kalypso'
          END IF
        ENDIF
      ENDIF
!-
!NiS,apr06: calculating the cwr-values for trees WITHIN THE ITERATION.
!           Calculation of actualized tree-parameters; file is only written in dependency of itefreq
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
!-
      IF(MAXN .LT. NITA) GO TO 200
C-
C......TEST ON MAXIMUM TIME
C-
cipk dec97  MOVE SKIP   350 IF(NCYC .GT. 0) GO TO 400
  350 CONTINUE
      if(niti .eq. 0) go to 400
!*************************************************************************DJW 04/08/04
!
!     Checks Salinity Values against maximum and minimum permissible values and resets
!     to keep within an appropriate range as appropriate
!
!*************************************************************************
      SalLowPerm = 0.0001
      SalHighPerm = 300
      Do J = 1,NP
        If (Vel(4,J).LT.SalLowPerm) Then
          Vel(4,J) = SalLowPerm
        End If
        If (Vel(4,J).GT.SalHighPerm) Then
          Vel(4,J) = SalHighPerm
        End If
      End Do
!*************************************************************************END DJW 04/08/04

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,apr06: calculating the cwr-values for trees; adding temporary simulation of maxn.eq.0
!           Updating the cwr-values for trees after convergence
        temp_maxn = maxn
        maxn = 0.
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
        maxn = temp_maxn
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if(niti .eq. 0) go to 400
C-
C......STORE AS A SINGLE PRECISION ARRAY
C-
      DO 320 J=1,NP
        DO 310 K=1,NDF
          VSING(K,J)=VEL(K,J)
  310   CONTINUE
cipk mar95 add a line to save dhdt
        VSING(7,J)=VDOT(3,J)
  320 CONTINUE

        !EFa jun07, autoconverge
        if (beiauto.ne.0.) then

          call autoconverge(4.)

          if (autoindex.eq.1.) then

            autoindex = 0.

            GOTO 200

          end if

        endif
        !-


!-
!NiS,may06,comment: Writing output files in different formats
!-
CIPK MAR00
      !NiS,apr06,comment: Writing RMA-outputfile after steady state converged solution
      IF(IRMAFM .GT. 0) THEN
        WRITE(IRMAFM) TETT,NP,NDG,NE,IYRR,((VSING(K,J),K=1,NDF),VVEL(J)
     1  ,WSLL(J),J=1,NP),(DFCT(J),J=1,NE),(VSING(7,J),J=1,NP)
      ENDIF

!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE STEADY STATE SOLUTION
      IF (IKALYPSOFM /= 0) THEN
        !NiS,may06: initializing MaxN shows subroutine write_Kalypso, that
        !           the call is for solution printing
        temp_maxn = maxn
        MAXN = 0.
        WRITE(*,*)' Entering write_Kalypso',
     +            ' for STEADY STATE SOLUTION.'
        call write_KALYPSO
        WRITE(*,*)'back from write_kalypso'
        MAXN = temp_maxn
      END IF
!-
CIPK AUG02
      !NiS,apr06,comment: Writing SMS-outputfile after steady state converged solution
      IF(ISMSFM .GT. 0) THEN
        DO JJ=1,NEM
          IMATL(JJ)=IMAT(JJ)
        ENDDO
        WRITE (ISMSFM) TETT, NPM, ((VSING(J,K),J=1,3), K = 1, NPM), 
     *                         (NDRY(K), K = 1, NPM), 
     *                    NEM,  (IMATL(JJ), JJ = 1, NEM), 
     *                         (WSLL(K), K = 1, NPM)
      ENDIF

      IF(ISMSFM1 .GT. 0) THEN
        DO JJ=1,NEM
          IMATL(JJ)=IMAT(JJ)
        ENDDO
        NQAL=3
        WRITE (ISMSFM1) TETT, NQAL,NPM,
     +   ((VSING(J,K),K=1,NPM),J=4,6),
     *   NEM,  (IMATL(JJ), JJ = 1, NEM)
      ENDIF


      !NiS,apr06,comment: writing control output file after steady state converged solution
      IF(NOPT .GT. 0) THEN
CIPK MAY96 ADD YEAR TO FILE
C       WRITE(NOPT) TET,NP,NDG,NE,((VSING(K,J),K=1,NDF),VVEL(J)
        WRITE(NOPT) TETT,NP,NDG,NE,IYRR,((VSING(K,J),K=1,NDF),VVEL(J)
     1  ,J=1,NP),(DFCT(J),J=1,NE),(VSING(7,J),J=1,NP)
      ENDIF
CIPK MAR95 ADD DHDT TO WRITE     1  ,J=1,NP),(DFCT(J),J=1,NE)
CIPK DEC97 MOVE SKIP TO DYNAMIC SOLUTION

!NiS,apr06,comment: End option, if just steady state is desired

      IF(NCYC .GT. 0) GO TO 400
      CALL ZVRS(1)

      !EFa jul07, necessary for autoconverge
      do i=1,ncl

        do k=1,3

          specccold(i,k)=specccfut(i,k)

        end do

      end do
      !-


CIPK JUL01      STOP
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,apr06: END OF STEADY STATE BLOCK!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-
C......DYNAMIC SOLUTION DESIRED
C-
C 400 NCYC=TMAX*3600./DELT+0.5
  400 CONTINUE

      !EFa jun07, autoconverge
      if (beiauto.ne.0.) then

        call autoconverge(5.)

      end if
      !-

      IDRYC=0
CIPK JUN02
      MAXN=0

!nis,sep06: Initialize the iteration counter for steady case
      NPR = maxn
!-

C-
C......LOOP ON NUMBER OF TIME STEPS
C-
        IF(LBED .GT. 0) THEN
          CALL KINVIS
          CALL SANDX
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NiS,may06: For the case of Kalypso-2D format geometry input file, there might be a later time step than the first to strart from. If so,
!           the boundary condition update is cycled till the correct boundary conditions of the step to start from is reached.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!NiS,mar06: Renaming the 800-dO-LOOP to Main_dynamic_Loop; to leave and cycle it easier.
!      DO 800 N=1,NCYC
      Main_dynamic_Loop: do n=1,ncyc
!-
!NiS,may06: calculation is only started with Kalypso-2D-geometry file, if the cycle iaccyc is reached; the user gives iaccyc in control file
!           as timestep to start from
        LaterTimestep: IF (IFILE == 60 .and.
     +                     IGEO == 2 .and. n < iaccyc) THEN
          !NiS,may06: This part of the if-clause is for simulating the update of the boundary conditions. The purpose is to read through
          !           time step date lines, that are not interesting, when starting from a later time step than the first. After reading through
          !           that data, the next time step can be read, as long as firstly the wanted starting cycle exceeds the current cycle number.
          CALL INPUTD(IBIN)
          WRITE(*,*) 'cycle main loop'
          CYCLE Main_dynamic_Loop
          !-
        ELSE
CIPK MAR06  REWIND OUTPUT FILE AT SPECIFIED INTERVAL
      IF(MOD(N,IOUTRWD) .EQ. 0) THEN
        REWIND LOUT
      ENDIF 
      write(*,*) 'starting cycle',n
CIPK JUN02
        MAXN=1
        IT=N

cipk nov99 initialize DFCT
        do j=1,ne
          dfct(j)=0.0
        enddo

CIPK SEP96 UPDATE AT START
CIPK REVISE TO SETUP HEL
        DO J=1,NP
          DO K=1,NDF
cipk dec00
            V2OL(K,J)=VDOTO(K,J)
            VOLD(K,J)=VEL(K,J)
            VDOTO(K,J)=VDOT(K,J)
            IESPC(K,J)=0
          ENDDO
          H2OL(J)=HDOT(J)
          HOL(J)=HEL(J)
          HDOT(J)=HDET(J)
        ENDDO
!NiS,may06,com: ICYC was initiated in INITL.subroutine
        ICYC=ICYC+1
C-
C...... UPDATE OF BOUNDARY CONDITIONS
C-
          CALL INPUTD(IBIN)
          

        ENDIF LaterTimestep
!-

CIPK AUG95 ADD A CALL TO UPDATE MET VALUES
cipk oct02 move heatex to use projections for heat budget
C        CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET)

        IF((LSAND .GT. 0  .OR.  LBED .GT. 0)  .AND. IWVIN .GT. 0) THEN
          CALL GETWAVE
        ENDIF

        IF(IWVFC .EQ. 102) THEN
          CALL GETSST
        ELSEIF(IWVFC .EQ. 104) THEN
          CALL GETDRSST
        ENDIF

        CALL SWANDT

CIPK AUG95 USE ALPHA=1.8
cipk dec99 test for delta = 0
        if(delt .gt. 0.) then

          ALTM=1.8/DELT
CIPK          ALTM=2.0/DELT
CIPK MAY02
          ALPHASN=1.8
          ALPHASN=2.0
        else
          altm=0.
        endif
C
C.....   If NIPT not equal to zero read velocities,depth from file
C
        IF(NIPT .GT. 0) THEN
          WRITE(*,*) 'GETTING VELS'
          DO 455 J=1,NP
            DO 452 K=1,NDF
              VOLD(K,J)=VEL(K,J)
              V2OL(K,J)=VDOTO(K,J)
              VDOTO(K,J)=VDOT(K,J)
  452       CONTINUE
cipk mar98 add logic to preserve HEL to VEL relationship
            VTM=VEL(3,J)
            CALL AMF(HEL(J),VTM,AKP(J),ADT(J),ADB(J),D1,D2,0)
            HOL(J)=HEL(J)
            HDOT(J)=HDET(J)    
  455     CONTINUE
CIPK MAY96 ADD IYYN TO LIST
          READ(NIPT) TETA,NPA,NDG,NTE,IYYN,((VSING(K,J),K=1,3)
     +   ,DUM,DUM1,DUM2,
     +    VVEL(J),J=1,NPA),(DFCT(J),J=1,NTE),(DUM,J=1,NP)
C-
C......STORE AS A DOUBLE PRECISION ARRAY
C-
          DO  J=1,NP
            DO  K=1,3
              VEL(K,J)=VSING(K,J)
            ENDDO
CIPK NOV97  457   CONTINUE
  !NiS,jul06: The usage of single and double precision waterdepth for calling of amf is used in the code. For
  !           consistency, the single precision waterdepth is replaced with double precision.
  !            CALL AMF(HEL(J),VSING(3,J),AKP(J),ADT(J),ADB(J),D1,D2,0)
            CALL AMF(HEL(J),VEL(3,J),AKP(J),ADT(J),ADB(J),D1,D2,0)
  !-
            WSLL(J) = HEL(J) + ADO(J)
          ENDDO  
CIPK NOV97  458 CONTINUE

          WRITE(*,*) 'GOING TO SANDX'
          CALL SANDX
          WRITE(*,*) 'BACK FROM SANDX'
          NDL=4
        ELSE
          NDL=1
        ENDIF
        IF (ITEQV(MAXN) .NE. 5  .AND.  IOPTZD .GT. 0
     +    .AND. IOPTZD .LT. 3 ) CALL MELLII

        DO 460 J=1,NP
cipk mar98 add logic to update HEL
          VTM=VEL(3,J)
          CALL AMF(HEL(J),VTM,AKP(J),ADT(J),ADB(J),D1,D2,0)

          DO 450 K=NDL,NDF
CIPK SEP96        VOLD(K,J)=VEL(K,J)
CIPK SEP96        VDOTO(K,J)=VDOT(K,J)
            IF(IESPC(K,J) .EQ. 0) THEN
CIPK DEC00            VEL(K,J)=VEL(K,J)+DELT*VDOT(K,J)

cipk dec00 rewrite projection approach add switch

              IF(IPROJ .EQ. 0) THEN
                VEL(K,J)=VEL(K,J)+DELT*VDOT(K,J)
              ELSEIF(IPROJ .EQ. 2) THEN
                dtfac=thetcn*vdot(k,j)+(1.-thetcn)*v2ol(k,j)
                vel(k,j)=vel(k,j)+ delt*dtfac
cc              VEL(K,J)=VEL(K,J)+VEL(K,J)-V2OL(K,J)
               VDOT(K,J)=ALTM*(VEL(K,J)-VOLD(K,J))-VDOTO(K,J)*(ALPHA-1.)
              ELSE
               VDOT(K,J)=ALTM*(VEL(K,J)-VOLD(K,J))-VDOTO(K,J)*(ALPHA-1.)
              ENDIF

            ENDIF

  450     CONTINUE

          IF(NDL .EQ. 1) THEN
            IF(IESPC(3,J) .EQ. 0) THEN

cipk dec00 add projection option

              IF(IPROJ .EQ. 0) THEN
                HEL(J)=HEL(J)+DELT*HDET(J)
                CALL AMF(HEL(J),HTP,AKP(J),ADT(J),ADB(J),D1,D2,1)
                VEL(3,J)=HTP
              ELSEIF(IPROJ .EQ. 2) THEN
                dtfac=thetcn*hdet(j)+(1.-thetcn)*h2ol(j)
                hel(j)=hel(j)+ delt*dtfac
c               HEL(J)=HEL(J)*2-H2OL(J)

cipk apr01 insert consistent argument

                VH=VEL(3,J)
                CALL AMF(HEL(J),VH,AKP(J),ADT(J),ADB(J),D1,D2,1)
              ENDIF
              IF(DELT .GT. 0.) THEN
              VDOT(3,J)=ALTM*(VEL(3,J)-VOLD(3,J))-(ALPHA-1.)*VDOTO(3,J)
              ELSE
                VDOT(3,J)=0.
              ENDIF
            ENDIF
          ENDIF
  460   CONTINUE

cipk dec00 set water surface elevation

        DO  J = 1, NPM
          IF (IDNOPT.NE.0) THEN
            HS = VEL(3,J)
            ISWT = 0
cipk jan01  change AME to AME1
            CALL AMF(H,HS,AKP(J),ADT(J),ADB(J),AME1,D2,ISWT)
            WSLL(J) = H + ADO(J)
          ELSE
            WSLL(J) = VEL(3,J) + AO(J)
          ENDIF
          K=NREF(J)+1
          IF(K .NE. 1) THEN
            L=NREF(J)+NDEP(J)-1
            IF(L .GE. K) THEN
              DO M=K,L
                WSLL(M)=WSLL(J)
              ENDDO
            ENDIF
          ENDIF
        ENDDO

CIPK APR01  NOW RESET FOR MID-SIDES
        DO K=1,NEM
          IF(NCRN(K) .GT. 0) THEN

            DO L=2,NCRN(K),2
              IF(IMAT(K) .LT. 901  .OR.  IMAT(K) .GT. 903) THEN
                J=NOPS(K,L)
                !EFa Apr07, not for mid-sides of Flow1dFE-elements
                if (j>0) then
                N1=NOPS(K,L-1)
                  IF(MOD(L,NCRN(K)) .EQ. 0) THEN
                    N2=1
                  ELSE
                    N2=NOPS(K,L+1)
                  ENDIF
                  WSLL(J) = (WSLL(N1)+WSLL(N2))/2.
                  KK=NREF(J)+1
                  IF(KK .NE. 1) THEN
                    LL=NREF(J)+NDEP(J)-1
                    IF(LL .GE. KK) THEN
                      DO M=KK,LL
                        WSLL(M)=WSLL(J)
                      ENDDO
                    ENDIF
                  ENDIF
                endif
              ENDIF
            ENDDO
          ENDIF
        ENDDO

CIPK OCT02  SET ICETHKOL
        DO J=1,NPM
          ICETHKOL(J)=ICETHK(J)
        ENDDO

CIPK JUN03

        CALL GETSTRESS
C-
C       TET=TET+DELT/3600.

        IF (LSS .GT. 0) THEN
          CALL SETVEL
cipk mar06
          CALL KINVIS
ciat mar06 adding new wbm bedshear stress subroutines for cohesive sediment calcs
!NiS,Nov06: Seems, that the name of the first shear-Subroutine is not correct. Change SHEAR1 to WSHEAR1
!          CALL SHEAR1
          CALL WSHEAR1
!-
          CALL WSHEAR2
c          CALL SHEAR
CIPK JUN97
c          CALL WSHEAR1
ciat mar06 end changes          
          CALL DEPSN
          CALL MEROSN
          CALL SEROSN
        ENDIF

C        DO NNN=1,NPM
C        WRITE(240,'(I6,6E15.5)') NNN,BSHEAR(NNN),SERAT(NNN),EDOT(NNN)
C     +   ,THICK(NNN,1),THICK(NNN,2),DEPRAT(NNN)
C        ENDDO
C-
C......ITERATION LOOP     !NiS,apr06:     starting iteration sequence
C-                        !               initialization:
        NITA=NITN         !               NITA = maximum number of iterations of timestep, local copy
        MAXN=0            !               NITN = maximum number of iterations of timestep, global value
        ITPAS=0           !               ITPAS= ???

        !EFa jun07, necessary for autoconverge
        if (beiauto.ne.0.0) then

          call autoconverge(6.)

        end if
        !-

  465   MAXN=MAXN+1       !               MAXN = actual iteration number; first initialized, then incremented

       !EFa jun07, autoconverge
       if (beiauto.ne.0.) then

         call autoconverge(7.)

       end if
       !-

cipk oct02
        IF(MAXN .EQ. 1) THEN
                write(75,*) 'going to heatex-535',n,maxn,TET,itpas
CIPK AUG05          CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
          CALL HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
          ITPAS=1
        ELSEIF(ITEQV(MAXN) .EQ. 8) THEN
                write(75,*) 'going to heatex-540',n,maxn,TET,itpas
CIPK AUG05          CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
          CALL HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
        ENDIF
C     DO 700 MAXN=1,NITN
C     REWIND IVS

!nis,jun07: Activating transition for dynamic loop
      if (MaxLT /= 0) call TransVelDistribution
!-

C-
C......  Process dry nodes
C-
        IF(IDSWT .NE. 0) THEN
          IF(IDRYC .EQ. 0) THEN
            WRITE(*,*) 'ENTERING REWET'
            CALL REWET
            CALL DEL
            IDRYC=IDSWT
          ENDIF
        ENDIF

cipk APR97  Add dropout
cipk mar01 rearrange dropout logic to allow for idrpt = 2
c          move logic into DRPOUT

        IF(IDRPT .EQ. 0) THEN

c     set IACTV to be active for all

          DO J=1,NP
            DO K=1,NDF
              IACTV(J,K)=10
            ENDDO
          ENDDO
        ELSE
          CALL DRPOUT
        ENDIF
cipk mar01 end change

cipkapr97 end changes

        CALL BLINE(MAXN)

        ICK=ITEQS(MAXN)+4

cipk nov99 add optional call for transitioning

cipk aug00 experimental
        IF(ITRANSIT .EQ. 1  .and. maxn .lt. 4) CALL TWODSW

CIPK MAY02 UPDATE SHEARS ETC

        IF((LSAND .GT. 0  .OR.  LBED .GT. 0) .and. ick .eq. 6) THEN
          CALL KINVIS
c          CALL SHEAR
          CALL SANDX
          CALL BEDXCG

        ENDIF

CIPK OCT02
  470   CONTINUE

        CALL LOAD
C-
C......  Compute areas of continuity lines for stage flow input
C-
        CALL AGEN
c      DO 470 J=1,NE
c      DFCT(J)=1.
c  470 CONTINUE
c     write(*,*) 'In main maxn,iteqv(maxn),ioptzd',maxn,iteqv(maxn)
c    +,    ioptzd
        IF (MAXN .GE. 2 .AND. ITEQV(MAXN) .NE. 5
     &      .AND. ITEQV(MAXN) .NE. 2  .AND.  IOPTZD .GT. 0
     +      .AND. IOPTZD .LT. 3) CALL MELLII
CIPK JAN97
        IF(IUTUB .EQ. 0) THEN
          IF(MAXN .GT. 2  .or. n .gt. 1) IUTUB=1
        ENDIF
CIPK JAN97 END CHANGES

        CALL FRONT(1)
        IDRYC=IDRYC-1

        CALL UPDATE

      !EFa jul07, necessary for autoconverge
      if (exterr.eq.1.0) then

        call autoconverge(8.)

        GOTO 465

      end if
      !-

C      CALL CHECK
C     REWIND IVS
        IF(ITEQV(MAXN) .NE. 5  .AND.  ITEQV(MAXN) .NE. 2
     +  .AND.  ITEQV(MAXN) .LT. 8) CALL VRTVEL



          IF(NPRTF .GT. 0) THEN

            IF(MOD(MAXN,NPRTF) .EQ. 0  .OR.  MAXN .EQ. NITA
     +        .OR. NCONV .EQ. 1) THEN

              CALL OUTPUT(2)
              CALL CHECK

            ENDIF

          ELSE

            IPRTF=IABS(NPRTF)

            IF(MOD(N,IPRTF) .EQ. 0) THEN

              IF(MAXN .EQ. NITA  .OR. NCONV .EQ. 2) THEN

                CALL OUTPUT(2)
                CALL CHECK

              ENDIF

            ENDIF

          ENDIF



C-
C......UPDATE TIME DERIVATIVE
C-
CIPK NOV97
        write(75,*) 'rma10-646'

        DO  J=1,NP

CIPK MAR98
          VTM=VEL(3,J)
          CALL AMF(HEL(J),VTM,AKP(J),ADT(J),ADB(J),D1,D2,0)
          DO  K=1,NDF
            VDOT(K,J)=ALTM*(VEL(K,J)-VOLD(K,J))-(ALPHA-1.)*VDOTO(K,J)
          ENDDO
          HDET(J)=ALTM*(HEL(J)-HOL(J))-(ALPHA-1.)*HDOT(J)
        ENDDO

  550   CONTINUE
CIPK OCT02  CHECK FOR NEGATIVE TEMPS

      IF(ICESW .GT. 0  .AND. ITEQV(MAXN) .EQ. 8  .AND. ITPAS .LT. 4)THEN
          ITPAS=ITPAS+1
          DO J=1,NPM
            IF(VEL(5,J) .LT. TMED) GO TO 570
          ENDDO
        ENDIF
        GO TO 580
  570   CONTINUE
        WRITE(75,*) 'GOING TO HEATEX-570',N,MAXN,TET,ITPAS
CIPK AUG05        CALL HEATEX(ORT,NMAT,DELT,LOUT,IYRR,TET,ITPAS)
        CALL HEATEX(NMAT,DELT,LOUT,IYRR,TET,ITPAS)
        GO TO 470

  580   CONTINUE
        IF(ITEQV(MAXN) .EQ. 8  .AND.  ITPAS .EQ. 0) ITPAS=1 
C-
C......SAVE RESTART CONDITIONS ON FILE
C-
CIPK MAY96 RESTORE TETT AS HOURS IN YEAR
        TETT=(DAYOFY-1)*24.+TET
        WRITE(75,*) 'TET,DAYOFY',TET,TETT,DAYOFY

        !NiS,apr06,comment: If convergent result files for this timestep have to be written
        IF(NCONV .EQ. 2) GO TO 750
  700   CONTINUE


C      END OF ITERATION LOOP

!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE DYNAMIC RUN ITERATION, IF NOT CONVERGED
        IF (itefreq.ne.0) THEN
          IF (MOD(maxn,itefreq).eq.0) THEN
            IF (IKALYPSOFM /= 0) THEN
            WRITE(*,*)' Entering write_Kalypso',
     +                ' dynamic at time step ', icyc+iaccyc-1,
     +                ' after Iteration = ',maxn
              call write_KALYPSO
            WRITE(*,*)'back from write_kalypso'
            ENDIF
          ENDIF
        ENDIF
!-
!NiS,apr06: calculating the cwr-values for trees WITHIN THE ITERATION.
!           Calculation of actualized tree-parameters; file is only written in dependency of itefreq
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
!-

        !NiS,apr06,comment: Start next iteration, until maximum number of iterations is reached
        IF (MAXN .LT. NITA) GO TO 465

        !NiS,apr06,comment: Writing results after the timestep
  750   CONTINUE
        write(75,*) 'rma10-668 at 750 continue'
CIPK MAY02 UPDATE BED INFORMATION FOR SAND CASE
        IF(LSAND .GT. 0) THEN
          CALL BEDSUR
        ELSEIF(LSS .GT. 0) THEN
C
C-    IF COHESIVE SED IS SIMULATED, CALCULATE BED CHANGE
C
          CALL NEWBED(2)
        ENDIF

        IF(LBED .GT. 0) THEN
        write(75,*) 'rma10-674 going to bedlbed'
          CALL BEDLBED
        ENDIF

!NiS,apr06: calculating the cwr-values for trees.
!           This option is only activated, if VEGETA is entered in input file at proper place
        temp_maxn = maxn
        maxn=0.
        IF (IVEGETATION /= 0) THEN
          CALL get_element_cwr
        END IF
        maxn=temp_maxn
!-

CIPK SEP02   LOGIC FOR RESTART FILE MOVED DOWN
C-
C......SAVE RESTART CONDITIONS ON FILE
C-
CIPK MAY96 RESTORE TETT AS HOURS IN YEAR
        TETT=(DAYOFY-1)*24.+TET
        WRITE(75,*) 'TET,DAYOFY',TET,TETT,DAYOFY
        IF(NLL .EQ. 0) GO TO 600
        REWIND NLL
C      WRITE(NLL) TET,NP,NDFF,((VEL(K,J),VDOT(K,J),K=1,NDF),VVEL(J),J=1
CIPK MAY96 ADD YEAR TO FILE

        IF(LSAND .EQ. 0   .AND.  LSS .EQ. 0) THEN
          WRITE(NLL) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NP)
     +     ,(hel(j),hdet(j),j=1,np)

        ELSEIF(LSAND .GT. 0) THEN
          WRITE(NLL) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NP)
     +     ,(hel(j),hdet(j),j=1,np)
     +   ,(DELBED(J),ELEVB(J),TTHICK(J),J=1,NP)
        ELSE
          WRITE(NLL) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NP)
     +     ,(hel(j),hdet(j),j=1,np)
     +     ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,MXSEDLAY),I=1,NP)
     +     ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J)
     &     ,SMVAL(I,J),J=1,MXSEDLAY),BEDORIG(I),I=1,NP)

C          WRITE(NLL) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
C     +           ,VVEL(J),J=1,NP)
C     +     ,(hel(j),hdet(j),j=1,np)
C     +     ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,NLAY(I)),I=1,NP)
C     +     ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J)
C     &     ,SMVAL(I,J),J=1,NLAYO(I)),I=1,NP)
        ENDIF	    
        IF(ICESW .GT. 0) THEN
          WRITE(NLL) (ICETHK(J) ,J=1,NPM)
        ENDIF
CIPK SEP02 ADD RESTART DATA FOR BED
cipk aug98 add line above for consistent restart
  600   CONTINUE

        !EFa jun07, autoconverge
        if (beiauto.ne.0.) then
          call autoconverge(9.)
          if (autoindex.eq.2.) then
            autoindex = 0.
            GOTO 465
          end if
        endif
        !-

C-
C......SAVE ON RESULTS FILE
C-

!NiS,apr06: Adding KALYPS-2D results file as option
CIPK MAR00
        IF((NOPT .GT. 0  .OR.  IRMAFM .GT. 0 .OR. IBEDOT .GT. 0
     + .OR. IWAVOT .GT. 0 .OR. ISMSFM2 .GT. 0
!     + .or. ISMSFM .gt. 0 .or. ismsfm1 .gt. 0)  .AND. N .GE. IRSAV) THEN
     + .or. ISMSFM .gt. 0 .or. ismsfm1 .gt. 0)  .AND. N .GE. IRSAV
     + .or. IKALYPSOFM > 0) THEN
!-

Cipk mar03 add option that allows output at a set frequency

          IF(MOD(N,IOUTFREQ) .EQ. 0) THEN
C-
C......STORE AS A SINGLE PRECISION ARRAY
C-
            DO 770 J=1,NP
              DO 760 K=1,NDF
                VSING(K,J)=VEL(K,J)
  760         CONTINUE
cipk mar95 add a line to save dhdt
CIPK NOV02 This is now water column potential and bed elevation
 
              VSING(7,J)=VEL(7,J)
              VSING(8,J)=AO(J)
  770       CONTINUE
CIPK MAR00
            IF(IRMAFM .GT. 0  .AND. MOD(N,NBSFRQ) .EQ. 0) THEN
         WRITE(IRMAFM) TETT,NP,NDF,NE,IYRR,((VSING(K,J),K=1,NDF),VVEL(J)
     1    ,WSLL(J),J=1,NP),(DFCT(J),J=1,NE),(VSING(7,J),J=1,NP)
            ENDIF
C       Output RMA results file contains
 
c       1   time in hours (Julian)
c       2   number of nodes
c       3   obsolete counter of degrees of freedom (set to 5) NDF=6
c       3a  number of elements
c       4   year
c       5   VSING subscript(1)  x-vel by node
c       6   VSING subscript(2)  y-vel by node
c       7   VSING subscript(3)  depth by node
c       8   VSING subscript(4)  salinity by node
c       9   VSING subscript(5)  temperature by node
c      10   VSING subscript(6)  sus-sed by node
c      11   VVEL                w-vel by node
c      12   WSLL                water surface elevation
c      13   DFCT                stratification multiplier by element
c      14   VSING subscript(7)  water column potential by node

!NiS,apr06: write Kalypso-2D format result/restart file at: THE END OF THE DYNAMIC RUN
            IF (IKALYPSOFM /= 0) THEN
              MAXN = 0.
              WRITE(*,*)' Entering write_Kalypso',
     +            ' after dynamic time step ', icyc+iaccyc-1
              call write_KALYPSO
              WRITE(*,*)'back from write_kalypso'
            END IF
!-
CIPK AUG02
            IF(ISMSFM .GT. 0  .AND. MOD(N,NBSFRQ) .EQ. 0) THEN
              DO JJ=1,NEM
                IMATL(JJ)=IMAT(JJ)
              ENDDO
            WRITE (ISMSFM) TETT, NPM, ((VSING(J,K),J=1,3), K = 1, NPM), 
     *                         (NDRY(K), K = 1, NPM), 
     *                    NEM,  (IMATL(JJ), JJ = 1, NEM), 
     *                         (WSLL(K), K = 1, NPM)
            ENDIF

            IF(ISMSFM1 .GT. 0  .AND. MOD(N,NBSFRQ) .EQ. 0) THEN
              DO JJ=1,NEM
                IMATL(JJ)=IMAT(JJ)
              ENDDO

CIPK DEC02 REVISE TO ADD BED ELEVATION AS 7TH COMPONENT
cipk aug05 correct to get BSHEAR OUTPUT
!            NQAL=7
            NQAL=9 ! djw 16/08/04 To enable addition of wave force x & wave force y
            NQAL=11 ! djw 16/08/04 To enable addition of bed form height and roughness
            !
            !  Initiates Bed form info if necessary ! djw 2/11/04
            !
            WBM = .FALSE.
            IF (WBM) THEN
!NiS,Nov06: Mixing logical type with arithmetic is not possible in Lahey
!	        IF (wbm_Initiated.EQ..FALSE.) THEN
               IF (.not.wbm_Initiated) THEN
!-
                CALL BedRoughInitiate(NPM,wbm_Initiated,wbm_MannTrans,
     +          wbm_NodeCounter,wbm_IT, wbm_MannTransOld, wbm_BedHeight)
               END IF
            END IF
            IF (WBM) THEN
              WRITE (ISMSFM1) TETT, NQAL,NPM,       
     +          ((VSING(J,K),K=1,NPM),J=4,7),(VSING(8,K),K=1,NPM),
     +         (DELBED(J),J=1,NP),(BSHEAR(J),J=1,NPM), 
!
!  DJW 16/08/04 writing wave force x and y coordinates to file : Constituents 8 and 9.
!
     +         (STRESS(J,1),J=1,NPM),(STRESS(J,2),J=1,NPM),
     +         (wbm_MANNTRANSOLD(J),J=1,NPM),
     +         (wbm_BedHeight(J), J=1,NPM),
!
!  DJW 16/08/04  end djw 16/08/04
!
     *                    NEM,  (IMATL(JJ), JJ = 1, NEM) 
            ELSE
              NQAL = 9
              WRITE (ISMSFM1) TETT, NQAL,NPM,
     +          ((VSING(J,K),K=1,NPM),J=4,7),(VSING(8,K),K=1,NPM),
     +          (DELBED(J),J=1,NP),(BSHEAR(J),J=1,NPM), 
!
!  DJW 16/08/04 writing wave force x and y coordinates to file : Constituents 8 and 9.
!
     +          (STRESS(J,1),J=1,NPM),(STRESS(J,2),J=1,NPM),
!
!  DJW 16/08/04  end djw 16/08/04
!
     +          NEM,  (IMATL(JJ), JJ = 1, NEM) 
            END IF
          ENDIF
c     1   TETT                Time in hours (Julian)
c     2   NQAL                Number of constituents = 7
c     3   NP                  Number of nodes
c     4   VSING subscript(4)  salinity by node   		CONST 1
c     5   VSING subscript(5)  temperature by node		CONST 2
c     6   VSING subscript(6)  water column concentration by nodeCONST 3
c     7   VSING subscript(7)  water column potential by node	CONST 4
c     8   VSING subscript(8)  Bed elevation by node		CONST 5
c     9   DELBED              Bed change by node		CONST 6
c    10   BSHEAR              Shear stress by node		CONST 7
            IF(ISMSFM2 .GT. 0  .AND. MOD(N,NBSFRQ) .EQ. 0) THEN
              DO JJ=1,NEM
                IMATL(JJ)=IMAT(JJ)
              ENDDO
              TETT=(DAYOFY-1)*24.+TET
              IF(LSAND .GT. 0) THEN
                WRITE (ISMSFM2) TETT, NPM 
     +         ,(WAVEHT(J)*COS(WAVEDR(J))
     +         ,WAVEHT(J)*SIN(WAVEDR(J))
     +         , TRRAT(J),J=1,NPM)
     +         ,(NDRY(K),K=1,NPM)
     +         , NEM,  (IMATL(JJ), JJ = 1, NEM) 
     +         ,(WSLL(J),J=1,NPM)
              ELSEIF(LSS .GT. 0) THEN
                WRITE (ISMSFM2) TETT, NPM 
     +     ,    (SWH(J)*COS(WVDR(J))
     +     ,     SWH(J)*SIN(WVDR(J))
     +         , AWL(J),J=1,NPM)
     +         ,(NDRY(K),K=1,NPM)
     +         , NEM,  (IMATL(JJ), JJ = 1, NEM) 
     +         ,(WSLL(J),J=1,NPM)
              ENDIF
            ENDIF
C
CIPK MAY96 ADD YEAR TO FILE
C       WRITE(NOPT) TET,NP,NDG,NE,((VSING(K,J),K=1,NDF),VVEL(J)
CIPK AUG02 TEST FRO SAVE FREQUENCY
            IF(NOPT .GT. 0  .AND. MOD(N,NBSFRQ) .EQ. 0) THEN
          WRITE(NOPT) TETT,NP,NDF,NE,IYRR,((VSING(K,J),K=1,NDF),VVEL(J)
     1    ,J=1,NP),(DFCT(J),J=1,NE),(VSING(7,J),J=1,NP)
            ENDIF
CIPK MAR95 ADD DHDT TO WRITE     1  ,J=1,NP),(DFCT(J),J=1,NE)

C     Output results file contains

c     1   time in hours (Julian)
c     2   number of nodes
c     3   obsolete counter of degrees of freedom (set to 5) NDF=6
c     3a  number of elements
c     4   year
c     5   VSING subscript(1)  x-vel by node
c     6   VSING subscript(2)  y-vel by node
c     7   VSING subscript(3)  depth by node
c     8   VSING subscript(4)  salinity by node
c     9   VSING subscript(5)  temperature by node
c    10   VSING subscript(6)  sus-sed by node
c    11   VVEL                w-vel by node
c    12   DFCT                stratification multiplier by element
c    13   VSING subscript(7)  water column potential by node
            IF(IBEDOT .GT. 0) THEN
              NQL=10
cipk aug05 correct to get BSHEAR OUTPUT
CIPK MAR06 CORRECT TO GET SAND OUTPUT
              if(LSS .GT. 0) THEN
                WRITE(IBEDOT) TETT,NQL,NP,IYRR
     +           ,((VSING(K,J),J=1,NP),K=1,2),(VVEL(J),J=1,NP)
     +           ,(VSING(3,J),J=1,NP),(WSLL(J),J=1,NP)
     1           ,((VSING(K,J),J=1,NP),K=6,8),
     +           (BEDEL(J)-BEDORIG(J),J=1,NP),(BSHEAR(J),J=1,NP)
                IF(IDEBUG .EQ. 3) THEN
                  do j=1,np
                    write(236,8888)j,BEDEL(J),BEDORIG(J),DELBED(J)
     +              ,(BEDEL(J)-BEDORIG(J)),ao(j),aorig(j),bshear(j)
 8888               format(i8,2f15.5,2e15.5,2f15.5,e15.5)
                  enddo
                ENDIF
              ELSE
                WRITE(IBEDOT) TETT,NQL,NP,IYRR
     +          ,((VSING(K,J),J=1,NP),K=1,2),(VVEL(J),J=1,NP)
     +          ,(VSING(3,J),J=1,NP),(WSLL(J),J=1,NP)
     1          ,((VSING(K,J),J=1,NP),K=6,8),
     +          (DELBED(J),J=1,NP),(BSHEAR(J),J=1,NP)
                IF(IDEBUG .EQ. 3) THEN
                  do j=1,np
                    write(236,8888)j,
     +              DELBED(J),ao(j),aorig(j)            
                  enddo
                ENDIF
              ENDIF
            ENDIF

            IF(IWAVOT .GT. 0) THEN
              IF(LSAND .GT. 0) THEN
                NQL=6
                WRITE(IWAVOT) TETT,NQL,NP,IYRR
     +     ,    (WAVEHT(J)*COS(WAVEDR(J)),J=1,NP)
     +     ,    (WAVEHT(J)*SIN(WAVEDR(J)),J=1,NP),(VVEL(J),J=1,NP)
     +     ,    (VSING(3,J),J=1,NP),(WSLL(J),J=1,NP),(TRRAT(J),J=1,NP)
              ELSEIF(LSS .GT. 0) THEN
cipk sep06 allow for new params
                NQL=11
                WRITE(IWAVOT) TETT,NQL,NP,IYRR
     +     ,    (SWH(J)*COS(WVDR(J)),J=1,NP)
     +     ,    (SWH(J)*SIN(WVDR(J)),J=1,NP),(VVEL(J),J=1,NP)
     +     ,    (VSING(3,J),J=1,NP),(WSLL(J),J=1,NP)
     +     ,    (SPWP(J),J=1,NP),(AWL(J),J=1,NP),(SWH(J),J=1,NP)
     +     ,    (serat(J),J=1,NP),(DEPRAT(J),J=1,NP),(edot(j),j=1,np)
cipk sep06 add line above     
C     +     ,    (edot(j)*delt/vel(3,j),j=1,np)

C     SPWP(N)    Spectral peak wave period (hours)

C     AWL(N)     Average wave length (m)

C     SWH(N)     Significant wave height (m)

C     WVDR(N)    Wave direction measure counter clockwise from the x -axis.

              ENDIF
            ENDIF

C     Output bed results file contains

c     1   TETT                Time in hours (Julian)
c     2   NQL                 Number of constituents
c     3   NP                  Number of nodes
c     4   IYRR                Year
c     5   VSING subscript(1)  x-vel by node
c     6   VSING subscript(2)  y-vel by node
c     7   VVEL                z-velocity
c     8   VSING subscript(3)  depth by node
c     9   WSLL                water surface elevation by node
c    10   VSING subscript(6)  water column concentration by node 1
c    11   VSING subscript(7)  water column potential by node 2
c    12   VSING subscript(8)  Bed elevation by node 3
c    13   DELBED              Bed change by node 4
cipk aug05 correct to get BSHEAR OUTPUT
c    14   BSHEAR              Shear stress by node 5

CIPK AUG98
C  Save restart file every IOURST timesteps
            IF (MOD(N,IOUTRST) .EQ. 0)  THEN
              CLOSE (131)
CIPK SEP04
!              WRITE(INUM,'(I4.4)') N
              WRITE(INUM,'(I6.6)') N ! Override djw 31/10/05 enables write of more than 9990 TS restart file
              FRST=FNAM(1:LNNAM) // 'RST'//INUM//'.RST'
              OPEN(131,FILE=FRST,FORM='UNFORMATTED',STATUS='UNKNOWN')
              IF(LSAND .EQ. 0   .AND.  LSS .EQ. 0) THEN
                WRITE(131) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NP)
     +          ,(hel(j),hdet(j),j=1,np)

              ELSEIF(LSAND .GT. 0) THEN
                WRITE(131) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NP)
     +           ,(hel(j),hdet(j),j=1,np)
     +           ,(DELBED(J),ELEVB(J),TTHICK(J),J=1,NP)
              ELSE
                WRITE(131) TETT,NP,NDFF,IYRR,((VEL(K,J),VDOT(K,J),K=1,7)
     +           ,VVEL(J),J=1,NP)
     +          ,(hel(j),hdet(j),j=1,np)
     +          ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,MXSEDLAY),I=1,NP)
     +          ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J)
     &          ,SMVAL(I,J),J=1,MXSEDLAY),BEDORIG(I),I=1,NP)

              ENDIF	    
              IF(ICESW .GT. 0) THEN
                WRITE(131) (ICETHK(J) ,J=1,NPM)
                ENDIF
CIPK SEP02 ADD RESTART DATA FOR BED
            ENDIF 

          ENDIF
        ENDIF
!NiS,may06: Renaming 800-DO-Loop to Main_dynamic_Loop
!  800 CONTINUE

      ENDDO Main_dynamic_Loop
!-
      CALL ZVRS(1)
CIPK JUL01      STOP
      RETURN
      END
