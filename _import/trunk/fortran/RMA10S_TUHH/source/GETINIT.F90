module mod_getinit
contains
!     Last change:  WP   18 Jun 2008    4:23 pm
!     Last change:  NIS   5 May 2008   11:42 pm
!IPK  LAST UPDATE APRIL 05 2006 ADD MULTUPILE ENTRIES SO THAT SS INITIAL CONDITIONS CAN BE READ
!ipk  last update FEB 11 2004  add IOV option
!ipk  last update sEP 30 2002 ADD READ OF ICETHICKNESS FROM RESTART FILE
!ipk  last update Dec 6 1999 allow for INIT data
!ipk  last updated Aug 7 1998 revise restart format
!IPK  LAST UPDATED MAR 23 1998
!IPK  LAST UPDATED DEC 5 1997
!IPK  LAST UPDATED NOVEMBER 13 1997
!IPK  LAST UPDATE MAY 1 1996
SUBROUTINE GETINIT(IBIN,ITIMESINIT, m_SimModel)
use mod_Model
use mod_RDKalypso_routines
USE BLK10MOD
USE BLK11MOD
USE BLKDRMOD
USE BLKSEDMOD
USE BLKSANMOD
USE BLKFTMOD
!NiS,may06: Module for Kalypso-Use
USE paraKALYPS
USE Para1DPoly
!-
!********************** DJW 20/07/04
USE WBMMODS
!**********************
SAVE
!-
!ipk aug05      INCLUDE 'BLK10.COM'
!IPK MAR98
!IPK AUG05      INCLUDE 'BLK11.COM'
!IPK AUG05      INCLUDE 'BLKDR.COM'
!IPK AUG05      INCLUDE 'BLKSAND.COM'
!IPK AUG05      INCLUDE 'BLKSED.COM'
!-
!IPK APR06

type (simulationModel), pointer :: m_SimModel
REAL (kind = 8) :: VDUM
DIMENSION TV(3),TVD(3)
!NiS,apr06: adding variable for inquire statement and dummy for calling RDKALYPS to Restart
CHARACTER(LEN=20)    :: inquiretest
INTEGER              :: dummy(9), j
!-
!NiS,jul06: Declaring variable types for proper call of amf
REAL (KIND = 8)      :: VAAA, HTP, H, HS
REAL (KIND = 8)      :: tmpdepth
!-

!nis,jul07:
INTEGER         :: TNode, LiNo, LiLe, na
REAL (KIND = 8) :: waspi, TDep, TDepv
!-


!-
!-.....INPUT INITIAL CONDITIONS, IF SPECIFIED.....
!-

!******************************************************************************************************
!RESTARTING, BECAUSE RESTART FILE UNIT IS GREATER 0; RESTARTING, BECAUSE RESTART FILE UNIT IS GREATER 0
!******************************************************************************************************

IF (NB > 0 .and. NB < 100) then

  NXX = NB
  NB = IABS(NB)

  !NiS,apr06: Making an INQUIRE-test for finding the format of restarting; Restarting from Kalypso-2D-file means FORMATTED file
  INQUIRE (nb, FORM = inquiretest)

  RESTARTTEST: IF (inquiretest == 'FORMATTED' .and. nb < 100) THEN
    WRITE(*,*) ' Going to RDKALYPSO for restarting'
    call RDKALYPS (dummy(1), dummy(2), dummy(3), dummy(4), dummy(5), dummy(6), dummy(7), dummy (8), dummy (9), 2, m_SimModel)
    npx = maxp-1
    ndx = 9

  ELSE

!IPK APR06   READ FOR SUS SED DATA ONLY

    IF (ITIMESINIT /= 1) THEN

      REWIND NB
      READ (NB) TETT, NPX, NDX, IYRR
      REWIND NB
      WRITE (*,*) TETT, NPX, NDX, IYRR, NDF
      IF (LSAND == 0 .AND. LSS == 0) THEN
        READ (NB) TETT, NPX, NDX, IYRR, ((VDUM, VDUM1, K = 1, 7) , VVELDUM, J = 1, NPX) , (helDUM, hdetDUM, j = 1, npx)

      ELSEIF (LSAND > 0) THEN
        READ (NB) TETT, NPX, NDX, IYRR, ((VDUM, VDUM1, K = 1, 7) , VVELDUM, J = 1, NPX) , (helDUM, hdetDUM, j = 1, npx), &
        &    (DELBED (J), ELEVB (J), TTHICK (J), J = 1, NPX)
      ELSE
        IF (INEWBED == 0) THEN
          READ (NB) TETT, NPX, NDX, IYRR,                                             &
          &    ((VDUM, VDUM1, K = 1, 7) , VVELDUM, J = 1, NPX),                       &
          &     (helDUM, hdetDUM, j = 1, NPX),                                        &
          &     (NLAY (I), (THICK (I, J), SST (I, J), J = 1, MXSEDLAY) , I = 1, NPX), &
          &     (NLAYO (I), (THICKO (I, J), GBO (I, J), SSTO(I, J),  SMVAL (I, J), J = 1, MXSEDLAY), BEDORIG (I), I = 1, NPX)
        ELSE
          READ (NB) TETT, NPX, NDX, IYRR
          REWIND (NB)
          READ (NB) TETT, NPX, NDX, IYRR, ((VDUM, VDUM1, K = 1, 7), VVELDUM, J = 1, NPX), (helDUM, hdetDUM, j = 1, NPX)
        ENDIF
      ENDIF
      RETURN
    ENDIF


!IPK APR06 READ FOR MAIN DATA BUT DO NOT READ SS VALUES

    REWIND NB
    READ (NB) TETT, NPX, NDX, IYRR
    REWIND NB
    WRITE (*,*) TETT, NPX, NDX, IYRR, NDF

    IF (LSAND == 0 .AND. LSS == 0) THEN
      READ (NB) TETT, NPX, NDX, IYRR, ((VEL (K, J), VDOT (K, J), K = 1, 7), VVEL (J), J = 1, NPX), (hel (j), hdet (j), j = 1, npx)

    ELSEIF (LSAND > 0) THEN
      READ (NB) TETT, NPX, NDX, IYRR, ((VEL (K, J), VDOT (K, J), K = 1, 7), VVEL (J), J = 1, NPX), &
      &         (hel (j), hdet (j), j = 1, npx), &
      &         (DELBED (J), ELEVB (J), TTHICK (J), J = 1, NPX)

    ELSE
      IF (INEWBED == 0) THEN
        READ (NB) TETT, NPX, NDX, IYRR, ((VEL (K, J), VDOT (K, J), K = 1, 7), VVEL (J), J = 1, NPX), &
        &         (hel (j), hdet (j), j = 1, npX)
!IPK APR06     +     ,(NLAY(I),(THICK(I,J),SST(I,J),J=1,MXSEDLAY),I=1,NPX)
!IPK APR06     +     ,(NLAYO(I),(THICKO(I,J),GBO(I,J),SSTO(I,J)
!IPK APR06     &     ,SMVAL(I,J),J=1,MXSEDLAY),BEDORIG(I),I=1,NPX)
      ELSE
        READ (NB) TETT, NPX, NDX, IYRR
        REWIND (NB)
        READ (NB) TETT, NPX, NDX, IYRR, ((VEL (K, J), VDOT (K, J), K = 1, 7), VVEL (J), J = 1, NPX), &
        &         (hel (j), hdet (j), j = 1, npX)
      ENDIF
    ENDIF



!IPK SEP02 ADD RESTART DATA FOR BED
!IPK SEP02 ADD ICE THICKNESS READ IF NEEDED
!ipk aug98 add line above for consistent restart

    IF (ICESW > 0) THEN
      READ (NB) (ICETHK (J), J = 1, NPM)
    ENDIF
  ENDIF RESTARTTEST

  !introducing the restart values for interpolated profiles
  do i = 1, maxp
    if (IntPolProf (i) .and. vel (1, i) == 0.0D0) then
    !if (IntPolProf (i)) then
      do j = 1, 3
        vel (j, i) = (1.0D0 - kmWeight(i)) * vel (j, NeighProf(i, 1)) + kmWeight(i)  * vel (j, NeighProf(i, 2))
        vold (j, i) = (1.0D0 - kmWeight(i)) * vold (j, NeighProf(i, 1)) + kmWeight(i)  * vold (j, NeighProf(i, 2))
        vdot (j, i) = (1.0D0 - kmWeight(i)) * vdot (j, NeighProf(i, 1)) + kmWeight(i)  * vdot (j, NeighProf(i, 2))
        vdoto (j, i) = (1.0D0 - kmWeight(i)) * vdoto (j, NeighProf(i, 1)) + kmWeight(i)  * vdoto (j, NeighProf(i, 2))
      end do
    end if
  end do

  !Interpolate variables at midside nodes, if they were not present
  Assign1DMidsidesValues: DO i = 1, MaxE
    IF (nop (i, 1) == 0) CYCLE Assign1DMidsidesValues
    IF (IsPolynomNode (nop (i, 1))) THEN
      IF (vel (1, nop (i, 1)) /= 0.0 .and. vel (1, nop (i, 2)) == 0.0) THEN
        DO j = 1, 2
          vel (j, nop (i, 2)) = 0.5 * (vel (j, nop (i, 1)) + vel (j, nop (i, 3)))
        ENDDO
      ENDIF
    ENDIF
  ENDDO Assign1DMidsidesValues

  !ipk FEB04  add IOV option
  !nis,may08: IOV is option to overwrite date of result, that has been read in before
  IF (IOV == 1) THEN
    IYRR = IYKK
    DAYOFY = IDTM
    TET = TTEM
  ELSE
  !ipk MAY96 decode TET
  !        IDAY=TETT/24.
  !        DAYOFY=IDAY
  !        TET=TETT-DAYOFY*24.
  !        DAYOFY=DAYOFY+1
  ENDIF

      !control output into outfile
  write (LOUT, 6240) iyrr, dayofy, tet
 6240 FORMAT(//' RESTART FOR YEAR', I6,'  DAY', I5,'  HOUR', F8.2)


  !ipk nov02 set bed to input level
  do N = 1, npx
  !IPK APR03
    !nis,may08,com
    !HEL (N)    = 'real' depth over slot elevation ADO at node N
    !VEL (3, N) = 'virtual' depth over slot elevation ADO at node N (flow active region)
    !VAAA       = VEL (3, N)
    !AKP (N)    = porosity
    !ADT (N)    = upper border depth of transition range
    !ADB (N)    = lower border depth of transition range
    !ADO (N)    = slot bottom elevation, where HEL (N) is measured above
    !D1, D2     = dummies to fill dummy list; no meaning here

    VAAA = VEL (3, N)
    !calculate the real water depth over ADO (hsig > h; switch = 0)
    CALL AMF (HEL (N), VAAA, AKP (N), ADT (N), ADB (N), D1, D2, 0)

!IPK JUN05
    !corrections (probably, if sediment layer is used)
    IF (LSAND > 0) THEN
      DIFF = ELEVB (N) - AO (N)
      AO (N) = ELEVB (N)
      ADO (N) = ADO (N) + DIFF
    ENDIF

    !current water surface elevation
    WSLL (N) = HEL (N) + ADO (N)
    !HEL(N)=WSLL(N)-ADO(N)
    !calculate back the virtual water depth (h > hsig; switch = 1)
    CALL AMF (HEL (N), VAAA, AKP (N), ADT (N), ADB (N), D1, D2, 1)
    VEL (3, N) = VAAA
  enddo


!IPK DEC97 DEVELOP VALUE FOR HEL
!ipk aug98 read in instead
!ipk aug98      DO J=1,NPX
!ipk aug98        HTP=VEL(3,J)
!ipk aug98        CALL AMF(HEL(J),HTP,AKP(J),ADT(J),ADB(J),D1,D2,0)
!ipk aug98      ENDDO

  !  *** CALL PRESR TO HAVE DENSITIES FOR MELLII FIRST HOT START ITER
  CALL PRESR

  DO N = 1, NP
    DO K = 1, NDF
      VDOTO (K, N) = VDOT (K, N)
      VOLD (K, N) = VEL (K, N)
    enddo
  enddo

  IF (NPX < NP) THEN
    SCFC = 6./ (UMIN + 1. + 4. * (UMIN + (1. - UMIN) * 0.5**PWERIN))
    NodeLoop: DO M = 1, NPM
      IF (NDEP (M) == 1) CYCLE NodeLoop
      NL = NREF (M) + 1
      NT = NL + NDEP (M) - 2

      DO N = NL, NT
        FCTV (N) = SCFC * (UMIN + (1. - UMIN) * CORD (N, 3)**PWERIN)
        DO K = 1, NDF
          FA = FCTV (N)
          IF (K == 3) FA = 1.0
          VEL (K, N) = VEL (K, M) * FA
          VOLD (K, N) = VEL (K, M) * FA
          VDOT (K, N) = VDOT (K, M) * FA
          VDOTO (K, N) = VDOT (K, M) * FA
        ENDDO
        VVEL (N) = VVEL (M)
      ENDDO

      FCTV (M) = SCFC * (UMIN + (1. - UMIN) * CORD (M, 3)**PWERIN)

      DO K = 1, NDF
        FA = FCTV(M)
        IF (K == 3) FA=1.0
        VEL (K, M) = VEL (K, M) * FA
        VOLD (K, M) = VEL (K, M) * FA
        VDOT (K, M) = VDOT (K, M) * FA
        VDOTO (K, M) = VDOT (K, M) * FA
      ENDDO
    ENDDO nodeloop
  ENDIF


!*******************************************************************************************************************
!RESTARTING FROM CONTROL FILE RESTARTING FROM CONTROL FILE RESTARTING FROM CONTROL FILE RESTARTING FROM CONTROL FILE
!*******************************************************************************************************************

ELSEIF (NXX > 100) then

  ReadControlRestarts: do
    READ (IBIN, 5032) M, TV, TVD
    !end data line reached?
    IF (M >= 9999) EXIT ReadControlRestarts
    !assign values
    DO K = 1, NDF
      VEL (K, M) = TV (K)
      VDOT (K, M) = TVD (K)
    ENDDO
  ENDDO ReadControlRestarts



!*************************************************************************************************************
!NO RESTART NO RESTART NO RESTART NO RESTART NO RESTART NO RESTART NO RESTART NO RESTART NO RESTART NO RESTART
!*************************************************************************************************************

!-
!-.....INITIALIZE PRIMARY STATE VARIABLE ARRAY.....
!-
ELSEIF (NB == 0) then

!nis,may08,com:
!ELEV1      = initial water surface elevation given by the user in the control file
!HMIN       = minimum (virtual) water depth given by the user in the control file
!
!VEL (1, J) = velocity in global x-direction at node J
!VEL (2, J) = velocity in global y-direction at node J
!VEL (3, J) = virtual water depth above ADO at node J; flow active region
!HEL (J)    = real water depth above ADO at node J
!HOL (J)    = real water depth above ADO from previous calculation run (previous time step; at the beginning HOL becomes the same as HEL)
!ADO (J)    = slot bottom elevation at node J
!WSLL (J)   = water surface elevation at node J; sum of ADO and HEL or given value ELEV1

  AssignInitialValues: DO J = 1, NP
    !initializing the velocities at every node
    VEL (1, J) = 0.00
    VEL (2, J) = 0.00
!IPK NOV9T	VEL(3,J) = ELEV - AO(J)
!IPK NOV97	IF(VEL(3,J) .LT. HMIN) VEL(3,J)=HMIN
!IPK DEC99 CHANGE TP ELEV1
    !calculating the real water depth over ADO
    HEL (J) = ELEV1 - ADO (J)
    !calculating the virtual water depth over ADO
    CALL AMF (HEL (J), HTP, AKP (J), ADT (J), ADB (J), D1, D2, 1)
    VEL (3, J) = HTP
    !checking whether virtual water depth is above given lower bound of minimum water depth for calcualtion (given by the user)
    IF (VEL (3, J) < HMIN) VEL (3, J) = HMIN
    !copying the current real water depth over ADO to the HOL-field; that means old value from previous time step is the same as current one
    HOL (J) = HEL (J)
    !TOASK; Why should one give -HMNN (which is just the original to HMIN in the input subroutine?)
    IF (HMNN < 0.) VEL (3, J) = -HMNN
    !copying the current virtual water depth over ADO to the VOLD-field; that means old value from previous time step is the same as current one
    VOLD (3, J) = VEL (3, J)
!ipk mar05
        !remember the water surface elevation (at the beginning, it comes from the user's input value ELEV1)
    WSLL (J) = ELEV1
!     VEL(4,J)=0.
!     IF(ICK .EQ. 1) VEL(4,J)=TEMP

    VEL (4, J) = SALI
    VEL (5, J) = TEMPI
    VEL (6, J) = SEDI
    VEL (7 ,J) = SEDI

    VOLD (4, J) = SALI
    VOLD (5, J) = TEMPI
    VOLD (6, J) = SEDI
    VOLD (7, J) = SEDI

    GPB (J, 1) = SEDI
  ENDDO AssignInitialValues

!********************************************************************djw 20/07/04
!
!     WBM Initital Salinity Sediment and Temperature Hook
!
  If (wbm_InitCons) Then
           
!
!     Read in Mid File of network and then extract Values from the Salinity Column Assigning Values to Nodes as 
!     We Go.
!
    Write (*,*) "Assigning Initial Conditions from Mid File"

    Allocate (wbm_NewConditions(Size(VEL,1),Size(VEL,2)))
    wbm_NewConditions = VEL
    Call wbm_AssignInits
    VEL = wbm_NewConditions
    Do J = 1,NP
      VOLD(4,J) = VEL(4,J)
      VOLD(5,J) = VEL(5,J)
      VOLD(6,J) = VEL(6,J)
      VOLD(7,J) = VEL(7,J)
    End Do
    Deallocate (wbm_NewConditions)
  End If
!********************************************************************end djw 20/07/04


!-
!...... Set up 1-D velocities
!-
  Assign1DVelos: DO N = 1, NE
    IF (IMAT (N) == 0) CYCLE Assign1DVelos
    !TOASK: Why is this here present and to lines later, same logic is deacitvated; the logic beyond is never reached for values > 905
    !       Probably following line has to be deactivated, because otherwise control structures don't get initial velocity values
    IF (IMAT (N) > 900) CYCLE Assign1DVelos
    IF (NOP (N, 6) > 0) CYCLE Assign1DVelos
!ipk revised nov06	IF(IMAT(N) .GT. 900) GO TO 173
    IF (IMAT (N) > 900 .and. IMAT (N) < 904) CYCLE Assign1DVelos


    !for transitions only the first (corner node) and the second (midside) node must get a velocity; the rest is set up by 2D-part of transition
    IF (NCRN (N) == 5) THEN
      NLM = 2
    ELSE
      NLM = 3
    ENDIF

    Get1DInits: DO M = 1, NLM

      !get node and set initial velocity
      NA = NOP (N, M)
      VEL (1, NA) = UNOM

!***************************************************
!nis,may08: Restructuring of this code is necessary!
      !skip midsides of polynomial approach
      if (m == 2 .and. imat (n) == 89) CYCLE Get1DInits

      !testing for polynomial nodes
      if (imat (n) == 89 .and. (.not. IntPolProf(na))) then
        vel (3, na) = 0.5 * (hhmin (na) + hhmax (na))

      ELSEIF (imat(n) == 89 .and. IntPolProf (na)) then
        VEL (3, na) = kmWeight (na)             * 0.5 *(hhmin (NeighProf (na, 1)) + hhmax (NeighProf (na, 1))) &
        &             + (1.0D0 - kmWeight (na)) * 0.5 *(hhmin (NeighProf (na, 2)) + hhmax (NeighProf (na, 2)))
      !fill vel(3,*) midsides for polynomial approach
      ENDIF

      !get the midside value
      if (m == 3) then
        vel(3, NOP(n, 2)) = (vel (3, nop(n, 1)) + VEL (3, nop(n, 3))) / 2.0d0
      end if
!nis,may08: Restructuring of this code is necessary!
!***************************************************

    ENDDO Get1DInits
  ENDDO Assign1DVelos

ENDIF


!TODEL: This fixitation of the vel(2,*)-field is not necessary?
!      DO 1780 N=1,NP
!        VEL(2,N)=0.1
! 1780 CONTINUE


!-
!-..... INITIALIZE FOR BOUNDARY CONDITIONS.....
!-
CALL BFORM(0)


!TODEL: These direction fixitations are done in BFORM probably
      IF(NB .EQ. 0) THEN
        if (unom /= 0.0d0) then
          estimateVelocities: DO N=1,NP
            IF(VEL(1,N) .NE. 0.) cycle estimateVelocities
            IF(NFIX(N)/1000 .EQ. 0) THEN
                VEL(1,N)=UNOM*COS(UDIR)
                VEL(2,N)=UNOM*SIN(UDIR)
            ELSE
                VEL(1,N)=UNOM*COS(ALFA(N))
                VEL(2,N)=UNOM*SIN(ALFA(N))
            ENDIF
          enddo estimateVelocities
        endif 
      ENDIF
!-


!ipk dec00 set water surface elevation
DO  J = 1, NPM
  !if MARSH-option is active
  IF (IDNOPT /= 0) THEN
    HS = VEL (3, J)
    ISWT = 0
!ipk jan01  change AME to AME1
    CALL AMF (H, HS, AKP (J), ADT (J), ADB (J), AME1, D2, ISWT)
    WSLL (J) = H + ADO(J)
  !MARSH-option not active
  ELSE
    WSLL (J) = VEL(3,J) + AO(J)
  ENDIF

  !try, if there are some referenced nodes, that means 3D-application; for the referenced nodes, the WSLL becomes the same value
  K = NREF (J) + 1
  IF (K /= 1) THEN
    L = NREF (J) + NDEP (J) - 1
    IF (L >= K) THEN
      DO M = K, L
        WSLL (M) = WSLL (J)
      ENDDO
    ENDIF
  ENDIF
ENDDO

!IPK APR01  NOW RESET FOR MID-SIDES
DO K = 1, NEM
  IF (NCRN (K) > 0) THEN

    DO L = 2, NCRN (K), 2

      IF ((IMAT (K) < 901 .AND. imat (k) > 0) .OR. IMAT(K) > 903) THEN
        J = NOPS (K, L)
        N1 = NOPS (K, L - 1)
        IF (MOD (L, NCRN (K)) == 0) THEN
          N2 = 1
        ELSE
          N2 = NOPS (K, L + 1)
        ENDIF

        WSLL (J) = (WSLL (N1) + WSLL (N2)) / 2.

        KK = NREF (J) + 1
        IF (KK /= 1) THEN
          LL = NREF (J) + NDEP (J) - 1
          IF (LL >= KK) THEN
            DO M = KK, LL
              WSLL (M) = WSLL (J)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
ENDDO

!nis,may08: update transition line initial depths
TransitionDepths: do j = 1, MaxLT

  !get transitioning 1D-node; it shows whether transition is operative in this run
  TNode = TransLines (j, 3)
  if (TNode == 0) CYCLE TransitionDepths

  LiNo  = TransLines (j, 2)
  LiLe  = lmt (LiNo)
  waspi = 0.0

  !calculate 2D's line average water level into waspi
  call getLineAverageWaterLevel (LiNo, waspi)

  !Assign water depth to nodes of transition
  if (idnopt == 0) then
    do k = 1, LiLe
      na = line(LiNo, k)
      if (vel(3,na) > 0.0) then
        Vel(3,na) = waspi - ao(na)
        WSLL(na)  = waspi
      endif
    end do
  else
    do k = 1, LiLe
      !get node
      na = line(LiNo, k)
      !if node is not deactivated
      if (vel(3,na) > 0.0) then
        !get virtual depth
        tmpdepth = waspi - ado(na)
        !calculate Calculation depth
        CALL amf (tmpdepth, vel(3,na), akp(na), adt (na), adb (na), dum1, dum2, 1)
        !take water surface elevation
        WSLL(na) = waspi
      endif
    end do
  end if
end do TransitionDepths

RETURN
 5032  FORMAT( I5,6E10.0 )
END subroutine

end module