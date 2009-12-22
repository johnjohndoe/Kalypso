!     Last change:  MD    8 Jun 2009    3:11 pm
!IPK  LAST UPDATE SEP 6 2004  add error file
!IPK  LAST UPDATE AUG 22 2001 REORGANIZE CONVERGENCE TESTING
!IPK  LAST UYPDATE APRIL 03  2001 ADD UPDATE OF WATER SURFACE ELEVATION
!IPK  LAST UPDATE DEC 21 2000 ALLOW FOR GATE STRUCTURE
!IPK  last update Jan 16 2000 fix bug from SL condition
!IPK  LAST UPDATED NOVEMBER 13 1997
!IPK  LAST UPDATE APR 30 1996
SUBROUTINE UPDATE (schwarzIt)
USE IHILMOD
USE EPORMOD
USE BLK10
USE BLK10MOD
USE BLK11MOD
USE BLKDRMOD
!EFa jun07, autoconverge
USE PARA1DPoly
USE parakalyps
!-
SAVE

!NiS,jul06: Consistent data types for passing paramters
REAL(KIND=8) H, H1, VT, HS
integer (kind = 4), intent (in) :: schwarzIt
!-
!nis,feb07: Cross section of Flow1dFE elements
REAL (KIND=8) :: NodalArea
!nis,sep07: Remember problematic node
character (len = 96) :: outputfilename, inputfilename

INTEGER :: problematicNode
!-
!IPK MAY02 EXPAND TO 7
real (kind = 8), dimension (1:7) :: EMAX, EAVG, EPercMax
integer, dimension (1:7) :: NMX, NRel
!-
!nis,apr08: NKCONV is no more used!
CHARACTER*4 IVAR(7,2)
!-
DATA IVAR/'(X-F','(Y-F','(DEP','(SAL','(TEM','(SED','(BED', 'LOW)','LOW)','TH) ',')   ','P)  ',')   ',')   '/

DATA ITIMS/0/

!-
!-.....SETUP FOR SOLUTION CORRECTIONS.....
!-


!The following block is done in the first iteration, where the convergency parameters have to be set up
!MAXN     : current iteration number
!NCNV (J) : shows, whether a particular degree of freedom J is converged or not
!  NCNV (J) = 9999 : initialized
!  NCNV (J) = 1    : active and not converged
!  NCNV (J) = 0    : active and fully converged
!NCONV    : global switch, that says, whehther model is converged in the time step
!  NCONV = 0 : model is not converged
!  NCONV = 1 : model is converged up to the variables, that had to be calculated to the iteration maxn; there might be another iteration, that
!              has to be done later in the time step, where another variable is active and is not converged
!  NCONV = 2 : all variables of the time step are fully converged

IF (MAXN == 1) THEN

!IPK aug01
!IPK MAY02 EXPAND TO 7

  Initalize0: DO K=1,7
    NCNV (K) = 9999
  ENDDO Initalize0

  !TOASK
  !nis,jan07: NITN is always the number of iterations in unsteady state. For steady state this would be NITI. The generalization for this
  !           loop would be using the NITA which is always a copy of the actual number
  !DO I=1,NITN
  DO I=1,NITA

    !hydrodynamic calculation and considering the salinity (vx, vy, h, csal)
    IF (ITEQV (I) == 0) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (3) = 1
      NCNV (4) = 1

    !classical hydrodynamic calculation (vx, vy, h)
    ELSEIF (ITEQV (I) == 1) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (3) = 1

    !only calculate the salinity concentration (csal)
    ELSEIF (ITEQV (I) == 2) THEN
      NCNV (4) = 1

    !don't consider the water depth to be changed (vx, vy, csal)
    ELSEIF (ITEQV (I) == 3) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (4) = 1

    !only hydrodynamics, without water depth to be changed (vx, vy)
    ELSEIF (ITEQV (I) == 4) THEN
      NCNV (1) = 1
      NCNV (2) = 1

    !hydrodynamics with temperature to be considered (vx, vy, h, T)
    ELSEIF (ITEQV (I) == 6) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (3) = 1
      NCNV (5) = 1

    !hydrodynamics with sediment concentration to be considered (vx, vy, h, csed)
    ELSEIF (ITEQV (I) == 7) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (3) = 1
      NCNV (6) = 1
    !  NCNV (7) = 1  !MD!MD!MD neu

    !only temperature to be considered (T)
    ELSEIF (ITEQV (I) == 8) THEN
      NCNV (5) = 1

    !only sediment concentration to be considered (csed)
    ELSEIF (ITEQV (I) == 9) THEN
      NCNV (6) = 1
    !  NCNV (7) = 1  !MD!MD!MD neu

!IPK MAY02
    !TOASK
    !nis,com: Is this 3D?
    ELSEIF (ITEQV (I) == 10) THEN
    !  NCNV (7) = 1

!IPK MAY02     ELSEIF (ITEQV (I) == 10) THEN
    !don't consider water depth to be changed for temperature calculations (vx, vy, T)
    ELSEIF (ITEQV (I) == 11) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (5) = 1

!IPK MAY02     ELSEIF (ITEQV (I) == 11) THEN
    !don't consider water depth to be changed for sediment concentration calculations (vx, vy, csed)
    ELSEIF (ITEQV (I) == 12) THEN
      NCNV (1) = 1
      NCNV (2) = 1
      NCNV (6) = 1
     ! NCNV (7) = 1  !MD!MD!MD neu
    ENDIF
  ENDDO

  !testoutput into output.out
  WRITE (75, *) 'NCNV', MAXN, (NCNV (I), I = 1, 7)
!IPK AUG01 END UPDATE

ENDIF

!testoutput into output.out
WRITE (75, *) 'NCNV', MAXN, (NCNV (I), I = 1, 7)

!Calculate current under relaxation factor
URFC = 1.0 - FLOAT (IURVL (MAXN)) * 0.1

!IPK MAY02 EXPAND TO 7
!EAVG (J) : average changes of degree of freedom J
!EMAX (J) : maximum changes of degree of freedom J
!EPercMax (J) : maximum changes of degree of freedom J referring to the absolute value, i.e. percentage of change
!NMX (J)  : node, where maximum absolute changes of degree of freedom J occurs
!NRel (J) : node, where maximum relative changes of degree of freedom J occurs
Initialize2: DO J = 1, 7
  !initialize fields for maximum and average changes calculated in present iteration
  EAVG (J) = 0.0
  EMAX (J) = 0.0
  EPercMax(J) = 0.0
  !initialize node of maximum changes
  NMX (J)  = 0
  NRel (J) = 0
ENDDO Initialize2


!-
!-.....COMPUTE SOLUTION CORRECTIONS....
!-
NCONV = 1
IF (NDF > 4) THEN
  NDFM = 4
ELSE
  NDFM = NDF
ENDIF

UpdateDOFs: DO KK = 1, NDFM
  K = KK
  COUNT = 0.0
  NP = NPSAV
  NE = NESAV
  IF (KK > 3) K = ICK

!IPK dec97 define urfcc
!URFCC : correction of the under relaxation factor depending on the variable sclae VSCALE (J) of the degree of freedom J
  URFCC = 1.0

  !Itest shows later in the loop the first occuring node of the current degree of freedom K
  ITEST = 0

  !run through all the nodes an update them for the current degree of freedom K
  UpdateNodes: DO J = 1, NP

    !TOASK
    !nis,sep07: If node is deactivated because element is deactivated, it might still have invalid WSLL, therefore refresh that WSLL
    IF (NDRY (J) == 2) then
      WSLL (J) = ado(j) + 0.0
    endif

    !don't work on deactivated degrees of freedom
    IF (NBC (J, KK) <= 0) CYCLE UpdateNodes

      !get the number of the equation/ global degree of freedom
      I = NBC (J, KK)

      !at the first occurence of the degree of freedom K it is assumed as being converged; this will change later, if it was not like this
      !ITEST : switch for showing, that it is the first node, where current degree of freedom K occurs
      !NCNV (K) : switch that shows, whether degree of freedom K is converged or not
      IF (ITEST == 0) THEN
!IPK AUG01 WE HAVE A NEW ACTIVE CONSTITUENT ASSUME ITS CONVERGED
        NCNV (K) = 0
        ITEST = 1
      ENDIF
      COUNT = COUNT + 1.0

!
!IPK dec97 add factor for distributed elevation
!
      IF (K == 1) THEN
        IF (VSCALE (J) == 0.) THEN
          URFCC = 1.0
        ELSE
          URFCC = VSCALE (J)
        ENDIF
      ENDIF

      !get the changes at the global degree of freedom I
      !EX     : local variable for changes at global degree of freedom I
      !R1 (I) : global residual vector, which shows the changes at global degree of freedom I
      !urfc   : under relaxation factor given by the user
      !urfcc  : under relaxation factor correction given by the scaling of the model
      EX = R1 (I) * URFC * urfcc

!IPK dec97 end changes
      !for converging check purposes the absolute change is measured
      !AEX : absolute change of current degree of freedom K
      !EAVG (K): cummulation of the absolute changes of the degree of freedom K; will be divided by the count later, to get the average changes
      AEX = ABS (EX)
      EAVG (K) = EAVG (K) + AEX

!IPK jun05
      !the maximum changes are stored, as well as the location, where it applies; the sign of the maximum change is always kept by not using aex
      !for the depth degree of freedom (K = 3), this can't be easily done, because marsh algorithm has to be considered
      if (k /= 3) then
        !this happens at reacitvated nodes
        if (abs(vel (k, j)) <= 1.0e-7) then
          EPercMax (k) = 5.0
          NRel (k) = j
        elseif (abs(aex/ vel(k, j)) > abs(EPercMax (k))) then
          EPercMax (k) = ex/ vel (k, j)
          NRel (k) = j
        endif

        !store maximum absolute changes
        IF (AEX >= ABS (EMAX (K))) then
          EMAX (K) = EX
          NMX  (K) = J
        ENDIF
      endif

      !special case for ITEQV == 5, which means ...
      IF (ITEQV (MAXN) == 5) THEN
        IF (K == 1) FCA = UDST (J)
        IF (K == 2) FCA = VDST (J)
        IF (K == 3) FCA = 1.0
        IF (K >= 4) FCA = SDST (J)

      !for normal hydrodynamics (ITEQV (MAXN) /= 5) ...

      !scale the change value EX by factor FCTV (J) of FCTS (J) for velocities or constituents; nothing for depth
      !FCTV (J) : ???
      !FCTS (J) : ???

      !for velocities (K = 1 or K = 2)
      ELSEIF (K < 3) THEN
        FCA = FCTV (J)
      !for constituents (K > 4)
      ELSEIF (K >= 4) THEN
        FCA = FCTS (J)
      !for depth (K = 3)
      ELSE
        FCA = 1.0
      ENDIF
      EX = EX * FCA


      !UPDATE VELOCITIES WITH FIXED DIRECTION ALFA (J)
      !***********************************************
      IF (K <= 2 .and. ALFA (j) /= 0.0) then

        !ALFA (J) : fixed direction for flow at node J; that reduces number of degrees of freedom from 2 velocities to 1
        !ADIF (J) : In special cases the flow direction must be different from defined ALFA, this difference angle is called ADIF (J)

        !TOASK: Can there be any node, that has ADIF /= 0 and ALFA == 0; probably not, because ADIF implies difference to angle ALFA
        IF (ADIF (J) /= 0.) EX = EX / COS (ADIF (J))

        IF (K == 1) THEN
          VEL (K, J)     = VEL (K, J)     + EX * COS (ALFA (J))
          VEL (K + 1, J) = VEL (K + 1, J) + EX * SIN (ALFA (J))
        ELSE
          VEL (K - 1, J) = VEL (K - 1, J) - EX * SIN (ALFA (J))
          VEL (K,     J) = VEL (K, J)     + EX * COS (ALFA (J))
        ENDIF

      !UPDATE WATER DEPTH CONSIDERING UNDER CIRCUMSTANCES OF THE MARSH ALGORITHM
      !*************************************************************************
      ELSEIF (K == 3) then

!IPK APR05


        !TOASK: What has the no transformation option to do with the effective porosity used in the marsh approach
        !IF (inotr == 1) THEN
        !TOASK: Why should should the changes EX be adapted by the effective porosity, although it is already the change on the calculation depth?
        if (idnopt < 0 .and. (.not. IsPolynomNode (J))) then
          !calculate the porosity of the Marsh range; although it should be already remembered
          H1 = VEL (3, J)
          CALL AMF (H, H1, AKP (J), ADT (J), ADB (J), AAT, D1, 0)

          !nis,jan09: After lots of test, it seems as the scaling with the effective porosity is the correct one;
          !           It's not totally clear, because the equations are partly set up with the transformed depth
          !           and partly with the real depth.
          EX = EX * EFPOR
        ENDIF

        !calculate the changes to be applied on the water stage
        VN = VEL (3, J) + EX


!
!.......Check sign of depth change
!
        !nis,com: akp is the effective porosity; this value is set to 1.0 in Marsh.subroutine, if Marsh option is
        !         not in use
        !nis,may08: Make sure, that the node is not fetched into the Marsh-Algorithm, if it is a 1D node with polnyomial approach
        !IF (AKP (J) > 0.9999) THEN
        IF (AKP (J) > 0.9999 .or. IsPolynomNode (J)) THEN
          VEL (3, J) = VN

        !if Marsh option is acitve
        ELSE
!
!.......Test for results passing through transition points
!
          ADTT = ADT (J) - (ADB (J) + ADT (J)) / 2. * (1. - AKP (J))
          ADBB = ADB (J) * AKP (J)
          IF (EX < 0.) THEN
            IF (VEL (3, J) > ADTT .AND. VN < ADTT ) THEN
              vel (3, j) = adtt - 0.00001
            ELSEIF (VEL (3, J) > ADBB .AND. VN < ADBB ) THEN
               VEL (3, J) = ADBB - 0.00001
            ELSE
              VEL (3, J) = VN
            ENDIF
          ELSE
            IF (VEL (3, J) < ADBB .AND. VN > ADBB ) THEN
              VEL (3, J) = ADBB + 0.00001
            ELSEIF (VEL (3, J) < ADTT .AND. VN > ADTT ) THEN
              VEL (3, J) = ADTT + 0.00001
            ELSE
              VEL (3, J) = VN
            ENDIF
          ENDIF
        ENDIF

!IPK jun05
      !calculate the changes for the water depths, average as well as maximum changes, considering the transformations from the marsh algorithm
      !made before the following lines
      horg = hel (j)
      !nis,may08: differ between application of Marsh approach
      if (idnopt == 0 .or. IsPolynomNode (J)) then
        hel (j) = VEL (3, J)
      else
        vt = vel (3, j)
        CALL AMF (HEL (J), VT, AKP (J), ADT (J), ADB (J), D1, D2, 0)
      end if

      !calculate the changes in water depth
      aex = hel (j) - horg


      !check, whether changes are more than current maximum changes until now
      
      !store relative nodal water depth changes 
      if (horg == 0.0) then
        if (5.0 > EPercMax (k)) then
          EPercMax (k) = 5.0
          NRel (k) = j
        endif
      else
        if (abs(aex)/horg > EPercMax (k)) then
          EPercMax (k) = aex/ horg
          NRel (k) = j
        endif
      endif 

      !store absolute maximum water depth changes
      IF (ABS(AEX) > ABS (EMAX (K))) THEN
        !remember the maximum depth changes and the node where it applies
        emax (k) = aex
        NMX (K) = J
      ENDIF

!IPK APR01 UPDATE WATER SURFACE ELEVATION

      !calculate the water stage
      IF (IDNOPT == 0 .or. IsPolynomNode (J)) THEN
        WSLL (J) = VEL (3, J) + AO (J)
      ELSE
        HS = VEL (3, J)
        ISWT = 0
        CALL AMF (H, HS, AKP (J), ADT (J), ADB(J), AME1, D2, ISWT)
        WSLL (J) = H + ADO (J)
      ENDIF

!IPK MAY02 ALLOW FOR ICK=7

    !UPDATE VELOCITIES WITHOUT FIXED DIRECTION (ALFA == 0) AND ALL THE OTHER CONSTITUENTS
    !************************************************************************************
    ELSE

      !update variable 7 (???)
      IF (K == 7) THEN
        GAN (J) = GAN (J) + EX

      !update variables 1 and 2 (without direction restriction) and 4 to 6
      ELSE
        VEL (K, J) = VEL (K, J) + EX
      ENDIF
    ENDIF
  ENDDO UpdateNodes


  !calculate the average changes, if there was no active node, then average changes are forced to be very small
  IF (COUNT == 0.) COUNT = 1.E20
  EAVG (K) = EAVG (K) / COUNT

  !If there is any change above the convergency border, then degree of freedom (NCNV) and full model (NCONV) is not converged
  if (percentCheck == 1) then
    if (abs (EPercMax (k)) > conv (k) .and. abs(EMAX(k)) > 0.0006) then
      NCONV = 0
      ncnv (k) = 1
    endif
  else
    IF (ABS (EMAX (K)) > CONV (K) ) then
      NCONV    = 0
      NCNV (K) = 1
    endif
  endif

ENDDO UpdateDOFs

!testoutput into output.out
WRITE (75, '(''NCNV-160'',9i5)') MAXN, (NCNV (I), I = 1, 7), nconv


!EFa jun07, autoconverge
if (beiauto > 0.) rss (maxn) = SQRT (eavg (1)**2 + eavg (2)**2)


!-
!-.....OUTPUT RESULTS OF CHANGES.....
!-
!IPK MAY02 EXPAND TO 7
WriteDOFOutputs: DO J = 1, 7

  !write first line including informations about time step and iteration cycle
  IF (J == 1) THEN
    !write header of output block in console
    write (*, 6011) schwarzIt, icyc, maxn
    write (*, 6012)
    if (percentCheck == 1) then
      write (*, 6014)
    else
      write (*, 6013)
    endif
  ENDIF   

  IF (nmx (j) == 0 .or. nRel (j) == 0) THEN
    WRITE (*, 6010) J, EAVG (J), EMAX (J), NMX(j),         0.0d0, EPercMax (J), NRel (J),          0.0d0, IVAR (J, 1), IVAR (J, 2)
  ELSEIF (kmx (nmx (j)) /= -1.0) THEN
    WRITE (*, 6010) J, EAVG (J), EMAX (J), NMX(j), kmx (nmx (J)), EPercMax (J), NRel (J), kmx (nRel (J)), IVAR (J, 1), IVAR (J, 2)
  ELSE
    WRITE (*, 6010) J, EAVG (J), EMAX (J), NMX(j),         0.0d0, EPercMax (J), NRel (J),          0.0d0, IVAR (J, 1), IVAR (J, 2)
  ENDIF
ENDDO WriteDOFOutputs

!TODO: Format descriptor does not work

!Writing header lines into iteration output file
IF (ITIMS == 0) THEN
  ITIMS = 1
  WRITE (LITR, 6041)
 6041 format ('#', 21x, '<--', 6x, 'MAX CHANGES', 96x, '-->', '<--', 6x, 'AVE CHANGES', 40x, '-->',/ &
      &       '#', 3x, 'TIME STEP IT  NSZF', 5x, 'X-VEL   NODE', 5x, 'Y-VEL   NODE', 5x, 'DEPTH   NODE', 7x, 'SAL   NODE', &
      &   6x, 'TEMP   NODE', 7x, 'SED   NODE', 5x, 'Z-VEL   NODE', 4x, 'X-VEL', 4x, 'Y-VEL', 4x, 'DEPTH', 6x, 'SAL', 5x, 'TEMP', &
      &   6x, 'SED', 5x, 'Z-VEL')
ENDIF

!empty line before next time step iteration block and title of that block
IF(MAXN == 1) THEN
  WRITE (LITR, '(a)') '#'
  IF (icyc == 0) THEN
    WRITE (LITR, '(a)') '# steady block'
  ELSE
    WRITE (LITR, '(a,i5,a)') '# ', icyc, '. time step block'
  ENDIF
ENDIF

!Write iteration output per iteration
!TOASK
!Changed format descriptor from 6 to 7. What is actually the 7th degree of freedom?
!IPK MAY02 EXPAND TO 7
if (percentCheck == 0) then
  WRITE (LITR, 6040) TET, ICYC, MAXN, NSZF, (EMAX (J), NMX (J), J = 1, 7), (EAVG (J), J = 1, 7)
else
  WRITE (LITR, 6040) TET, ICYC, MAXN, NSZF, (EPercMax (J), NRel (J), J = 1, 7), (EAVG (J), J = 1, 7)
endif
6040  FORMAT (F8.1, I7, I5, I9, 7(F12.4, I9), 7(F12.4))

!nis,jan08: flushing that output: KALYPSO-GUI reads the data simutanously. Information must be latest therefore
call FLUSH (LITR)


!TEST, WHETHER TIME STEP OR STEADY STEP IS FULLY CONVERGED FOR ALL VARIABLES
!***************************************************************************

!IPK AUG01     TEST FOR CONVERGENCE THIS ITERATION, IF SO ARE WE FULLY CONVERGED?
!Converged up to this iteration in the time step, now check, whether there is an open variable, still to be solved.
IF (NCONV == 1) THEN

!IPK MAY02 EXPAND TO 7
  !run through all degrees of freedom
  DOFs: DO MB = 1, 7

    !if degree of freedom is not converged (NCNV (J) == 1), then look for
    IF (NCNV (MB) == 1) THEN
      AllPossIterations: DO
        !if last possible iteration is reached and no settings showing other variable is active,
        !that is not converged, is reached, exit the loop
        IF(MAXN >= NITA) EXIT DOFs

        !look whether there is any iteration in the time step, where another variable is active for the calcualtion; if so, then of course
        !the time step is not converged
        IF (ITEQV (MAXN + 1) == ITEQV (MAXN) .AND. ITEQS (MAXN + 1) == ITEQS (MAXN)) THEN
          MAXN=MAXN+1
        ELSE
          EXIT DOFs
        ENDIF

      END DO AllPossIterations
    ENDIF

    !only if this place is reached in the last run, then the model is fully converged for the current time step
    IF (MB == 7) NCONV=2

  ENDDO DOFs

!IPK FEB03    RESET ALL FOR NON-CONVERGENCE
ELSE
  DO MB = 1, 7
    IF (NCNV (MB) /= 9999) THEN
      NCNV (MB) = 1
    ENDIF
  ENDDO
ENDIF

!*************************************************************************DJW 04/08/04
!
!     Checks Salinity Values against maximum and minimum permissible values and resets
!     to keep within an appropriate range as appropriate
!
!*************************************************************************
!IPK feb07
IF (NCONV /= 1) THEN
  SalLowPerm = 0.0000
  SalHighPerm = 250.00
  Do J = 1, NP
    IF (Vel (4, J) < SalLowPerm) THEN
      Vel (4, J) = SalLowPerm
    ENDIF
    !djw Salinity High Overide can be commented out if a crash is to be forced to
    IF (Vel (4, J) > SalHighPerm) THEN
      !Enable debugging
      Vel (4, J) = SalHighPerm
    ENDIF

    !MD neu:  Abfangen von Konzentration kleiner Null
    IF (Vel (6,J) < SedLowPerm) THEN
      Vel (6,J) = SedLowPerm
    ENDIF
    !MD neu:  Abfangen von Konzentration groesser MAXSED [mg/l]
    IF (Vel (6,J) > SedHighPerm) THEN
      Vel (6,J) = SedHighPerm
    ENDIF

  ENDDO
ENDIF
!*************************************************************************END DJW 04/08/04

!testoutput to the control panel
write (*, *) nconv, conv (1), conv (2), conv (3)
!
!...... DRPOUT forms list of equations eligible for dropout
!
!     CALL DRPOUT

!-
!......UPDATE MIDSIDE VALUES
!-
UpdateMidsides: DO N = 1, NE

  !check for elements in 3D-approach
  IF (IMAT (N) <= 5000) then

!IPK DEC00 ALLOW FOR GATE STRUCTURE
    !Skip all special elements except gate structures
    IF (IMAT (N) > 900 .and. IGTP (N) == 0) CYCLE UpdateMidsides
  endif

  ILK = 1
  NCN = NCORN (N)

  IF (NCN == 5) NCN = 3
  IF (NCN == 15  .OR.  NCN == 6 ) ILK = 2
  IF (NCN == 10) ILK = 3
  IF (NCN == 13) ILK = 4

  AllCornerNodes: DO M = 1, NCN
    N1 = IL (M, ILK)
    IF (N1 == 0) CYCLE AllCornerNodes
!IPK OCT98
    MM = NOP (N, M)
    N1 = NOP (N, N1)
    N2 = IH (M, ILK)
    N2 = NOP (N, N2)
    !interpolate the midside node's depth of the M-th arc of the element by using the corner nodes of that arc
    VEL (3, MM) = (VEL (3, N1) + VEL (3, N2)) / 2.
!IPK MAY02
    !also interpolate the 7-th constituent
    GAN (MM) = (GAN (N1) + GAN (N2)) / 2.

!IPK APR01 ADD WATER SURFACE ELEVATION UPDATE
    WSLL (MM) = (WSLL (N1) + WSLL (N2)) / 2.

    !update, if wanted by the user (nstrt == 1), the constituents
    IF (NSTRT (MM, 1) /= 0) THEN
      VEL (4, MM) = (VEL (4, N1) + VEL (4, N2)) / 2.
      VEL (5, MM) = (VEL (5, N1) + VEL (5, N2)) / 2.
      VEL (6, MM) = (VEL (6, N1) + VEL (6, N2)) / 2.
    ENDIF
  ENDDO AllCornerNodes
ENDDO UpdateMidsides

!-
!......PRINT AND/OR TERMINATE ON LARGE HEAD CHANGES
!-
IF (ABS (EMAX (3)) > 100. .OR. ABS (EMAX (1)) .GT. 50. .OR. ABS (EMAX (2)) >  50.) THEN

  !nis,sep07: Remember problematic node
  IF (ABS (emax (3)) > 100.) THEN
    problematicNode = nmx (3)
  elseif (ABS (emax (2)) >  50.) THEN
    problematicNode = nmx (2)
  ELSE
    problematicNode = nmx (1)
  ENDIF

  !nis,feb08: big changes cause parsing errors in KALYPSO-GUI
  do i = 1, 3
    if (emax (i) > 999.9) then
      emax (i) = 999.9
    end if
  end do

  !EFa jun07, necessary for autoconverge
  if (beiauto > 0.) then
    exterr = 1.
    return
  end if

  CALL OUTPUT (2)
  
  
  !generate file name for minimum values 
  call generateOutputFileName ('mini', 0, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
  !write minimum values file
  call write_Kalypso (outputFileName, 'mini')
  !generate file name for maximum values 
  call generateOutputFileName ('maxi', 0, icyc, maxn, modellaus, modellein, modellrst, ct, nb, outputFileName, inputFileName)
  !write maximum values file
  call write_Kalypso (outputFileName, 'maxi')

!IPK sep04

  !TOASK
  !nis,sep07
  !ERROR MESSAGE
  !EXECUTION TERMINATED BY EXCESS CHANGES
  if (IntPolProf (problematicNode)) then
    cord (ProblematicNode, 1) = (1.0 - kmWeight (problematicNode)) * cord (NeighProf (problematicNode, 1), 1) &
    &                         +        kmWeight (problematicNode)  * cord (NeighProf (problematicNode, 2), 1)

    cord (ProblematicNode, 2) = (1.0 - kmWeight (problematicNode)) * cord (NeighProf (problematicNode, 1), 2) &
    &                         +        kmWeight (problematicNode)  * cord (NeighProf (problematicNode, 2), 2)
  end if

  call ErrorMessageAndStop (4001, problematicNode, cord(problematicNode, 1), cord(problematicNode, 2))
ENDIF


 6000 FORMAT ( 1H1  / 10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW...PROGRAM RMA-10 '/ 10X, &
      &                    'THREE-DIMENSIONAL HYDRODYNAMICS WITH SALINITY-TEMPERATURE-SEDIMENT')

 6001 FORMAT (/ 5X, 20A4 )

 6003 FORMAT (/ 5X, 'RESULTS AT THE END OF',I4, ' TIME STEPS...TOTAL TIME =', F8.2 , ' HOURS....ITERATION CYCLE IS' , I5 )

 6005 FORMAT (// 10X, 'CONVERGENCE PARAMETERS'  // 8X, 'DF        AVG CHG        MAX CHG     LOCATION')

 6010 FORMAT (I2, 2F11.5, I9, 1x, f8.3,1x, F9.3, I9, 1x, f8.3, 1X, 2A4)

 6011 FORMAT (// 20x, ' SCHWARZ ITERATION', I4, ' TIME STEP', I4, ' NEWTON ITERATION', I3 /)
 
 6012 FORMAT ('      AVG CHG    MAX CHG  MaxNode    MaxKM MaxRelChg  RelNode    RelKM' / &
           &  '******************************************************************************')
 6013 FORMAT ('               (checked)')
 6014 FORMAT ('                                           (checked)')
 

!EFa jun07, testing
 6111 FORMAT(i4,' rss :',F10.5)
!-
      RETURN
      END